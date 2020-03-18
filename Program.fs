// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Data
open System.Xml.Linq
open System.Xml
open System
open System.IO
open System.Globalization

type Registry = XmlProvider<"https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/master/xml/gl.xml">

type ParamType =
    | Normal
    | Array
    | Ref


type GlParameter = {
    Type: string
    Name: string
    ParamType: ParamType
}

type GlCommand = {    
    Name : string
    Type : string 
    Parameters : GlParameter list
}

type GlEnum = {
    Name : string
    Value : int
}

let textOf (xmlElement: XElement) =
    xmlElement.Nodes()
    |> Seq.pick (fun f -> 
        if f.NodeType = XmlNodeType.Text then
            Some (f :?> XText).Value
        else
            None
        )

let transformType (typ: string) =
    match typ with
    | "GLenum"
    | "GLbitfield"
    | "GLuint" -> "uint"
    | "GLsizei"
    | "GLint" -> "int"
    | "GLbyte" -> "sbyte"
    | "GLubyte" -> "byte"
    | "GLcharARB"
    | "GLchar" -> "string"
    | "GLfixed" 
    | "GLclampf"
    | "GLfloat" -> "float"
    | "GLuint64EXT"
    | "GLuint64" -> "ulong"
    | "GLintptr"
    | "GLeglClientBufferEXT"
    | "GLsync"
    | "GLDEBUGPROC"
    | "GLsizeiptr" -> "IntPtr"
    | "GLshort" -> "short"
    | "GLushort" -> "ushort"
    | "GLboolean" -> "bool"
    | "GLdouble" -> "double"
    | "GLint64" -> "long"
    | "void " -> "void"
    | "void *" -> "IntPtr"
    | _ -> failwith (sprintf "GlType %s not implemented" typ)
    

let toGlEnum (enum : Registry.Enum2) =
    let value =
        match (enum.Value.Number, enum.Value.String) with
        | (Some num, _) -> Some num
        | (_, Some str) -> 
            match System.Int32.TryParse(str.Replace("0x","")) with
            | (true, v) -> Some v
            | _ -> None
        | _ -> None
    
    value
    |> Option.bind (fun v ->
        Some { Name = enum.Name; Value = v }
    )


type GroupedEnum = {
    Name : string
    GroupName : string
    Value : Registry.ValueChoice
}

let generateSources (data: Registry.Registry) (apiName: string) (apiNumber: decimal) (profile: string)=

    let gl1Commands =
        data.Features
        |> Seq.filter (fun f -> f.Name = "GL_VERSION_1_0" || f.Name = "GL_VERSION_1_1" )
        |> Seq.collect (fun f -> f.Requires)
        |> Seq.collect (fun f -> f.Commands)
        |> Seq.map (fun f -> f.Name)
        |> Set.ofSeq

    let apiStuff =
        data.Features 
        |> Seq.filter (fun f -> f.Api = apiName && f.Number <= apiNumber)

    let apiRequires = 
        apiStuff
        |> Seq.collect (fun f -> f.Requires)

    let apiRemove =        
        apiStuff
        |> Seq.collect (fun f -> f.Removes)
        |> Seq.filter (fun f -> f.Profile = profile)
       
    let apiRemoveCommands =        
        apiRemove
        |> Seq.collect (fun f -> f.Commands)
        |> Seq.map (fun f-> f.Name)
        |> Set.ofSeq

    let apiRemoveEnums =
        apiRemove
        |> Seq.collect (fun f -> f.Enums)
        |> Seq.map (fun f-> f.Name)
        |> Set.ofSeq

    let apiCommands =
        apiRequires
        |> Seq.collect (fun f -> f.Commands)        
        |> Seq.filter (fun f -> apiRemoveCommands |> Set.contains f.Name |> not )
        |> Seq.map (fun f -> f.Name)
        |> Set.ofSeq

    let apiEnums =
        apiRequires
        |> Seq.collect (fun f -> f.Enums)
        |> Seq.filter (fun f -> apiRemoveEnums |> Set.contains f.Name |> not )
        |> Seq.map (fun f -> f.Name)
        |> Set.ofSeq
            
    let allEnumValues =
        data.Enums
        |> Seq.collect (fun e -> e.Enums)
        |> Seq.map (fun e -> (e.Name, e.Value))
        |> Map.ofSeq

    let filteredEnums =
        data.Groups
        |> Seq.collect (fun g -> 
                        g.Enums 
                        |> Seq.filter (fun e -> allEnumValues |> Map.containsKey e.Name )
                        |> Seq.map (fun e -> { GroupName = g.Name; Name = e.Name; Value = allEnumValues |> Map.find e.Name }) 
                        |> Seq.distinct)
        |> Seq.groupBy (fun data -> data.GroupName)
        |> Map.ofSeq
        |> Map.filter (fun _ data -> data |> Seq.exists (fun d -> apiEnums |> Set.contains d.Name) 
        )
            
    let commands = 
        let readParameter (p: Registry.Param) =
            let ptype = 
                match (p.Group, p.Ptype) with
                | (Some group, Some typ) -> 
                    if Map.containsKey group filteredEnums then
                        group
                    else
                        typ |> transformType
                | (_, Some typ) -> typ |> transformType
                | _ -> "void"

            let filterParamName paramName =
                match paramName with
                | "params" -> "_params"
                | "string" -> "_string"
                | "ref" -> "_ref"
                | "base" -> "_base"
                | _ -> paramName
        
            let count x = Seq.filter ((=) x) >> Seq.length

            let pT =
                let txt = p.XElement.Value
                txt
                |> count '*'
                |> function
                    | 0 -> Normal
                    | 1 -> Array
                    | 2 -> Ref
                    | _ -> failwith "More than 2??"

            { 
                Type = ptype
                Name = p.Name |> filterParamName
                ParamType = pT
            }


        data.Commands.Commands 
        |> Seq.filter (fun c -> apiCommands |> Set.contains c.Proto.Name)
        |> Seq.map (fun c -> { 
            Name = c.Proto.Name
            Type = Option.defaultValue (textOf c.Proto.XElement) c.Proto.Ptype |> transformType
            Parameters = c.Params |> Seq.map readParameter |> Seq.toList
        })
        |> Seq.toList

    let enumsSource =
        let valueToString (value: Registry.ValueChoice) =
            match (value.String, value.Number) with
            | (Some str, _) -> str
            | (_, Some v) -> v.ToString()
            | _ -> "??????"
            
        let printEnum (e: GroupedEnum) =
            sprintf "%s = %s" e.Name (e.Value |> valueToString)

        filteredEnums
        |> Map.toList
        |> List.map (fun (groupName, enums) -> sprintf "public enum %s : uint {\r\n%s\r\n}" groupName (enums |> Seq.map printEnum |> String.concat ",\r\n"))
        
    let gl11filter predicate =
        List.filter (fun (f : GlCommand) -> gl1Commands |> Set.contains f.Name |> predicate)
    
    let printParameter (p: GlParameter) =            
            match p.ParamType with
            | Normal ->               
                sprintf "%s %s" p.Type p.Name
            | Array ->
                if p.Type = "string" then
                    sprintf "StringBuilder %s" p.Name
                else if p.Type = "void" then
                    sprintf "[MarshalAs(UnmanagedType.AsAny)] object %s" p.Name
                else
                    sprintf "%s[] %s" p.Type p.Name
            | Ref ->
                if p.Type = "string" then
                    sprintf "string[] %s" p.Name
                else if p.Type = "void" then
                    sprintf "[MarshalAs(UnmanagedType.AsAny)] object %s" p.Name
                else
                    sprintf "ref %s[] %s" p.Type p.Name

    let commandsSource =
        commands
        |> gl11filter not
        |> List.map (fun c ->            
            let commaSeparatedParams = 
                c.Parameters 
                |> List.map printParameter 
                |> String.concat ", "

            sprintf "public delegate %s Del%s(%s); public static Del%s %s;" c.Type c.Name commaSeparatedParams c.Name c.Name
            )

    let prologue = [
        "using System;"
        "using System.Runtime.InteropServices;"
        "using System.Text;"
        "class GL {"
        "[DllImport(\"opengl32.dll\")]"
        "public static extern IntPtr wglGetProcAddress([MarshalAs(UnmanagedType.LPStr)] string proc);"
        ]

    let epilogue = ["}";]

    let gl11FuncExternSources =       
        commands
        |> gl11filter id
        |> List.map (fun c ->
            let commaSeparatedParams = 
                c.Parameters 
                |> List.map printParameter 
                |> String.concat ", "

            sprintf "[DllImport(\"opengl32.dll\")] public static extern %s %s(%s);" c.Type c.Name commaSeparatedParams
        )

    let funcLoaders =
        commands
        |> gl11filter not
        |> List.map (fun c -> 
            sprintf "%s = (Del%s)Marshal.GetDelegateForFunctionPointer(wglGetProcAddress(\"%s\"), typeof(Del%s));" c.Name c.Name c.Name c.Name
        )
    let funcLoader =
        match List.length funcLoaders with
        | 0 ->
            []
        | _ ->
            ["public static void LoadGLFuncs() {"; funcLoaders |> String.concat "\r\n"; "}"]

    Seq.concat [prologue; enumsSource; gl11FuncExternSources; commandsSource; funcLoader; epilogue]


[<EntryPoint>]
let main argv = 
    let data = Registry.Load("https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/master/xml/gl.xml")

    let apiChoices =
        data.Features
        |> Seq.map (fun f -> f.Api + "=>" + string(f.Number))
    
    let listOp = 
        argv
        |> Array.exists (fun a -> a = "--list")
    
    match listOp with
    | true ->
        printfn "Available API choices:\r\n%s" (String.concat "\r\n" apiChoices)
    | false ->
        match argv with
        | [|apiName; version; profile|] ->
            match Decimal.TryParse(version, NumberStyles.Number, CultureInfo.InvariantCulture) with
            | (true, v) ->
                generateSources data apiName v profile
                |> String.concat "\r\n"
                |> printfn "%s"
            | (false, _) ->
                printfn "You need to specify a valid version number, for example 1.1"
        | _ ->
            printfn "You need to specify an API name, a version AND a profile. For example gl 1.1 core. Use --list to list available apis"
    
    0 // return an integer exit code