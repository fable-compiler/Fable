namespace Fabel

type CompilerOptions = {
    sourceRootPath: string
    targetRootPath: string
    environment: string
    jsLibFolder: string
}

type ICompiler =
    abstract Options: CompilerOptions

module Naming =
    let [<Literal>] coreLibIdent = "$Fabel"
    let [<Literal>] rootModuleIdent = "$M0"
    let getImportModuleIdent i = sprintf "$M%i" (i+1)
    
    let identForbiddenChars =
        System.Text.RegularExpressions.Regex "^[^a-zA-Z_]|[^0-9a-zA-Z_]"

    // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
    let jsKeywords =
        set["abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "self"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield" ]
        
    let sanitizeIdent conflicts name =
        let preventConflicts conflicts str =
            let rec check n =
                let name = if n > 0 then sprintf "%s_%i" str n else str
                if not (conflicts name) then name else check (n+1)
            check 0
        // Replace Forbidden Chars
        let sanitizedName = identForbiddenChars.Replace(name, "_")
        // Check if it's a keyword
        jsKeywords.Contains sanitizedName
        |> function true -> "_" + sanitizedName | false -> sanitizedName
        // Check if it already exists in scope
        |> preventConflicts conflicts

module IO =
    open System
    /// Creates a relative path from one file or folder to another.
    /// from http://stackoverflow.com/a/340454/3922220
    let makeRelativePath fromPath toPath =
        let fromUri = Uri(fromPath)
        let toUri = Uri(toPath)
        if fromUri.Scheme <> toUri.Scheme then
            toPath   // path can't be made relative.
        else
            let relativeUri = fromUri.MakeRelativeUri(toUri)
            let relativePath = Uri.UnescapeDataString(relativeUri.ToString())
            match toUri.Scheme.ToUpperInvariant() with
            | "FILE" -> relativePath.Replace(
                            IO.Path.AltDirectorySeparatorChar,
                            IO.Path.DirectorySeparatorChar)
            | _ -> relativePath        

module Json =
    open FSharp.Reflection
    open Newtonsoft.Json
            
    type ErasedUnionConverter() =
        inherit JsonConverter()
        override x.CanConvert t =
            t.Name = "FSharpOption`1" ||
            FSharpType.IsUnion t &&
                t.GetCustomAttributes true
                |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            match FSharpValue.GetUnionFields (v, v.GetType()) with
            | _, [|v|] -> serializer.Serialize(writer, v) 
            | _ -> writer.WriteNull()        

    type NumberConverter() =
        inherit JsonConverter()
        override x.CanConvert t = t.FullName = "System.Double"
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            let f = unbox<float> v
            writer.WriteValue(if f % 1. = 0. then box(int f) else box f)
            
    let converters: JsonConverter[] = [|
        ErasedUnionConverter ()
        NumberConverter ()
    |]
