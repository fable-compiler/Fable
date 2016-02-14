namespace Fabel

type CompilerOptions =
    {
        code: string
        projFile: string
        symbols: string[]
        outDir: string
        lib: string
    }
    static member Default projFile = {
        code=null; symbols=[||]; outDir="."; lib="."; projFile=projFile        
    }
    static member Sanitize opts = {
        opts with
            symbols = if opts.symbols <> null then opts.symbols else [||]
            outDir = if opts.outDir <> null then opts.outDir else "."
            lib = if opts.lib <> null then opts.lib else "."
    }

type ICompiler =
    abstract Options: CompilerOptions

module Naming =
    open System
    open System.IO
    open System.Text.RegularExpressions
    
    let removeBrackets, removeGetPrefix =
        let reg1 = Regex(@"^\( (.*) \)$")
        let reg2 = Regex(@"^get_")
        (fun s -> reg1.Replace(s, "$1")),
        (fun s -> reg2.Replace(s, ""))
        
    let lowerFirst (s: string) =
        s.Substring 1 |> (+) (Char.ToLowerInvariant s.[0] |> string)
        
    let getFieldIndex fieldName =
        match Regex.Match(fieldName, @"\d+$") with
        | m when m.Success -> int m.Value
        | _ -> 0
    
    let getCoreLibPath (com: ICompiler) =
        Path.Combine(com.Options.lib, "fabel-core.js")
        
    // TODO: Use $F for CoreLib?
    let getImportModuleIdent i = sprintf "$M%i" (i+1)
    
    let identForbiddenChars =
        Regex @"^[^a-zA-Z_]|[^0-9a-zA-Z_]"
        
    let trimDots (s: string) =
        match s.StartsWith ".", s.EndsWith "." with
        | true, true -> s.Substring(1, s.Length - 2)
        | true, false -> s.Substring(1)
        | false, true -> s.Substring(0, s.Length - 1)
        | false, false -> s

    // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
    let jsKeywords =
        set["abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "self"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield" ]
        
    let sanitizeIdent conflicts name =
        let preventConflicts conflicts name =
            let rec check n =
                let name = if n > 0 then sprintf "%s_%i" name n else name
                if not (conflicts name) then name else check (n+1)
            check 0
        // Replace Forbidden Chars
        let sanitizedName =
            identForbiddenChars.Replace(removeBrackets name, "_")
        // Check if it's a keyword
        jsKeywords.Contains sanitizedName
        |> function true -> "_" + sanitizedName | false -> sanitizedName
        // Check if it already exists in scope
        |> preventConflicts conflicts

    /// Creates a relative path from one file or folder to another.
    /// from http://stackoverflow.com/a/340454/3922220
    let getRelativePath toPath fromPath =
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

