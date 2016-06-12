namespace Fable

type CompilerOptions = {
        code: string
        projFile: string
        coreLib: string
        symbols: string list
        plugins: string list
        msbuild: string list
        refs: Map<string, string>
        watch: bool
        clamp: bool
        copyExt: bool
    }
    
type CompilerMessageType =
    | Error | Log
    override x.ToString() =
        match x with Error -> "ERROR" | Log -> "LOG"
    
type CompilerMessage(typ: CompilerMessageType, msg) =
    member x.``type`` = string typ
    member x.message: string = msg

type IPlugin =
    interface end

type ICompiler =
    abstract Options: CompilerOptions
    abstract Plugins: IPlugin list
    
type PerfTimer(label) =
    let t = System.Diagnostics.Stopwatch()
    do t.Start()
    /// Stops timer and returns a log message with label and total seconds
    member x.Finish() =
        t.Stop()
        t.Elapsed.TotalSeconds
        |> sprintf "%s: %fs" label
        |> Fable.AST.Info

module Patterns =
    let (|Try|_|) (f: 'a -> 'b option) a = f a
    
    let (|DicContains|_|) (dic: System.Collections.Generic.IDictionary<'k,'v>) key =
        let success, value = dic.TryGetValue key
        if success then Some value else None

    let (|SetContains|_|) set item =
        if Set.contains item set then Some item else None
    
module Naming =
    open System
    open System.IO
    open System.Text.RegularExpressions
    open Patterns
    
    let (|StartsWith|_|) pattern (txt: string) =
        if txt.StartsWith pattern then Some pattern else None

    let (|EndsWith|_|) pattern (txt: string) =
        if txt.EndsWith pattern then Some pattern else None
    
    let knownInterfaces =
        set [ "System.Object"; "System.IComparable"; "System.IDisposable";
            "System.IObservable"; "System.IObserver"]
             
    let automaticInterfaces =
        set [ "System.IEquatable"; "System.Collections.IStructuralEquatable";
            "System.IComparable"; "System.Collections.IStructuralComparable" ]
    
    let ignoredCompilerGenerated =
        set [ "CompareTo"; "Equals"; "GetHashCode" ]

    let ignoredAtts =
        set ["Erase"; "Import"; "Global"; "Emit"]

    let ignoredFilesRegex =
        Regex(@"Fable\.(?:Import|Core)[\w.]*\.fs$")
        
    let identForbiddenCharsRegex =
        Regex @"^[^a-zA-Z_$]|[^0-9a-zA-Z_$]"
    
    let removeParens, removeGetSetPrefix, sanitizeActivePattern =
        let reg1 = Regex(@"^\( (.*) \)$")
        let reg2 = Regex(@"^[gs]et_")
        let reg3 = Regex(@"^\|[^\|]+?(?:\|[^\|]+)*(?:\|_)?\|$")
        (fun s -> reg1.Replace(s, "$1")),
        (fun s -> reg2.Replace(s, "")),
        (fun (s: string) -> if reg3.IsMatch(s) then s.Replace("|", "$") else s)
        
    let lowerFirst (s: string) =
        s.Substring 1 |> (+) (Char.ToLowerInvariant s.[0] |> string)

    let normalizePath (path: string) =
        path.Replace("\\", "/")
        
    let getCommonPrefix (xs: string list) =
        let rec getCommonPrefix (prefix: string) = function
            | [] -> prefix
            | (x: string)::xs ->
                let mutable i = 0
                while i < prefix.Length && i < x.Length && x.[i] = prefix.[i] do
                    i <- i + 1
                getCommonPrefix (prefix.Substring(0,i)) xs
        match xs with
        | [] -> ""
        | [x] -> x
        | x::xs -> getCommonPrefix x xs
        
    let exportsIdent = "$exports"
    
    let getImportIdent i = sprintf "$import%i" i

    let getUniqueVar =
        let monitor = obj()    
        let mutable id = ref 0
        fun () -> lock monitor (fun () -> id := !id + 1; sprintf "$var%i" !id)
    
    /// If flag --copyExt is activated, copy files outside project folder into
    /// an internal `fable_external` folder (adding a hash to prevent naming conflicts) 
    let fixExternalPath =
        let cache = System.Collections.Generic.Dictionary<string, string>()
        let addToCache (cache: System.Collections.Generic.Dictionary<'k, 'v>) k v =
            cache.Add(k, v); v
        let isExternal projPath path =
            let rec parentContains rootPath path' =
                match Directory.GetParent path' with
                | null -> Some (rootPath, path)
                // Files in node_modules are always considered external, see #188
                | parent when parent.FullName.Contains("node_modules") -> Some(rootPath, path)
                | parent when rootPath = parent.FullName -> None
                | parent -> parentContains rootPath parent.FullName
            parentContains (Path.GetDirectoryName projPath) path
        fun (com: ICompiler) filePath ->
            if not com.Options.copyExt then filePath else
            match Path.GetFullPath filePath with
            | DicContains cache filePath -> filePath
            | Try (isExternal com.Options.projFile) (rootPath, filePath) ->
                Path.Combine(rootPath, "fable_external",
                    sprintf "%s-%i%s"
                        (Path.GetFileNameWithoutExtension filePath)
                        (filePath.GetHashCode() |> abs)
                        (Path.GetExtension filePath))
                |> addToCache cache filePath
            | filePath ->
                addToCache cache filePath filePath

    // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
    let jsKeywords =
        set [
            "abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "self"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield";
            // Standard built-in objects (https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects)
            "Object"; "Function"; "Boolean"; "Symbol"; "Map"; "Set"; "Number"; "Math"; "Date"; "String"; "RegExp"; "JSON"; "Promise";
            "Array"; "Int8Array"; "Uint8Array"; "Uint8ClampedArray"; "Int16Array"; "Uint16Array"; "Int32Array"; "Uint32Array"; "Float32Array"; "Float64Array";
        ] 

    let isInvalidJsIdent name =
        identForbiddenCharsRegex.IsMatch(name) || jsKeywords.Contains name

    let sanitizeIdent conflicts name =
        let preventConflicts conflicts name =
            let rec check n =
                let name = if n > 0 then sprintf "%s_%i" name n else name
                if not (conflicts name) then name else check (n+1)
            check 0
        // Replace Forbidden Chars
        let sanitizedName =
            identForbiddenCharsRegex.Replace(removeParens name, "_")
        // Check if it's a keyword or clashes with module ident pattern
        jsKeywords.Contains sanitizedName
        |> function true -> "_" + sanitizedName | false -> sanitizedName
        // Check if it already exists
        |> preventConflicts conflicts
        
    let getQueryParams (txt: string) =
        match txt.IndexOf("?") with
        | -1 -> txt, Map.empty<_,_>
        | i ->
            txt.Substring(i + 1).Split('&')
            |> Seq.choose (fun pair ->
                match pair.Split('=') with
                | [|key;value|] -> Some (key,value)
                | _ -> None)
            |> fun args -> txt.Substring(0, i), Map(args)

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
                            IO.Path.DirectorySeparatorChar)  |> normalizePath
            | _ -> relativePath |> normalizePath
            
    let getExternalImportPath (com: ICompiler) (filePath: string) (importPath: string) =
        if not(importPath.StartsWith ".")
        then importPath
        else
            let filePath = Path.GetFullPath filePath
            let projFile = Path.GetFullPath com.Options.projFile
            if Path.GetDirectoryName filePath = Path.GetDirectoryName projFile
            then importPath
            else
                getRelativePath projFile filePath
                |> Path.GetDirectoryName
                |> fun relPath ->
                    match Path.Combine(relPath, importPath) with
                    | path when path.StartsWith "." -> path
                    | path -> "./" + path 
                    |> normalizePath

module Json =
    open FSharp.Reflection
    open Newtonsoft.Json
    
    let isErasedUnion (t: System.Type) =
        t.Name = "FSharpOption`1" ||
        FSharpType.IsUnion t &&
            t.GetCustomAttributes true
            |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")
            
    let getErasedUnionValue (v: obj) =
        match FSharpValue.GetUnionFields (v, v.GetType()) with
        | _, [|v|] -> Some v
        | _ -> None
            
    type ErasedUnionConverter() =
        inherit JsonConverter()
        override x.CanConvert t = isErasedUnion t
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            match getErasedUnionValue v with
            | Some v -> serializer.Serialize(writer, v) 
            | None -> writer.WriteNull()        

