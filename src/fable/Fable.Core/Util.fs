namespace Fable

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
    
    let [<Literal>] placeholder = "PLACE-HOLDER"
    let [<Literal>] fableExternalDir = "fable_external"
    let [<Literal>] fableInjectFile = "./fable_inject.js"
    let [<Literal>] exportsIdent = "$exports"

    /// Calls to methods of these interfaces will be replaced
    /// so they cannot be customly implemented.
    let replacedInterfaces =
        set [ "System.Collections.IEnumerable"; "System.Collections.Generic.IEnumerable";
              "System.Collections.IEnumerator"; "System.Collections.Generic.IEnumerator";
              "System.Collections.Generic.ICollection"; "System.Collections.Generic.IList"
              "System.Collections.Generic.IDictionary"; "System.Collections.Generic.ISet" ]

    /// Interfaces automatically assigned by the F# compiler
    /// to unions and records. Ignored by Fable.
    let ignoredInterfaces =
        set [ "System.Collections.IStructuralEquatable"; "System.Collections.IStructuralComparable" ]

    /// Methods automatically assigned by the F# compiler
    /// for unions and records. Ignored by Fable.
    let ignoredCompilerGenerated =
        set [ "CompareTo"; "Equals"; "GetHashCode" ]

    let ignoredAtts =
        set ["Import"; "Global"; "Emit"]

    let ignoredFilesRegex =
        Regex(@"Fable\.Import\.[\w.]*\.fs$")

    let identForbiddenCharsRegex =
        Regex @"^[^a-zA-Z_$]|[^0-9a-zA-Z_$]"

    let replaceIdentForbiddenChars (ident: string) =
        identForbiddenCharsRegex.Replace(ident, fun (m: Match) ->
            "$" + (int m.Value.[0]).ToString("X"))
    
    let removeParens, removeGetSetPrefix, sanitizeActivePattern =
        let reg1 = Regex(@"^\( (.*) \)$")
        let reg2 = Regex(@"^[gs]et_")
        let reg3 = Regex(@"^\|[^\|]+?(?:\|[^\|]+)*(?:\|_)?\|$")
        (fun s -> reg1.Replace(s, "$1")),
        (fun s -> reg2.Replace(s, "")),
        (fun (s: string) -> if reg3.IsMatch(s) then s.Replace("|", "$") else s)
        
    let lowerFirst (s: string) =
        s.Substring(0,1).ToLowerInvariant() + s.Substring(1)

    let upperFirst (s: string) =
        s.Substring(0,1).ToUpperInvariant() + s.Substring(1)

    let getUniqueVar =
        let monitor = obj()    
        let mutable id = ref 0
        fun () -> lock monitor (fun () -> id := !id + 1; sprintf "$var%i" !id)
    
    let jsKeywords =
        set [
            // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
            "abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "self"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield";
            // Standard built-in objects (https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects)
            "Object"; "Function"; "Boolean"; "Symbol"; "Map"; "Set"; "Number"; "Math"; "Date"; "String"; "RegExp"; "JSON"; "Promise";
            "Array"; "Int8Array"; "Uint8Array"; "Uint8ClampedArray"; "Int16Array"; "Uint16Array"; "Int32Array"; "Uint32Array"; "Float32Array"; "Float64Array";
            // DOM interfaces (https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
            "Attr"; "CharacterData"; "Comment"; "CustomEvent"; "Document"; "DocumentFragment"; "DocumentType"; "DOMError"; "DOMException"; "DOMImplementation";
            "DOMString"; "DOMTimeStamp"; "DOMSettableTokenList"; "DOMStringList"; "DOMTokenList"; "Element"; "Event"; "EventTarget"; "HTMLCollection"; "MutationObserver";
            "MutationRecord"; "Node"; "NodeFilter"; "NodeIterator"; "NodeList"; "ProcessingInstruction"; "Range"; "Text"; "TreeWalker"; "URL"; "Window"; "Worker"; "XMLDocument"; 
            // See #258
            "arguments"
        ] 

    let sanitizeIdent conflicts name =
        let preventConflicts conflicts name =
            let rec check n =
                let name = if n > 0 then sprintf "%s_%i" name n else name
                if not (conflicts name) then name else check (n+1)
            check 0
        // Replace Forbidden Chars
        let sanitizedName =
            identForbiddenCharsRegex.Replace(name, "_")
        // Check if it's a keyword or clashes with module ident pattern
        jsKeywords.Contains sanitizedName
        |> function true -> "_" + sanitizedName | false -> sanitizedName
        // Check if it already exists
        |> preventConflicts conflicts

module Path =
    open System
    open System.IO
    open Patterns

    let normalizePath (path: string) =
        path.Replace("\\", "/")
        
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
                Path.Combine(rootPath, Naming.fableExternalDir,
                    sprintf "%s-%i%s"
                        (Path.GetFileNameWithoutExtension filePath)
                        (filePath.GetHashCode() |> abs)
                        (Path.GetExtension filePath))
                |> addToCache cache filePath
            | filePath ->
                addToCache cache filePath filePath

    /// Creates a relative path from one file or folder to another.
    /// from http://stackoverflow.com/a/340454/3922220
    let getRelativeFileOrDirPath fromIsDir fromPath toIsDir toPath =
        // Add a dummy file to make it work correctly with dirs
        let addDummyFile isDir path =
            if isDir
            then IO.Path.Combine(path, "dummy.txt")
            else path
        let fromPath = addDummyFile fromIsDir fromPath
        let toPath = addDummyFile toIsDir toPath
        let fromUri = Uri("file://" + fromPath)
        let toUri = Uri("file://" + toPath)
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

    let getRelativePath fromPath toPath =
        getRelativeFileOrDirPath 
          (IO.Directory.Exists fromPath) fromPath 
          (IO.Directory.Exists toPath) toPath

    let getExternalImportPath (com: ICompiler) (filePath: string) (importPath: string) =
        if not(importPath.StartsWith ".")
        then importPath
        else
            let filePath = Path.GetFullPath filePath
            let projFile = Path.GetFullPath com.Options.projFile
            if Path.GetDirectoryName filePath = Path.GetDirectoryName projFile
            then importPath
            else
                getRelativePath filePath projFile
                |> Path.GetDirectoryName
                |> fun relPath ->
                    match Path.Combine(relPath, importPath) with
                    | path when path.StartsWith "." -> path
                    | path -> "./" + path 
                    |> normalizePath

    let getCommonPrefix (xs: string[] list)=
        let rec getCommonPrefix (prefix: string[]) = function
            | [] -> prefix
            | (x: string[])::xs ->
                let mutable i = 0
                while i < prefix.Length && i < x.Length && x.[i] = prefix.[i] do
                    i <- i + 1
                getCommonPrefix prefix.[0..i-1] xs
        match xs with [] -> [||] | x::xs -> getCommonPrefix x xs 
        
    let getCommonBaseDir (filePaths: seq<string>) =
        filePaths
        |> Seq.map (fun filePath ->
            Path.GetFullPath filePath
            |> Path.GetDirectoryName
            |> normalizePath
            |> fun path -> path.Split('/'))
        |> Seq.toList |> getCommonPrefix
        |> String.concat "/"
