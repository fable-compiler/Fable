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

    /// This is null to keep compatibility with Require.js
    /// (which expects paths not to have extensions), in the
    /// future this will probably be changed to ".js"
    let targetFileExtension: string = null // ".js"

    let [<Literal>] placeholder = "__PLACE-HOLDER__"
    let [<Literal>] dummyFile = "__DUMMY-FILE__.txt"
    let [<Literal>] exportsIdent = "__exports"
    let [<Literal>] genArgsIdent = "_genArgs"
    let [<Literal>] fablemapExt = ".fablemap"

    /// Calls to methods of these interfaces will be replaced
    let replacedInterfaces =
        set [ "System.Collections.IEnumerable"; "System.Collections.Generic.IEnumerable";
              "System.Collections.IEnumerator"; "System.Collections.Generic.IEnumerator";
              "System.Collections.Generic.ICollection"; "System.Collections.Generic.IList"
              "System.Collections.Generic.IDictionary"; "System.Collections.Generic.ISet" ]

    let ignoredBaseClasses =
        set [ "System.Object"; "System.Exception" ]

    /// Interfaces automatically assigned by the F# compiler
    /// to unions and records. Ignored by Fable.
    let ignoredInterfaces =
        set [ "System.Collections.IStructuralEquatable"; "System.Collections.IStructuralComparable" ]

    /// Methods automatically assigned by the F# compiler
    /// for unions and records. Ignored by Fable.
    let ignoredCompilerGenerated =
        set [ "CompareTo"; "Equals"; "GetHashCode" ]

    let importAtts =
        set ["Import"; "Global"]

    let eraseAtts =
        set ["Erase"; "Emit"; "KeyValueList"; "StringEnum"]

    let identForbiddenCharsRegex =
        Regex @"^[^a-zA-Z_]|[^0-9a-zA-Z_]"

    /// Matches placeholders for generics in an Emit macro
    /// like `React.createElement($'T, $0, $1)`
    let genericPlaceholderRegex =
        Regex @"\$'(\w+)"

    let replaceIdentForbiddenChars (ident: string) =
        identForbiddenCharsRegex.Replace(ident, fun (m: Match) ->
            "$" + (int m.Value.[0]).ToString("X") + "$")

    let removeGetSetPrefix =
        let reg2 = Regex(@"^[gs]et_")
        (fun s -> reg2.Replace(s, ""))

    let lowerFirst (s: string) =
        s.Substring(0,1).ToLowerInvariant() + s.Substring(1)

    let upperFirst (s: string) =
        s.Substring(0,1).ToUpperInvariant() + s.Substring(1)

    let jsKeywords =
        set [
            // Fable reserved keywords
            exportsIdent; genArgsIdent
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
            // See https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
            "fetch"
        ]

    let preventConflicts conflicts name =
        let rec check n =
            let name = if n > 0 then sprintf "%s_%i" name n else name
            if not (conflicts name) then name else check (n+1)
        check 0

    let sanitizeIdent conflicts name =
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

    let normalizeFullPath (path: string) =
        Path.GetFullPath(path).Replace("\\", "/")

    /// Creates a relative path from one file or folder to another.
    let getRelativeFileOrDirPath fromIsDir fromPath toIsDir toPath =
        // Algorithm adapted from http://stackoverflow.com/a/6244188
        let pathDifference (path1: string) (path2: string) =
            let path1 = normalizePath path1
            let path2 = normalizePath path2
            let mutable c = 0  //index up to which the paths are the same
            let mutable d = -1 //index of trailing slash for the portion where the paths are the s
            while c < path1.Length && c < path2.Length && path1.[c] = path2.[c] do
                if path1.[c] = '/' then d <- c
                c <- c + 1
            if c = 0
            then path2
            elif c = path1.Length && c = path2.Length
            then String.Empty
            else
                let builder = new System.Text.StringBuilder()
                while c < path1.Length do
                    if path1.[c] = '/' then builder.Append("../") |> ignore
                    c <- c + 1
                if builder.Length = 0 && path2.Length - 1 = d
                then "./"
                else builder.ToString() + path2.Substring(d + 1)
        // Add a dummy file to make it work correctly with dirs
        let addDummyFile isDir path =
            if isDir
            then IO.Path.Combine(path, Naming.dummyFile)
            else path
        let fromPath = addDummyFile fromIsDir fromPath
        let toPath = addDummyFile toIsDir toPath
        match (pathDifference fromPath toPath).Replace(Naming.dummyFile, "") with
        | path when path.StartsWith "." -> path
        | path -> "./" + path

    let getRelativePath fromPath toPath =
        // This is not 100% reliable, but IO.Directory.Exists doesn't
        // work either if the directory doesn't exist (e.g. `outDir`)
        let isDir = Path.GetExtension >> String.IsNullOrWhiteSpace
        // let isDir = IO.Directory.Exists
        getRelativeFileOrDirPath (isDir fromPath) fromPath (isDir toPath) toPath

    let getCommonPrefix (xs: string[] list)=
        let rec getCommonPrefix (prefix: string[]) = function
            | [] -> prefix
            | (x: string[])::xs ->
                let mutable i = 0
                while i < prefix.Length && i < x.Length && x.[i] = prefix.[i] do
                    i <- i + 1
                getCommonPrefix prefix.[0..i-1] xs
        match xs with [] -> [||] | x::xs -> getCommonPrefix x xs

    let isChildPath (parent: string) (child: string) =
        let split x =
            (normalizeFullPath x).Split('/')
            |> Array.filter (String.IsNullOrWhiteSpace >> not)
        let parent = split parent
        let child = split child
        let commonPrefix = getCommonPrefix [parent; child]
        commonPrefix.Length >= parent.Length

    let getCommonBaseDir (filePaths: seq<string>) =
        filePaths
        |> Seq.map (fun filePath ->
            filePath
            |> Path.GetDirectoryName
            |> normalizePath
            |> fun path ->
                path.Split('/') |> Array.filter (String.IsNullOrWhiteSpace >> not))
        |> Seq.toList |> getCommonPrefix
        |> String.concat "/"
