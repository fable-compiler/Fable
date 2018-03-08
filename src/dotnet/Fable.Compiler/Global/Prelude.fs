namespace Fable

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
    { line: int; column: int; }
    static member Empty = { line = 1; column = 0 }

type SourceLocation =
    { (*source: string option;*) start: Position; ``end``: Position; }
    member x.Collapse() =
        { start = x.start; ``end`` = x.start }
    static member (+) (r1: SourceLocation, r2: SourceLocation) =
        { start = r1.start; ``end`` = r2.``end`` }
    static member Empty =
        { start = Position.Empty; ``end`` = Position.Empty }
    override x.ToString() =
        sprintf "(L%i,%i-L%i,%i)"
            x.start.line x.start.column
            x.``end``.line x.``end``.column

module Map =
    let findOrRun<'T> (f: unit->'T) (k: string) (m: Map<string, obj>) =
        match Map.tryFind k m with
        | Some x -> downcast x
        | _ -> f()

    // let findOrNew<'T when 'T : (new : unit->'T)> (k: string) (m: Map<string, obj>) =
    //     findOrRun (fun () -> new 'T()) k m

module Option =
    let toBool (f: 'T->bool) (opt: 'T option) =
        match opt with Some x when f x -> true | _ -> false

module List =
    let isSingle = function
        | [_] -> true
        | _ -> false

    /// Same as List.length xs > 1
    let isMultiple = function
        | [] | [_] -> false
        | _ -> true

    let rec sameLength xs1 xs2 =
        match xs1, xs2 with
        | [], [] -> true
        | [_], [_] -> true
        | _::xs1, _::xs2 -> sameLength xs1 xs2
        | _ -> false

module Patterns =
    let (|Try|_|) (f: 'a -> 'b option) a = f a

    let (|DicContains|_|) (dic: System.Collections.Generic.IDictionary<'k,'v>) key =
        let success, value = dic.TryGetValue key
        if success then Some value else None

    let (|SetContains|_|) set item =
        if Set.contains item set then Some SetContains else None

module Naming =

    let (|StartsWith|_|) (pattern: string) (txt: string) =
        if txt.StartsWith(pattern)
        then txt.Substring(pattern.Length) |> Some
        else None

    let (|EndsWith|_|) (pattern: string) (txt: string) =
        if txt.EndsWith(pattern)
        then txt.Substring(0, txt.Length - pattern.Length) |> Some
        else None

    /// This is null to keep compatibility with Require.js
    /// (which expects paths not to have extensions), in the
    /// future this will probably be changed to ".js"
    let targetFileExtension: string = "" // ".js"

    let [<Literal>] current = "CURRENT"
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

    /// Interfaces automatically assigned by the F# compiler to unions and records. Ignored by Fable.
    let ignoredInterfaces =
        set [ "System.Collections.IStructuralEquatable"; "System.Collections.IStructuralComparable"
              "System.Collections.IEnumerable"; "System.Collections.IEnumerator" ]

    let ignoredInterfaceMethods =
        set [ "System-Collections-IEnumerable-GetEnumerator"
              "System-Collections-IEnumerator-get_Current" ]

    /// Methods automatically assigned by the F# compiler for unions and records. Ignored by Fable.
    let ignoredCompilerGenerated =
        set [ "CompareTo"; "Equals"; "GetHashCode" ]

    let umdModules =
        set ["commonjs"; "amd"; "umd"]

    let isIdentChar c = System.Char.IsLetterOrDigit (c) || c = '_' || c = '$'

    let hasIdentForbiddenChars (ident: string) =
        let mutable i = 0
        while i < ident.Length && (isIdentChar ident.[i]) do i <- i + 1
        i < ident.Length

    let sanitizeIdentForbiddenChars (ident: string) =
        // Replace the most common external chars in .NET types
        let ident = ident.Replace('.', '_').Replace('`', '_')
        if hasIdentForbiddenChars ident then
            System.String.Concat(seq {
                for c in ident ->
                    if isIdentChar c
                    then string c
                    else sprintf "$%X$" (int c)
                })
        else ident

    let replacePattern (prefix: string) (cond: char->bool) (repl: string->string) (str: string) =
        let rec replace (acc: string) (s: string) =
            let i = s.IndexOf(prefix)
            let mutable i2 = i + prefix.Length;
            while i >= 0 && i2 < s.Length && (cond s.[i2]) do i2 <- i2 + 1
            if i2 = i + prefix.Length then // no match
                if acc.Length > 0 then acc + s else s
            else // match found
                let pattern = s.Substring(i, i2 - i)
                replace (acc + s.Substring(0, i) + (repl pattern)) (s.Substring(i2))
        replace "" str

    let replaceGenericPlaceholder (ident: string, onMatch: string -> string) =
        replacePattern @"\$'" isIdentChar onMatch ident

    let replaceGenericArgsCount (ident: string, replacement: string) =
        replacePattern @"`" System.Char.IsDigit (fun _ -> replacement) ident

    let removeGetSetPrefix (s: string) =
        if s.StartsWith("get_") || s.StartsWith("set_") then
            s.Substring(4)
        else s

    let extensionMethodName (s: string) =
        let i1 = s.IndexOf(".")
        if i1 < 0 then s
        else
            let i2 = s.IndexOf(".", i1 + 1)
            if i2 < 0 then s
            else s.Substring(i1 + 1, i2 - i1 - 1)

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
            "Object"; "LambdaType"; "Boolean"; "Symbol"; "Map"; "Set"; "NaN"; "Number"; "Math"; "Date"; "String"; "RegExp"; "JSON"; "Promise";
            "Array"; "Int8Array"; "Uint8Array"; "Uint8ClampedArray"; "Int16Array"; "Uint16Array"; "Int32Array"; "Uint32Array"; "Float32Array"; "Float64Array";
            // DOM interfaces (https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
            "Attr"; "CharacterData"; "Comment"; "CustomEvent"; "Document"; "DocumentFragment"; "DocumentType"; "DOMError"; "DOMException"; "DOMImplementation";
            "DOMString"; "DOMTimeStamp"; "DOMSettableTokenList"; "DOMStringList"; "DOMTokenList"; "Element"; "Event"; "EventTarget"; "Error"; "HTMLCollection"; "MutationObserver";
            "MutationRecord"; "Node"; "NodeFilter"; "NodeIterator"; "NodeList"; "ProcessingInstruction"; "Range"; "Text"; "TreeWalker"; "URL"; "Window"; "Worker"; "XMLDocument";
            // See #258
            "arguments"
            // See https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
            "fetch"
            // See https://twitter.com/FableCompiler/status/930725972629913600
            "eval"
            // See #1358
            "window"; "console"
        ]

    let preventConflicts conflicts name =
        let rec check n =
            let name = if n > 0 then name + "_" + (string n) else name
            if not (conflicts name) then name else check (n+1)
        check 0

    let sanitizeIdent conflicts name =
        // Replace Forbidden Chars
        let sanitizedName = sanitizeIdentForbiddenChars name
        // Check if it's a keyword or clashes with module ident pattern
        jsKeywords.Contains sanitizedName
        |> function true -> "_" + sanitizedName | false -> sanitizedName
        // Check if it already exists
        |> preventConflicts conflicts

module Path =
    open System

    let Combine (path1: string, path2: string) =
        (path1.TrimEnd [|'\\';'/'|]) + "/" + (path2.TrimStart [|'\\';'/'|])

    let Combine3 (path1: string, path2: string, path3: string) =
        (path1.TrimEnd [|'\\';'/'|]) + "/" + (path2.Trim [|'\\';'/'|]) + "/" + (path3.TrimStart [|'\\';'/'|])

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then path
        else path.Substring(0, i) + ext

    let GetExtension (path: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then ""
        else path.Substring(i)

    let GetFileName (path: string) =
        let normPath = path.Replace("\\", "/").TrimEnd('/')
        let i = normPath.LastIndexOf("/")
        path.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let filename = GetFileName path
        let i = filename.LastIndexOf(".")
        if i < 0 then filename
        else filename.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else path.Substring(0, i)

    let GetFullPath (path: string) =
#if FABLE_COMPILER
        path //TODO: proper implementation
#else
        IO.Path.GetFullPath(path)
#endif

    let normalizePath (path: string) =
        path.Replace("\\", "/")

    let normalizeFullPath (path: string) =
        normalizePath (GetFullPath path)

    /// Creates a relative path from one file or folder to another.
    let getRelativeFileOrDirPath fromIsDir fromFullPath toIsDir toFullPath =
        // Algorithm adapted from http://stackoverflow.com/a/6244188
        let pathDifference (path1: string) (path2: string) =
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
                let mutable builder = ""
                while c < path1.Length do
                    if path1.[c] = '/' then builder <- builder + "../"
                    c <- c + 1
                if builder.Length = 0 && path2.Length - 1 = d
                then "./"
                else builder + path2.Substring(d + 1)
        // Add a dummy file to make it work correctly with dirs
        let addDummyFile isDir path =
            if isDir
            then Combine (path, Naming.dummyFile)
            else path
        // Normalizing shouldn't be necessary at this stage but just in case
        let fromFullPath = normalizePath fromFullPath
        let toFullPath = normalizePath toFullPath
        if fromFullPath.[0] <> toFullPath.[0] then
            // If paths start differently, it means we're on Windows
            // and drive letters are different, so just return the toFullPath
            toFullPath
        else
            let fromPath = addDummyFile fromIsDir fromFullPath
            let toPath = addDummyFile toIsDir toFullPath
            match (pathDifference fromPath toPath).Replace(Naming.dummyFile, "") with
            | path when path.StartsWith "." -> path
            | path -> "./" + path

    let getRelativePath fromFullPath toFullPath =
        // This is not 100% reliable, but IO.Directory.Exists doesn't
        // work either if the directory doesn't exist (e.g. `outDir`)
        let isDir = GetExtension >> String.IsNullOrWhiteSpace
        // let isDir = IO.Directory.Exists
        getRelativeFileOrDirPath (isDir fromFullPath) fromFullPath (isDir toFullPath) toFullPath

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
            |> GetDirectoryName
            |> normalizePath
            |> fun path ->
                path.Split('/') |> Array.filter (String.IsNullOrWhiteSpace >> not))
        |> Seq.toList |> getCommonPrefix
        |> String.concat "/"
