namespace Fable

type ResizeArrayDictionary<'K, 'V when 'K : equality>() =
    let dic = System.Collections.Generic.Dictionary<'K, ResizeArray<'V>>()
    member __.Count = dic.Count
    member __.Add(k: 'K, v: 'V) =
        match dic.TryGetValue(k) with
        | true, xs -> xs.Add(v)
        | false, _ -> dic.Add(k, ResizeArray [|v|])
    member __.Get(k: 'K) =
        match dic.TryGetValue(k) with
        | true, xs -> Seq.toList xs
        | false, _ -> []

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
    { line: int; column: int; }
    static member Empty = { line = 1; column = 0 }

type SourceLocation =
    { start: Position;
      ``end``: Position;
      identifierName: string option }
    static member (+)(r1, r2) =
        { start = r1.start
          ``end`` = r2.start
          identifierName = None }
    static member Empty =
        { start = Position.Empty
          ``end`` = Position.Empty
          identifierName = None }
    override x.ToString() =
        sprintf "(L%i,%i-L%i,%i)"
            x.start.line x.start.column
            x.``end``.line x.``end``.column

[<RequireQualifiedAccess>]
module Tuple =
    let make2 x y = x, y

[<RequireQualifiedAccess>]
module Tuple3 =
    let item1 (x,_,_) = x
    let item2 (_,y,_) = y
    let item3 (_,_,z) = z

[<RequireQualifiedAccess>]
module Array =
    let mapToList (f: 'a -> 'b) (xs: 'a array) =
        let mutable li = []
        for i = xs.Length - 1 downto 0 do
            li <- (f xs.[i])::li
        li

    let chooseToList (f: 'a -> 'b option) (xs: 'a array) =
        let mutable li = []
        for i = xs.Length - 1 downto 0 do
            match f xs.[i] with
            | None -> ()
            | Some x -> li <- x::li
        li

[<RequireQualifiedAccess>]
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

    let splitLast (xs: 'a list) =
        let rec splitListInner acc = function
            | [] -> failwith "List is empty"
            | [x] -> List.rev acc, x
            | x::xs -> splitListInner (x::acc) xs
        splitListInner [] xs

    let replaceLast f (xs: 'a list) =
        let xs = List.toArray xs
        xs.[xs.Length - 1 ] <- f xs.[xs.Length - 1 ]
        List.ofArray xs

    let mapToArray (f: 'a -> 'b) (xs: 'a list) =
        let ar: 'b[] = List.length xs |> Array.zeroCreate
        xs |> List.iteri (fun i x -> ar.[i] <- f x)
        ar

    let mapiToArray (f: int -> 'a -> 'b) (xs: 'a list) =
        let ar: 'b[] = List.length xs |> Array.zeroCreate
        xs |> List.iteri (fun i x -> ar.[i] <- f i x)
        ar

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

    /// This used to be "" for compatibility with Require.js
    /// Mainly used for fable-library imports
    let targetFileExtension = ".js"

    let [<Literal>] fableCompilerConstant = "FABLE_COMPILER"
    let [<Literal>] placeholder = "__PLACE-HOLDER__"
    let [<Literal>] dummyFile = "__DUMMY-FILE__.txt"
    let [<Literal>] fableHiddenDir = ".fable"
    let [<Literal>] unknown = "UNKNOWN"

    let isInFableHiddenDir (file: string) =
        file.Split([|'\\'; '/'|]) |> Array.exists ((=) fableHiddenDir)

    /// Interfaces implemented in the prototype or automatically assigned by the F# compiler to unions and records.
    /// TODO: Allow explicit implementation of the first four?
    let ignoredInterfaces =
        set [ "System.Collections.IStructuralEquatable"
              "System.Collections.IStructuralComparable"
              "System.IEquatable`1"
              "System.IComparable`1"
              "System.IComparable"
              "System.Collections.Generic.IEnumerable"
              "System.Collections.IEnumerable"
              "System.Collections.Generic.IEnumerator`1"
            ]

    let interfaceMethodsImplementedInPrototype =
        set [ "System-IComparable-CompareTo"
              "System-Collections-Generic-IEnumerable`1-GetEnumerator" ]

    /// Methods automatically assigned by the F# compiler for unions and records. Ignored by Fable.
    let ignoredCompilerGenerated =
        set [ "CompareTo"; "Equals"; "GetHashCode" ]

    let umdModules =
        set ["commonjs"; "amd"; "umd"]

    // Dollar sign is reserved by Fable as a special character
    // to encode other characters, unique var names and as a separator
    let isIdentChar index (c: char) =
        let code = int c
        c = '_'
        || (65 <= code && code <= 90)   // a-z
        || (97 <= code && code <= 122)  // A-Z
        // Digits are not allowed in first position, see #1397
        || (index > 0 && 48 <= code && code <= 57) // 0-9

    let hasIdentForbiddenChars (ident: string) =
        let mutable i = 0
        while i < ident.Length && (isIdentChar i ident.[i]) do i <- i + 1
        i < ident.Length

    let sanitizeIdentForbiddenChars (ident: string) =
        if hasIdentForbiddenChars ident then
            System.String.Concat(seq {
                for i = 0 to (ident.Length - 1) do
                    let c = ident.[i]
                    if isIdentChar i c
                    then yield string c
                    else yield "$" + System.String.Format("{0:X}", int c).PadLeft(4, '0')
                })
        else ident

    /// Does not guarantee unique names, only used to clean function constructor names
    let unsafeReplaceIdentForbiddenChars (replacement: char) (ident: string): string =
        ident.ToCharArray() |> Array.mapi (fun i c -> if isIdentChar i c then c else replacement) |> System.String

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
        System.Collections.Generic.HashSet [
            // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
            "abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "self"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield";
            // Standard built-in objects (https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects)
            "Object"; "Function"; "Boolean"; "Symbol"; "Map"; "Set"; "NaN"; "Number"; "Math"; "Date"; "String"; "RegExp"; "JSON"; "Promise";
            "Array"; "Int8Array"; "Uint8Array"; "Uint8ClampedArray"; "Int16Array"; "Uint16Array"; "Int32Array"; "Uint32Array"; "Float32Array"; "Float64Array";
            // DOM interfaces (https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
            "Attr"; "CharacterData"; "Comment"; "CustomEvent"; "Document"; "DocumentFragment"; "DocumentType"; "DOMError"; "DOMException"; "DOMImplementation";
            "DOMString"; "DOMTimeStamp"; "DOMSettableTokenList"; "DOMStringList"; "DOMTokenList"; "Element"; "Event"; "EventTarget"; "Error"; "HTMLCollection"; "MutationObserver";
            "MutationRecord"; "Node"; "NodeFilter"; "NodeIterator"; "NodeList"; "ProcessingInstruction"; "Range"; "Text"; "TreeWalker"; "URL"; "Window"; "Worker"; "XMLDocument";
            // Other JS global and special objects/functions
            // See #258, #1358
            // See https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
            // See https://twitter.com/FableCompiler/status/930725972629913600
            "arguments"; "fetch"; "eval"; "window"; "console"; "global"; "document"
        ]

    // A dollar sign is used to prefix chars encoded in hexadecimal
    // so use two as separator to prevent conflicts
    // (see also `getUniqueName` and `buildName` below)
    let preventConflicts conflicts name =
        let rec check n =
            let name = if n > 0 then name + "$$" + (string n) else name
            if not (conflicts name) then name else check (n+1)
        check 0

    type MemberPart =
        | InstanceMemberPart of string * overloadSuffix: string
        | StaticMemberPart of string * overloadSuffix: string
        | NoMemberPart

    let getUniqueName baseName (index: int) =
        "$" + baseName + "$$" + string index

    let appendSuffix baseName suffix =
        if suffix = ""
        then baseName
        else baseName + "$" + suffix

    let reflectionSuffix = "reflection"

    let private printPart sanitize separator part overloadSuffix =
        (if part = "" then "" else separator + (sanitize part)) +
            (if overloadSuffix = "" then "" else "$$" + overloadSuffix)

    let private buildName sanitize name part =
        (sanitize name) +
            (match part with
                | InstanceMemberPart(s, i) -> printPart sanitize "$$" s i
                | StaticMemberPart(s, i)   -> printPart sanitize "$$$" s i
                | NoMemberPart -> "")

    let buildNameWithoutSanitation name part =
        buildName id name part

    /// This helper is intended for instance and static members in fable-library library compiled from F# (FSharpSet, FSharpMap...)
    let buildNameWithoutSanitationFrom (entityName: string) isStatic memberCompiledName overloadSuffix =
        (if isStatic
            then entityName, StaticMemberPart(memberCompiledName, overloadSuffix)
            else entityName, InstanceMemberPart(memberCompiledName, overloadSuffix))
        ||> buildName id

    let checkJsKeywords name =
        if jsKeywords.Contains name
        then name + "$"
        else name

    let sanitizeIdent conflicts name part =
        // Replace Forbidden Chars
        buildName sanitizeIdentForbiddenChars name part
        |> checkJsKeywords
        // Check if it already exists
        |> preventConflicts conflicts

module Path =
    open System

    let Combine (path1: string, path2: string) =
        let path1 =
            if path1.Length = 0 then path1
            else (path1.TrimEnd [|'\\';'/'|]) + "/"
        path1 + (path2.TrimStart [|'\\';'/'|])

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
        normPath.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let filename = GetFileName path
        let i = filename.LastIndexOf(".")
        if i < 0 then filename
        else filename.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else normPath.Substring(0, i)

    let GetFullPath (path: string) =
#if FABLE_COMPILER
        path //TODO: proper implementation
#else
        IO.Path.GetFullPath(path)
#endif

    let normalizePath (path: string) =
        path.Replace('\\', '/')

    let normalizeFullPath (path: string) =
        normalizePath (GetFullPath path)

    /// If path belongs to a signature file (.fsi), replace the extension with .fs
    let normalizePathAndEnsureFsExtension (path: string) =
        let path = normalizePath path
        if path.EndsWith(".fsi")
        then path.Substring(0, path.Length - 1)
        else path

    /// Checks if path starts with "./", ".\" or ".."
    let isRelativePath (path: string) =
        let len = path.Length
        if len = 0
        then false
        elif path.[0] = '.' then
            if len = 1
            then true
            // Some folders start with a dot, see #1599
            // For simplicity, ignore folders starting with TWO dots
            else match path.[1] with
                    | '/' | '\\' | '.' -> true
                    | _ -> false
        else false

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
            | "" -> "."
            // Some folders start with a period, see #1599
            | path when isRelativePath path -> path
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
