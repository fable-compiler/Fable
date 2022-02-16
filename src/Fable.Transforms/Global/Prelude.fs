namespace Fable

[<RequireQualifiedAccess>]
module Tuple =
    let make2 x y = x, y

[<RequireQualifiedAccess>]
module Tuple3 =
    let item1 (x,_,_) = x
    let item2 (_,y,_) = y
    let item3 (_,_,z) = z

[<RequireQualifiedAccess>]
module Option =
    let tap (f: 'a -> unit) (x: 'a option): 'a option =
        match x with Some a -> f a | None -> ()
        x

[<RequireQualifiedAccess>]
module Seq =
    let mapToList (f: 'a -> 'b) (xs: 'a seq) =
        ([], xs) ||> Seq.fold (fun li x -> (f x)::li) |> List.rev

[<RequireQualifiedAccess>]
module Array =
    let partitionBy (f: 'T -> Choice<'T1, 'T2>) (xs: 'T[]) =
        let r1 = ResizeArray()
        let r2 = ResizeArray()
        for x in xs do
            match f x with
            | Choice1Of2 x -> r1.Add(x)
            | Choice2Of2 x -> r2.Add(x)
        r1.ToArray(), r2.ToArray()

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

    let splitWhile (f: 'a -> bool) (xs: 'a array) =
        Array.tryFindIndex (f >> not) xs
        |> function
        | Some i -> Array.splitAt i xs
        | None -> xs, [||]

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

    let collecti (f: int -> 'a -> 'b list) (xs: 'a list) =
        let mutable i = -1
        xs |> List.collect (fun x -> i <- i + 1; f i x)

    let chooseToArray (f: 'a -> 'b option) (xs: 'a list) =
        let ar = ResizeArray()
        for x in xs do
            match f x with
            | None -> ()
            | Some x -> ar.Add(x)
        ar.ToArray()

    let mapToArray (f: 'a -> 'b) (xs: 'a list) =
        let ar: 'b[] = List.length xs |> Array.zeroCreate
        xs |> List.iteri (fun i x -> ar.[i] <- f x)
        ar

    let mapiToArray (f: int -> 'a -> 'b) (xs: 'a list) =
        let ar: 'b[] = List.length xs |> Array.zeroCreate
        xs |> List.iteri (fun i x -> ar.[i] <- f i x)
        ar

    let splitWhile (f: 'a -> bool) (xs: 'a list) =
        List.tryFindIndex (f >> not) xs
        |> function
        | Some i -> List.splitAt i xs
        | None -> xs, []

    /// Only zips first elements until length differs
    let zipSafe (xs: 'T1 list) (ys: 'T2 list) =
        let rec inner acc xs ys =
            match xs, ys with
            | x::xs, y::ys -> inner ((x,y)::acc) xs ys
            | _ -> List.rev acc
        inner [] xs ys

[<RequireQualifiedAccess>]
module Result =
    let mapEither mapOk mapError = function
        | Ok x -> mapOk x |> Ok
        | Error e -> mapError e |> Error

    let extract extractOk extractError = function
        | Ok x -> extractOk x
        | Error e -> extractError e

module Patterns =
    let (|Try|_|) (f: 'a -> 'b option) a = f a

    let (|DicContains|_|) (dic: System.Collections.Generic.IDictionary<'k,'v>) key =
        let success, value = dic.TryGetValue key
        if success then Some value else None

    let (|SetContains|_|) set item =
        if Set.contains item set then Some SetContains else None

module Naming =
    open Fable.Core
    open System.Text.RegularExpressions

    let (|StartsWith|_|) (pattern: string) (txt: string) =
        if txt.StartsWith(pattern)
        then txt.Substring(pattern.Length) |> Some
        else None

    let (|EndsWith|_|) (pattern: string) (txt: string) =
        if txt.EndsWith(pattern)
        then txt.Substring(0, txt.Length - pattern.Length) |> Some
        else None

    let [<Literal>] fableCompilerConstant = "FABLE_COMPILER"
    let [<Literal>] placeholder = "__PLACE-HOLDER__"
    let [<Literal>] dummyFile = "__DUMMY-FILE__.txt"
    let [<Literal>] fableModules = "fable_modules"
    let [<Literal>] fablePrecompile = "Fable.Precompiled"
    let [<Literal>] fableProjExt = ".fableproj"
    let [<Literal>] unknown = "UNKNOWN"

    let isInFableModules (file: string) =
        file.Split([|'\\'; '/'|]) |> Array.exists ((=) fableModules)

    let isIdentChar index (c: char) =
        let code = int c
        c = '_' || c = '$'
        || (65 <= code && code <= 90)   // a-z
        || (97 <= code && code <= 122)  // A-Z
        // Digits are not allowed in first position, see #1397
        || (index > 0 && 48 <= code && code <= 57) // 0-9

    let hasIdentForbiddenChars (ident: string) =
        let mutable found = false
        for i = 0 to ident.Length - 1 do
            found <- found || not(isIdentChar i ident.[i])
        found

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

    let replaceRegex (pattern: string) (value: string) (input: string) =
        Regex.Replace(input, pattern, value)

    let replacePrefix (prefix: string) (value: string) (input: string) =
        if input.StartsWith(prefix) then
            value + (input.Substring(prefix.Length))
        else input

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

    let private dashify (separator: string) (input: string) =
        Regex.Replace(input, "[a-z]?[A-Z]", fun m ->
            if m.Value.Length = 1 then m.Value.ToLowerInvariant()
            else m.Value.Substring(0,1) + separator + m.Value.Substring(1,1).ToLowerInvariant())

    let applyCaseRule caseRule name =
        match caseRule with
        | CaseRules.LowerFirst -> lowerFirst name
        | CaseRules.SnakeCase -> dashify "_" name
        | CaseRules.SnakeCaseAllCaps -> (dashify "_" name).ToUpperInvariant()
        | CaseRules.KebabCase -> dashify "-" name
        | CaseRules.None | _ -> name

    let jsKeywords =
        System.Collections.Generic.HashSet [
            // Keywords: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
            "break"
            "case"
            "catch"
            "class"
            "const"
            "continue"
            "debugger"
            "default"
            "delete"
            "do"
            "else"
            "export"
            "extends"
            "finally"
            "for"
            "function"
            "if"
            "import"
            "in"
            "instanceof"
            "new"
            "return"
            "super"
            "switch"
            "this"
            "throw"
            "try"
            "typeof"
            "var"
            "void"
            "while"
            "with"
            "yield"

            "enum"

            "implements"
            "interface"
            "let"
            "package"
            "private"
            "protected"
            "public"
            "static"

            "await"

            "null"
            "true"
            "false"
            "arguments"
            "get"
            "set"

            // Standard built-in objects: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects
            "Infinity"
            "NaN"
            "undefined"
            "globalThis"

            "eval"
            "uneval"
            "isFinite"
            "isNaN"
            "parseFloat"
            "parseInt"
            "decodeURI"
            "decodeURIComponent"
            "encodeURI"
            "encodeURIComponent"

            "Object"
            "Function"
            "Boolean"
            "Symbol"

            "Error"
            "AggregateError"
            "EvalError"
            "InternalError"
            "RangeError"
            "ReferenceError"
            "SyntaxError"
            "TypeError"
            "URIError"

            "Number"
            "BigInt"
            "Math"
            "Date"

            "String"
            "RegExp"

            "Array"
            "Int8Array"
            "Uint8Array"
            "Uint8ClampedArray"
            "Int16Array"
            "Uint16Array"
            "Int32Array"
            "Uint32Array"
            "Float32Array"
            "Float64Array"
            "BigInt64Array"
            "BigUint64Array"

            "Map"
            "Set"
            "WeakMap"
            "WeakSet"

            "ArrayBuffer"
            "SharedArrayBuffer"
            "Atomics"
            "DataView"
            "JSON"

            "Promise"
            "Generator"
            "GeneratorFunction"
            "AsyncFunction"

            "Reflect"
            "Proxy"

            "Intl"
            "WebAssembly"

            // DOM interfaces (omitting SVG): https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model
            "Attr"
            "CDATASection"
            "CharacterData"
            "ChildNode"
            "Comment"
            "CustomEvent"
            "Document"
            "DocumentFragment"
            "DocumentType"
            "DOMError"
            "DOMException"
            "DOMImplementation"
            "DOMString"
            "DOMTimeStamp"
            "DOMStringList"
            "DOMTokenList"
            "Element"
            "Event"
            "EventTarget"
            "HTMLCollection"
            "MutationObserver"
            "MutationRecord"
            "NamedNodeMap"
            "Node"
            "NodeFilter"
            "NodeIterator"
            "NodeList"
            "NonDocumentTypeChildNode"
            "ParentNode"
            "ProcessingInstruction"
            "Selection"
            "Range"
            "Text"
            "TextDecoder"
            "TextEncoder"
            "TimeRanges"
            "TreeWalker"
            "URL"
            "Window"
            "Worker"
            "XMLDocument"

            // Other JS global and special objects/functions. See #258, #1358
            "console"
            "window"
            "document"
            "global"
            "fetch"
        ]

    let preventConflicts conflicts originalName =
        let rec check originalName n =
            let name = if n > 0 then originalName + "_" + (string n) else originalName
            if not (conflicts name) then name else check originalName (n+1)
        check originalName 0

    // TODO: Move this to FSharp2Fable.Util
    type MemberPart =
        | InstanceMemberPart of string * overloadSuffix: string
        | StaticMemberPart of string * overloadSuffix: string
        | NoMemberPart
        member this.Replace(f: string -> string) =
            match this with
            | InstanceMemberPart(s, o) -> InstanceMemberPart(f s, o)
            | StaticMemberPart(s, o) -> StaticMemberPart(f s, o)
            | NoMemberPart -> this

        member this.OverloadSuffix =
            match this with
            | InstanceMemberPart(_,o)
            | StaticMemberPart(_,o) -> o
            | NoMemberPart -> ""

    let reflectionSuffix = "$reflection"

    let private printPart sanitize separator part overloadSuffix =
        (if part = "" then "" else separator + (sanitize part)) +
            (if overloadSuffix = "" then "" else "_" + overloadSuffix)

    let private buildName sanitize name part =
        (sanitize name) +
            (match part with
                | InstanceMemberPart(s, i) -> printPart sanitize "__" s i
                | StaticMemberPart(s, i) -> printPart sanitize "_" s i
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

    let normalizePath (path: string) =
        path.Replace('\\', '/').TrimEnd('/')

    let Combine (path1: string, path2: string) =
        let path1 =
            if path1.Length = 0 then path1
            else (path1.TrimEnd [|'\\';'/'|]) + "/"
        let path2 =
            if path2.StartsWith("./") then path2.[2..]
            else path2.TrimStart [|'\\';'/'|]
        path1 + path2

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then path
        else path.Substring(0, i) + ext

    let GetExtension (path: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then ""
        else path.Substring(i)

    let GetFileName (path: string) =
        let normPath = normalizePath path
        let i = normPath.LastIndexOf("/")
        normPath.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let filename = GetFileName path
        let i = filename.LastIndexOf(".")
        if i < 0 then filename
        else filename.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = normalizePath path
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else normPath.Substring(0, i)

    let GetDirectoryAndFileNames (path: string) =
        let normPath = normalizePath path
        let i = normPath.LastIndexOf("/")
        if i < 0 then "", normPath
        else normPath.Substring(0, i), normPath.Substring(i + 1)

    let GetFullPath (path: string): string =
#if FABLE_COMPILER
        // In the REPL we just remove the dot dirs as in foo/.././bar > bar
        let rec removeDotDirs acc parts =
            match acc, parts with
            | _, [] -> List.rev acc |> String.concat "/"
            | _, "."::rest -> removeDotDirs acc rest
            | _parent::acc, ".."::rest -> removeDotDirs acc rest
            | acc, part::rest -> removeDotDirs (part::acc) rest
        path.Split('/')
        |> Array.toList
        |> removeDotDirs []
#else
        IO.Path.GetFullPath(path)
#endif

    let normalizeFullPath (path: string) =
        normalizePath (GetFullPath path)

    /// If path belongs to a signature file (.fsi), replace the extension with .fs
    let ensureFsExtension (path: string) =
        if path.EndsWith(".fsi")
        then path.Substring(0, path.Length - 1)
        else path

    let normalizePathAndEnsureFsExtension (path: string) =
        normalizePath path |> ensureFsExtension

    let replaceExtension (newExt: string) (path: string) =
        let i = path.LastIndexOf(".")
        if i > 0 then path.Substring(0, i) + newExt
        else path + newExt

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
