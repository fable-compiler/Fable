namespace Fable

open System
open System.Globalization
open System.Text

[<AutoOpen>]
module Extensions =
    type String with

        member str.StartsWithAny([<ParamArray>] patterns: string[]) =
            patterns
            |> Array.exists (fun p ->
                str.StartsWith(p, StringComparison.Ordinal)
            )

module Dictionary =
    open System.Collections.Generic

    let tryFind key (dic: #IDictionary<'Key, 'Value>) =
        match dic.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

module ReadOnlyDictionary =
    open System.Collections.Generic

    let tryFind key (dic: #IReadOnlyDictionary<'Key, 'Value>) =
        match dic.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

[<RequireQualifiedAccess>]
module Tuple =
    let make2 x y = x, y

[<RequireQualifiedAccess>]
module Tuple3 =
    let item1 (x, _, _) = x
    let item2 (_, y, _) = y
    let item3 (_, _, z) = z

[<RequireQualifiedAccess>]
module Option =
    let tap (f: 'a -> unit) (x: 'a option) : 'a option =
        match x with
        | Some a -> f a
        | None -> ()

        x

[<RequireQualifiedAccess>]
module Map =
    let matchesKeyValue k v map =
        match Map.tryFind k map with
        | None -> false
        | Some v2 -> v = v2

[<RequireQualifiedAccess>]
module Seq =
    let mapToList (f: 'a -> 'b) (xs: 'a seq) : 'b list =
        ([], xs) ||> Seq.fold (fun li x -> (f x) :: li) |> List.rev

    let mapToArray (f: 'a -> 'b) (xs: 'a seq) : 'b[] =
        let ar = ResizeArray()
        xs |> Seq.iter (fun x -> ar.Add(f x))
        ar.ToArray()

    let mapiToList (f: int -> 'a -> 'b) (xs: 'a seq) =
        let mutable i = -1

        ([], xs)
        ||> Seq.fold (fun li x ->
            i <- i + 1
            (f i x) :: li
        )
        |> List.rev

    let chooseToList (f: 'a -> 'b option) (xs: 'a seq) =
        ([], xs)
        ||> Seq.fold (fun li x ->
            match f x with
            | Some x -> x :: li
            | None -> li
        )
        |> List.rev

[<RequireQualifiedAccess>]
module Array =
    let filteri (filter: int -> 'a -> bool) (xs: 'a[]) : 'a[] =
        let mutable i = -1

        xs
        |> Array.filter (fun x ->
            i <- i + 1
            filter i x
        )

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
            li <- (f xs.[i]) :: li

        li

    let chooseToList (f: 'a -> 'b option) (xs: 'a array) =
        let mutable li = []

        for i = xs.Length - 1 downto 0 do
            match f xs.[i] with
            | None -> ()
            | Some x -> li <- x :: li

        li

    let splitWhile (f: 'a -> bool) (xs: 'a array) =
        Array.tryFindIndex (f >> not) xs
        |> function
            | Some i -> Array.splitAt i xs
            | None -> xs, [||]

[<RequireQualifiedAccess>]
module List =
    let isSingle =
        function
        | [ _ ] -> true
        | _ -> false

    /// Same as List.length xs > 1 but doesn't calculate the whole length
    let isMultiple =
        function
        | []
        | [ _ ] -> false
        | _ -> true

    let rec sameLength xs1 xs2 =
        match xs1, xs2 with
        | [], [] -> true
        | [ _ ], [ _ ] -> true
        | _ :: xs1, _ :: xs2 -> sameLength xs1 xs2
        | _ -> false

    let splitLast (xs: 'a list) =
        let rec splitListInner acc =
            function
            | [] -> failwith "List is empty"
            | [ x ] -> List.rev acc, x
            | x :: xs -> splitListInner (x :: acc) xs

        splitListInner [] xs

    let replaceLast f (xs: 'a list) =
        let xs = List.toArray xs
        xs.[xs.Length - 1] <- f xs.[xs.Length - 1]
        List.ofArray xs

    let collecti (f: int -> 'a -> 'b list) (xs: 'a list) =
        let mutable i = -1

        xs
        |> List.collect (fun x ->
            i <- i + 1
            f i x
        )

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
            | x :: xs, y :: ys -> inner ((x, y) :: acc) xs ys
            | _ -> List.rev acc

        inner [] xs ys

[<RequireQualifiedAccess>]
module Result =
    let mapEither mapOk mapError =
        function
        | Ok x -> mapOk x |> Ok
        | Error e -> mapError e |> Error

    let extract extractOk extractError =
        function
        | Ok x -> extractOk x
        | Error e -> extractError e

module Patterns =
    let (|Try|_|) (f: 'a -> 'b option) a = f a

    let (|Run|) (f: 'a -> 'b) a = f a

    let (|DicContains|_|)
        (dic: System.Collections.Generic.IDictionary<'k, 'v>)
        key
        =
        let success, value = dic.TryGetValue key

        if success then
            Some value
        else
            None

    let (|SetContains|_|) set item =
        if Set.contains item set then
            Some SetContains
        else
            None

    let (|ListLast|_|) (xs: 'a list) =
        if List.isEmpty xs then
            None
        else
            let xs, last = List.splitLast xs
            Some(xs, last)

module Path =
    open System

    [<Literal>]
    let dummyFile = "__DUMMY-FILE__.txt"

    let normalizePath (path: string) = path.Replace('\\', '/').TrimEnd('/')

    let Combine (path1: string, path2: string) =
#if FABLE_COMPILER
        // TODO: Make sure path2 is not absolute in the polyfill
        let path1 =
            if path1.Length = 0 then
                path1
            else
                (path1.TrimEnd
                    [|
                        '\\'
                        '/'
                    |])
                + "/"

        let path2 =
            if path2.StartsWith("./") then
                path2.[2..]
            else
                path2.TrimStart
                    [|
                        '\\'
                        '/'
                    |]

        path1 + path2
#else
        IO.Path.Combine(path1, path2)
#endif

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")

        if i < 0 then
            path
        else
            path.Substring(0, i) + ext

    let GetExtension (path: string) =
        let i = path.LastIndexOf(".")

        if i < 0 then
            ""
        else
            path.Substring(i)

    let GetFileName (path: string) =
        let normPath = normalizePath path
        let i = normPath.LastIndexOf("/")
        normPath.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let filename = GetFileName path
        let i = filename.LastIndexOf(".")

        if i < 0 then
            filename
        else
            filename.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = normalizePath path
        let i = normPath.LastIndexOf("/")

        if i < 0 then
            ""
        else
            normPath.Substring(0, i)

    let GetDirectoryAndFileNames (path: string) =
        let normPath = normalizePath path
        let i = normPath.LastIndexOf("/")

        if i < 0 then
            "", normPath
        else
            normPath.Substring(0, i), normPath.Substring(i + 1)

    let IsPathRooted (path: string) : bool =
#if FABLE_COMPILER
        path.StartsWith("/") || path.StartsWith("\\") || path.IndexOf(":") = 1
#else
        IO.Path.IsPathRooted(path)
#endif

    let GetFullPath (path: string) : string =
#if FABLE_COMPILER
        // In the REPL we just remove the dot dirs as in foo/.././bar > bar
        let rec removeDotDirs acc parts =
            match acc, parts with
            | _, [] -> List.rev acc |> String.concat "/"
            | _, "." :: rest -> removeDotDirs acc rest
            | _parent :: acc, ".." :: rest -> removeDotDirs acc rest
            | acc, part :: rest -> removeDotDirs (part :: acc) rest

        path.Split('/') |> Array.toList |> removeDotDirs []
#else
        IO.Path.GetFullPath(path)
#endif

    let normalizeFullPath (path: string) = normalizePath (GetFullPath path)

    /// If path belongs to a signature file (.fsi), replace the extension with .fs
    let ensureFsExtension (path: string) =
        if path.EndsWith(".fsi", StringComparison.Ordinal) then
            path.Substring(0, path.Length - 1)
        else
            path

    let normalizePathAndEnsureFsExtension (path: string) =
        normalizePath path |> ensureFsExtension

    /// Checks if path starts with "./", ".\" or ".."
    let isRelativePath (path: string) =
        let len = path.Length

        if len = 0 then
            false
        elif path.[0] = '.' then
            if len = 1 then
                true
            // Some folders start with a dot, see #1599
            // For simplicity, ignore folders starting with TWO dots
            else
                match path.[1] with
                | '/'
                | '\\'
                | '.' -> true
                | _ -> false
        else
            false

    /// Creates a relative path from one file or folder to another.
    let getRelativeFileOrDirPath
        fromIsDir
        (fromFullPath: string)
        toIsDir
        (toFullPath: string)
        =
        // Algorithm adapted from http://stackoverflow.com/a/6244188
        let pathDifference (path1: string) (path2: string) =
            let mutable c = 0 //index up to which the paths are the same
            let mutable d = -1 //index of trailing slash for the portion where the paths are the s

            while c < path1.Length && c < path2.Length && path1.[c] = path2.[c] do
                if path1.[c] = '/' then
                    d <- c

                c <- c + 1

            if c = 0 then
                path2
            elif c = path1.Length && c = path2.Length then
                String.Empty
            else
                let mutable builder = ""

                while c < path1.Length do
                    if path1.[c] = '/' then
                        builder <- builder + "../"

                    c <- c + 1

                if builder.Length = 0 && path2.Length - 1 = d then
                    "./"
                else
                    builder + path2.Substring(d + 1)
        // Add a dummy file to make it work correctly with dirs
        let addDummyFile isDir path =
            if isDir then
                Combine(path, dummyFile)
            else
                path

        if fromFullPath.[0] <> toFullPath.[0] then
            // If paths start differently, it means we're on Windows
            // and drive letters are different, so just return the toFullPath
            toFullPath
        else
            let fromPath = addDummyFile fromIsDir fromFullPath |> normalizePath
            let toPath = addDummyFile toIsDir toFullPath |> normalizePath

            match (pathDifference fromPath toPath).Replace(dummyFile, "") with
            | "" -> "."
            // Some folders start with a period, see #1599
            | path when isRelativePath path -> path
            | path -> "./" + path

    let getRelativePath fromFullPath toFullPath =
        // This is not 100% reliable, but IO.Directory.Exists doesn't
        // work either if the directory doesn't exist (e.g. `outDir`)
        let isDir = GetExtension >> String.IsNullOrWhiteSpace
        // let isDir = IO.Directory.Exists
        getRelativeFileOrDirPath
            (isDir fromFullPath)
            fromFullPath
            (isDir toFullPath)
            toFullPath

    let getCommonPrefix (xs: string[] list) =
        let rec getCommonPrefix (prefix: string[]) =
            function
            | [] -> prefix
            | (x: string[]) :: xs ->
                let mutable i = 0

                while i < prefix.Length && i < x.Length && x.[i] = prefix.[i] do
                    i <- i + 1

                getCommonPrefix prefix.[0 .. i - 1] xs

        match xs with
        | [] -> [||]
        | x :: xs -> getCommonPrefix x xs

    let isChildPath (parent: string) (child: string) =
        let split x =
            (normalizeFullPath x).Split('/')
            |> Array.filter (String.IsNullOrWhiteSpace >> not)

        let parent = split parent
        let child = split child

        let commonPrefix =
            getCommonPrefix
                [
                    parent
                    child
                ]

        commonPrefix.Length >= parent.Length

    let getCommonBaseDir (filePaths: seq<string>) =
        filePaths
        |> Seq.map (fun filePath ->
            filePath
            |> GetDirectoryName
            |> normalizePath
            |> fun path ->
                path.Split('/')
                |> Array.filter (String.IsNullOrWhiteSpace >> not)
        )
        |> Seq.toList
        |> getCommonPrefix
        |> String.concat "/"
