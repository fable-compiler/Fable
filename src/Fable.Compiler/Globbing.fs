module Fable.Compiler.Globbing

open System
open System.Collections.Generic
open System.IO

/// Globbing support and operators
///
/// Forked from `Fake.IO.FileSystem`
///
/// This module contains a file pattern globbing implementation.
[<RequireQualifiedAccess>]
module Glob =
    open System
    open System.Text.RegularExpressions

    // Normalizes path for different OS
    let inline normalizePath (path: string) =
        path
            .Replace('\\', Path.DirectorySeparatorChar)
            .Replace('/', Path.DirectorySeparatorChar)

    type private SearchOption =
        | Directory of string
        | Drive of string
        | Recursive
        | FilePattern of string

    let private checkSubDirs absolute (dir: string) root =
        if dir.Contains "*" then
            try
                Directory.EnumerateDirectories(
                    root,
                    dir,
                    SearchOption.TopDirectoryOnly
                )
                |> Seq.toList
            with :? System.IO.DirectoryNotFoundException ->
                List.empty
        else
            let path = Path.Combine(root, dir)

            let di =
                if absolute then
                    new DirectoryInfo(dir)
                else
                    new DirectoryInfo(path)

            if di.Exists then
                [ di.FullName ]
            else
                []

    let rec private buildPaths acc (input: SearchOption list) =
        match input with
        | [] -> acc
        | Directory name :: t ->
            let subDirs = List.collect (checkSubDirs false name) acc
            buildPaths subDirs t
        | Drive name :: t ->
            let subDirs = List.collect (checkSubDirs true name) acc
            buildPaths subDirs t
        | Recursive :: [] ->
            let dirs =
                Seq.collect
                    (fun dir ->
                        try
                            Directory.EnumerateFileSystemEntries(
                                dir,
                                "*",
                                SearchOption.AllDirectories
                            )
                        with :? System.IO.DirectoryNotFoundException ->
                            Seq.empty
                    )
                    acc

            buildPaths (acc @ Seq.toList dirs) []
        | Recursive :: t ->
            let dirs =
                Seq.collect
                    (fun dir ->
                        try
                            Directory.EnumerateDirectories(
                                dir,
                                "*",
                                SearchOption.AllDirectories
                            )
                        with :? System.IO.DirectoryNotFoundException ->
                            Seq.empty
                    )
                    acc

            buildPaths (acc @ Seq.toList dirs) t
        | FilePattern pattern :: _ ->
            acc
            |> List.collect (fun dir ->
                if Directory.Exists(Path.Combine(dir, pattern)) then
                    [ Path.Combine(dir, pattern) ]
                else
                    try
                        Directory.EnumerateFiles(dir, pattern) |> Seq.toList
                    with
                    | :? System.IO.DirectoryNotFoundException
                    | :? System.IO.PathTooLongException -> []
            )

    let private driveRegex = Regex(@"^[A-Za-z]:$", RegexOptions.Compiled)

    let inline private normalizeOutputPath (p: string) =
        p
            .Replace('\\', Path.DirectorySeparatorChar)
            .Replace('/', Path.DirectorySeparatorChar)
            .TrimEnd(Path.DirectorySeparatorChar)

    let internal getRoot (baseDirectory: string) (pattern: string) =
        let baseDirectory = normalizePath baseDirectory
        let normPattern = normalizePath pattern

        let patternParts =
            normPattern.Split(
                [|
                    '/'
                    '\\'
                |],
                StringSplitOptions.RemoveEmptyEntries
            )

        let patternPathParts =
            patternParts
            |> Seq.takeWhile (fun p -> not (p.Contains("*")))
            |> Seq.toArray

        let globRoot =
            // If we did not find any "*", then drop the last bit (it is a file name, not a pattern)
            (if patternPathParts.Length = patternParts.Length then
                 patternPathParts.[0 .. patternPathParts.Length - 2]
             else
                 patternPathParts)
            |> String.concat (Path.DirectorySeparatorChar.ToString())

        let globRoot =
            // If we dropped "/" from the beginning of the path in the 'Split' call, put it back!
            if normPattern.StartsWith('/') then
                "/" + globRoot
            else
                globRoot

        if Path.IsPathRooted globRoot then
            globRoot
        else
            Path.Combine(baseDirectory, globRoot)

    let internal search (baseDir: string) (originalInput: string) =
        let baseDir = normalizePath baseDir
        let input = normalizePath originalInput

        let input =
            if String.IsNullOrEmpty baseDir then
                input
            else
                // The final \ (or /) makes sure to only match complete folder
                // names (as one folder name could be a substring of the other)
                let start =
                    baseDir.TrimEnd([| Path.DirectorySeparatorChar |])
                    + string<char> Path.DirectorySeparatorChar
                // See https://github.com/fsharp/FAKE/issues/1925
                if input.StartsWith(start, StringComparison.Ordinal) then
                    input.Substring start.Length
                else
                    input

        let filePattern = Path.GetFileName(input)

        let splits =
            input.Split(
                [|
                    '/'
                    '\\'
                |],
                StringSplitOptions.None
            )

        let baseItems =
            let start, rest =
                if
                    input.StartsWith("\\\\", StringComparison.Ordinal)
                    && splits.Length >= 4
                then
                    let serverName = splits.[2]
                    let share = splits.[3]

                    [ Directory(sprintf "\\\\%s\\%s" serverName share) ],
                    splits |> Seq.skip 4
                elif
                    splits.Length >= 2
                    && Path.IsPathRooted input
                    && driveRegex.IsMatch splits.[0]
                then
                    [ Directory(splits.[0] + "\\") ], splits |> Seq.skip 1
                elif
                    splits.Length >= 2
                    && Path.IsPathRooted input
                    && input.StartsWith '/'
                then
                    [ Directory("/") ], splits |> Array.toSeq
                else
                    if Path.IsPathRooted input then
                        if input.StartsWith '\\' then
                            failwithf
                                "Please remove the leading '\\' or '/' and replace them with \
                                       '.\\' or './' if you want to use a relative path. Leading \
                                       slashes are considered an absolute path (input was '%s')!"
                                originalInput
                        else
                            failwithf
                                "Unknown globbing input '%s', try to use a \
                                       relative path and report an issue!"
                                originalInput

                    [], splits |> Array.toSeq

            let restList =
                rest
                |> Seq.filter (String.IsNullOrEmpty >> not)
                |> Seq.map (
                    function
                    | "**" -> Recursive
                    | a when a = filePattern -> FilePattern(a)
                    | a -> Directory(a)
                )
                |> Seq.toList

            start @ restList

        baseItems |> buildPaths [ baseDir ] |> List.map normalizeOutputPath

    let internal compileGlobToRegex pattern =
        let pattern = normalizePath pattern

        let escapedPattern = (Regex.Escape pattern)

        let regexPattern =
            let xTOy =
                [
                    "dirwildcard", (@"\\\*\\\*(/|\\\\)", @"(.*(/|\\))?")
                    "stardotstar", (@"\\\*\\.\\\*", @"([^\\/]*)")
                    "wildcard", (@"\\\*", @"([^\\/]*)")
                ]
                |> List.map (fun (key, (pattern, replace)) ->
                    let pattern = sprintf "(?<%s>%s)" key pattern
                    key, (pattern, replace)
                )

            let xTOyMap = xTOy |> Map.ofList

            let replacePattern =
                xTOy
                |> List.map (fun x -> x |> snd |> fst)
                |> String.concat ("|")

            let replaced =
                Regex(replacePattern)
                    .Replace(
                        escapedPattern,
                        fun m ->
                            let matched =
                                xTOy
                                |> Seq.map (fst)
                                |> Seq.find (fun n -> m.Groups.Item(n).Success)

                            (xTOyMap |> Map.tryFind matched).Value |> snd
                    )

            "^" + replaced + "$"

        Regex(regexPattern)

    let private globRegexCache =
        System.Collections.Concurrent.ConcurrentDictionary<string, Regex>()

    let isMatch pattern path : bool =
        let path = normalizePath path

        let regex =
            let outRegex: ref<Regex> = ref null

            if globRegexCache.TryGetValue(pattern, outRegex) then
                outRegex.Value
            else
                let compiled = compileGlobToRegex pattern
                globRegexCache.TryAdd(pattern, compiled) |> ignore
                compiled

        regex.IsMatch(path)

type IGlobbingPattern =
    inherit IEnumerable<string>
    abstract BaseDirectory: string
    abstract Includes: string list
    abstract Excludes: string list

type LazyGlobbingPattern =
    {
        BaseDirectory: string
        Includes: string list
        Excludes: string list
    }

    interface IGlobbingPattern with
        member this.BaseDirectory = this.BaseDirectory
        member this.Includes = this.Includes
        member this.Excludes = this.Excludes

    interface IEnumerable<string> with

        member this.GetEnumerator() =
            let hashSet = HashSet()

            let excludes =
                seq {
                    for pattern in this.Excludes do
                        yield! Glob.search this.BaseDirectory pattern
                }
                |> Set.ofSeq

            let files =
                seq {
                    for pattern in this.Includes do
                        yield! Glob.search this.BaseDirectory pattern
                }
                |> Seq.filter (fun x -> not (Set.contains x excludes))
                |> Seq.filter (fun x -> hashSet.Add x)

            files.GetEnumerator()

        member this.GetEnumerator() =
            (this :> IEnumerable<string>).GetEnumerator()
            :> System.Collections.IEnumerator

type ResolvedGlobbingPattern =
    {
        BaseDirectory: string
        Includes: string list
        Excludes: string list
        Results: string list
    }

    interface IGlobbingPattern with
        member this.BaseDirectory = this.BaseDirectory
        member this.Includes = this.Includes
        member this.Excludes = this.Excludes

    interface IEnumerable<string> with
        member this.GetEnumerator() =
            (this.Results :> IEnumerable<string>).GetEnumerator()

        member this.GetEnumerator() =
            (this :> IEnumerable<string>).GetEnumerator()
            :> System.Collections.IEnumerator

[<AutoOpen>]
module GlobbingPatternExtensions =
    type IGlobbingPattern with

        member internal this.Pattern =
            match this with
            | :? LazyGlobbingPattern as l -> l
            | _ ->
                {
                    BaseDirectory = this.BaseDirectory
                    Includes = this.Includes
                    Excludes = this.Excludes
                }

        member this.Resolve() =
            match this with
            | :? ResolvedGlobbingPattern as res -> res :> IGlobbingPattern
            | _ ->
                let list = this |> Seq.toList

                {
                    BaseDirectory = this.BaseDirectory
                    Includes = this.Includes
                    Excludes = this.Excludes
                    Results = list
                }
                :> IGlobbingPattern

        /// Adds the given pattern to the file includes
        member this.And pattern =
            { this.Pattern with Includes = this.Includes @ [ pattern ] }
            :> IGlobbingPattern

        /// Ignores files with the given pattern
        member this.ButNot pattern =
            { this.Pattern with Excludes = pattern :: this.Excludes }
            :> IGlobbingPattern

        /// Sets a directory as BaseDirectory.
        member this.SetBaseDirectory(dir: string) =
            { this.Pattern with
                BaseDirectory = dir.TrimEnd(Path.DirectorySeparatorChar)
            }
            :> IGlobbingPattern

        /// Checks if a particular file is matched
        member this.IsMatch(path: string) =
            let fullDir (pattern: string) =
                if Path.IsPathRooted(pattern) then
                    pattern
                else
                    System.IO.Path.Combine(this.BaseDirectory, pattern)

            let fullPath = Path.GetFullPath path

            let included =
                this.Includes
                |> List.exists (fun fileInclude ->
                    Glob.isMatch (fullDir fileInclude) fullPath
                )

            let excluded =
                this.Excludes
                |> List.exists (fun fileExclude ->
                    Glob.isMatch (fullDir fileExclude) fullPath
                )

            included && not excluded

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module GlobbingPattern =
    let private defaultBaseDir = Path.GetFullPath "."

    /// Include files
    let create x =
        {
            BaseDirectory = defaultBaseDir
            Includes = [ x ]
            Excludes = []
        }
        :> IGlobbingPattern

    /// Start an empty globbing pattern from the specified directory
    let createFrom (dir: string) =
        {
            BaseDirectory = dir
            Includes = []
            Excludes = []
        }
        :> IGlobbingPattern

    /// Sets a directory as baseDirectory for fileIncludes.
    let setBaseDir (dir: string) (fileIncludes: IGlobbingPattern) =
        fileIncludes.SetBaseDirectory dir

    /// Get base include directories.
    ///
    /// Used to get a smaller set of directories from a globbing pattern.
    let getBaseDirectoryIncludes (fileIncludes: IGlobbingPattern) =
        let directoryIncludes =
            fileIncludes.Includes
            |> Seq.map (fun file ->
                Glob.getRoot fileIncludes.BaseDirectory file
            )

        // remove subdirectories
        directoryIncludes
        |> Seq.filter (fun d ->
            directoryIncludes
            |> Seq.exists (fun p ->
                d.StartsWith(
                    p + string<char> Path.DirectorySeparatorChar,
                    StringComparison.Ordinal
                )
                && p <> d
            )
            |> not
        )
        |> Seq.toList

/// Contains operators to find and process files.
///
/// ### Simple glob using as list
///
///     let csProjectFiles = !! "src/*.csproj"
///
///     for projectFile in csProjectFiles do
///         printf "F# ProjectFile: %s" projectFile
///
/// ### Combine globs
///
///     let projectFiles =
///         !! "src/*/*.*proj"
///         ++ "src/*/*.target"
///         -- "src/*/*.vbproj"
///
///     for projectFile in projectFiles do
///         printf "ProjectFile: %s" projectFile
///
module Operators =
    /// Add Include operator
    let inline (++) (x: IGlobbingPattern) pattern = x.And pattern

    /// Exclude operator
    let inline (--) (x: IGlobbingPattern) pattern = x.ButNot pattern

    /// Includes a single pattern and scans the files - !! x = AllFilesMatching x
    let inline (!!) x = GlobbingPattern.create x
