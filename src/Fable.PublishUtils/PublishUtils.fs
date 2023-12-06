module PublishUtils

open System
open System.Text.RegularExpressions

type JsonEl = System.Text.Json.JsonElement

type IJson =
    abstract Parse: string -> JsonEl
    abstract TryGetProperty: string -> JsonEl -> JsonEl option
    abstract GetString: JsonEl -> string

type IFile =
    abstract GetBytesLength: string -> float
    abstract Exists: string -> bool
    abstract Delete: string -> unit
    abstract Copy: source: string * target: string * overwrite: bool -> unit
    abstract WriteAllText: path: string * contents: string -> unit
    abstract ReadAllText: string -> string
    abstract ReadLines: string -> string seq

type IPath =
    abstract Combine: string * string -> string
    abstract GetFullPath: string -> string
    abstract GetDirectoryName: string -> string
    abstract GetFileName: string -> string
    abstract GetTempPath: unit -> string

type IDirectory =
    abstract DeleteEmpty: string -> unit
    abstract Exists: string -> bool
    abstract GetFiles: string -> string[]
    abstract GetDirectories: string -> string[]
    abstract GetCurrentDirectory: unit -> string
    abstract CreateDirectory: string -> unit

type IEnvironment =
    abstract GetEnvironmentVariable: string -> string
    abstract SetEnvironmentVariable: string * string -> unit
    abstract IsWindows: unit -> bool

type IProcess =
    abstract Run: cwd: string * exe: string * args: string[] -> unit
    abstract RunAsync: cwd: string * exe: string * args: string[] -> Async<unit>

module Platform =
    open System.Runtime
    open System.Text.Json

    module IO =
        let File =
            { new IFile with
                member _.GetBytesLength(p: string) =
                    System.IO.FileInfo(p).Length |> float

                member _.Exists(p: string) : bool = System.IO.File.Exists(p)

                member _.Delete(p: string) = System.IO.File.Delete(p)

                member _.Copy(source: string, target: string, overwrite: bool) =
                    System.IO.File.Copy(source, target, overwrite)

                member _.ReadAllText(p: string) : string =
                    System.IO.File.ReadAllText(p)

                member _.WriteAllText(p: string, contents) =
                    System.IO.File.WriteAllText(p, contents)

                member _.ReadLines(p: string) = System.IO.File.ReadLines(p)
            }

        let Path =
            { new IPath with
                member _.Combine(p1: string, p2: string) : string =
                    System.IO.Path.Combine(p1, p2)

                member _.GetFullPath(p: string) : string =
                    System.IO.Path.GetFullPath(p)

                member _.GetDirectoryName(p: string) : string =
                    System.IO.Path.GetDirectoryName(p)

                member _.GetFileName(p: string) : string =
                    System.IO.Path.GetFileName(p)

                member _.GetTempPath() : string = System.IO.Path.GetTempPath()
            }

        let Directory =
            { new IDirectory with
                member _.GetFiles(p: string) : string[] =
                    System.IO.Directory.GetFiles(p)

                member _.GetDirectories(p: string) : string[] =
                    System.IO.Directory.GetDirectories(p)

                member _.GetCurrentDirectory() : string =
                    System.IO.Directory.GetCurrentDirectory()

                member _.Exists(p: string) : bool =
                    System.IO.Directory.Exists(p)

                member _.CreateDirectory(p: string) : unit =
                    System.IO.Directory.CreateDirectory(p) |> ignore

                member _.DeleteEmpty(p: string) = System.IO.Directory.Delete(p)
            }

    let Environment =
        { new IEnvironment with
            member _.IsWindows() =
                InteropServices.RuntimeInformation.IsOSPlatform(
                    InteropServices.OSPlatform.Windows
                )

            member _.GetEnvironmentVariable(varName) =
                System.Environment.GetEnvironmentVariable(varName)

            member _.SetEnvironmentVariable(varName, value) =
                System.Environment.SetEnvironmentVariable(varName, value)
        }

    let Json =
        { new IJson with
            member _.Parse(json: string) =
                JsonSerializer.Deserialize<JsonElement>(json)

            member _.TryGetProperty (key: string) (jsonEl: JsonElement) =
                match jsonEl.TryGetProperty(key) with
                | true, prop -> Some prop
                | false, _ -> None

            member _.GetString(jsonEl: JsonElement) = jsonEl.GetString()
        }

    open System.Diagnostics

    type private TypeInThisAssembly =
        class
        end

    let private startProcess workingDir exePath args =
        let args = String.concat " " args
        // let exePath, args =
        //     if Environment.IsWindows() then "cmd", ("/C " + exePath + " " + args)
        //     else exePath, args

        // Log.always(File.getRelativePathFromCwd(workingDir) + "> " + exePath + " " + args)

        let psi = ProcessStartInfo()
        // for envVar in envVars do
        //     psi.EnvironmentVariables.[envVar.Key] <- envVar.Value
        psi.FileName <- exePath
        psi.WorkingDirectory <- workingDir
        psi.Arguments <- args
        psi.CreateNoWindow <- false
        psi.UseShellExecute <- false

        Process.Start(psi)

    let private kill (p: Process) =
        p.Refresh()

        if not p.HasExited then
            p.Kill(entireProcessTree = true)

    let Process =
        { new IProcess with

            member _.RunAsync
                (
                    workingDir: string,
                    exePath: string,
                    args: string[]
                )
                =
                let p = startProcess workingDir exePath args
                // In Windows, terminating the main process doesn't kill the spawned ones so we need
                // to listen for the Console.CancelKeyPress and AssemblyLoadContext.Unloading events
                if Environment.IsWindows() then
                    Console.add_CancelKeyPress (fun _ _ -> kill p)

                    let assemblyLoadContext =
                        typeof<TypeInThisAssembly>.Assembly
                        |> Loader.AssemblyLoadContext.GetLoadContext

                    assemblyLoadContext.add_Unloading (fun _ -> kill p)

                Async.FromContinuations(fun (onSuccess, onError, _) ->
                    p.add_Exited (fun _ _ ->
                        match p.ExitCode with
                        | 0 -> onSuccess ()
                        | c ->
                            sprintf "Process exited with code %i" c
                            |> exn
                            |> onError
                    )
                )

            member _.Run(workingDir: string, exePath: string, args: string[]) =
                let p = startProcess workingDir exePath args
                p.WaitForExit()

                match p.ExitCode with
                | 0 -> ()
                | c -> failwith $"Process exited with code %i{c}"
        }

open Platform

type NugetInfo =
    {
        ApiKey: string
        ReleaseVersion: string
        ReleaseNotes: string[]
    }

let (</>) (p1: string) (p2: string) : string = IO.Path.Combine(p1, p2)

let isWindows = Environment.IsWindows()

let tempPath () : string = IO.Path.GetTempPath()

let fullPath (p: string) : string = IO.Path.GetFullPath(p)

let dirname (p: string) : string =
    let parent = IO.Path.GetDirectoryName(p)

    if parent = p then
        null
    else
        parent

let filename (p: string) : string = IO.Path.GetFileName(p)

let pathExists (p: string) : bool =
    IO.Directory.Exists(p) || IO.File.Exists(p)

let fileSizeInBytes (p: string) : float = IO.File.GetBytesLength(p)

let removeFile (p: string) : unit = IO.File.Delete(p)

let getFullPathsInDirectoryRecursively (p: string) =
    let rec inner p =
        [|
            for file in IO.Directory.GetFiles p do
                yield file
            for dir in IO.Directory.GetDirectories p do
                yield! inner dir
        |]

    inner p

let filenameWithoutExtension (p: string) =
    let name = filename p
    let i = name.LastIndexOf(".")

    if i > -1 then
        name.Substring(0, i)
    else
        name

let rec removeDirRecursive (p: string) : unit =
    if IO.Directory.Exists(p) then
        for file in IO.Directory.GetFiles p do
            IO.File.Delete(file)

        for dir in IO.Directory.GetDirectories p do
            removeDirRecursive dir

        IO.Directory.DeleteEmpty(p)

let makeDirRecursive (p: string) : unit = IO.Directory.CreateDirectory(p)

let rec copyDir (source: string) (target: string) (recursive: bool) : unit =
    if not (IO.Directory.Exists(target)) then
        makeDirRecursive target

    for file in IO.Directory.GetFiles(source) do
        let target = target </> filename file
        IO.File.Copy(file, target, true)

    if recursive then
        for sourceDir in IO.Directory.GetDirectories(source) do
            let target = target </> filename sourceDir
            copyDir sourceDir target recursive

let copyDirNonRecursive (source: string) (target: string) : unit =
    copyDir source target false

let copyDirRecursive (source: string) (target: string) : unit =
    copyDir source target true

let copyFile (source: string) (target: string) : unit =
    if IO.Directory.Exists source then
        failwith "Source is a directory, use copyDirRecursive"

    if not (IO.File.Exists(source)) then
        failwith "Source file does not exist"

    let target =
        if IO.Directory.Exists target then
            target </> filename source
        else
            IO.Directory.CreateDirectory(IO.Path.GetDirectoryName(target))
            target

    IO.File.Copy(source, target, true)

let writeFile (filePath: string) (txt: string) : unit =
    IO.File.WriteAllText(filePath, txt)

let readFile (filePath: string) : string = IO.File.ReadAllText(filePath)

let private __getExeArgs (cmd: string) =
    if isWindows then
        "cmd", [| "/C " + cmd |]
    else
        "sh", [| "-c \"" + cmd.Replace("\"", "\\\"") + "\"" |]

let private __runInDir silent cwd (cmd: string) : unit =
    if not silent then
        printfn $"{cwd}> {cmd}"

    let exe, args = __getExeArgs cmd
    Process.Run(cwd, exe, args)

let runAsync (cmd: string) =
    printfn "> %s" cmd
    let exe, args = __getExeArgs cmd
    Process.RunAsync(IO.Directory.GetCurrentDirectory(), exe, args)

let runInDir cwd (cmd: string) : unit = __runInDir false cwd cmd

let run cmd : unit =
    let cwd = IO.Directory.GetCurrentDirectory()
    __runInDir false cwd cmd

let runSilent cmd : unit =
    let cwd = IO.Directory.GetCurrentDirectory()
    __runInDir true cwd cmd

let runList cmdParts = String.concat " " cmdParts |> run

let runBashOrCmd cwd (scriptFileName: string) args =
    if isWindows then
        runInDir cwd (scriptFileName.Replace("/", "\\") + ".cmd " + args)
    else
        runInDir cwd ("sh " + scriptFileName + ".sh " + args)

let runAsyncWorkflow (workflow: Async<'T>) : unit =
    Async.RunSynchronously workflow |> ignore

let envVar (varName: string) : string =
    Environment.GetEnvironmentVariable(varName)

let envVarOrNone (varName: string) : string option =
    Environment.GetEnvironmentVariable(varName) |> Option.ofObj

let addToEnvPath (p: string) : unit =
    let SEPARATOR =
        if isWindows then
            ";"
        else
            ":"

    Environment.SetEnvironmentVariable("PATH", p + SEPARATOR + envVar "PATH")

let (|IgnoreCase|_|) (pattern: string) (input: string) =
    if String.Equals(input, pattern, StringComparison.OrdinalIgnoreCase) then
        Some IgnoreCase
    else
        None

let (|Regex|_|) (pattern: string) (input: string) =
    let m = Regex.Match(input, pattern)

    if m.Success then
        let mutable groups = []

        for i = m.Groups.Count - 1 downto 0 do
            groups <- m.Groups.[i].Value :: groups

        Some groups
    else
        None

let replaceRegex
    (pattern: string)
    (evaluator: Match -> string)
    (input: string)
    =
    Regex.Replace(input, pattern, evaluator)

module Publish =
    let NUGET_VERSION = @"(<Version>)(.*?)(<\/Version>)"
    let NUGET_PACKAGE_VERSION = @"(<PackageVersion>)(.*?)(<\/PackageVersion>)"

    let NUGET_PACKAGE_RELEASE_NOTES =
        @"(<PackageReleaseNotes>)([\s\S]*?)(</PackageReleaseNotes>)"

    let VERSION = @"(\d+)\.(\d+)\.(\d+)(\S*)"
    let VERSION_HEADER = "#+ " + VERSION

    let splitPrerelease (version: string) =
        let i = version.IndexOf('-')

        if i > 0 then
            version.Substring(0, i), Some(version.Substring(i + 1))
        else
            version, None

    let findFileUpwards fileName dir =
        let originalDir = dir

        let rec findFileUpwardsInner fileName dir =
            let fullPath = dir </> fileName

            if pathExists fullPath then
                fullPath
            else
                let parent = dirname dir

                if isNull parent then
                    failwithf
                        "Couldn't find %s upwards from %s"
                        fileName
                        originalDir

                findFileUpwardsInner fileName parent

        findFileUpwardsInner fileName dir

    let loadReleaseVersionAndNotes projFile =
        let projDir =
            if IO.Directory.Exists(projFile) then
                projFile
            else
                dirname projFile

        let releaseNotes = findFileUpwards "RELEASE_NOTES.md" projDir

        let mutable version = None
        let mutable stop = false
        let notes = ResizeArray()
        let enum = IO.File.ReadLines(releaseNotes).GetEnumerator()

        while not stop && enum.MoveNext() do
            match enum.Current.Trim() with
            | "" -> ()
            | Regex VERSION_HEADER [ _; major; minor; patch; rest ] ->
                match version with
                | None -> version <- Some $"{major}.{minor}.{patch}{rest}"
                // We reached next version section, stop reading
                | Some _ -> stop <- true
            | note -> notes.Add(note)

        match version with
        | Some version -> version, notes.ToArray()
        | None -> failwith $"Cannot read lates version from {releaseNotes}"

    let loadReleaseVersion projFile =
        let projDir =
            if IO.Directory.Exists(projFile) then
                projFile
            else
                dirname projFile

        let releaseNotes = findFileUpwards "RELEASE_NOTES.md" projDir

        match readFile releaseNotes with
        | Regex VERSION (version :: _) -> version
        | _ -> failwithf "Couldn't find version in %s" releaseNotes

    let loadNpmVersion projDir =
        projDir </> "package.json"
        |> readFile
        |> Json.Parse
        |> Json.TryGetProperty "version"
        |> Option.map (Json.GetString)
        |> Option.defaultWith (fun _ -> failwith "Cannot parse version")

    let bumpNpmVersion projDir newVersion =
        runInDir projDir ("npm version " + newVersion)

    // Returns (major, minor, patch, rest)
    let splitVersion =
        function
        | Regex VERSION [ _; major; minor; patch; rest ] ->
            (int major, int minor, int patch, rest)
        | s -> failwithf "Input doesn't match VERSION pattern: %s" s

    let needsPublishing
        (checkPkgVersion: string -> string option)
        (releaseVersion: string)
        projFile
        =
        let print msg =
            let projName =
                let projName = filename projFile

                if projName = "package.json" then
                    dirname projFile |> filename
                else
                    projName

            printfn "%s > %s" projName msg

        match readFile projFile |> checkPkgVersion with
        | None -> failwithf "Couldn't find package version in %s" projFile
        | Some version ->
            let sameVersion = version = releaseVersion

            if sameVersion then
                sprintf "Already version %s, no need to publish" releaseVersion
                |> print

            not sameVersion

    let private findFileWithExt (dir: string) (ext: string) =
        IO.Directory.GetFiles(dir)
        |> Array.tryPick (fun path ->
            if path.EndsWith(ext, StringComparison.Ordinal) then
                Some(dir </> path)
            else
                None
        )
        |> function
            | Some x -> x
            | None -> failwithf "Cannot find %s in %s" ext dir

    let pushNugetWithInfo
        (projFile: string)
        props
        buildAction
        (nugetInfo: NugetInfo)
        =
        let checkPkgVersion =
            function
            | Regex NUGET_PACKAGE_VERSION [ _; _; pkgVersion; _ ] ->
                Some pkgVersion
            | _ -> None

        let releaseVersion = nugetInfo.ReleaseVersion

        if needsPublishing checkPkgVersion releaseVersion projFile then
            buildAction ()
            let projDir = dirname projFile
            // Restore dependencies here so they're updated to latest project versions
            runList
                [
                    "dotnet restore"
                    projDir
                ]
            // Update the project file
            readFile projFile
            |> replaceRegex
                NUGET_VERSION
                (fun m ->
                    m.Groups.[1].Value
                    + (splitPrerelease releaseVersion |> fst)
                    + m.Groups.[3].Value
                )
            |> replaceRegex
                NUGET_PACKAGE_VERSION
                (fun m ->
                    m.Groups.[1].Value + releaseVersion + m.Groups.[3].Value
                )
            |> fun fsproj ->
                if nugetInfo.ReleaseNotes.Length = 0 then
                    fsproj
                else
                    fsproj
                    |> replaceRegex
                        NUGET_PACKAGE_RELEASE_NOTES
                        (fun m ->
                            m.Groups.[1].Value
                            + (String.concat "\n" nugetInfo.ReleaseNotes)
                            + m.Groups.[3].Value
                        )
            |> writeFile projFile

            try
                let tempDir = fullPath (projDir </> "temp")
                removeDirRecursive tempDir

                runList
                    [
                        "dotnet pack"
                        projDir
                        yield!
                            props
                            |> List.map (fun (k, v) -> "-p:" + k + "=" + v)
                        "-c Release -o"
                        tempDir
                    ]

                let nupkg = findFileWithExt tempDir ".nupkg"

                runList
                    [
                        "dotnet nuget push"
                        nupkg
                        "-s nuget.org -k"
                        nugetInfo.ApiKey
                    ]

                // Looks like the `nuget push` command automatically detects the .snupkg symbols
                // We issue the command below just in case but with --skip-duplicate to prevent errors
                // let snupkg = findFileWithExt tempDir ".snupkg"
                // runList ["dotnet nuget push"; snupkg; "-s nuget.org --skip-duplicate -k"; nugetInfo.ApiKey]

                removeDirRecursive tempDir
            with _ ->
                filenameWithoutExtension projFile
                |> printfn "There's been an error when pushing project: %s"

                printfn "Please revert the version change in .fsproj"
                reraise ()

    let pushNpmWithoutReleaseNotesCheck projDir (tag: string option) =
        // let _npmToken =
        //     match envVarOrNone "NPM_TOKEN" with
        //     | Some npmToken -> npmToken
        //     | None -> failwith "The npm token key must be set in a NPM_TOKEN environmental variable"
        // runInDir projDir @"npm config set '//registry.npmjs.org/:_authToken' ""${NPM_TOKEN}"""
        try
            let publishCmd =
                match tag with
                | Some tag -> $"npm publish --tag {tag}"
                | None -> "npm publish"

            runInDir projDir publishCmd
        with _ ->
            printfn "There's been an error when pushing project: %s" projDir
            printfn "Please revert the version change in package.json"
            reraise ()

    let pushNpm (projDir: string) buildAction =
        let checkPkgVersion json : string option =
            Json.Parse(json)
            |> Json.TryGetProperty "version"
            |> Option.map Json.GetString

        let releaseVersion = loadReleaseVersion projDir

        if
            needsPublishing
                checkPkgVersion
                releaseVersion
                (projDir </> "package.json")
        then
            buildAction ()
            bumpNpmVersion projDir releaseVersion

            let tag =
                match splitPrerelease releaseVersion with
                | _, Some _ -> Some "next"
                | _, None -> None

            pushNpmWithoutReleaseNotesCheck projDir tag

let doNothing () = ()

let pushNuget projFile props buildAction =
    let nugetKey =
        match envVarOrNone "NUGET_KEY" with
        | Some nugetKey -> nugetKey
        | None ->
            failwith
                "The Nuget API key must be set in a NUGET_KEY environmental variable"

    {
        ApiKey = nugetKey
        ReleaseVersion = Publish.loadReleaseVersion projFile
        ReleaseNotes = [||]
    }
    |> Publish.pushNugetWithInfo projFile props buildAction

let pushFableNuget projFile props buildAction =
    let fableNugetKey =
        match envVarOrNone "FABLE_NUGET_KEY" with
        | Some nugetKey -> nugetKey
        | None ->
            match envVarOrNone "NUGET_KEY" with
            | Some _nugetKey ->
                failwith
                    """
    The Nuget API key must be set in a FABLE_NUGET_KEY environmental variable

    We recently created a Fable org on NuGet, if you are a member of this organisation you need to generate a new
    nuget key specifing Fable as the owner of the key.

    We are now using FABLE_NUGET_KEY for the Fable's key so you can keep hosting your personnal key inside NUGET_KEY.

    More information can be found at: https://github.com/fable-compiler/Fable/issues/2455
    """
            | None ->
                failwith
                    "The Nuget API key must be set in a FABLE_NUGET_KEY environmental variable"

    let version, notes = Publish.loadReleaseVersionAndNotes projFile

    {
        ApiKey = fableNugetKey
        ReleaseVersion = version
        ReleaseNotes = notes
    }
    |> Publish.pushNugetWithInfo projFile props buildAction

let pushNpm projDir buildAction = Publish.pushNpm projDir buildAction

let pushNpmWithoutReleaseNotesCheck projDir =
    Publish.pushNpmWithoutReleaseNotesCheck projDir None

let getDotNetSDKVersionFromGlobalJson () : string =
    readFile "global.json"
    |> Json.Parse
    |> Json.TryGetProperty "sdk"
    |> Option.bind (Json.TryGetProperty "version")
    |> Option.map (Json.GetString)
    |> Option.defaultWith (fun _ -> failwith "Cannot parse version")

let getNpmVersion (projDir: string) =
    let pkgJsonPath =
        if projDir.EndsWith("package.json", StringComparison.Ordinal) then
            projDir
        else
            projDir </> "package.json"

    readFile pkgJsonPath
    |> Json.Parse
    |> Json.TryGetProperty "version"
    |> Option.map Json.GetString
    |> Option.defaultWith (fun _ -> failwith "Cannot parse version")

(*
let installDotnetSdk() =
    let sdkVersion = getDotNetSDKVersionFromGlobalJson()
    let mustInstall =
        match tryRunForOutput "dotnet --version" with
        | None -> true
        | Some v -> v <> sdkVersion
    if mustInstall then
        let archiveFileName =
            if isWindows then
                sprintf "dotnet-sdk-%s-win-x64.zip" sdkVersion
            elif isLinux then
                sprintf "dotnet-sdk-%s-linux-x64.tar.gz" sdkVersion
            else
                sprintf "dotnet-sdk-%s-osx-x64.tar.gz" sdkVersion
        let _downloadPath = sprintf "https://dotnetcli.blob.core.windows.net/dotnet/Sdk/%s/%s" sdkVersion archiveFileName
        failwith "TODO: download and unzip"
        // downloadSDK downloadPath archiveFileName
        // addToEnvPath dotnetExe

function DownloadAndUnzip(URL, outDir){
    var zlib = require('zlib');
    var https = require('https');
    var out = fs.createWriteStream(outDir);
    var request = https.get(URL, function(response) {
        response.pipe(zlib.createGunzip()).pipe(out)
    });
}
*)
