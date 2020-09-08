module PublishUtils

open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Fable.Import

module private Helpers =
    let readline: obj = importAll "readline"
    let path: obj = importAll "path"
    let fs: obj = importAll "fs"
    let childProcess: obj = importAll "child_process"
    let [<Global("process")>] nodeProcess = obj()

    let inline (!>) x = ignore x
    let inline (~%) xs = createObj xs |> unbox

    type SingleObservable<'T>(?onDispose: unit->unit) =
        let mutable disposed = false
        let mutable listener: IObserver<'T> option = None
        member __.IsDisposed = disposed
        member __.Dispose() =
            if not disposed then
                onDispose |> Option.iter (fun d -> d())
                listener |> Option.iter (fun l -> l.OnCompleted())
                disposed <- true
                listener <- None
        member __.Trigger v =
            listener |> Option.iter (fun l -> l.OnNext v)
        interface IObservable<'T> with
            member this.Subscribe w =
                if disposed then failwith "Disposed"
                if Option.isSome listener then failwith "Busy"
                listener <- Some w
                { new IDisposable with
                    member __.Dispose() = this.Dispose() }

    let awaitWhileTrue (f: 'T->bool) (s: IObservable<'T>) =
        Async.FromContinuations <| fun (success,_,_) ->
            let mutable finished = false
            let mutable disp = Unchecked.defaultof<IDisposable>
            let observer =
                { new IObserver<'T> with
                    member __.OnNext v =
                        if not finished then
                            if not(f v) then
                                finished <- true
                                disp.Dispose()
                                success()
                    member __.OnError e = ()
                    member x.OnCompleted() =
                        success() }
            disp <- s.Subscribe(observer)

open Helpers

let (</>) (p1: string) (p2: string): string =
    path?join.Invoke(p1, p2)

let args: string list =
    nodeProcess?argv
    |> Seq.skip 2
    |> Seq.toList

let argsLower<'T> =
    args |> List.map (fun x -> x.ToLower())

let isWindows =
    nodeProcess?platform = "win32"

let isLinux =
    nodeProcess?platform = "linux"

let fullPath (p: string): string =
  path?resolve(p)

let dirname (p: string): string =
  let parent = path?dirname(p)
  if parent = p then null else parent

let dirFiles (p: string): string[] =
    fs?readdirSync(p)

let isDirectory (p: string): bool =
    fs?lstatSync(p)?isDirectory()

let getFullPathsInDirectoryRecursively (p: string) =
    let rec inner p =
        [| for file in dirFiles p do
            let file = p </> file
            if isDirectory file then
                yield! inner file
            else yield file |]
    inner p

let fileSizeInBytes (p: string): int =
    fs?lstatSync(p)?size

let pathExists (p: string): bool =
    fs?existsSync(p)

let filename (p: string): string =
  path?basename(p)

let filenameWithoutExtension (p: string) =
    let name = filename p
    let i = name.LastIndexOf(".")
    if i > -1 then name.Substring(0, i) else name

let removeFile (p: string): unit =
    fs?unlinkSync(p)

let moveFile (oldPath: string) (newPath: string): unit =
    fs?renameSync(oldPath, newPath)

let rec removeDirRecursive (p: string): unit =
    if fs?existsSync(p) then
        for file in dirFiles p do
            let curPath = p </> file
            if isDirectory curPath then
                removeDirRecursive curPath
            else
                fs?unlinkSync(curPath)
        fs?rmdirSync(p)

let makeDirRecursive (p: string): unit =
    fs?mkdirSync(p, %["recursive" ==> true])

[<Obsolete("makeDirRecursive")>]
let mkDirRecursive (p: string): unit =
    makeDirRecursive p

let rec copyDir (source: string) (target: string) (recursive: bool): unit =
    if fs?existsSync(target) |> not then
        makeDirRecursive target
    for file in dirFiles source do
        let source = source </> file
        let target = target </> file
        if isDirectory source then
            if recursive then
                copyDir source target recursive
        else
            fs?copyFileSync(source, target)

let copyDirNonRecursive (source: string) (target: string): unit =
    copyDir source target false

let copyDirRecursive (source: string) (target: string): unit =
    copyDir source target true

let copyFile (source: string) (target: string): unit =
    if isDirectory source then
        failwith "Source is a directory, use copyDirRecursive"
    let target =
        if pathExists target && isDirectory target then
            target </> filename source
        else target
    fs?copyFileSync(source, target)

let writeFile (filePath: string) (txt: string): unit =
    fs?writeFileSync(filePath, txt)

let readFile (filePath: string): string =
    fs?readFileSync(filePath)?toString()

let readAllLines (filePath: string): string[] =
    (readFile filePath).Split('\n')

let readLines (filePath: string): IObservable<string> =
    let rl = readline?createInterface %[
        "input" ==> fs?createReadStream(filePath)
        // Note: we use the crlfDelay option to recognize all instances of CR LF
        // ('\r\n') in input.txt as a single line break.
        "crlfDelay" ==> System.Double.PositiveInfinity
    ]
    let obs = SingleObservable(fun () -> rl?close())
    rl?on.Invoke("line", fun line ->
        obs.Trigger(line))
    rl?on.Invoke("close", fun _line ->
        obs.Dispose())
    obs :> _

let takeLines (numLines: int) (filePath: string) = async {
    let mutable i = -1
    let lines = ResizeArray()
    do! readLines filePath
        |> awaitWhileTrue (fun line ->
            i <- i + 1
            if i < numLines then lines.Add(line); true
            else false)
    return lines.ToArray()
}

let takeLinesWhile (predicate: string->bool) (filePath: string) = async {
    let lines = ResizeArray()
    do! readLines filePath
        |> awaitWhileTrue (fun line ->
            if predicate line then lines.Add(line); true
            else false)
    return lines.ToArray()
}
let run cmd: unit =
    printfn "> %s" cmd
    childProcess?execSync.Invoke(cmd, %[
        "stdio" ==> "inherit"
    ])

let runInDir cwd cmd: unit =
    printfn "> %s" cmd
    childProcess?execSync.Invoke(cmd, %[
        "stdio" ==> "inherit"
        "cwd" ==> cwd
    ])

let tryRunForOutput (cmd: string): string option =
    try
        childProcess?execSync.Invoke(cmd).ToString().Trim() |> Some
    with _ -> None

let runList cmdParts =
    String.concat " " cmdParts |> run

let runBashOrCmd cwd (scriptFileName: string) args =
    if isWindows
    then runInDir cwd (scriptFileName.Replace("/", "\\") + ".cmd " + args)
    else runInDir cwd ("sh " + scriptFileName + ".sh " + args)

let envVar (varName: string): string =
    nodeProcess?env?(varName)

let envVarOrNone (varName: string): string option =
    nodeProcess?env?(varName)
    |> Option.ofObj

let addToEnvPath (p: string): unit =
    let SEPARATOR = if isWindows then ";" else ":"
    nodeProcess?env?PATH <- p + SEPARATOR + nodeProcess?env?PATH

let (|IgnoreCase|_|) (pattern: string) (input: string) =
    if String.Equals(input, pattern, StringComparison.OrdinalIgnoreCase) then
        Some IgnoreCase
    else None

let (|Regex|_|) (pattern: string) (input: string) =
    let m = Regex.Match(input, pattern)
    if m.Success then
        let mutable groups = []
        for i = m.Groups.Count - 1 downto 0 do
            groups <- m.Groups.[i].Value::groups
        Some groups
    else None

let replaceRegex (pattern: string) (replacement: string list) (input: string) =
    Regex.Replace(input, pattern, String.concat "" replacement)

module Publish =
    let NUGET_VERSION = @"(<Version>)(.*?)(<\/Version>)"
    let NUGET_PACKAGE_VERSION = @"(<PackageVersion>)(.*?)(<\/PackageVersion>)"
    let VERSION = @"(\d+)\.(\d+)\.(\d+)(\S*)"

    let splitPrerelease (version: string) =
        let i = version.IndexOf("-")
        if i > 0
        then version.Substring(0, i), Some(version.Substring(i + 1))
        else version, None

    let findFileUpwards fileName dir =
        let originalDir = dir
        let rec findFileUpwardsInner fileName dir =
            let fullPath = dir </> fileName
            if pathExists fullPath
            then fullPath
            else
                let parent = dirname dir
                if isNull parent then
                    failwithf "Couldn't find %s upwards from %s" fileName originalDir
                findFileUpwardsInner fileName parent
        findFileUpwardsInner fileName dir

    let loadReleaseVersionAndNotes projFile =
        Async.FromContinuations(fun (cont,_,_) ->
            let projDir = if isDirectory projFile then projFile else dirname projFile
            let releaseNotes = findFileUpwards "RELEASE_NOTES.md" projDir
            let mutable version = ""
            let notes = ResizeArray()
            let mutable disp = Unchecked.defaultof<IDisposable>
            disp <- readLines releaseNotes |> Observable.subscribe (fun line ->
                match line.Trim() with
                | "" -> ()
                | Regex VERSION (v::_) ->
                    match version with
                    | "" -> version <- v
                    | _ ->
                        // We reached next version section, stop reading
                        disp.Dispose()
                        cont(version, notes.ToArray())
                | note -> notes.Add(note)))

    let loadReleaseVersion projFile =
        let projDir = if isDirectory projFile then projFile else dirname projFile
        let releaseNotes = findFileUpwards "RELEASE_NOTES.md" projDir
        match readFile releaseNotes with
        | Regex VERSION (version::_) -> version
        | _ -> failwithf "Couldn't find version in %s" releaseNotes

    let loadNpmVersion projDir =
        let json =
            projDir </> "package.json"
            |> readFile
            |> JS.JSON.parse
        json?version

    let bumpNpmVersion projDir newVersion =
        runInDir projDir ("npm version " + newVersion)

    // Returns (major, minor, patch, rest)
    let splitVersion = function
        | Regex VERSION [_;major;minor;patch;rest] -> (int major, int minor, int patch, rest)
        | s -> failwithf "Input doesn't match VERSION pattern: %s" s

    let needsPublishing (checkPkgVersion: string->string option) (releaseVersion: string) projFile =
        let print msg =
            let projName =
                let projName = filename projFile
                if projName = "package.json"
                then dirname projFile |> filename
                else projName
            printfn "%s > %s" projName msg
        match readFile projFile |> checkPkgVersion with
        | None -> failwithf "Couldn't find package version in %s" projFile
        | Some version ->
            let sameVersion = version = releaseVersion
            if sameVersion then
                sprintf "Already version %s, no need to publish" releaseVersion |> print
            not sameVersion

    let pushNuget (projFile: string) buildAction =
        let checkPkgVersion = function
            | Regex NUGET_PACKAGE_VERSION [_;_;pkgVersion;_] -> Some pkgVersion
            | _ -> None
        let releaseVersion = loadReleaseVersion projFile
        if needsPublishing checkPkgVersion releaseVersion projFile then
            buildAction()
            let projDir = dirname projFile
            let nugetKey =
                match envVarOrNone "NUGET_KEY" with
                | Some nugetKey -> nugetKey
                | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"
            // Restore dependencies here so they're updated to latest project versions
            runList ["dotnet restore"; projDir]
            // Update the project file
            readFile projFile
            |> replaceRegex NUGET_VERSION ["$1"; splitPrerelease releaseVersion |> fst; "$3"]
            |> replaceRegex NUGET_PACKAGE_VERSION ["$1"; releaseVersion; "$3"]
            |> writeFile projFile
            try
                let tempDir = fullPath(projDir </> "temp")
                removeDirRecursive tempDir
                runList ["dotnet pack"; projDir; sprintf "-c Release -o %s" tempDir]
                let pkgName = filenameWithoutExtension projFile
                let nupkg =
                    dirFiles tempDir
                    |> Seq.tryPick (fun path ->
                        if path.Contains(pkgName) then Some(tempDir </> path) else None)
                    |> function
                        | Some x -> x
                        | None -> failwithf "Cannot find .nupgk with name %s" pkgName
                runList ["dotnet nuget push"; nupkg; "-s nuget.org -k"; nugetKey]
                removeDirRecursive tempDir
            with _ ->
                filenameWithoutExtension projFile
                |> printfn "There's been an error when pushing project: %s"
                printfn "Please revert the version change in .fsproj"
                reraise()

    let pushNpm (projDir: string) buildAction =
        let checkPkgVersion json: string option =
            (JS.JSON.parse json)?version |> Option.ofObj
        let releaseVersion = loadReleaseVersion projDir
        if needsPublishing checkPkgVersion releaseVersion (projDir </> "package.json") then
            let nugetKey =
                match envVarOrNone "NPM_TOKEN" with
                | Some nugetKey -> nugetKey
                | None -> failwith "The npm token key must be set in a NPM_TOKEN environmental variable"
            runInDir projDir @"npm config set '//registry.npmjs.org/:_authToken' ""${NPM_TOKEN}"""
            buildAction()
            bumpNpmVersion projDir releaseVersion
            try
                let publishCmd =
                    match splitPrerelease releaseVersion with
                    | _, Some _ -> "npm publish --tag next"
                    | _, None -> "npm publish"
                runInDir projDir publishCmd
            with _ ->
                printfn "There's been an error when pushing project: %s" projDir
                printfn "Please revert the version change in package.json"
                reraise()

let doNothing () = ()

let pushNuget projFile buildAction =
    Publish.pushNuget projFile buildAction

let pushNpm projDir buildAction =
    Publish.pushNpm projDir buildAction

let getDotNetSDKVersionFromGlobalJson(): string =
    let json = readFile "global.json" |> JS.JSON.parse
    json?sdk?version

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
(*
function DownloadAndUnzip(URL, outDir){
    var zlib = require('zlib');
    var https = require('https');
    var out = fs.createWriteStream(outDir);
    var request = https.get(URL, function(response) {
        response.pipe(zlib.createGunzip()).pipe(out)
    });
}
*)
