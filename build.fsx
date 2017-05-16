#r "packages/FAKE/tools/FakeLib.dll"
#r "System.IO.Compression.FileSystem"
#load "paket-files/fsharp/FAKE/modules/Octokit/Octokit.fsx"

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic
open Fake
open Fake.AssemblyInfoFile
open Fake.Git
open Fake.ReleaseNotesHelper
open Octokit

#if MONO
// prevent incorrect output encoding (e.g. https://github.com/fsharp/FAKE/issues/1196)
System.Console.OutputEncoding <- System.Text.Encoding.UTF8
#endif

module Util =
    open System.Net

    let (|RegexReplace|_|) =
        let cache = new Dictionary<string, Regex>()
        fun pattern (replacement: string) input ->
            let regex =
                match cache.TryGetValue(pattern) with
                | true, regex -> regex
                | false, _ ->
                    let regex = Regex pattern
                    cache.Add(pattern, regex)
                    regex
            let m = regex.Match(input)
            if m.Success
            then regex.Replace(input, replacement) |> Some
            else None

    let join pathParts =
        Path.Combine(Array.ofSeq pathParts)

    let run workingDir fileName args =
        printfn "CWD: %s" workingDir
        let fileName, args =
            if EnvironmentHelper.isUnix
            then fileName, args else "cmd", ("/C " + fileName + " " + args)
        let ok =
            execProcess (fun info ->
                info.FileName <- fileName
                info.WorkingDirectory <- workingDir
                info.Arguments <- args) TimeSpan.MaxValue
        if not ok then failwith (sprintf "'%s> %s %s' task failed" workingDir fileName args)

    let start workingDir fileName args =
        let p = new System.Diagnostics.Process()
        p.StartInfo.FileName <- fileName
        p.StartInfo.WorkingDirectory <- workingDir
        p.StartInfo.Arguments <- args
        p.Start() |> ignore
        p

    let runAndReturn workingDir fileName args =
        printfn "CWD: %s" workingDir
        let fileName, args =
            if EnvironmentHelper.isUnix
            then fileName, args else "cmd", ("/C " + args)
        ExecProcessAndReturnMessages (fun info ->
            info.FileName <- fileName
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
        |> fun p -> p.Messages |> String.concat "\n"

    let downloadArtifact path (url: string) =
        async {
            let tempFile = Path.ChangeExtension(Path.GetTempFileName(), ".zip")
            use client = new WebClient()
            do! client.AsyncDownloadFile(Uri url, tempFile)
            FileUtils.mkdir path
            CleanDir path
            run path "unzip" (sprintf "-q %s" tempFile)
            File.Delete tempFile
        } |> Async.RunSynchronously

    let rmdir dir =
        if EnvironmentHelper.isUnix
        then FileUtils.rm_rf dir
        // Use this in Windows to prevent conflicts with paths too long
        else run "." "cmd" ("/C rmdir /s /q " + Path.GetFullPath dir)

    let visitFile (visitor: string->string) (fileName : string) =
        File.ReadAllLines(fileName)
        |> Array.map (visitor)
        |> fun lines -> File.WriteAllLines(fileName, lines)

        // This code is supposed to prevent OutOfMemory exceptions but it outputs wrong BOM
        // use reader = new StreamReader(fileName, encoding)
        // let tempFileName = Path.GetTempFileName()
        // use writer = new StreamWriter(tempFileName, false, encoding)
        // while not reader.EndOfStream do
        //     reader.ReadLine() |> visitor |> writer.WriteLine
        // reader.Close()
        // writer.Close()
        // File.Delete(fileName)
        // File.Move(tempFileName, fileName)

    let replaceLines (replacer: string->Match->string option) (reg: Regex) (fileName: string) =
        fileName |> visitFile (fun line ->
            let m = reg.Match(line)
            if not m.Success
            then line
            else
                match replacer line m with
                | None -> line
                | Some newLine -> newLine)

    let compileScript symbols outDir fsxPath =
        let dllFile = Path.ChangeExtension(Path.GetFileName fsxPath, ".dll")
        let opts = [
            yield FscHelper.Out (Path.Combine(outDir, dllFile))
            yield FscHelper.Target FscHelper.TargetType.Library
            yield! symbols |> List.map FscHelper.Define
        ]
        FscHelper.compile opts [fsxPath]
        |> function 0 -> () | _ -> failwithf "Cannot compile %s" fsxPath

    let normalizeVersion (version: string) =
        let i = version.IndexOf("-")
        if i > 0 then version.Substring(0, i) else version

    let assemblyInfo projectDir version extra =
        let version = normalizeVersion version
        let asmInfoPath = projectDir </> "AssemblyInfo.fs"
        (Attribute.Version version)::extra
        |> CreateFSharpAssemblyInfo asmInfoPath

    let loadReleaseNotes pkg =
        Lazy<_>(fun () ->
            match pkg with
            | "" -> "RELEASE_NOTES.md"
            | pkg -> sprintf "RELEASE_NOTES_%s.md" pkg
            |> ReleaseNotesHelper.LoadReleaseNotes)

    type ComparisonResult = Smaller | Same | Bigger

    let foldi f init (xs: 'T seq) =
        let mutable i = -1
        (init, xs) ||> Seq.fold (fun state x ->
            i <- i + 1
            f i state x)

    let compareVersions (expected: string) (actual: string) =
        if actual = "*" // Wildcard for custom fable-core builds
        then Same
        else
            let expected = expected.Split('.', '-')
            let actual = actual.Split('.', '-')
            (Same, expected) ||> foldi (fun i comp expectedPart ->
                match comp with
                | Bigger -> Bigger
                | Same when actual.Length <= i -> Smaller
                | Same ->
                    let actualPart = actual.[i]
                    match Int32.TryParse(expectedPart), Int32.TryParse(actualPart) with
                    // TODO: Don't allow bigger for major version?
                    | (true, expectedPart), (true, actualPart) ->
                        if actualPart > expectedPart
                        then Bigger
                        elif actualPart = expectedPart
                        then Same
                        else Smaller
                    | _ ->
                        if actualPart = expectedPart
                        then Same
                        else Smaller
                | Smaller -> Smaller)

module Npm =
    let script workingDir script args =
        sprintf "run %s -- %s" script (String.concat " " args)
        |> Util.run workingDir "npm"

    let install workingDir modules =
        sprintf "install %s" (String.concat " " modules)
        |> Util.run workingDir "npm"

    let command workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.run workingDir "npm"

    let commandAndReturn workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.runAndReturn workingDir "npm"

    let getLatestVersion package tag =
        let package =
            match tag with
            | Some tag -> package + "@" + tag
            | None -> package
        commandAndReturn "." "show" [package; "version"]

    let updatePackageKeyValue f pkgDir keys =
        let pkgJson = Path.Combine(pkgDir, "package.json")
        let reg =
            String.concat "|" keys
            |> sprintf "\"(%s)\"\\s*:\\s*\"(.*?)\""
            |> Regex
        let lines =
            File.ReadAllLines pkgJson
            |> Array.map (fun line ->
                let m = reg.Match(line)
                if m.Success then
                    match f(m.Groups.[1].Value, m.Groups.[2].Value) with
                    | Some(k,v) -> reg.Replace(line, sprintf "\"%s\": \"%s\"" k v)
                    | None -> line
                else line)
        File.WriteAllLines(pkgJson, lines)

module Node =
    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "node" args

module Fake =
    let fakePath = "packages" </> "docs" </> "FAKE" </> "tools" </> "FAKE.exe"
    let fakeStartInfo script workingDirectory args fsiargs environmentVars =
        (fun (info: System.Diagnostics.ProcessStartInfo) ->
            info.FileName <- System.IO.Path.GetFullPath fakePath
            info.Arguments <- sprintf "%s --fsiargs -d:FAKE %s \"%s\"" args fsiargs script
            info.WorkingDirectory <- workingDirectory
            let setVar k v = info.EnvironmentVariables.[k] <- v
            for (k, v) in environmentVars do setVar k v
            setVar "MSBuild" msBuildExe
            setVar "GIT" Git.CommandHelper.gitPath
            setVar "FSI" fsiPath)

    /// Run the given buildscript with FAKE.exe
    let executeFAKEWithOutput workingDirectory script fsiargs envArgs =
        let exitCode =
            ExecProcessWithLambdas
                (fakeStartInfo script workingDirectory "" fsiargs envArgs)
                TimeSpan.MaxValue false ignore ignore
        System.Threading.Thread.Sleep 1000
        exitCode

// Project info
let project = "Fable"
let authors = ["Alfonso García-Caro"]

let gitOwner = "fable-compiler"
let gitHome = "https://github.com/" + gitOwner

let release = Util.loadReleaseNotes ""
let releaseToolsJs = Util.loadReleaseNotes "TOOLS_JS"
let releaseLoader = Util.loadReleaseNotes "LOADER"
let releaseRollup = Util.loadReleaseNotes "ROLLUP"
let releaseJsonConverter = Util.loadReleaseNotes "JSON_CONVERTER"

let dotnetcliVersion = "1.0.4"
let mutable dotnetExePath = environVarOrDefault "DOTNET" "dotnet"

let toolsBuildDir = "build/fable"
let coreBuildDir = "build/fable-core"
let testsBuildDir = "build/tests"
let coreSrcDir = "src/dotnet/Fable.Core"
let compilerSrcDir = "src/dotnet/Fable.Compiler"
let toolsSrcDir = "src/dotnet/Fable.Tools"
let coreJsSrcDir = "src/typescript/fable-core"

// Targets
let installDotnetSdk () =
    let dotnetSDKPath = FullName "./dotnetsdk"

    let correctVersionInstalled =
        try
            let processResult =
                ExecProcessAndReturnMessages (fun info ->
                info.FileName <- dotnetExePath
                info.WorkingDirectory <- Environment.CurrentDirectory
                info.Arguments <- "--version") (TimeSpan.FromMinutes 30.)

            let installedVersion = processResult.Messages |> separated ""
            match Util.compareVersions dotnetcliVersion installedVersion with
            | Util.Same | Util.Bigger -> true
            | Util.Smaller -> false
        with
        | _ -> false

    if correctVersionInstalled then
        tracefn "dotnetcli %s already installed" dotnetcliVersion
    else
        CleanDir dotnetSDKPath
        let archiveFileName =
            if isWindows then
                sprintf "dotnet-dev-win-x64.%s.zip" dotnetcliVersion
            elif isLinux then
                sprintf "dotnet-dev-ubuntu-x64.%s.tar.gz" dotnetcliVersion
            else
                sprintf "dotnet-dev-osx-x64.%s.tar.gz" dotnetcliVersion
        let downloadPath =
                sprintf "https://dotnetcli.azureedge.net/dotnet/Sdk/%s/%s" dotnetcliVersion archiveFileName
        let localPath = Path.Combine(dotnetSDKPath, archiveFileName)

        tracefn "Installing '%s' to '%s'" downloadPath localPath

        use webclient = new Net.WebClient()
        webclient.DownloadFile(downloadPath, localPath)

        if not isWindows then
            let assertExitCodeZero x =
                if x = 0 then () else
                failwithf "Command failed with exit code %i" x

            Shell.Exec("tar", sprintf """-xvf "%s" -C "%s" """ localPath dotnetSDKPath)
            |> assertExitCodeZero
        else
            System.IO.Compression.ZipFile.ExtractToDirectory(localPath, dotnetSDKPath)

        tracefn "dotnet cli path - %s" dotnetSDKPath
        System.IO.Directory.EnumerateFiles dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s" path)
        System.IO.Directory.EnumerateDirectories dotnetSDKPath
        |> Seq.iter (fun path -> tracefn " - %s%c" path System.IO.Path.DirectorySeparatorChar)

        dotnetExePath <- dotnetSDKPath </> (if isWindows then "dotnet.exe" else "dotnet")

    // let oldPath = System.Environment.GetEnvironmentVariable("PATH")
    // System.Environment.SetEnvironmentVariable("PATH", sprintf "%s%s%s" dotnetSDKPath (System.IO.Path.PathSeparator.ToString()) oldPath)

let clean () =
    !! "src/dotnet/**/bin" ++ "src/dotnet/**/obj/"
        -- "src/dotnet/Fable.Client.Browser/demo/**"
        -- "src/dotnet/Fable.Client.Browser/testapp/**"
        ++ "build/fable-core" ++ "build/json-converter"
        ++ "build/nunit" ++ "build/tests_dll"
    |> CleanDirs

    // Don't delete node_modules for faster builds
    !! "build/fable/**/*.*" -- "build/fable/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
    !! "build/tests/**/*.*" -- "build/tests/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm

let nugetRestore baseDir () =
    Util.run (baseDir </> "Fable.Core") dotnetExePath "restore"
    Util.run (baseDir </> "Fable.Compiler") dotnetExePath "restore"
    Util.run (baseDir </> "Fable.Tools") dotnetExePath "restore"

let buildTools baseDir isRelease () =
    sprintf "publish -o ../../../%s -c %s -v n"
        toolsBuildDir (if isRelease then "Release" else "Debug")
    |> Util.run (baseDir </> "Fable.Tools") dotnetExePath

    // Put FSharp.Core.optdata/sigdata next to FSharp.Core.dll
    FileUtils.cp "packages/FSharp.Core/lib/netstandard1.6/FSharp.Core.optdata" toolsBuildDir
    FileUtils.cp "packages/FSharp.Core/lib/netstandard1.6/FSharp.Core.sigdata" toolsBuildDir

let buildCoreJs () =
    Npm.install __SOURCE_DIRECTORY__ []
    Npm.script __SOURCE_DIRECTORY__ "tslint" [sprintf "--project %s" coreJsSrcDir]
    Npm.script __SOURCE_DIRECTORY__ "tsc" [sprintf "--project %s" coreJsSrcDir]

let buildCore isRelease () =
    let config = if isRelease then "Release" else "Debug"
    sprintf "build -c %s" config
    |> Util.run coreSrcDir dotnetExePath

    CreateDir coreBuildDir
    FileUtils.cp (sprintf "%s/bin/%s/netstandard1.6/Fable.Core.dll" coreSrcDir config) coreBuildDir
    // TODO: Doc generation doesn't work with netcorecli-fsc atm
    // FileUtils.cp (sprintf "%s/bin/%s/netstandard1.6/Fable.Core.xml" coreSrcDir config) coreBuildDir

let buildNUnitPlugin () =
    let nunitDir = "src/plugins/nunit"
    CreateDir "build/nunit"  // if it does not exist
    Util.run nunitDir dotnetExePath "restore"
    // pass output path to build command
    Util.run nunitDir dotnetExePath ("build -c Release -o ../../../build/nunit")

let buildJsonConverter () =
    "restore src/dotnet/Fable.JsonConverter"
    |> Util.run __SOURCE_DIRECTORY__ dotnetExePath

    "build src/dotnet/Fable.JsonConverter -c Release -o ../../../build/json-converter"
    |> Util.run __SOURCE_DIRECTORY__ dotnetExePath

let runTestsDotnet () =
    Util.run "src/tests_external" dotnetExePath "restore"
    Util.run "src/tests/DllRef" dotnetExePath "restore"
    Util.run "src/tests/Project With Spaces" dotnetExePath "restore"
    Util.run "src/tests/Main" dotnetExePath "restore"
    Util.run "src/tests/Main" dotnetExePath "test"

let runFableServer f =
    let fableServer = Util.start __SOURCE_DIRECTORY__ dotnetExePath "build/fable/dotnet-fable.dll start"
    try f()
    finally fableServer.Kill()

let runTestsJs () =
    Npm.install __SOURCE_DIRECTORY__ []
    runFableServer <| fun () ->
        Npm.script __SOURCE_DIRECTORY__ "webpack" ["--config src/tests/webpack.config.js"]
    Npm.script __SOURCE_DIRECTORY__ "mocha" ["./build/tests/bundle.js"]

let quickTest() =
    Util.run "src/tools" dotnetExePath "../../build/fable/dotnet-fable.dll npm-run rollup"
    Node.run "." "src/tools/temp/QuickTest.js" []

Target "QuickTest" quickTest
Target "QuickFableCompilerTest" (fun () ->
    buildTools "src/dotnet" true ()
    quickTest ())
Target "QuickFableCoreTest" (fun () ->
    buildCoreJs ()
    quickTest ())

let pushNuget (releaseNotes: ReleaseNotes) (projFiles: string list) =
    let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)
    let pushProject (releaseNotes: ReleaseNotes) projFile =
        let projDir = Path.GetDirectoryName(projFile)
        let nugetKey =
            match environVarOrNone "NUGET_KEY" with
            | Some nugetKey -> nugetKey
            | None -> failwith "The Nuget API key must be set in a NUGET_KEY environmental variable"
        // Restore dependencies here so they're updated to latest project versions
        Util.run projDir dotnetExePath "restore"
        // If this is Fable.Core, build JS files
        if projFile.Contains("Fable.Core.fsproj") then
            buildCoreJs()
        // Update version in dotnet-fable Constants.fs file
        if projFile.Contains("dotnet-fable.fsproj") then
            let reg = Regex(@"VERSION\s*=\s*""(.*?)""")
            let mainFile = Path.Combine(Path.GetDirectoryName(projFile), "Constants.fs")
            (reg, mainFile) ||> Util.replaceLines (fun line m ->
                let replacement = sprintf "VERSION = \"%s\"" releaseNotes.NugetVersion
                reg.Replace(line, replacement) |> Some)
        Util.run projDir dotnetExePath (sprintf "pack -c Release /p:Version=%s" releaseNotes.NugetVersion)
        Directory.GetFiles(projDir </> "bin" </> "Release", "*.nupkg")
        |> Array.find (fun nupkg -> nupkg.Contains(releaseNotes.NugetVersion))
        |> (fun nupkg ->
            (Path.GetFullPath nupkg, nugetKey)
            ||> sprintf "nuget push %s -s nuget.org -k %s"
            |> Util.run projDir dotnetExePath)
        // After successful publishing, update the project file
        (versionRegex, projFile) ||> Util.replaceLines (fun line _ ->
            versionRegex.Replace(line, "<Version>"+releaseNotes.NugetVersion+"</Version>") |> Some)
    let checkProject (releaseNotes: ReleaseNotes) projFile =
        let projFile = __SOURCE_DIRECTORY__ </> projFile
        File.ReadLines(projFile)
        |> Seq.tryPick (fun line ->
            let m = versionRegex.Match(line)
            if m.Success then Some m else None)
        |> function
            | None -> failwithf "Couldn't find version in %s" projFile
            | Some m ->
                // Publish package if version has been updated
                if m.Groups.[1].Value <> releaseNotes.NugetVersion then
                    pushProject releaseNotes projFile
    for projFile in projFiles do
        checkProject releaseNotes projFile

let pushNpm (releaseNotes: ReleaseNotes) (projDir: string) =
    let projDir = __SOURCE_DIRECTORY__ </> (projDir.TrimEnd('/'))
    let updated = ref false
    let version = releaseNotes.NugetVersion
    let reg = Regex(@"""version"":\s*""(.*?)""")
    (reg, projDir </> "package.json") ||> Util.replaceLines (fun line m ->
        if m.Groups.[1].Value = version
        then None
        else
            updated := true
            reg.Replace(line, sprintf @"""version"": ""%s""" version) |> Some)
    // Publish package if version has been updated
    if !updated then
        if version.IndexOf("-") > 0 then ["--tag next"] else []
        |> Npm.command projDir "publish"

// Target "BrowseDocs" (fun _ ->
//     let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "BrowseDocs"]
//     if exit <> 0 then failwith "Browsing documentation failed"
// )

Target "GenerateDocs" (fun _ ->
    let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "GenerateDocs"]
    if exit <> 0 then failwith "Generating documentation failed"
)

Target "PublishDocs" (fun _ ->
    let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "PublishDocs"]
    if exit <> 0 then failwith "Publishing documentation failed"
)

Target "PublishStaticPages" (fun _ ->
    let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "PublishStaticPages"]
    if exit <> 0 then failwith "Publishing documentation failed"
)

Target "GitHubRelease" (fun _ ->
    let release = release.Value
    let user =
        match getBuildParam "github-user" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserInput "GitHub Username: "
    let pw =
        match getBuildParam "github-pw" with
        | s when not (String.IsNullOrWhiteSpace s) -> s
        | _ -> getUserPassword "GitHub Password: "
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + project))
        |> function None -> gitHome + "/" + project | Some (s: string) -> s.Split().[0]

    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.pushBranch "" remote (Information.getBranchName "")

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" remote release.NugetVersion

    // release on github
    createClient user pw
    |> createDraft gitOwner project release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // |> uploadFile (buildDir</>("FSharp.Compiler.Service." + release.NugetVersion + ".nupkg"))
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "Clean" clean
Target "NugetRestore" (nugetRestore "src/dotnet")
Target "FableTools" (fun _ ->
    nugetRestore "src/dotnet" ()
    buildTools "src/dotnet" true ())
Target "FableCoreJs" buildCoreJs
Target "RunTestsJs" runTestsJs

Target "PublishPackages" (fun () ->
    installDotnetSdk ()
    clean ()

    // Publish Nuget packages
    // TODO: Unify publishing of the three main Nuget packages
    pushNuget release.Value [
        "src/dotnet/Fable.Core/Fable.Core.fsproj"
        "src/dotnet/Fable.Compiler/Fable.Compiler.fsproj"
        "src/dotnet/Fable.Tools/dotnet-fable.fsproj"
    ]
    pushNuget releaseJsonConverter.Value
        ["src/dotnet/Fable.JsonConverter/Fable.JsonConverter.fsproj"]

    // Publish NPM packages
    pushNpm releaseToolsJs.Value "src/typescript/fable-utils"
    pushNpm releaseLoader.Value "src/typescript/fable-loader"
    pushNpm releaseRollup.Value "src/typescript/rollup-plugin-fable"
    // TODO: Add NUnit plugin, it must be built first
)

Target "All" (fun () ->
    installDotnetSdk ()
    clean ()
    nugetRestore "src/dotnet" ()
    buildTools "src/dotnet" true ()
    buildCoreJs ()
    buildNUnitPlugin ()
    buildJsonConverter ()
    runTestsJs ()
    runTestsDotnet ()
)

// For this target to work, you need the following:
// - Clone github.com/ncave/FSharp.Compiler.Service/ `fable` branch and put it
//   in a folder next to Fable repo named `FSharp.Compiler.Service_fable`
// - In `FSharp.Compiler.Service_fable` run `build CodeGen.NetCore -d:FABLE_COMPILER`
// > Attention: the generation of libraries metadata is not included in this target
Target "REPL" (fun () ->
    let replDir = "src/dotnet/Fable.Client.Browser/demo"
    Npm.install replDir []

    // Compile fable-core
    CreateDir (replDir + "/fable-core")
    Npm.script __SOURCE_DIRECTORY__ "tsc" [sprintf "--project src/typescript/fable-core -m amd --outDir %s/fable-core" replDir]

    // Compile FCS/Fable with Fable
    Util.run replDir dotnetExePath "../../../../build/fable/dotnet-fable.dll npm-run build"
)

// Start build
RunTargetOrDefault "All"
