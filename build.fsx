#r "packages/build/FAKE/tools/FakeLib.dll"
#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
#load "paket-files/build/fable-compiler/fake-helpers/Fable.FakeHelpers.fs"

open System
open System.IO
open System.Text.RegularExpressions
open Fake
open Fake.Git
open Octokit
open Fable.FakeHelpers

#if MONO
// prevent incorrect output encoding (e.g. https://github.com/fsharp/FAKE/issues/1196)
System.Console.OutputEncoding <- System.Text.Encoding.UTF8
#endif

module Yarn =
    open YarnHelper

    let install workingDir =
        Yarn (fun p ->
            { p with
                Command = YarnCommand.Install InstallArgs.Standard
                WorkingDirectory = workingDir
            })

    let run workingDir script args =
        Yarn (fun p ->
            { p with
                Command = YarnCommand.Custom ("run " + script + " " + args)
                WorkingDirectory = workingDir
            })

let runBashOrCmd cwd (scriptFileName: string) args =
    if isWindows
    then run cwd (scriptFileName.Replace("/", "\\") + ".cmd") args
    else run cwd "sh" (scriptFileName + ".sh " + args)

let addToPath newPath =
    let path = environVarOrDefault "PATH" ""
    let separator = if isWindows then ";" else ":"
    setEnvironVar "PATH" (newPath + separator + path)

// Move this to FakeHelpers
let findLineAndGetGroupValue regexPattern (groupIndex: int) filePath =
    let reg = Regex(regexPattern)
    File.ReadLines(filePath)
    |> Seq.tryPick (fun line ->
        let m = reg.Match(line)
        if m.Success
        then Some m.Groups.[groupIndex].Value
        else None)
    // Appveyor doesn't like `Option.defaultWith`
    |> function
        | Some x -> x
        | None -> failwithf "No line matches pattern %s in file %s" regexPattern filePath

// Project info
let project = "Fable"

let gitOwner = "fable-compiler"
let gitHome = "https://github.com/" + gitOwner

let mutable dotnetExePath = environVarOrDefault "DOTNET" "dotnet"

let CWD = __SOURCE_DIRECTORY__
let cliBuildDir = CWD </> "build/fable"
let coreJsBuildDir = CWD </> "build/fable-core"
let cliSrcDir = CWD </> "src/dotnet/Fable.Compiler"
let coreJsSrcDir = CWD </> "src/js/fable-core"
let ncaveFcsForkRepo = "https://github.com/ncave/FSharp.Compiler.Service"
let ncaveFcsForkBranch = "fable2"

// Targets
let installDotnetSdk () =
    let dotnetcliVersion =
        Path.Combine(__SOURCE_DIRECTORY__, "global.json")
        |> findLineAndGetGroupValue "\"version\": \"(.*?)\"" 1

    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
    if Path.IsPathRooted(dotnetExePath) then
        Path.GetDirectoryName(dotnetExePath) |> addToPath
    run CWD dotnetExePath "--version"

let clean () =
    !! "build" ++ "**/.fable"
    |> CleanDirs

let nugetRestore dir =
    run dir dotnetExePath "restore"

type BuildConfig = Release | Debug

let buildCLI cfg () =
    sprintf "publish -o %s -c %s" cliBuildDir
        (match cfg with Release -> "Release" | Debug -> "Debug")
    |> run cliSrcDir dotnetExePath

let buildCoreJsTypescriptFiles () =
    Yarn.run CWD "tsc" (sprintf "--project %s" coreJsSrcDir)

let runFableCli command fableArgs commandArgs =
    sprintf
        // "%s/dotnet-fable.dll %s %s -- %s" cliBuildDir
        "run -c Release -p %s %s %s --args \"%s\"" cliSrcDir
        command fableArgs commandArgs
    |> run CWD dotnetExePath

let buildCoreJsFsharpFiles () =
    runFableCli
        "fable-splitter"
        "--fable-core force:${outDir}"  // fable-splitter will adjust the path
        "-c src/js/fable-core/splitter.config.js"

let buildSplitter () =
    let buildDir = CWD </> "src/js/fable-splitter"
    Yarn.install CWD
    // Yarn.run CWD "tslint" [sprintf "--project %s" buildDir]
    Yarn.run CWD "tsc" (sprintf "--project %s" buildDir)
    // Copy JS files
    !! (buildDir + "/src/*.js") |> Seq.iter (fun jsFile ->
        FileUtils.cp jsFile (buildDir + "/dist") )

let buildCoreJsFull () =
    Yarn.install CWD
    Yarn.run CWD "tslint" (sprintf "--project %s" coreJsSrcDir)
    buildCoreJsTypescriptFiles ()
    buildCoreJsFsharpFiles ()
    run (CWD </> "src/tools/InjectProcessor") dotnetExePath "run"

let runTestsDotnet () =
    // CleanDir "tests/Main/obj"
    run (CWD </> "tests/Main") dotnetExePath "run"
    // run (CWD </> "src/dotnet/Fable.JsonConverter/tests") dotnetExePath "test"

let runTestsJS () =
    Yarn.install CWD
    Yarn.run CWD "build-tests" ""

let updateVersionInFile useNugetVersion releaseNotesPath targetFilePath (regexPatternInTargetFile: string) =
    let reg = Regex regexPatternInTargetFile
    let release =
        CWD </> releaseNotesPath
        |> ReleaseNotesHelper.LoadReleaseNotes
    let mainFile = CWD </> targetFilePath
    (reg, mainFile) ||> replaceLines (fun line m ->
        let original = m.Value
        let versionIndex = m.Groups.[1].Index - m.Index
        let versionLastIndex = versionIndex + m.Groups.[1].Value.Length
        reg.Replace(line,
            original.Substring(0, versionIndex) +
            (if useNugetVersion then release.NugetVersion else release.AssemblyVersion) +
            (if versionLastIndex < original.Length then original.Substring(versionLastIndex) else "")
        ) |> Some)

let updateVersionInCliUtil () =
    updateVersionInFile true
        "src/dotnet/Fable.Compiler/RELEASE_NOTES.md"
        "src/dotnet/Fable.Compiler/CLI/CLI.Util.fs"
        @"\bVERSION\s*=\s*""(.*?)"""
    updateVersionInFile false
        "src/dotnet/Fable.Core/RELEASE_NOTES.md"
        "src/dotnet/Fable.Compiler/CLI/CLI.Util.fs"
        @"\bCORE_VERSION\s*=\s*""(.*?)"""

Target "GitHubRelease" (fun _ ->
    let release =
        CWD </> "src/dotnet/Fable.Compiler/RELEASE_NOTES.md"
        |> ReleaseNotesHelper.LoadReleaseNotes
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
Target "FableCoreJs" buildCoreJsFull
Target "FableCoreJsTypescriptOnly" buildCoreJsTypescriptFiles
Target "FableCoreJsFSharpOnly" buildCoreJsFsharpFiles
Target "FableCoreJsInjects" (fun _ ->
    run (CWD </> "src/tools/InjectProcessor") dotnetExePath "run")
Target "FableSplitter" buildSplitter
Target "RunTestsJS" runTestsJS
Target "RunTestsDotnet" runTestsDotnet

Target "PublishPackages" (fun () ->
    let baseDir = CWD </> "src"
    let packages = [
        // Nuget packages
        Package("dotnet/Fable.Core/Fable.Core.fsproj", (fun () ->
            updateVersionInFile false
                "src/dotnet/Fable.Core/RELEASE_NOTES.md"
                "src/dotnet/Fable.Core/AssemblyInfo.fs"
                @"\bAssemblyVersion\s*\(\s*""(.*?)"""
        ))
        Package("dotnet/Fable.Compiler/Fable.Compiler.fsproj", (fun () ->
            buildCoreJsFull ()
            updateVersionInCliUtil ()
        ), pkgName="dotnet-fable", msbuildProps=["NugetPackage", "true"])
        // NPM packages
        Package "js/fable-utils"
        Package "js/fable-loader"
        Package "js/rollup-plugin-fable"
        Package("js/fable-splitter", buildSplitter)
    ]
    installDotnetSdk ()
    publishPackages2 baseDir dotnetExePath packages
)

let bundleRepl (fetchNcaveFork: bool) () =
    if fetchNcaveFork then
        let fcsFableDir =
            // Appveyor has problems with too long paths so download the fork closer to root
            // TODO: Another option for local Windows Systems (not AppVeyor)
            match environVarOrNone "APPVEYOR" with
            | Some _ -> "/projects/fcs"
            | None -> CWD </> "paket-files/ncave/FCS"

        let fableJsProj = CWD </> "src/dotnet/Fable.Repl/Fable.Repl.fsproj"
        let fcsFableProj = fcsFableDir </> "fcs/fcs-fable/fcs-fable.fsproj"

        let reg = Regex(@"ProjectReference Include="".*?""")
        (reg, fableJsProj) ||> replaceLines (fun line _ ->
            let replacement = reg.Replace(line, sprintf @"ProjectReference Include=""%s""" fcsFableProj)
            printfn "REPLACED:\n\t%s\n\t%s" line replacement
            Some replacement)

        CleanDir fcsFableDir
        Repository.cloneSingleBranch CWD ncaveFcsForkRepo ncaveFcsForkBranch fcsFableDir

        runBashOrCmd fcsFableDir "fcs/build" "CodeGen.Fable"
        Directory.GetFiles(fcsFableDir </> "fcs/fcs-fable/codegen")
        |> Seq.iter (printfn "%s")

    // Build REPL
    Yarn.install CWD
    Yarn.run CWD "build-repl-modules" ""
    Yarn.run CWD "bundle-repl" ""
    Yarn.run CWD "minify-repl" ""

    // Put fable-core files next to bundle
    let replDir = CWD </> "src/dotnet/Fable.Repl"
    let fableCoreTarget = replDir </> "bundle/fable-core"
    FileUtils.cp_r coreJsBuildDir fableCoreTarget
    // These files will be used in the browser, so make sure the import paths include .js extension
    let reg = Regex(@"^import (.*"".*)("".*)$", RegexOptions.Multiline)
    for file in Directory.EnumerateFiles(fableCoreTarget, "*.js", SearchOption.AllDirectories) do
        File.WriteAllText(file, reg.Replace(File.ReadAllText(file), "import $1.js$2"))

    // Write version number in a version.txt file
    let release =
        CWD </> "src/dotnet/Fable.Compiler/RELEASE_NOTES.md"
        |> ReleaseNotesHelper.LoadReleaseNotes
    File.WriteAllText(replDir </> "bundle/version.txt", release.NugetVersion)

let runBench2 () =
    Yarn.install CWD
    Yarn.run CWD "babel" "src/dotnet/Fable.Repl/out --out-dir src/dotnet/Fable.Repl/out2 --plugins @babel/plugin-transform-modules-commonjs --quiet"
    Yarn.run CWD "build-bench2" ""
    Yarn.run CWD "start-bench2" ""

    // Run the test script
    Yarn.run CWD "babel" "build/fable-core --out-dir src/dotnet/Fable.Repl/out-fable-core --plugins @babel/plugin-transform-modules-commonjs --quiet"
    Yarn.run CWD "test-bench2" ""

Target "All" (fun () ->
    installDotnetSdk ()
    clean ()
    buildCoreJsFull ()
    runTestsJS ()

    match environVarOrNone "APPVEYOR", environVarOrNone "TRAVIS" with
    | Some _, _ -> runTestsDotnet (); bundleRepl true (); runBench2 ()
    // .NET tests fail most of the times in Travis for obscure reasons
    | _, Some _ -> () // bundleRepl true ()
    // Don't build repl locally (takes too long)
    | None, None -> runTestsDotnet ()
)

Target "BundleReplLocally" (fun () ->
    printfn "Make sure you've run target All or FableCoreJs before this."
    printfn "You need to clone '%s' branch of '%s' repository in a sibling folder named: %s"
        ncaveFcsForkBranch ncaveFcsForkRepo "FSharp.Compiler.Service_fable"
    bundleRepl false ())

Target "BuildReplAsNodeApp" (fun () ->
    printfn "Make sure you've run target BundleReplLocally before this."
    printfn "Check 'start-bench2' script in package.json to see how to run the REPL as a node app."
    runBench2 ()
)

"PublishPackages"
==> "GitHubRelease"

// Start build
RunTargetOrDefault "All"