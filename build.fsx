#r "packages/build/FAKE/tools/FakeLib.dll"
#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
#load "paket-files/build/fable-compiler/fake-helpers/Fable.FakeHelpers.fs"

open System
open System.IO
open System.Net
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

// Project info
let project = "Fable"

let gitOwner = "fable-compiler"
let gitHome = "https://github.com/" + gitOwner

let mutable dotnetExePath = environVarOrDefault "DOTNET" "dotnet"

let CWD = __SOURCE_DIRECTORY__
let cliBuildDir = CWD </> "build/fable-compiler"
let libraryBuildDir = CWD </> "build/fable-library"
let cliSrcDir = CWD </> "src/dotnet/Fable.Compiler"
let librarySrcDir = CWD </> "src/js/fable-library"
let ncaveFcsForkRepo = "https://github.com/ncave/FSharp.Compiler.Service"
let ncaveFcsForkBranch = "fable"
let ncaveFcsCodeGenTarget = "fcs/build CodeGen.Fable"
let APPVEYOR_REPL_ARTIFACT_URL = "https://ci.appveyor.com/api/projects/fable-compiler/Fable/artifacts/src/dotnet/Fable.Repl/repl-bundle.zip?branch=master&pr=false"

// Targets
let downloadArtifact path (url: string) =
    let tempFile = Path.ChangeExtension(Path.GetTempFileName(), ".zip")
    use client = new WebClient()
    printfn "GET %s" url
    client.DownloadFile(Uri url, tempFile)
    CleanDir path
    Unzip path tempFile
    File.Delete tempFile
    printfn "Artifact unzipped at %s" path

let installDotnetSdk () =
    let dotnetcliVersion =
        Path.Combine(__SOURCE_DIRECTORY__, "global.json")
        |> findLineAndGetGroupValue "\"version\": \"(.*?)\"" 1

    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
    if Path.IsPathRooted(dotnetExePath) then
        Path.GetDirectoryName(dotnetExePath) |> addToPath
    run CWD dotnetExePath "--version"

let clean () =
    !! "build" ++ "**/.fable" ++ "src/**/obj"
    |> CleanDirs

let nugetRestore dir =
    run dir dotnetExePath "restore"

type BuildConfig = Release | Debug

let buildCompilerDotnet cfg outDir () =
    sprintf "publish -o %s -c %s" (CWD </> outDir)
        (match cfg with Release -> "Release" | Debug -> "Debug")
    |> run cliSrcDir dotnetExePath

let buildLibraryTypescriptFiles () =
    Yarn.run CWD "tsc" (sprintf "--project %s" librarySrcDir)

let runFableCli command fableArgs commandArgs =
    sprintf
        // "%s/dotnet-fable.dll %s %s -- %s" cliBuildDir
        "run -c Release -p %s %s %s --args \"%s\"" cliSrcDir
        command fableArgs commandArgs
    |> run CWD dotnetExePath

let buildLibraryFsharpFiles () =
    runFableCli
        "fable-splitter"
        "--fable-library force:${outDir}"  // fable-splitter will adjust the path
        "-c src/js/fable-library/splitter.config.js"

let buildTypescript projectDir () =
    Yarn.install CWD
    let projectDir = CWD </> projectDir
    // Yarn.run CWD "tslint" [sprintf "--project %s" projectDir]
    Yarn.run CWD "tsc" (sprintf "--project %s" projectDir)

let buildNpmPackage pkgDir () =
    Yarn.install CWD
    let pkgDir = CWD </> pkgDir
    run pkgDir "npm" "install"
    run pkgDir "npm" "run build"
    run pkgDir "npm" "test"

let buildLibraryFull () =
    Yarn.install CWD
    Yarn.run CWD "tslint" (sprintf "--project %s" librarySrcDir)
    buildLibraryTypescriptFiles ()
    buildLibraryFsharpFiles ()
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

let githubRelease releaseNotesDir () =
    let release =
        CWD </> releaseNotesDir </> "RELEASE_NOTES.md"
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

    // Put fable-library files next to bundle
    let replDir = CWD </> "src/dotnet/Fable.Repl"
    let libraryTarget = replDir </> "bundle/fable-library"
    FileUtils.cp_r libraryBuildDir libraryTarget
    // These files will be used in the browser, so make sure the import paths include .js extension
    let reg = Regex(@"^import (.*"".*)("".*)$", RegexOptions.Multiline)
    for file in Directory.EnumerateFiles(libraryTarget, "*.js", SearchOption.AllDirectories) do
        File.WriteAllText(file, reg.Replace(File.ReadAllText(file), "import $1.js$2"))

    // Write version number in a version.txt file
    let release =
        CWD </> "src/dotnet/Fable.Compiler/RELEASE_NOTES.md"
        |> ReleaseNotesHelper.LoadReleaseNotes
    File.WriteAllText(replDir </> "bundle/version.txt", release.NugetVersion)

let buildNpmFableCompilerJs () =
    let replDir = CWD </> "src/dotnet/Fable.Repl"
    let distDir = CWD </> "src/js/fable-compiler-js/dist"
    if not (Directory.Exists(replDir </> "bundle")) then
        // bundleRepl false ()
        downloadArtifact (replDir </> "bundle") APPVEYOR_REPL_ARTIFACT_URL
    CleanDir distDir
    FileUtils.cp_r (replDir </> "bundle") (distDir </> "bundle")
    FileUtils.cp_r (replDir </> "metadata2") (distDir </> "metadata2")
    Yarn.run CWD "babel" (sprintf "%s --out-dir %s --plugins @babel/plugin-transform-modules-commonjs --quiet"
        (replDir </> "bundle/fable-library") (distDir </> "fable-library-commonjs"))
    Yarn.run CWD "build-compiler" ""

let buildNpmFableCompilerDotnet () =
    let projectDir = "src/js/fable-compiler"
    CleanDir (projectDir </> "dist")
    CleanDir (projectDir </> "bin")
    buildTypescript projectDir ()
    buildCompilerDotnet Release (projectDir </> "bin/fable-compiler") ()
    buildLibraryTypescriptFiles ()
    buildLibraryFsharpFiles ()
    FileUtils.cp_r libraryBuildDir (projectDir </> "bin/fable-library")

let runBench2 () =
    Yarn.install CWD
    Yarn.run CWD "babel" "src/dotnet/Fable.Repl/out --out-dir src/dotnet/Fable.Repl/out2 --plugins @babel/plugin-transform-modules-commonjs --quiet"
    Yarn.run CWD "build-bench2" ""
    Yarn.run CWD "start-bench2" ""

    // Run the test script
    Yarn.run CWD "babel" "build/fable-library --out-dir src/dotnet/Fable.Repl/out-fable-library --plugins @babel/plugin-transform-modules-commonjs --quiet"
    Yarn.run CWD "test-bench2" ""

Target "Clean" clean
Target "FableLibrary" buildLibraryFull
Target "FableLibraryTypescriptOnly" buildLibraryTypescriptFiles
Target "FableLibraryFSharpOnly" buildLibraryFsharpFiles
Target "FableLibraryInjects" (fun _ ->
    run (CWD </> "src/tools/InjectProcessor") dotnetExePath "run")
Target "fable-splitter" (buildNpmPackage "src/js/fable-splitter")
Target "fable-compiler" buildNpmFableCompilerDotnet
Target "fable-compiler-js" buildNpmFableCompilerJs
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
            buildLibraryFull ()
            updateVersionInCliUtil ()
        ), pkgName="dotnet-fable", msbuildProps=["NugetPackage", "true"])
        // NPM packages
        Package("js/fable-compiler-js", buildNpmFableCompilerJs)
        Package("js/fable-compiler", buildNpmFableCompilerDotnet)
        Package "js/fable-loader"
        Package "js/rollup-plugin-fable"
        Package("js/fable-splitter", buildNpmPackage "src/js/fable-splitter")
    ]
    installDotnetSdk ()
    clean ()
    publishPackages2 baseDir dotnetExePath packages
)

Target "All" (fun () ->
    installDotnetSdk ()
    clean ()
    buildLibraryFull ()
    runTestsJS ()

    match environVarOrNone "APPVEYOR", environVarOrNone "TRAVIS" with
    | Some _, _ -> runTestsDotnet (); bundleRepl true (); runBench2 ()
    | _, Some _ -> () // .NET tests fail in Travis for obscure reasons
    | None, None -> runTestsDotnet () // Don't build repl locally (takes too long)
)

Target "BundleReplLocally" (fun () ->
    printfn "Make sure you've run target All or FableLibrary before this."
    printfn "You need to clone '%s' branch of '%s' repository in a sibling folder named: %s."
        ncaveFcsForkBranch ncaveFcsForkRepo "FSharp.Compiler.Service_fable"
    printfn "And run '%s' build target." ncaveFcsCodeGenTarget
    bundleRepl false ())

Target "BuildReplAsNodeApp" (fun () ->
    printfn "Make sure you've run target BundleReplLocally before this."
    printfn "Check 'start-bench2' script in package.json to see how to run the REPL as a node app."
    runBench2 ()
)

Target "GitHubRelease" (githubRelease "src/js/fable-compiler")

"PublishPackages"
==> "GitHubRelease"

// Start build
RunTargetOrDefault "All"