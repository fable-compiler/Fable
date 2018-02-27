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

// Project info
let project = "Fable"

let gitOwner = "fable-compiler"
let gitHome = "https://github.com/" + gitOwner

let dotnetcliVersion = "2.1.4"
let mutable dotnetExePath = environVarOrDefault "DOTNET" "dotnet"

let CWD = __SOURCE_DIRECTORY__
let cliBuildDir = CWD </> "build/fable"
let cliSrcDir = CWD </> "src/dotnet/Fable.Compiler"
let coreJsSrcDir = CWD </> "src/js/fable-core"

// Targets
let installDotnetSdk () =
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
    if Path.IsPathRooted(dotnetExePath) then
        Path.GetDirectoryName(dotnetExePath) |> addToPath
    run CWD dotnetExePath "--version"

let clean_ (full: bool) =
    !! "src/dotnet/**/bin"
        -- "src/dotnet/Fable.Client.JS/demo/**"
        -- "src/dotnet/Fable.Client.JS/testapp/**"
        ++ "tests*/**/bin"
        ++ "build"
        ++ "**/.fable"
    |> CleanDirs

    if full then
        !! "src/dotnet/**/obj"
            ++"tests**/**/obj"
        |> CleanDirs

let clean () = clean_ false
let fullClean () = clean_ true

let nugetRestore dir =
    run dir dotnetExePath "restore"

type BuildConfig = Release | Debug

let buildCLI cfg () =
    sprintf "publish -o %s -c %s" cliBuildDir
        (match cfg with Release -> "Release -v n" | Debug -> "Debug")
    |> run cliSrcDir dotnetExePath

let buildCoreJS () =
    Yarn.install CWD
    Yarn.run CWD "tslint" (sprintf "--project %s" coreJsSrcDir)
    Yarn.run CWD "tsc" (sprintf "--project %s" coreJsSrcDir)

    // Compile F# files
    nugetRestore coreJsSrcDir
    sprintf "%s/Fable.Compiler.dll node-run %s --fable-core %s -- -c splitter.config.js"
        cliBuildDir
        "../fable-splitter/dist/cli"
        "force:${outDir}" // fable-splitter will adjust the path
    |> run coreJsSrcDir dotnetExePath

let buildSplitter () =
    let buildDir = CWD </> "src/js/fable-splitter"
    Yarn.install CWD
    // Yarn.run CWD "tslint" [sprintf "--project %s" buildDir]
    Yarn.run CWD "tsc" (sprintf "--project %s" buildDir)
    // Copy JS files
    !! (buildDir + "/src/*.js") |> Seq.iter (fun jsFile ->
        FileUtils.cp jsFile (buildDir + "/dist") )

let buildJsonConverter () =
    // "restore src/dotnet/Fable.JsonConverter"
    // |> run CWD dotnetExePath
    "build src/dotnet/Fable.JsonConverter -c Release -o ../../../build/json-converter /p:TargetFramework=netstandard1.6"
    |> run CWD dotnetExePath

let runTestsDotnet () =
    // CleanDir "tests/Main/obj"
    run (CWD </> "tests/Main") dotnetExePath "run"
    // run (CWD </> "src/dotnet/Fable.JsonConverter/tests") dotnetExePath "test"

let runTestsJS () =
    Yarn.install CWD
    run CWD dotnetExePath "restore tests/Main"
    run CWD dotnetExePath "build/fable/Fable.Compiler.dll yarn-splitter --cwd tests --fable-core build/fable-core --verbose --port free"
    Yarn.run (CWD </> "tests") "test" ""

let quickTest() =
    run "src/tools" dotnetExePath "../../build/fable/Fable.Compiler.dll yarn-run rollup"
    run CWD "node" "src/tools/temp/QuickTest.js"

Target "QuickTest" quickTest
Target "QuickFableCompilerTest" (fun () ->
    buildCLI Debug ()
    quickTest ())
Target "QuickFableCoreTest" (fun () ->
    buildCoreJS ()
    quickTest ())

let updateVersionInToolsUtil () =
    let reg = Regex(@"\bVERSION\s*=\s*""(.*?)""")
    let release =
        CWD </> "src/dotnet/Fable.Compiler/RELEASE_NOTES.md"
        |> ReleaseNotesHelper.LoadReleaseNotes
    let mainFile = CWD </> "src/dotnet/Fable.Compiler/ToolsUtil.fs"
    (reg, mainFile) ||> replaceLines (fun line m ->
        let replacement = sprintf "VERSION = \"%s\"" release.NugetVersion
        reg.Replace(line, replacement) |> Some)

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
Target "FullClean" fullClean
Target "FableCLI" (buildCLI Release)
Target "FableCoreJS" (fun _ ->
    buildCLI Release ()
    buildSplitter ()
    buildCoreJS ())
Target "FableCoreJSFast" buildCoreJS
Target "FableSplitter" buildSplitter
Target "JsonConverter" buildJsonConverter
Target "RunTestsJS" runTestsJS
Target "RunTestsDotnet" runTestsDotnet

Target "PublishPackages" (fun () ->
    let baseDir = CWD </> "src"
    let packages = [
        // Nuget packages
        Some buildCoreJS, "dotnet/Fable.Core/Fable.Core.fsproj"
        None, "dotnet/Fable.Compiler/Fable.Compiler.fsproj"
        Some updateVersionInToolsUtil, "dotnet/Fable.Compiler/Fable.Compiler.fsproj"
        None, "dotnet/Fable.JsonConverter/Fable.JsonConverter.fsproj"
        None, "plugins/nunit/Fable.Plugins.NUnit.fsproj"
        // NPM packages
        None, "js/fable-utils"
        None, "js/fable-loader"
        None, "js/rollup-plugin-fable"
        Some buildSplitter, "js/fable-splitter"
    ]
    installDotnetSdk ()
    fullClean ()
    publishPackages2 baseDir dotnetExePath packages
)

let buildRepl () =
    let replDir = CWD </> "src/dotnet/Fable.JS/demo"
    let testappDir = CWD </> "src/dotnet/Fable.JS/testapp"
    let fcsFork = "https://github.com/ncave/FSharp.Compiler.Service"
    let fcsFableDir =
        // Appveyor has problems with too long paths so download the fork closer to root
        // TODO: Another option for local Windows Systems (not AppVeyor)
        match environVarOrNone "APPVEYOR" with
        | Some _ -> "/projects/fcs"
        | None -> CWD </> "paket-files/ncave/FCS"

    let fableJsProj = CWD </> "src/dotnet/Fable.JS/Fable.JS.fsproj"
    let fcsFableProj = fcsFableDir </> "fcs/fcs-fable/fcs-fable.fsproj"

    CleanDir fcsFableDir
    Repository.cloneSingleBranch CWD fcsFork "fable" fcsFableDir

    runBashOrCmd fcsFableDir "fcs/build" "CodeGen.Fable"
    Directory.GetFiles(fcsFableDir </> "fcs/fcs-fable/codegen")
    |> Seq.iter (printfn "%s")

    let reg = Regex(@"ProjectReference Include="".*?""")
    (reg, fableJsProj) ||> replaceLines (fun line _ ->
        reg.Replace(line, sprintf @"ProjectReference Include=""%s""" fcsFableProj) |> Some)

    // Build and minify REPL
    Yarn.install replDir
    Yarn.run replDir "build" ""
    Yarn.run replDir "minify" ""

    // build fable-core for umd
    sprintf "--project %s -m umd --outDir %s" coreJsSrcDir (replDir </> "repl/build/fable-core")
    |> Yarn.run CWD "tsc"

    // Build testapp
    Yarn.install testappDir
    Yarn.run testappDir "build" ""
    Yarn.run testappDir "start" ""
    Yarn.run testappDir "test" ""

    // Copy generated files to `../fable-compiler.github.io/public/repl/build`
    // let targetDir =  CWD </> "../fable-compiler.github.io/public/repl"
    // if Directory.Exists(targetDir) then
    //     let targetDir = targetDir </> "build"
    //     // fable-core
    //     sprintf "--project %s -m amd --outDir %s" coreJsSrcDir (targetDir </> "fable-core")
    //     |> Yarn.run CWD "tsc"
    //     // REPL bundle
    //     printfn "Copy REPL JS files to %s" targetDir
    //     for file in Directory.GetFiles(replDir </> "repl/build", "*.js") do
    //         FileUtils.cp file targetDir
    //         printfn "> Copied: %s" file


Target "All" (fun () ->
    installDotnetSdk ()
    clean ()
    // nugetRestore "src/dotnet" ()
    buildCLI Release ()
    buildSplitter ()
    buildCoreJS ()

    // Fable 2.0 development
    runTestsJS ()
    runTestsDotnet ()

    // Temporarily disabled for Fable 2.0 development
    // ----------------------------------------------
    // buildJsonConverter ()
    // runTestsJS ()

    // match environVarOrNone "APPVEYOR", environVarOrNone "TRAVIS" with
    // | Some _, _ -> runTestsDotnet (); buildRepl ()
    // // .NET tests fail most of the times in Travis for obscure reasons
    // | _, Some _ -> buildRepl ()
    // // Don't build repl locally (takes too long)
    // | None, None -> runTestsDotnet ()
)

Target "REPL" buildRepl

// Note: build target "All" and "REPL" before this
Target "REPL.test" (fun () ->
    let replTestDir = "src/dotnet/Fable.JS/testapp"
    Yarn.run replTestDir "build" ""
)

"PublishPackages"
==> "GitHubRelease"

// Start build
RunTargetOrDefault "All"