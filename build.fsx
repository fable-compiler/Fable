#r "src/fable-metadata/lib/Fable.Core.dll"
#load "src/fable-publish-utils/PublishUtils.fs"

open PublishUtils
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import

// Appveyor artifact
let FABLE_BRANCH = "master"
let APPVEYOR_REPL_ARTIFACT_URL_PARAMS = "?branch=" + FABLE_BRANCH + "&pr=false"
let APPVEYOR_REPL_ARTIFACT_URL =
    "https://ci.appveyor.com/api/projects/fable-compiler/Fable/artifacts/src/fable-standalone/fable-standalone.zip"
    + APPVEYOR_REPL_ARTIFACT_URL_PARAMS

// ncave FCS fork
let NCAVE_FORK = "https://github.com/ncave/FSharp.Compiler.Service"
let NCAVE_FORK_LOCAL = "../FSharp.Compiler.Service"
let NCAVE_FORK_FCS_FABLE_BRANCH = "fable"

type GhRealeases =
    [<Emit("""new Promise((succeed, fail) =>
        $0.create({user: $1, token: $2}, $3, $4, { tag_name: $5, name: $5, body: $6 }, (err, res) =>
            err != null ? fail(err) : succeed(res)))""")>]
    abstract create: user: string * token: string * owner: string * repo: string * name: string * msg: string -> JS.Promise<obj>

let cleanDirs dirs =
    for dir in dirs do
        removeDirRecursive dir

let updateVersionInCliUtil() =
    let filePath = "src/Fable.Cli/Util.fs"
    let version = Publish.loadReleaseVersion "src/fable-compiler"
    // printfn "VERSION %s" version
    Regex.Replace(
        readFile filePath,
        @"let \[<Literal>] VERSION = "".*?""",
        sprintf "let [<Literal>] VERSION = \"%s\"" version)
    |> writeFile filePath

let downloadAndExtractTo (url: string) (targetDir: string) =
    sprintf "npx download --extract --out %s \"%s\"" targetDir url |> run

let buildTypescript projectDir =
    // run ("npx tslint --project " + projectDir)
    run ("npx tsc --project " + projectDir)

let buildSplitterWithArgs projectDir args =
    run ("npx fable-splitter -c " + (projectDir </> "splitter.config.js") + " " + args)

let buildSplitter projectDir =
    buildSplitterWithArgs projectDir ""

let buildWebpack projectDir =
    run ("npx webpack --config " + (projectDir </> "webpack.config.js"))

let buildLibrary() =
    cleanDirs ["build/fable-library"]
    buildTypescript "src/fable-library"
    buildSplitter "src/fable-library"

let buildCompiler() =
    let projectDir = "src/fable-compiler"
    let libraryDir = "build/fable-library"
    cleanDirs [projectDir </> "dist"; projectDir </> "bin"]
    buildTypescript projectDir
    updateVersionInCliUtil()
    run "dotnet publish -c Release -o ../fable-compiler/bin/fable-cli src/Fable.Cli/Fable.Cli.fsproj"
    buildLibrary()
    copyDirRecursive libraryDir (projectDir </> "bin/fable-library")

let buildCompilerJs testLocal =
    let projectDir = "src/fable-compiler-js"
    let libraryDir = "build/fable-library"
    cleanDirs [projectDir </> "dist"]
    if testLocal then
        buildSplitterWithArgs projectDir "--test-local"
    else
        buildSplitter projectDir
        buildLibrary()
    copyDirRecursive libraryDir "src/fable-compiler-js/dist/fable-library"
    runInDir projectDir "npx babel dist/fable-library --out-dir dist/fable-library-commonjs --plugins @babel/plugin-transform-modules-commonjs --quiet"

let buildStandalone() =
    let projectDir = "src/fable-standalone"
    let libraryDir = "build/fable-library"
    cleanDirs [projectDir </> "dist"]
    buildLibrary()

    // ES2015 modules
    buildSplitter projectDir
    // commonjs
    run "npx babel src/fable-standalone/dist/es2015 --out-dir  src/fable-standalone/dist/commonjs --plugins @babel/plugin-transform-modules-commonjs --quiet"
    // Web Worker
    buildWebpack "src/fable-standalone/src/Worker"
    // fileSizeInBytes (projectDir </> "dist/worker.min.js") / 1000
    // |> printfn "Web worker bundle size: %iKB"

    // Put fable-library files next to bundle
    let libraryTarget = projectDir </> "dist/fable-library"
    copyDirRecursive libraryDir libraryTarget
    // These files will be used in the browser, so make sure the import paths include .js extension
    let reg = Regex(@"^import (.*"".*)("".*)$", RegexOptions.Multiline)
    getFullPathsInDirectoryRecursively libraryTarget
    |> Array.filter (fun file -> file.EndsWith(".js"))
    |> Array.iter (fun file ->
        reg.Replace(readFile file, "import $1.js$2")
        |> writeFile file)

    // Bump version
    // let compilerVersion = Publish.loadReleaseVersion "src/fable-compiler"
    // let standaloneVersion = Publish.loadNpmVersion projectDir
    // let (comMajor, comMinor, _, comPrerelease) = Publish.splitVersion compilerVersion
    // let (staMajor, staMinor, staPatch, _) = Publish.splitVersion standaloneVersion
    // Publish.bumpNpmVersion projectDir
    //     (if comMajor > staMajor || comMinor > staMinor then compilerVersion
    //      else sprintf "%i.%i.%i%s" staMajor staMinor (staPatch + 1) comPrerelease)

let test() =
    if pathExists "build/fable-library" |> not then
        buildLibrary()

    cleanDirs ["build/tests"]
    buildSplitter "tests"
    run "npx mocha build/tests --reporter dot -t 10000"

    if envVarOrNone "APPVEYOR" |> Option.isSome then
        runInDir "tests/Main" "dotnet run"
        buildStandalone()
        // Test fable-compiler-js locally
        buildCompilerJs true
        run "node src/fable-compiler-js tests/Main/Fable.Tests.fsproj build/tests-js --commonjs"
        run "npx mocha build/tests-js --reporter dot -t 10000"
        // runInDir "src/fable-compiler-js/test" "node .. test_script.fsx --commonjs"
        // runInDir "src/fable-compiler-js/test" "node bin/test_script.js"

let downloadStandalone() =
    let targetDir = "src/fable-standalone/dist"
    cleanDirs [targetDir]
    downloadAndExtractTo APPVEYOR_REPL_ARTIFACT_URL targetDir

// TODO: Run fable-splitter tests
let buildFableSplitter() =
    buildTypescript "src/fable-splitter"

let githubRelease() =
    match envVarOrNone "GITHUB_USER", envVarOrNone "GITHUB_TOKEN" with
    | Some user, Some token ->
        async {
            try
                let ghreleases: GhRealeases = JsInterop.importAll "ghreleases"
                let! version, notes = Publish.loadReleaseVersionAndNotes "src/fable-compiler"
                let! res = ghreleases.create(user, token, "fable-compiler", "Fable", version, String.concat "\n" notes) |> Async.AwaitPromise
                printfn "Github release %s created successfully" version
            with ex ->
                printfn "Github release failed: %s" ex.Message
        } |> Async.StartImmediate
    | _ -> failwith "Expecting GITHUB_USER and GITHUB_TOKEN enviromental variables"

let packages =
    ["fable-babel-plugins", doNothing
     "fable-compiler", buildCompiler
     "fable-compiler-js", (fun () -> buildCompilerJs false)
     "fable-loader", doNothing
     "fable-metadata", doNothing
     "fable-publish-utils", doNothing
     "fable-splitter", buildFableSplitter
     "fable-standalone", downloadStandalone
    ]

let publishPackages restArgs =
    let packages =
        match List.tryHead restArgs with
        | Some pkg -> packages |> List.filter (fun (name,_) -> name = pkg)
        | None -> packages
    for (pkg, buildAction) in packages do
        pushNpm ("src" </> pkg) buildAction

match argsLower with
| "test"::_ -> test()
| "library"::_ -> buildLibrary()
| "compiler"::_ -> buildCompiler()
| "compiler-js"::_ -> buildCompilerJs false
| "splitter"::_ -> buildFableSplitter()
| "standalone"::_ -> buildStandalone()
| "download-standalone"::_ -> downloadStandalone()
| "publish"::restArgs -> publishPackages restArgs
| "github-release"::_ ->
    publishPackages []
    githubRelease ()
| "sync-fcs-fable"::_ ->
    printfn "Expecting %s repo to be cloned at %s and '%s' branch checked out"
        NCAVE_FORK NCAVE_FORK_LOCAL NCAVE_FORK_FCS_FABLE_BRANCH

    runBashOrCmd (NCAVE_FORK_LOCAL </> "fcs") "build" "CodeGen.Fable"
    copyDirRecursive (NCAVE_FORK_LOCAL </> "fcs/fcs-fable") "src/fcs-fable"
    copyDirRecursive (NCAVE_FORK_LOCAL </> "src") "src/fcs-fable/src"
    removeFile "src/fcs-fable/.gitignore"
    let fcsFableProj = "src/fcs-fable/fcs-fable.fsproj"
    Regex.Replace(
            readFile fcsFableProj,
            @"(<FSharpSourcesRoot>\$\(MSBuildProjectDirectory\)).*?(<\/FSharpSourcesRoot>)",
            "$1/src$2")
    |> writeFile fcsFableProj
| _ ->
    printfn "Please pass a target name"

printf "Build finished successfully"