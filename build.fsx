#r "node_modules/fable-compiler-js/lib/Fable.Core.dll"
#load "src/fable-publish-utils/PublishUtils.fs"
open PublishUtils
open System
open System.Text.RegularExpressions

let FABLE_BRANCH = "master"
let APPVEYOR_REPL_ARTIFACT_URL_PARAMS = "?branch=" + FABLE_BRANCH + "&pr=false"
let APPVEYOR_REPL_ARTIFACT_URL =
    "https://ci.appveyor.com/api/projects/fable-compiler/Fable/artifacts/src/fable-standalone/fable-standalone.zip"
    + APPVEYOR_REPL_ARTIFACT_URL_PARAMS

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
    buildLibrary()
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
        runInDir "src/fable-compiler-js/test" "node .. test_script.fsx --commonjs"
        runInDir "src/fable-compiler-js/test" "node bin/test_script.js"

let downloadStandalone() =
    let targetDir = "src/fable-standalone/dist"
    cleanDirs [targetDir]
    downloadAndExtractTo APPVEYOR_REPL_ARTIFACT_URL targetDir

let packages =
    ["fable-babel-plugins", doNothing
     "fable-compiler", buildCompiler
     "fable-compiler-js", (fun () -> buildCompilerJs false)
     "fable-loader", doNothing
     "fable-metadata", doNothing
     "fable-publish-utils", doNothing
     "fable-splitter", doNothing
     "fable-standalone", downloadStandalone
    ]

match argsLower with
| "test"::_ -> test()
| "library"::_ -> buildLibrary()
| "compiler"::_ -> buildCompiler()
| "standalone"::_ -> buildStandalone()
| "compiler-js"::_ -> buildCompilerJs false
| "download-standalone"::_ -> downloadStandalone()

| "publish"::restArgs ->
    let packages =
        match List.tryHead restArgs with
        | Some pkg -> packages |> List.filter (fun (name,_) -> name = pkg)
        | None -> packages
    for (pkg, buildAction) in packages do
        pushNpm ("src" </> pkg) buildAction

| _ ->
    printfn "Please pass a target name"

printf "Build finished successfully"