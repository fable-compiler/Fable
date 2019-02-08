#r "node_modules/fable-compiler-js/lib/Fable.Core.dll"
#load "src/fable-publish-utils/PublishUtils.fs"
open PublishUtils
open System
open System.Text.RegularExpressions

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

let buildTypescript projectDir =
    // run ("npx tslint --project " + projectDir)
    run ("npx tsc --project " + projectDir)

let buildSplitter projectDir =
    run ("npx fable-splitter -c " + (projectDir </> "splitter.config.js"))

let buildWebpack projectDir =
    run ("npx webpack --config " + (projectDir </> "webpack.config.js"))

let buildLibrary() =
    cleanDirs ["build/fable-library"]
    buildTypescript "src/fable-library"
    buildSplitter "src/fable-library"

let buildCompiler() =
    let projectDir = "src/fable-compiler"
    cleanDirs [projectDir </> "dist"; projectDir </> "bin"]
    buildTypescript projectDir
    updateVersionInCliUtil()
    run "dotnet publish -c Release -o ../fable-compiler/bin/fable-compiler src/Fable.Cli/Fable.Cli.fsproj"

    buildLibrary()
    copyDirRecursive "build/fable-library" (projectDir </> "bin/fable-library")

let buildStandalone() =
    let projectDir = "src/fable-standalone"
    cleanDirs [projectDir </> "dist"; projectDir </> "out"]

    buildSplitter projectDir
    buildWebpack projectDir
    fileSizeInBytes (projectDir </> "dist/fable-standalone.min.js") / 1000
    |> printfn "fable-standalone bundle size: %iKB"

    // Put fable-library files next to bundle
    let libraryTarget = projectDir </> "dist/fable-library"
    copyDirRecursive "build/fable-library" libraryTarget
    // These files will be used in the browser, so make sure the import paths include .js extension
    let reg = Regex(@"^import (.*"".*)("".*)$", RegexOptions.Multiline)
    dirFiles libraryTarget
    |> Array.filter (fun file -> file.EndsWith(".js"))
    |> Array.iter (fun file ->
        reg.Replace(readFile file, "import $1.js$2")
        |> writeFile file)

    // Bump version
    let compilerVersion = Publish.loadReleaseVersion "src/fable-compiler"
    let standaloneVersion = Publish.loadNpmVersion projectDir
    let (comMajor, comMinor, _, comPrerelease) = Publish.splitVersion compilerVersion
    let (staMajor, staMinor, staPatch, _) = Publish.splitVersion standaloneVersion
    Publish.bumpNpmVersion projectDir
        (if comMajor > staMajor || comMinor > staMinor then compilerVersion
         else sprintf "%i.%i.%i%s" staMajor staMinor (staPatch + 1) comPrerelease)

let test() =
    if pathExists "build/fable-library" |> not then
        buildLibrary()

    cleanDirs ["build/tests"]
    buildSplitter "tests"
    run "npx mocha build/tests --reporter dot -t 10000"

    if environVarOrNone "APPVEYOR" |> Option.isSome then
        runInDir "tests/Main" "dotnet run"
        buildStandalone()


match args with
| IgnoreCase "test"::_ ->
    test()

| IgnoreCase "library"::_ ->
    buildLibrary()

| IgnoreCase "compiler"::_ ->
    buildCompiler()

| IgnoreCase "standalone"::_ ->
    buildStandalone()

| IgnoreCase "publish"::project ->
    match project with
    | [] -> failwith "Pass the project to publish"
    | IgnoreCase "compiler"::_
    | IgnoreCase "fable-compiler"::_ ->
        buildCompiler()
        pushNpm "src/fable-compiler"
    | _ -> failwithf "Cannot publish %A" project

| _ ->
    printfn "Please pass a target name"

printf "Build finished successfully"