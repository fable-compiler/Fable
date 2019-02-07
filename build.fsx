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

let buildLibrary() =
    cleanDirs ["build/fable-library"]
    buildTypescript "src/fable-library"
    run "npx fable-splitter -c src/fable-library/splitter.config.js"

let test() =
    if pathExists "build/fable-library" |> not then
        buildLibrary()

    cleanDirs ["build/tests"]
    run "npx fable-splitter -c tests/splitter.config.js"
    run "npx mocha build/tests --reporter dot -t 10000"

let buildCompiler() =
    let projectDir = "src/fable-compiler"
    cleanDirs [projectDir </> "dist"; projectDir </> "bin"]
    buildTypescript projectDir
    updateVersionInCliUtil()
    run "dotnet publish -c Release -o ../fable-compiler/bin/fable-compiler src/Fable.Cli/Fable.Cli.fsproj"

    buildLibrary()
    copyDirRecursive "build/fable-library" (projectDir </> "bin/fable-library")

match args with
| IgnoreCase "test"::_ ->
    test()

| IgnoreCase "library"::_ ->
    buildLibrary()

| IgnoreCase "compiler"::_ ->
    buildCompiler()

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