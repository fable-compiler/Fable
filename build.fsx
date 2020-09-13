// This script is compiled by fable-compiler-js and runs entirely on node.js
// You can execute it by typing: npx fable build.fsx --run [ARGUMENTS]

#r "src/fable-metadata/lib/Fable.Core.dll"
#load "src/fable-publish-utils/PublishUtils.fs"

open PublishUtils
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Core.JsInterop

// Appveyor artifact
let FABLE_BRANCH = "master"
let APPVEYOR_REPL_ARTIFACT_URL_PARAMS = "?branch=" + FABLE_BRANCH //+ "&pr=false"
let APPVEYOR_REPL_ARTIFACT_URL =
    "https://ci.appveyor.com/api/projects/fable-compiler/Fable/artifacts/src/fable-standalone/fable-standalone.zip"
    + APPVEYOR_REPL_ARTIFACT_URL_PARAMS

// ncave FCS fork
let FCS_REPO = "https://github.com/ncave/fsharp"
let FCS_REPO_LOCAL = "../fsharp_fable"
let FCS_REPO_FABLE_BRANCH = "fable"
let FCS_REPO_SERVICE_SLIM_BRANCH = "service_slim"

type GhRealeases =
    [<Emit("""new Promise((succeed, fail) =>
        $0.create({user: $1, token: $2}, $3, $4, { tag_name: $5, name: $5, body: $6 }, (err, res) =>
            err != null ? fail(err) : succeed(res)))""")>]
    abstract create: user: string * token: string * owner: string * repo: string * name: string * msg: string -> JS.Promise<obj>

let concurrently(commands: string[]): unit = importDefault "concurrently"

let cleanDirs dirs =
    for dir in dirs do
        removeDirRecursive dir

let updateVersionInCliUtil() =
    let filePath = "src/Fable.Cli/Util.fs"
    let version = Publish.loadReleaseVersion "src/Fable.Cli"
    // printfn "VERSION %s" version
    Regex.Replace(
        readFile filePath,
        @"let \[<Literal>] VERSION = "".*?""",
        sprintf "let [<Literal>] VERSION = \"%s\"" version)
    |> writeFile filePath

let downloadAndExtractTo (url: string) (targetDir: string) =
    sprintf "npx download --extract --out %s \"%s\"" targetDir url |> run

let runTypescript projectDir =
    // run ("npx tslint --project " + projectDir)
    run ("npx tsc --project " + projectDir)

let watchFableWithArgs projectDir args =
    run ("dotnet watch run -p src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

let runFableWithArgs projectDir args =
    run ("dotnet run -c Release -p src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

let runFable projectDir =
    runFableWithArgs projectDir []

let buildLibrary() =
    let buildDir = fullPath "build/fable-library"
    let projDir = fullPath "src/fable-library"

    cleanDirs [buildDir]
    runTypescript projDir

    runFableWithArgs projDir [
        "--define FX_NO_BIGINT"
        "--fable-library src/fable-library"
        "--extension .js"
        "--exclude Fable.Core"
    ]

    // Move js files to build folder
    let moveJsFile oldDir newDir (file: string) =
        if file.EndsWith(".js") then
            let newPath = newDir </> file
            let newDir = dirname newPath
            if not(pathExists newDir) then
                makeDirRecursive(newDir)
            moveFile (oldDir </> file) newPath

    for file in dirFiles projDir do
        if isDirectory (projDir </> file) then
            let dir = file
            if not(List.contains dir ["lib"; "bin"; "obj"]) then
                for file in dirFiles (projDir </> dir) do
                    moveJsFile projDir buildDir (dir </> file)
        else
            moveJsFile projDir buildDir file

let buildLibraryTs() =
    let projectDir = "src/fable-library"
    let buildDirTs = "build/fable-library-ts"
    let buildDirJs = "build/fable-library-js"
    cleanDirs [buildDirTs; buildDirJs]
    runFableWithArgs projectDir [
        "--define FX_NO_BIGINT"
        "--fable-library src/fable-library"
        "--typescript"
        "--extension .ts" // .fs.ts?
        "--exclude Fable.Core"
    ]
    // TODO: cleanDirs [buildDirTs </> "fable-library"]
    // TODO: copy *.ts/*.js from projectDir to buildDir
    runInDir buildDirTs "npx tsc --init --target es2020 --module es2020 --allowJs"
    runInDir buildDirTs ("npx tsc --outDir ../../" + buildDirJs)

let quicktest () =
    runFableWithArgs "src/quicktest" [
        "--force-pkgs"
        "--exclude Fable.Core"
    ]
    run "npx tsc src/quicktest/QuickTest.fs.js --allowJs -m commonJs --outDir build/quicktest"
    run "node build/quicktest/src/quicktest/QuickTest.fs.js"

let compileFcs() =
    runFableWithArgs "src/fable-standalone/src" [
        "--force-pkgs"
        "--typed-arrays"
        "--define FX_NO_CORHOST_SIGNER"
        "--define FX_NO_LINKEDRESOURCES"
        "--define FX_NO_PDB_READER"
        "--define FX_NO_PDB_WRITER"
        "--define FX_NO_WEAKTABLE"
        "--define FX_REDUCED_EXCEPTIONS"
        "--define NO_COMPILER_BACKEND"
        "--define NO_EXTENSIONTYPING"
        "--define NO_INLINE_IL_PARSER"
    ]

let buildStandalone() =
    let buildDir = "build/fable-standalone"
    let libraryDir = "build/fable-library"
    let projectDir = "src/fable-standalone/src"
    let distDir = "src/fable-standalone/dist"
    if pathExists libraryDir |> not then
        buildLibrary()

    // cleanup
    cleanDirs [buildDir; distDir]
    makeDirRecursive distDir

    // build
    compileFcs()
    runFableWithArgs (projectDir + "/Worker") [
        "--force-pkgs"
    ]

    // bundle
    run (sprintf "npx rollup %s/Main.fs.js --file %s/bundle.js --format umd --name __FABLE_STANDALONE__" projectDir buildDir)
    run (sprintf "npx rollup %s/Worker/Worker.fs.js --file %s/worker.js --format esm" projectDir buildDir)

    // minimize
    run (sprintf "npx terser %s/bundle.js -o %s/bundle.min.js --mangle --compress" buildDir distDir)
    // run (sprintf "npx terser %s/worker.js -o %s/worker.min.js --mangle --compress" buildDir distDir)
    run (sprintf "npx webpack --entry ./%s/worker.js --output ./%s/worker.min.js --config ./%s/../worker.config.js" buildDir distDir projectDir)

    // print bundle size
    fileSizeInBytes (distDir </> "bundle.min.js") / 1000 |> printfn "Bundle size: %iKB"
    fileSizeInBytes (distDir </> "worker.min.js") / 1000 |> printfn "Worker size: %iKB"

    // Put fable-library files next to bundle
    let libraryTarget = distDir </> "fable-library"
    copyDirRecursive libraryDir libraryTarget
    // These files will be used in the browser, so make sure the import paths include .js extension
    // let reg = Regex(@"^import (.*"".*)("".*)$", RegexOptions.Multiline)
    // getFullPathsInDirectoryRecursively libraryTarget
    // |> Array.filter (fun file -> file.EndsWith(".js"))
    // |> Array.iter (fun file ->
    //     reg.Replace(readFile file, fun m ->
    //         let fst = m.Groups.[1].Value
    //         if fst.EndsWith(".js") then m.Value
    //         else sprintf "import %s.js%s" fst m.Groups.[2].Value)
    //     |> writeFile file)

    // Bump version
    // let compilerVersion = Publish.loadReleaseVersion "src/fable-compiler"
    // let standaloneVersion = Publish.loadNpmVersion projectDir
    // let (comMajor, comMinor, _, comPrerelease) = Publish.splitVersion compilerVersion
    // let (staMajor, staMinor, staPatch, _) = Publish.splitVersion standaloneVersion
    // Publish.bumpNpmVersion projectDir
    //     (if comMajor > staMajor || comMinor > staMinor then compilerVersion
    //      else sprintf "%i.%i.%i%s" staMajor staMinor (staPatch + 1) comPrerelease)

let buildCompilerJs() =
    compileFcs()

    let projectDir = "src/fable-compiler-js"
    runFableWithArgs (projectDir </> "src") [
        "--force-pkgs"
        "--exclude Fable.Core"
    ]

    cleanDirs [projectDir </> "dist"]
    // run (sprintf "npx rollup %s/src/app.fs.js --file %s/dist/app.js --format umd --name Fable" projectDir projectDir)
    // run (sprintf "npx terser %s/dist/app.js -o %s/dist/app.min.js --mangle --compress" projectDir projectDir)

    // Compile to commonjs modules
    runTypescript "src/fable-compiler-js"
    // Copy fable-library
    copyDirRecursive ("build/fable-library") (projectDir </> "dist/fable-library")
    // Copy fable-metadata
    copyDirRecursive ("src/fable-metadata/lib") (projectDir </> "dist/fable-metadata")

let compileAndRunTests(compileTests) =
    runFableWithArgs "clean" ["tests"]
    compileTests()
    run "npx mocha tests/Main -r esm --reporter dot -t 10000"

let testJs() =
    buildCompilerJs()
    compileAndRunTests(fun () ->
        run "node src/fable-compiler-js tests/Main/Fable.Tests.fsproj")

    // let projectDir = "src/fable-compiler-js"
    // let buildDir = "build/tests-js"
    // if not (pathExists "build/fable-standalone") then
    //     buildStandalone()
    // if not (pathExists "build/fable-compiler-js") then
    //     buildCompilerJs()

    // cleanDirs [buildDir]

    // // Link fable-compiler-js to local packages
    // runInDir projectDir "npm link ../fable-metadata"
    // runInDir projectDir "npm link ../fable-standalone"

    // // Test fable-compiler-js locally
    // run ("node " + projectDir + " tests/Main/Fable.Tests.fsproj " + buildDir + " --commonjs")
    // run ("npx mocha " + buildDir + " --reporter dot -t 10000")
    // // and another test
    // runInDir "src/fable-compiler-js/test" "node .. test_script.fsx --commonjs"
    // runInDir "src/fable-compiler-js/test" "node bin/test_script.js"

    // // Unlink local packages after test
    // runInDir projectDir "npm unlink ../fable-metadata && cd ../fable-metadata && npm unlink"
    // runInDir projectDir "npm unlink ../fable-standalone && cd ../fable-standalone && npm unlink"

let test() =
    compileAndRunTests(fun () ->
        if pathExists "build/fable-library" |> not then
                buildLibrary()

        runFableWithArgs "tests/Main" [
            "--force-pkgs"
            "--exclude Fable.Core"
        ]
    )

    runInDir "tests/Main" "dotnet run"
    if envVarOrNone "APPVEYOR" |> Option.isSome then
        testJs()

let coverage() =
    // report converter
    // https://github.com/danielpalme/ReportGenerator
    // dotnet tool install dotnet-reportgenerator-globaltool --tool-path tools
    if not (pathExists "./bin/tools/reportgenerator") && not (pathExists "./bin/tools/reportgenerator.exe") then
        runInDir "." "dotnet tool install dotnet-reportgenerator-globaltool --tool-path bin/tools"
    let reportGen =
        if pathExists "./bin/tools/reportgenerator" then "bin/tools/reportgenerator"
        else "bin\\tools\\reportgenerator.exe"

    if pathExists "build/fable-library" |> not then
        buildLibrary()

    cleanDirs ["build/tests"]
    runFable "tests"

    // JS
    run "npx nyc mocha build/tests --require source-map-support/register --reporter dot -t 10000"
    runInDir "." (reportGen + " \"-reports:build/coverage/nyc/lcov.info\" -reporttypes:Html \"-targetdir:build/coverage/nyc/html\" ")

    // .NET
    //runInDir "tests/Main" "dotnet build /t:Collect_Coverage"
    cleanDirs ["build/coverage/netcoreapp2.0/out"]
    runInDir "." (reportGen + " \"-reports:build/coverage/netcoreapp2.0/coverage.xml\" -reporttypes:Html \"-targetdir:build/coverage/netcoreapp2.0/html\" ")

let downloadStandalone() =
    let targetDir = "src/fable-standalone/dist"
    cleanDirs [targetDir]
    downloadAndExtractTo APPVEYOR_REPL_ARTIFACT_URL targetDir

let githubRelease() =
    match envVarOrNone "GITHUB_USER", envVarOrNone "GITHUB_TOKEN" with
    | Some user, Some token ->
        async {
            try
                let ghreleases: GhRealeases = JsInterop.importAll "ghreleases"
                let! version, notes = Publish.loadReleaseVersionAndNotes "src/fable-compiler"
                run <| sprintf "git commit -am \"Release %s\" && git push" version
                let! res = ghreleases.create(user, token, "fable-compiler", "Fable", version, String.concat "\n" notes) |> Async.AwaitPromise
                printfn "Github release %s created successfully" version
            with ex ->
                printfn "Github release failed: %s" ex.Message
        } |> Async.StartImmediate
    | _ -> failwith "Expecting GITHUB_USER and GITHUB_TOKEN enviromental variables"

let syncFcsRepo() =
    // FAKE is giving lots of problems with the dotnet SDK version, ignore it
    let cheatWithDotnetSdkVersion dir f =
        let path = dir </> "build.fsx"
        let script = readFile path
        Regex.Replace(script, @"let dotnetExePath =[\s\S]*DotNetCli\.InstallDotNetSDK", "let dotnetExePath = \"dotnet\" //DotNetCli.InstallDotNetSDK") |> writeFile path
        f ()
        runInDir dir "git reset --hard"

    printfn "Expecting %s repo to be cloned at %s" FCS_REPO FCS_REPO_LOCAL

    // TODO: Prompt to reset --hard changes
    // service_slim
    runInDir FCS_REPO_LOCAL ("git checkout " + FCS_REPO_SERVICE_SLIM_BRANCH)
    runInDir FCS_REPO_LOCAL "git pull"
    cheatWithDotnetSdkVersion (FCS_REPO_LOCAL </> "fcs") (fun () ->
        runBashOrCmd (FCS_REPO_LOCAL </> "fcs") "build" "")
    copyFile (FCS_REPO_LOCAL </> "artifacts/bin/fcs/Release/netstandard2.0/FSharp.Compiler.Service.dll")  "../fable/lib/fcs/"
    copyFile (FCS_REPO_LOCAL </> "artifacts/bin/fcs/Release/netstandard2.0/FSharp.Compiler.Service.xml")  "../fable/lib/fcs/"

    // fcs-fable
    runInDir FCS_REPO_LOCAL ("git checkout " + FCS_REPO_FABLE_BRANCH)
    runInDir FCS_REPO_LOCAL "git pull"
    cheatWithDotnetSdkVersion (FCS_REPO_LOCAL </> "fcs") (fun () ->
        runBashOrCmd (FCS_REPO_LOCAL </> "fcs") "build" "CodeGen.Fable")
    copyDirRecursive (FCS_REPO_LOCAL </> "fcs/fcs-fable") "src/fcs-fable"
    copyDirNonRecursive (FCS_REPO_LOCAL </> "src/absil") "src/fcs-fable/src/absil"
    copyDirNonRecursive (FCS_REPO_LOCAL </> "src/fsharp") "src/fcs-fable/src/fsharp"
    copyDirNonRecursive (FCS_REPO_LOCAL </> "src/fsharp/service") "src/fcs-fable/src/fsharp/service"
    copyDirNonRecursive (FCS_REPO_LOCAL </> "src/fsharp/symbols") "src/fcs-fable/src/fsharp/symbols"
    copyDirNonRecursive (FCS_REPO_LOCAL </> "src/ilx") "src/fcs-fable/src/ilx"
    copyDirNonRecursive (FCS_REPO_LOCAL </> "src/utils") "src/fcs-fable/src/utils"
    removeFile "src/fcs-fable/.gitignore"
    let fcsFableProj = "src/fcs-fable/fcs-fable.fsproj"
    Regex.Replace(
            readFile fcsFableProj,
            @"(<FSharpSourcesRoot>\$\(MSBuildProjectDirectory\)).*?(<\/FSharpSourcesRoot>)",
            "$1/src$2")
    |> writeFile fcsFableProj

let packages =
    ["Fable.Cli", (fun () ->
        updateVersionInCliUtil()
        buildLibrary())
     "Fable.Core", doNothing
     "fable-compiler-js", buildCompilerJs
     "fable-metadata", doNothing
     "fable-publish-utils", doNothing
     "fable-standalone", downloadStandalone
    ]

let publishPackages restArgs =
    let packages =
        match List.tryHead restArgs with
        | Some pkg -> packages |> List.filter (fun (name,_) -> name = pkg)
        | None -> packages
    for (pkg, buildAction) in packages do
        if System.Char.IsUpper pkg.[0] then
            pushNuget ("src" </> pkg </> pkg + ".fsproj") buildAction
        else
            pushNpm ("src" </> pkg) buildAction

match argsLower with
| "test"::_ -> test()
| "test-js"::_ -> testJs()
| "coverage"::_ -> coverage()
| "quicktest"::_ -> quicktest()
// | "check-sourcemaps"::_ ->
//     ("src/quicktest/Quicktest.fs", "src/quicktest/bin/Quicktest.js", "src/quicktest/bin/Quicktest.js.map")
//     |||> sprintf "nodemon --watch src/quicktest/bin/Quicktest.js --exec 'source-map-visualization --sm=\"%s;%s;%s\"'"
//     |> List.singleton |> quicktest
| ("fable-library"|"library")::_ -> buildLibrary()
| ("fable-library-ts"|"library-ts")::_ -> buildLibraryTs()
| ("fable-compiler-js"|"compiler-js")::_ -> buildCompilerJs()
| ("fable-standalone"|"standalone")::_ -> buildStandalone()
| "fcs"::_ -> compileFcs()
| "download-standalone"::_ -> downloadStandalone()
| "publish"::restArgs -> publishPackages restArgs
| "github-release"::_ ->
    publishPackages []
    githubRelease ()
| "sync-fcs-repo"::_ -> syncFcsRepo()
| _ ->
    printfn "Please pass a target name"

printf "Build finished successfully"