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

module Util =
    type GhRealeases =
        [<Emit("""new Promise((succeed, fail) =>
            $0.create({user: $1, token: $2}, $3, $4, { tag_name: $5, name: $5, body: $6 }, (err, res) =>
                err != null ? fail(err) : succeed(res)))""")>]
        abstract create: user: string * token: string * owner: string * repo: string * name: string * msg: string -> JS.Promise<obj>

    let cleanDirs dirs =
        for dir in dirs do
            removeDirRecursive dir

    let updateVersionInFableTransforms version =
        let filePath = "src/Fable.Transforms/Global/Compiler.fs"
        // printfn "VERSION %s" version
        Regex.Replace(
            readFile filePath,
            @"let \[<Literal>] VERSION = "".*?""",
            sprintf "let [<Literal>] VERSION = \"%s\"" version)
        |> writeFile filePath

    let updatePkgVersionInFsproj projFile version =
        readFile projFile
        |> replaceRegex Publish.NUGET_PACKAGE_VERSION ["$1"; version; "$3"]
        |> writeFile projFile

    let runTypescript projectDir =
        // run ("npx tslint --project " + projectDir)
        run ("npm run tsc -- --project " + projectDir)

    let runFableWithArgs projectDir args =
        run ("dotnet run -c Release -p src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

    let runFableWithArgsAsync projectDir args =
        runAsync ("dotnet run -c Release -p src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

    let runNpx command args =
        run ("npx " + command + " " + (String.concat " " args))

    let runNpmScriptAsync script args =
        runAsync ("npm run " + script + " -- " + (String.concat " " args))

    let runFable projectDir =
        runFableWithArgs projectDir []

open Util

module Unused =
    // type Chokidar =
    //     abstract watch: path: string -> Chokidar
    //     abstract on: event: string * (string -> unit) -> Chokidar

    // let chokidar: Chokidar = importDefault "chokidar"

    // let concurrently(commands: string[]): unit = importDefault "concurrently"

    let downloadAndExtractTo (url: string) (targetDir: string) =
        sprintf "npx download --extract --out %s \"%s\"" targetDir url |> run

    let downloadStandalone() =
        let targetDir = "src/fable-standalone/dist"
        cleanDirs [targetDir]
        downloadAndExtractTo APPVEYOR_REPL_ARTIFACT_URL targetDir

    let coverage() =
        // report converter
        // https://github.com/danielpalme/ReportGenerator
        // dotnet tool install dotnet-reportgenerator-globaltool --tool-path tools
        if not (pathExists "./bin/tools/reportgenerator") && not (pathExists "./bin/tools/reportgenerator.exe") then
            runInDir "." "dotnet tool install dotnet-reportgenerator-globaltool --tool-path bin/tools"
        let reportGen =
            if pathExists "./bin/tools/reportgenerator" then "bin/tools/reportgenerator"
            else "bin\\tools\\reportgenerator.exe"

        // if not (pathExists "build/fable-library") then
        //     buildLibrary()

        cleanDirs ["build/tests"]
        runFable "tests"

        // JS
        run "npx nyc mocha build/tests --require source-map-support/register --reporter dot -t 10000"
        runInDir "." (reportGen + " \"-reports:build/coverage/nyc/lcov.info\" -reporttypes:Html \"-targetdir:build/coverage/nyc/html\" ")

        // .NET
        //runInDir "tests/Main" "dotnet build /t:Collect_Coverage"
        cleanDirs ["build/coverage/netcoreapp2.0/out"]
        runInDir "." (reportGen + " \"-reports:build/coverage/netcoreapp2.0/coverage.xml\" -reporttypes:Html \"-targetdir:build/coverage/netcoreapp2.0/html\" ")

// TARGETS ---------------------------

let buildLibraryWithOptions (opts: {| watch: bool |}) =
    let projectDir = fullPath "src/fable-library"
    let buildDir = fullPath "build/fable-library"
    let fableOpts = [
        "--outDir " + buildDir
        "--fableLib " + buildDir
        "--exclude Fable.Core"
        "--define FX_NO_BIGINT"
        "--define FABLE_LIBRARY"
        if opts.watch then "--watch"
    ]

    cleanDirs [buildDir]
    if opts.watch then
        runNpmScriptAsync "tsc" [
            "--project " + projectDir
            "--watch"
        ]
        runFableWithArgsAsync projectDir fableOpts
    else
        runTypescript projectDir
        runFableWithArgs projectDir fableOpts

let buildLibrary() = buildLibraryWithOptions {| watch = false |}
let watchLibrary() = buildLibraryWithOptions {| watch = true |}
    // let libDir = "src/fable-library"
    // printfn "Watching %s..." libDir
    // chokidar
    //     .watch(libDir)
    //     .on("change", fun _ -> buildLibrary())
    //     |> ignore

let buildLibraryTs() =
    let projectDir = "src/fable-library"
    let buildDirTs = "build/fable-library-ts"
    let buildDirJs = "build/fable-library-js"

    cleanDirs [buildDirTs; buildDirJs]

    runFableWithArgs projectDir [
        "--outDir " + buildDirTs
        "--fableLib " + buildDirTs
        "--typescript"
        "--exclude Fable.Core"
        "--define FX_NO_BIGINT"
        "--define FABLE_LIBRARY"
    ]
    // TODO: cleanDirs [buildDirTs </> "fable-library"]
    // TODO: copy *.ts/*.js from projectDir to buildDir
    runInDir buildDirTs "npm run tsc -- --init --target es2020 --module es2020 --allowJs"
    runInDir buildDirTs ("npm run tsc -- --outDir ../../" + buildDirJs)

let quicktest () =
    if not (pathExists "build/fable-library") then
        buildLibrary()

    // Nodemon will terminate if file doesn't exist
    let quicktestJsPath = "src/quicktest/Quicktest.fs.js"
    if pathExists quicktestJsPath |> not then
        writeFile quicktestJsPath "console.log('Getting ready, hold tight')"

    run "dotnet watch -p src/Fable.Cli run -- watch --cwd ../quicktest --exclude Fable.Core --forcePkgs --runScript"

// Like testJs() but doesn't create bundles/packages for fable-standalone & friends
// Mainly intended for CI
let testJsFast() =
    runFableWithArgs "src/fable-standalone/src" [
        "--forcePkgs"
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

    runFableWithArgs "src/fable-compiler-js/src" [
        "--exclude Fable.Core"
        "--define LOCAL_TEST"
    ]

    let fableJs = "./src/fable-compiler-js/src/app.fs.js"
    let testProj = "tests/Main/Fable.Tests.fsproj"
    let buildDir = "build/tests-js"
    run (sprintf "node --eval \"require('esm')(module)('%s')\" %s %s %s" fableJs fableJs testProj buildDir)
    run ("npx mocha " + buildDir + " -r esm --reporter dot -t 10000")


let buildStandalone (opts: {| minify: bool; watch: bool |}) =
    printfn "Building standalone%s..." (if opts.minify then "" else " (no minification)")

    let projectDir = "src/fable-standalone/src"
    let libraryDir = "build/fable-library"
    let buildDir = "build/fable-standalone"
    let distDir = "src/fable-standalone/dist"

    let rollupTarget =
        match opts.watch, opts.minify with
        | true, _ ->
            match args with
            | _::rollupTarget::_ -> rollupTarget
            | _ -> failwith "Pass the bundle output, e.g.: npm run build watch-standalone ../repl3/public/js/repl/bundle.min.js"
        | false, true -> buildDir </> "bundle.js"
        | false, false -> distDir </> "bundle.min.js"

    let rollupArgs = [
        buildDir + "/bundle/Main.js"
        "-o " + rollupTarget
        "--format umd"
        "--name __FABLE_STANDALONE__"
    ]

    if not (pathExists libraryDir) then
        buildLibrary()

    // cleanup
    if not opts.watch then
        cleanDirs [buildDir; distDir]
        makeDirRecursive distDir

    // build standalone bundle
    runFableWithArgs projectDir [
        "--outDir " + buildDir + "/bundle"
        "--define FX_NO_CORHOST_SIGNER"
        "--define FX_NO_LINKEDRESOURCES"
        "--define FX_NO_PDB_READER"
        "--define FX_NO_PDB_WRITER"
        "--define FX_NO_WEAKTABLE"
        "--define FX_REDUCED_EXCEPTIONS"
        "--define NO_COMPILER_BACKEND"
        "--define NO_EXTENSIONTYPING"
        "--define NO_INLINE_IL_PARSER"
        if opts.watch then
            "--watch"
            "--run rollup"
            yield! rollupArgs
            "--watch"
    ]

    // make standalone bundle dist
    runNpx "rollup" rollupArgs
    if opts.minify then
        runNpx "terser" [
            buildDir + "/bundle.js"
            sprintf "-o %s/bundle.min.js" distDir
            "--mangle"
            "--compress"
        ]

    // build standalone worker
    runFableWithArgs (projectDir + "/Worker") [
        "--outDir " + buildDir + "/worker"
    ]

    // make standalone worker dist
    runNpx "rollup" [sprintf "%s/worker/Worker.js -o %s/worker.js --format esm" buildDir buildDir]
    // runNpx "webpack" [sprintf "--entry ./%s/worker.js --output ./%s/worker.min.js --config ./%s/../worker.config.js" buildDir distDir projectDir]
    runNpx "terser" [sprintf "%s/worker.js -o %s/worker.min.js --mangle --compress" buildDir distDir]

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

let buildCompilerJs(minify: bool) =
    let projectDir = "src/fable-compiler-js/src"
    let buildDir = "build/fable-compiler-js"
    let distDir = "src/fable-compiler-js/dist"

    if not (pathExists "build/fable-standalone") then
        buildStandalone {|minify=minify; watch=false|}

    cleanDirs [buildDir; distDir]
    makeDirRecursive distDir

    runFableWithArgs projectDir [
        "--outDir " + buildDir
        "--exclude Fable.Core"
    ]

    let rollupTarget = if minify then distDir </> "app.js" else distDir </> "app.min.js"
    run (sprintf "npx rollup %s/app.js -o %s --format umd --name Fable" buildDir rollupTarget)
    if minify then
        run (sprintf "npx terser %s/app.js -o %s/app.min.js --mangle --compress" distDir distDir)

    // Copy fable-library
    copyDirRecursive ("build/fable-library") (distDir </> "fable-library")
    // Copy fable-metadata
    copyDirRecursive ("src/fable-metadata/lib") (distDir </> "fable-metadata")

let testJs(minify) =
    let fableDir = "src/fable-compiler-js"
    let buildDir = "build/tests-js"

    if not (pathExists "build/fable-compiler-js") then
        buildCompilerJs(minify)

    cleanDirs [buildDir]

    // Link fable-compiler-js to local packages
    runInDir fableDir "npm link ../fable-metadata"
    runInDir fableDir "npm link ../fable-standalone"

    // Test fable-compiler-js locally
    run ("node " + fableDir + " tests/Main/Fable.Tests.fsproj " + buildDir)
    run ("npx mocha " + buildDir + " -r esm --reporter dot -t 10000")

    // // Another local fable-compiler-js test
    // runInDir (fableDir </> "test") "node .. test_script.fsx"
    // runInDir (fableDir </> "test") "node test_script.fsx.js"

    // Unlink local packages after test
    runInDir fableDir "npm unlink ../fable-metadata && cd ../fable-metadata && npm unlink"
    runInDir fableDir "npm unlink ../fable-standalone && cd ../fable-standalone && npm unlink"

let testReact() =
    runFableWithArgs "tests/React" []
    runInDir "tests/React" "npm i && npm test"

let test() =
    let projectDir = "tests/Main"
    let libraryDir = "build/fable-library"
    let buildDir = "build/tests"

    if not (pathExists libraryDir) then
        buildLibrary()

    cleanDirs [buildDir]
    runFableWithArgs projectDir [
        "--outDir " + buildDir
        "--exclude Fable.Core"
    ]

    run (sprintf "npx mocha %s -r esm --reporter dot -t 10000" buildDir)

    runInDir projectDir "dotnet run"

    testReact()

    if envVarOrNone "APPVEYOR" |> Option.isSome then
        testJsFast()

let testRepos() =
    let repos = [
        "https://github.com/fable-compiler/fable-promise:master", "npm i && npm test"
        "https://github.com/alfonsogarciacaro/Thoth.Json:nagareyama", "dotnet paket restore  && npm i && dotnet fable tests -o tests/bin && npx mocha -r esm tests/bin"
        "https://github.com/alfonsogarciacaro/FSharp.Control.AsyncSeq:nagareyama", "cd tests/fable && npm i && npm test"
        "https://github.com/alfonsogarciacaro/Fable.Extras:nagareyama", "dotnet paket restore && npm i && npm test"
        "https://github.com/alfonsogarciacaro/Fable.Jester:nagareyama", "npm i && npm test"
        "https://github.com/Zaid-Ajaj/Fable.SimpleJson:master", "npm i && npm run test-nagareyama"
    ]

    let version = "3.0.0-local-build-" + DateTime.Now.ToString("yyyyMMdd-HHmm")
    let testDir = "temp"
    let pkgDir = "pkg"

    cleanDirs [testDir]
    makeDirRecursive testDir
    updateVersionInFableTransforms version
    updatePkgVersionInFsproj "src/Fable.Cli/Fable.Cli.fsproj" version
    run (sprintf "dotnet pack src/Fable.Cli/ -p:Pack=true -c Release -o %s" (testDir </> pkgDir))

    for (repo, command) in repos do
        let url, branch = let i = repo.LastIndexOf(":") in repo.[..i-1], repo.[i+1..]
        let name = url.[url.LastIndexOf("/") + 1..]
        runInDir testDir (sprintf "git clone %s %s" url name)
        let repoDir = testDir </> name
        runInDir repoDir ("git checkout " + branch)
        runInDir repoDir "dotnet tool uninstall fable"
        runInDir repoDir (sprintf "dotnet tool install fable --version \"%s\" --add-source %s"
                            version (".." </> pkgDir))
        runInDir repoDir "dotnet tool restore"
        runInDir repoDir command

let githubRelease() =
    match envVarOrNone "GITHUB_USER", envVarOrNone "GITHUB_TOKEN" with
    | Some user, Some token ->
        async {
            try
                let ghreleases: GhRealeases = JsInterop.importAll "ghreleases"
                let! version, notes = Publish.loadReleaseVersionAndNotes "src/Fable.Cli"
                run <| sprintf "git commit -am \"Release %s\" && git push" version
                let! res = ghreleases.create(user, token, "fable-compiler", "Fable", version, String.concat "\n" notes) |> Async.AwaitPromise
                printfn "Github release %s created successfully" version
            with ex ->
                printfn "Github release failed: %s" ex.Message
        } |> Async.StartImmediate
    | _ -> failwith "Expecting GITHUB_USER and GITHUB_TOKEN enviromental variables"

let copyFcsRepo sourceDir =
    let targetDir = "src/fcs-fable"
    cleanDirs [targetDir]
    copyDirRecursive (sourceDir </> "fcs/fcs-fable") targetDir
    [ "src/fsharp"
    ; "src/fsharp/absil"
    ; "src/fsharp/ilx"
    ; "src/fsharp/service"
    ; "src/fsharp/symbols"
    ; "src/fsharp/utils"
    ] |> List.iter (fun path ->
        copyDirNonRecursive (sourceDir </> path) (targetDir </> path))
    removeFile (targetDir </> ".gitignore")
    let projPath = (targetDir </> "fcs-fable.fsproj")
    let projText = readFile projPath
    let projText =
        Regex.Replace(projText,
            @"(<FSharpSourcesRoot>\$\(MSBuildProjectDirectory\)).*?(<\/FSharpSourcesRoot>)",
            "$1/src$2")
    // let projText =
    //     Regex.Replace(projText,
    //         @"artifacts\/bin\/FSharp.Core\/Release\/netstandard2.0",
    //         "lib/fcs")
    projText |> writeFile projPath

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
    copyFile (FCS_REPO_LOCAL </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Compiler.Service.dll")  "../fable/lib/fcs/"
    copyFile (FCS_REPO_LOCAL </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Compiler.Service.xml")  "../fable/lib/fcs/"
    copyFile (FCS_REPO_LOCAL </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Core.dll")  "../fable/lib/fcs/"
    copyFile (FCS_REPO_LOCAL </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Core.xml")  "../fable/lib/fcs/"

    // fcs-fable
    runInDir FCS_REPO_LOCAL ("git checkout " + FCS_REPO_FABLE_BRANCH)
    runInDir FCS_REPO_LOCAL "git pull"
    cheatWithDotnetSdkVersion (FCS_REPO_LOCAL </> "fcs") (fun () ->
        runBashOrCmd (FCS_REPO_LOCAL </> "fcs") "build" "CodeGen.Fable")
    copyFcsRepo FCS_REPO_LOCAL

let packages =
    ["Fable.AST", doNothing
     "Fable.Core", doNothing
     "Fable.Cli", (fun () ->
        Publish.loadReleaseVersion "src/Fable.Cli" |> updateVersionInFableTransforms
        buildLibrary())
     "fable-compiler-js", fun () -> buildCompilerJs true
     "fable-metadata", doNothing
     "fable-publish-utils", doNothing
     "fable-standalone", fun () -> buildStandalone {|minify=true; watch=false|}
    ]

let publishPackages restArgs =
    let packages =
        match List.tryHead restArgs with
        | Some pkg -> packages |> List.filter (fun (name,_) -> name = pkg)
        | None -> packages
    for (pkg, buildAction) in packages do
        if System.Char.IsUpper pkg.[0] then
            pushNuget ("src" </> pkg </> pkg + ".fsproj") ["Pack", "true"] buildAction
        else
            pushNpm ("src" </> pkg) buildAction

let minify<'T> =
    argsLower |> List.contains "--no-minify" |> not

match argsLower with
// | "check-sourcemaps"::_ ->
//     ("src/quicktest/Quicktest.fs", "src/quicktest/bin/Quicktest.js", "src/quicktest/bin/Quicktest.js.map")
//     |||> sprintf "nodemon --watch src/quicktest/bin/Quicktest.js --exec 'source-map-visualization --sm=\"%s;%s;%s\"'"
//     |> List.singleton |> quicktest
// | "download-standalone"::_ -> downloadStandalone()
// | "coverage"::_ -> coverage()
| "test"::_ -> test()
| "test-js"::_ -> testJs(minify)
| "test-js-fast"::_ -> testJsFast()
| "test-react"::_ -> testReact()
| "quicktest"::_ -> quicktest()
| ("watch-library")::_ -> watchLibrary()
| ("fable-library"|"library")::_ -> buildLibrary()
| ("fable-library-ts"|"library-ts")::_ -> buildLibraryTs()
| ("fable-compiler-js"|"compiler-js")::_ -> buildCompilerJs(minify)
| ("fable-standalone"|"standalone")::_ -> buildStandalone {|minify=minify; watch=false|}
| "watch-standalone"::_ -> buildStandalone {|minify=false; watch=true|}
| "publish"::restArgs -> publishPackages restArgs
| "github-release"::_ ->
    publishPackages []
    githubRelease ()
| "sync-fcs-repo"::_ -> syncFcsRepo()
| "copy-fcs-repo"::_ -> copyFcsRepo "../fsharp"
| "test-repos"::_ -> testRepos()
| _ ->
    printfn "Please pass a target name"

printf "Build finished successfully"