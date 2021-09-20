#load "src/fable-publish-utils/PublishUtils.fs"

open PublishUtils
open System
open System.Text.RegularExpressions

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
    let cleanDirs dirs =
        for dir in dirs do
            removeDirRecursive dir

    let updateVersionInFableTransforms version =
        let filePath = "src/Fable.Transforms/Global/Compiler.fs"
        // printfn "VERSION %s" version
        Regex.Replace(
            readFile filePath,
            @"let \[<Literal>] VERSION = "".*?""",
            $"let [<Literal>] VERSION = \"{version}\"")
        |> writeFile filePath

    let updatePkgVersionInFsproj projFile version =
        readFile projFile
        |> replaceRegex Publish.NUGET_PACKAGE_VERSION (fun m ->
            m.Groups.[1].Value + version + m.Groups.[3].Value)
        |> writeFile projFile

    let runTSLint projectDir =
        run ("npm run tslint -- --project " + projectDir)

    let runTypeScript projectDir =
        run ("npm run tsc -- --project " + projectDir)

    let runFableWithArgs projectDir args =
        run ("dotnet run -c Release -p src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

    let runFableWithArgsAsync projectDir args =
        runAsync ("dotnet run -c Release -p src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

    let runNpx command args =
        run ("npx " + command + " " + (String.concat " " args))

    let runNpmScript script args =
        run ("npm run " + script + " -- " + (String.concat " " args))

    let runNpmScriptAsync script args =
        runAsync ("npm run " + script + " -- " + (String.concat " " args))

    let runFable projectDir =
        runFableWithArgs projectDir []

    let runMocha testDir =
        runNpmScript "mocha" [$"{testDir} -r esm --reporter dot -t 10000"]

    let resolveDir dir =
        __SOURCE_DIRECTORY__ </> dir

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
        //     buildLibraryJs()

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

let buildLibraryJsWithOptions (opts: {| watch: bool |}) =
    let baseDir = __SOURCE_DIRECTORY__

    let projectDir = baseDir </> "src/fable-library"
    let buildDir = baseDir </> "build/fable-library"
    let fableOpts = [
        "--outDir " + buildDir
        "--fableLib " + buildDir
        "--exclude Fable.Core"
        "--define FX_NO_BIGINT"
        "--define FABLE_LIBRARY"
        if opts.watch then "--watch"
    ]

    cleanDirs [buildDir]
    runInDir baseDir "npm install"
    makeDirRecursive buildDir
    copyFile (projectDir </> "package.json") buildDir

    if opts.watch then
        Async.Parallel [
            runNpmScriptAsync "tsc" [
                "--project " + projectDir
                "--watch"
            ]
            runFableWithArgsAsync projectDir fableOpts
        ] |> runAsyncWorkflow
    else
        runTSLint projectDir
        runTypeScript projectDir
        runFableWithArgs projectDir fableOpts

let buildLibraryJs() = buildLibraryJsWithOptions {| watch = false |}
let watchLibraryJs() = buildLibraryJsWithOptions {| watch = true |}

let buildLibraryJsIfNotExists() =
    let baseDir = __SOURCE_DIRECTORY__
    if not (pathExists (baseDir </> "build/fable-library")) then
        buildLibraryJs()

let buildLibraryTs() =
    let projectDir = "src/fable-library"
    let buildDirTs = "build/fable-library-ts"
    let buildDirJs = "build/fable-library-js"

    cleanDirs [buildDirTs; buildDirJs]

    runFableWithArgs projectDir [
        "--outDir " + buildDirTs
        "--fableLib " + buildDirTs
        "--lang TypeScript"
        "--exclude Fable.Core"
        "--define FX_NO_BIGINT"
        "--define FABLE_LIBRARY"
    ]
    // TODO: cleanDirs [buildDirTs </> "fable-library"]
    // TODO: copy *.ts/*.js from projectDir to buildDir
    runInDir buildDirTs "npm run tsc -- --init --target es2020 --module es2020 --allowJs"
    runInDir buildDirTs ("npm run tsc -- --outDir ../../" + buildDirJs)

let buildLibraryPy() =
    let libraryDir = "src/fable-library-py"
    let projectDir = libraryDir + "/fable"
    let buildDirPy = "build/fable-library-py"

    cleanDirs [buildDirPy]

    runFableWithArgs projectDir [
        "--outDir " + buildDirPy </> "fable"
        "--fableLib " + buildDirPy </> "fable"
        "--lang Python"
        "--exclude Fable.Core"
        "--define FABLE_LIBRARY"
    ]
    // Copy *.py from projectDir to buildDir
    copyDirRecursive libraryDir buildDirPy
    copyDirNonRecursive (buildDirPy </> "fable/fable-library") (buildDirPy </> "fable")
    //copyFile (buildDirPy </> "fable/fable-library/*.py") (buildDirPy </> "fable")
    copyFile (buildDirPy </> "fable/system.text.py") (buildDirPy </> "fable/system_text.py")
    copyFile (buildDirPy </> "fable/fsharp.core.py") (buildDirPy </> "fable/fsharp_core.py")
    copyFile (buildDirPy </> "fable/fsharp.collections.py") (buildDirPy </> "fable/fsharp_collections.py")
    copyFile (buildDirPy </> "fable/system.collections.generic.py") (buildDirPy </> "fable/system_collections_generic.py")
    removeFile (buildDirPy </> "fable/system.text.py")

    runInDir buildDirPy ("python3 --version")
    runInDir buildDirPy ("python3 ./setup.py develop")

let buildPyLibraryIfNotExists() =
    let baseDir = __SOURCE_DIRECTORY__
    if not (pathExists (baseDir </> "build/fable-library-py")) then
        buildLibraryPy()

let buildLibraryRust() =
    let libraryDir = "src/fable-library-rust"
    let projectDir = libraryDir + "/fable"
    let buildDir = "build/fable-library-rust"

    cleanDirs [buildDir]

    runFableWithArgs projectDir [
        "--outDir " + buildDir
        "--fableLib " + buildDir
        "--lang Rust"
        "--exclude Fable.Core"
        "--define FABLE_LIBRARY"
    ]
    // copy *.rs from libraryDir to buildDir
    copyDirNonRecursive libraryDir buildDir
    // runInDir buildDir ("cargo build")

let buildLibraryRustIfNotExists() =
    let baseDir = __SOURCE_DIRECTORY__
    if not (pathExists (baseDir </> "build/fable-library-rust")) then
        buildLibraryRust()

// Like testJs() but doesn't create bundles/packages for fable-standalone & friends
// Mainly intended for CI
let testJsFast() =
    runFableWithArgs "src/fable-standalone/src" [
        "--noCache"
    ]

    runFableWithArgs "src/fable-compiler-js/src" [
        "--exclude Fable.Core"
        "--define LOCAL_TEST"
    ]

    let fableJs = "./src/fable-compiler-js/src/app.fs.js"
    let testProj = "tests/Main/Fable.Tests.fsproj"
    let buildDir = "build/tests-js"
    run $"node --eval \"require('esm')(module)('{fableJs}')\" {fableJs} {testProj} {buildDir}"
    runMocha buildDir


let buildStandalone (opts: {| minify: bool; watch: bool |}) =
    buildLibraryJsIfNotExists()

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
        buildDir </> "bundle/Main.js"
        "-o " + rollupTarget
        "--format umd"
        "--name __FABLE_STANDALONE__"
    ]

    // cleanup
    if not opts.watch then
        cleanDirs [buildDir; distDir]
        makeDirRecursive distDir

    // build standalone bundle
    runFableWithArgs projectDir [
        "--outDir " + buildDir </> "bundle"
        if opts.watch then
            "--watch"
            "--run rollup"
            yield! rollupArgs
            "--watch"
    ]

    // build standalone worker
    runFableWithArgs (projectDir + "/Worker") [
        "--outDir " + buildDir + "/worker"
    ]

    // make standalone bundle dist
    runNpmScript "rollup" rollupArgs
    if opts.minify then
        runNpmScript "terser" [
            buildDir </> "bundle.js"
            "-o " + distDir </> "bundle.min.js"
            "--mangle"
            "--compress"
        ]

    // make standalone worker dist
    runNpmScript "rollup" [$"{buildDir}/worker/Worker.js -o {buildDir}/worker.js --format iife"]
    // runNpx "webpack" [sprintf "--entry ./%s/worker.js --output ./%s/worker.min.js --config ./%s/../worker.config.js" buildDir distDir projectDir]
    runNpmScript "terser" [$"{buildDir}/worker.js -o {distDir}/worker.min.js --mangle --compress"]

    // print bundle size
    fileSizeInBytes (distDir </> "bundle.min.js") / 1000. |> printfn "Bundle size: %fKB"
    fileSizeInBytes (distDir </> "worker.min.js") / 1000. |> printfn "Worker size: %fKB"

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
    run $"npx rollup {buildDir}/app.js -o {rollupTarget} --format umd --name Fable"
    if minify then
        run $"npx terser {distDir}/app.js -o {distDir}/app.min.js --mangle --compress"

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
    run $"node {fableDir} tests/Main/Fable.Tests.fsproj {buildDir}"
    runMocha buildDir

    // // Another local fable-compiler-js test
    // runInDir (fableDir </> "test") "node .. test_script.fsx"
    // runInDir (fableDir </> "test") "node test_script.fsx.js"

    // Unlink local packages after test
    runInDir fableDir "npm unlink ../fable-metadata && cd ../fable-metadata && npm unlink"
    runInDir fableDir "npm unlink ../fable-standalone && cd ../fable-standalone && npm unlink"

let testReact() =
    runFableWithArgs "tests/React" []
    runInDir "tests/React" "npm i && npm test"


let testCompiler() =
    runInDir "tests/Compiler" "dotnet run -c Release"

let testIntegration() =
    runInDir "tests/Integration" "dotnet run -c Release"

let testMocha() =
    let projectDir = "tests/Main"
    let buildDir = "build/tests"

    cleanDirs [buildDir]
    runFableWithArgs projectDir [
        "--outDir " + buildDir
        "--exclude Fable.Core"
    ]

    runMocha buildDir

let testDefineConstants() =
    [ "tests/DefineConstants/DebugWithExtraDefines", "Debug"
      "tests/DefineConstants/CustomConfiguration", "Test"
      "tests/DefineConstants/ReleaseNoExtraDefines", String.Empty ]
    |> List.iter (fun (projectDir, configuration) ->
        let buildDir = "build/"+ projectDir

        cleanDirs [ buildDir ]
        runFableWithArgs projectDir [
            "--outDir " + buildDir
            "--exclude Fable.Core"
            if not(String.IsNullOrEmpty configuration) then
                "--configuration " + configuration
        ]

        runMocha buildDir
    )

let test() =
    buildLibraryJsIfNotExists()

    testDefineConstants()

    testMocha()

    runInDir "tests/Main" "dotnet run"

    // testReact()

    testCompiler()

    testIntegration()

    if envVarOrNone "APPVEYOR" |> Option.isSome then
        testJsFast()

let testPython() =
    buildPyLibraryIfNotExists() // NOTE: fable-library-py needs to be built separately.

    let projectDir = "tests/Python"
    let buildDir = "build/tests/Python"

    cleanDirs [buildDir]
    runInDir projectDir "dotnet test"
    runFableWithArgs projectDir [
        "--outDir " + buildDir
        "--exclude Fable.Core"
        "--lang Python"
    ]

    runInDir buildDir "touch __init__.py" // So relative imports works.
    runInDir buildDir "pytest"

let testRust() =
    buildLibraryRustIfNotExists()

    let projectDir = "tests/Rust"
    let buildDir = "build/tests/Rust"

    cleanDirs [buildDir]
    runInDir projectDir "dotnet test"
    runFableWithArgs projectDir [
        "--outDir " + buildDir
        "--exclude Fable.Core"
        "--lang Rust"
        "--fableLib " + "fable-library-rust"
    ]

    copyFile (projectDir </> "Cargo.toml") buildDir
    runInDir buildDir "cargo test"

let buildLocalPackageWith pkgDir pkgCommand fsproj action =
    let version = "3.0.0-local-build-" + DateTime.Now.ToString("yyyyMMdd-HHmm")
    action version
    updatePkgVersionInFsproj fsproj version
    run $"dotnet pack {fsproj} -p:Pack=true -c Release -o {pkgDir}"

    // Return install command
    $"""dotnet {pkgCommand} --version "{version}" --add-source {fullPath pkgDir}"""

let buildLocalPackage pkgDir =
    buildLocalPackageWith pkgDir
        "tool install fable"
        (resolveDir "src/Fable.Cli/Fable.Cli.fsproj") (fun version ->
            buildLibraryJs()
            updateVersionInFableTransforms version)

let testRepos() =
    let repos = [
        "https://github.com/alfonsogarciacaro/FsToolkit.ErrorHandling:update-fable-3", "npm i && npm test"
        "https://github.com/fable-compiler/fable-promise:master", "npm i && npm test"
        "https://github.com/alfonsogarciacaro/Thoth.Json:nagareyama", "dotnet paket restore && npm i && dotnet fable tests -o tests/bin --run mocha -r esm tests/bin"
        "https://github.com/alfonsogarciacaro/FSharp.Control.AsyncSeq:nagareyama", "cd tests/fable && npm i && npm test"
        "https://github.com/alfonsogarciacaro/Fable.Extras:nagareyama", "dotnet paket restore && npm i && npm test"
        "https://github.com/alfonsogarciacaro/Fable.Jester:nagareyama", "npm i && npm test"
        "https://github.com/Zaid-Ajaj/Fable.SimpleJson:master", "npm i && npm run test-nagareyama"
    ]

    let testDir = tempPath() </> "fable-repos"
    printfn $"Cloning repos to: {testDir}"

    cleanDirs [testDir]
    makeDirRecursive testDir
    let pkgInstallCmd = buildLocalPackage (testDir </> "pkg")

    for (repo, command) in repos do
        let url, branch = let i = repo.LastIndexOf(":") in repo.[..i-1], repo.[i+1..]
        let name = url.[url.LastIndexOf("/") + 1..]
        runInDir testDir $"git clone {url} {name}"
        let repoDir = testDir </> name
        runInDir repoDir ("git checkout " + branch)
        runInDir repoDir "dotnet tool uninstall fable"
        runInDir repoDir pkgInstallCmd
        runInDir repoDir "dotnet tool restore"
        runInDir repoDir command

let githubRelease() =
    match envVarOrNone "GITHUB_USER", envVarOrNone "GITHUB_TOKEN" with
    | Some user, Some token ->
        async {
            try
                let! version, notes = Publish.loadReleaseVersionAndNotes "src/Fable.Cli"
                let notes = notes |> Array.map (fun n -> $"""'{n.Replace("'", @"\'").Replace("`", @"\`")}'""") |> String.concat ","
                run $"git commit -am \"Release {version}\" && git push"
                runSilent $"""node --eval "require('ghreleases').create({{ user: '{user}', token: '{token}', }}, 'fable-compiler', 'Fable', {{ tag_name: '{version}', name: '{version}', body: [{notes}].join('\n'), }}, (err, res) => {{ if (err != null) {{ console.error(err) }} }})" """
                printfn "Github release %s created successfully" version
            with ex ->
                printfn "Github release failed: %s" ex.Message
        } |> runAsyncWorkflow
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
        buildLibraryJs())
     "fable-metadata", doNothing
     "fable-publish-utils", doNothing
     "fable-standalone", fun () -> buildStandalone {|minify=true; watch=false|}
     "fable-compiler-js", fun () -> buildCompilerJs true
    ]

let publishPackages restArgs =
    let packages =
        match List.tryHead restArgs with
        | Some pkg -> packages |> List.filter (fun (name,_) -> name = pkg)
        | None -> packages
    async {
        for (pkg, buildAction) in packages do
            if System.Char.IsUpper pkg.[0] then
                let projFile = "src" </> pkg </> pkg + ".fsproj"
                do! pushFableNuget projFile ["Pack", "true"] buildAction
            else
                pushNpm ("src" </> pkg) buildAction
    } |> runAsyncWorkflow

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
| "test-mocha"::_ -> testMocha()
| "test-define-constants"::_ -> testDefineConstants()
| "test-js"::_ -> testJs(minify)
| "test-js-fast"::_ -> testJsFast()
| "test-react"::_ -> testReact()
| "test-compiler"::_ -> testCompiler()
| "test-integration"::_ -> testIntegration()
| "test-py"::_ -> testPython()
| "test-rust"::_ -> testRust()
| "quicktest"::_ ->
    buildLibraryJsIfNotExists()
    run "dotnet watch -p src/Fable.Cli run -- watch --cwd ../quicktest --exclude Fable.Core --noCache --runScript"
| "quicktest-py"::_ ->
    buildPyLibraryIfNotExists()
    run "dotnet watch -p src/Fable.Cli run -- watch --cwd ../quicktest --lang Python --exclude Fable.Core --noCache"
| "run"::_ ->
    buildLibraryJsIfNotExists()
    // Don't take it from pattern matching as that one uses lowered args
    let restArgs = args |> List.skip 1 |> String.concat " "
    run $"""dotnet run -c Release -p {resolveDir "src/Fable.Cli"} -- {restArgs}"""

| "package"::_ ->
    let pkgInstallCmd = buildLocalPackage (resolveDir "temp/pkg")
    printfn $"\nPackage has been created, use the following command to install it:\n    {pkgInstallCmd}\n"

| "package-core"::_ ->
    let pkgInstallCmd = buildLocalPackageWith (resolveDir "temp/pkg") "add package Fable.Core" (resolveDir "src/Fable.Core/Fable.Core.fsproj") ignore
    printfn $"\nFable.Core package has been created, use the following command to install it:\n    {pkgInstallCmd}\n"

| ("watch-library")::_ -> watchLibraryJs()
| ("fable-library"|"library")::_ -> buildLibraryJs()
| ("fable-library-ts"|"library-ts")::_ -> buildLibraryTs()
| ("fable-library-py"|"library-py")::_ -> buildLibraryPy()
| ("fable-library-rust" | "library-rust")::_ -> buildLibraryRust()
| ("fable-compiler-js"|"compiler-js")::_ -> buildCompilerJs(minify)
| ("fable-standalone"|"standalone")::_ -> buildStandalone {|minify=minify; watch=false|}
| "watch-standalone"::_ -> buildStandalone {|minify=false; watch=true|}
| "publish"::restArgs -> publishPackages restArgs
| "github-release"::_ ->
    publishPackages []
    githubRelease ()
| "sync-fcs-repo"::_ -> syncFcsRepo()
| "copy-fcs-repo"::_ -> copyFcsRepo FCS_REPO_LOCAL
| "test-repos"::_ -> testRepos()
| _ ->
    printfn """Please pass a target name. Examples:

- Use `test` to run tests:
    dotnet fsi build.fsx test

- Use `package` to build a local package:
    dotnet fsi build.fsx package

- Use `run` to compile a project with development version:
    dotnet fsi build.fsx run ../path/to/my/project [Fable options]

- Use `quicktest` to quickly test development version with src/quicktest project:
    dotnet fsi build.fsx quicktest
"""

printfn "Build finished successfully"
