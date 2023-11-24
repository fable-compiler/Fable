#load "src/Fable.PublishUtils/PublishUtils.fs"

open System
open System.Text.RegularExpressions
open PublishUtils

// ncave FCS fork
let FCS_REPO = "https://github.com/ncave/fsharp"
let FCS_REPO_LOCAL = "../fsharp"
let FCS_REPO_FABLE_BRANCH = "fable"
let FCS_REPO_SERVICE_SLIM_BRANCH = "service_slim"

let BUILD_ARGS = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray

let BUILD_ARGS_LOWER = BUILD_ARGS |> List.map (fun x -> x.ToLower())

module Util =
    let cleanDirs dirs =
        for dir in dirs do
            printfn $"Clean {dir}"
            removeDirRecursive dir

    // TODO: move to PublishUtils.fs ?
    let copyFiles sourceDir searchPattern destDir =
        printfn $"Copy {sourceDir </> searchPattern} to {destDir}"

        for source in IO.Directory.GetFiles(sourceDir, searchPattern) do
            let fileName = IO.Path.GetFileName(source)
            let target = destDir </> fileName
            IO.File.Copy(source, target, true)

    let resolveDir dir = __SOURCE_DIRECTORY__ </> dir

    let updateVersionsInFableTransforms
        compilerVersion
        (libraryVersions: (string * string) list)
        =
        let mutable updated = Set.empty

        let replaceVersion (lang: string option) version fileContent =
            let prefix =
                match lang with
                | None -> ""
                | Some lang -> $"{lang.ToUpperInvariant()}_LIBRARY_"

            Regex.Replace(
                fileContent,
                $@"^(\s*)let \[<Literal>] {prefix}VERSION = ""(.*?)""",
                (fun (m: Match) ->
                    match lang with
                    | Some lang when m.Groups[2].Value <> version ->
                        updated <- Set.add lang updated
                    | _ -> ()

                    m.Groups[1].Value
                    + $"let [<Literal>] {prefix}VERSION = \"{version}\""
                ),
                RegexOptions.Multiline
            )

        let filePath = "src/Fable.Transforms/Global/Compiler.fs"

        readFile filePath
        |> replaceVersion None compilerVersion
        |> List.foldBack
            (fun (lang, version) fileContent ->
                replaceVersion (Some lang) version fileContent
            )
            libraryVersions
        |> writeFile filePath

        updated

    let updatePkgVersionInFsproj projFile version =
        readFile projFile
        |> replaceRegex
            Publish.NUGET_PACKAGE_VERSION
            (fun m -> m.Groups[1].Value + version + m.Groups[3].Value)
        |> writeFile projFile

    let runTSLint projectDir =
        run ("npm run tslint -- --project " + projectDir)

    let runTypeScript projectDir =
        run ("npm run tsc -- --project " + projectDir)

    let runTypeScriptWithArgs projectDir args =
        run (
            "npm run tsc -- --project "
            + projectDir
            + " "
            + String.concat " " args
        )

    let runFableWithArgs projectDir args =
        run (
            "dotnet run -c Release --project src/Fable.Cli -- "
            + projectDir
            + " "
            + String.concat " " args
        )

    let watchFableWithArgs projectDir args =
        run (
            "dotnet watch --project src/Fable.Cli run -- "
            + projectDir
            + " --cwd ../.. "
            + String.concat " " args
        )

    let runFableWithArgsInDirAs release projectDir args =
        let cliDir = resolveDir "src/Fable.Cli"
        let cliArgs = args |> String.concat " "

        let cliCmd =
            $"""dotnet run {if release then
                                "-c Release"
                            else
                                ""} --project {cliDir} -- {cliArgs}"""

        runInDir (resolveDir projectDir) cliCmd

    let runFableWithArgsInDir projectDir args =
        runFableWithArgsInDirAs true projectDir args

    let runFableWithArgsAsync projectDir args =
        runAsync (
            "dotnet run -c Release --project src/Fable.Cli -- "
            + projectDir
            + " "
            + String.concat " " args
        )

    let runNpx command args =
        run ("npx " + command + " " + (String.concat " " args))

    let runNpmScript script args =
        run ("npm run " + script + " -- " + (String.concat " " args))

    let runNpmScriptAsync script args =
        runAsync ("npm run " + script + " -- " + (String.concat " " args))

    let runFable projectDir = runFableWithArgs projectDir []

    let runMocha testDir =
        runNpmScript "mocha" [ $"{testDir} --reporter dot -t 10000" ]

open Util

module Unused =
    let downloadAndExtractTo (url: string) (targetDir: string) =
        $"npx download --extract --out %s{targetDir} \"%s{url}\"" |> run

    let coverage () =
        // report converter
        // https://github.com/danielpalme/ReportGenerator
        // dotnet tool install dotnet-reportgenerator-globaltool --tool-path tools
        if
            not (pathExists "./bin/tools/reportgenerator")
            && not (pathExists "./bin/tools/reportgenerator.exe")
        then
            runInDir
                "."
                "dotnet tool install dotnet-reportgenerator-globaltool --tool-path bin/tools"

        let reportGen =
            if pathExists "./bin/tools/reportgenerator" then
                "bin/tools/reportgenerator"
            else
                "bin\\tools\\reportgenerator.exe"

        // if not (pathExists "build/fable-library") then
        //     buildLibrary()

        cleanDirs [ "build/tests" ]
        runFable "tests"

        // JS
        run
            "npx nyc mocha build/tests --require source-map-support/register --reporter dot -t 10000"

        runInDir
            "."
            (reportGen
             + " \"-reports:build/coverage/nyc/lcov.info\" -reporttypes:Html \"-targetdir:build/coverage/nyc/html\" ")

        // .NET
        //runInDir "tests/Main" "dotnet build /t:Collect_Coverage"
        cleanDirs [ "build/coverage/netcoreapp2.0/out" ]

        runInDir
            "."
            (reportGen
             + " \"-reports:build/coverage/netcoreapp2.0/coverage.xml\" -reporttypes:Html \"-targetdir:build/coverage/netcoreapp2.0/html\" ")

// TARGETS ---------------------------

// let buildLibraryJsWithOptions (opts: {| watch: bool |}) =
//     let baseDir = __SOURCE_DIRECTORY__

//     let projectDir = baseDir </> "src/fable-library"
//     let buildDir = baseDir </> "build/fable-library"
//     let fableOpts = [
//         "--outDir " + buildDir
//         "--fableLib " + buildDir
//         "--exclude Fable.Core"
//         "--define FX_NO_BIGINT"
//         "--define FABLE_LIBRARY"
//         if opts.watch then "--watch"
//     ]

//     cleanDirs [buildDir]
//     runInDir baseDir "npm install"
//     makeDirRecursive buildDir

//     copyFile (projectDir </> "package.json") buildDir

//     if opts.watch then
//         Async.Parallel [
//             runNpmScriptAsync "tsc" [
//                 "--project " + projectDir
//                 "--watch"
//             ]
//             runFableWithArgsAsync projectDir fableOpts
//         ] |> runAsyncWorkflow
//     else
//         runTSLint projectDir
//         runTypeScript projectDir
//         runFableWithArgs projectDir fableOpts
//         removeDirRecursive (buildDir </> ".fable")

// let buildLibraryJs() = buildLibraryJsWithOptions {| watch = false |}

// let buildLibraryJsIfNotExists() =
//     if not (pathExists (__SOURCE_DIRECTORY__ </> "build/fable-library")) then
//         buildLibraryJs()

let buildLibraryTs () =
    let baseDir = __SOURCE_DIRECTORY__
    let sourceDir = "./src/fable-library"
    let buildDirTs = "./build/fable-library-ts"
    let buildDirJs = "./build/fable-library"

    cleanDirs
        [
            buildDirTs
            buildDirJs
        ]

    runInDir baseDir "npm install"

    runFableWithArgs
        sourceDir
        [
            "--outDir " + buildDirTs
            "--fableLib " + buildDirTs
            "--lang TypeScript"
            "--typedArrays false"
            "--exclude Fable.Core"
            "--define FX_NO_BIGINT"
            "--define FABLE_LIBRARY"
        ]

    copyFiles sourceDir "*.ts" buildDirTs
    copyFiles (sourceDir </> "ts") "*.json" buildDirTs
    copyDirRecursive (sourceDir </> "lib") (buildDirTs </> "lib")
    copyFile (sourceDir </> "package.json") buildDirTs

    // runTSLint buildDirTs
    runTypeScriptWithArgs buildDirTs [ "--outDir " + buildDirJs ]
    copyFile (buildDirTs </> "lib/big.d.ts") (buildDirJs </> "lib/big.d.ts")
    copyFile (buildDirTs </> "package.json") buildDirJs

    copyFile (sourceDir </> "README.md") buildDirJs

let buildLibraryTsIfNotExists () =
    if not (pathExists (__SOURCE_DIRECTORY__ </> "build/fable-library-ts")) then
        buildLibraryTs ()

let buildLibraryPy () =
    let libraryDir = "./src/fable-library-py"
    let projectDir = libraryDir </> "fable_library"
    let buildDirPy = "./build/fable-library-py"

    cleanDirs [ buildDirPy ]

    runFableWithArgs
        projectDir
        [
            "--outDir " + buildDirPy </> "fable_library"
            "--fableLib " + buildDirPy </> "fable_library"
            "--lang Python"
            "--exclude Fable.Core"
            "--define FABLE_LIBRARY"
        ]

    // Copy python related files from projectDir to buildDir
    copyFiles libraryDir "*" buildDirPy
    copyFiles projectDir "*.py" (buildDirPy </> "fable_library")

    // Fix issues with Fable .fsproj not supporting links
    copyDirNonRecursive
        (buildDirPy </> "fable_library/fable-library")
        (buildDirPy </> "fable_library")

    removeDirRecursive (buildDirPy </> "fable_library/fable-library")

let buildLibraryPyIfNotExists () =
    let baseDir = __SOURCE_DIRECTORY__

    if not (pathExists (baseDir </> "build/fable-library-py")) then
        buildLibraryPy ()

let buildLibraryRust () =
    let libraryDir = "src/fable-library-rust"
    let sourceDir = libraryDir </> "src"
    let buildDir = "build/fable-library-rust"
    let outDir = buildDir </> "src"
    let fableLib = "."

    cleanDirs [ buildDir ]

    runFableWithArgsInDir
        sourceDir
        [
            "--outDir " + resolveDir outDir
            "--fableLib " + fableLib
            "--lang Rust"
            "--exclude Fable.Core"
            "--define FABLE_LIBRARY"
            "--noCache"
        ]

    copyFiles libraryDir "*.toml" buildDir
    copyFiles sourceDir "*.rs" outDir
    copyDirRecursive (libraryDir </> "vendored") (buildDir </> "vendored")

    runInDir buildDir "cargo fmt"
    runInDir buildDir "cargo fix --allow-no-vcs"
    runInDir buildDir "cargo build"

let buildLibraryRustIfNotExists () =
    if
        not (pathExists (__SOURCE_DIRECTORY__ </> "build/fable-library-rust"))
    then
        buildLibraryRust ()

let buildLibraryDart (clean: bool) =
    let sourceDir = resolveDir "src/fable-library-dart"
    let buildDir = resolveDir "build/fable-library-dart"

    if clean then
        cleanDirs [ buildDir ]
        makeDirRecursive buildDir
        copyFiles sourceDir "pubspec.*" buildDir
        copyFiles sourceDir "analysis_options.yaml" buildDir

    copyFiles sourceDir "*.dart" buildDir

    runFableWithArgsInDir
        sourceDir
        [
            "--outDir " + buildDir
            "--fableLib " + buildDir
            "--lang Dart"
            "--exclude Fable.Core"
            "--define FABLE_LIBRARY"
            if clean then
                "--noCache"
        ]

let buildLibraryDartIfNotExists () =
    if
        not (pathExists (__SOURCE_DIRECTORY__ </> "build/fable-library-dart"))
    then
        buildLibraryDart (true)

// Like testStandalone() but doesn't create bundles/packages for fable-standalone & friends
// Mainly intended for CI
let testStandaloneFast () =
    runFableWithArgs "src/fable-standalone/src" [ "--noCache" ]

    runFableWithArgs
        "src/fable-compiler-js/src"
        [
            "--exclude Fable.Core"
            "--define LOCAL_TEST"
        ]

    let fableJs = "./src/fable-compiler-js/src/app.fs.js"
    let testProj = "tests/Js/Main/Fable.Tests.fsproj"
    let buildDir = "build/tests/Standalone"
    run $"node {fableJs} {testProj} {buildDir}"
    runMocha buildDir

let buildWorker
    (opts:
        {|
            minify: bool
            watch: bool
        |})
    =
    printfn
        "Building worker%s..."
        (if opts.minify then
             ""
         else
             " (no minification)")

    let projectDir = "src/fable-standalone/src"
    let buildDir = "build/fable-standalone"
    let fableLib = "./build/fable-library"
    let distDir = "src/fable-standalone/dist"

    runFableWithArgs
        (projectDir + "/Worker")
        [
            "--outDir " + buildDir + "/worker"
            "--fableLib " + fableLib
        ]

    let rollupTarget =
        match opts.minify with
        | true -> buildDir </> "worker.js"
        | false -> distDir </> "worker.min.js"

    // make standalone worker dist
    runNpmScript
        "rollup"
        [ $"""{buildDir}/worker/Worker.js -o {rollupTarget} --format iife""" ]

    if opts.minify then
        // runNpx "webpack" [sprintf "--entry ./%s/worker.js --output ./%s/worker.min.js --config ./%s/../worker.config.js" buildDir distDir projectDir]
        runNpmScript
            "terser"
            [
                $"{buildDir}/worker.js -o {distDir}/worker.min.js --mangle --compress"
            ]

    // Put fable-library files next to bundle
    printfn "Copying fable-library..."
    buildLibraryTsIfNotExists ()
    let libraryDir = "build/fable-library"
    let libraryTarget = distDir </> "fable-library"
    copyDirRecursive libraryDir libraryTarget

let buildStandalone
    (opts:
        {|
            minify: bool
            watch: bool
        |})
    =
    buildLibraryTs ()

    printfn
        "Building standalone%s..."
        (if opts.minify then
             ""
         else
             " (no minification)")

    let projectDir = "src/fable-standalone/src"
    let buildDir = "build/fable-standalone"
    let fableLib = "./build/fable-library"
    let distDir = "src/fable-standalone/dist"

    let rollupTarget =
        match opts.watch, opts.minify with
        | true, _ ->
            match BUILD_ARGS with
            | _ :: rollupTarget :: _ -> rollupTarget
            | _ ->
                failwith
                    "Pass the bundle output, e.g.: npm run build watch-standalone ../repl3/public/js/repl/bundle.min.js"
        | false, true -> buildDir </> "bundle.js"
        | false, false -> distDir </> "bundle.min.js"

    let rollupArgs =
        [
            buildDir </> "bundle/Main.js"
            "-o " + rollupTarget
            "--format umd"
            "--name __FABLE_STANDALONE__"
        ]

    // cleanup
    if not opts.watch then
        cleanDirs
            [
                buildDir
                distDir
            ]

        makeDirRecursive distDir

    // build standalone bundle
    runFableWithArgs
        projectDir
        [
            "--outDir " + buildDir </> "bundle"
            "--fableLib " + fableLib
            if opts.watch then
                "--watch"
                "--run rollup"
                yield! rollupArgs
                "--watch"
        ]

    // make standalone bundle dist
    runNpmScript "rollup" rollupArgs

    if opts.minify then
        runNpmScript
            "terser"
            [
                buildDir </> "bundle.js"
                "-o " + distDir </> "bundle.min.js"
                "--mangle"
                "--compress"
            ]

    // build standalone worker
    buildWorker opts

    // print bundle size
    fileSizeInBytes (distDir </> "bundle.min.js") / 1000.
    |> printfn "Bundle size: %fKB"

    fileSizeInBytes (distDir </> "worker.min.js") / 1000.
    |> printfn "Worker size: %fKB"

let buildCompilerJs (minify: bool) =
    let projectDir = "src/fable-compiler-js/src"
    let buildDir = "build/fable-compiler-js"
    let fableLib = "./build/fable-library"
    let distDir = "src/fable-compiler-js/dist"

    if not (pathExists "build/fable-standalone") then
        buildStandalone
            {|
                minify = minify
                watch = false
            |}

    cleanDirs
        [
            buildDir
            distDir
        ]

    makeDirRecursive distDir

    runFableWithArgs
        projectDir
        [
            "--outDir " + buildDir
            "--fableLib " + fableLib
            "--exclude Fable.Core"
        ]

    let rollupTarget =
        if minify then
            distDir </> "app.js"
        else
            distDir </> "app.min.js"

    run
        $"npx rollup {buildDir}/app.js -o {rollupTarget} --format umd --name Fable"

    if minify then
        run
            $"npx terser {distDir}/app.js -o {distDir}/app.min.js --mangle --compress"

    // Copy fable-library
    copyDirRecursive ("build/fable-library") (distDir </> "fable-library")
    // Copy fable-metadata
    copyDirRecursive ("src/fable-metadata/lib") (distDir </> "fable-metadata")

let testStandalone (minify) =
    let fableDir = "src/fable-compiler-js"
    let buildDir = "build/tests/Standalone"

    if not (pathExists "build/fable-compiler-js") then
        buildCompilerJs (minify)

    cleanDirs [ buildDir ]

    // Link fable-compiler-js to local packages
    runInDir fableDir "npm link ../fable-metadata"
    runInDir fableDir "npm link ../fable-standalone"

    // Test fable-compiler-js locally
    run $"node {fableDir} tests/Js/Main/Fable.Tests.fsproj {buildDir}"
    runMocha buildDir

    // // Another local fable-compiler-js test
    // runInDir (fableDir </> "test") "node .. test_script.fsx"
    // runInDir (fableDir </> "test") "node test_script.fsx.js"

    // Unlink local packages after test
    runInDir
        fableDir
        "npm unlink ../fable-metadata && cd ../fable-metadata && npm unlink"

    runInDir
        fableDir
        "npm unlink ../fable-standalone && cd ../fable-standalone && npm unlink"

let testReact () =
    runFableWithArgs "tests/React" [ "--noCache" ]
    runInDir "tests/React" "npm i && npm test"

let compileAndRunTestsWithMocha clean projectName =
    let projectDir = "tests/Js/" + projectName
    let buildDir = "build/tests/Js/" + projectName
    let fableLib = "./build/fable-library"

    if clean then
        cleanDirs [ buildDir ]

    runFableWithArgs
        projectDir
        [
            "--outDir " + buildDir
            "--fableLib " + fableLib
            "--exclude Fable.Core"
        ]

    runMocha buildDir

let testProjectConfigs () =
    [
        "tests/Integration/ProjectConfigs/DebugWithExtraDefines", "Debug"
        "tests/Integration/ProjectConfigs/CustomConfiguration", "Test"
        "tests/Integration/ProjectConfigs/ReleaseNoExtraDefines", String.Empty
        "tests/Integration/ProjectConfigs/ConsoleApp", String.Empty
    ]
    |> List.iter (fun (projectDir, configuration) ->
        let buildDir = "build/" + projectDir
        let fableLib = "./build/fable-library"

        cleanDirs [ buildDir ]

        runFableWithArgs
            projectDir
            [
                "--outDir " + buildDir
                "--fableLib " + fableLib
                "--exclude Fable.Core"
                if not (String.IsNullOrEmpty configuration) then
                    "--configuration " + configuration
            ]

        runMocha buildDir
    )

let testIntegration () =
    buildLibraryTsIfNotExists ()
    runInDir "tests/Integration/Integration" "dotnet run -c Release"
    runInDir "tests/Integration/Compiler" "dotnet run -c Release"
    testProjectConfigs ()

let testJs () =
    buildLibraryTsIfNotExists ()

    compileAndRunTestsWithMocha true "Main"

    runInDir "tests/Js/Main" "dotnet run -c Release"

    // Adaptive tests must go in a different project to avoid conflicts with Queue shim, see #2559
    compileAndRunTestsWithMocha true "Adaptive"

    testReact ()

    if envVarOrNone "CI" |> Option.isSome then
        testStandaloneFast ()

let testTypeScript isWatch =
    buildLibraryTsIfNotExists ()

    let projectDir = "tests/TypeScript"
    let buildDir = "build/tests/TypeScript"
    let buildDir2 = "build/tests/TypeScriptCompiled"
    let fableLib = "fable-library-ts"

    cleanDirs
        [
            buildDir
            buildDir2
        ]

    copyFile (projectDir </> "tsconfig.json") (buildDir </> "tsconfig.json")

    runFableWithArgsInDirAs
        (not isWatch)
        "."
        [
            projectDir
            "--lang ts"
            "--outDir " + buildDir
            "--fableLib " + fableLib
            "--exclude Fable.Core"
            if isWatch then
                "--watch"
                $"--runWatch npm run test-ts"
        ]

    runNpmScript "test-ts" []

let testPython () =
    buildLibraryPyIfNotExists () // NOTE: fable-library-py needs to be built separately.

    let projectDir = "tests/Python"
    let buildDir = "build/tests/Python"
    let fableLib = "fable-library-py"

    cleanDirs [ buildDir ]
    runInDir projectDir "dotnet test -c Release"

    runFableWithArgs
        projectDir
        [
            "--outDir " + buildDir
            "--fableLib " + fableLib
            "--exclude Fable.Core"
            "--lang Python"
        ]

    runInDir buildDir "poetry run pytest -x"
// Testing in Windows
// runInDir buildDir "python -m pytest -x"

type RustTestMode =
    | NoStd
    | Default
    | Threaded

let testRust testMode =
    // buildLibraryRustIfNotExists()
    buildLibraryRust ()

    let testAstDir = "src/Fable.Transforms/Rust/AST/Tests"
    let projectDir = "tests/Rust"
    let buildDir = "build/tests/Rust"
    let fableLib = "fable-library-rust"

    // limited cleanup to reduce IO churn, speed up rebuilds,
    // and save the ssd (target folder can get huge)
    cleanDirs [ buildDir </> "src" ]
    cleanDirs [ buildDir </> "tests" ]
    cleanDirs [ buildDir </> ".fable" ]

    // copy rust only tests files (these must be present when running dotnet test as import expr tests for file presence)
    makeDirRecursive (buildDir </> "tests" </> "src")
    copyFiles (projectDir </> "tests/src") "*.rs" (buildDir </> "tests/src")

    // run .NET tests
    runInDir testAstDir "dotnet test -c Release"
    runInDir projectDir "dotnet test -c Release"

    // build Fable Rust tests
    runFableWithArgs
        projectDir
        [
            "--outDir " + buildDir
            "--exclude Fable.Core"
            "--lang Rust"
            "--fableLib " + fableLib
            "--noCache"
            if testMode = NoStd then
                "--define NO_STD_NO_EXCEPTIONS"
        ]

    // copy project file
    copyFile (projectDir </> "Cargo.toml") buildDir

    // rustfmt all tests
    runInDir buildDir "cargo fmt"
    // runInDir buildDir "cargo fix --allow-no-vcs"
    runInDir buildDir "cargo build"

    // run Fable Rust tests
    match testMode with
    | Default -> runInDir buildDir "cargo test"
    | NoStd -> runInDir buildDir "cargo test --features no_std"
    | Threaded -> runInDir buildDir "cargo test --features threaded"

let testDart isWatch =
    if not (pathExists "build/fable-library-dart") then
        buildLibraryDart (true)

    let buildDir = resolveDir "build/tests/Dart"
    let sourceDir = resolveDir "tests/Dart"

    cleanDirs [ buildDir ]
    makeDirRecursive buildDir
    copyFiles sourceDir "pubspec.*" buildDir
    copyFiles sourceDir "analysis_options.yaml" buildDir
    copyFiles sourceDir "*.dart" buildDir

    runFableWithArgsInDirAs
        (not isWatch)
        sourceDir
        [
            "src"
            "--outDir " + (buildDir </> "src")
            "--exclude Fable.Core"
            "--lang Dart"
            "--noCache"
            if isWatch then
                "--watch"
                $"--runWatch dart test {buildDir}/main.dart"
        ]

    runInDir buildDir "dart test main.dart"

let buildLocalPackageWith pkgDir pkgCommand fsproj action =
    let version =
        Publish.loadReleaseVersion "src/Fable.Cli"
        + "-local-build-"
        + DateTime.Now.ToString("yyyyMMdd-HHmm")

    action version
    updatePkgVersionInFsproj fsproj version
    run $"dotnet pack {fsproj} -p:Pack=true -c Release -o {pkgDir}"

    // Return install command
    $"""dotnet {pkgCommand} --version "{version}" --add-source {fullPath pkgDir}"""

let buildLocalPackage pkgDir =
    buildLocalPackageWith
        pkgDir
        "tool install fable"
        (resolveDir "src/Fable.Cli/Fable.Cli.fsproj")
        (fun version ->
            updateVersionsInFableTransforms version [] |> ignore
            buildLibraryTs ()
        )

let testRepos () =
    let repos =
        [
            "https://github.com/alfonsogarciacaro/FsToolkit.ErrorHandling:update-fable-3",
            "npm i && npm test"
            "https://github.com/fable-compiler/fable-promise:master",
            "npm i && npm test"
            "https://github.com/alfonsogarciacaro/Thoth.Json:nagareyama",
            "dotnet paket restore && npm i && dotnet fable tests -o tests/bin --run mocha tests/bin"
            "https://github.com/alfonsogarciacaro/FSharp.Control.AsyncSeq:nagareyama",
            "cd tests/fable && npm i && npm test"
            "https://github.com/alfonsogarciacaro/Fable.Extras:nagareyama",
            "dotnet paket restore && npm i && npm test"
            "https://github.com/alfonsogarciacaro/Fable.Jester:nagareyama",
            "npm i && npm test"
            "https://github.com/Zaid-Ajaj/Fable.SimpleJson:master",
            "npm i && npm run test-nagareyama"
        ]

    let testDir = tempPath () </> "fable-repos"
    printfn $"Cloning repos to: {testDir}"

    cleanDirs [ testDir ]
    makeDirRecursive testDir
    let pkgInstallCmd = buildLocalPackage (testDir </> "pkg")

    for (repo, command) in repos do
        let url, branch =
            let i = repo.LastIndexOf(":") in repo[.. i - 1], repo[i + 1 ..]

        let name = url[url.LastIndexOf("/") + 1 ..]
        runInDir testDir $"git clone {url} {name}"
        let repoDir = testDir </> name
        runInDir repoDir ("git checkout " + branch)
        runInDir repoDir "dotnet tool uninstall fable"
        runInDir repoDir pkgInstallCmd
        runInDir repoDir "dotnet tool restore"
        runInDir repoDir command

let githubRelease () =
    match envVarOrNone "GITHUB_USER", envVarOrNone "GITHUB_TOKEN" with
    | Some user, Some token ->
        try
            let version, notes =
                Publish.loadReleaseVersionAndNotes "src/Fable.Cli"

            let notes =
                notes
                |> Array.map (fun n ->
                    $"""'{n.Replace("'", @"\'").Replace("`", @"\`")}'"""
                )
                |> String.concat ","

            run $"git commit -am \"Release {version}\" && git push"

            runSilent
                $"""node --eval "require('ghreleases').create({{ user: '{user}', token: '{token}', }}, 'fable-compiler', 'Fable', {{ tag_name: '{version}', name: '{version}', body: [{notes}].join('\n'), }}, (err, res) => {{ if (err != null) {{ console.error(err) }} }})" """

            printfn $"Github release %s{version} created successfully"
        with ex ->
            printfn $"Github release failed: %s{ex.Message}"
    | _ ->
        failwith "Expecting GITHUB_USER and GITHUB_TOKEN enviromental variables"

let copyFcsRepo sourceDir =
    let targetDir = "src/fcs-fable"
    let path1 = "fcs/fcs-fable"
    let path2 = "src/Compiler"
    cleanDirs [ targetDir ]
    copyDirRecursive (sourceDir </> path1) targetDir
    copyDirRecursive (sourceDir </> path2) (targetDir </> path2)
    removeFile (targetDir </> ".gitignore")
    let projPath = (targetDir </> "fcs-fable.fsproj")
    let projText = readFile projPath

    let projText =
        Regex.Replace(
            projText,
            @"(<FSharpSourcesRoot>\$\(MSBuildProjectDirectory\)).*?(<\/FSharpSourcesRoot>)",
            "$1/src/Compiler$2"
        )
    // let projText =
    //     Regex.Replace(projText,
    //         @"artifacts\/bin\/FSharp.Core\/Release\/netstandard2.0",
    //         "lib/fcs")
    projText |> writeFile projPath

let syncFcsRepo () =
    // FAKE is giving lots of problems with the dotnet SDK version, ignore it
    let cheatWithDotnetSdkVersion dir f =
        let path = dir </> "build.fsx"
        let script = readFile path

        Regex.Replace(
            script,
            @"let dotnetExePath =[\s\S]*DotNetCli\.InstallDotNetSDK",
            "let dotnetExePath = \"dotnet\" //DotNetCli.InstallDotNetSDK"
        )
        |> writeFile path

        f ()
        runInDir dir "git reset --hard"

    printfn $"Expecting %s{FCS_REPO} repo to be cloned at %s{FCS_REPO_LOCAL}"

    // TODO: Prompt to reset --hard changes
    // service_slim
    runInDir FCS_REPO_LOCAL ("git checkout " + FCS_REPO_SERVICE_SLIM_BRANCH)
    runInDir FCS_REPO_LOCAL "git pull"

    cheatWithDotnetSdkVersion
        (FCS_REPO_LOCAL </> "fcs")
        (fun () -> runBashOrCmd (FCS_REPO_LOCAL </> "fcs") "build" "")

    copyFile
        (FCS_REPO_LOCAL
         </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Compiler.Service.dll")
        "../fable/lib/fcs/"

    copyFile
        (FCS_REPO_LOCAL
         </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Compiler.Service.xml")
        "../fable/lib/fcs/"

    copyFile
        (FCS_REPO_LOCAL
         </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Core.dll")
        "../fable/lib/fcs/"

    copyFile
        (FCS_REPO_LOCAL
         </> "artifacts/bin/FSharp.Compiler.Service/Release/netstandard2.0/FSharp.Core.xml")
        "../fable/lib/fcs/"

    // fcs-fable
    runInDir FCS_REPO_LOCAL ("git checkout " + FCS_REPO_FABLE_BRANCH)
    runInDir FCS_REPO_LOCAL "git pull"

    cheatWithDotnetSdkVersion
        (FCS_REPO_LOCAL </> "fcs")
        (fun () ->
            runBashOrCmd (FCS_REPO_LOCAL </> "fcs") "build" "CodeGen.Fable"
        )

    copyFcsRepo FCS_REPO_LOCAL

let packages () =
    [
        "Fable.AST", doNothing
        "Fable.Core", doNothing
        "Fable.Cli",
        (fun () ->
            // TODO: Add library versions for other languages
            let compilerVersion = Publish.loadReleaseVersion "src/Fable.Cli"

            let updatedLibs =
                updateVersionsInFableTransforms
                    compilerVersion
                    [ "js", getNpmVersion "src/fable-library" ]

            buildLibraryTs ()
            buildLibraryPy ()
            buildLibraryRust ()
            buildLibraryDart true

            if updatedLibs.Contains("js") then
                pushNpmWithoutReleaseNotesCheck "build/fable-library"
        )
        "Fable.PublishUtils", doNothing
        "fable-metadata", doNothing
        "fable-standalone",
        fun () ->
            buildStandalone
                {|
                    minify = true
                    watch = false
                |}
        "fable-compiler-js", (fun () -> buildCompilerJs true)
    ]

let publishPackages restArgs =
    let packages =
        match List.tryHead restArgs with
        | Some pkg -> packages () |> List.filter (fun (name, _) -> name = pkg)
        | None -> packages ()

    for (pkg, buildAction) in packages do
        if Char.IsUpper pkg[0] then
            let projFile = "src" </> pkg </> pkg + ".fsproj"
            pushFableNuget projFile [ "Pack", "true" ] buildAction
        else
            pushNpm ("src" </> pkg) buildAction

let hasFlag flag = BUILD_ARGS_LOWER |> List.contains flag

match BUILD_ARGS_LOWER with
// | "check-sourcemaps"::_ ->
//     ("src/quicktest/Quicktest.fs", "src/quicktest/bin/Quicktest.js", "src/quicktest/bin/Quicktest.js.map")
//     |||> sprintf "nodemon --watch src/quicktest/bin/Quicktest.js --exec 'source-map-visualization --sm=\"%s;%s;%s\"'"
//     |> List.singleton |> quicktest
// | "coverage"::_ -> coverage()
| ("test" | "test-js") :: _ -> testJs ()
| "test-mocha" :: _ -> compileAndRunTestsWithMocha true "Main"
| "test-mocha-fast" :: _ -> compileAndRunTestsWithMocha false "Main"
| "test-react" :: _ -> testReact ()
| "test-standalone" :: _ ->
    let minify = hasFlag "--no-minify" |> not
    testStandalone (minify)
| "test-standalone-fast" :: _ -> testStandaloneFast ()
| "test-configs" :: _ -> testProjectConfigs ()
| "test-integration" :: _ -> testIntegration ()
| "test-repos" :: _ -> testRepos ()
| ("test-ts" | "test-typescript") :: _ -> testTypeScript (false)
| ("watch-test-ts" | "watch-test-typescript") :: _ -> testTypeScript (true)
| "test-py" :: _ -> testPython ()
| "test-rust" :: _ -> testRust Default
| "test-rust-no_std" :: _ -> testRust NoStd
| "test-rust-default" :: _ -> testRust Default
| "test-rust-threaded" :: _ -> testRust Threaded
| "test-dart" :: _ -> testDart (false)
| "watch-test-dart" :: _ -> testDart (true)

| "quicktest" :: _ ->
    buildLibraryTsIfNotExists ()

    watchFableWithArgs
        "src/quicktest"
        [ "--watch --exclude Fable.Core --noCache --runScript" ]
| "quicktest-ts" :: _ ->
    buildLibraryTsIfNotExists ()
    let srcDir = "src/quicktest"
    let outPath = "build/quicktest-ts/Quicktest.fs.js"
    // Make sure outPath exists so nodemon doesn't complain
    if not (pathExists outPath) then
        makeDirRecursive (dirname outPath)
        writeFile outPath ""

    let runCmd =
        $"npx concurrently \"tsc -w -p {srcDir} --outDir {dirname outPath}\" \"nodemon -w {outPath} {outPath}\""

    watchFableWithArgs
        srcDir
        [
            "--lang ts --watch --exclude Fable.Core --noCache --run"
            runCmd
        ]
| ("quicktest-py" | "quicktest-python") :: _ ->
    buildLibraryPyIfNotExists ()

    watchFableWithArgs
        "src/quicktest-py"
        [ "--lang py --watch --exclude Fable.Core --noCache --runScript" ]
| "quicktest-dart" :: _ ->
    buildLibraryDartIfNotExists ()

    watchFableWithArgs
        "src/quicktest-dart"
        [ "--lang dart --watch --exclude Fable.Core --noCache --runScript" ]
| ("quicktest-rs" | "quicktest-rust") :: _ ->
    buildLibraryRustIfNotExists ()

    watchFableWithArgs
        "src/quicktest-rust"
        [
            "--lang rs -e .rs --watch --exclude Fable.Core --noCache --runScript"
        ]
| "run" :: _ ->
    buildLibraryTsIfNotExists ()
    // Don't take args from pattern matching because they're lowered
    let restArgs = BUILD_ARGS |> List.skip 1 |> String.concat " "

    run
        $"""dotnet run -c Release --project {resolveDir "src/Fable.Cli"} -- {restArgs}"""

| "package" :: _ ->
    let pkgInstallCmd = buildLocalPackage (resolveDir "temp/pkg")

    printfn
        $"\nPackage has been created, use the following command to install it:\n    {pkgInstallCmd}\n"
| "package-core" :: _ ->
    let pkgInstallCmd =
        buildLocalPackageWith
            (resolveDir "temp/pkg")
            "add package Fable.Core"
            (resolveDir "src/Fable.Core/Fable.Core.fsproj")
            ignore

    printfn
        $"\nFable.Core package has been created, use the following command to install it:\n    {pkgInstallCmd}\n"

| ("fable-library" | "library") :: _
| ("fable-library-ts" | "library-ts") :: _ -> buildLibraryTs ()
| ("fable-library-py" | "library-py") :: _ -> buildLibraryPy ()
| ("fable-library-rust" | "library-rust") :: _ -> buildLibraryRust ()
| ("fable-library-dart" | "library-dart") :: _ ->
    let clean = hasFlag "--no-clean" |> not
    buildLibraryDart (clean)

| ("fable-compiler-js" | "compiler-js") :: _ ->
    let minify = hasFlag "--no-minify" |> not
    buildCompilerJs (minify)
| ("fable-standalone" | "standalone") :: _ ->
    let minify = hasFlag "--no-minify" |> not

    buildStandalone
        {|
            minify = minify
            watch = false
        |}
| ("fable-worker" | "worker") :: _ ->
    let minify = hasFlag "--no-minify" |> not

    buildWorker
        {|
            minify = minify
            watch = false
        |}
| "watch-standalone" :: _ ->
    buildStandalone
        {|
            minify = false
            watch = true
        |}

| "sync-fcs-repo" :: _ -> syncFcsRepo ()
| "copy-fcs-repo" :: _ -> copyFcsRepo FCS_REPO_LOCAL

| "publish" :: restArgs -> publishPackages restArgs
| "github-release" :: _ ->
    publishPackages []
    githubRelease ()

| _ ->
    printfn
        """Please pass a target name. Examples:

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
