#load "src/Fable.PublishUtils/PublishUtils.fs"

open System
open System.Text.RegularExpressions
open PublishUtils

// ncave FCS fork
let FCS_REPO = "https://github.com/ncave/fsharp"
let FCS_REPO_LOCAL = "../fsharp_fable"
let FCS_REPO_FABLE_BRANCH = "fable"
let FCS_REPO_SERVICE_SLIM_BRANCH = "service_slim"

let BUILD_ARGS =
    fsi.CommandLineArgs
    |> Array.skip 1
    |> List.ofArray

let BUILD_ARGS_LOWER =
    BUILD_ARGS
    |> List.map (fun x -> x.ToLower())

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

    let resolveDir dir =
        __SOURCE_DIRECTORY__ </> dir

    let updateVersionsInFableTransforms compilerVersion (libraryVersions: (string * string) list) =
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
                    m.Groups[1].Value + $"let [<Literal>] {prefix}VERSION = \"{version}\""
                ),
                RegexOptions.Multiline)

        let filePath = "src/Fable.Transforms/Global/Compiler.fs"
        readFile filePath
        |> replaceVersion None compilerVersion
        |> List.foldBack (fun (lang, version) fileContent ->
            replaceVersion (Some lang) version fileContent) libraryVersions
        |> writeFile filePath

        updated

    let updatePkgVersionInFsproj projFile version =
        readFile projFile
        |> replaceRegex Publish.NUGET_PACKAGE_VERSION (fun m ->
            m.Groups[1].Value + version + m.Groups[3].Value)
        |> writeFile projFile

    let runTSLint projectDir =
        run ("npm run tslint -- --project " + projectDir)

    let runTypeScript projectDir =
        run ("npm run tsc -- --project " + projectDir)

    let runTypeScriptWithArgs projectDir args =
        run ("npm run tsc -- --project " + projectDir + " " + String.concat " " args)

    let runFableWithArgs projectDir args =
        run ("dotnet run -c Release --project src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

    let watchFableWithArgs projectDir args =
        run ("dotnet watch --project src/Fable.Cli run -- " + projectDir + " --cwd ../.. " + String.concat " " args)

    let runFableWithArgsInDirAs release projectDir args =
        let cliDir = resolveDir "src/Fable.Cli"
        let cliArgs = args |> String.concat " "
        let cliCmd = $"""dotnet run {if release then "-c Release" else ""} --project {cliDir} -- {cliArgs}"""
        runInDir (resolveDir projectDir) cliCmd

    let runFableWithArgsInDir projectDir args =
        runFableWithArgsInDirAs true projectDir args

    let runFableWithArgsAsync projectDir args =
        runAsync ("dotnet run -c Release --project src/Fable.Cli -- " + projectDir + " " + String.concat " " args)

    let runNpx command args =
        run ("npx " + command + " " + (String.concat " " args))

    let runNpmScript script args =
        run ("npm run " + script + " -- " + (String.concat " " args))

    let runNpmScriptAsync script args =
        runAsync ("npm run " + script + " -- " + (String.concat " " args))

    let runFable projectDir =
        runFableWithArgs projectDir []

    let runMocha testDir =
        runNpmScript "mocha" [$"{testDir} --reporter dot -t 10000"]

open Util

module Unused =
    let downloadAndExtractTo (url: string) (targetDir: string) =
        $"npx download --extract --out %s{targetDir} \"%s{url}\"" |> run

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



let buildWorker (opts: {| minify: bool; watch: bool |}) =
    printfn "Building worker%s..." (if opts.minify then "" else " (no minification)")

    let projectDir = "src/fable-standalone/src"
    let buildDir = "build/fable-standalone"
    let distDir = "src/fable-standalone/dist"

    runFableWithArgs (projectDir + "/Worker") [
        "--outDir " + buildDir + "/worker"
    ]

    let rollupTarget =
        match opts.minify with
        | true -> buildDir </> "worker.js"
        | false -> distDir </> "worker.min.js"

    // make standalone worker dist
    runNpmScript "rollup" [$"""{buildDir}/worker/Worker.js -o {rollupTarget} --format iife"""]

    if opts.minify then
        // runNpx "webpack" [sprintf "--entry ./%s/worker.js --output ./%s/worker.min.js --config ./%s/../worker.config.js" buildDir distDir projectDir]
        runNpmScript "terser" [$"{buildDir}/worker.js -o {distDir}/worker.min.js --mangle --compress"]

    // Put fable-library files next to bundle
    printfn "Copying fable-library..."
    // buildLibraryTsIfNotExists()
    let libraryDir = "build/fable-library"
    let libraryTarget = distDir </> "fable-library"
    copyDirRecursive libraryDir libraryTarget

let buildStandalone (opts: {| minify: bool; watch: bool |}) =
    // buildLibraryTs()

    printfn "Building standalone%s..." (if opts.minify then "" else " (no minification)")

    let projectDir = "src/fable-standalone/src"
    let buildDir = "build/fable-standalone"
    let distDir = "src/fable-standalone/dist"

    let rollupTarget =
        match opts.watch, opts.minify with
        | true, _ ->
            match BUILD_ARGS with
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

    // make standalone bundle dist
    runNpmScript "rollup" rollupArgs
    if opts.minify then
        runNpmScript "terser" [
            buildDir </> "bundle.js"
            "-o " + distDir </> "bundle.min.js"
            "--mangle"
            "--compress"
        ]

    // build standalone worker
    buildWorker opts

    // print bundle size
    fileSizeInBytes (distDir </> "bundle.min.js") / 1000. |> printfn "Bundle size: %fKB"
    fileSizeInBytes (distDir </> "worker.min.js") / 1000. |> printfn "Worker size: %fKB"

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

let testStandalone(minify) =
    let fableDir = "src/fable-compiler-js"
    let buildDir = "build/tests/Standalone"

    if not (pathExists "build/fable-compiler-js") then
        buildCompilerJs(minify)

    cleanDirs [buildDir]

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
    runInDir fableDir "npm unlink ../fable-metadata && cd ../fable-metadata && npm unlink"
    runInDir fableDir "npm unlink ../fable-standalone && cd ../fable-standalone && npm unlink"

let buildLocalPackageWith pkgDir pkgCommand fsproj action =
    let version = Publish.loadReleaseVersion "src/Fable.Cli" + "-local-build-" + DateTime.Now.ToString("yyyyMMdd-HHmm")
    action version
    updatePkgVersionInFsproj fsproj version
    run $"dotnet pack {fsproj} -p:Pack=true -c Release -o {pkgDir}"

    // Return install command
    $"""dotnet {pkgCommand} --version "{version}" --add-source {fullPath pkgDir}"""

let buildLocalPackage pkgDir =
    buildLocalPackageWith pkgDir
        "tool install fable"
        (resolveDir "src/Fable.Cli/Fable.Cli.fsproj") (fun version ->
            updateVersionsInFableTransforms version [] |> ignore
            // buildLibraryTs()
            )

let testRepos() =
    let repos = [
        "https://github.com/alfonsogarciacaro/FsToolkit.ErrorHandling:update-fable-3", "npm i && npm test"
        "https://github.com/fable-compiler/fable-promise:master", "npm i && npm test"
        "https://github.com/alfonsogarciacaro/Thoth.Json:nagareyama", "dotnet paket restore && npm i && dotnet fable tests -o tests/bin --run mocha tests/bin"
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
        let url, branch = let i = repo.LastIndexOf(":") in repo[..i-1], repo[i+1..]
        let name = url[url.LastIndexOf("/") + 1..]
        runInDir testDir $"git clone {url} {name}"
        let repoDir = testDir </> name
        runInDir repoDir ("git checkout " + branch)
        runInDir repoDir "dotnet tool uninstall fable"
        runInDir repoDir pkgInstallCmd
        runInDir repoDir "dotnet tool restore"
        runInDir repoDir command


let hasFlag flag =
    BUILD_ARGS_LOWER |> List.contains flag

match BUILD_ARGS_LOWER with
// | "check-sourcemaps"::_ ->
//     ("src/quicktest/Quicktest.fs", "src/quicktest/bin/Quicktest.js", "src/quicktest/bin/Quicktest.js.map")
//     |||> sprintf "nodemon --watch src/quicktest/bin/Quicktest.js --exec 'source-map-visualization --sm=\"%s;%s;%s\"'"
//     |> List.singleton |> quicktest
// | "coverage"::_ -> coverage()

// | "test-standalone-fast"::_ -> testStandaloneFast()
| "test-repos"::_ -> testRepos()

| "package"::_ ->
    let pkgInstallCmd = buildLocalPackage (resolveDir "temp/pkg")
    printfn $"\nPackage has been created, use the following command to install it:\n    {pkgInstallCmd}\n"
| "package-core"::_ ->
    let pkgInstallCmd = buildLocalPackageWith (resolveDir "temp/pkg") "add package Fable.Core" (resolveDir "src/Fable.Core/Fable.Core.fsproj") ignore
    printfn $"\nFable.Core package has been created, use the following command to install it:\n    {pkgInstallCmd}\n"

| ("fable-compiler-js"|"compiler-js")::_ ->
    let minify = hasFlag "--no-minify" |> not
    buildCompilerJs(minify)
| ("fable-standalone"|"standalone")::_ ->
    let minify = hasFlag "--no-minify" |> not
    buildStandalone {|minify=minify; watch=false|}
| ("fable-worker"|"worker")::_ ->
    let minify = hasFlag "--no-minify" |> not
    buildWorker {|minify=minify; watch=false|}
| "watch-standalone"::_ -> buildStandalone {|minify=false; watch=true|}


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
