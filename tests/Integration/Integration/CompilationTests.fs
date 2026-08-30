module Fable.Tests.CompilationTests

open System.IO
open System.Text.RegularExpressions
open Expecto
open Fable.Compiler.Util

let private data = Path.Combine(__SOURCE_DIRECTORY__, "data")

let private taskNetstandardPythonTest =
    testCase "Python task expressions use the project's netstandard FSharp.Core" <| fun () ->
        let testCaseDir = Path.Combine(__SOURCE_DIRECTORY__, "fixtures", "taskNetstandardPython")
        let project = Path.Combine(testCaseDir, "taskNetstandardPython.fsproj")
        let outputDir = Path.Combine(testCaseDir, "out")

        if Directory.Exists outputDir then
            Directory.Delete(outputDir, true)

        let configuration =
            System.Reflection.Assembly.GetExecutingAssembly().Location
            |> Path.GetDirectoryName
            |> Directory.GetParent
            |> _.Name

        let compiler =
            Path.GetFullPath(
                Path.Combine(__SOURCE_DIRECTORY__, $"../../../src/Fable.Cli/bin/%s{configuration}/net10.0/fable.dll")
            )

        let exitCode =
            Process.runSync
                testCaseDir
                "dotnet"
                [ compiler
                  project
                  "--cwd"
                  testCaseDir
                  "--lang"
                  "Python"
                  "--outDir"
                  outputDir
                  "--noCache" ]

        Expect.equal exitCode 0 "Expected the netstandard task project to compile"

        let output = File.ReadAllText(Path.Combine(outputDir, "task.py"))
        Expect.isFalse (output.Contains("raise 1")) "Task lowering must not emit the FCS recovery expression"
        Expect.stringContains output "task()" "Expected task-builder-generated Python"

/// `fable precompile` writes each chunk of inline expressions twice: the compact copy the CLI
/// reads, and the browser copy fable-standalone reads. This asks four things of every chunk:
///
/// 1. it is there at all, so a writer that silently stopped is caught
/// 2. it decodes, through the very code fable-standalone runs in a browser - this is what catches
///    a type the codec has no coder for
/// 3. re-encoding what was decoded gives the file back, so reading it lost nothing
/// 4. every expression in it matches the one the CLI reads from the compact copy, so the two
///    cannot drift apart
///
/// This runs on .NET, so it covers the codec and its coders but not the JavaScript backend.
/// It relies on Thoth.Json being cross-platform and implemented "correctly".
///
/// The worker harness under src/fable-standalone/test/worker covers that side.
let private checkBrowserInlineExprs (precompiledDir: string) =
    let fableModulesDir = Path.Combine(precompiledDir, "fable_modules")
    let compact = PrecompiledInfoImpl.Load fableModulesDir :> Fable.Transforms.State.PrecompiledInfo

    let chunks =
        Seq.initInfinite id
        |> Seq.map (fun i -> PrecompiledInfoImpl.GetBrowserInlineExprsPath(fableModulesDir, i))
        |> Seq.takeWhile File.Exists
        |> Seq.toList

    // 1. written at all
    Expect.isNonEmpty chunks "Expected precompile to write a browser copy of the inline expressions"

    for path in chunks do
        let json = File.ReadAllText path

        // 2. readable by the browser codec
        match Fable.BrowserInlineExprs.fromString json with
        | Error error -> failtestf "Cannot read %s: %s" path error
        | Ok exprs ->
            Expect.isNonEmpty exprs $"No inline expression in {path}"

            // 3. nothing lost on the way in
            Expect.equal (Fable.BrowserInlineExprs.toString exprs) json $"Reading {path} back lost something"

            // 4. the same expressions the CLI reads
            for name, expr in exprs do
                match compact.TryGetInlineExpr name with
                | None -> failtestf "%s is in %s but not in the compact copy" name path
                | Some fromCompact ->
                    Expect.equal
                        (Fable.BrowserInlineExprs.toString [| name, expr |])
                        (Fable.BrowserInlineExprs.toString [| name, fromCompact |])
                        $"%s{name} differs between the two copies"

let tests =
    Directory.EnumerateDirectories(data)
    |> Seq.map (fun testCaseDir -> //
        testCaseAsync
            testCaseDir
            (async {
                let project =
                    Directory.GetFileSystemEntries(testCaseDir, "*.fsproj") |> Seq.exactlyOne
                // clean up old actual files
                for f in Directory.GetFileSystemEntries(testCaseDir, "*.actual") do
                    File.Delete f

                let libraryDir = Path.Combine(testCaseDir, "library")
                let precompiledDir = Path.Combine(testCaseDir, "precompiled")

                let precompiledLibArgs =
                    // If we have a library folder, it means we want to use Fable precompiledLib feature for the test
                    if Directory.Exists libraryDir then
                        let libraryProject =
                            Directory.GetFileSystemEntries(libraryDir, "*.fsproj") |> Seq.exactlyOne

                        if Directory.Exists precompiledDir then
                            Directory.Delete(precompiledDir, true)

                        let exitCode =
                            Fable.Cli.Entry.main
                                [| "precompile"; libraryProject; "--outDir"; precompiledDir; "--noCache" |]

                        Expect.equal exitCode 0 "Expected precompile exit code to be 0"
                        checkBrowserInlineExprs precompiledDir
                        [| "--precompiledLib"; precompiledDir |]
                    else
                        [||]

                // Compile project
                let exitCode =
                    Array.append
                        [| project; "--cwd"; $"'%s{testCaseDir}'"; "-e"; ".jsx.actual" |]
                        precompiledLibArgs
                    |> Fable.Cli.Entry.main

                Expect.equal exitCode 0 "Expected exit code to be 0"

                let normalize content =
                    Regex.Replace(content, @"(/fable-library-js)[^/]+", "$1")
                    |> fun c -> Regex.Replace(c, @"[^""]*/precompiled/", "precompiled/")
                    |> _.ReplaceLineEndings()
                    |> _.Trim()

                for expected in Directory.GetFileSystemEntries(testCaseDir, "*.expected") do
                    let actual = Path.ChangeExtension(expected, ".actual")
                    Expect.isTrue (File.Exists actual) $"No actual file was produced for {expected}"
                    let expectedContent = File.ReadAllText expected |> normalize
                    let actualContent = File.ReadAllText actual |> normalize
                    Expect.equal actualContent expectedContent "The expected content differs from the actual content"

                return ()
            }))

    |> Seq.toList
    |> List.append [ taskNetstandardPythonTest ]
    |> testList "Compilation"
