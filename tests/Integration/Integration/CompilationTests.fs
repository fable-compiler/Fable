module Fable.Tests.CompilationTests

open System.IO
open System.Text.RegularExpressions
open Expecto

let private data = Path.Combine(__SOURCE_DIRECTORY__, "data")

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
    |> testList "Compilation"
