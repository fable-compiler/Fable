module Fable.Tests.CompilationTests

open System.IO
open Expecto

let private data = Path.Combine(__SOURCE_DIRECTORY__, "data")

let tests =
    Directory.EnumerateDirectories(data)
    |> Seq.map (fun testCaseDir -> //
        testCaseAsync
            (Path.GetDirectoryName(testCaseDir))
            (async {
                let project =
                    Directory.GetFileSystemEntries(testCaseDir, "*.fsproj") |> Seq.exactlyOne
                // clean up old actual files
                for f in Directory.GetFileSystemEntries(testCaseDir, "*.actual") do
                    File.Delete f

                // Compile project
                let exitCode =
                    Fable.Cli.Entry.main [| project; "--cwd"; "$\"{testCaseDir}\""; "-e"; ".js.actual" |]

                Expect.equal exitCode 0 "Expected exit code to be 0"

                for expected in Directory.GetFileSystemEntries(testCaseDir, "*.expected") do
                    let actual = Path.ChangeExtension(expected, ".actual")
                    Expect.isTrue (File.Exists actual) $"No actual file was produced for {expected}"
                    let expectedContent = File.ReadAllText expected
                    let actualContent = File.ReadAllText actual
                    Expect.equal actualContent expectedContent "The expected content differs from the actual content"

                return ()
            }))

    |> Seq.toList
    |> testList "Compilation"
