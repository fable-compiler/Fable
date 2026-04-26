module Fable.Tests.CompilationTests

open System
open System.IO
open System.Text.RegularExpressions
open Expecto

let private data = Path.Combine(__SOURCE_DIRECTORY__, "data")

// Set UPDATE_SNAPSHOTS=true to regenerate *.expected files instead of comparing.
let private updateSnapshots =
    Environment.GetEnvironmentVariable("UPDATE_SNAPSHOTS") = "true"

let private normalize (content: string) =
    Regex.Replace(content, @"(/fable-library-(?:js|ts))[^/]+", "$1")
    |> _.ReplaceLineEndings()
    |> _.Trim()

let private compileAndCheck
    (testCaseDir: string)
    (project: string)
    (extension: string)
    (expectedGlob: string)
    (extraArgs: string list)
    =
    async {
        let actualExt = $"{extension}.actual"
        let args =
            [| yield project; yield "--cwd"; yield $"'%s{testCaseDir}'"; yield "-e"; yield actualExt; yield! extraArgs |]

        let exitCode = Fable.Cli.Entry.main args
        Expect.equal exitCode 0 "Expected exit code to be 0"

        for expected in Directory.GetFileSystemEntries(testCaseDir, expectedGlob) do
            let actual = Path.ChangeExtension(expected, ".actual")
            Expect.isTrue (File.Exists actual) $"No actual file was produced for {expected}"
            let actualContent = File.ReadAllText actual |> normalize

            if updateSnapshots then
                File.WriteAllText(expected, actualContent)
            else
                let expectedContent = File.ReadAllText expected |> normalize
                Expect.equal actualContent expectedContent "The expected content differs from the actual content"
    }

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

                // Compile and check JavaScript output
                do!
                    compileAndCheck testCaseDir project ".jsx" "*.jsx.expected" []

                // Compile and check TypeScript output (only when *.tsx.expected files exist)
                let hasTsExpected =
                    Directory.GetFileSystemEntries(testCaseDir, "*.tsx.expected").Length > 0

                if hasTsExpected then
                    do!
                        compileAndCheck
                            testCaseDir
                            project
                            ".tsx"
                            "*.tsx.expected"
                            [ "--lang"; "typescript" ]

                return ()
            }))

    |> Seq.toList
    |> testList "Compilation"
