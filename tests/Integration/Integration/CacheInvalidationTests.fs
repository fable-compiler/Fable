module Fable.Tests.CacheInvalidation

open System
open System.IO
open Expecto

/// A project whose compile order lives in an imported .props, so adding a source file
/// leaves the .fsproj timestamp untouched. In a temp dir: these tests mutate the project.
let private createProject (dir: string) =
    Directory.CreateDirectory(dir) |> ignore

    File.WriteAllText(
        Path.Combine(dir, "Test.fsproj"),
        """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <RollForward>Major</RollForward>
  </PropertyGroup>
  <Import Project="Tests.props" />
</Project>
"""
    )

    File.WriteAllText(
        Path.Combine(dir, "Tests.props"),
        """<Project>
  <ItemGroup>
    <Compile Include="$(MSBuildThisFileDirectory)Lib.fs" />
    <Compile Include="$(MSBuildThisFileDirectory)Main.fs" />
  </ItemGroup>
</Project>
"""
    )

    File.WriteAllText(Path.Combine(dir, "Lib.fs"), "module Lib\n\nlet greeting = \"hello\"\n")

    File.WriteAllText(Path.Combine(dir, "Main.fs"), "module Main\n\nlet run () = printfn \"%s\" Lib.greeting\n")

/// Cache invalidation compares timestamps, so a file rewritten within the same tick as the
/// previous compilation could look unchanged. Nudge it forward to keep the test from flaking.
let private writeNewer (path: string) (content: string) =
    File.WriteAllText(path, content)
    File.SetLastWriteTime(path, DateTime.Now.AddSeconds(2.0))

let private compile (dir: string) (outDir: string) =
    Fable.Cli.Entry.main
        [|
            Path.Combine(dir, "Test.fsproj")
            "--cwd"
            dir
            "--lang"
            "javascript"
            "--outDir"
            outDir
        |]

let private withTempProject name (f: string -> string -> unit) =
    let unique = Guid.NewGuid().ToString("N")
    let dir = Path.Combine(Path.GetTempPath(), $"fable-cache-tests-%s{name}-%s{unique}")

    try
        createProject dir
        f dir (Path.Combine(dir, "out"))
    finally
        try
            Directory.Delete(dir, true)
        with _ ->
            ()

let tests =
    testList
        "CacheInvalidation"
        [
            // A `<Compile Include>` arriving through an imported .props touches no .fsproj
            // timestamp, so the options cache used to be reused with a stale source list.
            testCase "Source file added via an imported .props is compiled"
            <| fun () ->
                withTempProject
                    "added"
                    (fun dir outDir ->
                        Expect.equal (compile dir outDir) 0 "First compilation should succeed"

                        writeNewer (Path.Combine(dir, "Scalars.fs")) "module Scalars\n\nlet tests = \"scalars\"\n"

                        writeNewer
                            (Path.Combine(dir, "Tests.props"))
                            """<Project>
  <ItemGroup>
    <Compile Include="$(MSBuildThisFileDirectory)Lib.fs" />
    <Compile Include="$(MSBuildThisFileDirectory)Scalars.fs" />
    <Compile Include="$(MSBuildThisFileDirectory)Main.fs" />
  </ItemGroup>
</Project>
"""

                        Expect.equal (compile dir outDir) 0 "Second compilation should succeed"

                        Expect.isTrue
                            (File.Exists(Path.Combine(outDir, "Scalars.js")))
                            "The file added through the imported .props should have been generated"
                    )

            testCase "A run after a compilation that failed with an F# error is not skipped"
            <| fun () ->
                withTempProject
                    "fsharp-error"
                    (fun dir outDir ->
                        let mainPath = Path.Combine(dir, "Main.fs")
                        File.WriteAllText(mainPath, "module Main\n\nlet run (x: int) : int = Undefined.thing x\n")

                        Expect.equal (compile dir outDir) 1 "First compilation should fail"
                        Expect.equal (compile dir outDir) 1 "Second compilation should fail too"

                        writeNewer mainPath "module Main\n\nlet run () = printfn \"%s\" Lib.greeting\n"

                        Expect.equal (compile dir outDir) 0 "Compilation should succeed once the error is fixed"
                    )

            testCase "A run after a compilation that failed with a Fable error is not skipped"
            <| fun () ->
                withTempProject
                    "fable-error"
                    (fun dir outDir ->
                        File.WriteAllText(
                            Path.Combine(dir, "Main.fs"),
                            "module Main\n\nlet run () = System.Exception().GetType().Name\n"
                        )

                        Expect.equal (compile dir outDir) 1 "First compilation should fail"
                        Expect.equal (compile dir outDir) 1 "Second compilation should fail too"
                    )

            testCase "A run after a successful compilation is skipped"
            <| fun () ->
                withTempProject
                    "skipped"
                    (fun dir outDir ->
                        let mainPath = Path.Combine(dir, "Main.fs")
                        let outPath = Path.Combine(outDir, "Main.js")
                        File.WriteAllText(mainPath, "module Main\n\nlet run (x: int) : int = Undefined.thing x\n")

                        Expect.equal (compile dir outDir) 1 "First compilation should fail"

                        writeNewer mainPath "module Main\n\nlet run () = printfn \"%s\" Lib.greeting\n"
                        Expect.equal (compile dir outDir) 0 "Compilation should succeed once the error is fixed"

                        File.SetLastWriteTime(outPath, DateTime.Now.AddSeconds(4.0))
                        let timestamp = File.GetLastWriteTime(outPath)

                        Expect.equal (compile dir outDir) 0 "Next compilation should succeed"

                        Expect.equal
                            (File.GetLastWriteTime(outPath))
                            timestamp
                            "The output file should not have been written again"
                    )

            testCase "A run after an interrupted compilation is not skipped"
            <| fun () ->
                withTempProject
                    "interrupted"
                    (fun dir outDir ->
                        let outPath = Path.Combine(outDir, "Main.js")
                        let marker = Path.Combine(outDir, "fable_modules", "compilation_unfinished")

                        Expect.equal (compile dir outDir) 0 "First compilation should succeed"
                        Expect.isFalse (File.Exists marker) "A successful compilation should leave no marker"

                        File.WriteAllText(marker, "")
                        File.SetLastWriteTime(outPath, DateTime.Now.AddSeconds(4.0))
                        let timestamp = File.GetLastWriteTime(outPath)

                        Expect.equal (compile dir outDir) 0 "Compilation after the interrupted one should succeed"

                        Expect.notEqual
                            (File.GetLastWriteTime(outPath))
                            timestamp
                            "The output file should have been written again"

                        Expect.isFalse (File.Exists marker) "The marker should have been removed"

                        File.SetLastWriteTime(outPath, DateTime.Now.AddSeconds(4.0))
                        let timestamp = File.GetLastWriteTime(outPath)

                        Expect.equal (compile dir outDir) 0 "Next compilation should succeed"

                        Expect.equal
                            (File.GetLastWriteTime(outPath))
                            timestamp
                            "The output file should not have been written again"
                    )
        ]
