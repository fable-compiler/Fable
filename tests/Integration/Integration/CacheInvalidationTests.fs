module Fable.Tests.CacheInvalidation

open System
open System.IO
open Expecto

/// A project whose compile order lives in an imported .props, so that adding a
/// source file leaves the .fsproj timestamp untouched. Written to a temp
/// directory because these tests have to mutate the project between compilations.
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

    // A second file so that deleting one output still leaves another behind: the
    // up-to-date check already forces a recompilation when it finds no generated
    // file at all, which would mask the case under test.
    File.WriteAllText(Path.Combine(dir, "Main.fs"), "module Main\n\nlet run () = printfn \"%s\" Lib.greeting\n")

/// The cache is invalidated by comparing timestamps, so a file rewritten within
/// the same tick as the previous compilation could be seen as unchanged. Nudge
/// the timestamp forward to keep the test from flaking.
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
            // A `<Compile Include>` arriving through an imported .props leaves every
            // .fsproj timestamp untouched, so the project options cache used to be
            // reused with a stale source list: the new module was never generated and
            // the build still reported success.
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

            // `getFilesToCompile` selects a source whose output is missing, so the
            // up-to-date check must not then report that same file as up-to-date and
            // skip the compilation it just asked for.
            testCase "Deleted output file is regenerated"
            <| fun () ->
                withTempProject
                    "deleted"
                    (fun dir outDir ->
                        Expect.equal (compile dir outDir) 0 "First compilation should succeed"

                        let outFile = Path.Combine(outDir, "Lib.js")
                        Expect.isTrue (File.Exists outFile) "Expected the first compilation to generate Lib.js"
                        File.Delete outFile

                        Expect.equal (compile dir outDir) 0 "Second compilation should succeed"
                        Expect.isTrue (File.Exists outFile) "The deleted output file should have been regenerated"
                    )
        ]
