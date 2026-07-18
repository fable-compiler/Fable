module Build.Test.Beam

open Build.FableLibrary
open System.IO
open Build.Utils
open BlackFox.CommandLine
open SimpleExec

let private buildDir = Path.Resolve("temp", "tests", "Beam")
let private sourceDir = Path.Resolve("tests", "Beam")
let private testRunnerSrc = Path.Resolve("tests", "Beam", "erl_test_runner.erl")

// OTP app name derived from Fable.Tests.Beam.fsproj — computed the same way
// generateBeamScaffold does: replace dots/hyphens with underscores, lowercase.
let private testProjectName =
    Path.GetFileNameWithoutExtension("Fable.Tests.Beam.fsproj").Replace('.', '_').Replace('-', '_').ToLowerInvariant()

/// Collect the ebin directories rebar3 produced, for `-pa`.
let private ebinArgs (projectBuildDir: string) =
    let libDir = Path.Combine(projectBuildDir, "_build", "default", "lib")

    if Directory.Exists(libDir) then
        Directory.GetDirectories(libDir)
        |> Array.map (fun d -> Path.Combine(d, "ebin"))
        |> Array.filter Directory.Exists
        |> Array.map (fun d -> $"-pa \"{d}\"")
        |> String.concat " "
    else
        ""

/// Run an Erlang expression, returning its stdout and exit code.
///
/// Driven through `Process` rather than `Command.Run`/`Command.ReadAsync` for two reasons: those
/// throw on a non-zero exit, and a non-zero exit is exactly what one of these programs is supposed
/// to produce; and the assertions below compare non-ASCII text, which needs the child's stdout
/// decoded as UTF-8. `Command.ReadAsync` decodes with the ambient console encoding, which is UTF-8
/// on the Linux CI runner but the console codepage on Windows — mangling `✓` for a developer
/// running the suite locally, in a failure that says nothing about what actually went wrong.
let private runErl (workingDir: string) (paArgs: string) (expr: string) =
    let startInfo =
        System.Diagnostics.ProcessStartInfo(
            FileName = "erl",
            Arguments = $"-noshell {paArgs} -eval \"{expr}\" -s init stop",
            WorkingDirectory = workingDir,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            StandardOutputEncoding = System.Text.Encoding.UTF8,
            StandardErrorEncoding = System.Text.Encoding.UTF8
        )

    use proc = System.Diagnostics.Process.Start(startInfo)

    // Read both streams before waiting: a child that fills one pipe's buffer blocks forever if
    // we are waiting on exit instead of draining.
    let stdout = proc.StandardOutput.ReadToEndAsync()
    let stderr = proc.StandardError.ReadToEndAsync()
    proc.WaitForExit()

    // stderr is folded into the returned text so a crash report reaches the failure message.
    stdout.Result + stderr.Result, proc.ExitCode

let private expect (what: string) (expected: 'a) (actual: 'a) =
    if expected <> actual then
        failwith $"Entry point test: %s{what}\n  expected: %A{expected}\n  actual:   %A{actual}"

/// Assert that a program's output contains `substring`, showing the output when it does not.
/// `expect` on a bare `output.Contains ...` reports only `expected: true / actual: false`, which
/// leaves a mismatch undiagnosable without re-running the program by hand.
let private expectContains (what: string) (substring: string) (output: string) =
    if not (output.Contains substring) then
        failwith
            $"Entry point test: %s{what}\n  expected output to contain: %s{substring}\n  actual output:\n%s{output}"

/// Compile a whole program and run it on the BEAM through the generated `main.erl` shim.
///
/// The test suite calls test functions directly and so never executes the shim — which is how the
/// shim stayed broken while the suite was green. This checks the three things a program depends on:
/// that it runs at all, that argv reaches F#, and that the entry point's return value becomes the
/// process exit code.
let private testEntryPointPrograms () =
    let compile (name: string) =
        let programSourceDir = Path.Resolve("tests", "Beam", name)
        let programBuildDir = Path.Resolve("temp", "tests", "Beam" + name)
        Directory.clean programBuildDir

        Command.Fable(
            CmdLine.empty
            |> CmdLine.appendRaw programSourceDir
            |> CmdLine.appendPrefix "--outDir" programBuildDir
            |> CmdLine.appendPrefix "--lang" "beam"
            |> CmdLine.appendPrefix "--exclude" "Fable.Core"
            |> CmdLine.appendRaw "--noCache",
            workingDirectory = programBuildDir
        )

        Command.Run("rebar3", "compile", workingDirectory = programBuildDir)
        programBuildDir, ebinArgs programBuildDir

    printfn "Running entry point program tests..."

    let buildDir, paArgs = compile "Program"

    // An [<EntryPoint>] lowers to main/1: the program must run, and see its argv.
    let output, exitCode =
        runErl buildDir paArgs "main:main([\\\"alpha\\\", \\\"beta\\\"])"

    expectContains "argv length" "argc=2" output
    expectContains "argv contents" "argv=alpha,beta" output
    expect "exit code of a successful run" 0 exitCode

    // Both console paths must agree on encoding, under the plain `erl -noshell` above with no
    // extra flags: `Console.WriteLine` wrote raw UTF-8 bytes while `printfn` wrote unicode, so
    // whichever device encoding you picked, one of them mangled non-ASCII text.
    expectContains "Console.WriteLine encodes non-ASCII" "writeline=✓ ✗ · é" output
    expectContains "printfn encodes non-ASCII" "printfn=✓ ✗ · é" output
    expectContains "Console.WriteLine of a char" "char=✓" output

    // ...and its return value must become the exit code, or a failing suite looks like a passing one.
    let _, failExitCode = runErl buildDir paArgs "main:main([\\\"fail\\\"])"
    expect "exit code of a failing run" 3 failExitCode

    // main/0 forwards to main/1 with an empty argv.
    let output0, exitCode0 = runErl buildDir paArgs "main:main()"
    expectContains "argv of main/0" "argc=0" output0
    expect "exit code of main/0" 0 exitCode0

    // A program with no [<EntryPoint>] has only top-level effects, and the shim falls back to main/0.
    let noEntryBuildDir, noEntryPaArgs = compile "ProgramNoEntry"

    let noEntryOutput, noEntryExitCode =
        runErl noEntryBuildDir noEntryPaArgs "main:main()"

    expectContains "top-level effects ran" "no-entry-point program ran" noEntryOutput
    expect "exit code without an entry point" 0 noEntryExitCode

    printfn "Entry point program tests passed"

let handle (args: string list) =
    let isWatch = args |> List.contains "--watch"
    let noDotnet = args |> List.contains "--no-dotnet"
    let forceFableLibrary = args |> List.contains "--force-fable-library"

    BuildFableLibraryBeam().Run(forceFableLibrary)

    Directory.clean buildDir

    let fableArgs =
        CmdLine.concat
            [
                CmdLine.empty
                |> CmdLine.appendRaw sourceDir
                |> CmdLine.appendPrefix "--outDir" buildDir
                |> CmdLine.appendPrefix "--lang" "beam"
                |> CmdLine.appendPrefix "--exclude" "Fable.Core"
                |> CmdLine.appendRaw "--noCache"

                if isWatch then
                    CmdLine.empty |> CmdLine.appendRaw "--watch"
            ]

    if isWatch then
        Async.Parallel
            [
                if not noDotnet then
                    Command.RunAsync("dotnet", "watch test -c Release", workingDirectory = sourceDir)
                    |> Async.AwaitTask

                Command.WatchFableAsync(fableArgs, workingDirectory = buildDir)
                |> Async.AwaitTask
            ]
        |> Async.RunSynchronously
        |> ignore
    else
        // Test against .NET
        Command.Run("dotnet", "test -c Release", workingDirectory = sourceDir)

        // Compile tests to Erlang.
        // Fable automatically generates the rebar3 scaffold (rebar.config, .app.src,
        // per-dep rebar.config and ebin/*.app) alongside the .erl files in src/.
        Command.Fable(fableArgs, workingDirectory = buildDir)

        // Copy test runner and native Erlang helpers into src/ so rebar3 compiles them
        // as part of the project.
        let buildSrcDir = Path.Combine(buildDir, "src")

        File.Copy(testRunnerSrc, Path.Combine(buildSrcDir, "erl_test_runner.erl"), overwrite = true)

        let nativeErlDir = Path.Resolve("tests", "Beam", "erl")

        if Directory.Exists(nativeErlDir) then
            for erlFile in Directory.GetFiles(nativeErlDir, "*.erl") do
                File.Copy(erlFile, Path.Combine(buildSrcDir, Path.GetFileName(erlFile)), overwrite = true)

        // Compile everything with rebar3 using the generated scaffold.
        Command.Run("rebar3", "compile", workingDirectory = buildDir)

        // Collect all ebin directories produced by rebar3 for the -pa flags.
        let buildDefaultLib = Path.Combine(buildDir, "_build", "default", "lib")

        let ebinDirs =
            if Directory.Exists(buildDefaultLib) then
                Directory.GetDirectories(buildDefaultLib)
                |> Array.map (fun d -> Path.Combine(d, "ebin"))
                |> Array.filter Directory.Exists
            else
                [||]

        let paArgs = ebinDirs |> Array.map (fun d -> $"-pa \"{d}\"") |> String.concat " "

        // The test runner scans this directory for *_tests.beam modules.
        let projectEbinDir = Path.Combine(buildDefaultLib, testProjectName, "ebin")

        // Run Erlang test runner
        Command.Run(
            "erl",
            $"-noshell {paArgs} -eval \"erl_test_runner:main([\\\"{projectEbinDir}\\\"])\" -s init stop",
            workingDirectory = buildDir
        )

        // The suite above calls test functions directly and never runs a program end to end, so the
        // generated entry point shim is only exercised here.
        testEntryPointPrograms ()
