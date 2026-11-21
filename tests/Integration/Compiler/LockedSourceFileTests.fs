module Fable.Tests.Compiler.LockedSourceFile

open System
open System.Diagnostics
open System.IO
open System.Threading
open Expecto
open FSharp.Compiler.CodeAnalysis
open Fable
open Fable.Cli.Main
open Fable.Compiler.ProjectCracker
open Fable.Compiler.Util
open Util.Testing

let private source = "module Library\n"

let private withSourceFile (test: string -> unit) =
    let dir = Directory.CreateTempSubdirectory("fable-locked-source-").FullName

    try
        let path = Path.Combine(dir, "Library.fs")
        File.WriteAllText(path, source)
        test path
    finally
        Directory.Delete(dir, true)

let private lockFile (path: string) =
    new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None)

let private releaseAfter (delayMs: int) (fileLock: FileStream) =
    let thread =
        Thread(fun () ->
            Thread.Sleep delayMs
            fileLock.Dispose()
        )

    thread.Start()
    thread

let private skipUnlessChmodBlocksReads (path: string) =
    if OperatingSystem.IsWindows() then
        skiptest "Unix file modes are not available on Windows"

    IO.File.SetUnixFileMode(path, UnixFileMode.None)

    let blocked =
        try
            IO.File.ReadAllText path |> ignore
            false
        with :? UnauthorizedAccessException ->
            true

    if not blocked then
        IO.File.SetUnixFileMode(path, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)
        skiptest "This user can read a file with mode 000"

let private restoreModeAfter (delayMs: int) (path: string) =
    let thread =
        Thread(fun () ->
            Thread.Sleep delayMs
            IO.File.SetUnixFileMode(path, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)
        )

    thread.Start()
    thread

let private projectCracked (files: string array) =
    let cliArgs: CliArgs =
        {
            ProjectFile = "Test.fsproj"
            FableLibraryPath = None
            RootDir = IO.Path.GetTempPath()
            Configuration = "Debug"
            OutDir = None
            IsWatch = true
            Precompile = false
            PrecompiledLib = None
            PrintAst = false
            SourceMaps = false
            SourceMapsRoot = None
            NoRestore = false
            NoCache = false
            NoGitignore = false
            NoParallelTypeCheck = false
            Exclude = []
            Replace = Map.empty
            RunProcess = None
            CompilerOptions = CompilerOptionsHelper.Make()
            Verbosity = Verbosity.Normal
        }

    let response: CrackerResponse =
        {
            FableLibDir = ""
            FableModulesDir = ""
            References = []
            ProjectOptions =
                {
                    ProjectFileName = "Test.fsproj"
                    ProjectId = None
                    SourceFiles = files
                    OtherOptions = [||]
                    ReferencedProjects = [||]
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime.UtcNow
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = None
                }
            OutputType = OutputType.Library
            TargetFramework = None
            PrecompiledInfo = None
            CanReuseCompiledFiles = false
            TreatWarningsAsErrors = false
        }

    ProjectCracked(cliArgs, response, files |> Array.map Fable.Compiler.File)

let tests =
  testList "Locked source file" [
    testCase "ReadSource waits for a short lock to be released" <| fun _ ->
      withSourceFile <| fun path ->
        let release = lockFile path |> releaseAfter 100
        let _, text = Fable.Compiler.File(path).ReadSource()
        release.Join()
        text.Value |> equal source

    testCase "ReadSource waits for a short lock when the hash is already cached" <| fun _ ->
      withSourceFile <| fun path ->
        let file = Fable.Compiler.File(path)
        file.ReadSource() |> ignore
        let release = lockFile path |> releaseAfter 500
        let _, text = file.ReadSource()
        let value = text.Value
        release.Join()
        value |> equal source

    testCase "ReadSource waits for a short permission denial" <| fun _ ->
      withSourceFile <| fun path ->
        skipUnlessChmodBlocksReads path
        let restore = restoreModeAfter 500 path

        try
            let _, text = Fable.Compiler.File(path).ReadSource()
            text.Value |> equal source
        finally
            restore.Join()

    testCase "ReadSource retries before raising when the lock outlasts the retries" <| fun _ ->
      withSourceFile <| fun path ->
        use _fileLock = lockFile path
        let stopwatch = Stopwatch.StartNew()

        let ex =
            Expect.throwsC
                (fun () -> Fable.Compiler.File(path).ReadSource() |> ignore)
                id

        stopwatch.Stop()
        Expect.isTrue (ex :? IOException) $"Expected an IOException, got {ex}"
        Expect.stringContains ex.Message path "The error names the locked file"
        Expect.isGreaterThanOrEqual stopwatch.ElapsedMilliseconds 1500L "The read retries before giving up"

    testCase "DirectiveDiagnostics reports a locked file and still checks the others" <| fun _ ->
      withSourceFile <| fun path ->
        let other = IO.Path.Combine(IO.Path.GetDirectoryName path, "Other.fs")
        IO.File.WriteAllText(other, "module Other\n// fable-disable-next-line\nlet x = 1\n")
        use _fileLock = lockFile path

        let logs = (projectCracked [| path; other |]).DirectiveDiagnostics [ path; other ]

        logs |> Array.map _.FileName |> equal [| Some path; Some other |]
        logs |> Array.iter (fun log -> equal Severity.Warning log.Severity)
        Expect.stringContains logs[0].Message path "The warning names the locked file"
  ]
