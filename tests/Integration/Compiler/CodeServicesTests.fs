module Fable.Tests.Compiler.CodeServicesApi

open System
open Expecto
open Fable
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker
open Fable.Transforms.State
open FSharp.Compiler.SourceCodeServices

/// Drives `Fable.Compiler.CodeServices` directly, the way a host embedding the compiler does,
/// instead of going through `Fable.Cli.Main`.
module private Fixture =

    let private projDir =
        IO.Path.Join(__SOURCE_DIRECTORY__, "CodeServicesProject") |> Path.normalizeFullPath

    let private projFile =
        IO.Path.Join(projDir, "CodeServicesProject.fsproj") |> Path.normalizeFullPath

    let private sourceFile = IO.Path.Join(projDir, "Program.fs") |> Path.normalizeFullPath

    let private cliArgs =
        {
            CliArgs.ProjectFile = projFile
            // These tests only look at what the compiler reports, they never run the emitted
            // JavaScript, so point at a fixed directory instead of depending on a fable-library
            // build being present.
            FableLibraryPath = Some(IO.Path.Join(projDir, "fable_modules", "fable-library-js"))
            RootDir = projDir
            Configuration = "Debug"
            OutDir = None
            IsWatch = false
            Precompile = false
            PrecompiledLib = None
            PrintAst = false
            SourceMaps = false
            SourceMapsRoot = None
            NoRestore = false
            NoCache = false
            NoGitignore = false
            NoParallelTypeCheck = false
            Exclude = [ "Fable.Core" ]
            Replace = Map.empty
            RunProcess = None
            CompilerOptions = CompilerOptionsHelper.Make()
            Verbosity = Verbosity.Silent
        }

    /// Cracking is expensive and the project options never change, only the source does.
    let private crackerResponse =
        lazy
            (let resolver: ProjectCrackerResolver = Fable.Compiler.MSBuildCrackerResolver()
             CrackerOptions(cliArgs, false) |> getFullProjectOpts resolver)

    let private pathResolver =
        { new PathResolver with
            member _.TryPrecompiledOutPath(_sourceDir, _relativePath) = None
            member _.GetOrAddDeduplicateTargetDir(_importDir, _addTargetDir) = ""
        }

    /// Writes `source` as the single file of the project and hands back what the compiler needs.
    /// NOTE: NOT threadsafe, that file is rewritten on every call.
    let private prepare (source: string) =
        IO.File.WriteAllText(sourceFile, source)
        let crackerResponse = crackerResponse.Value

        let _, sourceReader =
            crackerResponse.ProjectOptions.SourceFiles
            |> Array.map Fable.Compiler.File
            |> Fable.Compiler.File.MakeSourceReader

        let checker = InteractiveChecker.Create(crackerResponse.ProjectOptions)
        crackerResponse, sourceReader, checker

    let compile (source: string) =
        let crackerResponse, sourceReader, checker = prepare source

        async {
            let! typeCheckResult =
                Fable.Compiler.CodeServices.typeCheckProject sourceReader checker cliArgs crackerResponse

            return!
                Fable.Compiler.CodeServices.compileMultipleFilesToJavaScript
                    pathResolver
                    cliArgs
                    crackerResponse
                    typeCheckResult
                    [ sourceFile ]
        }
        |> Async.RunSynchronously

    let compileToFableAST (source: string) =
        let crackerResponse, sourceReader, checker = prepare source

        Fable.Compiler.CodeServices.compileFileToFableAST
            sourceReader
            checker
            cliArgs
            crackerResponse
            sourceFile
        |> Async.RunSynchronously

/// `Async.RunSynchronously` type-checks fine but has no translation, so Fable reports it through
/// `addErrorAndReturnNull` and emits `return null`.
let private unsupportedByFable =
    """module Program

let blocking () = Async.RunSynchronously(async { return 42 })
"""

let private fsharpTypeError =
    """module Program

let wrong: int = "not an int"
"""

let tests =
    testList
        "CodeServices"
        [
            testCase "Fable errors are reported"
            <| fun _ ->
                let result = Fixture.compile unsupportedByFable

                let errors =
                    result.Logs |> Array.filter (fun log -> log.Severity = Severity.Error)

                Expect.exists
                    errors
                    (fun log -> log.Tag = "FABLE" && log.Message.Contains "not supported by Fable")
                    "Fable should report that Async.RunSynchronously is not supported"

            testCase "F# errors are reported"
            <| fun _ ->
                let result = Fixture.compile fsharpTypeError

                let errors =
                    result.Logs |> Array.filter (fun log -> log.Severity = Severity.Error)

                Expect.exists
                    errors
                    (fun log -> log.Tag = "FSHARP")
                    "The F# type error should be reported"

            testCase "Fable errors are reported when only transforming to the Fable AST"
            <| fun _ ->
                let result = Fixture.compileToFableAST unsupportedByFable

                let errors =
                    result.Logs |> Array.filter (fun log -> log.Severity = Severity.Error)

                Expect.exists
                    errors
                    (fun log -> log.Tag = "FABLE" && log.Message.Contains "not supported by Fable")
                    "Fable should report that Async.RunSynchronously is not supported"
        ]
