namespace Fable.Compiler

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Fable
open Fable.Transforms.State
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker

type FableASTResult =
    {
        /// The current file transformed into the Fable AST
        FableAST: AST.Fable.File
        /// Everything the compiler reported: the diagnostics of the F# project as checked up to
        /// the current file, tagged "FSHARP", followed by the logs Fable raised while
        /// transforming that file, tagged "FABLE".
        Logs: LogEntry array
    }

type CompileResult =
    {
        /// A map of absolute file path to transpiled JavaScript
        CompiledFiles: Map<string, string>
        /// Everything the compiler reported: the diagnostics of the entire checked F# project,
        /// tagged "FSHARP", followed by the logs Fable raised while translating the compiled
        /// files, tagged "FABLE".
        Logs: LogEntry array
    }

type TypeCheckProjectResult =
    {
        Assemblies: FSharp.Compiler.Symbols.FSharpAssembly list
        ProjectCheckResults: FSharpCheckProjectResults
    }

[<RequireQualifiedAccess>]
module CodeServices =

    /// Convert the diagnostics of the F# compiler into log entries tagged "FSHARP".
    /// The error number is folded into the message, as `Fable.Cli` prints it.
    val getFSharpDiagnostics: diagnostics: FSharpDiagnostic array -> LogEntry array

    /// Type check a project using the InteractiveChecker
    val typeCheckProject:
        sourceReader: SourceReader ->
        checker: InteractiveChecker ->
        cliArgs: CliArgs ->
        crackerResponse: CrackerResponse ->
            Async<TypeCheckProjectResult>

    /// Transform a file in a project to Fable.AST
    val compileFileToFableAST:
        sourceReader: SourceReader ->
        checker: InteractiveChecker ->
        cliArgs: CliArgs ->
        crackerResponse: CrackerResponse ->
        currentFile: string ->
            Async<FableASTResult>

    /// And compile multiple files of a project to JavaScript.
    /// The expected usage of this function is either every file in the project or only the user files.
    val compileMultipleFilesToJavaScript:
        pathResolver: PathResolver ->
        cliArgs: CliArgs ->
        crackerResponse: CrackerResponse ->
        typeCheckProjectResult: TypeCheckProjectResult ->
        inputFiles: string seq ->
            Async<CompileResult>

    /// Type-checks the project up until the last transitive dependent file.
    /// Compile the current and the transitive dependent files to JavaScript.
    val compileFileToJavaScript:
        sourceReader: SourceReader ->
        checker: InteractiveChecker ->
        pathResolver: PathResolver ->
        cliArgs: CliArgs ->
        crackerResponse: CrackerResponse ->
        currentFile: string ->
            Async<CompileResult>
