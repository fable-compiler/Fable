namespace Fable.Compiler

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Fable
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker

type CompileResult =
    {
        /// A map of absolute file path to transpiled JavaScript
        CompiledFiles: Map<string, string>
        /// Diagnostics of the entire checked F# project
        Diagnostics: FSharpDiagnostic array
    }

type TypeCheckProjectResult =
    {
        Assemblies: FSharp.Compiler.Symbols.FSharpAssembly list
        ProjectCheckResults: FSharpCheckProjectResults
    }

[<RequireQualifiedAccess>]
module CodeServices =

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
            Async<AST.Fable.File>

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
