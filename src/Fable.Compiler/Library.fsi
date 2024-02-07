module Fable.Compiler.CodeServices

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Diagnostics
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

/// Does a full type-check of the current project.
/// And compiles the implementation files to JavaScript.
val compileProjectToJavaScript:
    sourceReader: SourceReader ->
    checker: InteractiveChecker ->
    pathResolver: PathResolver ->
    cliArgs: CliArgs ->
    crackerResponse: CrackerResponse ->
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
