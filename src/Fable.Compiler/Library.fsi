module Fable.Compiler.CodeServices

open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Compiler.Util
open Fable.Compiler.ProjectCracker

/// Does a full type-check of the current project.
/// And compiles the implementation files to JavaScript.
val compileProjectToJavaScript:
    sourceReader: SourceReader ->
    checker: InteractiveChecker ->
    pathResolver: PathResolver ->
    cliArgs: CliArgs ->
    crackerResponse: CrackerResponse ->
        Async<Map<string, string>>

/// Type-checks the project up until the last transitive dependent file.
/// Compile the current and the transitive dependent files to JavaScript.
val compileFileToJavaScript:
    sourceReader: SourceReader ->
    checker: InteractiveChecker ->
    pathResolver: PathResolver ->
    cliArgs: CliArgs ->
    crackerResponse: CrackerResponse ->
    currentFile: string ->
        Async<Map<string, string>>
