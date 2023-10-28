module Fable.Compiler.Service.Compiler

open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Compiler.Service.Util
open Fable.Compiler.Service.ProjectCracker

val mkCompilerForFile:
    sourceReader: SourceReader ->
    checker: InteractiveChecker ->
    cliArgs: CliArgs ->
    crackerResponse: CrackerResponse ->
    currentFile: string ->
        Async<Compiler>

val compileFile:
    sourceReader: SourceReader ->
    com: Compiler ->
    pathResolver: PathResolver ->
    outPath: string ->
        Async<string * string array>
