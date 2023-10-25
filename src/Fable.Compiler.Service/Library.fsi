module Fable.Compiler.Service.Compiler

open Fable
open Fable.Compiler.Service.Util
open Fable.Compiler.Service.ProjectCracker

val mkCompilerForFile: cliArgs: CliArgs -> crackerResponse: CrackerResponse -> currentFile: string -> Async<Compiler>
val compileFile: com: Compiler  -> pathResolver: PathResolver -> outPath: string -> Async<string * string array>
