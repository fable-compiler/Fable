module Fable.Compiler.Service.ProjectCracker

open FSharp.Compiler.CodeAnalysis
open Fable
open Fable.AST
open Fable.Compiler.Service.Util

type CrackerResponse =
    { FableLibDir: string
      FableModulesDir: string
      References: string list
      ProjectOptions: FSharpProjectOptions
      OutputType: OutputType
      TargetFramework: string
      PrecompiledInfo: PrecompiledInfoImpl option
      CanReuseCompiledFiles: bool }
