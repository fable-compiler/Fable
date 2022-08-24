module Fable.Tests.Compiler.CompilationOrderPreserved

open System
open Fable
open Fable.Cli.Main
open Util.Testing
open Fable.Tests.Compiler.Util.Compiler

let private projDir = IO.Path.Join(__SOURCE_DIRECTORY__, "A" ) |> Path.normalizeFullPath
let private projFile = IO.Path.Join(projDir, "A.fsproj" ) |> Path.normalizeFullPath

let tests =
  testCase "Compilation Order Preserved" <| fun _ ->
    let result =
      State.Create { Cached.cliArgs with ProjectFile = projFile }
      |> startCompilation
      |> Async.RunSynchronously

    Expecto.Expect.isOk result "Compiles successfully"

    (match result with
    | Error(_msg, logs) -> Array.toList logs
    | Ok(_, logs) -> Array.toList logs)
    |> Assert.Is.success
    |> ignore
