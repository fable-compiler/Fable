module Fable.Tests.Compiler.CompilationOrderPreserved

open System
open Fable
open Fable.Cli.Main
open Fable.Tests.Compiler.Util.Compiler
open Expecto

let private projDir = IO.Path.Join(__SOURCE_DIRECTORY__, "A" ) |> Path.normalizeFullPath
let private projFile = IO.Path.Join(projDir, "A.fsproj" ) |> Path.normalizeFullPath

let test =
  testCase "Compilation Order Preserved" <| fun _ ->
    let result =
      State.Create { Cached.cliArgs with ProjectFile = projFile }
      |> startCompilation
      |> Async.RunSynchronously

    Expect.isOk result "Compiles successfully"

    (match result with
    | Error(_msg, logs) -> Array.toList logs
    | Ok(_, logs) -> Array.toList logs)
    |> Assert.Is.success
    |> ignore

[<EntryPoint>]
let main args =
    let config = { defaultConfig with runInParallel = false }

    runTestsWithArgs config args (testList "All" [ test ])
