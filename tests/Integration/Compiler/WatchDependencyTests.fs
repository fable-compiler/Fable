module Fable.Tests.Compiler.WatchDependency

open System
open System.Threading
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private signature =
    """
module Library

val inline triple: x: int -> int
"""

let private library (factor: int) =
    $"""
module Library

let inline triple (x: int) = x * {factor}
"""

let private source =
    """
let six = Library.triple 2
"""

let private watchDependencies signature library source =
    Compiler.Cached.watchDependenciesWithSignedLibrary Compiler.Settings.standard signature library source

let private waitForOutput (fileName: string) (expected: string) =
    let deadline = DateTime.UtcNow + TimeSpan.FromSeconds 30.0
    let mutable output = Compiler.Cached.watchedOutput fileName

    while not (output.Contains expected) && DateTime.UtcNow < deadline do
        Thread.Sleep 200
        output <- Compiler.Cached.watchedOutput fileName

    if not (output.Contains expected) then
        failwith $"Timed out waiting for '{expected}' in {fileName}.js, last content:\n{output}"

let tests =
  testList "Watch Dependencies" [
    testCase "A file that inlines a function from a file with a signature depends on the .fs file" <| fun _ ->
      let logs, deps = watchDependencies signature (library 3) source

      logs |> Assert.Is.strictSuccess |> ignore
      deps |> Map.tryFind "Program.fs" |> Option.defaultValue [] |> equal [ "Library.fs" ]

    testCase "Editing an inline function in a file with a signature recompiles the files that inlined it" <| fun _ ->
      Compiler.Cached.startWatchingSignedLibrary Compiler.Settings.standard signature (library 3) source
      waitForOutput "Program.fs" "2 * 3"

      Compiler.Cached.rewriteWatchedLibrary (library 4)
      waitForOutput "Program.fs" "2 * 4"
  ]
