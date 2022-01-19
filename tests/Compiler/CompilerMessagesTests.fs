module Fable.Tests.Compiler.CompilerMessages

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

let tests =
  testList "Compiler Messages" [
    testCase "Compile Console.WriteLine" <| fun _ ->
      let source = "Console.WriteLine(\"Hello World\")"
      compile source
      |> Assert.Is.success
      |> ignore
    testCase "Compile printfn" <| fun _ ->
      let source = "printfn \"Hello %s\" \"World\""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Compiling printfn with %s placeholder and int argument results in error" <| fun _ ->
      let source = "printfn \"Hello %s\" 42"
      compile source
      |> Assert.Is.Single.error
      |> ignore
    testCase "Compiling incomplete pattern match results in warning" <| fun _ ->
      let source = "match None with | Some n -> 42 |> ignore" // without `ignore`: Warning: Result of Expression is implicitly ignored
      compile source
      |> Assert.Is.Single.warning
      |> ignore

    testCase "Compiling printfn with %s placeholder and int argument results in specific error" <| fun _ ->
      let source = "printfn \"Hello %s\" 42"
      compile source
      |> Assert.Exists.errorWith "This expression was expected to have type"
      |> ignore
    testCase "Compiling incomplete pattern match results in specific warning" <| fun _ ->
      let source = "match None with | Some n -> 42"
      compile source
      |> Assert.Is.success
      |> Assert.Exists.warningWith "Incomplete pattern matches on this expression"
      |> Assert.Exists.warningWith "The result of this expression has type 'int' and is implicitly ignored."
      |> ignore
  ]
