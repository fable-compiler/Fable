module Fable.Tests.Main

#if FABLE_COMPILER

open Fable.Core

let [<Global>] describe (name: string) (f: unit->unit) = jsNative
let [<Global>] it (msg: string) (f: unit->unit) = jsNative

let run () =
    let tests = [ Arithmetic.tests
                ] :> _ seq
    for (name, tests) in tests do
        describe name (fun () ->
            for (msg, test) in tests do
                it msg test)
run()

#else

open Expecto

[<EntryPoint>]
let main args =
  testList "All" [ Arithmetic.tests
                 ]
  |> runTestsWithArgs defaultConfig args

#endif
