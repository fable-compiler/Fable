module Fable.Tests.PluginsTests

open Fable.Core
open Fable.Tests
open Util.Testing

[<ReturnConstPlugin>]
let constFn () = 0

[<ReturnMemberNamePlugin>]
let memberNameFn () = ""

[<Erase>]
type ErasedWithCallPlugin [<ReturnMemberNamePlugin>] (a: int) =
    member _.A = a

let tests =
    testList
        "Member Declaration Plugins"
        [
            testCase "Transform replaces the compiled member body"
            <| fun () -> equal 42 (constFn ())

            testCase "TransformCall rewrites the call site"
            <| fun () -> equal "memberNameFn" (memberNameFn ())

            testCase "TransformCall rewrites a call to an erased constructor"
            <| fun () -> equal ".ctor" (box (ErasedWithCallPlugin(1)) :?> string)
        ]
