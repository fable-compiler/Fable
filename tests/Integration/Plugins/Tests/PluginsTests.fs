module Fable.Tests.PluginsTests

open Fable.Tests
open Util.Testing

[<ReturnConstPlugin>]
let constFn () = 0

[<ReturnMemberNamePlugin>]
let memberNameFn () = ""

let tests =
    testList
        "Member Declaration Plugins"
        [
            testCase "Transform replaces the compiled member body"
            <| fun () -> equal 42 (constFn ())

            testCase "TransformCall rewrites the call site"
            <| fun () -> equal "memberNameFn" (memberNameFn ())
        ]
