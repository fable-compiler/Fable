module Fable.Tests.TryCatch

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test try-catch returns body value on success`` () =
    let result =
        try
            42
        with _ ->
            0
    result |> equal 42

[<Fact>]
let ``test try-catch catches division by zero`` () =
    let result =
        try
            let x = 1 / 0
            x
        with _ ->
            99
    result |> equal 99

[<Fact>]
let ``test doesntThrow works`` () =
    doesntThrow (fun () -> 42)

[<Fact>]
let ``test throwsAnyError with division by zero works`` () =
    throwsAnyError (fun () -> 1 / 0)
