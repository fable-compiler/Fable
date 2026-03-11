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
let ``test try-catch catches failwith`` () =
    let result =
        try
            failwith "boom"
            0
        with _ ->
            99
    result |> equal 99

[<Fact>]
let ``test exception message is accessible`` () =
    let msg =
        try
            failwith "test error"
            ""
        with e ->
            e.Message
    msg |> equal "test error"

[<Fact>]
let ``test failwith raises exception`` () =
    throwsAnyError (fun () -> failwith "kaboom")

[<Fact>]
let ``test doesntThrow works`` () =
    doesntThrow (fun () -> 42)

[<Fact>]
let ``test throwsAnyError with division by zero works`` () =
    throwsAnyError (fun () -> 1 / 0)

[<Fact>]
let ``test try-catch with wildcard ignoring exception produces no warnings`` () =
    // Regression: catch with wildcard `_` previously generated unused #{message => ...} map
    let result =
        try
            failwith "ignored"
            0
        with _ ->
            42
    result |> equal 42

[<Fact>]
let ``test reraise preserves exception`` () =
    // Regression: reraise() generated unbound MatchValue variable
    let msg =
        try
            try
                failwith "original"
                ""
            with _ ->
                reraise ()
        with e ->
            e.Message
    msg |> equal "original"

[<Fact>]
let ``test reraise in try-catch with side effect`` () =
    // Matches the Seq.fs pattern: try ... with _ -> sideEffect(); reraise()
    let mutable sideEffectRan = false
    let msg =
        try
            try
                failwith "boom"
                ""
            with _ ->
                sideEffectRan <- true
                reraise ()
        with e ->
            e.Message
    sideEffectRan |> equal true
    msg |> equal "boom"

[<Fact>]
let ``test try-catch result used in match`` () =
    // Regression: try/catch result fed into pattern match (the Seq.fs generateWhileSome pattern)
    let compute (x: int) : int option =
        if x > 0 then Some(x * 2) else None
    let result =
        match
            try
               compute 5
            with _ ->
                reraise ()
        with
        | None -> -1
        | Some v -> v
    result |> equal 10

[<Fact>]
let ``test nested try-catch works`` () =
    let result =
        try
            try
                failwith "inner"
                0
            with _ ->
                failwith "outer"
                0
        with e ->
            e.Message |> equal "outer"
            99
    result |> equal 99
