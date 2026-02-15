module Fable.Tests.Exception

open Fable.Tests.Util
open Util.Testing

exception MyError of string
exception MyError2 of code: int * message: string
exception EmptyError

[<Fact>]
let ``test custom exception can be raised and caught`` () =
    let result =
        try
            raise (MyError "something went wrong")
            "no error"
        with
        | MyError msg -> msg
        | _ -> "unknown"
    result |> equal "something went wrong"

[<Fact>]
let ``test custom exception with multiple fields`` () =
    let result =
        try
            raise (MyError2 (42, "bad input"))
            "no error"
        with
        | MyError2 (code, msg) -> $"Error {code}: {msg}"
        | _ -> "unknown"
    result |> equal "Error 42: bad input"

[<Fact>]
let ``test custom exception type discrimination works`` () =
    let result =
        try
            raise (MyError "test")
            "no error"
        with
        | MyError2 _ -> "wrong type"
        | MyError msg -> msg
        | _ -> "unknown"
    result |> equal "test"

[<Fact>]
let ``test custom exception falls through to wildcard`` () =
    let result =
        try
            failwith "plain error"
            "no error"
        with
        | MyError _ -> "custom"
        | _ -> "wildcard"
    result |> equal "wildcard"

[<Fact>]
let ``test exception Message property with failwith`` () =
    let msg =
        try
            failwith "test message"
            ""
        with e ->
            e.Message
    msg |> equal "test message"

[<Fact>]
let ``test custom exception Message contains field value`` () =
    let msg =
        try
            raise (MyError "custom msg")
            ""
        with e ->
            e.Message
    // .NET formats as 'MyError "custom msg"', Beam uses the raw field value
    msg.Contains("custom msg") |> equal true

[<Fact>]
let ``test empty custom exception can be caught`` () =
    let result =
        try
            raise EmptyError
            "no error"
        with
        | EmptyError -> "caught"
        | _ -> "unknown"
    result |> equal "caught"

[<Fact>]
let ``test multiple exception types in same try-catch`` () =
    let test (exn: exn) =
        try
            raise exn
            "no error"
        with
        | MyError msg -> $"MyError: {msg}"
        | MyError2 (code, _) -> $"MyError2: {code}"
        | _ -> "other"
    test (MyError "hello") |> equal "MyError: hello"
    test (MyError2 (99, "err")) |> equal "MyError2: 99"

[<Fact>]
let ``test nested try-catch with custom exceptions`` () =
    let result =
        try
            try
                raise (MyError "inner")
                "no error"
            with
            | MyError msg ->
                raise (MyError2 (1, msg))
                "no error"
        with
        | MyError2 (code, msg) -> $"code={code}, msg={msg}"
        | _ -> "unknown"
    result |> equal "code=1, msg=inner"

[<Fact>]
let ``test custom exception in throwsAnyError`` () =
    throwsAnyError (fun () -> raise (MyError "boom"))
