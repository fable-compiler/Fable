module Fable.Tests.Exception

open Fable.Tests.Util
open Util.Testing

exception MyError of string
exception MyError2 of code: int * message: string
exception EmptyError
exception NumericError of int

type ExitReason =
    | Stopped
    | Failed of string

exception ExitError of ExitReason
exception NamedNumericError of message: int

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
let ``test numeric exception Message is printable and preserves its payload`` () =
    let error = NumericError 42
    let message = error.Message
    message.Contains("42") |> equal true
    message.Split([| '\n' |]).Length > 0 |> equal true
    match error with
    | NumericError value -> value |> equal 42
    | _ -> failwith "Expected NumericError"

[<Fact>]
let ``test union exception Message is printable and preserves its payload`` () =
    let reason = Failed "worker stopped"
    let error = ExitError reason
    let message = error.Message
    message.Contains("worker stopped") |> equal true
    message.Split([| '\n' |]).Length > 0 |> equal true
    match error with
    | ExitError value -> value |> equal reason
    | _ -> failwith "Expected ExitError"

[<Fact>]
let ``test named numeric exception field stays numeric when Message is read`` () =
    let error = NamedNumericError 17
    error.Message.Contains("17") |> equal true
    match error with
    | NamedNumericError value -> value |> equal 17
    | _ -> failwith "Expected NamedNumericError"

[<Fact>]
let ``test aggregate exception formats non-string custom exception messages`` () =
    let error = ExitError(Failed "inner worker stopped")
    let aggregate = System.AggregateException("outer failure", [| error |])
    aggregate.Message.Contains("outer failure") |> equal true
    aggregate.Message.Contains("inner worker stopped") |> equal true
    obj.ReferenceEquals(error, aggregate.InnerException) |> equal true
    match aggregate.InnerException with
    | ExitError(Failed message) -> message |> equal "inner worker stopped"
    | _ -> failwith "Expected the original ExitError"

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

[<Fact>]
let ``test invalidArg formats the message like .NET`` () =
    throwsError "This is invalid (Parameter 'arg')" (fun () ->
        invalidArg "arg" "This is invalid"
    )
