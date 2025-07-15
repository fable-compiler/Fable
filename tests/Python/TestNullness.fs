module Fable.Tests.Nullness

open System
open Util.Testing

type ABNull =
    | A
    | B of (string | null)

[<Fact>]
let ``test nullArgCheck`` () =
    let ex =
        try
            nullArgCheck "arg" null
        with e ->
            e

    equal "Value cannot be null. (Parameter 'arg')" ex.Message

[<Fact>]
let ``test Null active pattern works`` () =
    let getLength abnull =
        match abnull with
        | A -> 0
        | B Null -> 0
        | B(NonNull s) -> s.Length // `s` is derived to be `string`

    equal (getLength A) 0
    equal (getLength (B null)) 0

[<Fact>]
let ``test NonNull active pattern works`` () =
    let getLength abnull =
        match abnull with
        | A -> 0
        | B Null -> 0
        | B(NonNull s) -> s.Length // `s` is derived to be `string`

    equal (getLength (B "hello")) 5

[<Fact>]
let ``test works with generics`` () =
    // Generic code, note 'T must be constrained to be a reference type
    let findOrNull (index: int) (list: 'T list) : 'T | null when 'T: not struct =
        match List.tryItem index list with
        | Some item -> item
        | None -> null

    equal (findOrNull 1 [ "a"; "b"; "c" ]) "b"
    equal (findOrNull 3 [ "a"; "b"; "c" ]) null

[<Fact>]
let ``test Unchecked.nonNull works`` () =
    let toUpper (text: string | null) =
        (Unchecked.nonNull text).ToUpperInvariant()

    equal (toUpper "hello") "HELLO"