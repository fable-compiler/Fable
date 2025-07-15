module Fable.Tests.Nullness

open System
open Util.Testing
open Fable.Core.PyInterop

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

#if FABLE_COMPILER
[<Fact>]
let ``test works for interop with undefined`` () =
    let maybeUndefined (value: string) : (string | null) = importMember "./py/nullness.py"

    match maybeUndefined "ok" with
    | NonNull _ -> equal true true
    | Null -> equal true false

    match maybeUndefined "foo" with
    | NonNull _ -> equal true false
    | Null -> equal true true

[<Fact>]
let ``test works for interop with null`` () =
    let maybeNull (value: string) : (string | null) = importMember "./py/nullness.py"

    match maybeNull "ok" with
    | NonNull _ -> equal true true
    | Null -> equal true false

    match maybeNull "foo" with
    | NonNull _ -> equal true false
    | Null -> equal true true
#endif

[<Fact>]
let ``test Unchecked.nonNull works`` () =
    let toUpper (text: string | null) =
        (Unchecked.nonNull text).ToUpperInvariant()

    equal (toUpper "hello") "HELLO"