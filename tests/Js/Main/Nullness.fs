module Fable.Tests.Nullness

open Util.Testing
open Fable.Core.JsInterop

type ABNull =
    | A
    | B of (string | null)

let tests =
    testList
        "Nullness"
        [
            testCase
                "nullArgCheck"
                (fun () ->
                    let ex =
                        try
                            nullArgCheck "arg" null
                        with e ->
                            e

                    equal "Value cannot be null. (Parameter 'arg')" ex.Message
                )

            testCase "Null active pattern works"
            <| fun () ->
                let getLength abnull =
                    match abnull with
                    | A -> 0
                    | B Null -> 0
                    | B(NonNull s) -> s.Length // `s` is derived to be `string`

                equal (getLength A) 0
                equal (getLength (B null)) 0

            testCase "NonNull active pattern works"
            <| fun () ->
                let getLength abnull =
                    match abnull with
                    | A -> 0
                    | B Null -> 0
                    | B(NonNull s) -> s.Length // `s` is derived to be `string`

                equal (getLength (B "hello")) 5

            testCase "works with generics"
            <| fun _ ->
                // Generic code, note 'T must be constrained to be a reference type
                let findOrNull (index: int) (list: 'T list) : 'T | null when 'T: not struct =
                    match List.tryItem index list with
                    | Some item -> item
                    | None -> null

                equal (findOrNull 1 [ "a"; "b"; "c" ]) "b"
                equal (findOrNull 3 [ "a"; "b"; "c" ]) null

#if FABLE_COMPILER
            testCase "works for interop with undefined"
            <| fun () ->
                let maybeUndefined (value: string) : (string | null) = importMember "./js/nullness.js"

                match maybeUndefined "ok" with
                | NonNull _ -> equal true true
                | Null -> equal true false

                match maybeUndefined "foo" with
                | NonNull _ -> equal true false
                | Null -> equal true true

            testCase "works for interop with null"
            <| fun () ->
                let maybeNull (value: string) : (string | null) = importMember "./js/nullness.js"

                match maybeNull "ok" with
                | NonNull _ -> equal true true
                | Null -> equal true false

                match maybeNull "foo" with
                | NonNull _ -> equal true false
                | Null -> equal true true
#endif

            testCase "Unchecked.nonNull works"
            <| fun () ->
                let toUpper (text: string | null) =
                    (Unchecked.nonNull text).ToUpperInvariant()

                equal (toUpper "hello") "HELLO"
        ]
