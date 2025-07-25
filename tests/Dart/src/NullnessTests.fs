module Fable.Tests.Dart.Nullness

open Util

type ABNull =
    | A
    | B of (string | null)

[<AutoOpen>]
module NullChecking =

    let getNullableValueOrDefault (x: System.Nullable<int>) =
        if x.HasValue then x.Value else 0

    let getNullableValue (x: System.Nullable<int>) =
        x.Value

    let handleString1 (s: string | null) =
        match s with
        | null -> 0
        | s -> s.Length

    let handleString2 (s: string | null) =
        match s with
        | NonNull s -> s.Length
        | Null -> 0

    let handleUnion abnull =
        match abnull with
        | A -> 0
        | B Null -> 0
        | B (NonNull s) -> s.Length

    let argValidateShadowing (arg1: string | null) =
        let arg1 = nullArgCheck (nameof arg1) arg1
        arg1.Length // no warning given, since `arg1` is not nullable anymore

    let argValidateShadowing2 (arg1: string | null) =
        let arg1 = nonNull arg1
        arg1.Length // no warning given, since `arg1` is not nullable anymore

    let automaticValidationViaActivePattern (NonNullQuick arg1: string | null) =
        arg1.Length

    let handleString3(s: string | null) = Option.ofObj s // returns `string option`

    let (|NullOrEmpty|NonEmpty|) s =
        match s with
        | Null | NonNull "" -> NullOrEmpty
        | NonNull s -> NonEmpty s

    let getStringLengthSafe s =
        match s with
        | NullOrEmpty -> -1
        | NonEmpty s -> s.Length

[<AutoOpen>]
module TypeInference =

    // type of `x` as well as the return value are `(string list)| null`. Nullness is inferred from the initial `x = null` assignment
    let getList() =
        let mutable x = null
        x <- ["a"; "b"]
        x

    // Signature is `(int list)| null -> int list`. Parameter nullness is inferred from `nullArgCheck function`
    // Return type is not-nullable, because body of the function handles nullness
    let processNullableList l =
        let l = nullArgCheck (nameof l) l
        l |> List.map (fun x -> x + 1)

    // Inferred to be `(string | null) -> int`. Parameter `s` is inferred nullable via the `(Null|NonNull)` active pattern usage
    let processNullString s =
        match s with
        | Null -> 0
        | NonNull s -> String.length s

type Person =
    { First: string
      Last: string }

let tests =
    testList
        "Nullable"
        [
            testCase "Lists of nullable records works" <| fun () ->
                let args: (Person | null) list =
                    [
                        { First = "A"; Last = "B" }
                        null
                        { First = "C"; Last = "D" }
                        { First = "E"; Last = "F" }
                        null
                    ]
                let nullCount = args |> List.filter isNull |> List.length
                nullCount |> equal 2

            testCase "Lists of nullable strings works" <| fun () ->
                let args: (string | null * bool) list =
                    [
                        ("", true)
                        (null, true)
                        ("test", false)
                        (" \t", true)
                        (null, false)
                    ]
                let nullCount = args |> List.filter (fst >> isNull) |> List.length
                nullCount |> equal 2

            testCase "Nullable integer value works" <| fun () ->
                let i_null = System.Nullable<int>()
                let i_five = System.Nullable(5)
                i_null |> getNullableValueOrDefault |> equal 0
                i_five |> getNullableValueOrDefault |> equal 5
                i_five |> getNullableValue |> equal 5
                i_five |> nonNullV |> equal 5
                i_five |> isNullV |> equal false
                i_null |> isNullV |> equal true
                nullV<int> |> isNullV |> equal true
                (withNullV 42).HasValue |> equal true

            testCase "Nullable string length works" <| fun () ->
                handleString1 "abc" |> equal 3
                handleString1 null |> equal 0
                handleString2 "abc" |> equal 3
                handleString2 null |> equal 0

            testCase "Nullable string union works" <| fun () ->
                A |> handleUnion |> equal 0
                B null |> handleUnion |> equal 0
                B "cd" |> handleUnion |> equal 2

            testCase "Nullable string check works" <| fun () ->
                argValidateShadowing "ABC" |> equal 3
                argValidateShadowing2 "ABC" |> equal 3
                automaticValidationViaActivePattern "ABC" |> equal 3

            testCase "Nullable string to option works" <| fun () ->
                handleString3 "abc" |> Option.isSome |> equal true
                handleString3 null |> Option.isSome |> equal false

            testCase "Nullable pattern matching works" <| fun () ->
                getStringLengthSafe "abc" |> equal 3
                getStringLengthSafe null |> equal -1
                getStringLengthSafe "" |> equal -1

            testCase "Nullable type inferrence works" <| fun () ->
                getList() |> isNull |> equal false
                getList() |> nonNull |> List.length |> equal 2
                processNullableList [2;3] |> List.length |> equal 2
                processNullableList [1;2] |> List.sum |> equal 5
                processNullString "abc" |> equal 3
                processNullString null |> equal 0

            testCase "nullArgCheck works" <| fun () ->
                nullArgCheck "arg" "ABC" |> equal "ABC"
                throwsAnyError (fun () -> nullArgCheck<string> "arg" null)

            testCase "Null active pattern works" <| fun () ->
                let getLength abnull =
                    match abnull with
                    | A -> 0
                    | B Null -> 0
                    | B(NonNull s) -> s.Length // `s` is derived to be `string`

                equal (getLength A) 0
                equal (getLength (B null)) 0

            testCase "NonNull active pattern works" <| fun () ->
                let getLength abnull =
                    match abnull with
                    | A -> 0
                    | B Null -> 0
                    | B(NonNull s) -> s.Length // `s` is derived to be `string`

                equal (getLength (B "hello")) 5

            testCase "Nullness works with generics" <| fun () ->
                // Generic code, note 'T must be constrained to be a reference type
                let findOrNull (index: int) (list: 'T list) : 'T | null when 'T: not struct =
                    match List.tryItem index list with
                    | Some item -> item
                    | None -> null

                equal (findOrNull 1 [ "a"; "b"; "c" ]) "b"
                equal (findOrNull 3 [ "a"; "b"; "c" ]) null

            testCase "Unchecked.nonNull works" <| fun () ->
                let toUpper (text: string | null) =
                    (Unchecked.nonNull text).ToUpperInvariant()

                equal (toUpper "hello") "HELLO"

// #if FABLE_COMPILER
//             testCase "Nullness works for interop with undefined" <| fun () ->
//                 let maybeUndefined (value: string) : (string | null) = importMember "./js/nullness.js"

//                 match maybeUndefined "ok" with
//                 | NonNull _ -> equal true true
//                 | Null -> equal true false

//                 match maybeUndefined "foo" with
//                 | NonNull _ -> equal true false
//                 | Null -> equal true true

//             testCase "Nullness works for interop with null" <| fun () ->
//                 let maybeNull (value: string) : (string | null) = importMember "./js/nullness.js"

//                 match maybeNull "ok" with
//                 | NonNull _ -> equal true true
//                 | Null -> equal true false

//                 match maybeNull "foo" with
//                 | NonNull _ -> equal true false
//                 | Null -> equal true true
// #endif
        ]
