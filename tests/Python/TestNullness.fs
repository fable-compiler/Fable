module Fable.Tests.Nullness

open Util.Testing
open Fable.Tests.Util
open Fable.Core.PyInterop

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

[<Fact>]
let ``test Lists of nullable records works`` () =
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

[<Fact>]
let ``test Lists of nullable strings works`` () =
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

[<Fact>]
let ``test Nullable integer value works`` () =
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

[<Fact>]
let ``test Nullable string length works`` () =
    handleString1 "abc" |> equal 3
    handleString1 null |> equal 0
    handleString2 "abc" |> equal 3
    handleString2 null |> equal 0

[<Fact>]
let ``test Nullable string union works`` () =
    A |> handleUnion |> equal 0
    B null |> handleUnion |> equal 0
    B "cd" |> handleUnion |> equal 2

[<Fact>]
let ``test Nullable string check works`` () =
    argValidateShadowing "ABC" |> equal 3
    argValidateShadowing2 "ABC" |> equal 3
    automaticValidationViaActivePattern "ABC" |> equal 3

[<Fact>]
let ``test Nullable string to option works`` () =
    handleString3 "abc" |> Option.isSome |> equal true
    handleString3 null |> Option.isSome |> equal false

[<Fact>]
let ``test Nullable pattern matching works`` () =
    getStringLengthSafe "abc" |> equal 3
    getStringLengthSafe null |> equal -1
    getStringLengthSafe "" |> equal -1

[<Fact>]
let ``test Nullable type inferrence works`` () =
    getList() |> isNull |> equal false
    getList() |> nonNull |> List.length |> equal 2
    processNullableList [2;3] |> List.length |> equal 2
    processNullableList [1;2] |> List.sum |> equal 5
    processNullString "abc" |> equal 3
    processNullString null |> equal 0

[<Fact>]
let ``test nullArgCheck works`` () =
    nullArgCheck "arg" "ABC" |> equal "ABC"
    throwsAnyError (fun () -> nullArgCheck<string> "arg" null)

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
let ``test Nullness works with generics`` () =
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

#if FABLE_COMPILER
[<Fact>]
let ``test Nullness works for interop with undefined`` () =
    let maybeUndefined (value: string) : (string | null) = importMember "./py/nullness.py"

    match maybeUndefined "ok" with
    | NonNull _ -> equal true true
    | Null -> equal true false

    match maybeUndefined "foo" with
    | NonNull _ -> equal true false
    | Null -> equal true true

[<Fact>]
let ``test Nullness works for interop with null`` () =
    let maybeNull (value: string) : (string | null) = importMember "./py/nullness.py"

    match maybeNull "ok" with
    | NonNull _ -> equal true true
    | Null -> equal true false

    match maybeNull "foo" with
    | NonNull _ -> equal true false
    | Null -> equal true true
#endif
