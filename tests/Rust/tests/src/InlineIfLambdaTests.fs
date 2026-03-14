module Fable.Tests.InlineIfLambdaTests

open Util.Testing

// ── helpers ────────────────────────────────────────────────────────────────────

let inline iterateTwice ([<InlineIfLambda>] action) (array: 'T[]) =
    for i = 0 to array.Length - 1 do
        action array[i]
    for i = 0 to array.Length - 1 do
        action array[i]

let inline fold ([<InlineIfLambda>] folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[]) =
    let mutable acc = state
    for i = 0 to array.Length - 1 do
        acc <- folder acc array[i]
    acc

let inline applyToFirst ([<InlineIfLambda>] f: 'T -> 'U) (x: 'T) = f x

let inline applyTwice ([<InlineIfLambda>] f: 'T -> 'T) (x: 'T) =
    f (f x)

let inline applyPairTwice ([<InlineIfLambda>] f: int -> int -> int) a b =
    let first = f a b
    f first b

type TwiceBuilder() =
    member inline _.Return(x) = x
    member inline _.Delay([<InlineIfLambda>] f: unit -> 'T) =
        let _ = f ()
        f ()
    member inline _.Run(x) = x

let twice = TwiceBuilder()

// ── tests ──────────────────────────────────────────────────────────────────────

[<Fact>]
let ``Anonymous lambda is inlined across multiple uses`` () =
    let arr = [| 1..10 |]
    let mutable sum = 0
    arr |> iterateTwice (fun x -> sum <- sum + x)
    sum |> equal 110

[<Fact>]
let ``Named module-level lambda is inlined`` () =
    let arr = [| 1..10 |]
    let mutable sum = 0
    let myAction = fun x -> sum <- sum + x
    arr |> iterateTwice myAction
    sum |> equal 110

[<Fact>]
let ``InlineIfLambda works with fold-style accumulator`` () =
    let arr = [| 1..5 |]
    let sum = fold (fun acc x -> acc + x) 0 arr
    sum |> equal 15

[<Fact>]
let ``InlineIfLambda single application works`` () =
    let result = applyToFirst (fun x -> x * 2) 21
    result |> equal 42

[<Fact>]
let ``InlineIfLambda applied twice works`` () =
    let result = applyTwice (fun x -> x + 3) 1
    result |> equal 7

[<Fact>]
let ``InlineIfLambda with two parameters works`` () =
    let result =
        applyPairTwice
            (fun a b ->
                a + b
            )
            10
            32
    result |> equal 74

[<Fact>]
let ``InlineIfLambda captures mutable correctly`` () =
    let mutable count = 0
    let arr = [| 1..5 |]
    arr |> iterateTwice (fun _ -> count <- count + 1)
    count |> equal 10

[<Fact>]
let ``InlineIfLambda with early return via exception-free branching`` () =
    let arr = [| 1..10 |]
    let mutable evens = 0
    arr |> iterateTwice (fun x -> if x % 2 = 0 then evens <- evens + 1)
    evens |> equal 10

[<Fact>]
let ``InlineIfLambda on CE builder Delay inlines body at each call site`` () =
    let mutable callCount = 0
    let result =
        twice {
            callCount <- callCount + 1
            return callCount
        }
    callCount |> equal 2
    result |> equal 2
