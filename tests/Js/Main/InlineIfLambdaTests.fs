module Fable.Tests.InlineIfLambda

open Util.Testing

// ── helpers used by the tests ────────────────────────────────────────────────

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

// multi-arg InlineIfLambda
let inline applyPairTwice ([<InlineIfLambda>] f: int -> int -> int) a b =
    let first = f a b
    f first b

// ── CE builder that demonstrates [<InlineIfLambda>] on instance methods ──────
//
// `Delay` calls its thunk *twice* — once as a dry-run, once for the real result.
// Without [<InlineIfLambda>] the thunk would become a closure variable because
// `canInlineArg` blocks inlining when refCount > 1.  With [<InlineIfLambda>] the
// body is substituted at each call site so the side-effect (incrementing `log`)
// runs twice as expected and no closure is allocated.
type TwiceBuilder() =
    member inline _.Return(x) = x
    member inline _.Delay([<InlineIfLambda>] f: unit -> 'T) =
        let _ = f ()   // first call: side-effect runs here
        f ()           // second call: result comes from here
    member inline _.Run(x) = x

let twice = TwiceBuilder()

// ── tests ────────────────────────────────────────────────────────────────────

let tests =
    testList "InlineIfLambda" [

        testCase "Anonymous lambda is inlined across multiple uses" <| fun () ->
            let arr = [| 1..10 |]
            let mutable sum = 0
            arr |> iterateTwice (fun x -> sum <- sum + x)
            // each element visited twice → 2 * 55 = 110
            sum |> equal 110

        testCase "Named module-level lambda is inlined" <| fun () ->
            let arr = [| 1..10 |]
            let mutable sum = 0
            let myAction = fun x -> sum <- sum + x
            arr |> iterateTwice myAction
            sum |> equal 110

        testCase "InlineIfLambda works with fold-style accumulator" <| fun () ->
            let arr = [| 1..5 |]
            let sum = fold (fun acc x -> acc + x) 0 arr
            sum |> equal 15

        testCase "InlineIfLambda single application works" <| fun () ->
            let result = applyToFirst (fun x -> x * 2) 21
            result |> equal 42

        testCase "InlineIfLambda applied twice works" <| fun () ->
            let result = applyTwice (fun x -> x + 3) 1
            result |> equal 7

        testCase "InlineIfLambda with two parameters works" <| fun () ->
            let result =
                applyPairTwice
                    (fun a b ->
                        printfn "Adding %d and %d" a b
                        a + b
                    )
                    10
                    32
            result |> equal 74

        testCase "InlineIfLambda captures mutable correctly" <| fun () ->
            let mutable count = 0
            let arr = [| 1..5 |]
            // action captures mutable count
            arr |> iterateTwice (fun _ -> count <- count + 1)
            // 5 elements * 2 loops = 10 increments
            count |> equal 10

        testCase "InlineIfLambda with early return via exception-free branching" <| fun () ->
            let arr = [| 1..10 |]
            let mutable evens = 0
            arr |> iterateTwice (fun x -> if x % 2 = 0 then evens <- evens + 1)
            // 5 even numbers visited twice
            evens |> equal 10

        testCase "InlineIfLambda on CE builder Delay inlines body at each call site" <| fun () ->
            // `twice { ... }` desugars to `twice.Run(twice.Delay(fun () -> ...))`.
            // `Delay` calls its thunk twice, so the body runs twice.
            // With [<InlineIfLambda>] the thunk is inlined; without it `canInlineArg`
            // would keep a closure because refCount = 2.
            let mutable callCount = 0
            let result =
                twice {
                    callCount <- callCount + 1
                    return callCount
                }
            // Body ran twice: callCount = 2, return value = 2 (second call)
            callCount |> equal 2
            result   |> equal 2

    ]
