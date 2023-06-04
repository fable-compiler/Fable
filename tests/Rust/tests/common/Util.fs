module Fable.Tests.Util

module Testing =

#if FABLE_COMPILER
    open Fable.Core

    [<Emit("assert_eq!")>]
    let inline equal<'T> (expected: 'T) (actual: 'T): unit = nativeOnly
    [<Emit("assert_ne!")>]
    let inline notEqual<'T> (expected: 'T) (actual: 'T): unit = nativeOnly

    type FactAttribute() = inherit System.Attribute()
#else
    open Xunit
    type FactAttribute = Xunit.FactAttribute

    let inline equal<'T> (expected: 'T) (actual: 'T): unit = Assert.Equal(expected, actual)
    let inline notEqual<'T> (expected: 'T) (actual: 'T) : unit = Assert.NotEqual(expected, actual)
#endif

    let seqEqual (expected: seq<'T seq>) (actual: seq<'T seq>) =
        let a = actual |> Seq.map Seq.toArray |> Seq.toArray
        let e = expected |> Seq.map Seq.toArray |> Seq.toArray
        a = e |> equal true

#if NO_STD_NO_EXCEPTIONS
    [<AutoOpen>]
    // [<Fable.Core.Rust.OuterAttr("cfg", [|"feature = \"no_std\""|])>]
    module ExceptionUtils =
        let doesntThrow f = ()
        let throwsAnyError f = ()
        let throwsError (expected: string) f = ()
        let throwsErrorContaining (expected: string) f = ()
#else
    [<AutoOpen>]
    // [<Fable.Core.Rust.OuterAttr("cfg", [|"not(feature = \"no_std\")"|])>]
    module ExceptionUtils =
        let doesntThrow f =
            try
                f() |> ignore
                true
            with e ->
                false
            |> equal true

        let throwsAnyError f =
            try
                f() |> ignore
                false
            with e ->
                true
            |> equal true

        let throwsError (expected: string) f =
            try
                f() |> ignore
                false
            with e ->
                e.Message = expected
            |> equal true

        let throwsErrorContaining (expected: string) f =
            try
                f() |> ignore
                false
            with e ->
                e.Message.Contains(expected)
            |> equal true
#endif

// let rec sumFirstSeq (zs: seq<float>) (n: int): float =
//     match n with
//     | 0 -> 0.
//     | 1 -> Seq.head zs
//     | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

let f2 a b = a + b

// // Assignment block as expression outside a function is NOT optimized
// f2(let mutable x = 5 in let mutable y = 6 in x + y) |> ignore

let mutable a = 10

module B =
    let c = a
    a <- a + 5
    let mutable a = 20
    let d = f2 2 2
    let f2 a b = a - b

    module D =
        let d = a
        a <- a + 5
        let e = f2 2 2

// Test members with names conflicting with JS
let Int32Array = [|1;2|]

module Float64Array =
    let Float64Array = [|3.;4.|]

// type private R = { a: int }
// let mutable private x = 5

// // Check that variables with same name (the compiler generated `matchValue` here)
// // at module level don't conflict. See https://github.com/fable-compiler/Fable/issues/718#issuecomment-281533299
// match { a = 5 } with
// | { a = 3 } -> ()
// | _ -> x <- 4

// match { a = 2 } with
// | { a = 2 } -> x <- 2
// | _ -> x <- 4

module Foo =
    let update () = ()

module Bar =
    let rec nestedRecursive i = update (i+2)
    and update i = i + 5

let rec nonNestedRecursive s = update s
and update s = String.replicate 3 s

// let mutable mutableValue = 1
// let mutable mutableValueOpt = Some 1

// let getValueTimes2() = mutableValue * 2

// module Nested =
//     let mutable nestedMutableValue = "a"
//     let getValueTimes2() = nestedMutableValue + nestedMutableValue
//     let getOuterValueTimes4() = mutableValue * 4

// let getNestedValueTimes3() = Nested.nestedMutableValue + Nested.nestedMutableValue + Nested.nestedMutableValue
