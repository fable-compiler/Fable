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

    // let rec sumFirstSeq (zs: seq<float>) (n: int): float =
    //    match n with
    //    | 0 -> 0.
    //    | 1 -> Seq.head zs
    //    | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

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
