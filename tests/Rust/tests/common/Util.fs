module Fable.Tests.Util

module Testing =

#if FABLE_COMPILER
    open Fable.Core

    [<Emit("assert_eq!")>]
    let inline equal expected actual: unit = nativeOnly
    [<Emit("assert_ne!")>]
    let inline notEqual expected actual: unit = nativeOnly

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
