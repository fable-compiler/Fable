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

    module private RunResult =

        let inline wasSuccess actual =
            match actual with
            | Ok _ -> ()
            | Error actual ->
                equal (Ok 0) (Error actual)

        let inline wasAnyError actual =
            match actual with
            | Error _ -> ()
            | Ok value ->
                equal (Error 0) (Ok value)

        let inline wasError expected actual =
            match actual with
            | Error _ when System.String.IsNullOrEmpty expected -> ()
            | Error actual -> equal (Error expected) (Error actual)
            | Ok value  -> equal (Error expected) (Ok value)

        let inline wasErrorContaining expected actual =
            match actual with
            | Error _ when System.String.IsNullOrEmpty expected -> ()
            | Error (actual: string) when actual.Contains expected -> ()
            | Error actual -> equal (Error expected) (Error actual)
            | Ok value  -> equal (Error expected) (Ok value)

    let inline private run (f: unit -> 'a) =
        try
            Ok (f())
        with e ->
            Error (e.Message)

    let inline throwsError (expected: string) (f: unit -> 'a): unit =
        run f |> RunResult.wasError expected

    /// While `throwsError` tests exception message for equality,
    /// this checks if error message CONTAINS passed message
    let inline throwsErrorContaining (expected: string) (f: unit -> 'a): unit =
        run f |> RunResult.wasErrorContaining expected

    let inline throwsAnyError (f: unit -> 'a): unit =
        run f |> RunResult.wasAnyError

    let inline doesntThrow (f: unit -> 'a): unit =
        run f |> RunResult.wasSuccess
