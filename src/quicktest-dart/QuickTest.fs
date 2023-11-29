module QuickTest

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open Fable.Core
open Fable.Core.Dart

let equal expected actual =
    let areEqual = expected = actual
    print $"{expected} = {actual} > {areEqual}"

    if not areEqual then
        print $"[ASSERT ERROR] Expected {expected} but got {actual}"
        exn "" |> raise

let testCase (msg: string) f : unit =
    print msg
    f ()
    print ""

let testList (msg: string) (xs: unit list) : unit = ()

let throwsAnyError (f: unit -> 'a) : unit =
    let success =
        try
            f () |> ignore
            true
        with e ->
            print $"Got expected error: %s{string<exn> e}"
            false

    if success then
        print "[ERROR EXPECTED]"

let main () =
    testCase "Array.length works"
    <| fun () ->
        let xs =
            [|
                "a"
                "a"
                "a"
                "a"
            |]

        Array.length xs |> equal 4
