module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

// let log (o: obj) =
//    printfn "%A" o

let equal expected actual =
   let areEqual = expected = actual
   printfn "%A = %A > %b" expected actual areEqual
   if not areEqual then
       failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

// let throwsError (expected: string) (f: unit -> 'a): unit =
//    let success =
//        try
//            f () |> ignore
//            true
//        with e ->
//            if not <| String.IsNullOrEmpty(expected) then
//                equal e.Message expected
//            false
//    // TODO better error messages
//    equal false success

// let testCase (msg: string) f: unit =
//    try
//        printfn "%s" msg
//        f ()
//    with ex ->
//        printfn "%s" ex.Message
//        if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
//            printfn "%s" ex.StackTrace
//    printfn ""

// let testCaseAsync msg f =
//    testCase msg (fun () ->
//        async {
//            try
//                do! f ()
//            with ex ->
//                printfn "%s" ex.Message
//                if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
//                    printfn "%s" ex.StackTrace
//        } |> Async.StartImmediate)

// let measureTime (f: unit -> unit) = emitJsStatement () """
//    //js
//    const startTime = process.hrtime();
//    f();
//    const elapsed = process.hrtime(startTime);
//    console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
//    //!js
// """

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

open System.Collections.Generic

type [<Struct>]MemoizationKeyWrapper<'a> = MemoizationKeyWrapper of 'a

type MemoizeN =
    static member getOrAdd (cd: Dictionary<MemoizationKeyWrapper<'a>,'b>) (f: 'a -> 'b) k =
        match cd.TryGetValue (MemoizationKeyWrapper k) with
        | (true , v) -> v
        | (false, _) ->
            let v = f k
            cd.Add (MemoizationKeyWrapper k, v)
            v

let inline memoizeN (f:'``(T1 -> T2 -> ... -> Tn)``): '``(T1 -> T2 -> ... -> Tn)`` =
    let inline call_2 (a: ^MemoizeN, b: ^b) = ((^MemoizeN or ^b) : (static member MemoizeN : ^MemoizeN * 'b -> _ ) (a, b))
    call_2 (Unchecked.defaultof<MemoizeN>, Unchecked.defaultof<'``(T1 -> T2 -> ... -> Tn)``>) f

type MemoizeN with
    static member        MemoizeN (_: obj     , _:      'a -> 'b) = MemoizeN.getOrAdd (Dictionary ())
    static member inline MemoizeN (_: MemoizeN, _:'t -> 'a -> 'b) = MemoizeN.getOrAdd (Dictionary ()) << (<<) memoizeN

let effs = ResizeArray ()

let f x                       = effs.Add "f"; string x
let g x (y:string) z : uint32 = effs.Add "g"; uint32 (x * int y + int z)
let h x y z                   = effs.Add "h"; new System.DateTime (x, y, z)
let sum2 (a:int)       = effs.Add "sum2"; (+) a
let sum3 a (b:int) c   = effs.Add "sum3"; a + b + c
let sum4 a b c d : int = effs.Add "sum4"; a + b + c + d

// memoize them
let msum2 = memoizeN sum2
let msum3 = memoizeN sum3
let msum4 = memoizeN sum4
let mf    = memoizeN f
let mg    = memoizeN g
let mh    = memoizeN h

// check memoization really happens
let _v1  = msum2 1 1
let _v2  = msum2 1 1
let _v3  = msum2 2 1
let _v4  = msum3 1 2 3
let _v5  = msum3 1 2 3
let _v6  = msum4 3 1 2 3
let _v7  = msum4 3 1 2 3
let _v8  = msum4 3 5 2 3
let _v9  = mf 3M
let _v10 = mf 3M
let _v11 = mg 4 "2" 3M
let _v12 = mg 4 "2" 3M
let _v13 = mh 2010 1 1
let _v14 = mh 2010 1 1

equal [|"sum2"; "sum2"; "sum3"; "sum4"; "sum4"; "f"; "g"; "h"|] (effs.ToArray ())