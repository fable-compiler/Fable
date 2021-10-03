
module Adaptive

// Moved from ApplicativeTests to isolate dependency on FSharp.Data.Adaptive.
// FSharp.Data.Adaptive includes FableHelpers.fs, which has an implementation
// for Queue which now collides with Fable's own native implementation.

// When we can patch FSharp.Data.Adaptive to conditionally compile its own
// Queue implementation, we can re-enable these tests (see also Fable.Tests.fsproj)

#if FABLE_DEFINES_QUEUE
let tests = []
#else
open FSharp.Data.Adaptive

let simple () =
    let x = AVal.init 3
    let mappedX = x |> AVal.map (fun x' -> x' + 1)
    let adaptiveList =
        mappedX
        |> AList.single
        |> AList.mapA id
    let firstForce = adaptiveList |> AList.force
    transact (fun () -> x.Value <- 0)
    let secondForce = adaptiveList |> AList.force
    firstForce, secondForce

let tests = [
    testCase "FSharp.Data.Adaptive works" <| fun () -> // See #2291
        let first, second = simple ()
        equal 4 first.[0]
        equal 1 second.[0]
]
#endif