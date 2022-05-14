module Fable.Tests.Dart.Enumerable

open System.Collections
open System.Collections.Generic
open Util

type Enumerable<'T>(gen: unit -> IEnumerator<'T>) =
    interface System.IDisposable with
        member _.Dispose() = ()
    interface IEnumerable<'T> with
        member x.GetEnumerator() = gen()
        member x.GetEnumerator() = (gen() :> IEnumerator)
    static member Create(gen: unit -> IEnumerator<'T>) =
        new Enumerable<'T>(gen) :> IEnumerable<'T>

type Fibonacci() =
    let mutable state = (1,1)
    interface IEnumerator<int> with
        member x.Current = (fst state)
        member x.Current = box (fst state)
        member x.MoveNext() = state <- (snd state, fst state + snd state); (fst state) < 1000
        member x.Reset() = state <- (1,1)
    interface System.IDisposable with
        member x.Dispose() = ()

// let fibGen() =
//     let mutable state = (1,1)
//     { new IEnumerator<_> with
//         member x.Current = (fst state)
//     interface IEnumerator with
//         member x.Current = box (fst state)
//         member x.MoveNext() = state <- (snd state, fst state + snd state); (fst state) < 1000
//         member x.Reset() = state <- (1,1)
//     interface System.IDisposable with
//         member x.Dispose() = () }

let toSeq gen =
    Enumerable.Create(gen)

let fib() = Seq.unfold (fun state ->
    if (snd state < 1000) then
        Some (snd state, (snd state, fst state + snd state))
    else None) (1,1)

let mkEnumerable someList =
    Enumerable.Create(fun () ->
        (someList :> IEnumerable<_>).GetEnumerator())

let tests() =
    testCase "Enumerable class works" <| fun () ->
        let f1 = Seq.toArray (fib())
        let f2 = Seq.toArray (Enumerable.Create(fun () -> upcast new Fibonacci()))
        f1 = f2 |> equal true

    // TODO Object expr
    // testCase "Enumerable object expr works" <| fun () ->
    //     let f1 = Seq.toArray (fib())
    //     let f2 = Seq.toArray (toSeq fibGen)
    //     f1 = f2 |> equal true

    testCase ".NET Enumerator can be converted to JS iterator back and forth" <| fun () ->
        mkEnumerable [1..10]
        |> Seq.toList
        |> List.sum
        |> equal 55
