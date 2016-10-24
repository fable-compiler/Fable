[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Enumerable
open System
open System.Collections
open System.Collections.Generic
open NUnit.Framework
open Fable.Tests.Util

type Enumerator<'T>(gen) =
    interface IEnumerable<'T> with 
        member x.GetEnumerator() = gen()
    interface IEnumerable with 
        member x.GetEnumerator() = (gen() :> IEnumerator)

type Fibonacci() =
    let mutable state = (1,1)
    interface IEnumerator<int> with
        member x.Current = (fst state)
    interface IEnumerator with
        member x.Current = box (fst state)
        member x.MoveNext() = state <- (snd state, fst state + snd state); (fst state) < 1000
        member x.Reset() = state <- (1,1)
    interface System.IDisposable with 
        member x.Dispose() = ()

let fibGen() =
    let mutable state = (1,1)
    { new IEnumerator<_> with
        member x.Current = (fst state)
    interface IEnumerator with
        member x.Current = box (fst state)
        member x.MoveNext() = state <- (snd state, fst state + snd state); (fst state) < 1000
        member x.Reset() = state <- (1,1)
    interface System.IDisposable with 
        member x.Dispose() = () }

let toSeq gen = 
    { new IEnumerable<'T> with 
        member x.GetEnumerator() = gen() 
    interface IEnumerable with 
        member x.GetEnumerator() = (gen() :> IEnumerator) }

let fib() = Seq.unfold (fun state ->
    if (snd state < 1000) then
        Some (snd state, (snd state, fst state + snd state))
    else None) (1,1)

[<Test>]
let ``Enumerable class works``() =
    let f1 = Seq.toArray (fib())
    let f2 = Seq.toArray (Enumerator(fun () -> upcast new Fibonacci()))
    f1 = f2 |> equal true

[<Test>]
let ``Enumerable object expr works``() =
    let f1 = Seq.toArray (fib())
    let f2 = Seq.toArray (toSeq fibGen)
    f1 = f2 |> equal true
