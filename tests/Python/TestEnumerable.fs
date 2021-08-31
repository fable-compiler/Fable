module Fable.Tests.Enumerable

open System.Collections
open System.Collections.Generic
open Util.Testing

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

let mkEnumerable someList =
    { new IEnumerable<'T> with
        member x.GetEnumerator() = (someList :> IEnumerable<_>).GetEnumerator() // <-- javascript runtime error here
      interface System.Collections.IEnumerable with
        member x.GetEnumerator() = ((someList :> IEnumerable<_>).GetEnumerator() :> System.Collections.IEnumerator) }

[<Fact>]
let ``test Enumerable class works`` () =
    let f1 = Seq.toArray (fib())
    let f2 = Seq.toArray (Enumerator(fun () -> upcast new Fibonacci()))
    f1 = f2 |> equal true

[<Fact>]
let ``test Enumerable object expr works`` () =
    let f1 = Seq.toArray (fib())
    let f2 = Seq.toArray (toSeq fibGen)
    f1 = f2 |> equal true

[<Fact>]
let ``test .NET Enumerator can be converted to JS iterator back and forth`` () =
    mkEnumerable [1..10]
    |> Seq.toList
    |> List.sum
    |> equal 55
