module Fable.Tests.Observable

open System
open Util.Testing

type MyObserver<'T>(f) =
    interface IObserver<'T> with
        member x.OnNext v = f v
        member x.OnError e = ()
        member x.OnCompleted() = ()

type MyObservable<'T>() =
    let listeners = ResizeArray<IObserver<'T>>()
    member x.Trigger v =
        for lis in listeners do
            lis.OnNext v
    interface IObservable<'T> with
        member x.Subscribe w =
            listeners.Add(w)
            { new IDisposable with
                member x.Dispose() = listeners.Remove(w) |> ignore }

[<Fact>]
let ``test IObservable.Subscribe works`` () =
        let source = MyObservable()
        (source :> IObservable<int>)
            .Subscribe(MyObserver(equal 10)) |> ignore
        source.Trigger 10

[<Fact>]
let ``test Observable.subscribe works`` () =
    let source = MyObservable()
    Observable.subscribe (equal 10.) source |> ignore
    source.Trigger 10.

[<Fact>]
let ``test Disposing IObservable works`` () =
    let res = ref 0
    let source = MyObservable()
    let disp = Observable.subscribe (fun x -> res.Value <- res.Value + x) source
    source.Trigger 5
    disp.Dispose()
    source.Trigger 5
    equal 5 res.Value

[<Fact>]
let ``test Observable.add works`` () =
    let source = MyObservable()
    Observable.add (equal "hello") source
    source.Trigger "hello"

[<Fact>]
let ``test Observable.choose works`` () =
    let source = MyObservable()
    source
    |> Observable.choose (function
        | Choice1Of2 _ -> None
        | Choice2Of2 x -> Some x)
    |> Observable.add (equal 3)
    source.Trigger (Choice1Of2 2)
    source.Trigger (Choice2Of2 3)

[<Fact>]
let ``test Observable.filter works`` () =
    let source = MyObservable()
    Observable.filter ((>) 5) source
    |> Observable.add (equal 3)
    source.Trigger 10
    source.Trigger 3

[<Fact>]
let ``test Observable.map works`` () =
    let source = MyObservable()
    Observable.map not source
    |> Observable.add (equal false)
    source.Trigger true
