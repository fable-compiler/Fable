module Fable.Tests.ObservableTests

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

module tests =

    [<Fact>]
    let ``IObservable.Subscribe works`` () =
        let source = MyObservable()
        (source :> IObservable<int>)
            .Subscribe(MyObserver(equal 10)) |> ignore
        source.Trigger 10

    [<Fact>]
    let ``Observable.subscribe works`` () =
        let source = MyObservable()
        Observable.subscribe (equal 10.) source |> ignore
        source.Trigger 10.

    [<Fact>]
    let ``Disposing IObservable works`` () =
        let res = ref 0
        let source = MyObservable()
        let disp = Observable.subscribe (fun x -> res := !res + x) source
        source.Trigger 5
        disp.Dispose()
        source.Trigger 5
        equal 5 !res

    [<Fact>]
    let ``Observable.add works`` () =
        let source = MyObservable()
        Observable.add (equal "hello") source
        source.Trigger "hello"

    [<Fact>]
    let ``Observable.choose works`` () =
        let source = MyObservable()
        source
        |> Observable.choose (function
            | Choice1Of2 _ -> None
            | Choice2Of2 x -> Some x)
        |> Observable.add (equal 3)
        source.Trigger (Choice1Of2 2)
        source.Trigger (Choice2Of2 3)

    [<Fact>]
    let ``Observable.filter works`` () =
        let source = MyObservable()
        Observable.filter ((>) 5) source
        |> Observable.add (equal 3)
        source.Trigger 10
        source.Trigger 3

    [<Fact>]
    let ``Observable.map works`` () =
        let source = MyObservable()
        Observable.map not source
        |> Observable.add (equal false)
        source.Trigger true

    [<Fact>]
    let ``Observable.merge works`` () =
        let source1 = MyObservable()
        let source2 = MyObservable()
        (source1, source2)
        ||> Observable.merge
        |> Observable.add (equal 3)
        source2.Trigger 3
        source1.Trigger 3

    [<Fact>]
    let ``Observable.pairwise works`` () =
        let source = MyObservable()
        source
        |> Observable.pairwise
        |> Observable.add (fun (x, y) ->
            equal 1 x
            equal 2 y)
        source.Trigger 1
        source.Trigger 2

    [<Fact>]
    let ``Observable.partition works`` () =
        let source = MyObservable()
        let source1, source2 =
            source |> Observable.partition ((>) 5)
        Observable.add (equal 3) source1
        Observable.add (equal 8) source2
        source.Trigger 8
        source.Trigger 3

    [<Fact>]
    let ``Observable.scan works`` () =
        let mutable state = 5
        let source = MyObservable()
        (5, source)
        ||> Observable.scan (+)
        |> Observable.add (fun x ->
            state <- state + 1
            equal state x)
        source.Trigger 1
        source.Trigger 1

    [<Fact>]
    let ``Observable.split works`` () =
        let source = MyObservable()
        let source1, source2 =
            source |> Observable.split (fun x ->
                if 5 > x
                then Choice1Of2 (x*3)
                else Choice2Of2 (x*2))
        Observable.add (equal 6) source1
        Observable.add (equal 12) source2
        source.Trigger 6
        source.Trigger 2
