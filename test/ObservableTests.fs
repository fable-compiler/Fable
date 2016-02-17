[<NUnit.Framework.TestFixture>] 
module FunScript.Tests.Observable
open System
open NUnit.Framework
open Fabel.Tests.Util

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
            
let subscribe (source: IObservable<'T>) (f: 'T->unit) =
    source.Subscribe(MyObserver(f))
    |> ignore

[<Test>]
let ``IObservable.subscribe works``() =
    let source = MyObservable()
    subscribe source (equal 10)
    source.Trigger 10