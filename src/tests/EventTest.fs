[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Event
open System
open NUnit.Framework
open Fable.Tests.Util
            
[<Test>]
let ``Event.Subscribe works``() =
    let source = Event<int>()
    source.Publish.Subscribe(equal 10) |> ignore
    source.Trigger 10

[<Test>]
let ``Event.add works``() =
    let source = Event<int>()
    source.Publish |> Event.add (equal 10)
    source.Trigger 10

[<Test>]
let ``Event.choose works``() =
    let source = Event<_>()
    source.Publish
    |> Event.choose (function
        | Choice1Of2 _ -> None
        | Choice2Of2 x -> Some x)
    |> Event.add (equal 3)
    source.Trigger (Choice1Of2 2)
    source.Trigger (Choice2Of2 3)

[<Test>]
let ``Event.filter works``() =
    let source = Event<_>()
    source.Publish
    |> Event.filter ((>) 5) 
    |> Event.add (equal 3)
    source.Trigger 10
    source.Trigger 3

[<Test>]
let ``Event.map works``() =
    let source = Event<_>()
    source.Publish 
    |> Event.map not
    |> Event.add (equal false)
    source.Trigger true

[<Test>]
let ``Event.merge works``() =
    let source1 = Event<_>()
    let source2 = Event<_>()
    (source1.Publish, source2.Publish)
    ||> Event.merge
    |> Event.add (equal 3)
    source2.Trigger 3
    source1.Trigger 3

[<Test>]
let ``Event.pairwise works``() =
    let source = Event<_>()
    source.Publish
    |> Event.pairwise
    |> Event.add (fun (x, y) ->
        equal 1 x
        equal 2 y)
    source.Trigger 1
    source.Trigger 2

[<Test>]
let ``Event.partition works``() =
    let source = Event<_>()
    let source1, source2 =
        source.Publish |> Event.partition ((>) 5)
    Event.add (equal 3) source1
    Event.add (equal 8) source2
    source.Trigger 8
    source.Trigger 3

[<Test>]
let ``Event.scan works``() =
    let mutable state = 5
    let source =Event<_>()
    (5, source.Publish)
    ||> Event.scan (+)
    |> Event.add (fun x ->
        state <- state + 1
        equal state x)
    source.Trigger 1
    source.Trigger 1

[<Test>]
let ``Event.split works``() =
    let source = Event<_>()
    let source1, source2 =
        source.Publish |> Event.split (fun x ->
            if 5 > x
            then Choice1Of2 (x*3)
            else Choice2Of2 (x*2))
    Event.add (equal 6) source1
    Event.add (equal 12) source2
    source.Trigger 6
    source.Trigger 2
