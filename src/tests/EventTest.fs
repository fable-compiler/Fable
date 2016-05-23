[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Event
open System
open NUnit.Framework
open Fable.Tests.Util

[<Test>]
let ``Event.add works``() =
    let source = Event<int>()
    source.Publish |> Event.add (equal 10)
    source.Trigger 10

[<Test>]
let ``Event.choose works``() =
    let mutable result = 0

    let source = Event<_>()
    source.Publish
    |> Event.choose (function
        | Choice1Of2 _ -> None
        | Choice2Of2 x -> Some x)
    |> Event.add (fun n -> result <- n)
    source.Trigger (Choice1Of2 2)
    source.Trigger (Choice2Of2 3)
    
    equal 3 result

[<Test>]
let ``Event.filter works``() =
    let mutable result = 0 
    
    let source = Event<_>()
    source.Publish
    |> Event.filter ((>) 5) 
    |> Event.add (fun n -> result <- n )
    source.Trigger 3
    equal 3 result
    source.Trigger 10
    equal 3 result

[<Test>]
let ``Event.map works``() =
    let mutable result = 0 

    let source = Event<_>()
    source.Publish 
    |> Event.map ((+) 3)
    |> Event.add (fun n -> result <- n)
    source.Trigger 10
    
    equal 13 result

[<Test>]
let ``Event.merge works``() =
    let mutable result = 0

    let source1 = Event<_>()
    let source2 = Event<_>()
    (source1.Publish, source2.Publish)
    ||> Event.merge
    |> Event.add (fun n -> result <- n)
    source2.Trigger 4
    equal 4 result
    source1.Trigger 3
    equal 3 result

[<Test>]
let ``Event.pairwise works``() =
    let mutable result1 = 0
    let mutable result2 = 0

    let source = Event<_>()
    source.Publish
    |> Event.pairwise
    |> Event.add (fun (x, y) ->
        result1 <- x
        result2 <- y)
    source.Trigger 1
    source.Trigger 2
    
    equal 1 result1
    equal 2 result2
    

[<Test>]
let ``Event.partition works``() =
    let mutable result1 = 0
    let mutable result2 = 0

    let source = Event<_>()
    let source1, source2 =
        source.Publish |> Event.partition ((>) 5)
    Event.add (fun n -> result1 <- n) source1
    Event.add (fun n -> result2 <- n) source2
    source.Trigger 8
    source.Trigger 3
    
    equal 3 result1
    equal 8 result2

[<Test>]
let ``Event.scan works``() =
    let mutable state = 5
    let source =Event<_>()
    (5, source.Publish)
    ||> Event.scan (+)
    |> Event.add (fun x ->
        state <- state + 1
        )
    source.Trigger 1
    source.Trigger 1
    
    equal state 7

[<Test>]
let ``Event.split works``() =
    let mutable result1 = 0
    let mutable result2 = 0

    let source = Event<_>()
    let source1, source2 =
        source.Publish |> Event.split (fun x ->
            if 5 > x
            then Choice1Of2 (x*3)
            else Choice2Of2 (x*2))
    Event.add (fun n -> result1 <- n) source1
    Event.add (fun n -> result2 <- n) source2
    source.Trigger 6
    source.Trigger 2
    
    equal 6 result1
    equal 12 result2 
    
[<Test>]
let ``IEvent.add works``() =
    let mutable result = 0    
    
    let source = Event<_> ()
    source.Publish.Add(fun n -> result <- n)
    
    source.Trigger 6
    equal 6 result
    
[<Test>]
let ``IEvent.Subscribe works``() =
    let mutable result = 0      
    
    let source = Event<_> ()    
    source.Publish.Subscribe(fun n -> result <- n) |> ignore
     
    source.Trigger 6
    equal 6 result

[<Test>]
let ``IEvent.AddHandler works``() =
    let mutable result = 0      
    
    let source = Event<_> ()    
    source.Publish.AddHandler(new Handler<_>(fun sender n -> result <- n)) |> ignore
     
    source.Trigger 6
    equal 6 result
    
[<Test>]
let ``IEvent.RemoveHandler works``() =
    let mutable result = 0      
    
    let handler = new Handler<_>(fun sender n -> result <- n)
    
    let source = Event<_> ()    
    source.Publish.AddHandler(handler) |> ignore
    source.Publish.RemoveHandler(handler)
     
    source.Trigger 6
    equal 0 result