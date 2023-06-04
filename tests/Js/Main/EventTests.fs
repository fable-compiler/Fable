module Fable.Tests.Event

open Util.Testing

type ClassWithCLIEvent() =
    let event = new Event<_>()
    [<CLIEvent>]
    member _.Event = event.Publish
    member this.TestEvent(arg) =
        event.Trigger(this, arg)

type ClassWithNonCLIEvent() =
    let event = new Event<_>()
    member _.Event = event.Publish
    member this.TestEvent(arg) =
        event.Trigger(this, arg)

type InterfaceWithCLIEvent<'t> =
    [<CLIEvent>]
    abstract Event : IEvent<System.Action<obj,'t>,'t>

type ClassWithInterfaceWithCLIEvent<'t>() =
    let event = new Event<_,_>()
    member this.TestEvent(arg) =
        event.Trigger(this, arg)
    interface InterfaceWithCLIEvent<'t> with
        [<CLIEvent>]
        member _.Event = event.Publish

let tests =
  testList "Event" [
    testCase "Event.add works" <| fun () ->
        let source = Event<int>()
        source.Publish |> Event.add (equal 10)
        source.Trigger 10

    testCase "Event.choose works" <| fun () ->
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

    testCase "Event.filter works" <| fun () ->
        let mutable result = 0

        let source = Event<_>()
        source.Publish
        |> Event.filter ((>) 5)
        |> Event.add (fun n -> result <- n )
        source.Trigger 3
        equal 3 result
        source.Trigger 10
        equal 3 result

    testCase "Event.map works" <| fun () ->
        let mutable result = 0

        let source = Event<_>()
        source.Publish
        |> Event.map ((+) 3)
        |> Event.add (fun n -> result <- n)
        source.Trigger 10

        equal 13 result

    testCase "Event.merge works" <| fun () ->
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

    testCase "Event.pairwise works" <| fun () ->
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

    testCase "Event.partition works" <| fun () ->
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

    testCase "Event.scan works" <| fun () ->
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

    testCase "Event.split works" <| fun () ->
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

    testCase "IEvent.add works" <| fun () ->
        let mutable result = 0

        let source = Event<_> ()
        source.Publish.Add(fun n -> result <- n)

        source.Trigger 6
        equal 6 result

    testCase "IEvent.Subscribe works" <| fun () ->
        let mutable result = 0

        let source = Event<_> ()
        source.Publish.Subscribe(fun n -> result <- n) |> ignore

        source.Trigger 6
        equal 6 result

    testCase "IEvent.AddHandler works" <| fun () ->
        let mutable result = 0

        let source = Event<_> ()
        source.Publish.AddHandler(new Handler<_>(fun sender n -> result <- n)) |> ignore

        source.Trigger 6
        equal 6 result

    testCase "IEvent.RemoveHandler works" <| fun () ->
        let mutable result = 0

        let handler = new Handler<_>(fun sender n -> result <- n)

        let source = Event<_> ()
        source.Publish.AddHandler(handler) |> ignore
        source.Publish.RemoveHandler(handler)

        source.Trigger 6
        equal 0 result

    testCase "Classes can trigger CLI events" <| fun () ->
        let mutable result = 0
        let classWithEvent = new ClassWithCLIEvent()
        classWithEvent.Event.Add(fun (sender, x) -> result <- x)
        classWithEvent.TestEvent(5)
        equal 5 result

    testCase "Classes can trigger non-CLI events" <| fun () ->
        let mutable result = ""
        let classWithEvent = new ClassWithNonCLIEvent()
        let disp = classWithEvent.Event.Subscribe(fun (sender, arg) -> result <- arg)
        classWithEvent.TestEvent("Hello")
        disp.Dispose()
        classWithEvent.TestEvent("Bye")
        equal "Hello" result

    testCase "Classes can trigger CLI events on interfaces" <| fun () ->
        let mutable result = 0
        let mutable sender = null
        let classWithEvent = new ClassWithInterfaceWithCLIEvent<_>()
        (classWithEvent :> InterfaceWithCLIEvent<_>).Event.AddHandler(fun s x ->
            sender <- s
            result <- x
        )
        classWithEvent.TestEvent(5)
        equal 5 result
        equal (box classWithEvent) sender

#if !FABLE_COMPILER_TYPESCRIPT
    testCase "Generic interface expression can have CLI events" <| fun () ->
        let mutable actualSender = ""
        let mutable result = false
        let event = Event<_,_>()
        let ifaceWIthEvent =
            { new InterfaceWithCLIEvent<_> with
                [<CLIEvent>]
                member _.Event = event.Publish }
        ifaceWIthEvent.Event.AddHandler(fun sender arg ->
            actualSender <- string sender
            result <- arg)
        let expectedSender = "SENDER"
        let expectedResult = true
        event.Trigger(expectedSender, expectedResult)
        equal expectedSender actualSender
        equal expectedResult result
#endif

    testCase "Events are unsubscribed correctly" <| fun () -> // See #609
        let mutable counter = 0
        let test = new Event<_>()

        let firstSubscriber =
            test.Publish
            |> Observable.filter (fun x -> x < 25)
            |> Observable.subscribe (fun x -> counter <- counter + x)

        let secondSubscriber =
            test.Publish
            |> Observable.filter (fun x -> x > 25)
            |> Observable.subscribe (fun x -> counter <- counter + x)

        secondSubscriber.Dispose()

        for i in [1..50] do
            test.Trigger(i)

        equal 300 counter
  ]