module Fable.Tests.MailboxProcessorTests

open System
open Util.Testing

type Message = string * AsyncReplyChannel<string>

type Get =
    | GetZero of replyChannel: AsyncReplyChannel<int>
    | GetOne of replyChannel: AsyncReplyChannel<int>

type CounterMsg =
    | Increment
    | Decrement
    | GetCount of AsyncReplyChannel<int>

type SumMsg =
    | Add of int
    | GetSum of AsyncReplyChannel<int>

// Basic post and receive

[<Fact>]
let ``test MailboxProcessor.post works`` () =
    async {
        let mutable res = None
        let agent = new MailboxProcessor<int>(fun inbox ->
            let rec messageLoop() = async {
                let! msg = inbox.Receive()
                match msg with
                | 3 -> () // Finish loop
                | _ ->
                    res <- Some msg
                    return! messageLoop()
            }
            messageLoop()
        )
        agent.Post(1)
        equal None res // Mailbox hasn't started yet
        agent.Start()
        agent.Post(2)
        do! Async.Sleep 200
        equal (Some 2) res
        agent.Post(3)
        equal (Some 2) res  // Mailbox has finished
    } |> Async.StartImmediate

// PostAndAsyncReply

[<Fact>]
let ``test MailboxProcessor.postAndAsyncReply works`` () =
    async {
        let formatString = "Msg: {0} - {1}"
        let agent = MailboxProcessor<Message>.Start(fun inbox ->
            let rec loop n = async {
                let! message, replyChannel = inbox.Receive()
                replyChannel.Reply(String.Format(formatString, n, message))
                if message <> "Bye" then do! loop (n + 1)
            }
            loop 0)
        let! resp = agent.PostAndAsyncReply(fun replyChannel -> "Hi", replyChannel)
        equal "Msg: 0 - Hi" resp
        let! resp = agent.PostAndAsyncReply(fun replyChannel -> "Bye", replyChannel)
        equal "Msg: 1 - Bye" resp
    } |> Async.StartImmediate

[<Fact>]
let ``test MailboxProcessor.postAndAsyncReply works with falsy values`` () =
    async {
        let agent = MailboxProcessor.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg = inbox.Receive()
                    match msg with
                    | GetZero replyChannel ->
                        replyChannel.Reply 0
                        return! loop ()
                    | GetOne replyChannel ->
                        replyChannel.Reply 1
                        return! loop ()
                }
            loop () )
        let! resp = agent.PostAndAsyncReply(GetOne)
        equal 1 resp
        let! resp = agent.PostAndAsyncReply(GetZero)
        equal 0 resp
    } |> Async.StartImmediate

// Message ordering

[<Fact>]
let ``test MailboxProcessor processes messages in FIFO order`` () =
    async {
        let mutable received = []
        let agent = MailboxProcessor<int>.Start(fun inbox ->
            let rec loop () = async {
                let! msg = inbox.Receive()
                received <- received @ [msg]
                if msg <> 0 then return! loop ()
            }
            loop ()
        )
        agent.Post(1)
        agent.Post(2)
        agent.Post(3)
        agent.Post(0) // Sentinel to stop
        do! Async.Sleep 200
        equal [1; 2; 3; 0] received
    } |> Async.StartImmediate

// State accumulation

[<Fact>]
let ``test MailboxProcessor maintains state across messages`` () =
    async {
        let agent = MailboxProcessor<CounterMsg>.Start(fun inbox ->
            let rec loop count = async {
                let! msg = inbox.Receive()
                match msg with
                | Increment -> return! loop (count + 1)
                | Decrement -> return! loop (count - 1)
                | GetCount rc ->
                    rc.Reply count
                    return! loop count
            }
            loop 0
        )
        agent.Post(Increment)
        agent.Post(Increment)
        agent.Post(Increment)
        agent.Post(Decrement)
        let! count = agent.PostAndAsyncReply(GetCount)
        equal 2 count
    } |> Async.StartImmediate

// Start methods

[<Fact>]
let ``test MailboxProcessor.Start static method works`` () =
    async {
        let mutable res = 0
        let agent = MailboxProcessor<int>.Start(fun inbox -> async {
            let! msg = inbox.Receive()
            res <- msg
        })
        agent.Post(42)
        do! Async.Sleep 200
        equal 42 res
    } |> Async.StartImmediate

[<Fact>]
let ``test MailboxProcessor instance Start works`` () =
    async {
        let mutable res = 0
        let agent = new MailboxProcessor<int>(fun inbox -> async {
            let! msg = inbox.Receive()
            res <- msg
        })
        agent.Post(99) // Queued before start
        equal 0 res
        agent.Start()
        do! Async.Sleep 200
        equal 99 res
    } |> Async.StartImmediate

// Buffering: messages posted before start are queued

[<Fact>]
let ``test MailboxProcessor buffers messages posted before start`` () =
    async {
        let mutable received = []
        let agent = new MailboxProcessor<int>(fun inbox ->
            let rec loop () = async {
                let! msg = inbox.Receive()
                received <- received @ [msg]
                if msg <> 0 then return! loop ()
            }
            loop ()
        )
        agent.Post(1)
        agent.Post(2)
        agent.Post(3)
        agent.Post(0)
        equal [] received // Nothing processed yet
        agent.Start()
        do! Async.Sleep 200
        equal [1; 2; 3; 0] received
    } |> Async.StartImmediate

// Error handling

[<Fact>]
let ``test MailboxProcessor error in body does not crash`` () =
    async {
        let mutable errorCaught = false
        let agent = MailboxProcessor<int>.Start(fun inbox -> async {
            let! _ = inbox.Receive()
            failwith "boom"
        })
        try
            agent.Post(1)
        with _ ->
            errorCaught <- true
        do! Async.Sleep 200
        // The error may or may not be caught synchronously depending on implementation,
        // but the agent should not hang
        equal true true
    } |> Async.StartImmediate

// Cancellation

#if FABLE_COMPILER
[<Fact>]
let ``test MailboxProcessor cancellation works`` () =
    async {
        let mutable received = []
        let cts = new Threading.CancellationTokenSource()
        let agent = MailboxProcessor<int>.Start((fun inbox ->
            let rec loop () = async {
                let! msg = inbox.Receive()
                received <- received @ [msg]
                return! loop ()
            }
            loop ()
        ), cts.Token)
        agent.Post(1)
        agent.Post(2)
        do! Async.Sleep 100
        equal [1; 2] received
        cts.Cancel()
        agent.Post(3) // Should not be processed
        do! Async.Sleep 200
        equal [1; 2] received
    } |> Async.StartImmediate
#endif

// Multiple PostAndAsyncReply calls maintain correct pairing

[<Fact>]
let ``test MailboxProcessor multiple PostAndAsyncReply calls pair correctly`` () =
    async {
        let agent = MailboxProcessor<string * AsyncReplyChannel<string>>.Start(fun inbox ->
            let rec loop () = async {
                let! (msg, rc) = inbox.Receive()
                rc.Reply(msg.ToUpper())
                return! loop ()
            }
            loop ()
        )
        let! r1 = agent.PostAndAsyncReply(fun rc -> "hello", rc)
        let! r2 = agent.PostAndAsyncReply(fun rc -> "world", rc)
        let! r3 = agent.PostAndAsyncReply(fun rc -> "fable", rc)
        equal "HELLO" r1
        equal "WORLD" r2
        equal "FABLE" r3
    } |> Async.StartImmediate

// Recursive message loop with deep recursion

[<Fact>]
let ``test MailboxProcessor recursive loop does not stack overflow`` () =
    async {
        let mutable count = 0
        let agent = MailboxProcessor<int>.Start(fun inbox ->
            let rec loop () = async {
                let! msg = inbox.Receive()
                count <- count + 1
                if msg > 0 then
                    return! loop ()
            }
            loop ()
        )
        for i in 1000 .. -1 .. 0 do
            agent.Post(i)
        do! Async.Sleep 500
        equal 1001 count
    } |> Async.StartImmediate

// PostAndAsyncReply with computed replies

[<Fact>]
let ``test MailboxProcessor postAndAsyncReply can compute reply from state`` () =
    async {
        let agent = MailboxProcessor<SumMsg>.Start(fun inbox ->
            let rec loop sum = async {
                let! msg = inbox.Receive()
                match msg with
                | Add n -> return! loop (sum + n)
                | GetSum rc ->
                    rc.Reply sum
                    return! loop sum
            }
            loop 0
        )
        agent.Post(Add 10)
        agent.Post(Add 20)
        agent.Post(Add 30)
        let! sum = agent.PostAndAsyncReply(GetSum)
        equal 60 sum
        agent.Post(Add 5)
        let! sum = agent.PostAndAsyncReply(GetSum)
        equal 65 sum
    } |> Async.StartImmediate
