module Fable.Tests.MailboxProcessorTests

open Fable.Tests.Util
open Util.Testing

type Message = string * AsyncReplyChannel<string>

type Get =
    | GetZero of replyChannel: AsyncReplyChannel<int>
    | GetOne of replyChannel: AsyncReplyChannel<int>

[<Fact>]
let ``test MailboxProcessor.post works`` () =
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
    // On .NET, MailboxProcessor processes in the background; sleep lets it catch up
    Async.Sleep 200 |> Async.RunSynchronously
    equal (Some 2) res
    agent.Post(3)
    Async.Sleep 200 |> Async.RunSynchronously
    equal (Some 2) res // Mailbox has finished

[<Fact>]
let ``test MailboxProcessor.postAndAsyncReply works`` () =
    let formatString = "Msg: {0} - {1}"
    let agent = MailboxProcessor<Message>.Start(fun inbox ->
        let rec loop n = async {
            let! (message, replyChannel) = inbox.Receive()
            replyChannel.Reply(System.String.Format(formatString, n, message))
            if message <> "Bye" then do! loop (n + 1)
        }
        loop 0)
    let resp = agent.PostAndAsyncReply(fun replyChannel -> "Hi", replyChannel) |> Async.RunSynchronously
    equal "Msg: 0 - Hi" resp
    let resp = agent.PostAndAsyncReply(fun replyChannel -> "Bye", replyChannel) |> Async.RunSynchronously
    equal "Msg: 1 - Bye" resp

[<Fact>]
let ``test MailboxProcessor.postAndAsyncReply works with falsy values`` () =
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
    let resp = agent.PostAndAsyncReply(GetOne) |> Async.RunSynchronously
    equal 1 resp
    let resp = agent.PostAndAsyncReply(GetZero) |> Async.RunSynchronously
    equal 0 resp
