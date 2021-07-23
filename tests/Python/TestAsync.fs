module Fable.Tests.Async

open System
open Util.Testing

type DisposableAction(f) =
    interface IDisposable with
        member __.Dispose() = f()

let sleepAndAssign token res =
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res := true
    }, token)

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))
let cancelWork: Async<string> = Async.FromContinuations(fun (_,_,onCancel) ->
        System.OperationCanceledException("cancelled") |> onCancel)

[<Fact>]
let ``test Simple async translates without exception`` () =
    async { return () }
    |> Async.StartImmediate


[<Fact>]
let ``test Async while binding works correctly`` () =
    let mutable result = 0
    async {
        while result < 10 do
            result <- result + 1
    } |> Async.StartImmediate
    equal result 10

[<Fact>]
let ``test Async for binding works correctly`` () =
    let inputs = [|1; 2; 3|]
    let result = ref 0
    async {
        for inp in inputs do
            result := !result + inp
    } |> Async.StartImmediate
    equal !result 6

[<Fact>]
let ``test Async exceptions are handled correctly`` () =
    let result = ref 0
    let f shouldThrow =
        async {
            try
                if shouldThrow then failwith "boom!"
                else result := 12
            with _ -> result := 10
        } |> Async.StartImmediate
        !result
    f true + f false |> equal 22

[<Fact>]
let ``test Simple async is executed correctly`` () =
    let result = ref false
    let x = async { return 99 }
    async {
        let! x = x
        let y = 99
        result := x = y
    }
    |> Async.StartImmediate
    equal !result true

[<Fact>]
let ``test async use statements should dispose of resources when they go out of scope`` () =
    let isDisposed = ref false
    let step1ok = ref false
    let step2ok = ref false
    let resource = async {
        return new DisposableAction(fun () -> isDisposed := true)
    }
    async {
        use! r = resource
        step1ok := not !isDisposed
    }
    //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
    |> Async.StartImmediate
    step2ok := !isDisposed
    (!step1ok && !step2ok) |> equal true

[<Fact>]
let ``test Try ... with ... expressions inside async expressions work the same`` () =
    let result = ref ""
    let throw() : unit =
        raise(exn "Boo!")
    let append(x) =
        result := !result + x
    let innerAsync() =
        async {
            append "b"
            try append "c"
                throw()
                append "1"
            with _ -> append "d"
            append "e"
        }
    async {
        append "a"
        try do! innerAsync()
        with _ -> append "2"
        append "f"
    } |> Async.StartImmediate
    equal !result "abcdef"

// Disable this test for dotnet as it's failing too many times in Appveyor
#if FABLE_COMPILER

[<Fact>]
let ``test async cancellation works`` () =
    async {
        let res1, res2, res3 = ref false, ref false, ref false
        let tcs1 = new System.Threading.CancellationTokenSource(50)
        let tcs2 = new System.Threading.CancellationTokenSource()
        let tcs3 = new System.Threading.CancellationTokenSource()
        sleepAndAssign tcs1.Token res1
        sleepAndAssign tcs2.Token res2
        sleepAndAssign tcs3.Token res3
        tcs2.Cancel()
        tcs3.CancelAfter(1000)
        do! Async.Sleep 500
        equal false !res1
        equal false !res2
        equal true !res3
    } |> Async.StartImmediate

[<Fact>]
let ``test CancellationTokenSourceRegister works`` () =
    async {
        let mutable x = 0
        let res1 = ref false
        let tcs1 = new System.Threading.CancellationTokenSource(50)
        let foo = tcs1.Token.Register(fun () ->
            x <- x + 1)
        sleepAndAssign tcs1.Token res1
        do! Async.Sleep 500
        equal false !res1
        equal 1 x
    } |> Async.StartImmediate
#endif

[<Fact>]
let ``test Async StartWithContinuations works`` () =
    let res1, res2, res3 = ref "", ref "", ref ""
    Async.StartWithContinuations(successWork, (fun x -> res1 := x), ignore, ignore)
    Async.StartWithContinuations(errorWork, ignore, (fun x -> res2 := x.Message), ignore)
    Async.StartWithContinuations(cancelWork, ignore, ignore, (fun x -> res3 := x.Message))
    equal "success" !res1
    equal "error" !res2
    equal "cancelled" !res3

[<Fact>]
let ``test Async.Catch works`` () =
    let assign res = function
        | Choice1Of2 msg -> res := msg
        | Choice2Of2 (ex: Exception) -> res := "ERROR: " + ex.Message
    let res1 = ref ""
    let res2 = ref ""
    async {
        let! x1 = successWork |> Async.Catch
        assign res1 x1
        let! x2 = errorWork |> Async.Catch
        assign res2 x2
    } |> Async.StartImmediate
    equal "success" !res1
    equal "ERROR: error" !res2

[<Fact>]
let ``test Async.Ignore works`` () =
    let res = ref false
    async {
        do! successWork |> Async.Ignore
        res := true
    } |> Async.StartImmediate
    equal true !res

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
        // Not necessary in the JS implementation, but in .NET
        // MailboxProcessor works in the background so we must wait a bit
        do! Async.Sleep 200
        equal (Some 2) res
        agent.Post(3)
        equal (Some 2) res  // Mailbox has finished
    } |> Async.StartImmediate
