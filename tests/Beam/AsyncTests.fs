module Fable.Tests.AsyncTests

open System
open Fable.Tests.Util
open Util.Testing

type MyException(value) =
    inherit Exception()
    member _.Value: int = value

[<Fact>]
let ``test async return works`` () =
    let comp = async { return 42 }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async let bang works`` () =
    let comp = async {
        let! x = async { return 10 }
        let! y = async { return 20 }
        return x + y
    }
    Async.RunSynchronously comp |> equal 30

[<Fact>]
let ``test async do bang works`` () =
    let mutable x = 0
    let comp = async {
        do! async { x <- 42; return () }
        return x
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async return from works`` () =
    let inner = async { return "hello" }
    let outer = async { return! inner }
    Async.RunSynchronously outer |> equal "hello"

[<Fact>]
let ``test async try-with works`` () =
    let comp = async {
        try
            failwith "boom"
            return 0
        with _e ->
            return 42
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async sleep works`` () =
    let comp = async {
        do! Async.Sleep 10
        return 1
    }
    Async.RunSynchronously comp |> equal 1

[<Fact>]
let ``test async parallel works`` () =
    let comps = [|
        async { return 1 }
        async { return 2 }
        async { return 3 }
    |]
    let results = Async.RunSynchronously (Async.Parallel comps)
    Array.sum results |> equal 6

[<Fact>]
let ``test async sequential composition works`` () =
    let mutable count = 0
    let comp = async {
        let! a = async { count <- count + 1; return count }
        let! b = async { count <- count + 1; return count }
        return a + b
    }
    Async.RunSynchronously comp |> equal 3

[<Fact>]
let ``test async ignore works`` () =
    let comp = async {
        do! Async.Ignore (async { return 42 })
        return 1
    }
    Async.RunSynchronously comp |> equal 1

[<Fact>]
let ``test async start immediate works`` () =
    let mutable x = 0
    Async.StartImmediate (async { x <- 42 })
    equal 42 x

let asyncMap f a = async {
    let! a = a
    return f a
}

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))

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
    let mutable result = 0
    async {
        for inp in inputs do
            result <- result + inp
    } |> Async.StartImmediate
    equal result 6

[<Fact>]
let ``test Async exceptions are handled correctly`` () =
    let mutable result = 0
    let f shouldThrow =
        async {
            try
                if shouldThrow then failwith "boom!"
                else result <- 12
            with _ -> result <- 10
        } |> Async.StartImmediate
        result
    f true + f false |> equal 22

[<Fact>]
let ``test Simple async is executed correctly`` () =
    let mutable result = false
    let x = async { return 99 }
    async {
        let! x = x
        let y = 99
        result <- x = y
    }
    |> Async.StartImmediate
    equal result true

[<Fact>]
let ``test Try ... with ... expressions inside async expressions work the same`` () =
    let result = ref ""
    let throw() : unit =
        raise(exn "Boo!")
    let append(x) =
        result.Value <- result.Value + x
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
    equal "abcdef" result.Value

[<Fact>]
let ``test Async StartWithContinuations works`` () =
    let res1, res2 = ref "", ref ""
    Async.StartWithContinuations(successWork, (fun x -> res1.Value <- x), ignore, ignore)
    Async.StartWithContinuations(errorWork, ignore, (fun x -> res2.Value <- x.Message), ignore)
    equal "success" res1.Value
    equal "error" res2.Value

[<Fact>]
let ``test Async.Catch works`` () =
    let assign (res: Ref<string>) = function
        | Choice1Of2 msg -> res.Value <- msg
        | Choice2Of2 (ex: Exception) -> res.Value <- "ERROR: " + ex.Message
    let res1 = ref ""
    let res2 = ref ""
    async {
        let! x1 = successWork |> Async.Catch
        assign res1 x1
        let! x2 = errorWork |> Async.Catch
        assign res2 x2
    } |> Async.StartImmediate
    equal "success" res1.Value
    equal "ERROR: error" res2.Value

[<Fact>]
let ``test Deep recursion with async doesn't cause stack overflow`` () =
    async {
        let result = ref false
        let rec trampolineTest (res: bool ref) i = async {
            if i > 100000
            then res.Value <- true
            else return! trampolineTest res (i+1)
        }
        do! trampolineTest result 0
        equal result.Value true
    } |> Async.StartImmediate

[<Fact>]
let ``test Nested failure propagates in async expressions`` () =
    let data = ref ""
    let f1 x =
        async {
            try
                failwith "1"
                return x
            with
            | e -> return! failwith ("2 " + e.Message)
        }
    let f2 x =
        async {
            try
                return! f1 x
            with
            | e -> return! failwith ("3 " + e.Message)
        }
    let f() =
        async {
            try
                let! _y = f2 4
                return ()
            with
            | e -> data.Value <- e.Message
        }
        |> Async.StartImmediate
    f()
    equal "3 2 1" data.Value

[<Fact>]
let ``test Try .. finally expressions inside async expressions work`` () =
    let data = ref ""
    async {
        try data.Value <- data.Value + "1 "
        finally data.Value <- data.Value + "2 "
    } |> Async.StartImmediate
    async {
        try
            try failwith "boom!"
            finally data.Value <- data.Value + "3"
        with _ -> ()
    } |> Async.StartImmediate
    equal "1 2 3" data.Value

[<Fact>]
let ``test Final statement inside async expressions can throw`` () =
    let data = ref ""
    let f() = async {
        try data.Value <- data.Value + "1 "
        finally failwith "boom!"
    }
    async {
        try
            do! f()
            return ()
        with
        | e -> data.Value <- data.Value + e.Message
    }
    |> Async.StartImmediate
    equal "1 boom!" data.Value

[<Fact>]
let ``test Async.Bind propagates exceptions`` () =
    async {
        let task1 name = async {
            if name = "fail" then
                failwith "Invalid access credentials"
            return "Ok"
        }

        let task2 name = async {
            do! Async.Sleep 100
            if name = "fail" then
                failwith "Invalid access credentials"
            return "Ok"
        }

        let doWork name task =
            let catch comp = async {
                let! res = Async.Catch comp
                return
                    match res with
                    | Choice1Of2 str -> str
                    | Choice2Of2 ex -> ex.Message
            }
            async {
                let! a = task "work" |> catch
                let! b = task "fail" |> catch
                return a, b
            }

        let! res1 = doWork "task1" task1
        let! res2 = doWork "task2" task2
        equal ("Ok", "Invalid access credentials") res1
        equal ("Ok", "Invalid access credentials") res2
    } |> Async.StartImmediate

[<Fact>]
let ``test Unit arguments are erased`` () =
    let mutable token = 0
    async {
        let! _res =
            async.Return 5
            |> asyncMap (fun x -> token <- x)
        equal 5 token
    } |> Async.StartImmediate

#if FABLE_COMPILER

[<Fact>]
let ``test CancellationTokenSource create and cancel works`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    equal false tcs.Token.IsCancellationRequested
    tcs.Cancel()
    equal true tcs.Token.IsCancellationRequested

[<Fact>]
let ``test CancellationTokenSource pre-cancelled works`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Cancel()
    equal true tcs.Token.IsCancellationRequested

[<Fact>]
let ``test CancellationTokenSourceRegister works`` () =
    let mutable x = 0
    let tcs = new System.Threading.CancellationTokenSource()
    let _reg = tcs.Token.Register(fun () -> x <- x + 1)
    equal 0 x
    tcs.Cancel()
    equal 1 x

[<Fact>]
let ``test CancellationTokenSource multiple registers work`` () =
    let mutable x = 0
    let tcs = new System.Threading.CancellationTokenSource()
    let _reg1 = tcs.Token.Register(fun () -> x <- x + 1)
    let _reg2 = tcs.Token.Register(fun () -> x <- x + 10)
    tcs.Cancel()
    equal 11 x

[<Fact>]
let ``test async cancellation with pre-cancelled token`` () =
    let mutable res = false
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Cancel()
    Async.StartWithContinuations(
        async {
            do! Async.Sleep 10
            res <- true
        },
        ignore,
        ignore,
        (fun _ -> ()),  // on cancel
        tcs.Token)
    equal false res

[<Fact>]
let ``test async cancellation with auto-cancel token`` () =
    let mutable res = false
    let tcs = new System.Threading.CancellationTokenSource(50)
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res <- true
    }, tcs.Token)
    equal false res

[<Fact>]
let ``test CancellationToken Dispose is no-op`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Dispose()
    // Should not throw - Dispose is a no-op
    equal false tcs.Token.IsCancellationRequested

[<Fact>]
let ``test Can use custom exceptions in async workflows`` () =
    let workflow(): Async<unit> = async {
        return MyException(7) |> raise
    }
    let parentWorkflow() =
        async {
            try
                do! workflow()
                return 100
            with
            | :? MyException as ex -> return ex.Value
        }
    async {
        let! res = parentWorkflow()
        equal 7 res
    } |> Async.RunSynchronously

#endif
