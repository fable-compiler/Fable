#r "nuget:Fable.Python"

open Fable.Core
open Fable.Core.Testing
open Fable.Core.PyInterop
open Fable.Python.Builtins
open System

let equal expected actual =
    Assert.AreEqual(expected, actual)

[<EntryPoint>]
let main argv =
    let name = Array.tryHead argv |> Option.defaultValue "Guest"
    printfn $"Hello {name}!"

    // Open file with builtin `open`
    // use file = builtins.``open``(StringPath "data.txt")
    // file.read() |> printfn "File contents: %s"

    let t1 = TimeSpan(10, 10, 10)
    let t2 = TimeSpan(1, 1, 1, 1, 1, 1)
    let t3 = TimeSpan(1, 1, 1, 1, 1)

    // printfn "%A" t1.TotalSeconds
    // printfn "%A" t2.TotalSeconds
    // printfn "%A" t3.TotalMilliseconds

    // printfn "%A" t3.TotalMicroseconds
    // printfn "%A" t3.TotalMilliseconds
    // printfn "%A" t3.TotalSeconds
    // printfn "%A" t3.TotalMinutes
    // printfn "%A" t3.TotalHours
    // printfn "%A" t3.TotalDays
    // printfn "%A" 1.042372697
    // printfn "%A" (t3.TotalDays = 1.042372697)

    // TimeSpan(10, 20, 30, 10, 10, 10).Ticks |> printfn "TotalMicroseconds: %A"
    // TimeSpan(10, 20, 30, 10, 5, 12).Milliseconds |> printfn "TotalMicroseconds: %A"
    // TimeSpan(10, 20, 30, 10, -5, 12).Milliseconds |> printfn "TotalMicroseconds: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).Milliseconds |> printfn "Milliseconds: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).Seconds |> printfn "Seconds: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).Minutes |> printfn "Minutes: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).Hours |> printfn "Hours: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).Days |> printfn "TotalDays: %A"

    // TimeSpan(10, 0, 0, 0, 0).Divide(3.26).Ticks |> printfn "TotalMicroseconds: %A"

    // TimeSpan.FromTicks(2650306748466L).TotalSeconds |> printfn "TotalSeconds: %A"

    // TimeSpan(1).TotalNanoseconds |> printfn "TotalNanoseconds: %A"
    // TimeSpan(1).Ticks |> printfn "Ticks: %A"
    // TimeSpan.FromHours(3).TotalHours |> printfn "TotalHours: %A"
    // TimeSpan.FromHours(3).Hours |> printfn "TotalHours: %A"
    // TimeSpan(0,3,0,0,0).Hours |> printfn "TotalHours: %A"

    // TimeSpan.fro

    // TimeSpan(10, 20, 30, 10, 10, 10).TotalMicroseconds.ToString() |> printfn "TotalMicroseconds: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).TotalMilliseconds.ToString() |> printfn "TotalMilliseconds: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).TotalSeconds.ToString() |> printfn "TotalSeconds: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).TotalMinutes.ToString() |> printfn "TotalMinutes: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).TotalHours.ToString() |> printfn "TotalHours: %A"
    // TimeSpan(10, 20, 30, 10, 10, 10).TotalDays.ToString() |> printfn "TotalDays: %A"

    // TimeSpan(10, 20, 30, 10, 10, 10).Ticks.ToString() |> printfn "TotalDays: %A"

    // printfn "%A" (TimeSpan(10, 20, 30, 10, 10, 10).TotalMicroseconds = 9.3781001e+11)

    // printfn "%A" 9.3781001e+11

    // TimeSpan(10, 20, 30, 10, 10, 10).TotalDays |> printfn "%A"
    // printfn "%A" 10.85428252


    async {
        let mutable _aggregate = 0

        let makeWork i =
            async {
                // check that the individual work items run sequentially and not interleaved
                _aggregate <- _aggregate + i
                let copyOfI = _aggregate
                do! Async.Sleep 100
                equal copyOfI _aggregate
                do! Async.Sleep 100
                equal copyOfI _aggregate
                return i
            }
        let works = [ for i in 1 .. 5 -> makeWork i ]
        let now = DateTimeOffset.Now
        let! result = Async.Sequential works
        let ``then`` = DateTimeOffset.Now
        let d = ``then`` - now
        if d.TotalSeconds < 0.99 then
            failwithf "expected sequential operations to take 1 second or more, but took %.3f" d.TotalSeconds
        result |> equal [| 1 .. 5 |]
        result |> Seq.sum |> equal _aggregate
    } |> Async.RunSynchronously





    0
