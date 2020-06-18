module QuickTest

// Run `npm run build quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler and fable-library.
// When everything works, move the tests to the appropriate file in tests/Main.
// You can check the compiled JS in the "bin" folder within this directory.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let log (o: obj) =
    printfn "%O" o

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

let throwsError (expected: string) (f: unit -> 'a): unit =
    let success =
        try
            f () |> ignore
            true
        with e ->
            if not <| String.IsNullOrEmpty(expected) then
                equal e.Message expected
            false
    // TODO better error messages
    equal false success

let testCase (msg: string) f: unit =
    try
        printfn "%s" msg
        f ()
    with ex ->
        printfn "%s" ex.Message
        if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
            printfn "%s" ex.StackTrace
    printfn ""

let testCaseAsync msg f =
    testCase msg (fun () ->
        async {
            try
                do! f ()
            with ex ->
                printfn "%s" ex.Message
                if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
                    printfn "%s" ex.StackTrace
        } |> Async.StartImmediate)

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

// TODO: Tests for uncurrying arguments
type Foo =
    abstract Foo: string with get, set
    abstract DoSomething: float -> float
    abstract Item: int -> char with get, set
    abstract Sum: [<ParamArray>] items: string[] -> string

[<Mangle>]
type Bar =
    abstract Bar: string with get, set
    abstract DoSomething: float -> float
    abstract Item: int -> char with get, set
    abstract Item: char -> bool with get
    abstract Sum: [<ParamArray>] items: string[] -> string

[<AbstractClass>]
type FooAbstractClass(x: float) =
    member _.Value = x
    member _.DoSomething(x, y) = x * y
    abstract DoSomething: float -> float

type FooClass(x) =
    inherit FooAbstractClass(5.)
    let mutable x = x
    override this.DoSomething(x) =
        this.DoSomething(x, this.Value)
    static member ChangeChar(s: string, i: int, c: char) =
        s.ToCharArray() |> Array.mapi (fun i2 c2 -> if i = i2 then c else c2) |> String
    interface Foo with
        member _.Foo with get() = x and set(y) = x <- y
        member this.DoSomething(x) = this.DoSomething(x + 2.)
        member _.Item with get(i) = x.[i] and set i c = x <- FooClass.ChangeChar(x, i, c)
        member _.Sum(items) = Array.reduce (fun x y -> x + y + x + y) items

[<AbstractClass>]
type BarAbstractClass(x: float) =
    member _.Value = x
    member _.DoSomething(x, y) = x ** y
    abstract DoSomething: float -> float

type BarClass(x) =
    inherit BarAbstractClass(10.)
    let mutable x = x
    override this.DoSomething(x) =
        this.DoSomething(x, this.Value)
    interface Bar with
        member _.Bar with get() = x and set(y) = x <- y
        member this.DoSomething(x) = this.DoSomething(x + 3.)
        member _.Item with get(i) = x.[i] and set i c = x <- FooClass.ChangeChar(x, i + 1, c)
        member _.Item with get(c) = x.ToCharArray() |> Array.exists ((=) c)
        member _.Sum(items) = Array.reduce (fun x y -> x + x + y + y) items

let test() =
    let mutable foo = "Foo"
    let mutable bar = "Bar"
    let foo = { new Foo with member _.Foo with get() = foo and set x = foo <- x
                             member _.DoSomething(x) = x + 1.
                             member _.Item with get(i) = foo.[i] and set i c = foo <- FooClass.ChangeChar(foo, i - 1, c)
                             member _.Sum(items) = Array.reduce (+) items }
    let bar = { new Bar with member _.Bar with get() = bar and set x = bar <- x
                             member _.DoSomething(x) = x + 2.
                             member _.Item with get(i) = bar.[i] and set _ c = bar <- FooClass.ChangeChar(bar, 0, c)
                             member _.Item with get(c) = bar.ToCharArray() |> Array.exists ((=) c)
                             member _.Sum(items) = Array.rev items |> Array.reduce (+)  }

    foo.[3] <- 'W'
    bar.[3] <- 'Z'
    foo.Foo <- foo.Foo + foo.DoSomething(3.).ToString() + foo.[2].ToString()
    bar.Bar <- bar.Bar + bar.DoSomething(3.).ToString() + bar.[2].ToString() + (sprintf "%b%b" bar.['B'] bar.['x'])
    foo.Foo <- foo.Foo + foo.Sum("a", "bc", "d")
    bar.Bar <- bar.Bar + bar.Sum("a", "bc", "d")

    let foo2 = FooClass("Foo") :> Foo
    let bar2 = BarClass("Bar") :> Bar
    foo2.[0] <- 'W'
    bar2.[0] <- 'Z'
    foo2.Foo <- foo2.Foo + foo2.DoSomething(3.).ToString() + foo.[2].ToString()
    bar2.Bar <- bar2.Bar + bar2.DoSomething(3.).ToString() + bar.[2].ToString() + (sprintf "%b%b" bar.['B'] bar.['x'])
    foo2.Foo <- foo2.Foo + foo2.Sum("a", "bc", "d")
    bar2.Bar <- bar2.Bar + bar2.Sum("a", "bc", "d")

    foo.Foo + bar.Bar + foo2.Foo + bar2.Bar

test() |> log
