module QuickTest

open System
open Fable.Core

[<Mangle>]
type BarInterface =
    abstract Bar: string with get, set
    abstract DoSomething: f: (float -> float -> float) * v: float -> float
    abstract Item: int -> char with get, set
    abstract Item: char -> bool with get
    abstract Sum: [<ParamArray>] items: string[] -> string

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
    interface BarInterface with
        member _.Bar with get() = x and set(y) = x <- y
        member this.DoSomething(f, x) =
            let f = f x
            let x = f 4.5
            let y = f 7.
            this.DoSomething(x - y)
        member _.Item with get(i) = x.[i] and set i c = x <- x.ToCharArray() |> Array.mapi (fun i2 c2 -> if i = i2 then c else c2) |> String
        member _.Item with get(c) = x.ToCharArray() |> Array.exists ((=) c)
        member _.Sum(items) = Array.reduce (fun x y -> x + x + y + y) items

let test () =
    let addPlus2 x y = x + y + 2.
    let multiplyTwice x y = x * y * y
    let bar2 = BarClass("Bar") :> BarInterface
    bar2.[0] <- 'Z'
    bar2.Bar <- bar2.Bar + bar2.DoSomething(addPlus2, 3.).ToString("F2") + bar2.[2].ToString() + (sprintf "%b%b" bar2.['B'] bar2.['x'])
    bar2.Bar <- bar2.Bar + bar2.Sum("a", "bc", "d")
    bar2.Bar |> printfn "%s" // Zar9536.74rfalsefalseaabcbcaabcbcdd

test()
