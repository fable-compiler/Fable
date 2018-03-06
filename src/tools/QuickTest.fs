module QuickTest

// Use this template to make quick tests when adding new features to Fable.
// You must run a full build at least once (from repo root directory,
// type `sh build.sh` on OSX/Linux or just `build` on Windows). Then:
// - When making changes to Fable.Compiler run `build QuickFableCompilerTest`
// - When making changes to fable-core run `build QuickFableCoreTest`

// Please don't add this file to your commits

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open Fable.Import

// let equal expected actual =
//     let areEqual = expected = actual
//     printfn "%A = %A > %b" expected actual areEqual
//     if not areEqual then
//         failwithf "Expected %A but got %A" expected actual

// // Write here your unit test, you can later move it
// // to Fable.Tests project. For example:
// // [<Test>]
// // let ``My Test``() =
// //     Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
// //     Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4

// // You'll have to run your test manually, sorry!
// // ``My Test``()

let log (o: obj) =
    #if FABLE_COMPILER
    JS.console.log(o)
    #else
    printfn "%O" o
    #endif

type IFoo =
    abstract Foo: int -> int

type IBar =
    inherit IFoo
    abstract Bar: string
    abstract Jamon: int -> int

type Calostro(s) =
    member __.Joo = "jooo"
    interface IBar with
        member __.Foo(i) = i * 2
        member __.Jamon(i) = i * 3
        member this.Bar = string ((this :> IFoo).Foo(5) + (this :> IBar).Jamon(2) ) + this.Joo + s
        // member this.Bar = this.Joo + s

let test2438u9() =
    let c = Calostro("hihi") :> IBar
    log(c.Bar)
    log(c.Foo(6))

test2438u9()

// type Lib.Foo with
//     member this.XY() = this.Z() + 12

// type Foo2(x: int, y: int) =
//     member __.Z() = x + y
//     member this.Add(x, y) = x + y + this.Z()
//     member __.Add(x) = x * x * x
//     static member Add(x, y) = x - y
//     interface IFoo with
//         member my.Foo() = y

// // let delay (f:unit -> unit) = f
// // let rec a = delay (fun () -> b())
// // and b = delay a

// let test() =
//     let f = Foo2(14, 67)
//     let x = f.Add(4, 5)
//     let y = Lib.Foo.Add(6, 7)
//     let z = { Lib.x = 5 }
//     printfn "RESULT: %i" (x + y)
//     printfn "RESULT2: %i" (f.Add(10))
//     printfn "Extension: %i" (z.XY())

// test()

// let test2(f: IFoo) =
//     f.Foo()

// let print (i: int) = System.Console.WriteLine(i)

// let call2 f g = f 5 (fun x -> g)

// // call2(f, g) => f(5, x => g)

// call2 (fun i f -> f (i + 1)) 3 |> print

// // call2((i, f) => f(i +1))(3)


// call2 (fun i f -> f i 1) (fun x -> x) |> print


// let test_infinity() =
//     [1;2;3;4]
//     |> List.map (fun x -> x + 4)
//     |> List.filter (fun x -> x < 10)
//     |> List.choose (fun x -> Some([x; x]))
//     |> List.concat
//     |> List.fold (fun acc x -> acc + x) 6

// type Foo() =
//     member __.Z = 5
//     // static member (+) (x: Foo, y: string) = "cambalache" + y
//     // static member inline (+) (x: int, y: Foo) = x + 5

// let inline (+) (x: int) (y: Foo) = x + 5

// let test () =
//     let f = Foo()
//     4 + f

// let f (y: _ seq) = y // (y2: _ seq) = y

// let f2 () =
//     ["1";""] |> f
//     //  |> f [] // ("1"::"2"::li)


// type Foo5 = C1 | C2 | C3 of int | C4 | C5 | C6

// let test c =
//     match c with
//     | C1
//     | C2 -> 2
//     | C3 s -> s
//     | C4 -> 5
//     | _ -> 10

// type Foo1(i) =
//     member x.Foo() = i
//     member inline x.Foo(j) = i * j

// type Foo2(i) =
//     member inline x.Foo(j) = (i + j) * 2

// let inline foo< ^t when ^t : (member Foo : int -> int)> x i =
//     (^t : (member Foo : int -> int) (x, i))

// let ``Local inline typed lambdas work``() =
//     let inline localFoo (x:^t) = foo x 5
//     let x1 = Foo1(2)
//     let x2 = Foo2(3)
//     localFoo x1 + localFoo x2
//     // foo x1 5 + foo x2 10

// type C() =
//     member __.Bar = "bar"

// type A(s: string) =
//     member __.Foo = s
//     new () = A("jaja")
//     new (i: int) = if i <= 0 then A(C().Bar) else A(string i)

// type B(i: int) =
//     inherit A(i)

[<AbstractClass>]
type D(i) =
    abstract Value: int
    member this.Add(x) = this.Value + x + i

let test234918() =
    let d = { new D(5) with
                override __.Value = 60 }
    d.Add(25) |> log

test234918()

// type E(j) =
//     inherit D()
//     override this.Value = j

// JS.console.log(E(5).Add(6))

// let ``ParamArray in object expression works``() =
//    let mutable ja = 0.
//    let o =
//     { new IFoo with
//         member __.Foo = ja
//         member this.Foo2 = this.Foo + this.Foo
//         member __.FooBar with set v = ja <- v
//         member __.Bar(s: string, [<ParamArray>] rest: int[]) =
//             s + !!rest.[0] + !!rest.[1]
//     }
//    o.Bar("{0} + {0} = {1}", 2, 4)
