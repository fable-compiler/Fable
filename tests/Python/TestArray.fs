module Fable.Tests.Arrays

open System
open Util.Testing
open Fable.Tests.Util

type ParamArrayTest =
    static member Add([<ParamArray>] xs: int[]) = Array.sum xs

let add (xs: int[]) = ParamArrayTest.Add(xs)

#if FABLE_COMPILER
open Fable.Core

[<Emit("$1.constructor.name == $0")>]
let jsConstructorIs (s: string) (ar: 'T[]) = true
#endif

type ExceptFoo = { Bar:string }

let f (x:obj) (y:obj) (z:obj) = (string x) + (string y) + (string z)

let map f ar = Array.map f ar

type Point =
    { x: int; y: int }
    static member Zero = { x=0; y=0 }
    static member Neg(p: Point) = { x = -p.x; y = -p.y }
    static member (+) (p1, p2) = { x= p1.x + p2.x; y = p1.y + p2.y }

type MyNumber =
    | MyNumber of int
    static member Zero = MyNumber 0
    static member (+) (MyNumber x, MyNumber y) =
        MyNumber(x + y)
    static member DivideByInt (MyNumber x, i: int) =
        MyNumber(x / i)

type MyNumberWrapper =
    { MyNumber: MyNumber }

type Things =
    { MainThing: int
      OtherThing: string }

[<Fact>]
let ``test Pattern matching with arrays works`` () =
    match [||] with [||] -> true | _ -> false
    |> equal true
    match [|1|] with [||] -> 0 | [|x|] -> 1 | _ -> 2
    |> equal 1
    match [|"a";"b"|] with [|"a";"b"|] -> 1 | _ -> 2
    |> equal 1

[<Fact>]
let ``test ParamArrayAttribute works`` () =
    ParamArrayTest.Add(1, 2) |> equal 3

[<Fact>]
let ``test Passing an array to ParamArrayAttribute works`` () =
    ParamArrayTest.Add([|3; 2|]) |> equal 5

[<Fact>]
let ``test Passing an array to ParamArrayAttribute from another function works`` () =
    add [|5;-7|] |> equal -2

[<Fact>]
let ``test Can spread a complex expression`` () =
    let sideEffect (ar: int[]) = ar.[1] <- 5
    ParamArrayTest.Add(let ar = [|1;2;3|] in sideEffect ar; ar)
    |> equal 9

[<Fact>]
let ``test Mapping from values to functions works`` () =
    let a = [| "a"; "b"; "c" |]
    let b = [| 1; 2; 3 |]
    let concaters1 = a |> Array.map (fun x y -> y + x)
    let concaters2 = a |> Array.map (fun x -> (fun y -> y + x))
    let concaters3 = a |> Array.map (fun x -> let f = (fun y -> y + x) in f)
    let concaters4 = a |> Array.map f
    let concaters5 = b |> Array.mapi f
    concaters1.[0] "x" |> equal "xa"
    concaters2.[1] "x" |> equal "xb"
    concaters3.[2] "x" |> equal "xc"
    concaters4.[0] "x" "y" |> equal "axy"
    concaters5.[1] "x" |> equal "12x"
    let f2 = f
    a |> Array.mapi f2 |> Array.item 2 <| "x" |> equal "2cx"

[<Fact>]
let ``test Mapping from typed arrays to non-numeric arrays doesn't coerce values`` ()=
    let xs = map string [|1;2|]
    (box xs.[0]) :? string |> equal true
    let xs2 = Array.map string [|1;2|]
    (box xs2.[1]) :? string |> equal true

[<Fact>]
let ``test Array slice with upper index work`` () =
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[..2] |> Array.sum |> equal 6
    xs.[..2] <- ys
    xs |> Array.sum |> equal 39

[<Fact>]
let ``test Array slice with lower index work`` () =
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[4..] |> Array.sum |> equal 11
    xs.[4..] <- ys
    xs |> Array.sum |> equal 26

[<Fact>]
let ``test Array slice with both indices work`` () =
    let xs = [| 1; 2; 3; 4; 5; 6 |]
    let ys = [| 8; 8; 8; 8; 8; 8; 8; 8; |]
    xs.[1..3] |> Array.sum |> equal 9
    xs.[1..3] <- ys
    xs |> Array.sum |> equal 36

[<Fact>]
let ``test Array slice with non-numeric arrays work`` () =
    let xs = [|"A";"B";"C";"D"|]
    xs.[1..2] <- [|"X";"X";"X";"X"|]
    equal xs.[2] "X"
    equal xs.[3] "D"
