module Fable.Tests.Closure

open Util.Testing

let map f x =
    f x

let staticFnPassthrough x = x   //uniform parameters

let staticFnAdd1 x = x + 1

[<Fact>]
let ``fn as param should also accept static functions`` () =
    let a = 3
    let b = 2
    let w = {|X = 1|}

    a |> equal 3
    b |> equal 2
    a |> map staticFnAdd1 |> equal 4
    b |> map staticFnAdd1 |> equal 3
    a |> map staticFnPassthrough |> equal 3
    let wRes = w |> map staticFnPassthrough
    wRes.X |> equal 1


[<Fact>]
let ``Closure captures trivial case variable and does not break borrow checker`` () =
    let a = 3
    let b = 2

    let res = a |> map (fun x -> b + x)//b is captured, so it is borrowed
    a |> equal 3
    b |> equal 2
    res |> equal 5

type Wrapped = {
    Value: string
}

[<Fact>]
let ``Closure captures and clones`` () =
    let a = { Value = "a" }
    let b = { Value = "b" }

    let res1 = a |> map (fun x -> x.Value + b.Value)//capture b, clone
    let res2 = a |> map (fun x -> x.Value + b.Value + "x")//capture b, clone
    res1 |> equal "ab"
    res2 |> equal "abx"

[<Fact>]
let ``Closure can be declared locally and passed to a fn`` () =

    let x = "x"
    let cl s = s + x

    let res1 = "a." |> map (cl)//capture b, clone
    let res2 = "b." |> map (cl)//capture b, clone
    x |> equal "x"// prevent inlining
    res1 |> equal "a.x"
    res2 |> equal "b.x"

[<Fact>]
let ``Closure can close over another closure and call`` () =
    let x = "x"
    let cl1 s = s + x
    let cl2 s = cl1 s + x

    let res1 = "a." |> map (cl2)//capture b, clone
    let res2 = "b." |> map (cl2)//capture b, clone
    let res3 = "c." |> map (cl1)//capture b, clone
    x |> equal "x"// prevent inlining
    res1 |> equal "a.xx"
    res2 |> equal "b.xx"
    res3 |> equal "c.x"

[<Fact>]
let ``Closures can accept multiple params`` () =
    let x = { Value = "x"}
    let cl a b c =
        (a + b + c + x.Value)

    let res1 = cl "a" "b" "c"
    let res2 = cl "d" "e" "f"

    x.Value |> equal "x" // prevent inlining
    res1 |> equal "abcx"
    res2 |> equal "defx"


[<Fact>]
let ``parameterless closure works - unit type in`` () =
    let x = { Value = "x"}
    let cl () =
        ("closed." + x.Value)

    let res1 = cl()
    let res2 = cl()
    x.Value |> equal "x" // prevent inlining
    res1 |> equal "closed.x"
    res2 |> equal "closed.x"

// TODO : mutable x probably needs to be a Arc<RefCell<T>>?
// TODO : Support unit return type
// [<Fact>]
// let ``Mutable capture works`` () =
//     let mutable x = 0
//     let incrementX () =
//         x <- x + 1
//         ()

//     incrementX()
//     x |> equal 1
//     incrementX()
//     x |> equal 2
//     incrementX()
//     x |> equal 3

type MutWrapped = {
    mutable MutValue: int
}

[<Fact>]
let ``Capture works with type with interior mutability`` () =
    let x = { MutValue = 0 }
    let incrementX () =
        x.MutValue <- x.MutValue + 1
        0// TODO : support unit

    incrementX()
    x.MutValue |> equal 1
    incrementX()
    x.MutValue |> equal 2
    incrementX()
    x.MutValue |> equal 3

// type CTest(x: int, y: string) =
//     let a = x + x
//     let y = y
//     member this.Add m = a + m

// [<Fact>]
// let ``Class hellp`` () =
//     let a = CTest(1, "hello")
//     let r = a.Add(1)
//     r |> equal 3