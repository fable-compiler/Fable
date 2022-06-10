module Fable.Tests.ByRefTests

open Util.Testing

type Obj = {
    X: int
}

let byrefIntFn (x: int inref) =
    x + 1

[<Fact>]
let ``pass int by ref works`` () =
    let a = 1
    byrefIntFn &a |> equal 2
    a |> equal 1 // a is not modified & prevent inlining

let byrefObjFn (x: Obj inref) =
    x.X + 1

[<Fact>]
let ``pass obj by ref works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    byrefObjFn &a |> equal 2
    byrefObjFn &b |> equal 3
    a |> equal a //prevent inlining

[<Fable.Core.ByRef>]
let byrefAttrRootObjDecFn (x: Obj) =
    x.X + 1

[<Fact>]
let ``pass obj by ref using attr on fn works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    byrefAttrRootObjDecFn a |> equal 2
    byrefAttrRootObjDecFn b |> equal 3
    a |> equal a //prevent inlining

// TODO: ByRef as Param not yet working

// let byrefAttrIntFn ([<Fable.Core.ByRef>] x: int) =
//     x + 1

// [<Fact>]
// let ``pass int by ref using attr works`` () =
//     let a = 1
//     byrefAttrIntFn a |> equal 2
//     a |> equal 1 // a is not modified & prevent inlining

// let byrefAttrObjFn ([<Fable.Core.ByRef>] x: Obj) =
//     x.X + 1

// [<Fact>]
// let ``pass obj by ref using attr works`` () =
//     let a = { X = 1 }
//     let b = { X = 2 }
//     byrefAttrObjFn a |> equal 2
//     byrefAttrObjFn b |> equal 3
//     a |> equal a //prevent inlining

// TODO: passing byref into inref not working yet
// let add1 x (y: inref<int>) = x + y
// let add2 x (y: byref<int>) = add1 x &y

// TODO: byref as return body not yet working
// let byrefIdentity (x: int byref): int = x

// TODO: byref as return type not yet working
// let byrefReturns (x: int byref): int byref = &x
