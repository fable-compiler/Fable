module Fable.Tests.ByRefTests

open Util.Testing
open Fable.Core
open Fable.Core.Rust

type Obj = {
    X: int
}

[<ByRef>]
let byRefArgsIntFn (x: int) (y: int) = x + y

[<ByRef>]
let byRefArgsObjFn (x: Obj) (y: int) = x.X + y

let byRefTypeIntFn (x: int inref) (y: int) = x + y
let byRefTypeObjFn (x: Obj inref) (y: int) = x.X + y

let byRefAttrIntFn ([<ByRef>] x: int) (y: int) = x + y
let byRefAttrObjFn ([<ByRef>] x: Obj) (y: int) = x.X + y

let byValArgsIntFn (x: int) (y: int) = x + y
let byValArgsObjFn (x: Obj) (y: int) = x.X + y

[<Emit("byRefArgsIntFn(&$0, &$1)")>]
let callByRefArgsIntFn x y = nativeOnly

[<Emit("byRefTypeIntFn(&$0, $1)")>]
let callByRefTypeIntFn x y = nativeOnly

[<Emit("byRefAttrIntFn(&$0, $1)")>]
let callByRefAttrIntFn x y = nativeOnly

[<Emit("byValArgsIntFn($0, $1)")>]
let callByValArgsIntFn x y = nativeOnly

// this should break if parameters are no longer passed properly
let ensureIntRefsAreActuallyExpectedTypecheck () =
    callByRefArgsIntFn 1 2 |> ignore
    callByRefTypeIntFn 1 2 |> ignore
    callByRefAttrIntFn 1 2 |> ignore
    callByValArgsIntFn 1 2 |> ignore

[<Fact>]
let ``pass int by value works`` () =
    let a = 1
    byValArgsIntFn a 2 |> equal 3
    a |> equal 1

[<Fact>]
let ``pass obj by value works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    let c = 1
    byValArgsObjFn a c |> equal 2
    byValArgsObjFn b c |> equal 3
    a.X |> equal 1

[<Fact>]
let ``pass int by ref works`` () =
    let a = 1
    byRefTypeIntFn &a 2 |> equal 3
    a |> equal 1

[<Fact>]
let ``pass obj by ref works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    let c = 1
    byRefTypeObjFn &a c |> equal 2
    byRefTypeObjFn &b c |> equal 3
    a.X |> equal 1

[<Fact>]
let ``pass obj by ref using attr on fn works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    let c = 1
    byRefArgsObjFn a c |> equal 2
    byRefArgsObjFn b c |> equal 3
    a.X |> equal 1

[<Fact>]
let ``pass int by ref using ByRef attr works`` () =
    let a = 1
    let b = 2
    byRefAttrIntFn a b |> equal 3
    a |> equal 1

[<Fact>]
let ``pass obj by ref using ByRef attr works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    let c = 1
    byRefAttrObjFn a c |> equal 2
    byRefAttrObjFn b c |> equal 3
    a.X |> equal 1


// // TODO: passing byref into inref not working yet
// let addInRef x (y: inref<int>): int = x + y
// let addByRef x (y: byref<int>): int = addInRef x &y

// [<Fact>]
// let ``passing byref into inref works`` () =
//     let mutable y = 1
//     addByRef y &y |> equal 2

// // TODO: byref as return body not working yet
// let idByRef (x: int byref): int = x

// [<Fact>]
// let ``return byref as body works`` () =
//     let mutable y = 1
//     idByRef &y |> equal 1

// // TODO: byref as return type not working yet
// let retByRef (x: int byref): int byref = &x

// [<Fact>]
// let ``byref as return type works`` () =
//     let mutable y = 1
//     retByRef &y |> equal 1
