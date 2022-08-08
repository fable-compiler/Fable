module Fable.Tests.ByRefTests

open Util.Testing
open Fable.Core.Rust

type Obj = {
    X: int
}

let byrefIntFn (x: int inref) =
    x + 1
let byrefObjFn (x: Obj inref) =
    x.X + 1

[<ByRef>]
let byrefAttrRootObjDecFn (x: Obj) =
    x.X + 1
[<ByRef>]
let byrefAttrRootIntFn (x: int) =
    x + 1

let byrefAttrIntFn ([<ByRef>] x: int) =
    x + 1
let byrefAttrObjFn ([<ByRef>] x: Obj) =
    x.X + 1

[<Fable.Core.Emit("byrefAttrRootIntFn(&$0)")>]
let callByRefAttrRootIntFn i = i
[<Fable.Core.Emit("byrefAttrIntFn(&$0)")>]
let callByRefAttrIntFn i = i

// the intent of doing this is that this will break if there is some regression that the parameters no longer are passed by ref
let ensureIntRefsAreActuallyExpectedTypecheck () =
    callByRefAttrRootIntFn 1 |> ignore
    callByRefAttrIntFn 1 |> ignore

[<Fact>]
let ``pass int by ref works`` () =
    let a = 1
    byrefIntFn &a |> equal 2
    a |> equal 1 // a is not modified & prevent inlining





[<Fact>]
let ``pass obj by ref works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    byrefObjFn &a |> equal 2
    byrefObjFn &b |> equal 3
    a |> equal a //prevent inlining

[<Fact>]
let ``pass obj by ref using attr on fn works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    byrefAttrRootObjDecFn a |> equal 2
    byrefAttrRootObjDecFn b |> equal 3
    a |> equal a //prevent inlining



[<Fact>]
let ``pass int by ref using ByRef attr works`` () =
    let a = 1
    byrefAttrIntFn a |> equal 2
    a |> equal 1 // a is not modified & prevent inlining



[<Fact>]
let ``pass obj by ref using ByRef attr works`` () =
    let a = { X = 1 }
    let b = { X = 2 }
    byrefAttrObjFn a |> equal 2
    byrefAttrObjFn b |> equal 3
    a |> equal a //prevent inlining


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
