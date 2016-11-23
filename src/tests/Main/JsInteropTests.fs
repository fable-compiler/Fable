[<Util.Testing.TestFixture>]
module Fable.Tests.JsInterop

open System
open Util.Testing
open Fable.Tests.Util

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

[<Test>]
let ``Dynamic application works``() =
    let dynObj =
        createObj [
            "add" ==> Func<_,_,_>(fun x y -> x + y)
            "fn" ==> fun () ->
                createObj [
                    "subtract" ==> Func<_,_,_>(fun x y -> x - y)
                ]
            "child" ==>
                createObj [
                    "multiply" ==> Func<_,_,_>(fun x y -> x * y)
                ]
            "foo" ==> "foo"
            "apply" ==> fun (del: Func<int,int,int>) -> del.Invoke(2,3)
        ]
    dynObj?add(2,2) |> equal (box 4)
    // Assigning dynamic access result to a value
    let add = dynObj?add
    add(3,4) |> equal (box 7)
    // Accessing 2-level deep property
    dynObj?child?multiply(3,2) |> equal (box 6)
    // Dynamic application chaining
    dynObj?fn()?subtract(3,2) |> equal (box 1)
    // Using $ operator
    dynObj?add $ (2,2) |> equal (box 4)
    dynObj?foo |> unbox |> equal "foo"
    // Delegates are not modified when applied dynamically
    let del = Func<_,_,_>(fun x y -> y - x)
    dynObj?apply(del) |> unbox |> equal 1
    dynObj?apply(Func<_,_,_>(fun x y -> x - y)) |> unbox |> equal -1

let myMeth (x: int) (y: int) = x - y

[<Test>]
let ``Lambdas are converted to delegates with dynamic operators``() =
    let o =
        createObj [
            "bar" ==> fun x y z -> x * y * z
            "bar2" ==> myMeth
            "apply2and5" ==> fun (f: Func<int,int,int>) -> f.Invoke(2,5)
        ]
    o?bar(1,2,3) |> unbox<int> |> equal 6
    o?bar2(5,2) |> unbox<int> |> equal 3

    o?apply2and5(fun x y -> x + y) |> unbox<int> |> equal 7

    let f = unbox<obj> o?apply2and5
    f $ (fun x y -> x * y) |> unbox<int> |> equal 10

    o?foo <- fun x y -> x / y
    o?foo(25, 5) |> unbox<int> |> equal 5

// [<Test>]
// let ``JS accepts any object as exception``() =
//     try
//         let o = createObj [ "foo" ==> 3 ]
//         raise(unbox o)
//     with ex -> ex.Message
//     |> equal """{"foo":3}"""

[<KeyValueList>]
type MyOptions =
    | Flag1
    | Name of string
    | [<CompiledName("QTY")>] QTY of int

[<Test>]
let ``KeyValueList attribute works at compile time``() =
    let opts = [
        Name "Fable"
        QTY 5
        Flag1
    ]
    opts?name |> unbox |> equal "Fable"
    opts?QTY |> unbox |> equal 5
    opts?flag1 |> unbox |> equal true

[<Test>]
let ``KeyValueList attribute works at runtime``() =
    let buildAtRuntime = function
        | null | "" -> Flag1
        | name -> Name name
    let opts = [
        buildAtRuntime "Fable"
        QTY 5
        buildAtRuntime ""
    ]
    opts?name |> unbox |> equal "Fable"
    opts?QTY |> unbox |> equal 5
    opts?flag1 |> unbox |> equal true

[<StringEnum>]
type MyStrings =
    | Vertical
    | [<CompiledName("Horizontal")>] Horizontal

[<Test>]
let ``StringEnum attribute works``() =
    Vertical |> unbox |> equal "vertical"
    Horizontal |> unbox |> equal "Horizontal"

[<StringEnum>]
#endif
type Field = OldPassword | NewPassword | ConfirmPassword

let validatePassword = function
    | OldPassword -> "op"
    | NewPassword -> "np"
    | ConfirmPassword -> "cp"

[<Test>]
let ``Pattern matching with StringEnum works``() =
    validatePassword NewPassword
    |> equal "np"

