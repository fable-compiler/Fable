module Fable.Tests.JsInterop

open System
open Util.Testing

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental

let inline getNameofLambda (f: 'T->'U) =
    nameofLambda f

[<Global>]
module GlobalModule =
    [<Emit("var GlobalModule = { add(x, y) { return x + y }, foo: 'bar' }")>]
    let declare() = ()
    let add (x: int) (y: int): int = jsNative
    let foo: string = jsNative

GlobalModule.declare()

let myMeth (x: int) (y: int) = x - y

type IMyOptions =
    interface end

type MyOptions =
    | Flag1
    | Name of string
    | [<CompiledName("Foo")>] QTY of int
    interface IMyOptions

type MyOptions2 =
    | Bar of int*int
    interface IMyOptions

type MyOptions3 =
    | Bar2 of int*int
    | Foo2 of MyOptions list

type NameProp =
    { Name: string }

type Props =
    | Names of NameProp array
    | [<Erase>] Custom of key:string * value:obj

let [<Emit("arguments.length")>] argCount: int = jsNative

[<Erase>]
type ErasedUnion =
    | ErasedInt of int
    | ErasedString of string

[<Erase>]
type ErasedUnionWithMultipleFields =
    | ErasedUnionWithMultipleFields of string * int

type TextStyle =
    [<Emit("\"foo\"")>]
    abstract Bar : string
    [<Emit("$1+$2")>]
    abstract Add : int*int -> int
    [<Emit("$0[$1]{{=$2}}")>]
    abstract Item : int->string option with get, set
    [<Emit("$0.fontFamily{{=$1}}")>]
    abstract FontFamily : string option with get, set
    [<Emit("{{$1?\"foo\":\"bar\"}}")>]
    abstract FontSize : bool -> string

type InnerRecord = {
    Float: float
}

type Record = {
    String: string
    Int: int
    InnerRecord: InnerRecord
}

[<StringEnum>]
type MyStrings =
    | Vertical
    | [<CompiledName("Horizontal")>] Horizontal

[<StringEnum(CaseRules.SnakeCase); RequireQualifiedAccess>]
type UserInfo =
    | UserLoginCount

[<StringEnum(CaseRules.SnakeCaseAllCaps); RequireQualifiedAccess>]
type UserInfo2 =
    | UserLoginCount

[<StringEnum(CaseRules.KebabCase)>]
type MyCssOptions =
    | ContentBox
    | BorderBox

[<StringEnum>]
#endif
type Field = OldPassword | NewPassword | ConfirmPassword

type MyInterface =
    abstract foo: int
    abstract bar: string

let validatePassword = function
    | OldPassword -> "op"
    | NewPassword -> "np"
    | ConfirmPassword -> "cp"

type JsOptions =
    abstract foo: string with get, set
    abstract bar: int with get, set

let tests =
  testList "JsInterop" [
#if FABLE_COMPILER
    testCase "Dynamic application works" <| fun () ->
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
        dynObj?add(2,2) |> equal 4
        // Accessing 2-level deep property
        dynObj?child?multiply(3,2) |> equal 6
        // Dynamic application chaining
        dynObj?fn()?subtract(3,2) |> equal 1
        // Using $ operator
        dynObj?add $ (2,2) |> equal 4
        dynObj?foo |> equal "foo"
        // Delegates are not modified when applied dynamically
        let del = Func<_,_,_>(fun x y -> y - x)
        dynObj?apply(del) |> equal 1
        dynObj?apply(Func<_,_,_>(fun x y -> x - y)) |> equal -1

        // TODO: This doesn't work in Fable 2, fix it or leave it as is?
        // Assigning dynamic access result to a value and applying multiple args
        // let add = dynObj?add in add(3,4) |> equal 7

    testCase "Lambdas are converted to delegates with dynamic operators" <| fun () ->
        let o =
            createObj [
                "bar" ==> fun x y z -> x * y * z
                "bar2" ==> myMeth
                "apply2and5" ==> fun (f: Func<int,int,int>) -> f.Invoke(2,5)
            ]
        o?bar(1,2,3) |> equal 6
        o?bar2(5,2) |> equal 3

        o?apply2and5(fun x y -> x + y) |> equal 7

        let f = unbox<obj> o?apply2and5
        f $ (fun x y -> x * y) |> equal 10

        o?foo <- fun x y -> x / y
        o?foo(25, 5) |> equal 5

    testCase "KeyValueList works at compile time" <| fun () ->
        let opts =
            [ Name "Fable" :> IMyOptions
            ; QTY 5 :> IMyOptions
            ; Flag1 :> IMyOptions
            ; Bar(2,3) :> IMyOptions ]
            |> keyValueList CaseRules.LowerFirst
        opts?name |> unbox |> equal "Fable"
        opts?foo |> unbox |> equal 5
        opts?flag1 |> unbox |> equal true
        opts?bar?(1) |> unbox |> equal 3
        let opts2 = keyValueList CaseRules.None [ Name "Fable"]
        opts2?Name |> unbox |> equal "Fable"

    testCase "KeyValueList works at runtime" <| fun () ->
        let buildAtRuntime = function
            | null | "" -> Flag1 :> IMyOptions
            | name -> Name name :> IMyOptions
        let opts =
            [ buildAtRuntime "Fable"
            ; QTY 5 :> IMyOptions
            ; Bar(2,3) :> IMyOptions
            ; buildAtRuntime ""]
            |> keyValueList CaseRules.LowerFirst
        opts?name |> unbox |> equal "Fable"
        opts?foo |> unbox |> equal 5
        opts?flag1 |> unbox |> equal true
        opts?bar?(0) |> unbox |> equal 2
        let opts2 = keyValueList CaseRules.None [ buildAtRuntime "Fable"]
        opts2?Name |> unbox |> equal "Fable"

    // TODO: Make sublists work automatically with KeyValueList?
    // testCase "KeyValueList works with sublists" <| fun () ->
    //     let opts1 =
    //         [ Bar2(2,3)
    //           Foo2 [ Name "Fable"
    //                  QTY 5
    //                  Flag1 ] ]
    //         |> keyValueList CaseRules.LowerFirst
    //     let opts2 =
    //         let subList =
    //             [ Name "Fábula"
    //               QTY 5
    //               Flag1 ]
    //         [ Bar2(2,3)
    //           Foo2 subList ]
    //         |> keyValueList CaseRules.LowerFirst
    //     opts1?foo2?name |> unbox |> equal "Fable"
    //     opts1?foo2?foo |> unbox |> equal 5
    //     opts1?foo2?flag1 |> unbox |> equal true
    //     opts1?bar2?(0) |> unbox |> equal 2
    //     opts2?foo2?name |> unbox |> equal "Fábula"
    //     opts2?foo2?foo |> unbox |> equal 5
    //     opts2?foo2?flag1 |> unbox |> equal true
    //     opts2?bar2?(1) |> unbox |> equal 3

    testCase "Array inside keyValueList is preserved" <| fun () ->
        let props = [ Names [| { Name = "name" } |] ]
        let actual = [ Names [| { Name = "name" } |] ] |> keyValueList CaseRules.LowerFirst |> JS.JSON.stringify
        let expected = props |> keyValueList CaseRules.LowerFirst |> JS.JSON.stringify
        actual |> equal expected

    testCase "Erased union cases work with keyValueList" <| fun () ->
        let props: Props list = [ Custom("Foo", 5); Names [|{Name = "Mikhail"}|] ]
        let compiletime = [Custom("Bar", 10); Names [|{Name = "Mikhail"}|]]
                          |> keyValueList CaseRules.LowerFirst
        let expected = props |> keyValueList CaseRules.LowerFirst
        compiletime?Bar |> equal 10
        expected?Foo |> equal 5
        compiletime?names?(0)?Name |> equal "Mikhail"
        compiletime?names?(0)?Name |> equal "Mikhail"

    testCase "Dynamic casting works with keyValueList" <| fun () ->
        let props: Props list = [ !!("Foo", 5); Names [|{Name = "Mikhail"}|] ]
        let compiletime = [ (!!("Bar", 10): Props); Names [|{Name = "Mikhail"}|] ]
                          |> keyValueList CaseRules.LowerFirst
        let expected = props |> keyValueList CaseRules.LowerFirst
        compiletime?Bar |> equal 10
        expected?Foo |> equal 5
        compiletime?names?(0)?Name |> equal "Mikhail"
        compiletime?names?(0)?Name |> equal "Mikhail"

    testCase "Cast an anonymous record to an interface" <| fun () ->
        // The first three must raise warnings
        // let x: MyInterface = !!{| foo = "4"; bar = "5" |}
        // let y: MyInterface = !!{| foo = 4 |}
        // let z: MyInterface = !!{| foo = 4; bAr = "5" |}
        let u: MyInterface = !!{| foo = 4; bar = "5" |}
        equal 4 u.foo
        equal "5" u.bar

    testCase "Unit argument is not replaced by null in dynamic programming" <| fun () ->
        let o = createObj ["foo" ==> fun () -> argCount]
        o?foo() |> equal 0
        let f = box o?foo
        f$() |> equal 0
        (f :?> JsFunc).Invoke() |> unbox<int> |> equal 0

    testCase "jsThis works" <| fun () ->
        let o = createObj [
            "z" ==> 5
            "add" ==> fun x y -> x + y + jsThis?z
        ]
        o?add(2,3) |> equal 10

    testCase "Erase attribute works" <| fun () ->
        let convert = function
            | ErasedInt i -> string(i * 2)
            | ErasedString s -> "x" + s + "x"
        ErasedInt 4 |> convert |> equal "8"
        ErasedString "ab" |> convert |> equal "xabx"

    testCase "Erased unions with multiple fields work" <| fun _ ->
        let gimme (ErasedUnionWithMultipleFields(s, i)) =
            sprintf "Gimme %i %ss" i s
        ("apple", 5)
        |> ErasedUnionWithMultipleFields
        |> gimme
        |> equal "Gimme 5 apples"

    testCase "Emit attribute works" <| fun () ->
        let style = createEmpty<TextStyle>
        style.Bar |> equal "foo"
        style.Add(3,5) |> equal 8

    testCase "Assigning null with emit works" <| fun () ->
        let x = createEmpty<obj>
        x.["prop"] <- "prop value"
        x.["prop"] |> isNull |> equal false
        x.["prop"] <- null
        x.["prop"] |> equal null

    testCase "Emit attribute conditional parameters works" <| fun () ->
        let style = createEmpty<TextStyle>
        style.FontFamily <- Some "ha"
        style.FontFamily |> equal (Some "ha")
        !!style?fontFamily |> equal "ha"
        style.[5] <- Some "ho"
        style.[5] |> equal (Some "ho")
        style.[10] |> equal None

    testCase "Emit attribute conditional syntax works" <| fun () ->
        let style = createEmpty<TextStyle>
        style.FontSize(true) |> equal "foo"
        style.FontSize(false) |> equal "bar"

    testCase "nameof works" <| fun () ->
        let record = { String = ""; Int = 0; InnerRecord = { Float = 5.0 } }
        nameof(record) |> equal "record"
        nameof(record.String) |> equal "String"
        nameof(record.Int) |> equal "Int"
        nameof(record.InnerRecord) + "." + nameof(record.InnerRecord.Float)
        |> equal "InnerRecord.Float"
        nameof(typeof<Record>) |> equal "Record"
        nameof(typeof<InnerRecord>) |> equal "InnerRecord"

    testCase "nameofLambda works" <| fun () ->
        nameofLambda(fun (x:Record) -> x.String) |> equal "String"
        nameofLambda(fun (x:Record) -> x.Int) |> equal "Int"

    testCase "nameofLambda works in inlined functions" <| fun () ->
        getNameofLambda (fun (x:Record) -> x.String) |> equal "String"

    testCase "StringEnum attribute works" <| fun () ->
        Vertical |> unbox |> equal "vertical"
        Horizontal |> unbox |> equal "Horizontal"
        Vertical |> string |> equal "vertical"
        Horizontal |> string |> equal "Horizontal"

    testCase "StringEnum works with CaseRules.SnakeCase" <| fun () ->
        UserInfo.UserLoginCount |> unbox |> equal "user_login_count"

    testCase "StringEnum works with CaseRules.SnakeCaseAllCaps" <| fun () ->
        UserInfo2.UserLoginCount |> unbox |> equal "USER_LOGIN_COUNT"

    testCase "StringEnum works with CaseRules.KebabCase" <| fun () ->
        BorderBox |> unbox |> equal "border-box"
        ContentBox |> unbox |> equal "content-box"

    // See https://github.com/fable-compiler/fable-import/issues/72
    testCase "Can use values and functions from global modules" <| fun () ->
        GlobalModule.add 3 4 |> equal 7
        GlobalModule.foo |> equal "bar"

    testCase "Local import with curried signatures works" <| fun () ->
        let add (x:int) (y:int): int = importMember "./js/1foo.js"
        3 |> add 2 |> equal 5

    testCase "TypedArray element can be set and get using index" <| fun () ->
        let arr = JS.Uint8Array.Create(5)
        arr.[0] <- 5uy
        equal 5uy arr.[0]

    testCase "jsOptions works" <| fun _ ->
        let opts = jsOptions<JsOptions>(fun o ->
            o.foo <- "bar"
            o.bar <- 5)
        opts.foo |> equal "bar"
        opts.bar |> equal 5
#endif

    testCase "Pattern matching with StringEnum works" <| fun () ->
        validatePassword NewPassword
        |> equal "np"
  ]
