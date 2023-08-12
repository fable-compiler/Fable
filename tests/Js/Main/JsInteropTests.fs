module Fable.Tests.JsInterop

open System
open Util.Testing
open Fable.Core

#if FABLE_COMPILER
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental

importSideEffects "./js/polyfill.js"

// Make sure interface names don't conflict with values in JS, see #2864
type DoesNotConflict =
    interface end

let DoesNotConflict = 5

let logs = ResizeArray()

type LogAttribute(msg1: string, code: int) =
    inherit JS.DecoratorAttribute()
    override _.Decorate(fn) =
        let mutable count = 0
        JS.spreadFunc(fun args ->
            count <- count + 1
            logs.Add $"LOG1: [%s{msg1} (code %i{code})] called %i{count} time(s)!"
            fn.apply(null, args))

type Log2Attribute() =
    inherit JS.DecoratorAttribute()
    override _.Decorate(fn) =
        let mutable count = 0
        JS.spreadFunc(fun args ->
            count <- count + 1
            logs.Add $"LOG2: called %i{count} time(s)!"
            fn.apply(null, args))

type Log3Attribute() =
    inherit JS.ReflectedDecoratorAttribute()
    override _.Decorate(fn, info) =
        logs.Add $"{info.Name}: {info.ReturnType}"
        for p in info.GetParameters() do
            logs.Add $"{p.Name}: {p.ParameterType}"
        JS.spreadFunc(fun args ->
            fn.apply(null, args))

[<Log("MATH", 3)>]
[<Log2>]
[<Log3>]
let myComplexAdder x y = x + y

type GenericInfoAttribute() =
    inherit JS.ReflectedDecoratorAttribute()
    override _.Decorate(fn, info) =
        let genNames = ("", info.GetParameters()) ||> Seq.fold (fun acc p ->
            acc + p.ParameterType.Name)
        JS.spreadFunc(fun args ->
            genNames + fn.apply(null, args).ToString() |> box)

// type GenericType =
//     [<GenericInfo>]
//     static member GenericMethod(value1: 'X, value2: 'Y): string = "foo"

let inline getNameofLambda (f: 'T->'U) =
    nameofLambda f

let inline getNamesofLambda (f: 'T->'U) =
    namesofLambda f

[<Global>]
module GlobalModule =
    [<Emit("var GlobalModule = { add(x, y) { return x + y }, foo: 'bar' }")>]
    let declare() = ()
    let add (x: int) (y: int): int = jsNative
    let foo: string = jsNative

GlobalModule.declare()

module GlobalClasses =
    [<Global>]
    type MyHTMLElement(v: int) =
        member _.getMotivated(): string = jsNative
        static member lunchTime: string = jsNative

type MyCustomHTMLElement() =
    inherit GlobalClasses.MyHTMLElement(3)
    member this.SaySomethingNiceTo(name: string) =
        $"Repeat with me {name}: {this.getMotivated()}"

let inline sqrtJsExpr (jsExpr: string): float =
    emitJsExpr () ("Math.sqrt(" + jsExpr + ")")

let doSomethingInJs (x: int) (y: float) (z: float): string =
    emitJsStatement () $"""
    let acc = 0;
    for (let i = 0; i < {min x 10}; i++) {{
        acc += {y ** 2} + {x + 3} * 2;
    }}
    return acc + {sprintf "%.2f" z};
    """

let doSomethingInFSharp (x: int) (y: float) (z: float): string =
    let mutable acc = 0.
    for _ = 0 to (min x 10) - 1 do
        acc <- acc + (y ** 2) + (float x + 3.) * 2.
    string acc + sprintf "%.2f" z

let myMeth (x: int) (y: int) = x - y

let curry2 (f: 'a -> 'b -> 'c) = f
let inline curryInline2 (f: 'a -> 'b -> 'c) = f

type Curry<'T1, 'T2> = delegate of 'T1 -> 'T2

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

[<Erase>]
type ErasedUnion =
    | ErasedInt of int
    | ErasedString of string
    member this.SayHi() =
        match this with
        | ErasedInt i -> sprintf "Hi %i time(s)!" i
        | ErasedString s -> sprintf "Hi %s!" s

[<Erase>]
type ErasedUnionWithMultipleFields =
    | ErasedUnionWithMultipleFields of string * int

[<Erase; RequireQualifiedAccess>]
type MyErasedUnion2 =
    | Foo
    | Ohmy
    | Bar of float
    | Baz of int[]

[<Erase(CaseRules.KebabCase); RequireQualifiedAccess>]
type MyErasedUnion3 =
    | FooBar
    | OhMyDear
    | AnotherNumber of int

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
    [<Emit("$1 + [].concat($2...).reduce((x, y) => x + y)")>]
    abstract Sum: f: float * [<ParamArray>] ints: int[] -> float
    [<Emit("$1($2...)")>]
    abstract Apply: f: obj * [<ParamArray>] args: obj[] -> obj

type InnerInnerRecord = {
    Bar: string
}

type InnerRecord = {
    Float: float
    Foo: InnerInnerRecord
} with
    override this.ToString() =
        sprintf "%.0fx" (this.Float * 5.)

type Record = {
    String: string
    Int: int
    InnerRecord: InnerRecord
}

[<Global("Array")>]
type JsArray =
    abstract push: item: obj -> unit
    abstract length: int

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
    with member this.Kind =
            match this with
            | OldPassword -> "Old"
            | NewPassword -> "New"
            | ConfirmPassword -> "Confirm"
         static member Default = NewPassword

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

[<Fable.Core.AttachMembers>]
type ClassWithAttachments(v, ?sign) =
    static let secretSauce = "wasabi"
    let mutable x = v
    member _.Times with get() = x and set(y) = x <- x + y
    member this.SaySomethingTo(name: string, ?format) =
        let sign = defaultArg sign "!"
        let format = defaultArg format ClassWithAttachments.GreetingFormat
        let format = format + ClassWithAttachments.GetGrettingEnding(this.Times, sign)
        String.Format(format, name)
    static member GreetingFormat = "Hello {0}"
    static member GetGrettingEnding(times, sign) = String.replicate times sign
    member _.WithSecretSauce(food) = $"{food} with lots of {secretSauce}"
    member this.Add
        with get(added : int) = x + added

type ClassWithAttachmentsChild() =
    inherit ClassWithAttachments(3, "?")
    member this.dileHola(name) = this.SaySomethingTo(name, "Hola, {0}")

[<Fable.Core.AttachMembers>]
type ClassWithAttachmentsChild2() =
    inherit ClassWithAttachments(3, "?")
    member this.dileHola(name) = this.SaySomethingTo(name, "Hola, {0}")

module TaggedUnion =
    type Base<'Kind> =
        abstract kind: 'Kind

    type Foo<'Kind> =
        inherit Base<'Kind>
        abstract foo: string

    type Bar<'Kind> =
        inherit Base<'Kind>
        abstract bar: int

    type Baz<'Kind> =
        inherit Base<'Kind>
        abstract baz: bool

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type StringTagged =
        | Foo of Foo<string>
        | Bar of Bar<string>
        | [<CompiledName("_baz")>] Baz of Baz<string>

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type NumberTagged =
        | [<CompiledValue(0)>] Foo of Foo<int>
        | [<CompiledValue(1.0)>] Bar of Bar<int>
        | [<CompiledValue(2)>] Baz of Baz<int>

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type BoolTagged =
        | [<CompiledValue(true)>] Foo of Foo<bool>
        | [<CompiledValue(false)>] Bar of Bar<bool>

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type MixedTagged =
        | [<CompiledValue(0)>] Foo of Foo<int>
        | Bar of Bar<string>
        | [<CompiledValue(false)>] Baz of Baz<bool>

    type Kind = Foo = 0 | Bar = 1 | Baz = 2

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type EnumTagged =
        | [<CompiledValue(Kind.Foo)>] Foo of Foo<Kind>
        | [<CompiledValue(Kind.Bar)>] Bar of Bar<Kind>
        | [<CompiledValue(Kind.Baz)>] Baz of Baz<Kind>

let tests =
  testList "JsInterop" [
    testCase "Class with attached members works" <| fun _ ->
        let x = ClassWithAttachments(2)
        equal 2 x.Times
        x.Times <- 3
        x.SaySomethingTo("Tanaka") |> equal "Hello Tanaka!!!!!"
        // See https://github.com/fable-compiler/Fable/issues/3494
        x.Add 2 |> equal 7

    testCase "Class with attached members can have static constructors" <| fun _ ->
        let x = ClassWithAttachments(2)
        x.WithSecretSauce("Nuggets")
        |> equal "Nuggets with lots of wasabi"

    testCase "Class with attached members can be inherited" <| fun _ ->
        let x = ClassWithAttachmentsChild()
        x.dileHola("Pepe") |> equal "Hola, Pepe???"

    testCase "Class with attached members can be inherited II" <| fun _ ->
        let x = ClassWithAttachmentsChild2()
        x.dileHola("Pepe") |> equal "Hola, Pepe???"

#if FABLE_COMPILER
    testCase "Can type test interfaces decorated with Global" <| fun () ->
        let ar = ResizeArray [| 1; 2; 3 |] |> box
        match ar with
        | :? JsArray as ar ->
            ar.length |> equal 3
            ar.push("4")
            ar.length |> equal 4
        | _ -> failwith "Not an array"

    testCase "IsInstanceOfType works with interfaces decorated with Global" <| fun () ->
        let ar = ResizeArray [| 1; 2; 3 |] |> box
        typeof<JsArray>.IsInstanceOfType(ar) |> equal true
        typeof<JsArray>.IsInstanceOfType("foo") |> equal false

    testCase "Decorators work" <| fun () ->
        myComplexAdder 3 4 |> equal 7
        myComplexAdder 6 7 |> equal 13
        logs |> Seq.toList |> equal [
          "myComplexAdder: System.Int32"
          "x: System.Int32"
          "y: System.Int32"
          "LOG1: [MATH (code 3)] called 1 time(s)!"
          "LOG2: called 1 time(s)!"
          "LOG1: [MATH (code 3)] called 2 time(s)!"
          "LOG2: called 2 time(s)!"
        ]

    // testCase "ReflectedDecorator works with generic parameters" <| fun _ ->
    //     GenericType.GenericMethod(4, "bar") |> equal "XYfoo"

    testCaseAsync "Quoted members in interfaces work" <| fun _ -> async {
        let pr = JS.Constructors.Promise.resolve(5)
        let pr = pr.``then``(fun x -> x + 2)
        let! x = Async.AwaitPromise pr
        x |> equal 7
    }

    testCase "List can be JSON serialized" <| fun () ->
        let x = [3; 2; 1]
        JS.JSON.stringify(x)
        |> equal "[3,2,1]"

    testCase "Set can be JSON serialized" <| fun () ->
        let x = set ["b"; "a"; "b"]
        JS.JSON.stringify(x)
        |> equal """["a","b"]"""

    testCase "Map can be JSON serialized" <| fun () ->
        let x = Map [ "b", 3; "a", 1; "b", 2]
        JS.JSON.stringify(x)
        |> equal """[["a",1],["b",2]]"""

    testCase "BigInt can be JSON serialized" <| fun () ->
        let x = 291865927421743871985719734712342981734987481I
        JS.JSON.stringify(x)
        |> equal "\"291865927421743871985719734712342981734987481\""

    testCase "Class with attached members can be sent to JS" <| fun _ ->
        let handleClass (cons: obj): string = importMember "./js/1foo.js"
        jsConstructor<ClassWithAttachments>
        |> handleClass
        |> equal "Hello Narumi!!!!!!"

    testCase "Can useq equals with Object.create(null)" <| fun () -> // See #2900
        let o: obj = emitJsExpr () "Object.create(null)"
        o = obj() |> equal false
        o = null |> equal false
        isNull o |> equal false
        jsTypeof o |> equal "object"

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

    testCase "Function returned by other functions kept curried" <| fun () -> // See #2840
        let o = createObj [
            "add1" ==> curry2(fun a b -> a + b * 2)
            "add2" ==> curryInline2(fun a b -> a + b - 1)
            "add3" ==> Curry(fun a -> Curry(fun b -> a + b))
        ]
        let f1 = o?add1(2)
        let f2 = o?add2(5)
        f1 3 |> equal 8
        f2 4 |> equal 8
        let f3 = o?add3(5)
        f3 4 |> equal 9

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
        let o = Fable.Tests.DllRef.Lib2.getArgCount
        o?foo() |> equal 0
        let f = box o?foo
        f$() |> equal 0
        (f :?> JsFunc).Invoke() |> unbox<int> |> equal 0

    testCase "Erase attribute works" <| fun () ->
        let convert = function
            | ErasedInt i -> string(i * 2)
            | ErasedString s -> "x" + s + "x"
        ErasedInt 4 |> convert |> equal "8"
        ErasedString "ab" |> convert |> equal "xabx"

    testCase "Erased types can have members" <| fun () ->
        let x = ErasedString "Patrick"
        x.SayHi() |> equal "Hi Patrick!"

    testCase "Erased unions with multiple fields work" <| fun _ ->
        let gimme (ErasedUnionWithMultipleFields(s, i)) =
            sprintf "Gimme %i %ss" i s
        ("apple", 5)
        |> ErasedUnionWithMultipleFields
        |> gimme
        |> equal "Gimme 5 apples"

    testCase "Erased unions can have cases representing literal strings" <| fun _ ->
        let getValue = function
            | MyErasedUnion2.Foo -> 5
            | MyErasedUnion2.Ohmy -> 0
            | MyErasedUnion2.Bar f -> int f
            | MyErasedUnion2.Baz xs -> Array.sum xs

        MyErasedUnion2.Bar 4.4 |> getValue |> equal 4
        MyErasedUnion2.Ohmy |> getValue |> equal 0
        MyErasedUnion2.Baz [|1;2;3|] |> getValue |> equal 6
        MyErasedUnion2.Foo |> getValue |> equal 5
        box MyErasedUnion2.Foo |> equal (box "foo")
        box MyErasedUnion2.Ohmy |> equal (box "ohmy")

    testCase "Erased unions can have case rules" <| fun _ ->
        let getValue = function
            | MyErasedUnion3.FooBar -> 5
            | MyErasedUnion3.OhMyDear -> 0
            | MyErasedUnion3.AnotherNumber i -> i

        MyErasedUnion3.AnotherNumber 3 |> getValue |> equal 3
        MyErasedUnion3.OhMyDear |> getValue |> equal 0
        MyErasedUnion3.FooBar |> getValue |> equal 5
        box MyErasedUnion3.OhMyDear |> equal (box "oh-my-dear")
        box MyErasedUnion3.FooBar |> equal (box "foo-bar")

    testCase "Emit attribute works" <| fun () ->
        let style = createEmpty<TextStyle>
        style.Bar |> equal "foo"
        style.Add(3,5) |> equal 8

    testCase "Emit uncurries lambdas passed as obj" <| fun () -> // #2323
        let helper = createEmpty<TextStyle>
        helper.Apply((fun x y -> x + y), 5, 4) :?> int |> equal 9

    testCase "emitJsExpr works" <| fun () ->
        let x = 4
        let y = 8
        let z1: int = emitJsExpr (x, y) "$0 * Math.pow(2, $1)"
        let z2: int = emitJsExpr (x, y) "$0 << $1"
        equal z1 z2
        equal 1024 z1

    testCase "emitJsExpr works inlined and with addition" <| fun () ->
        let x = sqrtJsExpr "3 + 1"
        equal 2. x

    testCase "emitJsStatement works" <| fun () ->
        let f x y: int =
            emitJsStatement (x, y) "return $0 << $1"
        f 4 8 |> equal 1024

    testCase "emitJsStatement can be used with interpolation" <| fun () ->
        let res1 = doSomethingInJs 8 3 4.5678
        let res2 = doSomethingInFSharp 8 3 4.5678
        equal res1 res2

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

    testCase "Emit works with ParamArray" <| fun () ->
        let style = createEmpty<TextStyle>
        style.Sum(1.5, 2, 3, 4) |> equal 10.5

    testCase "nameof works" <| fun () ->
        let record = { String = ""; Int = 0; InnerRecord = { Float = 5.0; Foo = { Bar = "z" } } }
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

    testCase "namesofLambda works" <| fun () ->
        namesofLambda(fun (x:Record) -> x.InnerRecord.Float) |> equal [|"InnerRecord"; "Float"|]
        namesofLambda(fun (x:Record) -> x.InnerRecord.Foo.Bar) |> equal [|"InnerRecord"; "Foo"; "Bar"|]
        namesofLambda(fun (x:Record) -> x.Int) |> equal [|"Int"|]

    testCase "nameofLambda works in inlined functions" <| fun () ->
        getNamesofLambda(fun (x:Record) -> x.InnerRecord.Foo.Bar) |> equal [|"InnerRecord"; "Foo"; "Bar"|]

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

    testCase "Inheriting from global classes defined in nested modules works" <| fun () -> // #2327
        GlobalClasses.MyHTMLElement.lunchTime |> equal "13:00"
        let x = MyCustomHTMLElement()
        x.SaySomethingNiceTo("Mary")
        |> equal "Repeat with me Mary: I can do it! I can do it! I can do it!"

    testCase "Local import with curried signatures works" <| fun () ->
        let add (x:int) (y:int): int = importMember "./js/1foo.js"
        3 |> add 2 |> equal 5
        let add2 (x:int) (y:int): int = import "add" "./js/1foo.js"
        3 |> add2 2 |> equal 5

    testCase "TypedArray element can be set and get using index" <| fun () ->
        let arr = JS.Constructors.Uint8Array.Create(5)
        arr.[0] <- 5uy
        equal 5uy arr.[0]

    testCase "jsOptions works" <| fun _ ->
        let opts = jsOptions<JsOptions>(fun o ->
            o.foo <- "bar"
            o.bar <- 5)
        opts.foo |> equal "bar"
        opts.bar |> equal 5

    testCase "Stringifying a JS object works" <| fun () ->
        let fooOptional = importMember "./js/1foo.js"
        string fooOptional |> equal "much foo, much awesome"

    testCase "Stringifying an F# type in JS works" <| fun () ->
        let addFooString (x: obj) = importMember "./js/1foo.js"
        { Float = 7.; Foo = { Bar = "oh" } } |> addFooString |> equal "35x foo"

    testCase "JsFunc.Invoke call with param array is spreaded in JS" <| fun () ->
      let fn : Fable.Core.JsInterop.JsFunc = !!(fun (arg:int) -> arg+1) // actual implementation is not important
      let arg = [|box 1|] // invoke wants an obj array
      fn.Invoke(arg)  // fable compiler will spread this argument
      |> unbox
      |> equal 2

    testCase "Case testing with TS tagged unions of string tags works" <| fun () ->
        let describe = function
            | TaggedUnion.StringTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.StringTagged.Bar x -> sprintf "bar: %d" x.bar
            | TaggedUnion.StringTagged.Baz x -> sprintf "baz: %b" x.baz
        TaggedUnion.StringTagged.Foo !!{| kind = "foo"; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.StringTagged.Bar !!{| kind = "bar"; bar = 42 |} |> describe |> equal "bar: 42"
        TaggedUnion.StringTagged.Baz !!{| kind = "_baz"; baz = false |} |> describe |> equal "baz: false"

    testCase "Case testing with TS tagged unions of number tags works" <| fun () ->
        let describe = function
            | TaggedUnion.NumberTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.NumberTagged.Bar x -> sprintf "bar: %d" x.bar
            | TaggedUnion.NumberTagged.Baz x -> sprintf "baz: %b" x.baz
        TaggedUnion.NumberTagged.Foo !!{| kind = 0; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.NumberTagged.Bar !!{| kind = 1.0; bar = 42 |} |> describe |> equal "bar: 42"
        TaggedUnion.NumberTagged.Baz !!{| kind = 2; baz = false |} |> describe |> equal "baz: false"

    testCase "Case testing with TS tagged unions of boolean tags works" <| fun () ->
        let describe = function
            | TaggedUnion.BoolTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.BoolTagged.Bar x -> sprintf "bar: %d" x.bar
        TaggedUnion.BoolTagged.Foo !!{| kind = true; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.BoolTagged.Bar !!{| kind = false; bar = 42 |} |> describe |> equal "bar: 42"

    testCase "Case testing with TS tagged unions of mixed type tags works" <| fun () ->
        let describe = function
            | TaggedUnion.MixedTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.MixedTagged.Bar x -> sprintf "bar: %d" x.bar
            | TaggedUnion.MixedTagged.Baz x -> sprintf "baz: %b" x.baz
        TaggedUnion.MixedTagged.Foo !!{| kind = 0; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.MixedTagged.Bar !!{| kind = "bar"; bar = 42 |} |> describe |> equal "bar: 42"
        TaggedUnion.MixedTagged.Baz !!{| kind = false; baz = false |} |> describe |> equal "baz: false"

    testCase "Case testing with TS tagged unions of enum tags works" <| fun () ->
        let describe = function
            | TaggedUnion.EnumTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.EnumTagged.Bar x -> sprintf "bar: %d" x.bar
            | TaggedUnion.EnumTagged.Baz x -> sprintf "baz: %b" x.baz
        TaggedUnion.EnumTagged.Foo !!{| kind = TaggedUnion.Kind.Foo; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.EnumTagged.Bar !!{| kind = TaggedUnion.Kind.Bar; bar = 42 |} |> describe |> equal "bar: 42"
        TaggedUnion.EnumTagged.Baz !!{| kind = TaggedUnion.Kind.Baz; baz = false |} |> describe |> equal "baz: false"
#endif

    testCase "Pattern matching with StringEnum works" <| fun () ->
        validatePassword NewPassword
        |> equal "np"

    testCase "StringEnums can have members" <| fun () ->
        let x = ConfirmPassword
        x.Kind |> equal "Confirm"

    testCase "StringEnums can have static members" <| fun () ->
        let x = Field.Default
        validatePassword x |> equal "np"
  ]
