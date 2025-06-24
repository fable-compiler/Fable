module Fable.Tests.PyInterop

open System
open Util.Testing

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.PyInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental

let logs = ResizeArray()

type LogAttribute(msg1: string, code: int) =
    inherit Py.DecoratorAttribute()
    override _.Decorate(fn) =
        let mutable count = 0
        Py.argsFunc(fun args ->
            count <- count + 1
            logs.Add $"LOG1: [%s{msg1} (code %i{code})] called %i{count} time(s)!"
            fn.Invoke(args))

type Log2Attribute() =
    inherit Py.DecoratorAttribute()
    override _.Decorate(fn) =
        let mutable count = 0
        Py.argsFunc(fun args ->
            count <- count + 1
            logs.Add $"LOG2: called %i{count} time(s)!"
            fn.Invoke(args))

type Log3Attribute() =
    inherit Py.ReflectedDecoratorAttribute()
    override _.Decorate(fn, info) =
        logs.Add $"{info.Name}: {info.ReturnType}"
        for p in info.GetParameters() do
            logs.Add $"{p.Name}: {p.ParameterType}"
        Py.argsFunc(fun args ->
            fn.Invoke(args))

[<Log("MATH", 3)>]
[<Log2>]
[<Log3>]
let myComplexAdder x y = x + y

type NameProp =
    { Name: string }

// type Props =
//     | Names of NameProp array
//     | [<Erase>] Custom of key:string * value:obj

[<Global("list")>]
type PyArray =
    [<Emit("$0.append($1)")>]
    abstract push: item: obj -> unit
    [<Emit("len($0)")>]
    abstract length: int

[<Fable.Core.AttachMembers>]
type ClassWithAttachments(v, sign) =
    static let secretSauce = "wasabi"
    let mutable x = v
    member _.Times with get() = x and set(y) = x <- x + y
    member this.SaySomethingTo(name: string, format) =
        let sign = defaultArg sign "!"
        let format = defaultArg format ClassWithAttachments.GreetingFormat
        let format = format + ClassWithAttachments.GetGrettingEnding(this.Times, sign)
        String.Format(format, name)
    static member GreetingFormat = "Hello {0}"
    static member GetGrettingEnding(times, sign) = String.replicate times sign
    member _.WithSecretSauce(food) = $"{food} with lots of {secretSauce}"

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

[<StringEnum(CaseRules.KebabCase); RequireQualifiedAccess>]
type MyCssOptions =
    | ContentBox
    | BorderBox

[<StringEnum(CaseRules.LowerAll); RequireQualifiedAccess>]
type LowerAllOptions =
    | ContentBox
    | BorderBox

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


type ClassWithAttachmentsChild() =
    inherit ClassWithAttachments(3, Some "?")
    member this.dileHola(name) = this.SaySomethingTo(name, Some "Hola, {0}")

[<Fable.Core.AttachMembers>]
type ClassWithAttachmentsChild2() =
    inherit ClassWithAttachments(3, Some "?")
    member this.dileHola(name) = this.SaySomethingTo(name, Some "Hola, {0}")

[<Fact>]
let ``test Class with attached members works`` () =
    let x = ClassWithAttachments(2, None) // FIXME: remove l arg
    equal 2 x.Times
    x.Times <- 3
    x.SaySomethingTo("Tanaka", None) |> equal "Hello Tanaka!!!!!"

[<Fact>]
let ``test Class with attached members can have static constructors`` () =
    let x = ClassWithAttachments(2, None)
    x.WithSecretSauce("Nuggets")
    |> equal "Nuggets with lots of wasabi"

[<Fact>]
let ``test Class with attached members can be inherited`` () =
    let x = ClassWithAttachmentsChild()
    x.dileHola("Pepe") |> equal "Hola, Pepe???"

[<Fact>]
let ``test Class with attached members can be inherited II`` () =
    let x = ClassWithAttachmentsChild2()
    x.dileHola("Pepe") |> equal "Hola, Pepe???"

[<Fact>]
let ``test Can type test interfaces decorated with Global`` () =
    let ar = ResizeArray [| 1; 2; 3 |] |> box
    match ar with
    | :? PyArray as ar ->
        ar.length |> equal 3
        ar.push("4")
        ar.length |> equal 4
    | _ -> failwith "Not an array"

// FIXME: ImportError: cannot import name 'keyValueList' from 'fable.map_util'
// [<Fact>]
// let ``Array inside keyValueList is preserved`` () =
//     let props = [ Names [| { Name = "name" } |] ]
//     let actual = [ Names [| { Name = "name" } |] ] |> keyValueList CaseRules.LowerFirst |> JS.JSON.stringify
//     let expected = props |> keyValueList CaseRules.LowerFirst |> JS.JSON.stringify
//     actual |> equal expected

[<Fact>]
let ``test Decorators work`` () =
    myComplexAdder 3 4 |> equal 7
    myComplexAdder 6 7 |> equal 13
    logs |> Seq.toList |> equal [
        "my_complex_adder: System.Int32"
        "x: System.Int32"
        "y: System.Int32"
        "LOG1: [MATH (code 3)] called 1 time(s)!"
        "LOG2: called 1 time(s)!"
        "LOG1: [MATH (code 3)] called 2 time(s)!"
        "LOG2: called 2 time(s)!"
    ]

[<Fact>]
let ``test Erased types can have members`` () =
    let x = ErasedString "Patrick"
    x.SayHi() |> equal "Hi Patrick!"

[<Fact>]
let ``test Erased unions with multiple fields work`` () =
    let gimme (ErasedUnionWithMultipleFields(s, i)) =
        sprintf "Gimme %i %ss" i s
    ("apple", 5)
    |> ErasedUnionWithMultipleFields
    |> gimme
    |> equal "Gimme 5 apples"

[<Fact>]
let ``test Erased unions can have cases representing literal strings`` () =
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

[<Fact>]
let ``test Erased unions can have case rules`` () =
    let getValue = function
        | MyErasedUnion3.FooBar -> 5
        | MyErasedUnion3.OhMyDear -> 0
        | MyErasedUnion3.AnotherNumber i -> i

    MyErasedUnion3.AnotherNumber 3 |> getValue |> equal 3
    MyErasedUnion3.OhMyDear |> getValue |> equal 0
    MyErasedUnion3.FooBar |> getValue |> equal 5
    box MyErasedUnion3.OhMyDear |> equal (box "oh-my-dear")
    box MyErasedUnion3.FooBar |> equal (box "foo-bar")

[<Fact>]
let ``test emitPyExpr works with parameters`` () =
    let two : int =
        emitPyExpr (1, 1) "$0 + $1"

    two |> equal 2

[<Fact>]
let ``test emitPyExpr works without parameters`` () =
    let hello : string =
        emitPyExpr () "\"Hello\""

    hello |> equal "Hello"

// This function needs to be at the root level to avoid being mangled
let factorial (count : int) : int =
    emitPyStatement
        count
        """if $0 < 2:
        return 1
    else:
        return $0 * factorial($0-1)
    """

[<Fact>]
let ``test emitPyStatement works with parameters`` () =
    factorial 5 |> equal 120


type NativeCode =
    abstract add5: int -> int

[<Fact>]
let ``test importAll`` () =
    let nativeCode: NativeCode = importAll "./native_code.py"
    3 |> nativeCode.add5 |> equal 8

let add5 (x: int): int = importMember "./native_code.py"

[<Fact>]
let ``test importMember`` () =
    add5 -1 |> equal 4

    // Cannot use the same name as Fable will mangle the identifier
    let add7: int -> int = importMember "./native_code.py"
    add7 12 |> equal 19

    let add5': int -> int = import "add5" "./native_code.py"
    add5' 12 |> equal 17

    let multiply3 (x: int): int = importMember "./more_native_code.py"
    multiply3 9 |> equal 27

[<ImportAll("./native_code.py")>]
let nativeCode: NativeCode = nativeOnly

[<Fact>]
let ``test ImportAll works with relative paths`` () = // See #3481
    3 |> nativeCode.add5 |> equal 8

[<Fact>]
let ``test StringEnum attribute works`` () =
    Vertical |> unbox |> equal "vertical"
    Horizontal |> unbox |> equal "Horizontal"
    Vertical |> string |> equal "vertical"
    Horizontal |> string |> equal "Horizontal"

[<Fact>]
let ``test StringEnum works with CaseRules.SnakeCase`` () =
    UserInfo.UserLoginCount |> unbox |> equal "user_login_count"

[<Fact>]
let ``test StringEnum works with CaseRules.SnakeCaseAllCaps`` () =
    UserInfo2.UserLoginCount |> unbox |> equal "USER_LOGIN_COUNT"

[<Fact>]
let ``test StringEnum works with CaseRules.KebabCase`` () =
    MyCssOptions.BorderBox |> unbox |> equal "border-box"
    MyCssOptions.ContentBox |> unbox |> equal "content-box"

[<Fact>]
let ``test StringEnum works with CaseRules.LowerAll`` () =
    let x = LowerAllOptions.ContentBox
    x |> unbox |> equal "contentbox"

[<Fact>]
let ``test Pattern matching with StringEnum works`` () =
    validatePassword NewPassword
    |> equal "np"

[<Fact>]
let ``test StringEnums can have members`` () =
    let x = ConfirmPassword
    x.Kind |> equal "Confirm"

[<Fact>]
let ``test StringEnums can have static members`` () =
    let x = Field.Default
    validatePassword x |> equal "np"

#endif
