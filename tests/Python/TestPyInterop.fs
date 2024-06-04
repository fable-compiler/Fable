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

[<Global("Array")>]
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

#endif
