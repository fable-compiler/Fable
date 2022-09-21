module Fable.Tests.PyInterop

open System
open Util.Testing

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.PyInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental

type NameProp =
    { Name: string }

type Props =
    | Names of NameProp array
    | [<Erase>] Custom of key:string * value:obj

[<Global("Array")>]
type PyArray =
    abstract push: item: obj -> unit
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

#endif