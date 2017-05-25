[<Util.Testing.TestFixture>]
module Fable.Tests.RecordTypes
open Util.Testing
open Fable.Tests.Util

type RecursiveRecord =
    { things : RecursiveRecord list }

type Person =
    { name: string; mutable luckyNumber: int }
    member x.LuckyDay = x.luckyNumber % 30
    member x.SignDoc str = str + " by " + x.name

[<Test>]
let ``Recursive record does not cause issues``() =
    let r = { things = [ { things = [] } ] }
    equal r.things.Length 1

[<Test>]
let ``Record property access can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    equal "Alfonso" x.name
    equal 7 x.luckyNumber
    x.luckyNumber <- 14
    equal 14 x.luckyNumber

[<Test>]
let ``Record methods can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 54 }
    equal 24 x.LuckyDay
    x.SignDoc "Hello World!"
    |> equal "Hello World! by Alfonso"

[<Test>]
let ``Record expression constructors can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    let y = { x with luckyNumber = 14 }
    equal "Alfonso" y.name
    equal 14 y.luckyNumber

type JSKiller =
   { ``for`` : float; ``class`` : float }

type JSKiller2 =
   { ``s p a c e`` : float; ``s*y*m*b*o*l`` : float }

[<Test>]
let ``Records with key/reserved words are mapped correctly``() =
    let x = { ``for`` = 1.0; ``class`` = 2.0 }
    equal 2. x.``class``

[<Test>]
let ``Records with special characters are mapped correctly``() =
    let x = { ``s p a c e`` = 1.0; ``s*y*m*b*o*l`` = 2.0 }
    equal 1. x.``s p a c e``
    equal 2. x.``s*y*m*b*o*l``

type Child =
    { a: string; b: int }
    member x.Sum() = (int x.a) + x.b

type Parent =
    { children: Child[] }
    member x.Sum() = x.children |> Seq.sumBy (fun c -> c.Sum())

[<Test>]
let ``Records can be JSON serialized forth and back``() =
    let parent = { children=[|{a="3";b=5}; {b=7;a="1"} |] }
    let sum1 = parent.Sum()
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson parent
    let parent2 = Fable.Core.JsInterop.ofJson<Parent> json
    let sum2 = parent.Sum()
    equal true (box parent2 :? Parent) // Type is kept
    equal true (sum1 = sum2) // Prototype methods can be accessed
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo parent
    let parent2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Parent> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject parent
    let parent2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Parent> json
    #endif
    let sum2 = parent.Sum()
    equal true (box parent2 :? Parent) // Type is kept
    equal true (sum1 = sum2) // Prototype methods can be accessed

[<Test>]
let ``Records serialized with Json.NET can be deserialized``() =
    // let x = { a="Hi"; b=20 }
    // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
    let json = """{"$type":"Fable.Tests.RecordTypes+Child","a":"Hi","b":10}"""
    #if FABLE_COMPILER
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Child> json
    #else
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Child> json
    #endif
    x2.a |> equal "Hi"
    x2.b |> equal 10

#if FABLE_COMPILER
open Fable.Core
[<Test>]
let ``Trying to deserialize a JSON with unexpected $type info throws an exception``() =
    let success (f:unit->'T) =
        try f() |> ignore; true
        with _ -> false
    let child = {a="3";b=5}
    let json = JsInterop.toJsonWithTypeInfo child
    success (fun () -> JsInterop.ofJsonWithTypeInfo<Parent> json)
    |> equal false
    success (fun () -> JsInterop.ofJsonWithTypeInfo<Child> json)
    |> equal true
    success (fun () -> JsInterop.ofJsonWithTypeInfo<obj> json)
    |> equal true

[<Test>]
let ``POJOS can be inflated``() =
    let x = Fable.Import.JS.JSON.parse """{"a":"Hi","b":10}"""
    let x2: Child = JsInterop.inflate x
    x2.a |> equal "Hi"
    x2.b |> equal 10

#endif

type MutatingRecord =
    { uniqueA: int; uniqueB: int }

[<Test>]
let ``Mutating records work``() =
    let x = { uniqueA = 10; uniqueB = 20 }
    equal 10 x.uniqueA
    equal 20 x.uniqueB
    let uniqueB' = -x.uniqueB
    let x' = { x with uniqueB = uniqueB' }
    equal 10 x.uniqueA
    equal 10 x'.uniqueA
    equal -20 x'.uniqueB
    let x'' = { x' with uniqueA = -10 }
    equal -10 x''.uniqueA
    equal -20 x''.uniqueB
