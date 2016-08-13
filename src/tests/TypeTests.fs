[<NUnit.Framework.TestFixture>]
module Fable.Tests.TypeTests

open System
open NUnit.Framework
open Fable.Tests.Util

type ITest = interface end
type ITest2 = interface end

type TestType =
    | Union1 of string
    interface ITest

type TestType2 =
    | Union2 of string
    interface ITest
    
type TestType3() =
    member __.Value = "Hi"

type TestType4() =
    member __.Value = "Bye"

type TestType5(greeting: string) =
    member __.Value = greeting
    member __.Overload(x) = x + x
    member __.Overload(x, y) = x + y
    
let normalize (x: string) =
    #if MOCHA
    x
    #else
    x.Replace("+",".")
    #endif

[<Test>]
let ``Type Namespace``() =
    let x = typeof<TestType>.Namespace
    #if MOCHA
    equal "Fable.Tests.TypeTests" x
    #else
    equal "Fable.Tests" x
    #endif    

[<Test>]
let ``Type FullName``() =
    let x = typeof<TestType>.FullName
    x |> normalize |> equal "Fable.Tests.TypeTests.TestType"

[<Test>]
let ``Type Name``() =
    let x = typeof<TestType>.Name
    equal "TestType" x

[<Test>]
let ``Type testing``() =
    let x = Union1 "test" :> obj
    let y = Union2 "test" :> obj
    x :? TestType |> equal true
    x :? TestType2 |> equal false
    y :? TestType |> equal false
    y :? TestType2 |> equal true

[<Test>]
let ``Interface testing``() =
    let x = Union1 "test" :> obj
    let y = Union2 "test" :> obj
    x :? ITest |> equal true
    x :? ITest2 |> equal false
    y :? ITest |> equal true
    y :? ITest2 |> equal false
    
[<Test>]
let ``Type testing in pattern matching``() =
    let x = Union1 "test" :> obj
    match x with
    | :? TestType as x -> let (Union1 str) = x in str
    | _ -> "FAIL"
    |> equal "test"
    match x with
    | :? TestType2 as x -> let (Union2 str) = x in str
    | _ -> "FAIL"
    |> equal "FAIL"

[<Test>]
let ``Interface testing in pattern matching``() =
    let x = Union2 "test" :> obj
    match x with | :? ITest -> true | _ -> false
    |> equal true
    match x with | :? ITest2 -> true | _ -> false
    |> equal false

let inline fullname<'T> () = typeof<'T>.FullName |> normalize

[<Test>]
let ``Get fullname of generic types with inline function``() =
    fullname<TestType3>() |> equal "Fable.Tests.TypeTests.TestType3"
    fullname<TestType4>() |> equal "Fable.Tests.TypeTests.TestType4"

let inline create<'T when 'T:(new : unit -> 'T)> () = new 'T()
    
[<Test>]
let ``Create new generic objects with inline function``() =
    create<TestType3>().Value |> equal "Hi"
    create<TestType4>().Value |> equal "Bye"
    // create<TestType5>() // Doesn't compile    

let inline create2<'T> (args: obj[]) =
    System.Activator.CreateInstance(typeof<'T>, args) :?> 'T
    
[<Test>]
let ``Create new generic objects with System.Activator``() =
    (create2<TestType3> [||]).Value |> equal "Hi"
    (create2<TestType4> [||]).Value |> equal "Bye"
    (create2<TestType5> [|"Yo"|]).Value |> equal "Yo"
        
type RenderState = 
    { Now : int
      Players : Map<int, string>
      Map : string }

[<Test>]
let ``Property names don't clash with built-in JS objects``() = // See #168
    let gameState = {
        Now = 1
        Map = "dungeon"
        Players = Map.empty 
    } 
    gameState.Players.ContainsKey(1) |> equal false

[<Test>]
let ``Overloads work``() =
    let t = TestType5("")
    t.Overload(2) |> equal 4
    t.Overload(2, 3) |> equal 5
    
type T4 = TestType4

[<Test>]
let ``Type abbreviation works``() =
    let t = T4()
    t.Value |> equal "Bye"

type TestType6(x: int) =
    let mutable i = x
    member val Value1 = i with get, set
    member __.Value2 = i + i
    member __.Value3 with get() = i * i and set(v) = i <- v

[<Test>]
let ``Getter and Setter work``() =
    let t = TestType6(5)
    t.Value1 |> equal 5
    t.Value2 |> equal 10
    t.Value3 |> equal 25
    t.Value3 <- 10
    t.Value1 |> equal 5
    t.Value2 |> equal 20
    t.Value3 |> equal 100
    t.Value1 <- 20
    t.Value1 |> equal 20
    t.Value2 |> equal 20
    t.Value3 |> equal 100

type TestType7(a1, a2, a3) =
    let arr = [|a1; a2; a3|]
    member __.Value with get(i) = arr.[i] and set(i) (v) = arr.[i] <- v 

[<Test>]
let ``Getter and Setter with indexer work``() =
    let t = TestType7(1, 2, 3)
    t.Value(1) |> equal 2
    t.Value(2) |> equal 3
    t.Value(1) <- 5
    t.Value(1) |> equal 5
    t.Value(2) |> equal 3

type A  = { thing: int } with
    member x.show() = string x.thing
    static member show (x: A) = "Static: " + (string x.thing)

type B  = { label: string } with
    member x.show() = x.label
    static member show (x: B) = "Static: " + x.label

let inline show< ^T when ^T : (member show : unit -> string)> (x:^T) : string =
   (^T : (member show : unit -> string) (x))

let inline showStatic< ^T when ^T : (static member show : ^T -> string)> (x:^T) : string =
   (^T : (static member show : ^T -> string) (x))

[<Test>]
let ``Statically resolved instance calls work``() =
    let a = { thing = 5 }
    let b = { label = "five" }
    show a |> equal "5"
    show b |> equal "five"

[<Test>]
let ``Statically resolved static calls work``() =
    let a = { thing = 5 }
    let b = { label = "five" }
    showStatic a |> equal "Static: 5"
    showStatic b |> equal "Static: five"

[<Test>]
let ``Guid.NewGuid works``() =
    let g1 = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g1 = g2 |> equal false
    let s1 = string g1
    equal 36 s1.Length
    Text.RegularExpressions.Regex.IsMatch(
        s1, "^[a-f0-9]{8}(?:-[a-f0-9]{4}){3}-[a-f0-9]{12}$")
    |> equal true
    let g3 = Guid.Parse s1
    g1 = g3 |> equal true

[<Test>]
let ``Guid.Empty works``() =
    let g1 = Guid.Empty
    string g1 |> equal "00000000-0000-0000-0000-000000000000"

[<Test>]
let ``lazy works``() =
    let mutable snitch = 0 
    let lazyVal =
        lazy
            snitch <- snitch + 1
            5
    equal 0 snitch
    equal 5 lazyVal.Value
    equal 1 snitch
    lazyVal.Force() |> equal 5
    equal 1 snitch

[<Test>]
let ``Lazy.CreateFromValue works``() =
    let mutable snitch = 0 
    let lazyVal =
        Lazy<_>.CreateFromValue(
            snitch <- snitch + 1
            5)
    equal 1 snitch
    equal 5 lazyVal.Value
    equal 1 snitch

[<Test>]
let ``lazy.IsValueCreated works``() =
    let mutable snitch = 0 
    let lazyVal =
        Lazy<_>.Create(fun () ->
            snitch <- snitch + 1
            5)
    equal 0 snitch
    equal false lazyVal.IsValueCreated
    equal 5 lazyVal.Value
    equal true lazyVal.IsValueCreated
    lazyVal.Force() |> equal 5
    equal true lazyVal.IsValueCreated

[<AllowNullLiteral>]
type Serializable(?i: int) =
    let mutable deserialized = false
    let mutable publicValue = 1
    let mutable privateValue = defaultArg i 0
    member x.PublicValue
        with get() = publicValue
        and set(i) = deserialized <- true; publicValue <- i
    override x.ToString() =
        sprintf "Public: %i - Private: %i - Deserialized: %b"
                publicValue privateValue deserialized

[<Test>]
let ``Classes can be JSON serialized forth and back``() =
    let x = Serializable(5)
    #if MOCHA
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<Serializable> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Serializable> json
    #endif
    string x |> equal "Public: 1 - Private: 5 - Deserialized: false"
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"

[<Test>]
let ``Null values can be JSON serialized forth and back``() =
    let x: Serializable = null
    #if MOCHA
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<Serializable> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Serializable> json
    #endif
    equal x2 null

[<Test>]
let ``Classes serialized with Json.NET can be deserialized``() =
    // let x = Serializable(5)
    // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
    let json = """{"$type":"Fable.Tests.TypeTests+Serializable","PublicValue":1}"""
    #if MOCHA
    let x2 = Fable.Core.JsInterop.ofJson<Serializable> json
    #else
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Serializable> json
    #endif
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"

type SecondaryCons(x: int) =
    new () = SecondaryCons(5)
    member __.Value = x

[<Test>]
let ``Secondary constructors work``() =
    let s1 = SecondaryCons(3)
    let s2 = SecondaryCons()
    equal 3 s1.Value
    equal 5 s2.Value

type MultipleCons(x: int, y: int) =
    new () = MultipleCons(2,3)
    new (x:int) = MultipleCons(x,4)
    member __.Value = x + y

[<Test>]
let ``Multiple constructors work``() =
    let m1 = MultipleCons()
    let m2 = MultipleCons(5)
    let m3 = MultipleCons(7,7)
    equal 5 m1.Value
    equal 9 m2.Value
    equal 14 m3.Value
