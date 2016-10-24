[<Util.Testing.TestFixture>]
module Fable.Tests.Reflection

open System
open Util.Testing
open Fable.Tests.Util

type TestType =
    | Union1 of string

type TestType2 =
    | Union2 of string

type GenericRecord<'A,'B> = { a: 'A; b: 'B }

#if FABLE_COMPILER
open Fable.Core
type GenericParamTest =
    static member Foo<'T>(x: int, [<GenericParam("T")>] ?t: Type) = t.Value
    static member Bar<'T,'U>([<GenericParam("U")>] ?t1: Type, [<GenericParam("T")>] ?t2: Type) = t1.Value, t2.Value

[<Test>]
let ``GenericParamAttribute works``() =
    let t = GenericParamTest.Foo<string>(5)
    let t1, t2 = GenericParamTest.Bar<TestType, bool>()
    box t |> equal (box "string")
    box t1 |> equal (box "boolean")
    box t2 |> equal (box typeof<TestType>)
#endif

type GenericParamTest2 =
    static member OnlyAccept<'T>(msg: obj, [<Fable.Core.GenericParam("T")>] ?t: Type) =
        #if FABLE_COMPILER
        let t = t.Value
        #else
        let t = typeof<'T>
        #endif
        t = typeof<obj> || msg.GetType() = t

[<Test>]
let ``Comparing types works with primitives``() =
    GenericParamTest2.OnlyAccept<int>(43) |> equal true
    GenericParamTest2.OnlyAccept<string>("hi") |> equal true
    GenericParamTest2.OnlyAccept<string>(43) |> equal false

[<Test>]
let ``Comparing types works with custom types``() =
    GenericParamTest2.OnlyAccept<TestType>(Union1 "bye") |> equal true
    GenericParamTest2.OnlyAccept<TestType>(Union2 "bye") |> equal false
    GenericParamTest2.OnlyAccept<obj>("I'll accept anything") |> equal true

[<Test>]
let ``typedefof works``() =
    let tdef1 = typedefof<int list>
    let tdef2 = typedefof<string list>
    equal tdef1 tdef2

[<Test>]
let ``IsGenericType works``() =
    typeof<int list>.IsGenericType |> equal true
    typeof<TestType>.IsGenericType |> equal false
    let t1 = typeof<int list>
    let t2 = typeof<TestType>
    let t3 = typeof<string>
    t1.IsGenericType |> equal true
    t2.IsGenericType |> equal false
    t3.IsGenericType |> equal false

[<Test>]
let ``GetGenericTypeDefinition works``() =
    let tdef1 = typedefof<int list>
    let tdef2 = typeof<int list>.GetGenericTypeDefinition()
    let t = typeof<int list>
    let tdef3 = t.GetGenericTypeDefinition()
    equal tdef1 tdef2
    equal tdef1 tdef3

[<Test>]
let ``Comparing generic types works``() =
    let t1 = typeof<GenericRecord<string, TestType>>
    let t2 = typeof<GenericRecord<string, TestType>>
    let t3 = typeof<GenericRecord<string, int>>
    t1 = t2 |> equal true
    t1 = t3 |> equal false
