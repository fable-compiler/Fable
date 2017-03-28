[<Util.Testing.TestFixture>]
module Fable.Tests.Reflection

#if DOTNETCORE && !FABLE_COMPILER
open System.Reflection
module System =
    type System.Type with
        member this.IsGenericType = this.GetTypeInfo().IsGenericType
#endif

open System
open Util.Testing
open Fable.Tests.Util

type TestType =
    | Union1 of string

type TestType2 =
    | Union2 of string

type GenericRecord<'A,'B> = { a: 'A; b: 'B }

open Fable.Core
type PassingGenericsTest =
    [<PassGenerics>]
    static member Foo<'T>(x: int) = typeof<'T>
    [<PassGenerics>]
    static member Bar<'T,'U>() = typeof<'U>, typeof<'T>

[<Test>]
let ``PassGenericsAttribute works``() =
    let t = PassingGenericsTest.Foo<string>(5)
    let t1, t2 = PassingGenericsTest.Bar<TestType, bool>()
    #if FABLE_COMPILER
    box t |> equal (box "string")
    box t1 |> equal (box "boolean")
    #endif
    t |> equal typeof<string>
    t1 |> equal typeof<bool>
    t2 |> equal typeof<TestType>

type PassingGenericsTest2 =
    [<PassGenerics>]
    static member OnlyAccept<'T>(msg: obj) =
        let t = typeof<'T>
        t = typeof<obj> || msg.GetType() = t

[<Test>]
let ``Comparing types works with primitives``() =
    PassingGenericsTest2.OnlyAccept<int>(43) |> equal true
    PassingGenericsTest2.OnlyAccept<string>("hi") |> equal true
    PassingGenericsTest2.OnlyAccept<string>(43) |> equal false

[<Test>]
let ``Comparing types works with custom types``() =
    PassingGenericsTest2.OnlyAccept<TestType>(Union1 "bye") |> equal true
    PassingGenericsTest2.OnlyAccept<TestType>(Union2 "bye") |> equal false
    PassingGenericsTest2.OnlyAccept<obj>("I'll accept anything") |> equal true

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

let [<PassGenerics>] getName<'t> = function
    | "namespace" -> typedefof<'t>.Namespace
    | "name" -> typedefof<'t>.Name
    | _ -> typedefof<'t>.FullName

let [<PassGenerics>] getName2 (d:'t) = function
    | "namespace" -> typeof<'t>.Namespace
    | "name" -> typeof<'t>.Name
    | _ -> typeof<'t>.FullName

let getName3 (t:System.Type) = function
    | "namespace" -> t.Namespace
    | "name" -> t.Name
    | _ -> t.FullName

let getName4 (o:obj) = function
    | "namespace" -> o.GetType().Namespace
    | "name" -> o.GetType().Name
    | _ -> o.GetType().FullName

type Firm = { name: string }

[<Test>]
let ``Type name is accessible``() =
    let x = { name = "" }
    getName<Firm> "name" |> equal "Firm"
    getName2 x "name" |> equal "Firm"
    getName3 typedefof<Firm> "name" |> equal "Firm"
    getName4 x "name" |> equal "Firm"

[<Test>]
let ``Type namespace is accessible``() =
    let test (x: string) =
        x.StartsWith("Fable.Tests") |> equal true
    let x = { name = "" }
    getName<Firm> "namespace" |> test
    getName2 x "namespace" |> test
    getName3 typedefof<Firm> "namespace" |> test
    getName4 x "namespace" |> test

[<Test>]
let ``Type full name is accessible``() =
    let test (x: string) =
        x.Replace("+", ".") |> equal "Fable.Tests.Reflection.Firm"
    let x = { name = "" }
    getName<Firm> "fullname" |> test
    getName2 x "fullname" |> test
    getName3 typedefof<Firm> "fullname" |> test
    getName4 x "fullname" |> test

type MessageBus () =
  let _create topic value = ()
  [<PassGenerics>]
  member this.create (value:'t) = "topic1: " + (typeof<'t>.Name)
  [<PassGenerics>]
  member this.create (topic, value:'t) = topic + ": " + (typeof<'t>.Name)

[<Test>]
let ``Overloads with PassGenericsAttribute work``() =
    let x = { name = "" }
    let bus = MessageBus()
    bus.create x |> equal "topic1: Firm"
    bus.create ("global", x) |> equal "global: Firm"

// FSharpType and FSharpValue reflection tests

open FSharp.Reflection

exception TestException of String: string * Int: int

let flip f b a = f a b

type TestRecord = {
    String: string
    Int: int
}

type TestUnion =
    | StringCase of String: string * string
    | IntCase of Int: int

[<Test>]
let ``FSharp.Reflection: Exception`` () =
    let typ = typeof<TestException>
    let ex = TestException("a", 1)
    let exTypeFields = FSharpType.GetExceptionFields typ
    let exValueFields = FSharpValue.GetExceptionFields(ex)

    let expectedExFields =
        [|
            "String", box "a"
            "Int", box 1
        |]

    let exFields =
        exTypeFields
        |> Array.map (fun field -> field.Name)
        |> flip Array.zip exValueFields

    let isExceptionRepresentation = FSharpType.IsExceptionRepresentation typ
    let matchExFields = exFields = expectedExFields

    let all = isExceptionRepresentation && matchExFields
    all |> equal true

[<Test>]
let ``FSharp.Reflection: Record`` () =
    let typ = typeof<TestRecord>
    let record = { String = "a"; Int = 1 }
    let recordTypeFields = FSharpType.GetRecordFields typ
    let recordValueFields = FSharpValue.GetRecordFields record

    let expectedRecordFields =
        [|
            "String", box "a"
            "Int", box 1
        |]

    let recordFields =
        recordTypeFields
        |> Array.map (fun field -> field.Name)
        |> flip Array.zip recordValueFields

    let isRecord = FSharpType.IsRecord typ
    let matchRecordFields = recordFields = expectedRecordFields
    let matchIndividualRecordFields =
        Array.zip recordTypeFields recordValueFields
        |> Array.forall (fun (info, value) ->
            FSharpValue.GetRecordField(record, info) = value
        )
    let canMakeSameRecord =
        unbox<TestRecord> (FSharpValue.MakeRecord(typ, recordValueFields)) = record

    let all = isRecord && matchRecordFields && matchIndividualRecordFields && canMakeSameRecord
    all |> equal true

[<Test>]
let ``FSharp.Reflection: Tuple`` () =
    let typ = typeof<string * int>
    let tuple = "a", 1
    let tupleTypeFields = FSharpType.GetTupleElements typ
    let tupleValueFields = FSharpValue.GetTupleFields tuple

    let expectedTupleFields =
        [|
            typeof<string>, box "a"
            typeof<int>, box 1
        |]

    let tupleFields = Array.zip tupleTypeFields tupleValueFields

    let isTuple = FSharpType.IsTuple typ
    let matchTupleFields = tupleFields = expectedTupleFields
    let matchIndividualTupleFields =
        tupleValueFields
        |> Array.mapi (fun i value -> i, value)
        |> Array.forall (fun (i, value) ->
            FSharpValue.GetTupleField(tuple, i) = value
        )
    let canMakeSameTuple =
        unbox<string * int> (FSharpValue.MakeTuple(tupleValueFields, typ)) = tuple

    let all = isTuple && matchTupleFields && matchIndividualTupleFields && canMakeSameTuple
    all |> equal true

[<Test>]
let ``FSharp.Reflection: Union`` () =
    let typ = typeof<TestUnion>
    let unionCase1 = StringCase("a", "b")
    let unionCase2 = IntCase 1
    let unionTypeFields = FSharpType.GetUnionCases typ
    unionTypeFields |> Array.map (fun x -> x.Name) |> equal [| "StringCase"; "IntCase" |]
    let unionCase1Info, unionCase1ValueFields = FSharpValue.GetUnionFields(unionCase1, typ)
    let unionCase2Info, unionCase2ValueFields = FSharpValue.GetUnionFields(unionCase2, typ)
    let unionCaseInfos = [| unionCase1Info; unionCase2Info |]
    let unionCaseValueFields = [| unionCase1ValueFields; unionCase2ValueFields |]

    let expectedUnionCase1Fields = 0, "StringCase", [| typeof<string>; typeof<string> |], [| box "a"; box "b" |]
    let expectedUnionCase2Fields = 1, "IntCase", [| typeof<int> |], [| box 1 |]
    let expectedUnionFields = [| expectedUnionCase1Fields; expectedUnionCase2Fields |]

    let unionFields =
        Array.zip unionCaseInfos unionCaseValueFields
        |> Array.map (fun (info, values) ->
            let types =
                info.GetFields()
                |> Array.map (fun field -> field.PropertyType)
            info.Tag, info.Name, types, values)

    let canMakeSameUnionCases =
        unbox<TestUnion> (FSharpValue.MakeUnion(unionCase1Info, unionCase1ValueFields)) = unionCase1
        && unbox<TestUnion> (FSharpValue.MakeUnion(unionCase2Info, unionCase2ValueFields)) = unionCase2

    FSharpType.IsUnion typ |> equal true
    unionFields |> equal expectedUnionFields
    canMakeSameUnionCases |> equal true
