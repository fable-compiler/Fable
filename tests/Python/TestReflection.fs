module Fable.Tests.Reflection

open Util.Testing
open FSharp.Data.UnitSystems.SI.UnitSymbols

#if !OPTIMIZE_FCS

type MyDelegate = delegate of int * float -> string
type MyGenDelegate<'a,'b> = delegate of 'b * string -> 'a

// I can declare a type with delegate fields (both C# and F# style). See #1698
type WithDelegates =
    { Del1: MyDelegate
      Del2: MyGenDelegate<int,string>
      Del3: System.Func<int, float, string>
      Del4: System.Func<float>
      Del5: System.Action
      Del6: System.Action<int>
      Del7: System.Action<int, string> }

type MyType =
    | Union1 of string

type MyType2 =
    | Union2 of string

type MyType3 = class end
type MyType4 = class end
type MyType5 = class end

type GenericRecord<'A,'B> = { a: 'A; b: 'B }

type MyEnum =
    | Foo = 1y
    | Bar = 5y
    | Baz = 8y

type MyInterface<'T> =
     abstract Value: 'T

type MyClass() =
    class end

type MyClass2() =
    class end

type MyClass3<'T>(v) =
    interface MyInterface<'T> with
        member _.Value = v

type MyClass<'T1, 'T2>(v1: 'T1, v2: 'T2) =
    inherit MyClass()
    member _.Value1 = v1
    member _.Value2 = v2

[<Fact>]
let ``test typedefof works`` () =
    let tdef1 = typedefof<int list>
    let tdef2 = typedefof<string list>
    equal tdef1 tdef2

[<Fact>]
let ``test IsGenericType works`` () =
    typeof<int list>.IsGenericType |> equal true
    typeof<MyType>.IsGenericType |> equal false
    let t1 = typeof<int list>
    let t2 = typeof<MyType>
    let t3 = typeof<string>
    t1.IsGenericType |> equal true
    t2.IsGenericType |> equal false
    t3.IsGenericType |> equal false

[<Fact>]
let ``test GetGenericTypeDefinition works`` () =
    let tdef1 = typedefof<int list>
    let tdef2 = typeof<int list>.GetGenericTypeDefinition()
    let t = typeof<int list>
    let tdef3 = t.GetGenericTypeDefinition()
    equal tdef1 tdef2
    equal tdef1 tdef3
    tdef1 = typeof<int list> |> equal false

[<Fact>]
let ``test Comparing generic types works`` () =
    let t1 = typeof<GenericRecord<string, MyType>>
    let t2 = typeof<GenericRecord<string, MyType>>
    let t3 = typeof<GenericRecord<string, int>>
    t1 = t2 |> equal true
    t1 = t3 |> equal false

let inline getName<'t> = function
    | "namespace" -> typedefof<'t>.Namespace
    | "name" -> typedefof<'t>.Name
    | _ -> typedefof<'t>.FullName

let inline getName2 (d:'t) = function
    | "namespace" -> typeof<'t>.Namespace
    | "name" -> typeof<'t>.Name
    | _ -> typeof<'t>.FullName

let getName3 (t:System.Type) = function
    | "namespace" -> t.Namespace
    | "name" -> t.Name
    | _ -> t.FullName

type Firm = { name: string }

let normalize (x: string) =
    #if FABLE_COMPILER
    x
    #else
    x.Replace("+",".")
    #endif
let inline fullname<'T> () = typeof<'T>.FullName |> normalize

[<Fact>]
let ``test Type Namespace`` () =
    let x = typeof<MyType>.Namespace
    #if FABLE_COMPILER
    equal "Fable.Tests.Reflection" x
    #else
    equal "Fable.Tests" x
    #endif

[<Fact>]
let ``test Type FullName`` () =
    let x = typeof<MyType>.FullName
    x |> normalize |> equal "Fable.Tests.Reflection.MyType"

[<Fact>]
let ``test Type Name`` () =
    let x = typeof<MyType>.Name
    equal "MyType" x


[<Fact>]
let ``test Get fullname of generic types with inline function`` () =
    fullname<MyType3>() |> equal "Fable.Tests.Reflection.MyType3"
    fullname<MyType4>() |> equal "Fable.Tests.Reflection.MyType4"


[<Fact>]
let ``test Type name is accessible`` () =
    let x = { name = "" }
    getName<Firm> "name" |> equal "Firm"
    getName2 x "name" |> equal "Firm"
    getName3 typedefof<Firm> "name" |> equal "Firm"
    // getName4 x "name" |> equal "Firm"

[<Fact>]
let ``test Type namespace is accessible`` () =
    let test (x: string) =
        x.StartsWith("Fable.Tests") |> equal true
    let x = { name = "" }
    getName<Firm> "namespace" |> test
    getName2 x "namespace" |> test
    getName3 typedefof<Firm> "namespace" |> test
    // getName4 x "namespace" |> test

[<Fact>]
let ``test Type full name is accessible`` () =
    let test (x: string) =
        x.Replace("+", ".") |> equal "Fable.Tests.Reflection.Firm"
    let x = { name = "" }
    getName<Firm> "fullname" |> test
    getName2 x "fullname" |> test
    getName3 typedefof<Firm> "fullname" |> test
    // getName4 x "fullname" |> test

// FSharpType and FSharpValue reflection tests
open FSharp.Reflection

exception MyException of String: string * Int: int

let flip f b a = f a b

type MyRecord = {
    String: string
    Int: int
}

type MyUnion =
    | StringCase of SomeString: string * string
    | IntCase of SomeInt: int

type RecordF = { F : int -> string }

type AsyncRecord = {
  asyncProp : Async<string>
}

type MyList<'T> =
| Nil
| Cons of 'T * MyList<'T>

let inline create<'T when 'T: (new: unit->'T)> () = new 'T()

type A() = member __.Value = 5

type B() = member __.Value = 10

type AnonRec1 = {| name: string; child: {| name: string |} |}
type AnonRec2 = {| numbers: int list |}

type RecordGetValueType = {
    Firstname : string
    Age : int
}

[<Fact>]
let ``test Reflection Array`` () =
    let arType = typeof<int[]>
    let liType = typeof<int list>
    equal true arType.IsArray
    equal false liType.IsArray
    let elType = arType.GetElementType()
    typeof<int> = elType |> equal true
    typeof<bool> = elType |> equal false
    liType.GetElementType() |> equal null

[<Fact>]
let ``test FSharp.Reflection Union`` () =
    let typ = typeof<MyUnion>
    let unionCase1 = StringCase("a", "b")
    let unionCase2 = IntCase 1
    let unionTypeFields = FSharpType.GetUnionCases typ
    unionTypeFields |> Array.map (fun x -> x.Name) |> equal [| "StringCase"; "IntCase" |]
    let unionCase1Info, unionCase1ValueFields = FSharpValue.GetUnionFields(unionCase1, typ)
    let unionCase2Info, unionCase2ValueFields = FSharpValue.GetUnionFields(unionCase2, typ)
    let unionCaseInfos = [| unionCase1Info; unionCase2Info |]
    let unionCaseValueFields = [| unionCase1ValueFields; unionCase2ValueFields |]

    let expectedUnionCase1Fields = 0, "StringCase", [| typeof<string>; typeof<string> |], [| "SomeString"; "Item2" |], [| box "a"; box "b" |]
    let expectedUnionCase2Fields = 1, "IntCase", [| typeof<int> |], [| "SomeInt" |], [| box 1 |]
    let expectedUnionFields = [| expectedUnionCase1Fields; expectedUnionCase2Fields |]

    let unionFields =
        Array.zip unionCaseInfos unionCaseValueFields
        |> Array.map (fun (info, values) ->
            let types =
                info.GetFields()
                |> Array.map (fun field -> field.PropertyType)
            let names =
                info.GetFields()
                |> Array.map (fun field -> field.Name)
            info.Tag, info.Name, types, names, values)

    let canMakeSameUnionCases =
        unbox<MyUnion> (FSharpValue.MakeUnion(unionCase1Info, unionCase1ValueFields)) = unionCase1
        && unbox<MyUnion> (FSharpValue.MakeUnion(unionCase2Info, unionCase2ValueFields)) = unionCase2

    FSharpType.IsUnion typ |> equal true
    unionFields |> equal expectedUnionFields
    canMakeSameUnionCases |> equal true

[<Fact>]
let ``test FSharp.Reflection Record`` () =
    let typ = typeof<MyRecord>
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
        unbox<MyRecord> (FSharpValue.MakeRecord(typ, recordValueFields)) = record

    let all = isRecord && matchRecordFields && matchIndividualRecordFields && canMakeSameRecord
    all |> equal true

#endif
