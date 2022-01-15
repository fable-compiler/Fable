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

type MyRecord = {
    String: string
    Int: int
}

let inline getType<'a>() = typeof<'a>


[<Fact>]
let ``test obj.ReferenceEquals not reflexive`` () =
    let inline (==) (a: 'a) (b: 'b) = obj.ReferenceEquals(a, b)
    let t = getType<MyRecord>()
    typeof<MyRecord> == t |> equal true
    t == typeof<MyRecord> |> equal true

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

[<Fact>]
let ``test PropertyInfo.GetValue works`` () =
    let value: obj = { Firstname = "Maxime"; Age = 12 } :> obj

    let theType: System.Type = typeof<RecordGetValueType>

    // now we want to print out the fields
    let fieldNameToValue: Map<string, obj> =
        match theType with
        | t when FSharpType.IsRecord t ->
            FSharpType.GetRecordFields(t)
            |> Seq.fold
                (fun acc field ->
                    let fieldValue = field.GetValue value
                    acc.Add (field.Name, fieldValue)
                )
                Map.empty
        | _ -> Map.empty

    let expected = "map [(Age, 12); (Firstname, Maxime)]"

    equal expected (sprintf "%O" fieldNameToValue)


[<Fact>]
let ``test Comparing anonymous record types works`` () =
  let x = {| numbers = [3; 4] |}
  typeof<AnonRec1> = typeof<AnonRec2> |> equal false
  typeof<AnonRec1> = typeof<AnonRec1> |> equal true
  typeof<AnonRec2> = x.GetType() |> equal true
  let generic = typeof<Result<AnonRec2, string>>
  generic = typeof<Result<AnonRec1, string>> |> equal false
  generic = typeof<Result<{| numbers: int list |}, string>> |> equal true


[<Fact>]
let ``test FSharp.Reflection: Anonymous Record`` () =
    let typ = typeof<{| String: string; Int: int |}>
    let record = {| String = "a"; Int = 1 |}
    let recordTypeFields = FSharpType.GetRecordFields typ
    let recordValueFields = FSharpValue.GetRecordFields record

    let expectedRecordFields = // Alphabetical order
        [| "Int", box 1
           "String", box "a" |]

    let recordFields =
        recordTypeFields
        |> Array.map (fun field -> field.Name)
        |> flip Array.zip recordValueFields

    FSharpType.IsRecord typ |> equal true
    recordFields |> equal expectedRecordFields

    Array.zip recordTypeFields recordValueFields
    |> Array.forall (fun (info, value) ->
        FSharpValue.GetRecordField(record, info) = value)
    |> equal true

    FSharpValue.MakeRecord(typ, recordValueFields)
    |> unbox<{| String: string; Int: int |}>
    |> equal record

[<Fact>]
let ``test FSharp.Reflection Functions`` () =
    let recordType = typeof<RecordF>
    let fields = FSharpType.GetRecordFields recordType
    let funcProperty = Array.head fields
    let funcType = funcProperty.PropertyType
    let domain, range = FSharpType.GetFunctionElements funcType
    equal domain typeof<int>
    equal range typeof<string>
    equal true (FSharpType.IsFunction funcType)

[<Fact>]
let ``test FSharp.Reflection: Tuple`` () =
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

[<Fact>]
let ``test FSharp.Reflection: Array of tuples is not classified as a tuple`` () =
    let typ = typeof<(string * int * int)[]>
    equal true typ.IsArray
    FSharpType.IsTuple typ |> equal false

[<Fact>]
let ``test FSharp.Reflection: MakeTupleType`` () =
    let t = FSharpType.MakeTupleType [|typeof<float>; typeof<string>; typeof<int[]>|]
    FSharpValue.MakeTuple([|5.; "foo"; [|2;3|]|], t)
    |> unbox<float * string * int[]>
    |> equal (5., "foo", [|2;3|])

    let real = typeof<float * string * int[]>
    let generated = FSharpType.MakeTupleType [|typeof<float>; typeof<string>; typeof<int[]>|]
    equal real generated

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
let ``test FSharp.Reflection: Result`` () =
    let typ = typeof<Result<int,string>>
    let ucis = FSharpType.GetUnionCases typ
    FSharpValue.MakeUnion(ucis.[0], [|box 5|]) |> equal (box (Result<_,string>.Ok 5))
    FSharpValue.MakeUnion(ucis.[1], [|box "foo"|]) |> equal (box (Result<int,_>.Error "foo"))

[<Fact>]
let ``test FSharp.Reflection: Choice`` () =
    let typ = typeof<Choice<int,string>>
    let ucis = FSharpType.GetUnionCases typ
    FSharpValue.MakeUnion(ucis.[0], [|box 5|]) |> equal (box (Choice<_,string>.Choice1Of2 5))
    FSharpValue.MakeUnion(ucis.[1], [|box "foo"|]) |> equal (box (Choice<int,_>.Choice2Of2 "foo"))

    let typ = typeof<Choice<float,string list,float>>
    let ucis = FSharpType.GetUnionCases typ
    FSharpValue.MakeUnion(ucis.[0], [|box -0.3|]) |> equal (box (Choice<_,string list,float>.Choice1Of3 -0.3))
    FSharpValue.MakeUnion(ucis.[1], [|box ["foo";"bar"]|]) |> equal (box (Choice<float,_,float>.Choice2Of3 ["foo";"bar"]))
    FSharpValue.MakeUnion(ucis.[2], [|box 3.5|]) |> equal (box (Choice<float,string list,_>.Choice3Of3 3.5))
    FSharpValue.MakeUnion(ucis.[2], [|box 3.5|]) |> (=) (box (Choice<float,string list,_>.Choice1Of3 3.5)) |> equal false

[<Fact>]
let ``test Type.GenericTypeArguments works`` () =
    let recordType = typeof<AsyncRecord>
    let asyncProp = FSharpType.GetRecordFields recordType |> Array.head
    asyncProp.PropertyType.GenericTypeArguments |> Array.head |> equal typeof<string>

[<Fact>]
let ``test Recursive types work`` () =
    let cons =
        FSharpType.GetUnionCases(typeof<MyList<int>>)
        |> Array.find (fun x -> x.Name = "Cons")
    let fieldTypes = cons.GetFields()
    fieldTypes.[0].PropertyType.FullName |> equal typeof<int>.FullName
    fieldTypes.[1].PropertyType.GetGenericTypeDefinition().FullName |> equal typedefof<MyList<obj>>.FullName

[<Fact>]
let ``test Calling constructor of generic type in inline functions works`` () =
    let a = create<A>()
    let b = create<B>()
    a.Value |> equal 5
    b.Value |> equal 10

// See https://github.com/Microsoft/visualfsharp/issues/5992
[<Fact>]
let ``test Generic numbers type info doesn't get into runtime`` () =
    let value = 0.7833263478179128134089M
    value.GetType().FullName |> equal "System.Decimal"

  // See https://github.com/thoth-org/Thoth.Json/issues/74
[<Fact>]
let ``test Reflection info of int64/decimal with units of measure works`` () =
    typeof< int64 > = typeof< int64<m> > |> equal true
    typeof< decimal > = typeof< decimal<m> > |> equal true


[<Fact>]
let ``test Reflection works with enums`` () =
      typeof<MyEnum>.IsEnum |> equal true
      typeof<int>.IsEnum |> equal false
      let t = typeof<MyEnum>
      t.IsEnum |> equal true
      t.GetEnumUnderlyingType() |> equal typeof<sbyte>
      System.Enum.GetUnderlyingType(t) |> equal typeof<sbyte>

[<Fact>]
let ``test Can create generic classes at runtime`` () =
    let t = typedefof<MyClass<obj, obj>>
    let t = t.MakeGenericType(typeof<int>, typeof<string>)
    let x = System.Activator.CreateInstance(t, 123, "abc")
    x :? MyClass |> equal true
    x :? MyClass2 |> equal false
    let x = x :?> MyClass<int, string>
    x.Value1 |> equal 123
    x.Value2 |> equal "abc"

#endif
