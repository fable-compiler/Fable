module Fable.Tests.ReflectionTests

open Util.Testing
open Xunit

// === Existing typeof tests ===

[<Fact>]
let ``test typeof int FullName`` () =
    typeof<int>.FullName |> equal "System.Int32"

[<Fact>]
let ``test typeof string FullName`` () =
    typeof<string>.FullName |> equal "System.String"

[<Fact>]
let ``test typeof float FullName`` () =
    typeof<float>.FullName |> equal "System.Double"

[<Fact>]
let ``test typeof bool FullName`` () =
    typeof<bool>.FullName |> equal "System.Boolean"

[<Fact>]
let ``test typeof int Namespace`` () =
    typeof<int>.Namespace |> equal "System"

[<Fact>]
let ``test typeof string Namespace`` () =
    typeof<string>.Namespace |> equal "System"

[<Fact>]
let ``test typeof int IsGenericType is false`` () =
    typeof<int>.IsGenericType |> equal false

[<Fact>]
let ``test typeof int list IsGenericType is true`` () =
    typeof<int list>.IsGenericType |> equal true

[<Fact>]
let ``test typeof int list FullName starts with`` () =
    typeof<int list>.FullName.StartsWith("Microsoft.FSharp.Collections.FSharpList`1")
    |> equal true

[<Fact>]
let ``test typeof int IsArray is false`` () =
    typeof<int>.IsArray |> equal false

[<Fact>]
let ``test typeof int array IsArray is true`` () =
    typeof<int array>.IsArray |> equal true

// === New reflection tests ===

type MyType =
    | Union1 of string

type GenericRecord<'A,'B> = { a: 'A; b: 'B }

type MyInterface<'T> =
    abstract Value: 'T

type MyClass() =
    class end

type MyClass2() =
    class end

type MyClass3<'T>(v) =
    interface MyInterface<'T> with
        member _.Value = v

[<Fact>]
let ``test typedefof works`` () =
    let tdef1 = typedefof<int list>
    let tdef2 = typedefof<string list>
    equal tdef1 tdef2

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

[<Fact>]
let ``test Type Name`` () =
    let x = typeof<MyType>.Name
    equal "MyType" x

[<Fact>]
let ``test Type FullName`` () =
    let x = typeof<MyType>.FullName
    x.Replace("+", ".") |> equal "Fable.Tests.ReflectionTests.MyType"

[<Fact>]
let ``test Type Namespace`` () =
    let x = typeof<MyType>.Namespace
    #if FABLE_COMPILER
    equal "Fable.Tests.ReflectionTests" x
    #else
    equal "Fable.Tests" x
    #endif

[<Fact>]
let ``test Reflection Array`` () =
    let arType = typeof<int[]>
    let liType = typeof<int list>
    equal true arType.IsArray
    equal false liType.IsArray
    let elType = arType.GetElementType()
    typeof<int> = elType |> equal true
    typeof<bool> = elType |> equal false

// FSharpType and FSharpValue reflection tests
open FSharp.Reflection

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

let flip f b a = f a b

[<Fact>]
let ``test FSharp.Reflection: IsTuple`` () =
    let typ = typeof<string * int>
    FSharpType.IsTuple typ |> equal true
    FSharpType.IsTuple typeof<int> |> equal false
    FSharpType.IsTuple typeof<int list> |> equal false

[<Fact>]
let ``test FSharp.Reflection: Array of tuples is not classified as a tuple`` () =
    let typ = typeof<(string * int * int)[]>
    equal true typ.IsArray
    FSharpType.IsTuple typ |> equal false

[<Fact>]
let ``test FSharp.Reflection: IsRecord`` () =
    FSharpType.IsRecord typeof<MyRecord> |> equal true
    FSharpType.IsRecord typeof<int> |> equal false

[<Fact>]
let ``test FSharp.Reflection: IsUnion`` () =
    FSharpType.IsUnion typeof<MyUnion> |> equal true
    FSharpType.IsUnion typeof<int> |> equal false

[<Fact>]
let ``test FSharp.Reflection: IsFunction`` () =
    let funcType = typeof<int -> string>
    FSharpType.IsFunction funcType |> equal true
    FSharpType.IsFunction typeof<int> |> equal false

[<Fact>]
let ``test FSharp.Reflection: GetTupleElements`` () =
    let typ = typeof<string * int>
    let elements = FSharpType.GetTupleElements typ
    elements.Length |> equal 2
    elements.[0] |> equal typeof<string>
    elements.[1] |> equal typeof<int>

[<Fact>]
let ``test FSharp.Reflection: GetFunctionElements`` () =
    let funcType = typeof<int -> string>
    let domain, range = FSharpType.GetFunctionElements funcType
    equal domain typeof<int>
    equal range typeof<string>

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
let ``test FSharp.Reflection: MakeTupleType`` () =
    let t = FSharpType.MakeTupleType [|typeof<float>; typeof<string>; typeof<int[]>|]
    FSharpValue.MakeTuple([|5.; "foo"; [|2;3|]|], t)
    |> unbox<float * string * int[]>
    |> equal (5., "foo", [|2;3|])

    let real = typeof<float * string * int[]>
    let generated = FSharpType.MakeTupleType [|typeof<float>; typeof<string>; typeof<int[]>|]
    equal real generated

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
let ``test FSharp.Reflection: GetRecordFields returns field names`` () =
    let typ = typeof<MyRecord>
    let fields = FSharpType.GetRecordFields typ
    fields.Length |> equal 2
    #if FABLE_COMPILER
    fields.[0].Name |> equal "string"
    fields.[1].Name |> equal "int"
    #else
    fields.[0].Name |> equal "String"
    fields.[1].Name |> equal "Int"
    #endif

[<Fact>]
let ``test FSharp.Reflection Record`` () =
    let typ = typeof<MyRecord>
    let record = { String = "a"; Int = 1 }
    let recordTypeFields = FSharpType.GetRecordFields typ
    let recordValueFields = FSharpValue.GetRecordFields record

    #if FABLE_COMPILER
    let expectedRecordFields =
        [|
            "string", box "a"
            "int", box 1
        |]
    #else
    let expectedRecordFields =
        [|
            "String", box "a"
            "Int", box 1
        |]
    #endif

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
let ``test FSharp.Reflection: GetUnionCases`` () =
    let typ = typeof<MyUnion>
    let cases = FSharpType.GetUnionCases typ
    cases.Length |> equal 2
    cases.[0].Name |> equal "StringCase"
    cases.[1].Name |> equal "IntCase"
    cases.[0].Tag |> equal 0
    cases.[1].Tag |> equal 1

[<Fact>]
let ``test FSharp.Reflection Union`` () =
    let typ = typeof<MyUnion>
    let unionCase1 = StringCase("a", "b")
    let unionCase2 = IntCase 1
    let unionTypeFields = FSharpType.GetUnionCases typ
    unionTypeFields |> Array.map (fun x -> x.Name) |> equal [| "StringCase"; "IntCase" |]
    let unionCase1Info, unionCase1ValueFields = FSharpValue.GetUnionFields(unionCase1, typ)
    let unionCase2Info, unionCase2ValueFields = FSharpValue.GetUnionFields(unionCase2, typ)

    unionCase1Info.Name |> equal "StringCase"
    unionCase2Info.Name |> equal "IntCase"
    unionCase1Info.Tag |> equal 0
    unionCase2Info.Tag |> equal 1
    unionCase1ValueFields |> equal [| box "a"; box "b" |]
    unionCase2ValueFields |> equal [| box 1 |]

    let canMakeSameUnionCases =
        unbox<MyUnion> (FSharpValue.MakeUnion(unionCase1Info, unionCase1ValueFields)) = unionCase1
        && unbox<MyUnion> (FSharpValue.MakeUnion(unionCase2Info, unionCase2ValueFields)) = unionCase2

    FSharpType.IsUnion typ |> equal true
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

[<Fact>]
let ``test Reflection info of int64 decimal with units of measure works`` () =
    typeof< int64 > = typeof< int64<FSharp.Data.UnitSystems.SI.UnitSymbols.m> > |> equal true
    typeof< decimal > = typeof< decimal<FSharp.Data.UnitSystems.SI.UnitSymbols.m> > |> equal true

[<Fact>]
let ``test Type.GenericTypeArguments works`` () =
    let recordType = typeof<AsyncRecord>
    let asyncProp = FSharpType.GetRecordFields recordType |> Array.head
    asyncProp.PropertyType.GenericTypeArguments |> Array.head |> equal typeof<string>

[<Fact>]
let ``test Type.GetGenericArguments works`` () =
    let t = typeof<int list>
    t.GetGenericArguments().[0] = typeof<int> |> equal true
    t.GetGenericArguments().[0] = typeof<string> |> equal false

[<Fact>]
let ``test GetInterface works when types are known at compile time`` () =
    let t = typeof<MyClass3<int>>.GetInterface("MyInterface`1")
    t.GetGenericArguments().[0] = typeof<int> |> equal true
    t.GetGenericArguments().[0] = typeof<string> |> equal false
    typeof<MyClass3<int>>.GetInterface("myInterface`1") |> isNull |> equal true
    typeof<MyClass3<int>>.GetInterface("myInterface`1", true) |> isNull |> equal false
    typeof<MyClass2>.GetInterface("MyInterface`1") |> isNull |> equal true

// === Recursive types ===
// Reflecting over a type whose fields mention the type itself must terminate, both when
// emitting the type info and when forcing it at runtime.

type Tree =
    | Leaf of int
    | Branch of Tree * Tree

type LinkedNode = { Value: int; Next: LinkedNode option }

type RecGeneric<'T> = { Head: 'T; Tail: RecGeneric<'T> option }

type OddExpr =
    | OddLit of int
    | OddPair of EvenExpr

and EvenExpr = { Left: OddExpr; Right: OddExpr }

type MyList<'T> =
    | Nil
    | Cons of 'T * MyList<'T>

// Same test as the JS and Python suites (tests/Js/Main/ReflectionTests.fs)
[<Fact>]
let ``test Recursive types work`` () =
    let cons =
        FSharpType.GetUnionCases(typeof<MyList<int>>)
        |> Array.find (fun x -> x.Name = "Cons")

    let fieldTypes = cons.GetFields()
    fieldTypes.[0].PropertyType.FullName |> equal typeof<int>.FullName

    fieldTypes.[1].PropertyType.GetGenericTypeDefinition().FullName
    |> equal typedefof<MyList<obj>>.FullName

[<Fact>]
let ``test reflection of self-recursive union works`` () =
    let cases = FSharpType.GetUnionCases typeof<Tree>
    cases.Length |> equal 2
    cases.[0].Name |> equal "Leaf"
    cases.[1].Name |> equal "Branch"

[<Fact>]
let ``test reflection of self-recursive union case fields works`` () =
    let cases = FSharpType.GetUnionCases typeof<Tree>
    let branchFields = cases.[1].GetFields()
    branchFields.Length |> equal 2
    branchFields.[0].PropertyType |> equal typeof<Tree>
    branchFields.[1].PropertyType |> equal typeof<Tree>

[<Fact>]
let ``test reflection of self-recursive record works`` () =
    let fields = FSharpType.GetRecordFields typeof<LinkedNode>
    fields.Length |> equal 2
    fields.[0].PropertyType |> equal typeof<int>
    fields.[1].PropertyType |> equal typeof<LinkedNode option>

[<Fact>]
let ``test reflection of recursive generic record works`` () =
    let fields = FSharpType.GetRecordFields typeof<RecGeneric<string>>
    fields.Length |> equal 2
    fields.[0].PropertyType |> equal typeof<string>
    fields.[1].PropertyType |> equal typeof<RecGeneric<string> option>

[<Fact>]
let ``test reflection of mutually recursive types works`` () =
    let cases = FSharpType.GetUnionCases typeof<OddExpr>
    cases.Length |> equal 2
    let fields = FSharpType.GetRecordFields typeof<EvenExpr>
    fields.Length |> equal 2
    fields.[0].PropertyType |> equal typeof<OddExpr>

[<Fact>]
let ``test IsUnion and IsRecord work for recursive types`` () =
    FSharpType.IsUnion typeof<Tree> |> equal true
    FSharpType.IsRecord typeof<LinkedNode> |> equal true
    FSharpType.IsRecord typeof<Tree> |> equal false
    FSharpType.IsUnion typeof<LinkedNode> |> equal false

[<Fact>]
let ``test MakeUnion round-trips a recursive union`` () =
    let cases = FSharpType.GetUnionCases typeof<Tree>
    let leaf = FSharpValue.MakeUnion(cases.[0], [| box 42 |])
    leaf |> equal (box (Leaf 42))

    let branch =
        FSharpValue.MakeUnion(cases.[1], [| box (Leaf 1); box (Leaf 2) |])

    branch |> equal (box (Branch(Leaf 1, Leaf 2)))

[<Fact>]
let ``test MakeRecord round-trips a recursive record`` () =
    let node =
        FSharpValue.MakeRecord(typeof<LinkedNode>, [| box 1; box (Some { Value = 2; Next = None }) |])

    node
    |> equal (box { Value = 1; Next = Some { Value = 2; Next = None } })

[<Fact>]
let ``test recursive type FullName works`` () =
    typeof<Tree>.FullName.EndsWith("Tree") |> equal true
    typeof<LinkedNode>.FullName.EndsWith("LinkedNode") |> equal true

// === FSharpReflectionExtensions (the allowAccessToPrivateRepresentation overloads) ===

// Same test as the JS suite (tests/Js/Main/ReflectionTests.fs)
[<Fact>]
let ``test Reflection functions accept allowAccessToPrivateRepresentation`` () =
    let recordType = typeof<MyRecord>
    let record = { String = "a"; Int = 1 }

    FSharpType
        .GetRecordFields(recordType, allowAccessToPrivateRepresentation = true)
        .Length
    |> equal 2

    let values =
        FSharpValue.GetRecordFields(record, allowAccessToPrivateRepresentation = true)

    let rebuilt =
        FSharpValue.MakeRecord(recordType, values, allowAccessToPrivateRepresentation = true) :?> MyRecord

    rebuilt |> equal record

    let unionType = typeof<MyUnion>
    let intCase = FSharpType.GetUnionCases(unionType).[1]

    let u =
        FSharpValue.MakeUnion(intCase, [| box 5 |], allowAccessToPrivateRepresentation = true) :?> MyUnion

    let info, fields =
        FSharpValue.GetUnionFields(u, unionType, allowAccessToPrivateRepresentation = true)

    info.Name |> equal "IntCase"
    fields.[0] |> equal (box 5)

[<Fact>]
let ``test IsRecord and IsUnion accept allowAccessToPrivateRepresentation`` () =
    FSharpType.IsRecord(typeof<MyRecord>, allowAccessToPrivateRepresentation = true)
    |> equal true

    FSharpType.IsUnion(typeof<MyUnion>, allowAccessToPrivateRepresentation = true)
    |> equal true

    FSharpType.IsRecord(typeof<MyUnion>, allowAccessToPrivateRepresentation = true)
    |> equal false

// === Enums ===

type MyEnum =
    | Foo = 1y
    | Bar = 5y
    | Baz = 8y

// Same test as the JS suite (tests/Js/Main/ReflectionTests.fs)
[<Fact>]
let ``test Reflection works with enums`` () =
    typeof<MyEnum>.IsEnum |> equal true
    typeof<int>.IsEnum |> equal false
    let t = typeof<MyEnum>
    t.IsEnum |> equal true
    t.GetEnumUnderlyingType() |> equal typeof<sbyte>
    System.Enum.GetUnderlyingType(t) |> equal typeof<sbyte>

[<Fact>]
let ``test Enum.GetValues works`` () =
    let values = System.Enum.GetValues(typeof<MyEnum>)
    values.Length |> equal 3

    // Iterating the non-generic System.Array is what needs Array.GetEnumerator
    let mutable count = 0

    for v in values do
        System.Enum.IsDefined(typeof<MyEnum>, v) |> equal true
        count <- count + 1

    count |> equal 3

[<Fact>]
let ``test Enum.GetNames works`` () =
    let names = System.Enum.GetNames(typeof<MyEnum>)
    names.Length |> equal 3
    names.[0] |> equal "Foo"
    names.[2] |> equal "Baz"

[<Fact>]
let ``test Enum.GetName and IsDefined work`` () =
    System.Enum.GetName(typeof<MyEnum>, MyEnum.Bar) |> equal "Bar"
    System.Enum.IsDefined(typeof<MyEnum>, MyEnum.Baz) |> equal true

// === Type.MakeGenericType / Activator.CreateInstance ===

// Portable Fable code guards .NET-only reflection behind `Compiler.isDotnet`. On Fable the
// branch is dead, but it still has to survive the replacement stage, which runs before DCE.
// Both branches must agree, since .NET takes the first one and Beam the second.
let defaultOf (t: System.Type) : obj =
    if Fable.Core.Compiler.isDotnet then
        System.Activator.CreateInstance(t)
    else
        box 0

[<Fact>]
let ``test Activator.CreateInstance guarded by Compiler.isDotnet compiles`` () =
    defaultOf typeof<int> |> equal (box 0)

[<Fact>]
let ``test Type.MakeGenericType substitutes the generic arguments`` () =
    let t = typedefof<RecGeneric<obj>>.MakeGenericType [| typeof<string> |]
    t.GetGenericArguments().[0] |> equal typeof<string>

// === Decimal ===

[<Fact>]
let ``test Decimal MinValue and MaxValue work`` () =
    string System.Decimal.MinValue
    |> equal "-79228162514264337593543950335"

    string System.Decimal.MaxValue |> equal "79228162514264337593543950335"
    System.Decimal.MinValue < 0M |> equal true
    System.Decimal.MaxValue > 0M |> equal true

[<Fact>]
let ``test Decimal explicit conversions work`` () =
    int 3.75M |> equal 3
    float 3.75M |> equal 3.75
    int64 3.75M |> equal 3L
    decimal 3 |> equal 3M
