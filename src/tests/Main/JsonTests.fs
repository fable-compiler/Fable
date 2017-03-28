[<Util.Testing.TestFixture>]
module Fable.Tests.Json
open Util.Testing
open Fable.Tests.Util

#if FABLE_COMPILER
let toJson(x) = Fable.Core.JsInterop.toJson(x)
let [<Fable.Core.PassGenerics>] ofJson<'T>(x) = Fable.Core.JsInterop.ofJson<'T>(x)
#else
open Newtonsoft.Json
let toJson x = JsonConvert.SerializeObject(x, Fable.JsonConverter())
let ofJson<'T> x = JsonConvert.DeserializeObject<'T>(x, Fable.JsonConverter())
#endif

type Child =
    { a: string
      b: int }

type Simple = {
    Name : string
    Child : Child
}

type U =
    | CaseA of int
    | CaseB of Simple list
    | CaseC of Child * int

type R() =
    member __.Foo() = "foo"

type A<'U> = {a: 'U}
type B<'J> = {b: A<'J>}
type C<'T> = {c: B<'T>}

[<Test>]
let ``Nested generics``() =
    let x = { c={ b={ a=R() } } }
    let json = toJson x
    let x2 = ofJson<C<R>> json
    x2.c.b.a.Foo() |> equal "foo"

type D<'Q> = {d: A<'Q> option }

[<Test>]
let ``Nested options``() =
    let x = { d = Some { a=R() } }
    let json = toJson x
    let x2 = ofJson<D<R>> json
    x2.d.Value.a.Foo() |> equal "foo"

type E<'Q> = { e: A<'Q>[] }

[<Test>]
let ``Nested arrays``() =
    let x = { e = [|{ a=R() }|] }
    let json = toJson x
    let x2 = ofJson<E<R>> json
    x2.e.[0].a.Foo() |> equal "foo"

type R2() =
    member __.Foo() = "bar"

type F<'Q,'V> = { f: A<'Q>*B<'V> }

[<Test>]
let ``Nested tuples``() =
    let x = { f = { a=R() }, { b={a=R2()} } }
    let json = toJson x
    let x2 = ofJson<F<R,R2>> json
    let (i, j) = x2.f
    i.a.Foo() |> equal "foo"
    j.b.a.Foo() |> equal "bar"

[<Test>]
let ``Records``() =
    let json =
        """
        {
            "Name": "foo",
            "Child": {
                "a": "Hi",
                "b": 10
            }
        }
        """
    let result: Simple = ofJson json
    result.Name |> equal "foo"
    // Use the built in compare to ensure the fields are being hooked up.
    // Should compile to something like: result.Child.Equals(new Child("Hi", 10))
    result.Child = {a="Hi"; b=10} |> equal true

#if FABLE_COMPILER
[<Test>]
let ``Validation works``() =
    let mutable err = false
    let json =
        """
        {
            "Name": "foo",
            "Child": {
                "a": 5,
                "b": 10
            }
        }
        """
    try
        ofJson<Simple> json |> ignore
    with _ -> err <- true
    equal true err
#endif

[<Test>]
let ``Date``() =
    let d = System.DateTime(2016, 1, 30, 11, 25, 0, System.DateTimeKind.Utc)
    let json = d |> toJson
    let result : System.DateTime = ofJson json
    result.Year |> equal 2016
    result.Month |> equal 1
    result.Day |> equal 30
    result.Hour |> equal 11
    result.Minute |> equal 25

[<Test>]
let ``Date Kind Unspecified roundtrip``() =
    let d = System.DateTime(2016, 1, 30, 11, 25, 0, System.DateTimeKind.Unspecified)
    let json = d |> toJson
    let result : System.DateTime = ofJson json
    result.Year |> equal 2016
    result.Month |> equal 1
    result.Day |> equal 30
    result.Hour |> equal 11
    result.Minute |> equal 25

type JsonDate = {
    Date : System.DateTime
}

[<Test>]
let ``Child Date``() =
    let d = System.DateTime(2016, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let json = { Date = d } |> toJson
    let result : JsonDate = ofJson json
    result.Date.Year |> equal 2016

type JsonArray = {
    Name : string
}

[<Test>]
let ``Arrays``() =
    let json = """[{ "Name": "a" }, { "Name": "b" }]"""
    let result : JsonArray[] = ofJson json
    result |> Array.length |> equal 2
    result.[1] = { Name="b" } |> equal true

[<Test>]
let ``Arrays 2``() =
    let x = [|R2()|]
    let json = toJson x
    let x2 = ofJson<R2[]> json
    x2.[0].Foo() |> equal "bar"

[<Test>]
let ``Typed Arrays``() =
    let xs1 = [|1;2;3|]
    let json = toJson xs1
    let xs2 = ofJson<int[]> json
    Array.sum xs2 |> equal (Array.sum xs1)

type ChildArray = {
    Children : JsonArray[]
}

[<Test>]
let ``Child Array``() =
    let json = """{ "Children": [{ "Name": "a" }, { "Name": "b" }] }"""
    let result : ChildArray = ofJson json
    result.Children |> Array.length |> equal 2
    result.Children.[1] = { Name="b" } |> equal true

[<Test>]
let ``String Generic List``() =
    let json = """["a","b"]"""
    let result : System.Collections.Generic.List<string> = ofJson json
    result.Count |> equal 2
    result.[1] |> equal "b"

[<Test>]
let ``Child Generic List``() =
    let json = """[{ "Name": "a" }, { "Name": "b" }]"""
    let result : System.Collections.Generic.List<JsonArray> = ofJson json
    result.Count |> equal 2
    result.[1] = { Name="b" } |> equal true

[<Test>]
let ``Lists``() =
    let json = """["a","b"]"""
    let result : string list = ofJson json
    result |> List.length |> equal 2
    result.Tail |> List.length |> equal 1
    result.[1] |> equal "b"
    result.Head |> equal "a"


type ChildList = {
    Children : JsonArray list
}

[<Test>]
let ``Child List``() =
    let json = """{ "Children": [{ "Name": "a" }, { "Name": "b" }] }"""
    let result : ChildList = ofJson json
    result.Children |> List.length |> equal 2
    result.Children.[1] = { Name="b" } |> equal true

type Wrapper<'T> = { thing : 'T }

let inline parseAndUnwrap json: 'T = (ofJson<Wrapper<'T>> json).thing

[<Test>]
let ``generic`` () =
    let result1 : string = parseAndUnwrap """ { "thing" : "a" } """
    result1 |> equal "a"
    let result2 : int = parseAndUnwrap """ { "thing" : 1 } """
    result2 |> equal 1
    let result3 : Child = parseAndUnwrap """ { "thing" : { "a": "a", "b": 1 } } """
    result3.a |> equal "a"
    result3 = {a = "a"; b = 1} |> equal true
    // let result4 : Child = parseAndUnwrap """ {"$type":"Fable.Tests.Json+Wrapper`1[[Fable.Tests.Json+Child, Fable.Tests]], Fable.Tests","thing":{"$type":"Fable.Tests.Json+Child, Fable.Tests","a":"a","b":1}} """
    // if result4 <> {a = "a"; b = 1} then
    //     invalidOp "things not equal"

type OptionJson =
    { a: int option }

[<Test>]
let ``Option Some`` () =
    let json1 = """ {"a":1 } """
    let result1 : OptionJson = ofJson json1
    let json2 = """ {"a":null } """
    let result2 : OptionJson = ofJson json2
    match result1.a, result2.a with
    | Some v, None -> v
    | _ -> -1
    |> equal 1

type ComplexOptionJson =
    { a: Child option }

[<Test>]
let ``Complex Option Some`` () =
    let json = """ {"a":{"a":"John","b":14}} """
    let result : ComplexOptionJson = ofJson json
    match result.a with
    | Some v -> v = {a="John";b=14}
    | None -> false
    |> equal true

type TupleJson =
    { a: int * int }

[<Test>]
let ``Tuple`` () =
    let json = """ {"a":[1,2]} """
    let result : TupleJson = ofJson json
    result.a = (1, 2) |> equal true

type TupleComplexJson =
    { a: int * Child }

[<Test>]
let ``Complex Tuple`` () =
    let json = """ {"a":[1,{"a":"A","b":1}]} """
    let result : TupleComplexJson = ofJson json
    snd result.a = { a = "A"; b = 1 } |> equal true

type SetJson =
    { a: Set<string> }

[<Test>]
let ``Sets`` () =
    let json = """ {"a":["a","b"]} """
    let result : SetJson = ofJson json
    result.a |> Set.contains "b" |> equal true

type MapJson =
    { a: Map<string, Child> }

[<Test>]
let ``Maps`` () =
    let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
    let result : MapJson = ofJson json
    result.a.Count |> equal 2
    result.a.["b"] = { a="bb"; b=2 } |> equal true

[<Test>]
let ``Map string keys are not quoted``() = // #659
    let v = [ "foo", 1 ] |> Map.ofList
    let serialised = toJson v
    (toJson v).Replace(" ", "") |> equal """{"foo":1}"""

type DictionaryJson =
    { a: System.Collections.Generic.Dictionary<string, Child> }

[<Test>]
let ``Dictionaries`` () =
    let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
    let result : DictionaryJson = ofJson json
    result.a.Count |> equal 2
    result.a.["b"] = { a="bb"; b=2 } |> equal true

// Dunno why, but this tests is not working with Json.NET
#if FABLE_COMPILER
type PropertyJson() =
    member val Prop1 = {a="";b=0} with get,set

[<Test>]
let ``Properties`` () =
    let json = """ {"Prop1": { "a":"aa", "b": 1 }} """
    let result : PropertyJson = ofJson json
    result.Prop1.a |> equal "aa"
    result.Prop1.b |> equal 1
#endif

[<Test>]
let ``Union of list``() =
    let u = CaseB [{Name="Sarah";Child={a="John";b=14}}]
    let u2: U = toJson u |> ofJson
    u = u2 |> equal true
    let u3: U = ofJson """{"CaseB":[{"Name":"Sarah","Child":{"a":"John","b":14}}]}"""
    u = u3 |> equal true


[<Test>]
let ``Union with multiple fields``() =
    let u = CaseC({a="John";b=14}, 2)
    let u2: U = toJson u |> ofJson
    u = u2 |> equal true
    let u3: U = ofJson """{"CaseC":[{"a":"John","b":14},2]}"""
    u = u3 |> equal true

type UnionJson =
    | Type1 of string
    | Type2 of Child

type UnionHolder =
    { a : UnionJson }

[<Test>]
let ``Union of record`` () =
    let json = """ {"a":{"Type2": {"a":"a","b":1} }} """
    let result : UnionHolder = ofJson json
    match result.a with
    | Type2 t -> t = { a="a"; b=1 }
    | Type1 _ -> false
    |> equal true

type MultiUnion =
    | EmptyCase
    | SingleCase of int
    | MultiCase of string * Child

[<Test>]
let ``Union case with no fields``() =
    let u: MultiUnion = ofJson """ "EmptyCase" """
    u = EmptyCase |> equal true

[<Test>]
let ``Union case with single field``() =
    let u: MultiUnion = ofJson """ {"SingleCase": 100} """
    u = (SingleCase 100) |> equal true

[<Test>]
let ``Union case with multiple fields``() =
    let u: MultiUnion = ofJson """ {"MultiCase": ["foo",{"a":"John","b":14}]} """
    let u2 = MultiCase("foo", {a="John"; b=14})
    u = u2 |> equal true


type OptionalUnionHolder =
    { a : UnionJson option }

[<Test>]
let ``Optional union of record: with a value`` () =
    let json = """ {"a":{"Type2": {"a":"a","b":1} }} """
    let result : OptionalUnionHolder = ofJson json
    match result.a with
    | Some (Type2 t) -> t = { a= "a"; b=1 }
    | _ -> false
    |> equal true

[<Test>]
let ``Optional union of record: for undefined`` () =
    let json = """ {} """
    let result : OptionalUnionHolder = ofJson json
    match result.a with
    | None -> true
    | _ -> false
    |> equal true

[<Test>]
let ``Optional union of record: for null`` () =
    let json = """ {"a":null} """
    let result : OptionalUnionHolder = ofJson json
    match result.a with
    | None -> true
    | _ -> false
    |> equal true

#if FABLE_COMPILER
type UnionWithCaseOfObj =
    | CaseOfObj of obj
    | AnotherCase

[<Test>]
let ``Union with case of obj with single undefined field value`` () =
    let x = UnionWithCaseOfObj.CaseOfObj Fable.Import.Node.``global``.undefined
    let json = toJson x
    let x2 = ofJson<UnionWithCaseOfObj> json
    x2 |> equal x
#endif

#if FABLE_COMPILER
type IData = interface end

type Text =
  { kind:string; text:string }
  interface IData

type Numbered =
  { kind:string; number:int }
  interface IData

type Things = { name:string; data:IData }

[<Test>]
let ``Generics with interface`` () =
    // let x = [ { name = "one"; data = { kind = "number"; number = 4 } };
    //            { name = "two"; data = { kind = "number"; number = 3 } };
    //            { name = "three"; data = { kind = "text"; text = "yo!" } } ]
    // let json = Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.JsonSerializerSettings(TypeNameHandling=Newtonsoft.Json.TypeNameHandling.All))
    let json = """ {"$type":"Microsoft.FSharp.Collections.FSharpList`1[[Fable.Tests.Json+Things, Fable.Tests]], FSharp.Core","$values":[{"$type":"Fable.Tests.Json+Things, Fable.Tests","name":"one","data":{"$type":"Fable.Tests.Json+Numbered, Fable.Tests","kind":"number","number":4}},{"$type":"Fable.Tests.Json+Things, Fable.Tests","name":"two","data":{"$type":"Fable.Tests.Json+Numbered, Fable.Tests","kind":"number","number":3}},{"$type":"Fable.Tests.Json+Things, Fable.Tests","name":"three","data":{"$type":"Fable.Tests.Json+Text, Fable.Tests","kind":"text","text":"yo!"}}]} """
    let result : Things list = Fable.Core.JsInterop.ofJsonWithTypeInfo json
    result.[1].data = ({ kind = "number"; number = 3 } :> IData) |> equal true

type RrdWithLong = { long: int64 }

[<Test>]
let ``Roundtripped int64 is convertible to float``() =
    let r = {long = 0L} |> toJson  |> ofJson<RrdWithLong>
    r.long |> float
    |> equal 0.
#endif