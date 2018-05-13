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
type D<'Q> = {d: A<'Q> option }
type E<'Q> = { e: A<'Q>[] }
type R2() =
    member __.Foo() = "bar"

type F<'Q,'V> = { f: A<'Q>*B<'V> }

type JsonDate = {
    Date : System.DateTime
}

type JsonArray = {
    Name : string
}

type ChildArray = {
    Children : JsonArray[]
}

type ChildList = {
    Children : JsonArray list
}

type Wrapper<'T> = { thing : 'T }

let inline parseAndUnwrap json: 'T = (ofJson<Wrapper<'T>> json).thing

type OptionJson =
    { a: int option }

type ComplexOptionJson =
    { a: Child option }

type TupleJson =
    { a: int * int }

type OptionalTuple =
    { tuple: (string * string) option }

type TupleComplexJson =
    { a: int * Child }

type SetJson =
    { a: Set<string> }

type MapJson =
    { a: Map<string, Child> }

type DictionaryJson =
    { a: System.Collections.Generic.Dictionary<string, Child> }

type PropertyJson() =
    member val Prop1 = {a="";b=0} with get,set

type UnionJson =
    | Type1 of string
    | Type2 of Child

type UnionHolder =
    { a : UnionJson }

type MultiUnion =
    | EmptyCase
    | SingleCase of int
    | MultiCase of string * Child

type OptionalUnionHolder =
    { a : UnionJson option }

type UnionWithCaseOfObj =
    | CaseOfObj of obj
    | AnotherCase

[<Fable.Core.Emit("void 0")>]
let jsUndefined = obj()

type IData = interface end

type Text =
  { kind:string; text:string }
  interface IData

type Numbered =
  { kind:string; number:int }
  interface IData

type Things = { name:string; data:IData }

type RrdWithLong = { long: int64 }

type WrappedInteger = { Data : int }

let tests = 
    testList "Json Tests" [
        testCase "Nested generics" <| fun () ->
            let x = { c={ b={ a=R() } } }
            let json = toJson x
            let x2 = ofJson<C<R>> json
            x2.c.b.a.Foo() |> equal "foo"


        testCase "Nested options" <| fun () ->
            let x = { d = Some { a=R() } }
            let json = toJson x
            let x2 = ofJson<D<R>> json
            x2.d.Value.a.Foo() |> equal "foo"


        testCase "Nested arrays" <| fun () ->
            let x = { e = [|{ a=R() }|] }
            let json = toJson x
            let x2 = ofJson<E<R>> json
            x2.e.[0].a.Foo() |> equal "foo"


        testCase "Nested tuples" <| fun () ->
            let x = { f = { a=R() }, { b={a=R2()} } }
            let json = toJson x
            let x2 = ofJson<F<R,R2>> json
            let (i, j) = x2.f
            i.a.Foo() |> equal "foo"
            j.b.a.Foo() |> equal "bar"

        testCase "Records" <| fun () ->
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
        testCase "Validation works" <| fun () ->
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
    
        testCase "Date" <| fun () ->
            let d = System.DateTime(2016, 1, 30, 11, 25, 0, System.DateTimeKind.Utc)
            let json = d |> toJson
            let result : System.DateTime = ofJson json
            result.Year |> equal 2016
            result.Month |> equal 1
            result.Day |> equal 30
            result.Hour |> equal 11
            result.Minute |> equal 25

        testCase "Date Kind Unspecified roundtrip" <| fun () ->
            let d = System.DateTime(2016, 1, 30, 11, 25, 0, System.DateTimeKind.Unspecified)
            let json = d |> toJson
            let result : System.DateTime = ofJson json
            result.Year |> equal 2016
            result.Month |> equal 1
            result.Day |> equal 30
            result.Hour |> equal 11
            result.Minute |> equal 25


        testCase "Child Date" <| fun () ->
            let d = System.DateTime(2016, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
            let json = { Date = d } |> toJson
            let result : JsonDate = ofJson json
            result.Date.Year |> equal 2016

        testCase "Arrays" <| fun () ->
            let json = """[{ "Name": "a" }, { "Name": "b" }]"""
            let result : JsonArray[] = ofJson json
            result |> Array.length |> equal 2
            result.[1] = { Name="b" } |> equal true

        testCase "Arrays 2" <| fun () ->
            let x = [|R2()|]
            let json = toJson x
            let x2 = ofJson<R2[]> json
            x2.[0].Foo() |> equal "bar"

        testCase "Typed Arrays" <| fun () ->
            let xs1 = [|1;2;3|]
            let json = toJson xs1
            let xs2 = ofJson<int[]> json
            Array.sum xs2 |> equal (Array.sum xs1)

        testCase "Child Array" <| fun () ->
            let json = """{ "Children": [{ "Name": "a" }, { "Name": "b" }] }"""
            let result : ChildArray = ofJson json
            result.Children |> Array.length |> equal 2
            result.Children.[1] = { Name="b" } |> equal true

        testCase "String Generic List" <| fun () ->
            let json = """["a","b"]"""
            let result : System.Collections.Generic.List<string> = ofJson json
            result.Count |> equal 2
            result.[1] |> equal "b"

        testCase "Child Generic List" <| fun () ->
            let json = """[{ "Name": "a" }, { "Name": "b" }]"""
            let result : System.Collections.Generic.List<JsonArray> = ofJson json
            result.Count |> equal 2
            result.[1] = { Name="b" } |> equal true

        testCase "Lists" <| fun () ->
            let json = """["a","b"]"""
            let result : string list = ofJson json
            result |> List.length |> equal 2
            result.Tail |> List.length |> equal 1
            result.[1] |> equal "b"
            result.Head |> equal "a"

        testCase "Child List" <| fun () ->
            let json = """{ "Children": [{ "Name": "a" }, { "Name": "b" }] }"""
            let result : ChildList = ofJson json
            result.Children |> List.length |> equal 2
            result.Children.[1] = { Name="b" } |> equal true

        testCase "generic`` " <| fun () ->
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

        testCase "Option Some`` " <| fun () ->
            let json1 = """ {"a":1 } """
            let result1 : OptionJson = ofJson json1
            let json2 = """ {"a":null } """
            let result2 : OptionJson = ofJson json2
            match result1.a, result2.a with
            | Some v, None -> v
            | _ -> -1
            |> equal 1

        testCase "Option Wrapped Some`` " <| fun () ->
            let createSome (a: 'T) = Some a
            let a = createSome 1
            let (opt: OptionJson) = { a=a } 
            let json1 = toJson <| opt
            equal (json1.Replace(" ", "")) """{"a":1}"""
            let result1 : OptionJson = ofJson json1
            match result1.a with
            | Some v -> v
            | _ -> -1
            |> equal 1

        testCase "Complex Option Some`` " <| fun () ->
            let json = """ {"a":{"a":"John","b":14}} """
            let result : ComplexOptionJson = ofJson json
            match result.a with
            | Some v -> v = {a="John";b=14}
            | None -> false
            |> equal true

        testCase "Tuple`` " <| fun () ->
            let json = """ {"a":[1,2]} """
            let result : TupleJson = ofJson json
            result.a = (1, 2) |> equal true

        testCase "Optional Tuple in records`` " <| fun () ->

            let json1 = """ { "tuple": ["a", "b"] } """
            let result1 : OptionalTuple = ofJson json1
            let json2 = """ { "tuple": null } """
            let result2 : OptionalTuple = ofJson json2
            match result1.tuple, result2.tuple with
            | Some v, None -> v
            | _ -> ("", "")
            |> equal ("a", "b")


        testCase "Complex Tuple`` " <| fun () ->
            let json = """ {"a":[1,{"a":"A","b":1}]} """
            let result : TupleComplexJson = ofJson json
            snd result.a = { a = "A"; b = 1 } |> equal true

        testCase "Sets`` " <| fun () ->
            let json = """ {"a":["a","b"]} """
            let result : SetJson = ofJson json
            result.a |> Set.contains "b" |> equal true

        testCase "Maps`` " <| fun () ->
            let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
            let result : MapJson = ofJson json
            result.a.Count |> equal 2
            result.a.["b"] = { a="bb"; b=2 } |> equal true

        testCase "Map string keys are not quoted``() = // " <| fun () ->
            let v = [ "foo", 1 ] |> Map.ofList
            let serialised = toJson v
            (toJson v).Replace(" ", "") |> equal """{"foo":1}"""

        testCase "Dictionaries`` " <| fun () ->
            let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
            let result : DictionaryJson = ofJson json
            result.a.Count |> equal 2
            result.a.["b"] = { a="bb"; b=2 } |> equal true

        // Dunno why, but this tests is not working with Json.NET
        #if FABLE_COMPILER

        testCase "Properties`` " <| fun () ->
            let json = """ {"Prop1": { "a":"aa", "b": 1 }} """
            let result : PropertyJson = ofJson json
            result.Prop1.a |> equal "aa"
            result.Prop1.b |> equal 1
        #endif

        testCase "Union of list" <| fun () ->
            let u = CaseB [{Name="Sarah";Child={a="John";b=14}}]
            let u2: U = toJson u |> ofJson
            u = u2 |> equal true
            let u3: U = ofJson """{"CaseB":[{"Name":"Sarah","Child":{"a":"John","b":14}}]}"""
            u = u3 |> equal true


        testCase "Union with multiple fields" <| fun () ->
            let u = CaseC({a="John";b=14}, 2)
            let u2: U = toJson u |> ofJson
            u = u2 |> equal true
            let u3: U = ofJson """{"CaseC":[{"a":"John","b":14},2]}"""
            u = u3 |> equal true

        testCase "Union of record`` " <| fun () ->
            let json = """ {"a":{"Type2": {"a":"a","b":1} }} """
            let result : UnionHolder = ofJson json
            match result.a with
            | Type2 t -> t = { a="a"; b=1 }
            | Type1 _ -> false
            |> equal true

        testCase "Union case with no fields" <| fun () ->
            let u: MultiUnion = ofJson """ "EmptyCase" """
            u = EmptyCase |> equal true

        testCase "Union case with single field" <| fun () ->
            let u: MultiUnion = ofJson """ {"SingleCase": 100} """
            u = (SingleCase 100) |> equal true

        testCase "Union case with multiple fields" <| fun () ->
            let u: MultiUnion = ofJson """ {"MultiCase": ["foo",{"a":"John","b":14}]} """
            let u2 = MultiCase("foo", {a="John"; b=14})
            u = u2 |> equal true

        testCase "Optional union of record: with a value`` " <| fun () ->
            let json = """ {"a":{"Type2": {"a":"a","b":1} }} """
            let result : OptionalUnionHolder = ofJson json
            match result.a with
            | Some (Type2 t) -> t = { a= "a"; b=1 }
            | _ -> false
            |> equal true

        testCase "Optional union of record: for undefined`` " <| fun () ->
            let json = """ {} """
            let result : OptionalUnionHolder = ofJson json
            match result.a with
            | None -> true
            | _ -> false
            |> equal true

        testCase "Optional union of record: for null`` " <| fun () ->
            let json = """ {"a":null} """
            let result : OptionalUnionHolder = ofJson json
            match result.a with
            | None -> true
            | _ -> false
            |> equal true

        #if FABLE_COMPILER

        testCase "Union with case of obj with single undefined field value`` " <| fun () ->
            let x = UnionWithCaseOfObj.CaseOfObj jsUndefined
            let json = toJson x
            let x2 = ofJson<UnionWithCaseOfObj> json
            x2 |> equal x
        #endif

        #if FABLE_COMPILER

        testCase "Generics with interface`` " <| fun () ->
            // let x = [ { name = "one"; data = { kind = "number"; number = 4 } };
            //            { name = "two"; data = { kind = "number"; number = 3 } };
            //            { name = "three"; data = { kind = "text"; text = "yo!" } } ]
            // let json = Newtonsoft.Json.JsonConvert.SerializeObject(x, Newtonsoft.Json.JsonSerializerSettings(TypeNameHandling=Newtonsoft.Json.TypeNameHandling.All))
            let json = """ {"$type":"Microsoft.FSharp.Collections.FSharpList`1[[Fable.Tests.Json+Things, Fable.Tests]], FSharp.Core","$values":[{"$type":"Fable.Tests.Json+Things, Fable.Tests","name":"one","data":{"$type":"Fable.Tests.Json+Numbered, Fable.Tests","kind":"number","number":4}},{"$type":"Fable.Tests.Json+Things, Fable.Tests","name":"two","data":{"$type":"Fable.Tests.Json+Numbered, Fable.Tests","kind":"number","number":3}},{"$type":"Fable.Tests.Json+Things, Fable.Tests","name":"three","data":{"$type":"Fable.Tests.Json+Text, Fable.Tests","kind":"text","text":"yo!"}}]} """
            let result : Things list = Fable.Core.JsInterop.ofJsonWithTypeInfo json
            result.[1].data = ({ kind = "number"; number = 3 } :> IData) |> equal true

        testCase "Roundtripped int64 is convertible to float" <| fun () ->
            let r = {long = 0L} |> toJson  |> ofJson<RrdWithLong>
            r.long |> float
            |> equal 0.

        testCase "Can roundtrip Result of non-primitive type`` " <| fun () ->
            let x = { Data = 0 }
            let wx : Result<_, unit> = Ok x
            let wx' = wx |> toJson |> ofJson
            match wx' with
            | Ok x' ->
                if x' = x then
                    ()
                else
                    failwithf "Unexpected data %A" x'
            | res -> failwithf "Unexpected result %A" res
        #endif
    ]