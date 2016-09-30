[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Json
open NUnit.Framework
open Fable.Tests.Util
open Newtonsoft.Json

#if FABLE_COMPILER
let inline toJson x = Fable.Core.JsInterop.toJson x
let inline ofJson<'T> x = Fable.Core.JsInterop.ofJson<'T> x
#else
let toJson = Newtonsoft.Json.JsonConvert.SerializeObject
let ofJson<'T> (json:string) = 
    let clean (s: string) = s.Replace(" ", "").Replace("\r", "").Replace("\n", "")

    let result, compareJson =
        if json.Contains("$type") then 
            let settings = Newtonsoft.Json.JsonSerializerSettings(TypeNameHandling = Newtonsoft.Json.TypeNameHandling.All)
            let r = Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(json, settings)
            r, Newtonsoft.Json.JsonConvert.SerializeObject(r, settings)
        else 
            let r = Newtonsoft.Json.JsonConvert.DeserializeObject<'T>(json)
            r, Newtonsoft.Json.JsonConvert.SerializeObject(r)

    (clean compareJson) |> equal (clean json)

    result    

#endif

[<CLIMutable>]
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

[<Test>]
let ``Simple json - Unions``() =
    let u = CaseB [{Name="Sarah";Child={a="John";b=14}}]
    toJson u |> ofJson<U> |> (=) u |> equal true
    """{"Case":"CaseB","Fields":[[{"Name":"Sarah","Child":{"a":"John","b":14}}]]}"""
    |> ofJson<U> |> (=) u |> equal true

[<Test>] 
let ``Date``() =
    let d = System.DateTime(2016, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let json = d |> toJson
    let result : System.DateTime = ofJson json
    result.Year |> equal 2016

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
let ``Record Array``() =
    let json = """[{ "Name": "a" }, { "Name": "b" }]"""
    let result : JsonArray[] = ofJson json
    result |> Array.length |> equal 2
    result.[1] = { Name="b" } |> equal true  

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
let ``String List``() =
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
let ``Generic`` () =
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
    let json1 = """ {"a":{"Case":"Some","Fields":[1]}} """
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
    let json = """ {"a":{"Case":"Some","Fields":[{"a":"John","b":14}]}} """
    let result : ComplexOptionJson = ofJson json

    match result.a with
    | Some v -> v = {a="John";b=14} |> equal true
    | _ -> invalidOp "Doesn't equal 1"


type TupleJson =
    { a: int * int }

[<Test>]
let ``Tuple`` () =
    let json = """ {"a":{"Item1":1,"Item2":2}} """
    let result : TupleJson = ofJson json
    result.a = (1, 2) |> equal true

type TupleComplexJson =
    { a: int * Child }

[<Test>]
let ``Complex Tuple`` () =
    let json = """ {"a":{"Item1":1,"Item2":{"a":"A","b":1}}} """
    let result : TupleComplexJson = ofJson json
    snd result.a = { a = "A"; b = 1 } |> equal true

type SetJson =
    { a: Set<string> }

[<Test>]
let ``Set`` () =
    let json = """ {"a":["a","b"]} """
    let result : SetJson = ofJson json
    result.a |> Set.contains "b" |> equal true

type MapJson =
    { a: Map<string, Child> }

[<Test>]
let ``Map`` () =
    let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
    let result : MapJson = ofJson json
    result.a.Count |> equal 2
    result.a.["b"] = { a="bb"; b=2 } |> equal true
    
type DictionaryJson =
    { a: System.Collections.Generic.Dictionary<string, Child> }

[<Test>]
let ``Dictionary`` () =
    let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
    let result : DictionaryJson = ofJson json
    result.a.Count |> equal 2
    result.a.["b"] = { a="bb"; b=2 } |> equal true


type PropertyJson() =
    member val Prop1 = {a="";b=0} with get,set

[<Test>]
let ``Properties`` () =
    let json = """ {"Prop1": { "a":"aa", "b": 1 }} """
    let result : PropertyJson = ofJson json

    if result.Prop1 <> { a="aa"; b=1 } then 
        invalidOp "Not equal"
        
        

type UnionJson =
    | Type1 of string
    | Type2 of Child

type UnionHolder =
    { a : UnionJson }


[<Test>]
let ``Union`` () =
    let json = """ {"a":{"Case":"Type2","Fields":[{"a":"a","b":1}]}} """
    let result : UnionHolder = ofJson json

    match result.a with
    | Type2 t -> 
        t = { a="a"; b=1 } |> equal true
    | _ ->
        invalidOp "Wrong case" 
  
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
    let result : Things list = ofJson json

    result.[1].data = ({ kind = "number"; number = 3 } :> IData) |> equal true
