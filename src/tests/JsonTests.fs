[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Json
open NUnit.Framework
open Fable.Tests.Util

type Child =
    { a: string
      b: int }

type Simple = {
    Name : string
    Child : Child
}

[<Test>]
let ``Simple json - Records``() =
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

    let result: Simple = Fable.Core.JsInterop.ofJsonSimple json
    
    result.Name |> equal "foo"
    
    // Use the built in compare to ensure the fields are beening hooked up.
    // Should compile to something like: result.Child.Equals(new Child("Hi", 10))
    if result.Child <> {a="Hi"; b=10} then
        invalidOp "Child not equal"  
      
[<Test>] 
let ``Simple json - Date``() =
    let d = System.DateTime(2016, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let json = d |> Fable.Core.JsInterop.toJson
    let result : System.DateTime = Fable.Core.JsInterop.ofJsonSimple json

    result.Year |> equal 2016

type JsonDate = {  
    Date : System.DateTime
}

        
[<Test>] 
let ``Simple json - Child Date``() =
    let d = System.DateTime(2016, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)
    let json = { Date = d } |> Fable.Core.JsInterop.toJson
    let result : JsonDate = Fable.Core.JsInterop.ofJsonSimple json

    result.Date.Year |> equal 2016


type JsonArray = {
    Name : string
}

[<Test>] 
let ``Simple json - Array``() =
    let json = """[{ "Name": "a" }, { "Name": "b" }]"""
    let result : JsonArray[] = Fable.Core.JsInterop.ofJsonSimple json

    result |> Array.length |> equal 2

    if result.[1] <> { Name="b" } then
        invalidOp "Child not equal"  

type ChildArray = {
    Children : JsonArray[]
}

[<Test>] 
let ``Simple json - Child Array``() =
    let json = """{ "Children": [{ "Name": "a" }, { "Name": "b" }] }"""
    let result : ChildArray = Fable.Core.JsInterop.ofJsonSimple json

    result.Children |> Array.length |> equal 2

    if result.Children.[1] <> { Name="b" } then
        invalidOp "Child not equal"  

[<Test>] 
let ``Simple json - String Generic List``() =
    let json = """["a","b"]"""
    let result : System.Collections.Generic.List<string> = Fable.Core.JsInterop.ofJsonSimple json

    result.Count |> equal 2
    result.[1] |> equal "b"

[<Test>] 
let ``Simple json - Child Generic List``() =
    let json = """[{ "Name": "a" }, { "Name": "b" }]"""
    let result : System.Collections.Generic.List<JsonArray> = Fable.Core.JsInterop.ofJsonSimple json

    result.Count |> equal 2

    if result.[1] <> { Name="b" } then
        invalidOp "Child not equal"  

[<Test>] 
let ``Simple json - List``() =
    let json = """["a","b"]"""
    let result : string list = Fable.Core.JsInterop.ofJsonSimple json

    result |> List.length |> equal 2
    result.[1] |> equal "b"

type ChildList = {
    Children : JsonArray list
}

[<Test>] 
let ``Simple json - Child List``() =
    let json = """{ "Children": [{ "Name": "a" }, { "Name": "b" }] }"""
    let result : ChildList = Fable.Core.JsInterop.ofJsonSimple json

    result.Children |> List.length |> equal 2

    if result.Children.[1] <> { Name="b" } then
        invalidOp "Child not equal"  

type Wrapper<'T> = { thing : 'T }

[<Test>]
let ``Simple json - generic`` () =
    let parseAndUnwrap (json) : 'T = (Fable.Core.JsInterop.ofJsonSimple<Wrapper<'T>> json).thing

    let result1 : string = parseAndUnwrap """ { "thing" : "a" } """
    result1 |> equal "a"

    let result2 : int = parseAndUnwrap """ { "thing" : 1 } """
    result2 |> equal 1

    let result3 : Child = parseAndUnwrap """ { "thing" : { "a": "a", "b": 1 } } """
    result3.a |> equal "a"

    let parsedCorrectly =
        try 
            result3 = {a = "a"; b = 1}
        with _ ->
            false

    if parsedCorrectly then
        invalidOp "Complex object should not have equal hooked up" 

    let result4 : Child = parseAndUnwrap """ {"$type":"Fable.Tests.Json+Wrapper`1[[Fable.Tests.Json+Child, Fable.Tests]], Fable.Tests","thing":{"$type":"Fable.Tests.Json+Child, Fable.Tests","a":"a","b":1}} """
    if result4 <> {a = "a"; b = 1} then
        invalidOp "things not equal" 

type OptionJson =
    { a: int option }

[<Test>]
let ``Simple json - Option Some`` () =
    let json = """ {"a":{"Case":"Some","Fields":[1]}} """
    let result : OptionJson = Fable.Core.JsInterop.ofJsonSimple json

    match result.a with
    | Some v -> v |> equal 1
    | _ -> invalidOp "Doesn't equal 1"


type TupleJson =
    { a: int * int }

[<Test>]
let ``Simple json - Tuple`` () =
    let json = """ {"a":{"Item1":1,"Item2":2}} """
    let result : TupleJson = Fable.Core.JsInterop.ofJsonSimple json

    if result.a <> (1, 2) then
        invalidOp "Not equal"


type TupleComplexJson =
    { a: int * Child }

[<Test>]
let ``Simple json - Complex Tuple`` () =
    let json = """ {"a":{"Item1":1,"Item2":{"a":"A","b":1}}} """
    let result : TupleComplexJson = Fable.Core.JsInterop.ofJsonSimple json

    if snd result.a  <> { a = "A"; b = 1 } then
        invalidOp "Not equal"

type SetJson =
    { a: Set<string> }

[<Test>]
let ``Simple json - Set`` () =
    let json = """ {"a":["a","b"]} """
    let result : SetJson = Fable.Core.JsInterop.ofJsonSimple json

    if result.a |> Set.contains "b" |> not then
        invalidOp "b is missing"

type MapJson =
    { a: Map<string, Child> }

[<Test>]
let ``Simple json - Map`` () =
    let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
    let result : MapJson = Fable.Core.JsInterop.ofJsonSimple json

    result.a.Count |> equal 2
    if result.a.["b"] <> { a="bb"; b=2 } then 
        invalidOp "Not equal"
    
type DictionaryJson =
    { a: System.Collections.Generic.Dictionary<string, Child> }

[<Test>]
let ``Simple json - Dictionary`` () =
    let json = """ {"a":{"a":{"a":"aa","b":1},"b":{"a":"bb","b":2}}} """
    let result : DictionaryJson = Fable.Core.JsInterop.ofJsonSimple json

    result.a.Count |> equal 2
    if result.a.["b"] <> { a="bb"; b=2 } then 
        invalidOp "Not equal"
