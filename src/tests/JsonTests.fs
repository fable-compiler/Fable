[<NUnit.Framework.TestFixture>] 
module Fable.Tests.JsonTests
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