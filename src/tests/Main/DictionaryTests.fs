[<Util.Testing.TestFixture>]
module Fable.Tests.Dictionaries
open System
open System.Collections.Generic
open Util.Testing
open Fable.Tests.Util

[<Test>]
let ``Dictionary creation works``() =
    let dic = Dictionary<_,_>()
    equal 0 dic.Count

[<Test>]
let ``Interface IDictionary creation works``() =
    let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
    equal 4. dic.["2"]

[<Test>]
let ``Dictionary creation from IDictionary works``() =
    let idic = dict <| seq { for i in 1 .. 10 -> i.ToString(), i*i }
    let dic = Dictionary<_,_>(idic)
    dic.Add("100", 100)
    equal 10 idic.Count
    equal 11 dic.Count

[<Test>]
let ``Interface IDictionary iteration works``() =
    let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
    let i = ref 0.
    for kv in dic do
       i := kv.Value + !i
    equal 385. !i

[<Test>]
let ``Interface IDictionary folding works``() =
    let dic = dict [ ("A", 1.); ("B", 2.); ("C", 3.) ]
    dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 6.

[<Test>]
let ``Dictionary iteration works``() =
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    let i = ref 0.
    for kv in dic do
       i := kv.Value + !i
    !i + dic.[1.]
    |> equal 386.

[<Test>]
let ``Dictionary folding works``() =
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 385.

[<Test>]
let ``Dictionary.Count works``() =
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    dic.Count
    |> equal 10

[<Test>]
let ``Dictionary indexer works``() =
    let dic = Dictionary<string,obj>()
    dic.["A"] <- "Hello"
    dic.["B"] <- 2
    dic.["B"].ToString()
    |> equal "2"

[<Test>]
let ``Dictionary.TryGetValue works``() =
    let dic1 = dict ["A", 1]
    let dic2 = dict ["B", "2"]
    let success1, val1 = dic1.TryGetValue("A")
    let success2, val2 = dic1.TryGetValue("B")
    let success3, val3 = dic2.TryGetValue("B")
    let success4, val4 = dic2.TryGetValue("C")
    equal success1 true
    equal success2 false
    equal success3 true
    equal success4 false
    equal val1 1
    equal val2 0
    equal val3 "2"
    equal val4 null

[<Test>]
let ``Dictionary.Keys works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Keys |> Seq.fold (fun acc k -> acc + dic.[k]) 0
    |> equal 3

[<Test>]
let ``Dictionary.Keys.Count works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Keys.Count
    |> equal 2

[<Test>]
let ``Dictionary.Values works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    let i = ref 0
    for value in dic.Values do
       i := value + !i
    !i |> equal 3

[<Test>]
let ``Dictionary.Values.Count works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Values.Count
    |> equal 2

[<Test>]
let ``Dictionary.Clear works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Clear()
    dic.Count |> equal 0

[<Test>]
let ``Dictionary.Add works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Count |> equal 2

[<Test>]
let ``Dictionary.ContainsKey works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

[<Test>]
let ``Dictionary.ContainsValue works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsValue("Hello") |> equal true
    dic.ContainsValue("Everybody!") |> equal false

[<Test>]
let ``Dictionary.Remove works``() =
    let dic = Dictionary<_,_>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Remove("A") |> equal true
    dic.Remove("C") |> equal false

[<Test>]
let ``Interface IDictionary.Count works``() =
    let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
    dic.Count |> equal 10

[<Test>]
let ``Interface IDictionary indexer getter works``() =
    let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
    dic.["2"] |> equal 4.

[<Test>]
let ``Interface IDictionary.Keys works``() =
    let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
    let i = ref ""
    for key in dic.Keys do
       i := key + !i
    !i |> equal "10987654321"

[<Test>]
let ``Interface IDictionary.Values works``() =
    let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
    let i = ref 0.
    for value in dic.Values do
       i := value + !i
    !i |> equal 385.

[<Test>]
let ``Interface IDictionary.ContainsKey works``() =
    let dic = dict <| [ ("A", 1.); ("B", 2.) ]
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

type R = { i: int; s: string }

[<Test>]
let ``Dictionaries can be JSON serialized forth and back``() =
    let x = Dictionary<_,_>()
    x.Add("a", { i=1; s="1" })
    x.Add("b", { i=2; s="2" })
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<Dictionary<string, R>> json
    (0, x2) ||> Seq.fold (fun acc kv -> acc + kv.Value.i)
    |> equal 3
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Dictionary<string, R>> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string, R>> json
    #endif
    (0, x2) ||> Seq.fold (fun acc kv -> acc + kv.Value.i)
    |> equal 3

[<Test>]
let ``Dictionaries serialized with Json.NET can be deserialized``() =
    // let x = Dictionary<_,_>()
    // x.Add("a", { i=1; s="1" })
    // x.Add("b", { i=2; s="2" })
    // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
    let json = """{"$type":"System.Collections.Generic.Dictionary`2[[System.String, mscorlib],[Fable.Tests.Maps+R, Fable.Tests]], FSharp.Core","a":{"$type":"Fable.Tests.Maps+R, Fable.Tests","i":1,"s":"1"},"b":{"$type":"Fable.Tests.Maps+R, Fable.Tests","i":2,"s":"2"}}"""
    #if FABLE_COMPILER
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Dictionary<string, R>> json
    #else
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string, R>> json
    #endif
    (0, x2) ||> Seq.fold (fun acc kv -> acc + kv.Value.i)
    |> equal 3
