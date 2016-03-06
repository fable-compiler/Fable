[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Dictionaries
open System
open System.Collections.Generic
open NUnit.Framework
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
let ``Dictionary indexer getter works``() =
    let dic = Dictionary<string,obj>()
    dic.Add("A", "Hello")
    dic.Add("B", 2)
    dic.["B"].ToString()
    |> equal "2"

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
