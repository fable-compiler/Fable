module Fable.Tests.Dictionary

open System
open System.Collections.Generic
open Util.Testing


type MyRefType(i: int) =
    member x.Value = i

type MyRecord = { a: int }

type R = { i: int; s: string }

[<Fact>]
let ``test Dictionary KeyValuePattern works`` () = // See #509
    let dic = Dictionary<_, _>()

    for i in 1. .. 10. do
        dic.Add(i, i * i)

    let i = ref 0.

    for KeyValue (x, y) in dic do
        i.Value <- y + i.Value

    equal 385. i.Value

[<Fact>]
let ``test Dictionary creation works`` () =
    let dic = Dictionary<_, _>()
    equal 0 dic.Count

[<Fact>]
let ``test Interface IDictionary creation works`` () =
    let dic =
        dict
        <| seq { for i in 1. .. 10. -> i.ToString(), i * i }

    equal 4. dic.["2"]


[<Fact>]
let ``test Dictionary creation from IDictionary works`` () =
    let idic =
        dict
        <| seq { for i in 1..10 -> i.ToString(), i * i }

    let dic = Dictionary<_, _>(idic)
    dic.Add("100", 100)
    equal 10 idic.Count
    equal 11 dic.Count

[<Fact>]
let ``test Dictionaries with IEqualityComparer work`` () =
    let x = MyRefType(4)
    let y = MyRefType(4)
    let z = MyRefType(6)
    let dic = Dictionary<_,_>()
    dic.Add(x, "foo")
    dic.ContainsKey(x) |> equal true
    dic.ContainsKey(y) |> equal false

    let comparer =
        { new IEqualityComparer<MyRefType> with
            member _.Equals(x, y) = x.Value = y.Value
            member _.GetHashCode(x) = x.Value }
    let dic2 = Dictionary<_,_>(comparer)
    dic2.Add(x, "bar")
    dic2.ContainsKey(x) |> equal true
    dic2.ContainsKey(y) |> equal true
    dic2.ContainsKey(z) |> equal false

[<Fact>]
let ``test Interface IDictionary iteration works`` () =
    let dic =
        dict
        <| seq { for i in 1. .. 10. -> i.ToString(), i * i }

    let i = ref 0.

    for kv in dic do
        i.Value <- kv.Value + i.Value

    equal 385. i.Value

[<Fact>]
let ``test Interface IDictionary folding works`` () =
    let dic = dict [ ("A", 1.); ("B", 2.); ("C", 3.) ]

    dic
    |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 6.

[<Fact>]
let ``test Dictionary iteration works`` () =
    let dic = Dictionary<_, _>()

    for i in 1. .. 10. do
        dic.Add(i, i * i)

    let i = ref 0.

    for kv in dic do
        i.Value <- kv.Value + i.Value

    i.Value + dic.[1.] |> equal 386.

[<Fact>]
let ``test Dictionary folding works`` () =
    let dic = Dictionary<_, _>()

    for i in 1. .. 10. do
        dic.Add(i, i * i)

    dic
    |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 385.

[<Fact>]
let ``test Dictionary.Count works`` () =
    let dic = Dictionary<_, _>()

    for i in 1. .. 10. do
        dic.Add(i, i * i)

    dic.Count |> equal 10

[<Fact>]
let ``test Dictionary indexer works`` () =
    let dic = Dictionary<string, obj>()
    dic.["A"] <- "Hello"
    dic.["B"] <- 2
    dic.["B"].ToString() |> equal "2"

[<Fact>]
let ``test Dictionary.TryGetValue works`` () =
    let dic1 = dict [ "A", 1 ]
    let dic2 = dict [ "B", "2" ]
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

[<Fact>]
let ``test Dictionary.Keys works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", 1)
    dic.Add("B", 2)

    dic.Keys
    |> Seq.fold (fun acc k -> acc + dic.[k]) 0
    |> equal 3

[<Fact>]
let ``test Dictionary.Keys.Count works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Keys.Count |> equal 2

[<Fact>]
let ``test Dictionary.Values works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    let i = ref 0

    for value in dic.Values do
        i.Value <- value + i.Value

    i.Value |> equal 3

[<Fact>]
let ``test Dictionary.Values.Count works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Values.Count |> equal 2

[<Fact>]
let ``test Dictionary.Clear works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Clear()
    dic.Count |> equal 0

[<Fact>]
let ``test IDictionary.Clear works`` () = // see #1120
    let dic: IDictionary<_, _> = upcast Dictionary()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Clear()
    dic.Count |> equal 0

[<Fact>]
let ``test Dictionary.Add works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Count |> equal 2

[<Fact>]
let ``test Dictionary.ContainsKey works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

[<Fact>]
let ``test Dictionary.ContainsValue works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsValue("Hello") |> equal true
    dic.ContainsValue("Everybody!") |> equal false

[<Fact>]
let ``test Dictionary.Delete works`` () =
    let dic = Dictionary<_, _>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsValue("Hello") |> equal true
    dic.Remove("A") |> equal true
    dic.ContainsValue("Hello") |> equal false

[<Fact>]
let ``test Dictionary.Delete works with tuples as keys`` () =
    let my_dict = Dictionary((Map [for i in 0..10 do yield (i,i), sprintf "Number: %i" i]))
    my_dict.ContainsKey((0,0)) |> equal true
    my_dict.Remove((0,0)) |> equal true
    my_dict.ContainsKey((0,0)) |> equal false
