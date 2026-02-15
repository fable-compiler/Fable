module Fable.Tests.Dictionaries

open System
open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

// --- Construction ---

[<Fact>]
let ``test Dictionary creation works`` () =
    let dic = Dictionary<string, int>()
    dic.Count |> equal 0

[<Fact>]
let ``test Dictionary.Add works`` () =
    let dic = Dictionary<string, string>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Count |> equal 2

[<Fact>]
let ``test Dictionary.Count works`` () =
    let dic = Dictionary<int, int>()
    for i in 1 .. 10 do dic.Add(i, i*i)
    dic.Count |> equal 10

// --- Indexer ---

[<Fact>]
let ``test Dictionary indexer get works`` () =
    let dic = Dictionary<string, int>()
    dic.["A"] <- 1
    dic.["B"] <- 2
    dic.["A"] |> equal 1
    dic.["B"] |> equal 2

[<Fact>]
let ``test Dictionary indexer set works`` () =
    let dic = Dictionary<string, int>()
    dic.["A"] <- 1
    dic.["A"] <- 99
    dic.["A"] |> equal 99
    dic.Count |> equal 1

// --- ContainsKey ---

[<Fact>]
let ``test Dictionary.ContainsKey works`` () =
    let dic = Dictionary<string, string>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

// --- ContainsValue ---

[<Fact>]
let ``test Dictionary.ContainsValue works`` () =
    let dic = Dictionary<string, string>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.ContainsValue("Hello") |> equal true
    dic.ContainsValue("Everybody!") |> equal false

// --- Remove ---

[<Fact>]
let ``test Dictionary.Remove works`` () =
    let dic = Dictionary<string, string>()
    dic.Add("A", "Hello")
    dic.Add("B", "World!")
    dic.Remove("A") |> equal true
    dic.Remove("C") |> equal false
    dic.Count |> equal 1

// --- TryGetValue ---

[<Fact>]
let ``test Dictionary.TryGetValue works`` () =
    let dic = Dictionary<string, int>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    let success1, val1 = dic.TryGetValue("A")
    let success2, _val2 = dic.TryGetValue("C")
    success1 |> equal true
    val1 |> equal 1
    success2 |> equal false

// --- Clear ---

[<Fact>]
let ``test Dictionary.Clear works`` () =
    let dic = Dictionary<string, int>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Clear()
    dic.Count |> equal 0

// --- dict function ---

[<Fact>]
let ``test dict creation from list works`` () =
    let dic = dict [ ("A", 1); ("B", 2); ("C", 3) ]
    dic.["B"] |> equal 2
    dic.Count |> equal 3

[<Fact>]
let ``test dict ContainsKey works`` () =
    let dic = dict [ ("A", 1); ("B", 2) ]
    dic.ContainsKey("A") |> equal true
    dic.ContainsKey("C") |> equal false

// --- Integer keys ---

[<Fact>]
let ``test Dictionary with integer keys works`` () =
    let dic = Dictionary<int, string>()
    dic.Add(1, "one")
    dic.Add(2, "two")
    dic.[1] |> equal "one"
    dic.ContainsKey(2) |> equal true
    dic.ContainsKey(3) |> equal false

// --- Duplicate key throws ---

[<Fact>]
let ``test Adding duplicate key throws`` () =
    let dic = Dictionary<string, int>()
    dic.Add("A", 1)
    let mutable threw = false
    try
        dic.Add("A", 2)
    with _ ->
        threw <- true
    threw |> equal true

// --- Indexer throws for missing key ---

[<Fact>]
let ``test Indexer throws for missing key`` () =
    let dic = Dictionary<string, int>()
    dic.Add("A", 1)
    let mutable threw = false
    try
        dic.["B"] |> ignore
    with _ ->
        threw <- true
    threw |> equal true

// --- Iteration ---

[<Fact>]
let ``test Dictionary iteration with for-in works`` () =
    let dic = Dictionary<string, int>()
    dic.Add("A", 1)
    dic.Add("B", 2)
    dic.Add("C", 3)
    let mutable sum = 0
    for kv in dic do
        sum <- sum + kv.Value
    sum |> equal 6

// --- From existing dictionary ---

[<Fact>]
let ``test Dictionary creation from dict works`` () =
    let idic = dict [ ("A", 1); ("B", 2) ]
    let dic = Dictionary<string, int>(idic)
    dic.Add("C", 3)
    dic.Count |> equal 3
    idic.Count |> equal 2
