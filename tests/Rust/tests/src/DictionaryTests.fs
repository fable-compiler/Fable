module Fable.Tests.DictionaryTests

open Util.Testing
open System.Collections.Generic

// type MyRefType(i: int) =
//     member x.Value = i

type MyRecord = { a: int }

// type R = { i: int; s: string }

// [<Fact>]
// let ``Dictionary KeyValuePattern works`` () = // See #509
//     let dict = Dictionary<_,_>()
//     for i in 1 .. 10 do dict.Add(i, i*i)
//     let mutable i = 0
//     for KeyValue(x,y) in dict do
//         i <- i + y
//     i |> equal 385

[<Fact>]
let ``Dictionary ctor works`` () =
    let dict = Dictionary<int, int>()
    dict.Count |> equal 0

[<Fact>]
let ``Dictionary ctor with capacity works`` () =
    let dict = Dictionary<int, int>(10)
    dict.Count |> equal 0

// [<Fact>]
// let ``Dictionary ctor from IEnumerable works`` () =
//     let xs = seq { ("A", 1); ("B", 2); ("C", 3) }
//     let dict = Dictionary(xs |> Seq.map KeyValuePair)
//     dict.Count |> equal 3

[<Fact>]
let ``IDictionary ctor works`` () =
    let dict = dict <| seq { ("A", 1); ("B", 2); ("A", 3) }
    dict.Count |> equal 2

// [<Fact>]
// let ``Dictionary ctor from IDictionary works`` () =
//     let idic = dict <| seq { ("A", 1); ("B", 2); ("A", 3) }
//     let dict = Dictionary<_,_>(idic)
//     dict.Add("100", 100)
//     equal 3 idic.Count
//     equal 4 dict.Count

// [<Fact>]
// let ``Dictionaries with IEqualityComparer work`` () =
//     let x = MyRefType(4)
//     let y = MyRefType(4)
//     let z = MyRefType(6)
//     let dict = Dictionary<_,_>()
//     dict.Add(x, "foo")
//     dict.ContainsKey(x) |> equal true
//     dict.ContainsKey(y) |> equal false
//     let comparer =
//         { new IEqualityComparer<MyRefType> with
//             member __.Equals(x, y) = x.Value = y.Value
//             member __.GetHashCode(x) = x.Value }
//     let dic2 = Dictionary<_,_>(comparer)
//     dic2.Add(x, "bar")
//     dic2.ContainsKey(x) |> equal true
//     dic2.ContainsKey(y) |> equal true
//     dic2.ContainsKey(z) |> equal false

[<Fact>]
let ``Interface IDictionary iteration works`` () =
    let dict = dict <| seq { ("A", 1); ("B", 2); ("A", 3) }
    let mutable i = 0
    for kv in dict do
        i <- i + kv.Value
    i |> equal 5

[<Fact>]
let ``Interface IDictionary folding works`` () =
    let dict = dict [ ("A", 1.); ("B", 2.); ("C", 3.) ]
    dict |> Seq.fold (fun acc item -> acc + item.Value) 0.
    |> equal 6.

[<Fact>]
let ``Dictionary iteration works`` () =
    let dict = Dictionary<_,_>()
    for i in 1 .. 10 do dict.Add(i, i*i)
    let mutable i = 0
    for kv in dict do
        i <- i + kv.Value
    i + dict[1]
    |> equal 386

[<Fact>]
let ``Dictionary folding works`` () =
    let dict = Dictionary<_,_>()
    for i in 1 .. 10 do dict.Add(i, i*i)
    dict |> Seq.fold (fun acc item -> acc + item.Value) 0
    |> equal 385

[<Fact>]
let ``Dictionary.Count works`` () =
    let dict = Dictionary<_,_>()
    for i in 1 .. 10 do
        dict.Add(i, i*i)
    dict.Count |> equal 10

[<Fact>]
let ``Dictionary indexer works`` () =
    let dict = Dictionary()
    dict["A"] <- 1
    dict["B"] <- 2
    dict["A"] <- 3
    dict["A"] |> equal 3
    dict["B"] |> equal 2

// [<Fact>]
// let ``Dictionary indexer works II`` () =
//     let dict = Dictionary<string, obj>()
//     dict["A"] <- "Hello"
//     dict["B"] <- 2
//     dict["B"].ToString()
//     |> equal "2"

[<Fact>]
let ``Dictionary.TryAdd works`` () =
    let dict = Dictionary<_,_>()
    dict.TryAdd("A", "Hello") |> equal true
    dict.TryAdd("B", "World!") |> equal true
    dict.TryAdd("A", "Bye!") |> equal false
    dict.Count |> equal 2
    dict["A"] |> equal "Hello"

[<Fact>]
let ``Dictionary.TryGetValue works`` () =
    let dict = Dictionary<_,_>()
    dict.TryAdd("A", 1) |> equal true
    dict.TryAdd("B", 2) |> equal true
    dict.TryAdd("A", 3) |> equal false
    dict.Count |> equal 2
    let mutable v = 0
    dict.TryGetValue("A", &v) |> equal true
    v |> equal 1

[<Fact>]
let ``Dictionary.TryGetValue works II`` () =
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
    // equal val4 null //TODO:

[<Fact>]
let ``Dictionary.Keys works`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", 1)
    dict.Add("B", 2)
    dict.Keys |> Seq.fold (fun acc k -> acc + dict[k]) 0
    |> equal 3

[<Fact>]
let ``Dictionary.Keys.Count works`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", 1)
    dict.Add("B", 2)
    dict.Keys.Count
    |> equal 2

[<Fact>]
let ``Dictionary.Values works`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", 1)
    dict.Add("B", 2)
    let mutable i = 0
    for value in dict.Values do
        i <- value + i
    i |> equal 3

[<Fact>]
let ``Dictionary.Values.Count works`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", 1)
    dict.Add("B", 2)
    dict.Values.Count
    |> equal 2

[<Fact>]
let ``Dictionary.Clear works`` () =
    let dict = Dictionary<_,_>()
    dict.Add(1, "A")
    dict.Add(2, "B")
    dict.Clear()
    dict.Count |> equal 0

[<Fact>]
let ``IDictionary.Clear works`` () = // see #1120
    let dict: IDictionary<_,_> = upcast Dictionary()
    dict.Add("A", 1)
    dict.Add("B", 2)
    dict.Clear()
    dict.Count |> equal 0

[<Fact>]
let ``Dictionary.Add works`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", "Hello")
    dict.Add("B", "World!")
    dict.Count |> equal 2

[<Fact>]
let ``Dictionary.ContainsKey works`` () =
    let dict = Dictionary<_,_>()
    dict.Add(1, "Hello")
    dict.Add(2, "World!")
    dict.ContainsKey(1) |> equal true
    dict.ContainsKey(3) |> equal false
    dict.Count |> equal 2

[<Fact>]
let ``Dictionary.ContainsKey works II`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", "Hello")
    dict.Add("B", "World!")
    dict.ContainsKey("A") |> equal true
    dict.ContainsKey("C") |> equal false

[<Fact>]
let ``Dictionary.ContainsValue works`` () =
    let dict = Dictionary<_,_>()
    dict.Add("A", "Hello")
    dict.Add("B", "World!")
    dict.ContainsValue("Hello") |> equal true
    dict.ContainsValue("Everybody!") |> equal false

[<Fact>]
let ``Dictionary.Remove works`` () =
    let dict = Dictionary<_,_>()
    dict.Add(1, "Hello")
    dict.Add(2, "World!")
    dict.Remove(1) |> equal true
    dict.Remove(3) |> equal false
    dict.Count |> equal 1

[<Fact>]
let ``Dictionary.Remove with records as keys works`` () =
    let x1 = { a = 5 }
    let x2 = { a = 5 }
    let x3 = { a = 10 }
    let x4 = { a = 15 }
    let dict = Dictionary<_,_>()
    dict.Add(x1, "Hello")
    dict.Add(x3, "World!")
    dict.Remove(x2) |> equal true
    dict.Remove(x4) |> equal false
    dict.Count |> equal 1

[<Fact>]
let ``Interface IDictionary.Count works`` () =
    let dict = dict <| seq { for i in 1 .. 10 -> i, i*i }
    dict.Count |> equal 10

[<Fact>]
let ``Interface IDictionary indexer getter works`` () =
    let dict = dict <| seq { for i in 1 .. 10 -> i, i*i }
    dict[2] |> equal 4

[<Fact>]
let ``Interface IDictionary.Keys works`` () =
    let dict = dict <| seq { for i in 1 .. 10 -> i, i*i }
    let mutable i = 0
    for key in dict.Keys do
        i <- i + key
    i |> equal 55

[<Fact>]
let ``Interface IDictionary.Values works`` () =
    let dict = dict <| seq { for i in 1 .. 10 -> i, i*i }
    let mutable i = 0
    for value in dict.Values do
        i <- i + value
    i |> equal 385

[<Fact>]
let ``Interface IDictionary.ContainsKey works`` () =
    let dict = dict <| [ ("A", 1.); ("B", 2.) ]
    dict.ContainsKey("A") |> equal true
    dict.ContainsKey("C") |> equal false

// [<Fact>]
// let ``Adding 2 items with the same key throws`` () =
//     let dict = Dictionary<_,_>()
//     dict.Add("A", 65)
//     throwsError "An item with the same key has already been added. Key: A" (fun _ -> dict.Add("A", 95))

// [<Fact>]
// let ``Indexer throws when key not found`` () =
//     let dict = Dictionary<_,_>()
//     dict.Add("A", 65)
//     // throwsError "The given key 'B' was not present in the dictionary." (fun _ -> dict["B"] |> ignore)
//     throwsAnyError (fun () -> dict["B"])

[<Fact>]
let ``conversion from array works`` () =
    let dict = [| "A",1; "B",2 |] |> dict
    dict.Values.Count
    |> equal 2

[<Fact>]
let ``conversion from array works with duplicates`` () =
    let dict = [| "A",1; "A",3; "B",2 |] |> dict
    dict.Values.Count
    |> equal 2

// [<Fact>]
// let ``Dictionary with type as key works`` () = // See #2202
//     let cache = Dictionary<Type, int>()
//     cache.Add(typeof<int>, 1)
//     cache.Add(typeof<string>, 2)
//     cache.Add(typeof<int64>, 3)
//     equal 3 cache.Count
//     equal cache[typeof<int>] 1
//     equal cache[typeof<string>] 2
//     equal cache[typeof<int64>] 3
