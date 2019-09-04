module Fable.Tests.Dictionaries

open System
open System.Collections.Generic
open Util
open Util.Testing

type MyRefType(i: int) =
    member x.Value = i

type MyRecord = { a: int }

type R = { i: int; s: string }

let tests =
  testList "Dictionaries" [
    testCase "Dictionary KeyValuePattern works" <| fun () -> // See #509
        let dic = Dictionary<_,_>()
        for i in 1. .. 10. do dic.Add(i, i*i)
        let i = ref 0.
        for KeyValue(x,y) in dic do
           i := y + !i
        equal 385. !i

    testCase "Dictionary creation works" <| fun () ->
        let dic = Dictionary<_,_>()
        equal 0 dic.Count

    testCase "Interface IDictionary creation works" <| fun () ->
        let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
        equal 4. dic.["2"]

    testCase "Dictionary creation from IDictionary works" <| fun () ->
        let idic = dict <| seq { for i in 1 .. 10 -> i.ToString(), i*i }
        let dic = Dictionary<_,_>(idic)
        dic.Add("100", 100)
        equal 10 idic.Count
        equal 11 dic.Count

    testCase "Dictionaries with IEqualityComparer work" <| fun () ->
        let x = MyRefType(4)
        let y = MyRefType(4)
        let z = MyRefType(6)
        let dic = Dictionary<_,_>()
        dic.Add(x, "foo")
        dic.ContainsKey(x) |> equal true
        dic.ContainsKey(y) |> equal false

        let comparer =
            { new IEqualityComparer<MyRefType> with
                member __.Equals(x, y) = x.Value = y.Value
                member __.GetHashCode(x) = x.Value }
        let dic2 = Dictionary<_,_>(comparer)
        dic2.Add(x, "bar")
        dic2.ContainsKey(x) |> equal true
        dic2.ContainsKey(y) |> equal true
        dic2.ContainsKey(z) |> equal false

    testCase "Interface IDictionary iteration works" <| fun () ->
        let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
        let i = ref 0.
        for kv in dic do
           i := kv.Value + !i
        equal 385. !i

    testCase "Interface IDictionary folding works" <| fun () ->
        let dic = dict [ ("A", 1.); ("B", 2.); ("C", 3.) ]
        dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
        |> equal 6.

    testCase "Dictionary iteration works" <| fun () ->
        let dic = Dictionary<_,_>()
        for i in 1. .. 10. do dic.Add(i, i*i)
        let i = ref 0.
        for kv in dic do
           i := kv.Value + !i
        !i + dic.[1.]
        |> equal 386.

    testCase "Dictionary folding works" <| fun () ->
        let dic = Dictionary<_,_>()
        for i in 1. .. 10. do dic.Add(i, i*i)
        dic |> Seq.fold (fun acc item -> acc + item.Value) 0.
        |> equal 385.

    testCase "Dictionary.Count works" <| fun () ->
        let dic = Dictionary<_,_>()
        for i in 1. .. 10. do dic.Add(i, i*i)
        dic.Count
        |> equal 10

    testCase "Dictionary indexer works" <| fun () ->
        let dic = Dictionary<string,obj>()
        dic.["A"] <- "Hello"
        dic.["B"] <- 2
        dic.["B"].ToString()
        |> equal "2"

    testCase "Dictionary.TryGetValue works" <| fun () ->
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

    testCase "Dictionary.Keys works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 1)
        dic.Add("B", 2)
        dic.Keys |> Seq.fold (fun acc k -> acc + dic.[k]) 0
        |> equal 3

    testCase "Dictionary.Keys.Count works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 1)
        dic.Add("B", 2)
        dic.Keys.Count
        |> equal 2

    testCase "Dictionary.Values works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 1)
        dic.Add("B", 2)
        let i = ref 0
        for value in dic.Values do
           i := value + !i
        !i |> equal 3

    testCase "Dictionary.Values.Count works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 1)
        dic.Add("B", 2)
        dic.Values.Count
        |> equal 2

    testCase "Dictionary.Clear works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 1)
        dic.Add("B", 2)
        dic.Clear()
        dic.Count |> equal 0

    testCase "IDictionary.Clear works" <| fun () -> // see #1120
        let dic: IDictionary<_,_> = upcast Dictionary()
        dic.Add("A", 1)
        dic.Add("B", 2)
        dic.Clear()
        dic.Count |> equal 0

    testCase "Dictionary.Add works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.Count |> equal 2

    testCase "Dictionary.ContainsKey works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.ContainsKey("A") |> equal true
        dic.ContainsKey("C") |> equal false

    testCase "Dictionary.ContainsValue works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.ContainsValue("Hello") |> equal true
        dic.ContainsValue("Everybody!") |> equal false

    testCase "Dictionary.Remove works" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", "Hello")
        dic.Add("B", "World!")
        dic.Remove("A") |> equal true
        dic.Remove("C") |> equal false

    testCase "Dictionary.Remove with records as keys works" <| fun () ->
        let x1 = { a = 5 }
        let x2 = { a = 5 }
        let x3 = { a = 10 }
        let x4 = { a = 15 }
        let dic = Dictionary<_,_>()
        dic.Add(x1, "Hello")
        dic.Add(x3, "World!")
        dic.Remove(x2) |> equal true
        dic.Remove(x4) |> equal false

    testCase "Interface IDictionary.Count works" <| fun () ->
        let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
        dic.Count |> equal 10

    testCase "Interface IDictionary indexer getter works" <| fun () ->
        let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
        dic.["2"] |> equal 4.

    testCase "Interface IDictionary.Keys works" <| fun () ->
        let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
        let i = ref ""
        for key in dic.Keys do
           i := key + !i
        !i |> equal "10987654321"

    testCase "Interface IDictionary.Values works" <| fun () ->
        let dic = dict <| seq { for i in 1. .. 10. -> i.ToString(), i*i }
        let i = ref 0.
        for value in dic.Values do
           i := value + !i
        !i |> equal 385.

    testCase "Interface IDictionary.ContainsKey works" <| fun () ->
        let dic = dict <| [ ("A", 1.); ("B", 2.) ]
        dic.ContainsKey("A") |> equal true
        dic.ContainsKey("C") |> equal false

    testCase "Adding 2 items with the same key throws" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 65)
        throwsError "An item with the same key has already been added. Key: A" (fun _ -> dic.Add("A", 95))

    testCase "Indexer throws when key not found" <| fun () ->
        let dic = Dictionary<_,_>()
        dic.Add("A", 65)
        // throwsError "The given key 'B' was not present in the dictionary." (fun _ -> dic.["B"] |> ignore)
        throwsAnyError (fun _ -> dic.["B"] |> ignore)

    testCase "conversion from array works" <| fun () ->
        let dic = [| "A",1; "B",2|] |> dict
        dic.Values.Count
        |> equal 2
        
    testCase "conversion from array works with duplicates" <| fun () ->
        let dic = [| "A",1; "A",3; "B",2|] |> dict
        dic.Values.Count
        |> equal 2        

  ]
