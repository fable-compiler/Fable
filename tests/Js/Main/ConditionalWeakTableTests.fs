module Fable.Tests.ConditionalWeakTable

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open Util
open Util.Testing

let tests =
  testList "ConditionalWeakTables" [
    testCase "ConditionalWeakTable.TryGetValue works" <| fun () ->
        let key1, key2 = obj(), obj()
        let expected1, expected2 = obj(), obj()
        let dic1 = ConditionalWeakTable()
        let dic2 = ConditionalWeakTable()
        dic1.Add(key1, expected1)
        dic2.Add(key2, expected2)
        let success1, val1 = dic1.TryGetValue(key1)
        let success2, val2 = dic1.TryGetValue(key2)
        let success3, val3 = dic2.TryGetValue(key2)
        let success4, val4 = dic2.TryGetValue(obj())
        equal success1 true
        equal success2 false
        equal success3 true
        equal success4 false
        equal val1 expected1
        equal val2 null
        equal val3 expected2
        equal val4 null

    testCase "ConditionalWeakTable.Clear works" <| fun () ->
        let dic = ConditionalWeakTable<_,_>()
        let key1, key2 = obj(), obj()
        dic.Add(key1, obj())
        dic.Add(key2, obj())
        dic.Clear()
        dic.TryGetValue(key1) |> equal (false, null)
        dic.TryGetValue(key2) |> equal (false, null)

    testCase "ConditionalWeakTable.Remove works" <| fun () ->
        let dic = ConditionalWeakTable<_,_>()
        let key1, key2 = obj(), obj()
        dic.Add(key1, "Hello")
        dic.Add(key2, "World!")
        dic.Remove(key1) |> equal true
        dic.Remove("C") |> equal false
        dic.TryGetValue(key1) |> equal (false, null)
        dic.TryGetValue(key2) |> equal (true, "World!")

    testCase "Adding 2 items with the same key throws" <| fun () ->
        let dic = ConditionalWeakTable<_,_>()
        let key = obj()
        dic.Add(key, "foo")
        #if FABLE_COMPILER
        throwsError "An item with the same key has already been added. Key: [object Object]" (fun _ -> dic.Add(key, "bar"))
        #else
        throwsError "An item with the same key has already been added." (fun _ -> dic.Add(key, "bar"))
        #endif

    testCase "ConditionalWeakTable.GetValue works" <| fun () ->
        let dic = ConditionalWeakTable<_,_>()
        let key1, key2 = obj(), obj()
        let val1, val2 = obj(), obj()
        dic.Add(key1, val1)
        let value = dic.GetValue(key1, fun _ -> obj())
        value |> equal val1
        let value = dic.GetValue(key2, fun _ -> val2)
        value |> equal val2
  ]