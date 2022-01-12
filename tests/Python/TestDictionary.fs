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
    let dic = Dictionary<_,_>()
    for i in 1. .. 10. do dic.Add(i, i*i)
    let i = ref 0.
    for KeyValue(x, y) in dic do
       i.Value <- y + i.Value
    equal 385. i.Value
