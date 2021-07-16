module Fable.Tests.Maps

open System.Collections.Generic
open Util.Testing

type R = { i: int; s: string }
type R2 = { kv: KeyValuePair<string,int> }

[<CustomEquality; CustomComparison>]
type R3 =
    { Bar: string
      Foo: int }
    interface System.IComparable with
        member this.CompareTo(x) =
            match x with
            | :? R3 as x -> compare this.Bar x.Bar
            | _ -> -1
    override this.GetHashCode() = hash this.Bar
    override this.Equals(x) =
        match x with
        | :? R3 as x -> this.Bar = x.Bar
        | _ -> false

type R4 =
    { Bar: string
      Baz: int }

let ``test Map construction from lists works`` () =
    let xs = Map [1,1; 2,2]
    xs |> Seq.isEmpty
    |> equal false

let ``test Map.isEmpty works`` () =
    let xs = Map []
    Map.isEmpty xs |> equal true
    let ys = Map [1,1]
    Map.isEmpty ys |> equal false
