//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections.Immutable

open System.Collections.Generic

// not immutable, just an Array // TODO: immutable implementation
type ImmutableArray<'T> = 'T array

module ImmutableArray =
    let CreateBuilder<'T>() = ResizeArray<'T>()

    let Create<'T>(items: 'T[], start: int, length: int) =
        items[start..(start + length - 1)]

    let ofSeq<'T>(items: seq<'T>) = Array.ofSeq items

[<Sealed>]
type ImmutableHashSet<'T when 'T: equality>(values: 'T seq) =
    let xs = HashSet<'T>(values)

    static member Create<'T>(values: 'T seq) = ImmutableHashSet<'T>(values)
    static member Empty = ImmutableHashSet<'T>(Array.empty)

    member _.Add (value: 'T) =
        let copy = HashSet<'T>(xs)
        copy.Add(value) |> ignore
        ImmutableHashSet<'T>(copy)

    member _.Union (values: seq<'T>) =
        let copy = HashSet<'T>(xs)
        // copy.UnionWith(values)
        for value in values do
            copy.Add(value) |> ignore
        ImmutableHashSet<'T>(copy)

    member _.Overlaps (values: seq<'T>) =
        // xs.Overlaps(values)
        values |> Seq.exists (fun x -> xs.Contains(x))

    interface IEnumerable<'T> with
        member _.GetEnumerator() =
            xs.GetEnumerator()

    interface System.Collections.IEnumerable with
        member _.GetEnumerator() =
            (xs.GetEnumerator() :> System.Collections.IEnumerator)

[<Sealed>]
type ImmutableDictionary<'K, 'V when 'K: equality>(xs: Dictionary<'K, 'V>) =
    static member Create(comparer: IEqualityComparer<'K>) =
        ImmutableDictionary<'K, 'V>(Dictionary(comparer))

    static member CreateRange(items: IEnumerable<KeyValuePair<'K, 'V>>) =
        let xs = Dictionary<'K, 'V>()
        for pair in items do
            xs.Add(pair.Key, pair.Value)
        ImmutableDictionary<'K, 'V>(xs)

    static member Empty =
        ImmutableDictionary<'K, 'V>(Dictionary())

    member _.IsEmpty = xs.Count = 0
    member _.Item with get (key: 'K): 'V = xs[key]
    member _.ContainsKey (key: 'K) = xs.ContainsKey(key)

    member _.Add (key: 'K, value: 'V) =
        let copy = Dictionary<'K, 'V>(xs)
        copy.Add(key, value)
        ImmutableDictionary<'K, 'V>(copy)

    member _.SetItem (key: 'K, value: 'V) =
        let copy = Dictionary<'K, 'V>(xs)
        copy[key] <- value
        ImmutableDictionary<'K, 'V>(copy)

    member _.TryGetValue (key: 'K): bool * 'V =
        match xs.TryGetValue(key) with
        | true, v -> (true, v)
        | false, v -> (false, v)

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member _.GetEnumerator() =
            xs.GetEnumerator()

    interface System.Collections.IEnumerable with
        member _.GetEnumerator() =
            (xs.GetEnumerator() :> System.Collections.IEnumerator)
