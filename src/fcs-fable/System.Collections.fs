//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections

module Immutable =
    open System.Collections.Generic

    // not immutable, just a ResizeArray // TODO: immutable implementation
    type ImmutableArray<'T> =
        static member CreateBuilder() = ResizeArray<'T>()

    [<Sealed>]
    type ImmutableHashSet<'T when 'T: equality>(values: 'T seq) =
        let xs = HashSet<'T>(values)

        static member Create<'T>(values) = ImmutableHashSet<'T>(values)
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

module Concurrent =
    open System.Collections.Generic

    // not thread safe, just a ResizeArray // TODO: threaded implementation
    type ConcurrentStack<'T>() =
        let xs = ResizeArray<'T>()

        member _.Push (item: 'T) = xs.Add(item)
        member _.PushRange (items: 'T[]) = xs.AddRange(items)
        member _.Clear () = xs.Clear()
        member _.ToArray () = xs.ToArray()

        interface IEnumerable<'T> with
            member _.GetEnumerator() =
                xs.GetEnumerator()

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)

    // not thread safe, just a Dictionary // TODO: threaded implementation
    [<AllowNullLiteral>]
    type ConcurrentDictionary<'K, 'V>(comparer: IEqualityComparer<'K>) =
        let xs = Dictionary(comparer)

        new () =
            ConcurrentDictionary<'K, 'V>(EqualityComparer.Default)
        new (_concurrencyLevel: int, _capacity: int) =
            ConcurrentDictionary<'K, 'V>()
        new (_concurrencyLevel: int, comparer: IEqualityComparer<'K>) =
            ConcurrentDictionary<'K, 'V>(comparer)
        new (_concurrencyLevel: int, _capacity: int, comparer: IEqualityComparer<'K>) =
            ConcurrentDictionary<'K, 'V>(comparer)

        member _.Comparer = comparer
        member _.Keys = xs.Keys
        member _.Values = xs.Values

        member _.Item
            with get (key: 'K): 'V = xs[key]
            and set (key: 'K) (value: 'V) = xs[key] <- value

        member _.Clear () = xs.Clear()
        member _.ContainsKey (key: 'K) = xs.ContainsKey(key)

        member _.TryGetValue (key: 'K): bool * 'V =
            match xs.TryGetValue(key) with
            | true, v -> (true, v)
            | false, v -> (false, v)

        member _.TryAdd (key: 'K, value: 'V): bool =
            if xs.ContainsKey(key)
            then false
            else xs.Add(key, value); true

        member _.TryRemove (key: 'K): bool * 'V =
            match xs.TryGetValue(key) with
            | true, v -> (xs.Remove(key), v)
            | _ as res -> res

        member _.GetOrAdd (key: 'K, value: 'V): 'V =
            match xs.TryGetValue(key) with
            | true, v -> v
            | _ -> let v = value in xs.Add(key, v); v

        member _.GetOrAdd (key: 'K, valueFactory: System.Func<'K, 'V>): 'V =
            match xs.TryGetValue(key) with
            | true, v -> v
            | _ -> let v = valueFactory.Invoke(key) in xs.Add(key, v); v

        // member _.GetOrAdd<'Arg> (key: 'K, valueFactory: 'K * 'Arg -> 'V, arg: 'Arg): 'V =
        //     match xs.TryGetValue(key) with
        //     | true, v -> v
        //     | _ -> let v = valueFactory(key, arg) in xs.Add(key, v); v

        member _.TryUpdate (key: 'K, value: 'V, comparisonValue: 'V): bool =
            // match xs.TryGetValue(key) with
            // | true, v when Unchecked.equals v comparisonValue -> xs[key] <- value; true
            // | _ -> false
            xs[key] <- value
            true

        member _.AddOrUpdate (key: 'K, value: 'V, updateFactory: System.Func<'K, 'V, 'V>): 'V =
            match xs.TryGetValue(key) with
            | true, v -> let v = updateFactory.Invoke(key, v) in xs[key] <- v; v
            | _ -> let v = value in xs.Add(key, v); v

        // member _.AddOrUpdate (key: 'K, valueFactory: 'K -> 'V, updateFactory: 'K * 'V -> 'V): 'V =
        //     match xs.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, v) in xs[key] <- v; v
        //     | _ -> let v = valueFactory(key) in xs.Add(key, v); v

        // member _.AddOrUpdate (key: 'K, valueFactory: 'K * 'Arg -> 'V, updateFactory: 'K * 'Arg * 'V -> 'V, arg: 'Arg): 'V =
        //     match xs.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, arg, v) in xs[key] <- v; v
        //     | _ -> let v = valueFactory(key, arg) in xs.Add(key, v); v

        interface IEnumerable<KeyValuePair<'K, 'V>> with
            member _.GetEnumerator() =
                xs.GetEnumerator()

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)
