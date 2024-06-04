//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections

module Generic =

    type Queue<'T>() =
        let xs = ResizeArray<'T>()

        member _.Clear () = xs.Clear()

        member _.Enqueue (item: 'T) =
            xs.Add(item)

        member _.Dequeue () =
            let item = xs.Item(0)
            xs.RemoveAt(0)
            item

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)

        interface System.Collections.Generic.IEnumerable<'T> with
            member _.GetEnumerator() =
                xs.GetEnumerator()

module Immutable =
    open System.Collections.Generic

    // not immutable, just a ResizeArray // TODO: immutable implementation
    type ImmutableArray<'T> =
        static member CreateBuilder() = ResizeArray<'T>()

    [<Sealed>]
    type ImmutableHashSet<'T>(values: 'T seq) =
        let xs = HashSet<'T>(values)

        static member Create<'T>(values) = ImmutableHashSet<'T>(values)
        static member Empty = ImmutableHashSet<'T>(Array.empty)

        member _.Add (value: 'T) =
            let copy = HashSet<'T>(xs)
            copy.Add(value) |> ignore
            ImmutableHashSet<'T>(copy)

        member _.Union (values: seq<'T>) =
            let copy = HashSet<'T>(xs)
            copy.UnionWith(values)
            ImmutableHashSet<'T>(copy)

        member _.Overlaps (values: seq<'T>) =
            // xs.Overlaps(values)
            values |> Seq.exists (fun x -> xs.Contains(x))

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)

        interface IEnumerable<'T> with
            member _.GetEnumerator() =
                xs.GetEnumerator()

    [<Sealed>]
    type ImmutableDictionary<'Key, 'Value when 'Key: equality>(xs: Dictionary<'Key, 'Value>) =
        static member Create(comparer: IEqualityComparer<'Key>) =
            ImmutableDictionary<'Key, 'Value>(Dictionary(comparer))

        static member CreateRange(items: IEnumerable<KeyValuePair<'Key, 'Value>>) =
            let xs = Dictionary<'Key, 'Value>()
            for pair in items do
                xs.Add(pair.Key, pair.Value)
            ImmutableDictionary<'Key, 'Value>(xs)

        static member Empty =
            ImmutableDictionary<'Key, 'Value>(Dictionary())

        member _.IsEmpty = xs.Count = 0
        member _.Item with get (key: 'Key): 'Value = xs[key]
        member _.ContainsKey (key: 'Key) = xs.ContainsKey(key)

        member _.Add (key: 'Key, value: 'Value) =
            let copy = Dictionary<'Key, 'Value>(xs)
            copy.Add(key, value)
            ImmutableDictionary<'Key, 'Value>(copy)

        member _.SetItem (key: 'Key, value: 'Value) =
            let copy = Dictionary<'Key, 'Value>(xs)
            copy[key] <- value
            ImmutableDictionary<'Key, 'Value>(copy)

        member _.TryGetValue (key: 'Key): bool * 'Value =
            match xs.TryGetValue(key) with
            | true, v -> (true, v)
            | false, v -> (false, v)

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)

        interface IEnumerable<KeyValuePair<'Key, 'Value>> with
            member _.GetEnumerator() =
                xs.GetEnumerator()

module Concurrent =
    open System.Collections.Generic

    // not thread safe, just a ResizeArray // TODO: threaded implementation
    type ConcurrentStack<'T>() =
        let xs = ResizeArray<'T>()

        member _.Push (item: 'T) = xs.Add(item)
        member _.PushRange (items: 'T[]) = xs.AddRange(items)
        member _.Clear () = xs.Clear()
        member _.ToArray () = xs.ToArray()

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)

        interface IEnumerable<'T> with
            member _.GetEnumerator() =
                xs.GetEnumerator()

    // not thread safe, just a Dictionary // TODO: threaded implementation
    [<AllowNullLiteral>]
    type ConcurrentDictionary<'Key, 'Value>(comparer: IEqualityComparer<'Key>) =
        let xs = Dictionary(comparer)

        new () =
            ConcurrentDictionary<'Key, 'Value>(EqualityComparer.Default)
        new (_concurrencyLevel: int, _capacity: int) =
            ConcurrentDictionary<'Key, 'Value>()
        new (_concurrencyLevel: int, comparer: IEqualityComparer<'Key>) =
            ConcurrentDictionary<'Key, 'Value>(comparer)
        new (_concurrencyLevel: int, _capacity: int, comparer: IEqualityComparer<'Key>) =
            ConcurrentDictionary<'Key, 'Value>(comparer)

        member _.Keys = xs.Keys
        member _.Values = xs.Values

        member _.Item
            with get (key: 'Key): 'Value = xs[key]
            and set (key: 'Key) (value: 'Value) = xs[key] <- value

        member _.Clear () = xs.Clear()
        member _.ContainsKey (key: 'Key) = xs.ContainsKey(key)

        member _.TryGetValue (key: 'Key): bool * 'Value =
            match xs.TryGetValue(key) with
            | true, v -> (true, v)
            | false, v -> (false, v)

        member _.TryAdd (key: 'Key, value: 'Value): bool =
            if xs.ContainsKey(key)
            then false
            else xs.Add(key, value); true

        member _.TryRemove (key: 'Key): bool * 'Value =
            match xs.TryGetValue(key) with
            | true, v -> (xs.Remove(key), v)
            | _ as res -> res

        member _.GetOrAdd (key: 'Key, value: 'Value): 'Value =
            match xs.TryGetValue(key) with
            | true, v -> v
            | _ -> let v = value in xs.Add(key, v); v

        member _.GetOrAdd (key: 'Key, valueFactory: System.Func<'Key, 'Value>): 'Value =
            match xs.TryGetValue(key) with
            | true, v -> v
            | _ -> let v = valueFactory.Invoke(key) in xs.Add(key, v); v

        // member _.GetOrAdd<'Arg> (key: 'Key, valueFactory: 'Key * 'Arg -> 'Value, arg: 'Arg): 'Value =
        //     match xs.TryGetValue(key) with
        //     | true, v -> v
        //     | _ -> let v = valueFactory(key, arg) in xs.Add(key, v); v

        member _.TryUpdate (key: 'Key, value: 'Value, comparisonValue: 'Value): bool =
            match xs.TryGetValue(key) with
            | true, v when Unchecked.equals v comparisonValue -> xs[key] <- value; true
            | _ -> false

        member _.AddOrUpdate (key: 'Key, value: 'Value, updateFactory: System.Func<'Key, 'Value, 'Value>): 'Value =
            match xs.TryGetValue(key) with
            | true, v -> let v = updateFactory.Invoke(key, v) in xs[key] <- v; v
            | _ -> let v = value in xs.Add(key, v); v

        // member _.AddOrUpdate (key: 'Key, valueFactory: 'Key -> 'Value, updateFactory: 'Key * 'Value -> 'Value): 'Value =
        //     match xs.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, v) in xs[key] <- v; v
        //     | _ -> let v = valueFactory(key) in xs.Add(key, v); v

        // member _.AddOrUpdate (key: 'Key, valueFactory: 'Key * 'Arg -> 'Value, updateFactory: 'Key * 'Arg * 'Value -> 'Value, arg: 'Arg): 'Value =
        //     match xs.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, arg, v) in xs[key] <- v; v
        //     | _ -> let v = valueFactory(key, arg) in xs.Add(key, v); v

        interface System.Collections.IEnumerable with
            member _.GetEnumerator() =
                (xs.GetEnumerator() :> System.Collections.IEnumerator)

        interface IEnumerable<KeyValuePair<'Key, 'Value>> with
            member _.GetEnumerator() =
                xs.GetEnumerator()
