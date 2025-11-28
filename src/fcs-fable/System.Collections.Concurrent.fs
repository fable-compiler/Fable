//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections.Concurrent

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
type ConcurrentDictionary<'K, 'V>(comparer: IEqualityComparer<'K>) =
    let xs = Dictionary<'K, 'V>(comparer)

    new () =
        ConcurrentDictionary<'K, 'V>(EqualityComparer.Default)
    new (_concurrencyLevel: int, _capacity: int) =
        ConcurrentDictionary<'K, 'V>()
    new (_concurrencyLevel: int, comparer: IEqualityComparer<'K>) =
        ConcurrentDictionary<'K, 'V>(comparer)
    new (_concurrencyLevel: int, _capacity: int, comparer: IEqualityComparer<'K>) =
        ConcurrentDictionary<'K, 'V>(comparer)

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
        match xs.TryGetValue(key) with
        | true, v when Unchecked.equals v comparisonValue -> xs[key] <- value; true
        | _ -> false

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

    interface System.Collections.IEnumerable with
        member _.GetEnumerator(): System.Collections.IEnumerator =
            (xs.GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member _.GetEnumerator(): IEnumerator<KeyValuePair<'K, 'V>> =
            xs.GetEnumerator()

    interface ICollection<KeyValuePair<'K, 'V>> with
        member _.Add(item: KeyValuePair<'K, 'V>) : unit =
            xs.Add(item.Key, item.Value)

        member _.Clear() : unit = xs.Clear()

        member _.Contains(item: KeyValuePair<'K, 'V>) : bool =
            match xs.TryGetValue(item.Key) with
            | true, value when Unchecked.equals value item.Value -> true
            | _ -> false

        member _.CopyTo(array: KeyValuePair<'K, 'V>[], arrayIndex: int) : unit =
            xs |> Seq.iteri (fun i e -> array[arrayIndex + i] <- e)

        member _.Count: int = xs.Count
        member _.IsReadOnly: bool = false

        member _.Remove(item: KeyValuePair<'K, 'V>) : bool =
            match xs.TryGetValue(item.Key) with
            | true, value when Unchecked.equals value item.Value -> xs.Remove(item.Key)
            | _ -> false

    interface IDictionary<'K, 'V> with
        member _.Add(key: 'K, value: 'V) : unit = xs.Add(key, value)
        member _.ContainsKey(key: 'K) : bool = xs.ContainsKey(key)

        member _.Item
            with get (key: 'K): 'V = xs[key]
            and set (key: 'K) (v: 'V): unit = xs[key] <- v

        member _.Keys: ICollection<'K> = xs.Keys

        member _.Remove(key: 'K) : bool = xs.Remove(key)

        member _.TryGetValue(key: 'K, value: byref<'V>) : bool = xs.TryGetValue(key, &value)
        member _.Values: ICollection<'V> = xs.Values

    interface Fable.Core.JS.Map<'K, 'V> with
        member _.size = xs.Count
        member _.clear() = xs.Clear()
        member _.delete(k) = xs.Remove(k)

        member _.entries() =
            xs |> Seq.map (fun p -> p.Key, p.Value)

        member _.get(k) = xs[k]
        member _.has(k) = xs.ContainsKey(k)
        member _.keys() = xs.Keys
        member _.values() = xs.Values

        member this.set(k, v) =
            xs[k] <- v
            this

        member this.forEach(f, ?thisArg) =
            this |> Seq.iter (fun p -> f p.Value p.Key this)
