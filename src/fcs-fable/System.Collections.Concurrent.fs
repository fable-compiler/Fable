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
[<AllowNullLiteral>]
type ConcurrentDictionary<'K, 'V>(comparer: IEqualityComparer<'K>) =
    inherit Dictionary<'K, 'V>(comparer)

    new () =
        ConcurrentDictionary<'K, 'V>(EqualityComparer.Default)
    new (_concurrencyLevel: int, _capacity: int) =
        ConcurrentDictionary<'K, 'V>()
    new (_concurrencyLevel: int, comparer: IEqualityComparer<'K>) =
        ConcurrentDictionary<'K, 'V>(comparer)
    new (_concurrencyLevel: int, _capacity: int, comparer: IEqualityComparer<'K>) =
        ConcurrentDictionary<'K, 'V>(comparer)

    member x.TryAdd (key: 'K, value: 'V): bool =
        if x.ContainsKey(key)
        then false
        else x.Add(key, value); true

    member x.TryRemove (key: 'K): bool * 'V =
        match x.TryGetValue(key) with
        | true, v -> (x.Remove(key), v)
        | _ as res -> res

    member x.GetOrAdd (key: 'K, value: 'V): 'V =
        match x.TryGetValue(key) with
        | true, v -> v
        | _ -> let v = value in x.Add(key, v); v

    member x.GetOrAdd (key: 'K, valueFactory: System.Func<'K, 'V>): 'V =
        match x.TryGetValue(key) with
        | true, v -> v
        | _ -> let v = valueFactory.Invoke(key) in x.Add(key, v); v

    // member x.GetOrAdd<'Arg> (key: 'K, valueFactory: 'K * 'Arg -> 'V, arg: 'Arg): 'V =
    //     match x.TryGetValue(key) with
    //     | true, v -> v
    //     | _ -> let v = valueFactory(key, arg) in x.Add(key, v); v

    member x.TryUpdate (key: 'K, value: 'V, comparisonValue: 'V): bool =
        match x.TryGetValue(key) with
        | true, v when Unchecked.equals v comparisonValue -> x[key] <- value; true
        | _ -> false

    member x.AddOrUpdate (key: 'K, value: 'V, updateFactory: System.Func<'K, 'V, 'V>): 'V =
        match x.TryGetValue(key) with
        | true, v -> let v = updateFactory.Invoke(key, v) in x[key] <- v; v
        | _ -> let v = value in x.Add(key, v); v

    // member x.AddOrUpdate (key: 'K, valueFactory: 'K -> 'V, updateFactory: 'K * 'V -> 'V): 'V =
    //     match x.TryGetValue(key) with
    //     | true, v -> let v = updateFactory(key, v) in x[key] <- v; v
    //     | _ -> let v = valueFactory(key) in x.Add(key, v); v

    // member x.AddOrUpdate (key: 'K, valueFactory: 'K * 'Arg -> 'V, updateFactory: 'K * 'Arg * 'V -> 'V, arg: 'Arg): 'V =
    //     match x.TryGetValue(key) with
    //     | true, v -> let v = updateFactory(key, arg, v) in x[key] <- v; v
    //     | _ -> let v = valueFactory(key, arg) in x.Add(key, v); v
