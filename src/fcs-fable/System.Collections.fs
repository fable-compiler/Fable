//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections

module Generic =

    type Queue<'T> =
        inherit ResizeArray<'T>

        new () = Queue<'T>()

        member x.Enqueue (item: 'T) =
            x.Add(item)

        member x.Dequeue () =
            let item = x.Item(0)
            x.RemoveAt(0)
            item

module Immutable =

    type ImmutableArray<'T> =
        static member CreateBuilder() = ResizeArray<'T>()

module Concurrent =
    open System.Collections.Generic

    /// not actually thread safe, just an extension of Dictionary
    [<AllowNullLiteral>]
    type ConcurrentDictionary<'Key, 'Value>(comparer: IEqualityComparer<'Key>) =
        inherit Dictionary<'Key, 'Value>(comparer)

        new () =
            ConcurrentDictionary(EqualityComparer.Default)
        new (_concurrencyLevel: int, _capacity: int) =
            ConcurrentDictionary()
        new (_concurrencyLevel: int, comparer: IEqualityComparer<'Key>) =
            ConcurrentDictionary(comparer)
        new (_concurrencyLevel: int, _capacity: int, comparer: IEqualityComparer<'Key>) =
            ConcurrentDictionary(comparer)

        member x.TryAdd (key: 'Key, value: 'Value): bool =
            if x.ContainsKey(key)
            then false
            else x.Add(key, value); true

        member x.TryRemove (key: 'Key): bool * 'Value =
            match x.TryGetValue(key) with
            | true, v -> (x.Remove(key), v)
            | _ as res -> res

        member x.GetOrAdd (key: 'Key, valueFactory: 'Key -> 'Value): 'Value =
            match x.TryGetValue(key) with
            | true, v -> v
            | _ -> let v = valueFactory(key) in x.Add(key, v); v

        // member x.GetOrAdd (key: 'Key, value: 'Value): 'Value =
        //     match x.TryGetValue(key) with
        //     | true, v -> v
        //     | _ -> let v = value in x.Add(key, v); v

        // member x.GetOrAdd<'Arg> (key: 'Key, valueFactory: 'Key * 'Arg -> 'Value, arg: 'Arg): 'Value =
        //     match x.TryGetValue(key) with
        //     | true, v -> v
        //     | _ -> let v = valueFactory(key, arg) in x.Add(key, v); v

        // member x.TryUpdate (key: 'Key, value: 'Value, comparisonValue: 'Value): bool =
        //     match x.TryGetValue(key) with
        //     | true, v when v = comparisonValue -> x.[key] <- value; true
        //     | _ -> false

        // member x.AddOrUpdate (key: 'Key, value: 'Value, updateFactory: 'Key * 'Value -> 'Value): 'Value =
        //     match x.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, v) in x.[key] <- v; v
        //     | _ -> let v = value in x.Add(key, v); v

        // member x.AddOrUpdate (key: 'Key, valueFactory: 'Key -> 'Value, updateFactory: 'Key * 'Value -> 'Value): 'Value =
        //     match x.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, v) in x.[key] <- v; v
        //     | _ -> let v = valueFactory(key) in x.Add(key, v); v

        // member x.AddOrUpdate (key: 'Key, valueFactory: 'Key * 'Arg -> 'Value, updateFactory: 'Key * 'Arg * 'Value -> 'Value, arg: 'Arg): 'Value =
        //     match x.TryGetValue(key) with
        //     | true, v -> let v = updateFactory(key, arg, v) in x.[key] <- v; v
        //     | _ -> let v = valueFactory(key, arg) in x.Add(key, v); v
