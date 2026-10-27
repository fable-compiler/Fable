// Fable .NET Dictionary implementation for non-primitive keys.
namespace Fable.Collections

open System.Collections.Generic
open Native

[<Sealed>]
[<CompiledName("Dictionary")>]
type MutableMap<'Key, 'Value when 'Key: equality>
    (pairs: KeyValuePair<'Key, 'Value> seq, comparer: IEqualityComparer<'Key>)
    as this =

    // Compiles to JS Map of key hashes pointing to dynamic arrays of KeyValuePair<'Key, 'Value>.
    let hashMap = Dictionary<int, ResizeArray<KeyValuePair<'Key, 'Value>>>()
    let mutable count = 0

    do
        for pair in pairs do
            this.Add(pair.Key, pair.Value)

    // new () = MutableMap (Seq.empty, EqualityComparer.Default)
    // new (comparer) = MutableMap (Seq.empty, comparer)

    // An erased option of a reference type, so a hit allocates nothing
    member private this.TryFindBucket(h: int) : ResizeArray<KeyValuePair<'Key, 'Value>> option =
        if hashMap.ContainsKey h then
            Some hashMap.[h]
        else
            None

    member private this.FindIndex(k, bucket: ResizeArray<KeyValuePair<'Key, 'Value>>) =
        let mutable i = 0
        let mutable found = -1

        while found < 0 && i < bucket.Count do
            if comparer.Equals(k, bucket.[i].Key) then
                found <- i

            i <- i + 1

        found

    member this.TryFind(k: 'Key) =
        match this.TryFindBucket(comparer.GetHashCode(k)) with
        | Some bucket ->
            let i = this.FindIndex(k, bucket)

            if i > -1 then
                Some bucket.[i]
            else
                None
        | None -> None

    member this.Comparer = comparer

    member this.Clear() =
        hashMap.Clear()
        count <- 0

    member this.Count = count

    member this.Item
        with get (k: 'Key) =
            match this.TryFind(k) with
            | Some pair -> pair.Value
            | _ -> raise (KeyNotFoundException("The item was not found in collection"))
        and set (k: 'Key) (v: 'Value) =
            let h = comparer.GetHashCode(k)

            match this.TryFindBucket h with
            | Some bucket ->
                let i = this.FindIndex(k, bucket)

                if i > -1 then
                    bucket.[i] <- KeyValuePair(k, v)
                else
                    bucket.Add(KeyValuePair(k, v))
                    count <- count + 1
            | None ->
                hashMap.[h] <- ResizeArray([| KeyValuePair(k, v) |])
                count <- count + 1

    member this.Add(k: 'Key, v: 'Value) =
        let h = comparer.GetHashCode(k)

        match this.TryFindBucket h with
        | Some bucket ->
            if this.FindIndex(k, bucket) > -1 then
                let msg =
                    System.String.Format("An item with the same key has already been added. Key: {0}", k)

                raise (System.ArgumentException(msg))

            bucket.Add(KeyValuePair(k, v))
            count <- count + 1
        | None ->
            hashMap.[h] <- ResizeArray([| KeyValuePair(k, v) |])
            count <- count + 1

    member this.ContainsKey(k: 'Key) =
        match this.TryFindBucket(comparer.GetHashCode(k)) with
        | Some bucket -> this.FindIndex(k, bucket) > -1
        | None -> false

    member this.Remove(k: 'Key) =
        let h = comparer.GetHashCode(k)

        match this.TryFindBucket h with
        | Some bucket ->
            let i = this.FindIndex(k, bucket)

            if i > -1 then
                bucket.RemoveAt(i)
                count <- count - 1

                if bucket.Count = 0 then
                    hashMap.Remove(h) |> ignore

                true
            else
                false
        | None -> false

    interface Fable.Core.Symbol_wellknown with
        member _.``Symbol.toStringTag`` = "Dictionary"

    // Native JS Map (used for primitive keys) doesn't work with `JSON.stringify` but
    // let's add `toJSON` for consistency with the types within fable-library.
    interface Fable.Core.IJsonSerializable with
        member this.toJSON() = Helpers.arrayFrom (this)


    interface System.Collections.IEnumerable with
        member this.GetEnumerator() : System.Collections.IEnumerator =
            ((this :> IEnumerable<KeyValuePair<'Key, 'Value>>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member this.GetEnumerator() : IEnumerator<KeyValuePair<'Key, 'Value>> =
            let elems = Seq.concat hashMap.Values
            elems.GetEnumerator()

    interface ICollection<KeyValuePair<'Key, 'Value>> with
        member this.Add(item: KeyValuePair<'Key, 'Value>) : unit = this.Add(item.Key, item.Value)

        member this.Clear() : unit = this.Clear()

        member this.Contains(item: KeyValuePair<'Key, 'Value>) : bool =
            match this.TryFind item.Key with
            | Some pair when Unchecked.equals pair.Value item.Value -> true
            | _ -> false

        member this.CopyTo(array: KeyValuePair<'Key, 'Value>[], arrayIndex: int) : unit =
            this |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)

        member this.Count: int = this.Count
        member this.IsReadOnly: bool = false

        member this.Remove(item: KeyValuePair<'Key, 'Value>) : bool =
            match this.TryFind item.Key with
            | Some pair when Unchecked.equals pair.Value item.Value -> this.Remove(item.Key)
            | _ -> false

    interface IDictionary<'Key, 'Value> with
        member this.Add(key: 'Key, value: 'Value) : unit = this.Add(key, value)
        member this.ContainsKey(key: 'Key) : bool = this.ContainsKey(key)

        member this.Item
            with get (key: 'Key): 'Value = this.[key]
            and set (key: 'Key) (v: 'Value): unit = this.[key] <- v

        member this.Keys: ICollection<'Key> =
            [| for pair in this -> pair.Key |] :> ICollection<'Key>

        member this.Remove(key: 'Key) : bool = this.Remove(key)

        member this.TryGetValue(key: 'Key, value: byref<'Value>) : bool =
            match this.TryFind key with
            | Some pair ->
                value <- pair.Value
                true
            | _ -> false

        member this.Values: ICollection<'Value> =
            [| for pair in this -> pair.Value |] :> ICollection<'Value>

    interface Fable.Core.JS.Map<'Key, 'Value> with
        member this.size = this.Count
        member this.clear() = this.Clear()
        member this.delete(k) = this.Remove(k)

        member this.entries() =
            this |> Seq.map (fun p -> p.Key, p.Value)

        member this.get(k) = this.[k]
        member this.has(k) = this.ContainsKey(k)
        member this.keys() = this |> Seq.map (fun p -> p.Key)

        member this.set(k, v) =
            this.[k] <- v
            this :> Fable.Core.JS.Map<'Key, 'Value>

        member this.values() = this |> Seq.map (fun p -> p.Value)

        member this.forEach(f, ?thisArg) =
            this |> Seq.iter (fun p -> f p.Value p.Key this)
