// Fable .NET Dictionary implementation for non-primitive keys.
namespace Fable.Collections

open System.Collections.Generic

/// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
type IMutableMap<'Key, 'Value> =
    inherit IEnumerable<KeyValuePair<'Key, 'Value>>
    abstract size: int
    abstract clear: unit -> unit
    abstract delete: 'Key -> bool
    abstract entries: unit -> KeyValuePair<'Key,'Value> seq
    abstract get: 'Key -> 'Value
    abstract has: 'Key -> bool
    abstract keys: unit -> 'Key seq
    abstract set: 'Key * 'Value -> IMutableMap<'Key,'Value>
    abstract values: unit -> 'Value seq

[<Sealed>]
type MutableMap<'Key, 'Value when 'Key: equality>(pairs: KeyValuePair<'Key, 'Value> seq, comparer: IEqualityComparer<'Key>) as this =

    // Compiles to JS Map of key hashes pointing to dynamic arrays of KeyValuePair<'Key, 'Value>.
    let hashMap = Dictionary<int, ResizeArray<KeyValuePair<'Key, 'Value>>>()
    do for pair in pairs do this.Add(pair.Key, pair.Value)

    // new () = MutableMap (Seq.empty, EqualityComparer.Default)
    // new (comparer) = MutableMap (Seq.empty, comparer)

    member private this.TryFindIndex(k) =
        let h = comparer.GetHashCode(k)
        match hashMap.TryGetValue h with
        | true, pairs ->
            true, h, pairs.FindIndex (fun pair -> comparer.Equals(k, pair.Key))
        | false, _ ->
            false, h, -1

    member this.TryFind(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> Some hashMap.[h].[i]
        | _, _, _ -> None

    member this.Comparer =
        comparer

    member this.Clear() =
        hashMap.Clear()

    member this.Count =
        hashMap.Values |> Seq.sumBy (fun pairs -> pairs.Count)

    member this.Item
        with get (k: 'Key) =
            match this.TryFind(k) with
            | Some pair -> pair.Value
            | _ -> raise (KeyNotFoundException("The item was not found in collection"))
        and set (k: 'Key) (v: 'Value) =
            match this.TryFindIndex(k) with
            | true, h, i when i > -1 ->
                hashMap.[h].[i] <- KeyValuePair(k, v) // replace
            | true, h, _ ->
                hashMap.[h].Add(KeyValuePair(k, v)) |> ignore // append
            | false, h, _ ->
                hashMap.[h] <- ResizeArray([| KeyValuePair(k, v) |]) // add new

    member this.Add(k, v) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 ->
            let msg = sprintf "An item with the same key has already been added. Key: %A" k
            raise (System.ArgumentException(msg))
        | true, h, _ ->
            hashMap.[h].Add(KeyValuePair(k, v)) |> ignore // append
        | false, h, _ ->
            hashMap.[h] <- ResizeArray([| KeyValuePair(k, v) |]) // add new

    member this.ContainsKey(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> true
        | _, _, _ -> false

    member this.Remove(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 ->
            hashMap.[h].RemoveAt(i)
            true
        | _, _, _ ->
            false

    interface System.Collections.IEnumerable with
        member this.GetEnumerator(): System.Collections.IEnumerator =
            ((this :> IEnumerable<KeyValuePair<'Key, 'Value>>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member this.GetEnumerator(): IEnumerator<KeyValuePair<'Key, 'Value>> =
            let elems = seq {
                for pairs in hashMap.Values do
                    for pair in pairs do
                        yield pair }
            elems.GetEnumerator()

    interface ICollection<KeyValuePair<'Key, 'Value>> with
        member this.Add(item: KeyValuePair<'Key, 'Value>): unit =
            this.Add(item.Key, item.Value)
        member this.Clear(): unit =
            this.Clear()
        member this.Contains(item: KeyValuePair<'Key, 'Value>): bool =
            match this.TryFind item.Key with
            | Some p when Unchecked.equals p.Value item.Value -> true
            | _ -> false
        member this.CopyTo(array: KeyValuePair<'Key, 'Value> [], arrayIndex: int): unit =
            this |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)
        member this.Count: int =
            this.Count
        member this.IsReadOnly: bool =
            false
        member this.Remove(item: KeyValuePair<'Key, 'Value>): bool =
            match this.TryFind item.Key with
            | Some pair ->
                if Unchecked.equals pair.Value item.Value then
                    this.Remove(item.Key) |> ignore
                true
            | _ -> false

#if !FABLE_COMPILER
    interface IDictionary<'Key, 'Value> with
        member this.Add(key: 'Key, value: 'Value): unit =
            this.Add(key, value)
        member this.ContainsKey(key: 'Key): bool =
            this.ContainsKey(key)
        member this.Item
            with get (key: 'Key): 'Value =
                this.[key]
            and set (key: 'Key) (v: 'Value): unit =
                this.[key] <- v
        member this.Keys: ICollection<'Key> =
            [| for pair in this -> pair.Key |] :> ICollection<'Key>
        member this.Remove(key: 'Key): bool =
            this.Remove(key)
        member this.TryGetValue(key: 'Key, value: byref<'Value>): bool =
            match this.TryFind key with
            | Some pair -> value <- pair.Value; true
            | _ -> false
        member this.Values: ICollection<'Value> =
            [| for pair in this -> pair.Value |] :> ICollection<'Value>
#endif

    interface IMutableMap<'Key, 'Value> with
        member this.size = this.Count
        member this.clear() = this.Clear()
        member this.delete(k) = this.Remove(k)
        member this.entries() = this |> Seq.map id
        member this.get(k) = this.[k]
        member this.has(k) = this.ContainsKey(k)
        member this.keys() = this |> Seq.map (fun pair -> pair.Key)
        member this.set(k, v) = this.[k] <- v; this :> IMutableMap<'Key, 'Value>
        member this.values() = this |> Seq.map (fun pair -> pair.Value)
