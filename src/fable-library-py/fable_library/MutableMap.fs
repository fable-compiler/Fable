// Fable .NET Dictionary implementation for non-primitive keys.
// Python-specific version without JS.Map interface.
namespace Fable.Collections

open System.Collections.Generic
open Native

[<Sealed>]
[<CompiledName("Dictionary")>]
type MutableMap<'Key, 'Value when 'Key: equality>
    (pairs: KeyValuePair<'Key, 'Value> seq, comparer: IEqualityComparer<'Key>)
    as this =

    // Compiles to Python dict of key hashes pointing to dynamic arrays of KeyValuePair<'Key, 'Value>.
    let hashMap = Dictionary<int, ResizeArray<KeyValuePair<'Key, 'Value>>>()

    do
        for pair in pairs do
            this.Add(pair.Key, pair.Value)

    member private this.TryFindIndex(k) =
        let h = comparer.GetHashCode(k)

        match hashMap.TryGetValue h with
        | true, pairs -> true, h, pairs.FindIndex(fun pair -> comparer.Equals(k, pair.Key))
        | false, _ -> false, h, -1

    member this.TryFind(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> Some hashMap.[h].[i]
        | _, _, _ -> None

    member this.Comparer = comparer

    member this.Clear() = hashMap.Clear()

    member this.Count =
        let mutable count = 0

        for pairs in hashMap.Values do
            count <- count + pairs.Count

        count

    member this.Item
        with get (k: 'Key) =
            match this.TryFind(k) with
            | Some pair -> pair.Value
            | _ -> raise (KeyNotFoundException("The item was not found in collection"))
        and set (k: 'Key) (v: 'Value) =
            match this.TryFindIndex(k) with
            | true, h, i when i > -1 -> hashMap.[h].[i] <- KeyValuePair(k, v) // replace
            | true, h, _ -> hashMap.[h].Add(KeyValuePair(k, v)) |> ignore // append
            | false, h, _ -> hashMap.[h] <- ResizeArray([| KeyValuePair(k, v) |])

    member this.Add(k, v) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 ->
            let msg =
                System.String.Format("An item with the same key has already been added. Key: {0}", k)

            raise (System.ArgumentException(msg))
        | true, h, _ -> hashMap.[h].Add(KeyValuePair(k, v)) |> ignore // append
        | false, h, _ -> hashMap.[h] <- ResizeArray([| KeyValuePair(k, v) |]) // add new

    member this.ContainsKey(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> true
        | _, _, _ -> false

    member this.Remove(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 ->
            hashMap.[h].RemoveAt(i)
            true
        | _, _, _ -> false

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

    // Python MutableMapping interface for dict-like behavior
    interface Fable.Core.Py.MutableMapping<'Key, 'Value> with
        member this.``__getitem__``(k) = this.[k]
        member this.``__setitem__``(k, v) = this.[k] <- v
        member this.``__delitem__``(k) = this.Remove(k) |> ignore
        // Return iterator over keys
        member this.``__iter__``() =
            (hashMap.Values |> Seq.concat |> Seq.map (fun p -> p.Key)).GetEnumerator()

        member this.``__len__``() = this.Count
        member this.``__contains__``(k) = this.ContainsKey(k)

        member this.keys() =
            hashMap.Values |> Seq.concat |> Seq.map (fun p -> p.Key)

        member this.values() =
            hashMap.Values |> Seq.concat |> Seq.map (fun p -> p.Value)

        member this.items() =
            hashMap.Values |> Seq.concat |> Seq.map (fun p -> p.Key, p.Value)

        member this.get(k, ?defaultValue) =
            match this.TryFind(k) with
            | Some pair -> pair.Value
            | None ->
                match defaultValue with
                | Some v -> v
                | None -> Unchecked.defaultof<'Value>

        member this.clear() = this.Clear()

        member this.pop(k, ?defaultValue) =
            match this.TryFind(k) with
            | Some pair ->
                this.Remove(k) |> ignore
                pair.Value
            | None ->
                match defaultValue with
                | Some v -> v
                | None -> raise (KeyNotFoundException("Key not found"))

        member this.popitem() =
            let first = this |> Seq.tryHead

            match first with
            | Some pair ->
                this.Remove(pair.Key) |> ignore
                (pair.Key, pair.Value)
            | None -> raise (KeyNotFoundException("Dictionary is empty"))
