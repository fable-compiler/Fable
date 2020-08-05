// Fable .NET HashSet implementation for non-primitive keys.
namespace Fable.Collections

open System.Collections.Generic

/// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
type IMutableSet<'T> =
    inherit IEnumerable<'T>
    abstract size: int
    abstract add: 'T -> IMutableSet<'T>
    /// Convenience method (not in JS Set prototype) to check if the element has actually been added
    abstract add_: 'T -> bool
    abstract clear: unit -> unit
    abstract delete: 'T -> bool
    abstract has: 'T -> bool
    abstract keys: unit -> 'T seq
    abstract values: unit -> 'T seq
    abstract entries: unit -> ('T * 'T) seq

[<Sealed>]
type MutableSet<'T when 'T: equality>(items: 'T seq, comparer: IEqualityComparer<'T>) as this =

    // Compiles to JS Map of key hashes pointing to dynamic arrays of 'T.
    let hashMap = Dictionary<int, ResizeArray<'T>>()
    do for item in items do this.Add(item) |> ignore

    // new () = MutableSet (Seq.empty, EqualityComparer.Default)
    // new (comparer) = MutableSet (Seq.empty, comparer)

    member private this.TryFindIndex(k) =
        let h = comparer.GetHashCode(k)
        match hashMap.TryGetValue h with
        | true, values ->
            true, h, values.FindIndex (fun v -> comparer.Equals(k, v))
        | false, _ ->
            false, h, -1

    member private this.TryFind(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> Some hashMap.[h].[i]
        | _, _, _ -> None

    member this.Comparer =
        comparer

    member this.Clear() =
        hashMap.Clear()

    member this.Count =
        hashMap.Values |> Seq.sumBy (fun pairs -> pairs.Count)

    member this.Add(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 ->
            false
        | true, h, _ ->
            hashMap.[h].Add(k) |> ignore
            true
        | false, h, _ ->
            hashMap.[h] <- ResizeArray([| k |])
            true

    member this.Contains(k) =
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
            ((this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<'T> with
        member this.GetEnumerator(): IEnumerator<'T> =
            // let elems = seq {
            //     for values in hashMap.Values do
            //         for value in values do
            //             yield value }
            let elems = Seq.concat hashMap.Values
            elems.GetEnumerator()

    interface ICollection<'T> with
        member this.Add(item: 'T): unit =
            this.Add item |> ignore
        member this.Clear(): unit =
            this.Clear()
        member this.Contains(item: 'T): bool =
            this.Contains item
        member this.CopyTo(array: 'T [], arrayIndex: int): unit =
            this |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)
        member this.Count: int =
            this.Count
        member this.IsReadOnly: bool =
            false
        member this.Remove(item: 'T): bool =
            this.Remove item

#if !FABLE_COMPILER
    interface ISet<'T> with
        member this.Add(item: 'T): bool =
            this.Add item
        member this.ExceptWith(other: IEnumerable<'T>): unit =
            for x in other do
                this.Remove x |> ignore
        member this.IntersectWith(other: IEnumerable<'T>): unit =
            failwith "Not Implemented"
        member this.IsProperSubsetOf(other: IEnumerable<'T>): bool =
            failwith "Not Implemented"
        member this.IsProperSupersetOf(other: IEnumerable<'T>): bool =
            failwith "Not Implemented"
        member this.IsSubsetOf(other: IEnumerable<'T>): bool =
            failwith "Not Implemented"
        member this.IsSupersetOf(other: IEnumerable<'T>): bool =
            failwith "Not Implemented"
        member this.Overlaps(other: IEnumerable<'T>): bool =
            failwith "Not Implemented"
        member this.SetEquals(other: IEnumerable<'T>): bool =
            failwith "Not Implemented"
        member this.SymmetricExceptWith(other: IEnumerable<'T>): unit =
            failwith "Not Implemented"
        member this.UnionWith(other: IEnumerable<'T>): unit =
            for x in other do
                this.Add x |> ignore
#endif

    interface IMutableSet<'T> with
        member this.size = this.Count
        member this.add(k) = this.Add(k) |> ignore; this :> IMutableSet<'T>
        member this.add_(k) = this.Add(k)
        member this.clear() = this.Clear()
        member this.delete(k) = this.Remove(k)
        member this.has(k) = this.Contains(k)
        member this.keys() = this |> Seq.map id
        member this.values() = this |> Seq.map id
        member this.entries() = this |> Seq.map (fun v -> (v, v))
