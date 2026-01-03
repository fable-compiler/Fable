// Fable .NET HashSet implementation for non-primitive keys.
// Python-specific version with Py.MutableSet interface.
namespace Fable.Collections

open System.Collections.Generic
open Fable.Core
open Native

[<Sealed>]
[<CompiledName("HashSet")>]
type MutableSet<'T when 'T: equality>(items: 'T seq, comparer: IEqualityComparer<'T>) as this =

    // Compiles to Python dict of key hashes pointing to dynamic arrays of 'T.
    let hashMap = Dictionary<int, ResizeArray<'T>>()

    do
        for item in items do
            this.Add(item) |> ignore

    member private this.TryFindIndex(k) =
        let h = comparer.GetHashCode(k)

        match hashMap.TryGetValue h with
        | true, values -> true, h, values.FindIndex(fun v -> comparer.Equals(k, v))
        | false, _ -> false, h, -1

    member private this.TryFind(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> Some hashMap.[h].[i]
        | _, _, _ -> None

    member this.Comparer = comparer

    member this.Clear() = hashMap.Clear()

    member this.Count =
        let mutable count = 0

        for items in hashMap.Values do
            count <- count + items.Count

        count

    member this.Add(k) =
        match this.TryFindIndex(k) with
        | true, h, i when i > -1 -> false
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
        | _, _, _ -> false

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() : System.Collections.IEnumerator =
            ((this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IEnumerable<'T> with
        member this.GetEnumerator() : IEnumerator<'T> =
            let elems = Seq.concat hashMap.Values
            elems.GetEnumerator()

    interface ICollection<'T> with
        member this.Add(item: 'T) : unit = this.Add item |> ignore
        member this.Clear() : unit = this.Clear()
        member this.Contains(item: 'T) : bool = this.Contains item

        member this.CopyTo(array: 'T[], arrayIndex: int) : unit =
            this |> Seq.iteri (fun i e -> array.[arrayIndex + i] <- e)

        member this.Count: int = this.Count
        member this.IsReadOnly: bool = false
        member this.Remove(item: 'T) : bool = this.Remove item

#if !FABLE_COMPILER
    interface ISet<'T> with
        member this.Add(item: 'T) : bool = this.Add item

        member this.ExceptWith(other: IEnumerable<'T>) : unit =
            for x in other do
                this.Remove x |> ignore

        member this.IntersectWith(other: IEnumerable<'T>) : unit = failwith "Not Implemented"

        member this.IsProperSubsetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"

        member this.IsProperSupersetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"

        member this.IsSubsetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"

        member this.IsSupersetOf(other: IEnumerable<'T>) : bool = failwith "Not Implemented"

        member this.Overlaps(other: IEnumerable<'T>) : bool = failwith "Not Implemented"

        member this.SetEquals(other: IEnumerable<'T>) : bool = failwith "Not Implemented"

        member this.SymmetricExceptWith(other: IEnumerable<'T>) : unit = failwith "Not Implemented"

        member this.UnionWith(other: IEnumerable<'T>) : unit =
            for x in other do
                this.Add x |> ignore
#endif

    // Python MutableSet protocol - explicit interface implementation (methods become attached)
    interface Fable.Core.Py.Set.IMutableSet<'T> with
        member this.Contains(item) = this.Contains(item)
        member this.Count = this.Count
        member this.Add(item) = this.Add(item) |> ignore
        member this.Remove(item) = this.Remove(item)
        member this.Clear() = this.Clear()
