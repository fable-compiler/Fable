//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation.
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A
// copy of the license can be found in the License.html file at the root of this distribution.
// By using this source code in any fashion, you are agreeing to be bound
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

// Root of the distribution is at: https://github.com/fsharp/fsharp
// Modified Map implementation for FunScript/Fable

module Map

open System.Collections
open System.Collections.Generic
open Fable.Core

// [<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
// [<NoEquality; NoComparison>]
type MapTree<'Key,'Value when 'Key : comparison > =
    | MapEmpty
    | MapOne of 'Key * 'Value
    | MapNode of 'Key * 'Value * MapTree<'Key,'Value> *  MapTree<'Key,'Value> * int
    // REVIEW: performance rumour has it that the data held in MapNode and MapOne should be
    // exactly one cache line. It is currently ~7 and 4 words respectively.

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module MapTree =

    let rec sizeAux acc m =
        match m with
        | MapEmpty -> acc
        | MapOne _ -> acc + 1
        | MapNode(_,_,l,r,_) -> sizeAux (sizeAux (acc+1) l) r

    let size x = sizeAux 0 x

    let empty = MapEmpty

    let height  = function
        | MapEmpty -> 0
        | MapOne _ -> 1
        | MapNode(_,_,_,_,h) -> h

    let isEmpty m =
        match m with
        | MapEmpty -> true
        | _ -> false

    let mk l k v r =
        match l,r with
        | MapEmpty,MapEmpty -> MapOne(k,v)
        | _ ->
            let hl = height l
            let hr = height r
            let m = if hl < hr then hr else hl
            MapNode(k,v,l,r,m+1)

    let rebalance t1 k v t2 =
        let t1h = height t1
        let t2h = height t2
        if  t2h > t1h + 2 then (* right is heavier than left *)
            match t2 with
            | MapNode(t2k,t2v,t2l,t2r,_) ->
                (* one of the nodes must have height > height t1 + 1 *)
                if height t2l > t1h + 1 then  (* balance left: combination *)
                    match t2l with
                    | MapNode(t2lk,t2lv,t2ll,t2lr,_) ->
                        mk (mk t1 k v t2ll) t2lk t2lv (mk t2lr t2k t2v t2r)
                    | _ -> failwith "rebalance"
                else (* rotate left *)
                    mk (mk t1 k v t2l) t2k t2v t2r
            | _ -> failwith "rebalance"
        else
            if  t1h > t2h + 2 then (* left is heavier than right *)
                match t1 with
                | MapNode(t1k,t1v,t1l,t1r,_) ->
                    (* one of the nodes must have height > height t2 + 1 *)
                    if height t1r > t2h + 1 then
                        (* balance right: combination *)
                        match t1r with
                        | MapNode(t1rk,t1rv,t1rl,t1rr,_) ->
                            mk (mk t1l t1k t1v t1rl) t1rk t1rv (mk t1rr k v t2)
                        | _ -> failwith "re  balance"
                    else
                        mk t1l t1k t1v (mk t1r k v t2)
                | _ -> failwith "rebalance"
            else mk t1 k v t2

    let rec add (comparer: IComparer<'Value>) k v m =
        match m with
        | MapEmpty -> MapOne(k,v)
        | MapOne(k2,_) ->
            let c = comparer.Compare(k,k2)
            if c < 0   then MapNode (k,v,MapEmpty,m,2)
            elif c = 0 then MapOne(k,v)
            else            MapNode (k,v,m,MapEmpty,2)
        | MapNode(k2,v2,l,r,h) ->
            let c = comparer.Compare(k,k2)
            if c < 0 then rebalance (add comparer k v l) k2 v2 r
            elif c = 0 then MapNode(k,v,l,r,h)
            else rebalance l k2 v2 (add comparer k v r)

    let rec find (comparer: IComparer<'Value>) k m =
        match m with
        | MapEmpty -> failwith "key not found"
        | MapOne(k2,v2) ->
            let c = comparer.Compare(k,k2)
            if c = 0 then v2
            else failwith "key not found"
        | MapNode(k2,v2,l,r,_) ->
            let c = comparer.Compare(k,k2)
            if c < 0 then find comparer k l
            elif c = 0 then v2
            else find comparer k r

    let rec tryFind (comparer: IComparer<'Value>) k m =
        match m with
        | MapEmpty -> None
        | MapOne(k2,v2) ->
            let c = comparer.Compare(k,k2)
            if c = 0 then Some v2
            else None
        | MapNode(k2,v2,l,r,_) ->
            let c = comparer.Compare(k,k2)
            if c < 0 then tryFind comparer k l
            elif c = 0 then Some v2
            else tryFind comparer k r

    let partition1 (comparer: IComparer<'Value>) f k v (acc1,acc2) =
        if f k v then (add comparer k v acc1,acc2) else (acc1,add comparer k v acc2)

    let rec partitionAux (comparer: IComparer<'Value>) f s acc =
        match s with
        | MapEmpty -> acc
        | MapOne(k,v) -> partition1 comparer f k v acc
        | MapNode(k,v,l,r,_) ->
            let acc = partitionAux comparer f r acc
            let acc = partition1 comparer f k v acc
            partitionAux comparer f l acc

    let partition (comparer: IComparer<'Value>) f s = partitionAux comparer f s (empty,empty)

    let filter1 (comparer: IComparer<'Value>) f k v acc = if f k v then add comparer k v acc else acc

    let rec filterAux (comparer: IComparer<'Value>) f s acc =
        match s with
        | MapEmpty -> acc
        | MapOne(k,v) -> filter1 comparer f k v acc
        | MapNode(k,v,l,r,_) ->
            let acc = filterAux comparer f l acc
            let acc = filter1 comparer f k v acc
            filterAux comparer f r acc

    let filter (comparer: IComparer<'Value>) f s = filterAux comparer f s empty

    let rec spliceOutSuccessor m =
        match m with
        | MapEmpty -> failwith "internal error: Map.spliceOutSuccessor"
        | MapOne(k2,v2) -> k2,v2,MapEmpty
        | MapNode(k2,v2,l,r,_) ->
            match l with
            | MapEmpty -> k2,v2,r
            | _ -> let k3,v3,l' = spliceOutSuccessor l in k3,v3,mk l' k2 v2 r

    let rec remove (comparer: IComparer<'Value>) k m =
        match m with
        | MapEmpty -> empty
        | MapOne(k2,_) ->
            let c = comparer.Compare(k,k2)
            if c = 0 then MapEmpty else m
        | MapNode(k2,v2,l,r,_) ->
            let c = comparer.Compare(k,k2)
            if c < 0 then rebalance (remove comparer k l) k2 v2 r
            elif c = 0 then
                match l,r with
                | MapEmpty,_ -> r
                | _,MapEmpty -> l
                | _ ->
                    let sk,sv,r' = spliceOutSuccessor r
                    mk l sk sv r'
                    else rebalance l k2 v2 (remove comparer k r)

    let rec mem (comparer: IComparer<'Value>) k m =
        match m with
        | MapEmpty -> false
        | MapOne(k2,_) -> (comparer.Compare(k,k2) = 0)
        | MapNode(k2,_,l,r,_) ->
            let c = comparer.Compare(k,k2)
            if c < 0 then mem comparer k l
            else (c = 0 || mem comparer k r)

    let rec iter f m =
        match m with
        | MapEmpty -> ()
        | MapOne(k2,v2) -> f k2 v2
        | MapNode(k2,v2,l,r,_) -> iter f l; f k2 v2; iter f r

    let rec tryPick f m =
        match m with
        | MapEmpty -> None
        | MapOne(k2,v2) -> f k2 v2
        | MapNode(k2,v2,l,r,_) ->
            match tryPick f l with
            | Some _ as res -> res
            | None ->
                match f k2 v2 with
                | Some _ as res -> res
                | None ->
                    tryPick f r

    let rec exists f m =
        match m with
        | MapEmpty -> false
        | MapOne(k2,v2) -> f k2 v2
        | MapNode(k2,v2,l,r,_) -> exists f l || f k2 v2 || exists f r

    let rec forall f m =
        match m with
        | MapEmpty -> true
        | MapOne(k2,v2) -> f k2 v2
        | MapNode(k2,v2,l,r,_) -> forall f l && f k2 v2 && forall f r

    let rec map f m =
        match m with
        | MapEmpty -> empty
        | MapOne(k,v) -> MapOne(k,f v)
        | MapNode(k,v,l,r,h) ->
            let l2 = map f l
            let v2 = f v
            let r2 = map f r
            MapNode(k,v2,l2, r2,h)

    let rec mapi f m =
        match m with
        | MapEmpty -> empty
        | MapOne(k,v) -> MapOne(k,f k v)
        | MapNode(k,v,l,r,h) ->
            let l2 = mapi f l
            let v2 = f k v
            let r2 = mapi f r
            MapNode(k,v2, l2, r2,h)

    let rec foldBack f m x =
        match m with
        | MapEmpty -> x
        | MapOne(k,v) -> f k v x
        | MapNode(k,v,l,r,_) ->
            let x = foldBack f r x
            let x = f k v x
            foldBack f l x

    let rec fold f x m  =
        match m with
        | MapEmpty -> x
        | MapOne(k,v) -> f x k v
        | MapNode(k,v,l,r,_) ->
            let x = fold f x l
            let x = f x k v
            fold f x r

    let rec foldFromTo (comparer: IComparer<'Value>) lo hi f m x =
        match m with
        | MapEmpty -> x
        | MapOne(k,v) ->
            let cLoKey = comparer.Compare(lo,k)
            let cKeyHi = comparer.Compare(k,hi)
            let x = if cLoKey <= 0 && cKeyHi <= 0 then f k v x else x
            x
        | MapNode(k,v,l,r,_) ->
            let cLoKey = comparer.Compare(lo,k)
            let cKeyHi = comparer.Compare(k,hi)
            let x = if cLoKey < 0                then foldFromTo comparer lo hi f l x else x
            let x = if cLoKey <= 0 && cKeyHi <= 0 then f k v x                     else x
            let x = if cKeyHi < 0                then foldFromTo comparer lo hi f r x else x
            x

    let foldSection (comparer: IComparer<'Value>) lo hi f m x =
        if comparer.Compare(lo,hi) = 1 then x else foldFromTo comparer lo hi f m x

    let rec loop m acc =
        match m with
        | MapEmpty -> acc
        | MapOne(k,v) -> (k,v)::acc
        | MapNode(k,v,l,r,_) -> loop l ((k,v)::loop r acc)

    let toList m =
        loop m []

    let ofList comparer l = Seq.fold (fun acc (k,v) -> add comparer k v acc) empty l

    let rec mkFromEnumerator comparer acc (e : IEnumerator<_>) =
        if e.MoveNext() then
            let (x,y) = e.Current
            mkFromEnumerator comparer (add comparer x y acc) e
        else acc

    let ofArray comparer (arr : array<_>) =
        let mutable res = empty
        for i = 0 to arr.Length - 1 do
            let x,y = arr.[i]
            res <- add comparer x y res
        res

    let ofSeq comparer (c : seq<'Key * 'T>) =
    //      match c with
    //      | :? array<'Key * 'T> as xs -> ofArray comparer xs
    //      | :? list<'Key * 'T> as xs -> ofList comparer xs
    //      | _ ->
        use ie = c.GetEnumerator()
        mkFromEnumerator comparer empty ie


    let copyToArray s (arr: _[]) i =
        let mutable j = i
        s |> iter (fun x y -> arr.[j] <- KeyValuePair(x,y); j <- j + 1)


    /// Imperative left-to-right iterators.
    [<NoEquality; NoComparison>]
    type MapIterator<'Key,'Value when 'Key : comparison > =
        { /// invariant: always collapseLHS result
          mutable stack: MapTree<'Key,'Value> list;
          /// true when MoveNext has been called
          mutable started : bool }

    // collapseLHS:
    // a) Always returns either [] or a list starting with MapOne.
    // b) The "fringe" of the set stack is unchanged.
    let rec collapseLHS stack =
        match stack with
        | []                           -> []
        | MapEmpty             :: rest -> collapseLHS rest
        | MapOne _         :: _ -> stack
        | (MapNode(k,v,l,r,_)) :: rest -> collapseLHS (l :: MapOne (k,v) :: r :: rest)

    let mkIterator s = { stack = collapseLHS [s]; started = false }

    let notStarted() = failwith "enumeration not started"

    let alreadyFinished() = failwith "enumeration already finished"

    let current i =
        if i.started then
            match i.stack with
            | MapOne (k,v) :: _ -> KeyValuePair<_,_>(k,v)
            | []            -> alreadyFinished()
            | _             -> failwith "Please report error: Map iterator, unexpected stack for current"
        else
            notStarted()

    let rec moveNext i =
        if i.started then
            match i.stack with
            | MapOne _ :: rest ->
                i.stack <- collapseLHS rest
                not i.stack.IsEmpty
            | [] -> false
            | _ -> failwith "Please report error: Map iterator, unexpected stack for moveNext"
        else
            i.started <- true;  (* The first call to MoveNext "starts" the enumeration. *)
            not i.stack.IsEmpty

    type mkIEnumerator'<'Key,'Value when 'Key: comparison>(s) =
        let mutable i = mkIterator s
        interface IEnumerator<KeyValuePair<'Key,'Value>> with
            member __.Current = current i
        interface IEnumerator with
            member __.Current = box (current i)
            member __.MoveNext() = moveNext i
            member __.Reset() = i <- mkIterator s
        interface System.IDisposable with
            member __.Dispose() = ()

    let mkIEnumerator s = new mkIEnumerator'<_,_>(s) :> _ IEnumerator

    let toSeq s =
        let en = mkIEnumerator s
        en |> Seq.unfold (fun en ->
            if en.MoveNext()
            then Some(en.Current, en)
            else None)

[<CompiledName("FSharpMap"); Replaces("Microsoft.FSharp.Collections.FSharpMap`2")>]
type Map<[<EqualityConditionalOn>]'Key,[<EqualityConditionalOn;ComparisonConditionalOn>]'Value when 'Key : comparison >(comparer: IComparer<'Key>, tree: MapTree<'Key,'Value>) =
    member internal __.Comparer = comparer
    member internal __.Tree = tree

    member __.Add(k,v) : Map<'Key,'Value> =
        new Map<'Key,'Value>(comparer,MapTree.add comparer k v tree)
    member __.IsEmpty = MapTree.isEmpty tree
    member __.Item
        with get(k : 'Key) =
            MapTree.find comparer k tree
    member __.TryGetValue(k: 'Key, defValue: 'Value) =
        match MapTree.tryFind comparer k tree with
        | Some v -> true, v
        | None -> false, defValue
    member __.TryPick(f) = MapTree.tryPick f tree
    member __.Exists(f) = MapTree.exists f tree
    member __.Filter(f): Map<'Key,'Value> =
        new Map<'Key,'Value>(comparer, MapTree.filter comparer f tree)
    member __.ForAll(f) = MapTree.forall f tree
    member __.Fold f acc =
        MapTree.foldBack f tree acc

    member __.FoldSection (lo:'Key) (hi:'Key) f (acc:'z) = MapTree.foldSection comparer lo hi f tree acc

    member __.Iterate f = MapTree.iter f tree

    member __.MapRange f  = new Map<'Key,'b>(comparer,MapTree.map f tree)

    member __.Map f  = new Map<'Key,'b>(comparer,MapTree.mapi f tree)

    member __.Partition(f)  : Map<'Key,'Value> * Map<'Key,'Value> =
        let r1,r2 = MapTree.partition comparer f tree  in
        new Map<'Key,'Value>(comparer,r1), new Map<'Key,'Value>(comparer,r2)

    member __.Count = MapTree.size tree

    member __.ContainsKey(k) =
        MapTree.mem comparer k tree

    member __.Remove(k)  : Map<'Key,'Value> =
        new Map<'Key,'Value>(comparer,MapTree.remove comparer k tree)

    member __.TryFind(k) =
        MapTree.tryFind comparer k tree

    member __.ToList() = MapTree.toList tree

    override this.ToString() =
        "map [" + (this |> Seq.map (fun kv -> sprintf "(%A, %A)" kv.Key kv.Value) |> String.concat "; ") + "]"

    override this.GetHashCode() =
        let combineHash x y = (x <<< 1) + y + 631
        let mutable res = 0
        let e = MapTree.mkIEnumerator this.Tree
        while e.MoveNext() do
            let (KeyValue(x,y)) = e.Current
            res <- combineHash res (hash x)
            res <- combineHash res (Unchecked.hash y)
        abs res

    override this.Equals(that) =
        (this :> System.IComparable).CompareTo(that) = 0

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member __.GetEnumerator() = MapTree.mkIEnumerator tree

    interface System.Collections.IEnumerable with
        member __.GetEnumerator() = (MapTree.mkIEnumerator tree :> System.Collections.IEnumerator)


    interface System.IComparable with
        member m.CompareTo(obj: obj) =
            let m2 = obj :?> Map<'Key,'Value>
            let mutable res = 0
            let mutable finished = false
            use e1 = MapTree.mkIEnumerator m.Tree
            use e2 = MapTree.mkIEnumerator m2.Tree
            while not finished && res = 0 do
                match e1.MoveNext(), e2.MoveNext() with
                | false, false -> finished <- true
                | true, false -> res <- 1
                | false, true -> res <- -1
                | true, true ->
                    let kvp1 = e1.Current
                    let kvp2 = e2.Current
                    let c = comparer.Compare(kvp1.Key, kvp2.Key)
                    res <- if c <> 0 then c else Unchecked.compare kvp1.Value kvp2.Value
            res

let isEmpty (m:Map<_,_>) = m.IsEmpty

let add k v (m:Map<_,_>) = m.Add(k,v)

let find k (m:Map<_,_>) = m.[k]

let tryFind k (m:Map<_,_>) = m.TryFind(k)

let remove k (m:Map<_,_>) = m.Remove(k)

let containsKey k (m:Map<_,_>) = m.ContainsKey(k)

let iterate f (m:Map<_,_>) = m.Iterate(f)

let tryPick f (m:Map<_,_>) = m.TryPick(f)

let pick f (m:Map<_,_>) = match tryPick f m with None -> failwith "key not found" | Some res -> res

let exists f (m:Map<_,_>) = m.Exists(f)

let filter f (m:Map<_,_>) = m.Filter(f)

let partition f (m:Map<_,_>) = m.Partition(f)

let forAll f (m:Map<_,_>) = m.ForAll(f)

let mapRange f (m:Map<_,_>) = m.MapRange(f)

let map f (m:Map<_,_>) = m.Map(f)

let fold<'Key,'T,'State when 'Key : comparison> f (z:'State) (m:Map<'Key,'T>) =
    MapTree.fold f z m.Tree

let foldBack<'Key,'T,'State  when 'Key : comparison> f (m:Map<'Key,'T>) (z:'State) =
    MapTree.foldBack  f m.Tree z

let toSeq (m:Map<'a,'b>) =
    MapTree.toSeq m.Tree

let findKey f (m : Map<_,_>) =
    m.Tree |> MapTree.tryPick (fun k v ->
        if f k v then Some k else None)
    |> function Some k -> k | None -> failwith "Key not found"

let tryFindKey f (m : Map<_,_>) =
    m.Tree |> MapTree.tryPick (fun k v ->
        if f k v then Some k else None)

let ofList (l: ('Key * 'Value) list) ([<Inject>] comparer: IComparer<'Key>) =
    new Map<_,_>(comparer, MapTree.ofList comparer l)

let ofSeq (l: ('Key * 'Value) seq) ([<Inject>] comparer: IComparer<'Key>) =
    new Map<_,_>(comparer, MapTree.ofSeq comparer l)

let ofArray (array: ('Key * 'Value) array) ([<Inject>] comparer: IComparer<'Key>) =
    new Map<_,_>(comparer, MapTree.ofArray comparer array)

let toList (m:Map<_,_>) = m.ToList()

let toArray (m:Map<'Key,'Value>) =
    let res = Array.Helpers.newDynamicArrayImpl m.Count
    MapTree.copyToArray m.Tree res 0
    res

let empty<'Key,'Value  when 'Key : comparison> ([<Inject>] comparer: IComparer<'Key>) =
    new Map<'Key,'Value>(comparer, MapTree.MapEmpty)

/// Fable uses JS Map to represent .NET Dictionary. However when keys are non-primitive,
/// we need to disguise an F# map as a mutable map. Thus, this interface matches JS Map prototype.
/// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
type IMutableMap<'Key,'Value> =
    inherit IEnumerable<KeyValuePair<'Key,'Value>>
    abstract size: int
    abstract clear: unit -> unit
    abstract delete: 'Key -> bool
    abstract entries: unit -> KeyValuePair<'Key,'Value> seq
    abstract get: 'Key -> 'Value
    abstract has: 'Key -> bool
    abstract keys: unit -> 'Key seq
    abstract set: 'Key * 'Value -> IMutableMap<'Key,'Value>
    abstract values: unit -> 'Value seq

let private createMutablePrivate (comparer: IComparer<'Key>) tree' =
    let mutable tree = tree'
    { new IMutableMap<'Key,'Value> with
        member __.size = MapTree.size tree
        member __.clear () =
            tree <- MapEmpty
        member __.delete x =
            if MapTree.mem comparer x tree
            then tree <- MapTree.remove comparer x tree; true
            else false
        member __.entries () =
            MapTree.toSeq tree
        member __.get k =
            MapTree.find comparer k tree
        member __.has x =
            MapTree.mem comparer x tree
        member __.keys () =
            MapTree.toSeq tree |> Seq.map (fun kv -> kv.Key)
        member this.set(k, v) =
            tree <- MapTree.add comparer k v tree
            this
        member __.values () =
            MapTree.toSeq tree |> Seq.map (fun kv -> kv.Value)
    interface IEnumerable<_> with
        member __.GetEnumerator() =
            MapTree.mkIEnumerator tree
    interface IEnumerable with
        member __.GetEnumerator() =
            upcast MapTree.mkIEnumerator tree
    }

/// Emulate JS Set with custom comparer for non-primitive values
let createMutable (source: ('Key*'Value) seq) ([<Inject>] comparer: IComparer<'Key>) =
    MapTree.ofSeq comparer source
    |> createMutablePrivate comparer

let groupBy (projection: 'T -> 'Key) (xs: 'T seq) ([<Inject>] comparer: IComparer<'Key>): ('Key * 'T seq) seq =
    let dict: IMutableMap<_,ResizeArray<'T>> = createMutable Seq.empty comparer

    // Build the groupings
    for v in xs do
        let key = projection v
        if dict.has(key)
        then dict.get(key).Add(v)
        else dict.set(key, ResizeArray [v]) |> ignore

    // Mapping shouldn't be necessary because KeyValuePair compiles
    // as a tuple, but let's do it just in case the implementation changes
    dict |> Seq.map (fun kv -> kv.Key, upcast kv.Value)

let countBy (projection: 'T -> 'Key) (xs: 'T seq) ([<Inject>] comparer: IComparer<'Key>): ('Key * int) seq =
    let dict = createMutable Seq.empty comparer

    for value in xs do
        let key = projection value
        if dict.has(key)
        then dict.set(key, dict.get(key) + 1)
        else dict.set(key, 1)
        |> ignore

    dict |> Seq.map (fun kv -> kv.Key, kv.Value)

let count (m:Map<'Key,'Value>) = m.Count