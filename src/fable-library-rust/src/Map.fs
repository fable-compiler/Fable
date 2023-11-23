// Adapted from https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/map.fs
// Copyright (c) Microsoft Corporation.  All Rights Reserved.
// See https://github.com/dotnet/fsharp/blob/main/License.txt

module Map_

open Global_

// A functional language implementation using binary trees

[<NoEquality; NoComparison>]
type MapTree<'K, 'V> =
    {
        Height: int
        Key: 'K
        Value: 'V
        Left: Map<'K, 'V>
        Right: Map<'K, 'V>
    }

and [<Struct; CompiledName("Map")>] Map<'K, 'V> =
    { root: Option<MapTree<'K, 'V>> }

let inline private getRoot m = m.root

let private mkMap root = { root = root }

let empty: Map<'K, 'V> = { root = None }

let isEmpty (m: Map<'K, 'V>) = (getRoot m).IsNone

let mkMapTreeLeaf (k: 'K, v: 'V) =
    Some
        {
            Key = k
            Value = v
            Left = empty
            Right = empty
            Height = 1
        }
    |> mkMap

let mkMapTreeNode
    (
        k: 'K,
        v: 'V,
        left: Map<'K, 'V>,
        right: Map<'K, 'V>,
        h: int
    )
    =
    Some
        {
            Key = k
            Value = v
            Left = left
            Right = right
            Height = h
        }
    |> mkMap

let singleton (k: 'K, v: 'V) = mkMapTreeLeaf (k, v)

let rec sizeAux acc (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            acc + 1
        else
            sizeAux (sizeAux (acc + 1) t.Left) t.Right

let count x = sizeAux 0 x

let inline height (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> 0
    | Some t -> t.Height

[<Literal>]
let tolerance = 2

let mk l k v r : Map<'K, 'V> =
    let hl = height l
    let hr = height r

    let m =
        if hl < hr then
            hr
        else
            hl

    if m = 0 then // m=0 ~ isEmpty l && isEmpty r
        mkMapTreeLeaf (k, v)
    else
        mkMapTreeNode (k, v, l, r, m + 1) // new map is higher by 1 than the highest

let rebalance t1 (k: 'K) (v: 'V) t2 : Map<'K, 'V> =
    let t1h = height t1
    let t2h = height t2

    if t2h > t1h + tolerance then // right is heavier than left
        let t2' = (getRoot t2).Value
        // one of the nodes must have height > height t1 + 1
        if height t2'.Left > t1h + 1 then // balance left: combination
            let t2l = (getRoot t2'.Left).Value

            mk
                (mk t1 k v t2l.Left)
                t2l.Key
                t2l.Value
                (mk t2l.Right t2'.Key t2'.Value t2'.Right)
        else // rotate left
            mk (mk t1 k v t2'.Left) t2'.Key t2'.Value t2'.Right
    else if t1h > t2h + tolerance then // left is heavier than right
        let t1' = (getRoot t1).Value
        // one of the nodes must have height > height t2 + 1
        if height t1'.Right > t2h + 1 then
            // balance right: combination
            let t1r = (getRoot t1'.Right).Value

            mk
                (mk t1'.Left t1'.Key t1'.Value t1r.Left)
                t1r.Key
                t1r.Value
                (mk t1r.Right k v t2)
        else
            mk t1'.Left t1'.Key t1'.Value (mk t1'.Right k v t2)
    else
        mk t1 k v t2

let rec add k (v: 'V) (m: Map<'K, 'V>) : Map<'K, 'V> =
    match getRoot m with
    | None -> mkMapTreeLeaf (k, v)
    | Some t ->
        let c = compare k t.Key

        if t.Height = 1 then
            if c < 0 then
                mkMapTreeNode (k, v, empty, m, 2)
            elif c = 0 then
                mkMapTreeLeaf (k, v)
            else
                mkMapTreeNode (k, v, m, empty, 2)
        else if c < 0 then
            rebalance (add k v t.Left) t.Key t.Value t.Right
        elif c = 0 then
            mkMapTreeNode (k, v, t.Left, t.Right, t.Height)
        else
            rebalance t.Left t.Key t.Value (add k v t.Right)

let rec tryGetValue k (v: byref<'V>) (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> false
    | Some t ->
        let c = compare k t.Key

        if c = 0 then
            v <- t.Value
            true
        else if t.Height = 1 then
            false
        else
            tryGetValue
                k
                &v
                (if c < 0 then
                     t.Left
                 else
                     t.Right)

// [<MethodImpl(MethodImplOptions.NoInlining)>]
let throwKeyNotFound () = failwith SR.keyNotFound

// [<MethodImpl(MethodImplOptions.NoInlining)>]
let find k (m: Map<'K, 'V>) =
    let mutable v = Unchecked.defaultof<'V>

    if tryGetValue k &v m then
        v
    else
        throwKeyNotFound ()

let tryFind k (m: Map<'K, 'V>) =
    let mutable v = Unchecked.defaultof<'V>

    if tryGetValue k &v m then
        Some v
    else
        None

let item k (m: Map<'K, 'V>) = find k m

let partition1 f k v (acc1, acc2) =
    if f k v then
        (add k v acc1, acc2)
    else
        (acc1, add k v acc2)

let rec partitionAux f (m: Map<'K, 'V>) acc =
    match getRoot m with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            partition1 f t.Key t.Value acc
        else
            let acc = partitionAux f t.Right acc
            let acc = partition1 f t.Key t.Value acc
            partitionAux f t.Left acc

let partition f m = partitionAux f m (empty, empty)

let filter1 f k v acc =
    if f k v then
        add k v acc
    else
        acc

let rec filterAux f (m: Map<'K, 'V>) acc =
    match getRoot m with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            filter1 f t.Key t.Value acc
        else
            let acc = filterAux f t.Left acc
            let acc = filter1 f t.Key t.Value acc
            filterAux f t.Right acc

let filter f m = filterAux f m empty

let rec spliceOutSuccessor (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> failwith "internal error: Map.spliceOutSuccessor"
    | Some t ->
        if t.Height = 1 then
            t.Key, t.Value, empty
        else if isEmpty t.Left then
            t.Key, t.Value, t.Right
        else
            let k3, v3, l' = spliceOutSuccessor t.Left in
            k3, v3, mk l' t.Key t.Value t.Right

let rec remove k (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> empty
    | Some t ->
        let c = compare k t.Key

        if t.Height = 1 then
            if c = 0 then
                empty
            else
                m
        else if c < 0 then
            rebalance (remove k t.Left) t.Key t.Value t.Right
        elif c = 0 then
            if isEmpty t.Left then
                t.Right
            elif isEmpty t.Right then
                t.Left
            else
                let sk, sv, r' = spliceOutSuccessor t.Right
                mk t.Left sk sv r'
        else
            rebalance t.Left t.Key t.Value (remove k t.Right)

let rec change k (u: 'V option -> 'V option) (m: Map<'K, 'V>) : Map<'K, 'V> =
    match getRoot m with
    | None ->
        match u None with
        | None -> m
        | Some v -> mkMapTreeLeaf (k, v)
    | Some t ->
        if t.Height = 1 then
            let c = compare k t.Key

            if c < 0 then
                match u None with
                | None -> m
                | Some v -> mkMapTreeNode (k, v, empty, m, 2)
            elif c = 0 then
                match u (Some t.Value) with
                | None -> empty
                | Some v -> mkMapTreeLeaf (k, v)
            else
                match u None with
                | None -> m
                | Some v -> mkMapTreeNode (k, v, m, empty, 2)
        else
            let c = compare k t.Key

            if c < 0 then
                rebalance (change k u t.Left) t.Key t.Value t.Right
            elif c = 0 then
                match u (Some t.Value) with
                | None ->
                    if isEmpty t.Left then
                        t.Right
                    elif isEmpty t.Right then
                        t.Left
                    else
                        let sk, sv, r' = spliceOutSuccessor t.Right
                        mk t.Left sk sv r'
                | Some v -> mkMapTreeNode (k, v, t.Left, t.Right, t.Height)
            else
                rebalance t.Left t.Key t.Value (change k u t.Right)

let rec containsKey k (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> false
    | Some t ->
        let c = compare k t.Key

        if t.Height = 1 then
            c = 0
        else if c < 0 then
            containsKey k t.Left
        else
            (c = 0 || containsKey k t.Right)

let rec iterate f (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> ()
    | Some t ->
        if t.Height = 1 then
            f t.Key t.Value
        else
            iterate f t.Left
            f t.Key t.Value
            iterate f t.Right

let rec tryPick f (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> None
    | Some t ->
        if t.Height = 1 then
            f t.Key t.Value
        else
            match tryPick f t.Left with
            | Some _ as res -> res
            | None ->
                match f t.Key t.Value with
                | Some _ as res -> res
                | None -> tryPick f t.Right

let pick chooser (m: Map<'K, 'V>) =
    match tryPick chooser m with
    | None -> throwKeyNotFound ()
    | Some res -> res

let findKey predicate (m: Map<'K, 'V>) =
    m
    |> pick (fun k v ->
        if predicate k v then
            Some k
        else
            None
    )

let tryFindKey predicate (m: Map<'K, 'V>) =
    m
    |> tryPick (fun k v ->
        if predicate k v then
            Some k
        else
            None
    )

let rec exists f (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> false
    | Some t ->
        if t.Height = 1 then
            f t.Key t.Value
        else
            exists f t.Left || f t.Key t.Value || exists f t.Right

let rec forAll f (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> true
    | Some t ->
        if t.Height = 1 then
            f t.Key t.Value
        else
            forAll f t.Left && f t.Key t.Value && forAll f t.Right

let rec mapRange (f: 'V -> 'R) (m: Map<'K, 'V>) : Map<'K, 'R> =
    match getRoot m with
    | None -> empty
    | Some t ->
        if t.Height = 1 then
            mkMapTreeLeaf (t.Key, f t.Value)
        else
            let l2 = mapRange f t.Left
            let v2 = f t.Value
            let r2 = mapRange f t.Right
            mkMapTreeNode (t.Key, v2, l2, r2, t.Height)

let rec map (f: 'K -> 'V -> 'R) (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> empty
    | Some t ->
        if t.Height = 1 then
            mkMapTreeLeaf (t.Key, f t.Key t.Value)
        else
            let l2 = map f t.Left
            let v2 = f t.Key t.Value
            let r2 = map f t.Right
            mkMapTreeNode (t.Key, v2, l2, r2, t.Height)

let rec foldBack f (m: Map<'K, 'V>) x =
    match getRoot m with
    | None -> x
    | Some t ->
        if t.Height = 1 then
            f t.Key t.Value x
        else
            let x = foldBack f t.Right x
            let x = f t.Key t.Value x
            foldBack f t.Left x

let rec fold f x (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> x
    | Some t ->
        if t.Height = 1 then
            f x t.Key t.Value
        else
            let x = fold f x t.Left
            let x = f x t.Key t.Value
            fold f x t.Right

let rec foldFromTo lo hi f (m: Map<'K, 'V>) x =
    match getRoot m with
    | None -> x
    | Some t ->
        if t.Height = 1 then
            let cLoKey = compare lo t.Key
            let cKeyHi = compare t.Key hi

            let x =
                if cLoKey <= 0 && cKeyHi <= 0 then
                    f t.Key t.Value x
                else
                    x

            x
        else
            let cLoKey = compare lo t.Key
            let cKeyHi = compare t.Key hi

            let x =
                if cLoKey < 0 then
                    foldFromTo lo hi f t.Left x
                else
                    x

            let x =
                if cLoKey <= 0 && cKeyHi <= 0 then
                    f t.Key t.Value x
                else
                    x

            let x =
                if cKeyHi < 0 then
                    foldFromTo lo hi f t.Right x
                else
                    x

            x

let foldSection lo hi f (m: Map<'K, 'V>) x =
    if (compare lo hi) = 1 then
        x
    else
        foldFromTo lo hi f m x

let copyToArray m (arr: _[]) i =
    let mutable j = i

    iterate
        (fun k v ->
            arr[j] <- (k, v)
            j <- j + 1
        )
        m

let keys (m: Map<'K, 'V>) =
    // KeyCollection(m) :> ICollection<'K>
    let len = count m
    let res = ResizeArray<_>(len)
    iterate (fun k v -> res.Add(k)) m
    res |> asArray

let values (m: Map<'K, 'V>) =
    // ValueCollection(m) :> ICollection<'V>
    let len = count m
    let res = ResizeArray<_>(len)
    iterate (fun k v -> res.Add(v)) m
    res |> asArray

let toArray (m: Map<'K, 'V>) =
    let len = count m
    let res = ResizeArray<_>(len)
    iterate (fun k v -> res.Add((k, v))) m
    res |> asArray

let toList (m: Map<'K, 'V>) =
    foldBack (fun k v acc -> (k, v) :: acc) m []

let ofArray xs =
    Array.fold (fun acc (k, v) -> add k v acc) empty xs

let ofList xs =
    List.fold (fun acc (k, v) -> add k v acc) empty xs

let ofSeq xs =
    Seq.fold (fun acc (k, v) -> add k v acc) empty xs

/// Imperative left-to-right iterators.
[<NoEquality; NoComparison>]
type MapIterator<'K, 'V when 'K: comparison> =
    {
        /// invariant: always collapseLHS result
        mutable stack: Map<'K, 'V> list
        /// true when MoveNext has been called
        mutable started: bool
    }

// collapseLHS:
// a) Always returns either [] or a list starting with MapOne.
// b) The "fringe" of the set stack is unchanged.
let rec collapseLHS (stack: Map<'K, 'V> list) =
    match stack with
    | [] -> []
    | m :: rest ->
        match getRoot m with
        | None -> collapseLHS rest
        | Some t ->
            if t.Height = 1 then
                stack
            else
                collapseLHS (
                    t.Left :: mkMapTreeLeaf (t.Key, t.Value) :: t.Right :: rest
                )

let mkIterator m =
    {
        stack = collapseLHS [ m ]
        started = false
    }

let notStarted () = failwith SR.enumerationNotStarted

let alreadyFinished () = failwith SR.enumerationAlreadyFinished

let unexpectedStackForCurrent () =
    failwith "Please report error: Map iterator, unexpected stack for current"

let unexpectedStackForMoveNext () =
    failwith "Please report error: Map iterator, unexpected stack for moveNext"

let current i =
    if i.started then
        match i.stack with
        | { root = Some t } :: _ ->
            if t.Height = 1 then
                // KeyValuePair<_, _>(t.Key, t.Value)
                (t.Key, t.Value)
            else
                unexpectedStackForCurrent ()
        | _ -> alreadyFinished ()
    else
        notStarted ()

let rec moveNext i =
    if i.started then
        match i.stack with
        | { root = Some t } :: rest ->
            if t.Height = 1 then
                i.stack <- collapseLHS rest
                not i.stack.IsEmpty
            else
                unexpectedStackForMoveNext ()
        | _ -> false
    else
        i.started <- true // The first call to MoveNext "starts" the enumeration.
        not i.stack.IsEmpty

let toSeq (m: Map<'K, 'V>) =
    Seq.delay (fun () ->
        mkIterator m
        |> Seq.unfold (fun i ->
            if moveNext i then
                Some(current i, i)
            else
                None
        )
    )

let compareTo (m1: Map<'K, 'V>) (m2: Map<'K, 'V>) =
    // LanguagePrimitives.GenericComparison m1 m2
    Seq.compareWith compare (toSeq m1) (toSeq m2)

let equals (m1: Map<'K, 'V>) (m2: Map<'K, 'V>) =
    // LanguagePrimitives.GenericEquality m1 m2
    compareTo m1 m2 = 0

let rec minKeyValue (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> throwKeyNotFound ()
    | Some t ->
        if t.Height = 1 || isEmpty t.Left then
            (t.Key, t.Value)
        else
            minKeyValue t.Left

let rec maxKeyValue (m: Map<'K, 'V>) =
    match getRoot m with
    | None -> throwKeyNotFound ()
    | Some t ->
        if t.Height = 1 || isEmpty t.Right then
            (t.Key, t.Value)
        else
            maxKeyValue t.Right

// type MapEnumerator<'K, 'V when 'K : comparison>(m) =
//     let mutable i = mkIterator m
//     interface System.Collections.Generic.IEnumerator<'K * 'V> with
//         member _.Current: ('K * 'V) = current i
//         member _.Current: obj = box (current i)
//         member _.MoveNext() = moveNext i
//         member _.Reset() = i <- mkIterator m
//         member _.Dispose() = ()

// let mkIEnumerator m =
//     new MapEnumerator<_,_>(m) :> System.Collections.Generic.IEnumerator<_>

// let toSeq (m: Map<'K, 'V>) =
//     let en = mkIEnumerator m
//     en |> Seq.unfold (fun en ->
//         if en.MoveNext()
//         then Some(en.Current, en)
//         else None)

// // let mkIEnumerator m =
// //     let mutable i = mkIterator m
// //     { new IEnumerator<_> with
// //           member _.Current = current i
// //       interface System.Collections.IEnumerator with
// //           member _.Current = box (current i)
// //           member _.MoveNext() = moveNext i
// //           member _.Reset() = i <- mkIterator m
// //       interface System.IDisposable with
// //           member _.Dispose() = ()}

(*

[<System.Diagnostics.DebuggerTypeProxy(typedefof<MapDebugView<_, _>>)>]
[<System.Diagnostics.DebuggerDisplay("Count = {Count}")>]
[<Sealed>]
[<CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1710: IdentifiersShouldHaveCorrectSuffix")>]
[<CompiledName("FSharpMap`2")>]
type Map<[<EqualityConditionalOn>]'K, [<EqualityConditionalOn; ComparisonConditionalOn>]'V when 'K : comparison >(comparer: IComparer<'K>, tree: Map<'K, 'V>) =

    [<System.NonSerialized>]
    // This type is logically immutable. This field is only mutated during deserialization.
    let mutable = comparer

    [<System.NonSerialized>]
    // This type is logically immutable. This field is only mutated during deserialization.
    let mutable tree = tree

    // This type is logically immutable. This field is only mutated during serialization and deserialization.
    //
    // WARNING: The compiled name of this field may never be changed because it is part of the logical
    // WARNING: permanent serialization format for this type.
    let mutable serializedData = null

    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).
    static let empty =
        let = LanguagePrimitives.FastGenericComparer<'K>
        new Map<'K, 'V>(comparer, MapTree.empty)

    [<System.Runtime.Serialization.OnSerializingAttribute>]
    member _.OnSerializing(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        serializedData <- MapTree.toArray tree |> Array.map (fun (k, v) -> KeyValuePair(k, v))

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member _.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    [<System.Runtime.Serialization.OnDeserializedAttribute>]
    member _.OnDeserialized(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        <- LanguagePrimitives.FastGenericComparer<'K>
        tree <- serializedData |> Array.map (fun kvp -> kvp.Key, kvp.Value) |> MapTree.ofArray comparer
        serializedData <- null

    static member Empty : Map<'K, 'V> =
        empty

    static member Create(ie : IEnumerable<_>) : Map<'K, 'V> =
        let = LanguagePrimitives.FastGenericComparer<'K>
        new Map<_, _>(comparer, MapTree.ofSeq ie)

    new (elements : seq<_>) =
        let = LanguagePrimitives.FastGenericComparer<'K>
        new Map<_, _>(comparer, MapTree.ofSeq elements)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal t.Comparer = comparer

    //[<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal t.Tree = tree

    member t.Add(key, value) : Map<'K, 'V> =
#if TRACE_SETS_AND_MAPS
        MapTree.report()
        MapTree.numAdds <- MapTree.numAdds + 1
        let size = MapTree.count t.Tree + 1
        MapTree.totalSizeOnMapAdd <- MapTree.totalSizeOnMapAdd + float size
        if size > MapTree.largestMapSize then
            MapTree.largestMapSize <- size
            MapTree.largestMapStackTrace <- System.Diagnostics.StackTrace().ToString()
#endif
        new Map<'K, 'V>(comparer, MapTree.add key value tree)

    member t.Change(key, f) : Map<'K, 'V> =
        new Map<'K, 'V>(comparer, MapTree.change key f tree)

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member t.IsEmpty = MapTree.isEmpty tree

    member t.Item
     with get(key : 'K) =
#if TRACE_SETS_AND_MAPS
        MapTree.report()
        MapTree.numLookups <- MapTree.numLookups + 1
        MapTree.totalSizeOnMapLookup <- MapTree.totalSizeOnMapLookup + float (MapTree.count tree)
#endif
        MapTree.find key tree

    member t.TryPick f =
        MapTree.tryPick f tree

    member t.Exists predicate =
        MapTree.exists predicate tree

    member t.Filter predicate =
        new Map<'K, 'V>(comparer, MapTree.filter predicate tree)

    member t.ForAll predicate =
        MapTree.forAll predicate tree

    member t.Fold f acc =
        MapTree.foldBack f tree acc

    member t.FoldSection (lo: 'K) (hi: 'K) f (acc: 'z) =
        MapTree.foldSection lo hi f tree acc

    member t.Iterate f =
        MapTree.iterate f tree

    member t.MapRange (f: 'V->'Result) =
        new Map<'K, 'Result>(comparer, MapTree.mapRange f tree)

    member t.Map f =
        new Map<'K, 'b>(comparer, MapTree.map f tree)

    member t.Partition predicate : Map<'K, 'V> * Map<'K, 'V> =
        let r1, r2 = MapTree.partition predicate tree
        new Map<'K, 'V>(comparer, r1), new Map<'K, 'V>(comparer, r2)

    member t.Count =
        MapTree.count tree

    member t.ContainsKey key =
#if TRACE_SETS_AND_MAPS
        MapTree.report()
        MapTree.numLookups <- MapTree.numLookups + 1
        MapTree.totalSizeOnMapLookup <- MapTree.totalSizeOnMapLookup + float (MapTree.count tree)
#endif
        MapTree.containsKey key tree

    member t.Remove key =
        new Map<'K, 'V>(comparer, MapTree.remove key tree)

    member t.TryGetValue(key, [<System.Runtime.InteropServices.Out>] value: byref<'V>) =
        MapTree.tryGetValue key &value tree

    member t.TryFind key =
#if TRACE_SETS_AND_MAPS
        MapTree.report()
        MapTree.numLookups <- MapTree.numLookups + 1
        MapTree.totalSizeOnMapLookup <- MapTree.totalSizeOnMapLookup + float (MapTree.count tree)
#endif
        MapTree.tryFind key tree

    member t.ToList() =
        MapTree.toList tree

    member t.ToArray() =
        MapTree.toArray tree

    member t.Keys = KeyCollection(m) :> ICollection<'K>

    member t.Values = ValueCollection(m) :> ICollection<'V>

    static member ofList l : Map<'K, 'V> =
       let = LanguagePrimitives.FastGenericComparer<'K>
       new Map<_, _>(comparer, MapTree.ofList l)

    member this.ComputeHashCode() =
        let combineHash x y = (x <<< 1) + y + 631
        let mutable res = 0
        for (KeyValue(x, y)) in this do
            res <- combineHash res (hash x)
            res <- combineHash res (Unchecked.hash y)
        res

    override this.Equals that =
        match that with
        | :? Map<'K, 'V> as that ->
            use e1 = (this :> seq<_>).GetEnumerator()
            use e2 = (that :> seq<_>).GetEnumerator()
            let rec loop () =
                let m1 = e1.MoveNext()
                let m2 = e2.MoveNext()
                (m1 = m2) && (not m1 ||
                                 (let e1c = e1.Current
                                  let e2c = e2.Current
                                  ((e1c.Key = e2c.Key) && (Unchecked.equals e1c.Value e2c.Value) && loop())))
            loop()
        | _ -> false

    override this.GetHashCode() = this.ComputeHashCode()

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member _.GetEnumerator() = MapTree.mkIEnumerator tree

    interface IEnumerable with
        member _.GetEnumerator() = (MapTree.mkIEnumerator tree :> IEnumerator)

    interface IDictionary<'K, 'V> with
        member t.Item
            with get x = t[x]
            and  set _ _ = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member t.Keys = t.Keys

        member t.Values = t.Values

        member t.Add(_, _) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member t.ContainsKey k = t.ContainsKey k

        member t.TryGetValue(k, r) = t.TryGetValue(k, &r)

        member t.Remove(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

    interface ICollection<KeyValuePair<'K, 'V>> with
        member _.Add(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Clear() = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Remove(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member t.Contains x = t.ContainsKey x.Key && Unchecked.equals t[x.Key] x.Value

        member _.CopyTo(arr, i) = MapTree.copyToArray tree arr i

        member _.IsReadOnly = true

        member t.Count = t.Count

    interface System.IComparable with
        member t.CompareTo(obj: obj) =
            match obj with
            | :? Map<'K, 'V>  as m2->
                Seq.compareWith
                   (fun (kvp1 : KeyValuePair<_, _>) (kvp2 : KeyValuePair<_, _>)->
                       let c = compare kvp1.Key kvp2.Key
                       if c <> 0 then c else Unchecked.compare kvp1.Value kvp2.Value)
                   m m2
            | _ ->
                invalidArg "obj" (SR.GetString(SR.notComparable))

    interface IReadOnlyCollection<KeyValuePair<'K, 'V>> with
        member t.Count = t.Count

    interface IReadOnlyDictionary<'K, 'V> with

        member t.Item with get key = t[key]

        member t.Keys = t.Keys :> IEnumerable<'K>

        member t.TryGetValue(key, value: byref<'V>) = t.TryGetValue(key, &value)

        member t.Values = t.Values :> IEnumerable<'V>

        member t.ContainsKey key = t.ContainsKey key

    override x.ToString() =
        match List.ofSeq (Seq.truncate 4 x) with
        | [] -> "map []"
        | [KeyValue h1] ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            StringBuilder().Append("map [").Append(txt1).Append("]").ToString()
        | [KeyValue h1; KeyValue h2] ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            let txt2 = LanguagePrimitives.anyToStringShowingNull h2
            StringBuilder().Append("map [").Append(txt1).Append("; ").Append(txt2).Append("]").ToString()
        | [KeyValue h1; KeyValue h2; KeyValue h3] ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            let txt2 = LanguagePrimitives.anyToStringShowingNull h2
            let txt3 = LanguagePrimitives.anyToStringShowingNull h3
            StringBuilder().Append("map [").Append(txt1).Append("; ").Append(txt2).Append("; ").Append(txt3).Append("]").ToString()
        | KeyValue h1 :: KeyValue h2 :: KeyValue h3 :: _ ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            let txt2 = LanguagePrimitives.anyToStringShowingNull h2
            let txt3 = LanguagePrimitives.anyToStringShowingNull h3
            StringBuilder().Append("map [").Append(txt1).Append("; ").Append(txt2).Append("; ").Append(txt3).Append("; ... ]").ToString()

and
    [<Sealed>]
    MapDebugView<'K, 'V when 'K : comparison>(v: Map<'K, 'V>)  =

        [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
        member x.Items =
            v |> Seq.truncate 10000 |> Seq.map KeyValuePairDebugFriendly |> Seq.toArray

and
    [<DebuggerDisplay("{keyValue.Value}", Name = "[{keyValue.Key}]", Type = "")>]
    KeyValuePairDebugFriendly<'K, 'V>(keyValue : KeyValuePair<'K, 'V>) =

        [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
        member x.KeyValue = keyValue

and KeyCollection<'K, 'V when 'K : comparison>(parent: Map<'K, 'V>) =
    interface ICollection<'K> with
        member _.Add(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Clear() = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Remove(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Contains x = parent.ContainsKey x

        member _.CopyTo(arr, index) =
            if isNull arr then nullArg "arr"
            if index < 0 then invalidArg "index" "index must be positive"
            if index + parent.Count > arr.Length then invalidArg "index" "array is smaller than index plus the number of items to copy"

            let mutable i = index
            for item in parent do
                arr[i] <- item.Key
                i <- i + 1

        member _.IsReadOnly = true

        member _.Count = parent.Count

    interface IEnumerable<'K> with
        member _.GetEnumerator() =
            (seq { for item in parent do item.Key}).GetEnumerator()

    interface IEnumerable with
        member _.GetEnumerator() =
            (seq { for item in parent do item.Key}).GetEnumerator() :> IEnumerator

and ValueCollection<'K, 'V when 'K : comparison>(parent: Map<'K, 'V>) =
    interface ICollection<'V> with
        member _.Add(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Clear() = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Remove(_) = raise (NotSupportedException(SR.GetString(SR.mapCannotBeMutated)))

        member _.Contains x = parent.Exists(fun _ value -> Unchecked.equals value x)

        member _.CopyTo(arr, index) =
            if isNull arr then nullArg "arr"
            if index < 0 then invalidArg "index" "index must be positive"
            if index + parent.Count > arr.Length then invalidArg "index" "array is smaller than index plus the number of items to copy"

            let mutable i = index
            for item in parent do
                arr[i] <- item.Value
                i <- i + 1

        member _.IsReadOnly = true

        member _.Count = parent.Count

    interface IEnumerable<'V> with
        member _.GetEnumerator() =
            (seq { for item in parent do item.Value}).GetEnumerator()

    interface IEnumerable with
        member _.GetEnumerator() =
            (seq { for item in parent do item.Value }).GetEnumerator() :> IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Map =

    [<CompiledName("IsEmpty")>]
    let isEmpty (table: Map<_, _>) =
        table.IsEmpty

    [<CompiledName("Add")>]
    let add key value (table: Map<_, _>) =
        table.Add (key, value)

    [<CompiledName("Change")>]
    let change key f (table: Map<_, _>) =
        table.Change (key, f)

    [<CompiledName("Find")>]
    let find key (table: Map<_, _>) =
        table[key]

    [<CompiledName("TryFind")>]
    let tryFind key (table: Map<_, _>) =
        table.TryFind key

    [<CompiledName("Remove")>]
    let remove key (table: Map<_, _>) =
        table.Remove key

    [<CompiledName("ContainsKey")>]
    let containsKey key (table: Map<_, _>) =
        table.ContainsKey key

    [<CompiledName("Iterate")>]
    let iterate action (table: Map<_, _>) =
        table.Iterate action

    [<CompiledName("TryPick")>]
    let tryPick chooser (table: Map<_, _>) =
        table.TryPick chooser

    [<CompiledName("Pick")>]
    let pick chooser (table: Map<_, _>) =
        match tryPick chooser table with
        | None -> raise (KeyNotFoundException())
        | Some res -> res

    [<CompiledName("Exists")>]
    let exists predicate (table: Map<_, _>) =
        table.Exists predicate

    [<CompiledName("Filter")>]
    let filter predicate (table: Map<_, _>) =
        table.Filter predicate

    [<CompiledName("Partition")>]
    let partition predicate (table: Map<_, _>) =
        table.Partition predicate

    [<CompiledName("ForAll")>]
    let forAll predicate (table: Map<_, _>) =
        table.ForAll predicate

    [<CompiledName("Map")>]
    let map mapping (table: Map<_, _>) =
        table.Map mapping

    [<CompiledName("Fold")>]
    let fold<'K, 'T, 'State when 'K : comparison> folder (state: 'State) (table: Map<'K, 'T>) =
        MapTree.fold folder state table.Tree

    [<CompiledName("FoldBack")>]
    let foldBack<'K, 'T, 'State  when 'K : comparison> folder (table: Map<'K, 'T>) (state: 'State) =
        MapTree.foldBack folder table.Tree state

    [<CompiledName("ToSeq")>]
    let toSeq (table: Map<_, _>) =
        table |> Seq.map (fun kvp -> kvp.Key, kvp.Value)

    [<CompiledName("FindKey")>]
    let findKey predicate (table : Map<_, _>) =
        table |> Seq.pick (fun kvp -> let k = kvp.Key in if predicate k kvp.Value then Some k else None)

    [<CompiledName("TryFindKey")>]
    let tryFindKey predicate (table : Map<_, _>) =
        table |> Seq.tryPick (fun kvp -> let k = kvp.Key in if predicate k kvp.Value then Some k else None)

    [<CompiledName("OfList")>]
    let ofList (elements: ('K * 'V) list) =
        Map<_, _>.ofList elements

    [<CompiledName("OfSeq")>]
    let ofSeq elements =
        Map<_, _>.Create elements

    [<CompiledName("OfArray")>]
    let ofArray (elements: ('K * 'V) array) =
       let = LanguagePrimitives.FastGenericComparer<'K>
       new Map<_, _>(comparer, MapTree.ofArray elements)

    [<CompiledName("ToList")>]
    let toList (table: Map<_, _>) =
        table.ToList()

    [<CompiledName("ToArray")>]
    let toArray (table: Map<_, _>) =
        table.ToArray()

    [<CompiledName("Empty")>]
    let empty<'K, 'V  when 'K : comparison> =
        Map<'K, 'V>.Empty

    [<CompiledName("Count")>]
    let count (table: Map<_, _>) =
        table.Count

    [<CompiledName("Keys")>]
    let keys (table: Map<_, _>) = table.Keys

    [<CompiledName("Values")>]
    let values (table: Map<_, _>) = table.Values

*)
