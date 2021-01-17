// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module Map

open System.Collections.Generic

[<NoEquality; NoComparison>]
type MapTreeLeaf<'Key, 'Value>(k: 'Key, v: 'Value) =
    member _.Key = k
    member _.Value = v

type MapTree<'Key, 'Value> = Option<MapTreeLeaf<'Key, 'Value>>

[<NoEquality; NoComparison>]
[<Sealed>]
type MapTreeNode<'Key, 'Value>(k:'Key, v:'Value, left: MapTree<'Key, 'Value>, right: MapTree<'Key, 'Value>, h: int) =
    inherit MapTreeLeaf<'Key,'Value>(k, v)

    member _.Left = left
    member _.Right = right
    member _.Height = h

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MapTree =

    let empty: MapTree<'Key, 'Value> = None

    let inline isEmpty (m: MapTree<'Key, 'Value>) = m.IsNone

    let rec sizeAux acc (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> acc
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn -> sizeAux (sizeAux (acc+1) mn.Left) mn.Right
            | _ -> acc + 1

    let size x = sizeAux 0 x

// #if TRACE_SETS_AND_MAPS
//     let mutable traceCount = 0
//     let mutable numOnes = 0
//     let mutable numNodes = 0
//     let mutable numAdds = 0
//     let mutable numRemoves = 0
//     let mutable numLookups = 0
//     let mutable numUnions = 0
//     let mutable totalSizeOnNodeCreation = 0.0
//     let mutable totalSizeOnMapAdd = 0.0
//     let mutable totalSizeOnMapLookup = 0.0
//     let mutable largestMapSize = 0
//     let mutable largestMapStackTrace = Unchecked.defaultof<_>

//     let report() =
//        traceCount <- traceCount + 1
//        if traceCount % 1000000 = 0 then
//            System.Console.WriteLine(
//                "#MapOne = {0}, #MapNode = {1}, #Add = {2}, #Remove = {3}, #Unions = {4}, #Lookups = {5}, avMapTreeSizeOnNodeCreation = {6}, avMapSizeOnCreation = {7}, avMapSizeOnLookup = {8}",
//                numOnes, numNodes, numAdds, numRemoves, numUnions, numLookups,
//                (totalSizeOnNodeCreation / float (numNodes + numOnes)), (totalSizeOnMapAdd / float numAdds),
//                (totalSizeOnMapLookup / float numLookups))
//            System.Console.WriteLine("#largestMapSize = {0}, largestMapStackTrace = {1}", largestMapSize, largestMapStackTrace)

//     let MapOne n =
//         report()
//         numOnes <- numOnes + 1
//         totalSizeOnNodeCreation <- totalSizeOnNodeCreation + 1.0
//         MapTree n

//     let MapNode (x, l, v, r, h) =
//         report()
//         numNodes <- numNodes + 1
//         let n = MapTreeNode (x, l, v, r, h)
//         totalSizeOnNodeCreation <- totalSizeOnNodeCreation + float (size n)
//         n
// #endif

    let inline height (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> 0
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn -> mn.Height
            | _ -> 1

    [<Literal>]
    let tolerance = 2

    let mk l k v r : MapTree<'Key, 'Value> =
        let hl = height l
        let hr = height r
        let m = if hl < hr then hr else hl
        if m = 0 then // m=0 ~ isEmpty l && isEmpty r
            MapTreeLeaf (k,v) |> Some
        else
            MapTreeNode(k,v,l,r,m+1) :> MapTreeLeaf<'Key, 'Value> |> Some // new map is higher by 1 than the highest

    let rebalance (t1: MapTree<'Key, 'Value>) (k: 'Key) (v: 'Value) (t2: MapTree<'Key, 'Value>) : MapTree<'Key, 'Value> =
        let t1h = height t1
        let t2h = height t2
        if t2h > t1h + tolerance then // right is heavier than left
            match t2.Value with
            | :? MapTreeNode<'Key, 'Value> as t2' ->
                // one of the nodes must have height > height t1 + 1
                if height t2'.Left > t1h + 1 then // balance left: combination
                    match t2'.Left.Value with
                    | :? MapTreeNode<'Key, 'Value> as t2l ->
                        mk (mk t1 k v t2l.Left) t2l.Key t2l.Value (mk t2l.Right t2'.Key t2'.Value t2'.Right)
                    | _ -> failwith "internal error: Map.rebalance"
                else // rotate left
                    mk (mk t1 k v t2'.Left) t2'.Key t2'.Value t2'.Right
            | _ -> failwith "internal error: Map.rebalance"
        else
            if t1h > t2h + tolerance then // left is heavier than right
                match t1.Value with
                | :? MapTreeNode<'Key, 'Value> as t1' ->
                    // one of the nodes must have height > height t2 + 1
                    if height t1'.Right > t2h + 1 then // balance right: combination
                        match t1'.Right.Value with
                        | :? MapTreeNode<'Key, 'Value> as t1r ->
                            mk (mk t1'.Left t1'.Key t1'.Value t1r.Left) t1r.Key t1r.Value (mk t1r.Right k v t2)
                        | _ -> failwith "internal error: Map.rebalance"
                    else
                        mk t1'.Left t1'.Key t1'.Value (mk t1'.Right k v t2)
                | _ -> failwith "internal error: Map.rebalance"
            else mk t1 k v t2

    let rec add (comparer: IComparer<'Key>) k (v: 'Value) (m: MapTree<'Key, 'Value>) : MapTree<'Key, 'Value> =
        match m with
        | None -> MapTreeLeaf (k,v) |> Some
        | Some m2 ->
            let c = comparer.Compare(k, m2.Key)
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                if c < 0 then rebalance (add comparer k v mn.Left) mn.Key mn.Value mn.Right
                elif c = 0 then MapTreeNode(k,v,mn.Left,mn.Right,mn.Height) :> MapTreeLeaf<'Key, 'Value> |> Some
                else rebalance mn.Left mn.Key mn.Value (add comparer k v mn.Right)
            | _ ->
                if c < 0   then MapTreeNode (k,v,empty,m,2) :> MapTreeLeaf<'Key, 'Value> |> Some
                elif c = 0 then MapTreeLeaf (k,v) |> Some
                else            MapTreeNode (k,v,m,empty,2) :> MapTreeLeaf<'Key, 'Value> |> Some

    let rec tryFind (comparer: IComparer<'Key>) k (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> None
        | Some m2 ->
            let c = comparer.Compare(k, m2.Key)
            if c = 0 then Some m2.Value
            else
                match m2 with
                | :? MapTreeNode<'Key, 'Value> as mn ->
                    tryFind comparer k (if c < 0 then mn.Left else mn.Right)
                | _ -> None

    let find (comparer: IComparer<'Key>) k (m: MapTree<'Key, 'Value>) =
        match tryFind comparer k m with
        | Some v -> v
        | None -> raise (KeyNotFoundException())

    let partition1 (comparer: IComparer<'Key>) (f: OptimizedClosures.FSharpFunc<_, _, _>) k v (acc1, acc2) =
        if f.Invoke (k, v) then (add comparer k v acc1, acc2) else (acc1, add comparer k v acc2)

    let rec partitionAux (comparer: IComparer<'Key>) (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: MapTree<'Key, 'Value>) acc =
        match m with
        | None -> acc
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let acc = partitionAux comparer f mn.Right acc
                let acc = partition1 comparer f mn.Key mn.Value acc
                partitionAux comparer f mn.Left acc
            | _ -> partition1 comparer f m2.Key m2.Value acc

    let partition (comparer: IComparer<'Key>) f m =
        partitionAux comparer (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m (empty, empty)

    let filter1 (comparer: IComparer<'Key>) (f: OptimizedClosures.FSharpFunc<_, _, _>) k v acc =
        if f.Invoke (k, v) then add comparer k v acc else acc

    let rec filterAux (comparer: IComparer<'Key>) (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: MapTree<'Key, 'Value>) acc =
        match m with
        | None -> acc
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let acc = filterAux comparer f mn.Left acc
                let acc = filter1 comparer f mn.Key mn.Value acc
                filterAux comparer f mn.Right acc
            | _ -> filter1 comparer f m2.Key m2.Value acc

    let filter (comparer: IComparer<'Key>) f m =
        filterAux comparer (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m empty

    let rec spliceOutSuccessor (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> failwith "internal error: Map.spliceOutSuccessor"
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                if isEmpty mn.Left then mn.Key, mn.Value, mn.Right
                else let k3, v3, l' = spliceOutSuccessor mn.Left in k3, v3, mk l' mn.Key mn.Value mn.Right
            | _ -> m2.Key, m2.Value, empty

    let rec remove (comparer: IComparer<'Key>) k (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> empty
        | Some m2 ->
            let c = comparer.Compare(k, m2.Key)
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                if c < 0 then rebalance (remove comparer k mn.Left) mn.Key mn.Value mn.Right
                elif c = 0 then
                    if isEmpty mn.Left then mn.Right
                    elif isEmpty mn.Right then mn.Left
                    else
                        let sk, sv, r' = spliceOutSuccessor mn.Right
                        mk mn.Left sk sv r'
                else rebalance mn.Left mn.Key mn.Value (remove comparer k mn.Right)
            | _ ->
                if c = 0 then empty else m

    let rec change (comparer: IComparer<'Key>) k (u: 'Value option -> 'Value option) (m: MapTree<'Key, 'Value>) : MapTree<'Key,'Value> =
        match m with
        | None ->
            match u None with
            | None -> m
            | Some v -> MapTreeLeaf (k, v) |> Some
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let c = comparer.Compare(k, mn.Key)
                if c < 0 then
                    rebalance (change comparer k u mn.Left) mn.Key mn.Value mn.Right
                elif c = 0 then
                    match u (Some mn.Value) with
                    | None ->
                        if isEmpty mn.Left then mn.Right
                        elif isEmpty mn.Right then mn.Left
                        else
                            let sk, sv, r' = spliceOutSuccessor mn.Right
                            mk mn.Left sk sv r'
                    | Some v -> MapTreeNode (k, v, mn.Left, mn.Right, mn.Height) :> MapTreeLeaf<'Key,'Value> |> Some
                else
                    rebalance mn.Left mn.Key mn.Value (change comparer k u mn.Right)
            | _ ->
                let c = comparer.Compare(k, m2.Key)
                if c < 0 then
                    match u None with
                    | None -> m
                    | Some v -> MapTreeNode (k, v, empty, m, 2) :> MapTreeLeaf<'Key,'Value> |> Some
                elif c = 0 then
                    match u (Some m2.Value) with
                    | None -> empty
                    | Some v -> MapTreeLeaf (k, v) |> Some
                else
                    match u None with
                    | None -> m
                    | Some v -> MapTreeNode (k, v, m, empty, 2) :> MapTreeLeaf<'Key,'Value> |> Some

    let rec mem (comparer: IComparer<'Key>) k (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> false
        | Some m2 ->
            let c = comparer.Compare(k, m2.Key)
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                if c < 0 then mem comparer k mn.Left
                else (c = 0 || mem comparer k mn.Right)
            | _ -> c = 0

    let rec iterOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> ()
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn -> iterOpt f mn.Left; f.Invoke (mn.Key, mn.Value); iterOpt f mn.Right
            | _ -> f.Invoke (m2.Key, m2.Value)

    let iter f m =
        iterOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec tryPickOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> None
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                match tryPickOpt f mn.Left with
                | Some _ as res -> res
                | None ->
                match f.Invoke (mn.Key, mn.Value) with
                | Some _ as res -> res
                | None ->
                tryPickOpt f mn.Right
            | _ -> f.Invoke (m2.Key, m2.Value)

    let tryPick f m =
        tryPickOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec existsOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> false
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn -> existsOpt f mn.Left || f.Invoke (mn.Key, mn.Value) || existsOpt f mn.Right
            | _ -> f.Invoke (m2.Key, m2.Value)

    let exists f m =
        existsOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec forallOpt (f: OptimizedClosures.FSharpFunc<_, _, _>) (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> true
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn -> forallOpt f mn.Left && f.Invoke (mn.Key, mn.Value) && forallOpt f mn.Right
            | _ -> f.Invoke (m2.Key, m2.Value)

    let forall f m =
        forallOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec map (f:'Value -> 'Result) (m: MapTree<'Key, 'Value>) : MapTree<'Key, 'Result> =
        match m with
        | None -> empty
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let l2 = map f mn.Left
                let v2 = f mn.Value
                let r2 = map f mn.Right
                MapTreeNode (mn.Key, v2, l2, r2, mn.Height) :> MapTreeLeaf<'Key, 'Result> |> Some
            | _ -> MapTreeLeaf (m2.Key, f m2.Value) |> Some

    let rec mapiOpt (f: OptimizedClosures.FSharpFunc<'Key, 'Value, 'Result>) (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> empty
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let l2 = mapiOpt f mn.Left
                let v2 = f.Invoke (mn.Key, mn.Value)
                let r2 = mapiOpt f mn.Right
                MapTreeNode (mn.Key, v2, l2, r2, mn.Height) :> MapTreeLeaf<'Key, 'Result> |> Some
            | _ -> MapTreeLeaf (m2.Key, f.Invoke (m2.Key, m2.Value)) |> Some

    let mapi f m =
        mapiOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m

    let rec foldBackOpt (f: OptimizedClosures.FSharpFunc<_, _, _, _>) (m: MapTree<'Key, 'Value>) x =
        match m with
        | None -> x
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let x = foldBackOpt f mn.Right x
                let x = f.Invoke (mn.Key, mn.Value, x)
                foldBackOpt f mn.Left x
            | _ -> f.Invoke (m2.Key, m2.Value, x)

    let foldBack f m x =
        foldBackOpt (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f) m x

    let rec foldOpt (f: OptimizedClosures.FSharpFunc<_, _, _, _>) x (m: MapTree<'Key, 'Value>) =
        match m with
        | None -> x
        | Some m2 ->
            match m2 with
            | :? MapTreeNode<'Key, 'Value> as mn ->
                let x = foldOpt f x mn.Left
                let x = f.Invoke (x, mn.Key, mn.Value)
                foldOpt f x mn.Right
            | _ -> f.Invoke (x, m2.Key, m2.Value)

    let fold f x m =
        foldOpt (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f) x m

    let foldSectionOpt (comparer: IComparer<'Key>) lo hi (f: OptimizedClosures.FSharpFunc<_, _, _, _>) (m: MapTree<'Key, 'Value>) x =
        let rec foldFromTo (f: OptimizedClosures.FSharpFunc<_, _, _, _>) (m: MapTree<'Key, 'Value>) x =
            match m with
            | None -> x
            | Some m2 ->
                match m2 with
                | :? MapTreeNode<'Key, 'Value> as mn ->
                    let cLoKey = comparer.Compare(lo, mn.Key)
                    let cKeyHi = comparer.Compare(mn.Key, hi)
                    let x = if cLoKey < 0 then foldFromTo f mn.Left x else x
                    let x = if cLoKey <= 0 && cKeyHi <= 0 then f.Invoke (mn.Key, mn.Value, x) else x
                    let x = if cKeyHi < 0 then foldFromTo f mn.Right x else x
                    x
                | _ ->
                    let cLoKey = comparer.Compare(lo, m2.Key)
                    let cKeyHi = comparer.Compare(m2.Key, hi)
                    let x = if cLoKey <= 0 && cKeyHi <= 0 then f.Invoke (m2.Key, m2.Value, x) else x
                    x

        if comparer.Compare(lo, hi) = 1 then x else foldFromTo f m x

    let foldSection (comparer: IComparer<'Key>) lo hi f m x =
        foldSectionOpt comparer lo hi (OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f) m x

    let toList (m: MapTree<'Key, 'Value>) =
        let rec loop (m: MapTree<'Key, 'Value>) acc =
            match m with
            | None -> acc
            | Some m2 ->
                match m2 with
                | :? MapTreeNode<'Key, 'Value> as mn -> loop mn.Left ((mn.Key, mn.Value) :: loop mn.Right acc)
                | _ -> (m2.Key, m2.Value) :: acc
        loop m []

    let toArray (m: MapTree<'Key, 'Value>): ('Key * 'Value)[] =
        m |> toList |> Array.ofList

    let ofList comparer l =
        List.fold (fun acc (k, v) -> add comparer k v acc) empty l

    let rec mkFromEnumerator comparer acc (e : IEnumerator<_>) =
        if e.MoveNext() then
            let (x, y) = e.Current
            mkFromEnumerator comparer (add comparer x y acc) e
        else acc

    let ofArray comparer (arr: array<'Key * 'Value>) =
        let mutable res = empty
        for (x, y) in arr do
            res <- add comparer x y res
        res

    let ofSeq comparer (c: seq<'Key * 'T>) =
        match c with
        | :? array<'Key * 'T> as xs -> ofArray comparer xs
        // | :? list<'Key * 'T> as xs -> ofList comparer xs
        | _ ->
            use ie = c.GetEnumerator()
            mkFromEnumerator comparer empty ie

    let copyToArray m (arr: _[]) i =
        let mutable j = i
        m |> iter (fun x y -> arr.[j] <- KeyValuePair(x, y); j <- j + 1)

    /// Imperative left-to-right iterators.
    [<NoEquality; NoComparison>]
    type MapIterator<'Key, 'Value when 'Key : comparison > =
         { /// invariant: always collapseLHS result
           mutable stack: MapTree<'Key, 'Value> list

           /// true when MoveNext has been called
           mutable started : bool }

    // collapseLHS:
    // a) Always returns either [] or a list starting with MapOne.
    // b) The "fringe" of the set stack is unchanged.
    let rec collapseLHS (stack: MapTree<'Key, 'Value> list) =
        match stack with
        | [] -> []
        | m :: rest ->
            match m with
            | None -> collapseLHS rest
            | Some m2 ->
                match m2 with
                | :? MapTreeNode<'Key, 'Value> as mn ->
                    collapseLHS (mn.Left :: (MapTreeLeaf (mn.Key, mn.Value) |> Some) :: mn.Right :: rest)
                | _ -> stack

    let mkIterator m =
        { stack = collapseLHS [m]; started = false }

    let notStarted() = failwith "enumeration not started"

    let alreadyFinished() = failwith "enumeration already finished"

    let current i =
        if i.started then
            match i.stack with
            | []     -> alreadyFinished()
            | None :: _ ->
                failwith "Please report error: Map iterator, unexpected stack for current"
            | Some m :: _ ->
                match m with
                | :? MapTreeNode<'Key, 'Value> ->
                    failwith "Please report error: Map iterator, unexpected stack for current"
                | _ -> new KeyValuePair<_, _>(m.Key, m.Value)
        else
            notStarted()

    let rec moveNext i =
        if i.started then
            match i.stack with
            | [] -> false
            | None :: rest ->
                failwith "Please report error: Map iterator, unexpected stack for moveNext"
            | Some m :: rest ->
                match m with
                | :? MapTreeNode<'Key, 'Value> ->
                    failwith "Please report error: Map iterator, unexpected stack for moveNext"
                | _ ->
                    i.stack <- collapseLHS rest
                    not i.stack.IsEmpty
        else
            i.started <- true  // The first call to MoveNext "starts" the enumeration.
            not i.stack.IsEmpty

    let mkIEnumerator m =
        let mutable i = mkIterator m
        { new IEnumerator<_> with
              member __.Current = current i
          interface System.Collections.IEnumerator with
              member __.Current = box (current i)
              member __.MoveNext() = moveNext i
              member __.Reset() = i <- mkIterator m
          interface System.IDisposable with
              member __.Dispose() = ()}

    let toSeq s =
        let en = mkIEnumerator s
        en |> Seq.unfold (fun en ->
            if en.MoveNext()
            then Some(en.Current, en)
            else None)

[<Sealed>]
[<CompiledName("FSharpMap")>]
[<Fable.Core.NoOverloadSuffix>]
type Map<[<EqualityConditionalOn>]'Key, [<EqualityConditionalOn; ComparisonConditionalOn>]'Value when 'Key : comparison >(comparer: IComparer<'Key>, tree: MapTree<'Key, 'Value>) =

    // [<System.NonSerialized>]
    // This type is logically immutable. This field is only mutated during deserialization.
    // let mutable comparer = comparer

    // [<System.NonSerialized>]
    // This type is logically immutable. This field is only mutated during deserialization.
    // let mutable tree = tree

    // // This type is logically immutable. This field is only mutated during serialization and deserialization.
    // //
    // // WARNING: The compiled name of this field may never be changed because it is part of the logical
    // // WARNING: permanent serialization format for this type.
    // let mutable serializedData = null

    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).
    static let empty =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new Map<'Key, 'Value>(comparer, MapTree.empty)

    // [<System.Runtime.Serialization.OnSerializingAttribute>]
    // member __.OnSerializing(context: System.Runtime.Serialization.StreamingContext) =
    //     ignore context
    //     serializedData <- MapTree.toArray tree |> Array.map (fun (k, v) -> KeyValuePair(k, v))

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member __.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    // [<System.Runtime.Serialization.OnDeserializedAttribute>]
    // member __.OnDeserialized(context: System.Runtime.Serialization.StreamingContext) =
    //     ignore context
    //     comparer <- LanguagePrimitives.FastGenericComparer<'Key>
    //     tree <- serializedData |> Array.map (fun (KeyValue(k, v)) -> (k, v)) |> MapTree.ofArray comparer
    //     serializedData <- null

    static member Empty : Map<'Key, 'Value> =
        empty

    static member Create(ie : IEnumerable<_>) : Map<'Key, 'Value> =
        let comparer = LanguagePrimitives.FastGenericComparer<'Key>
        new Map<_, _>(comparer, MapTree.ofSeq comparer ie)

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal m.Comparer = comparer

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal m.Tree = tree

    interface Fable.Core.Symbol_wellknown with
        member _.``Symbol.toStringTag`` = "FSharpMap"

    member m.Add(key, value) : Map<'Key, 'Value> =
// #if TRACE_SETS_AND_MAPS
//         MapTree.report()
//         MapTree.numAdds <- MapTree.numAdds + 1
//         let size = MapTree.size m.Tree + 1
//         MapTree.totalSizeOnMapAdd <- MapTree.totalSizeOnMapAdd + float size
//         if size > MapTree.largestMapSize then
//             MapTree.largestMapSize <- size
//             MapTree.largestMapStackTrace <- System.Diagnostics.StackTrace().ToString()
// #endif
        new Map<'Key, 'Value>(comparer, MapTree.add comparer key value tree)

    member m.Change(key, f) : Map<'Key, 'Value> =
        new Map<'Key, 'Value>(comparer, MapTree.change comparer key f tree)

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member m.IsEmpty = MapTree.isEmpty tree

    member m.Item
     with get(key : 'Key) =
// #if TRACE_SETS_AND_MAPS
//         MapTree.report()
//         MapTree.numLookups <- MapTree.numLookups + 1
//         MapTree.totalSizeOnMapLookup <- MapTree.totalSizeOnMapLookup + float (MapTree.size tree)
// #endif
        MapTree.find comparer key tree

    member m.TryPick f =
        MapTree.tryPick f tree

    member m.Exists predicate =
        MapTree.exists predicate tree

    member m.Filter predicate =
        new Map<'Key, 'Value>(comparer, MapTree.filter comparer predicate tree)

    member m.ForAll predicate =
        MapTree.forall predicate tree

    member m.Fold f acc =
        MapTree.foldBack f tree acc

    member m.FoldSection (lo:'Key) (hi:'Key) f (acc:'z) =
        MapTree.foldSection comparer lo hi f tree acc

    member m.Iterate f =
        MapTree.iter f tree

    member m.MapRange (f:'Value->'Result) =
        new Map<'Key, 'Result>(comparer, MapTree.map f tree)

    member m.Map f =
        new Map<'Key, 'b>(comparer, MapTree.mapi f tree)

    member m.Partition predicate : Map<'Key, 'Value> * Map<'Key, 'Value> =
        let r1, r2 = MapTree.partition comparer predicate tree
        new Map<'Key, 'Value>(comparer, r1), new Map<'Key, 'Value>(comparer, r2)

    member m.Count =
        MapTree.size tree

    member m.ContainsKey key =
// #if TRACE_SETS_AND_MAPS
//         MapTree.report()
//         MapTree.numLookups <- MapTree.numLookups + 1
//         MapTree.totalSizeOnMapLookup <- MapTree.totalSizeOnMapLookup + float (MapTree.size tree)
// #endif
        MapTree.mem comparer key tree

    member m.Remove key =
        new Map<'Key, 'Value>(comparer, MapTree.remove comparer key tree)

    member __.TryGetValue(key: 'Key, value: 'Value ref) =
        match MapTree.tryFind comparer key tree with
        | Some v -> value := v; true
        | None -> false

    member m.TryFind key =
// #if TRACE_SETS_AND_MAPS
//         MapTree.report()
//         MapTree.numLookups <- MapTree.numLookups + 1
//         MapTree.totalSizeOnMapLookup <- MapTree.totalSizeOnMapLookup + float (MapTree.size tree)
// #endif
        MapTree.tryFind comparer key tree

    member m.ToList() =
        MapTree.toList tree

    member m.ToArray() =
        MapTree.toArray tree

    // static member ofList l : Map<'Key, 'Value> =
    //    let comparer = LanguagePrimitives.FastGenericComparer<'Key>
    //    new Map<_, _>(comparer, MapTree.ofList comparer l)

    member this.ComputeHashCode() =
        let combineHash x y = (x <<< 1) + y + 631
        let mutable res = 0
        for (KeyValue(x, y)) in this do
            res <- combineHash res (hash x)
            res <- combineHash res (Unchecked.hash y)
        res

    override this.GetHashCode() = this.ComputeHashCode()

    override this.Equals that =
        match that with
        | :? Map<'Key, 'Value> as that ->
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

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with
        member __.GetEnumerator() = MapTree.mkIEnumerator tree

    interface System.Collections.IEnumerable with
        member __.GetEnumerator() = MapTree.mkIEnumerator tree :> System.Collections.IEnumerator

    interface System.IComparable with
        member m.CompareTo(obj: obj) =
            match obj with
            | :? Map<'Key, 'Value>  as m2->
                Seq.compareWith
                   (fun (kvp1 : KeyValuePair<_, _>) (kvp2 : KeyValuePair<_, _>)->
                       let c = comparer.Compare(kvp1.Key, kvp2.Key) in
                       if c <> 0 then c else Unchecked.compare kvp1.Value kvp2.Value)
                   m m2
            | _ ->
                invalidArg "obj" "not comparable"

    // interface IDictionary<'Key, 'Value> with
    //     member m.Item
    //         with get x = m.[x]
    //         and  set x v = ignore(x, v); raise (System.NotSupportedException("Map cannot be mutated"))
    //     // REVIEW: this implementation could avoid copying the Values to an array
    //     member m.Keys = ([| for kvp in m -> kvp.Key |] :> ICollection<'Key>)
    //     // REVIEW: this implementation could avoid copying the Values to an array
    //     member m.Values = ([| for kvp in m -> kvp.Value |] :> ICollection<'Value>)
    //     member m.Add(k, v) = ignore(k, v); raise (System.NotSupportedException("Map cannot be mutated"))
    //     member m.ContainsKey k = m.ContainsKey k
    //     member m.TryGetValue(k, r) = m.TryGetValue(k, &r)
    //     member m.Remove(k : 'Key) = ignore k; (raise (System.NotSupportedException("Map cannot be mutated")) : bool)

    interface ICollection<KeyValuePair<'Key, 'Value>> with
        member m.Add x = ignore x; raise (System.NotSupportedException("Map cannot be mutated"))
        member m.Clear() = raise (System.NotSupportedException("Map cannot be mutated"))
        member m.Remove x = ignore x; raise (System.NotSupportedException("Map cannot be mutated"))
        member m.Contains x = m.ContainsKey x.Key && Unchecked.equals m.[x.Key] x.Value
        member m.CopyTo(arr, i) = MapTree.copyToArray tree arr i
        member m.IsReadOnly = true
        member m.Count = m.Count

    interface IReadOnlyCollection<KeyValuePair<'Key, 'Value>> with
        member m.Count = m.Count

    // interface IReadOnlyDictionary<'Key, 'Value> with
    //     member m.Item with get key = m.[key]
    //     member m.Keys = seq { for kvp in m -> kvp.Key }
    //     member m.TryGetValue(key, value: byref<'Value>) = m.TryGetValue(key, &value)
    //     member m.Values = seq { for kvp in m -> kvp.Value }
    //     member m.ContainsKey key = m.ContainsKey key

    interface Fable.Core.JS.Map<'Key,'Value> with
        member m.size = m.Count
        member m.clear() = failwith "Map cannot be mutated"; ()
        member m.delete(_) = failwith "Map cannot be mutated"; false
        member m.entries() = m |> Seq.map (fun p -> p.Key, p.Value)
        member m.get(k) = m.Item(k)
        member m.has(k) = m.ContainsKey(k)
        member m.keys() = m |> Seq.map (fun p -> p.Key)
        member m.set(k, v) = failwith "Map cannot be mutated"; m :> Fable.Core.JS.Map<'Key,'Value>
        member m.values() = m |> Seq.map (fun p -> p.Value)
        member m.forEach(f, ?thisArg) = m |> Seq.iter (fun p -> f p.Value p.Key m)

    override this.ToString() =
        let inline toStr (kv: KeyValuePair<'Key,'Value>) = System.String.Format("({0}, {1})", kv.Key, kv.Value)
        let str = this |> Seq.map toStr |> String.concat "; "
        "map [" + str + "]"

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module Map =

// [<CompiledName("IsEmpty")>]
let isEmpty (table: Map<_, _>) =
    table.IsEmpty

// [<CompiledName("Add")>]
let add key value (table: Map<_, _>) =
    table.Add (key, value)

// [<CompiledName("Change")>]
let change key f (table: Map<_, _>) =
    table.Change (key, f)

// [<CompiledName("Find")>]
let find key (table: Map<_, _>) =
    table.[key]

// [<CompiledName("TryFind")>]
let tryFind key (table: Map<_, _>) =
    table.TryFind key

// [<CompiledName("Remove")>]
let remove key (table: Map<_, _>) =
    table.Remove key

// [<CompiledName("ContainsKey")>]
let containsKey key (table: Map<_, _>) =
    table.ContainsKey key

// [<CompiledName("Iterate")>]
let iterate action (table: Map<_, _>) =
    table.Iterate action

// [<CompiledName("TryPick")>]
let tryPick chooser (table: Map<_, _>) =
    table.TryPick chooser

// [<CompiledName("Pick")>]
let pick chooser (table: Map<_, _>) =
    match tryPick chooser table with
    | None -> raise (KeyNotFoundException())
    | Some res -> res

// [<CompiledName("Exists")>]
let exists predicate (table: Map<_, _>) =
    table.Exists predicate

// [<CompiledName("Filter")>]
let filter predicate (table: Map<_, _>) =
    table.Filter predicate

// [<CompiledName("Partition")>]
let partition predicate (table: Map<_, _>) =
    table.Partition predicate

// [<CompiledName("ForAll")>]
let forAll predicate (table: Map<_, _>) =
    table.ForAll predicate

// [<CompiledName("Map")>]
let map mapping (table: Map<_, _>) =
    table.Map mapping

// [<CompiledName("Fold")>]
let fold<'Key, 'T, 'State when 'Key : comparison> folder (state:'State) (table: Map<'Key, 'T>) =
    MapTree.fold folder state table.Tree

// [<CompiledName("FoldBack")>]
let foldBack<'Key, 'T, 'State  when 'Key : comparison> folder (table: Map<'Key, 'T>) (state:'State) =
    MapTree.foldBack folder table.Tree state

// [<CompiledName("ToSeq")>]
let toSeq (table: Map<_, _>) =
    table |> Seq.map (fun kvp -> kvp.Key, kvp.Value)

// [<CompiledName("FindKey")>]
let findKey predicate (table : Map<_, _>) =
    table |> Seq.pick (fun kvp -> let k = kvp.Key in if predicate k kvp.Value then Some k else None)

// [<CompiledName("TryFindKey")>]
let tryFindKey predicate (table : Map<_, _>) =
    table |> Seq.tryPick (fun kvp -> let k = kvp.Key in if predicate k kvp.Value then Some k else None)

// [<CompiledName("OfList")>]
let ofList (elements: ('Key * 'Value) list) =
    Map<_, _>.Create elements

// [<CompiledName("OfSeq")>]
let ofSeq elements =
    Map<_, _>.Create elements

// [<CompiledName("OfArray")>]
let ofArray (elements: ('Key * 'Value) array) =
   let comparer = LanguagePrimitives.FastGenericComparer<'Key>
   new Map<_, _>(comparer, MapTree.ofArray comparer elements)

// [<CompiledName("ToList")>]
let toList (table: Map<_, _>) =
    table.ToList()

// [<CompiledName("ToArray")>]
let toArray (table: Map<_, _>) =
    table.ToArray()

// [<CompiledName("Empty")>]
let empty<'Key, 'Value  when 'Key : comparison> =
    Map<'Key, 'Value>.Empty

let createMutable (source: KeyValuePair<'Key, 'Value> seq) ([<Fable.Core.Inject>] comparer: IEqualityComparer<'Key>) =
    let map = Fable.Collections.MutableMap(source, comparer)
    map :> Fable.Core.JS.Map<_,_>

let groupBy (projection: 'T -> 'Key) (xs: 'T seq) ([<Fable.Core.Inject>] comparer: IEqualityComparer<'Key>): ('Key * 'T seq) seq =
    let dict: Fable.Core.JS.Map<_,ResizeArray<'T>> = createMutable Seq.empty comparer

    // Build the groupings
    for v in xs do
        let key = projection v
        if dict.has(key) then dict.get(key).Add(v)
        else dict.set(key, ResizeArray [v]) |> ignore

    // Mapping shouldn't be necessary because KeyValuePair compiles
    // as a tuple, but let's do it just in case the implementation changes
    dict.entries() |> Seq.map (fun (k,v) -> k, upcast v)

let countBy (projection: 'T -> 'Key) (xs: 'T seq) ([<Fable.Core.Inject>] comparer: IEqualityComparer<'Key>): ('Key * int) seq =
    let dict = createMutable Seq.empty comparer

    for value in xs do
        let key = projection value
        if dict.has(key) then dict.set(key, dict.get(key) + 1)
        else dict.set(key, 1)
        |> ignore

    dict.entries()

// [<CompiledName("Count")>]
let count (table: Map<_, _>) =
    table.Count