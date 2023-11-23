// Adapted from https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/set.fs
// Copyright (c) Microsoft Corporation.  All Rights Reserved.
// See https://github.com/dotnet/fsharp/blob/main/License.txt

module Set_

open Global_

// A functional language implementation of binary trees

[<NoEquality; NoComparison>]
type SetTree<'T> =
    {
        Height: int
        Key: 'T
        Left: Set<'T>
        Right: Set<'T>
    }

and [<Struct; CompiledName("Set")>] Set<'T> = { root: Option<SetTree<'T>> }

type 'T set = Set<'T>

let inline private getRoot s = s.root

let private mkSet root = { root = root }

let empty: Set<'T> = { root = None }

let isEmpty (s: Set<'T>) = (getRoot s).IsNone

let mkSetTreeLeaf (key: 'T) : Set<'T> =
    Some
        {
            Key = key
            Left = empty
            Right = empty
            Height = 1
        }
    |> mkSet

let mkSetTreeNode
    (
        key: 'T,
        left: Set<'T>,
        right: Set<'T>,
        height: int
    )
    : Set<'T>
    =
    Some
        {
            Key = key
            Left = left
            Right = right
            Height = height
        }
    |> mkSet

let singleton (value: 'T) = mkSetTreeLeaf value

let rec countAux (s: Set<'T>) acc =
    match getRoot s with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            acc + 1
        else
            countAux t.Left (countAux t.Right (acc + 1))

let count s = countAux s 0

let inline height (s: Set<'T>) =
    match getRoot s with
    | None -> 0
    | Some t -> t.Height

[<Literal>]
let private tolerance = 2

let mk l k r : Set<'T> =
    let hl = height l
    let hr = height r

    let m =
        if hl < hr then
            hr
        else
            hl

    if m = 0 then // m=0 ~ isEmpty l && isEmpty r
        mkSetTreeLeaf k
    else
        mkSetTreeNode (k, l, r, m + 1)

let rebalance (t1: Set<'T>) v (t2: Set<'T>) =
    let t1h = height t1
    let t2h = height t2

    if t2h > t1h + tolerance then // right is heavier than left
        let t2' = (getRoot t2).Value
        // one of the nodes must have height > height t1 + 1
        if height t2'.Left > t1h + 1 then // balance left: combination
            let t2l = (getRoot t2'.Left).Value
            mk (mk t1 v t2l.Left) t2l.Key (mk t2l.Right t2'.Key t2'.Right)
        else // rotate left
            mk (mk t1 v t2'.Left) t2'.Key t2'.Right
    else if t1h > t2h + tolerance then // left is heavier than right
        let t1' = (getRoot t1).Value
        // one of the nodes must have height > height t2 + 1
        if height t1'.Right > t2h + 1 then
            // balance right: combination
            let t1r = (getRoot t1'.Right).Value
            mk (mk t1'.Left t1'.Key t1r.Left) t1r.Key (mk t1r.Right v t2)
        else
            mk t1'.Left t1'.Key (mk t1'.Right v t2)
    else
        mk t1 v t2

let rec add k (s: Set<'T>) : Set<'T> =
    match getRoot s with
    | None -> mkSetTreeLeaf k
    | Some t ->
        let c = compare k t.Key

        if t.Height = 1 then
            // nb. no check for rebalance needed for small trees, also be sure to reuse node already allocated
            if c < 0 then
                mkSetTreeNode (k, empty, s, 2)
            elif c = 0 then
                s
            else
                mkSetTreeNode (k, s, empty, 2)
        else if c < 0 then
            rebalance (add k t.Left) t.Key t.Right
        elif c = 0 then
            s
        else
            rebalance t.Left t.Key (add k t.Right)

let rec balance (s1: Set<'T>) k (s2: Set<'T>) =
    // Given t1 < k < t2 where t1 and t2 are "balanced",
    // return a balanced tree for <t1, k, t2>.
    // Recall: balance means subtrees heights differ by at most "tolerance"
    match s1 |> getRoot with
    | None -> add k s2 // drop t1 = empty
    | Some t1 ->
        match s2 |> getRoot with
        | None -> add k s1 // drop t2 = empty
        | Some t2 ->
            if t1.Height = 1 then
                add k (add t1.Key s2)
            else if t2.Height = 1 then
                add k (add t2.Key s1)
            else if
                // Have:  (t1l < k1 < t1r) < k < (t2l < k2 < t2r)
                // Either (a) h1, h2 differ by at most 2 - no rebalance needed.
                //        (b) h1 too small, i.e. h1+2 < h2
                //        (c) h2 too small, i.e. h2+2 < h1
                t1.Height + tolerance < t2.Height
            then
                // case: b, h1 too small
                // push t1 into low side of t2, may increase height by 1 so rebalance
                rebalance (balance s1 k t2.Left) t2.Key t2.Right
            elif t2.Height + tolerance < t1.Height then
                // case: c, h2 too small
                // push t2 into high side of t1, may increase height by 1 so rebalance
                rebalance t1.Left t1.Key (balance t1.Right k s2)
            else
                // case: a, h1 and h2 meet balance requirement
                mk s1 k s2

let rec split pivot (s: Set<'T>) =
    // Given a pivot and a set t
    // Return { x in t s.t. x < pivot }, pivot in t?, { x in t s.t. x > pivot }
    match getRoot s with
    | None -> empty, false, empty
    | Some t ->
        if t.Height = 1 then
            let c = compare t.Key pivot

            if c < 0 then
                s, false, empty // singleton under pivot
            elif c = 0 then
                empty, true, empty // singleton is    pivot
            else
                empty, false, s // singleton over  pivot
        else
            let c = compare pivot t.Key

            if c < 0 then // pivot t1
                let t11Lo, havePivot, t11Hi = split pivot t.Left
                t11Lo, havePivot, balance t11Hi t.Key t.Right
            elif c = 0 then // pivot is k1
                t.Left, true, t.Right
            else // pivot t2
                let t12Lo, havePivot, t12Hi = split pivot t.Right
                balance t.Left t.Key t12Lo, havePivot, t12Hi

let rec spliceOutSuccessor (s: Set<'T>) =
    match getRoot s with
    | None -> failwith "internal error: Set.spliceOutSuccessor"
    | Some t ->
        if t.Height = 1 then
            t.Key, empty
        else if isEmpty t.Left then
            t.Key, t.Right
        else
            let k3, l' = spliceOutSuccessor t.Left in k3, mk l' t.Key t.Right

let rec remove k (s: Set<'T>) =
    match getRoot s with
    | None -> s
    | Some t ->
        let c = compare k t.Key

        if t.Height = 1 then
            if c = 0 then
                empty
            else
                s
        else if c < 0 then
            rebalance (remove k t.Left) t.Key t.Right
        elif c = 0 then
            if isEmpty t.Left then
                t.Right
            elif isEmpty t.Right then
                t.Left
            else
                let sk, r' = spliceOutSuccessor t.Right
                mk t.Left sk r'
        else
            rebalance t.Left t.Key (remove k t.Right)

let rec contains k (s: Set<'T>) =
    match getRoot s with
    | None -> false
    | Some t ->
        let c = compare k t.Key

        if t.Height = 1 then
            (c = 0)
        else if c < 0 then
            contains k t.Left
        elif c = 0 then
            true
        else
            contains k t.Right

let rec iterate f (s: Set<'T>) =
    match getRoot s with
    | None -> ()
    | Some t ->
        if t.Height = 1 then
            f t.Key
        else
            iterate f t.Left
            f t.Key
            iterate f t.Right

let rec foldBack f (s: Set<'T>) x =
    match getRoot s with
    | None -> x
    | Some t ->
        if t.Height = 1 then
            f t.Key x
        else
            foldBack f t.Left (f t.Key (foldBack f t.Right x))

let rec fold f x (s: Set<'T>) =
    match getRoot s with
    | None -> x
    | Some t ->
        if t.Height = 1 then
            f x t.Key
        else
            let x = fold f x t.Left in
            let x = f x t.Key
            fold f x t.Right

let map mapping (s: Set<'T>) =
    fold (fun acc k -> add (mapping k) acc) empty s

let rec forAll f (s: Set<'T>) =
    match getRoot s with
    | None -> true
    | Some t ->
        if t.Height = 1 then
            f t.Key
        else
            f t.Key && forAll f t.Left && forAll f t.Right

let rec exists f (s: Set<'T>) =
    match getRoot s with
    | None -> false
    | Some t ->
        if t.Height = 1 then
            f t.Key
        else
            f t.Key || exists f t.Left || exists f t.Right

let isSubset a b = forAll (fun x -> contains x b) a

let isSuperset a b = isSubset b a

let isProperSubset a b =
    forAll (fun x -> contains x b) a && exists (fun x -> not (contains x a)) b

let isProperSuperset a b = isProperSubset b a

let rec filterAux f (s: Set<'T>) acc =
    match getRoot s with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            if f t.Key then
                add t.Key acc
            else
                acc
        else
            let acc =
                if f t.Key then
                    add t.Key acc
                else
                    acc

            filterAux f t.Left (filterAux f t.Right acc)

let filter f s = filterAux f s empty

let rec diffAux (s: Set<'T>) (acc: Set<'T>) =
    match acc |> getRoot with
    | None -> acc
    | Some _acc ->
        match getRoot s with
        | None -> acc
        | Some t ->
            if t.Height = 1 then
                remove t.Key acc
            else
                diffAux t.Left (diffAux t.Right (remove t.Key acc))

let difference a b = diffAux b a

let rec union (s1: Set<'T>) (s2: Set<'T>) =
    // Perf: tried bruteForce for low heights, but nothing significant
    match s1 |> getRoot with
    | None -> s2
    | Some t1 ->
        match s2 |> getRoot with
        | None -> s1
        | Some t2 ->
            if t1.Height = 1 then
                add t1.Key s2
            else if t2.Height = 1 then
                add t2.Key s1
            else if
                // Divide and Conquer:
                //   Suppose t1 is largest.
                //   Split t2 using pivot k1 into lo and hi.
                //   Union disjoint subproblems and then combine.
                t1.Height > t2.Height
            then
                let lo, _, hi = split t1.Key s2 in
                balance (union t1.Left lo) t1.Key (union t1.Right hi)
            else
                let lo, _, hi = split t2.Key s1 in
                balance (union t2.Left lo) t2.Key (union t2.Right hi)

let unionMany (sets: seq<Set<'T>>) = Seq.fold union empty sets

let rec intersectionAux b (s: Set<'T>) acc =
    match getRoot s with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            if contains t.Key b then
                add t.Key acc
            else
                acc
        else
            let acc = intersectionAux b t.Right acc

            let acc =
                if contains t.Key b then
                    add t.Key acc
                else
                    acc

            intersectionAux b t.Left acc

let intersect a b =
    if isEmpty b then
        b
    else
        intersectionAux b a empty

let intersectMany (sets: seq<Set<'T>>) = Seq.reduce intersect sets

let partition1 f k (acc1, acc2) =
    if f k then
        (add k acc1, acc2)
    else
        (acc1, add k acc2)

let rec partitionAux f (s: Set<'T>) acc =
    match getRoot s with
    | None -> acc
    | Some t ->
        if t.Height = 1 then
            partition1 f t.Key acc
        else
            let acc = partitionAux f t.Right acc
            let acc = partition1 f t.Key acc
            partitionAux f t.Left acc

let partition f s = partitionAux f s (empty, empty)

let rec minimumElementAux (s: Set<'T>) n =
    match getRoot s with
    | None -> n
    | Some t ->
        if t.Height = 1 then
            t.Key
        else
            minimumElementAux t.Left t.Key

and minimumElementOpt (s: Set<'T>) =
    match getRoot s with
    | None -> None
    | Some t ->
        if t.Height = 1 then
            Some t.Key
        else
            Some(minimumElementAux t.Left t.Key)

and maximumElementAux (s: Set<'T>) n =
    match getRoot s with
    | None -> n
    | Some t ->
        if t.Height = 1 then
            t.Key
        else
            maximumElementAux t.Right t.Key

and maximumElementOpt (s: Set<'T>) =
    match getRoot s with
    | None -> None
    | Some t ->
        if t.Height = 1 then
            Some t.Key
        else
            Some(maximumElementAux t.Right t.Key)

let minElement s =
    match minimumElementOpt s with
    | Some k -> k
    | None -> invalidArg "s" (SR.setContainsNoElements)

let maxElement s =
    match maximumElementOpt s with
    | Some k -> k
    | None -> invalidArg "s" (SR.setContainsNoElements)

// Imperative left-to-right iterators.
[<NoEquality; NoComparison>]
type SetIterator<'T> when 'T: comparison =
    {
        mutable stack: Set<'T> list // invariant: always collapseLHS result
        mutable started: bool // true when MoveNext has been called
    }

// collapseLHS:
// a) Always returns either [] or a list starting with SetOne.
// b) The "fringe" of the set stack is unchanged.
let rec collapseLHS (stack: Set<'T> list) =
    match stack with
    | [] -> []
    | s :: rest ->
        match getRoot s with
        | None -> collapseLHS rest
        | Some t ->
            if t.Height = 1 then
                stack
            else
                collapseLHS (t.Left :: mkSetTreeLeaf t.Key :: t.Right :: rest)

let mkIterator s =
    {
        stack = collapseLHS [ s ]
        started = false
    }

let notStarted () = failwith SR.enumerationNotStarted
let alreadyFinished () = failwith SR.enumerationAlreadyFinished

let current (i: SetIterator<'T>) =
    if i.started then
        match i.stack with
        | { root = Some k } :: _ -> k.Key
        | _ -> alreadyFinished ()
    else
        notStarted ()

let unexpectedStackForMoveNext () =
    failwith "Please report error: Set iterator, unexpected stack for moveNext"

let unexpectedstateInSetTreeCompareStacks () =
    failwith "unexpected state in SetTree.compareStacks"

let rec moveNext (i: SetIterator<'T>) =
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

// type SetEnumerator<'T when 'T : comparison>(s) =
//     let mutable i = mkIterator s
//     interface System.Collections.Generic.IEnumerator<'T> with
//         member _.Current: 'T = current i
//         member _.Current: obj = box (current i)
//         member _.MoveNext() = moveNext i
//         member _.Reset() = i <- mkIterator s
//         member _.Dispose() = ()

// let mkIEnumerator s =
//     new SetEnumerator<_>(s) :> System.Collections.Generic.IEnumerator<_>

// let toSeq (s: Set<'T>) =
//     let en = mkIEnumerator s
//     en |> Seq.unfold (fun en ->
//         if en.MoveNext()
//         then Some(en.Current, en)
//         else None)

// // let mkIEnumerator s =
// //     let mutable  i = mkIterator s
// //     { new IEnumerator<_> with
// //           member _.Current = current i
// //       interface IEnumerator with
// //           member _.Current = box (current i)
// //           member _.MoveNext() = moveNext i
// //           member _.Reset() = i <- mkIterator s
// //       interface System.IDisposable with
// //           member _.Dispose() = () }

// /// Set comparison.  Note this can be expensive.
// let rec compareStacks (l1: Set<'T> list) (l2: Set<'T> list): int =
//     let cont() =
//         match l1, l2 with
//         | (Some x1 :: t1), _ ->
//             if x1.Height = 1 then
//                 compareStacks (empty :: mkSetTreeLeaf x1.Key :: t1) l2
//             else
//                 compareStacks (x1.Left :: (mkSetTreeNode (x1.Key, empty, x1.Right, 0)) :: t1) l2
//         | _, (Some x2 :: t2) ->
//             if x2.Height = 1 then
//                 compareStacks l1 (empty :: mkSetTreeLeaf x2.Key :: t2)
//             else
//                 compareStacks l1 (x2.Left :: (mkSetTreeNode (x2.Key, empty, x2.Right, 0)  ) :: t2)
//         | _ -> unexpectedstateInSetTreeCompareStacks()

//     match l1, l2 with
//     | [], [] ->  0
//     | [], _  -> -1
//     | _, [] ->  1
//     | (s1 :: t1), (s2 :: t2) ->
//         match s1, s2 with
//         | None, None -> compareStacks t1 t2
//         | None, _ -> cont()
//         | _, None -> cont()
//         | Some x1, Some x2 ->
//             if x1.Height = 1 then
//                 if x2.Height = 1 then
//                     let c = compare x1.Key x2.Key
//                     if c <> 0 then c else compareStacks t1 t2
//                 else
//                     if isEmpty x2.Left then
//                         let c = compare x1.Key x2.Key
//                         if c <> 0 then c else compareStacks (empty :: t1) (x2.Right :: t2)
//                     else cont()
//             else
//                 if isEmpty x1.Left then
//                     if x2.Height = 1 then
//                         let c = compare x1.Key x2.Key
//                         if c <> 0 then c else compareStacks (x1.Right :: t1) (empty :: t2)
//                     else
//                         if isEmpty x2.Left then
//                             let c = compare x1.Key x2.Key
//                             if c <> 0 then c else compareStacks (x1.Right :: t1) (x2.Right :: t2)
//                         else cont()
//                 else cont()

// let compareTo (s1: Set<'T>) (s2: Set<'T>) =
//     if isEmpty s1 then
//         if isEmpty s2 then 0
//         else -1
//     else
//         if isEmpty s2 then 1
//         else compareStacks [s1] [s2]

let choose s = minElement s

let copyToArray s (arr: _[]) i =
    let mutable j = i

    iterate
        (fun x ->
            arr[j] <- x
            j <- j + 1
        )
        s

let toArray (s: Set<'T>) =
    let len = count s
    let res = ResizeArray<_>(len)
    iterate (fun x -> res.Add(x)) s
    res |> asArray

let toList (s: Set<'T>) = foldBack (fun k acc -> k :: acc) s []

let ofArray xs =
    Array.fold (fun acc k -> add k acc) empty xs

let ofList xs =
    List.fold (fun acc k -> add k acc) empty xs

let ofSeq xs =
    Seq.fold (fun acc k -> add k acc) empty xs

let toSeq (s: Set<'T>) =
    Seq.delay (fun () ->
        mkIterator s
        |> Seq.unfold (fun i ->
            if moveNext i then
                Some(current i, i)
            else
                None
        )
    )

let compareTo (s1: Set<'T>) (s2: Set<'T>) =
    // LanguagePrimitives.GenericComparison s1 s2
    Seq.compareWith compare (toSeq s1) (toSeq s2)

let equals (s1: Set<'T>) (s2: Set<'T>) =
    // LanguagePrimitives.GenericEquality s1 s2
    compareTo s1 s2 = 0

(*

[<Sealed>]
[<CompiledName("FSharpSet`1")>]
[<DebuggerTypeProxy(typedefof<SetDebugView<_>>)>]
[<DebuggerDisplay("Count = {Count}")>]
[<CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix")>]
type Set<[<EqualityConditionalOn>]'T when 'T: comparison >(comparer: IComparer<'T>, tree: Set<'T>) =

    [<System.NonSerialized>]
    // NOTE: This type is logically immutable. This field is only mutated during deserialization.
    let mutable = comparer

    [<System.NonSerialized>]
    // NOTE: This type is logically immutable. This field is only mutated during deserialization.
    let mutable tree = tree

    // NOTE: This type is logically immutable. This field is only mutated during serialization and deserialization.
    // WARNING: The compiled name of this field may never be changed because it is part of the logical
    // WARNING: permanent serialization format for this type.
    let mutable serializedData = null

    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).

    static let empty: Set<'T> =
        let = LanguagePrimitives.FastGenericComparer<'T>
        Set<'T>(comparer, SetTree.empty)

    [<System.Runtime.Serialization.OnSerializingAttribute>]
    member _.OnSerializing(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        serializedData <- SetTree.toArray tree

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member _.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    [<System.Runtime.Serialization.OnDeserializedAttribute>]
    member _.OnDeserialized(context: System.Runtime.Serialization.StreamingContext) =
        ignore context
        <- LanguagePrimitives.FastGenericComparer<'T>
        tree <- SetTree.ofArray serializedData
        serializedData <- null

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal set.Comparer = comparer

    member internal set.Tree: Set<'T> = tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    static member Empty: Set<'T> = empty

    member s.Add value: Set<'T> =
#if TRACE_SETS_AND_MAPS
        SetTree.report()
        SetTree.numAdds <- SetTree.numAdds + 1
        SetTree.totalSizeOnSetAdd <- SetTree.totalSizeOnSetAdd + float (SetTree.count s.Tree)
#endif
        Set<'T>(s.Comparer, SetTree.add s.Comparer value s.Tree )

    member s.Remove value: Set<'T> =
#if TRACE_SETS_AND_MAPS
        SetTree.report()
        SetTree.numRemoves <- SetTree.numRemoves + 1
#endif
        Set<'T>(s.Comparer, SetTree.remove s.Comparer value s.Tree)

    member s.Count =
        SetTree.count s.Tree

    member s.Contains value =
#if TRACE_SETS_AND_MAPS
        SetTree.report()
        SetTree.numLookups <- SetTree.numLookups + 1
        SetTree.totalSizeOnSetLookup <- SetTree.totalSizeOnSetLookup + float (SetTree.count s.Tree)
#endif
        SetTree.contains s.Comparer  value s.Tree

    member s.Iterate x =
        SetTree.iterate x s.Tree

    member s.Fold f z =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
        SetTree.fold (fun x z -> f.Invoke(z, x)) z s.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member s.IsEmpty =
        SetTree.isEmpty s.Tree

    member s.Partition f : Set<'T> *  Set<'T> =
        if SetTree.isEmpty s.Tree then s,s
        else
            let t1, t2 = SetTree.partition s.Comparer f s.Tree in Set(s.Comparer, t1), Set(s.Comparer, t2)

    member s.Filter f : Set<'T> =
        if SetTree.isEmpty s.Tree then s
        else
            Set(s.Comparer, SetTree.filter s.Comparer f s.Tree)

    member s.Map f : Set<'U> =
        let = LanguagePrimitives.FastGenericComparer<'U>
        Set(comparer, SetTree.fold (fun acc k -> SetTree.add (f k) acc) (SetTree.empty) s.Tree)

    member s.Exists f =
        SetTree.exists f s.Tree

    member s.ForAll f =
        SetTree.forAll f s.Tree

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
    static member (-) (set1: Set<'T>, set2: Set<'T>) =
        if SetTree.isEmpty set1.Tree then set1 (* 0 - B = 0 *)
        else
            if SetTree.isEmpty set2.Tree then set1 (* A - 0 = A *)
            else Set(set1.Comparer, SetTree.difference set1.Comparer set1.Tree set2.Tree)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates")>]
    static member (+) (set1: Set<'T>, set2: Set<'T>) =
#if TRACE_SETS_AND_MAPS
        SetTree.report()
        SetTree.numUnions <- SetTree.numUnions + 1
#endif
        if SetTree.isEmpty set2.Tree then set1  (* A U 0 = A *)
        else
            if SetTree.isEmpty set1.Tree then set2  (* 0 U B = B *)
            else Set(set1.Comparer, SetTree.union set1.Comparer set1.Tree set2.Tree)

    static member Intersection(a: Set<'T>, b: Set<'T>): Set<'T> =
        if SetTree.isEmpty b.Tree then b  (* A INTER 0 = 0 *)
        else
            if SetTree.isEmpty a.Tree then a (* 0 INTER B = 0 *)
            else Set(a.Comparer, SetTree.intersect a.Comparer a.Tree b.Tree)

    static member Union(sets:seq<Set<'T>>): Set<'T> =
        Seq.fold (fun s1 s2 -> s1 + s2) Set<'T>.Empty sets

    static member Intersection(sets:seq<Set<'T>>): Set<'T> =
        Seq.reduce (fun s1 s2 -> Set.Intersection(s1, s2)) sets

    static member Equality(a: Set<'T>, b: Set<'T>) =
        (SetTree.compareTo a.Comparer  a.Tree b.Tree = 0)

    static member Compare(a: Set<'T>, b: Set<'T>) =
        SetTree.compareTo a.Comparer  a.Tree b.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.Choose = SetTree.choose x.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.MinimumElement = SetTree.minElement x.Tree

    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.MaximumElement = SetTree.maxElement x.Tree

    member x.IsSubsetOf(otherSet: Set<'T>) =
        SetTree.isSubset x.Comparer x.Tree otherSet.Tree

    member x.IsSupersetOf(otherSet: Set<'T>) =
        SetTree.isSubset x.Comparer otherSet.Tree x.Tree

    member x.IsProperSubsetOf(otherSet: Set<'T>) =
        SetTree.isProperSubset x.Comparer x.Tree otherSet.Tree

    member x.IsProperSupersetOf(otherSet: Set<'T>) =
        SetTree.isProperSubset x.Comparer otherSet.Tree x.Tree

    member x.ToList () = SetTree.toList x.Tree

    member x.ToArray () = SetTree.toArray x.Tree

    member this.ComputeHashCode() =
        let combineHash x y = (x <<< 1) + y + 631
        let mutable res = 0
        for x in this do
            res <- combineHash res (hash x)
        res

    override this.GetHashCode() = this.ComputeHashCode()

    override this.Equals that =
        match that with
        | :? Set<'T> as that ->
            use e1 = (this :> seq<_>).GetEnumerator()
            use e2 = (that :> seq<_>).GetEnumerator()
            let rec loop () =
                let m1 = e1.MoveNext()
                let m2 = e2.MoveNext()
                (m1 = m2) && (not m1 || ((e1.Current = e2.Current) && loop()))
            loop()
        | _ -> false

    interface System.IComparable with
        member this.CompareTo(that: obj) = SetTree.compareTo this.Comparer this.Tree ((that :?> Set<'T>).Tree)

    interface ICollection<'T> with
        member s.Add x = ignore x; raise (new System.NotSupportedException("ReadOnlyCollection"))

        member s.Clear() = raise (new System.NotSupportedException("ReadOnlyCollection"))

        member s.Remove x = ignore x; raise (new System.NotSupportedException("ReadOnlyCollection"))

        member s.Contains x = SetTree.contains s.Comparer x s.Tree

        member s.CopyTo(arr, i) = SetTree.copyToArray s.Tree arr i

        member s.IsReadOnly = true

        member s.Count = s.Count

    interface IReadOnlyCollection<'T> with
        member s.Count = s.Count

    interface IEnumerable<'T> with
        member s.GetEnumerator() = SetTree.mkIEnumerator s.Tree

    interface IEnumerable with
        override s.GetEnumerator() = (SetTree.mkIEnumerator s.Tree :> IEnumerator)

    static member Singleton(x: 'T): Set<'T> = Set<'T>.Empty.Add x

    new (elements : seq<'T>) =
        let = LanguagePrimitives.FastGenericComparer<'T>
        Set(comparer, SetTree.ofSeq elements)

    static member Create(elements : seq<'T>) =  Set<'T>(elements)

    static member FromArray(arr : 'T array): Set<'T> =
        let = LanguagePrimitives.FastGenericComparer<'T>
        Set(comparer, SetTree.ofArray arr)

    override x.ToString() =
        match List.ofSeq (Seq.truncate 4 x) with
        | [] -> "set []"
        | [h1] ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            StringBuilder().Append("set [").Append(txt1).Append("]").ToString()
        | [h1; h2] ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            let txt2 = LanguagePrimitives.anyToStringShowingNull h2
            StringBuilder().Append("set [").Append(txt1).Append("; ").Append(txt2).Append("]").ToString()
        | [h1; h2; h3] ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            let txt2 = LanguagePrimitives.anyToStringShowingNull h2
            let txt3 = LanguagePrimitives.anyToStringShowingNull h3
            StringBuilder().Append("set [").Append(txt1).Append("; ").Append(txt2).Append("; ").Append(txt3).Append("]").ToString()
        | h1 :: h2 :: h3 :: _ ->
            let txt1 = LanguagePrimitives.anyToStringShowingNull h1
            let txt2 = LanguagePrimitives.anyToStringShowingNull h2
            let txt3 = LanguagePrimitives.anyToStringShowingNull h3
            StringBuilder().Append("set [").Append(txt1).Append("; ").Append(txt2).Append("; ").Append(txt3).Append("; ... ]").ToString()

and
    [<Sealed>]
    SetDebugView<'T when 'T : comparison>(v: Set<'T>) =

         [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
         member x.Items = v |> Seq.truncate 1000 |> Seq.toArray

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Set =

    [<CompiledName("IsEmpty")>]
    let isEmpty (set: Set<'T>) = set.IsEmpty

    [<CompiledName("Contains")>]
    let contains element (set: Set<'T>) = set.Contains element

    [<CompiledName("Add")>]
    let add value (set: Set<'T>) = set.Add value

    [<CompiledName("Singleton")>]
    let singleton value = Set<'T>.Singleton value

    [<CompiledName("Remove")>]
    let remove value (set: Set<'T>) = set.Remove value

    [<CompiledName("Union")>]
    let union (set1: Set<'T>) (set2: Set<'T>) = set1 + set2

    [<CompiledName("UnionMany")>]
    let unionMany sets = Set.Union sets

    [<CompiledName("Intersect")>]
    let intersect (set1: Set<'T>) (set2: Set<'T>) = Set<'T>.Intersection(set1, set2)

    [<CompiledName("IntersectMany")>]
    let intersectMany sets = Set.Intersection sets

    [<CompiledName("Iterate")>]
    let iterate action (set: Set<'T>) = set.Iterate action

    [<CompiledName("Empty")>]
    let empty<'T when 'T : comparison> : Set<'T> = Set<'T>.Empty

    [<CompiledName("ForAll")>]
    let forAll predicate (set: Set<'T>) = set.ForAll predicate

    [<CompiledName("Exists")>]
    let exists predicate (set: Set<'T>) = set.Exists predicate

    [<CompiledName("Filter")>]
    let filter predicate (set: Set<'T>) = set.Filter predicate

    [<CompiledName("Partition")>]
    let partition predicate (set: Set<'T>) = set.Partition predicate

    [<CompiledName("Fold")>]
    let fold<'T, 'State  when 'T : comparison> folder (state: 'State) (set: Set<'T>) = SetTree.fold folder state set.Tree

    [<CompiledName("FoldBack")>]
    let foldBack<'T, 'State when 'T : comparison> folder (set: Set<'T>) (state: 'State) = SetTree.foldBack folder set.Tree state

    [<CompiledName("Map")>]
    let map mapping (set: Set<'T>) = set.Map mapping

    [<CompiledName("Count")>]
    let count (set: Set<'T>) = set.Count

    [<CompiledName("OfList")>]
    let ofList elements = Set(List.toSeq elements)

    [<CompiledName("OfArray")>]
    let ofArray (array: 'T array) = Set<'T>.FromArray array

    [<CompiledName("ToList")>]
    let toList (set: Set<'T>) = set.ToList()

    [<CompiledName("ToArray")>]
    let toArray (set: Set<'T>) = set.ToArray()

    [<CompiledName("ToSeq")>]
    let toSeq (set: Set<'T>) = (set:> seq<'T>)

    [<CompiledName("OfSeq")>]
    let ofSeq (elements: seq<_>) = Set elements

    [<CompiledName("Difference")>]
    let difference (set1: Set<'T>) (set2: Set<'T>) = set1 - set2

    [<CompiledName("IsSubset")>]
    let isSubset (set1:Set<'T>) (set2: Set<'T>) = SetTree.isSubset set1.Comparer set1.Tree set2.Tree

    [<CompiledName("IsSuperset")>]
    let isSuperset (set1:Set<'T>) (set2: Set<'T>) = SetTree.isSubset set1.Comparer set2.Tree set1.Tree

    [<CompiledName("IsProperSubset")>]
    let isProperSubset (set1:Set<'T>) (set2: Set<'T>) = SetTree.isProperSubset set1.Comparer set1.Tree set2.Tree

    [<CompiledName("IsProperSuperset")>]
    let isProperSuperset (set1:Set<'T>) (set2: Set<'T>) = SetTree.isProperSubset set1.Comparer set2.Tree set1.Tree

    [<CompiledName("MinElement")>]
    let minElement (set: Set<'T>) = set.MinimumElement

    [<CompiledName("MaxElement")>]
    let maxElement (set: Set<'T>) = set.MaximumElement

*)
