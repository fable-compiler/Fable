// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module Set

open System.Collections.Generic
open Native

// A functional language implementation of binary trees

[<NoEquality; NoComparison>]
type SetTreeLeaf<'T>(k: 'T) =
    member _.Key = k

type SetTree<'T> = Option<SetTreeLeaf<'T>>

[<NoEquality; NoComparison>]
[<Sealed>]
type SetTreeNode<'T>(v: 'T, left: SetTree<'T>, right: SetTree<'T>, h: int) =
    inherit SetTreeLeaf<'T>(v)

    member _.Left = left
    member _.Right = right
    member _.Height = h

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SetTree =

    let empty: SetTree<'T> = None

    let inline isEmpty (t: SetTree<'T>) = t.IsNone

    let rec countAux (t: SetTree<'T>) acc =
        match t with
        | None -> acc
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                countAux tn.Left (countAux tn.Right (acc + 1))
            | _ -> acc + 1

    let count s = countAux s 0

    // #if TRACE_SETS_AND_MAPS
    //     let mutable traceCount = 0
    //     let mutable numOnes = 0
    //     let mutable numNodes = 0
    //     let mutable numAdds = 0
    //     let mutable numRemoves = 0
    //     let mutable numLookups = 0
    //     let mutable numUnions = 0
    //     let mutable totalSizeOnNodeCreation = 0.0
    //     let mutable totalSizeOnSetAdd = 0.0
    //     let mutable totalSizeOnSetLookup = 0.0

    //     let report() =
    //        traceCount <- traceCount + 1
    //        if traceCount % 10000 = 0 then
    //            System.Console.WriteLine(
    //                 "#SetOne = {0}, #SetNode = {1}, #Add = {2}, #Remove = {3}, #Unions = {4}, #Lookups = {5}, avSetSizeOnNodeCreation = {6}, avSetSizeOnSetCreation = {7}, avSetSizeOnSetLookup = {8}",
    //                 numOnes, numNodes, numAdds, numRemoves, numUnions, numLookups,
    //                 (totalSizeOnNodeCreation / float (numNodes + numOnes)),
    //                 (totalSizeOnSetAdd / float numAdds),
    //                 (totalSizeOnSetLookup / float numLookups))

    //     let SetTreeLeaf n =
    //         report()
    //         numOnes <- numOnes + 1
    //         totalSizeOnNodeCreation <- totalSizeOnNodeCreation + 1.0
    //         SetTreeLeaf n

    //     let SetTreeNode (x, l, r, h) =
    //         report()
    //         numNodes <- numNodes + 1
    //         let n = SetTreeNode (x, l, r, h)
    //         totalSizeOnNodeCreation <- totalSizeOnNodeCreation + float (count n)
    //         n
    // #endif

    let inline height (t: SetTree<'T>) =
        match t with
        | None -> 0
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn -> tn.Height
            | _ -> 1

    // #if CHECKED
    //     let rec checkInvariant (t: SetTree<'T>) =
    //         // A good sanity check, loss of balance can hit perf
    //         match t with
    //         | None -> true
    //         | Some t2 ->
    //             match t2 with
    //             | :? SetTreeNode<'T> as tn  ->
    //                 let h1 = height tn.Left
    //                 let h2 = height tn.Right
    //                 (-2 <= (h1 - h2) && (h1 - h2) <= 2) && checkInvariant tn.Left && checkInvariant tn.Right
    //             | _ -> true
    // #endif

    [<Literal>]
    let private tolerance = 2

    let mk l k r : SetTree<'T> =
        let hl = height l
        let hr = height r

        let m =
            if hl < hr then
                hr
            else
                hl

        if m = 0 then // m=0 ~ isEmpty l && isEmpty r
            SetTreeLeaf k |> Some
        else
            SetTreeNode(k, l, r, m + 1) :> SetTreeLeaf<'T> |> Some

    let rebalance (t1: SetTree<'T>) v (t2: SetTree<'T>) =
        let t1h = height t1
        let t2h = height t2

        if t2h > t1h + tolerance then // right is heavier than left
            match t2.Value with
            | :? SetTreeNode<'T> as t2' ->
                // one of the nodes must have height > height t1 + 1
                if height t2'.Left > t1h + 1 then // balance left: combination
                    match t2'.Left.Value with
                    | :? SetTreeNode<'T> as t2l ->
                        mk
                            (mk t1 v t2l.Left)
                            t2l.Key
                            (mk t2l.Right t2'.Key t2'.Right)
                    | _ -> failwith "internal error: Set.rebalance"
                else // rotate left
                    mk (mk t1 v t2'.Left) t2'.Key t2'.Right
            | _ -> failwith "internal error: Set.rebalance"
        else if t1h > t2h + tolerance then // left is heavier than right
            match t1.Value with
            | :? SetTreeNode<'T> as t1' ->
                // one of the nodes must have height > height t2 + 1
                if height t1'.Right > t2h + 1 then // balance right: combination
                    match t1'.Right.Value with
                    | :? SetTreeNode<'T> as t1r ->
                        mk
                            (mk t1'.Left t1'.Key t1r.Left)
                            t1r.Key
                            (mk t1r.Right v t2)
                    | _ -> failwith "internal error: Set.rebalance"
                else
                    mk t1'.Left t1'.Key (mk t1'.Right v t2)
            | _ -> failwith "internal error: Set.rebalance"
        else
            mk t1 v t2

    let rec add (comparer: IComparer<'T>) k (t: SetTree<'T>) : SetTree<'T> =
        match t with
        | None -> SetTreeLeaf k |> Some
        | Some t2 ->
            let c = comparer.Compare(k, t2.Key)

            match t2 with
            | :? SetTreeNode<'T> as tn ->
                if c < 0 then
                    rebalance (add comparer k tn.Left) tn.Key tn.Right
                elif c = 0 then
                    t
                else
                    rebalance tn.Left tn.Key (add comparer k tn.Right)
            | _ ->
                // nb. no check for rebalance needed for small trees, also be sure to reuse node already allocated
                let c = comparer.Compare(k, t2.Key)

                if c < 0 then
                    SetTreeNode(k, empty, t, 2) :> SetTreeLeaf<'T> |> Some
                elif c = 0 then
                    t
                else
                    SetTreeNode(k, t, empty, 2) :> SetTreeLeaf<'T> |> Some

    let rec balance comparer (t1: SetTree<'T>) k (t2: SetTree<'T>) =
        // Given t1 < k < t2 where t1 and t2 are "balanced",
        // return a balanced tree for <t1, k, t2>.
        // Recall: balance means subtrees heights differ by at most "tolerance"
        match t1 with
        | None -> add comparer k t2 // drop t1 = empty
        | Some t1' ->
            match t2 with
            | None -> add comparer k t1 // drop t2 = empty
            | Some t2' ->
                match t1' with
                | :? SetTreeNode<'T> as t1n ->
                    match t2' with
                    | :? SetTreeNode<'T> as t2n ->
                        // Have:  (t1l < k1 < t1r) < k < (t2l < k2 < t2r)
                        // Either (a) h1, h2 differ by at most 2 - no rebalance needed.
                        //        (b) h1 too small, i.e. h1+2 < h2
                        //        (c) h2 too small, i.e. h2+2 < h1
                        if t1n.Height + tolerance < t2n.Height then
                            // case: b, h1 too small
                            // push t1 into low side of t2, may increase height by 1 so rebalance
                            rebalance
                                (balance comparer t1 k t2n.Left)
                                t2n.Key
                                t2n.Right
                        elif t2n.Height + tolerance < t1n.Height then
                            // case: c, h2 too small
                            // push t2 into high side of t1, may increase height by 1 so rebalance
                            rebalance
                                t1n.Left
                                t1n.Key
                                (balance comparer t1n.Right k t2)
                        else
                            // case: a, h1 and h2 meet balance requirement
                            mk t1 k t2
                    | _ -> add comparer k (add comparer t2'.Key t1)
                | _ -> add comparer k (add comparer t1'.Key t2)

    let rec split (comparer: IComparer<'T>) pivot (t: SetTree<'T>) =
        // Given a pivot and a set t
        // Return { x in t s.t. x < pivot }, pivot in t?, { x in t s.t. x > pivot }
        match t with
        | None -> empty, false, empty
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                let c = comparer.Compare(pivot, tn.Key)

                if c < 0 then // pivot t1
                    let t11Lo, havePivot, t11Hi = split comparer pivot tn.Left
                    t11Lo, havePivot, balance comparer t11Hi tn.Key tn.Right
                elif c = 0 then // pivot is k1
                    tn.Left, true, tn.Right
                else // pivot t2
                    let t12Lo, havePivot, t12Hi = split comparer pivot tn.Right
                    balance comparer tn.Left tn.Key t12Lo, havePivot, t12Hi
            | _ ->
                let c = comparer.Compare(t2.Key, pivot)

                if c < 0 then
                    t, false, empty // singleton under pivot
                elif c = 0 then
                    empty, true, empty // singleton is    pivot
                else
                    empty, false, t // singleton over  pivot

    let rec spliceOutSuccessor (t: SetTree<'T>) =
        match t with
        | None -> failwith "internal error: Set.spliceOutSuccessor"
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                if isEmpty tn.Left then
                    tn.Key, tn.Right
                else
                    let k3, l' = spliceOutSuccessor tn.Left in
                    k3, mk l' tn.Key tn.Right
            | _ -> t2.Key, empty

    let rec remove (comparer: IComparer<'T>) k (t: SetTree<'T>) =
        match t with
        | None -> t
        | Some t2 ->
            let c = comparer.Compare(k, t2.Key)

            match t2 with
            | :? SetTreeNode<'T> as tn ->
                if c < 0 then
                    rebalance (remove comparer k tn.Left) tn.Key tn.Right
                elif c = 0 then
                    if isEmpty tn.Left then
                        tn.Right
                    elif isEmpty tn.Right then
                        tn.Left
                    else
                        let sk, r' = spliceOutSuccessor tn.Right
                        mk tn.Left sk r'
                else
                    rebalance tn.Left tn.Key (remove comparer k tn.Right)
            | _ ->
                if c = 0 then
                    empty
                else
                    t

    let rec mem (comparer: IComparer<'T>) k (t: SetTree<'T>) =
        match t with
        | None -> false
        | Some t2 ->
            let c = comparer.Compare(k, t2.Key)

            match t2 with
            | :? SetTreeNode<'T> as tn ->
                if c < 0 then
                    mem comparer k tn.Left
                elif c = 0 then
                    true
                else
                    mem comparer k tn.Right
            | _ -> (c = 0)

    let rec iter f (t: SetTree<'T>) =
        match t with
        | None -> ()
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                iter f tn.Left
                f tn.Key
                iter f tn.Right
            | _ -> f t2.Key

    let rec foldBackOpt
        (f: OptimizedClosures.FSharpFunc<_, _, _>)
        (t: SetTree<'T>)
        x
        =
        match t with
        | None -> x
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                foldBackOpt
                    f
                    tn.Left
                    (f.Invoke(tn.Key, (foldBackOpt f tn.Right x)))
            | _ -> f.Invoke(t2.Key, x)

    let foldBack f m x =
        foldBackOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) m x

    let rec foldOpt
        (f: OptimizedClosures.FSharpFunc<_, _, _>)
        x
        (t: SetTree<'T>)
        =
        match t with
        | None -> x
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                let x = foldOpt f x tn.Left in
                let x = f.Invoke(x, tn.Key)
                foldOpt f x tn.Right
            | _ -> f.Invoke(x, t2.Key)

    let fold f x m =
        foldOpt (OptimizedClosures.FSharpFunc<_, _, _>.Adapt f) x m

    let rec forall f (t: SetTree<'T>) =
        match t with
        | None -> true
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                f tn.Key && forall f tn.Left && forall f tn.Right
            | _ -> f t2.Key

    let rec exists f (t: SetTree<'T>) =
        match t with
        | None -> false
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                f tn.Key || exists f tn.Left || exists f tn.Right
            | _ -> f t2.Key

    let subset comparer a b = forall (fun x -> mem comparer x b) a

    let properSubset comparer a b =
        forall (fun x -> mem comparer x b) a
        && exists (fun x -> not (mem comparer x a)) b

    let rec filterAux comparer f (t: SetTree<'T>) acc =
        match t with
        | None -> acc
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                let acc =
                    if f tn.Key then
                        add comparer tn.Key acc
                    else
                        acc

                filterAux comparer f tn.Left (filterAux comparer f tn.Right acc)
            | _ ->
                if f t2.Key then
                    add comparer t2.Key acc
                else
                    acc

    let filter comparer f s = filterAux comparer f s empty

    let rec diffAux comparer (t: SetTree<'T>) acc =
        if isEmpty acc then
            acc
        else
            match t with
            | None -> acc
            | Some t2 ->
                match t2 with
                | :? SetTreeNode<'T> as tn ->
                    diffAux
                        comparer
                        tn.Left
                        (diffAux comparer tn.Right (remove comparer tn.Key acc))
                | _ -> remove comparer t2.Key acc

    let diff comparer a b = diffAux comparer b a

    let rec union comparer (t1: SetTree<'T>) (t2: SetTree<'T>) =
        // Perf: tried bruteForce for low heights, but nothing significant
        match t1 with
        | None -> t2
        | Some t1' ->
            match t2 with
            | None -> t1
            | Some t2' ->
                match t1' with
                | :? SetTreeNode<'T> as t1n ->
                    match t2' with
                    | :? SetTreeNode<'T> as t2n -> // (t1l < k < t1r) AND (t2l < k2 < t2r)
                        // Divide and Conquer:
                        //   Suppose t1 is largest.
                        //   Split t2 using pivot k1 into lo and hi.
                        //   Union disjoint subproblems and then combine.
                        if t1n.Height > t2n.Height then
                            let lo, _, hi = split comparer t1n.Key t2 in

                            balance
                                comparer
                                (union comparer t1n.Left lo)
                                t1n.Key
                                (union comparer t1n.Right hi)
                        else
                            let lo, _, hi = split comparer t2n.Key t1 in

                            balance
                                comparer
                                (union comparer t2n.Left lo)
                                t2n.Key
                                (union comparer t2n.Right hi)
                    | _ -> add comparer t2'.Key t1
                | _ -> add comparer t1'.Key t2

    let rec intersectionAux comparer b (t: SetTree<'T>) acc =
        match t with
        | None -> acc
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                let acc = intersectionAux comparer b tn.Right acc

                let acc =
                    if mem comparer tn.Key b then
                        add comparer tn.Key acc
                    else
                        acc

                intersectionAux comparer b tn.Left acc
            | _ ->
                if mem comparer t2.Key b then
                    add comparer t2.Key acc
                else
                    acc

    let intersection comparer a b = intersectionAux comparer b a empty

    let partition1 comparer f k (acc1, acc2) =
        if f k then
            (add comparer k acc1, acc2)
        else
            (acc1, add comparer k acc2)

    let rec partitionAux comparer f (t: SetTree<'T>) acc =
        match t with
        | None -> acc
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                let acc = partitionAux comparer f tn.Right acc
                let acc = partition1 comparer f tn.Key acc
                partitionAux comparer f tn.Left acc
            | _ -> partition1 comparer f t2.Key acc

    let partition comparer f s =
        partitionAux comparer f s (empty, empty)

    let rec minimumElementAux (t: SetTree<'T>) n =
        match t with
        | None -> n
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn -> minimumElementAux tn.Left tn.Key
            | _ -> t2.Key

    and minimumElementOpt (t: SetTree<'T>) =
        match t with
        | None -> None
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn -> Some(minimumElementAux tn.Left tn.Key)
            | _ -> Some t2.Key

    and maximumElementAux (t: SetTree<'T>) n =
        match t with
        | None -> n
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn -> maximumElementAux tn.Right tn.Key
            | _ -> t2.Key

    and maximumElementOpt (t: SetTree<'T>) =
        match t with
        | None -> None
        | Some t2 ->
            match t2 with
            | :? SetTreeNode<'T> as tn ->
                Some(maximumElementAux tn.Right tn.Key)
            | _ -> Some t2.Key

    let minimumElement s =
        match minimumElementOpt s with
        | Some k -> k
        | None -> failwith "Set contains no elements"

    let maximumElement s =
        match maximumElementOpt s with
        | Some k -> k
        | None -> failwith "Set contains no elements"

    // Imperative left-to-right iterators.
    [<NoEquality; NoComparison>]
    type SetIterator<'T> when 'T: comparison =
        {
            mutable stack: SetTree<'T> list // invariant: always collapseLHS result
            mutable started: bool // true when MoveNext has been called
        }

    // collapseLHS:
    // a) Always returns either [] or a list starting with SetOne.
    // b) The "fringe" of the set stack is unchanged.
    let rec collapseLHS (stack: SetTree<'T> list) =
        match stack with
        | [] -> []
        | x :: rest ->
            match x with
            | None -> collapseLHS rest
            | Some x2 ->
                match x2 with
                | :? SetTreeNode<'T> as xn ->
                    collapseLHS (
                        xn.Left
                        :: (SetTreeLeaf xn.Key |> Some)
                        :: xn.Right
                        :: rest
                    )
                | _ -> stack

    let mkIterator s =
        {
            stack = collapseLHS [ s ]
            started = false
        }

    let notStarted () = failwith "Enumeration not started"

    let alreadyFinished () = failwith "Enumeration already started"

    let current i =
        if i.started then
            match i.stack with
            | None :: _ ->
                failwith
                    "Please report error: Set iterator, unexpected stack for current"
            | Some t :: _ -> t.Key
            | [] -> alreadyFinished ()
        else
            notStarted ()

    let rec moveNext i =
        if i.started then
            match i.stack with
            | [] -> false
            | None :: rest ->
                failwith
                    "Please report error: Set iterator, unexpected stack for moveNext"
            | Some t :: rest ->
                match t with
                | :? SetTreeNode<'T> ->
                    failwith
                        "Please report error: Set iterator, unexpected stack for moveNext"
                | _ ->
                    i.stack <- collapseLHS rest
                    not i.stack.IsEmpty
        else
            i.started <- true // The first call to MoveNext "starts" the enumeration.
            not i.stack.IsEmpty

    let mkIEnumerator s =
        let mutable i = mkIterator s

        { new IEnumerator<'a> with
            member _.Current: 'a = current i
            member _.Current: obj = box (current i)
            member _.MoveNext() = moveNext i
            member _.Reset() = i <- mkIterator s
            member _.Dispose() = ()
        }

    /// Set comparison.  Note this can be expensive.
    let rec compareStacks
        (comparer: IComparer<'T>)
        (l1: SetTree<'T> list)
        (l2: SetTree<'T> list)
        : int
        =
        // This must be inlined to activate tail call recursion in Fable
        let inline cont () =
            match l1, l2 with
            | (Some x1 :: t1), _ ->
                match x1 with
                | :? SetTreeNode<'T> as x1n ->
                    compareStacks
                        comparer
                        (x1n.Left
                         :: (SetTreeNode(x1n.Key, empty, x1n.Right, 0)
                             :> SetTreeLeaf<'T>
                             |> Some)
                         :: t1)
                        l2
                | _ ->
                    compareStacks
                        comparer
                        (empty :: (SetTreeLeaf x1.Key |> Some) :: t1)
                        l2
            | _, (Some x2 :: t2) ->
                match x2 with
                | :? SetTreeNode<'T> as x2n ->
                    compareStacks
                        comparer
                        l1
                        (x2n.Left
                         :: (SetTreeNode(x2n.Key, empty, x2n.Right, 0)
                             :> SetTreeLeaf<'T>
                             |> Some)
                         :: t2)
                | _ ->
                    compareStacks
                        comparer
                        l1
                        (empty :: (SetTreeLeaf x2.Key |> Some) :: t2)
            | _ -> failwith "unexpected state in SetTree.compareStacks"

        match l1, l2 with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | (None :: t1), (None :: t2) -> compareStacks comparer t1 t2
        | (None :: t1), (Some x2 :: t2) -> cont ()
        | (Some x1 :: t1), (None :: t2) -> cont ()
        | (Some x1 :: t1), (Some x2 :: t2) ->
            match x1 with
            | :? SetTreeNode<'T> as x1n ->
                if isEmpty x1n.Left then
                    match x2 with
                    | :? SetTreeNode<'T> as x2n ->
                        if isEmpty x2n.Left then
                            let c = comparer.Compare(x1n.Key, x2n.Key)

                            if c <> 0 then
                                c
                            else
                                compareStacks
                                    comparer
                                    (x1n.Right :: t1)
                                    (x2n.Right :: t2)
                        else
                            cont ()
                    | _ ->
                        let c = comparer.Compare(x1n.Key, x2.Key)

                        if c <> 0 then
                            c
                        else
                            compareStacks
                                comparer
                                (x1n.Right :: t1)
                                (empty :: t2)
                else
                    cont ()
            | _ ->
                match x2 with
                | :? SetTreeNode<'T> as x2n ->
                    if isEmpty x2n.Left then
                        let c = comparer.Compare(x1.Key, x2n.Key)

                        if c <> 0 then
                            c
                        else
                            compareStacks
                                comparer
                                (empty :: t1)
                                (x2n.Right :: t2)
                    else
                        cont ()
                | _ ->
                    let c = comparer.Compare(x1.Key, x2.Key)

                    if c <> 0 then
                        c
                    else
                        compareStacks comparer t1 t2

    let compare comparer (t1: SetTree<'T>) (t2: SetTree<'T>) =
        if isEmpty t1 then
            if isEmpty t2 then
                0
            else
                -1
        else if isEmpty t2 then
            1
        else
            compareStacks comparer [ t1 ] [ t2 ]

    let choose s = minimumElement s

    let toList (t: SetTree<'T>) =
        let rec loop (t': SetTree<'T>) acc =
            match t' with
            | None -> acc
            | Some t2 ->
                match t2 with
                | :? SetTreeNode<'T> as tn ->
                    loop tn.Left (tn.Key :: loop tn.Right acc)
                | _ -> t2.Key :: acc

        loop t []

    let copyToArray s (arr: _[]) i =
        let mutable j = i

        iter
            (fun x ->
                arr.[j] <- x
                j <- j + 1
            )
            s

    let toArray s =
        let n = count s
        let res = Array.zeroCreate n
        copyToArray s res 0
        res

    let rec mkFromEnumerator comparer acc (e: IEnumerator<_>) =
        if e.MoveNext() then
            mkFromEnumerator comparer (add comparer e.Current acc) e
        else
            acc

    let ofArray comparer l =
        Array.fold (fun acc k -> add comparer k acc) empty l

    let ofList comparer l =
        List.fold (fun acc k -> add comparer k acc) empty l

    let ofSeq comparer (c: seq<'T>) =
        match c with
        | :? array<'T> as xs -> ofArray comparer xs
        | :? list<'T> as xs -> ofList comparer xs
        | _ ->
            use ie = c.GetEnumerator()
            mkFromEnumerator comparer empty ie

open Fable.Core

[<Sealed>]
[<CompiledName("FSharpSet")>]
// [<CompiledName("FSharpSet`1")>]
// [<DebuggerTypeProxy(typedefof<SetDebugView<_>>)>]
// [<DebuggerDisplay("Count = {Count}")>]
// [<CodeAnalysis.SuppressMessage("Microsoft.Naming", "CA1710:IdentifiersShouldHaveCorrectSuffix")>]
type Set<[<EqualityConditionalOn>] 'T when 'T: comparison>
    (comparer: IComparer<'T>, tree: SetTree<'T>)
    =

    // [<System.NonSerialized>]
    // NOTE: This type is logically immutable. This field is only mutated during deserialization.
    // let mutable comparer = comparer

    // [<System.NonSerialized>]
    // NOTE: This type is logically immutable. This field is only mutated during deserialization.
    // let mutable tree = tree

    // NOTE: This type is logically immutable. This field is only mutated during serialization and deserialization.
    // WARNING: The compiled name of this field may never be changed because it is part of the logical
    // WARNING: permanent serialization format for this type.
    // let mutable serializedData = null

    // We use .NET generics per-instantiation static fields to avoid allocating a new object for each empty
    // set (it is just a lookup into a .NET table of type-instantiation-indexed static fields).

    // [<System.Runtime.Serialization.OnSerializingAttribute>]
    // member _.OnSerializing(context: System.Runtime.Serialization.StreamingContext) =
    //     ignore context
    //     serializedData <- SetTree.toArray tree

    // Do not set this to null, since concurrent threads may also be serializing the data
    //[<System.Runtime.Serialization.OnSerializedAttribute>]
    //member _.OnSerialized(context: System.Runtime.Serialization.StreamingContext) =
    //    serializedData <- null

    // [<System.Runtime.Serialization.OnDeserializedAttribute>]
    // member _.OnDeserialized(context: System.Runtime.Serialization.StreamingContext) =
    //     ignore context
    //     comparer <- LanguagePrimitives.FastGenericComparer<'T>
    //     tree <- SetTree.ofArray comparer serializedData
    //     serializedData <- null

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member internal set.Comparer = comparer

    member internal set.Tree: SetTree<'T> = tree

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    static member Empty comparer : Set<'T> = Set<'T>(comparer, SetTree.empty)

    member s.Add value : Set<'T> =
        // #if TRACE_SETS_AND_MAPS
        //         SetTree.report()
        //         SetTree.numAdds <- SetTree.numAdds + 1
        //         SetTree.totalSizeOnSetAdd <- SetTree.totalSizeOnSetAdd + float (SetTree.count s.Tree)
        // #endif
        Set<'T>(s.Comparer, SetTree.add s.Comparer value s.Tree)

    member s.Remove value : Set<'T> =
        // #if TRACE_SETS_AND_MAPS
        //         SetTree.report()
        //         SetTree.numRemoves <- SetTree.numRemoves + 1
        // #endif
        Set<'T>(s.Comparer, SetTree.remove s.Comparer value s.Tree)

    member s.Count = SetTree.count s.Tree

    member s.Contains value =
        // #if TRACE_SETS_AND_MAPS
        //         SetTree.report()
        //         SetTree.numLookups <- SetTree.numLookups + 1
        //         SetTree.totalSizeOnSetLookup <- SetTree.totalSizeOnSetLookup + float (SetTree.count s.Tree)
        // #endif
        SetTree.mem s.Comparer value s.Tree

    member s.Iterate x = SetTree.iter x s.Tree

    member s.Fold f z =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
        SetTree.fold (fun x z -> f.Invoke(z, x)) z s.Tree

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member s.IsEmpty = SetTree.isEmpty s.Tree

    member s.Partition f : Set<'T> * Set<'T> =
        if SetTree.isEmpty s.Tree then
            s, s
        else
            let t1, t2 = SetTree.partition s.Comparer f s.Tree in
            Set(s.Comparer, t1), Set(s.Comparer, t2)

    member s.Filter f : Set<'T> =
        if SetTree.isEmpty s.Tree then
            s
        else
            Set(s.Comparer, SetTree.filter s.Comparer f s.Tree)

    member s.Map(f, [<Inject>] comparer: IComparer<'U>) : Set<'U> =
        Set(
            comparer,
            SetTree.fold
                (fun acc k -> SetTree.add comparer (f k) acc)
                (SetTree.empty)
                s.Tree
        )

    member s.Exists f = SetTree.exists f s.Tree

    member s.ForAll f = SetTree.forall f s.Tree

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage",
                                                      "CA2225:OperatorOverloadsHaveNamedAlternates")>]
    static member (-)(set1: Set<'T>, set2: Set<'T>) =
        if SetTree.isEmpty set1.Tree then
            set1 (* 0 - B = 0 *)
        else if SetTree.isEmpty set2.Tree then
            set1 (* A - 0 = A *)
        else
            Set(set1.Comparer, SetTree.diff set1.Comparer set1.Tree set2.Tree)

    [<System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Usage",
                                                      "CA2225:OperatorOverloadsHaveNamedAlternates")>]
    static member (+)(set1: Set<'T>, set2: Set<'T>) =
        // #if TRACE_SETS_AND_MAPS
        //         SetTree.report()
        //         SetTree.numUnions <- SetTree.numUnions + 1
        // #endif
        if SetTree.isEmpty set2.Tree then
            set1 (* A U 0 = A *)
        else if SetTree.isEmpty set1.Tree then
            set2 (* 0 U B = B *)
        else
            Set(set1.Comparer, SetTree.union set1.Comparer set1.Tree set2.Tree)

    static member Intersection(a: Set<'T>, b: Set<'T>) : Set<'T> =
        if SetTree.isEmpty b.Tree then
            b (* A INTER 0 = 0 *)
        else if SetTree.isEmpty a.Tree then
            a (* 0 INTER B = 0 *)
        else
            Set(a.Comparer, SetTree.intersection a.Comparer a.Tree b.Tree)

    // static member Union(sets:seq<Set<'T>>) : Set<'T>  =
    //     Seq.fold (fun s1 s2 -> s1 + s2) Set<'T>.Empty sets

    static member IntersectionMany(sets: seq<Set<'T>>) : Set<'T> =
        Seq.reduce (fun s1 s2 -> Set.Intersection(s1, s2)) sets

    static member Equality(a: Set<'T>, b: Set<'T>) =
        (SetTree.compare a.Comparer a.Tree b.Tree = 0)

    static member Compare(a: Set<'T>, b: Set<'T>) =
        SetTree.compare a.Comparer a.Tree b.Tree

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.Choose = SetTree.choose x.Tree

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.MinimumElement = SetTree.minimumElement x.Tree

    // [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.MaximumElement = SetTree.maximumElement x.Tree

    member x.IsSubsetOf(otherSet: Set<'T>) =
        SetTree.subset x.Comparer x.Tree otherSet.Tree

    member x.IsSupersetOf(otherSet: Set<'T>) =
        SetTree.subset x.Comparer otherSet.Tree x.Tree

    member x.IsProperSubsetOf(otherSet: Set<'T>) =
        SetTree.properSubset x.Comparer x.Tree otherSet.Tree

    member x.IsProperSupersetOf(otherSet: Set<'T>) =
        SetTree.properSubset x.Comparer otherSet.Tree x.Tree

    member x.ToList() = SetTree.toList x.Tree

    member x.ToArray() = SetTree.toArray x.Tree

    member this.ComputeHashCode() =
        let combineHash x y = (x <<< 1) + y + 631
        let mutable res = 0

        for x in this do
            res <- combineHash res (hash x)

        abs res

    override this.GetHashCode() = this.ComputeHashCode()

    override this.Equals that =
        match that with
        | :? Set<'T> as that ->
            SetTree.compare this.Comparer this.Tree that.Tree = 0
        | _ -> false

    interface Symbol_wellknown with
        member _.``Symbol.toStringTag`` = "FSharpSet"

    interface IJsonSerializable with
        member this.toJSON() = Helpers.arrayFrom (this) |> box

    interface System.IComparable with
        member s.CompareTo(that: obj) =
            SetTree.compare s.Comparer s.Tree ((that :?> Set<'T>).Tree)

    interface ICollection<'T> with
        member s.Add x =
            ignore x
            raise (System.NotSupportedException("ReadOnlyCollection"))

        member s.Clear() =
            raise (System.NotSupportedException("ReadOnlyCollection"))

        member s.Remove x =
            ignore x
            raise (System.NotSupportedException("ReadOnlyCollection"))

        member s.Contains x = SetTree.mem s.Comparer x s.Tree
        member s.CopyTo(arr, i) = SetTree.copyToArray s.Tree arr i
        member s.IsReadOnly = true
        member s.Count = s.Count

    interface IReadOnlyCollection<'T> with
        member s.Count = s.Count

    interface IEnumerable<'T> with
        member s.GetEnumerator() = SetTree.mkIEnumerator s.Tree

    interface System.Collections.IEnumerable with
        member s.GetEnumerator() =
            SetTree.mkIEnumerator s.Tree :> System.Collections.IEnumerator

    interface JS.Set<'T> with
        member s.size = s.Count

        member s.add(k) =
            failwith "Set cannot be mutated"
            s :> JS.Set<'T>

        member s.clear() =
            failwith "Set cannot be mutated"
            ()

        member s.delete(k) =
            failwith "Set cannot be mutated"
            false

        member s.has(k) = s.Contains(k)
        member s.keys() = s |> Seq.map id
        member s.values() = s |> Seq.map id
        member s.entries() = s |> Seq.map (fun v -> (v, v))
        member s.forEach(f, ?thisArg) = s |> Seq.iter (fun x -> f x x s)

    // new (elements : seq<'T>) =
    //     let comparer = LanguagePrimitives.FastGenericComparer<'T>
    //     Set(comparer, SetTree.ofSeq comparer elements)

    // static member Create(elements : seq<'T>) =  Set<'T>(elements)

    // static member FromArray(arr : 'T array) : Set<'T> =
    //     let comparer = LanguagePrimitives.FastGenericComparer<'T>
    //     Set(comparer, SetTree.ofArray comparer arr)

    override this.ToString() =
        let inline toStr (x: 'T) = x.ToString()
        let str = this |> Seq.map toStr |> String.concat "; "
        "set [" + str + "]"

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module Set =

// [<CompiledName("IsEmpty")>]
let isEmpty (set: Set<'T>) = set.IsEmpty

// [<CompiledName("Contains")>]
let contains element (set: Set<'T>) = set.Contains element

// [<CompiledName("Add")>]
let add value (set: Set<'T>) = set.Add value

// [<CompiledName("Singleton")>]
let singleton (value: 'T) ([<Inject>] comparer: IComparer<'T>) : Set<'T> =
    Set<'T>.Empty(comparer).Add value

// [<CompiledName("Remove")>]
let remove value (set: Set<'T>) = set.Remove value

// [<CompiledName("Union")>]
let union (set1: Set<'T>) (set2: Set<'T>) = set1 + set2

// [<CompiledName("UnionMany")>]
let unionMany (sets: seq<Set<'T>>) ([<Inject>] comparer: IComparer<'T>) =
    Seq.fold (+) (Set<'T>.Empty comparer) sets

// [<CompiledName("Intersect")>]
let intersect (set1: Set<'T>) (set2: Set<'T>) = Set<'T>.Intersection(set1, set2)

// [<CompiledName("IntersectMany")>]
let intersectMany (sets: seq<Set<'T>>) = Set.IntersectionMany sets

// [<CompiledName("Iterate")>]
let iterate action (set: Set<'T>) = set.Iterate action

// [<CompiledName("Empty")>]
let empty<'T when 'T: comparison>
    ([<Inject>] comparer: IComparer<'T>)
    : Set<'T>
    =
    Set<'T>.Empty comparer

// [<CompiledName("ForAll")>]
let forAll predicate (set: Set<'T>) = set.ForAll predicate

// [<CompiledName("Exists")>]
let exists predicate (set: Set<'T>) = set.Exists predicate

// [<CompiledName("Filter")>]
let filter predicate (set: Set<'T>) = set.Filter predicate

// [<CompiledName("Partition")>]
let partition predicate (set: Set<'T>) = set.Partition predicate

// [<CompiledName("Fold")>]
let fold<'T, 'State when 'T: comparison> folder (state: 'State) (set: Set<'T>) =
    SetTree.fold folder state set.Tree

// [<CompiledName("FoldBack")>]
let foldBack<'T, 'State when 'T: comparison>
    folder
    (set: Set<'T>)
    (state: 'State)
    =
    SetTree.foldBack folder set.Tree state

// [<CompiledName("Map")>]
let map mapping (set: Set<'T>) ([<Inject>] comparer: IComparer<'U>) =
    set.Map(mapping, comparer)

// [<CompiledName("Count")>]
let count (set: Set<'T>) = set.Count

// [<CompiledName("OfList")>]
let ofList elements ([<Inject>] comparer: IComparer<'T>) =
    Set(comparer, SetTree.ofSeq comparer elements)

// [<CompiledName("OfArray")>]
let ofArray (array: 'T array) ([<Inject>] comparer: IComparer<'T>) =
    Set(comparer, SetTree.ofArray comparer array)

// [<CompiledName("ToList")>]
let toList (set: Set<'T>) = set.ToList()

// [<CompiledName("ToArray")>]
let toArray (set: Set<'T>) = set.ToArray()

// [<CompiledName("ToSeq")>]
let toSeq (set: Set<'T>) = (set |> Seq.map id)

// [<CompiledName("OfSeq")>]
let ofSeq (elements: seq<_>) ([<Inject>] comparer: IComparer<'T>) =
    Set(comparer, SetTree.ofSeq comparer elements)

// [<CompiledName("Difference")>]
let difference (set1: Set<'T>) (set2: Set<'T>) = set1 - set2

// [<CompiledName("IsSubset")>]
let isSubset (set1: Set<'T>) (set2: Set<'T>) =
    SetTree.subset set1.Comparer set1.Tree set2.Tree

// [<CompiledName("IsSuperset")>]
let isSuperset (set1: Set<'T>) (set2: Set<'T>) =
    SetTree.subset set1.Comparer set2.Tree set1.Tree

// [<CompiledName("IsProperSubset")>]
let isProperSubset (set1: Set<'T>) (set2: Set<'T>) =
    SetTree.properSubset set1.Comparer set1.Tree set2.Tree

// [<CompiledName("IsProperSuperset")>]
let isProperSuperset (set1: Set<'T>) (set2: Set<'T>) =
    SetTree.properSubset set1.Comparer set2.Tree set1.Tree

// [<CompiledName("MinElement")>]
let minElement (set: Set<'T>) = set.MinimumElement

// [<CompiledName("MaxElement")>]
let maxElement (set: Set<'T>) = set.MaximumElement

// Helpers to replicate HashSet methods

let unionWith (s1: JS.Set<'T>) (s2: 'T seq) =
    (s1, s2) ||> Seq.fold (fun acc x -> acc.add x)

// If s1 is a Fable MutableSet, use the comparer. See #2566
let newMutableSetWith (s1: JS.Set<'T>) (s2: 'T seq) =
    match s1 with
    | :? Fable.Collections.MutableSet<'T> as s1 ->
        Fable.Collections.MutableSet(s2, s1.Comparer) :> JS.Set<'T>
    | _ -> JS.Constructors.Set.Create(s2)

let intersectWith (s1: JS.Set<'T>) (s2: 'T seq) =
    let s2 = newMutableSetWith s1 s2

    s1.values ()
    |> Seq.iter (fun x ->
        if not (s2.has (x)) then
            s1.delete x |> ignore
    )

let exceptWith (s1: JS.Set<'T>) (s2: 'T seq) =
    s2 |> Seq.iter (fun x -> s1.delete x |> ignore)

let isSubsetOf (s1: JS.Set<'T>) (s2: 'T seq) =
    let s2 = newMutableSetWith s1 s2
    s1.values () |> Seq.forall s2.has

let isSupersetOf (s1: JS.Set<'T>) (s2: 'T seq) = s2 |> Seq.forall s1.has

let isProperSubsetOf (s1: JS.Set<'T>) (s2: 'T seq) =
    let s2 = newMutableSetWith s1 s2
    s2.size > s1.size && s1.values () |> Seq.forall s2.has

let isProperSupersetOf (s1: JS.Set<'T>) (s2: 'T seq) =
    let s2 = Seq.cache s2
    s2 |> Seq.exists (s1.has >> not) && s2 |> Seq.forall s1.has
