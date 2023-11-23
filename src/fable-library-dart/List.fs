module ListModule

open System.Collections.Generic
open Fable.Core

type EmptyList<'T>() =
    inherit LinkedList<'T>(None)

and ConsList<'T>(head: 'T, tail: LinkedList<'T>) =
    inherit LinkedList<'T>(Some tail)
    member _.GetHead() = head

and [<CompiledName("FSharpList"); AbstractClass>] LinkedList<'T>
    (tail_: 'T list option)
    =
    let mutable tail = tail_
    static member Empty: 'T list = EmptyList<'T>()
    static member Cons(x: 'T, xs: 'T list) : 'T list = ConsList<'T>(x, xs)

    member internal _.SetConsTail(t: 'T list) = tail <- Some t

    member internal xs.AppendConsNoTail(x: 'T) : 'T list =
        let t = ConsList<'T>(x, EmptyList())
        xs.SetConsTail t
        t

    member this.IsEmpty: bool = this :? EmptyList<'T>

    member this.Head: 'T =
        match this with
        | :? ConsList<'T> as this -> this.GetHead()
        | _ -> failwith "Empty list"

    member this.TryHead: 'T option =
        match this with
        | :? ConsList<'T> as this -> this.GetHead() |> Some
        | _ -> None

    member _.Tail: LinkedList<'T> = Option.get tail

    member _.TryTail: LinkedList<'T> option = tail

    member xs.Length =
        let rec loop i (xs: 'T list) =
            match xs.TryTail with
            | None -> i
            | Some t -> loop (i + 1) t

        loop 0 xs

    member xs.Item
        with get (index) =
            let rec loop i (xs: 'T list) =
                match xs.TryTail with
                | None -> invalidArg "index" SR.indexOutOfBounds
                | Some t ->
                    if i = index then
                        xs.Head
                    else
                        loop (i + 1) t

            loop 0 xs

    override xs.ToString() =
        "[" + System.String.Join("; ", xs) + "]"

    override xs.Equals(other: obj) : bool =
        if obj.ReferenceEquals(xs, other) then
            true
        else
            let ys = other :?> 'T list

            let rec loop (xs: 'T list) (ys: 'T list) =
                match xs.TryTail, ys.TryTail with
                | None, None -> true
                | None, Some _ -> false
                | Some _, None -> false
                | Some xt, Some yt ->
                    if Unchecked.equals xs.Head ys.Head then
                        loop xt yt
                    else
                        false

            loop xs ys

    override xs.GetHashCode() : int =
        let inline combineHash i x y = (x <<< 1) + y + 631 * i
        let iMax = 18 // limit the hash

        let rec loop i h (xs: 'T list) =
            match xs.TryTail with
            | None -> h
            | Some t ->
                if i > iMax then
                    h
                else
                    loop (i + 1) (combineHash i h (Unchecked.hash xs.Head)) t

        loop 0 0 xs

    interface System.IComparable<LinkedList<'T>> with
        member xs.CompareTo(ys: LinkedList<'T>) =
            let rec loop (xs: 'T list) (ys: 'T list) =
                match xs.TryTail, ys.TryTail with
                | None, None -> 0
                | None, Some _ -> -1
                | Some _, None -> 1
                | Some xt, Some yt ->
                    let c = Unchecked.compare xs.Head ys.Head

                    if c = 0 then
                        loop xt yt
                    else
                        c

            loop xs ys

    interface IEnumerable<'T> with
        member xs.GetEnumerator() : IEnumerator<'T> =
            new ListEnumerator<'T>(xs) :> IEnumerator<'T>

        member xs.GetEnumerator() : System.Collections.IEnumerator =
            ((xs :> IEnumerable<'T>).GetEnumerator()
            :> System.Collections.IEnumerator)

and ListEnumerator<'T>(xs: 'T list) =
    let mutable it = xs
    let mutable current_ = Unchecked.defaultof<'T>

    interface IEnumerator<'T> with
        member _.Current: 'T = current_
        member _.Current: obj = current_ |> box

        member _.MoveNext() =
            match it.TryTail with
            | None -> false
            | Some t ->
                current_ <- it.Head
                it <- t
                true

        member _.Reset() = it <- xs
        member _.Dispose() = ()

and 'T list = LinkedList<'T>
and List<'T> = LinkedList<'T>

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module List =

let inline indexNotFound () =
    raise (KeyNotFoundException(SR.keyNotFoundAlt))

let empty () = List.Empty

let cons (x: 'T) (xs: 'T list) = List.Cons(x, xs)

let singleton x = List.Cons(x, List.Empty)

let isEmpty (xs: 'T list) = xs.IsEmpty

let length (xs: 'T list) = xs.Length

let head (xs: 'T list) = xs.Head

let tryHead (xs: 'T list) =
    if xs.IsEmpty then
        None
    else
        Some xs.Head

let tail (xs: 'T list) = xs.Tail

let rec tryLast (xs: 'T list) =
    if xs.IsEmpty then
        None
    else
        let t = xs.Tail

        if t.IsEmpty then
            Some xs.Head
        else
            tryLast t

let last (xs: 'T list) =
    match tryLast xs with
    | Some x -> x
    | None -> failwith SR.inputWasEmpty

let compareWith (comparer: 'T -> 'T -> int) (xs: 'T list) (ys: 'T list) : int =
    let rec loop (xs: 'T list) (ys: 'T list) =
        match xs.IsEmpty, ys.IsEmpty with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false ->
            let c = comparer xs.Head ys.Head

            if c = 0 then
                loop xs.Tail ys.Tail
            else
                c

    loop xs ys

let toArray (xs: 'T list) =
    let len = xs.Length
    let e = new ListEnumerator<'T>(xs) :> IEnumerator<'T>

    ArrayModule.Native.generate
        len
        (fun _ ->
            e.MoveNext() |> ignore
            e.Current
        )

// let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
//     if xs.IsEmpty then state
//     else fold folder (folder state xs.Head) xs.Tail

let fold<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (xs: 'T list)
    =
    let mutable acc = state
    let mutable li = xs

    while not li.IsEmpty do
        acc <- folder acc li.Head
        li <- li.Tail

    acc

let reverse (xs: 'T list) =
    fold (fun acc x -> List.Cons(x, acc)) List.Empty xs

let foldBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (xs: 'T list)
    (state: 'State)
    =
    // fold (fun acc x -> folder x acc) state (reverse xs)
    Array.foldBack folder (toArray xs) state

let foldIndexed<'T, 'State>
    (folder: int -> 'State -> 'T -> 'State)
    (state: 'State)
    (xs: 'T list)
    =
    let rec loop i acc (xs: 'T list) =
        if xs.IsEmpty then
            acc
        else
            loop (i + 1) (folder i acc xs.Head) xs.Tail

    loop 0 state xs

// let rec fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (xs: 'T1 list) (ys: 'T2 list) =
//     if xs.IsEmpty || ys.IsEmpty then state
//     else fold2 folder (folder state xs.Head ys.Head) xs.Tail ys.Tail

let fold2<'T1, 'T2, 'State>
    (folder: 'State -> 'T1 -> 'T2 -> 'State)
    (state: 'State)
    (xs: 'T1 list)
    (ys: 'T2 list)
    =
    let mutable acc = state
    let mutable xs = xs
    let mutable ys = ys

    while not xs.IsEmpty && not ys.IsEmpty do
        acc <- folder acc xs.Head ys.Head
        xs <- xs.Tail
        ys <- ys.Tail

    acc

let foldBack2<'T1, 'T2, 'State>
    (folder: 'T1 -> 'T2 -> 'State -> 'State)
    (xs: 'T1 list)
    (ys: 'T2 list)
    (state: 'State)
    =
    // fold2 (fun acc x y -> folder x y acc) state (reverse xs) (reverse ys)
    Array.foldBack2 folder (toArray xs) (toArray ys) state

let unfold<'T, 'State> (gen: 'State -> ('T * 'State) option) (state: 'State) =
    let rec loop acc (node: 'T list) =
        match gen acc with
        | None -> node
        | Some(x, acc) -> loop acc (node.AppendConsNoTail x)

    let root = List.Empty
    let node = loop state root
    node.SetConsTail List.Empty
    root.Tail

let iterate action xs = fold (fun () x -> action x) () xs

let iterate2 action xs ys =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed action xs =
    fold
        (fun i x ->
            action i x
            i + 1
        )
        0
        xs
    |> ignore

let iterateIndexed2 action xs ys =
    fold2
        (fun i x y ->
            action i x y
            i + 1
        )
        0
        xs
        ys
    |> ignore

let toSeq (xs: 'T list) : 'T seq = xs :> IEnumerable<'T>

let ofArrayWithTail (xs: 'T[]) (tail: 'T list) =
    let mutable res = tail

    for i = xs.Length - 1 downto 0 do
        res <- List.Cons(xs[i], res)

    res

let ofArray (xs: 'T[]) = ofArrayWithTail xs List.Empty

let ofSeq (xs: seq<'T>) : 'T list =
    match xs with
    | :? array<'T> as xs -> ofArray xs
    | :? list<'T> as xs -> xs
    | _ ->
        let root = List.Empty
        let mutable node = root

        for x in xs do
            node <- node.AppendConsNoTail x

        node.SetConsTail List.Empty
        root.Tail

let concat (lists: seq<'T list>) =
    let root = List.Empty
    let mutable node = root

    let action xs =
        node <- fold (fun acc x -> acc.AppendConsNoTail x) node xs

    match lists with
    | :? array<'T list> as xs -> Array.iter action xs
    | :? list<'T list> as xs -> iterate action xs
    | _ ->
        for xs in lists do
            action xs

    node.SetConsTail List.Empty
    root.Tail

let scan<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (xs: 'T list)
    =
    let root = List.Empty
    let mutable node = root.AppendConsNoTail state
    let mutable acc = state
    let mutable xs = xs

    while not xs.IsEmpty do
        acc <- folder acc xs.Head
        node <- node.AppendConsNoTail acc
        xs <- xs.Tail

    node.SetConsTail List.Empty
    root.Tail

let scanBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (xs: 'T list)
    (state: 'State)
    =
    Array.scanBack folder (toArray xs) state |> ofArray

let append (xs: 'T list) (ys: 'T list) =
    fold (fun acc x -> List.Cons(x, acc)) ys (reverse xs)

let collect (mapping: 'T -> 'U list) (xs: 'T list) =
    let root = List.Empty
    let mutable node = root
    let mutable ys = xs

    while not ys.IsEmpty do
        let mutable zs = mapping ys.Head

        while not zs.IsEmpty do
            node <- node.AppendConsNoTail zs.Head
            zs <- zs.Tail

        ys <- ys.Tail

    node.SetConsTail List.Empty
    root.Tail

let mapIndexed (mapping: int -> 'T -> 'U) (xs: 'T list) =
    let root = List.Empty
    let folder i (acc: 'U list) x = acc.AppendConsNoTail(mapping i x)
    let node = foldIndexed folder root xs
    node.SetConsTail List.Empty
    root.Tail

let map (mapping: 'T -> 'U) (xs: 'T list) =
    let root = List.Empty
    let folder (acc: 'U list) x = acc.AppendConsNoTail(mapping x)
    let node = fold folder root xs
    node.SetConsTail List.Empty
    root.Tail

let indexed xs = mapIndexed (fun i x -> (i, x)) xs

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: 'T1 list) (ys: 'T2 list) =
    let root = List.Empty
    let folder (acc: 'U list) x y = acc.AppendConsNoTail(mapping x y)
    let node = fold2 folder root xs ys
    node.SetConsTail List.Empty
    root.Tail

let mapIndexed2
    (mapping: int -> 'T1 -> 'T2 -> 'U)
    (xs: 'T1 list)
    (ys: 'T2 list)
    =
    let rec loop i (acc: 'U list) (xs: 'T1 list) (ys: 'T2 list) =
        if xs.IsEmpty || ys.IsEmpty then
            acc
        else
            let node = acc.AppendConsNoTail(mapping i xs.Head ys.Head)
            loop (i + 1) node xs.Tail ys.Tail

    let root = List.Empty
    let node = loop 0 root xs ys
    node.SetConsTail List.Empty
    root.Tail

let map3
    (mapping: 'T1 -> 'T2 -> 'T3 -> 'U)
    (xs: 'T1 list)
    (ys: 'T2 list)
    (zs: 'T3 list)
    =
    let rec loop (acc: 'U list) (xs: 'T1 list) (ys: 'T2 list) (zs: 'T3 list) =
        if xs.IsEmpty || ys.IsEmpty || zs.IsEmpty then
            acc
        else
            let node = acc.AppendConsNoTail(mapping xs.Head ys.Head zs.Head)
            loop node xs.Tail ys.Tail zs.Tail

    let root = List.Empty
    let node = loop root xs ys zs
    node.SetConsTail List.Empty
    root.Tail

let mapFold<'T, 'State, 'Result>
    (mapping: 'State -> 'T -> 'Result * 'State)
    (state: 'State)
    (xs: 'T list)
    =
    let folder (node: 'Result list, st) x =
        let r, st = mapping st x
        node.AppendConsNoTail r, st

    let root = List.Empty
    let node, state = fold folder (root, state) xs
    node.SetConsTail List.Empty
    root.Tail, state

let mapFoldBack<'T, 'State, 'Result>
    (mapping: 'T -> 'State -> 'Result * 'State)
    (xs: 'T list)
    (state: 'State)
    =
    mapFold (fun acc x -> mapping x acc) state (reverse xs)

let tryPick f xs =
    let rec loop (xs: 'T list) =
        if xs.IsEmpty then
            None
        else
            match f xs.Head with
            | Some _ as res -> res
            | None -> loop xs.Tail

    loop xs

let pick f xs =
    match tryPick f xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFind f xs =
    tryPick
        (fun x ->
            if f x then
                Some x
            else
                None
        )
        xs

let find f xs =
    match tryFind f xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindBack f xs = xs |> toArray |> Array.tryFindBack f

let findBack f xs =
    match tryFindBack f xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindIndex f xs : int option =
    let rec loop i (xs: 'T list) =
        if xs.IsEmpty then
            None
        else if f xs.Head then
            Some i
        else
            loop (i + 1) xs.Tail

    loop 0 xs

let findIndex f xs : int =
    match tryFindIndex f xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindIndexBack f xs : int option =
    xs |> toArray |> Array.tryFindIndexBack f

let findIndexBack f xs : int =
    match tryFindIndexBack f xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryItem n (xs: 'T list) =
    let rec loop i (xs: 'T list) =
        if xs.IsEmpty then
            None
        else if i = n then
            Some xs.Head
        else
            loop (i + 1) xs.Tail

    loop 0 xs

let item n (xs: 'T list) = xs.Item(n)

let filter f (xs: 'T list) =
    let root = List.Empty

    let folder (acc: 'T list) x =
        if f x then
            acc.AppendConsNoTail x
        else
            acc

    let node = fold folder root xs
    node.SetConsTail List.Empty
    root.Tail

let partition f (xs: 'T list) =
    let root1, root2 = List.Empty, List.Empty

    let folder (lacc: 'T list, racc: 'T list) x =
        if f x then
            lacc.AppendConsNoTail x, racc
        else
            lacc, racc.AppendConsNoTail x

    let node1, node2 = fold folder (root1, root2) xs
    node1.SetConsTail List.Empty
    node2.SetConsTail List.Empty
    root1.Tail, root2.Tail

let choose<'T, 'U> (f: 'T -> 'U option) (xs: 'T list) =
    let root = List.Empty

    let folder (acc: 'U list) x =
        match f x with
        | Some y -> acc.AppendConsNoTail y
        | None -> acc

    let node = fold folder root xs
    node.SetConsTail List.Empty
    root.Tail

let contains (value: 'T) (xs: 'T list) ([<Inject>] eq: IEqualityComparer<'T>) =
    tryFindIndex (fun v -> eq.Equals(value, v)) xs |> Option.isSome

let initialize n (f: int -> 'T) =
    let root = List.Empty
    let mutable node = root

    for i = 0 to n - 1 do
        node <- node.AppendConsNoTail(f i)

    node.SetConsTail List.Empty
    root.Tail

let replicate n x = initialize n (fun _ -> x)

let reduce f (xs: 'T list) =
    if xs.IsEmpty then
        invalidOp SR.inputWasEmpty
    else
        fold f (head xs) (tail xs)

let reduceBack f (xs: 'T list) =
    if xs.IsEmpty then
        invalidOp SR.inputWasEmpty
    else
        foldBack f (tail xs) (head xs)

let forAll f xs = fold (fun acc x -> acc && f x) true xs

let forAll2 f xs ys =
    fold2 (fun acc x y -> acc && f x y) true xs ys

let exists f xs = tryFindIndex f xs |> Option.isSome

let rec exists2 (f: 'T1 -> 'T2 -> bool) (xs: 'T1 list) (ys: 'T2 list) =
    match xs.IsEmpty, ys.IsEmpty with
    | true, true -> false
    | false, false -> f xs.Head ys.Head || exists2 f xs.Tail ys.Tail
    | _ -> invalidArg "list2" SR.differentLengths

let unzip xs =
    foldBack
        (fun (x, y) (lacc, racc) -> List.Cons(x, lacc), List.Cons(y, racc))
        xs
        (List.Empty, List.Empty)

let unzip3 xs =
    foldBack
        (fun (x, y, z) (lacc, macc, racc) ->
            List.Cons(x, lacc), List.Cons(y, macc), List.Cons(z, racc)
        )
        xs
        (List.Empty, List.Empty, List.Empty)

let zip xs ys = map2 (fun x y -> x, y) xs ys

let zip3 xs ys zs = map3 (fun x y z -> x, y, z) xs ys zs

let sortWith (comparer: 'T -> 'T -> int) (xs: 'T list) =
    let arr = toArray xs
    Array.sortInPlaceWith comparer arr // Note: In JS this sort is stable
    arr |> ofArray

let sort (xs: 'T list) ([<Inject>] comparer: IComparer<'T>) =
    sortWith (fun x y -> comparer.Compare(x, y)) xs

let sortBy
    (projection: 'T -> 'U)
    (xs: 'T list)
    ([<Inject>] comparer: IComparer<'U>)
    =
    sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortDescending (xs: 'T list) ([<Inject>] comparer: IComparer<'T>) =
    sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

let sortByDescending
    (projection: 'T -> 'U)
    (xs: 'T list)
    ([<Inject>] comparer: IComparer<'U>)
    =
    sortWith (fun x y -> comparer.Compare(projection x, projection y) * -1) xs

let sum (xs: 'T list) ([<Inject>] adder: IGenericAdder<'T>) : 'T =
    fold (fun acc x -> adder.Add(acc, x)) (adder.GetZero()) xs

let sumBy
    (f: 'T -> 'U)
    (xs: 'T list)
    ([<Inject>] adder: IGenericAdder<'U>)
    : 'U
    =
    fold (fun acc x -> adder.Add(acc, f x)) (adder.GetZero()) xs

let maxBy (projection: 'T -> 'U) xs ([<Inject>] comparer: IComparer<'U>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                y
            else
                x
        )
        xs

let max xs ([<Inject>] comparer: IComparer<'T>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                y
            else
                x
        )
        xs

let minBy (projection: 'T -> 'U) xs ([<Inject>] comparer: IComparer<'U>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                x
            else
                y
        )
        xs

let min (xs: 'T list) ([<Inject>] comparer: IComparer<'T>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                x
            else
                y
        )
        xs

let average (xs: 'T list) ([<Inject>] averager: IGenericAverager<'T>) : 'T =
    let mutable count = 0

    let folder acc x =
        count <- count + 1
        averager.Add(acc, x)

    let total = fold folder (averager.GetZero()) xs
    averager.DivideByInt(total, count)

let averageBy
    (f: 'T -> 'U)
    (xs: 'T list)
    ([<Inject>] averager: IGenericAverager<'U>)
    : 'U
    =
    let mutable count = 0

    let inline folder acc x =
        count <- count + 1
        averager.Add(acc, f x)

    let total = fold folder (averager.GetZero()) xs
    averager.DivideByInt(total, count)

let permute f (xs: 'T list) =
    toArray xs |> Array.permute f |> ofArray

let chunkBySize (chunkSize: int) (xs: 'T list) : 'T list list =
    toArray xs |> Array.chunkBySize chunkSize |> Array.map ofArray |> ofArray

let allPairs (xs: 'T1 list) (ys: 'T2 list) : ('T1 * 'T2) list =
    let root = List.Empty
    let mutable node = root

    iterate
        (fun x -> iterate (fun y -> node <- node.AppendConsNoTail(x, y)) ys)
        xs

    node.SetConsTail List.Empty
    root.Tail

let rec skip count (xs: 'T list) =
    if count <= 0 then
        xs
    elif xs.IsEmpty then
        invalidArg "list" SR.notEnoughElements
    else
        skip (count - 1) xs.Tail

let rec skipWhile predicate (xs: 'T list) =
    if xs.IsEmpty then
        xs
    elif not (predicate xs.Head) then
        xs
    else
        skipWhile predicate xs.Tail

let take count (xs: 'T list) =
    if count < 0 then
        invalidArg "count" SR.inputMustBeNonNegative

    let rec loop i (acc: 'T list) (xs: 'T list) =
        if i <= 0 then
            acc
        elif xs.IsEmpty then
            invalidArg "list" SR.notEnoughElements
        else
            loop (i - 1) (acc.AppendConsNoTail xs.Head) xs.Tail

    let root = List.Empty
    let node = loop count root xs
    node.SetConsTail List.Empty
    root.Tail

let takeWhile predicate (xs: 'T list) =
    let rec loop (acc: 'T list) (xs: 'T list) =
        if xs.IsEmpty then
            acc
        elif not (predicate xs.Head) then
            acc
        else
            loop (acc.AppendConsNoTail xs.Head) xs.Tail

    let root = List.Empty
    let node = loop root xs
    node.SetConsTail List.Empty
    root.Tail

let truncate count (xs: 'T list) =
    let rec loop i (acc: 'T list) (xs: 'T list) =
        if i <= 0 then
            acc
        elif xs.IsEmpty then
            acc
        else
            loop (i - 1) (acc.AppendConsNoTail xs.Head) xs.Tail

    let root = List.Empty
    let node = loop count root xs
    node.SetConsTail List.Empty
    root.Tail

let getSlice (startIndex: int option) (endIndex: int option) (xs: 'T list) =
    let len = length xs

    let startIndex =
        let index = defaultArg startIndex 0

        if index < 0 then
            0
        else
            index

    let endIndex =
        let index = defaultArg endIndex (len - 1)

        if index >= len then
            len - 1
        else
            index

    if endIndex < startIndex then
        List.Empty
    else
        xs |> skip startIndex |> take (endIndex - startIndex + 1)

let splitAt index (xs: 'T list) =
    if index < 0 then
        invalidArg "index" SR.inputMustBeNonNegative

    if index > xs.Length then
        invalidArg "index" SR.notEnoughElements

    take index xs, skip index xs

let exactlyOne (xs: 'T list) =
    if xs.IsEmpty then
        invalidArg "list" SR.inputSequenceEmpty
    else if xs.Tail.IsEmpty then
        xs.Head
    else
        invalidArg "list" SR.inputSequenceTooLong

let tryExactlyOne (xs: 'T list) =
    if not (xs.IsEmpty) && xs.Tail.IsEmpty then
        Some(xs.Head)
    else
        None

let where predicate (xs: 'T list) = filter predicate xs

let pairwise (xs: 'T list) = toArray xs |> Array.pairwise |> ofArray

let windowed (windowSize: int) (xs: 'T list) : 'T list list =
    toArray xs |> Array.windowed windowSize |> Array.map ofArray |> ofArray

let splitInto (chunks: int) (xs: 'T list) : 'T list list =
    toArray xs |> Array.splitInto chunks |> Array.map ofArray |> ofArray

let transpose (lists: seq<'T list>) : 'T list list =
    lists
    |> Array.ofSeq
    |> Array.map toArray
    |> Array.transpose
    |> Array.map ofArray
    |> ofArray

// let init = initialize
// let iter = iterate
// let iter2 = iterate2
// let iteri = iterateIndexed
// let iteri2 = iterateIndexed2
// let forall = forAll
// let forall2 = forAll2
// let mapi = mapIndexed
// let mapi2 = mapIndexed2
// let rev = reverse

let insertAt (index: int) (y: 'T) (xs: 'T list) : 'T list =
    let mutable i = -1
    let mutable isDone = false

    let result =
        (List.Empty, xs)
        ||> fold (fun acc x ->
            i <- i + 1

            if i = index then
                isDone <- true
                List.Cons(x, List.Cons(y, acc))
            else
                List.Cons(x, acc)
        )

    let result =
        if isDone then
            result
        elif i + 1 = index then
            List.Cons(y, result)
        else
            invalidArg "index" SR.indexOutOfBounds

    reverse result

let insertManyAt (index: int) (ys: seq<'T>) (xs: 'T list) : 'T list =
    let mutable i = -1
    let mutable isDone = false
    let ys = ofSeq ys

    let result =
        (List.Empty, xs)
        ||> fold (fun acc x ->
            i <- i + 1

            if i = index then
                isDone <- true
                List.Cons(x, append ys acc)
            else
                List.Cons(x, acc)
        )

    let result =
        if isDone then
            result
        elif i + 1 = index then
            append ys result
        else
            invalidArg "index" SR.indexOutOfBounds

    reverse result

let removeAt (index: int) (xs: 'T list) : 'T list =
    let mutable i = -1
    let mutable isDone = false

    let ys =
        xs
        |> filter (fun _ ->
            i <- i + 1

            if i = index then
                isDone <- true
                false
            else
                true
        )

    if not isDone then
        invalidArg "index" SR.indexOutOfBounds

    ys

let removeManyAt (index: int) (count: int) (xs: 'T list) : 'T list =
    let mutable i = -1
    // incomplete -1, in-progress 0, complete 1
    let mutable status = -1

    let ys =
        xs
        |> filter (fun _ ->
            i <- i + 1

            if i = index then
                status <- 0
                false
            elif i > index then
                if i < index + count then
                    false
                else
                    status <- 1
                    true
            else
                true
        )

    let status =
        if status = 0 && i + 1 = index + count then
            1
        else
            status

    if status < 1 then
        // F# always says the wrong parameter is index but the problem may be count
        let arg =
            if status < 0 then
                "index"
            else
                "count"

        invalidArg arg SR.indexOutOfBounds

    ys

let updateAt (index: int) (y: 'T) (xs: 'T list) : 'T list =
    let mutable isDone = false

    let ys =
        xs
        |> mapIndexed (fun i x ->
            if i = index then
                isDone <- true
                y
            else
                x
        )

    if not isDone then
        invalidArg "index" SR.indexOutOfBounds

    ys
