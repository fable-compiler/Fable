module LinkedList

open Fable.Core

module SR =
    let indexOutOfBounds = "The index was outside the range of elements in the list."
    let inputListWasEmpty = "List was empty"
    let inputMustBeNonNegative = "The input must be non-negative."
    let inputSequenceEmpty = "The input sequence was empty."
    let inputSequenceTooLong = "The input sequence contains more than one element."
    let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
    let listsHadDifferentLengths = "The lists had different lengths."
    let notEnoughElements = "The input sequence has an insufficient number of elements."

[<Struct>]
[<CustomEquality; CustomComparison>]
// [<CompiledName("FSharpList`1")>]
type List<'T when 'T: comparison> =
    { head: 'T; mutable tail: List<'T> option }

    static member inline Empty: List<'T> = { head = Unchecked.defaultof<'T>; tail = None }
    static member inline Cons (x: 'T, xs: 'T list) = { head = x; tail = Some xs }

    static member inline internal ConsNoTail (x: 'T) = { head = x; tail = None }
    member inline internal xs.SetConsTail (t: 'T list) = xs.tail <- Some t
    member inline internal xs.AppendConsNoTail (x: 'T) =
        let t = List.ConsNoTail x
        xs.SetConsTail t
        t

    member inline xs.IsEmpty = xs.tail.IsNone

    member xs.Length =
        let rec loop i xs =
            match xs.tail with
            | None -> i
            | Some t -> loop (i + 1) t
        loop 0 xs

    member xs.Head =
        match xs.tail with
        | None -> invalidArg "list" SR.inputListWasEmpty
        | Some _ -> xs.head

    member xs.Tail =
        match xs.tail with
        | None -> invalidArg "list" SR.inputListWasEmpty
        | Some t -> t

    member xs.Item with get (index) =
        let rec loop i xs =
            match xs.tail with
            | None -> invalidArg "index" SR.indexOutOfBounds
            | Some t ->
                if i = index then xs.head
                else loop (i + 1) t
        loop 0 xs

    override xs.ToString() =
        "[" + System.String.Join("; ", xs) + "]"

    override xs.Equals(other: obj) =
        if obj.ReferenceEquals(xs, other)
        then true
        else
            let ys = other :?> 'T list
            let rec loop xs ys =
                match xs.tail, ys.tail with
                | None, None -> true
                | None, Some _ -> false
                | Some _, None -> false
                | Some xt, Some yt ->
                    if xs.head = ys.head
                    then loop xt yt
                    else false
            loop xs ys

    override xs.GetHashCode() =
        let inline combineHash i x y = (x <<< 1) + y + 631 * i
        let iMax = 18 // limit the hash
        let rec loop i h (xs: 'T list) =
            match xs.tail with
            | None -> h
            | Some t ->
                if i > iMax then h
                else loop (i + 1) (combineHash i h (hash xs.head)) t
        loop 0 0 xs

    interface System.IComparable with
        member xs.CompareTo(other: obj) =
            let ys = other :?> 'T list
            let rec loop xs ys =
                match xs.tail, ys.tail with
                | None, None -> 0
                | None, Some _ -> -1
                | Some _, None -> 1
                | Some xt, Some yt ->
                    let c = compare xs.head ys.head
                    if c = 0 then loop xt yt else c
            loop xs ys

    interface System.Collections.Generic.IEnumerable<'T> with
        member xs.GetEnumerator(): System.Collections.Generic.IEnumerator<'T> =
            new ListEnumerator<'T>(xs) :> System.Collections.Generic.IEnumerator<'T>

    interface System.Collections.IEnumerable with
        member xs.GetEnumerator(): System.Collections.IEnumerator =
            ((xs :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator)

and ListEnumerator<'T when 'T: comparison>(xs: List<'T>) =
    let mutable it = xs
    let mutable current = Unchecked.defaultof<'T>
    interface System.Collections.Generic.IEnumerator<'T> with
        member __.Current = current
    interface System.Collections.IEnumerator with
        member __.Current = box (current)
        member __.MoveNext() =
            match it.tail with
            | None -> false
            | Some t ->
                current <- it.head
                it <- t
                true
        member __.Reset() =
            it <- xs
            current <- Unchecked.defaultof<'T>
    interface System.IDisposable with
        member __.Dispose() = ()

and 'T list when 'T: comparison = List<'T>

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module List =

let inline indexNotFound() = raise (System.Collections.Generic.KeyNotFoundException(SR.keyNotFoundAlt))

let empty () = List.Empty

let cons (x: 'T) (xs: 'T list) = List.Cons(x, xs)

let singleton x = List.Cons(x, List.Empty)

let isEmpty (xs: 'T list) = xs.IsEmpty

let length (xs: 'T list) = xs.Length

let head (xs: 'T list) = xs.Head

let tryHead (xs: 'T list) =
    if xs.IsEmpty then None
    else Some xs.Head

let tail (xs: 'T list) = xs.Tail

let rec tryLast (xs: 'T list) =
    if xs.IsEmpty then None
    else
        let t = xs.Tail
        if t.IsEmpty then Some xs.Head
        else tryLast t

let last (xs: 'T list) =
    match tryLast xs with
    | Some x -> x
    | None -> failwith SR.inputListWasEmpty

let compareWith (comparer: 'T -> 'T -> int) (xs: 'T list) (ys: 'T list): int =
    let rec loop (xs: 'T list) (ys: 'T list) =
        match xs.IsEmpty, ys.IsEmpty with
        | true, true -> 0
        | true, false -> -1
        | false, true -> 1
        | false, false ->
            let c = comparer xs.Head ys.Head
            if c = 0 then loop xs.Tail ys.Tail else c
    loop xs ys

let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
    let rec loop acc (ys: 'T list) =
        if ys.IsEmpty then acc
        else loop (folder acc ys.Head) ys.Tail
    loop state xs

let foldBack (folder: 'T -> 'State -> 'State) (xs: 'T list) (state: 'State) =
    Seq.foldBack folder xs state

let foldIndexed (folder: int -> 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
    let rec loop i acc (ys: 'T list) =
        if ys.IsEmpty then acc
        else loop (i + 1) (folder i acc ys.Head) ys.Tail
    loop 0 state xs

let reverse (xs: 'T list) =
    fold (fun acc x -> List.Cons(x, acc)) List.Empty xs

let toSeq (xs: 'T list): 'T seq =
    xs :> System.Collections.Generic.IEnumerable<'T>

let ofArrayWithTail (xs: System.Collections.Generic.IList<'T>) (tail: 'T list) =
    let mutable res = tail
    for i = xs.Count - 1 downto 0 do
        res <- List.Cons(xs.[i], res)
    res

let ofArray (xs: System.Collections.Generic.IList<'T>) =
    ofArrayWithTail xs List.Empty

let ofSeq (xs: seq<'T>): 'T list =
    match xs with
    | :? list<'T> as lst -> lst
    | :? array<'T> as arr -> ofArray arr
    | _ ->
        let root = List.Empty
        let mutable node = root
        for x in xs do
            node <- node.AppendConsNoTail x
        node.SetConsTail List.Empty
        root.Tail

let concat (lists: seq<'T list>) =
    Seq.fold (fold (fun acc x -> List.Cons(x, acc))) List.Empty lists
    |> reverse

let fold2 f (state: 'State) (xs: 'T list) (ys: 'U list) =
    Seq.fold2 f state xs ys

let foldBack2 f (xs: 'T list) (ys: 'U list) (state: 'State) =
    Seq.foldBack2 f xs ys state

let unfold (gen: 'State -> ('T * 'State) option) (state: 'State) =
    let rec loop st (node: 'T list) =
        match gen st with
        | None -> node.SetConsTail List.Empty
        | Some (x, st) -> loop st (node.AppendConsNoTail x)
    let root = List.Empty
    loop state root
    root.Tail

let scan f (state: 'State) (xs: 'T list) =
    Seq.scan f state xs |> ofSeq

let scanBack f (xs: 'T list) (state: 'State) =
    Seq.scanBack f xs state |> ofSeq

let append (xs: 'T list) (ys: 'T list) =
    fold (fun acc x -> List.Cons(x, acc)) ys (reverse xs)

let collect (f: 'T -> 'U list) (xs: 'T list) =
    let root = List.Empty
    let mutable node = root
    let mutable ys = xs
    while not ys.IsEmpty do
        let mutable zs = f ys.Head
        while not zs.IsEmpty do
            node <- node.AppendConsNoTail zs.Head
            zs <- zs.Tail
        ys <- ys.Tail
    node.SetConsTail List.Empty
    root.Tail

let mapIndexed (f: int -> 'T -> 'U) (xs: 'T list) =
    let root = List.Empty
    let folder i (acc: 'U list) x = acc.AppendConsNoTail (f i x)
    let node = foldIndexed folder root xs
    node.SetConsTail List.Empty
    root.Tail

let map (f: 'T -> 'U) (xs: 'T list) =
    mapIndexed (fun i x -> f x) xs

let indexed xs =
    mapIndexed (fun i x -> (i, x)) xs

let map2 f xs ys =
    Seq.map2 f xs ys |> ofSeq

let mapIndexed2 f xs ys =
    Seq.mapi2 f xs ys |> ofSeq

let map3 f xs ys zs =
    Seq.map3 f xs ys zs |> ofSeq

let mapFold (f: 'S -> 'T -> 'R * 'S) s xs =
    let folder (nxs, fs) x =
        let nx, fs = f fs x
        List.Cons(nx, nxs), fs
    let nxs, s = fold folder (List.Empty, s) xs
    reverse nxs, s

let mapFoldBack (f: 'T -> 'S -> 'R * 'S) xs s =
    mapFold (fun s v -> f v s) s (reverse xs)

let iterate f xs =
    fold (fun () x -> f x) () xs

let iterate2 f xs ys =
    fold2 (fun () x y -> f x y) () xs ys

let iterateIndexed f xs =
    fold (fun i x -> f i x; i + 1) 0 xs |> ignore

let iterateIndexed2 f xs ys =
    fold2 (fun i x y -> f i x y; i + 1) 0 xs ys |> ignore

let tryPickIndexed (f: int -> 'T -> 'U option) (xs: 'T list) =
    let rec loop i (ys: 'T list) =
        if ys.IsEmpty then None
        else
            match f i ys.Head with
            | Some _  as res -> res
            | None -> loop (i + 1) ys.Tail
    loop 0 xs

let tryPickIndexedBack (f: int -> 'T -> 'U option) (xs: 'T list) =
    let rec loop i acc (ys: 'T list) =
        if ys.IsEmpty then acc
        else
            let result =
                match f i ys.Head with
                | Some _ as res -> res
                | None -> acc
            loop (i + 1) result ys.Tail
    loop 0 None xs

let tryPick f xs =
    tryPickIndexed (fun _ x -> f x) xs

let pick f xs =
    match tryPick f xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindIndexed f xs =
    tryPickIndexed (fun i x -> if f i x then Some x else None) xs

let tryFindIndexedBack f xs =
    tryPickIndexedBack (fun i x -> if f i x then Some x else None) xs

let findIndexed f xs =
    match tryFindIndexed f xs with
    | None -> indexNotFound()
    | Some x -> x

let findIndexedBack f xs =
    match tryFindIndexedBack f xs with
    | None -> indexNotFound()
    | Some x -> x

let find f xs =
    findIndexed (fun _ x -> f x) xs

let findBack f xs =
    findIndexedBack (fun _ x -> f x) xs

let tryFind f xs =
    tryPickIndexed (fun _ x -> if f x then Some x else None) xs

let tryFindBack f xs =
    tryPickIndexedBack (fun _ x -> if f x then Some x else None) xs

let tryFindIndex f xs: int option =
    tryPickIndexed (fun i x -> if f x then Some i else None) xs

let tryFindIndexBack f xs: int option =
    tryPickIndexedBack (fun i x -> if f x then Some i else None) xs

let findIndex f xs: int =
    match tryFindIndex f xs with
    | None -> indexNotFound()
    | Some x -> x

let findIndexBack f xs: int =
    match tryFindIndexBack f xs with
    | None -> indexNotFound()
    | Some x -> x

let tryItem n (xs: 'T list) =
    let rec loop i (ys: 'T list) =
        if ys.IsEmpty then None
        else
            if i = n then Some ys.Head
            else loop (i + 1) ys.Tail
    loop 0 xs

let item n (xs: 'T list) = xs.Item(n)

let filter f xs =
    fold (fun acc x ->
        if f x
        then List.Cons(x, acc)
        else acc) List.Empty xs
    |> reverse

let partition f xs =
    fold (fun (lacc, racc) x ->
        if f x then List.Cons(x, lacc), racc
        else lacc, List.Cons(x, racc)) (List.Empty, List.Empty) (reverse xs)

let choose f xs =
    fold (fun acc x ->
        match f x with
        | Some y -> List.Cons(y, acc)
        | None -> acc) List.Empty xs
    |> reverse

let contains (value: 'T) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    tryFindIndex (fun v -> eq.Equals (value, v)) xs |> Option.isSome

let except (itemsToExclude: seq<'T>) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    if xs.IsEmpty then xs
    else
        let cached = System.Collections.Generic.HashSet(itemsToExclude, eq)
        xs |> filter cached.Add

let initialize n f =
    let mutable res = List.Empty
    for i = 0 to n - 1 do
        res <- List.Cons(f i, res)
    res |> reverse

let replicate n x =
    initialize n (fun _ -> x)

let reduce f (xs: 'T list) =
    if xs.IsEmpty then invalidOp SR.inputListWasEmpty
    else fold f (head xs) (tail xs)

let reduceBack f (xs: 'T list) =
    if xs.IsEmpty then invalidOp SR.inputListWasEmpty
    else foldBack f (tail xs) (head xs)

let forAll f xs =
    fold (fun acc x -> acc && f x) true xs

let forAll2 f xs ys =
    fold2 (fun acc x y -> acc && f x y) true xs ys

let exists f xs =
    tryFindIndex f xs |> Option.isSome

let rec exists2 f (xs: 'T list) (ys: 'U list) =
    match xs.IsEmpty, ys.IsEmpty with
    | true, true -> false
    | false, false -> f xs.Head ys.Head || exists2 f xs.Tail ys.Tail
    | _ -> invalidArg "list2" SR.listsHadDifferentLengths

let unzip xs =
    foldBack (fun (x, y) (lacc, racc) -> List.Cons(x, lacc), List.Cons(y, racc)) xs (List.Empty, List.Empty)

let unzip3 xs =
    foldBack (fun (x, y, z) (lacc, macc, racc) -> List.Cons(x, lacc), List.Cons(y, macc), List.Cons(z, racc)) xs (List.Empty, List.Empty, List.Empty)

let zip xs ys =
    map2 (fun x y -> x, y) xs ys

let zip3 xs ys zs =
    map3 (fun x y z -> x, y, z) xs ys zs

let sortWith (comparison: 'T -> 'T -> int) (xs: 'T list): 'T list =
    let values = ResizeArray(xs)
    values.Sort(System.Comparison<_>(comparison))
    values |> ofSeq

let sort (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T list =
    sortWith (fun x y -> comparer.Compare(x, y)) xs

let sortBy (projection: 'T -> 'U) (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T list =
    sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortDescending (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T list =
    sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

let sortByDescending (projection: 'T -> 'U) (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T list =
    sortWith (fun x y -> comparer.Compare(projection x, projection y) * -1) xs

let sum (xs: 'T list) ([<Inject>] adder: IGenericAdder<'T>): 'T =
    fold (fun acc x -> adder.Add(acc, x)) (adder.GetZero()) xs

let sumBy (f: 'T -> 'U) (xs: 'T list) ([<Inject>] adder: IGenericAdder<'U>): 'U =
    fold (fun acc x -> adder.Add(acc, f x)) (adder.GetZero()) xs

let maxBy (projection: 'T -> 'U) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

let max xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) xs

let minBy (projection: 'T -> 'U) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then x else y) xs

let min (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then x else y) xs

let average (xs: 'T list) ([<Inject>] averager: IGenericAverager<'T>): 'T =
    let total = fold (fun acc x -> averager.Add(acc, x)) (averager.GetZero()) xs
    averager.DivideByInt(total, length xs)

let averageBy (f: 'T -> 'T2) (xs: 'T list) ([<Inject>] averager: IGenericAverager<'T2>): 'T2 =
    let total = fold (fun acc x -> averager.Add(acc, f x)) (averager.GetZero()) xs
    averager.DivideByInt(total, length xs)

let permute f (xs: 'T list) =
    Array.ofSeq xs
    |> Array.permute f
    |> ofArray

let chunkBySize (chunkSize: int) (xs: 'T list): 'T list list =
    Array.ofSeq xs
    |> Array.chunkBySize chunkSize
    |> ofArray
    |> map ofArray

let skip i (xs: 'T list) =
    Seq.skip i xs |> ofSeq

let skipWhile predicate (xs: 'T list) =
    Seq.skipWhile predicate xs |> ofSeq

let take i xs =
    Seq.take i xs |> ofSeq

let takeWhile predicate (xs: 'T list) =
    Seq.takeWhile predicate xs |> ofSeq

let truncate i xs =
    Seq.truncate i xs |> ofSeq

let getSlice (startIndex: int option) (endIndex: int option) (xs: 'T list) =
    let len = length xs
    let startIndex = defaultArg startIndex 0
    let endIndex = defaultArg endIndex (len - 1)
    if startIndex < 0 then invalidArg "startIndex" SR.indexOutOfBounds
    elif endIndex >= len then invalidArg "endIndex" SR.indexOutOfBounds
    elif endIndex < startIndex then List.Empty
    else xs |> skip startIndex |> take (endIndex - startIndex + 1)

let splitAt index (xs: 'T list) =
    if index < 0 then invalidArg "index" SR.inputMustBeNonNegative
    if index > xs.Length then invalidArg "index" SR.notEnoughElements
    take index xs, skip index xs

let distinctBy (projection: 'T -> 'Key) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
    let hashSet = System.Collections.Generic.HashSet<'Key>(eq)
    xs |> filter (projection >> hashSet.Add)

let distinct (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    distinctBy id xs eq

let exactlyOne (xs: 'T list) =
    if xs.IsEmpty
    then invalidArg "list" SR.inputSequenceEmpty
    else
        if xs.Tail.IsEmpty then xs.Head
        else invalidArg "list" SR.inputSequenceTooLong

let groupBy (projection: 'T -> 'Key) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>): ('Key * 'T list) list =
    let dict = System.Collections.Generic.Dictionary<'Key, 'T list>(eq)
    let mutable keys = List.Empty
    xs |> iterate (fun v ->
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev ->
            dict.[key] <- List.Cons(v, prev)
        | false, _ ->
            dict.Add(key, List.Cons(v, List.Empty))
            keys <- List.Cons(key, keys) )
    let mutable result = List.Empty
    keys |> iterate (fun key -> result <- List.Cons((key, reverse dict.[key]), result))
    result

let countBy (projection: 'T -> 'Key) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
    let dict = System.Collections.Generic.Dictionary<'Key, int>(eq)
    let mutable keys = List.Empty
    xs |> iterate (fun v ->
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev ->
            dict.[key] <- prev + 1
        | false, _ ->
            dict.[key] <- 1
            keys <- List.Cons(key, keys) )
    let mutable result = List.Empty
    keys |> iterate (fun key -> result <- List.Cons((key, dict.[key]), result))
    result

let where predicate (xs: 'T list) =
    filter predicate xs

let pairwise (xs: 'T list) =
    Seq.pairwise xs |> ofSeq

let windowed (windowSize: int) (xs: 'T list): 'T list list =
    Seq.windowed windowSize xs
    |> ofSeq
    |> map ofArray

let splitInto (chunks: int) (xs: 'T list): 'T list list =
    Array.ofSeq xs
    |> Array.splitInto chunks
    |> ofArray
    |> map ofArray

let transpose (lists: seq<'T list>): 'T list list =
    lists
    |> Seq.transpose
    |> Seq.map ofSeq
    |> ofSeq

// let rev = reverse
// let init = initialize
// let iter = iterate
// let iter2 = iterate2
// let iteri = iterateIndexed
// let iteri2 = iterateIndexed2
// let forall = forAll
// let forall2 = forAll2
