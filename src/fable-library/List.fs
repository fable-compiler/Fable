module List

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
    { head: 'T; tail: List<'T> option }

    static member inline Empty: List<'T> = { head = Unchecked.defaultof<'T>; tail = None }
    static member inline Cons (x: 'T, xs: 'T list) = { head = x; tail = Some xs }

    member xs.IsEmpty = xs.tail.IsNone

    member xs.Length =
        let rec loop i = function
            | None -> i
            | Some t -> loop (i + 1) t.tail
        loop 0 xs.tail

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
            Seq.forall2 (Unchecked.equals) xs ys

    override xs.GetHashCode() =
        let inline combineHash i x y = (x <<< 1) + y + 631 * i
        let mutable i = 0
        let mutable h = 0
        for x in xs do
            i <- i + 1
            h <- combineHash i h (hash x)
        h

    interface System.IComparable with
        member xs.CompareTo(other: obj) =
            Seq.compareWith compare xs (other :?> 'T list)

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
    Seq.compareWith comparer xs ys

let fold (folder: 'acc -> 'T -> 'acc) (state: 'acc) (xs: 'T list) =
    let rec loop acc (ys: 'T list) =
        if ys.IsEmpty then acc
        else loop (folder acc ys.Head) ys.Tail
    loop state xs

let foldBack (folder: 'T -> 'acc -> 'acc) (xs: 'T list) (state: 'acc) =
    Seq.foldBack folder xs state

let foldIndexed (folder: int -> 'acc -> 'T -> 'acc) (state: 'acc) (xs: 'T list) =
    let rec loop i acc (ys: 'T list) =
        if ys.IsEmpty then acc
        else loop (i + 1) (folder i acc ys.Head) ys.Tail
    loop 0 state xs

let reverse (xs: 'a list) =
    fold (fun acc x -> List.Cons(x, acc)) List.Empty xs

let toSeq (xs: 'a list): 'a seq =
    xs :> System.Collections.Generic.IEnumerable<'a>

let ofSeq (xs: 'a seq): 'a list =
    Seq.fold (fun acc x -> List.Cons(x, acc)) List.Empty xs
    |> reverse

let concat (lists: seq<'a list>) =
    Seq.fold (fold (fun acc x -> List.Cons(x, acc))) List.Empty lists
    |> reverse

let fold2 f (state: 'acc) (xs: 'a list) (ys: 'b list) =
    Seq.fold2 f state xs ys

let foldBack2 f (xs: 'a list) (ys: 'b list) (state: 'acc) =
    Seq.foldBack2 f xs ys state

let unfold (gen: 'acc -> ('T * 'acc) option) (state: 'acc) =
    let rec loop st acc =
        match gen st with
        | None -> reverse acc
        | Some (x, st) -> loop st (List.Cons(x, acc))
    loop state List.Empty

let scan f (state: 'acc) (xs: 'a list) =
    Seq.scan f state xs |> ofSeq

let scanBack f (xs: 'a list) (state: 'acc) =
    Seq.scanBack f xs state |> ofSeq

let append (xs: 'a list) (ys: 'a list) =
    fold (fun acc x -> List.Cons(x, acc)) ys (reverse xs)

let collect (f: 'a -> 'b list) (xs: 'a list) =
    Seq.collect f xs |> ofSeq

let mapIndexed (f: int -> 'a -> 'b) (xs: 'a list) =
    foldIndexed (fun i acc x -> List.Cons(f i x, acc)) List.Empty xs
    |> reverse

let map (f: 'a -> 'b) (xs: 'a list) =
    mapIndexed (fun i x -> f x) xs

let indexed (xs: 'a list) =
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

let ofArrayWithTail (xs: System.Collections.Generic.IList<'T>) (tail: 'T list) =
    let mutable res = tail
    for i = xs.Count - 1 downto 0 do
        res <- List.Cons(xs.[i], res)
    res

let ofArray (xs: System.Collections.Generic.IList<'T>) =
    ofArrayWithTail xs List.Empty

let tryPickIndexed (f: int -> 'a -> 'b option) (xs: 'a list) =
    let rec loop i (ys: 'a list) =
        if ys.IsEmpty then None
        else
            match f i ys.Head with
            | Some _  as res -> res
            | None -> loop (i + 1) ys.Tail
    loop 0 xs

let tryPickIndexedBack (f: int -> 'a -> 'b option) (xs: 'a list) =
    let rec loop i acc (ys: 'a list) =
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

let tryItem n (xs: 'a list) =
    let rec loop i (ys: 'a list) =
        if ys.IsEmpty then None
        else
            if i = n then Some ys.Head
            else loop (i + 1) ys.Tail
    loop 0 xs

let item n (xs: 'a list) = xs.Item(n)

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

let except (itemsToExclude: seq<'t>) (xs: 't list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'t>): 't list =
    if isEmpty xs then xs
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

let reduce f xs =
    if isEmpty xs then invalidOp SR.inputListWasEmpty
    else fold f (head xs) (tail xs)

let reduceBack f xs =
    if isEmpty xs then invalidOp SR.inputListWasEmpty
    else foldBack f (tail xs) (head xs)

let forAll f xs =
    fold (fun acc x -> acc && f x) true xs

let forAll2 f xs ys =
    fold2 (fun acc x y -> acc && f x y) true xs ys

let exists f xs =
    tryFindIndex f xs |> Option.isSome

let rec exists2 f (xs: 'a list) (ys: 'b list) =
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

let sortBy (projection: 'a -> 'b) (xs: 'a list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'b>): 'a list =
    sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortDescending (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T list =
    sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

let sortByDescending (projection: 'a -> 'b) (xs: 'a list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'b>): 'a list =
    sortWith (fun x y -> comparer.Compare(projection x, projection y) * -1) xs

let sum (xs: 'T list) ([<Inject>] adder: IGenericAdder<'T>): 'T =
    fold (fun acc x -> adder.Add(acc, x)) (adder.GetZero()) xs

let sumBy (f: 'T -> 'T2) (xs: 'T list) ([<Inject>] adder: IGenericAdder<'T2>): 'T2 =
    fold (fun acc x -> adder.Add(acc, f x)) (adder.GetZero()) xs

let maxBy (projection: 'a -> 'b) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'b>): 'a =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

let max xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'a>): 'a =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) xs

let minBy (projection: 'a -> 'b) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'b>): 'a =
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
