module List

// Disables warn:1204 raised by use of LanguagePrimitives.ErrorStrings.*
#nowarn "1204"

open Fable.Core

let msgListWasEmpty = "List was empty"
let msgListNoMatch = "List did not contain any matching elements"

[<CustomEquality; CustomComparison>]
type List<'T when 'T: comparison> =
    { Count: int; Values: ResizeArray<'T> }

    member inline internal xs.Add(x: 'T) =
        let values =
            if xs.Count = xs.Values.Count
            then xs.Values
            else xs.Values.GetRange(0, xs.Count)
        values.Add(x)
        { Count = values.Count; Values = values }

    member inline internal xs.Reverse() =
        let values = xs.Values.GetRange(0, xs.Count) // copy values
        values.Reverse()
        { Count = values.Count; Values = values }

    // This is a destructive internal optimization that
    // can only be performed on newly constructed lists.
    member inline internal xs.ReverseInPlace() =
        xs.Values.Reverse()
        xs

    static member inline Singleton(x: 'T) =
        let values = ResizeArray<'T>()
        values.Add(x)
        { Count = 1; Values = values }

    static member inline Empty = { Count = 0; Values = ResizeArray<'T>() }
    static member inline Cons (x: 'T, xs: 'T list) = xs.Add(x)

    member inline xs.IsEmpty = xs.Count <= 0
    member inline xs.Length = xs.Count

    member inline xs.Head =
        if xs.Count > 0
        then xs.Values.[xs.Count - 1]
        else failwith msgListWasEmpty

    member inline xs.Tail =
        if xs.Count > 0
        then { Count = xs.Count - 1; Values = xs.Values }
        else failwith msgListWasEmpty

    member inline xs.Item with get (index: int) =
        xs.Values.[xs.Count - 1 - index]

    override xs.ToString() =
        "[" + System.String.Join("; ", xs) + "]"

    override xs.Equals(other: obj) =
        if obj.ReferenceEquals(xs, other)
        then true
        else
            let ys = other :?> 'T list
            if xs.Length <> ys.Length then false
            else Seq.forall2 (Unchecked.equals) xs ys

    override xs.GetHashCode() =
        let inline combineHash i x y = (x <<< 1) + y + 631 * i
        let len = min (xs.Length - 1) 18 // limit the hash count
        let mutable h = 0
        for i = 0 to len do
            h <- combineHash i h (hash xs.[i])
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
    let mutable i = -1
    interface System.Collections.Generic.IEnumerator<'T> with
        member __.Current = xs.[i]
    interface System.Collections.IEnumerator with
        member __.Current = box (xs.[i])
        member __.MoveNext() = i <- i + 1; i < xs.Length
        member __.Reset() = i <- -1
    interface System.IDisposable with
        member __.Dispose() = ()

and 'T list when 'T: comparison = List<'T>

let newList (values: ResizeArray<'T>) = { Count = values.Count; Values = values }

let empty () = List.Empty

let cons (x: 'T) (xs: 'T list) = List.Cons (x, xs)

let singleton (x: 'T) = List.Singleton (x)

let isEmpty (xs: 'T list) = xs.IsEmpty

let length (xs: 'T list) = xs.Length

let head (xs: 'T list) = xs.Head

let tryHead (xs: 'T list) =
    if xs.Length > 0
    then Some xs.[0]
    else None

let tail (xs: 'T list) = xs.Tail

let last (xs: 'T list) =
    if xs.Length > 0
    then xs.[xs.Length - 1]
    else failwith msgListWasEmpty

let tryLast (xs: 'T list) =
    if xs.Length > 0
    then Some xs.[xs.Length - 1]
    else None

let compareWith (comparer: 'T -> 'T -> int) (xs: 'T list) (ys: 'T list): int =
    Seq.compareWith comparer xs ys

let fold (folder: 'acc -> 'T -> 'acc) (state: 'acc) (xs: 'T list) =
    let mutable acc = state
    for i = 0 to xs.Length - 1 do
        acc <- folder acc xs.[i]
    acc

let foldBack (folder: 'T -> 'acc -> 'acc) (xs: 'T list) (state: 'acc) =
    let mutable acc = state
    for i = xs.Length - 1 downto 0 do
        acc <- folder xs.[i] acc
    acc

let reverse (xs: 'a list) =
    xs.Reverse()

let inline reverseInPlace (xs: 'a list) =
    xs.ReverseInPlace()

let toSeq (xs: 'a list): 'a seq =
    xs :> System.Collections.Generic.IEnumerable<'a>

let ofSeq (xs: 'a seq): 'a list =
    Seq.fold (fun acc x -> cons x acc) List.Empty xs
    |> reverseInPlace

let concat (lists: seq<'a list>) =
    Seq.fold (fold (fun acc x -> cons x acc)) List.Empty lists
    |> reverseInPlace

let fold2 f (state: 'acc) (xs: 'a list) (ys: 'b list) =
    Seq.fold2 f state xs ys

let foldBack2 f (xs: 'a list) (ys: 'b list) (state: 'acc) =
    Seq.foldBack2 f xs ys state

let unfold (gen: 'acc -> ('T * 'acc) option) (state: 'acc) =
    let rec loop st acc =
        match gen st with
        | None -> reverseInPlace acc
        | Some (x, st) -> loop st (cons x acc)
    loop state List.Empty

let scan f (state: 'acc) (xs: 'a list) =
    Seq.scan f state xs |> ofSeq

let scanBack f (xs: 'a list) (state: 'acc) =
    Seq.scanBack f xs state |> ofSeq

let append (xs: 'a list) (ys: 'a list) =
    foldBack (fun x acc -> cons x acc) xs ys

let collect (f: 'a -> 'b list) (xs: 'a list) =
    Seq.collect f xs |> ofSeq

let mapIndexed (f: int -> 'a -> 'b) (xs: 'a list) =
    let rec loop i acc =
        if i < xs.Length
        then loop (i + 1) (cons (f i xs.[i]) acc)
        else reverseInPlace acc
    loop 0 List.Empty

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
        cons nx nxs, fs
    let nxs, s = fold folder (List.Empty, s) xs
    reverseInPlace nxs, s

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

let ofArray (xs: System.Collections.Generic.IList<'T>) =
    let mutable res = List.Empty
    for i = xs.Count - 1 downto 0 do
        res <- cons xs.[i] res
    res

let tryPickIndexed (f: int -> 'a -> 'b option) (xs: 'a list) =
    let rec loop i =
        let res = f i xs.[i]
        match res with
        | Some _ -> res
        | None -> if i < xs.Length - 1 then loop (i + 1) else None
    if xs.Length > 0 then loop 0 else None

let tryPickIndexedBack (f: int -> 'a -> 'b option) (xs: 'a list) =
    let rec loop i =
        let res = f i xs.[i]
        match res with
        | Some _ -> res
        | None -> if i > 0 then loop (i - 1) else None
    if xs.Length > 0 then loop (xs.Length - 1) else None

let tryPick f xs =
    tryPickIndexed (fun _ x -> f x) xs

let pick f xs =
    match tryPick f xs with
    | None -> invalidOp msgListNoMatch
    | Some x -> x

let tryFindIndexed f xs =
    tryPickIndexed (fun i x -> if f i x then Some x else None) xs

let tryFindIndexedBack f xs =
    tryPickIndexedBack (fun i x -> if f i x then Some x else None) xs

let findIndexed f xs =
    match tryFindIndexed f xs with
    | None -> invalidOp msgListNoMatch
    | Some x -> x

let findIndexedBack f xs =
    match tryFindIndexedBack f xs with
    | None -> invalidOp msgListNoMatch
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
    | None -> invalidOp msgListNoMatch
    | Some x -> x

let findIndexBack f xs: int =
    match tryFindIndexBack f xs with
    | None -> invalidOp msgListNoMatch
    | Some x -> x

let item n (xs: 'a list) =
    if n >= 0 && n < xs.Length
    then xs.[n]
    else failwith "Index out of range"

let tryItem n (xs: 'a list) =
    if n >= 0 && n < xs.Length
    then Some xs.[n]
    else None

let filter f xs =
    fold (fun acc x ->
        if f x
        then cons x acc
        else acc) List.Empty xs
    |> reverseInPlace

let partition f xs =
    fold (fun (lacc, racc) x ->
        if f x then cons x lacc, racc
        else lacc, cons x racc) (List.Empty, List.Empty) (reverse xs)

let choose f xs =
    fold (fun acc x ->
        match f x with
        | Some y -> cons y acc
        | None -> acc) List.Empty xs
    |> reverseInPlace

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
        res <- cons (f i) res
    res |> reverseInPlace

let replicate n x =
    initialize n (fun _ -> x)

let reduce f (xs: 't list) =
    if isEmpty xs then invalidOp msgListWasEmpty
    else fold f (head xs) (tail xs)

let reduceBack f (xs: 't list) =
    if isEmpty xs then invalidOp msgListWasEmpty
    else foldBack f (tail xs) (head xs)

let forAll f xs =
    fold (fun acc x -> acc && f x) true xs

let forAll2 f xs ys =
    fold2 (fun acc x y -> acc && f x y) true xs ys

let exists f xs =
    tryFindIndex f xs |> Option.isSome

let rec exists2 f xs ys =
    match length xs, length ys with
    | 0, 0 -> false
    | x, y when x = y -> f (head xs) (head ys) || exists2 f (tail xs) (tail ys)
    | _ -> invalidOp "Lists had different lengths"

let unzip xs =
    foldBack (fun (x, y) (lacc, racc) -> cons x lacc, cons y racc) xs (List.Empty, List.Empty)

let unzip3 xs =
    foldBack (fun (x, y, z) (lacc, macc, racc) -> cons x lacc, cons y macc, cons z racc) xs (List.Empty, List.Empty, List.Empty)

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

let maxBy (projection: 'a -> 'b) (xs: 'a list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'b>): 'a =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

let max (li:'a list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'a>): 'a =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) li

let minBy (projection: 'a -> 'b) (xs: 'a list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'b>): 'a =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then x else y) xs

let min (xs: 'a list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'a>): 'a =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then x else y) xs

let average (xs: 'T list) ([<Inject>] averager: IGenericAverager<'T>): 'T =
    let total = fold (fun acc x -> averager.Add(acc, x)) (averager.GetZero()) xs
    averager.DivideByInt(total, length xs)

let averageBy (f: 'T -> 'T2) (xs: 'T list) ([<Inject>] averager: IGenericAverager<'T2>): 'T2 =
    let total = fold (fun acc x -> averager.Add(acc, f x)) (averager.GetZero()) xs
    averager.DivideByInt(total, length xs)

let permute f (xs: 'T list) =
    Array.ofSeq xs Array.DynamicArrayCons
    |> Array.permute f
    |> ofArray

let chunkBySize (chunkSize: int) (xs: 'T list): 'T list list =
    Array.ofSeq xs Array.DynamicArrayCons
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

let getSlice (lower: int option) (upper: int option) (xs: 'T list) =
    let lower = defaultArg lower 0
    let upper = defaultArg upper (xs.Length - 1)
    if lower < 0 || upper >= xs.Length then failwith "Index out of range"
    elif upper < lower then List.Empty
    else
        let values = ResizeArray(upper - lower + 1)
        for i = lower to upper do values.Add(xs.[i])
        values |> ofSeq

let splitAt i (xs: 'T list) =
    if i < 0 then invalidArg "index" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    if i > xs.Length then invalidArg "index" "The input sequence has an insufficient number of elements."
    take i xs, skip i xs

let distinctBy (projection: 'T -> 'Key) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
    let hashSet = System.Collections.Generic.HashSet<'Key>(eq)
    xs |> filter (projection >> hashSet.Add)

let distinct (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    distinctBy id xs eq

let exactlyOne (xs: 'T list) =
    match xs.Length with
    | 1 -> head xs
    | 0 -> invalidArg "list" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
    | _ -> invalidArg "list" "Input list too long"

let groupBy (projection: 'T -> 'Key) (xs: 'T list)([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>): ('Key * 'T list) list =
    let dict = System.Collections.Generic.Dictionary<'Key, 'T list>(eq)
    let mutable keys = List.Empty
    xs |> iterate (fun v ->
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev ->
            dict.[key] <- cons v prev
        | false, _ ->
            dict.Add(key, cons v List.Empty)
            keys <- cons key keys )
    let mutable result = List.Empty
    keys |> iterate (fun key -> result <- cons (key, reverseInPlace dict.[key]) result)
    result

let countBy (projection: 'T -> 'Key) (xs: 'T list)([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
    let dict = System.Collections.Generic.Dictionary<'Key, int>(eq)
    let mutable keys = List.Empty
    xs |> iterate (fun v ->
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev ->
            dict.[key] <- prev + 1
        | false, _ ->
            dict.[key] <- 1
            keys <- cons key keys )
    let mutable result = List.Empty
    keys |> iterate (fun key -> result <- cons (key, dict.[key]) result)
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
    Array.ofSeq xs Array.DynamicArrayCons
    |> Array.splitInto chunks
    |> ofArray
    |> map ofArray

let transpose (lists: seq<'T list>): 'T list list =
    lists
    |> Seq.transpose
    |> Seq.map ofSeq
    |> ofSeq
