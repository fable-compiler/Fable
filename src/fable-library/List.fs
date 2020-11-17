module ListModule

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

[<Emit("new Array($0)")>]
let private allocate (i: int): ResizeArray<'T> = jsNative

// [<Struct>]
// [<CustomEquality; CustomComparison>]
[<CompiledName("FSharpList")>]
type ResizeList<'T>(count, values) =
    member _.Count: int = count
    member _.Values: ResizeArray<'T> = values

    member internal xs.Add(x: 'T) =
        let values =
            if xs.Count = xs.Values.Count
            then xs.Values
            else xs.Values.GetRange(0, xs.Count)
        values.Add(x)
        ResizeList<'T>.NewList(values)

    member internal xs.Reverse() =
        let values = allocate xs.Count
        let mutable j = 0
        for i = xs.Count - 1 downto 0 do
            values.[j] <- xs.Values.[i]
            j <- j + 1
        ResizeList<'T>.NewList(values)

    // This is a destructive internal optimization that
    // can only be performed on newly constructed lists.
    member inline internal xs.ReverseInPlace() =
        xs.Values.Reverse()
        xs

    static member inline Singleton(x: 'T) =
        ResizeList<'T>.NewList(ResizeArray [|x|])

    static member inline NewList (values: ResizeArray<'T>) =
        ResizeList(values.Count, values)

    static member inline NewList (count, values) =
        ResizeList(count, values)

    static member inline Empty: ResizeList<'T> =
        ResizeList<'T>.NewList(ResizeArray())

    static member inline Cons (x: 'T, xs: 'T list) = xs.Add(x)

    member inline xs.IsEmpty = xs.Count <= 0

    member inline xs.Length = xs.Count

    member xs.Head =
        if xs.Count > 0
        then xs.Values.[xs.Count - 1]
        else invalidArg "list" SR.inputListWasEmpty

    member xs.Tail =
        if xs.Count > 0
        then ResizeList<'T>.NewList(xs.Count - 1, xs.Values)
        else invalidArg "list" SR.inputListWasEmpty

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
            else Seq.forall2 Unchecked.equals xs ys

    override xs.GetHashCode() =
        let inline combineHash i x y = (x <<< 1) + y + 631 * i
        let len = min (xs.Length - 1) 18 // limit the hash count
        let mutable h = 0
        for i = 0 to len do
            h <- combineHash i h (Unchecked.hash xs.[i])
        h

    interface IJsonSerializable with
        member this.toJSON(_key) =
            JS.Constructors.Array.from(this) |> box

    interface System.IComparable with
        member xs.CompareTo(other: obj) =
            Seq.compareWith Unchecked.compare xs (other :?> 'T list)

    interface System.Collections.Generic.IEnumerable<'T> with
        member xs.GetEnumerator(): System.Collections.Generic.IEnumerator<'T> =
            new ListEnumerator<'T>(xs) :> System.Collections.Generic.IEnumerator<'T>

    interface System.Collections.IEnumerable with
        member xs.GetEnumerator(): System.Collections.IEnumerator =
            ((xs :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator)

and ListEnumerator<'T>(xs: 'T list) =
    let mutable i = -1
    interface System.Collections.Generic.IEnumerator<'T> with
        member __.Current = xs.[i]
    interface System.Collections.IEnumerator with
        member __.Current = box (xs.[i])
        member __.MoveNext() = i <- i + 1; i < xs.Length
        member __.Reset() = i <- -1
    interface System.IDisposable with
        member __.Dispose() = ()

and 'T list = ResizeList<'T>

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module List =

let inline indexNotFound() = raise (System.Collections.Generic.KeyNotFoundException(SR.keyNotFoundAlt))

let newList values = ResizeList<'T>.NewList (values)

let empty () = ResizeList.Empty

let cons (x: 'T) (xs: 'T list) = ResizeList.Cons (x, xs)

let singleton (x: 'T) = ResizeList.Singleton (x)

let isEmpty (xs: 'T list) = xs.IsEmpty

let length (xs: 'T list) = xs.Length

let head (xs: 'T list) = xs.Head

let tryHead (xs: 'T list) =
    if xs.Length > 0
    then Some xs.[0]
    else None

let tail (xs: 'T list) = xs.Tail

let (|Cons|Nil|) xs =
    if isEmpty xs then Nil
    else Cons (head xs, tail xs)

let last (xs: 'T list) =
    if xs.Length > 0
    then xs.[xs.Length - 1]
    else invalidArg "list" SR.inputListWasEmpty

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

let ofResizeArrayInPlace (xs: ResizeArray<'a>) =
    xs.Reverse()
    ResizeList.NewList xs

let toSeq (xs: 'a list): 'a seq =
    xs :> System.Collections.Generic.IEnumerable<'a>

let ofSeq (xs: 'a seq): 'a list =
    // Seq.fold (fun acc x -> cons x acc) ResizeList.Empty xs
    // |> ofResizeArrayInPlace
    let values = ResizeArray(xs)
    values.Reverse()
    values |> newList

let concat (lists: seq<'a list>) =
    (ResizeArray(), lists)
    ||> Seq.fold (fold (fun acc x -> acc.Add(x); acc))
    |> ofResizeArrayInPlace

let fold2 f (state: 'acc) (xs: 'a list) (ys: 'b list) =
    Seq.fold2 f state xs ys

let foldBack2 f (xs: 'a list) (ys: 'b list) (state: 'acc) =
    Seq.foldBack2 f xs ys state

let unfold (gen: 'acc -> ('T * 'acc) option) (state: 'acc) =
    let rec loop st acc =
        match gen st with
        | None -> ofResizeArrayInPlace acc
        | Some (x, st) -> acc.Add(x); loop st acc
    loop state (ResizeArray())

let scan f (state: 'acc) (xs: 'a list) =
    Seq.scan f state xs |> ofSeq

let scanBack f (xs: 'a list) (state: 'acc) =
    Seq.scanBack f xs state |> ofSeq

let append (xs: 'a list) (ys: 'a list) =
    let ylen = ys.Count
    let values = allocate (xs.Count + ys.Count)
    for i = xs.Count - 1 downto 0 do
        values.[i + ylen] <- xs.Values.[i]
    for i = ys.Count - 1 downto 0 do
        values.[i] <- ys.Values.[i]
    ResizeList<'a>.NewList(values)

let collect (f: 'a -> 'b list) (xs: 'a list) =
    Seq.collect f xs |> ofSeq

let mapIndexed (f: int -> 'a -> 'b) (xs: 'a list) =
    let values = allocate xs.Count
    let mutable j = 0
    for i = xs.Count - 1 downto 0 do
        values.[i] <- f j xs.Values.[i]
        j <- j + 1
    ResizeList<'b>.NewList(values)

let map (f: 'a -> 'b) (xs: 'a list) =
    mapIndexed (fun _i x -> f x) xs

let indexed (xs: 'a list) =
    mapIndexed (fun i x -> (i, x)) xs

let map2 f xs ys =
    Seq.map2 f xs ys |> ofSeq

let mapIndexed2 f xs ys =
    Seq.mapi2 f xs ys |> ofSeq

let map3 f xs ys zs =
    Seq.map3 f xs ys zs |> ofSeq

let mapFold (f: 'S -> 'T -> 'R * 'S) s xs =
    let folder (nxs: ResizeArray<_>, fs) x =
        let nx, fs = f fs x
        nxs.Add(nx)
        nxs, fs
    let nxs, s = fold folder (ResizeArray(), s) xs
    ofResizeArrayInPlace nxs, s

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

let ofArrayWithTail (xs: 'T[]) (tail: 'T list) =
    let values = tail.Values
    for i = xs.Length - 1 downto 0 do
        values.Add(xs.[i])
    newList values

// let ofArray (xs: 'T[]) =
//     ofArrayWithTail xs ResizeList.Empty

let ofArray (xs: 'T[]) =
    // let mutable res = ResizeList.Empty
    // for i = xs.Length - 1 downto 0 do
    //     res <- cons xs.[i] res
    // res
    let values = ResizeArray(xs.Length)
    let lastIndex = xs.Length - 1
    for i = lastIndex downto 0 do
        values.[lastIndex - i] <- xs.[i]
    values |> newList

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
    let rec loop (xs: 'T list) =
        if xs.IsEmpty then None
        else
            match f xs.Head with
            | Some _  as res -> res
            | None -> loop xs.Tail
    loop xs

let pick f xs =
    match tryPick f xs with
    | None -> indexNotFound()
    | Some x -> x
    | None -> indexNotFound()

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
    | None -> indexNotFound()

let tryFindBack f xs =
    xs |> toArray |> Array.tryFindBack f

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
    | None -> indexNotFound()

let findIndexBack f xs: int =
    match tryFindIndexBack f xs with
    | None -> indexNotFound()
    | Some x -> x

let item index (xs: 'a list) =
    if index >= 0 && index < xs.Length
    then xs.[index]
    else invalidArg "index" SR.indexOutOfBounds

let tryItem index (xs: 'a list) =
    if index >= 0 && index < xs.Length
    then Some xs.[index]
    else None

let filter f xs =
    (ResizeArray(), xs)
    ||> fold (fun acc x ->
        if f x
        then acc.Add(x); acc
        else acc)
    |> ofResizeArrayInPlace

// TODO: Optimize this
let partition f xs =
    fold (fun (lacc, racc) x ->
        if f x then cons x lacc, racc
        else lacc, cons x racc) (ResizeList.Empty, ResizeList.Empty) (reverse xs)

let choose f xs =
    (ResizeArray(), xs)
    ||> fold (fun acc x ->
        match f x with
        | Some y -> acc.Add(y); acc
        | None -> acc)
    |> ofResizeArrayInPlace

let contains (value: 'T) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    tryFindIndex (fun v -> eq.Equals (value, v)) xs |> Option.isSome

let except (itemsToExclude: seq<'t>) (xs: 't list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'t>): 't list =
    if isEmpty xs then xs
    else
        let cached = System.Collections.Generic.HashSet(itemsToExclude, eq)
        xs |> filter cached.Add

let initialize n f =
    let mutable j = 0
    let values = allocate n
    for i = n - 1 downto 0 do
        values.[i] <- f j
        j <- j + 1
    values |> newList

let replicate n x =
    initialize n (fun _ -> x)

let reduce f (xs: 'T list) =
    if isEmpty xs then invalidArg "list" SR.inputListWasEmpty
    else fold f (head xs) (tail xs)

let reduceBack f (xs: 't list) =
    if isEmpty xs then invalidArg "list" SR.inputListWasEmpty
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
    | _ -> invalidArg "list2" SR.listsHadDifferentLengths

// TODO: Optimize this
let unzip xs =
    foldBack (fun (x, y) (lacc, racc) -> cons x lacc, cons y racc) xs (ResizeList.Empty, ResizeList.Empty)

let unzip3 xs =
    foldBack (fun (x, y, z) (lacc, macc, racc) -> cons x lacc, cons y macc, cons z racc) xs (ResizeList.Empty, ResizeList.Empty, ResizeList.Empty)

let zip xs ys =
    map2 (fun x y -> x, y) xs ys

let zip3 xs ys zs =
    map3 (fun x y z -> x, y, z) xs ys zs

let sortWith (comparison: 'T -> 'T -> int) (xs: 'T list): 'T list =
    let values = ResizeArray(xs)
    values.Sort(System.Comparison<_>(comparison)) // should be a stable sort in JS
    values |> ofResizeArrayInPlace

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

let sumBy (f: 'T -> 'U) (xs: 'T list) ([<Inject>] adder: IGenericAdder<'U>): 'U =
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
    Seq.permute f xs |> ofSeq

let chunkBySize (chunkSize: int) (xs: 'T list): 'T list list =
    Seq.chunkBySize chunkSize xs
    |> Seq.map ofArray
    |> ofSeq

let skip count (xs: 'T list) =
    Seq.skip count xs |> ofSeq

let skipWhile predicate (xs: 'T list) =
    Seq.skipWhile predicate xs |> ofSeq

let take count xs =
    Seq.take count xs |> ofSeq

let takeWhile predicate (xs: 'T list) =
    Seq.takeWhile predicate xs |> ofSeq

let truncate count xs =
    Seq.truncate count xs |> ofSeq

let getSlice (startIndex: int option) (endIndex: int option) (xs: 'T list) =
    let startIndex = defaultArg startIndex 0
    let endIndex = defaultArg endIndex (xs.Length - 1)
    if startIndex > endIndex then
        ResizeList.Empty
    else
        let startIndex = if startIndex < 0 then 0 else startIndex
        let endIndex = if endIndex >= xs.Length then xs.Length - 1 else endIndex
        // take (endIndex - startIndex + 1) (skip startIndex xs)
        let values = allocate (endIndex - startIndex + 1)
        let mutable j = 0
        for i = endIndex downto startIndex do
            values.[j] <- xs.[i]
            j <- j + 1
        values |> newList

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
    match xs.Length with
    | 1 -> head xs
    | 0 -> invalidArg "list" SR.inputSequenceEmpty
    | _ -> invalidArg "list" SR.inputSequenceTooLong

// TODO: Optimize this
let groupBy (projection: 'T -> 'Key) (xs: 'T list)([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>): ('Key * 'T list) list =
    let dict = System.Collections.Generic.Dictionary<'Key, 'T list>(eq)
    let mutable keys = ResizeList.Empty
    xs |> iterate (fun v ->
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev ->
            dict.[key] <- cons v prev
        | false, _ ->
            dict.Add(key, cons v ResizeList.Empty)
            keys <- cons key keys )
    let mutable result = ResizeList.Empty
    keys |> iterate (fun key -> result <- cons (key, dict.[key].ReverseInPlace()) result)
    result

let countBy (projection: 'T -> 'Key) (xs: 'T list)([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
    let dict = System.Collections.Generic.Dictionary<'Key, int>(eq)
    let mutable keys = ResizeList.Empty
    xs |> iterate (fun v ->
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev ->
            dict.[key] <- prev + 1
        | false, _ ->
            dict.[key] <- 1
            keys <- cons key keys )
    let mutable result = ResizeList.Empty
    keys |> iterate (fun key -> result <- cons (key, dict.[key]) result)
    result

let where predicate (xs: 'T list) =
    filter predicate xs

let pairwise (xs: 'T list) =
    Seq.pairwise xs |> ofSeq

let windowed (windowSize: int) (xs: 'T list): 'T list list =
    Seq.windowed windowSize xs
    |> Seq.map ofArray
    |> ofSeq

let splitInto (chunks: int) (xs: 'T list): 'T list list =
    Seq.splitInto chunks xs
    |> Seq.map ofArray
    |> ofSeq

let transpose (lists: seq<'T list>): 'T list list =
    Seq.transpose lists
    |> Seq.map ofSeq
    |> ofSeq

// let rev = reverse
// let init = initialize
// let iter = iterate
// let iter2 = iterate2
// let iteri = iterateIndexed
// let iteri2 = iterateIndexed2
