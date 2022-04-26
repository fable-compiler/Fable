module ListModule

open System
open Fable.Core

[<CompiledName("FSharpListEnumerator")>]
type ResizeListEnumerator<'T>(xs: ResizeList<'T>) =
    let mutable curIdx: int = xs.HiddenCount
    let mutable curValues: ResizeArray<'T> = xs.HiddenValues
    let mutable curTail: ResizeList<'T> option = xs.HiddenTail
    interface System.Collections.Generic.IEnumerator<'T> with
        member _.Current: 'T = curValues[curIdx]
        member _.Current: obj = box curValues[curIdx]
        member _.MoveNext() =
            curIdx <- curIdx - 1
            if curIdx < 0 then
                match curTail with
                | Some t ->
                    curIdx <- t.HiddenCount - 1
                    curValues <- t.HiddenValues
                    curTail <- t.HiddenTail
                    curIdx >= 0
                | None -> false
            else true
        member _.Reset() =
            curIdx <- xs.HiddenCount
            curValues <- xs.HiddenValues
            curTail <- xs.HiddenTail
        member _.Dispose() = ()

// [<Struct>]
// [<CustomEquality; CustomComparison>]
and [<CompiledName("FSharpList")>] ResizeList<'T>(count: int, values: ResizeArray<'T>, ?tail: ResizeList<'T>) =
    // if count = 0 && Option.isSome tail then
    //     failwith "Unexpected, empty list with tail"

    member inline internal _.HiddenCount = count
    member inline internal _.HiddenValues = values
    member inline internal _.HiddenTail = tail
    member inline _.IsEmpty = count <= 0

    member _.Length =
        match tail with
        | Some tail -> count + tail.Length
        | None -> count
    
    member internal xs.Add(x: 'T) =
        if count = values.Count then
            values.Add(x)
            ResizeList<'T>(values.Count, values, ?tail=tail)
        elif count = 0 then
            ResizeList<'T>(1, ResizeArray [|x|])
        else
            ResizeList<'T>(1, ResizeArray [|x|], xs)

    member internal xs.AddRange(ys: 'T ResizeArray) =
        if count = values.Count then
            values.AddRange(ys)
            ResizeList<'T>(values.Count, values, ?tail=tail)
        elif count = 0 then
            ResizeList<'T>(ys.Count, ys)
        else
            ResizeList<'T>(ys.Count, ys, xs)

    member internal xs.Append(ys: 'T ResizeList) =
        match count, tail with
        | 0, _ -> ys
        | _, None -> ResizeList<'T>(count, values, ys)
        | _, Some _ ->
            let allValues = ResizeArray()
            let _len = xs.GetLenAndFillValues(allValues)
            let rec appendValues idx tail =
                if idx = allValues.Count then tail
                else
                    let values = allValues[idx]
                    ResizeList<'T>(values.Count, values, tail)
                    |> appendValues (idx + 1)
            appendValues 0 ys

    member internal xs.MapIndexed(f: int -> 'T -> 'U): ResizeList<'U> =
        match tail with
        | None ->
            let values = ArrayModule.Native.generateResize count (fun i -> f i values[i])
            ResizeList(count, values)
        | Some _ ->
            let allValues = ResizeArray()
            let len = xs.GetLenAndFillValues(allValues)
            let mutable i = allValues.Count - 1
            let mutable j = allValues[i].Count
            let values =
                ArrayModule.Native.generateResize len (fun idx ->
                    if j < 0 then
                        i <- i - 1
                        j <- allValues[i].Count - 1
                    else
                        j <- j - 1
                    f idx (allValues[i][j]))
            ResizeList(count, values)
    
    member internal _.Iterate f =
        for i = count - 1 downto 0 do
            f values[i]
        match tail with
        | Some t -> t.Iterate f
        | None -> ()

    member internal _.IterateBack f =
        match tail with
        | Some t -> t.IterateBack f
        | None -> ()
        for i = 0 to count - 1 do
            f values[i]

    member internal xs.DoWhile f =
        let rec loop idx (xs: 'T ResizeList) =
            if idx >= 0 && f xs.HiddenValues[idx] then
                let idx = idx - 1
                if idx < 0 then
                    match xs.HiddenTail with
                    | Some t -> loop (t.HiddenCount - 1) t
                    | None -> ()
                else loop idx xs
        loop (count - 1) xs

    member private xs.GetLenAndFillValues(allValues: ResizeArray<ResizeArray<'T>>, ?accLen: int) =
        let accLen = defaultArg accLen 0
        let accLen = accLen + xs.HiddenCount
        allValues.Insert
            (0, if xs.HiddenCount = xs.HiddenValues.Count
                then xs.HiddenValues else xs.HiddenValues.GetRange(0, xs.HiddenCount))
        match xs.HiddenTail with
        | None -> accLen
        | Some t -> t.GetLenAndFillValues(allValues, accLen)
        
    member internal xs.Reverse() =
        let allValues = ResizeArray()
        let len = xs.GetLenAndFillValues(allValues)
        let mutable i = 0
        let mutable j = -1
        ArrayModule.Native.generateResize len (fun _ ->
            if j = allValues[i].Count then
                i <- i + 1
                j <- 0
            else
                j <- j + 1
            allValues[i][j])
        |> ResizeList<'T>.NewList len

    member inline xs.ToArray(): 'T[] =
        let allValues = ResizeArray()
        let len = xs.GetLenAndFillValues(allValues)
        let mutable i = allValues.Count - 1
        let mutable j = allValues[i].Count
        ArrayModule.Native.generate len (fun _ ->
            if j < 0 then
                i <- i - 1
                j <- allValues[i].Count - 1
            else
                j <- j - 1
            allValues[i][j])
    
    static member inline Singleton(x: 'T) =
        ResizeList<'T>.NewList 1 (ResizeArray [|x|])

//    static member inline NewList (values: ResizeArray<'T>) =
//        ResizeList(values.Count, values)

    static member inline NewList (count: int) (values: ResizeArray<'T>) =
        ResizeList(count, values)

    static member inline Empty: ResizeList<'T> =
        ResizeList(0, ResizeArray())

    static member inline Cons (x: 'T, xs: 'T list) = xs.Add(x)

    member _.TryHead =
        if count > 0
        then Some values[count - 1]
        else None

    member xs.Head =
        match xs.TryHead with
        | Some h -> h
        | None -> invalidArg "list" SR.inputWasEmpty

    member _.TryTail =
        if count > 1 then
            ResizeList<'T>(count - 1, values, ?tail=tail) |> Some
        elif count = 1 then
            match tail with
            | Some t -> Some t
            | None -> ResizeList<'T>(count - 1, values) |> Some
        else
            None

    member xs.Tail =
        match xs.TryTail with
        | Some h -> h
        | None -> invalidArg "list" SR.inputWasEmpty

    member inline internal _.HeadUnsafe =
        values[count - 1]

    member inline internal _.TailUnsafe =
        if count = 1 && Option.isSome tail then tail.Value
        else ResizeList<'T>(count - 1, values, ?tail=tail)

    member _.Item with get (index: int) =
        let actualIndex = count - 1 - index
        if actualIndex >= 0 then
            values[actualIndex]
        else
            match tail with
            | None -> invalidArg "index" SR.indexOutOfBounds
            | Some t -> t.Item(index - count)

    override xs.ToString() =
        "[" + String.Concat("; ", xs) + "]"

    override xs.Equals(other: obj) =
        if obj.ReferenceEquals(xs, other)
        then true
        else
            let ys = other :?> 'T list
            if xs.Length <> ys.Length then false
            else Seq.forall2 Unchecked.equals xs ys

    override xs.GetHashCode() =
        let inline combineHash i x y = (x <<< 1) + y + 631 * i
        let mutable h = 0
        let mutable i = -1
        xs.DoWhile(fun v ->
            i <- i + 1
            h <- combineHash i h (Unchecked.hash v)
            i < 18) // limit the hash count
        h

    interface System.IComparable<ResizeList<'T>> with
        member this.CompareTo(other: ResizeList<'T>) =
            let values1 = ResizeArray()
            let values2 = ResizeArray()
            let len1 = this.GetLenAndFillValues(values1)
            let len2 = other.GetLenAndFillValues(values2)
            if len1 < len2 then -1
            elif len1 > len2 then 1
            else
                let mutable res = 0
                let mutable i1 = values1.Count - 1
                let mutable j1 = values1[i1].Count
                let mutable i2 = values2.Count - 1
                let mutable j2 = values2[i2].Count
                while res = 0 && (i1 > 0 || j1 > 0) && (i2 > 0 || j2 > 0) do
                    if j1 < 0 then
                        i1 <- i1 - 1
                        j1 <- values1[i1].Count - 1
                    else
                        j1 <- j1 - 1
                    if j2 < 0 then
                        i2 <- i2 - 1
                        j2 <- values2[i2].Count - 1
                    else
                        j2 <- j2 - 1                        
                    let v1 = values1[i1][j1]
                    let v2 = values2[i2][j2]
                    match box v1 with
                    | :? IComparable<'T> as v1 ->
                        res <- v1.CompareTo(v2)
                    | _ -> ()
                res                

    interface System.Collections.Generic.IEnumerable<'T> with
        member xs.GetEnumerator() = new ResizeListEnumerator<'T>(xs) :> System.Collections.Generic.IEnumerator<'T>
        member xs.GetEnumerator() = new ResizeListEnumerator<'T>(xs) :> System.Collections.IEnumerator

and 'T list = ResizeList<'T>

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module List =

let inline indexNotFound() = raise (System.Collections.Generic.KeyNotFoundException(SR.keyNotFoundAlt))

let newList (values: ResizeArray<'T>) =
    ResizeList<'T>.NewList values.Count values

let newListWithTail (xs: 'T ResizeArray) (tail: 'T list) =
    tail.AddRange(xs)

let empty () = ResizeList.Empty

let cons (x: 'T) (xs: 'T list) = ResizeList.Cons (x, xs)

let singleton (x: 'T) = ResizeList.Singleton (x)

let isEmpty (xs: 'T list) = xs.IsEmpty

let length (xs: 'T list) = xs.Length

let head (xs: 'T list) = xs.Head

let tryHead (xs: 'T list) = xs.TryHead

let tail (xs: 'T list) = xs.Tail

let head_ (xs: 'T list) = xs.HeadUnsafe

let tail_ (xs: 'T list) = xs.TailUnsafe

// let (|Cons|Nil|) xs =
//     if isEmpty xs then Nil
//     else Cons (head xs, tail xs)

let tryLast (xs: 'T list) =
    if xs.Length > 0
    then Some xs[xs.Length - 1]
    else None

let last (xs: 'T list) =
    match tryLast xs with
    | Some h -> h
    | None -> invalidArg "list" SR.inputWasEmpty

let compareWith (comparer: 'T -> 'T -> int) (xs: 'T list) (ys: 'T list): int =
    Seq.compareWith comparer xs ys

let fold (folder: 'acc -> 'T -> 'acc) (state: 'acc) (xs: 'T list): 'acc =
    let mutable acc = state
    xs.Iterate(fun v -> acc <- folder acc v)
    acc

let foldBack (folder: 'T -> 'acc -> 'acc) (xs: 'T list) (state: 'acc): 'acc =
    let mutable acc = state
    xs.IterateBack(fun v -> acc <- folder v acc)
    acc

let reverse (xs: 'a list) =
    xs.Reverse()

// One of the attempts to optimize but I'm not sure if it's much faster than native reverse
// If it is, we should use this as replacement of ResizeArray.Reverse
// https://stackoverflow.com/a/9113136
let private reverseInPlace (xs: ResizeArray<'a>) =
    let mutable left = 0
    let mutable right = 0
    let length = xs.Count
    while left < length / 2 do
        right <- length - 1 - left;
        let temporary = xs[left]
        xs[left] <- xs[right]
        xs[right] <- temporary
        left <- left + 1

let ofResizeArrayInPlace (xs: ResizeArray<'a>): ResizeList<'a> =
    reverseInPlace xs
    ResizeList<'a>.NewList xs.Count xs

let toSeq (xs: 'a list): 'a seq =
    xs :> System.Collections.Generic.IEnumerable<'a>

let ofSeq (xs: 'a seq): 'a list =
    // Seq.fold (fun acc x -> cons x acc) ResizeList.Empty xs
    // |> ofResizeArrayInPlace
    let values = ResizeArray(xs)
    reverseInPlace values
    values |> newList

let concat (lists: seq<'a list>): ResizeList<'a> =
    (ResizeArray(), lists)
    ||> Seq.fold (fold (fun acc x -> acc.Add(x); acc))
    |> ofResizeArrayInPlace

let fold2 (f: 'acc -> 'a -> 'b -> 'acc) (state: 'acc) (xs: 'a list) (ys: 'b list): 'acc =
    Seq.fold2 f state xs ys

let foldBack2 (f: 'a -> 'b -> 'acc -> 'acc) (xs: 'a list) (ys: 'b list) (state: 'acc): 'acc =
    Seq.foldBack2 f xs ys state

let unfold (gen: 'acc -> ('T * 'acc) option) (state: 'acc): ResizeList<'T> =
    let rec loop st acc =
        match gen st with
        | None -> ofResizeArrayInPlace acc
        | Some (x, st) -> acc.Add(x); loop st acc
    loop state (ResizeArray())

let scan (f: 'acc -> 'a -> 'acc) (state: 'acc) (xs: 'a list): 'acc list =
    Seq.scan f state xs |> ofSeq

let scanBack (f: 'a -> 'acc -> 'acc) (xs: 'a list) (state: 'acc): 'acc list =
    Seq.scanBack f xs state |> ofSeq

let append (xs: 'a list) (ys: 'a list): ResizeList<'a> =
    xs.Append(ys)

let collect (f: 'a -> 'b list) (xs: 'a list): 'b list =
    Seq.collect f xs |> ofSeq

let mapIndexed (f: int -> 'a -> 'b) (xs: 'a list) =
    xs.MapIndexed(f)

let map (f: 'a -> 'b) (xs: 'a list) =
    xs.MapIndexed(fun _ x -> f x)

let indexed (xs: 'a list) =
    xs.MapIndexed(fun i x -> (i, x))

let map2 (f: 'a -> 'b -> 'c) (xs: seq<'a>) (ys: seq<'b>): 'c list =
    Seq.map2 f xs ys |> ofSeq

let mapIndexed2 (f: int -> 'a -> 'b -> 'c) (xs: seq<'a>) (ys: seq<'b>): 'c list =
    Seq.mapi2 f xs ys |> ofSeq

let map3 (f: 'a -> 'b -> 'c -> 'd) (xs: seq<'a>) (ys: seq<'b>) (zs: seq<'c>): 'd list =
    Seq.map3 f xs ys zs |> ofSeq

let mapFold (f: 'S -> 'T -> 'R * 'S) (s: 'S) (xs: 'T list): ResizeList<'R> * 'S =
    let folder (nxs: ResizeArray<_>, fs) x =
        let nx, fs = f fs x
        nxs.Add(nx)
        nxs, fs
    let nxs, s = fold folder (ResizeArray(), s) xs
    ofResizeArrayInPlace nxs, s

let mapFoldBack (f: 'T -> 'S -> 'R * 'S) (xs: 'T list) (s: 'S): ResizeList<'R> * 'S =
    mapFold (fun s v -> f v s) s (reverse xs)

let iterate (f: 'a -> unit) (xs: 'a list): unit =
    xs.Iterate f

let iterate2 (f: 'a -> 'b -> unit) (xs: 'a list) (ys: 'b list): unit =
    fold2 (fun () x y -> f x y) () xs ys

let iterateIndexed f (xs: 'a list) =
    let mutable i = -1
    xs.Iterate(fun v ->
        i <- i + 1
        f i v)

let iterateIndexed2 f xs ys =
    fold2 (fun i x y -> f i x y; i + 1) 0 xs ys |> ignore

let toArray (xs: 'a list): 'a[] =
    xs.ToArray()
    
let ofArray (xs: 'T[]) =
    let values = ResizeArray(xs)
    reverseInPlace values
    values |> newList

let tryPickIndexed (f: int -> 'a -> 'b option) (xs: 'a list) =
    let mutable result = None
    let mutable i = -1
    xs.DoWhile(fun v ->
        i <- i + 1
        match f i v with
        | Some r -> result <- Some r; false
        | None -> true)
    result

let tryPickIndexedBack (f: int -> 'a -> 'b option) (xs: 'a list) =
    let mutable result = None
    let mutable i = xs.Length
    xs.IterateBack(fun v ->
        if Option.isNone result then
            i <- i - 1
            result <- f i v)
    result

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

let tryFindIndexedBack f xs =
    tryPickIndexedBack (fun i x -> if f i x then Some x else None) xs

let findIndexed (f: int -> 'a -> 'b option) (xs: 'a list): 'b =
    match tryPickIndexed f xs with
    | None -> indexNotFound()
    | Some x -> x

let findIndexedBack (f: int -> 'a -> bool) (xs: 'a list): 'a =
    match tryFindIndexedBack f xs with
    | None -> indexNotFound()
    | Some x -> x

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

let tryItem index (xs: 'a list) =
    if index >= 0 && index < xs.Length
    then Some xs[index]
    else None

let item index (xs: 'a list) =
    match tryItem index xs with
    | Some x -> x
    | None -> invalidArg "index" SR.indexOutOfBounds

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

//let except (itemsToExclude: seq<'t>) (xs: 't list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'t>): 't list =
//    if isEmpty xs then xs
//    else
//        let cached = System.Collections.Generic.HashSet(itemsToExclude, eq)
//        xs |> filter cached.Add

let initialize n f =
    ArrayModule.Native.generateResize n f
    |> ofResizeArrayInPlace

let replicate n x =
    initialize n (fun _ -> x)

let reduce f (xs: 'T list) =
    if isEmpty xs then invalidArg "list" SR.inputWasEmpty
    else fold f (head xs) (tail xs)

let reduceBack f (xs: 't list) =
    if isEmpty xs then invalidArg "list" SR.inputWasEmpty
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
    | _ -> invalidArg "list2" SR.differentLengths

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
    values.Sort(Comparison<_>(comparison)) // should be a stable sort in JS
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
        // TODO: Optimize this better so we don't to index the list every time
        ArrayModule.Native.generateResize (endIndex - startIndex + 1) (fun i ->
            xs[startIndex + i])
        |> newList

let splitAt index (xs: 'T list) =
    if index < 0 then invalidArg "index" SR.inputMustBeNonNegative
    if index > xs.Length then invalidArg "index" SR.notEnoughElements
    take index xs, skip index xs

//let distinctBy (projection: 'T -> 'Key) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
//    let hashSet = System.Collections.Generic.HashSet<'Key>(eq)
//    xs |> filter (projection >> hashSet.Add)

//let distinct (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
//    distinctBy id xs eq

let exactlyOne (xs: 'T list) =
    match xs.Length with
    | 1 -> head xs
    | 0 -> invalidArg "list" SR.inputSequenceEmpty
    | _ -> invalidArg "list" SR.inputSequenceTooLong

//let groupBy (projection: 'T -> 'Key) (xs: 'T list)([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>): ('Key * 'T list) list =
//    let dict = System.Collections.Generic.Dictionary<'Key, ResizeArray<'T>>(eq)
//    let keys = ResizeArray<'Key>()
//    for v in xs do
//        let key = projection v
//        match dict.TryGetValue(key) with
//        | true, prev ->
//            prev.Add(v)
//        | false, _ ->
//            dict.Add(key, ResizeArray [|v|])
//            keys.Add(key)
//    ArrayModule.Native.generateResize keys.Count (fun i ->
//        let key = keys[i]
//        (key, ofResizeArrayInPlace dict[key]))
//    |> ofResizeArrayInPlace

//let countBy (projection: 'T -> 'Key) (xs: 'T list)([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'Key>) =
//    let dict = System.Collections.Generic.Dictionary<'Key, int>(eq)
//    let mutable keys = ResizeList.Empty
//    xs |> iterate (fun v ->
//        let key = projection v
//        match dict.TryGetValue(key) with
//        | true, prev ->
//            dict[key] <- prev + 1
//        | false, _ ->
//            dict[key] <- 1
//            keys <- cons key keys )
//    let mutable result = ResizeList.Empty
//    keys |> iterate (fun key -> result <- cons (key, dict[key]) result)
//    result

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