module Array_

open Global_

// For optimization, functions may return ResizeArray instead of Array.
// That's fine, because they both have the same representation in Rust.

// The inline keyword is sometimes used just to infer type constraints.

let inline indexNotFound () = failwith SR.keyNotFoundAlt
let inline differentLengths () = failwith SR.arraysHadDifferentLengths

// native implementations
let inline empty () : 'T[] = Array.empty
let inline create (count: int) (value: 'T) : 'T[] = Array.create count value
let inline zeroCreate (count: int) : 'T[] = Array.zeroCreate count
let inline singleton (value: 'T) : 'T[] = Array.singleton value
let inline isEmpty (source: 'T[]) : bool = Array.isEmpty source
let inline length (source: 'T[]) : int = Array.length source
let inline item (index: int) (source: 'T[]) : 'T = Array.item index source
let inline get (source: 'T[]) (index: int) : 'T = Array.get source index

let inline set (source: 'T[]) (index: int) (value: 'T) : unit =
    Array.set source index value

let inline copy (source: 'T[]) : 'T[] = Array.copy source

let tryItem (index: int) (source: 'T[]) : 'T option =
    if index < 0 || index >= source.Length then
        None
    else
        Some source[index]

let reverse (source: 'T[]) : 'T[] =
    let res = Array.copy source
    System.Array.Reverse(res)
    res

let fill (target: 'T[]) (targetIndex: int) (count: int) (value: 'T) : unit =
    if targetIndex < 0 || targetIndex + count > target.Length then
        invalidArg "index" SR.indexOutOfBounds

    let len = target.Length

    for i = targetIndex to targetIndex + count - 1 do
        target[i] <- value

let getSubArray (source: 'T[]) (startIndex: int) (count: int) : 'T[] =
    if startIndex < 0 || startIndex + count > source.Length then
        invalidArg "index" SR.indexOutOfBounds

    let res = ResizeArray<_>(count)

    for i = 0 to count - 1 do
        res.Add(source[startIndex + i])

    res |> asArray

let exactlyOne (source: 'T[]) : 'T =
    if source.Length = 1 then
        source[0]
    elif isEmpty source then
        invalidArg "array" SR.inputSequenceEmpty
    else
        invalidArg "array" SR.inputSequenceTooLong

let tryExactlyOne (source: 'T[]) : 'T option =
    if source.Length = 1 then
        Some(source[0])
    else
        None

let head (source: 'T[]) : 'T =
    if isEmpty source then
        invalidArg "array" SR.arrayWasEmpty
    else
        source[0]

let tryHead (source: 'T[]) : 'T option =
    if isEmpty source then
        None
    else
        Some source[0]

let last (source: 'T[]) : 'T =
    let len = source.Length

    if isEmpty source then
        invalidArg "array" SR.arrayWasEmpty
    else
        source[len - 1]

let tryLast (source: 'T[]) : 'T option =
    let len = source.Length

    if isEmpty source then
        None
    else
        Some source[len - 1]

let tail (source: 'T[]) =
    if isEmpty source then
        invalidArg "array" SR.notEnoughElements

    getSubArray source 1 (source.Length - 1)

let append (source1: 'T[]) (source2: 'T[]) : 'T[] =
    let len1 = source1.Length
    let len2 = source2.Length
    let res = ResizeArray<_>(len1 + len2)

    for i = 0 to len1 - 1 do
        res.Add(source1[i])

    for i = 0 to len2 - 1 do
        res.Add(source2[i])

    res |> asArray

let choose (chooser: 'T -> 'U option) (source: 'T[]) : 'U[] =
    let res = ResizeArray<'U>()

    for i = 0 to source.Length - 1 do
        match chooser source[i] with
        | Some x -> res.Add(x)
        | None -> ()

    res |> asArray

let compareWith (comparer: 'T -> 'T -> int) (source1: 'T[]) (source2: 'T[]) =
    let len1 = source1.Length
    let len2 = source2.Length

    let len =
        if len1 < len2 then
            len1
        else
            len2

    let mutable i = 0
    let mutable res = 0

    while res = 0 && i < len do
        res <- comparer source1[i] source2[i]
        i <- i + 1

    if res <> 0 then
        res
    elif len1 > len2 then
        1
    elif len1 < len2 then
        -1
    else
        0

let compareTo (source1: 'T[]) (source2: 'T[]) =
    // LanguagePrimitives.GenericComparison source1 source2
    let len1 = source1.Length
    let len2 = source2.Length

    if len1 > len2 then
        1
    elif len1 < len2 then
        -1
    else
        let mutable i = 0
        let mutable res = 0

        while res = 0 && i < len1 do
            res <- compare source1[i] source2[i]
            i <- i + 1

        res

let equals (source1: 'T[]) (source2: 'T[]) =
    // LanguagePrimitives.GenericEquality source1 source2
    let len1 = source1.Length
    let len2 = source2.Length

    if len1 = len2 then
        let mutable i = 0
        let mutable res = true

        while res && i < len1 do
            res <- source1[i] = source2[i]
            i <- i + 1

        res
    else
        false

let mapIndexed (mapping: int -> 'T -> 'U) (source: 'T[]) : 'U[] =
    let len = source.Length
    let res = ResizeArray<'U>(len)

    for i = 0 to len - 1 do
        res.Add(mapping i source[i])

    res |> asArray

let map (mapping: 'T -> 'U) (source: 'T[]) : 'U[] =
    let len = source.Length
    let res = ResizeArray<'U>(len)

    for i = 0 to len - 1 do
        res.Add(mapping source[i])

    res |> asArray

let mapIndexed2
    (mapping: int -> 'T1 -> 'T2 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    : 'U[]
    =
    if source1.Length <> source2.Length then
        differentLengths ()

    let len = source1.Length
    let res = ResizeArray<'U>(len)

    for i = 0 to len - 1 do
        res.Add(mapping i source1[i] source2[i])

    res |> asArray

let map2 (mapping: 'T1 -> 'T2 -> 'U) (source1: 'T1[]) (source2: 'T2[]) : 'U[] =
    if source1.Length <> source2.Length then
        differentLengths ()

    let len = source1.Length
    let res = ResizeArray<'U>(len)

    for i = 0 to len - 1 do
        res.Add(mapping source1[i] source2[i])

    res |> asArray

let map3
    (mapping: 'T1 -> 'T2 -> 'T3 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    (source3: 'T3[])
    : 'U[]
    =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then
        differentLengths ()

    let len = source1.Length
    let res = ResizeArray<'U>(len)

    for i = 0 to len - 1 do
        res.Add(mapping source1[i] source2[i] source3[i])

    res |> asArray

let mapFold
    (mapping: 'State -> 'T -> 'U * 'State)
    state
    (source: 'T[])
    : 'U[] * 'State
    =
    let mutable acc = state
    let len = source.Length
    let res = ResizeArray<'U>(len)

    for i = 0 to len - 1 do
        let m = mapping acc source[i]
        res.Add(fst m)
        acc <- (snd m)

    res |> asArray, acc

let mapFoldBack
    (mapping: 'T -> 'State -> 'U * 'State)
    (source: 'T[])
    state
    : 'U[] * 'State
    =
    let mutable acc = state
    let len = source.Length
    let res = ResizeArray<'U>(len)

    for i = len - 1 downto 0 do
        let m = mapping source[i] acc
        res.Add(fst m)
        acc <- (snd m)

    res.Reverse()
    res |> asArray, acc

let indexed (source: 'T[]) : (int * 'T)[] =
    let len = source.Length
    let res = ResizeArray<_>(len)

    for i = 0 to len - 1 do
        res.Add(i, source[i])

    res |> asArray

// Array.concat will first call Seq.toArray if needed, see Replacements
let concat (sources: 'T[][]) : 'T[] =
    let mutable len = 0

    for arr in sources do
        len <- len + arr.Length

    let res = ResizeArray<_>(len)

    for arr in sources do
        for x in arr do
            res.Add x

    res |> asArray

let collect (mapping: 'T -> 'U[]) (source: 'T[]) : 'U[] =
    concat (map mapping source)

let exists predicate (source: 'T[]) : bool =
    let mutable i = 0
    let mutable res = false

    while i < source.Length && not res do
        res <- predicate source[i]
        i <- i + 1

    res

let exists2 predicate (source1: 'T1[]) (source2: 'T2[]) : bool =
    if source1.Length <> source2.Length then
        differentLengths ()

    let mutable i = 0
    let mutable res = false

    while i < source1.Length && not res do
        res <- predicate source1[i] source2[i]
        i <- i + 1

    res

let contains (value: 'T) (source: 'T[]) : bool =
    exists (fun x -> x = value) source

let filter (predicate: 'T -> bool) (source: 'T[]) =
    let res = ResizeArray<_>()

    for i = 0 to source.Length - 1 do
        if predicate source[i] then
            res.Add(source[i])

    res |> asArray

let initialize count (initializer: int -> 'T) : 'T[] =
    if count < 0 then
        invalidArg "count" SR.inputMustBeNonNegative

    let res = ResizeArray<_>(count)

    for i = 0 to count - 1 do
        res.Add(initializer i)

    res |> asArray

let pairwise (source: 'T[]) : ('T * 'T)[] =
    if source.Length < 2 then
        ResizeArray<_>() |> asArray
    else
        let len = source.Length - 1
        let res = ResizeArray<_>(len)

        for i = 0 to len - 1 do
            res.Add(source[i], source[i + 1])

        res |> asArray

let partition (predicate: 'T -> bool) (source: 'T[]) : 'T[] * 'T[] =
    let res1 = ResizeArray<_>()
    let res2 = ResizeArray<_>()

    for i = 0 to source.Length - 1 do
        if predicate source[i] then
            res1.Add(source[i])
        else
            res2.Add(source[i])

    res1 |> asArray, res2 |> asArray

let reduce reduction (source: 'T[]) : 'T =
    if isEmpty source then
        invalidOp SR.arrayWasEmpty

    let folder i acc x =
        if i = 0 then
            x
        else
            reduction acc x

    let mutable acc = source[0]

    for i = 0 to source.Length - 1 do
        acc <- folder i acc source[i]

    acc

let reduceBack reduction (source: 'T[]) : 'T =
    if isEmpty source then
        invalidOp SR.arrayWasEmpty

    let folder i x acc =
        if i = 0 then
            x
        else
            reduction acc x

    let len = source.Length
    let mutable acc = source[len - 1]

    for i = 1 to len do
        acc <- folder (i - 1) source[len - i] acc

    acc

let replicate count initial : 'T[] = initialize count (fun _ -> initial)

let scan<'T, 'State> folder (state: 'State) (source: 'T[]) : 'State[] =
    let len = source.Length
    let res = Array.create (len + 1) state
    res[0] <- state

    for i = 0 to len - 1 do
        res[i + 1] <- folder res[i] source[i]

    res

let scanBack<'T, 'State> folder (source: 'T[]) (state: 'State) : 'State[] =
    let len = source.Length
    let res = Array.create (len + 1) state
    res[len] <- state

    for i = len - 1 downto 0 do
        res[i] <- folder source[i] res[i + 1]

    res

let skip count (source: 'T[]) : 'T[] =
    if count > source.Length then
        invalidArg "count" SR.outOfRange

    let count =
        if count < 0 then
            0
        else
            count

    getSubArray source count (source.Length - count)

let skipWhile (predicate: 'T -> bool) (source: 'T[]) : 'T[] =
    let mutable count = 0

    while count < source.Length && predicate source[count] do
        count <- count + 1

    getSubArray source count (source.Length - count)

let take count (source: 'T[]) : 'T[] =
    if count < 0 then
        invalidArg "count" SR.inputMustBeNonNegative

    if count > source.Length then
        invalidArg "array" SR.notEnoughElements

    getSubArray source 0 count

let takeWhile (predicate: 'T -> bool) (source: 'T[]) : 'T[] =
    let mutable count = 0

    while count < source.Length && predicate source[count] do
        count <- count + 1

    getSubArray source 0 count

let truncate (count: int) (source: 'T[]) : 'T[] =
    let count =
        if count < 0 then
            0
        elif count > source.Length then
            source.Length
        else
            count

    getSubArray source 0 count

// let addInPlace (x: 'T) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "array" "Typed arrays not supported"
//     pushImpl source x |> ignore

// let addRangeInPlace (range: seq<'T>) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "array" "Typed arrays not supported"
//     for x in range do
//         addInPlace x source

// let insertRangeInPlace index (range: seq<'T>) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "array" "Typed arrays not supported"
//     let mutable i = index
//     for x in range do
//         insertImpl source i x |> ignore
//         i <- i + 1

// let removeInPlace (item: 'T) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "array" "Typed arrays not supported"
//     let i = indexOfImpl source item 0
//     if i > -1 then
//         spliceImpl source i 1 |> ignore
//         true
//     else
//         false

// let removeAllInPlace predicate (source: 'T[]) =
//     let rec countRemoveAll count =
//         let i = findIndexImpl predicate source
//         if i > -1 then
//             spliceImpl source i 1 |> ignore
//             countRemoveAll count + 1
//         else
//             count
//     countRemoveAll 0

// TODO: Check array lengths
let copyTo (source: 'T[]) sourceIndex (target: 'T[]) targetIndex count =
    let diff = targetIndex - sourceIndex

    for i = sourceIndex to sourceIndex + count - 1 do
        target[i + diff] <- source[i]

let tryFind (predicate: 'T -> bool) (source: 'T[]) : 'T option =
    let rec inner_loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i >= source.Length then
            None
        elif predicate source[i] then
            Some source[i]
        else
            inner_loop (i + 1) predicate source

    inner_loop 0 predicate source

let find (predicate: 'T -> bool) (source: 'T[]) : 'T =
    match tryFind predicate source with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindIndex (predicate: 'T -> bool) (source: 'T[]) : int option =
    let rec inner_loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i >= source.Length then
            None
        elif predicate source[i] then
            Some i
        else
            inner_loop (i + 1) predicate source

    inner_loop 0 predicate source

let findIndex (predicate: 'T -> bool) (source: 'T[]) : int =
    match tryFindIndex predicate source with
    | Some i -> i
    | None -> indexNotFound ()

let indexOf (source: 'T[]) (item: 'T) : int =
    match tryFindIndex (fun x -> x = item) source with
    | Some i -> i
    | None -> -1

let tryFindBack (predicate: 'T -> bool) (source: 'T[]) : 'T option =
    let rec inner_loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i < 0 then
            None
        elif predicate source[i] then
            Some source[i]
        else
            inner_loop (i - 1) predicate source

    inner_loop (source.Length - 1) predicate source

let findBack (predicate: 'T -> bool) (source: 'T[]) : 'T =
    match tryFindBack predicate source with
    | Some res -> res
    | None -> indexNotFound ()

let tryFindIndexBack (predicate: 'T -> bool) (source: 'T[]) : int option =
    let rec inner_loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i < 0 then
            None
        elif predicate source[i] then
            Some i
        else
            inner_loop (i - 1) predicate source

    inner_loop (source.Length - 1) predicate source

let findIndexBack (predicate: 'T -> bool) (source: 'T[]) : int =
    match tryFindIndexBack predicate source with
    | Some res -> res
    | None -> indexNotFound ()

let findLastIndex (predicate: 'T -> bool) (source: 'T[]) : int =
    match tryFindIndexBack predicate source with
    | Some res -> res
    | None -> -1

let tryPick (chooser: 'T -> 'U option) (source: 'T[]) : 'U option =
    let rec inner_loop i (chooser: 'T -> 'U option) (source: 'T[]) =
        if i >= source.Length then
            None
        else
            match chooser source[i] with
            | None -> inner_loop (i + 1) chooser source
            | res -> res

    inner_loop 0 chooser source

let pick (chooser: 'T -> 'U option) (source: 'T[]) : 'U =
    match tryPick chooser source with
    | Some res -> res
    | None -> indexNotFound ()

let fold folder (state: 'State) (source: 'T[]) : 'State =
    let mutable acc = state

    for i = 0 to source.Length - 1 do
        acc <- folder acc source[i]

    acc

let foldBack folder (source: 'T[]) (state: 'State) : 'State =
    let mutable acc = state
    let len = source.Length

    for i = 1 to len do
        acc <- folder source[len - i] acc

    acc

let fold2 folder (state: 'State) (source1: 'T1[]) (source2: 'T2[]) : 'State =
    let mutable acc = state

    if source1.Length <> source2.Length then
        differentLengths ()

    for i = 0 to source1.Length - 1 do
        acc <- folder acc source1[i] source2[i]

    acc

let foldBack2
    folder
    (source1: 'T1[])
    (source2: 'T2[])
    (state: 'State)
    : 'State
    =
    let mutable acc = state

    if source1.Length <> source2.Length then
        differentLengths ()

    let len = source1.Length

    for i = 1 to len do
        acc <- folder source1[len - i] source2[len - i] acc

    acc

let forAll predicate (source: 'T[]) : bool =
    let mutable i = 0
    let mutable res = true

    while i < source.Length && res do
        res <- predicate source[i]
        i <- i + 1

    res

let forAll2 predicate (source1: 'T1[]) (source2: 'T2[]) : bool =
    if source1.Length <> source2.Length then
        differentLengths ()

    let mutable i = 0
    let mutable res = true

    while i < source1.Length && res do
        res <- predicate source1[i] source2[i]
        i <- i + 1

    res

let iterate action (source: 'T[]) =
    for i = 0 to source.Length - 1 do
        action source[i]

let iterateIndexed action (source: 'T[]) =
    for i = 0 to source.Length - 1 do
        action i source[i]

let iterate2 action (source1: 'T[]) (source2: 'T[]) =
    if source1.Length <> source2.Length then
        differentLengths ()

    for i = 0 to source1.Length - 1 do
        action source1[i] source2[i]

let iterateIndexed2 action (source1: 'T[]) (source2: 'T[]) =
    if source1.Length <> source2.Length then
        differentLengths ()

    for i = 0 to source1.Length - 1 do
        action i source1[i] source2[i]

let permute (indexMap: int -> int) (source: 'T[]) : 'T[] =
    let len = source.Length
    let res = Array.copy source
    let checkFlags = Array.create len 0

    iterateIndexed
        (fun i x ->
            let j = indexMap i

            if j < 0 || j >= len then
                invalidOp SR.notAPermutation

            res[j] <- x
            checkFlags[j] <- 1
        )
        source

    let isValid = checkFlags |> forAll ((=) 1)

    if not isValid then
        invalidOp SR.notAPermutation

    res

let inline private setSubArray
    (target: 'T[])
    (start: int)
    (count: int)
    (source: 'T[])
    : unit
    =
    for i = 0 to count - 1 do
        target[start + i] <- source[i]

let inline private computeSlice bound lower upper length =
    let low =
        match lower with
        | Some n when n >= bound -> n
        | _ -> bound

    let high =
        match upper with
        | Some m when m < bound + length -> m
        | _ -> bound + length - 1

    low, high

let getSlice (source: 'T[]) (lower: int option) (upper: int option) : 'T[] =
    let start, stop = computeSlice 0 lower upper source.Length
    getSubArray source start (stop - start + 1)

let setSlice
    (target: 'T[])
    (lower: int option)
    (upper: int option)
    (source: 'T[])
    : unit
    =
    let start = defaultArg lower 0
    let stop = defaultArg upper (target.Length - 1)
    setSubArray target start (stop - start + 1) source

let sortInPlaceWith (comparer: 'T -> 'T -> int) (source: 'T[]) : unit =
    System.Array.Sort(source, comparer)

let sortInPlace (source: 'T[]) : unit = sortInPlaceWith compare source

let sortInPlaceBy (projection: 'T -> 'U) (source: 'T[]) : unit =
    sortInPlaceWith (fun x y -> compare (projection x) (projection y)) source

// let sortInPlaceWithComparer (source: 'T[]) (comparer: IComparer<'T>): 'T[] =
//     sortInPlaceWith (fun x y -> comparer.Compare(x, y)) source

let sort (source: 'T[]) : 'T[] =
    let res = Array.copy source
    sortInPlace res
    res

let sortBy (projection: 'T -> 'U) (source: 'T[]) : 'T[] =
    let res = Array.copy source
    sortInPlaceBy projection res
    res

let sortWith (comparer: 'T -> 'T -> int) (source: 'T[]) : 'T[] =
    let res = Array.copy source
    sortInPlaceWith comparer res
    res

let sortDescending (source: 'T[]) : 'T[] =
    sortWith (fun x y -> (compare x y) * -1) source

let sortByDescending (projection: 'T -> 'U) (source: 'T[]) : 'T[] =
    sortWith (fun x y -> (compare (projection x) (projection y)) * -1) source

let allPairs (xs: 'T1[]) (ys: 'T2[]) : ('T1 * 'T2)[] =
    let len1 = xs.Length
    let len2 = ys.Length
    let res = ResizeArray<_>(len1 * len2)

    for i = 0 to len1 - 1 do
        for j = 0 to len2 - 1 do
            res.Add((xs[i], ys[j]))

    res |> asArray

let unfold<'T, 'State>
    (generator: 'State -> ('T * 'State) option)
    (state: 'State)
    : 'T[]
    =
    let rec inner_loop generator (state: 'State) (res: ResizeArray<'T>) =
        match generator state with
        | None -> ()
        | Some(x, s) ->
            res.Add(x)
            inner_loop generator s res

    let res = ResizeArray<_>()
    inner_loop generator state res
    res |> asArray

let unzip (source: ('T1 * 'T2)[]) : 'T1[] * 'T2[] =
    let len = source.Length
    let res1 = ResizeArray<_>(len)
    let res2 = ResizeArray<_>(len)

    iterateIndexed
        (fun i (item1, item2) ->
            res1.Add(item1)
            res2.Add(item2)
        )
        source

    res1 |> asArray, res2 |> asArray

let unzip3 (source: ('T1 * 'T2 * 'T3)[]) : 'T1[] * 'T2[] * 'T3[] =
    let len = source.Length
    let res1 = ResizeArray<_>(len)
    let res2 = ResizeArray<_>(len)
    let res3 = ResizeArray<_>(len)

    iterateIndexed
        (fun i (item1, item2, item3) ->
            res1.Add(item1)
            res2.Add(item2)
            res3.Add(item3)
        )
        source

    res1 |> asArray, res2 |> asArray, res3 |> asArray

let zip (source1: 'T1[]) (source2: 'T2[]) : ('T1 * 'T2)[] =
    map2 (fun x y -> x, y) source1 source2

let zip3
    (source1: 'T1[])
    (source2: 'T2[])
    (source3: 'T3[])
    : ('T1 * 'T2 * 'T3)[]
    =
    map3 (fun x y z -> x, y, z) source1 source2 source3

let chunkBySize (chunkSize: int) (source: 'T[]) : 'T[][] =
    if chunkSize <= 0 then
        invalidArg "size" SR.inputMustBePositive

    let len = source.Length
    let chunkCount = (len - 1) / chunkSize + 1
    let res = ResizeArray<_>(chunkCount)

    for i = 0 to chunkCount - 1 do
        let start = i * chunkSize
        let csize = System.Math.Min(chunkSize, len - start)
        let slice = getSubArray source start csize
        res.Add(slice)

    res |> asArray

let splitAt (index: int) (source: 'T[]) : 'T[] * 'T[] =
    if index < 0 || index > source.Length then
        invalidArg "index" SR.indexOutOfBounds

    getSubArray source 0 index, getSubArray source index (source.Length - index)

[<CompiledName("sum")>]
let inline sum (source: 'T[]) : 'T =
    let mutable acc = LanguagePrimitives.GenericZero

    for i = 0 to source.Length - 1 do
        acc <- acc + source[i]

    acc

[<CompiledName("sumBy")>]
let inline sumBy (projection: 'T -> 'U) (source: 'T[]) : 'U =
    let mutable acc = LanguagePrimitives.GenericZero

    for i = 0 to source.Length - 1 do
        acc <- acc + (projection source[i])

    acc

let maxBy (projection: 'T -> 'U) (xs: 'T[]) : 'T =
    reduce
        (fun x y ->
            if (projection x) > (projection y) then
                x
            else
                y
        )
        xs

let max (xs: 'T[]) : 'T =
    reduce
        (fun x y ->
            if x > y then
                x
            else
                y
        )
        xs

let minBy (projection: 'T -> 'U) (xs: 'T[]) : 'T =
    reduce
        (fun x y ->
            if (projection x) < (projection y) then
                x
            else
                y
        )
        xs

let min (xs: 'T[]) : 'T =
    reduce
        (fun x y ->
            if x < y then
                x
            else
                y
        )
        xs

[<CompiledName("average")>]
let inline average (source: 'T[]) : 'T =
    if isEmpty source then
        invalidArg "array" SR.arrayWasEmpty

    let mutable total = LanguagePrimitives.GenericZero

    for i = 0 to source.Length - 1 do
        total <- total + source[i]

    LanguagePrimitives.DivideByInt total source.Length

[<CompiledName("averageBy")>]
let inline averageBy (projection: 'T -> 'U) (source: 'T[]) : 'U =
    if isEmpty source then
        invalidArg "array" SR.arrayWasEmpty

    let mutable total = LanguagePrimitives.GenericZero

    for i = 0 to source.Length - 1 do
        total <- total + (projection source[i])

    LanguagePrimitives.DivideByInt total source.Length

// Option.toArray redirects here to avoid dependency (see Replacements)
let ofOption<'T> (opt: 'T option) : 'T[] =
    match opt with
    | Some x -> Array.singleton x
    | None -> Array.empty

// Redirected to List.toArray to avoid dependency (see Replacements)
// let ofList (xs: 'T list): 'T[] = List.toArray

// Redirected to Seq.toArray to avoid dependency (see Replacements)
// let ofSeq (xs: 'T seq): 'T[] = Seq.toArray

// Redirected to List.ofArray to avoid dependency (see Replacements)
// let toList (source: 'T[]): 'T list = List.ofArray

// Redirected to Seq.ofArray to avoid dependency (see Replacements)
// let toSeq (source: 'T[]): 'T seq = Seq.ofArray

let where predicate (source: 'T[]) : 'T[] = filter predicate source

let windowed (windowSize: int) (source: 'T[]) : 'T[][] =
    if windowSize <= 0 then
        invalidArg "size" SR.inputMustBePositive

    let len = System.Math.Max(0, source.Length - windowSize + 1)
    let res = ResizeArray<_>(len)

    for i = 0 to len - 1 do
        let slice = getSubArray source i windowSize
        res.Add(slice)

    res |> asArray

let splitInto (chunks: int) (source: 'T[]) : 'T[][] =
    if chunks <= 0 then
        invalidArg "chunks" SR.inputMustBePositive

    if isEmpty source then
        ResizeArray<'T[]>() |> asArray
    else
        let res = ResizeArray<_>(chunks)
        let chunks = System.Math.Min(chunks, source.Length)
        let minChunkSize = source.Length / chunks
        let chunksWithExtraItem = source.Length % chunks

        for i = 0 to chunks - 1 do
            let chunkSize =
                if i < chunksWithExtraItem then
                    minChunkSize + 1
                else
                    minChunkSize

            let start =
                i * minChunkSize + (System.Math.Min(chunksWithExtraItem, i))

            let slice = getSubArray source start chunkSize
            res.Add(slice)

        res |> asArray

// let transpose (arrays: seq<'T[]>): 'T[][] = //TODO:
// Array.transpose will first call Seq.toArray if needed, see Replacements
let transpose (arrays: 'T[][]) : 'T[][] =
    if isEmpty arrays then
        ResizeArray<'T[]>() |> asArray
    else
        let len = arrays.Length
        let firstArray = arrays[0]
        let innerLen = firstArray.Length

        if not (arrays |> forAll (fun a -> a.Length = innerLen)) then
            differentLengths ()

        let res = ResizeArray<_>(innerLen)

        for i = 0 to innerLen - 1 do
            let res2 = ResizeArray<_>(len)

            for j = 0 to len - 1 do
                res2.Add(arrays[j][i])

            res.Add(res2 |> asArray)

        res |> asArray

let distinct<'T when 'T: equality> (xs: 'T[]) : 'T[] =
    let hashSet = System.Collections.Generic.HashSet<'T>()
    xs |> filter (fun x -> hashSet.Add(x))

let distinctBy<'T, 'Key when 'Key: equality>
    (projection: 'T -> 'Key)
    (xs: 'T[])
    : 'T[]
    =
    let hashSet = System.Collections.Generic.HashSet<'Key>()
    xs |> filter (fun x -> hashSet.Add(projection x))

let except<'T when 'T: equality> (itemsToExclude: seq<'T>) (xs: 'T[]) : 'T[] =
    let hashSet = System.Collections.Generic.HashSet<'T>(itemsToExclude)
    xs |> filter (fun x -> hashSet.Add(x))

let countBy<'T, 'Key when 'Key: equality>
    (projection: 'T -> 'Key)
    (xs: 'T[])
    : ('Key * int)[]
    =
    let dict = System.Collections.Generic.Dictionary<'Key, int>()
    let keys = ResizeArray<'Key>()

    for x in xs do
        let key = projection x

        match dict.TryGetValue(key) with
        | true, prev -> dict[key] <- prev + 1
        | false, _ ->
            dict[key] <- 1
            keys.Add(key)

    keys |> asArray |> map (fun key -> key, dict[key])

let groupBy<'T, 'Key when 'Key: equality>
    (projection: 'T -> 'Key)
    (xs: 'T[])
    : ('Key * 'T[])[]
    =
    let dict = System.Collections.Generic.Dictionary<'Key, ResizeArray<'T>>()
    let keys = ResizeArray<'Key>()

    for x in xs do
        let key = projection x

        match dict.TryGetValue(key) with
        | true, prev -> prev.Add(x)
        | false, _ ->
            dict.Add(key, ResizeArray [| x |])
            keys.Add(key)

    keys |> asArray |> map (fun key -> key, dict[key] |> asArray)

let insertAt (index: int) (y: 'T) (xs: 'T[]) : 'T[] =
    let len = xs.Length

    if index < 0 || index > len then
        invalidArg "index" SR.indexOutOfBounds

    let res = ResizeArray<_>(len + 1)

    for i = 0 to (index - 1) do
        res.Add(xs[i])

    res.Add(y)

    for i = index to (len - 1) do
        res.Add(xs[i])

    res |> asArray

let insertManyAt (index: int) (ys: seq<'T>) (xs: 'T[]) : 'T[] =
    let len = xs.Length

    if index < 0 || index > len then
        invalidArg "index" SR.indexOutOfBounds

    let ys = Seq.toArray ys
    let len2 = ys.Length
    let res = ResizeArray<_>(len + len2)

    for i = 0 to (index - 1) do
        res.Add(xs[i])

    for i = 0 to (len2 - 1) do
        res.Add(ys[i])

    for i = index to (len - 1) do
        res.Add(xs[i])

    res |> asArray

let removeAt (index: int) (xs: 'T[]) : 'T[] =
    if index < 0 || index >= xs.Length then
        invalidArg "index" SR.indexOutOfBounds

    let mutable i = -1

    let res =
        xs
        |> filter (fun _ ->
            i <- i + 1
            i <> index
        )

    res

let removeManyAt (index: int) (count: int) (xs: 'T[]) : 'T[] =
    let mutable i = -1
    // incomplete -1, in-progress 0, complete 1
    let mutable status = -1

    let res =
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

    res

let updateAt (index: int) (y: 'T) (xs: 'T[]) : 'T[] =
    let len = xs.Length

    if index < 0 || index >= len then
        invalidArg "index" SR.indexOutOfBounds

    let res = ResizeArray<_>(len)

    for i = 0 to (len - 1) do
        res.Add(
            if i = index then
                y
            else
                xs[i]
        )

    res |> asArray

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
// let sub = getSubArray
