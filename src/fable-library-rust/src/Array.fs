module Array

// open System.Collections.Generic

module SR =
    let indexOutOfBounds = "The index was outside the range of elements in the array."
    let inputArrayWasEmpty = "The input array was empty"
    let inputArrayWasTooLong = "The input array was too long"
    let inputArrayWasTooShort = "The input array has not enough elements"
    let inputMustBeNonNegative = "The input must be non-negative"
    let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
    let differentLengths = "Arrays had different lengths"
    let invalidPermutation = "Not a valid permutation"

let inline indexNotFound() = failwith SR.keyNotFoundAlt
let inline differentLengths() = failwith SR.differentLengths

// module NativeImpl =
//     let empty (): 'T[] = Array.empty
//     let create (count: int) (value: 'T): 'T[] = Array.create count value
//     let zeroCreate (count: int): 'T[] = Array.zeroCreate count
//     let singleton (value: 'T): 'T[] = Array.singleton value
//     let isEmpty (source: 'T[]): bool = Array.isEmpty source
//     let length (source: 'T[]): int = Array.length source
//     let item (index: int) (source: 'T[]): 'T = Array.item index source
//     let get (source: 'T[]) (index: int): 'T = Array.get source index
//     let set (source: 'T[]) (index: int) (value: 'T): unit = Array.set source index value
//     let copy (source: 'T[]): 'T[] = Array.copy source

let inline allocateArray (count: int): 'T[] =
    Array.create count Unchecked.defaultof<_>

let tryItem (index: int) (source: 'T[]): 'T option =
    if index < 0 || index >= source.Length then None
    else Some source.[index]

let reverse (source: 'T[]): 'T[] =
    let len = source.Length
    let res = allocateArray len
    for i = 0 to len - 1 do
        res.[len - 1 - i] <- source.[i]
    res

let filter (predicate: 'T -> bool) (source: 'T[]): 'T[] =
    let res = ResizeArray<'T>()
    for i = 0 to source.Length - 1 do
        if predicate source.[i] then
            res.Add(source.[i])
    res.ToArray()

let fill (target: 'T[]) (targetIndex: int) (count: int) (value: 'T): unit =
    if targetIndex < 0 || targetIndex + count > target.Length then
        invalidArg "index" SR.indexOutOfBounds
    let len = target.Length
    for i = targetIndex to targetIndex + count - 1 do
        target.[i] <- value

let getSubArray (source: 'T[]) (startIndex: int) (count: int): 'T[] =
    if startIndex < 0 || startIndex + count > source.Length then
        invalidArg "index" SR.indexOutOfBounds
    let res = allocateArray count
    for i = 0 to count - 1 do
        res.[i] <- source.[startIndex + i]
    res

let exactlyOne (source: 'T[]): 'T =
    if source.Length = 1 then source.[0]
    elif Array.isEmpty source
    then invalidArg "source" SR.inputArrayWasEmpty
    else invalidArg "source" SR.inputArrayWasTooLong

let tryExactlyOne (source: 'T[]): 'T option =
    if source.Length = 1
    then Some (source.[0])
    else None

let head (source: 'T[]): 'T =
    if Array.isEmpty source
    then invalidArg "source" SR.inputArrayWasEmpty
    else source.[0]

let tryHead (source: 'T[]): 'T option =
    if Array.isEmpty source
    then None
    else Some source.[0]

let last (source: 'T[]): 'T =
    if Array.isEmpty source
    then invalidArg "source" SR.inputArrayWasEmpty
    else source.[source.Length - 1]

let tryLast (source: 'T[]): 'T option =
    if Array.isEmpty source
    then None
    else Some source.[source.Length - 1]

let tail (source: 'T[]): 'T[] =
    if Array.isEmpty source then
        invalidArg "source" SR.inputArrayWasTooShort
    getSubArray source 1 (source.Length - 1)

let append (source1: 'T[]) (source2: 'T[]): 'T[] =
    let len1 = source1.Length
    let len2 = source2.Length
    let res = allocateArray (len1 + len2)
    for i = 0 to len1 - 1 do
        res.[i] <- source1.[i]
    for i = 0 to len2 - 1 do
        res.[i + len1] <- source2.[i]
    res

let mapIndexed (f: int -> 'T -> 'U) (source: 'T[]): 'U[] =
    let len = source.Length
    let target = allocateArray len
    for i = 0 to len - 1 do
        target.[i] <- f i source.[i]
    target

let map (f: 'T -> 'U) (source: 'T[]): 'U[] =
    let len = source.Length
    let target = allocateArray len
    for i = 0 to len - 1 do
        target.[i] <- f source.[i]
    target

let mapIndexed2 (f: int->'T1->'T2->'U) (source1: 'T1[]) (source2: 'T2[]): 'U[] =
    if source1.Length <> source2.Length then differentLengths()
    let res = allocateArray source1.Length
    for i = 0 to source1.Length - 1 do
        res.[i] <- f i source1.[i] source2.[i]
    res

let map2 (f: 'T1->'T2->'U) (source1: 'T1[]) (source2: 'T2[]): 'U[] =
    if source1.Length <> source2.Length then differentLengths()
    let res = allocateArray source1.Length
    for i = 0 to source1.Length - 1 do
        res.[i] <- f source1.[i] source2.[i]
    res

let mapIndexed3 (f: int->'T1->'T2->'T3->'U) (source1: 'T1[]) (source2: 'T2[]) (source3: 'T3[]): 'U[] =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then differentLengths()
    let res = allocateArray source1.Length
    for i = 0 to source1.Length - 1 do
        res.[i] <- f i source1.[i] source2.[i] source3.[i]
    res

let map3 (f: 'T1->'T2->'T3->'U) (source1: 'T1[]) (source2: 'T2[]) (source3: 'T3[]): 'U[] =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then differentLengths()
    let res = allocateArray source1.Length
    for i = 0 to source1.Length - 1 do
        res.[i] <- f source1.[i] source2.[i] source3.[i]
    res

// let mapFold<'T, 'State, 'Result> (mapping: 'State -> 'T -> 'Result * 'State) state (source: 'T[]) =
//     match source.Length with
//     | 0 -> [||], state
//     | len ->
//         let mutable acc = state
//         let res = allocateArray len
//         for i = 0 to source.Length - 1 do
//             let h,s = mapping acc source.[i]
//             res.[i] <- h
//             acc <- s
//         res, acc

// let mapFoldBack<'T, 'State, 'Result> (mapping: 'T -> 'State -> 'Result * 'State) (source: 'T[]) state =
//     match source.Length with
//     | 0 -> [||], state
//     | len ->
//         let mutable acc = state
//         let res = allocateArray len
//         for i = source.Length - 1 downto 0 do
//             let h,s = mapping source.[i] acc
//             res.[i] <- h
//             acc <- s
//         res, acc

let indexed (source: 'T[]) =
    let len = source.Length
    let target = Array.create len (0, Unchecked.defaultof<_>)
    for i = 0 to (len - 1) do
        target.[i] <- (i, source.[i])
    target

let truncate (count: int) (source: 'T[]): 'T[] =
    let count =
        if count < 0 then 0
        elif count > source.Length then source.Length
        else count
    getSubArray source 0 count

// let concat (arrays: 'T[] seq): 'T[] =
//     let arrays =
//         if isDynamicArrayImpl arrays then arrays :?> 'T[][] // avoid extra copy
//         else arrayFrom arrays
//     match arrays.Length with
//     | 0 -> [||]
//     | 1 -> arrays.[0]
//     | _ ->
//         let mutable totalIdx = 0
//         let mutable totalLength = 0
//         for arr in arrays do
//             totalLength <- totalLength + arr.Length
//         let res = allocateArray totalLength
//         for arr in arrays do
//             for j = 0 to (arr.Length - 1) do
//                 res.[totalIdx] <- arr.[j]
//                 totalIdx <- totalIdx + 1
//         res

// let collect (mapping: 'T -> 'U[]) (source: 'T[]): 'U[] =
//     let mapped = map mapping source Unchecked.defaultof<_>
//     concat mapped cons
//     // collectImpl mapping source // flatMap not widely available yet

let where predicate (source: 'T[]) =
    filter predicate source

// let contains (value: 'T) (source: 'T[]) ([<Inject>] eq: IEqualityComparer<'T>) =
//     let rec loop i =
//         if i >= source.Length
//         then false
//         else
//             if eq.Equals (value, source.[i]) then true
//             else loop (i + 1)
//     loop 0

let initialize count (initializer: int -> 'T): 'T[] =
    if count < 0 then invalidArg "count" SR.inputMustBeNonNegative
    let res = allocateArray count
    for i = 0 to count - 1 do
        res.[i] <- initializer i
    res

let pairwise (source: 'T[]) =
    if source.Length < 2 then [||]
    else
        let len = source.Length - 1
        let res = allocateArray len
        for i = 0 to len - 1 do
            res.[i] <- source.[i], source.[i + 1]
        res

let partition (predicate: 'T -> bool) (source: 'T[]) =
    let res1 = ResizeArray<'T>()
    let res2 = ResizeArray<'T>()
    for i = 0 to source.Length - 1 do
        if predicate source.[i]
        then res1.Add(source.[i])
        else res2.Add(source.[i])
    res1.ToArray(), res2.ToArray()

let reduce reduction (source: 'T[]) =
    if Array.isEmpty source then invalidOp SR.inputArrayWasEmpty
    let folder i acc x = if i = 0 then x else reduction acc x
    let mutable acc = source.[0]
    for i = 0 to source.Length - 1 do
        acc <- folder i acc source.[i]
    acc

let reduceBack reduction (source: 'T[]) =
    if Array.isEmpty source then invalidOp SR.inputArrayWasEmpty
    let folder i x acc = if i = 0 then x else reduction acc x
    let len = source.Length
    let mutable acc = source.[len - 1]
    for i = 1 to len do
        acc <- folder (i - 1) source.[len - i] acc
    acc

let replicate count initial =
    initialize count (fun _ -> initial)

// let scan<'T, 'State> folder (state: 'State) (source: 'T[]) =
//     let res = allocateArray (source.Length + 1)
//     res.[0] <- state
//     for i = 0 to source.Length - 1 do
//         res.[i + 1] <- folder res.[i] source.[i]
//     res

// let scanBack<'T, 'State> folder (source: 'T[]) (state: 'State) =
//     let res = allocateArray (source.Length + 1)
//     res.[source.Length] <- state
//     for i = source.Length - 1 downto 0 do
//         res.[i] <- folder source.[i] res.[i + 1]
//     res

let skip count (source: 'T[]): 'T[] =
    if count > source.Length then
        invalidArg "source" SR.inputArrayWasTooShort
    let count = if count < 0 then 0 else count
    getSubArray source count (source.Length - count)

let skipWhile (predicate: 'T -> bool) (source: 'T[]): 'T[] =
    let mutable count = 0
    while count < source.Length && predicate source.[count] do
        count <- count + 1
    getSubArray source count (source.Length - count)

let take count (source: 'T[]): 'T[] =
    if count < 0 then
        invalidArg "count" SR.inputMustBeNonNegative
    if count > source.Length then
        invalidArg "source" SR.inputArrayWasTooShort
    getSubArray source 0 count

let takeWhile (predicate: 'T -> bool) (source: 'T[]): 'T[] =
    let mutable count = 0
    while count < source.Length && predicate source.[count] do
        count <- count + 1
    getSubArray source 0 count

// let addInPlace (x: 'T) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "source" "Typed arrays not supported"
//     pushImpl source x |> ignore

// let addRangeInPlace (range: seq<'T>) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "source" "Typed arrays not supported"
//     for x in range do
//         addInPlace x source

// let insertRangeInPlace index (range: seq<'T>) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "source" "Typed arrays not supported"
//     let mutable i = index
//     for x in range do
//         insertImpl source i x |> ignore
//         i <- i + 1

// let removeInPlace (item: 'T) (source: 'T[]) =
//     // if isTypedArrayImpl source then invalidArg "source" "Typed arrays not supported"
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

// // TODO: Check array lengths
// let copyTo (source: 'T[]) sourceIndex (target: 'T[]) targetIndex count =
//     let diff = targetIndex - sourceIndex
//     for i = sourceIndex to sourceIndex + count - 1 do
//         target.[i + diff] <- source.[i]

// // More performant method to copy arrays, see #2352
// let copyToTypedArray (source: 'T[]) sourceIndex (target: 'T[]) targetIndex count =
//     try
//         Helpers.copyToTypedArray source sourceIndex target targetIndex count
//     with _ ->
//         // If these are not typed arrays (e.g. they come from JS), default to `copyTo`
//         copyTo source sourceIndex target targetIndex count

// // Performance test for above method
// // let numloops = 10000

// // do
// //     let src: uint8[] = Array.allocateArray 16384
// //     let trg: uint8[] = Array.allocateArray 131072

// //     measureTime <| fun () ->
// //         for _ in 1 .. numloops do
// //           let rec loopi i =
// //             if i < trg.Length then
// //               Array.blit src 0 trg i src.Length
// //               loopi (i + src.Length) in loopi 0

// // do
// //     let src: char[] = Array.allocateArray 16384
// //     let trg: char[] = Array.allocateArray 131072

// //     measureTime <| fun () ->
// //         for _ in 1 .. numloops do
// //           let rec loopi i =
// //             if i < trg.Length then
// //               Array.blit src 0 trg i src.Length
// //               loopi (i + src.Length) in loopi 0

// let indexOf (source: 'T[]) (item: 'T) (start: int option) (count: int option) =
//     let start = defaultArg start 0
//     let i = indexOfImpl source item start
//     if count.IsSome && i >= start + count.Value then -1 else i

let tryFind (predicate: 'T -> bool) (source: 'T[]): 'T option =
    let rec loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i >= source.Length then None
        elif predicate source.[i] then Some source.[i]
        else loop (i + 1) predicate source
    loop 0 predicate source

let find (predicate: 'T -> bool) (source: 'T[]): 'T =
    match tryFind predicate source with
    | Some res -> res
    | None -> indexNotFound()

let tryFindIndex (predicate: 'T -> bool) (source: 'T[]): int option =
    let rec loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i >= source.Length then None
        elif predicate source.[i] then Some i
        else loop (i + 1) predicate source
    loop 0 predicate source

let findIndex (predicate: 'T -> bool) (source: 'T[]): int =
    match tryFindIndex predicate source with
    | Some res -> res
    | None -> indexNotFound()

let tryFindBack (predicate: 'T -> bool) (source: 'T[]): 'T option =
    let rec loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i < 0 then None
        elif predicate source.[i] then Some source.[i]
        else loop (i - 1) predicate source
    loop (source.Length - 1) predicate source

let findBack (predicate: 'T -> bool) (source: 'T[]): 'T =
    match tryFindBack predicate source with
    | Some res -> res
    | None -> indexNotFound()

let tryFindIndexBack (predicate: 'T -> bool) (source: 'T[]): int option =
    let rec loop i (predicate: 'T -> bool) (source: 'T[]) =
        if i < 0 then None
        elif predicate source.[i] then Some i
        else loop (i - 1) predicate source
    loop (source.Length - 1) predicate source

let findIndexBack (predicate: 'T -> bool) (source: 'T[]): int =
    match tryFindIndexBack predicate source with
    | Some res -> res
    | None -> indexNotFound()

let findLastIndex (predicate: 'T -> bool) (source: 'T[]): int =
    match tryFindIndexBack predicate source with
    | Some res -> res
    | None -> -1

let tryPick (chooser: 'T -> 'U option) (source: 'T[]): 'U option =
    let rec loop i (chooser: 'T -> 'U option) (source: 'T[]) =
        if i >= source.Length then
            None
        else
            match chooser source.[i] with
            | None -> loop (i + 1) chooser source
            | res -> res
    loop 0 chooser source

let pick (chooser: 'T -> 'U option) (source: 'T[]): 'U =
    match tryPick chooser source with
    | Some res -> res
    | None -> indexNotFound()

let choose (chooser: 'T -> 'U option) (source: 'T[]): 'U[] =
    let res = ResizeArray<'U>()
    for i = 0 to source.Length - 1 do
        match chooser source.[i] with
        | Some x -> res.Add(x)
        | None -> ()
    res.ToArray()

let fold folder (state: 'State) (source: 'T[]) =
    let mutable acc = state
    for i = 0 to source.Length - 1 do
        acc <- folder acc source.[i]
    acc

let foldBack folder (source: 'T[]) (state: 'State) =
    let mutable acc = state
    let len = source.Length
    for i = 1 to len do
        acc <- folder source.[len - i] acc
    acc

let fold2 folder (state: 'State) (source1: 'T1[]) (source2: 'T2[]) =
    let mutable acc = state
    if source1.Length <> source2.Length then differentLengths()
    for i = 0 to source1.Length - 1 do
        acc <- folder acc source1.[i] source2.[i]
    acc

let foldBack2 folder (source1: 'T1[]) (source2: 'T2[]) (state: 'State) =
    let mutable acc = state
    if source1.Length <> source2.Length then differentLengths()
    let len = source1.Length
    for i = 1 to len do
        acc <- folder source1.[len - i] source2.[len - i] acc
    acc

let forAll predicate (source: 'T[]) =
    let mutable i = 0
    let mutable res = true
    while i < source.Length && res do
        res <- predicate source.[i]
        i <- i + 1
    res

let forAll2 predicate (source1: 'T1[]) (source2: 'T2[]) =
    if source1.Length <> source2.Length then differentLengths()
    let mutable i = 0
    let mutable res = true
    while i < source1.Length && res do
        res <- predicate source1.[i] source2.[i]
        i <- i + 1
    res

let iterate action (source: 'T[]) =
    for i = 0 to source.Length - 1 do
        action source.[i]

let iterateIndexed action (source: 'T[]) =
    for i = 0 to source.Length - 1 do
        action i source.[i]

let iterate2 action (source1: 'T[]) (source2: 'T[]) =
    if source1.Length <> source2.Length then differentLengths()
    for i = 0 to source1.Length - 1 do
        action source1.[i] source2.[i]

let iterateIndexed2 action (source1: 'T[]) (source2: 'T[]) =
    if source1.Length <> source2.Length then differentLengths()
    for i = 0 to source1.Length - 1 do
        action i source1.[i] source2.[i]

let permute (indexMap: int -> int) (source: 'T[]) =
    let len = source.Length
    let res = Array.copy source
    let checkFlags = allocateArray len
    iterateIndexed (fun i x ->
        let j = indexMap i
        if j < 0 || j >= len then
            invalidOp SR.invalidPermutation
        res.[j] <- x
        checkFlags.[j] <- 1) source
    let isValid = checkFlags |> forAll ((=) 1)
    if not isValid then
        invalidOp SR.invalidPermutation
    res

// let setSlice (target: 'T[]) (lower: int option) (upper: int option) (source: 'T[]) =
//     let lower = defaultArg lower 0
//     let upper = defaultArg upper 0
//     let length = (if upper > 0 then upper else target.Length - 1) - lower
//     // can't cast to TypedArray, so can't use TypedArray-specific methods
//     // if isTypedArrayImpl target && source.Length <= length then
//     //     typedArraySetImpl target source lower
//     // else
//     for i = 0 to length do
//         target.[i + lower] <- source.[i]

// let sortInPlaceBy (projection: 'a->'b) (xs: 'a[]) ([<Inject>] comparer: IComparer<'b>): unit =
//     sortInPlaceWithImpl (fun x y -> comparer.Compare(projection x, projection y)) xs

// let sortInPlace (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) =
//     sortInPlaceWithImpl (fun x y -> comparer.Compare(x, y)) xs

// let inline internal sortInPlaceWith (comparer: 'T -> 'T -> int) (xs: 'T[]) =
//     sortInPlaceWithImpl comparer xs
//     xs

// let sort (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>): 'T[] =
//     sortInPlaceWith (fun x y -> comparer.Compare(x, y)) (copyImpl xs)

// let sortBy (projection: 'a->'b) (xs: 'a[]) ([<Inject>] comparer: IComparer<'b>): 'a[] =
//     sortInPlaceWith (fun x y -> comparer.Compare(projection x, projection y)) (copyImpl xs)

// let sortDescending (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>): 'T[] =
//     sortInPlaceWith (fun x y -> comparer.Compare(x, y) * -1) (copyImpl xs)

// let sortByDescending (projection: 'a->'b) (xs: 'a[]) ([<Inject>] comparer: IComparer<'b>): 'a[] =
//     sortInPlaceWith (fun x y -> comparer.Compare(projection x, projection y) * -1) (copyImpl xs)

// let sortWith (comparer: 'T -> 'T -> int) (xs: 'T[]): 'T[] =
//     sortInPlaceWith comparer (copyImpl xs)

// let allPairs (xs: 'T1[]) (ys: 'T2[]): ('T1 * 'T2)[] =
//     let len1 = xs.Length
//     let len2 = ys.Length
//     let res = allocateArray (len1 * len2)
//     for i = 0 to xs.Length-1 do
//         for j = 0 to ys.Length-1 do
//             res.[i * len2 + j] <- (xs.[i], ys.[j])
//     res

let unfold<'T, 'State> (generator: 'State -> ('T*'State) option) (state: 'State): 'T[] =
    let res = ResizeArray<'T>()
    let rec loop state =
        match generator state with
        | None -> ()
        | Some (x, s) ->
            res.Add(x)
            loop s
    loop state
    res.ToArray()

let unzip (source: ('T1 * 'T2)[]) =
    let len = source.Length
    let res1 = allocateArray len
    let res2 = allocateArray len
    iterateIndexed (fun i (item1, item2) ->
        res1.[i] <- item1
        res2.[i] <- item2
    ) source
    res1, res2

let unzip3 (source: ('T1 * 'T2 * 'T3)[]) =
    let len = source.Length
    let res1 = allocateArray len
    let res2 = allocateArray len
    let res3 = allocateArray len
    iterateIndexed (fun i (item1, item2, item3) ->
        res1.[i] <- item1
        res2.[i] <- item2
        res3.[i] <- item3
    ) source
    res1, res2, res3

let zip (source1: 'T1[]) (source2: 'T2[]) =
    map2 (fun x y -> x, y) source1 source2

let zip3 (source1: 'T1[]) (source2: 'T2[]) (source3: 'T3[]) =
    map3 (fun x y z -> x, y, z) source1 source2 source3

// let chunkBySize (chunkSize: int) (source: 'T[]): 'T[][] =
//     if chunkSize < 1 then invalidArg "size" "The input must be positive."
//     if Array.isEmpty source then [| [||] |]
//     else
//         let res: 'T[][] = [||]
//         // add each chunk to the res
//         for x = 0 to int(System.Math.Ceiling(float(source.Length) / float(chunkSize))) - 1 do
//             let start = x * chunkSize
//             let slice = getSubArray source start chunkSize
//             pushImpl res slice |> ignore
//         res

let splitAt (index: int) (source: 'T[]): 'T[] * 'T[] =
    if index < 0 || index > source.Length then
        invalidArg "index" SR.indexOutOfBounds
    getSubArray source 0 index, getSubArray source index (source.Length - index)

// let compareWith (comparer: 'T -> 'T -> int) (source1: 'T[]) (source2: 'T[]) =
//     if isNull source1 then
//         if isNull source2 then 0 else -1
//     elif isNull source2 then
//         1
//     else
//         let mutable i = 0
//         let mutable res = 0
//         let length1 = source1.Length
//         let length2 = source2.Length
//         if length1 > length2 then 1
//         elif length1 < length2 then -1
//         else
//             while i < length1 && res = 0 do
//                 res <- comparer source1.[i] source2.[i]
//                 i <- i + 1
//             res

// let equalsWith (comparer: 'T -> 'T -> int) (source1: 'T[]) (source2: 'T[]) =
//     compareWith compare source1 source2 = 0

// let rec existsOffset predicate (source: 'T[]) index =
//     if index = source.Length then false
//     else predicate source.[index] || existsOffset predicate source (index+1)

// let exists predicate source =
//     existsOffset predicate source 0

// let rec existsOffset2 predicate (source1: _[]) (source2: _[]) index =
//     if index = source1.Length then false
//     else predicate source1.[index] source2.[index] || existsOffset2 predicate source1 source2 (index+1)

// let rec exists2 predicate (source1: _[]) (source2: _[]) =
//     if source1.Length <> source2.Length then differentLengths()
//     existsOffset2 predicate source1 source2 0

// let sum (source: 'T[]) ([<Inject>] adder: IGenericAdder<'T>): 'T =
//     let mutable acc = adder.GetZero()
//     for i = 0 to source.Length - 1 do
//         acc <- adder.Add(acc, source.[i])
//     acc

// let sumBy (projection: 'T -> 'T2) (source: 'T[]) ([<Inject>] adder: IGenericAdder<'T2>): 'T2 =
//     let mutable acc = adder.GetZero()
//     for i = 0 to source.Length - 1 do
//         acc <- adder.Add(acc, projection source.[i])
//     acc

// let maxBy (projection: 'a->'b) (xs: 'a[]) ([<Inject>] comparer: IComparer<'b>): 'a =
//     reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

// let max (xs: 'a[]) ([<Inject>] comparer: IComparer<'a>): 'a =
//     reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) xs

// let minBy (projection: 'a->'b) (xs: 'a[]) ([<Inject>] comparer: IComparer<'b>): 'a =
//     reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then x else y) xs

// let min (xs: 'a[]) ([<Inject>] comparer: IComparer<'a>): 'a =
//     reduce (fun x y -> if comparer.Compare(y, x) > 0 then x else y) xs

// let average (source: 'T []) ([<Inject>] averager: IGenericAverager<'T>): 'T =
//     if Array.isEmpty source then
//         invalidArg "source" SR.inputArrayWasEmpty
//     let mutable total = averager.GetZero()
//     for i = 0 to source.Length - 1 do
//         total <- averager.Add(total, source.[i])
//     averager.DivideByInt(total, source.Length)

// let averageBy (projection: 'T -> 'T2) (source: 'T[]) ([<Inject>] averager: IGenericAverager<'T2>): 'T2 =
//     if Array.isEmpty source then
//         invalidArg "source" SR.inputArrayWasEmpty
//     let mutable total = averager.GetZero()
//     for i = 0 to source.Length - 1 do
//         total <- averager.Add(total, projection source.[i])
//     averager.DivideByInt(total, source.Length)

// Redirected to List.ofArray to avoid dependency (see Replacements)
// let toList (source: 'T[]) = List.ofArray

// Redirected to Seq.ofArray to avoid dependency (see Replacements)
// let toSeq (source: 'T[]) = Seq.ofArray

// let windowed (windowSize: int) (source: 'T[]): 'T[][] =
//     if windowSize <= 0 then
//         failwith "windowSize must be positive"
//     let res = FSharp.Core.Operators.max 0 (source.Length - windowSize) |> allocateArray
//     for i = windowSize to source.Length do
//         res.[i - windowSize] <- source.[i-windowSize..i-1]
//     res

// let splitInto (chunks: int) (source: 'T[]): 'T[][] =
//     if chunks < 1 then
//         invalidArg "chunks" "The input must be positive."
//     if Array.isEmpty source then
//         [| [||] |]
//     else
//         let res: 'T[][] = [||]
//         let chunks = FSharp.Core.Operators.min chunks source.Length
//         let minChunkSize = source.Length / chunks
//         let chunksWithExtraItem = source.Length % chunks
//         for i = 0 to chunks - 1 do
//             let chunkSize = if i < chunksWithExtraItem then minChunkSize + 1 else minChunkSize
//             let start = i * minChunkSize + (FSharp.Core.Operators.min chunksWithExtraItem i)
//             let slice = getSubArray source start chunkSize
//             pushImpl res slice |> ignore
//         res

// let transpose (arrays: 'T[] seq): 'T[][] =
//     let arrays =
//         if isDynamicArrayImpl arrays then arrays :?> 'T[][] // avoid extra copy
//         else arrayFrom arrays
//     let len = arrays.Length
//     match len with
//     | 0 -> [||]
//     | _ ->
//         let firstArray = arrays.[0]
//         let lenInner = firstArray.Length
//         if arrays |> forAll (fun a -> a.Length = lenInner) |> not then
//             differentLengths()
//         let res: 'T[][] = allocateArray lenInner
//         for i in 0..lenInner-1 do
//             res.[i] <- allocateArray len
//             for j in 0..len-1 do
//                 res.[i].[j] <- arrays.[j].[i]
//         res

// let insertAt (index: int) (y: 'T) (xs: 'T[]): 'T[] =
//     let len = xs.Length
//     if index < 0 || index > len then
//         invalidArg "index" SR.indexOutOfBounds
//     let target = zeroCreateFrom xs (len + 1)
//     for i = 0 to (index - 1) do
//         target.[i] <- xs.[i]
//     target.[index] <- y
//     for i = index to (len - 1) do
//         target.[i + 1] <- xs.[i]
//     target

// let insertManyAt (index: int) (ys: seq<'T>) (xs: 'T[]): 'T[] =
//     let len = xs.Length
//     if index < 0 || index > len then
//         invalidArg "index" SR.indexOutOfBounds
//     let ys = arrayFrom ys
//     let len2 = ys.Length
//     let target = zeroCreateFrom xs (len + len2)
//     for i = 0 to (index - 1) do
//         target.[i] <- xs.[i]
//     for i = 0 to (len2 - 1) do
//         target.[index + i] <- ys.[i]
//     for i = index to (len - 1) do
//         target.[i + len2] <- xs.[i]
//     target

// let removeAt (index: int) (xs: 'T[]): 'T[] =
//     if index < 0 || index >= xs.Length then
//         invalidArg "index" SR.indexOutOfBounds
//     let mutable i = -1
//     xs |> filter (fun _ ->
//         i <- i + 1
//         i <> index)

// let removeManyAt (index: int) (count: int) (xs: 'T[]): 'T[] =
//     let mutable i = -1
//     // incomplete -1, in-progress 0, complete 1
//     let mutable status = -1
//     let ys =
//         xs |> filter (fun _ ->
//             i <- i + 1
//             if i = index then
//                 status <- 0
//                 false
//             elif i > index then
//                 if i < index + count then
//                     false
//                 else
//                     status <- 1
//                     true
//             else true)
//     let status =
//         if status = 0 && i + 1 = index + count then 1
//         else status
//     if status < 1 then
//         // F# always says the wrong parameter is index but the problem may be count
//         let arg = if status < 0 then "index" else "count"
//         invalidArg arg SR.indexOutOfBounds
//     ys

// let updateAt (index: int) (y: 'T) (xs: 'T[]): 'T[] =
//     let len = xs.Length
//     if index < 0 || index >= len then
//         invalidArg "index" SR.indexOutOfBounds
//     let target = zeroCreateFrom xs len
//     for i = 0 to (len - 1) do
//         target.[i] <- if i = index then y else xs.[i]
//     target

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
