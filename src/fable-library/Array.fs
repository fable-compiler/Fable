module ArrayModule

// Disables warn:1204 raised by use of LanguagePrimitives.ErrorStrings.*
#nowarn "1204"

open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop

open Native
open Native.Helpers

let private indexNotFound () =
    failwith
        "An index satisfying the predicate was not found in the collection."

let private differentLengths () = failwith "Arrays had different lengths"

// Pay attention when benchmarking to append and filter functions below
// if implementing via native JS array .concat() and .filter() do not fall behind due to js-native transitions.

// Don't use native JS Array.prototype.concat as it doesn't work with typed arrays
let append
    (array1: 'T[])
    (array2: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    : 'T[]
    =
    let len1 = array1.Length
    let len2 = array2.Length
    let newArray = allocateArrayFromCons cons (len1 + len2)

    for i = 0 to len1 - 1 do
        newArray.[i] <- array1.[i]

    for i = 0 to len2 - 1 do
        newArray.[i + len1] <- array2.[i]

    newArray

let filter (predicate: 'T -> bool) (array: 'T[]) = filterImpl predicate array

// intentionally returns target instead of unit
let fill (target: 'T[]) (targetIndex: int) (count: int) (value: 'T) : 'T[] =
    fillImpl target value targetIndex count

let getSubArray (array: 'T[]) (start: int) (count: int) : 'T[] =
    subArrayImpl array start count

let last (array: 'T[]) =
    if array.Length = 0 then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    array.[array.Length - 1]

let tryLast (array: 'T[]) =
    if array.Length = 0 then
        None
    else
        Some array.[array.Length - 1]

let mapIndexed
    (f: int -> 'T -> 'U)
    (source: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    let len = source.Length
    let target = allocateArrayFromCons cons len

    for i = 0 to (len - 1) do
        target.[i] <- f i source.[i]

    target

let map
    (f: 'T -> 'U)
    (source: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    let len = source.Length
    let target = allocateArrayFromCons cons len

    for i = 0 to (len - 1) do
        target.[i] <- f source.[i]

    target

let mapIndexed2
    (f: int -> 'T1 -> 'T2 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    if source1.Length <> source2.Length then
        failwith "Arrays had different lengths"

    let result = allocateArrayFromCons cons source1.Length

    for i = 0 to source1.Length - 1 do
        result.[i] <- f i source1.[i] source2.[i]

    result

let map2
    (f: 'T1 -> 'T2 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    if source1.Length <> source2.Length then
        failwith "Arrays had different lengths"

    let result = allocateArrayFromCons cons source1.Length

    for i = 0 to source1.Length - 1 do
        result.[i] <- f source1.[i] source2.[i]

    result

let mapIndexed3
    (f: int -> 'T1 -> 'T2 -> 'T3 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    (source3: 'T3[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then
        failwith "Arrays had different lengths"

    let result = allocateArrayFromCons cons source1.Length

    for i = 0 to source1.Length - 1 do
        result.[i] <- f i source1.[i] source2.[i] source3.[i]

    result

let map3
    (f: 'T1 -> 'T2 -> 'T3 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    (source3: 'T3[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then
        failwith "Arrays had different lengths"

    let result = allocateArrayFromCons cons source1.Length

    for i = 0 to source1.Length - 1 do
        result.[i] <- f source1.[i] source2.[i] source3.[i]

    result

let mapFold<'T, 'State, 'Result>
    (mapping: 'State -> 'T -> 'Result * 'State)
    state
    (array: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'Result>)
    =
    match array.Length with
    | 0 -> [||], state
    | len ->
        let mutable acc = state
        let res = allocateArrayFromCons cons len

        for i = 0 to array.Length - 1 do
            let h, s = mapping acc array.[i]
            res.[i] <- h
            acc <- s

        res, acc

let mapFoldBack<'T, 'State, 'Result>
    (mapping: 'T -> 'State -> 'Result * 'State)
    (array: 'T[])
    state
    ([<OptionalArgument; Inject>] cons: Cons<'Result>)
    =
    match array.Length with
    | 0 -> [||], state
    | len ->
        let mutable acc = state
        let res = allocateArrayFromCons cons len

        for i = array.Length - 1 downto 0 do
            let h, s = mapping array.[i] acc
            res.[i] <- h
            acc <- s

        res, acc

let indexed (source: 'T[]) =
    let len = source.Length
    let target = allocateArray len

    for i = 0 to (len - 1) do
        target.[i] <- i, source.[i]

    target

let truncate (count: int) (array: 'T[]) : 'T[] =
    let count = max 0 count
    subArrayImpl array 0 count

let concat
    (arrays: 'T[] seq)
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    : 'T[]
    =
    let arrays =
        if isDynamicArrayImpl arrays then
            arrays :?> 'T[][] // avoid extra copy
        else
            arrayFrom arrays

    match arrays.Length with
    | 0 -> allocateArrayFromCons cons 0
    | 1 -> arrays.[0]
    | _ ->
        let mutable totalIdx = 0
        let mutable totalLength = 0

        for arr in arrays do
            totalLength <- totalLength + arr.Length

        let result = allocateArrayFromCons cons totalLength

        for arr in arrays do
            for j = 0 to (arr.Length - 1) do
                result.[totalIdx] <- arr.[j]
                totalIdx <- totalIdx + 1

        result

let collect
    (mapping: 'T -> 'U[])
    (array: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    : 'U[]
    =
    let mapped = map mapping array Unchecked.defaultof<_>
    concat mapped cons
// collectImpl mapping array // flatMap not widely available yet

let where predicate (array: _[]) = filterImpl predicate array

let indexOf<'T>
    (array: 'T[])
    (item: 'T)
    (start: int option)
    (count: int option)
    ([<Inject>] eq: IEqualityComparer<'T>)
    =
    let start = defaultArg start 0

    let end' =
        count
        |> Option.map (fun c -> start + c)
        |> Option.defaultValue array.Length

    let rec loop i =
        if i >= end' then
            -1
        else if eq.Equals(item, array.[i]) then
            i
        else
            loop (i + 1)

    loop start

let contains<'T>
    (value: 'T)
    (array: 'T[])
    ([<Inject>] eq: IEqualityComparer<'T>)
    =
    indexOf array value None None eq >= 0

let empty cons = allocateArrayFromCons cons 0

let singleton value ([<OptionalArgument; Inject>] cons: Cons<'T>) =
    let ar = allocateArrayFromCons cons 1
    ar.[0] <- value
    ar

let initialize count initializer ([<OptionalArgument; Inject>] cons: Cons<'T>) =
    if count < 0 then
        invalidArg
            "count"
            LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString

    let result = allocateArrayFromCons cons count

    for i = 0 to count - 1 do
        result.[i] <- initializer i

    result

let pairwise (array: 'T[]) =
    if array.Length < 2 then
        [||]
    else
        let count = array.Length - 1
        let result = allocateArray count

        for i = 0 to count - 1 do
            result.[i] <- array.[i], array.[i + 1]

        result

let replicate count initial ([<OptionalArgument; Inject>] cons: Cons<'T>) =
    // Shorthand version: = initialize count (fun _ -> initial)
    if count < 0 then
        invalidArg
            "count"
            LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString

    let result: 'T array = allocateArrayFromCons cons count

    for i = 0 to result.Length - 1 do
        result.[i] <- initial

    result

let copy (array: 'T[]) =
    // if isTypedArrayImpl array then
    //     let res = allocateArrayFrom array array.Length
    //     for i = 0 to array.Length-1 do
    //         res.[i] <- array.[i]
    //     res
    // else
    copyImpl array

let copyTo
    (source: 'T[])
    (sourceIndex: int)
    (target: 'T[])
    (targetIndex: int)
    (count: int)
    =
    // TODO: Check array lengths
    System.Array.Copy(source, sourceIndex, target, targetIndex, count)

let reverse (array: 'T[]) =
    // if isTypedArrayImpl array then
    //     let res = allocateArrayFrom array array.Length
    //     let mutable j = array.Length-1
    //     for i = 0 to array.Length-1 do
    //         res.[j] <- array.[i]
    //         j <- j - 1
    //     res
    // else
    copyImpl array |> reverseImpl

let scan<'T, 'State>
    folder
    (state: 'State)
    (array: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'State>)
    =
    let res = allocateArrayFromCons cons (array.Length + 1)
    res.[0] <- state

    for i = 0 to array.Length - 1 do
        res.[i + 1] <- folder res.[i] array.[i]

    res

let scanBack<'T, 'State>
    folder
    (array: 'T[])
    (state: 'State)
    ([<OptionalArgument; Inject>] cons: Cons<'State>)
    =
    let res = allocateArrayFromCons cons (array.Length + 1)
    res.[array.Length] <- state

    for i = array.Length - 1 downto 0 do
        res.[i] <- folder array.[i] res.[i + 1]

    res

let skip count (array: 'T[]) ([<OptionalArgument; Inject>] cons: Cons<'T>) =
    if count > array.Length then
        invalidArg "count" "count is greater than array length"

    if count = array.Length then
        allocateArrayFromCons cons 0
    else
        let count =
            if count < 0 then
                0
            else
                count

        skipImpl array count

let skipWhile
    predicate
    (array: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    =
    let mutable count = 0

    while count < array.Length && predicate array.[count] do
        count <- count + 1

    if count = array.Length then
        allocateArrayFromCons cons 0
    else
        skipImpl array count

let take count (array: 'T[]) ([<OptionalArgument; Inject>] cons: Cons<'T>) =
    if count < 0 then
        invalidArg
            "count"
            LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString

    if count > array.Length then
        invalidArg "count" "count is greater than array length"

    if count = 0 then
        allocateArrayFromCons cons 0
    else
        subArrayImpl array 0 count

let takeWhile
    predicate
    (array: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    =
    let mutable count = 0

    while count < array.Length && predicate array.[count] do
        count <- count + 1

    if count = 0 then
        allocateArrayFromCons cons 0
    else
        subArrayImpl array 0 count

let addInPlace (x: 'T) (array: 'T[]) =
    // if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
    pushImpl array x |> ignore

let addRangeInPlace (range: seq<'T>) (array: 'T[]) =
    // if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
    for x in range do
        addInPlace x array

let insertRangeInPlace index (range: seq<'T>) (array: 'T[]) =
    // if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
    let mutable i = index

    for x in range do
        insertImpl array i x |> ignore
        i <- i + 1

let removeInPlace
    (item: 'T)
    (array: 'T[])
    ([<Inject>] eq: IEqualityComparer<'T>)
    =
    let i = indexOf array item None None eq

    if i > -1 then
        spliceImpl array i 1 |> ignore
        true
    else
        false

let removeAllInPlace predicate (array: 'T[]) =
    let rec countRemoveAll count =
        let i = findIndexImpl predicate array

        if i > -1 then
            spliceImpl array i 1 |> ignore
            countRemoveAll count + 1
        else
            count

    countRemoveAll 0

let partition
    (f: 'T -> bool)
    (source: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    =
    let len = source.Length
    let res1 = allocateArrayFromCons cons len
    let res2 = allocateArrayFromCons cons len
    let mutable iTrue = 0
    let mutable iFalse = 0

    for i = 0 to len - 1 do
        if f source.[i] then
            res1.[iTrue] <- source.[i]
            iTrue <- iTrue + 1
        else
            res2.[iFalse] <- source.[i]
            iFalse <- iFalse + 1

    res1 |> truncate iTrue, res2 |> truncate iFalse

let find (predicate: 'T -> bool) (array: 'T[]) : 'T =
    match findImpl predicate array with
    | Some res -> res
    | None -> indexNotFound ()

let tryFind (predicate: 'T -> bool) (array: 'T[]) : 'T option =
    findImpl predicate array

let findIndex (predicate: 'T -> bool) (array: 'T[]) : int =
    match findIndexImpl predicate array with
    | index when index > -1 -> index
    | _ ->
        indexNotFound ()
        -1

let tryFindIndex (predicate: 'T -> bool) (array: 'T[]) : int option =
    match findIndexImpl predicate array with
    | index when index > -1 -> Some index
    | _ -> None

let pick chooser (array: _[]) =
    let rec loop i =
        if i >= array.Length then
            indexNotFound ()
        else
            match chooser array.[i] with
            | None -> loop (i + 1)
            | Some res -> res

    loop 0

let tryPick chooser (array: _[]) =
    let rec loop i =
        if i >= array.Length then
            None
        else
            match chooser array.[i] with
            | None -> loop (i + 1)
            | res -> res

    loop 0

let findBack predicate (array: _[]) =
    let rec loop i =
        if i < 0 then
            indexNotFound ()
        elif predicate array.[i] then
            array.[i]
        else
            loop (i - 1)

    loop (array.Length - 1)

let tryFindBack predicate (array: _[]) =
    let rec loop i =
        if i < 0 then
            None
        elif predicate array.[i] then
            Some array.[i]
        else
            loop (i - 1)

    loop (array.Length - 1)

let findLastIndex predicate (array: _[]) =
    let rec loop i =
        if i < 0 then
            -1
        elif predicate array.[i] then
            i
        else
            loop (i - 1)

    loop (array.Length - 1)

let findIndexBack predicate (array: _[]) =
    let rec loop i =
        if i < 0 then
            indexNotFound ()
            -1
        elif predicate array.[i] then
            i
        else
            loop (i - 1)

    loop (array.Length - 1)

let tryFindIndexBack predicate (array: _[]) =
    let rec loop i =
        if i < 0 then
            None
        elif predicate array.[i] then
            Some i
        else
            loop (i - 1)

    loop (array.Length - 1)

let choose
    (chooser: 'T -> 'U option)
    (array: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'U>)
    =
    let res: 'U[] = [||]

    for i = 0 to array.Length - 1 do
        match chooser array.[i] with
        | None -> ()
        | Some y -> pushImpl res y |> ignore

    match box cons with
    | null -> res // avoid extra copy
    | _ -> map id res cons

let foldIndexed<'T, 'State> folder (state: 'State) (array: 'T[]) =
    // if isTypedArrayImpl array then
    //     let mutable acc = state
    //     for i = 0 to array.Length - 1 do
    //         acc <- folder i acc array.[i]
    //     acc
    // else
    foldIndexedImpl (fun acc x i -> folder i acc x) state array

let fold<'T, 'State> folder (state: 'State) (array: 'T[]) =
    // if isTypedArrayImpl array then
    //     let mutable acc = state
    //     for i = 0 to array.Length - 1 do
    //         acc <- folder acc array.[i]
    //     acc
    // else
    foldImpl folder state array

let iterate action (array: 'T[]) =
    for i = 0 to array.Length - 1 do
        action array.[i]

let iterateIndexed action (array: 'T[]) =
    for i = 0 to array.Length - 1 do
        action i array.[i]

let iterate2 action (array1: 'T1[]) (array2: 'T2[]) =
    if array1.Length <> array2.Length then
        differentLengths ()

    for i = 0 to array1.Length - 1 do
        action array1.[i] array2.[i]

let iterateIndexed2 action (array1: 'T1[]) (array2: 'T2[]) =
    if array1.Length <> array2.Length then
        differentLengths ()

    for i = 0 to array1.Length - 1 do
        action i array1.[i] array2.[i]

let isEmpty (array: 'T[]) = array.Length = 0

let forAll predicate (array: 'T[]) =
    // if isTypedArrayImpl array then
    //     let mutable i = 0
    //     let mutable result = true
    //     while i < array.Length && result do
    //         result <- predicate array.[i]
    //         i <- i + 1
    //     result
    // else
    forAllImpl predicate array

let permute f (array: 'T[]) =
    let size = array.Length
    let res = copyImpl array
    let checkFlags = allocateArray size

    iterateIndexed
        (fun i x ->
            let j = f i

            if j < 0 || j >= size then
                invalidOp "Not a valid permutation"

            res.[j] <- x
            checkFlags.[j] <- 1
        )
        array

    let isValid = checkFlags |> forAllImpl ((=) 1)

    if not isValid then
        invalidOp "Not a valid permutation"

    res

let setSlice
    (target: 'T[])
    (lower: int option)
    (upper: int option)
    (source: 'T[])
    =
    let lower = defaultArg lower 0
    let upper = defaultArg upper -1

    let length =
        (if upper >= 0 then
             upper
         else
             target.Length - 1)
        - lower
    // can't cast to TypedArray, so can't use TypedArray-specific methods
    // if isTypedArrayImpl target && source.Length <= length then
    //     typedArraySetImpl target source lower
    // else
    for i = 0 to length do
        target.[i + lower] <- source.[i]

let sortInPlaceBy
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : unit
    =
    sortInPlaceWithImpl
        (fun x y -> comparer.Compare(projection x, projection y))
        xs

let sortInPlace (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) =
    sortInPlaceWithImpl (fun x y -> comparer.Compare(x, y)) xs

let inline internal sortInPlaceWith (comparer: 'T -> 'T -> int) (xs: 'T[]) =
    sortInPlaceWithImpl comparer xs
    xs

let sort (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) : 'T[] =
    sortInPlaceWith (fun x y -> comparer.Compare(x, y)) (copyImpl xs)

let sortBy
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : 'a[]
    =
    sortInPlaceWith
        (fun x y -> comparer.Compare(projection x, projection y))
        (copyImpl xs)

let sortDescending (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) : 'T[] =
    sortInPlaceWith (fun x y -> comparer.Compare(x, y) * -1) (copyImpl xs)

let sortByDescending
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : 'a[]
    =
    sortInPlaceWith
        (fun x y -> comparer.Compare(projection x, projection y) * -1)
        (copyImpl xs)

let sortWith (comparer: 'T -> 'T -> int) (xs: 'T[]) : 'T[] =
    sortInPlaceWith comparer (copyImpl xs)

let allPairs (xs: 'T1[]) (ys: 'T2[]) : ('T1 * 'T2)[] =
    let len1 = xs.Length
    let len2 = ys.Length
    let res = allocateArray (len1 * len2)

    for i = 0 to xs.Length - 1 do
        for j = 0 to ys.Length - 1 do
            res.[i * len2 + j] <- (xs.[i], ys.[j])

    res

let unfold<'T, 'State>
    (generator: 'State -> ('T * 'State) option)
    (state: 'State)
    : 'T[]
    =
    let res: 'T[] = [||]

    let rec loop state =
        match generator state with
        | None -> ()
        | Some(x, s) ->
            pushImpl res x |> ignore
            loop s

    loop state
    res

// TODO: We should pass Cons<'T> here (and unzip3) but 'a and 'b may differ
let unzip (array: _[]) =
    let len = array.Length
    let res1 = allocateArray len
    let res2 = allocateArray len

    iterateIndexed
        (fun i (item1, item2) ->
            res1.[i] <- item1
            res2.[i] <- item2
        )
        array

    res1, res2

let unzip3 (array: _[]) =
    let len = array.Length
    let res1 = allocateArray len
    let res2 = allocateArray len
    let res3 = allocateArray len

    iterateIndexed
        (fun i (item1, item2, item3) ->
            res1.[i] <- item1
            res2.[i] <- item2
            res3.[i] <- item3
        )
        array

    res1, res2, res3

let zip (array1: 'T[]) (array2: 'U[]) =
    // Shorthand version: map2 (fun x y -> x, y) array1 array2
    if array1.Length <> array2.Length then
        differentLengths ()

    let result = allocateArray array1.Length

    for i = 0 to array1.Length - 1 do
        result.[i] <- array1.[i], array2.[i]

    result

let zip3 (array1: 'T[]) (array2: 'U[]) (array3: 'V[]) =
    // Shorthand version: map3 (fun x y z -> x, y, z) array1 array2 array3
    if array1.Length <> array2.Length || array2.Length <> array3.Length then
        differentLengths ()

    let result = allocateArray array1.Length

    for i = 0 to array1.Length - 1 do
        result.[i] <- array1.[i], array2.[i], array3.[i]

    result

let chunkBySize (chunkSize: int) (array: 'T[]) : 'T[][] =
    if chunkSize < 1 then
        invalidArg "size" "The input must be positive."

    if array.Length = 0 then
        [| [||] |]
    else
        let result: 'T[][] = [||]
        // add each chunk to the result
        for x = 0 to int (
                         System.Math.Ceiling(
                             float (array.Length) / float (chunkSize)
                         )
                     )
                     - 1 do
            let start = x * chunkSize
            let slice = subArrayImpl array start chunkSize
            pushImpl result slice |> ignore

        result

let splitAt (index: int) (array: 'T[]) : 'T[] * 'T[] =
    if index < 0 || index > array.Length then
        invalidArg "index" SR.indexOutOfBounds

    subArrayImpl array 0 index, skipImpl array index

// Note that, though it's not consistent with `compare` operator,
// Array.compareWith doesn't compare first the length, see #2961
let compareWith (comparer: 'T -> 'T -> int) (source1: 'T[]) (source2: 'T[]) =
    if isNull source1 then
        if isNull source2 then
            0
        else
            -1
    elif isNull source2 then
        1
    else
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
            res <- comparer source1.[i] source2.[i]
            i <- i + 1

        if res <> 0 then
            res
        elif len1 > len2 then
            1
        elif len1 < len2 then
            -1
        else
            0

let compareTo (comparer: 'T -> 'T -> int) (source1: 'T[]) (source2: 'T[]) =
    if isNull source1 then
        if isNull source2 then
            0
        else
            -1
    elif isNull source2 then
        1
    else
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
                res <- comparer source1.[i] source2.[i]
                i <- i + 1

            res

let equalsWith (equals: 'T -> 'T -> bool) (array1: 'T[]) (array2: 'T[]) =
    if isNull array1 then
        if isNull array2 then
            true
        else
            false
    elif isNull array2 then
        false
    else
        let mutable i = 0
        let mutable result = true
        let length1 = array1.Length
        let length2 = array2.Length

        if length1 > length2 then
            false
        elif length1 < length2 then
            false
        else
            while i < length1 && result do
                result <- equals array1.[i] array2.[i]
                i <- i + 1

            result

let exactlyOne (array: 'T[]) =
    if array.Length = 1 then
        array.[0]
    elif array.Length = 0 then
        invalidArg
            "array"
            LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
    else
        invalidArg "array" "Input array too long"

let tryExactlyOne (array: 'T[]) =
    if array.Length = 1 then
        Some(array.[0])
    else
        None

let head (array: 'T[]) =
    if array.Length = 0 then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    else
        array.[0]

let tryHead (array: 'T[]) =
    if array.Length = 0 then
        None
    else
        Some array.[0]

let tail (array: 'T[]) =
    if array.Length = 0 then
        invalidArg "array" "Not enough elements"

    skipImpl array 1

let item index (array: _[]) = array.[index]

let tryItem index (array: 'T[]) =
    if index < 0 || index >= array.Length then
        None
    else
        Some array.[index]

let foldBackIndexed<'T, 'State> folder (array: 'T[]) (state: 'State) =
    // if isTypedArrayImpl array then
    //     let mutable acc = state
    //     let size = array.Length
    //     for i = 1 to size do
    //         acc <- folder (i-1) array.[size - i] acc
    //     acc
    // else
    foldBackIndexedImpl (fun acc x i -> folder i x acc) state array

let foldBack<'T, 'State> folder (array: 'T[]) (state: 'State) =
    // if isTypedArrayImpl array then
    //     foldBackIndexed (fun _ x acc -> folder x acc) array state
    // else
    foldBackImpl (fun acc x -> folder x acc) state array

let foldIndexed2 folder state (array1: _[]) (array2: _[]) =
    let mutable acc = state

    if array1.Length <> array2.Length then
        failwith "Arrays have different lengths"

    for i = 0 to array1.Length - 1 do
        acc <- folder i acc array1.[i] array2.[i]

    acc

let fold2<'T1, 'T2, 'State>
    folder
    (state: 'State)
    (array1: 'T1[])
    (array2: 'T2[])
    =
    foldIndexed2 (fun _ acc x y -> folder acc x y) state array1 array2

let foldBackIndexed2<'T1, 'T2, 'State>
    folder
    (array1: 'T1[])
    (array2: 'T2[])
    (state: 'State)
    =
    let mutable acc = state

    if array1.Length <> array2.Length then
        differentLengths ()

    let size = array1.Length

    for i = 1 to size do
        acc <- folder (i - 1) array1.[size - i] array2.[size - i] acc

    acc

let foldBack2<'T1, 'T2, 'State>
    f
    (array1: 'T1[])
    (array2: 'T2[])
    (state: 'State)
    =
    foldBackIndexed2 (fun _ x y acc -> f x y acc) array1 array2 state

let reduce reduction (array: 'T[]) =
    if array.Length = 0 then
        invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    // if isTypedArrayImpl array then
    //     foldIndexed (fun i acc x -> if i = 0 then x else reduction acc x) Unchecked.defaultof<_> array
    // else
    reduceImpl reduction array

let reduceBack reduction (array: 'T[]) =
    if array.Length = 0 then
        invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    // if isTypedArrayImpl array then
    //     foldBackIndexed (fun i x acc -> if i = 0 then x else reduction acc x) array Unchecked.defaultof<_>
    // else
    reduceBackImpl reduction array

let forAll2 predicate array1 array2 =
    fold2 (fun acc x y -> acc && predicate x y) true array1 array2

let rec existsOffset predicate (array: 'T[]) index =
    if index = array.Length then
        false
    else
        predicate array.[index] || existsOffset predicate array (index + 1)

let exists predicate array = existsOffset predicate array 0

let rec existsOffset2 predicate (array1: _[]) (array2: _[]) index =
    if index = array1.Length then
        false
    else
        predicate array1.[index] array2.[index]
        || existsOffset2 predicate array1 array2 (index + 1)

let rec exists2 predicate (array1: _[]) (array2: _[]) =
    if array1.Length <> array2.Length then
        differentLengths ()

    existsOffset2 predicate array1 array2 0

let sum (array: 'T[]) ([<Inject>] adder: IGenericAdder<'T>) : 'T =
    let mutable acc = adder.GetZero()

    for i = 0 to array.Length - 1 do
        acc <- adder.Add(acc, array.[i])

    acc

let sumBy
    (projection: 'T -> 'T2)
    (array: 'T[])
    ([<Inject>] adder: IGenericAdder<'T2>)
    : 'T2
    =
    let mutable acc = adder.GetZero()

    for i = 0 to array.Length - 1 do
        acc <- adder.Add(acc, projection array.[i])

    acc

let maxBy
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : 'a
    =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                y
            else
                x
        )
        xs

let max (xs: 'a[]) ([<Inject>] comparer: IComparer<'a>) : 'a =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                y
            else
                x
        )
        xs

let minBy
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : 'a
    =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                x
            else
                y
        )
        xs

let min (xs: 'a[]) ([<Inject>] comparer: IComparer<'a>) : 'a =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                x
            else
                y
        )
        xs

let average (array: 'T[]) ([<Inject>] averager: IGenericAverager<'T>) : 'T =
    if array.Length = 0 then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    let mutable total = averager.GetZero()

    for i = 0 to array.Length - 1 do
        total <- averager.Add(total, array.[i])

    averager.DivideByInt(total, array.Length)

let averageBy
    (projection: 'T -> 'T2)
    (array: 'T[])
    ([<Inject>] averager: IGenericAverager<'T2>)
    : 'T2
    =
    if array.Length = 0 then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    let mutable total = averager.GetZero()

    for i = 0 to array.Length - 1 do
        total <- averager.Add(total, projection array.[i])

    averager.DivideByInt(total, array.Length)

// let toList (source: 'T[]) = List.ofArray (see Replacements)

let windowed (windowSize: int) (source: 'T[]) : 'T[][] =
    if windowSize <= 0 then
        failwith "windowSize must be positive"

    let res =
        FSharp.Core.Operators.max 0 (source.Length - windowSize + 1)
        |> allocateArray

    for i = windowSize to source.Length do
        res.[i - windowSize] <- source.[i - windowSize .. i - 1]

    res

let splitInto (chunks: int) (array: 'T[]) : 'T[][] =
    if chunks < 1 then
        invalidArg "chunks" "The input must be positive."

    if array.Length = 0 then
        [| [||] |]
    else
        let result: 'T[][] = [||]
        let chunks = FSharp.Core.Operators.min chunks array.Length
        let minChunkSize = array.Length / chunks
        let chunksWithExtraItem = array.Length % chunks

        for i = 0 to chunks - 1 do
            let chunkSize =
                if i < chunksWithExtraItem then
                    minChunkSize + 1
                else
                    minChunkSize

            let start =
                i * minChunkSize
                + (FSharp.Core.Operators.min chunksWithExtraItem i)

            let slice = subArrayImpl array start chunkSize
            pushImpl result slice |> ignore

        result

let transpose
    (arrays: 'T[] seq)
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    : 'T[][]
    =
    let arrays =
        if isDynamicArrayImpl arrays then
            arrays :?> 'T[][] // avoid extra copy
        else
            arrayFrom arrays

    let len = arrays.Length

    match len with
    | 0 -> allocateArray 0
    | _ ->
        let firstArray = arrays.[0]
        let lenInner = firstArray.Length

        if arrays |> forAll (fun a -> a.Length = lenInner) |> not then
            differentLengths ()

        let result: 'T[][] = allocateArray lenInner

        for i in 0 .. lenInner - 1 do
            result.[i] <- allocateArrayFromCons cons len

            for j in 0 .. len - 1 do
                result.[i].[j] <- arrays.[j].[i]

        result

let insertAt
    (index: int)
    (y: 'T)
    (xs: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    : 'T[]
    =
    let len = xs.Length

    if index < 0 || index > len then
        invalidArg "index" SR.indexOutOfBounds

    let target = allocateArrayFromCons cons (len + 1)

    for i = 0 to (index - 1) do
        target.[i] <- xs.[i]

    target.[index] <- y

    for i = index to (len - 1) do
        target.[i + 1] <- xs.[i]

    target

let insertManyAt
    (index: int)
    (ys: seq<'T>)
    (xs: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    : 'T[]
    =
    let len = xs.Length

    if index < 0 || index > len then
        invalidArg "index" SR.indexOutOfBounds

    let ys = arrayFrom ys
    let len2 = ys.Length
    let target = allocateArrayFromCons cons (len + len2)

    for i = 0 to (index - 1) do
        target.[i] <- xs.[i]

    for i = 0 to (len2 - 1) do
        target.[index + i] <- ys.[i]

    for i = index to (len - 1) do
        target.[i + len2] <- xs.[i]

    target

let removeAt (index: int) (xs: 'T[]) : 'T[] =
    if index < 0 || index >= xs.Length then
        invalidArg "index" SR.indexOutOfBounds

    let mutable i = -1

    xs
    |> filter (fun _ ->
        i <- i + 1
        i <> index
    )

let removeManyAt (index: int) (count: int) (xs: 'T[]) : 'T[] =
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

let updateAt
    (index: int)
    (y: 'T)
    (xs: 'T[])
    ([<OptionalArgument; Inject>] cons: Cons<'T>)
    : 'T[]
    =
    let len = xs.Length

    if index < 0 || index >= len then
        invalidArg "index" SR.indexOutOfBounds

    let target = allocateArrayFromCons cons len

    for i = 0 to (len - 1) do
        target.[i] <-
            if i = index then
                y
            else
                xs.[i]

    target
