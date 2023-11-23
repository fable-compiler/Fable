module ArrayModule

// Disables warn:1204 raised by use of LanguagePrimitives.ErrorStrings.*
#nowarn "1204"
#nowarn "1182"

open System.Runtime.InteropServices
open System.Collections.Generic
open Fable.Core

[<RequireQualifiedAccess; Erase>]
type Native =
    /// Converts resize array to fixed without creating a new copy
    [<Emit("$0")>]
    static member asFixed(array: ResizeArray<'T>) : 'T[] = jsNative

    /// Converts fixed to resize array without creating a new copy
    [<Emit("$0")>]
    static member asResize(array: 'T[]) : ResizeArray<'T> = jsNative

    [<Emit("List.generate($0, $1, growable: false)")>]
    static member generate (len: int) (f: int -> 'T) : 'T[] = jsNative

    [<Emit("List.generate($0, $1)")>]
    static member generateResize (len: int) (f: int -> 'T) : ResizeArray<'T> =
        jsNative

    [<Emit("$1.where($0).toList(growable: false)")>]
    static member where (f: 'T -> bool) (xs: 'T[]) : 'T[] = jsNative

    [<Emit("$1.every($0)")>]
    static member every (f: 'T -> bool) (xs: 'T[]) : bool = jsNative

    [<Emit("$1.reduce($0)")>]
    static member reduce (combine: 'T -> 'T -> 'T) (xs: 'T[]) : 'T = jsNative

    [<Emit("List.filled($0, $1, growable: false)")>]
    static member filled (len: int) (x: 'T) : 'T[] = jsNative

    [<Emit("$0.fillRange($1, $2, $3)")>]
    static member fillRange
        (xs: 'T[])
        (start: int)
        (end_: int)
        (fill: 'T)
        : unit
        =
        jsNative

    [<Emit("$0.sublist($1...)")>]
    static member sublist(xs: 'T[], start: int, [<Optional>] end_: int) : 'T[] =
        jsNative

    [<Emit("List.copyRange($0...)")>]
    static member copyRange
        (
            target: 'T[],
            at: int,
            source: 'T[],
            start: int,
            [<Optional>] end_: int
        )
        : unit
        =
        jsNative

    [<Emit("$0.toList(growable: false)")>]
    static member toList(xs: 'T seq) : 'T[] = nativeOnly

    [<Emit("$0.reversed.toList(growable: false)")>]
    static member reversed(xs: 'T[]) : 'T[] = nativeOnly

    [<Emit("$0.add($1)")>]
    static member add (xs: 'T[]) (x: 'T) : unit = nativeOnly

    [<Emit("$0.addAll($1)")>]
    static member addAll (xs: 'T[]) (range: 'T seq) : unit = nativeOnly

    [<Emit("$0.insert($1...)")>]
    static member insert (xs: 'T[]) (index: int) (x: 'T) : unit = nativeOnly

    [<Emit("$0.insertAll($1...)")>]
    static member insertAll (xs: 'T[]) (index: int) (range: 'T seq) : unit =
        nativeOnly

    [<Emit("$0.remove($1)")>]
    static member remove (xs: 'T[]) (value: obj) : bool = nativeOnly

    [<Emit("$0.removeAt($1)")>]
    static member removeAt (xs: 'T[]) (index: int) : 'T = nativeOnly

    [<Emit("$0.removeLast()")>]
    static member removeLast(xs: 'T[]) : 'T = nativeOnly

    [<Emit("$0.removeRange($1...)")>]
    static member removeRange (xs: 'T[]) (start: int) (end_: int) : unit =
        nativeOnly

    [<Emit("$0.removeWhere($1...)")>]
    static member removeWhere (xs: 'T[]) (predicate: 'T -> bool) : unit =
        nativeOnly

    [<Emit("$0.sort($1...)")>]
    static member sort(xs: 'T[], [<Optional>] compare: 'T -> 'T -> int) : unit =
        nativeOnly

    [<Emit("$0.contains($1)")>]
    static member contains (xs: 'T[]) (value: obj) : bool = nativeOnly

    [<Emit("$0.indexOf($1...)")>]
    static member indexOf(xs: 'T[], item: 'T, [<Optional>] start: int) : int =
        jsNative

    [<Emit("$0.indexWhere($1...)")>]
    static member indexWhere
        (
            xs: 'T[],
            predicate: 'T -> bool,
            [<Optional>] start: int
        )
        : int
        =
        jsNative

    [<Emit("$0.lastIndexOf($1...)")>]
    static member lastIndexOf
        (
            xs: 'T[],
            item: 'T,
            [<Optional>] start: int
        )
        : 'T[]
        =
        jsNative

    [<Emit("$0.lastIndexWhere($1...)")>]
    static member lastIndexWhere
        (
            xs: 'T[],
            predicate: 'T -> bool,
            [<Optional>] start: int
        )
        : 'T[]
        =
        jsNative

    // Dart's native function includes a named argument `orElse` for an alternative predicate
    [<Emit("$0.firstWhere($1...)")>]
    static member firstWhere(xs: 'T[], predicate: 'T -> bool) : 'T = jsNative

let private indexNotFound () =
    failwith
        "An index satisfying the predicate was not found in the collection."

let private differentLengths () = failwith "Arrays had different lengths"

// https://stackoverflow.com/a/9113136
let reverseInPlace (xs: 'T[]) : unit =
    //    let len = xs.Length
    //    let half = len / 2
    //    for i = 0 to half - 1 do
    //        let j = len - i - 1
    //        let tmp = xs[i]
    //        xs[i] <- xs[j]
    //        xs[j] <- tmp
    let mutable left = 0
    let mutable right = 0
    let length = xs.Length

    while left < length / 2 do
        right <- length - 1 - left
        let temporary = xs[left]
        xs[left] <- xs[right]
        xs[right] <- temporary
        left <- left + 1

let append (array1: 'T[]) (array2: 'T[]) : 'T[] =
    let len1 = array1.Length
    let len2 = array2.Length

    Native.generate
        (len1 + len2)
        (fun i ->
            if i < len1 then
                array1[i]
            else
                array2[i - len1]
        )

let filter (predicate: 'T -> bool) (array: 'T[]) = Native.where predicate array

// intentionally returns target instead of unit
let fill (target: 'T[]) (targetIndex: int) (count: int) (value: 'T) : 'T[] =
    Native.fillRange target targetIndex (targetIndex + count) value
    target

let getSubArray (array: 'T[]) (start: int) (count: int) : 'T[] =
    Native.sublist (array, start, start + count)

let last (array: 'T[]) =
    if Array.isEmpty array then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    array[array.Length - 1]

let tryLast (array: 'T[]) =
    if Array.isEmpty array then
        None
    else
        Some array[array.Length - 1]

let mapIndexed (f: int -> 'T -> 'U) (source: 'T[]) : 'U[] =
    Native.generate source.Length (fun i -> f i source[i])

let map (f: 'T -> 'U) (source: 'T[]) : 'U[] =
    Native.generate source.Length (fun i -> f source[i])

let mapIndexed2
    (f: int -> 'T1 -> 'T2 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    : 'U[]
    =
    if source1.Length <> source2.Length then
        failwith "Arrays had different lengths"

    Native.generate source1.Length (fun i -> f i source1[i] source2[i])

let map2 (f: 'T1 -> 'T2 -> 'U) (source1: 'T1[]) (source2: 'T2[]) : 'U[] =
    if source1.Length <> source2.Length then
        failwith "Arrays had different lengths"

    Native.generate source1.Length (fun i -> f source1[i] source2[i])

let mapIndexed3
    (f: int -> 'T1 -> 'T2 -> 'T3 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    (source3: 'T3[])
    : 'U[]
    =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then
        failwith "Arrays had different lengths"

    Native.generate
        source1.Length
        (fun i -> f i source1[i] source2[i] source3[i])

let map3
    (f: 'T1 -> 'T2 -> 'T3 -> 'U)
    (source1: 'T1[])
    (source2: 'T2[])
    (source3: 'T3[])
    : 'U[]
    =
    if source1.Length <> source2.Length || source2.Length <> source3.Length then
        failwith "Arrays had different lengths"

    Native.generate source1.Length (fun i -> f source1[i] source2[i] source3[i])

let mapFold<'T, 'State, 'Result>
    (mapping: 'State -> 'T -> 'Result * 'State)
    (state: 'State)
    (array: 'T[])
    : 'Result[] * 'State
    =
    if Array.isEmpty array then
        [||], state
    else
        let mutable acc = state

        let res =
            Native.generate
                array.Length
                (fun i ->
                    let h, s = mapping acc array[i]
                    acc <- s
                    h
                )

        res, acc

let mapFoldBack<'T, 'State, 'Result>
    (mapping: 'T -> 'State -> 'Result * 'State)
    (array: 'T[])
    (state: 'State)
    : 'Result[] * 'State
    =
    if Array.isEmpty array then
        [||], state
    else
        let len = array.Length
        let mutable acc = state

        let res =
            Native.generate
                len
                (fun i ->
                    let i = len - i - 1
                    let h, s = mapping array[i] acc
                    acc <- s
                    h
                )

        reverseInPlace res
        res, acc

let indexed (source: 'T[]) =
    Native.generate source.Length (fun i -> i, source[i])

let truncate (count: int) (array: 'T[]) : 'T[] =
    let count = max 0 count |> min array.Length
    Native.sublist (array, 0, count)

let concatArrays (arrays: 'T[][]) : 'T[] =
    match arrays.Length with
    | 0 -> Array.empty
    | 1 -> arrays[0]
    | _ ->
        let mutable totalLength = 0

        for arr in arrays do
            totalLength <- totalLength + arr.Length

        if totalLength = 0 then
            Array.empty
        else
            let mutable curIdx = 0
            let mutable accLen = 0
            let mutable curLen = arrays[0].Length

            Native.generate
                totalLength
                (fun i ->
                    while i >= accLen + curLen do
                        curIdx <- curIdx + 1
                        accLen <- accLen + curLen
                        curLen <- arrays[curIdx].Length

                    arrays[curIdx][i - accLen]
                )

let concat (arrays: 'T[] seq) : 'T[] = Native.toList arrays |> concatArrays

let collect (mapping: 'T -> 'U[]) (array: 'T[]) : 'U[] =
    map mapping array |> concatArrays

let initialize (count: int) (initializer: int -> 'a) : 'a[] =
    if count < 0 then
        invalidArg
            "count"
            LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString

    Native.generate count initializer

let pairwise (array: 'T[]) : ('T * 'T)[] =
    if array.Length < 2 then
        [||]
    else
        let count = array.Length - 1
        Native.generate count (fun i -> array[i], array[i + 1])

let contains<'T when 'T: equality> (value: 'T) (array: 'T[]) : bool =
    let rec loop i =
        if i >= array.Length then
            false
        else if value = array[i] then
            true
        else
            loop (i + 1)

    loop 0

let replicate (count: int) (initial: 'T) : 'T array =
    // Shorthand version: = initialize count (fun _ -> initial)
    if count < 0 then
        invalidArg
            "count"
            LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString

    Native.generate count (fun _ -> initial)

let copy (array: 'T[]) : 'T[] = Native.sublist (array, 0)

let reverse (array: 'T[]) : 'T[] = Native.reversed array

let scan<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (array: 'T[])
    : 'State[]
    =
    let mutable state = state

    Native.generate
        (array.Length + 1)
        (fun i ->
            if i = 0 then
                state
            else
                state <- folder state array[i - 1]
                state
        )

let scanBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (array: 'T[])
    (state: 'State)
    : 'State[]
    =
    let len = array.Length
    let mutable state = state

    let res =
        Native.generate
            (len + 1)
            (fun i ->
                if i = 0 then
                    state
                else
                    state <- folder array[len - i] state
                    state
            )

    reverseInPlace res
    res

let skip count (array: 'T[]) =
    if count > array.Length then
        invalidArg "count" "count is greater than array length"

    if count = array.Length then
        Array.empty
    else
        Native.sublist (array, max count 0)

let skipWhile predicate (array: 'T[]) =
    let mutable count = 0

    while count < array.Length && predicate array[count] do
        count <- count + 1

    if count = array.Length then
        Array.empty
    else
        Native.sublist (array, count)

let take count (array: 'T[]) =
    if count < 0 then
        invalidArg
            "count"
            LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString

    if count > array.Length then
        invalidArg "count" "count is greater than array length"

    if count = 0 then
        Array.empty
    else
        Native.sublist (array, 0, count)

let takeWhile predicate (array: 'T[]) =
    let mutable count = 0

    while count < array.Length && predicate array[count] do
        count <- count + 1

    if count = 0 then
        Array.empty
    else
        Native.sublist (array, 0, count)

let addInPlace (x: 'T) (array: 'T[]) : unit = Native.add array x

let addRangeInPlace (range: seq<'T>) (array: 'T[]) : unit =
    Native.addAll array range

let insertRangeInPlace (index: int) (range: seq<'T>) (array: 'T[]) : unit =
    Native.insertAll array index range

//let removeInPlace (item: 'T) (array: 'T[]): bool =
//    Native.remove array item

let removeAllInPlace (predicate: 'T -> bool) (array: 'T[]) : int =
    let len = array.Length
    Native.removeWhere array predicate
    len - array.Length

// TODO: Check array lengths
let copyTo
    (source: 'T[])
    (sourceIndex: int)
    (target: 'T[])
    (targetIndex: int)
    (count: int)
    : unit
    =
    Native.copyRange (
        target,
        targetIndex,
        source,
        sourceIndex,
        sourceIndex + count
    )

let indexOf
    (array: 'T[])
    (item: 'T)
    (start: int option)
    (count: int option)
    : int
    =
    let start = defaultArg start 0
    let i = Native.indexOf (array, item, start)

    match count with
    | Some count when i >= start + count -> -1
    | _ -> i

let partition (f: 'T -> bool) (source: 'T[]) =
    let res1 = ResizeArray()
    let res2 = ResizeArray()

    for x in source do
        if f x then
            res1.Add(x)
        else
            res2.Add(x)

    Native.asFixed res1, Native.asFixed res2

let find (predicate: 'T -> bool) (array: 'T[]) : 'T =
    Native.firstWhere (array, predicate)

let tryFind (predicate: 'T -> bool) (array: 'T[]) : 'T option =
    try
        find predicate array |> Some
    with _ ->
        None

let findIndex (predicate: 'T -> bool) (array: 'T[]) : int =
    match Native.indexWhere (array, predicate) with
    | -1 -> indexNotFound ()
    | index -> index

let tryFindIndex (predicate: 'T -> bool) (array: 'T[]) : int option =
    match Native.indexWhere (array, predicate) with
    | -1 -> None
    | index -> Some index

let pick (chooser: 'a -> 'b option) (array: _[]) : 'b =
    let rec loop i =
        if i >= array.Length then
            indexNotFound ()
        else
            match chooser array[i] with
            | None -> loop (i + 1)
            | Some res -> res

    loop 0

let tryPick (chooser: 'a -> 'b option) (array: _[]) : 'b option =
    let rec loop i =
        if i >= array.Length then
            None
        else
            match chooser array[i] with
            | None -> loop (i + 1)
            | res -> res

    loop 0

let findBack (predicate: 'a -> bool) (array: _[]) : 'a =
    let rec loop i =
        if i < 0 then
            indexNotFound ()
        elif predicate array[i] then
            array[i]
        else
            loop (i - 1)

    loop (array.Length - 1)

let tryFindBack (predicate: 'a -> bool) (array: _[]) : 'a option =
    let rec loop i =
        if i < 0 then
            None
        elif predicate array[i] then
            Some array[i]
        else
            loop (i - 1)

    loop (array.Length - 1)

let findLastIndex (predicate: 'a -> bool) (array: _[]) : int =
    let rec loop i =
        if i < 0 then
            -1
        elif predicate array[i] then
            i
        else
            loop (i - 1)

    loop (array.Length - 1)

let findIndexBack (predicate: 'a -> bool) (array: _[]) : int =
    let rec loop i =
        if i < 0 then
            indexNotFound ()
        elif predicate array[i] then
            i
        else
            loop (i - 1)

    loop (array.Length - 1)

let tryFindIndexBack (predicate: 'a -> bool) (array: _[]) : int option =
    let rec loop i =
        if i < 0 then
            None
        elif predicate array[i] then
            Some i
        else
            loop (i - 1)

    loop (array.Length - 1)

let choose (chooser: 'T -> 'U option) (array: 'T[]) : 'U[] =
    let res = ResizeArray<'U>()

    for i = 0 to array.Length - 1 do
        match chooser array[i] with
        | None -> ()
        | Some y -> res.Add(y)

    Native.asFixed res

let fold<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (array: 'T[])
    : 'State
    =
    let mutable state = state

    for x in array do
        state <- folder state x

    state

let iterate (action: 'T -> unit) (array: 'T[]) : unit =
    for i = 0 to array.Length - 1 do
        action array[i]

let iterateIndexed (action: int -> 'T -> unit) (array: 'T[]) : unit =
    for i = 0 to array.Length - 1 do
        action i array[i]

let iterate2
    (action: 'T1 -> 'T2 -> unit)
    (array1: 'T1[])
    (array2: 'T2[])
    : unit
    =
    if array1.Length <> array2.Length then
        differentLengths ()

    for i = 0 to array1.Length - 1 do
        action array1[i] array2[i]

let iterateIndexed2
    (action: int -> 'T1 -> 'T2 -> unit)
    (array1: 'T1[])
    (array2: 'T2[])
    : unit
    =
    if array1.Length <> array2.Length then
        differentLengths ()

    for i = 0 to array1.Length - 1 do
        action i array1[i] array2[i]

let forAll (predicate: 'T -> bool) (array: 'T[]) : bool =
    Native.every predicate array

let permute (f: int -> int) (array: 'T[]) : 'T[] =
    let size = array.Length
    let res = Native.sublist (array, 0)
    let checkFlags = Native.filled size 0

    iterateIndexed
        (fun i x ->
            let j = f i

            if j < 0 || j >= size then
                invalidOp "Not a valid permutation"

            res[j] <- x
            checkFlags[j] <- 1
        )
        array

    let isValid = checkFlags |> forAll ((=) 1)

    if not isValid then
        invalidOp "Not a valid permutation"

    res

let setSlice
    (target: 'T[])
    (lower: int option)
    (upper: int option)
    (source: 'T[])
    : unit
    =
    let lower = defaultArg lower 0
    let upper = defaultArg upper -1

    let length =
        (if upper >= 0 then
             upper
         else
             target.Length - 1)
        - lower

    for i = 0 to length do
        target[i + lower] <- source[i]

let sortInPlaceBy
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : unit
    =
    Native.sort (xs, (fun x y -> comparer.Compare(projection x, projection y)))

let sortInPlace (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) =
    Native.sort (xs, (fun x y -> comparer.Compare(x, y)))

let sortInPlaceWith (comparer: 'T -> 'T -> int) (xs: 'T[]) : 'T[] =
    Native.sort (xs, comparer)
    xs

let sort (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) : 'T[] =
    let xs = Native.sublist (xs, 0)
    Native.sort (xs, (fun x y -> comparer.Compare(x, y)))
    xs

let sortBy
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : 'a[]
    =
    Native.sublist (xs, 0)
    |> sortInPlaceWith (fun x y -> comparer.Compare(projection x, projection y))

let sortDescending (xs: 'T[]) ([<Inject>] comparer: IComparer<'T>) : 'T[] =
    Native.sublist (xs, 0)
    |> sortInPlaceWith (fun x y -> comparer.Compare(x, y) * -1)

let sortByDescending
    (projection: 'a -> 'b)
    (xs: 'a[])
    ([<Inject>] comparer: IComparer<'b>)
    : 'a[]
    =
    Native.sublist (xs, 0)
    |> sortInPlaceWith (fun x y ->
        comparer.Compare(projection x, projection y) * -1
    )

let sortWith (comparer: 'T -> 'T -> int) (xs: 'T[]) : 'T[] =
    Native.sublist (xs, 0) |> sortInPlaceWith comparer

let allPairs (xs: 'T1[]) (ys: 'T2[]) : ('T1 * 'T2)[] =
    let len1 = xs.Length
    let len2 = ys.Length

    Native.generate
        (len1 * len2)
        (fun i ->
            let x = xs[i / len2]
            let y = ys[i % len2]
            (x, y)
        )

let unfold<'T, 'State>
    (generator: 'State -> ('T * 'State) option)
    (state: 'State)
    : 'T[]
    =
    let res = ResizeArray()

    let rec loop state =
        match generator state with
        | None -> ()
        | Some(x, s) ->
            res.Add(x)
            loop s

    loop state
    Native.asFixed res

let unzip (array: ('a * 'b)[]) : 'a[] * 'b[] =
    let res1 = ResizeArray()
    let res2 = ResizeArray()

    for (item1, item2) in array do
        res1.Add(item1)
        res2.Add(item2)

    Native.asFixed res1, Native.asFixed res2

let unzip3 (array: ('a * 'b * 'c)[]) : 'a[] * 'b[] * 'c[] =
    let res1 = ResizeArray()
    let res2 = ResizeArray()
    let res3 = ResizeArray()

    for (item1, item2, item3) in array do
        res1.Add(item1)
        res2.Add(item2)
        res3.Add(item3)

    Native.asFixed res1, Native.asFixed res2, Native.asFixed res3

let zip (array1: 'T[]) (array2: 'U[]) : ('T * 'U)[] =
    // Shorthand version: map2 (fun x y -> x, y) array1 array2
    if array1.Length <> array2.Length then
        differentLengths ()

    Native.generate array1.Length (fun i -> array1[i], array2[i])

let zip3 (array1: 'T1[]) (array2: 'T2[]) (array3: 'T3[]) : ('T1 * 'T2 * 'T3)[] =
    // Shorthand version: map3 (fun x y z -> x, y, z) array1 array2 array3
    if array1.Length <> array2.Length || array2.Length <> array3.Length then
        differentLengths ()

    Native.generate array1.Length (fun i -> array1[i], array2[i], array3[i])

let chunkBySize (chunkSize: int) (array: 'T[]) : 'T[][] =
    if chunkSize < 1 then
        invalidArg "size" "The input must be positive."

    if Array.isEmpty array then
        [| [||] |]
    else
        let len = array.Length
        let result = ResizeArray()
        // add each chunk to the result
        for x = 0 to int (System.Math.Ceiling(float len / float chunkSize)) - 1 do
            let start = x * chunkSize
            let end_ = min len (start + chunkSize)
            let slice = Native.sublist (array, start, end_)
            result.Add(slice)

        Native.asFixed result

let splitAt (index: int) (array: 'T[]) : 'T[] * 'T[] =
    if index < 0 || index > array.Length then
        invalidArg "index" SR.indexOutOfBounds

    Native.sublist (array, 0, index), Native.sublist (array, index)

let compareWith
    (comparer: 'T -> 'T -> int)
    (array1: 'T[])
    (array2: 'T[])
    : int
    =
    // Null checks not necessary because Dart provides null safety
    //    if isNull array1 then
    //        if isNull array2 then 0 else -1
    //    elif isNull array2 then
    //        1
    //    else
    let mutable i = 0
    let mutable result = 0
    let length1 = array1.Length
    let length2 = array2.Length

    if length1 > length2 then
        1
    elif length1 < length2 then
        -1
    else
        while i < length1 && result = 0 do
            result <- comparer array1[i] array2[i]
            i <- i + 1

        result

let equalsWith (equals: 'T -> 'T -> bool) (array1: 'T[]) (array2: 'T[]) : bool =
    // Null checks not necessary because Dart provides null safety
    //    if isNull array1 then
    //        if isNull array2 then true else false
    //    elif isNull array2 then
    //        false
    //    else
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
            result <- equals array1[i] array2[i]
            i <- i + 1

        result

let exactlyOne (array: 'T[]) : 'T =
    if array.Length = 1 then
        array[0]
    elif Array.isEmpty array then
        invalidArg
            "array"
            LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
    else
        invalidArg "array" "Input array too long"

let tryExactlyOne (array: 'T[]) : 'T option =
    if array.Length = 1 then
        Some(array[0])
    else
        None

let head (array: 'T[]) : 'T =
    if Array.isEmpty array then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    else
        array[0]

let tryHead (array: 'T[]) : 'T option =
    if Array.isEmpty array then
        None
    else
        Some array[0]

let tail (array: 'T[]) : 'T[] =
    if Array.isEmpty array then
        invalidArg "array" "Not enough elements"

    Native.sublist (array, 1)

let item (index: int) (array: 'a[]) : 'a = array[index]

let tryItem (index: int) (array: 'T[]) : 'T option =
    if index < 0 || index >= array.Length then
        None
    else
        Some array[index]

let foldBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (array: 'T[])
    (state: 'State)
    : 'State
    =
    let mutable acc = state

    for i = array.Length - 1 downto 0 do
        acc <- folder array[i] acc

    acc

let fold2<'T1, 'T2, 'State>
    (folder: 'State -> 'T1 -> 'T2 -> 'State)
    (state: 'State)
    (array1: 'T1[])
    (array2: 'T2[])
    : 'State
    =
    let mutable acc = state

    if array1.Length <> array2.Length then
        failwith "Arrays have different lengths"

    for i = 0 to array1.Length - 1 do
        acc <- folder acc array1[i] array2[i]

    acc

let foldBack2<'T1, 'T2, 'State>
    (folder: 'T1 -> 'T2 -> 'State -> 'State)
    (array1: 'T1[])
    (array2: 'T2[])
    (state: 'State)
    : 'State
    =
    let mutable acc = state

    if array1.Length <> array2.Length then
        differentLengths ()

    for i = array1.Length - 1 downto 0 do
        acc <- folder array1[i] array2[i] acc

    acc

let reduce (reduction: 'T -> 'T -> 'T) (array: 'T[]) : 'T =
    // Dart's native reduce will fail if collection is empty
    //    if Array.isEmpty array then invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    Native.reduce reduction array

let reduceBack (reduction: 'T -> 'T -> 'T) (array: 'T[]) : 'T =
    if Array.isEmpty array then
        invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    let mutable i = array.Length - 1
    let mutable state = array[i]

    while i > 0 do
        i <- i - 1
        state <- reduction array[i] state

    state

let forAll2 (predicate: 'a -> 'b -> bool) (array1: 'a[]) (array2: 'b[]) : bool =
    fold2 (fun acc x y -> acc && predicate x y) true array1 array2

let rec existsOffset predicate (array: 'T[]) index =
    if index = array.Length then
        false
    else
        predicate array[index] || existsOffset predicate array (index + 1)

let exists predicate array = existsOffset predicate array 0

let rec existsOffset2 predicate (array1: _[]) (array2: _[]) index =
    if index = array1.Length then
        false
    else
        predicate array1[index] array2[index]
        || existsOffset2 predicate array1 array2 (index + 1)

let rec exists2 predicate (array1: _[]) (array2: _[]) =
    if array1.Length <> array2.Length then
        differentLengths ()

    existsOffset2 predicate array1 array2 0

let sum (array: 'T[]) ([<Inject>] adder: IGenericAdder<'T>) : 'T =
    let mutable acc = adder.GetZero()

    for i = 0 to array.Length - 1 do
        acc <- adder.Add(acc, array[i])

    acc

let sumBy
    (projection: 'T -> 'T2)
    (array: 'T[])
    ([<Inject>] adder: IGenericAdder<'T2>)
    : 'T2
    =
    let mutable acc = adder.GetZero()

    for i = 0 to array.Length - 1 do
        acc <- adder.Add(acc, projection array[i])

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
    if Array.isEmpty array then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    let mutable total = averager.GetZero()

    for i = 0 to array.Length - 1 do
        total <- averager.Add(total, array[i])

    averager.DivideByInt(total, array.Length)

let averageBy
    (projection: 'T -> 'T2)
    (array: 'T[])
    ([<Inject>] averager: IGenericAverager<'T2>)
    : 'T2
    =
    if Array.isEmpty array then
        invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString

    let mutable total = averager.GetZero()

    for i = 0 to array.Length - 1 do
        total <- averager.Add(total, projection array[i])

    averager.DivideByInt(total, array.Length)

// let toList (source: 'T[]) = List.ofArray (see Replacements)

let windowed (windowSize: int) (source: 'T[]) : 'T[][] =
    if windowSize <= 0 then
        failwith "windowSize must be positive"

    let len = Operators.max 0 (source.Length - windowSize + 1)
    Native.generate len (fun i -> source[i .. i + windowSize - 1])

let splitInto (chunks: int) (array: 'T[]) : 'T[][] =
    if chunks < 1 then
        invalidArg "chunks" "The input must be positive."

    if Array.isEmpty array then
        [| [||] |]
    else
        let len = array.Length
        let result = ResizeArray()
        let chunks = Operators.min chunks len
        let minChunkSize = len / chunks
        let chunksWithExtraItem = len % chunks

        for i = 0 to chunks - 1 do
            let chunkSize =
                if i < chunksWithExtraItem then
                    minChunkSize + 1
                else
                    minChunkSize

            let start = i * minChunkSize + (Operators.min chunksWithExtraItem i)
            let end_ = Operators.min len (start + chunkSize)
            let slice = Native.sublist (array, start, end_)
            result.Add(slice)

        Native.asFixed result

let transpose (arrays: 'T[] seq) : 'T[][] =
    let arrays =
        match arrays with
        | :? ('T[][]) as arrays -> arrays // avoid extra copy
        | _ -> Array.ofSeq arrays

    let len = arrays.Length

    if len = 0 then
        Array.empty
    else
        let firstArray = arrays[0]
        let lenInner = firstArray.Length

        if arrays |> forAll (fun a -> a.Length = lenInner) |> not then
            differentLengths ()

        Native.generate
            lenInner
            (fun i -> Native.generate len (fun j -> arrays[j][i]))

let insertAt (index: int) (y: 'T) (xs: 'T[]) : 'T[] =
    let len = xs.Length

    if index < 0 || index > len then
        invalidArg "index" SR.indexOutOfBounds

    Native.generate
        (len + 1)
        (fun i ->
            if i < index then
                xs[i]
            elif i = index then
                y
            else
                xs[i - 1]
        )

let insertManyAt (index: int) (ys: seq<'T>) (xs: 'T[]) : 'T[] =
    let len = xs.Length

    if index < 0 || index > len then
        invalidArg "index" SR.indexOutOfBounds

    let ys =
        match ys with
        | :? ('T[]) as ys -> ys // avoid extra copy
        | _ -> Array.ofSeq ys

    let len2 = ys.Length
    let index2 = index + len2

    Native.generate
        (len + len2)
        (fun i ->
            if i < index then
                xs[i]
            elif i < index2 then
                ys[i - index]
            else
                xs[i - len2]
        )

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

let updateAt (index: int) (y: 'T) (xs: 'T[]) : 'T[] =
    let len = xs.Length

    if index < 0 || index >= len then
        invalidArg "index" SR.indexOutOfBounds

    Native.generate
        len
        (fun i ->
            if i = index then
                y
            else
                xs[i]
        )
