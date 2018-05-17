module Array

// Warning: "This value is for use by compiled F# code and should not be used directly."
// We skip this for LanguagePrimitives.ErrorStrings.
#nowarn "1204"

open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type IArrayCons<'T> =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract FromSequence: 'T seq -> 'T array

let [<Emit("Array")>] DynamicArrayCons<'T> : IArrayCons<'T> = jsNative

module Helpers =
    /// Creates an empty array with the same type and length as another
    [<Emit("new $0.constructor($0.length)")>]
    let createArrayFromImpl (_:'T[]): 'T[] = jsNative

    let inline newDynamicArrayImpl (len: int): 'T[] =
        DynamicArrayCons.Create(len)

    let inline isTypedArrayImpl arr =
        JS.ArrayBuffer.isView arr

    let inline typedArraySetImpl (target: obj) (source: obj) (offset: int): unit =
        !!target?set(source, offset)

    // Typed arrays not supported, only dynamic ones do
    let inline pushImpl (array: 'T[]) (item: 'T): int =
        !!array?push(item)

    // Typed arrays not supported, only dynamic ones do
    let inline spliceImpl (array: 'T[]) (start: int) (deleteCount: int): 'T[] =
        !!array?splice(start, deleteCount)

    let inline sliceImpl (array: 'T[]) (``begin``: int) (``end``: int): 'T[] =
        !!array?slice(``begin``, ``end``)

    let inline sliceFromImpl (array: 'T[]) (``begin``: int): 'T[] =
        !!array?slice(``begin``)

    let inline indexOfImpl (array: 'T[]) (item: 'T): int =
        !!array?indexOf(item)

    let inline findImpl (predicate: 'T -> bool) (array: 'T[]): 'T option =
        !!array?find(predicate)

    let inline findIndexImpl (predicate: 'T -> bool) (array: 'T[]): int =
        !!array?findIndex(predicate)

    let inline filterImpl (predicate: 'T -> bool) (array: 'T[]): 'T[] =
        !!array?filter(predicate)

    let inline sortInPlaceWithImpl (comparer: 'T -> 'T -> int) (array:'T[]): unit =
        !!array?sort(comparer)

open Helpers

let private indexNotFound() = failwith "An index satisfying the predicate was not found in the collection."

// Pay attention when benchmarking to append and filter functions below
// if implementing via native JS array .concat() and .filter() do not fall behind due to js-native transitions.

// Don't use native JS Array.prototype.concat as it doesn't work with typed arrays
let append (array1: 'T[]) (array2: 'T[]) ([<Inject>] cons: IArrayCons<'T>): 'T[] =
    let len1 = array1.Length
    let len2 = array2.Length
    let newArray = cons.Create(len1 + len2)
    for i = 0 to len1 - 1 do
        newArray.[i] <- array1.[i]
    for i = 0 to len2 - 1 do
        newArray.[i + len1] <- array2.[i]
    newArray

let filter (predicate: 'T -> bool) (array: 'T[]) = filterImpl predicate array

let fill (target: 'T[]) (targetIndex: int) (count: int) (value: 'T): 'T[] =
    for i = targetIndex to (targetIndex + count - 1) do
        target.[i] <- value
    target

let getSubArray (array: 'T[]) (offset: int) (length: int): 'T[] =
    sliceImpl array offset (offset + length)

let last (array : 'T[]) =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    array.[array.Length-1]

let tryLast (array : 'T[]) =
    if array.Length = 0 then None
    else Some array.[array.Length-1]

let mapIndexed (f: int -> 'T -> 'U) (source: 'T[]) ([<Inject>] cons: IArrayCons<'U>): 'U[] =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f i source.[i]
    target

let map (f: 'T -> 'U) (source: 'T[]) ([<Inject>] cons: IArrayCons<'U>): 'U[] =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f source.[i]
    target

let mapIndexed2 (f: int->'T1->'T2->'U) (source1: 'T1[]) (source2: 'T2[]) ([<Inject>] cons: IArrayCons<'U>): 'U[] =
   if source1.Length <> source2.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f i source1.[i] source2.[i]
   result

let map2 (f: 'T1->'T2->'U) (source1: 'T1[]) (source2: 'T2[]) ([<Inject>] cons: IArrayCons<'U>): 'U[] =
   if source1.Length <> source2.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f source1.[i] source2.[i]
   result

let mapIndexed3 (f: int->'T1->'T2->'T3->'U) (source1: 'T1[]) (source2: 'T2[]) (source3: 'T3[]) ([<Inject>] cons: IArrayCons<'U>): 'U[] =
   if source1.Length <> source2.Length || source2.Length <> source3.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f i source1.[i] source2.[i] source3.[i]
   result

let map3 f (source1: 'T[]) (source2: 'U[]) (source3: 'U[]) ([<Inject>] cons: IArrayCons<'W>): 'W[] =
   if source1.Length <> source2.Length || source2.Length <> source3.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f source1.[i] source2.[i] source3.[i]
   result

let mapFold<'T,'State,'Result> (mapping : 'State -> 'T -> 'Result * 'State) state (array: 'T[]) ([<Inject>] cons: IArrayCons<'Result>) =
    match array.Length with
    | 0 -> [| |], state
    | len ->
        let mutable acc = state
        let res = cons.Create len
        for i = 0 to array.Length-1 do
            let h',s' = mapping acc array.[i]
            res.[i] <- h'
            acc <- s'
        res, acc

let mapFoldBack<'T,'State,'Result> (mapping : 'T -> 'State -> 'Result * 'State) (array: 'T[]) state ([<Inject>] cons: IArrayCons<'Result>) =
    match array.Length with
    | 0 -> [| |], state
    | len ->
        let mutable acc = state
        let res = cons.Create len
        for i = array.Length-1 downto 0 do
            let h',s' = mapping array.[i] acc
            res.[i] <- h'
            acc <- s'
        res, acc

let indexed (source: 'T[]) =
    let len = source.Length
    let target = newDynamicArrayImpl len
    for i = 0 to (len - 1) do
        target.[i] <- i, source.[i]
    target

let private concatImpl (cons: IArrayCons<'T>) (arrays: 'T[][]): 'T[] =
    if arrays.Length > 0 then
        let mutable totalLength = 0
        for arr in arrays do
            totalLength <- totalLength + arr.Length

        let result = cons.Create totalLength
        let mutable offset = 0;
        for arr in arrays do
            typedArraySetImpl result arr offset
            offset <- offset + arr.Length
        result
    else
        cons.Create 0

let concat (arrays: 'T[] seq) ([<Inject>] cons: IArrayCons<'T>): 'T[] =
    arrays
    |> Seq.toArray
    |> concatImpl cons

let collect (mapping: 'T -> 'U[]) (array: 'T[]) ([<Inject>] cons: IArrayCons<'U>): 'U[] =
    map mapping array DynamicArrayCons
    |> concatImpl cons

let countBy (projection: 'T->'Key) (array: 'T[]) ([<Inject>] eq: IEqualityComparer<'Key>) =
    let dict = Dictionary<'Key, int>(eq)

    for value in array do
        let key = projection value
        match dict.TryGetValue(key) with
        | true, prev -> dict.[key] <- prev + 1
        | false, _ -> dict.[key] <- 1

    let res = newDynamicArrayImpl dict.Count
    let mutable i = 0
    for group in dict do
        res.[i] <- group.Key, group.Value
        i <- i + 1
    res

let distinctBy projection (array:'T[]) ([<Inject>] cons: IArrayCons<'T>) ([<Inject>] eq: IEqualityComparer<'T>) =
    let temp = cons.Create array.Length
    let mutable i = 0

    let hashSet = HashSet<'T>(eq)
    for v in array do
        if hashSet.Add(projection v) then
            temp.[i] <- v
            i <- i + 1

    sliceImpl temp 0 i

let distinct (array: 'T[]) ([<Inject>] cons: IArrayCons<'T>) ([<Inject>] eq: IEqualityComparer<'T>) =
    distinctBy id array cons eq

let where predicate (array: _[]) = filterImpl predicate array

let except (itemsToExclude: seq<'t>) (array: 't[]) ([<Inject>] eq: IEqualityComparer<'t>): 't[] =
    if array.Length = 0 then
        array
    else
        let cached = HashSet(itemsToExclude, eq)
        array |> filterImpl cached.Add

let groupBy (projection: 'T->'Key) (array: 'T[]) ([<Inject>] cons: IArrayCons<'T>) ([<Inject>] eq: IEqualityComparer<'Key>) =
    let dict = Dictionary<'Key, 'T[]>(eq)

    // Build the groupings
    for i = 0 to (array.Length - 1) do
        let v = array.[i]
        let key = projection v
        match dict.TryGetValue(key) with
        | true, prev -> pushImpl prev v |> ignore
        | false, _ ->
            // Use dynamic array so we can build it up via .push()
            // Consider benchmarking if another collection type performs better here
            let prev = [|v|]
            dict.[key] <- prev

    // Return the array-of-arrays.
    let result = newDynamicArrayImpl dict.Count
    let mutable i = 0
    for group in dict do
        result.[i] <- group.Key, cons.FromSequence group.Value
        i <- i + 1

    result

let inline private emptyImpl (cons: IArrayCons<'T>) = cons.Create(0)

let empty cons = emptyImpl cons

let singleton value ([<Inject>] cons: IArrayCons<'T>) =
    let ar = cons.Create 1
    ar.[0] <- value
    ar

let initialize count initializer ([<Inject>] cons: IArrayCons<'T>) =
    if count < 0 then invalidArg "count" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    let result = cons.Create count
    for i = 0 to count - 1 do
        result.[i] <- initializer i
    result

let pairwise (array: 'T[]) =
    if array.Length < 2
    then [||]
    else
        let count = array.Length - 1
        let result = newDynamicArrayImpl count
        for i = 0 to count - 1 do
            result.[i] <-  array.[i], array.[i+1]
        result

let replicate count initial ([<Inject>] cons: IArrayCons<'T>) =
    // Shorthand version: = initialize count (fun _ -> initial)
    if count < 0 then invalidArg "count" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    let result : 'T array = cons.Create count
    for i = 0 to result.Length-1 do
        result.[i] <- initial
    result

let reverse (array: _[]) ([<Inject>] cons: IArrayCons<'T>) =
    let res = cons.Create array.Length
    let mutable j = array.Length-1
    for i = 0 to array.Length-1 do
        res.[j] <- array.[i]
        j <- j - 1
    res

let scan<'T, 'State> folder (state: 'State) (array: 'T []) ([<Inject>] cons: IArrayCons<'State>) =
    let res = cons.Create (array.Length + 1)
    res.[0] <- state
    for i = 0 to array.Length - 1 do
        res.[i + 1] <- folder res.[i] array.[i]
    res

let scanBack<'T, 'State> folder (array: 'T []) (state: 'State) ([<Inject>] cons: IArrayCons<'State>) =
    let res = cons.Create(array.Length + 1)
    res.[array.Length] <- state
    for i = array.Length - 1 downto 0 do
        res.[i] <- folder array.[i] res.[i + 1]
    res

let skip count (array:'T[]) ([<Inject>] cons: IArrayCons<'T>) =
    if count > array.Length then invalidArg "count" "count is greater than array length"
    if count = array.Length then
        emptyImpl cons
    else
        let count = if count > 0 then 0 else 0
        sliceFromImpl array count

let skipWhile predicate (array: 'T[]) ([<Inject>] cons: IArrayCons<'T>) =
    let mutable count = 0
    while count < array.Length && predicate array.[count] do
        count <- count + 1

    if count = array.Length then
        emptyImpl cons
    else
        sliceFromImpl array count

let take count (array:'T[]) ([<Inject>] cons: IArrayCons<'T>) =
    if count < 0 then invalidArg "count" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    if count > array.Length then invalidArg "count" "count is greater than array length"
    if count = 0 then
        emptyImpl cons
    else
        sliceImpl array 0 count

let takeWhile predicate (array: 'T[]) ([<Inject>] cons: IArrayCons<'T>) =
    let mutable count = 0
    while count < array.Length && predicate array.[count] do
        count <- count + 1

    if count = 0 then
        emptyImpl cons
    else
        sliceImpl array 0 count

let addRangeInPlace (range: JS.Iterable<'T>) (array: 'T[]) =
    // if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
    let iter = range.``[Symbol.iterator]``()
    let mutable cur = iter.next()
    while not (cur.``done``) do
       pushImpl array !!cur.value |> ignore
       cur <- iter.next()

let removeInPlace (item: 'T) (array: 'T[]) =
    // if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
    let i = indexOfImpl array item
    if i > -1 then
        spliceImpl array i 1 |> ignore
        true
    else
        false

let copyTo (source: JS.ArrayLike<'T>) sourceIndex (target: JS.ArrayLike<'T>) targetIndex count =
    let diff = targetIndex - sourceIndex
    for i = sourceIndex to sourceIndex + count - 1 do
        target.[i + diff] <- source.[i]

let partition (f: 'T -> bool) (source: 'T[]) ([<Inject>] cons: IArrayCons<'T>) =
    let len = source.Length
    let res1 = cons.Create len
    let res2 = cons.Create len
    let mutable iTrue = 0
    let mutable iFalse = 0
    for i = 0 to len - 1 do
        if f source.[i] then
            res1.[iTrue] <- source.[i]
            iTrue <- iTrue + 1
        else
            res2.[iFalse] <- source.[i]
            iFalse <- iFalse + 1
    res1, res2

let find (predicate: 'T -> bool) (array: 'T[]): 'T =
    match findImpl predicate array with
    | Some res -> res
    | None -> indexNotFound()

let tryFind (predicate: 'T -> bool) (array: 'T[]): 'T option =
    findImpl predicate array

let findIndex (predicate: 'T -> bool) (array: 'T[]): int =
    match findIndexImpl predicate array with
    | index when index > -1 -> index
    | _ -> indexNotFound()

let tryFindIndex (predicate: 'T -> bool) (array: 'T[]): int option =
    match findIndexImpl predicate array with
    | index when index > -1 -> Some index
    | _ -> None

let pick chooser (array: _[]) =
    let rec loop i =
        if i >= array.Length then
            indexNotFound()
        else
            match chooser array.[i] with
            | None -> loop(i+1)
            | Some res -> res
    loop 0

let tryPick chooser (array: _[]) =
    let rec loop i =
        if i >= array.Length then None else
        match chooser array.[i] with
        | None -> loop(i+1)
        | res -> res
    loop 0

let findBack predicate (array: _[]) =
    let rec loop i =
        if i < 0 then indexNotFound()
        elif predicate array.[i] then array.[i]
        else loop (i - 1)
    loop (array.Length - 1)

let tryFindBack predicate (array: _[]) =
    let rec loop i =
        if i < 0 then None
        elif predicate array.[i] then Some array.[i]
        else loop (i - 1)
    loop (array.Length - 1)

let findIndexBack predicate (array : _[]) =
    let rec loop i =
        if i < 0 then indexNotFound()
        elif predicate array.[i] then i
        else loop (i - 1)
    loop (array.Length - 1)

let tryFindIndexBack predicate (array : _[]) =
    let rec loop i =
        if i < 0 then None
        elif predicate array.[i] then Some i
        else loop (i - 1)
    loop (array.Length - 1)

let choose f (source: 'T[]) ([<Inject>] cons: IArrayCons<'T>) =
   let res = cons.Create 0
   let mutable j = 0
   for i = 0 to source.Length - 1 do
      match f source.[i] with
      | Some y ->
         res.[j] <- y
         j <- j + 1
      | None -> ()
   res

let foldIndexed folder state (array: 'T[]) =
    let mutable acc = state
    for i = 0 to array.Length - 1 do
        acc <- folder i acc array.[i]
    acc

let fold<'T,'State> folder (state: 'State) (array: 'T[]) =
    let mutable acc = state
    for i = 0 to array.Length - 1 do
        acc <- folder acc array.[i]
    acc

let iterate action (array: 'T[]) =
    for i = 0 to array.Length - 1 do
        action array.[i]

let iterate2 action (array1: 'T[]) (array2: 'T[]) =
    if array1.Length <> array2.Length then failwith "Arrays had different lengths"
    for i = 0 to array1.Length - 1 do
        action array1.[i] array2.[i]

let iterateIndexed action (array: 'T[]) =
    for i = 0 to array.Length - 1 do
        action i array.[i]

let iterateIndexed2 action (array1: 'T[]) (array2: 'T[]) =
    if array1.Length <> array2.Length then failwith "Arrays had different lengths"
    for i = 0 to array1.Length - 1 do
        action i array1.[i] array2.[i]

let isEmpty (array: 'T[]) =
    array.Length = 0

let forAll predicate (array: 'T[]) =
    // Shorthand: fold (fun acc x -> predicate x && acc) true array
    let mutable i = 0
    let mutable result = true
    while i < array.Length && result do
        result <- predicate array.[i]
        i <- i + 1
    result

let permute f (array: 'T[]) =
    let size = array.Length
    let res = createArrayFromImpl array
    let checkFlags = newDynamicArrayImpl size
    iterateIndexed (fun i x ->
        let j = f i
        if j < 0 || j >= size then
            invalidOp "Not a valid permutation"
        res.[j] <- x
        checkFlags.[j] <- 1) array
    let isValid = forAll ((=) 1) checkFlags
    if not isValid then
        invalidOp "Not a valid permutation"
    res

let setSlice (target: 'T[]) (lower: int option) (upper: int option) (source: 'T[]) =
    let lower = defaultArg lower 0
    let upper = defaultArg upper 0
    let length = (if upper > 0 then upper else target.Length - 1) - lower
    if isTypedArrayImpl target && source.Length <= length then
        typedArraySetImpl target source lower
    else
        for i = 0 to length do
            target.[i + lower] <- source.[i]

let sortInPlaceBy (projection:'a->'b) (xs : 'a[]) ([<Inject>] comparer: IComparer<'b>): unit =
    sortInPlaceWithImpl (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortInPlace (xs : 'T[]) ([<Inject>] comparer: IComparer<'T>) =
    sortInPlaceWithImpl (fun x y -> comparer.Compare(x, y)) xs

let inline internal sortInPlaceWith (comparer: 'T -> 'T -> int) (xs : 'T[]) =
    sortInPlaceWithImpl comparer xs
    xs

let private copyArray (array : 'T[]) =
    let result = createArrayFromImpl array
    for i = 0 to array.Length - 1 do
        result.[i] <- array.[i]
    result

let sort (xs : 'T[]) ([<Inject>] comparer: IComparer<'T>): 'T[] =
    sortInPlaceWith (fun x y -> comparer.Compare(x, y)) (copyArray xs)

let sortBy (projection:'a->'b) (xs : 'a[]) ([<Inject>] comparer: IComparer<'b>): 'a[] =
    sortInPlaceWith (fun x y -> comparer.Compare(projection x, projection y)) (copyArray xs)

let sortDescending (xs : 'T[]) ([<Inject>] comparer: IComparer<'T>): 'T[] =
    sortInPlaceWith (fun x y -> comparer.Compare(x, y) * -1) (copyArray xs)

let sortByDescending (projection:'a->'b) (xs : 'a[]) ([<Inject>] comparer: IComparer<'b>): 'a[] =
    sortInPlaceWith (fun x y -> comparer.Compare(projection x, projection y) * -1) (copyArray xs)

let sortWith (comparer: 'T -> 'T -> int) (xs : 'T[]): 'T[] =
    sortInPlaceWith comparer (copyArray xs)

let unfold<'T,'State> (generator:'State -> ('T*'State) option) (state:'State): 'State[] =
    let res = [||]
    let rec loop state =
        match generator state with
        | None -> ()
        | Some (x,s') ->
            pushImpl res x |> ignore
            loop s'
    loop state
    res

// TODO: We should pass ArrayCons<'T> here (and unzip3) but 'a and 'b may differ
let unzip (array: _[]) =
    let len = array.Length
    let res1 = newDynamicArrayImpl len
    let res2 = newDynamicArrayImpl len
    iterateIndexed (fun i (item1, item2) ->
        res1.[i] <- item1
        res2.[i] <- item2
    ) array
    res1, res2

let unzip3 (array: _[]) =
    let len = array.Length
    let res1 = newDynamicArrayImpl len
    let res2 = newDynamicArrayImpl len
    let res3 = newDynamicArrayImpl len
    iterateIndexed (fun i (item1, item2, item3) ->
        res1.[i] <- item1
        res2.[i] <- item2
        res3.[i] <- item3
    ) array
    res1, res2, res3

let zip (array1: 'T[]) (array2: 'U[]) =
    // Shorthand version: map2 (fun x y -> x, y) array1 array2
    if array1.Length <> array2.Length then failwith "Arrays had different lengths"
    let result = newDynamicArrayImpl array1.Length
    for i = 0 to array1.Length - 1 do
       result.[i] <- array1.[i], array2.[i]
    result

let zip3 (array1: 'T[]) (array2: 'U[]) (array3: 'U[]) =
    // Shorthand version: map3 (fun x y z -> x, y, z) array1 array2 array3
    if array1.Length <> array2.Length || array2.Length <> array3.Length then failwith "Arrays had different lengths"
    let result = newDynamicArrayImpl array1.Length
    for i = 0 to array1.Length - 1 do
       result.[i] <- array1.[i], array2.[i], array3.[i]
    result

let chunkBySize (chunkSize: int) (array: 'T[]): 'T[][] =
    if chunkSize < 1 then invalidArg "size" "The input must be positive."

    if array.Length = 0 then [| [||] |]
    else
        let result: 'T[][] = [||]
        // add each chunk to the result
        for x = 0 to int(System.Math.Ceiling(float(array.Length) / float(chunkSize))) - 1 do
            let start = x * chunkSize
            let end' = start + chunkSize
            let slice = sliceImpl array start end'
            pushImpl result slice |> ignore

        result

let splitAt (index: int) (array: 'T[]): 'T[] * 'T[] =
    if index < 0 then invalidArg "index" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    if index > array.Length then invalidArg "index" "The input sequence has an insufficient number of elements."

    sliceImpl array 0 index, sliceFromImpl array index

let compareWith (comparer: 'T -> 'T -> int) (array1: 'T[]) (array2: 'T[]) =
    if isNull array1 then
        if isNull array2 then 0 else -1
    elif isNull array2 then
        1
    else
        let mutable i = 0
        let mutable result = 0
        let length1 = array1.Length
        let length2 = array2.Length
        if length1 > length2 then 1
        elif length1 < length2 then -1
        else
            while i < length1 && result = 0 do
                result <- comparer array1.[i] array2.[i]
                i <- i + 1
            result

let equalsWith (comparer: 'T -> 'T -> int) (array1: 'T[]) (array2: 'T[]) =
    compareWith compare array1 array2 = 0

let exactlyOne (array: 'T[]) =
    if array.Length = 1 then array.[0]
    elif array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputSequenceEmptyString
    else invalidArg "array" "Input array too long"

let head (array : 'T[]) =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    else array.[0]

let tryHead (array : 'T[]) =
    if array.Length = 0 then None
    else Some array.[0]

let tail (array : 'T[]) =
    if array.Length = 0 then invalidArg "array" "Not enough elements"
    sliceFromImpl array 1

let item index (array: _[]) =
    array.[index]

let tryItem index (array:'T[]) =
    if index < 0 || index >= array.Length then None
    else Some array.[index]

// let toList array =
//     List.ofArray array

let foldBackIndexed<'T,'State> folder (array: 'T[]) (state:'State) =
   let mutable acc = state
   let size = array.Length
   for i = 1 to size do
      acc <- folder (i-1) array.[size - i] acc
   acc

let foldBack<'T,'State> folder (array: 'T[]) (state:'State) =
   foldBackIndexed (fun _ x acc -> folder x acc) array state

let foldIndexed2 folder state (array1: _[]) (array2: _[]) =
   let mutable acc = state
   if array1.Length <> array2.Length then failwith "Arrays have different lengths"
   for i = 0 to array1.Length - 1 do
      acc <- folder i acc array1.[i] array2.[i]
   acc

let fold2<'T1, 'T2, 'State> folder (state: 'State) (array1: 'T1[]) (array2: 'T2[]) =
   foldIndexed2 (fun _ acc x y -> folder acc x y) state array1 array2

let foldBackIndexed2<'T1, 'T2, 'State> folder (array1: 'T1[]) (array2: 'T2[]) (state:'State) =
   let mutable acc = state
   if array1.Length <> array2.Length then failwith "Arrays had different lengths"
   let size = array1.Length
   for i = 1 to size do
      acc <- folder (i-1) array1.[size - i] array2.[size - i] acc
   acc

let foldBack2<'T1, 'T2, 'State> f (array1: 'T1[]) (array2: 'T2[]) (state: 'State) =
   foldBackIndexed2 (fun _ x y acc -> f x y acc) array1 array2 state

let reduce reduction (array: 'T[]) =
   if array.Length = 0 then invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
   else foldIndexed (fun i acc x -> if i = 0 then x else reduction acc x) Unchecked.defaultof<_> array

let reduceBack reduction (array: 'T[]) =
   if array.Length = 0 then invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
   else foldBackIndexed (fun i x acc -> if i = 0 then x else reduction acc x) array Unchecked.defaultof<_>

let forAll2 predicate array1 array2 =
   fold2 (fun acc x y -> acc && predicate x y) true array1 array2

let rec existsOffset predicate (array: 'T[]) index =
   if index = array.Length then false
   else predicate array.[index] || existsOffset predicate array (index+1)

let exists predicate array =
   existsOffset predicate array 0

let rec existsOffset2 predicate (array1: _[]) (array2: _[]) index =
   if index = array1.Length then false
   else predicate array1.[index] array2.[index] || existsOffset2 predicate array1 array2 (index+1)

let rec exists2 predicate (array1: _[]) (array2: _[]) =
   if array1.Length <> array2.Length then failwith "Arrays had different lengths"
   existsOffset2 predicate array1 array2 0

// TODO!!!: Pass add function for non-number types
let sum (array: float[]) : float =
    let mutable acc = 0.
    for i = 0 to array.Length - 1 do
        acc <- acc + array.[i]
    acc

let sumBy (projection: 'T -> float) (array: 'T []) : float =
    let mutable acc = 0.
    for i = 0 to array.Length - 1 do
        acc <- acc + projection array.[i]
    acc

let maxBy (projection:'a->'b) (xs:'a[]) ([<Inject>] comparer: IComparer<'b>): 'a =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

let max (xs:'a[]) ([<Inject>] comparer: IComparer<'a>): 'a =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) xs

let minBy (projection:'a->'b) (xs:'a[]) ([<Inject>] comparer: IComparer<'b>): 'a =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then x else y) xs

let min (xs:'a[]) ([<Inject>] comparer: IComparer<'a>): 'a =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then x else y) xs

let average (array: float []) : float =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    let total = sum array
    total / float array.Length

let averageBy (projection: 'T -> float) (array: 'T []) : float =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    let total = sumBy projection array
    total / float array.Length

let ofSeq (source: 'T list) ([<Inject>] cons: IArrayCons<'T>) =
    cons.FromSequence(source)

let ofList (source: 'T list) ([<Inject>] cons: IArrayCons<'T>) =
    let len = List.length source
    let target = cons.Create(len)
    let mutable i = 0
    for x in source do
        target.[i] <- x
        i <- i + 1
    target

let toList (source: 'T[]) =
    let len = source.Length
    let mutable target = []
    for i = (len - 1) downto 0 do
        target <- source.[i]::target
    target
