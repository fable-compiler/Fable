module Array

// Warning: "This value is for use by compiled F# code and should not be used directly."
// We skip this for LanguagePrimitives.ErrorStrings.
#nowarn "1204"

open Fable.Core
open Fable.Import

type ArrayCons =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract From: 'T seq -> 'T array

[<AutoOpen>]
module private JS =
    let inline private jsCast x = x |> box :?> _

    let inline private jsArray (array: 'T[]): JS.Array<'T> = jsCast array

    // Let's consider "Emit" as a last resort here and consider re-using Fable.Import.JS definitions

    [<Emit("$0 + $1")>]
    let inline nativeAddition (_x: 'T) (_y: 'U): 'V = jsNative

    [<Emit("$0 / $1")>]
    let inline nativeDivision (_x: 'T) (_y: 'U): 'V = jsNative

    [<Emit("0")>]
    let inline nativeZero<'T> (): 'T = jsNative

    let inline isTypedArrayImpl arr = JS.ArrayBuffer.isView arr :?> bool

    [<Emit("[].concat.apply([], $0)")>]
    let dynamicArrayConcatImpl (_arrays: 'T[][]): 'T[] = jsNative

    [<Emit("$0.set($1, $2)")>]
    let typedArraySetImpl (_target: obj) (_source: obj) (_offset: float) = jsNative

    let inline lengthImpl (array: 'T[]): int =
        (jsArray array).length |> jsCast

    // Typed arrays not supported, only dynamic ones do
    let inline pushImpl (array: 'T[]) (item: 'T): int =
        (jsArray array).push(item) |> jsCast

    // Typed arrays not supported, only dynamic ones do
    let inline spliceImpl (array: 'T[]) (start: int) (deleteCount: int): 'T[] =
        (jsArray array).splice(jsCast start, jsCast deleteCount) |> jsCast

    let inline sliceImpl (array: 'T[]) (``begin``: int) (``end``: int): 'T[] =
        (jsArray array).slice(jsCast ``begin``, jsCast ``end``) |> jsCast

    let inline sliceFromImpl (array: 'T[]) (``begin``: int): 'T[] =
        (jsArray array).slice(jsCast ``begin``) |> jsCast

    let inline concatImpl (array1: 'T[]) (array2: 'T[]): 'T[] =
        (jsArray array1).concat(jsCast array2 |> U2.Case2) |> jsCast

    let inline indexOfImpl (array: 'T[]) (item: 'T): int =
        (jsArray array).indexOf(item) |> jsCast

    let inline findImpl (predicate: 'T -> bool) (array: 'T[]): 'T option =
        (jsArray array).find(jsCast predicate) |> jsCast

    let inline findIndexImpl (predicate: 'T -> bool) (array: 'T[]): int =
        (jsArray array).findIndex(jsCast predicate) |> jsCast

    let inline filterImpl (predicate: 'T -> bool) (array: 'T[]): 'T[] =
        (jsArray array).filter(jsCast predicate) |> jsCast

    [<Emit("$1.sort(function($a,$b) { return $0($a)($b); })")>]
    let inline sortInPlaceWithImpl (comparer: 'T -> 'T -> int) (_array:'T[]): unit = jsNative


let private indexNotFound() = failwith "An index satisfying the predicate was not found in the collection."

// Pay attention when benchmarking to append and filter functions below
// if implementing via native JS array .concat() and .filter() do not fall behind due to js-native transitions.

let append (array1: 'T[]) (array2: 'T[]): 'T[] = concatImpl array1 array2

let filter (predicate: 'T -> bool) (array: 'T[]) = filterImpl predicate array

let length array = lengthImpl array

let getSubArray (array: 'T[]) (offset: int) (length: int): 'T[] =
    sliceImpl array offset (offset + length)

let last (array : 'T[]) =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    array.[array.Length-1]

let tryLast (array : 'T[]) =
    if array.Length = 0 then None
    else Some array.[array.Length-1]

let mapIndexed (f: int -> 'T -> 'U) (source: 'T[]) (cons: ArrayCons) =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f i source.[i]
    target

let map (f: 'T -> 'U) (source: 'T[]) (cons: ArrayCons) =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f source.[i]
    target

let mapIndexed2 f (source1: 'T[]) (source2: 'U[]) (cons: ArrayCons) =
   if source1.Length <> source2.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f i source1.[i] source2.[i]
   result

let map2 f (source1: 'T[]) (source2: 'U[]) (cons: ArrayCons) =
   if source1.Length <> source2.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f source1.[i] source2.[i]
   result

let mapIndexed3 f (source1: 'T[]) (source2: 'U[]) (source3: 'U[]) (cons: ArrayCons) =
   if source1.Length <> source2.Length || source2.Length <> source3.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f i source1.[i] source2.[i] source3.[i]
   result

let map3 f (source1: 'T[]) (source2: 'U[]) (source3: 'U[]) (cons: ArrayCons) =
   if source1.Length <> source2.Length || source2.Length <> source3.Length then failwith "Arrays had different lengths"
   let result = cons.Create(source1.Length)
   for i = 0 to source1.Length - 1 do
      result.[i] <- f source1.[i] source2.[i] source3.[i]
   result

let mapFold<'T,'State,'Result> (mapping : 'State -> 'T -> 'Result * 'State) state (array: 'T[]) (cons: ArrayCons) =
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

let mapFoldBack<'T,'State,'Result> (mapping : 'T -> 'State -> 'Result * 'State) state (array: 'T[]) (cons: ArrayCons) =
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
    
let indexed (source: 'T[]) (cons: ArrayCons) =
    source |> mapIndexed (fun i x -> i, x) <| cons;

let private typedArrayConcatImpl (cons: ArrayCons) (arrays: 'T[][]): 'T[] =
    let mutable totalLength = 0
    for arr in arrays do
        totalLength <- totalLength + arr.Length

    let result = cons.Create totalLength
    let mutable offset = 0;
    for arr in arrays do
        typedArraySetImpl result arr <| float(offset)
        offset <- offset + arr.Length
    result

let private concatImpl (cons: ArrayCons) (arrays: 'T[][]): 'T[] =
    if arrays.Length > 0 then
        if isTypedArrayImpl arrays.[0] then
            typedArrayConcatImpl cons arrays
        else
            dynamicArrayConcatImpl arrays
    else
        cons.Create 0

let concat (arrays: 'T[] seq) (cons: ArrayCons): 'T[] =
    arrays
    |> Seq.toArray
    |> concatImpl cons

let collect (mapping: 'T -> 'U[]) (array: 'T[]) (cons: ArrayCons): 'U[] =
    map mapping array cons
    |> concatImpl cons

let countBy (projection: 'T->'Key) (array: 'T[]) (cons: ArrayCons) =
    let dict = System.Collections.Generic.Dictionary<'Key, int>()

    for value in array do
        let key = projection value
        let mutable prev = Unchecked.defaultof<_>
        if dict.TryGetValue(key, &prev) then dict.[key] <- prev + 1 else dict.[key] <- 1

    let res = cons.Create dict.Count
    let mutable i = 0
    for group in dict do
        res.[i] <- group.Key, group.Value
        i <- i + 1
    res

let distinctBy projection (array:'T[]) (cons: ArrayCons) =
    let temp = cons.Create array.Length
    let mutable i = 0

    let hashSet = System.Collections.Generic.HashSet<'T>()
    for v in array do
        if hashSet.Add(projection v) then
            temp.[i] <- v
            i <- i + 1

    sliceImpl temp 0 i

let distinct (array: 'T[]) (cons: ArrayCons) = distinctBy id array cons

let where predicate (array: _[]) = filterImpl predicate array

let except (itemsToExclude: seq<_>) (array:_[]) =
    if array.Length = 0 then
        array
    else
        let cached = System.Collections.Generic.HashSet(itemsToExclude)
        array |> filterImpl cached.Add

let groupBy (projection: 'T->'Key) (array: 'T[]) (cons: ArrayCons) =
    let dict = System.Collections.Generic.Dictionary<'Key, 'T[]>()

    // Build the groupings
    for i = 0 to (array.Length - 1) do
        let v = array.[i]
        let key = projection v
        let mutable prev = Unchecked.defaultof<_>
        if dict.TryGetValue(key, &prev) then
            pushImpl prev v |> ignore
        else
            // Use dynamic array so we can build it up via .push()
            // Consider benchmarking if another collection type performs better here
            let prev = [|v|]
            dict.[key] <- prev

    // Return the array-of-arrays.
    let result = cons.Create dict.Count
    let mutable i = 0
    for group in dict do
        result.[i] <- group.Key, cons.From group.Value
        i <- i + 1

    result

let ofArray (arr: 'T[]) = arr

let inline private emptyImpl (cons: ArrayCons) = cons.Create(0)

let empty = emptyImpl

let singleton value (cons: ArrayCons) = cons.From [|value|]

let initialize count initializer (cons: ArrayCons) =
    if count < 0 then invalidArg "count" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    let result = cons.Create count
    for i = 0 to count - 1 do
        result.[i] <- initializer i
    result

let pairwise (array: 'T[]) (cons: ArrayCons) =
    if array.Length < 2 then emptyImpl cons else
    initialize (array.Length-1) (fun i -> array.[i],array.[i+1]) cons

let replicate count initial (cons: ArrayCons) =
    // Shorthand version: = initialize count (fun _ -> initial)
    if count < 0 then invalidArg "count" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    let result : 'T array = cons.Create count
    for i = 0 to result.Length-1 do
        result.[i] <- initial
    result

let reverse (array: _[]) (cons: ArrayCons) =
    let res = cons.Create array.Length
    let mutable j = array.Length-1
    for i = 0 to array.Length-1 do
        res.[j] <- array.[i]
        j <- j - 1
    res

let scan<'T, 'State> folder (state: 'State) (array: 'T []) (cons: ArrayCons) =
    let res = cons.Create (array.Length + 1)
    res.[0] <- state
    for i = 0 to array.Length - 1 do
        res.[i + 1] <- folder res.[i] array.[i]
    res

let scanBack<'T, 'State> folder (state: 'State) (array: 'T []) (cons: ArrayCons) =
    let res = cons.Create (array.Length + 1)
    let size = array.Length
    res.[array.Length] <- state
    for i = 1 to array.Length do
        res.[size - i] <- folder array.[size - i] res.[size - i + 1]
    res

let skip count (array:'T[]) (cons: ArrayCons) =
    if count > array.Length then invalidArg "count" "count is greater than array length"
    if count = array.Length then
        emptyImpl cons
    else
        let count = max count 0
        sliceFromImpl array count

let skipWhile predicate (array: 'T[]) (cons: ArrayCons) =
    let mutable count = 0
    while count < array.Length && predicate array.[count] do
        count <- count + 1

    if count = array.Length then
        emptyImpl cons
    else
        sliceFromImpl array count

let take count (array:'T[]) (cons: ArrayCons) =
    if count < 0 then invalidArg "count" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    if count > array.Length then invalidArg "count" "count is greater than array length"
    if count = 0 then
        emptyImpl cons
    else
        sliceImpl array 0 count

let takeWhile predicate (array: 'T[]) (cons: ArrayCons) =
    let mutable count = 0
    while count < array.Length && predicate array.[count] do
        count <- count + 1

    if count = 0 then
        emptyImpl cons
    else
        sliceImpl array 0 count

let addRangeInPlace (range: JS.Iterable<'T>) (array: 'T[]) =
    if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
    let iter = range.``[Symbol.iterator]``()
    let mutable cur = iter.next()
    while not (cur.``done``) do
       pushImpl array (cur.value :> obj :?> 'T) |> ignore
       cur <- iter.next()

let removeInPlace (item: 'T) (array: 'T[]) =
    if isTypedArrayImpl array then invalidArg "array" "Typed arrays not supported"
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

let partition (f: 'T -> bool) (source: JS.ArrayLike<'T>) (cons: ArrayCons) =
    let len = source.length |> int
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

let choose f (source: 'T[]) (cons: ArrayCons) =
   let res = cons.Create 0
   let mutable j = 0
   for i = 0 to source.Length - 1 do
      match f source.[i] with
      | Some y -> 
         res.[j] <- y
         j <- j + 1
      | None -> ()
   res

let foldIndexed folder state array =
    let mutable acc = state
    for i = 0 to lengthImpl array - 1 do
        acc <- folder i acc array.[i]
    acc

let fold<'T,'State> folder (state: 'State) (array: 'T[]) = 
    let mutable acc = state
    for i = 0 to lengthImpl array - 1 do
        acc <- folder acc array.[i]
    acc

let iterate action array =
    for i = 0 to lengthImpl array - 1 do
        action array.[i]

let iterate2 action (array1: 'T[]) (array2: 'T[]) =
    if array1.Length <> array2.Length then failwith "Arrays had different lengths"
    for i = 0 to array1.Length - 1 do
        action array1.[i] array2.[i]

let iterateIndexed action array =
    for i = 0 to lengthImpl array - 1 do
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

let permute f array (cons: ArrayCons) = 
    let size = lengthImpl array
    let res  = cons.Create size
    let checkFlags = cons.Create size
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

let setSlice (target: JS.ArrayLike<'T>) (lower: int) (upper: int) (source: JS.ArrayLike<'T>) =
    let length = (if upper > 0 then upper else int(target.length) - 1) - lower
    if isTypedArrayImpl target && source.length <= float(length) then
        typedArraySetImpl target source (float(lower))
    else
        for i = 0 to length - 1 do
            target.[i + lower] <- source.[i]

let sortInPlaceBy f array =
    sortInPlaceWithImpl (fun (x:'T) (y:'T) -> 
        let x = f x
        let y = f y
        compare x y) array

let sortInPlace array =
    sortInPlaceWithImpl compare array

let sortWith (comparer: 'T -> 'T -> int) (array : 'T[]) (cons: ArrayCons) =
    let result = cons.From array
    sortInPlaceWithImpl comparer result
    result

let unfold<'T,'State> (generator:'State -> ('T*'State) option) (state:'State) (cons: ArrayCons) =
    let res = [||]
    let rec loop state =
        match generator state with
        | None -> ()
        | Some (x,s') ->
            pushImpl res x |> ignore
            loop s'
    loop state
    cons.From res

let unzip array (cons: ArrayCons) =
    let len = lengthImpl array
    let res1 = cons.Create len
    let res2 = cons.Create len
    iterateIndexed (fun i (item1, item2) ->
        res1.[i] <- item1
        res2.[i] <- item2
    ) array
    res1, res2

let unzip3 array (cons: ArrayCons) =
    let len = lengthImpl array
    let res1 = cons.Create len
    let res2 = cons.Create len
    let res3 = cons.Create len
    iterateIndexed (fun i (item1, item2, item3) ->
        res1.[i] <- item1
        res2.[i] <- item2
        res3.[i] <- item3
    ) array
    res1, res2, res3

let zip (array1: 'T[]) (array2: 'U[]) (cons: ArrayCons) =
    // Shorthand version: map2 (fun x y -> x, y) array1 array2
    if array1.Length <> array2.Length then failwith "Arrays had different lengths"
    let result = cons.Create(array1.Length)
    for i = 0 to array1.Length - 1 do
       result.[i] <- array1.[i], array2.[i]
    result

let zip3 (array1: 'T[]) (array2: 'U[]) (array3: 'U[]) (cons: ArrayCons) =
    // Shorthand version: map3 (fun x y z -> x, y, z) array1 array2 array3
    if array1.Length <> array2.Length || array2.Length <> array3.Length then failwith "Arrays had different lengths"
    let result = cons.Create(array1.Length)
    for i = 0 to array1.Length - 1 do
       result.[i] <- array1.[i], array2.[i], array3.[i]
    result

let chunkBySize (chunkSize: int) (array: 'T[]): 'T[][] =
    if chunkSize < 1 then invalidArg "size" "The input must be positive."

    if array.Length = 0 then [| [||] |]
    else
        let result: 'T[][] = [||]
        // add each chunk to the result
        for x = 0 to System.Math.Floor(float(array.Length) / float(chunkSize)) |> int do
            let start = x * chunkSize;
            let end' = start + chunkSize;
            let slice = sliceImpl array start end'
            pushImpl result slice |> ignore

        result

let fill (array: 'T[]) offset count value =
    for i = offset to offset + count - 1 do
        array.[i] <- value

let splitAt (index: int) (array: 'T[]): 'T[] * 'T[] =
    if index < 0 then invalidArg "index" LanguagePrimitives.ErrorStrings.InputMustBeNonNegativeString
    if index > array.Length then invalidArg "index" "The input sequence has an insufficient number of elements."

    sliceImpl array 0 index, sliceFromImpl array index

let compareWith (comparer: 'T -> 'T -> int) (array1: 'T[]) (array2: 'T[]) =
    let length1 = array1.Length
    let length2 = array2.Length

    let mutable i = 0
    let mutable result = 0

    if length1 < length2 then
        while i < array1.Length && result = 0 do
            result <- comparer array1.[i] array2.[i]
            i <- i + 1
    else
        while i < array2.Length && result = 0 do
            result <- comparer array1.[i] array2.[i]
            i <- i + 1

    if result <> 0 then result
    elif length1 = length2 then 0
    elif length1 < length2 then -1
    else 1

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

let toList array = 
    List.ofArray array

let foldBackIndexed<'T,'State> folder (array: 'T[]) (state:'State) =
   let mutable acc = state
   let size = lengthImpl array
   for i = 1 to size do
      acc <- folder (i-1) array.[size - i] acc
   acc

let foldBack<'T,'State> folder (array: 'T[]) (state:'State) =
   foldBackIndexed (fun _ x acc -> folder x acc) array state

let foldIndexed2 folder state array1 array2 =
   let mutable acc = state
   if lengthImpl array1 <> lengthImpl array2 then failwith "Arrays have different lengths"
   for i = 0 to lengthImpl array1 - 1 do
      acc <- folder i acc array1.[i] array2.[i]
   acc

let fold2<'T1, 'T2, 'State> folder (state: 'State) (array1: 'T1[]) (array2: 'T2[]) =
   foldIndexed2 (fun _ acc x y -> folder acc x y) state array1 array2

let foldBackIndexed2<'T1, 'T2, 'State> folder (array1: 'T1[]) (array2: 'T2[]) (state:'State) =
   let mutable acc = state
   if lengthImpl array1 <> lengthImpl array2 then failwith "Arrays had different lengths"
   let size = lengthImpl array1
   for i = 1 to size do
      acc <- folder (i-1) array1.[size - i] array2.[size - i] acc
   acc

let foldBack2<'T1, 'T2, 'State> f (array1: 'T1[]) (array2: 'T2[]) (state: 'State) = 
   foldBackIndexed2 (fun _ x y acc -> f x y acc) array1 array2 state 

let reduce reduction array =
   if lengthImpl array = 0 then invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
   else foldIndexed (fun i acc x -> if i = 0 then x else reduction acc x) Unchecked.defaultof<_> array

let reduceBack reduction array =
   if lengthImpl array = 0 then invalidOp LanguagePrimitives.ErrorStrings.InputArrayEmptyString
   else foldBackIndexed (fun i x acc -> if i = 0 then x else reduction acc x) array Unchecked.defaultof<_>

let forAll2 predicate array1 array2 =
   fold2 (fun acc x y -> acc && predicate x y) true array1 array2

let rec existsOffset predicate array index =
   if index = lengthImpl array then false
   else predicate array.[index] || existsOffset predicate array (index+1)

let exists predicate array = 
   existsOffset predicate array 0

let rec existsOffset2 predicate array1 (array2:_ []) index =
   if index = lengthImpl array1 then false
   else predicate array1.[index] array2.[index] || existsOffset2 predicate array1 array2 (index+1)

let rec exists2 predicate array1 array2 =
   if lengthImpl array1 <> lengthImpl array2 then failwith "Arrays had different lengths"
   existsOffset2 predicate array1 array2 0

let sum (array: 'T[]) : 'T =
    let mutable acc = nativeZero<'T>()
    for i = 0 to array.Length - 1 do
        acc <- nativeAddition acc array.[i]
    acc

let sumBy (projection: 'T -> 'U) (array: 'T []) : 'U =
    let mutable acc = nativeZero<'U>()
    for i = 0 to array.Length - 1 do
        acc <- array.[i] |> projection |> nativeAddition acc 
    acc

let maxBy projection array =
    reduce (fun x y -> if projection y > projection x then y else x) array

let max array =
    reduce max array

let minBy projection array =
    reduce (fun x y -> if projection y > projection x then x else y) array

let min array =
    reduce min array

let average (array: 'T []) : 'T =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    let total = sum array
    nativeDivision total array.Length

let averageBy (projection: 'T -> 'U) (array: 'T []) : 'U =
    if array.Length = 0 then invalidArg "array" LanguagePrimitives.ErrorStrings.InputArrayEmptyString
    let total = sumBy projection array
    nativeDivision total array.Length

