module Array

open Fable.Core
open Fable.Import.JS

type ArrayCons =
    [<Emit("new $0($1)")>]
    abstract Create: capacity: int -> 'T array
    [<Emit("$0.from($1)")>]
    abstract From: 'T seq -> 'T array

[<Emit("$0.push($1)")>]
let private push (xs:'T[]) (item: 'T): int = jsNative

[<Emit("$0.slice($1, $2)")>]
let private slice (xs:'T[]) (``begin``: int) (``end``: int): 'T[] = jsNative

[<Emit("$0.slice($1)")>]
let private sliceFrom (xs:'T[]) (``begin``: int): 'T[] = jsNative

[<Emit("$0.splice($1, $2)")>]
let private splice (xs:'T[]) (start: int) (deleteCount: int): 'T[] = jsNative

[<Emit("$0.indexOf($1)")>]
let private indexOf (xs:'T[]) (item: 'T): int = jsNative

[<Emit("$1.sort(function($a,$b) { return $0($a)($b); });")>]
let sortInPlaceWith (f:'a -> 'a -> int) (xs:'a[]): unit = jsNative

[<Emit("$0.length")>]
let length (xs: 'T[]) = xs.Length

[<Emit("$0.slice($1, $1 + $2)")>]
let getSubArray (xs:'a[]) (offset:int) (length:int): 'a[] = jsNative

[<Emit("$0.set($1, $2)")>]
let setInArrayLike (target: obj) (source: obj) (offset: float) = jsNative

let mapIndexed (f: int -> 'T -> 'U) (source: 'T[]) (cons: ArrayCons) =
    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f i source.[i]
    target

let map (f: 'T -> 'U) (source: 'T[]) (cons: ArrayCons) =
    // TODO: Should we involve currying and re-use mapIndexed here?
    // Don't quite know yet how is it optimized so can't decide on performance vs code reuse.
    // mapIndexed (fun _ x -> f x) source

    let len = source.Length
    let target = cons.Create(len)
    for i = 0 to (len - 1) do
        target.[i] <- f source.[i]
    target

let indexed (source: 'T[]) (cons: ArrayCons) =
    // TODO: How do we get concrete instance of ArrayCons for dynamic JS Arrays creation?
    // Passing as a parameter for now.
    source |> mapIndexed (fun i x -> i, x) <| cons;

let addRangeInPlace (range: Iterable<'T>) (xs: 'T[]) =
    let iter = range.``[Symbol.iterator]``()
    let mutable cur = iter.next()
    while not (cur.``done``) do
       push xs (cur.value :> obj :?> 'T) |> ignore
       cur <- iter.next()

let copyTo (source: ArrayLike<'T>) sourceIndex (target: ArrayLike<'T>) targetIndex count =
    let diff = targetIndex - sourceIndex
    for i = sourceIndex to sourceIndex + count - 1 do
        target.[i + diff] <- source.[i]

let partition (f: 'T -> bool) (source: ArrayLike<'T>) (cons: ArrayCons) =
    let len = source.length |> int
    let ys = cons.Create len
    let zs = cons.Create len
    let mutable j = 0
    let mutable k = 0
    for i = 0 to len - 1 do
        if f source.[i] then
            ys.[j] <- source.[i]
            j <- j + 1
        else
            zs.[k] <- source.[i]
            k <- k + 1
    ys, zs

let foldIndexed f seed xs =
    let mutable acc = seed
    for i = 0 to length xs - 1 do
        acc <- f i acc xs.[i]
    acc

let fold<'a,'acc> f (seed:'acc) (xs: 'a []) = 
    foldIndexed (fun _ acc x -> f acc x) seed xs

let iterate f xs =
    fold (fun () x -> f x) () xs

let iterateIndexed f xs =
    foldIndexed (fun i () x -> f i x) () xs

let forAll f xs =
    fold (fun acc x -> f x && acc) true xs

let permute f xs (cons: ArrayCons) = 
    let size = length xs
    let ys  = cons.Create size
    let checkFlags = cons.Create size
    iterateIndexed (fun i x ->
        let j = f i 
        if j < 0 || j >= size then 
            invalidOp "Not a valid permutation"
        ys.[j] <- x
        checkFlags.[j] <- 1) xs
    let isValid = forAll ((=) 1) checkFlags
    if not isValid then
        invalidOp "Not a valid permutation"
    ys

let removeInPlace (item: 'T) (xs: 'T[]) =
    let i = indexOf xs item;
    if i > -1 then
        splice xs i 1 |> ignore
        true
    else
        false

let setSlice (target: ArrayLike<'T>) (lower: int) (upper: int) (source: ArrayLike<'T>) =
    let length = (if upper > 0 then upper else int(target.length) - 1) - lower
    if ArrayBuffer.isView target :?> bool && source.length <= float(length) then
        setInArrayLike target source (float(lower))
    else
        for i = 0 to length - 1 do
            target.[i + lower] <- source.[i]

let sortInPlaceBy f xs =
    sortInPlaceWith (fun (x:'a) (y:'a) -> 
        let x = f x
        let y = f y
        compare x y) xs

let sortInPlace xs =
    sortInPlaceWith compare xs

let unzip xs (cons: ArrayCons) =
    let len = length xs
    let bs = cons.Create len
    let cs = cons.Create len
    iterateIndexed (fun i (b, c) ->
        bs.[i] <- b
        cs.[i] <- c
    ) xs
    bs, cs

let unzip3 xs (cons: ArrayCons) =
    let len = length xs
    let bs = cons.Create len
    let cs = cons.Create len
    let ds = cons.Create len
    iterateIndexed (fun i (b, c, d) ->
        bs.[i] <- b
        cs.[i] <- c
        ds.[i] <- d
    ) xs
    bs, cs, ds

let chunkBySize (chunkSize: int) (xs: 'T[]): 'T[][] =
    if chunkSize < 1 then invalidArg "size" "The input must be positive."

    if xs.Length = 0 then [| [||] |]
    else
        let result: 'T[][] = [||]
        // add each chunk to the result
        for x = 0 to System.Math.Floor(float(xs.Length) / float(chunkSize)) |> int do
            let start = x * chunkSize;
            let end' = start + chunkSize;
            let slice = slice xs start end'
            push result slice |> ignore

        result

let fill (xs:'a []) offset count value =
    for i = offset to offset + count - 1 do
        xs.[i] <- value

let splitAt (index: int) (xs: 'T[]): 'T[] * 'T[] =
    if index < 0 then invalidArg "index" "The input must be non-negative."
    if index > xs.Length then invalidArg "index" "The input sequence has an insufficient number of elements."

    slice xs 0 index, sliceFrom xs index
