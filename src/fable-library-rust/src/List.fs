module List

type Node<'T> = {
    head: 'T
    mutable tail: Node<'T> option
}

type List<'T> = Node<'T> option
type 'T list = List<'T>

module SR =
    let indexOutOfBounds = "The index was outside the range of elements in the list."
    let inputListWasEmpty = "List was empty"
    let inputMustBeNonNegative = "The input must be non-negative."
    let inputSequenceEmpty = "The input sequence was empty."
    let inputSequenceTooLong = "The input sequence contains more than one element."
    let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
    let differentLengths = "The lists had different lengths."
    let notEnoughElements = "The input sequence has an insufficient number of elements."

let inline indexNotFound() = failwith SR.keyNotFoundAlt

let inline private consNoTail (x: 'T): 'T list =
    Some { head = x; tail = None }

let private setConsTail (t: 'T list) (xs: 'T list) =
    match xs with
    | Some node -> node.tail <- t
    | None -> ()

let private emptyRoot (): 'T list =
    Some { head = Unchecked.defaultof<_>; tail = None }

let private appendConsNoTail (x: 'T) (xs: 'T list) =
    let t = consNoTail x
    setConsTail t xs
    t

// type List<'T> with
// TODO: there may be some class members here when those are supported

let empty (): 'T list = //List.Empty
    None
    // { head = Unchecked.defaultof<_>; tail = None }

let cons (x: 'T) (xs: 'T list) = //List.Cons(x, xs)
    Some { head = x; tail = xs }

let singleton (x: 'T) = //List.Cons(x, List.Empty)
    cons x (empty())

let isEmpty (xs: 'T list) = //xs.IsEmpty
    xs |> Option.isNone

let head (xs: 'T list) = //xs.Head
    match xs with
    | Some node -> node.head
    | None -> invalidArg "list" SR.inputListWasEmpty

let tryHead (xs: 'T list) = //xs.TryHead
    match xs with
    | Some node -> Some (node.head)
    | None -> None

let tail (xs: 'T list) = //xs.Tail
    match xs with
    | Some node -> node.tail
    | None -> invalidArg "list" SR.inputListWasEmpty

let length (xs: 'T list) = //xs.Length
    let rec loop i xs =
        match xs with
        | None -> i
        | Some node -> loop (i + 1) node.tail
    loop 0 xs

// let compareWith (comparer: 'T -> 'T -> int) (xs: 'T list) (ys: 'T list): int =
//     let rec loop (xs: 'T list) (ys: 'T list) =
//         match (isEmpty xs), (isEmpty ys) with
//         | true, true -> 0
//         | true, false -> -1
//         | false, true -> 1
//         | false, false ->
//             let c = comparer (head xs) (head ys)
//             if c = 0 then loop (tail xs) (tail ys) else c
//     loop xs ys

let rec tryLast (xs: 'T list) =
    match xs with
    | None -> None
    | Some node ->
        if (isEmpty node.tail)
        then Some (node.head)
        else tryLast node.tail

let last (xs: 'T list) =
    match tryLast xs with
    | Some x -> x
    | None -> failwith SR.inputListWasEmpty

let ofOption<'T> (opt: 'T option): 'T list =
    match opt with
    | Some x -> singleton x
    | None -> empty()

let toArray (xs: 'T list) =
    let len = length xs
    let res = Array.zeroCreate len
    let rec loop (arr: 'T[]) i xs =
        if not (isEmpty xs) then
            arr.[i] <- (head xs)
            loop arr (i + 1) (tail xs)
    loop res 0 xs
    res

// let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
//     if (isEmpty xs) then state
//     else fold folder (folder state (head xs)) (tail xs)

let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
    let mutable acc = state
    let mutable xs = xs
    while not (isEmpty xs) do
        acc <- folder acc (head xs)
        xs <- (tail xs)
    acc

let reverse (xs: 'T list) =
    fold (fun acc x -> cons x acc) (empty()) xs

let foldBack (folder: 'T -> 'State -> 'State) (xs: 'T list) (state: 'State) =
    fold (fun acc x -> folder x acc) state (reverse xs)
    // Array.foldBack folder (toArray xs) state

// let rec fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (xs: 'T1 list) (ys: 'T2 list) =
//     if (isEmpty xs) || (isEmpty ys) then state
//     else fold2 folder (folder state (head xs) (head ys)) (tail xs) (tail ys)

let fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (xs: 'T1 list) (ys: 'T2 list) =
    let mutable acc = state
    let mutable xs = xs
    let mutable ys = ys
    while not (isEmpty xs) && not (isEmpty ys) do
        acc <- folder acc (head xs) (head ys)
        xs <- (tail xs)
        ys <- (tail ys)
    acc

let foldBack2 (folder: 'T1 -> 'T2 -> 'State -> 'State) (xs: 'T1 list) (ys: 'T2 list) (state: 'State) =
    fold2 (fun acc x y -> folder x y acc) state (reverse xs) (reverse ys)
    // Array.foldBack2 folder (toArray xs) (toArray ys) state

// let foldIndexed (folder: int -> 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
//     // let rec loop i acc (xs: 'T list) =
//     //     if (isEmpty xs) then acc
//     //     else loop (i + 1) (folder i acc (head xs)) (tail xs)
//     // loop 0 state xs
//     let mutable acc = state
//     let mutable xs = xs
//     while not (isEmpty xs) do
//         acc <- folder acc (head xs)
//         xs <- (tail xs)
//     acc

let unfold (gen: 'State -> ('T * 'State) option) (state: 'State) =
    let rec loop gen acc (root: 'T list) (xs: 'T list) =
        match gen acc with
        | None -> root
        | Some (x, acc) ->
            let t = xs |> appendConsNoTail x
            let root = if isEmpty root then t else root
            loop gen acc root t
    loop gen state (empty()) (empty())

let iterate action xs =
    fold (fun () x -> action x) () xs

let iterate2 action xs ys =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed action xs =
    fold (fun i x -> action i x; i + 1) 0 xs |> ignore

let iterateIndexed2 action xs ys =
    fold2 (fun i x y -> action i x y; i + 1) 0 xs ys |> ignore

// Redirected to Seq.ofList to avoid dependency (see Replacements)
// let toSeq (xs: 'T list): 'T seq = Seq.ofList xs
// //     xs :> System.Collections.Generic.IEnumerable<'T>

// let toSeq (xs: 'T list): IEnumerator<'T> =
//     let mutable curr = xs.root
//     let next() =
//         match curr with
//         | Some node ->
//             curr <- node.next
//             Some (node.item)
//         | None -> None
//     Enumerable.fromFunction next

let ofArrayWithTail (xs: 'T[]) (tail: 'T list) =
    let mutable res = tail
    let len = Array.length xs
    for i = len - 1 downto 0 do
        res <- cons xs.[i] res
    res

let ofArray (xs: 'T[]) =
    ofArrayWithTail xs (empty())

// Redirected to Seq.toList to avoid dependency (see Replacements)
// let ofSeq (xs: seq<'T>): 'T list = Seq.toList xs
// //     match xs with
// //     | :? array<'T> as xs -> ofArray xs
// //     | :? list<'T> as xs -> xs
// //     | _ ->
// //         let root = (empty())
// //         let mutable node = root
// //         for x in xs do
// //             node <- node |> appendConsNoTail x
// //         node |> setConsTailEmpty
// //         root |> tail

// let concat (lists: seq<'T list>) =
//     let root = (empty())
//     let mutable node = root
//     let action xs = node <- fold (fun acc x -> acc |> appendConsNoTail x) node xs
//     match lists with
//     | :? array<'T list> as xs -> Array.iter action xs
//     | :? list<'T list> as xs -> iterate action xs
//     | _ -> for xs in lists do action xs
//     node |> setConsTailEmpty
//     root |> tail

// let scan (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
//     let root = (empty())
//     let mutable node = root |> appendConsNoTail state
//     let mutable acc = state
//     let mutable xs = xs
//     while not (isEmpty xs) do
//         acc <- folder acc (head xs)
//         node <- node |> appendConsNoTail acc
//         xs <- (tail xs)
//     node |> setConsTailEmpty
//     root |> tail

// let scanBack (folder: 'T -> 'State -> 'State) (xs: 'T list) (state: 'State) =
//     Array.scanBack folder (toArray xs) state
//     |> ofArray

let append (xs: 'T list) (ys: 'T list) =
    fold (fun acc x -> cons x acc) ys (reverse xs)

let choose (chooser: 'T -> 'U option) (xs: 'T list) =
    let mutable root = empty()
    let mutable acc = root
    let mutable xs = xs
    while not (isEmpty xs) do
        match chooser (head xs) with
        | Some x ->
            acc <- acc |> appendConsNoTail x
            if isEmpty root then root <- acc
        | None -> ()
        xs <- (tail xs)
    root

// let collect (mapping: 'T -> 'U list) (xs: 'T list) =
//     let root = (empty())
//     let mutable node = root
//     let mutable ys = xs
//     while not (isEmpty ys) do
//         let mutable zs = mapping (head ys)
//         while not (isEmpty zs) do
//             node <- node |> appendConsNoTail (head zs)
//             zs <- (tail zs)
//         ys <- (tail ys)
//     node |> setConsTailEmpty
//     root |> tail

// let contains (value: 'T) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
//     tryFindIndex (fun v -> eq.Equals (value, v)) xs
//     |> Option.isSome

let map (mapping: 'T -> 'U) (xs: 'T list) =
    let gen xs =
        if (isEmpty xs) then None
        else Some(mapping (head xs), tail xs)
    unfold gen xs

let mapIndexed (mapping: int -> 'T -> 'U) (xs: 'T list) =
    let gen (i, xs) =
        if (isEmpty xs) then None
        else Some(mapping i (head xs), (i + 1, tail xs))
    unfold gen (0, xs)

let indexed xs =
    mapIndexed (fun i x -> (i, x)) xs

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: 'T1 list) (ys: 'T2 list) =
    let gen (xs, ys) =
        if (isEmpty xs) || (isEmpty ys) then None
        else Some(mapping (head xs) (head ys), (tail xs, tail ys))
    unfold gen (xs, ys)

let mapIndexed2 (mapping: int -> 'T1 -> 'T2 -> 'U) (xs: 'T1 list) (ys: 'T2 list) =
    let gen (i, xs, ys) =
        if (isEmpty xs) || (isEmpty ys) then None
        else Some(mapping i (head xs) (head ys), (i + 1, tail xs, tail ys))
    unfold gen (0, xs, ys)

let map3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'U) (xs: 'T1 list) (ys: 'T2 list) (zs: 'T3 list) =
    let gen (xs, ys, zs) =
        if (isEmpty xs) || (isEmpty ys) || (isEmpty zs) then None
        else Some(mapping (head xs) (head ys) (head zs), (tail xs, tail ys, tail zs))
    unfold gen (xs, ys, zs)

// let mapFold (mapping: 'State -> 'T -> 'Result * 'State) (state: 'State) (xs: 'T list) =
//     let folder (node: 'Result list, st) x =
//         let r, st = mapping st x
//         node |> appendConsNoTail r, st
//     let root = (empty())
//     let node, state = fold folder (root, state) xs
//     node |> setConsTailEmpty
//     root |> tail, state

// let mapFoldBack (mapping: 'T -> 'State -> 'Result * 'State) (xs: 'T list) (state: 'State) =
//     mapFold (fun acc x -> mapping x acc) state (reverse xs)

let tryPick (chooser: 'T -> 'U option) (xs: 'T list) =
    let rec loop (xs: 'T list) =
        if (isEmpty xs) then None
        else
            match chooser (head xs) with
            | Some _  as res -> res
            | None -> loop (tail xs)
    loop xs

let pick (chooser: 'T -> 'U option) (xs: 'T list) =
    match tryPick chooser xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFind (predicate: 'T -> bool) (xs: 'T list) =
    tryPick (fun x -> if predicate x then Some x else None) xs

let find (predicate: 'T -> bool) (xs: 'T list) =
    match tryFind predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindBack (predicate: 'T -> bool) (xs: 'T list) =
    xs |> toArray |> Array.tryFindBack predicate

let findBack (predicate: 'T -> bool) (xs: 'T list) =
    match tryFindBack predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindIndex (predicate: 'T -> bool) (xs: 'T list): int option =
    let rec loop i (xs: 'T list) =
        if (isEmpty xs) then None
        else
            if predicate (head xs)
            then Some i
            else loop (i + 1) (tail xs)
    loop 0 xs

let findIndex (predicate: 'T -> bool) (xs: 'T list): int =
    match tryFindIndex predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindIndexBack (predicate: 'T -> bool) (xs: 'T list): int option =
    xs |> toArray |> Array.tryFindIndexBack predicate

let findIndexBack (predicate: 'T -> bool) (xs: 'T list): int =
    match tryFindIndexBack predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryItem index (xs: 'T list) =
    let mutable xs = xs
    let mutable i = 0
    while i < index && not (isEmpty xs) do
        i <- i + 1
        xs <- tail xs
    if i < index || index < 0
    then None
    else Some (head xs)

let item index (xs: 'T list) = // xs.Item(n)
    match tryItem index xs with
    | None -> invalidArg "index" SR.indexOutOfBounds
    | Some x -> x

let filter (predicate: 'T -> bool) (xs: 'T list) =
    let mutable root = empty()
    let mutable acc = root
    let mutable xs = xs
    while not (isEmpty xs) do
        let x = head xs
        if predicate x then
            acc <- acc |> appendConsNoTail x
            if isEmpty root then root <- acc
        xs <- tail xs
    root

let initialize count (initializer: int -> 'T) =
    let gen i =
        if i < count
        then Some(initializer i, i + 1)
        else None
    unfold gen 0

let pairwise (xs: 'T list) =
    toArray xs
    |> Array.pairwise
    |> ofArray

let partition (predicate: 'T -> bool) (xs: 'T list) =
    let mutable root1 = empty()
    let mutable root2 = empty()
    let mutable acc1 = root1
    let mutable acc2 = root2
    let mutable xs = xs
    while not (isEmpty xs) do
        let x = head xs
        if predicate x then
            acc1 <- acc1 |> appendConsNoTail x
            if isEmpty root1 then root1 <- acc1
        else
            acc2 <- acc2 |> appendConsNoTail x
            if isEmpty root2 then root2 <- acc2
        xs <- tail xs
    root1, root2

let reduce reduction (xs: 'T list) =
    if (isEmpty xs) then invalidOp SR.inputListWasEmpty
    fold reduction (head xs) (tail xs)

let reduceBack reduction (xs: 'T list) =
    if (isEmpty xs) then invalidOp SR.inputListWasEmpty
    foldBack reduction (tail xs) (head xs)

let replicate count initial =
    initialize count (fun _ -> initial)

let forAll f (xs: 'T list) =
    //TODO: stop early
    fold (fun acc x -> acc && f x) true xs

let forAll2 f (xs: 'T1 list) (ys: 'T2 list) =
    //TODO: stop early
    fold2 (fun acc x y -> acc && f x y) true xs ys

let exists f (xs: 'T list) =
    tryFindIndex f xs |> Option.isSome

let rec exists2 (f: 'T1 -> 'T2 -> bool) (xs: 'T1 list) (ys: 'T2 list) =
    match (isEmpty xs), (isEmpty ys) with
    | true, true -> false
    | false, false -> f (head xs) (head ys) || exists2 f (tail xs) (tail ys)
    | _ -> invalidArg "list2" SR.differentLengths

let unzip xs =
    foldBack (fun (x, y) (lacc, racc) -> cons x lacc, cons y racc) xs ((empty()), (empty()))

let unzip3 xs =
    foldBack (fun (x, y, z) (lacc, macc, racc) -> cons x lacc, cons y macc, cons z racc) xs ((empty()), (empty()), (empty()))

let zip xs ys =
    map2 (fun x y -> x, y) xs ys

let zip3 xs ys zs =
    map3 (fun x y z -> x, y, z) xs ys zs

// let sortWith (comparer: 'T -> 'T -> int) (xs: 'T list) =
//     let arr = toArray xs
//     Array.sortInPlaceWith comparer arr // Note: In JS this sort is stable
//     arr |> ofArray

// let sort (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>) =
//     sortWith (fun x y -> comparer.Compare(x, y)) xs

// let sortBy (projection: 'T -> 'U) (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>) =
//     sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

// let sortDescending (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>) =
//     sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

// let sortByDescending (projection: 'T -> 'U) (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>) =
//     sortWith (fun x y -> comparer.Compare(projection x, projection y) * -1) xs

// let sum (xs: 'T list) ([<Inject>] adder: IGenericAdder<'T>): 'T =
//     fold (fun acc x -> adder.Add(acc, x)) (adder.GetZero()) xs

// let sumBy (f: 'T -> 'U) (xs: 'T list) ([<Inject>] adder: IGenericAdder<'U>): 'U =
//     fold (fun acc x -> adder.Add(acc, f x)) (adder.GetZero()) xs

// let maxBy (projection: 'T -> 'U) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T =
//     reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

// let max xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T =
//     reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) xs

// let minBy (projection: 'T -> 'U) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T =
//     reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then x else y) xs

// let min (xs: 'T list) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T =
//     reduce (fun x y -> if comparer.Compare(y, x) > 0 then x else y) xs

// let average (xs: 'T list) ([<Inject>] averager: IGenericAverager<'T>): 'T =
//     let mutable count = 0
//     let folder acc x = count <- count + 1; averager.Add(acc, x)
//     let total = fold folder (averager.GetZero()) xs
//     averager.DivideByInt(total, count)

// let averageBy (f: 'T -> 'U) (xs: 'T list) ([<Inject>] averager: IGenericAverager<'U>): 'U =
//     let mutable count = 0
//     let inline folder acc x = count <- count + 1; averager.Add(acc, f x)
//     let total = fold folder (averager.GetZero()) xs
//     averager.DivideByInt(total, count)

let permute (indexMap: int -> int) (xs: 'T list) =
    toArray xs
    |> Array.permute indexMap
    |> ofArray

// let chunkBySize (chunkSize: int) (xs: 'T list): 'T list list =
//     toArray xs
//     |> Array.chunkBySize chunkSize
//     |> Array.map ofArray
//     |> ofArray

// let allPairs (xs: 'T1 list) (ys: 'T2 list): ('T1 * 'T2) list =
//     let root = (empty())
//     let mutable node = root
//     iterate (fun x ->
//         iterate (fun y ->
//             node <- node |> appendConsNoTail (x, y)
//         ) ys) xs
//     node |> setConsTailEmpty
//     root |> tail

let rec skip count (xs: 'T list) =
    if count <= 0 then xs
    else
        if (isEmpty xs) then invalidArg "list" SR.notEnoughElements
        skip (count - 1) (tail xs)

let rec skipWhile (predicate: 'T -> bool) (xs: 'T list) =
    if (isEmpty xs) then xs
    elif not (predicate (head xs)) then xs
    else skipWhile predicate (tail xs)

let take count (xs: 'T list) =
    if count < 0 then invalidArg "count" SR.inputMustBeNonNegative
    let gen (i, xs: 'T list) =
        if i > 0 then
            if (isEmpty xs) then invalidArg "list" SR.notEnoughElements
            Some(head xs, (i - 1, tail xs))
        else None
    unfold gen (count, xs)

let takeWhile (predicate: 'T -> bool) (xs: 'T list) =
    let gen xs =
        if not (isEmpty xs) && (predicate (head xs))
        then Some(head xs, tail xs)
        else None
    unfold gen xs

let truncate count (xs: 'T list) =
    let gen (i, xs: 'T list) =
        if i > 0 && not (isEmpty xs)
        then Some(head xs, (i - 1, tail xs))
        else None
    unfold gen (count, xs)

// let getSlice (startIndex: int option) (endIndex: int option) (xs: 'T list) =
//     let len = length xs
//     let startIndex = defaultArg startIndex 0
//     let endIndex = defaultArg endIndex (len - 1)
//     if startIndex < 0 then invalidArg "startIndex" SR.indexOutOfBounds
//     elif endIndex >= len then invalidArg "endIndex" SR.indexOutOfBounds
//     elif endIndex < startIndex then (empty())
//     else xs |> skip startIndex |> take (endIndex - startIndex + 1)

let splitAt index (xs: 'T list) =
    if index < 0 then invalidArg "index" SR.inputMustBeNonNegative
    if index > length xs then invalidArg "index" SR.notEnoughElements
    take index xs, skip index xs

let exactlyOne (xs: 'T list) =
    if (isEmpty xs)
    then invalidArg "list" SR.inputSequenceEmpty
    else
        if isEmpty (tail xs) then (head xs)
        else invalidArg "list" SR.inputSequenceTooLong

let tryExactlyOne (xs: 'T list) =
    if not (isEmpty xs) && isEmpty (tail xs)
    then Some (head xs)
    else None

let where predicate (xs: 'T list) =
    filter predicate xs

// let windowed (windowSize: int) (xs: 'T list): 'T list list =
//     toArray xs
//     |> Array.windowed windowSize
//     |> Array.map ofArray
//     |> ofArray

// let splitInto (chunks: int) (xs: 'T list): 'T list list =
//     toArray xs
//     |> Array.splitInto chunks
//     |> Array.map ofArray
//     |> ofArray

// let transpose (lists: seq<'T list>): 'T list list =
//     lists
//     |> Array.ofSeq
//     |> Array.map toArray
//     |> Array.transpose
//     |> Array.map ofArray
//     |> ofArray

// let insertAt (index: int) (y: 'T) (xs: 'T list): 'T list =
//     let mutable i = -1
//     let mutable isDone = false
//     let result =
//         (List.Empty, xs) ||> fold (fun acc x ->
//             i <- i + 1
//             if i = index then
//                 isDone <- true
//                 List.Cons(x, List.Cons(y, acc))
//             else List.Cons(x, acc))
//     let result =
//         if isDone then result
//         elif i + 1 = index then List.Cons(y, result)
//         else invalidArg "index" SR.indexOutOfBounds
//     reverse result

// let insertManyAt (index: int) (ys: seq<'T>) (xs: 'T list): 'T list =
//     let mutable i = -1
//     let mutable isDone = false
//     let ys = ofSeq ys
//     let result =
//         (List.Empty, xs) ||> fold (fun acc x ->
//             i <- i + 1
//             if i = index then
//                 isDone <- true
//                 List.Cons(x, append ys acc)
//             else List.Cons(x, acc))
//     let result =
//         if isDone then result
//         elif i + 1 = index then append ys result
//         else invalidArg "index" SR.indexOutOfBounds
//     reverse result

// let removeAt (index: int) (xs: 'T list): 'T list =
//     let mutable i = -1
//     let mutable isDone = false
//     let ys =
//         xs |> filter (fun _ ->
//             i <- i + 1
//             if i = index then
//                 isDone <- true
//                 false
//             else true)
//     if not isDone then
//         invalidArg "index" SR.indexOutOfBounds
//     ys

// let removeManyAt (index: int) (count: int) (xs: 'T list): 'T list =
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

// let updateAt (index: int) (y: 'T) (xs: 'T list): 'T list =
//     let mutable isDone = false
//     let ys =
//         xs |> mapIndexed (fun i x ->
//             if i = index then
//                 isDone <- true
//                 y
//             else x)
//     if not isDone then
//         invalidArg "index" SR.indexOutOfBounds
//     ys

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
