module List

type Node<'T> = {
    item: 'T
    next: Node<'T> option
}

type List<'T> = {
    root: Node<'T> option
}

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

// type List<'T> with
// TODO: there may be some class members here when those are supported

let empty (): 'T list = //List.Empty
    { root = None }

let cons (x: 'T) (xs: 'T list) = //List.Cons(x, xs)
    { root = Some { item = x; next = xs.root } }

let singleton (x: 'T) = //List.Cons(x, List.Empty)
    cons x (empty())

let isEmpty (xs: 'T list) = //xs.IsEmpty
    xs.root |> Option.isNone

let head (xs: 'T list) = //xs.Head
    (xs.root |> Option.get).item

let tryHead (xs: 'T list) = //xs.TryHead
    xs.root |> Option.map (fun node -> node.item)

let tail (xs: 'T list) = //xs.Tail
    { root = xs.root |> Option.bind (fun node -> node.next) }

let length (xs: 'T list) = //xs.Length
    let rec loop i next =
        match next with
        | None -> i
        | Some node -> loop (i + 1) node.next
    loop 0 xs.root

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
    if (isEmpty xs) then None
    else
        let t = (tail xs)
        if (isEmpty t) then Some (head xs)
        else tryLast t

let last (xs: 'T list) =
    match tryLast xs with
    | Some x -> x
    | None -> failwith SR.inputListWasEmpty

let ofOption<'T> (opt: 'T option): 'T list =
    match opt with
    | Some x -> singleton x
    | None -> empty()

//TODO: redo when ResizeArray is available
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

// let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
//     let mutable acc = state
//     let mutable xs = xs
//     while not (isEmpty xs) do
//         acc <- folder acc (head xs)
//         xs <- (tail xs)
//     acc

let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
    let mutable acc = state
    let mutable node = xs.root
    while Option.isSome node do
        let item = node.Value.item
        let next = node.Value.next
        acc <- folder acc item
        node <- next
    acc

let reverse (xs: 'T list) =
    fold (fun acc x -> cons x acc) (empty()) xs

let foldBack (folder: 'T -> 'State -> 'State) (xs: 'T list) (state: 'State) =
    fold (fun acc x -> folder x acc) state (reverse xs)
    // Array.foldBack folder (toArray xs) state

let foldIndexed (folder: int -> 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
    let rec loop i acc (xs: 'T list) =
        if (isEmpty xs) then acc
        else loop (i + 1) (folder i acc (head xs)) (tail xs)
    loop 0 state xs

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

// let unfold (gen: 'State -> ('T * 'State) option) (state: 'State) =
//     let rec loop acc (node: 'T list) =
//         match gen acc with
//         | None -> node
//         | Some (x, acc) -> loop acc (node.AppendConsNoTail x)
//     let root = (empty())
//     let node = loop state root
//     node.SetConsTail (empty())
//     root.Tail

let iterate action xs =
    fold (fun () x -> action x) () xs

let iterate2 action xs ys =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed action xs =
    fold (fun i x -> action i x; i + 1) 0 xs |> ignore

let iterateIndexed2 action xs ys =
    fold2 (fun i x y -> action i x y; i + 1) 0 xs ys |> ignore

// List.toSeq is redirected to Seq.ofList to avoid dependency

// let toSeq (xs: 'T list): 'T seq =
//     xs :> System.Collections.Generic.IEnumerable<'T>

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

// List.ofSeq is redirected to Seq.toList to avoid dependency

// let ofSeq (xs: seq<'T>): 'T list =
//     match xs with
//     | :? array<'T> as xs -> ofArray xs
//     | :? list<'T> as xs -> xs
//     | _ ->
//         let root = (empty())
//         let mutable node = root
//         for x in xs do
//             node <- node.AppendConsNoTail x
//         node.SetConsTail (empty())
//         root.Tail

// let concat (lists: seq<'T list>) =
//     let root = (empty())
//     let mutable node = root
//     let action xs = node <- fold (fun acc x -> acc.AppendConsNoTail x) node xs
//     match lists with
//     | :? array<'T list> as xs -> Array.iter action xs
//     | :? list<'T list> as xs -> iterate action xs
//     | _ -> for xs in lists do action xs
//     node.SetConsTail (empty())
//     root.Tail

// let scan (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T list) =
//     let root = (empty())
//     let mutable node = root.AppendConsNoTail state
//     let mutable acc = state
//     let mutable xs = xs
//     while not (isEmpty xs) do
//         acc <- folder acc (head xs)
//         node <- node.AppendConsNoTail acc
//         xs <- (tail xs)
//     node.SetConsTail (empty())
//     root.Tail

// let scanBack (folder: 'T -> 'State -> 'State) (xs: 'T list) (state: 'State) =
//     Array.scanBack folder (toArray xs) state
//     |> ofArray

let append (xs: 'T list) (ys: 'T list) =
    fold (fun acc x -> cons x acc) ys (reverse xs)

// let collect (mapping: 'T -> 'U list) (xs: 'T list) =
//     let root = (empty())
//     let mutable node = root
//     let mutable ys = xs
//     while not (isEmpty ys) do
//         let mutable zs = mapping (head ys)
//         while not (isEmpty zs) do
//             node <- node.AppendConsNoTail (head zs)
//             zs <- (tail zs)
//         ys <- (tail ys)
//     node.SetConsTail (empty())
//     root.Tail

let mapIndexed (mapping: int -> 'T -> 'U) (xs: 'T list) =
    foldIndexed (fun i acc x -> cons (mapping i x) acc) (empty()) xs
    |> reverse

// let mapIndexed (mapping: int -> 'T -> 'U) (xs: 'T list) =
//     let root = (empty())
//     let folder i (acc: 'U list) x = acc.AppendConsNoTail (mapping i x)
//     let node = foldIndexed folder root xs
//     node.SetConsTail (empty())
//     root.Tail

let map (mapping: 'T -> 'U) (xs: 'T list) =
    fold (fun acc x -> cons (mapping x) acc) (empty()) xs
    |> reverse

// let map (mapping: 'T -> 'U) (xs: 'T list) =
//     let root = (empty())
//     let folder (acc: 'U list) x = acc.AppendConsNoTail (mapping x)
//     let node = fold folder root xs
//     node.SetConsTail (empty())
//     root.Tail

let indexed xs =
    mapIndexed (fun i x -> (i, x)) xs

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: 'T1 list) (ys: 'T2 list) =
    fold2 (fun acc x y -> cons (mapping x y) acc) (empty()) xs ys
    |> reverse

// let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: 'T1 list) (ys: 'T2 list) =
//     let root = (empty())
//     let folder (acc: 'U list) x y = acc.AppendConsNoTail (mapping x y)
//     let node = fold2 folder root xs ys
//     node.SetConsTail (empty())
//     root.Tail

// let mapIndexed2 (mapping: int -> 'T1 -> 'T2 -> 'U) (xs: 'T1 list) (ys: 'T2 list) =
//     let rec loop i (acc: 'U list) (xs: 'T1 list) (ys: 'T2 list) =
//         if (isEmpty xs) || (isEmpty ys) then acc
//         else
//             let node = acc.AppendConsNoTail (mapping i (head xs) (head ys))
//             loop (i + 1) node (tail xs) (tail ys)
//     let root = (empty())
//     let node = loop 0 root xs ys
//     node.SetConsTail (empty())
//     root.Tail

// let map3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'U) (xs: 'T1 list) (ys: 'T2 list) (zs: 'T3 list) =
//     let rec loop (acc: 'U list) (xs: 'T1 list) (ys: 'T2 list) (zs: 'T3 list) =
//         if (isEmpty xs) || (isEmpty ys) || (isEmpty zs) then acc
//         else
//             let node = acc.AppendConsNoTail (mapping (head xs) (head ys) (head zs))
//             loop node (tail xs) (tail ys) (tail zs)
//     let root = (empty())
//     let node = loop root xs ys zs
//     node.SetConsTail (empty())
//     root.Tail

// let mapFold (mapping: 'State -> 'T -> 'Result * 'State) (state: 'State) (xs: 'T list) =
//     let folder (node: 'Result list, st) x =
//         let r, st = mapping st x
//         node.AppendConsNoTail r, st
//     let root = (empty())
//     let node, state = fold folder (root, state) xs
//     node.SetConsTail (empty())
//     root.Tail, state

// let mapFoldBack (mapping: 'T -> 'State -> 'Result * 'State) (xs: 'T list) (state: 'State) =
//     mapFold (fun acc x -> mapping x acc) state (reverse xs)

let tryPick f xs =
    let rec loop (xs: 'T list) =
        if (isEmpty xs) then None
        else
            match f (head xs) with
            | Some _  as res -> res
            | None -> loop (tail xs)
    loop xs

let pick f xs =
    match tryPick f xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFind f xs =
    tryPick (fun x -> if f x then Some x else None) xs

let find f xs =
    match tryFind f xs with
    | Some x -> x
    | None -> indexNotFound()

// let tryFindBack f xs =
//     xs |> toArray |> Array.tryFindBack f

// let findBack f xs =
//     match tryFindBack f xs with
//     | Some x -> x
//     | None -> indexNotFound()

let tryFindIndex f xs: int option =
    let rec loop i (xs: 'T list) =
        if (isEmpty xs) then None
        else
            if f (head xs)
            then Some i
            else loop (i + 1) (tail xs)
    loop 0 xs

let findIndex f xs: int =
    match tryFindIndex f xs with
    | Some x -> x
    | None -> indexNotFound()

// let tryFindIndexBack f xs: int option =
//     xs |> toArray |> Array.tryFindIndexBack f

// let findIndexBack f xs: int =
//     match tryFindIndexBack f xs with
//     | Some x -> x
//     | None -> indexNotFound()

let tryItem n (xs: 'T list) =
    let rec loop i (xs: 'T list) =
        if (isEmpty xs) then None
        else
            if i = n then Some (head xs)
            else loop (i + 1) (tail xs)
    loop 0 xs

let item n (xs: 'T list) = // xs.Item(n)
    match tryItem n xs with
    | Some t -> t
    | None -> invalidArg "index" SR.indexOutOfBounds

// let filter f (xs: 'T list) =
//     let root = (empty())
//     let folder (acc: 'T list) x =
//         if f x then acc.AppendConsNoTail x else acc
//     let node = fold folder root xs
//     node.SetConsTail (empty())
//     root.Tail

// let partition f (xs: 'T list) =
//     let root1, root2 = (empty()), (empty())
//     let folder (lacc: 'T list, racc: 'T list) x =
//         if f x
//         then lacc.AppendConsNoTail x, racc
//         else lacc, racc.AppendConsNoTail x
//     let node1, node2 = fold folder (root1, root2) xs
//     node1.SetConsTail (empty())
//     node2.SetConsTail (empty())
//     root1.Tail, root2.Tail

// let choose f (xs: 'T list) =
//     let root = (empty())
//     let folder (acc: 'T list) x =
//         match f x with
//         | Some y -> acc.AppendConsNoTail y
//         | None -> acc
//     let node = fold folder root xs
//     node.SetConsTail (empty())
//     root.Tail

// let contains (value: 'T) (xs: 'T list) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
//     tryFindIndex (fun v -> eq.Equals (value, v)) xs
//     |> Option.isSome

// let initialize n (f: int -> 'T) =
//     let root = (empty())
//     let mutable node = root
//     for i = 0 to n - 1 do
//         node <- node.AppendConsNoTail (f i)
//     node.SetConsTail (empty())
//     root.Tail

// let replicate n x =
//     initialize n (fun _ -> x)

// let reduce f (xs: 'T list) =
//     if (isEmpty xs) then invalidOp SR.inputListWasEmpty
//     else fold f (head xs) (tail xs)

// let reduceBack f (xs: 'T list) =
//     if (isEmpty xs) then invalidOp SR.inputListWasEmpty
//     else foldBack f (tail xs) (head xs)

let forAll f xs =
    fold (fun acc x -> acc && f x) true xs

let forAll2 f xs ys =
    fold2 (fun acc x y -> acc && f x y) true xs ys

let exists f xs =
    tryFindIndex f xs |> Option.isSome

let rec exists2 (f: 'T1 -> 'T2 -> bool) (xs: 'T1 list) (ys: 'T2 list) =
    match (isEmpty xs), (isEmpty ys) with
    | true, true -> false
    | false, false -> f (head xs) (head ys) || exists2 f (tail xs) (tail ys)
    | _ -> invalidArg "list2" SR.differentLengths

// let unzip xs =
//     foldBack (fun (x, y) (lacc, racc) -> cons x lacc, cons y racc) xs ((empty()), (empty()))

// let unzip3 xs =
//     foldBack (fun (x, y, z) (lacc, macc, racc) -> cons x lacc, cons y macc, cons z racc) xs ((empty()), (empty()), (empty()))

// let zip xs ys =
//     map2 (fun x y -> x, y) xs ys

// let zip3 xs ys zs =
//     map3 (fun x y z -> x, y, z) xs ys zs

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

// let permute f (xs: 'T list) =
//     toArray xs
//     |> Array.permute f
//     |> ofArray

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
//             node <- node.AppendConsNoTail (x, y)
//         ) ys) xs
//     node.SetConsTail (empty())
//     root.Tail

// let rec skip count (xs: 'T list) =
//     if count <= 0 then xs
//     elif (isEmpty xs) then invalidArg "list" SR.notEnoughElements
//     else skip (count - 1) (tail xs)

// let rec skipWhile predicate (xs: 'T list) =
//     if (isEmpty xs) then xs
//     elif not (predicate (head xs)) then xs
//     else skipWhile predicate (tail xs)

// let take count (xs: 'T list) =
//     if count < 0 then invalidArg "count" SR.inputMustBeNonNegative
//     let rec loop i (acc: 'T list) (xs: 'T list) =
//         if i <= 0 then acc
//         elif (isEmpty xs) then invalidArg "list" SR.notEnoughElements
//         else loop (i - 1) (acc.AppendConsNoTail (head xs)) (tail xs)
//     let root = (empty())
//     let node = loop count root xs
//     node.SetConsTail (empty())
//     root.Tail

// let takeWhile predicate (xs: 'T list) =
//     let rec loop (acc: 'T list) (xs: 'T list) =
//         if (isEmpty xs) then acc
//         elif not (predicate (head xs)) then acc
//         else loop (acc.AppendConsNoTail (head xs)) (tail xs)
//     let root = (empty())
//     let node = loop root xs
//     node.SetConsTail (empty())
//     root.Tail

// let truncate count (xs: 'T list) =
//     let rec loop i (acc: 'T list) (xs: 'T list) =
//         if i <= 0 then acc
//         elif (isEmpty xs) then acc
//         else loop (i - 1) (acc.AppendConsNoTail (head xs)) (tail xs)
//     let root = (empty())
//     let node = loop count root xs
//     node.SetConsTail (empty())
//     root.Tail

// let getSlice (startIndex: int option) (endIndex: int option) (xs: 'T list) =
//     let len = length xs
//     let startIndex = defaultArg startIndex 0
//     let endIndex = defaultArg endIndex (len - 1)
//     if startIndex < 0 then invalidArg "startIndex" SR.indexOutOfBounds
//     elif endIndex >= len then invalidArg "endIndex" SR.indexOutOfBounds
//     elif endIndex < startIndex then (empty())
//     else xs |> skip startIndex |> take (endIndex - startIndex + 1)

// let splitAt index (xs: 'T list) =
//     if index < 0 then invalidArg "index" SR.inputMustBeNonNegative
//     if index > length xs then invalidArg "index" SR.notEnoughElements
//     take index xs, skip index xs

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

// let where predicate (xs: 'T list) =
//     filter predicate xs

// let pairwise (xs: 'T list) =
//     toArray xs
//     |> Array.pairwise
//     |> ofArray

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
//     let ys = (ys, List.Empty) ||> Seq.foldBack (fun y acc -> List.Cons(y, acc))
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
