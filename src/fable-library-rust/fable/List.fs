module List

type Node<'T> = {
    elem: 'T
    next: Option<Node<'T>>
}

type List<'T> = {
    root: Option<Node<'T>>
}

type 'T list = List<'T>

// type List<'T> with
// TODO: there will be some class members here when those are supported

let empty () = //List.Empty
    { root = None }

let cons (x: 'T) (xs: 'T list) = //List.Cons(x, xs)
    { root = Some { elem = x; next = xs.root } }

let singleton (x: 'T) = //List.Cons(x, List.Empty)
    cons x (empty())

let head (xs: 'T list) = //xs.Head
    (xs.root |> Option.get).elem

let tryHead (xs: 'T list) = //xs.TryHead
    xs.root |> Option.map (fun node -> node.elem)

let tail (xs: 'T list) = //xs.Tail
    { root = xs.root |> Option.bind (fun node -> node.next) }

let isEmpty (xs: 'T list) = //xs.IsEmpty
    xs.root |> Option.isNone

// let length (xs: 'T list) = //xs.Length
//     let rec loop i next =
//         match next with
//         | None -> i
//         | Some node -> loop (i + 1) node.next
//     loop 0 xs.root

// let tryLast (xs: 'T list) =
//     match xs.root with
//     | None -> None
//     | Some node ->
//         let rec loop node =
//             match node.next with
//             | None -> Some(node.elem)
//             | Some node -> loop node
//         loop node

// let last (xs: 'T list) =
//     match tryLast xs with
//     | Some x -> x
//     | None -> failwith "input list was empty"

// let toArray (xs: 'T list) =
//     let len = xs.Length
//     let res = Array.zeroCreate len
//     let rec loop i (xs: 'T list) =
//         if not xs.IsEmpty then
//             res.[i] <- xs.Head
//             loop (i + 1) xs.Tail
//     loop 0 xs
//     res

// let ofArrayWithTail (xs: 'T[]) (tail: 'T list) =
//     let mutable res = tail
//     for i = xs.Length - 1 downto 0 do
//         res <- cons xs.[i] res
//     res

// let ofArray (xs: 'T[]) =
//     ofArrayWithTail xs (empty())
