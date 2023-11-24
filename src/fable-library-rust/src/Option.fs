module Option_

let bind<'T, 'U> (binder: 'T -> 'U option) (opt: 'T option) : 'U option =
    match opt with
    | Some x -> binder x
    | None -> None

let contains<'T when 'T: equality> (value: 'T) (opt: 'T option) : bool =
    match opt with
    | Some x -> x = value
    | None -> false

let count<'T> (opt: 'T option) : int =
    match opt with
    | Some _ -> 1
    | None -> 0

let defaultArg<'T> (opt: 'T option) (defaultValue: 'T) : 'T =
    match opt with
    | Some x -> x
    | None -> defaultValue

let defaultValue<'T> (defaultValue: 'T) (opt: 'T option) : 'T =
    match opt with
    | Some x -> x
    | None -> defaultValue

let defaultWith<'T> (defThunk: unit -> 'T) (opt: 'T option) : 'T =
    match opt with
    | Some x -> x
    | None -> defThunk ()

let exists<'T> (predicate: 'T -> bool) (opt: 'T option) : bool =
    match opt with
    | Some x -> predicate x
    | None -> false

let filter<'T> (predicate: 'T -> bool) (opt: 'T option) : 'T option =
    match opt with
    | Some x ->
        if predicate x then
            opt
        else
            None
    | None -> None

let flatten<'T> (opt: 'T option option) : 'T option =
    match opt with
    | Some x -> x
    | None -> None

let fold<'T, 'S> (folder: 'S -> 'T -> 'S) (state: 'S) (opt: 'T option) : 'S =
    match opt with
    | Some x -> folder state x
    | None -> state

let foldBack<'T, 'S>
    (folder: 'T -> 'S -> 'S)
    (opt: 'T option)
    (state: 'S)
    : 'S
    =
    match opt with
    | Some x -> folder x state
    | None -> state

let forAll<'T> (predicate: 'T -> bool) (opt: 'T option) : bool =
    match opt with
    | Some x -> predicate x
    | None -> true

let getValue<'T> (opt: 'T option) : 'T =
    match opt with
    | Some x -> x
    | None -> failwith "Option has no value"

let iterate<'T, 'U> (action: 'T -> unit) (opt: 'T option) : unit =
    match opt with
    | Some x -> action x
    | None -> ()

let map<'T, 'U> (mapping: 'T -> 'U) (opt: 'T option) : 'U option =
    match opt with
    | Some x -> Some(mapping x)
    | None -> None

let map2<'T1, 'T2, 'U>
    (mapping: 'T1 -> 'T2 -> 'U)
    (opt1: 'T1 option)
    (opt2: 'T2 option)
    : 'U option
    =
    match opt1 with
    | None -> None
    | Some x ->
        match opt2 with
        | None -> None
        | Some y -> Some(mapping x y)

let map3<'T1, 'T2, 'T3, 'U>
    (mapping: 'T1 -> 'T2 -> 'T3 -> 'U)
    (opt1: 'T1 option)
    (opt2: 'T2 option)
    (opt3: 'T3 option)
    : 'U option
    =
    match opt1 with
    | None -> None
    | Some x ->
        match opt2 with
        | None -> None
        | Some y ->
            match opt3 with
            | None -> None
            | Some z -> Some(mapping x y z)

let orElse<'T> (ifNone: 'T option) (opt: 'T option) : 'T option =
    match opt with
    | Some _ -> opt
    | None -> ifNone

let orElseWith<'T>
    (ifNoneThunk: unit -> 'T option)
    (opt: 'T option)
    : 'T option
    =
    match opt with
    | Some _ -> opt
    | None -> ifNoneThunk ()

// moved to Array.ofOption to avoid dependency
// let toArray<'T> (opt: 'T option): 'T[] = Array.ofOption

// moved to List.ofOption to avoid dependency
// let toList<'T> (opt: 'T option): 'T list = List.ofOption

// let ofNullable<'T>(x: 'T): 'T option =
//     if x != null then Some(x) else None

// let ofObj<'T>(x: 'T): 'T option =
//     if x != null then Some(x) else None

// let toNullable<'T> (opt: 'T option): 'T =
//     match opt with
//     | Some x -> x
//     | None -> null //defaultOf('T)

// let toObj<'T> (opt: 'T option): 'T =
//     match opt with
//     | Some x -> x
//     | None -> null //defaultOf('T)
