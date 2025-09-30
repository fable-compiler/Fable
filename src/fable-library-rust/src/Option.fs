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

let foldBack<'T, 'S> (folder: 'T -> 'S -> 'S) (opt: 'T option) (state: 'S) : 'S =
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

let iterate<'T> (action: 'T -> unit) (opt: 'T option) : unit =
    match opt with
    | Some x -> action x
    | None -> ()

let map<'T, 'U> (mapping: 'T -> 'U) (opt: 'T option) : 'U option =
    match opt with
    | Some x -> Some(mapping x)
    | None -> None

let map2<'T1, 'T2, 'U> (mapping: 'T1 -> 'T2 -> 'U) (opt1: 'T1 option) (opt2: 'T2 option) : 'U option =
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

let orElseWith<'T> (ifNoneThunk: unit -> 'T option) (opt: 'T option) : 'T option =
    match opt with
    | Some _ -> opt
    | None -> ifNoneThunk ()

let toArray (opt: 'T option) =
    match opt with
    | None -> [||]
    | Some x -> [| x |]

let toList (opt: 'T option) =
    match opt with
    | None -> []
    | Some x -> [ x ]

let toNullable (opt: 'T option) =
    match opt with
    | None -> System.Nullable()
    | Some v -> System.Nullable(v)

let ofNullable (value: System.Nullable<'T>) =
    if value.HasValue then
        Some(value.Value)
    else
        None

// let ofObj (value: 'T) =
//     match box value with
//     | null -> None
//     | _ -> Some value

// let toObj (opt: 'T option) =
//     match opt with
//     | None -> null
//     | Some x -> box x

let ofOption (option: 'T option) =
    match option with
    | None -> ValueNone
    | Some x -> ValueSome x

let toOption (voption: 'T voption) =
    match voption with
    | ValueNone -> None
    | ValueSome x -> Some x

let ofValueOption (voption: 'T voption) =
    match voption with
    | ValueNone -> None
    | ValueSome x -> Some x

let toValueOption (option: 'T option) =
    match option with
    | None -> ValueNone
    | Some x -> ValueSome x
