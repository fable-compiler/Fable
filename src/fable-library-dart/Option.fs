module OptionModule

open System

let defaultValue (def: 'T) (opt: 'T option) : 'T =
    match opt with
    | None -> def
    | Some opt -> opt

let defaultWith (fn: unit -> 'T) (opt: 'T option) : 'T =
    match opt with
    | None -> fn ()
    | Some opt -> opt

let orElse (def: 'T option) (opt: 'T option) : 'T option =
    match opt with
    | None -> def
    | Some opt -> Some opt

let orElseWith (fn: unit -> 'T option) (opt: 'T option) : 'T option =
    match opt with
    | None -> fn ()
    | Some opt -> Some opt

let toArray (opt: 'T option) : 'T[] =
    match opt with
    | None -> [||]
    | Some opt -> [| opt |]

let toList (opt: 'T option) : 'T list =
    match opt with
    | None -> []
    | Some opt -> [ opt ]

let count (opt: 'T option) : int =
    match opt with
    | Some _ -> 1
    | None -> 0

let contains (value: 'T) (opt: 'T option) : bool =
    match opt with
    | Some value2 -> value = value2
    | None -> false

let iterate (fn: 'T -> unit) (opt: 'T option) : unit =
    match opt with
    | Some opt -> fn opt
    | None -> ()

let forAll (fn: 'T -> bool) (opt: 'T option) : bool =
    match opt with
    | Some opt -> fn opt
    | None -> true

let exists (fn: 'T -> bool) (opt: 'T option) : bool =
    match opt with
    | Some opt -> fn opt
    | None -> false

let fold<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (opt: 'T option)
    : 'State
    =
    match opt with
    | Some opt -> folder state opt
    | None -> state

let foldBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (opt: 'T option)
    (state: 'State)
    : 'State
    =
    match opt with
    | Some opt -> folder opt state
    | None -> state

let filter (fn: 'T -> bool) (opt: 'T option) : 'T option =
    match opt with
    | None -> None
    | Some opt ->
        if fn opt then
            Some opt
        else
            None

let flatten<'T> (opt: 'T option option) : 'T option =
    match opt with
    | Some x -> x
    | None -> None

let map (fn: 'T -> 'U) (opt: 'T option) : 'U option =
    match opt with
    | None -> None
    | Some opt -> fn opt |> Some

let map2
    (fn: 'T1 -> 'T2 -> 'U)
    (opt1: 'T1 option)
    (opt2: 'T2 option)
    : 'U option
    =
    match opt1, opt2 with
    | Some opt1, Some opt2 -> fn opt1 opt2 |> Some
    | _ -> None

let map3
    (fn: 'T1 -> 'T2 -> 'T3 -> 'U)
    (opt1: 'T1 option)
    (opt2: 'T2 option)
    (opt3: 'T3 option)
    : 'U option
    =
    match opt1, opt2, opt3 with
    | Some opt1, Some opt2, Some opt3 -> fn opt1 opt2 opt3 |> Some
    | _ -> None

let bind (fn: 'T -> 'U option) (opt: 'T option) : 'U option =
    match opt with
    | None -> None
    | Some opt -> fn opt

let ofNullable (x: Nullable<'T>) : 'T option =
    if x.HasValue then
        Some(x.Value)
    else
        None

let toNullable (opt: 'T option) : Nullable<'T> =
    match opt with
    | Some x -> Nullable x
    | None -> Nullable()
