module Result_

let map mapping result =
    match result with
    | Ok x -> Ok(mapping x)
    | Error e -> Error e

let mapError mapping result =
    match result with
    | Ok x -> Ok x
    | Error e -> Error(mapping e)

let bind binder result =
    match result with
    | Ok x -> binder x
    | Error e -> Error e

let isOk result =
    match result with
    | Error _ -> false
    | Ok _ -> true

let isError result : bool =
    match result with
    | Error _ -> true
    | Ok _ -> false

let contains value result =
    match result with
    | Error _ -> false
    | Ok x -> x = value

let count result =
    match result with
    | Error _ -> 0
    | Ok _ -> 1

let defaultValue defaultValue result =
    match result with
    | Error _ -> defaultValue
    | Ok x -> x

let defaultWith defThunk result =
    match result with
    | Error e -> defThunk e
    | Ok x -> x

let exists predicate result =
    match result with
    | Error _ -> false
    | Ok x -> predicate x

let fold folder state result =
    match result with
    | Error _ -> state
    | Ok x -> folder state x

let foldBack folder result state =
    match result with
    | Error _ -> state
    | Ok x -> folder x state

let forAll predicate result =
    match result with
    | Error _ -> true
    | Ok x -> predicate x

let iterate action result =
    match result with
    | Error _ -> ()
    | Ok x -> action x

let toArray result =
    match result with
    | Error _ -> [||]
    | Ok x -> [| x |]

let toList result =
    match result with
    | Error _ -> []
    | Ok x -> [ x ]

let toOption result =
    match result with
    | Error _ -> None
    | Ok x -> Some x

let toValueOption result =
    match result with
    | Error _ -> ValueNone
    | Ok x -> ValueSome x
