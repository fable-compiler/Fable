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
