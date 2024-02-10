namespace FSharp.Core

[<CompiledName("FSharpResult`2")>]
type Result<'T, 'TError> =
    | Ok of ResultValue: 'T
    | Error of ErrorValue: 'TError

module Result =
    [<CompiledName("Map")>]
    let map (mapping: 'a -> 'b) (result: Result<'a, 'c>) : Result<'b, 'c> =
        match result with
        | Error e -> Error e
        | Ok x -> Ok(mapping x)

    [<CompiledName("MapError")>]
    let mapError (mapping: 'a -> 'b) (result: Result<'c, 'a>) : Result<'c, 'b> =
        match result with
        | Error e -> Error(mapping e)
        | Ok x -> Ok x

    [<CompiledName("Bind")>]
    let bind (binder: 'a -> Result<'b, 'c>) (result: Result<'a, 'c>) : Result<'b, 'c> =
        match result with
        | Error e -> Error e
        | Ok x -> binder x

    [<CompiledName("IsOk")>]
    let isOk (result: Result<'a, 'b>) : bool =
        match result with
        | Error _ -> false
        | Ok _ -> true

    [<CompiledName("IsError")>]
    let isError (result: Result<'a, 'b>) : bool =
        match result with
        | Error _ -> true
        | Ok _ -> false

    [<CompiledName("Contains")>]
    let contains (value: 'a) (result: Result<'a, 'b>) : bool =
        match result with
        | Error _ -> false
        | Ok x -> x = value

    [<CompiledName("Count")>]
    let count (result: Result<'a, 'b>) : int =
        match result with
        | Error _ -> 0
        | Ok _ -> 1

    [<CompiledName("DefaultValue")>]
    let defaultValue (defaultValue: 'a) (result: Result<'a, 'b>) : 'a =
        match result with
        | Error _ -> defaultValue
        | Ok x -> x

    [<CompiledName("DefaultWith")>]
    let defaultWith (defThunk: 'b -> 'a) (result: Result<'a, 'b>) : 'a =
        match result with
        | Error e -> defThunk e
        | Ok x -> x

    [<CompiledName("Exists")>]
    let exists (predicate: 'a -> bool) (result: Result<'a, 'b>) : bool =
        match result with
        | Error _ -> false
        | Ok x -> predicate x

    [<CompiledName("Fold")>]
    let fold<'a, 'b, 's> (folder: 's -> 'a -> 's) (state: 's) (result: Result<'a, 'b>) : 's =
        match result with
        | Error _ -> state
        | Ok x -> folder state x

    [<CompiledName("FoldBack")>]
    let foldBack<'a, 'b, 's> (folder: 'a -> 's -> 's) (result: Result<'a, 'b>) (state: 's) : 's =
        match result with
        | Error _ -> state
        | Ok x -> folder x state

    [<CompiledName("ForAll")>]
    let forall (predicate: 'a -> bool) (result: Result<'a, 'b>) : bool =
        match result with
        | Error _ -> true
        | Ok x -> predicate x

    [<CompiledName("Iterate")>]
    let iterate (action: 'a -> unit) (result: Result<'a, 'b>) : unit =
        match result with
        | Error _ -> ()
        | Ok x -> action x

    [<CompiledName("ToArray")>]
    let toArray (result: Result<'a, 'b>) : 'a[] =
        match result with
        | Error _ -> [||]
        | Ok x -> [| x |]

    [<CompiledName("ToList")>]
    let toList (result: Result<'a, 'b>) : 'a list =
        match result with
        | Error _ -> []
        | Ok x -> [ x ]

    [<CompiledName("ToOption")>]
    let toOption (result: Result<'a, 'b>) : Option<'a> =
        match result with
        | Error _ -> None
        | Ok x -> Some x

    [<CompiledName("ToValueOption")>]
    let toValueOption (result: Result<'a, 'b>) : ValueOption<'a> =
        match result with
        | Error _ -> ValueNone
        | Ok x -> ValueSome x
