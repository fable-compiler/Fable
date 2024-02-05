namespace FSharp.Core

[<CompiledName("FSharpResult`2")>]
type Result<'T, 'TError> =
    | Ok of ResultValue: 'T
    | Error of ErrorValue: 'TError

module Result =
    [<CompiledName("Map")>]
    let map mapping result =
        match result with
        | Error e -> Error e
        | Ok x -> Ok(mapping x)

    [<CompiledName("MapError")>]
    let mapError mapping result =
        match result with
        | Error e -> Error(mapping e)
        | Ok x -> Ok x

    [<CompiledName("Bind")>]
    let bind binder result =
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
    let fold<'a, 'b, 's>
        (folder: 's -> 'a -> 's)
        (state: 's)
        (result: Result<'a, 'b>)
        : 's
        =
        match result with
        | Error _ -> state
        | Ok x -> folder state x

    [<CompiledName("FoldBack")>]
    let foldBack<'a, 'b, 's>
        (folder: 'a -> 's -> 's)
        (result: Result<'a, 'b>)
        (state: 's)
        : 's
        =
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

[<CompiledName("FSharpChoice`2")>]
type Choice<'T1, 'T2> =
    | Choice1Of2 of 'T1
    | Choice2Of2 of 'T2

[<CompiledName("FSharpChoice`3")>]
type Choice<'T1, 'T2, 'T3> =
    | Choice1Of3 of 'T1
    | Choice2Of3 of 'T2
    | Choice3Of3 of 'T3

[<CompiledName("FSharpChoice`4")>]
type Choice<'T1, 'T2, 'T3, 'T4> =
    | Choice1Of4 of 'T1
    | Choice2Of4 of 'T2
    | Choice3Of4 of 'T3
    | Choice4Of4 of 'T4

[<CompiledName("FSharpChoice`5")>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5> =
    | Choice1Of5 of 'T1
    | Choice2Of5 of 'T2
    | Choice3Of5 of 'T3
    | Choice4Of5 of 'T4
    | Choice5Of5 of 'T5

[<CompiledName("FSharpChoice`6")>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> =
    | Choice1Of6 of 'T1
    | Choice2Of6 of 'T2
    | Choice3Of6 of 'T3
    | Choice4Of6 of 'T4
    | Choice5Of6 of 'T5
    | Choice6Of6 of 'T6

[<CompiledName("FSharpChoice`7")>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> =
    | Choice1Of7 of 'T1
    | Choice2Of7 of 'T2
    | Choice3Of7 of 'T3
    | Choice4Of7 of 'T4
    | Choice5Of7 of 'T5
    | Choice6Of7 of 'T6
    | Choice7Of7 of 'T7

module Choice =
    let makeChoice1Of2 (x: 'T1) = Choice1Of2 x

    let makeChoice2Of2 (x: 'T2) = Choice2Of2 x

    let tryValueIfChoice1Of2 (x: Choice<'T1, 'T2>) : Option<'T1> =
        match x with
        | Choice1Of2 x -> Some x
        | _ -> None

    let tryValueIfChoice2Of2 (x: Choice<'T1, 'T2>) : Option<'T2> =
        match x with
        | Choice2Of2 x -> Some x
        | _ -> None
