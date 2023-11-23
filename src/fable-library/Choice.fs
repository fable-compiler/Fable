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
    let bind
        (binder: 'a -> Result<'b, 'c>)
        (result: Result<'a, 'c>)
        : Result<'b, 'c>
        =
        match result with
        | Error e -> Error e
        | Ok x -> binder x

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
    let makeChoice1Of2 (x: 'T1) : Choice<'T1, 'a> = Choice1Of2 x

    let makeChoice2Of2 (x: 'T2) : Choice<'a, 'T2> = Choice2Of2 x

    let tryValueIfChoice1Of2 (x: Choice<'T1, 'T2>) : Option<'T1> =
        match x with
        | Choice1Of2 x -> Some x
        | _ -> None

    let tryValueIfChoice2Of2 (x: Choice<'T1, 'T2>) : Option<'T2> =
        match x with
        | Choice2Of2 x -> Some x
        | _ -> None
