namespace FSharp.Core

[<CompiledName("Lazy")>]
type Lazy<'T> =
    abstract Force: unit -> 'T

module Operators =
    let Failure message = new System.Exception(message)

    [<CompiledName("FailurePattern")>]
    let (|Failure|_|) (exn: exn) = Some exn.Message
    //if exn.GetType().FullName.EndsWith("Exception") then Some exn.Message else None

    [<CompiledName("NullArg")>]
    let nullArg x = raise (System.ArgumentNullException(x))

    [<CompiledName("Using")>]
    let using<'T, 'R when 'T :> System.IDisposable> (resource: 'T) (action: 'T -> 'R) =
        try
            action resource
        finally
            match box resource with
            | null -> ()
            | _ -> resource.Dispose()

    [<CompiledName("Lock")>]
    let lock _lockObj action = action () // no locking, just invoke

    [<CompiledName("IsNull")>]
    let isNull (value: 'T) =
        match box value with
        | null -> true
        | _ -> false

    [<CompiledName("IsNotNull")>]
    let isNotNull (value: 'T) =
        match box value with
        | null -> false
        | _ -> true

    [<CompiledName("IsNullV")>]
    let isNullV (value: System.Nullable<'T>) = not value.HasValue

    [<CompiledName("NonNull")>]
    let nonNull (value: 'T) =
        match box value with
        | null -> raise (System.NullReferenceException())
        | _ -> value

    [<CompiledName("NonNullV")>]
    let nonNullV (value: System.Nullable<'T>) =
        if value.HasValue then
            value.Value
        else
            raise (System.NullReferenceException())

    [<CompiledName("NullMatchPattern")>]
    let (|Null|NonNull|) (value: 'T) =
        match box value with
        | null -> Null()
        | _ -> NonNull value

    [<CompiledName("NullValueMatchPattern")>]
    let (|NullV|NonNullV|) (value: System.Nullable<'T>) =
        if value.HasValue then
            NonNullV value.Value
        else
            NullV()

    [<CompiledName("NonNullQuickPattern")>]
    let (|NonNullQuick|) (value: 'T) =
        match box value with
        | null -> raise (System.NullReferenceException())
        | _ -> value

    [<CompiledName("NonNullQuickValuePattern")>]
    let (|NonNullQuickV|) (value: System.Nullable<'T>) =
        if value.HasValue then
            value.Value
        else
            raise (System.NullReferenceException())

    [<CompiledName("WithNull")>]
    let withNull (value: 'T) : 'T = value

    [<CompiledName("WithNullV")>]
    let withNullV (value: 'T) : System.Nullable<'T> = System.Nullable<'T>(value)

    [<CompiledName("NullV")>]
    let nullV<'T when 'T: struct and 'T: (new: unit -> 'T) and 'T :> System.ValueType> =
        System.Nullable<'T>()

    [<CompiledName("NullArgCheck")>]
    let nullArgCheck (argumentName: string) (value: 'T) =
        match box value with
        | null -> raise (new System.ArgumentNullException(argumentName))
        | _ -> value

module ExtraTopLevelOperators =
    [<CompiledName("LazyPattern")>]
    let (|Lazy|) (input: Lazy<_>) = input.Force()
