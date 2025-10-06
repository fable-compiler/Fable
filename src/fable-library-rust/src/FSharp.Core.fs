namespace FSharp.Core

module LanguagePrimitives =

    // let GenericEquality<'T> (x: 'T) (y: 'T) : bool =
    //     System.Collections.Generic.EqualityComparer<'T>.Default.Equals(x, y)

    // let GenericEqualityIntrinsic<'T> (x: 'T) (y: 'T) : bool =
    //     System.Collections.Generic.EqualityComparer<'T>.Default.Equals(x, y)

    // let GenericComparison<'T> (x: 'T) (y: 'T) : int =
    //     System.Collections.Generic.Comparer<'T>.Default.Compare(x, y)

    // let GenericComparisonIntrinsic<'T> (x: 'T) (y: 'T) : int =
    //     System.Collections.Generic.Comparer<'T>.Default.Compare(x, y)

    let GenericEqualityComparer<'T when 'T: equality> =
        FSharp.Collections.HashIdentity.Structural<'T>

    let GenericEqualityERComparer<'T when 'T: equality> =
        FSharp.Collections.HashIdentity.Structural<'T>

    let FastGenericComparer<'T when 'T: comparison> =
        FSharp.Collections.ComparisonIdentity.Structural<'T>

    let FastGenericComparerFromTable<'T when 'T: comparison> =
        FSharp.Collections.ComparisonIdentity.Structural<'T>

    let FastGenericEqualityComparer<'T when 'T: equality> =
        FSharp.Collections.HashIdentity.Structural<'T>

    let FastGenericEqualityComparerFromTable<'T when 'T: equality> =
        FSharp.Collections.HashIdentity.Structural<'T>

module Operators =
    let Failure message = new System.Exception(message)

    [<CompiledName("FailurePattern")>]
    let (|Failure|_|) (exn: exn) = Some exn.Message

    [<CompiledName("NullArg")>]
    let nullArg (argumentName: string) =
        raise (System.ArgumentNullException(argumentName))

    [<CompiledName("Using")>]
    let using<'T, 'R when 'T :> System.IDisposable and 'T: null> (resource: 'T) (action: 'T -> 'R) =
        try
            action resource
        finally
            match resource with
            | null -> ()
            | _ -> resource.Dispose()

    [<CompiledName("Lock")>]
    let lock _lockObj action = action () // no locking, just invoke

    [<CompiledName("IsNull")>]
    let isNull (value: 'T when 'T: null) =
        match value with
        | null -> true
        | _ -> false

    [<CompiledName("IsNotNull")>]
    let isNotNull (value: 'T when 'T: null) =
        match value with
        | null -> false
        | _ -> true

    [<CompiledName("IsNullV")>]
    let isNullV (value: System.Nullable<'T>) = not value.HasValue

    [<CompiledName("NonNull")>]
    let nonNull (value: 'T when 'T: null) =
        match value with
        | null -> raise (System.NullReferenceException())
        | _ -> value

    [<CompiledName("NonNullV")>]
    let nonNullV (value: System.Nullable<'T>) =
        if value.HasValue then
            value.Value
        else
            raise (System.NullReferenceException())

    [<CompiledName("NullMatchPattern")>]
    let (|Null|NonNull|) (value: 'T when 'T: null) =
        match value with
        | null -> Null()
        | _ -> NonNull(value)

    [<CompiledName("NullValueMatchPattern")>]
    let (|NullV|NonNullV|) (value: System.Nullable<'T>) =
        if value.HasValue then
            NonNullV(value.Value)
        else
            NullV()

    [<CompiledName("NonNullQuickPattern")>]
    let (|NonNullQuick|) (value: 'T when 'T: null) =
        match value with
        | null -> raise (System.NullReferenceException())
        | _ -> value

    [<CompiledName("NonNullQuickValuePattern")>]
    let (|NonNullQuickV|) (value: System.Nullable<'T>) =
        if value.HasValue then
            value.Value
        else
            raise (System.NullReferenceException())

    [<CompiledName("WithNull")>]
    let withNull (value: 'T when 'T: null) = value

    [<CompiledName("WithNullV")>]
    let withNullV (value: 'T) : System.Nullable<'T> = System.Nullable<'T>(value)

    [<CompiledName("NullV")>]
    let nullV<'T when 'T: struct and 'T: (new: unit -> 'T) and 'T :> System.ValueType> =
        System.Nullable<'T>()

    [<CompiledName("NullArgCheck")>]
    let nullArgCheck (argumentName: string) (value: 'T when 'T: null) =
        match value with
        | null -> raise (System.ArgumentNullException(argumentName))
        | _ -> value

module ExtraTopLevelOperators =
    [<CompiledName("LazyPattern")>]
    let (|Lazy|) (input: Lazy<_>) = input.Force()
