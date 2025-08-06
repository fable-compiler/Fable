namespace FSharp.Core

#nowarn "42" // This construct is deprecated: it is only for use in the F# library

module LanguagePrimitives =

    let GenericEqualityComparer =
        { new System.Collections.IEqualityComparer with
            override __.Equals(x: objnull, y: objnull) = LanguagePrimitives.GenericEquality x y

            override __.GetHashCode(x: obj) = LanguagePrimitives.GenericHash x
        }

    let GenericEqualityERComparer =
        { new System.Collections.IEqualityComparer with
            override __.Equals(x: objnull, y: objnull) =
                LanguagePrimitives.GenericEqualityER x y

            override __.GetHashCode(x: obj) = LanguagePrimitives.GenericHash x
        }

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
    let isNull (value: 'T when 'T: null) =
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
    let nonNull (value: 'T | null when 'T: not null and 'T: not struct) =
        match box value with
        | null -> raise (System.NullReferenceException())
        | _ -> (# "" value : 'T #)

    [<CompiledName("NonNullV")>]
    let nonNullV (value: System.Nullable<'T>) =
        if value.HasValue then
            value.Value
        else
            raise (System.NullReferenceException())

    [<CompiledName("NullMatchPattern")>]
    let (|Null|NonNull|) (value: 'T | null when 'T: not null and 'T: not struct) =
        match value with
        | null -> Null()
        | _ -> NonNull (# "" value : 'T #)

    [<CompiledName("NullValueMatchPattern")>]
    let (|NullV|NonNullV|) (value: System.Nullable<'T>) =
        if value.HasValue then
            NonNullV value.Value
        else
            NullV()

    [<CompiledName("NonNullQuickPattern")>]
    let (|NonNullQuick|) (value: 'T | null when 'T: not null and 'T: not struct) =
        match box value with
        | null -> raise (System.NullReferenceException())
        | _ -> (# "" value : 'T #)

    [<CompiledName("NonNullQuickValuePattern")>]
    let (|NonNullQuickV|) (value: System.Nullable<'T>) =
        if value.HasValue then
            value.Value
        else
            raise (System.NullReferenceException())

    [<CompiledName("WithNull")>]
    let withNull (value: 'T when 'T: not null and 'T: not struct) = (# "" value : 'T | null #)

    [<CompiledName("WithNullV")>]
    let withNullV (value: 'T) : System.Nullable<'T> = System.Nullable<'T>(value)

    [<CompiledName("NullV")>]
    let nullV<'T when 'T: struct and 'T: (new: unit -> 'T) and 'T :> System.ValueType> =
        System.Nullable<'T>()

    [<CompiledName("NullArgCheck")>]
    let nullArgCheck (argumentName: string) (value: 'T | null when 'T: not null and 'T: not struct) =
        match value with
        | null -> raise (new System.ArgumentNullException($"Value cannot be null. (Parameter '{argumentName}')"))
        | _ -> (# "" value : 'T #)

module ExtraTopLevelOperators =
    [<CompiledName("LazyPattern")>]
    let (|Lazy|) (input: Lazy<_>) = input.Force()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printf =

    [<CompiledName("PrintFormatToStringBuilderThen")>]
    let kbprintf continuation (builder: System.Text.StringBuilder) format =
        let append (s: string) =
            builder.Append(s) |> ignore
            continuation ()

        Printf.kprintf append format

    [<CompiledName("PrintFormatToStringBuilder")>]
    let bprintf builder format = kbprintf ignore builder format
