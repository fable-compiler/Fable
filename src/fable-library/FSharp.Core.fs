namespace FSharp.Core

module LanguagePrimitives =
    let GenericEqualityComparer =
        { new System.Collections.IEqualityComparer with
            override __.Equals(x: obj, y: obj) =
                LanguagePrimitives.GenericEquality x y

            override __.GetHashCode(x: obj) = LanguagePrimitives.GenericHash x
        }

    let GenericEqualityERComparer =
        { new System.Collections.IEqualityComparer with
            override __.Equals(x: obj, y: obj) =
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
    let using<'T, 'R when 'T :> System.IDisposable>
        (resource: 'T)
        (action: 'T -> 'R)
        =
        try
            action (resource)
        finally
            match (box resource) with
            | null -> ()
            | _ -> resource.Dispose()

    [<CompiledName("Lock")>]
    let lock _lockObj action = action () // no locking, just invoke

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
