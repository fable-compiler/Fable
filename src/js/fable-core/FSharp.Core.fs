namespace FSharp.Core

module LanguagePrimitives =
    let FastGenericComparer<'T when 'T : comparison> =
        FSharp.Collections.ComparisonIdentity.Structural<'T>

module Operators =
    let Failure message = new System.Exception(message)

    [<CompiledName("FailurePattern")>]
    let (|Failure|_|) (exn: exn) = Some exn.Message
        //if exn.GetType().FullName.EndsWith("Exception") then Some exn.Message else None

    [<CompiledName("NullArg")>]
    let nullArg x = raise(System.ArgumentNullException(x))

    [<CompiledName("Lock")>]
    let lock _lockObj action = action() // no locking, just invoke

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printf =

    [<CompiledName("PrintFormatToStringBuilderThen")>]
    let kbprintf continuation (builder: System.Text.StringBuilder) format =
        let append (s:string) = builder.Append(s) |> ignore; continuation()
        Printf.kprintf append format

    [<CompiledName("PrintFormatToStringBuilder")>]
    let bprintf builder format = kbprintf ignore builder format
