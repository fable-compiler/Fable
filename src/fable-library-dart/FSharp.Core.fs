namespace FSharp.Core

[<CompiledName("Lazy")>]
type Lazy<'T> =
    abstract Force: unit -> 'T

module Operators =

    //    let Failure message = new System.Exception(message)
    //
    //    [<CompiledName("FailurePattern")>]
    //    let (|Failure|_|) (exn: exn) = Some exn.Message
    //        //if exn.GetType().FullName.EndsWith("Exception") then Some exn.Message else None
    //
    //    [<CompiledName("NullArg")>]
    //    let nullArg x = raise(System.ArgumentNullException(x))

    [<CompiledName("Using")>]
    let using<'T, 'U when 'T :> System.IDisposable>
        (resource: 'T)
        (action: 'T -> 'U)
        : 'U
        =
        try
            action (resource)
        finally
            resource.Dispose()

    [<CompiledName("Lock")>]
    let lock (_lockObj: 'a) (action: unit -> 'b) : 'b = action () // no locking, just invoke

module ExtraTopLevelOperators =
    [<CompiledName("LazyPattern")>]
    let (|Lazy|) (input: Lazy<_>) = input.Force()
