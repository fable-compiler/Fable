namespace System

open Global_

type Array() = class end

type Attribute() = class end

type Enum() = class end

[<AllowNullLiteral>]
type Exception(message: string, innerException: Exception) =
    new() = Exception("", null)
    new(message) = Exception(message, null)
    member _.Message = message
    member _.StackTrace = ""
    member _.InnerException = innerException

    interface System.Collections.IStructuralEquatable with
        member x.Equals(y, comparer) = false
        member x.GetHashCode(comparer) = 0

type SystemException(message: string) =
    inherit Exception(message, null)
    new() = SystemException(SR.Arg_SystemException)

type ApplicationException(message: string) =
    inherit Exception(message, null)
    new() = ApplicationException(SR.Arg_ApplicationException)

type ArithmeticException(message: string) =
    inherit Exception(message, null)
    new() = ArithmeticException(SR.Arg_ArithmeticException)

type DivideByZeroException(message: string) =
    inherit Exception(message, null)
    new() = DivideByZeroException(SR.Arg_DivideByZero)

type FormatException(message: string) =
    inherit Exception(message, null)
    new() = FormatException(SR.Arg_FormatException)

type IndexOutOfRangeException(message: string) =
    inherit Exception(message, null)
    new() = IndexOutOfRangeException(SR.Arg_IndexOutOfRangeException)

type InvalidOperationException(message: string) =
    inherit Exception(message, null)
    new() = InvalidOperationException(SR.Arg_InvalidOperationException)

type NotFiniteNumberException(message: string) =
    inherit Exception(message, null)
    new() = NotFiniteNumberException(SR.Arg_NotFiniteNumberException)

type NotImplementedException(message: string) =
    inherit Exception(message, null)
    new() = NotImplementedException(SR.Arg_NotImplementedException)

type NotSupportedException(message: string) =
    inherit Exception(message, null)
    new() = NotSupportedException(SR.Arg_NotSupportedException)

type NullReferenceException(message: string) =
    inherit Exception(message, null)
    new() = NullReferenceException(SR.Arg_NullReferenceException)

type OutOfMemoryException(message: string) =
    inherit Exception(message, null)
    new() = OutOfMemoryException(SR.Arg_OutOfMemoryException)

type OverflowException(message: string) =
    inherit Exception(message, null)
    new() = OverflowException(SR.Arg_OverflowException)

type RankException(message: string) =
    inherit Exception(message, null)
    new() = RankException(SR.Arg_RankException)

type StackOverflowException(message: string) =
    inherit Exception(message, null)
    new() = StackOverflowException(SR.Arg_StackOverflowException)

type TimeoutException(message: string) =
    inherit Exception(message, null)
    new() = TimeoutException(SR.Arg_TimeoutException)

type ArgumentException(message: string, paramName: string, innerException: Exception) =
    inherit Exception(message, innerException)

    new() = ArgumentException(SR.Arg_ArgumentException, "", null)
    new(message) = ArgumentException(message, "", null)
    new(message: string, paramName: string) = ArgumentException(message, paramName, null)
    new(message: string, innerException: Exception) = ArgumentException(message, "", innerException)

    member _.Message =
        if System.String.IsNullOrEmpty(paramName) then
            message
        else
            message + SR.Arg_ParamName_Name + paramName + "')"

    member _.ParamName = paramName

type ArgumentNullException(paramName: string, message: string) =
    inherit ArgumentException(message, paramName, null)
    new(paramName) = ArgumentNullException(paramName, SR.ArgumentNull_Generic)
    new() = ArgumentNullException("")

type ArgumentOutOfRangeException(paramName: string, message: string) =
    inherit ArgumentException(message, paramName, null)
    new(paramName) = ArgumentOutOfRangeException(paramName, SR.Arg_ArgumentOutOfRangeException)
    new() = ArgumentOutOfRangeException("")

type LazyState<'T> =
    | Initial of (unit -> 'T)
    | Success of 'T
    | Failure of exn

type Lazy<'T>(state: LazyState<'T>, isThreadSafe: bool) =
    let mutable lazyState = state

    new(f: System.Func<'T>) = Lazy<'T>(Initial(fun () -> f.Invoke()), true)

    new(f: System.Func<'T>, isThreadSafe: bool) = Lazy<'T>(Initial(fun () -> f.Invoke()), isThreadSafe)

    member _.IsValueCreated =
        match lazyState with
        | Success _ -> true
        | _ -> false

    member _.Force() : 'T =
        match lazyState with
        | Success v -> v
        | Failure e -> raise e
        | Initial f ->
            // Monitor.Enter x
            // try
            try
                let v = f ()
                lazyState <- Success v
                v
            with e ->
                lazyState <- Failure e
                reraise ()
    // finally
    //     Monitor.Exit x

    member this.Value = this.Force()
