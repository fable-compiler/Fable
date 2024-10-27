namespace System

open Global_

type Array() = class end

type Attribute() = class end

type Enum() = class end

type Exception(message: string) =
    new() = Exception("")

    member _.Message =
        if System.String.IsNullOrEmpty(message) then
            "Specified argument was out of the range of valid values."
        else
            message

    interface System.Collections.IStructuralEquatable with
        member x.Equals(y, comparer) = false
        member x.GetHashCode(comparer) = 0

type InvalidOperationException(message: string) =
    new() = InvalidOperationException("")

    member _.Message =
        if System.String.IsNullOrEmpty(message) then
            "Operation is not valid due to the current state of the object."
        else
            message

type ArgumentException(message: string, paramName: string) =
    new() = ArgumentException("", "")
    new(message) = ArgumentException(message, "")

    member _.Message =
        let message =
            if System.String.IsNullOrEmpty(message) then
                "Value does not fall within the expected range."
            else
                message

        if System.String.IsNullOrEmpty(paramName) then
            message
        else
            message + " (Parameter '" + paramName + "')"

    member _.ParamName = paramName

type ArgumentOutOfRangeException(paramName: string, message: string) =
    new() = ArgumentOutOfRangeException("", "")
    new(paramName) = ArgumentOutOfRangeException(paramName, "")

    member _.Message =
        let message =
            if System.String.IsNullOrEmpty(message) then
                "Specified argument was out of the range of valid values."
            else
                message

        if System.String.IsNullOrEmpty(paramName) then
            message
        else
            message + " (Parameter '" + paramName + "')"

    member _.ParamName = paramName

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
