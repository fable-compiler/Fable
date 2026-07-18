namespace System

type SystemException(message: string) =
    inherit Exception(message)
    new() = SystemException(SR.Arg_SystemException)

type ApplicationException(message: string) =
    inherit Exception(message)
    new() = ApplicationException(SR.Arg_ApplicationException)

type ArithmeticException(message: string) =
    inherit Exception(message)
    new() = ArithmeticException(SR.Arg_ArithmeticException)

type DivideByZeroException(message: string) =
    inherit Exception(message)
    new() = DivideByZeroException(SR.Arg_DivideByZero)

type FormatException(message: string) =
    inherit Exception(message)
    new() = FormatException(SR.Arg_FormatException)

type IndexOutOfRangeException(message: string) =
    inherit Exception(message)
    new() = IndexOutOfRangeException(SR.Arg_IndexOutOfRangeException)

type InvalidOperationException(message: string) =
    inherit Exception(message)
    new() = InvalidOperationException(SR.Arg_InvalidOperationException)

type NotFiniteNumberException(message: string) =
    inherit Exception(message)
    new() = NotFiniteNumberException(SR.Arg_NotFiniteNumberException)

type NotImplementedException(message: string) =
    inherit Exception(message)
    new() = NotImplementedException(SR.Arg_NotImplementedException)

type NotSupportedException(message: string) =
    inherit Exception(message)
    new() = NotSupportedException(SR.Arg_NotSupportedException)

type NullReferenceException(message: string) =
    inherit Exception(message)
    new() = NullReferenceException(SR.Arg_NullReferenceException)

type OutOfMemoryException(message: string) =
    inherit Exception(message)
    new() = OutOfMemoryException(SR.Arg_OutOfMemoryException)

type OverflowException(message: string) =
    inherit Exception(message)
    new() = OverflowException(SR.Arg_OverflowException)

type RankException(message: string) =
    inherit Exception(message)
    new() = RankException(SR.Arg_RankException)

type StackOverflowException(message: string) =
    inherit Exception(message)
    new() = StackOverflowException(SR.Arg_StackOverflowException)

type TimeoutException(message: string) =
    inherit Exception(message)
    new() = TimeoutException(SR.Arg_TimeoutException)

type ArgumentException(message: string, paramName: string, innerException: exn | null) =
    inherit
        Exception(
            (if System.String.IsNullOrEmpty(paramName) then
                 message
             else
                 message + SR.Arg_ParamName_Name + paramName + "')"),
            innerException
        )

    // Use Unchecked.defaultof rather than a `null` literal for the absent inner
    // exception: Fable erases nullable reference annotations, so a `null` literal
    // would be emitted against a non-nullable parameter type and fail type checking.
    new() = ArgumentException(SR.Arg_ArgumentException, "", Unchecked.defaultof<exn>)
    new(message) = ArgumentException(message, "", Unchecked.defaultof<exn>)
    new(message: string, paramName: string) = ArgumentException(message, paramName, Unchecked.defaultof<exn>)
    new(message: string, innerException: exn | null) = ArgumentException(message, "", innerException)
    member _.ParamName = paramName

type ArgumentNullException(paramName: string, message: string) =
    inherit ArgumentException(message, paramName, Unchecked.defaultof<exn>)
    new(paramName) = ArgumentNullException(paramName, SR.ArgumentNull_Generic)
    new() = ArgumentNullException("")

type ArgumentOutOfRangeException(paramName: string, message: string) =
    inherit ArgumentException(message, paramName, Unchecked.defaultof<exn>)
    new(paramName) = ArgumentOutOfRangeException(paramName, SR.Arg_ArgumentOutOfRangeException)
    new() = ArgumentOutOfRangeException("")
