namespace Fable.Core

type IGenericAdder<'T> =
    abstract GetZero: unit -> 'T
    abstract Add: 'T * 'T -> 'T

type IGenericAverager<'T> =
    abstract GetZero: unit -> 'T
    abstract Add: 'T * 'T -> 'T
    abstract DivideByInt: 'T * int -> 'T

type Symbol_wellknown =
    abstract ``Symbol.toStringTag``: string

type IJsonSerializable =
    abstract toJSON: unit -> obj

namespace global

[<RequireQualifiedAccess>]
module SR =
    let indexOutOfBounds =
        "The index was outside the range of elements in the collection."

    let inputWasEmpty = "Collection was empty."
    let inputMustBeNonNegative = "The input must be non-negative."
    let inputSequenceEmpty = "The input sequence was empty."

    let inputSequenceTooLong = "The input sequence contains more than one element."

    let keyNotFoundAlt =
        "An index satisfying the predicate was not found in the collection."

    let differentLengths = "The collections had different lengths."

    let notEnoughElements = "The input sequence has an insufficient number of elements."

    let Arg_ApplicationException = "Error in the application."
    let Arg_ArgumentException = "Value does not fall within the expected range."

    let Arg_ArgumentOutOfRangeException =
        "Specified argument was out of the range of valid values."

    let ArgumentNull_Generic = "Value cannot be null."
    let Arg_ParamName_Name = " (Parameter '"
    let Arg_ArithmeticException = "Overflow or underflow in the arithmetic operation."
    let Arg_DivideByZero = "Attempted to divide by zero."
    let Arg_FormatException = "One of the identified items was in an invalid format."
    let Arg_IndexOutOfRangeException = "Index was outside the bounds of the array."

    let Arg_InvalidOperationException =
        "Operation is not valid due to the current state of the object."

    let Arg_KeyNotFound = "The given key was not present in the dictionary."
    let Arg_NotFiniteNumberException = "Number encountered was not a finite quantity."
    let Arg_NotImplementedException = "The method or operation is not implemented."
    let Arg_NotSupportedException = "Specified method is not supported."

    let Arg_NullReferenceException =
        "Object reference not set to an instance of an object."

    let Arg_OutOfMemoryException =
        "Insufficient memory to continue the execution of the program."

    let Arg_OverflowException = "Arithmetic operation resulted in an overflow."

    let Arg_RankException =
        "Attempted to operate on an array with the incorrect number of dimensions."

    let Arg_StackOverflowException = "Operation caused a stack overflow."
    let Arg_SystemException = "System error."
    let Arg_TimeoutException = "The operation has timed out."
