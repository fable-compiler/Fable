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

    let inputSequenceTooLong =
        "The input sequence contains more than one element."

    let keyNotFoundAlt =
        "An index satisfying the predicate was not found in the collection."

    let differentLengths = "The collections had different lengths."

    let notEnoughElements =
        "The input sequence has an insufficient number of elements."
