module Global_

[<RequireQualifiedAccess>]
module SR =
    let arrayWasEmpty = "The input array was empty."

    let arrayIndexOutOfBounds =
        "The index was outside the range of elements in the array."

    let arraysHadDifferentLengths = "The arrays have different lengths."
    let enumerationAlreadyFinished = "Enumeration already finished."
    let enumerationNotStarted = "Enumeration has not started. Call MoveNext."

    let indexOutOfBounds =
        "The index was outside the range of elements in the list."

    let inputListWasEmpty = "The input list was empty."
    let inputMustBeNonNegative = "The input must be non-negative."
    let inputMustBePositive = "The input must be positive."
    let inputSequenceEmpty = "The input sequence was empty."

    let inputSequenceTooLong =
        "The input sequence contains more than one element."

    let keyNotFound = "The item, key, or index was not found in the collection."

    let keyNotFoundAlt =
        "An index satisfying the predicate was not found in the collection."

    let listsHadDifferentLengths = "The lists had different lengths."
    let mapCannotBeMutated = "Map values cannot be mutated."
    let notAPermutation = "The function did not compute a permutation."

    let notEnoughElements =
        "The input sequence has an insufficient number of elements."

    let outOfRange = "The index is outside the legal range."
    let resetNotSupported = "Reset is not supported on this enumerator."
    let setContainsNoElements = "Set contains no elements."

[<AutoOpen>]
module Helpers =
    open Fable.Core

#if FABLE_COMPILER_RUST
    // ResizeArray<T> intentionally has same representation as Array<T> in Rust
    // so can be casted to array instead of using .ToArray() which makes a copy
    [<Emit("$0")>]
    let inline internal asArray (a: ResizeArray<'T>) : 'T[] = nativeOnly
#else
    let inline internal asArray (a: ResizeArray<'T>) : 'T[] = a.ToArray()
#endif

// type IObject =
//     abstract to_any: unit -> obj

// type IException =
//     inherit IObject
//     abstract Message: string
