namespace FSharp.Collections

open System.Collections.Generic

module HashIdentity =
    let FromFunctions hash eq : IEqualityComparer<'T> =
        { new IEqualityComparer<'T> with
            member _.Equals(x, y) = eq x y
            member _.GetHashCode(x) = hash x
        }

    let Structural<'T when 'T: equality> : IEqualityComparer<'T> =
        FromFunctions
            LanguagePrimitives.GenericHash
            LanguagePrimitives.GenericEquality

    let Reference<'T when 'T: not struct> : IEqualityComparer<'T> =
        FromFunctions
            LanguagePrimitives.PhysicalHash
            LanguagePrimitives.PhysicalEquality

module ComparisonIdentity =
    let FromFunction comparer : IComparer<'T> =
        { new IComparer<'T> with
            member _.Compare(x, y) = comparer x y
        }

    let Structural<'T when 'T: comparison> : IComparer<'T> =
        FromFunction LanguagePrimitives.GenericComparison
