namespace FSharp.Collections

open System.Collections.Generic

module HashIdentity =
    let FromFunctions hash eq : IEqualityComparer<'T> =
        { new IEqualityComparer<'T> with
            member __.Equals(x, y) = eq x y
            member __.GetHashCode(x) = hash x }
    let Structural<'T when 'T : equality> : IEqualityComparer<'T> =
        FromFunctions LanguagePrimitives.GenericHash LanguagePrimitives.GenericEquality
    let Reference<'T when 'T : not struct > : IEqualityComparer<'T> =
        FromFunctions LanguagePrimitives.PhysicalHash LanguagePrimitives.PhysicalEquality

module ComparisonIdentity = 
    let FromFunction comparer : IComparer<'T> =
        { new IComparer<'T> with
            member __.Compare(x, y) = comparer x y }
    let Structural<'T when 'T : comparison> : IComparer<'T> =
        FromFunction LanguagePrimitives.GenericComparison
