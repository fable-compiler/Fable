namespace FSharp.Collections

open System.Collections.Generic

module HashIdentity =
    let FromFunctions<'T> hasher equals : IEqualityComparer<'T> =
        EqualityComparer<'T>.Create(equals, hasher)

    let Structural<'T when 'T: equality> : IEqualityComparer<'T> =
        EqualityComparer<'T>
            .Create(LanguagePrimitives.GenericEquality, LanguagePrimitives.GenericHash)

    let Reference<'T when 'T: not struct> : IEqualityComparer<'T> =
        EqualityComparer<'T>
            .Create(LanguagePrimitives.PhysicalEquality, LanguagePrimitives.PhysicalHash)

module ComparisonIdentity =
    let FromFunction<'T> comparer : IComparer<'T> = Comparer<'T>.Create(comparer)

    let Structural<'T when 'T: comparison> : IComparer<'T> =
        Comparer<'T>.Create(LanguagePrimitives.GenericComparison)
