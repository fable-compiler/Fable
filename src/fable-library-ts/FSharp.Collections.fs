namespace FSharp.Collections

open System.Collections.Generic

module HashIdentity =
    let FromFunctions<'T> hasher equals : IEqualityComparer<'T> =
        { new IEqualityComparer<'T> with
            member _.GetHashCode(x) = hasher x
            member _.Equals(x, y) = equals x y
        }

    let Structural<'T when 'T: equality> : IEqualityComparer<'T> =
        { new IEqualityComparer<'T> with
            member _.GetHashCode(x) = LanguagePrimitives.GenericHash x
            member _.Equals(x, y) = LanguagePrimitives.GenericEquality x y
        }

    let Reference<'T when 'T: not struct> : IEqualityComparer<'T> =
        { new IEqualityComparer<'T> with
            member _.GetHashCode(x) = LanguagePrimitives.PhysicalHash x
            member _.Equals(x, y) = LanguagePrimitives.PhysicalEquality x y
        }

module ComparisonIdentity =
    let FromFunction<'T> comparer : IComparer<'T> =
        { new IComparer<'T> with
            member _.Compare(x, y) = comparer x y
        }

    let Structural<'T when 'T: comparison> : IComparer<'T> =
        { new IComparer<'T> with
            member _.Compare(x, y) =
                LanguagePrimitives.GenericComparison x y
        }
