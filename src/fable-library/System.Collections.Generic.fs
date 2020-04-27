namespace System.Collections.Generic

type Comparer<'T when 'T : comparison>() =
    static member Default =
        { new IComparer<'T> with
            member __.Compare(x, y) = LanguagePrimitives.GenericComparison x y }
    interface IComparer<'T> with
        member __.Compare(x, y) = LanguagePrimitives.GenericComparison x y

type EqualityComparer<'T when 'T : equality>() =
    static member Default =
        { new IEqualityComparer<'T> with
            member __.Equals(x, y) = LanguagePrimitives.GenericEquality x y
            member __.GetHashCode(x) = LanguagePrimitives.GenericHash x }
    interface IEqualityComparer<'T> with
        member __.Equals(x, y) = LanguagePrimitives.GenericEquality x y
        member __.GetHashCode(x) = LanguagePrimitives.GenericHash x
