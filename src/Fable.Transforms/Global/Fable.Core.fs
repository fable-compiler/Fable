namespace Fable.Core

// Common types with Fable.Core

open System

type CaseRules =
    | None = 0
    | LowerFirst = 1
    | SnakeCase = 2

[<AttributeUsage(AttributeTargets.Class)>]
type StringEnumAttribute() =
    inherit Attribute()
    new (caseRules: CaseRules) = StringEnumAttribute()

type EraseAttribute() =
    inherit Attribute()

type [<Erase>] U2<'a, 'b> =
    | Case1 of 'a
    | Case2 of 'b

type [<Erase>] U3<'a, 'b, 'c> =
    | Case1 of 'a
    | Case2 of 'b
    | Case3 of 'c
