namespace Fable.Core

// Common types with Fable.Core

open System

type CaseRules =
    | None = 0
    /// FooBar -> fooBar
    | LowerFirst = 1
    /// FooBar -> foo_bar
    | SnakeCase = 2
    /// FooBar -> FOO_BAR
    | SnakeCaseAllCaps = 3
    /// FooBar -> foo-bar
    | KebabCase = 4

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
