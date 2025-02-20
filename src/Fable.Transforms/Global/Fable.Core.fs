namespace Fable.Core

open System

// Common types with Fable.Core

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
    /// FooBar -> foobar
    | LowerAll = 5

[<AttributeUsage(AttributeTargets.Class)>]
type StringEnumAttribute() =
    inherit Attribute()
    new(caseRules: CaseRules) = StringEnumAttribute()

[<AttributeUsage(AttributeTargets.Interface)>]
type MangleAttribute() =
    inherit Attribute()
