module Fable.Core.Dart

open System

type IsConstAttribute() =
    inherit Attribute()

type DartNullable<'T>() =
    new (value: 'T) = DartNullable()
    member _.HasValue: bool = nativeOnly
    member _.Value: 'T = nativeOnly

module DartNullable =
    let toOption (x: DartNullable<'T>): 'T option = nativeOnly
    let ofOption (x: 'T option): DartNullable<'T> = nativeOnly
    let toNullable (x: DartNullable<'T>): Nullable<'T> = nativeOnly
    let ofNullable (x: 'T Nullable): DartNullable<'T> = nativeOnly

[<Global>]
type Future<'T> =
    interface end

[<Global>]
type Stream<'T> =
    interface end

[<Global>]
let print(item: obj): unit = nativeOnly

/// Destructure a tuple of arguments and apply them to literal code as with EmitAttribute.
/// E.g. `emitExpr (arg1, arg2) "$0 + $1"` becomes `arg1 + arg2`
let emitExpr<'T> (args: obj) (code: string): 'T = nativeOnly

/// Same as emitExpr but intended for code that must appear in statement position
/// (so it can contain `return`, `break`, loops, etc)
/// E.g. `emitStatement aValue "while($0 < 5) { doSomething() }"`
let emitStatement<'T> (args: obj) (code: string): 'T = nativeOnly

/// Works like `ImportAttribute` (same semantics as Dart imports).
/// You can use "*" selector.
let import<'T> (selector: string) (path: string):'T = nativeOnly

/// Must be immediately assigned to a value in a let binding.
/// Imports a member from the external module with same name as value in binding.
let importMember<'T> (path: string):'T = nativeOnly

/// Imports a whole external module.
let importAll<'T> (path: string):'T = nativeOnly