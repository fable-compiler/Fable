module Fable.Core.Dart

open System
open Fable.Core

type IsConstAttribute() =
    inherit Attribute()

type DartNullable<'T>() =
    new(value: 'T) = DartNullable()
    member _.HasValue: bool = nativeOnly
    member _.Value: 'T = nativeOnly

module DartNullable =
    let defaultValue (defVal: 'T) (value: DartNullable<'T>) : 'T = nativeOnly

    let defaultWith (defThunk: unit -> 'T) (value: DartNullable<'T>) : 'T =
        nativeOnly

    let toOption (value: DartNullable<'T>) : 'T option = nativeOnly
    let ofOption (value: 'T option) : DartNullable<'T> = nativeOnly
    let toNullable (value: DartNullable<'T>) : Nullable<'T> = nativeOnly
    let ofNullable (value: 'T Nullable) : DartNullable<'T> = nativeOnly

[<ImportMember "dart:async">]
type Future<'T> =
    interface
    end

[<ImportMember "dart:async">]
type Stream<'T> =
    interface
    end

// [<ImportMember "dart:core">]
[<Global>]
let print (item: obj) : unit = nativeOnly

/// Destructure a tuple of arguments and apply them to literal code as with EmitAttribute.
/// E.g. `emitExpr (arg1, arg2) "$0 + $1"` becomes `arg1 + arg2`
let emitExpr<'T> (args: obj) (code: string) : 'T = nativeOnly

/// Same as emitExpr but intended for code that must appear in statement position
/// (so it can contain `return`, `break`, loops, etc)
/// E.g. `emitStatement aValue "while($0 < 5) { doSomething() }"`
let emitStatement<'T> (args: obj) (code: string) : 'T = nativeOnly

/// Works like `ImportAttribute` (same semantics as Dart imports).
/// You can use "*" selector.
let import<'T> (selector: string) (path: string) : 'T = nativeOnly

/// Must be immediately assigned to a value in a let binding.
/// Imports a member from the external module with same name as value in binding.
let importMember<'T> (path: string) : 'T = nativeOnly

/// Imports a whole external module.
let importAll<'T> (path: string) : 'T = nativeOnly
