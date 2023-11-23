module Fable.Core.RustInterop

open System

/// Implicit cast for erased unions (U2, U3...)
let inline (!^) (x: ^t1) : ^t2 =
    ((^t1 or ^t2): (static member op_ErasedCast: ^t1 -> ^t2) x)

/// Destructure a tuple of arguments and apply them to literal code as with EmitAttribute.
/// E.g. `emitRustExpr (arg1, arg2) "$0 + $1"` becomes `arg1 + arg2`
let emitRustExpr<'T> (args: obj) (code: string) : 'T = nativeOnly
