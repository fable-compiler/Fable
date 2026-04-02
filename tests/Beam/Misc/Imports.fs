module Fable.Tests.Imports

#if FABLE_COMPILER
open Fable.Core
#endif

type IHasAdd =
    abstract Add: x: int -> y: int -> int

type MyClass() =
    static member Mul x y = x * y
    member _.Sub x y = x - y
    interface IHasAdd with
        member _.Add x y = x + y

#if FABLE_COMPILER
/// Cross-module Emit returning unit. When called from another module
/// and piped to ignore, this can trigger the variable shadowing bug.
[<Emit("ok")>]
let emitReturningUnit () : unit = nativeOnly

/// Cross-module Emit returning obj.
[<Emit("ok")>]
let emitReturningValue () : obj = nativeOnly

/// Wrapper around emitReturningValue — tests that a non-Emit wrapper
/// function piped to ignore also doesn't introduce variable bindings.
let wrapperReturningValue () = emitReturningValue ()
#endif
