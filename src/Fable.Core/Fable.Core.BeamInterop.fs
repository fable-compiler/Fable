module Fable.Core.BeamInterop

open Fable.Core

/// Destructure a tuple of arguments and apply to literal Erlang code as with EmitAttribute.
/// E.g. `emitErlExpr (arg1, arg2) "$0 + $1"` in Erlang becomes `arg1 + arg2`
let emitErlExpr<'T> (args: obj) (erlCode: string) : 'T = nativeOnly

/// Same as emitErlExpr but intended for Erlang code that must appear in a statement position.
let emitErlStatement<'T> (args: obj) (erlCode: string) : 'T = nativeOnly

/// Works like `ImportAttribute` (same semantics as ES6 imports).
/// You can use "*" or "default" selectors.
let import<'T> (selector: string) (path: string) : 'T = nativeOnly

/// F#: let myMember = importMember<string> "my_module"
/// Erl: my_module:my_member()
/// Note the import must be immediately assigned to a value in a let binding
let importMember<'T> (path: string) : 'T = nativeOnly

/// F#: let myLib = importAll<obj> "my_module"
/// Imports all exports from an Erlang module
let importAll<'T> (path: string) : 'T = nativeOnly

module Erlang =
    /// Selective receive with timeout. Returns Some(msg) on match, None on timeout.
    /// Each DU case maps to a receive clause with the case's CompiledName (or snake_case name)
    /// as the Erlang atom tag.
    let receive<'T> (timeoutMs: int) : 'T option = nativeOnly
    /// Blocking selective receive (no timeout). Blocks until a matching message arrives.
    let receiveForever<'T> () : 'T = nativeOnly
