/// The file name is what the Beam backend used to name the generated Erlang module after, so
/// this file used to compile to `gen.erl` — and `gen` is an OTP stdlib module, which won every
/// lookup. Calls into this module hit OTP's `gen` instead and failed at runtime with `undef`.
module Fable.Tests.Naming.Gen

let delay (f: unit -> int) = f () + 1

let constant x = fun () -> x
