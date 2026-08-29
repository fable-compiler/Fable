/// Used to compile to `random.erl`, shadowing OTP's deprecated `random` stdlib module. rebar3
/// reported the clash only as a deprecation warning on our own function.
module Fable.Tests.Naming.Random

let next (seed: int) = (seed * 1103515245 + 12345) % 2147483647

let range (lo: int) (hi: int) (seed: int) = lo + (abs (next seed) % (hi - lo))
