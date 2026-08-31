/// The derived name for this file would be `fable_tests_beam_naming_pinned`. The attribute pins it
/// to an exact atom instead — what a module implementing an OTP behaviour, or one called from
/// hand-written Erlang, needs.
[<Fable.Core.Beam.ModuleName("fable_tests_pinned")>]
module Fable.Tests.Naming.Pinned

let value = 42

let double (x: int) = x * 2
