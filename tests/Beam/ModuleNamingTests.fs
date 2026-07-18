module Fable.Tests.ModuleNaming

open Fable.Tests.Util
open Util.Testing

// Erlang's module namespace is flat and global, so the Beam backend qualifies every generated
// module with the app it belongs to. These tests call across file boundaries into modules whose
// file names would otherwise produce a colliding module name — a wrong module atom shows up as
// an `undef` at runtime, not as a compile error.

// --- Names that collide with OTP stdlib modules ---

[<Fact>]
let ``test Module named Gen does not resolve to OTP gen`` () =
    Naming.Gen.delay (fun () -> 41) |> equal 42
    Naming.Gen.constant "x" () |> equal "x"

[<Fact>]
let ``test Module named Random does not resolve to OTP random`` () =
    Naming.Random.next 1 |> equal 1103527590
    Naming.Random.range 0 10 1 |> equal 0

[<Fact>]
let ``test Module named String does not resolve to OTP string`` () =
    Naming.String.reverse "abc" |> equal "cba"
    Naming.String.repeat 3 "ab" |> equal "ababab"

// --- Same file name in two directories of the same assembly ---

[<Fact>]
let ``test Same-named files in different directories both survive`` () =
    Naming.First.Types.area (Naming.First.Types.Circle 2.0) |> equal 12.0
    Naming.Second.Types.name Naming.Second.Types.Red |> equal "red"
