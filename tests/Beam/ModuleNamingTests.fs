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

// --- Names longer than Erlang's 255-character atom limit ---

// Both names are 263 characters and share their first 260, so the truncated prefix alone
// cannot tell them apart; an overlong atom fails in `erlc`, a merged one calls the wrong body.
let private overlongfunctionnamexxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxone () = 1

let private overlongfunctionnamexxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxtwo () = 2

[<Fact>]
let ``test Names over the atom length limit stay valid and distinct`` () =
    overlongfunctionnamexxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxone () |> equal 1
    overlongfunctionnamexxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxtwo () |> equal 2

// The atom limiter caps each sanitized name at 255, so anything joined onto a capped name
// (`<record>_reflection`, `<class>_<property>`) overflows the limit again unless re-capped.
type Overlongrecordnamerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrend = { Value: int }

type Overlongclassnamecccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccend(value: int) =
    member _.Overlongpropertynameppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppend = value

[<Fact>]
let ``test Atoms composed from capped names stay within the limit`` () =
    let record = { Overlongrecordnamerrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrend.Value = 1 }
    record.Value |> equal 1
    Overlongclassnamecccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccend(2).Overlongpropertynameppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppend |> equal 2
