module Fable.Tests.CollidingMemberNames

type Foo() = class end

let fooBar () = 1
let foo_bar () = 2
let foo_ctor () = 42

let values () = fooBar (), foo_bar (), foo_ctor ()

// Members differing only in case are legal F# but collapse to one Erlang atom. Unlike a captured
// `let`, another file can call them, so they must be reported rather than silently renamed.
type Renderer() =
    member _.work() = 1
    member _.Work() = 2

let render () =
    let r = Renderer()
    r.work (), r.Work()

// Class and `val` fields are keyed by `classFieldAtomName`, which collapses case and runs of
// underscores. Two fields that differ only there share one map key.
type CtorFields(fooBar: int, FooBar: int) =
    member _.Lower = fooBar
    member _.Upper = FooBar

type ValFields() =
    [<DefaultValue>]
    val mutable fooBar: int

    [<DefaultValue>]
    val mutable FooBar: int

// Union tags and interface dispatch keys are atoms too. A shared tag makes `match` pick the wrong
// branch and two values compare equal; a shared key makes one member resolve to the other's body.
type CollidingUnion =
    | FooBar
    | Foo_Bar

type CompiledNameUnion =
    | [<CompiledName("foo_bar")>] Tagged
    | Foo_Bar

type ICollidingMembers =
    abstract FooBar: unit -> int
    abstract Foo_Bar: unit -> int
