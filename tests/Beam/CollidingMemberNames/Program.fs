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
