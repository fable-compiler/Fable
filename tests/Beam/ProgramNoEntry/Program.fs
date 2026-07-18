/// The other kind of Fable program: no `[<EntryPoint>]`, just top-level effects, which compile to
/// the entry module's `main/0`. The generated shim has to fall back to that arity — this is the
/// branch a program with an entry point never takes.
module Fable.Tests.ProgramNoEntry.Main

printfn "no-entry-point program ran"

type private Pair = { A: int; B: int }

// Each top-level effect compiles to its own main/0, and those are merged into a single Erlang
// clause — so temporaries belonging to separate effects end up sharing one scope. Erlang variables
// are single-assignment, so a repeated name is a pattern match against the earlier binding rather
// than a rebind. These two deliberately disagree: while every effect bound the same value the match
// succeeded by coincidence and the bug stayed invisible.
printfn "effect2=%b" ({ A = 1; B = 2 } = { A = 1; B = 2 })
printfn "effect3=%b" ({ A = 1; B = 2 } = { A = 9; B = 9 })
