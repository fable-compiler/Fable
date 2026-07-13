/// The other kind of Fable program: no `[<EntryPoint>]`, just top-level effects, which compile to
/// the entry module's `main/0`. The generated shim has to fall back to that arity — this is the
/// branch a program with an entry point never takes.
module Fable.Tests.ProgramNoEntry.Main

printfn "no-entry-point program ran"
