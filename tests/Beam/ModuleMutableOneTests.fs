module Fable.Tests.ModuleMutableOne

// Companion module for ModuleMutableTwoTests. It declares a module-level mutable with
// the *same* name (`shared`) as the one in Fable.Tests.ModuleMutableTwo. On the BEAM
// target module-level mutables are backed by the (process-local) Erlang process
// dictionary, so their keys must be namespaced by module or the two `shared` values
// collide. The cross-module assertions live in ModuleMutableTwoTests.fs.

let mutable shared = 0

let setShared (v: int) = shared <- v

let getShared () = shared
