module Fable.Tests.ModuleMutableTwo

open Util.Testing

// A module-level mutable with the same name (`shared`) as the one in
// Fable.Tests.ModuleMutableOne. On the BEAM target both are stored in the Erlang
// process dictionary; before keys were namespaced by module they shared the bare key
// `shared` and overwrote each other when both modules' state was used in the same
// process (as the test runner does). The fix namespaces the key by module name.

let mutable shared = 0

let setShared (v: int) = shared <- v

let getShared () = shared

let ``test module-level mutables with the same name don't collide across modules`` () =
    // Write distinct values through each module's own mutable, then read them back.
    // If the process-dict keys collided, the second write would clobber the first.
    ModuleMutableOne.setShared 111
    setShared 222
    equal 111 (ModuleMutableOne.getShared ())
    equal 222 (getShared ())
    // Reassign in the opposite order to make sure either module can win last.
    setShared 333
    ModuleMutableOne.setShared 444
    equal 444 (ModuleMutableOne.getShared ())
    equal 333 (getShared ())
