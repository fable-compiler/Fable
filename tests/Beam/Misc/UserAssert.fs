module Fable.Tests.UserAssert

// Fixture for the "user function collides with an assertion selector" regression.
//
// Fable2Beam used to rewrite any call site whose *import selector was named* `assertEqual`,
// `assertNotEqual`, `Testing_equal` or `Testing_notEqual` into an inlined assertion. It matched a
// name, not an entity, so these functions' bodies never ran: a call was replaced by an equality
// check that raised `{assert_equal, ...}` when the arguments differed, and returned the `ok` atom
// when they didn't. The four names below are exactly the ones that were intercepted (`equal` and
// `notEqual` are mangled to `Testing_equal`/`Testing_notEqual` by the enclosing module).
//
// They live in their own file on purpose: only *cross-file* calls become imports with a selector,
// so a same-file caller would never have hit the interception in the first place.

let assertEqual (a: int) (b: int) : string = "assertEqual:" + string a + ":" + string b

let assertNotEqual (a: int) (b: int) : string = "assertNotEqual:" + string a + ":" + string b

module Testing =
    let equal (a: int) (b: int) : string = "equal:" + string a + ":" + string b

    let notEqual (a: int) (b: int) : string = "notEqual:" + string a + ":" + string b
