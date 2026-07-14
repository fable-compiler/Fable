module Fable.Tests.AssertTests

// The suite's own `equal` (Util.fs) *is* `Assert.AreEqual`, so these tests deliberately avoid it on
// the paths that matter: an assertion that never raises would otherwise make a test of "assertions
// raise" vacuously green, which is precisely the bug being pinned here. Anything that must fail the
// test fails it with `failwith`, which raises on its own.

#if FABLE_COMPILER
open Xunit
open Fable.Core.Testing

/// Returns the message of the exception raised by `f`, or None if it didn't raise.
let private messageOf (f: unit -> unit) =
    try
        f ()
        None
    with e ->
        Some e.Message

[<Fact>]
let ``test Assert.AreEqual raises on mismatch`` () =
    match messageOf (fun () -> Assert.AreEqual(2, 1)) with
    | None -> failwith "Assert.AreEqual did not raise on a mismatch"
    | Some msg ->
        if msg <> "Expected: 1 - Actual: 2" then
            failwith ("Unexpected assertion message: " + msg)

[<Fact>]
let ``test Assert.AreEqual does not raise when equal`` () =
    match messageOf (fun () -> Assert.AreEqual(1, 1)) with
    | Some msg -> failwith ("Assert.AreEqual raised on equal values: " + msg)
    | None -> ()

[<Fact>]
let ``test Assert.AreEqual raises on structural mismatch`` () =
    match messageOf (fun () -> Assert.AreEqual([ 1; 2 ], [ 1; 3 ])) with
    | None -> failwith "Assert.AreEqual did not raise on a structural mismatch"
    | Some _ -> ()

[<Fact>]
let ``test Assert.AreEqual does not raise on structural equality`` () =
    match messageOf (fun () -> Assert.AreEqual([ 1; 2 ], [ 1; 2 ])) with
    | Some msg -> failwith ("Assert.AreEqual raised on structurally equal values: " + msg)
    | None -> ()

[<Fact>]
let ``test Assert.AreEqual uses the custom message`` () =
    match messageOf (fun () -> Assert.AreEqual(2, 1, "values differ")) with
    | None -> failwith "Assert.AreEqual did not raise on a mismatch"
    | Some msg ->
        if msg <> "values differ" then
            failwith ("Unexpected assertion message: " + msg)

[<Fact>]
let ``test Assert.NotEqual raises when equal`` () =
    match messageOf (fun () -> Assert.NotEqual(1, 1)) with
    | None -> failwith "Assert.NotEqual did not raise on equal values"
    | Some _ -> ()

[<Fact>]
let ``test Assert.NotEqual does not raise on mismatch`` () =
    match messageOf (fun () -> Assert.NotEqual(1, 2)) with
    | Some msg -> failwith ("Assert.NotEqual raised on different values: " + msg)
    | None -> ()

[<Fact>]
let ``test Assert.NotEqual uses the custom message`` () =
    match messageOf (fun () -> Assert.NotEqual(1, 1, "values are equal")) with
    | None -> failwith "Assert.NotEqual did not raise on equal values"
    | Some msg ->
        if msg <> "values are equal" then
            failwith ("Unexpected assertion message: " + msg)

// === User functions named like the old assertion selectors ===
// Fable2Beam used to inline an assertion at any call site whose import selector was named
// `assertEqual`/`assertNotEqual`/`Testing_equal`/`Testing_notEqual`, matching a name rather than an
// entity — so a user's own same-named function never ran its body. Each call below passes differing
// arguments, which the interception would have turned into a *raise*; they must instead return the
// function's own result. See tests/Beam/Misc/UserAssert.fs.

let private returns (expected: string) (f: unit -> string) =
    match (try Ok(f ()) with e -> Error e.Message) with
    | Error msg -> failwith ("User function was rewritten into an assertion, and raised: " + msg)
    | Ok actual ->
        if actual <> expected then
            failwith ("User function did not run its own body, returned: " + actual)

[<Fact>]
let ``test a user function named assertEqual is not rewritten`` () =
    returns "assertEqual:1:2" (fun () -> UserAssert.assertEqual 1 2)

[<Fact>]
let ``test a user function named assertNotEqual is not rewritten`` () =
    returns "assertNotEqual:1:1" (fun () -> UserAssert.assertNotEqual 1 1)

[<Fact>]
let ``test a user function named Testing.equal is not rewritten`` () =
    returns "equal:1:2" (fun () -> UserAssert.Testing.equal 1 2)

[<Fact>]
let ``test a user function named Testing.notEqual is not rewritten`` () =
    returns "notEqual:1:1" (fun () -> UserAssert.Testing.notEqual 1 1)
#endif
