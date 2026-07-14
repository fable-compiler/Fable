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
#endif
