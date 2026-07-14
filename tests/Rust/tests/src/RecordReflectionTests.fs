// Value-based reflection needs the type registry, which is std-only
// (it uses thread_local), so these tests do not apply to no_std builds.
[<Fable.Core.Rust.OuterAttr("cfg", [| "not(feature = \"no_std\")" |])>]
module Fable.Tests.RecordReflectionTests

open Util.Testing
open Microsoft.FSharp.Reflection

// Record reflection MVP for the Rust target. Mirrors the record-focused cases of
// the (disabled) ReflectionTests.fs / JS Main ReflectionTests.fs. Value comparisons
// are done after unbox<'T> rather than on boxed `obj` directly, because Rust has no
// structural equality on `dyn Any` (see report). Behaviour is otherwise equivalent.

type TestRecord = { String: string; Int: int }
type OtherRecord = { Z: int }

type SampleUnion =
    | CaseA
    | CaseB of int

[<Fact>]
let ``typeof and System.Type equality on records`` () =
    (typeof<TestRecord> = typeof<TestRecord>) |> equal true
    (typeof<TestRecord> = typeof<OtherRecord>) |> equal false

[<Fact>]
let ``typeof and System.Type equality on non-record types`` () =
    // A non-record `typeof` must materialize as a valid runtime value (a boxed
    // TypeId), and equality must distinguish types rather than always being equal.
    let t = typeof<int>
    ignore t
    (typeof<int> = typeof<int>) |> equal true
    (typeof<int> = typeof<string>) |> equal false
    (typeof<SampleUnion> = typeof<SampleUnion>) |> equal true
    (typeof<SampleUnion> = typeof<int>) |> equal false
    // Mixed record / non-record must not compare equal.
    (typeof<TestRecord> = typeof<int>) |> equal false

[<Fact>]
let ``FSharpType.IsRecord works`` () =
    FSharpType.IsRecord(typeof<TestRecord>) |> equal true

[<Fact>]
let ``FSharpType.GetRecordFields exposes PropertyInfo names`` () =
    let fields = FSharpType.GetRecordFields(typeof<TestRecord>)
    fields.Length |> equal 2
    fields.[0].Name |> equal "String"
    fields.[1].Name |> equal "Int"

[<Fact>]
let ``FSharpValue.GetRecordFields returns boxed field values`` () =
    // Value-based reflection resolves the type via the runtime registry, which is
    // populated when typeof<T> is evaluated (as the JS record test also does first).
    FSharpType.IsRecord(typeof<TestRecord>) |> equal true
    let record = { String = "a"; Int = 1 }
    let values = FSharpValue.GetRecordFields(record)
    values.Length |> equal 2
    unbox<string> values.[0] |> equal "a"
    unbox<int> values.[1] |> equal 1

[<Fact>]
let ``FSharpValue.GetRecordField reads an individual field`` () =
    let record = { String = "a"; Int = 1 }
    let fields = FSharpType.GetRecordFields(typeof<TestRecord>)
    unbox<string> (FSharpValue.GetRecordField(record, fields.[0])) |> equal "a"
    unbox<int> (FSharpValue.GetRecordField(record, fields.[1])) |> equal 1

[<Fact>]
let ``PropertyInfo.GetValue reads a field from a boxed record`` () =
    let record = { String = "a"; Int = 1 }
    let fields = FSharpType.GetRecordFields(typeof<TestRecord>)
    unbox<string> (fields.[0].GetValue(box record)) |> equal "a"
    unbox<int> (fields.[1].GetValue(box record)) |> equal 1

[<Fact>]
let ``FSharpValue.MakeRecord round-trips`` () =
    let record = { String = "a"; Int = 1 }
    let values = FSharpValue.GetRecordFields(record)
    let rebuilt = unbox<TestRecord> (FSharpValue.MakeRecord(typeof<TestRecord>, values))
    rebuilt |> equal record

[<Fact>]
let ``FSharp.Reflection: Record (end to end)`` () =
    let record = { String = "a"; Int = 1 }
    let recordTypeFields = FSharpType.GetRecordFields(typeof<TestRecord>)
    let recordValueFields = FSharpValue.GetRecordFields(record)

    FSharpType.IsRecord(typeof<TestRecord>) |> equal true
    recordTypeFields.Length |> equal 2

    // field names line up with values (verified per-slot, unboxed)
    recordTypeFields.[0].Name |> equal "String"
    unbox<string> recordValueFields.[0] |> equal "a"
    recordTypeFields.[1].Name |> equal "Int"
    unbox<int> recordValueFields.[1] |> equal 1

    // GetRecordField matches GetRecordFields per slot
    unbox<string> (FSharpValue.GetRecordField(record, recordTypeFields.[0])) |> equal "a"
    unbox<int> (FSharpValue.GetRecordField(record, recordTypeFields.[1])) |> equal 1

    // MakeRecord reconstructs an equal record
    unbox<TestRecord> (FSharpValue.MakeRecord(typeof<TestRecord>, recordValueFields)) |> equal record

[<Fact>]
let ``obj.GetType round-trips to the concrete record type`` () =
    let record = { String = "a"; Int = 1 }
    FSharpType.IsRecord(typeof<TestRecord>) |> equal true // ensure registration
    ((box record).GetType() = typeof<TestRecord>) |> equal true
    let rebuilt =
        unbox<TestRecord> (FSharpValue.MakeRecord((box record).GetType(), FSharpValue.GetRecordFields(record)))
    rebuilt |> equal record
