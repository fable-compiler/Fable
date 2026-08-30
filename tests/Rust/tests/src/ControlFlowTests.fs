module Fable.Tests.ControlFlowTests

open Util.Testing

[<Fact>]
let ``if_then_else works`` () =
    let r = if true then 4 else 6
    r |> equal 4
    let r = if false then 4 else 6
    r |> equal 6

let bfn x (a: int) (b: int) =
    if x then a else b

[<Fact>]
let ``if_then_else works II`` () =
    bfn true 1 2 |> equal 1
    bfn false 3 4 |> equal 4

[<Fact>]
let ``Nested if_then_else works`` () =
    let a x =
        if x = 1 then
            1
        else if x = 2 then
            2
        else 3
    a 1 |> equal 1
    a 2 |> equal 2
    a 3 |> equal 3

[<Fact>]
let ``Curried apply works`` () =
    let mul a b = a * b
    let mul2 x = mul 2 x
    // TODO: let mul2 = mul 2 // partial apply calls util.PartialApply which is not yet working (module loading)
    mul 3 2 |> equal 6
    mul2 4 |> equal 8

[<Fact>]
let ``for_in loop works`` () =
    let mutable a = 42
    for i in 0..5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``for_to loop works`` () =
    let mutable a = 42
    for i = 0 to 5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``for_downto loop works`` () =
    let mutable a = 42
    for i = 5 downto 0 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``for_in descending range with -1 step works`` () =
    let mutable a = 42
    for i in 5 .. -1 .. 0 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``for_in ascending range with 1 step works`` () =
    let mutable a = 42
    for i in 0 .. 1 .. 5 do
        a <- i + a
    a |> equal 57

[<Fact>]
let ``for_in const-step range with zero iterations works`` () =
    let mutable count = 0
    for _i in 0 .. -1 .. 5 do
        count <- count + 1
    count |> equal 0

[<Fact>]
let ``for_in step range other than one still works`` () =
    let mutable a = 0
    for i in 9 .. -2 .. 0 do
        a <- a + i
    a |> equal 25

[<Fact>]
let ``while loop works`` () =
    let mutable i = 0
    let mutable total = 0
    while i < 10 do
        i <- i + 1
        total <- total + i
    total |> equal 55

// --- Regression tests for the Rust decision-tree/switch backend ---

type private OptRec = { Field: int option }

let private (|Positive|_|) x =
    if x > 0 then Some() else None

// Bug 2: bare-wildcard option match where the scrutinee is NOT a plain ident
// (field access / active-pattern result) used to emit `0_i32` int patterns
// against a native Option<T>, a type error.
let private optFieldNoBind (r: OptRec) =
    match r.Field with
    | Some _ -> "some"
    | None -> "none"

let private optApNoBind n =
    match n with
    | Positive _ -> "pos"
    | _ -> "other"

[<Fact>]
let ``option wildcard match on field access works`` () =
    optFieldNoBind { Field = Some 3 } |> equal "some"
    optFieldNoBind { Field = None } |> equal "none"

[<Fact>]
let ``option wildcard match on active pattern works`` () =
    optApNoBind 5 |> equal "pos"
    optApNoBind -1 |> equal "other"

let private optFieldBind (r: OptRec) =
    match r.Field with
    | Some v -> v * 10
    | None -> 0

[<Fact>]
let ``option binding match on field access works`` () =
    optFieldBind { Field = Some 4 } |> equal 40
    optFieldBind { Field = None } |> equal 0

// Bug 1: reference-typed bindings reaching the two-switch decision-tree path
// used to be pre-declared via getZero (mem::zeroed), which panics on Rc pointers.

type private RefC(v: int) =
    member _.V = v

// when-guard causes a shared default target -> two-switch path, ref binding `c`
let private twoSwitchRef (x: RefC option) (b: bool) =
    match x with
    | Some c when b -> c.V + 1
    | Some c -> c.V
    | None -> -1

[<Fact>]
let ``two-switch with reference binding works`` () =
    twoSwitchRef (Some(RefC 5)) true |> equal 6
    twoSwitchRef (Some(RefC 5)) false |> equal 5
    twoSwitchRef None false |> equal -1

type private RefU =
    | RA of RefC
    | RB of RefC

// or-pattern shares one target with a binding -> two-switch path
let private orPatRef (u: RefU) =
    match u with
    | RA c
    | RB c -> c.V

[<Fact>]
let ``two-switch or-pattern with reference binding works`` () =
    orPatRef (RA(RefC 5)) |> equal 5
    orPatRef (RB(RefC 7)) |> equal 7

type private ObjU =
    | OA of obj
    | OB of obj

let private orPatObj (u: ObjU) : obj =
    match u with
    | OA o
    | OB o -> o

[<Fact>]
let ``two-switch or-pattern with obj binding works`` () =
    // exercises the getZeroObj placeholder for a `dyn Any` binding on the two-switch path
    orPatObj (OA(box 9)) :? int |> equal true
    orPatObj (OB(box 11)) :? string |> equal false

// An active pattern in the first clause of a match F# cannot prove exhaustive.
// F# emits `raise (MatchFailureException(...))` for the fallthrough, and the
// rewrite that normally removes it only fires when the decision expression
// starts with a UnionCaseTest — an active pattern there is not one, so the
// construction survived into codegen and crashed the compiler with
// "The lists had different lengths" and no source location.
type private ApExpr =
    | ApColumn of string * string
    | ApConst of string

let private (|IsApConst|_|) e =
    match e with
    | ApConst c -> Some c
    | _ -> None

let private (|IsApColumn|_|) e =
    match e with
    | ApColumn(t, n) -> Some(t, n)
    | _ -> None

let private renderUnionAfterAp e =
    match e with
    | IsApConst c -> c
    | ApColumn(t, n) -> t + "." + n

let private renderApOnly e =
    match e with
    | IsApConst c -> c
    | IsApColumn(t, n) -> t + "." + n

[<Fact>]
let ``non-exhaustive match starting with an active pattern works`` () =
    renderUnionAfterAp (ApColumn("c", "x")) |> equal "c.x"
    renderUnionAfterAp (ApConst "lit") |> equal "lit"
    renderApOnly (ApColumn("c", "x")) |> equal "c.x"
    renderApOnly (ApConst "lit") |> equal "lit"

// A fieldless union reached through a record field rather than a plain ident.
// The switch conversion recovers the union type from the ident; without one it
// used to fall back to the tag's int32 and emit a bare integer literal pattern
// against the union value, which does not compile.
type JoinKind =
    | InnerJoin
    | LeftJoin

type Join = { Kind: JoinKind; Table: string }

let private renderJoinKind (j: Join) =
    let mutable out = ""

    match j.Kind with
    | InnerJoin -> out <- "INNER"
    | LeftJoin -> out <- "LEFT"

    out

[<Fact>]
let ``fieldless union matched through a record field works`` () =
    renderJoinKind { Kind = InnerJoin; Table = "t" } |> equal "INNER"
    renderJoinKind { Kind = LeftJoin; Table = "t" } |> equal "LEFT"

// A guard on a nested pattern makes the whole match take the decision-tree
// path, which pre-declares a mutable binding for every variable in the match
// and initialises it before testing anything. Strings, options, arrays and
// lists are heap-backed, and zero-initialising one is a runtime panic rather
// than a compile error.
type Payload =
    | Number of int
    | Numbers of int[]
    | Names of string list
    | Maybe of int option
    | Label of string

let private describePayload (p: Payload) (flag: bool) =
    match p with
    | Maybe(Some v) when flag -> "guarded " + string v
    | Maybe(Some v) -> "some " + string v
    | Maybe None -> "none"
    | Number n -> "number " + string n
    | Numbers xs -> "numbers " + string xs.Length
    | Names ns -> "names " + string (List.length ns)
    | Label s -> "label " + s

[<Fact>]
let ``a guarded nested pattern does not zero-initialise the other bindings`` () =
    describePayload (Maybe(Some 1)) true |> equal "guarded 1"
    describePayload (Maybe(Some 2)) false |> equal "some 2"
    describePayload (Maybe None) false |> equal "none"
    describePayload (Number 3) false |> equal "number 3"
    describePayload (Numbers [| 1; 2; 3 |]) false |> equal "numbers 3"
    describePayload (Names [ "a"; "b" ]) false |> equal "names 2"
    describePayload (Label "hi") false |> equal "label hi"

// A try/with/finally lowers to closures on this target, but the Fable AST has no
// Lambda there, so the capture walk did not treat those bodies as closure
// contexts. A `let mutable` assigned in a finally block was emitted as a bare
// MutCell and the closure mutated a clone, so the write was silently lost.
//
// The exception-free case runs everywhere; the ones below need a `with` that
// actually catches, which no_std's try_catch does not do.
[<Fact>]
let ``a mutable assigned in a finally block keeps its value`` () =
    let mutable ran = false

    let message =
        try
            "body"
        finally
            ran <- true

    ran |> equal true
    message |> equal "body"

#if !NO_STD_NO_EXCEPTIONS
[<Fact>]
let ``a mutable assigned in a finally block survives an exception`` () =
    let mutable ran = false

    let message =
        try
            try
                failwith "inner"
            finally
                ran <- true
        with ex ->
            ex.Message

    ran |> equal true
    message |> equal "inner"

[<Fact>]
let ``a mutable assigned in a with handler keeps its value`` () =
    let mutable ran = false

    try
        failwith "x"
    with _ ->
        ran <- true

    ran |> equal true
#endif
