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
