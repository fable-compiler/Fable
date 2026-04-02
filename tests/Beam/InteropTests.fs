module Fable.Tests.InteropTests

open System
open Util.Testing

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.BeamInterop

// ============================================================
// Emit tests
// ============================================================

[<Emit("$0 + $1")>]
let addWithEmit (x: int) (y: int) : int = nativeOnly

[<Emit("erlang:length($0)")>]
let listLength (xs: 'a list) : int = nativeOnly

[<Emit("<<$0/binary, $1/binary>>")>]
let concatBinaries (a: string) (b: string) : string = nativeOnly

// ============================================================
// Import tests
// ============================================================

// Function declarations with explicit params
[<Import("add5", "native_code")>]
let add5Imported (x: int) : int = nativeOnly

[<Import("greet", "native_code")>]
let greetImported (name: string) : string = nativeOnly

[<Import("multiply", "native_code")>]
let multiplyImported (x: int) (y: int) : int = nativeOnly

// Value bindings (function type without explicit params)
[<Import("add5", "native_code")>]
let add5Value: int -> int = nativeOnly

[<Import("multiply", "native_code")>]
let multiplyValue: int -> int -> int = nativeOnly

// ============================================================
// ImportAll + Erase interface tests
// ============================================================

[<Erase>]
type INativeCode =
    abstract getName: unit -> string
    abstract addValues: x: int * y: int -> int
    abstract concatStrings: a: string * b: string -> string

[<ImportAll("native_code")>]
let nativeCode: INativeCode = nativeOnly

// ImportAll with Erlang stdlib module (maps)
[<Erase>]
type IMaps =
    abstract from_list: list: obj -> obj
    abstract get: key: obj * map: obj -> obj

[<ImportAll("maps")>]
let erlangMaps: IMaps = nativeOnly

// ============================================================
// Erased union tests
// ============================================================

[<Erase>]
type ErasedUnion =
    | ErasedInt of int
    | ErasedString of string
    member this.SayHi() =
        match this with
        | ErasedInt i -> sprintf "Hi %i time(s)!" i
        | ErasedString s -> sprintf "Hi %s!" s

[<Erase>]
type ErasedUnionWithMultipleFields =
    | ErasedUnionWithMultipleFields of string * int

// ============================================================
// StringEnum tests
// ============================================================

[<StringEnum>]
type MyStrings =
    | Vertical
    | [<CompiledName("Horizontal")>] Horizontal

[<StringEnum(CaseRules.SnakeCase); RequireQualifiedAccess>]
type UserInfo =
    | UserLoginCount

[<StringEnum(CaseRules.KebabCase); RequireQualifiedAccess>]
type MyCssOptions =
    | ContentBox
    | BorderBox

// ============================================================
// Erlang.receive tests
// ============================================================

type RecvMsg =
    | Ping
    | [<CompiledName("custom_tag")>] CustomTag of value: int
    | DataMsg of x: int * y: string
    | [<CompiledName("EXIT")>] Exit of pid: int * reason: string

// Emit with case expression — calling twice in the same function
// would cause "unsafe variable" errors without scope isolation
[<Emit("case $0 of true -> <<\"yes\">>; false -> <<\"no\">> end")>]
let boolToString (x: bool) : string = nativeOnly

// Emit that uses `Value` as a case-clause variable WITHOUT an IIFE wrapper.
// In Erlang's flat scope, if a previous `|> ignore` leaked `Value = ok`,
// then `Value` in the case pattern becomes a bound-variable match (checking
// if the input equals `ok`) instead of a fresh binding, causing case_clause
// errors for any other value.
[<Emit("case $0 of undefined -> <<\"none\">>; Value -> Value end")>]
let emitWithValueCaseVar (x: string option) : string = nativeOnly

#endif

// ============================================================
// Tests
// ============================================================

[<Fact>]
let ``test Emit attribute on function works`` () =
#if FABLE_COMPILER
    addWithEmit 3 4 |> equal 7
#else
    ()
#endif

[<Fact>]
let ``test Emit can call Erlang BIFs`` () =
#if FABLE_COMPILER
    listLength [ 1; 2; 3 ] |> equal 3
#else
    ()
#endif

[<Fact>]
let ``test Emit can use binary syntax`` () =
#if FABLE_COMPILER
    concatBinaries "Hello" "World" |> equal "HelloWorld"
#else
    ()
#endif

[<Fact>]
let ``test emitErlExpr works`` () =
#if FABLE_COMPILER
    let x = 10
    let y = 20
    let result: int = emitErlExpr (x, y) "$0 + $1"
    equal 30 result
#else
    ()
#endif

[<Fact>]
let ``test emitErlExpr works without arguments`` () =
#if FABLE_COMPILER
    let result: int = emitErlExpr () "42"
    equal 42 result
#else
    ()
#endif

[<Fact>]
let ``test emitErlExpr can call Erlang functions`` () =
#if FABLE_COMPILER
    let result: int = emitErlExpr (3, 4) "erlang:max($0, $1)"
    equal 4 result
#else
    ()
#endif

[<Fact>]
let ``test Emit with case expression called twice does not leak variables`` () =
#if FABLE_COMPILER
    // Two calls to the same Emit-with-case in one function would cause
    // Erlang "unsafe variable" errors without IIFE scope isolation
    let a = boolToString true
    let b = boolToString false
    equal "yes" a
    equal "no" b
#else
    ()
#endif

[<Fact>]
let ``test ignore on wrapper function does not shadow Emit variables`` () =
#if FABLE_COMPILER
    // Piping cross-module Emit/wrapper functions to ignore should NOT
    // generate `Value = <call>` which would shadow `Value` in
    // subsequent Emit case clauses.
    Fable.Tests.Imports.emitReturningUnit () |> ignore
    Fable.Tests.Imports.emitReturningValue () |> ignore
    Fable.Tests.Imports.wrapperReturningValue () |> ignore
    let result = emitWithValueCaseVar (Some "hello")
    equal "hello" result
#else
    ()
#endif

[<Fact>]
let ``test Import attribute works`` () =
#if FABLE_COMPILER
    add5Imported 10 |> equal 15
#else
    ()
#endif

[<Fact>]
let ``test Import attribute works for string functions`` () =
#if FABLE_COMPILER
    greetImported "World" |> equal "Hello, World!"
#else
    ()
#endif

[<Fact>]
let ``test Import with multiple arguments works`` () =
#if FABLE_COMPILER
    multiplyImported 6 7 |> equal 42
#else
    ()
#endif

[<Fact>]
let ``test Import value binding works`` () =
#if FABLE_COMPILER
    add5Value 10 |> equal 15
#else
    ()
#endif

[<Fact>]
let ``test Import value binding with multiple arguments works`` () =
#if FABLE_COMPILER
    multiplyValue 6 7 |> equal 42
#else
    ()
#endif

[<Fact>]
let ``test ImportAll with Erase interface calls zero-arg method`` () =
#if FABLE_COMPILER
    nativeCode.getName () |> equal "native_code"
#else
    ()
#endif

[<Fact>]
let ``test ImportAll with Erase interface calls multi-arg method`` () =
#if FABLE_COMPILER
    nativeCode.addValues (3, 4) |> equal 7
#else
    ()
#endif

[<Fact>]
let ``test ImportAll with Erase interface calls string method`` () =
#if FABLE_COMPILER
    nativeCode.concatStrings ("Hello, ", "World!") |> equal "Hello, World!"
#else
    ()
#endif

[<Fact>]
let ``test ImportAll with stdlib module single-arg method`` () =
#if FABLE_COMPILER
    // maps:from_list([{key, value}]) should work via ImportAll
    let list: obj = emitErlExpr () "[{<<\"a\">>, 1}]"
    let result = erlangMaps.from_list(list)
    let value: obj = erlangMaps.get(box "a", result)
    equal 1 (unbox<int> value)
#else
    ()
#endif

[<Fact>]
let ``test ImportAll binding assigned to local variable`` () =
#if FABLE_COMPILER
    // Test that ImportAll works when assigned to a local let binding
    let m = nativeCode
    m.getName () |> equal "native_code"  // 0-arity (property-style)
    m.addValues (3, 4) |> equal 7        // multi-arg method
#else
    ()
#endif

[<Fact>]
let ``test ImportAll used with pipe operator`` () =
#if FABLE_COMPILER
    // Test that ImportAll works when used in a pipeline
    (3, 4) |> nativeCode.addValues |> equal 7
#else
    ()
#endif

[<Fact>]
let ``test Erased types can have members`` () =
#if FABLE_COMPILER
    let x = ErasedString "Patrick"
    x.SayHi() |> equal "Hi Patrick!"
#else
    ()
#endif

[<Fact>]
let ``test Erased unions with multiple fields work`` () =
#if FABLE_COMPILER
    let gimme (ErasedUnionWithMultipleFields(s, i)) =
        sprintf "Gimme %i %ss" i s
    ("apple", 5)
    |> ErasedUnionWithMultipleFields
    |> gimme
    |> equal "Gimme 5 apples"
#else
    ()
#endif

[<Fact>]
let ``test StringEnum works`` () =
#if FABLE_COMPILER
    let value = Vertical
    let expected: string = emitErlExpr () "<<\"vertical\">>"
    equal expected (string value)
#else
    ()
#endif

[<Fact>]
let ``test StringEnum with CompiledName works`` () =
#if FABLE_COMPILER
    let value = Horizontal
    let expected: string = emitErlExpr () "<<\"Horizontal\">>"
    equal expected (string value)
#else
    ()
#endif

[<Fact>]
let ``test StringEnum with SnakeCase works`` () =
#if FABLE_COMPILER
    let value = UserInfo.UserLoginCount
    let expected: string = emitErlExpr () "<<\"user_login_count\">>"
    equal expected (string value)
#else
    ()
#endif

[<Fact>]
let ``test StringEnum with KebabCase works`` () =
#if FABLE_COMPILER
    let value = MyCssOptions.ContentBox
    let expected: string = emitErlExpr () "<<\"content-box\">>"
    equal expected (string value)
#else
    ()
#endif

// ============================================================
// Erlang.receive tests
// ============================================================

[<Fact>]
let ``test Erlang receive with self-send returns Some`` () =
#if FABLE_COMPILER
    // Send a bare-atom message (Ping has zero fields) to self
    emitErlExpr () "erlang:self() ! ping"
    match Erlang.receive<RecvMsg> 1000 with
    | Some Ping -> equal 1 1
    | _ -> equal 0 1 // fail
#else
    ()
#endif

[<Fact>]
let ``test Erlang receive timeout returns None`` () =
#if FABLE_COMPILER
    // No message in mailbox, should timeout immediately
    match Erlang.receive<RecvMsg> 0 with
    | None -> equal 1 1
    | Some _ -> equal 0 1 // fail
#else
    ()
#endif

[<Fact>]
let ``test Erlang receive with CompiledName atom tag`` () =
#if FABLE_COMPILER
    // Send {custom_tag, 42} to self
    emitErlExpr () "erlang:self() ! {custom_tag, 42}"
    match Erlang.receive<RecvMsg> 1000 with
    | Some(CustomTag v) -> equal 42 v
    | _ -> equal 0 1 // fail
#else
    ()
#endif

[<Fact>]
let ``test Erlang receive with multi-field DU case`` () =
#if FABLE_COMPILER
    // Send {data_msg, 7, <<"hello">>} to self
    emitErlExpr () "erlang:self() ! {data_msg, 7, <<\"hello\">>}"
    match Erlang.receive<RecvMsg> 1000 with
    | Some(DataMsg(x, y)) ->
        equal 7 x
        equal "hello" y
    | _ -> equal 0 1 // fail
#else
    ()
#endif

[<Fact>]
let ``test Erlang receive with uppercase CompiledName atom tag`` () =
#if FABLE_COMPILER
    // Send {'EXIT', 1, <<"normal">>} to self — uppercase atoms must be single-quoted
    emitErlExpr () "erlang:self() ! {'EXIT', 1, <<\"normal\">>}"
    match Erlang.receive<RecvMsg> 1000 with
    | Some(Exit(pid, reason)) ->
        equal 1 pid
        equal "normal" reason
    | _ -> equal 0 1 // fail
#else
    ()
#endif

[<Fact>]
let ``test Erlang receive blocking with self-send`` () =
#if FABLE_COMPILER
    // Send a message before calling receive so it doesn't block
    emitErlExpr () "erlang:self() ! ping"
    let msg = Erlang.receive<RecvMsg> ()
    match msg with
    | Ping -> equal 1 1
    | _ -> equal 0 1 // fail
#else
    ()
#endif

[<Fact>]
let ``test Erlang receive blocking waits for delayed message`` () =
#if FABLE_COMPILER
    // Spawn a process that sends a message after a short delay.
    // If receive generates an invalid "after ok" clause, BEAM will crash
    // with {timeout_value, ...} before the message arrives.
    let self: obj = emitErlExpr () "erlang:self()"
    emitErlExpr self "erlang:spawn(fun() -> timer:sleep(50), $0 ! ping end)"
    let msg = Erlang.receive<RecvMsg> ()
    match msg with
    | Ping -> equal 1 1
    | _ -> equal 0 1 // fail
#else
    ()
#endif
