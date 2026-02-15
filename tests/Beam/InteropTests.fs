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
