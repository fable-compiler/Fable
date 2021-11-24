module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let log (o: obj) =
    JS.console.log(o)
    // printfn "%A" o

let equal expected actual =
   let areEqual = expected = actual
   printfn "%A = %A > %b" expected actual areEqual
   if not areEqual then
       failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

let throwsError (expected: string) (f: unit -> 'a): unit =
   let success =
       try
           f () |> ignore
           true
       with e ->
           if not <| String.IsNullOrEmpty(expected) then
               equal e.Message expected
           false
   // TODO better error messages
   equal false success

let testCase (msg: string) f: unit =
   try
       printfn "%s" msg
       f ()
   with ex ->
       printfn "%s" ex.Message
       if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
           printfn "%s" ex.StackTrace
   printfn ""

let testCaseAsync msg f =
   testCase msg (fun () ->
       async {
           try
               do! f ()
           with ex ->
               printfn "%s" ex.Message
               if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
                   printfn "%s" ex.StackTrace
       } |> Async.StartImmediate)

let measureTime (f: unit -> unit) = emitJsStatement () """
   //js
   const startTime = process.hrtime();
   f();
   const elapsed = process.hrtime(startTime);
   console.log("Ms:", elapsed[0] * 1e3 + elapsed[1] / 1e6);
   //!js
"""

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4


type Base<'Kind> =
    abstract kind: 'Kind

type Foo<'Kind> =
    inherit Base<'Kind>
    abstract foo: string

type Bar<'Kind> =
    inherit Base<'Kind>
    abstract bar: int

type Baz<'Kind> =
    inherit Base<'Kind>
    abstract baz: bool

[<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
type StringTagged =
    | Foo of Foo<string>
    | Bar of Bar<string>
    | [<CompiledName("_baz")>] Baz of Baz<string>

testCase "Case testing with TS tagged unions of string tags works" <| fun () ->
    let describe = function
        | StringTagged.Foo x -> sprintf "foo: %s" x.foo
        | StringTagged.Bar x -> sprintf "bar: %d" x.bar
        | StringTagged.Baz x -> sprintf "baz: %b" x.baz
    StringTagged.Foo !!{| kind = "foo"; foo = "hello" |} |> describe |> equal "foo: hello"
    StringTagged.Bar !!{| kind = "bar"; bar = 42 |} |> describe |> equal "bar: 42"
    StringTagged.Baz !!{| kind = "_baz"; baz = false |} |> describe |> equal "baz: false"

[<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
type NumberTagged =
    | [<CompiledValue(0)>] Foo of Foo<int>
    | [<CompiledValue(1.0)>] Bar of Bar<int>
    | [<CompiledValue(2)>] Baz of Baz<int>

testCase "Case testing with TS tagged unions of number tags works" <| fun () ->
    let describe = function
        | NumberTagged.Foo x -> sprintf "foo: %s" x.foo
        | NumberTagged.Bar x -> sprintf "bar: %d" x.bar
        | NumberTagged.Baz x -> sprintf "baz: %b" x.baz
    NumberTagged.Foo !!{| kind = 0; foo = "hello" |} |> describe |> equal "foo: hello"
    NumberTagged.Bar !!{| kind = 1.0; bar = 42 |} |> describe |> equal "bar: 42"
    NumberTagged.Baz !!{| kind = 2; baz = false |} |> describe |> equal "baz: false"

[<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
type BoolTagged =
    | [<CompiledValue(true)>] Foo of Foo<bool>
    | [<CompiledValue(false)>] Bar of Bar<bool>

testCase "Case testing with TS tagged unions of boolean tags works" <| fun () ->
    let describe = function
        | BoolTagged.Foo x -> sprintf "foo: %s" x.foo
        | BoolTagged.Bar x -> sprintf "bar: %d" x.bar
    BoolTagged.Foo !!{| kind = true; foo = "hello" |} |> describe |> equal "foo: hello"
    BoolTagged.Bar !!{| kind = false; bar = 42 |} |> describe |> equal "bar: 42"

[<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
type MixedTagged =
    | [<CompiledValue(0)>] Foo of Foo<int>
    | Bar of Bar<string>
    | [<CompiledValue(false)>] Baz of Baz<bool>

testCase "Case testing with TS tagged unions of mixed type tags works" <| fun () ->
    let describe = function
        | MixedTagged.Foo x -> sprintf "foo: %s" x.foo
        | MixedTagged.Bar x -> sprintf "bar: %d" x.bar
        | MixedTagged.Baz x -> sprintf "baz: %b" x.baz
    MixedTagged.Foo !!{| kind = 0; foo = "hello" |} |> describe |> equal "foo: hello"
    MixedTagged.Bar !!{| kind = "bar"; bar = 42 |} |> describe |> equal "bar: 42"
    MixedTagged.Baz !!{| kind = false; baz = false |} |> describe |> equal "baz: false"

type Kind = Foo = 0 | Bar = 1 | Baz = 2

[<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
type EnumTagged =
    | [<CompiledValue(Kind.Foo)>] Foo of Foo<Kind>
    | [<CompiledValue(Kind.Bar)>] Bar of Bar<Kind>
    | [<CompiledValue(Kind.Baz)>] Baz of Baz<Kind>

testCase "Case testing with TS tagged unions of enum tags works" <| fun () ->
    let describe = function
        | EnumTagged.Foo x -> sprintf "foo: %s" x.foo
        | EnumTagged.Bar x -> sprintf "bar: %d" x.bar
        | EnumTagged.Baz x -> sprintf "baz: %b" x.baz
    EnumTagged.Foo !!{| kind = Kind.Foo; foo = "hello" |} |> describe |> equal "foo: hello"
    EnumTagged.Bar !!{| kind = Kind.Bar; bar = 42 |} |> describe |> equal "bar: 42"
    EnumTagged.Baz !!{| kind = Kind.Baz; baz = false |} |> describe |> equal "baz: false"