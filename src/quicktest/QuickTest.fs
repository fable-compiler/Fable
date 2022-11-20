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
           printfn "%s" (ex.StackTrace ??= "")
   printfn ""

let testCaseAsync msg f =
   testCase msg (fun () ->
       async {
           try
               do! f ()
           with ex ->
               printfn "%s" ex.Message
               if ex.Message <> null && ex.Message.StartsWith("[ASSERT ERROR]") |> not then
                   printfn "%s" (ex.StackTrace ??= "")
       } |> Async.StartImmediate)

let throwsAnyError (f: unit -> 'a): unit =
    let success =
        try
            f() |> ignore
            true
        with e ->
            printfn "Got expected error: %s" e.Message
            false
    if success then
        printfn "[ERROR EXPECTED]"

let measureTime (f: unit -> unit): unit = emitJsStatement () """
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

[<Erase>]
type MyRecord =
    { Foo: int }

let testMyRecord (r: MyRecord) =
    r.Foo

testMyRecord { Foo = 5 } |> printfn "%i"

[<Erase(tag=true)>]
type Foo =
    | Foo of foo: string * bar: int
    | Zas of ja: float

let test = function
    | Foo(foo, bar) -> String.replicate bar foo
    | Zas f -> $"It is a float: %.2f{f}"

Zas 5.67890 |> test |> printfn "%s"
Foo("oh", 3) |> test |> printfn "%s"

(*
module TaggedUnion =
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

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type NumberTagged =
        | [<CompiledValue(0)>] Foo of Foo<int>
        | [<CompiledValue(1.0)>] Bar of Bar<int>
        | [<CompiledValue(2)>] Baz of Baz<int>

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("kind")>]
    type BoolTagged =
        | [<CompiledValue(true)>] Foo of Foo<bool>
        | [<CompiledValue(false)>] Bar of Bar<bool>

module Tests =
    testCase "Case testing with TS tagged unions of string tags works" <| fun () ->
        let describe = function
            | TaggedUnion.StringTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.StringTagged.Bar x -> sprintf "bar: %d" x.bar
            | TaggedUnion.StringTagged.Baz x -> sprintf "baz: %b" x.baz
        TaggedUnion.StringTagged.Foo !!{| kind = "foo"; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.StringTagged.Bar !!{| kind = "bar"; bar = 42 |} |> describe |> equal "bar: 42"
        TaggedUnion.StringTagged.Baz !!{| kind = "_baz"; baz = false |} |> describe |> equal "baz: false"

    testCase "Case testing with TS tagged unions of number tags works" <| fun () ->
        let describe = function
            | TaggedUnion.NumberTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.NumberTagged.Bar x -> sprintf "bar: %d" x.bar
            | TaggedUnion.NumberTagged.Baz x -> sprintf "baz: %b" x.baz
        TaggedUnion.NumberTagged.Foo !!{| kind = 0; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.NumberTagged.Bar !!{| kind = 1.0; bar = 42 |} |> describe |> equal "bar: 42"
        TaggedUnion.NumberTagged.Baz !!{| kind = 2; baz = false |} |> describe |> equal "baz: false"

    testCase "Case testing with TS tagged unions of boolean tags works" <| fun () ->
        let describe = function
            | TaggedUnion.BoolTagged.Foo x -> sprintf "foo: %s" x.foo
            | TaggedUnion.BoolTagged.Bar x -> sprintf "bar: %d" x.bar
        TaggedUnion.BoolTagged.Foo !!{| kind = true; foo = "hello" |} |> describe |> equal "foo: hello"
        TaggedUnion.BoolTagged.Bar !!{| kind = false; bar = 42 |} |> describe |> equal "bar: 42"

    // testCase "Case testing with TS tagged unions of mixed type tags works" <| fun () ->
    //     let describe = function
    //         | TaggedUnion.MixedTagged.Foo x -> sprintf "foo: %s" x.foo
    //         | TaggedUnion.MixedTagged.Bar x -> sprintf "bar: %d" x.bar
    //         | TaggedUnion.MixedTagged.Baz x -> sprintf "baz: %b" x.baz
    //     TaggedUnion.MixedTagged.Foo !!{| kind = 0; foo = "hello" |} |> describe |> equal "foo: hello"
    //     TaggedUnion.MixedTagged.Bar !!{| kind = "bar"; bar = 42 |} |> describe |> equal "bar: 42"
    //     TaggedUnion.MixedTagged.Baz !!{| kind = false; baz = false |} |> describe |> equal "baz: false"

    // testCase "Case testing with TS tagged unions of enum tags works" <| fun () ->
    //     let describe = function
    //         | TaggedUnion.EnumTagged.Foo x -> sprintf "foo: %s" x.foo
    //         | TaggedUnion.EnumTagged.Bar x -> sprintf "bar: %d" x.bar
    //         | TaggedUnion.EnumTagged.Baz x -> sprintf "baz: %b" x.baz
    //     TaggedUnion.EnumTagged.Foo !!{| kind = TaggedUnion.Kind.Foo; foo = "hello" |} |> describe |> equal "foo: hello"
    //     TaggedUnion.EnumTagged.Bar !!{| kind = TaggedUnion.Kind.Bar; bar = 42 |} |> describe |> equal "bar: 42"
    //     TaggedUnion.EnumTagged.Baz !!{| kind = TaggedUnion.Kind.Baz; baz = false |} |> describe |> equal "baz: false"
*)