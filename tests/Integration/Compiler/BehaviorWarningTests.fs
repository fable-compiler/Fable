module Fable.Tests.Compiler.BehaviorWarning

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

(*
    Warnings about code that compiles but behaves differently from .NET. Each one carries a
    code so that a user who accepts the difference can silence it at the call site.
*)

let tests =
  testList "Behavior Warnings" [
    testCase "Console.Write is reported" <| fun _ ->
      let source =
        """
System.Console.Write("x")
"""
      compile source
      |> Assert.Code.warning "FABLE0108"
      |> ignore

    testCase "Console.Write can be suppressed by code" <| fun _ ->
      let source =
        """
System.Console.Write("x") // fable-disable-line FABLE0108
"""
      compile source
      |> Assert.Code.noWarning "FABLE0108"
      |> ignore

    testCase "Async.Start is reported" <| fun _ ->
      let source =
        """
Async.Start(async { () })
"""
      compile source
      |> Assert.Code.warning "FABLE0109"
      |> ignore

    testCase "Task.Result is reported" <| fun _ ->
      let source =
        """
let t = task { return 1 }
let r: int = t.Result
"""
      compile source
      |> Assert.Code.warning "FABLE0110"
      |> ignore

    testCase "GetType on obj is reported" <| fun _ ->
      let source =
        """
let x: obj = box 1
x.GetType() |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0111"
      |> ignore

    testCase "A type test against an interface is reported" <| fun _ ->
      let source =
        """
type IFoo =
    abstract X: int

let isFoo (o: obj) = match o with :? IFoo -> true | _ -> false
"""
      compile source
      |> Assert.Code.warning "FABLE0112"
      |> ignore

    testCase "Inheriting through a secondary constructor is reported" <| fun _ ->
      let source =
        """
type Base =
    val X: int
    new() = { X = 1 }

type Derived() =
    inherit Base()
"""
      compile source
      |> Assert.Code.warning "FABLE0114"
      |> ignore

    testCase "An inlined argument at risk of double evaluation is reported" <| fun _ ->
      let source =
        """
let inline orZero (o: int option) =
    defaultArg o 0 + (if o.IsSome then 1 else 0)

orZero (Some(System.Random().Next())) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0115"
      |> ignore

    testCase "CaseTest outside a TypeScriptTaggedUnion is reported" <| fun _ ->
      let source =
        """
open Fable.Core

type Id =
    | [<CaseTest("$0.startsWith('n:')")>] Name of string
    | [<CaseTest("$0.startsWith('s:')")>] Slug of string

let isName (id: Id) = match id with Name _ -> true | Slug _ -> false
"""
      compile source
      |> Assert.Code.warning "FABLE0116"
      |> ignore

    testCase "Duplicate CompiledName on union cases is reported" <| fun _ ->
      let source =
        """
type Prop =
    | [<CompiledName("ref")>] RefHook of int
    | [<CompiledName("ref")>] RefValue of string
"""
      compile source
      |> Assert.Code.warning "FABLE0121"
      |> Assert.Exists.warningWith "Cases 'RefValue' and 'RefHook' of 'Prop' both compile to 'ref'"
      |> ignore

    testCase "CompiledName clashing with another case name is reported" <| fun _ ->
      let source =
        """
type Id =
    | [<CompiledName("Slug")>] Name
    | Slug
"""
      compile source
      |> Assert.Code.warning "FABLE0121"
      |> ignore

    testCase "Duplicate CompiledName on field-less erased union cases is reported" <| fun _ ->
      let source =
        """
open Fable.Core

[<Erase>]
type X =
    | [<CompiledName("ff")>] A
    | [<CompiledName("ff")>] B
"""
      compile source
      |> Assert.Code.warning "FABLE0121"
      |> Assert.Exists.warningWith "Cases 'B' and 'A' of 'X' both compile to 'ff'"
      |> ignore

    testCase "Duplicate CompiledName on erased union cases with a field is not reported" <| fun _ ->
      let source =
        """
open Fable.Core

[<Erase>]
type X =
    | [<CompiledName("ff")>] A of int
    | [<CompiledName("ff")>] B of string
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "StringEnum values clashing through the case rule are reported" <| fun _ ->
      let source =
        """
open Fable.Core

[<StringEnum>]
type Kind =
    | [<CompiledName("small")>] S
    | Small
"""
      compile source
      |> Assert.Code.warning "FABLE0121"
      |> Assert.Exists.warningWith "Cases 'Small' and 'S' of 'Kind' both compile to 'small'"
      |> ignore

    testCase "Duplicate CompiledName can be suppressed by code" <| fun _ ->
      let source =
        """
type Prop =
    | [<CompiledName("ref")>] RefHook of int
    | [<CompiledName("ref")>] RefValue of string // fable-disable-line FABLE0121
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag are not reported as duplicates" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("state")>]
type Terminal =
    | [<CompiledName("done")>] Done of value: float
    | [<CompiledName("done")>] DoneError of error: string
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore
  ]
