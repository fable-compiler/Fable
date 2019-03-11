module Fable.Tests.Import

open System
open Util.Testing
open Fable.Tests.DllRef

type IFooImported =
    abstract foo: string

#if FABLE_COMPILER
open Fable.Core

[<Import("*", "./js/1foo.js")>]
let fooAll: IFooImported = jsNative

[<Import("MyClass", "./js/1foo.js")>]
type MyClass() =
    new (v: string) = MyClass()
    member x.value: string = jsNative
    member x.bar(): string = jsNative
    member x.bar(s: string): string = jsNative
    static member foo(): string = jsNative
    static member foo(i: int): int = jsNative

type FooOptional =
    abstract Foo1: x: int * ?y: string -> int
    abstract Foo2: x: int * y: string option -> int
    abstract Foo3: x: int * y: string -> int

let square: int -> int = JsInterop.importMember "./js/1foo.js"
// let add: int -> int -> int = JsInterop.importMember "./js/1foo.js"
#endif

let tests =
  testList "Import" [
    #if FABLE_COMPILER
    testCase "Import with relative paths works" <| fun () ->
        fooAll.foo |> equal "foo"
        let fooAll2: IFooImported = JsInterop.importAll "./js/1foo.js"
        fooAll2.foo |> equal "foo"

    testCase "Symbols in external projects work" <| fun () ->
        equal "Fable Rocks!" Spaces.Helper.ConditionalExternalValue
    #endif

    testCase "Import with relative paths from project subdirectory works" <| fun () ->
        // Import expression
        Fable.Tests.Util.foo |> equal "foo"
        // Import attribute
        Fable.Tests.Util.foo2 |> equal "foo"

    testCase "Import with relative paths from external files works" <| fun () ->
        // Import expressions
        Fable.Tests.Util4.foo |> equal "foo"
        Fable.Tests.Util4.bar |> equal 5
        // Import attribute
        Fable.Tests.Util4.bar2 |> equal 5

    testCase "Import with curried signatures works" <| fun () ->
        // If the import is in another file and cross-module opt (--crossoptimize+) is enabled,
        // the resulting import path may be wrong if the other module is not in the same folder.
        let apply = Fable.Tests.Util.apply
        // let apply (f:Func<int,int,int>) (x:int) (y:int): int = JsInterop.importMember "./js/1foo.js"
        let add a b = apply (Func<_,_,_>(fun x y -> x + y)) a b
        3 |> add 2 |> equal 5

    testCase "Import with relative paths from referenced dll works" <| fun () ->
        Lib.モジュール.one |> equal 1
        Lib.モジュール.three |> equal 3
        Util.two |> equal 2
        Util.four |> equal 4

    testCase "Including JS files in compilation works" <| fun () ->
        Fable.Tests.DllRef.Lib.foo |> equal "foo"

    testCase "Including JS files with same name works" <| fun () ->
        Fable.Tests.DllRef.Lib.fooGenerator 3 |> equal "foofoofoo"

    testCase "Including same JS file from different F# sources works" <| fun () ->
        Fable.Tests.DllRef.Lib2.foo |> equal "foo"

    testCase "Default imports work" <| fun () ->
        Fable.Tests.DllRef.Lib2.bar |> equal "bar"

    testCase "Classes from included JS files work" <| fun () ->
        let x = Fable.Tests.DllRef.Lib2.Bar(2, "ho")
        x.generator() |> equal "hoho"

    testCase "Referencing a Fable project through a dll works" <| fun () ->
        Fable.Tests.DllRef.Util.add2 5 |> equal 7

    testCase "Root members with JS non-valid chars work" <| fun () -> // See #207
        Lib.足す 3 2 |> equal 5
        Lib.引く 3 2 |> equal 1
        Lib.モジュール.ファンクション 0 |> equal false

    testCase "Identifiers are encoded correctly" <| fun () -> // See #482
        equal "bar1" Lib.``$5EAfoo``
        equal "bar2" Lib.``$5E$Afoo``
        equal "bar3" Lib.``$5EA$foo``
        equal "bar4" Lib.``^Afoo``
        equal "bar5" Lib.``תfoo``
        equal "bar6" Lib.``foo$5EA``
        equal "bar7" Lib.``foo$5E$A``
        equal "bar8" Lib.``foo$5EA$``
        equal "bar9" Lib.``foo^A``
        equal "bar10" Lib.``fooת``

    #if FABLE_COMPILER
    testCase "Overloads of an imported class are not mangled" <| fun () -> // See #564
        MyClass.foo() |> equal "foo"
        MyClass.foo(5) |> equal 25
        let c = MyClass()
        c.bar() |> equal "bar"
        c.bar("caracola") |> equal "CARACOLA"

    testCase "Secondary constructors of an imported class work" <| fun () ->
        let c = MyClass()
        let c2 = MyClass("hoho")
        c.value |> equal "haha"
        c2.value |> equal "hoho"

    testCase "Only omitted optional arguments are removed" <| fun () -> // See #231, #640
        let x: FooOptional = JsInterop.import "fooOptional" "./js/1foo.js"
        x.Foo1(5) |> equal 1
        x.Foo1(5, "3") |> equal 2
        x.Foo2(5, None) |> equal 2
        x.Foo3(5, null) |> equal 2
        x.Foo1(5,"3") |> equal 2
        x.Foo2(5,Some "3") |> equal 2
        x.Foo3(5, "3") |> equal 2

    testCase "Importing curried functions via `importMember` without naming arguments works" <| fun () -> // See #1185
        square 2 |> equal 4

    // TODO: Importing like `let add: int->int->int = import ...` doesn't work in Fable 2
    // Fix it or leave the warning as it's now?
    // testCase "Importing curried functions with multiple arguments via `importMember` without naming arguments works" <| fun () ->
    //     add 40 2 |> equal 42
    #endif

    testCase "Module mutable values work" <| fun () -> // See #986
        Util.mutableValue <- 3
        Util.mutableValue |> equal 3
        Util.getValueTimes2() |> equal 6
        Util.Nested.getOuterValueTimes4() |> equal 12

    testCase "Nested module mutable values work" <| fun () -> // See #986
        Util.Nested.nestedMutableValue <- "C"
        Util.Nested.nestedMutableValue |> equal "C"
        Util.Nested.getValueTimes2() |> equal "CC"
        Util.getNestedValueTimes3() |> equal "CCC"
  ]