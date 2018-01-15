[<Util.Testing.TestFixture>]
module Fable.Tests.Import

open System
open Util.Testing
open Fable.Tests.Util
open Fable.Tests.DllRef

type IFooImported =
    abstract foo: string

#if FABLE_COMPILER
[<Fable.Core.Import("*", "./js/foo.js")>]
let fooAll: IFooImported = failwith "JS only"

[<Test>]
let ``Import with relative paths works``() =
    fooAll.foo |> equal "foo"
    let fooAll2: IFooImported = Fable.Core.JsInterop.importAll "./js/foo.js"
    fooAll2.foo |> equal "foo"

[<Test>]
let ``Symbols in external projects work``() =
    equal "Fable Rocks!" Spaces.Helper.ConditionalExternalValue
#endif

[<Test>]
let ``Import with relative paths from project subdirectory works``() =
    // Import expression
    Fable.Tests.Util.foo |> equal "foo"
    // Import attribute
    Fable.Tests.Util.foo2 |> equal "foo"

[<Test>]
let ``Import with relative paths from external files works``() =
    // Import expressions
    Fable.Tests.Util4.foo |> equal "foo"
    Fable.Tests.Util4.bar |> equal 5
    // Import attribute
    Fable.Tests.Util4.bar2 |> equal 5

[<Test>]
let ``Import expressions with methods work``() = // See #721
    Fable.Tests.Util.apply (Func<_,_,_>(fun x y -> x + y)) 2 3 |> equal 5

[<Test>]
let ``Import with relative paths from referenced dll works``() =
    Lib.モジュール.one |> equal 1
    Lib.モジュール.three |> equal 3
    Util.two |> equal 2
    Util.four |> equal 4

[<Test>]
let ``Including JS files in compilation works``() =
    Fable.Tests.DllRef.Lib.foo |> equal "foo"

[<Test>]
let ``Including JS files with same name works``() =
    Fable.Tests.DllRef.Lib.fooGenerator 3 |> equal "foofoofoo"

[<Test>]
let ``Including same JS file from different F# sources works``() =
    Fable.Tests.DllRef.Lib2.foo |> equal "foo"

[<Test>]
let ``Default imports work``() =
    Fable.Tests.DllRef.Lib2.bar |> equal "bar"

[<Test>]
let ``Classes from included JS files work``() =
    let x = Fable.Tests.DllRef.Lib2.Bar(2, "ho")
    x.generator() |> equal "hoho"

[<Test>]
let ``Referencing a Fable project through a dll works``() =
    Fable.Tests.DllRef.Util.add2 5 |> equal 7

open Fable.Tests.DllRef

[<Test>]
let ``Root members with JS non-valid chars work``() = // See #207
    Lib.足す 3 2 |> equal 5
    Lib.引く 3 2 |> equal 1
    Lib.モジュール.ファンクション 0 |> equal false

[<Test>]
let ``Identifiers are encoded correctly``() = // See #482
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
open Fable.Core

[<Import("MyClass", "./js/foo.js")>]
type MyClass() =
    new (v: string) = MyClass()
    member x.value: string = jsNative
    member x.bar(): string = jsNative
    member x.bar(s: string): string = jsNative
    static member foo(): string = jsNative
    static member foo(i: int): int = jsNative

[<Test>]
let ``Overloads of an imported class are not mangled``() = // See #564
    MyClass.foo() |> equal "foo"
    MyClass.foo(5) |> equal 25
    let c = MyClass()
    c.bar() |> equal "bar"
    c.bar("caracola") |> equal "CARACOLA"

[<Test>]
let ``Secondary constructors of an imported class work``() =
    let c = MyClass()
    let c2 = MyClass("hoho")
    c.value |> equal "haha"
    c2.value |> equal "hoho"

type FooOptional =
    abstract Foo1: x: int * ?y: string -> int
    abstract Foo2: x: int * y: string option -> int
    abstract Foo3: x: int * y: string -> int

[<Test>]
let ``Only omitted optional arguments are removed``() = // See #231, #640
    let x: FooOptional = Fable.Core.JsInterop.import "fooOptional" "./js/foo.js"
    x.Foo1(5) |> equal 1
    x.Foo1(5, "3") |> equal 2
    x.Foo2(5, None) |> equal 2
    x.Foo3(5, null) |> equal 2
    x.Foo1(5,"3") |> equal 2
    x.Foo2(5,Some "3") |> equal 2
    x.Foo3(5, "3") |> equal 2

let square : int -> int = JsInterop.importMember "./js/foo.js"
[<Test>]
let ``Importing curried functions via `importMember` without naming arguments works`` () = // See #1185
    square 2 |> equal 4

let add : int -> int -> int = JsInterop.importMember "./js/foo.js"
[<Test>]
let ``Importing curried functions with multiple arguments via `importMember` without naming arguments works`` () =
    add 40 2 |> equal 42

#endif

[<Test>]
let ``Module mutable values work``() = // See #986
    Util.mutableValue <- 3
    Util.mutableValue |> equal 3
    Util.getValueTimes2() |> equal 6
    Util.Nested.getOuterValueTimes4() |> equal 12

[<Test>]
let ``Nested module mutable values work``() = // See #986
    Util.Nested.nestedMutableValue <- "C"
    Util.Nested.nestedMutableValue |> equal "C"
    Util.Nested.getValueTimes2() |> equal "CC"
    Util.getNestedValueTimes3() |> equal "CCC"

#if FABLE_COMPILER
module ExternalFuncs = 
  let toSpecialUniversalTime (time: obj) = Fable.Core.JsInterop.import "toUniversalTime" "../../build/fable-core/Date"

[<AutoOpen>]
module Extensions = 
  type System.DateTime with
    [<Fable.Core.Import("toUniversalTime", "../../build/fable-core/Date")>]
    member self.ToVerySpecialUniversalTime() : DateTime = jsNative

[<Test>]
let ``Import as extension method works``() =
    let value = DateTime(2015, 1, 1)
    let importedAsExtensionMethod = value.ToVerySpecialUniversalTime()
    let importedOridinaryFunction = value |> ExternalFuncs.toSpecialUniversalTime
    equal importedOridinaryFunction importedAsExtensionMethod
#endif
