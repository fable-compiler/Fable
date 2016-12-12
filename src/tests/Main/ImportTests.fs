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
#endif
