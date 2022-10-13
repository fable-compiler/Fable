module Fable.Tests.ImportTests

open System
open Util.Testing
open Fable.Tests.DllRef

type IFooImported =
    abstract foo: string

#if FABLE_COMPILER
open Fable.Core

[<Import("*", "./js/1foo.js")>]
let fooAll: IFooImported = jsNative

[<ImportMember("./js/1foo.js")>]
type MyClass() =
    new (v: string) = MyClass()
    member x.value: string = jsNative
    member x.bar(): string = jsNative
    member x.bar(s: string): string = jsNative
    static member foo(): string = jsNative
    static member foo(i: int): int = jsNative

[<ImportMember("./js/1foo.js")>]
type MyJsClassWithOptionArgs
    [<ParamObject>] (?foo: int, ?bar: string, ?baz: float) =

    member _.value: string = jsNative

    [<ParamObject(2)>] member _.method(foo: int, bar: string, baz: float, ?lol: int) = jsNative

    [<ImportMember("./js/1foo.js"); ParamObject(2)>] static member myJsMethodWithOptionArgs(foo: int, bar: string, baz: float, ?lol: int) = jsNative

type FooOptional =
    abstract Foo1: x: int * ?y: string -> int
    abstract Foo2: x: int * y: string option -> int
    abstract Foo3: x: int * y: string -> int

let square: int -> int = JsInterop.importMember "./js/1foo.js"
// let addWithSurprise: int -> int -> int = JsInterop.importMember "./js/1foo.js"
let inline add (x: int) (y: int): int = JsInterop.import "addWithSurprise" "./js/1foo.js "

type Adder = delegate of int * int -> int

let addWithDelegate (adder: Adder) (arg1: int) (arg2: int): int =
    adder.Invoke(arg1, arg2)

let inline addWithImported (importMember: string) (importPath: string) (arg1: int) (arg2: int): int =
    addWithDelegate (JsInterop.import importMember importPath) arg1 arg2

let addWithLambda (adder: int->int->int) (arg1: int) (arg2: int): int =
    adder arg1 arg2

let inline addWithImported2 (importMember: string) (importPath: string) (arg1: int) (arg2: int): int =
    addWithLambda (JsInterop.import importMember importPath) arg1 arg2

module Stylesheet =
    type IStylesheet =
        [<Emit "$0[$1]">]
        abstract Item : className:string -> string

    /// Loads a CSS module and makes the classes within available
    let inline load (path: string) = JsInterop.importDefault<IStylesheet> path

[<AbstractClass; ImportMember("./js/1foo.js")>]
type MyJsClass(x: int) =
    member this.foo(): int = jsNative
    abstract bar: unit -> int
    static member fuzzyMultiply(x: int, i: int): int = jsNative

type MyFSharpClass() =
    inherit MyJsClass(3)
    override _.bar() = 4

type ITreasure =
    abstract myKey: string

type Treasure =
    static member keyHolder: ITreasure = JsInterop.importDefault "./js/1foo.js"
    static member inline keyHolderInline: ITreasure = JsInterop.importDefault "./js/1foo.js"

let myKeyInlineWrapper() =
    Treasure.keyHolderInline
#endif

module tests =

    #if FABLE_COMPILER
    [<Fact>]
    let ``importDefault works with getters`` () = // #2329
        Treasure.keyHolder.myKey |> equal "a secret"

    [<Fact>]
    let ``importDefault works with getters when inlined`` () = // #2329
        Treasure.keyHolderInline.myKey |> equal "a secret"
        // Make sure Fable doesn't remove the args in the wrapper
        myKeyInlineWrapper().myKey |> equal "a secret"

    [<Fact>]
    let ``Import with relative paths works`` () =
        fooAll.foo |> equal "foo"
        let fooAll2: IFooImported = JsInterop.importAll "./js/1foo.js"
        fooAll2.foo |> equal "foo"

    [<Fact>]
    let ``Symbols in external projects work`` () =
        equal "Fable Rocks!" Spaces.Helper.ConditionalExternalValue

    [<Fact>]
    let ``Can inherit imported abstract classes`` () =
        let x = MyFSharpClass()
        x.foo() |> equal 7

    [<Fact>]
    let ``Static members of imported classes work`` () =
        MyJsClass.fuzzyMultiply(5, 4) |> equal 15

    [<Fact>]
    let ``ParamObject works with constructors`` () =
        let c = MyJsClassWithOptionArgs(4, baz=5.6)
        c.value |> equal "4undefined5.6"

    [<Fact>]
    let ``ParamObject works with fromIndex arg`` () =
        let c = MyJsClassWithOptionArgs()
        c.method(5, baz=4., lol=10, bar="b")
        |> equal "5b410"

    [<Fact>]
    let ``ParamObject works with imported members`` () =
        MyJsClassWithOptionArgs.myJsMethodWithOptionArgs(5, baz=4., lol=10, bar="b")
        |> equal "5b410"
    #endif

    [<Fact>]
    let ``Import with relative paths from project subdirectory works`` () =
        // Import expression
        Fable.Tests.Util.foo |> equal "foo"
        // Import attribute
        Fable.Tests.Util.foo2 |> equal "foo"

    [<Fact>]
    let ``Import with relative paths from external files works`` () =
        // Import expressions
        Fable.Tests.Util4.foo |> equal "foo"
        Fable.Tests.Util4.bar |> equal 5
        // Import attribute
        Fable.Tests.Util4.bar2 |> equal 5

    [<Fact>]
    let ``Import with curried signatures works`` () =
        // If the import is in another file and cross-module opt (--crossoptimize+) is enabled,
        // the resulting import path may be wrong if the other module is not in the same folder.
        let apply = Fable.Tests.Util.apply
        // let apply (f:Func<int,int,int>) (x:int) (y:int): int = JsInterop.importMember "./js/1foo.js"
        let add a b = apply (Func<_,_,_>(fun x y -> x + y)) a b
        3 |> add 2 |> equal 5

    [<Fact>]
    let ``Import with relative paths from referenced dll works`` () =
        Lib.モジュール.one |> equal 1
        Lib.モジュール.three |> equal 3
        Util.two |> equal 2
        Util.four |> equal 4

    [<Fact>]
    let ``Including JS files in compilation works`` () =
        Fable.Tests.DllRef.Lib.foo |> equal "foo"

    [<Fact>]
    let ``Including JS files with same name works`` () =
        Fable.Tests.DllRef.Lib.fooGenerator 3 |> equal "foofoofoo"

    [<Fact>]
    let ``Including same JS file from different F# sources works`` () =
        Fable.Tests.DllRef.Lib2.foo |> equal "foo"

    [<Fact>]
    let ``Default imports work`` () =
        Fable.Tests.DllRef.Lib2.bar |> equal "bar"

    [<Fact>]
    let ``Classes from included JS files work`` () =
        let x = Fable.Tests.DllRef.Lib2.Bar(2, "ho")
        x.generator() |> equal "hoho"

    [<Fact>]
    let ``Files in outDir don't conflict`` () = // See #2259
        Fable.Tests.DllRef.Lib2.value |> equal 10
        Fable.Tests.DllRef2.Lib2.value |> equal 20

    [<Fact>]
    let ``Referencing a Fable project through a dll works`` () =
        Fable.Tests.DllRef.Util.add2 5 |> equal 7

    [<Fact>]
    let ``Root members with JS non-valid chars work`` () = // See #207
        Lib.足す 3 2 |> equal 5
        Lib.引く 3 2 |> equal 1
        Lib.モジュール.ファンクション 0 |> equal false

    [<Fact>]
    let ``Identifiers are encoded correctly`` () = // See #482
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
    [<Fact>]
    let ``Overloads of an imported class are not mangled`` () = // See #564
        MyClass.foo() |> equal "foo"
        MyClass.foo(5) |> equal 25
        let c = MyClass()
        c.bar() |> equal "bar"
        c.bar("caracola") |> equal "CARACOLA"

    [<Fact>]
    let ``Secondary constructors of an imported class work`` () =
        let c = MyClass()
        let c2 = MyClass("hoho")
        c.value |> equal "haha"
        c2.value |> equal "hoho"

    [<Fact>]
    let ``Only omitted optional arguments are removed`` () = // See #231, #640
        let x: FooOptional = JsInterop.import "fooOptional" "./js/1foo.js"
        x.Foo1(5) |> equal 1
        x.Foo1(5, "3") |> equal 2
        x.Foo2(5, None) |> equal 2
        x.Foo3(5, null) |> equal 2
        x.Foo1(5,"3") |> equal 2
        x.Foo2(5,Some "3") |> equal 2
        x.Foo3(5, "3") |> equal 2

    [<Fact>]
    let ``Importing curried functions via `importMember` without naming arguments works`` () = // See #1185
        square 2 |> equal 4

    // We can try to fix this (see commented code in FableTransforms.uncurryReceivedArgs) but then we break
    // cases when the function is actually curried (see test: Import with curried signatures works)
    // [<Fact>]
    // let ``Importing functions as curried lambdas works`` () =
    //     addWithSurprise 40 2 |> equal 45

    [<Fact>]
    let ``ofImport should inline properly`` () =
        addWithImported "addWithSurprise" "./js/1foo.js" 8 9 |> equal 20
        addWithImported2 "addWithSurprise" "./js/1foo.js" 4 5 |> equal 12

    [<Fact>]
    let ``Inline import expressions work`` () = // See #2280
        add 3 2 |> equal 8

    [<Fact>]
    let ``Inline import expressions can absorb arguments`` () = // See #2284
        let stylesheet = Stylesheet.load "./js/1foo.js"
        stylesheet["myKey"] |> equal "a secret"
    #endif

    [<Fact>]
    let ``Module mutable values work`` () = // See #986
        Util.mutableValue <- 3
        Util.mutableValue |> equal 3
        Util.getValueTimes2() |> equal 6
        Util.Nested.getOuterValueTimes4() |> equal 12

    [<Fact>]
    let ``Nested module mutable values work`` () = // See #986
        Util.Nested.nestedMutableValue <- "C"
        Util.Nested.nestedMutableValue |> equal "C"
        Util.Nested.getValueTimes2() |> equal "CC"
        Util.getNestedValueTimes3() |> equal "CCC"

    [<Fact>]
    let ``Module mutable option values work`` () = // See #2147
        Util.mutableValueOpt <- Some 3
        Util.mutableValueOpt.Value |> equal 3
        Util.mutableValueOpt <- None
        Util.mutableValueOpt.IsNone |> equal true
