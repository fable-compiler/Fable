module Fable.Tests.Import

open System
open Util.Testing
open Fable.Tests.DllRef

type IFooImported =
    abstract foo: string

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

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

[<AllowNullLiteral>]
[<Global>]
type MyJsClassWithOptionsOverload [<ParamObject; Emit("$0")>]
    private (name: U2<string, ResizeArray<string>>, weight: float) =

    [<ParamObject; Emit("$0")>]
    new (name: string, weight: float) =
        MyJsClassWithOptionsOverload(U2.Case1 name, weight)

    [<ParamObject; Emit("$0")>]
    new (name: ResizeArray<string>, weight: float) =
        MyJsClassWithOptionsOverload(U2.Case2 name, weight)

    member val name: U2<string, ResizeArray<string>> = jsNative with get, set
    member val weight: float = jsNative with get, set

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

let tests =
  testList "Import" [
    #if FABLE_COMPILER
    testCase "importDefault works with getters" <| fun () -> // #2329
        Treasure.keyHolder.myKey |> equal "a secret"

    testCase "importDefault works with getters when inlined" <| fun () -> // #2329
        Treasure.keyHolderInline.myKey |> equal "a secret"
        // Make sure Fable doesn't remove the args in the wrapper
        myKeyInlineWrapper().myKey |> equal "a secret"

    testCase "Import with relative paths works" <| fun () ->
        fooAll.foo |> equal "foo"
        let fooAll2: IFooImported = JsInterop.importAll "./js/1foo.js"
        fooAll2.foo |> equal "foo"

    testCase "Symbols in external projects work" <| fun () ->
        equal "Fable Rocks!" Spaces.Helper.ConditionalExternalValue

    testCase "Can inherit imported abstract classes" <| fun () ->
        let x = MyFSharpClass()
        x.foo() |> equal 7

    testCase "Static members of imported classes work" <| fun () ->
        MyJsClass.fuzzyMultiply(5, 4) |> equal 15

    testCase "ParamObject works with constructors" <| fun () ->
        let c = MyJsClassWithOptionArgs(4, baz=5.6)
        c.value |> equal "4undefined5.6"

    testCase "ParamObject works with fromIndex arg" <| fun () ->
        let c = MyJsClassWithOptionArgs()
        c.method(5, baz=4., lol=10, bar="b")
        |> equal "5b410"

    testCase "ParamObject works with imported members" <| fun () ->
        MyJsClassWithOptionArgs.myJsMethodWithOptionArgs(5, baz=4., lol=10, bar="b")
        |> equal "5b410"

    testCase "ParamObject works with overloaded constructors" <| fun () ->
        // Test first overload
        let options1 = MyJsClassWithOptionsOverload("foo", 5.)
        let expected =
            createObj [
                "name", "foo"
                "weight", 5.
            ] |> unbox<MyJsClassWithOptionsOverload>

        options1 |> equal expected

        // Test second overload
        let options2 = MyJsClassWithOptionsOverload(ResizeArray ["foo"; "bar"], 5.)
        let expected =
            createObj [
                "name", [|"foo"; "bar"|]
                "weight", 5.
            ] |> unbox<MyJsClassWithOptionsOverload>

        options2 |> equal expected
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

    testCase "Files in outDir don't conflict" <| fun () -> // See #2259
        Fable.Tests.DllRef.Lib2.value |> equal 10
        Fable.Tests.DllRef2.Lib2.value |> equal 20

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

    // We can try to fix this (see commented code in FableTransforms.uncurryReceivedArgs) but then we break
    // cases when the function is actually curried (see test: Import with curried signatures works)
    // testCase "Importing functions as curried lambdas works" <| fun () ->
    //     addWithSurprise 40 2 |> equal 45

    testCase "ofImport should inline properly" <| fun () ->
        addWithImported "addWithSurprise" "./js/1foo.js" 8 9 |> equal 20
        addWithImported2 "addWithSurprise" "./js/1foo.js" 4 5 |> equal 12

    testCase "Inline import expressions work" <| fun () -> // See #2280
        add 3 2 |> equal 8

    testCase "Inline import expressions can absorb arguments" <| fun _ -> // See #2284
        let stylesheet = Stylesheet.load "./js/1foo.js"
        stylesheet.["myKey"] |> equal "a secret"
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

    testCase "Module mutable option values work" <| fun () -> // See #2147
        Util.mutableValueOpt <- Some 3
        Util.mutableValueOpt.Value |> equal 3
        Util.mutableValueOpt <- None
        Util.mutableValueOpt.IsNone |> equal true

  ]