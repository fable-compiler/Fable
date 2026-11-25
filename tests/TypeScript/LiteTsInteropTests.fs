module Fable.Tests.LiteTsInterop

open System
open Util.Testing
open Fable.Core

#if FABLE_COMPILER
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental
#endif

type JsOptions =
    abstract foo: string with get, set
    abstract bar: int with get, set

#if FABLE_COMPILER
module PojoInterface =
    [<JS.Pojo; AllowNullLiteral>]
    type Animal(name: string) =
        member val name = name

    [<JS.Pojo; AllowNullLiteral>]
    type Dog(name: string, breed: string, ?age: int) =
        inherit Animal(name)
        member val name = name
        member val breed = breed
        member val age = age with get, set

module TaggedUnion =
    type Take =
        abstract ``type``: string
        abstract fromIndex: int
        abstract toIndex: int

    type Edit =
        abstract text: string

    [<RequireQualifiedAccess; TypeScriptTaggedUnion("type")>]
    type Command =
        | Take of Take
        | Edit of edit: Edit
        | Save

// Top level values get TS annotations, so tsc checks the typed array type names exist
module TypedArrays =
    let int8s = JS.Constructors.Int8Array.Create [| 1y; -2y |]
    let uint8s = JS.Constructors.Uint8Array.Create [| 1uy; 2uy |]
    let uint8Clamped = JS.Constructors.Uint8ClampedArray.Create [| 1; 300 |]
    let int16s = JS.Constructors.Int16Array.Create [| 1s; -2s |]
    let uint16s = JS.Constructors.Uint16Array.Create [| 1us; 2us |]
    let int32s = JS.Constructors.Int32Array.Create [| 1; -2 |]
    let uint32s = JS.Constructors.Uint32Array.Create [| 1u; 2u |]
    let float32s = JS.Constructors.Float32Array.Create [| 1.5; -2.5 |]
    let float64s = JS.Constructors.Float64Array.Create [| 1.5; -2.5 |]
    let bigInt64s = JS.Constructors.BigInt64Array.Create 2
    let makeFloat32s () = JS.Constructors.Float32Array.Create [| 0.0; 1.0 |]

// tsc checks the declarations against the unmangled names the JavaScript uses, see #5158
module MangleInterfaces =
    [<Mangle(false)>]
    type INotMangled =
        abstract Foo: int -> int
        abstract Bar: string
        abstract Baz: int with get, set

    [<Mangle>]
    type IMangled =
        abstract Foo: int -> int

    let useNotMangled (x: INotMangled) = x.Foo 1, x.Bar
    let incrementBaz (x: INotMangled) = x.Baz <- x.Baz + 1
    let useMangled (x: IMangled) = x.Foo 1

module MangleAbstractClasses =
    [<AbstractClass; AttachMembers; Mangle>]
    type AttachedBase() =
        abstract Run: int -> int

    type AttachedImpl() =
        inherit AttachedBase()
        override _.Run x = x + 3

    let useAttached (x: AttachedBase) = x.Run 1
#endif

type GenericNameRecord<'a> =
    { Value: 'a }

    static member Stringify v = string v

let tests =
  testList "LiteTsInterop" [
    testCase "Compiler-generated generic names do not clash with the type's own" <| fun () ->
        GenericNameRecord<string>.Stringify 456 |> equal "456"
        { Value = "x" }.Value |> equal "x"

    #if FABLE_COMPILER
    testCase "emitJsExpr can be used with interpolation" <| fun () ->
        let twoPieces (x: int): int = emitJsExpr () $"1 + {x}"
        let threePieces (x: int): float = emitJsExpr () $"Math.sqrt({x})"
        let fourPieces (s: string) (x: int): string = emitJsExpr () $"{s}.repeat({x})"
        let fivePieces (x: int) (y: float): float = emitJsExpr () $"Math.pow({x}, {y})"
        let sevenPieces (x: int) (y: int) (z: int): int = emitJsExpr () $"Math.max({x}, {y}, {z})"
        let floatHole (x: float): string = emitJsExpr () $"typeof {x}"
        let stringHole (s: string): int = emitJsExpr () $"({s}).length"
        let boolHole (b: bool): bool = emitJsExpr () $"!{b}"
        let charHole (c: char): string = emitJsExpr () $"{c} + {c}"
        twoPieces 1 |> equal 2
        threePieces 16 |> equal 4.
        fourPieces "ab" 2 |> equal "abab"
        fivePieces 2 3. |> equal 8.
        sevenPieces 1 3 2 |> equal 3
        floatHole 1.5 |> equal "number"
        stringHole "abc" |> equal 3
        boolHole true |> equal false
        charHole 'a' |> equal "aa"

    testCase "emitJsStatement can be used with a short interpolation" <| fun () ->
        let f (x: int): int = emitJsStatement () $"return {x} + 1;"
        f 1 |> equal 2

    testCase "emitJsExpr keeps conversions written in interpolation holes" <| fun () ->
        let stringOperator (x: int): int = emitJsExpr () $"({string x}).length + {x} * 0"
        let toStringMethod (x: int): int = emitJsExpr () $"({x.ToString()}).length + {x} * 0"
        let floatHole (y: float): string = emitJsExpr () $"typeof {string y} + typeof {y}"
        let optionHole (o: int option): string = emitJsExpr () $"typeof {string o} + typeof {o}"
        let stringHole (s: string): string = emitJsExpr () $"typeof {string s} + typeof {s}"
        stringOperator 123 |> equal 3
        toStringMethod 123 |> equal 3
        floatHole 1.5 |> equal "stringnumber"
        optionHole (Some 1) |> equal "stringnumber"
        stringHole null |> equal "stringobject"

    testCase "emitJsExpr can be used with a single hole interpolation" <| fun () ->
        let intHole (x: int): int = emitJsExpr () $"{x}"
        let floatHole (y: float): float = emitJsExpr () $"{y}"
        let stringHole (s: string): string = emitJsExpr () $"{s}"
        let charHole (c: char): string = emitJsExpr () $"{c}"
        let operationHole (x: int): int = emitJsExpr () $"{x + 1}"
        let conversionHole (x: int): string = emitJsExpr () $"{string x}"
        intHole 5 |> equal 5
        floatHole 1.5 |> equal 1.5
        stringHole "a" |> equal "a"
        charHole 'a' |> equal "a"
        operationHole 1 |> equal 2
        conversionHole 5 |> equal "5"

    testCase "emitJsExpr evaluates unit holes in interpolation" <| fun () ->
        let mutable calls = 0
        let effect () : unit = calls <- calls + 1
        let unitHole (x: int): obj[] = emitJsExpr () $"[{effect ()}, {x}]"
        unitHole 1 |> Array.length |> equal 2
        calls |> equal 1

    testCase "jsOptions works" <| fun _ ->
        let opts = jsOptions<JsOptions>(fun o ->
            o.foo <- "bar"
            o.bar <- 5
        )
        opts.foo |> equal "bar"
        opts.bar |> equal 5

    testCase "jsOptions works when it is directly replaced as a POJO" <| fun () ->
        let opts = jsOptions<JsOptions>(fun o ->
            // This function call avoid the optimization to literal POJO
            let foo () = "foo"
            o.foo <- foo()
            o.bar <- 5
        )
        opts.foo |> equal "foo"
        opts.bar |> equal 5

    testCase "[<Pojo>] generates interface and can be constructed" <| fun () ->
        let dog = PojoInterface.Dog("Buddy", "Labrador")
        dog.name |> equal "Buddy"
        dog.breed |> equal "Labrador"
        dog.age |> equal None

    testCase "[<Pojo>] optional field can be mutated" <| fun () ->
        let dog = PojoInterface.Dog("Max", "Beagle")
        dog.age |> equal None
        dog.age <- Some 5
        dog.age |> equal (Some 5)

    testCase "TS tagged union case with an interface field compiles to the value itself" <| fun () ->
        let take =
            { new TaggedUnion.Take with
                member _.``type`` = "take"
                member _.fromIndex = 1
                member _.toIndex = 3 }
        let describe = function
            | TaggedUnion.Command.Take t -> sprintf "take: %d-%d" t.fromIndex t.toIndex
            | TaggedUnion.Command.Edit e -> sprintf "edit: %s" e.text
            | TaggedUnion.Command.Save -> "save"
        let command = TaggedUnion.Command.Take take
        obj.ReferenceEquals(command, take) |> equal true
        command |> describe |> equal "take: 1-3"
        TaggedUnion.Command.Edit { new TaggedUnion.Edit with member _.text = "hi" } |> describe |> equal "edit: hi"
        TaggedUnion.Command.Save |> describe |> equal "save"
        !!{| ``type`` = "take"; fromIndex = 2; toIndex = 5 |} |> describe |> equal "take: 2-5"

    testCase "Fable.Core typed arrays are annotated with native TS types" <| fun () ->
        TypedArrays.int8s.[1] |> equal -2y
        TypedArrays.uint8s.[1] |> equal 2uy
        TypedArrays.uint8Clamped.[1] |> equal 255uy
        TypedArrays.int16s.[1] |> equal -2s
        TypedArrays.uint16s.[1] |> equal 2us
        TypedArrays.int32s.[1] |> equal -2
        TypedArrays.uint32s.[1] |> equal 2u
        TypedArrays.float32s.[1] |> equal -2.5f
        TypedArrays.float64s.[1] |> equal -2.5
        TypedArrays.bigInt64s.length |> equal 2
        TypedArrays.makeFloat32s().[1] |> equal 1.0f

    testCase "Interfaces with [<Mangle(false)>] are declared with unmangled members" <| fun () ->
        let baz = ref 1
        let notMangled =
            { new MangleInterfaces.INotMangled with
                member _.Foo x = x + 1
                member _.Bar = "bar"
                member _.Baz with get () = baz.Value and set v = baz.Value <- v }
        MangleInterfaces.useNotMangled notMangled |> equal (2, "bar")
        notMangled?Bar |> equal "bar"
        MangleInterfaces.incrementBaz notMangled
        notMangled.Baz |> equal 2
        notMangled?Baz |> equal 2
        let mangled = { new MangleInterfaces.IMangled with member _.Foo x = x + 2 }
        MangleInterfaces.useMangled mangled |> equal 3

    testCase "Abstract classes with [<AttachMembers>] ignore [<Mangle>] in declarations" <| fun () ->
        let attached = MangleAbstractClasses.AttachedImpl()
        MangleAbstractClasses.useAttached attached |> equal 4
        attached?Run(2) |> equal 5
    #endif
  ]
