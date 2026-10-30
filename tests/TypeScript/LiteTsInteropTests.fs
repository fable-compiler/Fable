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

    [<Mangle>]
    type IMangled =
        abstract Foo: int -> int

    let useNotMangled (x: INotMangled) = x.Foo 1, x.Bar
    let useMangled (x: IMangled) = x.Foo 1
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
        let notMangled =
            { new MangleInterfaces.INotMangled with
                member _.Foo x = x + 1
                member _.Bar = "bar" }
        MangleInterfaces.useNotMangled notMangled |> equal (2, "bar")
        notMangled?Bar |> equal "bar"
        let mangled = { new MangleInterfaces.IMangled with member _.Foo x = x + 2 }
        MangleInterfaces.useMangled mangled |> equal 3
    #endif
  ]
