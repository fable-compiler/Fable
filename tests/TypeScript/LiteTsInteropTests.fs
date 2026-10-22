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
#endif

let tests =
  testList "LiteTsInterop" [
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
    #endif
  ]
