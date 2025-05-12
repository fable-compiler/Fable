module Fable.Tests.JSX_API

open Fable.Core
open Fable.Core.JsInterop
open Fable.Jester
open Fable.ReactTestingLibrary
open JsxListOptimisation

// Necessary for JSX compilation
let React: obj = importAll "react"

// Expose a render version to work directly with JSX.Element
type RTL with

    static member inline render (jsxElement: JSX.Element) =
        RTL.render(unbox<Feliz.ReactElement> jsxElement)

    static member inline render (jsxElement: Components.JSX_ReactElement) =
        RTL.render(unbox<Feliz.ReactElement> jsxElement)

let private matchSnapshot (element : Components.JSX_ReactElement)=
    element
    |> RTL.render
    |> _.container
    |> Jest.expect
    |> _.toMatchSnapshot()

Jest.describe("JSX API tests (using React)", fun () ->

    Jest.test("Element can have text directly in them", fun () ->
        matchSnapshot Components.divWithText
    )

    Jest.test("Element can have nested list", fun () ->
        matchSnapshot Components.divWithNestedList
    )

    Jest.test("Element can have multiple nested list", fun () ->
        matchSnapshot Components.divWithMultipleNestedList
    )

    Jest.test("Element can have mix of element and list", fun () ->
        matchSnapshot Components.divWithMixOfElementAndList
    )

    Jest.test("Element can have multi level mix of element and list", fun () ->
        matchSnapshot Components.multiLevelMixOfElementAndList
    )

    Jest.test("Test that optimised condition works for the true branch", fun () ->
        matchSnapshot Components.divWithOptimisedTrueCondition
    )

    Jest.test("Test that optimised condition works for the false branch", fun () ->
        matchSnapshot Components.divWithOptimisedFalseCondition
    )

    Jest.test("Test that non optimised condition works", fun () ->
        let mutable myCondition = "a"
        myCondition |> ignore

        matchSnapshot (Components.divWithConditionalChildren myCondition)
    )

    Jest.test("Element can have for loops", fun () ->
        matchSnapshot Components.divWithForLoop
    )

    Jest.test("Fragments are supported", fun () ->
        matchSnapshot Components.divWithFragment
    )

    Jest.test("Elements can have attributes", fun () ->
        matchSnapshot Components.divWithAttributes
    )

    Jest.test("Test that condition without else branch works if condition is True", fun () ->
        matchSnapshot (Components.divWithConditionalWithoutElseBranchWorks true)
    )

    Jest.test("Test that condition without else branch works if condition is False", fun () ->
        matchSnapshot (Components.divWithConditionalWithoutElseBranchWorks false)
    )

    Jest.test("Test that properties can be defined using `unbox`", fun () ->
        matchSnapshot Components.propsCanUseUnbox
    )
)
