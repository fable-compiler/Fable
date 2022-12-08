module Fable.Tests.React

open Fable.Core.JsInterop
open Feliz
open Fable.Jester
open Fable.ReactTestingLibrary

// Necessary for JSX compilation
let React: obj = importAll "react"

[<ReactComponent>]
let ComponentAcceptingCurriedFunction fn _oneMoreParam =
    let value = fn 1 2
    Html.p [
        prop.testId "text"
        prop.text (value.ToString())
    ]

Jest.describe("React tests", (fun () ->

    Jest.test("Counter renders correctly", (fun () ->
        let elem = RTL.render(Counter.Counter(5))
        Jest.expect(elem.container).toMatchSnapshot()
    ))

    Jest.test("Counter state works", (fun () ->
        let elem = RTL.render(Counter.Counter(5))
        let header = elem.getByTestId "header"
        let button = elem.getByTestId "button-increment"
        Jest.expect(header).toHaveTextContent("5")
        RTL.fireEvent.click(button)
        Jest.expect(header).toHaveTextContent("6")
    ))

    Jest.test("Counter JSX renders correctly", (fun () ->
        let elem = RTL.render(Counter.CounterJSX(5) |> unbox)
        Jest.expect(elem.container).toMatchSnapshot()
    ))

    Jest.test("Counter JSX state works", (fun () ->
        let elem = RTL.render(Counter.CounterJSX(5) |> unbox)
        let header = elem.getByTestId "header-jsx"
        let button = elem.getByTestId "button-increment-jsx"
        Jest.expect(header).toHaveTextContent("5")
        RTL.fireEvent.click(button)
        Jest.expect(header).toHaveTextContent("6")
    ))

    Jest.test("SpreadSheet renders correctly", (fun () ->
        let elem = RTL.render(SpreadSheet.SpreadSheet() |> unbox)
        Jest.expect(elem.container).toMatchSnapshot()
    ))

    Jest.test("SpreadSheet parser works", (fun () ->
        let spreadsheet = RTL.render(SpreadSheet.SpreadSheet() |> unbox)
        let cells = spreadsheet.getAllByRole("cell")
        let cell = cells.[5]
        RTL.fireEvent.click(cell)
        let input = spreadsheet.getByAltText("cell editor")
        RTL.fireEvent.input(input, [
            event.target [
                Feliz.prop.value "=4+3"
            ]
        ])
        // Click another cell to remove the editor
        RTL.fireEvent.click(cells.[8])
        Jest.expect(cell).toHaveTextContent("7")
    ))

    // See #2628
    Jest.test("Curried functions passed to plugin transforms", (fun () ->
        let fn a b = a + b
        let elem = RTL.render(ComponentAcceptingCurriedFunction fn "ignored")
        let text = elem.getByTestId "text"
        Jest.expect(text).toHaveTextContent("3")
    ))
))