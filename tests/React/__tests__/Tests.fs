module Fable.Tests.React

open Fable.Jester
open Fable.ReactTestingLibrary

Jest.describe("React tests", (fun () ->

    Jest.test("ReactComponent renders correctly", (fun () ->
        let elem = RTL.render(App.Counter())
        Jest.expect(elem.container).toMatchSnapshot()
    ))

    Jest.test("ReactComponent state works", (fun () ->
        let elem = RTL.render(App.Counter())
        let header = elem.getByTestId "header"
        let button = elem.getByTestId "button-increment"
        Jest.expect(header).toHaveTextContent("0")
        RTL.fireEvent.click(button)
        Jest.expect(header).toHaveTextContent("1")
    ))
))