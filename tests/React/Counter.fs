module Counter

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Feliz

// Necessary for JSX compilation
let React: obj = importAll "react"

[<ReactComponent>]
let Counter(init: int) =
    let (count, setCount) = Feliz.React.useState(init)
    Html.div [
        prop.style [ style.padding 10 ]
        prop.children [
            Html.h1 [
                prop.testId "header"
                prop.text count
            ]
            Html.button [
                prop.testId "button-increment"
                prop.text "Increment"
                prop.onClick (fun _ -> setCount(count + 1))
            ]

            // Check that fluent-ui bindings work, see #2709
            !!(Fable.FluentUI.Button.defaultButton [] [ str "A fluent button" ])
        ]
    ]

[<JSX.Component>]
let CounterJSX(init: int) =
    let (count, setCount) = Feliz.React.useState(init)
    JSX.html
        $"""
        <div style={createObj ["padding" ==> 10]}>
            <h1 data-testid="header-jsx">{count}</h1>
            <button data-testid="button-increment-jsx"
                onClick={fun _ -> setCount(count + 1)}>
                Increment
            </button>
        </div>
        """
