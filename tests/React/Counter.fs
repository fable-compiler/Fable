module Counter

open Feliz

[<ReactComponent>]
let Counter() =
    let (count, setCount) = React.useState(0)
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
        ]
    ]
