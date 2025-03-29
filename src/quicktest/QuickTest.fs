module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open Fable.Core
open Fable.Core.JsInterop

[<ImportMember("react")>]
[<AllowNullLiteral>]
type ReactElement = interface end

type private IReactProperty = JSX.Prop

[<Erase>]
type prop =
    static member inline children(children: ReactElement list) : IReactProperty = "children", children

    static member inline text(value: int) : IReactProperty = "children", [ JSX.text !!value ]

    static member inline key(value: int) : IReactProperty = "key", string value

[<Erase>]
type Html =

    static member inline fragment(children: ReactElement list) : ReactElement =
        JSX.create "" [ "children" ==> children ] |> unbox

    static member inline div(number: int) : ReactElement =
        JSX.create "div" [ "children" ==> JSX.text (!!number) ] |> unbox

    static member inline div(children: ReactElement list) : ReactElement =
        JSX.create "div" [ "children" ==> children ] |> unbox

    static member inline div(text: string) : ReactElement =
        JSX.create "div" [ "children" ==> JSX.text text ] |> unbox

    static member inline div(props: IReactProperty list) : ReactElement = JSX.create "div" props |> unbox

// let child1 = Html.div [
//     Html.div "Test 1"
// ]
// let child2 = Html.div "Test 3"

// let allChildren = [ child1 ]
let test_0 =
    Html.div "Test 1"

let test_1  =
    Html.div
        [
            yield! [
                Html.div "Test 1"
                Html.div "Test 2"
            ]
        ]

let test_2 =
    Html.div [
        yield! [
            Html.div "Test 1"
        ]
        yield! [
            Html.div "Test 2"
        ]
    ]

let test_3 =
    Html.div [
        Html.div "Test 1"
        yield! [
            Html.div "Test 2"
        ]
    ]

let test_4 =
    Html.div [
        Html.div "Test 1"
        yield! [
            Html.div "Test 2"
            yield! [
                Html.div "Test 2.2"
            ]
        ]
    ]

let test_5 =
    Html.div [
        Html.div "Test 1"
        yield! [
            Html.div "Test 2"
            Html.div [
                Html.div "Test 2.1"
                yield! [
                    Html.div "Test 2.1"
                ]
            ]
        ]
    ]

let test_6 =
    Html.div [
        for i in 0..2 do
            Html.div [
                prop.key i
                prop.text i
            ]
    ]

// // Html.div "Test 1"
//                 // Html.div "Test 2"
//                 // // Html.fragment
//                 // //     [
//                 // for i in 0..2 do
//                 //     Html.div [
//                 //         prop.key i
//                 //         prop.text i
//                 //     ]
//                 // //     ]
//                 // yield! [ Html.div "Test 2" ]
//                 yield! [
//                     Html.div "Test 1"
//                     Html.div "Test 2"
//                     // Html.div [
//                     //     yield! [
//                     //         Html.div "Test 1.1"
//                     //         Html.div "Test 1.2"
//                     //     ]
//                     // ]
//                 ]
//                 // Test 1
