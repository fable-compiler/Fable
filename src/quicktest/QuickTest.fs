module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open System.Globalization

open Fable.Core

// let x : obj = JS.Object?fromObjectEntries("hello")

// x |> ignore

let mutable a = 0
a |> ignore
let mutable b = 0
b |> ignore

// // let Ele() =
// //     JSX.create "div" [
// //         if a = 1 then
// //             "className", "first"
// //         else
// //             "className", "second"

// //         // "className", "first"
// //     ]

// // let Ele2() =
// //     JSX.create "div" [
// //         if a = 1 then
// //             "children", [
// //                 "div", "True 1"
// //                 "div", "True 2"
// //             ]
// //         else
// //             "children", [
// //                 "div", "False 1"
// //                 "div", "False 2"
// //             ]

// //         // "className", "first"
// //     ]

// // Condition children props

// // JSX.create "div" [
// //     if a = 1 then
// //         "className", "success"
// //         "children", "All good"
// //     else
// //         "className", "error"
// //         "children", "Please fix"
// // ]

// // Should we make sure we keep properties order in case their is an overwrite

// // [<Global>]
// // let Object :obj = jsNative

// // let x : obj =
// //     Object?fromEntries(ResizeArray [
// //         "id", "myId"
// //         if a = 0 then
// //             "class", "myId"
// //             "style", "red"
// //         else
// //             "id", "myId"
// //     ])

// // JS.console.log(x)

let Ele1 () =
    JSX.create
        "div"
        [
            if a = 1 then
                "className", "second"

            "children",
            [
                // JSX.create "div" []
                JSX.create "div" []
            ]
        ]

// let Ele2() =
//     JSX.create "div" [
//         // if a = 1 then
//         //     "className", "first"
//         //     "id", "my-id"
//         //     "children", "first"
//         // else
//         //     "className", "second"
//         //     "children", "second"

//         if a = 1 then
//             // "className", JSX.text "second"
//             "children", JSX.text "Level 1"

//         // if b = 1 then
//         //     "children", JSX.create "div" [
//         //         if a = 1 then
//         //             "children", "1.1"
//         //     ]

//         // "children", JSX.create "div" []

//         // "className", "first"
//     ]


// // const MyObjSpreadExample = () => {
// //   const shouldHaveBackground = true;

// //   const props = Object.fromEntries([
// //     ["id", "myId"],
// //     shouldHaveBackground && ["style", { background: "red" }],
// //     ["children", <button>Click me!</button>]
// //   ].filter(Boolean));

// //   return <div {...props}></div>;
// // };

// // [<AllowNullLiteral>]
// // [<Global>]
// // type Props
// //     [<ParamObject; Emit("$0")>]
// //     (
// //         ?id: string,
// //         ?className: string,
// //         ?children: obj list
// //     ) =
// //     member val searchTerm: string = jsNative with get, set
// //     member val isCaseSensitive: bool option = jsNative with get, set

// // [<Erase>]
// // type Html =

// //     static member inline div(props:
// //         {|
// //             id : string
// //             className : string
// //             children: obj list
// //             style : obj
// //         |}) : obj =
// //         JSX.create "div" (unbox props) |> unbox


// //     static member inline span(props:
// //         // The anonymous record needs to be duplicated over and over
// //         // (yes you could use an alias in theory)
// //         {|
// //             id : string
// //             className : string
// //             children: obj list
// //             style : obj
// //         |}) : obj =
// //         JSX.create "div" (unbox props) |> unbox

// // // But this would not solve the caller side because people need to provides all the properties of an
// // // anonymous records

// // let b =
// //     Html.div
// //         {|
// //             className = "button"
// //         |}
// //     // Error: This anonymous record is missing fields 'children', 'id', 'style'.F


// // const res = Object.fromEntries([
// //     (a === 1 ? ['className', 'second'] : [],
// //     b === 1
// //         ? [
// //             'children',
// //             'child content',
// //         ]
// //         : []),
// // ])

// // console.log(res)

// // console.log(Object.fromEntries([[]].filter(e => e.length === 2)))
// // Remove invalid tuples from the list
