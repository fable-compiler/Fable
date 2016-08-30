#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

let [<Global>] Redux = obj()
let [<Emit("window.devToolsExtension && window.devToolsExtension()")>] reduxTools() = obj()

// type Action = Increment | Decrement
type [<StringEnum>] ActionType = Increment | Decrement
type Action = { ``type``: ActionType }

let counter state action =
    let state = defaultArg state 0
    match action with
    | { ``type``=Increment } -> state + 1
    | { ``type``=Decrement } -> state - 1
    
// let store = Redux?createStore(counter)
let store = Redux?createStore(counter, reduxTools());

let inline dispatch x = store?dispatch(toPlainJsObj { ``type``=x }) 

let valueEl = Browser.document.getElementById("value")

let render() =
    valueEl.innerHTML <- store?getState() |> string

render()
store?subscribe(render)

let inline listenTo elId ev (f: unit->'a) =
    Browser.document.getElementById(elId)
            .addEventListener(ev, unbox f)     

listenTo "increment" "click" (fun () ->
    dispatch Increment)

listenTo "decrement" "click" (fun () ->
    dispatch Decrement)

listenTo "incrementIfOdd" "click" (fun () ->
    if unbox(store?getState()) % 2 <> 0 then
        dispatch Increment |> ignore)

listenTo "incrementAsync" "click" (fun () ->
    Browser.window.setTimeout((fun () ->
    dispatch Increment), 1000.))
