#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

// Redux Helper
module Redux =
    type IStore<'TState, 'TAction> = interface end

    let [<Import("*","redux")>] redux = obj()
        
    let inline createStore (reducer: 'TState->'TAction->'TState) (initState: 'TState) =
        redux?createStore((fun state action ->
            match box action?Case with
            | :? string -> reducer state action  
            | _ -> state), initState, unbox Browser.window?devToolsExtension
                                      && unbox (Browser.window?devToolsExtension()))
        |> unbox<IStore<'TState, 'TAction>>

    let dispatch (store: IStore<'TState, 'TAction>) (x: 'TAction) =
        let x = toPlainJsObj x
        x?``type`` <- x?Case 
        store?dispatch(x) |> ignore

    let inline subscribe (store: IStore<'TState, 'TAction>) (f: unit->unit) =
        store?subscribe(f)

    let inline getState (store: IStore<'TState, 'TAction>) =
        store?getState() |> unbox<'TState>

open Redux

type Action = Increment of int | Decrement of int

let counter state = function
    | Increment i -> state + i
    | Decrement i -> state - i

let store = createStore counter 0

let valueEl = Browser.document.getElementById("value")

let render() =
    valueEl.innerHTML <- getState store |> string

render()
subscribe store render

let inline listenTo elId ev (f: unit->'a) =
    Browser.document.getElementById(elId)
            .addEventListener(ev, unbox f)     

listenTo "increment" "click" (fun () ->
    Increment 2 |> dispatch store)

listenTo "decrement" "click" (fun () ->
    Decrement 1 |> dispatch store)

listenTo "incrementIfOdd" "click" (fun () ->
    if getState store % 2 <> 0 then
        Increment 1 |> dispatch store)

listenTo "incrementAsync" "click" (fun () ->
    Browser.window.setTimeout((fun () ->
    Increment 5 |> dispatch store), 1000.))
