module Storage

open Types
open Fable.Import

let private storageKey = "todos-vuejs"
let mutable uid = 0

let fetch (): Todo[] =
    let todos: Todo[] =
        Browser.localStorage.getItem(storageKey)
        |> function null -> "[]" | x -> unbox x
        |> JS.JSON.parse |> unbox
    uid <- todos.Length
    todos |> Array.mapi (fun i x -> { x with id = i })

let save (todos: Todo[]) =
    Browser.localStorage.setItem(storageKey, JS.JSON.stringify todos)
