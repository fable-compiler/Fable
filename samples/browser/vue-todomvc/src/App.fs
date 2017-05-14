module VueTodoMVC

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Types


// To make dynamic programing less verbose
// we can define a couple of helpers and
let inline private (~%) x = createObj x

let inline private (=>) x y = x ==> y
#nowarn "0058"

// As we need to reference the filters using strings from the routes
// let's use a simple dictionary to hold them
let filters =
    let isComplete (x: Todo) = x.completed
    dict [
        "all", id
        "active", Array.filter (isComplete >> not)
        "completed", Array.filter isComplete
    ]

    // We'll use a typed object to represent our viewmodel instead of
    // a JS dynamic object to take advantage of static type checking
type TodoViewModel() =
    let mutable todos: Todo[] = Storage.fetch()
    let mutable newTodo: string option = None
    let mutable editedTodo: Todo option = None
    let mutable beforeEditCache: string = ""
    let mutable visibility = "all"

    // The type getters and setters will become
    // computed properties for Vue
    member __.filteredTodos =
        filters.[visibility] todos
    member __.remaining =
        filters.["active"] todos |> Seq.length
    member self.allDone
        with get() = self.remaining = 0
        and set(v) = todos |> Seq.iter (fun todo ->
            todo.completed <- v)

    // The type methods will become the view model methods
    // callable from the Vue HTML template
    // Note the self references will be interpreted correctly
    member __.addTodo() =
        match newTodo with
        | Some v when v.Trim().Length > 0 ->
            let todo = {
                id = Storage.uid + 1
                title = v.Trim()
                completed = false
            }
            newTodo <- None
            todos <- Array.append todos [|todo|]
        | _ -> ()
    member __.removeTodo todo =
        todos <- Array.filter ((<>) todo) todos
    member __.editTodo todo =
        beforeEditCache <- todo.title
        editedTodo <- Some todo
    member self.doneEdit todo =
        match editedTodo with
        | None -> ()
        | Some _ ->
            editedTodo <- None
            match todo.title.Trim() with
            | "" -> self.removeTodo todo
            | title -> todo.title <- title
    member __.cancelEdit todo =
        editedTodo <- None
        todo.title <- beforeEditCache
    member __.removeCompleted() =
        todos <- filters.["active"] todos

let extraOpts = %[
    "watch" => %[
        "todos" => %[
            "deep" => true
            "handler" => Storage.save
        ]
    ]
    "filters" => %[
        "pluralize" => fun n ->
            if n = 1 then "item" else "items"
    ]
    "directives" => %[
        "todo-focus" => fun (el: Browser.HTMLElement) value ->
            if value then el.focus()
    ]
]

    // Now instantiate the type and create a Vue view model
    // using the helper method
let app = VueHelpers.mount(TodoViewModel(), extraOpts, ".todo-app")

[<Emit("Router($0)")>]
let Router (x : obj) : obj = jsNative
let router = Router (obj())

["all"; "active"; "completed"] |> Seq.iter (fun visibility ->
    router?on(visibility, fun () ->
        app?visibility <- visibility)
    |> ignore)

router?configure(%[
    "notfound" ==> fun () ->
        Browser.location.hash <- ""
        app?visibility <- "all"
]) |> ignore

router?init() |> ignore