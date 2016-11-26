// TODO MVC app Vue.js
// It doesn't use the F# functional capabilities
// but takes advantage of static type checking

// Load Fable.Core
#r "../../node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import
open Fable.Core.JsInterop

// Import Vue and director objects
let Vue: obj = importDefault "vue"
let Router: obj->obj = importDefault "./lib/director.js"

// To make dynamic programing less verbose
// we can define a couple of helpers and
// disable warning 0058
#nowarn "0058"

let inline private (~%) x = createObj x

let inline private (=>) x y = x ==> y

// This helper uses JS reflection to convert a class instance
// to the options' format required by Vue
module VueHelper =
    let private toPojo (o: obj) =
        let o2 = obj()
        for k in JS.Object.getOwnPropertyNames o do
            o2?(k) <- o?(k)
        o2

    let mount(data: obj, extraOpts: obj, el: string) =
        let methods = obj()
        let computed = obj()
        let proto = JS.Object.getPrototypeOf data
        for k in JS.Object.getOwnPropertyNames proto do
            let prop = JS.Object.getOwnPropertyDescriptor(proto, k)
            match prop.value with
            | Some f ->
                methods?(k) <- f
            | None ->
                computed?(k) <- createObj [
                    "get" ==> prop?get
                    "set" ==> prop?set
                ]
        extraOpts?data <- toPojo data
        extraOpts?computed <- computed
        extraOpts?methods <- methods
        let app = createNew Vue extraOpts
        app?``$mount``(el) |> ignore
        app

type Todo = {
    id: int
    mutable title: string
    mutable completed: bool
}

module Storage =
    let private STORAGE_KEY = "todos-vuejs"
    let mutable uid = 0

    let fetch (): Todo[] =
        let todos: Todo[] =
            Browser.localStorage.getItem(STORAGE_KEY)
            |> function null -> "[]" | x -> unbox x
            |> JS.JSON.parse |> unbox
        uid <- todos.Length
        todos |> Array.mapi (fun i x -> { x with id = i })

    let save (todos: Todo[]) =
        Browser.localStorage.setItem(STORAGE_KEY, JS.JSON.stringify todos)

module Main =
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

    let x = 4 % 5

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
    let app = VueHelper.mount(TodoViewModel(), extraOpts, ".todoapp")

module Routes =
    let router = createNew Router ()

    ["all"; "active"; "completed"] |> Seq.iter (fun visibility ->
        router?on(visibility, fun () ->
            Main.app?visibility <- visibility)
        |> ignore)

    router?configure(%[
        "notfound" ==> fun () ->
            Browser.location.hash <- ""
            Main.app?visibility <- "all"
    ])

    router?init()
