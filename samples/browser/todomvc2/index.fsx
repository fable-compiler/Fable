// This is an alternate version of todomvc with Vue
// It doesn't use the F# functional capabilities yet
// but takes advantage of static type checking

// Load Fable.Core and bindings to JS global objects
#load "node_modules/fable-core/Fable.Core.fs"
#load "node_modules/fable-import-js/Fable.Import.JS.fs"
#load "node_modules/fable-import-browser/Fable.Import.Browser.fs"

open Fable.Core
type JS = Fable.Import.JS.Globals
type Browser = Fable.Import.Browser.Globals

// Use this dummy module to hold references to Vue and Router objects
// exposed globally by loading the corresponding libraries with HTML script tags
[<Erase>]
module Lib =
    let [<Global>] Vue: obj = failwith "JS only"
    let [<Global>] Router: obj = failwith "JS only"

type Todo = {
    mutable title: string
    mutable completed: bool
}

// This helper uses JS reflection to convert a class instance
// to the options' format required by Vue
module VueHelper =
    let createFromObj(data: obj, extraOpts: obj) =
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
        extraOpts?data <- data
        extraOpts?computed <- computed
        extraOpts?methods <- methods
        createNew Lib.Vue extraOpts

module Storage =
    let private STORAGE_KEY = "todos-vuejs"

    let fetch (): Todo[] =
        Browser.localStorage.getItem(STORAGE_KEY)
        |> function null -> "[]" | x -> unbox x
        |> JS.JSON.parse |> unbox

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
            | None -> ()
            | Some v ->
                newTodo <- None
                todos <- [|
                    yield! todos
                    yield { title = v.Trim(); completed = false }
                |]
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
            
    // We still need to add some extra options and
    // for that we can still use a dynamic object
    let [<Emit("this")>] private this: obj = failwith "JS only"
    let extraOpts =
        createObj [
            "el" ==> ".todoapp"
            "watch" ==>
                createObj [
                    "todos" ==>
                        createObj [
                            "deep" ==> true
                            "handler" ==> Storage.save
                        ]
                ]
                "directives" ==>
                    createObj [
                        "todo-focus" ==> function
                            | None -> ()
                            | Some _ ->
                                let el = this?el
                                Lib.Vue?nextTick $ fun () ->
                                    el?focus $ () |> ignore
                                |> ignore
                    ]

        ]
        
    // Now instantiate the type and create a Vue view model
    // using the helper method
    let app = VueHelper.createFromObj(TodoViewModel(), extraOpts)

module Routes =
    let router = createNew Lib.Router ()

    ["all"; "active"; "completed"] |> Seq.iter (fun visibility ->
        router?on $ (visibility, fun () ->
            Main.app?visibility <- visibility)
        |> ignore)

    router?configure $ (
        createObj [
            "notfound" ==> fun () ->
                Browser.location.hash <- ""
                Main.app?visibility <- "all"
        ]
    )
    router?init $ ()
