#load "../../../lib/Fable.Core.fs"
#load "../../../lib/Fable.Import.JS.fs"
#load "../../../lib/Fable.Import.Browser.fs"

open Fable.Core
open Fable.Import

[<Erase>]
module Lib =
    let [<Global>] Vue: obj = failwith "JS only"
    let [<Global>] Router: obj = failwith "JS only"

type Todo = {
    mutable title: string
    mutable completed: bool
}

module VueHelper =
    let createFromObj(data: obj, extraOpts: obj) =
        let methods = obj()
        let computed = obj()
        let proto = JS.Globals.Object.getPrototypeOf data
        for k in JS.Globals.Object.getOwnPropertyNames proto do
            let prop = JS.Globals.Object.getOwnPropertyDescriptor(proto, k)
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
        Browser.Globals.localStorage.getItem(STORAGE_KEY)
        |> unbox |> Option.ofObj
        |> function Some x -> x | None -> "[]"
        |> JS.Globals.JSON.parse |> unbox

    let save (todos: Todo[]) =
        Browser.Globals.localStorage.setItem(STORAGE_KEY, JS.Globals.JSON.stringify todos)

module Main =
    let [<Emit("this")>] this: obj = failwith "JS only"

    let filters =
        let isComplete (x: Todo) = x.completed
        dict [
            "all", id
            "active", Array.filter (isComplete >> not)
            "completed", Array.filter isComplete
        ]
        
    type TodoApp() =
        let mutable todos: Todo[] = Storage.fetch()
        let mutable newTodo: string option = None
        let mutable editedTodo: Todo option = None
        let mutable beforeEditCache: string = ""
        let mutable visibility = "all"
        
        // Computed properties
        member __.filteredTodos =
            filters.[visibility] todos 
        member __.remaining =
            filters.["active"] todos |> Seq.length
        member self.allDone
            with get() = self.remaining = 0
            and set(v) = todos |> Seq.iter (fun todo ->
                todo.completed <- v)
             
        // Methods
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
        
    let app = VueHelper.createFromObj(TodoApp(), extraOpts)

module Routes =
    let router = createNew Lib.Router ()
    
    ["all"; "active"; "completed"] |> Seq.iter (fun visibility ->
        router?on $ (visibility, fun () ->
            Main.app?visibility <- visibility)
        |> ignore)
    
    router?configure $ (
        createObj [
            "notfound" ==> fun () ->
                Browser.Globals.location.hash <- ""
                Main.app?visibility <- "all"
        ]
    )
    router?init $ ()