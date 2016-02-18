// This is a direct translation of Vue example in todomvc.com and
// doesn't show much the benefits of using F#. It's mainly intended
// to explain how to program dynamically with F# and Fable. 

// Load and open Fable.Core to get access to Fable attributes and operators
#load "../../../lib/Fabel.Core.fs"
open Fabel.Core

type Todo = {
    mutable title: string
    completed: bool
}

// Define the methods we're going to use of
// the following native JS objects
type Json =
    abstract parse: string -> obj
    abstract stringify: obj -> string
    
type LocalStorage =
    // This method can return null, so we set
    // the return type to string option
    abstract getItem: string -> string option
    abstract setItem: string * string -> unit

// This module only contains placeholders, EraseAttribute
// tells the compiler it can safely be ignored
[<Erase>]
module Lib =
    let [<Global>] window: obj = failwith "JS only"
    let [<Global>] JSON: Json = failwith "JS only"
    let [<Global>] localStorage: LocalStorage = failwith "JS only"
    // We're loading these components directly in index.html
    // so we can access them globally instead of importing
    let [<Global>] Vue: obj = failwith "JS only"
    let [<Global>] Router: obj = failwith "JS only"

module Storage =
    let private STORAGE_KEY = "todos-vuejs"

    let fetch () =
        Lib.JSON.parse(
            // As this native JS method may return null,
            // we use pattern matching to make it safer 
            match Lib.localStorage.getItem(STORAGE_KEY) with
            | Some x -> x
            | None -> "[]")
            
    let save todos =
        Lib.localStorage.setItem(STORAGE_KEY, Lib.JSON.stringify todos)
    
module Main =
    // Vue uses a lot the `this` keyword which is missing in F#,
    // so we use this to cheat the compiler
    [<Emit("this")>]
    let this: obj = failwith "JS only"

    // Notice we use point free style to define the functions
    // in the filters object
    let filters =
        let isComplete (x: Todo) = x.completed
        createObj [
            "all" ==> id
            "active" ==> Array.filter (isComplete >> not)
            "completed" ==> Array.filter (isComplete)
        ]
        
    let app =
        // Use createNew to apply the `new` keyword on Vue
        createNew Lib.Vue (
            createObj [
                "el" ==> ".todoapp"
                "data" ==>
                    createObj [
                        "todos" ==> Storage.fetch()
                        "newTodo" ==> ""
                        "editedTodo" ==> None
                        "visibility" ==> "all"
                    ]
                // Nested objects
                "watch" ==>
                    createObj [
                        "todos" ==>
                            createObj [
                                "deep" ==> true
                                "handler" ==> Storage.save
                            ]
                    ]
                "computed" ==>
                    createObj [
                        // We use the (?) operator for dynamic programming
                        // Notice we use parens if we want to pase the runtime
                        // value instead the identifier
                        "filteredTodos" ==> fun () ->
                            filters?(this?visibility) $ this?todos
                        "remaining" ==> fun () ->
                            filters?active $ this?todos |> unbox |> Seq.length
                        "allDone" ==>
                            createObj [
                                "get" ==> fun () ->
                                    this?remaining |> unbox |> (=) 0
                                // Seq methods are compatible with native JS collections
                                "set" ==> fun value ->
                                    this?todos |> unbox |> Seq.iter (fun todo ->
                                        todo?completed <- value)
                            ]
                    ]
                    "methods" ==>
                        createObj [
                            "addTodo" ==> fun () ->
                                match unbox this?newTodo with
                                | None -> ()
                                // Make the type explicit here as the compiler
                                // doesn't know the type of `newTodo` field
                                | Some (value: string) ->
                                    let todos: ResizeArray<Todo> = unbox this?todos
                                    todos.Add { title = value.Trim(); completed = false }
                                    this?newTodo <- ""
                            // As $ is forbidden in F# for identifiers,
                            // we surrorund it with quotations
                            "removeTodo" ==> fun todo ->
                                this?todos?``$remove`` $ todo
                            "editTodo" ==> fun (todo: Todo) ->
                                this?beforeEditCache <- todo.title
                                this?editedTodo <- todo
                            // Here is not necessary, but if we have more than
                            // one argument, we need to wrap the lambda in a delegate
                            // like Func<_,_,_>(fun x y -> x + y) so it's called
                            // correctly from JS (tuples won't work)
                            "doneEdit" ==> fun (todo: Todo) ->
                                match unbox this?editedTodo with
                                | None -> ()
                                | Some _ ->
                                    this?editedTodo <- None
                                    match todo.title.Trim() with
                                    | "" -> this?removeTodo $ todo |> ignore
                                    | title -> todo.title <- title
                            "cancelEdit" ==> fun (todo: Todo) ->
                                this?editedTodo <- None
                                todo.title <- unbox this?beforeEditCache
                            "removeCompleted" ==> fun () ->
                                this?todos <- filters?active $ this?todos
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
                                    // The compiler expects unit in this branch too
                                    // so we ignore the result of calling `nextTick`
                        ]
                    
            ]
        )
        

module Routes =
    let router = createNew Lib.Router ()
    
    // Notice we use ($) to call a method in JS dynamically
    // and we can pass several arguments as a tuple
    ["all"; "active"; "completed"] |> Seq.iter (fun visibility ->
        router?on $ (visibility, fun () ->
            Main.app?visibility <- visibility)
        |> ignore)
    
    router?configure $ (
        createObj [
            // Be careful not to mess up assignment (<-) and equality (=)!
            // As the types are unknown, the compiler cannot help you here
            "notfound" ==> fun () ->
                Lib.window?location?hash <- ""
                Main.app?visibility <- "all"
        ]
    )
    router?init $ ()