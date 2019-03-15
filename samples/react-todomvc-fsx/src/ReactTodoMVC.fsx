#load "../.paket/load/netstandard2.0/main.group.fsx"

#if INTERACTIVE
#r "netstandard"
#endif
(**
## JavaScript bindings and helpers

Fable includes React bindings and helpers to make interaction with the tool more idiomatic in F#.
We will also load a couple more of JS libraries: [classnames](https://github.com/JedWatson/classnames)
and [director](https://github.com/flatiron/director).
*)

open System
open Fable.Core
open Fable.Core.JsInterop
open Browser
open Browser.Types

// JS utility for conditionally joining classNames together
let classNames(classes: obj): string =
    importDefault "./lib/classnames.js"

// Director is a router. Routing is the process of determining
// what code to run when a URL is requested.
let Router(routes: obj): obj =
    importDefault "./lib/director.js"

(**
## Utility module

This module is the equivalent of [utils.js](https://github.com/tastejs/todomvc/blob/gh-pages/examples/react/js/utils.js)
in the original implementation. Note we only need a couple of functions to load
and save data from the browser local storage as things like Guid generation
(`System.Guid.NewGuid()`) or record immutable updates are built-in in F#/Fable.

> Because our `Todo` type is really simple (see below), `JSON.parse` will meet our needs.
For more complicated structures see [JSON serialization with Fable](https://fable-compiler.github.io/docs/interacting.html#JSON-serialization).
*)

module Util =
    let load<'T> key: 'T option =
        !!window.localStorage.getItem(key)
        |> Option.map (fun json -> !!JS.JSON.parse(json))

    let save key (data: 'T) =
        window.localStorage.setItem(key, JS.JSON.stringify data)

(**
## Model definiton

This is an almost direct port of [todoModel.js](https://github.com/tastejs/todomvc/blob/gh-pages/examples/react/js/todoModel.js)
which separates de logic of the app from the views. The biggest difference is with
a line of code we can define our `Todo` type and let the F# compiler statically check
we're always manipulating the structure correctly.
*)

type Todo = { id: Guid; title: string; completed: bool }

type TodoModel(key) =
    member val key = key
    member val todos: Todo[] = defaultArg (Util.load key) [||] with get, set
    member val onChanges: (unit->unit) [] = [||] with get, set

    member this.subscribe(onChange) =
        this.onChanges <- [|onChange|]

    member this.inform() =
        Util.save this.key this.todos
        this.onChanges |> Seq.iter (fun cb -> cb())

    member this.addTodo(title) =
        this.todos <-
            [|{ id=Guid.NewGuid(); title=title; completed=false }|]
            |> Array.append this.todos
        this.inform()

    member this.toggleAll(checked') =
        this.todos <- this.todos |> Array.map (fun todo ->
            { todo with completed = checked' })
        this.inform()

    member this.toggle(todoToToggle) =
        this.todos <- this.todos |> Array.map (fun todo ->
            if todo.id <> todoToToggle.id
            then todo
            else { todo with completed = (not todo.completed) })
        this.inform()

    member this.destroy(todoToDestroy) =
        this.todos <- this.todos |> Array.filter (fun todo ->
            todo.id <> todoToDestroy.id)
        this.inform()

    member this.save(todoToSave, text) =
        this.todos <- this.todos |> Array.map (fun todo ->
            if todo.id <> todoToSave.id
            then todo
            else { todo with title = text })
        this.inform()

    member this.clearCompleted() =
        this.todos <- this.todos |> Array.filter (fun todo ->
            not todo.completed)
        this.inform()

(**
## React views

We enter now in React's realm to define three views: TodoItem, TodoFooter and TodoApp.
We can use classes to define the views as explained in [React docs](https://facebook.github.io/react/docs/reusable-components.html#es6-classes),
inheriting from `React.Component` and defining a `render` method, where we can use
the DSL defined in Fable's React helper to build HTML elements in a similar fashion
as we would do with JSX.

> For convenience, we use a module alias (`R`) to shorten references to the React helper.

A big difference from JS is we can define simple models for the state and props
of custom views, either by using records or interfaces, to allow for autocompletion
and static checking, making our app much more robust than by using plain JS objects.

*)

open Fable.React
open Fable.React.Props
module R = Fable.React.Standard

type TodoItemProps =
    { key: Guid
      todo: Todo
      editing: bool
      onSave: string->unit
      onEdit: Event->unit
      onDestroy: Event->unit
      onCancel: Event->unit
      onToggle: Event->unit }

let [<Literal>] ESCAPE_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let [<Literal>] ALL_TODOS = "all"
let [<Literal>] ACTIVE_TODOS = "active"
let [<Literal>] COMPLETED_TODOS = "completed"

let mutable private editField: HTMLInputElement = Unchecked.defaultof<_>

[<Emit("undefined")>]
let undefined : (unit -> unit) option = jsNative

let TodoItem (props: TodoItemProps) =
    let editText, setState = Hooks.useState(props.todo.title)
    Hooks.useEffect((fun () ->
        editField.focus()
        editField.setSelectionRange(editField.value.Length, editField.value.Length)
        undefined), [|props.editing|])

    let handleSubmit (e: Event) =
        match editText.Trim() with
        | value when value.Length > 0 ->
            props.onSave(value)
            setState value
        | _ ->
            props.onDestroy(e)

    let handleEdit (ev: MouseEvent) =
        props.onEdit(upcast ev)
        setState props.todo.title

    let handleKeyDown (e: KeyboardEvent) =
        match e.which with
        | ESCAPE_KEY ->
            setState props.todo.title
            props.onCancel(upcast e)
        | ENTER_KEY ->
            handleSubmit(e)
        | _ -> ()

    let handleChange (e: Event) =
        if props.editing then
            e.Value |> setState


    let className =
        classNames(
            createObj [
                "completed" ==> props.todo.completed
                "editing" ==> props.editing
            ])
    // The React helper defines a simple DSL to build HTML elements.
    // For more info about transforming F# unions to JS option objects:
    // https://fable-compiler.github.io/docs/interacting.html#KeyValueList-attribute
    R.li [ Class className ] [
        R.div [ Class "view" ] [
            R.input [
                Class "toggle"
                Type "checkbox"
                Checked props.todo.completed
                OnChange (fun e -> props.onToggle(e))
            ]
            R.label [ OnDoubleClick handleEdit ]
                    [ str props.todo.title ]
            R.button [
                Class "destroy"
                OnClick (fun e -> props.onDestroy(upcast e)) ] [ ]
        ]
        R.input [
            Class "edit"
            Ref (fun x -> editField <- x:?>_)
            Value editText
            OnBlur handleSubmit
            OnChange handleChange
            OnKeyDown handleKeyDown
        ]
    ]

(**
The next view is `TodoFooter`. This component just presents some buttons below
the Todo list to filter by or change the `completed` property of the Todos.

Same as `TodoItem`, notice the component subscribes to some events (like `OnClick`)
but instead of containing the logic to react to the event it just runs a callback
received from its parent through the `props` object. Remember the state of React
components cannot be directly updated, so this is a way to transmit the event to
the parent and let it re-render the subtree if necessary.
*)

type TodoFooterProps =
    { count: int
      completedCount: int
      onClearCompleted: MouseEvent->unit
      nowShowing: string }

let TodoFooter(props: TodoFooterProps) =
    let activeTodoWord =
        "item" + (if props.count = 1 then "" else "s")
    let clearButton =
        if props.completedCount > 0
        then
            R.button [
                Class "clear-completed"
                OnClick props.onClearCompleted
            ] [ str "Clear completed" ] |> Some
        else None
    let className category =
        classNames(
            createObj ["selected" ==> (props.nowShowing = category)])
    R.footer [ Class "footer" ] [
        R.span [ Class "todo-count" ] [
            strong [] [ props.count |> string |> str ]
            str (" " + activeTodoWord + " left")
        ]
        R.ul [ Class "filters" ] [
            R.li [] [
                R.a [
                    Href "#/"
                    Class (className ALL_TODOS)
                ] [ str "All" ] ]
            str " "
            R.li [] [
                R.a [
                    Href "#/active"
                    Class (className ACTIVE_TODOS)
                ] [ str "Active" ] ]
            str " "
            R.li [] [
                R.a [
                    Href "#/completed"
                    Class (className COMPLETED_TODOS)
                ] [ str "Completed" ] ]
            ofOption clearButton
        ]
    ]

(**
We finish with the `TodoApp` view. This component is the parent of the two previously
defined components, which are invoked in `render` by calling the `R.ofType` helper.
Notice that, among the arguments of `R.ofType`, we use F# object expressions to build the props.

In the [original JS implementation](https://github.com/tastejs/todomvc/blob/gh-pages/examples/react/js/app.jsx#L28)
of `componentDidMount` method, we need to take care to preserve the meaning of `this`
when passing a lambda to another object by using `bind`. Luckily, that's not something
we need to worry about in Fable :)

Note also we haven't defined an interface for the object returned by `Router`,
so we just access its `init` method with the dynamic `?` operator.
*)

type TodoAppProps =
    { model: TodoModel }

type TodoAppState =
    { nowShowing: string
      editing: Guid option
      newTodo: string }

type TodoApp(props) =
    inherit Component<TodoAppProps, TodoAppState>(props)
    do base.setInitState({ nowShowing=ALL_TODOS; editing=None; newTodo="" })

    override this.componentDidMount () =
        let nowShowing category =
            fun () -> this.setState(fun s _ -> {s with nowShowing = category})
        let router =
            Router(createObj [
                    "/" ==> nowShowing ALL_TODOS
                    "/active" ==> nowShowing ACTIVE_TODOS
                    "/completed" ==> nowShowing COMPLETED_TODOS
            ])
        router?init("/") |> ignore

    member this.handleChange (ev: Event) =
        let newTodo = ev.Value
        this.setState(fun s _ -> { s with newTodo = newTodo })

    member this.handleNewTodoKeyDown (ev: KeyboardEvent) =
        if ev.keyCode = ENTER_KEY then
            ev.preventDefault()
            let v = this.state.newTodo.Trim()
            if v.Length > 0 then
                this.props.model.addTodo(v)
                this.setState(fun s _ -> { s with newTodo = "" })

    member this.toggleAll (ev: Event) =
        this.props.model.toggleAll(ev.Checked)

    member this.toggle (todoToToggle) =
        this.props.model.toggle(todoToToggle)

    member this.destroy (todo) =
        this.props.model.destroy(todo)

    member this.edit (todo: Todo) =
        this.setState(fun s _ -> { s with editing = Some todo.id })

    member this.save (todoToSave, text) =
        this.props.model.save(todoToSave, text)
        this.setState(fun s _ -> { s with editing = None })

    member this.cancel () =
        this.setState(fun s _ -> { s with editing = None })

    member this.clearCompleted () =
        this.props.model.clearCompleted()

    override this.render () =
        let todos = this.props.model.todos
        let todoItems =
            todos
            |> Seq.filter (fun todo ->
                match this.state.nowShowing with
                | ACTIVE_TODOS -> not todo.completed
                | COMPLETED_TODOS -> todo.completed
                | _ -> true)
            |> Seq.map (fun todo ->
                ofFunction TodoItem
                    { key = todo.id
                      todo = todo
                      onToggle = fun _ -> this.toggle(todo)
                      onDestroy = fun _ -> this.destroy(todo)
                      onEdit = fun _ -> this.edit(todo)
                      editing =
                        match this.state.editing with
                        | Some editing -> editing = todo.id
                        | None -> false
                      onSave = fun text -> this.save(todo, string text)
                      onCancel = fun _ -> this.cancel() } [])
                |> Seq.toList
        let activeTodoCount =
            todos |> Array.fold (fun accum todo ->
                if todo.completed then accum else accum + 1
            ) 0
        let completedCount =
            todos.Length - activeTodoCount
        let footer =
            if activeTodoCount > 0 || completedCount > 0
            then
                ofFunction TodoFooter
                    { count = activeTodoCount
                      completedCount = completedCount
                      nowShowing = this.state.nowShowing
                      onClearCompleted = fun _ -> this.clearCompleted() } []
                |> Some
            else None
        let main =
            if todos.Length > 0
            then
                R.section [ Class "main" ] [
                    R.input [
                        Class "toggle-all"
                        Type "checkbox"
                        OnChange this.toggleAll
                        Checked (activeTodoCount = 0)
                    ]
                    R.ul [ Class "todo-list" ] todoItems
                ] |> Some
            else None
        R.div [] [
            R.header [ Class "header" ] [
                R.h1 [] [ str "todos" ]
                R.input [
                    Class "new-todo"
                    Placeholder "What needs to be done?"
                    Value this.state.newTodo
                    OnKeyDown this.handleNewTodoKeyDown
                    OnChange this.handleChange
                    AutoFocus true
                ]
            ]
            ofOption main
            ofOption footer
        ]

(**
## Firing up the app

There's nothing left to do but building our model, mount our `TodoApp` view
in the DOM by using `ReactDom.render` and subscribe to the events. Happy coding!
*)

let model = TodoModel("react-todos")
let render() =
    Fable.ReactDom.render(
        ofType<TodoApp,_,_> { model = model } [],
        document.getElementsByClassName("todoapp").[0])

model.subscribe(render)
render()