(**
 - title: React TodoMVC with Fable
 - tagline: Unveil the power of React and functional programming!
 - app-style: width:800px; margin:20px auto 50px auto;
 - intro: This is a port of [React TodoMVC](http://todomvc.com/examples/react/) to show how easy
   is to take advantage of the full power of [React](https://facebook.github.io/react/) in Fable apps.
   You can also compare the [F# source code](https://github.com/fable-compiler/Fable/blob/master/samples/browser/react-todomvc/react-todomvc.fsx)
   with the [original JS implementation](https://github.com/tastejs/todomvc/tree/gh-pages/examples/react)
   to see the advantages of Fable programming. There's also a port of the [React tutorial](https://github.com/fable-compiler/Fable/tree/master/samples/browser/react-tutorial),
   including an express server and hot reloading. And remember [Fable is also compatible with React Native](http://www.navision-blog.de/blog/2016/08/06/fable-react-native/) for mobile development!
*)

(**
## JavaScript bindings and helpers

Fable includes [React bindings and helpers](https://www.npmjs.com/package/fable-import-react)
to make interaction with the tool more idiomatic in F#. We will also load a couple more of
JS libraries: [classnames](https://github.com/JedWatson/classnames) and
[director](https://github.com/flatiron/director).
*)

module React.TodoMVC

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

// JS utility for conditionally joining classNames together
let classNames: obj->string = importDefault "./lib/classnames.js"

// Director is a router. Routing is the process of determining
// what code to run when a URL is requested.
let Router: obj->obj = importDefault "./lib/director.js"

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
        !!Browser.localStorage.getItem(key)
        |> Option.map (fun json -> !!JS.JSON.parse(json))

    let save key (data: 'T) =
        Browser.localStorage.setItem(key, JS.JSON.stringify data)

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
    member val onChanges: (unit->unit)[] = [||] with get, set

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

module R = Fable.Helpers.React
open R.Props

type [<Pojo>] TodoItemProps =
    { key: Guid
    ; todo: Todo
    ; editing: bool
    ; onSave: string->unit
    ; onEdit: React.SyntheticEvent->unit
    ; onDestroy: React.SyntheticEvent->unit
    ; onCancel: React.SyntheticEvent->unit
    ; onToggle: React.SyntheticEvent->unit }

type [<Pojo>] TodoItemState =
    { editText: string }

let [<Literal>] ESCAPE_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let [<Literal>] ALL_TODOS = "all"
let [<Literal>] ACTIVE_TODOS = "active"
let [<Literal>] COMPLETED_TODOS = "completed"

type TodoItem(props) =
    inherit React.Component<TodoItemProps, TodoItemState>(props)
    do base.setInitState({ editText = props.todo.title })

    let mutable editField: Browser.HTMLInputElement option = None

    member this.handleSubmit (e: React.SyntheticEvent) =
        match this.state.editText.Trim() with
        | value when value.Length > 0 ->
            this.props.onSave(value)
            this.setState { editText = value }
        | _ ->
            this.props.onDestroy(e)

    member this.handleEdit (ev: React.MouseEvent) =
        this.props.onEdit(upcast ev)
        this.setState { editText = this.props.todo.title }

    member this.handleKeyDown (e: React.KeyboardEvent) =
        match e.which with
        | ESCAPE_KEY ->
            this.setState { editText = this.props.todo.title }
            this.props.onCancel(upcast e)
        | ENTER_KEY ->
            this.handleSubmit(e)
        | _ -> ()

    member this.handleChange (e: React.SyntheticEvent) =
        if this.props.editing then
            this.setState { editText = string e.target?value }

    member this.shouldComponentUpdate (nextProps: TodoItemProps) (nextState: TodoItemState) =
        not(obj.ReferenceEquals(nextProps.todo, this.props.todo))
        || nextProps.editing <> this.props.editing
        || nextState.editText <> this.state.editText

    member this.componentDidUpdate (prevProps: TodoItemProps) =
        if not prevProps.editing && this.props.editing then
            match editField with
            | None -> ()
            | Some node ->
                node.focus()
                node.setSelectionRange(float node.value.Length, float node.value.Length)

    member this.render () =
        let className =
            classNames(
                createObj [
                    "completed" ==> this.props.todo.completed
                    "editing" ==> this.props.editing
                ])
        // The React helper defines a simple DSL to build HTML elements.
        // For more info about transforming F# unions to JS option objects:
        // https://fable-compiler.github.io/docs/interacting.html#KeyValueList-attribute
        R.li [ ClassName className ] [
            R.div [ ClassName "view" ] [
                R.input [
                    ClassName "toggle"
                    Type "checkbox"
                    Checked this.props.todo.completed
                    OnChange (fun e -> this.props.onToggle(upcast e))
                ]
                R.label [ OnDoubleClick this.handleEdit ]
                        [ R.str this.props.todo.title ]
                R.button [
                    ClassName "destroy"
                    OnClick (fun e -> this.props.onDestroy(upcast e)) ] [ ]
            ]
            R.input [
                ClassName "edit"
                Ref (fun x -> editField <- Some(x:?>Browser.HTMLInputElement))
                Value (U2.Case1 this.state.editText)
                OnBlur this.handleSubmit
                OnChange this.handleChange
                OnKeyDown this.handleKeyDown
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

type [<Pojo>] TodoFooterProps =
    { count: int
    ; completedCount: int
    ; onClearCompleted: React.MouseEvent->unit
    ; nowShowing: string }

let TodoFooter(props: TodoFooterProps) =
    let activeTodoWord =
        "item" + (if props.count = 1 then "" else "s")
    let clearButton =
        if props.completedCount > 0
        then
            R.button [
                ClassName "clear-completed"
                OnClick props.onClearCompleted
            ] [ R.str "Clear completed" ] |> Some
        else None
    let className category =
        classNames(
            createObj ["selected" ==> (props.nowShowing = category)])
    R.footer [ ClassName "footer" ] [
        R.span [ ClassName "todo-count" ] [
            R.strong [] [ props.count |> string |> R.str ]
            R.str (" " + activeTodoWord + " left")
        ]
        R.ul [ ClassName "filters" ] [
            R.li [] [
                R.a [
                    Href "#/"
                    ClassName (className ALL_TODOS)
                ] [ R.str "All" ] ]
            R.str " "
            R.li [] [
                R.a [
                    Href "#/active"
                    ClassName (className ACTIVE_TODOS)
                ] [ R.str "Active" ] ]
            R.str " "
            R.li [] [
                R.a [
                    Href "#/completed"
                    ClassName (className COMPLETED_TODOS)
                ] [ R.str "Completed" ] ]
            R.opt clearButton
        ]
    ]

(**
We finish with the `TodoApp` view. This component is the parent of the two previously
defined components, which are invoked in `render` by calling the `R.com` helper.
Notice that, among the arguments of `R.com`, we use F# object expressions to build the props.

In the [original JS implementation](https://github.com/tastejs/todomvc/blob/gh-pages/examples/react/js/app.jsx#L28)
of `componentDidMount` method, we need to take care to preserve the meaning of `this`
when passing a lambda to another object by using `bind`. Luckily, that's not something
we need to worry about in Fable :)

Note also we haven't defined an interface for the object returned by `Router`,
so we just access its `init` method with the dynamic `?` operator.
*)

type [<Pojo>] TodoAppProps =
    { model: TodoModel }

type [<Pojo>] TodoAppState =
    { nowShowing: string
    ; editing: Guid option
    ; newTodo: string }

type TodoApp(props) =
    inherit React.Component<TodoAppProps, TodoAppState>(props)
    do base.setInitState({ nowShowing=ALL_TODOS; editing=None; newTodo="" })

    member this.componentDidMount () =
        let nowShowing category =
            fun () -> this.setState({this.state with nowShowing = category})
        let router =
            Router(createObj [
                    "/" ==> nowShowing ALL_TODOS
                    "/active" ==> nowShowing ACTIVE_TODOS
                    "/completed" ==> nowShowing COMPLETED_TODOS
            ])
        router?init("/")

    member this.handleChange (ev: React.SyntheticEvent) =
        this.setState({ this.state with newTodo = string ev.target?value })

    member this.handleNewTodoKeyDown (ev: React.KeyboardEvent) =
        if ev.keyCode = ENTER_KEY then
            ev.preventDefault()
            let v = this.state.newTodo.Trim()
            if v.Length > 0 then
                this.props.model.addTodo(v)
                this.setState({ this.state with newTodo = "" })

    member this.toggleAll (ev: React.SyntheticEvent) =
        this.props.model.toggleAll(!!ev.target?``checked``)

    member this.toggle (todoToToggle) =
        this.props.model.toggle(todoToToggle)

    member this.destroy (todo) =
        this.props.model.destroy(todo)

    member this.edit (todo: Todo) =
        this.setState({ this.state with editing = Some todo.id })

    member this.save (todoToSave, text) =
        this.props.model.save(todoToSave, text)
        this.setState({ this.state with editing = None })

    member this.cancel () =
        this.setState({ this.state with editing = None })

    member this.clearCompleted () =
        this.props.model.clearCompleted()

    member this.render () =
        let todos = this.props.model.todos
        let todoItems =
            todos
            |> Seq.filter (fun todo ->
                match this.state.nowShowing with
                | ACTIVE_TODOS -> not todo.completed
                | COMPLETED_TODOS -> todo.completed
                | _ -> true)
            |> Seq.map (fun todo ->
                R.com<TodoItem,_,_>
                    { key = todo.id
                    ; todo = todo
                    ; onToggle = fun _ -> this.toggle(todo)
                    ; onDestroy = fun _ -> this.destroy(todo)
                    ; onEdit = fun _ -> this.edit(todo)
                    ; editing =
                        match this.state.editing with
                        | Some editing -> editing = todo.id
                        | None -> false
                    ; onSave = fun text -> this.save(todo, string text)
                    ; onCancel = fun _ -> this.cancel() } [])
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
                R.fn TodoFooter
                    { count = activeTodoCount
                    ; completedCount = completedCount
                    ; nowShowing = this.state.nowShowing
                    ; onClearCompleted = fun _ -> this.clearCompleted() } []
                |> Some
            else None
        let main =
            if todos.Length > 0
            then
                R.section [ ClassName "main" ] [
                    R.input [
                        ClassName "toggle-all"
                        Type "checkbox"
                        OnChange this.toggleAll
                        Checked (activeTodoCount = 0)
                    ]
                    R.ul [ ClassName "todo-list" ] todoItems
                ] |> Some
            else None
        R.div [] [
            R.header [ ClassName "header" ] [
                R.h1 [] [ R.str "todos" ]
                R.input [
                    ClassName "new-todo"
                    Placeholder "What needs to be done?"
                    Value (U2.Case1 this.state.newTodo)
                    OnKeyDown this.handleNewTodoKeyDown
                    OnChange this.handleChange
                    AutoFocus true
                ]
            ]
            R.opt main
            R.opt footer
        ]

(**
## Firing up the app

There's nothing left to do but building our model, mount our `TodoApp` view
in the DOM by using `ReactDom.render` and subscribe to the events. Happy coding!
*)

let model = TodoModel("react-todos")
let render() =
    ReactDom.render(
        R.com<TodoApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("todoapp").[0]
    )
model.subscribe(render)
render()