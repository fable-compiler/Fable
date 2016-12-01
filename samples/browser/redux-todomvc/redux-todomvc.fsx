(**
 - title: React-Redux TodoMVC with Fable
 - tagline: Use Redux development tools with Fable
 - app-style: width:800px; margin:20px auto 50px auto;
 - require-paths: `'react': 'lib/react/react-with-addons', 'react-dom': 'lib/react/react-dom', 'redux': 'lib/redux/redux'`
 - intro: This is a port of [React-Redux TodoMVC](https://github.com/reactjs/redux/tree/master/examples/todomvc) to show how easy
   is to take advantage of the full power of [React](https://facebook.github.io/react/) and [Redux](http://redux.js.org/) in Fable apps.
   You can also compare the [F# source code](https://github.com/fable-compiler/Fable/blob/master/samples/browser/react-redux/react-redux.fsx)
   with the [original JS implementation](https://github.com/reactjs/redux/tree/master/examples/todomvc)
   to see the advantages of Fable programming. And you can install the [Redux DevTools](http://zalmoxisus.github.io/redux-devtools-extension/)
   to get the full debugging experience of modern JS development with Fable!
*)

(**
## JavaScript bindings

Fable includes [React bindings and helpers](https://www.npmjs.com/package/fable-import-react)
to make interaction with the tool more idiomatic in F#.
*)

#r "../../node_modules/fable-core/Fable.Core.dll"
#r "../../node_modules/fable-react/Fable.React.dll"

(**
## Redux helper

Fable's support for Redux is experimental and there're no bindings available.
In order to use Redux in you Fable apps, just copy the code of the module below
and be sure to add Redux as a JS dependency.
*)

open System
open Fable.Core
open Fable.Core.JsInterop

module Redux =
    open System
    open Fable.Import
    open Fable.Core
    open Fable.Core.JsInterop

    type IStore<'TState, 'TAction> =
        abstract dispatch: 'TAction->unit
        abstract subscribe: (unit->unit)->unit
        abstract getState: unit->'TState

    let private createStore_: JsFunc = import "createStore" "redux"

    let createStore (reducer: 'TState->'TAction->'TState) (initState: 'TState): IStore<'TState, 'TAction> =
        // Check if the action is a lifecycle event dispatched by Redux before applying the reducer
        let jsReducer = JsFunc2(fun _this state action ->
            match !!action?``type``: obj with
            | :?string as s when s.StartsWith "@@" -> state
            | _ -> reducer state action)
        match !!Browser.window?devToolsExtension: JsFunc with
        | null -> !!createStore_.Invoke(jsReducer, initState)
        | ext -> !!createStore_.Invoke(jsReducer, initState, ext.Invoke())

(**
## Domain models

Open the necessary namespaces, define our domain models (`Todo`, `TodoAction`, `TodoFilter`)
and some utilities (key codes, `classNames`).
*)

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

module R = Fable.Helpers.React
open R.Props

type Todo = { Text: string; Completed: bool; Id: int }

[<Pojo>]
type TodoAction =
    | AddTodo of text:string
    | DeleteTodo of id:int
    | EditTodo of id:int * text:string
    | CompleteTodo of id:int
    | CompleteAll
    | ClearCompleted

type TodoFilter =
    | ShowAll = 0
    | ShowActive = 1
    | ShowCompleted = 2

let [<Literal>] ESCAPE_KEY = 27.
let [<Literal>] ENTER_KEY = 13.

let classNames =
    List.choose (fun (txt,add) -> if add then Some txt else None)
    >> String.concat " "

(**
## React components

Define React components in the same way as explained in [React guide](https://facebook.github.io/react/docs/reusable-components.html#es6-classes)
but with statically checked `props` and `state` types and the DSL from the [Fable React helper](https://www.npmjs.com/package/fable-import-react).
Check the `render` method of each component and the [Fable React TodoMVC sample](https://fable-compiler.github.io/samples/react-todomvc/index.html#/) for details,
or scroll down to see how to subscribe React components to the Redux store.
*)

[<Pojo>]
type TodoTextInputProps =
    { OnSave: string->unit
    ; Text: string option
    ; Placeholder: string
    ; Editing: bool
    ; NewTodo: bool }

[<Pojo>]
type TodoTextInputState = { Text: string }

type TodoTextInput(props) =
    inherit React.Component<TodoTextInputProps, TodoTextInputState>(props)
    do base.setInitState({ Text = defaultArg props.Text "" })

    member this.HandleSubmit(e: React.KeyboardEvent) =
        if e.which = ENTER_KEY then
            let text = (unbox<string> e.target?value).Trim()
            this.props.OnSave(text)
            if this.props.NewTodo then
                this.setState({ Text = "" })

    member this.HandleChange(e: React.SyntheticEvent) =
        this.setState({ Text=unbox e.target?value })

    member this.HandleBlur(e: React.SyntheticEvent) =
        if not this.props.NewTodo then
            this.props.OnSave(unbox e.target?value)

    member this.render() =
        R.input [
            ClassName(
                classNames [
                    "edit", this.props.Editing
                    "new-todo", this.props.NewTodo
                ])
            Type "text"
            OnBlur this.HandleBlur
            OnChange this.HandleChange
            OnKeyDown this.HandleSubmit
            AutoFocus (this.state.Text.Length > 0)
            Placeholder this.props.Placeholder
        ] []

[<Pojo>]
type TodoItemProps =
    { Todo: Todo
    ; EditTodo: int * string -> unit
    ; DeleteTodo: int -> unit
    ; CompleteTodo: int -> unit }

[<Pojo>]
type TodoItemState = { Editing: bool }

type TodoItem(props) =
    inherit React.Component<TodoItemProps, TodoItemState>(props)
    do base.setInitState({ Editing = false })

    member this.HandleDoubleClick(_) =
        this.setState({ Editing = true })

    member this.HandleSave(id, text: string) =
        if text.Length = 0
        then this.props.DeleteTodo(id)
        else this.props.EditTodo(id, text)
        this.setState({ Editing = false })

    member this.render() =
        let element =
            if this.state.Editing
            then R.com<TodoTextInput,_,_>
                    { OnSave = fun (text: string) ->
                        this.HandleSave(this.props.Todo.Id, text)
                    ; Editing = this.state.Editing
                    ; Text = Some this.props.Todo.Text
                    ; Placeholder = ""
                    ; NewTodo = false } []
            else R.div [ClassName "view"] [
                    R.input [
                        ClassName "toggle"
                        Type "checkbox"
                        Checked this.props.Todo.Completed
                        OnChange (fun _ ->
                            this.props.CompleteTodo(this.props.Todo.Id))
                    ] []
                    R.label [OnDoubleClick this.HandleDoubleClick]
                            [R.str this.props.Todo.Text]
                    R.div [
                        ClassName "destroy"
                        OnClick (fun _ ->
                            this.props.DeleteTodo(this.props.Todo.Id))
                    ] []
                ]
        R.li [ClassName(
                classNames [
                    "completed", this.props.Todo.Completed
                    "editing", this.state.Editing])]
             [element]

[<Pojo>]
type HeaderProps = { AddTodo: string->unit }

let Header (props: HeaderProps) =
    R.header [ClassName "header"] [
        R.h1 [] [R.str "todos"]
        R.com<TodoTextInput,_,_>
            { OnSave = fun (text: string) ->
                if text.Length <> 0 then
                    props.AddTodo text
            ; Placeholder = "What needs to be done?"
            ; NewTodo = true
            ; Text = None
            ; Editing = false } []
    ]

[<Pojo>]
type FooterProps =
    { ActiveCount: int
    ; CompletedCount: int
    ; Filter: TodoFilter
    ; OnShow: TodoFilter->unit
    ; OnClearCompleted: React.MouseEvent->unit }

let Footer =
    let filterTitles =
        dict [
            TodoFilter.ShowAll, "All"
            TodoFilter.ShowActive, "Active"
            TodoFilter.ShowCompleted, "Completed"
        ]
    let renderTodoCount activeCount =
        R.span [ClassName "todo-count"] [
            R.str(sprintf "%s item%s left"
                (if activeCount > 0 then string activeCount else "No")
                (if activeCount <> 1 then "s" else ""))
        ]
    let renderFilterLink filter selectedFilter onShow =
        R.a [
            ClassName (classNames ["selected", filter = selectedFilter])
            Style [unbox("cursor", "pointer")]
            OnClick (fun _ -> onShow filter)
        ] [R.str filterTitles.[filter]]
    let renderClearButton completedCount onClearCompleted =
        if completedCount > 0
        then R.button [
                ClassName "clear-completed"
                OnClick onClearCompleted
             ] [R.str "Clear completed"] |> Some
        else None
    fun (props: FooterProps) ->
        let listItems =
            [ TodoFilter.ShowAll
              TodoFilter.ShowActive
              TodoFilter.ShowCompleted ]
            |> List.map (fun filter ->
                [renderFilterLink filter props.Filter props.OnShow]
                |> R.li [Key (string filter)])
        R.footer [ClassName "footer"] [
            renderTodoCount props.ActiveCount
            R.ul [ClassName "filters"] listItems
            R.opt(renderClearButton props.CompletedCount props.OnClearCompleted)
        ]

type [<Pojo>] MainSectionProps = { Todos: Todo[]; Dispatch: TodoAction->unit }
type [<Pojo>] MainSectionState = { Filter: TodoFilter }

type MainSection(props) =
    inherit React.Component<MainSectionProps, MainSectionState>(props)
    let todoFilters =
        dict [
            TodoFilter.ShowAll, fun _ -> true
            TodoFilter.ShowActive, fun (todo: Todo) -> not todo.Completed
            TodoFilter.ShowCompleted, fun todo -> todo.Completed
        ]
    do base.setInitState({ Filter = TodoFilter.ShowAll })

    member this.HandleClearCompleted() =
        this.props.Dispatch(ClearCompleted)

    member this.HandleShow(filter) =
        this.setState({ Filter = filter })

    member this.renderToggleAll(completedCount) =
        if this.props.Todos.Length > 0
        then R.input [
                ClassName "toggle-all"
                Type "checkbox"
                Checked (completedCount = this.props.Todos.Length)
                OnChange (fun _ -> this.props.Dispatch(CompleteAll))
             ] [] |> Some
        else None

    member this.renderFooter(completedCount) =
        if this.props.Todos.Length > 0
        then R.fn Footer
                { ActiveCount = this.props.Todos.Length - completedCount
                ; CompletedCount = completedCount
                ; Filter = this.state.Filter
                ; OnShow = fun filter ->
                    this.HandleShow filter
                ; OnClearCompleted = fun _ ->
                    this.HandleClearCompleted() } [] |> Some
        else None

    member this.render() =
        let filteredTodos =
            this.props.Todos
            |> Array.filter todoFilters.[this.state.Filter]
            |> Array.toList
        let completedCount =
            (0, this.props.Todos) ||> Array.fold (fun count todo ->
                if todo.Completed then count + 1 else count)
        R.section [ClassName "main"] [
            R.opt(this.renderToggleAll completedCount)
            R.ul [ClassName "todo-list"]
                (filteredTodos
                |> List.map (fun todo ->
                    R.com<TodoItem,_,_>
                        { Todo = todo
                        ; EditTodo = fun (id, text) ->
                            this.props.Dispatch(EditTodo(id, text))
                        ; DeleteTodo = fun id ->
                            this.props.Dispatch(DeleteTodo id)
                        ; CompleteTodo = fun id ->
                            this.props.Dispatch(CompleteTodo id) } []))
            R.opt(this.renderFooter completedCount)
        ]

(**
## Subscription to Redux store

The component on top of our UI hierarchy subscribes to the Redux store
and will automatically invalidate children rendering as needed. React
checks when DOM updates are actually necessary, so we don't need to worry
about the performance hit of too frequent updates.
*)

[<Pojo>]
type AppProps = { Store: Redux.IStore<Todo[], TodoAction> }

type App(p) as this =
    inherit React.Component<AppProps, MainSectionProps>(p)
    let getState() = { Todos=this.props.Store.getState(); Dispatch=this.props.Store.dispatch }
    do base.setInitState(getState())
    do this.props.Store.subscribe(getState >> this.setState)

    member this.render() =
        R.div [] [
            R.fn Header { AddTodo = AddTodo >> this.props.Store.dispatch } []
            R.com<MainSection,_,_> this.state []
        ]

(**
## Reducer

The reducer is a single function (which can be composed of other smaller function)
with the responsibility of updating the state in reaction to the actions
dispatched to the Redux store. F# union types and pattern matching makes
it really easy to identify and extract the data from the received actions
in a type-safe manner. The compiler will even warn us if we forget to handle
any of the possible `TodoAction` cases.
*)

let reducer (state: Todo[]) = function
    | AddTodo text ->
        let id =
            (-1, state)
            ||> Array.fold(fun id todo -> max id todo.Id)
            |> (+) 1
        state
        |> Array.append [|{Id=id; Completed=false; Text=text}|]
    | DeleteTodo id ->
        state
        |> Array.filter(fun todo -> todo.Id <> id)
    | EditTodo(id, text) ->
        state
        |> Array.map(fun todo ->
            if todo.Id = id
            then { todo with Text=text }
            else todo)
    | CompleteTodo id ->
        state
        |> Array.map(fun todo ->
            if todo.Id = id
            then { todo with Completed=not todo.Completed }
            else todo)
    | CompleteAll ->
        let areAllMarked =
            state |> Array.forall(fun todo -> todo.Completed)
        state
        |> Array.map(fun todo -> { todo with Completed=not areAllMarked})
    | ClearCompleted ->
        state
        |> Array.filter(fun todo -> not todo.Completed)

(**
## Firing up the app

There's nothing left to do but create the Redux store with the reducer
and the initial state, pass it to our `App` component, and mount it
onto the DOM by using `ReactDom.render`. Happy coding!
*)

let start() =
    let store =
        { Text="Use Fable + React + Redux"; Completed=false; Id=0}
        |> Array.singleton
        |> Redux.createStore reducer

    ReactDom.render(
        R.com<App,_,_> { Store=store } [],
        Browser.document.getElementsByClassName("todoapp").[0]
    ) |> ignore

start()
