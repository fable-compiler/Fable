(**
## JavaScript bindings

Fable includes [React bindings and helpers](https://www.npmjs.com/package/fable-import-react)
to make interaction with the tool more idiomatic in F#.
*)

#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "node_modules/fable-import-react/Fable.Helpers.React.fs"

(**
## Redux helper

Fable's support for Redux is experimental and there're no bindings available.
In order to use Redux in you Fable apps, just copy the code of the module below
and be sure to add Redux as a JS dependency.
*)

module Redux =
    open System
    open Fable.Import
    open Fable.Core
    open Fable.Core.JsInterop

    type IStore<'TState, 'TAction> = interface end

    let [<Import("createStore","Redux")>] private createStore' = obj()

    let createStore (reducer: 'TState->'TAction->'TState) (initState: 'TState) =
        // Check if the action is a union type before applying the reducer
        let reducer = fun state action ->
            match box action?Case with
            | :? string -> reducer state action
            | _ -> state
        match unbox Browser.window?devToolsExtension with
        | Some ext -> createStore'$(Func<_,_,_> reducer, initState, ext$())
        | None -> createStore'$(Func<_,_,_> reducer, initState)
        |> unbox<IStore<'TState, 'TAction>>

    let dispatch (store: IStore<'TState, 'TAction>) (x: 'TAction) =
        let x = toPlainJsObj x
        x?``type`` <- x?Case
        store?dispatch(x) |> ignore

    let inline subscribe (store: IStore<'TState, 'TAction>) (f: unit->unit) =
        store?subscribe(f) |> ignore

    let inline getState (store: IStore<'TState, 'TAction>) =
        store?getState() |> unbox<'TState>

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

type TodoTextInputProps =
    abstract OnSave: string->unit
    abstract Text: string option
    abstract Placeholder: string
    abstract Editing: bool
    abstract NewTodo: bool

type TodoTextInputState = { Text: string }

type TodoTextInput(props, ctx) as this =
    inherit React.Component<TodoTextInputProps, TodoTextInputState>(props, ctx)
    do this.state <- { Text = defaultArg props.Text "" } 

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

type TodoItemProps =
    abstract Todo: Todo
    abstract EditTodo: int * string -> unit
    abstract DeleteTodo: int -> unit
    abstract CompleteTodo: int -> unit

type TodoItemState = { Editing: bool }

type TodoItem(props, ctx) as this =
    inherit React.Component<TodoItemProps, TodoItemState>(props, ctx)
    do this.state <- { Editing = false } 

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
                    { new TodoTextInputProps with
                        member __.OnSave(text: string) =
                            this.HandleSave(this.props.Todo.Id, text)
                        member __.Editing = this.state.Editing
                        member __.Text = Some this.props.Todo.Text
                        member __.Placeholder = ""
                        member __.NewTodo = false } []
            else R.div [ClassName "view"] [
                    R.input [
                        ClassName "toggle"
                        Type "checkbox"
                        Checked this.props.Todo.Completed
                        OnChange (fun _ ->
                            this.props.CompleteTodo(this.props.Todo.Id))
                    ] []
                    R.label [OnDoubleClick this.HandleDoubleClick]
                            [unbox this.props.Todo.Text]
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

type HeaderProps = { AddTodo: string->unit }

let Header (props: HeaderProps) =
    R.header [ClassName "header"] [
        R.h1 [] [unbox "todos"]
        R.com<TodoTextInput,_,_>
            { new TodoTextInputProps with
                member __.OnSave(text: string) =
                    if text.Length <> 0 then
                        props.AddTodo text
                member __.Placeholder = "What needs to be done?"
                member __.NewTodo = true
                member __.Text = None
                member __.Editing = false } []
    ]

type FooterProps =
    abstract ActiveCount: int
    abstract CompletedCount: int
    abstract Filter: TodoFilter
    abstract OnShow: TodoFilter->unit
    abstract OnClearCompleted: React.SyntheticEvent->unit

let Footer =
    let filterTitles =
        dict [
            TodoFilter.ShowAll, "All"
            TodoFilter.ShowActive, "Active"
            TodoFilter.ShowCompleted, "Completed"
        ]
    let renderTodoCount activeCount =
        R.span [ClassName "todo-count"] [
            unbox(sprintf "%s item%s left"
                (if activeCount > 0 then string activeCount else "No")
                (if activeCount <> 1 then "s" else ""))
        ]
    let renderFilterLink filter selectedFilter onShow =
        R.a [
            ClassName (classNames ["selected", filter = selectedFilter])
            Style [unbox("cursor", "pointer")]
            OnClick (fun _ -> onShow filter)
        ] [unbox filterTitles.[filter]]
    let renderClearButton completedCount onClearCompleted =
        if completedCount > 0
        then R.button [
                ClassName "clear-completed"
                OnClick onClearCompleted
             ] [unbox "Clear completed"] |> Some
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
            // We can pass option types as React children but we
            // need to use `unbox` to appease the F# compiler
            unbox(renderClearButton props.CompletedCount props.OnClearCompleted)
        ]

type MainSectionProps = { Todos: Todo[]; Dispatch: TodoAction->unit }
type MainSectionState = { Filter: TodoFilter }

type MainSection(props, ctx) as this =
    inherit React.Component<MainSectionProps, MainSectionState>(props, ctx)
    let todoFilters =
        dict [
            TodoFilter.ShowAll, fun _ -> true
            TodoFilter.ShowActive, fun (todo: Todo) -> not todo.Completed
            TodoFilter.ShowCompleted, fun todo -> todo.Completed
        ]
    do this.state <- { Filter = TodoFilter.ShowAll } 

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
                { new FooterProps with
                    member __.ActiveCount =
                        this.props.Todos.Length - completedCount
                    member __.CompletedCount = completedCount
                    member __.Filter = this.state.Filter
                    member __.OnShow filter = this.HandleShow filter
                    member __.OnClearCompleted _ =
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
            unbox(this.renderToggleAll completedCount)
            R.ul [ClassName "todo-list"]
                (filteredTodos
                |> List.map (fun todo ->
                    R.com<TodoItem,_,_>
                        { new TodoItemProps with
                            member __.Todo = todo
                            member __.EditTodo(id, text) =
                                this.props.Dispatch(EditTodo(id, text))
                            member __.DeleteTodo(id) =
                                this.props.Dispatch(DeleteTodo id)
                            member __.CompleteTodo(id) =
                                this.props.Dispatch(CompleteTodo id) } []))
            unbox(this.renderFooter completedCount)
        ]

(**
## Subscription to Redux store

The component on top of our UI hierarchy subscribes to the Redux store
and will automatically invalidate children rendering as needed. React
checks when DOM updates are actually necessary, so we don't need to worry
about the performance hit of too frequent updates.
*)

type AppProps = { Store: Redux.IStore<Todo[], TodoAction> }

type App(props, ctx) as this =
    inherit React.Component<AppProps, MainSectionProps>(props, ctx)
    let dispatch = Redux.dispatch props.Store
    let getState() = { Todos=Redux.getState props.Store; Dispatch=dispatch }
    do this.state <- getState()
    do Redux.subscribe props.Store (getState >> this.setState)

    member this.render() =
        R.div [] [
            R.fn Header { AddTodo = AddTodo >> dispatch } []
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

let store =
    { Text="Use Fable + React + Redux"; Completed=false; Id=0}
    |> Array.singleton
    |> Redux.createStore reducer
