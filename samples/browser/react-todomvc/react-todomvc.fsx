
// Load Fable.Core and bindings to JS global objects
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "node_modules/fable-import-react/Fable.Helpers.React.fs"

open System
open Fable.Core
open Fable.Import

type Todo = { id: string; title: string; completed: bool }

type Util =
    // JS utility for conditionally joining classNames together
    // See https://github.com/JedWatson/classnames
    [<Global>] static member classNames(o: obj): string = failwith "JS only"

    // Director is a router. Routing is the process of determining what code to run when a URL is requested.
    // See https://github.com/flatiron/director
    [<Global>] static member Router(o: obj): obj = failwith "JS only"

    static member uuid () =
        let makeRandom =
            let rnd = Random()
            fun () -> rnd.Next(16)
        ("", seq { 0..31 })
        ||> Seq.fold (fun uuid i ->
            let hyphen =
                match i with 8 | 12 | 16 | 20 -> "-" | _ -> ""
            let random =
                match i with
                 | 12 -> 4
                 | 16 -> makeRandom() &&& 3 ||| 8
                 | _ -> makeRandom()
            sprintf "%s%s%s" uuid hyphen (Convert.ToString(random, 16)))

    static member pluralize count word =
        if count = 1 then word else word + "s"

    static member store
        with get ns: Todo[] =
            match Browser.localStorage.getItem(ns) |> unbox with
            | Some data -> JS.JSON.parse(data) |> unbox
            | None -> [||]
        and set ns (data: Todo[]) =
            Browser.localStorage.setItem(ns, JS.JSON.stringify data)

type TodoModel(key) =
    member val key = key
    member val todos: Todo[] = Util.store(key) with get, set
    member val onChanges: (unit->unit)[] = [||] with get, set

    member this.subscribe (onChange) =
        this.onChanges <- [|onChange|]

    member this.inform () =
        Util.store(this.key) <- this.todos
        this.onChanges |> Seq.iter (fun cb -> cb())

    member this.addTodo (title) =
        this.todos <- [|
            yield! this.todos
            yield { id=Util.uuid(); title=title; completed=false }
        |]
        this.inform()

    member this.toggleAll (checked') =
        this.todos <- this.todos |> Array.map (fun todo ->
            { todo with completed = checked' })
        this.inform()

    member this.toggle (todoToToggle) =
        this.todos <- this.todos |> Array.map (fun todo ->
            if todo.id <> todoToToggle.id
            then todo
            else { todo with completed = (not todo.completed) })
        this.inform()

    member this.destroy (todo) =
        this.todos <- this.todos |> Array.filter ((<>) todo)
        this.inform()

    member this.save (todoToSave, text) =
        this.todos <- this.todos |> Array.map (fun todo ->
            if todo.id <> todoToSave.id
            then todo
            else { todo with title = text })
        this.inform()

    member this.clearCompleted () =
        this.todos <- this.todos |> Array.filter (fun todo -> not todo.completed)
        this.inform()

module R = Fable.Helpers.React
open R.Props

type TodoItemState = { editText: string }
type TodoItemProps =
    abstract key: string 
    abstract todo: Todo 
    abstract editing: bool
    abstract onSave: obj->unit
    abstract onEdit: obj->unit
    abstract onDestroy: obj->unit
    abstract onCancel: obj->unit
    abstract onToggle: obj->unit

let [<Literal>] ESCAPE_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let [<Literal>] ALL_TODOS = "all"
let [<Literal>] ACTIVE_TODOS = "active"
let [<Literal>] COMPLETED_TODOS = "completed"

type TodoItem(props) =
    inherit R.Component<TodoItemProps, TodoItemState>(
                props, { editText = props.todo.title })

    let mutable editField: obj option = None

    member this.handleSubmit (e: React.SyntheticEvent) =
        match this.state.editText.Trim() with
        | value when value.Length > 0 ->
            this.props.onSave(value)
            this.setState { editText = value }
        | _ ->
            this.props.onDestroy(e)

    member this.handleEdit (ev: React.MouseEvent) =
        this.props.onEdit(ev)
        this.setState { editText = this.props.todo.title }

    member this.handleKeyDown (e: React.KeyboardEvent) =
        match e.which with
        | ESCAPE_KEY ->
            this.setState { editText = this.props.todo.title }
            this.props.onCancel(e)
        | ENTER_KEY ->
            this.handleSubmit(e)
        | _ -> ()

    member this.handleChange (e: React.SyntheticEvent) =
        if this.props.editing then
            this.setState { editText = string e.target?value }

    member this.shouldComponentUpdate (nextProps: TodoItemProps) (nextState: TodoItemState) =
        nextProps.todo <> this.props.todo
        || nextProps.editing <> this.props.editing
        || nextState.editText <> this.state.editText

    member this.componentDidUpdate (prevProps: TodoItemProps) =
        if not prevProps.editing && this.props.editing then
            let node =
                ReactDom.findDOMNode(unbox editField.Value)
                :?> Browser.HTMLInputElement
            node.focus()
            node.setSelectionRange(float node.value.Length, float node.value.Length)

    member this.render () =
        let className =
            Util.classNames(
                createObj [
                    "completed" ==> this.props.todo.completed
                    "editing" ==> this.props.editing
                ])
        R.li [ ClassName className ] [
            R.div [ ClassName "view" ] [
                R.input [
                    ClassName "toggle"
                    Type "checkbox"
                    Checked this.props.todo.completed
                    OnChange this.props.onToggle  
                ] []
                R.label [ OnDoubleClick this.handleEdit ]
                        [ unbox this.props.todo.title ]
                R.button [
                    ClassName "destroy"
                    OnClick this.props.onDestroy ] [ ]
            ]
            R.input [
                ClassName "edit"
                Ref (fun x -> editField <- Some x)
                Value (U2.Case1 this.state.editText)
                OnBlur this.handleSubmit
                OnChange this.handleChange
                OnKeyDown this.handleKeyDown
            ] []
        ]

type TodoFooterProps =
    abstract count: int
    abstract completedCount: int
    abstract onClearCompleted: obj->unit
    abstract nowShowing: string

type TodoFooter(props) =
    inherit React.Component<TodoFooterProps,obj>(props)
    member this.render () =
        let activeTodoWord = Util.pluralize this.props.count "item"
        let clearButton =
            if this.props.completedCount > 0 then
                R.button [
                    ClassName "clear-completed"
                    OnClick this.props.onClearCompleted
                ] [ unbox "Clear completed" ] |> Some
            else None
        let className category =
            Util.classNames(
                createObj ["selected" ==> (this.props.nowShowing = category)])
        R.footer [ ClassName "footer" ] [
            R.span [ ClassName "todo-count" ] [
                R.strong [] [ unbox this.props.count ]
                unbox (" " + activeTodoWord + " left")
            ]
            R.ul [ ClassName "filters" ] [
                R.li [] [
                    R.a [
                        Href "#/"
                        ClassName (className ALL_TODOS)
                    ] [ unbox "All" ] ]
                unbox " "
                R.li [] [
                    R.a [
                        Href "#/active"
                        ClassName (className ACTIVE_TODOS)
                    ] [ unbox "Active" ] ]
                unbox " "
                R.li [] [
                    R.a [
                        Href "#/completed"
                        ClassName (className COMPLETED_TODOS)
                    ] [ unbox "Completed" ] ]
                clearButton.Value
            ]
        ]

type TodoAppProps = { model: TodoModel }
type TodoAppState = { nowShowing: string; editing: string option; newTodo: string }

type TodoApp(props) =
    inherit R.Component<TodoAppProps, TodoAppState>(
                props, { nowShowing=ALL_TODOS; editing=None; newTodo="" })

    member this.componentDidMount () =
        let nowShowing category =
            fun () -> this.setState({this.state with nowShowing = category})
        let router =
            Util.Router(
                createObj [
                    "/" ==> nowShowing ALL_TODOS
                    "/active" ==> nowShowing ACTIVE_TODOS
                    "/completed" ==> nowShowing COMPLETED_TODOS
                ]
            )
        router?init$("/")

    member this.handleChange (ev: React.SyntheticEvent) =
        this.setState({ this.state with newTodo = unbox ev.target?value })

    member this.handleNewTodoKeyDown (ev: React.KeyboardEvent) =
        if ev.keyCode = ENTER_KEY then
            ev.preventDefault()
            let v = this.state.newTodo.Trim()
            if v.Length > 0 then
                this.props.model.addTodo(v)
                this.setState({ this.state with newTodo = "" })

    member this.toggleAll (ev: React.SyntheticEvent) =
        this.props.model.toggleAll(unbox ev.target?``checked``)

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
            todos |> Array.filter (fun todo ->
                match this.state.nowShowing with
                | ACTIVE_TODOS -> not todo.completed
                | COMPLETED_TODOS -> todo.completed
                | _ -> true)
            |> Array.map (fun todo ->
                R.com<TodoItem,_,_>(
                    { new TodoItemProps with
                        member __.key = todo.id
                        member __.todo = todo
                        member __.onToggle _ = this.toggle(todo)
                        member __.onDestroy _ = this.destroy(todo)
                        member __.onEdit _ = this.edit(todo)
                        member __.editing =
                            match this.state.editing with
                            | Some editing -> editing = todo.id
                            | None -> false
                        member __.onSave text = this.save(todo, string text)
                        member __.onCancel _ = this.cancel()
                    }) [])
        let activeTodoCount =
            todos |> Array.fold (fun accum todo ->
                if todo.completed then accum else accum + 1
            ) 0
        let completedCount =
            todos.Length - activeTodoCount
        let footer =
            if activeTodoCount > 0 || completedCount > 0 then
                R.com<TodoFooter,_,_>(
                    { new TodoFooterProps with
                        member __.count = activeTodoCount
                        member __.completedCount = completedCount
                        member __.nowShowing = this.state.nowShowing
                        member __.onClearCompleted _ = this.clearCompleted()
                }) [] |> Some
            else None
        let main =
            if todos.Length > 0 then
                R.section [ ClassName "main" ] [
                    R.input [
                        ClassName "toggle-all"
                        Type "checkbox"
                        OnChange this.toggleAll
                        Checked (activeTodoCount = 0)
                    ] []
                    R.ul [ ClassName "todo-list" ] [ unbox todoItems ]
                ] |> Some
            else None
        R.div [] [
            R.header [ ClassName "header" ] [
                R.h1 [] [ unbox "todos" ]
                R.input [
                    ClassName "new-todo"
                    Placeholder "What needs to be done?"
                    Value (U2.Case1 this.state.newTodo)
                    OnKeyDown this.handleNewTodoKeyDown
                    OnChange this.handleChange
                    AutoFocus true
                ] []
            ]
            main.Value
            footer.Value
        ]

let model = TodoModel("react-todos")
let render() =
    ReactDom.render(
        R.com<TodoApp,_,_> { model = model } [],
        Browser.document.getElementsByClassName("todoapp").[0]
    ) |> ignore

model.subscribe(render)
render()
