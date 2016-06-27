(**
 - title: The Elm architecture using Fable
 - tagline: Fable implementation of the Elm architecture
 - intro: This demo is an implementation of the [Elm architecture](http://guide.elm-lang.org/architecture/)
 using the same virtual-dom as Elm originally used, https://github.com/Matt-Esch/virtual-dom.
*)
(*** hide ***)
#load "Fable.Helpers.Virtualdom.fsx"
#load "Fable.Import.Virtualdom.fsx"
#r "node_modules/fable-core/Fable.Core.dll"
(**
##Architecture overview

The beauty of the architecture Elm is using for their application is its
simplicity. You can read and grasp the whole architecture in a matter of minutes
here: http://guide.elm-lang.org/architecture/. I won't explain the architecture
further, instead I will go straight to the examples.

###First example - a simple counter
*)
open Fable.Core
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.Virtualdom.App
open Fable.Helpers.Virtualdom.Html
open Fable.Import.Virtualdom.Virtualdom

// model
type Counter = int
let initCounter = 0

(**
The model for the first example is a simple integer that will act as hour counter.
We also provide a default value for our counter.
*)

// Update
type CounterAction =
    | Decrement of int
    | Increment of int

let counterUpdate model action =
    match action with
    | Decrement x -> model - x
    | Increment x -> model + x
    |> (fun m -> m,[])

(**
The counter can be incremented or decremented in step of `x`. If you look closely
the update function return the new model and a list of something. The list of
something is list of calls js-calls of type `unit->unit` that should be executed
after this version of the model has been rendered. We will see in a later example
why this is useful.
*)

// View
let counterView handler model =
    let bgColor =
        match model with
        | x when x > 100 -> "red"
        | x when x < 0 -> "blue"
        | _ -> "green"
    div []
        [
            div [   Style ["border","1px solid blue"]
                    onMouseClick (fun x -> handler (Increment 1))
                    onDblClick (fun x -> handler ((Increment 100)))] [text (string "Increment")]
            div [ Style ["background-color", bgColor; "color", "white"]] [text (string model)]
            div [   Style ["border", "1px solid green"; "height", ((string (70 + model)) + "px")]
                    onMouseClick (fun x -> handler (Decrement 1))
                    onDblClick (fun x -> handler (Decrement 50))]
                [text (string "Decrement")]
        ]

(**
The `counterView` defines how a model should be rendered. It also takes a `handler`
that should have a function of the type `'TAction -> unit`, this makes it possible
to react user actions like we do here on simple mouse actions. The dsl that is used
here is quite simple and have helper functions for a majority of the standard
HTML elements. It is trivial to add custom tags if you are missing some tag
that you would like to use.
*)

// Start the application
let counterApp =
    createApp {Model = initCounter; View = counterView; Update = counterUpdate}
    |> withStartNode "#counter"

counterApp |> start renderer

(**
The dsl has been separated from the actual rendering of the dsl, to allow for
future server side rendering as well. So to get this application started you first
need to create the application with the `createApp` function. The we pass that
result to a helper function to specify where in the document it should be rendered,
default is directly in the body. When we have defined an application we can call
the `start` function and pass in a `renderer`. We are using the standard `renderer`
for `virtual-dom.js`, but this separation makes it a little bit easier to change
to another framework in the future.

That's it, the first application is done and we are ready for example 2.

### Second example - todomvc

To have something to compare to other js-framework, Elm and whatnot a todomvc app
is in its place. If you don't know what todomvc is check it out here:
http://todomvc.com/.

We will follow the exact same steps as with the counter example. First implement
the model, then the update function that to handle actions and lastly the view.
This example is a little bit longer and have some more features. I'll also show
how easy it is to store data from the model in the local storage, and that could
easily be swapped to server side storage without effecting any of the application
code. Let's start.
*)

// Todo model
type Filter =
    | All
    | Completed
    | Active

type Item =
    {
        Name: string
        Done: bool
        Id: int
        IsEditing: bool
    }

type TodoModel =
    {
        Items: Item list
        Input: string
        Filter: Filter
    }

(**
The model is really simple. It consists of a list of items, which you can edit
and they can be marked as done. You also have a input field and something to
filter the models with. I use a discriminated union to filter the items, which
is a nice feature you don't have in standard js.
*)

// Todo update
type TodoAction =
    | AddItem of Item
    | ChangeInput of string
    | MarkAsDone of Item
    | ToggleItem of Item
    | Destroy of Item
    | CheckAll
    | UnCheckAll
    | SetActiveFilter of Filter
    | ClearCompleted
    | EditItem of Item
    | SaveItem of Item*string

(**
First we define the actual actions before moving on to the actual update function.
*)

let todoUpdate model msg =
    let checkAllWith v =
        { model with Items = model.Items |> List.map (fun i -> { i with Done = v })}

    let updateItem i model =
        let items' =
            model.Items |> List.map (fun i' -> if i'.Id <> i.Id then i' else i)
        {model with Items = items'}

    let model' =
        match msg with
        | AddItem item ->
            let maxId =
                if model.Items |> List.isEmpty then 1
                else
                    model.Items
                    |> List.map (fun x -> x.Id)
                    |> List.max
            let item' = {item with Id = maxId + 1}
            {model with Items = item'::model.Items; Input = ""}
        | ChangeInput v -> {model with Input = v}
        | MarkAsDone i ->
            let items' =
                model.Items |> List.map (fun i' -> if i' <> i then i' else {i with Done = true})
            {model with Items = items'}
        | CheckAll -> checkAllWith true
        | UnCheckAll -> checkAllWith false
        | Destroy i -> {model with Items = model.Items |> List.filter (fun i' -> i'.Id <> i.Id)}
        | ToggleItem i -> updateItem {i with Done = not i.Done} model
        | SetActiveFilter f -> { model with Filter = f }
        | ClearCompleted -> { model with Items = model.Items |> List.filter (fun i -> not i.Done)}
        | EditItem i -> updateItem { i with IsEditing = true} model
        | SaveItem (i,str) -> updateItem { i with Name = str; IsEditing = false} model

    let jsCalls =
        match msg with
        | EditItem i -> [fun () -> document.getElementById("item-" + (i.Id.ToString())).focus()]
        | _ -> []
    model',jsCalls

(**
It might seem like a lot of code, but we need to handle all actions and respond
to them accordingly. I won't go into the detail in all the scenarios, but you
should pay attention to the step where `jsCalls` is defined. Since we are re-rendering
the application on changes in the model, or render what has changed as least, we need
a way to give an input focus if we start edit it. That is when the list of js
function calls come in handy. So the update function returns the new model a
long side a list of js function calls if we need to do something after rendering,
and that is something we need to do when we start edit an item, we want to give
that item focus.

With this done all we need is to define the view.
*)

// Todo view
let filterToTextAndUrl = function
    | All -> "All", ""
    | Completed -> "Completed", "completed"
    | Active -> "Active", "active"

let filter handler activeFilter f =
    let linkClass = if f = activeFilter then "selected" else ""
    let fText,url = f |> filterToTextAndUrl
    li
        [ onMouseClick (fun _ -> SetActiveFilter f |> handler)]
        [ a
            [ attribute "href" ("#/" + url); attribute "class" linkClass ]
            [ text fText] ]

let filters model handler =
    ul
        [ attribute "class" "filters" ]
        ([ All; Active; Completed ] |> List.map (filter handler model.Filter))

let todoFooter model handler =
    let clearVisibility =
        if model.Items |> List.exists (fun i -> i.Done)
        then ""
        else "none"
    let activeCount = model.Items |> List.filter (fun i -> not i.Done) |> List.length |> string
    footer
        [   attribute "class" "footer"; Style ["display","block"]]
        [   span
                [ attribute "class" "todo-count" ]
                [   strong [] [text activeCount]
                    text " items left" ]
            (filters model handler)
            button
                [   attribute "class" "clear-completed"
                    Style [ "display", clearVisibility ]
                    onMouseClick (fun _ -> handler ClearCompleted)]
                [ text "Clear completed" ] ]


let todoHeader model handler =
    header
        [attribute "class" "header"]
        [   h1 [] [text "todos"]
            input [ attribute "class" "new-todo"
                    property "placeholder" "What needs to be done??"
                    property "value" model
                    onKeydown (fun x ->
                        if x.code = "Enter"
                        then handler (AddItem {Name = model; Id = 0; Done = false; IsEditing = false})
                        )
                    onKeyup (fun x -> handler (ChangeInput (x?target?value :?> string))) ]]

let listItem handler item =
    let itemChecked = if item.Done then "true" else ""
    let editClass = if item.IsEditing then "editing" else ""
    li [ attribute "class" ((if item.Done then "completed " else " ") + editClass)]
       [ div [  attribute "class" "view"
                onDblClick (fun x -> printfn "%A" x; EditItem item |> handler) ]
             [ input [  property "className" "toggle"
                        property "type" "checkbox"
                        property "checked" itemChecked
                        onMouseClick (fun e -> handler (ToggleItem item)) ]
               label [] [ text item.Name ]
               button [ attribute "class" "destroy"
                        onMouseClick (fun e -> handler (Destroy item)) ] [] ]
         input [ attribute "class" "edit"
                 attribute "value" item.Name
                 property "id" ("item-"+item.Id.ToString())
                 onBlur (fun e -> SaveItem (item, (e?target?value :?> string)) |> handler) ] ]

let itemList handler items activeFilter =
    let filterItems i =
        match activeFilter with
        | All -> true
        | Completed -> i.Done
        | Active -> not i.Done

    ul [attribute "class" "todo-list" ]
       (items |> List.filter filterItems |> List.map (listItem handler))

let todoMain model handler =
    let items = model.Items
    let allChecked = items |> List.exists (fun i -> not i.Done)
    section [  attribute "class" "main"
               Style [ "style", "block" ] ]
            [   input [ property "id" "toggle-all"
                        attribute "class" "toggle-all"
                        property "type" "checkbox"
                        property "checked" (if not allChecked then "true" else "")
                        onMouseClick (fun e ->
                                    if allChecked
                                    then handler CheckAll
                                    else handler UnCheckAll) ]
                label [ attribute "for" "toggle-all" ]
                      [ text "Mark all as complete" ]
                (itemList handler items model.Filter) ]

let todoView handler model =
    section
        [attribute "class" "todoapp"]
        ((todoHeader model.Input handler)::(if model.Items |> List.isEmpty
                then []
                else [  (todoMain model handler)
                        (todoFooter model handler) ] ))

(**
This view is more complex than the first example, but it also show how easy it is
to split a view up into pieces and then combine them together to form a whole. This
makes it quite easy to re-use parts in different views.

Before this is done, there are one hidden gem that is worth knowing, and it will
be showed with two examples. We will add local storage support of the items and a
logger of all the actions and model changes. First we need a helper for the storage.
*)

// Storage
module Storage =
    let private STORAGE_KEY = "vdom-storage"
    open Microsoft.FSharp.Core
    let fetch<'T> (): 'T [] =
        Browser.localStorage.getItem(STORAGE_KEY)
        |> function null -> "[]" | x -> unbox x
        |> JS.JSON.parse |> unbox

    let save<'T> (todos: 'T []) =
        Browser.localStorage.setItem(STORAGE_KEY, JS.JSON.stringify todos)

(**
The module above is a simple helper to store a list of items in local storage
of the browser, now let's add it and support for logging.
*)

open Storage
let initList = fetch<Item>() |> List.ofArray
let initModel = {Filter = All; Items = initList; Input = ""}

let todoApp =
    createApp {Model = initModel; View = todoView; Update = todoUpdate}
    |> (withSubscriber "storagesub" (function
            | ModelChanged (newModel,old) -> save (newModel.Items |> Array.ofList)
            | _ -> ()))
    |> (withSubscriber "modellogger" (printfn "%A"))
    |> withStartNode "#todo"

todoApp |> start renderer

(**
First we initiate the model by checking the local storage if there are any items
there. The to add support for local storage we add a subscriber. A subscriber is
a function that handles `AppEvents`, they can be `ModelChanged` or `ActionReceived`.
For the storage we are only interested in model changes, so that is what we act on
and store the list of items in the local storage when the model was changed. For
the logger we just logs everything.

We also start the application on the `#todo` element in the document.
*)
