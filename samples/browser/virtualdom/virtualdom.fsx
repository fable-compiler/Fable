(**
 - title: The Elm architecture using Fable
 - tagline: Fable implementation of the Elm architecture
 - app-style: width:800px; margin:20px auto 50px auto;
 - require-paths: `'virtual-dom':'lib/virtual-dom'`
 - intro: This demo is an implementation of the [Elm architecture](http://guide.elm-lang.org/architecture/)
   using the same [virtual-dom](https://github.com/Matt-Esch/virtual-dom) as Elm originally used.
   Contributed by [Tomas Jansson](https://twitter.com/TomasJansson).
*)
(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
//#load "node_modules/fable-import-virtualdom/Fable.Helpers.Virtualdom.fs"
#load "../../../import/virtualdom/Fable.Helpers.Virtualdom.fs"
(**
##Architecture overview

The beauty of the architecture Elm is using for their application is its
simplicity. You can read and grasp the whole architecture in a matter of minutes
here: http://guide.elm-lang.org/architecture/. I won't explain the architecture
further, instead I will go straight to the examples.

###First example - a simple counter with svg and ajax call simulation

The example below doesn't make any sense except for demonstrating most of 
the framework. The example consists of a counter that you can increment and 
decrement by clicking on the labels. The color and some styling (height) 
changes depending on the counter. Above the counter is a simple svg square 
that also changes color on based on a threshold. To bootstrap the 
application a fake ajax call is made using a timeout, that function is 
also called for each click on any of the labels faking more ajax call.

<div id="counter">
</div>

*)
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open Fable.Helpers.Virtualdom
open Fable.Helpers.Virtualdom.App
open Fable.Helpers.Virtualdom.Html

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

let fakeAjaxCall model (h:CounterAction->unit) = 
    let message = if model < 30 then Increment 10 else Decrement 5 
    if model > 30 && model < 60 then () 
    else window.setTimeout((fun _ -> h (message)), 2000) |> ignore

let counterUpdate model command =
    match command with
    | Decrement x -> model - x
    | Increment x -> model + x
    |> (fun m -> m, (fakeAjaxCall model) |> action) 

(**
The counter can be incremented or decremented in step of `x`. If you look closely
the update function return the new model, a list of something and a second list of 
functions. The list of something is a list of functions of type `unit->unit` that 
will be executed after this version of the model has been rendered. That way
you can run some custom js after something has been rendered. The last list of
functions is a list of "long" running functions. These functions must be of 
the type `('TMessage -> unit) -> unit`. The first argument is what makes it 
possible for these long running functions to trigger a new update. In our 
example we use `fakeAjaxCall` to simulate a function call that 
take a while to execute. The type doesn't match completely of what is 
expected, but after we partially apply it in `counterUpdate` we do get the right type.
*)

// View
let counterView model =
    let bgColor =
        match model with
        | x when x > 10 -> "red"
        | x when x < 0 -> "blue"
        | _ -> "green"
    div []
        [
            div [ Style ["width", "120px"; "height", "120px"] ] [
                svg [ width "120"; height "120"; viewBox "0 0 100 100" ]
                    [ rect [width "110"; height "110"; fill bgColor] []]
            ]
            div [ Style ["border","1px solid blue"]
                  onMouseClick (fun x -> (Increment 1))
                  onDblClick (fun x -> ((Increment 10)))]
                [ text (string "Increment")]
            div [ Style ["background-color", bgColor; "color", "white"]]
                [text (string model)]
            div [ Style ["border", "1px solid green";
                         "height", ((string (7 + model)) + "px")]
                  onMouseClick (fun x -> (Decrement 1))
                  onDblClick (fun x -> (Decrement 5))]
                [ text (string "Decrement")]
        ]

(**
The `counterView` defines how a model should be rendered. The dsl that is used
here is quite simple and have helper functions for a majority of the standard
HTML elements. It is trivial to add custom tags if you are missing some tag
that you would like to use.

You define a `svg` the same way as you do with any other html element. 
*)

// Start the application
createApp initCounter counterView counterUpdate
|> withInitMessage (fakeAjaxCall initCounter) 
|> withStartNodeSelector "#counter"
|> start renderer

let bindOpt<'T1,'T2> (m:'T1 -> 'T2 option) (o: 'T1 Option) =
    match o with
    | Some x -> m x
    | None -> None

let mapOpt<'T1,'T2> (m:'T1 -> 'T2) (o: 'T1 Option) = (bindOpt (m >> Some) o)

type NestedModel = { Top: int; Bottom: int}

type NestedAction = 
    | Reset
    | Top of CounterAction
    | Bottom of CounterAction

let nestedUpdate model action = 
    let mapCounterAction tag action = 
        match action with
        | Some a -> (fun x -> a (tag >> x)) |> Some 
        | None -> None

    match action with
    | Reset -> {Top = 0; Bottom = 0},[]
    | Top ca -> 
        let (res, action) = (counterUpdate model.Top ca)
        let action' = App.mapActions Top action
        {model with Top = res},action'
    | Bottom ca -> 
        let (res, action) = (counterUpdate model.Bottom ca)
        let action' = App.mapActions Bottom action
        {model with Bottom = res},action'

let nestedView model = 
    div []
        [
            Html.map Top (counterView model.Top)
            Html.map Bottom (counterView model.Bottom)
        ]

let resetEveryTenth h =
    window.setInterval((fun _ -> Reset |> h), 10000) |> ignore

createApp {Top = 0; Bottom = 0} nestedView nestedUpdate
|> withStartNodeSelector "#nested-counter"
|> withProducer resetEveryTenth
|> start renderer

(**
The dsl has been separated from the actual rendering of the dsl, to allow for
future server side rendering as well. So to get this application started you first
need to create the application with the `createApp` function. The we pass that
result to a helper function, `withStartNode` to specify where in the document 
it should be rendered, default is directly in the body. We also want to start the 
application by making a call to our `fakeAjaxCall` function, this will result
in an update of the model two seconds after the application starts.

When we have defined an application we can call
the `start` function and pass in a `renderer`. We are using the standard `renderer`
for `virtual-dom.js`, but this separation makes it a little bit easier to change
to another framework in the future.

That's it, the first application is done and we are ready for example 2.

### Second example - todomvc

To have something to compare to other js-framework, Elm and whatnot a todomvc app
is in its place. If you don't know what todomvc is check it out here:
http://todomvc.com/. The app below should have all the features expected from a
todomvc app.

<div id="todo">
</div>

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
    | NoOp
    | AddItem
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
    let updateItems model f =
        let items' = f model.Items
        {model with Items = items'}

    let checkAllWith v =
        List.map (fun i -> { i with Done = v })
        |> updateItems model

    let updateItem i model =
        List.map (fun i' ->
                if i'.Id <> i.Id then i' else i)
        |> updateItems model

    let model' =
        match msg with
        | NoOp -> model
        | AddItem ->
            let maxId =
                if model.Items |> List.isEmpty then 1
                else
                    model.Items
                    |> List.map (fun x -> x.Id)
                    |> List.max
            (fun items ->
                items @ [{  Id = maxId + 1
                            Name = model.Input
                            Done = false
                            IsEditing = false}])
            |> updateItems {model with Input = ""}
        | ChangeInput v -> {model with Input = v}
        | MarkAsDone i ->
            updateItem {i with Done = true} model
        | CheckAll -> checkAllWith true
        | UnCheckAll -> checkAllWith false
        | Destroy i ->
            List.filter (fun i' -> i'.Id <> i.Id)
            |> updateItems model
        | ToggleItem i ->
            updateItem {i with Done = not i.Done} model
        | SetActiveFilter f ->
            { model with Filter = f }
        | ClearCompleted ->
            List.filter (fun i -> not i.Done)
            |> updateItems model
        | EditItem i ->
            updateItem { i with IsEditing = true} model
        | SaveItem (i,str) ->
            updateItem { i with Name = str; IsEditing = false} model

    let jsCall =
        match msg with
        | EditItem i -> action <| fun x -> document.getElementById("item-" + (i.Id.ToString())).focus()
        | _ -> []
    model', jsCall

(**
It might seem like a lot of code, but we need to handle all actions and respond
to them accordingly, and this is basically all the logic associated with the todo app. 
I won't go into the detail in all the scenarios, but you
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

let filter activeFilter f =
    let linkClass = if f = activeFilter then "selected" else ""
    let fText,url = f |> filterToTextAndUrl
    li
        [ onMouseClick (fun _ -> SetActiveFilter f)]
        [ a
            [ attribute "href" ("#/" + url); attribute "class" linkClass ]
            [ text fText] ]

let filters model =
    ul
        [ attribute "class" "filters" ]
        ([ All; Active; Completed ] |> List.map (filter model.Filter))

let todoFooter model =
    let clearVisibility =
        if model.Items |> List.exists (fun i -> i.Done)
        then ""
        else "none"
    let activeCount =
        model.Items
        |> List.filter (fun i -> not i.Done)
        |> List.length |> string
    footer
        [   attribute "class" "footer"; Style ["display","block"]]
        [   span
                [   attribute "class" "todo-count" ]
                [   strong [] [text activeCount]
                    text " items left" ]
            (filters model)
            button
                [   attribute "class" "clear-completed"
                    Style [ "display", clearVisibility ]
                    onMouseClick (fun _ -> ClearCompleted)]
                [ text "Clear completed" ] ]

let inline onInput x = onEvent "oninput" (fun e -> x (unbox e?target?value)) 
let onEnter succ nop = onKeyup (fun x -> if (unbox x?keyCode) = 13 then succ else nop)
let todoHeader model =
    header
        [attribute "class" "header"]
        [   h1 [] [text "todos"]
            input [ attribute "class" "new-todo"
                    attribute "id" "new-todo"
                    property "value" model
                    property "placeholder" "What needs to be done?"
                    onInput (fun x -> ChangeInput x)
                    onEnter AddItem NoOp ]]
let listItem item =
    let itemChecked = if item.Done then "true" else ""
    let editClass = if item.IsEditing then "editing" else ""
    li [ attribute "class" ((if item.Done then "completed " else " ") + editClass)]
       [ div [  attribute "class" "view"
                onDblClick (fun x -> EditItem item) ]
             [ input [  property "className" "toggle"
                        property "type" "checkbox"
                        property "checked" itemChecked
                        onMouseClick (fun e -> ToggleItem item) ]
               label [] [ text item.Name ]
               button [ attribute "class" "destroy"
                        onMouseClick (fun e -> Destroy item) ] [] ]
         input [ attribute "class" "edit"
                 attribute "value" item.Name
                 property "id" ("item-"+item.Id.ToString())
                 onBlur (fun e -> SaveItem (item, (unbox e?target?value))) ] ]

let itemList items activeFilter =
    let filterItems i =
        match activeFilter with
        | All -> true
        | Completed -> i.Done
        | Active -> not i.Done

    ul [attribute "class" "todo-list" ]
       (items |> List.filter filterItems |> List.map listItem)

let todoMain model =
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
                                    then CheckAll
                                    else UnCheckAll) ]
                label [ attribute "for" "toggle-all" ]
                      [ text "Mark all as complete" ]
                (itemList items model.Filter) ]

let todoView model =
    section
        [attribute "class" "todoapp"]
        ((todoHeader model.Input)::(if model.Items |> List.isEmpty
                then []
                else [  (todoMain model)
                        (todoFooter model) ] ))

(**
This view is more complex than the first example, but it also show how easy it is
to split a view up into pieces and then combine them together to form a whole. This
makes it quite easy to re-use parts in different views.

One thing to notice is that only a few properties are mapped at the moment, but if
you know the property name you can use the syntax `e?target?value`, which will
look app the `value` property on the `target` property on the `e` event as in the
example above, and that is what is done in the helper function `onInput`.

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

createApp initModel todoView todoUpdate
|> (withSubscriber "storagesub" (function
        | ModelChanged (newModel,old) ->
            save (newModel.Items |> Array.ofList)
        | _ -> ()))
|> (withSubscriber "modellogger" (printfn "%A"))
|> withStartNodeSelector "#todo"
|> start renderer

(**
First we initiate the model by checking the local storage if there are any items
there. The to add support for local storage we add a subscriber. A subscriber is
a function that handles `AppEvents`, they can be `ModelChanged` or `ActionReceived`.
For the storage we are only interested in model changes, so that is what we act on
and store the list of items in the local storage when the model was changed. For
the logger we just logs everything.

We also start the application on the `#todo` element in the document.

### Creating custom elements

If some tag or you want to create a custom helper function that represent some
html element it is easy to extend the dsl with your needs. To add a custom html
node where you set the css class directly you write something like:
*)

let inline myDiv className = elem "div" [attribute "class" className]

(*

Creating svg nodes are as easy as regular html nodes:

*)

let inline redRect x = svgElem "rect" ((fill "red")::x)

(*

As you see the only difference is that you use `svgElem` instead of `elem`. 
You do this to add the correct namespace to the svg nodes. To see more
example of how to define your own tags just look at the source code, 
the dsl is not that complex.

*)