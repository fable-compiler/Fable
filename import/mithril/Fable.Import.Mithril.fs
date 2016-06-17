

namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

module Mithril =
   // type ChildArray = ResizeArray<Children>


    type Children =
        U2<Child, ResizeArray<Object>>

    and Child =
        U3<string, VirtualElement, Component<Controller>>

    and Static =
        abstract redraw: obj with get, set
        abstract route: obj with get, set
        abstract deferred: obj with get, set
        [<Emit("$0($1...)")>] abstract Invoke: selector: string * [<ParamArray>] children: Children[] -> VirtualElement
        [<Emit("$0($1...)")>] abstract Invoke: selector: string * attributes: Attributes * [<ParamArray>] children: Children[] -> VirtualElement
        [<Emit("$0($1...)")>] abstract Invoke: ``component``: Component<'T> * [<ParamArray>] args: obj[] -> Component<'T>
        abstract prop: promise: Thennable<'T> -> Promise<'T>
        abstract prop: value: 'T -> BasicProperty<'T>
        abstract prop: unit -> BasicProperty<'T>
        abstract withAttr: property: string * callback: Func<obj, obj> * ?callbackThis: obj -> Func<Event, unit>
        abstract ``module``: rootElement: Node * ``component``: Component<'T> -> 'T
        abstract mount: rootElement: Node * ``component``: Component<'T> -> 'T
        abstract ``component``: ``component``: Component<'T> * [<ParamArray>] args: obj[] -> Component<'T>
        abstract trust: html: string -> string
        abstract render: rootElement: Element * children: U2<VirtualElement, ResizeArray<VirtualElement>> * ?forceRecreation: bool -> unit
        abstract request: options: XHROptions -> Promise<obj>
        abstract request: options: JSONPOptions -> Promise<obj>
        abstract sync: promises: ResizeArray<Thennable<'T>> -> Promise<ResizeArray<'T>>
        abstract startComputation: unit -> unit
        abstract endComputation: unit -> unit
        abstract deps: mockWindow: Window -> Window

    and VirtualElement =
        abstract tag: string with get, set
        abstract attrs: Attributes with get, set
        abstract children: ResizeArray<Children> with get, set

    and Event =
        abstract preventDefault: unit -> unit

    and Context =
        abstract retain: bool option with get, set
        abstract onunload: unit -> obj

    and ElementConfig =
        [<Emit("$0($1...)")>] abstract Invoke: element: Element * isInitialized: bool * context: Context * vdom: VirtualElement -> unit

    and Attributes =
        abstract className: string option with get, set
        abstract ``class``: string option with get, set
        abstract config: ElementConfig option with get, set
        abstract key: U2<string, float> option with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: property: string -> obj with get, set

    and Controller =
        abstract onunload: evt: Event -> obj

    and ControllerFunction<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: obj[] -> 'T

    and ControllerConstructor<'T> =
        [<Emit("new $0($1...)")>] abstract Create: [<ParamArray>] args: obj[] -> 'T

    and Component<'T> =
        abstract controller: U2<ControllerFunction<'T>, ControllerConstructor<'T>> with get, set
        abstract view: ?ctrl: 'T * [<ParamArray>] args: obj[] -> VirtualElement

    and Property<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: unit -> 'T
        [<Emit("$0($1...)")>] abstract Invoke: value: 'T -> 'T

    and BasicProperty<'T> =
        inherit Property<'T>
        abstract toJSON: unit -> 'T

    and Routes =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> Component<Controller> with get, set

    and Deferred<'T> =
        abstract promise: Promise<'T> with get, set
        abstract resolve: ?value: 'T -> unit
        abstract reject: ?reason: obj -> unit

    and SuccessCallback<'T, 'U> =
        [<Emit("$0($1...)")>] abstract Invoke: value: 'T -> U2<'U, Thennable<'U>>

    and ErrorCallback<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: value: Error -> U2<'T, Thennable<'T>>

    and Thennable<'T> =
        abstract ``then``: success: SuccessCallback<'T, 'U> -> Thennable<'U>
        abstract ``then``: success: SuccessCallback<'T, 'U> * error: ErrorCallback<'V> -> Thennable<U2<'U, 'V>>
        abstract catch: error: ErrorCallback<'T> -> Thennable<'T>
        abstract catch: error: ErrorCallback<'U> -> Thennable<U2<'T, 'U>>

    and Promise<'T> =
        inherit Thennable<'T>
        inherit Property<U2<'T, Promise<'T>>>
        abstract ``then``: success: SuccessCallback<'T, 'U> -> Promise<'U>
        abstract ``then``: success: SuccessCallback<'T, 'U> * error: ErrorCallback<'V> -> Promise<U2<'U, 'V>>
        abstract catch: error: ErrorCallback<'U> -> Promise<U2<'T, 'U>>

    and RequestOptions =
        abstract data: obj option with get, set
        abstract background: bool option with get, set
        abstract initialValue: obj option with get, set
        abstract ``type``: obj option with get, set
        abstract url: string with get, set
        abstract unwrapSuccess: data: obj -> obj
        abstract unwrapError: data: obj -> obj
        abstract serialize: dataToSerialize: obj -> string
        abstract deserialize: dataToDeserialize: string -> obj
        abstract extract: xhr: XMLHttpRequest * options: obj -> string

    and JSONPOptions =
        inherit RequestOptions
        abstract dataType: obj with get, set
        abstract callbackKey: string option with get, set
        abstract data: obj option with get, set

    and XHROptions =
        inherit RequestOptions
        abstract ``method``: (* TODO StringEnum GET | POST | PUT | DELETE | HEAD | OPTIONS *) string with get, set
        abstract user: string option with get, set
        abstract password: string option with get, set
        abstract data: obj option with get, set
        abstract config: xhr: XMLHttpRequest * options: obj -> obj

type Globals =
    [<Global>] static member m with get(): Mithril.Static = failwith "JS only" and set(v: Mithril.Static): unit = failwith "JS only"

