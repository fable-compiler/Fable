namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

// Type definitions for history v2.0.0; generated from Typescript with some modifications including some module names.
module HistoryModule =
    type Action =
        string

    and BeforeUnloadHook =
        Func<U2<string, bool>>

    and CreateHistory<'T> =
        Func<HistoryOptions, 'T>

    and CreateHistoryEnhancer<'T, 'E> =
        Func<CreateHistory<'T>, CreateHistory<'E>>

    and History =
        abstract listenBefore: hook: TransitionHook -> Func<unit>
        abstract listen: listener: LocationListener -> Func<unit>
        abstract transitionTo: location: Location -> unit
        abstract push: path: LocationDescriptor -> unit
        abstract replace: path: LocationDescriptor -> unit
        abstract go: n: float -> unit
        abstract goBack: unit -> unit
        abstract goForward: unit -> unit
        abstract createKey: unit -> LocationKey
        abstract createPath: path: LocationDescriptor -> Path
        abstract createHref: path: LocationDescriptor -> Href
        abstract createLocation: ?path: LocationDescriptor * ?action: Action * ?key: LocationKey -> Location
        abstract createLocation: ?path: Path * ?state: LocationState * ?action: Action * ?key: LocationKey -> Location
        abstract pushState: state: LocationState * path: Path -> unit
        abstract replaceState: state: LocationState * path: Path -> unit
        abstract setState: state: LocationState -> unit
        abstract registerTransitionHook: hook: TransitionHook -> unit
        abstract unregisterTransitionHook: hook: TransitionHook -> unit

    and HistoryOptions =
        obj

    and Href =
        string

    and Location =
        obj

    and LocationDescriptorObject =
        obj

    and LocationDescriptor =
        U2<LocationDescriptorObject, Path>

    and LocationKey =
        string

    and LocationListener =
        Func<Location, unit>

    and LocationState =
        obj

    and Path =
        string

    and Pathname =
        string

    and Query =
        obj

    and QueryString =
        string

    and Search =
        string

    and TransitionHook =
        Func<Location, Func<obj, unit>, obj>

    and Hash =
        string

    and HistoryBeforeUnload =
        abstract listenBeforeUnload: hook: BeforeUnloadHook -> Func<unit>

    and HistoryQueries =
        abstract pushState: state: LocationState * pathname: U2<Pathname, Path> * ?query: Query -> unit
        abstract replaceState: state: LocationState * pathname: U2<Pathname, Path> * ?query: Query -> unit
        abstract createPath: path: Path * ?query: Query -> Path
        abstract createHref: path: Path * ?query: Query -> Href

    and Module =
        abstract createHistory: CreateHistory<History> with get
        abstract createHashHistory: CreateHistory<History> with get
        abstract createMemoryHistory: CreateHistory<History> with get
        abstract actions: obj with get
        abstract createLocation: ?path: Path * ?state: LocationState * ?action: Action * ?key: LocationKey -> Location
        abstract useBasename: createHistory: CreateHistory<'T> -> CreateHistory<'T>
        abstract useBeforeUnload: createHistory: CreateHistory<'T> -> CreateHistory<obj>
        abstract useQueries: createHistory: CreateHistory<'T> -> CreateHistory<obj>



module CreateBrowserHistory =
    type [<Import("*","history/lib/createBrowserHistory")>] Globals =
        static member createBrowserHistory(?options: HistoryModule.HistoryOptions): HistoryModule.History = failwith "JS only"



module CreateHashHistory =
    type [<Import("*","history/lib/createHashHistory")>] Globals =
        static member createHashHistory(?options: HistoryModule.HistoryOptions): HistoryModule.History = failwith "JS only"



module CreateMemoryHistory =
    type [<Import("*","history/lib/createMemoryHistory")>] Globals =
        static member createMemoryHistory(?options: HistoryModule.HistoryOptions): HistoryModule.History = failwith "JS only"



module CreateLocation =
    type [<Import("*","history/lib/createLocation")>] Globals =
        static member createLocation(?path: HistoryModule.Path, ?state: HistoryModule.LocationState, ?action: HistoryModule.Action, ?key: HistoryModule.LocationKey): HistoryModule.Location = failwith "JS only"



module UseBasename =
    type [<Import("*","history/lib/useBasename")>] Globals =
        static member useBasename(createHistory: HistoryModule.CreateHistory<'T>): HistoryModule.CreateHistory<'T> = failwith "JS only"



module UseBeforeUnload =
    type [<Import("*","history/lib/useBeforeUnload")>] Globals =
        static member useBeforeUnload(createHistory: HistoryModule.CreateHistory<'T>): HistoryModule.CreateHistory<obj> = failwith "JS only"



module UseQueries =
    type [<Import("*","history/lib/useQueries")>] Globals =
        static member useQueries(createHistory: HistoryModule.CreateHistory<'T>): HistoryModule.CreateHistory<obj> = failwith "JS only"



module HistoryActions =
    type [<Import("*","history/lib/actions")>] Globals =
        static member PUSH with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member REPLACE with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member POP with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"



module HistoryDOMUtils =
    type [<Import("*","history/lib/DOMUtils")>] Globals =
        static member addEventListener(node: EventTarget, ``event``: string, listener: EventListenerOrEventListenerObject): unit = failwith "JS only"
        static member removeEventListener(node: EventTarget, ``event``: string, listener: EventListenerOrEventListenerObject): unit = failwith "JS only"
        static member getHashPath(): string = failwith "JS only"
        static member replaceHashPath(path: string): unit = failwith "JS only"
        static member getWindowPath(): string = failwith "JS only"
        static member go(n: float): unit = failwith "JS only"
        static member getUserConfirmation(message: string, callback: Func<bool, unit>): unit = failwith "JS only"
        static member supportsHistory(): bool = failwith "JS only"
        static member supportsGoWithoutReloadUsingHash(): bool = failwith "JS only"
