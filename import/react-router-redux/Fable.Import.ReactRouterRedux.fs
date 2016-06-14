namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
module H = Fable.Import.HistoryModule
module R = Fable.Import.Redux

// Type definitions for react-router-redux v4.0.0, generated from Typescript
module ReactRouterRedux =
    type LocationDescriptor =
        U2<H.Location, H.Path>

    and PushAction =
        Func<LocationDescriptor, RouterAction>

    and ReplaceAction =
        Func<LocationDescriptor, RouterAction>

    and GoAction =
        Func<float, RouterAction>

    and GoForwardAction =
        Func<RouterAction>

    and GoBackAction =
        Func<RouterAction>

    and RouterAction =
        obj

    and RouteActions =
        abstract push: PushAction with get
        abstract replace: ReplaceAction with get
        abstract go: GoAction with get
        abstract goForward: GoForwardAction with get
        abstract goBack: GoBackAction with get

    and ReactRouterReduxHistory =
        inherit H.History
        abstract unsubscribe: unit -> unit

    and DefaultSelectLocationState =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: state: obj -> obj

    and SyncHistoryWithStoreOptions =
        abstract selectLocationState: DefaultSelectLocationState option with get
        abstract adjustUrlOnReplay: bool option with get

    type [<Import("*","ReactRouterRedux")>] Globals =
        static member CALL_HISTORY_METHOD with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member LOCATION_CHANGE with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member push with get(): PushAction = failwith "JS only" and set(v: PushAction): unit = failwith "JS only"
        static member replace with get(): ReplaceAction = failwith "JS only" and set(v: ReplaceAction): unit = failwith "JS only"
        static member go with get(): GoAction = failwith "JS only" and set(v: GoAction): unit = failwith "JS only"
        static member goBack with get(): GoForwardAction = failwith "JS only" and set(v: GoForwardAction): unit = failwith "JS only"
        static member goForward with get(): GoBackAction = failwith "JS only" and set(v: GoBackAction): unit = failwith "JS only"
        static member routerActions with get(): RouteActions = failwith "JS only" and set(v: RouteActions): unit = failwith "JS only"
        static member routerReducer(?state: obj, ?options: obj): R.Reducer<'T,'U> = failwith "JS only"
        static member syncHistoryWithStore(history: H.History, store: R.Store, ?options: SyncHistoryWithStoreOptions): ReactRouterReduxHistory = failwith "JS only"
        static member routerMiddleware(history: H.History): R.Middleware = failwith "JS only"


