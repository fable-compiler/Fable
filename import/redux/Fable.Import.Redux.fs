namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS

module Redux =
    type ActionCreator =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: obj[] -> obj

    and Reducer =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: state: obj * action: obj -> obj

    and Dispatch =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: action: obj -> obj

    and StoreMethods =
        abstract dispatch: Dispatch with get, set
        abstract getState: unit -> obj

    and MiddlewareArg =
        abstract dispatch: Dispatch with get, set
        abstract getState: Function with get, set

    and Middleware =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: obj: MiddlewareArg -> Function

    and [<Import("Store","Redux")>] Store() =
        member __.getReducer(): Reducer = failwith "JS only"
        member __.replaceReducer(nextReducer: Reducer): unit = failwith "JS only"
        member __.dispatch(action: obj): obj = failwith "JS only"
        member __.getState(): obj = failwith "JS only"
        member __.subscribe(listener: Function): Function = failwith "JS only"

    type [<Import("*","Redux")>] Globals =
        static member createStore(reducer: Reducer, ?initialState: obj, ?enhancer: Function): Store = failwith "JS only"
        static member bindActionCreators(actionCreators: 'T, dispatch: Dispatch): 'T = failwith "JS only"
        static member combineReducers(reducers: obj): Reducer = failwith "JS only"
        static member applyMiddleware([<ParamArray>] middlewares: Middleware[]): Function = failwith "JS only"
        static member compose([<ParamArray>] functions: Function[]): 'T = failwith "JS only"


