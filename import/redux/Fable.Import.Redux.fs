namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS

// Type definitions for redux v3.3.1, generated from Typescript
module Redux =
    type ActionCreator =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: obj[] -> obj

    and Reducer<'T, 'U> = Func<'T, 'U, 'T>

    and Dispatch =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: action: obj -> obj

    and StoreMethods =
        abstract dispatch: Dispatch with get
        abstract getState: unit -> obj

    and MiddlewareArg =
        abstract dispatch: Dispatch with get
        abstract getState: Function with get

    and Middleware =
        inherit Function
        [<Emit("$0($1...)")>] abstract Invoke: obj: MiddlewareArg -> Function

    and [<Import("store","redux")>] Store() =
        member __.getReducer(): Reducer<'T,'U> = failwith "JS only"
        member __.replaceReducer(nextReducer: Reducer<'T,'U>): unit = failwith "JS only"
        member __.dispatch(action: obj): obj = failwith "JS only"
        member __.getState(): obj = failwith "JS only"
        member __.subscribe(listener: Function): Function = failwith "JS only"

    type [<Import("*","redux")>] Globals =
        static member createStore(reducer: Reducer<'T,'U>, ?initialState: obj, ?enhancer: Function): Store = failwith "JS only"
        static member bindActionCreators(actionCreators: 'T, dispatch: Dispatch): 'T = failwith "JS only"
        static member combineReducers([<ParamArray>] reducers: Reducer<'T,'U>[]): Reducer<'T,'U> = failwith "JS only"
        static member applyMiddleware([<ParamArray>] middlewares: Middleware[]): Function = failwith "JS only"
        static member compose([<ParamArray>] functions: Function[]): 'T = failwith "JS only"


