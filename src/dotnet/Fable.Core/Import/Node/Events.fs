module rec Fable.Import.Node.Events

open System
open Fable.Import.JS
open Fable.Core

type [<AllowNullLiteral>] EventEmitter =
    abstract defaultMaxListeners: float with get, set
    abstract addListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
    abstract on: ev: string * listener: ('a -> unit) -> EventEmitter
    abstract on: ev: string * listener: ('a -> 'b -> unit) -> EventEmitter
    abstract once: ev: string * listener: ('a -> unit) -> EventEmitter
    abstract once: ev: string * listener: ('a -> 'b -> unit) -> EventEmitter
    abstract prependListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
    abstract prependOnceListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
    abstract removeListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
    abstract removeAllListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
    abstract setMaxListeners: n: int -> EventEmitter
    abstract getMaxListeners: unit -> int
    abstract listeners: ev: U2<string, Symbol> -> ResizeArray<Function>
    abstract emit: ev: string * [<ParamArray>] args: obj[] -> bool
    abstract eventNames: unit -> ResizeArray<U2<string, Symbol>>
    
type [<AllowNullLiteral>] EventEmitterStatic =
    [<Emit("new $0()")>] abstract Create: unit -> EventEmitter

type IExports =
    abstract defaultMaxListeners: float with get, set
    abstract EventEmitter: EventEmitterStatic with get, set