module rec Fable.Import.Node.Stream

open Fable.Core
open Fable.Import.JS
open Fable.Import.Node.Buffer
open Fable.Import.Node.Events

module stream_types =
    type [<AllowNullLiteral>] Stream =
        inherit event_types.EventEmitter
        abstract pipe: destination: 'T * ?options: obj -> 'T

    type [<AllowNullLiteral>] StreamStatic = 
        [<Emit("new $0()")>] abstract Create: unit -> Stream

    type ReadableOptions = {
        highWaterMark: float option
        encoding: string;
        objectMode: bool option;
        read: float -> obj option;
    }

    type [<AllowNullLiteral>] Readable =
        inherit Stream
        abstract readable: bool with get, set
        abstract push: chunk: obj * ?encoding: string -> bool
        abstract unshift: chunk: obj -> unit
        abstract unpipe: ?destination: 'T -> unit
        abstract wrap: oldStream: Readable -> Readable
        abstract pause: unit -> Readable
        abstract resume: unit -> Readable
        abstract isPaused: unit -> bool
        abstract setEncoding: string -> unit
        abstract read: ?size: int -> U2<string option, buffer_types.Buffer option>

    type [<AllowNullLiteral>] ReadableStatic = 
        [<Emit("new $0($1)")>] abstract Create: readableOptions:ReadableOptions -> Readable
        [<Emit("new $0($1)")>] abstract Create: unit -> Readable

    type WritableOptions = {
        highWaterMark: float option;
        decodeStrings: bool option;
        objectMode: bool option;
        write: U2<string, buffer_types.Buffer> -> string -> Function -> obj option;
        writev: ResizeArray<obj> -> Function -> obj option;
    }

    type [<AllowNullLiteral>] Writable =
        inherit Stream
        abstract writable: bool with get, set
        abstract write: chunk: obj * ?cb: Function -> bool
        abstract ``end``:unit -> unit
        abstract ``end``:obj: obj -> unit
        abstract ``end``: obj: obj * cb: Function -> unit
        abstract ``end``: obj: obj * encoding: string * cb: Function -> unit

    type [<AllowNullLiteral>] WritableStatic = 
        [<Emit("new $0($1)")>] abstract Create: writableOptions:WritableOptions -> Writable
            
    type DuplexOptions = {
        decodeStrings: bool option;
        encoding: string;
        objectMode: bool option;
        allowHalfOpen: bool option;
        readableObjectMode: bool option;
        writableObjectMode: bool option;
        read: float -> obj option;
        write: U2<string, buffer_types.Buffer> -> string -> Function -> obj option;
        writev: ResizeArray<obj> -> Function -> obj option;
    }

    type [<AllowNullLiteral>] Duplex =
        inherit Readable
        inherit Writable

    type [<AllowNullLiteral>] DuplexStatic = 
        [<Emit("new $0($1)")>] abstract Create: duplexOptions:DuplexOptions -> Duplex

    type [<AllowNullLiteral>] TransformOptions = 
        abstract decodeStrings: bool option with get, set
        abstract encoding: string option with get, set
        abstract objectMode: bool option with get, set
        abstract allowHalfOpen: bool option with get, set
        abstract readableObjectMode: bool option with get, set
        abstract writableObjectMode: bool option with get, set
        abstract transform: string -> string -> (Error option -> obj option -> unit) -> unit option
        abstract flush: (Error option -> unit) -> unit option

    type [<AllowNullLiteral>] TransformBufferOptions =
        abstract encoding: string option with get, set
        abstract objectMode: bool option with get, set
        abstract allowHalfOpen: bool option with get, set
        abstract readableObjectMode: bool option with get, set
        abstract writableObjectMode: bool option with get, set
        abstract transform: Option<buffer_types.Buffer -> string -> (Error option -> obj option -> unit) -> unit> with get, set
        abstract flush: Option<(Error option -> unit) -> unit> with get, set

    type [<AllowNullLiteral>] Transform = 
        inherit Readable
        inherit Writable
        abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
        abstract _flush: callback: Function -> unit

    type [<AllowNullLiteral>] TransformStatic = 
        [<Emit("new $0($1)")>] abstract Create: transformOptions:TransformOptions -> Transform
        [<Emit("new $0($1)")>] abstract Create: transformOptions:TransformBufferOptions -> Transform


    type [<AllowNullLiteral>] PassThrough =
        inherit Transform

    type [<AllowNullLiteral>] PassThroughStatic = 
        [<Emit("new $0()")>] abstract Create: unit -> PassThrough

    type Globals =
        abstract Stream: StreamStatic with get, set
        abstract Readable: ReadableStatic with get, set
        abstract Writable: WritableStatic with get, set
        abstract Duplex: DuplexStatic with get, set
        abstract Transform: TransformStatic with get, set
        abstract PassThrough: PassThroughStatic with get, set

[<Import("*", "stream")>]
let stream: stream_types.Globals = jsNative