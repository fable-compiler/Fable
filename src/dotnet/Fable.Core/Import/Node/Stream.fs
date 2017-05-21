module rec Fable.Import.Node.Stream

open Fable.Core
open Fable.Import.JS
open Fable.Import.Node

type [<AllowNullLiteral>] Stream =
    inherit Events.EventEmitter
    abstract pipe: destination: Writable<'a> * ?options: obj -> Writable<'a>

type [<AllowNullLiteral>] StreamStatic = 
    [<Emit("new $0()")>] abstract Create: unit -> Stream

type [<AllowNullLiteral>] ReadableOptions =
    abstract highWaterMark: float option with get, set
    abstract encoding: string option with get, set
    abstract objectMode: bool option with get, set
    abstract read<'a> : (float -> 'a) with get, set

type [<AllowNullLiteral>] Readable<'a> =
    inherit Stream
    abstract readable: bool with get, set
    abstract push: chunk: obj * ?encoding: string -> bool
    abstract unshift: chunk: obj -> unit
    abstract unpipe: ?destination: 'T -> unit
    abstract wrap: oldStream: Readable<'a> -> Readable<'a>
    abstract pause: unit -> Readable<'a>
    abstract resume: unit -> Readable<'a>
    abstract isPaused: unit -> bool
    abstract setEncoding: string -> unit
    abstract read: ?size: int -> 'a

type [<AllowNullLiteral>] ReadableStatic =
    [<Emit("new $0($1)")>] abstract Create<'a> : readableOptions:ReadableOptions -> Readable<'a>
    [<Emit("new $0($1)")>] abstract Create<'a> : unit -> Readable<'a>

type WritevChunk<'a> = {
    chunk: 'a;
    encoding: string
}

type [<AllowNullLiteral>] WritableOptions =
    abstract highWaterMark: float option with get, set
    abstract decodeStrings: bool option with get, set
    abstract objectMode: bool option with get, set
    abstract write<'a> : ('a -> string -> (Error option -> unit) -> unit) with get, set
    abstract writev<'a> : Option<WritevChunk<'a> -> string -> (Error option -> unit) -> unit> with get, set

type [<AllowNullLiteral>] Writable<'a> =
    inherit Stream
    abstract writable: bool with get, set
    abstract write: chunk: obj * ?cb: Function -> bool
    abstract ``end``:unit -> unit
    abstract ``end``:obj: obj -> unit
    abstract ``end``: obj: obj * cb: Function -> unit
    abstract ``end``: obj: obj * encoding: string * cb: Function -> unit

type [<AllowNullLiteral>] WritableStatic =
    [<Emit("new $0($1)")>] abstract Create<'a> : writableOptions:WritableOptions -> Writable<'a>

type DuplexOptions = {
    decodeStrings: bool option;
    encoding: string;
    objectMode: bool option;
    allowHalfOpen: bool option;
    readableObjectMode: bool option;
    writableObjectMode: bool option;
    read: float -> obj option;
    write: U2<string, Buffer.Buffer> -> string -> Function -> obj option;
    writev: ResizeArray<obj> -> Function -> obj option;
}

type [<AllowNullLiteral>] Duplex<'a, 'b> =
    inherit Readable<'a>
    inherit Writable<'b>

type [<AllowNullLiteral>] DuplexStatic =
    [<Emit("new $0($1)")>] abstract Create<'a, 'b> : duplexOptions:DuplexOptions -> Duplex<'a, 'b>

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
    abstract transform<'a> : (Buffer.Buffer -> string -> (Error option -> 'a option -> unit) -> unit) with get, set
    abstract flush: Option<(Error option -> unit) -> unit> with get, set

type [<AllowNullLiteral>] Transform<'a, 'b> =
    inherit Readable<'a>
    inherit Writable<'b>
    abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
    abstract _flush: callback: Function -> unit

type [<AllowNullLiteral>] TransformStatic =
    [<Emit("new $0($1)")>] abstract Create<'a, 'b> : transformOptions:TransformOptions -> Transform<'a, 'b>
    [<Emit("new $0($1)")>] abstract Create<'a, 'b> : transformOptions:TransformBufferOptions -> Transform<'a, 'b>


type [<AllowNullLiteral>] PassThrough<'a, 'b> =
    inherit Transform<'a, 'b>

type [<AllowNullLiteral>] PassThroughStatic =
    [<Emit("new $0()")>] abstract Create<'a, 'b> : unit -> PassThrough<'a, 'b>

type IExports =
    abstract Stream: StreamStatic with get, set
    abstract Readable: ReadableStatic with get, set
    abstract Writable: WritableStatic with get, set
    abstract Duplex: DuplexStatic with get, set
    abstract Transform: TransformStatic with get, set
    abstract PassThrough: PassThroughStatic with get, set
