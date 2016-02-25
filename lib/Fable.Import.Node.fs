namespace Fable.Import.Node
open System

type private ImportAttribute(path) =
    inherit Attribute()

type private GlobalAttribute() =
    inherit Attribute()
    
type EraseAttribute() =
    inherit System.Attribute()
    
type [<Erase>] U2<'a, 'b> =
    | Case1 of 'a | Case2 of 'b  
    
type Error =
    abstract stack: string option with get, set

and MapConstructor =
    interface end

and WeakMapConstructor =
    interface end

and SetConstructor =
    interface end

and WeakSetConstructor =
    interface end

and NodeRequireFunction =
    interface end

and NodeRequire =
    inherit NodeRequireFunction
    abstract cache: obj with get, set
    abstract extensions: obj with get, set
    abstract main: obj with get, set
    abstract resolve: id: string -> string

and NodeModule =
    abstract exports: obj with get, set
    abstract require: NodeRequireFunction with get, set
    abstract id: string with get, set
    abstract filename: string with get, set
    abstract loaded: bool with get, set
    abstract parent: obj with get, set
    abstract children: ResizeArray<obj> with get, set

and Buffer =
    inherit NodeBuffer

and NodeBuffer =
    abstract length: float with get, set
    abstract write: string: string * ?offset: float * ?length: float * ?encoding: string -> float
    abstract toString: ?encoding: string * ?start: float * ?``end``: float -> string
    abstract toJSON: unit -> obj
    abstract equals: otherBuffer: Buffer -> bool
    abstract compare: otherBuffer: Buffer -> float
    abstract copy: targetBuffer: Buffer * ?targetStart: float * ?sourceStart: float * ?sourceEnd: float -> float
    abstract slice: ?start: float * ?``end``: float -> Buffer
    abstract writeUIntLE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
    abstract writeUIntBE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
    abstract writeIntLE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
    abstract writeIntBE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
    abstract readUIntLE: offset: float * byteLength: float * ?noAssert: bool -> float
    abstract readUIntBE: offset: float * byteLength: float * ?noAssert: bool -> float
    abstract readIntLE: offset: float * byteLength: float * ?noAssert: bool -> float
    abstract readIntBE: offset: float * byteLength: float * ?noAssert: bool -> float
    abstract readUInt8: offset: float * ?noAsset: bool -> float
    abstract readUInt16LE: offset: float * ?noAssert: bool -> float
    abstract readUInt16BE: offset: float * ?noAssert: bool -> float
    abstract readUInt32LE: offset: float * ?noAssert: bool -> float
    abstract readUInt32BE: offset: float * ?noAssert: bool -> float
    abstract readInt8: offset: float * ?noAssert: bool -> float
    abstract readInt16LE: offset: float * ?noAssert: bool -> float
    abstract readInt16BE: offset: float * ?noAssert: bool -> float
    abstract readInt32LE: offset: float * ?noAssert: bool -> float
    abstract readInt32BE: offset: float * ?noAssert: bool -> float
    abstract readFloatLE: offset: float * ?noAssert: bool -> float
    abstract readFloatBE: offset: float * ?noAssert: bool -> float
    abstract readDoubleLE: offset: float * ?noAssert: bool -> float
    abstract readDoubleBE: offset: float * ?noAssert: bool -> float
    abstract writeUInt8: value: float * offset: float * ?noAssert: bool -> float
    abstract writeUInt16LE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeUInt16BE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeUInt32LE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeUInt32BE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeInt8: value: float * offset: float * ?noAssert: bool -> float
    abstract writeInt16LE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeInt16BE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeInt32LE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeInt32BE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeFloatLE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeFloatBE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeDoubleLE: value: float * offset: float * ?noAssert: bool -> float
    abstract writeDoubleBE: value: float * offset: float * ?noAssert: bool -> float
    abstract fill: value: obj * ?offset: float * ?``end``: float -> Buffer

module NodeJS =
    type ErrnoException =
        inherit Error
        abstract errno: float option with get, set
        abstract code: string option with get, set
        abstract path: string option with get, set
        abstract syscall: string option with get, set
        abstract stack: string option with get, set

    and EventEmitter =
        abstract addListener: ``event``: string * listener: (obj->obj) -> EventEmitter
        abstract on: ``event``: string * listener: (obj->obj) -> EventEmitter
        abstract once: ``event``: string * listener: (obj->obj) -> EventEmitter
        abstract removeListener: ``event``: string * listener: (obj->obj) -> EventEmitter
        abstract removeAllListeners: ?``event``: string -> EventEmitter
        abstract setMaxListeners: n: float -> unit
        abstract listeners: ``event``: string -> ResizeArray<(obj->obj)>
        abstract emit: ``event``: string * [<ParamArray>] args: obj[] -> bool

    and ReadableStream =
        inherit EventEmitter
        abstract readable: bool with get, set
        abstract read: ?size: float -> U2<string, Buffer>
        abstract setEncoding: encoding: string -> unit
        abstract pause: unit -> unit
        abstract resume: unit -> unit
        abstract pipe: destination: 'T * ?options: obj -> 'T
        abstract unpipe: ?destination: 'T -> unit
        abstract unshift: chunk: U2<string, Buffer> -> unit
        abstract wrap: oldStream: ReadableStream -> ReadableStream

    and WritableStream =
        inherit EventEmitter
        abstract writable: bool with get, set
        abstract write: buffer: U2<Buffer, string> * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?cb: (obj->obj) -> bool
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: Buffer * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: (obj->obj) -> unit

    and ReadWriteStream =
        inherit ReadableStream
        inherit WritableStream

    and Process =
        inherit EventEmitter
        abstract stdout: WritableStream with get, set
        abstract stderr: WritableStream with get, set
        abstract stdin: ReadableStream with get, set
        abstract argv: ResizeArray<string> with get, set
        abstract execPath: string with get, set
        abstract env: obj with get, set
        abstract version: string with get, set
        abstract versions: obj with get, set
        abstract config: obj with get, set
        abstract pid: float with get, set
        abstract title: string with get, set
        abstract arch: string with get, set
        abstract platform: string with get, set
        abstract abort: unit -> unit
        abstract chdir: directory: string -> unit
        abstract cwd: unit -> string
        abstract exit: ?code: float -> unit
        abstract getgid: unit -> float
        abstract setgid: id: float -> unit
        abstract setgid: id: string -> unit
        abstract getuid: unit -> float
        abstract setuid: id: float -> unit
        abstract setuid: id: string -> unit
        abstract kill: pid: float * ?signal: string -> unit
        abstract memoryUsage: unit -> obj
        abstract nextTick: callback: (obj->obj) -> unit
        abstract umask: ?mask: float -> float
        abstract uptime: unit -> float
        abstract hrtime: ?time: ResizeArray<float> -> ResizeArray<float>
        abstract send: message: obj * ?sendHandle: obj -> unit

    and Global =
        abstract Array: obj with get, set
        abstract ArrayBuffer: obj with get, set
        abstract Boolean: obj with get, set
        abstract Buffer: obj with get, set
        abstract DataView: obj with get, set
        abstract Date: obj with get, set
        abstract Error: obj with get, set
        abstract EvalError: obj with get, set
        abstract Float32Array: obj with get, set
        abstract Float64Array: obj with get, set
        abstract Function: obj with get, set
        abstract GLOBAL: Global with get, set
        abstract Infinity: obj with get, set
        abstract Int16Array: obj with get, set
        abstract Int32Array: obj with get, set
        abstract Int8Array: obj with get, set
        abstract Intl: obj with get, set
        abstract JSON: obj with get, set
        abstract Map: MapConstructor with get, set
        abstract Math: obj with get, set
        abstract NaN: obj with get, set
        abstract Number: obj with get, set
        abstract Object: obj with get, set
        abstract Promise: (obj->obj) with get, set
        abstract RangeError: obj with get, set
        abstract ReferenceError: obj with get, set
        abstract RegExp: obj with get, set
        abstract Set: SetConstructor with get, set
        abstract String: obj with get, set
        abstract Symbol: (obj->obj) with get, set
        abstract SyntaxError: obj with get, set
        abstract TypeError: obj with get, set
        abstract URIError: obj with get, set
        abstract Uint16Array: obj with get, set
        abstract Uint32Array: obj with get, set
        abstract Uint8Array: obj with get, set
        abstract Uint8ClampedArray: (obj->obj) with get, set
        abstract WeakMap: WeakMapConstructor with get, set
        abstract WeakSet: WeakSetConstructor with get, set
        abstract clearImmediate: Func<obj, unit> with get, set
        abstract clearInterval: Func<Timer, unit> with get, set
        abstract clearTimeout: Func<Timer, unit> with get, set
        abstract console: obj with get, set
        abstract decodeURI: obj with get, set
        abstract decodeURIComponent: obj with get, set
        abstract encodeURI: obj with get, set
        abstract encodeURIComponent: obj with get, set
        abstract escape: Func<string, string> with get, set
        abstract eval: obj with get, set
        abstract ``global``: Global with get, set
        abstract isFinite: obj with get, set
        abstract isNaN: obj with get, set
        abstract parseFloat: obj with get, set
        abstract parseInt: obj with get, set
        abstract ``process``: Process with get, set
        abstract root: Global with get, set
        abstract setImmediate: Func<Func<obj, unit>, obj, obj> with get, set
        abstract setInterval: Func<Func<obj, unit>, float, obj, Timer> with get, set
        abstract setTimeout: Func<Func<obj, unit>, float, obj, Timer> with get, set
        abstract undefined: obj with get, set
        abstract unescape: Func<string, string> with get, set
        abstract gc: Func<unit> with get, set

    and Timer =
        abstract ref: unit -> unit
        abstract unref: unit -> unit

module Globals =
    let [<Global>] ``process``: NodeJS.Process = failwith "JS only"
    let [<Global>] ``global``: NodeJS.Global = failwith "JS only"
    let [<Global>] ___filename: string = failwith "JS only"
    let [<Global>] ___dirname: string = failwith "JS only"
    let [<Global>] require: NodeRequire = failwith "JS only"
    let [<Global>] ``module``: NodeModule = failwith "JS only"
    let [<Global>] exports: obj = failwith "JS only"
    let [<Global>] SlowBuffer: obj = failwith "JS only"
    let [<Global>] Buffer: obj = failwith "JS only"


module buffer =
    type Globals =
        abstract INSPECT_MAX_BYTES: float with get, set

    let [<Import("buffer")>] Globals: Globals = failwith "JS only"

module querystring =
    type Globals =
        abstract stringify: obj: obj * ?sep: string * ?eq: string -> string
        abstract parse: str: string * ?sep: string * ?eq: string * ?options: obj -> obj
        abstract escape: str: string -> string
        abstract unescape: str: string -> string

    let [<Import("querystring")>] Globals: Globals = failwith "JS only"

module events =
    type [<Import("events?get=EventEmitter")>] EventEmitter() =
        member __.listenerCount(emitter: EventEmitter, ``event``: string): float = failwith "JS only"
        interface NodeJS.EventEmitter with
            member __.addListener(``event``: string, listener: (obj->obj)): NodeJS.EventEmitter = failwith "JS only"
            member __.on(``event``: string, listener: (obj->obj)): NodeJS.EventEmitter = failwith "JS only"
            member __.once(``event``: string, listener: (obj->obj)): NodeJS.EventEmitter = failwith "JS only"
            member __.removeListener(``event``: string, listener: (obj->obj)): NodeJS.EventEmitter = failwith "JS only"
            member __.removeAllListeners(?``event``: string): NodeJS.EventEmitter = failwith "JS only"
            member __.setMaxListeners(n: float): unit = failwith "JS only"
            member __.listeners(``event``: string): ResizeArray<(obj->obj)> = failwith "JS only"
            member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = failwith "JS only"

module stream =
    type Stream =
        // inherit events.EventEmitter() // TODO
        abstract pipe: destination: 'T * ?options: obj -> 'T

    and ReadableOptions =
        abstract highWaterMark: float option with get, set
        abstract encoding: string option with get, set
        abstract objectMode: bool option with get, set

    and [<Import("stream?get=Readable")>] Readable(?opts: ReadableOptions) =
        inherit events.EventEmitter()
        member __._read(size: float): unit = failwith "JS only"
        member __.push(chunk: obj, ?encoding: string): bool = failwith "JS only"
        // interface NodeJS.ReadableStream // TODO
        member __.readable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.read(?size: float): U2<string,Buffer> = failwith "JS only"
        member __.setEncoding(encoding: string): unit = failwith "JS only"
        member __.pause(): unit = failwith "JS only"
        member __.resume(): unit = failwith "JS only"
        member __.pipe(destination: 'T, ?options: obj): 'T = failwith "JS only"
        member __.unpipe(?destination: 'T): unit = failwith "JS only"
        member __.unshift(chunk: U2<string, Buffer>): unit = failwith "JS only"
        member __.wrap(oldStream: NodeJS.ReadableStream): NodeJS.ReadableStream = failwith "JS only"

    and WritableOptions =
        abstract highWaterMark: float option with get, set
        abstract decodeStrings: bool option with get, set
        abstract objectMode: bool option with get, set

    and [<Import("stream?get=Writable")>] Writable(?opts: WritableOptions) =
        inherit events.EventEmitter()
        // interface NodeJS.WritableStream // TODO
        member __.writable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __._write(chunk: obj, encoding: string, callback: (obj->obj)): unit = failwith "JS only"
        member __.write(chunk: obj, ?cb: (obj->obj)): bool = failwith "JS only"
        member __.write(chunk: obj, ?encoding: string, ?cb: (obj->obj)): bool = failwith "JS only"
        member __.``end``(): unit = failwith "JS only"
        member __.``end``(chunk: obj, ?cb: (obj->obj)): unit = failwith "JS only"
        member __.``end``(chunk: obj, ?encoding: string, ?cb: (obj->obj)): unit = failwith "JS only"

    and DuplexOptions =
        inherit ReadableOptions
        inherit WritableOptions
        abstract allowHalfOpen: bool option with get, set

    and [<Import("stream?get=Duplex")>] Duplex(?opts: DuplexOptions) =
        inherit Readable()
        // interface NodeJS.ReadWriteStream // TODO
        member __.writable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __._write(chunk: obj, encoding: string, callback: (obj->obj)): unit = failwith "JS only"
        member __.write(chunk: obj, ?cb: (obj->obj)): bool = failwith "JS only"
        member __.write(chunk: obj, ?encoding: string, ?cb: (obj->obj)): bool = failwith "JS only"
        member __.``end``(): unit = failwith "JS only"
        member __.``end``(chunk: obj, ?cb: (obj->obj)): unit = failwith "JS only"
        member __.``end``(chunk: obj, ?encoding: string, ?cb: (obj->obj)): unit = failwith "JS only"

    and TransformOptions =
        inherit ReadableOptions
        inherit WritableOptions

    and [<Import("stream?get=Transform")>] Transform(?opts: TransformOptions) =
        inherit events.EventEmitter()
        // interface NodeJS.ReadWriteStream // TODO
        member __.readable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.writable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __._transform(chunk: obj, encoding: string, callback: (obj->obj)): unit = failwith "JS only"
        member __._flush(callback: (obj->obj)): unit = failwith "JS only"
        member __.read(?size: float): obj = failwith "JS only"
        member __.setEncoding(encoding: string): unit = failwith "JS only"
        member __.pause(): unit = failwith "JS only"
        member __.resume(): unit = failwith "JS only"
        member __.pipe(destination: 'T, ?options: obj): 'T = failwith "JS only"
        member __.unpipe(?destination: 'T): unit = failwith "JS only"
        member __.unshift(chunk: obj): unit = failwith "JS only"
        member __.wrap(oldStream: NodeJS.ReadableStream): NodeJS.ReadableStream = failwith "JS only"
        member __.push(chunk: obj, ?encoding: string): bool = failwith "JS only"
        member __.write(chunk: obj, ?cb: (obj->obj)): bool = failwith "JS only"
        member __.write(chunk: obj, ?encoding: string, ?cb: (obj->obj)): bool = failwith "JS only"
        member __.``end``(): unit = failwith "JS only"
        member __.``end``(chunk: obj, ?cb: (obj->obj)): unit = failwith "JS only"
        member __.``end``(chunk: obj, ?encoding: string, ?cb: (obj->obj)): unit = failwith "JS only"

    and [<Import("stream?get=PassThrough")>] PassThrough(?opts) =
        inherit Transform()


module net =
    type Socket =
        // inherit stream.Duplex // TODO
        abstract bufferSize: float with get, set
        abstract remoteAddress: string with get, set
        abstract remoteFamily: string with get, set
        abstract remotePort: float with get, set
        abstract localAddress: string with get, set
        abstract localPort: float with get, set
        abstract bytesRead: float with get, set
        abstract bytesWritten: float with get, set
        abstract write: buffer: Buffer -> bool
        abstract write: buffer: Buffer * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?fd: string -> bool
        abstract connect: port: float * ?host: string * ?connectionListener: (obj->obj) -> unit
        abstract connect: path: string * ?connectionListener: (obj->obj) -> unit
        abstract setEncoding: ?encoding: string -> unit
        abstract write: data: obj * ?encoding: string * ?callback: (obj->obj) -> unit
        abstract destroy: unit -> unit
        abstract pause: unit -> unit
        abstract resume: unit -> unit
        abstract setTimeout: timeout: float * ?callback: (obj->obj) -> unit
        abstract setNoDelay: ?noDelay: bool -> unit
        abstract setKeepAlive: ?enable: bool * ?initialDelay: float -> unit
        abstract address: unit -> obj
        abstract unref: unit -> unit
        abstract ref: unit -> unit
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: Buffer * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: (obj->obj) -> unit
        abstract ``end``: ?data: obj * ?encoding: string -> unit

    and Server =
        inherit Socket
        abstract maxConnections: float with get, set
        abstract connections: float with get, set
        abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: (obj->obj) -> Server
        abstract listen: path: string * ?listeningListener: (obj->obj) -> Server
        abstract listen: handle: obj * ?listeningListener: (obj->obj) -> Server
        abstract close: ?callback: (obj->obj) -> Server
        abstract address: unit -> obj

    type Globals =
        abstract Socket: obj with get, set
        abstract createServer: ?connectionListener: Func<Socket, unit> -> Server
        abstract createServer: ?options: obj * ?connectionListener: Func<Socket, unit> -> Server
        abstract connect: options: obj * ?connectionListener: (obj->obj) -> Socket
        abstract connect: port: float * ?host: string * ?connectionListener: (obj->obj) -> Socket
        abstract connect: path: string * ?connectionListener: (obj->obj) -> Socket
        abstract createConnection: options: obj * ?connectionListener: (obj->obj) -> Socket
        abstract createConnection: port: float * ?host: string * ?connectionListener: (obj->obj) -> Socket
        abstract createConnection: path: string * ?connectionListener: (obj->obj) -> Socket
        abstract isIP: input: string -> float
        abstract isIPv4: input: string -> bool
        abstract isIPv6: input: string -> bool

    let [<Import("net")>] Globals: Globals = failwith "JS only"

module http =
    type Server =
        // inherit events.EventEmitter() // TODO
        abstract maxHeadersCount: float with get, set
        abstract listen: port: float * ?hostname: string * ?backlog: float * ?callback: (obj->obj) -> Server
        abstract listen: port: float * ?hostname: string * ?callback: (obj->obj) -> Server
        abstract listen: path: string * ?callback: (obj->obj) -> Server
        abstract listen: handle: obj * ?listeningListener: (obj->obj) -> Server
        abstract close: ?cb: obj -> Server
        abstract address: unit -> obj

    and ServerRequest =
        // inherit IncomingMessage // TODO
        abstract connection: net.Socket with get, set

    and ServerResponse =
        // inherit events.EventEmitter() // TODO
        // inherit stream.Writable // TODO
        abstract statusCode: float with get, set
        abstract statusMessage: string with get, set
        abstract sendDate: bool with get, set
        abstract write: buffer: Buffer -> bool
        abstract write: buffer: Buffer * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?fd: string -> bool
        abstract writeContinue: unit -> unit
        abstract writeHead: statusCode: float * ?reasonPhrase: string * ?headers: obj -> unit
        abstract writeHead: statusCode: float * ?headers: obj -> unit
        abstract setHeader: name: string * value: string -> unit
        abstract getHeader: name: string -> string
        abstract removeHeader: name: string -> unit
        abstract write: chunk: obj * ?encoding: string -> obj
        abstract addTrailers: headers: obj -> unit
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: Buffer * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: (obj->obj) -> unit
        abstract ``end``: ?data: obj * ?encoding: string -> unit

    and ClientRequest =
        // inherit events.EventEmitter() // TODO
        // inherit stream.Writable // TODO
        abstract write: buffer: Buffer -> bool
        abstract write: buffer: Buffer * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?cb: (obj->obj) -> bool
        abstract write: str: string * ?encoding: string * ?fd: string -> bool
        abstract write: chunk: obj * ?encoding: string -> unit
        abstract abort: unit -> unit
        abstract setTimeout: timeout: float * ?callback: (obj->obj) -> unit
        abstract setNoDelay: ?noDelay: bool -> unit
        abstract setSocketKeepAlive: ?enable: bool * ?initialDelay: float -> unit
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: Buffer * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?cb: (obj->obj) -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: (obj->obj) -> unit
        abstract ``end``: ?data: obj * ?encoding: string -> unit

    and IncomingMessage =
        // inherit events.EventEmitter() // TODO
        // inherit stream.Readable // TODO
        abstract httpVersion: string with get, set
        abstract headers: obj with get, set
        abstract rawHeaders: ResizeArray<string> with get, set
        abstract trailers: obj with get, set
        abstract rawTrailers: obj with get, set
        abstract ``method``: string option with get, set
        abstract url: string option with get, set
        abstract statusCode: float option with get, set
        abstract statusMessage: string option with get, set
        abstract socket: net.Socket with get, set
        abstract setTimeout: msecs: float * callback: (obj->obj) -> NodeJS.Timer

    and ClientResponse =
        inherit IncomingMessage

    and AgentOptions =
        abstract keepAlive: bool option with get, set
        abstract keepAliveMsecs: float option with get, set
        abstract maxSockets: float option with get, set
        abstract maxFreeSockets: float option with get, set

    and [<Import("http?get=Agent")>] Agent(?opts: AgentOptions) =
        member __.maxSockets with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.sockets with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.requests with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.destroy(): unit = failwith "JS only"

    type Globals =
        abstract METHODS: ResizeArray<string> with get, set
        abstract STATUS_CODES: obj with get, set
        abstract globalAgent: Agent with get, set
        abstract createServer: ?requestListener: Func<IncomingMessage, ServerResponse, unit> -> Server
        abstract createClient: ?port: float * ?host: string -> obj
        abstract request: options: obj * ?callback: Func<IncomingMessage, unit> -> ClientRequest
        abstract get: options: obj * ?callback: Func<IncomingMessage, unit> -> ClientRequest

    let [<Import("http")>] Globals: Globals = failwith "JS only"


module child_process =
    type ChildProcess =
        // inherit events.EventEmitter() // TODO
        abstract stdin: stream.Writable with get, set
        abstract stdout: stream.Readable with get, set
        abstract stderr: stream.Readable with get, set
        abstract pid: float with get, set
        abstract kill: ?signal: string -> unit
        abstract send: message: obj * ?sendHandle: obj -> unit
        abstract disconnect: unit -> unit
        abstract unref: unit -> unit

    type Globals =
        abstract spawn: command: string * ?args: ResizeArray<string> * ?options: obj -> ChildProcess
        abstract exec: command: string * options: obj * ?callback: Func<Error, Buffer, Buffer, unit> -> ChildProcess
        abstract exec: command: string * ?callback: Func<Error, Buffer, Buffer, unit> -> ChildProcess
        abstract execFile: file: string * ?callback: Func<Error, Buffer, Buffer, unit> -> ChildProcess
        abstract execFile: file: string * ?args: ResizeArray<string> * ?callback: Func<Error, Buffer, Buffer, unit> -> ChildProcess
        abstract execFile: file: string * ?args: ResizeArray<string> * ?options: obj * ?callback: Func<Error, Buffer, Buffer, unit> -> ChildProcess
        abstract fork: modulePath: string * ?args: ResizeArray<string> * ?options: obj -> ChildProcess
        abstract spawnSync: command: string * ?args: ResizeArray<string> * ?options: obj -> obj
        abstract execSync: command: string * ?options: obj -> U2<string, Buffer>
        abstract execFileSync: command: string * ?args: ResizeArray<string> * ?options: obj -> U2<string, Buffer>

    let [<Import("child_process")>] Globals: Globals = failwith "JS only"


module cluster =
    type ClusterSettings =
        abstract exec: string option with get, set
        abstract args: ResizeArray<string> option with get, set
        abstract silent: bool option with get, set

    and [<Import("cluster?get=Worker")>] Worker() =
        inherit events.EventEmitter()
        member __.id with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.``process`` with get(): child_process.ChildProcess = failwith "JS only" and set(v: child_process.ChildProcess): unit = failwith "JS only"
        member __.suicide with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.send(message: obj, ?sendHandle: obj): unit = failwith "JS only"
        member __.kill(?signal: string): unit = failwith "JS only"
        member __.destroy(?signal: string): unit = failwith "JS only"
        member __.disconnect(): unit = failwith "JS only"

    type Globals =
        abstract settings: ClusterSettings with get, set
        abstract isMaster: bool with get, set
        abstract isWorker: bool with get, set
        abstract worker: Worker with get, set
        abstract workers: ResizeArray<Worker> with get, set
        abstract setupMaster: ?settings: ClusterSettings -> unit
        abstract fork: ?env: obj -> Worker
        abstract disconnect: ?callback: (obj->obj) -> unit
        abstract addListener: ``event``: string * listener: (obj->obj) -> unit
        abstract on: ``event``: string * listener: (obj->obj) -> obj
        abstract once: ``event``: string * listener: (obj->obj) -> unit
        abstract removeListener: ``event``: string * listener: (obj->obj) -> unit
        abstract removeAllListeners: ?``event``: string -> unit
        abstract setMaxListeners: n: float -> unit
        abstract listeners: ``event``: string -> ResizeArray<(obj->obj)>
        abstract emit: ``event``: string * [<ParamArray>] args: obj[] -> bool

    let [<Import("cluster")>] Globals: Globals = failwith "JS only"


module zlib =
    type ZlibOptions =
        abstract chunkSize: float option with get, set
        abstract windowBits: float option with get, set
        abstract level: float option with get, set
        abstract memLevel: float option with get, set
        abstract strategy: float option with get, set
        abstract dictionary: obj option with get, set

    and Gzip() =
        inherit stream.Transform()

    and Gunzip() =
        inherit stream.Transform()

    and Deflate() =
        inherit stream.Transform()

    and Inflate() =
        inherit stream.Transform()

    and DeflateRaw() =
        inherit stream.Transform()

    and InflateRaw() =
        inherit stream.Transform()

    and Unzip() =
        inherit stream.Transform()

    type Globals =
        abstract Z_NO_FLUSH: float with get, set
        abstract Z_PARTIAL_FLUSH: float with get, set
        abstract Z_SYNC_FLUSH: float with get, set
        abstract Z_FULL_FLUSH: float with get, set
        abstract Z_FINISH: float with get, set
        abstract Z_BLOCK: float with get, set
        abstract Z_TREES: float with get, set
        abstract Z_OK: float with get, set
        abstract Z_STREAM_END: float with get, set
        abstract Z_NEED_DICT: float with get, set
        abstract Z_ERRNO: float with get, set
        abstract Z_STREAM_ERROR: float with get, set
        abstract Z_DATA_ERROR: float with get, set
        abstract Z_MEM_ERROR: float with get, set
        abstract Z_BUF_ERROR: float with get, set
        abstract Z_VERSION_ERROR: float with get, set
        abstract Z_NO_COMPRESSION: float with get, set
        abstract Z_BEST_SPEED: float with get, set
        abstract Z_BEST_COMPRESSION: float with get, set
        abstract Z_DEFAULT_COMPRESSION: float with get, set
        abstract Z_FILTERED: float with get, set
        abstract Z_HUFFMAN_ONLY: float with get, set
        abstract Z_RLE: float with get, set
        abstract Z_FIXED: float with get, set
        abstract Z_DEFAULT_STRATEGY: float with get, set
        abstract Z_BINARY: float with get, set
        abstract Z_TEXT: float with get, set
        abstract Z_ASCII: float with get, set
        abstract Z_UNKNOWN: float with get, set
        abstract Z_DEFLATED: float with get, set
        abstract Z_NULL: float with get, set
        abstract createGzip: ?options: ZlibOptions -> Gzip
        abstract createGunzip: ?options: ZlibOptions -> Gunzip
        abstract createDeflate: ?options: ZlibOptions -> Deflate
        abstract createInflate: ?options: ZlibOptions -> Inflate
        abstract createDeflateRaw: ?options: ZlibOptions -> DeflateRaw
        abstract createInflateRaw: ?options: ZlibOptions -> InflateRaw
        abstract createUnzip: ?options: ZlibOptions -> Unzip
        abstract deflate: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract deflateSync: buf: Buffer * ?options: ZlibOptions -> obj
        abstract deflateRaw: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract deflateRawSync: buf: Buffer * ?options: ZlibOptions -> obj
        abstract gzip: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract gzipSync: buf: Buffer * ?options: ZlibOptions -> obj
        abstract gunzip: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract gunzipSync: buf: Buffer * ?options: ZlibOptions -> obj
        abstract inflate: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract inflateSync: buf: Buffer * ?options: ZlibOptions -> obj
        abstract inflateRaw: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract inflateRawSync: buf: Buffer * ?options: ZlibOptions -> obj
        abstract unzip: buf: Buffer * callback: Func<Error, obj, unit> -> unit
        abstract unzipSync: buf: Buffer * ?options: ZlibOptions -> obj

    let [<Import("zlib")>] Globals: Globals = failwith "JS only"


module os =
    type Globals =
        abstract EOL: string with get, set
        abstract tmpdir: unit -> string
        abstract hostname: unit -> string
        abstract ``type``: unit -> string
        abstract platform: unit -> string
        abstract arch: unit -> string
        abstract release: unit -> string
        abstract uptime: unit -> float
        abstract loadavg: unit -> ResizeArray<float>
        abstract totalmem: unit -> float
        abstract freemem: unit -> float
        abstract cpus: unit -> ResizeArray<obj>
        abstract networkInterfaces: unit -> obj

    let [<Import("os")>] Globals: Globals = failwith "JS only"


module crypto =
    type CredentialDetails =
        abstract pfx: string with get, set
        abstract key: string with get, set
        abstract passphrase: string with get, set
        abstract cert: string with get, set
        abstract ca: obj with get, set
        abstract crl: obj with get, set
        abstract ciphers: string with get, set

    and Credentials =
        abstract context: obj option with get, set

    and Hash =
        abstract update: data: obj * ?input_encoding: string -> Hash
        abstract digest: encoding: obj -> Buffer
        abstract digest: encoding: string -> obj
        abstract digest: unit -> Buffer

    and Hmac =
        abstract update: data: obj * ?input_encoding: string -> Hmac
        abstract digest: encoding: obj -> Buffer
        abstract digest: encoding: string -> obj
        abstract digest: unit -> Buffer

    and Cipher =
        abstract update: data: Buffer -> Buffer
        abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract final: unit -> Buffer
        abstract final: output_encoding: string -> string
        abstract setAutoPadding: auto_padding: bool -> unit

    and Decipher =
        abstract update: data: Buffer -> Buffer
        abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract final: unit -> Buffer
        abstract final: output_encoding: string -> string
        abstract setAutoPadding: auto_padding: bool -> unit

    and Signer =
        inherit NodeJS.WritableStream
        abstract update: data: obj -> unit
        abstract sign: private_key: string * output_format: string -> string

    and Verify =
        inherit NodeJS.WritableStream
        abstract update: data: obj -> unit
        abstract verify: ``object``: string * signature: string * ?signature_format: string -> bool

    and DiffieHellman =
        abstract generateKeys: ?encoding: string -> string
        abstract computeSecret: other_public_key: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract getPrime: ?encoding: string -> string
        abstract getGenerator: encoding: string -> string
        abstract getPublicKey: ?encoding: string -> string
        abstract getPrivateKey: ?encoding: string -> string
        abstract setPublicKey: public_key: string * ?encoding: string -> unit
        abstract setPrivateKey: public_key: string * ?encoding: string -> unit

    type Globals =
        abstract createCredentials: details: CredentialDetails -> Credentials
        abstract createHash: algorithm: string -> Hash
        abstract createHmac: algorithm: string * key: string -> Hmac
        abstract createHmac: algorithm: string * key: Buffer -> Hmac
        abstract createCipher: algorithm: string * password: obj -> Cipher
        abstract createCipheriv: algorithm: string * key: obj * iv: obj -> Cipher
        abstract createDecipher: algorithm: string * password: obj -> Decipher
        abstract createDecipheriv: algorithm: string * key: obj * iv: obj -> Decipher
        abstract createSign: algorithm: string -> Signer
        abstract createVerify: algorith: string -> Verify
        abstract createDiffieHellman: prime_length: float -> DiffieHellman
        abstract createDiffieHellman: prime: float * ?encoding: string -> DiffieHellman
        abstract getDiffieHellman: group_name: string -> DiffieHellman
        abstract pbkdf2: password: string * salt: string * iterations: float * keylen: float * callback: Func<Error, Buffer, obj> -> unit
        abstract pbkdf2: password: string * salt: string * iterations: float * keylen: float * digest: string * callback: Func<Error, Buffer, obj> -> unit
        abstract pbkdf2Sync: password: string * salt: string * iterations: float * keylen: float -> Buffer
        abstract pbkdf2Sync: password: string * salt: string * iterations: float * keylen: float * digest: string -> Buffer
        abstract randomBytes: size: float -> Buffer
        abstract randomBytes: size: float * callback: Func<Error, Buffer, unit> -> unit
        abstract pseudoRandomBytes: size: float -> Buffer
        abstract pseudoRandomBytes: size: float * callback: Func<Error, Buffer, unit> -> unit

    let [<Import("crypto")>] Globals: Globals = failwith "JS only"


module tls =
    type TlsOptions =
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract crl: obj option with get, set
        abstract ciphers: string option with get, set
        abstract honorCipherOrder: obj option with get, set
        abstract requestCert: bool option with get, set
        abstract rejectUnauthorized: bool option with get, set
        abstract NPNProtocols: obj option with get, set
        abstract SNICallback: Func<string, obj> option with get, set

    and ConnectionOptions =
        abstract host: string option with get, set
        abstract port: float option with get, set
        abstract socket: net.Socket option with get, set
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract rejectUnauthorized: bool option with get, set
        abstract NPNProtocols: obj option with get, set
        abstract servername: string option with get, set

    and Server =
        inherit net.Server
        abstract maxConnections: float with get, set
        abstract connections: float with get, set
        abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: (obj->obj) -> Server
        abstract listen: path: string * ?listeningListener: (obj->obj) -> Server
        abstract listen: handle: obj * ?listeningListener: (obj->obj) -> Server
        abstract listen: port: float * ?host: string * ?callback: (obj->obj) -> Server
        abstract close: unit -> Server
        abstract address: unit -> obj
        abstract addContext: hostName: string * credentials: obj -> unit

    and ClearTextStream =
        // inherit stream.Duplex // TODO
        abstract authorized: bool with get, set
        abstract authorizationError: Error with get, set
        abstract getCipher: obj with get, set
        abstract address: obj with get, set
        abstract remoteAddress: string with get, set
        abstract remotePort: float with get, set
        abstract getPeerCertificate: unit -> obj

    and SecurePair =
        abstract encrypted: obj with get, set
        abstract cleartext: obj with get, set

    and SecureContextOptions =
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract crl: obj option with get, set
        abstract ciphers: string option with get, set
        abstract honorCipherOrder: bool option with get, set

    and SecureContext =
        abstract context: obj with get, set

    type Globals =
        abstract CLIENT_RENEG_LIMIT: float with get, set
        abstract CLIENT_RENEG_WINDOW: float with get, set
        abstract createServer: options: TlsOptions * ?secureConnectionListener: Func<ClearTextStream, unit> -> Server
        abstract connect: options: TlsOptions * ?secureConnectionListener: Func<unit> -> ClearTextStream
        abstract connect: port: float * ?host: string * ?options: ConnectionOptions * ?secureConnectListener: Func<unit> -> ClearTextStream
        abstract connect: port: float * ?options: ConnectionOptions * ?secureConnectListener: Func<unit> -> ClearTextStream
        abstract createSecurePair: ?credentials: crypto.Credentials * ?isServer: bool * ?requestCert: bool * ?rejectUnauthorized: bool -> SecurePair
        abstract createSecureContext: details: SecureContextOptions -> SecureContext

    let [<Import("tls")>] Globals: Globals = failwith "JS only"


module https =
    type ServerOptions =
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract crl: obj option with get, set
        abstract ciphers: string option with get, set
        abstract honorCipherOrder: bool option with get, set
        abstract requestCert: bool option with get, set
        abstract rejectUnauthorized: bool option with get, set
        abstract NPNProtocols: obj option with get, set
        abstract SNICallback: Func<string, obj> option with get, set

    and RequestOptions =
        abstract host: string option with get, set
        abstract hostname: string option with get, set
        abstract port: float option with get, set
        abstract path: string option with get, set
        abstract ``method``: string option with get, set
        abstract headers: obj option with get, set
        abstract auth: string option with get, set
        abstract agent: obj option with get, set
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract ciphers: string option with get, set
        abstract rejectUnauthorized: bool option with get, set

    and Agent =
        abstract maxSockets: float with get, set
        abstract sockets: obj with get, set
        abstract requests: obj with get, set

    and Server =
        inherit tls.Server

    type Globals =
        abstract Agent: obj with get, set
        abstract globalAgent: Agent with get, set
        abstract createServer: options: ServerOptions * ?requestListener: (obj->obj) -> Server
        abstract request: options: RequestOptions * ?callback: Func<http.IncomingMessage, unit> -> http.ClientRequest
        abstract get: options: RequestOptions * ?callback: Func<http.IncomingMessage, unit> -> http.ClientRequest

    let [<Import("https")>] Globals: Globals = failwith "JS only"


module punycode =
    type ucs2 =
        abstract decode: string: string -> string
        abstract encode: codePoints: ResizeArray<float> -> string

    type Globals =
        abstract ucs2: ucs2 with get, set
        abstract version: obj with get, set
        abstract decode: string: string -> string
        abstract encode: string: string -> string
        abstract toUnicode: domain: string -> string
        abstract toASCII: domain: string -> string

    let [<Import("punycode")>] Globals: Globals = failwith "JS only"


module repl =
    type ReplOptions =
        abstract prompt: string option with get, set
        abstract input: NodeJS.ReadableStream option with get, set
        abstract output: NodeJS.WritableStream option with get, set
        abstract terminal: bool option with get, set
        abstract eval: (obj->obj) option with get, set
        abstract useColors: bool option with get, set
        abstract useGlobal: bool option with get, set
        abstract ignoreUndefined: bool option with get, set
        abstract writer: (obj->obj) option with get, set

    type Globals =
        abstract start: options: ReplOptions -> events.EventEmitter

    let [<Import("repl")>] Globals: Globals = failwith "JS only"


module readline =
    type ReadLine =
        // inherit events.EventEmitter() // TODO
        abstract setPrompt: prompt: string -> unit
        abstract prompt: ?preserveCursor: bool -> unit
        abstract question: query: string * callback: (obj->obj) -> unit
        abstract pause: unit -> unit
        abstract resume: unit -> unit
        abstract close: unit -> unit
        abstract write: data: obj * ?key: obj -> unit

    and ReadLineOptions =
        abstract input: NodeJS.ReadableStream with get, set
        abstract output: NodeJS.WritableStream with get, set
        abstract completer: (obj->obj) option with get, set
        abstract terminal: bool option with get, set

    type Globals =
        abstract createInterface: options: ReadLineOptions -> ReadLine

    let [<Import("readline")>] Globals: Globals = failwith "JS only"


module vm =
    type Context =
        interface end

    and Script =
        abstract runInThisContext: unit -> unit
        abstract runInNewContext: ?sandbox: Context -> unit

    type Globals =
        abstract runInThisContext: code: string * ?filename: string -> unit
        abstract runInNewContext: code: string * ?sandbox: Context * ?filename: string -> unit
        abstract runInContext: code: string * context: Context * ?filename: string -> unit
        abstract createContext: ?initSandbox: Context -> Context
        abstract createScript: code: string * ?filename: string -> Script

    let [<Import("vm")>] Globals: Globals = failwith "JS only"

module url =
    type Url =
        abstract href: string with get, set
        abstract protocol: string with get, set
        abstract auth: string with get, set
        abstract hostname: string with get, set
        abstract port: string with get, set
        abstract host: string with get, set
        abstract pathname: string with get, set
        abstract search: string with get, set
        abstract query: obj with get, set
        abstract slashes: bool with get, set
        abstract hash: string option with get, set
        abstract path: string option with get, set

    and UrlOptions =
        abstract protocol: string option with get, set
        abstract auth: string option with get, set
        abstract hostname: string option with get, set
        abstract port: string option with get, set
        abstract host: string option with get, set
        abstract pathname: string option with get, set
        abstract search: string option with get, set
        abstract query: obj option with get, set
        abstract hash: string option with get, set
        abstract path: string option with get, set

    type Globals =
        abstract parse: urlStr: string * ?parseQueryString: bool * ?slashesDenoteHost: bool -> Url
        abstract format: url: UrlOptions -> string
        abstract resolve: from: string * ``to``: string -> string

    let [<Import("url")>] Globals: Globals = failwith "JS only"


module dns =


    type Globals =
        abstract lookup: domain: string * family: float * callback: Func<Error, string, float, unit> -> string
        abstract lookup: domain: string * callback: Func<Error, string, float, unit> -> string
        abstract resolve: domain: string * rrtype: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolve: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolve4: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolve6: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolveMx: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolveTxt: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolveSrv: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolveNs: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract resolveCname: domain: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>
        abstract reverse: ip: string * callback: Func<Error, ResizeArray<string>, unit> -> ResizeArray<string>

    let [<Import("dns")>] Globals: Globals = failwith "JS only"

module dgram =
    type RemoteInfo =
        abstract address: string with get, set
        abstract port: float with get, set
        abstract size: float with get, set

    and AddressInfo =
        abstract address: string with get, set
        abstract family: string with get, set
        abstract port: float with get, set

    and Socket =
        // inherit events.EventEmitter() // TODO
        abstract send: buf: Buffer * offset: float * length: float * port: float * address: string * ?callback: Func<Error, float, unit> -> unit
        abstract bind: port: float * ?address: string * ?callback: Func<unit> -> unit
        abstract close: unit -> unit
        abstract address: unit -> AddressInfo
        abstract setBroadcast: flag: bool -> unit
        abstract setMulticastTTL: ttl: float -> unit
        abstract setMulticastLoopback: flag: bool -> unit
        abstract addMembership: multicastAddress: string * ?multicastInterface: string -> unit
        abstract dropMembership: multicastAddress: string * ?multicastInterface: string -> unit

    type Globals =
        abstract createSocket: ``type``: string * ?callback: Func<Buffer, RemoteInfo, unit> -> Socket

    let [<Import("dgram")>] Globals: Globals = failwith "JS only"


module fs =
    type Stats =
        abstract dev: float with get, set
        abstract ino: float with get, set
        abstract mode: float with get, set
        abstract nlink: float with get, set
        abstract uid: float with get, set
        abstract gid: float with get, set
        abstract rdev: float with get, set
        abstract size: float with get, set
        abstract blksize: float with get, set
        abstract blocks: float with get, set
        abstract atime: DateTime with get, set
        abstract mtime: DateTime with get, set
        abstract ctime: DateTime with get, set
        abstract birthtime: DateTime with get, set
        abstract isFile: unit -> bool
        abstract isDirectory: unit -> bool
        abstract isBlockDevice: unit -> bool
        abstract isCharacterDevice: unit -> bool
        abstract isSymbolicLink: unit -> bool
        abstract isFIFO: unit -> bool
        abstract isSocket: unit -> bool

    and FSWatcher =
        // inherit events.EventEmitter() // TODO
        abstract close: unit -> unit

    and ReadStream =
        // inherit stream.Readable // TODO
        abstract close: unit -> unit

    and WriteStream =
        // inherit stream.Writable // TODO
        abstract bytesWritten: float with get, set
        abstract close: unit -> unit

    type Globals =
        abstract F_OK: float with get, set
        abstract R_OK: float with get, set
        abstract W_OK: float with get, set
        abstract X_OK: float with get, set
        abstract rename: oldPath: string * newPath: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract renameSync: oldPath: string * newPath: string -> unit
        abstract truncate: path: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract truncate: path: string * len: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract truncateSync: path: string * ?len: float -> unit
        abstract ftruncate: fd: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract ftruncate: fd: float * len: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract ftruncateSync: fd: float * ?len: float -> unit
        abstract chown: path: string * uid: float * gid: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract chownSync: path: string * uid: float * gid: float -> unit
        abstract fchown: fd: float * uid: float * gid: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract fchownSync: fd: float * uid: float * gid: float -> unit
        abstract lchown: path: string * uid: float * gid: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract lchownSync: path: string * uid: float * gid: float -> unit
        abstract chmod: path: string * mode: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract chmod: path: string * mode: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract chmodSync: path: string * mode: float -> unit
        abstract chmodSync: path: string * mode: string -> unit
        abstract fchmod: fd: float * mode: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract fchmod: fd: float * mode: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract fchmodSync: fd: float * mode: float -> unit
        abstract fchmodSync: fd: float * mode: string -> unit
        abstract lchmod: path: string * mode: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract lchmod: path: string * mode: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract lchmodSync: path: string * mode: float -> unit
        abstract lchmodSync: path: string * mode: string -> unit
        abstract stat: path: string * ?callback: Func<NodeJS.ErrnoException, Stats, obj> -> unit
        abstract lstat: path: string * ?callback: Func<NodeJS.ErrnoException, Stats, obj> -> unit
        abstract fstat: fd: float * ?callback: Func<NodeJS.ErrnoException, Stats, obj> -> unit
        abstract statSync: path: string -> Stats
        abstract lstatSync: path: string -> Stats
        abstract fstatSync: fd: float -> Stats
        abstract link: srcpath: string * dstpath: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract linkSync: srcpath: string * dstpath: string -> unit
        abstract symlink: srcpath: string * dstpath: string * ?``type``: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract symlinkSync: srcpath: string * dstpath: string * ?``type``: string -> unit
        abstract readlink: path: string * ?callback: Func<NodeJS.ErrnoException, string, obj> -> unit
        abstract readlinkSync: path: string -> string
        abstract realpath: path: string * ?callback: Func<NodeJS.ErrnoException, string, obj> -> unit
        abstract realpath: path: string * cache: obj * callback: Func<NodeJS.ErrnoException, string, obj> -> unit
        abstract realpathSync: path: string * ?cache: obj -> string
        abstract unlink: path: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract unlinkSync: path: string -> unit
        abstract rmdir: path: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract rmdirSync: path: string -> unit
        abstract mkdir: path: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract mkdir: path: string * mode: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract mkdir: path: string * mode: string * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract mkdirSync: path: string * ?mode: float -> unit
        abstract mkdirSync: path: string * ?mode: string -> unit
        abstract readdir: path: string * ?callback: Func<NodeJS.ErrnoException, ResizeArray<string>, unit> -> unit
        abstract readdirSync: path: string -> ResizeArray<string>
        abstract close: fd: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract closeSync: fd: float -> unit
        abstract ``open``: path: string * flags: string * ?callback: Func<NodeJS.ErrnoException, float, obj> -> unit
        abstract ``open``: path: string * flags: string * mode: float * ?callback: Func<NodeJS.ErrnoException, float, obj> -> unit
        abstract ``open``: path: string * flags: string * mode: string * ?callback: Func<NodeJS.ErrnoException, float, obj> -> unit
        abstract openSync: path: string * flags: string * ?mode: float -> float
        abstract openSync: path: string * flags: string * ?mode: string -> float
        abstract utimes: path: string * atime: float * mtime: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract utimes: path: string * atime: DateTime * mtime: DateTime * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract utimesSync: path: string * atime: float * mtime: float -> unit
        abstract utimesSync: path: string * atime: DateTime * mtime: DateTime -> unit
        abstract futimes: fd: float * atime: float * mtime: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract futimes: fd: float * atime: DateTime * mtime: DateTime * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract futimesSync: fd: float * atime: float * mtime: float -> unit
        abstract futimesSync: fd: float * atime: DateTime * mtime: DateTime -> unit
        abstract fsync: fd: float * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract fsyncSync: fd: float -> unit
        abstract write: fd: float * buffer: Buffer * offset: float * length: float * position: float * ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit> -> unit
        abstract write: fd: float * buffer: Buffer * offset: float * length: float * ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit> -> unit
        abstract write: fd: float * data: obj * ?callback: Func<NodeJS.ErrnoException, float, string, unit> -> unit
        abstract write: fd: float * data: obj * offset: float * ?callback: Func<NodeJS.ErrnoException, float, string, unit> -> unit
        abstract write: fd: float * data: obj * offset: float * encoding: string * ?callback: Func<NodeJS.ErrnoException, float, string, unit> -> unit
        abstract writeSync: fd: float * buffer: Buffer * offset: float * length: float * position: float -> float
        abstract read: fd: float * buffer: Buffer * offset: float * length: float * position: float * ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit> -> unit
        abstract readSync: fd: float * buffer: Buffer * offset: float * length: float * position: float -> float
        abstract readFile: filename: string * encoding: string * callback: Func<NodeJS.ErrnoException, string, unit> -> unit
        abstract readFile: filename: string * options: obj * callback: Func<NodeJS.ErrnoException, string, unit> -> unit
        abstract readFile: filename: string * options: obj * callback: Func<NodeJS.ErrnoException, Buffer, unit> -> unit
        abstract readFile: filename: string * callback: Func<NodeJS.ErrnoException, Buffer, unit> -> unit
        abstract readFileSync: filename: string * encoding: string -> string
        abstract readFileSync: filename: string * options: obj -> string
        abstract readFileSync: filename: string * ?options: obj -> Buffer
        abstract writeFile: filename: string * data: obj * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract writeFile: filename: string * data: obj * options: obj * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract writeFileSync: filename: string * data: obj * ?options: obj -> unit
        abstract appendFile: filename: string * data: obj * options: obj * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract appendFile: filename: string * data: obj * ?callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract appendFileSync: filename: string * data: obj * ?options: obj -> unit
        abstract watchFile: filename: string * listener: Func<Stats, Stats, unit> -> unit
        abstract watchFile: filename: string * options: obj * listener: Func<Stats, Stats, unit> -> unit
        abstract unwatchFile: filename: string * ?listener: Func<Stats, Stats, unit> -> unit
        abstract watch: filename: string * ?listener: Func<string, string, obj> -> FSWatcher
        abstract watch: filename: string * options: obj * ?listener: Func<string, string, obj> -> FSWatcher
        abstract exists: path: string * ?callback: Func<bool, unit> -> unit
        abstract existsSync: path: string -> bool
        abstract access: path: string * callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract access: path: string * mode: float * callback: Func<NodeJS.ErrnoException, unit> -> unit
        abstract accessSync: path: string * ?mode: float -> unit
        abstract createReadStream: path: string * ?options: obj -> ReadStream
        abstract createWriteStream: path: string * ?options: obj -> WriteStream

    let [<Import("fs")>] Globals: Globals = failwith "JS only"


module path =
    type ParsedPath =
        abstract root: string with get, set
        abstract dir: string with get, set
        abstract ``base``: string with get, set
        abstract ext: string with get, set
        abstract name: string with get, set

    type Globals =
        abstract sep: string with get, set
        abstract delimiter: string with get, set
        abstract normalize: p: string -> string
        abstract join: [<ParamArray>] paths: obj[] -> string
        abstract join: [<ParamArray>] paths: string[] -> string
        abstract resolve: [<ParamArray>] pathSegments: obj[] -> string
        abstract isAbsolute: path: string -> bool
        abstract relative: from: string * ``to``: string -> string
        abstract dirname: p: string -> string
        abstract basename: p: string * ?ext: string -> string
        abstract extname: p: string -> string
        abstract parse: pathString: string -> ParsedPath
        abstract format: pathObject: ParsedPath -> string

    let [<Import("path")>] Globals: Globals = failwith "JS only"
    module posix =


        type Globals =
            abstract sep: string with get, set
            abstract delimiter: string with get, set
            abstract normalize: p: string -> string
            abstract join: [<ParamArray>] paths: obj[] -> string
            abstract resolve: [<ParamArray>] pathSegments: obj[] -> string
            abstract isAbsolute: p: string -> bool
            abstract relative: from: string * ``to``: string -> string
            abstract dirname: p: string -> string
            abstract basename: p: string * ?ext: string -> string
            abstract extname: p: string -> string
            abstract parse: p: string -> ParsedPath
            abstract format: pP: ParsedPath -> string

        let [<Import("posix")>] Globals: Globals = failwith "JS only"


    module win32 =


        type Globals =
            abstract sep: string with get, set
            abstract delimiter: string with get, set
            abstract normalize: p: string -> string
            abstract join: [<ParamArray>] paths: obj[] -> string
            abstract resolve: [<ParamArray>] pathSegments: obj[] -> string
            abstract isAbsolute: p: string -> bool
            abstract relative: from: string * ``to``: string -> string
            abstract dirname: p: string -> string
            abstract basename: p: string * ?ext: string -> string
            abstract extname: p: string -> string
            abstract parse: p: string -> ParsedPath
            abstract format: pP: ParsedPath -> string

        let [<Import("win32")>] Globals: Globals = failwith "JS only"


module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> string
        abstract detectIncompleteChar: buffer: Buffer -> float

    type Globals =
        abstract StringDecoder: obj with get, set

    let [<Import("string_decoder")>] Globals: Globals = failwith "JS only"


module util =
    type InspectOptions =
        abstract showHidden: bool option with get, set
        abstract depth: float option with get, set
        abstract colors: bool option with get, set
        abstract customInspect: bool option with get, set

    type Globals =
        abstract format: format: obj * [<ParamArray>] param: obj[] -> string
        abstract debug: string: string -> unit
        abstract error: [<ParamArray>] param: obj[] -> unit
        abstract puts: [<ParamArray>] param: obj[] -> unit
        abstract print: [<ParamArray>] param: obj[] -> unit
        abstract log: string: string -> unit
        abstract inspect: ``object``: obj * ?showHidden: bool * ?depth: float * ?color: bool -> string
        abstract inspect: ``object``: obj * options: InspectOptions -> string
        abstract isArray: ``object``: obj -> bool
        abstract isRegExp: ``object``: obj -> bool
        abstract isDate: ``object``: obj -> bool
        abstract isError: ``object``: obj -> bool
        abstract inherits: ``constructor``: obj * superConstructor: obj -> unit
        abstract debuglog: key: string -> Func<string, obj, unit>

    let [<Import("util")>] Globals: Globals = failwith "JS only"


module ``assert`` =
    type Globals =
        abstract ``internal``: value: obj * ?message: string -> unit

    let [<Import("assert")>] Globals: Globals = failwith "JS only"
    module ``internal`` =
        type [<Import("internal?get=AssertionError")>] AssertionError(?options: obj) =
            member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.message with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.actual with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.expected with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.operator with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.generatedMessage with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            interface Error with
                member __.stack with get(): string option = failwith "JS only" and set(v): unit = failwith "JS only"

        type Globals =
            abstract throws: obj with get, set
            abstract doesNotThrow: obj with get, set
            abstract fail: ?actual: obj * ?expected: obj * ?message: string * ?operator: string -> unit
            abstract ok: value: obj * ?message: string -> unit
            abstract equal: actual: obj * expected: obj * ?message: string -> unit
            abstract notEqual: actual: obj * expected: obj * ?message: string -> unit
            abstract deepEqual: actual: obj * expected: obj * ?message: string -> unit
            abstract notDeepEqual: acutal: obj * expected: obj * ?message: string -> unit
            abstract strictEqual: actual: obj * expected: obj * ?message: string -> unit
            abstract notStrictEqual: actual: obj * expected: obj * ?message: string -> unit
            abstract ifError: value: obj -> unit

        let [<Import("internal")>] Globals: Globals = failwith "JS only"


module tty =
    type ReadStream =
        inherit net.Socket
        abstract isRaw: bool with get, set
        abstract setRawMode: mode: bool -> unit

    and WriteStream =
        inherit net.Socket
        abstract columns: float with get, set
        abstract rows: float with get, set

    type Globals =
        abstract isatty: fd: float -> bool

    let [<Import("tty")>] Globals: Globals = failwith "JS only"


module domain =
    type [<Import("domain?get=Domain")>] Domain() =
        inherit events.EventEmitter()
        member __.run(fn: (obj->obj)): unit = failwith "JS only"
        member __.add(emitter: events.EventEmitter): unit = failwith "JS only"
        member __.remove(emitter: events.EventEmitter): unit = failwith "JS only"
        member __.bind(cb: Func<Error, obj, obj>): obj = failwith "JS only"
        member __.intercept(cb: Func<obj, obj>): obj = failwith "JS only"
        member __.dispose(): unit = failwith "JS only"
        member __.addListener(``event``: string, listener: (obj->obj)): Domain = failwith "JS only"
        member __.on(``event``: string, listener: (obj->obj)): Domain = failwith "JS only"
        member __.once(``event``: string, listener: (obj->obj)): Domain = failwith "JS only"
        member __.removeListener(``event``: string, listener: (obj->obj)): Domain = failwith "JS only"
        member __.removeAllListeners(?``event``: string): Domain = failwith "JS only"

    type Globals =
        abstract create: unit -> Domain

    let [<Import("domain")>] Globals: Globals = failwith "JS only"


module constants =
    type Globals =
        abstract E2BIG: float with get, set
        abstract EACCES: float with get, set
        abstract EADDRINUSE: float with get, set
        abstract EADDRNOTAVAIL: float with get, set
        abstract EAFNOSUPPORT: float with get, set
        abstract EAGAIN: float with get, set
        abstract EALREADY: float with get, set
        abstract EBADF: float with get, set
        abstract EBADMSG: float with get, set
        abstract EBUSY: float with get, set
        abstract ECANCELED: float with get, set
        abstract ECHILD: float with get, set
        abstract ECONNABORTED: float with get, set
        abstract ECONNREFUSED: float with get, set
        abstract ECONNRESET: float with get, set
        abstract EDEADLK: float with get, set
        abstract EDESTADDRREQ: float with get, set
        abstract EDOM: float with get, set
        abstract EEXIST: float with get, set
        abstract EFAULT: float with get, set
        abstract EFBIG: float with get, set
        abstract EHOSTUNREACH: float with get, set
        abstract EIDRM: float with get, set
        abstract EILSEQ: float with get, set
        abstract EINPROGRESS: float with get, set
        abstract EINTR: float with get, set
        abstract EINVAL: float with get, set
        abstract EIO: float with get, set
        abstract EISCONN: float with get, set
        abstract EISDIR: float with get, set
        abstract ELOOP: float with get, set
        abstract EMFILE: float with get, set
        abstract EMLINK: float with get, set
        abstract EMSGSIZE: float with get, set
        abstract ENAMETOOLONG: float with get, set
        abstract ENETDOWN: float with get, set
        abstract ENETRESET: float with get, set
        abstract ENETUNREACH: float with get, set
        abstract ENFILE: float with get, set
        abstract ENOBUFS: float with get, set
        abstract ENODATA: float with get, set
        abstract ENODEV: float with get, set
        abstract ENOENT: float with get, set
        abstract ENOEXEC: float with get, set
        abstract ENOLCK: float with get, set
        abstract ENOLINK: float with get, set
        abstract ENOMEM: float with get, set
        abstract ENOMSG: float with get, set
        abstract ENOPROTOOPT: float with get, set
        abstract ENOSPC: float with get, set
        abstract ENOSR: float with get, set
        abstract ENOSTR: float with get, set
        abstract ENOSYS: float with get, set
        abstract ENOTCONN: float with get, set
        abstract ENOTDIR: float with get, set
        abstract ENOTEMPTY: float with get, set
        abstract ENOTSOCK: float with get, set
        abstract ENOTSUP: float with get, set
        abstract ENOTTY: float with get, set
        abstract ENXIO: float with get, set
        abstract EOPNOTSUPP: float with get, set
        abstract EOVERFLOW: float with get, set
        abstract EPERM: float with get, set
        abstract EPIPE: float with get, set
        abstract EPROTO: float with get, set
        abstract EPROTONOSUPPORT: float with get, set
        abstract EPROTOTYPE: float with get, set
        abstract ERANGE: float with get, set
        abstract EROFS: float with get, set
        abstract ESPIPE: float with get, set
        abstract ESRCH: float with get, set
        abstract ETIME: float with get, set
        abstract ETIMEDOUT: float with get, set
        abstract ETXTBSY: float with get, set
        abstract EWOULDBLOCK: float with get, set
        abstract EXDEV: float with get, set
        abstract WSAEINTR: float with get, set
        abstract WSAEBADF: float with get, set
        abstract WSAEACCES: float with get, set
        abstract WSAEFAULT: float with get, set
        abstract WSAEINVAL: float with get, set
        abstract WSAEMFILE: float with get, set
        abstract WSAEWOULDBLOCK: float with get, set
        abstract WSAEINPROGRESS: float with get, set
        abstract WSAEALREADY: float with get, set
        abstract WSAENOTSOCK: float with get, set
        abstract WSAEDESTADDRREQ: float with get, set
        abstract WSAEMSGSIZE: float with get, set
        abstract WSAEPROTOTYPE: float with get, set
        abstract WSAENOPROTOOPT: float with get, set
        abstract WSAEPROTONOSUPPORT: float with get, set
        abstract WSAESOCKTNOSUPPORT: float with get, set
        abstract WSAEOPNOTSUPP: float with get, set
        abstract WSAEPFNOSUPPORT: float with get, set
        abstract WSAEAFNOSUPPORT: float with get, set
        abstract WSAEADDRINUSE: float with get, set
        abstract WSAEADDRNOTAVAIL: float with get, set
        abstract WSAENETDOWN: float with get, set
        abstract WSAENETUNREACH: float with get, set
        abstract WSAENETRESET: float with get, set
        abstract WSAECONNABORTED: float with get, set
        abstract WSAECONNRESET: float with get, set
        abstract WSAENOBUFS: float with get, set
        abstract WSAEISCONN: float with get, set
        abstract WSAENOTCONN: float with get, set
        abstract WSAESHUTDOWN: float with get, set
        abstract WSAETOOMANYREFS: float with get, set
        abstract WSAETIMEDOUT: float with get, set
        abstract WSAECONNREFUSED: float with get, set
        abstract WSAELOOP: float with get, set
        abstract WSAENAMETOOLONG: float with get, set
        abstract WSAEHOSTDOWN: float with get, set
        abstract WSAEHOSTUNREACH: float with get, set
        abstract WSAENOTEMPTY: float with get, set
        abstract WSAEPROCLIM: float with get, set
        abstract WSAEUSERS: float with get, set
        abstract WSAEDQUOT: float with get, set
        abstract WSAESTALE: float with get, set
        abstract WSAEREMOTE: float with get, set
        abstract WSASYSNOTREADY: float with get, set
        abstract WSAVERNOTSUPPORTED: float with get, set
        abstract WSANOTINITIALISED: float with get, set
        abstract WSAEDISCON: float with get, set
        abstract WSAENOMORE: float with get, set
        abstract WSAECANCELLED: float with get, set
        abstract WSAEINVALIDPROCTABLE: float with get, set
        abstract WSAEINVALIDPROVIDER: float with get, set
        abstract WSAEPROVIDERFAILEDINIT: float with get, set
        abstract WSASYSCALLFAILURE: float with get, set
        abstract WSASERVICE_NOT_FOUND: float with get, set
        abstract WSATYPE_NOT_FOUND: float with get, set
        abstract WSA_E_NO_MORE: float with get, set
        abstract WSA_E_CANCELLED: float with get, set
        abstract WSAEREFUSED: float with get, set
        abstract SIGHUP: float with get, set
        abstract SIGINT: float with get, set
        abstract SIGILL: float with get, set
        abstract SIGABRT: float with get, set
        abstract SIGFPE: float with get, set
        abstract SIGKILL: float with get, set
        abstract SIGSEGV: float with get, set
        abstract SIGTERM: float with get, set
        abstract SIGBREAK: float with get, set
        abstract SIGWINCH: float with get, set
        abstract SSL_OP_ALL: float with get, set
        abstract SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION: float with get, set
        abstract SSL_OP_CIPHER_SERVER_PREFERENCE: float with get, set
        abstract SSL_OP_CISCO_ANYCONNECT: float with get, set
        abstract SSL_OP_COOKIE_EXCHANGE: float with get, set
        abstract SSL_OP_CRYPTOPRO_TLSEXT_BUG: float with get, set
        abstract SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS: float with get, set
        abstract SSL_OP_EPHEMERAL_RSA: float with get, set
        abstract SSL_OP_LEGACY_SERVER_CONNECT: float with get, set
        abstract SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER: float with get, set
        abstract SSL_OP_MICROSOFT_SESS_ID_BUG: float with get, set
        abstract SSL_OP_MSIE_SSLV2_RSA_PADDING: float with get, set
        abstract SSL_OP_NETSCAPE_CA_DN_BUG: float with get, set
        abstract SSL_OP_NETSCAPE_CHALLENGE_BUG: float with get, set
        abstract SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG: float with get, set
        abstract SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG: float with get, set
        abstract SSL_OP_NO_COMPRESSION: float with get, set
        abstract SSL_OP_NO_QUERY_MTU: float with get, set
        abstract SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION: float with get, set
        abstract SSL_OP_NO_SSLv2: float with get, set
        abstract SSL_OP_NO_SSLv3: float with get, set
        abstract SSL_OP_NO_TICKET: float with get, set
        abstract SSL_OP_NO_TLSv1: float with get, set
        abstract SSL_OP_NO_TLSv1_1: float with get, set
        abstract SSL_OP_NO_TLSv1_2: float with get, set
        abstract SSL_OP_PKCS1_CHECK_1: float with get, set
        abstract SSL_OP_PKCS1_CHECK_2: float with get, set
        abstract SSL_OP_SINGLE_DH_USE: float with get, set
        abstract SSL_OP_SINGLE_ECDH_USE: float with get, set
        abstract SSL_OP_SSLEAY_080_CLIENT_DH_BUG: float with get, set
        abstract SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG: float with get, set
        abstract SSL_OP_TLS_BLOCK_PADDING_BUG: float with get, set
        abstract SSL_OP_TLS_D5_BUG: float with get, set
        abstract SSL_OP_TLS_ROLLBACK_BUG: float with get, set
        abstract ENGINE_METHOD_DSA: float with get, set
        abstract ENGINE_METHOD_DH: float with get, set
        abstract ENGINE_METHOD_RAND: float with get, set
        abstract ENGINE_METHOD_ECDH: float with get, set
        abstract ENGINE_METHOD_ECDSA: float with get, set
        abstract ENGINE_METHOD_CIPHERS: float with get, set
        abstract ENGINE_METHOD_DIGESTS: float with get, set
        abstract ENGINE_METHOD_STORE: float with get, set
        abstract ENGINE_METHOD_PKEY_METHS: float with get, set
        abstract ENGINE_METHOD_PKEY_ASN1_METHS: float with get, set
        abstract ENGINE_METHOD_ALL: float with get, set
        abstract ENGINE_METHOD_NONE: float with get, set
        abstract DH_CHECK_P_NOT_SAFE_PRIME: float with get, set
        abstract DH_CHECK_P_NOT_PRIME: float with get, set
        abstract DH_UNABLE_TO_CHECK_GENERATOR: float with get, set
        abstract DH_NOT_SUITABLE_GENERATOR: float with get, set
        abstract NPN_ENABLED: float with get, set
        abstract RSA_PKCS1_PADDING: float with get, set
        abstract RSA_SSLV23_PADDING: float with get, set
        abstract RSA_NO_PADDING: float with get, set
        abstract RSA_PKCS1_OAEP_PADDING: float with get, set
        abstract RSA_X931_PADDING: float with get, set
        abstract RSA_PKCS1_PSS_PADDING: float with get, set
        abstract POINT_CONVERSION_COMPRESSED: float with get, set
        abstract POINT_CONVERSION_UNCOMPRESSED: float with get, set
        abstract POINT_CONVERSION_HYBRID: float with get, set
        abstract O_RDONLY: float with get, set
        abstract O_WRONLY: float with get, set
        abstract O_RDWR: float with get, set
        abstract S_IFMT: float with get, set
        abstract S_IFREG: float with get, set
        abstract S_IFDIR: float with get, set
        abstract S_IFCHR: float with get, set
        abstract S_IFLNK: float with get, set
        abstract O_CREAT: float with get, set
        abstract O_EXCL: float with get, set
        abstract O_TRUNC: float with get, set
        abstract O_APPEND: float with get, set
        abstract F_OK: float with get, set
        abstract R_OK: float with get, set
        abstract W_OK: float with get, set
        abstract X_OK: float with get, set
        abstract UV_UDP_REUSEADDR: float with get, set

    let [<Import("constants")>] Globals: Globals = failwith "JS only"

