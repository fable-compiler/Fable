module Fable.Import.Node
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS

// Buffer Start
module buffer_types =
    type BufferEncoding =
    (* TODO StringEnum ascii | utf8 | utf16le | ucs2 | binary | hex *) string

    and Buffer = 
        inherit Uint8Array
        abstract write: string: string * ?offset: float * ?length: float * ?encoding: string -> float
        abstract toString: ?encoding: string * ?start: float * ?``end``: float -> string
        abstract toJSON: unit -> obj
        abstract equals: otherBuffer: Buffer -> bool
        abstract compare: otherBuffer: Buffer * ?targetStart: float * ?targetEnd: float * ?sourceStart: float * ?sourceEnd: float -> float
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
        abstract readUInt8: offset: float * ?noAssert: bool -> float
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
        abstract swap16: unit -> Buffer
        abstract swap32: unit -> Buffer
        abstract swap64: unit -> Buffer
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
        abstract fill: value: obj * ?offset: float * ?``end``: float -> obj
        abstract indexOf: value: U3<string, float, Buffer> * ?byteOffset: float * ?encoding: string -> float
        abstract lastIndexOf: value: U3<string, float, Buffer> * ?byteOffset: float * ?encoding: string -> float
        abstract entries: unit -> IterableIterator<float * float>
        abstract includes: value: U3<string, float, Buffer> * ?byteOffset: float * ?encoding: string -> bool
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        abstract from: array: ResizeArray<obj> -> Buffer
        abstract from: arrayBuffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Buffer
        abstract from: buffer: Buffer -> Buffer
        abstract from: str: string * ?encoding: string -> Buffer
        abstract isBuffer: obj: obj -> obj
        abstract isEncoding: encoding: string -> bool
        abstract byteLength: string: string * ?encoding: string -> float
        abstract concat: list: ResizeArray<Buffer> * ?totalLength: float -> Buffer
        abstract compare: buf1: Buffer * buf2: Buffer -> float
        abstract alloc: size: float * ?fill: U3<string, Buffer, float> * ?encoding: string -> Buffer
        abstract allocUnsafe: size: float -> Buffer
        abstract allocUnsafeSlow: size: float -> Buffer
    
    and [<AllowNullLiteral>] BufferStatic =
        [<Emit("new $0($1...)")>] abstract Create: str: string * ?encoding: string -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: str: float -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: Uint8Array -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: arrayBuffer: ArrayBuffer -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: ResizeArray<obj> -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: buffer: Buffer -> Buffer
        abstract from: array: ResizeArray<obj> -> Buffer
        abstract from: buffer: Buffer -> Buffer
        abstract from: arrayBuffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Buffer
        abstract from: str: string * ?encoding: string -> Buffer

    and [<AllowNullLiteral>] SlowBuffer =
        abstract prototype: Buffer with get, set
        abstract isBuffer: obj: obj -> bool
        abstract byteLength: string: string * ?encoding: string -> float
        abstract concat: list: ResizeArray<Buffer> * ?totalLength: float -> Buffer

    and [<AllowNullLiteral>] SlowBufferStatic =
        [<Emit("new $0($1...)")>] abstract Create: str: string * ?encoding: string -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: str: float -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: Uint8Array -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: ResizeArray<obj> -> Buffer

    and Globals = 
        abstract Buffer: BufferStatic with get, set
        abstract SlowBuffer: SlowBufferStatic with get, set

[<Import("*", "buffer")>] 
let buffer: buffer_types.Globals = jsNative
// Buffer End

// Events start
module event_types =
    type [<AllowNullLiteral>] EventEmitter =
        abstract defaultMaxListeners: float with get, set
        abstract addListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract on: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract once: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract prependListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract prependOnceListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract removeListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract removeAllListener: ev: U2<string, Symbol> * listener: Function -> EventEmitter
        abstract setMaxListeners: n: int -> EventEmitter
        abstract getMaxListeners: unit -> int
        abstract listeners: ev: U2<string, Symbol> -> ResizeArray<Function>
        abstract emit: ev: string * [<ParamArray>] args: obj[] -> bool
        abstract eventNames: unit -> ResizeArray<U2<string, Symbol>>
        
    and [<AllowNullLiteral>] EventEmitterStatic =
        [<Emit("new $0()")>] abstract Create: unit -> EventEmitter

    type Globals =
        abstract defaultMaxListeners: float with get, set
        abstract EventEmitter: EventEmitterStatic with get, set

[<Import("*", "events")>]
let events: event_types.Globals = jsNative
// Events end

// Stream Start
module stream_types =
    type [<AllowNullLiteral>] Stream =
        abstract pipe: destination: 'T * ?options: obj -> 'T

    and [<AllowNullLiteral>] StreamStatic = 
        [<Emit("new $0()")>] abstract Create: unit -> Stream

    and ReadableOptions = {
        highWaterMark: float option
        encoding: string;
        objectMode: bool option;
        read: Func<float, obj> option;
    }

    and [<AllowNullLiteral>] Readable =
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

    and [<AllowNullLiteral>] ReadableStatic = 
        [<Emit("new $0()")>] abstract Create: readableOptions:ReadableOptions -> Readable

    and WritableOptions = {
        highWaterMark: float option;
        decodeStrings: bool option;
        objectMode: bool option;
        write: Func<U2<string, buffer_types.Buffer>, string, Function, obj> option;
        writev: Func<ResizeArray<obj>, Function, obj> option;
    }

    and [<AllowNullLiteral>] Writable =
        inherit Stream
        abstract writable: bool with get, set
        abstract write: chunk: obj * ?cb: Function -> bool
        abstract ``end``:unit -> unit
        abstract ``end``: obj: obj * ?cb: Function -> unit
        abstract ``end``: obj: obj * ?encoding: string * ?cb: Function -> unit

    and [<AllowNullLiteral>] WritableStatic = 
        [<Emit("new $0()")>] abstract Create: writableOptions:WritableOptions -> Writable
            
    type DuplexOptions = {
        decodeStrings: bool option;
        encoding: string;
        objectMode: bool option;
        allowHalfOpen: bool option;
        readableObjectMode: bool option;
        writableObjectMode: bool option;
        read: Func<float, obj> option;
        write: Func<U2<string, buffer_types.Buffer>, string, Function, obj> option;
        writev: Func<ResizeArray<obj>, Function, obj> option;
    }

    and [<AllowNullLiteral>] Duplex =
        inherit Readable
        inherit Writable

    and [<AllowNullLiteral>] DuplexStatic = 
        [<Emit("new $0()")>] abstract Create: duplexOptions:DuplexOptions -> Duplex

    and TransformOptions = {
        decodeStrings: bool option;
        encoding: string option;
        objectMode: bool option;
        allowHalfOpen: bool option;
        readableObjectMode: bool option;
        writableObjectMode: bool option;
        transform: Func<U2<string, buffer_types.Buffer>, string, Function, obj> option;
        flush: Func<Function, obj> option;
    }

    and [<AllowNullLiteral>] Transform = 
        inherit Readable
        inherit Writable
        abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
        abstract _flush: callback: Function -> unit

    and [<AllowNullLiteral>] TransformStatic = 
        [<Emit("new $0()")>] abstract Create: transformOptions:TransformOptions -> Transform


    and [<AllowNullLiteral>] PassThrough =
        inherit Transform

    and [<AllowNullLiteral>] PassThroughStatic = 
        [<Emit("new $0()")>] abstract Create: unit -> PassThrough

    and Globals =
        abstract Stream: StreamStatic with get, set
        abstract Readable: ReadableStatic with get, set
        abstract Writable: WritableStatic with get, set
        abstract Duplex: DuplexStatic with get, set
        abstract Transform: TransformStatic with get, set
        abstract PassThrough: PassThroughStatic with get, set

[<Import("*", "stream")>]
let stream: stream_types.Globals = jsNative
// Stream End

type [<AllowNullLiteral>] Console =
    abstract ``assert``: value: obj * ?message: string * [<ParamArray>] optionalParams: obj[] -> unit
    abstract dir: obj: obj * ?options: obj -> unit
    abstract error: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract info: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract log: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract time: label: string -> unit
    abstract timeEnd: label: string -> unit
    abstract trace: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract warn: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit

and [<AllowNullLiteral>] Error =
    abstract stack: string option with get, set
    abstract message: string option with get, set

and [<AllowNullLiteral>] NodeRequireFunction =
    [<Emit("$0($1...)")>] abstract Invoke: id: string -> obj

and [<AllowNullLiteral>] NodeRequire =
    inherit NodeRequireFunction
    abstract cache: obj with get, set
    abstract extensions: obj with get, set
    abstract main: obj with get, set
    abstract resolve: id: string -> string

and [<AllowNullLiteral>] NodeModule =
    abstract exports: obj with get, set
    abstract require: NodeRequireFunction with get, set
    abstract id: string with get, set
    abstract filename: string with get, set
    abstract loaded: bool with get, set
    abstract parent: U2<NodeModule, obj> with get, set
    abstract ``null``: obj with get, set
    abstract children: ResizeArray<NodeModule> with get, set

and [<AllowNullLiteral>] IterableIterator<'T> =
    interface end

module NodeJS =
    type [<AllowNullLiteral>] ErrnoException =
        inherit Error
        abstract errno: float option with get, set
        abstract code: string option with get, set
        abstract path: string option with get, set
        abstract syscall: string option with get, set
        abstract stack: string option with get, set

    and [<AllowNullLiteral>] ProcessVersions =
        abstract http_parser: string with get, set
        abstract node: string with get, set
        abstract v8: string with get, set
        abstract ares: string with get, set
        abstract uv: string with get, set
        abstract zlib: string with get, set
        abstract modules: string with get, set
        abstract openssl: string with get, set

    and [<AllowNullLiteral>] MemoryUsage =
        abstract rss: float with get, set
        abstract heapTotal: float with get, set
        abstract heapUsed: float with get, set

    and [<AllowNullLiteral>] CpuUsage =
        abstract user: float with get, set
        abstract system: float with get, set

    and [<StringEnum>] Platform =
        | Aix | Android | Darwin | Freebsd | Linux | Openbsd | Sunos | Win32

    and [<AllowNullLiteral>] Process =
        abstract addListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract on: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract once: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract removeListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract removeAllListeners: ?``event``: U2<string, Symbol> -> obj
        abstract setMaxListeners: n: float -> obj
        abstract getMaxListeners: unit -> float
        abstract listeners: ``event``: U2<string, Symbol> -> ResizeArray<Function>
        abstract emit: ``event``: U2<string, Symbol> * [<ParamArray>] args: obj[] -> bool
        abstract listenerCount: ``type``: U2<string, Symbol> -> float
        abstract prependListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract prependOnceListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract eventNames: unit -> ResizeArray<U2<string, Symbol>>
        abstract stdout: stream_types.Writable with get, set
        abstract stderr: stream_types.Writable with get, set
        abstract stdin: stream_types.Readable with get, set
        abstract argv: ResizeArray<string> with get, set
        abstract argv0: string with get, set
        abstract execArgv: ResizeArray<string> with get, set
        abstract execPath: string with get, set
        abstract env: obj with get, set
        abstract exitCode: float with get, set
        abstract version: string with get, set
        abstract versions: ProcessVersions with get, set
        abstract config: obj with get, set
        abstract pid: float with get, set
        abstract title: string with get, set
        abstract arch: string with get, set
        abstract platform: Platform with get, set
        abstract mainModule: NodeModule option with get, set
        abstract connected: bool with get, set
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
        abstract kill: pid: float * ?signal: U2<string, float> -> unit
        abstract memoryUsage: unit -> MemoryUsage
        abstract cpuUsage: ?previousValue: CpuUsage -> CpuUsage
        abstract nextTick: callback: Function * [<ParamArray>] args: obj[] -> unit
        abstract umask: ?mask: float -> float
        abstract uptime: unit -> float
        abstract hrtime: ?time: float * float -> float * float
        abstract send: message: obj * ?sendHandle: obj -> unit
        abstract disconnect: unit -> unit

    and [<AllowNullLiteral>] Global =
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
        abstract Promise: Function with get, set
        abstract RangeError: obj with get, set
        abstract ReferenceError: obj with get, set
        abstract RegExp: obj with get, set
        abstract Set: SetConstructor with get, set
        abstract String: obj with get, set
        abstract Symbol: Function with get, set
        abstract SyntaxError: obj with get, set
        abstract TypeError: obj with get, set
        abstract URIError: obj with get, set
        abstract Uint16Array: obj with get, set
        abstract Uint32Array: obj with get, set
        abstract Uint8Array: obj with get, set
        abstract Uint8ClampedArray: Function with get, set
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
        abstract gc: Func<unit, unit> with get, set
        abstract v8debug: obj option with get, set

    and [<AllowNullLiteral>] Timer =
        abstract ref: unit -> unit
        abstract unref: unit -> unit

let [<Global>] ``process``: NodeJS.Process = jsNative
let [<Global>] ``global``: NodeJS.Global = jsNative
let [<Global>] __filename: string = jsNative
let [<Global>] __dirname: string = jsNative
let [<Global>] require: NodeRequire = jsNative
let [<Global>] ``module``: NodeModule = jsNative
let [<Global>] exports: obj = jsNative
let [<Global>] SlowBuffer: buffer_types.SlowBufferStatic = jsNative
let [<Global>] Buffer: buffer_types.BufferStatic = jsNative

// ChildProcess start
module child_process_types =
    type ExecError = 
        inherit Error
        abstract code: int with get, set
        abstract signal: string with get, set

    and [<AllowNullLiteral>] ChildProcess =
        inherit event_types.EventEmitter
        abstract stdin: stream_types.Writable with get, set
        abstract stdout: stream_types.Readable with get, set
        abstract stderr: stream_types.Readable with get, set
        abstract connected: bool with get, set
        abstract stdio: U2<stream_types.Readable,stream_types.Writable>[] with get, set
        abstract pid: float with get, set
        abstract kill: ?signal: string -> unit
        abstract send: message: obj * ?sendHandle: obj -> unit
        abstract disconnect: unit -> unit
        abstract unref: unit -> unit

    and [<AllowNullLiteral>] ChildProcessStatic =
        [<Emit("new $0()")>] abstract Create: unit -> ChildProcess

    and ExecOptions = {
        encoding : string option;
    }

    and Globals =
        abstract ChildProcess: ChildProcessStatic with get, set
        abstract spawn: command: string * ?args: ResizeArray<string> * ?options: obj -> ChildProcess
        abstract exec: command: string * options: ExecOptions * callback: Func<ExecError option, buffer_types.Buffer, buffer_types.Buffer, unit> -> ChildProcessStatic

        abstract execFile: file: string * ?callback: Func<ExecError option, buffer_types.Buffer, buffer_types.Buffer, unit> -> ChildProcess
        
        abstract execFile: file: string * ?args: ResizeArray<string> * ?callback: Func<ExecError option, buffer_types.Buffer, buffer_types.Buffer, unit> -> ChildProcess
        
        abstract execFile :file: string * ?args: ResizeArray<string> * ?options: ExecOptions * ?callback: Func<ExecError option, buffer_types.Buffer, buffer_types.Buffer, unit> -> ChildProcess
        
        abstract fork: modulePath: string * ?args: ResizeArray<string> * ?options: obj -> ChildProcess
        
        abstract execSync: command: string * ?options: obj -> U2<string, buffer_types.Buffer>
        
        abstract execFileSync: command: string * ?args: ResizeArray<string> * ?options: obj -> U2<string, buffer_types.Buffer>
        
        abstract spawnSync: command: string * ?args: ResizeArray<string> * ?options: obj -> obj

[<Import("*", "child_process")>]
let child_process: child_process_types.Globals = jsNative
// ChildProcess end

module querystring_types =
    type [<AllowNullLiteral>] StringifyOptions =
        abstract encodeURIComponent: Function option with get, set

    and [<AllowNullLiteral>] ParseOptions =
        abstract maxKeys: float option with get, set
        abstract decodeURIComponent: Function option with get, set

    type Globals =
        abstract stringify: obj: 'T * ?sep: string * ?eq: string * ?options: StringifyOptions -> string
        abstract parse: str: string * ?sep: string * ?eq: string * ?options: ParseOptions -> obj
        abstract parse: str: string * ?sep: string * ?eq: string * ?options: ParseOptions -> 'T
        abstract escape: str: string -> string
        abstract unescape: str: string -> string

[<Import("*","querystring")>]
let querystring: querystring_types.Globals = jsNative

// net start
module net_types =
    type [<AllowNullLiteral>] Socket =
        inherit stream_types.Readable
        abstract writable: bool with get, set
        abstract _write: chunk: obj * encoding: string * callback: Function -> unit
        abstract write: chunk: obj * ?cb: Function -> bool
        abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
        abstract ``end``: unit -> unit
        abstract ``end``: chunk: obj * ?cb: Function -> unit
        abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit
        abstract bufferSize: float with get, set
        abstract remoteAddress: string with get, set
        abstract remoteFamily: string with get, set
        abstract remotePort: float with get, set
        abstract localAddress: string with get, set
        abstract localPort: float with get, set
        abstract bytesRead: float with get, set
        abstract bytesWritten: float with get, set
        abstract write: buffer: buffer_types.Buffer -> bool
        abstract write: buffer: buffer_types.Buffer * ?cb: Function -> bool
        abstract write: str: string * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?fd: string -> bool
        abstract connect: port: float * ?host: string * ?connectionListener: Function -> unit
        abstract connect: path: string * ?connectionListener: Function -> unit
        abstract setEncoding: ?encoding: string -> unit
        abstract destroy: unit -> unit
        abstract pause: unit -> unit
        abstract resume: unit -> unit
        abstract setTimeout: timeout: float * ?callback: Function -> unit
        abstract setNoDelay: ?noDelay: bool -> unit
        abstract setKeepAlive: ?enable: bool * ?initialDelay: float -> unit
        abstract address: unit -> obj
        abstract unref: unit -> unit
        abstract ref: unit -> unit
        abstract ``end``: buffer: buffer_types.Buffer * ?cb: Function -> unit
        abstract ``end``: str: string * ?cb: Function -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit
        abstract ``end``: ?data: obj * ?encoding: string -> unit

    and [<AllowNullLiteral>] Server =
        inherit Socket
        abstract maxConnections: float with get, set
        abstract connections: float with get, set
        abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: Function -> Server
        abstract listen: path: string * ?listeningListener: Function -> Server
        abstract listen: handle: obj * ?listeningListener: Function -> Server
        abstract close: ?callback: Function -> Server
        abstract address: unit -> obj

    type Globals =
        member __.Socket with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.createServer(?connectionListener: Func<Socket, unit>): Server = jsNative
        member __.createServer(?options: obj, ?connectionListener: Func<Socket, unit>): Server = jsNative
        member __.connect(options: obj, ?connectionListener: Function): Socket = jsNative
        member __.connect(port: float, ?host: string, ?connectionListener: Function): Socket = jsNative
        member __.connect(path: string, ?connectionListener: Function): Socket = jsNative
        member __.createConnection(options: obj, ?connectionListener: Function): Socket = jsNative
        member __.createConnection(port: float, ?host: string, ?connectionListener: Function): Socket = jsNative
        member __.createConnection(path: string, ?connectionListener: Function): Socket = jsNative
        member __.isIP(input: string): float = jsNative
        member __.isIPv4(input: string): bool = jsNative
        member __.isIPv6(input: string): bool = jsNative

[<Import("*","net")>]
let net: net_types.Globals = jsNative
//net end

module http =
    type [<AllowNullLiteral>] RequestOptions =
        abstract protocol: string option with get, set
        abstract host: string option with get, set
        abstract hostname: string option with get, set
        abstract family: float option with get, set
        abstract port: float option with get, set
        abstract localAddress: string option with get, set
        abstract socketPath: string option with get, set
        abstract ``method``: string option with get, set
        abstract path: string option with get, set
        abstract headers: obj option with get, set
        abstract auth: string option with get, set
        abstract agent: U2<Agent, bool> option with get, set

    and [<AllowNullLiteral>] Server =
        inherit net_types.Server
        abstract maxHeadersCount: float with get, set
        abstract timeout: float with get, set
        abstract listening: bool with get, set
        abstract setTimeout: msecs: float * callback: Function -> unit

    and [<AllowNullLiteral>] ServerRequest =
        inherit IncomingMessage
        abstract connection: net_types.Socket with get, set

    and [<AllowNullLiteral>] ServerResponse =
        inherit stream_types.Writable
        abstract statusCode: float with get, set
        abstract statusMessage: string with get, set
        abstract headersSent: bool with get, set
        abstract sendDate: bool with get, set
        abstract finished: bool with get, set
        abstract write: buffer: buffer_types.Buffer -> bool
        abstract write: buffer: buffer_types.Buffer * ?cb: Function -> bool
        abstract write: str: string * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?fd: string -> bool
        abstract writeContinue: unit -> unit
        abstract writeHead: statusCode: float * ?reasonPhrase: string * ?headers: obj -> unit
        abstract writeHead: statusCode: float * ?headers: obj -> unit
        abstract setHeader: name: string * value: U2<string, ResizeArray<string>> -> unit
        abstract setTimeout: msecs: float * callback: Function -> ServerResponse
        abstract getHeader: name: string -> string
        abstract removeHeader: name: string -> unit
        abstract write: chunk: obj * ?encoding: string -> obj
        abstract addTrailers: headers: obj -> unit
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: buffer_types.Buffer * ?cb: Function -> unit
        abstract ``end``: str: string * ?cb: Function -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit
        abstract ``end``: ?data: obj * ?encoding: string -> unit

    and [<AllowNullLiteral>] ClientRequest =
        inherit stream_types.Writable
        abstract write: buffer: buffer_types.Buffer -> bool
        abstract write: buffer: buffer_types.Buffer * ?cb: Function -> bool
        abstract write: str: string * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?fd: string -> bool
        abstract write: chunk: obj * ?encoding: string -> unit
        abstract abort: unit -> unit
        abstract setTimeout: timeout: float * ?callback: Function -> unit
        abstract setNoDelay: ?noDelay: bool -> unit
        abstract setSocketKeepAlive: ?enable: bool * ?initialDelay: float -> unit
        abstract setHeader: name: string * value: U2<string, ResizeArray<string>> -> unit
        abstract getHeader: name: string -> string
        abstract removeHeader: name: string -> unit
        abstract addTrailers: headers: obj -> unit
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: buffer_types.Buffer * ?cb: Function -> unit
        abstract ``end``: str: string * ?cb: Function -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit
        abstract ``end``: ?data: obj * ?encoding: string -> unit

    and [<AllowNullLiteral>] IncomingMessage =
        inherit stream_types.Readable
        abstract httpVersion: string with get, set
        abstract httpVersionMajor: float with get, set
        abstract httpVersionMinor: float with get, set
        abstract connection: net_types.Socket with get, set
        abstract headers: obj with get, set
        abstract rawHeaders: ResizeArray<string> with get, set
        abstract trailers: obj with get, set
        abstract rawTrailers: obj with get, set
        abstract ``method``: string option with get, set
        abstract url: string option with get, set
        abstract statusCode: float option with get, set
        abstract statusMessage: string option with get, set
        abstract socket: net_types.Socket with get, set
        abstract setTimeout: msecs: float * callback: Function -> NodeJS.Timer
        abstract destroy: ?error: Error -> unit

    and [<AllowNullLiteral>] ClientResponse =
        inherit IncomingMessage


    and [<AllowNullLiteral>] AgentOptions =
        abstract keepAlive: bool option with get, set
        abstract keepAliveMsecs: float option with get, set
        abstract maxSockets: float option with get, set
        abstract maxFreeSockets: float option with get, set

    and [<AllowNullLiteral>] [<Import("Agent","http")>] Agent(?opts: AgentOptions) =
        member __.maxSockets with get(): float = jsNative and set(v: float): unit = jsNative
        member __.sockets with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.requests with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.destroy(): unit = jsNative

    and [<AllowNullLiteral>] STATUS_CODESType =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: errorCode: float -> string with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: errorCode: string -> string with get, set

    type [<Import("*","http")>] Globals =
        static member METHODS with get(): ResizeArray<string> = jsNative and set(v: ResizeArray<string>): unit = jsNative
        static member STATUS_CODES with get(): STATUS_CODESType = jsNative and set(v: STATUS_CODESType): unit = jsNative
        static member globalAgent with get(): Agent = jsNative and set(v: Agent): unit = jsNative
        static member createServer(?requestListener: Func<IncomingMessage, ServerResponse, unit>): Server = jsNative
        static member createClient(?port: float, ?host: string): obj = jsNative
        static member request(options: RequestOptions, ?callback: Func<IncomingMessage, unit>): ClientRequest = jsNative
        static member get(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = jsNative



module zlib =
    type [<AllowNullLiteral>] ZlibOptions =
        abstract chunkSize: float option with get, set
        abstract windowBits: float option with get, set
        abstract level: float option with get, set
        abstract memLevel: float option with get, set
        abstract strategy: float option with get, set
        abstract dictionary: obj option with get, set
        abstract finishFlush: float option with get, set

    and [<AllowNullLiteral>] Gzip =
        inherit stream_types.Transform


    and [<AllowNullLiteral>] Gunzip =
        inherit stream_types.Transform


    and [<AllowNullLiteral>] Deflate =
        inherit stream_types.Transform


    and [<AllowNullLiteral>] Inflate =
        inherit stream_types.Transform


    and [<AllowNullLiteral>] DeflateRaw =
        inherit stream_types.Transform


    and [<AllowNullLiteral>] InflateRaw =
        inherit stream_types.Transform


    and [<AllowNullLiteral>] Unzip =
        inherit stream_types.Transform


    type [<Import("*","zlib")>] Globals =
        static member Z_NO_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_PARTIAL_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_SYNC_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_FULL_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_FINISH with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_BLOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_TREES with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_OK with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_STREAM_END with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_NEED_DICT with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_ERRNO with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_STREAM_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_DATA_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_MEM_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_BUF_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_VERSION_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_NO_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_BEST_SPEED with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_BEST_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_DEFAULT_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_FILTERED with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_HUFFMAN_ONLY with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_RLE with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_FIXED with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_DEFAULT_STRATEGY with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_BINARY with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_TEXT with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_ASCII with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_UNKNOWN with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_DEFLATED with get(): float = jsNative and set(v: float): unit = jsNative
        static member Z_NULL with get(): float = jsNative and set(v: float): unit = jsNative
        static member createGzip(?options: ZlibOptions): Gzip = jsNative
        static member createGunzip(?options: ZlibOptions): Gunzip = jsNative
        static member createDeflate(?options: ZlibOptions): Deflate = jsNative
        static member createInflate(?options: ZlibOptions): Inflate = jsNative
        static member createDeflateRaw(?options: ZlibOptions): DeflateRaw = jsNative
        static member createInflateRaw(?options: ZlibOptions): InflateRaw = jsNative
        static member createUnzip(?options: ZlibOptions): Unzip = jsNative
        static member deflate(buf: U2<buffer_types.Buffer, string>, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member deflateSync(buf: U2<buffer_types.Buffer, string>, ?options: ZlibOptions): buffer_types.Buffer = jsNative
        static member deflateRaw(buf: U2<buffer_types.Buffer, string>, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member deflateRawSync(buf: U2<buffer_types.Buffer, string>, ?options: ZlibOptions): buffer_types.Buffer = jsNative
        static member gzip(buf: buffer_types.Buffer, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member gzipSync(buf: buffer_types.Buffer, ?options: ZlibOptions): buffer_types.Buffer = jsNative
        static member gunzip(buf: buffer_types.Buffer, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member gunzipSync(buf: buffer_types.Buffer, ?options: ZlibOptions): buffer_types.Buffer = jsNative
        static member inflate(buf: buffer_types.Buffer, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member inflateSync(buf: buffer_types.Buffer, ?options: ZlibOptions): buffer_types.Buffer = jsNative
        static member inflateRaw(buf: buffer_types.Buffer, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member inflateRawSync(buf: buffer_types.Buffer, ?options: ZlibOptions): buffer_types.Buffer = jsNative
        static member unzip(buf: buffer_types.Buffer, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        static member unzipSync(buf: buffer_types.Buffer, ?options: ZlibOptions): buffer_types.Buffer = jsNative


module os =
    type [<AllowNullLiteral>] CpuInfo =
        abstract model: string with get, set
        abstract speed: float with get, set
        abstract times: obj with get, set

    and [<AllowNullLiteral>] NetworkInterfaceInfo =
        abstract address: string with get, set
        abstract netmask: string with get, set
        abstract family: string with get, set
        abstract mac: string with get, set
        abstract ``internal``: bool with get, set

    and [<AllowNullLiteral>] constantsType =
        abstract UV_UDP_REUSEADDR: float with get, set
        abstract errno: obj with get, set
        abstract signals: obj with get, set

    type [<Import("*","os")>] Globals =
        static member constants with get(): constantsType = jsNative and set(v: constantsType): unit = jsNative
        static member EOL with get(): string = jsNative and set(v: string): unit = jsNative
        static member hostname(): string = jsNative
        static member loadavg(): ResizeArray<float> = jsNative
        static member uptime(): float = jsNative
        static member freemem(): float = jsNative
        static member totalmem(): float = jsNative
        static member cpus(): ResizeArray<CpuInfo> = jsNative
        static member ``type``(): string = jsNative
        static member release(): string = jsNative
        static member networkInterfaces(): obj = jsNative
        static member homedir(): string = jsNative
        static member userInfo(?options: obj): obj = jsNative
        static member arch(): string = jsNative
        static member platform(): NodeJS.Platform = jsNative
        static member tmpdir(): string = jsNative
        static member endianness(): (* TODO StringEnum BE | LE *) string = jsNative

module vm =
    type [<AllowNullLiteral>] Context =
        interface end

    and [<AllowNullLiteral>] ScriptOptions =
        abstract filename: string option with get, set
        abstract lineOffset: float option with get, set
        abstract columnOffset: float option with get, set
        abstract displayErrors: bool option with get, set
        abstract timeout: float option with get, set
        abstract cachedData: buffer_types.Buffer option with get, set
        abstract produceCachedData: bool option with get, set

    and [<AllowNullLiteral>] RunningScriptOptions =
        abstract filename: string option with get, set
        abstract lineOffset: float option with get, set
        abstract columnOffset: float option with get, set
        abstract displayErrors: bool option with get, set
        abstract timeout: float option with get, set

    and [<AllowNullLiteral>] [<Import("Script","vm")>] Script(code: string, ?options: ScriptOptions) =
        member __.runInContext(contextifiedSandbox: Context, ?options: RunningScriptOptions): obj = jsNative
        member __.runInNewContext(?sandbox: Context, ?options: RunningScriptOptions): obj = jsNative
        member __.runInThisContext(?options: RunningScriptOptions): obj = jsNative

    type [<Import("*","vm")>] Globals =
        static member createContext(?sandbox: Context): Context = jsNative
        static member isContext(sandbox: Context): bool = jsNative
        static member runInContext(code: string, contextifiedSandbox: Context, ?options: RunningScriptOptions): obj = jsNative
        static member runInDebugContext(code: string): obj = jsNative
        static member runInNewContext(code: string, ?sandbox: Context, ?options: RunningScriptOptions): obj = jsNative
        static member runInThisContext(code: string, ?options: RunningScriptOptions): obj = jsNative




module cluster_types =
    type [<AllowNullLiteral>] ClusterSettings =
        abstract exec: string option with get, set
        abstract args: ResizeArray<string> option with get, set
        abstract silent: bool option with get, set

    and [<AllowNullLiteral>] [<Import("Worker","cluster")>] Worker =
        inherit event_types.EventEmitter
        abstract id: string with get, set
        abstract ``process``: child_process_types.ChildProcess with get, set
        abstract suicide: bool with get, set
        abstract send: message: obj * ?sendHandle: obj -> unit
        abstract kill: ?signal: string -> unit
        abstract destroy: ?signal: string -> unit
        abstract disconnect: unit -> unit

    type Globals =
        member __.settings with get(): ClusterSettings = jsNative and set(v: ClusterSettings): unit = jsNative
        member __.isMaster with get(): bool = jsNative and set(v: bool): unit = jsNative
        member __.isWorker with get(): bool = jsNative and set(v: bool): unit = jsNative
        member __.worker with get(): Worker = jsNative and set(v: Worker): unit = jsNative
        member __.workers with get(): ResizeArray<Worker> = jsNative and set(v: ResizeArray<Worker>): unit = jsNative
        member __.setupMaster(?settings: ClusterSettings): unit = jsNative
        member __.fork(?env: obj): Worker = jsNative
        member __.disconnect(?callback: Function): unit = jsNative
        member __.addListener(``event``: string, listener: Function): unit = jsNative
        member __.on(``event``: string, listener: Function): obj = jsNative
        member __.once(``event``: string, listener: Function): unit = jsNative
        member __.removeListener(``event``: string, listener: Function): unit = jsNative
        member __.removeAllListeners(?``event``: string): unit = jsNative
        member __.setMaxListeners(n: float): unit = jsNative
        member __.listeners(``event``: string): ResizeArray<Function> = jsNative
        member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = jsNative

let [<Import("*","cluster")>] cluster: cluster_types.Globals = jsNative

module url =
    type [<AllowNullLiteral>] Url =
        abstract href: string option with get, set
        abstract protocol: string option with get, set
        abstract auth: string option with get, set
        abstract hostname: string option with get, set
        abstract port: string option with get, set
        abstract host: string option with get, set
        abstract pathname: string option with get, set
        abstract search: string option with get, set
        abstract query: U2<string, obj> option with get, set
        abstract slashes: bool option with get, set
        abstract hash: string option with get, set
        abstract path: string option with get, set

    type [<Import("*","url")>] Globals =
        static member parse(urlStr: string, ?parseQueryString: bool, ?slashesDenoteHost: bool): Url = jsNative
        static member format(url: Url): string = jsNative
        static member resolve(from: string, ``to``: string): string = jsNative



module dns =
    type [<AllowNullLiteral>] MxRecord =
        abstract exchange: string with get, set
        abstract priority: float with get, set

    type [<Import("*","dns")>] Globals =
        static member NODATA with get(): string = jsNative and set(v: string): unit = jsNative
        static member FORMERR with get(): string = jsNative and set(v: string): unit = jsNative
        static member SERVFAIL with get(): string = jsNative and set(v: string): unit = jsNative
        static member NOTFOUND with get(): string = jsNative and set(v: string): unit = jsNative
        static member NOTIMP with get(): string = jsNative and set(v: string): unit = jsNative
        static member REFUSED with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADQUERY with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADNAME with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADFAMILY with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADRESP with get(): string = jsNative and set(v: string): unit = jsNative
        static member CONNREFUSED with get(): string = jsNative and set(v: string): unit = jsNative
        static member TIMEOUT with get(): string = jsNative and set(v: string): unit = jsNative
        static member EOF with get(): string = jsNative and set(v: string): unit = jsNative
        static member FILE with get(): string = jsNative and set(v: string): unit = jsNative
        static member NOMEM with get(): string = jsNative and set(v: string): unit = jsNative
        static member DESTRUCTION with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADSTR with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADFLAGS with get(): string = jsNative and set(v: string): unit = jsNative
        static member NONAME with get(): string = jsNative and set(v: string): unit = jsNative
        static member BADHINTS with get(): string = jsNative and set(v: string): unit = jsNative
        static member NOTINITIALIZED with get(): string = jsNative and set(v: string): unit = jsNative
        static member LOADIPHLPAPI with get(): string = jsNative and set(v: string): unit = jsNative
        static member ADDRGETNETWORKPARAMS with get(): string = jsNative and set(v: string): unit = jsNative
        static member CANCELLED with get(): string = jsNative and set(v: string): unit = jsNative
        static member lookup(domain: string, family: float, callback: Func<Error, string, float, unit>): string = jsNative
        static member lookup(domain: string, callback: Func<Error, string, float, unit>): string = jsNative
        static member resolve(domain: string, rrtype: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolve(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolve4(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolve6(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolveMx(domain: string, callback: Func<Error, ResizeArray<MxRecord>, unit>): ResizeArray<string> = jsNative
        static member resolveTxt(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolveSrv(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolveNs(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member resolveCname(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member reverse(ip: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        static member setServers(servers: ResizeArray<string>): unit = jsNative







module dgram =
    type [<AllowNullLiteral>] RemoteInfo =
        abstract address: string with get, set
        abstract family: string with get, set
        abstract port: float with get, set

    and [<AllowNullLiteral>] AddressInfo =
        abstract address: string with get, set
        abstract family: string with get, set
        abstract port: float with get, set

    and [<AllowNullLiteral>] BindOptions =
        abstract port: float with get, set
        abstract address: string option with get, set
        abstract exclusive: bool option with get, set

    and [<AllowNullLiteral>] SocketOptions =
        abstract ``type``: (* TODO StringEnum udp4 | udp6 *) string with get, set
        abstract reuseAddr: bool option with get, set

    and [<AllowNullLiteral>] Socket =
        inherit event_types.EventEmitter
        abstract send: msg: U3<buffer_types.Buffer, string, ResizeArray<obj>> * port: float * address: string * ?callback: Func<Error, float, unit> -> unit
        abstract send: msg: U3<buffer_types.Buffer, string, ResizeArray<obj>> * offset: float * length: float * port: float * address: string * ?callback: Func<Error, float, unit> -> unit
        abstract bind: ?port: float * ?address: string * ?callback: Func<unit, unit> -> unit
        abstract bind: options: BindOptions * ?callback: Function -> unit
        abstract close: ?callback: obj -> unit
        abstract address: unit -> AddressInfo
        abstract setBroadcast: flag: bool -> unit
        abstract setTTL: ttl: float -> unit
        abstract setMulticastTTL: ttl: float -> unit
        abstract setMulticastLoopback: flag: bool -> unit
        abstract addMembership: multicastAddress: string * ?multicastInterface: string -> unit
        abstract dropMembership: multicastAddress: string * ?multicastInterface: string -> unit
        abstract ref: unit -> obj
        abstract unref: unit -> obj
        abstract addListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.addListener('close',$1...)")>] abstract addListener_close: listener: Func<unit, unit> -> obj
        [<Emit("$0.addListener('error',$1...)")>] abstract addListener_error: listener: Func<Error, unit> -> obj
        [<Emit("$0.addListener('listening',$1...)")>] abstract addListener_listening: listener: Func<unit, unit> -> obj
        [<Emit("$0.addListener('message',$1...)")>] abstract addListener_message: listener: Func<buffer_types.Buffer, AddressInfo, unit> -> obj
        abstract emit: ``event``: string * [<ParamArray>] args: obj[] -> bool
        [<Emit("$0.emit('close')")>] abstract emit_close: unit -> bool
        [<Emit("$0.emit('error',$1...)")>] abstract emit_error: err: Error -> bool
        [<Emit("$0.emit('listening')")>] abstract emit_listening: unit -> bool
        [<Emit("$0.emit('message',$1...)")>] abstract emit_message: msg: buffer_types.Buffer * rinfo: AddressInfo -> bool
        abstract on: ``event``: string * listener: Function -> obj
        [<Emit("$0.on('close',$1...)")>] abstract on_close: listener: Func<unit, unit> -> obj
        [<Emit("$0.on('error',$1...)")>] abstract on_error: listener: Func<Error, unit> -> obj
        [<Emit("$0.on('listening',$1...)")>] abstract on_listening: listener: Func<unit, unit> -> obj
        [<Emit("$0.on('message',$1...)")>] abstract on_message: listener: Func<buffer_types.Buffer, AddressInfo, unit> -> obj
        abstract once: ``event``: string * listener: Function -> obj
        [<Emit("$0.once('close',$1...)")>] abstract once_close: listener: Func<unit, unit> -> obj
        [<Emit("$0.once('error',$1...)")>] abstract once_error: listener: Func<Error, unit> -> obj
        [<Emit("$0.once('listening',$1...)")>] abstract once_listening: listener: Func<unit, unit> -> obj
        [<Emit("$0.once('message',$1...)")>] abstract once_message: listener: Func<buffer_types.Buffer, AddressInfo, unit> -> obj
        abstract prependListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependListener('close',$1...)")>] abstract prependListener_close: listener: Func<unit, unit> -> obj
        [<Emit("$0.prependListener('error',$1...)")>] abstract prependListener_error: listener: Func<Error, unit> -> obj
        [<Emit("$0.prependListener('listening',$1...)")>] abstract prependListener_listening: listener: Func<unit, unit> -> obj
        [<Emit("$0.prependListener('message',$1...)")>] abstract prependListener_message: listener: Func<buffer_types.Buffer, AddressInfo, unit> -> obj
        abstract prependOnceListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependOnceListener('close',$1...)")>] abstract prependOnceListener_close: listener: Func<unit, unit> -> obj
        [<Emit("$0.prependOnceListener('error',$1...)")>] abstract prependOnceListener_error: listener: Func<Error, unit> -> obj
        [<Emit("$0.prependOnceListener('listening',$1...)")>] abstract prependOnceListener_listening: listener: Func<unit, unit> -> obj
        [<Emit("$0.prependOnceListener('message',$1...)")>] abstract prependOnceListener_message: listener: Func<buffer_types.Buffer, AddressInfo, unit> -> obj

    type [<Import("*","dgram")>] Globals =
        static member createSocket(``type``: string, ?callback: Func<buffer_types.Buffer, RemoteInfo, unit>): Socket = jsNative
        static member createSocket(options: SocketOptions, ?callback: Func<buffer_types.Buffer, RemoteInfo, unit>): Socket = jsNative



module fs =
    type [<AllowNullLiteral>] Stats =
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

    and [<AllowNullLiteral>] FSWatcher =
        inherit event_types.EventEmitter
        abstract close: unit -> unit
        abstract addListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.addListener('change',$1...)")>] abstract addListener_change: listener: Func<string, U2<string, buffer_types.Buffer>, unit> -> obj
        [<Emit("$0.addListener('error',$1...)")>] abstract addListener_error: listener: Func<float, string, unit> -> obj
        abstract on: ``event``: string * listener: Function -> obj
        [<Emit("$0.on('change',$1...)")>] abstract on_change: listener: Func<string, U2<string, buffer_types.Buffer>, unit> -> obj
        [<Emit("$0.on('error',$1...)")>] abstract on_error: listener: Func<float, string, unit> -> obj
        abstract once: ``event``: string * listener: Function -> obj
        [<Emit("$0.once('change',$1...)")>] abstract once_change: listener: Func<string, U2<string, buffer_types.Buffer>, unit> -> obj
        [<Emit("$0.once('error',$1...)")>] abstract once_error: listener: Func<float, string, unit> -> obj
        abstract prependListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependListener('change',$1...)")>] abstract prependListener_change: listener: Func<string, U2<string, buffer_types.Buffer>, unit> -> obj
        [<Emit("$0.prependListener('error',$1...)")>] abstract prependListener_error: listener: Func<float, string, unit> -> obj
        abstract prependOnceListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependOnceListener('change',$1...)")>] abstract prependOnceListener_change: listener: Func<string, U2<string, buffer_types.Buffer>, unit> -> obj
        [<Emit("$0.prependOnceListener('error',$1...)")>] abstract prependOnceListener_error: listener: Func<float, string, unit> -> obj

    and [<AllowNullLiteral>] ReadStream =
        inherit stream_types.Readable
        abstract bytesRead: float with get, set
        abstract path: U2<string, buffer_types.Buffer> with get, set
        abstract close: unit -> unit
        abstract destroy: unit -> unit
        abstract addListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.addListener('open',$1...)")>] abstract addListener_open: listener: Func<float, unit> -> obj
        [<Emit("$0.addListener('close',$1...)")>] abstract addListener_close: listener: Func<unit, unit> -> obj
        abstract on: ``event``: string * listener: Function -> obj
        [<Emit("$0.on('open',$1...)")>] abstract on_open: listener: Func<float, unit> -> obj
        [<Emit("$0.on('close',$1...)")>] abstract on_close: listener: Func<unit, unit> -> obj
        abstract once: ``event``: string * listener: Function -> obj
        [<Emit("$0.once('open',$1...)")>] abstract once_open: listener: Func<float, unit> -> obj
        [<Emit("$0.once('close',$1...)")>] abstract once_close: listener: Func<unit, unit> -> obj
        abstract prependListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependListener('open',$1...)")>] abstract prependListener_open: listener: Func<float, unit> -> obj
        [<Emit("$0.prependListener('close',$1...)")>] abstract prependListener_close: listener: Func<unit, unit> -> obj
        abstract prependOnceListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependOnceListener('open',$1...)")>] abstract prependOnceListener_open: listener: Func<float, unit> -> obj
        [<Emit("$0.prependOnceListener('close',$1...)")>] abstract prependOnceListener_close: listener: Func<unit, unit> -> obj

    and [<AllowNullLiteral>] WriteStream =
        inherit stream_types.Writable
        abstract bytesWritten: float with get, set
        abstract path: U2<string, buffer_types.Buffer> with get, set
        abstract close: unit -> unit
        abstract addListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.addListener('open',$1...)")>] abstract addListener_open: listener: Func<float, unit> -> obj
        [<Emit("$0.addListener('close',$1...)")>] abstract addListener_close: listener: Func<unit, unit> -> obj
        abstract on: ``event``: string * listener: Function -> obj
        [<Emit("$0.on('open',$1...)")>] abstract on_open: listener: Func<float, unit> -> obj
        [<Emit("$0.on('close',$1...)")>] abstract on_close: listener: Func<unit, unit> -> obj
        abstract once: ``event``: string * listener: Function -> obj
        [<Emit("$0.once('open',$1...)")>] abstract once_open: listener: Func<float, unit> -> obj
        [<Emit("$0.once('close',$1...)")>] abstract once_close: listener: Func<unit, unit> -> obj
        abstract prependListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependListener('open',$1...)")>] abstract prependListener_open: listener: Func<float, unit> -> obj
        [<Emit("$0.prependListener('close',$1...)")>] abstract prependListener_close: listener: Func<unit, unit> -> obj
        abstract prependOnceListener: ``event``: string * listener: Function -> obj
        [<Emit("$0.prependOnceListener('open',$1...)")>] abstract prependOnceListener_open: listener: Func<float, unit> -> obj
        [<Emit("$0.prependOnceListener('close',$1...)")>] abstract prependOnceListener_close: listener: Func<unit, unit> -> obj

    type [<Import("*","fs")>] Globals =
        static member rename(oldPath: string, newPath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member renameSync(oldPath: string, newPath: string): unit = jsNative
        static member truncate(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member truncate(path: U2<string, buffer_types.Buffer>, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member truncateSync(path: U2<string, buffer_types.Buffer>, ?len: float): unit = jsNative
        static member ftruncate(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member ftruncate(fd: float, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member ftruncateSync(fd: float, ?len: float): unit = jsNative
        static member chown(path: U2<string, buffer_types.Buffer>, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member chownSync(path: U2<string, buffer_types.Buffer>, uid: float, gid: float): unit = jsNative
        static member fchown(fd: float, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member fchownSync(fd: float, uid: float, gid: float): unit = jsNative
        static member lchown(path: U2<string, buffer_types.Buffer>, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member lchownSync(path: U2<string, buffer_types.Buffer>, uid: float, gid: float): unit = jsNative
        static member chmod(path: U2<string, buffer_types.Buffer>, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member chmod(path: U2<string, buffer_types.Buffer>, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member chmodSync(path: U2<string, buffer_types.Buffer>, mode: float): unit = jsNative
        static member chmodSync(path: U2<string, buffer_types.Buffer>, mode: string): unit = jsNative
        static member fchmod(fd: float, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member fchmod(fd: float, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member fchmodSync(fd: float, mode: float): unit = jsNative
        static member fchmodSync(fd: float, mode: string): unit = jsNative
        static member lchmod(path: U2<string, buffer_types.Buffer>, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member lchmod(path: U2<string, buffer_types.Buffer>, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member lchmodSync(path: U2<string, buffer_types.Buffer>, mode: float): unit = jsNative
        static member lchmodSync(path: U2<string, buffer_types.Buffer>, mode: string): unit = jsNative
        static member stat(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = jsNative
        static member lstat(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = jsNative
        static member fstat(fd: float, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = jsNative
        static member statSync(path: U2<string, buffer_types.Buffer>): Stats = jsNative
        static member lstatSync(path: U2<string, buffer_types.Buffer>): Stats = jsNative
        static member fstatSync(fd: float): Stats = jsNative
        static member link(srcpath: U2<string, buffer_types.Buffer>, dstpath: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member linkSync(srcpath: U2<string, buffer_types.Buffer>, dstpath: U2<string, buffer_types.Buffer>): unit = jsNative
        static member symlink(srcpath: U2<string, buffer_types.Buffer>, dstpath: U2<string, buffer_types.Buffer>, ?``type``: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member symlinkSync(srcpath: U2<string, buffer_types.Buffer>, dstpath: U2<string, buffer_types.Buffer>, ?``type``: string): unit = jsNative
        static member readlink(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = jsNative
        static member readlinkSync(path: U2<string, buffer_types.Buffer>): string = jsNative
        static member realpath(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = jsNative
        static member realpath(path: U2<string, buffer_types.Buffer>, cache: obj, callback: Func<NodeJS.ErrnoException, string, obj>): unit = jsNative
        static member realpathSync(path: U2<string, buffer_types.Buffer>, ?cache: obj): string = jsNative
        static member unlink(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member unlinkSync(path: U2<string, buffer_types.Buffer>): unit = jsNative
        static member rmdir(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member rmdirSync(path: U2<string, buffer_types.Buffer>): unit = jsNative
        static member mkdir(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member mkdir(path: U2<string, buffer_types.Buffer>, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member mkdir(path: U2<string, buffer_types.Buffer>, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member mkdirSync(path: U2<string, buffer_types.Buffer>, ?mode: float): unit = jsNative
        static member mkdirSync(path: U2<string, buffer_types.Buffer>, ?mode: string): unit = jsNative
        static member mkdtemp(prefix: string, ?callback: Func<NodeJS.ErrnoException, string, unit>): unit = jsNative
        static member mkdtempSync(prefix: string): string = jsNative
        static member readdir(path: U2<string, buffer_types.Buffer>, ?callback: Func<NodeJS.ErrnoException, ResizeArray<string>, unit>): unit = jsNative
        static member readdirSync(path: U2<string, buffer_types.Buffer>): ResizeArray<string> = jsNative
        static member close(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member closeSync(fd: float): unit = jsNative
        static member ``open``(path: U2<string, buffer_types.Buffer>, flags: U2<string, float>, callback: Func<NodeJS.ErrnoException, float, unit>): unit = jsNative
        static member ``open``(path: U2<string, buffer_types.Buffer>, flags: U2<string, float>, mode: float, callback: Func<NodeJS.ErrnoException, float, unit>): unit = jsNative
        static member openSync(path: U2<string, buffer_types.Buffer>, flags: U2<string, float>, ?mode: float): float = jsNative
        static member utimes(path: U2<string, buffer_types.Buffer>, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member utimes(path: U2<string, buffer_types.Buffer>, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member utimesSync(path: U2<string, buffer_types.Buffer>, atime: float, mtime: float): unit = jsNative
        static member utimesSync(path: U2<string, buffer_types.Buffer>, atime: DateTime, mtime: DateTime): unit = jsNative
        static member futimes(fd: float, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member futimes(fd: float, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member futimesSync(fd: float, atime: float, mtime: float): unit = jsNative
        static member futimesSync(fd: float, atime: DateTime, mtime: DateTime): unit = jsNative
        static member fsync(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member fsyncSync(fd: float): unit = jsNative
        static member write(fd: float, buffer: buffer_types.Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, buffer_types.Buffer, unit>): unit = jsNative
        static member write(fd: float, buffer: buffer_types.Buffer, offset: float, length: float, ?callback: Func<NodeJS.ErrnoException, float, buffer_types.Buffer, unit>): unit = jsNative
        static member write(fd: float, data: obj, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = jsNative
        static member write(fd: float, data: obj, offset: float, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = jsNative
        static member write(fd: float, data: obj, offset: float, encoding: string, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = jsNative
        static member writeSync(fd: float, buffer: buffer_types.Buffer, offset: float, length: float, ?position: float): float = jsNative
        static member writeSync(fd: float, data: obj, ?position: float, ?enconding: string): float = jsNative
        static member read(fd: float, buffer: buffer_types.Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, buffer_types.Buffer, unit>): unit = jsNative
        static member readSync(fd: float, buffer: buffer_types.Buffer, offset: float, length: float, position: float): float = jsNative
        static member readFile(filename: string, encoding: string, callback: Func<NodeJS.ErrnoException, string, unit>): unit = jsNative
        static member readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, string, unit>): unit = jsNative
        static member readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, buffer_types.Buffer, unit>): unit = jsNative
        static member readFile(filename: string, callback: Func<NodeJS.ErrnoException, buffer_types.Buffer, unit>): unit = jsNative
        static member readFileSync(filename: string, encoding: string): string = jsNative
        static member readFileSync(filename: string, options: obj): string = jsNative
        static member readFileSync(filename: string, ?options: obj): buffer_types.Buffer = jsNative
        static member writeFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member writeFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member writeFileSync(filename: string, data: obj, ?options: obj): unit = jsNative
        static member appendFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member appendFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member appendFileSync(filename: string, data: obj, ?options: obj): unit = jsNative
        static member watchFile(filename: string, listener: Func<Stats, Stats, unit>): unit = jsNative
        static member watchFile(filename: string, options: obj, listener: Func<Stats, Stats, unit>): unit = jsNative
        static member unwatchFile(filename: string, ?listener: Func<Stats, Stats, unit>): unit = jsNative
        static member watch(filename: string, ?listener: Func<string, string, obj>): FSWatcher = jsNative
        static member watch(filename: string, encoding: string, ?listener: Func<string, U2<string, buffer_types.Buffer>, obj>): FSWatcher = jsNative
        static member watch(filename: string, options: obj, ?listener: Func<string, U2<string, buffer_types.Buffer>, obj>): FSWatcher = jsNative
        static member exists(path: U2<string, buffer_types.Buffer>, ?callback: Func<bool, unit>): unit = jsNative
        static member existsSync(path: U2<string, buffer_types.Buffer>): bool = jsNative
        static member access(path: U2<string, buffer_types.Buffer>, callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member access(path: U2<string, buffer_types.Buffer>, mode: float, callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        static member accessSync(path: U2<string, buffer_types.Buffer>, ?mode: float): unit = jsNative
        static member createReadStream(path: U2<string, buffer_types.Buffer>, ?options: obj): ReadStream = jsNative
        static member createWriteStream(path: U2<string, buffer_types.Buffer>, ?options: obj): WriteStream = jsNative
        static member fdatasync(fd: float, callback: Function): unit = jsNative
        static member fdatasyncSync(fd: float): unit = jsNative

    module constants =
        type [<Import("constants","fs")>] Globals =
            static member F_OK with get(): float = jsNative and set(v: float): unit = jsNative
            static member R_OK with get(): float = jsNative and set(v: float): unit = jsNative
            static member W_OK with get(): float = jsNative and set(v: float): unit = jsNative
            static member X_OK with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_RDONLY with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_WRONLY with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_RDWR with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_CREAT with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_EXCL with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_NOCTTY with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_TRUNC with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_APPEND with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_DIRECTORY with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_NOATIME with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_NOFOLLOW with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_SYNC with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_SYMLINK with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_DIRECT with get(): float = jsNative and set(v: float): unit = jsNative
            static member O_NONBLOCK with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFMT with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFREG with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFDIR with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFCHR with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFBLK with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFIFO with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFLNK with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IFSOCK with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IRWXU with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IRUSR with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IWUSR with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IXUSR with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IRWXG with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IRGRP with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IWGRP with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IXGRP with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IRWXO with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IROTH with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IWOTH with get(): float = jsNative and set(v: float): unit = jsNative
            static member S_IXOTH with get(): float = jsNative and set(v: float): unit = jsNative



module path =
    type [<AllowNullLiteral>] ParsedPath =
        abstract root: string with get, set
        abstract dir: string with get, set
        abstract ``base``: string with get, set
        abstract ext: string with get, set
        abstract name: string with get, set

    type [<Import("*","path")>] Globals =
        static member sep with get(): string = jsNative and set(v: string): unit = jsNative
        static member delimiter with get(): string = jsNative and set(v: string): unit = jsNative
        static member normalize(p: string): string = jsNative
        static member join([<ParamArray>] paths: string[]): string = jsNative
        static member resolve([<ParamArray>] pathSegments: obj[]): string = jsNative
        static member isAbsolute(path: string): bool = jsNative
        static member relative(from: string, ``to``: string): string = jsNative
        static member dirname(p: string): string = jsNative
        static member basename(p: string, ?ext: string): string = jsNative
        static member extname(p: string): string = jsNative
        static member parse(pathString: string): ParsedPath = jsNative
        static member format(pathObject: ParsedPath): string = jsNative

    module posix =
        type [<Import("posix","path")>] Globals =
            static member sep with get(): string = jsNative and set(v: string): unit = jsNative
            static member delimiter with get(): string = jsNative and set(v: string): unit = jsNative
            static member normalize(p: string): string = jsNative
            static member join([<ParamArray>] paths: obj[]): string = jsNative
            static member resolve([<ParamArray>] pathSegments: obj[]): string = jsNative
            static member isAbsolute(p: string): bool = jsNative
            static member relative(from: string, ``to``: string): string = jsNative
            static member dirname(p: string): string = jsNative
            static member basename(p: string, ?ext: string): string = jsNative
            static member extname(p: string): string = jsNative
            static member parse(p: string): ParsedPath = jsNative
            static member format(pP: ParsedPath): string = jsNative



    module win32 =
        type [<Import("win32","path")>] Globals =
            static member sep with get(): string = jsNative and set(v: string): unit = jsNative
            static member delimiter with get(): string = jsNative and set(v: string): unit = jsNative
            static member normalize(p: string): string = jsNative
            static member join([<ParamArray>] paths: obj[]): string = jsNative
            static member resolve([<ParamArray>] pathSegments: obj[]): string = jsNative
            static member isAbsolute(p: string): bool = jsNative
            static member relative(from: string, ``to``: string): string = jsNative
            static member dirname(p: string): string = jsNative
            static member basename(p: string, ?ext: string): string = jsNative
            static member extname(p: string): string = jsNative
            static member parse(p: string): ParsedPath = jsNative
            static member format(pP: ParsedPath): string = jsNative



module string_decoder =
    type [<AllowNullLiteral>] NodeStringDecoder =
        abstract write: buffer: buffer_types.Buffer -> string
        abstract ``end``: ?buffer: buffer_types.Buffer -> string

    and [<AllowNullLiteral>] StringDecoderType =
        [<Emit("new $0($1...)")>] abstract Create: ?encoding: string -> NodeStringDecoder

    type [<Import("*","string_decoder")>] Globals =
        static member StringDecoder with get(): StringDecoderType = jsNative and set(v: StringDecoderType): unit = jsNative

module crypto_types =
    type [<AllowNullLiteral>] CredentialDetails =
        abstract pfx: string with get, set
        abstract key: string with get, set
        abstract passphrase: string with get, set
        abstract cert: string with get, set
        abstract ca: obj with get, set
        abstract crl: obj with get, set
        abstract ciphers: string with get, set

    and [<AllowNullLiteral>] Credentials =
        abstract context: obj option with get, set

    and [<AllowNullLiteral>] Hash =
        abstract update: data: obj * ?input_encoding: string -> Hash
        [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> buffer_types.Buffer
        abstract digest: encoding: string -> obj
        abstract digest: unit -> buffer_types.Buffer

    and [<AllowNullLiteral>] Hmac =
        abstract update: data: obj * ?input_encoding: string -> Hmac
        [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> buffer_types.Buffer
        abstract digest: encoding: string -> obj
        abstract digest: unit -> buffer_types.Buffer

    and [<AllowNullLiteral>] Cipher =
        abstract update: data: buffer_types.Buffer -> buffer_types.Buffer
        abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract final: unit -> buffer_types.Buffer
        abstract final: output_encoding: string -> string
        abstract setAutoPadding: auto_padding: bool -> unit

    and [<AllowNullLiteral>] Decipher =
        abstract update: data: buffer_types.Buffer -> buffer_types.Buffer
        abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract final: unit -> buffer_types.Buffer
        abstract final: output_encoding: string -> string
        abstract setAutoPadding: auto_padding: bool -> unit

    and [<AllowNullLiteral>] Signer =
        inherit stream_types.Writable
        abstract update: data: obj -> unit
        abstract sign: private_key: string * output_format: string -> string

    and [<AllowNullLiteral>] Verify =
        inherit stream_types.Writable
        abstract update: data: obj -> unit
        abstract verify: ``object``: string * signature: string * ?signature_format: string -> bool

    and [<AllowNullLiteral>] DiffieHellman =
        abstract generateKeys: ?encoding: string -> string
        abstract computeSecret: other_public_key: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract getPrime: ?encoding: string -> string
        abstract getGenerator: encoding: string -> string
        abstract getPublicKey: ?encoding: string -> string
        abstract getPrivateKey: ?encoding: string -> string
        abstract setPublicKey: public_key: string * ?encoding: string -> unit
        abstract setPrivateKey: public_key: string * ?encoding: string -> unit

    type Globals =
        member __.createCredentials(details: CredentialDetails): Credentials = jsNative
        member __.createHash(algorithm: string): Hash = jsNative
        member __.createHmac(algorithm: string, key: string): Hmac = jsNative
        member __.createHmac(algorithm: string, key: buffer_types.Buffer): Hmac = jsNative
        member __.createCipher(algorithm: string, password: obj): Cipher = jsNative
        member __.createCipheriv(algorithm: string, key: obj, iv: obj): Cipher = jsNative
        member __.createDecipher(algorithm: string, password: obj): Decipher = jsNative
        member __.createDecipheriv(algorithm: string, key: obj, iv: obj): Decipher = jsNative
        member __.createSign(algorithm: string): Signer = jsNative
        member __.createVerify(algorith: string): Verify = jsNative
        member __.createDiffieHellman(prime_length: float): DiffieHellman = jsNative
        member __.createDiffieHellman(prime: float, ?encoding: string): DiffieHellman = jsNative
        member __.getDiffieHellman(group_name: string): DiffieHellman = jsNative
        member __.pbkdf2(password: string, salt: string, iterations: float, keylen: float, callback: Func<Error, buffer_types.Buffer, obj>): unit = jsNative
        member __.pbkdf2(password: string, salt: string, iterations: float, keylen: float, digest: string, callback: Func<Error, buffer_types.Buffer, obj>): unit = jsNative
        member __.pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float): buffer_types.Buffer = jsNative
        member __.pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float, digest: string): buffer_types.Buffer = jsNative
        member __.randomBytes(size: float): buffer_types.Buffer = jsNative
        member __.randomBytes(size: float, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative
        member __.pseudoRandomBytes(size: float): buffer_types.Buffer = jsNative
        member __.pseudoRandomBytes(size: float, callback: Func<Error, buffer_types.Buffer, unit>): unit = jsNative

let [<Import("*","crypto")>] crypto: crypto_types.Globals = jsNative

module tls_types =
    type [<AllowNullLiteral>] TlsOptions =
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

    and [<AllowNullLiteral>] ConnectionOptions =
        abstract host: string option with get, set
        abstract port: float option with get, set
        abstract socket: net_types.Socket option with get, set
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract rejectUnauthorized: bool option with get, set
        abstract NPNProtocols: obj option with get, set
        abstract servername: string option with get, set

    and [<AllowNullLiteral>] Server =
        inherit net_types.Server
        abstract maxConnections: float with get, set
        abstract connections: float with get, set
        abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: Function -> Server
        abstract listen: path: string * ?listeningListener: Function -> Server
        abstract listen: handle: obj * ?listeningListener: Function -> Server
        abstract listen: port: float * ?host: string * ?callback: Function -> Server
        abstract close: unit -> Server
        abstract address: unit -> obj
        abstract addContext: hostName: string * credentials: obj -> unit

    and [<AllowNullLiteral>] ClearTextStream =
        abstract writable: bool with get, set
        abstract _write: chunk: obj * encoding: string * callback: Function -> unit
        abstract write: chunk: obj * ?cb: Function -> bool
        abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
        abstract ``end``: unit -> unit
        abstract ``end``: chunk: obj * ?cb: Function -> unit
        abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit
        abstract authorized: bool with get, set
        abstract authorizationError: Error with get, set
        abstract getCipher: obj with get, set
        abstract address: obj with get, set
        abstract remoteAddress: string with get, set
        abstract remotePort: float with get, set
        abstract getPeerCertificate: unit -> obj

    and [<AllowNullLiteral>] SecurePair =
        abstract encrypted: obj with get, set
        abstract cleartext: obj with get, set

    and [<AllowNullLiteral>] SecureContextOptions =
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract crl: obj option with get, set
        abstract ciphers: string option with get, set
        abstract honorCipherOrder: bool option with get, set

    and [<AllowNullLiteral>] SecureContext =
        abstract context: obj with get, set

    type Globals =
        member __.CLIENT_RENEG_LIMIT with get(): float = jsNative and set(v: float): unit = jsNative
        member __.CLIENT_RENEG_WINDOW with get(): float = jsNative and set(v: float): unit = jsNative
        member __.createServer(options: TlsOptions, ?secureConnectionListener: Func<ClearTextStream, unit>): Server = jsNative
        member __.connect(options: TlsOptions, ?secureConnectionListener: Func<unit, unit>): ClearTextStream = jsNative
        member __.connect(port: float, ?host: string, ?options: ConnectionOptions, ?secureConnectListener: Func<unit, unit>): ClearTextStream = jsNative
        member __.connect(port: float, ?options: ConnectionOptions, ?secureConnectListener: Func<unit, unit>): ClearTextStream = jsNative
        member __.createSecurePair(?credentials: crypto_types.Credentials, ?isServer: bool, ?requestCert: bool, ?rejectUnauthorized: bool): SecurePair = jsNative
        member __.createSecureContext(details: SecureContextOptions): SecureContext = jsNative

let [<Import("*","tls")>] tls: tls_types.Globals = jsNative

module https =
    type [<AllowNullLiteral>] ServerOptions =
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
        abstract SNICallback: Func<string, Func<Error, tls_types.SecureContext, obj>, obj> option with get, set

    and [<AllowNullLiteral>] RequestOptions =
        inherit http.RequestOptions
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract ciphers: string option with get, set
        abstract rejectUnauthorized: bool option with get, set
        abstract secureProtocol: string option with get, set

    and [<AllowNullLiteral>] Agent =
        abstract maxSockets: float with get, set
        abstract sockets: obj with get, set
        abstract requests: obj with get, set
        abstract destroy: unit -> unit


    and [<AllowNullLiteral>] AgentOptions =
        inherit http.AgentOptions
        abstract pfx: obj option with get, set
        abstract key: obj option with get, set
        abstract passphrase: string option with get, set
        abstract cert: obj option with get, set
        abstract ca: obj option with get, set
        abstract ciphers: string option with get, set
        abstract rejectUnauthorized: bool option with get, set
        abstract secureProtocol: string option with get, set
        abstract maxCachedSessions: float option with get, set

    and [<AllowNullLiteral>] AgentType =
        [<Emit("new $0($1...)")>] abstract Create: ?options: AgentOptions -> Agent

    and [<AllowNullLiteral>] Server =
        inherit tls_types.Server


    type [<Import("*","https")>] Globals =
        static member Agent with get(): AgentType = jsNative and set(v: AgentType): unit = jsNative
        static member globalAgent with get(): Agent = jsNative and set(v: Agent): unit = jsNative
        static member createServer(options: ServerOptions, ?requestListener: Function): Server = jsNative
        static member request(options: RequestOptions, ?callback: Func<http.IncomingMessage, unit>): http.ClientRequest = jsNative
        static member get(options: RequestOptions, ?callback: Func<http.IncomingMessage, unit>): http.ClientRequest = jsNative

module util =
    type [<AllowNullLiteral>] InspectOptions =
        abstract showHidden: bool option with get, set
        abstract depth: float option with get, set
        abstract colors: bool option with get, set
        abstract customInspect: bool option with get, set

    type [<Import("*","util")>] Globals =
        static member format(format: obj, [<ParamArray>] param: obj[]): string = jsNative
        static member debug(string: string): unit = jsNative
        static member error([<ParamArray>] param: obj[]): unit = jsNative
        static member puts([<ParamArray>] param: obj[]): unit = jsNative
        static member print([<ParamArray>] param: obj[]): unit = jsNative
        static member log(string: string): unit = jsNative
        static member inspect(``object``: obj, ?showHidden: bool, ?depth: float, ?color: bool): string = jsNative
        static member inspect(``object``: obj, options: InspectOptions): string = jsNative
        static member isArray(``object``: obj): bool = jsNative
        static member isRegExp(``object``: obj): bool = jsNative
        static member isDate(``object``: obj): bool = jsNative
        static member isError(``object``: obj): bool = jsNative
        static member inherits(``constructor``: obj, superConstructor: obj): unit = jsNative
        static member debuglog(key: string): Func<string, obj, unit> = jsNative
        static member isBoolean(``object``: obj): bool = jsNative
        static member isBuffer(``object``: obj): bool = jsNative
        static member isFunction(``object``: obj): bool = jsNative
        static member isNull(``object``: obj): bool = jsNative
        static member isNullOrUndefined(``object``: obj): bool = jsNative
        static member isNumber(``object``: obj): bool = jsNative
        static member isObject(``object``: obj): bool = jsNative
        static member isPrimitive(``object``: obj): bool = jsNative
        static member isString(``object``: obj): bool = jsNative
        static member isSymbol(``object``: obj): bool = jsNative
        static member isUndefined(``object``: obj): bool = jsNative
        static member deprecate(fn: Function, message: string): Function = jsNative


module tty =
    type [<AllowNullLiteral>] ReadStream =
        inherit net_types.Socket
        abstract isRaw: bool with get, set
        abstract isTTY: bool with get, set
        abstract setRawMode: mode: bool -> unit

    and [<AllowNullLiteral>] WriteStream =
        inherit net_types.Socket
        abstract columns: float with get, set
        abstract rows: float with get, set
        abstract isTTY: bool with get, set

    type [<Import("*","tty")>] Globals =
        static member isatty(fd: float): bool = jsNative


module constants =
    type [<Import("*","constants")>] Globals =
        static member E2BIG with get(): float = jsNative and set(v: float): unit = jsNative
        static member EACCES with get(): float = jsNative and set(v: float): unit = jsNative
        static member EADDRINUSE with get(): float = jsNative and set(v: float): unit = jsNative
        static member EADDRNOTAVAIL with get(): float = jsNative and set(v: float): unit = jsNative
        static member EAFNOSUPPORT with get(): float = jsNative and set(v: float): unit = jsNative
        static member EAGAIN with get(): float = jsNative and set(v: float): unit = jsNative
        static member EALREADY with get(): float = jsNative and set(v: float): unit = jsNative
        static member EBADF with get(): float = jsNative and set(v: float): unit = jsNative
        static member EBADMSG with get(): float = jsNative and set(v: float): unit = jsNative
        static member EBUSY with get(): float = jsNative and set(v: float): unit = jsNative
        static member ECANCELED with get(): float = jsNative and set(v: float): unit = jsNative
        static member ECHILD with get(): float = jsNative and set(v: float): unit = jsNative
        static member ECONNABORTED with get(): float = jsNative and set(v: float): unit = jsNative
        static member ECONNREFUSED with get(): float = jsNative and set(v: float): unit = jsNative
        static member ECONNRESET with get(): float = jsNative and set(v: float): unit = jsNative
        static member EDEADLK with get(): float = jsNative and set(v: float): unit = jsNative
        static member EDESTADDRREQ with get(): float = jsNative and set(v: float): unit = jsNative
        static member EDOM with get(): float = jsNative and set(v: float): unit = jsNative
        static member EEXIST with get(): float = jsNative and set(v: float): unit = jsNative
        static member EFAULT with get(): float = jsNative and set(v: float): unit = jsNative
        static member EFBIG with get(): float = jsNative and set(v: float): unit = jsNative
        static member EHOSTUNREACH with get(): float = jsNative and set(v: float): unit = jsNative
        static member EIDRM with get(): float = jsNative and set(v: float): unit = jsNative
        static member EILSEQ with get(): float = jsNative and set(v: float): unit = jsNative
        static member EINPROGRESS with get(): float = jsNative and set(v: float): unit = jsNative
        static member EINTR with get(): float = jsNative and set(v: float): unit = jsNative
        static member EINVAL with get(): float = jsNative and set(v: float): unit = jsNative
        static member EIO with get(): float = jsNative and set(v: float): unit = jsNative
        static member EISCONN with get(): float = jsNative and set(v: float): unit = jsNative
        static member EISDIR with get(): float = jsNative and set(v: float): unit = jsNative
        static member ELOOP with get(): float = jsNative and set(v: float): unit = jsNative
        static member EMFILE with get(): float = jsNative and set(v: float): unit = jsNative
        static member EMLINK with get(): float = jsNative and set(v: float): unit = jsNative
        static member EMSGSIZE with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENAMETOOLONG with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENETDOWN with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENETRESET with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENETUNREACH with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENFILE with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOBUFS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENODATA with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENODEV with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOENT with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOEXEC with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOLCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOLINK with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOMEM with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOMSG with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOPROTOOPT with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOSPC with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOSR with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOSTR with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOSYS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOTCONN with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOTDIR with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOTEMPTY with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOTSOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOTSUP with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENOTTY with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENXIO with get(): float = jsNative and set(v: float): unit = jsNative
        static member EOPNOTSUPP with get(): float = jsNative and set(v: float): unit = jsNative
        static member EOVERFLOW with get(): float = jsNative and set(v: float): unit = jsNative
        static member EPERM with get(): float = jsNative and set(v: float): unit = jsNative
        static member EPIPE with get(): float = jsNative and set(v: float): unit = jsNative
        static member EPROTO with get(): float = jsNative and set(v: float): unit = jsNative
        static member EPROTONOSUPPORT with get(): float = jsNative and set(v: float): unit = jsNative
        static member EPROTOTYPE with get(): float = jsNative and set(v: float): unit = jsNative
        static member ERANGE with get(): float = jsNative and set(v: float): unit = jsNative
        static member EROFS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ESPIPE with get(): float = jsNative and set(v: float): unit = jsNative
        static member ESRCH with get(): float = jsNative and set(v: float): unit = jsNative
        static member ETIME with get(): float = jsNative and set(v: float): unit = jsNative
        static member ETIMEDOUT with get(): float = jsNative and set(v: float): unit = jsNative
        static member ETXTBSY with get(): float = jsNative and set(v: float): unit = jsNative
        static member EWOULDBLOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member EXDEV with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEINTR with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEBADF with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEACCES with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEFAULT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEINVAL with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEMFILE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEWOULDBLOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEINPROGRESS with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEALREADY with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENOTSOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEDESTADDRREQ with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEMSGSIZE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEPROTOTYPE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENOPROTOOPT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEPROTONOSUPPORT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAESOCKTNOSUPPORT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEOPNOTSUPP with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEPFNOSUPPORT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEAFNOSUPPORT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEADDRINUSE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEADDRNOTAVAIL with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENETDOWN with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENETUNREACH with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENETRESET with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAECONNABORTED with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAECONNRESET with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENOBUFS with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEISCONN with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENOTCONN with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAESHUTDOWN with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAETOOMANYREFS with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAETIMEDOUT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAECONNREFUSED with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAELOOP with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENAMETOOLONG with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEHOSTDOWN with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEHOSTUNREACH with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENOTEMPTY with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEPROCLIM with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEUSERS with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEDQUOT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAESTALE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEREMOTE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSASYSNOTREADY with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAVERNOTSUPPORTED with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSANOTINITIALISED with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEDISCON with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAENOMORE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAECANCELLED with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEINVALIDPROCTABLE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEINVALIDPROVIDER with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEPROVIDERFAILEDINIT with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSASYSCALLFAILURE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSASERVICE_NOT_FOUND with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSATYPE_NOT_FOUND with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSA_E_NO_MORE with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSA_E_CANCELLED with get(): float = jsNative and set(v: float): unit = jsNative
        static member WSAEREFUSED with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGHUP with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGINT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGILL with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGABRT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGFPE with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGKILL with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGSEGV with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGTERM with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGBREAK with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGWINCH with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_ALL with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_CIPHER_SERVER_PREFERENCE with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_CISCO_ANYCONNECT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_COOKIE_EXCHANGE with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_CRYPTOPRO_TLSEXT_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_EPHEMERAL_RSA with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_LEGACY_SERVER_CONNECT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_MICROSOFT_SESS_ID_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_MSIE_SSLV2_RSA_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NETSCAPE_CA_DN_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NETSCAPE_CHALLENGE_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_QUERY_MTU with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_SSLv2 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_SSLv3 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_TICKET with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_TLSv1 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_TLSv1_1 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_NO_TLSv1_2 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_PKCS1_CHECK_1 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_PKCS1_CHECK_2 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_SINGLE_DH_USE with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_SINGLE_ECDH_USE with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_SSLEAY_080_CLIENT_DH_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_TLS_BLOCK_PADDING_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_TLS_D5_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SSL_OP_TLS_ROLLBACK_BUG with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_DSA with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_DH with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_RAND with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_ECDH with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_ECDSA with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_CIPHERS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_DIGESTS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_STORE with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_PKEY_METHS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_PKEY_ASN1_METHS with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_ALL with get(): float = jsNative and set(v: float): unit = jsNative
        static member ENGINE_METHOD_NONE with get(): float = jsNative and set(v: float): unit = jsNative
        static member DH_CHECK_P_NOT_SAFE_PRIME with get(): float = jsNative and set(v: float): unit = jsNative
        static member DH_CHECK_P_NOT_PRIME with get(): float = jsNative and set(v: float): unit = jsNative
        static member DH_UNABLE_TO_CHECK_GENERATOR with get(): float = jsNative and set(v: float): unit = jsNative
        static member DH_NOT_SUITABLE_GENERATOR with get(): float = jsNative and set(v: float): unit = jsNative
        static member NPN_ENABLED with get(): float = jsNative and set(v: float): unit = jsNative
        static member RSA_PKCS1_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member RSA_SSLV23_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member RSA_NO_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member RSA_PKCS1_OAEP_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member RSA_X931_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member RSA_PKCS1_PSS_PADDING with get(): float = jsNative and set(v: float): unit = jsNative
        static member POINT_CONVERSION_COMPRESSED with get(): float = jsNative and set(v: float): unit = jsNative
        static member POINT_CONVERSION_UNCOMPRESSED with get(): float = jsNative and set(v: float): unit = jsNative
        static member POINT_CONVERSION_HYBRID with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_RDONLY with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_WRONLY with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_RDWR with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFMT with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFREG with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFDIR with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFCHR with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFBLK with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFIFO with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFSOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IRWXU with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IRUSR with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IWUSR with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IXUSR with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IRWXG with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IRGRP with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IWGRP with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IXGRP with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IRWXO with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IROTH with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IWOTH with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IXOTH with get(): float = jsNative and set(v: float): unit = jsNative
        static member S_IFLNK with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_CREAT with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_EXCL with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_NOCTTY with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_DIRECTORY with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_NOATIME with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_NOFOLLOW with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_SYNC with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_SYMLINK with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_DIRECT with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_NONBLOCK with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_TRUNC with get(): float = jsNative and set(v: float): unit = jsNative
        static member O_APPEND with get(): float = jsNative and set(v: float): unit = jsNative
        static member F_OK with get(): float = jsNative and set(v: float): unit = jsNative
        static member R_OK with get(): float = jsNative and set(v: float): unit = jsNative
        static member W_OK with get(): float = jsNative and set(v: float): unit = jsNative
        static member X_OK with get(): float = jsNative and set(v: float): unit = jsNative
        static member UV_UDP_REUSEADDR with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGQUIT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGTRAP with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGIOT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGBUS with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGUSR1 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGUSR2 with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGPIPE with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGALRM with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGCHLD with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGSTKFLT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGCONT with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGSTOP with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGTSTP with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGTTIN with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGTTOU with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGURG with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGXCPU with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGXFSZ with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGVTALRM with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGPROF with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGIO with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGPOLL with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGPWR with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGSYS with get(): float = jsNative and set(v: float): unit = jsNative
        static member SIGUNUSED with get(): float = jsNative and set(v: float): unit = jsNative
        static member defaultCoreCipherList with get(): string = jsNative and set(v: string): unit = jsNative
        static member defaultCipherList with get(): string = jsNative and set(v: string): unit = jsNative
        static member ENGINE_METHOD_RSA with get(): float = jsNative and set(v: float): unit = jsNative
        static member ALPN_ENABLED with get(): float = jsNative and set(v: float): unit = jsNative



module v8 =
    type [<AllowNullLiteral>] HeapSpaceInfo =
        abstract space_name: string with get, set
        abstract space_size: float with get, set
        abstract space_used_size: float with get, set
        abstract space_available_size: float with get, set
        abstract physical_space_size: float with get, set

    and DoesZapCodeSpaceFlag =
        | Disabled = 0
        | Enabled = 1

    and [<AllowNullLiteral>] HeapInfo =
        abstract total_heap_size: float with get, set
        abstract total_heap_size_executable: float with get, set
        abstract total_physical_size: float with get, set
        abstract total_available_size: float with get, set
        abstract used_heap_size: float with get, set
        abstract heap_size_limit: float with get, set
        abstract malloced_memory: float with get, set
        abstract peak_malloced_memory: float with get, set
        abstract does_zap_garbage: DoesZapCodeSpaceFlag with get, set

    type [<Import("*","v8")>] Globals =
        static member getHeapStatistics(): HeapInfo = jsNative
        static member getHeapSpaceStatistics(): ResizeArray<HeapSpaceInfo> = jsNative
        static member setFlagsFromString(flags: string): unit = jsNative



module timers =
    type [<Import("*","timers")>] Globals =
        static member setTimeout(callback: Func<obj, unit>, ms: float, [<ParamArray>] args: obj[]): NodeJS.Timer = jsNative
        static member clearTimeout(timeoutId: NodeJS.Timer): unit = jsNative
        static member setInterval(callback: Func<obj, unit>, ms: float, [<ParamArray>] args: obj[]): NodeJS.Timer = jsNative
        static member clearInterval(intervalId: NodeJS.Timer): unit = jsNative
        static member setImmediate(callback: Func<obj, unit>, [<ParamArray>] args: obj[]): obj = jsNative
        static member clearImmediate(immediateId: obj): unit = jsNative



module _debugger =
    type [<AllowNullLiteral>] Packet =
        abstract raw: string with get, set
        abstract headers: ResizeArray<string> with get, set
        abstract body: Message with get, set

    and [<AllowNullLiteral>] Message =
        abstract seq: float with get, set
        abstract ``type``: string with get, set

    and [<AllowNullLiteral>] RequestInfo =
        abstract command: string with get, set
        abstract arguments: obj with get, set

    and [<AllowNullLiteral>] Request =
        inherit Message
        inherit RequestInfo


    and [<AllowNullLiteral>] Event =
        inherit Message
        abstract ``event``: string with get, set
        abstract body: obj option with get, set

    and [<AllowNullLiteral>] Response =
        inherit Message
        abstract request_seq: float with get, set
        abstract success: bool with get, set
        abstract message: string option with get, set
        abstract body: obj option with get, set

    and [<AllowNullLiteral>] BreakpointMessageBody =
        abstract ``type``: string with get, set
        abstract target: float with get, set
        abstract line: float with get, set

    and [<AllowNullLiteral>] [<Import("Protocol","_debugger")>] Protocol() =
        member __.res with get(): Packet = jsNative and set(v: Packet): unit = jsNative
        member __.state with get(): string = jsNative and set(v: string): unit = jsNative
        member __.onResponse with get(): Func<Packet, unit> = jsNative and set(v: Func<Packet, unit>): unit = jsNative
        member __.execute(data: string): unit = jsNative
        member __.serialize(rq: Request): string = jsNative

    and [<AllowNullLiteral>] ScriptDesc =
        abstract name: string with get, set
        abstract id: float with get, set
        abstract isNative: bool option with get, set
        abstract handle: float option with get, set
        abstract ``type``: string with get, set
        abstract lineOffset: float option with get, set
        abstract columnOffset: float option with get, set
        abstract lineCount: float option with get, set

    and [<AllowNullLiteral>] Breakpoint =
        abstract id: float with get, set
        abstract scriptId: float with get, set
        abstract script: ScriptDesc with get, set
        abstract line: float with get, set
        abstract condition: string option with get, set
        abstract scriptReq: string option with get, set

    and [<AllowNullLiteral>] RequestHandler =
        abstract request_seq: float option with get, set
        [<Emit("$0($1...)")>] abstract Invoke: err: bool * body: Message * res: Packet -> unit

    and [<AllowNullLiteral>] ResponseBodyHandler =
        abstract request_seq: float option with get, set
        [<Emit("$0($1...)")>] abstract Invoke: err: bool * ?body: obj -> unit

    and [<AllowNullLiteral>] ExceptionInfo =
        abstract text: string with get, set

    and [<AllowNullLiteral>] BreakResponse =
        abstract script: ScriptDesc option with get, set
        abstract ``exception``: ExceptionInfo option with get, set
        abstract sourceLine: float with get, set
        abstract sourceLineText: string with get, set
        abstract sourceColumn: float with get, set

    and [<AllowNullLiteral>] ClientInstance =
        abstract addListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract on: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract once: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract removeListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract removeAllListeners: ?``event``: U2<string, Symbol> -> obj
        abstract setMaxListeners: n: float -> obj
        abstract getMaxListeners: unit -> float
        abstract listeners: ``event``: U2<string, Symbol> -> ResizeArray<Function>
        abstract emit: ``event``: U2<string, Symbol> * [<ParamArray>] args: obj[] -> bool
        abstract listenerCount: ``type``: U2<string, Symbol> -> float
        abstract prependListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract prependOnceListener: ``event``: U2<string, Symbol> * listener: Function -> obj
        abstract eventNames: unit -> ResizeArray<U2<string, Symbol>>
        abstract protocol: Protocol with get, set
        abstract scripts: ResizeArray<ScriptDesc> with get, set
        abstract handles: ResizeArray<ScriptDesc> with get, set
        abstract breakpoints: ResizeArray<Breakpoint> with get, set
        abstract currentSourceLine: float with get, set
        abstract currentSourceColumn: float with get, set
        abstract currentSourceLineText: string with get, set
        abstract currentFrame: float with get, set
        abstract currentScript: string with get, set
        abstract connect: port: float * host: string -> unit
        abstract req: req: obj * cb: RequestHandler -> unit
        abstract reqFrameEval: code: string * frame: float * cb: RequestHandler -> unit
        abstract mirrorObject: obj: obj * depth: float * cb: ResponseBodyHandler -> unit
        abstract setBreakpoint: rq: BreakpointMessageBody * cb: RequestHandler -> unit
        abstract clearBreakpoint: rq: Request * cb: RequestHandler -> unit
        abstract listbreakpoints: cb: RequestHandler -> unit
        abstract reqSource: from: float * ``to``: float * cb: RequestHandler -> unit
        abstract reqScripts: cb: obj -> unit
        abstract reqContinue: cb: RequestHandler -> unit

    and [<AllowNullLiteral>] ClientType =
        [<Emit("new $0($1...)")>] abstract Create: unit -> ClientInstance

    type [<Import("*","_debugger")>] Globals =
        static member NO_FRAME with get(): float = jsNative and set(v: float): unit = jsNative
        static member port with get(): float = jsNative and set(v: float): unit = jsNative
        static member Client with get(): ClientType = jsNative and set(v: ClientType): unit = jsNative
        static member SourceInfo(body: BreakResponse): string = jsNative