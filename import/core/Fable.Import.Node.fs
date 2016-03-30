namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

type Node = interface end
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Node =
    type Error =
        abstract stack: string option with get, set

    and NodeRequireFunction =
        [<Emit("$0($1...)")>] abstract Invoke: id: string -> obj

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
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: float -> float with get, set
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
            abstract addListener: ``event``: string * listener: Function -> EventEmitter
            abstract on: ``event``: string * listener: Function -> EventEmitter
            abstract once: ``event``: string * listener: Function -> EventEmitter
            abstract removeListener: ``event``: string * listener: Function -> EventEmitter
            abstract removeAllListeners: ?``event``: string -> EventEmitter
            abstract setMaxListeners: n: float -> unit
            abstract listeners: ``event``: string -> ResizeArray<Function>
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
            abstract unshift: chunk: string -> unit
            abstract unshift: chunk: Buffer -> unit
            abstract wrap: oldStream: ReadableStream -> ReadableStream

        and WritableStream =
            inherit EventEmitter
            abstract writable: bool with get, set
            abstract write: buffer: U2<Buffer, string> * ?cb: Function -> bool
            abstract write: str: string * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: buffer: Buffer * ?cb: Function -> unit
            abstract ``end``: str: string * ?cb: Function -> unit
            abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit

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
            abstract nextTick: callback: Function -> unit
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
            abstract gc: Func<unit> with get, set

        and Timer =
            abstract ref: unit -> unit
            abstract unref: unit -> unit
            
    open NodeJS

    let [<Global>] ``process``: NodeJS.Process = failwith "JS only"
    let [<Global>] ``global``: NodeJS.Global = failwith "JS only"
    let [<Global>] __filename: string = failwith "JS only"
    let [<Global>] __dirname: string = failwith "JS only"
    let [<Global>] require: NodeRequire = failwith "JS only"
    let [<Global>] ``module``: NodeModule = failwith "JS only"
    let [<Global>] exports: obj = failwith "JS only"
    let [<Global>] SlowBuffer: obj = failwith "JS only"
    let [<Global>] Buffer: obj = failwith "JS only"


    type net = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module net =
        type Socket =
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
            abstract write: buffer: Buffer -> bool
            abstract write: buffer: Buffer * ?cb: Function -> bool
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
            abstract ``end``: buffer: Buffer * ?cb: Function -> unit
            abstract ``end``: str: string * ?cb: Function -> unit
            abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit
            abstract ``end``: ?data: obj * ?encoding: string -> unit

        and Server =
            inherit Socket
            abstract maxConnections: float with get, set
            abstract connections: float with get, set
            abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: Function -> Server
            abstract listen: path: string * ?listeningListener: Function -> Server
            abstract listen: handle: obj * ?listeningListener: Function -> Server
            abstract close: ?callback: Function -> Server
            abstract address: unit -> obj


    type crypto = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
            [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> Buffer
            abstract digest: encoding: string -> obj
            abstract digest: unit -> Buffer

        and Hmac =
            abstract update: data: obj * ?input_encoding: string -> Hmac
            [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> Buffer
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


    module events =
        type [<Import("events", "EventEmitter")>] EventEmitter() =
            interface NodeJS.EventEmitter with
                member __.addListener(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
                member __.on(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
                member __.once(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
                member __.removeListener(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
                member __.removeAllListeners(?``event``: string): NodeJS.EventEmitter = failwith "JS only"
                member __.setMaxListeners(n: float): unit = failwith "JS only"
                member __.listeners(``event``: string): ResizeArray<Function> = failwith "JS only"
                member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = failwith "JS only"
            member __.listenerCount(emitter: EventEmitter, ``event``: string): float = failwith "JS only"


    module stream =
        type Stream =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract pipe: destination: 'T * ?options: obj -> 'T

        and ReadableOptions =
            abstract highWaterMark: float option with get, set
            abstract encoding: string option with get, set
            abstract objectMode: bool option with get, set

        and [<Import("*",from="Readable")>] Readable(?opts: ReadableOptions) =
            inherit events.EventEmitter()
            interface ReadableStream with
                member __.readable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
                member __.read(?size: float): U2<string, Buffer> = failwith "JS only"
                member __.setEncoding(encoding: string): unit = failwith "JS only"
                member __.pause(): unit = failwith "JS only"
                member __.resume(): unit = failwith "JS only"
                member __.pipe(destination: 'T, ?options: obj): 'T = failwith "JS only"
                member __.unpipe(?destination: 'T): unit = failwith "JS only"
                member __.unshift(chunk: string): unit = failwith "JS only"
                member __.unshift(chunk: Buffer): unit = failwith "JS only"
                member __.wrap(oldStream: ReadableStream): ReadableStream = failwith "JS only"
            member __._read(size: float): unit = failwith "JS only"
            member __.unshift(chunk: obj): unit = failwith "JS only"
            member __.wrap(oldStream: NodeJS.ReadableStream): NodeJS.ReadableStream = failwith "JS only"
            member __.push(chunk: obj, ?encoding: string): bool = failwith "JS only"

        and WritableOptions =
            abstract highWaterMark: float option with get, set
            abstract decodeStrings: bool option with get, set
            abstract objectMode: bool option with get, set

        and [<Import("Writable","stream")>] Writable(?opts: WritableOptions) =
            inherit events.EventEmitter()
            interface WritableStream with
                member __.writable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
                member __.write(buffer: U2<Buffer, string>, ?cb: Function): bool = failwith "JS only"
                member __.write(str: string, ?encoding: string, ?cb: Function): bool = failwith "JS only"
                member __.``end``(): unit = failwith "JS only"
                member __.``end``(buffer: Buffer, ?cb: Function): unit = failwith "JS only"
                member __.``end``(str: string, ?cb: Function): unit = failwith "JS only"
                member __.``end``(str: string, ?encoding: string, ?cb: Function): unit = failwith "JS only"
            member __._write(chunk: obj, encoding: string, callback: Function): unit = failwith "JS only"
            member __.write(chunk: obj, ?cb: Function): bool = failwith "JS only"
            member __.write(chunk: obj, ?encoding: string, ?cb: Function): bool = failwith "JS only"
            member __.``end``(chunk: obj, ?cb: Function): unit = failwith "JS only"
            member __.``end``(chunk: obj, ?encoding: string, ?cb: Function): unit = failwith "JS only"

        and DuplexOptions =
            inherit ReadableOptions
            inherit WritableOptions
            abstract allowHalfOpen: bool option with get, set

        and [<Import("Duplex","stream")>] Duplex(?opts: DuplexOptions) =
            inherit Readable()
            // interface NodeJS.ReadWriteStream
            member __.writable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __._write(chunk: obj, encoding: string, callback: Function): unit = failwith "JS only"
            member __.write(chunk: obj, ?cb: Function): bool = failwith "JS only"
            member __.write(chunk: obj, ?encoding: string, ?cb: Function): bool = failwith "JS only"
            member __.``end``(): unit = failwith "JS only"
            member __.``end``(chunk: obj, ?cb: Function): unit = failwith "JS only"
            member __.``end``(chunk: obj, ?encoding: string, ?cb: Function): unit = failwith "JS only"

        and TransformOptions =
            inherit ReadableOptions
            inherit WritableOptions

        and [<Import("Transform","stream")>] Transform(?opts: TransformOptions) =
            inherit events.EventEmitter()
            // interface NodeJS.ReadWriteStream
            member __.readable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.writable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __._transform(chunk: obj, encoding: string, callback: Function): unit = failwith "JS only"
            member __._flush(callback: Function): unit = failwith "JS only"
            member __.read(?size: float): obj = failwith "JS only"
            member __.setEncoding(encoding: string): unit = failwith "JS only"
            member __.pause(): unit = failwith "JS only"
            member __.resume(): unit = failwith "JS only"
            member __.pipe(destination: 'T, ?options: obj): 'T = failwith "JS only"
            member __.unpipe(?destination: 'T): unit = failwith "JS only"
            member __.unshift(chunk: obj): unit = failwith "JS only"
            member __.wrap(oldStream: NodeJS.ReadableStream): NodeJS.ReadableStream = failwith "JS only"
            member __.push(chunk: obj, ?encoding: string): bool = failwith "JS only"
            member __.write(chunk: obj, ?cb: Function): bool = failwith "JS only"
            member __.write(chunk: obj, ?encoding: string, ?cb: Function): bool = failwith "JS only"
            member __.``end``(): unit = failwith "JS only"
            member __.``end``(chunk: obj, ?cb: Function): unit = failwith "JS only"
            member __.``end``(chunk: obj, ?encoding: string, ?cb: Function): unit = failwith "JS only"

        and [<Import("PassThrough","stream")>] PassThrough() =
            inherit Transform()


    type tls = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
            abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: Function -> Server
            abstract listen: path: string * ?listeningListener: Function -> Server
            abstract listen: handle: obj * ?listeningListener: Function -> Server
            abstract listen: port: float * ?host: string * ?callback: Function -> Server
            abstract close: unit -> Server
            abstract address: unit -> obj
            abstract addContext: hostName: string * credentials: obj -> unit

        and ClearTextStream =
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


    type child_process = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module child_process =
        type ChildProcess =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract stdin: stream.Writable with get, set
            abstract stdout: stream.Readable with get, set
            abstract stderr: stream.Readable with get, set
            abstract pid: float with get, set
            abstract kill: ?signal: string -> unit
            abstract send: message: obj * ?sendHandle: obj -> unit
            abstract disconnect: unit -> unit
            abstract unref: unit -> unit


    [<Import("*","buffer")>]
    module buffer =
        let INSPECT_MAX_BYTES: float = failwith "JS only"


    [<Import("*","querystring")>] 
    type querystring =
        static member stringify(obj: obj, ?sep: string, ?eq: string): string = failwith "JS only"
        static member parse(str: string, ?sep: string, ?eq: string, ?options: obj): obj = failwith "JS only"
        static member escape(str: string): string = failwith "JS only"
        static member unescape(str: string): string = failwith "JS only"


    type http = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module http =
        type Server =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract maxHeadersCount: float with get, set
            abstract listen: port: float * ?hostname: string * ?backlog: float * ?callback: Function -> Server
            abstract listen: port: float * ?hostname: string * ?callback: Function -> Server
            abstract listen: path: string * ?callback: Function -> Server
            abstract listen: handle: obj * ?listeningListener: Function -> Server
            abstract close: ?cb: obj -> Server
            abstract address: unit -> obj

        and ServerRequest =
            inherit IncomingMessage
            abstract connection: net.Socket with get, set

        and ServerResponse =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract writable: bool with get, set
            abstract _write: chunk: obj * encoding: string * callback: Function -> unit
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit
            abstract statusCode: float with get, set
            abstract statusMessage: string with get, set
            abstract sendDate: bool with get, set
            abstract write: buffer: Buffer -> bool
            abstract write: buffer: Buffer * ?cb: Function -> bool
            abstract write: str: string * ?cb: Function -> bool
            abstract write: str: string * ?encoding: string * ?cb: Function -> bool
            abstract write: str: string * ?encoding: string * ?fd: string -> bool
            abstract writeContinue: unit -> unit
            abstract writeHead: statusCode: float * ?reasonPhrase: string * ?headers: obj -> unit
            abstract writeHead: statusCode: float * ?headers: obj -> unit
            abstract setHeader: name: string * value: string -> unit
            abstract getHeader: name: string -> string
            abstract removeHeader: name: string -> unit
            abstract write: chunk: obj * ?encoding: string -> obj
            abstract addTrailers: headers: obj -> unit
            abstract ``end``: buffer: Buffer * ?cb: Function -> unit
            abstract ``end``: str: string * ?cb: Function -> unit
            abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit
            abstract ``end``: ?data: obj * ?encoding: string -> unit

        and ClientRequest =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract writable: bool with get, set
            abstract _write: chunk: obj * encoding: string * callback: Function -> unit
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit
            abstract write: buffer: Buffer -> bool
            abstract write: buffer: Buffer * ?cb: Function -> bool
            abstract write: str: string * ?cb: Function -> bool
            abstract write: str: string * ?encoding: string * ?cb: Function -> bool
            abstract write: str: string * ?encoding: string * ?fd: string -> bool
            abstract write: chunk: obj * ?encoding: string -> unit
            abstract abort: unit -> unit
            abstract setTimeout: timeout: float * ?callback: Function -> unit
            abstract setNoDelay: ?noDelay: bool -> unit
            abstract setSocketKeepAlive: ?enable: bool * ?initialDelay: float -> unit
            abstract ``end``: buffer: Buffer * ?cb: Function -> unit
            abstract ``end``: str: string * ?cb: Function -> unit
            abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit
            abstract ``end``: ?data: obj * ?encoding: string -> unit

        and IncomingMessage =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract readable: bool with get, set
            abstract _read: size: float -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
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
            abstract setTimeout: msecs: float * callback: Function -> NodeJS.Timer

        and ClientResponse =
            inherit IncomingMessage

        and AgentOptions =
            abstract keepAlive: bool option with get, set
            abstract keepAliveMsecs: float option with get, set
            abstract maxSockets: float option with get, set
            abstract maxFreeSockets: float option with get, set

        and [<Import("Agent","http")>] Agent(?opts: AgentOptions) =
            member __.maxSockets with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.sockets with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.requests with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.destroy(): unit = failwith "JS only"


    type cluster = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module cluster =
        type ClusterSettings =
            abstract exec: string option with get, set
            abstract args: ResizeArray<string> option with get, set
            abstract silent: bool option with get, set

        and [<Import("Worker","cluster")>] Worker() =
            inherit events.EventEmitter()
            member __.id with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.``process`` with get(): child_process.ChildProcess = failwith "JS only" and set(v: child_process.ChildProcess): unit = failwith "JS only"
            member __.suicide with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.send(message: obj, ?sendHandle: obj): unit = failwith "JS only"
            member __.kill(?signal: string): unit = failwith "JS only"
            member __.destroy(?signal: string): unit = failwith "JS only"
            member __.disconnect(): unit = failwith "JS only"


    type zlib = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module zlib =
        type ZlibOptions =
            abstract chunkSize: float option with get, set
            abstract windowBits: float option with get, set
            abstract level: float option with get, set
            abstract memLevel: float option with get, set
            abstract strategy: float option with get, set
            abstract dictionary: obj option with get, set

        and Gzip =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit

        and Gunzip =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit

        and Deflate =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit

        and Inflate =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit

        and DeflateRaw =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit

        and InflateRaw =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit

        and Unzip =
            abstract readable: bool with get, set
            abstract writable: bool with get, set
            abstract _transform: chunk: obj * encoding: string * callback: Function -> unit
            abstract _flush: callback: Function -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit


    type [<Import("*","os")>] os =
        static member EOL with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member tmpdir(): string = failwith "JS only"
        static member hostname(): string = failwith "JS only"
        static member ``type``(): string = failwith "JS only"
        static member platform(): string = failwith "JS only"
        static member arch(): string = failwith "JS only"
        static member release(): string = failwith "JS only"
        static member uptime(): float = failwith "JS only"
        static member loadavg(): ResizeArray<float> = failwith "JS only"
        static member totalmem(): float = failwith "JS only"
        static member freemem(): float = failwith "JS only"
        static member cpus(): ResizeArray<obj> = failwith "JS only"
        static member networkInterfaces(): obj = failwith "JS only"


    type https = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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


    [<Import("*","punycode")>]
    module punycode =
        type ucs2 =
            abstract decode: string: string -> string
            abstract encode: codePoints: ResizeArray<float> -> string

        let ucs2: ucs2 = failwith "JS only"
        let version: obj = failwith "JS only"
        let decode(string: string): string = failwith "JS only"
        let encode(string: string): string = failwith "JS only"
        let toUnicode(domain: string): string = failwith "JS only"
        let toASCII(domain: string): string = failwith "JS only"


    [<Import("*","repl")>]
    module repl =
        type ReplOptions =
            abstract prompt: string option with get, set
            abstract input: NodeJS.ReadableStream option with get, set
            abstract output: NodeJS.WritableStream option with get, set
            abstract terminal: bool option with get, set
            abstract eval: Function option with get, set
            abstract useColors: bool option with get, set
            abstract useGlobal: bool option with get, set
            abstract ignoreUndefined: bool option with get, set
            abstract writer: Function option with get, set

        let start(options: ReplOptions): events.EventEmitter = failwith "JS only"

    [<Import("*","readline")>]
    module readline =
        type ReadLine =
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract setPrompt: prompt: string -> unit
            abstract prompt: ?preserveCursor: bool -> unit
            abstract question: query: string * callback: Function -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract close: unit -> unit
            abstract write: data: obj * ?key: obj -> unit

        and ReadLineOptions =
            abstract input: NodeJS.ReadableStream with get, set
            abstract output: NodeJS.WritableStream with get, set
            abstract completer: Function option with get, set
            abstract terminal: bool option with get, set

        let createInterface(options: ReadLineOptions): ReadLine = failwith "JS only"


    type vm = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module vm =
        type Context =
            interface end

        and Script =
            abstract runInThisContext: unit -> unit
            abstract runInNewContext: ?sandbox: Context -> unit


    type url = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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


    type [<Import("*","dns")>] dns =
        static member lookup(domain: string, family: float, callback: Func<Error, string, float, unit>): string = failwith "JS only"
        static member lookup(domain: string, callback: Func<Error, string, float, unit>): string = failwith "JS only"
        static member resolve(domain: string, rrtype: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolve(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolve4(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolve6(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolveMx(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolveTxt(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolveSrv(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolveNs(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member resolveCname(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        static member reverse(ip: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"


    type dgram = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract send: buf: Buffer * offset: float * length: float * port: float * address: string * ?callback: Func<Error, float, unit> -> unit
            abstract bind: port: float * ?address: string * ?callback: Func<unit> -> unit
            abstract close: unit -> unit
            abstract address: unit -> AddressInfo
            abstract setBroadcast: flag: bool -> unit
            abstract setMulticastTTL: ttl: float -> unit
            abstract setMulticastLoopback: flag: bool -> unit
            abstract addMembership: multicastAddress: string * ?multicastInterface: string -> unit
            abstract dropMembership: multicastAddress: string * ?multicastInterface: string -> unit


    type fs = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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
            abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
            abstract close: unit -> unit

        and ReadStream =
            abstract readable: bool with get, set
            abstract _read: size: float -> unit
            abstract read: ?size: float -> obj
            abstract setEncoding: encoding: string -> unit
            abstract pause: unit -> unit
            abstract resume: unit -> unit
            abstract pipe: destination: 'T * ?options: obj -> 'T
            abstract unpipe: ?destination: 'T -> unit
            abstract unshift: chunk: obj -> unit
            abstract wrap: oldStream: NodeJS.ReadableStream -> NodeJS.ReadableStream
            abstract push: chunk: obj * ?encoding: string -> bool
            abstract close: unit -> unit

        and WriteStream =
            abstract writable: bool with get, set
            abstract _write: chunk: obj * encoding: string * callback: Function -> unit
            abstract write: chunk: obj * ?cb: Function -> bool
            abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
            abstract ``end``: unit -> unit
            abstract ``end``: chunk: obj * ?cb: Function -> unit
            abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit
            abstract bytesWritten: float with get, set
            abstract close: unit -> unit


    type path = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module path =
        type ParsedPath =
            abstract root: string with get, set
            abstract dir: string with get, set
            abstract ``base``: string with get, set
            abstract ext: string with get, set
            abstract name: string with get, set

        type [<Import("posix","path")>] posix =
            static member sep with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member delimiter with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member normalize(p: string): string = failwith "JS only"
            static member join([<ParamArray>] paths: obj[]): string = failwith "JS only"
            static member resolve([<ParamArray>] pathSegments: obj[]): string = failwith "JS only"
            static member isAbsolute(p: string): bool = failwith "JS only"
            static member relative(from: string, ``to``: string): string = failwith "JS only"
            static member dirname(p: string): string = failwith "JS only"
            static member basename(p: string, ?ext: string): string = failwith "JS only"
            static member extname(p: string): string = failwith "JS only"
            static member parse(p: string): ParsedPath = failwith "JS only"
            static member format(pP: ParsedPath): string = failwith "JS only"

        type [<Import("win32","path")>] win32 =
            static member sep with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member delimiter with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            static member normalize(p: string): string = failwith "JS only"
            static member join([<ParamArray>] paths: obj[]): string = failwith "JS only"
            static member resolve([<ParamArray>] pathSegments: obj[]): string = failwith "JS only"
            static member isAbsolute(p: string): bool = failwith "JS only"
            static member relative(from: string, ``to``: string): string = failwith "JS only"
            static member dirname(p: string): string = failwith "JS only"
            static member basename(p: string, ?ext: string): string = failwith "JS only"
            static member extname(p: string): string = failwith "JS only"
            static member parse(p: string): ParsedPath = failwith "JS only"
            static member format(pP: ParsedPath): string = failwith "JS only"


    [<Import("*","string_decoder")>]
    module string_decoder =
        type NodeStringDecoder =
            abstract write: buffer: Buffer -> string
            abstract detectIncompleteChar: buffer: Buffer -> float

        let StringDecoder: obj = failwith "JS only"


    type util = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module util =
        type InspectOptions =
            abstract showHidden: bool option with get, set
            abstract depth: float option with get, set
            abstract colors: bool option with get, set
            abstract customInspect: bool option with get, set


    type ``assert`` = interface end
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ``assert`` =
        type ``internal`` = interface end
        [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
        module ``internal`` =
            type [<Import("internal.AssertionError","assert")>] AssertionError(?options: obj) =
                // interface Error
                member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
                member __.message with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
                member __.actual with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
                member __.expected with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
                member __.operator with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
                member __.generatedMessage with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"


    [<Import("*","tty")>] 
    module tty =
        type ReadStream =
            inherit net.Socket
            abstract isRaw: bool with get, set
            abstract setRawMode: mode: bool -> unit

        and WriteStream =
            inherit net.Socket
            abstract columns: float with get, set
            abstract rows: float with get, set

        let isatty(fd: float): bool = failwith "JS only"


    [<Import("*","domain")>]
    module domain =
        type [<Import("Domain","domain")>] Domain() =
            inherit events.EventEmitter()
            member __.run(fn: Function): unit = failwith "JS only"
            member __.add(emitter: events.EventEmitter): unit = failwith "JS only"
            member __.remove(emitter: events.EventEmitter): unit = failwith "JS only"
            member __.bind(cb: Func<Error, obj, obj>): obj = failwith "JS only"
            member __.intercept(cb: Func<obj, obj>): obj = failwith "JS only"
            member __.dispose(): unit = failwith "JS only"
            member __.addListener(``event``: string, listener: Function): Domain = failwith "JS only"
            member __.on(``event``: string, listener: Function): Domain = failwith "JS only"
            member __.once(``event``: string, listener: Function): Domain = failwith "JS only"
            member __.removeListener(``event``: string, listener: Function): Domain = failwith "JS only"
            member __.removeAllListeners(?``event``: string): Domain = failwith "JS only"

        let create(): Domain = failwith "JS only"


    type [<Import("*","constants")>] constants =
        static member E2BIG with get(): float = failwith "JS only"
        static member EACCES with get(): float = failwith "JS only"
        static member EADDRINUSE with get(): float = failwith "JS only"
        static member EADDRNOTAVAIL with get(): float = failwith "JS only"
        static member EAFNOSUPPORT with get(): float = failwith "JS only"
        static member EAGAIN with get(): float = failwith "JS only"
        static member EALREADY with get(): float = failwith "JS only"
        static member EBADF with get(): float = failwith "JS only"
        static member EBADMSG with get(): float = failwith "JS only"
        static member EBUSY with get(): float = failwith "JS only"
        static member ECANCELED with get(): float = failwith "JS only"
        static member ECHILD with get(): float = failwith "JS only"
        static member ECONNABORTED with get(): float = failwith "JS only"
        static member ECONNREFUSED with get(): float = failwith "JS only"
        static member ECONNRESET with get(): float = failwith "JS only"
        static member EDEADLK with get(): float = failwith "JS only"
        static member EDESTADDRREQ with get(): float = failwith "JS only"
        static member EDOM with get(): float = failwith "JS only"
        static member EEXIST with get(): float = failwith "JS only"
        static member EFAULT with get(): float = failwith "JS only"
        static member EFBIG with get(): float = failwith "JS only"
        static member EHOSTUNREACH with get(): float = failwith "JS only"
        static member EIDRM with get(): float = failwith "JS only"
        static member EILSEQ with get(): float = failwith "JS only"
        static member EINPROGRESS with get(): float = failwith "JS only"
        static member EINTR with get(): float = failwith "JS only"
        static member EINVAL with get(): float = failwith "JS only"
        static member EIO with get(): float = failwith "JS only"
        static member EISCONN with get(): float = failwith "JS only"
        static member EISDIR with get(): float = failwith "JS only"
        static member ELOOP with get(): float = failwith "JS only"
        static member EMFILE with get(): float = failwith "JS only"
        static member EMLINK with get(): float = failwith "JS only"
        static member EMSGSIZE with get(): float = failwith "JS only"
        static member ENAMETOOLONG with get(): float = failwith "JS only"
        static member ENETDOWN with get(): float = failwith "JS only"
        static member ENETRESET with get(): float = failwith "JS only"
        static member ENETUNREACH with get(): float = failwith "JS only"
        static member ENFILE with get(): float = failwith "JS only"
        static member ENOBUFS with get(): float = failwith "JS only"
        static member ENODATA with get(): float = failwith "JS only"
        static member ENODEV with get(): float = failwith "JS only"
        static member ENOENT with get(): float = failwith "JS only"
        static member ENOEXEC with get(): float = failwith "JS only"
        static member ENOLCK with get(): float = failwith "JS only"
        static member ENOLINK with get(): float = failwith "JS only"
        static member ENOMEM with get(): float = failwith "JS only"
        static member ENOMSG with get(): float = failwith "JS only"
        static member ENOPROTOOPT with get(): float = failwith "JS only"
        static member ENOSPC with get(): float = failwith "JS only"
        static member ENOSR with get(): float = failwith "JS only"
        static member ENOSTR with get(): float = failwith "JS only"
        static member ENOSYS with get(): float = failwith "JS only"
        static member ENOTCONN with get(): float = failwith "JS only"
        static member ENOTDIR with get(): float = failwith "JS only"
        static member ENOTEMPTY with get(): float = failwith "JS only"
        static member ENOTSOCK with get(): float = failwith "JS only"
        static member ENOTSUP with get(): float = failwith "JS only"
        static member ENOTTY with get(): float = failwith "JS only"
        static member ENXIO with get(): float = failwith "JS only"
        static member EOPNOTSUPP with get(): float = failwith "JS only"
        static member EOVERFLOW with get(): float = failwith "JS only"
        static member EPERM with get(): float = failwith "JS only"
        static member EPIPE with get(): float = failwith "JS only"
        static member EPROTO with get(): float = failwith "JS only"
        static member EPROTONOSUPPORT with get(): float = failwith "JS only"
        static member EPROTOTYPE with get(): float = failwith "JS only"
        static member ERANGE with get(): float = failwith "JS only"
        static member EROFS with get(): float = failwith "JS only"
        static member ESPIPE with get(): float = failwith "JS only"
        static member ESRCH with get(): float = failwith "JS only"
        static member ETIME with get(): float = failwith "JS only"
        static member ETIMEDOUT with get(): float = failwith "JS only"
        static member ETXTBSY with get(): float = failwith "JS only"
        static member EWOULDBLOCK with get(): float = failwith "JS only"
        static member EXDEV with get(): float = failwith "JS only"
        static member WSAEINTR with get(): float = failwith "JS only"
        static member WSAEBADF with get(): float = failwith "JS only"
        static member WSAEACCES with get(): float = failwith "JS only"
        static member WSAEFAULT with get(): float = failwith "JS only"
        static member WSAEINVAL with get(): float = failwith "JS only"
        static member WSAEMFILE with get(): float = failwith "JS only"
        static member WSAEWOULDBLOCK with get(): float = failwith "JS only"
        static member WSAEINPROGRESS with get(): float = failwith "JS only"
        static member WSAEALREADY with get(): float = failwith "JS only"
        static member WSAENOTSOCK with get(): float = failwith "JS only"
        static member WSAEDESTADDRREQ with get(): float = failwith "JS only"
        static member WSAEMSGSIZE with get(): float = failwith "JS only"
        static member WSAEPROTOTYPE with get(): float = failwith "JS only"
        static member WSAENOPROTOOPT with get(): float = failwith "JS only"
        static member WSAEPROTONOSUPPORT with get(): float = failwith "JS only"
        static member WSAESOCKTNOSUPPORT with get(): float = failwith "JS only"
        static member WSAEOPNOTSUPP with get(): float = failwith "JS only"
        static member WSAEPFNOSUPPORT with get(): float = failwith "JS only"
        static member WSAEAFNOSUPPORT with get(): float = failwith "JS only"
        static member WSAEADDRINUSE with get(): float = failwith "JS only"
        static member WSAEADDRNOTAVAIL with get(): float = failwith "JS only"
        static member WSAENETDOWN with get(): float = failwith "JS only"
        static member WSAENETUNREACH with get(): float = failwith "JS only"
        static member WSAENETRESET with get(): float = failwith "JS only"
        static member WSAECONNABORTED with get(): float = failwith "JS only"
        static member WSAECONNRESET with get(): float = failwith "JS only"
        static member WSAENOBUFS with get(): float = failwith "JS only"
        static member WSAEISCONN with get(): float = failwith "JS only"
        static member WSAENOTCONN with get(): float = failwith "JS only"
        static member WSAESHUTDOWN with get(): float = failwith "JS only"
        static member WSAETOOMANYREFS with get(): float = failwith "JS only"
        static member WSAETIMEDOUT with get(): float = failwith "JS only"
        static member WSAECONNREFUSED with get(): float = failwith "JS only"
        static member WSAELOOP with get(): float = failwith "JS only"
        static member WSAENAMETOOLONG with get(): float = failwith "JS only"
        static member WSAEHOSTDOWN with get(): float = failwith "JS only"
        static member WSAEHOSTUNREACH with get(): float = failwith "JS only"
        static member WSAENOTEMPTY with get(): float = failwith "JS only"
        static member WSAEPROCLIM with get(): float = failwith "JS only"
        static member WSAEUSERS with get(): float = failwith "JS only"
        static member WSAEDQUOT with get(): float = failwith "JS only"
        static member WSAESTALE with get(): float = failwith "JS only"
        static member WSAEREMOTE with get(): float = failwith "JS only"
        static member WSASYSNOTREADY with get(): float = failwith "JS only"
        static member WSAVERNOTSUPPORTED with get(): float = failwith "JS only"
        static member WSANOTINITIALISED with get(): float = failwith "JS only"
        static member WSAEDISCON with get(): float = failwith "JS only"
        static member WSAENOMORE with get(): float = failwith "JS only"
        static member WSAECANCELLED with get(): float = failwith "JS only"
        static member WSAEINVALIDPROCTABLE with get(): float = failwith "JS only"
        static member WSAEINVALIDPROVIDER with get(): float = failwith "JS only"
        static member WSAEPROVIDERFAILEDINIT with get(): float = failwith "JS only"
        static member WSASYSCALLFAILURE with get(): float = failwith "JS only"
        static member WSASERVICE_NOT_FOUND with get(): float = failwith "JS only"
        static member WSATYPE_NOT_FOUND with get(): float = failwith "JS only"
        static member WSA_E_NO_MORE with get(): float = failwith "JS only"
        static member WSA_E_CANCELLED with get(): float = failwith "JS only"
        static member WSAEREFUSED with get(): float = failwith "JS only"
        static member SIGHUP with get(): float = failwith "JS only"
        static member SIGINT with get(): float = failwith "JS only"
        static member SIGILL with get(): float = failwith "JS only"
        static member SIGABRT with get(): float = failwith "JS only"
        static member SIGFPE with get(): float = failwith "JS only"
        static member SIGKILL with get(): float = failwith "JS only"
        static member SIGSEGV with get(): float = failwith "JS only"
        static member SIGTERM with get(): float = failwith "JS only"
        static member SIGBREAK with get(): float = failwith "JS only"
        static member SIGWINCH with get(): float = failwith "JS only"
        static member SSL_OP_ALL with get(): float = failwith "JS only"
        static member SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION with get(): float = failwith "JS only"
        static member SSL_OP_CIPHER_SERVER_PREFERENCE with get(): float = failwith "JS only"
        static member SSL_OP_CISCO_ANYCONNECT with get(): float = failwith "JS only"
        static member SSL_OP_COOKIE_EXCHANGE with get(): float = failwith "JS only"
        static member SSL_OP_CRYPTOPRO_TLSEXT_BUG with get(): float = failwith "JS only"
        static member SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS with get(): float = failwith "JS only"
        static member SSL_OP_EPHEMERAL_RSA with get(): float = failwith "JS only"
        static member SSL_OP_LEGACY_SERVER_CONNECT with get(): float = failwith "JS only"
        static member SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER with get(): float = failwith "JS only"
        static member SSL_OP_MICROSOFT_SESS_ID_BUG with get(): float = failwith "JS only"
        static member SSL_OP_MSIE_SSLV2_RSA_PADDING with get(): float = failwith "JS only"
        static member SSL_OP_NETSCAPE_CA_DN_BUG with get(): float = failwith "JS only"
        static member SSL_OP_NETSCAPE_CHALLENGE_BUG with get(): float = failwith "JS only"
        static member SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG with get(): float = failwith "JS only"
        static member SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG with get(): float = failwith "JS only"
        static member SSL_OP_NO_COMPRESSION with get(): float = failwith "JS only"
        static member SSL_OP_NO_QUERY_MTU with get(): float = failwith "JS only"
        static member SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION with get(): float = failwith "JS only"
        static member SSL_OP_NO_SSLv2 with get(): float = failwith "JS only"
        static member SSL_OP_NO_SSLv3 with get(): float = failwith "JS only"
        static member SSL_OP_NO_TICKET with get(): float = failwith "JS only"
        static member SSL_OP_NO_TLSv1 with get(): float = failwith "JS only"
        static member SSL_OP_NO_TLSv1_1 with get(): float = failwith "JS only"
        static member SSL_OP_NO_TLSv1_2 with get(): float = failwith "JS only"
        static member SSL_OP_PKCS1_CHECK_1 with get(): float = failwith "JS only"
        static member SSL_OP_PKCS1_CHECK_2 with get(): float = failwith "JS only"
        static member SSL_OP_SINGLE_DH_USE with get(): float = failwith "JS only"
        static member SSL_OP_SINGLE_ECDH_USE with get(): float = failwith "JS only"
        static member SSL_OP_SSLEAY_080_CLIENT_DH_BUG with get(): float = failwith "JS only"
        static member SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG with get(): float = failwith "JS only"
        static member SSL_OP_TLS_BLOCK_PADDING_BUG with get(): float = failwith "JS only"
        static member SSL_OP_TLS_D5_BUG with get(): float = failwith "JS only"
        static member SSL_OP_TLS_ROLLBACK_BUG with get(): float = failwith "JS only"
        static member ENGINE_METHOD_DSA with get(): float = failwith "JS only"
        static member ENGINE_METHOD_DH with get(): float = failwith "JS only"
        static member ENGINE_METHOD_RAND with get(): float = failwith "JS only"
        static member ENGINE_METHOD_ECDH with get(): float = failwith "JS only"
        static member ENGINE_METHOD_ECDSA with get(): float = failwith "JS only"
        static member ENGINE_METHOD_CIPHERS with get(): float = failwith "JS only"
        static member ENGINE_METHOD_DIGESTS with get(): float = failwith "JS only"
        static member ENGINE_METHOD_STORE with get(): float = failwith "JS only"
        static member ENGINE_METHOD_PKEY_METHS with get(): float = failwith "JS only"
        static member ENGINE_METHOD_PKEY_ASN1_METHS with get(): float = failwith "JS only"
        static member ENGINE_METHOD_ALL with get(): float = failwith "JS only"
        static member ENGINE_METHOD_NONE with get(): float = failwith "JS only"
        static member DH_CHECK_P_NOT_SAFE_PRIME with get(): float = failwith "JS only"
        static member DH_CHECK_P_NOT_PRIME with get(): float = failwith "JS only"
        static member DH_UNABLE_TO_CHECK_GENERATOR with get(): float = failwith "JS only"
        static member DH_NOT_SUITABLE_GENERATOR with get(): float = failwith "JS only"
        static member NPN_ENABLED with get(): float = failwith "JS only"
        static member RSA_PKCS1_PADDING with get(): float = failwith "JS only"
        static member RSA_SSLV23_PADDING with get(): float = failwith "JS only"
        static member RSA_NO_PADDING with get(): float = failwith "JS only"
        static member RSA_PKCS1_OAEP_PADDING with get(): float = failwith "JS only"
        static member RSA_X931_PADDING with get(): float = failwith "JS only"
        static member RSA_PKCS1_PSS_PADDING with get(): float = failwith "JS only"
        static member POINT_CONVERSION_COMPRESSED with get(): float = failwith "JS only"
        static member POINT_CONVERSION_UNCOMPRESSED with get(): float = failwith "JS only"
        static member POINT_CONVERSION_HYBRID with get(): float = failwith "JS only"
        static member O_RDONLY with get(): float = failwith "JS only"
        static member O_WRONLY with get(): float = failwith "JS only"
        static member O_RDWR with get(): float = failwith "JS only"
        static member S_IFMT with get(): float = failwith "JS only"
        static member S_IFREG with get(): float = failwith "JS only"
        static member S_IFDIR with get(): float = failwith "JS only"
        static member S_IFCHR with get(): float = failwith "JS only"
        static member S_IFLNK with get(): float = failwith "JS only"
        static member O_CREAT with get(): float = failwith "JS only"
        static member O_EXCL with get(): float = failwith "JS only"
        static member O_TRUNC with get(): float = failwith "JS only"
        static member O_APPEND with get(): float = failwith "JS only"
        static member F_OK with get(): float = failwith "JS only"
        static member R_OK with get(): float = failwith "JS only"
        static member W_OK with get(): float = failwith "JS only"
        static member X_OK with get(): float = failwith "JS only"
        static member UV_UDP_REUSEADDR with get(): float = failwith "JS only"


[<AutoOpen>]
module Node_Extensions =
    open Node
    
    open net
    type [<Import("*","net")>] net with
        static member Socket with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member createServer(?connectionListener: Func<Socket, unit>): Server = failwith "JS only"
        static member createServer(?options: obj, ?connectionListener: Func<Socket, unit>): Server = failwith "JS only"
        static member connect(options: obj, ?connectionListener: Function): Socket = failwith "JS only"
        static member connect(port: float, ?host: string, ?connectionListener: Function): Socket = failwith "JS only"
        static member connect(path: string, ?connectionListener: Function): Socket = failwith "JS only"
        static member createConnection(options: obj, ?connectionListener: Function): Socket = failwith "JS only"
        static member createConnection(port: float, ?host: string, ?connectionListener: Function): Socket = failwith "JS only"
        static member createConnection(path: string, ?connectionListener: Function): Socket = failwith "JS only"
        static member isIP(input: string): float = failwith "JS only"
        static member isIPv4(input: string): bool = failwith "JS only"
        static member isIPv6(input: string): bool = failwith "JS only"

    open crypto
    type [<Import("*","crypto")>] crypto with
        static member createCredentials(details: CredentialDetails): Credentials = failwith "JS only"
        static member createHash(algorithm: string): Hash = failwith "JS only"
        static member createHmac(algorithm: string, key: string): Hmac = failwith "JS only"
        static member createHmac(algorithm: string, key: Buffer): Hmac = failwith "JS only"
        static member createCipher(algorithm: string, password: obj): Cipher = failwith "JS only"
        static member createCipheriv(algorithm: string, key: obj, iv: obj): Cipher = failwith "JS only"
        static member createDecipher(algorithm: string, password: obj): Decipher = failwith "JS only"
        static member createDecipheriv(algorithm: string, key: obj, iv: obj): Decipher = failwith "JS only"
        static member createSign(algorithm: string): Signer = failwith "JS only"
        static member createVerify(algorith: string): Verify = failwith "JS only"
        static member createDiffieHellman(prime_length: float): DiffieHellman = failwith "JS only"
        static member createDiffieHellman(prime: float, ?encoding: string): DiffieHellman = failwith "JS only"
        static member getDiffieHellman(group_name: string): DiffieHellman = failwith "JS only"
        static member pbkdf2(password: string, salt: string, iterations: float, keylen: float, callback: Func<Error, Buffer, obj>): unit = failwith "JS only"
        static member pbkdf2(password: string, salt: string, iterations: float, keylen: float, digest: string, callback: Func<Error, Buffer, obj>): unit = failwith "JS only"
        static member pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float): Buffer = failwith "JS only"
        static member pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float, digest: string): Buffer = failwith "JS only"
        static member randomBytes(size: float): Buffer = failwith "JS only"
        static member randomBytes(size: float, callback: Func<Error, Buffer, unit>): unit = failwith "JS only"
        static member pseudoRandomBytes(size: float): Buffer = failwith "JS only"
        static member pseudoRandomBytes(size: float, callback: Func<Error, Buffer, unit>): unit = failwith "JS only"

    open tls
    type [<Import("*","tls")>] tls with
        static member CLIENT_RENEG_LIMIT with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member CLIENT_RENEG_WINDOW with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member createServer(options: TlsOptions, ?secureConnectionListener: Func<ClearTextStream, unit>): Server = failwith "JS only"
        static member connect(options: TlsOptions, ?secureConnectionListener: Func<unit>): ClearTextStream = failwith "JS only"
        static member connect(port: float, ?host: string, ?options: ConnectionOptions, ?secureConnectListener: Func<unit>): ClearTextStream = failwith "JS only"
        static member connect(port: float, ?options: ConnectionOptions, ?secureConnectListener: Func<unit>): ClearTextStream = failwith "JS only"
        static member createSecurePair(?credentials: crypto.Credentials, ?isServer: bool, ?requestCert: bool, ?rejectUnauthorized: bool): SecurePair = failwith "JS only"
        static member createSecureContext(details: SecureContextOptions): SecureContext = failwith "JS only"

    open child_process
    type [<Import("*","child_process")>] child_process with
        static member spawn(command: string, ?args: ResizeArray<string>, ?options: obj): ChildProcess = failwith "JS only"
        static member exec(command: string, options: obj, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        static member exec(command: string, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        static member execFile(file: string, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        static member execFile(file: string, ?args: ResizeArray<string>, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        static member execFile(file: string, ?args: ResizeArray<string>, ?options: obj, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        static member fork(modulePath: string, ?args: ResizeArray<string>, ?options: obj): ChildProcess = failwith "JS only"
        static member spawnSync(command: string, ?args: ResizeArray<string>, ?options: obj): obj = failwith "JS only"
        static member execSync(command: string, ?options: obj): U2<string, Buffer> = failwith "JS only"
        static member execFileSync(command: string, ?args: ResizeArray<string>, ?options: obj): U2<string, Buffer> = failwith "JS only"

    open http
    type [<Import("*","http")>] http with
        static member METHODS with get(): ResizeArray<string> = failwith "JS only" and set(v: ResizeArray<string>): unit = failwith "JS only"
        static member STATUS_CODES with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member globalAgent with get(): Agent = failwith "JS only" and set(v: Agent): unit = failwith "JS only"
        static member createServer(?requestListener: Func<IncomingMessage, ServerResponse, unit>): Server = failwith "JS only"
        static member createClient(?port: float, ?host: string): obj = failwith "JS only"
        static member request(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = failwith "JS only"
        static member get(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = failwith "JS only"

    open cluster
    type [<Import("*","cluster")>] cluster with
        static member settings with get(): ClusterSettings = failwith "JS only" and set(v: ClusterSettings): unit = failwith "JS only"
        static member isMaster with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        static member isWorker with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        static member worker with get(): Worker = failwith "JS only" and set(v: Worker): unit = failwith "JS only"
        static member workers with get(): ResizeArray<Worker> = failwith "JS only" and set(v: ResizeArray<Worker>): unit = failwith "JS only"
        static member setupMaster(?settings: ClusterSettings): unit = failwith "JS only"
        static member fork(?env: obj): Worker = failwith "JS only"
        static member disconnect(?callback: Function): unit = failwith "JS only"
        static member addListener(``event``: string, listener: Function): unit = failwith "JS only"
        static member on(``event``: string, listener: Function): obj = failwith "JS only"
        static member once(``event``: string, listener: Function): unit = failwith "JS only"
        static member removeListener(``event``: string, listener: Function): unit = failwith "JS only"
        static member removeAllListeners(?``event``: string): unit = failwith "JS only"
        static member setMaxListeners(n: float): unit = failwith "JS only"
        static member listeners(``event``: string): ResizeArray<Function> = failwith "JS only"
        static member emit(``event``: string, [<ParamArray>] args: obj[]): bool = failwith "JS only"

    open zlib
    type [<Import("*","zlib")>] zlib with
        static member Z_NO_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_PARTIAL_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_SYNC_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_FULL_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_FINISH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_BLOCK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_TREES with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_STREAM_END with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_NEED_DICT with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_ERRNO with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_STREAM_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_DATA_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_MEM_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_BUF_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_VERSION_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_NO_COMPRESSION with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_BEST_SPEED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_BEST_COMPRESSION with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_DEFAULT_COMPRESSION with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_FILTERED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_HUFFMAN_ONLY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_RLE with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_FIXED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_DEFAULT_STRATEGY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_BINARY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_TEXT with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_ASCII with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_UNKNOWN with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_DEFLATED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member Z_NULL with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member createGzip(?options: ZlibOptions): Gzip = failwith "JS only"
        static member createGunzip(?options: ZlibOptions): Gunzip = failwith "JS only"
        static member createDeflate(?options: ZlibOptions): Deflate = failwith "JS only"
        static member createInflate(?options: ZlibOptions): Inflate = failwith "JS only"
        static member createDeflateRaw(?options: ZlibOptions): DeflateRaw = failwith "JS only"
        static member createInflateRaw(?options: ZlibOptions): InflateRaw = failwith "JS only"
        static member createUnzip(?options: ZlibOptions): Unzip = failwith "JS only"
        static member deflate(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member deflateSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        static member deflateRaw(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member deflateRawSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        static member gzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member gzipSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        static member gunzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member gunzipSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        static member inflate(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member inflateSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        static member inflateRaw(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member inflateRawSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        static member unzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        static member unzipSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"

    open https
    type [<Import("*","https")>] https with
        static member Agent with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member globalAgent with get(): Agent = failwith "JS only" and set(v: Agent): unit = failwith "JS only"
        static member createServer(options: ServerOptions, ?requestListener: Function): Server = failwith "JS only"
        static member request(options: RequestOptions, ?callback: Func<http.IncomingMessage, unit>): http.ClientRequest = failwith "JS only"
        static member get(options: RequestOptions, ?callback: Func<http.IncomingMessage, unit>): http.ClientRequest = failwith "JS only"

    open vm
    type [<Import("*","vm")>] vm with
        static member runInThisContext(code: string, ?filename: string): unit = failwith "JS only"
        static member runInNewContext(code: string, ?sandbox: Context, ?filename: string): unit = failwith "JS only"
        static member runInContext(code: string, context: Context, ?filename: string): unit = failwith "JS only"
        static member createContext(?initSandbox: Context): Context = failwith "JS only"
        static member createScript(code: string, ?filename: string): Script = failwith "JS only"

    open url
    type [<Import("*","url")>] url with
        static member parse(urlStr: string, ?parseQueryString: bool, ?slashesDenoteHost: bool): Url = failwith "JS only"
        static member format(url: UrlOptions): string = failwith "JS only"
        static member resolve(from: string, ``to``: string): string = failwith "JS only"

    open dgram
    type [<Import("*","dgram")>] dgram with
        static member createSocket(``type``: string, ?callback: Func<Buffer, RemoteInfo, unit>): Socket = failwith "JS only"

    open fs
    type [<Import("*","fs")>] fs =
        static member F_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member R_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member W_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member X_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        static member rename(oldPath: string, newPath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member renameSync(oldPath: string, newPath: string): unit = failwith "JS only"
        static member truncate(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member truncate(path: string, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member truncateSync(path: string, ?len: float): unit = failwith "JS only"
        static member ftruncate(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member ftruncate(fd: float, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member ftruncateSync(fd: float, ?len: float): unit = failwith "JS only"
        static member chown(path: string, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member chownSync(path: string, uid: float, gid: float): unit = failwith "JS only"
        static member fchown(fd: float, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member fchownSync(fd: float, uid: float, gid: float): unit = failwith "JS only"
        static member lchown(path: string, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member lchownSync(path: string, uid: float, gid: float): unit = failwith "JS only"
        static member chmod(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member chmod(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member chmodSync(path: string, mode: float): unit = failwith "JS only"
        static member chmodSync(path: string, mode: string): unit = failwith "JS only"
        static member fchmod(fd: float, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member fchmod(fd: float, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member fchmodSync(fd: float, mode: float): unit = failwith "JS only"
        static member fchmodSync(fd: float, mode: string): unit = failwith "JS only"
        static member lchmod(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member lchmod(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member lchmodSync(path: string, mode: float): unit = failwith "JS only"
        static member lchmodSync(path: string, mode: string): unit = failwith "JS only"
        static member stat(path: string, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = failwith "JS only"
        static member lstat(path: string, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = failwith "JS only"
        static member fstat(fd: float, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = failwith "JS only"
        static member statSync(path: string): Stats = failwith "JS only"
        static member lstatSync(path: string): Stats = failwith "JS only"
        static member fstatSync(fd: float): Stats = failwith "JS only"
        static member link(srcpath: string, dstpath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member linkSync(srcpath: string, dstpath: string): unit = failwith "JS only"
        static member symlink(srcpath: string, dstpath: string, ?``type``: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member symlinkSync(srcpath: string, dstpath: string, ?``type``: string): unit = failwith "JS only"
        static member readlink(path: string, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = failwith "JS only"
        static member readlinkSync(path: string): string = failwith "JS only"
        static member realpath(path: string, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = failwith "JS only"
        static member realpath(path: string, cache: obj, callback: Func<NodeJS.ErrnoException, string, obj>): unit = failwith "JS only"
        static member realpathSync(path: string, ?cache: obj): string = failwith "JS only"
        static member unlink(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member unlinkSync(path: string): unit = failwith "JS only"
        static member rmdir(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member rmdirSync(path: string): unit = failwith "JS only"
        static member mkdir(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member mkdir(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member mkdir(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member mkdirSync(path: string, ?mode: float): unit = failwith "JS only"
        static member mkdirSync(path: string, ?mode: string): unit = failwith "JS only"
        static member readdir(path: string, ?callback: Func<NodeJS.ErrnoException, ResizeArray<string>, unit>): unit = failwith "JS only"
        static member readdirSync(path: string): ResizeArray<string> = failwith "JS only"
        static member close(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member closeSync(fd: float): unit = failwith "JS only"
        static member ``open``(path: string, flags: string, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = failwith "JS only"
        static member ``open``(path: string, flags: string, mode: float, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = failwith "JS only"
        static member ``open``(path: string, flags: string, mode: string, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = failwith "JS only"
        static member openSync(path: string, flags: string, ?mode: float): float = failwith "JS only"
        static member openSync(path: string, flags: string, ?mode: string): float = failwith "JS only"
        static member utimes(path: string, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member utimes(path: string, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member utimesSync(path: string, atime: float, mtime: float): unit = failwith "JS only"
        static member utimesSync(path: string, atime: DateTime, mtime: DateTime): unit = failwith "JS only"
        static member futimes(fd: float, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member futimes(fd: float, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member futimesSync(fd: float, atime: float, mtime: float): unit = failwith "JS only"
        static member futimesSync(fd: float, atime: DateTime, mtime: DateTime): unit = failwith "JS only"
        static member fsync(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member fsyncSync(fd: float): unit = failwith "JS only"
        static member write(fd: float, buffer: Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = failwith "JS only"
        static member write(fd: float, buffer: Buffer, offset: float, length: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = failwith "JS only"
        static member write(fd: float, data: obj, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = failwith "JS only"
        static member write(fd: float, data: obj, offset: float, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = failwith "JS only"
        static member write(fd: float, data: obj, offset: float, encoding: string, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = failwith "JS only"
        static member writeSync(fd: float, buffer: Buffer, offset: float, length: float, position: float): float = failwith "JS only"
        static member read(fd: float, buffer: Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = failwith "JS only"
        static member readSync(fd: float, buffer: Buffer, offset: float, length: float, position: float): float = failwith "JS only"
        static member readFile(filename: string, encoding: string, callback: Func<NodeJS.ErrnoException, string, unit>): unit = failwith "JS only"
        static member readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, string, unit>): unit = failwith "JS only"
        static member readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, Buffer, unit>): unit = failwith "JS only"
        static member readFile(filename: string, callback: Func<NodeJS.ErrnoException, Buffer, unit>): unit = failwith "JS only"
        static member readFileSync(filename: string, encoding: string): string = failwith "JS only"
        static member readFileSync(filename: string, options: obj): string = failwith "JS only"
        static member readFileSync(filename: string, ?options: obj): Buffer = failwith "JS only"
        static member writeFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member writeFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member writeFileSync(filename: string, data: obj, ?options: obj): unit = failwith "JS only"
        static member appendFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member appendFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member appendFileSync(filename: string, data: obj, ?options: obj): unit = failwith "JS only"
        static member watchFile(filename: string, listener: Func<Stats, Stats, unit>): unit = failwith "JS only"
        static member watchFile(filename: string, options: obj, listener: Func<Stats, Stats, unit>): unit = failwith "JS only"
        static member unwatchFile(filename: string, ?listener: Func<Stats, Stats, unit>): unit = failwith "JS only"
        static member watch(filename: string, ?listener: Func<string, string, obj>): FSWatcher = failwith "JS only"
        static member watch(filename: string, options: obj, ?listener: Func<string, string, obj>): FSWatcher = failwith "JS only"
        static member exists(path: string, ?callback: Func<bool, unit>): unit = failwith "JS only"
        static member existsSync(path: string): bool = failwith "JS only"
        static member access(path: string, callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member access(path: string, mode: float, callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        static member accessSync(path: string, ?mode: float): unit = failwith "JS only"
        static member createReadStream(path: string, ?options: obj): ReadStream = failwith "JS only"
        static member createWriteStream(path: string, ?options: obj): WriteStream = failwith "JS only"

    open path
    type [<Import("*","path")>] path with
        static member sep with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member delimiter with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member normalize(p: string): string = failwith "JS only"
        static member join([<ParamArray>] paths: obj[]): string = failwith "JS only"
        static member join([<ParamArray>] paths: string[]): string = failwith "JS only"
        static member resolve([<ParamArray>] pathSegments: obj[]): string = failwith "JS only"
        static member isAbsolute(path: string): bool = failwith "JS only"
        static member relative(from: string, ``to``: string): string = failwith "JS only"
        static member dirname(p: string): string = failwith "JS only"
        static member basename(p: string, ?ext: string): string = failwith "JS only"
        static member extname(p: string): string = failwith "JS only"
        static member parse(pathString: string): ParsedPath = failwith "JS only"
        static member format(pathObject: ParsedPath): string = failwith "JS only"

    open util
    type [<Import("*","util")>] util with
        static member format(format: obj, [<ParamArray>] param: obj[]): string = failwith "JS only"
        static member debug(string: string): unit = failwith "JS only"
        static member error([<ParamArray>] param: obj[]): unit = failwith "JS only"
        static member puts([<ParamArray>] param: obj[]): unit = failwith "JS only"
        static member print([<ParamArray>] param: obj[]): unit = failwith "JS only"
        static member log(string: string): unit = failwith "JS only"
        static member inspect(``object``: obj, ?showHidden: bool, ?depth: float, ?color: bool): string = failwith "JS only"
        static member inspect(``object``: obj, options: InspectOptions): string = failwith "JS only"
        static member isArray(``object``: obj): bool = failwith "JS only"
        static member isRegExp(``object``: obj): bool = failwith "JS only"
        static member isDate(``object``: obj): bool = failwith "JS only"
        static member isError(``object``: obj): bool = failwith "JS only"
        static member inherits(``constructor``: obj, superConstructor: obj): unit = failwith "JS only"
        static member debuglog(key: string): Func<string, obj, unit> = failwith "JS only"

    open ``assert``
    type [<Import("*","assert")>] ``assert`` =
        static member ``internal``(value: obj, ?message: string): unit = failwith "JS only"
    
    type [<Import("internal","assert")>] ``internal`` with
        static member throws with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member doesNotThrow with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member fail(?actual: obj, ?expected: obj, ?message: string, ?operator: string): unit = failwith "JS only"
        static member ok(value: obj, ?message: string): unit = failwith "JS only"
        static member equal(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        static member notEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        static member deepEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        static member notDeepEqual(acutal: obj, expected: obj, ?message: string): unit = failwith "JS only"
        static member strictEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        static member notStrictEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        static member ifError(value: obj): unit = failwith "JS only"
