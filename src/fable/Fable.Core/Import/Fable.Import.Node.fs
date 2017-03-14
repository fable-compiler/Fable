module Fable.Import.Node
open System
open Fable.Core
open Fable.Import.JS

type [<AllowNullLiteral>] Error =
    abstract stack: string option with get, set

// and [<AllowNullLiteral>] MapConstructor =
//     interface end
// 
// and [<AllowNullLiteral>] WeakMapConstructor =
//     interface end
// 
// and [<AllowNullLiteral>] SetConstructor =
//     interface end
// 
// and [<AllowNullLiteral>] WeakSetConstructor =
//     interface end

and [<AllowNullLiteral>] Buffer =
    inherit NodeBuffer

and [<AllowNullLiteral>] NodeBuffer =
    [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
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
    type [<AllowNullLiteral>] ErrnoException =
        inherit Error
        abstract errno: float option with get, set
        abstract code: string option with get, set
        abstract path: string option with get, set
        abstract syscall: string option with get, set
        abstract stack: string option with get, set

    and [<AllowNullLiteral>] EventEmitter =
        abstract addListener: ``event``: string * listener: Function -> EventEmitter
        abstract on: ``event``: string * listener: Function -> EventEmitter
        abstract once: ``event``: string * listener: Function -> EventEmitter
        abstract removeListener: ``event``: string * listener: Function -> EventEmitter
        abstract removeAllListeners: ?``event``: string -> EventEmitter
        abstract setMaxListeners: n: int -> unit
        abstract getMaxListeners: unit -> int
        abstract listeners: ``event``: string -> ResizeArray<Function>
        abstract emit: ``event``: string * [<ParamArray>] args: obj[] -> bool
        abstract listenerCount: ``type``: string -> int

    and [<AllowNullLiteral>] ReadableStream =
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

    and [<AllowNullLiteral>] WritableStream =
        inherit EventEmitter
        abstract writable: bool with get, set
        abstract write: buffer: U2<Buffer, string> * ?cb: Function -> bool
        abstract write: str: string * ?encoding: string * ?cb: Function -> bool
        abstract ``end``: unit -> unit
        abstract ``end``: buffer: Buffer * ?cb: Function -> unit
        abstract ``end``: str: string * ?cb: Function -> unit
        abstract ``end``: str: string * ?encoding: string * ?cb: Function -> unit

    and [<AllowNullLiteral>] ReadWriteStream =
        inherit ReadableStream
        inherit WritableStream

    and [<AllowNullLiteral>] Process =
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

    and [<AllowNullLiteral>] Timer =
        abstract ref: unit -> unit
        abstract unref: unit -> unit
        
open NodeJS


module net_types =
    type [<AllowNullLiteral>] Socket =
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
        
let [<Import("*","net")>] net: net_types.Globals = jsNative


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
        [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> Buffer
        abstract digest: encoding: string -> obj
        abstract digest: unit -> Buffer

    and [<AllowNullLiteral>] Hmac =
        abstract update: data: obj * ?input_encoding: string -> Hmac
        [<Emit("$0.digest('buffer')")>] abstract digest_buffer: unit -> Buffer
        abstract digest: encoding: string -> obj
        abstract digest: unit -> Buffer

    and [<AllowNullLiteral>] Cipher =
        abstract update: data: Buffer -> Buffer
        abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract final: unit -> Buffer
        abstract final: output_encoding: string -> string
        abstract setAutoPadding: auto_padding: bool -> unit

    and [<AllowNullLiteral>] Decipher =
        abstract update: data: Buffer -> Buffer
        abstract update: data: string * ?input_encoding: string * ?output_encoding: string -> string
        abstract final: unit -> Buffer
        abstract final: output_encoding: string -> string
        abstract setAutoPadding: auto_padding: bool -> unit

    and [<AllowNullLiteral>] Signer =
        inherit NodeJS.WritableStream
        abstract update: data: obj -> unit
        abstract sign: private_key: string * output_format: string -> string

    and [<AllowNullLiteral>] Verify =
        inherit NodeJS.WritableStream
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
        member __.createHmac(algorithm: string, key: Buffer): Hmac = jsNative
        member __.createCipher(algorithm: string, password: obj): Cipher = jsNative
        member __.createCipheriv(algorithm: string, key: obj, iv: obj): Cipher = jsNative
        member __.createDecipher(algorithm: string, password: obj): Decipher = jsNative
        member __.createDecipheriv(algorithm: string, key: obj, iv: obj): Decipher = jsNative
        member __.createSign(algorithm: string): Signer = jsNative
        member __.createVerify(algorith: string): Verify = jsNative
        member __.createDiffieHellman(prime_length: float): DiffieHellman = jsNative
        member __.createDiffieHellman(prime: float, ?encoding: string): DiffieHellman = jsNative
        member __.getDiffieHellman(group_name: string): DiffieHellman = jsNative
        member __.pbkdf2(password: string, salt: string, iterations: float, keylen: float, callback: Func<Error, Buffer, obj>): unit = jsNative
        member __.pbkdf2(password: string, salt: string, iterations: float, keylen: float, digest: string, callback: Func<Error, Buffer, obj>): unit = jsNative
        member __.pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float): Buffer = jsNative
        member __.pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float, digest: string): Buffer = jsNative
        member __.randomBytes(size: float): Buffer = jsNative
        member __.randomBytes(size: float, callback: Func<Error, Buffer, unit>): unit = jsNative
        member __.pseudoRandomBytes(size: float): Buffer = jsNative
        member __.pseudoRandomBytes(size: float, callback: Func<Error, Buffer, unit>): unit = jsNative

let [<Import("*","crypto")>] crypto: crypto_types.Globals = jsNative


module events =
    type [<AllowNullLiteral>] [<Import("EventEmitter","events")>] EventEmitter() =
        interface NodeJS.EventEmitter with
            member __.addListener(``event``: string, listener: Function): NodeJS.EventEmitter = jsNative
            member __.on(``event``: string, listener: Function): NodeJS.EventEmitter = jsNative
            member __.once(``event``: string, listener: Function): NodeJS.EventEmitter = jsNative
            member __.removeListener(``event``: string, listener: Function): NodeJS.EventEmitter = jsNative
            member __.removeAllListeners(?``event``: string): NodeJS.EventEmitter = jsNative
            member __.setMaxListeners(n: int): unit = jsNative
            member __.getMaxListeners(): int = jsNative
            member __.listeners(``event``: string): ResizeArray<Function> = jsNative
            member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = jsNative
            member __.listenerCount(``type``: string): int = jsNative
        member __.listenerCount(emitter: EventEmitter, ``event``: string): float = jsNative
        static member EventEmitter with get(): EventEmitter = jsNative
        static member defaultMaxListeners with get(): int = jsNative


module stream =
    type [<AllowNullLiteral>] Stream =
        abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
        abstract pipe: destination: 'T * ?options: obj -> 'T

    and [<AllowNullLiteral>] ReadableOptions =
        abstract highWaterMark: float option with get, set
        abstract encoding: string option with get, set
        abstract objectMode: bool option with get, set

    and [<AllowNullLiteral>] [<Import("Readable","stream")>] Readable(?opts: ReadableOptions) =
        inherit events.EventEmitter()
        interface ReadableStream with
            member __.readable with get(): bool = jsNative and set(v: bool): unit = jsNative
            member __.read(?size: float): U2<string, Buffer> = jsNative
            member __.setEncoding(encoding: string): unit = jsNative
            member __.pause(): unit = jsNative
            member __.resume(): unit = jsNative
            member __.pipe(destination: 'T, ?options: obj): 'T = jsNative
            member __.unpipe(?destination: 'T): unit = jsNative
            member __.unshift(chunk: string): unit = jsNative
            member __.unshift(chunk: Buffer): unit = jsNative
            member __.wrap(oldStream: ReadableStream): ReadableStream = jsNative
        member __._read(size: float): unit = jsNative
        member __.unshift(chunk: obj): unit = jsNative
        member __.wrap(oldStream: NodeJS.ReadableStream): NodeJS.ReadableStream = jsNative
        member __.push(chunk: obj, ?encoding: string): bool = jsNative

    and [<AllowNullLiteral>] WritableOptions =
        abstract highWaterMark: float option with get, set
        abstract decodeStrings: bool option with get, set
        abstract objectMode: bool option with get, set

    and [<AllowNullLiteral>] [<Import("Writable","stream")>] Writable(?opts: WritableOptions) =
        inherit events.EventEmitter()
        interface WritableStream with
            member __.writable with get(): bool = jsNative and set(v: bool): unit = jsNative
            member __.write(buffer: U2<Buffer, string>, ?cb: Function): bool = jsNative
            member __.write(str: string, ?encoding: string, ?cb: Function): bool = jsNative
            member __.``end``(): unit = jsNative
            member __.``end``(buffer: Buffer, ?cb: Function): unit = jsNative
            member __.``end``(str: string, ?cb: Function): unit = jsNative
            member __.``end``(str: string, ?encoding: string, ?cb: Function): unit = jsNative
        member __._write(chunk: obj, encoding: string, callback: Function): unit = jsNative
        member __.write(chunk: obj, ?cb: Function): bool = jsNative
        member __.write(chunk: obj, ?encoding: string, ?cb: Function): bool = jsNative
        member __.``end``(chunk: obj, ?cb: Function): unit = jsNative
        member __.``end``(chunk: obj, ?encoding: string, ?cb: Function): unit = jsNative

    and [<AllowNullLiteral>] DuplexOptions =
        inherit ReadableOptions
        inherit WritableOptions
        abstract allowHalfOpen: bool option with get, set

    and [<AllowNullLiteral>] [<Import("Duplex","stream")>] Duplex(?opts: DuplexOptions) =
        inherit Readable()
        // interface NodeJS.ReadWriteStream
        member __.writable with get(): bool = jsNative and set(v: bool): unit = jsNative
        member __._write(chunk: obj, encoding: string, callback: Function): unit = jsNative
        member __.write(chunk: obj, ?cb: Function): bool = jsNative
        member __.write(chunk: obj, ?encoding: string, ?cb: Function): bool = jsNative
        member __.``end``(): unit = jsNative
        member __.``end``(chunk: obj, ?cb: Function): unit = jsNative
        member __.``end``(chunk: obj, ?encoding: string, ?cb: Function): unit = jsNative

    and [<AllowNullLiteral>] TransformOptions =
        abstract highWaterMark: float option with get, set
        abstract decodeStrings: bool option with get, set
        abstract objectMode: bool option with get, set
        abstract encoding: string option with get, set

    and [<AllowNullLiteral>] [<Import("Transform","stream")>] Transform(?opts: TransformOptions) =
        inherit events.EventEmitter()
        // interface NodeJS.ReadWriteStream
        member __.readable with get(): bool = jsNative and set(v: bool): unit = jsNative
        member __.writable with get(): bool = jsNative and set(v: bool): unit = jsNative
        member __._transform(chunk: obj, encoding: string, callback: Function): unit = jsNative
        member __._flush(callback: Function): unit = jsNative
        member __.read(?size: float): obj = jsNative
        member __.setEncoding(encoding: string): unit = jsNative
        member __.pause(): unit = jsNative
        member __.resume(): unit = jsNative
        member __.pipe(destination: 'T, ?options: obj): 'T = jsNative
        member __.unpipe(?destination: 'T): unit = jsNative
        member __.unshift(chunk: obj): unit = jsNative
        member __.wrap(oldStream: NodeJS.ReadableStream): NodeJS.ReadableStream = jsNative
        member __.push(chunk: obj, ?encoding: string): bool = jsNative
        member __.write(chunk: obj, ?cb: Function): bool = jsNative
        member __.write(chunk: obj, ?encoding: string, ?cb: Function): bool = jsNative
        member __.``end``(): unit = jsNative
        member __.``end``(chunk: obj, ?cb: Function): unit = jsNative
        member __.``end``(chunk: obj, ?encoding: string, ?cb: Function): unit = jsNative

    and [<AllowNullLiteral>] [<Import("PassThrough","stream")>] PassThrough() =
        inherit Transform()


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


module child_process_types =
    type [<AllowNullLiteral>] ChildProcess =
        inherit EventEmitter
        abstract stdin: stream.Writable with get, set
        abstract stdout: stream.Readable with get, set
        abstract stderr: stream.Readable with get, set
        abstract stdio: U2<stream.Readable,stream.Writable>[] with get, set
        abstract pid: float with get, set
        abstract kill: ?signal: string -> unit
        abstract send: message: obj * ?sendHandle: obj -> unit
        abstract disconnect: unit -> unit
        abstract unref: unit -> unit

    type Globals =
        member __.spawn(command: string, ?args: ResizeArray<string>, ?options: obj): ChildProcess = jsNative
        member __.exec(command: string, options: obj, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = jsNative
        member __.exec(command: string, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = jsNative
        member __.execFile(file: string, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = jsNative
        member __.execFile(file: string, ?args: ResizeArray<string>, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = jsNative
        member __.execFile(file: string, ?args: ResizeArray<string>, ?options: obj, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = jsNative
        member __.fork(modulePath: string, ?args: ResizeArray<string>, ?options: obj): ChildProcess = jsNative
        member __.spawnSync(command: string, ?args: ResizeArray<string>, ?options: obj): obj = jsNative
        member __.execSync(command: string, ?options: obj): U2<string, Buffer> = jsNative
        member __.execFileSync(command: string, ?args: ResizeArray<string>, ?options: obj): U2<string, Buffer> = jsNative

let [<Import("*","child_process")>] child_process: child_process_types.Globals = jsNative


type [<AllowNullLiteral>] NodeRequireFunction =
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
    abstract parent: obj with get, set
    abstract children: ResizeArray<obj> with get, set

let [<Global>] ``process``: NodeJS.Process = jsNative
let [<Global>] ``global``: NodeJS.Global = jsNative
let [<Global>] __filename: string = jsNative
let [<Global>] __dirname: string = jsNative
let [<Global>] require: NodeRequire = jsNative
let [<Global>] ``module``: NodeModule = jsNative
let [<Global>] exports: obj = jsNative
let [<Global>] SlowBuffer: obj = jsNative
let [<Global>] Buffer: obj = jsNative


module buffer_types =
    type Globals =
        member __.INSPECT_MAX_BYTES with get(): float = jsNative and set(v: float): unit = jsNative

let [<Import("*","buffer")>] buffer: buffer_types.Globals = jsNative


module querystring_types =
    type Globals =
        member __.stringify(obj: obj, ?sep: string, ?eq: string): string = jsNative
        member __.parse(str: string, ?sep: string, ?eq: string, ?options: obj): obj = jsNative
        member __.escape(str: string): string = jsNative
        member __.unescape(str: string): string = jsNative

let [<Import("*","querystring")>] querystring: querystring_types.Globals = jsNative


module http_types =
    type [<AllowNullLiteral>] Server =
        abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
        abstract maxHeadersCount: float with get, set
        abstract listen: port: float * ?hostname: string * ?backlog: float * ?callback: Function -> Server
        abstract listen: port: float * ?hostname: string * ?callback: Function -> Server
        abstract listen: path: string * ?callback: Function -> Server
        abstract listen: handle: obj * ?listeningListener: Function -> Server
        abstract close: ?cb: obj -> Server
        abstract address: unit -> obj

    and [<AllowNullLiteral>] ServerRequest =
        inherit IncomingMessage
        abstract connection: net_types.Socket with get, set

    and [<AllowNullLiteral>] ServerResponse =
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

    and [<AllowNullLiteral>] ClientRequest =
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

    and [<AllowNullLiteral>] IncomingMessage =
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
        abstract socket: net_types.Socket with get, set
        abstract setTimeout: msecs: float * callback: Function -> NodeJS.Timer

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

    type Globals =
        member __.METHODS with get(): ResizeArray<string> = jsNative and set(v: ResizeArray<string>): unit = jsNative
        member __.STATUS_CODES with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.globalAgent with get(): Agent = jsNative and set(v: Agent): unit = jsNative
        member __.createServer(?requestListener: Func<IncomingMessage, ServerResponse, unit>): Server = jsNative
        member __.createClient(?port: float, ?host: string): obj = jsNative
        member __.request(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = jsNative
        member __.get(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = jsNative

let [<Import("*","http")>] http: http_types.Globals = jsNative

module cluster_types =
    type [<AllowNullLiteral>] ClusterSettings =
        abstract exec: string option with get, set
        abstract args: ResizeArray<string> option with get, set
        abstract silent: bool option with get, set

    and [<AllowNullLiteral>] [<Import("Worker","cluster")>] Worker() =
        inherit events.EventEmitter()
        member __.id with get(): string = jsNative and set(v: string): unit = jsNative
        member __.``process`` with get(): child_process_types.ChildProcess = jsNative and set(v: child_process_types.ChildProcess): unit = jsNative
        member __.suicide with get(): bool = jsNative and set(v: bool): unit = jsNative
        member __.send(message: obj, ?sendHandle: obj): unit = jsNative
        member __.kill(?signal: string): unit = jsNative
        member __.destroy(?signal: string): unit = jsNative
        member __.disconnect(): unit = jsNative

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


module zlib_types =
    type [<AllowNullLiteral>] ZlibOptions =
        abstract chunkSize: float option with get, set
        abstract windowBits: float option with get, set
        abstract level: float option with get, set
        abstract memLevel: float option with get, set
        abstract strategy: float option with get, set
        abstract dictionary: obj option with get, set

    and [<AllowNullLiteral>] Gzip =
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


    and [<AllowNullLiteral>] Gunzip =
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


    and [<AllowNullLiteral>] Deflate =
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


    and [<AllowNullLiteral>] Inflate =
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


    and [<AllowNullLiteral>] DeflateRaw =
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


    and [<AllowNullLiteral>] InflateRaw =
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


    and [<AllowNullLiteral>] Unzip =
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


    type Globals =
        member __.Z_NO_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_PARTIAL_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_SYNC_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_FULL_FLUSH with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_FINISH with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_BLOCK with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_TREES with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_OK with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_STREAM_END with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_NEED_DICT with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_ERRNO with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_STREAM_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_DATA_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_MEM_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_BUF_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_VERSION_ERROR with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_NO_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_BEST_SPEED with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_BEST_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_DEFAULT_COMPRESSION with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_FILTERED with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_HUFFMAN_ONLY with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_RLE with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_FIXED with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_DEFAULT_STRATEGY with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_BINARY with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_TEXT with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_ASCII with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_UNKNOWN with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_DEFLATED with get(): float = jsNative and set(v: float): unit = jsNative
        member __.Z_NULL with get(): float = jsNative and set(v: float): unit = jsNative
        member __.createGzip(?options: ZlibOptions): Gzip = jsNative
        member __.createGunzip(?options: ZlibOptions): Gunzip = jsNative
        member __.createDeflate(?options: ZlibOptions): Deflate = jsNative
        member __.createInflate(?options: ZlibOptions): Inflate = jsNative
        member __.createDeflateRaw(?options: ZlibOptions): DeflateRaw = jsNative
        member __.createInflateRaw(?options: ZlibOptions): InflateRaw = jsNative
        member __.createUnzip(?options: ZlibOptions): Unzip = jsNative
        member __.deflate(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.deflateSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative
        member __.deflateRaw(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.deflateRawSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative
        member __.gzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.gzipSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative
        member __.gunzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.gunzipSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative
        member __.inflate(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.inflateSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative
        member __.inflateRaw(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.inflateRawSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative
        member __.unzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = jsNative
        member __.unzipSync(buf: Buffer, ?options: ZlibOptions): obj = jsNative

let [<Import("*","zlib")>] zlib: zlib_types.Globals = jsNative


module os_types =
    type Globals =
        member __.EOL with get(): string = jsNative and set(v: string): unit = jsNative
        member __.tmpdir(): string = jsNative
        member __.hostname(): string = jsNative
        member __.``type``(): string = jsNative
        member __.platform(): string = jsNative
        member __.arch(): string = jsNative
        member __.release(): string = jsNative
        member __.uptime(): float = jsNative
        member __.loadavg(): ResizeArray<float> = jsNative
        member __.totalmem(): float = jsNative
        member __.freemem(): float = jsNative
        member __.cpus(): ResizeArray<obj> = jsNative
        member __.networkInterfaces(): obj = jsNative

let [<Import("*","os")>] os: os_types.Globals = jsNative


module https_types =
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
        abstract SNICallback: Func<string, obj> option with get, set

    and [<AllowNullLiteral>] RequestOptions =
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

    and [<AllowNullLiteral>] Agent =
        abstract maxSockets: float with get, set
        abstract sockets: obj with get, set
        abstract requests: obj with get, set

    and [<AllowNullLiteral>] Server =
        inherit tls_types.Server


    type Globals =
        member __.Agent with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.globalAgent with get(): Agent = jsNative and set(v: Agent): unit = jsNative
        member __.createServer(options: ServerOptions, ?requestListener: Function): Server = jsNative
        member __.request(options: RequestOptions, ?callback: Func<http_types.IncomingMessage, unit>): http_types.ClientRequest = jsNative
        member __.get(options: RequestOptions, ?callback: Func<http_types.IncomingMessage, unit>): http_types.ClientRequest = jsNative

let [<Import("*","https")>] https: https_types.Globals = jsNative


module punycode_types =
    type [<AllowNullLiteral>] ucs2 =
        abstract decode: string: string -> string
        abstract encode: codePoints: ResizeArray<float> -> string

    type Globals =
        member __.ucs2 with get(): ucs2 = jsNative and set(v: ucs2): unit = jsNative
        member __.version with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.decode(string: string): string = jsNative
        member __.encode(string: string): string = jsNative
        member __.toUnicode(domain: string): string = jsNative
        member __.toASCII(domain: string): string = jsNative

let [<Import("*","punycode")>] punycode: punycode_types.Globals = jsNative


module repl_types =
    type [<AllowNullLiteral>] ReplOptions =
        abstract prompt: string option with get, set
        abstract input: NodeJS.ReadableStream option with get, set
        abstract output: NodeJS.WritableStream option with get, set
        abstract terminal: bool option with get, set
        abstract eval: Function option with get, set
        abstract useColors: bool option with get, set
        abstract useGlobal: bool option with get, set
        abstract ignoreUndefined: bool option with get, set
        abstract writer: Function option with get, set

    type Globals =
        member __.start(options: ReplOptions): events.EventEmitter = jsNative

let [<Import("*","repl")>] repl: repl_types.Globals = jsNative


module readline_types =
    type [<AllowNullLiteral>] ReadLine =
        abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
        abstract setPrompt: prompt: string -> unit
        abstract prompt: ?preserveCursor: bool -> unit
        abstract question: query: string * callback: Function -> unit
        abstract pause: unit -> unit
        abstract resume: unit -> unit
        abstract close: unit -> unit
        abstract write: data: obj * ?key: obj -> unit

    and [<AllowNullLiteral>] ReadLineOptions =
        abstract input: NodeJS.ReadableStream with get, set
        abstract output: NodeJS.WritableStream with get, set
        abstract completer: Function option with get, set
        abstract terminal: bool option with get, set

    type Globals =
        member __.createInterface(options: ReadLineOptions): ReadLine = jsNative

let [<Import("*","readline")>] readline: readline_types.Globals = jsNative


module vm_types =
    type [<AllowNullLiteral>] Context =
        interface end

    and [<AllowNullLiteral>] Script =
        abstract runInThisContext: unit -> unit
        abstract runInNewContext: ?sandbox: Context -> unit

    type Globals =
        member __.runInThisContext(code: string, ?filename: string): unit = jsNative
        member __.runInNewContext(code: string, ?sandbox: Context, ?filename: string): unit = jsNative
        member __.runInContext(code: string, context: Context, ?filename: string): unit = jsNative
        member __.createContext(?initSandbox: Context): Context = jsNative
        member __.createScript(code: string, ?filename: string): Script = jsNative

let [<Import("*","vm")>] vm: vm_types.Globals = jsNative


module url_types =
    type [<AllowNullLiteral>] Url =
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

    and [<AllowNullLiteral>] UrlOptions =
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
        member __.parse(urlStr: string, ?parseQueryString: bool, ?slashesDenoteHost: bool): Url = jsNative
        member __.format(url: UrlOptions): string = jsNative
        member __.resolve(from: string, ``to``: string): string = jsNative

let [<Import("*","url")>] url: url_types.Globals = jsNative


module dns_types =
    type Globals =
        member __.lookup(domain: string, family: float, callback: Func<Error, string, float, unit>): string = jsNative
        member __.lookup(domain: string, callback: Func<Error, string, float, unit>): string = jsNative
        member __.resolve(domain: string, rrtype: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolve(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolve4(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolve6(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolveMx(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolveTxt(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolveSrv(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolveNs(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.resolveCname(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative
        member __.reverse(ip: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = jsNative

let [<Import("*","dns")>] dns: dns_types.Globals = jsNative


module dgram_types =
    type [<AllowNullLiteral>] RemoteInfo =
        abstract address: string with get, set
        abstract port: float with get, set
        abstract size: float with get, set

    and [<AllowNullLiteral>] AddressInfo =
        abstract address: string with get, set
        abstract family: string with get, set
        abstract port: float with get, set

    and [<AllowNullLiteral>] Socket =
        abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
        abstract send: buf: Buffer * offset: float * length: float * port: float * address: string * ?callback: Func<Error, float, unit> -> unit
        abstract bind: port: float * ?address: string * ?callback: Func<unit, unit> -> unit
        abstract close: unit -> unit
        abstract address: unit -> AddressInfo
        abstract setBroadcast: flag: bool -> unit
        abstract setMulticastTTL: ttl: float -> unit
        abstract setMulticastLoopback: flag: bool -> unit
        abstract addMembership: multicastAddress: string * ?multicastInterface: string -> unit
        abstract dropMembership: multicastAddress: string * ?multicastInterface: string -> unit

    type Globals =
        member __.createSocket(``type``: string, ?callback: Func<Buffer, RemoteInfo, unit>): Socket = jsNative

let [<Import("*","dgram")>] dgram: dgram_types.Globals = jsNative


module fs_types =
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
        abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
        abstract close: unit -> unit

    and [<AllowNullLiteral>] ReadStream =
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

    and [<AllowNullLiteral>] WriteStream =
        abstract writable: bool with get, set
        abstract _write: chunk: obj * encoding: string * callback: Function -> unit
        abstract write: chunk: obj * ?cb: Function -> bool
        abstract write: chunk: obj * ?encoding: string * ?cb: Function -> bool
        abstract ``end``: unit -> unit
        abstract ``end``: chunk: obj * ?cb: Function -> unit
        abstract ``end``: chunk: obj * ?encoding: string * ?cb: Function -> unit
        abstract bytesWritten: float with get, set
        abstract close: unit -> unit

    type Globals =
        member __.F_OK with get(): float = jsNative and set(v: float): unit = jsNative
        member __.R_OK with get(): float = jsNative and set(v: float): unit = jsNative
        member __.W_OK with get(): float = jsNative and set(v: float): unit = jsNative
        member __.X_OK with get(): float = jsNative and set(v: float): unit = jsNative
        member __.rename(oldPath: string, newPath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.renameSync(oldPath: string, newPath: string): unit = jsNative
        member __.truncate(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.truncate(path: string, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.truncateSync(path: string, ?len: float): unit = jsNative
        member __.ftruncate(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.ftruncate(fd: float, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.ftruncateSync(fd: float, ?len: float): unit = jsNative
        member __.chown(path: string, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.chownSync(path: string, uid: float, gid: float): unit = jsNative
        member __.fchown(fd: float, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.fchownSync(fd: float, uid: float, gid: float): unit = jsNative
        member __.lchown(path: string, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.lchownSync(path: string, uid: float, gid: float): unit = jsNative
        member __.chmod(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.chmod(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.chmodSync(path: string, mode: float): unit = jsNative
        member __.chmodSync(path: string, mode: string): unit = jsNative
        member __.fchmod(fd: float, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.fchmod(fd: float, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.fchmodSync(fd: float, mode: float): unit = jsNative
        member __.fchmodSync(fd: float, mode: string): unit = jsNative
        member __.lchmod(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.lchmod(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.lchmodSync(path: string, mode: float): unit = jsNative
        member __.lchmodSync(path: string, mode: string): unit = jsNative
        member __.stat(path: string, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = jsNative
        member __.lstat(path: string, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = jsNative
        member __.fstat(fd: float, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = jsNative
        member __.statSync(path: string): Stats = jsNative
        member __.lstatSync(path: string): Stats = jsNative
        member __.fstatSync(fd: float): Stats = jsNative
        member __.link(srcpath: string, dstpath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.linkSync(srcpath: string, dstpath: string): unit = jsNative
        member __.symlink(srcpath: string, dstpath: string, ?``type``: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.symlinkSync(srcpath: string, dstpath: string, ?``type``: string): unit = jsNative
        member __.readlink(path: string, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = jsNative
        member __.readlinkSync(path: string): string = jsNative
        member __.realpath(path: string, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = jsNative
        member __.realpath(path: string, cache: obj, callback: Func<NodeJS.ErrnoException, string, obj>): unit = jsNative
        member __.realpathSync(path: string, ?cache: obj): string = jsNative
        member __.unlink(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.unlinkSync(path: string): unit = jsNative
        member __.rmdir(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.rmdirSync(path: string): unit = jsNative
        member __.mkdir(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.mkdir(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.mkdir(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.mkdirSync(path: string, ?mode: float): unit = jsNative
        member __.mkdirSync(path: string, ?mode: string): unit = jsNative
        member __.readdir(path: string, ?callback: Func<NodeJS.ErrnoException, ResizeArray<string>, unit>): unit = jsNative
        member __.readdirSync(path: string): ResizeArray<string> = jsNative
        member __.close(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.closeSync(fd: float): unit = jsNative
        member __.``open``(path: string, flags: string, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = jsNative
        member __.``open``(path: string, flags: string, mode: float, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = jsNative
        member __.``open``(path: string, flags: string, mode: string, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = jsNative
        member __.openSync(path: string, flags: string, ?mode: float): float = jsNative
        member __.openSync(path: string, flags: string, ?mode: string): float = jsNative
        member __.utimes(path: string, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.utimes(path: string, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.utimesSync(path: string, atime: float, mtime: float): unit = jsNative
        member __.utimesSync(path: string, atime: DateTime, mtime: DateTime): unit = jsNative
        member __.futimes(fd: float, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.futimes(fd: float, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.futimesSync(fd: float, atime: float, mtime: float): unit = jsNative
        member __.futimesSync(fd: float, atime: DateTime, mtime: DateTime): unit = jsNative
        member __.fsync(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.fsyncSync(fd: float): unit = jsNative
        member __.write(fd: float, buffer: Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = jsNative
        member __.write(fd: float, buffer: Buffer, offset: float, length: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = jsNative
        member __.write(fd: float, data: obj, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = jsNative
        member __.write(fd: float, data: obj, offset: float, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = jsNative
        member __.write(fd: float, data: obj, offset: float, encoding: string, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = jsNative
        member __.writeSync(fd: float, buffer: Buffer, offset: float, length: float, position: float): float = jsNative
        member __.read(fd: float, buffer: Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = jsNative
        member __.readSync(fd: float, buffer: Buffer, offset: float, length: float, position: float): float = jsNative
        member __.readFile(filename: string, encoding: string, callback: Func<NodeJS.ErrnoException, string, unit>): unit = jsNative
        member __.readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, string, unit>): unit = jsNative
        member __.readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, Buffer, unit>): unit = jsNative
        member __.readFile(filename: string, callback: Func<NodeJS.ErrnoException, Buffer, unit>): unit = jsNative
        member __.readFileSync(filename: string, encoding: string): string = jsNative
        member __.readFileSync(filename: string, options: obj): string = jsNative
        member __.readFileSync(filename: string, ?options: obj): Buffer = jsNative
        member __.writeFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.writeFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.writeFileSync(filename: string, data: obj, ?options: obj): unit = jsNative
        member __.appendFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.appendFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.appendFileSync(filename: string, data: obj, ?options: obj): unit = jsNative
        member __.watchFile(filename: string, listener: Func<Stats, Stats, unit>): unit = jsNative
        member __.watchFile(filename: string, options: obj, listener: Func<Stats, Stats, unit>): unit = jsNative
        member __.unwatchFile(filename: string, ?listener: Func<Stats, Stats, unit>): unit = jsNative
        member __.watch(filename: string, ?listener: Func<string, string, obj>): FSWatcher = jsNative
        member __.watch(filename: string, options: obj, ?listener: Func<string, string, obj>): FSWatcher = jsNative
        member __.exists(path: string, ?callback: Func<bool, unit>): unit = jsNative
        member __.existsSync(path: string): bool = jsNative
        member __.access(path: string, callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.access(path: string, mode: float, callback: Func<NodeJS.ErrnoException, unit>): unit = jsNative
        member __.accessSync(path: string, ?mode: float): unit = jsNative
        member __.createReadStream(path: string, ?options: obj): ReadStream = jsNative
        member __.createWriteStream(path: string, ?options: obj): WriteStream = jsNative

let [<Import("*","fs")>] fs: fs_types.Globals = jsNative


module path_types =
    type [<AllowNullLiteral>] ParsedPath =
        abstract root: string with get, set
        abstract dir: string with get, set
        abstract ``base``: string with get, set
        abstract ext: string with get, set
        abstract name: string with get, set

    type Globals =
        member __.sep with get(): string = jsNative and set(v: string): unit = jsNative
        member __.delimiter with get(): string = jsNative and set(v: string): unit = jsNative
        member __.normalize(p: string): string = jsNative
        member __.join([<ParamArray>] paths: obj[]): string = jsNative
        member __.join([<ParamArray>] paths: string[]): string = jsNative
        member __.resolve([<ParamArray>] pathSegments: obj[]): string = jsNative
        member __.isAbsolute(path: string): bool = jsNative
        member __.relative(from: string, ``to``: string): string = jsNative
        member __.dirname(p: string): string = jsNative
        member __.basename(p: string, ?ext: string): string = jsNative
        member __.extname(p: string): string = jsNative
        member __.parse(pathString: string): ParsedPath = jsNative
        member __.format(pathObject: ParsedPath): string = jsNative

    module posix_types =
        type Globals =
            member __.sep with get(): string = jsNative and set(v: string): unit = jsNative
            member __.delimiter with get(): string = jsNative and set(v: string): unit = jsNative
            member __.normalize(p: string): string = jsNative
            member __.join([<ParamArray>] paths: obj[]): string = jsNative
            member __.resolve([<ParamArray>] pathSegments: obj[]): string = jsNative
            member __.isAbsolute(p: string): bool = jsNative
            member __.relative(from: string, ``to``: string): string = jsNative
            member __.dirname(p: string): string = jsNative
            member __.basename(p: string, ?ext: string): string = jsNative
            member __.extname(p: string): string = jsNative
            member __.parse(p: string): ParsedPath = jsNative
            member __.format(pP: ParsedPath): string = jsNative
            
    let [<Import("posix","path")>] posix: posix_types.Globals = jsNative

    module win32_types =
        type Globals =
            member __.sep with get(): string = jsNative and set(v: string): unit = jsNative
            member __.delimiter with get(): string = jsNative and set(v: string): unit = jsNative
            member __.normalize(p: string): string = jsNative
            member __.join([<ParamArray>] paths: obj[]): string = jsNative
            member __.resolve([<ParamArray>] pathSegments: obj[]): string = jsNative
            member __.isAbsolute(p: string): bool = jsNative
            member __.relative(from: string, ``to``: string): string = jsNative
            member __.dirname(p: string): string = jsNative
            member __.basename(p: string, ?ext: string): string = jsNative
            member __.extname(p: string): string = jsNative
            member __.parse(p: string): ParsedPath = jsNative
            member __.format(pP: ParsedPath): string = jsNative

    let [<Import("win32","path")>] win32: win32_types.Globals = jsNative

let [<Import("*","path")>] path: path_types.Globals = jsNative


[<Import("*","string_decoder")>]
module string_decoder =
    type [<AllowNullLiteral>] NodeStringDecoder =
        abstract write: buffer: Buffer -> string
        abstract detectIncompleteChar: buffer: Buffer -> float

    let StringDecoder: NodeStringDecoder = jsNative


module util_types =
    type [<AllowNullLiteral>] InspectOptions =
        abstract showHidden: bool option with get, set
        abstract depth: float option with get, set
        abstract colors: bool option with get, set
        abstract customInspect: bool option with get, set

    type Globals =
        member __.format(format: obj, [<ParamArray>] param: obj[]): string = jsNative
        member __.debug(string: string): unit = jsNative
        member __.error([<ParamArray>] param: obj[]): unit = jsNative
        member __.puts([<ParamArray>] param: obj[]): unit = jsNative
        member __.print([<ParamArray>] param: obj[]): unit = jsNative
        member __.log(string: string): unit = jsNative
        member __.inspect(``object``: obj, ?showHidden: bool, ?depth: float, ?color: bool): string = jsNative
        member __.inspect(``object``: obj, options: InspectOptions): string = jsNative
        member __.isArray(``object``: obj): bool = jsNative
        member __.isRegExp(``object``: obj): bool = jsNative
        member __.isDate(``object``: obj): bool = jsNative
        member __.isError(``object``: obj): bool = jsNative
        member __.inherits(``constructor``: obj, superConstructor: obj): unit = jsNative
        member __.debuglog(key: string): Func<string, obj, unit> = jsNative

let [<Import("*","util")>] util: util_types.Globals = jsNative


module assert_types =
    type [<AllowNullLiteral>] [<Import("AssertionError","assert")>] AssertionError(?options: obj) =
        // interface Error
        member __.name with get(): string = jsNative and set(v: string): unit = jsNative
        member __.message with get(): string = jsNative and set(v: string): unit = jsNative
        member __.actual with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.expected with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.operator with get(): string = jsNative and set(v: string): unit = jsNative
        member __.generatedMessage with get(): bool = jsNative and set(v: bool): unit = jsNative

    type Globals =
        member __.throws with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.doesNotThrow with get(): obj = jsNative and set(v: obj): unit = jsNative
        member __.fail(?actual: obj, ?expected: obj, ?message: string, ?operator: string): unit = jsNative
        member __.ok(value: obj, ?message: string): unit = jsNative
        member __.equal(actual: obj, expected: obj, ?message: string): unit = jsNative
        member __.notEqual(actual: obj, expected: obj, ?message: string): unit = jsNative
        member __.deepEqual(actual: obj, expected: obj, ?message: string): unit = jsNative
        member __.notDeepEqual(acutal: obj, expected: obj, ?message: string): unit = jsNative
        member __.strictEqual(actual: obj, expected: obj, ?message: string): unit = jsNative
        member __.notStrictEqual(actual: obj, expected: obj, ?message: string): unit = jsNative
        member __.ifError(value: obj): unit = jsNative

let [<Import("*","assert")>] ``assert``: assert_types.Globals = jsNative


[<Import("*","tty")>] 
module tty =
    type [<AllowNullLiteral>] ReadStream =
        inherit net_types.Socket
        abstract isRaw: bool with get, set
        abstract setRawMode: mode: bool -> unit

    and [<AllowNullLiteral>] WriteStream =
        inherit net_types.Socket
        abstract columns: float with get, set
        abstract rows: float with get, set

    let isatty(fd: float): bool = jsNative


[<Import("*","domain")>] 
module domain =
    type [<AllowNullLiteral>] [<Import("Domain","domain")>] Domain() =
        inherit events.EventEmitter()
        member __.run(fn: Function): unit = jsNative
        member __.add(emitter: events.EventEmitter): unit = jsNative
        member __.remove(emitter: events.EventEmitter): unit = jsNative
        member __.bind(cb: Func<Error, obj, obj>): obj = jsNative
        member __.intercept(cb: Func<obj, obj>): obj = jsNative
        member __.dispose(): unit = jsNative
        member __.addListener(``event``: string, listener: Function): Domain = jsNative
        member __.on(``event``: string, listener: Function): Domain = jsNative
        member __.once(``event``: string, listener: Function): Domain = jsNative
        member __.removeListener(``event``: string, listener: Function): Domain = jsNative
        member __.removeAllListeners(?``event``: string): Domain = jsNative

    let create(): Domain = jsNative


[<Import("*","constants")>]
module constants =
    let E2BIG: float = jsNative
    let EACCES: float = jsNative
    let EADDRINUSE: float = jsNative
    let EADDRNOTAVAIL: float = jsNative
    let EAFNOSUPPORT: float = jsNative
    let EAGAIN: float = jsNative
    let EALREADY: float = jsNative
    let EBADF: float = jsNative
    let EBADMSG: float = jsNative
    let EBUSY: float = jsNative
    let ECANCELED: float = jsNative
    let ECHILD: float = jsNative
    let ECONNABORTED: float = jsNative
    let ECONNREFUSED: float = jsNative
    let ECONNRESET: float = jsNative
    let EDEADLK: float = jsNative
    let EDESTADDRREQ: float = jsNative
    let EDOM: float = jsNative
    let EEXIST: float = jsNative
    let EFAULT: float = jsNative
    let EFBIG: float = jsNative
    let EHOSTUNREACH: float = jsNative
    let EIDRM: float = jsNative
    let EILSEQ: float = jsNative
    let EINPROGRESS: float = jsNative
    let EINTR: float = jsNative
    let EINVAL: float = jsNative
    let EIO: float = jsNative
    let EISCONN: float = jsNative
    let EISDIR: float = jsNative
    let ELOOP: float = jsNative
    let EMFILE: float = jsNative
    let EMLINK: float = jsNative
    let EMSGSIZE: float = jsNative
    let ENAMETOOLONG: float = jsNative
    let ENETDOWN: float = jsNative
    let ENETRESET: float = jsNative
    let ENETUNREACH: float = jsNative
    let ENFILE: float = jsNative
    let ENOBUFS: float = jsNative
    let ENODATA: float = jsNative
    let ENODEV: float = jsNative
    let ENOENT: float = jsNative
    let ENOEXEC: float = jsNative
    let ENOLCK: float = jsNative
    let ENOLINK: float = jsNative
    let ENOMEM: float = jsNative
    let ENOMSG: float = jsNative
    let ENOPROTOOPT: float = jsNative
    let ENOSPC: float = jsNative
    let ENOSR: float = jsNative
    let ENOSTR: float = jsNative
    let ENOSYS: float = jsNative
    let ENOTCONN: float = jsNative
    let ENOTDIR: float = jsNative
    let ENOTEMPTY: float = jsNative
    let ENOTSOCK: float = jsNative
    let ENOTSUP: float = jsNative
    let ENOTTY: float = jsNative
    let ENXIO: float = jsNative
    let EOPNOTSUPP: float = jsNative
    let EOVERFLOW: float = jsNative
    let EPERM: float = jsNative
    let EPIPE: float = jsNative
    let EPROTO: float = jsNative
    let EPROTONOSUPPORT: float = jsNative
    let EPROTOTYPE: float = jsNative
    let ERANGE: float = jsNative
    let EROFS: float = jsNative
    let ESPIPE: float = jsNative
    let ESRCH: float = jsNative
    let ETIME: float = jsNative
    let ETIMEDOUT: float = jsNative
    let ETXTBSY: float = jsNative
    let EWOULDBLOCK: float = jsNative
    let EXDEV: float = jsNative
    let WSAEINTR: float = jsNative
    let WSAEBADF: float = jsNative
    let WSAEACCES: float = jsNative
    let WSAEFAULT: float = jsNative
    let WSAEINVAL: float = jsNative
    let WSAEMFILE: float = jsNative
    let WSAEWOULDBLOCK: float = jsNative
    let WSAEINPROGRESS: float = jsNative
    let WSAEALREADY: float = jsNative
    let WSAENOTSOCK: float = jsNative
    let WSAEDESTADDRREQ: float = jsNative
    let WSAEMSGSIZE: float = jsNative
    let WSAEPROTOTYPE: float = jsNative
    let WSAENOPROTOOPT: float = jsNative
    let WSAEPROTONOSUPPORT: float = jsNative
    let WSAESOCKTNOSUPPORT: float = jsNative
    let WSAEOPNOTSUPP: float = jsNative
    let WSAEPFNOSUPPORT: float = jsNative
    let WSAEAFNOSUPPORT: float = jsNative
    let WSAEADDRINUSE: float = jsNative
    let WSAEADDRNOTAVAIL: float = jsNative
    let WSAENETDOWN: float = jsNative
    let WSAENETUNREACH: float = jsNative
    let WSAENETRESET: float = jsNative
    let WSAECONNABORTED: float = jsNative
    let WSAECONNRESET: float = jsNative
    let WSAENOBUFS: float = jsNative
    let WSAEISCONN: float = jsNative
    let WSAENOTCONN: float = jsNative
    let WSAESHUTDOWN: float = jsNative
    let WSAETOOMANYREFS: float = jsNative
    let WSAETIMEDOUT: float = jsNative
    let WSAECONNREFUSED: float = jsNative
    let WSAELOOP: float = jsNative
    let WSAENAMETOOLONG: float = jsNative
    let WSAEHOSTDOWN: float = jsNative
    let WSAEHOSTUNREACH: float = jsNative
    let WSAENOTEMPTY: float = jsNative
    let WSAEPROCLIM: float = jsNative
    let WSAEUSERS: float = jsNative
    let WSAEDQUOT: float = jsNative
    let WSAESTALE: float = jsNative
    let WSAEREMOTE: float = jsNative
    let WSASYSNOTREADY: float = jsNative
    let WSAVERNOTSUPPORTED: float = jsNative
    let WSANOTINITIALISED: float = jsNative
    let WSAEDISCON: float = jsNative
    let WSAENOMORE: float = jsNative
    let WSAECANCELLED: float = jsNative
    let WSAEINVALIDPROCTABLE: float = jsNative
    let WSAEINVALIDPROVIDER: float = jsNative
    let WSAEPROVIDERFAILEDINIT: float = jsNative
    let WSASYSCALLFAILURE: float = jsNative
    let WSASERVICE_NOT_FOUND: float = jsNative
    let WSATYPE_NOT_FOUND: float = jsNative
    let WSA_E_NO_MORE: float = jsNative
    let WSA_E_CANCELLED: float = jsNative
    let WSAEREFUSED: float = jsNative
    let SIGHUP: float = jsNative
    let SIGINT: float = jsNative
    let SIGILL: float = jsNative
    let SIGABRT: float = jsNative
    let SIGFPE: float = jsNative
    let SIGKILL: float = jsNative
    let SIGSEGV: float = jsNative
    let SIGTERM: float = jsNative
    let SIGBREAK: float = jsNative
    let SIGWINCH: float = jsNative
    let SSL_OP_ALL: float = jsNative
    let SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION: float = jsNative
    let SSL_OP_CIPHER_SERVER_PREFERENCE: float = jsNative
    let SSL_OP_CISCO_ANYCONNECT: float = jsNative
    let SSL_OP_COOKIE_EXCHANGE: float = jsNative
    let SSL_OP_CRYPTOPRO_TLSEXT_BUG: float = jsNative
    let SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS: float = jsNative
    let SSL_OP_EPHEMERAL_RSA: float = jsNative
    let SSL_OP_LEGACY_SERVER_CONNECT: float = jsNative
    let SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER: float = jsNative
    let SSL_OP_MICROSOFT_SESS_ID_BUG: float = jsNative
    let SSL_OP_MSIE_SSLV2_RSA_PADDING: float = jsNative
    let SSL_OP_NETSCAPE_CA_DN_BUG: float = jsNative
    let SSL_OP_NETSCAPE_CHALLENGE_BUG: float = jsNative
    let SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG: float = jsNative
    let SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG: float = jsNative
    let SSL_OP_NO_COMPRESSION: float = jsNative
    let SSL_OP_NO_QUERY_MTU: float = jsNative
    let SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION: float = jsNative
    let SSL_OP_NO_SSLv2: float = jsNative
    let SSL_OP_NO_SSLv3: float = jsNative
    let SSL_OP_NO_TICKET: float = jsNative
    let SSL_OP_NO_TLSv1: float = jsNative
    let SSL_OP_NO_TLSv1_1: float = jsNative
    let SSL_OP_NO_TLSv1_2: float = jsNative
    let SSL_OP_PKCS1_CHECK_1: float = jsNative
    let SSL_OP_PKCS1_CHECK_2: float = jsNative
    let SSL_OP_SINGLE_DH_USE: float = jsNative
    let SSL_OP_SINGLE_ECDH_USE: float = jsNative
    let SSL_OP_SSLEAY_080_CLIENT_DH_BUG: float = jsNative
    let SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG: float = jsNative
    let SSL_OP_TLS_BLOCK_PADDING_BUG: float = jsNative
    let SSL_OP_TLS_D5_BUG: float = jsNative
    let SSL_OP_TLS_ROLLBACK_BUG: float = jsNative
    let ENGINE_METHOD_DSA: float = jsNative
    let ENGINE_METHOD_DH: float = jsNative
    let ENGINE_METHOD_RAND: float = jsNative
    let ENGINE_METHOD_ECDH: float = jsNative
    let ENGINE_METHOD_ECDSA: float = jsNative
    let ENGINE_METHOD_CIPHERS: float = jsNative
    let ENGINE_METHOD_DIGESTS: float = jsNative
    let ENGINE_METHOD_STORE: float = jsNative
    let ENGINE_METHOD_PKEY_METHS: float = jsNative
    let ENGINE_METHOD_PKEY_ASN1_METHS: float = jsNative
    let ENGINE_METHOD_ALL: float = jsNative
    let ENGINE_METHOD_NONE: float = jsNative
    let DH_CHECK_P_NOT_SAFE_PRIME: float = jsNative
    let DH_CHECK_P_NOT_PRIME: float = jsNative
    let DH_UNABLE_TO_CHECK_GENERATOR: float = jsNative
    let DH_NOT_SUITABLE_GENERATOR: float = jsNative
    let NPN_ENABLED: float = jsNative
    let RSA_PKCS1_PADDING: float = jsNative
    let RSA_SSLV23_PADDING: float = jsNative
    let RSA_NO_PADDING: float = jsNative
    let RSA_PKCS1_OAEP_PADDING: float = jsNative
    let RSA_X931_PADDING: float = jsNative
    let RSA_PKCS1_PSS_PADDING: float = jsNative
    let POINT_CONVERSION_COMPRESSED: float = jsNative
    let POINT_CONVERSION_UNCOMPRESSED: float = jsNative
    let POINT_CONVERSION_HYBRID: float = jsNative
    let O_RDONLY: float = jsNative
    let O_WRONLY: float = jsNative
    let O_RDWR: float = jsNative
    let S_IFMT: float = jsNative
    let S_IFREG: float = jsNative
    let S_IFDIR: float = jsNative
    let S_IFCHR: float = jsNative
    let S_IFLNK: float = jsNative
    let O_CREAT: float = jsNative
    let O_EXCL: float = jsNative
    let O_TRUNC: float = jsNative
    let O_APPEND: float = jsNative
    let F_OK: float = jsNative
    let R_OK: float = jsNative
    let W_OK: float = jsNative
    let X_OK: float = jsNative
    let UV_UDP_REUSEADDR: float = jsNative
