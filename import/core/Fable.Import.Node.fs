module Fable.Import.Node
open System
open Fable.Core
open Fable.Import.JS

type Error =
    abstract stack: string option with get, set

// and MapConstructor =
//     interface end
// 
// and WeakMapConstructor =
//     interface end
// 
// and SetConstructor =
//     interface end
// 
// and WeakSetConstructor =
//     interface end

and Buffer =
    inherit NodeBuffer

and NodeBuffer =
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
        abstract setMaxListeners: n: int -> unit
        abstract getMaxListeners: unit -> int
        abstract listeners: ``event``: string -> ResizeArray<Function>
        abstract emit: ``event``: string * [<ParamArray>] args: obj[] -> bool
        abstract listenerCount: ``type``: string -> int

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
        abstract gc: Func<unit, unit> with get, set

    and Timer =
        abstract ref: unit -> unit
        abstract unref: unit -> unit
        
open NodeJS


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

    type Globals =
        member __.Socket with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.createServer(?connectionListener: Func<Socket, unit>): Server = failwith "JS only"
        member __.createServer(?options: obj, ?connectionListener: Func<Socket, unit>): Server = failwith "JS only"
        member __.connect(options: obj, ?connectionListener: Function): Socket = failwith "JS only"
        member __.connect(port: float, ?host: string, ?connectionListener: Function): Socket = failwith "JS only"
        member __.connect(path: string, ?connectionListener: Function): Socket = failwith "JS only"
        member __.createConnection(options: obj, ?connectionListener: Function): Socket = failwith "JS only"
        member __.createConnection(port: float, ?host: string, ?connectionListener: Function): Socket = failwith "JS only"
        member __.createConnection(path: string, ?connectionListener: Function): Socket = failwith "JS only"
        member __.isIP(input: string): float = failwith "JS only"
        member __.isIPv4(input: string): bool = failwith "JS only"
        member __.isIPv6(input: string): bool = failwith "JS only"
        
let [<Import("*","net")>] net: net.Globals = failwith "JS only"


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

    type Globals =
        member __.createCredentials(details: CredentialDetails): Credentials = failwith "JS only"
        member __.createHash(algorithm: string): Hash = failwith "JS only"
        member __.createHmac(algorithm: string, key: string): Hmac = failwith "JS only"
        member __.createHmac(algorithm: string, key: Buffer): Hmac = failwith "JS only"
        member __.createCipher(algorithm: string, password: obj): Cipher = failwith "JS only"
        member __.createCipheriv(algorithm: string, key: obj, iv: obj): Cipher = failwith "JS only"
        member __.createDecipher(algorithm: string, password: obj): Decipher = failwith "JS only"
        member __.createDecipheriv(algorithm: string, key: obj, iv: obj): Decipher = failwith "JS only"
        member __.createSign(algorithm: string): Signer = failwith "JS only"
        member __.createVerify(algorith: string): Verify = failwith "JS only"
        member __.createDiffieHellman(prime_length: float): DiffieHellman = failwith "JS only"
        member __.createDiffieHellman(prime: float, ?encoding: string): DiffieHellman = failwith "JS only"
        member __.getDiffieHellman(group_name: string): DiffieHellman = failwith "JS only"
        member __.pbkdf2(password: string, salt: string, iterations: float, keylen: float, callback: Func<Error, Buffer, obj>): unit = failwith "JS only"
        member __.pbkdf2(password: string, salt: string, iterations: float, keylen: float, digest: string, callback: Func<Error, Buffer, obj>): unit = failwith "JS only"
        member __.pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float): Buffer = failwith "JS only"
        member __.pbkdf2Sync(password: string, salt: string, iterations: float, keylen: float, digest: string): Buffer = failwith "JS only"
        member __.randomBytes(size: float): Buffer = failwith "JS only"
        member __.randomBytes(size: float, callback: Func<Error, Buffer, unit>): unit = failwith "JS only"
        member __.pseudoRandomBytes(size: float): Buffer = failwith "JS only"
        member __.pseudoRandomBytes(size: float, callback: Func<Error, Buffer, unit>): unit = failwith "JS only"

let [<Import("*","crypto")>] crypto: crypto.Globals = failwith "JS only"


module events =
    type [<Import("EventEmitter","events")>] EventEmitter() =
        interface NodeJS.EventEmitter with
            member __.addListener(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
            member __.on(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
            member __.once(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
            member __.removeListener(``event``: string, listener: Function): NodeJS.EventEmitter = failwith "JS only"
            member __.removeAllListeners(?``event``: string): NodeJS.EventEmitter = failwith "JS only"
            member __.setMaxListeners(n: int): unit = failwith "JS only"
            member __.getMaxListeners(): int = failwith "JS only"
            member __.listeners(``event``: string): ResizeArray<Function> = failwith "JS only"
            member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = failwith "JS only"
            member __.listenerCount(``type``: string): int = failwith "JS only"
        member __.listenerCount(emitter: EventEmitter, ``event``: string): float = failwith "JS only"
        static member EventEmitter with get(): EventEmitter = failwith "JS only"
        static member defaultMaxListeners with get(): int = failwith "JS only"


module stream =
    type Stream =
        abstract listenerCount: emitter: EventEmitter * ``event``: string -> float
        abstract pipe: destination: 'T * ?options: obj -> 'T

    and ReadableOptions =
        abstract highWaterMark: float option with get, set
        abstract encoding: string option with get, set
        abstract objectMode: bool option with get, set

    and [<Import("Readable","stream")>] Readable(?opts: ReadableOptions) =
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

    type Globals =
        member __.CLIENT_RENEG_LIMIT with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.CLIENT_RENEG_WINDOW with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.createServer(options: TlsOptions, ?secureConnectionListener: Func<ClearTextStream, unit>): Server = failwith "JS only"
        member __.connect(options: TlsOptions, ?secureConnectionListener: Func<unit, unit>): ClearTextStream = failwith "JS only"
        member __.connect(port: float, ?host: string, ?options: ConnectionOptions, ?secureConnectListener: Func<unit, unit>): ClearTextStream = failwith "JS only"
        member __.connect(port: float, ?options: ConnectionOptions, ?secureConnectListener: Func<unit, unit>): ClearTextStream = failwith "JS only"
        member __.createSecurePair(?credentials: crypto.Credentials, ?isServer: bool, ?requestCert: bool, ?rejectUnauthorized: bool): SecurePair = failwith "JS only"
        member __.createSecureContext(details: SecureContextOptions): SecureContext = failwith "JS only"

let [<Import("*","tls")>] tls: tls.Globals = failwith "JS only"


module child_process =
    type ChildProcess =
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
        member __.spawn(command: string, ?args: ResizeArray<string>, ?options: obj): ChildProcess = failwith "JS only"
        member __.exec(command: string, options: obj, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        member __.exec(command: string, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        member __.execFile(file: string, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        member __.execFile(file: string, ?args: ResizeArray<string>, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        member __.execFile(file: string, ?args: ResizeArray<string>, ?options: obj, ?callback: Func<Error, Buffer, Buffer, unit>): ChildProcess = failwith "JS only"
        member __.fork(modulePath: string, ?args: ResizeArray<string>, ?options: obj): ChildProcess = failwith "JS only"
        member __.spawnSync(command: string, ?args: ResizeArray<string>, ?options: obj): obj = failwith "JS only"
        member __.execSync(command: string, ?options: obj): U2<string, Buffer> = failwith "JS only"
        member __.execFileSync(command: string, ?args: ResizeArray<string>, ?options: obj): U2<string, Buffer> = failwith "JS only"

let [<Import("*","child_process")>] child_process: child_process.Globals = failwith "JS only"


type NodeRequireFunction =
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

let [<Global>] ``process``: NodeJS.Process = failwith "JS only"
let [<Global>] ``global``: NodeJS.Global = failwith "JS only"
let [<Global>] __filename: string = failwith "JS only"
let [<Global>] __dirname: string = failwith "JS only"
let [<Global>] require: NodeRequire = failwith "JS only"
let [<Global>] ``module``: NodeModule = failwith "JS only"
let [<Global>] exports: obj = failwith "JS only"
let [<Global>] SlowBuffer: obj = failwith "JS only"
let [<Global>] Buffer: obj = failwith "JS only"


module buffer =
    type Globals =
        member __.INSPECT_MAX_BYTES with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

let [<Import("*","buffer")>] buffer: buffer.Globals = failwith "JS only"


module querystring =
    type Globals =
        member __.stringify(obj: obj, ?sep: string, ?eq: string): string = failwith "JS only"
        member __.parse(str: string, ?sep: string, ?eq: string, ?options: obj): obj = failwith "JS only"
        member __.escape(str: string): string = failwith "JS only"
        member __.unescape(str: string): string = failwith "JS only"

let [<Import("*","querystring")>] querystring: querystring.Globals = failwith "JS only"


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

    type Globals =
        member __.METHODS with get(): ResizeArray<string> = failwith "JS only" and set(v: ResizeArray<string>): unit = failwith "JS only"
        member __.STATUS_CODES with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.globalAgent with get(): Agent = failwith "JS only" and set(v: Agent): unit = failwith "JS only"
        member __.createServer(?requestListener: Func<IncomingMessage, ServerResponse, unit>): Server = failwith "JS only"
        member __.createClient(?port: float, ?host: string): obj = failwith "JS only"
        member __.request(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = failwith "JS only"
        member __.get(options: obj, ?callback: Func<IncomingMessage, unit>): ClientRequest = failwith "JS only"

let [<Import("*","http")>] http: http.Globals = failwith "JS only"


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

    type Globals =
        member __.settings with get(): ClusterSettings = failwith "JS only" and set(v: ClusterSettings): unit = failwith "JS only"
        member __.isMaster with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.isWorker with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
        member __.worker with get(): Worker = failwith "JS only" and set(v: Worker): unit = failwith "JS only"
        member __.workers with get(): ResizeArray<Worker> = failwith "JS only" and set(v: ResizeArray<Worker>): unit = failwith "JS only"
        member __.setupMaster(?settings: ClusterSettings): unit = failwith "JS only"
        member __.fork(?env: obj): Worker = failwith "JS only"
        member __.disconnect(?callback: Function): unit = failwith "JS only"
        member __.addListener(``event``: string, listener: Function): unit = failwith "JS only"
        member __.on(``event``: string, listener: Function): obj = failwith "JS only"
        member __.once(``event``: string, listener: Function): unit = failwith "JS only"
        member __.removeListener(``event``: string, listener: Function): unit = failwith "JS only"
        member __.removeAllListeners(?``event``: string): unit = failwith "JS only"
        member __.setMaxListeners(n: float): unit = failwith "JS only"
        member __.listeners(``event``: string): ResizeArray<Function> = failwith "JS only"
        member __.emit(``event``: string, [<ParamArray>] args: obj[]): bool = failwith "JS only"

let [<Import("*","cluster")>] cluster: cluster.Globals = failwith "JS only"


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


    type Globals =
        member __.Z_NO_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_PARTIAL_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_SYNC_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_FULL_FLUSH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_FINISH with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_BLOCK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_TREES with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_STREAM_END with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_NEED_DICT with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_ERRNO with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_STREAM_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_DATA_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_MEM_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_BUF_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_VERSION_ERROR with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_NO_COMPRESSION with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_BEST_SPEED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_BEST_COMPRESSION with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_DEFAULT_COMPRESSION with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_FILTERED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_HUFFMAN_ONLY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_RLE with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_FIXED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_DEFAULT_STRATEGY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_BINARY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_TEXT with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_ASCII with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_UNKNOWN with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_DEFLATED with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.Z_NULL with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.createGzip(?options: ZlibOptions): Gzip = failwith "JS only"
        member __.createGunzip(?options: ZlibOptions): Gunzip = failwith "JS only"
        member __.createDeflate(?options: ZlibOptions): Deflate = failwith "JS only"
        member __.createInflate(?options: ZlibOptions): Inflate = failwith "JS only"
        member __.createDeflateRaw(?options: ZlibOptions): DeflateRaw = failwith "JS only"
        member __.createInflateRaw(?options: ZlibOptions): InflateRaw = failwith "JS only"
        member __.createUnzip(?options: ZlibOptions): Unzip = failwith "JS only"
        member __.deflate(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.deflateSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        member __.deflateRaw(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.deflateRawSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        member __.gzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.gzipSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        member __.gunzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.gunzipSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        member __.inflate(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.inflateSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        member __.inflateRaw(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.inflateRawSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"
        member __.unzip(buf: Buffer, callback: Func<Error, obj, unit>): unit = failwith "JS only"
        member __.unzipSync(buf: Buffer, ?options: ZlibOptions): obj = failwith "JS only"

let [<Import("*","zlib")>] zlib: zlib.Globals = failwith "JS only"


module os =
    type Globals =
        member __.EOL with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.tmpdir(): string = failwith "JS only"
        member __.hostname(): string = failwith "JS only"
        member __.``type``(): string = failwith "JS only"
        member __.platform(): string = failwith "JS only"
        member __.arch(): string = failwith "JS only"
        member __.release(): string = failwith "JS only"
        member __.uptime(): float = failwith "JS only"
        member __.loadavg(): ResizeArray<float> = failwith "JS only"
        member __.totalmem(): float = failwith "JS only"
        member __.freemem(): float = failwith "JS only"
        member __.cpus(): ResizeArray<obj> = failwith "JS only"
        member __.networkInterfaces(): obj = failwith "JS only"

let [<Import("*","os")>] os: os.Globals = failwith "JS only"


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
        member __.Agent with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.globalAgent with get(): Agent = failwith "JS only" and set(v: Agent): unit = failwith "JS only"
        member __.createServer(options: ServerOptions, ?requestListener: Function): Server = failwith "JS only"
        member __.request(options: RequestOptions, ?callback: Func<http.IncomingMessage, unit>): http.ClientRequest = failwith "JS only"
        member __.get(options: RequestOptions, ?callback: Func<http.IncomingMessage, unit>): http.ClientRequest = failwith "JS only"

let [<Import("*","https")>] https: https.Globals = failwith "JS only"


module punycode =
    type ucs2 =
        abstract decode: string: string -> string
        abstract encode: codePoints: ResizeArray<float> -> string

    type Globals =
        member __.ucs2 with get(): ucs2 = failwith "JS only" and set(v: ucs2): unit = failwith "JS only"
        member __.version with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.decode(string: string): string = failwith "JS only"
        member __.encode(string: string): string = failwith "JS only"
        member __.toUnicode(domain: string): string = failwith "JS only"
        member __.toASCII(domain: string): string = failwith "JS only"

let [<Import("*","punycode")>] punycode: punycode.Globals = failwith "JS only"


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

    type Globals =
        member __.start(options: ReplOptions): events.EventEmitter = failwith "JS only"

let [<Import("*","repl")>] repl: repl.Globals = failwith "JS only"


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

    type Globals =
        member __.createInterface(options: ReadLineOptions): ReadLine = failwith "JS only"

let [<Import("*","readline")>] readline: readline.Globals = failwith "JS only"


module vm =
    type Context =
        interface end

    and Script =
        abstract runInThisContext: unit -> unit
        abstract runInNewContext: ?sandbox: Context -> unit

    type Globals =
        member __.runInThisContext(code: string, ?filename: string): unit = failwith "JS only"
        member __.runInNewContext(code: string, ?sandbox: Context, ?filename: string): unit = failwith "JS only"
        member __.runInContext(code: string, context: Context, ?filename: string): unit = failwith "JS only"
        member __.createContext(?initSandbox: Context): Context = failwith "JS only"
        member __.createScript(code: string, ?filename: string): Script = failwith "JS only"

let [<Import("*","vm")>] vm: vm.Globals = failwith "JS only"


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
        member __.parse(urlStr: string, ?parseQueryString: bool, ?slashesDenoteHost: bool): Url = failwith "JS only"
        member __.format(url: UrlOptions): string = failwith "JS only"
        member __.resolve(from: string, ``to``: string): string = failwith "JS only"

let [<Import("*","url")>] url: url.Globals = failwith "JS only"


module dns =
    type Globals =
        member __.lookup(domain: string, family: float, callback: Func<Error, string, float, unit>): string = failwith "JS only"
        member __.lookup(domain: string, callback: Func<Error, string, float, unit>): string = failwith "JS only"
        member __.resolve(domain: string, rrtype: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolve(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolve4(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolve6(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolveMx(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolveTxt(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolveSrv(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolveNs(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.resolveCname(domain: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"
        member __.reverse(ip: string, callback: Func<Error, ResizeArray<string>, unit>): ResizeArray<string> = failwith "JS only"

let [<Import("*","dns")>] dns: dns.Globals = failwith "JS only"


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
        abstract bind: port: float * ?address: string * ?callback: Func<unit, unit> -> unit
        abstract close: unit -> unit
        abstract address: unit -> AddressInfo
        abstract setBroadcast: flag: bool -> unit
        abstract setMulticastTTL: ttl: float -> unit
        abstract setMulticastLoopback: flag: bool -> unit
        abstract addMembership: multicastAddress: string * ?multicastInterface: string -> unit
        abstract dropMembership: multicastAddress: string * ?multicastInterface: string -> unit

    type Globals =
        member __.createSocket(``type``: string, ?callback: Func<Buffer, RemoteInfo, unit>): Socket = failwith "JS only"

let [<Import("*","dgram")>] dgram: dgram.Globals = failwith "JS only"


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

    type Globals =
        member __.F_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.R_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.W_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.X_OK with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
        member __.rename(oldPath: string, newPath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.renameSync(oldPath: string, newPath: string): unit = failwith "JS only"
        member __.truncate(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.truncate(path: string, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.truncateSync(path: string, ?len: float): unit = failwith "JS only"
        member __.ftruncate(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.ftruncate(fd: float, len: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.ftruncateSync(fd: float, ?len: float): unit = failwith "JS only"
        member __.chown(path: string, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.chownSync(path: string, uid: float, gid: float): unit = failwith "JS only"
        member __.fchown(fd: float, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.fchownSync(fd: float, uid: float, gid: float): unit = failwith "JS only"
        member __.lchown(path: string, uid: float, gid: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.lchownSync(path: string, uid: float, gid: float): unit = failwith "JS only"
        member __.chmod(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.chmod(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.chmodSync(path: string, mode: float): unit = failwith "JS only"
        member __.chmodSync(path: string, mode: string): unit = failwith "JS only"
        member __.fchmod(fd: float, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.fchmod(fd: float, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.fchmodSync(fd: float, mode: float): unit = failwith "JS only"
        member __.fchmodSync(fd: float, mode: string): unit = failwith "JS only"
        member __.lchmod(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.lchmod(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.lchmodSync(path: string, mode: float): unit = failwith "JS only"
        member __.lchmodSync(path: string, mode: string): unit = failwith "JS only"
        member __.stat(path: string, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = failwith "JS only"
        member __.lstat(path: string, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = failwith "JS only"
        member __.fstat(fd: float, ?callback: Func<NodeJS.ErrnoException, Stats, obj>): unit = failwith "JS only"
        member __.statSync(path: string): Stats = failwith "JS only"
        member __.lstatSync(path: string): Stats = failwith "JS only"
        member __.fstatSync(fd: float): Stats = failwith "JS only"
        member __.link(srcpath: string, dstpath: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.linkSync(srcpath: string, dstpath: string): unit = failwith "JS only"
        member __.symlink(srcpath: string, dstpath: string, ?``type``: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.symlinkSync(srcpath: string, dstpath: string, ?``type``: string): unit = failwith "JS only"
        member __.readlink(path: string, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = failwith "JS only"
        member __.readlinkSync(path: string): string = failwith "JS only"
        member __.realpath(path: string, ?callback: Func<NodeJS.ErrnoException, string, obj>): unit = failwith "JS only"
        member __.realpath(path: string, cache: obj, callback: Func<NodeJS.ErrnoException, string, obj>): unit = failwith "JS only"
        member __.realpathSync(path: string, ?cache: obj): string = failwith "JS only"
        member __.unlink(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.unlinkSync(path: string): unit = failwith "JS only"
        member __.rmdir(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.rmdirSync(path: string): unit = failwith "JS only"
        member __.mkdir(path: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.mkdir(path: string, mode: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.mkdir(path: string, mode: string, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.mkdirSync(path: string, ?mode: float): unit = failwith "JS only"
        member __.mkdirSync(path: string, ?mode: string): unit = failwith "JS only"
        member __.readdir(path: string, ?callback: Func<NodeJS.ErrnoException, ResizeArray<string>, unit>): unit = failwith "JS only"
        member __.readdirSync(path: string): ResizeArray<string> = failwith "JS only"
        member __.close(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.closeSync(fd: float): unit = failwith "JS only"
        member __.``open``(path: string, flags: string, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = failwith "JS only"
        member __.``open``(path: string, flags: string, mode: float, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = failwith "JS only"
        member __.``open``(path: string, flags: string, mode: string, ?callback: Func<NodeJS.ErrnoException, float, obj>): unit = failwith "JS only"
        member __.openSync(path: string, flags: string, ?mode: float): float = failwith "JS only"
        member __.openSync(path: string, flags: string, ?mode: string): float = failwith "JS only"
        member __.utimes(path: string, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.utimes(path: string, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.utimesSync(path: string, atime: float, mtime: float): unit = failwith "JS only"
        member __.utimesSync(path: string, atime: DateTime, mtime: DateTime): unit = failwith "JS only"
        member __.futimes(fd: float, atime: float, mtime: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.futimes(fd: float, atime: DateTime, mtime: DateTime, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.futimesSync(fd: float, atime: float, mtime: float): unit = failwith "JS only"
        member __.futimesSync(fd: float, atime: DateTime, mtime: DateTime): unit = failwith "JS only"
        member __.fsync(fd: float, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.fsyncSync(fd: float): unit = failwith "JS only"
        member __.write(fd: float, buffer: Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = failwith "JS only"
        member __.write(fd: float, buffer: Buffer, offset: float, length: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = failwith "JS only"
        member __.write(fd: float, data: obj, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = failwith "JS only"
        member __.write(fd: float, data: obj, offset: float, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = failwith "JS only"
        member __.write(fd: float, data: obj, offset: float, encoding: string, ?callback: Func<NodeJS.ErrnoException, float, string, unit>): unit = failwith "JS only"
        member __.writeSync(fd: float, buffer: Buffer, offset: float, length: float, position: float): float = failwith "JS only"
        member __.read(fd: float, buffer: Buffer, offset: float, length: float, position: float, ?callback: Func<NodeJS.ErrnoException, float, Buffer, unit>): unit = failwith "JS only"
        member __.readSync(fd: float, buffer: Buffer, offset: float, length: float, position: float): float = failwith "JS only"
        member __.readFile(filename: string, encoding: string, callback: Func<NodeJS.ErrnoException, string, unit>): unit = failwith "JS only"
        member __.readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, string, unit>): unit = failwith "JS only"
        member __.readFile(filename: string, options: obj, callback: Func<NodeJS.ErrnoException, Buffer, unit>): unit = failwith "JS only"
        member __.readFile(filename: string, callback: Func<NodeJS.ErrnoException, Buffer, unit>): unit = failwith "JS only"
        member __.readFileSync(filename: string, encoding: string): string = failwith "JS only"
        member __.readFileSync(filename: string, options: obj): string = failwith "JS only"
        member __.readFileSync(filename: string, ?options: obj): Buffer = failwith "JS only"
        member __.writeFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.writeFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.writeFileSync(filename: string, data: obj, ?options: obj): unit = failwith "JS only"
        member __.appendFile(filename: string, data: obj, options: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.appendFile(filename: string, data: obj, ?callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.appendFileSync(filename: string, data: obj, ?options: obj): unit = failwith "JS only"
        member __.watchFile(filename: string, listener: Func<Stats, Stats, unit>): unit = failwith "JS only"
        member __.watchFile(filename: string, options: obj, listener: Func<Stats, Stats, unit>): unit = failwith "JS only"
        member __.unwatchFile(filename: string, ?listener: Func<Stats, Stats, unit>): unit = failwith "JS only"
        member __.watch(filename: string, ?listener: Func<string, string, obj>): FSWatcher = failwith "JS only"
        member __.watch(filename: string, options: obj, ?listener: Func<string, string, obj>): FSWatcher = failwith "JS only"
        member __.exists(path: string, ?callback: Func<bool, unit>): unit = failwith "JS only"
        member __.existsSync(path: string): bool = failwith "JS only"
        member __.access(path: string, callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.access(path: string, mode: float, callback: Func<NodeJS.ErrnoException, unit>): unit = failwith "JS only"
        member __.accessSync(path: string, ?mode: float): unit = failwith "JS only"
        member __.createReadStream(path: string, ?options: obj): ReadStream = failwith "JS only"
        member __.createWriteStream(path: string, ?options: obj): WriteStream = failwith "JS only"

let [<Import("*","fs")>] fs: fs.Globals = failwith "JS only"


module path =
    type ParsedPath =
        abstract root: string with get, set
        abstract dir: string with get, set
        abstract ``base``: string with get, set
        abstract ext: string with get, set
        abstract name: string with get, set

    type Globals =
        member __.sep with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.delimiter with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.normalize(p: string): string = failwith "JS only"
        member __.join([<ParamArray>] paths: obj[]): string = failwith "JS only"
        member __.join([<ParamArray>] paths: string[]): string = failwith "JS only"
        member __.resolve([<ParamArray>] pathSegments: obj[]): string = failwith "JS only"
        member __.isAbsolute(path: string): bool = failwith "JS only"
        member __.relative(from: string, ``to``: string): string = failwith "JS only"
        member __.dirname(p: string): string = failwith "JS only"
        member __.basename(p: string, ?ext: string): string = failwith "JS only"
        member __.extname(p: string): string = failwith "JS only"
        member __.parse(pathString: string): ParsedPath = failwith "JS only"
        member __.format(pathObject: ParsedPath): string = failwith "JS only"

    module posix =
        type Globals =
            member __.sep with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.delimiter with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.normalize(p: string): string = failwith "JS only"
            member __.join([<ParamArray>] paths: obj[]): string = failwith "JS only"
            member __.resolve([<ParamArray>] pathSegments: obj[]): string = failwith "JS only"
            member __.isAbsolute(p: string): bool = failwith "JS only"
            member __.relative(from: string, ``to``: string): string = failwith "JS only"
            member __.dirname(p: string): string = failwith "JS only"
            member __.basename(p: string, ?ext: string): string = failwith "JS only"
            member __.extname(p: string): string = failwith "JS only"
            member __.parse(p: string): ParsedPath = failwith "JS only"
            member __.format(pP: ParsedPath): string = failwith "JS only"
            
    let [<Import("posix","path")>] posix: posix.Globals = failwith "JS only"

    module win32 =
        type Globals =
            member __.sep with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.delimiter with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.normalize(p: string): string = failwith "JS only"
            member __.join([<ParamArray>] paths: obj[]): string = failwith "JS only"
            member __.resolve([<ParamArray>] pathSegments: obj[]): string = failwith "JS only"
            member __.isAbsolute(p: string): bool = failwith "JS only"
            member __.relative(from: string, ``to``: string): string = failwith "JS only"
            member __.dirname(p: string): string = failwith "JS only"
            member __.basename(p: string, ?ext: string): string = failwith "JS only"
            member __.extname(p: string): string = failwith "JS only"
            member __.parse(p: string): ParsedPath = failwith "JS only"
            member __.format(pP: ParsedPath): string = failwith "JS only"

    let [<Import("win32","path")>] win32: win32.Globals = failwith "JS only"

let [<Import("*","path")>] path: path.Globals = failwith "JS only"


[<Import("*","string_decoder")>]
module string_decoder =
    type NodeStringDecoder =
        abstract write: buffer: Buffer -> string
        abstract detectIncompleteChar: buffer: Buffer -> float

    let StringDecoder: NodeStringDecoder = failwith "JS only"


module util =
    type InspectOptions =
        abstract showHidden: bool option with get, set
        abstract depth: float option with get, set
        abstract colors: bool option with get, set
        abstract customInspect: bool option with get, set

    type Globals =
        member __.format(format: obj, [<ParamArray>] param: obj[]): string = failwith "JS only"
        member __.debug(string: string): unit = failwith "JS only"
        member __.error([<ParamArray>] param: obj[]): unit = failwith "JS only"
        member __.puts([<ParamArray>] param: obj[]): unit = failwith "JS only"
        member __.print([<ParamArray>] param: obj[]): unit = failwith "JS only"
        member __.log(string: string): unit = failwith "JS only"
        member __.inspect(``object``: obj, ?showHidden: bool, ?depth: float, ?color: bool): string = failwith "JS only"
        member __.inspect(``object``: obj, options: InspectOptions): string = failwith "JS only"
        member __.isArray(``object``: obj): bool = failwith "JS only"
        member __.isRegExp(``object``: obj): bool = failwith "JS only"
        member __.isDate(``object``: obj): bool = failwith "JS only"
        member __.isError(``object``: obj): bool = failwith "JS only"
        member __.inherits(``constructor``: obj, superConstructor: obj): unit = failwith "JS only"
        member __.debuglog(key: string): Func<string, obj, unit> = failwith "JS only"

let [<Import("*","util")>] util: util.Globals = failwith "JS only"


module ``assert`` =
    type [<Import("AssertionError","assert")>] AssertionError(?options: obj) =
        // interface Error
        member __.name with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.message with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.actual with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.expected with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.operator with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        member __.generatedMessage with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"

    type Globals =
        member __.throws with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.doesNotThrow with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.fail(?actual: obj, ?expected: obj, ?message: string, ?operator: string): unit = failwith "JS only"
        member __.ok(value: obj, ?message: string): unit = failwith "JS only"
        member __.equal(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        member __.notEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        member __.deepEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        member __.notDeepEqual(acutal: obj, expected: obj, ?message: string): unit = failwith "JS only"
        member __.strictEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        member __.notStrictEqual(actual: obj, expected: obj, ?message: string): unit = failwith "JS only"
        member __.ifError(value: obj): unit = failwith "JS only"

let [<Import("*","assert")>] ``assert``: ``assert``.Globals = failwith "JS only"


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


[<Import("*","constants")>]
module constants =
    let E2BIG: float = failwith "JS only"
    let EACCES: float = failwith "JS only"
    let EADDRINUSE: float = failwith "JS only"
    let EADDRNOTAVAIL: float = failwith "JS only"
    let EAFNOSUPPORT: float = failwith "JS only"
    let EAGAIN: float = failwith "JS only"
    let EALREADY: float = failwith "JS only"
    let EBADF: float = failwith "JS only"
    let EBADMSG: float = failwith "JS only"
    let EBUSY: float = failwith "JS only"
    let ECANCELED: float = failwith "JS only"
    let ECHILD: float = failwith "JS only"
    let ECONNABORTED: float = failwith "JS only"
    let ECONNREFUSED: float = failwith "JS only"
    let ECONNRESET: float = failwith "JS only"
    let EDEADLK: float = failwith "JS only"
    let EDESTADDRREQ: float = failwith "JS only"
    let EDOM: float = failwith "JS only"
    let EEXIST: float = failwith "JS only"
    let EFAULT: float = failwith "JS only"
    let EFBIG: float = failwith "JS only"
    let EHOSTUNREACH: float = failwith "JS only"
    let EIDRM: float = failwith "JS only"
    let EILSEQ: float = failwith "JS only"
    let EINPROGRESS: float = failwith "JS only"
    let EINTR: float = failwith "JS only"
    let EINVAL: float = failwith "JS only"
    let EIO: float = failwith "JS only"
    let EISCONN: float = failwith "JS only"
    let EISDIR: float = failwith "JS only"
    let ELOOP: float = failwith "JS only"
    let EMFILE: float = failwith "JS only"
    let EMLINK: float = failwith "JS only"
    let EMSGSIZE: float = failwith "JS only"
    let ENAMETOOLONG: float = failwith "JS only"
    let ENETDOWN: float = failwith "JS only"
    let ENETRESET: float = failwith "JS only"
    let ENETUNREACH: float = failwith "JS only"
    let ENFILE: float = failwith "JS only"
    let ENOBUFS: float = failwith "JS only"
    let ENODATA: float = failwith "JS only"
    let ENODEV: float = failwith "JS only"
    let ENOENT: float = failwith "JS only"
    let ENOEXEC: float = failwith "JS only"
    let ENOLCK: float = failwith "JS only"
    let ENOLINK: float = failwith "JS only"
    let ENOMEM: float = failwith "JS only"
    let ENOMSG: float = failwith "JS only"
    let ENOPROTOOPT: float = failwith "JS only"
    let ENOSPC: float = failwith "JS only"
    let ENOSR: float = failwith "JS only"
    let ENOSTR: float = failwith "JS only"
    let ENOSYS: float = failwith "JS only"
    let ENOTCONN: float = failwith "JS only"
    let ENOTDIR: float = failwith "JS only"
    let ENOTEMPTY: float = failwith "JS only"
    let ENOTSOCK: float = failwith "JS only"
    let ENOTSUP: float = failwith "JS only"
    let ENOTTY: float = failwith "JS only"
    let ENXIO: float = failwith "JS only"
    let EOPNOTSUPP: float = failwith "JS only"
    let EOVERFLOW: float = failwith "JS only"
    let EPERM: float = failwith "JS only"
    let EPIPE: float = failwith "JS only"
    let EPROTO: float = failwith "JS only"
    let EPROTONOSUPPORT: float = failwith "JS only"
    let EPROTOTYPE: float = failwith "JS only"
    let ERANGE: float = failwith "JS only"
    let EROFS: float = failwith "JS only"
    let ESPIPE: float = failwith "JS only"
    let ESRCH: float = failwith "JS only"
    let ETIME: float = failwith "JS only"
    let ETIMEDOUT: float = failwith "JS only"
    let ETXTBSY: float = failwith "JS only"
    let EWOULDBLOCK: float = failwith "JS only"
    let EXDEV: float = failwith "JS only"
    let WSAEINTR: float = failwith "JS only"
    let WSAEBADF: float = failwith "JS only"
    let WSAEACCES: float = failwith "JS only"
    let WSAEFAULT: float = failwith "JS only"
    let WSAEINVAL: float = failwith "JS only"
    let WSAEMFILE: float = failwith "JS only"
    let WSAEWOULDBLOCK: float = failwith "JS only"
    let WSAEINPROGRESS: float = failwith "JS only"
    let WSAEALREADY: float = failwith "JS only"
    let WSAENOTSOCK: float = failwith "JS only"
    let WSAEDESTADDRREQ: float = failwith "JS only"
    let WSAEMSGSIZE: float = failwith "JS only"
    let WSAEPROTOTYPE: float = failwith "JS only"
    let WSAENOPROTOOPT: float = failwith "JS only"
    let WSAEPROTONOSUPPORT: float = failwith "JS only"
    let WSAESOCKTNOSUPPORT: float = failwith "JS only"
    let WSAEOPNOTSUPP: float = failwith "JS only"
    let WSAEPFNOSUPPORT: float = failwith "JS only"
    let WSAEAFNOSUPPORT: float = failwith "JS only"
    let WSAEADDRINUSE: float = failwith "JS only"
    let WSAEADDRNOTAVAIL: float = failwith "JS only"
    let WSAENETDOWN: float = failwith "JS only"
    let WSAENETUNREACH: float = failwith "JS only"
    let WSAENETRESET: float = failwith "JS only"
    let WSAECONNABORTED: float = failwith "JS only"
    let WSAECONNRESET: float = failwith "JS only"
    let WSAENOBUFS: float = failwith "JS only"
    let WSAEISCONN: float = failwith "JS only"
    let WSAENOTCONN: float = failwith "JS only"
    let WSAESHUTDOWN: float = failwith "JS only"
    let WSAETOOMANYREFS: float = failwith "JS only"
    let WSAETIMEDOUT: float = failwith "JS only"
    let WSAECONNREFUSED: float = failwith "JS only"
    let WSAELOOP: float = failwith "JS only"
    let WSAENAMETOOLONG: float = failwith "JS only"
    let WSAEHOSTDOWN: float = failwith "JS only"
    let WSAEHOSTUNREACH: float = failwith "JS only"
    let WSAENOTEMPTY: float = failwith "JS only"
    let WSAEPROCLIM: float = failwith "JS only"
    let WSAEUSERS: float = failwith "JS only"
    let WSAEDQUOT: float = failwith "JS only"
    let WSAESTALE: float = failwith "JS only"
    let WSAEREMOTE: float = failwith "JS only"
    let WSASYSNOTREADY: float = failwith "JS only"
    let WSAVERNOTSUPPORTED: float = failwith "JS only"
    let WSANOTINITIALISED: float = failwith "JS only"
    let WSAEDISCON: float = failwith "JS only"
    let WSAENOMORE: float = failwith "JS only"
    let WSAECANCELLED: float = failwith "JS only"
    let WSAEINVALIDPROCTABLE: float = failwith "JS only"
    let WSAEINVALIDPROVIDER: float = failwith "JS only"
    let WSAEPROVIDERFAILEDINIT: float = failwith "JS only"
    let WSASYSCALLFAILURE: float = failwith "JS only"
    let WSASERVICE_NOT_FOUND: float = failwith "JS only"
    let WSATYPE_NOT_FOUND: float = failwith "JS only"
    let WSA_E_NO_MORE: float = failwith "JS only"
    let WSA_E_CANCELLED: float = failwith "JS only"
    let WSAEREFUSED: float = failwith "JS only"
    let SIGHUP: float = failwith "JS only"
    let SIGINT: float = failwith "JS only"
    let SIGILL: float = failwith "JS only"
    let SIGABRT: float = failwith "JS only"
    let SIGFPE: float = failwith "JS only"
    let SIGKILL: float = failwith "JS only"
    let SIGSEGV: float = failwith "JS only"
    let SIGTERM: float = failwith "JS only"
    let SIGBREAK: float = failwith "JS only"
    let SIGWINCH: float = failwith "JS only"
    let SSL_OP_ALL: float = failwith "JS only"
    let SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION: float = failwith "JS only"
    let SSL_OP_CIPHER_SERVER_PREFERENCE: float = failwith "JS only"
    let SSL_OP_CISCO_ANYCONNECT: float = failwith "JS only"
    let SSL_OP_COOKIE_EXCHANGE: float = failwith "JS only"
    let SSL_OP_CRYPTOPRO_TLSEXT_BUG: float = failwith "JS only"
    let SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS: float = failwith "JS only"
    let SSL_OP_EPHEMERAL_RSA: float = failwith "JS only"
    let SSL_OP_LEGACY_SERVER_CONNECT: float = failwith "JS only"
    let SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER: float = failwith "JS only"
    let SSL_OP_MICROSOFT_SESS_ID_BUG: float = failwith "JS only"
    let SSL_OP_MSIE_SSLV2_RSA_PADDING: float = failwith "JS only"
    let SSL_OP_NETSCAPE_CA_DN_BUG: float = failwith "JS only"
    let SSL_OP_NETSCAPE_CHALLENGE_BUG: float = failwith "JS only"
    let SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG: float = failwith "JS only"
    let SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG: float = failwith "JS only"
    let SSL_OP_NO_COMPRESSION: float = failwith "JS only"
    let SSL_OP_NO_QUERY_MTU: float = failwith "JS only"
    let SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION: float = failwith "JS only"
    let SSL_OP_NO_SSLv2: float = failwith "JS only"
    let SSL_OP_NO_SSLv3: float = failwith "JS only"
    let SSL_OP_NO_TICKET: float = failwith "JS only"
    let SSL_OP_NO_TLSv1: float = failwith "JS only"
    let SSL_OP_NO_TLSv1_1: float = failwith "JS only"
    let SSL_OP_NO_TLSv1_2: float = failwith "JS only"
    let SSL_OP_PKCS1_CHECK_1: float = failwith "JS only"
    let SSL_OP_PKCS1_CHECK_2: float = failwith "JS only"
    let SSL_OP_SINGLE_DH_USE: float = failwith "JS only"
    let SSL_OP_SINGLE_ECDH_USE: float = failwith "JS only"
    let SSL_OP_SSLEAY_080_CLIENT_DH_BUG: float = failwith "JS only"
    let SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG: float = failwith "JS only"
    let SSL_OP_TLS_BLOCK_PADDING_BUG: float = failwith "JS only"
    let SSL_OP_TLS_D5_BUG: float = failwith "JS only"
    let SSL_OP_TLS_ROLLBACK_BUG: float = failwith "JS only"
    let ENGINE_METHOD_DSA: float = failwith "JS only"
    let ENGINE_METHOD_DH: float = failwith "JS only"
    let ENGINE_METHOD_RAND: float = failwith "JS only"
    let ENGINE_METHOD_ECDH: float = failwith "JS only"
    let ENGINE_METHOD_ECDSA: float = failwith "JS only"
    let ENGINE_METHOD_CIPHERS: float = failwith "JS only"
    let ENGINE_METHOD_DIGESTS: float = failwith "JS only"
    let ENGINE_METHOD_STORE: float = failwith "JS only"
    let ENGINE_METHOD_PKEY_METHS: float = failwith "JS only"
    let ENGINE_METHOD_PKEY_ASN1_METHS: float = failwith "JS only"
    let ENGINE_METHOD_ALL: float = failwith "JS only"
    let ENGINE_METHOD_NONE: float = failwith "JS only"
    let DH_CHECK_P_NOT_SAFE_PRIME: float = failwith "JS only"
    let DH_CHECK_P_NOT_PRIME: float = failwith "JS only"
    let DH_UNABLE_TO_CHECK_GENERATOR: float = failwith "JS only"
    let DH_NOT_SUITABLE_GENERATOR: float = failwith "JS only"
    let NPN_ENABLED: float = failwith "JS only"
    let RSA_PKCS1_PADDING: float = failwith "JS only"
    let RSA_SSLV23_PADDING: float = failwith "JS only"
    let RSA_NO_PADDING: float = failwith "JS only"
    let RSA_PKCS1_OAEP_PADDING: float = failwith "JS only"
    let RSA_X931_PADDING: float = failwith "JS only"
    let RSA_PKCS1_PSS_PADDING: float = failwith "JS only"
    let POINT_CONVERSION_COMPRESSED: float = failwith "JS only"
    let POINT_CONVERSION_UNCOMPRESSED: float = failwith "JS only"
    let POINT_CONVERSION_HYBRID: float = failwith "JS only"
    let O_RDONLY: float = failwith "JS only"
    let O_WRONLY: float = failwith "JS only"
    let O_RDWR: float = failwith "JS only"
    let S_IFMT: float = failwith "JS only"
    let S_IFREG: float = failwith "JS only"
    let S_IFDIR: float = failwith "JS only"
    let S_IFCHR: float = failwith "JS only"
    let S_IFLNK: float = failwith "JS only"
    let O_CREAT: float = failwith "JS only"
    let O_EXCL: float = failwith "JS only"
    let O_TRUNC: float = failwith "JS only"
    let O_APPEND: float = failwith "JS only"
    let F_OK: float = failwith "JS only"
    let R_OK: float = failwith "JS only"
    let W_OK: float = failwith "JS only"
    let X_OK: float = failwith "JS only"
    let UV_UDP_REUSEADDR: float = failwith "JS only"
