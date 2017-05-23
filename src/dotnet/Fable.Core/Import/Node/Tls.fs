module rec Fable.Import.Node.Tls

open Fable.Import.Node
open Fable.Import.JS


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
    abstract SNICallback: (string -> obj) option with get, set

type [<AllowNullLiteral>] ConnectionOptions =
    abstract host: string option with get, set
    abstract port: float option with get, set
    abstract socket: Net.Socket option with get, set
    abstract pfx: obj option with get, set
    abstract key: obj option with get, set
    abstract passphrase: string option with get, set
    abstract cert: obj option with get, set
    abstract ca: obj option with get, set
    abstract rejectUnauthorized: bool option with get, set
    abstract NPNProtocols: obj option with get, set
    abstract servername: string option with get, set

type [<AllowNullLiteral>] Server =
    inherit Net.Server
    abstract maxConnections: float with get, set
    abstract connections: float with get, set
    abstract listen: port: float * ?host: string * ?backlog: float * ?listeningListener: Function -> Server
    abstract listen: path: string * ?listeningListener: Function -> Server
    abstract listen: handle: obj * ?listeningListener: Function -> Server
    abstract listen: port: float * ?host: string * ?callback: Function -> Server
    abstract close: unit -> Server
    abstract address: unit -> obj
    abstract addContext: hostName: string * credentials: obj -> unit

type [<AllowNullLiteral>] ClearTextStream =
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

type [<AllowNullLiteral>] SecurePair =
    abstract encrypted: obj with get, set
    abstract cleartext: obj with get, set

type [<AllowNullLiteral>] SecureContextOptions =
    abstract pfx: obj option with get, set
    abstract key: obj option with get, set
    abstract passphrase: string option with get, set
    abstract cert: obj option with get, set
    abstract ca: obj option with get, set
    abstract crl: obj option with get, set
    abstract ciphers: string option with get, set
    abstract honorCipherOrder: bool option with get, set

type [<AllowNullLiteral>] SecureContext =
    abstract context: obj with get, set

type IExports =
    abstract CLIENT_RENEG_LIMIT: float with get, set
    abstract CLIENT_RENEG_WINDOW: float with get, set
    abstract createServer: options: TlsOptions * ?secureConnectionListener: (ClearTextStream -> unit) -> Server
    abstract connect: options: TlsOptions * ?secureConnectionListener: (unit -> unit) -> ClearTextStream
    abstract connect: port: float * ?host: string * ?options: ConnectionOptions * ?secureConnectListener: (unit -> unit) -> ClearTextStream
    abstract connect: port: float * ?options: ConnectionOptions * ?secureConnectListener: (unit -> unit) -> ClearTextStream
    abstract createSecurePair: ?credentials: Crypto.Credentials * ?isServer: bool * ?requestCert: bool * ?rejectUnauthorized: bool -> SecurePair
    abstract createSecureContext: details: SecureContextOptions -> SecureContext
