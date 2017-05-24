module rec Fable.Import.Node.Https

open Fable.Core
open Fable.Import.JS
open Fable.Import.Node

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
    abstract SNICallback: (string -> (Error -> Tls.SecureContext -> obj) -> obj) option with get, set

type [<AllowNullLiteral>] RequestOptions =
    inherit Http.RequestOptions
    abstract pfx: obj option with get, set
    abstract key: obj option with get, set
    abstract passphrase: string option with get, set
    abstract cert: obj option with get, set
    abstract ca: obj option with get, set
    abstract ciphers: string option with get, set
    abstract rejectUnauthorized: bool option with get, set
    abstract secureProtocol: string option with get, set

type [<AllowNullLiteral>] Agent =
    abstract maxSockets: float with get, set
    abstract sockets: obj with get, set
    abstract requests: obj with get, set
    abstract destroy: unit -> unit

type [<AllowNullLiteral>] AgentOptions =
    inherit Http.AgentOptions
    abstract pfx: obj option with get, set
    abstract key: obj option with get, set
    abstract passphrase: string option with get, set
    abstract cert: obj option with get, set
    abstract ca: obj option with get, set
    abstract ciphers: string option with get, set
    abstract rejectUnauthorized: bool option with get, set
    abstract secureProtocol: string option with get, set
    abstract maxCachedSessions: float option with get, set

type [<AllowNullLiteral>] AgentStatic =
  [<Emit("new $0($1...)")>] abstract Create: ?options: AgentOptions -> Agent
    
type [<AllowNullLiteral>] Server =
    inherit Tls.Server

type IExports =
    abstract Agent: AgentStatic with get, set
    abstract globalAgent: Agent with get, set
    abstract createServer: options: ServerOptions * ?requestListener: (Http.IncomingMessage -> Http.ServerResponse -> unit) -> Server
    abstract request: options: RequestOptions * ?callback: (Http.IncomingMessage -> unit) -> Http.ClientRequest
    abstract get: options: RequestOptions * ?callback: (Http.IncomingMessage -> unit) -> Http.ClientRequest