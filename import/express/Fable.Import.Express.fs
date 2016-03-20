namespace Fable.Import
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS
open Fable.Import.Node

module Express =
    type Request =
        interface end

    and Response =
        interface end

    and Application =
        interface end

module express =
    type IRoute =
        abstract path: string with get, set
        abstract stack: obj with get, set
        abstract all: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract get: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract post: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract put: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract delete: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract patch: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract options: [<ParamArray>] handler: RequestHandler[] -> IRoute
        abstract head: [<ParamArray>] handler: RequestHandler[] -> IRoute

    and IRouter<'T> =
        // inherit RequestHandler
        abstract all: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract get: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract post: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract put: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract delete: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract patch: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract options: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract head: name: U2<string, Regex> * [<ParamArray>] handlers: RequestHandler[] -> obj
        abstract param: name: string * handler: RequestParamHandler -> 'T
        abstract param: name: string * matcher: Regex -> 'T
        abstract param: name: string * mapper: Func<obj, obj> -> 'T
        abstract param: callback: Func<string, Regex, RequestParamHandler> -> 'T
        abstract route: path: string -> IRoute
        abstract ``use``: [<ParamArray>] handler: RequestHandler[] -> 'T
        // abstract ``use``: handler: U2<ErrorRequestHandler, RequestHandler> -> 'T
        abstract ``use``: path: string * [<ParamArray>] handler: RequestHandler[] -> 'T
        // abstract ``use``: path: string * handler: U2<ErrorRequestHandler, RequestHandler> -> 'T
        abstract ``use``: path: string[] * [<ParamArray>] handler: RequestHandler[] -> 'T
        // abstract ``use``: path: ResizeArray<string> * handler: ErrorRequestHandler -> 'T
        abstract ``use``: path: Regex * [<ParamArray>] handler: RequestHandler[] -> 'T
        // abstract ``use``: path: Regex * handler: ErrorRequestHandler -> 'T
        abstract ``use``: path: string * router: Router -> 'T

    and Router =
        inherit IRouter<Router>


    and CookieOptions =
        abstract maxAge: float option with get, set
        abstract signed: bool option with get, set
        abstract expires: DateTime option with get, set
        abstract httpOnly: bool option with get, set
        abstract path: string option with get, set
        abstract domain: string option with get, set
        abstract secure: bool option with get, set

    and Errback = Func<Error, unit>
        // [<Emit("$0($1...)")>] abstract Invoke: err: Error -> unit

    and Request =
        inherit http.ServerRequest
        inherit Express.Request
        abstract headers: obj with get, set
        abstract accepted: ResizeArray<MediaType> with get, set
        abstract protocol: string with get, set
        abstract secure: bool with get, set
        abstract ip: string with get, set
        abstract ips: ResizeArray<string> with get, set
        abstract subdomains: ResizeArray<string> with get, set
        abstract path: string with get, set
        abstract hostname: string with get, set
        abstract host: string with get, set
        abstract fresh: bool with get, set
        abstract stale: bool with get, set
        abstract xhr: bool with get, set
        abstract body: obj with get, set
        abstract cookies: obj with get, set
        abstract ``method``: string with get, set
        abstract ``params``: obj with get, set
        abstract user: obj with get, set
        abstract authenticatedUser: obj with get, set
        abstract query: obj with get, set
        abstract route: obj with get, set
        abstract signedCookies: obj with get, set
        abstract originalUrl: string with get, set
        abstract url: string with get, set
        abstract baseUrl: string with get, set
        abstract app: Application with get, set
        abstract get: name: string -> string
        abstract header: name: string -> string
        abstract accepts: ``type``: string -> string
        abstract accepts: ``type``: ResizeArray<string> -> string
        abstract acceptsCharsets: ?charset: U2<string, ResizeArray<string>> -> ResizeArray<string>
        abstract acceptsEncodings: ?encoding: U2<string, ResizeArray<string>> -> ResizeArray<string>
        abstract acceptsLanguages: ?lang: U2<string, ResizeArray<string>> -> ResizeArray<string>
        abstract range: size: float -> ResizeArray<obj>
        abstract param: name: string * ?defaultValue: obj -> string
        abstract is: ``type``: string -> bool
        abstract clearCookie: name: string * ?options: obj -> Response

    and MediaType =
        abstract value: string with get, set
        abstract quality: float with get, set
        abstract ``type``: string with get, set
        abstract subtype: string with get, set

    and Response =
        inherit http.ServerResponse
        inherit Express.Response
        abstract send: body: obj -> Response
        abstract json: body: obj -> Response
        abstract jsonp: body: obj -> Response
        abstract send: status: float * ?body: obj -> Response
        abstract json: status: float * ?body: obj -> Response
        abstract jsonp: status: float * ?body: obj -> Response
        abstract headersSent: bool with get, set
        abstract locals: obj with get, set
        abstract charset: string with get, set
        abstract status: code: float -> Response
        abstract sendStatus: code: float -> Response
        abstract links: links: obj -> Response
        abstract sendFile: path: string -> unit
        abstract sendFile: path: string * options: obj -> unit
        abstract sendFile: path: string * fn: Errback -> unit
        abstract sendFile: path: string * options: obj * fn: Errback -> unit
        abstract sendfile: path: string -> unit
        abstract sendfile: path: string * options: obj -> unit
        abstract sendfile: path: string * fn: Errback -> unit
        abstract sendfile: path: string * options: obj * fn: Errback -> unit
        abstract download: path: string -> unit
        abstract download: path: string * filename: string -> unit
        abstract download: path: string * fn: Errback -> unit
        abstract download: path: string * filename: string * fn: Errback -> unit
        abstract contentType: ``type``: string -> Response
        abstract ``type``: ``type``: string -> Response
        abstract format: obj: obj -> Response
        abstract attachment: ?filename: string -> Response
        abstract set: field: obj -> Response
        abstract set: field: string * ?value: string -> Response
        abstract header: field: obj -> Response
        abstract header: field: string * ?value: string -> Response
        abstract get: field: string -> string
        abstract clearCookie: name: string * ?options: obj -> Response
        abstract cookie: name: string * ``val``: string * options: CookieOptions -> Response
        abstract cookie: name: string * ``val``: obj * options: CookieOptions -> Response
        abstract cookie: name: string * ``val``: obj -> Response
        abstract location: url: string -> Response
        abstract redirect: url: string -> unit
        abstract redirect: status: float * url: string -> unit
        abstract redirect: url: string * status: float -> unit
        abstract render: view: string * ?options: obj * ?callback: Func<Error, string, unit> -> unit
        abstract render: view: string * ?callback: Func<Error, string, unit> -> unit

    and NextFunction = Func<obj,unit>
        // [<Emit("$0($1...)")>] abstract Invoke: unit -> unit
        // [<Emit("$0($1...)")>] abstract Invoke: err: obj -> unit

    and ErrorRequestHandler = Func<obj, Request, Response, (obj->unit), obj>
        // [<Emit("$0($1...)")>] abstract Invoke: err: obj * req: Request * res: Response * next: NextFunction -> obj

    and RequestHandler = Func<Request, Response, (obj->unit), obj>
        // [<Emit("$0($1...)")>] abstract Invoke: req: Request * res: Response * next: NextFunction -> obj

    and Handler = RequestHandler
    //     inherit RequestHandler

    and RequestParamHandler = Func<Request, Response, (obj->unit), obj, obj>
        // [<Emit("$0($1...)")>] abstract Invoke: req: Request * res: Response * next: NextFunction * param: obj -> obj

    and Application =
        inherit IRouter<Application>
        inherit Express.Application
        // abstract get: obj with get, set
        abstract router: string with get, set
        abstract settings: obj with get, set
        abstract resource: obj with get, set
        abstract map: obj with get, set
        abstract locals: obj with get, set
        abstract routes: obj with get, set
        abstract init: unit -> unit
        abstract defaultConfiguration: unit -> unit
        abstract engine: ext: string * fn: Function -> Application
        abstract set: setting: string * ``val``: obj -> Application
        abstract path: unit -> string
        abstract enabled: setting: string -> bool
        abstract disabled: setting: string -> bool
        abstract enable: setting: string -> Application
        abstract disable: setting: string -> Application
        abstract configure: fn: Function -> Application
        abstract configure: env0: string * fn: Function -> Application
        abstract configure: env0: string * env1: string * fn: Function -> Application
        abstract configure: env0: string * env1: string * env2: string * fn: Function -> Application
        abstract configure: env0: string * env1: string * env2: string * env3: string * fn: Function -> Application
        abstract configure: env0: string * env1: string * env2: string * env3: string * env4: string * fn: Function -> Application
        abstract render: name: string * ?options: obj * ?callback: Func<Error, string, unit> -> unit
        abstract render: name: string * callback: Func<Error, string, unit> -> unit
        abstract listen: port: float * hostname: string * backlog: float * ?callback: Function -> http.Server
        abstract listen: port: float * hostname: string * ?callback: Function -> http.Server
        abstract listen: port: float * ?callback: Function -> http.Server
        abstract listen: path: string * ?callback: Function -> http.Server
        abstract listen: handle: obj * ?listeningListener: Function -> http.Server
        abstract route: path: string -> IRoute

    and Express =
        inherit Application
        abstract version: string with get, set
        abstract mime: string with get, set
        abstract application: obj with get, set
        abstract request: Request with get, set
        abstract response: Response with get, set
        [<Emit("$0($1...)")>] abstract Invoke: unit -> Application
        abstract createApplication: unit -> Application
        abstract createServer: unit -> Application

    module mime =
        type Charsets =
            abstract lookup: mime: string -> string

        type Globals =
            member __.charsets with get(): Charsets = failwith "JS only" and set(v: Charsets): unit = failwith "JS only"
            member __.default_type with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.lookup(path: string): string = failwith "JS only"
            member __.extension(mime: string): string = failwith "JS only"
            member __.load(filepath: string): unit = failwith "JS only"
            member __.define(mimes: obj): unit = failwith "JS only"

    module ``serve-static`` =
        type Options =
            abstract dotfiles: string option with get, set
            abstract etag: bool option with get, set
            abstract extensions: ResizeArray<string> option with get, set
            abstract index: U3<bool, string, ResizeArray<string>> option with get, set
            abstract lastModified: bool option with get, set
            abstract maxAge: U2<float, string> option with get, set
            abstract redirect: bool option with get, set
            abstract setHeaders: Func<Response, string, obj, obj> option with get, set
    
        type Globals =
            member __.mime: mime.Globals = failwith "JS only"
            [<Emit("$0($1...)")>] member __.Invoke(root: string, ?options: Options): Handler = failwith "JS only"

    type Globals =
        member __.``static``: ``serve-static``.Globals = failwith "JS only"
        member __.Router(?options: obj): Router = failwith "JS only"
        [<Emit("$0($1...)")>] member __.Invoke(): Express = failwith "JS only"

    let [<Import("express?asDefault=true")>] Globals: Globals = failwith "JS only"
