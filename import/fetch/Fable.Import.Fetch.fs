namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

module Fetch =

    type [<AbstractClass; Import("*","Body")>] Body() =
        abstract bodyUsed: bool with get, set
        abstract arrayBuffer: unit -> Promise<ArrayBuffer>
        abstract blob: unit -> Promise<Browser.Blob>;
        abstract formData: unit -> Promise<Browser.FormData>
        abstract json : unit -> Promise<obj>;
        abstract json<'T> : unit -> Promise<'T>
        abstract text : unit -> Promise<string>
        
    and [<AbstractClass; Import("*","Request")>] Request(input: U2<string, Request>, ?init: RequestInit) =
        inherit Body()
        abstract ``method`` : string with get
        abstract url: string with get
        abstract headers: Headers with get
        //Deprecated: abstract context: U2<string,RequestContext> 
        abstract referrer: string with get
        abstract mode: U2<string,RequestMode> with get
        abstract credentials: U2<string,RequestCredentials> with get
        abstract cache: U2<string,RequestCache> with get
        abstract clone: unit -> unit

    and RequestInit =
        abstract ``method``: string option with get, set
        abstract headers: U2<HeaderInit, obj> option with get, set
        abstract body: BodyInit option with get, set
        abstract mode: RequestMode option with get, set
        abstract credentials: RequestCredentials option with get, set
        abstract cache: RequestCache option with get, set

    and [<StringEnum>] RequestContext =
        | Audio | Beacon | Cspreport | Download | Embed | Eventsource | Favicon | Fetch | Font 
        | Form | Frame | Hyperlink | Iframe | Image | Imageset | Import | Internal | Location 
        | Manifest | Object | Ping | Plugin | Prefetch | Script | Serviceworker | Sharedworker 
        | Subresource | Style | Track | Video | Worker | Xmlhttprequest | Xslt

    and [<StringEnum>] RequestMode =
        | [<CompiledName("same-origin")>]Sameorigin | [<CompiledName("no-cors")>]Nocors | Cors

    and [<StringEnum>] RequestCredentials =
        Omit | [<CompiledName("same-origin")>]Sameorigin | Include

    and [<StringEnum>] RequestCache =
        | Default
        | [<CompiledName("no-store")>]Nostore 
        | Reload
        | [<CompiledName("no-cache")>]Nocache
        | [<CompiledName("force-cache")>]Forcecache
        | [<CompiledName("only-if-cached")>]Onlyifcached

    and [<AbstractClass; Import("*","Headers")>] Headers() =
        abstract append : string * string -> unit
        abstract delete : string -> unit
        abstract get : string -> string
        abstract getAll : string -> Array<string>;
        abstract has : string -> bool
        abstract set : string * string -> unit

    and [<AbstractClass; Import("*","Response")>] Response(?body: BodyInit, ?init: ResponseInit) =
        inherit Body()

    and [<StringEnum>] ResponseType =
        | Basic | Cors | Default | Error | Opaque

    and ResponseInit =
        abstract status: float with get, set
        abstract statusText: string option with get, set
        abstract headers: HeaderInit option with get, set

    and HeaderInit =
        U2<Headers, ResizeArray<string>>

    and BodyInit =
        U3<Browser.Blob, Browser.FormData, string>

    [<Erase>]
    type RequestInfo =
        | Url of string
        | Req of Request
        
    type GlobalFetch =
        [<Global>]static member fetch (req: RequestInfo, ?init: RequestInit) =  failwith "JS only" :Promise<Response>