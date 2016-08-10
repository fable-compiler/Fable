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

    and [<StringEnum; RequireQualifiedAccess>] RequestContext =
        | Audio | Beacon | Cspreport | Download | Embed | Eventsource | Favicon | Fetch | Font 
        | Form | Frame | Hyperlink | Iframe | Image | Imageset | Import | Internal | Location 
        | Manifest | Object | Ping | Plugin | Prefetch | Script | Serviceworker | Sharedworker 
        | Subresource | Style | Track | Video | Worker | Xmlhttprequest | Xslt

    and [<StringEnum; RequireQualifiedAccess>] RequestMode =
        | [<CompiledName("same-origin")>]Sameorigin | [<CompiledName("no-cors")>]Nocors | Cors

    and [<StringEnum; RequireQualifiedAccess>] RequestCredentials =
        Omit | [<CompiledName("same-origin")>]Sameorigin | Include

    and [<StringEnum; RequireQualifiedAccess>] RequestCache =
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

        /// Specifying which web sites can participate in cross-origin resource sharing        
        [<Emit("$0.get('Access-Control-Allow-Origin')")>] abstract AccessControlAllowOrigin: string option

        /// Specifies which patch document formats this server supports
        [<Emit("$0.get('Accept-Patch')")>] abstract AcceptPatch: string option

        /// What partial content range types this server supports via byte serving
        [<Emit("$0.get('Accept-Ranges')")>] abstract AcceptRanges: string option

        /// The age the object has been in a proxy cache in seconds
        [<Emit("$0.get('Age')")>] abstract Age: string option

        /// Valid actions for a specified resource. To be used for a 405 Method not allowed
        [<Emit("$0.get('Allow')")>] abstract Allow: string option

        /// A server uses "Alt-Svc" header (meaning Alternative Services) to indicate that its resources can also be accessed at a different network location (host or port) or using a different protocol
        [<Emit("$0.get('Alt-Svc')")>] abstract AltSvc: string option

        /// Tells all caching mechanisms from server to client whether they may cache this object. It is measured in seconds
        [<Emit("$0.get('Cache-Control')")>] abstract CacheControl: string option

        /// Control options for the current connection and list of hop-by-hop response fields
        [<Emit("$0.get('Connection')")>] abstract Connection: string option

        /// An opportunity to raise a "File Download" dialogue box for a known MIME type with binary format or suggest a filename for dynamic content. Quotes are necessary with special characters.
        [<Emit("$0.get('Content-Disposition')")>] abstract ContentDisposition: string option

        /// The type of encoding used on the data
        [<Emit("$0.get('Content-Encoding')")>] abstract ContentEncoding: string option

        /// The natural language or languages of the intended audience for the enclosed content
        [<Emit("$0.get('Content-Language')")>] abstract ContentLanguage: string option

        /// The length of the response body in octets (8-bit bytes)
        [<Emit("$0.get('Content-Length')")>] abstract ContentLength: string option

        ///	An alternate location for the returned data
        [<Emit("$0.get('Content-Location')")>] abstract ContentLocation: string option

        ///	A Base64-encoded binary MD5 sum of the content of the response
        [<Emit("$0.get('Content-MD5')")>] abstract ContentMD5: string option
        
        /// Where in a full body message this partial message belongs
        [<Emit("$0.get('Content-Range'')")>] abstract ContentRange: string option

        /// The MIME type of this content
        [<Emit("$0.get('Content-Type')")>] abstract ContentType: string option

        /// The date and time that the message was sent (in "HTTP-date" format as defined by RFC 7231)
        [<Emit("$0.get('Date')")>] abstract Date: string option

        /// An identifier for a specific version of a resource, often a message digest
        [<Emit("$0.get('ETag')")>] abstract ETag: string option

        /// Gives the date/time after which the response is considered stale (in "HTTP-date" format as defined by RFC 7231)
        [<Emit("$0.get('Expires')")>] abstract Expires: string option

        /// The last modified date for the requested object (in "HTTP-date" format as defined by RFC 7231)
        [<Emit("$0.get('Last-Modified')")>] abstract LastModified: string option

        /// Used to express a typed relationship with another resource, where the relation type is defined by RFC 5988
        [<Emit("$0.get('Link')")>] abstract Link: string option

        /// Used in redirection, or when a new resource has been created.
        [<Emit("$0.get('Location')")>] abstract Location: string option

        /// This field is supposed to set P3P policy, in the form of P3P:CP="your_compact_policy". 
        [<Emit("$0.get('P3P')")>] abstract P3P: string option

        /// Implementation-specific fields that may have various effects anywhere along the request-response chain.
        [<Emit("$0.get('Pragma')")>] abstract Pragma: string option

        /// Request authentication to access the proxy.
        [<Emit("$0.get('Proxy-Authenticate')")>] abstract ProxyAuthenticate: string option

        ///	HTTP Public Key Pinning, announces hash of website's authentic TLS certificate
        [<Emit("$0.get('Public-Key-Pins')")>] abstract PublicKeyPins: string option

        ///	Used in redirection, or when a new resource has been created. This refresh redirects after 5 seconds.
        [<Emit("$0.get('Refresh')")>] abstract Refresh: string option

        ///	If an entity is temporarily unavailable, this instructs the client to try again later. Value could be a specified period of time (in seconds) or a HTTP-date.
        [<Emit("$0.get('Retry-After')")>] abstract RetryAfter: string option

        ///	A name for the server
        [<Emit("$0.get('Server')")>] abstract Server: string option

        ///	An HTTP cookie
        [<Emit("$0.get('Set-Cookie')")>] abstract SetCookie: string option

        ///	CGI header field specifying the status of the HTTP response. Normal HTTP responses use a separate "Status-Line" instead, defined by RFC 7230
        [<Emit("$0.get('Status')")>] abstract Status: string option

        ///	A HSTS Policy informing the HTTP client how long to cache the HTTPS only policy and whether this applies to subdomains.
        [<Emit("$0.get('Strict-Transport-Security')")>] abstract StrictTransportSecurity: string option

        ///	The Trailer general field value indicates that the given set of header fields is present in the trailer of a message encoded with chunked transfer coding.
        [<Emit("$0.get('Trailer')")>] abstract Trailer: string option

        ///	The form of encoding used to safely transfer the entity to the user. Currently defined methods are: chunked, compress, deflate, gzip, identity.
        [<Emit("$0.get('Transfer-Encoding')")>] abstract TransferEncoding: string option

        ///	Tracking Status Value, value suggested to be sent in response to a DNT(do-not-track)
        [<Emit("$0.get('TSV')")>] abstract TSV: string option

        ///	Ask the client to upgrade to another protocol.
        [<Emit("$0.get('Upgrade')")>] abstract Upgrade: string option

        ///	Tells downstream proxies how to match future request headers to decide whether the cached response can be used rather than requesting a fresh one from the origin server.
        [<Emit("$0.get('Vary')")>] abstract Vary: string option

        ///	Informs the client of proxies through which the response was sent.
        [<Emit("$0.get('Via')")>] abstract Via: string option

        ///	A general warning about possible problems with the entity body.
        [<Emit("$0.get('Warning')")>] abstract Warning: string option

        ///	Indicates the authentication scheme that should be used to access the requested entity.
        [<Emit("$0.get('WWW-Authenticate')")>] abstract WWWAuthenticate: string option

        ///	Clickjacking protection: deny - no rendering within a frame, sameorigin - no rendering if origin mismatch, allow-from - allow from specified location, allowall - non-standard, allow from any location
        [<Emit("$0.get('X-Frame-Options')")>] abstract XFrameOptions: string option

        ///	Cross-site scripting (XSS) filter
        [<Emit("$0.get('X-XSS-Protection')")>] abstract XXSSProtection: string option

        ///	Content Security Policy definition.
        [<Emit("$0.get('Content-Security-Policy')")>] abstract ContentSecurityPolicy: string option

        /// The only defined value, "nosniff", prevents Internet Explorer from MIME-sniffing a response away from the declared content-type. This also applies to Google Chrome, when downloading extensions
        [<Emit("$0.get('X-Content-Type-Options')")>] abstract XContentTypeOptions: string option

        /// Specifies the technology (e.g. ASP.NET, PHP, JBoss) supporting the web application (version details are often in X-Runtime, X-Version, or X-AspNet-Version)
        [<Emit("$0.get('X-Powered-By')")>] abstract XPoweredBy: string option

        /// Recommends the preferred rendering engine (often a backward-compatibility mode) to use to display the content. Also used to activate Chrome Frame in Internet Explorer.
        [<Emit("$0.get('X-UA-Compatible')")>] abstract XUACompatible: string option

        /// Provide the duration of the audio or video in seconds; only supported by Gecko browsers
        [<Emit("$0.get('X-Content-Duration')")>] abstract XContentDuration: string option

        /// Tells a server which (presumably in the middle of a HTTP -> HTTPS migration) hosts mixed content that the client would prefer redirection to HTTPS and can handle Content-Security-Policy: upgrade-insecure-requests
        [<Emit("$0.get('Upgrade-Insecure-Requests')")>] abstract UpgradeInsecureRequests: string option

        /// Correlates HTTP requests between a client and server.
        [<Emit("$0.get('X-Request-ID')")>] abstract XRequestID: string option

        /// Correlates HTTP requests between a client and server.
        [<Emit("$0.get('X-Correlation-ID')")>] abstract XCorrelationID: string option


    and [<AbstractClass; Import("*","Response")>] Response(?body: BodyInit, ?init: ResponseInit) =
        inherit Body()

        /// Verifies that the fetch was successful
        [<Emit("$0.ok")>] abstract Ok: bool
        
        /// Returns the HTTP status code
        [<Emit("$0.status")>] abstract Status: int

        /// Returns the headers objct
        [<Emit("$0.headers")>] abstract Headers : Headers

    and [<StringEnum; RequireQualifiedAccess>] ResponseType =
        | Basic | Cors | Default | Error | Opaque

    and ResponseInit =
        abstract status: float with get, set
        abstract statusText: string option with get, set
        abstract headers: HeaderInit option with get, set

    and HeaderInit =
        U2<Headers, ResizeArray<string>>

    and BodyInit =
        U3<Browser.Blob, Browser.FormData, string>

    [<Erase; RequireQualifiedAccess>]
    type RequestInfo =
        /// Uses a simple Url as string to create the request info
        | Url of string
        /// Uses a Request object as request info
        | Req of Request
    
    
    /// The Fetch API provides a JavaScript interface for accessing and manipulating parts of the HTTP pipeline, such as requests and responses. 
    /// It also provides a global fetch() method that provides an easy, logical way to fetch resources asynchronously across the network.
    type GlobalFetch =
        [<Global>]static member fetch (req: RequestInfo, ?init: RequestInit) = failwith "JS only" :Promise<Response>

        [<Global>]static member fetch (url:string, ?init: RequestInit) = failwith "JS only" :Promise<Response>

        [<Global>]static member fetch (url:Request, ?init: RequestInit) = failwith "JS only" :Promise<Response>