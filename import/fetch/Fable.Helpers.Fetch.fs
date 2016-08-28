[<Fable.Core.Erase>]
/// The Fetch API provides a JavaScript interface for accessing and manipulating parts of the HTTP pipeline, such as requests and responses. 
/// It also provides a global fetch() method that provides an easy, logical way to fetch resources asynchronously across the network.
module internal Fable.Helpers.Fetch

open System
open Fable.Import.Fetch
open Fable.Core
open Fable.Import
open Fable.Core.JsInterop

type [<StringEnum; RequireQualifiedAccess>] HttpMethod =
| [<CompiledName("GET")>] GET 
| [<CompiledName("HEAD")>] HEAD 
| [<CompiledName("POST")>] POST 
| [<CompiledName("PUT")>] PUT 
| [<CompiledName("DELETE")>] DELETE 
| [<CompiledName("TRACE")>] TRACE
| [<CompiledName("CONNECT")>] CONNECT

[<KeyValueList>]
type IHttpRequestHeaders =
    interface end

[<KeyValueList>]
type HttpRequestHeaders =
    | Accept of string
    | [<CompiledName("Accept-Charset")>] AcceptCharset of string
    | [<CompiledName("Accept-Encoding")>] AcceptEncoding of string
    | [<CompiledName("Accept-Language")>] AcceptLanguage of string
    | [<CompiledName("Accept-Datetime")>] AcceptDatetime of string
    | Authorization of string
    | [<CompiledName("Cache-Control")>] CacheControl of string
    | Connection of string
    | Cookie of string
    | [<CompiledName("Content-Length")>] ContentLength of string
    | [<CompiledName("Content-MD5")>] ContentMD5 of string
    | [<CompiledName("Content-Type")>] ContentType of string
    | Date of string
    | Expect of string
    | Forwarded of string
    | From of string
    | Host of string
    | [<CompiledName("If-Match")>] IfMatch of string
    | [<CompiledName("If-Modified-Since")>] IfModifiedSince of string
    | [<CompiledName("If-None-Match")>] IfNoneMatch of string
    | [<CompiledName("If-Range")>] IfRange of string
    | [<CompiledName("If-Unmodified-Since")>] IfUnmodifiedSince of string
    | [<CompiledName("Max-Forwards")>] MaxForwards of int
    | Origin of string
    | Pragma of string
    | [<CompiledName("Proxy-Authorization")>] ProxyAuthorization of string
    | Range of string
    | Referer of string
    | [<CompiledName("TE")>] TE of string
    | [<CompiledName("User-Agent")>] UserAgent of string
    | Upgrade of string
    | Via of string
    | Warning of string
    | [<CompiledName("X-Requested-With")>] XRequestedWith of string
    | [<CompiledName("DNT")>] DNT of string
    | [<CompiledName("X-Forwarded-For")>] XForwardedFor of string
    | [<CompiledName("X-Forwarded-Host")>] XForwardedHost of string
    | [<CompiledName("X-Forwarded-Proto")>] XForwardedProto of string
    | [<CompiledName("Front-End-Https")>] FrontEndHttps of string
    | [<CompiledName("X-Http-Method-Override")>] XHttpMethodOverride of string
    | [<CompiledName("X-ATT-DeviceId")>] XATTDeviceId of string
    | [<CompiledName("X-Wap-Profile")>] XWapProfile of string
    | [<CompiledName("Proxy-Connection")>] ProxyConnection of string
    | [<CompiledName("X-UIDH")>] XUIDH of string
    | [<CompiledName("X-Csrf-Token")>] XCsrfToken of string
    interface IHttpRequestHeaders

[<KeyValueList>]
type IRequestProperties =
    interface end

[<KeyValueList>]
type RequestProperties =
    | Method of HttpMethod
    | Headers of HttpRequestHeaders list
    | Body of BodyInit
    | Mode of RequestMode
    | Credentials of RequestCredentials
    | Cache of RequestCache
    interface IRequestProperties

/// Retrieves data from the specified resource.
let inline fetchAsync (url:string, init: RequestProperties list) : Async<Response> = 
    GlobalFetch.fetch(url, unbox init) |> Async.AwaitPromise

/// Retrieves data from the specified resource, parses the json and returns the data as an object of type 'T. 
let inline fetchAs<'T> (url:string, init: RequestProperties list) : Async<'T> = async {
    let! fetched = GlobalFetch.fetch(url, unbox init) |> Async.AwaitPromise
    let! json = fetched.text() |> Async.AwaitPromise
    return ofJson<'T> json    
}

[<Emit("Object.assign({}, $0, $1)")>]
let inline private ( ++ ) (a:'a list) (b:'a list) : 'a list = failwith "JS Only"

/// Sends a HTTP post with the record serialized as JSON.
/// This function already sets the HTTP Method to POST sets the json into the body.
let inline postRecord<'T> (url,record:'T, properties: RequestProperties list) : Async<Response> =
    let props = 
        [ RequestProperties.Method HttpMethod.POST
          RequestProperties.Body (unbox (toJson record))]
          ++ properties 

    fetchAsync(url,props)