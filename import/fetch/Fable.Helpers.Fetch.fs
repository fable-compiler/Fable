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
type IRequestProperties =
    interface end

[<KeyValueList>]
type RequestProperties =
    | Method of HttpMethod
    | Headers of U2<HeaderInit, obj>
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

/// Sends a HTTP post with the record serialized as JSON  
let inline postRecord<'T> (url,record:'T, properties: RequestProperties list) : Async<Response> =
    let props = 
        [ RequestProperties.Method HttpMethod.POST
          RequestProperties.Body (unbox (toJson record))]
          ++ properties 

    fetchAsync(url,props)