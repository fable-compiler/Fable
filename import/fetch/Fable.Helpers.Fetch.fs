[<Fable.Core.Erase>]
module internal Fable.Helpers.Fetch

open System
open Fable.Import.Fetch
open Fable.Core
open Fable.Import
open Fable.Core.JsInterop

/// Retrieves data from the specified resource.
let inline fetchAsyncWithInit (req: RequestInfo, init: RequestInit) : Async<Response> = 
    GlobalFetch.fetch(req, init) |> Async.AwaitPromise

/// Retrieves data from the specified resource.
let inline fetchAsync (req: RequestInfo) : Async<Response> = 
    GlobalFetch.fetch(req) |> Async.AwaitPromise

/// Retrieves data from the specified resource, parses the json and returns the data as an object of type 'T.
let inline fetchAsWithInit<'T> (req: RequestInfo, init: RequestInit) : Async<'T> = async {
    let! fetched = GlobalFetch.fetch(req, init) |> Async.AwaitPromise
    let! json = fetched.text() |> Async.AwaitPromise
    return ofJson<'T> json
}

/// Retrieves data from the specified resource, parses the json and returns the data as an object of type 'T. 
let inline fetchAs<'T> (req: RequestInfo) : Async<'T> = async {
    let! fetched = GlobalFetch.fetch(req) |> Async.AwaitPromise
    let! json = fetched.text() |> Async.AwaitPromise
    return ofJson<'T> json    
}
