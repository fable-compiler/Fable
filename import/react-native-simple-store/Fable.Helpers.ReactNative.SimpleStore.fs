[<Fable.Core.Erase>]
module internal Fable.Helpers.ReactNative.SimpleStore

open System
open Fable.Import.ReactNative
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Core.JsInterop

/// Loads a value as string with the given key from the local device storage. Returns None if the key is not found.
let inline getItem (key:string) = async {
    let! v = Globals.AsyncStorage.getItem key |> Async.AwaitPromise
    match v with
    | null -> return None
    | _ -> return Some v
}

/// Loads a value with the given key from the local device storage. Returns None if the key is not found.
let inline load<'a> (key:string) : Async<'a option> = async {
    let! v = Globals.AsyncStorage.getItem key |> Async.AwaitPromise
    match v with
    | null -> return None
    | _ -> return Some (ofJson v)
}

/// Saves a value with the given key to the local device storage.
let inline setItem (k:string) (v:string) = async {
    let! v = Globals.AsyncStorage.setItem(k,v) |> Async.AwaitPromise
    ()
}

/// Saves a value with the given key to the local device storage.
let inline save<'a> (k:string) (v:'a) = async {
    let s:string = toJson v
    let! v = Globals.AsyncStorage.setItem(k,s) |> Async.AwaitPromise
    ()
}    