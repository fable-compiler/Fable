[<Fable.Core.Erase>]
module internal Fable.Helpers.ReactNativeSimpleStore

open System
open Fable.Import.ReactNative
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Core.JsInterop

module DB =

    type Table<'a> = {
        TotalRows : int
        AutoInc: int
        Rows : 'a[]
    }

    /// Creates a new model.
    let inline private getModel<'a> (key) : Async<Table<'a>> = async {
        let! v = Globals.AsyncStorage.getItem (key) |> Async.AwaitPromise
        match v with
        | null -> 
            return {
                TotalRows = 0
                AutoInc = 0
                Rows = [||]
            }
        | _ -> return ofJson v
    }


    // Adds a row to a model
    let inline add<'a>(data:'a) = 
        let key = "models/" + typeof<'a>.FullName
        async {
            let! model = getModel<'a> key
            let newModel : string =
                { TotalRows = model.TotalRows + 1
                  AutoInc = model.AutoInc + 1
                  Rows = Array.append [| data |] model.Rows }
                |> toJson

            let! _ = Globals.AsyncStorage.setItem(key,newModel) |> Async.AwaitPromise
            ()
        }

    // Gets a row from the model
    let inline get<'a>(index:int) = 
        let key = "models/" + typeof<'a>.FullName
        async {
            let! model = getModel<'a> key
            return model.Rows.[index]
        }

    // Gets a row from the model
    let inline getCount<'a>() = 
        let key = "models/" + typeof<'a>.FullName
        async {
            let! model = getModel<'a> key
            return model.TotalRows
        }