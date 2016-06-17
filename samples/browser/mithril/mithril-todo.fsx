
// Load Fable.Core and bindings to JS global objects
#r "node_modules/fable-core/Fable.Core.dll"
#load "C:\Users\Orlando\Desktop\Active Projects\Fable\Fable\import\mithril/Fable.Import.Mithril.fs"

open System
open Fable.Core
open Fable.Import
open Mithril



type Todo = { description: Mithril.Property<string>; complete: Mithril.Property<bool>;  }

let todo (str :string) =
    {description=Globals.m.prop str; complete=Globals.m.prop false}

type VM() =
    let mutable list :array<Todo> = [||]
    let discription :BasicProperty<string> = Globals.m.prop ()
    
    member x.Discription with get () = discription
    
    member x.Add() = 
        if not (discription.Invoke () = "") then 
            list <- Array.append list [|todo (discription.Invoke ())|]
            discription.Invoke "" |> ignore
        
let vm = VM()


let attr = Attributes.Item onchange=Globals.m.withAttr("value",vm.Discription()))
let view = Globals.m.Invoke ("input",attr)


let com = Component<VM>()
U2.Case1 
        
Globals.m.mount(document.body, com )        
        
        
(*  "devDependencies": {
    "fable-import-mithril": "^0.0.19"
  },*)