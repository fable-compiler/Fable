
// Load Fable.Core and bindings to JS global objects
#r "node_modules/fable-core/Fable.Core.dll"
#load "Mithril.fs"

open System
open Fable.Core
open Fable.Import.Browser
open Fable.Import
open MithrilBase
open Mithril

//ridiculus crazy hack
let m = Fable.Import.Node.require.Invoke("mithril")


//mithril demo

type Todo = { description: MithrilBase.Property<string>; complete: MithrilBase.Property<bool>;  }

let todo (str :string) =
    {description=property str; complete=property false}

type VM() =
    let mutable list :array<Todo> = [||]
    let discription :BasicProperty<string> = property ""
    
    member x.Discription with get () = discription
    
    member x.List with get () = list
    
    member x.Add() = 
        if not (discription.get = "") then 
            list <- Array.append list [|todo (discription.get)|]
            discription.set "" |> ignore
    
    interface Controller with
        member x.onunload evt = "1" :> obj


let vm = VM()

let vm_init x = vm        


let view = (fun (vm1 :VM) -> 
    let attr1 = attr [ onChange (bindattr "value" vm1.Discription.set );
                       prop "value" (vm1.Discription.get ) ]
    let children2 = 
        vm1.List 
        |> Seq.mapi (fun i x ->
            tr None [
                    td None [input (attr [ onClick (bindattr "checked" x.complete.set) ;
                                                    prop "checked" (x.complete.get) ;
                                                    prop "type" "checkbox"]) []] ;
                    td (attr [incss [ ("textDecoration",( if x.complete.get then "line-through" else "none") )] ]) [x.description.get]
                    ] :> obj) 
        |> Seq.toList

    div None [
              input attr1 [];
              button (attr [ onClick (fun e -> vm1.Add()) ]) ["Add"];
              table None children2 
             ]
    )
        

let com = newComponent vm_init view


        
mount(document.body, com )        
        
        
(*  "devDependencies": {
    "fable-import-mithril": "^0.0.19"
  },*)