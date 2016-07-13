
// Load Fable.Core and bindings to JS global objects
#r "node_modules/fable-core/Fable.Core.dll"
#load "C:\Users\Orlando\Desktop\Active Projects\Fable\Fable\import\mithril/Fable.Import.Mithril.fs"

open System
open Fable.Core
open Fable.Import.Browser
open Fable.Import
open Mithril

//Fable.Import.Node.require.Invoke("core-js") |> ignore
let m = Fable.Import.Node.require.Invoke("mithril")

//lib

type MVirtualElement(tag,attrs,[<ParamArray>] args) = 
    let mutable _tag :string = tag
    let mutable _attrs :Attributes = attrs
    let mutable _children :ResizeArray<Children> = args
    interface VirtualElement with
        member x.tag with get () = _tag and set s = _tag <- s
        member x.attrs with get () = _attrs and set a = _attrs <- a
        member x.children with get () = _children and set c = _children <- c



let newAttribute str obj =
    let o :Attributes = createEmpty<Attributes>
    o.Item(str) <- obj
    o

let addAttribute str obj (o :Attributes) =
    o.Item(str) <- obj
    o
    
let withattr value (func :('a -> 'b)) =
    let f = (fun (a :obj) -> func (a :?> 'a) :> obj)
    Globals.m.withAttr(value,Func<_,_>(f))

let newComponent (c :obj [] -> 'a) (v :'a -> VirtualElement) =
    let o = createEmpty<Component<'a>>
    o?controller <- (fun x -> c x)
    o?view <- v
    o

let ma str atr chd =
    Child.Case2 (Globals.m.Invoke (str,atr, chd))

let mm str chd = 
    Child.Case2 (Globals.m.Invoke (str,chd))
//type MComponent<'a>(c: obj[] -> 'a, v: 'a  * obj[] -> VirtualElement ) =
 //   let _c : obj[] -> 'a = c
 //   let _v : 'a  * obj[] -> VirtualElement = v
 //   interface Component<'a> with
  //      member x.controller args = _c args
 //       member x.view (c,args) = _v (c,args)   
//example

type Todo = { description: Mithril.Property<string>; complete: Mithril.Property<bool>;  }

let todo (str :string) =
    {description=Globals.m.prop str; complete=Globals.m.prop false}

type VM() =
    let mutable list :array<Todo> = [||]
    let discription :BasicProperty<string> = Globals.m.prop ()
    
    member x.Discription with get () = discription
    
    member x.List with get () = list
    
    member x.Add() = 
        if not (discription.Invoke () = "") then 
            list <- Array.append list [|todo (discription.Invoke ())|]
            discription.Invoke "" |> ignore
    
    interface Controller with
        member x.onunload evt = "1" :> obj


let vm = VM()

let vm_init x = vm        



let view = (fun (vm1 :VM) -> 
    let attr1 = (newAttribute "onchange" (withattr "value" vm1.Discription.Invoke) ) 
                    |> addAttribute "value" (vm1.Discription.Invoke ())
    let attr2 = newAttribute "onclick" (Func<unit,unit>vm1.Add :>obj )
    let children2 = vm1.List |> Array.toSeq |> Seq.mapi (fun i x ->
        let attr3 =  (newAttribute "onclick" (withattr "checked" x.complete.Invoke ))
                        |> addAttribute "checked" (x.complete.Invoke ())
        let attr4 = newAttribute "style" (withattr "textDecoration" (fun () -> if x.complete.Invoke() then "line-through" else "none") )
        mm "tr" [|
                 Children.Case1 (mm "td" [(Children.Case1 (ma "input[type=checkbox]" attr3 []))] ),
                 Children.Case1 (ma ("td" attr4 [( Children.Case1 (Child.Case1 (x.description.Invoke())))]))
                |]
    )
    Globals.m.Invoke ("div", 
              Children.Case1 (ma "input" attr1 [||]),
              Children.Case1 (ma "button" attr2 [||]),
              Children.Case1 (mm "table" [|(Children.Case2 (ResizeArray<obj>(children2)))|] )
))
        
        
    


let com = newComponent vm_init view


        
Globals.m.mount(document.body, com )        
        
        
(*  "devDependencies": {
    "fable-import-mithril": "^0.0.19"
  },*)