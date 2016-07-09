
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
    
let withattr value (func :('a -> 'b)) =
    let f = (fun (a :obj) -> func (a :?> 'a) :> obj)
    Globals.m.withAttr(value,Func<_,_>(f))
    
type MComponent<'a>(c: obj[] -> 'a, v: 'a  * obj[] -> VirtualElement ) =
    interface Component<'a> with
        member x.controller(args) = c args
        member x.view(c,args) = v (c,args)
    
//example

type Todo = { description: Mithril.Property<string>; complete: Mithril.Property<bool>;  }

let todo (str :string) =
    {description=Globals.m.prop str; complete=Globals.m.prop false}

type VM([<ParamArray>] args :obj[]) =
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
  
        
let vm_init = (fun x -> VM(x))



    



let view = (fun (vm :VM,[<ParamArray>] args: obj[]) -> 
    let attr1 = newAttribute "onchange" (withattr "value" (vm.Discription.Invoke))
    let attr2 = newAttribute "onclick" (Func<unit,unit>vm.Add :>obj )
    let children2 = vm.List |> Array.toSeq |> Seq.mapi (fun i x ->
        let attr3 =  newAttribute "onclick" (withattr "checked" x.complete.Invoke )
        attr3.Item("checked") <- (x.complete.Invoke() :> obj)
        let attr4 = newAttribute "style" (withattr "textDecoration" (fun () -> if x.complete.Invoke() then "line-through" else "none") )
        (Child.Case2 (Globals.m.Invoke ("tr",
            Children.Case1 (Child.Case2 (Globals.m.Invoke ("td", Children.Case1 (Child.Case2(Globals.m.Invoke ("input[type=checkbox]",attr3))) ))),
            Children.Case1 (Child.Case2 (Globals.m.Invoke ("td", attr4,Children.Case1 (Child.Case1 (x.description.Invoke())))))
            )
        ) ) :> obj
    )
    Globals.m.Invoke ("div",
           Children.Case1 (Child.Case2 (Globals.m.Invoke ("input",attr1))),
           Children.Case1 (Child.Case2 (Globals.m.Invoke ("button",attr2))),
           Children.Case1 (Child.Case2 (Globals.m.Invoke ("table",Children.Case2 (ResizeArray<obj>(children2))) ))
        )
    )    
    


let com = MComponent<VM>(vm_init,view)


        
Globals.m.mount(document.body, com )        
        
        
(*  "devDependencies": {
    "fable-import-mithril": "^0.0.19"
  },*)