
// Load Fable.Core and bindings to JS global objects
#r "node_modules/fable-core/Fable.Core.dll"
#load "Mithril.fs"

open System.Text.RegularExpressions
open System
open Fable.Core
open Fable.Import.Browser
open Fable.Import
open MithrilBase
open Mithril

//ridiculus crazy hack
let m = Fable.Import.Node.require.Invoke("mithril")


//mithril demo
type LocalStorage<'t>(id :string) =
    let store_id = id

    member x.get() :'t =
        JS.JSON.parse(Browser.localStorage.getItem(store_id) :?> string) :?> 't

    member x.set (ls :'t) = 
        Browser.localStorage.setItem(store_id, JS.JSON.stringify(ls))

type Reminder = { description: MithrilBase.Property<string>; 
                  date: MithrilBase.Property<DateTime>;
                  edited: MithrilBase.Property<bool>;
                  error: MithrilBase.Property<bool>  }

                static member New dis dte =
                 {description = property dis; date = property DateTime.UtcNow; edited = property false; error = property false} 

                


type Cont() =
    
    let local = LocalStorage<seq<Reminder>>("reminder_list")
    member this.MaxString = 100

    member this.List with get () = local.get()
                      and set (x) = local.set(x)

    member this.Discription = property ""

    member this.Add() =
        if not (this.Discription.get = "") && (Regex.Replace(this.Discription.get,"/\s/g","").Length <= this.MaxString) then 
            this.List <- Seq.append (Seq.singleton (Reminder.New (this.Discription.get.Trim()) DateTime.UtcNow )) this.List
            this.Discription.set ""

    interface Controller with
        member x.onunload evt = () :> obj

        



let vm = Cont()

let vm_init x = vm        


let item_view (index :int) (r :Reminder) =
    let charlimit = span (attr [css "inner-status"]) []
    let description = div (attr [
                            css "description" ;
                            prop "data-edited" r.edited ;
                            prop "data-error" r.error
                            onDblClick (fun e -> )
                        ]) [r.description]
    let ts = (DateTime.UtcNow - r.date.get) 
    let ago = if ts.Days > 0 then ts.Days.ToString() + " days ago"
              else if ts.Hours > 0 then ts.Hours.ToString() + " hours ago"
              else if ts.Minues > 0 then ts.Minutes.ToString() + " minutes ago"
              else ts.Seconds.ToString() + " seconds ago"
    let edit = textarea (attr [
                        css "hide" ;
                        prop "rows" 1;
                        onInput (bindattr "value" r.description.get) ;
                        onKeyup (fun e -> 
                                    if e?keyCode :?> int = 13 then 
                                        
                                    else if e?keyCode :?> int = 27 then 
                                        
                                    else 
                                        Mithril.redrawStrategy "none") );
                        onBlur (fun e -> );
                        
                    ]) [r.description.get] ;
    li None [
        label (attr [prop "data-date" r.date])
        [
            description;
            charlimit;
            innerstatus;
            span (attr [css "date"]) [ ago]

        ]
    ] :> obj

let main_view (vm1 :Cont) =  
    div (attr [css "task-container"]) [
        h1 None ["Notifications"] ;
        input (attr [
                    css "add-remind" ;
                    prop "placeholder" "Notification test" ;
                    prop "autofocus" true ;
                    prop "value" vm1.Discription.get ;
                    onKeyup (fun e -> if e?keyCode :?> int = 13 then 
                                        vm1.Add()
                                      else if e?keyCode :?> int = 27 then 
                                        vm1.Discription.set ""
                                      else 
                                        Mithril.redrawStrategy "none") ;
                    onInput (bindattr "value" vm1.Discription.set) ;
        ]) [] ;
        button (attr [
                     css "add"
                     onClick (fun e -> vm1.Add())
        ]) [] ;
        div (attr [name "input-status"]) 
            (if (Regex.Replace(vm1.Discription.get,"/\s/g","").Length <= vm1.MaxString) 
            then [(vm1.MaxString - Regex.Replace(vm1.Discription.get,"/\s/g","").Length).ToString()  + " character left"] 
            else [ span (attr [css "danger"]) ["limit of " + vm1.MaxString.ToString()]]) ;
        ul (attr [name "todo-list"])
            (vm1.List |> Seq.mapi item_view |> Seq.toList)
    ]

        

let com = newComponent vm_init view


        
mount(document.body, com )        
        
        
(*  "devDependencies": {
    "fable-import-mithril": "^0.0.19"
  },*)