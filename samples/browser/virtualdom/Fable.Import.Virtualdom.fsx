#r "node_modules/fable-core/Fable.Core.dll"
#load "Fable.Helpers.Virtualdom.fsx"

open Fable.Core
open Fable.Import.Browser
open Fable.Helpers.Virtualdom.Html
open Fable.Helpers.Virtualdom.App

let failwithjs() = failwith "JS only"
let [<Import("default","virtual-dom/h")>] h(arg1: string, arg2: obj, arg3: obj[]): obj = failwithjs()
let [<Import("default","virtual-dom/diff")>] diff(arg1:obj, arg2:obj): obj = failwithjs()
let [<Import("default","virtual-dom/patch")>] patch(arg1:obj, arg2:obj): Fable.Import.Browser.Node = failwithjs()
let [<Import("default","virtual-dom/create-element")>] createElement:obj -> Fable.Import.Browser.Node = failwithjs()

[<Emit("String($0)")>]
let String i :obj= failwith "JS only"

[<Emit("$1.join($0)")>]
let join sep strs = failwith "JS only"

module Virtualdom =
    let createTree tag attributes children =
        let renderEventHandler (eventType, handler) = eventType, handler

        let renderEventBinding binding =
            match binding with
            | MouseEventHandler (eventType, handler) -> (eventType, handler :> obj)//renderMouseEventHandler mh
            | KeyboardEventHandler (eventType, handler) -> (eventType, handler :> obj)
            | EventHandler (eventType, handler) -> (eventType, handler :> obj)
            |> renderEventHandler

        let renderAttributes attributes =
            attributes
            |> List.map (function
                            | Attribute.Attribute (k,v) -> Some (k,(v :> obj))
                            | _ -> None)
            |> List.choose id
            |> (function
                | [] -> None
                | p -> Some ("attributes", (p |> createObj)))

        let toAttrs attrs =
            let (attributes, others) = attrs |> List.partition (function Attribute _ -> true | _ -> false)
            let renderedAttributes = attributes |> renderAttributes
            let renderedOthers =
                others
                |> List.map (function
                        | EventHandlerBinding binding -> binding |> renderEventBinding
                        | Style style ->
                            let v =
                                style
                                |> Array.ofList
                                |> Array.map (fun (k,v) -> k + ":" + v)
                                |> join ";"
                                :> obj
                            "style", v //((style |> Array.ofList |> Array.map (fun (k,v) -> k + ":" + v) |> join ";") :> obj)
                        | Property (key, value) -> key,(value :> obj)
                        | Attribute _ -> failwith "Should not happen"
                    )
            match renderedAttributes with
            | Some x -> x::renderedOthers
            | _ -> renderedOthers
            |> createObj

        let hAttrs = attributes |> toAttrs
        let childrenArr = children |> List.toArray
        h(tag, hAttrs, childrenArr)

    let diff tree1 tree2 = diff(tree1, tree2)
    let patch node patches = patch(node, patches)
    let createElement e = createElement(e)

    let rec render node =
        match node with
        | Element((tag,attrs), nodes) -> createTree tag attrs (nodes |> List.map render)
        | VoidElement (tag, attrs) -> createTree tag attrs []
        | Text str -> String str
        | WhiteSpace str -> String str

    let renderer =
        {
            Render = render
            Diff = diff
            Patch = patch
            CreateElement = createElement
        }
