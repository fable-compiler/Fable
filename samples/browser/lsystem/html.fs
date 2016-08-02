module Fable.Html
module FsOption = FSharp.Core.Option

open Fable.Import.Browser
open Fable.Core

type DomAttribute = 
  | Event of (Element -> Event -> unit)
  | Property of string

type DomNode = 
  | Text of string
  | Element of ns:string * tag:string * attributes:(string * DomAttribute)[] * children : DomNode[]

let rec render node = 
  match node with
  | Text(s) -> 
      document.createTextNode(s) :> Node

  | Element(ns, tag, attrs, children) ->
      let el = 
        if ns = "" then document.createElement(tag) :> Element
        else document.createElementNS(ns, tag)
      let rc = Array.map render children
      for c in rc do el.appendChild(c) |> ignore
      for k, a in attrs do 
        match a with
        | Property(v) -> 
            if ns = "" then el.setAttribute(k, v)
            else el.setAttributeNS(null, k, v)
        | Event(f) -> el.addEventListener(k, U2.Case1(EventListener(f el)))
      el :> Node

let renderTo (node:HTMLElement) dom = 
  while box node.lastChild <> null do ignore(node.removeChild(node.lastChild))
  let el = render dom
  node.appendChild(el) |> ignore
  
let text s = Text(s)
let (=>) k v = k, Property(string v)
let (=!>) k f = k, Event(f)

type El(ns) = 
  member x.NS = ns
  static member (?) (el:El, n:string) = fun a b ->
    Element(el.NS, n, Array.ofList a, Array.ofList b)

let h = El("")
let s = El("http://www.w3.org/2000/svg")