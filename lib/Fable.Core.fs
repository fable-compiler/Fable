namespace Fable.Core
open System

type EraseAttribute() =
    inherit Attribute()

type GlobalAttribute() =
    inherit Attribute()

type ImportAttribute(path: string) =
    inherit Attribute()

type EmitAttribute(macro: string) =
    inherit Attribute()

[<AutoOpen>]
module Operators =
    let (?) (o: obj) (prop: obj): obj = failwith "JS only"
    
    let (?<-) (o: obj) (prop: obj) (v: obj): unit = failwith "JS only"

    let ($) (callee: obj) (args: obj): obj = failwith "JS only"
    
    let (==>) (key: string) (v: obj): string*obj = failwith "JS only"
    
    let createNew (o: obj) (args: obj): obj = failwith "JS only"
    
    let createObj (fields: #seq<string*obj>): obj = failwith "JS only"
    
    let createEmpty<'T> : 'T = failwith "JS only"

    