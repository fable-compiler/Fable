namespace Fabel.Core
open System

type EraseAttribute() =
    inherit Attribute()

type GlobalAttribute(path: string) =
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
    
    let createNew<'T> (o: obj) (args: obj): 'T = failwith "JS only"
    
    let createObj (fields: (string*obj) list): obj = failwith "JS only"
    
    let createEmpty<'T> : 'T = failwith "JS only"

    