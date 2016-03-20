namespace Fable.Core
open System

type ReplaceAttribute() =
    inherit Attribute()

type EraseAttribute() =
    inherit Attribute()

type GlobalAttribute() =
    inherit Attribute()

type ImportAttribute(path: string) =
    inherit Attribute()

type EmitAttribute(macro: string) =
    inherit Attribute()
    
type [<Erase>] U2<'a, 'b> = Case1 of 'a | Case2 of 'b
type [<Erase>] U3<'a, 'b, 'c> = Case1 of 'a | Case2 of 'b | Case3 of 'c    
type [<Erase>] U4<'a, 'b, 'c, 'd> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd    

[<AutoOpen>]
module Operators =
    let (?) (o: obj) (prop: obj): obj = failwith "JS only"
    
    let (?<-) (o: obj) (prop: obj) (v: obj): unit = failwith "JS only"

    let ($) (callee: obj) (args: obj): obj = failwith "JS only"
    
    let (==>) (key: string) (v: obj): string*obj = failwith "JS only"
    
    let createNew (o: obj) (args: obj): obj = failwith "JS only"
    
    let createObj (fields: #seq<string*obj>): obj = failwith "JS only"
    
    let createEmpty<'T> : 'T = failwith "JS only"
    
module Testing =
    type TestAttribute() =
        inherit Attribute()
    
    type TestFixtureAttribute() =
        inherit Attribute()
        
    type Assert =
        static member AreEqual(x: 'T, y: 'T): unit = failwith "JS only"

    