namespace Fable.Core
open System

type ReplaceAttribute() =
    inherit Attribute()

type EraseAttribute() =
    inherit Attribute()

type GlobalAttribute() =
    inherit Attribute()

type ImportAttribute(get: string, from: string) =
    inherit Attribute()

type EmitAttribute private () =
    inherit Attribute()
    new (macro: string) = EmitAttribute()
    new (emitterType: Type, methodName: string) = EmitAttribute()
    
type KeyValueListAttribute() =
    inherit Attribute()

type StringEnumAttribute() =
    inherit Attribute()    
    
type [<Erase>] U2<'a, 'b> = Case1 of 'a | Case2 of 'b
type [<Erase>] U3<'a, 'b, 'c> = Case1 of 'a | Case2 of 'b | Case3 of 'c    
type [<Erase>] U4<'a, 'b, 'c, 'd> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd    
type [<Erase>] U5<'a, 'b, 'c, 'd, 'e> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd | Case5 of 'e    
type [<Erase>] U6<'a, 'b, 'c, 'd, 'e, 'f> = Case1 of 'a | Case2 of 'b | Case3 of 'c | Case4 of 'd | Case5 of 'e | Case6 of 'f    

type Applicable = obj->obj

[<AutoOpen>]
module Operators =
    let (?) (o: obj) (prop: obj): Applicable = failwith "JS only"
    
    let (?<-) (o: obj) (prop: obj) (v: obj): unit = failwith "JS only"

    // let ($) (callee: obj) (args: obj): obj = failwith "JS only"
    
    let (==>) (key: string) (v: obj): string*obj = failwith "JS only"
    
    let createNew (o: obj) (args: obj): obj = failwith "JS only"
    
    let createObj (fields: #seq<string*obj>): obj = failwith "JS only"
    
    let createEmpty<'T> : 'T = failwith "JS only"

module Serialize =
    let toJson (o: 'T): string = failwith "JS only"
    let ofJson<'T> (json: string): 'T = failwith "JS only"
    
module Testing =
    type TestAttribute() =
        inherit Attribute()
    
    type TestFixtureAttribute() =
        inherit Attribute()
    
    type TestFixtureSetUpAttribute() =
        inherit Attribute()

    type TestFixtureTearDownAttribute() =
        inherit Attribute()

    type SetUpAttribute() =
        inherit Attribute()

    type TearDownAttribute() =
        inherit Attribute()     
        
    type Assert =
        static member AreEqual(x: 'T, y: 'T): unit = failwith "JS only"

    