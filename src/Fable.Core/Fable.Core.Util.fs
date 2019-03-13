namespace Fable.Core

open System

[<AutoOpen>]
module Util =
    /// Used to indicate that a member is only implemented in native Javascript
    let inline jsNative<'T> : 'T =
        try failwith "JS only" // try/catch is just for padding so it doesn't get optimized
        with ex -> raise ex

module Experimental =
    /// Reads the name of an identifier, a property or a type
    let nameof(expr: 'a): string = jsNative

    /// Like nameof but also returns the expression as second element of the tuple
    let nameof2(expr: 'a): string * 'a = jsNative

    /// Reads the name of a property or a type from the lambda body
    let nameofLambda(f: 'a->'b): string = jsNative


module Testing =
    type Assert =
        static member AreEqual(expected: 'T, actual: 'T, ?msg: string): unit = jsNative
        static member NotEqual(expected: 'T, actual: 'T, ?msg: string): unit = jsNative


module Reflection =
    let isUnion (x: obj): bool = jsNative
    let isRecord (x: obj): bool = jsNative

    let getCaseTag (x: obj): int = jsNative
    let getCaseName (x: obj): string = jsNative
    let getCaseFields (x: obj): obj[] = jsNative
