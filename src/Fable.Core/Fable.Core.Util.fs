namespace Fable.Core

open System

[<AutoOpen>]
module Util =
    /// Used to indicate that a member is only implemented in native Javascript
    let inline jsNative<'T> : 'T =
        try failwith "JS only" // try/catch is just for padding so it doesn't get optimized
        with ex -> raise ex

    let inline pyNative<'T> : 'T = jsNative

module Experimental =
    /// Reads the name of an identifier, a property or a type
    let inline nameof(expr: 'a): string = jsNative

    /// Like nameof but also returns the expression as second element of the tuple
    let inline nameof2(expr: 'a): string * 'a = jsNative

    /// Reads the name of a property or a type from the lambda body
    let inline nameofLambda(f: 'a -> 'b): string = jsNative

    /// Reads the case name and field count of a simple match: `casenameWithFieldCount(function Foo _ -> true | _ -> false)`
    let casenameWithFieldCount<'T> (f: 'T -> bool): string * int = jsNative

    /// Reads the case name and field index of a simple match: `casenameWithFieldIndex(function Bar(_,i) -> i | _ -> failwith "")`
    let casenameWithFieldIndex<'T, 'O> (f: 'T -> 'O): string * int = jsNative

module Testing =
    type Assert =
        static member AreEqual(actual: 'T, expected: 'T, ?msg: string): unit = jsNative
        static member NotEqual(actual: 'T, expected: 'T, ?msg: string): unit = jsNative


module Reflection =
    let isUnion (x: obj): bool = jsNative
    let isRecord (x: obj): bool = jsNative

    let getCaseTag (x: obj): int = jsNative
    let getCaseName (x: obj): string = jsNative
    let getCaseFields (x: obj): obj[] = jsNative

module Compiler =
    let version: string = ""
    let majorMinorVersion: float = 0.
    let debugMode: bool = false
    let typedArrays: bool = false
    let extension: string = ".fs.js"
