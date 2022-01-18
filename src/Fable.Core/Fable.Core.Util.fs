namespace Fable.Core

open System

[<AutoOpen>]
module Util =
    /// Used to indicate that a member is only implemented in the native target language
    let nativeOnly<'T> : 'T = failwith "Native only"

    /// Alias of nativeOnly
    let jsNative<'T> : 'T = failwith "JS only"

    /// Marks the start of a region. When using the --delimiter option,
    /// Fable will try to match these regions with areas surrounded by
    /// lines starting with the delimiter in the target file.
    let region (header: string): unit = ()

module Experimental =
    /// Reads the name of an identifier, a property or a type
    let inline nameof(expr: 'a): string = nativeOnly

    /// Like nameof but also returns the expression as second element of the tuple
    let inline nameof2(expr: 'a): string * 'a = nativeOnly

    /// Reads the name of a property or a type from the lambda body
    let inline nameofLambda(f: 'a -> 'b): string = nativeOnly

    /// Reads the names of an access path from the lambda body. E.g (fun x -> x.foo.bar) gives [|"foo"; "bar"|]
    let inline namesofLambda(f: 'a -> 'b): string[] = nativeOnly

    /// Reads the case name and field count of a simple match: `casenameWithFieldCount(function Foo _ -> true | _ -> false)`
    let casenameWithFieldCount<'T> (f: 'T -> bool): string * int = nativeOnly

    /// Reads the case name and field index of a simple match: `casenameWithFieldIndex(function Bar(_,i) -> i | _ -> failwith "")`
    let casenameWithFieldIndex<'T, 'O> (f: 'T -> 'O): string * int = nativeOnly

module Testing =
    type Assert =
        static member AreEqual(actual: 'T, expected: 'T, ?msg: string): unit = nativeOnly
        static member NotEqual(actual: 'T, expected: 'T, ?msg: string): unit = nativeOnly


module Reflection =
    let isUnion (x: obj): bool = nativeOnly
    let isRecord (x: obj): bool = nativeOnly

    let getCaseTag (x: obj): int = nativeOnly
    let getCaseName (x: obj): string = nativeOnly
    let getCaseFields (x: obj): obj[] = nativeOnly

module Compiler =
    /// Compiler full version as string
    let version: string = ""

    /// Compiler major/minor version as number (e.g. 3.6)
    let majorMinorVersion: float = 0.

    /// Indicates if compiler is running in debug mode
    let debugMode: bool = false

    /// Indicates if Fable will compile numeric arrays as JS typed arrays
    let typedArrays: bool = false

    /// Extension used for generated files
    let extension: string = ".fs.js"

    /// In watch compilations, indicates if the file is being recompiled
    /// not because of a direct change, but because a dependency has changed
    let triggeredByDependency: bool = false
