[<AutoOpen>]
module Forge.PreludeCore

open System
open System.IO
open System.Diagnostics


// Writes a trace to the command line
let traceWarning text = printfn "%s" text
let traceException (ex: System.Exception) = printfn "%s" (ex.Message)

module System =
    let Converter (mapping: 'T -> 'U) =
        mapping

type System.Collections.Generic.List<'T> with
    member this.ConvertAll (mapping: 'T -> 'U) =
        let len = this.Count
        let res = ResizeArray<_>(len)
        for i = 0 to len - 1 do
            res.Add(mapping this.[i])
        res


//===============================================================================================
// below is a subset of https://github.com/fsprojects/Forge/blob/master/src/Forge.Core/Prelude.fs
//===============================================================================================


// Operators
//========================

let (^) = (<|)

let inline (|?|) (pred1:'a->bool) (pred2:'a->bool)  =
    fun a -> pred1 a || pred2 a

let inline (|&|) (pred1:'a->bool) (pred2:'a->bool)  =
    fun a -> pred1 a && pred2 a


/// Detects whether the given path does not contains invalid characters.
let isValidPath (path:string) =
    let invalidChars = Path.GetInvalidPathChars()
    (true, path.ToCharArray())
    ||> Array.fold (fun isValid pathChar ->
        if not isValid then false else
        not ^ Array.exists ((=) pathChar) invalidChars
    )

// Helpers
//========================

let inline mapOpt (opt:'a option) mapfn (x:'b) =
    match opt with
    | None -> x
    | Some a -> mapfn a x

let parseGuid text =
    let mutable g = Unchecked.defaultof<Guid>
    if Guid.TryParse(text,&g) then Some g else None

let parseBool text =
    let mutable b = Unchecked.defaultof<bool>
    if Boolean.TryParse(text,&b) then Some b else None

[<RequireQualifiedAccess>]
module Option =

    /// Gets the value associated with the option or the supplied default value.
    let inline getOrElse v = function Some x -> x | None -> v

// ACTIVE PATTERNS
//=====================================

let (|InvariantEqual|_|) (str:string) arg =
  if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0
  then Some () else None
