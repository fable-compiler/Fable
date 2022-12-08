// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Configurable Diagnostics channel for the Abstract IL library

module internal FSharp.Compiler.AbstractIL.Diagnostics

#if FABLE_COMPILER

let dprintf fmt = printf fmt
let dprintfn fmt = printfn fmt
let dprintn s = printfn "%s" s

#else

let mutable diagnosticsLog = Some stdout

let setDiagnosticsChannel s = diagnosticsLog <- s

let dflushn () =
    match diagnosticsLog with
    | None -> ()
    | Some d ->
        d.WriteLine()
        d.Flush()

let dflush () =
    match diagnosticsLog with
    | None -> ()
    | Some d -> d.Flush()

let dprintn (s: string) =
    match diagnosticsLog with
    | None -> ()
    | Some d ->
        d.Write s
        d.Write "\n"
        dflush ()

let dprintf (fmt: Format<_, _, _, _>) =
    Printf.kfprintf
        dflush
        (match diagnosticsLog with
         | None -> System.IO.TextWriter.Null
         | Some d -> d)
        fmt

let dprintfn (fmt: Format<_, _, _, _>) =
    Printf.kfprintf
        dflushn
        (match diagnosticsLog with
         | None -> System.IO.TextWriter.Null
         | Some d -> d)
        fmt

#endif //!FABLE_COMPILER
