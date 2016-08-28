[<AutoOpen>]
module Forge.Stubs

// Writes a trace to the command line
let traceWarning text = printfn "%s" text
let traceException (ex: System.Exception) = printfn "%s" (ex.Message)
