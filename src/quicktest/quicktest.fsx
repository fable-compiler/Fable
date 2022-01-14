#r "nuget: Fable.Core.Experimental,4.0.0-alpha-026"

open Fable.Core

// Make data visible in F#
let [<Global>] mutable private s2: float[] = nativeOnly

s2  <- s2 |> Array.map ((*) 100.)