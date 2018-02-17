module Fable.Transforms.Replacements

open Fable
open Fable.Core
open Fable.AST
open Fable.AST.Fable.Util

 module Util =
     let [<Literal>] system = "System."
     let [<Literal>] fsharp = "Microsoft.FSharp."
     let [<Literal>] fableCore = "Fable.Core."
     let [<Literal>] genericCollections = "System.Collections.Generic."

open Util

let isReplaceCandidate (fullName: string) =
    fullName.StartsWith(system)
        || fullName.StartsWith(fsharp)
        || fullName.StartsWith(fableCore)