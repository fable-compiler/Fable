module Fable.Transforms.Replacements

open Fable
open Fable.Core
open Fable.AST
open Fable.AST.Fable
open Fable.AST.Fable.Util

 module Util =
     let [<Literal>] system = "System."
     let [<Literal>] fsharp = "Microsoft.FSharp."
     let [<Literal>] fableCore = "Fable.Core."
     let [<Literal>] operators = "Microsoft.FSharp.Core.Operators."
     let [<Literal>] genericCollections = "System.Collections.Generic."

open Util

let isReplaceCandidate (fullName: string) =
    fullName.StartsWith(system)
        || fullName.StartsWith(fsharp)
        || fullName.StartsWith(fableCore)

/// Resolve pipes and composition in a first pass
let tryFirstPass r t (args: Expr list) (fullName: string) =
    let compose f1 f2 =
        None // TODO
        // let tempVar = com.GetUniqueVar() |> makeIdent
        // [Fable.IdentExpr tempVar]
        // |> makeApply com info.range Fable.Any f1
        // |> List.singleton
        // |> makeApply com info.range Fable.Any f2
        // |> makeLambda [tempVar]
    if fullName.StartsWith(operators) then
        match fullName.Substring(fullName.LastIndexOf(".") + 1), args with
        | "( |> )", [x; f]
        | "( <| )", [f; x] -> makeApply r t f [x] |> Some
        | "( ||> )", [x; y; f]
        | "( <|| )", [f; x; y] -> makeApply r t f [x; y] |> Some
        | "( |||> )", [x; y; z; f]
        | "( <||| )", [f; x; y; z] -> makeApply r t f [x; y; z] |> Some
        | "( >> )", [f1; f2] -> compose f1 f2
        | "( << )", [f2; f1] -> compose f1 f2
        | _ -> None
    else None
