module rec Fable.Compilers.Lua

open Fable
open Fable.AST
open Fable.AST.Fable

type LuaCompiler(com: Fable.Compiler) =
    let mutable types = Map.empty
    let mutable decisionTreeTargets = []
    member this.Com = com
    member this.AddClassDecl (c: ClassDecl) =
        types <- types |> Map.add c.Entity c
    member this.GetByRef (e: EntityRef) =
        types |> Map.tryFind e
    member this.DecisionTreeTargets (exprs: (list<Fable.Ident> * Expr) list) =
        decisionTreeTargets <- exprs
    member this.GetDecisionTreeTargets (idx: int) = decisionTreeTargets.[idx]
    member this.GetMember(memberRef: Fable.MemberRef): Fable.MemberFunctionOrValue =
        com.GetMember(memberRef: Fable.MemberRef)