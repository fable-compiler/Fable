module rec Fable.Compilers.C

open Fable.AST
open Fable.AST.Fable

type CCompiler(com: Fable.Compiler) =
    let mutable types = Map.empty
    let mutable decisionTreeTargets = []
    let mutable additionalDeclarations = []
    let mutable includes = Set.empty
    //member this.Com = com
    // member this.AddClassDecl (c: ClassDecl) =
    //     types <- types |> Map.add c.Entity c
    // member this.GetByRef (e: EntityRef) =
    //     types |> Map.tryFind e
    member this.DecisionTreeTargets (exprs: (list<Fable.Ident> * Expr) list) =
        decisionTreeTargets <- exprs
    member this.GetDecisionTreeTargets (idx: int) = decisionTreeTargets.[idx]
    member this.GetEntity entRef= com.TryGetEntity(entRef).Value
    member this.GenAndCallDeferredFunctionFromExpr (scopedArgs, body, retType) =
        let seed = scopedArgs.GetHashCode() + body.GetHashCode() //todo prevent collisions
        let delegatedName = "delegated_" + seed.ToString() //todo generate procedurally
        let declaration = C.FunctionDeclaration(
                delegatedName,
                scopedArgs |> List.map (fun (s: C.CIdent) -> s.Name, s.Type),
                body,
                retType)
        additionalDeclarations <- declaration::additionalDeclarations
        C.FunctionCall(C.Ident {Name = delegatedName; Type = C.Void }, scopedArgs |> List.map C.Ident)
    member this.GetAdditionalDeclarations() = additionalDeclarations
    member this.RegisterInclude(fInclude: Fable.AST.C.Include) =
        includes |> Set.add fInclude
    member this.GetIncludes() = includes |> Set.toList