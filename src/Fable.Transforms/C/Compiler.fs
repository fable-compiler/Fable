module rec Fable.Compilers.C

open Fable
open Fable.AST
open Fable.AST.Fable

type CCompiler(com: Fable.Compiler) =

    let mutable types = Map.empty
    let mutable decisionTreeTargets = []
    let mutable additionalDeclarations = Set.empty
    let mutable includes = Set.empty
    let mutable identSubstitutions = Map.empty
    //member this.Com = com
    // member this.AddClassDecl (c: ClassDecl) =
    //     types <- types |> Map.add c.Entity c
    // member this.GetByRef (e: EntityRef) =
    //     types |> Map.tryFind e
    member this.DecisionTreeTargets (exprs: (list<Fable.Ident> * Expr) list) =
        decisionTreeTargets <- exprs
    member this.GetDecisionTreeTargets (idx: int) = decisionTreeTargets.[idx]
    member this.GetEntity entRef= com.TryGetEntity(entRef).Value
    member this.GetMember = com.GetMember
    // member _.MakeImportPath(path) =
    //     let projDir = System.IO.Path.GetDirectoryName(cliArgs.ProjectFile)
    //     let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
    //     if path.EndsWith(".fs") then Path.ChangeExtension(path, fileExt) else path
    member this.GenFunctionSignatureAlias (args, retType) =
        let seed =
            let v = args.GetHashCode() + retType.GetHashCode()
            if v < 0 then -v else v//todo prevent collisions
        let declName = "function_" + seed.ToString();
        additionalDeclarations <- additionalDeclarations |> Set.add (C.TypedefFnDeclaration(declName, args |> List.mapi (fun i a -> "p_"+i.ToString(), a), retType))
        C.CTypeDef declName

    member this.GenAndCallDeferredFunctionFromExpr (scopedArgs, body, retType) =
        let seed =
            let v = scopedArgs.GetHashCode() + body.GetHashCode()
            if v < 0 then -v else v//todo prevent collisions
        let delegatedName = "delegated_" + seed.ToString() //todo generate procedurally
        let declaration = C.FunctionDeclaration(
                delegatedName,
                scopedArgs |> List.map (fun (s: C.CIdent) -> s.Name, s.Type),
                body,
                retType)
        additionalDeclarations <- additionalDeclarations |> Set.add (declaration)
        C.FunctionCall(C.Ident {Name = delegatedName; Type = C.Void }, scopedArgs |> List.map C.Ident)
    member this.GenAndCallDeferredClosureFromExpr (lambdaType: Fable.Type, scopedArgs, closedOverIdents, body, retType) =
        let seed =
            let v = scopedArgs.GetHashCode() + body.GetHashCode()
            if v < 0 then -v else v//todo prevent collisions
        let delegatedName = "delegated_" + seed.ToString() //todo generate procedurally
        let functionDeclaration = C.FunctionDeclaration(
                delegatedName,
                scopedArgs |> List.map (fun (s: C.CIdent) -> s.Name, s.Type),
                body,
                retType)
        let structClosureNm = "delegatedclosure_" + seed.ToString()
        let fsParams = (scopedArgs |> List.map (fun s -> s.Type)) @ (closedOverIdents |> List.map snd)
        let identParam = "fn", this.GenFunctionSignatureAlias(fsParams, retType)
        let structClosureDeclaration = C.StructDeclaration(
            structClosureNm,
            identParam::closedOverIdents
        )
        let newStructClosureDeclaration = C.FunctionDeclaration(
            structClosureNm + "_new",
            closedOverIdents,
            [
                C.DeclareIdent("item", structClosureNm |> C.CStruct)
                C.Do(C.SetValue(C.GetField(C.Ident {Name = "item"; Type = C.Void;}, "fn"), C.Ident {Name = delegatedName; Type = C.Void}))
                for (name, ctype) in closedOverIdents do
                    C.Do(C.SetValue(C.GetField(C.Ident {Name = "item"; Type = C.Void;}, name), C.Ident {Name = name; Type = C.Void}))
                C.Assignment(["rc"],
                    C.FunctionCall(C.Ident { Name="Rc_New"; Type= C.Void},
                        [

                            C.FunctionCall(C.Ident { Name = "sizeof"; Type = C.Void }, [ C.Ident { Name = "item"; Type = C.Void }])
                            C.Unary(C.UnaryOp.RefOf, C.Ident { Name = "item"; Type = C.Void })
                            C.Const C.ConstNull
                        ]
                    ),
                    C.Rc (structClosureNm |> C.CStruct))
                C.Return (C.Ident { Name = "rc"; Type = structClosureNm |> C.CStruct})
            ],
            C.Rc C.Void
        )
        additionalDeclarations <-
            additionalDeclarations
            |> Set.add functionDeclaration
            |> Set.add newStructClosureDeclaration
            |> Set.add structClosureDeclaration
        //struct with captures
        C.FunctionCall(C.Ident {Name = structClosureNm + "_new"; Type = C.Void },
            closedOverIdents |> List.map (fun (name, t) -> C.Ident {Name = name; Type = t }))
    member this.GetAdditionalDeclarations() = additionalDeclarations |> Set.toList
    member this.RegisterInclude(fInclude: Fable.AST.C.Include) =
        // failwithf "%A" com.LibraryDir
        includes <- includes |> Set.add fInclude
    member this.RegisterIdentSubstitution (oldIdent: string, newIdent: string) =
        identSubstitutions <- identSubstitutions |> Map.add oldIdent newIdent
    member this.GetIdentSubstitution oldValue =
        identSubstitutions |> Map.tryFind oldValue |> Option.defaultValue oldValue
    member this.GetIncludes() = includes |> Set.toList