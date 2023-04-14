module rec Fable.Compilers.C

open Fable
open Fable.AST
open Fable.AST.Fable

module CHelpers =
    let clone outExpr = C.FunctionCall(C.Ident { Name = "Rc_Clone" ; Type = C.Void }, [outExpr])

type CCompiler(com: Fable.Compiler) =

    let mutable types = Map.empty
    let mutable decisionTreeTargets = []
    let mutable additionalDeclarations = []
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
        let declaration = C.TypedefFnDeclaration(declName, args |> List.mapi (fun i a -> "p_"+i.ToString(), a), retType)
        additionalDeclarations <-
            additionalDeclarations @ [declaration]
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
        additionalDeclarations <- additionalDeclarations @ [declaration]
        C.FunctionCall(C.Ident {Name = delegatedName; Type = C.Void }, scopedArgs |> List.map C.Ident)
    member this.GenAndCallDeferredClosureFromExpr (lambdaType: Fable.Type, scopedArgs: C.CIdent list, closedOverIdents: (string * C.CType) list, body, retType) =
        let seed =
            let v = scopedArgs.GetHashCode() + body.GetHashCode()
            if v < 0 then -v else v//todo prevent collisions
        let hasCaptures = closedOverIdents |> List.isEmpty |> not
        let delegatedName = "fn_with_closed_" + seed.ToString()
        let structCapturesNm = "closure_struct_captures_" + seed.ToString()
        let structClosureNm = "closure_struct_" + seed.ToString()
        let self = C.CStruct "FnClosure1" |> C.Rc
        let functionDeclaration =
            let bindClosedValsBody = [
                if hasCaptures then
                    let structType = C.CStruct "FnClosure1" // todo need to defer as chicken and egg problem
                    let expr = C.Ident { Name = "self"; Type = C.Rc structType}
                    let unwrappedSelf = C.Brackets(C.GetField(C.Cast(structClosureNm |> C.CStruct |> C.Pointer, expr), "data"))
                    C.Assignment(["captures"], C.GetFieldThroughPointer(unwrappedSelf, "captures") |> CHelpers.clone, C.CStruct structCapturesNm |> C.Rc)
                    for (name, ctype) in closedOverIdents do
                        let unwrappedCapt = C.Brackets(C.GetField(C.Cast(structCapturesNm |> C.CStruct |> C.Pointer, C.Ident {Name = "captures"; Type = C.CStruct structCapturesNm}), "data"))
                        C.Assignment([name], C.GetFieldThroughPointer(unwrappedCapt, name) |> CHelpers.clone ,ctype)
            ]
            C.FunctionDeclaration(
                delegatedName,
                ("self", self)::(scopedArgs |> List.map (fun (s: C.CIdent) -> s.Name, s.Type)),
                //closedOverIdents,
                bindClosedValsBody @ body,
                retType)

        let fsParams = (scopedArgs |> List.map (fun s -> s.Type)) @ (closedOverIdents |> List.map snd)
        let identParam = "fn", this.GenFunctionSignatureAlias(self::fsParams, retType) |> C.Pointer
        let structCapturesDeclaration = C.StructDeclaration(
            structCapturesNm,
            closedOverIdents
        )
        let structClosureDeclaration = C.StructDeclaration(
            structClosureNm,
            [
                identParam
                if hasCaptures then
                    "captures", C.Rc (C.CStruct structCapturesNm)
            ]
        )
        let newStructClosureDeclaration = C.FunctionDeclaration(
            structClosureNm + "_new",
            closedOverIdents,
            [
                C.DeclareIdent("item", structClosureNm |> C.CStruct)
                C.Do(C.SetValue(C.GetField(C.Ident {Name = "item"; Type = C.Void;}, "fn"), C.Ident {Name = delegatedName; Type = C.Void}))
                if hasCaptures then
                    C.DeclareIdent("captures", structCapturesNm |> C.CStruct)
                    for (name, ctype) in closedOverIdents do
                        C.Do(C.SetValue(C.GetField(C.Ident {Name = "captures"; Type = C.Void;}, name), C.Ident {Name = name; Type = C.Void}))
                    C.Do(C.SetValue(C.GetField(C.Ident {Name = "item"; Type = C.Void;}, "captures"),
                        C.FunctionCall(C.Ident { Name="Rc_New"; Type= C.Void},
                            [

                                C.FunctionCall(C.Ident { Name = "sizeof"; Type = C.Void }, [ C.Ident {Name = "captures"; Type = C.Void} ])
                                C.Unary(C.UnaryOp.RefOf, C.Ident {Name = "captures"; Type = C.Void} )
                                C.Const C.ConstNull
                            ]
                        )))
                // for (name, ctype) in closedOverIdents do
                //     C.Do(C.SetValue(C.GetField(C.Ident {Name = "item"; Type = C.Void;}, name), C.Ident {Name = name; Type = C.Void}))
                C.Assignment(["rc"],
                    C.FunctionCall(C.Ident { Name="Rc_New"; Type= C.Void},
                        [

                            C.FunctionCall(C.Ident { Name = "sizeof"; Type = C.Void }, [ C.Ident { Name = "item"; Type = C.Void }])
                            C.Unary(C.UnaryOp.RefOf, C.Ident { Name = "item"; Type = C.Void })
                            C.Const C.ConstNull
                        ]
                    ),
                    C.Rc (structClosureNm |> C.CStruct))
                C.Return (C.Ident { Name = "rc"; Type = structClosureNm |> C.CStruct |> C.Rc}, structClosureNm |> C.CStruct |> C.Rc)
            ],
            C.Rc C.Void
        )
        additionalDeclarations <-
            additionalDeclarations
            @ [
                if hasCaptures then
                    structCapturesDeclaration
                structClosureDeclaration
                functionDeclaration
                newStructClosureDeclaration ]

        //struct with captures
        C.FunctionCall(C.Ident {Name = structClosureNm + "_new"; Type = C.Void },
            closedOverIdents |> List.map (fun (name, t) -> C.Ident {Name = name; Type = t }) |> List.map CHelpers.clone)
    member this.GetAdditionalDeclarations() =
        additionalDeclarations
        |> List.distinct
    member this.GetAdditionalDeclarationsAndClear() =
        let decs = additionalDeclarations
                    |> List.distinct
        additionalDeclarations <- []
        decs
    member this.RegisterInclude(fInclude: Fable.AST.C.Include) =
        // failwithf "%A" com.LibraryDir
        includes <- includes |> Set.add fInclude
    member this.RegisterIdentSubstitution (oldIdent: string, newIdent: string) =
        identSubstitutions <- identSubstitutions |> Map.add oldIdent newIdent
    member this.GetIdentSubstitution oldValue =
        identSubstitutions |> Map.tryFind oldValue |> Option.defaultValue oldValue
    member this.GetIncludes() = includes |> Set.toList