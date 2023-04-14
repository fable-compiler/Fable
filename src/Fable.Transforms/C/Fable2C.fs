module rec Fable.Transforms.Fable2C

//cloned from FableToBabel

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.C
open Fable.Compilers.C
open Fable.Naming
open Fable.Core


module Transforms =
    module Helpers =
        let transformStatements transformStatements transformReturn exprs = [
                match exprs |> List.rev with
                | h::t ->
                    for x in t |> List.rev do
                        yield transformStatements x
                    yield transformReturn h
                | [] -> ()
            ]
        let ident name t = Ident { Name = name; Type = t }
        let voidIdent name = ident name Void
        let fcall args expr= FunctionCall(expr, args)
        let iife statements = FunctionCall(AnonymousFunc([], statements), [])
        let debugLog expr = FunctionCall(Helpers.ident "print" Void, [expr]) |> Do
        let libEquality a b=
            FunctionCall(GetObjMethod(FunctionCall(Helpers.ident "require" Void, [ConstString "./fable-lib/Util" |> Const]), "equals"), [a; b])
        let maybeIife = function
            | [] -> NoOp
            | [Return (expr, _)] -> expr
            | statements -> iife statements

        module Out =
            open Fable.AST.C
            let rec identUsesInExpr = function
                | FunctionCall(f, args) ->
                    f::args |> List.map identUsesInExpr |> List.collect id
                | Unary(_, expr) ->
                    identUsesInExpr expr
                | GetField(expr, name) ->
                    identUsesInExpr expr
                | Ident i ->
                    [i]
                | Const(_) -> []
                | Binary(_, l, r) -> identUsesInExpr l @ identUsesInExpr r
                | GetObjMethod(expr, name) -> identUsesInExpr expr
                | GetAtIndex(expr, idx) -> identUsesInExpr expr
                | SetValue(expr, value) -> identUsesInExpr expr @ identUsesInExpr value
                | SetExpr(a, b, value) -> identUsesInExpr a @ identUsesInExpr b @ identUsesInExpr value
                | Brackets(expr) -> identUsesInExpr expr
                | AnonymousFunc(args, body) -> failwith "Not Implemented"
                | Unknown(_) -> []
                | Macro(_, args) -> args |> List.collect identUsesInExpr
                // | Ternary(guardExpr, thenExpr, elseExpr) ->
                //     identUsesInExpr guardExpr @ identUsesInExpr thenExpr @ identUsesInExpr elseExpr
                | NoOp -> []
                // | Function(args, body) -> failwith "Not Implemented"
                | NewArr(values) ->
                    values |> List.collect identUsesInExpr
                | GetFieldThroughPointer(expr, name) ->
                    identUsesInExpr expr
                | Cast(_, expr) ->
                    identUsesInExpr expr
                | Comment(_) -> []
            let rec identUsesInSingleStatement = function
                | Return (expr, _) -> identUsesInExpr expr
                | Do expr -> identUsesInExpr expr
                | DeclareIdent(_, _) -> []
                | Assignment(names, expr, _) -> identUsesInExpr expr
                | SNoOp -> []
                | ForLoop(_, start, limit, body) ->
                    (identUsesInExpr start) @ (identUsesInExpr limit) @ (body |> List.collect identUsesInSingleStatement)
                | WhileLoop(guard, body) ->
                    (identUsesInExpr guard) @ (body |> List.collect identUsesInSingleStatement)
                | IfThenElse(guard, thenSt, elseSt) ->
                    (identUsesInExpr guard) @ (thenSt |> List.collect identUsesInSingleStatement) @ (elseSt |> List.collect identUsesInSingleStatement)
            let identUsesInStatements =
                List.collect identUsesInSingleStatement
                >> Set.ofList
            let unwrapRc tOut expr=
                Brackets(GetField(Cast(tOut |> Pointer, expr), "data"))



            let statementsToExpr (com: CCompiler) retType = function
                | [] -> NoOp
                | [Return (expr, _)] ->
                    expr
                | lst ->
                    let identsToCapture = identUsesInStatements lst
                    com.GenAndCallDeferredFunctionFromExpr(identsToCapture |> Set.toList, lst, retType)
                // | lst ->
                //     let captures = []
                    // com.CreateAdditionalDeclaration(FunctionDeclaration())
            // let addCleanupOnExit (com: CCompiler) t args statements =
            //     let locallyDeclaredIdents =
            //         statements |> List.choose(function
            //                                     | DeclareIdent(name, Rc t) -> Some (name, t)
            //                                     | _ -> None)
            //     let rcArgs = args |> List.filter (function
            //                                         | _, Rc t -> true
            //                                         | _ -> false )
            //     let rationalizedStatements =
            //         //there should only be a return statement in the tail call position
            //         match statements |> List.rev with
            //         | h::t ->
            //             let tNext =
            //                 t |> List.map(function
            //                         | Return x -> Do x
            //                         | x -> x)
            //             (h::tNext) |> List.rev
            //     let toCleanup = rcArgs @ locallyDeclaredIdents
            //     [
            //         for s in rationalizedStatements do
            //             match s with
            //             | Return r -> // where the scope ends, add clean up
            //                 // yield! toCleanup
            //                 // yield DeclareIdent("ret", t)
            //                 if toCleanup.Length > 0 then
            //                     yield Assignment(["ret"],r, t)
            //                     //cleanup
            //                     for (name, t) in toCleanup do
            //                         yield FunctionCall("Rc_Dispose" |> voidIdent, [Ident {Name = name; Type = t}]) |> Do
            //                     yield Return (Ident { Name="ret"; Type=t })
            //                 else
            //                     yield Return r
            //             | IfThenElse(guard, thenSt, elseSt) ->
            //                 yield IfThenElse(guard, thenSt, elseSt) //addCleanupOnExit com t toCleanup thenSt/elseSt
            //             | _ -> yield s
            //     ]

        let getEntityFieldsAsIdents (ent: Fable.Entity): Fable.Ident list =
            ent.FSharpFields
            |> Seq.map (fun field ->
                let name = field.Name
                let typ = FableTransforms.uncurryType field.FieldType
                let id: Fable.Ident = { makeTypedIdent typ name with IsMutable = field.IsMutable }
                id)
            |> Seq.toList

    module FCalls =
        let newRc expr t =
            match t with
            | C.Int ->
                FunctionCall(Ident { Name="Rc_New_Int"; Type = t},
                    [
                        expr
                    ]
                )
            | _ ->
                FunctionCall(Ident { Name="Rc_New"; Type = t},
                    [
                        FunctionCall(
                            Helpers.voidIdent "sizeof",[ expr ])
                        Unary(UnaryOp.RefOf, expr)
                        Const ConstNull
                    ]
                )
    //from rs
    let isClosedOverIdent com ctx (ident: Fable.Ident) =
        true
    //from rs
    let getIgnoredNames (name: string option) (args: Fable.Ident list) =
        let argNames = args |> List.map (fun arg -> arg.Name)
        let allNames = name |> Option.fold (fun xs x -> x :: xs) argNames
        allNames |> Set.ofList

    //from rs
    let tryFindClosedOverIdent com ctx (ignoredNames: HashSet<string>) expr =
        match expr with
        | Fable.IdentExpr ident ->
            if not (ignoredNames.Contains(ident.Name))
                && (isClosedOverIdent com ctx ident)
            then Some ident
            else None
        // add local names in the closure to the ignore list
        // TODO: not perfect, local name shadowing will ignore captured names
        | Fable.ForLoop(ident, _, _, _, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.Lambda(arg, _, _) ->
            ignoredNames.Add(arg.Name) |> ignore
            None
        | Fable.Delegate(args, body, name, _) ->
            args |> List.iter (fun arg ->
                ignoredNames.Add(arg.Name) |> ignore)
            None
        | Fable.Let(ident, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.LetRec(bindings, _) ->
            bindings |> List.iter (fun (ident, _) ->
                ignoredNames.Add(ident.Name) |> ignore)
            None
        | Fable.DecisionTree(_, targets) ->
            targets |> List.iter (fun (idents, _) ->
                idents |> List.iter (fun ident ->
                    ignoredNames.Add(ident.Name) |> ignore))
            None
        | _ ->
            None

    //from rs
    let getCapturedIdents com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames name args)
        let mutable capturedIdents = Map.empty
        let addClosedOver expr =
            tryFindClosedOverIdent com ctx ignoredNames expr
            |> Option.iter (fun ident ->
                capturedIdents <- capturedIdents |> Map.add ident.Name ident
            )
            false
        // collect all closed over names that are not arguments
        deepExists addClosedOver body |> ignore
        capturedIdents

    let transformValueKind (com: CCompiler) = function
        | Fable.NumberConstant(v, kind,_) ->
            let c =
                match kind, v with
                | Int16, (:? int16 as x) ->
                    ConstInt16(x)
                | Int32, (:? int32 as x) ->
                    ConstInt32(x)
                | _ -> ConstNull
            Const(c)
        | Fable.StringConstant(s) ->
            FunctionCall(Ident { Name="Rc_New"; Type= Char }, [
                ConstInt32(s.Length) |> Const
                ConstString s |> Const
                Const ConstNull
            ])
            //Const(ConstString s)
        | Fable.BoolConstant(b) ->
            Const(ConstBool b)
        | Fable.UnitConstant ->
            Const(ConstNull)
        | Fable.CharConstant(c) ->
            Const(ConstString (string c))
        // | Fable.EnumConstant(e,ref) ->
        //     convertExpr com e
        | Fable.NewRecord(values, ref, args) ->
            let entity = com.GetEntity(ref)
            if entity.IsFSharpRecord then
                let names = entity.FSharpFields |> List.map(fun f -> f.Name)
                let values = values |> List.map (transformExpr com)
                FunctionCall(Ident({ Name = entity.FullName.Replace(".", "_") + "_new"; Type = C.Void}), values)
            else sprintf "unknown ety %A %A %A %A" values ref args entity |> Unknown
        | Fable.NewAnonymousRecord(values, names, _, _) ->
            let transformedValues = values |> List.map (transformExpr com)
            FunctionCall(Ident({ Name = "anon" + "_new"; Type = C.Void}), transformedValues)
        | Fable.NewUnion(values, tag, entRef, _) ->
            let entity = com.GetEntity(entRef)
            let values = values |> List.map(transformExpr com)
            let tagM = entity.UnionCases[tag]
            FunctionCall(Ident({ Name = entity.FullName.Replace(".", "_") + "_" + tagM.Name + "_new"; Type = C.Void}), values)
        | Fable.NewOption (value, t, _) ->
            value |> Option.map (transformExpr com) |> Option.defaultValue (Const ConstNull)
        | Fable.NewTuple(values, isStruct) ->
            // let fields = values |> List.mapi(fun i x -> sprintf "p_%i" i, transformExpr com x)
            // NewObj(fields)
            NewArr(values |> List.map (transformExpr com))
        | Fable.NewArray(Fable.ArrayValues values, t, _) ->
            NewArr(values |> List.map (transformExpr com))
        | Fable.Null _ ->
            Const(ConstNull)
        | x -> sprintf "unknown %A" x |> ConstString |> Const

    let transformType (com: CCompiler) (t: Fable.Type) =
        let tOut =
            match t with
            | Fable.Type.Char -> Char
            | Fable.Type.Number(kind, info) ->
                match kind with
                | Int32 ->
                    Int
                | _ -> Void
            | Fable.Type.String ->
                Rc (Char)
            | Fable.Type.Unit ->
                Void
            | Fable.Type.DeclaredType (entRef, genArgs) ->
                let ent = com.GetEntity entRef
                if ent.IsFSharpRecord then
                    if ent.IsValueType then
                        ent.FullName.Replace(".", "_") |> CStruct
                    else
                        ent.FullName.Replace(".", "_") |> CStruct |> Rc
                elif ent.IsFSharpUnion then
                    ent.FullName.Replace(".", "_") |> CStruct |> Rc
                else Pointer Void
            | Fable.Type.GenericParam(name, false, constraints) ->
                Rc Void
            | Fable.Type.LambdaType(arg, returnType) ->
                Rc Void
            | _ ->
                sprintf "unrecognised %A" t |> CStruct
        tOut
    let isRcType (com: CCompiler) t =
        let cType = transformType com t
        match cType with
        | Rc _ -> true
        | _ -> false
    let transformCallIdentsWithTypes com =
        List.filter(fun (ident: Fable.Ident) -> match ident.Type with | Fable.Unit -> false | _ -> true)
        >> List.map(fun ident -> ident.Name, transformType com ident.Type)
    let transformOp com =
        let transformExpr = transformExpr com
        function
        | Fable.OperationKind.Binary(BinaryModulus, left, right) ->
             GetField(Helpers.ident "math" Void, "fmod") |> Helpers.fcall [transformExpr left; transformExpr right]
        | Fable.OperationKind.Binary (op, left, right) ->
            let op = match op with
                | BinaryMultiply -> Multiply
                | BinaryDivide -> Divide
                | BinaryEqual -> Equals
                | BinaryPlus -> Plus
                | BinaryMinus -> Minus
                | BinaryEqualStrict -> Equals
                | BinaryUnequal -> Unequal
                | BinaryUnequalStrict -> Unequal
                | BinaryLess -> Less
                | BinaryGreater -> Greater
                | BinaryLessOrEqual -> LessOrEqual
                | BinaryGreaterOrEqual -> GreaterOrEqual
                | x -> sprintf "%A" x |> BinaryTodo
            Binary(op, transformExpr left, transformExpr right )
        | Fable.OperationKind.Unary (op, expr) ->
            match op with
            | UnaryNotBitwise -> transformExpr expr //not sure why this is being added
            | UnaryNot -> Unary(Not, transformExpr expr)
            | UnaryVoid -> NoOp
            | _ -> sprintf "%A %A" op expr |> Unknown
        | x -> Unknown(sprintf "%A" x)

    let shouldBox expectedType (actualType: Fable.Type) =
        match expectedType, actualType with
        | Fable.Type.GenericParam _, Fable.Type.Number _ ->
            true
        | Fable.Type.GenericParam _, Fable.Type.Boolean _ ->
            true
        | _ -> false

    let transformCallArgs (com: CCompiler) (memberRef: Fable.AST.Fable.MemberRef option) args=
        match args with
        | [] -> []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant, _))] -> []
        | args ->
            let parameters =
                memberRef |> Option.map com.GetMember |> Option.map (fun m -> m.CurriedParameterGroups |> List.concat) |> Option.defaultValue []

            args |> List.mapi (fun idx arg ->
                        let shouldBox = parameters |> List.tryItem idx |> Option.map (fun p -> shouldBox p.Type arg.Type) |> Option.defaultValue false
                        if shouldBox then
                            FCalls.newRc (transformLeaveContext com arg) (transformType com arg.Type)
                        else transformLeaveContext com arg
                        )

    let transformExprAsStatements (com: CCompiler) (expr: Fable.Expr) : Statement list =
        let transformExpr = transformExpr com
        let transformOp = transformOp com
        let singletonStatement outExpr =
            match expr.Type with
            | Fable.Type.Unit -> [Do outExpr]
            | _ -> [Return (outExpr, transformType com expr.Type)]

        match expr with
        | Fable.Expr.Value(value, _) -> transformValueKind com value |> singletonStatement
        | Fable.Expr.Call(expr, callInfo, t, r) ->
            let lhs =
                match expr with
                // | Fable.Expr.IdentExpr i ->
                //     let ptr =
                //         transformExpr expr |> Helpers.Out.unwrapRc Void
                //     GetFieldThroughPointer(ptr, "fn")
                | Fable.Expr.Get(expr, Fable.GetKind.FieldGet(fi), t, _) ->
                    match t with
                    | Fable.DeclaredType(_, _)
                    | Fable.AnonymousRecordType(_, _, _) ->
                        GetObjMethod(transformExpr expr, fi.Name)
                    | _ -> transformExpr expr
                | Fable.Expr.Delegate _ ->
                    transformExpr expr |> Brackets
                | _ -> transformExpr expr

            let args = transformCallArgs com callInfo.MemberRef callInfo.Args
            //let mref = callInfo.MemberRef |> Option.map com.GetMember
            //sprintf "%A" expr |> Unknown |> singletonStatement
            FunctionCall(lhs, args) |> singletonStatement
        | Fable.Expr.Import (info, t, r) ->
                // match info.Kind, info.Path with
                // | LibraryImport, Regex "fable-lib\/(\w+).(?:fs|js)" [name] ->
                //     "fable-lib/" + name
                // | LibraryImport, Regex "fable-library-c\/fable\/fable-library\/(\w+).(?:fs|js)" [name] ->
                //     "fable-lib/fable-library" + name
                // | LibraryImport, Regex "fable-library-c\/fable\/(\w+).(?:fs|js)" [name] ->
                //     "fable-lib/" + name
                // | _ ->
                //     info.Path.Replace(".fs", "").Replace(".js", "") //todo - make less brittle
            com.RegisterInclude({Name = info.Path.Replace(".fs",".c"); IsBuiltIn = false})
            let fullName =
                match info.Kind with
                | Fable.UserImport _ -> info.Selector
                | Fable.LibraryImport x ->  info.Selector
                | Fable.MemberImport m ->
                    let mm = com.GetMember m
                    mm.FullName.Replace(".", "_")
                | Fable.ClassImport c ->
                    c.FullName.Replace(".", "_")

            Ident { Name = fullName; Type = transformType com t } |> singletonStatement
        | Fable.Expr.IdentExpr(i) when i.Name <> "" ->
            let name = com.GetIdentSubstitution i.Name
            Ident { Name = name; Type = transformType com i.Type } |> singletonStatement
        | Fable.Expr.Operation (kind, _, _, _) ->
            transformOp kind |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.FieldGet(fi), t, _) ->
            match transformType com expr.Type with
            | Rc tOut ->
                let ptr =
                    transformExpr expr |> Helpers.Out.unwrapRc tOut
                GetFieldThroughPointer(ptr, fi.Name)
            | _ ->
                GetField(transformExpr expr, fi.Name)
            |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.UnionField(fi), _, _) ->
            let outExpr = transformExpr expr
            let ety = com.GetEntity fi.Entity
            let case = ety.UnionCases |> List.item fi.CaseIndex
            let structName = ety.FullName.Replace(".", "_") + "_" + case.Name
            let ptr =  Helpers.Out.unwrapRc (CStruct structName) outExpr
            let field = case.UnionCaseFields |> List.item fi.FieldIndex
            //failwithf "%A" (case, ety, ety.UnionCases, expr.Type)
            GetFieldThroughPointer(ptr, field.Name) |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.ExprGet(e), _, _) ->
            GetAtIndex(transformExpr expr, transformExpr e) |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.TupleIndex(i), _, _) ->
            GetAtIndex(transformExpr expr, Const (ConstInt32 i)) |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.OptionValue, _, _) ->
            transformExpr expr |> singletonStatement //todo null check, throw if null?
        | Fable.Expr.Set(expr, Fable.SetKind.ValueSet, t, value, _) ->
            SetValue(transformExpr expr, transformExpr value) |> singletonStatement
        | Fable.Expr.Set(expr, Fable.SetKind.ExprSet(e), t, value, _) ->
            SetExpr(transformExpr expr, transformExpr e, transformExpr value) |> singletonStatement
        | Fable.Expr.Sequential exprs ->
            exprs |> List.map (transformExprAsStatements com) |> List.collect id
        | Fable.Expr.Let (ident, value, body) ->
            let shouldBox = shouldBox ident.Type value.Type
            let outType =
                if shouldBox then transformType com value.Type |> Rc else transformType com value.Type
            [
                yield sprintf "%A" value.Type |> Comment |> Do
                yield Assignment([ident.Name], transformExpr value, outType)
                yield! transformExprAsStatements com body
            ]
        | Fable.Expr.Emit(m, _, _) ->
            // let argsExprs = m.CallInfo.Args |> List.map transformExpr
            // let macroExpr = Macro(m.Macro, argsExprs)
            // let exprs =
            //     argsExprs
            //     @ [macroExpr]
            // asSingleExprIife exprs
            Macro(m.Macro, m.CallInfo.Args |> transformCallArgs com m.CallInfo.MemberRef) |> singletonStatement
        | Fable.Expr.DecisionTree(expr, lst) ->
            com.DecisionTreeTargets(lst)
            transformExpr expr |> singletonStatement
        | Fable.Expr.DecisionTreeSuccess(i, boundValues, _) ->
            let idents,target = com.GetDecisionTreeTargets(i)
            if idents.Length = boundValues.Length then
                let statements =
                    [   for (ident, value) in List.zip idents boundValues do
                            yield Assignment([ident.Name], transformExpr value, transformType com value.Type)
                        yield Return (transformExpr target, transformType com target.Type)
                            ]
                statements
                // |> Helpers.maybeIife
            else sprintf "not equal lengths %A %A" idents boundValues |> Unknown |> singletonStatement
        | Fable.Expr.Lambda(arg, body, name) ->
            //let closedOverIdents
            let bodyStmnts = transformExprAsStatements com body
            let identsToCapture =
                getCapturedIdents com () None [arg] body
                |> Map.toList
                |> List.map (snd >> fun ident -> ident.Name, ident.Type |> transformType com)
            let res = com.GenAndCallDeferredClosureFromExpr(
                        expr.Type,
                        [{Name = arg.Name; Type = transformType com arg.Type}],
                        identsToCapture,
                        bodyStmnts,
                        transformType com body.Type)
            // [ sprintf "%A" expr.Type |> Comment |> Do ]
            // @ (res |> singletonStatement)
            res |> singletonStatement
            // Function([arg.Name], transformExprAsStatements com body) |> singletonStatement
        | Fable.Expr.CurriedApply(applied, args, t, _) ->
            match applied with
            | Fable.Expr.IdentExpr i ->
                // i.Type
                //todo need to get to
                //struct Rc resr2 = ((struct delegatedclosure_1742910231*)f.data)->fn(x);
                //let tOut = com.GenFunctionSignatureAlias(args |> List.map (fun a -> a.Type |> transformType com), transformType com t)
                let ptr =
                    transformExpr applied
                let ptrUnwrapped =
                    let closureTmpl = CStruct "FnClosure1"
                    ptr
                    |> Helpers.Out.unwrapRc closureTmpl
                let tagValExpr = GetFieldThroughPointer(ptrUnwrapped, "fn")
                let called = FunctionCall(tagValExpr,  ptr::(args |> List.map transformExpr) |> List.map CHelpers.clone)
                [
                    sprintf "%A" i.Type |> Comment |> Do
                ] @ (singletonStatement called)
                // sprintf "%A" expr |> Unknown |> singletonStatement
            | _ ->
                FunctionCall(transformExpr applied, args |> List.map transformExpr) |> singletonStatement
        | Fable.Expr.IfThenElse (guardExpr, thenExpr, elseExpr, _) ->
            [IfThenElse(transformExpr guardExpr, transformExprAsStatements com thenExpr, transformExprAsStatements com elseExpr)]
        | Fable.Test(expr, kind, b) ->
            match kind with
            | Fable.UnionCaseTest i->
                match expr.Type with
                | Fable.DeclaredType(entRef, genArgs) ->
                    let ent = com.GetEntity(entRef)
                    assert(ent.IsFSharpUnion)
                    let unionCase = ent.UnionCases |> List.head
                    let structName = ent.FullName.Replace(".", "_") + "_" + unionCase.Name
                    let tOut = CStruct (structName)
                    let ptr =
                        transformExpr expr |> Helpers.Out.unwrapRc tOut
                    let tagValExpr = GetFieldThroughPointer(ptr, "tag")
                    Binary(Equals, tagValExpr , Const (ConstInt32 i)) |> singletonStatement
                | _ ->
                    Binary(Equals, GetField(transformExpr expr, "tag") , Const (ConstInt32 i)) |> singletonStatement
            | Fable.OptionTest isSome ->
                if isSome then Binary(Unequal, Const ConstNull, transformExpr expr) else Binary(Equals, Const ConstNull, transformExpr expr)
                |> singletonStatement
            | Fable.TestKind.TypeTest t ->
                // match t with
                // | Fable.DeclaredType (ent, genArgs) ->
                //     match ent.FullName with
                //     | Fable.Transforms.Types.ienumerable -> //isArrayLike
                //     | Fable.Transforms.Types.array
                //     | _ ->
                // | _ -> ()
                Binary(Equals, GetField(transformExpr expr, "type"), Const (t.ToString() |> ConstString))
                |> singletonStatement
            | _ ->
                Unknown(sprintf "test %A %A" expr kind)
                |> singletonStatement
        | Fable.Extended(Fable.ExtendedSet.Throw(expr, _), t) ->
            // let errorExpr =
            //     Const (ConstString ("There was an error, todo " + sprintf "%A" expr))
                //transformExpr expr
            FunctionCall(Helpers.ident "error" Void, expr |> Option.map transformExpr |> Option.toList)
            |> singletonStatement
        | Fable.Extended(Fable.ExtendedSet.Curry(expr, d), _) ->
            transformExpr expr
            |> sprintf "todo curry %A"
            |> Unknown
            |> singletonStatement
        | Fable.Delegate(idents, body, _, _) ->
            // Function(idents |> List.map(fun i -> i.Name), transformExprAsStatements com body) //can be flattened
            // |> singletonStatement
            sprintf "%A" expr |> Unknown |> singletonStatement
        | Fable.ForLoop(ident, start, limit, body, isUp, _) ->
            [ForLoop(ident.Name, transformExpr start, transformExpr limit, transformExprAsStatements com body)]
        | Fable.TypeCast(expr, t) ->
            transformExprAsStatements com expr //typecasts are meaningless
        | Fable.WhileLoop(guard, body, range) ->
            [
                WhileLoop(transformExpr guard, transformExprAsStatements com body)
            ]
        | Fable.TryCatch(body, catch, finalizer, _) ->
            [
                // Assignment(["status"; "resOrErr"], FunctionCall(Helpers.ident "pcall" Void, [
                //     Function([], [
                //         transformExpr body |> Return
                //     ])
                // ]), transformType com body.Type)
                let finalizer = finalizer |> Option.map transformExpr
                let catch = catch |> Option.map (fun (ident, expr) -> ident.Name, transformExpr expr, transformType com expr.Type)
                IfThenElse(Helpers.ident "status" Void, [
                    match finalizer with
                    | Some finalizer -> yield Do finalizer
                    | None -> ()
                    yield Return (Helpers.ident "resOrErr" Void, Void)
                ], [
                    match catch with
                    | Some(ident, expr, t) ->
                        yield Return (expr, t)
                    | _ -> ()
                ])
            ]
        | x -> [Unknown (sprintf "%A" x) |> Do]
    let transformExpr com expr =
        let retType = transformType com expr.Type
        transformExprAsStatements com expr
        |> Helpers.Out.statementsToExpr com retType

    let transformLeaveContext com expr =
        let outExpr = transformExpr com expr
        let isOnlyReference =
            match expr with
            | Fable.Let _
            | Fable.Call _
            | Fable.CurriedApply _
            | Fable.Value(_, _)
            | Fable.Operation(Fable.Binary _, _, _, _)
            | Fable.Lambda _
            | Fable.Delegate _
            | Fable.IfThenElse _
            | Fable.DecisionTree _
            | Fable.DecisionTreeSuccess _
            | Fable.Sequential _
            | Fable.ForLoop _ ->
                true
            | _ -> false
        if isRcType com expr.Type && not (isOnlyReference) then
            FunctionCall(Helpers.voidIdent("Rc_Clone"), [outExpr])
        else outExpr

    let transformDeclarations (com: CCompiler) = function
        | Fable.ModuleDeclaration m ->
            []
        | Fable.MemberDeclaration m ->
            // if m.Args.Length = 0 then
            //     Assignment([m.Name], transformExpr com m.Body, transformType com m.Body.Type)
            // else

            // let unwrapSelfExStatements =
            //     match transformExpr com m.Body |> Return |> flattenReturnIifes with
            //     | Return (FunctionCall(AnonymousFunc([], statements), [])) ->
            //         statements
            //     | s -> [s]
            // match m.MemberRef with
            // | MemberRef(ety, _) -> com.GetEntity(ety)
            // failwithf "%A" m
            let mr = com.GetMember(m.MemberRef)
            let isEntryPoint = mr.Attributes |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.entryPoint) |> Option.isSome
            let t = transformType com m.Body.Type
            let args = if isEntryPoint then [] else m.Args |> transformCallIdentsWithTypes com
            let body = transformExprAsStatements com m.Body// |> Helpers.Out.addCleanupOnExit com t args
            let finalName =
                if mr.Attributes |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.entryPoint) |> Option.isSome then
                    "main"
                else
                    mr.FullName.Replace(".", "_")
            com.RegisterIdentSubstitution(mr.CompiledName, finalName)
            [FunctionDeclaration(finalName, args, body, t)]
        | Fable.ClassDeclaration(d) ->
            let ent = com.GetEntity(d.Entity)
            if ent.IsFSharpRecord then
                if ent.IsValueType then
                    let idents = Transforms.Helpers.getEntityFieldsAsIdents ent
                    let fields = idents |> List.map (fun i -> i.Name, transformType com i.Type)
                    let cdIdent = { Name = "item"; Type = Void }
                    [
                        StructDeclaration(ent.FullName.Replace(".", "_"), fields)
                        FunctionDeclaration(ent.FullName.Replace(".", "_") + "_new", fields, [
                            DeclareIdent("item", ent.FullName.Replace(".", "_") |> CStruct)
                            for (name, ctype) in fields do
                                Do(SetValue(GetField(Ident {Name = "item"; Type = Void;}, name), Ident {Name = name; Type = Void}))
                            Return (Ident cdIdent, cdIdent.Type)
                        ], ent.FullName.Replace(".", "_") |> CStruct)
                    ]
                else
                    let idents = Transforms.Helpers.getEntityFieldsAsIdents ent
                    let fields = idents |> List.map (fun i -> i.Name, transformType com i.Type)
                    let cdIdent = { Name = "item"; Type = ent.FullName.Replace(".", "_") |> CStruct }
                    let rcIdent = { Name = "rc"; Type = ent.FullName.Replace(".", "_") |> CStruct |> Rc}
                    [
                        StructDeclaration(ent.FullName.Replace(".", "_"), fields)
                        FunctionDeclaration(ent.FullName.Replace(".", "_") + "_new", fields, [
                            DeclareIdent("item", ent.FullName.Replace(".", "_") |> CStruct)
                            for (name, ctype) in fields do
                                Do(SetValue(GetField(Ident cdIdent, name), Ident {Name = name; Type = ctype}))
                            Assignment(["rc"],
                                FCalls.newRc (Helpers.voidIdent "item") Void,
                                // FunctionCall(Ident { Name="Rc_New"; Type= Void},
                                //     [
                                //         FunctionCall(
                                //             Helpers.voidIdent "sizeof",[ Helpers.voidIdent "item"])
                                //         Unary(UnaryOp.RefOf, Helpers.voidIdent "item")
                                //         Const ConstNull
                                //     ]
                                // ),
                                Rc (ent.FullName.Replace(".", "_") |> CStruct))
                            Return (Ident rcIdent, rcIdent.Type)
                        ], Rc (ent.FullName.Replace(".", "_") |> CStruct))
                    ]
            elif ent.IsFSharpUnion then
                [
                    for i, case in ent.UnionCases |> List.mapi (fun i x -> i, x) do
                        let fields =
                            case.UnionCaseFields
                                    |> List.map (fun f -> f.Name, transformType com f.FieldType)
                        let fieldsIncTag =
                            ["tag", Int] @ fields
                        let structName = ent.FullName.Replace(".", "_") + "_" + case.Name
                        yield StructDeclaration(structName, fieldsIncTag)
                        yield FunctionDeclaration(structName + "_new", fields, [
                            let cdIdent = { Name = "item"; Type = CStruct structName }
                            let rcIdent = { Name = "rc"; Type = Rc (CStruct structName)}
                            DeclareIdent("item", CStruct structName)
                            Do(SetValue(GetField(Ident cdIdent, "tag"), ConstInt32 i |> Const))
                            for (name, ctype) in fields do
                                Do(SetValue(GetField(Ident cdIdent, name), Ident {Name = name; Type = ctype}))
                            Assignment(["rc"],
                                // FunctionCall(Ident { Name="Rc_New"; Type= Void},
                                //     [
                                //         FunctionCall(
                                //             Helpers.voidIdent "sizeof",[ Helpers.voidIdent "item"])
                                //         Unary(UnaryOp.RefOf, Helpers.voidIdent "item")
                                //         Const ConstNull
                                //     ]
                                // ),
                                FCalls.newRc (Helpers.voidIdent "item") Void,
                                Rc (ent.FullName.Replace(".", "_") |> CStruct))
                            Return (Ident rcIdent, rcIdent.Type)
                        ], Rc (ent.FullName.Replace(".", "_") |> CStruct))
                ]
            else
                []
        | x -> []

let rec sanitizeReturnStatements = function
    | h::[] ->
        h::[]
    | h::t ->
        let hNext =
            match h with
            | Return (expr, t) ->
                Do expr
            | h -> h
        hNext::(sanitizeReturnStatements t)
    | [] -> []


let transformDeclPostprocess = function
    | FunctionDeclaration(name, args, statements, Void) ->
        let statements =
            statements
            |> List.filter(function
                | Return (Const ConstNull, _) -> false
                | Do (Const(ConstNull)) -> false
                | _ -> true)
        let statements = sanitizeReturnStatements statements
        FunctionDeclaration(name, args, statements, Void)
    | x -> x

let rec collectNewStatementsWithCleanup (ownedRcIdents, statementsOutRev, hasCleanedUp) statements =
    // let recurse = findIdentsWithRc
    match statements with
    | h::t ->
        let acc =
            match h with
            | DeclareIdent (name, Rc t) ->
                ((name, t)::ownedRcIdents, h::statementsOutRev, false)
            | Assignment ([name], _, Rc t) ->
                ((name, t)::ownedRcIdents, h::statementsOutRev, false)
            | Return (expr, t) ->
                let statements = [
                    let ownedRcIdents =
                        match expr with
                        | C.Ident i -> //if we return this, we transfer ownership, so do not clean up
                            ownedRcIdents |> List.filter(fun (name, _) -> name <> i.Name)
                        | _ -> ownedRcIdents
                    if ownedRcIdents.Length > 0 then
                        yield Assignment(["ret"], expr, t)
                        //cleanup
                        for (name, t) in ownedRcIdents do
                            yield FunctionCall("Rc_Dispose" |> Transforms.Helpers.voidIdent, [Ident {Name = name; Type = t}]) |> Do
                        yield Return (Ident { Name="ret"; Type= t }, t)
                    else
                        yield Return (expr, t)
                ]
                (ownedRcIdents, (statements |> List.rev) @ statementsOutRev, true)
            | IfThenElse (guard, thenSt, elseSt) ->
                let (_, thenStRev, _) = collectNewStatementsWithCleanup (ownedRcIdents, [], false) thenSt
                let (_, elseStRev, _) = collectNewStatementsWithCleanup (ownedRcIdents, [], false) elseSt
                (ownedRcIdents, IfThenElse(guard, thenStRev |> List.rev, elseStRev |> List.rev)::statementsOutRev, true)
            | _ ->
                (ownedRcIdents, h::statementsOutRev, false)
        collectNewStatementsWithCleanup acc t
    | [] ->
        if hasCleanedUp then
            (ownedRcIdents, statementsOutRev, true)
        else
            let statements = [
                for (name, t) in ownedRcIdents do
                    yield FunctionCall("Rc_Dispose" |> Transforms.Helpers.voidIdent, [Ident {Name = name; Type = t}]) |> Do
            ]
            (ownedRcIdents, (statements |> List.rev) @ statementsOutRev, true)

let buildNewStatementsWithGc args statements =
    let (idents, statementsOutRev, _) = collectNewStatementsWithCleanup (args, [], false) statements
    List.rev statementsOutRev

let transformDeclGc = function
    | FunctionDeclaration(name, args, statements, t) ->
        let argsWithOwnedRc = args |> List.filter(function | name, Rc t -> true | _ -> false)
        let statements = buildNewStatementsWithGc argsWithOwnedRc statements
        FunctionDeclaration(name, args, statements, t)
    | x -> x

let transformSanitizeReturnStatements = function
    | FunctionDeclaration(name, args, statements, t) ->
        let statements = sanitizeReturnStatements statements
        FunctionDeclaration(name, args, statements, t)
    | x -> x

let transformFile com (file: Fable.File): File =
    let builtInIncludes =
        [
            { Name = "stdio.h"; IsBuiltIn = true }
            { Name = "assert.h"; IsBuiltIn = true }
            { Name = getLibPath com "native"; IsBuiltIn = false }
            { Name = getLibPath com "closure"; IsBuiltIn = false }
            { Name = getLibPath com "rc"; IsBuiltIn = false }
        ]
    let comp = CCompiler(com)

    let declarations =
        file.Declarations
        |> List.collect (fun dec ->
                            let stdDecs = Transforms.transformDeclarations comp dec
                            let additionalDecs = comp.GetAdditionalDeclarationsAndClear()
                            additionalDecs @ stdDecs)
        |> List.map (transformDeclPostprocess >> transformDeclGc >> transformSanitizeReturnStatements)
    {
        Filename = com.CurrentFile
        Includes = builtInIncludes @ comp.GetIncludes()
        Declarations =  declarations
        ASTDebug = sprintf "%A" file.Declarations
    }