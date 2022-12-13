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
            | [Return expr] -> expr
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
                | Unknown(_) -> failwith "Not Implemented"
                | Macro(_, args) -> args |> List.collect identUsesInExpr
                // | Ternary(guardExpr, thenExpr, elseExpr) ->
                //     identUsesInExpr guardExpr @ identUsesInExpr thenExpr @ identUsesInExpr elseExpr
                | NoOp -> failwith "Not Implemented"
                | Function(args, body) -> failwith "Not Implemented"
                | NewArr(values) ->
                    values |> List.collect identUsesInExpr
                | GetFieldThroughPointer(expr, name) ->
                    identUsesInExpr expr
                | Cast(_, expr) ->
                    identUsesInExpr expr
            let rec identUsesInSingleStatement = function
                | Return expr -> identUsesInExpr expr
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
                | [Return expr] ->
                    expr
                | lst ->
                    let identsToCapture = identUsesInStatements lst
                    com.GenAndCallDeferredFunctionFromExpr(identsToCapture |> Set.toList, lst, retType)
                // | lst ->
                //     let captures = []
                    // com.CreateAdditionalDeclaration(FunctionDeclaration())
            let addCleanupOnExit (com: CCompiler) t args statements =
                let locallyDeclaredIdents =
                    statements |> List.choose(function
                                                | DeclareIdent(name, Rc t) -> Some (name, t)
                                                | _ -> None)
                let rcArgs = args |> List.filter (function
                                                    | _, Rc t -> true
                                                    | _ -> false )
                let toCleanup = rcArgs @ locallyDeclaredIdents
                [
                    for s in statements do
                        match s with
                        | Return r -> // where the scope ends, add clean up
                            // yield! toCleanup
                            // yield DeclareIdent("ret", t)
                            yield Assignment(["ret"],r, t)
                            //cleanup
                            for (name, t) in toCleanup do
                                yield FunctionCall("Rc_Dispose" |> voidIdent, [Ident {Name = name; Type = t}]) |> Do
                            yield Return (Ident { Name="ret"; Type=t })
                        | IfThenElse(guard, thenSt, elseSt) ->
                            yield IfThenElse(guard, addCleanupOnExit com t toCleanup thenSt, addCleanupOnExit com t toCleanup elseSt)
                        | _ -> yield s
                ]

        let getEntityFieldsAsIdents (ent: Fable.Entity): Fable.Ident list =
            ent.FSharpFields
            |> Seq.map (fun field ->
                let name = field.Name
                let typ = FableTransforms.uncurryType field.FieldType
                let id: Fable.Ident = { makeTypedIdent typ name with IsMutable = field.IsMutable }
                id)
            |> Seq.toList
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
            Const(ConstString s)
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
                FunctionCall(Ident({ Name = entity.CompiledName + "_new"; Type = C.Void}), values)
            else sprintf "unknown ety %A %A %A %A" values ref args entity |> Unknown
        | Fable.NewAnonymousRecord(values, names, _, _) ->
            let transformedValues = values |> List.map (transformExpr com)
            FunctionCall(Ident({ Name = "anon" + "_new"; Type = C.Void}), transformedValues)
        | Fable.NewUnion(values, tag, entRef, _) ->
            let entity = com.GetEntity(entRef)
            let values = values |> List.map(transformExpr com)
            let tagM = entity.UnionCases[tag]
            FunctionCall(Ident({ Name = entity.CompiledName + "_" + tagM.Name + "_new"; Type = C.Void}), values)
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
        match t with
        | Fable.Type.Char -> Char
        | Fable.Type.Number(kind, info) ->
            match kind with
            | Int32 ->
                Int
            | _ -> Void
        | Fable.Type.String ->
            Array Char
        | Fable.Type.Unit ->
            Void
        | Fable.Type.DeclaredType (entRef, genArgs) ->
            let ent = com.GetEntity entRef
            if ent.IsFSharpRecord then
                if ent.IsValueType then
                    CStruct ent.CompiledName
                else
                    CStruct ent.CompiledName |> Rc
            elif ent.IsFSharpUnion then
                CStruct ent.CompiledName |> Rc
            else Pointer Void
        | _ ->
            sprintf "unrecognised %A" t |> CStruct
            //Pointer Void
    let isRcType (com: CCompiler) t =
        let cType = transformType com t
        match cType with
        | Rc _ -> true
        | _ -> false
    let transformCallArgsWithTypes com =
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

    let transformCallArgs com args =
        match args with
        | [] -> []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant, _))] -> []
        | args ->
            args |> List.map (fun arg -> transformLeaveContext com arg)

    let transformExprAsStatements (com: CCompiler) (expr: Fable.Expr) : Statement list =
        let transformExpr = transformExpr com
        let transformOp = transformOp com
        let singletonStatement expr = [Return expr]

        match expr with
        | Fable.Expr.Value(value, _) -> transformValueKind com value |> singletonStatement
        | Fable.Expr.Call(expr, callInfo, t, r) ->
            let lhs =
                match expr with
                | Fable.Expr.Get(expr, Fable.GetKind.FieldGet(fi), t, _) ->
                    match t with
                    | Fable.DeclaredType(_, _)
                    | Fable.AnonymousRecordType(_, _, _) ->
                        GetObjMethod(transformExpr expr, fi.Name)
                    | _ -> transformExpr expr
                | Fable.Expr.Delegate _ ->
                    transformExpr expr |> Brackets
                | _ -> transformExpr expr
            let args = transformCallArgs com callInfo.Args
            FunctionCall(lhs, args) |> singletonStatement
        | Fable.Expr.Import (info, t, r) ->
            let path = "todo"
                // match info.Kind, info.Path with
                // | LibraryImport, Regex "fable-lib\/(\w+).(?:fs|js)" [name] ->
                //     "fable-lib/" + name
                // | LibraryImport, Regex "fable-library-c\/fable\/fable-library\/(\w+).(?:fs|js)" [name] ->
                //     "fable-lib/fable-library" + name
                // | LibraryImport, Regex "fable-library-c\/fable\/(\w+).(?:fs|js)" [name] ->
                //     "fable-lib/" + name
                // | _ ->
                //     info.Path.Replace(".fs", "").Replace(".js", "") //todo - make less brittle
            let rcall = FunctionCall(Ident { Name= "require"; Type = Void; }, [Const (ConstString path)])
            match info.Selector with
            | "" -> rcall |> singletonStatement
            | s -> GetObjMethod(rcall, s) |> singletonStatement
        | Fable.Expr.IdentExpr(i) when i.Name <> "" ->
            Ident { Name = i.Name; Type = transformType com i.Type } |> singletonStatement
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
            let structName = ety.CompiledName + "_" + case.Name
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
            [
                yield Assignment([ident.Name], transformExpr value, transformType com value.Type)
                yield! transformExprAsStatements com body
            ]
        | Fable.Expr.Emit(m, _, _) ->
            // let argsExprs = m.CallInfo.Args |> List.map transformExpr
            // let macroExpr = Macro(m.Macro, argsExprs)
            // let exprs =
            //     argsExprs
            //     @ [macroExpr]
            // asSingleExprIife exprs
            Macro(m.Macro, m.CallInfo.Args |> List.map transformExpr) |> singletonStatement
        | Fable.Expr.DecisionTree(expr, lst) ->
            com.DecisionTreeTargets(lst)
            transformExpr expr |> singletonStatement
        | Fable.Expr.DecisionTreeSuccess(i, boundValues, _) ->
            let idents,target = com.GetDecisionTreeTargets(i)
            if idents.Length = boundValues.Length then
                let statements =
                    [   for (ident, value) in List.zip idents boundValues do
                            yield Assignment([ident.Name], transformExpr value, transformType com value.Type)
                        yield transformExpr target |> Return
                            ]
                statements
                // |> Helpers.maybeIife
            else sprintf "not equal lengths %A %A" idents boundValues |> Unknown |> singletonStatement
        | Fable.Expr.Lambda(arg, body, name) ->
            Function([arg.Name], transformExprAsStatements com body) |> singletonStatement
        | Fable.Expr.CurriedApply(applied, args, _, _) ->
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
                    let structName = ent.CompiledName + "_" + unionCase.Name
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
            Function(idents |> List.map(fun i -> i.Name), transformExprAsStatements com body) //can be flattened
            |> singletonStatement
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
                Assignment(["status"; "resOrErr"], FunctionCall(Helpers.ident "pcall" Void, [
                    Function([], [
                        transformExpr body |> Return
                    ])
                ]), transformType com body.Type)
                let finalizer = finalizer |> Option.map transformExpr
                let catch = catch |> Option.map (fun (ident, expr) -> ident.Name, transformExpr expr)
                IfThenElse(Helpers.ident "status" Void, [
                    match finalizer with
                    | Some finalizer -> yield Do finalizer
                    | None -> ()
                    yield Helpers.ident "resOrErr" Void |> Return
                ], [
                    match catch with
                    | Some(ident, expr) ->
                        yield expr |> Return
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
            let t = transformType com m.Body.Type
            let args = m.Args |> transformCallArgsWithTypes com
            let body = transformExprAsStatements com m.Body |> Helpers.Out.addCleanupOnExit com t args
            [FunctionDeclaration(m.Name, args, body, t)]
        | Fable.ClassDeclaration(d) ->
            let ent = com.GetEntity(d.Entity)
            if ent.IsFSharpRecord then
                if ent.IsValueType then
                    let idents = Transforms.Helpers.getEntityFieldsAsIdents ent
                    let fields = idents |> List.map (fun i -> i.Name, transformType com i.Type)
                    let cdIdent = Ident { Name = "item"; Type = Void }
                    [
                        StructDeclaration(ent.CompiledName, fields)
                        FunctionDeclaration(ent.CompiledName + "_new", fields, [
                            DeclareIdent("item", CStruct d.Name)
                            for (name, ctype) in fields do
                                Do(SetValue(GetField(Ident {Name = "item"; Type = Void;}, name), Ident {Name = name; Type = Void}))
                            Return (cdIdent)
                        ], CStruct ent.CompiledName)
                    ]
                else
                    let idents = Transforms.Helpers.getEntityFieldsAsIdents ent
                    let fields = idents |> List.map (fun i -> i.Name, transformType com i.Type)
                    let cdIdent = { Name = "item"; Type = CStruct d.Name }
                    let rcIdent = { Name = "rc"; Type = Rc (CStruct d.Name)}
                    [
                        StructDeclaration(ent.CompiledName, fields)
                        FunctionDeclaration(ent.CompiledName + "_new", fields, [
                            DeclareIdent("item", CStruct d.Name)
                            for (name, ctype) in fields do
                                Do(SetValue(GetField(Ident cdIdent, name), Ident {Name = name; Type = ctype}))
                            Assignment(["rc"],
                                FunctionCall(Ident { Name="Rc_New"; Type= Void},
                                    [
                                        FunctionCall(
                                            Helpers.voidIdent "sizeof",[ Helpers.voidIdent "item"])
                                        Unary(UnaryOp.RefOf, Helpers.voidIdent "item")
                                        Const ConstNull
                                    ]
                                ),
                                Rc (CStruct d.Name))
                            Return (Ident rcIdent)
                        ], Rc (CStruct d.Name))
                    ]
            elif ent.IsFSharpUnion then
                [
                    for i, case in ent.UnionCases |> List.mapi (fun i x -> i, x) do
                        let fields =
                            case.UnionCaseFields
                                    |> List.map (fun f -> f.Name, transformType com f.FieldType)
                        let fieldsIncTag =
                            ["tag", Int] @ fields
                        let structName = ent.CompiledName + "_" + case.Name
                        yield StructDeclaration(structName, fieldsIncTag)
                        yield FunctionDeclaration(structName + "_new", fields, [
                            let cdIdent = { Name = "item"; Type = CStruct structName }
                            let rcIdent = { Name = "rc"; Type = Rc (CStruct d.Name)}
                            DeclareIdent("item", CStruct structName)
                            Do(SetValue(GetField(Ident cdIdent, "tag"), ConstInt32 i |> Const))
                            for (name, ctype) in fields do
                                Do(SetValue(GetField(Ident cdIdent, name), Ident {Name = name; Type = ctype}))
                            Assignment(["rc"],
                                FunctionCall(Ident { Name="Rc_New"; Type= Void},
                                    [
                                        FunctionCall(
                                            Helpers.voidIdent "sizeof",[ Helpers.voidIdent "item"])
                                        Unary(UnaryOp.RefOf, Helpers.voidIdent "item")
                                        Const ConstNull
                                    ]
                                ),
                                Rc (CStruct d.Name))
                            Return (Ident rcIdent)
                        ], Rc (CStruct d.Name))
                ]
            else
                []
        | x -> []

let transformDeclPostprocess = function
    | FunctionDeclaration(name, args, statements, Void) ->
        let statements = statements |> List.filter(function | Return (Const ConstNull) -> false | _ -> true)
        FunctionDeclaration(name, args, statements, Void)
    | x -> x

let transformFile com (file: Fable.File): File =
    let comp = CCompiler(com)
    {
        Filename = "abc"
        Includes = comp.GetIncludes()
        Declarations = ((file.Declarations |> List.collect (Transforms.transformDeclarations comp)) @ comp.GetAdditionalDeclarations())
                        |> List.map transformDeclPostprocess
        ASTDebug = sprintf "%A" file.Declarations
    }