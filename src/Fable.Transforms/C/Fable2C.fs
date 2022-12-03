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
        let ident name = Ident {Name = name; Namespace = None}
        let fcall args expr= FunctionCall(expr, args)
        let iife statements = FunctionCall(AnonymousFunc([], statements), [])
        let debugLog expr = FunctionCall(Helpers.ident "print", [expr]) |> Do
        let libEquality a b=
            FunctionCall(GetObjMethod(FunctionCall(Helpers.ident "require", [ConstString "./fable-lib/Util" |> Const]), "equals"), [a; b])
        let maybeIife = function
            | [] -> NoOp
            | [Return expr] -> expr
            | statements -> iife statements

        let statementsToExpr (com: CCompiler) = function
            | [] -> NoOp
            | lst ->
                match lst |> List.rev with
                Return expr::revT -> expr
                | _ -> sprintf "%A" lst |> Expr.Unknown
            // | lst ->
            //     let captures = []
                // com.CreateAdditionalDeclaration(FunctionDeclaration())


        let tryNewObj (names: string list) (values: Expr list) =
            if names.Length = values.Length then
                let pairs = List.zip names values
                let compareExprs = names
                                |> List.map (fun name ->
                                    libEquality
                                        (GetField(Helpers.ident "self", name))
                                        (GetField(Helpers.ident "toCompare", name)))
                let compareExprAcc = compareExprs |> List.reduce (fun acc item -> Binary(And, acc, item) )
                let equality = "Equals", Function (["self"; "toCompare"], [
                    //yield debugLog (ConstString "Calling equality" |> Const)
                    // debugLog (Helpers.ident "self")
                    // debugLog (Helpers.ident "toCompare")
                    //yield! compareExprs |> List.map debugLog
                    Return compareExprAcc
                ])
                NewObj(equality::pairs)
            else sprintf "Names and values do not match %A %A" names values |> Unknown
    let transformValueKind (com: CCompiler) = function
        | Fable.NumberConstant(v,_,_) ->
            Const(ConstNumber 3.141)
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
            let entity = com.Com.GetEntity(ref)
            if entity.IsFSharpRecord then
                let names = entity.FSharpFields |> List.map(fun f -> f.Name)
                let values = values |> List.map (transformExpr com)
                Helpers.tryNewObj names values
            else sprintf "unknown ety %A %A %A %A" values ref args entity |> Unknown
        | Fable.NewAnonymousRecord(values, names, _, _) ->
            let transformedValues = values |> List.map (transformExpr com)
            Helpers.tryNewObj (Array.toList names) transformedValues
        | Fable.NewUnion(values, tag, _, _) ->
            let values = values |> List.map(transformExpr com) |> List.mapi(fun i x -> sprintf "p_%i" i, x)
            NewObj(("tag", tag |> float |> ConstNumber |> Const)::values)
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

    let transformType com (t: Fable.Type) =
        match t with
        | Fable.Type.Char -> Char
        | Fable.Type.Number(kind, info) ->
            Int
        | Fable.Type.String ->
            Array Char
        | Fable.Type.Unit ->
            Void
        | _ ->
            Pointer Void
    let transformOp com =
        let transformExpr = transformExpr com
        function
        | Fable.OperationKind.Binary(BinaryModulus, left, right) ->
             GetField(Helpers.ident "math", "fmod") |> Helpers.fcall [transformExpr left; transformExpr right]
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
            FunctionCall(lhs, List.map transformExpr callInfo.Args) |> singletonStatement
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
            let rcall = FunctionCall(Ident { Namespace=None; Name= "require" }, [Const (ConstString path)])
            match info.Selector with
            | "" -> rcall |> singletonStatement
            | s -> GetObjMethod(rcall, s) |> singletonStatement
        | Fable.Expr.IdentExpr(i) when i.Name <> "" ->
            Ident {Namespace = None; Name = i.Name } |> singletonStatement
        | Fable.Expr.Operation (kind, _, _, _) ->
            transformOp kind |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.FieldGet(fi), t, _) ->
            GetField(transformExpr expr, fi.Name) |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.UnionField(fi), _, _) ->
            GetField(transformExpr expr, sprintf "p_%i" fi.CaseIndex) |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.ExprGet(e), _, _) ->
            GetAtIndex(transformExpr expr, transformExpr e) |> singletonStatement
        | Fable.Expr.Get(expr, Fable.GetKind.TupleIndex(i), _, _) ->
            GetAtIndex(transformExpr expr, Const (ConstNumber (float i))) |> singletonStatement
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
            Ternary(transformExpr guardExpr, transformExpr thenExpr, transformExpr elseExpr) |> singletonStatement
        | Fable.Test(expr, kind, b) ->
            match kind with
            | Fable.UnionCaseTest i->
                Binary(Equals, GetField(transformExpr expr, "tag") , Const (ConstNumber (float i))) |> singletonStatement
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
            let errorExpr =
                Const (ConstString "There was an error, todo")
                //transformExpr expr
            FunctionCall(Helpers.ident "error", [errorExpr])
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
                Assignment(["status"; "resOrErr"], FunctionCall(Helpers.ident "pcall", [
                    Function([], [
                        transformExpr body |> Return
                    ])
                ]), transformType com body.Type)
                let finalizer = finalizer |> Option.map transformExpr
                let catch = catch |> Option.map (fun (ident, expr) -> ident.Name, transformExpr expr)
                IfThenElse(Helpers.ident "status", [
                    match finalizer with
                    | Some finalizer -> yield Do finalizer
                    | None -> ()
                    yield Helpers.ident "resOrErr" |> Return
                ], [
                    match catch with
                    | Some(ident, expr) ->
                        yield expr |> Return
                    | _ -> ()
                ])
            ]
        | x -> [Unknown (sprintf "%A" x) |> Do]
    let transformExpr com expr=
        transformExprAsStatements com expr |> Transforms.Helpers.statementsToExpr com

    let transformDeclarations (com: CCompiler) = function
        | Fable.ModuleDeclaration m ->
            NothingDeclared
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
            let body = transformExprAsStatements com m.Body
            FunctionDeclaration(m.Name, m.Args |> List.map(fun a -> a.Name), body, transformType com m.Body.Type)
        | Fable.ClassDeclaration(d) ->
            com.AddClassDecl d
            //todo - build prototype members out
            //SNoOp
            NothingDeclared
        | x -> NothingDeclared

let transformFile com (file: Fable.File): File =
    let comp = CCompiler(com)
    {
        Filename = "abc"
        Includes = []
        Declarations = (comp.GetAdditionalDeclarations() @ file.Declarations)
                        |> List.map (Transforms.transformDeclarations comp)
        ASTDebug = sprintf "%A" file.Declarations
    }