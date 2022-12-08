module rec Fable.Transforms.Fable2Lua

//cloned from FableToBabel

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Lua
open Fable.Compilers.Lua
open Fable.Naming
open Fable.Core


// type LuaCompiler(com: Fable.Compiler) =
//     interface ILuaCompiler // with
        //member this.AddType(entref, Type: LuaType) = this.AddType(entref, phpType)
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
    let transformValueKind (com: LuaCompiler) = function
        | Fable.NumberConstant(:? float as v,kind,_) ->
            Const(ConstNumber v)
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
        | Fable.NewArray(kind, t, _) ->
            match kind with
            | Fable.ArrayValues values -> NewArr(values |> List.map (transformExpr com))
            | _ -> NewArr([])
        | Fable.Null _ ->
            Const(ConstNull)
        | x -> sprintf "unknown %A" x |> ConstString |> Const
    let transformOp com =
        let transformExpr = transformExpr com
        function
        | Fable.OperationKind.Binary(BinaryModulus, left, right) ->
            GetField(Helpers.ident "math", "fmod") |> Helpers.fcall [transformExpr left; transformExpr right]
        | Fable.OperationKind.Binary (op, left, right) ->
            let op =
                match op with
                | BinaryOperator.BinaryMultiply -> Multiply
                | BinaryOperator.BinaryDivide -> Divide
                | BinaryOperator.BinaryEqual -> Equals
                | BinaryOperator.BinaryPlus -> Plus
                | BinaryOperator.BinaryMinus -> Minus
                | BinaryOperator.BinaryUnequal -> Unequal
                | BinaryOperator.BinaryLess -> Less
                | BinaryOperator.BinaryGreater -> Greater
                | BinaryOperator.BinaryLessOrEqual -> LessOrEqual
                | BinaryOperator.BinaryGreaterOrEqual -> GreaterOrEqual
                | x -> sprintf "%A" x |> BinaryTodo
            Binary(op, transformExpr left, transformExpr right )
        | Fable.OperationKind.Unary (op, expr) ->
            match op with
            | UnaryOperator.UnaryNotBitwise -> transformExpr expr //not sure why this is being added
            | UnaryOperator.UnaryNot -> Unary(Not, transformExpr expr)
            | _ -> sprintf "%A %A" op expr |> Unknown
        | x -> Unknown(sprintf "%A" x)
    let asSingleExprIife (exprs: Expr list): Expr= //function
        match exprs with
        | [] -> NoOp
        | [h] -> h
        | exprs ->
            let statements =
                Helpers.transformStatements
                    (Do)
                    (Return)
                    exprs
            statements |> Helpers.maybeIife
    let flattenReturnIifes e =
        let rec collectStatementsRec =
            function
            | Return (FunctionCall(AnonymousFunc([], [Return s]), [])) ->
                [Return s]
            | Return (FunctionCall(AnonymousFunc([], statements), [])) -> //self executing functions only
                statements |> List.collect collectStatementsRec
            | x -> [x]
        let statements = collectStatementsRec e
        match statements with
        | [Return s] -> Return s
        | [] -> NoOp |> Do
        | _ -> FunctionCall(AnonymousFunc([], statements), []) |> Return

    let asSingleExprIifeTr com : Fable.Expr list -> Expr = List.map (transformExpr com) >> asSingleExprIife
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let transformExpr (com: LuaCompiler) expr=
        let transformExpr = transformExpr com
        let transformOp = transformOp com
        match expr with
        | Fable.Expr.Value(value, _) -> transformValueKind com value
        | Fable.Expr.Call(expr, callInfo, t, r) ->
            let lhs =
                match expr with
                | Fable.Expr.Get(expr, Fable.GetKind.FieldGet info, t, _) ->
                    match t with
                    | Fable.DeclaredType(_, _)
                    | Fable.AnonymousRecordType(_, _, _) ->
                        GetObjMethod(transformExpr expr, info.Name)
                    | _ -> transformExpr expr
                | Fable.Expr.Delegate _ ->
                    transformExpr expr |> Brackets
                | _ -> transformExpr expr
            FunctionCall(lhs, List.map transformExpr callInfo.Args)
        | Fable.Expr.Import (info, t, r) ->
            let path =
                match info.Kind, info.Path with
                | libImport, Regex "fable-lib\/(\w+).(?:fs|js)" [name] ->
                    "fable-lib/" + name
                | _, Regex"fable-library-lua\/fable\/fable-library\/(\w+).(?:fs|js)" [name] ->
                    "fable-lib/fable-library" + name
                | _, Regex"fable-library-lua\/fable\/(\w+).(?:fs|js)" [name] ->
                    "fable-lib/" + name
                | _ ->
                    info.Path.Replace(".fs", "").Replace(".js", "") //todo - make less brittle
            let rcall = FunctionCall(Ident { Namespace=None; Name= "require" }, [Const (ConstString path)])
            match info.Selector with
            | "" -> rcall
            | s -> GetObjMethod(rcall, s)
        | Fable.Expr.IdentExpr(i) when i.Name <> "" ->
            Ident {Namespace = None; Name = i.Name }
        | Fable.Expr.Operation (kind, _, _, _) ->
            transformOp kind
        | Fable.Expr.Get(expr, Fable.GetKind.FieldGet info, t, _) ->
            GetField(transformExpr expr, info.Name)
        | Fable.Expr.Get(expr, Fable.GetKind.UnionField info , _, _) ->
            GetField(transformExpr expr, sprintf "p_%i" info.FieldIndex)
        | Fable.Expr.Get(expr, Fable.GetKind.ExprGet(e), _, _) ->
            GetAtIndex(transformExpr expr, transformExpr e)
        | Fable.Expr.Get(expr, Fable.GetKind.TupleIndex(i), _, _) ->
            GetAtIndex(transformExpr expr, Const (ConstNumber (float i)))
        | Fable.Expr.Get(expr, Fable.GetKind.OptionValue, _, _) ->
            transformExpr expr  //todo null check, throw if null?
        | Fable.Expr.Set(expr, Fable.SetKind.ValueSet, t, value, _) ->
            SetValue(transformExpr expr, transformExpr value)
        | Fable.Expr.Set(expr, Fable.SetKind.ExprSet(e), t, value, _) ->
            SetExpr(transformExpr expr, transformExpr e, transformExpr value)
        | Fable.Expr.Sequential exprs ->
            asSingleExprIifeTr com exprs
        | Fable.Expr.Let (ident, value, body) ->
            let statements = [
                Assignment([ident.Name], transformExpr value, true)
                transformExpr body |> Return
            ]
            Helpers.maybeIife statements
        | Fable.Expr.Emit(m, _, _) ->
            // let argsExprs = m.CallInfo.Args |> List.map transformExpr
            // let macroExpr = Macro(m.Macro, argsExprs)
            // let exprs =
            //     argsExprs
            //     @ [macroExpr]
            // asSingleExprIife exprs
            Macro(m.Macro, m.CallInfo.Args |> List.map transformExpr)
        | Fable.Expr.DecisionTree(expr, lst) ->
            com.DecisionTreeTargets(lst)
            transformExpr expr
        | Fable.Expr.DecisionTreeSuccess(i, boundValues, _) ->
            let idents,target = com.GetDecisionTreeTargets(i)
            if idents.Length = boundValues.Length then
                let statements =
                    [   for (ident, value) in List.zip idents boundValues do
                            yield Assignment([ident.Name], transformExpr value, false)
                        yield transformExpr target |> Return
                            ]
                statements
                |> Helpers.maybeIife
            else sprintf "not equal lengths %A %A" idents boundValues |> Unknown
        | Fable.Expr.Lambda(arg, body, name) ->
            Function([arg.Name], [transformExpr body |> Return])
        | Fable.Expr.CurriedApply(applied, args, _, _) ->
            FunctionCall(transformExpr applied, args |> List.map transformExpr)
        | Fable.Expr.IfThenElse (guardExpr, thenExpr, elseExpr, _) ->
            Ternary(transformExpr guardExpr, transformExpr thenExpr, transformExpr elseExpr)
        | Fable.Test(expr, kind, b) ->
            match kind with
            | Fable.UnionCaseTest i->
                Binary(Equals, GetField(transformExpr expr, "tag") , Const (ConstNumber (float i)))
            | Fable.OptionTest isSome ->
                if isSome then Binary(Unequal, Const ConstNull, transformExpr expr) else Binary(Equals, Const ConstNull, transformExpr expr)
            | Fable.TestKind.TypeTest t ->
                // match t with
                // | Fable.DeclaredType (ent, genArgs) ->
                //     match ent.FullName with
                //     | Fable.Transforms.Types.ienumerable -> //isArrayLike
                //     | Fable.Transforms.Types.array
                //     | _ ->
                // | _ -> ()
                Binary(Equals, GetField(transformExpr expr, "type"), Const (t.ToString() |> ConstString))
            | _ ->
                Unknown(sprintf "test %A %A" expr kind)
        | Fable.Extended(Fable.ExtendedSet.Throw(expr, _), t) ->
            let errorExpr =
                Const (ConstString "There was an error, todo")
                //transformExpr expr
            FunctionCall(Helpers.ident "error", [errorExpr])
        | Fable.Extended(Fable.ExtendedSet.Curry(expr, d), _) ->
            transformExpr expr |> sprintf "todo curry %A" |> Unknown
        | Fable.Delegate(idents, body, _, _) ->
            Function(idents |> List.map(fun i -> i.Name), [transformExpr body |> Return |> flattenReturnIifes]) //can be flattened
        | Fable.ForLoop(ident, start, limit, body, isUp, _) ->
            Helpers.maybeIife [
                ForLoop(ident.Name, transformExpr start, transformExpr limit, [transformExpr body |> Do])
                ]
        | Fable.TypeCast(expr, t) ->
            transformExpr expr //typecasts are meaningless
        | Fable.WhileLoop(guard, body, label) ->
            Helpers.maybeIife [
                WhileLoop(transformExpr guard, [transformExpr body |> Do])
            ]
        | Fable.TryCatch(body, catch, finalizer, _) ->
            Helpers.maybeIife [
                Assignment(["status"; "resOrErr"], FunctionCall(Helpers.ident "pcall", [
                    Function([], [
                        transformExpr body |> Return
                    ])
                ]), true)
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
        | x -> Unknown (sprintf "%A" x)

    let transformDeclarations (com: LuaCompiler) = function
        | Fable.ModuleDeclaration m ->
            Assignment(["moduleDecTest"], Expr.Const (ConstString "moduledectest"), false)
        | Fable.MemberDeclaration m ->
            if m.Args.Length = 0 then
                Assignment([m.Name], transformExpr com m.Body, true)
            else
                let info = com.Com.GetMember(m.MemberRef)
                let unwrapSelfExStatements =
                    match transformExpr com m.Body |> Return |> flattenReturnIifes with
                    | Return (FunctionCall(AnonymousFunc([], statements), [])) ->
                        statements
                    | s -> [s]
                FunctionDeclaration(m.Name, m.Args |> List.map(fun a -> a.Name), unwrapSelfExStatements, info.IsPublic)
        | Fable.ClassDeclaration(d) ->
            com.AddClassDecl d
            //todo - build prototype members out
            //SNoOp
            sprintf "ClassDeclaration %A" d |> Unknown |> Do
        | x -> sprintf "%A" x |> Unknown |> Do

let transformFile com (file: Fable.File): File =
    let comp = LuaCompiler(com)
    {
        Filename = "abc"
        Statements =  file.Declarations |> List.map (Transforms.transformDeclarations comp)
        ASTDebug = sprintf "%A" file.Declarations
    }