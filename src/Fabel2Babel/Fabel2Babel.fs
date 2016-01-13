module Fabel.Fabel2Babel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST

// type Enclosing = Root | Module | Class | TestSuite
    
type Context =
    | Context
    member x.GetImport fileName: string = failwith "TODO"
    
let private (|ExprKind|) (expr: Fabel.Expr) = expr.Kind

let rec private ident (expr: Fabel.IdentifierExpr) =
    Babel.Identifier (expr.Name)  //?loc=expr.Range

and private declaration ctx var value range =
    Babel.VariableDeclaration (
        Babel.VariableDeclarationKind.Var,
        [Babel.VariableDeclarator (ident var, transformExpr ctx value)],
        ?loc = range)

and private iife ctx fnBody =
    Babel.CallExpression (
        Babel.FunctionExpression ([],
            transformFunctionBody ctx fnBody, false, false), [])

and private block ctx exprs =
    let exprs =
        match exprs with
        | [ExprKind (Fabel.Sequential exprs)] -> exprs
        | _ -> exprs
    Babel.BlockStatement (exprs |> List.map (transformStatement ctx), [])
    
and private memberExpr ctx expr (property: Fabel.Expr) =
    let property, computed =
        match property.Kind with
        | Fabel.Value (Fabel.StringConst name)
            when IdentForbiddenChars.Regex.IsMatch name = false ->
            Babel.Identifier (name) :> Babel.Expression, false
        | _ -> transformExpr ctx property, true
    Babel.MemberExpression (transformExpr ctx expr, property, computed)

and private transformStatement (ctx: Context) (expr: Fabel.Expr): Babel.Statement =
    match expr.Kind with
    | Fabel.Loop loopKind ->
        match loopKind with
        | Fabel.While (guard, body) ->
            upcast Babel.WhileStatement (
                transformExpr ctx guard,
                block ctx [body])
        | Fabel.ForOf (var, enumerable, body) ->
            upcast Babel.ForOfStatement (
                U2.Case1 (declaration ctx var enumerable None),
                transformExpr ctx enumerable,
                block ctx [body])
        | Fabel.For (var, start, limit, body, isUp) ->
            upcast Babel.ForStatement (
                block ctx [body],
                U2.Case1 (declaration ctx var start None),
                Babel.BinaryExpression (BinaryOperator.BinaryLessOrEqual, ident var, transformExpr ctx limit),
                Babel.UpdateExpression (UpdateOperator.UpdatePlus, false, ident var))
    | Fabel.Set (callee, property, value) ->
        let left =
            match property with
            | None -> transformExpr ctx callee
            | Some property -> upcast memberExpr ctx callee property
        upcast Babel.ExpressionStatement (
            Babel.AssignmentExpression (
                AssignmentOperator.AssignEqual, left, transformExpr ctx value),
            ?loc = expr.Range)
    | Fabel.VarDeclaration (var, value, _isMutable) ->
        upcast declaration ctx var value expr.Range
    | Fabel.TryCatch (body, catch, finalizers) ->
        let handler = catch |> Option.map (fun (param, body)->
            Babel.CatchClause (ident param, block ctx [body]))
        let finalizer = match finalizers with [] -> None | xs -> Some (block ctx xs)
        upcast Babel.TryStatement (block ctx [body], ?handler=handler, ?finalizer=finalizer)
    | Fabel.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let elseExpr =
            match elseExpr.Kind with
            | Fabel.Value (Fabel.Null) -> None
            | _ -> Some (transformStatement ctx elseExpr)
        upcast Babel.IfStatement (
            transformExpr ctx guardExpr,
            transformStatement ctx thenExpr,
            ?alternate = elseExpr,
            ?loc = expr.Range)
    | Fabel.Sequential _ ->
        failwithf "Sequence when single statement expected in %A: %A" expr.Range expr 
    // Expressions become ExpressionStatements
    | Fabel.Value _ | Fabel.Get _ | Fabel.Apply _ | Fabel.Lambda _ | Fabel.Operation _ ->
        upcast Babel.ExpressionStatement (transformExpr ctx expr, ?loc=expr.Range)

and private transformExpr (ctx: Context) (expr: Fabel.Expr): Babel.Expression =
    match expr.Kind with
    | Fabel.Value kind ->
        match kind with
        | Fabel.CoreModule name ->
            let coreLibRef = Babel.Identifier Literals.coreLibIdent
            upcast Babel.MemberExpression (coreLibRef, Babel.Identifier (name), false)
        | Fabel.This -> upcast Babel.ThisExpression (?loc=expr.Range)
        | Fabel.Super -> upcast Babel.Super (?loc=expr.Range)
        | Fabel.Null -> upcast Babel.NullLiteral (?loc=expr.Range)
        | Fabel.Identifier name -> upcast Babel.Identifier (name, ?loc=expr.Range)
        | Fabel.IntConst x -> upcast Babel.NumericLiteral (U2.Case1 x, ?loc=expr.Range)
        | Fabel.FloatConst x -> upcast Babel.NumericLiteral (U2.Case2 x, ?loc=expr.Range)
        | Fabel.StringConst x -> upcast Babel.StringLiteral (x, ?loc=expr.Range)
        | Fabel.BoolConst x -> upcast Babel.BooleanLiteral (x, ?loc=expr.Range)
        | Fabel.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (source, flags, ?loc=expr.Range)
        | Fabel.ArrayConst items -> failwith "TODO"
        | Fabel.TypeRef typ ->
            match typ with
            // TODO: Resolve type reference: Hierarchies for internal/import and fail for external
            | Fabel.DeclaredType typEnt -> upcast Babel.Identifier (typEnt.FullName, ?loc=expr.Range)
            | _ -> failwithf "Not supported type reference: %A" typ
    | Fabel.Operation op ->
        match op with
        | Fabel.Logical (op, left, right) -> failwith "TODO"
        | Fabel.Unary (op, expr) ->
            upcast Babel.UnaryExpression (op, transformExpr ctx expr, ?loc=expr.Range)
        | Fabel.Binary (op, left, right) ->
            upcast Babel.BinaryExpression (
                op, transformExpr ctx left, transformExpr ctx right, ?loc=expr.Range)
    | Fabel.Apply (callee, args, isPrimaryConstructor) ->
        let args = args |> List.map (transformExpr ctx >> U2<_,_>.Case1)
        if isPrimaryConstructor
        then upcast Babel.NewExpression (transformExpr ctx callee, args, ?loc=expr.Range)
        else upcast Babel.CallExpression (transformExpr ctx callee, args, ?loc=expr.Range)
    | Fabel.Lambda (args, body, kind, restParams) ->
        let generator, async =
            match kind with
            | Fabel.Immediate -> false, false
            | Fabel.Generator -> true, false
            | Fabel.Async -> false, true
        let body = transformFunctionBody ctx body
        let args: Babel.Pattern list =
            if restParams then failwith "TODO"
            else args |> List.map (fun x -> upcast ident x)
        upcast Babel.FunctionExpression (args, body, generator, async, ?loc=expr.Range)
    | Fabel.Get (callee, property) -> upcast memberExpr ctx callee property
    | Fabel.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        upcast Babel.ConditionalExpression (
            transformExpr ctx guardExpr,
            transformExpr ctx thenExpr,
            transformExpr ctx elseExpr,
            ?loc = expr.Range)
    | Fabel.Sequential statements ->
        Babel.BlockStatement (statements |> List.map (transformStatement ctx), [])
        |> fun block -> upcast Babel.DoExpression (block)
    | Fabel.TryCatch _ ->
        upcast (iife ctx expr)
    | Fabel.Loop _ | Fabel.Set _  | Fabel.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr 
    
and private transformFunctionBody (ctx: Context) (expr: Fabel.Expr): Babel.BlockStatement =
    let returnBlock e = Babel.BlockStatement ([Babel.ReturnStatement e], [])
    match expr.Type, expr.Kind with
    | Fabel.PrimitiveType (Fabel.Unit), _ ->
        block ctx [expr]
    | _, Fabel.TryCatch (tryBody, catch, finalizers) ->
        let handler = catch |> Option.map (fun (param, body) ->
            Babel.CatchClause (ident param, returnBlock (transformExpr ctx body)))
        let finalizer = match finalizers with [] -> None | xs -> Some (block ctx xs)
        Babel.BlockStatement (
            [Babel.TryStatement (
                returnBlock (transformExpr ctx tryBody),
                ?handler = handler,
                ?finalizer = finalizer,
                ?loc = expr.Range)], [])
    | _ -> returnBlock (transformExpr ctx expr)
    
let transformFiles (com: ICompiler) (files: Fabel.File list): Babel.Program list =
    failwith "TODO"
