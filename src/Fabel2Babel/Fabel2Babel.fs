module Fabel.Fabel2Babel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST

type Context = {
    fullName: string
    }
    
type IBabelCompiler =
    inherit ICompiler
    abstract GetImport: string -> string
    
let rec private ident (expr: Fabel.IdentifierExpr) =
    Babel.Identifier (expr.Name, ?loc=expr.Range)

and private identFromName name =
    let sanitizedName = Naming.sanitizeIdent (fun _ -> false) name
    Babel.Identifier name

and private assign left right =
    Babel.AssignmentExpression(AssignEqual, left, right) :> Babel.Expression

and private func com ctx kind body args restParams =
    let generator, async =
        match kind with
        | Fabel.Immediate -> false, false
        | Fabel.Generator -> true, false
        | Fabel.Async -> false, true
    let body = transformFunctionBody com ctx body
    let args: Babel.Pattern list =
        if restParams then failwith "TODO: RestParams"
        else args |> List.map (fun x -> upcast ident x)
    args, body, generator, async

and private funcDeclaration com ctx id kind body args restParams =
    let args, body, generator, async = func com ctx kind body args restParams
    Babel.FunctionDeclaration(id, args, body, generator, async)

and private funcExpression com ctx kind body args restParams range =
    let args, body, generator, async = func com ctx kind body args restParams
    Babel.FunctionExpression (args, body, generator, async, ?loc=range)

and private varDeclaration range var value =
    Babel.VariableDeclaration (
        Babel.VariableDeclarationKind.Var,
        [Babel.VariableDeclarator (var, value)],
        ?loc = range)

/// Immediately Invoked Function Expression
and private iife com ctx fnBody =
    Babel.CallExpression (
        Babel.FunctionExpression ([],
            transformFunctionBody com ctx fnBody), [])

and private block com ctx (exprs: Fabel.Expr list) =
    let exprs =
        match exprs with
        | [expr] ->
            match expr.Kind with
            | Fabel.Sequential statements -> statements
            | _ -> exprs
        | _ -> exprs
    Babel.BlockStatement (exprs |> List.map (transformStatement com ctx), [])
    
and private memberExpr com ctx expr (property: Fabel.Expr) =
    let property, computed =
        match property.Kind with
        | Fabel.Value (Fabel.StringConst name)
            when Naming.identForbiddenChars.IsMatch name = false ->
            Babel.Identifier (name) :> Babel.Expression, false
        | _ -> transformExpr com ctx property, true
    Babel.MemberExpression (transformExpr com ctx expr, property, computed)

and private transformStatement com ctx (expr: Fabel.Expr): Babel.Statement =
    match expr.Kind with
    | Fabel.Loop loopKind ->
        match loopKind with
        | Fabel.While (guard, body) ->
            upcast Babel.WhileStatement (transformExpr com ctx guard, block com ctx [body])
        | Fabel.ForOf (var, enumerable, body) ->
            // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right 
            let var = Babel.VariableDeclaration(Babel.VariableDeclarationKind.Var,
                        [Babel.VariableDeclarator (ident var)])
            upcast Babel.ForOfStatement (
                U2.Case1 var, transformExpr com ctx enumerable, block com ctx [body])
        | Fabel.For (var, start, limit, body, isUp) ->
            upcast Babel.ForStatement (
                block com ctx [body],
                transformExpr com ctx start
                    |> varDeclaration None (ident var) |> U2.Case1,
                Babel.BinaryExpression (BinaryOperator.BinaryLessOrEqual, ident var, transformExpr com ctx limit),
                Babel.UpdateExpression (UpdateOperator.UpdatePlus, false, ident var))
    | Fabel.Set (callee, property, value) ->
        let left =
            match property with
            | None -> transformExpr com ctx callee
            | Some property -> upcast memberExpr com ctx callee property
        upcast Babel.ExpressionStatement (
            assign left (transformExpr com ctx value), ?loc = expr.Range)
    | Fabel.VarDeclaration (var, value, _isMutable) ->
        transformExpr com ctx value
        |> varDeclaration expr.Range (ident var) :> Babel.Statement
    | Fabel.TryCatch (body, catch, finalizers) ->
        let handler = catch |> Option.map (fun (param, body)->
            Babel.CatchClause (ident param, block com ctx [body]))
        let finalizer = match finalizers with [] -> None | xs -> Some (block com ctx xs)
        upcast Babel.TryStatement (block com ctx [body], ?handler=handler, ?finalizer=finalizer)
    | Fabel.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let elseExpr =
            match elseExpr.Kind with
            | Fabel.Value (Fabel.Null) -> None
            | _ -> Some (transformStatement com ctx elseExpr)
        upcast Babel.IfStatement (
            transformExpr com ctx guardExpr,
            transformStatement com ctx thenExpr,
            ?alternate = elseExpr,
            ?loc = expr.Range)
    | Fabel.Sequential _ ->
        failwithf "Sequence when single statement expected in %A: %A" expr.Range expr 
    // Expressions become ExpressionStatements
    | Fabel.Value _ | Fabel.Get _ | Fabel.Apply _ | Fabel.Lambda _ | Fabel.Operation _ ->
        upcast Babel.ExpressionStatement (transformExpr com ctx expr, ?loc=expr.Range)

and private transformExpr com ctx (expr: Fabel.Expr): Babel.Expression =
    match expr.Kind with
    | Fabel.Value kind ->
        match kind with
        | Fabel.CoreModule name ->
            let coreLibRef = Babel.Identifier Naming.coreLibIdent
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
            upcast Babel.UnaryExpression (op, transformExpr com ctx expr, ?loc=expr.Range)
        | Fabel.Binary (op, left, right) ->
            upcast Babel.BinaryExpression (
                op, transformExpr com ctx left, transformExpr com ctx right, ?loc=expr.Range)
    | Fabel.Apply (callee, args, isPrimaryConstructor) ->
        let args = args |> List.map (transformExpr com ctx >> U2<_,_>.Case1)
        if isPrimaryConstructor
        then upcast Babel.NewExpression (transformExpr com ctx callee, args, ?loc=expr.Range)
        else upcast Babel.CallExpression (transformExpr com ctx callee, args, ?loc=expr.Range)
    | Fabel.Lambda (args, body, kind, restParams) ->
        upcast funcExpression com ctx kind body args restParams expr.Range
    | Fabel.Get (callee, property) -> upcast memberExpr com ctx callee property
    | Fabel.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        upcast Babel.ConditionalExpression (
            transformExpr com ctx guardExpr,
            transformExpr com ctx thenExpr,
            transformExpr com ctx elseExpr,
            ?loc = expr.Range)
    | Fabel.Sequential statements ->
        Babel.BlockStatement (statements |> List.map (transformStatement com ctx), [])
        |> fun block -> upcast Babel.DoExpression (block)
    | Fabel.TryCatch _ ->
        upcast (iife com ctx expr)
    | Fabel.Loop _ | Fabel.Set _  | Fabel.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr 
    
and private transformFunctionBody com ctx (expr: Fabel.Expr): Babel.BlockStatement =
    let returnBlock e = Babel.BlockStatement [Babel.ReturnStatement e]
    match expr.Type, expr.Kind with
    | Fabel.PrimitiveType (Fabel.Unit), _ ->
        block com ctx [expr]
    | _, Fabel.TryCatch (tryBody, catch, finalizers) ->
        let handler = catch |> Option.map (fun (param, body) ->
            Babel.CatchClause (ident param, returnBlock (transformExpr com ctx body)))
        let finalizer = match finalizers with [] -> None | xs -> Some (block com ctx xs)
        Babel.BlockStatement (
            [Babel.TryStatement (
                returnBlock (transformExpr com ctx tryBody),
                ?handler = handler,
                ?finalizer = finalizer,
                ?loc = expr.Range)], [])
    | _ -> returnBlock (transformExpr com ctx expr)
    
let private transformTestSuite com ctx decls: Babel.CallExpression =
    failwith "TODO: TestSuite members"
    
let private transformClass com ctx decls: Babel.ClassExpression =
    failwith "TODO: Class members"

let rec private transformModDecls com ctx modIdent decls = seq {
    let get left propName =
        if Naming.identForbiddenChars.IsMatch propName
        then Babel.MemberExpression(left, Babel.StringLiteral propName, true)
        else Babel.MemberExpression(left, Babel.Identifier propName, false)
        :> Babel.Expression
    // TODO: Keep track of sanitized member names to be sure they don't clash? 
    let declareMember var name expr isPublic =
        let var = match var with Some x -> x | None -> identFromName name
        let expr = if not isPublic then expr else assign (get modIdent name) expr 
        varDeclaration None var expr :> Babel.Statement
    for decl in decls do
    match decl with
    | Fabel.ActionDeclaration e ->
        yield transformStatement com ctx e
    | Fabel.MemberDeclaration m ->
        let expr, name =
            match m.Kind with
            | Fabel.Getter name ->
                transformExpr com ctx m.Function.Body, name
            | Fabel.Method name ->
                let f = m.Function
                upcast funcExpression com ctx f.Kind f.Body f.Arguments f.RestParams None, name
            | Fabel.Constructor | Fabel.Setter _ ->
                failwithf "Unexpected member in module: %A" m.Kind
        yield declareMember None name expr m.IsPublic
    | Fabel.EntityDeclaration (e, sub) ->
        match e with
        // Interfaces or attribute declarations shouldn't reach this point
        | :? Fabel.TypeEntity as te when te.Kind <> Fabel.Module ->
            // Don't create a new context for class declarations
            let classExpr = transformClass com ctx sub
            yield declareMember None e.Name classExpr e.IsPublic
        | _ ->
            let nestedIdent, protectedIdent =
                let memberNames =
                    decls |> Seq.choose (function
                    | Fabel.EntityDeclaration (e, _) -> Some e.Name
                    | Fabel.ActionDeclaration e -> None
                    | Fabel.MemberDeclaration m ->
                        match m.Kind with
                        | Fabel.Method name | Fabel.Getter name -> Some name
                        | Fabel.Constructor | Fabel.Setter _ -> None)
                    |> Set.ofSeq
                identFromName e.Name,
                // Protect module identifier against members with same name
                Babel.Identifier (Naming.sanitizeIdent memberNames.Contains e.Name)
            // var NestedMod = ParentMod.NestedMod = {};
            yield declareMember (Some nestedIdent) e.Name (Babel.ObjectExpression []) e.IsPublic
            // (function (/* protected */ NestedMod_1) {
            //     var privateVar = 1;
            //     var publicVar = NestedMod_1.publicVar = 2;
            //     var NestedMod = NestedMod_1.NestedMod = {};
            // })(NestedMod);
            let nestedDecls =
                transformModDecls com { fullName = e.FullName } protectedIdent sub |> Seq.toList
            yield Babel.ExpressionStatement(
                    Babel.CallExpression(
                        Babel.FunctionExpression([protectedIdent],
                            Babel.BlockStatement nestedDecls),
                            [U2.Case1 (upcast nestedIdent)])) :> Babel.Statement
    }
    
let makeCompiler (com: ICompiler) =
    let imports =
        System.Collections.Generic.Dictionary<string, string>()
    { new IBabelCompiler with
        member __.GetImport moduleName: string =
            match imports.TryGetValue moduleName with
            | true, import -> import
            | false, _ ->
                let m = sprintf "$M%i" imports.Count in imports.Add(moduleName, m); m  
      interface ICompiler with
        member __.Options = com.Options }

let transformFiles (com: ICompiler) (files: Fabel.File list): Babel.Program list =
    let babelCom = makeCompiler com
    files |> List.map (fun file ->
        let ctx = { fullName = file.RootNamespace.FullName }
        let rootIdent = Babel.Identifier Naming.rootModuleIdent
        let rootHead = [
            // export default $M0;
            varDeclaration None rootIdent (Babel.ObjectExpression [])
                :> Babel.Statement |> U2.Case1
            // var publicVar = $M0.publicVar = {};
            Babel.ExportDefaultDeclaration (U2.Case2 (upcast rootIdent))
                :> Babel.ModuleDeclaration |> U2.Case2
        ]
        let rootDecls =
            transformModDecls babelCom ctx rootIdent file.Declarations
            |> Seq.map (fun x -> U2<_,Babel.ModuleDeclaration>.Case1 x)
            |> Seq.toList
        Babel.Program (rootHead@rootDecls))
