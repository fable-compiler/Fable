module Fabel.Fabel2Babel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST

type Context = {
    file: string
    moduleFullName: string
    imports: System.Collections.Generic.Dictionary<string, string>
    }
    
type FabelFunctionInfo =
    | FabelFunctionInfo of
        args: Fabel.IdentifierExpr list *
        restParams: bool *
        body: Fabel.Expr *
        kind: Fabel.LambdaKind
    
type BabelFunctionInfo =
    | BabelFunctionInfo of
        args: Babel.Pattern list *
        body: U2<Babel.BlockStatement, Babel.Expression> *
        generator: bool *
        async: bool

type IBabelCompiler =
    inherit ICompiler
    abstract GetFabelFile: string -> Fabel.File
    abstract GetImport: Context -> SourceLocation -> string -> Babel.Expression
    abstract TransformExpr: Context -> Fabel.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fabel.Expr -> Babel.Statement
    abstract TransformFunction: Context -> FabelFunctionInfo -> BabelFunctionInfo
    
let (|ExprKind|) (e: Fabel.Expr) = e.Kind
let (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
let (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e

let private returnBlock (e: Babel.Expression) =
    Babel.BlockStatement(e.loc, [Babel.ReturnStatement(e.loc, e)])

let (|Block|) (e: U2<Babel.BlockStatement, Babel.Expression>) =
    match e with
    | U2.Case1 e -> e
    | U2.Case2 e -> returnBlock e
    
let private ident (expr: Fabel.IdentifierExpr) =
    Babel.Identifier(expr.Range, expr.Name)

let private identFromName loc name =
    let sanitizedName = Naming.sanitizeIdent (fun _ -> false) name
    Babel.Identifier(loc, name)
    
let private get (left: Babel.Expression) propName =
    if Naming.identForbiddenChars.IsMatch propName
    then Babel.MemberExpression(left.loc, left, Babel.StringLiteral(left.loc, propName), true)
    else Babel.MemberExpression(left.loc, left, Babel.Identifier(left.loc, propName), false)
    :> Babel.Expression
    
let private getExpr com ctx (TransformExpr com ctx expr) (property: Fabel.Expr) =
    let property, computed =
        match property with
        | ExprKind (Fabel.Value (Fabel.StringConst name))
            when Naming.identForbiddenChars.IsMatch name = false ->
            Babel.Identifier(property.Range, name) :> Babel.Expression, false
        | TransformExpr com ctx property -> property, true
    match expr with
    | :? Babel.EmptyExpression -> property
    | _ -> Babel.MemberExpression(expr.loc + property.loc, expr, property, computed)
            :> Babel.Expression

let private typeRef (com: IBabelCompiler) ctx range file fullName =
    let getDiff s1 s2 =
        let split (s: string) =
            s.Split('.') |> Array.toList
        let rec removeCommon (xs1: string list) (xs2: string list) =
            match xs1, xs2 with
            | x1::xs1, x2::xs2 when x1 = x2 -> removeCommon xs1 xs2
            | _ -> xs2
        removeCommon (split s1) (split s2)
    let rec makeExpr (members: string list) (baseExpr: Babel.Expression option) =
        match baseExpr with
        | Some baseExpr ->
            match members with
            | [] -> baseExpr
            | m::ms -> get baseExpr m |> Some |> makeExpr ms
        | None ->
            match members with
            | [] -> upcast Babel.EmptyExpression range
            | m::ms -> identFromName range m :> Babel.Expression
                       |> Some |> makeExpr ms
    match file with
    | None -> failwithf "Cannot reference type: %s" fullName
    | Some file ->
        let file = com.GetFabelFile file
        file.External
        |> List.tryFind (fun ext ->
            fullName = ext.FullName || fullName.StartsWith ext.FullName)
        |> function
            | Some (Fabel.ImportModule (ns, modName)) ->
                Some (com.GetImport ctx range modName)
                |> makeExpr (getDiff ns fullName)
            | Some (Fabel.GlobalModule ns) ->
                makeExpr (getDiff ns fullName) None
            | None when ctx.file <> file.FileName ->
                Some (com.GetImport ctx range file.FileName)
                |> makeExpr (getDiff file.RootFullName fullName)
            | None ->
                makeExpr (getDiff ctx.moduleFullName fullName) None

let private assign (left: Babel.Expression) (right: Babel.Expression) =
    let loc = left.loc + right.loc
    Babel.AssignmentExpression(loc, AssignEqual, left, right) :> Babel.Expression
    
let private block (com: IBabelCompiler) ctx (exprs: Fabel.Expr list) =
    let exprs = match exprs with
                | [ExprKind (Fabel.Sequential statements)] -> statements
                | _ -> exprs
    let loc = exprs |> Seq.map (fun x -> x.Range) |> Seq.reduce (+)
    Babel.BlockStatement(loc, exprs |> List.map (com.TransformStatement ctx))

let private funcExpression, funcDeclaration =
    let func (com: IBabelCompiler) ctx funcInfo =
        let (BabelFunctionInfo(args, Block body, generator, async)) =
            com.TransformFunction ctx funcInfo
        args, body, generator, async
    let funcExpression (com: IBabelCompiler) ctx range args restParams body kind =
        let args, body, generator, async =
            func com ctx (FabelFunctionInfo(args, restParams, body, kind))
        Babel.FunctionExpression(range, args, body, generator, async)
    let funcDeclaration (com: IBabelCompiler) ctx range id args restParams body kind =
        let args, body, generator, async =
            func com ctx (FabelFunctionInfo(args, restParams, body, kind))
        Babel.FunctionDeclaration(range, id, args, body, generator, async)
    funcExpression, funcDeclaration

let private funcArrow (com: IBabelCompiler) ctx range args restParams body kind =
    let (BabelFunctionInfo (args, body, generator, async)) =
        com.TransformFunction ctx (FabelFunctionInfo (args, restParams, body, kind))
    Babel.ArrowFunctionExpression(range, args, body, async) :> Babel.Expression

let private varDeclaration range var (value: Babel.Expression) =
    Babel.VariableDeclaration(range, var, value)

/// Immediately Invoked Function Expression
let private iife com ctx (body: Fabel.Expr) =
    let fexpr = funcExpression com ctx body.Range [] false body Fabel.Immediate
    Babel.CallExpression(body.Range, fexpr, [])

let private catchClause (com: IBabelCompiler) ctx (catch: (Fabel.IdentifierExpr * Fabel.Expr) option) =
    catch |> Option.map (fun (param, body) ->
        Babel.CatchClause(param.Range + body.Range,
            ident param, returnBlock (com.TransformExpr ctx body)))

let private transformStatement com ctx (expr: Fabel.Expr): Babel.Statement =
    match expr.Kind with
    | Fabel.Loop loopKind ->
        match loopKind with
        | Fabel.While (TransformExpr com ctx guard, body) ->
            upcast Babel.WhileStatement(expr.Range, guard, block com ctx [body])
        | Fabel.ForOf (var, TransformExpr com ctx enumerable, body) ->
            // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right 
            let var = Babel.VariableDeclaration(var.Range, ident var)
            upcast Babel.ForOfStatement(expr.Range, U2.Case1 var, enumerable, block com ctx [body])
        | Fabel.For (var, TransformExpr com ctx start,
                        TransformExpr com ctx limit, body, isUp) ->
            let updateOp = if isUp then UpdateOperator.UpdatePlus else UpdateOperator.UpdateMinus
            upcast Babel.ForStatement (expr.Range,
                block com ctx [body],
                start |> varDeclaration var.Range (ident var) |> U2.Case1,
                Babel.BinaryExpression(var.Range, BinaryOperator.BinaryLessOrEqual, ident var, limit),
                Babel.UpdateExpression(var.Range, updateOp, false, ident var))

    | Fabel.Set (callee, property, TransformExpr com ctx value) ->
        let left =
            match property with
            | None -> com.TransformExpr ctx callee
            | Some property -> getExpr com ctx callee property
        upcast Babel.ExpressionStatement(expr.Range, assign left value)

    | Fabel.VarDeclaration (var, TransformExpr com ctx value, _isMutable) ->
        varDeclaration expr.Range (ident var) value :> Babel.Statement

    | Fabel.TryCatch (body, catch, finalizers) ->
        let handler = catchClause com ctx catch
        let finalizer =
            match finalizers with
            | [] -> None
            | xs -> Some (block com ctx xs)
        upcast Babel.TryStatement(expr.Range,
            block com ctx [body], ?handler=handler, ?finalizer=finalizer)

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformStatement com ctx thenExpr, elseExpr) ->
        let elseExpr =
            match elseExpr.Kind with
            | Fabel.Value (Fabel.Null) -> None
            | _ -> Some (com.TransformStatement ctx elseExpr)
        upcast Babel.IfStatement (expr.Range, guardExpr, thenExpr, ?alternate=elseExpr)

    | Fabel.Sequential _ ->
        failwithf "Sequence when single statement expected in %A: %A" expr.Range expr 

    // Expressions become ExpressionStatements
    | Fabel.Value _ | Fabel.Get _ | Fabel.Apply _ | Fabel.Lambda _ ->
        upcast Babel.ExpressionStatement(expr.Range, com.TransformExpr ctx expr)

let private transformExpr (com: IBabelCompiler) ctx (expr: Fabel.Expr): Babel.Expression =
    match expr.Kind with
    | Fabel.Value kind ->
        match kind with
        | Fabel.CoreModule name ->
            let coreLibRef = Babel.Identifier(expr.Range, Naming.coreLibIdent)
            upcast Babel.MemberExpression(expr.Range,
                coreLibRef, Babel.Identifier(expr.Range, name), false)
        | Fabel.This -> upcast Babel.ThisExpression (expr.Range)
        | Fabel.Super -> upcast Babel.Super (expr.Range)
        | Fabel.Null -> upcast Babel.NullLiteral (expr.Range)
        | Fabel.Identifier name -> upcast Babel.Identifier (expr.Range, name)
        | Fabel.IntConst x -> upcast Babel.NumericLiteral (expr.Range, U2.Case1 x)
        | Fabel.FloatConst x -> upcast Babel.NumericLiteral (expr.Range, U2.Case2 x)
        | Fabel.StringConst x -> upcast Babel.StringLiteral (expr.Range, x)
        | Fabel.BoolConst x -> upcast Babel.BooleanLiteral (expr.Range, x)
        | Fabel.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (expr.Range, source, flags)
        | Fabel.ArrayConst items -> failwith "TODO: Array initializers"
        | Fabel.ObjExpr _ -> failwith "TODO: Object expressions"
        | Fabel.TypeRef typ ->
            match typ with
            | Fabel.DeclaredType typEnt ->
                let typFullName =
                    if Option.isSome (typEnt.HasDecoratorNamed "Erase") 
                    then typEnt.FullName.Substring(0, typEnt.FullName.LastIndexOf ".")
                    else typEnt.FullName
                typeRef com ctx expr.Range typEnt.File typFullName
            | _ -> failwithf "Not supported type reference: %A" typ
        | Fabel.LogicalOp _ | Fabel.BinaryOp _ | Fabel.UnaryOp _ ->
            failwithf "Unexpected stand-alone operation: %A" expr

    | Fabel.Apply (callee, args, isPrimaryConstructor) ->
        match callee.Kind, args with
        | Fabel.Value (Fabel.LogicalOp op), [left; right] ->
            failwith "TODO: Logical operations"
        | Fabel.Value (Fabel.UnaryOp op), [TransformExpr com ctx operand as expr] ->
            upcast Babel.UnaryExpression (expr.Range, op, operand)
        | Fabel.Value (Fabel.BinaryOp op), [TransformExpr com ctx left; TransformExpr com ctx right] ->
            upcast Babel.BinaryExpression (expr.Range, op, left, right)
        | _ ->
            let callee = com.TransformExpr ctx callee
            let args = args |> List.map (com.TransformExpr ctx >> U2<_,_>.Case1)
            if isPrimaryConstructor
            then upcast Babel.NewExpression (expr.Range, callee, args)
            else upcast Babel.CallExpression (expr.Range, callee, args)

    | Fabel.Lambda (args, body, kind, restParams) ->
        funcArrow com ctx expr.Range args restParams body kind

    | Fabel.Get (callee, property) ->
        getExpr com ctx callee property

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformExpr com ctx thenExpr,
                        TransformExpr com ctx elseExpr) ->
        upcast Babel.ConditionalExpression (expr.Range, guardExpr, thenExpr, elseExpr)

    | Fabel.Sequential statements ->
        match statements with
        | [] -> upcast Babel.NullLiteral expr.Range
        | [expr] -> com.TransformExpr ctx expr 
        | statements ->
            Babel.BlockStatement(expr.Range, statements |> List.map (com.TransformStatement ctx), [])
            |> fun block -> upcast Babel.DoExpression (expr.Range, block)

    | Fabel.TryCatch _ ->
        upcast (iife com ctx expr)

    | Fabel.Loop _ | Fabel.Set _  | Fabel.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr 
    
let private transformFunctionBody com ctx (FabelFunctionInfo (args, restParams, body, kind)) =
    let generator, async =
        match kind with
        | Fabel.Immediate -> false, false
        | Fabel.Generator -> true, false
        | Fabel.Async -> false, true
    let args: Babel.Pattern list =
        if restParams then failwith "TODO: RestParams"
        else args |> List.map (fun x -> upcast ident x)
    let body: U2<Babel.BlockStatement, Babel.Expression> =
        match body.Type, body.Kind with
        | Fabel.PrimitiveType (Fabel.Unit), _ ->
            block com ctx [body] |> U2.Case1
        | _, Fabel.TryCatch (tryBody, catch, finalizers) ->
            let handler = catchClause com ctx catch
            let finalizer = match finalizers with [] -> None | xs -> Some (block com ctx xs)
            Babel.BlockStatement (body.Range,
                [Babel.TryStatement(body.Range,
                    returnBlock (transformExpr com ctx tryBody),
                    ?handler = handler,
                    ?finalizer = finalizer)], []) |> U2.Case1
        | _ -> returnBlock (transformExpr com ctx body) |> U2.Case1
    BabelFunctionInfo (args, body, generator, async)
    
let private transformTestSuite com ctx decls: Babel.CallExpression =
    failwith "TODO: TestSuite members"
    
// type ClassMethodKind =
//     | ClassConstructor | ClassMethod | ClassGetter | ClassSetter
let private transformClass com ctx classRange (baseClass: Fabel.EntityLocation option) decls =
    let declareMember range kind name (f: Fabel.LambdaExpr) isStatic =
        let name, computed: Babel.Expression * bool =
            if Naming.identForbiddenChars.IsMatch name
            then upcast Babel.StringLiteral(range, name), true
            else upcast Babel.Identifier(range, name), false
        let finfo = FabelFunctionInfo (f.Arguments, f.RestParams, f.Body, f.Kind)
        let (BabelFunctionInfo (args, Block body, generator, async)) =
            transformFunctionBody com ctx finfo
        // TODO: Optimization: remove null statement that F# compiler 
        // adds at the bottom of constructors
        Babel.ClassMethod(range, kind, name, args, body, computed, isStatic)
    let baseClass = baseClass |> Option.map (fun loc ->
        typeRef com ctx SourceLocation.Empty (Some loc.file) loc.fullName)
    decls
    |> List.map (function
        | Fabel.MemberDeclaration m ->
            let kind, name, isStatic =
                match m.Kind with
                | Fabel.Constructor -> Babel.ClassConstructor, "constructor", false
                | Fabel.Method name -> Babel.ClassFunction, name, m.IsStatic
                | Fabel.Getter name -> Babel.ClassGetter, name, m.IsStatic
                | Fabel.Setter name -> Babel.ClassSetter, name, m.IsStatic
            declareMember m.Range kind name m.Function isStatic
        | Fabel.ActionDeclaration _
        | Fabel.EntityDeclaration _ as decl ->
            failwithf "Unexpected declaration in class: %A" decl)
    |> List.map U2<_,Babel.ClassProperty>.Case1
    |> fun meths -> Babel.ClassExpression(classRange,
                        Babel.ClassBody(classRange, meths), ?super=baseClass)

let rec private transformModDecls com ctx modIdent decls = seq {
    // TODO: Keep track of sanitized member names to be sure they don't clash? 
    let declareMember range var name expr isPublic =
        let var = match var with Some x -> x | None -> identFromName range name
        let expr = if not isPublic then expr else assign (get modIdent name) expr 
        varDeclaration range var expr :> Babel.Statement
    for decl in decls do
    match decl with
    | Fabel.ActionDeclaration e ->
        yield transformStatement com ctx e
    | Fabel.MemberDeclaration m ->
        let expr, name =
            match m.Kind with
            | Fabel.Getter name ->
                let finfo = FabelFunctionInfo ([], false, m.Function.Body, Fabel.Immediate)
                let (BabelFunctionInfo (_, body, _, _)) = transformFunctionBody com ctx finfo
                match body with
                | U2.Case1 e -> Babel.DoExpression(e.loc, e) :> Babel.Expression, name
                | U2.Case2 e -> e, name // TODO: Optimize do expressions?
            | Fabel.Method name ->
                let f = m.Function
                upcast funcExpression com ctx f.Range f.Arguments f.RestParams f.Body f.Kind, name
            | Fabel.Constructor | Fabel.Setter _ ->
                failwithf "Unexpected member in module: %A" m.Kind
        yield declareMember m.Range None name expr m.IsPublic
    | Fabel.EntityDeclaration (e, sub) ->
        let declareClass range baseClass =
            // Don't create a new context for class declarations
            let classExpr = transformClass com ctx range baseClass sub
            declareMember range None e.Name classExpr e.IsPublic
        match e.Kind with
        // Interfaces, attribute or erased declarations shouldn't reach this point
        | Fabel.Interface -> ()
        | Fabel.Class baseClass -> yield declareClass e.Range baseClass
        | Fabel.Union | Fabel.Record -> yield declareClass e.Range None 
        | Fabel.Module ->
            let nestedIdent, protectedIdent =
                let memberNames =
                    sub |> Seq.choose (function
                        | Fabel.EntityDeclaration (e, _) -> Some e.Name
                        | Fabel.ActionDeclaration e -> None
                        | Fabel.MemberDeclaration m ->
                            match m.Kind with
                            | Fabel.Method name | Fabel.Getter name -> Some name
                            | Fabel.Constructor | Fabel.Setter _ -> None)
                    |> Set.ofSeq
                identFromName e.Range e.Name,
                // Protect module identifier against members with same name
                Babel.Identifier(e.Range, Naming.sanitizeIdent memberNames.Contains e.Name)
            // var NestedMod = ParentMod.NestedMod = {};
            yield declareMember e.Range (Some nestedIdent) e.Name
                    (Babel.ObjectExpression(SourceLocation.Empty, [])) e.IsPublic
            // (function (/* protected */ NestedMod_1) {
            //     var privateVar = 1;
            //     var publicVar = NestedMod_1.publicVar = 2;
            //     var NestedMod = NestedMod_1.NestedMod = {};
            // })(NestedMod);
            let nestedDecls =
                let ctx = { ctx with moduleFullName = e.FullName } 
                transformModDecls com ctx protectedIdent sub |> Seq.toList
            let range = nestedDecls |> Seq.map (fun x -> x.loc) |> Seq.reduce (+)
            yield Babel.ExpressionStatement(range,
                    Babel.CallExpression(range,
                        Babel.FunctionExpression(range, [protectedIdent],
                            Babel.BlockStatement(range, nestedDecls)),
                            [U2.Case1 (upcast nestedIdent)])) :> Babel.Statement
    }
    
let makeCompiler (com: ICompiler) (files: Fabel.File list) =
    let fileMap =
        files |> Seq.map (fun f -> f.FileName, f) |> Map.ofSeq
    { new IBabelCompiler with
        member bcom.GetFabelFile fileName =
            Map.tryFind fileName fileMap
            |> function Some file -> file
                      | None -> failwithf "File not parsed: %s" fileName
        member bcom.GetImport ctx range moduleName =
            match ctx.imports.TryGetValue moduleName with
            | true, import ->
                upcast Babel.Identifier(range, import)
            | false, _ ->
                let import = Naming.getImportModuleIdent ctx.imports.Count
                ctx.imports.Add(moduleName, import)
                upcast Babel.Identifier(range, import)
        member bcom.TransformExpr ctx e = transformExpr bcom ctx e
        member bcom.TransformStatement ctx e = transformStatement bcom ctx e
        member bcom.TransformFunction ctx e = transformFunctionBody bcom ctx e
      interface ICompiler with
        member __.Options = com.Options }

let transformFiles (com: ICompiler) (files: Fabel.File list): Babel.Program list =
    let babelCom = makeCompiler com files
    files |> List.choose (fun file ->
        match file.Root with
        | Some (Fabel.EntityDeclaration (root, decls)) ->
            // for decl in decls do printfn "%A" decl
            let ctx = {
                file = file.FileName
                moduleFullName = root.FullName
                imports = System.Collections.Generic.Dictionary<_,_>()
            }
            let emptyLoc = SourceLocation.Empty
            let rootIdent =
                Babel.Identifier(emptyLoc, Naming.rootModuleIdent)
            let rootHead =
                // var $M0 = {};
                // exports.default = $M0;
                [ varDeclaration emptyLoc rootIdent (Babel.ObjectExpression(emptyLoc, []))
                    :> Babel.Statement |> U2.Case1
                  Babel.ExportDefaultDeclaration(emptyLoc, U2.Case2 (upcast rootIdent))
                    :> Babel.ModuleDeclaration |> U2.Case2 ]
            let rootDecls, lastRange =
                let rootDecls = transformModDecls babelCom ctx rootIdent decls |> Seq.toList
                List.map (fun x -> U2<_,Babel.ModuleDeclaration>.Case1 x) rootDecls,
                (List.last rootDecls).loc
            Babel.Program (emptyLoc + lastRange, rootHead @ rootDecls) |> Some
        | _ -> None)
