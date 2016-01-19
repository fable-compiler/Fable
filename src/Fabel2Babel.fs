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
    abstract GetImport: Context -> string -> Babel.Expression
    abstract TransformExpr: Context -> Fabel.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fabel.Expr -> Babel.Statement
    abstract TransformFunction: Context -> FabelFunctionInfo -> BabelFunctionInfo
    
let (|ExprKind|) (e: Fabel.Expr) = e.Kind
let (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
let (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e

let (|Block|) (e: U2<Babel.BlockStatement, Babel.Expression>) =
    match e with
    | U2.Case1 e -> e
    | U2.Case2 e -> Babel.BlockStatement [Babel.ReturnStatement e]
    
let private ident (expr: Fabel.IdentifierExpr) =
    Babel.Identifier (expr.Name, ?loc=expr.Range)

let private identFromName name =
    let sanitizedName = Naming.sanitizeIdent (fun _ -> false) name
    Babel.Identifier name
    
let get left propName =
    if Naming.identForbiddenChars.IsMatch propName
    then Babel.MemberExpression(left, Babel.StringLiteral propName, true)
    else Babel.MemberExpression(left, Babel.Identifier propName, false)
    :> Babel.Expression
    
let private getExpr com ctx (TransformExpr com ctx expr) (property: Fabel.Expr) =
    let property, computed =
        match property with
        | ExprKind (Fabel.Value (Fabel.StringConst name))
            when Naming.identForbiddenChars.IsMatch name = false ->
            Babel.Identifier (name) :> Babel.Expression, false
        | TransformExpr com ctx property -> property, true
    match expr with
    | :? Babel.EmptyExpression -> property
    | _ -> Babel.MemberExpression (expr, property, computed) :> Babel.Expression

let private typeRef (com: IBabelCompiler) ctx file fullName: Babel.Expression =
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
            | [] -> upcast Babel.EmptyExpression()
            | m::ms -> identFromName m :> Babel.Expression |> Some |> makeExpr ms
    match file with
    | None -> failwithf "Cannot reference type: %s" fullName
    | Some file ->
        let file = com.GetFabelFile file
        file.External
        |> List.tryFind (fun ext ->
            fullName = ext.FullName || fullName.StartsWith ext.FullName)
        |> function
            | Some (Fabel.ImportModule (ns, modName)) ->
                Some (com.GetImport ctx modName)
                |> makeExpr (getDiff ns fullName)
            | Some (Fabel.GlobalModule ns) ->
                makeExpr (getDiff ns fullName) None
            | None when ctx.file <> file.FileName ->
                Some (com.GetImport ctx file.FileName)
                |> makeExpr (getDiff file.RootFullName fullName)
            | None ->
                makeExpr (getDiff ctx.moduleFullName fullName) None

let private assign left right =
    Babel.AssignmentExpression(AssignEqual, left, right) :> Babel.Expression
    
let private block (com: IBabelCompiler) ctx (exprs: Fabel.Expr list) =
    let exprs = match exprs with
                | [ExprKind (Fabel.Sequential statements)] -> statements
                | _ -> exprs
    Babel.BlockStatement (exprs |> List.map (com.TransformStatement ctx))

let private funcExpression, funcDeclaration =
    let func (com: IBabelCompiler) ctx funcInfo =
        let (BabelFunctionInfo (args, body, generator, async)) = com.TransformFunction ctx funcInfo
        let body = match body with
                    | U2.Case1 block -> block
                    | U2.Case2 expr -> Babel.BlockStatement [Babel.ReturnStatement expr]
        args, body, generator, async
    let funcExpression (com: IBabelCompiler) ctx range args restParams body kind =
        let args, body, generator, async =
            func com ctx (FabelFunctionInfo (args, restParams, body, kind))
        Babel.FunctionExpression (args, body, generator, async, ?loc=range)
    let funcDeclaration (com: IBabelCompiler) ctx id args restParams body kind =
        let args, body, generator, async =
            func com ctx (FabelFunctionInfo (args, restParams, body, kind))
        Babel.FunctionDeclaration(id, args, body, generator, async)
    funcExpression, funcDeclaration

let private funcArrow (com: IBabelCompiler) ctx range args restParams body kind =
    let (BabelFunctionInfo (args, body, generator, async)) =
        com.TransformFunction ctx (FabelFunctionInfo (args, restParams, body, kind))
    Babel.ArrowFunctionExpression (args, body, async, ?loc=range) :> Babel.Expression

let private varDeclaration range var value =
    Babel.VariableDeclaration (
        Babel.VariableDeclarationKind.Var,
        [Babel.VariableDeclarator (var, value)],
        ?loc = range)

/// Immediately Invoked Function Expression
let private iife com ctx body =
    let fexpr = funcExpression com ctx None [] false body Fabel.Immediate
    Babel.CallExpression (fexpr, [])

let private transformStatement com ctx (expr: Fabel.Expr): Babel.Statement =
    match expr.Kind with
    | Fabel.Loop loopKind ->
        match loopKind with
        | Fabel.While (TransformExpr com ctx guard, body) ->
            upcast Babel.WhileStatement (guard, block com ctx [body])
        | Fabel.ForOf (var, TransformExpr com ctx enumerable, body) ->
            // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right 
            let var =
                Babel.VariableDeclaration(
                    Babel.VariableDeclarationKind.Var,
                    [Babel.VariableDeclarator (ident var)])
            upcast Babel.ForOfStatement (
                U2.Case1 var, enumerable, block com ctx [body])
        | Fabel.For (var, TransformExpr com ctx start,
                        TransformExpr com ctx limit, body, isUp) ->
            upcast Babel.ForStatement (
                block com ctx [body],
                start |> varDeclaration None (ident var) |> U2.Case1,
                Babel.BinaryExpression (BinaryOperator.BinaryLessOrEqual, ident var, limit),
                Babel.UpdateExpression (UpdateOperator.UpdatePlus, false, ident var))

    | Fabel.Set (callee, property, TransformExpr com ctx value) ->
        let left =
            match property with
            | None -> com.TransformExpr ctx callee
            | Some property -> getExpr com ctx callee property
        upcast Babel.ExpressionStatement (assign left value, ?loc = expr.Range)

    | Fabel.VarDeclaration (var, TransformExpr com ctx value, _isMutable) ->
        varDeclaration expr.Range (ident var) value :> Babel.Statement

    | Fabel.TryCatch (body, catch, finalizers) ->
        let handler =
            catch |> Option.map (fun (param, body) ->
                Babel.CatchClause (ident param, block com ctx [body]))
        let finalizer =
            match finalizers with
            | [] -> None
            | xs -> Some (block com ctx xs)
        upcast Babel.TryStatement (
            block com ctx [body], ?handler=handler, ?finalizer=finalizer)

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformStatement com ctx thenExpr, elseExpr) ->
        let elseExpr =
            match elseExpr.Kind with
            | Fabel.Value (Fabel.Null) -> None
            | _ -> Some (com.TransformStatement ctx elseExpr)
        upcast Babel.IfStatement (
            guardExpr, thenExpr, ?alternate=elseExpr, ?loc=expr.Range)

    | Fabel.Sequential _ ->
        failwithf "Sequence when single statement expected in %A: %A" expr.Range expr 

    // Expressions become ExpressionStatements
    | Fabel.Value _ | Fabel.Get _ | Fabel.Apply _ | Fabel.Lambda _ | Fabel.Operation _ ->
        upcast Babel.ExpressionStatement (com.TransformExpr ctx expr, ?loc=expr.Range)

let private transformExpr com ctx (expr: Fabel.Expr): Babel.Expression =
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
        | Fabel.ArrayConst items -> failwith "TODO: Array initializers"
        | Fabel.ObjExpr _ -> failwith "TODO: Object expressions"
        | Fabel.TypeRef typ ->
            match typ with
            | Fabel.DeclaredType typEnt ->
                let typFullName =
                    if Option.isSome (typEnt.HasDecoratorNamed "Erase") 
                    then typEnt.FullName.Substring(0, typEnt.FullName.LastIndexOf ".")
                    else typEnt.FullName
                typeRef com ctx typEnt.File typFullName
            | _ -> failwithf "Not supported type reference: %A" typ

    | Fabel.Operation op ->
        match op with
        | Fabel.Logical (op, left, right) ->
            failwith "TODO"
        | Fabel.Unary (op, (TransformExpr com ctx operand as expr)) ->
            upcast Babel.UnaryExpression (op, operand, ?loc=expr.Range)
        | Fabel.Binary (op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast Babel.BinaryExpression (op, left, right, ?loc=expr.Range)

    | Fabel.Apply (TransformExpr com ctx callee, args, isPrimaryConstructor) ->
        let args = args |> List.map (com.TransformExpr ctx >> U2<_,_>.Case1)
        if isPrimaryConstructor
        then upcast Babel.NewExpression (callee, args, ?loc=expr.Range)
        else upcast Babel.CallExpression (callee, args, ?loc=expr.Range)

    | Fabel.Lambda (args, body, kind, restParams) ->
        funcArrow com ctx expr.Range args restParams body kind

    | Fabel.Get (callee, property) ->
        getExpr com ctx callee property

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformExpr com ctx thenExpr,
                        TransformExpr com ctx elseExpr) ->
        upcast Babel.ConditionalExpression (
            guardExpr, thenExpr, elseExpr, ?loc = expr.Range)

    | Fabel.Sequential statements ->
        Babel.BlockStatement (statements |> List.map (com.TransformStatement ctx), [])
        |> fun block -> upcast Babel.DoExpression (block)

    | Fabel.TryCatch _ ->
        upcast (iife com ctx expr)

    | Fabel.Loop _ | Fabel.Set _  | Fabel.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr 
    
let private transformFunctionBody com ctx (FabelFunctionInfo (args, restParams, body, kind)) =
    let returnBlock e =
        Babel.BlockStatement [Babel.ReturnStatement e]
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
            let handler = catch |> Option.map (fun (param, body) ->
                Babel.CatchClause (ident param, returnBlock (transformExpr com ctx body)))
            let finalizer = match finalizers with [] -> None | xs -> Some (block com ctx xs)
            Babel.BlockStatement (
                [Babel.TryStatement (
                    returnBlock (transformExpr com ctx tryBody),
                    ?handler = handler,
                    ?finalizer = finalizer,
                    ?loc = body.Range)], []) |> U2.Case1
        | _ -> returnBlock (transformExpr com ctx body) |> U2.Case1
    BabelFunctionInfo (args, body, generator, async)
    
let private transformTestSuite com ctx decls: Babel.CallExpression =
    failwith "TODO: TestSuite members"
    
// type ClassMethodKind =
//     | ClassConstructor | ClassMethod | ClassGetter | ClassSetter
let private transformClass com ctx (baseClass: Fabel.EntityLocation option) decls =
    let declareMember kind name (f: Fabel.LambdaExpr) isStatic =
        let name, computed: Babel.Expression * bool =
            if Naming.identForbiddenChars.IsMatch name
            then upcast Babel.StringLiteral name, true
            else upcast Babel.Identifier name, false
        let finfo = FabelFunctionInfo (f.Arguments, f.RestParams, f.Body, f.Kind)
        let (BabelFunctionInfo (args, Block body, generator, async)) =
            transformFunctionBody com ctx finfo
        // TODO: Optimization: remove null statement that F# compiler 
        // adds at the bottom of constructors
        Babel.ClassMethod(kind, name, args, body, computed, isStatic)
    let baseClass = baseClass |> Option.map (fun loc ->
        typeRef com ctx (Some loc.file) loc.fullName)
    decls
    |> List.map (function
        | Fabel.MemberDeclaration m ->
            let kind, name, isStatic =
                match m.Kind with
                | Fabel.Constructor -> Babel.ClassConstructor, "constructor", false
                | Fabel.Method name -> Babel.ClassFunction, name, m.IsStatic
                | Fabel.Getter name -> Babel.ClassGetter, name, m.IsStatic
                | Fabel.Setter name -> Babel.ClassSetter, name, m.IsStatic
            declareMember kind name m.Function isStatic
        | Fabel.ActionDeclaration _
        | Fabel.EntityDeclaration _ as decl ->
            failwithf "Unexpected declaration in class: %A" decl)
    |> List.map U2<_,Babel.ClassProperty>.Case1
    |> fun meths -> Babel.ClassExpression(Babel.ClassBody meths, ?super=baseClass)

let rec private transformModDecls com ctx modIdent decls = seq {
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
                let finfo = FabelFunctionInfo ([], false, m.Function.Body, Fabel.Immediate)
                let (BabelFunctionInfo (_, body, _, _)) = transformFunctionBody com ctx finfo
                match body with
                | U2.Case1 e -> Babel.DoExpression e :> Babel.Expression, name
                | U2.Case2 e -> e, name // TODO: Optimize do expressions?
            | Fabel.Method name ->
                let f = m.Function
                upcast funcExpression com ctx None f.Arguments f.RestParams f.Body f.Kind, name
            | Fabel.Constructor | Fabel.Setter _ ->
                failwithf "Unexpected member in module: %A" m.Kind
        yield declareMember None name expr m.IsPublic
    | Fabel.EntityDeclaration (e, sub) ->
        let declareClass baseClass =
            // Don't create a new context for class declarations
            let classExpr = transformClass com ctx baseClass sub
            declareMember None e.Name classExpr e.IsPublic
        match e.Kind with
        // Interfaces, attribute or erased declarations shouldn't reach this point
        | Fabel.Interface -> ()
        | Fabel.Class baseClass -> yield declareClass baseClass
        | Fabel.Union | Fabel.Record -> yield declareClass None 
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
                let ctx = { ctx with moduleFullName = e.FullName } 
                transformModDecls com ctx protectedIdent sub |> Seq.toList
            yield Babel.ExpressionStatement(
                    Babel.CallExpression(
                        Babel.FunctionExpression([protectedIdent],
                            Babel.BlockStatement nestedDecls),
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
        member bcom.GetImport ctx moduleName =
            match ctx.imports.TryGetValue moduleName with
            | true, import ->
                upcast Babel.Identifier import
            | false, _ ->
                let import = Naming.getImportModuleIdent ctx.imports.Count
                ctx.imports.Add(moduleName, import)
                upcast Babel.Identifier import
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
            let rootIdent =
                Babel.Identifier Naming.rootModuleIdent
            let rootHead = [
                // export default $M0;
                varDeclaration None rootIdent (Babel.ObjectExpression [])
                    :> Babel.Statement |> U2.Case1
                // var publicVar = $M0.publicVar = {};
                Babel.ExportDefaultDeclaration (U2.Case2 (upcast rootIdent))
                    :> Babel.ModuleDeclaration |> U2.Case2
            ]
            let rootDecls =
                transformModDecls babelCom ctx rootIdent decls
                |> Seq.map (fun x -> U2<_,Babel.ModuleDeclaration>.Case1 x)
                |> Seq.toList
            Babel.Program (rootHead@rootDecls) |> Some
        | _ -> None)
