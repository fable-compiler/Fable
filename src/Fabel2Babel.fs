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

type BabelFunctionInfo =
    {
        args: Babel.Pattern list
        body: U2<Babel.BlockStatement, Babel.Expression>
        generator: bool
        async: bool
    }
    static member Create (args, body, generator, async) =
        { args=args; body=body; generator=generator; async=async }

type IBabelCompiler =
    inherit ICompiler
    abstract GetFabelFile: string -> Fabel.File
    abstract GetImport: Context -> SourceLocation -> string -> Babel.Expression
    abstract TransformExpr: Context -> Fabel.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fabel.Expr -> Babel.Statement
    abstract TransformFunction: Context -> Fabel.FunctionInfo -> BabelFunctionInfo

let (|ExprType|) (fexpr: Fabel.Expr) = fexpr.Type
let (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
let (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e
    
let private ident (id: Fabel.Ident) =
    Babel.Identifier id.name

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
        | Fabel.Value (Fabel.StringConst name)
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
        file.ExternalEntities
        |> List.tryFind (fun ext ->
            fullName = ext.FullName || fullName.StartsWith ext.FullName)
        |> function
            | Some (Fabel.ImportModule (ns, modName)) ->
                Some (com.GetImport ctx range modName)
                |> makeExpr (getDiff ns fullName)
            | Some (Fabel.GlobalModule ns) ->
                makeExpr (getDiff ns fullName) None
            | None when ctx.file <> file.FileName ->
                Some (com.GetImport ctx file.FileName)
                |> makeExpr (getDiff file.Root.FullName fullName)
            | None ->
                makeExpr (getDiff ctx.moduleFullName fullName) None

let private assign range left right =
    Babel.AssignmentExpression(AssignEqual, left, right, ?loc=range)
    :> Babel.Expression

let private block (com: IBabelCompiler) ctx range (exprs: Fabel.Expr list) =
    let exprs = match exprs with
                | [Fabel.Sequential (statements,_)] -> statements
                | _ -> exprs
    Babel.BlockStatement (exprs |> List.map (com.TransformStatement ctx), ?loc=range)

let private returnBlock e =
    Babel.BlockStatement([Babel.ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)

let private funcExpression, funcDeclaration =
    let func (com: IBabelCompiler) ctx funcInfo =
        let f = com.TransformFunction ctx funcInfo
        let body = match f.body with U2.Case1 block -> block | U2.Case2 expr -> returnBlock expr
        f.args, body, f.generator, f.async
    (fun (com: IBabelCompiler) ctx funcInfo ->
        let args, body, generator, async = func com ctx funcInfo
        Babel.FunctionExpression (args, body, generator, async, ?loc=body.loc)),
    (fun (com: IBabelCompiler) ctx id funcInfo ->
        let args, body, generator, async = func com ctx funcInfo
        Babel.FunctionDeclaration(id, args, body, generator, async, ?loc=body.loc))

let private funcArrow (com: IBabelCompiler) ctx funcInfo =
    let f = com.TransformFunction ctx funcInfo
    let range = match f.body with U2.Case1 x -> x.loc | U2.Case2 x -> x.loc
    Babel.ArrowFunctionExpression (f.args, f.body, f.async, ?loc=range)
    :> Babel.Expression

/// Immediately Invoked Function Expression
let private iife (com: IBabelCompiler) ctx (expr: Fabel.Expr) =
    let lambda =
        Fabel.FunctionInfo.Create(Fabel.Immediate, [], false, expr)
        |> funcExpression com ctx
    Babel.CallExpression (lambda, [], ?loc=expr.Range)

let private varDeclaration range var (value: Babel.Expression) =
    Babel.VariableDeclaration(range, var, value)

let private transformStatement com ctx (expr: Fabel.Expr): Babel.Statement =
    match expr with
    | Fabel.Loop (loopKind,_) ->
        match loopKind with
        | Fabel.While (TransformExpr com ctx guard, body) ->
            upcast Babel.WhileStatement (guard, block com ctx body.Range [body], ?loc=expr.Range)
        | Fabel.ForOf (var, TransformExpr com ctx enumerable, body) ->
            // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right
            let var =
                Babel.VariableDeclaration(
                    Babel.VariableDeclarationKind.Var,
                    [Babel.VariableDeclarator (ident var)])
            upcast Babel.ForOfStatement (
                U2.Case1 var, enumerable, block com ctx body.Range [body], ?loc=expr.Range)
        | Fabel.For (var, TransformExpr com ctx start,
                        TransformExpr com ctx limit, body, isUp) ->
            upcast Babel.ForStatement (
                block com ctx body.Range [body],
                start |> varDeclaration None (ident var) |> U2.Case1,
                Babel.BinaryExpression (BinaryOperator.BinaryLessOrEqual, ident var, limit),
                Babel.UpdateExpression (UpdateOperator.UpdatePlus, false, ident var), ?loc=expr.Range)

    | Fabel.Set (callee, property, TransformExpr com ctx value, range) ->
        let left =
            match property with
            | None -> com.TransformExpr ctx callee
            | Some property -> getExpr com ctx callee property
        upcast Babel.ExpressionStatement (assign range left value, ?loc = expr.Range)

    | Fabel.VarDeclaration (var, TransformExpr com ctx value, _isMutable) ->
        varDeclaration expr.Range (ident var) value :> Babel.Statement

    | Fabel.TryCatch (body, catch, finalizer, _) ->
        let handler =
            catch |> Option.map (fun (param, body) ->
                Babel.CatchClause (ident param, block com ctx body.Range [body]))
        let finalizer =
            match finalizer with
            | None -> None
            | Some e -> Some (block com ctx e.Range [e])
        upcast Babel.TryStatement (block com ctx expr.Range [body],
            ?handler=handler, ?finalizer=finalizer, ?loc=expr.Range)

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformStatement com ctx thenExpr, elseExpr, _) ->
        let elseExpr =
            match elseExpr with
            | Fabel.Value Fabel.Null -> None
            | _ -> Some (com.TransformStatement ctx elseExpr)
        upcast Babel.IfStatement (expr.Range, guardExpr, thenExpr, ?alternate=elseExpr)

    | Fabel.Sequential _ ->
        failwithf "Sequence when single statement expected in %A: %A" expr.Range expr

    // Expressions become ExpressionStatements
    | Fabel.Value _ | Fabel.Get _ | Fabel.Apply _ ->
        upcast Babel.ExpressionStatement (com.TransformExpr ctx expr, ?loc=expr.Range)

let private transformExpr com ctx (expr: Fabel.Expr): Babel.Expression =
    match expr with
    | Fabel.Value kind ->
        match kind with
        | Fabel.CoreModule name ->
            let coreLibRef = Babel.Identifier Naming.coreLibIdent
            upcast Babel.MemberExpression (coreLibRef, Babel.Identifier (name), false, ?loc=expr.Range)
        | Fabel.This _ -> upcast Babel.ThisExpression (?loc=expr.Range)
        | Fabel.Super _ -> upcast Babel.Super (?loc=expr.Range)
        | Fabel.Null -> upcast Babel.NullLiteral (?loc=expr.Range)
        | Fabel.IdentValue {name=name} -> upcast Babel.Identifier (name, ?loc=expr.Range)
        | Fabel.IntConst (x,_) -> upcast Babel.NumericLiteral (U2.Case1 x, ?loc=expr.Range)
        | Fabel.FloatConst (x,_) -> upcast Babel.NumericLiteral (U2.Case2 x, ?loc=expr.Range)
        | Fabel.StringConst x -> upcast Babel.StringLiteral (x, ?loc=expr.Range)
        | Fabel.BoolConst x -> upcast Babel.BooleanLiteral (x, ?loc=expr.Range)
        | Fabel.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (source, flags, ?loc=expr.Range)
        | Fabel.Lambda finfo -> funcArrow com ctx finfo
        | Fabel.ArrayConst (items, kind) -> failwith "TODO: Array initializers"
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

    | Fabel.Apply (callee, args, isPrimaryConstructor, _, _) ->
        match callee, args with
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

    | Fabel.Get (callee, property, _) ->
        getExpr com ctx callee property

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformExpr com ctx thenExpr,
                        TransformExpr com ctx elseExpr, _) ->
        upcast Babel.ConditionalExpression (
            guardExpr, thenExpr, elseExpr, ?loc = expr.Range)

    | Fabel.Sequential (statements, _) ->
        Babel.BlockStatement (statements |> List.map (com.TransformStatement ctx), [])
        |> fun block -> upcast Babel.DoExpression (block)

    | Fabel.TryCatch _ ->
        upcast (iife com ctx expr)

    | Fabel.Loop _ | Fabel.Set _  | Fabel.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr

let private transformFunction com ctx (finfo: Fabel.FunctionInfo) =
    let generator, async =
        match finfo.kind with
        | Fabel.Immediate -> false, false
        | Fabel.Generator -> true, false
        | Fabel.Async -> false, true
    let args: Babel.Pattern list =
        if finfo.restParams then failwith "TODO: RestParams"
        else finfo.args |> List.map (fun x -> upcast ident x)
    let body: U2<Babel.BlockStatement, Babel.Expression> =
        match finfo.body with
        | ExprType (Fabel.PrimitiveType Fabel.Unit) ->
            block com ctx finfo.body.Range [finfo.body] |> U2.Case1
        | Fabel.TryCatch (tryBody, handler, finalizer, tryRange) ->
            let handler =
                handler |> Option.map (fun (param, body) ->
                    let clause = transformExpr com ctx body |> returnBlock
                    Babel.CatchClause (ident param, clause))
            let finalizer =
                finalizer |> Option.map (fun x -> block com ctx x.Range [x])
            let tryBody =
                transformExpr com ctx tryBody |> returnBlock
            Babel.BlockStatement (
                [Babel.TryStatement (tryBody, ?handler=handler, ?finalizer=finalizer, ?loc=tryRange)],
                ?loc = finfo.body.Range) |> U2.Case1
        | _ ->
            transformExpr com ctx finfo.body |> U2.Case2
    BabelFunctionInfo.Create(args, body, generator, async)

let private transformTestSuite com ctx decls: Babel.CallExpression =
    failwith "TODO: TestSuite members"

// type ClassMethodKind =
//     | ClassConstructor | ClassMethod | ClassGetter | ClassSetter
let private transformClass com ctx classRange (baseClass: Fabel.EntityLocation option) decls =
    let declareMember range kind name (finfo: Fabel.FunctionInfo) isStatic =
        let name, computed: Babel.Expression * bool =
            if Naming.identForbiddenChars.IsMatch name
            then upcast Babel.StringLiteral name, true
            else upcast Babel.Identifier name, false
        let finfo = transformFunction com ctx finfo
        let body = match finfo.body with U2.Case1 e -> e | U2.Case2 e -> returnBlock e
        // TODO: Optimization: remove null statement that F# compiler adds at the bottom of constructors
        Babel.ClassMethod(range, kind, name, finfo.args, body, computed, isStatic)
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
    |> fun meths -> Babel.ClassExpression(classRange, Babel.ClassBody(classRange, meths), ?super=baseClass)

let rec private transformModDecls com ctx modIdent decls = seq {
    // TODO: Keep track of sanitized member names to be sure they don't clash?
    let declareMember range var name expr isPublic =
        let var = match var with Some x -> x | None -> identFromName name
        let expr = if not isPublic then expr else assign range (get modIdent name) expr
        varDeclaration range var expr :> Babel.Statement
    let declareClass classRange className classDecls isPublic baseClass =
        // Don't create a new context for class declarations
        let classExpr = transformClass com ctx classRange baseClass classDecls
        declareMember (Some classRange) None className classExpr isPublic
    for decl in decls do
    match decl with
    | Fabel.ActionDeclaration e ->
        yield transformStatement com ctx e
    | Fabel.MemberDeclaration m ->
        let expr, name =
            match m.Kind with
            | Fabel.Getter name ->
                let finfo =
                    Fabel.FunctionInfo.Create(Fabel.Immediate, [], false, m.Function.body)
                    |> transformFunction com ctx
                match finfo.body with
                | U2.Case2 e -> e, name
                | U2.Case1 e -> Babel.DoExpression(e, ?loc=e.loc) :> Babel.Expression, name
            | Fabel.Method name ->
                upcast funcExpression com ctx m.Function, name
            | Fabel.Constructor | Fabel.Setter _ ->
                failwithf "Unexpected member in module: %A" m.Kind
        yield declareMember (Some m.Range) None name expr m.IsPublic
    | Fabel.EntityDeclaration (ent, entDecls, entRange) ->
        match ent.Kind with
        // Interfaces, attribute or erased declarations shouldn't reach this point
        | Fabel.Interface -> failwithf "Cannot emit interface declaration into JS: %s" ent.FullName
        | Fabel.Class baseClass -> yield declareClass entRange ent.Name entDecls ent.IsPublic baseClass
        | Fabel.Union | Fabel.Record -> yield declareClass entRange ent.Name entDecls ent.IsPublic None
        | Fabel.Module ->
            let nestedIdent, protectedIdent =
                let memberNames =
                    entDecls |> Seq.choose (function
                        | Fabel.EntityDeclaration (ent,_,_) -> Some ent.Name
                        | Fabel.ActionDeclaration ent -> None
                        | Fabel.MemberDeclaration m ->
                            match m.Kind with
                            | Fabel.Method name | Fabel.Getter name -> Some name
                            | Fabel.Constructor | Fabel.Setter _ -> None)
                    |> Set.ofSeq
                identFromName ent.Name,
                // Protect module identifier against members with same name
                Babel.Identifier (Naming.sanitizeIdent memberNames.Contains ent.Name)
            // var NestedMod = ParentMod.NestedMod = {};
            yield declareMember None (Some nestedIdent) ent.Name (Babel.ObjectExpression []) ent.IsPublic
            // (function (/* protected */ NestedMod_1) {
            //     var privateVar = 1;
            //     var publicVar = NestedMod_1.publicVar = 2;
            //     var NestedMod = NestedMod_1.NestedMod = {};
            // })(NestedMod);
            let nestedDecls =
                let ctx = { ctx with moduleFullName = ent.FullName }
                transformModDecls com ctx protectedIdent entDecls |> Seq.toList
            yield Babel.ExpressionStatement(
                    Babel.CallExpression(
                        Babel.FunctionExpression([protectedIdent],
                            Babel.BlockStatement nestedDecls,
                            ?loc=Some entRange),
                        [U2.Case1 (upcast nestedIdent)],
                        entRange),
                    entRange) :> Babel.Statement
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
        member bcom.TransformFunction ctx e = transformFunction bcom ctx e
      interface ICompiler with
        member __.Options = com.Options }

let transformFiles (com: ICompiler) (files: Fabel.File list): Babel.Program list =
    let babelCom = makeCompiler com files
    files |> List.choose (fun file ->
        match file.Declarations with
        | [] -> None
        | _ ->
            // for decl in decls do printfn "%A" decl
            let ctx = {
                file = file.FileName
                moduleFullName = file.Root.FullName
                imports = System.Collections.Generic.Dictionary<_,_>()
            }
            let emptyLoc = SourceLocation.Empty
            let rootIdent =
                Babel.Identifier Naming.rootModuleIdent
            let rootHead = [
                // var $M0 = {};
                // exports.default = $M0;
                varDeclaration None rootIdent (Babel.ObjectExpression [])
                    :> Babel.Statement |> U2.Case1
                Babel.ExportDefaultDeclaration (U2.Case2 (upcast rootIdent))
                    :> Babel.ModuleDeclaration |> U2.Case2
            ]
            let rootDecls =
                transformModDecls babelCom ctx rootIdent file.Declarations
                |> Seq.map (fun x -> U2<_,Babel.ModuleDeclaration>.Case1 x)
                |> Seq.toList
            Babel.Program (rootHead@rootDecls) |> Some)
