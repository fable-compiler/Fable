module rec Fable.Transforms.Babel2Python

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Python

[<RequireQualifiedAccess>]
type ReturnStrategy =
    | Return
    | NoReturn

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
    { RootScope: HashSet<string>
      DeclarationScopes: HashSet<string>
      CurrentDeclarationScope: HashSet<string> }

type Context =
    { //UsedNames: UsedNames
      DecisionTargets: (Fable.Ident list * Fable.Expr) list
      HoistVars: Fable.Ident list -> bool
      TailCallOpportunity: ITailCallOpportunity option
      OptimizeTailCall: unit -> unit
      ScopedTypeParams: Set<string> }

type IPythonCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Statement list
    abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Babel.Expression -> Expression * Statement list
    abstract TransformAsStatements: Context * ReturnStrategy option * Babel.Statement -> Statement list
    abstract TransformAsStatements: Context * ReturnStrategy option * Babel.Expression -> Statement list
    abstract TransformAsClassDef: Context * Babel.ClassDeclaration -> Statement list
    abstract TransformAsImports: Context * Babel.ImportDeclaration -> Statement list
    //abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement

    abstract WarnOnlyOnce: string * ?range:SourceLocation -> unit


module Helpers =
    let index = (Seq.initInfinite id).GetEnumerator()

    let getIdentifier (name: string): Identifier =
        do index.MoveNext() |> ignore
        let idx = index.Current.ToString()
        Identifier($"{name}_{idx}")


    /// Replaces all '$' with '_'
    let cleanNameAsPythonIdentifier (name: string) = name.Replace('$', '_')

    let rewriteFableImport moduleName =
        let _reFableLib =
            Regex(".*\/fable-library[\.0-9]*\/(?<module>[^\/]*)\.js", RegexOptions.Compiled)

        let m = _reFableLib.Match(moduleName)
        printfn "Count: %d" m.Groups.Count
        if m.Groups.Count > 1 then
            let pymodule = m.Groups.["module"].Value.ToLower()

            let moduleName =
                String.concat "." [ "fable"; pymodule ]

            moduleName
        else
            // TODO: Can we expect all modules to be lower case?
            let moduleName = moduleName.Replace("/", "").ToLower()
            printfn "moduleName: %s" moduleName
            moduleName

    let unzipArgs (args: (Expression * Statement list) list): Expression list * Statement list =
        let stmts = args |> List.map snd |> List.collect id
        let args = args |> List.map fst
        args, stmts

    /// A few statements in the generated Babel AST do not produce any effect, and will not be printet. But they are
    /// left in the AST and we need to skip them since they are not valid for Python (either).
    let isProductiveStatement (stmt: Statement) =
        let rec hasNoSideEffects (e: Expression) =
            printfn $"hasNoSideEffects: {e}"

            match e with
            | :? Constant -> true
            | :? Dict as d -> d.Keys.IsEmpty  // Empty object
            | :? Name -> true  // E.g `void 0` is translated to Name(None)
            | _ -> false

        match stmt with
        | :? Expr as expr ->
            if hasNoSideEffects expr.Value then
                None
            else
                Some stmt
        | _ -> Some stmt

module Util =
    let transformBody (body: Statement list) =
        let body =
            body |> List.choose Helpers.isProductiveStatement

        if body.IsEmpty then
            [ Return() :> Statement ]
        else
            body

    let transformAsImports (com: IPythonCompiler) (ctx: Context) (imp: Babel.ImportDeclaration): Statement list =
        let pymodule =
            imp.Source.Value
            |> Helpers.rewriteFableImport

        printfn "Module: %A" pymodule

        let imports: ResizeArray<Alias> = ResizeArray ()
        let importFroms = ResizeArray<Alias> ()

        for expr in imp.Specifiers do
            match expr with
            | :? Babel.ImportMemberSpecifier as im ->
                printfn "ImportMemberSpecifier"
                let alias = Alias(Identifier(im.Imported.Name), if im.Imported.Name <> im.Local.Name then Identifier(im.Local.Name) |> Some else None)
                importFroms.Add(alias)
            | :? Babel.ImportDefaultSpecifier as ids ->
                printfn "ImportDefaultSpecifier"
                let alias = Alias(Identifier(pymodule), if ids.Local.Name <> pymodule then Identifier(ids.Local.Name) |> Some else None)
                imports.Add(alias)
            | :? Babel.ImportNamespaceSpecifier as ins ->
                printfn "ImportNamespaceSpecifier: %A" (ins.Local.Name, ins.Local.Name)
                let alias = Alias(Identifier(pymodule), if pymodule <> ins.Local.Name then Identifier(ins.Local.Name) |> Some else None)
                importFroms.Add(alias)
            | _ -> failwith $"Unhandled import: {expr}"

        [
            if imports.Count > 0 then
                Import(imports |> List.ofSeq)

            if importFroms.Count > 0 then
                ImportFrom(Some(Identifier(pymodule)), importFroms |> List.ofSeq)
        ]


    let transformAsClassDef (com: IPythonCompiler) ctx (cls: Babel.ClassDeclaration): Statement list =
        printfn $"transformAsClassDef"

        let bases, stmts =
            let entries =
                cls.SuperClass
                |> Option.map (fun expr -> com.TransformAsExpr(ctx, expr))

            match entries with
            | Some (expr, stmts) -> [ expr ], stmts
            | None -> [], []

        let body: Statement list =
            [ for mber in cls.Body.Body do
                match mber with
                | :? Babel.ClassMethod as cm ->
                    let self = Arg(Identifier("self"))

                    let args =
                        cm.Params
                        |> List.ofArray
                        |> List.map (fun arg -> Arg(Identifier(arg.Name)))

                    let arguments = Arguments(args = self :: args)

                    match cm.Kind with
                    | "method" ->
                        let body =
                            com.TransformAsStatements(ctx, (Some ReturnStrategy.Return), cm.Body)

                        let name =
                            match cm.Key with
                            | :? Babel.Identifier as id -> Identifier(id.Name)
                            | _ -> failwith "transformAsClassDef: Unknown key: {cm.Key}"

                        FunctionDef(name, arguments, body = body)
                    | "constructor" ->
                        let name = Identifier("__init__")

                        let body =
                            com.TransformAsStatements(ctx, (Some ReturnStrategy.NoReturn), cm.Body)

                        FunctionDef(name, arguments, body = body)
                    | _ -> failwith $"transformAsClassDef: Unknown kind: {cm.Kind}"
                | _ -> failwith $"transformAsClassDef: Unhandled class member {mber}" ]

        printfn $"Body length: {body.Length}: ${body}"
        let name = Helpers.cleanNameAsPythonIdentifier(cls.Id.Value.Name)

        [ yield! stmts
          ClassDef(Identifier(name), body = body, bases = bases) ]
    /// Transform Babel expression as Python expression
    let rec transformAsExpr
        (com: IPythonCompiler)
        (ctx: Context)
        (expr: Babel.Expression)
        : Expression * list<Statement> =
        printfn $"transformAsExpr: {expr}"

        match expr with
        | :? Babel.BinaryExpression as be ->
            let left, leftStmts = com.TransformAsExpr(ctx, be.Left)
            let right, rightStmts = com.TransformAsExpr(ctx, be.Right)

            let bin op =
                BinOp(left, op, right).AsExpr(), leftStmts @ rightStmts

            let cmp op =
                Compare(left, [ op ], [ right ]).AsExpr(), leftStmts @ rightStmts

            match be.Operator with
            | "+" -> Add() |> bin
            | "-" -> Sub() |> bin
            | "*" -> Mult() |> bin
            | "/" -> Div() |> bin
            | "%" -> Mod() |> bin
            | "**" -> Pow() |> bin
            | "<<" -> LShift() |> bin
            | ">>" -> RShift() |> bin
            | "|" -> BitOr() |> bin
            | "^" -> BitXor() |> bin
            | "&" -> BitAnd() |> bin
            | "==="
            | "==" -> Eq() |> cmp
            | "!=="
            | "!=" -> NotEq() |> cmp
            | ">" -> Gt() |> cmp
            | ">=" -> GtE() |> cmp
            | "<" -> Lt() |> cmp
            | "<=" -> LtE() |> cmp
            | _ -> failwith $"Unknown operator: {be.Operator}"

        | :? Babel.UnaryExpression as ue ->
            let op =
                match ue.Operator with
                | "-" -> USub() :> UnaryOperator |> Some
                | "+" -> UAdd() :> UnaryOperator |> Some
                | "~" -> Invert() :> UnaryOperator |> Some
                | "!" -> Not() :> UnaryOperator |> Some
                | "void" -> None
                | _ -> failwith $"Unhandled unary operator: {ue.Operator}"

            let operand, stmts = com.TransformAsExpr(ctx, ue.Argument)

            match op with
            | Some op -> UnaryOp(op, operand).AsExpr(), stmts
            | _ ->
                // TODO: Should be Contant(value=None) but we cannot create that in F#
                Name(id=Identifier("None"), ctx=Load()).AsExpr(), stmts

        | :? Babel.ArrowFunctionExpression as afe ->
            let args =
                afe.Params
                |> List.ofArray
                |> List.map (fun pattern -> Arg(Identifier pattern.Name))

            let arguments = Arguments(args = args)

            match afe.Body.Body.Length with
            | 1 ->
                let body =
                    com.TransformAsStatements(ctx, (Some ReturnStrategy.NoReturn), afe.Body)

                Lambda(arguments, body).AsExpr(), []
            | _ ->
                let body =
                    com.TransformAsStatements(ctx, (Some ReturnStrategy.Return), afe.Body)

                let name = Helpers.getIdentifier ("lifted")

                let func =
                    FunctionDef(name = name, args = arguments, body = body)

                Name(name, Load()).AsExpr(), [ func ]
        | Babel.Patterns.CallExpression(callee, args, loc)  -> // FIXME: use transformAsCall
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Call(func, args).AsExpr(), stmts @ stmtArgs
        | :? Babel.ArrayExpression as ae ->
            let elems, stmts =
                ae.Elements
                |> List.ofArray
                |> List.map (fun ex -> com.TransformAsExpr(ctx, ex))
                |> Helpers.unzipArgs

            Tuple(elems).AsExpr(), stmts
        | :? Babel.NumericLiteral as nl -> Constant(value = nl.Value).AsExpr(), []
        | :? Babel.StringLiteral as sl -> Constant(value = sl.Value).AsExpr(), []
        | Babel.Patterns.Identifier(name, _, _, _) ->
            let name =
                Helpers.cleanNameAsPythonIdentifier name

            Name(id = Identifier name, ctx = Load()).AsExpr(), []
        | :? Babel.NewExpression as ne -> // FIXME: use transformAsCall
            let func, stmts = com.TransformAsExpr(ctx, ne.Callee)

            let args, stmtArgs =
                ne.Arguments
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Call(func, args).AsExpr(), stmts @ stmtArgs
        | :? Babel.Super ->
            Name(Identifier("super().__init__"), ctx = Load())
                .AsExpr(),
            []
        | :? Babel.ObjectExpression as oe ->
            let kv =
                [ for prop in oe.Properties do
                    match prop with
                    | :? Babel.ObjectProperty as op ->
                        let key, _ = com.TransformAsExpr(ctx, op.Key)
                        let value, _ = com.TransformAsExpr(ctx, op.Value)
                        key, value
                    | _ -> failwith $"transformAsExpr: unhandled object expression property: {prop}" ]

            let keys = kv |> List.map fst
            let values = kv |> List.map snd
            Dict(keys = keys, values = values).AsExpr(), []
        | Babel.Patterns.EmitExpression(value, args, _) ->
            let args, stmts = args |> List.ofArray |> List.map (fun expr -> com.TransformAsExpr(ctx, expr)) |> Helpers.unzipArgs

            Emit(value, args).AsExpr(), stmts
        | :? Babel.MemberExpression as me ->
            let value, stmts = com.TransformAsExpr(ctx, me.Object)

            if me.Computed then
                let attr =
                    match me.Property with
                    | :? Babel.NumericLiteral as nl -> Constant(nl.Value)
                    | _ -> failwith $"transformExpressionAsStatements: unknown property {me.Property}"

                Subscript(value = value, slice = attr, ctx = Load())
                    .AsExpr(), stmts
            else
                let attr =
                    match me.Property with
                    | Babel.Patterns.Identifier(name, _, _, _) -> Identifier(name)
                    | _ -> failwith $"transformExpressionAsStatements: unknown property {me.Property}"

                let value =
                    match value with
                    | :? Name as name ->
                        let (Identifier id) = name.Id

                        // TODO: Need to make this more generic and robust
                        let id =
                            if id = "Math" then
                                //com.imports.Add("math", )
                                "math"
                            else
                                id

                        Name(id=Identifier(id), ctx=name.Context).AsExpr()
                    | _ -> value
                Attribute(value = value, attr = attr, ctx = Load())
                    .AsExpr(),
                stmts
        | :? Babel.BooleanLiteral as bl -> Constant(value = bl.Value).AsExpr(), []
        | :? Babel.FunctionExpression as fe ->
            let args =
                fe.Params
                |> List.ofArray
                |> List.map (fun pattern -> Arg(Identifier pattern.Name))

            let arguments = Arguments(args = args)

            match fe.Body.Body.Length with
            | 1 ->
                let body =
                    com.TransformAsStatements(ctx, (Some ReturnStrategy.NoReturn), fe.Body)

                Lambda(arguments, body).AsExpr(), []
            | _ ->
                let body =
                    com.TransformAsStatements(ctx, (Some ReturnStrategy.Return), fe.Body)

                let name = Helpers.getIdentifier "lifted"

                let func =
                    FunctionDef(name = name, args = arguments, body = body)

                Name(name, Load()).AsExpr(), [ func ]
        | _ -> failwith $"Unhandled value: {expr}"

    /// Transform Babel expressions as Python statements.
    let rec transformExpressionAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy option)
        (expr: Babel.Expression)
        : Statement list =
        printfn $"transformExpressionAsStatements: {expr}"

        match expr with
        | :? Babel.AssignmentExpression as ae ->
            let value, stmts = com.TransformAsExpr(ctx, ae.Right)

            let targets: Expression list =
                let attr =
                    match ae.Left with
                    | :? Babel.Identifier as identifier -> Identifier(identifier.Name)
                    | :? Babel.MemberExpression as me ->
                        match me.Property with
                        | :? Babel.Identifier as id -> Identifier(id.Name)
                        | _ -> failwith "transformExpressionAsStatements: unknown property {me.Property}"
                    | _ -> failwith $"AssignmentExpression, unknow expression: {ae.Left}"

                [ Attribute(value = Name(id = Identifier("self"), ctx = Load()), attr = attr, ctx = Store()) ]

            [ yield! stmts
              Assign(targets = targets, value = value) ]

        | _ -> failwith $"transformExpressionAsStatements: unknown expr: {expr}"

    /// Transform Babel statement as Python statements.
    let rec transformStatementAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy option)
        (stmt: Babel.Statement)
        : list<Statement> =
        printfn $"transformStatementAsStatements: {stmt}, returnStrategy: {returnStrategy}"

        match stmt with
        | :? Babel.BlockStatement as bl ->
            [ for st in bl.Body do
                yield! com.TransformAsStatements(ctx, returnStrategy, st) ]
            |> transformBody

        | :? Babel.ReturnStatement as rtn ->
            let expr, stmts = transformAsExpr com ctx rtn.Argument

            match returnStrategy with
            | Some ReturnStrategy.NoReturn -> stmts @ [ Expr(expr) ]
            | _ -> stmts @ [ Return(expr) ]
        | :? Babel.VariableDeclaration as vd ->
            [ for vd in vd.Declarations do
                let targets: Expression list =
                    [ Name(id = Identifier(vd.Id.Name), ctx = Store()) ]

                match vd.Init with
                | Some value ->
                    let expr, stmts = com.TransformAsExpr(ctx, value)
                    yield! stmts
                    Assign(targets, expr)
                | None -> () ]

        | :? Babel.ExpressionStatement as es ->
            // Handle Babel expressions that we need to transforme here as Python statements
            match es.Expression with
            | :? Babel.AssignmentExpression -> com.TransformAsStatements(ctx, returnStrategy, es.Expression)
            | _ ->
                [ let expr, stmts = com.TransformAsExpr(ctx, es.Expression)
                  yield! stmts
                  Expr(expr) ]
        | :? Babel.IfStatement as iff ->
            let test, stmts = com.TransformAsExpr(ctx, iff.Test)

            let body =
                com.TransformAsStatements(ctx, returnStrategy, iff.Consequent)

            let orElse =
                match iff.Alternate with
                | Some alt -> com.TransformAsStatements(ctx, returnStrategy, alt)
                | _ -> []

            [ yield! stmts
              If(test = test, body = body, orelse = orElse) ]
        | :? Babel.WhileStatement as ws ->
            let expr, stmts = com.TransformAsExpr(ctx, ws.Test)

            let body =
                com.TransformAsStatements(ctx, returnStrategy, ws.Body)

            [ yield! stmts
              While(test = expr, body = body, orelse = []) ]
        | _ -> failwith $"transformStatementAsStatements: Unhandled: {stmt}"

    /// Transform Babel program to Python module.
    let transformProgram (com: IPythonCompiler) ctx (body: Babel.ModuleDeclaration array): Module =
        let returnStrategy = Some ReturnStrategy.Return

        let stmt: Statement list =
            [ for md in body do
                match md with
                | :? Babel.ExportNamedDeclaration as decl ->
                    match decl.Declaration with
                    | :? Babel.VariableDeclaration as decl ->
                        for decls in decl.Declarations do
                            let value, stmts =
                                com.TransformAsExpr(ctx, decls.Init.Value)

                            let targets: Expression list =
                                [ Name(id = Identifier(decls.Id.Name), ctx = Store()) ]

                            yield! stmts
                            yield Assign(targets = targets, value = value)
                    | :? Babel.FunctionDeclaration as fd ->
                        let args =
                            fd.Params
                            |> List.ofArray
                            |> List.map (fun pattern -> Arg(Identifier pattern.Name))

                        let arguments = Arguments(args = args)

                        let body =
                            com.TransformAsStatements(ctx, returnStrategy, fd.Body)

                        let name =
                            Helpers.cleanNameAsPythonIdentifier (fd.Id.Name)

                        yield FunctionDef(Identifier(name), arguments, body = body)
                    | :? Babel.ClassDeclaration as cd -> yield! com.TransformAsClassDef(ctx, cd)
                    | _ -> failwith $"Unhandled Declaration: {decl.Declaration}"

                | :? Babel.ImportDeclaration as imp -> yield! com.TransformAsImports(ctx, imp)
                | :? Babel.PrivateModuleDeclaration as pmd ->
                    let st = pmd.Statement
                    yield! com.TransformAsStatements(ctx, returnStrategy, st)
                | _ -> failwith $"Unknown module declaration: {md}" ]
        let imports = com.GetAllImports ()
        Module(imports @ stmt)

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if String.IsNullOrEmpty selector then
            None
        else
            match selector with
            | "*"
            | "default" -> Path.GetFileNameWithoutExtension(path)
            | _ -> selector
            //|> getUniqueNameInRootScope ctx
            |> Some

module Compiler =
    open Util

    type PythonCompiler(com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IPythonCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, selector, path, r) =
                let cachedName = path + "::" + selector
                // match imports.TryGetValue(cachedName) with
                // | true, i ->
                //     match i.LocalIdent with
                //     | Some localIdent -> upcast Babel.Identifier(localIdent)
                //     | None -> upcast Babel.NullLiteral ()
                // | false, _ ->
                //     let localId = getIdentForImport ctx path selector
                //     let i =
                //       { Selector =
                //             if selector = Naming.placeholder then
                //                      "`importMember` must be assigned to a variable"
                //                      |> addError com [] r; selector
                //             else selector
                //         Path = path
                //         LocalIdent = localId }
                //     imports.Add(cachedName, i)
                //     match localId with
                //     | Some localId -> upcast Babel.Identifier(localId)
                //     | None -> upcast Babel.NullLiteral ()
                failwith "Not implemented"

            member _.GetAllImports() = imports.Values |> List.ofSeq |> List.map (fun im -> upcast im)
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e

            member bcom.TransformAsStatements(ctx, ret, e) =
                transformExpressionAsStatements bcom ctx ret e

            member bcom.TransformAsStatements(ctx, ret, e) =
                transformStatementAsStatements bcom ctx ret e

            member bcom.TransformAsClassDef(ctx, cls) = transformAsClassDef bcom ctx cls
            //member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformAsImports(ctx, imp) = transformAsImports bcom ctx imp

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.GetEntity(fullName) = com.GetEntity(fullName)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)

            member _.GetOrAddInlineExpr(fullName, generate) =
                com.GetOrAddInlineExpr(fullName, generate)

            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)

            member _.AddLog(msg, severity, ?range, ?fileName: string, ?tag: string) =
                com.AddLog(msg, severity, ?range = range, ?fileName = fileName, ?tag = tag)

    let makeCompiler com = PythonCompiler(com)

    let transformFile (com: Compiler) (program: Babel.Program) =
        let com = makeCompiler com :> IPythonCompiler

        let ctx =
            { DecisionTargets = []
              HoistVars = fun _ -> false
              TailCallOpportunity = None
              OptimizeTailCall = fun () -> ()
              ScopedTypeParams = Set.empty }

        transformProgram com ctx program.Body
