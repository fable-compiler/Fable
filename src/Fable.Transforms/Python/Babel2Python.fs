module rec Fable.Transforms.Babel2Python

open System
open System.Collections.Generic

open Fable
open Fable.AST
open Fable.AST.Python

[<RequireQualifiedAccess>]
type ReturnStrategy =
    | Return
    | ReturnUnit
    | Assign of Expression
    | Target of Identifier

type Import =
    { Selector: string
      LocalIdent: string option
      Path: string }

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
    //abstract GetAllImports: unit -> seq<Python.Import>
    //abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Babel.Expression -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Babel.Statement -> Statement list
    abstract TransformAsStatements: Context * ReturnStrategy option * Babel.Expression -> Statement list
    abstract TransformAsClassDef: Context * Babel.ClassDeclaration -> Statement list
    //abstract TransformImport: Context * selector:string * path:string -> Expression
    //abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement

    abstract WarnOnlyOnce: string * ?range:SourceLocation -> unit

module Util =
    let transformAsClassDef (com: IPythonCompiler) ctx (cls: Babel.ClassDeclaration) : Statement list=
        let body : Statement list = [
            for mber in cls.Body.Body do
                match mber with
                | :? Babel.ClassMethod as cm ->
                    match cm.Kind with
                    | "constructor" ->
                        let args = cm.Params |> List.ofArray |> List.map (fun arg -> Arg(Identifier arg.Name))
                        let arguments = Arguments(args=args)
                        let body = com.TransformAsStatements(ctx, (Some ReturnStrategy.ReturnUnit), cm.Body)
                        FunctionDef(Identifier "__init__", arguments, body=body)
                    | _ -> failwith $"Unknown kind: {cm.Kind}"
                | _ -> failwith $"Unhandled class member {mber}"
                //yield! com.TransformAsStatements(ctx, (Some ReturnStrategy.ReturnUnit), stmt)
                ]
        //yield ClassDef(Identifier cd.Id.Value.Name, body=body)
        body

    /// Transform Babel expression as Python expression
    let rec transformAsExpr (com: IPythonCompiler) ctx (expr: Babel.Expression): Expression =
        match expr with
        | :? Babel.BinaryExpression as bin ->
            let left = com.TransformAsExpr(ctx, bin.Left)
            let right = com.TransformAsExpr(ctx, bin.Right)

            let op: Operator =
                match bin.Operator with
                | "+" -> Add() :> _
                | "-" -> Sub() :> _
                | "*" -> Mult() :> _
                | "/" -> Div() :> _
                | "%" -> Mod() :> _
                | "**" -> Pow() :> _
                | "<<" -> LShift() :> _
                | ">>" -> RShift() :> _
                | "|" -> BitOr() :> _
                | "^" -> BitXor() :> _
                | "&" -> BitAnd() :> _
                | _ -> failwith $"Unknown operator: {bin.Operator}"

            BinOp(left, op, right).AsExpr()
        | :? Babel.UnaryExpression as ue ->
            let op =
                match ue.Operator with
                | "-" -> USub() :> UnaryOperator |> Some
                | "+" -> UAdd() :> UnaryOperator |> Some
                | "~" -> Invert() :> UnaryOperator |> Some
                | "!" -> Not() :> UnaryOperator |> Some
                | "void" -> None
                | _ -> failwith $"Unhandled unary operator: {ue.Operator}"

            let operand = com.TransformAsExpr(ctx, ue.Argument)
            match op with
            | Some op -> UnaryOp(op, operand).AsExpr()
            | _ -> operand

        | :? Babel.ArrowFunctionExpression as afe ->
            let args = afe.Params |> List.ofArray
                            |> List.map (fun pattern -> Arg(Identifier pattern.Name))
            let arguments = Arguments(args = args)
            let body = com.TransformAsStatements(ctx, (Some ReturnStrategy.ReturnUnit), afe.Body)
            Lambda(arguments, body).AsExpr()
        | :? Babel.CallExpression as ce ->  // FIXME: use transformAsCall
            let func = com.TransformAsExpr(ctx, ce.Callee)
            let args = ce.Arguments |> List.ofArray |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
            Call(func, args).AsExpr()
        | :? Babel.ArrayExpression as ae ->
            let elems = ae.Elements |> List.ofArray |> List.map (fun ex -> com.TransformAsExpr(ctx, ex))
            Tuple(elems).AsExpr()
        | :? Babel.NumericLiteral as nl -> Constant(value = nl.Value).AsExpr()
        | :? Babel.StringLiteral as sl -> Constant(value = sl.Value).AsExpr()
        | :? Babel.Identifier as ident -> Name(id = Identifier ident.Name, ctx = Load()).AsExpr()
        | :? Babel.NewExpression as ne -> // FIXME: use transformAsCall
            let func = com.TransformAsExpr(ctx, ne.Callee)
            let args = ne.Arguments |> List.ofArray |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
            Call(func, args).AsExpr()
        | :? Babel.Super ->
            Name(Identifier("super().__init__"), ctx = Load()).AsExpr()
        // | :? Babel.ObjectExpression as oe ->
        //     oe.Properties
        | a -> failwith $"Unhandled value: {a}"

    /// Transform Babel expressions as Python statements.
    let rec transformExpressionAsStatements (com: IPythonCompiler) ctx (returnStrategy: ReturnStrategy option) (expr: Babel.Expression): Statement list =
        match expr with
        | :? Babel.AssignmentExpression as ae ->
            let value = com.TransformAsExpr(ctx, ae.Right)
            let targets: Expression list =
                let attr =
                    match ae.Left with
                    | :? Babel.Identifier as identifier -> Identifier(identifier.Name)
                    | :? Babel.MemberExpression as me ->
                        match me.Property with
                        | :? Babel.Identifier as id -> Identifier(id.Name)
                        | _ -> failwith "transformExpressionAsStatements: unknown property {me.Property}"
                    | _ -> failwith $"AssignmentExpression, unknow expression: {ae.Left}"
                [ Attribute(value=Name(id=Identifier("self"), ctx=Load()), attr=attr, ctx = Store()) ]

            [ Assign(targets = targets, value = value) ]

        | _ -> failwith $"transformExpressionAsStatements: unknown expr: {expr}"

    /// Transform Babel statement as Python statements.
    let rec transformStatementAsStatements (com: IPythonCompiler) ctx (returnStrategy: ReturnStrategy option) (stmt: Babel.Statement): Statement list =
        match stmt with
        | :? Babel.BlockStatement as bl ->
            [ for st in bl.Body do
                yield! com.TransformAsStatements(ctx, returnStrategy, st) ]
        | :? Babel.ReturnStatement as rtn ->
            let expr = transformAsExpr com ctx rtn.Argument
            [ Return expr ]
        | :? Babel.VariableDeclaration as vd ->
            [ for vd in vd.Declarations do
                let targets: Expression list =
                    [ Name(id = Identifier(vd.Id.Name), ctx = Store()) ]

                match vd.Init with
                | Some value -> Assign(targets, com.TransformAsExpr(ctx, value))
                | None -> () ]

        | :? Babel.ExpressionStatement as es ->
            // Handle Babel expressions that needs to be transformed as Python statements
            match es.Expression with
            | :? Babel.AssignmentExpression ->
                com.TransformAsStatements(ctx, returnStrategy, es.Expression)
            | _ ->
                [ Expr(com.TransformAsExpr(ctx, es.Expression)) ]

        | _ -> failwith $"transformStatementAsStatements: Unhandled stmt: {stmt}"

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
                            let value = com.TransformAsExpr(ctx, decls.Init.Value)
                            let targets: Expression list =
                                [ Name(id = Identifier(decls.Id.Name), ctx = Store()) ]

                            yield Assign(targets = targets, value = value)
                    | :? Babel.FunctionDeclaration as fd ->
                        let args =
                            fd.Params
                            |> List.ofArray
                            |> List.map (fun pattern -> Arg(Identifier pattern.Name))

                        let arguments = Arguments(args = args)

                        let body =
                            com.TransformAsStatements(ctx, returnStrategy, fd.Body)

                        yield FunctionDef(Identifier fd.Id.Name, arguments, body = body)
                    | :? Babel.ClassDeclaration as cd ->
                        yield! com.TransformAsClassDef(ctx, cd)
                    | _ -> failwith $"Unhandled Declaration: {decl.Declaration}"

                | :? Babel.ImportDeclaration as imp ->
                    let source = imp.Source.Value |> Identifier |> Some
                    let mapper (expr: Babel.ImportSpecifier) =
                        match expr with
                        | :? Babel.ImportMemberSpecifier as im ->
                            let a = im.Imported.Name
                            Alias(Identifier a, None)
                        | _ -> failwith $"Unhandled import: {expr}"

                    let sa = imp.Specifiers |> List.ofArray |> List.map mapper
                    yield ImportFrom(source, sa) :> _
                | :? Babel.PrivateModuleDeclaration as pmd ->
                    let st = pmd.Statement
                    yield! com.TransformAsStatements(ctx, returnStrategy, st)
                | _ ->
                    failwith $"Unknown module declaration: {md}" ]

        Module(stmt)

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

            // member _.GetImportExpr(ctx, selector, path, r) =
            //     let cachedName = path + "::" + selector
            //     match imports.TryGetValue(cachedName) with
            //     | true, i ->
            //         match i.LocalIdent with
            //         | Some localIdent -> upcast Babel.Identifier(localIdent)
            //         | None -> upcast Babel.NullLiteral ()
            //     | false, _ ->
            //         let localId = getIdentForImport ctx path selector
            //         let i =
            //           { Selector =
            //                 if selector = Naming.placeholder then
            //                          "`importMember` must be assigned to a variable"
            //                          |> addError com [] r; selector
            //                 else selector
            //             Path = path
            //             LocalIdent = localId }
            //         imports.Add(cachedName, i)
            //         match localId with
            //         | Some localId -> upcast Babel.Identifier(localId)
            //         | None -> upcast Babel.NullLiteral ()
            //member _.GetAllImports() = upcast imports.Values
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformExpressionAsStatements bcom ctx ret e
            member bcom.TransformAsStatements(ctx, ret, e) = transformStatementAsStatements bcom ctx ret e
            member bcom.TransformAsClassDef(ctx, cls) = transformAsClassDef bcom ctx cls
        //member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
        //member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None selector path

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
