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
    //abstract TransformAsStatements: Context * ReturnStrategy option * Babel.Statement -> Python.Statement list
    //abstract TransformImport: Context * selector:string * path:string -> Expression
    //abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement

    abstract WarnOnlyOnce: string * ?range:SourceLocation -> unit

module Util =
    let rec transformAsExpr (com: IPythonCompiler) ctx (expr: Babel.Expression): Expression =
        printfn "transformAsExpr: %A" expr

        match expr with
        | :? Babel.BinaryExpression as bin ->
            let left = transformAsExpr com ctx bin.Left
            let right = transformAsExpr com ctx bin.Right

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

            BinOp(left, op, right) :> _
        | :? Babel.UnaryExpression as ue ->
            let op: UnaryOperator =
                match ue.Operator with
                | "-" -> USub() :> _
                | "+" -> UAdd() :> _
                | "~" -> Invert() :> _
                | "!" -> Not() :> _
                | a -> failwith $"Unhandled unary operator: {a}"

            let operand = transformAsExpr com ctx ue.Argument
            UnaryOp(op, operand) :> _
        | :? Babel.ArrowFunctionExpression as afe ->
            let args = afe.Params |> List.ofArray
                            |> List.map (fun pattern -> Arg(Identifier pattern.Name))
            let arguments = Arguments(args = args)
            let body = transformAsStatements com ctx ReturnStrategy.Return afe.Body
            Lambda(arguments, body) :> _
        | :? Babel.CallExpression as ce ->
            let func = transformAsExpr com ctx ce.Callee
            let args = ce.Arguments |> List.ofArray |> List.map (fun arg -> transformAsExpr com ctx arg)
            Call(func, args) :> _
        | :? Babel.NumericLiteral as nl -> Constant(value = nl.Value) :> _
        | :? Babel.StringLiteral as sl -> Constant(value = sl.Value) :> _
        | :? Babel.Identifier as ident -> Name(id = Identifier ident.Name, ctx = Load()) :> _
        | a -> failwith $"Unhandled value: {a}"

    let transformProgram (com: IPythonCompiler) ctx (body: Babel.ModuleDeclaration array): Module =
        let stmt: Statement list =
            [ for md in body do
                match md with
                | :? Babel.ExportNamedDeclaration as decl ->
                    match decl.Declaration with
                    | :? Babel.VariableDeclaration as decl ->
                        for decls in decl.Declarations do
                            let value: Expression = transformAsExpr com ctx decls.Init.Value

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
                            transformAsStatements com ctx ReturnStrategy.Return fd.Body

                        yield FunctionDef(Identifier fd.Id.Name, arguments, body = body)
                    | a -> printfn "Declaration: %A" a
                | :? Babel.ImportDeclaration as imp ->
                    let source = imp.Source.Value |> Identifier |> Some
                    let mapper (expr: Babel.ImportSpecifier) =
                        match expr with
                        | :? Babel.ImportMemberSpecifier as im ->
                            let a = im.Imported.Name
                            Alias(Identifier a, None)

                    let sa = imp.Specifiers |> List.ofArray |> List.map mapper
                    yield ImportFrom(source, sa) :> _
                | a ->
                    failwith "Unknown module declaration: %A" a ]

        Module(stmt)

    let rec transformAsStatements (com: IPythonCompiler) ctx returnStrategy (expr: Babel.Statement): Statement list =
        match expr with
        | :? Babel.BlockStatement as bl ->
            [ for stmt in bl.Body do
                yield! transformAsStatements com ctx returnStrategy stmt ]
        | :? Babel.ReturnStatement as rtn ->
            let expr = transformAsExpr com ctx rtn.Argument
            [ Return expr ]
        | :? Babel.VariableDeclaration as vd ->
            [ for vd in vd.Declarations do
                let targets: Expression list =
                    [ Name(id = Identifier(vd.Id.Name), ctx = Store()) ]

                match vd.Init with
                | Some value -> Assign(targets, transformAsExpr com ctx value)
                | None -> () ]
        | a -> failwith $"Unhandled statement: {a}"

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then
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
        //member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
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

        let declScopes =
            let hs = HashSet()
            //for decl in file.Body .Declarations do
            //    hs.UnionWith(decl.UsedNames)
            hs

        let ctx =
            { DecisionTargets = []
              HoistVars = fun _ -> false
              TailCallOpportunity = None
              OptimizeTailCall = fun () -> ()
              ScopedTypeParams = Set.empty }

        let body = transformProgram com ctx program.Body
        printfn "Body: %A" body
        body
