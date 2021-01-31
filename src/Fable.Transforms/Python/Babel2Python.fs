module Fable.Transforms.Babel2Python

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
    | NoBreak    // Used in switch statement blocks

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
    {
        RootScope: HashSet<string>
        DeclarationScopes: HashSet<string>
        CurrentDeclarationScope: HashSet<string>
    }

type Context =
    {
        //UsedNames: UsedNames
        DecisionTargets: (Fable.Ident list * Fable.Expr) list
        HoistVars: Fable.Ident list -> bool
        TailCallOpportunity: ITailCallOpportunity option
        OptimizeTailCall: unit -> unit
        ScopedTypeParams: Set<string>
    }

type IPythonCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Statement list
    abstract GetImportExpr: Context * selector:string * path:string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Babel.Expression -> Expression * Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.Expression -> Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.Statement -> Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.BlockStatement -> Statement list
    abstract TransformAsClassDef: Context * Babel.ClassDeclaration -> Statement list
    abstract TransformAsImports: Context * Babel.ImportDeclaration -> Statement list
    abstract TransformFunction: Context * Babel.Identifier * Babel.Pattern array * Babel.BlockStatement -> Statement

    abstract WarnOnlyOnce: string * ?range:SourceLocation -> unit


module Helpers =
    let index = (Seq.initInfinite id).GetEnumerator()

    let getIdentifier (name: string): Identifier =
        do index.MoveNext() |> ignore
        let idx = index.Current.ToString()
        Identifier($"{name}_{idx}")


    /// Replaces all '$' with '_'
    let cleanNameAsPythonIdentifier (name: string) =
        match name with
        | "this" -> "self" // TODO: Babel should use ThisExpression to avoid this hack.
        | "async" -> "asyncio"
        | _ -> name.Replace('$', '_')

    let rewriteFableImport moduleName =
        let _reFableLib =
            Regex(".*\/fable-library[\.0-9]*\/(?<module>[^\/]*)\.js", RegexOptions.Compiled)

        let m = _reFableLib.Match(moduleName)
        if m.Groups.Count > 1 then
            let pymodule = m.Groups.["module"].Value.ToLower() |> cleanNameAsPythonIdentifier

            let moduleName = String.concat "." [ "fable"; pymodule ]

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
            | Constant _ -> true
            | Dict { Keys = keys } -> keys.IsEmpty // Empty object
            | Name _ -> true // E.g `void 0` is translated to Name(None)
            | _ -> false

        match stmt with
        | Expr expr ->
            if hasNoSideEffects expr.Value then
                None
            else
                Some stmt
        | _ -> Some stmt

module Util =
    let rec transformBody (returnStrategy: ReturnStrategy) (body: Statement list) =
        let body = body |> List.choose Helpers.isProductiveStatement

        match body, returnStrategy with
        | [], ReturnStrategy.Return -> [ Return.Create() ]
        | [], ReturnStrategy.NoBreak
        | [], ReturnStrategy.NoReturn -> [ Pass ]
        | xs, ReturnStrategy.NoBreak ->
            xs
            |> List.filter (fun x -> x <> Break)
            |> transformBody ReturnStrategy.NoReturn
        | _ -> body

    let transformAsImports (com: IPythonCompiler) (ctx: Context) (imp: Babel.ImportDeclaration): Statement list =
        let pymodule = imp.Source.Value |> Helpers.rewriteFableImport

        printfn "Module: %A" pymodule

        let imports: ResizeArray<Alias> = ResizeArray()
        let importFroms = ResizeArray<Alias>()

        for expr in imp.Specifiers do
            match expr with
            | Babel.ImportMemberSpecifier(im) ->
                printfn "ImportMemberSpecifier"

                let alias =
                    Alias.Create(
                        Identifier(im.Imported.Name),
                        if im.Imported.Name <> im.Local.Name then
                            Identifier(im.Local.Name) |> Some
                        else
                            None
                    )

                importFroms.Add(alias)
            | Babel.ImportDefaultSpecifier(ids) ->
                printfn "ImportDefaultSpecifier"

                let alias =
                    Alias.Create(
                        Identifier(pymodule),
                        if ids.Local.Name <> pymodule then
                            Identifier(ids.Local.Name) |> Some
                        else
                            None
                    )

                imports.Add(alias)
            | Babel.ImportNamespaceSpecifier { Local = { Name = name } } ->
                printfn "ImportNamespaceSpecifier: %A" (name, name)

                let alias =
                    Alias.Create(
                        Identifier(pymodule),
                        if pymodule <> name then
                            Identifier(name) |> Some
                        else
                            None
                    )

                importFroms.Add(alias)
        [
            if imports.Count > 0 then
                Import.Create(imports |> List.ofSeq)

            if importFroms.Count > 0 then
                ImportFrom.Create(Some(Identifier(pymodule)), importFroms |> List.ofSeq)
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
            [
                for mber in cls.Body.Body do
                    match mber with
                    | Babel.ClassMethod(cm) ->
                        let self = Arg.Create(Identifier("self"))

                        let parms =
                            cm.Params
                            |> List.ofArray

                        let args =
                            parms
                            |> List.choose (function
                                | Babel.IdentifierPattern(id) ->
                                    Arg.Create(Identifier(id.Name)) |> Some
                                | _ -> None)

                        let varargs =
                            parms
                            |> List.choose (function
                                | Babel.RestElement(rest) ->
                                    Arg.Create(Identifier(rest.Argument.Name)) |> Some
                                | _ -> None)
                            |> List.tryHead

                        let arguments = Arguments.Create(args = self :: args, ?vararg=varargs)

                        match cm.Kind with
                        | "method" ->
                            let body =
                                com.TransformAsStatements(ctx, ReturnStrategy.Return, cm.Body |> Babel.BlockStatement)

                            let name =
                                match cm.Key with
                                | Babel.Identifier(id) -> Identifier(id.Name)
                                | _ -> failwith $"transformAsClassDef: Unknown key: {cm.Key}"

                            FunctionDef.Create(name, arguments, body = body)
                        | "constructor" ->
                            let name = Identifier("__init__")

                            let body =
                                com.TransformAsStatements(ctx, ReturnStrategy.NoReturn, cm.Body |> Babel.BlockStatement)

                            FunctionDef.Create(name, arguments, body = body)
                        | _ -> failwith $"transformAsClassDef: Unknown kind: {cm.Kind}"
                    | _ -> failwith $"transformAsClassDef: Unhandled class member {mber}"
            ]

        printfn $"Body length: {body.Length}: ${body}"
        let name = Helpers.cleanNameAsPythonIdentifier (cls.Id.Value.Name)

        [ yield! stmts; ClassDef.Create(Identifier(name), body = body, bases = bases) ]

    let transformAsFunction (com: IPythonCompiler) (ctx: Context) (name: Babel.Identifier) (parms: Babel.Pattern array) (body: Babel.BlockStatement) =
        let args =
            parms
            |> List.ofArray
            |> List.map
                (fun pattern ->
                    let name = Helpers.cleanNameAsPythonIdentifier (pattern.Name)
                    Arg.Create(Identifier(name)))

        let arguments = Arguments.Create(args = args)
        let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body |> Babel.BlockStatement)
        let name = Helpers.cleanNameAsPythonIdentifier (name.Name)

        FunctionDef.Create(Identifier(name), arguments, body = body)

    /// Transform Babel expression as Python expression
    let rec transformAsExpr
        (com: IPythonCompiler)
        (ctx: Context)
        (expr: Babel.Expression)
        : Expression * list<Statement> =
        printfn $"transformAsExpr: {expr}"

        match expr with
        | Babel.AssignmentExpression { Left=left; Operator=operator; Right=right } ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)
            match operator with
            | "=" -> NamedExpr.Create(left, right), leftStmts @ rightStmts
            | _ -> failwith $"Unsuppored assingment expression: {operator}"

        | Babel.BinaryExpression { Left=left; Operator=operator; Right=right } ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)

            let toBinOp op = BinOp.Create(left, op, right), leftStmts @ rightStmts
            let toCompare op = Compare.Create(left, [ op ], [ right ]), leftStmts @ rightStmts
            let toCall name =
                let func = Name.Create(Identifier(name), Load)
                let args = [left; right]
                Call.Create(func, args),leftStmts @ rightStmts

            match operator with
            | "+" -> Add |> toBinOp
            | "-" -> Sub |> toBinOp
            | "*" -> Mult |> toBinOp
            | "/" -> Div |> toBinOp
            | "%" -> Mod |> toBinOp
            | "**" -> Pow |> toBinOp
            | "<<" -> LShift |> toBinOp
            | ">>" -> RShift |> toBinOp
            | "|" -> BitOr |> toBinOp
            | "^" -> BitXor |> toBinOp
            | "&" -> BitAnd |> toBinOp
            | "==="
            | "==" -> Eq |> toCompare
            | "!=="
            | "!=" -> NotEq |> toCompare
            | ">" -> Gt |> toCompare
            | ">=" -> GtE |> toCompare
            | "<" -> Lt |> toCompare
            | "<=" -> LtE |> toCompare
            | "isinstance" -> toCall "isinstance"
            | _ -> failwith $"Unknown operator: {operator}"

        | Babel.UnaryExpression { Operator=operator; Argument=arg } ->
            let op =
                match operator with
                | "-" -> USub |> Some
                | "+" -> UAdd |> Some
                | "~" -> Invert |> Some
                | "!" -> Not |> Some
                | "void" -> None
                | _ -> failwith $"Unhandled unary operator: {operator}"

            let operand, stmts = com.TransformAsExpr(ctx, arg)

            match op with
            | Some op -> UnaryOp.Create(op, operand), stmts
            | _ ->
                // TODO: Should be Contant(value=None) but we cannot create that in F#
                Name.Create(id = Identifier("None"), ctx = Load), stmts

        | Babel.ArrowFunctionExpression { Params=parms; Body=body} ->
            let args =
                parms
                |> List.ofArray
                |> List.map (fun pattern -> Arg.Create(Identifier pattern.Name))

            let arguments =
                let args =
                    match args with
                    | [] -> [ Arg.Create(Identifier("_"), Name.Create(Identifier("None"), Load)) ] // Need to receive unit
                    | _ -> args
                Arguments.Create(args = args)

            let stmts = body.Body // TODO: Babel AST should be fixed. Body does not have to be a BlockStatement.
            match stmts with
            | [| Babel.ReturnStatement(rtn) |] ->
                let body, stmts = com.TransformAsExpr(ctx, rtn.Argument)
                Lambda.Create(arguments, body), stmts
            | _ ->
                let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                let name = Helpers.getIdentifier "lifted"

                let func =
                    FunctionDef.Create(name = name, args = arguments, body = body)

                Name.Create(name, Load), [ func ]
        | Babel.CallExpression { Callee=callee; Arguments=args } -> // FIXME: use transformAsCall
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Call.Create(func, args), stmts @ stmtArgs
        | Babel.ArrayExpression(ae) ->
            let elems, stmts =
                ae.Elements
                |> List.ofArray
                |> List.map (fun ex -> com.TransformAsExpr(ctx, ex))
                |> Helpers.unzipArgs

            Tuple.Create(elems), stmts
        | Babel.Literal(Babel.NumericLiteral(nl)) -> Constant.Create(value = nl.Value), []
        | Babel.Literal(Babel.StringLiteral(sl)) -> Constant.Create(value = sl.Value), []
        | Babel.Identifier { Name=name } ->
            let name = Helpers.cleanNameAsPythonIdentifier name
            Name.Create(id = Identifier name, ctx = Load), []
        | Babel.NewExpression { Callee=callee; Arguments=args} -> // FIXME: use transformAsCall
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Call.Create(func, args), stmts @ stmtArgs
        | Babel.Super(se) -> Name.Create(Identifier("super().__init__"), ctx = Load), []
        | Babel.ObjectExpression { Properties=properties } ->
            let keys, values, stmts =
                [
                    for prop in properties do
                        match prop with
                        | Babel.ObjectProperty(op) ->
                            let key, stmts1 = com.TransformAsExpr(ctx, op.Key)
                            let value, stmts2 = com.TransformAsExpr(ctx, op.Value)
                            key, value, stmts1 @ stmts2
                        | Babel.ObjectMethod(om) ->
                            let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, om.Body)
                            let key, stmts = com.TransformAsExpr(ctx, om.Key)
                            let args =
                                om.Params
                                |> List.ofArray
                                |> List.map (fun pattern -> Arg.Create(Identifier pattern.Name))

                            let arguments = Arguments.Create(args = args)
                            let name = Helpers.getIdentifier "lifted"

                            let func =
                                FunctionDef.Create(name = name, args = arguments, body = body)
                            key, Name.Create(name, Load), stmts @ [func]

                ]
                |> List.unzip3

            Dict.Create(keys = keys, values = values), stmts |> List.collect id
        | Babel.EmitExpression { Value=value; Args=args } ->
            let args, stmts =
                args
                |> List.ofArray
                |> List.map (fun expr -> com.TransformAsExpr(ctx, expr))
                |> Helpers.unzipArgs

            match value with
            | "void $0" -> args.[0], stmts
            //| "raise %0" -> Raise.Create()
            | _ -> Emit.Create(value, args), stmts
        | Babel.MemberExpression { Computed=true; Object=object; Property=Babel.Literal(literal) } ->
            let value, stmts = com.TransformAsExpr(ctx, object)

            let attr =
                match literal with
                | Babel.NumericLiteral(nl) -> Constant.Create(nl.Value)
                | Babel.StringLiteral(literal) -> Constant.Create(literal.Value)
                | _ -> failwith $"transformExpressionAsStatements: unknown literal {literal}"

            Subscript.Create(value = value, slice = attr, ctx = Load), stmts
         | Babel.MemberExpression { Computed=false; Object=object; Property=Babel.Identifier { Name="length" } } ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Name.Create(Identifier "len", Load)
            Call.Create(func, [value]), stmts
         | Babel.MemberExpression { Computed=false; Object=object; Property=Babel.Identifier { Name="message" } } ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Name.Create(Identifier "str", Load)
            Call.Create(func, [value]), stmts
         | Babel.MemberExpression { Computed=false; Object=object; Property=property } ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let attr =
                match property with
                | Babel.Identifier { Name=name } -> Identifier(name)
                | _ -> failwith $"transformExpressionAsStatements: unknown property {property}"

            let value =
                match value with
                | Name { Id = Identifier (id); Context = ctx } ->
                    // TODO: Need to make this more generic and robust
                    let id =
                        if id = "Math" then
                            //com.imports.Add("math", )
                            "math"
                        else
                            id

                    Name.Create(id = Identifier(id), ctx = ctx)
                | _ -> value

            Attribute.Create(value = value, attr = attr, ctx = Load), stmts
        | Babel.Literal(Babel.BooleanLiteral { Value=value }) -> Constant.Create(value = value), []
        | Babel.FunctionExpression(fe) ->
            let args =
                fe.Params
                |> List.ofArray
                |> List.map (fun pattern -> Arg.Create(Identifier pattern.Name))

            let arguments = Arguments.Create(args = args)

            match fe.Body.Body with
            | [| Babel.ExpressionStatement(expr) |]  ->
                let body, stmts = com.TransformAsExpr(ctx, expr.Expression)
                Lambda.Create(arguments, body), stmts
            | _ ->
                let body =
                    com.TransformAsStatements(ctx, ReturnStrategy.Return, fe.Body)

                let name = Helpers.getIdentifier "lifted"

                let func =
                    FunctionDef.Create(name = name, args = arguments, body = body)

                Name.Create(name, Load), [ func ]
        | Babel.ConditionalExpression { Test=test; Consequent=consequent; Alternate=alternate } ->
            let test, stmts1 = com.TransformAsExpr(ctx, test)
            let body, stmts2 = com.TransformAsExpr(ctx, consequent)
            let orElse, stmts3 = com.TransformAsExpr(ctx, alternate)

            IfExp.Create(test, body, orElse), stmts1 @ stmts2 @ stmts3
        | Babel.Literal(Babel.NullLiteral(nl)) -> Name.Create(Identifier("None"), ctx = Load), []
        | Babel.SequenceExpression { Expressions=exprs } ->
            // Sequence expressions are tricky. We currently convert them to a function that we call w/zero arguments
            let exprs, stmts =
                exprs
                |> List.ofArray
                |> List.map (fun ex -> com.TransformAsExpr(ctx, ex))
                |> Helpers.unzipArgs

            let body =
                exprs
                |> List.mapi
                    (fun i n ->
                        if i = exprs.Length - 1 then
                            Return.Create(n) // Return the last statement
                        else
                            Expr.Create(n))

            let name = Helpers.getIdentifier ("lifted")

            let func =
                FunctionDef.Create(name = name, args = Arguments.Create [], body = body)

            let name = Name.Create(name, Load)
            Call.Create(name), stmts @ [ func ]
        | _ -> failwith $"transformAsExpr: Unhandled value: {expr}"

    /// Transform Babel expressions as Python statements.
    let rec transformExpressionAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (expr: Babel.Expression)
        : Statement list =

        printfn $"transformExpressionAsStatements: {expr}"

        match expr with
        | Babel.AssignmentExpression { Left=left; Right=right } ->
            let value, stmts = com.TransformAsExpr(ctx, right)

            let targets: Expression list =
                match left with
                | Babel.Identifier(identifier) ->
                    let target =
                        Identifier(Helpers.cleanNameAsPythonIdentifier (identifier.Name))

                    [ Name.Create(id = target, ctx = Store) ]
                | Babel.MemberExpression({Property=property}) ->
                    match property with
                    | Babel.Identifier(id) ->
                        let attr = Identifier(Helpers.cleanNameAsPythonIdentifier (id.Name))
                        [
                            Attribute.Create(
                                value = Name.Create(id = Identifier("self"), ctx = Load),
                                attr = attr,
                                ctx = Store
                            )
                        ]
                    | _ -> failwith $"transformExpressionAsStatements: unknown property {property}"
                | _ -> failwith $"AssignmentExpression, unknown expression: {left}"
            [ yield! stmts; Assign.Create(targets = targets, value = value) ]
        | _ -> failwith $"transformExpressionAsStatements: unknown expr: {expr}"

    /// Transform Babel statement as Python statements.
    let rec transformStatementAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (stmt: Babel.Statement)
        : list<Statement> =
        printfn $"transformStatementAsStatements: {stmt}, returnStrategy: {returnStrategy}"

        match stmt with
        | Babel.BlockStatement(bs) ->
            [ yield! com.TransformAsStatements(ctx, returnStrategy, bs) ]
            |> transformBody returnStrategy

        | Babel.ReturnStatement { Argument=arg } ->
            let expr, stmts = transformAsExpr com ctx arg

            match returnStrategy with
            | ReturnStrategy.NoReturn -> stmts @ [ Expr.Create(expr) ]
            | _ -> stmts @ [ Return.Create(expr) ]
        | Babel.Declaration(Babel.VariableDeclaration(vd)) ->
            [
                for vd in vd.Declarations do
                    let targets: Expression list =
                        let name = Helpers.cleanNameAsPythonIdentifier (vd.Id.Name)
                        [ Name.Create(id = Identifier(name), ctx = Store) ]

                    match vd.Init with
                    | Some value ->
                        let expr, stmts = com.TransformAsExpr(ctx, value)
                        yield! stmts
                        Assign.Create(targets, expr)
                    | None -> ()
            ]
        | Babel.ExpressionStatement { Expression=expression } ->
            // Handle Babel expressions that we need to transforme here as Python statements
            match expression with
            | Babel.AssignmentExpression(ae) -> com.TransformAsStatements(ctx, returnStrategy, ae |> Babel.AssignmentExpression)
            | _ ->
                [
                    let expr, stmts = com.TransformAsExpr(ctx, expression)
                    yield! stmts
                    Expr.Create(expr)
                ]
        | Babel.IfStatement { Test=test; Consequent=consequent; Alternate=alternate } ->
            let test, stmts = com.TransformAsExpr(ctx, test)

            let body =
                com.TransformAsStatements(ctx, returnStrategy, consequent)
                |> transformBody ReturnStrategy.NoReturn

            let orElse =
                match alternate with
                | Some alt ->
                    com.TransformAsStatements(ctx, returnStrategy, alt)
                    |> transformBody ReturnStrategy.NoReturn

                | _ -> []

            [ yield! stmts; If.Create(test = test, body = body, orelse = orElse) ]
        | Babel.WhileStatement { Test=test; Body=body } ->
            let expr, stmts = com.TransformAsExpr(ctx, test)

            let body =
                com.TransformAsStatements(ctx, returnStrategy, body)
                |> transformBody ReturnStrategy.NoReturn

            [ yield! stmts; While.Create(test = expr, body = body, orelse = []) ]
        | Babel.TryStatement(ts) ->
            let body = com.TransformAsStatements(ctx, returnStrategy, ts.Block)

            let finalBody =
                ts.Finalizer
                |> Option.map (fun f -> com.TransformAsStatements(ctx, returnStrategy, f))

            let handlers =
                match ts.Handler with
                | Some cc ->
                    let body = com.TransformAsStatements(ctx, returnStrategy, cc.Body)

                    let exn =
                        Name.Create(Identifier("Exception"), ctx = Load)
                        |> Some

                    // Insert a ex.message = str(ex) for all aliased exceptions.
                    let identifier = Identifier(cc.Param.Name)
                    // let idName = Name.Create(identifier, Load)
                    // let message = Identifier("message")
                    // let trg = Attribute.Create(idName, message, Store)
                    // let value = Call.Create(Name.Create(Identifier("str"), Load), [idName])
                    // let msg = Assign.Create([trg], value)
                    // let body =  msg :: body
                    let handlers =
                        [ ExceptHandler.Create(``type`` = exn, name = identifier, body = body) ]

                    handlers
                | _ -> []

            [ Try.AsStatement(body = body, handlers = handlers, ?finalBody = finalBody) ]
        | Babel.SwitchStatement { Discriminant=discriminant; Cases=cases } ->
            let value, stmts = com.TransformAsExpr(ctx, discriminant)

            let rec ifThenElse (fallThrough: Expression option) (cases: Babel.SwitchCase list): Statement list option =
                match cases with
                | [] -> None
                | case :: cases ->
                    let body =
                        case.Consequent
                        |> List.ofArray
                        |> List.collect (fun x -> com.TransformAsStatements(ctx, ReturnStrategy.NoBreak, x))

                    match case.Test with
                    | None -> body |> Some
                    | Some test ->
                        let test, st = com.TransformAsExpr(ctx, test)

                        let expr =
                            Compare.Create(left = value, ops = [ Eq ], comparators = [ test ])

                        let test =
                            match fallThrough with
                            | Some ft -> BoolOp.Create(op = Or, values = [ ft; expr ])
                            | _ -> expr
                        // Check for fallthrough
                        if body.IsEmpty then
                            ifThenElse (Some test) cases
                        else
                            [ If.Create(test = test, body = body, ?orelse = ifThenElse None cases) ]
                            |> Some

            let result = cases |> List.ofArray |> ifThenElse None
            match result with
            | Some ifStmt -> stmts @ ifStmt
            | None -> []
        | Babel.BreakStatement(_) -> [ Break ]
        | Babel.Declaration(Babel.FunctionDeclaration(fd)) ->
            [ com.TransformFunction(ctx, fd.Id, fd.Params, fd.Body) ]
        | Babel.Declaration(Babel.ClassDeclaration(cd)) -> com.TransformAsClassDef(ctx, cd)
        | Babel.ForStatement
            { Init=Some({ Declarations= [| { Id=id; Init=Some(init)} |] })
              Test=Some(Babel.BinaryExpression { Left=left; Right=right; Operator="<="})
              Body=body } ->
            let body = com.TransformAsStatements(ctx, ReturnStrategy.NoReturn, body)
            let target = Name.Create(Identifier id.Name, Load)
            let start, stmts1 = com.TransformAsExpr(ctx, init)
            let stop, stmts2 = com.TransformAsExpr(ctx, right)
            let stop = BinOp.Create(stop, Add, Constant.Create(1)) // Python `range` has exclusive end.
            let iter = Call.Create(Name.Create(Identifier "range", Load), args=[start; stop])
            stmts1 @ stmts2 @ [ For.AsStatement(target=target, iter=iter, body=body) ]
        | Babel.LabeledStatement { Body=body } -> com.TransformAsStatements(ctx, returnStrategy, body)
        | Babel.ContinueStatement(_) -> [ Continue ]
        | _ -> failwith $"transformStatementAsStatements: Unhandled: {stmt}"

    let transformBlockStatementAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (block: Babel.BlockStatement)
        : list<Statement> =

        [ for stmt in block.Body do
            yield! transformStatementAsStatements com ctx returnStrategy stmt ]

    /// Transform Babel program to Python module.
    let transformProgram (com: IPythonCompiler) ctx (body: Babel.ModuleDeclaration array): Module =
        let returnStrategy = ReturnStrategy.Return

        let stmt: Statement list =
            [
                for md in body do
                    match md with
                    | Babel.ExportNamedDeclaration(decl) ->
                        match decl.Declaration with
                        | Babel.VariableDeclaration(decl) ->
                            for decls in decl.Declarations do
                                let value, stmts = com.TransformAsExpr(ctx, decls.Init.Value)

                                let targets: Expression list =
                                    let name = Helpers.cleanNameAsPythonIdentifier (decls.Id.Name)
                                    [ Name.Create(id = Identifier(name), ctx = Store) ]

                                yield! stmts
                                yield Assign.Create(targets = targets, value = value)
                        | Babel.FunctionDeclaration(fd) ->
                            yield com.TransformFunction(ctx, fd.Id, fd.Params, fd.Body)

                        | Babel.ClassDeclaration(cd) -> yield! com.TransformAsClassDef(ctx, cd)
                        | _ -> failwith $"Unhandled Declaration: {decl.Declaration}"

                    | Babel.ImportDeclaration(imp) -> yield! com.TransformAsImports(ctx, imp)
                    | Babel.PrivateModuleDeclaration(pmd) ->
                        yield!
                            com.TransformAsStatements(ctx, returnStrategy, pmd.Statement)
                            |> transformBody returnStrategy
                    | _ -> failwith $"Unknown module declaration: {md}"
            ]

        let imports = com.GetAllImports()
        Module.Create(imports @ stmt)

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

    type PythonCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IPythonCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, selector, path, r) = failwith "Not implemented"
            member _.GetAllImports() = imports.Values |> List.ofSeq |> List.map Import

            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformExpressionAsStatements bcom ctx ret e
            member bcom.TransformAsStatements(ctx, ret, e) = transformStatementAsStatements bcom ctx ret e
            member bcom.TransformAsStatements(ctx, ret, e) = transformBlockStatementAsStatements bcom ctx ret e
            member bcom.TransformAsClassDef(ctx, cls) = transformAsClassDef bcom ctx cls
            member bcom.TransformFunction(ctx, name, args, body) = transformAsFunction bcom ctx name args body
            member bcom.TransformAsImports(ctx, imp) = transformAsImports bcom ctx imp

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.GetEntity(fullName) = com.GetEntity(fullName)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName: string, ?tag: string) =
                com.AddLog(msg, severity, ?range = range, ?fileName = fileName, ?tag = tag)

    let makeCompiler com = PythonCompiler(com)

    let transformFile (com: Compiler) (program: Babel.Program) =
        let com = makeCompiler com :> IPythonCompiler

        let ctx =
            {
                DecisionTargets = []
                HoistVars = fun _ -> false
                TailCallOpportunity = None
                OptimizeTailCall = fun () -> ()
                ScopedTypeParams = Set.empty
            }

        transformProgram com ctx program.Body
