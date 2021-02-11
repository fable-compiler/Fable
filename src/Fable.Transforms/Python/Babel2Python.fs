module Fable.Transforms.Babel2Python

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Python
open Fable.AST.Babel

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
    abstract GetAllImports: unit -> Python.Statement list
    abstract GetImportExpr: Context * selector:string * path:string * SourceLocation option -> Python.Expression
    abstract TransformAsExpr: Context * Babel.Expression -> Python.Expression * Python.Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.Expression -> Python.Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.Statement -> Python.Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.BlockStatement -> Python.Statement list
    abstract TransformAsClassDef: Context * Babel.ClassBody * Babel.Identifier option * Babel.Expression option * Babel.ClassImplements array option * Babel.TypeParameterInstantiation option * Babel.TypeParameterDeclaration option * SourceLocation option -> Python.Statement list
    abstract TransformAsImports: Context * Babel.ImportSpecifier array * Babel.StringLiteral -> Python.Statement list
    abstract TransformFunction: Context * Babel.Identifier * Babel.Pattern array * Babel.BlockStatement -> Python.Statement

    abstract WarnOnlyOnce: string * ?range:SourceLocation -> unit


module Helpers =
    let index = (Seq.initInfinite id).GetEnumerator()

    let getIdentifier (name: string): Python.Identifier =
        do index.MoveNext() |> ignore
        let idx = index.Current.ToString()
        Python.Identifier($"{name}_{idx}")


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

    let unzipArgs (args: (Python.Expression * Python.Statement list) list): Python.Expression list * Python.Statement list =
        let stmts = args |> List.map snd |> List.collect id
        let args = args |> List.map fst
        args, stmts

    /// A few statements in the generated Babel AST do not produce any effect, and will not be printet. But they are
    /// left in the AST and we need to skip them since they are not valid for Python (either).
    let isProductiveStatement (stmt: Python.Statement) =
        let rec hasNoSideEffects (e: Python.Expression) =
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
    let rec transformBody (returnStrategy: ReturnStrategy) (body: Python.Statement list) =
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

    let transformAsImports (com: IPythonCompiler) (ctx: Context) (specifiers: Babel.ImportSpecifier array) (source: Babel.StringLiteral) : Python.Statement list =
        let (StringLiteral(value=value)) = source
        let pymodule = value |> Helpers.rewriteFableImport

        printfn "Module: %A" pymodule

        let imports: ResizeArray<Alias> = ResizeArray()
        let importFroms = ResizeArray<Alias>()

        for expr in specifiers do
            match expr with
            | Babel.ImportMemberSpecifier(local, imported) ->
                printfn "ImportMemberSpecifier"

                let alias =
                    Alias.Create(
                        Python.Identifier(imported.Name),
                        if imported.Name <> local.Name then
                            Python.Identifier(local.Name) |> Some
                        else
                            None
                    )

                importFroms.Add(alias)
            | Babel.ImportDefaultSpecifier(local) ->
                printfn "ImportDefaultSpecifier"

                let alias =
                    Alias.Create(
                        Python.Identifier(pymodule),
                        if local.Name <> pymodule then
                            Python.Identifier(local.Name) |> Some
                        else
                            None
                    )

                imports.Add(alias)
            | Babel.ImportNamespaceSpecifier(Identifier(name = name)) ->
                printfn "ImportNamespaceSpecifier: %A" (name, name)

                let alias =
                    Alias.Create(
                        Python.Identifier(pymodule),
                        if pymodule <> name then
                            Python.Identifier(name) |> Some
                        else
                            None
                    )

                importFroms.Add(alias)
        [
            if imports.Count > 0 then
                Import.Create(imports |> List.ofSeq)

            if importFroms.Count > 0 then
                ImportFrom.Create(Some(Python.Identifier(pymodule)), importFroms |> List.ofSeq)
        ]


    let transformAsClassDef
        (com: IPythonCompiler)
        (ctx: Context)
        (body: Babel.ClassBody)
        (id: Babel.Identifier option)
        (superClass: Babel.Expression option)
        (implements: Babel.ClassImplements array option)
        (superTypeParameters: Babel.TypeParameterInstantiation option)
        (typeParameters: Babel.TypeParameterDeclaration option)
        (loc: SourceLocation option)
        : Python.Statement list =
        printfn $"transformAsClassDef"

        let bases, stmts =
            let entries =
                superClass
                |> Option.map (fun expr -> com.TransformAsExpr(ctx, expr))

            match entries with
            | Some (expr, stmts) -> [ expr ], stmts
            | None -> [], []

        let body: Python.Statement list =
            [
                let (ClassBody(body=body)) = body
                for mber in body do
                    match mber with
                    | Babel.ClassMember.ClassMethod(kind, key, ``params``, body, computed, ``static``, ``abstract``, returnType, typeParameters, loc) ->
                        let self = Arg.Create(Python.Identifier("self"))

                        let parms =
                            ``params``
                            |> List.ofArray

                        let args =
                            parms
                            |> List.choose (function
                                | Pattern.Identifier(id) ->
                                    Arg.Create(Python.Identifier(id.Name)) |> Some
                                | _ -> None)

                        let varargs =
                            parms
                            |> List.choose (function
                                | Pattern.RestElement(argument=argument) ->
                                    Arg.Create(Python.Identifier(argument.Name)) |> Some
                                | _ -> None)
                            |> List.tryHead

                        let arguments = Arguments.Create(args = self :: args, ?vararg=varargs)

                        match kind with
                        | "method" ->
                            let body =
                                com.TransformAsStatements(ctx, ReturnStrategy.Return, body |> Statement.BlockStatement)

                            let name =
                                match key with
                                | Expression.Identifier(id) -> Python.Identifier(id.Name)
                                | _ -> failwith $"transformAsClassDef: Unknown key: {key}"

                            FunctionDef.Create(name, arguments, body = body)
                        | "constructor" ->
                            let name = Python.Identifier("__init__")

                            let body =
                                com.TransformAsStatements(ctx, ReturnStrategy.NoReturn, body |> Statement.BlockStatement)

                            FunctionDef.Create(name, arguments, body = body)
                        | _ -> failwith $"transformAsClassDef: Unknown kind: {kind}"
                    | _ -> failwith $"transformAsClassDef: Unhandled class member {mber}"
            ]

        printfn $"Body length: {body.Length}: ${body}"
        let name = Helpers.cleanNameAsPythonIdentifier (id.Value.Name)

        [ yield! stmts; ClassDef.Create(Python.Identifier(name), body = body, bases = bases) ]

    let transformAsFunction (com: IPythonCompiler) (ctx: Context) (name: Babel.Identifier) (parms: Babel.Pattern array) (body: Babel.BlockStatement) =
        let args =
            parms
            |> List.ofArray
            |> List.map
                (fun pattern ->
                    let name = Helpers.cleanNameAsPythonIdentifier (pattern.Name)
                    Arg.Create(Python.Identifier(name)))

        let arguments = Arguments.Create(args = args)
        let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body |> Statement.BlockStatement)
        let name = Helpers.cleanNameAsPythonIdentifier (name.Name)

        FunctionDef.Create(Python.Identifier(name), arguments, body = body)

    /// Transform Babel expression as Python expression
    let rec transformAsExpr
        (com: IPythonCompiler)
        (ctx: Context)
        (expr: Babel.Expression)
        : Python.Expression * list<Python.Statement> =
        printfn $"transformAsExpr: {expr}"

        match expr with
        | Expression.AssignmentExpression(left=left; operator=operator; right=right) ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)
            match operator with
            | "=" -> NamedExpr.Create(left, right), leftStmts @ rightStmts
            | _ -> failwith $"Unsuppored assingment expression: {operator}"

        | Expression.BinaryExpression(left=left; operator=operator; right=right) ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)

            let toBinOp op = BinOp.Create(left, op, right), leftStmts @ rightStmts
            let toCompare op = Compare.Create(left, [ op ], [ right ]), leftStmts @ rightStmts
            let toCall name =
                let func = Name.Create(Python.Identifier(name), Load)
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

        | Expression.UnaryExpression(operator=operator; argument=arg) ->
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
                Name.Create(id = Python.Identifier("None"), ctx = Load), stmts

        | Expression.ArrowFunctionExpression(``params``=parms; body=body) ->
            let args =
                parms
                |> List.ofArray
                |> List.map (fun pattern -> Arg.Create(Python.Identifier pattern.Name))

            let arguments =
                let args =
                    match args with
                    | [] -> [ Arg.Create(Python.Identifier("_"), Name.Create(Python.Identifier("None"), Load)) ] // Need to receive unit
                    | _ -> args
                Arguments.Create(args = args)

            let stmts = body.Body // TODO: Babel AST should be fixed. Body does not have to be a BlockStatement.
            match stmts with
            | [| Statement.ReturnStatement(argument=argument) |] ->
                let body, stmts = com.TransformAsExpr(ctx, argument)
                Lambda.Create(arguments, body), stmts
            | _ ->
                let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                let name = Helpers.getIdentifier "lifted"

                let func =
                    FunctionDef.Create(name = name, args = arguments, body = body)

                Name.Create(name, Load), [ func ]
        | Expression.CallExpression(callee=callee; arguments=args) -> // FIXME: use transformAsCall
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Call.Create(func, args), stmts @ stmtArgs
        | Expression.ArrayExpression(elements=elements) ->
            let elems, stmts =
                elements
                |> List.ofArray
                |> List.map (fun ex -> com.TransformAsExpr(ctx, ex))
                |> Helpers.unzipArgs

            Tuple.Create(elems), stmts
        | Expression.Literal(Literal.NumericLiteral(value=value)) -> Constant.Create(value = value), []
        | Expression.Literal(Literal.StringLiteral(StringLiteral.StringLiteral(value=value))) -> Constant.Create(value = value), []
        | Expression.Identifier(Identifier(name=name)) ->
            let name = Helpers.cleanNameAsPythonIdentifier name
            Name.Create(id = Python.Identifier name, ctx = Load), []
        | Expression.NewExpression(callee=callee; arguments=args) -> // FIXME: use transformAsCall
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Call.Create(func, args), stmts @ stmtArgs
        | Expression.Super(se) -> Name.Create(Python.Identifier("super().__init__"), ctx = Load), []
        | Expression.ObjectExpression(properties=properties) ->
            let keys, values, stmts =
                [
                    for prop in properties do
                        match prop with
                        | ObjectProperty(key=key; value=value) ->
                            let key, stmts1 = com.TransformAsExpr(ctx, key)
                            let value, stmts2 = com.TransformAsExpr(ctx, value)
                            key, value, stmts1 @ stmts2
                        | Babel.ObjectMethod(key=key; ``params``=parms; body=body) ->
                            let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                            let key, stmts = com.TransformAsExpr(ctx, key)
                            let args =
                                parms
                                |> List.ofArray
                                |> List.map (fun pattern -> Arg.Create(Python.Identifier pattern.Name))

                            let arguments = Arguments.Create(args = args)
                            let name = Helpers.getIdentifier "lifted"

                            let func =
                                FunctionDef.Create(name = name, args = arguments, body = body)
                            key, Name.Create(name, Load), stmts @ [func]

                ]
                |> List.unzip3

            Dict.Create(keys = keys, values = values), stmts |> List.collect id
        | Expression.EmitExpression(value=value; args=args) ->
            let args, stmts =
                args
                |> List.ofArray
                |> List.map (fun expr -> com.TransformAsExpr(ctx, expr))
                |> Helpers.unzipArgs

            match value with
            | "void $0" -> args.[0], stmts
            //| "raise %0" -> Raise.Create()
            | _ -> Emit.Create(value, args), stmts
        | Expression.MemberExpression(computed=true; object=object; property=Expression.Literal(literal)) ->
            let value, stmts = com.TransformAsExpr(ctx, object)

            let attr =
                match literal with
                | Literal.NumericLiteral(value=value) -> Constant.Create(value)
                | Literal.StringLiteral(StringLiteral.StringLiteral(value=value)) -> Constant.Create(value)
                | _ -> failwith $"transformExpressionAsStatements: unknown literal {literal}"

            Subscript.Create(value = value, slice = attr, ctx = Load), stmts
         | Expression.MemberExpression(computed=false; object=object; property=Expression.Identifier(Identifier(name="length"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Name.Create(Python.Identifier "len", Load)
            Call.Create(func, [value]), stmts
         | Expression.MemberExpression(computed=false; object=object; property=Expression.Identifier(Identifier(name="message"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Name.Create(Python.Identifier "str", Load)
            Call.Create(func, [value]), stmts
         | Expression.MemberExpression(computed=false; object=object; property=property) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let attr =
                match property with
                | Expression.Identifier(Identifier(name=name)) -> Python.Identifier(name)
                | _ -> failwith $"transformExpressionAsStatements: unknown property {property}"

            let value =
                match value with
                | Name { Id = Python.Identifier (id); Context = ctx } ->
                    // TODO: Need to make this more generic and robust
                    let id =
                        if id = "Math" then
                            //com.imports.Add("math", )
                            "math"
                        else
                            id

                    Name.Create(id = Python.Identifier(id), ctx = ctx)
                | _ -> value

            Attribute.Create(value = value, attr = attr, ctx = Load), stmts
        | Expression.Literal(Literal.BooleanLiteral(value=value)) -> Constant.Create(value = value), []
        | Expression.FunctionExpression(``params``=parms; body=body) ->
            let args =
                parms
                |> List.ofArray
                |> List.map (fun pattern -> Arg.Create(Python.Identifier pattern.Name))

            let arguments = Arguments.Create(args = args)

            match body.Body with
            | [| Statement.ExpressionStatement(expr) |]  ->
                let body, stmts = com.TransformAsExpr(ctx, expr)
                Lambda.Create(arguments, body), stmts
            | _ ->
                let body =
                    com.TransformAsStatements(ctx, ReturnStrategy.Return, body)

                let name = Helpers.getIdentifier "lifted"

                let func =
                    FunctionDef.Create(name = name, args = arguments, body = body)

                Name.Create(name, Load), [ func ]
        | Expression.ConditionalExpression(test=test; consequent=consequent; alternate=alternate) ->
            let test, stmts1 = com.TransformAsExpr(ctx, test)
            let body, stmts2 = com.TransformAsExpr(ctx, consequent)
            let orElse, stmts3 = com.TransformAsExpr(ctx, alternate)

            IfExp.Create(test, body, orElse), stmts1 @ stmts2 @ stmts3
        | Expression.Literal(Literal.NullLiteral(nl)) -> Name.Create(Python.Identifier("None"), ctx = Load), []
        | Expression.SequenceExpression(expressions=exprs) ->
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
        : Python.Statement list =

        printfn $"transformExpressionAsStatements: {expr}"

        match expr with
        | Expression.AssignmentExpression(left=left; right=right) ->
            let value, stmts = com.TransformAsExpr(ctx, right)

            let targets: Python.Expression list =
                match left with
                | Expression.Identifier(Identifier(name=name)) ->
                    let target =
                        Python.Identifier(Helpers.cleanNameAsPythonIdentifier (name))

                    [ Name.Create(id = target, ctx = Store) ]
                | Expression.MemberExpression(property=property) ->
                    match property with
                    | Expression.Identifier(id) ->
                        let attr = Python.Identifier(Helpers.cleanNameAsPythonIdentifier (id.Name))
                        [
                            Attribute.Create(
                                value = Name.Create(id = Python.Identifier("self"), ctx = Load),
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
        : Python.Statement list =
        printfn $"transformStatementAsStatements: {stmt}, returnStrategy: {returnStrategy}"

        match stmt with
        | Statement.BlockStatement(bs) ->
            [ yield! com.TransformAsStatements(ctx, returnStrategy, bs) ]
            |> transformBody returnStrategy

        | Statement.ReturnStatement(argument=arg) ->
            let expr, stmts = transformAsExpr com ctx arg

            match returnStrategy with
            | ReturnStrategy.NoReturn -> stmts @ [ Expr.Create(expr) ]
            | _ -> stmts @ [ Return.Create(expr) ]
        | Statement.Declaration(Declaration.VariableDeclaration(VariableDeclaration(declarations=declarations))) ->
            [
                for (VariableDeclarator(id=id; init=init)) in declarations do
                    let targets: Python.Expression list =
                        let name = Helpers.cleanNameAsPythonIdentifier (id.Name)
                        [ Name.Create(id = Python.Identifier(name), ctx = Store) ]

                    match init with
                    | Some value ->
                        let expr, stmts = com.TransformAsExpr(ctx, value)
                        yield! stmts
                        Assign.Create(targets, expr)
                    | None -> ()
            ]
        | Statement.ExpressionStatement(expr=expression) ->
            // Handle Babel expressions that we need to transforme here as Python statements
            match expression with
            | Expression.AssignmentExpression(_) -> com.TransformAsStatements(ctx, returnStrategy, expression)
            | _ ->
                [
                    let expr, stmts = com.TransformAsExpr(ctx, expression)
                    yield! stmts
                    Expr.Create(expr)
                ]
        | Statement.IfStatement(test=test; consequent=consequent; alternate=alternate) ->
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
        | Statement.WhileStatement(test=test; body=body) ->
            let expr, stmts = com.TransformAsExpr(ctx, test)

            let body =
                com.TransformAsStatements(ctx, returnStrategy, body)
                |> transformBody ReturnStrategy.NoReturn

            [ yield! stmts; While.Create(test = expr, body = body, orelse = []) ]
        | Statement.TryStatement(block=block; handler=handler; finalizer=finalizer) ->
            let body = com.TransformAsStatements(ctx, returnStrategy, block)

            let finalBody =
                finalizer
                |> Option.map (fun f -> com.TransformAsStatements(ctx, returnStrategy, f))

            let handlers =
                match handler with
                | Some (CatchClause(``param``=parm; body=body)) ->
                    let body = com.TransformAsStatements(ctx, returnStrategy, body)

                    let exn =
                        Name.Create(Python.Identifier("Exception"), ctx = Load)
                        |> Some

                    // Insert a ex.message = str(ex) for all aliased exceptions.
                    let identifier = Python.Identifier(parm.Name)
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
        | Statement.SwitchStatement(discriminant=discriminant; cases=cases) ->
            let value, stmts = com.TransformAsExpr(ctx, discriminant)

            let rec ifThenElse (fallThrough: Python.Expression option) (cases: Babel.SwitchCase list): Python.Statement list option =
                match cases with
                | [] -> None
                | SwitchCase(test=test; consequent=consequent) :: cases ->
                    let body =
                        consequent
                        |> List.ofArray
                        |> List.collect (fun x -> com.TransformAsStatements(ctx, ReturnStrategy.NoBreak, x))

                    match test with
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
        | Statement.BreakStatement(_) -> [ Break ]
        | Statement.Declaration(Declaration.FunctionDeclaration(``params``=parms; id=id; body=body)) ->
            [ com.TransformFunction(ctx, id, parms, body) ]
        | Statement.Declaration(Declaration.ClassDeclaration(body, id, superClass, implements, superTypeParameters, typeParameters, loc)) ->
            transformAsClassDef com ctx body id superClass implements superTypeParameters typeParameters loc
        | Statement.ForStatement
              (init=Some(VariableDeclaration(declarations= [| VariableDeclarator(id=id; init=Some(init)) |] ))
               test=Some(Expression.BinaryExpression(left=left; right=right; operator="<="))
               body=body) ->
            let body = com.TransformAsStatements(ctx, ReturnStrategy.NoReturn, body)
            let target = Name.Create(Python.Identifier id.Name, Load)
            let start, stmts1 = com.TransformAsExpr(ctx, init)
            let stop, stmts2 = com.TransformAsExpr(ctx, right)
            let stop = BinOp.Create(stop, Add, Constant.Create(1)) // Python `range` has exclusive end.
            let iter = Call.Create(Name.Create(Python.Identifier "range", Load), args=[start; stop])
            stmts1 @ stmts2 @ [ For.AsStatement(target=target, iter=iter, body=body) ]
        | Statement.LabeledStatement(body=body) -> com.TransformAsStatements(ctx, returnStrategy, body)
        | Statement.ContinueStatement(_) -> [ Continue ]
        | _ -> failwith $"transformStatementAsStatements: Unhandled: {stmt}"

    let transformBlockStatementAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (block: Babel.BlockStatement)
        : Python.Statement list =

        [ for stmt in block.Body do
            yield! transformStatementAsStatements com ctx returnStrategy stmt ]

    /// Transform Babel program to Python module.
    let transformProgram (com: IPythonCompiler) ctx (body: Babel.ModuleDeclaration array): Module =
        let returnStrategy = ReturnStrategy.Return

        let stmt: Python.Statement list =
            [
                for md in body do
                    match md with
                    | Babel.ExportNamedDeclaration(decl) ->
                        match decl with
                        | Babel.VariableDeclaration(VariableDeclaration(declarations=declarations)) ->
                            for (VariableDeclarator(id, init)) in declarations do
                                let value, stmts = com.TransformAsExpr(ctx, init.Value)

                                let targets: Python.Expression list =
                                    let name = Helpers.cleanNameAsPythonIdentifier (id.Name)
                                    [ Name.Create(id = Python.Identifier(name), ctx = Store) ]

                                yield! stmts
                                yield Assign.Create(targets = targets, value = value)
                        | Babel.FunctionDeclaration(``params``=``params``; body=body; id=id) ->
                            yield com.TransformFunction(ctx, id, ``params``, body)

                        | Babel.ClassDeclaration(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
                            yield! transformAsClassDef com ctx body id superClass implements superTypeParameters typeParameters loc
                        | _ -> failwith $"Unhandled Declaration: {decl}"

                    | Babel.ImportDeclaration(specifiers, source) -> yield! com.TransformAsImports(ctx, specifiers, source)
                    | Babel.PrivateModuleDeclaration(statement=statement) ->
                        yield!
                            com.TransformAsStatements(ctx, returnStrategy, statement)
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
            member bcom.TransformAsClassDef(ctx, body, id, superClass, implements, superTypeParameters, typeParameters, loc) = transformAsClassDef bcom ctx body id superClass implements superTypeParameters typeParameters loc
            member bcom.TransformFunction(ctx, name, args, body) = transformAsFunction bcom ctx name args body
            member bcom.TransformAsImports(ctx, specifiers, source) = transformAsImports bcom ctx specifiers source

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

        let (Program body) = program
        transformProgram com ctx body
