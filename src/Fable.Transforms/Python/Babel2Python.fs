module Fable.Transforms.Babel2Python

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Python
open Fable.AST.Babel
open Fable.Naming
open Fable.Core

[<RequireQualifiedAccess>]
type ReturnStrategy =
    | Return
    | NoReturn
    | NoBreak // Used in switch statement blocks

type UsedNames =
    { GlobalScope: HashSet<string>
      EnclosingScope: HashSet<string>
      LocalScope: HashSet<string>
      NonLocals: HashSet<string> }

type Context =
    { ReturnStrategy: ReturnStrategy
      UsedNames: UsedNames }

type IPythonCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Python.Statement list
    abstract GetIdentifier: ctx: Context * name: string -> Python.Identifier
    abstract GetImportExpr: ctx: Context * moduleName: string * ?name: string * ?loc: SourceLocation -> Python.Identifier option

    abstract TransformAsExpr: Context * Babel.Expression -> Python.Expression * Python.Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.Expression -> Python.Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.Statement -> Python.Statement list
    abstract TransformAsStatements: Context * ReturnStrategy * Babel.BlockStatement -> Python.Statement list

    abstract TransformAsClassDef:
        Context
        * Babel.ClassBody
        * Babel.Identifier option
        * Babel.Expression option
        * Babel.ClassImplements array option
        * Babel.TypeParameterInstantiation option
        * Babel.TypeParameterDeclaration option
        * SourceLocation option ->
        Python.Statement list

    abstract TransformAsImports: Context * Babel.ImportSpecifier array * Babel.StringLiteral -> Python.Statement list
    abstract TransformAsFunction: Context * string * Babel.Pattern array * Babel.BlockStatement -> Python.Statement

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Helpers =
    let index = (Seq.initInfinite id).GetEnumerator()

    let getUniqueIdentifier (name: string): Python.Identifier =
        do index.MoveNext() |> ignore
        let idx = index.Current.ToString()
        Python.Identifier($"{name}_{idx}")

    /// Replaces all '$' and `.`with '_'
    let clean (name: string) =
        //printfn $"clean: {name}"
        match name with
        | "this" -> "self"
        | "async" -> "async_"
        | "from" -> "from_"
        | "class" -> "class_"
        | "for" -> "for_"
        | "Math" -> "math"
        | "Error" -> "Exception"
        | "toString" -> "str"
        | "len" -> "len_"
        | "Map" -> "dict"
        | _ ->
            name.Replace('$', '_').Replace('.', '_').Replace('`', '_')

    let rewriteFableImport moduleName =
        //printfn "ModuleName: %s" moduleName
        let _reFableLib =
            Regex(".*\/fable-library.*\/(?<module>[^\/]*)\.js", RegexOptions.Compiled)

        let m = _reFableLib.Match(moduleName)
        let dashify = applyCaseRule CaseRules.SnakeCase

        if m.Groups.Count > 1 then
            let pymodule =
                m.Groups.["module"].Value
                |> dashify
                |> clean

            let moduleName = String.concat "." [ "fable"; pymodule ]

            printfn "-> Module: %A" moduleName
            moduleName
        else
            // Modules should have short, all-lowercase names.
            let moduleName =
                moduleName.Replace("/", "")
                |> dashify

            printfn "-> Module: %A" moduleName
            moduleName

    let unzipArgs (args: (Python.Expression * Python.Statement list) list): Python.Expression list * Python.Statement list =
        let stmts = args |> List.map snd |> List.collect id
        let args = args |> List.map fst
        args, stmts

    /// A few statements in the generated Babel AST do not produce any effect, and will not be printet. But they are
    /// left in the AST and we need to skip them since they are not valid for Python (either).
    let isProductiveStatement (stmt: Python.Statement) =
        let rec hasNoSideEffects (e: Python.Expression) =
            //printfn $"hasNoSideEffects: {e}"

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
    let getIdentifier (com: IPythonCompiler) (ctx: Context) (name: string) =
        let name = Helpers.clean name

        match name with
        | "math" -> com.GetImportExpr(ctx, "math") |> ignore
        | _ -> ()

        Python.Identifier name

    let rec transformBody (returnStrategy: ReturnStrategy) (body: Python.Statement list) =
        let body = body |> List.choose Helpers.isProductiveStatement

        match body, returnStrategy with
        | [], ReturnStrategy.Return -> [ Statement.return' () ]
        | [], ReturnStrategy.NoBreak
        | [], ReturnStrategy.NoReturn -> [ Pass ]
        | xs, ReturnStrategy.NoBreak ->
            xs
            |> List.filter (fun x -> x <> Break)
            |> transformBody ReturnStrategy.NoReturn
        | _ -> body

    let transformAsImports
        (com: IPythonCompiler)
        (ctx: Context)
        (specifiers: Babel.ImportSpecifier array)
        (source: Babel.StringLiteral)
        : Python.Statement list =
        let (StringLiteral (value = value)) = source
        let pymodule = value |> Helpers.rewriteFableImport

        //printfn "Module: %A" pymodule

        let imports: ResizeArray<Alias> = ResizeArray()
        let importFroms = ResizeArray<Alias>()

        for expr in specifiers do
            match expr with
            | Babel.ImportMemberSpecifier (local, imported) ->
                let asname =
                    if imported.Name <> local.Name then
                        com.GetIdentifier(ctx, local.Name) |> Some
                    else
                        None
                let alias = Alias.alias(com.GetIdentifier(ctx, imported.Name),?asname=asname)
                importFroms.Add(alias)
            | Babel.ImportDefaultSpecifier (local) ->
                let asname =
                    if local.Name <> pymodule then
                        com.GetIdentifier(ctx, local.Name) |> Some
                    else
                        None
                let alias = Alias.alias(com.GetIdentifier(ctx, pymodule), ?asname=asname)
                imports.Add(alias)
            | Babel.ImportNamespaceSpecifier (Identifier (name = name)) ->
                printfn "ImportNamespaceSpecifier: %A" (name, name)

                let asname =
                    if pymodule <> name then
                        com.GetIdentifier(ctx, name) |> Some
                    else
                        None
                let alias = Alias.alias(Python.Identifier(pymodule), ?asname=asname)
                importFroms.Add(alias)

        [ if imports.Count > 0 then
              Statement.import (imports |> List.ofSeq)

          if importFroms.Count > 0 then
              Statement.importFrom (Some(Python.Identifier(pymodule)), importFroms |> List.ofSeq) ]

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
        //printfn $"transformAsClassDef"

        let bases, stmts =
            let entries =
                superClass
                |> Option.map (fun expr -> com.TransformAsExpr(ctx, expr))

            match entries with
            | Some (expr, stmts) -> [ expr ], stmts
            | None -> [], []

        let body: Python.Statement list =
            [ let (ClassBody (body = body)) = body

              for mber in body do
                  match mber with
                  | Babel.ClassMember.ClassMethod (kind, key, ``params``, body, computed, ``static``, ``abstract``, returnType, typeParameters, loc) ->
                      let self = Arg.arg (Python.Identifier("self"))
                      let parms = ``params`` |> List.ofArray

                      let args =
                          parms
                          |> List.choose
                              (function
                              | Pattern.Identifier (id) ->
                                Arg.arg (com.GetIdentifier(ctx, id.Name)) |> Some
                              | _ -> None)

                      let varargs =
                          parms
                          |> List.choose
                              (function
                              | Pattern.RestElement (argument = argument) -> Arg.arg (com.GetIdentifier(ctx, argument.Name)) |> Some
                              | _ -> None)
                          |> List.tryHead
                      let defaults =
                          if List.length args = 1 then
                              [ Expression.name "None"]
                          else
                              []
                      let arguments = Arguments.arguments (args = self :: args, ?vararg = varargs, defaults=defaults)

                      match kind, key with
                      | "method", _ ->
                          let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                          let name =
                              match key with
                              | Expression.Identifier(Identifier(name="toString")) ->
                                  com.GetIdentifier(ctx, "__str__")
                              | Expression.Identifier (id) -> com.GetIdentifier(ctx, id.Name)
                              | Expression.Literal(Literal.StringLiteral(StringLiteral(value=name))) ->
                                  com.GetIdentifier(ctx, name)
                              | MemberExpression(object=Expression.Identifier(Identifier(name="Symbol")); property=Expression.Identifier(Identifier(name="iterator"))) ->
                                  com.GetIdentifier(ctx, "__iter__")
                              | _ ->
                                  failwith $"transformAsClassDef: Unknown key: {key}"

                          FunctionDef.Create(name, arguments, body = body)
                      | "constructor", _ ->
                          let name = Python.Identifier("__init__")
                          let body = com.TransformAsStatements(ctx, ReturnStrategy.NoReturn, body)
                          FunctionDef.Create(name, arguments, body = body)
                      | "get", MemberExpression(object=Expression.Identifier(Identifier(name="Symbol")); property=Expression.Identifier(Identifier(name="toStringTag"))) ->
                          let name = Python.Identifier("__str__")
                          let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                          FunctionDef.Create(name, arguments, body = body)
                      | "get", Expression.Identifier(Identifier(name="size")) ->
                          let name = Python.Identifier("__len__")
                          let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                          FunctionDef.Create(name, arguments, body = body)
                      | "get", Expression.Identifier(Identifier(name=name)) ->
                          let name = Python.Identifier(name)
                          let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                          let decorators = [ Python.Expression.name "property"]
                          FunctionDef.Create(name, arguments, body = body, decoratorList=decorators)
                      | _ -> failwith $"transformAsClassDef: Unknown kind: {kind}, key: {key}"
                  | _ -> failwith $"transformAsClassDef: Unhandled class member {mber}" ]

        let name = com.GetIdentifier(ctx, id.Value.Name)
        [ yield! stmts; Statement.classDef(name, body = body, bases = bases) ]

    /// Transform Bable parameters as Python function
    let transformAsFunction
        (com: IPythonCompiler)
        (ctx: Context)
        (name: string)
        (parms: Babel.Pattern array)
        (body: Babel.BlockStatement)
        =
        let args =
            parms
            |> List.ofArray
            |> List.map
                (fun pattern ->
                    let ident = com.GetIdentifier(ctx, pattern.Name)
                    Arg.arg (ident))

        let arguments = Arguments.arguments (args = args)
        let enclosingScope = HashSet<string>()
        enclosingScope.UnionWith(ctx.UsedNames.EnclosingScope)
        enclosingScope.UnionWith(ctx.UsedNames.LocalScope)
        let ctx' = { ctx with UsedNames = { ctx.UsedNames with LocalScope = HashSet (); EnclosingScope = enclosingScope; NonLocals = HashSet () }}
        let body = com.TransformAsStatements(ctx', ReturnStrategy.Return, body)

        let name = com.GetIdentifier(ctx, name)
        FunctionDef.Create(name, arguments, body = body)

    /// Transform Babel expression as Python expression
    let rec transformAsExpr (com: IPythonCompiler) (ctx: Context) (expr: Babel.Expression): Python.Expression * list<Python.Statement> =
        //printfn $"transformAsExpr: {expr}"

        match expr with
        | AssignmentExpression (left = left; operator = operator; right = right) ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)

            match operator with
            | "=" ->
                Expression.namedExpr (left, right), leftStmts @ rightStmts
            | _ -> failwith $"Unsuppored assingment expression: {operator}"

        | BinaryExpression (left = left; operator = "=="; right = Literal(NullLiteral(_))) ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right = Expression.name("None")
            Expression.compare (left, [ Python.Is ], [ right ]), leftStmts
        | BinaryExpression (left = left; operator = "!="; right = Literal(NullLiteral(_))) ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right = Expression.name("None")
            Expression.compare (left, [ Python.IsNot ], [ right ]), leftStmts
        | BinaryExpression (left = left; operator = operator; right = right) ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)

            let toBinOp op = Expression.binOp (left, op, right), leftStmts @ rightStmts
            let toCompare op = Expression.compare (left, [ op ], [ right ]), leftStmts @ rightStmts

            let toCall name =
                let func = Expression.name (Python.Identifier(name))
                let args = [ left; right ]
                Expression.call (func, args), leftStmts @ rightStmts

            match operator with
            | "+" -> Add |> toBinOp
            | "-" -> Sub |> toBinOp
            | "*" -> Mult |> toBinOp
            | "/" -> Div |> toBinOp
            | "%" -> Mod |> toBinOp
            | "**" -> Pow |> toBinOp
            | "<<<" | "<<" -> LShift |> toBinOp
            | ">>" | ">>>" -> RShift |> toBinOp
            | "|" -> BitOr |> toBinOp
            | "^" -> BitXor |> toBinOp
            | "&" -> BitAnd |> toBinOp
            | "===" | "==" -> Eq |> toCompare
            | "!==" | "!=" -> NotEq |> toCompare
            | ">" -> Gt |> toCompare
            | ">=" -> GtE |> toCompare
            | "<" -> Lt |> toCompare
            | "<=" -> LtE |> toCompare
            | "instanceof" -> toCall "isinstance"
            | _ -> failwith $"Unknown operator: {operator}"

        // Transform `~(~(a/b))` to `a // b`
        | UnaryExpression (operator = "~"; argument = UnaryExpression(operator="~"; argument=BinaryExpression(left, right, operator, loc))) when operator = "/" ->
            let left, leftStmts = com.TransformAsExpr(ctx, left)
            let right, rightStmts = com.TransformAsExpr(ctx, right)
            Expression.binOp(left, FloorDiv,right), leftStmts @ rightStmts
        | UnaryExpression (operator = operator; argument = arg) ->
            let op =
                match operator with
                | "-" -> USub |> Some
                | "+" -> UAdd |> Some
                | "~" -> Invert |> Some
                | "!" -> Not |> Some
                | "void" -> None
                | _ -> failwith $"Unhandled unary operator: {operator}"

            let operand, stmts = com.TransformAsExpr(ctx, arg)

            match op, arg with
            | Some op, _ -> Expression.unaryOp (op, operand), stmts
            | None, Literal(NumericLiteral(value=0.)) ->
                Expression.name (id = Python.Identifier("None")), stmts
            | _ ->
                operand, stmts

        | ArrowFunctionExpression (``params`` = parms; body = body) ->
            let args =
                parms
                |> List.ofArray
                |> List.map (fun pattern -> Arg.arg (com.GetIdentifier(ctx, pattern.Name)))

            let arguments =
                let args =
                    match args with
                    | [] -> [ Arg.arg (Python.Identifier("_"), Expression.name (Python.Identifier("None"))) ] // Need to receive unit
                    | _ -> args

                Arguments.arguments (args = args)

            let stmts = body.Body // TODO: Babel AST should be fixed. Body does not have to be a BlockStatement.

            match stmts with
            | [| Statement.ReturnStatement (argument = argument) |] ->
                let body, stmts = com.TransformAsExpr(ctx, argument)
                Expression.lambda (arguments, body), stmts
            | _ ->
                let ctx' = { ctx with UsedNames = { ctx.UsedNames with LocalScope = HashSet (); EnclosingScope = ctx.UsedNames.LocalScope; NonLocals = HashSet () }}
                let body = com.TransformAsStatements(ctx', ReturnStrategy.Return, body)
                let name = Helpers.getUniqueIdentifier "lifted"

                let func = FunctionDef.Create(name = name, args = arguments, body = body)

                Expression.name (name), [ func ]
        // Transform xs.toString() to str(xs)
        | CallExpression (callee = MemberExpression(object=object; property=Expression.Identifier(Identifier(name="toString")))) ->
            let object, stmts = com.TransformAsExpr(ctx, object)
            let func = Expression.name("str")
            Expression.call (func, [ object ]), stmts
        // Transform xs.charCodeAt(0) to ord(xs)
        | CallExpression (callee = MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "charCodeAt")))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Expression.name (Python.Identifier "ord")
            Expression.call (func, [ value ]), stmts
         // Transform String.fromCharCode(97) to chr(xs)
        | CallExpression (callee = MemberExpression (computed = false; object = Expression.Identifier (Identifier(name = "String")); property = Expression.Identifier (Identifier(name = "fromCharCode"))); arguments=args) ->
            let args, stmts = args |> Array.map (fun obj -> com.TransformAsExpr(ctx, obj)) |> List.ofArray |> Helpers.unzipArgs
            let func = Expression.name (Python.Identifier "chr")
            Expression.call (func, args), stmts
        // Transform "text".split("") to list("text")
        | CallExpression (callee = MemberExpression(object=object; property=Expression.Identifier(Identifier(name="split"))); arguments = [| Literal(Literal.StringLiteral(StringLiteral(value=""))) |]) ->
            let object, stmts = com.TransformAsExpr(ctx, object)
            let func = Expression.name("list")
            Expression.call (func, [ object ]), stmts
        // Transform xs.join("|") to "|".join(xs)
        | CallExpression (callee = MemberExpression(object=object; property=Expression.Identifier(Identifier(name="join"))); arguments = [| Literal(Literal.StringLiteral(StringLiteral(value=value))) |]) ->
            let object, stmts = com.TransformAsExpr(ctx, object)
            let func = Expression.attribute(value=Expression.constant(value), attr=Python.Identifier("join"))
            Expression.call (func, [ object ]), stmts
        | CallExpression (callee = callee; arguments = args) ->
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Expression.call (func, args), stmts @ stmtArgs
        | ArrayExpression (elements = elements) ->
            let elems, stmts =
                elements
                |> List.ofArray
                |> List.map (fun ex -> com.TransformAsExpr(ctx, ex))
                |> Helpers.unzipArgs

            Expression.list (elems), stmts
        | Expression.Literal (Literal.NumericLiteral (value = value)) -> Expression.constant (value = value), []
        | Expression.Literal (Literal.StringLiteral (StringLiteral.StringLiteral (value = value))) ->
            Expression.constant (value = value), []
        | Expression.Identifier (Identifier (name = name)) ->
            let name = com.GetIdentifier(ctx, name)
            Expression.name (id = name), []
        | NewExpression (callee = Expression.Identifier(Identifier(name="Int32Array")); arguments = args) ->
            match args with
            | [| arg |] ->
                let expr, stmts = com.TransformAsExpr(ctx, arg)
                expr, stmts
            | _ -> failwith "Int32Array with multiple arguments not supported."
        | NewExpression (callee = callee; arguments = args) ->
            let func, stmts = com.TransformAsExpr(ctx, callee)

            let args, stmtArgs =
                args
                |> List.ofArray
                |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
                |> Helpers.unzipArgs

            Expression.call (func, args), stmts @ stmtArgs
        | Expression.Super (se) -> Expression.name (Python.Identifier("super().__init__")), []
        | ObjectExpression (properties = properties) ->
            let keys, values, stmts =
                [ for prop in properties do
                      match prop with
                      | ObjectProperty (key = key; value = value) ->
                          let key, stmts1 = com.TransformAsExpr(ctx, key)
                          let value, stmts2 = com.TransformAsExpr(ctx, value)
                          key, value, stmts1 @ stmts2
                      | Babel.ObjectMethod (key = key; ``params`` = parms; body = body) ->
                          //let body = com.TransformAsStatements(ctx, ReturnStrategy.Return, body)
                          let key, stmts = com.TransformAsExpr(ctx, key)

                          let name = Helpers.getUniqueIdentifier "lifted"
                          let func = com.TransformAsFunction(ctx, name.Name, parms, body)
                          key, Expression.name (name), stmts @ [ func ] ]
                |> List.unzip3

            // Transform as namedtuple to allow attribute access of keys.
            com.GetImportExpr(ctx, "collections", "namedtuple") |> ignore
            let keys =
                keys
                |> List.map (function | Expression.Name { Id=Python.Identifier name} -> Expression.constant(name) | ex -> ex)

            Expression.call(
                Expression.call(
                    Expression.name("namedtuple"), [ Expression.constant("object"); Expression.list(keys)]
                ), values
            ), stmts |> List.collect id

        | EmitExpression (value = value; args = args) ->
            let args, stmts =
                args
                |> List.ofArray
                |> List.map (fun expr -> com.TransformAsExpr(ctx, expr))
                |> Helpers.unzipArgs

            match value with
            //| "void $0" -> args.[0], stmts
            | "$0.join('')" ->
                let value = "''.join($0)"
                Expression.emit (value, args), stmts
            | "throw $0" ->
                let value = "raise $0"
                Expression.emit (value, args), stmts
            | Naming.StartsWith("void ") value
            | Naming.StartsWith("new ") value ->
                Expression.emit (value, args), stmts
            | _ -> Expression.emit (value, args), stmts
        // If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression
        | MemberExpression (computed = true; object = object; property = property) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            match property with
            | Expression.Literal (NumericLiteral (value = numeric)) ->
                let attr = Expression.constant(numeric)
                Expression.subscript(value = value, slice = attr, ctx = Load), stmts
            | Expression.Literal (Literal.StringLiteral (StringLiteral (value = str))) ->
                let attr = Expression.constant (str)
                let func = Expression.name("getattr")
                Expression.call(func, args=[value; attr]), stmts
            | Expression.Identifier (Identifier(name=name)) ->
                let attr = Expression.name (com.GetIdentifier(ctx, name))
                Expression.subscript(value = value, slice = attr, ctx = Load), stmts
            | _ ->
                let attr, stmts' = com.TransformAsExpr(ctx, property)
                let func = Expression.name("getattr")
                Expression.call(func=func, args=[value; attr]), stmts @ stmts'
        // If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier
        | MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "indexOf"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let attr = Python.Identifier "index"
            Expression.attribute (value = value, attr = attr, ctx = Load), stmts
        // If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier
        | MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "toLocaleUpperCase"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let attr = Python.Identifier "upper"
            Expression.attribute (value = value, attr = attr, ctx = Load), stmts
        | MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "toLocaleLowerCase"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let attr = Python.Identifier "lower"
            Expression.attribute (value = value, attr = attr, ctx = Load), stmts
        | MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "push"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let attr = Python.Identifier "append"
            Expression.attribute (value = value, attr = attr, ctx = Load), stmts
        // If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier
        | MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "length"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Expression.name (Python.Identifier "len")
            Expression.call (func, [ value ]), stmts
        // If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier
        | MemberExpression (computed = false; object = object; property = Expression.Identifier (Identifier(name = "message"))) ->
            let value, stmts = com.TransformAsExpr(ctx, object)
            let func = Expression.name (Python.Identifier "str")
            Expression.call (func, [ value ]), stmts
        // If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier
        | MemberExpression (computed=false; object = object; property = property) ->
            let value, stmts = com.TransformAsExpr(ctx, object)

            let attr =
                match property with
                | Expression.Identifier (Identifier (name = name)) -> com.GetIdentifier(ctx, name)
                | _ -> failwith $"transformAsExpr: MemberExpression (false): unknown property {property}"
            Expression.attribute (value = value, attr = attr, ctx = Load), stmts
        | Expression.Literal (Literal.BooleanLiteral (value = value)) -> Expression.constant (value = value), []
        | FunctionExpression (``params`` = parms; body = body) ->
            let args =
                parms
                |> List.ofArray
                |> List.map (fun pattern -> Arg.arg (Python.Identifier pattern.Name))

            let arguments = Arguments.arguments (args = args)

            match body.Body with
            | [| Statement.ExpressionStatement (expr) |] ->
                let body, stmts = com.TransformAsExpr(ctx, expr)
                Expression.lambda (arguments, body), stmts
            | _ ->
                let ctx' = { ctx with UsedNames = { ctx.UsedNames with LocalScope = HashSet (); EnclosingScope = ctx.UsedNames.LocalScope; NonLocals = HashSet () }}

                let body = com.TransformAsStatements(ctx', ReturnStrategy.Return, body)
                let name = Helpers.getUniqueIdentifier "lifted"
                let func =
                    FunctionDef.Create(name = name, args = arguments, body = body)

                Expression.name (name), [ func ]
        | ConditionalExpression (test = test; consequent = consequent; alternate = alternate) ->
            let test, stmts1 = com.TransformAsExpr(ctx, test)
            let body, stmts2 = com.TransformAsExpr(ctx, consequent)
            let orElse, stmts3 = com.TransformAsExpr(ctx, alternate)

            Expression.ifExp (test, body, orElse), stmts1 @ stmts2 @ stmts3
        | Expression.Literal (Literal.NullLiteral (nl)) -> Expression.name (Python.Identifier("None")), []
        | SequenceExpression (expressions = exprs) -> //XXX
            // Sequence expressions are tricky. We currently convert them to a function that we call w/zero arguments
            let ctx' = { ctx with UsedNames = { ctx.UsedNames with LocalScope = HashSet (); EnclosingScope = ctx.UsedNames.LocalScope; NonLocals = HashSet () }}

            let body =
                exprs
                |> List.ofArray
                |> List.mapi
                    (fun i ex ->
                         // Return the last expression
                        if i = exprs.Length - 1 then
                            let expr, stmts = com.TransformAsExpr(ctx', ex)
                            stmts @ [Statement.return' (expr)]
                        else
                            com.TransformAsStatements(ctx', ReturnStrategy.Return, ex))
                |> List.collect id
                |> transformBody ReturnStrategy.Return


            let name = Helpers.getUniqueIdentifier ("lifted")
            let func = FunctionDef.Create(name = name, args = Arguments.arguments [], body = body)

            let name = Expression.name (name)
            Expression.call (name), [ func ]
        | ClassExpression(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
            let name =
                match id with
                | Some id -> Python.Identifier(id.Name)
                | None -> Helpers.getUniqueIdentifier "Lifted"

            let babelId = Identifier.identifier(name=name.Name) |> Some
            let stmts = com.TransformAsClassDef(ctx, body, babelId, superClass, implements, superTypeParameters, typeParameters, loc)
            Expression.name name, stmts
        | ThisExpression (_) -> Expression.name ("self"), []
        | _ -> failwith $"transformAsExpr: Unhandled value: {expr}"

    /// Transform Babel expressions as Python statements.
    let rec transformExpressionAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (expr: Babel.Expression)
        : Python.Statement list =

        //printfn $"transformExpressionAsStatements: {expr}"

        match expr with
        // Transform e.g `this["x@22"] = x;` into `setattr(self, "x@22", x)`
        | AssignmentExpression (left = MemberExpression (object = object
                                                         property = Literal (Literal.StringLiteral (StringLiteral (value = attr))))
                                right = right) ->
            // object, attr, value
            let object, stmts1 = com.TransformAsExpr(ctx, object)
            let value, stmts2 = com.TransformAsExpr(ctx, right)
            let attr = Expression.constant(attr)

            [ yield! stmts1
              yield! stmts2
              Statement.expr (value = Expression.call (func = Expression.name ("setattr"), args = [ object; attr; value ])) ]

        // Transform e.g `this.x = x;` into `self.x = x`
        | AssignmentExpression (left = left; right = right) ->
            let value, stmts = com.TransformAsExpr(ctx, right)
            let targets, stmts2: Python.Expression list * Python.Statement list =
                //printfn "AssignmentExpression: left: %A" left
                match left with
                | Expression.Identifier (Identifier (name = name)) ->
                    let target = com.GetIdentifier(ctx, name)
                    let stmts =
                        if not (ctx.UsedNames.LocalScope.Contains name) && ctx.UsedNames.EnclosingScope.Contains name then
                            ctx.UsedNames.NonLocals.Add name |> ignore
                            // printfn "**** Adding non-local: %A" target
                            [ Statement.nonLocal [target] ]
                        else
                            []

                    [ Expression.name (id = target, ctx = Store) ], stmts
                // a.b = c
                | MemberExpression (property = Expression.Identifier (id); object = object; computed=false) ->
                    let attr = com.GetIdentifier(ctx, id.Name)
                    let value, stmts = com.TransformAsExpr(ctx, object)
                    [ Expression.attribute (value = value, attr = attr, ctx = Store) ], stmts
                // a.b[c] = d
                | MemberExpression (property = Expression.Literal(NumericLiteral(value=value)); object = object; computed=false) ->
                    let slice = Expression.constant(value)
                    let expr, stmts = com.TransformAsExpr(ctx, object)
                    [ Expression.subscript (value = expr, slice = slice, ctx = Store) ], stmts
                // object[property] =
                | MemberExpression (property = property; object = object; computed=true) ->
                    let value, stmts = com.TransformAsExpr(ctx, object)
                    let index, stmts' = com.TransformAsExpr(ctx, property)
                    [ Expression.subscript (value = value, slice = index, ctx = Store) ], stmts @ stmts'
                | _ -> failwith $"AssignmentExpression, unknown expression: {left}"

            [ yield! stmts; yield! stmts2; Statement.assign (targets = targets, value = value) ]
        | _ ->
            // Wrap the rest in statement expression
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            [ yield! stmts; Statement.expr expr ]

    /// Transform Babel statement as Python statements.
    let rec transformStatementAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (stmt: Babel.Statement)
        : Python.Statement list =
        //printfn $"transformStatementAsStatements: {stmt}, returnStrategy: {returnStrategy}"

        match stmt with
        | Statement.BlockStatement (bs) ->
            [ yield! com.TransformAsStatements(ctx, returnStrategy, bs) ]
            |> transformBody returnStrategy

        | ReturnStatement (argument = arg) ->
            let expr, stmts = transformAsExpr com ctx arg

            match returnStrategy with
            | ReturnStrategy.NoReturn -> stmts @ [ Statement.expr (expr) ]
            | _ -> stmts @ [ Statement.return' (expr) ]
        | Statement.Declaration (Declaration.VariableDeclaration (VariableDeclaration (declarations = declarations))) ->
            [ for (VariableDeclarator (id = id; init = init)) in declarations do
                  let targets: Python.Expression list =
                      let name = com.GetIdentifier(ctx, id.Name)
                      [ Expression.name (id = name, ctx = Store) ]

                  match init with
                  | Some value ->
                      ctx.UsedNames.LocalScope.Add id.Name |> ignore
                      let expr, stmts = com.TransformAsExpr(ctx, value)
                      yield! stmts
                      Statement.assign (targets, expr)
                  | None -> () ]
        | ExpressionStatement (expr = expression) ->
            // Handle Babel expressions that we need to transforme here as Python statements
            match expression with
            | Expression.AssignmentExpression (_) -> com.TransformAsStatements(ctx, returnStrategy, expression)
            | _ ->
                [ let expr, stmts = com.TransformAsExpr(ctx, expression)
                  yield! stmts
                  Statement.expr (expr) ]
        | IfStatement (test = test; consequent = consequent; alternate = alternate) ->
            let test, stmts = com.TransformAsExpr(ctx, test)

            let body, nonLocals =
                com.TransformAsStatements(ctx, returnStrategy, consequent)
                |> transformBody ReturnStrategy.NoReturn
                |> List.partition (function | Statement.NonLocal (_) -> false | _ -> true )

            let orElse, nonLocals' =
                match alternate with
                | Some alt ->
                    com.TransformAsStatements(ctx, returnStrategy, alt)
                    |> transformBody ReturnStrategy.NoReturn
                    |> List.partition (function | Statement.NonLocal (_) -> false | _ -> true )

                | _ -> [], []

            [ yield! nonLocals @ nonLocals' @ stmts; Statement.if' (test = test, body = body, orelse = orElse) ]
        | WhileStatement (test = test; body = body) ->
            let expr, stmts = com.TransformAsExpr(ctx, test)

            let body =
                com.TransformAsStatements(ctx, returnStrategy, body)
                |> transformBody ReturnStrategy.NoReturn

            [ yield! stmts; Statement.while' (test = expr, body = body, orelse = []) ]
        | TryStatement (block = block; handler = handler; finalizer = finalizer) ->
            let body = com.TransformAsStatements(ctx, returnStrategy, block)

            let finalBody, nonLocals =
                finalizer
                |> Option.map (fun f -> com.TransformAsStatements(ctx, returnStrategy, f))
                |> Option.defaultValue []
                |> List.partition (function | Statement.NonLocal (_) -> false | _ -> true )

            let handlers =
                match handler with
                | Some (CatchClause (param = parm; body = body)) ->
                    let body = com.TransformAsStatements(ctx, returnStrategy, body)

                    let exn =
                        Expression.name (Python.Identifier("Exception"))
                        |> Some

                    // Insert a ex.message = str(ex) for all aliased exceptions.
                    let identifier = Python.Identifier(parm.Name)
                    // let idName = Name.Create(identifier, Load)
                    // let message = Identifier("message")
                    // let trg = Attribute.Create(idName, message, Store)
                    // let value = Call.Create(Name.Create(Identifier("str"), Load), [idName])
                    // let msg = Assign.Create([trg], value)
                    // let body =  msg :: body
                    let handlers = [ ExceptHandler.exceptHandler (``type`` = exn, name = identifier, body = body) ]

                    handlers
                | _ -> []

            [ yield! nonLocals; Statement.try' (body = body, handlers = handlers, finalBody = finalBody) ]
        | SwitchStatement (discriminant = discriminant; cases = cases) ->
            let value, stmts = com.TransformAsExpr(ctx, discriminant)

            let rec ifThenElse (fallThrough: Python.Expression option) (cases: Babel.SwitchCase list): Python.Statement list option =
                match cases with
                | [] -> None
                | SwitchCase (test = test; consequent = consequent) :: cases ->
                    let body =
                        consequent
                        |> List.ofArray
                        |> List.collect (fun x -> com.TransformAsStatements(ctx, ReturnStrategy.NoBreak, x))

                    match test with
                    | None -> body |> Some
                    | Some test ->
                        let test, st = com.TransformAsExpr(ctx, test)

                        let expr = Expression.compare (left = value, ops = [ Eq ], comparators = [ test ])

                        let test =
                            match fallThrough with
                            | Some ft -> Expression.boolOp (op = Or, values = [ ft; expr ])
                            | _ -> expr
                        // Check for fallthrough
                        if body.IsEmpty then
                            ifThenElse (Some test) cases
                        else
                            [ Statement.if' (test = test, body = body, ?orelse = ifThenElse None cases) ]
                            |> Some

            let result = cases |> List.ofArray |> ifThenElse None

            match result with
            | Some ifStmt -> stmts @ ifStmt
            | None -> []
        | Statement.BreakStatement (_) -> [ Break ]
        | Statement.Declaration (Declaration.FunctionDeclaration (``params`` = parms; id = id; body = body)) ->
            [ com.TransformAsFunction(ctx, id.Name, parms, body) ]
        | Statement.Declaration (Declaration.ClassDeclaration (body, id, superClass, implements, superTypeParameters, typeParameters, loc)) ->
            transformAsClassDef com ctx body id superClass implements superTypeParameters typeParameters loc
        | Statement.ForStatement (init = Some (VariableDeclaration(declarations = [| VariableDeclarator (id = id; init = Some (init)) |]))
                                  test = Some (Expression.BinaryExpression (left = left; right = right; operator = operator))
                                  update = Some (Expression.UpdateExpression (operator=update))
                                  body = body) ->
            // printfn "For: %A" body
            let body = com.TransformAsStatements(ctx, ReturnStrategy.NoReturn, body)
            let start, stmts1 = com.TransformAsExpr(ctx, init)
            let stop, stmts2 = com.TransformAsExpr(ctx, right)

            let stop, step =
                match operator, update with
                | "<=", "++" ->
                    let stop = Expression.binOp (stop, Add, Expression.constant (1)) // Python `range` has exclusive end.
                    stop,  1
                | ">=", "--" -> stop, -1
                | _ -> failwith $"Unhandled for loop with operator {operator} and update {update}"

            let target = Expression.name (Python.Identifier id.Name)
            let step = Expression.constant(step)

            let iter = Expression.call (Expression.name (Python.Identifier "range"), args = [ start; stop; step ])

            stmts1 @ stmts2 @ [ Statement.for' (target = target, iter = iter, body = body) ]
        | LabeledStatement (body = body) -> com.TransformAsStatements(ctx, returnStrategy, body)
        | ContinueStatement (_) -> [ Continue ]
        | _ -> failwith $"transformStatementAsStatements: Unhandled: {stmt}"

    let transformBlockStatementAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        (returnStrategy: ReturnStrategy)
        (block: Babel.BlockStatement)
        : Python.Statement list =

        [ for stmt in block.Body do
            yield! transformStatementAsStatements com ctx returnStrategy stmt ]
            |> List.sortBy (function | Statement.NonLocal _ -> 0 | _ -> 1)
            |> transformBody returnStrategy

    /// Transform Babel program to Python module.
    let transformProgram (com: IPythonCompiler) ctx (body: Babel.ModuleDeclaration array): Module =
        let returnStrategy = ReturnStrategy.NoReturn

        //printfn "Transform Program: %A" body
        let stmt: Python.Statement list =
            [ for md in body do
                  match md with
                  | Babel.ExportNamedDeclaration (decl) ->
                      match decl with
                      | Babel.VariableDeclaration (VariableDeclaration (declarations = declarations)) ->
                          for (VariableDeclarator (id, init)) in declarations do
                              let value, stmts = com.TransformAsExpr(ctx, init.Value)

                              let targets: Python.Expression list =
                                  let name = com.GetIdentifier(ctx, id.Name)
                                  ctx.UsedNames.GlobalScope.Add id.Name |> ignore
                                  [ Expression.name (id = name, ctx = Store) ]

                              yield! stmts
                              yield Statement.assign (targets = targets, value = value)
                      | Babel.FunctionDeclaration (``params`` = ``params``; body = body; id = id) ->
                          yield com.TransformAsFunction(ctx, id.Name, ``params``, body)

                      | Babel.ClassDeclaration (body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
                          yield! transformAsClassDef com ctx body id superClass implements superTypeParameters typeParameters loc
                      | _ -> failwith $"Unhandled Declaration: {decl}"

                  | Babel.ImportDeclaration (specifiers, source) -> yield! com.TransformAsImports(ctx, specifiers, source)
                  | Babel.PrivateModuleDeclaration (statement = statement) ->
                      yield!
                          com.TransformAsStatements(ctx, ReturnStrategy.Return, statement)
                          |> transformBody returnStrategy
                  | _ -> failwith $"Unknown module declaration: {md}" ]

        let imports = com.GetAllImports()
        Module.module' (imports @ stmt)

    let getIdentForImport (ctx: Context) (moduleName: string) (name: string option) =
        match name with
        | None ->
            Path.GetFileNameWithoutExtension(moduleName)
            |> Python.Identifier
            |> Some
        | Some name ->
            match name with
            | "*"
            | _ -> name
            |> Python.Identifier
            |> Some

module Compiler =
    open Util

    type PythonCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Python.Statement>()

        interface IPythonCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member bcom.GetIdentifier(ctx, name) = getIdentifier bcom ctx name
            member _.GetImportExpr(ctx, moduleName, ?name, ?loc) =
                let cachedName = moduleName + "::" + defaultArg name "module"

                match imports.TryGetValue(cachedName) with
                | (true, ImportFrom { Names = [ { AsName = localIdent } ] }) ->
                    match localIdent with
                    | Some localIdent -> localIdent |> Some
                    | None -> None
                | (true, Import _) -> None
                | _ ->
                    let localId = getIdentForImport ctx moduleName name

                    match name with
                    | Some name ->
                        let nameId =
                            if name = Naming.placeholder then
                                "`importMember` must be assigned to a variable"
                                |> addError com [] loc
                            name |> Python.Identifier

                        let i = Statement.importFrom(Python.Identifier moduleName |> Some, [ Alias.alias (nameId, ?asname=localId) ])
                        imports.Add(cachedName, i)
                     | None ->
                        let i = Statement.import([ Alias.alias (Python.Identifier moduleName)])
                        imports.Add(cachedName, i)

                    match localId with
                    | Some localId -> localId |> Some
                    | None -> None

            member _.GetAllImports() =
                imports.Values
                |> List.ofSeq

            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformExpressionAsStatements bcom ctx ret e
            member bcom.TransformAsStatements(ctx, ret, e) = transformStatementAsStatements bcom ctx ret e
            member bcom.TransformAsStatements(ctx, ret, e) = transformBlockStatementAsStatements bcom ctx ret e
            member bcom.TransformAsClassDef(ctx, body, id, superClass, implements, superTypeParameters, typeParameters, loc) =
                transformAsClassDef bcom ctx body id superClass implements superTypeParameters typeParameters loc
            member bcom.TransformAsFunction(ctx, name, args, body) = transformAsFunction bcom ctx name args body
            member bcom.TransformAsImports(ctx, specifiers, source) = transformAsImports bcom ctx specifiers source

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.ProjectFile = com.ProjectFile
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
            { ReturnStrategy = ReturnStrategy.NoReturn // Don't return in global scope.
              UsedNames = {
                GlobalScope = HashSet ()
                EnclosingScope = HashSet ()
                LocalScope = HashSet ()
                NonLocals = HashSet ()
              }
            }

        let (Program body) = program
        transformProgram com ctx body
