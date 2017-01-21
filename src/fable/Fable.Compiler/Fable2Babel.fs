module Fable.Fable2Babel

open Fable
open Fable.AST
open System
open System.Collections.Generic

type ReturnStrategy =
    | Return
    | Assign of Babel.Expression * SourceLocation option

type Import = {
    path: string
    selector: string
    localIdent: string
    internalFile: string option
}

type Context = {
    file: Fable.File
    moduleFullName: string
    rootEntitiesPrivateNames: Map<string, string>
}

type IBabelCompiler =
    inherit ICompiler
    abstract DeclarePlugins: (string*IDeclarePlugin) list
    abstract GetFileInfo: string -> Fable.FileInfo
    abstract GetImportExpr: Context -> selector: string -> path: string ->
        Fable.ImportKind -> Babel.Expression
    abstract GetAllImports: unit -> seq<Import>
    abstract GetAllJsIncludes: unit -> seq<Babel.JsInclude>
    abstract TransformExpr: Context -> Fable.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fable.Expr -> Babel.Statement
    abstract TransformExprAndResolve: Context -> ReturnStrategy -> Fable.Expr -> Babel.Statement
    abstract TransformFunction: Context -> Fable.Ident list -> Fable.Expr ->
        (Babel.Pattern list) * U2<Babel.BlockStatement, Babel.Expression>
    abstract TransformClass: Context -> SourceLocation option -> Fable.Expr option ->
        Fable.Declaration list -> Babel.ClassExpression
    abstract TransformObjectExpr: Context -> Fable.ObjExprMember list ->
        Fable.Expr option -> SourceLocation option -> Babel.Expression

and IDeclarePlugin =
    inherit IPlugin
    abstract member TryDeclare:
        com: IBabelCompiler -> ctx: Context -> decl: Fable.Declaration
        -> (Babel.Statement list) option
    abstract member TryDeclareRoot:
        com: IBabelCompiler -> ctx: Context -> file: Fable.File
        -> (U2<Babel.Statement, Babel.ModuleDeclaration> list) option

module Util =
    let inline (|EntKind|) (ent: Fable.Entity) = ent.Kind
    let inline (|ExprType|) (fexpr: Fable.Expr) = fexpr.Type
    let inline (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
    let inline (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e

    /// Matches a sequence of assignments and a return value: a.b = 1, a.c = 2, a
    let (|Assignments|_|) e =
        match e with
        | Fable.Sequential(exprs, r) ->
            let length = exprs.Length
            ((true, 1), exprs)
            ||> List.fold (fun (areAssignments, i) e ->
                match areAssignments, e with
                | false, _ -> false, 0
                | _, Fable.Set _ when i < length -> true, i + 1
                | _, Fable.Value _ -> true, i + 1
                | _ -> false, 0)
            |> function true, _ -> Some(exprs, r) | _ -> None
        | _ -> None

    let consBack tail head = head::tail

    let isNull = function
        | Fable.Value Fable.Null | Fable.Wrapped(Fable.Value Fable.Null,_) -> true
        | _ -> false

    let prepareArgs (com: IBabelCompiler) ctx =
        List.map (function
            | Fable.Value (Fable.Spread expr) ->
                Babel.SpreadElement(com.TransformExpr ctx expr) |> U2.Case2
            | expr -> com.TransformExpr ctx expr |> U2.Case1)

    let ident (id: Fable.Ident) =
        Babel.Identifier id.Name

    let identFromName name =
        let name = Naming.sanitizeIdent (fun _ -> false) name
        Babel.Identifier name

    let sanitizeName propName: Babel.Expression * bool =
        if Naming.identForbiddenCharsRegex.IsMatch propName
        then upcast Babel.StringLiteral propName, true
        else upcast Babel.Identifier propName, false

    let sanitizeProp com ctx = function
        | Fable.Value (Fable.StringConst name)
            when not(Naming.identForbiddenCharsRegex.IsMatch name) ->
            Babel.Identifier (name) :> Babel.Expression, false
        | TransformExpr com ctx property -> property, true

    let getCoreLibImport (com: IBabelCompiler) (ctx: Context) coreModule memb =
        com.GetImportExpr ctx memb coreModule Fable.CoreLib

    let getSymbol com ctx name =
        (getCoreLibImport com ctx "Symbol" "default", Babel.Identifier name)
        |> Babel.MemberExpression :> Babel.Expression

    let get left propName =
        let expr, computed = sanitizeName propName
        Babel.MemberExpression(left, expr, computed) :> Babel.Expression

    let getExpr com ctx (TransformExpr com ctx expr) (property: Fable.Expr) =
        let property, computed = sanitizeProp com ctx property
        match expr with
        | :? Babel.EmptyExpression ->
            match property with
            | :? Babel.StringLiteral as lit ->
                identFromName lit.value :> Babel.Expression
            | _ -> property
        | _ -> Babel.MemberExpression (expr, property, computed) :> Babel.Expression

    let rec accessExpr (members: string list) (baseExpr: Babel.Expression option) =
        match baseExpr with
        | Some baseExpr ->
            match members with
            | [] -> baseExpr
            | m::ms -> get baseExpr m |> Some |> accessExpr ms
        | None ->
            match members with
            // Temporary placeholder to be deleted by getExpr
            | [] -> upcast Babel.EmptyExpression()
            | m::ms -> identFromName m :> Babel.Expression |> Some |> accessExpr ms

    let typeRef (com: IBabelCompiler) ctx (ent: Fable.Entity)
                (genArgs: (string*Fable.Expr) list)
                (memb: string option): Babel.Expression =
        let makeGeneric expr =
            match genArgs, memb with
            | [], _ -> expr
            | genArgs, None ->
                genArgs |> List.map (fun (name, expr) ->
                    let m = Fable.Member(name, Fable.Field, Fable.InstanceLoc, [], Fable.Any)
                    m, [], expr)
                |> fun ms -> com.TransformObjectExpr ctx ms None None
                |> fun genArgs ->
                    upcast Babel.CallExpression(
                        getCoreLibImport com ctx "Util" "makeGeneric",
                        [expr; genArgs] |> List.map U2.Case1)
            | _ -> expr
        let getParts ns fullName memb =
            let split (s: string) =
                s.Split('.') |> Array.toList
            let rec removeCommon (xs1: string list) (xs2: string list) =
                match xs1, xs2 with
                | x1::xs1, x2::xs2 when x1 = x2 -> removeCommon xs1 xs2
                | _ -> xs2
            (@) (removeCommon (split ns) (split fullName))
                (match memb with Some memb -> [memb] | None ->  [])
        match ent.File with
        | None ->
            match Replacements.tryReplaceEntity com ent genArgs with
            | Some expr -> com.TransformExpr ctx expr
            | None -> failwithf "Cannot access type: %s" ent.FullName
        | Some file when ctx.file.SourceFile <> file ->
            let fileInfo = com.GetFileInfo file
            let importPath =
                if fileInfo.targetFile.StartsWith("///")
                then fileInfo.targetFile.Substring(3)
                else Path.getRelativeFileOrDirPath false ctx.file.TargetFile false fileInfo.targetFile
                |> fun x -> Path.ChangeExtension(x, Naming.targetFileExtension)
            getParts fileInfo.rootModule ent.FullName memb
            |> function
            | [] -> com.GetImportExpr ctx "*" importPath (Fable.Internal file)
            | memb::parts ->
                com.GetImportExpr ctx memb importPath (Fable.Internal file)
                |> Some |> accessExpr parts
            |> makeGeneric
        | _ ->
            match getParts ctx.moduleFullName ent.FullName memb with
            | rootMemb::parts when Naming.identForbiddenCharsRegex.IsMatch rootMemb ->
                // Check if the root entity is represented internally with a private name
                if ctx.rootEntitiesPrivateNames.ContainsKey(rootMemb)
                then ctx.rootEntitiesPrivateNames.[rootMemb]
                else rootMemb
                |> fun rootMemb -> accessExpr (rootMemb::parts) None
            | parts -> accessExpr parts None
            |> makeGeneric

    let rec typeAnnotation com ctx typ: Babel.TypeAnnotationInfo =
        let (|FullName|) (ent: Fable.Entity) = ent.FullName
        match typ with
        | Fable.Unit -> upcast Babel.VoidTypeAnnotation()
        | Fable.Boolean -> upcast Babel.BooleanTypeAnnotation()
        | Fable.String -> upcast Babel.StringTypeAnnotation()
        | Fable.Number _ -> upcast Babel.NumberTypeAnnotation()
        // TODO: Typed arrays?
        | Fable.Array genArg ->
            upcast Babel.GenericTypeAnnotation(
                Babel.Identifier("Array"),
                Babel.TypeParameterInstantiation([typeAnnotation com ctx genArg]))
        | Fable.Tuple genArgs ->
            List.map (typeAnnotation com ctx) genArgs
            |> Babel.TupleTypeAnnotation
            :> Babel.TypeAnnotationInfo
        | Fable.Function(argTypes, returnType) ->
            argTypes
            |> List.mapi (fun i argType ->
                Babel.FunctionTypeParam(
                    Babel.Identifier("arg" + (string i)),
                    typeAnnotation com ctx argType))
            |> fun argTypes ->
                Babel.FunctionTypeAnnotation(
                    argTypes, typeAnnotation com ctx returnType)
                :> Babel.TypeAnnotationInfo
        | Fable.GenericParam name ->
            upcast Babel.GenericTypeAnnotation(Babel.Identifier(name))
        // TODO: Make union type annotation?
        | Fable.Enum _ ->
            upcast Babel.NumberTypeAnnotation()
        | Fable.Option genArg ->
            upcast Babel.NullableTypeAnnotation(typeAnnotation com ctx genArg)
        | Fable.DeclaredType(FullName "System.Collections.Generic.IEnumerable", [genArg]) ->
            upcast Babel.GenericTypeAnnotation(
                Babel.Identifier("Iterable"),
                Babel.TypeParameterInstantiation([typeAnnotation com ctx genArg]))
        | Fable.DeclaredType(ent, genArgs) ->
            try
                match typeRef com ctx ent [] None with
                | :? Babel.StringLiteral as str ->
                    match str.value with
                    | "number" -> upcast Babel.NumberTypeAnnotation()
                    | "boolean" -> upcast Babel.BooleanTypeAnnotation()
                    | "string" -> upcast Babel.StringTypeAnnotation()
                    | _ -> upcast Babel.AnyTypeAnnotation()
                | :? Babel.Identifier as id ->
                    let typeParams =
                        match List.map (typeAnnotation com ctx) genArgs  with
                        | [] -> None
                        | xs -> Babel.TypeParameterInstantiation(xs) |> Some
                    upcast Babel.GenericTypeAnnotation(id, ?typeParams=typeParams)
                // TODO: Resolve references to types in nested modules
                | _ -> upcast Babel.AnyTypeAnnotation()
            with
            | _ -> upcast Babel.AnyTypeAnnotation()
        | _ -> upcast Babel.AnyTypeAnnotation()

    let buildArray (com: IBabelCompiler) ctx consKind typ =
        match typ with
        | Fable.Number kind when not com.Options.noTypedArrays ->
            let cons =
                Fable.Util.getTypedArrayName com kind
                |> Babel.Identifier
            let args =
                match consKind with
                | Fable.ArrayValues args ->
                    List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
                    |> Babel.ArrayExpression :> Babel.Expression |> U2.Case1 |> List.singleton
                | Fable.ArrayAlloc arg ->
                    [U2.Case1 (com.TransformExpr ctx arg)]
            Babel.NewExpression(cons, args) :> Babel.Expression
        | _ ->
            match consKind with
            | Fable.ArrayValues args ->
                List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
                |> Babel.ArrayExpression :> Babel.Expression
            | Fable.ArrayAlloc (TransformExpr com ctx arg) ->
                upcast Babel.NewExpression(Babel.Identifier "Array", [U2.Case1 arg])

    let buildStringArray strings =
        strings
        |> List.map (fun x -> Babel.StringLiteral x :> Babel.Expression |> U2.Case1 |> Some)
        |> Babel.ArrayExpression :> Babel.Expression

    let assign range left right =
        Babel.AssignmentExpression(AssignEqual, left, right, ?loc=range)
        :> Babel.Expression

    /// Immediately Invoked Function Expression
    let iife range (statement: Babel.Statement) =
        let block =
            match statement with
            | :? Babel.BlockStatement as block -> U2.Case1 block
            | _ -> Babel.BlockStatement([statement], ?loc=range) |> U2.Case1
        Babel.CallExpression(Babel.ArrowFunctionExpression([], block, ?loc=range), [], ?loc=range)

    let varDeclaration range (var: Babel.Pattern) (isMutable: bool) value =
        let kind = if isMutable then Babel.Let else Babel.Const
        Babel.VariableDeclaration(var, value, kind, ?loc=range)

    let macroExpression range (txt: string) args =
        Babel.MacroExpression(txt, args, ?loc=range) :> Babel.Expression

    let rec flattenSequential = function
        | Fable.Sequential(statements,_) ->
            List.collect flattenSequential statements
        | e -> [e]

    // Sometimes F# compilers access `this` before calling `super` (this happens when using class self identifiers)
    // We need to bring the `super` call to the top of the constructor
    let checkBaseCall consBody =
        let statements = flattenSequential consBody
        ((None, []), statements) ||> List.fold (fun (baseCall, statements) statement ->
            match baseCall, statement with
            | Some baseCall, statement -> Some baseCall, statement::statements
            | None, Fable.Apply(Fable.Value Fable.Super,_,_,_,_) -> Some statement, statements
            | _ -> None, statement::statements)
        |> function
        | Some baseCall, statements -> Fable.Sequential(baseCall::(List.rev statements), consBody.Range)
        | None, statements -> consBody

    let getMemberArgs (com: IBabelCompiler) ctx args (body: Fable.Expr) typeParams hasRestParams =
        let args', body' = com.TransformFunction ctx args body
        let args, returnType, typeParams =
            if com.Options.declaration then
                args' |> List.mapi (fun i arg ->
                    match arg with
                    | :? Babel.Identifier as id ->
                        Babel.Identifier(id.name,
                            Babel.TypeAnnotation(typeAnnotation com ctx args.[i].Type))
                        :> Babel.Pattern
                    | arg -> arg),
                Babel.TypeAnnotation(typeAnnotation com ctx body.Type) |> Some,
                typeParams |> List.map Babel.TypeParameter |> Babel.TypeParameterDeclaration |> Some
            else
                args', None, None
        let args =
            if not hasRestParams then args else
            let args = List.rev args
            (Babel.RestElement(args.Head) :> Babel.Pattern) :: args.Tail |> List.rev
        let body =
            match body' with
            | U2.Case1 e -> e
            | U2.Case2 e -> Babel.BlockStatement([Babel.ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)
        args, body, returnType, typeParams

    let transformValue (com: IBabelCompiler) ctx r = function
        | Fable.ImportRef (memb, path, kind) ->
            let memb, parts =
                let parts = Array.toList(memb.Split('.'))
                parts.Head, parts.Tail
            com.GetImportExpr ctx memb path kind
            |> Some |> accessExpr parts
        | Fable.This -> upcast Babel.ThisExpression ()
        | Fable.Super -> upcast Babel.Super ()
        | Fable.Null -> upcast Babel.NullLiteral ()
        | Fable.IdentValue i -> upcast Babel.Identifier (i.Name)
        | Fable.NumberConst (x,_) -> upcast Babel.NumericLiteral x
        | Fable.StringConst x -> upcast Babel.StringLiteral (x)
        | Fable.BoolConst x -> upcast Babel.BooleanLiteral (x)
        | Fable.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (source, flags)
        | Fable.Lambda (args, body, isArrow) ->
            let args, body = com.TransformFunction ctx args body
            if isArrow
            // Arrow functions capture the enclosing `this` in JS
            then upcast Babel.ArrowFunctionExpression (args, body, ?loc=r)
            else
                match body with
                | U2.Case1 body -> body
                | U2.Case2 e -> Babel.BlockStatement([Babel.ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)
                |> fun body -> upcast Babel.FunctionExpression (args, body, ?loc=r)
        | Fable.ArrayConst (cons, typ) -> buildArray com ctx cons typ
        | Fable.TupleConst vals -> buildArray com ctx (Fable.ArrayValues vals) Fable.Any
        | Fable.Emit emit -> macroExpression None emit []
        | Fable.TypeRef(typEnt, genArgs) -> typeRef com ctx typEnt genArgs None
        | Fable.Spread _ ->
            "Unexpected array spread" |> Fable.Util.attachRange r |> failwith
        | Fable.LogicalOp _ | Fable.BinaryOp _ | Fable.UnaryOp _ ->
            "Unexpected stand-alone operator detected" |> Fable.Util.attachRange r |> failwith

    let transformObjectExpr (com: IBabelCompiler) ctx
                            (members, baseClass, range): Babel.Expression =
        match baseClass with
        | Some _ as baseClass ->
            members
            |> List.map (fun (m, args, body: Fable.Expr) ->
                Fable.MemberDeclaration(m, None, args, body, body.Range))
            |> com.TransformClass ctx range baseClass
            |> fun c -> upcast Babel.NewExpression(c, [], ?loc=range)
        | None ->
            members |> List.map (fun (m: Fable.Member, args, body: Fable.Expr) ->
                let key, computed =
                    match m.Computed with
                    | Some e -> com.TransformExpr ctx e, true
                    | None -> sanitizeName m.Name
                let makeMethod kind =
                    let args, body', returnType, typeParams =
                        getMemberArgs com ctx args body m.GenericParameters m.HasRestParams
                    Babel.ObjectMethod(kind, key, args, body', ?returnType=returnType,
                        ?typeParams=typeParams, computed=computed, ?loc=body.Range)
                    |> U3.Case2
                match m.Kind with
                | Fable.Constructor ->
                    "Unexpected constructor in Object Expression"
                    |> Fable.Util.attachRange range |> failwith
                | Fable.Method -> makeMethod Babel.ObjectMeth
                | Fable.Setter -> makeMethod Babel.ObjectSetter
                | Fable.Getter -> makeMethod Babel.ObjectGetter
                | Fable.Field ->
                    Babel.ObjectProperty(key, com.TransformExpr ctx body,
                            computed=computed, ?loc=body.Range)
                    |> U3.Case1)
            |> fun props ->
                upcast Babel.ObjectExpression(props, ?loc=range)

    let transformApply com ctx (callee, args, kind, range): Babel.Expression =
        match callee, args with
        // Logical, Binary and Unary Operations
        // If the operation has been wrapped in a lambda, there may be arguments in excess,
        // take that into account in matching patterns
        | Fable.Value (Fable.LogicalOp op), (TransformExpr com ctx left)::(TransformExpr com ctx right)::_ ->
            upcast Babel.LogicalExpression (op, left, right, ?loc=range)
        | Fable.Value (Fable.UnaryOp op), (TransformExpr com ctx operand as expr)::_ ->
            upcast Babel.UnaryExpression (op, operand, ?loc=range)
        | Fable.Value (Fable.BinaryOp op), (TransformExpr com ctx left)::(TransformExpr com ctx right)::_ ->
            upcast Babel.BinaryExpression (op, left, right, ?loc=range)
        // Emit expressions
        | Fable.Value (Fable.Emit emit), args ->
            args |> List.map (function
                | Fable.Value(Fable.Spread expr) ->
                    Babel.SpreadElement(com.TransformExpr ctx expr, ?loc=expr.Range) :> Babel.Node
                | expr -> com.TransformExpr ctx expr :> Babel.Node)
            |> macroExpression range emit
        // Module or class static members
        | Fable.Value(Fable.TypeRef(typEnt, _)), [Fable.Value(Fable.StringConst memb)]
            when kind = Fable.ApplyGet ->
            typeRef com ctx typEnt [] (Some memb)
        | _ ->
            match kind with
            | Fable.ApplyMeth ->
                Babel.CallExpression (com.TransformExpr ctx callee, prepareArgs com ctx args, ?loc=range)
                :> Babel.Expression
            | Fable.ApplyCons ->
                Babel.NewExpression (com.TransformExpr ctx callee, prepareArgs com ctx args, ?loc=range)
                :> Babel.Expression
            | Fable.ApplyGet ->
                if List.length args = 1
                then getExpr com ctx callee args.Head
                else FableError("Getter with none or multiple arguments detected", ?range=range) |> raise

    let block r statements =
        Babel.BlockStatement(statements, ?loc=r)

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr: Babel.BlockStatement =
        match ret, expr with
        | None, Fable.Sequential(statements, range) ->
            List.map (com.TransformStatement ctx) statements |> block range
        | None, _ ->
            block expr.Range [com.TransformStatement ctx expr]
        | Some ret, Fable.Sequential(statements, range) ->
            let statements =
                let lasti = (List.length statements) - 1
                statements |> List.mapi (fun i statement ->
                    if i < lasti
                    then com.TransformStatement ctx statement
                    else com.TransformExprAndResolve ctx ret statement)
            block range statements
        | Some ret, _ ->
            block expr.Range [com.TransformExprAndResolve ctx ret expr]

    // TODO: Experimental support for Quotations
    let transformQuote com ctx r expr =
        FableError("Quotations are not supported", ?range=r) |> raise
        // let rec toJson (expr: obj): Babel.Expression =
        //     match expr with
        //     | :? Babel.Node ->
        //         expr.GetType().GetProperties()
        //         |> Seq.choose (fun p ->
        //             match p.Name with
        //             | "loc" -> None // Remove location to make the object lighter
        //             | key ->
        //                 let key = Babel.StringLiteral key
        //                 let value = p.GetValue(expr) |> toJson
        //                 Some(Babel.ObjectProperty(key, value)))
        //         |> Seq.map U3.Case1
        //         |> Seq.toList
        //         |> fun props -> upcast Babel.ObjectExpression(props)
        //     | :? bool as expr -> upcast Babel.BooleanLiteral(expr)
        //     | :? int as expr -> upcast Babel.NumericLiteral(U2.Case1 expr)
        //     | :? float as expr -> upcast Babel.NumericLiteral(U2.Case2 expr)
        //     | :? string as expr -> upcast Babel.StringLiteral(expr)
        //     | expr when Json.isErasedUnion(expr.GetType()) ->
        //         match Json.getErasedUnionValue expr with
        //         | Some v -> toJson v
        //         | None -> upcast Babel.NullLiteral()
        //     | :? System.Collections.IEnumerable as expr ->
        //         let xs = [for x in expr -> U2.Case1(toJson x) |> Some]
        //         upcast Babel.ArrayExpression(xs)
        //     | _ -> failwithf "Unexpected expression inside quote %O" fExpr.Range
        // toJson expr

    let transformStatement com ctx (expr: Fable.Expr): Babel.Statement =
        match expr with
        | Fable.Loop (loopKind, range) ->
            match loopKind with
            | Fable.While (TransformExpr com ctx guard, body) ->
                upcast Babel.WhileStatement (guard, transformBlock com ctx None body, ?loc=range)
            | Fable.ForOf (var, TransformExpr com ctx enumerable, body) ->
                // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right
                let var = Babel.VariableDeclaration(ident var, kind=Babel.Let)
                upcast Babel.ForOfStatement (
                    U2.Case1 var, enumerable, transformBlock com ctx None body, ?loc=range)
            | Fable.For (var, TransformExpr com ctx start,
                            TransformExpr com ctx limit, body, isUp) ->
                let op1, op2 =
                    if isUp
                    then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                    else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus
                upcast Babel.ForStatement (
                    transformBlock com ctx None body,
                    start |> varDeclaration None (ident var) true |> U2.Case1,
                    Babel.BinaryExpression (op1, ident var, limit),
                    Babel.UpdateExpression (op2, false, ident var), ?loc=range)

        | Fable.Set (callee, property, value, range) ->
            let ret =
                match property with
                | None -> Assign(com.TransformExpr ctx callee, range)
                | Some property -> Assign(getExpr com ctx callee property, range)
            com.TransformExprAndResolve ctx ret value

        | Fable.VarDeclaration (var, Fable.Value(Fable.ImportRef(Naming.placeholder, path, kind)), isMutable) ->
            let value = com.GetImportExpr ctx var.Name path kind
            varDeclaration expr.Range (ident var) isMutable value :> Babel.Statement

        | Fable.VarDeclaration (var, TransformExpr com ctx value, isMutable) ->
            varDeclaration expr.Range (ident var) isMutable value :> Babel.Statement

        | Fable.TryCatch (body, catch, finalizer, range) ->
            let handler =
                catch |> Option.map (fun (param, body) ->
                    Babel.CatchClause (ident param,
                        transformBlock com ctx None body, ?loc=body.Range))
            let finalizer =
                finalizer |> Option.map (fun finalizer ->
                    transformBlock com ctx None body)
            upcast Babel.TryStatement (transformBlock com ctx None body,
                ?handler=handler, ?finalizer=finalizer, ?loc=range)

        | Fable.Throw (TransformExpr com ctx ex, _, range) ->
            upcast Babel.ThrowStatement(ex, ?loc=range)

        | Fable.DebugBreak range ->
            upcast Babel.DebuggerStatement(?loc=range)

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse (TransformExpr com ctx guardExpr,
                            thenStatement, elseStatement, range) ->
            let thenStatement = transformBlock com ctx None thenStatement
            let elseStatement =
                match elseStatement with
                | e when isNull e -> None
                | Fable.IfThenElse _ as e-> com.TransformStatement ctx e |> Some
                | e -> transformBlock com ctx None e :> Babel.Statement |> Some
            upcast Babel.IfStatement(guardExpr, thenStatement,
                            ?alternate=elseStatement, ?loc=range)

        | Fable.Switch(TransformExpr com ctx matchValue, cases, defaultBranch, _, range) ->
            let transformBranch test branch =
                let b = transformBlock com ctx None branch
                match test with
                | Some(TransformExpr com ctx test) -> b.body@[Babel.BreakStatement()], Some test
                | None -> b.body, None // Default branch
                |> fun (statements, test) ->
                    // Put the body of the case in a block to prevent scope problems (see #483)
                    let body = [block b.loc statements :> Babel.Statement]
                    Babel.SwitchCase(body, ?test=test, ?loc=b.loc)
            let cases =
                cases |> List.collect(fun (tests, branch) ->
                    let prev =
                        match List.length tests with
                        | l when l > 1 ->
                            List.take (l - 1) tests
                            |> List.map (fun test ->
                                Babel.SwitchCase([], com.TransformExpr ctx test))
                        | _ -> []
                    let case = transformBranch (List.last tests |> Some) branch
                    prev@[case])
            let cases =
                match defaultBranch with
                | Some defaultBranch -> cases@[transformBranch None defaultBranch]
                | None -> cases
            upcast Babel.SwitchStatement(matchValue, cases, ?loc=range)

        | Fable.Sequential(statements, range) ->
            statements |> List.map (com.TransformStatement ctx)
            |> block range :> Babel.Statement

        | Fable.Label(name, body, range) ->
            upcast Babel.LabeledStatement(ident name, com.TransformStatement ctx body, ?loc=range)

        | Fable.Break(optName, range) ->
            upcast Babel.BreakStatement(?label=Option.map ident optName, ?loc=range)

        | Fable.Continue(optName, range) ->
            upcast Babel.ContinueStatement(?label=Option.map ident optName, ?loc=range)

        | Fable.Return(e, range) ->
            com.TransformExprAndResolve ctx Return e

        | Fable.Wrapped (expr, _) ->
            com.TransformStatement ctx expr

        // Expressions become ExpressionStatements
        | Fable.Value _ | Fable.Apply _ | Fable.ObjExpr _ | Fable.Quote _ ->
            upcast Babel.ExpressionStatement (com.TransformExpr ctx expr, ?loc=expr.Range)

    let transformExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Babel.Expression =
        match expr with
        | Fable.Value kind -> transformValue com ctx expr.Range kind

        | Fable.ObjExpr (members, _, baseClass, _) ->
            transformObjectExpr com ctx (members, baseClass, expr.Range)

        | Fable.Wrapped (TransformExpr com ctx expr, _) -> expr

        | Fable.Apply (callee, args, kind, _, range) ->
            transformApply com ctx (callee, args, kind, range)

        | Fable.IfThenElse (TransformExpr com ctx guardExpr,
                            TransformExpr com ctx thenExpr,
                            TransformExpr com ctx elseExpr, range) ->
            upcast Babel.ConditionalExpression (
                guardExpr, thenExpr, elseExpr, ?loc = range)

        | Fable.Set (callee, property, TransformExpr com ctx value, range) ->
            match property with
            | None -> com.TransformExpr ctx callee
            | Some property -> getExpr com ctx callee property
            |> assign range <| value

        // Optimization: Compile sequential as expression if possible
        | Assignments(exprs, r) ->
            List.map (com.TransformExpr ctx) exprs
            |> fun exprs -> upcast Babel.SequenceExpression(exprs, ?loc=r)

        // These cannot appear in expression position in JS
        // They must be wrapped in a lambda
        | Fable.Sequential _ | Fable.TryCatch _ | Fable.Throw _
        | Fable.DebugBreak _ | Fable.Loop _ | Fable.Switch _
        | Fable.Break _ | Fable.Continue _ | Fable.Label _ | Fable.Return _ ->
            transformBlock com ctx (Some Return) expr :> Babel.Statement
            |> iife expr.Range :> Babel.Expression

        | Fable.VarDeclaration _ ->
            "Unexpected variable declaration"
            |> Fable.Util.attachRange expr.Range |> failwith

        | Fable.Quote quote ->
            transformQuote com ctx expr.Range quote

    let transformExprAndResolve (com: IBabelCompiler) ctx ret
                                 (expr: Fable.Expr): Babel.Statement =
        let resolve strategy expr: Babel.Statement =
            match strategy with
            | Return -> upcast Babel.ReturnStatement(expr, ?loc=expr.loc)
            | Assign (left, r) -> upcast Babel.ExpressionStatement(assign r left expr, ?loc=r)
        match expr with
        | Fable.Value kind ->
            transformValue com ctx expr.Range kind |> resolve ret

        | Fable.ObjExpr (members, _, baseClass, _) ->
            transformObjectExpr com ctx (members, baseClass, expr.Range)
            |> resolve ret

        | Fable.Wrapped (TransformExpr com ctx expr, _) ->
            resolve ret expr

        | Fable.Apply (callee, args, kind, _, range) ->
            transformApply com ctx (callee, args, kind, range)
            |> resolve ret

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse (TransformExpr com ctx guardExpr,
                            thenStatement, elseStatement, range) ->
            let thenStatement = transformBlock com ctx (Some ret) thenStatement
            let elseStatement =
                match elseStatement with
                | e when isNull e -> None
                | Fable.IfThenElse _ as e-> com.TransformExprAndResolve ctx ret e |> Some
                | e -> transformBlock com ctx (Some ret) e :> Babel.Statement |> Some
            upcast Babel.IfStatement(guardExpr, thenStatement,
                            ?alternate=elseStatement, ?loc=range)

        | Fable.Sequential (statements, range) ->
            let statements =
                let lasti = (List.length statements) - 1
                statements |> List.mapi (fun i statement ->
                    if i < lasti
                    then com.TransformStatement ctx statement
                    else com.TransformExprAndResolve ctx ret statement)
            upcast block range statements

        | Fable.TryCatch (body, catch, finalizer, range) ->
            let handler =
                catch |> Option.map (fun (param, body) ->
                    Babel.CatchClause (ident param,
                        transformBlock com ctx (Some ret) body, ?loc=body.Range))
            let finalizer =
                finalizer |> Option.map (transformBlock com ctx None)
            upcast Babel.TryStatement (transformBlock com ctx (Some ret) body,
                ?handler=handler, ?finalizer=finalizer, ?loc=range)

        // These cannot be resolved (don't return anything)
        // Just compile as a statement
        | Fable.Throw _ | Fable.DebugBreak _ | Fable.Loop _
        | Fable.Set _ | Fable.VarDeclaration _ | Fable.Switch _
        | Fable.Label _ | Fable.Continue _ | Fable.Break _ | Fable.Return _ ->
            com.TransformStatement ctx expr

        | Fable.Quote quote ->
            transformQuote com ctx expr.Range quote |> resolve ret

    let transformFunction com ctx args (body: Fable.Expr) =
        let args: Babel.Pattern list =
            List.map (fun x -> upcast ident x) args
        let body: U2<Babel.BlockStatement, Babel.Expression> =
            match body with
            | ExprType Fable.Unit
            | Fable.Throw _ | Fable.DebugBreak _ | Fable.Loop _ | Fable.Set _ ->
                transformBlock com ctx None body |> U2.Case1
            | Fable.Sequential _ | Fable.TryCatch _ | Fable.Switch _ ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | Fable.IfThenElse _ when body.IsJsStatement ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | _ -> transformExpr com ctx body |> U2.Case2
        args, body

    let transformClass com ctx range (ent: Fable.Entity option) baseClass decls =
        let declareProperty com ctx name typ =
            let typ = Babel.TypeAnnotation(typeAnnotation com ctx typ)
            Babel.ClassProperty(Babel.Identifier(name), typeAnnotation=typ)
            |> U2<Babel.ClassMethod,_>.Case2
        let declareMethod range kind name args (body: Fable.Expr)
                          typeParams hasRestParams isStatic computed =
            let name, computed =
                match computed with
                | Some e -> transformExpr com ctx e, true
                | None -> sanitizeName name
            let args, body, returnType, typeParams =
                getMemberArgs com ctx args body typeParams hasRestParams
            Babel.ClassMethod(kind, name, args, body, computed, isStatic,
                ?returnType=returnType, ?typeParams=typeParams, ?loc=range)
            |> U2<_,Babel.ClassProperty>.Case1
        let baseClass = baseClass |> Option.map (transformExpr com ctx)
        let interfaces = match ent with | Some e -> e.Interfaces | None -> []
        decls
        |> List.map (function
            | Fable.MemberDeclaration(m, _, args, body, range) ->
                let kind, name, loc, computed, body =
                    match m.Kind with
                    | Fable.Constructor ->
                        let body =
                            match ent with
                            | Some(EntKind(Fable.Class(Some _, _))) -> checkBaseCall body
                            | _ -> body
                        Babel.ClassConstructor, "constructor", Fable.InstanceLoc, None, body
                    | Fable.Method -> Babel.ClassFunction, m.OverloadName, m.Location, m.Computed, body
                    | Fable.Getter | Fable.Field -> Babel.ClassGetter, m.Name, m.Location, m.Computed, body
                    | Fable.Setter -> Babel.ClassSetter, m.Name, m.Location, m.Computed, body
                let isStatic = loc = Fable.StaticLoc
                declareMethod range kind name args body m.GenericParameters m.HasRestParams isStatic computed
            | Fable.ActionDeclaration _
            | Fable.EntityDeclaration _ as decl ->
                failwithf "Unexpected declaration in class: %A" decl)
        |> fun members ->
            let id = ent |> Option.map (fun x -> identFromName x.Name)
            let typeParams, members =
                match com.Options.declaration, ent with
                | true, Some ent ->
                    let typeParams =
                        ent.GenericParameters
                        |> List.map Babel.TypeParameter
                        |> Babel.TypeParameterDeclaration |> Some
                    let props =
                        match ent.Kind with
                        | Fable.Union _ ->
                            ["Case", Fable.String; "Fields", Fable.Array Fable.Any]
                            |> List.map (fun (name, typ) -> declareProperty com ctx name typ)
                        | Fable.Record fields | Fable.Exception fields ->
                            fields |> List.map (fun (name, typ) -> declareProperty com ctx name typ)
                        | _ -> []
                    typeParams, props@members
                | _ -> None, members
            Babel.ClassExpression(Babel.ClassBody(members, ?loc=range),
                    ?id=id, ?typeParams=typeParams, ?super=baseClass, ?loc=range)

    let declareType (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        Babel.CallExpression(
            getCoreLibImport com ctx "Symbol" "setType",
            [Babel.StringLiteral ent.FullName :> Babel.Expression |> U2.Case1
            ; typeRef com ctx ent [] None |> U2.Case1])
        |> Babel.ExpressionStatement :> Babel.Statement

    let declareEntryPoint com ctx (funcExpr: Babel.Expression) =
        let argv = macroExpression None "process.argv.slice(2)" []
        let main = Babel.CallExpression (funcExpr, [U2.Case1 argv], ?loc=funcExpr.loc) :> Babel.Expression
        // Don't exit the process after leaving main, as there may be a server running
        // Babel.ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        Babel.ExpressionStatement(main, ?loc=funcExpr.loc) :> Babel.Statement

    let declareNestedModMember range publicName privateName isPublic isMutable modIdent expr =
        let privateName = defaultArg privateName publicName
        match isPublic, modIdent with
        | true, Some modIdent ->
            // TODO: Define also get-only properties for non-mutable values?
            if isMutable then
                let macro = sprintf "Object.defineProperty($0,'%s',{get:()=>$1,set:x=>$1=x}),$2" publicName
                macroExpression range macro [modIdent; identFromName privateName; expr]
            else
                assign range (get modIdent publicName) expr
        | _ -> expr
        |> varDeclaration range (identFromName privateName) isMutable :> Babel.Statement
        |> U2.Case1 |> List.singleton

    let declareRootModMember range publicName privateName isPublic isMutable _
                             (expr: Babel.Expression) =
        let privateName = defaultArg privateName publicName
        let privateIdent = identFromName privateName
        let decl: Babel.Declaration =
            match expr with
            | :? Babel.ClassExpression as e ->
                upcast Babel.ClassDeclaration(e.body, privateIdent,
                    ?super=e.superClass, ?typeParams=e.typeParameters, ?loc=e.loc)
            | :? Babel.FunctionExpression as e ->
                upcast Babel.FunctionDeclaration(privateIdent, e.``params``, e.body,
                    ?returnType=e.returnType, ?typeParams=e.typeParameters, ?loc=e.loc)
            | _ -> upcast varDeclaration range privateIdent isMutable expr
        match isPublic with
        | false -> U2.Case1 (decl :> Babel.Statement) |> List.singleton
        | true when publicName = privateName ->
            Babel.ExportNamedDeclaration(decl, ?loc=range)
            :> Babel.ModuleDeclaration |> U2.Case2 |> List.singleton
        | true ->
            // Replace ident forbidden chars of root members, see #207
            let publicName = Naming.replaceIdentForbiddenChars publicName
            let expSpec = Babel.ExportSpecifier(privateIdent, Babel.Identifier publicName)
            let expDecl = Babel.ExportNamedDeclaration(specifiers=[expSpec])
            [expDecl :> Babel.ModuleDeclaration |> U2.Case2; decl :> Babel.Statement |> U2.Case1]

    let transformModMember (com: IBabelCompiler) ctx declareMember modIdent
                           (m: Fable.Member, privName, args, body, range) =
        let expr =
            match m.Kind with
            | Fable.Getter | Fable.Field ->
                match body with
                | Fable.Value(Fable.ImportRef(Naming.placeholder, path, kind)) ->
                    com.GetImportExpr ctx m.Name path kind
                | _ -> transformExpr com ctx body
            | Fable.Method ->
                let bodyRange = body.Range
                let args, body, returnType, typeParams =
                    getMemberArgs com ctx args body m.GenericParameters false
                // Don't lexically bind `this` (with arrow function) or
                // it will fail with extension members
                let id = Babel.Identifier(defaultArg privName m.OverloadName)
                upcast Babel.FunctionExpression (args, body, id=id,
                    ?returnType=returnType, ?typeParams=typeParams, ?loc=bodyRange)
            | Fable.Constructor | Fable.Setter ->
                failwithf "Unexpected member in module %O: %A" modIdent m.Kind
        let memberRange =
            match range, expr.loc with Some r1, Some r2 -> Some(r1 + r2) | _ -> None
        if m.TryGetDecorator("EntryPoint").IsSome
        then declareEntryPoint com ctx expr |> U2.Case1 |> List.singleton
        else declareMember memberRange m.OverloadName privName m.IsPublic m.IsMutable modIdent expr

    let declareInterfaceEntity (com: IBabelCompiler) (ent: Fable.Entity) =
        // TODO: Add `extends` (inherited interfaces)
        // TODO: Add generic parameters
        // TODO: Add abstract methods
        if not com.Options.declaration then [] else
        let id = Babel.Identifier ent.Name
        let body = Babel.ObjectTypeAnnotation []
        Babel.InterfaceDeclaration(body, id, []) :> Babel.Statement
        |> U2.Case1 |> List.singleton

    let declareClass com ctx declareMember modIdent
                     (ent: Fable.Entity) privateName
                     entDecls entRange baseClass isClass =
        let classDecl =
            // Don't create a new context for class declarations
            transformClass com ctx entRange (Some ent) baseClass entDecls
            |> declareMember entRange ent.Name (Some privateName) ent.IsPublic false modIdent
        let classDecl =
            (declareType com ctx ent |> U2.Case1)::classDecl
        // Check if there's a static constructor
        entDecls |> Seq.exists (function
            | Fable.MemberDeclaration(m,_,_,_,_) ->
                match m.Name, m.Kind, m.Location with
                | ".cctor", Fable.Method, Fable.StaticLoc -> true
                | _ -> false
            | _ -> false)
        |> function
        | false -> classDecl
        | true ->
            let cctor = Babel.MemberExpression(
                            typeRef com ctx ent [] None, Babel.StringLiteral ".cctor", true)
            Babel.ExpressionStatement(Babel.CallExpression(cctor, [])) :> Babel.Statement
            |> U2.Case1 |> consBack classDecl

    let rec transformNestedModule com ctx (ent: Fable.Entity) entDecls entRange =
        let modIdent = Babel.Identifier Naming.exportsIdent
        let modDecls =
            let ctx = { ctx with moduleFullName = ent.FullName }
            transformModDecls com ctx declareNestedModMember (Some modIdent) entDecls
            |> List.map (function
                | U2.Case1 statement -> statement
                | U2.Case2 _ -> failwith "Unexpected export in nested module")
        Babel.CallExpression(
            Babel.FunctionExpression([modIdent],
                block entRange modDecls, ?loc=entRange),
            [U2.Case1 (upcast Babel.ObjectExpression [])],
            ?loc=entRange)

    and transformModDecls (com: IBabelCompiler) ctx declareMember modIdent decls =
        let pluginDeclare (decl: Fable.Declaration) =
            com.DeclarePlugins
            |> Plugins.tryPlugin decl.Range (fun p -> p.TryDeclare com ctx decl)
        decls |> List.fold (fun acc decl ->
            match decl with
            | Patterns.Try pluginDeclare statements ->
                (statements |> List.map U2.Case1) @ acc
            | Fable.ActionDeclaration (e,_) ->
                transformStatement com ctx e
                |> U2.Case1
                |> consBack acc
            | Fable.MemberDeclaration(m,privName,args,body,r) ->
                match m.Kind with
                | Fable.Constructor | Fable.Setter _ -> acc // Only happens for VS tests
                | _ -> transformModMember com ctx declareMember modIdent (m,privName,args,body,r) @ acc
            | Fable.EntityDeclaration (ent, privateName, entDecls, entRange) ->
                match ent.Kind with
                | Fable.Interface ->
                    declareInterfaceEntity com ent
                | Fable.Class(baseClass, _) ->
                    let baseClass = baseClass |> Option.map snd
                    declareClass com ctx declareMember modIdent
                        ent privateName entDecls entRange baseClass true
                | Fable.Exception _ ->
                    let baseClass = Some(Fable.Value(Fable.IdentValue(Fable.Ident("Error"))))
                    declareClass com ctx declareMember modIdent
                        ent privateName entDecls entRange baseClass true
                | Fable.Union _ | Fable.Record _ ->
                    declareClass com ctx declareMember modIdent
                        ent privateName entDecls entRange None false
                | Fable.Module ->
                    transformNestedModule com ctx ent entDecls entRange
                    |> declareMember entRange ent.Name (Some privateName)
                        ent.IsPublic false modIdent
                |> List.append <| acc) []
        |> fun decls ->
            match modIdent with
            | None -> decls
            | Some modIdent ->
                Babel.ReturnStatement modIdent
                :> Babel.Statement |> U2.Case1
                |> consBack decls
            |> List.rev

    let makeCompiler (com: ICompiler) (prevJsIncludes: Babel.JsInclude seq)
                     (projectMaps: Dictionary<string,Map<string, Fable.FileInfo>>) =
        let prevJsIncludes = prevJsIncludes |> Seq.toList
        let jsIncludes = ResizeArray<Babel.JsInclude>()
        let imports = Dictionary<string*string,Import>()
        let declarePlugins =
            com.Plugins |> List.choose (function
                | path, (:? IDeclarePlugin as plugin) -> Some (path, plugin)
                | _ -> None)
        { new IBabelCompiler with
            member bcom.DeclarePlugins =
                declarePlugins
            member bcom.GetFileInfo fileName =
                projectMaps |> Seq.tryPick (fun kv ->
                    Map.tryFind fileName kv.Value)
                |> function
                | Some info -> info
                | None -> failwithf "Cannot find info for file: %s" fileName
            // TODO: Create a cache to optimize imports
            member bcom.GetImportExpr ctx selector path kind =
                let sanitizeSelector selector =
                    if selector = "*"
                    then selector
                    elif selector = Naming.placeholder
                    then FableError("`importMember` must be assigned to a variable") |> raise
                    // Replace ident forbidden chars of root members, see #207
                    else Naming.replaceIdentForbiddenChars selector
                let getLocalIdent (ctx: Context) (selector: string) =
                    match selector with
                    | "*" | "default" | "" ->
                        let x = path.TrimEnd('/')
                        x.Substring(x.LastIndexOf '/' + 1)
                    | _ -> selector
                    |> Naming.sanitizeIdent (fun s ->
                        ctx.file.UsedVarNames.Contains s
                            || (imports.Values |> Seq.exists (fun i -> i.localIdent = s)))
                let includeJs (sourcePath: string) =
                    let getRelativePath name =
                        Path.Combine3(bcom.Options.outDir, "js_includes", name + ".js")
                        |> Path.getRelativeFileOrDirPath false ctx.file.TargetFile false
                    Seq.append prevJsIncludes jsIncludes
                    |> Seq.tryFind (fun x -> x.sourcePath = sourcePath)
                    |> function
                    | Some jsInclude -> getRelativePath jsInclude.name
                    | None ->
                        let name =
                            Path.GetFileNameWithoutExtension(sourcePath)
                            |> Naming.preventConflicts (fun name ->
                                Seq.append prevJsIncludes jsIncludes
                                |> Seq.exists (fun x -> x.name = name))
                        jsIncludes.Add({ sourcePath=sourcePath; name=name })
                        getRelativePath name
                let resolvePath (com: ICompiler) (ctx: Context) (importPath: string) =
                    let resolveRelative (ctx: Context) (importPath: string) =
                        let fileDir = Path.GetDirectoryName(ctx.file.SourceFile)
                        Path.GetFullPath(Path.Combine(fileDir, importPath))
                    match com.Options.includeJs, importPath with
                    | true, Naming.StartsWith "." _ ->
                        resolveRelative ctx importPath |> includeJs
                    | false, Naming.StartsWith "." _ ->
                        // Resolve relative paths with `outDir` if they don't point to an internal file: see #472
                        resolveRelative ctx importPath
                        |> Path.getRelativeFileOrDirPath false ctx.file.TargetFile false
                    | _ -> importPath
                match imports.TryGetValue((selector, path)) with
                | true, i -> upcast Babel.Identifier(i.localIdent)
                | false, _ ->
                    let localId = getLocalIdent ctx selector
                    let i = {
                        selector = sanitizeSelector selector
                        localIdent = localId
                        internalFile =
                            match kind with
                            | Fable.Internal file -> Some file
                            | _ -> None
                        path =
                            match kind with
                            | Fable.CustomImport -> resolvePath com ctx path
                            | Fable.Internal _ -> path
                            | Fable.CoreLib ->
                                let path = com.Options.coreLib + "/" + path + Naming.targetFileExtension
                                if not(path.StartsWith ".") then path else
                                Path.GetFullPath path
                                |> Path.getRelativePath ctx.file.TargetFile
                                |> fun path -> path.TrimEnd('/')
                    }
                    imports.Add((selector,path), i)
                    upcast Babel.Identifier (localId)
            member bcom.GetAllImports () = upcast imports.Values
            member bcom.GetAllJsIncludes () = upcast jsIncludes
            member bcom.TransformExpr ctx e = transformExpr bcom ctx e
            member bcom.TransformStatement ctx e = transformStatement bcom ctx e
            member bcom.TransformExprAndResolve ctx ret e = transformExprAndResolve bcom ctx ret e
            member bcom.TransformFunction ctx args body = transformFunction bcom ctx args body
            member bcom.TransformClass ctx r baseClass members =
                transformClass bcom ctx r None baseClass members
            member bcom.TransformObjectExpr ctx membs baseClass r =
                transformObjectExpr bcom ctx (membs, baseClass, r)
        interface ICompiler with
            member __.Options = com.Options
            member __.ProjDir = com.ProjDir
            member __.Plugins = com.Plugins
            member __.AddLog msg = com.AddLog msg
            member __.GetLogs() = com.GetLogs()
            member __.GetUniqueVar() = com.GetUniqueVar() }

module Compiler =
    open Util
    open System.IO

    let transformFiles (com: ICompiler) (extra: Map<string, obj>, files) =
        let projectMaps: Dictionary<string, Map<string, Fable.FileInfo>> =
            ("projectMaps", extra)
            ||> Map.findOrRun (fun () -> failwith "Expected project maps")
        let prevJsIncludes = ResizeArray<Babel.JsInclude>()
        let newCache = fun () -> Dictionary<string, string list>()
        let dependenciesDic = Map.findOrRun newCache "dependencies" extra
        files |> Seq.map (fun (file: Fable.File) ->
            try
                // let t = PerfTimer("Fable > Babel")
                let curProjMap = projectMaps.[Naming.current]
                let com = makeCompiler com prevJsIncludes projectMaps
                let ctx = {
                    file = file
                    moduleFullName = curProjMap.[file.SourceFile].rootModule
                    rootEntitiesPrivateNames =
                        file.Declarations
                        |> Seq.choose (function
                            | Fable.EntityDeclaration(ent, privName, _, _) when ent.Name <> privName ->
                                Some(ent.Name, privName)
                            | _ -> None)
                        |> Map
                }
                let rootDecls =
                    com.DeclarePlugins
                    |> Plugins.tryPlugin (Some file.Range) (fun p ->
                        p.TryDeclareRoot com ctx file)
                    |> function
                    | Some rootDecls -> rootDecls
                    | None -> transformModDecls com ctx declareRootModMember None file.Declarations
                // Add imports
                let rootDecls, dependencies =
                    let dependencies = HashSet()
                    com.GetAllImports()
                    |> Seq.mapi (fun ident import ->
                        import.internalFile |> Option.iter (dependencies.Add >> ignore)
                        let localId = Babel.Identifier(import.localIdent)
                        let specifier =
                            match import.selector with
                            | "default" | "" -> Babel.ImportDefaultSpecifier(localId) |> U3.Case2
                            | "*" -> Babel.ImportNamespaceSpecifier(localId) |> U3.Case3
                            | memb -> Babel.ImportSpecifier(localId, Babel.Identifier memb) |> U3.Case1
                        import.path, specifier)
                    |> Seq.groupBy (fun (path, _) -> path)
                    |> Seq.collect (fun (path, specifiers) ->
                        let mems, defs, alls =
                            (([], [], []), Seq.map snd specifiers)
                            ||> Seq.fold (fun (mems, defs, alls) x ->
                                match x with
                                | U3.Case1 _ -> x::mems, defs, alls
                                | U3.Case2 _ -> mems, x::defs, alls
                                | U3.Case3 _ -> mems, defs, x::alls)
                        [mems; defs; alls]
                        |> Seq.choose (function
                            | [] -> None
                            | specifiers ->
                                Babel.ImportDeclaration(specifiers, Babel.StringLiteral path)
                                :> Babel.ModuleDeclaration |> U2.Case2 |> Some))
                    |> Seq.toList |> fun importDecls ->
                        (importDecls@rootDecls), Seq.toList dependencies
                dependenciesDic.AddOrUpdate(
                    Path.normalizeFullPath file.SourceFile,
                    (fun _ -> dependencies), (fun _ _ -> dependencies))
                |> ignore
                let jsIncludes = com.GetAllJsIncludes() |> Seq.toList
                prevJsIncludes.AddRange(jsIncludes)
                // Return the Babel file
                Babel.Program(file.TargetFile, file.SourceFile, jsIncludes,
                                file.Range, rootDecls, isEntry=file.IsEntry)
            with
            | :? FableError as e -> FableError(e.Message, ?range=e.Range, file=file.SourceFile) |> raise
            | ex -> exn (sprintf "%s (%s)" ex.Message file.SourceFile, ex) |> raise
        )
        |> fun seq ->
            let extra = Map.add "dependencies" (box dependenciesDic) extra
            extra, seq
