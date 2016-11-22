module Fable.Fable2Babel

open Fable
open Fable.AST
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
    fixedFileName: string
    moduleFullName: string
    rootEntitiesPrivateNames: Map<string, string>
}

type IBabelCompiler =
    inherit ICompiler
    abstract DeclarePlugins: (string*IDeclarePlugin) list
    abstract GetProjectAndNamespace: string -> Fable.Project * string
    abstract GetImportExpr: Context -> internalFile: string option -> selector: string -> path: string -> Babel.Expression
    abstract GetAllImports: unit -> seq<Import>
    abstract TransformExpr: Context -> Fable.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fable.Expr -> Babel.Statement
    abstract TransformExprAndResolve: Context -> ReturnStrategy -> Fable.Expr -> Babel.Statement
    abstract TransformFunction: Context -> Fable.Ident list -> Fable.Expr ->
        (Babel.Pattern list) * U2<Babel.BlockStatement, Babel.Expression>
    abstract TransformClass: Context -> SourceLocation option -> Fable.Expr option ->
        Fable.Declaration list -> Babel.ClassExpression
        
and IDeclarePlugin =
    inherit IPlugin
    abstract member TryDeclare:
        com: IBabelCompiler -> ctx: Context -> decl: Fable.Declaration
        -> (Babel.Statement list) option
    abstract member TryDeclareRoot:
        com: IBabelCompiler -> ctx: Context -> file: Fable.File
        -> (U2<Babel.Statement, Babel.ModuleDeclaration> list) option

module Util =
    let (|EntKind|) (ent: Fable.Entity) = ent.Kind
    let (|ExprType|) (fexpr: Fable.Expr) = fexpr.Type
    let (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
    let (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e

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
        Babel.Identifier id.name

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

    let getCoreLibImport (com: IBabelCompiler) (ctx: Context) coreModule =
        com.Options.coreLib
        |> Path.getExternalImportPath com ctx.fixedFileName
        |> com.GetImportExpr ctx None coreModule

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

    let typeRef (com: IBabelCompiler) ctx (ent: Fable.Entity) (memb: string option): Babel.Expression =
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
            match Map.tryFind ent.FullName Replacements.coreLibMappedTypes with
            | Some mappedType ->
                Path.getExternalImportPath com ctx.fixedFileName com.Options.coreLib
                |> com.GetImportExpr ctx None mappedType
            | None -> failwithf "Cannot access type: %s" ent.FullName
        | Some file when ctx.file.FileName <> file ->
            let proj, ns = com.GetProjectAndNamespace file
            let importPath =
                match proj.ImportPath with
                | Some importPath ->
                    let ext = Path.getExternalImportPath com ctx.fixedFileName importPath
                    let rel = Path.getRelativeFileOrDirPath true proj.BaseDir false file
                    System.IO.Path.Combine(ext, rel)
                    |> Path.normalizePath
                    |> fun x -> System.IO.Path.ChangeExtension(x, null)
                | None ->
                    Path.fixExternalPath com file
                    |> Path.getRelativePath ctx.fixedFileName
                    |> fun x -> "./" + System.IO.Path.ChangeExtension(x, null)
            getParts ns ent.FullName memb
            |> function
            | [] -> com.GetImportExpr ctx (Some file) "*" importPath
            | memb::parts ->
                com.GetImportExpr ctx (Some file) memb importPath
                |> Some |> accessExpr parts
        | _ ->
            match getParts ctx.moduleFullName ent.FullName memb with
            | rootMemb::parts when Naming.identForbiddenCharsRegex.IsMatch rootMemb ->
                // Check if the root entity is represented internally with a private name
                if ctx.rootEntitiesPrivateNames.ContainsKey(rootMemb)
                then ctx.rootEntitiesPrivateNames.[rootMemb]
                else rootMemb
                |> fun rootMemb -> accessExpr (rootMemb::parts) None
            | parts -> accessExpr parts None

    let rec typeAnnotation com ctx typ: Babel.TypeAnnotationInfo =
        let (|FullName|) (ent: Fable.Entity) = ent.FullName
        match typ with
        | Fable.Unit -> upcast Babel.VoidTypeAnnotation()
        | Fable.Boolean -> upcast Babel.BooleanTypeAnnotation()
        | Fable.String -> upcast Babel.StringTypeAnnotation()
        | Fable.Regex -> upcast Babel.GenericTypeAnnotation(Babel.Identifier("RegExp"))
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
        | Fable.DeclaredType(FullName "Microsoft.FSharp.Core.FSharpOption", [genArg]) ->
            upcast Babel.NullableTypeAnnotation(typeAnnotation com ctx genArg)
        | Fable.DeclaredType(FullName "System.Collections.Generic.IEnumerable", [genArg]) ->
            upcast Babel.GenericTypeAnnotation(
                Babel.Identifier("Iterable"),
                Babel.TypeParameterInstantiation([typeAnnotation com ctx genArg]))
        | Fable.DeclaredType(FullName "System.DateTime", _) ->
            upcast Babel.GenericTypeAnnotation(Babel.Identifier("Date"))
        | Fable.DeclaredType(FullName "System.TimeSpan", _) ->
            upcast Babel.NumberTypeAnnotation()
        | Fable.DeclaredType(ent, genArgs) ->
            try
                match typeRef com ctx ent None with
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

    let buildFields com ctx (ent: Fable.Entity)  =
        match ent.Kind with
        | Fable.Record fields ->
           fields |> List.map(fun (n,t) -> n, t)
        | Fable.Union (cases) ->
           cases |> List.map(fun (n,fs) -> "UnionCase " + n, Fable.Tuple fs)
        | _ -> []
        
        |> List.append (ent.GetProperties() |> List.map(fun (n,t) -> n, t))
        |> List.map(fun (n,t) -> 
            let rec convertType (tp: Fable.Type) = 
                if tp.FullName.EndsWith("[]") || List.length tp.GenericArgs = 0 then tp.FullName
                else tp.FullName + "[" + (tp.GenericArgs |> List.map(fun a -> "[" + convertType a + "]") |> String.concat "," )  + "]"

            [ Babel.StringLiteral n :> Babel.Expression |> U2.Case1 |> Some
              Babel.StringLiteral (convertType t) :> Babel.Expression |> U2.Case1 |> Some ]
            |> Babel.ArrayExpression :> Babel.Expression
            |> U2.Case1 |> Some
        )
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
                            Babel.TypeAnnotation(typeAnnotation com ctx args.[i].typ))
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
        | Fable.ImportRef (memb, path) ->
            let memb, parts =
                let parts = Array.toList(memb.Split('.'))
                parts.Head, parts.Tail
            Path.getExternalImportPath com ctx.fixedFileName path
            |> com.GetImportExpr ctx None memb
            |> Some |> accessExpr parts
        | Fable.This -> upcast Babel.ThisExpression ()
        | Fable.Super -> upcast Babel.Super ()
        | Fable.Null -> upcast Babel.NullLiteral ()
        | Fable.IdentValue {name=name} -> upcast Babel.Identifier (name)
        | Fable.NumberConst (x,_) -> upcast Babel.NumericLiteral x
        | Fable.StringConst x -> upcast Babel.StringLiteral (x)
        | Fable.BoolConst x -> upcast Babel.BooleanLiteral (x)
        | Fable.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (source, flags)
        | Fable.Lambda (args, body) ->
            let args, body = com.TransformFunction ctx args body
            // It's important to use arrow functions to lexically bind `this`
            upcast Babel.ArrowFunctionExpression (args, body, ?loc=r)
        | Fable.ArrayConst (cons, typ) -> buildArray com ctx cons typ
        | Fable.TupleConst vals -> buildArray com ctx (Fable.ArrayValues vals) Fable.Any
        | Fable.Emit emit -> macroExpression None emit []
        | Fable.TypeRef typEnt -> typeRef com ctx typEnt None
        | Fable.Spread _ ->
            failwithf "Unexpected array spread %O" r
        | Fable.LogicalOp _ | Fable.BinaryOp _ | Fable.UnaryOp _ -> 
            failwithf "Unexpected stand-alone operator detected %O" r

    let transformObjectExpr (com: IBabelCompiler) ctx
                            (members, interfaces, baseClass, range): Babel.Expression =
        match baseClass with
        | Some _ as baseClass ->
            members
            |> com.TransformClass ctx range baseClass
            |> fun c -> upcast Babel.NewExpression(c, [], ?loc=range)
        | None ->
            members |> List.choose (function
                | Fable.EntityDeclaration _ | Fable.ActionDeclaration _ -> None // Shouldn't happen
                | Fable.MemberDeclaration(m, _, args, body, r) ->
                    let makeMethod kind name =
                        let name, computed = sanitizeName name
                        let args, body, returnType, typeParams =
                            getMemberArgs com ctx args body m.GenericParameters m.HasRestParams
                        Babel.ObjectMethod(kind, name, args, body, ?returnType=returnType,
                            ?typeParams=typeParams, computed=computed, ?loc=Some r)
                        |> U3.Case2
                    match m.Kind with
                    | Fable.Constructor ->
                        "Unexpected constructor in Object Expression"
                        |> Fable.Util.attachRange range |> failwith
                    | Fable.Method -> makeMethod Babel.ObjectMeth m.Name
                    | Fable.Setter -> makeMethod Babel.ObjectSetter m.Name
                    | Fable.Getter -> makeMethod Babel.ObjectGetter m.Name
                    | Fable.Field ->
                        let key, _ = sanitizeName m.Name
                        Babel.ObjectProperty(key, com.TransformExpr ctx body, ?loc=Some r) |> U3.Case1
                    |> Some)
            |> fun props ->
                match interfaces with
                | [] -> props
                | interfaces ->
                    let ifcsSymbol =
                        getCoreLibImport com ctx "Symbol" 
                        |> get <| "interfaces"
                    Babel.ObjectProperty(ifcsSymbol, buildStringArray interfaces, computed=true)
                    |> U3.Case1 |> consBack props
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
        | Fable.Value (Fable.TypeRef typEnt), [Fable.Value (Fable.StringConst memb)]
            when kind = Fable.ApplyGet ->
            typeRef com ctx typEnt (Some memb)
        | _ ->
            match kind with
            | Fable.ApplyMeth ->
                Babel.CallExpression (com.TransformExpr ctx callee, prepareArgs com ctx args, ?loc=range)
                :> Babel.Expression
            | Fable.ApplyCons ->
                Babel.NewExpression (com.TransformExpr ctx callee, prepareArgs com ctx args, ?loc=range)
                :> Babel.Expression
            | Fable.ApplyGet ->
                getExpr com ctx callee args.Head

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
        failwithf "Quotations are not supported %O" r
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

        | Fable.VarDeclaration (var, Fable.Value(Fable.ImportRef(Naming.placeholder, path)), isMutable) ->
            let value = com.GetImportExpr ctx None var.name path
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
                if isNull elseStatement
                then None else Some(transformBlock com ctx None elseStatement :> Babel.Statement) 
            upcast Babel.IfStatement(guardExpr, thenStatement,
                            ?alternate=elseStatement, ?loc=range)

        | Fable.Switch(TransformExpr com ctx matchValue, cases, defaultBranch, _, range) ->
            let transformBranch test branch =
                let block = transformBlock com ctx None branch
                match test with
                | Some(TransformExpr com ctx test) ->
                    Babel.SwitchCase(block.body@[Babel.BreakStatement()], test, ?loc=block.loc)
                // Default branch
                | None -> Babel.SwitchCase(block.body, ?loc=block.loc)
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
        
        | Fable.ObjExpr (members, interfaces, baseClass, range) ->
            transformObjectExpr com ctx (members, interfaces, baseClass, range) 
        
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
        
        | Fable.ObjExpr (members, interfaces, baseClass, range) ->
            transformObjectExpr com ctx (members, interfaces, baseClass, range)
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
                if isNull elseStatement
                then None else Some(transformBlock com ctx (Some ret) elseStatement :> Babel.Statement) 
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
            | Fable.Throw _ | Fable.DebugBreak _ | Fable.Loop _ ->
                transformBlock com ctx None body |> U2.Case1
            | Fable.Sequential _ | Fable.TryCatch _ ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | _ -> transformExpr com ctx body |> U2.Case2
        args, body
        
    let transformClass com ctx range (ent: Fable.Entity option) baseClass decls =
        let declareProperty com ctx name typ =
            let typ = Babel.TypeAnnotation(typeAnnotation com ctx typ)
            Babel.ClassProperty(Babel.Identifier(name), typeAnnotation=typ)
            |> U2<Babel.ClassMethod,_>.Case2
        let declareMethod range kind name args (body: Fable.Expr) typeParams hasRestParams isStatic =
            let name, computed = sanitizeName name
            let args, body, returnType, typeParams =
                getMemberArgs com ctx args body typeParams hasRestParams
            Babel.ClassMethod(kind, name, args, body, computed, isStatic,
                ?returnType=returnType, ?typeParams=typeParams, loc=range)
            |> U2<_,Babel.ClassProperty>.Case1
        let baseClass = baseClass |> Option.map (transformExpr com ctx)
        decls
        |> List.map (function
            | Fable.MemberDeclaration(m, _, args, body, range) ->
                let kind, name, isStatic, body =
                    match m.Kind with
                    | Fable.Constructor ->
                        let body =
                            match ent with
                            | Some(EntKind(Fable.Class(Some _))) -> checkBaseCall body
                            | _ -> body
                        Babel.ClassConstructor, "constructor", false, body
                    | Fable.Method -> Babel.ClassFunction, m.OverloadName, m.IsStatic, body
                    | Fable.Getter | Fable.Field -> Babel.ClassGetter, m.Name, m.IsStatic, body
                    | Fable.Setter -> Babel.ClassSetter, m.Name, m.IsStatic, body
                declareMethod range kind name args body m.GenericParameters m.HasRestParams isStatic
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

    let declareInterfaces (com: IBabelCompiler) ctx (ent: Fable.Entity) isClass =
        ent.Interfaces
        |> Seq.tryFind (Naming.replacedInterfaces.Contains)
        |> Option.iter (fun i ->
            failwithf "Fable doesn't support custom implementations of %s (%s)" i ent.FullName)
        let interfaces =
            match ent.Kind with
            | Fable.Union _ -> "FSharpUnion"::ent.Interfaces
            | Fable.Record _ -> "FSharpRecord"::ent.Interfaces
            | Fable.Exception _ -> "FSharpException"::ent.Interfaces
            | _ -> ent.Interfaces
        [ getCoreLibImport com ctx "Util"
          typeRef com ctx ent None
          buildStringArray interfaces
          upcast Babel.StringLiteral ent.FullName
          buildFields com ctx ent ]
        |> fun args ->
            // "$0.setInterfaces($1.prototype, $2, $3)"
            Babel.CallExpression(
                get args.[0] "setInterfaces",
                [get args.[1] "prototype"; args.[2]; args.[3]; args.[4]] |> List.map U2.Case1)
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
                macroExpression (Some range) macro [modIdent; identFromName privateName; expr]
            else
                assign (Some range) (get modIdent publicName) expr
        | _ -> expr
        |> varDeclaration (Some range) (identFromName privateName) isMutable :> Babel.Statement
        |> U2.Case1 |> List.singleton

    let declareRootModMember range publicName privateName isPublic isMutable _
                             (expr: Babel.Expression) =
        let privateName = defaultArg privateName publicName
        let privateIdent = identFromName privateName
        let decl: Babel.Declaration =
            match expr with
            | :? Babel.ClassExpression as e when e.id.IsSome ->
                upcast Babel.ClassDeclaration(e.body, e.id.Value,
                    ?super=e.superClass, ?typeParams=e.typeParameters, ?loc=e.loc)
            | :? Babel.FunctionExpression as e when e.id.IsSome ->
                upcast Babel.FunctionDeclaration(e.id.Value, e.``params``, e.body,
                    ?returnType=e.returnType, ?typeParams=e.typeParameters, ?loc=e.loc)
            | _ -> upcast varDeclaration (Some range) privateIdent isMutable expr
        match isPublic with
        | false -> U2.Case1 (decl :> Babel.Statement) |> List.singleton
        | true when publicName = privateName ->
            Babel.ExportNamedDeclaration(decl, loc=range)
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
                | Fable.Value(Fable.ImportRef(Naming.placeholder, path)) ->
                    com.GetImportExpr ctx None m.Name path
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
            match expr.loc with Some loc -> range + loc | None -> range
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
            transformClass com ctx (Some entRange) (Some ent) baseClass entDecls
            |> declareMember entRange ent.Name (Some privateName) ent.IsPublic false modIdent
        let classDecl =
            declareInterfaces com ctx ent isClass
            |> fun ifcDecl -> (U2.Case1 ifcDecl)::classDecl
        // Check if there's a static constructor
        entDecls |> Seq.exists (function
            | Fable.MemberDeclaration(m,_,_,_,_) ->
                match m.Kind, m.Name with
                | Fable.Method, ".cctor" when m.IsStatic -> true
                | _ -> false
            | _ -> false)
        |> function
        | false -> classDecl
        | true ->
            let cctor = Babel.MemberExpression(
                            typeRef com ctx ent None, Babel.StringLiteral ".cctor", true)
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
                block (Some entRange) modDecls, ?loc=Some entRange),
            [U2.Case1 (upcast Babel.ObjectExpression [])],
            entRange)

    and transformModDecls (com: IBabelCompiler) ctx declareMember modIdent decls =
        let pluginDeclare decl =
            com.DeclarePlugins |> Seq.tryPick (fun (path, plugin) ->
                try plugin.TryDeclare com ctx decl
                with ex -> failwithf "Error in plugin %s: %s (%O)"
                            path ex.Message decl.Range)
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
                    (declareInterfaceEntity com ent)@acc
                | Fable.Class baseClass ->
                    let baseClass = Option.map snd baseClass
                    declareClass com ctx declareMember modIdent
                        ent privateName entDecls entRange baseClass true
                    |> List.append <| acc
                | Fable.Union _ | Fable.Record _ | Fable.Exception _ ->                
                    declareClass com ctx declareMember modIdent
                        ent privateName entDecls entRange None false
                    |> List.append <| acc
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
            
    let makeCompiler (com: ICompiler) (projs: Fable.Project list) =
        let imports = Dictionary<string*string,Import>()
        let declarePlugins =
            com.Plugins |> List.choose (function
                | path, (:? IDeclarePlugin as plugin) -> Some (path, plugin)
                | _ -> None)
        { new IBabelCompiler with
            member bcom.DeclarePlugins =
                declarePlugins
            member bcom.GetProjectAndNamespace fileName =
                projs
                |> Seq.tryPick (fun p ->
                    match Map.tryFind fileName p.FileMap with
                    | None -> None
                    | Some ns -> Some(p, ns))
                |> function
                | Some res -> res
                | None -> failwithf "Cannot find file: %s" fileName
            // TODO: Create a cache to optimize imports
            member bcom.GetImportExpr ctx internalFile selector path =
                let sanitizeSelector selector =
                    if selector = "*"
                    then selector
                    elif selector = Naming.placeholder
                    then failwith "importMember must be assigned to a variable"
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
                match imports.TryGetValue((selector, path)) with
                | true, i -> upcast Babel.Identifier(i.localIdent)
                | false, _ ->
                    let localId = getLocalIdent ctx selector
                    let i = {
                        selector = sanitizeSelector selector
                        path = path
                        internalFile = internalFile
                        localIdent = localId
                    }
                    imports.Add((selector,path), i)
                    upcast Babel.Identifier (localId)
            member bcom.GetAllImports () = upcast imports.Values                     
            member bcom.TransformExpr ctx e = transformExpr bcom ctx e
            member bcom.TransformStatement ctx e = transformStatement bcom ctx e
            member bcom.TransformExprAndResolve ctx ret e = transformExprAndResolve bcom ctx ret e
            member bcom.TransformFunction ctx args body = transformFunction bcom ctx args body
            member bcom.TransformClass ctx r baseClass members =
                transformClass bcom ctx r None baseClass members
        interface ICompiler with
            member __.Options = com.Options
            member __.Plugins = com.Plugins
            member __.AddLog msg = com.AddLog msg
            member __.GetLogs() = com.GetLogs()
            member __.GetUniqueVar() = com.GetUniqueVar() }
            
module Compiler =
    open Util
    open System.IO

    let transformFiles (com: ICompiler) (extra: Map<string, obj>, files) =
        let projs: Fable.Project list =
            ("projects", extra)
            ||> Map.findOrRun (fun () -> failwith "Expected project list")  
        let dependenciesDic: Dictionary<string, string list> =
            Map.findOrNew "dependencies" extra
        files |> Seq.map (fun (file: Fable.File) ->
            try
                // let t = PerfTimer("Fable > Babel")
                let com = makeCompiler com projs
                let ctx = {
                    file = file
                    fixedFileName = Path.fixExternalPath com file.FileName
                    moduleFullName = defaultArg (Map.tryFind file.FileName projs.Head.FileMap) ""
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
                    |> Seq.tryPick (fun (path, plugin) ->
                        try plugin.TryDeclareRoot com ctx file
                        with ex -> failwithf "Error in plugin %s: %s (%O)"
                                    path ex.Message file.Range)
                    |> function
                    | Some rootDecls -> rootDecls
                    | None -> transformModDecls com ctx declareRootModMember None file.Declarations
                // Add imports
                let rootDecls, dependencies =
                    com.GetAllImports()
                    |> Seq.mapi (fun ident import ->
                        let localId = Babel.Identifier(import.localIdent)
                        let specifier =
                            match import.selector with
                            | "default" | "" ->
                                Babel.ImportDefaultSpecifier(localId)
                                |> U3.Case2
                            | "*" ->
                                Babel.ImportNamespaceSpecifier(localId)
                                |> U3.Case3
                            | memb ->
                                Babel.ImportSpecifier(localId, Babel.Identifier memb)
                                |> U3.Case1
                        Babel.ImportDeclaration([specifier], Babel.StringLiteral import.path)
                        :> Babel.ModuleDeclaration |> U2.Case2, import.internalFile)
                    |> Seq.toList |> List.unzip
                    |> fun (importDecls, dependencies) ->
                        (importDecls@rootDecls), (List.choose id dependencies |> List.distinct)
                // Files not present in the FileMap are injected, no need for source maps or dependencies
                let originalFileName =
                    if Map.containsKey file.FileName projs.Head.FileMap
                    then
                        dependenciesDic.AddOrUpdate(
                            Path.normalizeFullPath file.FileName,
                            (fun _ -> dependencies), (fun _ _ -> dependencies))
                        |> ignore
                        Some file.FileName
                    else None
                // Return the Babel file
                Babel.Program(Path.fixExternalPath com file.FileName,
                                originalFileName, file.Range, rootDecls)
            with
            | ex -> exn (sprintf "%s (%s)" ex.Message file.FileName, ex) |> raise
        )
        |> fun seq ->
            let extra = Map.add "dependencies" (box dependenciesDic) extra
            extra, seq
