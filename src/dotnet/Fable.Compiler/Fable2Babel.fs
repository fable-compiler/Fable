module Fable.Fable2Babel

open Fable
open Fable.AST
open Fable.AST.Babel
open Fable.AST.Fable.Util
open System
open System.Collections.Generic
open System.Text.RegularExpressions

type ReturnStrategy =
    | Return
    | Assign of Expression

type Import = {
    path: string
    selector: string
    localIdent: string option
    internalFile: string option
}

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract ReplaceArgs: bool
    abstract IsRecursiveRef: Fable.Expr -> bool

let private getTailCallArgIds (com: ICompiler) (args: Fable.Ident list) =
    // If some arguments are functions we need to capture the current values to
    // prevent delayed references from getting corrupted, for that we use block-scoped
    // ES2015 variable declarations. See #681
    let replaceArgs =
        args |> List.exists (fun arg ->
            match arg.Type with
            | Fable.Function _ -> true
            | _ -> false)
    replaceArgs, args |> List.map (fun arg -> if replaceArgs then com.GetUniqueVar() else arg.Name)

type ClassTailCallOpportunity(com: ICompiler, name, args: Fable.Ident list) =
    let replaceArgs, argIds = getTailCallArgIds com args
    interface ITailCallOpportunity with
        member x.Label = name
        member x.Args = argIds
        member x.ReplaceArgs = replaceArgs
        member x.IsRecursiveRef(e) =
            match e with
            | Fable.Apply(Fable.Value Fable.This, [Fable.Value(Fable.StringConst name2)], Fable.ApplyGet, _, _) ->
                name = name2
            | _ -> false

type NamedTailCallOpportunity(com: ICompiler, name, args: Fable.Ident list) =
    let replaceArgs, argIds = getTailCallArgIds com args
    interface ITailCallOpportunity with
        member x.Label = name
        member x.Args = argIds
        member x.ReplaceArgs = replaceArgs
        member x.IsRecursiveRef(e) =
            match e with
            | Fable.Value(Fable.IdentValue id) -> name = id.Name
            | _ -> false

type Context = {
    file: Fable.File
    moduleFullName: string
    rootEntitiesPrivateNames: Map<string, string>
    tailCallOpportunity: ITailCallOpportunity option
    mutable isTailCallOptimized: bool
}

type IBabelCompiler =
    inherit ICompiler
    abstract DeclarePlugins: (string*IDeclarePlugin) list
    abstract GetRootModule: string -> string
    abstract GetImportExpr: Context -> selector: string -> path: string ->
        Fable.ImportKind -> Expression
    abstract GetAllImports: unit -> seq<Import>
    abstract TransformExpr: Context -> Fable.Expr -> Expression
    abstract TransformStatement: Context -> Fable.Expr -> Statement list
    abstract TransformExprAndResolve: Context -> ReturnStrategy -> Fable.Expr -> Statement list
    abstract TransformFunction: Context -> ITailCallOpportunity option -> Fable.Ident list -> Fable.Expr ->
        (Pattern list) * U2<BlockStatement, Expression>
    abstract TransformClass: Context -> SourceLocation option -> Fable.Expr option ->
        Fable.Declaration list -> ClassExpression
    abstract TransformObjectExpr: Context -> Fable.ObjExprMember list ->
        Fable.Expr option -> SourceLocation option -> Expression

and IDeclarePlugin =
    inherit IPlugin
    abstract member TryDeclare:
        com: IBabelCompiler -> ctx: Context -> decl: Fable.Declaration
        -> (Statement list) option
    abstract member TryDeclareRoot:
        com: IBabelCompiler -> ctx: Context -> file: Fable.File
        -> (U2<Statement, ModuleDeclaration> list) option

type IDeclareMember =
    abstract member DeclareMember: SourceLocation option * string * string option * bool * bool * Identifier option * Expression -> U2<Statement, ModuleDeclaration> list

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

    let rec deepExists f (expr: Fable.Expr) =
        if f expr
        then true
        else List.exists (deepExists f) expr.ImmediateSubExpressions

    let consBack tail head = head::tail

    let prepareArgs (com: IBabelCompiler) ctx =
        List.map (function
            | Fable.Value (Fable.Spread expr) ->
                SpreadElement(com.TransformExpr ctx expr) |> U2.Case2
            | expr -> com.TransformExpr ctx expr |> U2.Case1)

    let ident (id: Fable.Ident) =
        Identifier id.Name

    let identFromName name =
        let name = Naming.sanitizeIdent (fun _ -> false) name
        Identifier name

    let sanitizeName propName: Expression * bool =
        if Naming.identForbiddenCharsRegex.IsMatch propName
        then upcast StringLiteral propName, true
        else upcast Identifier propName, false

    let sanitizeProp com ctx = function
        | Fable.Value (Fable.StringConst name)
            when not(Naming.identForbiddenCharsRegex.IsMatch name) ->
            Identifier (name) :> Expression, false
        | TransformExpr com ctx property -> property, true

    let getCoreLibImport (com: IBabelCompiler) (ctx: Context) coreModule memb =
        com.GetImportExpr ctx memb coreModule Fable.CoreLib

    let getSymbol com ctx name =
        (getCoreLibImport com ctx "Symbol" "default", Identifier name)
        |> MemberExpression :> Expression

    let get left propName =
        let expr, computed = sanitizeName propName
        MemberExpression(left, expr, computed) :> Expression

    let getExpr com ctx (TransformExpr com ctx expr) (property: Fable.Expr) =
        let property, computed = sanitizeProp com ctx property
        match expr with
        | :? EmptyExpression ->
            match property with
            | :? StringLiteral as lit ->
                identFromName lit.value :> Expression
            | _ -> property
        | _ -> MemberExpression (expr, property, computed) :> Expression

    let rec tryFindMember (ownerName: string) membName entName decls =
        decls |> List.tryPick (function
            | Fable.EntityDeclaration(ent,_,_,subDecls,_) when ownerName.StartsWith(ent.FullName) ->
                tryFindMember ownerName membName ent.FullName subDecls
            | Fable.MemberDeclaration(m,_,privateName,_,_,_) when ownerName = entName && m.Name = membName ->
                Some(m, privateName)
            | _ -> None)

    let rec accessExpr (members: string list) (baseExpr: Expression option) =
        match baseExpr with
        | Some baseExpr ->
            match members with
            | [] -> baseExpr
            | m::ms -> get baseExpr m |> Some |> accessExpr ms
        | None ->
            match members with
            // Temporary placeholder to be deleted by getExpr
            | [] -> upcast EmptyExpression()
            | m::ms -> identFromName m :> Expression |> Some |> accessExpr ms

    let typeRef (com: IBabelCompiler) ctx (ent: Fable.Entity)
                (genArgs: (string*Fable.Expr) list)
                (memb: string option): Expression =
        let makeGeneric expr =
            match genArgs, memb with
            | [], _ -> expr
            | genArgs, None ->
                genArgs |> List.map (fun (name, expr) ->
                    let m = Fable.Member(name, Fable.Field, Fable.InstanceLoc, [], Fable.Any)
                    m, [], expr)
                |> fun ms -> com.TransformObjectExpr ctx ms None None
                |> fun genArgs ->
                    upcast CallExpression(
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
        | Some file when ctx.file.SourcePath <> file ->
            let rootModule = com.GetRootModule(file)
            let importPath = Path.getRelativeFileOrDirPath false ctx.file.SourcePath false file
            getParts rootModule ent.FullName memb
            |> function
            | [] -> com.GetImportExpr ctx "*" importPath (Fable.Internal file)
            | memb::parts ->
                com.GetImportExpr ctx memb importPath (Fable.Internal file)
                |> Some |> accessExpr parts
            |> makeGeneric
        | _ ->
            match getParts ctx.moduleFullName ent.FullName memb with
            | [membName] when Option.isSome memb ->
                // Check if the member has a private name
                match tryFindMember ent.FullName membName ctx.file.Root.FullName ctx.file.Declarations with
                | Some(_, Some privateName) -> accessExpr [privateName] None
                // TODO: Fail if member couldn't be found?
                | _ -> accessExpr [membName] None
            | rootMemb::parts when Naming.identForbiddenCharsRegex.IsMatch rootMemb ->
                // Check if the root entity is represented internally with a private name
                if ctx.rootEntitiesPrivateNames.ContainsKey(rootMemb)
                then ctx.rootEntitiesPrivateNames.[rootMemb]
                else rootMemb
                |> fun rootMemb -> accessExpr (rootMemb::parts) None
            | parts -> accessExpr parts None
            |> makeGeneric

    let rec typeAnnotation com ctx typ: TypeAnnotationInfo =
        let (|FullName|) (ent: Fable.Entity) = ent.FullName
        match typ with
        | Fable.Unit -> upcast VoidTypeAnnotation()
        | Fable.Boolean -> upcast BooleanTypeAnnotation()
        | Fable.String -> upcast StringTypeAnnotation()
        | Fable.Number _ -> upcast NumberTypeAnnotation()
        // TODO: Typed arrays?
        | Fable.Array genArg ->
            upcast GenericTypeAnnotation(
                Identifier("Array"),
                TypeParameterInstantiation([typeAnnotation com ctx genArg]))
        | Fable.Tuple genArgs ->
            List.map (typeAnnotation com ctx) genArgs
            |> TupleTypeAnnotation
            :> TypeAnnotationInfo
        | Fable.Function(argTypes, returnType, _) ->
            argTypes
            |> List.mapi (fun i argType ->
                FunctionTypeParam(
                    Identifier("arg" + (string i)),
                    typeAnnotation com ctx argType))
            |> fun argTypes ->
                FunctionTypeAnnotation(
                    argTypes, typeAnnotation com ctx returnType)
                :> TypeAnnotationInfo
        | Fable.GenericParam name ->
            upcast GenericTypeAnnotation(Identifier(name))
        // TODO: Make union type annotation?
        | Fable.Enum _ ->
            upcast NumberTypeAnnotation()
        | Fable.Option genArg ->
            upcast NullableTypeAnnotation(typeAnnotation com ctx genArg)
        | Fable.DeclaredType(FullName "System.Collections.Generic.IEnumerable", [genArg]) ->
            upcast GenericTypeAnnotation(
                Identifier("Iterable"),
                TypeParameterInstantiation([typeAnnotation com ctx genArg]))
        | Fable.DeclaredType(ent, genArgs) ->
            try
                match typeRef com ctx ent [] None with
                | :? StringLiteral as str ->
                    match str.value with
                    | "number" -> upcast NumberTypeAnnotation()
                    | "boolean" -> upcast BooleanTypeAnnotation()
                    | "string" -> upcast StringTypeAnnotation()
                    | _ -> upcast AnyTypeAnnotation()
                | :? Identifier as id ->
                    let typeParams =
                        match List.map (typeAnnotation com ctx) genArgs  with
                        | [] -> None
                        | xs -> TypeParameterInstantiation(xs) |> Some
                    upcast GenericTypeAnnotation(id, ?typeParams=typeParams)
                // TODO: Resolve references to types in nested modules
                | _ -> upcast AnyTypeAnnotation()
            with
            | _ -> upcast AnyTypeAnnotation()
        | _ -> upcast AnyTypeAnnotation()

    let buildArray (com: IBabelCompiler) ctx consKind typ =
        match typ with
        | Fable.Number kind when com.Options.typedArrays ->
            let cons =
                Fable.Util.getTypedArrayName com kind
                |> Identifier
            let args =
                match consKind with
                | Fable.ArrayValues args ->
                    List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
                    |> ArrayExpression :> Expression |> U2.Case1 |> List.singleton
                | Fable.ArrayAlloc arg ->
                    [U2.Case1 (com.TransformExpr ctx arg)]
            NewExpression(cons, args) :> Expression
        | _ ->
            match consKind with
            | Fable.ArrayValues args ->
                List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
                |> ArrayExpression :> Expression
            | Fable.ArrayAlloc (TransformExpr com ctx arg) ->
                upcast NewExpression(Identifier "Array", [U2.Case1 arg])

    let buildStringArray strings =
        strings
        |> List.map (fun x -> StringLiteral x :> Expression |> U2.Case1 |> Some)
        |> ArrayExpression :> Expression

    let assign range left right =
        AssignmentExpression(AssignEqual, left, right, ?loc=range)
        :> Expression

    /// Immediately Invoked Function Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction ctx None [] expr
        CallExpression(ArrowFunctionExpression ([], body, ?loc=expr.Range), [], ?loc=expr.Range)

    let varDeclaration range (var: Pattern) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration(var, value, kind, ?loc=range)

    let macroExpression range (txt: string) args =
        MacroExpression(txt, args, ?loc=range) :> Expression

    let getMemberArgsAndBody (com: IBabelCompiler) ctx tc args (body: Fable.Expr) typeParams hasRestParams =
        let args', body' = com.TransformFunction ctx tc args body
        let args, returnType, typeParams =
            if com.Options.declaration then
                args' |> List.mapi (fun i arg ->
                    match arg with
                    | :? Identifier as id ->
                        Identifier(id.name,
                            TypeAnnotation(typeAnnotation com ctx args.[i].Type))
                        :> Pattern
                    | arg -> arg),
                TypeAnnotation(typeAnnotation com ctx body.Type) |> Some,
                typeParams |> List.map TypeParameter |> TypeParameterDeclaration |> Some
            else
                args', None, None
        let args =
            if not hasRestParams then args else
            let args = List.rev args
            (RestElement(args.Head) :> Pattern) :: args.Tail |> List.rev
        let body =
            match body' with
            | U2.Case1 e -> e
            | U2.Case2 e -> BlockStatement([ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)
        args, body, returnType, typeParams

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | :? NumericLiteral, _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.Enum _ ->
            BinaryExpression(BinaryOrBitwise, e, NumericLiteral(0.), ?loc=e.loc)
            :> Expression
        | _ -> e

    let transformLambda r (info: Fable.LambdaInfo) args body: Expression =
        if info.CaptureThis
        // Arrow functions capture the enclosing `this` in JS
        then upcast ArrowFunctionExpression (args, body, ?loc=r)
        else
            match body with
            | U2.Case1 body -> body
            | U2.Case2 e -> BlockStatement([ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)
            |> fun body -> upcast FunctionExpression (args, body, ?loc=r)

    let transformValue (com: IBabelCompiler) (ctx: Context) r = function
        | Fable.ImportRef (memb, path, kind) ->
            let memb, parts =
                let parts = Array.toList(memb.Split('.'))
                parts.Head, parts.Tail
            com.GetImportExpr ctx memb path kind
            |> Some |> accessExpr parts
        | Fable.This -> upcast ThisExpression ()
        | Fable.Super -> upcast Super ()
        | Fable.Null -> upcast NullLiteral ()
        | Fable.IdentValue i -> upcast Identifier i.Name
        | Fable.NumberConst (x,_) -> upcast NumericLiteral x
        | Fable.StringConst x -> upcast StringLiteral (x)
        | Fable.BoolConst x -> upcast BooleanLiteral (x)
        | Fable.RegexConst (source, flags) -> upcast RegExpLiteral (source, flags)
        | Fable.Lambda (args, body, info) ->
            com.TransformFunction ctx None args body
            ||> transformLambda r info
        | Fable.ArrayConst (cons, typ) -> buildArray com ctx cons typ
        | Fable.TupleConst vals -> buildArray com ctx (Fable.ArrayValues vals) Fable.Any
        | Fable.Emit emit -> macroExpression None emit []
        | Fable.TypeRef(typEnt, genArgs) -> typeRef com ctx typEnt genArgs None
        | Fable.Spread _ ->
            "Unexpected array spread" |> Fable.Util.attachRange r |> failwith
        | Fable.LogicalOp _ | Fable.BinaryOp _ | Fable.UnaryOp _ ->
            "Unexpected stand-alone operator detected" |> Fable.Util.attachRange r |> failwith

    let transformObjectExpr (com: IBabelCompiler) ctx
                            (members, baseClass, range): Expression =
        match baseClass with
        | Some _ as baseClass ->
            members
            |> List.map (fun (m, args, body: Fable.Expr) ->
                Fable.MemberDeclaration(m, true, None, args, body, body.Range))
            |> com.TransformClass ctx range baseClass
            |> fun c -> upcast NewExpression(c, [], ?loc=range)
        | None ->
            members |> List.map (fun (m: Fable.Member, args, body: Fable.Expr) ->
                let key, computed =
                    match m.Computed with
                    | Some e -> com.TransformExpr ctx e, true
                    | None -> sanitizeName m.Name
                let makeMethod kind =
                    let args, body', returnType, typeParams =
                        let tc =
                            match key with
                            | :? Identifier as id ->
                                ClassTailCallOpportunity(com, id.name, args)
                                :> ITailCallOpportunity |> Some
                            | _ -> None
                        getMemberArgsAndBody com ctx tc args body m.GenericParameters m.HasRestParams
                    ObjectMethod(kind, key, args, body', ?returnType=returnType,
                        ?typeParams=typeParams, computed=computed, ?loc=body.Range)
                    |> U3.Case2
                match m.Kind with
                | Fable.Constructor ->
                    "Unexpected constructor in Object Expression"
                    |> Fable.Util.attachRange range |> failwith
                | Fable.Method -> makeMethod ObjectMeth
                | Fable.Setter -> makeMethod ObjectSetter
                | Fable.Getter -> makeMethod ObjectGetter
                | Fable.Field ->
                    ObjectProperty(key, com.TransformExpr ctx body,
                            computed=computed, ?loc=body.Range)
                    |> U3.Case1)
            |> fun props ->
                upcast ObjectExpression(props, ?loc=range)

    let transformApply com ctx (callee, args, kind, range): Expression =
        let args =
            match args with
            | [unitArg: Fable.Expr] when unitArg.Type = Fable.Unit && unitArg.IsNull -> []
            | args -> args
        match callee, args with
        // Logical, Binary and Unary Operations
        // If the operation has been wrapped in a lambda, there may be arguments in excess,
        // take that into account in matching patterns
        | Fable.Value (Fable.LogicalOp op), (TransformExpr com ctx left)::(TransformExpr com ctx right)::_ ->
            upcast LogicalExpression (op, left, right, ?loc=range)
        | Fable.Value (Fable.UnaryOp op), (TransformExpr com ctx operand as expr)::_ ->
            upcast UnaryExpression (op, operand, ?loc=range)
        | Fable.Value (Fable.BinaryOp op), (TransformExpr com ctx left)::(TransformExpr com ctx right)::_ ->
            upcast BinaryExpression (op, left, right, ?loc=range)
        // Emit expressions
        | Fable.Value (Fable.Emit emit), args ->
            args |> List.map (function
                | Fable.Value(Fable.Spread expr) ->
                    SpreadElement(com.TransformExpr ctx expr, ?loc=expr.Range) :> Node
                | expr -> com.TransformExpr ctx expr :> Node)
            |> macroExpression range emit
        // Module or class static members
        | Fable.Value(Fable.TypeRef(typEnt, _)), [Fable.Value(Fable.StringConst memb)]
            when kind = Fable.ApplyGet ->
            typeRef com ctx typEnt [] (Some memb)
        | _ ->
            match kind with
            | Fable.ApplyMeth ->
                CallExpression (com.TransformExpr ctx callee, prepareArgs com ctx args, ?loc=range)
                :> Expression
            | Fable.ApplyCons ->
                NewExpression (com.TransformExpr ctx callee, prepareArgs com ctx args, ?loc=range)
                :> Expression
            | Fable.ApplyGet ->
                if List.length args = 1
                then getExpr com ctx callee args.Head
                else failwithf "Getter with none or multiple arguments detected at %A" range

    let block r statements =
        BlockStatement(statements, ?loc=r)

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr: BlockStatement =
        match ret with
        | None ->
            com.TransformStatement ctx expr |> block expr.Range
        | Some ret ->
            com.TransformExprAndResolve ctx ret expr |> block expr.Range

    let transformSwitch (com: IBabelCompiler) ctx range returnStrategy (matchValue, cases, defaultCase) =
        let transformCase test branch =
            let statements, test =
                let statements =
                    match returnStrategy with
                    | Some ret -> com.TransformExprAndResolve ctx ret branch
                    | None -> com.TransformStatement ctx branch
                match test with
                | Some(TransformExpr com ctx test) ->
                    match returnStrategy with
                    | Some Return -> statements, Some test
                    | _ -> statements@[BreakStatement()], Some test
                | None -> statements, None // Default branch
            SwitchCase(statements, ?test=test, ?loc=branch.Range)
        let cases =
            cases |> List.collect(fun (tests, branch) ->
                let prev =
                    match List.length tests with
                    | l when l > 1 ->
                        List.take (l - 1) tests
                        |> List.map (fun test ->
                            SwitchCase([], com.TransformExpr ctx test))
                    | _ -> []
                let case = transformCase (List.last tests |> Some) branch
                prev@[case])
        let cases =
            match defaultCase with
            | Some defaultCase -> cases@[transformCase None defaultCase]
            | None -> cases
        SwitchStatement(com.TransformExpr ctx matchValue, cases, ?loc=range)

    let transformTryCatch com ctx range returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with tailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause (ident param,
                    transformBlock com ctx returnStrategy body, ?loc=body.Range))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [TryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer, ?loc=range) :> Statement]

    // Even if IfStatement doesn't enforce it, compile both branches as blocks
    // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
    let rec transformIfStatement (com: IBabelCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
        let guardExpr = com.TransformExpr ctx guardExpr
        let thenStmnt = transformBlock com ctx ret thenStmnt
        let elseStmnt =
            match elseStmnt: Fable.Expr with
            | e when Option.isNone ret && e.IsNull -> None
            | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt, r) ->
                transformIfStatement com ctx r ret guardExpr thenStmnt elseStmnt
                :> Statement |> Some
            | e -> transformBlock com ctx ret e :> Statement |> Some
        IfStatement(guardExpr, thenStmnt, ?alternate=elseStmnt, ?loc=r)

    // TODO: Experimental support for Quotations
    let transformQuote com (ctx: Context) r expr: Expression =
        "Quotations are not supported"
        |> addError com ctx.file.SourcePath r
        upcast NullLiteral ()
        // let rec toJson (expr: obj): Expression =
        //     match expr with
        //     | :? Node ->
        //         expr.GetType().GetProperties()
        //         |> Seq.choose (fun p ->
        //             match p.Name with
        //             | "loc" -> None // Remove location to make the object lighter
        //             | key ->
        //                 let key = StringLiteral key
        //                 let value = p.GetValue(expr) |> toJson
        //                 Some(ObjectProperty(key, value)))
        //         |> Seq.map U3.Case1
        //         |> Seq.toList
        //         |> fun props -> upcast ObjectExpression(props)
        //     | :? bool as expr -> upcast BooleanLiteral(expr)
        //     | :? int as expr -> upcast NumericLiteral(U2.Case1 expr)
        //     | :? float as expr -> upcast NumericLiteral(U2.Case2 expr)
        //     | :? string as expr -> upcast StringLiteral(expr)
        //     | expr when Json.isErasedUnion(expr.GetType()) ->
        //         match Json.getErasedUnionValue expr with
        //         | Some v -> toJson v
        //         | None -> upcast NullLiteral()
        //     | :? System.Collections.IEnumerable as expr ->
        //         let xs = [for x in expr -> U2.Case1(toJson x) |> Some]
        //         upcast ArrayExpression(xs)
        //     | _ -> failwithf "Unexpected expression inside quote %O" fExpr.Range
        // toJson expr

    let transformStatement com ctx (expr: Fable.Expr): Statement list =
        match expr with
        | Fable.Loop (loopKind, range) ->
            match loopKind with
            | Fable.While (TransformExpr com ctx guard, body) ->
                WhileStatement(guard, transformBlock com ctx None body, ?loc=range) :> Statement
            | Fable.ForOf (var, TransformExpr com ctx enumerable, body) ->
                // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right
                let var = VariableDeclaration(ident var, kind=Let)
                ForOfStatement(U2.Case1 var, enumerable, transformBlock com ctx None body, ?loc=range) :> Statement
            | Fable.For (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp) ->
                let op1, op2 =
                    if isUp
                    then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                    else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus
                ForStatement(
                    transformBlock com ctx None body,
                    start |> varDeclaration None (ident var) true |> U2.Case1,
                    BinaryExpression (op1, ident var, limit),
                    UpdateExpression (op2, false, ident var), ?loc=range) :> Statement
            |> List.singleton

        | Fable.Set (callee, property, value, range) ->
            let ret =
                match property with
                | None -> Assign(com.TransformExpr ctx callee)
                | Some property -> Assign(getExpr com ctx callee property)
            com.TransformExprAndResolve ctx ret value

        | Fable.VarDeclaration (var, Fable.Value(Fable.ImportRef(Naming.placeholder, path, kind)), isMutable) ->
            let value = com.GetImportExpr ctx var.Name path kind
            [varDeclaration expr.Range (ident var) isMutable value :> Statement]

        | Fable.VarDeclaration (var, Fable.Value(Fable.Lambda(args, body, info)), false) ->
            let value =
                let tc = NamedTailCallOpportunity(com, var.Name, args) :> ITailCallOpportunity |> Some
                com.TransformFunction ctx tc args body
                ||> transformLambda body.Range info
            [varDeclaration expr.Range (ident var) false value :> Statement]

        | Fable.VarDeclaration (var, value, isMutable) ->
            if value.IsJsStatement
            then
                let var = ident var
                let decl = VariableDeclaration var :> Statement
                let body = com.TransformExprAndResolve ctx (Assign var) value
                decl::body
            else
                let value = com.TransformExpr ctx value |> wrapIntExpression value.Type
                [varDeclaration expr.Range (ident var) isMutable value :> Statement]

        | Fable.TryCatch (body, catch, finalizer, range) ->
            transformTryCatch com ctx range None (body, catch, finalizer)

        | Fable.Throw (TransformExpr com ctx ex, _, range) ->
            [ThrowStatement(ex, ?loc=range) :> Statement]

        | Fable.DebugBreak range ->
            [DebuggerStatement(?loc=range) :> Statement]

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt, range) ->
            [transformIfStatement com ctx range None guardExpr thenStmnt elseStmnt :> Statement ]

        | Fable.Switch(matchValue, cases, defaultCase, _, range) ->
            [transformSwitch com ctx range None (matchValue, cases, defaultCase) :> Statement]

        | Fable.Sequential(statements, range) ->
            statements |> List.collect (com.TransformStatement ctx)

        | Fable.Wrapped (expr, _) ->
            com.TransformStatement ctx expr

        // Expressions become ExpressionStatements
        | Fable.Value _ | Fable.Apply _ | Fable.ObjExpr _ | Fable.Quote _ ->
            [ExpressionStatement (com.TransformExpr ctx expr, ?loc=expr.Range) :> Statement]

    let transformExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Expression =
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
            upcast ConditionalExpression (
                guardExpr, thenExpr, elseExpr, ?loc = range)

        | Fable.Set(callee, property, value, range) ->
            let value = com.TransformExpr ctx value |> wrapIntExpression value.Type
            match property with
            | None -> com.TransformExpr ctx callee
            | Some property -> getExpr com ctx callee property
            |> assign range <| value

        // Optimization: Compile sequential as expression if possible
        | Assignments(exprs, r) ->
            List.map (com.TransformExpr ctx) exprs
            |> fun exprs -> upcast SequenceExpression(exprs, ?loc=r)

        // These cannot appear in expression position in JS
        // They must be wrapped in a lambda
        | Fable.Sequential _ | Fable.TryCatch _ | Fable.Throw _
        | Fable.DebugBreak _ | Fable.Loop _ | Fable.Switch _ ->
            iife com ctx expr :> Expression

        | Fable.VarDeclaration _ ->
            "Unexpected variable declaration"
            |> Fable.Util.attachRange expr.Range |> failwith

        | Fable.Quote quote ->
            transformQuote com ctx expr.Range quote

    let transformExprAndResolve (com: IBabelCompiler) ctx ret
                                 (expr: Fable.Expr): Statement list =
        let resolve strategy expr: Statement =
            match strategy with
            | Return -> upcast ReturnStatement(expr, ?loc=expr.loc)
            | Assign left -> upcast ExpressionStatement(assign expr.loc left expr, ?loc=expr.loc)
        match expr with
        | Fable.Value kind ->
            transformValue com ctx expr.Range kind
            |> wrapIntExpression expr.Type |> resolve ret |> List.singleton

        | Fable.ObjExpr (members, _, baseClass, _) ->
            transformObjectExpr com ctx (members, baseClass, expr.Range)
            |> resolve ret |> List.singleton

        | Fable.Wrapped (TransformExpr com ctx expr, _) ->
            resolve ret expr |> List.singleton

        | Fable.Apply (callee, args, kind, _, range) ->
            match ctx.tailCallOpportunity, kind, ret with
            | Some tc, Fable.ApplyMeth, Return
                when tc.Args.Length = args.Length && tc.IsRecursiveRef callee ->
                ctx.isTailCallOptimized <- true
                let zippedArgs = List.zip tc.Args args
                let tempVars =
                    let rec checkCrossRefs acc = function
                        | [] | [_] -> acc
                        | (argId, arg)::rest ->
                            rest |> List.exists (snd >> deepExists
                                (function Fable.Value(Fable.IdentValue i) -> argId = i.Name | _ -> false))
                            |> function true -> Map.add argId (com.GetUniqueVar()) acc | false -> acc
                            |> checkCrossRefs <| rest
                    checkCrossRefs Map.empty zippedArgs
                [ for (argId, arg) in zippedArgs do
                    let arg = transformExpr com ctx arg
                    match Map.tryFind argId tempVars with
                    | Some tempVar ->
                        yield varDeclaration None (Identifier tempVar) false arg :> Statement
                    | None ->
                        yield assign None (Identifier argId) arg |> ExpressionStatement :> Statement
                  for KeyValue(argId,tempVar) in tempVars do
                    yield assign None (Identifier argId) (Identifier tempVar) |> ExpressionStatement :> Statement
                  yield upcast ContinueStatement(Identifier tc.Label) ]
            | _ ->
                transformApply com ctx (callee, args, kind, range)
                |> wrapIntExpression expr.Type |> resolve ret |> List.singleton

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt, range) ->
            [transformIfStatement com ctx range (Some ret) guardExpr thenStmnt elseStmnt :> Statement ]

        | Fable.Sequential (statements, range) ->
            let lasti = (List.length statements) - 1
            statements |> List.mapi (fun i statement ->
                if i < lasti
                then com.TransformStatement ctx statement
                else com.TransformExprAndResolve ctx ret statement)
            |> List.concat

        | Fable.TryCatch (body, catch, finalizer, range) ->
            transformTryCatch com ctx range (Some ret) (body, catch, finalizer)

        | Fable.Switch(matchValue, cases, defaultCase, _, range) ->
            [transformSwitch com ctx range (Some ret) (matchValue, cases, defaultCase) :> Statement]

        // These cannot be resolved (don't return anything)
        // Just compile as a statement
        | Fable.Throw _ | Fable.DebugBreak _ | Fable.Loop _
        | Fable.Set _ | Fable.VarDeclaration _ ->
            com.TransformStatement ctx expr

        | Fable.Quote quote ->
            transformQuote com ctx expr.Range quote |> resolve ret |> List.singleton

    let transformFunction com ctx tailcallChance (args: Fable.Ident list) (body: Fable.Expr) =
        let ctx, args =
            match args with
            | [unitArg] when unitArg.Type = Fable.Unit ->
                { ctx with isTailCallOptimized = false; tailCallOpportunity = None }, []
            | args ->
                let args = List.map ident args
                { ctx with isTailCallOptimized = false; tailCallOpportunity = tailcallChance }, args
        let body: U2<BlockStatement, Expression> =
            match body with
            | ExprType Fable.Unit
            | Fable.Throw _ | Fable.DebugBreak _ | Fable.Loop _ | Fable.Set _ ->
                transformBlock com ctx None body |> U2.Case1
            | Fable.Sequential _ | Fable.TryCatch _ | Fable.Switch _ ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | Fable.IfThenElse _ when body.IsJsStatement ->
                transformBlock com ctx (Some Return) body |> U2.Case1
            | _ ->
                if Option.isSome tailcallChance
                then transformBlock com ctx (Some Return) body |> U2.Case1
                else transformExpr com ctx body |> U2.Case2
        let args, body =
            match ctx.isTailCallOptimized, tailcallChance, body with
            | true, Some tc, U2.Case1 body ->
                let args, body =
                    if tc.ReplaceArgs
                    then
                        let statements =
                            (List.zip args tc.Args, []) ||> List.foldBack (fun (arg, tempVar) acc ->
                                (varDeclaration None arg false (Identifier tempVar) :> Statement)::acc)
                        tc.Args |> List.map Identifier, block body.loc (statements@body.body)
                    else args, body
                args, LabeledStatement(Identifier tc.Label,
                    WhileStatement(BooleanLiteral true, body, ?loc=body.loc), ?loc=body.loc)
                :> Statement |> List.singleton |> block body.loc |> U2.Case1
            | _ -> args, body
        args |> List.map (fun x -> x :> Pattern), body

    let transformClass com ctx range (ent: Fable.Entity option) baseClass decls =
        let declareProperty (com: IBabelCompiler) ctx name typ =
            let typ =
                if com.Options.declaration
                then TypeAnnotation(typeAnnotation com ctx typ) |> Some
                else None
            ClassProperty(Identifier(name), ?typeAnnotation=typ)
            |> U2<ClassMethod,_>.Case2
        let declareMethod range kind name args (body: Fable.Expr)
                          typeParams hasRestParams isStatic computed =
            let name, computed =
                match computed with
                | Some e -> transformExpr com ctx e, true
                | None -> sanitizeName name
            let args, body, returnType, typeParams =
                let tc =
                    match name with
                    | :? Identifier as id -> ClassTailCallOpportunity(com, id.name, args) :> ITailCallOpportunity |> Some
                    | _ -> None
                getMemberArgsAndBody com ctx tc args body typeParams hasRestParams
            ClassMethod(kind, name, args, body, computed, isStatic,
                ?returnType=returnType, ?typeParams=typeParams, ?loc=range)
            |> U2<_,ClassProperty>.Case1
        let baseClass = baseClass |> Option.map (transformExpr com ctx)
        let interfaces = match ent with | Some e -> e.Interfaces | None -> []
        decls
        |> List.map (function
            | Fable.MemberDeclaration(m, _, _, args, body, range) ->
                let kind, name, loc, computed, body =
                    match m.Kind with
                    | Fable.Constructor -> ClassConstructor, "constructor", Fable.InstanceLoc, None, body
                    | Fable.Method -> ClassFunction, m.OverloadName, m.Location, m.Computed, body
                    | Fable.Getter | Fable.Field -> ClassGetter, m.Name, m.Location, m.Computed, body
                    | Fable.Setter -> ClassSetter, m.Name, m.Location, m.Computed, body
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
                        |> List.map TypeParameter
                        |> TypeParameterDeclaration |> Some
                    let props =
                        match ent.Kind with
                        | Fable.Union _ ->
                            ["tag", (Fable.Number NumberKind.Int32)] // TODO: Add data?
                            |> List.map (fun (name, typ) -> declareProperty com ctx name typ)
                        | Fable.Record fields | Fable.Exception fields ->
                            fields |> List.map (fun (name, typ) -> declareProperty com ctx name typ)
                        | _ -> []
                    typeParams, props@members
                | _ -> None, members
            ClassExpression(ClassBody(members, ?loc=range),
                    ?id=id, ?typeParams=typeParams, ?super=baseClass, ?loc=range)

    let declareType (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        CallExpression(
            getCoreLibImport com ctx "Symbol" "setType",
            [StringLiteral ent.FullName :> Expression |> U2.Case1
            ; typeRef com ctx ent [] None |> U2.Case1])
        |> ExpressionStatement :> Statement

    let declareEntryPoint com ctx (funcExpr: Expression) =
        let argv = macroExpression None "process.argv.slice(2)" []
        let main = CallExpression (funcExpr, [U2.Case1 argv], ?loc=funcExpr.loc) :> Expression
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        ExpressionStatement(main, ?loc=funcExpr.loc) :> Statement

    let declareNestedModMember range publicName privateName isPublic isMutable modIdent expr =
        let privateName = defaultArg privateName publicName
        match isPublic, modIdent with
        | true, Some modIdent -> assign range (get modIdent publicName) expr
        | _ -> expr
        |> varDeclaration range (identFromName privateName) isMutable :> Statement
        |> U2.Case1 |> List.singleton

    let declareRootModMember range publicName privateName isPublic isMutable _
                             (expr: Expression) =
        let privateName = defaultArg privateName publicName
        let privateIdent = identFromName privateName
        let decl: Declaration =
            match expr with
            | :? ClassExpression as e ->
                upcast ClassDeclaration(e.body, privateIdent,
                    ?super=e.superClass, ?typeParams=e.typeParameters, ?loc=e.loc)
            | :? FunctionExpression as e ->
                upcast FunctionDeclaration(privateIdent, e.``params``, e.body,
                    ?returnType=e.returnType, ?typeParams=e.typeParameters, ?loc=e.loc)
            | _ -> upcast varDeclaration range privateIdent isMutable expr
        match isPublic with
        | false -> U2.Case1 (decl :> Statement) |> List.singleton
        | true when publicName = privateName ->
            ExportNamedDeclaration(decl, ?loc=range)
            :> ModuleDeclaration |> U2.Case2 |> List.singleton
        | true ->
            // Replace ident forbidden chars of root members, see #207
            let publicName = Naming.replaceIdentForbiddenChars publicName
            let expSpec = ExportSpecifier(privateIdent, Identifier publicName)
            let expDecl = ExportNamedDeclaration(specifiers=[expSpec])
            [expDecl :> ModuleDeclaration |> U2.Case2; decl :> Statement |> U2.Case1]

    let transformModMember (com: IBabelCompiler) ctx (helper: IDeclareMember) modIdent
                           (m: Fable.Member, isPublic, privName, args, body, range) =
        let expr =
            match m.Kind with
            | Fable.Getter | Fable.Field ->
                transformExpr com ctx body
            | Fable.Method when m.IsMutable ->
                // Mutable module values are compiled as functions, because values
                // imported from ES2015 modules cannot be modified (see #986)
                let expr = transformExpr com ctx body
                let import = getCoreLibImport com ctx "Util" "createAtom"
                upcast CallExpression(import, [U2.Case1 expr])
            | Fable.Method ->
                let bodyRange = body.Range
                let id = defaultArg privName m.OverloadName
                let args, body, returnType, typeParams =
                    let tc = NamedTailCallOpportunity(com, id, args) :> ITailCallOpportunity
                    getMemberArgsAndBody com ctx (Some tc) args body m.GenericParameters false
                // Don't lexically bind `this` (with arrow function) or
                // it will fail with extension members
                upcast FunctionExpression(args, body, ?returnType=returnType, ?typeParams=typeParams, ?loc=bodyRange)
            | Fable.Constructor | Fable.Setter ->
                failwithf "Unexpected member in module %O: %A" modIdent m.Kind
        let memberRange =
            match range, expr.loc with Some r1, Some r2 -> Some(r1 + r2) | _ -> None
        if m.TryGetDecorator("EntryPoint").IsSome
        then declareEntryPoint com ctx expr |> U2.Case1 |> List.singleton
        else helper.DeclareMember(memberRange, m.OverloadName, privName, isPublic, m.IsMutable, modIdent, expr)

    let declareInterfaceEntity (com: IBabelCompiler) (ent: Fable.Entity) =
        // TODO: Add `extends` (inherited interfaces)
        // TODO: Add generic parameters
        // TODO: Add abstract methods
        if not com.Options.declaration then [] else
        let id = Identifier ent.Name
        let body = ObjectTypeAnnotation []
        InterfaceDeclaration(body, id, []) :> Statement
        |> U2.Case1 |> List.singleton

    let declareClass com ctx (helper: IDeclareMember) modIdent
                     (ent: Fable.Entity, isPublic, privateName, entDecls, entRange, baseClass) =
        let classDecl =
            // Don't create a new context for class declarations
            let classExpr = transformClass com ctx entRange (Some ent) baseClass entDecls
            helper.DeclareMember(entRange, ent.Name, Some privateName, isPublic, false, modIdent, classExpr)
        let classDecl =
            (declareType com ctx ent |> U2.Case1)::classDecl
        // Check if there's a static constructor
        entDecls |> Seq.exists (function
            | Fable.MemberDeclaration(m,_,_,_,_,_) ->
                match m.Name, m.Kind, m.Location with
                | ".cctor", Fable.Method, Fable.StaticLoc -> true
                | _ -> false
            | _ -> false)
        |> function
        | false -> classDecl
        | true ->
            let cctor = MemberExpression(
                            typeRef com ctx ent [] None, StringLiteral ".cctor", true)
            ExpressionStatement(CallExpression(cctor, [])) :> Statement
            |> U2.Case1 |> consBack classDecl

    let rec transformNestedModule com ctx (ent: Fable.Entity) entDecls entRange =
        let modIdent = Identifier Naming.exportsIdent
        let modDecls =
            let ctx = { ctx with moduleFullName = ent.FullName }
            let helper =
                { new IDeclareMember with
                    member __.DeclareMember(a,b,c,d,e,f,g) =
                        declareNestedModMember a b c d e f g }
            transformModDecls com ctx helper (Some modIdent) entDecls
            |> List.map (function
                | U2.Case1 statement -> statement
                | U2.Case2 _ -> failwith "Unexpected export in nested module")
        CallExpression(
            FunctionExpression([modIdent],
                block entRange modDecls, ?loc=entRange),
            [U2.Case1 (upcast ObjectExpression [])],
            ?loc=entRange)

    and transformModDecls (com: IBabelCompiler) ctx (helper: IDeclareMember) modIdent decls =
        let pluginDeclare (decl: Fable.Declaration) =
            com.DeclarePlugins
            |> Plugins.tryPlugin decl.Range (fun p -> p.TryDeclare com ctx decl)
        decls |> List.fold (fun acc decl ->
            match decl with
            | Patterns.Try pluginDeclare statements ->
                (statements |> List.map U2.Case1) @ acc
            | Fable.ActionDeclaration (e,_) ->
                transformStatement com ctx e
                |> List.map U2.Case1
                // The accumulated statements will be reverted,
                // so we have to revert these too
                |> List.rev
                |> List.append <| acc
            | Fable.MemberDeclaration(m, isPublic, privName, args, body, r) ->
                match m.Kind with
                | Fable.Constructor | Fable.Setter _ -> acc // Only happens for VS tests
                | _ -> transformModMember com ctx helper modIdent (m,isPublic,privName,args,body,r) @ acc
            | Fable.EntityDeclaration (ent, isPublic, privName, entDecls, entRange) ->
                match ent.Kind with
                | Fable.Interface ->
                    declareInterfaceEntity com ent
                | Fable.Class(baseClass, _) ->
                    let baseClass = baseClass |> Option.map snd
                    declareClass com ctx helper modIdent (ent,isPublic, privName, entDecls, entRange, baseClass)
                | Fable.Exception _ ->
                    let baseClass = Some(Fable.Value(Fable.IdentValue(Fable.Ident("Error"))))
                    declareClass com ctx helper modIdent (ent,isPublic, privName, entDecls, entRange, baseClass)
                | Fable.Union _ | Fable.Record _ ->
                    declareClass com ctx helper modIdent (ent,isPublic, privName, entDecls, entRange, None)
                | Fable.Module ->
                    let m = transformNestedModule com ctx ent entDecls entRange
                    helper.DeclareMember(entRange, ent.Name, Some privName, isPublic, false, modIdent, m)
                |> List.append <| acc) []
        |> fun decls ->
            match modIdent with
            | None -> decls
            | Some modIdent ->
                ReturnStatement modIdent
                :> Statement |> U2.Case1
                |> consBack decls
            |> List.rev

    let makeCompiler (com: ICompiler) (state: ICompilerState) =
        let imports = Dictionary<string,Import>()
        let declarePlugins =
            com.Plugins |> List.choose (function
                | { path=path; plugin=(:? IDeclarePlugin as plugin)} -> Some (path, plugin)
                | _ -> None)
        { new IBabelCompiler with
            member bcom.DeclarePlugins =
                declarePlugins
            member bcom.GetRootModule(file) =
                state.GetRootModule(file)
            member bcom.GetImportExpr ctx selector path kind =
                let sanitizeSelector selector =
                    if selector = "*"
                    then selector
                    elif selector = Naming.placeholder
                    then "`importMember` must be assigned to a variable"
                         |> addError bcom ctx.file.SourcePath None; selector
                    // Replace ident forbidden chars of root members, see #207
                    else Naming.replaceIdentForbiddenChars selector
                let getLocalIdent (ctx: Context) (selector: string) =
                    match selector with
                    | "" -> None
                    | "*" | "default" ->
                        let x = path.TrimEnd('/')
                        x.Substring(x.LastIndexOf '/' + 1) |> Some
                    | selector -> Some selector
                    |> Option.map (Naming.sanitizeIdent (fun s ->
                        ctx.file.UsedVarNames.Contains s
                            || (imports.Values |> Seq.exists (fun i -> i.localIdent = Some s))))
                match kind with
                | Fable.CoreLib when com.Options.fableCore.StartsWith("var ") ->
                    let ident: Expression = upcast Identifier(com.Options.fableCore.[4..])
                    get (get ident path) selector
                | _ ->
                    match imports.TryGetValue(path + "::" + selector) with
                    | true, i ->
                        match i.localIdent with
                        | Some localIdent -> upcast Identifier(localIdent)
                        | None -> upcast NullLiteral ()
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
                                | Fable.CustomImport | Fable.Internal _ -> path
                                | Fable.CoreLib -> com.Options.fableCore + "/" + path + Naming.targetFileExtension
                        }
                        imports.Add(path + "::" + selector, i)
                        match localId with
                        | Some localId -> upcast Identifier(localId)
                        | None -> upcast NullLiteral ()
            member bcom.GetAllImports () = upcast imports.Values
            member bcom.TransformExpr ctx e = transformExpr bcom ctx e
            member bcom.TransformStatement ctx e = transformStatement bcom ctx e
            member bcom.TransformExprAndResolve ctx ret e = transformExprAndResolve bcom ctx ret e
            member bcom.TransformFunction ctx tc args body = transformFunction bcom ctx tc args body
            member bcom.TransformClass ctx r baseClass members =
                transformClass bcom ctx r None baseClass members
            member bcom.TransformObjectExpr ctx membs baseClass r =
                transformObjectExpr bcom ctx (membs, baseClass, r)
        interface ICompiler with
            member __.Options = com.Options
            member __.Plugins = com.Plugins
            member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)
            member __.GetUniqueVar() = com.GetUniqueVar() }

module Compiler =
    open Util
    open System.IO

    let createFacade (dependencies: string[]) (facadeFile: string) =
        let decls =
            let importFile = Array.last dependencies
            StringLiteral(Path.getRelativeFileOrDirPath false facadeFile false importFile)
            |> ExportAllDeclaration :> ModuleDeclaration |> U2.Case2 |> List.singleton
        Program(facadeFile, SourceLocation.Empty, decls, dependencies=dependencies)

    let transformFile (com: ICompiler) (state: ICompilerState) (file: Fable.File) =
        try
            // let t = PerfTimer("Fable > Babel")
            let com = makeCompiler com state
            let ctx = {
                file = file
                tailCallOpportunity = None
                isTailCallOptimized = false
                moduleFullName = state.GetRootModule(file.SourcePath)
                rootEntitiesPrivateNames =
                    file.Declarations
                    |> Seq.choose (function
                        | Fable.EntityDeclaration(ent,_,privName,_,_) when ent.Name <> privName ->
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
                | None ->
                    let helper =
                        { new IDeclareMember with
                            member __.DeclareMember(a,b,c,d,e,f,g) =
                                declareRootModMember a b c d e f g }
                    transformModDecls com ctx helper None file.Declarations
            let dependencies =
                com.GetAllImports()
                |> Seq.choose (fun i -> i.internalFile)
                |> Seq.distinct
                |> Seq.append (file.Dependencies)
                |> Seq.toArray
            // Add imports
            com.GetAllImports()
            |> Seq.mapi (fun ident import ->
                let specifier =
                    import.localIdent
                    |> Option.map (fun localId ->
                        let localId = Identifier(localId)
                        match import.selector with
                        | "*" -> ImportNamespaceSpecifier(localId) |> U3.Case3
                        | "default" | "" -> ImportDefaultSpecifier(localId) |> U3.Case2
                        | memb -> ImportSpecifier(localId, Identifier memb) |> U3.Case1)
                import.path, specifier)
            |> Seq.groupBy (fun (path, _) -> path)
            |> Seq.collect (fun (path, specifiers) ->
                let mems, defs, alls =
                    (([], [], []), Seq.choose snd specifiers)
                    ||> Seq.fold (fun (mems, defs, alls) x ->
                        match x.``type`` with
                        | "ImportNamespaceSpecifier" -> mems, defs, x::alls
                        | "ImportDefaultSpecifier" -> mems, x::defs, alls
                        | _ -> x::mems, defs, alls)
                // There seem to be errors if we mix member, default and namespace imports
                // so we must issue an import statement for each kind
                match [mems; defs; alls] with
                | [[];[];[]] ->
                    // No specifiers, so this is just a import for side effects
                    [ImportDeclaration([], StringLiteral path) :> ModuleDeclaration |> U2.Case2]
                | specifiers ->
                    specifiers |> List.choose (function
                    | [] -> None
                    | specifiers ->
                        ImportDeclaration(specifiers, StringLiteral path)
                        :> ModuleDeclaration |> U2.Case2 |> Some))
            // Return the Babel file
            |> fun importDecls ->
                 Program(file.SourcePath, file.Range, (Seq.toList importDecls)@rootDecls, dependencies=dependencies)
        with
        | ex -> exn (sprintf "%s (%s)" ex.Message file.SourcePath, ex) |> raise
