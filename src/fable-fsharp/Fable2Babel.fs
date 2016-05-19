module Fable.Fable2Babel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST

type Context = {
    file: string
    originalFile: string
    moduleFullName: string
    imports: System.Collections.Generic.List<string * string>
    }

type IBabelCompiler =
    inherit ICompiler
    abstract DeclarePlugins: IDeclarePlugin list
    abstract GetProjectAndNamespace: string -> Fable.Project * string
    abstract GetImport: Context -> memb: string -> path: string -> Babel.Expression
    abstract TransformExpr: Context -> Fable.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fable.Expr -> Babel.Statement
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
    let (|ExprType|) (fexpr: Fable.Expr) = fexpr.Type
    let (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
    let (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e
        
    let consBack tail head = head::tail

    let cleanNullArgs args =
        let rec cleanNull = function
            | [] -> []
            | (Fable.Value Fable.Null)::args
            | (Fable.Wrapped(Fable.Value Fable.Null,_))::args -> cleanNull args
            | args -> args
        List.rev args |> cleanNull |> List.rev

    let prepareArgs (com: IBabelCompiler) ctx args =
        cleanNullArgs args
        |> List.map (function
            | Fable.Value (Fable.Spread expr) ->
                Babel.SpreadElement(com.TransformExpr ctx expr) |> U2.Case2
            | _ as expr -> com.TransformExpr ctx expr |> U2.Case1)
        
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
            when Naming.identForbiddenCharsRegex.IsMatch name = false ->
            Babel.Identifier (name) :> Babel.Expression, false
        | TransformExpr com ctx property -> property, true

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
        | None -> failwithf "Cannot reference type: %s" ent.FullName
        | Some file when ctx.originalFile <> file ->
            let proj, ns = com.GetProjectAndNamespace file
            let importPath =
                match proj.ImportPath with
                | Some importPath ->
                    let ext = Naming.getExternalImportPath com ctx.file importPath
                    let rel = Naming.getRelativePath file proj.ProjectFileName
                    System.IO.Path.Combine(ext, rel)
                    |> Naming.normalizePath
                    |> fun x -> System.IO.Path.ChangeExtension(x, null)
                | None ->
                    let file = Naming.fixExternalPath com file
                    Naming.getRelativePath file ctx.file
                    |> fun x -> "./" + System.IO.Path.ChangeExtension(x, null)
            getParts ns ent.FullName memb
            |> function
            | [] -> com.GetImport ctx "*" importPath
            | memb::parts ->
                com.GetImport ctx memb importPath
                |> Some |> accessExpr parts
        | _ ->
            accessExpr (getParts ctx.moduleFullName ent.FullName memb) None

    let buildArray (com: IBabelCompiler) ctx consKind kind =
        match kind with
        | Fable.TypedArray kind ->
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
        | Fable.DynamicArray | Fable.Tuple ->
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
        
    let block (com: IBabelCompiler) ctx range (exprs: Fable.Expr list) =
        let exprs = match exprs with
                    | [Fable.Sequential (statements,_)] -> statements
                    | _ -> exprs
        Babel.BlockStatement (exprs |> List.map (com.TransformStatement ctx), ?loc=range)
        
    let returnBlock e =
        Babel.BlockStatement([Babel.ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)

    let func (com: IBabelCompiler) ctx args body =
        let args, body = com.TransformFunction ctx args body
        let body = match body with U2.Case1 block -> block | U2.Case2 expr -> returnBlock expr
        args, body

    let funcExpression (com: IBabelCompiler) ctx args body =
        let args, body = func com ctx args body
        Babel.FunctionExpression (args, body, ?loc=body.loc)

    let funcDeclaration (com: IBabelCompiler) ctx id args body =
        let args, body = func com ctx args body
        Babel.FunctionDeclaration(id, args, body, ?loc=body.loc)

    // It's important to use arrow functions to lexically bind `this`
    // However, compile them always with a block `x => { return x + 1 }`
    // to prevent problems when transforming do expressions `x => do { var y = 5, x + y }`
    let funcArrow (com: IBabelCompiler) ctx args body =
        let args, body = func com ctx args body
        Babel.ArrowFunctionExpression (args, U2.Case1 body, ?loc=body.loc)
        :> Babel.Expression

    /// Immediately Invoked Function Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        Babel.CallExpression (funcExpression com ctx [] expr, [], ?loc=expr.Range)

    let doExpression r (typ: Fable.Type) block =
        let doExpr = Babel.DoExpression(block, ?loc=r) :> Babel.Expression
        match typ with
        // Wrap closures in a function to prevent naming conflicts
        // when the do expression is resolved (see #115)
        | Fable.PrimitiveType (Fable.Function _) ->
            let block = Babel.BlockStatement([Babel.ReturnStatement(doExpr, ?loc=r)], ?loc=r)
            Babel.CallExpression(Babel.ArrowFunctionExpression([], U2.Case1 block, ?loc=r), [], ?loc=r)
            :> Babel.Expression
        | _ -> doExpr

    let varDeclaration range (var: Babel.Pattern) value =
        Babel.VariableDeclaration (var, value, ?loc=range)
            
    let macroExpression range (txt: string) args =
        Babel.MacroExpression(txt, args, ?loc=range) :> Babel.Expression
        
    let getMemberArgs (com: IBabelCompiler) ctx args body hasRestParams =
        let args, body = com.TransformFunction ctx args body
        let args =
            if not hasRestParams then args else
            let args = List.rev args
            (Babel.RestElement(args.Head) :> Babel.Pattern) :: args.Tail |> List.rev
        let body =
            match body with
            | U2.Case1 e -> e
            | U2.Case2 e -> returnBlock e
        args, body
        // TODO: Optimization: remove null statement that F# compiler adds at the bottom of constructors

    let transformStatement com ctx (expr: Fable.Expr): Babel.Statement =
        match expr with
        | Fable.Loop (loopKind, range) ->
            match loopKind with
            | Fable.While (TransformExpr com ctx guard, body) ->
                upcast Babel.WhileStatement (guard, block com ctx body.Range [body], ?loc=range)
            | Fable.ForOf (var, TransformExpr com ctx enumerable, body) ->
                // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right 
                let var = Babel.VariableDeclaration (ident var)
                upcast Babel.ForOfStatement (
                    U2.Case1 var, enumerable, block com ctx body.Range [body], ?loc=range)
            | Fable.For (var, TransformExpr com ctx start,
                            TransformExpr com ctx limit, body, isUp) ->
                upcast Babel.ForStatement (
                    block com ctx body.Range [body],
                    start |> varDeclaration None (ident var) |> U2.Case1,
                    Babel.BinaryExpression (BinaryOperator.BinaryLessOrEqual, ident var, limit),
                    Babel.UpdateExpression (UpdateOperator.UpdatePlus, false, ident var), ?loc=range)

        | Fable.Set (callee, property, TransformExpr com ctx value, range) ->
            let left =
                match property with
                | None -> com.TransformExpr ctx callee
                | Some property -> getExpr com ctx callee property
            upcast Babel.ExpressionStatement (assign range left value, ?loc = range)

        | Fable.VarDeclaration (var, TransformExpr com ctx value, _isMutable) ->
            varDeclaration expr.Range (ident var) value :> Babel.Statement

        | Fable.TryCatch (body, catch, finalizer, range) ->
            let handler =
                catch |> Option.map (fun (param, body) ->
                    Babel.CatchClause (ident param,
                        block com ctx body.Range [body], ?loc=body.Range))
            let finalizer =
                match finalizer with
                | None -> None
                | Some e -> Some (block com ctx e.Range [e])
            upcast Babel.TryStatement (block com ctx expr.Range [body],
                ?handler=handler, ?finalizer=finalizer, ?loc=range)

        | Fable.Throw (TransformExpr com ctx ex, range) ->
            upcast Babel.ThrowStatement(ex, ?loc=range)

        // Expressions become ExpressionStatements
        | Fable.Value _ | Fable.Apply _ | Fable.ObjExpr _ | Fable.Sequential _
        | Fable.Wrapped _ | Fable.IfThenElse _ ->
            upcast Babel.ExpressionStatement (com.TransformExpr ctx expr, ?loc=expr.Range)

    let transformExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Babel.Expression =
        match expr with
        | Fable.Value kind ->
            match kind with
            | Fable.ImportRef (memb, path) ->
                let memb, parts =
                    let parts = Array.toList(memb.Split('.'))
                    parts.Head, parts.Tail
                Naming.getExternalImportPath com ctx.file path
                |> com.GetImport ctx memb
                |> Some |> accessExpr parts
            | Fable.This -> upcast Babel.ThisExpression ()
            | Fable.Super -> upcast Babel.Super ()
            | Fable.Null -> upcast Babel.NullLiteral ()
            | Fable.IdentValue {name=name} -> upcast Babel.Identifier (name)
            | Fable.NumberConst (x,_) -> upcast Babel.NumericLiteral x
            | Fable.StringConst x -> upcast Babel.StringLiteral (x)
            | Fable.BoolConst x -> upcast Babel.BooleanLiteral (x)
            | Fable.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (source, flags)
            | Fable.Lambda (args, body) -> funcArrow com ctx args body
            | Fable.ArrayConst (cons, kind) -> buildArray com ctx cons kind
            | Fable.Emit emit -> macroExpression None emit []
            | Fable.TypeRef typEnt -> typeRef com ctx typEnt None
            | Fable.Spread _ ->
                failwithf "%s %s %s"
                    "An array is being passed from a function accepting an array to another"
                    "accepting a ParamArray argument in a way that's not directly compilable."
                    "Try inlining the first function."
            | Fable.LogicalOp _ | Fable.BinaryOp _ | Fable.UnaryOp _ -> 
                failwithf "Unexpected stand-alone operator detected: %A" expr 

        | Fable.ObjExpr (members, interfaces, baseClass, range) ->
            match baseClass with
            | Some _ as baseClass ->
                members
                |> List.map Fable.MemberDeclaration
                |> com.TransformClass ctx range baseClass
                |> fun c -> upcast Babel.NewExpression(c, [], ?loc=range)
            | None ->
                members |> List.map (fun m ->
                    let makeMethod kind name =
                        let name, computed = sanitizeName name
                        let args, body = getMemberArgs com ctx m.Arguments m.Body m.HasRestParams
                        Babel.ObjectMethod(kind, name, args, body, computed, ?loc=Some m.Range)
                        |> U3.Case2
                    match m.Kind with
                    | Fable.Constructor ->
                        "Unexpected constructor in Object Expression"
                        |> Fable.Util.attachRange range |> failwith
                    | Fable.Method name -> makeMethod Babel.ObjectMeth name
                    | Fable.Setter name -> makeMethod Babel.ObjectSetter name
                    | Fable.Getter (name, false) -> makeMethod Babel.ObjectGetter name
                    | Fable.Getter (name, true) ->
                        let key, _ = sanitizeName name
                        Babel.ObjectProperty(key, com.TransformExpr ctx m.Body, ?loc=Some m.Range) |> U3.Case1)
                |> fun props ->
                    match interfaces with
                    | [] -> props
                    | interfaces ->
                        let ifcsSymbol =
                            com.GetImport ctx "Symbol" Naming.coreLib
                            |> get <| "interfaces"
                        Babel.ObjectProperty(ifcsSymbol, buildStringArray interfaces, computed=true)
                        |> U3.Case1 |> consBack props
                |> fun props ->
                    upcast Babel.ObjectExpression(props, ?loc=range)
            
        | Fable.Wrapped (expr, _) ->
            com.TransformExpr ctx expr

        | Fable.Apply (callee, args, kind, _, range) ->
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
                cleanNullArgs args
                |> List.map (com.TransformExpr ctx)
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

        | Fable.IfThenElse (TransformExpr com ctx guardExpr,
                            TransformExpr com ctx thenExpr,
                            TransformExpr com ctx elseExpr, range) ->
            upcast Babel.ConditionalExpression (
                guardExpr, thenExpr, elseExpr, ?loc = range)

        | Fable.Sequential (statements, range) ->
            Babel.BlockStatement (statements |> List.map (com.TransformStatement ctx), ?loc=range)
            |> fun block -> doExpression range expr.Type block

        | Fable.Set (callee, property, TransformExpr com ctx value, range) ->
            let left =
                match property with
                | None -> com.TransformExpr ctx callee
                | Some property -> getExpr com ctx callee property
            assign range left value

        | Fable.TryCatch _ | Fable.Throw _ | Fable.Loop _ ->
            upcast (iife com ctx expr)

        | Fable.VarDeclaration _ ->
            "Unexpected variable declaration"
            |> Fable.Util.attachRange expr.Range |> failwith
        
    let transformFunction com ctx args body =
        let args: Babel.Pattern list =
            List.map (fun x -> upcast ident x) args
        let body: U2<Babel.BlockStatement, Babel.Expression> =
            match body with
            | ExprType (Fable.PrimitiveType Fable.Unit) ->
                block com ctx body.Range [body] |> U2.Case1
            | Fable.TryCatch (tryBody, handler, finalizer, tryRange) ->
                let handler =
                    handler |> Option.map (fun (param, body) ->
                        let clause = transformExpr com ctx body |> returnBlock
                        Babel.CatchClause (ident param, clause, ?loc=body.Range))
                let finalizer =
                    finalizer |> Option.map (fun x -> block com ctx x.Range [x])
                let tryBody =
                    transformExpr com ctx tryBody |> returnBlock
                Babel.BlockStatement (
                    [Babel.TryStatement (tryBody, ?handler=handler, ?finalizer=finalizer, ?loc=tryRange)],
                    ?loc = body.Range) |> U2.Case1
            | _ ->
                transformExpr com ctx body |> U2.Case2
        args, body
        
    let transformClass com ctx range ident baseClass decls =
        let declareMember range kind name args body isStatic hasRestParams =
            let name, computed = sanitizeName name
            let args, body = getMemberArgs com ctx args body hasRestParams
            Babel.ClassMethod(kind, name, args, body, computed, isStatic, range)
        let baseClass = baseClass |> Option.map (transformExpr com ctx)
        decls
        |> List.map (function
            | Fable.MemberDeclaration m ->
                let kind, name, isStatic =
                    match m.Kind with
                    | Fable.Constructor -> Babel.ClassConstructor, "constructor", false
                    | Fable.Method name -> Babel.ClassFunction, name, m.IsStatic
                    | Fable.Getter (name, _) -> Babel.ClassGetter, name, m.IsStatic
                    | Fable.Setter name -> Babel.ClassSetter, name, m.IsStatic
                declareMember m.Range kind name m.Arguments m.Body isStatic m.HasRestParams
            | Fable.ActionDeclaration _
            | Fable.EntityDeclaration _ as decl ->
                failwithf "Unexpected declaration in class: %A" decl)
        |> List.map U2<_,Babel.ClassProperty>.Case1
        |> fun meths -> Babel.ClassExpression(Babel.ClassBody(meths, ?loc=range),
                            ?id=ident, ?super=baseClass, ?loc=range)

    let declareInterfaces (com: IBabelCompiler) ctx (ent: Fable.Entity) isClass =
        // TODO: For now, we're ignoring compiler generated interfaces for union and records
        let ifcs = ent.Interfaces |> List.filter (fun x ->
            isClass || (not (Naming.automaticInterfaces.Contains x)))
        if ifcs.Length = 0
        then None
        else [ com.GetImport ctx "Util" Naming.coreLib
               typeRef com ctx ent None
               buildStringArray ifcs ]
            |> macroExpression None "$0.setInterfaces($1.prototype, $2)"
            |> Babel.ExpressionStatement :> Babel.Statement
            |> Some

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
        |> varDeclaration (Some range) (identFromName privateName) :> Babel.Statement
        |> U2.Case1

    let declareRootModMember range name _ isPublic _ modIdent expr =
        if Naming.isInvalidJsIdent name then
            failwithf "%s cannot be used as a member name" name
        let decl =
            varDeclaration (Some range) (identFromName name) expr
            :> Babel.Declaration
        match isPublic with
        | false -> U2.Case1 (decl :> Babel.Statement)
        | true ->
            Babel.ExportNamedDeclaration(decl, loc=range)
            :> Babel.ModuleDeclaration |> U2.Case2

    let transformModMember com ctx declareMember modIdent (m: Fable.Member) =
        let expr, name =
            match m.Kind with
            | Fable.Getter (name, _) ->
                let args, body = transformFunction com ctx [] m.Body
                match body with
                | U2.Case2 e -> e, name
                | U2.Case1 e -> doExpression e.loc m.Body.Type e, name
            | Fable.Method name ->
                upcast funcExpression com ctx m.Arguments m.Body, name
            | Fable.Constructor | Fable.Setter _ ->
                failwithf "Unexpected member in module %O: %A" modIdent m.Kind
        let memberRange =
            match expr.loc with Some loc -> m.Range + loc | None -> m.Range
        if m.TryGetDecorator("EntryPoint").IsSome
        then declareEntryPoint com ctx expr |> U2.Case1
        else declareMember memberRange name m.PrivateName m.IsPublic m.IsMutable modIdent expr
        
    let declareClass com ctx declareMember modIdent
                    (ent: Fable.Entity) entDecls entRange baseClass isClass =
        let classDecl =
            // Don't create a new context for class declarations
            let classIdent = identFromName ent.Name |> Some
            transformClass com ctx (Some entRange) classIdent baseClass entDecls
            |> declareMember entRange ent.Name None ent.IsPublic false modIdent
        let classDecl =
            match declareInterfaces com ctx ent isClass with
            | None -> [classDecl]
            | Some ifcDecl -> (U2.Case1 ifcDecl)::[classDecl]
        // Check if there's a static constructor
        entDecls |> Seq.exists (function
            | Fable.MemberDeclaration m ->
                match m.Kind with
                | Fable.Method ".cctor" when m.IsStatic -> true
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
                Babel.BlockStatement (modDecls, ?loc=Some entRange),
                ?loc=Some entRange),
            [U2.Case1 (upcast Babel.ObjectExpression [])],
            entRange)

    and transformModDecls (com: IBabelCompiler) ctx declareMember modIdent decls =
        let pluginDeclare decl =
            com.DeclarePlugins |> Seq.tryPick (fun plugin -> plugin.TryDeclare com ctx decl)
        decls |> List.fold (fun acc decl ->
            match decl with
            | Patterns.Try pluginDeclare statements ->
                (statements |> List.map U2.Case1) @ acc
            | Fable.ActionDeclaration (e,_) ->
                transformStatement com ctx e
                |> U2.Case1
                |> consBack acc
            | Fable.MemberDeclaration m ->
                match m.Kind with
                | Fable.Constructor | Fable.Setter _ -> acc
                | _ -> transformModMember com ctx declareMember modIdent m |> consBack acc
            | Fable.EntityDeclaration (ent, entDecls, entRange) ->
                match ent.Kind with
                // Interfaces, attribute or erased declarations shouldn't reach this point
                | Fable.Interface ->
                    failwithf "Cannot emit interface declaration: %s" ent.FullName
                | Fable.Class baseClass ->
                    let baseClass = Option.map snd baseClass
                    declareClass com ctx declareMember modIdent ent entDecls entRange baseClass true
                    |> List.append <| acc
                | Fable.Union | Fable.Record | Fable.Exception ->                
                    declareClass com ctx declareMember modIdent ent entDecls entRange None false
                    |> List.append <| acc
                | Fable.Module ->
                    transformNestedModule com ctx ent entDecls entRange
                    |> declareMember entRange ent.Name None ent.IsPublic false modIdent
                    |> consBack acc) []
        |> fun decls ->
            match modIdent with
            | None -> decls
            | Some modIdent ->
                Babel.ReturnStatement modIdent
                :> Babel.Statement |> U2.Case1
                |> consBack decls
            |> List.rev
            
    let makeCompiler (com: ICompiler) (projs: Fable.Project list) =
        let declarePlugins =
            com.Plugins |> List.choose (function
                | :? IDeclarePlugin as plugin -> Some plugin
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
            member bcom.GetImport ctx memb importPath =
                let i =
                    match Seq.tryFindIndex ((=) (memb, importPath)) ctx.imports with
                    | Some i -> i
                    | None ->
                        ctx.imports.Add(memb, importPath)
                        ctx.imports.Count - 1
                upcast Babel.Identifier (Naming.getImportIdent i)
            member bcom.TransformExpr ctx e = transformExpr bcom ctx e
            member bcom.TransformStatement ctx e = transformStatement bcom ctx e
            member bcom.TransformFunction ctx args body = transformFunction bcom ctx args body
            member bcom.TransformClass ctx r baseClass members =
                transformClass bcom ctx r None baseClass members
        interface ICompiler with
            member __.Options = com.Options
            member __.Plugins = com.Plugins }
            
module Compiler =
    open Util
    open System.IO

    let transformFile (com: ICompiler) (projs, files) =
        let com = makeCompiler com projs
        files |> Seq.map (fun (file: Fable.File) ->
            try
                let ctx = {
                    file = Naming.fixExternalPath com file.FileName
                    originalFile = file.FileName
                    moduleFullName = projs.Head.FileMap.[file.FileName]
                    imports = System.Collections.Generic.List<_>()
                }
                let rootDecls =
                    com.DeclarePlugins
                    |> Seq.tryPick (fun plugin -> plugin.TryDeclareRoot com ctx file)
                    |> function
                    | Some rootDecls -> rootDecls
                    | None -> transformModDecls com ctx declareRootModMember None file.Declarations
                // Add imports
                let rootDecls =
                    ctx.imports |> Seq.mapi (fun i (memb, path) ->
                        let localId = Babel.Identifier(Naming.getImportIdent i)
                        let specifier =
                            match memb with
                            | "default" | "" ->
                                Babel.ImportDefaultSpecifier(localId)
                                |> U3.Case2
                            | "*" ->
                                Babel.ImportNamespaceSpecifier(localId)
                                |> U3.Case3
                            | memb ->
                                Babel.ImportSpecifier(localId, Babel.Identifier memb)
                                |> U3.Case1
                        Babel.ImportDeclaration([specifier], Babel.StringLiteral path)
                        :> Babel.ModuleDeclaration |> U2.Case2)
                    |> Seq.toList
                    |> (@) <| rootDecls
                Babel.Program (Naming.fixExternalPath com file.FileName,
                                file.FileName, file.Range, rootDecls)
            with
            | ex -> failwithf "%s (%s)" ex.Message file.FileName)
