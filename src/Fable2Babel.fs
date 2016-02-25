module Fable.Fable2Babel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST

type private Context = {
    file: string
    moduleFullName: string
    imports: System.Collections.Generic.Dictionary<string, string * bool>
    }

type private IBabelCompiler =
    inherit ICompiler
    abstract GetFableFile: string -> Fable.File
    abstract GetImport: Context -> bool -> string -> Babel.Expression
    abstract TransformExpr: Context -> Fable.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fable.Expr -> Babel.Statement
    abstract TransformFunction: Context -> Fable.Ident list -> Fable.Expr ->
        (Babel.Pattern list) * U2<Babel.BlockStatement, Babel.Expression>

let private (|ExprType|) (fexpr: Fable.Expr) = fexpr.Type
let private (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
let private (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e

let private (|TestFixture|_|) (decl: Fable.Declaration) =
    match decl with
    | Fable.EntityDeclaration (ent, entDecls, entRange) ->
        match ent.TryGetDecorator "TestFixture" with
        | Some _ -> Some (ent, entDecls, entRange)
        | None -> None
    | _ -> None

let private (|Test|_|) (decl: Fable.Declaration) =
    match decl with
    | Fable.MemberDeclaration m ->
        match m.Kind, m.TryGetDecorator "Test" with
        | Fable.Method name, Some _ -> Some (m, name)
        | _ -> None
    | _ -> None
    
let private consBack tail head = head::tail

let private foldRanges (baseRange: SourceLocation) (decls: Babel.Statement list) =
    decls
    |> Seq.choose (fun x -> x.loc)
    |> Seq.fold (fun _ x -> x) baseRange
    |> (+) baseRange
    
let private prepareArgs (com: IBabelCompiler) ctx args =
    let rec cleanNull = function
        | [] -> []
        | (Fable.Value Fable.Null)::args -> cleanNull args
        | args -> args
    args
    |> List.rev |> cleanNull |> List.rev
    |> List.map (function
        | Fable.Value (Fable.Spread expr) ->
            Babel.SpreadElement(com.TransformExpr ctx expr) |> U2.Case2
        | _ as expr -> com.TransformExpr ctx expr |> U2.Case1)
    
let private ident (id: Fable.Ident) =
    Babel.Identifier id.name

let private identFromName name =
    let name = Naming.sanitizeIdent (fun _ -> false) name
    Babel.Identifier name
    
let private sanitizeName propName: Babel.Expression * bool =
    if Naming.identForbiddenChars.IsMatch propName
    then upcast Babel.StringLiteral propName, true
    else upcast Babel.Identifier propName, false

let private sanitizeProp com ctx = function
    | Fable.Value (Fable.StringConst name)
        when Naming.identForbiddenChars.IsMatch name = false ->
        Babel.Identifier (name) :> Babel.Expression, false
    | TransformExpr com ctx property -> property, true

let private get left propName =
    let expr, computed = sanitizeName propName
    Babel.MemberExpression(left, expr, computed) :> Babel.Expression
    
let private getExpr com ctx (TransformExpr com ctx expr) (property: Fable.Expr) =
    let property, computed = sanitizeProp com ctx property
    match expr with
    | :? Babel.EmptyExpression ->
        match property with
        | :? Babel.StringLiteral as lit when not lit.macro ->
            identFromName lit.value :> Babel.Expression
        | _ -> property
    | _ -> Babel.MemberExpression (expr, property, computed) :> Babel.Expression

let private typeRef (com: IBabelCompiler) ctx file fullName: Babel.Expression =
    let getDiff s1 s2 =
        let split (s: string) =
            s.Split('.') |> Array.toList
        let rec removeCommon (xs1: string list) (xs2: string list) =
            match xs1, xs2 with
            | x1::xs1, x2::xs2 when x1 = x2 -> removeCommon xs1 xs2
            | _ -> xs2
        removeCommon (split s1) (split s2)
    let rec makeExpr (members: string list) (baseExpr: Babel.Expression option) =
        match baseExpr with
        | Some baseExpr ->
            match members with
            | [] -> baseExpr
            | m::ms -> get baseExpr m |> Some |> makeExpr ms 
        | None ->
            match members with
            | [] -> upcast Babel.EmptyExpression()
            | m::ms -> identFromName m :> Babel.Expression |> Some |> makeExpr ms
    match file with
    | None -> failwithf "Cannot reference type: %s" fullName
    | Some file ->
        let file = com.GetFableFile file
        if ctx.file <> file.FileName then
            ctx.file
            |> Naming.getRelativePath file.FileName
            |> fun x -> System.IO.Path.ChangeExtension(x, ".js")
            |> (+) "./"
            |> com.GetImport ctx true
            |> Some
            |> makeExpr (getDiff file.Root.FullName fullName)
        else
            makeExpr (getDiff ctx.moduleFullName fullName) None

let private buildArray (com: IBabelCompiler) ctx consKind kind =
    match kind with
    | Fable.TypedArray kind ->
        let cons =
            match kind with
            | Int8 -> "Int8Array" 
            | UInt8 -> "Uint8Array" 
            | UInt8Clamped -> "Uint8ClampedArray" 
            | Int16 -> "Int16Array" 
            | UInt16 -> "Uint16Array" 
            | Int32 -> "Int32Array" 
            | UInt32 -> "Uint32Array" 
            | Float32 -> "Float32Array"
            | Float64 -> "Float64Array"
            |> Babel.Identifier
        let args =
            match consKind with
            | Fable.ArrayValues args ->
                List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
                |> Babel.ArrayExpression :> Babel.Expression |> U2.Case1 |> List.singleton
            | Fable.ArrayAlloc arg
            | Fable.ArrayConversion arg ->
                [U2.Case1 (com.TransformExpr ctx arg)]
        Babel.NewExpression(cons, args) :> Babel.Expression
    | Fable.DynamicArray | Fable.Tuple ->
        match consKind with
        | Fable.ArrayValues args ->
            List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
            |> Babel.ArrayExpression :> Babel.Expression
        | Fable.ArrayAlloc (TransformExpr com ctx arg) ->
            upcast Babel.NewExpression(Babel.Identifier "Array", [U2.Case1 arg])
        | Fable.ArrayConversion (TransformExpr com ctx arr) ->
            arr

let private buildStringArray strings =
    strings
    |> List.map (fun x -> Babel.StringLiteral x :> Babel.Expression |> U2.Case1 |> Some)
    |> Babel.ArrayExpression :> Babel.Expression

let private assign range left right =
    Babel.AssignmentExpression(AssignEqual, left, right, ?loc=range)
    :> Babel.Expression
    
let private block (com: IBabelCompiler) ctx range (exprs: Fable.Expr list) =
    let exprs = match exprs with
                | [Fable.Sequential (statements,_)] -> statements
                | _ -> exprs
    Babel.BlockStatement (exprs |> List.map (com.TransformStatement ctx), ?loc=range)
    
let private returnBlock e =
    Babel.BlockStatement([Babel.ReturnStatement(e, ?loc=e.loc)], ?loc=e.loc)

let private func (com: IBabelCompiler) ctx args body =
    let args, body = com.TransformFunction ctx args body
    let body = match body with U2.Case1 block -> block | U2.Case2 expr -> returnBlock expr
    args, body

let private funcExpression (com: IBabelCompiler) ctx args body =
    let args, body = func com ctx args body
    Babel.FunctionExpression (args, body, ?loc=body.loc)

let private funcDeclaration (com: IBabelCompiler) ctx id args body =
    let args, body = func com ctx args body
    Babel.FunctionDeclaration(id, args, body, ?loc=body.loc)

let private funcArrow (com: IBabelCompiler) ctx args body =
    let args, body = com.TransformFunction ctx args body
    let range = match body with U2.Case1 x -> x.loc | U2.Case2 x -> x.loc
    Babel.ArrowFunctionExpression (args, body, ?loc=range)
    :> Babel.Expression

/// Immediately Invoked Function Expression
let private iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
    Babel.CallExpression (funcExpression com ctx [] expr, [], ?loc=expr.Range)

let private varDeclaration range (var: Babel.Pattern) value =
    Babel.VariableDeclaration (var, value, ?loc=range)
        
let private macroExpression range (txt: string) args =
    Babel.StringLiteral(txt, macro=true, args=args, ?loc=range)
    :> Babel.Expression
    
let private getMemberArgs (com: IBabelCompiler) ctx args body hasRestParams =
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

let private transformStatement com ctx (expr: Fable.Expr): Babel.Statement =
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

let private transformExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Babel.Expression =
    match expr with
    | Fable.Value kind ->
        match kind with
        | Fable.ImportRef (import, asDefault, prop) ->
            match prop with
            | Some prop -> get (com.GetImport ctx asDefault import) prop
            | None -> com.GetImport ctx asDefault import
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
        | Fable.TypeRef typEnt -> typeRef com ctx typEnt.File typEnt.FullName
        | Fable.LogicalOp _ | Fable.BinaryOp _ | Fable.UnaryOp _ | Fable.Spread _ ->
            failwithf "Unexpected stand-alone value: %A" expr

    | Fable.ObjExpr (members, interfaces, range) ->
        members
        |> List.map (fun m ->
            let makeMethod kind name =
                let name, computed = sanitizeName name
                let args, body = getMemberArgs com ctx m.Arguments m.Body m.HasRestParams
                Babel.ObjectMethod(kind, name, args, body, computed, ?loc=Some m.Range)
                |> U3.Case2
            match m.Kind with
            | Fable.Constructor -> failwithf "Unexpected constructor in Object Expression: %A" range
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
                    get (com.GetImport ctx false (Naming.getCoreLibPath com)) "Symbol"
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
            List.map (com.TransformExpr ctx) args
            |> macroExpression range emit
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
        |> fun block -> upcast Babel.DoExpression (block, ?loc=range)

    | Fable.TryCatch _ | Fable.Throw _ ->
        upcast (iife com ctx expr)

    | Fable.Loop _ | Fable.Set _  | Fable.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr 
    
let private transformFunction com ctx args body =
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
    
let private transformClass com ctx classRange (baseClass: Fable.EntityLocation option) decls =
    let declareMember range kind name args body isStatic hasRestParams =
        let name, computed = sanitizeName name
        let args, body = getMemberArgs com ctx args body hasRestParams
        Babel.ClassMethod(range, kind, name, args, body, computed, isStatic)
    let baseClass = baseClass |> Option.map (fun loc ->
        typeRef com ctx (Some loc.file) loc.fullName)
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
    |> fun meths -> Babel.ClassExpression(classRange, Babel.ClassBody(classRange, meths), ?super=baseClass)

let private declareEntryPoint com ctx (funcExpr: Babel.Expression) =
    let argv = macroExpression None "process.argv.slice(2)" []
    let main = Babel.CallExpression (funcExpr, [U2.Case1 argv], ?loc=funcExpr.loc) :> Babel.Expression
    // Don't exit the process after leaving main, as there may be a server running
    // Babel.ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
    Babel.ExpressionStatement(main, ?loc=funcExpr.loc) :> Babel.Statement

// TODO: Keep track of sanitized member names to be sure they don't clash? 
let private declareModMember range (var, name) isPublic modIdent expr =
    let var = match var with Some x -> x | None -> identFromName name
    match isPublic, modIdent with
    | true, Some modIdent -> assign (Some range) (get modIdent name) expr 
    | _ -> expr
    |> varDeclaration (Some range) var :> Babel.Statement

let private transformModMember com ctx modIdent (m: Fable.Member) =
    let expr, name =
        match m.Kind with
        | Fable.Getter (name, _) ->
            let args, body = transformFunction com ctx [] m.Body
            match body with
            | U2.Case2 e -> e, name
            | U2.Case1 e -> Babel.DoExpression(e, ?loc=e.loc) :> Babel.Expression, name
        | Fable.Method name ->
            upcast funcExpression com ctx m.Arguments m.Body, name
        | Fable.Constructor | Fable.Setter _ ->
            failwithf "Unexpected member in module: %A" m.Kind
    let memberRange =
        match expr.loc with Some loc -> m.Range + loc | None -> m.Range
    if m.TryGetDecorator("EntryPoint").IsSome
    then declareEntryPoint com ctx expr
    else declareModMember memberRange (None, name) m.IsPublic modIdent expr

// Compile tests using Mocha.js BDD interface
let private transformTest com ctx (test: Fable.Member) name =
    let testName =
        Babel.StringLiteral name :> Babel.Expression
    let testBody =
        funcExpression com ctx test.Arguments test.Body :> Babel.Expression
    let testRange =
        match testBody.loc with
        | Some loc -> test.Range + loc | None -> test.Range
    // it('Test name', function() { /* Tests */ });
    Babel.ExpressionStatement(
        Babel.CallExpression(Babel.Identifier "it",
            [U2.Case1 testName; U2.Case1 testBody], testRange), testRange)
    :> Babel.Statement

let private transformTestFixture com ctx (fixture: Fable.Entity) testDecls testRange =
    let testDesc =
        Babel.StringLiteral fixture.Name :> Babel.Expression
    let testBody =
        Babel.FunctionExpression([],
            Babel.BlockStatement (testDecls, ?loc=Some testRange), ?loc=Some testRange)
        :> Babel.Expression
    Babel.ExpressionStatement(
        Babel.CallExpression(Babel.Identifier "describe",
            [U2.Case1 testDesc; U2.Case1 testBody],
            testRange)) :> Babel.Statement
                
let rec private transformModule com ctx modIdent (ent: Fable.Entity) entDecls entRange =
    let nestedIdent, protectedIdent =
        let memberNames =
            entDecls |> Seq.choose (function
                | Fable.EntityDeclaration (ent,_,_) -> Some ent.Name
                | Fable.ActionDeclaration ent -> None
                | Fable.MemberDeclaration m ->
                    match m.Kind with
                    | Fable.Method name | Fable.Getter (name, _) -> Some name
                    | Fable.Constructor | Fable.Setter _ -> None)
            |> Set.ofSeq
        identFromName ent.Name,
        // Protect module identifier against members with same name
        Babel.Identifier (Naming.sanitizeIdent memberNames.Contains ent.Name)
    let nestedDecls =
        let ctx = { ctx with moduleFullName = ent.FullName }
        transformModDecls com ctx (Some protectedIdent) entDecls
    let nestedRange =
        foldRanges entRange nestedDecls
    Babel.CallExpression(
        Babel.FunctionExpression([protectedIdent],
            Babel.BlockStatement (nestedDecls, ?loc=Some nestedRange),
            ?loc=Some nestedRange),
        [U2.Case1 (upcast Babel.ObjectExpression [])],
        nestedRange)
    // var NestedMod = ParentMod.NestedMod = function (/* protected */ NestedMod_1) {
    //     var privateVar = 1;
    //     var publicVar = NestedMod_1.publicVar = 2;
    //     var NestedMod = NestedMod_1.NestedMod = {};
    // }({});
    |> declareModMember nestedRange (Some nestedIdent, ent.Name) ent.IsPublic modIdent

and private transformModDecls com ctx modIdent decls =
    let declareClass com ctx (ent: Fable.Entity) entDecls entRange baseClass isClass =
        // TODO: For now, we're ignoring compiler generated interfaces for union and records
        let ifcs = ent.Interfaces |> List.filter (fun x ->
            isClass || (not (Naming.automaticInterfaces.Contains x)))
        let classDecl =
            // Don't create a new context for class declarations
            transformClass com ctx entRange baseClass entDecls
            |> declareModMember entRange (None, ent.Name) ent.IsPublic modIdent
        if ifcs.Length = 0
        then [classDecl]
        else
            [ get (com.GetImport ctx false (Naming.getCoreLibPath com)) "Util"
              typeRef com ctx ent.File ent.FullName
              buildStringArray ifcs ]
            |> macroExpression None "$0.setInterfaces($1.prototype, $2)"
            |> Babel.ExpressionStatement :> Babel.Statement
            |> consBack [classDecl]
    decls |> List.fold (fun acc decl ->
        match decl with
        | Test (test, name) ->
            transformTest com ctx test name
            |> consBack acc
        | TestFixture (fixture, testDecls, testRange) ->
            let testDecls =
                let ctx = { ctx with moduleFullName = fixture.FullName } 
                transformModDecls com ctx None testDecls
            let testRange = foldRanges testRange testDecls
            transformTestFixture com ctx fixture testDecls testRange
            |> consBack acc  
        | Fable.ActionDeclaration e ->
            transformStatement com ctx e
            |> consBack acc
        | Fable.MemberDeclaration m ->
            transformModMember com ctx modIdent m
            |> consBack acc
        | Fable.EntityDeclaration (ent, entDecls, entRange) ->
            match ent.Kind with
            // Interfaces, attribute or erased declarations shouldn't reach this point
            | Fable.Interface ->
                failwithf "Cannot emit interface declaration into JS: %s" ent.FullName
            | Fable.Class baseClass ->
                declareClass com ctx ent entDecls entRange baseClass true
                |> List.append <| acc
            | Fable.Union | Fable.Record | Fable.Exception ->                
                declareClass com ctx ent entDecls entRange None false
                |> List.append <| acc
            | Fable.Module ->
                transformModule com ctx modIdent ent entDecls entRange
                |> consBack acc) []
    |> fun decls ->
        match modIdent with
        | Some modIdent -> (Babel.ReturnStatement modIdent :> Babel.Statement)::decls
        | None -> decls
        |> List.rev

let private makeCompiler (com: ICompiler) (files: Fable.File list) =
    let fileMap =
        files |> Seq.map (fun f -> f.FileName, f) |> Map.ofSeq
    { new IBabelCompiler with
        member bcom.GetFableFile fileName =
            Map.tryFind fileName fileMap
            |> function Some file -> file
                      | None -> failwithf "File not parsed: %s" fileName
        member bcom.GetImport ctx asDefault moduleName =
            match ctx.imports.TryGetValue moduleName with
            | true, (import, _) ->
                upcast Babel.Identifier import
            | false, _ ->
                let import = Naming.getImportModuleIdent ctx.imports.Count
                ctx.imports.Add(moduleName, (import, asDefault))
                upcast Babel.Identifier import
        member bcom.TransformExpr ctx e = transformExpr bcom ctx e
        member bcom.TransformStatement ctx e = transformStatement bcom ctx e
        member bcom.TransformFunction ctx args body = transformFunction bcom ctx args body
      interface ICompiler with
        member __.Options = com.Options }

let transformFiles (com: ICompiler) (files: Fable.File list) =
    let babelCom = makeCompiler com files
    files |> Seq.choose (fun file ->
        match file.Declarations with
        | [] -> None
        | _ ->
            let ctx = {
                file = file.FileName
                moduleFullName = file.Root.FullName
                imports = System.Collections.Generic.Dictionary<_,_>()
            }
            let isRootTest =
                file.Root.TryGetDecorator "TestFixture" |> Option.isSome
            let rootIdent =
                if isRootTest
                then None
                else Naming.getImportModuleIdent -1 |> Babel.Identifier |> Some
            let rootDecls = transformModDecls babelCom ctx rootIdent file.Declarations
            let rootRange = foldRanges SourceLocation.Empty rootDecls
            let rootMod =
                if isRootTest
                then transformTestFixture com ctx file.Root rootDecls rootRange
                     |> U2.Case1
                else Babel.ExportDefaultDeclaration(
                        U2.Case2 (Babel.CallExpression(
                                    Babel.FunctionExpression(
                                        [rootIdent.Value],
                                        Babel.BlockStatement(rootDecls, ?loc=Some rootRange),
                                        ?loc=Some rootRange),
                                    [U2.Case1 (upcast Babel.ObjectExpression [])],
                                    rootRange) :> Babel.Expression),
                            rootRange) :> Babel.ModuleDeclaration |> U2.Case2
            // Add imports
            let rootDecls =
                ctx.imports |> Seq.fold (fun acc import ->
                    let importVar, asDefault = import.Value
                    let specifier =
                        if asDefault
                        then Babel.Identifier importVar
                             |> Babel.ImportDefaultSpecifier
                             |> U3.Case2
                        else Babel.Identifier importVar
                             |> Babel.ImportNamespaceSpecifier
                             |> U3.Case3
                    Babel.ImportDeclaration(
                        [specifier],
                        Babel.StringLiteral import.Key)
                    :> Babel.ModuleDeclaration
                    |> U2.Case2
                    |> consBack acc) [rootMod]
            Babel.Program (file.FileName, rootRange, rootDecls) |> Some)         
