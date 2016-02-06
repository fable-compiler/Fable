module Fabel.Fabel2Babel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST

type private Context = {
    file: string
    moduleFullName: string
    imports: System.Collections.Generic.Dictionary<string, string * bool>
    }

type private IBabelCompiler =
    inherit ICompiler
    abstract GetFabelFile: string -> Fabel.File
    abstract GetImport: Context -> bool -> string -> Babel.Expression
    abstract TransformExpr: Context -> Fabel.Expr -> Babel.Expression
    abstract TransformStatement: Context -> Fabel.Expr -> Babel.Statement
    abstract TransformFunction: Context -> Fabel.Ident list -> Fabel.Expr ->
        (Babel.Pattern list) * U2<Babel.BlockStatement, Babel.Expression>

let private (|ExprType|) (fexpr: Fabel.Expr) = fexpr.Type
let private (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformExpr ctx e
let private (|TransformStatement|) (com: IBabelCompiler) ctx e = com.TransformStatement ctx e

let private (|TestFixture|_|) (decl: Fabel.Declaration) =
    match decl with
    | Fabel.EntityDeclaration (ent, entDecls, entRange) ->
        match ent.TryGetDecorator "TestFixture" with
        | Some _ -> Some (ent, entDecls, entRange)
        | None -> None
    | _ -> None

let private (|Test|_|) (decl: Fabel.Declaration) =
    match decl with
    | Fabel.MemberDeclaration m ->
        match m.Kind, m.TryGetDecorator "Test" with
        | Fabel.Method name, Some _ -> Some (m, name)
        | _ -> None
    | _ -> None
    
let private consBack tail head = head::tail

let private foldRanges (baseRange: SourceLocation) (decls: Babel.Statement list) =
    decls
    |> Seq.choose (fun x -> x.loc)
    |> Seq.fold (fun _ x -> x) baseRange
    |> (+) baseRange
    
let private cleanArgs (com: IBabelCompiler) ctx args =
    let rec cleanNull = function
        | [] -> []
        | (Fabel.Value Fabel.Null)::args -> cleanNull args
        | args -> args
    args
    |> List.rev |> cleanNull |> List.rev
    |> List.map (com.TransformExpr ctx >> U2<_,_>.Case1)
    
let private ident (id: Fabel.Ident) =
    Babel.Identifier id.name

let private identFromName name =
    let name = Naming.sanitizeIdent (fun _ -> false) name
    Babel.Identifier name
    
let private get left propName =
    if Naming.identForbiddenChars.IsMatch propName
    then Babel.MemberExpression(left, Babel.StringLiteral propName, true)
    else Babel.MemberExpression(left, Babel.Identifier propName, false)
    :> Babel.Expression
    
let private getExpr com ctx (TransformExpr com ctx expr) (property: Fabel.Expr) =
    let property, computed =
        match property with
        | Fabel.Value (Fabel.StringConst name)
            when Naming.identForbiddenChars.IsMatch name = false ->
            Babel.Identifier (name) :> Babel.Expression, false
        | TransformExpr com ctx property -> property, true
    match expr with
    | :? Babel.EmptyExpression -> property
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
        let file = com.GetFabelFile file
        file.ExternalEntities
        |> List.tryFind (fun ext ->
            fullName = ext.FullName || fullName.StartsWith ext.FullName)
        |> function
            | Some (Fabel.ImportModule (ns, modName, isNs)) ->
                Some (com.GetImport ctx isNs modName)
                |> makeExpr (getDiff ns fullName)
            | Some (Fabel.GlobalModule ns) ->
                makeExpr (getDiff ns fullName) None
            | None when ctx.file <> file.FileName ->
                ctx.file
                |> Naming.getRelativePath file.FileName
                |> fun x -> System.IO.Path.ChangeExtension(x, ".js")
                |> (+) "./"
                |> com.GetImport ctx false
                |> Some
                |> makeExpr (getDiff file.Root.FullName fullName)
            | None ->
                makeExpr (getDiff ctx.moduleFullName fullName) None

let private buildArray (com: IBabelCompiler) ctx consKind kind =
    match kind with
    | Fabel.TypedArray kind ->
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
            | Fabel.ArrayValues args ->
                List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
                |> Babel.ArrayExpression :> Babel.Expression |> U2.Case1 |> List.singleton
            | Fabel.ArrayAlloc length ->
                Babel.NumericLiteral(U2.Case1 length) :> Babel.Expression
                |> U2.Case1 |> List.singleton
            | Fabel.ArrayConversion (TransformExpr com ctx arr) ->
                [U2.Case1 arr]
        Babel.NewExpression(cons, args) :> Babel.Expression
    | Fabel.DynamicArray | Fabel.Tuple ->
        match consKind with
        | Fabel.ArrayValues args ->
            List.map (com.TransformExpr ctx >> U2.Case1 >> Some) args
            |> Babel.ArrayExpression :> Babel.Expression
        | Fabel.ArrayAlloc length ->
            let length = Babel.NumericLiteral(U2.Case1 length) :> Babel.Expression
            upcast Babel.NewExpression(Babel.Identifier "Array", [length |> U2.Case1])
        | Fabel.ArrayConversion (TransformExpr com ctx arr) ->
            arr

let private assign range left right =
    Babel.AssignmentExpression(AssignEqual, left, right, ?loc=range)
    :> Babel.Expression
    
let private block (com: IBabelCompiler) ctx range (exprs: Fabel.Expr list) =
    let exprs = match exprs with
                | [Fabel.Sequential (statements,_)] -> statements
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
let private iife (com: IBabelCompiler) ctx (expr: Fabel.Expr) =
    Babel.CallExpression (funcExpression com ctx [] expr, [], ?loc=expr.Range)

let private varDeclaration range (var: Babel.Pattern) value =
    Babel.VariableDeclaration (var, value, ?loc=range)
        
let private macroExpression range (txt: string) args =
    let args = Array.ofList args
    let el isTail value = Babel.TemplateElement(value, isTail)
    let rec buildEls idx (matches: System.Text.RegularExpressions.Match list) els =
        match matches with
        | [] -> (el true (txt.Substring idx), None)::els
        | m::ms ->
            let arg = m.Value.Substring 1 |> int |> Array.tryItem <| args
            let value, idx = txt.Substring(idx, m.Index - idx), m.Index + m.Length
            (el false value, arg)::els |> buildEls idx ms
    let placeholders =
        System.Text.RegularExpressions.Regex.Matches(txt, @"\$\d+")
        |> Seq.cast<_> |> Seq.toList
    match buildEls 0 placeholders [] with
    | [] -> Babel.StringLiteral(txt, ?loc=range) :> Babel.Expression
    | els ->
        let els, args =
            let els, args = els |> List.rev |> List.unzip
            els, List.choose id args
        // TODO: Check args length? (Omission may be intended)
        Babel.TemplateLiteral(els, args, macro=true, ?loc=range) 
        :> Babel.Expression

let private transformStatement com ctx (expr: Fabel.Expr): Babel.Statement =
    match expr with
    | Fabel.Loop (loopKind, range) ->
        match loopKind with
        | Fabel.While (TransformExpr com ctx guard, body) ->
            upcast Babel.WhileStatement (guard, block com ctx body.Range [body], ?loc=range)
        | Fabel.ForOf (var, TransformExpr com ctx enumerable, body) ->
            // enumerable doesn't go in VariableDeclator.init but in ForOfStatement.right 
            let var = Babel.VariableDeclaration (ident var)
            upcast Babel.ForOfStatement (
                U2.Case1 var, enumerable, block com ctx body.Range [body], ?loc=range)
        | Fabel.For (var, TransformExpr com ctx start,
                        TransformExpr com ctx limit, body, isUp) ->
            upcast Babel.ForStatement (
                block com ctx body.Range [body],
                start |> varDeclaration None (ident var) |> U2.Case1,
                Babel.BinaryExpression (BinaryOperator.BinaryLessOrEqual, ident var, limit),
                Babel.UpdateExpression (UpdateOperator.UpdatePlus, false, ident var), ?loc=range)

    | Fabel.Set (callee, property, TransformExpr com ctx value, range) ->
        let left =
            match property with
            | None -> com.TransformExpr ctx callee
            | Some property -> getExpr com ctx callee property
        upcast Babel.ExpressionStatement (assign range left value, ?loc = range)

    | Fabel.VarDeclaration (var, TransformExpr com ctx value, _isMutable) ->
        varDeclaration expr.Range (ident var) value :> Babel.Statement

    | Fabel.TryCatch (body, catch, finalizer, range) ->
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

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformStatement com ctx thenExpr, elseExpr, range) ->
        let elseExpr =
            match elseExpr with
            | Fabel.Value Fabel.Null -> None
            | _ -> Some (com.TransformStatement ctx elseExpr)
        upcast Babel.IfStatement (
            guardExpr, thenExpr, ?alternate=elseExpr, ?loc=range)

    | Fabel.Sequential _ ->
        failwithf "Sequence when single statement expected in %A: %A" expr.Range expr
        
    | Fabel.Throw (TransformExpr com ctx ex, range) ->
        upcast Babel.ThrowStatement(ex, ?loc=range)
        
    | Fabel.Wrapped (expr, _) ->
        com.TransformStatement ctx expr        

    // Expressions become ExpressionStatements
    | Fabel.Value _ | Fabel.Apply _ | Fabel.ObjExpr _ ->
        upcast Babel.ExpressionStatement (com.TransformExpr ctx expr, ?loc=expr.Range)

let private transformExpr (com: IBabelCompiler) ctx (expr: Fabel.Expr): Babel.Expression =
    match expr with
    | Fabel.Value kind ->
        match kind with
        | Fabel.ImportRef (import, isNs, prop) ->
            match prop with
            | Some prop -> get (com.GetImport ctx isNs import) prop
            | None -> com.GetImport ctx isNs import
        | Fabel.This _ -> upcast Babel.ThisExpression ()
        | Fabel.Super _ -> upcast Babel.Super ()
        | Fabel.Null -> upcast Babel.NullLiteral ()
        | Fabel.IdentValue {name=name} -> upcast Babel.Identifier (name)
        | Fabel.NumberConst (x,_) -> upcast Babel.NumericLiteral x
        | Fabel.StringConst x -> upcast Babel.StringLiteral (x)
        | Fabel.BoolConst x -> upcast Babel.BooleanLiteral (x)
        | Fabel.RegexConst (source, flags) -> upcast Babel.RegExpLiteral (source, flags)
        | Fabel.Lambda (args, body) -> funcArrow com ctx args body
        | Fabel.ArrayConst (cons, kind) -> buildArray com ctx cons kind
        | Fabel.TypeRef typ ->
            match typ with
            | Fabel.DeclaredType typEnt ->
                let typFullName =
                    if Option.isSome (typEnt.TryGetDecorator "Erase") 
                    then typEnt.FullName.Substring(0, typEnt.FullName.LastIndexOf ".")
                    else typEnt.FullName
                typeRef com ctx typEnt.File typFullName
            | _ -> failwithf "Not supported type reference: %A" typ
        | Fabel.LogicalOp _ | Fabel.BinaryOp _ | Fabel.UnaryOp _ | Fabel.Emit _ ->
            failwithf "Unexpected stand-alone value: %A" expr

    | Fabel.ObjExpr (props, range) ->
        props
        |> List.map (fun (key, value) ->
            let key =
                if Naming.identForbiddenChars.IsMatch key
                then Babel.StringLiteral key :> Babel.Expression
                else Babel.Identifier key :> Babel.Expression
            Babel.ObjectProperty(key, com.TransformExpr ctx value, ?loc=value.Range)
            |> U3.Case1 )
        |> fun props -> upcast Babel.ObjectExpression(props, ?loc=range)
        
    | Fabel.Wrapped (expr, _) ->
        com.TransformExpr ctx expr

    | Fabel.Apply (callee, args, kind, _, range) ->
        match callee, args with
        // Logical, Binary and Unary Operations
        | Fabel.Value (Fabel.LogicalOp op), [TransformExpr com ctx left; TransformExpr com ctx right] ->
            upcast Babel.LogicalExpression (op, left, right, ?loc=range)
        | Fabel.Value (Fabel.UnaryOp op), [TransformExpr com ctx operand as expr] ->
            upcast Babel.UnaryExpression (op, operand, ?loc=range)
        | Fabel.Value (Fabel.BinaryOp op), [TransformExpr com ctx left; TransformExpr com ctx right] ->
            upcast Babel.BinaryExpression (op, left, right, ?loc=range)
        // Emit expressions
        | Fabel.Value (Fabel.Emit emit), args ->
            List.map (com.TransformExpr ctx) args
            |> macroExpression range emit
        | _ ->
            match kind with
            | Fabel.ApplyMeth ->
                Babel.CallExpression (com.TransformExpr ctx callee, cleanArgs com ctx args, ?loc=range)
                :> Babel.Expression
            | Fabel.ApplyCons ->
                Babel.NewExpression (com.TransformExpr ctx callee, cleanArgs com ctx args, ?loc=range)
                :> Babel.Expression
            | Fabel.ApplyGet ->
                getExpr com ctx callee args.Head

    | Fabel.IfThenElse (TransformExpr com ctx guardExpr,
                        TransformExpr com ctx thenExpr,
                        TransformExpr com ctx elseExpr, range) ->
        upcast Babel.ConditionalExpression (
            guardExpr, thenExpr, elseExpr, ?loc = range)

    | Fabel.Sequential (statements, range) ->
        Babel.BlockStatement (statements |> List.map (com.TransformStatement ctx), ?loc=range)
        |> fun block -> upcast Babel.DoExpression (block, ?loc=range)

    | Fabel.TryCatch _ | Fabel.Throw _ ->
        upcast (iife com ctx expr)

    | Fabel.Loop _ | Fabel.Set _  | Fabel.VarDeclaration _ ->
        failwithf "Statement when expression expected in %A: %A" expr.Range expr 
    
let private transformFunction com ctx args body =
    let args: Babel.Pattern list =
        List.map (fun x -> upcast ident x) args
    let body: U2<Babel.BlockStatement, Babel.Expression> =
        match body with
        | ExprType (Fabel.PrimitiveType Fabel.Unit) ->
            block com ctx body.Range [body] |> U2.Case1
        | Fabel.TryCatch (tryBody, handler, finalizer, tryRange) ->
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
    
let private transformTestSuite com ctx decls: Babel.CallExpression =
    failwith "TODO: TestSuite members"
    
// TODO: Set interfaces with $Fabel.Util.setInterfaces(Class.prototyp, [interfaces])
let private transformClass com ctx classRange (baseClass: Fabel.EntityLocation option) decls =
    let declareMember range kind name args body isStatic =
        let name, computed: Babel.Expression * bool =
            if Naming.identForbiddenChars.IsMatch name
            then upcast Babel.StringLiteral name, true
            else upcast Babel.Identifier name, false
        let args, body = transformFunction com ctx args body
        let body = match body with U2.Case1 e -> e | U2.Case2 e -> returnBlock e
        // TODO: Optimization: remove null statement that F# compiler adds at the bottom of constructors
        Babel.ClassMethod(range, kind, name, args, body, computed, isStatic)
    let baseClass = baseClass |> Option.map (fun loc ->
        typeRef com ctx (Some loc.file) loc.fullName)
    decls
    |> List.map (function
        | Fabel.MemberDeclaration m ->
            let kind, name, isStatic =
                match m.Kind with
                | Fabel.Constructor -> Babel.ClassConstructor, "constructor", false
                | Fabel.Method name -> Babel.ClassFunction, name, m.IsStatic
                | Fabel.Getter name -> Babel.ClassGetter, name, m.IsStatic
                | Fabel.Setter name -> Babel.ClassSetter, name, m.IsStatic
            declareMember m.Range kind name m.Arguments m.Body isStatic
        | Fabel.ActionDeclaration _
        | Fabel.EntityDeclaration _ as decl ->
            failwithf "Unexpected declaration in class: %A" decl)
    |> List.map U2<_,Babel.ClassProperty>.Case1
    |> fun meths -> Babel.ClassExpression(classRange, Babel.ClassBody(classRange, meths), ?super=baseClass)

// TODO: Keep track of sanitized member names to be sure they don't clash? 
let private declareModMember range (var, name) isPublic modIdent expr =
    let var = match var with Some x -> x | None -> identFromName name
    match isPublic, modIdent with
    | true, Some modIdent -> assign (Some range) (get modIdent name) expr 
    | _ -> expr
    |> varDeclaration (Some range) var :> Babel.Statement

let private transformModMember com ctx modIdent (m: Fabel.Member) =
    let expr, name =
        match m.Kind with
        | Fabel.Getter name ->
            let args, body = transformFunction com ctx [] m.Body
            match body with
            | U2.Case2 e -> e, name
            | U2.Case1 e -> Babel.DoExpression(e, ?loc=e.loc) :> Babel.Expression, name
        | Fabel.Method name ->
            upcast funcExpression com ctx m.Arguments m.Body, name
        | Fabel.Constructor | Fabel.Setter _ ->
            failwithf "Unexpected member in module: %A" m.Kind
    let memberRange =
        match expr.loc with Some loc -> m.Range + loc | None -> m.Range
    declareModMember memberRange (None, name) m.IsPublic modIdent expr

// Compile tests using Mocha.js BDD interface
let private transformTest com ctx (test: Fabel.Member) name =
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
            [U2.Case1 testName; U2.Case1 testBody], testRange))
        :> Babel.Statement

let private transformTestFixture com ctx (fixture: Fabel.Entity) testDecls testRange =
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
                
let rec private transformModule com ctx modIdent (ent: Fabel.Entity) entDecls entRange =
    let nestedIdent, protectedIdent =
        let memberNames =
            entDecls |> Seq.choose (function
                | Fabel.EntityDeclaration (ent,_,_) -> Some ent.Name
                | Fabel.ActionDeclaration ent -> None
                | Fabel.MemberDeclaration m ->
                    match m.Kind with
                    | Fabel.Method name | Fabel.Getter name -> Some name
                    | Fabel.Constructor | Fabel.Setter _ -> None)
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
        | Fabel.ActionDeclaration e ->
            transformStatement com ctx e
            |> consBack acc
        | Fabel.MemberDeclaration m ->
            transformModMember com ctx modIdent m
            |> consBack acc
        | Fabel.EntityDeclaration (ent, entDecls, entRange) ->
            match ent.Kind with
            // Interfaces, attribute or erased declarations shouldn't reach this point
            | Fabel.Interface ->
                failwithf "Cannot emit interface declaration into JS: %s" ent.FullName
            | Fabel.Class _ | Fabel.Union | Fabel.Record ->
                let baseClass = match ent.Kind with Fabel.Class x -> x | _ -> None
                // Don't create a new context for class declarations
                transformClass com ctx entRange baseClass entDecls
                |> declareModMember entRange (None, ent.Name) ent.IsPublic modIdent
                |> consBack acc
            | Fabel.Module ->
                transformModule com ctx modIdent ent entDecls entRange
                |> consBack acc) []
    |> fun decls ->
        match modIdent with
        | Some modIdent -> (Babel.ReturnStatement modIdent :> Babel.Statement)::decls
        | None -> decls
        |> List.rev

let private makeCompiler (com: ICompiler) (files: Fabel.File list) =
    let fileMap =
        files |> Seq.map (fun f -> f.FileName, f) |> Map.ofSeq
    { new IBabelCompiler with
        member bcom.GetFabelFile fileName =
            Map.tryFind fileName fileMap
            |> function Some file -> file
                      | None -> failwithf "File not parsed: %s" fileName
        member bcom.GetImport ctx isNs moduleName =
            match ctx.imports.TryGetValue moduleName with
            | true, (import, _) ->
                upcast Babel.Identifier import
            | false, _ ->
                let import = Naming.getImportModuleIdent ctx.imports.Count
                ctx.imports.Add(moduleName, (import, isNs))
                upcast Babel.Identifier import
        member bcom.TransformExpr ctx e = transformExpr bcom ctx e
        member bcom.TransformStatement ctx e = transformStatement bcom ctx e
        member bcom.TransformFunction ctx args body = transformFunction bcom ctx args body
      interface ICompiler with
        member __.Options = com.Options }

let transformFiles (com: ICompiler) (files: Fabel.File list) =
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
                if isRootTest then
                    transformTestFixture com ctx file.Root rootDecls rootRange
                    |> U2.Case1
                else
                    Babel.ExportDefaultDeclaration(
                        U2.Case2 (Babel.CallExpression(
                                    Babel.FunctionExpression(
                                        [rootIdent.Value],
                                        Babel.BlockStatement(rootDecls, ?loc=Some rootRange),
                                        ?loc=Some rootRange),
                                    [U2.Case1 (upcast Babel.ObjectExpression [])],
                                    rootRange) :> Babel.Expression),
                            rootRange) :> Babel.ModuleDeclaration |> U2.Case2
            // Add imports
            // TODO: Import namespaces `import * as $M1 from foo`
            let rootDecls =
                ctx.imports |> Seq.fold (fun acc import ->
                    let importVar, isNs = import.Value
                    let specifier =
                        if isNs then
                            Babel.Identifier importVar
                            |> Babel.ImportNamespaceSpecifier
                            |> U3.Case3
                        else
                            Babel.Identifier importVar
                            |> Babel.ImportDefaultSpecifier
                            |> U3.Case2
                    Babel.ImportDeclaration(
                        [specifier],
                        Babel.StringLiteral import.Key)
                    :> Babel.ModuleDeclaration
                    |> U2.Case2
                    |> consBack acc) [rootMod]
            Babel.Program (file.FileName, rootRange, rootDecls) |> Some)         
