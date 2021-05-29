module rec Fable.Transforms.Fable2Python

open Fable
open Fable.AST
open Fable.AST.Python
open System.Collections.Generic
open System.Text.RegularExpressions
open Fable.Naming
open Fable.Core

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
  { File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string> }

type IPythonCompiler =
    inherit Compiler
    abstract GetIdentifier: ctx: Context * name: string -> Python.Identifier
    abstract GetAllImports: unit -> seq<Import>
    abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression * Statement list
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement list
    abstract TransformImport: Context * selector:string * path:string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> Arg list * Statement list

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

// TODO: All things that depend on the library should be moved to Replacements
// to become independent of the specific implementation
module Lib =
    let libCall (com: IPythonCompiler) ctx r moduleName memberName args =
        Expression.call(com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc=r)

    let libConsCall (com: IPythonCompiler) ctx r moduleName memberName args =
        Expression.call(com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc=r)

    let libValue (com: IPythonCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryJsConstructor (com: IPythonCompiler) ctx ent =
        match Replacements.tryJsConstructor com ent with
        | Some e -> com.TransformAsExpr(ctx, e) |> Some
        | None -> None

    let jsConstructor (com: IPythonCompiler) ctx ent =
        let entRef = Replacements.jsConstructor com ent
        com.TransformAsExpr(ctx, entRef)

// TODO: This is too implementation-dependent, ideally move it to Replacements
module Reflection =
    open Lib

    let private libReflectionCall (com: IPythonCompiler) ctx r memberName args =
        libCall com ctx r "Reflection" (memberName + "_type") args

    let private transformRecordReflectionInfo com ctx r (ent: Fable.Entity) generics =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = ent.FullName
        let fullnameExpr = Expression.constant(fullname)
        let genMap =
            let genParamNames = ent.GenericParameters |> List.mapToArray (fun x -> x.Name) |> Seq.toList
            List.zip genParamNames generics |> Map
        let fields, stmts =
            ent.FSharpFields |> Seq.map (fun fi ->
                let typeInfo, stmts = transformTypeInfo com ctx r genMap fi.FieldType
                (Expression.list([ Expression.constant(fi.Name); typeInfo ])), stmts)
            |> Seq.toList
            |> Helpers.unzipArgs
        let fields = Expression.lambda(Arguments.arguments [], Expression.list(fields))
        let js, stmts' = jsConstructor com ctx ent
        [ fullnameExpr; Expression.list(generics); js; fields ]
        |> libReflectionCall com ctx None "record", stmts @ stmts'

    let private transformUnionReflectionInfo com ctx r (ent: Fable.Entity) generics =
        let fullname = ent.FullName
        let fullnameExpr = Expression.constant(fullname)
        let genMap =
            let genParamNames = ent.GenericParameters |> List.map (fun x -> x.Name) |> Seq.toList
            List.zip genParamNames generics |> Map
        let cases =
            ent.UnionCases |> Seq.map (fun uci ->
                uci.UnionCaseFields |> List.map (fun fi ->
                    Expression.list([
                        fi.Name |> Expression.constant
                        let expr, stmts = transformTypeInfo com ctx r genMap fi.FieldType
                        expr
                    ]))
                |> Expression.list
            ) |> Seq.toList
        let cases = Expression.lambda(Arguments.arguments [], Expression.list(cases))
        let js, stmts = jsConstructor com ctx ent
        [ fullnameExpr; Expression.list(generics); js; cases ]
        |> libReflectionCall com ctx None "union", stmts

    let transformTypeInfo (com: IPythonCompiler) ctx r (genMap: Map<string, Expression>) t: Expression * Statement list =
        let primitiveTypeInfo name =
           libValue com ctx "Reflection" (name + "_type")
        let numberInfo kind =
            getNumberKindName kind
            |> primitiveTypeInfo
        let nonGenericTypeInfo fullname =
            [ Expression.constant(fullname) ]
            |> libReflectionCall com ctx None "class"
        let resolveGenerics generics: Expression list * Statement list =
            generics |> Array.map (transformTypeInfo com ctx r genMap) |> List.ofArray |> Helpers.unzipArgs
        let genericTypeInfo name genArgs =
            let resolved, stmts = resolveGenerics genArgs
            libReflectionCall com ctx None name resolved, stmts
        let genericEntity (fullname: string) (generics: Expression list) =
            libReflectionCall com ctx None "class" [
                Expression.constant(fullname)
                if not(List.isEmpty generics) then
                    Expression.list(generics)
            ]
        match t with
        | Fable.Any -> primitiveTypeInfo "obj", []
        | Fable.GenericParam name ->
            match Map.tryFind name genMap with
            | Some t -> t, []
            | None ->
                Replacements.genericTypeInfoError name |> addError com [] r
                Expression.none(), []
        | Fable.Unit    -> primitiveTypeInfo "unit", []
        | Fable.Boolean -> primitiveTypeInfo "bool", []
        | Fable.Char    -> primitiveTypeInfo "char", []
        | Fable.String  -> primitiveTypeInfo "string", []
        | Fable.Enum entRef ->
            let ent = com.GetEntity(entRef)
            let mutable numberKind = Int32
            let cases =
                ent.FSharpFields |> Seq.choose (fun fi ->
                    // F# seems to include a field with this name in the underlying type
                    match fi.Name with
                    | "value__" ->
                        match fi.FieldType with
                        | Fable.Number kind -> numberKind <- kind
                        | _ -> ()
                        None
                    | name ->
                        let value = match fi.LiteralValue with Some v -> System.Convert.ToDouble v | None -> 0.
                        Expression.list([ Expression.constant(name); Expression.constant(value) ]) |> Some)
                |> Seq.toList
                |> Expression.list
            [ Expression.constant(entRef.FullName); numberInfo numberKind; cases ]
            |> libReflectionCall com ctx None "enum", []
        | Fable.Number kind ->
            numberInfo kind, []
        | Fable.LambdaType(argType, returnType) ->
            genericTypeInfo "lambda" [|argType; returnType|]
        | Fable.DelegateType(argTypes, returnType) ->
            genericTypeInfo "delegate" ([|yield! argTypes; yield returnType|])
        | Fable.Tuple genArgs   -> genericTypeInfo "tuple" (List.toArray genArgs)
        | Fable.Option genArg   -> genericTypeInfo "option" [|genArg|]
        | Fable.Array genArg    -> genericTypeInfo "array" [|genArg|]
        | Fable.List genArg     -> genericTypeInfo "list" [|genArg|]
        | Fable.Regex           -> nonGenericTypeInfo Types.regex, []
        | Fable.MetaType        -> nonGenericTypeInfo Types.type_, []
        | Fable.AnonymousRecordType(fieldNames, genArgs) ->
            let genArgs, stmts = resolveGenerics (List.toArray genArgs)
            List.zip (List.ofArray fieldNames) genArgs
            |> List.map (fun (k, t) -> Expression.list[ Expression.constant(k); t ])
            |> libReflectionCall com ctx None "anonRecord", stmts
        | Fable.DeclaredType(entRef, generics) ->
            let fullName = entRef.FullName
            match fullName, generics with
            | Replacements.BuiltinEntity kind ->
                match kind with
                | Replacements.BclGuid
                | Replacements.BclTimeSpan
                | Replacements.BclDateTime
                | Replacements.BclDateTimeOffset
                | Replacements.BclTimer
                | Replacements.BclInt64
                | Replacements.BclUInt64
                | Replacements.BclDecimal
                | Replacements.BclBigInt -> genericEntity fullName [], []
                | Replacements.BclHashSet gen
                | Replacements.FSharpSet gen ->
                    let gens, stmts = transformTypeInfo com ctx r genMap gen
                    genericEntity fullName [ gens ], stmts
                | Replacements.BclDictionary(key, value)
                | Replacements.BclKeyValuePair(key, value)
                | Replacements.FSharpMap(key, value) ->
                    let keys, stmts = transformTypeInfo com ctx r genMap key
                    let values, stmts' = transformTypeInfo com ctx r genMap value
                    genericEntity fullName [
                        keys
                        values
                    ], stmts @ stmts'
                | Replacements.FSharpResult(ok, err) ->
                    let ent = com.GetEntity(entRef)
                    let ok', stmts = transformTypeInfo com ctx r genMap ok
                    let err', stmts' = transformTypeInfo com ctx r genMap err
                    let expr, stmts'' =transformUnionReflectionInfo com ctx r ent [ ok'; err' ]
                    expr, stmts @ stmts' @ stmts''
                | Replacements.FSharpChoice gen ->
                    let ent = com.GetEntity(entRef)
                    let gen, stmts = List.map (transformTypeInfo com ctx r genMap) gen |> Helpers.unzipArgs
                    let expr, stmts' = gen |> transformUnionReflectionInfo com ctx r ent
                    expr, stmts @ stmts'
                | Replacements.FSharpReference gen ->
                    let ent = com.GetEntity(entRef)
                    let gen, stmts = transformTypeInfo com ctx r genMap gen
                    let expr, stmts' = [ gen ] |> transformRecordReflectionInfo com ctx r ent
                    expr, stmts @ stmts'
            | _ ->
                let ent = com.GetEntity(entRef)
                let generics, stmts = generics |> List.map (transformTypeInfo com ctx r genMap) |> Helpers.unzipArgs
                /// Check if the entity is actually declared in JS code
                if ent.IsInterface
                    || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    || FSharp2Fable.Util.isGlobalOrImportedEntity ent
                    || FSharp2Fable.Util.isReplacementCandidate ent then
                    genericEntity ent.FullName generics, stmts
                else
                    let reflectionMethodExpr = FSharp2Fable.Util.entityRefWithSuffix com ent Naming.reflectionSuffix
                    let callee, stmts' = com.TransformAsExpr(ctx, reflectionMethodExpr)
                    Expression.call(callee, generics), stmts @ stmts'

    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName
            let exprs, stmts =
                [
                    yield Expression.constant(fullname), []
                    match generics with
                    | [] -> yield Util.undefined None, []
                    | generics -> yield Expression.list(generics), []
                    match tryJsConstructor com ctx ent with
                    | Some (cons, stmts) -> yield cons, stmts
                    | None -> ()
                    match ent.BaseType with
                    | Some d ->
                        let genMap =
                            Seq.zip ent.GenericParameters generics
                            |> Seq.map (fun (p, e) -> p.Name, e)
                            |> Map
                        yield Fable.DeclaredType(d.Entity, d.GenericArgs)
                              |> transformTypeInfo com ctx r genMap
                    | None -> ()
                ]
                |> Helpers.unzipArgs
            exprs
            |> libReflectionCall com ctx r "class", stmts

    let private ofString s = Expression.constant(s)
    let private ofArray babelExprs = Expression.list(babelExprs)

    let transformTypeTest (com: IPythonCompiler) ctx range expr (typ: Fable.Type): Expression * Statement list =
        let warnAndEvalToFalse msg =
            "Cannot type test (evals to false): " + msg
            |> addWarning com [] range
            Expression.constant(false)

        let jsTypeof (primitiveType: string) (Util.TransformExpr com ctx (expr, stmts)): Expression * Statement list =
            let typeof = Expression.unaryOp(UnaryTypeof, expr)
            Expression.binOp(typeof, BinaryEqualStrict, Expression.constant(primitiveType), ?loc=range), stmts

        let jsInstanceof consExpr (Util.TransformExpr com ctx (expr, stmts)): Expression * Statement list=
            Expression.binOp(expr, BinaryInstanceOf, consExpr, ?loc=range), stmts

        match typ with
        | Fable.Any -> Expression.constant(true), []
        | Fable.Unit ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            Expression.compare(expr, [ Is ],  [ Util.undefined None ], ?loc=range), stmts
        | Fable.Boolean -> jsTypeof "boolean" expr
        | Fable.Char | Fable.String _ -> jsTypeof "string" expr
        | Fable.Number _ | Fable.Enum _ -> jsTypeof "number" expr
        | Fable.Regex -> jsInstanceof (Expression.identifier("RegExp")) expr
        | Fable.LambdaType _ | Fable.DelegateType _ -> jsTypeof "function" expr
        | Fable.Array _ | Fable.Tuple _ ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            libCall com ctx None "Util" "isArrayLike" [ expr ], stmts
        | Fable.List _ ->
            jsInstanceof (libValue com ctx "List" "FSharpList") expr
        | Fable.AnonymousRecordType _ ->
            warnAndEvalToFalse "anonymous records", []
        | Fable.MetaType ->
            jsInstanceof (libValue com ctx "Reflection" "TypeInfo") expr
        | Fable.Option _ -> warnAndEvalToFalse "options", [] // TODO
        | Fable.GenericParam _ -> warnAndEvalToFalse "generic parameters", []
        | Fable.DeclaredType (ent, genArgs) ->
            match ent.FullName with
            | Types.idisposable ->
                match expr with
                | MaybeCasted(ExprType(Fable.DeclaredType (ent2, _)))
                        when com.GetEntity(ent2) |> FSharp2Fable.Util.hasInterface Types.idisposable ->
                    Expression.constant(true), []
                | _ ->
                    let expr, stmts = com.TransformAsExpr(ctx, expr)
                    libCall com ctx None "Util" "isDisposable" [ expr ], stmts
            | Types.ienumerable ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)
                [ expr ]
                |> libCall com ctx None "Util" "isIterable", stmts
            | Types.array ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)
                [ expr ]
                |> libCall com ctx None "Util" "isArrayLike", stmts
            | Types.exception_ ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)
                [ expr ]
                |> libCall com ctx None "Types" "isException", stmts
            | _ ->
                let ent = com.GetEntity(ent)
                if ent.IsInterface then
                    warnAndEvalToFalse "interfaces", []
                else
                    match tryJsConstructor com ctx ent with
                    | Some (cons, stmts) ->
                        if not(List.isEmpty genArgs) then
                            com.WarnOnlyOnce("Generic args are ignored in type testing", ?range=range)
                        let expr, stmts' = jsInstanceof cons expr
                        expr, stmts @ stmts'
                    | None ->
                        warnAndEvalToFalse ent.FullName, []

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
            Regex(".*(\/fable-library.*)?\/(?<module>[^\/]*)\.(js|fs)", RegexOptions.Compiled)

        let m = _reFableLib.Match(moduleName)
        let dashify = applyCaseRule CaseRules.SnakeCase

        if m.Groups.Count > 1 then
            let pymodule =
                m.Groups.["module"].Value
                |> dashify
                |> clean

            let moduleName = String.concat "." [ "fable"; pymodule ]

            //printfn "-> Module: %A" moduleName
            moduleName
        else
            // Modules should have short, all-lowercase names.
            let moduleName =
                let name =
                    moduleName.Replace("/", "")
                    |> dashify
                string(name.[0]) + name.[1..].Replace(".", "_")

            //printfn "-> Module: %A" moduleName
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
    open Lib
    open Reflection

    let getIdentifier (com: IPythonCompiler) (ctx: Context) (name: string) =
        let name = Helpers.clean name

        // FIXME:
        //match name with
        //| "math" -> com.GetImportExpr(ctx, "math") |> ignore
        //| _ -> ()

        Python.Identifier name

    let (|TransformExpr|) (com: IPythonCompiler) ctx e : Expression * Statement list =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, _) -> Some([arg], body)
        | Fable.Delegate(args, body, _) -> Some(args, body)
        | _ -> None

    let (|Lets|_|) = function
        | Fable.Let(ident, value, body) -> Some([ident, value], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArgument && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    let getUniqueNameInRootScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name)
            || ctx.UsedNames.DeclarationScopes.Contains(name))
        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.CurrentDeclarationScope.Contains(name))
        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity(_com: Compiler, ctx, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds = discardUnitArg args |> List.map (fun arg ->
            getUniqueNameInDeclarationScope ctx (arg.Name + "_mut"))
        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds
            member _.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id -> name = id.Name | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf "Cannot find DecisionTree target %i" targetIndex
        | Some(idents, target) -> idents, target

    let rec isJsStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Value _ | Fable.Import _  | Fable.IdentExpr _
        | Fable.Lambda _ | Fable.Delegate _ | Fable.ObjectExpr _
        | Fable.Call _ | Fable.CurriedApply _ | Fable.Curry _ | Fable.Operation _
        | Fable.Get _ | Fable.Test _ | Fable.TypeCast _ -> false

        | Fable.TryCatch _
        | Fable.Sequential _ | Fable.Let _ | Fable.LetRec _ | Fable.Set _
        | Fable.ForLoop _ | Fable.WhileLoop _ -> true

        // TODO: If IsJsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i,_,_) -> i.IsJsStatement

        | Fable.DecisionTreeSuccess(targetIndex,_, _) ->
            getDecisionTarget ctx targetIndex
            |> snd |> isJsStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_,targets) ->
            preferStatement
            || List.exists (snd >> (isJsStatement ctx false)) targets

        | Fable.IfThenElse(_,thenExpr,elseExpr,_) ->
            preferStatement || isJsStatement ctx false thenExpr || isJsStatement ctx false elseExpr


    let addErrorAndReturnNull (com: Compiler) (range: SourceLocation option) (error: string) =
        addError com [] range error
        Expression.name (Python.Identifier("None"))

    let ident (id: Fable.Ident) =
        Identifier(id.Name)

    let identAsExpr (id: Fable.Ident) =
        Expression.identifier(id.Name, ?loc=id.Range)

    //let identAsPattern (id: Fable.Ident) =
    //    Pattern.identifier(id.Name, ?loc=id.Range)

    let thisExpr =
        Expression.name ("self")

    let ofInt (i: int) =
        Expression.constant(float i)

    let ofString (s: string) =
       Expression.constant(s)

    let memberFromName (memberName: string): Expression * Statement list =
        match memberName with
        | "ToString" -> Expression.identifier("toString"), []
        | n when n.StartsWith("Symbol.iterator") ->
            //Expression.memberExpression(Expression.identifier("Symbol"), Expression.identifier(n.[7..]), false), true
            let name = Identifier "__iter__"
            Expression.name(name), []
        | n when Naming.hasIdentForbiddenChars n -> Expression.constant(n), []
        | n -> Expression.identifier(n), []


    let get r left memberName =
        let expr = Identifier memberName
        Expression.attribute (value = left, attr = expr, ctx = Load)

    let getExpr r (object: Expression) (expr: Expression) =
        let attr, stmts =
            match expr with
            | Expression.Constant(value=value) ->
                match value with
                | :? string as str -> memberFromName str
                | _ -> failwith "Need to be string"
            | e -> e, []
        let func = Expression.name("getattr")
        Expression.call(func=func, args=[object; attr]), stmts
        //Expression.attribute (value = object, attr = expr, ctx = Load), stmts
        //Expression.memberExpression(object, expr, computed, ?loc=r)

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

    let makeArray (com: IPythonCompiler) ctx exprs =
        let expr, stmts = exprs |> List.map (fun e -> com.TransformAsExpr(ctx, e)) |> Helpers.unzipArgs
        expr |> Expression.list, stmts

    let makeStringArray strings =
        strings
        |> List.map (fun x -> Expression.constant(x))
        |> Expression.list

    let makeJsObject (pairs: seq<string * Expression>) =
        pairs |> Seq.map (fun (name, value) ->
           let prop, computed = memberFromName name
           prop, value)
        |> Seq.toList
        |> List.unzip
        |> Expression.dict

    let assign range left right =
        Expression.namedExpr(left, right, ?loc=range)

    /// Immediately Invoked Function Expression
    let iife (com: IPythonCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction(ctx, None, [], expr)
        // Use an arrow function in case we need to capture `this`
        let afe, stmts = makeArrowFunctionExpression [] body
        Expression.call(afe, []), stmts

    let multiVarDeclaration (variables: (Identifier * Expression option) list) =
        let varDeclarators =
            // TODO: Log error if there're duplicated non-empty var declarations
            variables
            |> List.distinctBy (fun (Identifier(name=name), _value) -> name)

        [
            for id, init in varDeclarators do
                let name = Expression.name(id, Store)
                match init with
                | Some value ->
                    Statement.assign ([name], value)
                | None -> () ]

    let varDeclaration (var: Expression) (isMutable: bool) value =
        Statement.assign([var], value)

    let restElement (var: Fable.Ident) =
        let var = Expression.name (ident var)
        Expression.starred(var)

    let callSuper (args: Expression list) =
        let super = Expression.name (Python.Identifier("super().__init__"))
        Expression.call(super, args)

    let callSuperAsStatement (args: Expression list) =
        Statement.expr(callSuper args)

    let makeClassConstructor args body =
        // ClassMember.classMethod(ClassImplicitConstructor, Expression.identifier("constructor"), args, body)
        let name = Python.Identifier("__init__")
        let args = Arguments.arguments (args = args)
        FunctionDef.Create(name, args, body = body)

    let callFunction r funcExpr (args: Expression list) =
        Expression.call(funcExpr, args, ?loc=r)

    let callFunctionWithThisContext r funcExpr (args: Expression list) =
        let args = thisExpr::args
        Expression.call(get None funcExpr "call", args, ?loc=r)

    let emitExpression range (txt: string) args =
        Expression.emit (txt, args, ?loc=range)

    let undefined range: Expression =
        Expression.unaryOp(UnaryVoid, Expression.constant(0.), ?loc=range)

    let getGenericTypeParams (types: Fable.Type list) =
        let rec getGenParams = function
            | Fable.GenericParam name -> [name]
            | t -> t.Generics |> List.collect getGenParams
        types
        |> List.collect getGenParams
        |> Set.ofList

    let uncurryLambdaType t =
        let rec uncurryLambdaArgs acc = function
            | Fable.LambdaType(paramType, returnType) ->
                uncurryLambdaArgs (paramType::acc) returnType
            | t -> List.rev acc, t
        uncurryLambdaArgs [] t

    type MemberKind =
        | ClassConstructor
        | NonAttached of funcName: string
        | Attached of isStatic: bool

    let getMemberArgsAndBody (com: IPythonCompiler) ctx kind hasSpread (args: Fable.Ident list) (body: Fable.Expr) =
        let funcName, genTypeParams, args, body =
            match kind, args with
            | Attached(isStatic=false), (thisArg::args) ->
                let genTypeParams = Set.difference (getGenericTypeParams [thisArg.Type]) ctx.ScopedTypeParams
                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if FableTransforms.isIdentUsed thisArg.Name body then
                        let thisKeyword = Fable.IdentExpr { thisArg with Name = "this" }
                        Fable.Let(thisArg, thisKeyword, body)
                    else body
                None, genTypeParams, args, body
            | Attached(isStatic=true), _
            | ClassConstructor, _ -> None, ctx.ScopedTypeParams, args, body
            | NonAttached funcName, _ -> Some funcName, Set.empty, args, body
            | _ -> None, Set.empty, args, body

        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams genTypeParams }
        let args, body = transformFunction com ctx funcName args body

        let args =
            let len = List.length args
            if not hasSpread || len = 0 then args
            else [
                if len > 1 then
                    yield! args.[..len-2]
                // FIXME: yield restElement args.[len-1]
            ]

        args, body

    let getUnionCaseName (uci: Fable.UnionCase) =
        match uci.CompiledName with Some cname -> cname | None -> uci.Name

    let getUnionExprTag (com: IPythonCompiler) ctx r (fableExpr: Fable.Expr) =
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
        let expr, stmts' = getExpr r expr (Expression.constant("tag"))
        expr, stmts @ stmts'

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | Expression.Constant(_), _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.Enum _ ->
            Expression.binOp(e, BinaryOrBitwise, Expression.constant(0.))
        | _ -> e

    let wrapExprInBlockWithReturn (e, stmts) =
        stmts @ [ Statement.return'(e) ]

    let makeArrowFunctionExpression (args: Arg list) (body: Statement list) : Expression * Statement list =
        let name = Helpers.getUniqueIdentifier "lifted"
        let args = Arguments.arguments(args)
        let func = FunctionDef.Create(name = name, args = args, body = body)
        Expression.name (name), [ func ]

    let makeFunctionExpression name (args, (body: Expression)) : Expression * Statement list=
        let id = name |> Option.map Identifier
        let body = wrapExprInBlockWithReturn (body, [])
        let name = Helpers.getUniqueIdentifier "lifted"
        let func =
            FunctionDef.Create(name = name, args = Arguments.arguments args, body = body)
        //Expression.functionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)
        Expression.name (name), [ func ]

    let optimizeTailCall (com: IPythonCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
        let rec checkCrossRefs tempVars allArgs = function
            | [] -> tempVars
            | (argId, _arg)::rest ->
                let found = allArgs |> List.exists (FableTransforms.deepExists (function
                    | Fable.IdentExpr i -> argId = i.Name
                    | _ -> false))
                let tempVars =
                    if found then
                        let tempVarName = getUniqueNameInDeclarationScope ctx (argId + "_tmp")
                        Map.add argId tempVarName tempVars
                    else tempVars
                checkCrossRefs tempVars allArgs rest
        ctx.OptimizeTailCall()
        let zippedArgs = List.zip tc.Args args
        let tempVars = checkCrossRefs Map.empty args zippedArgs
        let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExpr v)
        [
            // First declare temp variables
            for (KeyValue(argId, tempVar)) in tempVars do
                yield varDeclaration (Expression.identifier(tempVar)) false (Expression.identifier(argId))
            // Then assign argument expressions to the original argument identifiers
            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
            for (argId, arg) in zippedArgs do
                let arg = FableTransforms.replaceValues tempVarReplacements arg
                let arg, stmts = com.TransformAsExpr(ctx, arg)
                yield! stmts
                yield assign None (Expression.identifier(argId)) arg |> Statement.expr
            yield Statement.continue'(?loc=range)
        ]

    let transformImport (com: IPythonCompiler) ctx r (selector: string) (path: string) =
        let selector, parts =
            let parts = Array.toList(selector.Split('.'))
            parts.Head, parts.Tail
        com.GetImportExpr(ctx, selector, path, r)
        |> getParts parts

    let transformCast (com: IPythonCompiler) (ctx: Context) t tag e: Expression * Statement list =
        // HACK: Try to optimize some patterns after FableTransforms
        let optimized =
            match tag with
            | Some (Naming.StartsWith "optimizable:" optimization) ->
                match optimization, e with
                | "array", Fable.Call(_,info,_,_) ->
                    match info.Args with
                    | [Replacements.ArrayOrListLiteral(vals,_)] -> Fable.Value(Fable.NewArray(vals, Fable.Any), e.Range) |> Some
                    | _ -> None
                | "pojo", Fable.Call(_,info,_,_) ->
                    match info.Args with
                    | keyValueList::caseRule::_ -> Replacements.makePojo com (Some caseRule) keyValueList
                    | keyValueList::_ -> Replacements.makePojo com None keyValueList
                    | _ -> None
                | _ -> None
            | _ -> None

        match optimized, t with
        | Some e, _ -> com.TransformAsExpr(ctx, e)
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | None, Fable.DeclaredType(ent,[_]) ->
            match ent.FullName, e with
            | Types.ienumerableGeneric, Replacements.ArrayOrListLiteral(exprs, _) ->
                makeArray com ctx exprs
            | _ -> com.TransformAsExpr(ctx, e)
        | _ -> com.TransformAsExpr(ctx, e)

    let transformCurry (com: IPythonCompiler) (ctx: Context) _r expr arity: Expression * Statement list =
        com.TransformAsExpr(ctx, Replacements.curryExprAtRuntime com arity expr)

    let transformValue (com: IPythonCompiler) (ctx: Context) r value: Expression * Statement list =
        match value with
        | Fable.BaseValue(None,_) -> Expression.identifier("super().__init__"), []
        | Fable.BaseValue(Some boundIdent,_) -> identAsExpr boundIdent, []
        | Fable.ThisValue _ -> Expression.identifier("self"), []
        | Fable.TypeInfo t -> transformTypeInfo com ctx r Map.empty t
        | Fable.Null _t ->
            // if com.Options.typescript
            //     let ta = typeAnnotation com ctx t |> TypeAnnotation |> Some
            //     upcast Identifier("null", ?typeAnnotation=ta, ?loc=r)
            // else
                Expression.identifier("None", ?loc=r), []
        | Fable.UnitConstant -> undefined r, []
        | Fable.BoolConstant x -> Expression.constant(x, ?loc=r), []
        | Fable.CharConstant x -> Expression.constant(string x, ?loc=r), []
        | Fable.StringConstant x -> Expression.constant(x, ?loc=r), []
        | Fable.NumberConstant (x,_) -> Expression.constant(x, ?loc=r), []
        //| Fable.RegexConstant (source, flags) -> Expression.regExpLiteral(source, flags, ?loc=r)
        | Fable.NewArray (values, typ) -> makeArray com ctx values
        //| Fable.NewArrayFrom (size, typ) -> makeAllocatedFrom com ctx size, []
        | Fable.NewTuple vals -> makeArray com ctx vals
        // | Fable.NewList (headAndTail, _) when List.contains "FABLE_LIBRARY" com.Options.Define ->
        //     makeList com ctx r headAndTail
        // Optimization for bundle size: compile list literals as List.ofArray
        | Fable.NewList (headAndTail, _) ->
            let rec getItems acc = function
                | None -> List.rev acc, None
                | Some(head, Fable.Value(Fable.NewList(tail, _),_)) -> getItems (head::acc) tail
                | Some(head, tail) -> List.rev (head::acc), Some tail
            match getItems [] headAndTail with
            | [], None ->
                libCall com ctx r "List" "empty" [], []
            | [TransformExpr com ctx (expr, stmts)], None ->
                libCall com ctx r "List" "singleton" [ expr ], stmts
            | exprs, None ->
                let expr, stmts = makeArray com ctx exprs
                [ expr ]
                |> libCall com ctx r "List" "ofArray", stmts
            | [TransformExpr com ctx (head, stmts)], Some(TransformExpr com ctx (tail, stmts')) ->
                libCall com ctx r "List" "cons" [ head; tail], stmts @ stmts'
            | exprs, Some(TransformExpr com ctx (tail, stmts)) ->
                let expr, stmts' = makeArray com ctx exprs
                [ expr; tail ]
                |> libCall com ctx r "List" "ofArrayWithTail", stmts @ stmts'
        | Fable.NewOption (value, t) ->
            match value with
            | Some (TransformExpr com ctx (e, stmts)) ->
                if mustWrapOption t
                then libCall com ctx r "Option" "some" [ e ], stmts
                else e, []
            | None -> undefined r, []
        | Fable.EnumConstant(x,_) ->
            com.TransformAsExpr(ctx, x)
        | Fable.NewRecord(values, ent, genArgs) ->
            let ent = com.GetEntity(ent)
            let values, stmts = List.map (fun x -> com.TransformAsExpr(ctx, x)) values |> Helpers.unzipArgs
            let consRef, stmts' = ent |> jsConstructor com ctx
            Expression.call(consRef, values, ?loc=r), stmts @ stmts'
        | Fable.NewAnonymousRecord(values, fieldNames, _genArgs) ->
            let values, stmts = values |> List.map (fun x -> com.TransformAsExpr(ctx, x)) |> Helpers.unzipArgs
            List.zip (List.ofArray fieldNames) values |> makeJsObject, stmts
        | Fable.NewUnion(values, tag, ent, genArgs) ->
            let ent = com.GetEntity(ent)
            let values, stmts = List.map (fun x -> com.TransformAsExpr(ctx, x)) values |> Helpers.unzipArgs
            let consRef, stmts' = ent |> jsConstructor com ctx
            // let caseName = ent.UnionCases |> List.item tag |> getUnionCaseName |> ofString
            let values = (ofInt tag)::values
            Expression.call(consRef, values, ?loc=r), stmts @ stmts'
        | _ -> failwith $"transformValue: value {value} not supported!"

    let enumerator2iterator com ctx =
        let enumerator = Expression.call(get None (Expression.identifier("self")) "GetEnumerator", [])
        [ Statement.return'(libCall com ctx None "Util" "toIterator" [ enumerator ]) ]

    let extractBaseExprFromBaseCall (com: IPythonCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
        match baseCall, baseType with
        | Some (Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr, stmts =
                match baseRef with
                | Fable.IdentExpr id -> Expression.identifier id.Name, []
                | _ -> transformAsExpr com ctx baseRef
            let args = transformCallArgs com ctx info.HasSpread info.Args
            Some (baseExpr, args)
        | Some (Fable.Value _), Some baseType ->
            // let baseEnt = com.GetEntity(baseType.Entity)
            // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
            // let entityType = FSharp2Fable.Util.getEntityType baseEnt
            // let baseRefId = makeTypedIdent entityType entityName
            // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
            // Some (baseExpr, []) // default base constructor
            let range = baseCall |> Option.bind (fun x -> x.Range)
            sprintf "Ignoring base call for %s" baseType.Entity.FullName |> addWarning com [] range
            None
        | Some _, _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)
            "Unexpected base call expression, please report" |> addError com [] range
            None
        | None, _ ->
            None

    // let transformObjectExpr (com: IPythonCompiler) ctx (members: Fable.MemberDecl list) baseCall: Expression * Statement list =
    //     let compileAsClass =
    //         Option.isSome baseCall || members |> List.exists (fun m ->
    //             // Optimization: Object literals with getters and setters are very slow in V8
    //             // so use a class expression instead. See https://github.com/fable-compiler/Fable/pull/2165#issuecomment-695835444
    //             m.Info.IsSetter || (m.Info.IsGetter && canHaveSideEffects m.Body))

    //     let makeMethod kind prop computed hasSpread args body =
    //         let args, body =
    //             getMemberArgsAndBody com ctx (Attached(isStatic=false)) hasSpread args body
    //         ObjectMember.objectMethod(kind, prop, args, body, computed_=computed,
    //             ?returnType=returnType, ?typeParameters=typeParamDecl)

    //     let members =
    //         members |> List.collect (fun memb ->
    //             let info = memb.Info
    //             let prop, computed = memberFromName memb.Name
    //             // If compileAsClass is false, it means getters don't have side effects
    //             // and can be compiled as object fields (see condition above)
    //             if info.IsValue || (not compileAsClass && info.IsGetter) then
    //                 [ObjectMember.objectProperty(prop, com.TransformAsExpr(ctx, memb.Body), computed_=computed)]
    //             elif info.IsGetter then
    //                 [makeMethod ObjectGetter prop computed false memb.Args memb.Body]
    //             elif info.IsSetter then
    //                 [makeMethod ObjectSetter prop computed false memb.Args memb.Body]
    //             elif info.IsEnumerator then
    //                 let method = makeMethod ObjectMeth prop computed info.HasSpread memb.Args memb.Body
    //                 let iterator =
    //                     let prop, computed = memberFromName "Symbol.iterator"
    //                     let body = enumerator2iterator com ctx
    //                     ObjectMember.objectMethod(ObjectMeth, prop, [||], body, computed_=computed)
    //                 [method; iterator]
    //             else
    //                 [makeMethod ObjectMeth prop computed info.HasSpread memb.Args memb.Body]
    //         )

    //     let classMembers =
    //         members |> List.choose (function
    //             | ObjectProperty(key, value, computed) ->
    //                 ClassMember.classProperty(key, value, computed_=computed) |> Some
    //             | ObjectMethod(kind, key, ``params``, body, computed, returnType, typeParameters, _) ->
    //                 let kind =
    //                     match kind with
    //                     | "get" -> ClassGetter
    //                     | "set" -> ClassSetter
    //                     | _ -> ClassFunction
    //                 ClassMember.classMethod(kind, key, ``params``, body, computed_=computed,
    //                     ?returnType=returnType, ?typeParameters=typeParameters) |> Some)

    //     let baseExpr, classMembers =
    //         baseCall
    //         |> extractBaseExprFromBaseCall com ctx None
    //         |> Option.map (fun (baseExpr, baseArgs) ->
    //             let consBody = [ callSuperAsStatement baseArgs ]
    //             let cons = makeClassConstructor []  consBody
    //             Some baseExpr, cons::classMembers
    //         )
    //         |> Option.defaultValue (None, classMembers)

    //     let classBody = ClassBody.classBody(List.toArray classMembers)
    //     let classExpr = Expression.classExpression(classBody, ?superClass=baseExpr)
    //     Expression.newExpression(classExpr, [||])

    let transformCallArgs (com: IPythonCompiler) ctx hasSpread args : Expression list * Statement list =
        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> [], []
        | args when hasSpread ->
            match List.rev args with
            | [] -> [], []
            | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs) |> Helpers.unzipArgs
            | last::rest ->
                let rest, stmts = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e)) |> Helpers.unzipArgs
                let expr, stmts' = com.TransformAsExpr(ctx, last)
                rest @ [ Expression.starred(expr) ], stmts @ stmts'
        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args |> Helpers.unzipArgs

    let resolveExpr t strategy babelExpr: Statement =
        match strategy with
        | None | Some ReturnUnit -> Statement.expr(babelExpr)
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return ->  Statement.return'(wrapIntExpression t babelExpr)
        | Some(Assign left) -> Statement.expr(assign None left babelExpr)
        | Some(Target left) -> Statement.expr(assign None (left |> Expression.identifier) babelExpr)

    let transformOperation com ctx range opKind: Expression * Statement list =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx (expr, stmts)) ->
            Expression.unaryOp(op, expr, ?loc=range), stmts

        | Fable.Binary(op, TransformExpr com ctx (left, stmts), TransformExpr com ctx (right, stmts')) ->
            Expression.binOp(left, op, right, ?loc=range), stmts @ stmts'

        | Fable.Logical(op, TransformExpr com ctx (left, stmts), TransformExpr com ctx (right, stmts')) ->
            Expression.boolOp(op, [left; right], ?loc=range), stmts @ stmts'

    let transformEmit (com: IPythonCompiler) ctx range (info: Fable.EmitInfo) =
        let macro = info.Macro
        let info = info.CallInfo
        let thisArg, stmts = info.ThisArg |> Option.map (fun e -> com.TransformAsExpr(ctx, e)) |> Option.toList |> Helpers.unzipArgs
        let exprs, stmts' = transformCallArgs com ctx info.HasSpread info.Args

        let args =
            exprs
            |> List.append thisArg
        Expression.emit(macro, args, ?loc=range), stmts @ stmts'

    let transformCall (com: IPythonCompiler) ctx range callee (callInfo: Fable.CallInfo) : Expression * Statement list =
        let callee, stmts = com.TransformAsExpr(ctx, callee)
        let args, stmts' = transformCallArgs com ctx callInfo.HasSpread callInfo.Args
        match callInfo.ThisArg with
        | Some(TransformExpr com ctx (thisArg, stmts'')) -> callFunction range callee (thisArg::args), stmts @ stmts' @ stmts''
        | None when callInfo.IsJsConstructor -> Expression.call(callee, args, ?loc=range), stmts @ stmts'
        | None -> callFunction range callee args, stmts @ stmts'

    let transformCurriedApply com ctx range (TransformExpr com ctx (applied, stmts)) args =
        match transformCallArgs com ctx false args with
        | [], stmts' -> callFunction range applied [], stmts @ stmts'
        | args, stmts' -> (applied, args) ||> List.fold (fun e arg -> callFunction range e [arg]), stmts @ stmts'

    let transformCallAsStatements com ctx range t returnStrategy callee callInfo =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args + (if Option.isSome i.ThisArg then 1 else 0)
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return|ReturnUnit), Some tc when tc.IsRecursiveRef(callee)
                                            && argsLen callInfo = List.length tc.Args ->
            let args =
                match callInfo.ThisArg with
                | Some thisArg -> thisArg::callInfo.Args
                | None -> callInfo.Args
            optimizeTailCall com ctx range tc args
        | _ ->
            let expr, stmts = transformCall com ctx range callee callInfo
            stmts @ [ expr |> resolveExpr t returnStrategy ]

    let transformCurriedApplyAsStatements com ctx range t returnStrategy callee args =
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return|ReturnUnit), Some tc when tc.IsRecursiveRef(callee)
                                            && List.sameLength args tc.Args ->
            optimizeTailCall com ctx range tc args
        | _ ->
            let expr, stmts = transformCurriedApply com ctx range callee args
            stmts @ [ expr |> resolveExpr t returnStrategy ]

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IPythonCompiler) ctx ret expr: Statement list =
        com.TransformAsStatements(ctx, ret, expr)

    let transformTryCatch com ctx r returnStrategy (body, (catch: option<Fable.Ident * Fable.Expr>), finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handlers =
            catch |> Option.map (fun (param, body) ->
                let body = transformBlock com ctx returnStrategy body
                let exn = Expression.identifier("Exception") |> Some
                let identifier = ident param
                [ ExceptHandler.exceptHandler (``type`` = exn, name = identifier, body = body) ])
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [ Statement.try'(transformBlock com ctx returnStrategy body,
            ?handlers=handlers, ?finalBody=finalizer, ?loc=r) ]

    let rec transformIfStatement (com: IPythonCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
        let expr, stmts = com.TransformAsExpr(ctx, guardExpr)
        match expr with
        | Constant(value=value) when (value :? bool) ->
            match value with
            | :? bool as value when value -> stmts @ com.TransformAsStatements(ctx, ret, thenStmnt)
            | _ -> stmts @ com.TransformAsStatements(ctx, ret, elseStmnt)
        | guardExpr ->
            let thenStmnt = transformBlock com ctx ret thenStmnt
            let ifStatement =
                match com.TransformAsStatements(ctx, ret, elseStmnt) with
                | [ ] -> Statement.if'(guardExpr, thenStmnt, ?loc=r)
                | [ elseStmnt ] -> Statement.if'(guardExpr, thenStmnt, [ elseStmnt ], ?loc=r)
                | statements -> Statement.if'(guardExpr, thenStmnt, statements, ?loc=r)
                |> List.singleton
            stmts @ ifStatement

    let transformGet (com: IPythonCompiler) ctx range typ fableExpr kind =
        match kind with
        | Fable.ByKey key ->
            let fableExpr =
                match fableExpr with
                // If we're accessing a virtual member with default implementation (see #701)
                // from base class, we can use `super` in JS so we don't need the bound this arg
                | Fable.Value(Fable.BaseValue(_,t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            match key with
            | Fable.ExprKey(TransformExpr com ctx (prop, stmts')) ->
                let expr, stmts'' = getExpr range expr prop
                expr, stmts @ stmts' @ stmts''
            | Fable.FieldKey field -> get range expr field.Name, stmts

        | Fable.ListHead ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            libCall com ctx range "List" "head" [ expr ], stmts

        | Fable.ListTail ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            libCall com ctx range "List" "tail" [ expr ], stmts

        | Fable.TupleIndex index ->
            match fableExpr with
            // TODO: Check the erased expressions don't have side effects?
            | Fable.Value(Fable.NewTuple exprs, _) ->
                com.TransformAsExpr(ctx, List.item index exprs)
            | TransformExpr com ctx (expr, stmts) ->
                let expr, stmts' = getExpr range expr (ofInt index)
                expr, stmts @ stmts'

        | Fable.OptionValue ->
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            if mustWrapOption typ || com.Options.Language = TypeScript
            then libCall com ctx None "Option" "value" [ expr ], stmts
            else expr, stmts

        | Fable.UnionTag ->
            let expr, stmts = getUnionExprTag com ctx range fableExpr
            expr, stmts

        | Fable.UnionField(index, _) ->
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            let expr, stmts' = getExpr None expr (Expression.constant("fields"))
            let expr, stmts'' = getExpr range expr (ofInt index)
            expr, stmts @ stmts' @ stmts''

    let transformSet (com: IPythonCompiler) ctx range fableExpr (value: Fable.Expr) kind =
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
        let value', stmts' = com.TransformAsExpr(ctx, value)
        let value = value' |> wrapIntExpression value.Type
        let ret, stmts =
            match kind with
            | None -> expr, stmts @ stmts'
            | Some(Fable.FieldKey fi) -> get None expr fi.Name, stmts @ stmts'
            | Some(Fable.ExprKey(TransformExpr com ctx (e, stmts''))) ->
                let expr, stmts''' = getExpr None expr e
                expr, stmts @ stmts' @ stmts'' @ stmts'''
        assign range ret value, stmts

    // let transformBindingExprBody (com: IPythonCompiler) (ctx: Context) (var: Fable.Ident) (value: Fable.Expr) =
    //     match value with
    //     | Function(args, body) ->
    //         let name = Some var.Name
    //         transformFunction com ctx name args body
    //         |> makeArrowFunctionExpression name
    //     | _ ->
    //         if var.IsMutable then
    //             com.TransformAsExpr(ctx, value)
    //         else
    //             let expr, stmts = com.TransformAsExpr(ctx, value)
    //             expr |> wrapIntExpression value.Type, stmts

    // let transformBindingAsExpr (com: IPythonCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
    //     let expr, stmts = transformBindingExprBody com ctx var value
    //     expr |> assign None (identAsExpr var), stmts

    // let transformBindingAsStatements (com: IPythonCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
    //     if isJsStatement ctx false value then
    //         let varName, varExpr = Expression.name(var.Name), identAsExpr var
    //         let decl = Statement.assign([varName], varExpr)
    //         let body = com.TransformAsStatements(ctx, Some(Assign varExpr), value)
    //         List.append [ decl ] body
    //     else
    //         let value, stmts = transformBindingExprBody com ctx var value
    //         let varName = Expression.name(var.Name)
    //         let decl = varDeclaration varName var.IsMutable value
    //         [ decl ]

    let transformTest (com: IPythonCompiler) ctx range kind expr: Expression * Statement list =
        match kind with
        | Fable.TypeTest t ->
            transformTypeTest com ctx range expr t
        | Fable.OptionTest nonEmpty ->
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            Expression.binOp(expr, op, Expression.none(), ?loc=range), stmts
        | Fable.ListTest nonEmpty ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            // let op = if nonEmpty then BinaryUnequal else BinaryEqual
            // Expression.binaryExpression(op, get None expr "tail", Expression.none(), ?loc=range)
            let expr =
                let expr = libCall com ctx range "List" "isEmpty" [ expr ]
                if nonEmpty then Expression.unaryOp(UnaryNot, expr, ?loc=range) else expr
            expr, stmts
        | Fable.UnionCaseTest tag ->
            let expected = ofInt tag
            let actual, stmts = getUnionExprTag com ctx None expr
            Expression.compare(actual, [Eq], [ expected ], ?loc=range), stmts

    // let transformSwitch (com: IPythonCompiler) ctx useBlocks returnStrategy evalExpr cases defaultCase: Statement =
    //     let cases =
    //         cases |> List.collect (fun (guards, expr) ->
    //             // Remove empty branches
    //             match returnStrategy, expr, guards with
    //             | None, Fable.Value(Fable.UnitConstant,_), _
    //             | _, _, [] -> []
    //             | _, _, guards ->
    //                 let guards, lastGuard = List.splitLast guards
    //                 let guards = guards |> List.map (fun e -> SwitchCase.switchCase([||], com.TransformAsExpr(ctx, e)))
    //                 let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
    //                 let caseBody =
    //                     match returnStrategy with
    //                     | Some Return -> caseBody
    //                     | _ -> Array.append caseBody [|Statement.break'()|]
    //                 guards @ [SwitchCase.switchCase(caseBody, com.TransformAsExpr(ctx, lastGuard))]
    //             )
    //     let cases =
    //         match defaultCase with
    //         | Some expr ->
    //             let defaultCaseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
    //             cases @ [SwitchCase.switchCase(consequent defaultCaseBody)]
    //         | None -> cases
    //     Statement.switchStatement(com.TransformAsExpr(ctx, evalExpr), List.toArray cases)

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.sameLength idents values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IPythonCompiler) (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues
        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr)::bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements)
            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccessAsExpr (com: IPythonCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)

    // let transformDecisionTreeSuccessAsStatements (com: IPythonCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement list =
    //     match returnStrategy with
    //     | Some(Target targetId) as target ->
    //         let idents, _ = getDecisionTarget ctx targetIndex
    //         let assignments =
    //             matchTargetIdentAndValues idents boundValues
    //             |> List.collect (fun (id, TransformExpr com ctx (value, stmts)) ->
    //                 let stmt = assign None (identAsExpr id) value |> Statement.expr
    //                 stmts @ [ stmt ])
    //         let targetAssignment = assign None (targetId |> Expression.name) (ofInt targetIndex) |> Statement.expr
    //         [ targetAssignment ] @ assignments
    //     | ret ->
    //         let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
    //         let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toList
    //         bindings @ (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.Binary(BinaryEqualStrict, expr, right), _, _) ->
                Some(expr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number Int32, None)
                let right = Fable.NumberConstant(float tag, Int32) |> makeValue None
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | _ -> false
        let rec checkInner cases evalExpr = function
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _)
                                    when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                    let cases = (caseExpr, targetIndex, boundValues)::cases |> List.rev
                    Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr -> checkInner ((caseExpr, targetIndex, boundValues)::cases) evalExpr treeExpr
            | _ -> None
        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
            match checkInner [caseExpr, targetIndex, boundValues] evalExpr treeExpr with
            | Some(evalExpr, cases, defaultCase) ->
                Some(evalExpr, cases, defaultCase)
            | None -> None
        | _ -> None

    let transformDecisionTreeAsExpr (com: IPythonCompiler) (ctx: Context) targets expr: Expression * Statement list =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let groupSwitchCases t (cases: (Fable.Expr * int * Fable.Expr list) list) (defaultIndex, defaultBoundValues) =
        cases
        |> List.groupBy (fun (_,idx,boundValues) ->
            // Try to group cases with some target index and empty bound values
            // If bound values are non-empty use also a non-empty Guid to prevent grouping
            if List.isEmpty boundValues
            then idx, System.Guid.Empty
            else idx, System.Guid.NewGuid())
        |> List.map (fun ((idx,_), cases) ->
            let caseExprs = cases |> List.map Tuple3.item1
            // If there are multiple cases, it means boundValues are empty
            // (see `groupBy` above), so it doesn't mind which one we take as reference
            let boundValues = cases |> List.head |> Tuple3.item3
            caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t))
        |> function
            | [] -> []
            // Check if the last case can also be grouped with the default branch, see #2357
            | cases when List.isEmpty defaultBoundValues ->
                match List.splitLast cases with
                | cases, (_, Fable.DecisionTreeSuccess(idx, [], _))
                    when idx = defaultIndex -> cases
                | _ -> cases
            | cases -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int,int>) = function
            | [] -> targetRefs
            | expr::exprs ->
                match expr with
                // We shouldn't actually see this, but shortcircuit just in case
                | Fable.DecisionTree _ ->
                    findSuccess targetRefs exprs
                | Fable.DecisionTreeSuccess(idx,_,_) ->
                    let count =
                        Map.tryFind idx targetRefs
                        |> Option.defaultValue 0
                    let targetRefs = Map.add idx (count + 1) targetRefs
                    findSuccess targetRefs exprs
                | expr ->
                    let exprs2 = FableTransforms.getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)
        findSuccess Map.empty [expr] |> Seq.choose (fun kv ->
            if kv.Value > 1 then Some kv.Key else None) |> Seq.toList

    /// When several branches share target create first a switch to get the target index and bind value
    /// and another to execute the actual target
    // let transformDecisionTreeWithTwoSwitches (com: IPythonCompiler) ctx returnStrategy
    //                 (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
    //     // Declare target and bound idents
    //     let targetId = getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent |> ident
    //     let multiVarDecl =
    //         let boundIdents =
    //             targets |> List.collect (fun (idents,_) ->
    //             idents) |> List.map (fun id -> ident id, None)
    //         multiVarDeclaration ((targetId,None)::boundIdents)
    //     // Transform targets as switch
    //     let switch2 =
    //         // TODO: Declare the last case as the default case?
    //         let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
    //         transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None
    //     // Transform decision tree
    //     let targetAssign = Target(ident targetId)
    //     let ctx = { ctx with DecisionTargets = targets }
    //     match transformDecisionTreeAsSwitch treeExpr with
    //     | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
    //         let cases = groupSwitchCases (Fable.Number Int32) cases (defaultIndex, defaultBoundValues)
    //         let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number Int32)
    //         let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
    //         [ multiVarDecl; switch1; switch2 ]
    //     | None ->
    //         let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
    //         [ yield multiVarDecl; yield! decisionTree; yield switch2 ]

    // let transformDecisionTreeAsStatements (com: IPythonCompiler) (ctx: Context) returnStrategy
    //                     (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement list =
    //     // If some targets are referenced multiple times, hoist bound idents,
    //     // resolve the decision index and compile the targets as a switch
    //     let targetsWithMultiRefs =
    //         if com.Options.Language = TypeScript then [] // no hoisting when compiled with types
    //         else getTargetsWithMultipleReferences treeExpr
    //     match targetsWithMultiRefs with
    //     | [] ->
    //         let ctx = { ctx with DecisionTargets = targets }
    //         match transformDecisionTreeAsSwitch treeExpr with
    //         | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
    //             let t = treeExpr.Type
    //             let cases = cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->
    //                 [caseExpr], Fable.DecisionTreeSuccess(targetIndex, boundValues, t))
    //             let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
    //             [ transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase) ]
    //         | None ->
    //             com.TransformAsStatements(ctx, returnStrategy, treeExpr)
    //     | targetsWithMultiRefs ->
    //         // If the bound idents are not referenced in the target, remove them
    //         let targets =
    //             targets |> List.map (fun (idents, expr) ->
    //                 idents
    //                 |> List.exists (fun i -> FableTransforms.isIdentUsed i.Name expr)
    //                 |> function
    //                     | true -> idents, expr
    //                     | false -> [], expr)
    //         let hasAnyTargetWithMultiRefsBoundValues =
    //             targetsWithMultiRefs |> List.exists (fun idx ->
    //                 targets.[idx] |> fst |> List.isEmpty |> not)
    //         if not hasAnyTargetWithMultiRefsBoundValues then
    //             match transformDecisionTreeAsSwitch treeExpr with
    //             | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
    //                 let t = treeExpr.Type
    //                 let cases = groupSwitchCases t cases (defaultIndex, defaultBoundValues)
    //                 let ctx = { ctx with DecisionTargets = targets }
    //                 let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
    //                 [ transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase) ]
    //             | None ->
    //                 transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr
    //         else
    //             transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr

    let rec transformAsExpr (com: IPythonCompiler) ctx (expr: Fable.Expr): Expression * Statement list=
        match expr with
        | Fable.TypeCast(e,t,tag) -> transformCast com ctx t tag e

        | Fable.Curry(e, arity, _, r) -> transformCurry com ctx r e arity

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> identAsExpr id, []

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path, []

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformFunction com ctx name [arg] body
            ||> makeArrowFunctionExpression

        | Fable.Delegate(args, body, name) ->
            transformFunction com ctx name args body
            ||> makeArrowFunctionExpression

        //  FIXME:
        //| Fable.ObjectExpr (members, _, baseCall) ->
        //   transformObjectExpr com ctx members baseCall

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.CurriedApply(callee, args, _, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, _, range) ->
            transformOperation com ctx range kind

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(TransformExpr com ctx (guardExpr, stmts),
                           TransformExpr com ctx (thenExpr, stmts'),
                           TransformExpr com ctx (elseExpr, stmts''), r) ->
            Expression.ifExp (guardExpr, thenExpr, elseExpr), stmts @ stmts' @ stmts''

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, value, range) ->
            transformSet com ctx range expr value kind

        // | Fable.Let(ident, value, body) ->
        //     if ctx.HoistVars [ident] then
        //         let assignment = transformBindingAsExpr com ctx ident value
        //         Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        // | Fable.LetRec(bindings, body) ->
        //     if ctx.HoistVars(List.map fst bindings) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        // | Fable.Sequential exprs ->
        //     List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
        //     |> Expression.sequenceExpression

        | Fable.Emit(info, _, range) ->
            if info.IsJsStatement then iife com ctx expr
            else transformEmit com ctx range info

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.WhileLoop _ | Fable.ForLoop _ | Fable.TryCatch _ ->
            iife com ctx expr

        | _ -> failwith $"Expression {expr} not supported."

    let rec transformAsStatements (com: IPythonCompiler) ctx returnStrategy
                                    (expr: Fable.Expr): Statement list =
        match expr with
        | Fable.TypeCast(e, t, tag) ->
            let expr, stmts = transformCast com ctx t tag e
            stmts @ [ expr |> resolveExpr t returnStrategy ]

        | Fable.Curry(e, arity, t, r) ->
            let expr, stmts = transformCurry com ctx r e arity
            stmts @ [ expr |> resolveExpr t returnStrategy ]

        | Fable.Value(kind, r) ->
            let expr, stmts = transformValue com ctx r kind
            stmts @ [ expr |> resolveExpr kind.Type returnStrategy ]

        | Fable.IdentExpr id ->
            [ identAsExpr id |> resolveExpr id.Type returnStrategy ]

        | Fable.Import({ Selector = selector; Path = path }, t, r) ->
            [ transformImport com ctx r selector path |> resolveExpr t returnStrategy ]

        | Fable.Test(expr, kind, range) ->
            let expr, stmts = transformTest com ctx range kind expr
            stmts @ [ expr |> resolveExpr Fable.Boolean returnStrategy ]

        | Fable.Lambda(arg, body, name) ->
            let args, body = transformFunction com ctx name [arg] body
            let expr', stmts = makeArrowFunctionExpression args body
            stmts @ [ expr' |> resolveExpr expr.Type returnStrategy ]

        | Fable.Delegate(args, body, name) ->
            let args, body = transformFunction com ctx name args body
            let expr', stmts = makeArrowFunctionExpression args body
            stmts @ [ expr' |> resolveExpr expr.Type returnStrategy ]

        // | Fable.ObjectExpr (members, t, baseCall) ->
        //     let expr, stmts = transformObjectExpr com ctx members baseCall
        //     stmts @ [ expr |> resolveExpr t returnStrategy ]

        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements com ctx range typ returnStrategy callee info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApplyAsStatements com ctx range typ returnStrategy callee args

        | Fable.Emit(info, t, range) ->
            let e, stmts = transformEmit com ctx range info
            if info.IsJsStatement then
                stmts @ [ Statement.expr(e) ] // Ignore the return strategy
            else stmts @ [ resolveExpr t returnStrategy e ]

        | Fable.Operation(kind, t, range) ->
            let expr, stmts = transformOperation com ctx range kind
            stmts @ [ expr |> resolveExpr t returnStrategy ]

        | Fable.Get(expr, kind, t, range) ->
            let expr, stmts = transformGet com ctx range t expr kind
            stmts @ [ expr |> resolveExpr t returnStrategy ]

        // | Fable.Let(ident, value, body) ->
        //     let binding = transformBindingAsStatements com ctx ident value
        //     List.append binding (transformAsStatements com ctx returnStrategy body)

        // | Fable.LetRec(bindings, body) ->
        //     let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toList
        //     List.append bindings (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, kind, value, range) ->
            let expr', stmts = transformSet com ctx range expr value kind
            stmts @ [ expr' |> resolveExpr expr.Type returnStrategy ]

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            let asStatement =
                match returnStrategy with
                | None | Some ReturnUnit -> true
                | Some(Target _) -> true // Compile as statement so values can be bound
                | Some(Assign _) -> (isJsStatement ctx false thenExpr) || (isJsStatement ctx false elseExpr)
                | Some Return ->
                    Option.isSome ctx.TailCallOpportunity
                    || (isJsStatement ctx false thenExpr) || (isJsStatement ctx false elseExpr)
            if asStatement then
                transformIfStatement com ctx r returnStrategy guardExpr thenExpr elseExpr
            else
                let guardExpr', stmts = transformAsExpr com ctx guardExpr
                let thenExpr', stmts' = transformAsExpr com ctx thenExpr
                let elseExpr', stmts'' = transformAsExpr com ctx elseExpr
                stmts
                @ stmts'
                @ stmts''
                @ [ Expression.ifExp(guardExpr', thenExpr', elseExpr', ?loc=r) |> resolveExpr thenExpr.Type returnStrategy ]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapiToArray (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                com.TransformAsStatements(ctx, ret, statement))
            |> List.concat

        | Fable.TryCatch (body, catch, finalizer, r) ->
            transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

        // | Fable.DecisionTree(expr, targets) ->
        //     transformDecisionTreeAsStatements com ctx returnStrategy targets expr

        // | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
        //     transformDecisionTreeSuccessAsStatements com ctx returnStrategy idx boundValues

        | Fable.WhileLoop(TransformExpr com ctx (guard, stmts), body, range) ->
            stmts @ [ Statement.while'(guard, transformBlock com ctx None body, ?loc=range) ]

        // | Fable.ForLoop (var, TransformExpr com ctx (start, stmts), TransformExpr com ctx (limit, stmts'), body, isUp, range) ->
        //     let op1, op2 =
        //         if isUp
        //         then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
        //         else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus

        //     let a = start |> varDeclaration (typedIdent com ctx var |> Pattern.Identifier) true

        //     [ Statement.for'(
        //         transformBlock com ctx None body,
        //         start |> varDeclaration (Expression.identifier(ident var)) true,
        //         Expression.binOp(identAsExpr var, op1, limit),
        //         Expression.updateExpression(op2, false, identAsExpr var), ?loc=range)  ]

        | _ -> failwith $"Expression {expr} not supported."

    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr): Arg list * Statement list =
        let tailcallChance =
            Option.map (fun name ->
                NamedTailCallOpportunity(com, ctx, name, args) :> ITailCallOpportunity) name
        let args = discardUnitArg args
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with TailCallOpportunity = tailcallChance
                       HoistVars = fun ids -> declaredVars.AddRange(ids); true
                       OptimizeTailCall = fun () -> isTailCallOptimized <- true }
        let body =
            if body.Type = Fable.Unit then
                transformBlock com ctx (Some ReturnUnit) body
            elif isJsStatement ctx (Option.isSome tailcallChance) body then
                transformBlock com ctx (Some Return) body
            else
                transformAsExpr com ctx body |> wrapExprInBlockWithReturn
        let args, body =
            match isTailCallOptimized, tailcallChance with
            | true, Some tc ->
                // Replace args, see NamedTailCallOpportunity constructor
                let args' =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) -> id)
                let varDecls =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) -> ident id, Some (Expression.identifier(tcArg)))
                    |> multiVarDeclaration

                let body = varDecls @ body
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body = body @ [ Statement.break'() ]
                args', Statement.while'(Expression.constant(true), body)
                |> List.singleton
            | _ -> args |> List.map id, body
        let body =
            if declaredVars.Count = 0 then body
            else
                let varDeclStatement = multiVarDeclaration [for v in declaredVars -> ident v, None]
                List.append varDeclStatement body
        args |> List.map (ident >> Arg.arg), body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = emitExpression None "typeof process === 'object' ? process.argv.slice(2) : []" []
        let main = Expression.call(funcExpr, [ argv ])
        // Don't exit the process after leaving main, as there may be a server running
        // Statement.expr(emitExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        Statement.expr(main)

    let declareModuleMember isPublic (membName: string) isMutable (expr: Expression) =
        let membName = Expression.name(membName)
        match expr with
        // | ClassExpression(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
        //     Declaration.classDeclaration(
        //         body,
        //         ?id = Some membName,
        //         ?superClass = superClass,
        //         ?superTypeParameters = superTypeParameters,
        //         ?typeParameters = typeParameters,
        //         ?implements = implements)
        // | FunctionExpression(_, ``params``, body, returnType, typeParameters, _) ->
        //     Declaration.functionDeclaration(
        //         ``params``, body, membName,
        //         ?returnType = returnType,
        //         ?typeParameters = typeParameters)
        | _ ->
            printfn $"declareModuleMember: Got {expr}"
            varDeclaration membName isMutable expr
        // if not isPublic then PrivateModuleDeclaration(decl |> Declaration)
        // else ExportNamedDeclaration(decl)

//     let makeEntityTypeParamDecl (com: IPythonCompiler) _ctx (ent: Fable.Entity) =
//         if com.Options.Language = TypeScript then
//             getEntityGenParams ent |> makeTypeParamDecl
//         else
//             None

//     let getClassImplements com ctx (ent: Fable.Entity) =
//         let mkNative genArgs typeName =
//             let id = Identifier.identifier(typeName)
//             let typeParamInst = makeGenTypeParamInst com ctx genArgs
//             ClassImplements.classImplements(id, ?typeParameters=typeParamInst) |> Some
// //        let mkImport genArgs moduleName typeName =
// //            let id = makeImportTypeId com ctx moduleName typeName
// //            let typeParamInst = makeGenTypeParamInst com ctx genArgs
// //            ClassImplements(id, ?typeParameters=typeParamInst) |> Some
//         ent.AllInterfaces |> Seq.choose (fun ifc ->
//             match ifc.Entity.FullName with
//             | "Fable.Core.JS.Set`1" -> mkNative ifc.GenericArgs "Set"
//             | "Fable.Core.JS.Map`2" -> mkNative ifc.GenericArgs "Map"
//             | _ -> None
//         )

    let getUnionFieldsAsIdents (_com: IPythonCompiler) _ctx (_ent: Fable.Entity) =
        let tagId = makeTypedIdent (Fable.Number Int32) "tag"
        let fieldsId = makeTypedIdent (Fable.Array Fable.Any) "fields"
        [| tagId; fieldsId |]

    let getEntityFieldsAsIdents _com (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> Naming.sanitizeIdentForbiddenChars |> Naming.checkJsKeywords
            let typ = field.FieldType
            let id: Fable.Ident = { makeTypedIdent typ name with IsMutable = field.IsMutable }
            id)
        |> Seq.toArray

    let getEntityFieldsAsProps (com: IPythonCompiler) ctx (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            getUnionFieldsAsIdents com ctx ent
            |> Array.map (fun id ->
                let prop = identAsExpr id
                //let ta = typeAnnotation com ctx id.Type
                prop)
        else
            ent.FSharpFields
            |> Seq.map (fun field ->
                let prop, computed = memberFromName field.Name
                //let ta = typeAnnotation com ctx field.FieldType
                let isStatic = if field.IsStatic then Some true else None
                prop)
            |> Seq.toArray

    let declareClassType (com: IPythonCompiler) ctx (ent: Fable.Entity) entName (consArgs: Arg list) (consBody: Statement list) (baseExpr: Expression option) classMembers =
        //let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let classCons = makeClassConstructor consArgs consBody
        let classFields = Array.empty
        let classMembers = List.append [ classCons ] classMembers
        let classBody = [ yield! classFields; yield! classMembers ]
        let bases = baseExpr |> Option.toList
        //let classExpr = Expression.classExpression(classBody, ?superClass=baseExpr, ?typeParameters=typeParamDecl, ?implements=implements)
        let name =  Helpers.getUniqueIdentifier "Lifted"
        let classStmt = Statement.classDef(name, body = classBody, bases = bases)
        let expr = Expression.name(name)
        expr |> declareModuleMember ent.IsPublic entName false

    let declareType (com: IPythonCompiler) ctx (ent: Fable.Entity) entName (consArgs: Arg list) (consBody: Statement list) baseExpr classMembers : Statement list =
        let typeDeclaration = declareClassType com ctx ent entName consArgs consBody baseExpr classMembers
        let reflectionDeclaration, stmts =
            let ta = None
            let genArgs = Array.init (ent.GenericParameters.Length) (fun i -> "gen" + string i |> makeIdent)
            let generics = genArgs |> Array.mapToList identAsExpr
            let body, stmts = transformReflectionInfo com ctx None ent generics
            let args = genArgs |> Array.mapToList (ident >> Arg.arg)
            let returnType = ta
            let expr, stmts' = makeFunctionExpression None (args, body)
            expr |> declareModuleMember ent.IsPublic (entName + Naming.reflectionSuffix) false, stmts @ stmts'
        stmts @ [typeDeclaration; reflectionDeclaration]

    let transformModuleFunction (com: IPythonCompiler) ctx (info: Fable.MemberInfo) (membName: string) args body =
        let args, body =
            getMemberArgsAndBody com ctx (NonAttached membName) info.HasSpread args body
        //let expr = Expression.functionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)
        let name = Helpers.getUniqueIdentifier "lifted"
        let stmt = FunctionDef.Create(name = name, args = Arguments.arguments args, body = body)
        let expr = Expression.name (name)
        info.Attributes
        |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)
        |> function
        | true ->  [ stmt; declareEntryPoint com ctx expr ]
        | false -> [ stmt; declareModuleMember info.IsPublic membName false expr ]

    let transformAction (com: IPythonCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        // let hasVarDeclarations =
        //     statements |> List.exists (function
        //         | Declaration(_) -> true
        //         | _ -> false)
        // if hasVarDeclarations then
        //     [ Expression.call(Expression.functionExpression([||], BlockStatement(statements)), [||])
        //       |> Statement.expr |> PrivateModuleDeclaration ]
        //else
        statements

    let transformAttachedProperty (com: IPythonCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not memb.Info.IsInstance
        //let kind = if memb.Info.IsGetter then ClassGetter else ClassSetter
        let args, body =
            getMemberArgsAndBody com ctx (Attached isStatic) false memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        let arguments = Arguments.arguments args
        FunctionDef.Create(Identifier memb.Name, arguments, body = body)
        |> List.singleton
        //ClassMember.classMethod(kind, key, args, body, computed_=computed, ``static``=isStatic)

    let transformAttachedMethod (com: IPythonCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not memb.Info.IsInstance
        let makeMethod name args body =
            let key, computed = memberFromName name
            //ClassMember.classMethod(ClassFunction, key, args, body, computed_=computed, ``static``=isStatic)
            FunctionDef.Create(Identifier name, args, body = body)
        let args, body =
            getMemberArgsAndBody com ctx (Attached isStatic) memb.Info.HasSpread memb.Args memb.Body
        let arguments = Arguments.arguments args
        [
            yield makeMethod memb.Name arguments body
            if memb.Info.IsEnumerator then
                yield makeMethod "Symbol.iterator" (Arguments.arguments []) (enumerator2iterator com ctx)
        ]

    let transformUnion (com: IPythonCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getUnionFieldsAsIdents com ctx ent
        let args =
            [ fieldIds.[0] |> ident |> Arg.arg
              fieldIds.[1] |> ident |> Arg.arg ] //restElement ]
        let body =
            [
                yield callSuperAsStatement []
                yield! fieldIds |> Array.map (fun id ->
                    let left = get None thisExpr id.Name
                    let right =
                        match id.Type with
                        | Fable.Number _ ->
                            Expression.binOp(identAsExpr id, BinaryOrBitwise, Expression.constant(0.))
                        | _ -> identAsExpr id
                    assign None left right |> Statement.expr)
            ]
        let cases =
            let expr, stmts =
                ent.UnionCases
                |> Seq.map (getUnionCaseName >> makeStrConst)
                |> Seq.toList
                |> makeArray com ctx

            let body = stmts @ [ Statement.return'(expr) ]
            let name = Identifier("cases")
            FunctionDef.Create(name, Arguments.arguments [], body = body)

        let baseExpr = libValue com ctx "Types" "Union" |> Some
        let classMembers = List.append [ cases ] classMembers
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithCompilerGeneratedConstructor (com: IPythonCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getEntityFieldsAsIdents com ent
        let args = fieldIds |> Array.map identAsExpr
        let baseExpr =
            if ent.IsFSharpExceptionDeclaration
            then libValue com ctx "Types" "FSharpException" |> Some
            elif ent.IsFSharpRecord || ent.IsValueType
            then libValue com ctx "Types" "Record" |> Some
            else None
        let body = [
                if Option.isSome baseExpr then
                    yield callSuperAsStatement []
                yield! ent.FSharpFields |> Seq.mapi (fun i field ->
                    let left = get None thisExpr field.Name
                    let right = wrapIntExpression field.FieldType args.[i]
                    assign None left right |> Statement.expr)
                |> Seq.toArray
            ]
        let args = fieldIds |> Array.mapToList (ident >> Arg.arg)
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithImplicitConstructor (com: IPythonCompiler) ctx (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
        let classEnt = com.GetEntity(classDecl.Entity)
        let classIdent = Expression.identifier(classDecl.Name)
        let consArgs, consBody =
            getMemberArgsAndBody com ctx ClassConstructor cons.Info.HasSpread cons.Args cons.Body

        let exposedCons, stmts =
            let argExprs = consArgs |> List.map (fun p -> Expression.identifier(p.Arg))
            let exposedConsBody = Expression.call(classIdent, argExprs)
            makeFunctionExpression None (consArgs, exposedConsBody)

        let baseExpr, consBody =
            classDecl.BaseCall
            |> extractBaseExprFromBaseCall com ctx classEnt.BaseType
            |> Option.orElseWith (fun () ->
                if classEnt.IsValueType then Some(libValue com ctx "Types" "Record", ([], []))
                else None)
            |> Option.map (fun (baseExpr, (baseArgs, stmts)) ->
                let consBody =
                    stmts @ consBody
                    |> List.append [ callSuperAsStatement baseArgs ]
                Some baseExpr, consBody)
            |> Option.defaultValue (None, consBody)

        [
            yield! declareType com ctx classEnt classDecl.Name consArgs consBody baseExpr classMembers
            yield declareModuleMember cons.Info.IsPublic cons.Name false exposedCons
        ]

    let rec transformDeclaration (com: IPythonCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                let decls =
                    if decl.Info.IsValue then
                        let value, stmts = transformAsExpr com ctx decl.Body
                        stmts @ [declareModuleMember decl.Info.IsPublic decl.Name decl.Info.IsMutable value]
                    else
                        transformModuleFunction com ctx decl.Info decl.Name decl.Args decl.Body

                if decl.ExportDefault then
                    decls //@ [ ExportDefaultDeclaration(Choice2Of2(Expression.identifier(decl.Name))) ]
                else decls

        | Fable.ClassDeclaration decl ->
            let ent = decl.Entity

            let classMembers =
                decl.AttachedMembers
                |> List.collect (fun memb ->
                    withCurrentScope ctx memb.UsedNames <| fun ctx ->
                        if memb.Info.IsGetter || memb.Info.IsSetter then
                            transformAttachedProperty com ctx memb
                        else
                            transformAttachedMethod com ctx memb)

            match decl.Constructor with
            | Some cons ->
                withCurrentScope ctx cons.UsedNames <| fun ctx ->
                    transformClassWithImplicitConstructor com ctx decl classMembers cons
            | None ->
                let ent = com.GetEntity(ent)
                if ent.IsFSharpUnion then transformUnion com ctx ent decl.Name classMembers
                else transformClassWithCompilerGeneratedConstructor com ctx ent decl.Name classMembers

        //| _ -> failwith $"Declaration {decl} not implemented"

    let transformImports (imports: Import seq) : Statement list =
        let statefulImports = ResizeArray()
        printfn "Imports: %A" imports

        [
            for import in imports do
                match import with
                | { Selector = selector; LocalIdent = local; Path = path } ->
                    let path = path |> Helpers.rewriteFableImport
                    let alias = Alias.alias(Identifier(selector), ?asname=None)
                    Statement.importFrom (Some(Identifier(path)), [ alias ])
        ]

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then None
        else
            match selector with
            | "*" | "default" -> Path.GetFileNameWithoutExtension(path)
            | _ -> selector
            |> getUniqueNameInRootScope ctx
            |> Some

module Compiler =
    open Util

    type BabelCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string,  Import>()

        interface IPythonCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, selector, path, r) =
                let cachedName = path + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> Expression.identifier(localIdent)
                    | None -> Expression.none()
                | false, _ ->
                    let localId = getIdentForImport ctx path selector
                    let i =
                      { Selector =
                            if selector = Naming.placeholder then
                                     "`importMember` must be assigned to a variable"
                                     |> addError com [] r; selector
                            else selector
                        Path = path
                        LocalIdent = localId }
                    imports.Add(cachedName, i)
                    match localId with
                    | Some localId -> Expression.identifier(localId)
                    | None -> Expression.none()

            member _.GetAllImports() = imports.Values :> _
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None selector path
            member bcom.GetIdentifier(ctx, name) = getIdentifier bcom ctx name

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
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = BabelCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IPythonCompiler
        let declScopes =
            let hs = HashSet()
            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)
            hs

        let ctx =
          { File = file
            UsedNames = { RootScope = HashSet file.UsedNamesInRootScope
                          DeclarationScopes = declScopes
                          CurrentDeclarationScope = Unchecked.defaultof<_> }
            DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> ()
            ScopedTypeParams = Set.empty }

        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let importDecls = com.GetAllImports() |> transformImports
        let body = importDecls @ rootDecls
        Module.module' (body)
