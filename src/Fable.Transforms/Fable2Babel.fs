module Fable.Transforms.Fable2Babel

open Fable
open Fable.Core
open Fable.AST
open Fable.AST.Babel
open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices

type ReturnStrategy =
    | Return
    | ReturnUnit
    | Assign of Expression // TODO: Add SourceLocation?
    | Target of Identifier

type Import =
  { Selector: string
    LocalIdent: string option
    Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type Context =
  { File: Fable.File
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string> }

type IBabelCompiler =
    inherit ICompiler
    abstract GetAllImports: unit -> seq<Import>
    abstract GetImportExpr: Context * selector: string * path: string * Fable.ImportKind -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement array
    abstract TransformImport: Context * selector:string * path:string * Fable.ImportKind -> Expression
    abstract TransformObjectExpr: Context * Fable.ObjectMember list * boundThis: string * ?baseCall: Fable.Expr -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr
        -> (Pattern array) * U2<BlockStatement, Expression>

module Util =
    let inline (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformAsExpr(ctx, e)

    let (|FunctionArgs|) = function
        | Fable.Lambda arg -> [arg]
        | Fable.Delegate args -> args

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArgDeclaration && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    type NamedTailCallOpportunity(com: ICompiler, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        let argIds = discardUnitArg args |> List.map (fun arg -> com.GetUniqueVar(arg.Name))
        interface ITailCallOpportunity with
            member __.Label = name
            member __.Args = argIds
            member __.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id -> name = id.Name | _ -> false

    let prepareBoundThis (boundThis: string) (args: Fable.Ident list) =
        match args with
        | thisArg::args -> Some(boundThis, thisArg), args
        | _ -> failwith "Expecting thisArg to be first element of argument list"

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf "Cannot find DecisionTree target %i" targetIndex
        | Some(idents, target) -> idents, target

    let rec isJsStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Value _ | Fable.Import _ | Fable.DelayedResolution _ | Fable.Test _ | Fable.IdentExpr _ | Fable.Function _
        | Fable.ObjectExpr _ | Fable.Operation _ | Fable.Get _ | Fable.TypeCast _ -> false

        | Fable.TryCatch _ | Fable.Debugger _
        | Fable.Sequential _ | Fable.Let _ | Fable.Set _
        | Fable.Loop _ | Fable.Throw _ -> true

        | Fable.DecisionTreeSuccess(targetIndex,_, _) ->
            getDecisionTarget ctx targetIndex
            |> snd |> isJsStatement ctx preferStatement

        // TODO: Make it also statement if we have more than, say, 3 targets?
        // This will increase the chances to convert it into a switch
        | Fable.DecisionTree(_,targets) ->
            preferStatement
            || List.exists (snd >> (isJsStatement ctx false)) targets

        | Fable.IfThenElse(_,thenExpr,elseExpr,_) ->
            preferStatement || isJsStatement ctx false thenExpr || isJsStatement ctx false elseExpr

    let addErrorAndReturnNull (com: ICompiler) (range: SourceLocation option) (error: string) =
        addError com [] range error
        NullLiteral () :> Expression

    let toPattern (e: PatternExpression): Pattern =
        U2.Case2 e

    let ident (id: Fable.Ident) =
        Identifier(id.Name, ?loc=id.Range)

    let identAsPattern (id: Fable.Ident) =
        ident id |> toPattern

    let identAsExpr (id: Fable.Ident) =
        (ident id) :> Expression

    let thisExpr =
        ThisExpression() :> Expression

    let ofInt i =
        NumericLiteral(float i) :> Expression

    let ofString s =
        StringLiteral s :> Expression

    let memberFromName memberName: Expression * bool =
        if Naming.hasIdentForbiddenChars memberName
        then upcast StringLiteral(memberName), true
        else upcast Identifier(memberName), false

    let memberFromExpr (com: IBabelCompiler) ctx memberExpr: Expression * bool =
        match memberExpr with
        | Fable.Value(Fable.StringConstant name, _) -> memberFromName name
        | e -> com.TransformAsExpr(ctx, e), true

    let get r left memberName =
        let expr, computed = memberFromName memberName
        MemberExpression(left, expr, computed, ?loc=r) :> Expression

    let getExpr r (object: Expression) (expr: Expression) =
        let expr, computed =
            match expr with
            | :? StringLiteral as e -> memberFromName e.Value
            | e -> e, true
        MemberExpression(object, expr, computed, ?loc=r) :> Expression

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

    let jsObject methodName args =
        CallExpression(get None (Identifier "Object") methodName, args) :> Expression

    let coreUtil (com: IBabelCompiler) ctx memberName args =
        CallExpression(com.TransformImport(ctx, memberName, "Util", Fable.Library), args) :> Expression

    let coreLibCall (com: IBabelCompiler) ctx r moduleName memberName args =
        CallExpression(com.TransformImport(ctx, memberName, moduleName, Fable.Library), args, ?loc=r) :> Expression

    let coreLibConstructorCall (com: IBabelCompiler) ctx moduleName memberName args =
        NewExpression(com.TransformImport(ctx, memberName, moduleName, Fable.Library), args) :> Expression

    let coreValue (com: IBabelCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, moduleName, Fable.Library)

    let coreReflectionCall (com: IBabelCompiler) ctx r memberName args =
        coreLibCall com ctx r "Reflection" (memberName + "_type") args

    let tryJsConstructor (com: IBabelCompiler) ctx ent =
        match Replacements.tryJsConstructor com ent with
        | Some e -> com.TransformAsExpr(ctx, e) |> Some
        | None -> None

    let jsConstructor (com: IBabelCompiler) ctx ent =
        let entRef = Replacements.jsConstructor com ent
        com.TransformAsExpr(ctx, entRef)

    let makeList com ctx headAndTail =
        match headAndTail with
        | None -> [||]
        | Some(TransformExpr com ctx head, TransformExpr com ctx tail) -> [|head; tail|]
        |> coreLibConstructorCall com ctx "Types" "List"

    // TODO: range
    let makeArray (com: IBabelCompiler) ctx exprs =
        List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
        |> ArrayExpression :> Expression

    let makeTypedArray (com: IBabelCompiler) ctx typ (arrayKind: Fable.NewArrayKind) =
        match typ, arrayKind with
        | Fable.Number kind, _ when com.Options.typedArrays ->
            let jsName = getTypedArrayName com kind
            let args =
                match arrayKind with
                | Fable.ArrayValues args ->
                    [| List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) args
                       |> ArrayExpression :> Expression |]
                | Fable.ArrayAlloc(TransformExpr com ctx size) -> [|size|]
            NewExpression(Identifier jsName, args) :> Expression
        | _, Fable.ArrayAlloc(TransformExpr com ctx size) ->
            upcast NewExpression(Identifier "Array", [|size|])
        | _, Fable.ArrayValues exprs ->
            makeArray com ctx exprs

    let makeStringArray strings =
        strings
        |> List.mapToArray (fun x -> StringLiteral x :> Expression)
        |> ArrayExpression :> Expression

    let makeJsObject pairs =
        pairs |> Seq.map (fun (name, value) ->
            let prop, computed = memberFromName name
            ObjectProperty(prop, value, computed_=computed) |> U3.Case1)
        |> Seq.toArray
        |> ObjectExpression :> Expression

    let assign range left right =
        AssignmentExpression(AssignEqual, left, right, ?loc=range)
        :> Expression

    /// Immediately Invoked Function Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction(ctx, None, [], expr)
        // Use an arrow function in case we need to capture `this`
        CallExpression(ArrowFunctionExpression([||], body), [||])

    let multiVarDeclaration kind (variables: (Identifier * Expression option) list) =
        let varDeclarators =
            // TODO: Log error if there're duplicated non-empty var declarations
            variables
            |> List.distinctBy (fun (id, value) -> id.Name)
            |> List.mapToArray (fun (id, value) ->
                VariableDeclarator(id |> toPattern, ?init=value))
        VariableDeclaration(kind, varDeclarators) :> Statement

    let varDeclaration (var: Identifier) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration(toPattern var, value, kind, ?loc=addRanges[var.Loc; value.Loc])

    let restElement (var: Identifier) =
        RestElement(toPattern var) :> PatternNode |> U2.Case1

    let callSuperConstructor r _funcExpr _thisArg (args: Expression list) =
        CallExpression(Super(?loc=r), List.toArray args, ?loc=r) :> Expression

    let callFunctionWithThisContext r funcExpr thisArg (args: Expression list) =
        CallExpression(get None funcExpr "call", List.toArray (thisArg::args), ?loc=r) :> Expression

    let macroExpression range (txt: string) args =
        MacroExpression(txt, List.toArray args, ?loc=range) :> Expression

    let getGenericTypeParams (types: Fable.Type list) =
        let rec getGenParams = function
            | Fable.GenericParam name -> [name]
            | t -> t.Generics |> List.collect getGenParams
        types
        |> List.collect getGenParams
        |> Set.ofList

    let getEntityGenParams (ent: FSharpEntity) =
        ent.GenericParameters
        |> Seq.map (fun x -> x.Name)
        |> Set.ofSeq

    let makeTypeParamDecl genParams =
        if (Set.isEmpty genParams) then
            None
        else
            genParams
            |> Set.toArray
            |> Array.map TypeParameter
            |> TypeParameterDeclaration |> Some

    let makeTypeParamInst genParams =
        if (Set.isEmpty genParams) then
            None
        else
            genParams
            |> Set.toArray
            |> Array.map (fun x -> GenericTypeAnnotation(Identifier(x)) :> TypeAnnotationInfo)
            |> TypeParameterInstantiation |> Some

    let mergeTypeParamDecls (decl1: TypeParameterDeclaration option) (decl2: TypeParameterDeclaration option) =
        match decl1, decl2 with
        | Some d1, Some d2 ->
            Array.append
                (d1.Params |> Array.map (fun x -> x.Name))
                (d2.Params |> Array.map (fun x -> x.Name))
            |> Array.distinct
            |> Array.map TypeParameter
            |> TypeParameterDeclaration |> Some
        | Some _, None -> decl1
        | None, Some _ -> decl2
        | None, None -> None

    let uncurryLambdaType t =
        let rec uncurryLambdaArgs acc = function
            | Fable.FunctionType(Fable.LambdaType paramType, returnType) ->
                uncurryLambdaArgs (paramType::acc) returnType
            | t -> List.rev acc, t
        uncurryLambdaArgs [] t

    let rec typeAnnotation com ctx typ: TypeAnnotationInfo =
        match typ with
        | Fable.MetaType -> upcast AnyTypeAnnotation()
        | Fable.Any -> upcast AnyTypeAnnotation()
        | Fable.Unit -> upcast VoidTypeAnnotation()
        | Fable.Boolean -> upcast BooleanTypeAnnotation()
        | Fable.Char -> upcast StringTypeAnnotation()
        | Fable.String -> upcast StringTypeAnnotation()
        | Fable.Regex -> upcast AnyTypeAnnotation()
        | Fable.Number kind -> makeNumericTypeAnnotation com ctx kind
        | Fable.Enum _ent -> upcast NumberTypeAnnotation()
        | Fable.Option genArg -> makeOptionTypeAnnotation com ctx genArg
        | Fable.Tuple genArgs -> makeTupleTypeAnnotation com ctx genArgs
        | Fable.Array genArg -> makeArrayTypeAnnotation com ctx genArg
        | Fable.List genArg -> makeListTypeAnnotation com ctx genArg
        | Replacements.Builtin kind -> makeBuiltinTypeAnnotation com ctx kind
        | Fable.FunctionType(kind, returnType) ->
            makeFunctionTypeAnnotation com ctx typ kind returnType
        | Fable.GenericParam name -> makeSimpleTypeAnnotation com ctx name
        | Fable.ErasedUnion genArgs -> makeUnionTypeAnnotation com ctx genArgs
        | Fable.DeclaredType(ent, genArgs) ->
            makeEntityTypeAnnotation com ctx ent genArgs
        | Fable.AnonymousRecordType(fieldNames, genArgs) ->
            makeAnonymousRecordTypeAnnotation com ctx fieldNames genArgs

    and makeSimpleTypeAnnotation _com _ctx name =
        GenericTypeAnnotation(Identifier(name))
        :> TypeAnnotationInfo

    and makeGenTypeParamInst com ctx genArgs =
        match genArgs |> List.map (typeAnnotation com ctx) with
        | [] -> None
        | xs -> xs |> List.toArray |> TypeParameterInstantiation |> Some

    and makeGenericTypeAnnotation com ctx genArgs id =
        let typeParamInst = makeGenTypeParamInst com ctx genArgs
        GenericTypeAnnotation(id, ?typeParameters=typeParamInst)
        :> TypeAnnotationInfo

    and makeNativeTypeAnnotation com ctx genArgs typeName =
        Identifier(typeName)
        |> makeGenericTypeAnnotation com ctx genArgs

    and makeImportTypeId (com: IBabelCompiler) ctx moduleName typeName =
        let expr = com.GetImportExpr(ctx, typeName, moduleName, Fable.Library)
        match expr with
        | :? Identifier as id -> id
        | _ -> Identifier(typeName)

    and makeImportTypeAnnotation com ctx genArgs moduleName typeName =
        let id = makeImportTypeId com ctx moduleName typeName
        makeGenericTypeAnnotation com ctx genArgs id

    and makeNumericTypeAnnotation com ctx kind =
        let typeName = getNumberKindName kind
        makeImportTypeAnnotation com ctx [] "Int32" typeName

    and makeOptionTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Option" "Option"

    and makeTupleTypeAnnotation com ctx genArgs =
        List.map (typeAnnotation com ctx) genArgs
        |> List.toArray |> TupleTypeAnnotation
        :> TypeAnnotationInfo

    and makeArrayTypeAnnotation com ctx genArg =
        match genArg with
        | Fable.Number kind when com.Options.typedArrays ->
            let name = getTypedArrayName com kind
            makeSimpleTypeAnnotation com ctx name
        | _ ->
            makeNativeTypeAnnotation com ctx [genArg] "Array"

    and makeListTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Types" "List"

    and makeUnionTypeAnnotation com ctx genArgs =
        List.map (typeAnnotation com ctx) genArgs
        |> List.toArray |> UnionTypeAnnotation
        :> TypeAnnotationInfo

    and makeBuiltinTypeAnnotation com ctx kind =
        match kind with
        | Replacements.BclGuid -> upcast StringTypeAnnotation()
        | Replacements.BclTimeSpan -> upcast NumberTypeAnnotation()
        | Replacements.BclDateTime -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.BclDateTimeOffset -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.BclTimer -> makeImportTypeAnnotation com ctx [] "Timer" "Timer"
        | Replacements.BclInt64 -> makeImportTypeAnnotation com ctx [] "Long" "int64"
        | Replacements.BclUInt64 -> makeImportTypeAnnotation com ctx [] "Long" "uint64"
        | Replacements.BclDecimal -> makeImportTypeAnnotation com ctx [] "Decimal" "decimal"
        | Replacements.BclBigInt -> makeImportTypeAnnotation com ctx [] "BigInt/z" "BigInteger"
        | Replacements.BclHashSet key -> makeNativeTypeAnnotation com ctx [key] "Set"
        | Replacements.BclDictionary (key, value) -> makeNativeTypeAnnotation com ctx [key; value] "Map"
        | Replacements.BclKeyValuePair (key, value) -> makeTupleTypeAnnotation com ctx [key; value]
        | Replacements.FSharpSet key -> makeImportTypeAnnotation com ctx [key] "Set" "FSharpSet"
        | Replacements.FSharpMap (key, value) -> makeImportTypeAnnotation com ctx [key; value] "Map" "FSharpMap"
        | Replacements.FSharpResult (ok, err) -> makeImportTypeAnnotation com ctx [ok; err] "Option" "Result"
        | Replacements.FSharpChoice genArgs -> makeUnionTypeAnnotation com ctx genArgs
        | Replacements.FSharpReference genArg -> makeImportTypeAnnotation com ctx [genArg] "Types" "FSharpRef"

    and makeFunctionTypeAnnotation com ctx typ kind returnType =
        let argTypes, returnType =
            match kind with
            | Fable.LambdaType _argType -> uncurryLambdaType typ
            | Fable.DelegateType argTypes -> argTypes, returnType
        let funcTypeParams =
            argTypes
            |> List.mapi (fun i argType ->
                FunctionTypeParam(
                    Identifier("arg" + (string i)),
                    typeAnnotation com ctx argType))
            |> List.toArray
        let genTypeParams = getGenericTypeParams (argTypes @ [returnType])
        let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
        let returnType = typeAnnotation com ctx returnType
        let typeParamDecl = makeTypeParamDecl newTypeParams
        FunctionTypeAnnotation(funcTypeParams, returnType, ?typeParameters=typeParamDecl)
        :> TypeAnnotationInfo

    and makeEntityTypeAnnotation com ctx ent genArgs =
        match ent.TryFullName with
        | Some Types.ienumerableGeneric ->
            makeNativeTypeAnnotation com ctx genArgs "Iterable"
        | Some Types.result ->
            makeUnionTypeAnnotation com ctx genArgs
        | Some entName when entName.StartsWith(Types.choiceNonGeneric) ->
            makeUnionTypeAnnotation com ctx genArgs
        | _ when ent.IsInterface ->
            upcast AnyTypeAnnotation() // TODO:
        | _ ->
            match tryJsConstructor com ctx ent with
            | Some entRef ->
                match entRef with
                | :? StringLiteral as str ->
                    match str.Value with
                    | "number" -> upcast NumberTypeAnnotation()
                    | "boolean" -> upcast BooleanTypeAnnotation()
                    | "string" -> upcast StringTypeAnnotation()
                    | _ -> upcast AnyTypeAnnotation()
                | :? Identifier as id ->
                    makeGenericTypeAnnotation com ctx genArgs id
                // TODO: Resolve references to types in nested modules
                | _ -> upcast AnyTypeAnnotation()
            | None -> upcast AnyTypeAnnotation()

    and makeAnonymousRecordTypeAnnotation com ctx fieldNames genArgs =
         upcast AnyTypeAnnotation() // TODO:

    let typedIdent (com: IBabelCompiler) ctx (id: Fable.Ident) =
        let typeAnnotation =
            if com.Options.typeDecls then
                typeAnnotation com ctx id.Type
                |> TypeAnnotation |> Some
            else None
        Identifier(id.Name, ?typeAnnotation=typeAnnotation, ?loc=id.Range)

    let transformFunc (com: IBabelCompiler) ctx name (args: Fable.Ident list) (body: Fable.Expr) =
        if com.Options.typeDecls then
            let argTypes = args |> List.map (fun id -> id.Type)
            let genTypeParams = getGenericTypeParams (argTypes @ [body.Type])
            let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
            let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
            let args', body' = com.TransformFunction(ctx, name, args, body)
            let returnType = TypeAnnotation(typeAnnotation com ctx body.Type) |> Some
            let typeParamDecl = makeTypeParamDecl newTypeParams
            args', body', returnType, typeParamDecl
        else
            let args', body' = com.TransformFunction(ctx, name, args, body)
            args', body', None, None

    let getMemberArgsAndBody (com: IBabelCompiler) ctx name boundThis args hasSpread (body: Fable.Expr) =
        let args, body, genTypeParams =
            match boundThis with
            | Some(boundThis, thisArg: Fable.Ident) ->
                let genTypeParams = Set.difference (getGenericTypeParams [thisArg.Type]) ctx.ScopedTypeParams
                let isThisUsed =
                    body |> FableTransforms.deepExists (function
                        | Fable.IdentExpr id when id.Name = thisArg.Name -> true
                        | _ -> false)
                if not isThisUsed
                then args, body, genTypeParams
                else
                    // If the boundThis is the actual JS `this` keyword bind it at the beginning
                    // to prevent problems in closures. If not, replace thisArg in body with boundThis.
                    let boundThisExpr = { thisArg with Name = boundThis } |> Fable.IdentExpr
                    let body =
                        if boundThis = "this"
                        then Fable.Let([thisArg, boundThisExpr], body)
                        else FableTransforms.replaceValues (Map [thisArg.Name, boundThisExpr]) body
                    args, body, genTypeParams
            | None -> args, body, Set.empty
        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams genTypeParams }
        let args, body, returnType, typeParamDecl = transformFunc com ctx name args body
        let typeParamDecl =
            if com.Options.typeDecls then
                makeTypeParamDecl genTypeParams |> mergeTypeParamDecls typeParamDecl
            else typeParamDecl
        let args =
            if not hasSpread
            then args
            else
                let args = Array.rev args
                let restEl = RestElement(Array.head args) :> PatternNode |> U2.Case1
                Array.append [|restEl|] (Array.tail args) |> Array.rev
        let body =
            match body with
            | U2.Case1 e -> e
            | U2.Case2 e -> BlockStatement [|ReturnStatement(e, ?loc=e.Loc)|]
        args, body, returnType, typeParamDecl

    let getUnionCaseName uci =
        FSharp2Fable.Helpers.unionCaseCompiledName uci
        |> Option.defaultValue uci.Name

    let getUnionExprTag r expr =
        getExpr r expr (ofString "tag")

    let getUnionExprField r expr fieldIndex =
        getExpr r (getExpr None expr (ofString "fields")) (ofInt fieldIndex)

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | :? NumericLiteral, _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.Enum _ ->
            BinaryExpression(BinaryOrBitwise, e, NumericLiteral(0.)) :> Expression
        | _ -> e

    let makeFunctionExpression name (args, (body: U2<BlockStatement, Expression>), returnType, typeParamDecl): Expression =
        let id = name |> Option.map Identifier
        let body =
            match body with
            | U2.Case1 body -> body
            | U2.Case2 e -> BlockStatement [|ReturnStatement(e, ?loc=e.Loc)|]
        upcast FunctionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let optimizeTailCall (com: IBabelCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
        let rec checkCrossRefs tempVars allArgs = function
            | [] -> tempVars
            | (argId, _arg)::rest ->
                let found = allArgs |> List.exists (FableTransforms.deepExists (function
                    | Fable.IdentExpr i -> argId = i.Name
                    | _ -> false))
                let tempVars =
                    if found
                    then Map.add argId (com.GetUniqueVar(argId)) tempVars
                    else tempVars
                checkCrossRefs tempVars allArgs rest
        ctx.OptimizeTailCall()
        let zippedArgs = List.zip tc.Args args
        let tempVars = checkCrossRefs Map.empty args zippedArgs
        let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExprNonMangled v)
        [|
            // First declare temp variables
            for (KeyValue(argId, tempVar)) in tempVars do
                yield varDeclaration (Identifier tempVar) false (Identifier argId) :> Statement
            // Then assign argument expressions to the original argument identifiers
            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
            for (argId, arg) in zippedArgs do
                let arg = FableTransforms.replaceValues tempVarReplacements arg
                let arg = com.TransformAsExpr(ctx, arg)
                yield assign None (Identifier argId) arg |> ExpressionStatement :> Statement
            yield upcast ContinueStatement(Identifier tc.Label, ?loc=range)
        |]

    let transformImport (com: IBabelCompiler) ctx r (selector: Fable.Expr) (path: Fable.Expr) kind =
        match selector, path with
        | Fable.Value(Fable.StringConstant selector,_), Fable.Value(Fable.StringConstant path,_) ->
            let selector, parts =
                let parts = Array.toList(selector.Split('.'))
                parts.Head, parts.Tail
            com.GetImportExpr(ctx, selector, path, kind)
            |> getParts parts
        | _ -> "Import expressions only accept string literals" |> addErrorAndReturnNull com r

    let transformCast (com: IBabelCompiler) (ctx: Context) t e: Expression =
        match t with
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | Fable.DeclaredType(ent,[_]) ->
            match ent.TryFullName, e with
            | Some Types.ienumerableGeneric, Replacements.ArrayOrListLiteral(exprs, _) ->
                makeArray com ctx exprs
            | _ -> com.TransformAsExpr(ctx, e)
        | _ -> com.TransformAsExpr(ctx, e)

    let transformDelayedResolution (com: IBabelCompiler) (ctx: Context) r kind: Expression =
        match kind with
        | Fable.AsPojo(expr, caseRule) -> com.TransformAsExpr(ctx, Replacements.makePojo com r caseRule expr)
        | Fable.Curry(expr, arity) -> com.TransformAsExpr(ctx, Replacements.curryExprAtRuntime arity expr)

    let rec transformRecordReflectionInfo com ctx r (ent: FSharpEntity) generics =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = defaultArg ent.TryFullName Naming.unknown
        let fullnameExpr = StringLiteral fullname :> Expression
        let genMap =
            let genParamNames = ent.GenericParameters |> Seq.map (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map
        let fields =
            ent.FSharpFields |> Seq.map (fun fi ->
                let typeInfo =
                    FSharp2Fable.TypeHelpers.makeType com Map.empty fi.FieldType
                    |> transformTypeInfo com ctx r genMap
                (ArrayExpression [|StringLiteral fi.Name; typeInfo|] :> Expression))
            |> Seq.toArray
        let fields = ArrowFunctionExpression([||], ArrayExpression fields :> Expression |> U2.Case2) :> Expression
        [|fullnameExpr; upcast ArrayExpression generics; jsConstructor com ctx ent; fields|]
        |> coreReflectionCall com ctx None "record"

    and transformUnionReflectionInfo com ctx r (ent: FSharpEntity) generics =
        let fullname = defaultArg ent.TryFullName Naming.unknown
        let fullnameExpr = StringLiteral fullname :> Expression
        let genMap =
            let genParamNames = ent.GenericParameters |> Seq.map (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map
        let cases =
            ent.UnionCases |> Seq.map (fun uci ->
                let fieldInfos =
                    uci.UnionCaseFields
                    |> Seq.map (fun fi ->
                        let fieldType =
                            FSharp2Fable.TypeHelpers.makeType com Map.empty fi.FieldType
                            |> transformTypeInfo com ctx r genMap
                        ArrayExpression [|
                            fi.Name |> StringLiteral :> Expression
                            fieldType
                        |] :> Expression
                    )
                    |> Seq.toArray
                let caseInfo =
                    if fieldInfos.Length = 0 then
                        getUnionCaseName uci |> StringLiteral :> Expression
                    else
                        ArrayExpression [|
                            getUnionCaseName uci |> StringLiteral :> Expression
                            ArrayExpression fieldInfos :> Expression
                        |] :> Expression
                caseInfo) |> Seq.toArray
        let cases = ArrowFunctionExpression([||], ArrayExpression cases :> Expression |> U2.Case2) :> Expression
        [|fullnameExpr; upcast ArrayExpression generics; jsConstructor com ctx ent; cases|]
        |> coreReflectionCall com ctx None "union"

    and transformTypeInfo (com: IBabelCompiler) ctx r (genMap: Map<string, Expression>) t: Expression =
        let primitiveTypeInfo name =
           coreValue com ctx "Reflection" (name + "_type")
        let numberInfo kind =
            getNumberKindName kind
            |> primitiveTypeInfo
        let nonGenericTypeInfo fullname =
            [| StringLiteral fullname :> Expression |]
            |> coreReflectionCall com ctx None "generic"
        let resolveGenerics generics: Expression[] =
            generics |> Array.map (transformTypeInfo com ctx r genMap)
        let genericTypeInfo name genArgs =
            let resolved = resolveGenerics genArgs
            coreReflectionCall com ctx None name resolved
        let genericEntity (ent: FSharpEntity) generics =
            let fullname = defaultArg ent.TryFullName Naming.unknown
            let fullnameExpr = StringLiteral fullname :> Expression
            let args = if Array.isEmpty generics then [|fullnameExpr|] else [|fullnameExpr; ArrayExpression generics :> Expression|]
            coreReflectionCall com ctx None "generic" args
        match t with
        | Fable.ErasedUnion _genArgs -> primitiveTypeInfo "obj" // TODO: Type info for ErasedUnion?
        | Fable.Any -> primitiveTypeInfo "obj"
        | Fable.GenericParam name ->
            match Map.tryFind name genMap with
            | Some t -> t
            | None ->
                Replacements.genericTypeInfoError name |> addError com [] r
                NullLiteral () :> Expression
        | Fable.Unit    -> primitiveTypeInfo "unit"
        | Fable.Boolean -> primitiveTypeInfo "bool"
        | Fable.Char    -> primitiveTypeInfo "char"
        | Fable.String  -> primitiveTypeInfo "string"
        | Fable.Enum ent ->
            let fullName = defaultArg ent.TryFullName Naming.unknown
            let mutable numberKind = Int32
            let cases =
                ent.FSharpFields |> Seq.choose (fun fi ->
                    // F# seems to include a field with this name with the underlying type
                    match fi.Name with
                    | "value__" ->
                        match FSharp2Fable.TypeHelpers.makeType com Map.empty fi.FieldType with
                        | Fable.Number kind -> numberKind <- kind
                        | _ -> ()
                        None
                    | name ->
                        let value = match fi.LiteralValue with Some v -> System.Convert.ToDouble v | None -> 0.
                        ArrayExpression [|StringLiteral name; NumericLiteral value|] :> Expression |> Some)
                |> Seq.toArray
                |> ArrayExpression
            [|StringLiteral fullName :> Expression; numberInfo numberKind; cases :> _|]
            |> coreReflectionCall com ctx None "enum"
        | Fable.Number kind ->
            numberInfo kind
        | Fable.FunctionType(Fable.LambdaType argType, returnType) ->
            genericTypeInfo "lambda" [|argType; returnType|]
        | Fable.FunctionType(Fable.DelegateType argTypes, returnType) ->
            genericTypeInfo "delegate" ([|yield! argTypes; yield returnType|])
        | Fable.Tuple genArgs   -> genericTypeInfo "tuple" (List.toArray genArgs)
        | Fable.Option genArg   -> genericTypeInfo "option" [|genArg|]
        | Fable.Array genArg    -> genericTypeInfo "array" [|genArg|]
        | Fable.List genArg     -> genericTypeInfo "list" [|genArg|]
        | Fable.Regex           -> nonGenericTypeInfo Types.regex
        | Fable.MetaType        -> nonGenericTypeInfo Types.type_
        | Fable.AnonymousRecordType(fieldNames, genArgs) ->
            let genArgs = resolveGenerics (List.toArray genArgs)
            Array.zip fieldNames genArgs
            |> Array.map (fun (k, t) -> ArrayExpression [|StringLiteral k; t|] :> Expression)
            |> coreReflectionCall com ctx None "anonRecord"
        | Fable.DeclaredType(ent, generics) ->
            match ent, generics with
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
                | Replacements.BclBigInt -> genericEntity ent [||]
                | Replacements.BclHashSet gen
                | Replacements.FSharpSet gen ->
                    genericEntity ent [|transformTypeInfo com ctx r genMap gen|]
                | Replacements.BclDictionary(key, value)
                | Replacements.BclKeyValuePair(key, value)
                | Replacements.FSharpMap(key, value) ->
                    genericEntity ent [|
                        transformTypeInfo com ctx r genMap key
                        transformTypeInfo com ctx r genMap value
                    |]
                | Replacements.FSharpResult(ok, err) ->
                    transformUnionReflectionInfo com ctx r ent [|
                        transformTypeInfo com ctx r genMap ok
                        transformTypeInfo com ctx r genMap err
                    |]
                | Replacements.FSharpChoice gen ->
                    let gen = List.map (transformTypeInfo com ctx r genMap) gen
                    List.toArray gen |> transformUnionReflectionInfo com ctx r ent
                | Replacements.FSharpReference gen ->
                    transformRecordReflectionInfo com ctx r ent [|transformTypeInfo com ctx r genMap gen|]
            | _ ->
                let generics = generics |> List.map (transformTypeInfo com ctx r genMap) |> List.toArray
                /// Check if the entity is actually declared in JS code
                if ent.IsInterface
                    || FSharp2Fable.Util.isErasedEntity ent
                    // TODO!!! Get reflection info from types in precompiled libs
                    || FSharp2Fable.Util.isReplacementCandidate ent then
                    genericEntity ent generics
                else
                    let reflectionMethodExpr = FSharp2Fable.Util.entityRefWithSuffix com ent Naming.reflectionSuffix
                    CallExpression(com.TransformAsExpr(ctx, reflectionMethodExpr), generics) :> Expression

    let transformReflectionInfo com ctx r (ent: FSharpEntity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = defaultArg ent.TryFullName Naming.unknown
            let fullnameExpr = StringLiteral fullname :> Expression
            let args = if Array.isEmpty generics then [|fullnameExpr|] else [|fullnameExpr; ArrayExpression generics :> Expression|]
            coreReflectionCall com ctx None "generic" args

    let transformValue (com: IBabelCompiler) (ctx: Context) r value: Expression =
        match value with
        | Fable.TypeInfo t -> transformTypeInfo com ctx r Map.empty t
        | Fable.Null _ -> upcast NullLiteral(?loc=r)
        | Fable.UnitConstant -> upcast UnaryExpression(UnaryVoid, NullLiteral(), ?loc=r)
        | Fable.BoolConstant x -> upcast BooleanLiteral(x, ?loc=r)
        | Fable.CharConstant x -> upcast StringLiteral(string x, ?loc=r)
        | Fable.StringConstant x -> upcast StringLiteral(x, ?loc=r)
        | Fable.NumberConstant (x,_) ->
            if x < 0.
            // Negative numeric literals can give issues in Babel AST, see #1186
            then upcast UnaryExpression(UnaryMinus, NumericLiteral(x * -1.), ?loc=r)
            else upcast NumericLiteral(x, ?loc=r)
        | Fable.RegexConstant (source, flags) -> upcast RegExpLiteral(source, flags, ?loc=r)
        | Fable.NewArray (arrayKind, typ) -> makeTypedArray com ctx typ arrayKind
        | Fable.NewTuple vals -> makeTypedArray com ctx Fable.Any (Fable.ArrayValues vals)
        // Optimization for bundle size: compile list literals as List.ofArray
        | Replacements.ListLiteral(exprs, t) ->
            match exprs with
            | [] -> makeList com ctx None
            | [expr] -> Some(expr, Fable.Value(Fable.NewList (None,t), None)) |> makeList com ctx
            | exprs -> [|makeArray com ctx exprs|] |> coreLibCall com ctx r "List" "ofArray"
        | Fable.NewList (headAndTail, _) ->
            makeList com ctx headAndTail
        | Fable.NewOption (value, t) ->
            match value with
            | Some (TransformExpr com ctx e) ->
                if mustWrapOption t
                then coreLibCall com ctx r "Option" "some" [|e|]
                else e
            | None -> upcast NullLiteral ()
        | Fable.EnumConstant(x,_) ->
            com.TransformAsExpr(ctx, x)
        | Fable.NewRecord(values, kind, _) ->
            let values = List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values
            match kind with
            | Fable.DeclaredRecord ent ->
                let consRef = jsConstructor com ctx ent
                upcast NewExpression(consRef, values, ?loc=r)
            | Fable.AnonymousRecord fieldNames ->
                Array.zip fieldNames values
                |> makeJsObject
                |> Array.singleton
                |> coreLibCall com ctx r "Types" "anonRecord"
        | Fable.NewUnion(values, uci, ent, _) ->
            // Union cases with EraseAttribute are used for `Custom`-like cases in unions meant for `keyValueList`
            if FSharp2Fable.Helpers.hasAtt Atts.erase uci.Attributes then
                Fable.ArrayValues values |> makeTypedArray com ctx Fable.Any
            else
                let name = getUnionCaseName uci
                let consRef = jsConstructor com ctx ent
                let tag = FSharp2Fable.Helpers.unionCaseTag ent uci
                let values = List.map (fun x -> com.TransformAsExpr(ctx, x)) values
                upcast NewExpression(consRef, (ofInt tag)::(ofString name)::values |> List.toArray, ?loc=r)
        | Fable.NewErasedUnion(e,_) -> com.TransformAsExpr(ctx, e)

    let transformObjectExpr (com: IBabelCompiler) ctx members (boundThis: string) baseCall: Expression =
        let makeObjMethod kind prop computed hasSpread args body =
            let boundThis, args = prepareBoundThis boundThis args
            let args, body, returnType, typeParamDecl =
                getMemberArgsAndBody com ctx None boundThis args hasSpread body
            ObjectMethod(kind, prop, args, body, computed_=computed,
                ?returnType=returnType, ?typeParameters=typeParamDecl) |> U3.Case2 |> Some
        let pojo =
            members |> List.choose (fun (Fable.ObjectMember(key, expr, kind)) ->
                match kind, expr with
                | Fable.ObjectValue, Fable.Function(Fable.Delegate args, body, _) ->
                    // Don't call the `makeObjMethod` helper here because function as values don't bind `this` arg
                    let args, body', returnType, typeParamDecl = getMemberArgsAndBody com ctx None None args false body
                    let prop, computed = memberFromExpr com ctx key
                    ObjectMethod(ObjectMeth, prop, args, body', computed,
                        ?returnType=returnType, ?typeParameters=typeParamDecl) |> U3.Case2 |> Some
                | Fable.ObjectValue, TransformExpr com ctx value ->
                    let prop, computed = memberFromExpr com ctx key
                    ObjectProperty(prop, value, computed_=computed) |> U3.Case1 |> Some
                | Fable.ObjectMethod hasSpread, Fable.Function(Fable.Delegate args, body, _) ->
                    let prop, computed =
                        match key with
                        // Compile ToString in lower case for compatibity with JS (and debugger tools)
                        | Fable.Value(Fable.StringConstant "ToString", _) -> memberFromName "toString"
                        | key -> memberFromExpr com ctx key
                    makeObjMethod ObjectMeth prop computed hasSpread args body
                | Fable.ObjectIterator, Fable.Function(Fable.Delegate args, body, _) ->
                    let prop = get None (Identifier "Symbol") "iterator"
                    Replacements.enumerator2iterator body
                    |> makeObjMethod ObjectMeth prop true false args
                | Fable.ObjectGetter, Fable.Function(Fable.Delegate args, body, _) ->
                    let prop, computed = memberFromExpr com ctx key
                    makeObjMethod ObjectGetter prop computed false args body
                | Fable.ObjectSetter, Fable.Function(Fable.Delegate args, body, _) ->
                    let prop, computed = memberFromExpr com ctx key
                    makeObjMethod ObjectSetter prop computed false args body
                | kind, _ ->
                    sprintf "Object member has kind %A but value is not a function" kind
                    |> addError com [] None; None
            ) |> List.toArray |> ObjectExpression
        match baseCall with
        | Some(TransformExpr com ctx baseCall) ->
            coreUtil com ctx "extend" [|baseCall; pojo|]
        | None -> pojo :> Expression

    let transformArgs (com: IBabelCompiler) ctx args spread =
        match args, spread with
        | [], _
        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))], _ -> []
        | [MaybeCasted(Fable.Value(Fable.NewTuple args,_))], Fable.TupleSpread ->
            List.map (fun e -> com.TransformAsExpr(ctx, e)) args
        | args, Fable.SeqSpread ->
            match List.rev args with
            | [] -> []
            // TODO: Check also lists?
            | Fable.Value(Fable.NewArray(Fable.ArrayValues spreadArgs,_),_)::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
            | last::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ [SpreadElement(com.TransformAsExpr(ctx, last))]
        | args, _ -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr t strategy babelExpr: Statement =
        match strategy with
        | None | Some ReturnUnit -> upcast ExpressionStatement babelExpr
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return -> upcast ReturnStatement(wrapIntExpression t babelExpr, ?loc=babelExpr.Loc)
        | Some(Assign left) -> upcast ExpressionStatement(assign None left babelExpr, ?loc=babelExpr.Loc)
        | Some(Target left) -> upcast ExpressionStatement(assign None left babelExpr, ?loc=babelExpr.Loc)

    let transformOperation com ctx range opKind: Expression =
        match opKind with
        | Fable.UnaryOperation(op, TransformExpr com ctx expr) ->
            upcast UnaryExpression (op, expr, ?loc=range)
        | Fable.BinaryOperation(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast BinaryExpression (op, left, right, ?loc=range)
        | Fable.LogicalOperation(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast LogicalExpression (op, left, right, ?loc=range)
        | Fable.Emit(emit, argInfo) ->
            match argInfo with
            | Some argInfo ->
                let args = transformArgs com ctx argInfo.Args argInfo.Spread
                match argInfo.ThisArg with
                | Some(TransformExpr com ctx thisArg) -> macroExpression range emit (thisArg::args)
                | None -> macroExpression range emit args
            | None -> macroExpression range emit []
        | Fable.Call(kind, argInfo) ->
            let args = transformArgs com ctx argInfo.Args argInfo.Spread
            match kind with
            | Fable.ConstructorCall(TransformExpr com ctx consExpr) ->
                upcast NewExpression(consExpr, List.toArray args, ?loc=range)
            | Fable.StaticCall(TransformExpr com ctx funcExpr) ->
                if argInfo.IsBaseOrSelfConstructorCall then
                    let thisArg =
                        match argInfo.ThisArg with
                        | Some(TransformExpr com ctx thisArg) -> thisArg
                        | None -> thisExpr
                    callFunctionWithThisContext range funcExpr thisArg args
                else
                    let args =
                        match argInfo.ThisArg with
                        | Some(TransformExpr com ctx thisArg) -> thisArg::args
                        | None -> args
                    upcast CallExpression(funcExpr, List.toArray args, ?loc=range)
            | Fable.InstanceCall membExpr ->
                match argInfo.ThisArg, membExpr with
                | None, _ -> addErrorAndReturnNull com range "InstanceCall with empty this argument"
                // When calling a virtual method with default implementation from base class,
                // compile it as: `BaseClass.prototype.Foo.call(this, ...args)` (see #701)
                | Some(Fable.IdentExpr(IdentType(Fable.DeclaredType(baseEntity, _)) as thisIdent)), Some membExpr
                        when thisIdent.IsBaseValue ->
                    let baseClassExpr = jsConstructor com ctx baseEntity
                    let baseProtoMember =
                        com.TransformAsExpr(ctx, membExpr)
                        |> getExpr None (get None baseClassExpr "prototype")
                    callFunctionWithThisContext range baseProtoMember (ident thisIdent) args
                | Some thisArg, None ->
                    upcast CallExpression(com.TransformAsExpr(ctx, thisArg), List.toArray args, ?loc=range)
                | Some thisArg, Some(TransformExpr com ctx m) ->
                    let thisArg = com.TransformAsExpr(ctx, thisArg)
                    upcast CallExpression(getExpr None thisArg m, List.toArray args, ?loc=range)
        | Fable.CurriedApply(TransformExpr com ctx applied, args) ->
            match transformArgs com ctx args Fable.NoSpread with
            | [] -> upcast CallExpression(applied, [||], ?loc=range)
            | head::rest ->
                let baseExpr = CallExpression(applied, [|head|], ?loc=range) :> Expression
                (baseExpr, rest) ||> List.fold (fun e arg ->
                    CallExpression(e, [|arg|], ?loc=range) :> Expression)

    let transformOperationAsStatements com ctx range t returnStrategy opKind =
        let argsLen (i: Fable.ArgInfo) =
            List.length i.Args + (if Option.isSome i.ThisArg then 1 else 0)
        // TODO: Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity, opKind with
        | Some(Return|ReturnUnit), Some tc, Fable.Call(Fable.StaticCall funcExpr, argInfo)
                                when not argInfo.IsBaseOrSelfConstructorCall
                                && tc.IsRecursiveRef(funcExpr)
                                && argsLen argInfo = List.length tc.Args ->
            let args =
                match argInfo.ThisArg with
                | Some thisArg -> thisArg::argInfo.Args
                | None -> argInfo.Args
            optimizeTailCall com ctx range tc args
        | Some(Return|ReturnUnit), Some tc, Fable.CurriedApply(funcExpr, args)
                                when tc.IsRecursiveRef(funcExpr)
                                && List.sameLength args tc.Args ->
            optimizeTailCall com ctx range tc args
        | _ ->
            [|transformOperation com ctx range opKind |> resolveExpr t returnStrategy|]

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr: BlockStatement =
        com.TransformAsStatements(ctx, ret, expr) |> BlockStatement

    let transformTryCatch com ctx r returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause (identAsPattern param,
                    transformBlock com ctx returnStrategy body))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [|TryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer, ?loc=r) :> Statement|]

    // Even if IfStatement doesn't enforce it, compile both branches as blocks
    // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
    let rec transformIfStatement (com: IBabelCompiler) ctx r ret (guardExpr: Expression) thenStmnt elseStmnt =
        let thenStmnt = transformBlock com ctx ret thenStmnt
        match elseStmnt: Fable.Expr with
        | Fable.IfThenElse(TransformExpr com ctx guardExpr', thenStmnt', elseStmnt', r2) ->
            let elseStmnt = transformIfStatement com ctx r2 ret guardExpr' thenStmnt' elseStmnt'
            IfStatement(guardExpr, thenStmnt, elseStmnt, ?loc=r)
        | expr ->
            match com.TransformAsStatements(ctx, ret, expr) with
            | [||] -> IfStatement(guardExpr, thenStmnt, ?loc=r)
            | [|:? ExpressionStatement as e|] when (e.Expression :? NullLiteral) ->
                IfStatement(guardExpr, thenStmnt, ?loc=r)
            | statements -> IfStatement(guardExpr, thenStmnt, BlockStatement statements, ?loc=r)

    let transformGet (com: IBabelCompiler) ctx range typ fableExpr (getKind: Fable.GetKind) =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        match getKind with
        | Fable.ExprGet(TransformExpr com ctx prop) -> getExpr range expr prop
        | Fable.ListHead -> get range expr "head"
        | Fable.ListTail -> get range expr "tail"
        | Fable.FieldGet(fieldName,_,_) ->
            let expr =
                match fableExpr with
                // When calling a virtual property with default implementation from base class,
                // compile it as: `BaseClass.prototype.Foo` (see #701)
                | Fable.IdentExpr(IdentType(Fable.DeclaredType(baseEntity, _)) as thisIdent)
                        when thisIdent.IsBaseValue ->
                    let baseClassExpr = jsConstructor com ctx baseEntity
                    get None baseClassExpr "prototype"
                | _ -> expr
            get range expr fieldName
        | Fable.TupleGet index -> getExpr range expr (ofInt index)
        | Fable.OptionValue ->
            if mustWrapOption typ || com.Options.typeDecls
            then coreLibCall com ctx None "Option" "value" [|expr|]
            else expr
        | Fable.UnionTag -> getUnionExprTag range expr
        | Fable.UnionField(field, uci, _) ->
            let fieldName = field.Name
            uci.UnionCaseFields
            |> Seq.findIndex (fun fi -> fi.Name = fieldName)
            |> getUnionExprField range expr

    let transformSet (com: IBabelCompiler) ctx range var (value: Fable.Expr) (setKind: Fable.SetKind) =
        let var = com.TransformAsExpr(ctx, var)
        let value = com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type
        let var =
            match setKind with
            | Fable.VarSet -> var
            | Fable.FieldSet(name,_) -> get None var name
            | Fable.ExprSet(TransformExpr com ctx e) -> getExpr None var e
        assign range var value

    let getSetReturnStrategy com ctx (TransformExpr com ctx expr) = function
        | Fable.VarSet -> Assign expr
        | Fable.ExprSet(TransformExpr com ctx prop) -> getExpr None expr prop |> Assign
        | Fable.FieldSet(name,_) -> get None expr name |> Assign

    let transformBindingExprBody (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        // Check imports with name placeholder
        | Fable.Import((Fable.Value(Fable.StringConstant Naming.placeholder,_)), path, kind, _, r) ->
            transformImport com ctx r (makeStrConst var.Name) path kind
        | Fable.Function(_,Fable.Import((Fable.Value(Fable.StringConstant Naming.placeholder,_)), path, kind, _, r),_) ->
            transformImport com ctx r (makeStrConst var.Name) path kind
        | Fable.Function(args, body, _) ->
            let args =
                match args with
                | Fable.Lambda arg -> [arg]
                | Fable.Delegate args -> args
            let name = Some var.Name
            transformFunc com ctx name args body
            |> makeFunctionExpression name
        | _ ->
            com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type

    let transformBindingAsExpr (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        transformBindingExprBody com ctx var value
        |> assign None (ident var)

    let transformBindingAsStatements (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isJsStatement ctx false value then
            let var = typedIdent com ctx var
            let decl = VariableDeclaration(toPattern var) :> Statement
            let body = com.TransformAsStatements(ctx, Some(Assign var), value)
            Array.append [|decl|] body
        else
            let value = transformBindingExprBody com ctx var value
            [|varDeclaration (typedIdent com ctx var) var.IsMutable value :> Statement|]

    let transformTypeTest (com: IBabelCompiler) ctx range expr (typ: Fable.Type): Expression =
        let fail msg =
            "Cannot type test: " + msg |> addErrorAndReturnNull com range
        let warnAndEvalToFalse msg =
            "Cannot type test (evals to false): " + msg
            |> addWarning com [] range
            BooleanLiteral false :> Expression
        let jsTypeof (primitiveType: string) (TransformExpr com ctx expr): Expression =
            let typof = UnaryExpression(UnaryTypeof, expr)
            upcast BinaryExpression(BinaryEqualStrict, typof, StringLiteral primitiveType, ?loc=range)
        let jsInstanceof consExpr (TransformExpr com ctx expr): Expression =
            upcast BinaryExpression(BinaryInstanceOf, expr, consExpr, ?loc=range)
        match typ with
        | Fable.Any -> upcast BooleanLiteral true
        | Fable.Unit -> upcast BinaryExpression(BinaryEqual, com.TransformAsExpr(ctx, expr), NullLiteral(), ?loc=range)
        | Fable.Boolean -> jsTypeof "boolean" expr
        | Fable.Char | Fable.String _ -> jsTypeof "string" expr
        | Fable.Number _ | Fable.Enum _ -> jsTypeof "number" expr
        | Fable.Regex -> jsInstanceof (Identifier "RegExp") expr
        // TODO: Fail for functions, arrays, tuples and list because we cannot check generics?
        | Fable.FunctionType _ -> jsTypeof "function" expr
        | Fable.Array _ | Fable.Tuple _ ->
            coreLibCall com ctx None "Util" "isArrayLike" [|com.TransformAsExpr(ctx, expr)|]
        | Fable.List _ -> jsInstanceof (coreValue com ctx "Types" "List") expr
        | Replacements.Builtin kind ->
            match kind with
            | Replacements.BclGuid -> jsTypeof "string" expr
            | Replacements.BclTimeSpan -> jsTypeof "number" expr
            | Replacements.BclDateTime -> jsInstanceof (Identifier "Date") expr
            | Replacements.BclDateTimeOffset -> jsInstanceof (Identifier "Date") expr
            | Replacements.BclTimer -> jsInstanceof (coreValue com ctx "Timer" "default") expr
            | Replacements.BclInt64 -> jsInstanceof (coreValue com ctx "Long" "default") expr
            | Replacements.BclUInt64 -> jsInstanceof (coreValue com ctx "Long" "default") expr
            | Replacements.BclDecimal -> jsInstanceof (coreValue com ctx "Decimal" "default") expr
            | Replacements.BclBigInt -> coreLibCall com ctx None "BigInt" "isBigInt" [|com.TransformAsExpr(ctx, expr)|]
            | Replacements.BclHashSet _ -> fail "MutableSet" // TODO:
            | Replacements.BclDictionary _ -> fail "MutableMap" // TODO:
            | Replacements.BclKeyValuePair _ -> fail "KeyValuePair" // TODO:
            | Replacements.FSharpSet _ -> fail "Set" // TODO:
            | Replacements.FSharpMap _ -> fail "Map" // TODO:
            | Replacements.FSharpResult _ -> fail "Result" // TODO:
            | Replacements.FSharpChoice _ -> fail "Choice" // TODO:
            | Replacements.FSharpReference _ -> fail "FSharpRef" // TODO:
        | Fable.AnonymousRecordType _ ->
            "Type testing is not yet supported for anonymous records" // TODO:
            |> addWarning com [] range
            upcast BooleanLiteral false
        | Fable.DeclaredType (ent, genArgs) ->
            match ent.TryFullName with
            | Some Types.idisposable ->
                match expr.Type with
                // In F# AST this is coerced to obj, but the cast should have been removed
                | Fable.DeclaredType (ent2, _) when FSharp2Fable.Util.hasInterface Types.idisposable ent2 ->
                    upcast BooleanLiteral true
                | _ -> coreLibCall com ctx None "Util" "isDisposable" [|com.TransformAsExpr(ctx, expr)|]
            | Some Types.ienumerable ->
                [|com.TransformAsExpr(ctx, expr)|] |> coreLibCall com ctx None "Util" "isIterable"
            | Some Types.array ->
                [|com.TransformAsExpr(ctx, expr)|] |> coreLibCall com ctx None "Util" "isArrayLike"
            | _ when ent.IsInterface ->
                fail (sprintf "interface %A" ent.FullName)
            | _ when FSharp2Fable.Util.isReplacementCandidate ent ->
                match ent.TryFullName with
                | Some Types.exception_ ->
                    coreLibCall com ctx None "Types" "isException" [|com.TransformAsExpr(ctx, expr)|]
                | fullName -> warnAndEvalToFalse (defaultArg fullName Naming.unknown)
            | _ ->
                if not(List.isEmpty genArgs) then
                    "Cannot type test generic arguments"
                    |> addWarning com [] range
                let entRef = jsConstructor com ctx ent
                jsInstanceof entRef expr
        | Fable.MetaType | Fable.Option _ | Fable.GenericParam _ | Fable.ErasedUnion _ ->
            fail "options, generic parameters or erased unions"

    let transformTest (com: IBabelCompiler) ctx range kind expr: Expression =
        match kind with
        | Fable.TypeTest t
        | Fable.ErasedUnionTest t ->
            transformTypeTest com ctx range expr t
        | Fable.OptionTest nonEmpty ->
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            upcast BinaryExpression(op, com.TransformAsExpr(ctx, expr), NullLiteral(), ?loc=range)
        | Fable.ListTest nonEmpty ->
            let expr = com.TransformAsExpr(ctx, expr)
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            upcast BinaryExpression(op, get None expr "tail", NullLiteral(), ?loc=range)
        | Fable.UnionCaseTest(uci, ent) ->
            let expected = FSharp2Fable.Helpers.unionCaseTag ent uci |> ofInt
            let actual = com.TransformAsExpr(ctx, expr) |> getUnionExprTag None
            upcast BinaryExpression(BinaryEqualStrict, actual, expected, ?loc=range)

    let transformSwitch (com: IBabelCompiler) ctx useBlocks returnStrategy evalExpr cases defaultCase: Statement =
        let consequent caseBody =
            if useBlocks then [|BlockStatement caseBody :> Statement|] else caseBody
        let cases =
            cases |> List.collect (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | None, Fable.Value(Fable.UnitConstant,_), _
                | _, _, [] -> []
                | _, _, guards ->
                    let guards, lastGuard = List.splitLast guards
                    let guards = guards |> List.map (fun e -> SwitchCase([||], com.TransformAsExpr(ctx, e)))
                    let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                    let caseBody =
                        match returnStrategy with
                        | Some Return -> caseBody
                        | _ -> Array.append caseBody [|BreakStatement() :> Statement|]
                    guards @ [SwitchCase(consequent caseBody, com.TransformAsExpr(ctx, lastGuard))]
                )
        let cases =
            match defaultCase with
            | Some expr ->
                let defaultCaseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                cases @ [SwitchCase(consequent defaultCaseBody)]
            | None -> cases
        SwitchStatement(com.TransformAsExpr(ctx, evalExpr), List.toArray cases) :> Statement

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.sameLength idents values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IBabelCompiler) (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues
        if not com.Options.debugMode then
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

    let transformDecisionTreeSuccessAsExpr (com: IBabelCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings -> com.TransformAsExpr(ctx, Fable.Let(bindings, target))

    let transformDecisionTreeSuccessAsStatements (com: IBabelCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement[] =
        match returnStrategy with
        | Some(Target targetId) ->
            let idents, _ = getDecisionTarget ctx targetIndex
            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
                    assign None (ident id) value |> ExpressionStatement :> Statement)
            let targetAssignment = assign None targetId (ofInt targetIndex) |> ExpressionStatement :> Statement
            Array.append [|targetAssignment|] assignments
        | ret ->
            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.BinaryOperation(BinaryEqualStrict, expr, right), _, _) ->
                Some(expr, right)
            | Fable.Test(expr, Fable.UnionCaseTest(uci, ent), _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number Int32, None)
                let right = Fable.NumberConstant(FSharp2Fable.Helpers.unionCaseTag ent uci |> float, Int32) |> makeValue None
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

    let transformDecisionTreeAsExpr (com: IBabelCompiler) (ctx: Context) targets expr: Expression =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let groupSwitchCases t (cases: (Fable.Expr * int * Fable.Expr list) list) =
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
    let transformDecisionTreeWithTwoSwitches (com: IBabelCompiler) ctx returnStrategy
                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
        // Declare $target and bound idents
        let targetId = makeIdentUnique com "target"
        let varDeclaration =
            let boundIdents = targets |> List.collect (fun (idents,_) ->
                idents |> List.map (fun id -> typedIdent com ctx id, None))
            multiVarDeclaration Var ((typedIdent com ctx targetId, None)::boundIdents)
        // Transform targets as switch
        let switch2 =
            // TODO: Declare the last case as the default case?
            let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
            transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None
        // Transform decision tree
        let targetAssign = Target(ident targetId)
        let ctx = { ctx with DecisionTargets = targets }
        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases = groupSwitchCases (Fable.Number Int32) cases
            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number Int32)
            let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
            [|varDeclaration; switch1; switch2|]
        | None ->
            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
            [| yield varDeclaration; yield! decisionTree; yield switch2 |]

    let transformDecisionTreeAsStatements (com: IBabelCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement[] =
        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        let targetsWithMultiRefs =
            if com.Options.typeDecls then [] // no hoisting when compiled with types
            else getTargetsWithMultipleReferences treeExpr
        match targetsWithMultiRefs with
        | [] ->
            let ctx = { ctx with DecisionTargets = targets }
            match transformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                let t = treeExpr.Type
                let cases = cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                    [caseExpr], Fable.DecisionTreeSuccess(targetIndex, boundValues, t))
                let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
                [|transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)|]
            | None ->
                com.TransformAsStatements(ctx, returnStrategy, treeExpr)
        | targetsWithMultiRefs ->
            // If the bound idents are not referenced in the target, remove them
            let targets =
                targets |> List.map (fun (idents, expr) ->
                    if idents |> List.exists (fun i ->
                        expr |> FableTransforms.deepExists (function
                            | Fable.IdentExpr i2 -> i2.Name = i.Name
                            | _ -> false)) then idents, expr
                    else [], expr)
            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs |> List.exists (fun idx ->
                    targets.[idx] |> fst |> List.isEmpty |> not)
            if not hasAnyTargetWithMultiRefsBoundValues then
                match transformDecisionTreeAsSwitch treeExpr with
                | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                    let t = treeExpr.Type
                    let cases = groupSwitchCases t cases
                    let ctx = { ctx with DecisionTargets = targets }
                    let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
                    [|transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)|]
                | None ->
                    transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr
            else
                transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr

    let rec transformAsExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        | Fable.TypeCast(e,t) -> transformCast com ctx t e

        | Fable.DelayedResolution(kind, _, r) -> transformDelayedResolution com ctx r kind

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> upcast ident id

        | Fable.Import(selector, path, kind, _, r) ->
            transformImport com ctx r selector path kind

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Function(FunctionArgs args, body, name) ->
            transformFunc com ctx name args body |> makeFunctionExpression name

        | Fable.ObjectExpr (members, _, baseCall) ->
           transformObjectExpr com ctx members "this" baseCall

        | Fable.Operation(opKind, _, range) ->
            transformOperation com ctx range opKind

        | Fable.Get(expr, getKind, typ, range) ->
            transformGet com ctx range typ expr getKind

        | Fable.IfThenElse(TransformExpr com ctx guardExpr,
                           TransformExpr com ctx thenExpr,
                           TransformExpr com ctx elseExpr, r) ->
            upcast ConditionalExpression(guardExpr, thenExpr, elseExpr, ?loc=r)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(var, setKind, value, range) ->
            transformSet com ctx range var value setKind

        | Fable.Let(bindings, body) ->
            if ctx.HoistVars(List.map fst bindings) then
                let values = bindings |> List.mapToArray (fun (id, value) ->
                    transformBindingAsExpr com ctx id value)
                upcast SequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
            else upcast iife com ctx expr

        | Fable.Sequential exprs ->
            List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
            |> SequenceExpression :> Expression

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.Debugger _ | Fable.Throw _ | Fable.Loop _ | Fable.TryCatch _ ->
            iife com ctx expr :> Expression

    let rec transformAsStatements (com: IBabelCompiler) ctx returnStrategy
                                    (expr: Fable.Expr): Statement array =
        match expr with
        | Fable.TypeCast(e, t) ->
            [|transformCast com ctx t e |> resolveExpr t returnStrategy|]

        | Fable.DelayedResolution(kind, t, r) ->
            [|transformDelayedResolution com ctx r kind |> resolveExpr t returnStrategy|]

        | Fable.Value(kind, r) ->
            [|transformValue com ctx r kind |> resolveExpr kind.Type returnStrategy|]

        | Fable.IdentExpr id ->
            [|identAsExpr id |> resolveExpr id.Type returnStrategy|]

        | Fable.Import(selector, path, kind, t, r) ->
            [|transformImport com ctx r selector path kind |> resolveExpr t returnStrategy|]

        | Fable.Test(expr, kind, range) ->
            [|transformTest com ctx range kind expr |> resolveExpr Fable.Boolean returnStrategy|]

        | Fable.Function(FunctionArgs args, body, name) ->
            [|transformFunc com ctx name args body |> makeFunctionExpression name
               |> resolveExpr expr.Type returnStrategy|]

        | Fable.ObjectExpr (members, t, baseCall) ->
            [|transformObjectExpr com ctx members "this" baseCall |> resolveExpr t returnStrategy|]

        | Fable.Operation(callKind, t, range) ->
            transformOperationAsStatements com ctx range t returnStrategy callKind

        | Fable.Get(expr, getKind, t, range) ->
            [|transformGet com ctx range t expr getKind |> resolveExpr t returnStrategy|]

        | Fable.Let(bindings, body) ->
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, setKind, value, _range) ->
            let ret = getSetReturnStrategy com ctx expr setKind
            com.TransformAsStatements(ctx, Some ret, value)

        | Fable.Throw(TransformExpr com ctx ex, _, range) ->
            [|ThrowStatement(ex, ?loc=range) :> Statement|]

        | Fable.Debugger range ->
            [|DebuggerStatement(?loc=range) :> Statement|]

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflicts (e.g. `then` doesn't become a block while `else` does)
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
                match com.TransformAsExpr(ctx, guardExpr) with
                // In some situations (like some type tests) the condition may be always true
                | :? BooleanLiteral as e when e.Value -> com.TransformAsStatements(ctx, returnStrategy, thenExpr)
                | guardExpr -> [|transformIfStatement com ctx r returnStrategy guardExpr thenExpr elseExpr :> Statement|]
            else
                let guardExpr' = transformAsExpr com ctx guardExpr
                let thenExpr' = transformAsExpr com ctx thenExpr
                let elseExpr' = transformAsExpr com ctx elseExpr
                [|ConditionalExpression(guardExpr', thenExpr', elseExpr', ?loc=r) |> resolveExpr thenExpr.Type returnStrategy|]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapiToArray (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                com.TransformAsStatements(ctx, ret, statement))
            |> Array.concat

        | Fable.TryCatch (body, catch, finalizer, r) ->
            transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsStatements com ctx returnStrategy targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsStatements com ctx returnStrategy idx boundValues

        | Fable.Loop (loopKind, range) ->
            match loopKind with
            | Fable.While (TransformExpr com ctx guard, body) ->
                WhileStatement(guard, transformBlock com ctx None body, ?loc=range) :> Statement
            | Fable.For (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp) ->
                let op1, op2 =
                    if isUp
                    then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                    else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus
                ForStatement(
                    transformBlock com ctx None body,
                    start |> varDeclaration (typedIdent com ctx var) true |> U2.Case1,
                    BinaryExpression (op1, ident var, limit),
                    UpdateExpression (op2, false, ident var), ?loc=range) :> Statement
            |> Array.singleton

    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr) =
        let tailcallChance =
            Option.map (fun name ->
                NamedTailCallOpportunity(com, name, args) :> ITailCallOpportunity) name
        let args = discardUnitArg args |> List.map (typedIdent com ctx)
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with TailCallOpportunity = tailcallChance
                       HoistVars = fun ids -> declaredVars.AddRange(ids); true
                       OptimizeTailCall = fun () -> isTailCallOptimized <- true }
        let body: U2<BlockStatement, Expression> =
            if body.Type = Fable.Unit
            then transformBlock com ctx (Some ReturnUnit) body |> U2.Case1
            elif isJsStatement ctx (Option.isSome tailcallChance) body
            then transformBlock com ctx (Some Return) body |> U2.Case1
            else transformAsExpr com ctx body |> U2.Case2
        let args, body =
            match isTailCallOptimized, tailcallChance, body with
            | true, Some tc, U2.Case1 body ->
                // Replace args, see NamedTailCallOpportunity constructor
                let args, body =
                    let tcArgs =
                        tc.Args
                        |> List.zip args
                        |> List.map (fun (arg, tcArg) ->
                            Identifier(tcArg, ?typeAnnotation=arg.TypeAnnotation))
                    let varDecls =
                        tcArgs
                        |> List.map (fun arg -> Some (arg :> Expression))
                        |> List.zip args
                        |> multiVarDeclaration Const
                    tcArgs, BlockStatement(Array.append [|varDecls|] body.Body)
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body = BlockStatement(Array.append body.Body [|BreakStatement()|])
                args, LabeledStatement(Identifier tc.Label, WhileStatement(BooleanLiteral true, body))
                :> Statement |> Array.singleton |> BlockStatement |> U2.Case1
            | _ -> args, body
        let body =
            if declaredVars.Count = 0
            then body
            else
                let varDeclStatement =
                    multiVarDeclaration Var [for v in declaredVars -> typedIdent com ctx v, None]
                let bodyStatements =
                    match body with
                    | U2.Case1 bodyBlock -> bodyBlock.Body
                    | U2.Case2 bodyExpr -> [|ReturnStatement(bodyExpr, ?loc=bodyExpr.Loc) :> Statement|]
                BlockStatement(Array.append [|varDeclStatement|] bodyStatements) |> U2.Case1
        args |> List.mapToArray toPattern, body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = macroExpression None "process.argv.slice(2)" []
        let main = CallExpression (funcExpr, [|argv|]) :> Expression
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        ExpressionStatement(main) :> Statement

    let declareModuleMember r isPublic name isMutable (expr: Expression) =
        let privateIdent = Identifier name
        let decl: Declaration =
            match expr with
            | :? ClassExpression as e ->
                upcast ClassDeclaration(e.Body, ?id=Some privateIdent,
                    ?superClass=e.SuperClass, ?typeParameters=e.TypeParameters, ?loc=r)
            | :? FunctionExpression as e ->
                upcast FunctionDeclaration(e.Params, e.Body, ?id=Some privateIdent,
                    ?returnType=e.ReturnType, ?typeParameters=e.TypeParameters, ?loc=r)
            | _ -> upcast varDeclaration privateIdent isMutable expr
        if not isPublic
        then U2.Case1 (decl :> Statement)
        else
            ExportNamedDeclaration(decl)
            :> ModuleDeclaration |> U2.Case2

    let makeEntityTypeParamDecl (com: IBabelCompiler) ctx (ent: FSharpEntity) =
        if com.Options.typeDecls then
            getEntityGenParams ent |> makeTypeParamDecl
        else
            None

    let makeInterfaceExtends com ctx (ent: FSharpEntity) =
        ent.AllInterfaces
        |> Seq.choose (fun typ ->
            match FSharp2Fable.TypeHelpers.makeType com Map.empty typ with
            | Fable.DeclaredType(ent, genArgs) ->
                match ent.TryFullName with
                | Some Types.ienumerableGeneric ->
                    let id = Identifier("Iterable")
                    let typeParamInst = makeGenTypeParamInst com ctx genArgs
                    InterfaceExtends(id, ?typeParameters=typeParamInst) |> Some
                // | Some Types.iequatableGeneric ->
                //     let id = makeImportTypeId com ctx "Util" "IEquatable"
                //     let typeParamInst = makeGenTypeParamInst com ctx genArgs
                //     InterfaceExtends(id, ?typeParameters=typeParamInst) |> Some
                | Some Types.iequatable ->
                    let id = makeImportTypeId com ctx "Types" "IEquatable"
                    InterfaceExtends(id) |> Some
                | Some Types.icomparable ->
                    let id = makeImportTypeId com ctx "Types" "IComparable"
                    InterfaceExtends(id) |> Some
                | Some Types.idisposable ->
                    let id = makeImportTypeId com ctx "Util" "IDisposable"
                    InterfaceExtends(id) |> Some
                // TODO: other interfaces
                | _ -> None
            | _ -> None
        )

    let makeInterfaceExpression com ctx r (ent: FSharpEntity) name (baseExpr: Expression option) =
        let properties =
            ent.FSharpFields
            |> Seq.map (fun field ->
                let id =
                    if Naming.hasIdentForbiddenChars field.Name
                    then StringLiteral(field.Name) |> U2.Case2
                    else Identifier(field.Name) |> U2.Case1
                let ta =
                    FSharp2Fable.TypeHelpers.makeType com Map.empty field.FieldType
                    |> typeAnnotation com ctx
                ObjectTypeProperty(id, ta))
            |> Seq.toArray
        let baseExt =
            match baseExpr with
            | Some expr ->
                match expr with
                | :? Identifier as id ->
                    let typeParamInst =
                        FSharp2Fable.Helpers.tryEntityBase ent
                        |> Option.bind (getEntityGenParams >> makeTypeParamInst)
                    InterfaceExtends(id, ?typeParameters=typeParamInst) |> Seq.singleton
                | _ -> Seq.empty
            | _ -> Seq.empty
        let interfaceExt = makeInterfaceExtends com ctx ent
        let allExt = Seq.append baseExt interfaceExt |> Seq.toArray
        let extends = if Array.isEmpty allExt then None else Some allExt
        let id = Identifier(name)
        let body = ObjectTypeAnnotation(properties)
        let typeParamDecl = getEntityGenParams ent |> makeTypeParamDecl
        InterfaceDeclaration(id, body, ?extends_=extends, ?typeParameters=typeParamDecl, ?loc=r)

    let declareObjectType (com: IBabelCompiler) ctx r isPublic (ent: FSharpEntity) name (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr =
        let displayName =
            ent.TryGetFullDisplayName()
            |> Option.map (Naming.unsafeReplaceIdentForbiddenChars '_')
            |> Option.defaultValue name
        let returnType = None
        let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let consFunction = makeFunctionExpression (Some displayName) (consArgs, U2.Case1 consBody, returnType, typeParamDecl)
        match baseExpr with
        | Some e -> [|consFunction; e|]
        | None -> [|consFunction|]
        |> coreLibCall com ctx None "Types" "declare"
        |> declareModuleMember r isPublic name false

    let declareClassType (com: IBabelCompiler) ctx r isPublic (ent: FSharpEntity) name (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr =
        let key = Identifier "constructor"
        let returnType = None
        let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let classMethod = ClassMethod(ClassImplicitConstructor, key, consArgs, consBody, false, false, ?returnType=returnType, ?typeParameters=typeParamDecl, ?loc=r)
        let classBody = ClassBody([| U2.Case1 classMethod |], ?loc=r)
        let classExpr = ClassExpression(classBody, ?superClass=baseExpr, ?loc=r)
        classExpr |> declareModuleMember r isPublic name false

    let declareType com ctx r isPublic (ent: FSharpEntity) name (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr: U2<Statement, ModuleDeclaration> list =
        let typeDeclaration =
            // if ent.IsFSharpUnion || ent.IsFSharpRecord || ent.IsValueType || ent.IsFSharpExceptionDeclaration
            // then declareClassType com ctx r isPublic ent name consArgs consBody baseExpr
            // else
            declareObjectType com ctx r isPublic ent name consArgs consBody baseExpr
        let reflectionDeclaration =
            let genArgs = Array.init ent.GenericParameters.Count (fun _ -> makeIdentUnique com "gen" |> typedIdent com ctx)
            let body = transformReflectionInfo com ctx r ent (Array.map (fun x -> x :> _) genArgs)
            let returnType =
                if com.Options.typeDecls then
                    makeImportTypeAnnotation com ctx [] "Reflection" "TypeInfo"
                    |> TypeAnnotation |> Some
                else None
            makeFunctionExpression None (Array.map (fun x -> U2.Case2(upcast x)) genArgs, U2.Case2 body, returnType, None)
            |> declareModuleMember None isPublic (Naming.appendSuffix name Naming.reflectionSuffix) false
        if com.Options.typeDecls then
            let interfaceExpr = makeInterfaceExpression com ctx r ent name baseExpr
            let interfaceDeclaration =
                ExportNamedDeclaration(interfaceExpr) :> ModuleDeclaration |> U2.Case2
            [interfaceDeclaration; typeDeclaration; reflectionDeclaration]
        else
            [typeDeclaration; reflectionDeclaration]

    let transformModuleFunction (com: IBabelCompiler) ctx (info: Fable.ValueDeclarationInfo) args body =
        let args, body, returnType, typeParamDecl = getMemberArgsAndBody com ctx (Some info.Name) None args info.HasSpread body
        // Don't lexically bind `this` (with arrow function) or it will fail with extension members
        let expr: Expression = upcast FunctionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)
        if info.IsEntryPoint then
            declareEntryPoint com ctx expr |> U2.Case1
        else
            declareModuleMember info.Range info.IsPublic info.Name false expr

    let transformAction (com: IBabelCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        let hasVarDeclarations =
            statements |> Array.exists (function
                | :? VariableDeclaration -> true
                | _ -> false)
        if hasVarDeclarations then
            [ CallExpression(FunctionExpression([||], BlockStatement(statements)), [||])
              |> ExpressionStatement :> Statement |> U2.Case1 ]
        else Array.map U2.Case1 statements |> Array.toList

    let transformOverrideProperty (com: IBabelCompiler) ctx (info: Fable.AttachedMemberDeclarationInfo) getter setter =
        let funcExpr (args, body) =
            let boundThis, args = prepareBoundThis "this" args
            let args, body, returnType, typeParamDecl = getMemberArgsAndBody com ctx None boundThis args false body
            makeFunctionExpression None (args, U2.Case1 body, returnType, typeParamDecl)
        let getterFuncExpr = Option.map funcExpr getter
        let setterFuncExpr = Option.map funcExpr setter
        let funcCons = Identifier info.EntityName :> Expression
        jsObject "defineProperty" [|
            get None funcCons "prototype"
            StringLiteral info.Name
            ObjectExpression [|
                match getterFuncExpr with
                | Some e -> yield ObjectProperty(StringLiteral "get", e) |> U3.Case1
                | None -> ()
                match setterFuncExpr with
                | Some e -> yield ObjectProperty(StringLiteral "set", e) |> U3.Case1
                | None -> ()
            |]
        |]
        |> ExpressionStatement :> Statement
        |> U2<_,ModuleDeclaration>.Case1 |> List.singleton

    let transformOverrideMethod (com: IBabelCompiler) ctx (info: Fable.AttachedMemberDeclarationInfo) args body =
        let funcCons = Identifier info.EntityName :> Expression
        let memberName, hasSpread, body =
            match info.Kind with
            | Fable.ObjectIterator -> "Symbol.iterator", false, Replacements.enumerator2iterator body
            | Fable.ObjectMethod hasSpread -> info.Name, hasSpread, body
            | _ -> info.Name, false, body
        let boundThis, args = prepareBoundThis "this" args
        let args, body, returnType, typeParamDecl = getMemberArgsAndBody com ctx None boundThis args hasSpread body
        let funcExpr = makeFunctionExpression None (args, U2.Case1 body, returnType, typeParamDecl)
        let protoMember =
            match memberName with
            | Naming.StartsWith "Symbol." symbolName -> get None (Identifier "Symbol") symbolName
            // Compile ToString in lower case for compatibity with JS (and debugger tools)
            | "ToString" -> upcast StringLiteral "toString"
            | name -> upcast StringLiteral name
        let protoMember = getExpr None (get None funcCons "prototype") protoMember
        assign None protoMember funcExpr
        |> ExpressionStatement :> Statement
        |> U2<_,ModuleDeclaration>.Case1 |> List.singleton

    let transformUnionConstructor (com: IBabelCompiler) ctx r (info: Fable.UnionConstructorInfo) =
        let baseRef = coreValue com ctx "Types" "Union"
        let args =
            [|Identifier "tag" |> toPattern
              Identifier "name" |> toPattern
              Identifier "fields" |> restElement|]
        let body =
            [Identifier "tag" :> Expression; Identifier "name" :> _; SpreadElement(Identifier "fields") :> _]
            |> callFunctionWithThisContext None baseRef thisExpr |> ExpressionStatement
        declareType com ctx r info.IsPublic info.Entity info.EntityName args (BlockStatement [|body|]) (Some baseRef)

    let transformCompilerGeneratedConstructor (com: IBabelCompiler) ctx r (info: Fable.CompilerGeneratedConstructorInfo) =
        let args =
            [| for i = 1 to info.Entity.FSharpFields.Count do
                yield Identifier("arg" + string i) |]
        let setters =
            info.Entity.FSharpFields
            |> Seq.mapi (fun i field ->
                let left = get None (ThisExpression()) field.Name
                let right =
                    /// Shortcut instead of using wrapIntExpression
                    if FSharp2Fable.TypeHelpers.isSignedIntType field.FieldType
                    then BinaryExpression(BinaryOrBitwise, args.[i], NumericLiteral(0.)) :> Expression
                    else args.[i] :> _
                assign None left right |> ExpressionStatement :> Statement)
            |> Seq.toArray
        let baseExpr =
            if info.Entity.IsFSharpExceptionDeclaration
            then coreValue com ctx "Types" "FSharpException" |> Some
            elif info.Entity.IsFSharpRecord || info.Entity.IsValueType
            then coreValue com ctx "Types" "Record" |> Some
            else None
        let args = [|for arg in args do yield arg |> toPattern|]
        declareType com ctx r info.IsPublic info.Entity info.EntityName args (BlockStatement setters) baseExpr

    let transformImplicitConstructor (com: IBabelCompiler) ctx r (info: Fable.ClassImplicitConstructorInfo) =
        let boundThis = Some("this", info.BoundConstructorThis)
        let consIdent = Identifier(info.EntityName) :> Expression
        let args, body, returnType, typeParamDecl = getMemberArgsAndBody com ctx None boundThis info.Arguments info.HasSpread info.Body
        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.Options.typeDecls then
                let id = Identifier(info.EntityName)
                let genParams = getEntityGenParams info.Entity
                let typeParamInst = makeTypeParamInst genParams
                let returnType =
                    GenericTypeAnnotation(id, ?typeParameters=typeParamInst) :> TypeAnnotationInfo
                    |> TypeAnnotation |> Some
                let typeParamDecl = makeTypeParamDecl genParams |> mergeTypeParamDecls typeParamDecl
                returnType, typeParamDecl
            else
                returnType, typeParamDecl
        let typedPattern = typedIdent com ctx >> toPattern
        let argIdents, argExprs: Pattern list * Expression list =
            match info.Arguments with
            | [] -> [], []
            | [unitArg] when unitArg.Type = Fable.Unit -> [], []
            | args when info.HasSpread ->
                let args = List.rev args
                (restElement(typedIdent com ctx args.Head)) :: (List.map typedPattern args.Tail) |> List.rev,
                (SpreadElement(ident args.Head) :> Expression) :: (List.map identAsExpr args.Tail) |> List.rev
            | args ->
                args |> List.map typedPattern,
                args |> List.map identAsExpr
        let exposedCons =
            let body =
                BlockStatement [|
                    ReturnStatement(
                        ConditionalExpression(
                            // Don't do a null check here, some environments can assign a value to `this`, see #1757
                            BinaryExpression(BinaryInstanceOf, ThisExpression(), consIdent),
                            callFunctionWithThisContext None consIdent thisExpr argExprs,
                            NewExpression(consIdent, List.toArray argExprs))
                    ) |]
            let consArgs = List.toArray argIdents
            makeFunctionExpression None (consArgs, U2.Case1 body, returnType, typeParamDecl)
        let baseExpr =
            match info.Base with
            | Some(TransformExpr com ctx baseRef) -> Some baseRef
            // Structs have same properties as records
            | None when info.Entity.IsValueType -> coreValue com ctx "Types" "Record" |> Some
            | None -> None
        [
            yield! declareType com ctx r info.IsEntityPublic info.Entity info.EntityName args body baseExpr
            yield declareModuleMember r info.IsConstructorPublic info.Name false exposedCons
        ]

    let rec transformDeclarations (com: IBabelCompiler) ctx decls transformed =
        match decls with
        | [] -> transformed
        | decl::restDecls ->
            match decl with
            | Fable.ActionDeclaration e ->
                transformAction com ctx e
                |> List.append transformed
                |> transformDeclarations com ctx restDecls
            | Fable.ValueDeclaration(value, info) ->
                match value with
                // Mutable public values must be compiled as functions (see #986)
                // because values imported from ES2015 modules cannot be modified
                | value when info.IsMutable && info.IsPublic ->
                    Replacements.createAtom value
                    |> transformAsExpr com ctx
                    |> declareModuleMember info.Range true info.Name false
                    |> List.singleton
                | Fable.Function(Fable.Delegate args, body, _) ->
                    [transformModuleFunction com ctx info args body]
                | _ ->
                    let value = transformAsExpr com ctx value
                    [declareModuleMember info.Range info.IsPublic info.Name info.IsMutable value]
                |> List.append transformed
                |> transformDeclarations com ctx restDecls
            | Fable.ConstructorDeclaration(kind, r) ->
                let consDecls =
                    match kind with
                    | Fable.ClassImplicitConstructor info ->
                        transformImplicitConstructor com ctx r info
                    | Fable.UnionConstructor info ->
                        transformUnionConstructor com ctx r info
                    | Fable.CompilerGeneratedConstructor info ->
                        transformCompilerGeneratedConstructor com ctx r info
                consDecls
                |> List.append transformed
                |> transformDeclarations com ctx restDecls
            | Fable.AttachedMemberDeclaration(args, body, info) ->
                let newDecls, restDecls =
                    match info.Kind with
                    | Fable.ObjectGetter | Fable.ObjectSetter as kind ->
                        let getter, setter, restDecls =
                            // Check if the next declaration is a getter/setter for same property
                            match restDecls with
                            | Fable.AttachedMemberDeclaration(args2, body2, info2)::restDecls when info.Name = info2.Name ->
                                match kind with
                                | Fable.ObjectGetter -> Some(args, body), Some(args2, body2), restDecls
                                | _ -> Some(args2, body2), Some(args, body), restDecls
                            | _ ->
                                match kind with
                                | Fable.ObjectGetter -> Some(args, body), None, restDecls
                                | _ -> None, Some(args, body), restDecls
                        transformOverrideProperty com ctx info getter setter, restDecls
                    | _ -> transformOverrideMethod com ctx info args body, restDecls
                List.append transformed newDecls
                |> transformDeclarations com ctx restDecls

    let transformImports (imports: Import seq): U2<Statement, ModuleDeclaration> list =
        imports |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier(localId)
                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId) |> U3.Case3
                    | "default" | "" -> ImportDefaultSpecifier(localId) |> U3.Case2
                    | memb -> ImportSpecifier(localId, Identifier memb) |> U3.Case1)
            import.Path, specifier)
        |> Seq.groupBy fst
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    let t =
                        match x with
                        | U3.Case1 x -> x.Type
                        | U3.Case2 x -> x.Type
                        | U3.Case3 x -> x.Type
                    match t with
                    | "ImportNamespaceSpecifier" -> mems, defs, x::alls
                    | "ImportDefaultSpecifier" -> mems, x::defs, alls
                    | _ -> x::mems, defs, alls)
            // There seem to be errors if we mix member, default and namespace imports
            // so we must issue an import statement for each kind
            match [mems; defs; alls] with
            | [[];[];[]] ->
                // No specifiers, so this is just an import for side effects
                [ImportDeclaration([||], StringLiteral path) :> ModuleDeclaration |> U2.Case2]
            | specifiers ->
                specifiers |> List.choose (function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(List.toArray specifiers, StringLiteral path)
                    :> ModuleDeclaration |> U2.Case2 |> Some))
        |> Seq.toList

    let getLocalIdent (ctx: Context) (imports: Dictionary<string,Import>) (path: string) (selector: string) =
        match selector with
        | "" -> None
        | "*" | "default" ->
            let x = path.TrimEnd('/')
            x.Substring(x.LastIndexOf '/' + 1) |> Some
        | selector -> Some selector
        |> Option.map (fun selector ->
            (selector, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun s ->
            ctx.File.UsedVarNames.Contains s
                || (imports.Values |> Seq.exists (fun i -> i.LocalIdent = Some s))))

module Compiler =
    open Util

    type BabelCompiler (com: ICompiler) =
        let imports = Dictionary<string,Import>()

        interface IBabelCompiler with
            member __.GetImportExpr(ctx, selector, path, kind) =
                let sanitizedPath =
                    match kind with
                    | Fable.CustomImport | Fable.Internal -> path
                    | Fable.Library -> com.LibraryDir + "/" + path + Naming.targetFileExtension
                let cachedName = sanitizedPath + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> upcast Identifier(localIdent)
                    | None -> upcast NullLiteral ()
                | false, _ ->
                    let localId = getLocalIdent ctx imports path selector
                    let i =
                      { Selector =
                            if selector = Naming.placeholder
                            then "`importMember` must be assigned to a variable"
                                 |> addError com [] None; selector
                            else selector
                        LocalIdent = localId
                        Path = sanitizedPath }
                    imports.Add(cachedName, i)
                    match localId with
                    | Some localId -> upcast Identifier(localId)
                    | None -> upcast NullLiteral ()
            member __.GetAllImports() = upcast imports.Values
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformObjectExpr(ctx, members, boundThis, baseCall) = transformObjectExpr bcom ctx members boundThis baseCall
            member bcom.TransformImport(ctx, selector, path, kind) = transformImport bcom ctx None (makeStrConst selector) (makeStrConst path) kind

        interface ICompiler with
            member __.Options = com.Options
            member __.LibraryDir = com.LibraryDir
            member __.CurrentFile = com.CurrentFile
            member __.GetUniqueVar(name) = com.GetUniqueVar(?name=name)
            member __.GetRootModule(fileName) = com.GetRootModule(fileName)
            member __.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = BabelCompiler(com)

    let createFacade (sourceFiles: string[]) (facadeFile: string) =
        // Remove signature files so fable-splitter doesn't try to compile them
        // when `allFiles` option is selected
        let sourceFiles = sourceFiles |> Array.filter (fun x -> x.EndsWith(".fsi") |> not)
        let decls =
            let importFile = Array.last sourceFiles
            StringLiteral(Path.getRelativeFileOrDirPath false facadeFile false importFile)
            |> ExportAllDeclaration :> ModuleDeclaration |> U2.Case2 |> Array.singleton
        Program(facadeFile, decls, sourceFiles_ = sourceFiles)

    let transformFile (com: ICompiler) (file: Fable.File) =
        try
            // let t = PerfTimer("Fable > Babel")
            let com = makeCompiler com :> IBabelCompiler
            let ctx =
              { File = file
                DecisionTargets = []
                HoistVars = fun _ -> false
                TailCallOpportunity = None
                OptimizeTailCall = fun () -> ()
                ScopedTypeParams = Set.empty }
            let rootDecls = transformDeclarations com ctx file.Declarations []
            let importDecls = com.GetAllImports() |> transformImports
            let body = importDecls @ rootDecls |> List.toArray
            // We don't add imports as dependencies because those will be handled by Webpack
            // TODO: Do it for other clients, like fable-splitter?
            let dependencies = Array.ofSeq file.InlineDependencies
            Program(file.SourcePath, body, dependencies_ = dependencies)
        with
        | ex -> exn (sprintf "%s (%s)" ex.Message file.SourcePath, ex) |> raise
