module rec Fable.Transforms.Fable2Babel

open Fable
open Fable.AST
open Fable.AST.Babel
open System.Collections.Generic
open System.Text.RegularExpressions

type ReturnStrategy =
    | Return
    | ReturnUnit
    | Assign of Expression
    | Target of Identifier

type ArgsInfo =
    | CallInfo of Fable.CallInfo * Fable.MemberFunctionOrValue option
    | NoCallInfo of args: Fable.Expr list

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
    IsParamType: bool
    ScopedTypeParams: Set<string> }

type IBabelCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> seq<Import>
    abstract GetImportExpr: Context * selector: string * path: string * range: SourceLocation option * ?noMangle: bool -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement array
    abstract TransformImport: Context * selector:string * path:string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Lib =

    let libCall (com: IBabelCompiler) ctx r moduleName memberName genArgs args =
        let typeParamInst = Annotation.makeTypeParamInstantiationIfTypeScript com ctx genArgs
        let callee = com.TransformImport(ctx, memberName, getLibPath com moduleName)
        Expression.callExpression(callee, List.toArray args, ?typeParameters=typeParamInst, ?loc=r)

    let libValue (com: IBabelCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryJsConstructorWithSuffix (com: IBabelCompiler) ctx ent (suffix: string) =
        match JS.Replacements.tryConstructor com ent with
        | Some(Fable.Import(info, typ, range)) when suffix.Length > 0 ->
            let consExpr = Fable.Import({ info with Selector = info.Selector + suffix }, typ, range)
            com.TransformAsExpr(ctx, consExpr) |> Some
        | Some(Fable.IdentExpr ident) when suffix.Length > 0 ->
            let consExpr = Fable.IdentExpr { ident with Name = ident.Name + suffix }
            com.TransformAsExpr(ctx, consExpr) |> Some
        | consExpr -> consExpr |> Option.map (fun e -> com.TransformAsExpr(ctx, e))

    let tryJsConstructorForAnnotation forAnnotation (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        // TODO: Check this is not an StringEnum or Erase union
        let suffix = if not forAnnotation && com.Options.Language = TypeScript && ent.IsFSharpUnion then "_Cons" else ""
        tryJsConstructorWithSuffix com ctx ent suffix

    /// Cannot be used for annotations (use `tryJsConstructorForAnnotation true` instead)
    let jsConstructor (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        tryJsConstructorForAnnotation false com ctx ent
        |> Option.defaultWith (fun () ->
            $"Cannot find %s{ent.FullName} constructor"
            |> addError com [] None
            Expression.nullLiteral())

module Reflection =
    open Lib

    let private libReflectionCall (com: IBabelCompiler) ctx r memberName args =
        libCall com ctx r "Reflection" (memberName + "_type") [] args

    let private transformRecordReflectionInfo com ctx r (ent: Fable.Entity) generics =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringLiteral(fullname)
        let genMap =
            let genParamNames = ent.GenericParameters |> List.mapToArray (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map |> Some
        let fields =
            ent.FSharpFields |> Seq.map (fun fi ->
                let typeInfo = transformTypeInfo com ctx r genMap fi.FieldType
                (Expression.arrayExpression([|Expression.stringLiteral(fi.Name); typeInfo|])))
            |> Seq.toArray
        let fields = Expression.arrowFunctionExpression([||], Expression.arrayExpression(fields))
        [fullnameExpr; Expression.arrayExpression(generics); jsConstructor com ctx ent; fields]
        |> libReflectionCall com ctx None "record"

    let private transformUnionReflectionInfo com ctx r (ent: Fable.Entity) generics =
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringLiteral(fullname)
        let genMap =
            let genParamNames = ent.GenericParameters |> List.map (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map |> Some
        let cases =
            ent.UnionCases |> Seq.map (fun uci ->
                uci.UnionCaseFields |> List.mapToArray (fun fi ->
                    Expression.arrayExpression([|
                        fi.Name |> Expression.stringLiteral
                        transformTypeInfo com ctx r genMap fi.FieldType
                    |]))
                |> Expression.arrayExpression
            ) |> Seq.toArray
        let cases = Expression.arrowFunctionExpression([||], Expression.arrayExpression(cases))
        [fullnameExpr; Expression.arrayExpression(generics); jsConstructor com ctx ent; cases]
        |> libReflectionCall com ctx None "union"

    let transformTypeInfo (com: IBabelCompiler) ctx r (genMap: Map<string, Expression> option) t: Expression =
        let primitiveTypeInfo name =
           libValue com ctx "Reflection" (name + "_type")
        let numberInfo kind =
            getNumberKindName kind
            |> primitiveTypeInfo
        let nonGenericTypeInfo fullname =
            [Expression.stringLiteral(fullname)]
            |> libReflectionCall com ctx None "class"
        let resolveGenerics generics: Expression list =
            generics |> List.map (transformTypeInfo com ctx r genMap)
        let genericTypeInfo name genArgs =
            let resolved = resolveGenerics genArgs
            libReflectionCall com ctx None name resolved
        let genericEntity (fullname: string) generics =
            libReflectionCall com ctx None "class" [
                Expression.stringLiteral(fullname)
                if not(Array.isEmpty generics) then
                    Expression.arrayExpression(generics)
            ]
        let genericGlobalOrImportedEntity generics (ent: Fable.Entity) =
            libReflectionCall com ctx None "class" [
                yield Expression.stringLiteral(ent.FullName)
                match generics with
                | [||] -> yield Util.undefined None
                | generics -> yield Expression.arrayExpression(generics)
                match tryJsConstructorForAnnotation false com ctx ent with
                | Some cons -> yield cons
                | None -> ()
            ]
        match t with
        | Fable.Measure _
        | Fable.Any -> primitiveTypeInfo "obj"
        | Fable.GenericParam(name=name) ->
            match genMap with
            | None -> [Expression.stringLiteral(name)] |> libReflectionCall com ctx None "generic"
            | Some genMap ->
                match Map.tryFind name genMap with
                | Some t -> t
                | None ->
                    Replacements.Util.genericTypeInfoError name |> addError com [] r
                    Expression.nullLiteral()
        | Fable.Unit    -> primitiveTypeInfo "unit"
        | Fable.Boolean -> primitiveTypeInfo "bool"
        | Fable.Char    -> primitiveTypeInfo "char"
        | Fable.String  -> primitiveTypeInfo "string"
        | Fable.Number(kind, info) ->
            match info with
            | Fable.NumberInfo.IsEnum entRef ->
                let ent = com.GetEntity(entRef)
                let cases =
                    ent.FSharpFields |> Seq.choose (fun fi ->
                        match fi.Name with
                        | "value__" -> None
                        | name ->
                            let value = match fi.LiteralValue with Some v -> System.Convert.ToDouble v | None -> 0.
                            Expression.arrayExpression([|Expression.stringLiteral(name); Expression.numericLiteral(value)|]) |> Some)
                    |> Seq.toArray
                    |> Expression.arrayExpression
                [Expression.stringLiteral(entRef.FullName); numberInfo kind; cases ]
                |> libReflectionCall com ctx None "enum"
            | _ ->
                numberInfo kind
        | Fable.LambdaType(argType, returnType) ->
            genericTypeInfo "lambda" [argType; returnType]
        | Fable.DelegateType(argTypes, returnType) ->
            genericTypeInfo "delegate" [yield! argTypes; yield returnType]
        | Fable.Tuple(genArgs,_)-> genericTypeInfo "tuple" genArgs
        | Fable.Option(genArg,_)-> genericTypeInfo "option" [genArg]
        | Fable.Array(genArg,_) -> genericTypeInfo "array" [genArg]
        | Fable.List genArg     -> genericTypeInfo "list" [genArg]
        | Fable.Regex           -> nonGenericTypeInfo Types.regex
        | Fable.MetaType        -> nonGenericTypeInfo Types.type_
        | Fable.AnonymousRecordType(fieldNames, genArgs, _isStruct) ->
            let genArgs = resolveGenerics genArgs
            List.zip (fieldNames |> Array.toList) genArgs
            |> List.map (fun (k, t) -> Expression.arrayExpression[|Expression.stringLiteral(k); t|])
            |> libReflectionCall com ctx None "anonRecord"
        | Fable.DeclaredType(entRef, genArgs) ->
            let fullName = entRef.FullName
            match fullName, genArgs with
            | Replacements.Util.BuiltinEntity kind ->
                match kind with
                | Replacements.Util.BclGuid
                | Replacements.Util.BclTimeSpan
                | Replacements.Util.BclDateTime
                | Replacements.Util.BclDateTimeOffset
                | Replacements.Util.BclDateOnly
                | Replacements.Util.BclTimeOnly
                | Replacements.Util.BclTimer -> genericEntity fullName [||]
                | Replacements.Util.BclHashSet gen
                | Replacements.Util.FSharpSet gen ->
                    genericEntity fullName [|transformTypeInfo com ctx r genMap gen|]
                | Replacements.Util.BclDictionary(key, value)
                | Replacements.Util.BclKeyValuePair(key, value)
                | Replacements.Util.FSharpMap(key, value) ->
                    genericEntity fullName [|
                        transformTypeInfo com ctx r genMap key
                        transformTypeInfo com ctx r genMap value
                    |]
                | Replacements.Util.FSharpResult(ok, err) ->
                    let ent = com.GetEntity(entRef)
                    transformUnionReflectionInfo com ctx r ent [|
                        transformTypeInfo com ctx r genMap ok
                        transformTypeInfo com ctx r genMap err
                    |]
                | Replacements.Util.FSharpChoice gen ->
                    let ent = com.GetEntity(entRef)
                    let gen = List.map (transformTypeInfo com ctx r genMap) gen
                    List.toArray gen |> transformUnionReflectionInfo com ctx r ent
                | Replacements.Util.FSharpReference gen ->
                    let ent = com.GetEntity(entRef)
                    [|transformTypeInfo com ctx r genMap gen|]
                    |> transformRecordReflectionInfo com ctx r ent
            | _ ->
                let ent = com.GetEntity(entRef)
                let generics = genArgs |> List.map (transformTypeInfo com ctx r genMap) |> List.toArray
                // Check if the entity is actually declared in JS code
                // TODO: Interfaces should be declared when generating Typescript
                if FSharp2Fable.Util.isGlobalOrImportedEntity ent then
                    genericGlobalOrImportedEntity generics ent
                elif ent.IsInterface
                    || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    || FSharp2Fable.Util.isReplacementCandidate entRef then
                    genericEntity ent.FullName generics
                // TODO: Strictly measure types shouldn't appear in the runtime, but we support it for now
                // See Fable.Transforms.FSharp2Fable.TypeHelpers.makeTypeGenArgs
                elif ent.IsMeasure then
                    [Expression.stringLiteral(ent.FullName)]
                    |> libReflectionCall com ctx None "measure"
                else
                    let reflectionMethodExpr = FSharp2Fable.Util.entityIdentWithSuffix com entRef Naming.reflectionSuffix
                    let callee = com.TransformAsExpr(ctx, reflectionMethodExpr)
                    Expression.callExpression(callee, generics)

    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName
            [
                yield Expression.stringLiteral(fullname)
                match generics with
                | [||] -> yield Util.undefined None
                | generics -> yield Expression.arrayExpression(generics)
                match tryJsConstructorForAnnotation false com ctx ent with
                | Some cons -> yield cons
                | None -> ()
                match ent.BaseType with
                | Some d ->
                    let genMap =
                        Seq.zip ent.GenericParameters generics
                        |> Seq.map (fun (p, e) -> p.Name, e)
                        |> Map
                        |> Some
                    yield Fable.DeclaredType(d.Entity, d.GenericArgs)
                          |> transformTypeInfo com ctx r genMap
                | None -> ()
            ]
            |> libReflectionCall com ctx r "class"

    let private ofString s = Expression.stringLiteral(s)
    let private ofArray babelExprs = Expression.arrayExpression(List.toArray babelExprs)

    let transformTypeTest (com: IBabelCompiler) ctx range expr (typ: Fable.Type): Expression =
        let warnAndEvalToFalse msg =
            "Cannot type test (evals to false): " + msg
            |> addWarning com [] range
            Expression.booleanLiteral(false)

        let jsTypeof (primitiveType: string) (Util.TransformExpr com ctx expr): Expression =
            let typeof = UnaryExpression(expr, "typeof", None)
            Expression.binaryExpression(BinaryEqual, typeof, Expression.stringLiteral(primitiveType), ?loc=range)

        let jsInstanceof consExpr (Util.TransformExpr com ctx expr): Expression =
            BinaryExpression(expr, consExpr, "instanceof", range)

        match typ with
        | Fable.Measure _ // Dummy, shouldn't be possible to test against a measure type
        | Fable.Any -> Expression.booleanLiteral(true)
        | Fable.Unit -> com.TransformAsExpr(ctx, expr) |> Util.makeNullCheck range true
        | Fable.Boolean -> jsTypeof "boolean" expr
        | Fable.Char | Fable.String _ -> jsTypeof "string" expr
        | Fable.Number(Decimal,_) -> jsInstanceof (libValue com ctx "Decimal" "default") expr
        | Fable.Number(BigInt,_) -> jsInstanceof (libValue com ctx "BigInt/z" "BigInteger") expr
        | Fable.Number((Int64|UInt64),_) -> jsInstanceof (libValue com ctx "Long" "default") expr
        | Fable.Number _ -> jsTypeof "number" expr
        | Fable.Regex -> jsInstanceof (Expression.identifier("RegExp")) expr
        | Fable.LambdaType _ | Fable.DelegateType _ -> jsTypeof "function" expr
        | Fable.Array _ | Fable.Tuple _ ->
            libCall com ctx None "Util" "isArrayLike" [] [com.TransformAsExpr(ctx, expr)]
        | Fable.List _ ->
            jsInstanceof (libValue com ctx "List" "FSharpList") expr
        | Fable.AnonymousRecordType _ ->
            warnAndEvalToFalse "anonymous records"
        | Fable.MetaType ->
            jsInstanceof (libValue com ctx "Reflection" "TypeInfo") expr
        | Fable.Option _ -> warnAndEvalToFalse "options" // TODO
        | Fable.GenericParam _ -> warnAndEvalToFalse "generic parameters"
        | Fable.DeclaredType (ent, genArgs) ->
            match ent.FullName with
            | Types.idisposable ->
                match expr with
                | MaybeCasted(ExprType(Fable.DeclaredType (ent2, _)))
                        when com.GetEntity(ent2) |> FSharp2Fable.Util.hasInterface Types.idisposable ->
                    Expression.booleanLiteral(true)
                | _ ->
                    [com.TransformAsExpr(ctx, expr)]
                    |> libCall com ctx None "Util" "isDisposable" []
            | Types.ienumerable ->
                [com.TransformAsExpr(ctx, expr)]
                |> libCall com ctx None "Util" "isIterable" []
            | Types.array ->
                [com.TransformAsExpr(ctx, expr)]
                |> libCall com ctx None "Util" "isArrayLike" []
            | Types.exception_ ->
                [com.TransformAsExpr(ctx, expr)]
                |> libCall com ctx None "Types" "isException" []
            | _ ->
                let ent = com.GetEntity(ent)
                if ent.IsInterface then
                    match FSharp2Fable.Util.tryGlobalOrImportedEntity com ent with
                    | Some typeExpr ->
                        let typeExpr = com.TransformAsExpr(ctx, typeExpr)
                        jsInstanceof typeExpr expr
                    | None -> warnAndEvalToFalse "interfaces"
                else
                    match tryJsConstructorForAnnotation false com ctx ent with
                    | Some cons ->
                        if not(List.isEmpty genArgs) then
                            com.WarnOnlyOnce("Generic args are ignored in type testing", ?range=range)
                        jsInstanceof cons expr
                    | None ->
                        warnAndEvalToFalse ent.FullName

module Annotation =

    let isByRefOrAnyType (com: IBabelCompiler) = function
        | Replacements.Util.IsByRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let isInRefOrAnyType (com: IBabelCompiler) = function
        | Replacements.Util.IsInRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let makeTypeParamDecl (_com: IBabelCompiler) (ctx: Context) genArgs =
        if ctx.IsParamType then [||]
        else
            // TODO: Keep inheritance constraint
            // Maybe there's a way to represent measurements in TypeScript
            genArgs |> List.chooseToArray (function
                | Fable.GenericParam(name, isMeasure, _constraints) when not isMeasure ->
                    TypeParameter.typeParameter(name) |> Some
                | _ -> None)

    let makeTypeParamInstantiation (com: IBabelCompiler) ctx genArgs =
        genArgs |> List.chooseToArray (function
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                if ent.IsMeasure then None
                else makeEntityTypeAnnotation com ctx ent genArgs |> Some
            | t -> makeTypeAnnotation com ctx t |> Some)

    let makeTypeParamInstantiationIfTypeScript (com: IBabelCompiler) ctx genArgs =
        if com.Options.Language = TypeScript then
            makeTypeParamInstantiation com ctx genArgs |> Some
        else None

    let getGenericTypeAnnotation com ctx name genArgs =
        let typeParamInst = makeTypeParamInstantiation com ctx genArgs
        TypeAnnotation.aliasTypeAnnotation(Identifier.identifier(name), typeParameters=typeParamInst)

    let makeTypeAnnotation com ctx typ: TypeAnnotation =
        match typ with
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any -> AnyTypeAnnotation
        | Fable.Unit -> VoidTypeAnnotation
        | Fable.Boolean -> BooleanTypeAnnotation
        | Fable.Char -> StringTypeAnnotation
        | Fable.String -> StringTypeAnnotation
        | Fable.Regex -> makeSimpleTypeAnnotation com ctx "RegExp"
        | Fable.Number(Int64,_) -> makeImportTypeAnnotation com ctx [] "Long" "int64"
        | Fable.Number(UInt64,_) -> makeImportTypeAnnotation com ctx [] "Long" "uint64"
        | Fable.Number(Decimal,_) -> makeImportTypeAnnotation com ctx [] "Decimal" "decimal"
        | Fable.Number(BigInt,_) -> makeImportTypeAnnotation com ctx [] "BigInt/z" "BigInteger"
        | Fable.Number(kind,_) -> makeNumericTypeAnnotation com ctx kind
        | Fable.Option(genArg,_) -> makeOptionTypeAnnotation com ctx genArg
        | Fable.Tuple(genArgs,_) -> makeTupleTypeAnnotation com ctx genArgs
        | Fable.Array(genArg, kind) -> makeArrayTypeAnnotation com ctx genArg kind
        | Fable.List genArg -> makeListTypeAnnotation com ctx genArg
        | Fable.GenericParam(name=name) -> makeSimpleTypeAnnotation com ctx name
        | Fable.LambdaType(argType, returnType) ->
            ([argType], returnType)
            ||> FableTransforms.uncurryLambdaType
            ||> makeFunctionTypeAnnotation com ctx typ
        | Fable.DelegateType(argTypes, returnType) ->
            makeFunctionTypeAnnotation com ctx typ argTypes returnType
        | Fable.AnonymousRecordType(fieldNames, genArgs, _isStruct) ->
            makeAnonymousRecordTypeAnnotation com ctx fieldNames genArgs
        // TODO: Maybe we don't need this as makeEntityTypeAnnotation also checks for built-ins through JS.Replacements.tryJsConstructor
        | Replacements.Util.Builtin kind ->
            makeBuiltinTypeAnnotation com ctx typ kind
        | Fable.DeclaredType(entRef, genArgs) ->
            let ent = com.GetEntity(entRef)
            makeEntityTypeAnnotation com ctx ent genArgs

    let makeSimpleTypeAnnotation _com _ctx name =
        TypeAnnotation.aliasTypeAnnotation(Identifier.identifier(name))

    let makeGenericTypeAnnotation com ctx genArgs id =
        let typeParamInst = makeTypeParamInstantiation com ctx genArgs
        TypeAnnotation.aliasTypeAnnotation(id, typeParameters=typeParamInst)

    let makeNativeTypeAnnotation com ctx genArgs typeName =
        Identifier.identifier(typeName)
        |> makeGenericTypeAnnotation com ctx genArgs

    let makeImportTypeId (com: IBabelCompiler) ctx moduleName typeName =
        let expr = com.GetImportExpr(ctx, typeName, getLibPath com moduleName, None)
        match expr with
        | Expression.Identifier(id) -> id
        | _ -> Identifier.identifier(typeName)

    let makeImportTypeAnnotation com ctx genArgs moduleName typeName =
        let id = makeImportTypeId com ctx moduleName typeName
        makeGenericTypeAnnotation com ctx genArgs id

    let makeNumericTypeAnnotation com ctx kind =
        let typeName = getNumberKindName kind
        makeImportTypeAnnotation com ctx [] "Int32" typeName

    let makeNullableTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Option" "Nullable"

    let makeOptionTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Option" "Option"

    let makeTupleTypeAnnotation com ctx genArgs =
        List.map (makeTypeAnnotation com ctx) genArgs
        |> List.toArray |> TupleTypeAnnotation

    let makeArrayTypeAnnotation com ctx genArg kind =
        match genArg with
        | JS.Replacements.TypedArrayCompatible com kind name ->
            makeSimpleTypeAnnotation com ctx name
        | _ ->
            // makeNativeTypeAnnotation com ctx [genArg] "Array"
            makeTypeAnnotation com ctx genArg |> ArrayTypeAnnotation

    let makeListTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "List" "FSharpList"

    let makeUnionTypeAnnotation com ctx genArgs =
        List.map (makeTypeAnnotation com ctx) genArgs
        |> List.toArray |> UnionTypeAnnotation

    let makeBuiltinTypeAnnotation com ctx typ kind =
        match kind with
        | Replacements.Util.BclGuid -> StringTypeAnnotation
        | Replacements.Util.BclTimeSpan -> NumberTypeAnnotation
        | Replacements.Util.BclDateTime -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.Util.BclDateTimeOffset -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.Util.BclDateOnly -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.Util.BclTimeOnly -> NumberTypeAnnotation
        | Replacements.Util.BclTimer -> makeImportTypeAnnotation com ctx [] "Timer" "Timer"
        | Replacements.Util.BclHashSet key -> makeImportTypeAnnotation com ctx [key] "Util" "ISet"
        | Replacements.Util.BclDictionary (key, value) -> makeImportTypeAnnotation com ctx [key; value] "Util" "IMap"
        | Replacements.Util.BclKeyValuePair (key, value) -> makeTupleTypeAnnotation com ctx [key; value]
        | Replacements.Util.FSharpSet key -> makeImportTypeAnnotation com ctx [key] "Set" "FSharpSet"
        | Replacements.Util.FSharpMap (key, value) -> makeImportTypeAnnotation com ctx [key; value] "Map" "FSharpMap"
        | Replacements.Util.FSharpResult (ok, err) -> makeImportTypeAnnotation com ctx [ok; err] "Choice" "FSharpResult$2"
        | Replacements.Util.FSharpChoice genArgs ->
            $"FSharpChoice${List.length genArgs}"
            |> makeImportTypeAnnotation com ctx genArgs "Choicee"
        | Replacements.Util.FSharpReference genArg ->
            if isInRefOrAnyType com typ
            then makeTypeAnnotation com ctx genArg
            else makeImportTypeAnnotation com ctx [genArg] "Types" "FSharpRef"

    let makeFunctionTypeAnnotation com ctx _typ argTypes returnType =
        let funcTypeParams =
            match argTypes with
            | [Fable.Unit] -> []
            | _ -> argTypes
            |> List.mapi (fun i argType ->
                FunctionTypeParam.functionTypeParam(
                    Identifier.identifier("arg" + (string i)),
                    makeTypeAnnotation com ctx argType))
            |> List.toArray
        let ctx = { ctx with IsParamType = true };
        let genParams = Util.getGenericTypeParams ctx (argTypes @ [returnType])
        let returnType = makeTypeAnnotation com ctx returnType
        let typeParamDecl = makeTypeParamDecl com ctx genParams
        TypeAnnotation.functionTypeAnnotation(funcTypeParams, returnType, typeParameters=typeParamDecl)

    let makeInterfaceTypeAnnotation com ctx (ent: Fable.Entity) genArgs =
        match ent.FullName with
        | Types.icollection
            -> makeNativeTypeAnnotation com ctx genArgs "Iterable"
            // -> makeImportTypeAnnotation com ctx [Fable.Any] "Util" "ICollection"
        | Types.icollectionGeneric
            -> makeNativeTypeAnnotation com ctx genArgs "Iterable"
            // -> makeImportTypeAnnotation com ctx genArgs "Util" "ICollection"
        // | Types.idictionary
        // | Types.ireadonlydictionary
        | Types.idisposable
            -> makeImportTypeAnnotation com ctx genArgs "Util" "IDisposable"
        | Types.ienumerable
            -> makeNativeTypeAnnotation com ctx [Fable.Any] "Iterable"
            // -> makeImportTypeAnnotation com ctx [Fable.Any] "Util" "IEnumerable"
        | Types.ienumerableGeneric
            -> makeNativeTypeAnnotation com ctx genArgs "Iterable"
            // -> makeImportTypeAnnotation com ctx genArgs "Util" "IEnumerable"
        | Types.ienumerator
            -> makeImportTypeAnnotation com ctx [Fable.Any] "Util" "IEnumerator"
        | Types.ienumeratorGeneric
            -> makeImportTypeAnnotation com ctx genArgs "Util" "IEnumerator"
        | Types.icomparable
            -> makeImportTypeAnnotation com ctx [Fable.Any] "Util" "IComparable"
        | Types.icomparableGeneric
        | Types.iStructuralComparable
            -> makeImportTypeAnnotation com ctx genArgs "Util" "IComparable"
        | Types.iequatableGeneric
        | Types.iStructuralEquatable
            -> makeImportTypeAnnotation com ctx genArgs "Util" "IEquatable"
        | Types.icomparer
            -> makeImportTypeAnnotation com ctx [Fable.Any] "Util" "IComparer"
        | Types.icomparerGeneric
            -> makeImportTypeAnnotation com ctx genArgs "Util" "IComparer"
        | Types.iequalityComparerGeneric
            -> makeImportTypeAnnotation com ctx genArgs "Util" "IEqualityComparer"
        | _ ->
            // TODO: add more interfaces
            AnyTypeAnnotation

    let makeEntityTypeAnnotation com ctx (ent: Fable.Entity) genArgs =
        match ent.FullName, genArgs with
        | "System.Nullable`1", [genArg] ->
            makeNullableTypeAnnotation com ctx genArg
        | _ ->
            if ent.IsInterface then
                makeInterfaceTypeAnnotation com ctx ent genArgs
            else
                match Lib.tryJsConstructorForAnnotation true com ctx ent with
                | Some entRef ->
                    match entRef with
                    | Literal(Literal.StringLiteral(StringLiteral(str, _))) ->
                        match str with
                        | "number" -> NumberTypeAnnotation
                        | "boolean" -> BooleanTypeAnnotation
                        | "string" -> StringTypeAnnotation
                        | _ -> AnyTypeAnnotation
                    | Expression.Identifier(id) ->
                        makeGenericTypeAnnotation com ctx genArgs id
                    // TODO: Resolve references to types in nested modules
                    | _ -> AnyTypeAnnotation
                | None -> AnyTypeAnnotation

    let makeAnonymousRecordTypeAnnotation _com _ctx _fieldNames _genArgs =
         AnyTypeAnnotation // TODO:

    let typedIdentWith (com: IBabelCompiler) ctx r typ name =
        let ta =
            if com.Options.Language = TypeScript then
                makeTypeAnnotation com ctx typ |> Some
            else None
        Identifier.identifier(name, ?typeAnnotation=ta, ?loc=r)

    let typedIdent (com: IBabelCompiler) ctx (id: Fable.Ident) =
        typedIdentWith com ctx id.Range id.Type id.Name

    let transformFunctionWithAnnotations (com: IBabelCompiler) ctx name typeParams (args: Fable.Ident list) (body: Fable.Expr) =
        if com.Options.Language = TypeScript then
            let argTypes = args |> List.map (fun id -> id.Type)
            let genParams = typeParams |> Option.defaultWith (fun () ->
                Util.getGenericTypeParams ctx (argTypes @ [body.Type]))
            let args', body' = com.TransformFunction(ctx, name, args, body)
            let returnType = makeTypeAnnotation com ctx body.Type
            let typeParamDecl = makeTypeParamDecl com ctx genParams
            args', body', Some returnType, Some typeParamDecl
        else
            let args', body' = com.TransformFunction(ctx, name, args, body)
            args', body', None, None

module Util =
    open Lib
    open Reflection
    open Annotation

    let IMPORT_REGEX = Regex("""^import\b\s*(\{?.*?\}?)\s*\bfrom\s+["'](.*?)["'](?:\s*;)?$""")
    let IMPORT_SELECTOR_REGEX = Regex(@"^(\*|\w+)(?:\s+as\s+(\w+))?$")

    let stripImports (com: IBabelCompiler) ctx r (str: string) =
        str.Split('\n')
        |> Array.skipWhile (fun line ->
            match line.Trim() with
            | "" -> true
            | Naming.Regex IMPORT_REGEX (_::selector::path::_) ->
                if selector.StartsWith("{") then
                    for selector in selector.TrimStart('{').TrimEnd('}').Split(',') do
                        com.GetImportExpr(ctx, selector, path, r, noMangle=true) |> ignore
                    true
                else
                    let selector =
                        if selector.StartsWith("*") then selector
                        else $"default as {selector}"
                    com.GetImportExpr(ctx, selector, path, r, noMangle=true) |> ignore
                    true
            | _ -> false)
        |> String.concat "\n"

    let (|TransformExpr|) (com: IBabelCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, _) -> Some([arg], body)
        | Fable.Delegate(args, body, _, []) -> Some(args, body)
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
        let argIds = args |> discardUnitArg |> List.map (fun arg ->
            getUniqueNameInDeclarationScope ctx (arg.Name + "_mut"))
        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds
            member _.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id -> name = id.Name | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwith $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let rec isJsStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Unresolved _
        | Fable.Value _ | Fable.Import _  | Fable.IdentExpr _
        | Fable.Lambda _ | Fable.Delegate _ | Fable.ObjectExpr _
        | Fable.Call _ | Fable.CurriedApply _ | Fable.Operation _
        | Fable.Get _ | Fable.Test _ -> false

        | Fable.TypeCast(e,_) -> isJsStatement ctx preferStatement e

        | Fable.TryCatch _
        | Fable.Sequential _ | Fable.Let _ | Fable.LetRec _ | Fable.Set _
        | Fable.ForLoop _ | Fable.WhileLoop _ -> true

        | Fable.Extended(kind, _) ->
            match kind with
            | Fable.Throw _ | Fable.Debugger -> true
            | Fable.Curry _ -> false

        // TODO: If IsJsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i,_,_) -> i.IsStatement

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
        Expression.nullLiteral()

    let identAsIdent (id: Fable.Ident) =
        Identifier.identifier(id.Name, ?loc=id.Range)

    let identAsExpr (id: Fable.Ident) =
        Expression.identifier(id.Name, ?loc=id.Range)

    let identAsPattern (id: Fable.Ident) =
        Pattern.identifier(id.Name, ?loc=id.Range)

    let typedIdentAsExpr com ctx (id: Fable.Ident) =
        typedIdent com ctx id |> Expression.Identifier

    let typedIdentAsPattern com ctx (id: Fable.Ident) =
        Pattern.Identifier(typedIdent com ctx id, None)

    let thisExpr =
        Expression.thisExpression()

    let ofInt i =
        Expression.numericLiteral(float i)

    let ofString s =
       Expression.stringLiteral(s)

    let memberFromNameComputeStrings computeStrings (memberName: string): Expression * bool =
        match memberName with
        | "ToString" -> Expression.identifier("toString"), false
        | n when n.StartsWith("Symbol.") ->
            Expression.memberExpression(Expression.identifier("Symbol"), Expression.identifier(n[7..]), false), true
        | n when Naming.hasIdentForbiddenChars n ->
            Expression.stringLiteral(n), computeStrings
        | n -> Expression.identifier(n), false

    let memberFromName (memberName: string): Expression * bool =
        memberFromNameComputeStrings false memberName

    let get r left memberName =
        let expr, computed = memberFromNameComputeStrings true memberName
        Expression.memberExpression(left, expr, computed, ?loc=r)

    let getExpr r (object: Expression) (expr: Expression) =
        let expr, computed =
            match expr with
            | Literal(Literal.StringLiteral(StringLiteral(value, _))) -> memberFromNameComputeStrings true value
            | e -> e, true
        Expression.memberExpression(object, expr, computed, ?loc=r)

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

    // Use non-strict equality for null checks
    let makeNullCheck r isNull e =
        let op = if isNull then "==" else "!="
        BinaryExpression(e, Expression.nullLiteral(), op, r)

    let makeArray (com: IBabelCompiler) ctx exprs =
        List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
        |> Expression.arrayExpression

    let makeTypedArray (com: IBabelCompiler) ctx t kind (args: Fable.Expr list) =
        match t with
        | JS.Replacements.TypedArrayCompatible com kind jsName ->
            let args = [|makeArray com ctx args|]
            Expression.newExpression(Expression.identifier(jsName), args)
        | _ -> makeArray com ctx args

    let getArrayCons com t kind =
        match t with
        | JS.Replacements.TypedArrayCompatible com kind name -> Expression.identifier name
        | _ -> Expression.identifier("Array")

    let makeArrayAllocated (com: IBabelCompiler) ctx typ kind (size: Fable.Expr) =
        let cons = getArrayCons com typ kind
        let size = com.TransformAsExpr(ctx, size)
        Expression.newExpression(cons, [|size |])

    let makeArrayFrom (com: IBabelCompiler) ctx typ kind (fableExpr: Fable.Expr) =
        match fableExpr with
        | Replacements.Util.ArrayOrListLiteral(exprs, _) ->
            makeTypedArray com ctx typ kind exprs
        | _ ->
            let cons = getArrayCons com typ kind
            let expr = com.TransformAsExpr(ctx, fableExpr)
            Expression.callExpression(get None cons "from", [|expr|])

    let makeStringArray strings =
        strings
        |> List.mapToArray (fun x -> Expression.stringLiteral(x))
        |> Expression.arrayExpression

    let makeJsObject pairs =
        pairs |> Seq.map (fun (name, value) ->
            let prop, computed = memberFromName name
            ObjectMember.objectProperty(prop, value, computed_=computed))
        |> Seq.toArray
        |> Expression.objectExpression

    let assign range left right =
        Expression.assignmentExpression(AssignEqual, left, right, ?loc=range)

    /// Immediately Invoked Function Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction(ctx, None, [], expr)
        // Use an arrow function in case we need to capture `this`
        Expression.callExpression(Expression.arrowFunctionExpression([||], body), [||])

    let multiVarDeclaration kind (variables: (Identifier * Expression option) list) =
        let varDeclarators =
            // TODO: Log error if there're duplicated non-empty var declarations
            variables
            |> List.distinctBy (fun (Identifier(name=name), _value) -> name)
            |> List.mapToArray (fun (id, value) ->
                VariableDeclarator(Pattern.Identifier(id, None), value))
        Statement.variableDeclaration(kind, varDeclarators)

    let varDeclaration (var: Pattern) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration.variableDeclaration(var, value, kind)

    let restElement (var: Pattern) =
        Pattern.restElement(var)

    let callSuper (args: Expression list) =
        Expression.callExpression(Super(None), List.toArray args)

    let callSuperAsStatement (args: Expression list) =
        ExpressionStatement(callSuper args)

    let makeClassConstructor args body =
        ClassMember.classMethod(ClassPrimaryConstructor, Expression.identifier("constructor"), args, body)

    let callFunction com ctx r funcExpr genArgs (args: Expression list) =
        let genArgs = makeTypeParamInstantiationIfTypeScript com ctx genArgs
        Expression.callExpression(funcExpr, List.toArray args, ?typeParameters=genArgs, ?loc=r)

    let callFunctionWithThisContext r funcExpr (args: Expression list) =
        let args = thisExpr::args |> List.toArray
        Expression.callExpression(get None funcExpr "call", args, ?loc=r)

    let emitExpression range (txt: string) args =
        EmitExpression (txt, List.toArray args, ?loc=range)

    let undefined range =
//        Undefined(?loc=range) :> Expression
        UnaryExpression(Expression.numericLiteral(0.), "void", range)

    // TODO: See Fable2Dart for alternative gen param resolution
    let getGenericTypeParams (ctx: Context) (types: Fable.Type list) =
        let rec getGenParams = function
            | Fable.GenericParam (_, false, _) as p -> [p]
            | t -> t.Generics |> List.collect getGenParams
        let mutable dedupSet = ctx.ScopedTypeParams
        types
        |> List.collect getGenParams
        |> List.filter (function
            | Fable.GenericParam(name=name) ->
                if Set.contains name dedupSet then false
                else dedupSet <- Set.add name dedupSet; true
            | _ -> false)

    type MemberKind =
        | ClassConstructor
        | NonAttached of funcName: string
        | Attached of isStatic: bool

    let getMemberArgsAndBody (com: IBabelCompiler) ctx kind (info: Fable.MemberFunctionOrValue) (args: Fable.Ident list) (body: Fable.Expr) =
        let funcName, args, body =
            match kind, args with
            | Attached(isStatic=false), (thisArg::args) ->
                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if isIdentUsed thisArg.Name body then
                        let thisKeyword = Fable.IdentExpr { thisArg with Name = "this" }
                        Fable.Let(thisArg, thisKeyword, body)
                    else body
                None, args, body
            | Attached(isStatic=true), _
            | ClassConstructor, _ -> None, args, body
            | NonAttached funcName, _ -> Some funcName, args, body
            | _ -> None, args, body

        let typeParams =
            if com.Options.Language = TypeScript then
                let genParams =
                    match info.DeclaringEntity with
                    | Some e ->
                        let e = com.GetEntity(e)
                        if not e.IsFSharpModule then e.GenericParameters @ info.GenericParameters
                        else info.GenericParameters
                    | None -> info.GenericParameters
                genParams |> List.map (fun g -> Fable.GenericParam(g.Name, g.IsMeasure, g.Constraints)) |> Some
            else None

        let args, body, returnType, typeParamDecl =
            transformFunctionWithAnnotations com ctx funcName typeParams args body

        let args =
            let len = Array.length args
            if not info.HasSpread || len = 0 then args
            else [|
                if len > 1 then
                    yield! args[..len-2]
                yield restElement args[len-1]
            |]

        args, body, returnType, typeParamDecl

    let getUnionCaseName (uci: Fable.UnionCase) =
        match uci.CompiledName with Some cname -> cname | None -> uci.Name

    let getUnionExprTag (com: IBabelCompiler) ctx r (fableExpr: Fable.Expr) =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        getExpr r expr (Expression.stringLiteral("tag"))

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | NewExpression(Expression.Identifier (Identifier("FSharpRef", _, _, _, _)), _, _, _), _
        | Literal(NumericLiteral(_)), _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number((Int8 | Int16 | Int32),_) ->
            Expression.binaryExpression(BinaryOrBitwise, e, Expression.numericLiteral(0.))
        | _ -> e

    let wrapExprInBlockWithReturn e =
        BlockStatement([| Statement.returnStatement(e)|])

    let makeArrowFunctionExpression _name (args, (body: BlockStatement), returnType, typeParamDecl): Expression =
        Expression.arrowFunctionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let makeFunctionExpression name (args, (body: Expression), returnType, typeParamDecl): Expression =
        let id = name |> Option.map Identifier.identifier
        let body = wrapExprInBlockWithReturn body
        Expression.functionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let optimizeTailCall (com: IBabelCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
        let rec checkCrossRefs tempVars allArgs = function
            | [] -> tempVars
            | (argId, _arg)::rest ->
                let found = allArgs |> List.exists (deepExists (function
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
        [|
            // First declare temp variables
            for (KeyValue(argId, tempVar)) in tempVars do
                yield varDeclaration (Pattern.identifier(tempVar)) false (Expression.identifier(argId)) |> Declaration.VariableDeclaration |> Declaration
            // Then assign argument expressions to the original argument identifiers
            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
            for (argId, arg) in zippedArgs do
                let arg = FableTransforms.replaceValues tempVarReplacements arg
                let arg = com.TransformAsExpr(ctx, arg)
                yield assign None (Expression.identifier(argId)) arg |> ExpressionStatement
            yield Statement.continueStatement(Identifier.identifier(tc.Label), ?loc=range)
        |]

    let transformImport (com: IBabelCompiler) ctx r (selector: string) (path: string) =
        let selector, parts =
            let parts = Array.toList(selector.Split('.'))
            parts.Head, parts.Tail
        com.GetImportExpr(ctx, selector, path, r)
        |> getParts parts

    let transformCast (com: IBabelCompiler) (ctx: Context) t e: Expression =
        match t with
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | Fable.DeclaredType(ent, [_]) ->
            match ent.FullName with
            | Types.ienumerableGeneric | Types.ienumerable ->
                match e with
                | ExprType Fable.String ->
                    // Convert to array to get 16-bit code units, see #1279
                    let e = JS.Replacements.stringToCharArray e
                    com.TransformAsExpr(ctx, e)
                | Replacements.Util.ArrayOrListLiteral(exprs, _) ->
                    makeArray com ctx exprs
                | _ -> com.TransformAsExpr(ctx, e)
            | _ -> com.TransformAsExpr(ctx, e)
        | Fable.Unit -> UnaryExpression(com.TransformAsExpr(ctx, e), "void", e.Range)
        | _ -> com.TransformAsExpr(ctx, e)

    let transformCurry (com: IBabelCompiler) (ctx: Context) expr arity: Expression =
        com.TransformAsExpr(ctx, Replacements.Api.curryExprAtRuntime com arity expr)

    let transformValue (com: IBabelCompiler) (ctx: Context) r value: Expression =
        match value with
        | Fable.BaseValue(None,_) -> Super(None)
        | Fable.BaseValue(Some boundIdent,_) -> identAsExpr boundIdent
        | Fable.ThisValue _ -> Expression.thisExpression()
        | Fable.TypeInfo(t, tags) ->
            if com.Options.NoReflection then addErrorAndReturnNull com r "Reflection is disabled"
            else
                let genMap = if List.contains "allow-generics" tags then None else Some Map.empty
                transformTypeInfo com ctx r genMap t
        | Fable.Null _t ->
            // if com.Options.Language = TypeScript
            //     let ta = makeTypeAnnotation com ctx t |> TypeAnnotation |> Some
            //     upcast Identifier("null", ?typeAnnotation=ta, ?loc=r)
            // else
                Expression.nullLiteral(?loc=r)
        | Fable.UnitConstant -> undefined r
        | Fable.BoolConstant x -> Expression.booleanLiteral(x, ?loc=r)
        | Fable.CharConstant x -> Expression.stringLiteral(string x, ?loc=r)
        | Fable.StringConstant x -> Expression.stringLiteral(x, ?loc=r)
        | Fable.StringTemplate(tag, parts, values) ->
            let tag = tag |> Option.map (fun e -> com.TransformAsExpr(ctx, e))
            let values = values |> List.mapToArray (fun e -> com.TransformAsExpr(ctx, e))
            StringTemplate(tag, List.toArray parts, values, r) |> Literal
        | Fable.NumberConstant (x, kind, _) ->
            match kind, x with
            | Decimal, (:? decimal as x) -> JS.Replacements.makeDecimal com r value.Type x |> transformAsExpr com ctx
            | Int64, (:? int64 as x) -> JS.Replacements.makeLongInt com r value.Type true (uint64 x) |> transformAsExpr com ctx
            | UInt64, (:? uint64 as x) -> JS.Replacements.makeLongInt com r value.Type false x |> transformAsExpr com ctx
            | _, (:? int8 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? uint8 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? char as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? int16 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? uint16 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? int32 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? uint32 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? float32 as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? float as x) -> Expression.numericLiteral(x, ?loc=r)
            // We don't really support pointers for JS compilation, but compile them as numbers for standalone self-compilation
            | _, (:? nativeint as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _, (:? unativeint as x) -> Expression.numericLiteral(float x, ?loc=r)
            | _ -> addErrorAndReturnNull com r $"Numeric literal is not supported: {x.GetType().FullName}"
        | Fable.RegexConstant (source, flags) -> Expression.regExpLiteral(source, flags, ?loc=r)
        | Fable.NewArray (newKind, typ, kind) ->
            match newKind with
            | Fable.ArrayValues values -> makeTypedArray com ctx typ kind values
            | Fable.ArrayAlloc size -> makeArrayAllocated com ctx typ kind size
            | Fable.ArrayFrom expr -> makeArrayFrom com ctx typ kind expr
        | Fable.NewTuple(vals,_) -> makeArray com ctx vals
        // | Fable.NewList (headAndTail, _) when List.contains "FABLE_LIBRARY" com.Options.Define ->
        //     makeList com ctx r headAndTail
        // Optimization for bundle size: compile list literals as List.ofArray
        | Fable.NewList (headAndTail, typ) ->
            let rec getItems acc = function
                | None -> List.rev acc, None
                | Some(head, Fable.Value(Fable.NewList(tail, _),_)) -> getItems (head::acc) tail
                | Some(head, tail) -> List.rev (head::acc), Some tail
            match getItems [] headAndTail with
            | [], None ->
                libCall com ctx r "List" "empty" [typ] []
            | [TransformExpr com ctx expr], None ->
                libCall com ctx r "List" "singleton" [] [expr]
            | exprs, None ->
                [makeArray com ctx exprs]
                |> libCall com ctx r "List" "ofArray" []
            | [TransformExpr com ctx head], Some(TransformExpr com ctx tail) ->
                libCall com ctx r "List" "cons" [] [head; tail]
            | exprs, Some(TransformExpr com ctx tail) ->
                [makeArray com ctx exprs; tail]
                |> libCall com ctx r "List" "ofArrayWithTail" []
        | Fable.NewOption (value, t, _) ->
            match value with
            | Some (TransformExpr com ctx e) ->
                if mustWrapOption t
                then libCall com ctx r "Option" "some" [] [e]
                else e
            | None -> undefined r
        | Fable.NewRecord(values, ent, genArgs) ->
            let ent = com.GetEntity(ent)
            let values = List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values
            let consRef = ent |> jsConstructor com ctx
            let typeParamInst =
                if com.Options.Language = TypeScript && (ent.FullName = Types.refCell)
                then makeTypeParamInstantiation com ctx genArgs |> Some
                else None
            Expression.newExpression(consRef, values, ?typeParameters=typeParamInst, ?loc=r)
        | Fable.NewAnonymousRecord(values, fieldNames, _genArgs, _isStruct) ->
            let values = List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values
            Array.zip fieldNames values |> makeJsObject
        | Fable.NewUnion(values, tag, ent, genArgs) ->
            let transformNewUnion (com: IBabelCompiler) (ctx: Context) r (ent: Fable.Entity) tag values =
                let consRef = jsConstructor com ctx ent
                let values = makeArray com ctx values
                Expression.newExpression(consRef, [|ofInt tag; values|], ?loc=r)
            let ent = com.GetEntity(ent)
            if com.Options.Language = TypeScript then
                let case = ent.UnionCases[tag].Name
                match tryJsConstructorWithSuffix com ctx ent ("_" + case) with
                | Some helperRef ->
                    let values = values |> List.mapToArray (transformAsExpr com ctx)
                    let typeParams = makeTypeParamInstantiation com ctx genArgs
                    Expression.callExpression(helperRef, values, typeParameters=typeParams)
                | None -> transformNewUnion com ctx r ent tag values
            else
                transformNewUnion com ctx r ent tag values

    let enumerableThisToIterator com ctx =
        let enumerator = Expression.callExpression(get None (Expression.identifier("this")) "GetEnumerator", [||])
        BlockStatement([| Statement.returnStatement(libCall com ctx None "Util" "toIterator" [] [enumerator])|])

    let extractBaseExprFromBaseCall (com: IBabelCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
        match baseCall, baseType with
        | Some (Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr =
                match baseRef, baseType with
                | Fable.IdentExpr id, Some d ->
                    let typ = Fable.DeclaredType(d.Entity, d.GenericArgs)
                    typedIdentAsExpr com ctx { id with Type = typ }
                | Fable.IdentExpr id, _ -> typedIdentAsExpr com ctx id
                | _ -> transformAsExpr com ctx baseRef
            let args = CallInfo(info, info.MemberRef |> Option.bind com.TryGetMember) |> transformCallArgs com ctx
            Some (baseExpr, args)
        | Some (Fable.Value _), Some baseType ->
            // let baseEnt = com.GetEntity(baseType.Entity)
            // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
            // let entityType = FSharp2Fable.Util.getEntityType baseEnt
            // let baseRefId = makeTypedIdent entityType entityName
            // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
            // Some (baseExpr, []) // default base constructor
            let range = baseCall |> Option.bind (fun x -> x.Range)
            $"Ignoring base call for %s{baseType.Entity.FullName}" |> addWarning com [] range
            None
        | Some _, _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)
            "Unexpected base call expression, please report" |> addError com [] range
            None
        | None, _ ->
            None

    let transformObjectExpr (com: IBabelCompiler) ctx (members: Fable.ObjectExprMember list) baseCall: Expression =

        let makeMethod kind prop computed (info: Fable.MemberFunctionOrValue) args body =
            let args, body, returnType, typeParamDecl =
                getMemberArgsAndBody com ctx (Attached(isStatic=false)) info args body
            ObjectMember.objectMethod(kind, prop, args, body, computed_=computed,
                ?returnType=returnType, ?typeParameters=typeParamDecl)

        let members = members |> List.map (fun memb -> memb, com.GetMember(memb.MemberRef))

        // Optimization: Object literals with getters and setters are very slow in V8
        // so use a class expression instead. See https://github.com/fable-compiler/Fable/pull/2165#issuecomment-695835444
        let compileAsClass = (Option.isSome baseCall, members) ||> List.fold (fun compileAsClass (memb, info) ->
            compileAsClass || (not memb.IsMangled && (info.IsSetter || (info.IsGetter && canHaveSideEffects memb.Body))))

        let members =
            members |> List.collect (fun (memb, info) ->
                let prop, computed = memberFromName memb.Name
                // If compileAsClass is false, it means getters don't have side effects
                // and can be compiled as object fields (see condition above)
                if not memb.IsMangled && (info.IsValue || (not compileAsClass && info.IsGetter)) then
                    [ObjectMember.objectProperty(prop, com.TransformAsExpr(ctx, memb.Body), computed_=computed)]
                elif not memb.IsMangled && info.IsGetter then
                    [makeMethod ObjectGetter prop computed info memb.Args memb.Body]
                elif not memb.IsMangled && info.IsSetter then
                    [makeMethod ObjectSetter prop computed info memb.Args memb.Body]
                elif info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" then
                    let method = makeMethod ObjectMeth prop computed info memb.Args memb.Body
                    let iterator =
                        let prop, computed = memberFromName "Symbol.iterator"
                        let body = enumerableThisToIterator com ctx
                        ObjectMember.objectMethod(ObjectMeth, prop, [||], body, computed_=computed)
                    [method; iterator]
                else
                    [makeMethod ObjectMeth prop computed info memb.Args memb.Body]
            )

        if not compileAsClass then
            Expression.objectExpression(List.toArray  members)
        else
            let classMembers =
                members |> List.choose (function
                    | ObjectProperty(key, value, computed) ->
                        ClassMember.classProperty(key, value, computed_=computed) |> Some
                    | ObjectMethod(kind, key, parameters, body, computed, returnType, typeParameters, _) ->
                        let kind =
                            match kind with
                            | "get" -> ClassGetter
                            | "set" -> ClassSetter
                            | _ -> ClassFunction
                        ClassMember.classMethod(kind, key, parameters, body, computed_=computed,
                            ?returnType=returnType, ?typeParameters=typeParameters) |> Some)

            let baseExpr, classMembers =
                baseCall
                |> extractBaseExprFromBaseCall com ctx None
                |> Option.map (fun (baseExpr, baseArgs) ->
                    let consBody = BlockStatement([|callSuperAsStatement baseArgs|])
                    let cons = makeClassConstructor [||]  consBody
                    Some baseExpr, cons::classMembers
                )
                |> Option.defaultValue (None, classMembers)

            let classExpr = Expression.classExpression(List.toArray classMembers, ?superClass=baseExpr)
            Expression.newExpression(classExpr, [||])

    let transformCallArgs (com: IBabelCompiler) ctx (info: ArgsInfo) =
        let paramsInfo, args =
            match info with
            | NoCallInfo args -> None, args
            | CallInfo(callInfo, memberInfo) ->
                let paramsInfo = Option.map getParamsInfo memberInfo
                paramsInfo, callInfo.Args

        let args, objArg =
            paramsInfo
            |> Option.map (splitNamedArgs args)
            |> function
            | None -> args, None
            | Some(args, []) -> args, None
            | Some(args, namedArgs) ->
                let objArg =
                    namedArgs
                    |> List.choose (fun (p, v) ->
                        match p.Name, v with
                        | Some k, Fable.Value(Fable.NewOption(value,_, _),_) ->
                            value |> Option.map (fun v -> k, v)
                        | Some k, v -> Some(k, v)
                        | None, _ -> None)
                    |> List.map (fun (k, v) -> k, com.TransformAsExpr(ctx, v))
                    |> makeJsObject
                args, Some objArg

        let hasSpread =
            paramsInfo
            |> Option.map (fun i -> i.HasSpread)
            |> Option.defaultValue false

        let args =
            match args with
            | []
            | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> []
            | args when hasSpread ->
                match List.rev args with
                | [] -> []
                | (Replacements.Util.ArrayOrListLiteral(spreadArgs,_))::rest ->
                    let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                    rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
                | last::rest ->
                    let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                    rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
            | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

        match objArg with
        | None -> args
        | Some objArg -> args @ [objArg]

    let resolveExpr t strategy babelExpr: Statement =
        match strategy with
        | None | Some ReturnUnit -> ExpressionStatement(babelExpr)
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return ->  Statement.returnStatement(wrapIntExpression t babelExpr)
        | Some(Assign left) -> ExpressionStatement(assign None left babelExpr)
        | Some(Target left) -> ExpressionStatement(assign None (left |> Expression.Identifier) babelExpr)

    let transformOperation com ctx range opKind: Expression =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            Expression.unaryExpression(op, expr, ?loc=range)

        | Fable.Binary(op, left, right) ->
            match op, left, right with
            | (BinaryEqual | BinaryUnequal), Fable.Value(Fable.Null _, _), e
            | (BinaryEqual | BinaryUnequal), e,  Fable.Value(Fable.Null _, _) ->
                com.TransformAsExpr(ctx, e) |> makeNullCheck range (op = BinaryEqual)

            | _, TransformExpr com ctx left, TransformExpr com ctx right ->
                Expression.binaryExpression(op, left, right, ?loc=range)

        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            Expression.logicalExpression(left, op, right, ?loc=range)

    let transformEmit (com: IBabelCompiler) ctx range (info: Fable.EmitInfo) =
        let macro = stripImports com ctx range info.Macro
        let info = info.CallInfo
        let thisArg = info.ThisArg |> Option.map (fun e -> com.TransformAsExpr(ctx, e)) |> Option.toList
        CallInfo(info, info.MemberRef |> Option.bind com.TryGetMember)
        |> transformCallArgs com ctx
        |> List.append thisArg
        |> emitExpression range macro

    let transformJsxProps (com: IBabelCompiler) props =
        (Some([], []), props) ||> List.fold (fun propsAndChildren prop ->
            match propsAndChildren, prop with
            | None, _ -> None
            | Some(props, children), Fable.Value(Fable.NewTuple([StringConst key; value],_),_) ->
                if key = "children" then
                    match value with
                    | Replacements.Util.ArrayOrListLiteral(children, _) -> Some(props, children)
                    | value -> Some(props, [value])
                else
                    Some((key, value)::props, children)
            | Some _, e ->
                addError com [] e.Range "Cannot detect JSX prop key at compile time"
                None)

    let transformJsxEl (com: IBabelCompiler) ctx componentOrTag props =
        match transformJsxProps com props with
        | None -> Expression.nullLiteral()
        | Some(props, children) ->
            let componentOrTag = transformAsExpr com ctx componentOrTag
            let children =
                children
                |> List.map (transformAsExpr com ctx)
                |> function
                    // Because of call optimizations, it may happen a list has been transformed to an array in JS
                    | [ArrayExpression(children, _)] -> Array.toList children
                    | children -> children
            let props = props |> List.rev |> List.map (fun (k, v) -> k, transformAsExpr com ctx v)
            Expression.jsxElement(componentOrTag, props, children)

    let transformJsxCall (com: IBabelCompiler) ctx callee (args: Fable.Expr list) (info: Fable.MemberFunctionOrValue) =
        let names = info.CurriedParameterGroups |> List.concat |> List.choose (fun p -> p.Name)
        let props =
            List.zipSafe names args
            |> List.map (fun (key, value) ->
                Fable.Value(Fable.NewTuple([Fable.Value(Fable.StringConstant key, None); value], false), None))
        transformJsxEl com ctx callee props

    let transformCall (com: IBabelCompiler) ctx range typ callee (callInfo: Fable.CallInfo) =
        // Try to optimize some patterns after FableTransforms
        let optimized =
            match callInfo.Tags, callInfo.Args with
            | Fable.Tags.Contains "array", [maybeList] ->
                match maybeList with
                | Replacements.Util.ArrayOrListLiteral(vals,_) ->
                    Fable.Value(Fable.NewArray(Fable.ArrayValues vals, Fable.Any, Fable.MutableArray), range)
                    |> transformAsExpr com ctx
                    |> Some
                | Fable.Call(Fable.Import({Selector = "toList"; Path = Naming.EndsWith "/Seq.js" _; Kind = Fable.LibraryImport _},_,_), callInfo, _,_) ->
                    List.head callInfo.Args
                    |> Replacements.Util.toArray range typ
                    |> transformAsExpr com ctx
                    |> Some
                | _ -> None
            | Fable.Tags.Contains "pojo", keyValueList::caseRule::_ ->
                JS.Replacements.makePojo com (Some caseRule) keyValueList
                |> Option.map (transformAsExpr com ctx)
            | Fable.Tags.Contains "pojo", keyValueList::_ ->
                JS.Replacements.makePojo com None keyValueList
                |> Option.map (transformAsExpr com ctx)
            | Fable.Tags.Contains "jsx", componentOrTag::Replacements.Util.ArrayOrListLiteral(props, _)::_ ->
                transformJsxEl com ctx componentOrTag props |> Some
            | Fable.Tags.Contains "jsx", _ ->
                "Expecting a static list or array literal (no generator) for JSX props"
                |> addErrorAndReturnNull com range |> Some
            | Fable.Tags.Contains "jsx-template", args ->
                match args with
                | StringConst template ::_ ->
                    let template = stripImports com ctx range template
                    Expression.jsxTemplate(template) |> Some
                | MaybeCasted(Fable.Value(Fable.StringTemplate(_, parts, values), _))::_ ->
                    let parts =
                        match parts with
                        | head::parts -> (stripImports com ctx range head)::parts
                        | parts -> parts
                    let values = values |> List.mapToArray (transformAsExpr com ctx)
                    Expression.jsxTemplate(List.toArray parts, values) |> Some
                | _ ->
                    "Expecting a string literal or interpolation without formatting"
                    |> addErrorAndReturnNull com range |> Some
            | _ -> None

        match optimized with
        | Some e -> e
        | None ->
            callInfo.MemberRef
            |> Option.bind com.TryGetMember
            |> function
            | Some memberInfo when hasAttribute Atts.jsxComponent memberInfo.Attributes ->
                transformJsxCall com ctx callee callInfo.Args memberInfo
            | memberInfo ->
                let callee = com.TransformAsExpr(ctx, callee)
                let args = CallInfo(callInfo, memberInfo) |> transformCallArgs com ctx
                match callInfo.ThisArg with
                | None when List.contains "new" callInfo.Tags ->
                    let typeParamInst =
                        match typ with
                        | Fable.DeclaredType(_entRef, genArgs) -> makeTypeParamInstantiationIfTypeScript com ctx genArgs
                        | _ -> None
                    Expression.newExpression(callee, List.toArray args, ?typeParameters=typeParamInst, ?loc=range)
                | None -> callFunction com ctx range callee callInfo.GenericArgs args
                | Some(TransformExpr com ctx thisArg) -> callFunction com ctx range callee callInfo.GenericArgs (thisArg::args)

    let transformCurriedApply com ctx range (TransformExpr com ctx applied) args =
        match transformCallArgs com ctx (NoCallInfo args) with
        | [] -> callFunction com ctx range applied [] []
        | args -> (applied, args) ||> List.fold (fun e arg -> callFunction com ctx range e [] [arg])

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
            [|transformCall com ctx range t callee callInfo |> resolveExpr t returnStrategy|]

    let transformCurriedApplyAsStatements com ctx range t returnStrategy callee args =
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return|ReturnUnit), Some tc when tc.IsRecursiveRef(callee)
                                            && List.sameLength args tc.Args ->
            optimizeTailCall com ctx range tc args
        | _ ->
            [|transformCurriedApply com ctx range callee args |> resolveExpr t returnStrategy|]

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr: BlockStatement =
        com.TransformAsStatements(ctx, ret, expr) |> BlockStatement

    let transformTryCatch com ctx r returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                let e: Fable.Ident = { param with Type = Fable.Any } // intentionally set catch type to 'any'
                CatchClause.catchClause(typedIdentAsPattern com ctx e, transformBlock com ctx returnStrategy body))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [|Statement.tryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer, ?loc=r)|]

    let rec transformIfStatement (com: IBabelCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
        match com.TransformAsExpr(ctx, guardExpr) with
        | Literal(BooleanLiteral(value=value)) when value ->
            com.TransformAsStatements(ctx, ret, thenStmnt)
        | Literal(BooleanLiteral(value=value)) when not value ->
            com.TransformAsStatements(ctx, ret, elseStmnt)
        | guardExpr ->
            let thenStmnt = transformBlock com ctx ret thenStmnt
            match com.TransformAsStatements(ctx, ret, elseStmnt) with
            | [||] -> Statement.ifStatement(guardExpr, thenStmnt, ?loc=r)
            | [|elseStmnt|] -> Statement.ifStatement(guardExpr, thenStmnt, elseStmnt, ?loc=r)
            | statements -> Statement.ifStatement(guardExpr, thenStmnt, Statement.blockStatement(statements), ?loc=r)
            |> Array.singleton

    let transformGet (com: IBabelCompiler) ctx range typ fableExpr kind =
        match kind with
        | Fable.ExprGet(TransformExpr com ctx prop) ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            getExpr range expr prop

        | Fable.FieldGet info ->
            let fableExpr =
                match fableExpr with
                // If we're accessing a virtual member with default implementation (see #701)
                // from base class, we can use `super` in JS so we don't need the bound this arg
                | Fable.Value(Fable.BaseValue(_,t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr
            let expr = com.TransformAsExpr(ctx, fableExpr)
            get range expr info.Name

        | Fable.ListHead ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
            libCall com ctx range "List" "head" [] [com.TransformAsExpr(ctx, fableExpr)]

        | Fable.ListTail ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
            libCall com ctx range "List" "tail" [] [com.TransformAsExpr(ctx, fableExpr)]

        | Fable.TupleIndex index ->
            match fableExpr with
            // TODO: Check the erased expressions don't have side effects?
            | Fable.Value(Fable.NewTuple(exprs,_), _) ->
                com.TransformAsExpr(ctx, List.item index exprs)
            | TransformExpr com ctx expr -> getExpr range expr (ofInt index)

        | Fable.OptionValue ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            if mustWrapOption typ || com.Options.Language = TypeScript
            then libCall com ctx None "Option" "value" [] [expr]
            else expr

        | Fable.UnionTag ->
            getUnionExprTag com ctx range fableExpr

        | Fable.UnionField info ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            getExpr range (getExpr None expr (Expression.stringLiteral("fields"))) (ofInt info.FieldIndex)

    let transformSet (com: IBabelCompiler) ctx range fableExpr typ (value: Fable.Expr) kind =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        let value = com.TransformAsExpr(ctx, value) |> wrapIntExpression typ
        let ret =
            match kind with
            | Fable.ValueSet -> expr
            | Fable.ExprSet(TransformExpr com ctx e) -> getExpr None expr e
            | Fable.FieldSet(fieldName) -> get None expr fieldName
        assign range ret value

    let transformBindingExprBody (com: IBabelCompiler) (ctx: Context) (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        | Function(args, body) ->
            let name = Some var.Name
            transformFunctionWithAnnotations com ctx name None args body
            |> makeArrowFunctionExpression name
        | _ ->
            if var.IsMutable then
                com.TransformAsExpr(ctx, value)
            else
                com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type

    let transformBindingAsExpr (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        transformBindingExprBody com ctx var value
        |> assign None (identAsExpr var)

    let transformBindingAsStatements (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isJsStatement ctx false value then
            let varPattern, varExpr = typedIdentAsPattern com ctx var, identAsExpr var
            let decl = Statement.variableDeclaration(varPattern)
            let body = com.TransformAsStatements(ctx, Some(Assign varExpr), value)
            Array.append [|decl|] body
        else
            let value = transformBindingExprBody com ctx var value
            let decl = varDeclaration (typedIdentAsPattern com ctx var) var.IsMutable value |> Declaration.VariableDeclaration |> Declaration
            [|decl|]

    let transformTest (com: IBabelCompiler) ctx range kind expr: Expression =
        match kind with
        | Fable.TypeTest t ->
            transformTypeTest com ctx range expr t
        | Fable.OptionTest isSome ->
            com.TransformAsExpr(ctx, expr) |> makeNullCheck range (not isSome)
        | Fable.ListTest nonEmpty ->
            let expr = com.TransformAsExpr(ctx, expr)
            let expr = libCall com ctx range "List" "isEmpty" [] [expr]
            if nonEmpty then Expression.unaryExpression(UnaryNot, expr, ?loc=range) else expr
        | Fable.UnionCaseTest tag ->
            let expected =
                if com.Options.Language = TypeScript then
                    match expr.Type with
                    | Fable.DeclaredType(ent, _) ->
                        let ent = com.GetEntity(ent)
                        match tryJsConstructorWithSuffix com ctx ent "_Tag" with
                        | Some(Expression.Identifier(tagIdent)) -> EnumCaseLiteral(tagIdent, ent.UnionCases[tag].Name) |> Literal
                        | _ -> ofInt tag
                    | _ -> ofInt tag
                else
                    ofInt tag
            let actual = getUnionExprTag com ctx None expr
            Expression.binaryExpression(BinaryEqual, actual, expected, ?loc=range)

    let transformSwitch (com: IBabelCompiler) ctx useBlocks returnStrategy evalExpr cases defaultCase: Statement =
        let consequent caseBody =
            if useBlocks then [|Statement.blockStatement(caseBody)|] else caseBody
        let cases =
            cases |> List.collect (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | None, Fable.Value(Fable.UnitConstant,_), _
                | _, _, [] -> []
                | _, _, guards ->
                    let guards, lastGuard = List.splitLast guards
                    let guards = guards |> List.map (fun e -> SwitchCase.switchCase([||], com.TransformAsExpr(ctx, e)))
                    let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                    let caseBody =
                        match returnStrategy with
                        | Some Return -> caseBody
                        | _ -> Array.append caseBody [|Statement.breakStatement()|]
                    guards @ [SwitchCase.switchCase(consequent caseBody, com.TransformAsExpr(ctx, lastGuard))]
                )
        let cases =
            match defaultCase with
            | Some expr ->
                let defaultCaseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                cases @ [SwitchCase.switchCase(consequent defaultCaseBody)]
            | None -> cases
        Statement.switchStatement(com.TransformAsExpr(ctx, evalExpr), List.toArray cases)

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.sameLength idents values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IBabelCompiler) (ctx: Context) targetIndex boundValues =
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

    let transformDecisionTreeSuccessAsExpr (com: IBabelCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)

    let transformDecisionTreeSuccessAsStatements (com: IBabelCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement[] =
        match returnStrategy with
        | Some(Target targetId) ->
            let idents, _ = getDecisionTarget ctx targetIndex
            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
                    assign None (identAsExpr id) value |> ExpressionStatement)
            let targetAssignment = assign None (targetId |> Expression.Identifier) (ofInt targetIndex) |> ExpressionStatement
            Array.append [|targetAssignment|] assignments
        | ret ->
            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.Binary(BinaryEqual, expr, right), _, _, _) ->
                match expr with
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _), _) -> Some(expr, right)
                | _ -> None
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, Fable.NumberInfo.Empty), None)
                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | Fable.Get(Fable.IdentExpr i1, Fable.FieldGet fieldInfo1,_,_), Fable.Get(Fable.IdentExpr i2, Fable.FieldGet fieldInfo2,_,_) ->
                i1.Name = i2.Name && fieldInfo1.Name = fieldInfo2.Name
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
                    let exprs2 = getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)
        findSuccess Map.empty [expr] |> Seq.choose (fun kv ->
            if kv.Value > 1 then Some kv.Key else None) |> Seq.toList

    /// When several branches share target create first a switch to get the target index and bind value
    /// and another to execute the actual target
    let transformDecisionTreeWithTwoSwitches (com: IBabelCompiler) ctx returnStrategy
                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
        // Declare target and bound idents
        let targetId = getUniqueNameInDeclarationScope ctx "matchResult" |> makeIdent
        let multiVarDecl =
            let boundIdents = targets |> List.collect (fun (idents,_) ->
                idents |> List.map (fun id -> typedIdent com ctx id, None))
            multiVarDeclaration Let ((typedIdent com ctx targetId, None)::boundIdents)
        // Transform targets as switch
        let switch2 =
            // TODO: Declare the last case as the default case?
            let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
            transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None
        // Transform decision tree
        let targetAssign = Target(identAsIdent targetId)
        let ctx = { ctx with DecisionTargets = targets }
        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases = groupSwitchCases (Fable.Number(Int32, Fable.NumberInfo.Empty)) cases (defaultIndex, defaultBoundValues)
            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number(Int32, Fable.NumberInfo.Empty))
            let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
            [|multiVarDecl; switch1; switch2|]
        | None ->
            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
            [| yield multiVarDecl; yield! decisionTree; yield switch2 |]

    let transformDecisionTreeAsStatements (com: IBabelCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement[] =
        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        let targetsWithMultiRefs =
            if com.Options.Language = TypeScript then [] // no hoisting when compiled with types
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
                    idents
                    |> List.exists (fun i -> isIdentUsed i.Name expr)
                    |> function
                        | true -> idents, expr
                        | false -> [], expr)
            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs |> List.exists (fun idx ->
                    targets[idx] |> fst |> List.isEmpty |> not)
            if not hasAnyTargetWithMultiRefsBoundValues then
                match transformDecisionTreeAsSwitch treeExpr with
                | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                    let t = treeExpr.Type
                    let cases = groupSwitchCases t cases (defaultIndex, defaultBoundValues)
                    let ctx = { ctx with DecisionTargets = targets }
                    let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
                    [|transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)|]
                | None ->
                    transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr
            else
                transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr

    let rec transformAsExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        | Fable.Unresolved(_,_,r) -> addErrorAndReturnNull com r "Unexpected unresolved expression"

        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> identAsExpr id

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformFunctionWithAnnotations com ctx name None [arg] body
            |> makeArrowFunctionExpression name

        | Fable.Delegate(args, body, name, tags) ->
            if List.contains "not-arrow" tags then
                let id = name |> Option.map Identifier.identifier
                let args, body, returnType, typeParamDecl = transformFunctionWithAnnotations com ctx name None args body
                Expression.functionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)
            else
                transformFunctionWithAnnotations com ctx name None args body
                |> makeArrowFunctionExpression name

        | Fable.ObjectExpr (members, _, baseCall) ->
           transformObjectExpr com ctx members baseCall

        | Fable.Call(callee, info, typ, range) ->
            transformCall com ctx range typ callee info

        | Fable.CurriedApply(callee, args, _, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, _, _, range) ->
            transformOperation com ctx range kind

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(TransformExpr com ctx guardExpr,
                           TransformExpr com ctx thenExpr,
                           TransformExpr com ctx elseExpr, r) ->
            Expression.conditionalExpression(guardExpr, thenExpr, elseExpr, ?loc=r)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            if ctx.HoistVars [ident] then
                let assignment = transformBindingAsExpr com ctx ident value
                Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
            else iife com ctx expr

        | Fable.LetRec(bindings, body) ->
            if ctx.HoistVars(List.map fst bindings) then
                let values = bindings |> List.mapToArray (fun (id, value) ->
                    transformBindingAsExpr com ctx id value)
                Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
            else iife com ctx expr

        | Fable.Sequential exprs ->
            List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
            |> Expression.sequenceExpression

        | Fable.Emit(info, _, range) ->
            if info.IsStatement then iife com ctx expr
            else transformEmit com ctx range info

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.WhileLoop _ | Fable.ForLoop _ | Fable.TryCatch _ -> iife com ctx expr

        | Fable.Extended(instruction, _) ->
            match instruction with
            | Fable.Curry(e, arity) -> transformCurry com ctx e arity
            | Fable.Throw _ | Fable.Debugger -> iife com ctx expr

    let rec transformAsStatements (com: IBabelCompiler) ctx returnStrategy
                                    (expr: Fable.Expr): Statement array =
        match expr with
        | Fable.Unresolved(_,_,r) ->
            addError com [] r "Unexpected unresolved expression"
            [||]

        | Fable.Extended(kind, r) ->
            match kind with
            | Fable.Curry(e, arity) -> [|transformCurry com ctx e arity |> resolveExpr e.Type returnStrategy|]
            | Fable.Throw(Some(TransformExpr com ctx e), _) -> [|Statement.throwStatement(e, ?loc=r)|]
            | Fable.Throw(None, _) -> [|Statement.throwStatement(Expression.nullLiteral(), ?loc=r)|]
            | Fable.Debugger -> [|Statement.debuggerStatement(?loc=r)|]

        | Fable.TypeCast(e, t) ->
            [|transformCast com ctx t e |> resolveExpr t returnStrategy|]

        | Fable.Value(kind, r) ->
            [|transformValue com ctx r kind |> resolveExpr kind.Type returnStrategy|]

        | Fable.IdentExpr id ->
            [|identAsExpr id |> resolveExpr id.Type returnStrategy|]

        | Fable.Import({ Selector = selector; Path = path }, t, r) ->
            [|transformImport com ctx r selector path |> resolveExpr t returnStrategy|]

        | Fable.Test(expr, kind, range) ->
            [|transformTest com ctx range kind expr |> resolveExpr Fable.Boolean returnStrategy|]

        | Fable.Lambda(arg, body, name) ->
            [|transformFunctionWithAnnotations com ctx name None [arg] body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy|]

        | Fable.Delegate(args, body, name, _) ->
            [|transformFunctionWithAnnotations com ctx name None args body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy|]

        | Fable.ObjectExpr (members, t, baseCall) ->
            [|transformObjectExpr com ctx members baseCall |> resolveExpr t returnStrategy|]

        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements com ctx range typ returnStrategy callee info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApplyAsStatements com ctx range typ returnStrategy callee args

        | Fable.Emit(info, t, range) ->
            let e = transformEmit com ctx range info
            if info.IsStatement then
                [|ExpressionStatement(e)|] // Ignore the return strategy
            else [|resolveExpr t returnStrategy e|]

        | Fable.Operation(kind, _, t, range) ->
            [|transformOperation com ctx range kind |> resolveExpr t returnStrategy|]

        | Fable.Get(expr, kind, t, range) ->
            [|transformGet com ctx range t expr kind |> resolveExpr t returnStrategy|]

        | Fable.Let(ident, value, body) ->
            let binding = transformBindingAsStatements com ctx ident value
            Array.append binding (transformAsStatements com ctx returnStrategy body)

        | Fable.LetRec(bindings, body) ->
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, kind, typ, value, range) ->
            [|transformSet com ctx range expr typ value kind |> resolveExpr expr.Type returnStrategy|]

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
                let guardExpr' = transformAsExpr com ctx guardExpr
                let thenExpr' = transformAsExpr com ctx thenExpr
                let elseExpr' = transformAsExpr com ctx elseExpr
                [|Expression.conditionalExpression(guardExpr', thenExpr', elseExpr', ?loc=r) |> resolveExpr thenExpr.Type returnStrategy|]

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

        | Fable.WhileLoop(TransformExpr com ctx guard, body, range) ->
            [|Statement.whileStatement(guard, transformBlock com ctx None body, ?loc=range)|]

        | Fable.ForLoop (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp, range) ->
            let op1, op2 =
                if isUp
                then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus

            [|Statement.forStatement(
                transformBlock com ctx None body,
                start |> varDeclaration (typedIdentAsPattern com ctx var) true,
                Expression.binaryExpression(op1, identAsExpr var, limit),
                Expression.updateExpression(op2, false, identAsExpr var), ?loc=range)|]

    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr): Pattern array * BlockStatement =
        let tailcallChance =
            Option.map (fun name ->
                NamedTailCallOpportunity(com, ctx, name, args) :> ITailCallOpportunity) name
        let args = discardUnitArg args
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with TailCallOpportunity = tailcallChance
                       HoistVars = fun ids -> declaredVars.AddRange(ids); true
                       OptimizeTailCall = fun () -> isTailCallOptimized <- true
                       IsParamType = true }
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
                    |> List.map (fun (id, tcArg) ->
                        makeTypedIdent id.Type tcArg |> typedIdentAsPattern com ctx)
                let varDecls =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) ->
                        id |> typedIdent com ctx, Some (Expression.identifier(tcArg)))
                    |> multiVarDeclaration Const

                let body = Array.append [|varDecls|] body.Body
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body = BlockStatement(Array.append body [|Statement.breakStatement()|])
                let body =
                    Statement.labeledStatement(Identifier.identifier(tc.Label), Statement.whileStatement(Expression.booleanLiteral(true), body))
                    |> Array.singleton |> BlockStatement
                args', body
            | _ ->
                args |> List.map (typedIdentAsPattern com ctx), body
        let body =
            if declaredVars.Count = 0 then body
            else
                let varDeclStatement = multiVarDeclaration Let [for v in declaredVars -> typedIdent com ctx v, None]
                BlockStatement(Array.append [|varDeclStatement|] body.Body)
        args |> List.toArray, body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = emitExpression None "typeof process === 'object' ? process.argv.slice(2) : []" []
        let main = Expression.callExpression(funcExpr, [|argv|])
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(emitExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        PrivateModuleDeclaration(ExpressionStatement(main))

    let asModuleDeclaration isPublic decl =
        if not isPublic then PrivateModuleDeclaration(decl |> Declaration)
        else ExportNamedDeclaration(decl)

    let declareModuleMember isPublic membName isMutable (expr: Expression) =
        match expr with
        | ClassExpression(body, _id, superClass, implements, typeParameters, _loc) ->
            Declaration.classDeclaration(
                body,
                id = Identifier.identifier(membName),
                ?superClass = superClass,
                ?typeParameters = typeParameters,
                ?implements = implements)
        | FunctionExpression(_, parameters, body, returnType, typeParameters, _) ->
            Declaration.functionDeclaration(
                parameters,
                body,
                id = Identifier.identifier(membName),
                ?returnType = returnType,
                ?typeParameters = typeParameters)
        | _ ->
            let var = Pattern.identifier(membName)
            varDeclaration var isMutable expr
            |> Declaration.VariableDeclaration

        |> asModuleDeclaration isPublic

    let getClassImplements com ctx (ent: Fable.Entity) =
        // let mkNative genArgs typeName =
        //     let id = Identifier.identifier(typeName)
        //     let typeParamInst = makeTypeParamInstantiationIfTypeScript com ctx genArgs
        //     ClassImplements.classImplements(id, ?typeParameters=typeParamInst) |> Some
        let mkImport genArgs moduleName typeName =
            let id = makeImportTypeId com ctx moduleName typeName
            let typeParamInst = makeTypeParamInstantiationIfTypeScript com ctx genArgs
            ClassImplements.classImplements(id, ?typeParameters=typeParamInst) |> Some

        ent.AllInterfaces |> Seq.choose (fun ifc ->
            match ifc.Entity.FullName with
            // | "Fable.Core.JS.Set`1" -> mkNative ifc.GenericArgs "Set"
            // | "Fable.Core.JS.Map`2" -> mkNative ifc.GenericArgs "Map"
            | "Fable.Core.JS.Set`1" -> mkImport ifc.GenericArgs "Util" "ISet"
            | "Fable.Core.JS.Map`2" -> mkImport ifc.GenericArgs "Util" "IMap"
            | _ -> None
        )

    let getEntityFieldsAsIdents _com (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> Naming.sanitizeIdentForbiddenChars |> Naming.checkJsKeywords
            let typ = field.FieldType
            let id: Fable.Ident = { makeTypedIdent typ name with IsMutable = field.IsMutable }
            id)
        |> Seq.toArray

    let declareClassWithParams (com: IBabelCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) (baseExpr: Expression option) classMembers typeParamDecl =
        let implements =
            if com.Options.Language = TypeScript then
                let implements = Util.getClassImplements com ctx ent |> Seq.toArray
                if Array.isEmpty implements then None else Some implements
            else None
        let classCons = makeClassConstructor consArgs consBody
        let classFields =
            if com.Options.Language = TypeScript && not ent.IsFSharpUnion then
                ent.FSharpFields |> List.mapToArray (fun field ->
                    let prop, computed = memberFromName field.Name
                    let ta = makeTypeAnnotation com ctx field.FieldType
                    // Static fields need to be initialized by static constructor
                    let am = if field.IsMutable || field.IsStatic then None else Some Readonly
                    ClassMember.classProperty(prop, computed_=computed, ``static``=field.IsStatic, typeAnnotation=ta, ?accessModifier=am)
                )
            else Array.empty
        Expression.classExpression([|
            yield! classFields
            classCons
            yield! classMembers
        |], ?superClass=baseExpr, ?typeParameters=typeParamDecl, ?implements=implements)
        |> declareModuleMember ent.IsPublic entName false

    let declareClass (com: IBabelCompiler) ctx ent entName consArgs consBody baseExpr classMembers =
        if com.Options.Language = TypeScript
        then FSharp2Fable.Util.getEntityGenArgs ent |> makeTypeParamDecl com ctx |> Some
        else None
        |> declareClassWithParams com ctx ent entName consArgs consBody baseExpr classMembers

    let declareTypeReflection (com: IBabelCompiler) ctx (ent: Fable.Entity) entName: ModuleDeclaration =
        let ta =
            if com.Options.Language = TypeScript then
                makeImportTypeAnnotation com ctx [] "Reflection" "TypeInfo" |> Some
            else None
        let genArgs = Array.init (ent.GenericParameters.Length) (fun i -> "gen" + string i |> makeIdent)
        let generics = genArgs |> Array.map identAsExpr
        let body = transformReflectionInfo com ctx None ent generics
        let args = genArgs |> Array.map (fun x -> Pattern.identifier(x.Name, ?typeAnnotation=ta))
        let returnType = ta
        makeFunctionExpression None (args, body, returnType, None)
        |> declareModuleMember ent.IsPublic (entName + Naming.reflectionSuffix) false

    let declareType (com: IBabelCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr classMembers: ModuleDeclaration list =
        let typeDeclaration = declareClass com ctx ent entName consArgs consBody baseExpr classMembers
        if com.Options.NoReflection then
            [typeDeclaration]
        else
            let reflectionDeclaration = declareTypeReflection com ctx ent entName
            [typeDeclaration; reflectionDeclaration]

    let hasAttribute fullName (atts: Fable.Attribute seq) =
        atts |> Seq.exists (fun att -> att.Entity.FullName = fullName)

    let transformModuleFunction (com: IBabelCompiler) ctx (info: Fable.MemberFunctionOrValue) (membName: string) (args: Fable.Ident list) body =
        let isJsx = hasAttribute Atts.jsxComponent info.Attributes
        let args, body =
            match args with
            | [] -> args, body
            | [arg] when arg.Type = Fable.Unit -> [], body
            | _ when not isJsx -> args, body
            | _ ->
                // SolidJS requires values being accessed directly from the props object for reactivity to work properly
                // https://www.solidjs.com/guides/rendering#props
                let propsArg = makeIdent "$props"
                let propsExpr = Fable.IdentExpr propsArg
                let replacements = args |> List.map (fun a -> a.Name, getFieldWith None a.Type propsExpr a.Name) |> Map
                [propsArg], FableTransforms.replaceValues replacements body

        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx (NonAttached membName) info args body

        Expression.functionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let transformAction (com: IBabelCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        let hasVarDeclarations =
            statements |> Array.exists (function
                | Declaration(Declaration.VariableDeclaration(_)) -> true
                | _ -> false)
        if hasVarDeclarations then
            [ Expression.callExpression(Expression.functionExpression([||], BlockStatement(statements)), [||])
              |> ExpressionStatement |> PrivateModuleDeclaration ]
        else statements |> Array.mapToList (fun x -> PrivateModuleDeclaration(x))

    let transformAttachedProperty (com: IBabelCompiler) ctx (info: Fable.MemberFunctionOrValue) (memb: Fable.MemberDecl) =
        let isStatic = not info.IsInstance
        let kind = if info.IsGetter then ClassGetter else ClassSetter
        let args, body, returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) info memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        ClassMember.classMethod(kind, key, args, body, computed_=computed, ``static``=isStatic,
            ?returnType=returnType) //, ?typeParameters=typeParamDecl)
        |> Array.singleton

    let transformAttachedMethod (com: IBabelCompiler) ctx (info: Fable.MemberFunctionOrValue) (memb: Fable.MemberDecl) =
        let isStatic = not info.IsInstance
        let makeMethod name args body returnType _typeParamDecl =
            let key, computed = memberFromName name
            ClassMember.classMethod(ClassFunction, key, args, body, computed_=computed, ``static``=isStatic,
                ?returnType=returnType) //, ?typeParameters=typeParamDecl)
        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) info memb.Args memb.Body
        [|
            yield makeMethod memb.Name args body returnType typeParamDecl
            if info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" then
                yield makeMethod "Symbol.iterator" [||] (enumerableThisToIterator com ctx) None None
        |]

    let transformUnion (com: IBabelCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let baseExpr = libValue com ctx "Types" "Union" |> Some
        let cases =
            let body =
                ent.UnionCases
                |> List.map (getUnionCaseName >> makeStrConst)
                |> makeArray com ctx
                |> Statement.returnStatement
                |> Array.singleton
                |> BlockStatement
            ClassMember.classMethod(ClassFunction, Expression.identifier("cases"), [||], body)

        if com.Options.Language = TypeScript then
            // Merge this with makeTypeParamDecl/makeTypeParamInstantiation?
            let entParams = ent.GenericParameters |> List.chooseToArray (fun p ->
                if not p.IsMeasure then Some p.Name else None)
            let entParamsDecl = entParams |> Array.map TypeParameter.typeParameter
            let entParamsInst = entParams |> Array.map (makeSimpleTypeAnnotation com ctx)
            let union_tag = entName + "_Tag" |> Identifier.identifier
            let union_fields = entName + "_Fields" |> Identifier.identifier
            let union_cons = entName + "_Cons" |> Identifier.identifier
            let union_ta, union_tag_cases, union_fields_ta =
                ent.UnionCases |> List.mapiToArray (fun i uci ->
                    let typeParams = Array.append entParamsInst [|LiteralTypeAnnotation(EnumCaseLiteral(union_tag, uci.Name))|]
                    let case_ta = TypeAnnotation.aliasTypeAnnotation(union_cons, typeParams)
                    let fields_ta =
                        uci.UnionCaseFields |> List.mapToArray (fun fi ->
                            makeTypeAnnotation com ctx fi.FieldType)
                        |> TupleTypeAnnotation
                    case_ta, (uci.Name, ofInt i), fields_ta
                ) |> Array.unzip3

            let isPublic = ent.IsPublic
            let union_fields_alias = AliasTypeAnnotation(union_fields, entParamsInst)
            let tagArgTa = makeSimpleTypeAnnotation com ctx "Tag"
            let fieldsArgTa = IndexedTypeAnnotation(union_fields_alias, tagArgTa)
            let consArgs = [|
                Pattern.identifier("tag", typeAnnotation=tagArgTa, accessModifier=Readonly)
                Pattern.identifier("fields", typeAnnotation=fieldsArgTa, accessModifier=Readonly)
            |]
            let consBody = BlockStatement [| callSuperAsStatement [] |]
            let classMembers = Array.append [|cases|] classMembers
            let unionConsTypeParams = Some(Array.append entParamsDecl [|
                TypeParameter.typeParameter("Tag", bound=KeyofTypeAnnotation(union_fields_alias))
            |])
            [
                EnumDeclaration(union_tag.Name, union_tag_cases, isConst=true) |> asModuleDeclaration isPublic
                TypeAliasDeclaration(union_fields.Name, entParamsDecl, TupleTypeAnnotation union_fields_ta) |> asModuleDeclaration isPublic
                TypeAliasDeclaration(entName, entParamsDecl, UnionTypeAnnotation union_ta) |> asModuleDeclaration isPublic

                // Helpers to instantiate union
                for case in ent.UnionCases do
                    let args = case.UnionCaseFields |> List.mapToArray (fun fi -> typedIdentWith com ctx None fi.FieldType fi.Name)
                    let tag = EnumCaseLiteral(union_tag, case.Name)
                    let passedArgs = args |> Array.map Expression.Identifier |> Expression.arrayExpression
                    let consTypeParams = Array.append entParamsInst [|LiteralTypeAnnotation tag|]
                    let body = BlockStatement [|
                       Expression.newExpression(Expression.Identifier union_cons, [|Expression.Literal tag; passedArgs|], typeParameters=consTypeParams)
                       |> Statement.returnStatement
                    |]
                    let args = args |> Array.map (fun a -> Pattern.Identifier(a, None))
                    let fnId = entName + "_" + case.Name |> Identifier.identifier
                    Declaration.functionDeclaration(args, body, fnId, typeParameters=entParamsDecl)
                    |> asModuleDeclaration isPublic

                // Actual class
                declareClassWithParams com ctx ent union_cons.Name consArgs consBody baseExpr classMembers unionConsTypeParams
                if not com.Options.NoReflection then
                    declareTypeReflection com ctx ent entName
            ]
        else
            let args = [| Pattern.identifier("tag"); Pattern.identifier("fields") |]
            let body = BlockStatement [|
                callSuperAsStatement []
                yield! ["tag"; "fields"] |> List.map (fun name ->
                    let left = get None thisExpr name
                    let right = Expression.identifier(name)
                    assign None left right |> ExpressionStatement)
            |]
            let classMembers = Array.append [|cases|] classMembers
            declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithCompilerGeneratedConstructor (com: IBabelCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getEntityFieldsAsIdents com ent
        let args = fieldIds |> Array.map identAsExpr
        let baseExpr =
            if ent.IsFSharpExceptionDeclaration
            then libValue com ctx "Types" "FSharpException" |> Some
            elif ent.IsFSharpRecord || ent.IsValueType
            then libValue com ctx "Types" "Record" |> Some
            else None
        let body =
            BlockStatement([|
                if Option.isSome baseExpr then
                    yield callSuperAsStatement []
                yield! ent.FSharpFields |> Seq.mapi (fun i field ->
                    let left = get None thisExpr field.Name
                    let right = wrapIntExpression field.FieldType args[i]
                    assign None left right |> ExpressionStatement)
                |> Seq.toArray
            |])
        let args = fieldIds |> Array.map (typedIdentAsPattern com ctx)
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithPrimaryConstructor (com: IBabelCompiler) ctx (classEnt: Fable.Entity) (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
        let consInfo = com.GetMember(cons.MemberRef)
        let classIdent = Expression.identifier(classDecl.Name)
        let consArgs, consBody, returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx ClassConstructor consInfo cons.Args cons.Body

        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.Options.Language = TypeScript then
                let genArgs = FSharp2Fable.Util.getEntityGenArgs classEnt
                let returnType = getGenericTypeAnnotation com ctx classDecl.Name genArgs
                let typeParamDecl = makeTypeParamDecl com ctx genArgs |> Some
                Some returnType, typeParamDecl
            else
                returnType, None

        let exposedCons =
            let argExprs = consArgs |> Array.map (fun p -> Expression.identifier(p.Name))
            let exposedConsBody = Expression.newExpression(classIdent, argExprs)
            makeFunctionExpression None (consArgs, exposedConsBody, returnType, typeParamDecl)

        let baseExpr, consBody =
            classDecl.BaseCall
            |> extractBaseExprFromBaseCall com ctx classEnt.BaseType
            |> Option.orElseWith (fun () ->
                if classEnt.IsValueType then Some(libValue com ctx "Types" "Record", [])
                else None)
            |> Option.map (fun (baseExpr, baseArgs) ->
                let consBody =
                    consBody.Body
                    |> Array.append [|callSuperAsStatement baseArgs|]
                    |> BlockStatement
                Some baseExpr, consBody)
            |> Option.defaultValue (None, consBody)

        [
            yield! declareType com ctx classEnt classDecl.Name consArgs consBody baseExpr classMembers
            yield declareModuleMember consInfo.IsPublic cons.Name false exposedCons
        ]

    let rec transformDeclaration (com: IBabelCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                let info = com.GetMember(decl.MemberRef)
                let valueExpr =
                    match decl.Body with
                    | body when info.IsValue -> transformAsExpr com ctx body |> Some
                    // Some calls with special attributes (like React lazy or memo) can turn the surrounding function into a value
                    | Fable.Call(callee, ({ ThisArg = None; MemberRef = Some m } as callInfo), _, r) as body ->
                        match com.TryGetMember(m), callInfo.Args with
                        | Some m, _ when hasAttribute "Fable.Core.JS.RemoveSurroundingArgsAttribute" m.Attributes ->
                            transformAsExpr com ctx body |> Some
                        | Some m, arg::restArgs when hasAttribute "Fable.Core.JS.WrapSurroundingFunctionAttribute" m.Attributes ->
                            let arg = transformModuleFunction com ctx info decl.Name decl.Args arg
                            let callee = com.TransformAsExpr(ctx, callee)
                            let restArgs = List.map (fun e -> com.TransformAsExpr(ctx, e)) restArgs
                            callFunction com ctx r callee [] (arg::restArgs) |> Some
                        | _ -> None
                    | _ -> None
                let decls =
                    match valueExpr with
                    | Some value ->
                        [declareModuleMember info.IsPublic decl.Name info.IsMutable value]
                    | None ->
                        let expr = transformModuleFunction com ctx info decl.Name decl.Args decl.Body
                        if hasAttribute Atts.entryPoint info.Attributes
                        then [declareEntryPoint com ctx expr]
                        else [declareModuleMember info.IsPublic decl.Name false expr]

                let isDefaultExport =
                    List.contains "export-default" decl.Tags || (
                        com.TryGetMember(decl.MemberRef)
                        |> Option.map (fun m -> hasAttribute Atts.exportDefault m.Attributes)
                        |> Option.defaultValue false)

                if not isDefaultExport then decls
                else decls @ [ExportDefaultDeclaration(Choice2Of2(Expression.identifier(decl.Name)))]

        | Fable.ClassDeclaration decl ->
            let entRef = decl.Entity
            let ent = com.GetEntity(entRef)
            if ent.IsInterface then
                // TODO: Add type annotation for Typescript
                []
            else
                let classMembers =
                    decl.AttachedMembers
                    |> List.toArray
                    |> Array.collect (fun memb ->
                        withCurrentScope ctx memb.UsedNames <| fun ctx ->
                            memb.ImplementedSignatureRef
                            |> Option.bind (com.TryGetMember)
                            |> Option.orElseWith (fun () -> com.TryGetMember(memb.MemberRef))
                            |> function
                                | None -> [||]
                                | Some info ->
                                    if not memb.IsMangled && (info.IsGetter || info.IsSetter)
                                    then transformAttachedProperty com ctx info memb
                                    else transformAttachedMethod com ctx info memb)

                match decl.Constructor with
                | Some cons ->
                    withCurrentScope ctx cons.UsedNames <| fun ctx ->
                        transformClassWithPrimaryConstructor com ctx ent decl classMembers cons
                | None ->
                    if ent.IsFSharpUnion then transformUnion com ctx ent decl.Name classMembers
                    else transformClassWithCompilerGeneratedConstructor com ctx ent decl.Name classMembers

    let transformImports (imports: Import seq): ModuleDeclaration list =
        let statefulImports = ResizeArray()
        imports |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier.identifier(localId)
                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId)
                    | "default" -> ImportDefaultSpecifier(localId)
                    | memb -> ImportMemberSpecifier(localId, Identifier.identifier(memb)))
            import.Path, specifier)
        |> Seq.groupBy fst
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    match x with
                    | ImportNamespaceSpecifier(_) -> mems, defs, x::alls
                    | ImportDefaultSpecifier(_) -> mems, x::defs, alls
                    | _ -> x::mems, defs, alls)
            // We used to have trouble when mixing member, default and namespace imports,
            // issue an import statement for each kind just in case
            [mems; defs; alls] |> List.choose (function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(List.toArray specifiers, StringLiteral.stringLiteral(path))
                    |> Some)
            |> function
                | [] ->
                    // If there are no specifiers, this is just an import for side effects,
                    // put it after the other ones to match standard JS practices, see #2228
                    ImportDeclaration([||], StringLiteral.stringLiteral(path))
                    |> statefulImports.Add
                    []
                | decls -> decls
            )
        |> fun staticImports -> [
            yield! staticImports
            yield! statefulImports
        ]

    let getIdentForImport (com: IBabelCompiler) (ctx: Context) noMangle (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then selector, None
        else
            let selector, alias =
                match selector with
                | Naming.Regex IMPORT_SELECTOR_REGEX (_::selector::alias::_) ->
                    let alias =
                        if alias.Length = 0 then
                            if selector = "*" || selector = "default"
                            then Path.GetFileNameWithoutExtension(path).Replace("-", "_")
                            else selector
                        else alias
                    selector, alias
                | _ -> selector, selector

            let alias =
                if noMangle then
                    let noConflict = ctx.UsedNames.RootScope.Add(alias)
                    if not noConflict then
                        com.WarnOnlyOnce($"Import {alias} conflicts with existing identifier in root scope")
                    alias
                else
                    getUniqueNameInRootScope ctx alias
            selector, Some alias

module Compiler =
    open Util

    type BabelCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string,Import>()

        interface IBabelCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member com.GetImportExpr(ctx, selector, path, r, noMangle) =
                let noMangle = defaultArg noMangle false
                let selector = selector.Trim()
                let path = path.Trim()
                let cachedName = path + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> Expression.identifier(localIdent)
                    | None -> Expression.nullLiteral()
                | false, _ ->
                    let selector, localId = getIdentForImport com ctx noMangle path selector
                    if selector = Naming.placeholder then
                        "`importMember` must be assigned to a variable"
                        |> addError com [] r
                    let i =
                      { Selector = selector
                        Path = path
                        LocalIdent = localId }
                    imports.Add(cachedName, i)
                    match localId with
                    | Some localId -> Expression.identifier(localId)
                    | None -> Expression.nullLiteral()
            member _.GetAllImports() = imports.Values :> _
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.SourceFiles = com.SourceFiles
            member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
            member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = BabelCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IBabelCompiler
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
            IsParamType = false
            ScopedTypeParams = Set.empty }
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let importDecls = com.GetAllImports() |> transformImports
        let body = importDecls @ rootDecls |> List.toArray
        Program(body)
