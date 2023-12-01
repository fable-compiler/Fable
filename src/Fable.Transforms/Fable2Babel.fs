module rec Fable.Transforms.Fable2Babel

open System
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

type ConstructorRef =
    | Annotation
    | ActualConsRef
    | Reflection

type Import =
    {
        Selector: string
        LocalIdent: string option
        Path: string
    }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
    {
        RootScope: HashSet<string>
        DeclarationScopes: HashSet<string>
        CurrentDeclarationScope: HashSet<string>
    }

type Context =
    {
        File: Fable.File
        UsedNames: UsedNames
        DecisionTargets: (Fable.Ident list * Fable.Expr) list
        HoistVars: Fable.Ident list -> bool
        TailCallOpportunity: ITailCallOpportunity option
        OptimizeTailCall: unit -> unit
        ScopedTypeParams: Set<string>
        ForcedIdents: Set<string>
    }

type ModuleDecl(name, ?isPublic, ?isMutable, ?typ, ?doc) =
    member _.Name: string = name
    member _.IsPublic = defaultArg isPublic false
    member _.IsMutable = defaultArg isMutable false
    member _.Type = defaultArg typ Fable.Any
    member _.JsDoc: string option = doc

type IBabelCompiler =
    inherit Compiler
    abstract IsTypeScript: bool
    abstract GetAllImports: unit -> seq<Import>

    abstract GetImportExpr:
        Context *
        selector: string *
        path: string *
        range: SourceLocation option *
        ?noMangle: bool ->
            Expression

    abstract TransformAsExpr: Context * Fable.Expr -> Expression

    abstract TransformAsStatements:
        Context * ReturnStrategy option * Fable.Expr -> Statement array

    abstract TransformImport:
        Context * selector: string * path: string -> Expression

    abstract TransformFunction:
        Context * string option * Fable.Ident list * Fable.Expr ->
            (Parameter array) * BlockStatement

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Lib =

    let libCall (com: IBabelCompiler) ctx r moduleName memberName genArgs args =
        let typeArguments =
            Annotation.makeTypeParamInstantiationIfTypeScript com ctx genArgs

        let callee =
            com.TransformImport(ctx, memberName, getLibPath com moduleName)

        Expression.callExpression (
            callee,
            List.toArray args,
            ?typeArguments = typeArguments,
            ?loc = r
        )

    let libValue (com: IBabelCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryJsConstructorWithSuffix
        (com: IBabelCompiler)
        ctx
        ent
        (suffix: string)
        =
        match JS.Replacements.tryConstructor com ent with
        | Some(Fable.Import(info, typ, range)) when suffix.Length > 0 ->
            let consExpr =
                Fable.Import(
                    { info with Selector = info.Selector + suffix },
                    typ,
                    range
                )

            com.TransformAsExpr(ctx, consExpr) |> Some
        | Some(Fable.IdentExpr ident) when suffix.Length > 0 ->
            let consExpr =
                Fable.IdentExpr { ident with Name = ident.Name + suffix }

            com.TransformAsExpr(ctx, consExpr) |> Some
        | consExpr ->
            consExpr |> Option.map (fun e -> com.TransformAsExpr(ctx, e))

    let tryJsConstructorFor
        purpose
        (com: IBabelCompiler)
        ctx
        (ent: Fable.Entity)
        =
        let isErased =
            match purpose with
            | Annotation ->
                ent.IsMeasure
                || (ent.IsInterface && not com.IsTypeScript)
                || (FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    && not ent.IsFSharpUnion)
            // Historically we have used interfaces to represent JS classes in bindings,
            // so we allow explicit type references (e.g. for type testing) when the interface is global or imported.
            // But just in case we avoid referencing interfaces for reflection (as the type may not exist in actual code)
            | ActualConsRef ->
                if ent.IsInterface then
                    not (FSharp2Fable.Util.isGlobalOrImportedEntity ent)
                else
                    ent.IsMeasure
                    || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
            | Reflection ->
                ent.IsInterface
                || ent.IsMeasure
                || FSharp2Fable.Util.isErasedOrStringEnumEntity ent

        if isErased then
            None
        else
            let suffix =
                match purpose with
                | Reflection
                | ActualConsRef -> ""
                | Annotation when
                    com.IsTypeScript
                    && ent.IsFSharpUnion
                    && List.isMultiple ent.UnionCases
                    && not (
                        Util.hasAnyAttribute
                            [
                                Atts.stringEnum
                                Atts.erase
                                Atts.tsTaggedUnion
                            ]
                            ent.Attributes
                    )
                    ->
                    Util.UnionHelpers.UNION_SUFFIX
                | Annotation -> ""

            tryJsConstructorWithSuffix com ctx ent suffix

    /// Cannot be used for annotations (use `tryJsConstructorFor Annotation` instead)
    let jsConstructor (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        tryJsConstructorFor ActualConsRef com ctx ent
        |> Option.defaultWith (fun () ->
            $"Cannot find %s{ent.FullName} constructor" |> addError com [] None
            Expression.nullLiteral ()
        )

    let sanitizeMemberName memberName =
        if memberName = "constructor" then
            memberName + "$"
        else
            memberName

module Reflection =
    open Lib

    let private libReflectionCall (com: IBabelCompiler) ctx r memberName args =
        libCall com ctx r "Reflection" (memberName + "_type") [] args

    let private transformRecordReflectionInfo
        com
        ctx
        r
        (ent: Fable.Entity)
        generics
        =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringLiteral (fullname)

        let genMap =
            let genParamNames =
                ent.GenericParameters
                |> List.mapToArray (fun x -> x.Name)
                |> Seq.toArray

            Array.zip genParamNames generics |> Map |> Some

        let fields =
            ent.FSharpFields
            |> List.map (fun fi ->
                let fieldName =
                    sanitizeMemberName fi.Name |> Expression.stringLiteral

                let typeInfo =
                    transformTypeInfoFor
                        Reflection
                        com
                        ctx
                        r
                        genMap
                        fi.FieldType

                Expression.arrayExpression (
                    [|
                        fieldName
                        typeInfo
                    |]
                )
            )
            |> List.toArray

        let fields =
            Expression.arrowFunctionExpression (
                [||],
                Expression.arrayExpression (fields)
            )

        [
            fullnameExpr
            Expression.arrayExpression (generics)
            jsConstructor com ctx ent
            fields
        ]
        |> libReflectionCall com ctx None "record"

    let private transformUnionReflectionInfo
        com
        ctx
        r
        (ent: Fable.Entity)
        generics
        =
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringLiteral (fullname)

        let genMap =
            let genParamNames =
                ent.GenericParameters
                |> List.map (fun x -> x.Name)
                |> Seq.toArray

            Array.zip genParamNames generics |> Map |> Some

        let cases =
            ent.UnionCases
            |> Seq.map (fun uci ->
                uci.UnionCaseFields
                |> List.mapToArray (fun fi ->
                    Expression.arrayExpression (
                        [|
                            fi.Name |> Expression.stringLiteral
                            transformTypeInfoFor
                                Reflection
                                com
                                ctx
                                r
                                genMap
                                fi.FieldType
                        |]
                    )
                )
                |> Expression.arrayExpression
            )
            |> Seq.toArray

        let cases =
            Expression.arrowFunctionExpression (
                [||],
                Expression.arrayExpression (cases)
            )

        [
            fullnameExpr
            Expression.arrayExpression (generics)
            jsConstructor com ctx ent
            cases
        ]
        |> libReflectionCall com ctx None "union"

    let transformTypeInfoFor
        purpose
        (com: IBabelCompiler)
        ctx
        r
        (genMap: Map<string, Expression> option)
        t
        : Expression
        =
        let primitiveTypeInfo name =
            libValue com ctx "Reflection" (name + "_type")

        let numberInfo kind =
            getNumberKindName kind |> primitiveTypeInfo

        let nonGenericTypeInfo fullname =
            [ Expression.stringLiteral (fullname) ]
            |> libReflectionCall com ctx None "class"

        let resolveGenerics generics : Expression list =
            generics |> List.map (transformTypeInfoFor purpose com ctx r genMap)

        let genericTypeInfo name genArgs =
            let resolved = resolveGenerics genArgs
            libReflectionCall com ctx None name resolved

        let genericEntity (fullname: string) generics =
            libReflectionCall
                com
                ctx
                None
                "class"
                [
                    Expression.stringLiteral (fullname)
                    if not (Array.isEmpty generics) then
                        Expression.arrayExpression (generics)
                ]

        let genericGlobalOrImportedEntity generics (ent: Fable.Entity) =
            libReflectionCall
                com
                ctx
                None
                "class"
                [
                    yield Expression.stringLiteral (ent.FullName)
                    match generics with
                    | [||] -> yield Util.undefined None None
                    | generics -> yield Expression.arrayExpression (generics)
                    match tryJsConstructorFor purpose com ctx ent with
                    | Some cons -> yield cons
                    | None -> ()
                ]

        match t with
        | Fable.Measure _
        | Fable.Any -> primitiveTypeInfo "obj"
        | Fable.GenericParam(name = name) ->
            match genMap with
            | None ->
                [ Expression.stringLiteral (name) ]
                |> libReflectionCall com ctx None "generic"
            | Some genMap ->
                match Map.tryFind name genMap with
                | Some t -> t
                | None ->
                    Replacements.Util.genericTypeInfoError name
                    |> addError com [] r

                    Expression.nullLiteral ()
        | Fable.Unit -> primitiveTypeInfo "unit"
        | Fable.Boolean -> primitiveTypeInfo "bool"
        | Fable.Char -> primitiveTypeInfo "char"
        | Fable.String -> primitiveTypeInfo "string"
        | Fable.Number(kind, info) ->
            match info with
            | Fable.NumberInfo.IsEnum entRef ->
                let ent = com.GetEntity(entRef)

                let cases =
                    ent.FSharpFields
                    |> List.choose (fun fi ->
                        match fi.Name with
                        | "value__" -> None
                        | name ->
                            let value =
                                match fi.LiteralValue with
                                | Some v -> System.Convert.ToDouble v
                                | None -> 0.

                            Expression.arrayExpression (
                                [|
                                    Expression.stringLiteral (name)
                                    Expression.numericLiteral (value)
                                |]
                            )
                            |> Some
                    )
                    |> Seq.toArray
                    |> Expression.arrayExpression

                [
                    Expression.stringLiteral (entRef.FullName)
                    numberInfo kind
                    cases
                ]
                |> libReflectionCall com ctx None "enum"
            | _ -> numberInfo kind
        | Fable.LambdaType(argType, returnType) ->
            genericTypeInfo
                "lambda"
                [
                    argType
                    returnType
                ]
        | Fable.DelegateType(argTypes, returnType) ->
            genericTypeInfo
                "delegate"
                [
                    yield! argTypes
                    yield returnType
                ]
        | Fable.Tuple(genArgs, _) -> genericTypeInfo "tuple" genArgs
        | Fable.Option(genArg, _) -> genericTypeInfo "option" [ genArg ]
        | Fable.Array(genArg, _) -> genericTypeInfo "array" [ genArg ]
        | Fable.List genArg -> genericTypeInfo "list" [ genArg ]
        | Fable.Regex -> nonGenericTypeInfo Types.regex
        | Fable.MetaType -> nonGenericTypeInfo Types.type_
        | Fable.AnonymousRecordType(fieldNames, genArgs, _isStruct) ->
            let genArgs = resolveGenerics genArgs

            List.zip (fieldNames |> Array.toList) genArgs
            |> List.map (fun (k, t) ->
                Expression.arrayExpression
                    [|
                        Expression.stringLiteral (k)
                        t
                    |]
            )
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
                    genericEntity
                        fullName
                        [| transformTypeInfoFor purpose com ctx r genMap gen |]
                | Replacements.Util.BclDictionary(key, value)
                | Replacements.Util.BclKeyValuePair(key, value)
                | Replacements.Util.FSharpMap(key, value) ->
                    genericEntity
                        fullName
                        [|
                            transformTypeInfoFor purpose com ctx r genMap key
                            transformTypeInfoFor purpose com ctx r genMap value
                        |]
                | Replacements.Util.FSharpResult(ok, err) ->
                    let ent = com.GetEntity(entRef)

                    transformUnionReflectionInfo
                        com
                        ctx
                        r
                        ent
                        [|
                            transformTypeInfoFor purpose com ctx r genMap ok
                            transformTypeInfoFor purpose com ctx r genMap err
                        |]
                | Replacements.Util.FSharpChoice gen ->
                    let ent = com.GetEntity(entRef)

                    let gen =
                        List.map
                            (transformTypeInfoFor purpose com ctx r genMap)
                            gen

                    List.toArray gen
                    |> transformUnionReflectionInfo com ctx r ent
                | Replacements.Util.FSharpReference gen ->
                    let ent = com.GetEntity(entRef)

                    [| transformTypeInfoFor purpose com ctx r genMap gen |]
                    |> transformRecordReflectionInfo com ctx r ent
            | _ ->
                let generics =
                    genArgs
                    |> List.map (transformTypeInfoFor purpose com ctx r genMap)
                    |> List.toArray

                match com.GetEntity(entRef) with
                | Patterns.Try (Util.tryFindAnyEntAttribute [ Atts.stringEnum
                                                              Atts.erase
                                                              Atts.tsTaggedUnion ]) (att,
                                                                                     _) as ent ->
                    match att with
                    | Atts.stringEnum -> primitiveTypeInfo "string"
                    | Atts.erase ->
                        match ent.UnionCases with
                        | [ uci ] when List.isSingle uci.UnionCaseFields ->
                            transformTypeInfoFor
                                purpose
                                com
                                ctx
                                r
                                genMap
                                uci.UnionCaseFields[0].FieldType
                        | cases when
                            cases
                            |> List.forall (fun c ->
                                List.isEmpty c.UnionCaseFields
                            )
                            ->
                            primitiveTypeInfo "string"
                        | _ -> genericEntity ent.FullName generics
                    | _ -> genericEntity ent.FullName generics
                | ent ->
                    if FSharp2Fable.Util.isGlobalOrImportedEntity ent then
                        genericGlobalOrImportedEntity generics ent
                    elif
                        ent.IsInterface
                        || FSharp2Fable.Util.isReplacementCandidate entRef
                    then
                        genericEntity ent.FullName generics
                    elif ent.IsMeasure then
                        [ Expression.stringLiteral (ent.FullName) ]
                        |> libReflectionCall com ctx None "measure"
                    else
                        let reflectionMethodExpr =
                            FSharp2Fable.Util.entityIdentWithSuffix
                                com
                                entRef
                                Naming.reflectionSuffix

                        let callee =
                            com.TransformAsExpr(ctx, reflectionMethodExpr)

                        Expression.callExpression (callee, generics)

    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName

            [
                yield Expression.stringLiteral (fullname)
                match generics with
                | [||] -> yield Util.undefined None None
                | generics -> yield Expression.arrayExpression (generics)
                match tryJsConstructorFor Reflection com ctx ent with
                | Some cons -> yield cons
                | None -> ()
                match ent.BaseType with
                | Some d ->
                    let genMap =
                        Seq.zip ent.GenericParameters generics
                        |> Seq.map (fun (p, e) -> p.Name, e)
                        |> Map
                        |> Some

                    yield
                        Fable.DeclaredType(d.Entity, d.GenericArgs)
                        |> transformTypeInfoFor Reflection com ctx r genMap
                | None -> ()
            ]
            |> libReflectionCall com ctx r "class"

    let transformTypeTest
        (com: IBabelCompiler)
        ctx
        range
        expr
        (typ: Fable.Type)
        : Expression
        =
        let warnAndEvalToFalse msg =
            "Cannot type test (evals to false): " + msg
            |> addWarning com [] range

            Expression.booleanLiteral (false)

        let jsTypeof
            (primitiveType: string)
            (Util.TransformExpr com ctx expr)
            : Expression
            =
            let typeof = Expression.unaryExpression ("typeof", expr)

            Expression.binaryExpression (
                BinaryEqual,
                typeof,
                Expression.stringLiteral (primitiveType),
                ?loc = range
            )

        let jsInstanceof
            consExpr
            (Util.TransformExpr com ctx expr)
            : Expression
            =
            BinaryExpression(expr, consExpr, "instanceof", range)

        match typ with
        | Fable.Measure _ // Dummy, shouldn't be possible to test against a measure type
        | Fable.Any -> Expression.booleanLiteral (true)
        | Fable.Unit ->
            com.TransformAsExpr(ctx, expr) |> Util.makeNullCheck range true
        | Fable.Boolean -> jsTypeof "boolean" expr
        | Fable.Char
        | Fable.String -> jsTypeof "string" expr
        | Fable.Number(Decimal, _) ->
            jsInstanceof (libValue com ctx "Decimal" "default") expr
        | Fable.Number(JS.Replacements.BigIntegers _, _) ->
            jsTypeof "bigint" expr
        | Fable.Number _ -> jsTypeof "number" expr
        | Fable.Regex -> jsInstanceof (Expression.identifier ("RegExp")) expr
        | Fable.LambdaType _
        | Fable.DelegateType _ -> jsTypeof "function" expr
        | Fable.Array _
        | Fable.Tuple _ ->
            libCall
                com
                ctx
                None
                "Util"
                "isArrayLike"
                []
                [ com.TransformAsExpr(ctx, expr) ]
        | Fable.List _ ->
            jsInstanceof (libValue com ctx "List" "FSharpList") expr
        | Fable.AnonymousRecordType _ -> warnAndEvalToFalse "anonymous records"
        | Fable.MetaType ->
            jsInstanceof (libValue com ctx "Reflection" "TypeInfo") expr
        | Fable.Option _ -> warnAndEvalToFalse "options" // TODO
        | Fable.GenericParam _ -> warnAndEvalToFalse "generic parameters"
        | Fable.DeclaredType(ent, genArgs) ->
            match ent.FullName with
            | Types.idisposable ->
                match expr with
                | MaybeCasted(ExprType(Fable.DeclaredType(ent2, _))) when
                    com.GetEntity(ent2)
                    |> FSharp2Fable.Util.hasInterface Types.idisposable
                    ->
                    Expression.booleanLiteral (true)
                | _ ->
                    [ com.TransformAsExpr(ctx, expr) ]
                    |> libCall com ctx range "Util" "isDisposable" []
            | Types.ienumerable ->
                [ com.TransformAsExpr(ctx, expr) ]
                |> libCall com ctx range "Util" "isIterable" []
            | Types.array ->
                [ com.TransformAsExpr(ctx, expr) ]
                |> libCall com ctx range "Util" "isArrayLike" []
            | Types.exception_ ->
                [ com.TransformAsExpr(ctx, expr) ]
                |> libCall com ctx range "Types" "isException" []
            | _ ->
                match com.GetEntity(ent) with
                | Patterns.Try (Util.tryFindAnyEntAttribute [ Atts.stringEnum
                                                              Atts.erase
                                                              Atts.tsTaggedUnion ]) (att,
                                                                                     _) as ent ->
                    match att with
                    | Atts.stringEnum -> jsTypeof "string" expr
                    | Atts.erase when ent.IsFSharpUnion ->
                        match ent.UnionCases with
                        | [ uci ] when List.isSingle uci.UnionCaseFields ->
                            transformTypeTest
                                com
                                ctx
                                range
                                expr
                                uci.UnionCaseFields[0].FieldType
                        | cases when
                            cases
                            |> List.forall (fun c ->
                                List.isEmpty c.UnionCaseFields
                            )
                            ->
                            jsTypeof "string" expr
                        | _ -> warnAndEvalToFalse (ent.FullName + " (erased)")
                    | _ -> warnAndEvalToFalse (ent.FullName + " (erased)")

                | Patterns.Try (tryJsConstructorFor ActualConsRef com ctx) cons ->
                    if not (List.isEmpty genArgs) then
                        com.WarnOnlyOnce(
                            "Generic args are ignored in type testing",
                            ?range = range
                        )

                    jsInstanceof cons expr

                | _ -> warnAndEvalToFalse ent.FullName

module Annotation =
    let isByRefOrAnyType (com: IBabelCompiler) =
        function
        | Replacements.Util.IsByRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let isInRefOrAnyType (com: IBabelCompiler) =
        function
        | Replacements.Util.IsInRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let makeTypeParamDecl (com: IBabelCompiler) (ctx: Context) genArgs =
        // Maybe there's a way to represent measurements in TypeScript
        genArgs
        |> List.chooseToArray (
            function
            | Fable.GenericParam(name, isMeasure, constraints) when
                not isMeasure
                ->
                // TODO: Other constraints? comparison, nullable
                let bound =
                    constraints
                    |> List.choose (
                        function
                        | Fable.Constraint.CoercesTo t ->
                            makeTypeAnnotation com ctx t |> Some
                        | _ -> None
                    )
                    |> function
                        | [] -> None
                        | [ t ] -> Some t
                        | ts ->
                            ts
                            |> List.toArray
                            |> IntersectionTypeAnnotation
                            |> Some

                TypeParameter.typeParameter (name, ?bound = bound) |> Some
            | _ -> None
        )

    let makeTypeParamInstantiation (com: IBabelCompiler) ctx genArgs =
        if List.isEmpty genArgs then
            [||]
        else
            genArgs
            |> List.chooseToArray (fun t ->
                if isUnitOfMeasure t then
                    None
                else
                    makeTypeAnnotation com ctx t |> Some
            )

    let makeTypeParamInstantiationIfTypeScript
        (com: IBabelCompiler)
        ctx
        genArgs
        =
        if com.IsTypeScript then
            makeTypeParamInstantiation com ctx genArgs |> Some
        else
            None

    let getGenericTypeAnnotation com ctx name genArgs =
        let typeParamInst = makeTypeParamInstantiation com ctx genArgs

        TypeAnnotation.aliasTypeAnnotation (
            Identifier.identifier (name),
            typeArguments = typeParamInst
        )

    let makeTypeAnnotation com ctx typ : TypeAnnotation =
        match typ with
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Any -> AnyTypeAnnotation
        | Fable.Unit -> VoidTypeAnnotation
        | Fable.Boolean -> BooleanTypeAnnotation
        | Fable.Char -> StringTypeAnnotation
        | Fable.String -> StringTypeAnnotation
        | Fable.Regex -> makeAliasTypeAnnotation com ctx "RegExp"
        | Fable.Number(BigInt, _) -> makeAliasTypeAnnotation com ctx "bigint"
        | Fable.Number(kind, _) -> makeNumericTypeAnnotation com ctx kind
        | Fable.Option(genArg, _) -> makeOptionTypeAnnotation com ctx genArg
        | Fable.Tuple(genArgs, _) -> makeTupleTypeAnnotation com ctx genArgs
        | Fable.Array(genArg, kind) ->
            makeArrayTypeAnnotation com ctx genArg kind
        | Fable.List genArg -> makeListTypeAnnotation com ctx genArg
        | Fable.GenericParam(name = name) ->
            makeAliasTypeAnnotation com ctx name
        | Fable.LambdaType(argType, returnType) ->
            makeFunctionTypeAnnotation com ctx typ [ argType ] returnType
        | Fable.DelegateType(argTypes, returnType) ->
            makeFunctionTypeAnnotation com ctx typ argTypes returnType
        | Fable.AnonymousRecordType(fieldNames, fieldTypes, _isStruct) ->
            makeAnonymousRecordTypeAnnotation com ctx fieldNames fieldTypes
        | Replacements.Util.Builtin kind ->
            makeBuiltinTypeAnnotation com ctx typ kind
        | Fable.DeclaredType(entRef, genArgs) ->
            com.GetEntity(entRef) |> makeEntityTypeAnnotation com ctx genArgs

    let makeTypeAnnotationIfTypeScript (com: IBabelCompiler) ctx typ expr =
        if com.IsTypeScript then
            match typ, expr with
            | Fable.Option _, _ -> makeTypeAnnotation com ctx typ |> Some
            // Use type annotation for NullLiteral and enum cases
            | _, Some(Literal(Literal.StringLiteral _))
            | _, Some(Literal(StringTemplate _))
            | _, Some(Literal(BooleanLiteral _))
            | _, Some(Literal(NumericLiteral _))
            | _, Some(Literal(RegExp _))
            | _, Some(FunctionExpression _)
            | _, Some(ArrowFunctionExpression _) -> None
            | _, Some(AsExpression _) -> None
            | _ -> makeTypeAnnotation com ctx typ |> Some
        else
            None

    // Fields are uncurried in the AST but not the declaration
    let makeFieldAnnotation (com: IBabelCompiler) ctx (fieldType: Fable.Type) =
        FableTransforms.uncurryType fieldType |> makeTypeAnnotation com ctx

    let makeFieldAnnotationIfTypeScript
        (com: IBabelCompiler)
        ctx
        (fieldType: Fable.Type)
        =
        if com.IsTypeScript then
            makeFieldAnnotation com ctx fieldType |> Some
        else
            None

    let makeTypeAnnotationWithParametersIfTypeScript
        (com: IBabelCompiler)
        ctx
        typ
        expr
        =
        match makeTypeAnnotationIfTypeScript com ctx typ expr with
        | Some(FunctionTypeAnnotation _) as annotation ->
            let _, typeParams =
                match typ with
                | Fable.LambdaType(argType, returnType) ->
                    [
                        argType
                        returnType
                    ]
                | Fable.DelegateType(argTypes, returnType) ->
                    argTypes @ [ returnType ]
                | _ -> []
                |> Util.getTypeParameters ctx

            annotation, makeTypeParamDecl com ctx typeParams
        | annotation -> annotation, [||]

    let makeAliasTypeAnnotation _com _ctx name =
        TypeAnnotation.aliasTypeAnnotation (Identifier.identifier (name))

    let makeGenericTypeAnnotation com ctx genArgs id =
        let typeParamInst = makeTypeParamInstantiation com ctx genArgs
        TypeAnnotation.aliasTypeAnnotation (id, typeArguments = typeParamInst)

    let makeNativeTypeAnnotation com ctx genArgs typeName =
        Identifier.identifier (typeName)
        |> makeGenericTypeAnnotation com ctx genArgs

    let makeFableLibImportTypeId (com: IBabelCompiler) ctx moduleName typeName =
        let expr =
            com.GetImportExpr(ctx, typeName, getLibPath com moduleName, None)

        match expr with
        | Expression.Identifier(id) -> id
        | _ -> Identifier.identifier (typeName)

    let makeFableLibImportTypeAnnotation com ctx genArgs moduleName typeName =
        let id = makeFableLibImportTypeId com ctx moduleName typeName
        makeGenericTypeAnnotation com ctx genArgs id

    let makeNumericTypeAnnotation com ctx kind =
        let moduleName =
            match kind with
            | Decimal -> "Decimal"
            | JS.Replacements.BigIntegers _ -> "BigInt"
            | _ -> "Int32"

        let typeName = getNumberKindName kind
        makeFableLibImportTypeAnnotation com ctx [] moduleName typeName

    let makeNullableTypeAnnotation com ctx genArg =
        makeFableLibImportTypeAnnotation com ctx [ genArg ] "Option" "Nullable"

    let makeOptionTypeAnnotation com ctx genArg =
        makeFableLibImportTypeAnnotation com ctx [ genArg ] "Option" "Option"

    let makeTupleTypeAnnotation com ctx genArgs =
        List.map (makeTypeAnnotation com ctx) genArgs
        |> List.toArray
        |> TupleTypeAnnotation

    let makeArrayTypeAnnotation com ctx genArg kind =
        match genArg with
        | JS.Replacements.TypedArrayCompatible com kind name ->
            makeAliasTypeAnnotation com ctx name
        | _ ->
            // makeNativeTypeAnnotation com ctx [genArg] "Array"
            makeTypeAnnotation com ctx genArg |> ArrayTypeAnnotation

    let makeListTypeAnnotation com ctx genArg =
        makeFableLibImportTypeAnnotation com ctx [ genArg ] "List" "FSharpList"

    let makeUnionTypeAnnotation com ctx genArgs =
        List.map (makeTypeAnnotation com ctx) genArgs
        |> List.toArray
        |> UnionTypeAnnotation

    let makeBuiltinTypeAnnotation com ctx typ kind =
        match kind with
        | Replacements.Util.BclGuid -> StringTypeAnnotation
        | Replacements.Util.BclTimeSpan -> NumberTypeAnnotation
        | Replacements.Util.BclDateTime ->
            makeAliasTypeAnnotation com ctx "Date"
        | Replacements.Util.BclDateTimeOffset ->
            makeAliasTypeAnnotation com ctx "Date"
        | Replacements.Util.BclDateOnly ->
            makeAliasTypeAnnotation com ctx "Date"
        | Replacements.Util.BclTimeOnly -> NumberTypeAnnotation
        | Replacements.Util.BclTimer ->
            makeFableLibImportTypeAnnotation com ctx [] "Timer" "Timer"
        | Replacements.Util.BclHashSet key ->
            makeFableLibImportTypeAnnotation com ctx [ key ] "Util" "ISet"
        | Replacements.Util.BclDictionary(key, value) ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                [
                    key
                    value
                ]
                "Util"
                "IMap"
        | Replacements.Util.BclKeyValuePair(key, value) ->
            makeTupleTypeAnnotation
                com
                ctx
                [
                    key
                    value
                ]
        | Replacements.Util.FSharpSet key ->
            makeFableLibImportTypeAnnotation com ctx [ key ] "Set" "FSharpSet"
        | Replacements.Util.FSharpMap(key, value) ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                [
                    key
                    value
                ]
                "Map"
                "FSharpMap"
        | Replacements.Util.FSharpResult(ok, err) ->
            $"FSharpResult$2{Util.UnionHelpers.UNION_SUFFIX}"
            |> makeFableLibImportTypeAnnotation
                com
                ctx
                [
                    ok
                    err
                ]
                "Choice"
        | Replacements.Util.FSharpChoice genArgs ->
            $"FSharpChoice${List.length genArgs}{Util.UnionHelpers.UNION_SUFFIX}"
            |> makeFableLibImportTypeAnnotation com ctx genArgs "Choice"
        | Replacements.Util.FSharpReference genArg ->
            if isInRefOrAnyType com typ then
                makeTypeAnnotation com ctx genArg
            else
                makeFableLibImportTypeAnnotation
                    com
                    ctx
                    [ genArg ]
                    "Types"
                    "FSharpRef"

    let makeFunctionTypeAnnotation com ctx _typ argTypes returnType =
        let funcTypeParams =
            match argTypes with
            | [ Fable.Unit ] -> []
            | _ -> argTypes
            |> List.mapi (fun i argType ->
                FunctionTypeParam.functionTypeParam (
                    Identifier.identifier ($"arg{i}"),
                    makeTypeAnnotation com ctx argType
                )
            )
            |> List.toArray

        let returnType = makeTypeAnnotation com ctx returnType
        TypeAnnotation.functionTypeAnnotation (funcTypeParams, returnType)

    // Move this to Replacements.tryEntity?
    let tryNativeOrFableLibraryInterface com ctx genArgs (ent: Fable.Entity) =
        match ent.FullName with
        | _ when not ent.IsInterface -> None
        | Types.icollection ->
            makeNativeTypeAnnotation com ctx genArgs "Iterable" |> Some
        // -> makeFableLibImportTypeAnnotation com ctx [Fable.Any] "Util" "ICollection"
        | Types.icollectionGeneric ->
            makeNativeTypeAnnotation com ctx genArgs "Iterable" |> Some
        // -> makeFableLibImportTypeAnnotation com ctx genArgs "Util" "ICollection"
        // | Types.idictionary
        // | Types.ireadonlydictionary
        | Types.idisposable ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                genArgs
                "Util"
                "IDisposable"
            |> Some
        | Types.ienumerable ->
            makeNativeTypeAnnotation com ctx [ Fable.Any ] "Iterable" |> Some
        // -> makeFableLibImportTypeAnnotation com ctx [Fable.Any] "Util" "IEnumerable" |> Some
        | Types.ienumerableGeneric ->
            makeNativeTypeAnnotation com ctx genArgs "Iterable" |> Some
        // -> makeFableLibImportTypeAnnotation com ctx genArgs "Util" "IEnumerable" |> Some
        | Types.ienumerator ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                [ Fable.Any ]
                "Util"
                "IEnumerator"
            |> Some
        | Types.ienumeratorGeneric ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                genArgs
                "Util"
                "IEnumerator"
            |> Some
        | Types.icomparable ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                [ Fable.Any ]
                "Util"
                "IComparable"
            |> Some
        | Types.icomparableGeneric
        | Types.iStructuralComparable ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                genArgs
                "Util"
                "IComparable"
            |> Some
        | Types.iequatableGeneric
        | Types.iStructuralEquatable ->
            makeFableLibImportTypeAnnotation com ctx genArgs "Util" "IEquatable"
            |> Some
        | Types.icomparer ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                [ Fable.Any ]
                "Util"
                "IComparer"
            |> Some
        | Types.icomparerGeneric ->
            makeFableLibImportTypeAnnotation com ctx genArgs "Util" "IComparer"
            |> Some
        | Types.iequalityComparerGeneric ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                genArgs
                "Util"
                "IEqualityComparer"
            |> Some
        | Types.iobserverGeneric ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                genArgs
                "Observable"
                "IObserver"
            |> Some
        | Types.iobservableGeneric ->
            makeFableLibImportTypeAnnotation
                com
                ctx
                genArgs
                "Observable"
                "IObservable"
            |> Some
        | "Microsoft.FSharp.Control.IEvent`1" ->
            makeFableLibImportTypeAnnotation com ctx genArgs "Event" "IEvent"
            |> Some
        | Types.ievent2 ->
            makeFableLibImportTypeAnnotation com ctx genArgs "Event" "IEvent$2"
            |> Some
        | "Fable.Core.JS.Set`1" ->
            makeFableLibImportTypeAnnotation com ctx genArgs "Util" "ISet"
            |> Some
        | "Fable.Core.JS.Map`2" ->
            makeFableLibImportTypeAnnotation com ctx genArgs "Util" "IMap"
            |> Some
        | _ -> None

    let makeStringEnumTypeAnnotation (ent: Fable.Entity) (attArgs: obj list) =
        let rule =
            match List.tryHead attArgs with
            | Some(:? int as rule) -> enum<Core.CaseRules> (rule)
            | _ -> Core.CaseRules.LowerFirst

        ent.UnionCases
        |> List.mapToArray (fun uci ->
            match uci.CompiledName with
            | Some name -> name
            | None -> Naming.applyCaseRule rule uci.Name
            |> Literal.stringLiteral
            |> LiteralTypeAnnotation
        )
        |> UnionTypeAnnotation

    let makeErasedUnionTypeAnnotation com ctx genArgs (ent: Fable.Entity) =
        let transformSingleFieldType (uci: Fable.UnionCase) =
            List.tryHead uci.UnionCaseFields
            |> Option.map (fun fi ->
                fi.FieldType
                |> resolveInlineType genArgs
                |> makeFieldAnnotation com ctx
            )
            |> Option.defaultValue VoidTypeAnnotation

        match ent.UnionCases with
        | [ uci ] when List.isMultiple uci.UnionCaseFields ->
            uci.UnionCaseFields
            |> List.mapToArray (fun fi ->
                fi.FieldType
                |> resolveInlineType genArgs
                |> makeFieldAnnotation com ctx
            )
            |> TupleTypeAnnotation
        | [ uci ] -> transformSingleFieldType uci
        | ucis ->
            ucis
            |> List.mapToArray transformSingleFieldType
            |> UnionTypeAnnotation

    let makeTypeScriptTaggedUnionTypeAnnotation
        com
        ctx
        genArgs
        (ent: Fable.Entity)
        (attArgs: obj list)
        =
        let tag, rule =
            match attArgs with
            | (:? string as tag) :: (:? int as rule) :: _ ->
                tag, enum<Core.CaseRules> (rule)
            | (:? string as tag) :: _ -> tag, Core.CaseRules.LowerFirst
            | _ -> "kind", Core.CaseRules.LowerFirst

        ent.UnionCases
        |> List.mapToArray (fun uci ->
            let tagMember =
                let tagType =
                    match uci.CompiledName with
                    | Some name -> name
                    | None -> Naming.applyCaseRule rule uci.Name
                    |> Literal.stringLiteral
                    |> LiteralTypeAnnotation

                let prop, isComputed = Util.memberFromName tag

                AbstractMember.abstractProperty (
                    prop,
                    tagType,
                    isComputed = isComputed
                )

            match uci.UnionCaseFields with
            | [ field ] when field.Name = "Item" ->
                IntersectionTypeAnnotation
                    [|
                        field.FieldType
                        |> resolveInlineType genArgs
                        |> makeFieldAnnotation com ctx
                        ObjectTypeAnnotation [| tagMember |]
                    |]
            | fields ->
                let names, types =
                    fields
                    |> List.map (fun fi -> fi.Name, fi.FieldType)
                    |> List.unzip

                makeAnonymousRecordTypeAnnotation
                    com
                    ctx
                    (List.toArray names)
                    types
                |> function
                    | ObjectTypeAnnotation members ->
                        ObjectTypeAnnotation(
                            Array.append [| tagMember |] members
                        )
                    | t -> t // Unexpected
        )
        |> UnionTypeAnnotation

    let makeEntityTypeAnnotation com ctx genArgs (ent: Fable.Entity) =
        match genArgs, ent with
        | [ genArg ], EntFullName Types.nullable ->
            makeNullableTypeAnnotation com ctx genArg

        | _, Patterns.Try (tryNativeOrFableLibraryInterface com ctx genArgs) ta ->
            ta

        | _, Patterns.Try (Lib.tryJsConstructorFor Annotation com ctx) entRef ->
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

        | _,
          Patterns.Try (Util.tryFindAnyEntAttribute [ Atts.erase
                                                      Atts.stringEnum
                                                      Atts.tsTaggedUnion ]) (attFullName,
                                                                             attArgs) when
            ent.IsFSharpUnion
            ->

            let genArgs =
                List.zip ent.GenericParameters genArgs
                |> List.map (fun (p, a) -> p.Name, a)
                |> Map

            match attFullName with
            | Atts.stringEnum -> makeStringEnumTypeAnnotation ent attArgs
            | Atts.erase -> makeErasedUnionTypeAnnotation com ctx genArgs ent
            | _ ->
                makeTypeScriptTaggedUnionTypeAnnotation
                    com
                    ctx
                    genArgs
                    ent
                    attArgs

        | _ -> AnyTypeAnnotation

    let unwrapOptionalType t =
        match t with
        | Fable.Option(t, _) when not (mustWrapOption t) -> t
        | _ -> t

    let unwrapOptionalArg com (arg: Fable.Expr) =
        match arg.Type with
        | Fable.Option(t, _) when not (mustWrapOption t) ->
            match arg with
            | Fable.Value(Fable.NewOption(Some arg, _, _), _) -> true, arg
            | Fable.Value(Fable.NewOption(None, _, _), _) ->
                true, Fable.TypeCast(arg, t)
            | _ ->
                true,
                Replacements.Util.Helper.LibCall(
                    com,
                    "Option",
                    "unwrap",
                    t,
                    [ arg ]
                )
        | _ -> false, arg

    // In TypeScript we don't need to type optional properties or arguments as Option (e.g. `{ foo?: string }` so we try to unwrap the option.
    // But in some situations this may conflict with TS type cheking, usually when we assing an Option ident directly to the field (e.g. `fun (i: int option) -> {| foo = i |})
    // If we find problems we may need to disable this, or make sure somehow the values assigned to the fields are not `Some`.
    let makeAbstractPropertyAnnotation com ctx typ =
        let isOptional, typ =
            match typ with
            | Fable.Option(genArg, _) ->
                if mustWrapOption genArg then
                    true, typ
                else
                    true, genArg
            | typ -> false, typ

        isOptional, makeFieldAnnotation com ctx typ

    let makeAnonymousRecordTypeAnnotation
        com
        ctx
        fieldNames
        fieldTypes
        : TypeAnnotation
        =
        Seq.zip fieldNames fieldTypes
        |> Seq.mapToArray (fun (name, typ) ->
            let prop, isComputed = Util.memberFromName name
            let isOptional, typ = makeAbstractPropertyAnnotation com ctx typ

            AbstractMember.abstractProperty (
                prop,
                typ,
                isComputed = isComputed,
                isOptional = isOptional
            )
        )
        |> ObjectTypeAnnotation

    let transformFunctionWithAnnotations
        (com: IBabelCompiler)
        ctx
        name
        typeParams
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        if com.IsTypeScript then
            let argTypes = args |> List.map (fun id -> id.Type)

            let scopedTypeParams, genParams =
                match typeParams with
                | Some typeParams -> ctx.ScopedTypeParams, typeParams
                | None -> Util.getTypeParameters ctx (argTypes @ [ body.Type ])

            let ctx = { ctx with ScopedTypeParams = scopedTypeParams }
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

    module UnionHelpers =
        [<Literal>]
        let CASES_SUFFIX = "_$cases"

        [<Literal>]
        let UNION_SUFFIX = "_$union"

    let IMPORT_REGEX =
        Regex("""^import\b\s*(\{?.*?\}?)\s*\bfrom\s+["'](.*?)["'](?:\s*;)?$""")

    let IMPORT_SELECTOR_REGEX = Regex(@"^(\*|\w+)(?:\s+as\s+(\w+))?$")

    let stripImports (com: IBabelCompiler) ctx r (str: string) =
        str.Split('\n')
        |> Array.skipWhile (fun line ->
            match line.Trim() with
            | "" -> true
            | Naming.Regex IMPORT_REGEX (_ :: selector :: path :: _) ->
                if selector.StartsWith("{", StringComparison.Ordinal) then
                    for selector in
                        selector.TrimStart('{').TrimEnd('}').Split(',') do
                        com.GetImportExpr(
                            ctx,
                            selector,
                            path,
                            r,
                            noMangle = true
                        )
                        |> ignore

                    true
                else
                    let selector =
                        if
                            selector.StartsWith("*", StringComparison.Ordinal)
                        then
                            selector
                        else
                            $"default as {selector}"

                    com.GetImportExpr(ctx, selector, path, r, noMangle = true)
                    |> ignore

                    true
            | _ -> false
        )
        |> String.concat "\n"

    let (|TransformExpr|) (com: IBabelCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) =
        function
        | Fable.Lambda(arg, body, _) -> Some([ arg ], body)
        | Fable.Delegate(args, body, _, []) -> Some(args, body)
        | _ -> None

    let (|Lets|_|) =
        function
        | Fable.Let(ident, value, body) -> Some([ ident, value ], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let getUniqueNameInRootScope (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.DeclarationScopes.Contains(name)
            )

        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.CurrentDeclarationScope.Contains(name)
            )

        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity
        (_com: Compiler, ctx, name, args: Fable.Ident list)
        =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        let argIds =
            args
            |> FSharp2Fable.Util.discardUnitArg
            |> List.map (fun arg ->
                getUniqueNameInDeclarationScope ctx (arg.Name + "_mut")
            )

        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds

            member _.IsRecursiveRef(e) =
                match e with
                | Fable.IdentExpr id -> name = id.Name
                | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwith $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let rec isJsStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Unresolved _
        | Fable.Value _
        | Fable.Import _
        | Fable.IdentExpr _
        | Fable.Lambda _
        | Fable.Delegate _
        | Fable.ObjectExpr _
        | Fable.Call _
        | Fable.CurriedApply _
        | Fable.Operation _
        | Fable.Get _
        | Fable.Test _ -> false

        | Fable.TypeCast(e, _) -> isJsStatement ctx preferStatement e

        | Fable.TryCatch _
        | Fable.Sequential _
        | Fable.Let _
        | Fable.LetRec _
        | Fable.Set _
        | Fable.ForLoop _
        | Fable.WhileLoop _ -> true

        | Fable.Extended(kind, _) ->
            match kind with
            | Fable.Throw _
            | Fable.Debugger -> true
            | Fable.Curry _ -> false

        // TODO: If IsJsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i, _, _) -> i.IsStatement

        | Fable.DecisionTreeSuccess(targetIndex, _, _) ->
            getDecisionTarget ctx targetIndex
            |> snd
            |> isJsStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_, targets) ->
            preferStatement
            || List.exists (snd >> (isJsStatement ctx false)) targets

        | Fable.IfThenElse(_, thenExpr, elseExpr, _) ->
            preferStatement
            || isJsStatement ctx false thenExpr
            || isJsStatement ctx false elseExpr

    let addErrorAndReturnNull
        (com: Compiler)
        (range: SourceLocation option)
        (error: string)
        =
        addError com [] range error
        Expression.nullLiteral ()

    let identAsIdent (id: Fable.Ident) =
        Identifier.identifier (id.Name, ?loc = id.Range)

    let identAsExpr (id: Fable.Ident) =
        Expression.identifier (id.Name, ?loc = id.Range)

    let thisExpr = Expression.thisExpression ()

    let ofInt i = Expression.numericLiteral (float i)

    let ofString s = Expression.stringLiteral (s)

    let memberFromNameComputeStrings
        computeStrings
        (memberName: string)
        : Expression * bool
        =
        match memberName with
        | "ToString" -> Expression.identifier ("toString"), false
        | n when n.StartsWith("Symbol.", StringComparison.Ordinal) ->
            Expression.memberExpression (
                Expression.identifier ("Symbol"),
                Expression.identifier (n[7..]),
                false
            ),
            true
        | n when Naming.hasIdentForbiddenChars n ->
            Expression.stringLiteral (n), computeStrings
        | n -> Expression.identifier (n |> sanitizeMemberName), false

    let memberFromName (memberName: string) : Expression * bool =
        memberFromNameComputeStrings false memberName

    let get r left memberName =
        let expr, isComputed = memberFromNameComputeStrings true memberName
        Expression.memberExpression (left, expr, isComputed, ?loc = r)

    let getExpr r (object: Expression) (expr: Expression) =
        let expr, isComputed =
            match expr with
            | Literal(Literal.StringLiteral(StringLiteral(value, _))) ->
                memberFromNameComputeStrings true value
            | e -> e, true

        Expression.memberExpression (object, expr, isComputed, ?loc = r)

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m :: ms -> get None expr m |> getParts ms

    // Use non-strict equality for null checks
    let makeNullCheck r isNull e =
        let op =
            if isNull then
                "=="
            else
                "!="

        BinaryExpression(e, Expression.nullLiteral (), op, r)

    let makeArray (com: IBabelCompiler) ctx exprs =
        List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
        |> Expression.arrayExpression

    let makeTypedArray
        (com: IBabelCompiler)
        ctx
        t
        kind
        (args: Fable.Expr list)
        =
        match t with
        | JS.Replacements.TypedArrayCompatible com kind jsName ->
            let args = [| makeArray com ctx args |]
            Expression.newExpression (Expression.identifier (jsName), args)
        | _ -> makeArray com ctx args

    let getArrayCons com t kind =
        match t with
        | JS.Replacements.TypedArrayCompatible com kind name ->
            Expression.identifier name
        | _ -> Expression.identifier ("Array")

    let makeArrayAllocated
        (com: IBabelCompiler)
        ctx
        typ
        kind
        (size: Fable.Expr)
        =
        let cons = getArrayCons com typ kind
        let size = com.TransformAsExpr(ctx, size)
        Expression.newExpression (cons, [| size |])

    let makeArrayFrom
        (com: IBabelCompiler)
        ctx
        typ
        kind
        (fableExpr: Fable.Expr)
        =
        match fableExpr with
        | Replacements.Util.ArrayOrListLiteral(exprs, _) ->
            makeTypedArray com ctx typ kind exprs
        | _ ->
            let cons = getArrayCons com typ kind
            let expr = com.TransformAsExpr(ctx, fableExpr)
            Expression.callExpression (get None cons "from", [| expr |])

    let makeStringArray strings =
        strings
        |> List.mapToArray (fun x -> Expression.stringLiteral (x))
        |> Expression.arrayExpression

    let makeJsObject pairs =
        pairs
        |> Seq.map (fun (name, value) ->
            let prop, isComputed = memberFromName name
            ObjectMember.objectProperty (prop, value, isComputed = isComputed)
        )
        |> Seq.toArray
        |> Expression.objectExpression

    let assign range left right =
        Expression.assignmentExpression (AssignEqual, left, right, ?loc = range)

    /// Immediately Invoked Function Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction(ctx, None, [], expr)
        // Use an arrow function in case we need to capture `this`
        Expression.callExpression (
            Expression.arrowFunctionExpression ([||], body),
            [||]
        )

    let multiVarDeclaration
        (com: IBabelCompiler)
        ctx
        kind
        (variables: (Fable.Ident * Expression option) seq)
        =
        let varDeclarators =
            // TODO: Log error if there're duplicated non-empty var declarations
            variables
            |> Seq.distinctBy (fun (id, _) -> id.Name)
            |> Seq.map (fun (id, value) ->
                let ta, tp =
                    makeTypeAnnotationWithParametersIfTypeScript
                        com
                        ctx
                        id.Type
                        value

                VariableDeclarator.variableDeclarator (
                    id.Name,
                    ?annotation = ta,
                    typeParameters = tp,
                    ?init = value,
                    ?loc = id.Range
                )
            )
            |> Seq.toArray

        Statement.variableDeclaration (kind, varDeclarators)

    let callSuper (args: Expression list) =
        Expression.callExpression (Super(None), List.toArray args)

    let callSuperAsStatement (args: Expression list) =
        ExpressionStatement(callSuper args)

    let callFunction com ctx r funcExpr genArgs (args: Expression list) =
        let genArgs = makeTypeParamInstantiationIfTypeScript com ctx genArgs

        Expression.callExpression (
            funcExpr,
            List.toArray args,
            ?typeArguments = genArgs,
            ?loc = r
        )

    let callFunctionWithThisContext r funcExpr (args: Expression list) =
        let args = thisExpr :: args |> List.toArray
        Expression.callExpression (get None funcExpr "call", args, ?loc = r)

    let emitExpression range (txt: string) args =
        EmitExpression(txt, List.toArray args, ?loc = range)

    let undefined range e =
        //        Undefined(?loc=range) :> Expression
        let e = defaultArg e (Expression.numericLiteral (0.))
        Expression.unaryExpression ("void", e, ?loc = range)

    let getTypeParameters (ctx: Context) (types: Fable.Type list) =
        let rec getGenParams =
            function
            | Fable.GenericParam(_, false, _) as p -> [ p ]
            | t -> t.Generics |> List.collect getGenParams

        let mutable scopedTypeParams = ctx.ScopedTypeParams

        let typeParams =
            types
            |> List.collect getGenParams
            |> List.filter (
                function
                | Fable.GenericParam(name = name) ->
                    if Set.contains name scopedTypeParams then
                        false
                    else
                        scopedTypeParams <- Set.add name scopedTypeParams
                        true
                | _ -> false
            )

        scopedTypeParams, typeParams

    type MemberKind =
        | ClassConstructor
        | NonAttached of funcName: string
        | Attached of isStatic: bool

    let getMemberArgsAndBody
        (com: IBabelCompiler)
        ctx
        kind
        (classEnt: Fable.Entity option)
        (info: Fable.MemberFunctionOrValue)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let funcName, args, body =
            match kind, args with
            | Attached(isStatic = false), (thisArg :: args) ->
                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if isIdentUsed thisArg.Name body then
                        let thisIdent =
                            Fable.IdentExpr { thisArg with Name = "this" }

                        let thisIdent =
                            if com.IsTypeScript then
                                match classEnt with
                                | Some ent when
                                    ent.IsFSharpUnion
                                    && List.isMultiple ent.UnionCases
                                    ->
                                    Replacements.Util.Helper.LibCall(
                                        com,
                                        "Util",
                                        "downcast",
                                        thisArg.Type,
                                        [ thisIdent ]
                                    )
                                    |> Replacements.Util.withTag "downcast"
                                | _ -> thisIdent
                            else
                                thisIdent

                        Fable.Let(thisArg, thisIdent, body)
                    else
                        body

                None, args, body
            | Attached(isStatic = true), _
            | Attached _, _ -> None, args, body
            | ClassConstructor, _ -> None, args, body
            | NonAttached funcName, _ -> Some funcName, args, body

        let ctx, typeParams =
            if com.IsTypeScript then
                let isAttached, entGenParams =
                    match classEnt with
                    | None ->
                        match info.DeclaringEntity with
                        | Some entRef ->
                            let ent = com.GetEntity(entRef)

                            false,
                            if ent.IsFSharpModule then
                                []
                            else
                                ent.GenericParameters
                        | None -> false, []
                    | Some ent -> true, ent.GenericParameters

                let scopedTypeParams =
                    List.append entGenParams info.GenericParameters
                    |> List.map (fun g -> g.Name)
                    |> set

                let declaredTypeParams =
                    if isAttached then
                        info.GenericParameters
                    else
                        entGenParams @ info.GenericParameters
                    |> List.map (fun g ->
                        Fable.GenericParam(g.Name, g.IsMeasure, g.Constraints)
                    )
                    |> Some

                { ctx with ScopedTypeParams = scopedTypeParams },
                declaredTypeParams
            else
                ctx, None

        let parameters = List.concat info.CurriedParameterGroups

        let argsWithFlags =
            match args with
            | [] -> []

            | args when info.HasSpread ->
                let args, lastArg = List.splitLast args
                let args = args |> List.map (fun a -> a, ParameterFlags())
                args @ [ lastArg, ParameterFlags(isSpread = true) ]

            | args when List.sameLength args parameters ->
                List.zip args parameters
                |> List.map (fun (a, p) ->
                    match p.Name with
                    | Some name when p.IsNamed && a.Name <> name ->
                        $"Argument {name} is marked as named but conflicts with another name in scope"
                        |> addWarning com [] a.Range
                    | _ -> ()

                    let a =
                        if p.IsOptional then
                            { a with Type = unwrapOptionalType a.Type }
                        else
                            a

                    a,
                    ParameterFlags(
                        isNamed = p.IsNamed,
                        isOptional = (p.IsOptional && com.IsTypeScript),
                        ?defVal =
                            (p.DefaultValue
                             |> Option.map (transformAsExpr com ctx))
                    )
                )

            | _ -> args |> List.map (fun a -> a, ParameterFlags())

        let args, body, returnType, typeParamDecl =
            transformFunctionWithAnnotations
                com
                ctx
                funcName
                typeParams
                (List.map fst argsWithFlags)
                body

        let args =
            if Array.isEmpty args then
                args
            else
                Seq.zip args argsWithFlags
                |> Seq.mapToArray (fun (a, (_, flags)) -> a.WithFlags(flags))

        args, body, returnType, typeParamDecl

    let getUnionCaseName (uci: Fable.UnionCase) =
        match uci.CompiledName with
        | Some cname -> cname
        | None -> uci.Name

    let getUnionExprTag (com: IBabelCompiler) ctx r (fableExpr: Fable.Expr) =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        getExpr r expr (Expression.stringLiteral ("tag"))

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | Literal(NumericLiteral(_)), _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number((Int8 | Int16 | Int32), _) ->
            Expression.binaryExpression (
                BinaryOrBitwise,
                e,
                Expression.numericLiteral (0.)
            )
        | _ -> e

    let wrapExprInBlockWithReturn e =
        BlockStatement([| Statement.returnStatement (e) |])

    let makeArrowFunctionExpression
        _name
        (args, (body: BlockStatement), returnType, typeParamDecl)
        : Expression
        =
        Expression.arrowFunctionExpression (
            args,
            body,
            ?returnType = returnType,
            ?typeParameters = typeParamDecl
        )

    let makeFunctionExpression
        name
        (args, (body: Expression), returnType, typeParamDecl)
        : Expression
        =
        let id = name |> Option.map Identifier.identifier
        let body = wrapExprInBlockWithReturn body

        Expression.functionExpression (
            args,
            body,
            ?id = id,
            ?returnType = returnType,
            ?typeParameters = typeParamDecl
        )

    let optimizeTailCall
        (com: IBabelCompiler)
        (ctx: Context)
        range
        (tc: ITailCallOpportunity)
        args
        =
        let rec checkCrossRefs tempVars allArgs =
            function
            | [] -> tempVars
            | (argId, _arg) :: rest ->
                let found =
                    allArgs
                    |> List.exists (
                        deepExists (
                            function
                            | Fable.IdentExpr i -> argId = i.Name
                            | _ -> false
                        )
                    )

                let tempVars =
                    if found then
                        let tempVarName =
                            getUniqueNameInDeclarationScope ctx (argId + "_tmp")

                        Map.add argId tempVarName tempVars
                    else
                        tempVars

                checkCrossRefs tempVars allArgs rest

        ctx.OptimizeTailCall()
        let zippedArgs = List.zip tc.Args args
        let tempVars = checkCrossRefs Map.empty args zippedArgs

        let tempVarReplacements =
            tempVars |> Map.map (fun _ v -> makeIdentExpr v)

        [|
            // First declare temp variables
            for (KeyValue(argId, tempVar)) in tempVars do
                yield
                    Statement.variableDeclaration (
                        Const,
                        tempVar,
                        init = Expression.identifier (argId)
                    )
            // Then assign argument expressions to the original argument identifiers
            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
            for (argId, arg) in zippedArgs do
                let arg = FableTransforms.replaceValues tempVarReplacements arg
                let arg = com.TransformAsExpr(ctx, arg)

                yield
                    assign None (Expression.identifier (argId)) arg
                    |> ExpressionStatement
            yield
                Statement.continueStatement (
                    Identifier.identifier (tc.Label),
                    ?loc = range
                )
        |]

    let transformImport
        (com: IBabelCompiler)
        ctx
        r
        (selector: string)
        (path: string)
        =
        let selector, parts =
            let parts = Array.toList (selector.Split('.'))
            parts.Head, parts.Tail

        com.GetImportExpr(ctx, selector, path, r) |> getParts parts

    let transformCast (com: IBabelCompiler) (ctx: Context) t e : Expression =
        match t with
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | Fable.DeclaredType(ent, _) ->
            match ent.FullName with
            | Types.ienumerableGeneric
            | Types.ienumerable ->
                match e with
                | ExprType Fable.String ->
                    // Convert to array to get 16-bit code units, see #1279
                    let e = JS.Replacements.stringToCharArray e
                    com.TransformAsExpr(ctx, e) |> Some
                | Replacements.Util.ArrayOrListLiteral(exprs, _) ->
                    makeArray com ctx exprs |> Some
                | _ -> None
            | _ -> None
            |> Option.defaultWith (fun () ->
                let jsExpr = com.TransformAsExpr(ctx, e)

                match e.Type with
                | Fable.DeclaredType(sourceEnt, _) when com.IsTypeScript ->
                    let sourceEnt = com.GetEntity(sourceEnt)
                    // Because we use a wrapper type for multi-case unions, TypeScript
                    // won't automatically cast them to implementing interfaces
                    if
                        sourceEnt.IsFSharpUnion
                        && List.isMultiple sourceEnt.UnionCases
                    then
                        AsExpression(jsExpr, makeTypeAnnotation com ctx t)
                    else
                        jsExpr
                | _ -> jsExpr
            )
        | Fable.Unit -> com.TransformAsExpr(ctx, e) |> Some |> undefined e.Range
        | _ -> com.TransformAsExpr(ctx, e)

    let transformCurry
        (com: IBabelCompiler)
        (ctx: Context)
        expr
        arity
        : Expression
        =
        com.TransformAsExpr(
            ctx,
            Replacements.Api.curryExprAtRuntime com arity expr
        )

    let transformNewUnion
        (com: IBabelCompiler)
        (ctx: Context)
        r
        (ent: Fable.Entity)
        genArgs
        (tag: int)
        values
        =
        let values = values |> List.mapToArray (transformAsExpr com ctx)

        if List.isSingle ent.UnionCases then
            let typeParamInst =
                makeTypeParamInstantiationIfTypeScript com ctx genArgs

            Expression.newExpression (
                jsConstructor com ctx ent,
                values,
                ?typeArguments = typeParamInst,
                ?loc = r
            )
        else
            let callConstructor (case: Fable.UnionCase option) =
                let tagExpr =
                    match case with
                    | Some case -> CommentedExpression(case.Name, ofInt tag)
                    | None -> ofInt tag

                let consRef = jsConstructor com ctx ent

                let typeParamInst =
                    makeTypeParamInstantiationIfTypeScript com ctx genArgs
                    |> Option.map (fun typeParams ->
                        Array.append
                            typeParams
                            [|
                                LiteralTypeAnnotation(
                                    Literal.numericLiteral (tag)
                                )
                            |]
                    )

                Expression.newExpression (
                    consRef,
                    [|
                        tagExpr
                        Expression.arrayExpression values
                    |],
                    ?typeArguments = typeParamInst,
                    ?loc = r
                )

            if com.IsTypeScript then
                match List.tryItem tag ent.UnionCases with
                | Some case ->
                    match
                        tryJsConstructorWithSuffix com ctx ent ("_" + case.Name)
                    with
                    | Some helperRef ->
                        let typeParams =
                            makeTypeParamInstantiation com ctx genArgs

                        Expression.callExpression (
                            helperRef,
                            values,
                            typeArguments = typeParams
                        )
                    | None -> callConstructor (Some case)
                | None ->
                    $"Unmatched union case tag: {tag} for {ent.FullName}"
                    |> addWarning com [] r

                    callConstructor None
            else
                callConstructor None

    let transformValue
        (com: IBabelCompiler)
        (ctx: Context)
        r
        value
        : Expression
        =
        match value with
        | Fable.BaseValue(None, _) -> Super(None)
        | Fable.BaseValue(Some boundIdent, _) -> identAsExpr boundIdent
        | Fable.ThisValue _ -> Expression.thisExpression ()
        | Fable.TypeInfo(t, tags) ->
            if com.Options.NoReflection then
                addErrorAndReturnNull com r "Reflection is disabled"
            else
                let genMap =
                    if List.contains "allow-generics" tags then
                        None
                    else
                        Some Map.empty

                transformTypeInfoFor ActualConsRef com ctx r genMap t
        | Fable.Null _t ->
            // if com.IsTypeScript
            //     let ta = makeTypeAnnotation com ctx t |> TypeAnnotation |> Some
            //     upcast Identifier("null", ?typeAnnotation=ta, ?loc=r)
            // else
            Expression.nullLiteral (?loc = r)
        | Fable.UnitConstant -> undefined r None
        | Fable.BoolConstant x -> Expression.booleanLiteral (x, ?loc = r)
        | Fable.CharConstant x ->
            Expression.stringLiteral (string<char> x, ?loc = r)
        | Fable.StringConstant x -> Expression.stringLiteral (x, ?loc = r)
        | Fable.StringTemplate(tag, parts, values) ->
            let tag = tag |> Option.map (fun e -> com.TransformAsExpr(ctx, e))

            let values =
                values |> List.mapToArray (fun e -> com.TransformAsExpr(ctx, e))

            StringTemplate(tag, List.toArray parts, values, r) |> Literal
        | Fable.NumberConstant(x, kind, _) ->
            match kind, x with
            | Decimal, (:? decimal as x) ->
                JS.Replacements.makeDecimal com r value.Type x
                |> transformAsExpr com ctx
            | BigInt, (:? bigint as x) ->
                Expression.bigintLiteral (string<bigint> x, ?loc = r)
            | Int64, (:? int64 as x) ->
                Expression.bigintLiteral (string<int64> x, ?loc = r)
            | UInt64, (:? uint64 as x) ->
                Expression.bigintLiteral (string<uint64> x, ?loc = r)
            // | Int128,  (:? System.Int128 as x) -> Expression.bigintLiteral(string x, ?loc=r)
            // | UInt128, (:? System.UInt128 as x) -> Expression.bigintLiteral(string x, ?loc=r)
            | NativeInt, (:? nativeint as x) ->
                Expression.bigintLiteral (string<nativeint> x, ?loc = r)
            | UNativeInt, (:? unativeint as x) ->
                Expression.bigintLiteral (string<unativeint> x, ?loc = r)
            | Int8, (:? int8 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | UInt8, (:? uint8 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | Int16, (:? int16 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | UInt16, (:? uint16 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | Int32, (:? int32 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | UInt32, (:? uint32 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            // | Float16, (:? System.Half as x) -> Expression.numericLiteral(float x, ?loc=r)
            | Float32, (:? float32 as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | Float64, (:? float as x) ->
                Expression.numericLiteral (float x, ?loc = r)
            | _, (:? char as x) -> Expression.numericLiteral (float x, ?loc = r)
            | _ ->
                addErrorAndReturnNull
                    com
                    r
                    $"Numeric literal is not supported: {x.GetType().FullName}"
        | Fable.RegexConstant(source, flags) ->
            Expression.regExpLiteral (source, flags, ?loc = r)
        | Fable.NewArray(newKind, typ, kind) ->
            match newKind with
            | Fable.ArrayValues values -> makeTypedArray com ctx typ kind values
            | Fable.ArrayAlloc size -> makeArrayAllocated com ctx typ kind size
            | Fable.ArrayFrom expr -> makeArrayFrom com ctx typ kind expr
        | Fable.NewTuple(vals, _) ->
            let tup = makeArray com ctx vals

            if com.IsTypeScript then
                AsExpression(tup, makeTypeAnnotation com ctx value.Type)
            else
                tup
        // | Fable.NewList (headAndTail, _) when List.contains "FABLE_LIBRARY" com.Options.Define ->
        //     makeList com ctx r headAndTail
        // Optimization for bundle size: compile list literals as List.ofArray
        | Fable.NewList(headAndTail, typ) ->
            let rec getItems acc =
                function
                | None -> List.rev acc, None
                | Some(head, Fable.Value(Fable.NewList(tail, _), _)) ->
                    getItems (head :: acc) tail
                | Some(head, tail) -> List.rev (head :: acc), Some tail

            match getItems [] headAndTail with
            | [], None -> libCall com ctx r "List" "empty" [ typ ] []
            | [ TransformExpr com ctx expr ], None ->
                libCall com ctx r "List" "singleton" [] [ expr ]
            | exprs, None ->
                [ makeArray com ctx exprs ]
                |> libCall com ctx r "List" "ofArray" []
            | [ TransformExpr com ctx head ], Some(TransformExpr com ctx tail) ->
                libCall
                    com
                    ctx
                    r
                    "List"
                    "cons"
                    []
                    [
                        head
                        tail
                    ]
            | exprs, Some(TransformExpr com ctx tail) ->
                [
                    makeArray com ctx exprs
                    tail
                ]
                |> libCall com ctx r "List" "ofArrayWithTail" []
        | Fable.NewOption(value, t, _) ->
            match value with
            | Some(TransformExpr com ctx e) ->
                if mustWrapOption t then
                    libCall com ctx r "Option" "some" [] [ e ]
                else
                    e
            | None -> undefined r None
        | Fable.NewRecord(values, ent, genArgs) ->
            let ent = com.GetEntity(ent)

            let values =
                List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values

            let consRef = ent |> jsConstructor com ctx

            let typeParamInst =
                if com.IsTypeScript && (ent.FullName = Types.refCell) then
                    makeTypeParamInstantiation com ctx genArgs |> Some
                else
                    None

            Expression.newExpression (
                consRef,
                values,
                ?typeArguments = typeParamInst,
                ?loc = r
            )
        | Fable.NewAnonymousRecord(values, fieldNames, _genArgs, _isStruct) ->
            let values =
                List.mapToArray
                    (unwrapOptionalArg com >> snd >> transformAsExpr com ctx)
                    values

            Array.zip fieldNames values |> makeJsObject
        | Fable.NewUnion(values, tag, entRef, genArgs) ->
            let ent = com.GetEntity(entRef)
            transformNewUnion com ctx r ent genArgs tag values

    let enumerableThisToIterator com ctx =
        let enumerator =
            libCall
                com
                ctx
                None
                "Util"
                "getEnumerator"
                []
                [ Expression.identifier ("this") ]

        BlockStatement(
            [|
                Statement.returnStatement (
                    libCall com ctx None "Util" "toIterator" [] [ enumerator ]
                )
            |]
        )

    let extractSuperClassFromBaseCall
        (com: IBabelCompiler)
        (ctx: Context)
        (baseType: Fable.DeclaredType option)
        baseCall
        =
        match baseCall, baseType with
        | Some(Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr =
                match com.Options.Language, baseType, baseRef with
                | TypeScript, Some d, _ ->
                    Fable.DeclaredType(d.Entity, d.GenericArgs)
                    |> makeTypeAnnotation com ctx
                    |> SuperType
                | TypeScript, None, Fable.IdentExpr id ->
                    makeTypeAnnotation com ctx id.Type |> SuperType
                | _ -> transformAsExpr com ctx baseRef |> SuperExpression

            let args =
                info.MemberRef
                |> Option.bind com.TryGetMember
                |> transformCallArgs com ctx info

            Some(baseExpr, args)
        | Some(Fable.Value _), Some baseType ->
            // let baseEnt = com.GetEntity(baseType.Entity)
            // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
            // let entityType = FSharp2Fable.Util.getEntityType baseEnt
            // let baseRefId = makeTypedIdent entityType entityName
            // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
            // Some (baseExpr, []) // default base constructor
            let range = baseCall |> Option.bind (fun x -> x.Range)

            $"Ignoring base call for %s{baseType.Entity.FullName}"
            |> addWarning com [] range

            None
        | Some _, _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)

            "Unexpected base call expression, please report"
            |> addError com [] range

            None
        | None, _ -> None

    let transformObjectExpr
        (com: IBabelCompiler)
        ctx
        t
        (members: Fable.ObjectExprMember list)
        baseCall
        : Expression
        =

        let members =
            members
            |> List.map (fun memb -> memb, com.GetMember(memb.MemberRef))

        // Optimization: Object literals with getters and setters are very slow in V8
        // so use a class expression instead. See https://github.com/fable-compiler/Fable/pull/2165#issuecomment-695835444
        let compileAsClass =
            (Option.isSome baseCall, members)
            ||> List.fold (fun compileAsClass (memb, info) ->
                compileAsClass
                || (not memb.IsMangled
                    && (info.IsSetter
                        || (info.IsGetter && canHaveSideEffects memb.Body)))
            )

        let members =
            members
            |> List.collect (fun (memb, info) ->
                let ent =
                    info.DeclaringEntity
                    |> Option.bind (fun e -> com.TryGetEntity(e))

                let prop, isComputed = memberFromName memb.Name

                let makeMethod kind =
                    let args = memb.Args

                    let isOptional, body =
                        match kind with
                        | ObjectGetter -> unwrapOptionalArg com memb.Body
                        | _ -> false, memb.Body

                    let args, body, returnType, typeParams =
                        getMemberArgsAndBody
                            com
                            ctx
                            (Attached(isStatic = false))
                            ent
                            info
                            args
                            body

                    let returnType =
                        if isOptional then
                            returnType
                            |> Option.map (fun t ->
                                UnionTypeAnnotation
                                    [|
                                        t
                                        UndefinedTypeAnnotation
                                    |]
                            )
                        else
                            returnType

                    ObjectMember.objectMethod (
                        kind,
                        prop,
                        args,
                        body,
                        isComputed = isComputed,
                        ?returnType = returnType,
                        ?typeParameters = typeParams
                    )

                // If compileAsClass is false, it means getters don't have side effects
                // and can be compiled as object fields (see condition above)
                if
                    not memb.IsMangled
                    && (info.IsValue || (not compileAsClass && info.IsGetter))
                then
                    let _, body = unwrapOptionalArg com memb.Body

                    [
                        ObjectMember.objectProperty (
                            prop,
                            com.TransformAsExpr(ctx, body),
                            isComputed = isComputed
                        )
                    ]

                elif not memb.IsMangled && info.IsGetter then
                    [ makeMethod ObjectGetter ]

                elif not memb.IsMangled && info.IsSetter then
                    [ makeMethod ObjectSetter ]

                elif
                    info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator"
                then
                    let method = makeMethod ObjectMeth

                    let iterator =
                        let prop, isComputed = memberFromName "Symbol.iterator"
                        let body = enumerableThisToIterator com ctx

                        ObjectMember.objectMethod (
                            ObjectMeth,
                            prop,
                            [||],
                            body,
                            isComputed = isComputed
                        )

                    [
                        method
                        iterator
                    ]

                else
                    [ makeMethod ObjectMeth ]
            )

        if not compileAsClass then
            let expr = Expression.objectExpression (List.toArray members)

            match t with
            | Fable.DeclaredType(ent, _) when
                com.IsTypeScript && ent.FullName = Types.ienumerableGeneric
                ->
                AsExpression(expr, makeTypeAnnotation com ctx t)
            | _ -> expr
        else
            let classMembers =
                members
                |> List.choose (
                    function
                    | ObjectProperty(key, value, isComputed, doc) ->
                        ClassMember.classProperty (
                            key,
                            value,
                            isComputed = isComputed,
                            ?doc = doc
                        )
                        |> Some
                    | ObjectMethod(kind,
                                   key,
                                   parameters,
                                   body,
                                   isComputed,
                                   returnType,
                                   typeParameters,
                                   _,
                                   doc) ->
                        let kind =
                            match kind with
                            | ObjectGetter -> ClassGetter(key, isComputed)
                            | ObjectSetter -> ClassSetter(key, isComputed)
                            | _ -> ClassFunction(key, isComputed)

                        ClassMember.classMethod (
                            kind,
                            parameters,
                            body,
                            ?returnType = returnType,
                            typeParameters = typeParameters,
                            ?doc = doc
                        )
                        |> Some
                )

            let baseExpr, classMembers =
                baseCall
                |> extractSuperClassFromBaseCall com ctx None
                |> Option.map (fun (baseExpr, baseArgs) ->
                    let consBody =
                        BlockStatement([| callSuperAsStatement baseArgs |])

                    let cons =
                        ClassMember.classMethod (
                            ClassPrimaryConstructor [||],
                            [||],
                            consBody
                        )

                    Some baseExpr, cons :: classMembers
                )
                |> Option.defaultValue (None, classMembers)

            let classExpr =
                Expression.classExpression (
                    List.toArray classMembers,
                    ?superClass = baseExpr
                )

            Expression.newExpression (classExpr, [||])

    let transformCallArgs
        (com: IBabelCompiler)
        ctx
        (callInfo: Fable.CallInfo)
        (memberInfo: Fable.MemberFunctionOrValue option)
        =

        let args =
            FSharp2Fable.Util.dropUnitCallArg
                callInfo.Args
                callInfo.SignatureArgTypes

        let paramsInfo = Option.map getParamsInfo memberInfo

        let args =
            match paramsInfo with
            | Some i when List.sameLength args i.Parameters ->
                List.zip args i.Parameters
                |> List.map (fun (a, i) ->
                    if i.IsOptional then
                        unwrapOptionalArg com a |> snd
                    else
                        a
                )
            | _ -> args

        let args, objArg =
            paramsInfo
            |> Option.map (splitNamedArgs args)
            |> function
                | None -> args, None
                | Some(args, []) ->
                    // Detect if the method has a ParamObject attribute
                    // If yes and no argument is passed, pass an empty object
                    // See https://github.com/fable-compiler/Fable/issues/3480
                    match callInfo.MemberRef with
                    | Some(Fable.MemberRef(_, info)) ->
                        let hasParamObjectAttribute =
                            info.Attributes
                            |> Seq.tryFind (fun attr ->
                                attr.Entity.FullName = Atts.paramObject
                            )
                            |> Option.isSome

                        if hasParamObjectAttribute then
                            args, Some(makeJsObject [])
                        else
                            args, None
                    | _ ->
                        // Here detect empty named args
                        args, None
                | Some(args, namedArgs) ->
                    let objArg =
                        namedArgs
                        |> List.choose (fun (p, v) ->
                            match p.Name, v with
                            | Some k,
                              Fable.Value(Fable.NewOption(value, _, _), _) ->
                                value |> Option.map (fun v -> k, v)
                            | Some k, v -> Some(k, v)
                            | None, _ -> None
                        )
                        |> List.map (fun (k, v) ->
                            k, com.TransformAsExpr(ctx, v)
                        )
                        |> makeJsObject

                    args, Some objArg

        let hasSpread =
            paramsInfo
            |> Option.map (fun i -> i.HasSpread)
            |> Option.defaultValue false

        let args =
            if hasSpread then
                match List.rev args with
                | [] -> []
                | (Replacements.Util.ArrayOrListLiteral(spreadArgs, _)) :: rest ->
                    let rest =
                        List.rev rest
                        |> List.map (fun e -> com.TransformAsExpr(ctx, e))

                    rest
                    @ (List.map
                        (fun e -> com.TransformAsExpr(ctx, e))
                        spreadArgs)
                | last :: rest ->
                    let rest =
                        List.rev rest
                        |> List.map (fun e -> com.TransformAsExpr(ctx, e))

                    rest
                    @ [
                        Expression.spreadElement (
                            com.TransformAsExpr(ctx, last)
                        )
                    ]
            else
                List.map (fun e -> com.TransformAsExpr(ctx, e)) args

        match objArg with
        | None -> args
        | Some objArg -> args @ [ objArg ]

    let resolveExpr t strategy babelExpr : Statement =
        match strategy with
        | None
        | Some ReturnUnit -> ExpressionStatement(babelExpr)
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return ->
            Statement.returnStatement (
                wrapIntExpression t babelExpr,
                ?loc = babelExpr.Location
            )
        | Some(Assign left) ->
            ExpressionStatement(assign babelExpr.Location left babelExpr)
        | Some(Target left) ->
            ExpressionStatement(
                assign
                    babelExpr.Location
                    (left |> Expression.Identifier)
                    babelExpr
            )

    let transformOperation com ctx range opKind : Expression =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            Expression.unaryExpression (op, expr, ?loc = range)

        | Fable.Binary(op, left, right) ->
            match op, left, right with
            | (BinaryEqual | BinaryUnequal), e1, e2 ->
                match e1, e2 with
                | Fable.Value(Fable.Null _, _), e
                | e, Fable.Value(Fable.Null _, _) ->
                    com.TransformAsExpr(ctx, e)
                    |> makeNullCheck range (op = BinaryEqual)
                | ExprType(Fable.MetaType), _ ->
                    let e =
                        Replacements.Util.Helper.LibCall(
                            com,
                            "Reflection",
                            "equals",
                            Fable.Boolean,
                            [
                                e1
                                e2
                            ],
                            ?loc = range
                        )

                    let e =
                        if op = BinaryEqual then
                            e
                        else
                            makeUnOp None Fable.Boolean e UnaryNot

                    transformAsExpr com ctx e
                | TransformExpr com ctx left, TransformExpr com ctx right ->
                    Expression.binaryExpression (op, left, right, ?loc = range)

            | _, TransformExpr com ctx left, TransformExpr com ctx right ->
                Expression.binaryExpression (op, left, right, ?loc = range)

        | Fable.Logical(op,
                        TransformExpr com ctx left,
                        TransformExpr com ctx right) ->
            Expression.logicalExpression (left, op, right, ?loc = range)

    let transformEmit (com: IBabelCompiler) ctx range (info: Fable.EmitInfo) =
        let macro = stripImports com ctx range info.Macro
        let info = info.CallInfo

        let thisArg =
            info.ThisArg
            |> Option.map (fun e -> com.TransformAsExpr(ctx, e))
            |> Option.toList

        info.MemberRef
        |> Option.bind com.TryGetMember
        |> transformCallArgs com ctx info
        |> List.append thisArg
        |> emitExpression range macro

    let transformJsxProps (com: IBabelCompiler) props =
        (Some([], []), props)
        ||> List.fold (fun propsAndChildren prop ->
            match propsAndChildren, prop with
            | None, _ -> None
            | Some(props, children),
              Fable.Value(Fable.NewTuple([ StringConst key; value ], _), _) ->
                if key = "children" then
                    match value with
                    | Replacements.Util.ArrayOrListLiteral(children, _) ->
                        Some(props, children)
                    | value -> Some(props, [ value ])
                else
                    Some((key, value) :: props, children)
            | Some _, e ->
                addError
                    com
                    []
                    e.Range
                    "Cannot detect JSX prop key at compile time"

                None
        )

    let transformJsxEl (com: IBabelCompiler) ctx componentOrTag props =
        match transformJsxProps com props with
        | None -> Expression.nullLiteral ()
        | Some(props, children) ->
            let componentOrTag = transformAsExpr com ctx componentOrTag

            let children =
                children
                |> List.map (transformAsExpr com ctx)
                |> function
                    // Because of call optimizations, it may happen a list has been transformed to an array in JS
                    | [ ArrayExpression(children, _) ] -> Array.toList children
                    | children -> children

            let props =
                props
                |> List.rev
                |> List.map (fun (k, v) -> k, transformAsExpr com ctx v)

            Expression.jsxElement (componentOrTag, props, children)

    let transformJsxCall
        (com: IBabelCompiler)
        ctx
        callee
        (args: Fable.Expr list)
        (info: Fable.MemberFunctionOrValue)
        =
        let names =
            info.CurriedParameterGroups
            |> List.concat
            |> List.choose (fun p -> p.Name)

        let props =
            List.zipSafe names args
            |> List.map (fun (key, value) ->
                Fable.Value(
                    Fable.NewTuple(
                        [
                            Fable.Value(Fable.StringConstant key, None)
                            value
                        ],
                        false
                    ),
                    None
                )
            )

        transformJsxEl com ctx callee props

    let optimizeCall
        (com: IBabelCompiler)
        ctx
        range
        typ
        callee
        (callInfo: Fable.CallInfo)
        =
        // Try to optimize some patterns after FableTransforms
        match callInfo.Tags, callInfo.Args with
        | Fable.Tags.Contains "downcast", [ e ] ->
            let e = transformAsExpr com ctx e

            if com.IsTypeScript then
                let typ = makeTypeAnnotation com ctx typ
                AsExpression(e, typ) |> Some
            else
                Some e
        | Fable.Tags.Contains "array", [ maybeList ] ->
            match maybeList with
            | Replacements.Util.ArrayOrListLiteral(vals, _) ->
                Fable.Value(
                    Fable.NewArray(
                        Fable.ArrayValues vals,
                        Fable.Any,
                        Fable.MutableArray
                    ),
                    range
                )
                |> transformAsExpr com ctx
                |> Some
            | Fable.Call(Fable.Import({
                                          Selector = "toList"
                                          Path = Naming.EndsWith "/Seq.js" _
                                          Kind = Fable.LibraryImport _
                                      },
                                      _,
                                      _),
                         callInfo,
                         _,
                         _) ->
                List.head callInfo.Args
                |> Replacements.Util.toArray range typ
                |> transformAsExpr com ctx
                |> Some
            | _ -> None
        | Fable.Tags.Contains "pojo", keyValueList :: caseRule :: _ ->
            JS.Replacements.makePojo com (Some caseRule) keyValueList
            |> Option.map (transformAsExpr com ctx)
        | Fable.Tags.Contains "pojo", keyValueList :: _ ->
            JS.Replacements.makePojo com None keyValueList
            |> Option.map (transformAsExpr com ctx)
        | Fable.Tags.Contains "jsx",
          componentOrTag :: Replacements.Util.ArrayOrListLiteral(props, _) :: _ ->
            transformJsxEl com ctx componentOrTag props |> Some
        | Fable.Tags.Contains "jsx", _ ->
            "Expecting a static list or array literal (no generator) for JSX props"
            |> addErrorAndReturnNull com range
            |> Some
        | Fable.Tags.Contains "jsx-template", args ->
            match args with
            | StringConst template :: _ ->
                let template = stripImports com ctx range template
                Expression.jsxTemplate (template) |> Some
            | MaybeCasted(Fable.Value(Fable.StringTemplate(_, parts, values), _)) :: _ ->
                let parts =
                    match parts with
                    | head :: parts ->
                        (stripImports com ctx range head) :: parts
                    | parts -> parts

                let values = values |> List.mapToArray (transformAsExpr com ctx)
                Expression.jsxTemplate (List.toArray parts, values) |> Some
            | _ ->
                "Expecting a string literal or interpolation without formatting"
                |> addErrorAndReturnNull com range
                |> Some
        | _ -> None

    let transformCall
        (com: IBabelCompiler)
        ctx
        range
        typ
        callee
        (callInfo: Fable.CallInfo)
        =
        // Try to optimize some patterns after FableTransforms
        match optimizeCall com ctx range typ callee callInfo with
        | Some e -> e
        | None ->
            match callInfo.MemberRef |> Option.bind com.TryGetMember with
            | Some memberInfo when
                hasAttribute Atts.jsxComponent memberInfo.Attributes
                ->
                transformJsxCall com ctx callee callInfo.Args memberInfo
            | memberInfo ->
                let callee = com.TransformAsExpr(ctx, callee)
                let args = transformCallArgs com ctx callInfo memberInfo

                match callInfo.ThisArg with
                | None when List.contains "new" callInfo.Tags ->
                    let typeParamInst =
                        match typ with
                        | Fable.DeclaredType(_entRef, genArgs) ->
                            makeTypeParamInstantiationIfTypeScript
                                com
                                ctx
                                genArgs
                        | _ -> None

                    Expression.newExpression (
                        callee,
                        List.toArray args,
                        ?typeArguments = typeParamInst,
                        ?loc = range
                    )
                | None ->
                    callFunction com ctx range callee callInfo.GenericArgs args
                | Some(TransformExpr com ctx thisArg) ->
                    callFunction
                        com
                        ctx
                        range
                        callee
                        callInfo.GenericArgs
                        (thisArg :: args)

    let transformCurriedApply
        com
        ctx
        range
        (TransformExpr com ctx applied)
        args
        =
        (applied, args)
        ||> List.fold (fun expr arg ->
            match arg with
            // TODO: If arg type is unit but it's an expression with potential
            // side-effects, we need to extract it and execute it before the call
            | Fable.Value(Fable.UnitConstant, _) -> []
            | Fable.IdentExpr ident when ident.Type = Fable.Unit -> []
            | TransformExpr com ctx arg -> [ arg ]
            |> callFunction com ctx range expr []
        )

    let transformCallAsStatements
        com
        ctx
        range
        t
        returnStrategy
        callee
        callInfo
        =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args
            + (if Option.isSome i.ThisArg then
                   1
               else
                   0)
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return | ReturnUnit), Some tc when
            tc.IsRecursiveRef(callee) && argsLen callInfo = List.length tc.Args
            ->
            let args =
                match callInfo.ThisArg with
                | Some thisArg -> thisArg :: callInfo.Args
                | None -> callInfo.Args

            optimizeTailCall com ctx range tc args
        | _ ->
            [|
                transformCall com ctx range t callee callInfo
                |> resolveExpr t returnStrategy
            |]

    let transformCurriedApplyAsStatements
        com
        ctx
        range
        t
        returnStrategy
        callee
        args
        =
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return | ReturnUnit), Some tc when
            tc.IsRecursiveRef(callee) && List.sameLength args tc.Args
            ->
            optimizeTailCall com ctx range tc args
        | _ ->
            [|
                transformCurriedApply com ctx range callee args
                |> resolveExpr t returnStrategy
            |]

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr : BlockStatement =
        com.TransformAsStatements(ctx, ret, expr) |> BlockStatement

    let transformTryCatch com ctx r returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }

        let handler =
            catch
            |> Option.map (fun (param: Fable.Ident, body) ->
                let ta = makeTypeAnnotationIfTypeScript com ctx Fable.Any None // intentionally set catch type to 'any'

                CatchClause.catchClause (
                    param.Name,
                    ?annotation = ta,
                    body = transformBlock com ctx returnStrategy body
                )
            )

        let finalizer = finalizer |> Option.map (transformBlock com ctx None)

        [|
            Statement.tryStatement (
                transformBlock com ctx returnStrategy body,
                ?handler = handler,
                ?finalizer = finalizer,
                ?loc = r
            )
        |]

    let rec transformIfStatement
        (com: IBabelCompiler)
        ctx
        r
        ret
        guardExpr
        thenStmnt
        elseStmnt
        =
        match com.TransformAsExpr(ctx, guardExpr) with
        // This optimization is already in FableTransforms so we can remove it
        // or try to check if the value is JS truthy or falsy
        | Literal(BooleanLiteral(value = value)) ->
            let e =
                if value then
                    thenStmnt
                else
                    elseStmnt

            com.TransformAsStatements(ctx, ret, e)

        | jsGuardExpr ->
            match
                tryTransformIfThenElseAsSwitch guardExpr thenStmnt elseStmnt
            with
            | Some(evalExpr, cases, defaultCase) ->
                transformSwitch com ctx ret evalExpr cases (Some defaultCase)
            | _ ->
                let thenStmnt = transformBlock com ctx ret thenStmnt

                match com.TransformAsStatements(ctx, ret, elseStmnt) with
                | [||] ->
                    Statement.ifStatement (jsGuardExpr, thenStmnt, ?loc = r)
                | [| elseStmnt |] ->
                    Statement.ifStatement (
                        jsGuardExpr,
                        thenStmnt,
                        elseStmnt,
                        ?loc = r
                    )
                | statements ->
                    Statement.ifStatement (
                        jsGuardExpr,
                        thenStmnt,
                        Statement.blockStatement (statements),
                        ?loc = r
                    )
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
                | Fable.Value(Fable.BaseValue(_, t), r) ->
                    Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr

            let expr = com.TransformAsExpr(ctx, fableExpr)
            get range expr info.Name

        | Fable.ListHead ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
            libCall
                com
                ctx
                range
                "List"
                "head"
                []
                [ com.TransformAsExpr(ctx, fableExpr) ]

        | Fable.ListTail ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
            libCall
                com
                ctx
                range
                "List"
                "tail"
                []
                [ com.TransformAsExpr(ctx, fableExpr) ]

        | Fable.TupleIndex index ->
            match fableExpr with
            // TODO: Check the erased expressions don't have side effects?
            | Fable.Value(Fable.NewTuple(exprs, _), _) ->
                com.TransformAsExpr(ctx, List.item index exprs)
            | TransformExpr com ctx expr -> getExpr range expr (ofInt index)

        | Fable.OptionValue ->
            let expr = com.TransformAsExpr(ctx, fableExpr)

            if mustWrapOption typ || com.IsTypeScript then
                libCall com ctx None "Option" "value" [] [ expr ]
            else
                expr

        | Fable.UnionTag -> getUnionExprTag com ctx range fableExpr

        | Fable.UnionField info ->
            let expr = com.TransformAsExpr(ctx, fableExpr)

            let expr =
                if com.IsTypeScript then
                    match fableExpr with
                    | Fable.IdentExpr _ -> expr
                    | _ ->
                        // If this is not an ident, chances are TypeScript cannot infer
                        // the actual case, so we use a cast to prevent errors
                        let ent = com.GetEntity(info.Entity)

                        if List.isSingle ent.UnionCases then
                            expr
                        else
                            match
                                Lib.tryJsConstructorFor
                                    ActualConsRef
                                    com
                                    ctx
                                    ent
                            with
                            | Some(Expression.Identifier(id)) ->
                                let typeParams =
                                    makeTypeParamInstantiation
                                        com
                                        ctx
                                        info.GenericArgs

                                let typeParams =
                                    Array.append
                                        typeParams
                                        [|
                                            LiteralTypeAnnotation(
                                                Literal.numericLiteral (
                                                    info.CaseIndex
                                                )
                                            )
                                        |]

                                AsExpression(
                                    expr,
                                    AliasTypeAnnotation(id, typeParams)
                                )
                            | _ -> expr
                else
                    expr

            getExpr
                range
                (getExpr None expr (Expression.stringLiteral ("fields")))
                (ofInt info.FieldIndex)

    let transformSet
        (com: IBabelCompiler)
        ctx
        range
        fableExpr
        typ
        (value: Fable.Expr)
        kind
        =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        let value = com.TransformAsExpr(ctx, value) |> wrapIntExpression typ

        let ret =
            match kind with
            | Fable.ValueSet -> expr
            | Fable.ExprSet(TransformExpr com ctx e) -> getExpr None expr e
            | Fable.FieldSet(fieldName) -> get None expr fieldName

        assign range ret value

    let transformBindingExprBody
        (com: IBabelCompiler)
        (ctx: Context)
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
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

    let transformBindingAsExpr
        (com: IBabelCompiler)
        ctx
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
        transformBindingExprBody com ctx var value
        |> assign var.Range (identAsExpr var)

    let transformBindingAsStatements
        (com: IBabelCompiler)
        ctx
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
        if isJsStatement ctx false value then
            let ta, tp =
                makeTypeAnnotationWithParametersIfTypeScript
                    com
                    ctx
                    var.Type
                    None

            let decl =
                Statement.variableDeclaration (
                    Let,
                    var.Name,
                    ?annotation = ta,
                    typeParameters = tp,
                    ?loc = var.Range
                )

            let body =
                com.TransformAsStatements(
                    ctx,
                    Some(Assign(identAsExpr var)),
                    value
                )

            Array.append [| decl |] body
        else
            let value = transformBindingExprBody com ctx var value

            let ta, tp =
                makeTypeAnnotationWithParametersIfTypeScript
                    com
                    ctx
                    var.Type
                    (Some value)

            let kind =
                if var.IsMutable then
                    Let
                else
                    Const

            [|
                Statement.variableDeclaration (
                    kind,
                    var.Name,
                    ?annotation = ta,
                    typeParameters = tp,
                    init = value,
                    ?loc = var.Range
                )
            |]

    let transformUnionCaseTag (com: IBabelCompiler) range typ tag =
        let caseName =
            match typ with
            | Fable.DeclaredType(entRef, _) when com.IsTypeScript ->
                let ent = com.GetEntity(entRef)

                match List.tryItem tag ent.UnionCases with
                | Some case -> Some case.Name
                | None ->
                    $"Unmatched union case tag: {tag} for {ent.FullName}"
                    |> addWarning com [] range

                    None
            | _ -> None

        match caseName with
        | Some name -> CommentedExpression(name, ofInt tag)
        | None -> ofInt tag

    let transformTest (com: IBabelCompiler) ctx range kind expr : Expression =
        match kind with
        | Fable.TypeTest t -> transformTypeTest com ctx range expr t
        | Fable.OptionTest isSome ->
            com.TransformAsExpr(ctx, expr) |> makeNullCheck range (not isSome)
        | Fable.ListTest nonEmpty ->
            let expr = com.TransformAsExpr(ctx, expr)
            let expr = libCall com ctx range "List" "isEmpty" [] [ expr ]

            if nonEmpty then
                Expression.unaryExpression (UnaryNot, expr, ?loc = range)
            else
                expr
        | Fable.UnionCaseTest tag ->
            let expected = transformUnionCaseTag com range expr.Type tag
            let actual = getUnionExprTag com ctx None expr

            Expression.binaryExpression (
                BinaryEqual,
                actual,
                expected,
                ?loc = range
            )

    let transformSwitch
        (com: IBabelCompiler)
        ctx
        returnStrategy
        (evalExpr: Fable.Expr)
        cases
        defaultCase
        : Statement[]
        =
        let transformGuard =
            function
            | Fable.Test(expr, Fable.UnionCaseTest tag, range) ->
                transformUnionCaseTag com range expr.Type tag
            | TransformExpr com ctx e -> e

        let cases =
            cases
            |> List.collect (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | None, Fable.Value(Fable.UnitConstant, _), _
                | _, _, [] -> []
                | _, _, guards ->
                    let guards, lastGuard = List.splitLast guards

                    let guards =
                        guards
                        |> List.map (fun e ->
                            SwitchCase.switchCase (transformGuard e)
                        )

                    let caseBody =
                        com.TransformAsStatements(ctx, returnStrategy, expr)

                    let caseBody =
                        match returnStrategy with
                        | Some Return -> caseBody
                        | _ ->
                            Array.append
                                caseBody
                                [| Statement.breakStatement () |]

                    guards
                    @ [
                        SwitchCase.switchCase (
                            transformGuard lastGuard,
                            [| Statement.blockStatement (caseBody) |]
                        )
                    ]
            )

        let cases = cases |> List.toArray
        let switchGuard = transformAsExpr com ctx evalExpr

        let defaultCase =
            defaultCase
            |> Option.map (transformAsStatements com ctx returnStrategy)

        match cases, defaultCase with
        | [||], Some defaultCase when not (canHaveSideEffects evalExpr) ->
            defaultCase
        | cases, Some defaultCase ->
            let cases =
                Array.append
                    cases
                    [|
                        SwitchCase.switchCase (
                            body = [| Statement.blockStatement (defaultCase) |]
                        )
                    |]

            [| Statement.switchStatement (switchGuard, cases) |]
        | cases, None -> [| Statement.switchStatement (switchGuard, cases) |]

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then
            []
        elif List.sameLength idents values then
            List.zip idents values
        else
            failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBoundValues
        (com: IBabelCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues

        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr) :: bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements
                )

            let target = FableTransforms.replaceValues replacements target
            target, List.rev bindings
        else
            target, identsAndValues

    let transformDecisionTreeSuccessAsExpr
        (com: IBabelCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let target, bindings =
            getDecisionTargetAndBoundValues com ctx targetIndex boundValues

        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target =
                List.rev bindings
                |> List.fold (fun e (i, v) -> Fable.Let(i, v, e)) target

            com.TransformAsExpr(ctx, target)

    let transformDecisionTreeSuccessAsStatements
        (com: IBabelCompiler)
        (ctx: Context)
        returnStrategy
        targetIndex
        boundValues
        : Statement[]
        =
        match returnStrategy with
        | Some(Target targetId) ->
            let idents, _ = getDecisionTarget ctx targetIndex

            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
                    assign None (identAsExpr id) value |> ExpressionStatement
                )

            if System.String.IsNullOrEmpty targetId.Name then
                assignments
            else
                let targetAssignment =
                    assign
                        None
                        (targetId |> Expression.Identifier)
                        (ofInt targetIndex)
                    |> ExpressionStatement

                Array.append [| targetAssignment |] assignments
        | ret ->
            let target, bindings =
                getDecisionTargetAndBoundValues com ctx targetIndex boundValues

            let bindings =
                bindings
                |> Seq.collect (fun (i, v) ->
                    transformBindingAsStatements com ctx i v
                )
                |> Seq.toArray

            Array.append bindings (com.TransformAsStatements(ctx, ret, target))

    let tryTransformIfThenElseAsSwitch
        guardExpr
        thenExpr
        elseExpr
        : (Fable.Expr * (Fable.Expr list * Fable.Expr) list * Fable.Expr) option
        =
        let (|Equals|_|) =
            function
            | Fable.Operation(Fable.Binary(BinaryEqual, left, right), _, _, _) ->
                match left, right with
                | _,
                  Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _),
                              _) -> Some(left, right)
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _),
                              _),
                  _ -> Some(right, left)
                | _ -> None
            | Fable.Test(expr, Fable.UnionCaseTest _, _) as right ->
                let evalExpr =
                    Fable.Get(
                        expr,
                        Fable.UnionTag,
                        Fable.Number(Int32, Fable.NumberInfo.Empty),
                        None
                    )

                Some(evalExpr, right)
            | _ -> None

        let rec sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2 -> i1.Name = i2.Name
            | Fable.Get(e1, Fable.UnionTag, _, _),
              Fable.Get(e2, Fable.UnionTag, _, _)
            | Fable.Get(e1, Fable.ListHead, _, _),
              Fable.Get(e2, Fable.ListHead, _, _)
            | Fable.Get(e1, Fable.ListTail, _, _),
              Fable.Get(e2, Fable.ListTail, _, _)
            | Fable.Get(e1, Fable.OptionValue, _, _),
              Fable.Get(e2, Fable.OptionValue, _, _) -> sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.TupleIndex i1, _, _),
              Fable.Get(e2, Fable.TupleIndex i2, _, _) ->
                i1 = i2 && sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.FieldGet f1, _, _),
              Fable.Get(e2, Fable.FieldGet f2, _, _) ->
                f1.Name = f2.Name && sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.UnionField f1, _, _),
              Fable.Get(e2, Fable.UnionField f2, _, _) ->
                f1.CaseIndex = f2.CaseIndex
                && f1.FieldIndex = f2.FieldIndex
                && sameEvalExprs e1 e2
            | _ -> false

        let rec checkInner cases evalExpr =
            function
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               thenExpr,
                               elseExpr,
                               _) when sameEvalExprs evalExpr evalExpr2 ->
                checkInner ((caseExpr, thenExpr) :: cases) evalExpr elseExpr
            | defaultCase when List.isMultiple cases ->
                Some(evalExpr, List.rev cases, defaultCase)
            | _ -> None

        match guardExpr with
        | Equals(evalExpr, caseExpr) ->
            match checkInner [ caseExpr, thenExpr ] evalExpr elseExpr with
            | Some(evalExpr, cases, defaultCase) ->
                let cases = groupSwitchCases cases defaultCase
                Some(evalExpr, cases, defaultCase)
            | None -> None
        | _ -> None

    let tryTransformAsSwitch =
        function
        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, _) ->
            tryTransformIfThenElseAsSwitch guardExpr thenExpr elseExpr
        | _ -> None

    let transformDecisionTreeAsExpr
        (com: IBabelCompiler)
        (ctx: Context)
        targets
        expr
        : Expression
        =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let groupSwitchCases (cases: (Fable.Expr * Fable.Expr) list) defaultCase =
        let canBeGrouped, cannotBeGrouped =
            cases
            |> List.partition (
                function
                | _, Fable.DecisionTreeSuccess(_, [], _) -> true
                | _ -> false
            )

        let grouped =
            canBeGrouped
            |> List.groupBy (
                function
                | _, Fable.DecisionTreeSuccess(idx, _, _) -> idx
                | _ -> failwith "unexpected group candidate"
            )
            |> List.map (fun (_, cases) ->
                let caseExprs = cases |> List.map fst
                caseExprs, List.head cases |> snd
            )

        let cases =
            if grouped |> List.exists (fst >> List.isMultiple) then
                grouped @ List.map (fun (e, b) -> [ e ], b) cannotBeGrouped
            else
                List.map (fun (e, b) -> [ e ], b) cases

        match defaultCase with
        // Remove cases that can be grouped with the default branch, see #2357
        | Fable.DecisionTreeSuccess(defaultIndex, [], _) ->
            cases
            |> List.filter (
                function
                | _, Fable.DecisionTreeSuccess(idx, [], _) ->
                    idx <> defaultIndex
                | _ -> true
            )
        | _ -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int, int>) =
            function
            | Fable.DecisionTreeSuccess(idx, _, _) ->
                let count = Map.tryFind idx targetRefs |> Option.defaultValue 0
                Map.add idx (count + 1) targetRefs
            | Fable.Let(_, _, body) -> List.fold findSuccess targetRefs [ body ]
            | Fable.IfThenElse(_cond, thenExpr, elseExpr, _) ->
                List.fold
                    findSuccess
                    targetRefs
                    [
                        thenExpr
                        elseExpr
                    ]
            // | Fable.LetRec(_, body) -> List.fold findSuccess targetRefs [body]
            // | Fable.Sequential exprs -> exprs |> List.tryLast |> Option.toList |> List.fold findSuccess targetRefs
            // | Fable.TryCatch(body, catch, _finalizer, _) -> body::(catch |> Option.map snd |> Option.toList) |> List.fold findSuccess targetRefs
            | _ -> targetRefs

        findSuccess Map.empty expr
        |> Seq.chooseToList (fun kv ->
            if kv.Value > 1 then
                Some kv.Key
            else
                None
        )

    /// When several branches share target, first get the target index and bound values
    /// and then add a switch to execute the actual targets
    let transformDecisionTreeWithExtraSwitch
        (com: IBabelCompiler)
        ctx
        returnStrategy
        (targets: (Fable.Ident list * Fable.Expr) list)
        treeExpr
        =
        // Declare target and bound idents
        let targetId =
            getUniqueNameInDeclarationScope ctx "matchResult"
            |> makeTypedIdent (Fable.Number(Int32, Fable.NumberInfo.Empty))

        let boundIdents =
            targets
            |> List.collect (fun (idents, _) ->
                idents |> List.map (fun id -> id, None)
            )

        // Transform targets as switch
        let singleCase, switch2 =
            let ctx =
                { ctx with
                    ForcedIdents =
                        boundIdents |> List.map (fun (id, _) -> id.Name) |> set
                }

            let cases =
                targets
                |> List.mapi (fun i (_, target) -> [ makeIntConst i ], target)

            match cases with
            | [ (_, caseBody) ] ->
                true, transformAsStatements com ctx returnStrategy caseBody
            | cases ->
                let cases, defaultCase =
                    match returnStrategy with
                    | None
                    | Some ReturnUnit -> cases, None
                    | _ ->
                        let cases, (_, defaultCase) = List.splitLast cases
                        cases, Some defaultCase

                false,
                transformSwitch
                    com
                    ctx
                    returnStrategy
                    (targetId |> Fable.IdentExpr)
                    cases
                    defaultCase

        let targetId, multiVarDecl =
            if singleCase then
                { targetId with Name = "" },
                multiVarDeclaration com ctx Let boundIdents
            else
                targetId,
                multiVarDeclaration
                    com
                    ctx
                    Let
                    ((targetId, None) :: boundIdents)

        // Transform decision tree
        let targetAssign = Target(identAsIdent targetId)
        let ctx = { ctx with DecisionTargets = targets }

        let decisionTree =
            com.TransformAsStatements(ctx, Some targetAssign, treeExpr)

        [|
            yield multiVarDecl
            yield! decisionTree
            yield! switch2
        |]

    let transformDecisionTreeAsStatements
        (com: IBabelCompiler)
        (ctx: Context)
        returnStrategy
        (targets: (Fable.Ident list * Fable.Expr) list)
        (treeExpr: Fable.Expr)
        : Statement[]
        =

        let doesNotNeedExtraSwitch cases defaultCase =
            (Some Map.empty, defaultCase :: (cases |> List.map snd))
            ||> List.fold (fun map case ->
                match map, case with
                | Some map, Fable.DecisionTreeSuccess(_, [], _) -> Some map
                | Some map, Fable.DecisionTreeSuccess(idx, _, _) ->
                    map
                    |> Map.change idx (fun i -> (defaultArg i 0) + 1 |> Some)
                    |> Some
                | _ -> None
            )
            |> function
                | Some map -> Map.forall (fun _ count -> count = 1) map
                | _ -> false

        match getTargetsWithMultipleReferences treeExpr, treeExpr with
        | [], _ ->
            let ctx = { ctx with DecisionTargets = targets }
            com.TransformAsStatements(ctx, returnStrategy, treeExpr)

        // If we can compile as switch and there are no bound values, we don't need an extra switch
        | _, Patterns.Try tryTransformAsSwitch (evalExpr, cases, defaultCase) when
            doesNotNeedExtraSwitch cases defaultCase
            ->
            let ctx = { ctx with DecisionTargets = targets }

            transformSwitch
                com
                ctx
                returnStrategy
                evalExpr
                cases
                (Some defaultCase)

        | _ ->
            transformDecisionTreeWithExtraSwitch
                com
                ctx
                returnStrategy
                targets
                treeExpr

    let transformIdent (com: IBabelCompiler) ctx id =
        let e = identAsExpr id

        if com.IsTypeScript && ctx.ForcedIdents.Contains id.Name then
            Expression.unaryExpression (UnaryNot, e, isSuffix = true)
        else
            e

    let rec transformAsExpr
        (com: IBabelCompiler)
        ctx
        (expr: Fable.Expr)
        : Expression
        =
        match expr with
        | Fable.Unresolved(_, _, r) ->
            addErrorAndReturnNull com r "Unexpected unresolved expression"

        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> transformIdent com ctx id

        | Fable.Import({
                           Selector = selector
                           Path = path
                       },
                       _,
                       r) -> transformImport com ctx r selector path

        | Fable.Test(expr, kind, range) -> transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformFunctionWithAnnotations com ctx name None [ arg ] body
            |> makeArrowFunctionExpression name

        | Fable.Delegate(args, body, name, tags) ->
            if List.contains "not-arrow" tags then
                let id = name |> Option.map Identifier.identifier

                let args, body, returnType, typeParamDecl =
                    transformFunctionWithAnnotations com ctx name None args body

                Expression.functionExpression (
                    args,
                    body,
                    ?id = id,
                    ?returnType = returnType,
                    ?typeParameters = typeParamDecl
                )
            else
                transformFunctionWithAnnotations com ctx name None args body
                |> makeArrowFunctionExpression name

        | Fable.ObjectExpr(members, _, baseCall) ->
            transformObjectExpr com ctx expr.Type members baseCall

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
                           TransformExpr com ctx elseExpr,
                           r) ->
            Expression.conditionalExpression (
                guardExpr,
                thenExpr,
                elseExpr,
                ?loc = r
            )

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            if ctx.HoistVars [ ident ] then
                let assignment = transformBindingAsExpr com ctx ident value

                Expression.sequenceExpression (
                    [|
                        assignment
                        com.TransformAsExpr(ctx, body)
                    |]
                )
            else
                iife com ctx expr

        | Fable.LetRec(bindings, body) ->
            if ctx.HoistVars(List.map fst bindings) then
                let values =
                    bindings
                    |> List.mapToArray (fun (id, value) ->
                        transformBindingAsExpr com ctx id value
                    )

                Expression.sequenceExpression (
                    Array.append values [| com.TransformAsExpr(ctx, body) |]
                )
            else
                iife com ctx expr

        | Fable.Sequential exprs ->
            List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
            |> Expression.sequenceExpression

        | Fable.Emit(info, _, range) ->
            if info.IsStatement then
                iife com ctx expr
            else
                transformEmit com ctx range info

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.WhileLoop _
        | Fable.ForLoop _
        | Fable.TryCatch _ -> iife com ctx expr

        | Fable.Extended(instruction, _) ->
            match instruction with
            | Fable.Curry(e, arity) -> transformCurry com ctx e arity
            | Fable.Throw _
            | Fable.Debugger -> iife com ctx expr

    let rec transformAsStatements
        (com: IBabelCompiler)
        ctx
        returnStrategy
        (expr: Fable.Expr)
        : Statement array
        =
        match expr with
        | Fable.Unresolved(_, _, r) ->
            addError com [] r "Unexpected unresolved expression"
            [||]

        | Fable.Extended(kind, r) ->
            match kind with
            | Fable.Curry(e, arity) ->
                [|
                    transformCurry com ctx e arity
                    |> resolveExpr e.Type returnStrategy
                |]
            | Fable.Throw(Some(TransformExpr com ctx e), _) ->
                [| Statement.throwStatement (e, ?loc = r) |]
            | Fable.Throw(None, _) ->
                [|
                    Statement.throwStatement (
                        Expression.nullLiteral (),
                        ?loc = r
                    )
                |]
            | Fable.Debugger -> [| Statement.debuggerStatement (?loc = r) |]

        | Fable.TypeCast(e, t) ->
            [| transformCast com ctx t e |> resolveExpr t returnStrategy |]

        | Fable.Value(kind, r) ->
            [|
                transformValue com ctx r kind
                |> resolveExpr kind.Type returnStrategy
            |]

        | Fable.IdentExpr id ->
            [|
                transformIdent com ctx id |> resolveExpr id.Type returnStrategy
            |]

        | Fable.Import({
                           Selector = selector
                           Path = path
                       },
                       t,
                       r) ->
            [|
                transformImport com ctx r selector path
                |> resolveExpr t returnStrategy
            |]

        | Fable.Test(expr, kind, range) ->
            [|
                transformTest com ctx range kind expr
                |> resolveExpr Fable.Boolean returnStrategy
            |]

        | Fable.Lambda(arg, body, name) ->
            [|
                transformFunctionWithAnnotations com ctx name None [ arg ] body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy
            |]

        | Fable.Delegate(args, body, name, _) ->
            [|
                transformFunctionWithAnnotations com ctx name None args body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy
            |]

        | Fable.ObjectExpr(members, t, baseCall) ->
            [|
                transformObjectExpr com ctx expr.Type members baseCall
                |> resolveExpr t returnStrategy
            |]

        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements
                com
                ctx
                range
                typ
                returnStrategy
                callee
                info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApplyAsStatements
                com
                ctx
                range
                typ
                returnStrategy
                callee
                args

        | Fable.Emit(info, t, range) ->
            let e = transformEmit com ctx range info

            if info.IsStatement then
                [| ExpressionStatement(e) |] // Ignore the return strategy
            else
                [| resolveExpr t returnStrategy e |]

        | Fable.Operation(kind, _, t, range) ->
            [|
                transformOperation com ctx range kind
                |> resolveExpr t returnStrategy
            |]

        | Fable.Get(expr, kind, t, range) ->
            [|
                transformGet com ctx range t expr kind
                |> resolveExpr t returnStrategy
            |]

        | Fable.Let(ident, value, body) ->
            let binding = transformBindingAsStatements com ctx ident value

            Array.append
                binding
                (transformAsStatements com ctx returnStrategy body)

        | Fable.LetRec(bindings, body) ->
            let bindings =
                bindings
                |> Seq.collect (fun (i, v) ->
                    transformBindingAsStatements com ctx i v
                )
                |> Seq.toArray

            Array.append
                bindings
                (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, kind, typ, value, range) ->
            [|
                transformSet com ctx range expr typ value kind
                |> resolveExpr expr.Type returnStrategy
            |]

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            let asStatement =
                match returnStrategy with
                | None
                | Some ReturnUnit -> true
                | Some(Target _) -> true // Compile as statement so values can be bound
                | Some(Assign _) ->
                    (isJsStatement ctx false thenExpr)
                    || (isJsStatement ctx false elseExpr)
                | Some Return ->
                    Option.isSome ctx.TailCallOpportunity
                    || (isJsStatement ctx false thenExpr)
                    || (isJsStatement ctx false elseExpr)

            if asStatement then
                transformIfStatement
                    com
                    ctx
                    r
                    returnStrategy
                    guardExpr
                    thenExpr
                    elseExpr
            else
                let guardExpr' = transformAsExpr com ctx guardExpr
                let thenExpr' = transformAsExpr com ctx thenExpr
                let elseExpr' = transformAsExpr com ctx elseExpr

                [|
                    Expression.conditionalExpression (
                        guardExpr',
                        thenExpr',
                        elseExpr',
                        ?loc = r
                    )
                    |> resolveExpr thenExpr.Type returnStrategy
                |]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1

            statements
            |> List.mapiToArray (fun i statement ->
                let ret =
                    if i < lasti then
                        None
                    else
                        returnStrategy

                com.TransformAsStatements(ctx, ret, statement)
            )
            |> Array.concat

        | Fable.TryCatch(body, catch, finalizer, r) ->
            transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsStatements
                com
                ctx
                returnStrategy
                targets
                expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsStatements
                com
                ctx
                returnStrategy
                idx
                boundValues

        | Fable.WhileLoop(TransformExpr com ctx guard, body, range) ->
            [|
                Statement.whileStatement (
                    guard,
                    transformBlock com ctx None body,
                    ?loc = range
                )
            |]

        | Fable.ForLoop(var,
                        TransformExpr com ctx start,
                        TransformExpr com ctx limit,
                        body,
                        isUp,
                        range) ->
            let op1, op2 =
                if isUp then
                    BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                else
                    BinaryOperator.BinaryGreaterOrEqual,
                    UpdateOperator.UpdateMinus

            [|
                Statement.forStatement (
                    transformBlock com ctx None body,
                    VariableDeclaration.variableDeclaration (
                        Let,
                        var.Name,
                        init = start,
                        ?annotation =
                            makeTypeAnnotationIfTypeScript
                                com
                                ctx
                                var.Type
                                (Some start)
                    ),
                    Expression.binaryExpression (op1, identAsExpr var, limit),
                    Expression.updateExpression (op2, false, identAsExpr var),
                    ?loc = range
                )
            |]

    let transformFunction
        com
        ctx
        name
        (args: Fable.Ident list)
        (body: Fable.Expr)
        : Parameter array * BlockStatement
        =
        let tailcallChance =
            Option.map
                (fun name ->
                    NamedTailCallOpportunity(com, ctx, name, args)
                    :> ITailCallOpportunity
                )
                name

        let args = FSharp2Fable.Util.discardUnitArg args
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false

        let ctx =
            { ctx with
                TailCallOpportunity = tailcallChance
                HoistVars =
                    fun ids ->
                        declaredVars.AddRange(ids)
                        true
                OptimizeTailCall = fun () -> isTailCallOptimized <- true
            }

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
                        let ta =
                            makeTypeAnnotationIfTypeScript com ctx id.Type None

                        Parameter.parameter (tcArg, ?typeAnnotation = ta)
                    )

                let varDecls =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) ->
                        id, Some(Expression.identifier (tcArg))
                    )
                    |> multiVarDeclaration com ctx Const

                let body = Array.append [| varDecls |] body.Body
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body =
                    BlockStatement(
                        Array.append body [| Statement.breakStatement () |]
                    )

                let body =
                    Statement.labeledStatement (
                        Identifier.identifier (tc.Label),
                        Statement.whileStatement (
                            Expression.booleanLiteral (true),
                            body
                        )
                    )
                    |> Array.singleton
                    |> BlockStatement

                args', body
            | _ ->
                args
                |> List.map (fun a ->
                    let ta = makeTypeAnnotationIfTypeScript com ctx a.Type None
                    Parameter.parameter (a.Name, ?typeAnnotation = ta)
                ),
                body

        let body =
            if declaredVars.Count = 0 then
                body
            else
                let varDeclStatement =
                    declaredVars
                    |> Seq.map (fun v -> v, None)
                    |> multiVarDeclaration com ctx Let

                BlockStatement(Array.append [| varDeclStatement |] body.Body)

        args |> List.toArray, body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv =
            emitExpression
                None
                "typeof process === 'object' ? process.argv.slice(2) : []"
                []

        let main = Expression.callExpression (funcExpr, [| argv |])
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(emitExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        PrivateModuleDeclaration(ExpressionStatement(main))

    let asModuleDeclaration isPublic decl =
        if not isPublic then
            PrivateModuleDeclaration(decl |> Declaration)
        else
            ExportNamedDeclaration(decl)

    let declareModuleMember com ctx (expr: Expression) (info: ModuleDecl) =
        match expr with
        | ClassExpression(body,
                          _id,
                          superClass,
                          implements,
                          typeParameters,
                          _loc) ->
            Declaration.classDeclaration (
                body,
                id = Identifier.identifier (info.Name),
                ?superClass = superClass,
                typeParameters = typeParameters,
                implements = implements
            )
        | FunctionExpression(_, parameters, body, returnType, typeParameters, _) ->
            Declaration.functionDeclaration (
                parameters,
                body,
                id = Identifier.identifier (info.Name),
                ?returnType = returnType,
                typeParameters = typeParameters,
                ?doc = info.JsDoc
            )
        | _ ->
            let kind =
                if info.IsMutable then
                    Let
                else
                    Const

            let annotation =
                // Public mutable values are compiled as functions so we omit the type in those cases
                if info.IsMutable && info.IsPublic then
                    None
                else
                    makeTypeAnnotationIfTypeScript com ctx info.Type (Some expr)

            Declaration.variableDeclaration (
                kind,
                info.Name,
                init = expr,
                ?annotation = annotation
            )

        |> asModuleDeclaration info.IsPublic

    let sanitizeName fieldName =
        fieldName
        |> Naming.sanitizeIdentForbiddenChars
        |> Naming.checkJsKeywords

    let getEntityFieldsAsIdents (ent: Fable.Entity) =
        ent.FSharpFields
        |> List.map (fun field ->
            let name = sanitizeName field.Name
            let typ = field.FieldType
            { makeTypedIdent typ name with IsMutable = field.IsMutable }
        )

    let declareClassWithParams
        (com: IBabelCompiler)
        ctx
        (ent: Fable.Entity)
        entName
        (consArgs: Parameter[])
        (consArgsModifiers: AccessModifier[])
        (consBody: BlockStatement)
        (superClass: SuperClass option)
        classMembers
        typeParamDecl
        =
        let implements =
            if com.IsTypeScript then
                let isUnion = ent.IsFSharpUnion

                ent.AllInterfaces
                |> Seq.choose (fun ifc ->
                    match ifc.Entity.FullName with
                    // Discard non-generic versions of IEquatable & IComparable
                    | "System.IEquatable"
                    | Types.iStructuralEquatable
                    | Types.iequalityComparer
                    | "System.IComparable"
                    | Types.iStructuralComparable
                    | Types.ienumerable
                    | Types.ienumerator -> None
                    | Types.ienumerableGeneric ->
                        makeNativeTypeAnnotation
                            com
                            ctx
                            ifc.GenericArgs
                            "Iterable"
                        |> Some
                    | Types.ienumeratorGeneric ->
                        makeFableLibImportTypeAnnotation
                            com
                            ctx
                            ifc.GenericArgs
                            "Util"
                            "IEnumerator"
                        |> Some
                    | Types.iequatableGeneric
                    | Types.icomparableGeneric when isUnion -> None
                    | _ ->
                        com.GetEntity(ifc.Entity)
                        |> makeEntityTypeAnnotation com ctx ifc.GenericArgs
                        |> Some
                )
                |> Seq.toArray
                |> Some
            else
                None

        let classCons =
            ClassMember.classMethod (
                ClassPrimaryConstructor consArgsModifiers,
                consArgs,
                consBody
            )

        let classFields =
            if com.IsTypeScript && not ent.IsFSharpUnion then
                ent.FSharpFields
                |> List.mapToArray (fun field ->
                    let prop, isComputed = memberFromName field.Name
                    let ta = makeFieldAnnotation com ctx field.FieldType
                    // Static fields need to be initialized by static constructor
                    let am =
                        if field.IsMutable || field.IsStatic then
                            None
                        else
                            Some Readonly

                    ClassMember.classProperty (
                        prop,
                        isComputed = isComputed,
                        isStatic = field.IsStatic,
                        typeAnnotation = ta,
                        ?accessModifier = am
                    )
                )
            else
                Array.empty

        let classExpr =
            Expression.classExpression (
                [|
                    yield! classFields
                    classCons
                    yield! classMembers
                |],
                ?superClass = superClass,
                ?typeParameters = typeParamDecl,
                ?implements = implements
            )

        ModuleDecl(entName, isPublic = ent.IsPublic)
        |> declareModuleMember com ctx classExpr

    let declareClass
        (com: IBabelCompiler)
        ctx
        ent
        entName
        consArgs
        consBody
        superClass
        classMembers
        =
        if com.IsTypeScript then
            FSharp2Fable.Util.getEntityGenArgs ent
            |> makeTypeParamDecl com ctx
            |> Some
        else
            None
        |> declareClassWithParams
            com
            ctx
            ent
            entName
            consArgs
            [||]
            consBody
            superClass
            classMembers

    let declareTypeReflection
        (com: IBabelCompiler)
        ctx
        (ent: Fable.Entity)
        entName
        : ModuleDeclaration
        =
        let ta =
            if com.IsTypeScript then
                makeFableLibImportTypeAnnotation
                    com
                    ctx
                    []
                    "Reflection"
                    "TypeInfo"
                |> Some
            else
                None

        let genArgs =
            Array.init
                (ent.GenericParameters.Length)
                (fun i -> "gen" + string<int> i |> makeIdent)

        let generics = genArgs |> Array.map identAsExpr
        let body = transformReflectionInfo com ctx None ent generics

        let args =
            genArgs
            |> Array.map (fun x ->
                Parameter.parameter (x.Name, ?typeAnnotation = ta)
            )

        let returnType = ta
        let fnExpr = makeFunctionExpression None (args, body, returnType, None)

        ModuleDecl(entName + Naming.reflectionSuffix, isPublic = ent.IsPublic)
        |> declareModuleMember com ctx fnExpr

    let declareType
        (com: IBabelCompiler)
        ctx
        (ent: Fable.Entity)
        entName
        (consArgs: Parameter[])
        (consBody: BlockStatement)
        baseExpr
        classMembers
        : ModuleDeclaration list
        =
        let typeDeclaration =
            declareClass
                com
                ctx
                ent
                entName
                consArgs
                consBody
                baseExpr
                classMembers

        if com.Options.NoReflection then
            [ typeDeclaration ]
        else
            let reflectionDeclaration =
                declareTypeReflection com ctx ent entName

            [
                typeDeclaration
                reflectionDeclaration
            ]

    let hasAttribute fullName (atts: Fable.Attribute seq) =
        atts |> Seq.exists (fun att -> att.Entity.FullName = fullName)

    let hasAnyAttribute fullNames (atts: Fable.Attribute seq) =
        let fullNames = set fullNames

        atts
        |> Seq.exists (fun att -> Set.contains att.Entity.FullName fullNames)

    let tryFindAnyAttribute
        fullNames
        (atts: Fable.Attribute seq)
        : (string * obj list) option
        =
        let fullNames = set fullNames

        atts
        |> Seq.tryPick (fun att ->
            let fullName = att.Entity.FullName

            if Set.contains fullName fullNames then
                Some(fullName, att.ConstructorArgs)
            else
                None
        )

    let tryFindAnyEntAttribute
        fullNames
        (ent: Fable.Entity)
        : (string * obj list) option
        =
        tryFindAnyAttribute fullNames ent.Attributes

    let transformModuleFunction
        (com: IBabelCompiler)
        ctx
        (info: Fable.MemberFunctionOrValue)
        (membName: string)
        (args: Fable.Ident list)
        body
        =
        let isJsx = hasAttribute Atts.jsxComponent info.Attributes

        let args, body =
            match args with
            | [] -> args, body
            | [ arg ] when arg.Type = Fable.Unit -> [], body
            | _ when not isJsx -> args, body
            | _ ->
                // SolidJS requires values being accessed directly from the props object for reactivity to work properly
                // https://www.solidjs.com/guides/rendering#props
                let propsArg = makeIdent "$props"
                let propsExpr = Fable.IdentExpr propsArg

                let replacements =
                    args
                    |> List.map (fun a ->
                        a.Name, getFieldWith None a.Type propsExpr a.Name
                    )
                    |> Map

                [ propsArg ], FableTransforms.replaceValues replacements body

        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody
                com
                ctx
                (NonAttached membName)
                None
                info
                args
                body

        Expression.functionExpression (
            args,
            body,
            ?returnType = returnType,
            ?typeParameters = typeParamDecl
        )

    let transformAction (com: IBabelCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr

        let hasVarDeclarations =
            statements
            |> Array.exists (
                function
                | Declaration(Declaration.VariableDeclaration(_)) -> true
                | _ -> false
            )

        if hasVarDeclarations then
            [
                Expression.callExpression (
                    Expression.functionExpression (
                        [||],
                        BlockStatement(statements)
                    ),
                    [||]
                )
                |> ExpressionStatement
                |> PrivateModuleDeclaration
            ]
        else
            statements |> Array.mapToList (fun x -> PrivateModuleDeclaration(x))

    let transformAttachedProperty
        (com: IBabelCompiler)
        ctx
        classEnt
        (info: Fable.MemberFunctionOrValue)
        (memb: Fable.MemberDecl)
        =
        let isStatic = not info.IsInstance
        let key, isComputed = memberFromName memb.Name

        let args = memb.Args

        let kind, (isOptional, body) =
            if info.IsGetter then
                ClassGetter(key, isComputed), unwrapOptionalArg com memb.Body
            else
                ClassSetter(key, isComputed), (false, memb.Body)

        let args, body, returnType, _typeParamDecl =
            getMemberArgsAndBody
                com
                ctx
                (Attached isStatic)
                (Some classEnt)
                info
                args
                body

        let returnType =
            if isOptional then
                returnType
                |> Option.map (fun t ->
                    UnionTypeAnnotation
                        [|
                            t
                            UndefinedTypeAnnotation
                        |]
                )
            else
                returnType

        ClassMember.classMethod (
            kind,
            args,
            body,
            isStatic = isStatic,
            ?returnType = returnType
        ) //, ?typeParameters=typeParamDecl)
        |> Array.singleton

    let transformAttachedMethod
        (com: IBabelCompiler)
        ctx
        classEnt
        (info: Fable.MemberFunctionOrValue)
        (memb: Fable.MemberDecl)
        =
        let isStatic = not info.IsInstance

        let makeMethod name args body returnType typeParamDecl =
            let key, isComputed = memberFromName name

            ClassMember.classMethod (
                ClassFunction(key, isComputed),
                args,
                body,
                isStatic = isStatic,
                ?returnType = returnType,
                ?typeParameters = typeParamDecl
            )

        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody
                com
                ctx
                (Attached isStatic)
                (Some classEnt)
                info
                memb.Args
                memb.Body

        [|
            yield makeMethod memb.Name args body returnType typeParamDecl
            if
                info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator"
            then
                let returnType =
                    match returnType with
                    | Some(AliasTypeAnnotation(_, typeArguments)) ->
                        TypeAnnotation.aliasTypeAnnotation (
                            Identifier.identifier ("Iterator"),
                            typeArguments = typeArguments
                        )
                        |> Some
                    | _ -> None

                yield
                    makeMethod
                        "Symbol.iterator"
                        [||]
                        (enumerableThisToIterator com ctx)
                        returnType
                        None
        |]

    let transformUnion
        (com: IBabelCompiler)
        ctx
        (ent: Fable.Entity)
        (entName: string)
        classMembers
        =
        let isPublic = ent.IsPublic
        let tagArgName = "Tag"
        let tagArgTa = makeAliasTypeAnnotation com ctx tagArgName

        let union_cases =
            entName + UnionHelpers.CASES_SUFFIX |> Identifier.identifier

        let entParamsDecl =
            FSharp2Fable.Util.getEntityGenArgs ent |> makeTypeParamDecl com ctx

        let entParamsInst =
            entParamsDecl
            |> Array.map (fun (TypeParameter(name = name)) ->
                makeAliasTypeAnnotation com ctx name
            )

        let union_cases_alias = AliasTypeAnnotation(union_cases, entParamsInst)

        let baseExpr =
            let id = makeFableLibImportTypeId com ctx "Types" "Union"

            let typeParamInst =
                match ent.UnionCases with
                | _ when not com.IsTypeScript -> [||]
                | [ singleCase ] ->
                    [|
                        LiteralTypeAnnotation(Literal.numericLiteral (0))
                        LiteralTypeAnnotation(
                            Literal.stringLiteral (singleCase.Name)
                        )
                    |]
                | _ ->
                    [|
                        tagArgTa
                        IndexedTypeAnnotation(
                            IndexedTypeAnnotation(union_cases_alias, tagArgTa),
                            LiteralTypeAnnotation(Literal.numericLiteral (0))
                        )
                    |]

            TypeAnnotation.aliasTypeAnnotation (
                id,
                typeArguments = typeParamInst
            )
            |> SuperType
            |> Some

        let cases =
            let body =
                ent.UnionCases
                |> List.map (getUnionCaseName >> makeStrConst)
                |> makeArray com ctx
                |> Statement.returnStatement
                |> Array.singleton
                |> BlockStatement

            ClassMember.classMethod (
                ClassFunction(Expression.identifier ("cases"), false),
                [||],
                body
            )

        // Don't emit helpers for single-case unions but make constructor with typed arguments
        match ent.UnionCases with
        | [ singleCase ] ->
            let fieldAnnotations =
                if com.IsTypeScript then
                    singleCase.UnionCaseFields
                    |> List.mapToArray (fun fi ->
                        makeFieldAnnotation com ctx fi.FieldType
                    )
                else
                    [||]

            let args =
                if com.IsTypeScript then
                    Seq.zip singleCase.UnionCaseFields fieldAnnotations
                    |> Seq.mapToArray (fun (fi, ta) ->
                        Parameter.parameter (
                            sanitizeName fi.Name,
                            typeAnnotation = ta
                        )
                    )
                else
                    singleCase.UnionCaseFields
                    |> List.mapToArray (fun fi ->
                        Parameter.parameter (sanitizeName fi.Name)
                    )

            let fieldsExpr =
                args
                |> Array.map (fun a -> Expression.identifier (a.Name))
                |> Expression.arrayExpression

            let consBody =
                BlockStatement
                    [|
                        callSuperAsStatement []
                        assign
                            None
                            (get None thisExpr "tag")
                            (Expression.numericLiteral (0.))
                        |> ExpressionStatement
                        assign None (get None thisExpr "fields") fieldsExpr
                        |> ExpressionStatement
                    |]

            declareType
                com
                ctx
                ent
                entName
                args
                consBody
                baseExpr
                [|
                    if com.IsTypeScript then
                        ClassMember.classProperty (
                            Expression.identifier "tag",
                            typeAnnotation =
                                LiteralTypeAnnotation(
                                    Literal.numericLiteral (0)
                                ),
                            accessModifier = Readonly
                        )

                        ClassMember.classProperty (
                            Expression.identifier "fields",
                            typeAnnotation =
                                TupleTypeAnnotation fieldAnnotations,
                            accessModifier = Readonly
                        )
                    cases
                    yield! classMembers
                |]

        | _ when com.IsTypeScript ->
            let union_cons = entName |> Identifier.identifier

            let union_ta, union_cases_ta =
                ent.UnionCases
                |> List.mapiToArray (fun i uci ->
                    let typeParams =
                        Array.append
                            entParamsInst
                            [|
                                LiteralTypeAnnotation(
                                    Literal.numericLiteral (i)
                                )
                            |]

                    let case_ta =
                        TypeAnnotation.aliasTypeAnnotation (
                            union_cons,
                            typeParams
                        )

                    let fields_ta =
                        uci.UnionCaseFields
                        |> List.mapToArray (fun fi ->
                            makeFieldAnnotation com ctx fi.FieldType
                        )
                        |> TupleTypeAnnotation

                    case_ta,
                    AbstractMember.abstractProperty (
                        Expression.numericLiteral (i),
                        TupleTypeAnnotation
                            [|
                                LiteralTypeAnnotation(
                                    Literal.stringLiteral (uci.Name)
                                )
                                fields_ta
                            |]
                    )
                )
                |> Array.unzip

            let fieldsArgTa =
                IndexedTypeAnnotation(
                    IndexedTypeAnnotation(union_cases_alias, tagArgTa),
                    LiteralTypeAnnotation(Literal.numericLiteral (1))
                )

            let consArgs =
                [|
                    Parameter.parameter ("tag", typeAnnotation = tagArgTa)
                    Parameter.parameter ("fields", typeAnnotation = fieldsArgTa)
                |]

            let consArgsModifiers =
                [|
                    Readonly
                    Readonly
                |]

            let consBody = BlockStatement [| callSuperAsStatement [] |]
            let classMembers = Array.append [| cases |] classMembers

            let unionConsTypeParams =
                Some(
                    Array.append
                        entParamsDecl
                        [|
                            TypeParameter.typeParameter (
                                tagArgName,
                                bound = KeyofTypeAnnotation(union_cases_alias)
                            )
                        |]
                )

            [
                TypeAliasDeclaration(
                    entName + UnionHelpers.UNION_SUFFIX,
                    entParamsDecl,
                    UnionTypeAnnotation union_ta
                )
                |> asModuleDeclaration isPublic
                TypeAliasDeclaration(
                    union_cases.Name,
                    entParamsDecl,
                    ObjectTypeAnnotation union_cases_ta
                )
                |> asModuleDeclaration isPublic

                // Helpers to instantiate union
                yield!
                    ent.UnionCases
                    |> List.mapi (fun i case ->
                        let tag = Literal.numericLiteral (i)

                        let passedArgs =
                            case.UnionCaseFields
                            |> List.mapToArray (fun fi ->
                                Expression.identifier (sanitizeName fi.Name)
                            )
                            |> Expression.arrayExpression

                        let consTypeArgs =
                            Array.append
                                entParamsInst
                                [| LiteralTypeAnnotation tag |]

                        let body =
                            BlockStatement
                                [|
                                    Expression.newExpression (
                                        Expression.Identifier union_cons,
                                        [|
                                            Expression.Literal tag
                                            passedArgs
                                        |],
                                        typeArguments = consTypeArgs
                                    )
                                    |> Statement.returnStatement
                                |]

                        let parameters =
                            case.UnionCaseFields
                            |> List.mapToArray (fun fi ->
                                Parameter.parameter (
                                    sanitizeName fi.Name,
                                    typeAnnotation =
                                        makeFieldAnnotation
                                            com
                                            ctx
                                            fi.FieldType
                                )
                            )

                        let fnId =
                            entName + "_" + case.Name |> Identifier.identifier
                        // Don't use return type, TypeScript will infer it and sometimes we want to use
                        // the actual constructor type in case it implements an interface
                        // let returnType = AliasTypeAnnotation(Identifier.identifier(entName + UnionHelpers.UNION_SUFFIX), entParamsInst)
                        Declaration.functionDeclaration (
                            parameters,
                            body,
                            fnId,
                            typeParameters = entParamsDecl
                        )
                        |> asModuleDeclaration isPublic
                    )

                // Actual class
                declareClassWithParams
                    com
                    ctx
                    ent
                    union_cons.Name
                    consArgs
                    consArgsModifiers
                    consBody
                    baseExpr
                    classMembers
                    unionConsTypeParams
                if not com.Options.NoReflection then
                    declareTypeReflection com ctx ent entName
            ]

        // Multiple cases, no-TypeScript
        | _ ->
            let args =
                [|
                    Parameter.parameter ("tag")
                    Parameter.parameter ("fields")
                |]

            let body =
                BlockStatement
                    [|
                        callSuperAsStatement []
                        yield!
                            [
                                "tag"
                                "fields"
                            ]
                            |> List.map (fun name ->
                                let left = get None thisExpr name
                                let right = Expression.identifier (name)
                                assign None left right |> ExpressionStatement
                            )
                    |]

            let classMembers = Array.append [| cases |] classMembers
            declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithCompilerGeneratedConstructor
        (com: IBabelCompiler)
        ctx
        (ent: Fable.Entity)
        (entName: string)
        classMembers
        =
        let fieldIds = getEntityFieldsAsIdents ent |> List.toArray
        let args = fieldIds |> Array.map identAsExpr

        let baseExpr =
            if ent.IsFSharpExceptionDeclaration then
                libValue com ctx "Types" "FSharpException"
                |> SuperExpression
                |> Some
            elif ent.IsFSharpRecord || ent.IsValueType then
                libValue com ctx "Types" "Record" |> SuperExpression |> Some
            else
                None

        let body =
            BlockStatement(
                [|
                    if Option.isSome baseExpr then
                        yield callSuperAsStatement []
                    yield!
                        ent.FSharpFields
                        |> List.mapi (fun i field ->
                            let left = get None thisExpr field.Name

                            let right =
                                wrapIntExpression field.FieldType args[i]

                            assign None left right |> ExpressionStatement
                        )
                        |> List.toArray
                |]
            )

        let args =
            fieldIds
            |> Array.map (fun fi ->
                Parameter.parameter (
                    fi.Name,
                    ?typeAnnotation =
                        makeFieldAnnotationIfTypeScript com ctx fi.Type
                )
            )

        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithPrimaryConstructor
        (com: IBabelCompiler)
        ctx
        (classEnt: Fable.Entity)
        (classDecl: Fable.ClassDecl)
        classMembers
        (cons: Fable.MemberDecl)
        =
        let consInfo = com.GetMember(cons.MemberRef)
        let classIdent = Expression.identifier (classDecl.Name)

        let consArgs, consBody, returnType, _typeParamDecl =
            getMemberArgsAndBody
                com
                ctx
                ClassConstructor
                (Some classEnt)
                consInfo
                cons.Args
                cons.Body

        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.IsTypeScript then
                let genArgs = FSharp2Fable.Util.getEntityGenArgs classEnt

                let returnType =
                    getGenericTypeAnnotation com ctx classDecl.Name genArgs

                let typeParamDecl = makeTypeParamDecl com ctx genArgs |> Some
                Some returnType, typeParamDecl
            else
                returnType, None

        let exposedCons =
            let argExprs =
                consArgs |> Array.map (fun p -> Expression.identifier (p.Name))

            let exposedConsBody =
                Expression.newExpression (classIdent, argExprs)

            makeFunctionExpression
                None
                (consArgs, exposedConsBody, returnType, typeParamDecl)

        let baseExpr, consBody =
            classDecl.BaseCall
            |> extractSuperClassFromBaseCall com ctx classEnt.BaseType
            |> Option.orElseWith (fun () ->
                if classEnt.IsValueType then
                    Some(
                        libValue com ctx "Types" "Record" |> SuperExpression,
                        []
                    )
                else
                    None
            )
            |> Option.map (fun (baseExpr, baseArgs) ->
                let consBody =
                    consBody.Body
                    |> Array.append [| callSuperAsStatement baseArgs |]
                    |> BlockStatement

                Some baseExpr, consBody
            )
            |> Option.defaultValue (None, consBody)

        [
            yield!
                declareType
                    com
                    ctx
                    classEnt
                    classDecl.Name
                    consArgs
                    consBody
                    baseExpr
                    classMembers
            yield
                ModuleDecl(cons.Name, isPublic = consInfo.IsPublic)
                |> declareModuleMember com ctx exposedCons
        ]

    let transformInterfaceDeclaration
        (com: IBabelCompiler)
        ctx
        (decl: Fable.ClassDecl)
        (ent: Fable.Entity)
        =
        let getters, methods =
            ent.MembersFunctionsAndValues
            // It's not usual to have getters/setters in TS interfaces, so let's ignore setters
            // and compile getters as fields
            |> Seq.filter (fun info ->
                not (info.IsProperty || info.IsSetter)
                // TODO: Deal with other emit attributes like EmitMethod or EmitConstructor
                && not (hasAttribute Atts.emitAttr info.Attributes)
            )
            |> Seq.toArray
            |> Array.partition (fun info -> info.IsGetter)

        let getters =
            getters
            |> Array.map (fun info ->
                let prop, isComputed = memberFromName info.DisplayName

                let isOptional, typ =
                    makeAbstractPropertyAnnotation
                        com
                        ctx
                        info.ReturnParameter.Type

                AbstractMember.abstractProperty (
                    prop,
                    typ,
                    isComputed = isComputed,
                    isOptional = isOptional,
                    ?doc = info.XmlDoc
                )
            )

        let methods =
            methods
            |> Array.map (fun info ->
                let prop, isComputed = memberFromName info.DisplayName

                let args =
                    info.CurriedParameterGroups
                    |> List.concat
                    // |> FSharp2Fable.Util.discardUnitArg
                    |> List.toArray

                let argsLen = Array.length args

                let args =
                    args
                    |> Array.mapi (fun i a ->
                        let name = defaultArg a.Name $"arg{i}"

                        let ta =
                            if a.IsOptional then
                                unwrapOptionalType a.Type
                            else
                                a.Type
                            |> FableTransforms.uncurryType
                            |> makeTypeAnnotation com ctx

                        Parameter
                            .parameter(name, ta)
                            .WithFlags(
                                ParameterFlags(
                                    isOptional = a.IsOptional,
                                    isSpread =
                                        (i = argsLen - 1 && info.HasSpread),
                                    isNamed = a.IsNamed
                                )
                            )
                    )

                let typeParams =
                    info.GenericParameters
                    |> List.map (fun g ->
                        Fable.GenericParam(g.Name, g.IsMeasure, g.Constraints)
                    )
                    |> makeTypeParamDecl com ctx

                let returnType =
                    makeTypeAnnotation com ctx info.ReturnParameter.Type

                AbstractMember.abstractMethod (
                    ObjectMeth,
                    prop,
                    args,
                    returnType = returnType,
                    typeParameters = typeParams,
                    isComputed = isComputed,
                    ?doc = info.XmlDoc
                )
            )

        let members = Array.append getters methods

        let extends =
            ent.DeclaredInterfaces
            |> Seq.map (fun parent ->
                com.GetEntity(parent.Entity)
                |> makeEntityTypeAnnotation com ctx parent.GenericArgs
            )
            |> Seq.toArray

        let typeParameters =
            FSharp2Fable.Util.getEntityGenArgs ent |> makeTypeParamDecl com ctx

        Declaration.interfaceDeclaration (
            Identifier.identifier decl.Name,
            members,
            extends,
            typeParameters
        )
        |> asModuleDeclaration ent.IsPublic

    let transformStringEnumDeclaration
        (decl: Fable.ClassDecl)
        (ent: Fable.Entity)
        (attArgs: obj list)
        =
        let ta = makeStringEnumTypeAnnotation ent attArgs

        TypeAliasDeclaration(decl.Name, [||], ta)
        |> asModuleDeclaration ent.IsPublic

    let transformErasedUnionDeclaration
        (com: IBabelCompiler)
        ctx
        (decl: Fable.ClassDecl)
        (ent: Fable.Entity)
        =
        let ta = makeErasedUnionTypeAnnotation com ctx Map.empty ent

        let entParams =
            FSharp2Fable.Util.getEntityGenArgs ent |> makeTypeParamDecl com ctx

        TypeAliasDeclaration(decl.Name, entParams, ta)
        |> asModuleDeclaration ent.IsPublic

    let transformTypeScriptTaggedUnionDeclaration
        (com: IBabelCompiler)
        ctx
        (decl: Fable.ClassDecl)
        (ent: Fable.Entity)
        (attArgs: obj list)
        =
        let ta =
            makeTypeScriptTaggedUnionTypeAnnotation
                com
                ctx
                Map.empty
                ent
                attArgs

        let entParams =
            FSharp2Fable.Util.getEntityGenArgs ent |> makeTypeParamDecl com ctx

        TypeAliasDeclaration(decl.Name, entParams, ta)
        |> asModuleDeclaration ent.IsPublic

    let rec transformDeclaration (com: IBabelCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx =
                { ctx with
                    UsedNames =
                        { ctx.UsedNames with
                            CurrentDeclarationScope = HashSet usedNames
                        }
                }

            let result = f ctx

            ctx.UsedNames.DeclarationScopes.UnionWith(
                ctx.UsedNames.CurrentDeclarationScope
            )

            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames
            <| fun ctx -> transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames
            <| fun ctx ->
                let info = com.GetMember(decl.MemberRef)

                let valueExpr =
                    match decl.Body with
                    | body when info.IsValue ->
                        transformAsExpr com ctx body |> Some
                    // Some calls with special attributes (like React lazy or memo) can turn the surrounding function into a value
                    | Fable.Call(callee,
                                 ({
                                      ThisArg = None
                                      MemberRef = Some m
                                  } as callInfo),
                                 _,
                                 r) as body ->
                        match com.TryGetMember(m), callInfo.Args with
                        | Some m, _ when
                            hasAttribute
                                "Fable.Core.JS.RemoveSurroundingArgsAttribute"
                                m.Attributes
                            ->
                            transformAsExpr com ctx body |> Some
                        | Some m, arg :: restArgs when
                            hasAttribute
                                "Fable.Core.JS.WrapSurroundingFunctionAttribute"
                                m.Attributes
                            ->
                            let arg =
                                transformModuleFunction
                                    com
                                    ctx
                                    info
                                    decl.Name
                                    decl.Args
                                    arg

                            let callee = com.TransformAsExpr(ctx, callee)

                            let restArgs =
                                List.map
                                    (fun e -> com.TransformAsExpr(ctx, e))
                                    restArgs

                            callFunction com ctx r callee [] (arg :: restArgs)
                            |> Some
                        | _ -> None
                    | _ -> None

                let decls =
                    match valueExpr with
                    | Some value ->
                        ModuleDecl(
                            decl.Name,
                            isPublic = info.IsPublic,
                            isMutable = info.IsMutable,
                            typ = decl.Body.Type,
                            ?doc = decl.XmlDoc
                        )
                        |> declareModuleMember com ctx value
                        |> List.singleton
                    | None ->
                        let expr =
                            transformModuleFunction
                                com
                                ctx
                                info
                                decl.Name
                                decl.Args
                                decl.Body

                        if hasAttribute Atts.entryPoint info.Attributes then
                            [ declareEntryPoint com ctx expr ]
                        else
                            [
                                ModuleDecl(
                                    decl.Name,
                                    isPublic = info.IsPublic,
                                    ?doc = decl.XmlDoc
                                )
                                |> declareModuleMember com ctx expr
                            ]

                let isDefaultExport =
                    List.contains "export-default" decl.Tags
                    || (com.TryGetMember(decl.MemberRef)
                        |> Option.map (fun m ->
                            hasAttribute Atts.exportDefault m.Attributes
                        )
                        |> Option.defaultValue false)

                if List.contains "remove-declaration" decl.Tags then
                    []
                elif not isDefaultExport then
                    decls
                else
                    decls
                    @ [
                        ExportDefaultDeclaration(
                            Choice2Of2(Expression.identifier (decl.Name))
                        )
                    ]

        | Fable.ClassDeclaration decl ->
            match com.GetEntity(decl.Entity) with
            | Patterns.Try (tryFindAnyEntAttribute [ Atts.stringEnum
                                                     Atts.erase
                                                     Atts.tsTaggedUnion ]) (att,
                                                                            attArgs) as ent ->
                match com.IsTypeScript, ent.IsFSharpUnion, att with
                | true, true, Atts.stringEnum ->
                    [ transformStringEnumDeclaration decl ent attArgs ]
                | true, true, Atts.erase ->
                    [ transformErasedUnionDeclaration com ctx decl ent ]
                | true, true, Atts.tsTaggedUnion ->
                    [
                        transformTypeScriptTaggedUnionDeclaration
                            com
                            ctx
                            decl
                            ent
                            attArgs
                    ]
                | _ -> []
            | ent when ent.IsInterface ->
                if com.IsTypeScript then
                    [ transformInterfaceDeclaration com ctx decl ent ]
                else
                    []
            | ent ->
                let classMembers =
                    decl.AttachedMembers
                    |> List.toArray
                    |> Array.collect (fun memb ->
                        withCurrentScope ctx memb.UsedNames
                        <| fun ctx ->
                            memb.ImplementedSignatureRef
                            |> Option.bind (com.TryGetMember)
                            |> Option.orElseWith (fun () ->
                                com.TryGetMember(memb.MemberRef)
                            )
                            |> function
                                | None -> [||]
                                | Some info ->
                                    if
                                        not memb.IsMangled
                                        && (info.IsGetter || info.IsSetter)
                                    then
                                        transformAttachedProperty
                                            com
                                            ctx
                                            ent
                                            info
                                            memb
                                    else
                                        transformAttachedMethod
                                            com
                                            ctx
                                            ent
                                            info
                                            memb
                    )

                match decl.Constructor with
                | Some cons ->
                    withCurrentScope ctx cons.UsedNames
                    <| fun ctx ->
                        transformClassWithPrimaryConstructor
                            com
                            ctx
                            ent
                            decl
                            classMembers
                            cons
                | None ->
                    if ent.IsFSharpUnion then
                        transformUnion com ctx ent decl.Name classMembers
                    else
                        transformClassWithCompilerGeneratedConstructor
                            com
                            ctx
                            ent
                            decl.Name
                            classMembers

    let transformImports (imports: Import seq) : ModuleDeclaration list =
        let statefulImports = ResizeArray()

        imports
        |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier.identifier (localId)

                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId)
                    | "default" -> ImportDefaultSpecifier(localId)
                    | memb ->
                        ImportMemberSpecifier(
                            localId,
                            Identifier.identifier (memb)
                        )
                )

            import.Path, specifier
        )
        |> Seq.groupBy fst
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    match x with
                    | ImportNamespaceSpecifier(_) -> mems, defs, x :: alls
                    | ImportDefaultSpecifier(_) -> mems, x :: defs, alls
                    | _ -> x :: mems, defs, alls
                )
            // We used to have trouble when mixing member, default and namespace imports,
            // issue an import statement for each kind just in case
            [
                mems
                defs
                alls
            ]
            |> List.choose (
                function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(
                        List.toArray specifiers,
                        StringLiteral.stringLiteral (path)
                    )
                    |> Some
            )
            |> function
                | [] ->
                    // If there are no specifiers, this is just an import for side effects,
                    // put it after the other ones to match standard JS practices, see #2228
                    ImportDeclaration([||], StringLiteral.stringLiteral (path))
                    |> statefulImports.Add

                    []
                | decls -> decls
        )
        |> fun staticImports ->
            [
                yield! staticImports
                yield! statefulImports
            ]

    let getIdentForImport
        (com: IBabelCompiler)
        (ctx: Context)
        noMangle
        (path: string)
        (selector: string)
        =
        if System.String.IsNullOrEmpty selector then
            selector, None
        else
            let selector, alias =
                match selector with
                | Naming.Regex IMPORT_SELECTOR_REGEX (_ :: selector :: alias :: _) ->
                    let alias =
                        if alias.Length = 0 then
                            if selector = "*" || selector = "default" then
                                Path
                                    .GetFileNameWithoutExtension(path)
                                    .Replace("-", "_")
                            else
                                selector
                        else
                            alias

                    selector, alias
                | _ -> selector, selector

            let alias =
                if noMangle then
                    let noConflict = ctx.UsedNames.RootScope.Add(alias)

                    if not noConflict then
                        com.WarnOnlyOnce(
                            $"Import {alias} conflicts with existing identifier in root scope"
                        )

                    alias
                else
                    getUniqueNameInRootScope ctx alias

            selector, Some alias

module Compiler =
    open Util

    type BabelCompiler(com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()
        let isTypeScript = com.Options.Language = TypeScript

        interface IBabelCompiler with
            member _.IsTypeScript = isTypeScript

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
                    | Some localIdent -> Expression.identifier (localIdent)
                    | None -> Expression.nullLiteral ()
                | false, _ ->
                    let selector, localId =
                        getIdentForImport com ctx noMangle path selector

                    if selector = Naming.placeholder then
                        "`importMember` must be assigned to a variable"
                        |> addError com [] r

                    let i =
                        {
                            Selector = selector
                            Path = path
                            LocalIdent = localId
                        }

                    imports.Add(cachedName, i)

                    match localId with
                    | Some localId -> Expression.identifier (localId)
                    | None -> Expression.nullLiteral ()

            member _.GetAllImports() = imports.Values :> _
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e

            member bcom.TransformAsStatements(ctx, ret, e) =
                transformAsStatements bcom ctx ret e

            member bcom.TransformFunction(ctx, name, args, body) =
                transformFunction bcom ctx name args body

            member bcom.TransformImport(ctx, selector, path) =
                transformImport bcom ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.SourceFiles = com.SourceFiles
            member _.IncrementCounter() = com.IncrementCounter()

            member _.IsPrecompilingInlineFunction =
                com.IsPrecompilingInlineFunction

            member _.WillPrecompileInlineFunction(file) =
                com.WillPrecompileInlineFunction(file)

            member _.GetImplementationFile(fileName) =
                com.GetImplementationFile(fileName)

            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)

            member _.AddWatchDependency(fileName) =
                com.AddWatchDependency(fileName)

            member _.AddLog
                (
                    msg,
                    severity,
                    ?range,
                    ?fileName: string,
                    ?tag: string
                )
                =
                com.AddLog(
                    msg,
                    severity,
                    ?range = range,
                    ?fileName = fileName,
                    ?tag = tag
                )

    let makeCompiler com = BabelCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IBabelCompiler

        let declScopes =
            let hs = HashSet()

            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)

            hs

        let ctx =
            {
                File = file
                UsedNames =
                    {
                        RootScope = HashSet file.UsedNamesInRootScope
                        DeclarationScopes = declScopes
                        CurrentDeclarationScope = Unchecked.defaultof<_>
                    }
                DecisionTargets = []
                HoistVars = fun _ -> false
                TailCallOpportunity = None
                OptimizeTailCall = fun () -> ()
                ScopedTypeParams = Set.empty
                ForcedIdents = Set.empty
            }

        let rootDecls =
            List.collect (transformDeclaration com ctx) file.Declarations

        let importDecls = com.GetAllImports() |> transformImports
        let body = importDecls @ rootDecls |> List.toArray
        Program(body)
