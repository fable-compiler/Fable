module rec Fable.Transforms.Fable2Babel

open Fable
open Fable.AST
open Fable.AST.Babel
open System.Collections.Generic

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

type IBabelCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> seq<Import>
    abstract GetImportExpr: Context * selector: string * path: string -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement array
    abstract TransformImport: Context * selector:string * path:string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr
        -> (Pattern array) * Choice<BlockStatement, Expression>

// TODO: All things that depend on the library should be moved to Replacements
// to become independent of the specific implementation
module Lib =
    let libCall (com: IBabelCompiler) ctx r moduleName memberName args =
        CallExpression(com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc=r) :> Expression

    let libConsCall (com: IBabelCompiler) ctx moduleName memberName args =
        NewExpression(com.TransformImport(ctx, memberName, getLibPath com moduleName), args) :> Expression

    let libValue (com: IBabelCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryJsConstructor (com: IBabelCompiler) ctx ent =
        match Replacements.tryJsConstructor com ent with
        | Some e -> com.TransformAsExpr(ctx, e) |> Some
        | None -> None

    let jsConstructor (com: IBabelCompiler) ctx ent =
        let entRef = Replacements.jsConstructor com ent
        com.TransformAsExpr(ctx, entRef)

// TODO: This is too implementation-dependent, ideally move it to Replacements
module Reflection =
    open Lib

    let private libReflectionCall (com: IBabelCompiler) ctx r memberName args =
        libCall com ctx r "Reflection" (memberName + "_type") args

    let private asFSharpEntity (ent: Fable.Entity) =
        match ent with
        | :? FSharp2Fable.FsEnt as ent -> Some ent
        | _ -> None

    let private transformRecordReflectionInfo com ctx r (ent: Fable.Entity) generics =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = ent.FullName
        let fullnameExpr = StringLiteral fullname :> Expression
        let genMap =
            let genParamNames = ent.GenericParameters |> List.mapToArray (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map
        let fields =
            ent.FSharpFields |> Seq.map (fun fi ->
                let typeInfo = transformTypeInfo com ctx r genMap fi.FieldType
                (ArrayExpression [|StringLiteral fi.Name; typeInfo|] :> Expression))
            |> Seq.toArray
        let fields = ArrowFunctionExpression([||], ArrayExpression fields :> Expression |> Choice2Of2) :> Expression
        [|fullnameExpr; upcast ArrayExpression generics; jsConstructor com ctx ent; fields|]
        |> libReflectionCall com ctx None "record"

    let private transformUnionReflectionInfo com ctx r (ent: Fable.Entity) generics =
        let fullname = ent.FullName
        let fullnameExpr = StringLiteral fullname :> Expression
        let genMap =
            let genParamNames = ent.GenericParameters |> List.map (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map
        let cases =
            ent.UnionCases |> Seq.map (fun uci ->
                uci.UnionCaseFields |> List.mapToArray (fun fi ->
                    ArrayExpression [|
                        fi.Name |> StringLiteral :> Expression
                        transformTypeInfo com ctx r genMap fi.FieldType
                    |] :> Expression)
                |> ArrayExpression :> Expression
            ) |> Seq.toArray
        let cases = ArrowFunctionExpression([||], ArrayExpression cases :> Expression |> Choice2Of2) :> Expression
        [|fullnameExpr; upcast ArrayExpression generics; jsConstructor com ctx ent; cases|]
        |> libReflectionCall com ctx None "union"

    let transformTypeInfo (com: IBabelCompiler) ctx r (genMap: Map<string, Expression>) t: Expression =
        let primitiveTypeInfo name =
           libValue com ctx "Reflection" (name + "_type")
        let numberInfo kind =
            getNumberKindName kind
            |> primitiveTypeInfo
        let nonGenericTypeInfo fullname =
            [| StringLiteral fullname :> Expression |]
            |> libReflectionCall com ctx None "class"
        let resolveGenerics generics: Expression[] =
            generics |> Array.map (transformTypeInfo com ctx r genMap)
        let genericTypeInfo name genArgs =
            let resolved = resolveGenerics genArgs
            libReflectionCall com ctx None name resolved
        let genericEntity (ent: Fable.Entity) generics =
            let fullname = ent.FullName
            let fullnameExpr = StringLiteral fullname :> Expression
            let args = if Array.isEmpty generics then [|fullnameExpr|] else [|fullnameExpr; ArrayExpression generics :> Expression|]
            libReflectionCall com ctx None "class" args
        match t with
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
            let fullName = ent.FullName
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
                        ArrayExpression [|StringLiteral name; NumericLiteral value|] :> Expression |> Some)
                |> Seq.toArray
                |> ArrayExpression
            [|StringLiteral fullName :> Expression; numberInfo numberKind; cases :> _|]
            |> libReflectionCall com ctx None "enum"
        | Fable.Number kind ->
            numberInfo kind
        | Fable.LambdaType(argType, returnType) ->
            genericTypeInfo "lambda" [|argType; returnType|]
        | Fable.DelegateType(argTypes, returnType) ->
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
            |> libReflectionCall com ctx None "anonRecord"
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
                    || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    || FSharp2Fable.Util.isGlobalOrImportedEntity ent
                    // TODO: Get reflection info from types in precompiled libs
                    || FSharp2Fable.Util.isReplacementCandidate ent then
                    genericEntity ent generics
                else
                    let reflectionMethodExpr = FSharp2Fable.Util.entityRefWithSuffix com ent Naming.reflectionSuffix
                    let callee = com.TransformAsExpr(ctx, reflectionMethodExpr)
                    CallExpression(callee, generics) :> Expression

    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName
            [|
                yield StringLiteral fullname :> Expression
                match generics with
                | [||] -> yield Util.undefined None
                | generics -> yield ArrayExpression generics :> _
                match tryJsConstructor com ctx ent with
                | Some cons -> yield cons
                | None -> ()
            |]
            |> libReflectionCall com ctx r "class"

    let private ofString s = StringLiteral s :> Expression
    let private ofArray babelExprs = ArrayExpression(List.toArray babelExprs) :> Expression

    let rec private toTypeTester com ctx r = function
        | Fable.Regex -> Identifier "RegExp" :> Expression
        | Fable.MetaType -> libValue com ctx "Reflection" "TypeInfo"
        | Fable.LambdaType _ | Fable.DelegateType _ -> ofString "function"
        | Fable.AnonymousRecordType _ -> ofString "unknown" // Recognize shape? (it's possible in F#)
        | Fable.Any -> ofString "any"
        | Fable.Unit -> ofString "undefined"
        | Fable.Boolean -> ofString "boolean"
        | Fable.Char
        | Fable.String -> ofString "string"
        | Fable.Number _ -> ofString "number"
        | Fable.Enum _ -> ofString "number"
        | Fable.Option t -> ofArray [ofString "option"; toTypeTester com ctx r t]
        | Fable.Array t -> ofArray [ofString "array"; toTypeTester com ctx r t]
        | Fable.List t -> ofArray [ofString "list"; toTypeTester com ctx r t]
        | Fable.Tuple genArgs ->
            let genArgs = List.map (toTypeTester com ctx r) genArgs
            ofArray [ofString "tuple"; ofArray genArgs]
        | Fable.GenericParam name ->
            sprintf "Cannot resolve generic param %s for type testing, evals to true" name |> addWarning com [] r
            ofString "any"
        | Fable.DeclaredType(ent, _) when ent.IsInterface ->
            "Cannot type test interfaces, evals to false" |> addWarning com [] r
            ofString "unknown"
        | Fable.DeclaredType(ent, genArgs) ->
            match tryJsConstructor com ctx ent with
            | Some cons ->
                if not(List.isEmpty genArgs) then
                    "Generic args are ignored in type testing" |> addWarning com [] r
                cons
            | None ->
                sprintf "Cannot type test %s, evals to false" ent.FullName |> addWarning com [] r
                ofString "unknown"

    let transformTypeTest (com: IBabelCompiler) ctx range (expr': Fable.Expr) (typ: Fable.Type): Expression =
        let (|EntityFullName|) (e: Fable.Entity) = e.FullName

        let expr = com.TransformAsExpr(ctx, expr')
        match typ with
        // Special cases
        | Fable.DeclaredType(EntityFullName Types.idisposable, _) ->
            match expr' with
            | MaybeCasted(ExprType(Fable.DeclaredType(ent2, _))) when FSharp2Fable.Util.hasInterface Types.idisposable ent2 ->
                upcast BooleanLiteral true
            | _ -> libCall com ctx None "Util" "isDisposable" [|expr|]
        | Fable.DeclaredType(EntityFullName Types.ienumerable, _) ->
            libCall com ctx None "Util" "isIterable" [|expr|]
        | Fable.DeclaredType(EntityFullName Types.array, _) -> // Untyped array
            libCall com ctx None "Util" "isArrayLike" [|expr|]
        | Fable.DeclaredType(EntityFullName Types.exception_, _) ->
            libCall com ctx None "Types" "isException" [|expr|]
        | _ ->
            let typeTester = toTypeTester com ctx range typ
            libCall com ctx range "Reflection" "typeTest" [|expr; typeTester|]


// TODO: I'm trying to tell apart the code to generate annotations, but it's not a very clear distinction
// as there are many dependencies from/to the Util module below
module Annotation =
    let getEntityGenParams (ent: Fable.Entity) =
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

    let getGenericTypeAnnotation com ctx name genParams =
        let typeParamInst = makeTypeParamInst genParams
        GenericTypeAnnotation(Identifier name, ?typeParameters=typeParamInst) :> TypeAnnotationInfo
        |> TypeAnnotation |> Some

    let makeInterfaceDecl (com: IBabelCompiler) ctx (ent: Fable.Entity) (entName: string) (baseExpr: Expression option) =
        let genTypeParams = getEntityGenParams ent
        let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
        let attached = Util.getEntityExplicitInterfaceMembers com ctx ent
        let extends =
            Util.getInterfaceExtends com ctx ent
            |> Seq.toArray
            |> function [||] -> None | e -> Some e
        // Type declaration merging only works well with class declarations, not class expressions,
        // but Babel does not allow duplicate declarations (interface and class with the same name)
        // so we're adding a prefix to the interface name, which will be removed after transpiling.
        let prefix = "$INTERFACE_DECL_PREFIX$_"
        let id = Identifier(prefix + entName)
        let body = ObjectTypeAnnotation([| yield! attached |])
        let typeParamDecl = genTypeParams |> makeTypeParamDecl
        InterfaceDeclaration(id, body, ?extends_=extends, ?typeParameters=typeParamDecl)

    let typeAnnotation com ctx typ: TypeAnnotationInfo =
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
        | Fable.LambdaType _ -> Util.uncurryLambdaType typ ||> makeFunctionTypeAnnotation com ctx typ
        | Fable.DelegateType(argTypes, returnType) -> makeFunctionTypeAnnotation com ctx typ argTypes returnType
        | Fable.GenericParam name -> makeSimpleTypeAnnotation com ctx name
        | Fable.DeclaredType(ent, genArgs) ->
            makeEntityTypeAnnotation com ctx ent genArgs
        | Fable.AnonymousRecordType(fieldNames, genArgs) ->
            makeAnonymousRecordTypeAnnotation com ctx fieldNames genArgs

    let makeSimpleTypeAnnotation _com _ctx name =
        GenericTypeAnnotation(Identifier(name))
        :> TypeAnnotationInfo

    let makeGenTypeParamInst com ctx genArgs =
        match genArgs with
        | [] -> None
        | xs -> genArgs |> List.map (typeAnnotation com ctx)
                        |> List.toArray |> TypeParameterInstantiation |> Some

    let makeGenericTypeAnnotation com ctx genArgs id =
        let typeParamInst = makeGenTypeParamInst com ctx genArgs
        GenericTypeAnnotation(id, ?typeParameters=typeParamInst)
        :> TypeAnnotationInfo

    let makeNativeTypeAnnotation com ctx genArgs typeName =
        Identifier(typeName)
        |> makeGenericTypeAnnotation com ctx genArgs

    let makeImportTypeId (com: IBabelCompiler) ctx moduleName typeName =
        let expr = com.GetImportExpr(ctx, typeName, getLibPath com moduleName)
        match expr with
        | :? Identifier as id -> id
        | _ -> Identifier(typeName)

    let makeImportTypeAnnotation com ctx genArgs moduleName typeName =
        let id = makeImportTypeId com ctx moduleName typeName
        makeGenericTypeAnnotation com ctx genArgs id

    let makeNumericTypeAnnotation com ctx kind =
        let typeName = getNumberKindName kind
        makeImportTypeAnnotation com ctx [] "Int32" typeName

    let makeOptionTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Option" "Option"

    let makeTupleTypeAnnotation com ctx genArgs =
        List.map (typeAnnotation com ctx) genArgs
        |> List.toArray |> TupleTypeAnnotation
        :> TypeAnnotationInfo

    let makeArrayTypeAnnotation com ctx genArg =
        match genArg with
        | Fable.Number kind when com.Options.TypedArrays ->
            let name = getTypedArrayName com kind
            makeSimpleTypeAnnotation com ctx name
        | _ ->
            makeNativeTypeAnnotation com ctx [genArg] "Array"

    let makeListTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Types" "List"

    let makeUnionTypeAnnotation com ctx genArgs =
        List.map (typeAnnotation com ctx) genArgs
        |> List.toArray |> UnionTypeAnnotation
        :> TypeAnnotationInfo

    let makeBuiltinTypeAnnotation com ctx kind =
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
        | Replacements.FSharpChoice genArgs -> makeImportTypeAnnotation com ctx genArgs "Option" "Choice"
        | Replacements.FSharpReference genArg -> makeImportTypeAnnotation com ctx [genArg] "Types" "FSharpRef"

    let makeFunctionTypeAnnotation com ctx typ argTypes returnType =
        let funcTypeParams =
            argTypes
            |> List.mapi (fun i argType ->
                FunctionTypeParam(
                    Identifier("arg" + (string i)),
                    typeAnnotation com ctx argType))
            |> List.toArray
        let genTypeParams = Util.getGenericTypeParams (argTypes @ [returnType])
        let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
        let returnType = typeAnnotation com ctx returnType
        let typeParamDecl = makeTypeParamDecl newTypeParams
        FunctionTypeAnnotation(funcTypeParams, returnType, ?typeParameters=typeParamDecl)
        :> TypeAnnotationInfo

    let makeEntityTypeAnnotation com ctx ent genArgs =
        match ent.FullName with
        | Types.ienumerableGeneric ->
            makeNativeTypeAnnotation com ctx genArgs "Iterable"
        | Types.result ->
            makeUnionTypeAnnotation com ctx genArgs
        | entName when entName.StartsWith(Types.choiceNonGeneric) ->
            makeUnionTypeAnnotation com ctx genArgs
        | _ when ent.IsInterface ->
            upcast AnyTypeAnnotation() // TODO:
        | _ ->
            match Lib.tryJsConstructor com ctx ent with
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

    let makeAnonymousRecordTypeAnnotation com ctx fieldNames genArgs =
         upcast AnyTypeAnnotation() // TODO:

    let typedIdent (com: IBabelCompiler) ctx (id: Fable.Ident) =
        if com.Options.Typescript then
            let ta = typeAnnotation com ctx id.Type |> TypeAnnotation |> Some
            let optional = None // match id.Type with | Fable.Option _ -> Some true | _ -> None
            Identifier(id.Name, ?optional=optional, ?typeAnnotation=ta, ?loc=id.Range)
        else
            Identifier(id.Name, ?loc=id.Range)

    let transformFunctionWithAnnotations (com: IBabelCompiler) ctx name (args: Fable.Ident list) (body: Fable.Expr) =
        if com.Options.Typescript then
            let argTypes = args |> List.map (fun id -> id.Type)
            let genTypeParams = Util.getGenericTypeParams (argTypes @ [body.Type])
            let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
            let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
            let args', body' = com.TransformFunction(ctx, name, args, body)
            let returnType = TypeAnnotation(typeAnnotation com ctx body.Type) |> Some
            let typeParamDecl = makeTypeParamDecl newTypeParams
            args', body', returnType, typeParamDecl
        else
            let args', body' = com.TransformFunction(ctx, name, args, body)
            args', body', None, None


module Util =
    open Lib
    open Reflection
    open Annotation

    let (|TransformExpr|) (com: IBabelCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, _) -> Some([arg], body)
        | Fable.Delegate(args, body, _) -> Some(args, body)
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

    type NamedTailCallOpportunity(com: Compiler, ctx, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds = discardUnitArg args |> List.map (fun arg ->
            getUniqueNameInDeclarationScope ctx (arg.Name + "_mut"))
        interface ITailCallOpportunity with
            member __.Label = name
            member __.Args = argIds
            member __.IsRecursiveRef(e) =
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
        | Fable.Sequential _ | Fable.Let _ | Fable.Set _
        | Fable.ForLoop _ | Fable.WhileLoop _ -> true

        | Fable.Emit(info,_,_) -> info.IsJsStatement

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
        NullLiteral () :> Expression

    let toPattern (e: PatternExpression): Pattern =
        Choice2Of2 e

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

    let memberFromName (memberName: string): Expression * bool =
        if memberName.StartsWith("Symbol.") then
            upcast MemberExpression(Identifier "Symbol", Identifier memberName.[7..], false), true
        elif Naming.hasIdentForbiddenChars memberName then
            upcast StringLiteral(memberName), true
        else
            upcast Identifier(memberName), false

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

    let makeList com ctx headAndTail =
        match headAndTail with
        | None -> [||]
        | Some(TransformExpr com ctx head, TransformExpr com ctx tail) -> [|head; tail|]
        |> libConsCall com ctx "Types" "List"

    let makeArray (com: IBabelCompiler) ctx exprs =
        List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
        |> ArrayExpression :> Expression

    let makeTypedArray (com: IBabelCompiler) ctx typ (args: Fable.Expr list) =
        match typ with
        | Fable.Number kind when com.Options.TypedArrays ->
            let jsName = getTypedArrayName com kind
            let args =
                [| List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) args
                   |> ArrayExpression :> Expression |]
            NewExpression(Identifier jsName, args) :> Expression
        | _ ->
            makeArray com ctx args

    let makeTypedAllochedArray (com: IBabelCompiler) ctx typ (TransformExpr com ctx size) =
        match typ with
        | Fable.Number kind when com.Options.TypedArrays ->
            let jsName = getTypedArrayName com kind
            let args = [|size|]
            NewExpression(Identifier jsName, [|size|]) :> Expression
        | _ ->
            upcast NewExpression(Identifier "Array", [|size|])

    let makeStringArray strings =
        strings
        |> List.mapToArray (fun x -> StringLiteral x :> Expression)
        |> ArrayExpression :> Expression

    let makeJsObject pairs =
        pairs |> Seq.map (fun (name, value) ->
            let prop, computed = memberFromName name
            ObjectProperty(prop, value, computed_=computed) |> Choice1Of2)
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
        RestElement(toPattern var, ?typeAnnotation=var.TypeAnnotation) :> PatternNode |> Choice1Of2

    let callSuperConstructor r (args: Expression list) =
        CallExpression(Super(), List.toArray args, ?loc=r) :> Expression

    let callFunction r funcExpr (args: Expression list) =
        CallExpression(funcExpr, List.toArray args, ?loc=r) :> Expression

    let callFunctionWithThisContext r funcExpr (args: Expression list) =
        let args = (Identifier "this" :> Expression)::args |> List.toArray
        CallExpression(get None funcExpr "call", args, ?loc=r) :> Expression

    let macroExpression range (txt: string) args =
        MacroExpression(txt, List.toArray args, ?loc=range) :> Expression

    let undefined range =
//        Undefined(?loc=range) :> Expression
        UnaryExpression(UnaryVoid, NumericLiteral(0.), ?loc=range) :> Expression

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
        | Attached

    let getMemberArgsAndBody (com: IBabelCompiler) ctx kind hasSpread (args: Fable.Ident list) (body: Fable.Expr) =
        let funcName, genTypeParams, args, body =
            match kind, args with
            | Attached, (thisArg::args) ->
                let genTypeParams = Set.difference (getGenericTypeParams [thisArg.Type]) ctx.ScopedTypeParams
                let body = Fable.Let([thisArg, Fable.IdentExpr { thisArg with Name = "this" }], body)
                None, genTypeParams, args, body
            | ClassConstructor, _ -> None, ctx.ScopedTypeParams, args, body
            | NonAttached funcName, _ -> Some funcName, Set.empty, args, body
            | _ -> None, Set.empty, args, body

        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams genTypeParams }
        let args, body, returnType, typeParamDecl = transformFunctionWithAnnotations com ctx funcName args body

        let typeParamDecl =
            if com.Options.Typescript then
                makeTypeParamDecl genTypeParams |> mergeTypeParamDecls typeParamDecl
            else typeParamDecl

        let args =
            if not hasSpread then args
            else
                let args = Array.rev args
                let restEl = RestElement(Array.head args) :> PatternNode |> Choice1Of2
                Array.append [|restEl|] (Array.tail args) |> Array.rev

        let body =
            match body with
            | Choice1Of2 e -> e
            | Choice2Of2 e -> BlockStatement [|ReturnStatement(e, ?loc=e.Loc)|]

        args, body, returnType, typeParamDecl

    let getUnionCaseName (uci: Fable.UnionCase) =
        match uci.CompiledName with Some cname -> cname | None -> uci.Name

    let getUnionExprTag r expr =
        getExpr r expr (StringLiteral "tag")

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | :? NumericLiteral, _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.Enum _ ->
            BinaryExpression(BinaryOrBitwise, e, NumericLiteral(0.)) :> Expression
        | _ -> e

    let makeArrowFunctionExpression name (args, (body: Choice<BlockStatement, Expression>), returnType, typeParamDecl): Expression =
        upcast ArrowFunctionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let makeFunctionExpression name (args, (body: Choice<BlockStatement, Expression>), returnType, typeParamDecl): Expression =
        let id = name |> Option.map Identifier
        let body =
            match body with
            | Choice1Of2 body -> body
            | Choice2Of2 e -> BlockStatement [|ReturnStatement(e, ?loc=e.Loc)|]
        upcast FunctionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let optimizeTailCall (com: IBabelCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
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

    let transformImport (com: IBabelCompiler) ctx r (selector: Fable.Expr) (path: Fable.Expr) =
        match selector, path with
        | Fable.Value(Fable.StringConstant selector,_), Fable.Value(Fable.StringConstant path,_) ->
            let selector, parts =
                let parts = Array.toList(selector.Split('.'))
                parts.Head, parts.Tail
            com.GetImportExpr(ctx, selector, path)
            |> getParts parts
        | _ -> "Import expressions only accept string literals" |> addErrorAndReturnNull com r

    let transformCast (com: IBabelCompiler) (ctx: Context) t e: Expression =
        match t with
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | Fable.DeclaredType(ent,[_]) ->
            match ent.FullName, e with
            | Types.ienumerableGeneric, Replacements.ArrayOrListLiteral(exprs, _) ->
                makeArray com ctx exprs
            | _ -> com.TransformAsExpr(ctx, e)
        | _ -> com.TransformAsExpr(ctx, e)

    let transformCurry (com: IBabelCompiler) (ctx: Context) r expr arity: Expression =
        com.TransformAsExpr(ctx, Replacements.curryExprAtRuntime com arity expr)

    let transformValue (com: IBabelCompiler) (ctx: Context) r value: Expression =
        match value with
        | Fable.BaseValue(None,_) -> upcast Super()
        | Fable.BaseValue(Some boundIdent,_) -> identAsExpr boundIdent
        | Fable.ThisValue _ -> upcast ThisExpression()
        | Fable.TypeInfo t -> transformTypeInfo com ctx r Map.empty t
        | Fable.Null _t ->
            // if com.Options.typescript
            //     let ta = typeAnnotation com ctx t |> TypeAnnotation |> Some
            //     upcast Identifier("null", ?typeAnnotation=ta, ?loc=r)
            // else
                upcast NullLiteral(?loc=r)
        | Fable.UnitConstant -> undefined r
        | Fable.BoolConstant x -> upcast BooleanLiteral(x, ?loc=r)
        | Fable.CharConstant x -> upcast StringLiteral(string x, ?loc=r)
        | Fable.StringConstant x -> upcast StringLiteral(x, ?loc=r)
        | Fable.NumberConstant (x,_) ->
            if x < 0.
            // Negative numeric literals can give issues in Babel AST, see #1186
            // TODO: We don't need this when using our own printer
            then upcast UnaryExpression(UnaryMinus, NumericLiteral(x * -1.), ?loc=r)
            else upcast NumericLiteral(x, ?loc=r)
        | Fable.RegexConstant (source, flags) -> upcast RegExpLiteral(source, flags, ?loc=r)
        | Fable.NewArray (values, typ) -> makeTypedArray com ctx typ values
        | Fable.NewArrayAlloc (size, typ) -> makeTypedAllochedArray com ctx typ size
        | Fable.NewTuple vals -> makeArray com ctx vals
        // Optimization for bundle size: compile list literals as List.ofArray
        | Replacements.ListLiteral(exprs, t) ->
            match exprs with
            | [] -> makeList com ctx None
            | [expr] -> Some(expr, Fable.Value(Fable.NewList (None,t), None)) |> makeList com ctx
            | exprs -> [|makeArray com ctx exprs|] |> libCall com ctx r "List" "ofArray"
        | Fable.NewList (headAndTail, _) ->
            makeList com ctx headAndTail
        | Fable.NewOption (value, t) ->
            match value with
            | Some (TransformExpr com ctx e) ->
                if mustWrapOption t
                then libCall com ctx r "Option" "some" [|e|]
                else e
            | None -> undefined r
        | Fable.EnumConstant(x,_) ->
            com.TransformAsExpr(ctx, x)
        | Fable.NewRecord(values, ent, genArgs) ->
            let values = List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values
            let consRef = jsConstructor com ctx ent
            let typeParamInst =
                if com.Options.Typescript && (ent.FullName = Types.reference)
                then makeGenTypeParamInst com ctx genArgs
                else None
            upcast NewExpression(consRef, values, ?typeArguments=typeParamInst, ?loc=r)
        | Fable.NewAnonymousRecord(values, fieldNames, genArgs) ->
            let values = List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values
            Array.zip fieldNames values
            |> makeJsObject
            |> Array.singleton
            |> libCall com ctx r "Types" "anonRecord"
        | Fable.NewUnion(values, tag, ent, genArgs) ->
            let consRef = jsConstructor com ctx ent
            let values = List.map (fun x -> com.TransformAsExpr(ctx, x)) values
            let typeParamInst =
                if com.Options.Typescript
                then makeGenTypeParamInst com ctx genArgs
                else None
            let values = (ofInt tag)::values |> List.toArray
            upcast NewExpression(consRef, values, ?typeArguments=typeParamInst, ?loc=r)

    let enumerator2iterator com ctx =
        let enumerator = CallExpression(get None (Identifier "this") "GetEnumerator", [||]) :> Expression
        BlockStatement [|ReturnStatement(libCall com ctx None "Seq" "toIterator" [|enumerator|])|]

    let transformObjectExpr (com: IBabelCompiler) ctx (members: Fable.MemberDecl list) baseCall: Expression =
        let makeObjMethod kind prop computed hasSpread args body =
            let args, body, returnType, typeParamDecl =
                getMemberArgsAndBody com ctx Attached hasSpread args body
            ObjectMethod(kind, prop, args, body, computed_=computed,
                ?returnType=returnType, ?typeParameters=typeParamDecl) |> Choice2Of2
        let pojo =
            members |> List.collect (fun memb ->
                let info = memb.Info
                let prop, computed = memberFromName memb.Name
                if info.IsValue then
                    [ObjectProperty(prop, com.TransformAsExpr(ctx, memb.Body), computed_=computed) |> Choice1Of2]
                elif info.IsGetter then
                    [makeObjMethod ObjectGetter prop computed false memb.Args memb.Body]
                elif info.IsSetter then
                    [makeObjMethod ObjectSetter prop computed false memb.Args memb.Body]
                elif info.IsEnumerator then
                    let method = makeObjMethod ObjectMeth prop computed info.HasSpread memb.Args memb.Body
                    let iterator =
                        let prop, computed = memberFromName "Symbol.iterator"
                        let body = enumerator2iterator com ctx
                        ObjectMethod(ObjectMeth, prop, [||], body, computed_=computed) |> Choice2Of2
                    [method; iterator]
                else
                    [makeObjMethod ObjectMeth prop computed info.HasSpread memb.Args memb.Body]
            ) |> List.toArray |> ObjectExpression
        match baseCall with
        | Some(TransformExpr com ctx baseCall) ->
            libCall com ctx None "Util" "extend" [|baseCall; pojo|]
        | None -> pojo :> Expression

    let transformCallArgs (com: IBabelCompiler) ctx hasSpread args =
        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> []
        | args when hasSpread ->
            match List.rev args with
            | [] -> []
            | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
            | last::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ [SpreadElement(com.TransformAsExpr(ctx, last))]
        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr t strategy babelExpr: Statement =
        match strategy with
        | None | Some ReturnUnit -> upcast ExpressionStatement babelExpr
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return -> upcast ReturnStatement(wrapIntExpression t babelExpr, ?loc=babelExpr.Loc)
        | Some(Assign left) -> upcast ExpressionStatement(assign None left babelExpr, ?loc=babelExpr.Loc)
        | Some(Target left) -> upcast ExpressionStatement(assign None left babelExpr, ?loc=babelExpr.Loc)

    let transformOperation com ctx range opKind: Expression =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            upcast UnaryExpression (op, expr, ?loc=range)

        | Fable.Binary(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast BinaryExpression (op, left, right, ?loc=range)

        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast LogicalExpression (op, left, right, ?loc=range)

    let transformEmit com ctx range (info: Fable.EmitInfo) =
        transformCallArgs com ctx false info.Args
        |> macroExpression range info.Macro

    let transformCall com ctx range callee (callInfo: Fable.CallInfo) =
        let args = transformCallArgs com ctx callInfo.HasSpread callInfo.Args
        match callee, callInfo.ThisArg with
        | TransformExpr com ctx callee, None when callInfo.IsJsConstructor ->
            NewExpression(callee, List.toArray args, ?loc=range) :> Expression
        | TransformExpr com ctx callee, Some(TransformExpr com ctx thisArg) ->
            callFunction range callee (thisArg::args)
        | TransformExpr com ctx callee, None ->
            callFunction range callee args

    let transformCurriedApply com ctx range (TransformExpr com ctx applied) args =
        match transformCallArgs com ctx false args with
        | [] -> callFunction range applied []
        | args -> (applied, args) ||> List.fold (fun e arg -> callFunction range e [arg])

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
            [|transformCall com ctx range callee callInfo |> resolveExpr t returnStrategy|]

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
        let fableExpr =
            match fableExpr with
            // If we're accessing a virtual member with default implementation (see #701)
            // from base class, we can use `super` in JS so we don't need the bound this arg
            | Fable.Value(Fable.BaseValue(_,t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
            | _ -> fableExpr
        let expr = com.TransformAsExpr(ctx, fableExpr)
        match getKind with
        | Fable.ByKey(Fable.ExprKey(TransformExpr com ctx prop)) -> getExpr range expr prop
        | Fable.ByKey(Fable.FieldKey field) -> get range expr field.Name
        | Fable.ListHead -> get range expr "head"
        | Fable.ListTail -> get range expr "tail"
        | Fable.TupleIndex index -> getExpr range expr (ofInt index)
        | Fable.OptionValue ->
            if mustWrapOption typ || com.Options.Typescript
            then libCall com ctx None "Option" "value" [|expr|]
            else expr
        | Fable.UnionTag -> getUnionExprTag range expr
        | Fable.UnionField(idx, _) ->
            getExpr range (getExpr None expr (StringLiteral "fields")) (ofInt idx)

    let transformSet (com: IBabelCompiler) ctx range var (value: Fable.Expr) setKind =
        let var = com.TransformAsExpr(ctx, var)
        let value = com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type
        let var =
            match setKind with
            | None -> var
            | Some(Fable.FieldKey fi) -> get None var fi.Name
            | Some(Fable.ExprKey(TransformExpr com ctx e)) -> getExpr None var e
        assign range var value

    let transformBindingExprBody (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        // Check imports with name placeholder
        | Fable.Import({ Selector = Fable.Value(Fable.StringConstant Naming.placeholder,_); Path = path }, _, r) ->
            transformImport com ctx r (makeStrConst var.Name) path
        | Function(_,Fable.Import({ Selector = Fable.Value(Fable.StringConstant Naming.placeholder,_); Path = path }, _, r)) ->
            transformImport com ctx r (makeStrConst var.Name) path
        | Function(args, body) ->
            let name = Some var.Name
            transformFunctionWithAnnotations com ctx name args body
            |> makeArrowFunctionExpression name
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

    let transformTest (com: IBabelCompiler) ctx range kind expr: Expression =
        match kind with
        | Fable.TypeTest t ->
            transformTypeTest com ctx range expr t
        | Fable.OptionTest nonEmpty ->
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            upcast BinaryExpression(op, com.TransformAsExpr(ctx, expr), NullLiteral(), ?loc=range)
        | Fable.ListTest nonEmpty ->
            let expr = com.TransformAsExpr(ctx, expr)
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            upcast BinaryExpression(op, get None expr "tail", NullLiteral(), ?loc=range)
        | Fable.UnionCaseTest tag ->
            let expected = ofInt tag
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
        // Declare target and bound idents
        let targetId = getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent
        let multiVarDecl =
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
            [|multiVarDecl; switch1; switch2|]
        | None ->
            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
            [| yield multiVarDecl; yield! decisionTree; yield switch2 |]

    let transformDecisionTreeAsStatements (com: IBabelCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement[] =
        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        let targetsWithMultiRefs =
            if com.Options.Typescript then [] // no hoisting when compiled with types
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

        | Fable.Curry(e, arity, _, r) -> transformCurry com ctx r e arity

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> upcast ident id

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformFunctionWithAnnotations com ctx name [arg] body
            |> makeArrowFunctionExpression name

        | Fable.Delegate(args, body, name) ->
            transformFunctionWithAnnotations com ctx name args body
            |> makeArrowFunctionExpression name

        | Fable.ObjectExpr (members, _, baseCall) ->
           transformObjectExpr com ctx members baseCall

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.CurriedApply(callee, args, _, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, _, range) ->
            transformOperation com ctx range kind

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

        | Fable.Emit(info, _, range) ->
            if info.IsJsStatement then iife com ctx expr :> Expression
            else transformEmit com ctx range info

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.WhileLoop _ | Fable.ForLoop _ | Fable.TryCatch _ ->
            iife com ctx expr :> Expression

    let rec transformAsStatements (com: IBabelCompiler) ctx returnStrategy
                                    (expr: Fable.Expr): Statement array =
        match expr with
        | Fable.TypeCast(e, t) ->
            [|transformCast com ctx t e |> resolveExpr t returnStrategy|]

        | Fable.Curry(e, arity, t, r) ->
            [|transformCurry com ctx r e arity |> resolveExpr t returnStrategy|]

        | Fable.Value(kind, r) ->
            [|transformValue com ctx r kind |> resolveExpr kind.Type returnStrategy|]

        | Fable.IdentExpr id ->
            [|identAsExpr id |> resolveExpr id.Type returnStrategy|]

        | Fable.Import({ Selector = selector; Path = path }, t, r) ->
            [|transformImport com ctx r selector path |> resolveExpr t returnStrategy|]

        | Fable.Test(expr, kind, range) ->
            [|transformTest com ctx range kind expr |> resolveExpr Fable.Boolean returnStrategy|]

        | Fable.Lambda(arg, body, name) ->
            [|transformFunctionWithAnnotations com ctx name [arg] body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy|]

        | Fable.Delegate(args, body, name) ->
            [|transformFunctionWithAnnotations com ctx name args body
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
            if info.IsJsStatement then
                [|ExpressionStatement(e)|] // Ignore the return strategy
            else [|resolveExpr t returnStrategy e|]

        | Fable.Operation(kind, t, range) ->
            [|transformOperation com ctx range kind |> resolveExpr t returnStrategy|]

        | Fable.Get(expr, getKind, t, range) ->
            [|transformGet com ctx range t expr getKind |> resolveExpr t returnStrategy|]

        | Fable.Let(bindings, body) ->
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(TransformExpr com ctx expr, kind, value, _range) ->
            let ret =
                match kind with
                | None -> Assign expr
                | Some(Fable.ExprKey(TransformExpr com ctx prop)) -> getExpr None expr prop |> Assign
                | Some(Fable.FieldKey fi) -> get None expr fi.Name |> Assign
            com.TransformAsStatements(ctx, Some ret, value)

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

        | Fable.WhileLoop(TransformExpr com ctx guard, body, range) ->
            [|WhileStatement(guard, transformBlock com ctx None body, ?loc=range) :> Statement|]

        | Fable.ForLoop (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp, range) ->
            let op1, op2 =
                if isUp
                then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus
            [|ForStatement(
                transformBlock com ctx None body,
                start |> varDeclaration (typedIdent com ctx var) true |> Choice1Of2,
                BinaryExpression (op1, ident var, limit),
                UpdateExpression (op2, false, ident var), ?loc=range) :> Statement|]

    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr) =
        let tailcallChance =
            Option.map (fun name ->
                NamedTailCallOpportunity(com, ctx, name, args) :> ITailCallOpportunity) name
        let args = discardUnitArg args |> List.map (typedIdent com ctx)
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with TailCallOpportunity = tailcallChance
                       HoistVars = fun ids -> declaredVars.AddRange(ids); true
                       OptimizeTailCall = fun () -> isTailCallOptimized <- true }
        let body: Choice<BlockStatement, Expression> =
            if body.Type = Fable.Unit
            then transformBlock com ctx (Some ReturnUnit) body |> Choice1Of2
            elif isJsStatement ctx (Option.isSome tailcallChance) body
            then transformBlock com ctx (Some Return) body |> Choice1Of2
            else transformAsExpr com ctx body |> Choice2Of2
        let args, body =
            match isTailCallOptimized, tailcallChance, body with
            | true, Some tc, Choice1Of2 body ->
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
                :> Statement |> Array.singleton |> BlockStatement |> Choice1Of2
            | _ -> args, body
        let body =
            if declaredVars.Count = 0
            then body
            else
                let varDeclStatement =
                    multiVarDeclaration Var [for v in declaredVars -> typedIdent com ctx v, None]
                let bodyStatements =
                    match body with
                    | Choice1Of2 bodyBlock -> bodyBlock.Body
                    | Choice2Of2 bodyExpr -> [|ReturnStatement(bodyExpr, ?loc=bodyExpr.Loc) :> Statement|]
                BlockStatement(Array.append [|varDeclStatement|] bodyStatements) |> Choice1Of2
        args |> List.mapToArray toPattern, body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = macroExpression None "process.argv.slice(2)" []
        let main = CallExpression (funcExpr, [|argv|]) :> Expression
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        ExpressionStatement(main) :> Statement

    let declareModuleMember isPublic membName isMutable (expr: Expression) =
        let membName = Identifier membName
        let decl: Declaration =
            match expr with
            | :? ClassExpression as e ->
                upcast ClassDeclaration(
                    e.Body,
                    ?id = Some membName,
                    ?superClass = e.SuperClass,
                    ?implements = e.Implements,
                    ?superTypeParameters = e.SuperTypeParameters,
                    ?typeParameters = e.TypeParameters)
            | :? FunctionExpression as e ->
                upcast FunctionDeclaration(
                    e.Params, e.Body, membName,
                    ?returnType = e.ReturnType,
                    ?typeParameters = e.TypeParameters)
            | _ -> upcast varDeclaration membName isMutable expr
        if not isPublic
        then Choice1Of2 (decl :> Statement)
        else ExportNamedDeclaration(decl) :> ModuleDeclaration |> Choice2Of2

    let makeEntityTypeParamDecl (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        if com.Options.Typescript then
            getEntityGenParams ent |> makeTypeParamDecl
        else
            None

    let getInterfaceExtends com ctx (ent: Fable.Entity) =
        let mkNative genArgs typeName =
            let id = Identifier(typeName)
            let typeParamInst = makeGenTypeParamInst com ctx genArgs
            InterfaceExtends(id, ?typeParameters=typeParamInst) |> Some
        let mkImport genArgs moduleName typeName =
            let id = makeImportTypeId com ctx moduleName typeName
            let typeParamInst = makeGenTypeParamInst com ctx genArgs
            InterfaceExtends(id, ?typeParameters=typeParamInst) |> Some

        // let isIEquatable = FSharp2Fable.Util.hasInterface Types.iequatable ent
        // let isIComparable = FSharp2Fable.Util.hasInterface Types.icomparable ent

        ent.AllInterfaces |> Seq.choose (fun ifc ->
            match ifc.Definition.FullName with
            | Types.ienumerableGeneric ->
                mkImport ifc.GenericArgs "Seq" "IEnumerable"
            | Types.ienumeratorGeneric ->
                mkImport ifc.GenericArgs "Seq" "IEnumerator"
            | Types.iequatable ->
                mkImport [Fable.Any] "Util" "IEquatable"
            | Types.icomparable ->
                mkImport [Fable.Any] "Util" "IComparable"
            // | Types.iequatableGeneric when not isIEquatable ->
            //     mkImport ifc.GenericArgs "Util" "IEquatable"
            // | Types.icomparableGeneric when not isIComparable ->
            //     mkImport ifc.GenericArgs "Util" "IComparable"
            | Types.comparer ->
                mkImport ifc.GenericArgs "Util" "IComparer"
            // this is not needed, as it's already included in every object
            // | Types.equalityComparer ->
            //     mkImport ifc.GenericArgs "Util" "IEqualityComparer"
            | Types.idisposable ->
                mkImport [] "Util" "IDisposable"
            | Types.icollectionGeneric ->
                mkImport ifc.GenericArgs "Util" "ICollection"
            | "Fable.Collections.IMutableSet`1" ->
                mkImport ifc.GenericArgs "Util" "IMutableSet"
            | "Fable.Collections.IMutableMap`2" ->
                mkImport ifc.GenericArgs "Util" "IMutableMap"
            // TODO: add other interfaces
            | _ -> None
        )

    // must match the above list (with the exception of IEqualityComparer)
    let alreadyDeclaredInterfaces =
        set [
            Types.ienumerableGeneric
            Types.ienumeratorGeneric
            Types.iequatable
            Types.icomparable
            // Types.iequatableGeneric
            // Types.icomparableGeneric
            Types.comparer
            Types.equalityComparer
            Types.idisposable
            Types.icollectionGeneric
            "Fable.Collections.IMutableSet`1"
            "Fable.Collections.IMutableMap`2"
            ]

    let isOtherInterfaceMember (memb: Fable.MemberFunctionOrValue) =
        let isInterface, fullName =
            if memb.IsExplicitInterfaceImplementation then
                true, memb.CompiledName.Replace("-",".")
            else
                let ent = memb.ApparentEnclosingEntity
                ent.IsInterface, memb.FullName
        let lastDot = fullName.LastIndexOf(".")
        let entName = if lastDot < 0 then fullName else fullName.Substring(0, lastDot)
        isInterface && not (alreadyDeclaredInterfaces.Contains entName)

    let getEntityExplicitInterfaceMembers com ctx (ent: Fable.Entity) =
        ent.MembersFunctionsAndValues
        |> Seq.filter isOtherInterfaceMember
        |> Seq.map (fun memb ->
            let args =
                List.concat memb.CurriedParameterGroups
                |> List.mapi (fun i p ->
                    let name =
                        defaultArg p.Name ("arg" + (string i))
                        |> Naming.sanitizeIdentForbiddenChars |> Naming.checkJsKeywords
                    name, p.Type
                )
            let argTypes = args |> List.map snd
            let retType = memb.ReturnParameter.Type
            let genTypeParams = getGenericTypeParams (argTypes @ [retType])
            let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
            let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
            let funcArgs =
                args
                |> Seq.map (fun (name, typ) ->
                    let typeInfo = typeAnnotation com ctx typ
                    FunctionTypeParam(Identifier(name), typeInfo)
                ) |> Seq.toArray
            let returnType = retType |> typeAnnotation com ctx
            let typeParamDecl = makeTypeParamDecl newTypeParams
            let funcTypeInfo =
                FunctionTypeAnnotation(funcArgs, returnType, ?typeParameters=typeParamDecl)
                :> TypeAnnotationInfo
            // TODO!!! This should be the compiled name if the interface is not mangled
            let name = memb.DisplayName
            let membId = Identifier(name) |> Choice1Of2
            ObjectTypeProperty(membId, funcTypeInfo)
        )
        |> Seq.toArray

    let getEntityFieldsAsProps (com: IBabelCompiler) ctx (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let id =
                if Naming.hasIdentForbiddenChars field.Name
                then StringLiteral(field.Name) |> Choice2Of2
                else Identifier(field.Name) |> Choice1Of2
            let ta =
                typeAnnotation com ctx field.FieldType
            let isStaticOpt = if field.IsStatic then Some true else None
            ObjectTypeProperty(id, ta, ?``static``=isStaticOpt))
        |> Seq.toArray

    let getEntityFieldsAsIdents com (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> Naming.sanitizeIdentForbiddenChars |> Naming.checkJsKeywords
            let typ = field.FieldType
            let id: Fable.Ident = makeTypedIdent typ name
            id)
        |> Seq.toArray

    let declareClassType (com: IBabelCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) (baseExpr: Expression option) classMembers =
        let consId = Identifier "constructor"
        let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let baseRef =
            match baseExpr with
            | Some baseRef -> baseRef
            | _ -> makeImportTypeId com ctx "Types" "SystemObject" :> Expression
        let consBody =
            if (Option.isNone baseExpr) || (not ent.IsFSharpUnion) && (ent.IsFSharpRecord || ent.IsValueType || ent.IsFSharpExceptionDeclaration)
            then
                let super = callSuperConstructor None [] |> ExpressionStatement :> Statement
                BlockStatement (Array.append [|super|] consBody.Body)
            else consBody
        let classCons = ClassMethod(ClassImplicitConstructor, consId, consArgs, consBody)
        let classFields =
            if com.Options.Typescript then
                getEntityFieldsAsProps com ctx ent
                |> Array.map (fun prop ->
                    let ta = prop.Value |> TypeAnnotation |> Some
                    ClassProperty(prop.Key, ?``static``=prop.Static, ?typeAnnotation=ta) |> Choice2Of2)
            else Array.empty
        // no need for constructor in unions
        let classMembers = if ent.IsFSharpUnion then classMembers else Array.append [| Choice1Of2 classCons |] classMembers
        let classBody = ClassBody([| yield! classFields; yield! classMembers |])
        let classExpr = ClassExpression(classBody, ?superClass=Some baseRef, ?typeParameters=typeParamDecl)
        classExpr |> declareModuleMember ent.IsPublic entName false

    let declareType (com: IBabelCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr classMembers: Choice<Statement, ModuleDeclaration> list =
        let typeDeclaration = declareClassType com ctx ent entName consArgs consBody baseExpr classMembers
        let reflectionDeclaration =
            let genArgs = Array.init (ent.GenericParameters.Length) (fun i -> "gen" + string i |> makeIdent |> typedIdent com ctx)
            let body = transformReflectionInfo com ctx None ent (Array.map (fun x -> x :> _) genArgs)
            let returnType =
                if com.Options.Typescript then
                    makeImportTypeAnnotation com ctx [] "Reflection" "TypeInfo"
                    |> TypeAnnotation |> Some
                else None
            let args = genArgs |> Array.map toPattern
            makeFunctionExpression None (args, Choice2Of2 body, returnType, None)
            |> declareModuleMember ent.IsPublic (entName + Naming.reflectionSuffix) false
        if com.Options.Typescript then
            let interfaceDecl = makeInterfaceDecl com ctx ent entName baseExpr
            let interfaceDeclaration = ExportNamedDeclaration(interfaceDecl) :> ModuleDeclaration |> Choice2Of2
            [interfaceDeclaration; typeDeclaration; reflectionDeclaration]
        else
            [typeDeclaration; reflectionDeclaration]

    let transformModuleFunction (com: IBabelCompiler) ctx (info: Fable.MemberInfo) (membName: string) args body =
        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx (NonAttached membName) info.HasSpread args body
        let expr = FunctionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl) :> Expression
        info.Attributes
        |> Seq.exists (fun att -> att.FullName = Atts.entryPoint)
        |> function
        | true -> declareEntryPoint com ctx expr |> Choice1Of2
        | false -> declareModuleMember info.IsPublic membName false expr

    let transformAction (com: IBabelCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        let hasVarDeclarations =
            statements |> Array.exists (function
                | :? VariableDeclaration -> true
                | _ -> false)
        if hasVarDeclarations then
            [ CallExpression(FunctionExpression([||], BlockStatement(statements)), [||])
              |> ExpressionStatement :> Statement |> Choice1Of2 ]
        else Array.map Choice1Of2 statements |> Array.toList

    let transformAttachedProperty (com: IBabelCompiler) ctx (memb: Fable.MemberDecl) =
        let kind = if memb.Info.IsGetter then ClassGetter else ClassSetter
        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx Attached false memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        ClassMethod(kind, key, args, body, computed_=computed)
        |> Choice<_,ClassProperty>.Choice1Of2
        |> Array.singleton

    let transformAttachedMethod (com: IBabelCompiler) ctx (memb: Fable.MemberDecl) =
        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx Attached memb.Info.HasSpread memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        let method =
            ClassMethod(ClassFunction, key, args, body, computed_=computed)
            |> Choice<_,ClassProperty>.Choice1Of2
        if memb.Info.IsEnumerator then
            let iterator =
                let key, computed = memberFromName "Symbol.iterator"
                ClassMethod(ClassFunction, key, [||], enumerator2iterator com ctx, computed_=computed)
                |> Choice<_,ClassProperty>.Choice1Of2
            [|method; iterator|]
        else
            [|method|]

    let transformUnion (com: IBabelCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let baseRef = libValue com ctx "Types" "Union"
        let tagId = makeTypedIdent (Fable.Number Int32) "tag"
        let fieldsId = makeTypedIdent (Fable.Array Fable.Any) "fields"
        let args =
            [| typedIdent com ctx tagId |> toPattern
               typedIdent com ctx fieldsId |> restElement |]
        let body =
            [ (ident tagId) :> Expression
              SpreadElement(ident fieldsId) :> Expression ]
            |> callSuperConstructor None
            |> ExpressionStatement :> Statement |> Array.singleton |> BlockStatement
            // [| tagId; fieldsId |]
            // |> Array.map (fun id ->
            //     let left = get None thisExpr id.Name
            //     let right =
            //         match id.Type with
            //         | Fable.Number _ ->
            //             BinaryExpression(BinaryOrBitwise, ident id, NumericLiteral(0.)) :> Expression
            //         | _ -> ident id :> Expression
            //     assign None left right |> ExpressionStatement :> Statement)
            // |> BlockStatement

        let cases =
            let body =
                ent.UnionCases
                |> Seq.map (getUnionCaseName >> makeStrConst)
                |> Seq.toList
                |> makeArray com ctx
                |> ReturnStatement :> Statement
                |> Array.singleton
                |> BlockStatement
            ClassMethod(ClassFunction, Identifier "cases", [||], body) |> Choice<_, ClassProperty>.Choice1Of2

        Array.append [|cases|] classMembers
        |> declareType com ctx ent entName args body (Some baseRef)

    let transformClassWithCompilerGeneratedConstructor (com: IBabelCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getEntityFieldsAsIdents com ent
        let args = fieldIds |> Array.map ident
        let body =
            ent.FSharpFields
            |> Seq.mapi (fun i field ->
                let left = get None thisExpr field.Name
                let right = wrapIntExpression field.FieldType args.[i]
                assign None left right |> ExpressionStatement :> Statement)
            |> Seq.toArray |> BlockStatement
        let baseExpr =
            if ent.IsFSharpExceptionDeclaration
            then libValue com ctx "Types" "FSharpException" |> Some
            elif ent.IsFSharpRecord || ent.IsValueType
            then libValue com ctx "Types" "Record" |> Some
            else None
        let typedPattern = typedIdent com ctx >> toPattern
        let args = fieldIds |> Array.map typedPattern
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithImplicitConstructor (com: IBabelCompiler) ctx (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
        let classIdent = Identifier(classDecl.Name) :> Expression
        let consArgs, consBody, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx ClassConstructor cons.Info.HasSpread cons.Args cons.Body

        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.Options.Typescript then
                let genParams = getEntityGenParams classDecl.Entity
                let returnType = getGenericTypeAnnotation com ctx classDecl.Name genParams
                let typeParamDecl = makeTypeParamDecl genParams |> mergeTypeParamDecls typeParamDecl
                returnType, typeParamDecl
            else
                returnType, typeParamDecl

        let typedPattern = typedIdent com ctx >> toPattern
        let argIdents, argExprs: Pattern list * Expression list =
            match cons.Args with
            | [] -> [], []
            | [unitArg] when unitArg.Type = Fable.Unit -> [], []
            | args when cons.Info.HasSpread ->
                let args = List.rev args
                (restElement(typedIdent com ctx args.Head)) :: (List.map typedPattern args.Tail) |> List.rev,
                (SpreadElement(ident args.Head) :> Expression) :: (List.map identAsExpr args.Tail) |> List.rev
            | args ->
                args |> List.map typedPattern,
                args |> List.map identAsExpr

        let consArgs = List.toArray argIdents

        let exposedCons =
            let exposedConsBody =
                BlockStatement [| ReturnStatement
                    (NewExpression(classIdent, List.toArray argExprs)) |] |> Choice1Of2
            makeFunctionExpression None (consArgs, exposedConsBody, returnType, typeParamDecl)

        let baseExpr, consBody =
            match classDecl.BaseCall with
            | Some baseCall ->
                match baseCall with
                | Fable.Call(TransformExpr com ctx baseRef, info,_,_) ->
                    let args = transformCallArgs com ctx info.HasSpread info.Args
                    let baseCall = callSuperConstructor baseCall.Range args
                    Some baseRef, consBody.Body
                                  |> Array.append [|ExpressionStatement baseCall|]
                                  |> BlockStatement
                | _ -> None, consBody // Unexpected
            // Structs have same properties as records
            | None when classDecl.Entity.IsValueType ->
                Some(libValue com ctx "Types" "Record"), consBody
            | None -> None, consBody

        [
            yield! declareType com ctx classDecl.Entity classDecl.Name consArgs consBody baseExpr classMembers
            yield declareModuleMember cons.Info.IsPublic cons.Name false exposedCons
        ]

    let rec transformDeclaration (com: IBabelCompiler) ctx decl =
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
                if decl.Info.IsValue then
                    let isPublic, isMutable, value =
                        // Mutable public values must be compiled as functions (see #986)
                        // because values imported from ES2015 modules cannot be modified
                        match decl.Info.IsPublic, decl.Info.IsMutable with
                        | true, true -> true, false, Replacements.createAtom com decl.Body |> transformAsExpr com ctx
                        | isPublic, isMutable -> isPublic, isMutable, transformAsExpr com ctx decl.Body
                    [declareModuleMember isPublic decl.Name isMutable value]
                else
                    [transformModuleFunction com ctx decl.Info decl.Name decl.Args decl.Body]

        | Fable.ClassDeclaration decl ->
            let ent = decl.Entity

            let classMembers =
                decl.AttachedMembers
                |> List.toArray
                |> Array.collect (fun memb ->
                    withCurrentScope ctx memb.UsedNames <| fun ctx ->
                        if memb.Info.IsGetter || memb.Info.IsSetter then
                            transformAttachedProperty com ctx memb
                        else
                            transformAttachedMethod com ctx memb)

            match decl.Constructor with
            | Some cons ->
                withCurrentScope ctx cons.UsedNames <| fun ctx ->
                    transformClassWithImplicitConstructor com ctx decl classMembers cons
            | None when ent.IsFSharpUnion -> transformUnion com ctx ent decl.Name classMembers
            | None -> transformClassWithCompilerGeneratedConstructor com ctx ent decl.Name classMembers

    let transformImports (imports: Import seq): Choice<Statement, ModuleDeclaration> list =
        imports |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier(localId)
                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId) |> Choice3Of3
                    | "default" | "" -> ImportDefaultSpecifier(localId) |> Choice2Of3
                    | memb -> ImportSpecifier(localId, Identifier memb) |> Choice1Of3)
            import.Path, specifier)
        |> Seq.groupBy fst
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    let t =
                        match x with
                        | Choice1Of3 x -> x.Type
                        | Choice2Of3 x -> x.Type
                        | Choice3Of3 x -> x.Type
                    match t with
                    | "ImportNamespaceSpecifier" -> mems, defs, x::alls
                    | "ImportDefaultSpecifier" -> mems, x::defs, alls
                    | _ -> x::mems, defs, alls)
            // There seem to be errors if we mix member, default and namespace imports
            // so we must issue an import statement for each kind
            match [mems; defs; alls] with
            | [[];[];[]] ->
                // No specifiers, so this is just an import for side effects
                [ImportDeclaration([||], StringLiteral path) :> ModuleDeclaration |> Choice2Of2]
            | specifiers ->
                specifiers |> List.choose (function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(List.toArray specifiers, StringLiteral path)
                    :> ModuleDeclaration |> Choice2Of2 |> Some))
        |> Seq.toList

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
        let imports = Dictionary<string,Import>()

        interface IBabelCompiler with
            member __.GetImportExpr(ctx, selector, path) =
                let ext = if com.Options.Typescript then "" else Naming.targetFileExtension
                let cachedName = path + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> upcast Identifier(localIdent)
                    | None -> upcast NullLiteral ()
                | false, _ ->
                    let localId = getIdentForImport ctx path selector
                    let i =
                      { Selector =
                            if selector = Naming.placeholder
                            then "`importMember` must be assigned to a variable"
                                 |> addError com [] None; selector
                            else selector
                        LocalIdent = localId
                        Path = path }
                    imports.Add(cachedName, i)
                    match localId with
                    | Some localId -> upcast Identifier(localId)
                    | None -> upcast NullLiteral ()
            member __.GetAllImports() = upcast imports.Values
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None (makeStrConst selector) (makeStrConst path)

        interface Compiler with
            member __.Options = com.Options
            member __.LibraryDir = com.LibraryDir
            member __.CurrentFile = com.CurrentFile
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
            |> ExportAllDeclaration :> ModuleDeclaration |> Choice2Of2 |> Array.singleton
        Program(facadeFile, decls, sourceFiles_ = sourceFiles)

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
            ScopedTypeParams = Set.empty }
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let importDecls = com.GetAllImports() |> transformImports
        let body = importDecls @ rootDecls |> List.toArray
        // We don't add imports as dependencies because those will be handled by Webpack
        // TODO: Do it for other clients, like fable-splitter?
        let dependencies = Array.ofSeq file.WatchDependencies
        Program(file.SourcePath, body, dependencies_ = dependencies)
