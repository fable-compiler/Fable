module rec Fable.Transforms.Fable2Rust

// open Fable
open Fable.AST
open Fable.Transforms.Rust.AST.Adapters
open Fable.Transforms.Rust.AST.Spans
open Fable.Transforms.Rust.AST.Helpers

module Rust = Fable.Transforms.Rust.AST.Types
// module token = Fable.Transforms.Rust.AST.Types.token
// module kw = Fable.Transforms.Rust.AST.Symbols.kw
// module sym = Fable.Transforms.Rust.AST.Symbols.sym

type HashSet<'T> = System.Collections.Generic.HashSet<'T>

// type ReturnStrategy =
//     | Return
//     | ReturnUnit
//     | Assign of Rust.Expr
//     | Target of Ident

// type Import =
//   { Selector: string
//     LocalIdent: string option
//     Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
  { RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string> }

type TypegenContext = {
    FavourClosureTraitOverFunctionPointer: bool
    IsParamType: bool
    TakingOwnership: bool
    // optimization todo - IsAssigningUnwrapped/IsAssigningWrapped - if only ref allow a T rather than a Rc<T>. Can be determined in let where MultipleUsages is calculated
}

type ScopedVarAttrs = {
    IsMutable: bool
    IsRef: bool
    IsRefCountWrapped: bool
    MultipleUsages: bool
}

type Context =
  { File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string>
    ScopedSymbols: FSharp.Collections.Map<string, ScopedVarAttrs>
    Typegen: TypegenContext }

type IRustCompiler =
    inherit Fable.Compiler
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit
    abstract GetAllImports: unit -> seq<string>
    abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Rust.Expr
    abstract TransformAsExpr: Context * Fable.Expr -> Rust.Expr
    // abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Rust.Stmt array
    // abstract TransformImport: Context * selector:string * path:string -> Rust.Expr
    // abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Pattern array) * BlockStatement

// TODO : Centralise and find a home for this
module Helpers =
    module Map =
        let merge a b =
            (a, b) ||> Map.fold(fun acc key t -> acc |> Map.add key t)
        let mergeAndAggregate aggregateFn a b =
            (a, b) ||> Map.fold(fun acc key value ->
                        match acc |> Map.tryFind key with
                        | Some old -> acc |> Map.add key (aggregateFn old value)
                        | None -> acc |> Map.add key value
                        )

// TODO: All things that depend on the library should be moved to Replacements
// to become independent of the specific implementation
module Lib =

    let libCall com ctx r moduleName memberName args =
        let libPath = getLibPath com moduleName
        let callee = Util.transformImport com ctx None memberName libPath
        Util.callFunction com ctx r callee args
(*
    let libConsCall (com: IRustCompiler) ctx r moduleName memberName args =
        Expression.newExpression(com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc=r)

    let libValue (com: IRustCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryJsConstructor (com: IRustCompiler) ctx ent =
        match Replacements.tryJsConstructor com ent with
        | Some e -> com.TransformAsExpr(ctx, e) |> Some
        | None -> None

    let jsConstructor (com: IRustCompiler) ctx ent =
        let entRef = Replacements.jsConstructor com ent
        com.TransformAsExpr(ctx, entRef)

// TODO: This is too implementation-dependent, ideally move it to Replacements
module Reflection =
    open Lib

    let private libReflectionCall (com: IRustCompiler) ctx r memberName args =
        libCall com ctx r "Reflection" (memberName + "_type") args

    let private transformRecordReflectionInfo com ctx r (ent: Fable.Entity) generics =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringLiteral(fullname)
        let genMap =
            let genParamNames = ent.GenericParameters |> List.mapToArray (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map
        let fields =
            ent.FSharpFields |> Seq.map (fun fi ->
                let typeInfo = transformTypeInfo com ctx r genMap fi.FieldType
                (Expression.arrayExpression([|Expression.stringLiteral(fi.Name); typeInfo|])))
            |> Seq.toArray
        let fields = Expression.arrowFunctionExpression([||], Expression.arrayExpression(fields))
        [|fullnameExpr; Expression.arrayExpression(generics); jsConstructor com ctx ent; fields|]
        |> libReflectionCall com ctx None "record"

    let private transformUnionReflectionInfo com ctx r (ent: Fable.Entity) generics =
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringLiteral(fullname)
        let genMap =
            let genParamNames = ent.GenericParameters |> List.map (fun x -> x.Name) |> Seq.toArray
            Array.zip genParamNames generics |> Map
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
        [|fullnameExpr; Expression.arrayExpression(generics); jsConstructor com ctx ent; cases|]
        |> libReflectionCall com ctx None "union"
*)

module TypeInfo =

    let isCopyType (_com: IRustCompiler) t =
        match t with
        | Fable.Boolean
        | Fable.Char
        | Fable.Enum _
        | Fable.Number _ -> true
        // TODO: should we consider some dotnet value types?
        // | Fable.Type.DeclaredType (entRef, gargs) ->
        //     let ent = com.GetEntity entRef
        //     ent.IsValueType && not (ent.IsFSharpRecord)
        | _ -> false

    let primitiveType (name: string): Rust.Ty =
        mkGenericPathTy [name] None

    /// TODO: Emit Arc or Rc wrapper depending on context.
    /// Could also support Gc<T> in the future - https://github.com/Manishearth/rust-gc
    let makeRefTy (ty: Rust.Ty): Rust.Ty =
        [ty] |> mkGenericTy ["Rc"]

    let makeMutTy com (t: Fable.Type) (ty: Rust.Ty): Rust.Ty =
        let cellTy = if isCopyType com t then "Cell" else "RefCell"
        [ty] |> mkGenericTy [cellTy]

    let isCloneable (com: IRustCompiler) = function
        | Fable.String -> true
        | Fable.Type.LambdaType _
        | Fable.Type.DelegateType _ -> true
        | Fable.Type.DeclaredType(eref, _) ->
            let ety = com.GetEntity eref
            ety.IsValueType && ety.IsFSharpRecord
        | _ -> false


    /// Check to see if the type is to be modelled as a ref counted wrapper such as Rc<T> or Arc<T> in a multithreaded context
    let shouldBeRefCountWrapped (com: IRustCompiler) t =
        match t with
        | Fable.String -> true
        | Fable.Array _ -> true
        | Fable.Option _ -> true
        | Fable.Type.Tuple (types, isStruct) ->
            not isStruct
        | Fable.Type.AnonymousRecordType _ -> true
        | Fable.Type.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)
            // TODO: work out if this entity is/can be Copy. If copy, do not wrap?
            (ent.IsFSharpRecord || ent.IsFSharpUnion)
            && not ent.IsValueType  // F# struct records/unions/tuples are modelled as value types, and should support Copy where possible, or Clone if 1 or more children are not Copy
        | _ -> false

    let shouldBePassByRefForParam (com: IRustCompiler) t=
        let isPassByRefTy =
            match t with
            | Fable.Type.LambdaType _
            | Fable.Type.DelegateType _ -> true
            | Fable.Type.DeclaredType(eref, _) ->
                let ety = com.GetEntity eref
                not ety.IsValueType
            | _ -> false
        shouldBeRefCountWrapped com t || isPassByRefTy

    let rec tryGetIdent = function
        | Fable.IdentExpr i -> i.Name |> Some
        | Fable.Get (expr, kind, _, _) -> tryGetIdent expr
        | _ -> None

    module UseageTracking =
        let calcIdentUsages expr =
            let mutable usages = Map.empty
            let something = FableTransforms.deepExists
                                (function | Fable.Expr.IdentExpr ident ->
                                            let count = usages |> Map.tryFind ident.Name |> Option.defaultValue 0
                                            usages <- usages |> Map.add ident.Name (count + 1)
                                            false
                                          | _ -> false) expr
                                |> ignore
            usages

        let hasMultipleUsages (name: string) =
            Map.tryFind name >> Option.map(fun x -> x > 1) >> Option.defaultValue false
            //fun _ -> true

    // let transformTypeInfo (com: IRustCompiler) ctx r (genMap: Map<string, Rust.Expr>) (t: Fable.Type): Rust.Ty =
    let transformType (com: IRustCompiler) ctx (t: Fable.Type): Rust.Ty =
        let numberType kind =
            match kind with
            | Int8 -> "i8"
            | UInt8 -> "u8"
            | Int16 -> "i16"
            | UInt16 -> "u16"
            | Int32 -> "i32"
            | UInt32 -> "u32"
            | Float32 -> "f32"
            | Float64 -> "f64"
            |> primitiveType
        // let nonGenericTypeInfo fullname =
        //     [| Expression.stringLiteral(fullname) |]
        //     |> libReflectionCall com ctx None "class"
        // let resolveGenerics generics: Rust.Expr[] =
        //     generics |> Array.map (transformTypeInfo com ctx r genMap)
        // let genericTypeInfo name genArgs =
        //     let resolved = resolveGenerics genArgs
        //     libReflectionCall com ctx None name resolved
        // let genericEntity (fullname: string) generics =
        //     libReflectionCall com ctx None "class" [|
        //         Expression.stringLiteral(fullname)
        //         if not(Array.isEmpty generics) then
        //             Expression.arrayExpression(generics)
        //     |]
        let ty =
            match t with
            // | Fable.Any -> primitiveType "obj"
            | Fable.GenericParam(name, _) ->
                mkGenericPathTy [name] None
                // match Map.tryFind name genMap with
                // | Some t -> t
                // | None ->
                //     Replacements.genericTypeInfoError name |> addError com [] r
                //     Expression.nullLiteral()
            // | Fable.Unit    -> primitiveType "unit"
            | Fable.Boolean -> primitiveType "bool"
            | Fable.Char    -> primitiveType "char"
            | Fable.String  -> primitiveType "str"
            // | Fable.Enum entRef ->
            //     let ent = com.GetEntity(entRef)

            //     let mutable numberKind = Int32
            //     let cases =
            //         ent.FSharpFields |> Seq.choose (fun fi ->
            //             // F# seems to include a field with this name in the underlying type
            //             match fi.Name with
            //             | "value__" ->
            //                 match fi.FieldType with
            //                 | Fable.Number kind -> numberKind <- kind
            //                 | _ -> ()
            //                 None
            //             | name ->
            //                 let value = match fi.LiteralValue with Some v -> System.Convert.ToDouble v | None -> 0.
            //                 Expression.arrayExpression([|Expression.stringLiteral(name); Expression.numericLiteral(value)|]) |> Some)
            //         |> Seq.toArray
            //         |> Expression.arrayExpression
            //     [|Expression.stringLiteral(entRef.FullName); numberType numberKind; cases |]
            //     |> libReflectionCall com ctx None "enum"
            | Fable.Number(kind, _) ->
                numberType kind
            | Fable.LambdaType(_, returnType) ->
                let inputTypes, returnType = uncurryLambdaType ([], t)
                if ctx.Typegen.FavourClosureTraitOverFunctionPointer then
                    let bounds =
                        [mkFnTraitGenericBound (inputTypes |> List.map (transformParamType com ctx)) (returnType |> transformType com ctx |> Rust.FnRetTy.Ty)]
                    if ctx.Typegen.IsParamType then
                        mkImplTraitsTy bounds
                    else
                        mkTraitsTy bounds
                else
                    transformLambdaType com ctx inputTypes returnType
            // | Fable.DelegateType(argTypes, returnType) ->
            //     genericTypeInfo "delegate" ([|yield! argTypes; yield returnType|])
            | Fable.Tuple(genArgs, _) ->
                genArgs |> List.map (transformType com ctx)
                |> mkTupleTy
            | Fable.Option(genArg, _) ->
                [genArg] |> List.map (transformType com ctx)
                |> mkGenericTy ["Option"]
            | Fable.Array genArg ->
                genArg |> transformType com ctx
                |> makeMutTy com genArg
                |> mkSliceTy
            // | Fable.List genArg     -> genericTypeInfo "list" [|genArg|]
            // | Fable.Regex           -> nonGenericTypeInfo Types.regex
            // | Fable.MetaType        -> nonGenericTypeInfo Types.type_
            | Fable.AnonymousRecordType(fieldNames, genArgs) ->
                genArgs |> List.map (transformType com ctx) //temporary - just use tuples for now!
                |> mkTupleTy
            //     let genArgs = resolveGenerics (List.toArray genArgs)
            //     Array.zip fieldNames genArgs
            //     |> Array.map (fun (k, t) -> Expression.arrayExpression[|Expression.stringLiteral(k); t|])
            //     |> libReflectionCall com ctx None "anonRecord"
            | Fable.DeclaredType(entRef, generics) ->
                let fullName = entRef.FullName
                match fullName, generics with
                // | Replacements.BuiltinEntity kind ->
                //     match kind with
                //     | Replacements.BclGuid
                //     | Replacements.BclTimeSpan
                //     | Replacements.BclDateTime
                //     | Replacements.BclDateTimeOffset
                //     | Replacements.BclTimer
                //     | Replacements.BclInt64
                //     | Replacements.BclUInt64
                //     | Replacements.BclDecimal
                //     | Replacements.BclBigInt -> genericEntity fullName [||]
                //     | Replacements.BclHashSet gen
                //     | Replacements.FSharpSet gen ->
                //         genericEntity fullName [|transformTypeInfo com ctx r genMap gen|]
                //     | Replacements.BclDictionary(key, value)
                //     | Replacements.BclKeyValuePair(key, value)
                //     | Replacements.FSharpMap(key, value) ->
                //         genericEntity fullName [|
                //             transformTypeInfo com ctx r genMap key
                //             transformTypeInfo com ctx r genMap value
                //         |]
                //     | Replacements.FSharpResult(ok, err) ->
                //         let ent = com.GetEntity(entRef)
                //         transformUnionReflectionInfo com ctx r ent [|
                //             transformTypeInfo com ctx r genMap ok
                //             transformTypeInfo com ctx r genMap err
                //         |]
                //     | Replacements.FSharpChoice gen ->
                //         let ent = com.GetEntity(entRef)
                //         let gen = List.map (transformTypeInfo com ctx r genMap) gen
                //         List.toArray gen |> transformUnionReflectionInfo com ctx r ent
                //     | Replacements.FSharpReference gen ->
                //         let ent = com.GetEntity(entRef)
                //         [|transformTypeInfo com ctx r genMap gen|]
                //         |> transformRecordReflectionInfo com ctx r ent
                | _ ->
                    let ent = com.GetEntity(entRef)
                    let genArgs = generics |> List.map (transformType com ctx) |> mkGenericArgs
                    mkFullNamePathTy ent.FullName genArgs
                    // // let generics = generics |> List.map (transformTypeInfo com ctx r genMap) |> List.toArray
                    // /// Check if the entity is actually declared in JS code
                    // if ent.IsInterface
                    //     || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    //     || FSharp2Fable.Util.isGlobalOrImportedEntity ent
                    //     || FSharp2Fable.Util.isReplacementCandidate ent then
                    //     genericEntity ent.FullName generics
                    // else
                    //     let reflectionMethodExpr = FSharp2Fable.Util.entityRefWithSuffix com ent Naming.reflectionSuffix
                    //     let callee = com.TransformAsExpr(ctx, reflectionMethodExpr)
                    //     Expression.callExpression(callee, generics)

            // TODO: remove this catch-all
            | _ -> TODO_TYPE (sprintf "%A" t)
        if shouldBeRefCountWrapped com t then makeRefTy ty else ty

    let transformParamType com ctx t =
        let ty = transformType com ctx t
        if shouldBePassByRefForParam com t then ty |> mkRefTy else ty

    let uncurryLambdaType = function
        | lst, Fable.LambdaType(u, returnType) ->
            uncurryLambdaType (lst @ [u], returnType)
        | lst, treturn -> lst, treturn

    let transformLambdaType com ctx tInputs tRes =
        let fnRetTy =
            if tRes = Fable.Unit then VOID_RETURN_TY
            else transformType com ctx tRes |> Rust.FnRetTy.Ty
        let pat = mkIdentPat "a" false false
        let inputs = tInputs |> List.map (fun tInput ->
            mkParam [] (transformParamType com ctx tInput) pat false)
        let fnDecl = mkFnDecl inputs fnRetTy
        let genParams = [] // TODO:
        mkFnTy genParams fnDecl

(*
    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName
            [|
                yield Expression.stringLiteral(fullname)
                match generics with
                | [||] -> yield Util.undefined None
                | generics -> yield Expression.arrayExpression(generics)
                match tryJsConstructor com ctx ent with
                | Some cons -> yield cons
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
            |]
            |> libReflectionCall com ctx r "class"

    let private ofString s = Expression.stringLiteral(s)
    let private ofArray rustExprs = Expression.arrayExpression(List.toArray rustExprs)

    let transformTypeTest (com: IRustCompiler) ctx range expr (typ: Fable.Type): Rust.Expr =
        let warnAndEvalToFalse msg =
            "Cannot type test (evals to false): " + msg
            |> addWarning com [] range
            Expression.booleanLiteral(false)

        let jsTypeof (primitiveType: string) (Util.TransformExpr com ctx expr): Rust.Expr =
            let typeof = Expression.unaryExpression(UnaryTypeof, expr)
            Expression.binaryExpression(BinaryEqualStrict, typeof, Expression.stringLiteral(primitiveType), ?loc=range)

        let jsInstanceof consExpr (Util.TransformExpr com ctx expr): Rust.Expr =
            Expression.binaryExpression(BinaryInstanceOf, expr, consExpr, ?loc=range)

        match typ with
        | Fable.Any -> Expression.booleanLiteral(true)
        | Fable.Unit -> Expression.binaryExpression(BinaryEqual, com.TransformAsExpr(ctx, expr), Util.undefined None, ?loc=range)
        | Fable.Boolean -> jsTypeof "boolean" expr
        | Fable.Char | Fable.String _ -> jsTypeof "string" expr
        | Fable.Number _ | Fable.Enum _ -> jsTypeof "number" expr
        | Fable.Regex -> jsInstanceof (Expression.identifier("RegExp")) expr
        | Fable.LambdaType _ | Fable.DelegateType _ -> jsTypeof "function" expr
        | Fable.Array _ | Fable.Tuple _ ->
            libCall com ctx None "Util" "isArrayLike" [|com.TransformAsExpr(ctx, expr)|]
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
                | _ -> libCall com ctx None "Util" "isDisposable" [|com.TransformAsExpr(ctx, expr)|]
            | Types.ienumerable ->
                [|com.TransformAsExpr(ctx, expr)|]
                |> libCall com ctx None "Util" "isIterable"
            | Types.array ->
                [|com.TransformAsExpr(ctx, expr)|]
                |> libCall com ctx None "Util" "isArrayLike"
            | Types.exception_ ->
                [|com.TransformAsExpr(ctx, expr)|]
                |> libCall com ctx None "Types" "isException"
            | _ ->
                let ent = com.GetEntity(ent)
                if ent.IsInterface then
                    warnAndEvalToFalse "interfaces"
                else
                    match tryJsConstructor com ctx ent with
                    | Some cons ->
                        if not(List.isEmpty genArgs) then
                            com.WarnOnlyOnce("Generic args are ignored in type testing", ?range=range)
                        jsInstanceof cons expr
                    | None ->
                        warnAndEvalToFalse ent.FullName

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
            |> Array.map TypeParameter.typeParameter
            |> TypeParameterDeclaration |> Some

    let makeTypeParamInst genParams =
        if (Set.isEmpty genParams) then
            None
        else
            genParams
            |> Set.toArray
            |> Array.map (Identifier.identifier >> TypeAnnotationInfo.genericTypeAnnotation)
            |> TypeParameterInstantiation |> Some

    let mergeTypeParamDecls (decl1: TypeParameterDeclaration option) (decl2: TypeParameterDeclaration option) =
        match decl1, decl2 with
        | Some (TypeParameterDeclaration(``params``=p1)), Some (TypeParameterDeclaration(``params``=p2)) ->
            Array.append
                (p1 |> Array.map (fun (TypeParameter(name=name)) -> name))
                (p2 |> Array.map (fun (TypeParameter(name=name)) -> name))
            |> Array.distinct
            |> Array.map TypeParameter.typeParameter
            |> TypeParameterDeclaration |> Some
        | Some _, None -> decl1
        | None, Some _ -> decl2
        | None, None -> None

    let getGenericTypeAnnotation _com _ctx name genParams =
        let typeParamInst = makeTypeParamInst genParams
        GenericTypeAnnotation(Identifier.identifier(name), ?typeParameters=typeParamInst)
        |> TypeAnnotation |> Some

    let typeAnnotation com ctx typ: TypeAnnotationInfo =
        match typ with
        | Fable.MetaType -> AnyTypeAnnotation
        | Fable.Any -> AnyTypeAnnotation
        | Fable.Unit -> VoidTypeAnnotation
        | Fable.Boolean -> BooleanTypeAnnotation
        | Fable.Char -> StringTypeAnnotation
        | Fable.String -> StringTypeAnnotation
        | Fable.Regex -> AnyTypeAnnotation
        | Fable.Number kind -> makeNumericTypeAnnotation com ctx kind
        | Fable.Enum _ent -> NumberTypeAnnotation
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
        TypeAnnotationInfo.genericTypeAnnotation(Identifier.identifier(name))

    let makeGenTypeParamInst com ctx genArgs =
        match genArgs with
        | [] -> None
        | _  -> genArgs |> List.map (typeAnnotation com ctx)
                        |> List.toArray |> TypeParameterInstantiation |> Some

    let makeGenericTypeAnnotation com ctx genArgs id =
        let typeParamInst = makeGenTypeParamInst com ctx genArgs
        GenericTypeAnnotation(id, ?typeParameters=typeParamInst)

    let makeNativeTypeAnnotation com ctx genArgs typeName =
        Identifier.identifier(typeName)
        |> makeGenericTypeAnnotation com ctx genArgs

    let makeImportTypeId (com: IRustCompiler) ctx moduleName typeName =
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

    let makeOptionTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "Option" "Option"

    let makeTupleTypeAnnotation com ctx genArgs =
        List.map (typeAnnotation com ctx) genArgs
        |> List.toArray |> TupleTypeAnnotation

    let makeArrayTypeAnnotation com ctx genArg =
        match genArg with
        | Fable.Number kind when com.Options.TypedArrays ->
            let name = getTypedArrayName com kind
            makeSimpleTypeAnnotation com ctx name
        | _ ->
            makeNativeTypeAnnotation com ctx [genArg] "Array"

    let makeListTypeAnnotation com ctx genArg =
        makeImportTypeAnnotation com ctx [genArg] "List" "List"

    let makeUnionTypeAnnotation com ctx genArgs =
        List.map (typeAnnotation com ctx) genArgs
        |> List.toArray |> UnionTypeAnnotation

    let makeBuiltinTypeAnnotation com ctx kind =
        match kind with
        | Replacements.BclGuid -> StringTypeAnnotation
        | Replacements.BclTimeSpan -> NumberTypeAnnotation
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
        | Replacements.FSharpResult (ok, err) -> makeImportTypeAnnotation com ctx [ok; err] "Fable.Core" "FSharpResult$2"
        | Replacements.FSharpChoice genArgs -> makeImportTypeAnnotation com ctx genArgs "Fable.Core" "FSharpChoice$2"
        | Replacements.FSharpReference genArg -> makeImportTypeAnnotation com ctx [genArg] "Types" "FSharpRef"

    let makeFunctionTypeAnnotation com ctx _typ argTypes returnType =
        let funcTypeParams =
            argTypes
            |> List.mapi (fun i argType ->
                FunctionTypeParam.functionTypeParam(
                    Identifier.identifier("arg" + (string i)),
                    typeAnnotation com ctx argType))
            |> List.toArray
        let genTypeParams = Util.getGenericTypeParams (argTypes @ [returnType])
        let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
        let returnType = typeAnnotation com ctx returnType
        let typeParamDecl = makeTypeParamDecl newTypeParams
        TypeAnnotationInfo.functionTypeAnnotation(funcTypeParams, returnType, ?typeParameters=typeParamDecl)

    let makeEntityTypeAnnotation com ctx (ent: Fable.EntityRef) genArgs =
        match ent.FullName with
        | Types.ienumerableGeneric ->
            makeNativeTypeAnnotation com ctx genArgs "IterableIterator"
        | Types.result ->
            makeUnionTypeAnnotation com ctx genArgs
        | entName when entName.StartsWith(Types.choiceNonGeneric) ->
            makeUnionTypeAnnotation com ctx genArgs
        | _ ->
            let ent = com.GetEntity(ent)
            if ent.IsInterface then
                AnyTypeAnnotation // TODO:
            else
                match Lib.tryJsConstructor com ctx ent with
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

    let typedIdent (com: IRustCompiler) ctx (id: Fable.Ident) =
        if com.Options.Typescript then
            let ta = typeAnnotation com ctx id.Type |> TypeAnnotation |> Some
            let optional = None // match id.Type with | Fable.Option _ -> Some true | _ -> None
            Identifier.identifier(id.Name, ?optional=optional, ?typeAnnotation=ta, ?loc=id.Range)
        else
            Identifier.identifier(id.Name, ?loc=id.Range)

    let transformFunctionWithAnnotations (com: IRustCompiler) ctx name (args: Fable.Ident list) (body: Fable.Expr) =
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
*)
module Util =

    open Lib
    // open Reflection
    // open Annotation
    open TypeInfo

    let (|TransformExpr|) (com: IRustCompiler) ctx e =
        com.TransformAsExpr(ctx, e)
(*
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
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.DeclarationScopes.Contains(name))
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
*)
    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf "Cannot find DecisionTree target %i" targetIndex
        | Some(idents, target) -> idents, target
(*
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
        Expression.nullLiteral()

    let ident (id: Fable.Ident) =
        Identifier.identifier(id.Name, ?loc=id.Range)
*)
    let transformIdent _com _ctx _r (ident: Fable.Ident) =
        mkGenericPathExpr [ident.Name] None

    let transformIdentGet com ctx r (ident: Fable.Ident) =
        let expr = transformIdent com ctx r ident
        if ident.IsMutable
        then expr |> mutableGet com ctx r ident.Type
        else expr

    let transformIdentSet com ctx r (ident: Fable.Ident) (value: Rust.Expr) =
        let expr = transformIdent com ctx r ident
        assert(ident.IsMutable)
        mutableSet com ctx r ident.Type expr value

(*
    let identAsPattern (id: Fable.Ident) =
        Pattern.identifier(id.Name, ?loc=id.Range)

    let thisExpr =
        Expression.thisExpression()

    let ofInt i =
        mkIntLitExpr (uint64 (abs i))
        // Expression.numericLiteral(float i)

    let ofString s =
        mkStrLitExpr s
        // Expression.stringLiteral(s)

    let memberFromName (memberName: string): Rust.Expr * bool =
        match memberName with
        | "ToString" -> Expression.identifier("toString"), false
        | n when n.StartsWith("Symbol.") ->
            Expression.memberExpression(Expression.identifier("Symbol"), Expression.identifier(n.[7..]), false), true
        | n when Naming.hasIdentForbiddenChars n -> Expression.stringLiteral(n), true
        | n -> Expression.identifier(n), false

    let memberFromExpr (com: IRustCompiler) ctx memberExpr: Rust.Expr * bool =
        match memberExpr with
        | Fable.Value(Fable.StringConstant name, _) -> memberFromName name
        | e -> com.TransformAsExpr(ctx, e), true
*)
    let getField r (expr: Rust.Expr) (fieldName: string) =
        let ident = mkIdent fieldName
        mkFieldExpr expr ident // ?loc=r)

    let getExpr r (expr: Rust.Expr) (index: Rust.Expr) =
        mkIndexExpr expr index // ?loc=r)
(*
    let rec getParts (parts: string list) (expr: Rust.Expr) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms
*)
    let makeArray (com: IRustCompiler) ctx typ (exprs: Fable.Expr list) =
        exprs
        |> List.map (fun e ->
            com.TransformAsExpr(ctx, e)
            |> makeMutValue com ctx typ)
        |> mkArrayExpr
        |> makeRefValue com ctx

    let makeTuple (com: IRustCompiler) ctx (exprs: (Fable.Expr) list) =
        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = true}}
        List.map (fun (e: Fable.Expr) ->
                transformLeaveContextByValue com ctx e.Type None e
            ) exprs
        |> mkTupleExpr
(*
    let makeTypedArray (com: IRustCompiler) ctx t (args: Fable.Expr list) =
        match t with
        | Fable.Number kind when com.Options.TypedArrays ->
            let jsName = getTypedArrayName com kind
            let args = [|makeArray com ctx args|]
            Expression.newExpression(Expression.identifier(jsName), args)
        | _ -> makeArray com ctx args

    let makeTypedAllocatedFrom (com: IRustCompiler) ctx typ (fableExpr: Fable.Expr) =
        let getArrayCons t =
            match t with
            | Fable.Number kind when com.Options.TypedArrays ->
                getTypedArrayName com kind |> Expression.identifier
            | _ -> Expression.identifier("Array")

        match fableExpr with
        | ExprType(Fable.Number _) ->
            let cons = getArrayCons typ
            let expr = com.TransformAsExpr(ctx, fableExpr)
            Expression.newExpression(cons, [|expr|])
        | Replacements.ArrayOrListLiteral(exprs, _) ->
            makeTypedArray com ctx typ exprs
        | _ ->
            let cons = getArrayCons typ
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

    let multiVarDeclaration kind (variables: (Identifier * Expression option) list) =
        let varDeclarators =
            // TODO: Log error if there're duplicated non-empty var declarations
            variables
            |> List.distinctBy (fun (Identifier(name=name), _value) -> name)
            |> List.mapToArray (fun (id, value) ->
                VariableDeclarator(id |> Pattern.Identifier, value))
        Statement.variableDeclaration(kind, varDeclarators)

    let varDeclaration (var: Pattern) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration.variableDeclaration(var, value, kind)

    let restElement (var: Pattern) =
        Pattern.restElement(var)

    let callSuper (args: Rust.Expr list) =
        Expression.callExpression(Super(None), List.toArray args)

    let callSuperAsStatement (args: Rust.Expr list) =
        ExpressionStatement(callSuper args)

    let makeClassConstructor args body =
        ClassMember.classMethod(ClassImplicitConstructor, Expression.identifier("constructor"), args, body)
*)
    let callFunction com ctx range (callee: Rust.Expr) (args: Fable.Expr list) =
        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = false}}
        let trArgs = transformCallArgs com ctx false args []
        mkCallExpr callee trArgs //?loc=range)
    let callFunctionTakingOwnership com ctx range (callee: Rust.Expr) (args: Fable.Expr list) =
        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = true}}
        let trArgs = transformCallArgs com ctx false args []
        mkCallExpr callee trArgs //?loc=range)

    /// Immediately Invoked Function Expression
    let iife (com: IRustCompiler) ctx (expr: Fable.Expr) =
        let fnExpr = transformLambda com ctx [] expr
        let range = None // TODO:
        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = false}}
        callFunction com ctx range fnExpr []
        // let _, body = com.TransformFunction(ctx, None, [], expr)
        // // Use an arrow function in case we need to capture `this`
        // Expression.callExpression(Expression.arrowFunctionExpression([||], body), [||])

(*
    let callFunctionWithThisContext r callee (args: Rust.Expr list) =
        let args = thisExpr::args |> List.toArray
        Expression.callExpression(get None funcExpr "call", args, ?loc=r)

    let emitExpression range (txt: string) args =
        mkEmitExpr txt // TODO: apply args, range

    let undefined range =
//        Undefined(?loc=range) :> Expression
        Expression.unaryExpression(UnaryVoid, Expression.numericLiteral(0.), ?loc=range)
*)
    let getGenericTypeParams (types: Fable.Type list) =
        let rec getGenParams = function
            | Fable.GenericParam(name, _) -> [name]
            | t -> t.Generics |> List.collect getGenParams
        types
        |> List.collect getGenParams
        |> Set.ofList
(*
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

    let getMemberArgsAndBody (com: IRustCompiler) ctx kind hasSpread (args: Fable.Ident list) (body: Fable.Expr) =
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
        let args, body, returnType, typeParamDecl = transformFunctionWithAnnotations com ctx funcName args body

        let typeParamDecl =
            if com.Options.Typescript then
                makeTypeParamDecl genTypeParams |> mergeTypeParamDecls typeParamDecl
            else typeParamDecl

        let args =
            let len = Array.length args
            if not hasSpread || len = 0 then args
            else [|
                if len > 1 then
                    yield! args.[..len-2]
                yield restElement args.[len-1]
            |]

        args, body, returnType, typeParamDecl

    let getUnionCaseName (uci: Fable.UnionCase) =
        // match uci.CompiledName with Some cname -> cname | None -> uci.Name
        uci.FullName

    let getUnionExprTag (com: IRustCompiler) ctx range (fableExpr: Fable.Expr) =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        // getExpr range expr (Expression.stringLiteral("tag"))
        expr

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Rust.Expr) =
        match e, typ with
        | Literal(NumericLiteral(_)), _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.Enum _ ->
            Expression.binaryExpression(BinaryOrBitwise, e, Expression.numericLiteral(0.))
        | _ -> e

    let wrapExprInBlockWithReturn e =
        BlockStatement([| Statement.returnStatement(e)|])

    let makeArrowFunctionExpression _name (args, (body: BlockStatement), returnType, typeParamDecl): Rust.Expr =
        Expression.arrowFunctionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let makeFunctionExpression name (args, (body: Rust.Expr), returnType, typeParamDecl): Rust.Expr =
        let id = name |> Option.map Identifier.identifier
        let body = wrapExprInBlockWithReturn body
        Expression.functionExpression(args, body, ?id=id, ?returnType=returnType, ?typeParameters=typeParamDecl)

    let optimizeTailCall (com: IRustCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
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
                yield varDeclaration (Pattern.identifier(tempVar)) false (Expression.identifier(argId)) |> Declaration.VariableDeclaration |> Declaration
            // Then assign argument expressions to the original argument identifiers
            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
            for (argId, arg) in zippedArgs do
                let arg = FableTransforms.replaceValues tempVarReplacements arg
                let arg = com.TransformAsExpr(ctx, arg)
                yield assign None (Expression.identifier(argId)) arg |> ExpressionStatement
            yield Statement.continueStatement(Identifier.identifier(tc.Label), ?loc=range)
        |]
*)
    let transformImport (com: IRustCompiler) ctx r (selector: string) (path: string) =
        // let selector, parts =
        //     let parts = Array.toList(selector.Split('.'))
        //     parts.Head, parts.Tail
        com.GetImportExpr(ctx, selector, path, r)
        // |> getParts parts

    let transformCast (com: IRustCompiler) (ctx: Context) typ (expr: Fable.Expr): Rust.Expr =
        let fromType, toType = expr.Type, typ
        match fromType, toType with
        | Fable.Number _, Fable.Number _ ->
            let ty = transformType com ctx typ
            com.TransformAsExpr(ctx, expr)
            |> mkCastExpr ty
        // TODO: other casts
        | _ ->
            com.TransformAsExpr(ctx, expr)

(*
    let transformCast (com: IRustCompiler) (ctx: Context) t tag e: Rust.Expr =
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

    let transformCurry (com: IRustCompiler) (ctx: Context) _r expr arity: Rust.Expr =
        com.TransformAsExpr(ctx, Replacements.curryExprAtRuntime com arity expr)
*)
    /// This guarantees a new owned Rc<T>
    let makeClone expr = mkMethodCallExpr "clone" None expr []

    /// Calling this on an rc guarantees a &T, regardless of if the Rc is a ref or not
    let makeAsRef expr = mkMethodCallExpr "as_ref" None expr []

    let makeRefValue (com: IRustCompiler) ctx (value: Rust.Expr) =
        let callee = mkGenericPathExpr ["Rc";"from"] None
        mkCallExpr callee [value]

    let makeMutValue (com: IRustCompiler) ctx typ (value: Rust.Expr) =
        let cellTy = if isCopyType com typ then "Cell" else "RefCell"
        let callee = mkGenericPathExpr [cellTy;"from"] None
        mkCallExpr callee [value]

    let transformCallArgs (com: IRustCompiler) ctx hasSpread args (argTypes: Fable.Type list) =
        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant, _))] -> []
        // | args when hasSpread ->
        //     match List.rev args with
        //     | [] -> []
        //     | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
        //     | last::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
        | args ->
            if ctx.Typegen.TakingOwnership then
                args |> List.map (fun e -> transformLeaveContextByValue com ctx e.Type None e)
            else
                args |> List.map (transformLeaveContextByPreferredBorrow com ctx)

    let maybeUnwrapRef com ctx typ name expr =
        let isRef = ctx.ScopedSymbols |> Map.tryFind name |> Option.map(fun s -> s.IsRef) |> Option.defaultValue false
        let expr =
            if shouldBeRefCountWrapped com typ
            then mkDerefExpr expr
            else expr
        if isRef then mkDerefExpr expr else expr

    let prepareRefForPatternMatch com ctx typ name expr =
        let isRef = ctx.ScopedSymbols |> Map.tryFind name |> Option.map(fun s -> s.IsRef) |> Option.defaultValue false
        if shouldBeRefCountWrapped com typ
        then makeAsRef expr
        else
            if isRef then expr else mkAddrOfExpr expr

    let transformValue (com: IRustCompiler) (ctx: Context) r value: Rust.Expr =
        match value with
        // | Fable.BaseValue (None, _) -> Super(None)
        // | Fable.BaseValue (Some boundIdent, _) -> identAsExpr boundIdent
        // | Fable.ThisValue _ -> Expression.thisExpression()
        // | Fable.TypeInfo t -> transformTypeInfo com ctx r Map.empty t
        // | Fable.Null _t ->
        //     // if com.Options.typescript
        //     //     let ta = typeAnnotation com ctx t |> TypeAnnotation |> Some
        //     //     upcast Identifier("null", ?typeAnnotation=ta, ?loc=r)
        //     // else
        //         Expression.nullLiteral(?loc=r)
        // | Fable.UnitConstant -> undefined r
        | Fable.BoolConstant x -> mkBoolLitExpr x //, ?loc=r)
        | Fable.CharConstant x -> mkCharLitExpr x //, ?loc=r)
        | Fable.StringConstant x ->
            let value = mkStrLitExpr x
            let strTy = primitiveType "str" |> makeRefTy
            makeRefValue com ctx value
            |> mkCastExpr strTy // casting is necessary for Rc<_> to get type
        | Fable.NumberConstant (x, kind, _) ->
            let expr =
                match kind with
                | Float32 | Float64 -> mkFloatLitExpr (abs x) //, ?loc=r)
                | Int8 | Int16 | Int32 -> mkIntLitExpr (uint64 (abs x)) //, ?loc=r)
                | UInt8 | UInt16 | UInt32 -> mkIntLitExpr (uint64 (abs x)) //, ?loc=r)
            // if negative, wrap in unary minus
            if x < 0.0
            then expr |> mkUnaryExpr Rust.UnOp.Neg
            else expr
        // | Fable.RegexConstant (source, flags) -> Expression.regExpLiteral(source, flags, ?loc=r)
        | Fable.NewArray (values, typ) -> makeArray com ctx typ values
        // | Fable.NewArrayFrom (size, typ) -> makeTypedAllocatedFrom com ctx typ size
        | Fable.NewTuple (values, isStruct) ->
            let tuple = makeTuple com ctx values
            if isStruct then tuple else makeRefValue com ctx tuple
        // | Fable.NewList (headAndTail, _typ) ->
        //     let rec getItems acc = function
        //         | None -> List.rev acc, None
        //         | Some(head, Fable.Value(Fable.NewList(tail, _),_)) -> getItems (head::acc) tail
        //         | Some(head, tail) -> List.rev (head::acc), Some tail
        //     match getItems [] headAndTail with
        //     | [], None ->
        //         libCall com ctx r "List" "empty" [||]
        //     | [TransformExpr com ctx expr], None ->
        //         libCall com ctx r "List" "singleton" [|expr|]
        //     | exprs, None ->
        //         [|makeArray com ctx exprs|]
        //         |> libCall com ctx r "List" "ofArray"
        //     | [TransformExpr com ctx head], Some(TransformExpr com ctx tail) ->
        //         libCall com ctx r "List" "cons" [|head; tail|]
        //     | exprs, Some(TransformExpr com ctx tail) ->
        //         [|makeArray com ctx exprs; tail|]
        //         |> libCall com ctx r "List" "ofArrayWithTail"
        | Fable.NewOption (value, t, _) ->
            match value with
            | Some arg ->
                let callee = mkGenericPathExpr ["Some"] None
                callFunctionTakingOwnership com ctx r callee [arg]
            | None ->
                mkGenericPathExpr ["None"] None
            |> makeRefValue com ctx
        // | Fable.EnumConstant (x, _) ->
        //     com.TransformAsExpr(ctx, x)
        | Fable.NewRecord (values, ent, genArgs) ->
            let ent = com.GetEntity(ent)
            let fields =
                Seq.zip ent.FSharpFields values
                |> Seq.map (fun (fi, value) ->
                    let attrs = []
                    let ident = mkIdent fi.Name
                    let expr =
                        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = true}}
                        if fi.IsMutable then
                            com.TransformAsExpr(ctx, value)
                            |> makeMutValue com ctx fi.FieldType
                        else
                            transformLeaveContextByValue com ctx fi.FieldType None value
                    mkExprField attrs ident expr false false)
            let genArgs = genArgs |> List.map (transformType com ctx) |> mkGenericArgs
            let path = mkFullNamePath ent.FullName genArgs
            let expr = mkStructExpr path fields // TODO: range
            if ent.IsValueType then expr else expr |> makeRefValue com ctx
        | Fable.NewAnonymousRecord (values, fieldNames, _genArgs) ->
            Fable.NewTuple (values, false) |> transformValue com ctx None   //temporary, use tuples!
        //     let values = List.mapToArray (fun x -> com.TransformAsExpr(ctx, x)) values
        //     Array.zip fieldNames values |> makeJsObject
        | Fable.NewUnion (values, tag, ent, genArgs) ->
            let ent = com.GetEntity(ent)
            let genArgs = genArgs |> List.map (transformType com ctx) |> mkGenericArgs
            let unionCase = ent.UnionCases |> List.item tag
            let callee = mkFullNamePathExpr unionCase.FullName genArgs
            callFunctionTakingOwnership com ctx r callee values
            |> makeRefValue com ctx

        // TODO: remove this catch-all
        | _ -> TODO_EXPR (sprintf "%A" value)

    let calcVarAttrsAndOnlyRef com ctx t name e=
        let name = name |> Option.defaultValue(tryGetIdent e |> Option.defaultValue "")
        let varAttrs =
            ctx.ScopedSymbols   // todo - cover more than just root level idents
            |> Map.tryFind name
            |> Option.defaultValue {
                IsRef = false
                IsRefCountWrapped = shouldBeRefCountWrapped com t
                IsMutable = false
                MultipleUsages = true }
        let isOnlyReference =
            if varAttrs.IsRef then false
            else
                match e with
                | Fable.Call _ ->
                    //if the source is the returned value of a function, it is never bound, so we can assume this is the only reference
                    true
                | Fable.CurriedApply _ -> true
                | Fable.Value(kind, r) ->
                    //an inline value kind is also never bound, so can assume this is the only reference also
                    true
                | Fable.Lambda _
                | Fable.Delegate _ -> true
                | _ ->
                    //would need to track all useages to work out if this is actually referenced more than once, so for safety assume false
                    not varAttrs.MultipleUsages
        varAttrs, isOnlyReference
    let transformLeaveContextByPreferredBorrow (com: IRustCompiler) ctx (e: Fable.Expr): Rust.Expr =
        let expr = com.TransformAsExpr (ctx, e)
        let varAttrs, isOnlyReference = calcVarAttrsAndOnlyRef com ctx e.Type None e
        if shouldBePassByRefForParam com e.Type then
            expr |> mkAddrOfExpr
        else if isCloneable com e.Type && not isOnlyReference then makeClone expr else expr
    let transformLeaveContextByValue (com: IRustCompiler) ctx t (name: string option) (e: Fable.Expr): Rust.Expr =
        let expr = com.TransformAsExpr (ctx, e)
        let varAttrs, isOnlyReference = calcVarAttrsAndOnlyRef com ctx t name e

        if varAttrs.IsRefCountWrapped && not isOnlyReference then
            makeClone expr
        else if varAttrs.IsRef then mkDerefExpr expr
        else if isCloneable com t && not isOnlyReference then makeClone expr //shouldn't really be using a rchelper as this is NOT an rc
        else expr
(*
    let enumerator2iterator com ctx =
        let enumerator = Expression.callExpression(get None (Expression.identifier("this")) "GetEnumerator", [||])
        BlockStatement([| Statement.returnStatement(libCall com ctx None "Util" "toIterator" [|enumerator|])|])

    let extractBaseExprFromBaseCall (com: IRustCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
        match baseCall, baseType with
        | Some (Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr =
                match baseRef with
                | Fable.IdentExpr id -> typedIdent com ctx id |> Expression.Identifier
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

    let transformObjectExpr (com: IRustCompiler) ctx (members: Fable.MemberDecl list) baseCall: Rust.Expr =
        let compileAsClass =
            Option.isSome baseCall || members |> List.exists (fun m ->
                // Optimization: Object literals with getters and setters are very slow in V8
                // so use a class expression instead. See https://github.com/fable-compiler/Fable/pull/2165#issuecomment-695835444
                m.Info.IsSetter || (m.Info.IsGetter && canHaveSideEffects m.Body))

        let makeMethod kind prop computed hasSpread args body =
            let args, body, returnType, typeParamDecl =
                getMemberArgsAndBody com ctx (Attached(isStatic=false)) hasSpread args body
            ObjectMember.objectMethod(kind, prop, args, body, computed_=computed,
                ?returnType=returnType, ?typeParameters=typeParamDecl)

        let members =
            members |> List.collect (fun memb ->
                let info = memb.Info
                let prop, computed = memberFromName memb.Name
                // If compileAsClass is false, it means getters don't have side effects
                // and can be compiled as object fields (see condition above)
                if info.IsValue || (not compileAsClass && info.IsGetter) then
                    [ObjectMember.objectProperty(prop, com.TransformAsExpr(ctx, memb.Body), computed_=computed)]
                elif info.IsGetter then
                    [makeMethod ObjectGetter prop computed false memb.Args memb.Body]
                elif info.IsSetter then
                    [makeMethod ObjectSetter prop computed false memb.Args memb.Body]
                elif info.IsEnumerator then
                    let method = makeMethod ObjectMeth prop computed info.HasSpread memb.Args memb.Body
                    let iterator =
                        let prop, computed = memberFromName "Symbol.iterator"
                        let body = enumerator2iterator com ctx
                        ObjectMember.objectMethod(ObjectMeth, prop, [||], body, computed_=computed)
                    [method; iterator]
                else
                    [makeMethod ObjectMeth prop computed info.HasSpread memb.Args memb.Body]
            )

        if not compileAsClass then
            Expression.objectExpression(List.toArray  members)
        else
            let classMembers =
                members |> List.choose (function
                    | ObjectProperty(key, value, computed) ->
                        ClassMember.classProperty(key, value, computed_=computed) |> Some
                    | ObjectMethod(kind, key, ``params``, body, computed, returnType, typeParameters, _) ->
                        let kind =
                            match kind with
                            | "get" -> ClassGetter
                            | "set" -> ClassSetter
                            | _ -> ClassFunction
                        ClassMember.classMethod(kind, key, ``params``, body, computed_=computed,
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

            let classBody = ClassBody.classBody(List.toArray classMembers)
            let classExpr = Expression.classExpression(classBody, ?superClass=baseExpr)
            Expression.newExpression(classExpr, [||])
*)

(*
    let resolveExpr t strategy rustExpr: Rust.Stmt =
        match strategy with
        | None | Some ReturnUnit -> ExpressionStatement(rustExpr)
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return ->  Statement.returnStatement(wrapIntExpression t rustExpr)
        | Some(Assign left) -> ExpressionStatement(assign None left rustExpr)
        | Some(Target left) -> ExpressionStatement(assign None (left |> Expression.Identifier) rustExpr)
*)
    let transformOperation com ctx t range opKind: Rust.Expr =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            match op with
            | UnaryOperator.UnaryMinus -> mkUnaryExpr Rust.UnOp.Neg expr //?loc=range)
            | UnaryOperator.UnaryPlus -> expr // no unary plus
            | UnaryOperator.UnaryNot -> mkUnaryExpr Rust.UnOp.Not expr //?loc=range)
            | UnaryOperator.UnaryNotBitwise -> mkUnaryExpr Rust.UnOp.Not expr //?loc=range)
            | UnaryOperator.UnaryTypeof -> failwith "UnaryTypeof not supported"
            | UnaryOperator.UnaryDelete -> failwith "UnaryDelete not supported"
            | UnaryOperator.UnaryVoid -> failwith "Should not happen"

        | Fable.Binary(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            let kind =
                match op with
                | BinaryOperator.BinaryEqual -> Rust.BinOpKind.Eq
                | BinaryOperator.BinaryUnequal -> Rust.BinOpKind.Ne
                | BinaryOperator.BinaryEqualStrict -> Rust.BinOpKind.Eq
                | BinaryOperator.BinaryUnequalStrict -> Rust.BinOpKind.Ne
                | BinaryOperator.BinaryLess -> Rust.BinOpKind.Lt
                | BinaryOperator.BinaryLessOrEqual -> Rust.BinOpKind.Le
                | BinaryOperator.BinaryGreater -> Rust.BinOpKind.Gt
                | BinaryOperator.BinaryGreaterOrEqual -> Rust.BinOpKind.Ge
                | BinaryOperator.BinaryShiftLeft -> Rust.BinOpKind.Shl
                | BinaryOperator.BinaryShiftRightSignPropagating -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryShiftRightZeroFill -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryMinus -> Rust.BinOpKind.Sub
                | BinaryOperator.BinaryPlus -> Rust.BinOpKind.Add
                | BinaryOperator.BinaryMultiply -> Rust.BinOpKind.Mul
                | BinaryOperator.BinaryDivide -> Rust.BinOpKind.Div
                | BinaryOperator.BinaryModulus -> Rust.BinOpKind.Rem
                | BinaryOperator.BinaryExponent -> failwithf "BinaryExponent not supported. TODO: implement with pow."
                | BinaryOperator.BinaryOrBitwise -> Rust.BinOpKind.BitOr
                | BinaryOperator.BinaryXorBitwise -> Rust.BinOpKind.BitXor
                | BinaryOperator.BinaryAndBitwise -> Rust.BinOpKind.BitAnd
                | BinaryOperator.BinaryIn -> failwithf "BinaryIn not supported"
                | BinaryOperator.BinaryInstanceOf -> failwithf "BinaryInstanceOf not supported"
            let left = maybeUnwrapRef com ctx t "" left
            let right = maybeUnwrapRef com ctx t "" right
            match t, kind with
            | Fable.Type.String, Rust.BinOpKind.Add ->
                //proprietary string concatenation - String + &String = String
                let left = mkMethodCallExpr "to_string" None left []
                let strTy = primitiveType "str" |> makeRefTy
                mkBinaryExpr (mkBinOp kind) left (mkAddrOfExpr right)
                |> makeRefValue com ctx
                |> mkCastExpr strTy
            | _ ->
                mkBinaryExpr (mkBinOp kind) left right //?loc=range)

        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            let kind =
                match op with
                | LogicalOperator.LogicalOr -> Rust.BinOpKind.Or
                | LogicalOperator.LogicalAnd -> Rust.BinOpKind.And
            mkBinaryExpr (mkBinOp kind) left right //?loc=range)

    let transformEmit (com: IRustCompiler) ctx range (info: Fable.EmitInfo) =
        let macro = info.Macro
        let info = info.CallInfo
        // let thisArg = info.ThisArg |> Option.map (fun e -> com.TransformAsExpr(ctx, e)) |> Option.toList
        transformCallArgs com ctx info.HasSpread info.Args info.SignatureArgTypes
        // |> List.append thisArg
        // for now implemented as emit macro
        // TODO: better implementation, range
        |> mkMacroExpr macro

    let transformCall (com: IRustCompiler) ctx range calleeExpr (callInfo: Fable.CallInfo) =
        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = false}}
        let args = transformCallArgs com ctx callInfo.HasSpread callInfo.Args callInfo.SignatureArgTypes
        match calleeExpr with
        | Fable.Get(callee, Fable.FieldGet(membName, _), _t, _r) ->
            // instance call
            let callee = com.TransformAsExpr(ctx, callee)
            mkMethodCallExpr membName None callee args
        | _ ->
            let callee = com.TransformAsExpr(ctx, calleeExpr)
            match callInfo.ThisArg with
            | Some(TransformExpr com ctx thisArg) ->
                mkCallExpr callee (thisArg::args)
            // | None when callInfo.IsJsConstructor -> Expression.newExpression(callee, List.toArray args, ?loc=range)
            | None ->
                mkCallExpr callee args
(*
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
    let transformBlock (com: IRustCompiler) ctx ret expr: BlockStatement =
        com.TransformAsStatements(ctx, ret, expr) |> BlockStatement

    let transformTryCatch com ctx r returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause.catchClause(identAsPattern param, transformBlock com ctx returnStrategy body))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [|Statement.tryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer, ?loc=r)|]

    let rec transformIfStatement (com: IRustCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
        match com.TransformAsExpr(ctx, guardExpr) with
        | Literal(BooleanLiteral(value=value)) when value ->
            com.TransformAsStatements(ctx, ret, thenStmnt)
        | Literal(BooleanLiteral(value=value)) when not value ->
            com.TransformAsStatements(ctx, ret, elseStmnt)
        | guardExpr ->
            let thenStmnt = transformBlock com ctx ret thenStmnt
            match com.TransformAsStatements(ctx, ret, elseStmnt) with
            | [||] -> Rust.Stmt.ifStatement(guardExpr, thenStmnt, ?loc=r)
            | [|elseStmnt|] -> Rust.Stmt.ifStatement(guardExpr, thenStmnt, elseStmnt, ?loc=r)
            | statements -> Rust.Stmt.ifStatement(guardExpr, thenStmnt, Statement.blockStatement(statements), ?loc=r)
            |> Array.singleton
*)
    let mutableGet (com: IRustCompiler) ctx range typ expr =
        if isCopyType com typ then
            mkMethodCallExpr "get" None expr []
        else
            mkMethodCallExpr "borrow" None expr []
            |> mkUnaryExpr Rust.UnOp.Deref

    let transformGet (com: IRustCompiler) ctx range typ (fableExpr: Fable.Expr) kind =
        match kind with
        | Fable.ExprGet idx ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            let prop = com.TransformAsExpr(ctx, idx)
            // if indexing an array, cast index to usize
            match fableExpr.Type, idx.Type with
            | Fable.Array t, Fable.Number(Int32, None) ->
                let uintTy = primitiveType "usize"
                let prop = mkCastExpr uintTy prop
                getExpr range expr prop
                |> mutableGet com ctx range t
            | _ ->
                getExpr range expr prop

        | Fable.FieldGet(fieldName, isMutable) ->
            match fableExpr.Type with
            | Fable.AnonymousRecordType (fields, args) ->
                // temporary - redirect anon to tuple calls
                let idx = fields |> Array.findIndex (fun f -> f = fieldName)
                (Fable.TupleIndex (idx))
                |> transformGet com ctx range typ fableExpr
            | _ ->
                let expr = com.TransformAsExpr(ctx, fableExpr)
                let field = getField range expr fieldName
                if isMutable
                then mutableGet com ctx range typ field
                else field

        // | Fable.ListHead ->
        //     // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
        //     libCall com ctx range "List" "head" [|com.TransformAsExpr(ctx, fableExpr)|]

        // | Fable.ListTail ->
        //     // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
        //     libCall com ctx range "List" "tail" [|com.TransformAsExpr(ctx, fableExpr)|]

        | Fable.TupleIndex index ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            mkFieldExpr expr (mkIdent (index.ToString()))

        | Fable.OptionValue ->
            // let expr = com.TransformAsExpr(ctx, fableExpr)
            // mkMethodCallExpr "unwrap" None expr []
            libCall com ctx range "Option" "get" [fableExpr]

        | Fable.UnionTag ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            // TODO: range
            expr

        | Fable.UnionField(caseIndex, fieldIndex) ->
            // let expr = com.TransformAsExpr(ctx, fableExpr)
            // getExpr range (getExpr None expr (Expression.stringLiteral("fields"))) (ofInt fieldIndex)
            match fableExpr with
            | Fable.IdentExpr id ->
                let name = $"{id.Name}_{caseIndex}_{fieldIndex}"
                mkGenericPathExpr [name] None
            | _ ->
                // compile as: "if let MyUnion::Case(x, _) = opt { x } else { unreachable!() }"
                match fableExpr.Type with
                | Fable.DeclaredType(entRef, genArgs) ->
                    let ent = com.GetEntity(entRef)
                    assert(ent.IsFSharpUnion)
                    // let genArgs = genArgs |> List.map (transformType com ctx) |> mkGenericArgs // TODO:
                    let unionCase = ent.UnionCases |> List.item caseIndex
                    let fieldName = "x"
                    let fields =
                        unionCase.UnionCaseFields
                        |> Seq.mapi (fun i field ->
                            if i = fieldIndex then
                                mkIdentPat fieldName false false
                            else WILD_PAT)
                    let path = mkFullNamePath unionCase.FullName None
                    let pat = mkTupleStructPat path fields
                    let expr = com.TransformAsExpr(ctx, fableExpr)
                                    |> prepareRefForPatternMatch com ctx fableExpr.Type ""
                    let thenExpr =
                        mkGenericPathExpr [fieldName] None


                    let arms = [
                        mkArm [] pat None thenExpr
                    ]
                    let arms =
                        if (List.length ent.UnionCases) > 1 then
                            // only add a default arm if needed
                            let defaultArm = mkArm [] WILD_PAT None (mkMacroExpr "unreachable" [])
                            arms @ [defaultArm]
                        else arms

                    mkMatchExpr expr arms
                    // TODO : Cannot use if let because it moves references out of their Rc's, which breaks borrow checker. We cannot bind
                    // let ifExpr = mkLetExpr pat expr
                    // let thenExpr = mkGenericPathExpr [fieldName] None
                    // let elseExpr = mkMacroExpr "unreachable" []
                    // mkIfThenElseExpr ifExpr thenExpr elseExpr
                | _ ->
                    failwith "Should not happen"

        // TODO: remove this catch-all
        | _ -> TODO_EXPR (sprintf "kind: %A" kind)

    let mutableSet (com: IRustCompiler) ctx range typ expr value =
        if isCopyType com typ then
            mkMethodCallExpr "set" None expr [value]
        else
            let mutableField =
                mkMethodCallExpr "borrow_mut" None expr []
                |> mkUnaryExpr Rust.UnOp.Deref
            mkAssignExpr mutableField value //?loc=range)

    let transformSet (com: IRustCompiler) ctx range fableExpr typ (value: Fable.Expr) kind =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        let value = com.TransformAsExpr(ctx, value)
        match kind with
        | Fable.ValueSet ->
            match fableExpr with
            | Fable.IdentExpr id when id.IsMutable ->
                transformIdentSet com ctx range id value
            | _ ->
                mkAssignExpr expr value
        | Fable.ExprSet idx ->
            let prop = com.TransformAsExpr(ctx, idx)
            // if indexing an array, cast index to usize
            match fableExpr.Type, idx.Type with
            | Fable.Array t, Fable.Number(Int32, None) ->
                let uintTy = primitiveType "usize"
                let prop = mkCastExpr uintTy prop
                let left = getExpr range expr prop
                mutableSet com ctx range typ left value
            | _ ->
                let left = getExpr range expr prop
                mkAssignExpr left value //?loc=range)
        | Fable.FieldSet(fieldName) ->
            let field = getField None expr fieldName
            mutableSet com ctx range typ field value

    let transformAsStmt (com: IRustCompiler) ctx (e: Fable.Expr): Rust.Stmt =
        let expr = com.TransformAsExpr(ctx, e)
        mkExprStmt expr

    // flatten nested Let binding expressions
    let rec flattenLet acc (expr: Fable.Expr) =
        match expr with
        | Fable.Let(ident, value, body) ->
            flattenLet ((ident, value)::acc) body
        | _ -> List.rev acc, expr

    // flatten nested Sequential expressions (depth first)
    let rec flattenSequential (expr: Fable.Expr) =
        match expr with
        | Fable.Sequential exprs ->
            List.collect flattenSequential exprs
        | _ -> [expr]

    let transformLet (com: IRustCompiler) ctx bindings body =
        let usages =
            let bodyUseages = UseageTracking.calcIdentUsages body
            let bindingsUseages = bindings |> List.map (snd >> UseageTracking.calcIdentUsages)
            (Map.empty, bodyUseages::bindingsUseages)
            ||> List.fold (Helpers.Map.mergeAndAggregate (+))

        let makeLetStmt ctx (ident: Fable.Ident, value: Fable.Expr) =
            let isRef = false
            let isMut = false
            let pat = mkIdentPat ident.Name isRef isMut
            let tyOpt =
                match ident.Type with
                | Fable.LambdaType _
                | Fable.DelegateType _ -> None
                | _ ->
                    let typegen = { FavourClosureTraitOverFunctionPointer = true
                                    IsParamType = false
                                    TakingOwnership = true }
                    let ctx = { ctx with Typegen = typegen }
                    transformType com ctx ident.Type
                    |> Some
            let tyOpt =
                tyOpt |> Option.map (fun ty ->
                    if ident.IsMutable
                    then ty |> makeMutTy com ident.Type
                    else ty)
            let init =
                transformLeaveContextByValue com ctx ident.Type (Some ident.Name) value
            let expr =
                if ident.IsMutable
                then init |> makeMutValue com ctx ident.Type
                else init
            let local = mkLocal [] pat tyOpt (Some expr)
            // TODO : traverse body and follow references to decide on if this should be wrapped or not]
            let scopedVarAttrs = {  IsRef = isRef
                                    IsMutable = isMut
                                    IsRefCountWrapped = shouldBeRefCountWrapped com ident.Type
                                    MultipleUsages = UseageTracking.hasMultipleUsages ident.Name usages
                                     }
            let ctxNext = { ctx with ScopedSymbols = ctx.ScopedSymbols |> Map.add ident.Name scopedVarAttrs }
            mkLocalStmt local, ctxNext
        let ctx, letStmtsRev = //Context needs to be threaded through all lets, appending itself to ScopedSymbols each time
            ((ctx, []), bindings)
            ||> List.fold (fun (ctx, lst) (ident, expr) ->
                let (stmt, ctxNext) = makeLetStmt ctx (ident, expr)
                (ctxNext, stmt::lst)
                )
        let letStmts = letStmtsRev |> List.rev

        let bodyStmts =
            match body with
            | Fable.Sequential exprs ->
                let exprs = flattenSequential body
                List.map (transformAsStmt com ctx) exprs
            | _ ->
                [transformAsStmt com ctx body]
        (letStmts @ bodyStmts) |> mkBlock |> mkBlockExpr

    let transformSequential (com: IRustCompiler) ctx exprs =
        exprs
        |> List.map (transformAsStmt com ctx)
        |> mkBlock
        |> mkBlockExpr

    let transformIfThenElse (com: IRustCompiler) ctx range guard thenBody elseBody =
        let guardExpr = com.TransformAsExpr(ctx, guard)
                        |> maybeUnwrapRef com ctx guard.Type (tryGetIdent guard |> Option.defaultValue "")
        let thenExpr = com.TransformAsExpr(ctx, thenBody)
        match elseBody with
        | Fable.Value(Fable.UnitConstant, _) ->
            mkIfThenExpr guardExpr thenExpr //?loc=range)
        | _ ->
            let elseExpr = com.TransformAsExpr(ctx, elseBody)
            mkIfThenElseExpr guardExpr thenExpr elseExpr //?loc=range)

    let transformWhileLoop (com: IRustCompiler) ctx range label guard body =
        // TODO: loop label
        let guardExpr = com.TransformAsExpr(ctx, guard)
        let bodyExpr = com.TransformAsExpr(ctx, body)
        mkWhileExpr guardExpr bodyExpr //?loc=range)

    let transformForLoop (com: IRustCompiler) ctx range isUp (var: Fable.Ident) start limit body =
        let startExpr = com.TransformAsExpr(ctx, start)
        let limitExpr = com.TransformAsExpr(ctx, limit)
        let bodyExpr = com.TransformAsExpr(ctx, body)
        let varName = if var.Name = "forLoopVar" then "_" + var.Name else var.Name
        let varPat = mkIdentPat varName false false
        let rangeExpr =
            if isUp then
                mkRangeExpr (Some startExpr) (Some limitExpr) true
            else
                // downward loop
                let rangeExpr =
                    mkRangeExpr (Some limitExpr) (Some startExpr) true
                    |> mkParenExpr
                mkMethodCallExpr "rev" None rangeExpr []
        mkForLoopExpr varPat rangeExpr bodyExpr //?loc=range)

    let transformTryCatch (com: IRustCompiler) ctx range body catch finalizer =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let bodyExpr = com.TransformAsExpr(ctx, body)
        mkTryBlockExpr bodyExpr // TODO: add catch and finally

    let transformCurriedApply (com: IRustCompiler) ctx range expr args =
        let callee = com.TransformAsExpr(ctx, expr)
        let ctx = { ctx with Typegen = { ctx.Typegen with TakingOwnership = false}}
        callFunction com ctx range callee args
        // let handler =
        //     catch |> Option.map (fun (param, body) ->
        //         CatchClause.catchClause(identAsPattern param, transformBlock com ctx returnStrategy body))
        // let finalizer =
        //     finalizer |> Option.map (transformBlock com ctx None)
        // [|Statement.tryStatement(transformBlock com ctx returnStrategy body,
        //     ?handler=handler, ?finalizer=finalizer, ?loc=r)|]

(*
    let transformBindingExprBody (com: IRustCompiler) (ctx: Context) (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        | Function(args, body) ->
            let name = Some var.Name
            transformFunctionWithAnnotations com ctx name args body
            |> makeArrowFunctionExpression name
        | _ ->
            if var.IsMutable then
                com.TransformAsExpr(ctx, value)
            else
                com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type

    let transformBindingAsExpr (com: IRustCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        transformBindingExprBody com ctx var value
        |> assign None (identAsExpr var)

    let transformBindingAsStatements (com: IRustCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isJsStatement ctx false value then
            let varPattern, varExpr = identAsPattern var, identAsExpr var
            let decl = Statement.variableDeclaration(varPattern)
            let body = com.TransformAsStatements(ctx, Some(Assign varExpr), value)
            Array.append [|decl|] body
        else
            let value = transformBindingExprBody com ctx var value
            let decl = varDeclaration (identAsPattern var) var.IsMutable value |> Declaration.VariableDeclaration |> Declaration
            [|decl|]
*)
    let transformTest (com: IRustCompiler) ctx range kind (expr: Fable.Expr): Rust.Expr =
        match kind with
        // | Fable.TypeTest t ->
        //     transformTypeTest com ctx range expr t
        | Fable.OptionTest nonEmpty ->
            let test = if nonEmpty then "is_some" else "is_none"
            let expr = com.TransformAsExpr(ctx, expr)
            mkMethodCallExpr test None expr []
        // | Fable.ListTest nonEmpty ->
        //     let expr = com.TransformAsExpr(ctx, expr)
        //     let expr = libCall com ctx range "List" "isEmpty" [|expr|]
        //     if nonEmpty then mkUnaryExpr Rust.UnOp.Not expr else expr //, ?loc=range
        | Fable.UnionCaseTest tag ->
            match expr.Type with
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                assert(ent.IsFSharpUnion)
                // let genArgs = genArgs |> List.map (transformType com ctx) |> mkGenericArgs // TODO:
                let unionCase = ent.UnionCases |> List.item tag
                let path = mkFullNamePath unionCase.FullName None
                let fields =
                    match expr with
                    | Fable.IdentExpr id ->
                        unionCase.UnionCaseFields
                        |> Seq.mapi (fun i _field ->
                            let fieldName = $"{id.Name}_{tag}_{i}"
                            mkIdentPat fieldName false false
                        )
                        |> Seq.toList
                    | _ ->
                        [WILD_PAT]
                let pat = mkTupleStructPat path fields
                let expr = com.TransformAsExpr(ctx, expr)
                               |>  maybeUnwrapRef com ctx expr.Type (tryGetIdent expr |> Option.defaultValue "")
                mkLetExpr pat expr
            | _ ->
                failwith "Should not happen"

        // TODO: remove this catch-all
        | _ -> TODO_EXPR (sprintf "%A" expr)

    let transformSwitch (com: IRustCompiler) ctx (evalExpr: Fable.Expr) cases defaultCase targets: Rust.Expr =
        let namesForIndex evalType evalName caseIndex = //todo refactor with below
            match evalType with
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                if ent.IsFSharpUnion then
                    let unionCase = ent.UnionCases |> List.item caseIndex
                    match evalName with
                    | Some idName ->
                        unionCase.UnionCaseFields
                        |> Seq.mapi (fun i _field ->
                            $"{idName}_{caseIndex}_{i}", idName, _field.FieldType
                        )
                    | _ -> Seq.empty
                else Seq.empty
            | _ -> Seq.empty
            |> Seq.toList
        let makeArm pat targetIndex boundValues (extraVals: (string * string * Fable.Type) list)=
            let attrs = []
            let guard = None // TODO:
            let idents, (bodyExpr: Fable.Expr) = targets |> List.item targetIndex // TODO:
            let vars = idents |> List.map (fun (id: Fable.Ident) -> id.Name)
            // TODO: vars, boundValues
            let body =
                //com.TransformAsExpr(ctx, bodyExpr)
                let usages = UseageTracking.calcIdentUsages bodyExpr

                let symbolsAndNames =
                    let fromIdents =
                        idents
                        |> List.map(fun id ->
                            id.Name, {  IsRef = true
                                        IsMutable = id.IsMutable
                                        IsRefCountWrapped = shouldBeRefCountWrapped com id.Type
                                        MultipleUsages = UseageTracking.hasMultipleUsages id.Name usages })

                    let fromExtra =
                        extraVals
                        |> List.map (fun (name, friendlyName, t) ->
                            friendlyName, { IsRef = true
                                            IsMutable = false
                                            IsRefCountWrapped = shouldBeRefCountWrapped com t
                                            MultipleUsages = UseageTracking.hasMultipleUsages friendlyName usages })
                    fromIdents @ fromExtra
                let scopedSymbolsNext =
                    Helpers.Map.merge ctx.ScopedSymbols (symbolsAndNames |> Map.ofList)
                let ctx = { ctx with ScopedSymbols = scopedSymbolsNext; Typegen = { ctx.Typegen with TakingOwnership = true } }
                transformLeaveContextByValue com ctx (bodyExpr.Type) None bodyExpr
            mkArm attrs pat guard body
        let makeUnionCasePat evalType evalName caseIndex =
            match evalType with
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                if ent.IsFSharpUnion then
                    let genArgs = genArgs |> List.map (transformType com ctx) |> mkGenericArgs
                    let unionCase = ent.UnionCases |> List.item caseIndex
                    let fields =
                        match evalName with
                        | Some idName ->
                            unionCase.UnionCaseFields
                            |> Seq.mapi (fun i _field ->
                                let fieldName = $"{idName}_{caseIndex}_{i}"
                                mkIdentPat fieldName false false
                            )
                            |> Seq.toList
                        | _ ->
                            [WILD_PAT]
                    let path = mkFullNamePath unionCase.FullName None
                    Some (mkTupleStructPat path fields)
                else
                    None
            | _ ->
                None
        let evalType, evalName =
            match evalExpr with
            | Fable.Get (Fable.IdentExpr id, Fable.UnionTag, _, _) ->
                id.Type, Some id.Name
            | _ -> evalExpr.Type, None
        let arms =
            cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                let patOpt =
                    match caseExpr with
                    | Fable.Value (Fable.NumberConstant (tag, Int32, None), r) ->
                        makeUnionCasePat evalType evalName (int tag)
                    | _ -> None
                let pat =
                    match patOpt with
                    | Some pat -> pat
                    | _ -> com.TransformAsExpr(ctx, caseExpr) |> mkLitPat
                let extraVals = namesForIndex evalType evalName targetIndex
                makeArm pat targetIndex (boundValues) extraVals
            )
        let defaultArm =
            let targetIndex, boundValues = defaultCase
            // To see if the default arm should actually be a union case pattern, we have to
            // examine its body to see if it starts with union field get. // TODO: look deeper
            // If it does, we'll replace the wildcard "_" with a union case pattern
            let idents, bodyExpr = targets |> List.item targetIndex
            let patOpt =
                match bodyExpr with
                | Fable.Get (Fable.IdentExpr id, Fable.UnionField(caseIndex, _), _, _)
                    when Some id.Name = evalName && id.Type = evalType->
                    makeUnionCasePat evalType evalName caseIndex
                | _ -> None
            let pat = patOpt |> Option.defaultValue WILD_PAT
            let extraVals = namesForIndex evalType evalName targetIndex
            makeArm pat targetIndex boundValues extraVals
        let expr = com.TransformAsExpr(ctx, evalExpr)
                    |> prepareRefForPatternMatch com ctx evalType (evalName |> Option.defaultValue "")
        mkMatchExpr expr (arms @ [defaultArm])

(*
    let transformSwitch (com: IRustCompiler) ctx useBlocks returnStrategy evalExpr cases defaultCase: Rust.Stmt =
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
*)
    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.length idents = List.length values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
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

    let transformDecisionTreeSuccessAsExpr (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)
(*
    let transformDecisionTreeSuccessAsStatements (com: IRustCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Rust.Stmt[] =
        match returnStrategy with
        | Some(Target targetId) as target ->
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
*)
    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.Binary(BinaryEqualStrict, expr, right), _, _) ->
                Some(expr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, None), None)
                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | _ -> false
        let rec checkInner cases evalExpr treeExpr =
            match treeExpr with
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _)
                                    when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                    let cases = (caseExpr, targetIndex, boundValues)::cases |> List.rev
                    Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr ->
                    checkInner ((caseExpr, targetIndex, boundValues)::cases) evalExpr treeExpr
            | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
            | _ -> None
        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
            checkInner [(caseExpr, targetIndex, boundValues)] evalExpr treeExpr
        | _ -> None

    let transformDecisionTreeAsExpr (com: IRustCompiler) ctx targets (expr: Fable.Expr): Rust.Expr =
        match transformDecisionTreeAsSwitch expr with
        | Some(evalExpr, cases, defaultCase) ->
            transformSwitch com ctx evalExpr cases defaultCase targets
        | None ->
            let ctx = { ctx with DecisionTargets = targets }
            com.TransformAsExpr(ctx, expr)
(*
    let transformDecisionTreeAsExpr (com: IRustCompiler) (ctx: Context) targets expr: Rust.Expr =
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
    let transformDecisionTreeWithTwoSwitches (com: IRustCompiler) ctx returnStrategy
                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
        // Declare target and bound idents
        let targetId = getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent
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
        let targetAssign = Target(ident targetId)
        let ctx = { ctx with DecisionTargets = targets }
        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases = groupSwitchCases (Fable.Number Int32) cases (defaultIndex, defaultBoundValues)
            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number Int32)
            let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
            [|multiVarDecl; switch1; switch2|]
        | None ->
            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
            [| yield multiVarDecl; yield! decisionTree; yield switch2 |]

    let transformDecisionTreeAsStatements (com: IRustCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Rust.Stmt[] =
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
                    idents
                    |> List.exists (fun i -> FableTransforms.isIdentUsed i.Name expr)
                    |> function
                        | true -> idents, expr
                        | false -> [], expr)
            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs |> List.exists (fun idx ->
                    targets.[idx] |> fst |> List.isEmpty |> not)
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
*)
    let rec transformAsExpr (com: IRustCompiler) ctx (fableExpr: Fable.Expr): Rust.Expr =
        match fableExpr with

        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        // | Fable.Curry(e, arity, _, r) -> transformCurry com ctx r e arity

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> transformIdentGet com ctx None id // TODO: range

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, _name) ->
            // transformFunctionWithAnnotations com ctx name [arg] body
            // |> makeArrowFunctionExpression name
            transformLambda com ctx [arg] body

        | Fable.Delegate(args, body, _name) ->
            // transformFunctionWithAnnotations com ctx name args body
            // |> makeArrowFunctionExpression name
            transformLambda com ctx args body

        // | Fable.ObjectExpr (members, _, baseCall) ->
        //    transformObjectExpr com ctx members baseCall

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.CurriedApply(callee, args, t, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, t, range) ->
            transformOperation com ctx t range kind

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            transformIfThenElse com ctx r guardExpr thenExpr elseExpr

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            // flatten nested let binding expressions
            let bindings, body = flattenLet [] fableExpr
            transformLet com ctx bindings body
            // if ctx.HoistVars [ident] then
            //     let assignment = transformBindingAsExpr com ctx ident value
            //     Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
            // else iife com ctx expr

        // | Fable.LetRec(bindings, body) ->
        //     if ctx.HoistVars(List.map fst bindings) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        | Fable.Sequential exprs ->
            // flatten nested sequential expressions
            let exprs = flattenSequential fableExpr
            transformSequential com ctx exprs

        | Fable.Emit(info, _, range) ->
            // if info.IsJsStatement then iife com ctx expr
            // else transformEmit com ctx range info
            transformEmit com ctx range info

        | Fable.WhileLoop(guard, body, label, range) ->
            transformWhileLoop com ctx range label guard body

        | Fable.ForLoop (var, start, limit, body, isUp, range) ->
            transformForLoop com ctx range isUp var start limit body

        | Fable.TryCatch (body, catch, finalizer, range) ->
            transformTryCatch com ctx range body catch finalizer

        | Fable.Extended(Fable.Throw(expr, _typ), _range) ->
            mkMacroExpr "panic" [transformAsExpr com ctx expr]

        // | Fable.Extended(kind, _) ->
        //     match kind with
        //     | Fable.Throw _ ->
        //     | Fable.Return _ ->
        //     | Fable.Break _ ->
        //     | Fable.Debugger ->
        //     | Fable.Curry _ ->

        // TODO: remove this catch-all
        | _ -> TODO_EXPR (sprintf "%A" fableExpr)
(*
    let rec transformAsStatements (com: IRustCompiler) ctx returnStrategy
                                    (expr: Fable.Expr): Rust.Stmt array =
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

            let a = start |> varDeclaration (typedIdent com ctx var |> Pattern.Identifier) true

            [|Statement.forStatement(
                transformBlock com ctx None body,
                start |> varDeclaration (typedIdent com ctx var |> Pattern.Identifier) true,
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
                    |> List.map (fun (id, tcArg) ->
                        makeTypedIdent id.Type tcArg |> typedIdent com ctx)
                let varDecls =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) ->
                        id |> typedIdent com ctx, Some (Expression.identifier(tcArg)))
                    |> multiVarDeclaration Const

                let body = Array.append [|varDecls|] body.Body
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body = BlockStatement(Array.append body [|Statement.breakStatement()|])
                args', Statement.labeledStatement(Identifier.identifier(tc.Label), Statement.whileStatement(Expression.booleanLiteral(true), body))
                |> Array.singleton |> BlockStatement
            | _ -> args |> List.map (typedIdent com ctx), body
        let body =
            if declaredVars.Count = 0 then body
            else
                let varDeclStatement = multiVarDeclaration Let [for v in declaredVars -> typedIdent com ctx v, None]
                BlockStatement(Array.append [|varDeclStatement|] body.Body)
        args |> List.mapToArray Pattern.Identifier, body

    let declareEntryPoint _com _ctx (funcExpr: Rust.Expr) =
        let argv = emitExpression None "typeof process === 'object' ? process.argv.slice(2) : []" []
        let main = Expression.callExpression(funcExpr, [|argv|])
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(emitExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        PrivateModuleDeclaration(ExpressionStatement(main))
*)
    let rec tryFindEntryPoint decl: string list option =
        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members
            |> List.tryPick tryFindEntryPoint
            |> Option.map (fun name -> decl.Name :: name)
        | Fable.MemberDeclaration decl ->
            decl.Info.Attributes
            |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.entryPoint)
            |> Option.map (fun _ -> [decl.Name])
        | Fable.ActionDeclaration decl -> None
        | Fable.ClassDeclaration decl -> None

    let getEntryPointDecls decls =
        let entryPoint =
            decls |> List.tryPick tryFindEntryPoint
        match entryPoint with
        | Some path ->
            let strBody = [
                "let toMutableStr = |x: &String| RefCell::from(Rc::from(x.to_owned()))"
                "let args: Vec<String> = std::env::args().collect()"
                "let args: Vec<RefCell<Rc<str>>> = args[1..].iter().map(toMutableStr).collect()"
                (String.concat "::" path) + "(&Rc::from(args.to_owned()))"
            ]
            let fnBody = strBody |> Seq.map mkEmitSemiStmt |> mkBlock |> Some

            let attrs = []
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            [mkFnItem attrs "main" fnKind]
        | None -> []

    let transformModuleMember com ctx membName isMutable (fableExpr: Fable.Expr) =
        let expr = transformAsExpr com ctx fableExpr
        let attrs = []
        let ty = transformType com ctx fableExpr.Type
        let item = mkStaticItem attrs membName ty isMutable (Some expr)
        [item]
(*
    let declareModuleMember isPublic membName isMutable (expr: Rust.Expr) =
        let membName' = Pattern.identifier(membName)
        let membName = Identifier.identifier(membName)
        let decl: Declaration =
            match expr with
            | ClassExpression(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
                Declaration.classDeclaration(
                    body,
                    ?id = Some membName,
                    ?superClass = superClass,
                    ?superTypeParameters = superTypeParameters,
                    ?typeParameters = typeParameters,
                    ?implements = implements)
            | FunctionExpression(_, ``params``, body, returnType, typeParameters, _) ->
                Declaration.functionDeclaration(
                    ``params``, body, membName,
                    ?returnType = returnType,
                    ?typeParameters = typeParameters)
            | _ -> varDeclaration membName' isMutable expr |> Declaration.VariableDeclaration
        if not isPublic then PrivateModuleDeclaration(decl |> Declaration)
        else ExportNamedDeclaration(decl)

    let makeEntityTypeParamDecl (com: IRustCompiler) _ctx (ent: Fable.Entity) =
        if com.Options.Typescript then
            getEntityGenParams ent |> makeTypeParamDecl
        else
            None

    let getClassImplements com ctx (ent: Fable.Entity) =
        let mkNative genArgs typeName =
            let id = Identifier.identifier(typeName)
            let typeParamInst = makeGenTypeParamInst com ctx genArgs
            ClassImplements.classImplements(id, ?typeParameters=typeParamInst) |> Some
//        let mkImport genArgs moduleName typeName =
//            let id = makeImportTypeId com ctx moduleName typeName
//            let typeParamInst = makeGenTypeParamInst com ctx genArgs
//            ClassImplements(id, ?typeParameters=typeParamInst) |> Some
        ent.AllInterfaces |> Seq.choose (fun ifc ->
            match ifc.Entity.FullName with
            | "Fable.Core.JS.Set`1" -> mkNative ifc.GenericArgs "Set"
            | "Fable.Core.JS.Map`2" -> mkNative ifc.GenericArgs "Map"
            | _ -> None
        )

    let getUnionFieldsAsIdents (_com: IRustCompiler) _ctx (_ent: Fable.Entity) =
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

    let getEntityFieldsAsProps (com: IRustCompiler) ctx (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            getUnionFieldsAsIdents com ctx ent
            |> Array.map (fun id ->
                let prop = identAsExpr id
                let ta = typeAnnotation com ctx id.Type
                ObjectTypeProperty.objectTypeProperty(prop, ta))
        else
            ent.FSharpFields
            |> Seq.map (fun field ->
                let prop, computed = memberFromName field.Name
                let ta = typeAnnotation com ctx field.FieldType
                let isStatic = if field.IsStatic then Some true else None
                ObjectTypeProperty.objectTypeProperty(prop, ta, computed_=computed, ?``static``=isStatic))
            |> Seq.toArray

    let declareClassType (com: IRustCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) (baseExpr: Rust.Expr option) classMembers =
        let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let implements =
            if com.Options.Typescript then
                let implements = Util.getClassImplements com ctx ent |> Seq.toArray
                if Array.isEmpty implements then None else Some implements
            else None
        let classCons = makeClassConstructor consArgs consBody
        let classFields =
            if com.Options.Typescript then
                getEntityFieldsAsProps com ctx ent
                |> Array.map (fun (ObjectTypeProperty(key, value, _, _, ``static``, _, _, _)) ->
                    let ta = value |> TypeAnnotation |> Some
                    ClassMember.classProperty(key, ``static``=``static``, ?typeAnnotation=ta))
            else Array.empty
        let classMembers = Array.append [| classCons |] classMembers
        let classBody = ClassBody.classBody([| yield! classFields; yield! classMembers |])
        let classExpr = Expression.classExpression(classBody, ?superClass=baseExpr, ?typeParameters=typeParamDecl, ?implements=implements)
        classExpr |> declareModuleMember ent.IsPublic entName false

    let declareType (com: IRustCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr classMembers: ModuleDeclaration list =
        let typeDeclaration = declareClassType com ctx ent entName consArgs consBody baseExpr classMembers
        let reflectionDeclaration =
            let ta =
                if com.Options.Typescript then
                    makeImportTypeAnnotation com ctx [] "Reflection" "TypeInfo"
                    |> TypeAnnotation |> Some
                else None
            let genArgs = Array.init (ent.GenericParameters.Length) (fun i -> "gen" + string i |> makeIdent)
            let generics = genArgs |> Array.map identAsExpr
            let body = transformReflectionInfo com ctx None ent generics
            let args = genArgs |> Array.map (fun x -> Pattern.identifier(x.Name, ?typeAnnotation=ta))
            let returnType = ta
            makeFunctionExpression None (args, body, returnType, None)
            |> declareModuleMember ent.IsPublic (entName + Naming.reflectionSuffix) false
        [typeDeclaration; reflectionDeclaration]
*)
    let typedParam (com: IRustCompiler) ctx (ident: Fable.Ident) =
        let typegen = { FavourClosureTraitOverFunctionPointer = true
                        IsParamType = true
                        TakingOwnership = false }
        let ctx = { ctx with Typegen = typegen }
        let ty = transformParamType com ctx ident.Type
        let isRef = false
        let isMut = false
        mkParamFromType ident.Name ty isRef isMut //?loc=id.Range)

    let inferredParam (com: IRustCompiler) ctx (ident: Fable.Ident) =
        let isRef = false
        let isMut = false
        mkInferredParam ident.Name isRef isMut //?loc=id.Range)

    let transformFunction (com: IRustCompiler) ctx (args: Fable.Ident list) (body: Fable.Expr) =
        let argTypes = args |> List.map (fun arg -> arg.Type)
        let genTypeParams = Util.getGenericTypeParams (argTypes @ [body.Type])
        let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams
        let ctx = { ctx with ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams }
        let fnRetTy =
            if body.Type = Fable.Unit then VOID_RETURN_TY
            else transformType com ctx body.Type |> Rust.FnRetTy.Ty
        let inputs =
            args
            |> List.filter (fun id -> id.Type <> Fable.Unit)
            |> List.map (typedParam com ctx)
        let fnDecl = mkFnDecl inputs fnRetTy
        let ctx =
            let scopedSymbols =
                let usages = UseageTracking.calcIdentUsages body

                (ctx.ScopedSymbols, args)
                ||> List.fold (fun acc arg ->
                    //todo optimizations go here
                    let scopedVarAttrs = {  IsRef = shouldBePassByRefForParam com arg.Type
                                            IsMutable = arg.IsMutable
                                            IsRefCountWrapped = shouldBeRefCountWrapped com arg.Type
                                            MultipleUsages = UseageTracking.hasMultipleUsages arg.Name usages

                                            }
                    acc |> Map.add arg.Name scopedVarAttrs)
            { ctx with ScopedSymbols = scopedSymbols }
        let fnBody = transformLeaveContextByValue com ctx body.Type None body
        fnDecl, fnBody, newTypeParams

    let transformLambda (com: IRustCompiler) ctx (args: Fable.Ident list) (body: Fable.Expr) =
        let fnRetTy = VOID_RETURN_TY
        let inputs =
            let ctx = { ctx with Typegen = { ctx.Typegen with IsParamType = true } }
            args
            |> List.filter (fun id -> id.Type <> Fable.Unit)
            |> List.map (fun i -> mkParamFromType i.Name (transformParamType com ctx i.Type) false false)
        let fnDecl = mkFnDecl inputs fnRetTy
        let ctx =
            let usages = UseageTracking.calcIdentUsages body
            let scopedSymbols =
                (ctx.ScopedSymbols, args)
                ||> List.fold (fun acc arg ->
                    //todo optimizations go here
                    let scopedVarAttrs = {
                        IsRef = shouldBePassByRefForParam com arg.Type
                        IsMutable = arg.IsMutable
                        IsRefCountWrapped = shouldBeRefCountWrapped com arg.Type
                        MultipleUsages = UseageTracking.hasMultipleUsages arg.Name usages
                        }
                    acc |> Map.add arg.Name scopedVarAttrs)
            { ctx with ScopedSymbols = scopedSymbols }
        let fnBody = transformLeaveContextByValue com ctx body.Type None body
        let closedOverCloneableNames =
            let paramNamesToExclude = args |> List.map (fun arg -> arg.Name) |> Set.ofList
            let mutable names:ResizeArray<string> = ResizeArray()
            FableTransforms.deepExists
                (function | Fable.Expr.IdentExpr ident ->
                                if shouldBeRefCountWrapped com ident.Type && not (Set.contains ident.Name paramNamesToExclude) then
                                    names.Add(ident.Name)
                                else
                                    match ident.Type with
                                    | Fable.Type.LambdaType _
                                    | Fable.Type.DelegateType _ ->
                                        //Closures may capture Ref counted vars, so by cloning the actual closure, you inadvertently clone all attached ref counted vars
                                        names.Add(ident.Name)
                                    | _ -> ()
                                    ()
                                false
                          | _ -> false) body
                |> ignore
            names
            |> Seq.toList
            |> List.distinct //there seem to be duplicates in some contexts?
        let closureExpr = mkClosureExpr fnDecl fnBody
        if closedOverCloneableNames.Length > 0 then
            mkBlockExpr (mkBlock [
                for name in closedOverCloneableNames do
                    let pat = mkIdentPat (name) false false
                    let identExpr = com.TransformAsExpr(ctx, makeIdentExpr name)
                    let nexpr = makeClone identExpr
                    let letExpr = mkLetExpr pat nexpr
                    yield letExpr |> Rust.AST.Types.StmtKind.Semi |> mkStmt

                yield closureExpr |> Rust.AST.Types.StmtKind.Expr |> mkStmt
            ])
        else closureExpr

    let transformModuleFunction (com: IRustCompiler) ctx (info: Fable.MemberInfo) (membName: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let fnDecl, fnBody, fnGenericNames = transformFunction com ctx args body
        let fnBodyBlock =
            if body.Type = Fable.Unit
            then mkSemiBlock fnBody
            else mkExprBlock fnBody
        let header = DEFAULT_FN_HEADER
        let generics =
            //it does not seem like the current AST gives access to the actual generic params, so this is inferring them from useage,
            //which is a hack/temporary solution that only works for trivial cases.
            let parameters =
                fnGenericNames
                |> Set.toList
                |> List.map(fun name ->
                    //todo map constraints
                    mkGenericParam [] (mkIdent (name)) [] false (Rust.AST.Types.GenericParamKind.Type None)
                    )
            //todo map return type if encoded as generic param
            parameters |> mkGenerics
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs =
            info.Attributes
            |> Seq.filter (fun att -> att.Entity.FullName.EndsWith(".FactAttribute"))
            |> Seq.map (fun _ -> mkAttr "test" [])
        let fnItem = mkFnItem attrs membName kind
        [fnItem]

(*
        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx (NonAttached membName) info.HasSpread args body
        let expr = Expression.functionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)
        info.Attributes
        |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)
        |> function
        | true -> declareEntryPoint com ctx expr
        | false -> declareModuleMember info.IsPublic membName false expr

    let transformAction (com: IRustCompiler) ctx expr =
        // let statements = transformAsStatements com ctx None expr
        // let hasVarDeclarations =
        //     statements |> Array.exists (function
        //         | Declaration(Declaration.VariableDeclaration(_)) -> true
        //         | _ -> false)
        // if hasVarDeclarations then
        //     [ Expression.callExpression(Expression.functionExpression([||], BlockStatement(statements)), [||])
        //       |> ExpressionStatement |> PrivateModuleDeclaration ]
        // else statements |> Array.mapToList (fun x -> PrivateModuleDeclaration(x))

    let transformAttachedProperty (com: IRustCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not memb.Info.IsInstance
        let kind = if memb.Info.IsGetter then ClassGetter else ClassSetter
        let args, body, _returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) false memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        ClassMember.classMethod(kind, key, args, body, computed_=computed, ``static``=isStatic)
        |> Array.singleton

    let transformAttachedMethod (com: IRustCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not memb.Info.IsInstance
        let makeMethod name args body =
            let key, computed = memberFromName name
            ClassMember.classMethod(ClassFunction, key, args, body, computed_=computed, ``static``=isStatic)
        let args, body, _returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) memb.Info.HasSpread memb.Args memb.Body
        [|
            yield makeMethod memb.Name args body
            if memb.Info.IsEnumerator then
                yield makeMethod "Symbol.iterator" [||] (enumerator2iterator com ctx)
        |]
*)
    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let generics =
            ent.GenericParameters |> List.map (fun x -> x.Name)
            |> mkGenericParams
        let variants =
            ent.UnionCases |> Seq.map (fun uci ->
                let name = uci.Name
                let fields =
                    uci.UnionCaseFields |> List.map (fun fi ->
                        let ty = transformType com ctx fi.FieldType
                        mkField [] fi.Name ty)
                mkTupleVariant [] name fields
            )
        let attrs = [mkAttr "derive" ["Clone";"PartialEq";"Debug"]]
        let enumItem = mkEnumItem attrs entName variants generics
        [enumItem] // TODO: add traits for attached members

    let transformClass (com: IRustCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let generics =
            ent.GenericParameters |> List.map (fun x -> x.Name)
            |> mkGenericParams
        let fields =
            ent.FSharpFields |> Seq.map (fun fi ->
                let ty = transformType com ctx fi.FieldType
                let ty =
                    if fi.IsMutable
                    then ty |> makeMutTy com fi.FieldType
                    else ty
                mkField [] fi.Name ty
            )
        let attrs = [mkAttr "derive" ["Clone";"PartialEq";"Debug"]];
        let structItem = mkStructItem attrs entName fields generics
        [structItem]// TODO: add traits for attached members

    let transformClassDecl (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let ent = com.GetEntity(decl.Entity)
        let classMembers = [] // TODO:
        if ent.IsFSharpUnion
        then transformUnion com ctx ent decl.Name classMembers
        else
            let ctorItem =
                match decl.Constructor with
                | Some ctor ->
                    transformDeclaration com ctx (Fable.MemberDeclaration ctor) |> Some
                | _ ->
                    // ent.MembersFunctionsAndValues |> makeCompilerGeneratedCtor ?
                    None
                |> Option.toList |> List.collect id
            transformClass com ctx ent decl.Name classMembers
            @ ctorItem

(*
    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getUnionFieldsAsIdents com ctx ent
        let args =
            [| typedIdent com ctx fieldIds.[0] |> Pattern.Identifier
               typedIdent com ctx fieldIds.[1] |> Pattern.Identifier |> restElement |]
        let body =
            BlockStatement([|
                yield callSuperAsStatement []
                yield! fieldIds |> Array.map (fun id ->
                    let left = get None thisExpr id.Name
                    let right =
                        match id.Type with
                        | Fable.Number _ ->
                            Expression.binaryExpression(BinaryOrBitwise, identAsExpr id, Expression.numericLiteral(0.))
                        | _ -> identAsExpr id
                    assign None left right |> ExpressionStatement)
            |])
        let cases =
            let body =
                ent.UnionCases
                |> Seq.map (getUnionCaseName >> makeStrConst)
                |> Seq.toList
                |> makeArray com ctx
                |>  Statement.returnStatement
                |> Array.singleton
                |> BlockStatement
            ClassMember.classMethod(ClassFunction, Expression.identifier("cases"), [||], body)

        let baseExpr = libValue com ctx "Types" "Union" |> Some
        let classMembers = Array.append [|cases|] classMembers
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithCompilerGeneratedConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
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
                    let right = wrapIntExpression field.FieldType args.[i]
                    assign None left right |> ExpressionStatement)
                |> Seq.toArray
            |])
        let typedPattern x = typedIdent com ctx x
        let args = fieldIds |> Array.map (typedPattern >> Pattern.Identifier)
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithImplicitConstructor (com: IRustCompiler) ctx (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
        let classEnt = com.GetEntity(classDecl.Entity)
        let classIdent = Expression.identifier(classDecl.Name)
        let consArgs, consBody, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx ClassConstructor cons.Info.HasSpread cons.Args cons.Body

        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.Options.Typescript then
                let genParams = getEntityGenParams classEnt
                let returnType = getGenericTypeAnnotation com ctx classDecl.Name genParams
                let typeParamDecl = makeTypeParamDecl genParams |> mergeTypeParamDecls typeParamDecl
                returnType, typeParamDecl
            else
                returnType, typeParamDecl

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
            yield declareModuleMember cons.Info.IsPublic cons.Name false exposedCons
        ]
*)
    let preludeDecls = [
        mkSimpleUseItem [] ["std"; "rc"; "Rc"] None
        mkSimpleUseItem [] ["core"; "cell"; "Cell"] None
        mkSimpleUseItem [] ["core"; "cell"; "RefCell"] None
        ]

    let rec transformDeclaration (com: IRustCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            // TODO: collect use decls from usage in body
            let useDecls = []
            let memberDecls = decl.Members |> List.collect (transformDeclaration com ctx)
            let attrs =  []
            let modDecls = preludeDecls @ useDecls @ memberDecls
            let modItem = modDecls |> mkModItem attrs decl.Name
            [modItem]

        | Fable.ActionDeclaration decl ->
            // TODO: use ItemKind.Static with IIFE closure?
            [TODO_ITEM "module_do_bindings_not_implemented_yet"]
            // withCurrentScope ctx decl.UsedNames <| fun ctx ->
            //     transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                // TODO: decl.Info.IsPublic
                if decl.Info.IsValue
                then transformModuleMember com ctx decl.Name decl.Info.IsMutable decl.Body
                else transformModuleFunction com ctx decl.Info decl.Name decl.Args decl.Body

                // if decl.ExportDefault then
                //     decls @ [ExportDefaultDeclaration(Choice2Of2(Expression.identifier(decl.Name)))]
                // else decls

        | Fable.ClassDeclaration decl ->
            transformClassDecl com ctx decl
            // let ent = decl.Entity
            // let classMembers =
            //     decl.AttachedMembers
            //     |> List.toArray
            //     |> Array.collect (fun memb ->
            //         withCurrentScope ctx memb.UsedNames <| fun ctx ->
            //             if memb.Info.IsGetter || memb.Info.IsSetter then
            //                 transformAttachedProperty com ctx memb
            //             else
            //                 transformAttachedMethod com ctx memb)
            // match decl.Constructor with
            // | Some cons ->
            //     withCurrentScope ctx cons.UsedNames <| fun ctx ->
            //         transformClassWithImplicitConstructor com ctx decl classMembers cons
            // | None ->
            //     let ent = com.GetEntity(ent)
            //     if ent.IsFSharpUnion then transformUnion com ctx ent decl.Name classMembers
            //     else transformClassWithCompilerGeneratedConstructor com ctx ent decl.Name classMembers

    let transformImports ctx (imports: string seq): Rust.Item list =
        imports
        |> List.ofSeq
        |> List.sort
        |> List.collect (fun (i: string) ->
            match i.Split('|') with
            | [| path; moduleNamespace; selector |] ->
                let hashedPath = hash path
                let name = System.String.Format("import_{0:x}", hashedPath, moduleNamespace)
                let useName = System.String.Format("import_{0:x}::{1:x}", hashedPath, moduleNamespace)
                let attrs = [mkEqAttr "path" (path.Replace(".js", ".rs") |> sprintf "\"%s\"")] // TODO: relative path
                let item1 = mkUnloadedModItem attrs name
                let item2 =
                    match selector with
                    | "" | "*" | "default" ->
                        mkGlobUseItem [] [useName]
                    | _ ->
                        let parts = selector.Split('.') |> List.ofSeq
                        mkSimpleUseItem [] (useName::parts) None
                [item1; item2]
            | _ -> []
            )

(*
    let transformImports (imports: Import seq): ModuleDeclaration list =
        let statefulImports = ResizeArray()
        imports |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier.identifier(localId)
                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId)
                    | "default" | "" -> ImportDefaultSpecifier(localId)
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

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then None
        else
            match selector with
            | "" | "*" | "default" -> Fable.Path.GetFileNameWithoutExtension(path)
            | _ -> selector
            |> getUniqueNameInRootScope ctx
            |> Some
*)

module Compiler =
    open Util

    type RustCompiler (com: Fable.Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        // let imports = Dictionary<string, Import>()
        let imports = HashSet<string>()

        interface IRustCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, selector, path, r) =

                let mnamespace =
                    //Todo - somehow we need to get out the NAMESPACE of the requested input, which seems not to be available in the Type, ImportKind, or SourceLocation.
                    //the below is a fudge that will only work when the module is the same as the file name, with no namespace prefix
                    path.Replace(".js", "").Split('/') |> Seq.rev |> Seq.tryHead
                    |> Option.map(fun n -> n + "|")
                    |> Option.defaultValue ""
                let import = path + "|" + mnamespace + selector
                imports.Add(import) |> ignore
                mkFullNamePathExpr selector None
        //         let cachedName = path + "::" + selector
        //         match imports.TryGetValue(cachedName) with
        //         | true, i ->
        //             match i.LocalIdent with
        //             | Some localIdent -> Expression.identifier(localIdent)
        //             | None -> Expression.nullLiteral()
        //         | false, _ ->
        //             let localId = getIdentForImport ctx path selector
        //             let i =
        //               { Selector =
        //                     if selector = Naming.placeholder then
        //                              "`importMember` must be assigned to a variable"
        //                              |> addError com [] r; selector
        //                     else selector
        //                 Path = path
        //                 LocalIdent = localId }
        //             imports.Add(cachedName, i)
        //             match localId with
        //             | Some localId -> Expression.identifier(localId)
        //             | None -> Expression.nullLiteral()
            member _.GetAllImports() = imports :> seq<_>
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
        //     member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
        //     member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
        //     member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None selector path

        interface Fable.Compiler with
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

    let makeCompiler com = RustCompiler(com)

    let transformFile (com: Fable.Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IRustCompiler
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
            ScopedTypeParams = Set.empty
            ScopedSymbols = Map.empty
            Typegen = { FavourClosureTraitOverFunctionPointer = false
                        IsParamType = false
                        TakingOwnership = false } }

        let topAttrs = [
            mkInnerAttr "allow" ["unused_imports"] // TODO: remove later?
            mkInnerAttr "allow" ["non_snake_case"]
            mkInnerAttr "allow" ["non_camel_case_types"]
            mkInnerAttr "feature" ["stmt_expr_attributes"]
            mkInnerAttr "feature" ["destructuring_assignment"]
        ]
        let useDecls = preludeDecls
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let importDecls = com.GetAllImports() |> transformImports ctx
        let entryPointDecls = getEntryPointDecls file.Declarations
        let items = importDecls @ useDecls @ rootDecls @ entryPointDecls

        let crate = mkCrate topAttrs items
        crate
