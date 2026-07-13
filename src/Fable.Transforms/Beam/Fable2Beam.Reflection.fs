module Fable.Transforms.Beam.Reflection

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms

/// Create a string literal Erlang expression
let private strLit s =
    Beam.ErlExpr.Literal(Beam.ErlLiteral.StringLit s)

/// Create an atom literal Erlang expression
let private atomLit s =
    Beam.ErlExpr.Literal(Beam.ErlLiteral.AtomLit(Beam.Atom s))

/// Create an integer literal Erlang expression
let private intLit i =
    Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer(int64 i))

/// Build a type info map: #{fullname => <<"...">>, generics => [...]}
let private makeTypeInfoMap (fullname: string) (generics: Beam.ErlExpr list) =
    Beam.ErlExpr.Map
        [
            atomLit "fullname", strLit fullname
            atomLit "generics", Beam.ErlExpr.List generics
        ]

/// Wrap a list of field/case infos in a zero-arity fun: `fun() -> [...] end`.
///
/// Record fields and union cases are emitted lazily so a recursive type (whose fields
/// mention the type itself) does not build an infinite map. `fable_reflection` forces
/// the thunk when the fields/cases are actually needed.
let private makeThunk (elements: Beam.ErlExpr list) =
    Beam.ErlExpr.Fun
        [
            {
                Patterns = []
                Guard = []
                Body = [ Beam.ErlExpr.List elements ]
            }
        ]

/// Name of the per-entity reflection function generated for a record/union
/// (e.g. entity `Tree` -> `tree_reflection/0`).
let reflectionFuncName (declarationName: string) =
    Fable.Beam.Naming.sanitizeErlangName declarationName + "_reflection"

/// Erlang variable names bound to an entity's resolved generic arguments inside its
/// reflection function (`gen0`, `gen1`, ...).
let reflectionGenArgVar (index: int) = $"Gen%d{index}"

/// Build a PropertyInfo map: #{name => <<"field_name">>, property_type => TypeInfo}
let private makePropertyInfo (fieldName: string) (typeInfo: Beam.ErlExpr) =
    let erlName = Fable.Beam.Naming.sanitizeErlangName fieldName
    Beam.ErlExpr.Map [ atomLit "name", strLit erlName; atomLit "property_type", typeInfo ]

/// Build a CaseInfo map: #{tag => N, name => <<"CaseName">>, erl_tag => case_name, fields => [...]}
let private makeCaseInfo (tag: int) (caseName: string) (fields: Beam.ErlExpr list) =
    let erlTag = Fable.Beam.Naming.sanitizeErlangName caseName

    Beam.ErlExpr.Map
        [
            atomLit "tag", intLit tag
            atomLit "name", strLit caseName
            atomLit "erl_tag", atomLit erlTag
            atomLit "fields", Beam.ErlExpr.List fields
        ]

/// Get the full name for a number kind
let private getNumberFullName (kind: NumberKind) =
    match kind with
    | Int8 -> Types.int8
    | UInt8 -> Types.uint8
    | Int16 -> Types.int16
    | UInt16 -> Types.uint16
    | Int32 -> Types.int32
    | UInt32 -> Types.uint32
    | Int64 -> Types.int64
    | UInt64 -> Types.uint64
    | Int128 -> Types.int128
    | UInt128 -> Types.uint128
    | NativeInt -> Types.nativeint
    | UNativeInt -> Types.unativeint
    | Float16 -> Types.float16
    | Float32 -> Types.float32
    | Float64 -> Types.float64
    | Decimal -> Types.decimal
    | BigInt -> Types.bigint

/// Transform a Fable Type into an Erlang type info map expression.
///
/// `expanding` holds the entities whose fields/cases are currently being inlined. It only
/// guards the fallback path taken by entities with no source file (BCL/`.dll` types, which
/// have no generated Erlang module to call into); entities we compile get a by-name call to
/// their reflection function instead, which is what breaks recursive types.
let rec private transformTypeInfoRec
    (com: Compiler)
    (r: SourceLocation option)
    (genMap: Map<string, Beam.ErlExpr>)
    (expanding: Set<string>)
    (t: Type)
    : Beam.ErlExpr
    =
    let transformTypeInfo com r genMap t =
        transformTypeInfoRec com r genMap expanding t

    let resolveGenerics (genArgs: Type list) =
        genArgs |> List.map (transformTypeInfo com r genMap)

    match t with
    | Fable.Measure _
    | Fable.Any -> makeTypeInfoMap Types.object []
    | Fable.Unit -> makeTypeInfoMap "Microsoft.FSharp.Core.Unit" []
    | Fable.Boolean -> makeTypeInfoMap Types.bool []
    | Fable.Char -> makeTypeInfoMap Types.char []
    | Fable.String -> makeTypeInfoMap Types.string []
    | Fable.Number(kind, Fable.NumberInfo.IsEnum entRef) ->
        // An enum keeps its underlying numeric kind in the Fable AST, so its type info carries
        // the declared cases alongside the underlying type (as the single generic).
        let ent = com.GetEntity(entRef)

        let enumCases =
            ent.FSharpFields
            |> List.choose (fun fi ->
                match fi.Name with
                | "value__" -> None
                | name ->
                    let value =
                        match fi.LiteralValue with
                        | Some v -> System.Convert.ToInt64 v
                        | None -> 0L

                    Beam.ErlExpr.Tuple [ strLit name; Beam.ErlExpr.Literal(Beam.ErlLiteral.Integer value) ]
                    |> Some
            )

        Beam.ErlExpr.Map
            [
                atomLit "fullname", strLit entRef.FullName
                atomLit "generics", Beam.ErlExpr.List [ makeTypeInfoMap (getNumberFullName kind) [] ]
                atomLit "enum_cases", Beam.ErlExpr.List enumCases
            ]
    | Fable.Number(kind, _) -> makeTypeInfoMap (getNumberFullName kind) []
    | Fable.GenericParam(name = name) ->
        match Map.tryFind name genMap with
        | Some t -> t
        | None -> makeTypeInfoMap ("'" + name) []
    | Fable.LambdaType(argType, returnType) ->
        let genArgs = resolveGenerics [ argType; returnType ]
        makeTypeInfoMap "Microsoft.FSharp.Core.FSharpFunc`2" genArgs
    | Fable.DelegateType(argTypes, returnType) ->
        let genArgs = resolveGenerics (argTypes @ [ returnType ])
        let arity = List.length argTypes + 1
        makeTypeInfoMap $"System.Func`%d{arity}" genArgs
    | Fable.Tuple(genArgs, isStruct) ->
        let resolved = resolveGenerics genArgs
        let n = List.length genArgs

        let prefix =
            if isStruct then
                "System.ValueTuple"
            else
                "System.Tuple"

        makeTypeInfoMap $"%s{prefix}`%d{n}" resolved
    | Fable.Option(genArg, _) ->
        let resolved = resolveGenerics [ genArg ]
        makeTypeInfoMap "Microsoft.FSharp.Core.FSharpOption`1" resolved
    | Fable.Nullable(genArg, true) ->
        let resolved = resolveGenerics [ genArg ]
        makeTypeInfoMap "Microsoft.FSharp.Core.FSharpOption`1" resolved
    | Fable.Nullable(genArg, false) -> transformTypeInfo com r genMap genArg
    | Fable.Array(genArg, _) ->
        let resolved = resolveGenerics [ genArg ]
        makeTypeInfoMap (AST.getTypeFullName false genArg + "[]") resolved
    | Fable.List genArg ->
        let resolved = resolveGenerics [ genArg ]
        makeTypeInfoMap "Microsoft.FSharp.Collections.FSharpList`1" resolved
    | Fable.Regex -> makeTypeInfoMap Types.regex []
    | Fable.MetaType -> makeTypeInfoMap Types.type_ []
    | Fable.AnonymousRecordType _ -> makeTypeInfoMap "" []
    | Fable.DeclaredType(entRef, generics) ->
        let resolved = resolveGenerics generics

        match com.TryGetEntity(entRef) with
        | Some ent when (ent.IsFSharpRecord || ent.IsFSharpUnion) && entRef.SourcePath.IsSome ->
            // Call the entity's generated reflection function instead of inlining its
            // fields/cases. The by-name indirection is what lets a recursive type refer to
            // itself: `tree_reflection()` mentions `tree_reflection()` inside a thunk.
            let sourcePath = entRef.SourcePath.Value

            let moduleName =
                if sourcePath = com.CurrentFile then
                    None // local call
                else
                    Some(Fable.Beam.Naming.moduleNameFromFile sourcePath)

            let funcName =
                FSharp2Fable.Helpers.getEntityDeclarationName com entRef |> reflectionFuncName

            Beam.ErlExpr.Call(moduleName, funcName, resolved)
        | Some ent when (ent.IsFSharpRecord || ent.IsFSharpUnion) && expanding.Contains entRef.FullName ->
            // Re-entering an entity we are already inlining (no source file to call into):
            // emit the bare type info to stop the recursion.
            makeTypeInfoMap entRef.FullName resolved
        | Some ent when ent.IsFSharpRecord ->
            let expanding = expanding.Add entRef.FullName

            let fields =
                ent.FSharpFields
                |> List.map (fun fi ->
                    let typeInfo = transformTypeInfoRec com r genMap expanding fi.FieldType
                    makePropertyInfo fi.Name typeInfo
                )

            Beam.ErlExpr.Map
                [
                    atomLit "fullname", strLit entRef.FullName
                    atomLit "generics", Beam.ErlExpr.List resolved
                    atomLit "fields", makeThunk fields
                ]
        | Some ent when ent.IsFSharpUnion ->
            let expanding = expanding.Add entRef.FullName

            let cases =
                ent.UnionCases
                |> List.mapi (fun i uci ->
                    let caseFields =
                        uci.UnionCaseFields
                        |> List.map (fun fi ->
                            let typeInfo = transformTypeInfoRec com r genMap expanding fi.FieldType
                            makePropertyInfo fi.Name typeInfo
                        )

                    makeCaseInfo i uci.Name caseFields
                )

            Beam.ErlExpr.Map
                [
                    atomLit "fullname", strLit entRef.FullName
                    atomLit "generics", Beam.ErlExpr.List resolved
                    atomLit "cases", makeThunk cases
                ]
        | _ -> makeTypeInfoMap entRef.FullName resolved

/// Transform a Fable Type into an Erlang type info map expression
let transformTypeInfo
    (com: Compiler)
    (r: SourceLocation option)
    (genMap: Map<string, Beam.ErlExpr>)
    (t: Type)
    : Beam.ErlExpr
    =
    transformTypeInfoRec com r genMap Set.empty t

/// Build the body of an entity's reflection function: the type info map for a record/union,
/// with `fields`/`cases` emitted lazily and the entity's generic parameters bound to the
/// function's arguments (`Gen0`, `Gen1`, ...).
let transformEntityReflectionBody (com: Compiler) (ent: Entity) : Beam.ErlExpr =
    let genMap =
        ent.GenericParameters
        |> List.mapi (fun i gp -> gp.Name, Beam.ErlExpr.Variable(reflectionGenArgVar i))
        |> Map.ofList

    let generics =
        ent.GenericParameters
        |> List.mapi (fun i _ -> Beam.ErlExpr.Variable(reflectionGenArgVar i))

    let expanding = Set.singleton ent.FullName

    if ent.IsFSharpUnion then
        let cases =
            ent.UnionCases
            |> List.mapi (fun i uci ->
                let caseFields =
                    uci.UnionCaseFields
                    |> List.map (fun fi ->
                        let typeInfo = transformTypeInfoRec com None genMap expanding fi.FieldType
                        makePropertyInfo fi.Name typeInfo
                    )

                makeCaseInfo i uci.Name caseFields
            )

        Beam.ErlExpr.Map
            [
                atomLit "fullname", strLit ent.FullName
                atomLit "generics", Beam.ErlExpr.List generics
                atomLit "cases", makeThunk cases
            ]
    else
        let fields =
            ent.FSharpFields
            |> List.map (fun fi ->
                let typeInfo = transformTypeInfoRec com None genMap expanding fi.FieldType
                makePropertyInfo fi.Name typeInfo
            )

        Beam.ErlExpr.Map
            [
                atomLit "fullname", strLit ent.FullName
                atomLit "generics", Beam.ErlExpr.List generics
                atomLit "fields", makeThunk fields
            ]
