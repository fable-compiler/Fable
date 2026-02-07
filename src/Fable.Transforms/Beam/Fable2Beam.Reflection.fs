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

/// Build a type info map: #{fullname => <<"...">>, generics => [...]}
let private makeTypeInfoMap (fullname: string) (generics: Beam.ErlExpr list) =
    Beam.ErlExpr.Map
        [
            atomLit "fullname", strLit fullname
            atomLit "generics", Beam.ErlExpr.List generics
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

/// Transform a Fable Type into an Erlang type info map expression
let rec transformTypeInfo
    (com: Compiler)
    (r: SourceLocation option)
    (genMap: Map<string, Beam.ErlExpr>)
    (t: Type)
    : Beam.ErlExpr
    =
    let resolveGenerics (genArgs: Type list) =
        genArgs |> List.map (transformTypeInfo com r genMap)

    match t with
    | Fable.Measure _
    | Fable.Any -> makeTypeInfoMap Types.object []
    | Fable.Unit -> makeTypeInfoMap "Microsoft.FSharp.Core.Unit" []
    | Fable.Boolean -> makeTypeInfoMap Types.bool []
    | Fable.Char -> makeTypeInfoMap Types.char []
    | Fable.String -> makeTypeInfoMap Types.string []
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
        makeTypeInfoMap $"System.Func`{arity}" genArgs
    | Fable.Tuple(genArgs, isStruct) ->
        let resolved = resolveGenerics genArgs
        let n = List.length genArgs

        let prefix =
            if isStruct then
                "System.ValueTuple"
            else
                "System.Tuple"

        makeTypeInfoMap $"{prefix}`{n}" resolved
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
        makeTypeInfoMap entRef.FullName resolved
