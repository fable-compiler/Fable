module Fable.Transforms.Replacements.Util

#nowarn "1182"

open System
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

type Helper =
    static member ConstructorCall
        (
            consExpr: Expr,
            returnType: Type,
            args: Expr list,
            ?argTypes,
            ?genArgs,
            ?loc: SourceLocation
        )
        =
        let info =
            CallInfo.Create(
                args = args,
                ?sigArgTypes = argTypes,
                ?genArgs = genArgs,
                isCons = true
            )

        Call(consExpr, info, returnType, loc)

    static member InstanceCall
        (
            callee: Expr,
            memb: string,
            returnType: Type,
            args: Expr list,
            ?argTypes: Type list,
            ?genArgs,
            ?loc: SourceLocation
        )
        =
        let callee = getField callee memb

        let info =
            CallInfo.Create(
                args = args,
                ?sigArgTypes = argTypes,
                ?genArgs = genArgs
            )

        Call(callee, info, returnType, loc)

    static member Application
        (
            callee: Expr,
            returnType: Type,
            args: Expr list,
            ?argTypes: Type list,
            ?loc: SourceLocation
        )
        =
        let info = defaultArg argTypes [] |> makeCallInfo None args
        Call(callee, info, returnType, loc)

    static member LibValue
        (
            com,
            coreModule: string,
            coreMember: string,
            returnType: Type
        )
        =
        makeImportLib com returnType coreMember coreModule

    static member LibCall
        (
            com,
            coreModule: string,
            coreMember: string,
            returnType: Type,
            args: Expr list,
            ?argTypes: Type list,
            ?genArgs,
            ?thisArg: Expr,
            ?hasSpread: bool,
            ?isModuleMember,
            ?isConstructor: bool,
            ?loc: SourceLocation
        )
        =
        let isInstanceMember = Option.isSome thisArg
        let isModuleMember = defaultArg isModuleMember (not isInstanceMember)

        let callee =
            LibraryImportInfo.Create(
                isInstanceMember = isInstanceMember,
                isModuleMember = isModuleMember
            )
            |> makeImportLibWithInfo com Any coreMember coreModule

        let memberRef =
            match hasSpread with
            | Some true ->
                let argTypes =
                    argTypes
                    |> Option.defaultWith (fun () ->
                        args |> List.map (fun a -> a.Type)
                    )

                GeneratedMember.Function(
                    coreMember,
                    argTypes,
                    returnType,
                    isInstance = isInstanceMember,
                    hasSpread = true
                )
                |> Some
            | Some false
            | None -> None

        let info =
            CallInfo.Create(
                ?thisArg = thisArg,
                args = args,
                ?sigArgTypes = argTypes,
                ?genArgs = genArgs,
                ?memberRef = memberRef,
                ?isCons = isConstructor
            )

        Call(callee, info, returnType, loc)

    static member ImportedValue
        (
            com,
            coreModule: string,
            coreMember: string,
            returnType: Type
        )
        =
        makeImportUserGenerated None Any coreMember coreModule

    static member ImportedCall
        (
            path: string,
            selector: string,
            returnType: Type,
            args: Expr list,
            ?argTypes: Type list,
            ?genArgs,
            ?thisArg: Expr,
            ?hasSpread: bool,
            ?isConstructor: bool,
            ?loc: SourceLocation
        )
        =
        let callee = makeImportUserGenerated None Any selector path

        let memberRef =
            match hasSpread with
            | Some true ->
                let argTypes =
                    argTypes
                    |> Option.defaultWith (fun () ->
                        args |> List.map (fun a -> a.Type)
                    )

                GeneratedMember.Function(
                    selector,
                    argTypes,
                    returnType,
                    isInstance = false,
                    hasSpread = true
                )
                |> Some
            | Some false
            | None -> None

        let info =
            CallInfo.Create(
                ?thisArg = thisArg,
                args = args,
                ?sigArgTypes = argTypes,
                ?genArgs = genArgs,
                ?memberRef = memberRef,
                ?isCons = isConstructor
            )

        Call(callee, info, returnType, loc)

    static member GlobalCall
        (
            ident: string,
            returnType: Type,
            args: Expr list,
            ?argTypes: Type list,
            ?genArgs,
            ?memb: string,
            ?isConstructor: bool,
            ?loc: SourceLocation
        )
        =
        let callee =
            match memb with
            | Some memb -> getField (makeIdentExpr ident) memb
            | None -> makeIdentExpr ident

        let info =
            CallInfo.Create(
                args = args,
                ?sigArgTypes = argTypes,
                ?genArgs = genArgs,
                ?isCons = isConstructor
            )

        Call(callee, info, returnType, loc)

    static member GlobalIdent
        (
            ident: string,
            memb: string,
            typ: Type,
            ?loc: SourceLocation
        )
        =
        getFieldWith loc typ (makeIdentExpr ident) memb

type NumberKind with

    member this.Number = Number(this, NumberInfo.Empty)

let makeUniqueIdent ctx t name =
    FSharp2Fable.Helpers.getIdentUniqueName ctx name |> makeTypedIdent t

let withTag tag =
    function
    | Call(e, i, t, r) -> Call(e, { i with Tags = tag :: i.Tags }, t, r)
    | Get(e, FieldGet i, t, r) ->
        Get(e, FieldGet { i with Tags = tag :: i.Tags }, t, r)
    | e -> e

let getTags =
    function
    | Call(e, i, t, r) -> i.Tags
    | Get(e, FieldGet i, t, r) -> i.Tags
    | _e -> []

let objValue (k, v) : ObjectExprMember =
    {
        Name = k
        Args = []
        Body = v
        IsMangled = false
        MemberRef = GeneratedMember.Value(k, v.Type)
    }

let typedObjExpr t kvs =
    ObjectExpr(List.map objValue kvs, t, None)

let objExpr kvs = typedObjExpr Any kvs

let add left right =
    Operation(Binary(BinaryPlus, left, right), Tags.empty, left.Type, None)

let sub left right =
    Operation(Binary(BinaryMinus, left, right), Tags.empty, left.Type, None)

let eq left right =
    Operation(Binary(BinaryEqual, left, right), Tags.empty, Boolean, None)

let neq left right =
    Operation(Binary(BinaryUnequal, left, right), Tags.empty, Boolean, None)

let nullCheck r isNull expr =
    let op =
        if isNull then
            BinaryEqual
        else
            BinaryUnequal

    Operation(
        Binary(op, expr, Value(Null expr.Type, None)),
        Tags.empty,
        Boolean,
        r
    )

let str txt = Value(StringConstant txt, None)

let genArg (com: ICompiler) (ctx: Context) r i (genArgs: Type list) =
    List.tryItem i genArgs
    |> Option.defaultWith (fun () ->
        "Couldn't find generic argument in position " + (string<int> i)
        |> addError com ctx.InlinePath r

        Any
    )

let toArray r t expr =
    let t, kind =
        match t with
        | Array(t, kind) -> t, kind
        // This is used also by Seq.cache, which returns `'T seq` instead of `'T array`
        | DeclaredType(_, [ t ])
        | t -> t, MutableArray

    Value(NewArray(ArrayFrom expr, t, kind), r)

let getBoxedZero kind : obj =
    match kind with
    | Int8 -> 0y: int8
    | UInt8 -> 0uy: uint8
    | Int16 -> 0s: int16
    | UInt16 -> 0us: uint16
    | Int32 -> 0: int32
    | UInt32 -> 0u: uint32
    | Int64 -> 0L: int64
    | UInt64 -> 0UL: uint64
    | Int128 -> 0L: int64 //System.Int128.Zero
    | UInt128 -> 0UL: uint64 //System.UInt128.Zero
    | BigInt -> 0I: bigint
    | NativeInt -> 0n: nativeint
    | UNativeInt -> 0un: unativeint
    | Float16 -> 0.f: float32 //System.Half.Zero
    | Float32 -> 0.f: float32
    | Float64 -> 0.: float
    | Decimal -> 0M: decimal

let getBoxedOne kind : obj =
    match kind with
    | Int8 -> 1y: int8
    | UInt8 -> 1uy: uint8
    | Int16 -> 1s: int16
    | UInt16 -> 1us: uint16
    | Int32 -> 1: int32
    | UInt32 -> 1u: uint32
    | Int64 -> 1L: int64
    | UInt64 -> 1UL: uint64
    | Int128 -> 1L: int64 //System.Int128.One
    | UInt128 -> 1UL: uint64 //System.UInt128.One
    | BigInt -> 1I: bigint
    | NativeInt -> 1n: nativeint
    | UNativeInt -> 1un: unativeint
    | Float16 -> 1.f: float32 //System.Half.One
    | Float32 -> 1.f: float32
    | Float64 -> 1.: float
    | Decimal -> 1M: decimal

type BuiltinType =
    | BclGuid
    | BclTimeSpan
    | BclDateTime
    | BclDateTimeOffset
    | BclDateOnly
    | BclTimeOnly
    | BclTimer
    | BclHashSet of Type
    | BclDictionary of key: Type * value: Type
    | BclKeyValuePair of key: Type * value: Type
    | FSharpSet of Type
    | FSharpMap of key: Type * value: Type
    | FSharpChoice of Type list
    | FSharpResult of Type * Type
    | FSharpReference of Type

let (|BuiltinDefinition|_|) =
    function
    | Types.guid -> Some BclGuid
    | Types.timespan -> Some BclTimeSpan
    | Types.datetime -> Some BclDateTime
    | Types.datetimeOffset -> Some BclDateTimeOffset
    | Types.dateOnly -> Some BclDateOnly
    | Types.timeOnly -> Some BclTimeOnly
    | "System.Timers.Timer" -> Some BclTimer
    | Types.decimal
    | Types.fsharpSet -> Some(FSharpSet(Any))
    | Types.fsharpMap -> Some(FSharpMap(Any, Any))
    | Types.hashset -> Some(BclHashSet(Any))
    | Types.dictionary -> Some(BclDictionary(Any, Any))
    | Types.keyValuePair -> Some(BclKeyValuePair(Any, Any))
    | Types.result -> Some(FSharpResult(Any, Any))
    | Types.byref -> Some(FSharpReference(Any))
    | Types.byref2 -> Some(FSharpReference(Any))
    | Types.refCell -> Some(FSharpReference(Any))
    | Naming.StartsWith Types.choiceNonGeneric genArgs ->
        List.replicate (int genArgs[1..]) Any |> FSharpChoice |> Some
    | _ -> None

let (|BuiltinEntity|_|) (ent: string, genArgs) =
    match ent, genArgs with
    | BuiltinDefinition(FSharpSet _), [ t ] -> Some(FSharpSet(t))
    | BuiltinDefinition(FSharpMap _), [ k; v ] -> Some(FSharpMap(k, v))
    | BuiltinDefinition(BclHashSet _), [ t ] -> Some(BclHashSet(t))
    | BuiltinDefinition(BclDictionary _), [ k; v ] -> Some(BclDictionary(k, v))
    | BuiltinDefinition(BclKeyValuePair _), [ k; v ] ->
        Some(BclKeyValuePair(k, v))
    | BuiltinDefinition(FSharpResult _), [ k; v ] -> Some(FSharpResult(k, v))
    | BuiltinDefinition(FSharpReference _), [ t ] -> Some(FSharpReference(t))
    | BuiltinDefinition(FSharpReference _), [ t; _ ] -> Some(FSharpReference(t))
    | BuiltinDefinition(FSharpChoice _), genArgs -> Some(FSharpChoice genArgs)
    | BuiltinDefinition t, _ -> Some t
    | _ -> None

let (|Builtin|_|) =
    function
    | DeclaredType(ent, genArgs) ->
        match ent.FullName, genArgs with
        | BuiltinEntity x -> Some x
        | _ -> None
    | _ -> None

let getElementType =
    function
    | Array(t, _) -> t
    | List t -> t
    | DeclaredType(_, [ t ]) -> t
    | _ -> Any

let genericTypeInfoError (name: string) =
    $"Cannot get type info of generic parameter {name}. Fable erases generics at runtime, try inlining the functions so generics can be resolved at compile time."

// This is mainly intended for typeof errors because we want to show the user where the function is originally called
let changeRangeToCallSite
    (inlinePath: InlinePath list)
    (range: SourceLocation option)
    =
    List.tryLast inlinePath
    |> Option.bind (fun i -> i.FromRange)
    |> Option.orElse range

let splitFullName (fullname: string) =
    let fullname =
        match fullname.IndexOf("[", StringComparison.Ordinal) with
        | -1 -> fullname
        | i -> fullname[.. i - 1]

    match fullname.LastIndexOf(".") with
    | -1 -> "", fullname
    | i -> fullname.Substring(0, i), fullname.Substring(i + 1)

let getTypeNameFromFullName (fullname: string) =
    let fullname =
        match fullname.IndexOf("[", StringComparison.Ordinal) with
        | -1 -> fullname
        | i -> fullname[.. i - 1]

    match fullname.LastIndexOf(".") with
    | -1 -> fullname
    | i -> fullname.Substring(i + 1)

let rec getTypeName com (ctx: Context) r t =
    match t with
    | GenericParam(name = name) ->
        genericTypeInfoError name |> addError com ctx.InlinePath r
        name
    | Array(elemType, _) -> // TODO: check kind
        getTypeName com ctx r elemType + "[]"
    | _ -> getTypeFullName false t |> splitFullName |> snd

let makeDeclaredType assemblyName genArgs fullName =
    let entRef: EntityRef =
        {
            FullName = fullName
            Path = CoreAssemblyName assemblyName
        }

    DeclaredType(entRef, genArgs)

let makeRuntimeType genArgs fullName =
    makeDeclaredType "System.Runtime" genArgs fullName

let makeFSharpCoreType genArgs fullName =
    makeDeclaredType "FSharp.Core" genArgs fullName

let makeStringTemplate
    tag
    (str: string)
    (holes:
        {|
            Index: int
            Length: int
        |}[])
    values
    =
    let mutable prevIndex = 0

    let parts =
        [
            for i = 0 to holes.Length - 1 do
                let m = holes[i]
                let strPart = str.Substring(prevIndex, m.Index - prevIndex)
                prevIndex <- m.Index + m.Length
                strPart
            str.Substring(prevIndex)
        ]

    StringTemplate(tag, parts, values)

let makeStringTemplateFrom simpleFormats values =
    function
    | StringConst str ->
        // In the case of interpolated strings, the F# compiler doesn't resolve escaped %
        // (though it does resolve double braces {{ }})
        let str = str.Replace("%%", "%")

        (Some [],
         Regex.Matches(str, @"((?<!%)%(?:[0+\- ]*)(?:\d+)?(?:\.\d+)?\w)?%P\(\)")
         |> Seq.cast<Match>)
        ||> Seq.fold (fun acc m ->
            match acc with
            | None -> None
            | Some acc ->
                // TODO: If arguments need format, format them individually
                let doesNotNeedFormat =
                    not m.Groups[1].Success
                    || (Array.contains m.Groups[1].Value simpleFormats)

                if doesNotNeedFormat then
                    {|
                        Index = m.Index
                        Length = m.Length
                    |}
                    :: acc
                    |> Some
                else
                    None
        )
        |> Option.map (fun holes ->
            let holes = List.toArray holes |> Array.rev
            makeStringTemplate None str holes values
        )
    | _ -> None

let rec namesof com ctx acc e =
    match acc, e with
    | acc, Get(e, ExprGet(StringConst prop), _, _) ->
        namesof com ctx (prop :: acc) e
    | acc, Get(e, FieldGet i, _, _) -> namesof com ctx (i.Name :: acc) e
    | [], IdentExpr ident -> ident.DisplayName :: acc |> Some
    | [], NestedLambda(args, Call(IdentExpr ident, info, _, _), c) ->
        if
            List.sameLength args info.Args
            && List.zip args info.Args
               |> List.forall (fun (a1, a2) ->
                   match a2 with
                   | IdentExpr id2 -> a1.Name = id2.Name
                   | _ -> false
               )
        then
            ident.DisplayName :: acc |> Some
        else
            None
    | [], Value(TypeInfo(t, _), r) -> (getTypeName com ctx r t) :: acc |> Some
    | [], _ -> None
    | acc, _ -> Some acc

let curriedApply r t applied args = CurriedApply(applied, args, t, r)

let compose (com: ICompiler) ctx r t (f1: Expr) (f2: Expr) =
    let argType, retType =
        match t with
        | LambdaType(argType, retType) -> argType, retType
        | _ -> Any, Any

    let interType =
        match f1.Type with
        | LambdaType(_, interType) -> interType
        | _ -> Any

    let arg = makeUniqueIdent ctx argType "arg"
    // Eagerly evaluate and capture the value of the functions, see #2851
    // If possible, the bindings will be optimized away in FableTransforms
    let capturedFun1Var = makeUniqueIdent ctx argType "f1"
    let capturedFun2Var = makeUniqueIdent ctx argType "f2"

    let argExpr =
        match argType with
        // Erase unit references, because the arg may be erased
        | Unit -> Value(UnitConstant, None)
        | _ -> IdentExpr arg

    let body =
        [ argExpr ]
        |> curriedApply None interType (IdentExpr capturedFun1Var)
        |> List.singleton
        |> curriedApply r retType (IdentExpr capturedFun2Var)

    Let(capturedFun1Var, f1, Let(capturedFun2Var, f2, Lambda(arg, body, None)))

let partialApplyAtRuntime
    (com: Compiler)
    t
    arity
    (expr: Expr)
    (partialArgs: Expr list)
    =
    match com.Options.Language with
    | JavaScript
    | TypeScript
    | Dart
    | Python ->
        match uncurryLambdaType -1 [] expr.Type with
        | ([] | [ _ ]), _ -> expr
        | argTypes, returnType ->
            let curriedType = makeLambdaType argTypes returnType

            let curried =
                Helper.LibCall(
                    com,
                    "Util",
                    $"curry{argTypes.Length}",
                    curriedType,
                    [ expr ]
                )

            match partialArgs with
            | [] -> curried
            | partialArgs -> curriedApply None t curried partialArgs
    | _ ->
        // Check if argTypes.Length < arity?
        let makeArgIdent i typ = makeTypedIdent typ $"a{i}" // $"a{com.IncrementCounter()}$"
        let argTypes, returnType = uncurryLambdaType arity [] t
        let argIdents = argTypes |> List.mapi makeArgIdent
        let args = argIdents |> List.map Fable.IdentExpr
        let body = Helper.Application(expr, returnType, partialArgs @ args)
        makeLambda argIdents body

let curryExprAtRuntime (com: Compiler) arity (expr: Expr) =
    if arity = 1 then
        expr
    else
        match expr with
        | Value(Null _, _) -> expr
        | Value(NewOption(value, t, isStruct), r) ->
            match value with
            | None -> expr
            | Some v ->
                let curried = partialApplyAtRuntime com t arity v []
                Value(NewOption(Some curried, t, isStruct), r)
        | ExprType(Option(t, isStruct)) ->
            let uncurriedType =
                let argTypes, returnType = uncurryLambdaType arity [] t
                DelegateType(argTypes, returnType)

            let f = makeTypedIdent uncurriedType "f"
            let fe = makeTypedIdent t "f" |> IdentExpr
            let curried = partialApplyAtRuntime com t arity fe []
            let fn = Delegate([ f ], curried, None, Tags.empty)
            // TODO: This may be different per language
            Helper.LibCall(
                com,
                "Option",
                "map",
                Option(curried.Type, isStruct),
                [
                    fn
                    expr
                ]
            )
        | _ -> partialApplyAtRuntime com expr.Type arity expr []

let uncurryExprAtRuntime (com: Compiler) arity (expr: Expr) =
    let uncurry (expr: Expr) =
        // Check if argTypes.Length < arity?
        let argTypes, returnType = uncurryLambdaType arity [] expr.Type

        match com.Options.Language with
        | JavaScript
        | TypeScript
        | Dart
        | Python ->
            let uncurriedType = DelegateType(argTypes, returnType)

            Helper.LibCall(
                com,
                "Util",
                $"uncurry{arity}",
                uncurriedType,
                [ expr ]
            )
        | _ ->
            // let makeArgIdent typ = makeTypedIdent typ $"a{com.IncrementCounter()}$"
            // let argIdents = argTypes |> List.map makeArgIdent
            // let expr, argIdents2 =
            //     match expr with
            //     | Extended(Curry(expr, arity2),_) when arity2 >= arity ->
            //         if arity2 = arity
            //         then expr, []
            //         else
            //             let argTypes2, _returnType = uncurryLambdaType arity2 [] expr.Type
            //             expr, argTypes2 |> List.skip arity |> List.map makeArgIdent
            //     | _ -> expr, []
            // let args = (argIdents1 @ argIdents2) |> List.map IdentExpr
            // let body = curriedApply None returnType expr args
            // let body = makeLambda argIdents2 body
            // Delegate(argIdents1, body, None, Tags.empty)
            let argTypes, returnType =
                match expr.Type with
                | Fable.LambdaType(argType, returnType) ->
                    uncurryLambdaType arity [] expr.Type
                | Fable.DelegateType(argTypes, returnType) ->
                    argTypes, returnType
                | _ -> [], expr.Type

            let makeArgIdent i typ = makeTypedIdent typ $"b{i}" // $"a{com.IncrementCounter()}$"
            let argIdents = argTypes |> List.mapi makeArgIdent
            let args = argIdents |> List.map Fable.IdentExpr
            let body = curriedApply None returnType expr args
            Fable.Delegate(argIdents, body, None, Fable.Tags.empty)

    match expr with
    | Value(Null _, _) -> expr
    | Value(NewOption(value, t, isStruct), r) ->
        let t = Fable.DelegateType(uncurryLambdaType arity [] t)

        match value with
        | None -> Value(NewOption(None, t, isStruct), r)
        | Some v -> Value(NewOption(Some(uncurry v), t, isStruct), r)
    | ExprType(Option(t, isStruct)) ->
        let f = makeTypedIdent t "f"
        let uncurried = uncurry (IdentExpr f)
        let fn = Delegate([ f ], uncurried, None, Tags.empty)
        // TODO: This may be different per language
        Helper.LibCall(
            com,
            "Option",
            "map",
            Option(uncurried.Type, isStruct),
            [
                fn
                expr
            ]
        )
    | expr -> uncurry expr

let (|Namesof|_|) com ctx e = namesof com ctx [] e

let (|Nameof|_|) com ctx e =
    namesof com ctx [] e |> Option.bind List.tryLast

let (|ReplaceName|_|) (namesAndReplacements: (string * string) list) name =
    namesAndReplacements
    |> List.tryPick (fun (name2, replacement) ->
        if name2 = name then
            Some replacement
        else
            None
    )

let (|OrDefault|) (def: 'T) =
    function
    | Some v -> v
    | None -> def

let (|IsByRefType|_|) (com: Compiler) =
    function
    | DeclaredType(entRef, genArgs) ->
        let ent = com.GetEntity(entRef)

        match ent.IsByRef, genArgs with
        | true, (genArg :: _) -> Some genArg
        | _ -> None
    | _ -> None

let (|IsInRefType|_|) (com: Compiler) =
    function
    | DeclaredType(entRef, genArgs) ->
        let ent = com.GetEntity(entRef)

        match ent.IsByRef, genArgs with
        | true, [ genArg; DeclaredType(byRefKind, _) ] when
            byRefKind.FullName = Types.byrefKindIn
            ->
            Some genArg
        | _ -> None
    | _ -> None

let (|HasReferenceEquality|_|) (com: Compiler) =
    function
    | Any
    | LambdaType _
    | DelegateType _ -> Some true
    | DeclaredType(entRef, _) ->
        let ent = com.GetEntity(entRef)

        if ent |> FSharp2Fable.Util.hasStructuralEquality then
            None
        else
            Some true
    | _ -> None

let (|ListLiteral|_|) expr =
    let rec untail t acc =
        function
        | Value(NewList(None, _), _) -> Some(List.rev acc, t)
        | Value(NewList(Some(head, tail), _), _) -> untail t (head :: acc) tail
        | _ -> None

    match expr with
    | NewList(None, t) -> Some([], t)
    | NewList(Some(head, tail), t) -> untail t [ head ] tail
    | _ -> None

let (|ArrayOrListLiteral|_|) =
    function
    | MaybeCasted(Value((NewArray(ArrayValues vals, t, _) | ListLiteral(vals, t)),
                        _)) -> Some(vals, t)
    | _ -> None

let (|IsEntity|_|) fullName =
    function
    | DeclaredType(entRef, genArgs) ->
        if entRef.FullName = fullName then
            Some(entRef, genArgs)
        else
            None
    | _ -> None

let (|IDictionary|IEqualityComparer|Other|) =
    function
    | DeclaredType(entRef, _) ->
        match entRef.FullName with
        | Types.idictionary -> IDictionary
        | Types.iequalityComparerGeneric -> IEqualityComparer
        | _ -> Other
    | _ -> Other

let (|IEnumerable|IEqualityComparer|Other|) =
    function
    | DeclaredType(entRef, _) ->
        match entRef.FullName with
        | Types.ienumerableGeneric -> IEnumerable
        | Types.iequalityComparerGeneric -> IEqualityComparer
        | _ -> Other
    | _ -> Other

let (|Enumerator|Other|) =
    function
    | "System.CharEnumerator"
    | "System.Collections.Generic.List`1.Enumerator"
    | "System.Collections.Generic.HashSet`1.Enumerator"
    | "System.Collections.Generic.Dictionary`2.Enumerator"
    | "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator"
    | "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator" ->
        Enumerator
    | _ -> Other

let (|IsEnumerator|_|) =
    function
    | DeclaredType(entRef, genArgs) ->
        match entRef.FullName with
        | Enumerator -> Some(entRef, genArgs)
        | _ -> None
    | _ -> None

let (|IsNewAnonymousRecord|_|) =
    function
    // The F# compiler may create some bindings of expression arguments to fix https://github.com/dotnet/fsharp/issues/6487
    | NestedRevLets(bindings,
                    Value(NewAnonymousRecord(exprs,
                                             fieldNames,
                                             genArgs,
                                             isStruct),
                          r)) ->
        Some(List.rev bindings, exprs, fieldNames, genArgs, isStruct, r)
    | Value(NewAnonymousRecord(exprs, fieldNames, genArgs, isStruct), r) ->
        Some([], exprs, fieldNames, genArgs, isStruct, r)
    | _ -> None

let (|ListSingleton|) x = [ x ]

let tryFindInScope (ctx: Context) identName =
    let rec findInScopeInner scope identName =
        match scope with
        | [] -> None
        | (_, ident2: Ident, expr) :: prevScope ->
            if identName = ident2.Name then
                match expr with
                | Some(MaybeCasted(IdentExpr ident)) ->
                    findInScopeInner prevScope ident.Name
                | expr -> expr
                |> Option.map (fun e ->
                    if not (isNull ctx.CapturedBindings) then
                        ctx.CapturedBindings.Add(identName) |> ignore

                    e
                )
            else
                findInScopeInner prevScope identName

    findInScopeInner ctx.Scope identName

let (|MaybeInScope|) (ctx: Context) e =
    match e with
    | MaybeCasted(IdentExpr ident) when not ident.IsMutable ->
        match tryFindInScope ctx ident.Name with
        | Some(MaybeCasted e) -> e
        | None -> e
    | e -> e

let rec (|MaybeInScopeStringConst|_|) ctx =
    function
    | MaybeInScope ctx expr ->
        match expr with
        | StringConst s -> Some s
        | Operation(Binary(BinaryPlus,
                           (MaybeInScopeStringConst ctx s1),
                           (MaybeInScopeStringConst ctx s2)),
                    _,
                    _,
                    _) -> Some(s1 + s2)
        | Value(StringTemplate(None, start :: parts, values), _) ->
            (Some [], values)
            ||> List.fold (fun acc value ->
                match acc, value with
                | None, _ -> None
                | Some acc, MaybeInScopeStringConst ctx value ->
                    Some(value :: acc)
                | _ -> None
            )
            |> Option.map (fun values ->
                let valuesAndParts = List.zip (List.rev values) parts

                (start, valuesAndParts)
                ||> List.fold (fun acc (v, p) -> acc + v + p)
            )
        | _ -> None

let rec (|RequireStringConst|) com (ctx: Context) r e =
    match e with
    | MaybeInScopeStringConst ctx s -> s
    | _ ->
        addError com ctx.InlinePath r "Expecting string literal"
        ""

let rec (|RequireStringConstOrTemplate|) com (ctx: Context) r e =
    match e with
    | MaybeInScopeStringConst ctx s -> [ s ], []
    // If any of the interpolated values can have side effects, beta binding reduction won't work
    // so we don't check interpolation in scope
    | Value(StringTemplate(None, parts, values), _) -> parts, values
    | _ ->
        addError com ctx.InlinePath r "Expecting string literal"
        [ "" ], []

let (|CustomOp|_|)
    (com: ICompiler)
    (ctx: Context)
    r
    t
    opName
    (argExprs: Expr list)
    sourceTypes
    =
    let argTypes = argExprs |> List.map (fun a -> a.Type)

    match FSharp2Fable.TypeHelpers.tryFindWitness ctx argTypes false opName with
    | Some w ->
        let callInfo = makeCallInfo None argExprs w.ArgTypes
        makeCall r t callInfo w.Expr |> Some
    | None ->
        sourceTypes
        |> List.tryPick (
            function
            | DeclaredType(ent, _) ->
                let ent = com.GetEntity(ent)

                FSharp2Fable.TypeHelpers.tryFindMember
                    ent
                    ctx.GenericArgs
                    opName
                    false
                    argTypes
            | _ -> None
        )
        |> Option.map (
            FSharp2Fable.Util.makeCallFrom com ctx r t [] None argExprs
        )

let (|RegexFlags|_|) e =
    let rec getFlags =
        function
        | NumberConst(:? int as value, _, _) ->
            match value with
            | 1 -> Some [ RegexIgnoreCase ]
            | 2 -> Some [ RegexMultiline ]
            | 8 -> Some [] // Compiled flag (ignored)
            | 16 -> Some [ RegexSingleline ]
            | 256 -> Some [] // ECMAScript flag (ignored)
            | _ -> None
        | Operation(Binary(BinaryOrBitwise, flags1, flags2), _, _, _) ->
            match getFlags flags1, getFlags flags2 with
            | Some flags1, Some flags2 -> Some(flags1 @ flags2)
            | _ -> None
        | _ -> None

    getFlags e

let (|UniversalFableCoreHelpers|_|)
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    args
    error
    =
    function
    | "op_ErasedCast" -> List.tryHead args
    | ".ctor" -> typedObjExpr t [] |> Some
    | "jsNative"
    | "pyNative"
    | "nativeOnly" ->
        // TODO: Fail at compile time?
        addWarning
            com
            ctx.InlinePath
            r
            $"{i.CompiledName} is being compiled without replacement, this will fail at runtime."

        let runtimeMsg =
            "A function supposed to be replaced by native code has been called, please check."
            |> StringConstant
            |> makeValue None

        makeThrow r t (error runtimeMsg) |> Some

    | "nameof"
    | "nameof2" as meth ->
        match args with
        | [ Nameof com ctx name as arg ] ->
            if meth = "nameof2" then
                makeTuple
                    r
                    true
                    [
                        makeStrConst name
                        arg
                    ]
                |> Some
            else
                makeStrConst name |> Some
        | _ ->
            "Cannot infer name of expression" |> addError com ctx.InlinePath r
            makeStrConst Naming.unknown |> Some

    | "nameofLambda"
    | "namesofLambda" as meth ->
        match args with
        | [ MaybeInScope ctx (Lambda(_, (Namesof com ctx names), _)) ] ->
            Some names
        | _ -> None
        |> Option.defaultWith (fun () ->
            "Cannot infer name of expression" |> addError com ctx.InlinePath r
            [ Naming.unknown ]
        )
        |> fun names ->
            if meth = "namesofLambda" then
                List.map makeStrConst names |> makeArray String |> Some
            else
                List.tryHead names |> Option.map makeStrConst

    | "casenameWithFieldCount"
    | "casenameWithFieldIndex" as meth ->
        let rec inferCasename =
            function
            | Lambda(arg,
                     IfThenElse(Test(IdentExpr arg2, UnionCaseTest tag, _),
                                thenExpr,
                                _,
                                _),
                     _) when arg.Name = arg2.Name ->
                match arg.Type with
                | DeclaredType(e, _) ->
                    let e = com.GetEntity(e)

                    if e.IsFSharpUnion then
                        let c = e.UnionCases[tag]
                        let caseName = defaultArg c.CompiledName c.Name

                        if meth = "casenameWithFieldCount" then
                            Some(caseName, c.UnionCaseFields.Length)
                        else
                            match thenExpr with
                            | NestedRevLets(bindings, IdentExpr i) ->
                                bindings
                                |> List.tryPick (fun (i2, v) ->
                                    match v with
                                    | Get(_, UnionField unionInfo, _, _) when
                                        i.Name = i2.Name
                                        ->
                                        Some unionInfo.FieldIndex
                                    | _ -> None
                                )
                                |> Option.map (fun fieldIdx ->
                                    caseName, fieldIdx
                                )
                            | _ -> None
                    else
                        None
                | _ -> None
            | _ -> None

        match args with
        | [ MaybeInScope ctx e ] -> inferCasename e
        | _ -> None
        |> Option.orElseWith (fun () ->
            "Cannot infer case name of expression"
            |> addError com ctx.InlinePath r

            Some(Naming.unknown, -1)
        )
        |> Option.map (fun (s, i) ->
            makeTuple
                r
                true
                [
                    makeStrConst s
                    makeIntConst i
                ]
        )

    | _ -> None

module AnonRecords =
    open System
    open Fable.Transforms.FSharp2Fable
    open FSharp.Compiler.Symbols

    [<Flags>]
    type private Allow =
        | TheUsual = 0b0000
        /// Enums in F# are uint32
        /// -> Allow into all int & uint
        | EnumIntoInt = 0b0001
        // Erased Unions are reduced to `Any`
        // -> Cannot distinguish between 'normal' Any (like `obj`) and Erased Union (like Erased Union with string field)
        //
        // For interface members the FSharp Type is available
        // -> `Ux<...>` receive special treatment and its types are extracted
        // -> `abstract Value: U2<int,string>` -> extract `int` & `string`
        // BUT: for Expressions in Anon Records that's not possible, and `U2<int,string>` is only recognized as `Any`
        // -> `{| Value = v |}`: `v: int` and `v: string` are recognized as matching,
        //    but `v: U2<int,string>` isn't: only `Any`/`obj` as Type available
        // To recognize as matching, we must allow all `Any` expressions for `U2` in interface place.
        //
        // Note: Only `Ux<...>` are currently handled (on interface side), not other Erased Unions!
        //| AnyIntoErased = 0b0010
        /// Unlike `AnyIntoErased`, this allows all expressions of type `Any` in all interface properties.
        /// (The other way is always allow: Expression of all Types fits into `Any`)
        | AlwaysAny = 0b0100

    let private makeType = TypeHelpers.makeType Map.empty
    let private quote = sprintf "'%s'"
    let private unreachable () = failwith "unreachable"
    let private formatType = getTypeFullName true

    let private formatTypes =
        List.map (formatType >> quote) >> String.concat "; "

    /// Returns for:
    /// * `Ux<...>`: extracted types from `<....>`: `U2<string,int>` -> `[String; Int]`
    /// * `Option<Ux<...>>`: extracted types from `<...>`, then made Optional: `Option<U2<string,int>>` -> `[Option String; Option Int]`
    /// * 'normal' type: `makeType`ed type: `string` -> `[String]`
    ///     Note: Erased Unions (except handled `Ux<...>`) are reduced to `Any`
    ///
    /// Extracting necessary: Erased Unions are reduced to `Any` -> special handling for `Ux<...>`
    ///
    /// Note: nested types aren't handled: `U2<string, U<int, float>>` -> `[Int; Any]`
    let rec private collectTypes (ty: FSharpType) : Fable.Type list =
        // Special treatment for Ux<...> and Option<Ux<...>>: extract types in Ux
        // This is necessary because: `makeType` reduces Erased Unions (including Ux) to `Any` -> no type info any more
        //
        // Note: no handling of nested types: `U2<string, U<int, float>>` -> `int` & `float` don't get extract
        let ty = Helpers.nonAbbreviatedType ty

        match ty with
        | UType tys -> tys |> List.map makeType |> List.distinct
        | OptionType(UType tys, isStruct) ->
            tys
            |> List.map (fun t -> Fable.Option(makeType t, isStruct))
            |> List.distinct
        | _ -> makeType ty |> List.singleton

    and private (|OptionType|_|) (ty: FSharpType) =
        match ty with
        | Patterns.TypeDefinition tdef ->
            match FsEnt.FullName tdef with
            | Types.valueOption ->
                Some(Helpers.nonAbbreviatedType ty.GenericArguments[0], true)
            | Types.option ->
                Some(Helpers.nonAbbreviatedType ty.GenericArguments[0], false)
            | _ -> None
        | _ -> None

    and private (|UType|_|) (ty: FSharpType) =
        let (|UName|_|) (tdef: FSharpEntity) =
            if
                tdef.Namespace = Some "Fable.Core"
                && (let name = tdef.DisplayName
                    name.Length = 2 && name[0] = 'U' && Char.IsDigit name[1])
            then
                Some()
            else
                None

        match ty with
        | Patterns.TypeDefinition UName ->
            ty.GenericArguments
            |> Seq.mapToList Helpers.nonAbbreviatedType
            |> Some
        | _ -> None

    /// Special Rules mostly for Indexers:
    ///     For direct interface member implementation we want to be precise (-> exact_ish match)
    ///     But for indexer allow a bit more types like erased union with string field when indexer is string
    let private fitsInto
        (rules: Allow)
        (expected: Fable.Type list)
        (actual: Fable.Type)
        =
        assert (expected |> List.isEmpty |> not)

        let (|IntNumber|_|) =
            function
            | Fable.Number((Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32), _) ->
                Some()
            | _ -> None

        let fitsIntoSingle
            (rules: Allow)
            (expected: Fable.Type)
            (actual: Fable.Type)
            =
            match expected, actual with
            | Fable.Any, _ -> true
            | _, Fable.Any when rules.HasFlag Allow.AlwaysAny ->
                // Erased Unions are reduced to `Any`
                // -> cannot distinguish between 'normal' Any (like 'obj')
                // and Erased Union (like Erased Union with string field)
                true
            | IntNumber, Fable.Number(_, Fable.NumberInfo.IsEnum _) when
                rules.HasFlag Allow.EnumIntoInt
                ->
                // the underlying type of enum in F# is uint32
                // For practicality: allow in all uint & int fields
                true
            | Fable.Option(t1, _), Fable.Option(t2, _)
            | Fable.Option(t1, _), t2
            | t1, t2 -> typeEquals false t1 t2

        let fitsIntoMulti
            (rules: Allow)
            (expected: Fable.Type list)
            (actual: Fable.Type)
            =
            expected |> List.contains Fable.Any
            || (
            // special treatment for actual=Any & multiple expected:
            // multiple expected -> `Ux<...>` -> extracted types
            // BUT: in actual that's not possible -> in actual `Ux<...>` = `Any`
            //      -> no way to distinguish Ux (or other Erased Unions) from 'normal` Any (like obj)
            //rules.HasFlag Allow.AnyIntoErased
            //&&
            expected |> List.isMultiple && actual = Fable.Any)
            || expected
               |> List.exists (fun expected ->
                   fitsIntoSingle rules expected actual
               )

        fitsIntoMulti rules expected actual

    let private formatMissingFieldError
        (range: SourceLocation option)
        (interface_: FSharpEntity)
        (fieldName: string)
        (expectedTypes: Fable.Type list)
        =
        assert (expectedTypes |> List.isEmpty |> not)

        let interfaceName = interface_.DisplayName

        // adjust error messages based on:
        // * 1, more expectedTypes
        let msg =
            match expectedTypes with
            | [] -> unreachable ()
            | [ expectedType ] ->
                let expectedType = expectedType |> formatType
                $"Object doesn't contain field '{fieldName}' of type '{expectedType}' required by interface '{interfaceName}'"
            | _ ->
                let expectedTypes = expectedTypes |> formatTypes
                $"Object doesn't contain field '{fieldName}' of any type [{expectedTypes}] required by interface '{interfaceName}'"

        (range, fieldName, msg)

    let private formatUnexpectedTypeError
        (range: SourceLocation option)
        (interface_: FSharpEntity)
        (indexers: FSharpMemberOrFunctionOrValue list option)
        (fieldName: string)
        (expectedTypes: Fable.Type list)
        (actualType: Fable.Type)
        (r: SourceLocation option)
        =
        assert (expectedTypes |> List.isEmpty |> not)

        let interfaceName = interface_.DisplayName
        let actualType = actualType |> formatType

        // adjust error messages based on:
        // * 1, more expectedTypes
        // * 0 (None), 1, more indexer
        let msg =
            match indexers with
            | None ->
                match expectedTypes with
                | [] -> unreachable ()
                | [ expectedType ] ->
                    let expectedType = expectedType |> formatType
                    $"Expected type '{expectedType}' for field '{fieldName}' in interface '{interfaceName}', but is '{actualType}'"
                | _ ->
                    let expectedTypes = expectedTypes |> formatTypes
                    $"Expected any type of [{expectedTypes}] for field '{fieldName}' in interface '{interfaceName}', but is '{actualType}'"
            | Some indexers ->
                assert (indexers |> List.isEmpty |> not)

                let indexers =
                    indexers
                    |> List.map (fun i -> i.DisplayName)
                    |> List.distinct

                match indexers with
                | [] -> unreachable ()
                | [ indexerName ] ->
                    match expectedTypes with
                    | [] -> unreachable ()
                    | [ expectedType ] ->
                        let expectedType = expectedType |> formatType
                        $"Expected type '{expectedType}' for field '{fieldName}' because of Indexer '{indexerName}' in interface '{interfaceName}', but is '{actualType}'"
                    | _ ->
                        let expectedTypes = expectedTypes |> formatTypes
                        $"Expected any type of [{expectedTypes}] for field '{fieldName}' because of Indexer '{indexerName}' in interface '{interfaceName}', but is '{actualType}'"
                | _ ->
                    let indexerNames =
                        indexers |> List.map (quote) |> String.concat "; "

                    match expectedTypes with
                    | [] -> unreachable ()
                    | [ expectedType ] ->
                        let expectedType = expectedType |> formatType
                        $"Expected type '{expectedType}' for field '{fieldName}' because of Indexers [{indexerNames}] in interface '{interfaceName}', but is '{actualType}'"
                    | _ ->
                        let expectedTypes = expectedTypes |> formatTypes
                        $"Expected any type of [{expectedTypes}] for field '{fieldName}' because of Indexers [{indexerNames}] in interface '{interfaceName}', but is '{actualType}'"

        let r = r |> Option.orElse range // fall back to anon record range

        (r, fieldName, msg)

    /// Returns: errors
    let private fitsInterfaceMembers
        range
        (argExprs: Fable.Expr list)
        fieldNames
        (interface_: FSharpEntity)
        (fieldsToIgnore: Set<string>)
        (interfaceMembers: FSharpMemberOrFunctionOrValue list)
        =

        interfaceMembers
        |> List.filter (fun m ->
            not (m.Attributes |> Helpers.hasAttrib Atts.emitIndexer)
        )
        |> List.filter (fun m -> m.IsPropertyGetterMethod)
        |> List.choose (fun m ->
            if fieldsToIgnore |> Set.contains m.DisplayName then
                None
            else
                let expectedTypes = m.ReturnParameter.Type |> collectTypes

                fieldNames
                |> Array.tryFindIndex ((=) m.DisplayName)
                |> function
                    | None ->
                        if
                            expectedTypes
                            |> List.forall (
                                function
                                | Fable.Option _ -> true
                                | _ -> false
                            )
                        then
                            None // Optional fields can be missing
                        else
                            formatMissingFieldError
                                range
                                interface_
                                m.DisplayName
                                expectedTypes
                            |> Some
                    | Some i ->
                        let expr = List.item i argExprs
                        let ty = expr.Type

                        if
                            ty
                            |> fitsInto
                                (Allow.TheUsual (*||| Allow.AnyIntoErased*) )
                                expectedTypes
                        then
                            None
                        else
                            formatUnexpectedTypeError
                                range
                                interface_
                                None
                                m.DisplayName
                                expectedTypes
                                ty
                                expr.Range
                            |> Some
        )

    /// Returns errors
    let private fitsInterfaceIndexers
        range
        (argExprs: Fable.Expr list)
        fieldNames
        (interface_: FSharpEntity)
        (fieldsToIgnore: Set<string>)
        (interfaceMembers: FSharpMemberOrFunctionOrValue list)
        =

        // Note: Indexers are assumed to be "valid" index properties (like `string` and/or `int` input (TS rules))
        let indexers =
            interfaceMembers
            |> List.filter (fun m ->
                m.Attributes |> Helpers.hasAttrib Atts.emitIndexer
            )
            // Indexer:
            // * with explicit get: IsPropertyGetterMethod
            // * with explicit set: IsPropertySetterMetod
            // * without explicit get (readonly -> same as get): IsPropertyGetterMethod = false
            |> List.filter (fun m -> not m.IsPropertySetterMethod)
        // far from perfect: Erased Types are `Fable.Any` instead of their actual type
        // (exception: `Ux<...>` (and `Option<Ux<...>>`) -> types get extracted)
        let validTypes =
            indexers
            |> List.collect (fun i -> collectTypes i.ReturnParameter.Type)
            |> List.distinct

        match validTypes with
        | [] -> [] // no indexer
        | _ when validTypes |> List.contains Fable.Any -> []
        | _ ->
            List.zip (fieldNames |> Array.toList) argExprs
            |> List.filter (fun (fieldName, _) ->
                fieldsToIgnore |> Set.contains fieldName |> not
            )
            |> List.choose (fun (name, expr) ->
                let ty = expr.Type

                if
                    fitsInto
                        (Allow.TheUsual
                         ||| Allow.EnumIntoInt (*||| Allow.AnyIntoErased*) )
                        validTypes
                        ty
                then
                    None
                else
                    formatUnexpectedTypeError
                        range
                        interface_
                        (Some indexers)
                        name
                        validTypes
                        ty
                        expr.Range
                    |> Some
            )

    //let withoutErrored
    //    (interfaceMembers: FSharpMemberOrFunctionOrValue list)
    //    (errors: _ list)
    //    =
    //    let fieldsWithError = errors |> List.map (fun (_, fieldName, _) -> fieldName) |> Set.ofList
    //    interfaceMembers
    //    |> List.filter (fun m -> fieldsWithError |> Set.contains (m.DisplayName) |> not)

    let fitsInInterface
        (_com: IFableCompiler)
        (range: SourceLocation option)
        (argExprs: Fable.Expr list)
        (fieldNames: string array)
        (interface_: Fable.Entity)
        =
        match interface_ with
        | :? FsEnt as fsEnt ->
            let interface_ = fsEnt.FSharpEntity

            let interfaceMembers =
                Helpers.getAllInterfaceMembers interface_ |> Seq.toList

            // TODO: Check also if there are extra fields in the record not present in the interface?
            let fieldErrors =
                fitsInterfaceMembers
                    range
                    argExprs
                    fieldNames
                    interface_
                    Set.empty
                    interfaceMembers

            let indexerErrors =
                fitsInterfaceIndexers
                    range
                    argExprs
                    fieldNames
                    interface_
                    // don't check already errored fields
                    (fieldErrors
                     |> List.map (fun (_, fieldName, _) -> fieldName)
                     |> Set.ofList)
                    interfaceMembers

            List.append fieldErrors indexerErrors
            |> List.map (fun (r, _, m) -> (r, m))
            // sort errors by their appearance in code
            |> List.sortBy fst
            |> function
                | [] -> Ok()
                | errors -> Error errors
        | _ -> Ok() // TODO: Error instead if we cannot check the interface?
