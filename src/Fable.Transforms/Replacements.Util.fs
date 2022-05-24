module Fable.Transforms.Replacements.Util

#nowarn "1182"

open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

type Helper =
    static member ConstructorCall(consExpr: Expr, returnType: Type, args: Expr list, ?argTypes, ?genArgs, ?loc: SourceLocation) =
        let info = CallInfo.Make(args=args, ?sigArgTypes=argTypes, ?genArgs=genArgs)
        Call(consExpr, { info with IsConstructor = true }, returnType, loc)

    static member InstanceCall(callee: Expr, memb: string, returnType: Type, args: Expr list,
                               ?argTypes: Type list, ?genArgs, ?loc: SourceLocation) =
        let callee = getField callee memb
        let info = CallInfo.Make(args=args, ?sigArgTypes=argTypes, ?genArgs=genArgs)
        Call(callee, info, returnType, loc)

    static member Application(callee: Expr, returnType: Type, args: Expr list,
                               ?argTypes: Type list, ?loc: SourceLocation) =
        let info = defaultArg argTypes [] |> makeCallInfo None args
        Call(callee, info, returnType, loc)

    static member LibValue(com, coreModule: string, coreMember: string, returnType: Type) =
        makeImportLib com returnType coreMember coreModule

    static member LibCall(com, coreModule: string, coreMember: string, returnType: Type, args: Expr list,
                           ?argTypes: Type list, ?genArgs, ?thisArg: Expr, ?hasSpread: bool, ?isConstructor: bool, ?loc: SourceLocation) =

        let callee = makeImportLib com Any coreMember coreModule
        let info = CallInfo.Make(?thisArg=thisArg, args=args, ?sigArgTypes=argTypes, ?genArgs=genArgs)
        Call(callee, { info with HasSpread = defaultArg hasSpread false
                                 IsConstructor = defaultArg isConstructor false }, returnType, loc)

    static member ImportedCall(path: string, selector: string, returnType: Type, args: Expr list,
                                ?argTypes: Type list, ?genArgs, ?thisArg: Expr, ?hasSpread: bool, ?isConstructor: bool, ?loc: SourceLocation) =
        let callee = makeImportUserGenerated None Any selector path
        let info = CallInfo.Make(?thisArg=thisArg, args=args, ?sigArgTypes=argTypes, ?genArgs=genArgs)
        Call(callee, { info with HasSpread = defaultArg hasSpread false
                                 IsConstructor = defaultArg isConstructor false }, returnType, loc)

    static member GlobalCall(ident: string, returnType: Type, args: Expr list, ?argTypes: Type list, ?genArgs,
                             ?memb: string, ?isConstructor: bool, ?loc: SourceLocation) =
        let callee =
            match memb with
            | Some memb -> getField (makeIdentExpr ident) memb
            | None -> makeIdentExpr ident
        let info = CallInfo.Make(args=args, ?sigArgTypes=argTypes, ?genArgs=genArgs)
        Call(callee, { info with IsConstructor = defaultArg isConstructor false }, returnType, loc)

    static member GlobalIdent(ident: string, memb: string, typ: Type, ?loc: SourceLocation) =
        getFieldWith loc typ (makeIdentExpr ident) memb

let makeUniqueIdent ctx t name =
    FSharp2Fable.Helpers.getIdentUniqueName ctx name
    |> makeTypedIdent t

let withTag tag = function
    | Call(e, i, t, r) -> Call(e, { i with Tag = Some tag }, t, r)
    | e -> e

let objValue (k, v): MemberDecl =
    {
        Name = k
        FullDisplayName = k
        Args = []
        Body = v
        GenericParams = []
        UsedNames = Set.empty
        Info = FSharp2Fable.MemberInfo(isValue=true)
        ExportDefault = false
        DeclaringEntity = None
        XmlDoc = None
    }

let typedObjExpr t kvs =
    ObjectExpr(List.map objValue kvs, t, None)

let objExpr kvs =
    typedObjExpr Any kvs

let add left right =
    Operation(Binary(BinaryPlus, left, right), left.Type, None)

let sub left right =
    Operation(Binary(BinaryMinus, left, right), left.Type, None)

let eq left right =
    Operation(Binary(BinaryEqual, left, right), Boolean, None)

let neq left right =
    Operation(Binary(BinaryUnequal, left, right), Boolean, None)

let nullCheck r isNull expr =
    let op = if isNull then BinaryEqual else BinaryUnequal
    Operation(Binary(op, expr, Value(Null expr.Type, None)), Boolean, r)

let str txt = Value(StringConstant txt, None)

let genArg (com: ICompiler) (ctx: Context) r i (genArgs: Type list) =
    List.tryItem i genArgs
    |> Option.defaultWith (fun () ->
        "Couldn't find generic argument in position " + (string i)
        |> addError com ctx.InlinePath r
        Any)

let toArray r t expr =
    let t, kind =
        match t with
        | Array(t, kind) -> t, kind
        // This is used also by Seq.cache, which returns `'T seq` instead of `'T array`
        | DeclaredType(_, [t])
        | t -> t, MutableArray
    Value(NewArray(ArrayFrom expr, t, kind), r)

let getBoxedZero kind: obj =
    match kind with
    | Int8 -> 0y: int8
    | UInt8 -> 0uy: uint8
    | Int16 -> 0s: int16
    | UInt16 -> 0us: uint16
    | Int32 -> 0: int32
    | UInt32 -> 0u: uint32
    | Int64 -> 0L: int64
    | UInt64 -> 0UL: uint64
    | BigInt -> 0I: bigint
    | NativeInt -> 0n: nativeint
    | UNativeInt -> 0un: unativeint
    | Float32 -> 0.f: float32
    | Float64 -> 0.: float
    | Decimal -> 0M: decimal

let getBoxedOne kind: obj =
    match kind with
    | Int8 -> 1y: int8
    | UInt8 -> 1uy: uint8
    | Int16 -> 1s: int16
    | UInt16 -> 1us: uint16
    | Int32 -> 1: int32
    | UInt32 -> 1u: uint32
    | Int64 -> 1L: int64
    | UInt64 -> 1UL: uint64
    | BigInt -> 1I: bigint
    | NativeInt -> 1n: nativeint
    | UNativeInt -> 1un: unativeint
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
    | BclDictionary of key:Type * value:Type
    | BclKeyValuePair of key:Type * value:Type
    | FSharpSet of Type
    | FSharpMap of key:Type * value:Type
    | FSharpChoice of Type list
    | FSharpResult of Type * Type
    | FSharpReference of Type

let (|BuiltinDefinition|_|) = function
    | Types.guid -> Some BclGuid
    | Types.timespan -> Some BclTimeSpan
    | Types.datetime -> Some BclDateTime
    | Types.datetimeOffset -> Some BclDateTimeOffset
    | Types.dateOnly -> Some BclDateOnly
    | Types.timeOnly -> Some BclTimeOnly
    | "System.Timers.Timer" -> Some BclTimer
    | Types.decimal
    | Types.fsharpSet -> Some(FSharpSet(Any))
    | Types.fsharpMap -> Some(FSharpMap(Any,Any))
    | Types.hashset -> Some(BclHashSet(Any))
    | Types.dictionary -> Some(BclDictionary(Any,Any))
    | Types.keyValuePair -> Some(BclKeyValuePair(Any,Any))
    | Types.result -> Some(FSharpResult(Any,Any))
    | Types.byref -> Some(FSharpReference(Any))
    | Types.byref2 -> Some(FSharpReference(Any))
    | Types.reference -> Some(FSharpReference(Any))
    | Naming.StartsWith Types.choiceNonGeneric genArgs ->
        List.replicate (int genArgs[1..]) Any |> FSharpChoice |> Some
    | _ -> None

let (|BuiltinEntity|_|) (ent: string, genArgs) =
    match ent, genArgs with
    | BuiltinDefinition(FSharpSet _), [t] -> Some(FSharpSet(t))
    | BuiltinDefinition(FSharpMap _), [k;v] -> Some(FSharpMap(k,v))
    | BuiltinDefinition(BclHashSet _), [t] -> Some(BclHashSet(t))
    | BuiltinDefinition(BclDictionary _), [k;v] -> Some(BclDictionary(k,v))
    | BuiltinDefinition(BclKeyValuePair _), [k;v] -> Some(BclKeyValuePair(k,v))
    | BuiltinDefinition(FSharpResult _), [k;v] -> Some(FSharpResult(k,v))
    | BuiltinDefinition(FSharpReference _), [v] -> Some(FSharpReference(v))
    | BuiltinDefinition(FSharpReference _), [v; _] -> Some(FSharpReference(v))
    | BuiltinDefinition(FSharpChoice _), genArgs -> Some(FSharpChoice genArgs)
    | BuiltinDefinition t, _ -> Some t
    | _ -> None

let (|Builtin|_|) = function
    | DeclaredType(ent, genArgs) ->
        match ent.FullName, genArgs with
        | BuiltinEntity x -> Some x
        | _ -> None
    | _ -> None

let getElementType = function
    | Array(t,_) -> t
    | List t -> t
    | DeclaredType(_, [t]) -> t
    | _ -> Any

let genericTypeInfoError (name: string) =
    $"Cannot get type info of generic parameter {name}. Fable erases generics at runtime, try inlining the functions so generics can be resolved at compile time."

let splitFullName (fullname: string) =
    let fullname =
        match fullname.IndexOf("[") with
        | -1 -> fullname
        | i -> fullname[..i - 1]
    match fullname.LastIndexOf(".") with
    | -1 -> "", fullname
    | i -> fullname.Substring(0, i), fullname.Substring(i + 1)

let getTypeNameFromFullName (fullname: string) =
    let fullname =
        match fullname.IndexOf("[") with
        | -1 -> fullname
        | i -> fullname[.. i - 1]

    match fullname.LastIndexOf(".") with
    | -1 -> fullname
    | i -> fullname.Substring(i + 1)

let rec getTypeName com (ctx: Context) r t =
    match t with
    | GenericParam(name=name) ->
        genericTypeInfoError name
        |> addError com ctx.InlinePath r
        name
    | Array(elemType,_) -> // TODO: check kind
        getTypeName com ctx r elemType + "[]"
    | _ ->
        getTypeFullName false t |> splitFullName |> snd

let makeDeclaredType assemblyName genArgs fullName =
    let entRef: EntityRef = {
        FullName = fullName
        Path = CoreAssemblyName assemblyName
    }
    DeclaredType(entRef, genArgs)

let makeRuntimeType genArgs fullName =
    makeDeclaredType "System.Runtime" genArgs fullName

let makeFSharpCoreType genArgs fullName =
    makeDeclaredType "FSharp.Core" genArgs fullName

let makeStringTemplate tag (str: string) (holes: {| Index: int; Length: int |}[]) values =
    let mutable prevIndex = 0
    let parts = [
        for i = 0 to holes.Length - 1 do
            let m = holes[i]
            let strPart = str.Substring(prevIndex, m.Index - prevIndex)
            prevIndex <- m.Index + m.Length
            strPart
        str.Substring(prevIndex)
    ]
    StringTemplate(tag, parts, values)

let makeStringTemplateFrom simpleFormats values = function
    | StringConst str ->
        // In the case of interpolated strings, the F# compiler doesn't resolve escaped %
        // (though it does resolve double braces {{ }})
        let str = str.Replace("%%" , "%")
        (Some [], Regex.Matches(str, @"((?<!%)%(?:[0+\- ]*)(?:\d+)?(?:\.\d+)?\w)?%P\(\)") |> Seq.cast<Match>)
        ||> Seq.fold (fun acc m ->
            match acc with
            | None -> None
            | Some acc ->
                // TODO: If arguments need format, format them individually
                let doesNotNeedFormat =
                    not m.Groups[1].Success
                    || (Array.contains m.Groups[1].Value simpleFormats)
                if doesNotNeedFormat
                then {| Index = m.Index; Length = m.Length |}::acc |> Some
                else None)
        |> Option.map (fun holes ->
            let holes = List.toArray holes |> Array.rev
            makeStringTemplate None str holes values)
    | _ -> None

let rec namesof com ctx acc e =
    match acc, e with
    | acc, Get(e, ExprGet(StringConst prop), _, _) -> namesof com ctx (prop::acc) e
    | acc, Get(e, FieldGet i, _, _) -> namesof com ctx (i.Name::acc) e
    | [], IdentExpr ident -> ident.DisplayName::acc |> Some
    | [], NestedLambda(args, Call(IdentExpr ident, info, _, _), c) ->
        if List.sameLength args info.Args && List.zip args info.Args |> List.forall (fun (a1, a2) ->
            match a2 with IdentExpr id2 -> a1.Name = id2.Name | _ -> false)
        then ident.DisplayName::acc |> Some
        else None
    | [], Value(TypeInfo(t, _), r) -> (getTypeName com ctx r t)::acc |> Some
    | [], _ -> None
    | acc, _ -> Some acc

let curriedApply r t applied args =
    CurriedApply(applied, args, t, r)

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
        [argExpr]
        |> curriedApply None interType (IdentExpr capturedFun1Var)
        |> List.singleton
        |> curriedApply r retType (IdentExpr capturedFun2Var)
    Let(capturedFun1Var, f1, Let(capturedFun2Var, f2, Lambda(arg, body, FuncInfo.Empty)))

let (|Namesof|_|) com ctx e = namesof com ctx [] e
let (|Nameof|_|) com ctx e = namesof com ctx [] e |> Option.bind List.tryLast

let (|ReplaceName|_|) (namesAndReplacements: (string*string) list) name =
    namesAndReplacements |> List.tryPick (fun (name2, replacement) ->
        if name2 = name then Some replacement else None)

let (|OrDefault|) (def:'T) = function
    | Some v -> v
    | None -> def

let (|EntFullName|_|) (typ: Type) =
    match typ with
    | DeclaredType(ent, _) -> Some ent.FullName
    | _ -> None

let (|ListLiteral|_|) e =
    let rec untail t acc = function
        | Value(NewList(None, _),_) -> Some(List.rev acc, t)
        | Value(NewList(Some(head, tail), _),_) -> untail t (head::acc) tail
        | _ -> None
    match e with
    | NewList(None, t) -> Some([], t)
    | NewList(Some(head, tail), t) -> untail t [head] tail
    | _ -> None

let (|ArrayOrListLiteral|_|) = function
    | MaybeCasted(Value((NewArray(ArrayValues vals, t,_)|ListLiteral(vals, t)),_)) -> Some(vals, t)
    | _ -> None

let (|IsEntity|_|) fullName = function
    | DeclaredType(entRef, genArgs) ->
        if entRef.FullName = fullName
        then Some(entRef, genArgs)
        else None
    | _ -> None

let (|IDictionary|IEqualityComparer|Other|) = function
    | DeclaredType(ent,_) ->
        match ent.FullName with
        | Types.idictionary -> IDictionary
        | Types.equalityComparer -> IEqualityComparer
        | _ -> Other
    | _ -> Other

let (|IEnumerable|IEqualityComparer|Other|) = function
    | DeclaredType(ent,_) ->
        match ent.FullName with
        | Types.ienumerableGeneric -> IEnumerable
        | Types.equalityComparer -> IEqualityComparer
        | _ -> Other
    | _ -> Other

let (|Enumerator|Other|) = function
    | "System.CharEnumerator"
    | "System.Collections.Generic.List`1.Enumerator"
    | "System.Collections.Generic.HashSet`1.Enumerator"
    | "System.Collections.Generic.Dictionary`2.Enumerator"
    | "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator"
    | "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator"
        -> Enumerator
    | _ -> Other

let (|IsEnumerator|_|) = function
    | DeclaredType(entRef, genArgs) ->
        match entRef.FullName with
        | Enumerator -> Some(entRef, genArgs)
        | _ -> None
    | _ -> None

let (|NewAnonymousRecord|_|) = function
    // The F# compiler may create some bindings of expression arguments to fix https://github.com/dotnet/fsharp/issues/6487
    | NestedRevLets(bindings, Value(NewAnonymousRecord(exprs, fieldNames, genArgs), r)) ->
        Some(List.rev bindings, exprs, fieldNames, genArgs, r)
    | Value(NewAnonymousRecord(exprs, fieldNames, genArgs), r) ->
        Some([], exprs, fieldNames, genArgs, r)
    | _ -> None

let (|ListSingleton|) x = [x]

let tryFindInScope (ctx: Context) identName =
    let rec findInScopeInner scope identName =
        match scope with
        | [] -> None
        | (ident2: Ident, expr: Expr option)::prevScope ->
            if identName = ident2.Name then
                match expr with
                | Some(MaybeCasted(IdentExpr ident)) when not ident.IsMutable -> findInScopeInner prevScope ident.Name
                | expr -> expr
            else findInScopeInner prevScope identName
    let scope1 = ctx.Scope |> List.map (fun (_,i,e) -> i,e)
    let scope2 = ctx.ScopeInlineArgs |> List.map (fun (i,e) -> i, Some e)
    findInScopeInner (scope1 @ scope2) identName

let (|MaybeInScope|) (ctx: Context) e =
    match e with
    | IdentExpr ident ->
        match tryFindInScope ctx ident.Name with
        | Some e -> e
        | None -> e
    | e -> e

let (|RequireStringConst|) com (ctx: Context) r e =
    match e with
    | MaybeInScope ctx (StringConst s) -> s
    | _ ->
        addError com ctx.InlinePath r "Expecting string literal"
        ""

let (|CustomOp|_|) (com: ICompiler) (ctx: Context) r t opName (argExprs: Expr list) sourceTypes =
   let argTypes = argExprs |> List.map (fun a -> a.Type)
   match FSharp2Fable.TypeHelpers.tryFindWitness ctx argTypes false opName with
   | Some w ->
       let callInfo = makeCallInfo None argExprs w.ArgTypes
       makeCall r t callInfo w.Expr |> Some
   | None ->
        sourceTypes |> List.tryPick (function
            | DeclaredType(ent,_) ->
                let ent = com.GetEntity(ent)
                FSharp2Fable.TypeHelpers.tryFindMember com ent ctx.GenericArgs opName false argTypes
            | _ -> None)
        |> Option.map (FSharp2Fable.Util.makeCallFrom com ctx r t [] None argExprs)
