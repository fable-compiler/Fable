module Fable.AST.Fable.Util
open Fable
open Fable.AST

let attachRange (range: SourceLocation option) msg =
    match range with
    | Some range -> msg + " " + (string range)
    | None -> msg

let attachRangeAndFile (range: SourceLocation option) (fileName: string) msg =
    match range with
    | Some range -> msg + " " + (string range) + " (" + fileName + ")"
    | None -> msg + " (" + fileName + ")"

type CallKind =
    | InstanceCall of callee: Expr * meth: string * args: Expr list
    | ImportCall of importPath: string * modName: string * meth: string option * isCons: bool * args: Expr list
    | CoreLibCall of modName: string * meth: string option * isCons: bool * args: Expr list
    | GlobalCall of modName: string * meth: string option * isCons: bool * args: Expr list

let makeLoop range loopKind = Loop (loopKind, range)
let makeIdent name = Ident(name)
let makeTypedIdent name typ = Ident(name, typ)
let makeIdentExpr name = makeIdent name |> IdentValue |> Value
let makeLambdaExpr args body = Value(Lambda(args, body, LambdaInfo(true)))

let makeCoreRef modname prop =
    Value(ImportRef(defaultArg prop "default", modname, CoreLib))

let makeImport (selector: string) (path: string) =
    Value(ImportRef(selector.Trim(), path.Trim(), CustomImport))

let private makeOp range typ args op =
    Apply (Value op, args, ApplyMeth, typ, range)

let makeBinOp range typ args op =
    makeOp range typ args (BinaryOp op)

let makeUnOp range typ args op =
    makeOp range typ args (UnaryOp op)

let makeLogOp range args op =
    makeOp range Boolean args (LogicalOp op)

let makeEqOp range args op =
    makeOp range Boolean args (BinaryOp op)

let rec makeSequential range statements =
    match statements with
    | [] -> Value Null
    | [expr] -> expr
    | first::rest ->
        match first, rest with
        | first, _ when first.IsNull -> makeSequential range rest
        | Sequential (statements, _), _ -> makeSequential range (statements@rest)
        | _, [Sequential (statements, _)] -> makeSequential range (first::statements)
        // Calls to System.Object..ctor in class constructors
        | ObjExpr ([],[],_,_), _ -> makeSequential range rest
        | _ -> Sequential (statements, range)

let makeLongInt (x: uint64) unsigned =
    let lowBits = NumberConst (float (uint32 x), Float64)
    let highBits = NumberConst (float (x >>> 32), Float64)
    let unsigned = BoolConst (unsigned)
    let args = [Value lowBits; Value highBits; Value unsigned]
    Apply (makeCoreRef "Long" (Some "fromBits"), args, ApplyMeth, Any, None)

let makeFloat32 (x: float32) =
    let args = [Value (NumberConst (float x, Float32))]
    let callee = Apply (makeIdentExpr "Math", [Value (StringConst "fround")], ApplyGet, Any, None)
    Apply (callee, args, ApplyMeth, Any, None)

let makeBoolConst (x: bool) = BoolConst x |> Value
let makeStrConst (x: string) = StringConst x |> Value
let makeIntConst (x: int) = NumberConst (float x, Int32) |> Value
let makeNumConst (x: float) = NumberConst (float x, Float64) |> Value
let makeDecConst (x: decimal) = NumberConst (float x, Float64) |> Value

let makeTypeConst (typ: Type) (value: obj) =
    match typ, value with
    // Long Integer types
    | ExtendedNumber Int64, (:? int64 as x) -> makeLongInt (uint64 x) false
    | ExtendedNumber UInt64, (:? uint64 as x) -> makeLongInt x true
    // Decimal type
    | ExtendedNumber Decimal, (:? decimal as x) -> makeDecConst x
    // Enum 64-bit types (TODO: proper JS support, as Enum has no type)
    | Enum _, (:? int64 as x) -> makeLongInt (uint64 x) false
    | Enum _, (:? uint64 as x) -> makeLongInt x true
    // Short Float type
    | Number Float32, (:? float32 as x) -> makeFloat32 x
    | _ ->
        match typ, value with
        | Boolean, (:? bool as x) -> BoolConst x
        | String, (:? string as x) -> StringConst x
        | Char, (:? char as x) -> StringConst (string x)
        // Integer types
        | Number UInt8, (:? byte as x) -> NumberConst (float x, UInt8)
        | Number Int8, (:? sbyte as x) -> NumberConst (float x, Int8)
        | Number Int16, (:? int16 as x) -> NumberConst (float x, Int16)
        | Number UInt16, (:? uint16 as x) -> NumberConst (float x, UInt16)
        | Number Int32, (:? int as x) -> NumberConst (float x, Int32)
        | Number UInt32, (:? uint32 as x) -> NumberConst (float x, UInt32)
        // Float types
        | Number Float64, (:? float as x) -> NumberConst (float x, Float64)
        // Enums (TODO: proper JS support, as Enum has no type)
        | Enum _, (:? byte as x) -> NumberConst (float x, UInt8)
        | Enum _, (:? sbyte as x) -> NumberConst (float x, Int8)
        | Enum _, (:? int16 as x) -> NumberConst (float x, Int16)
        | Enum _, (:? uint16 as x) -> NumberConst (float x, UInt16)
        | Enum _, (:? int as x) -> NumberConst (float x, Int32)
        | Enum _, (:? uint32 as x) -> NumberConst (float x, UInt32)
        // TODO: Regex
        | Unit, (:? unit) | _ when isNull value -> Null
        // Arrays with small data type (ushort, byte) come as Const
        | Array (Number kind), (:? (byte[]) as arr) ->
            let values = arr |> Array.map (fun x -> NumberConst (float x, kind) |> Value) |> Seq.toList
            ArrayConst (ArrayValues values, Number kind)
        | Array (Number kind), (:? (uint16[]) as arr) ->
            let values = arr |> Array.map (fun x -> NumberConst (float x, kind) |> Value) |> Seq.toList
            ArrayConst (ArrayValues values, Number kind)
        | _ -> failwithf "Unexpected type %A, literal %O" typ value
        |> Value

let makeGet range typ callee propExpr =
    Apply (callee, [propExpr], ApplyGet, typ, range)

let makeUntypedGet callee prop =
    Apply (callee, [Value(StringConst prop)], ApplyGet, Any, None)

let makeArray elementType arrExprs =
    ArrayConst(ArrayValues arrExprs, elementType) |> Value

/// Ignores relative imports (e.g. `[<Import("foo","./lib.js")>]`)
let tryImported (name: Lazy<string>) (decs: #seq<Decorator>) =
    decs |> Seq.tryPick (fun x ->
        match x.Name, x.Arguments with
        | "Global", [:? string as name] ->
            makeIdent name |> IdentValue |> Value |> Some
        | "Global", _ ->
            makeIdent name.Value |> IdentValue |> Value |> Some
        | "Import", [(:? string as memb); (:? string as path)]
            when not(path.StartsWith ".") ->
            Some(Value(ImportRef(memb.Trim(), path.Trim(), CustomImport)))
        | _ -> None)

let makeJsObject range (props: (string * Expr) list) =
    let membs = props |> List.map (fun (name, body) ->
        let m = Member(name, Field, InstanceLoc, [], body.Type)
        m, [], body)
    ObjExpr(membs, [], None, range)

let getTypedArrayName (com: ICompiler) numberKind =
    match numberKind with
    | Int8 -> "Int8Array"
    | UInt8 -> if com.Options.clampByteArrays then "Uint8ClampedArray" else "Uint8Array"
    | Int16 -> "Int16Array"
    | UInt16 -> "Uint16Array"
    | Int32 -> "Int32Array"
    | UInt32 -> "Uint32Array"
    | Float32 -> "Float32Array"
    | Float64 -> "Float64Array"

let makeNonDeclaredTypeRef (nonDeclType: NonDeclaredType) =
    let get t =
        Wrapped(makeCoreRef "Util" (Some t), MetaType)
    let call t args =
        Apply(makeCoreRef "Util" (Some t), args, ApplyMeth, MetaType, None)
    match nonDeclType with
    | NonDeclAny -> get "Any"
    | NonDeclUnit -> get "Unit"
    | NonDeclOption genArg -> call "Option" [genArg]
    | NonDeclArray genArg -> call "Array" [genArg]
    | NonDeclTuple genArgs ->
        ArrayConst(ArrayValues genArgs, Any) |> Value
        |> List.singleton |> call "Tuple"
    | NonDeclFunction genArgs ->
        ArrayConst(ArrayValues genArgs, Any) |> Value
        |> List.singleton |> call "Function"
    | NonDeclGenericParam name ->
        call "GenericParam" [Value(StringConst name)]
    | NonDeclInterface name ->
        call "Interface" [Value(StringConst name)]

type GenericInfo = {
    makeGeneric: bool
    genericAvailability: bool
}

let rec makeTypeRef (com: ICompiler) (genInfo: GenericInfo) typ =
    let str s = Wrapped(Value(StringConst s), MetaType)
    match typ with
    | Boolean -> str "boolean"
    | Char
    | String -> str "string"
    | Number _ | Enum _ -> str "number"
    | ExtendedNumber kind ->
        match kind with
        | Int64|UInt64 -> makeCoreRef "Long" (Some "Long")
        | Decimal -> str "number"
        | BigInt -> makeCoreRef "BigInt" None
    | Function(argTypes, returnType, _) ->
        argTypes@[returnType]
        |> List.map (makeTypeRef com genInfo)
        |> NonDeclFunction
        |> makeNonDeclaredTypeRef
    | MetaType | Any -> makeNonDeclaredTypeRef NonDeclAny
    | Unit -> makeNonDeclaredTypeRef NonDeclUnit
    | Array (Number kind) when com.Options.typedArrays ->
        let def = Ident(getTypedArrayName com kind, MetaType) |> IdentValue |> Value
        Apply(makeCoreRef "Util" (Some "Array"), [def; makeBoolConst true], ApplyMeth, MetaType, None)
    | Array genArg ->
        makeTypeRef com genInfo genArg
        |> NonDeclArray
        |> makeNonDeclaredTypeRef
    | Option genArg ->
        makeTypeRef com genInfo genArg
        |> NonDeclOption
        |> makeNonDeclaredTypeRef
    | Tuple genArgs ->
        List.map (makeTypeRef com genInfo) genArgs
        |> NonDeclTuple
        |> makeNonDeclaredTypeRef
    | GenericParam name ->
        if genInfo.genericAvailability
        then (makeIdentExpr Naming.genArgsIdent, Value(StringConst name))
             ||> makeGet None MetaType
        else makeNonDeclaredTypeRef (NonDeclGenericParam name)
    | DeclaredType(ent, _) when ent.Kind = Interface ->
        makeNonDeclaredTypeRef (NonDeclInterface ent.FullName)
    | DeclaredType(ent, genArgs) ->
        // Imported types come from JS so they don't need to be made generic
        match tryImported (lazy ent.Name) ent.Decorators with
        | Some expr -> expr
        | None when not genInfo.makeGeneric || genArgs.IsEmpty -> Value(TypeRef(ent,[]))
        | None ->
            List.map (makeTypeRef com genInfo) genArgs
            |> List.zip ent.GenericParameters
            |> fun genArgs -> Value(TypeRef(ent, genArgs))

let makeCall (range: SourceLocation option) typ kind =
    let getCallee meth args returnType owner =
        match meth with
        | None -> owner
        | Some meth ->
            // let fnTyp = Function(List.map Expr.getType args |> Some, returnType)
            Apply (owner, [makeStrConst meth], ApplyGet, Any, None)
    let apply kind args callee =
        Apply(callee, args, kind, typ, range)
    let getKind isCons =
        if isCons then ApplyCons else ApplyMeth
    match kind with
    | InstanceCall (callee, meth, args) ->
        // let fnTyp = Function(List.map Expr.getType args |> Some, typ)
        Apply (callee, [makeStrConst meth], ApplyGet, Any, None)
        |> apply ApplyMeth args
    | ImportCall (importPath, modName, meth, isCons, args) ->
        Value (ImportRef (modName, importPath, CustomImport))
        |> getCallee meth args typ
        |> apply (getKind isCons) args
    | CoreLibCall (modName, meth, isCons, args) ->
        makeCoreRef modName meth
        |> apply (getKind isCons) args
    | GlobalCall (modName, meth, isCons, args) ->
        makeIdentExpr modName
        |> getCallee meth args typ
        |> apply (getKind isCons) args

let makeNonGenTypeRef com typ =
    makeTypeRef com { makeGeneric=false; genericAvailability=false } typ

let makeTypeRefFrom com ent =
    let genInfo = { makeGeneric=false; genericAvailability=false }
    DeclaredType(ent, []) |> makeTypeRef com genInfo

let makeEmit r t args macro =
    Apply(Value(Emit macro), args, ApplyMeth, t, r)

let rec makeTypeTest com range (typ: Type) expr =
    let jsTypeof (primitiveType: string) expr =
        let typof = makeUnOp None String [expr] UnaryTypeof
        makeBinOp range Boolean [typof; makeStrConst primitiveType] BinaryEqualStrict
    let jsInstanceOf (typeRef: Expr) expr =
        makeBinOp None Boolean [expr; typeRef] BinaryInstanceOf
    match typ with
    | MetaType -> Value(BoolConst false) // This shouldn't happen
    | Char
    | String _ -> jsTypeof "string" expr
    | Number _ | Enum _ -> jsTypeof "number" expr
    | ExtendedNumber (Int64|UInt64) ->
        makeBinOp range Boolean [expr; makeCoreRef "Long" (Some "Long")] BinaryInstanceOf
    | ExtendedNumber Decimal -> jsTypeof "number" expr
    | ExtendedNumber BigInt ->
        makeBinOp range Boolean [expr; makeCoreRef "BigInt" None] BinaryInstanceOf
    | Boolean -> jsTypeof "boolean" expr
    | Unit -> makeBinOp range Boolean [expr; Value Null] BinaryEqual
    | Function _ -> jsTypeof "function" expr
    | Array _ | Tuple _ ->
        CoreLibCall ("Util", Some "isArray", false, [expr])
        |> makeCall range Boolean
    | Any -> makeBoolConst true
    | Option typ -> makeTypeTest com range typ expr
    | DeclaredType(typEnt, _) ->
        match typEnt.Kind with
        | Interface ->
            CoreLibCall ("Util", Some "hasInterface", false, [expr; makeStrConst typEnt.FullName])
            |> makeCall range Boolean
        | _ ->
            makeBinOp range Boolean [expr; makeNonGenTypeRef com typ] BinaryInstanceOf
    | GenericParam name ->
        "Cannot type test generic parameter " + name
        |> attachRange range |> failwith

let makeUnionCons cases =
    let args = [Ident("tag", String); Ident("data", Any)]
    let argTypes = List.map Ident.getType args
    let setter1 = Set(Value This, Some(makeStrConst "tag"), Value(IdentValue args.[0]), None)
    let setter2 = Set(Value This, Some(makeStrConst "data"), Value(IdentValue args.[1]), None)
    let body = Sequential([setter1; setter2], None)
    MemberDeclaration(Member(".ctor", Constructor, InstanceLoc, argTypes, Any), true, None, args, body, None)

/// This is necessary when extending built-in JS types and compiling to ES5
/// See https://github.com/Microsoft/TypeScript/wiki/Breaking-Changes#extending-built-ins-like-error-array-and-map-may-no-longer-work
let setProto com (ent: Entity) =
    let meth = makeUntypedGet (makeIdentExpr "Object") "setPrototypeOf"
    Apply(meth, [This |> Value; makeUntypedGet (DeclaredType(ent, []) |> makeNonGenTypeRef com) "prototype"], ApplyMeth, Any, None)

let makeRecordCons com (ent: Entity) (props: (string*Type) list) =
    let args =
        ([], props) ||> List.fold (fun args (name, typ) ->
            let name =
                Naming.lowerFirst name |> Naming.sanitizeIdent (fun x ->
                    List.exists (fun (y: Ident) -> y.Name = x) args)
            (Ident(name, typ))::args)
        |> List.rev
    let body =
        List.zip args props
        |> List.map (fun (arg, (propName, _)) ->
            Set(Value This, Some(makeStrConst propName), arg |> IdentValue |> Value, None))
        |> fun setters ->
            match ent.Kind with
            | Exception _ ->
                // TODO: If we are compiling to ES2015 this is not needed
                let superCall = Apply(Value Super, [], ApplyMeth, Any, None)
                Sequential(superCall::(setProto com ent)::setters, None)
            | _ -> Sequential(setters, None)
    MemberDeclaration(Member(".ctor", Constructor, InstanceLoc, List.map Ident.getType args, Any), true, None, args, body, None)

let private makeMeth argType returnType name coreMeth =
    let arg = Ident("other", argType)
    let body =
        CoreLibCall("Util", Some coreMeth, false, [Value This; Value(IdentValue arg)])
        |> makeCall None returnType
    MemberDeclaration(Member(name, Method, InstanceLoc, [arg.Type], returnType), true, None, [arg], body, None)

let makeUnionEqualMethod argType =
    let this = Value This
    let arg = Ident("other", argType)
    let argValue = Value(IdentValue arg)
    let equalsTag =
        makeEqOp None [makeUntypedGet this "tag"; makeUntypedGet argValue "tag"] BinaryEqualStrict
    let equalsData =
        CoreLibCall("Util", Some "equals", false, [makeUntypedGet this "data"; makeUntypedGet argValue "data"])
        |> makeCall None Boolean
    let andOp = Apply(Value (LogicalOp LogicalAnd), [equalsTag; equalsData], ApplyMeth, Boolean, None)
    let body = Apply(Value (LogicalOp LogicalOr), [makeEqOp None [this; argValue] BinaryEqualStrict; andOp], ApplyMeth, Boolean, None)
    MemberDeclaration(Member("Equals", Method, InstanceLoc, [arg.Type], Boolean), true, None, [arg], body, None)

let makeRecordEqualMethod argType = makeMeth argType Boolean "Equals" "equalsRecords"
let makeUnionCompareMethod argType = makeMeth argType (Number Int32) "CompareTo" "compareUnions"
let makeRecordCompareMethod argType = makeMeth argType (Number Int32) "CompareTo" "compareRecords"

let makeIteratorMethodArgsAndBody() =
    let body =
        let arg = Apply (makeUntypedGet (Value This) "GetEnumerator", [], ApplyMeth, Any, None)
        CoreLibCall("Seq", Some "toIterator", false, [arg]) |> makeCall None Any
    let comp = makeUntypedGet (makeIdentExpr "Symbol") "iterator"
    Member("Symbol.iterator", Method, InstanceLoc, [], Any, computed=comp), [], body

let makeIteratorMethod() =
    let m, args, body = makeIteratorMethodArgsAndBody()
    MemberDeclaration(m, true, None, args, body, None)

let makeReflectionMethodArgsAndBody com (ent: Entity option) extend nullable interfaces cases properties =
    let members = [
        match ent with
        | Some ent -> yield "type", Value(StringConst ent.FullName)
        | None -> ()
        if nullable then yield "nullable", Value(BoolConst true)
        if extend || not(List.isEmpty interfaces)
        then
            let interfaces = List.map (StringConst >> Value) interfaces
            yield "interfaces", Value(ArrayConst(ArrayValues interfaces, String))
        yield! properties |> function
        | Some properties ->
            let genInfo = { makeGeneric=true; genericAvailability=false }
            properties |> List.map (fun (name, typ) ->
                let body = makeTypeRef com genInfo typ
                Member(name, Field, InstanceLoc, [], Any), [], body)
            |> fun decls -> ["properties", ObjExpr(decls, [], None, None)]
        | None -> []
        yield! cases |> function
        | Some cases ->
            let genInfo = { makeGeneric=true; genericAvailability=false }
            cases |> List.map (fun (tag, typs) ->
                let tag = Value(StringConst tag)
                let typs = List.map (makeTypeRef com genInfo) typs
                ArrayConst(ArrayValues(tag::typs), Any) |> Value)
            |> fun cases ->
                ["cases", ArrayConst(ArrayValues cases, Any) |> Value]
        | None -> []
    ]
    let info =
        match ent, extend with
        | Some ent, true ->
            CoreLibCall("Util", Some "extendInfo", false,
                [makeTypeRefFrom com ent; makeJsObject None members]) |> makeCall None Any
        | _ -> makeJsObject None members
    let comp = makeUntypedGet (makeCoreRef "Symbol" None) "reflection"
    Member("FSymbol.reflection", Method, InstanceLoc, [], Any, computed=comp), [], info

let makeReflectionMethod com (ent: Fable.Entity) extend nullable cases properties =
    let interfaces =
        match ent.Kind with
        | Fable.Union _ -> "FSharpUnion"::ent.Interfaces
        | Fable.Record _ -> "FSharpRecord"::ent.Interfaces
        | Fable.Exception _ -> "FSharpException"::ent.Interfaces
        | _ -> ent.Interfaces
    let m, args, body = makeReflectionMethodArgsAndBody com (Some ent) extend nullable interfaces cases properties
    MemberDeclaration(m, true, None, args, body, None)

let (|Type|) (expr: Expr) = expr.Type

// Unit args can be removed from final code,
// so we must prevent references to missing args
let argIdentToExpr (id: Ident) =
    match id.Type with
    | Unit -> Value Null
    | _ -> IdentValue id |> Value

let makeDynamicCurriedLambda range typ lambda =
    let args =
        match lambda with
        | Value(Lambda(_,_,info)) when info.CaptureThis -> [lambda; Value This]
        | _ -> [lambda]
    CoreLibCall("CurriedLambda", None, false, args)
    |> makeCall range typ

let (|CurriedLambda|_|) = function
    | Apply(Value(ImportRef("default", "CurriedLambda", CoreLib)),_,_,_,_) ->
        Some CurriedLambda
    | _ -> None

// Deal with function arguments with higher arity than expected
// E.g.: [|"1";"2"|] |> Array.map (fun x y -> x + y)
// JS: ["1","2"].map($var1 => $var2 => ((x, y) => x + y)($var1, $var2))
let rec ensureArity com argTypes args =
    let rec needsWrapping = function
        | Option(Function(expected,_,_)), Option(Function(actual,returnType,_))
        | Function(expected,_,_), Function(actual,returnType,_) ->
            let expectedLength = List.length expected
            let actualLength = List.length actual
            if (expectedLength < actualLength)
                || (expectedLength > actualLength)
                || List.zip expected actual |> List.exists (needsWrapping >> Option.isSome)
            then Some(expected, actual, returnType)
            else None
        | _ -> None
    let (|NeedsWrapping|_|) (expectedType, arg: Expr) =
        needsWrapping (expectedType, arg.Type)
    let wrap (com: ICompiler) typ (f: Expr) expectedArgs actualArgs =
        let outerArgs =
            expectedArgs |> List.map (fun t -> makeTypedIdent (com.GetUniqueVar()) t)
        let expectedArgsLength = List.length expectedArgs
        let actualArgsLength = List.length actualArgs
        if expectedArgsLength < actualArgsLength then
            List.skip expectedArgsLength actualArgs
            |> List.map (fun t -> makeTypedIdent (com.GetUniqueVar()) t)
            |> fun innerArgs ->
                let args = outerArgs@innerArgs |> List.map argIdentToExpr
                makeApply com f.Range typ f args
                |> makeLambdaExpr innerArgs
        elif expectedArgsLength > actualArgsLength then
            // if Option.isSome f.Range then
            //     com.AddLog("A function with less arguments than expected has been wrapped. " +
            //                 "Side effects may be delayed.", Warning, f.Range.Value) // filename
            let innerArgs = List.take actualArgsLength outerArgs |> List.map argIdentToExpr
            let outerArgs = List.skip actualArgsLength outerArgs |> List.map argIdentToExpr
            let innerApply = makeApply com f.Range (Function(List.map Expr.getType outerArgs,typ,true)) f innerArgs
            makeApply com f.Range typ innerApply outerArgs
        else
            outerArgs |> List.map argIdentToExpr
            |> makeApply com f.Range typ f
        |> makeLambdaExpr outerArgs
    if not(List.sameLength argTypes args) then args else // TODO: Raise warning?
    List.zip argTypes args
    |> List.map (fun (argType, arg: Expr) ->
        match argType, arg with
        // Dynamic CurriedLambda shouldn't be wrapped, see #996
        | _, (CurriedLambda as curriedLambda) -> curriedLambda
        // If the expected type is a generic parameter, we cannot infer the arity
        // so generate a dynamic curried lambda just in case.
        | GenericParam _, (Type(Function(args,_,isCurried)) as lambda)
                when isCurried && List.isMultiple args ->
            makeDynamicCurriedLambda lambda.Range lambda.Type lambda
        | NeedsWrapping (expected, actual, returnType) ->
            wrap com returnType arg expected actual
        | _ -> arg)

and makeApply com range typ callee (args: Expr list) =
    let callee =
        match callee with
        // If we're applying against a F# let binding, wrap it with a lambda
        | Sequential _ ->
            Apply(Value(Lambda([],callee,LambdaInfo(true))), [], ApplyMeth, callee.Type, callee.Range)
        | _ -> callee
    match callee with
    // Dynamic CurriedLambda shouldn't be wrapped, see #996
    | CurriedLambda _ -> Apply(callee, args, ApplyMeth, typ, range)
    // Make necessary transformations if we're applying more or less
    // arguments than the specified function arity
    | Type(Function(argTypes, _, _)) ->
        let argsLength = List.length args
        let argTypesLength = List.length argTypes
        if argTypesLength < argsLength && argTypesLength >= 1 // TODO: Remove >= 1
        then
            let innerArgs = List.take argTypesLength args
            let outerArgs = List.skip argTypesLength args
            Apply(callee, ensureArity com argTypes innerArgs, ApplyMeth,
                    Function(List.map Expr.getType outerArgs, typ, true), range)
            |> makeApply com range typ <| outerArgs
        elif argTypesLength > argsLength && argsLength >= 1 // TODO: Remove >= 1
        then
            List.skip argsLength argTypes
            |> List.map (fun t -> Ident(com.GetUniqueVar(), t))
            |> fun argTypes2 ->
                let args2 = argTypes2 |> List.map argIdentToExpr
                Apply(callee, ensureArity com argTypes (args@args2), ApplyMeth, typ, range)
                |> makeLambdaExpr argTypes2
        else
            Apply(callee, ensureArity com argTypes args, ApplyMeth, typ, range)
    | _ ->
        Apply(callee, args, ApplyMeth, typ, range)

/// Helper when we need to compare the types of the arguments applied to a method
/// (concrete) with the declared argument types for that method (may be generic)
/// (e.g. when resolving a TraitCall)
let compareDeclaredAndAppliedArgs declaredArgs appliedArgs =
    let listsEqual f li1 li2 =
        if not(List.sameLength li1 li2)
        then false
        else List.fold2 (fun b x y -> if b then f x y else false) true li1 li2
    let rec argEqual x y =
        match x, y with
        | Option genArg1, Option genArg2
        | Array genArg1, Array genArg2 ->
            argEqual genArg1 genArg2
        | Tuple genArgs1, Tuple genArgs2 ->
            listsEqual argEqual genArgs1 genArgs2
        | Function (genArgs1, typ1, isCurried1), Function (genArgs2, typ2, isCurried2) ->
            isCurried1 = isCurried2 && argEqual typ1 typ2 && listsEqual argEqual genArgs1 genArgs2
        | DeclaredType(ent1, genArgs1), DeclaredType(ent2, genArgs2) ->
            ent1 = ent2 && listsEqual argEqual genArgs1 genArgs2
        | GenericParam _, _ ->
            true
        | x, y -> x = y
    listsEqual argEqual declaredArgs appliedArgs

let addWarning (com: ICompiler) (fileName: string) (range: SourceLocation option) (warning: string) =
    com.AddLog(warning, Severity.Warning, ?range=range, fileName=fileName)

let addError (com: ICompiler) (fileName: string) (range: SourceLocation option) (warning: string) =
    com.AddLog(warning, Severity.Error, ?range=range, fileName=fileName)

let addErrorAndReturnNull (com: ICompiler) (fileName: string) (range: SourceLocation option) (error: string) =
    com.AddLog(error, Severity.Error, ?range=range, fileName=fileName)
    Value Null
