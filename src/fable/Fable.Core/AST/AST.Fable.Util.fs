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
let makeLambdaExpr args body = Value(Lambda(args, body, true))

let makeCoreRef modname prop =
    Value(ImportRef(defaultArg prop "default", modname, CoreLib))

let makeImport (selector: string) (path: string) =
    Value(ImportRef(selector.Trim(), path.Trim(), CustomImport))

let makeBinOp, makeUnOp, makeLogOp, makeEqOp =
    let makeOp range typ args op =
        Apply (Value op, args, ApplyMeth, typ, range)
    (fun range typ args op -> makeOp range typ args (BinaryOp op)),
    (fun range typ args op -> makeOp range typ args (UnaryOp op)),
    (fun range args op -> makeOp range Boolean args (LogicalOp op)),
    (fun range args op -> makeOp range Boolean args (BinaryOp op))

let rec makeSequential range statements =
    match statements with
    | [] -> Value Null
    | [expr] -> expr
    | first::rest ->
        match first, rest with
        | Value Null, _ -> makeSequential range rest
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
        | Number Decimal, (:? decimal as x) -> NumberConst (float x, Float64)
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

let makeFnType args (body: Expr) =
    Function(List.map Ident.getType args, body.Type)

let makeUnknownFnType (arity: int) =
    Function(List.init arity (fun _ -> Any), Any)

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
    | UInt8 -> if com.Options.clamp then "Uint8ClampedArray" else "Uint8Array"
    | Int16 -> "Int16Array"
    | UInt16 -> "Uint16Array"
    | Int32 -> "Int32Array"
    | UInt32 -> "Uint32Array"
    | Float32 -> "Float32Array"
    | Float64 -> "Float64Array"
    | Decimal -> "Float64Array"

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
        | BigInt -> makeCoreRef "BigInt" None
    | Function _ -> str "function"
    | MetaType | Any -> makeNonDeclaredTypeRef NonDeclAny
    | Unit -> makeNonDeclaredTypeRef NonDeclUnit
    | Array (Number kind) when not com.Options.noTypedArrays ->
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

and makeCall (range: SourceLocation option) typ kind =
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

let makeUnionCons () =
    let args = [Ident("caseName", String); Ident("fields", Array Any)]
    let argTypes = List.map Ident.getType args
    let emit = Emit "this.Case=caseName; this.Fields = fields;" |> Value
    let body = Apply (emit, [], ApplyMeth, Unit, None)
    MemberDeclaration(Member(".ctor", Constructor, InstanceLoc, argTypes, Any), None, args, body, None)

// This is necessary when extending built-in JS types and compiling to ES5
// See https://github.com/Microsoft/TypeScript/wiki/Breaking-Changes#extending-built-ins-like-error-array-and-map-may-no-longer-work
let setProto com (ent: Entity) =
    let meth = makeUntypedGet (makeIdentExpr "Object") "setPrototypeOf"
    Apply(meth, [This |> Value; makeUntypedGet (Fable.DeclaredType(ent, []) |> makeNonGenTypeRef com) "prototype"], ApplyMeth, Any, None)

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
            Set(Value This, Some(makeStrConst propName), makeIdentExpr arg.Name, None))
        |> fun setters ->
            match ent.Kind with
            | Fable.Exception _ ->
                let superCall = Apply(Value Super, [], ApplyMeth, Any, None)
                Sequential(superCall::(setProto com ent)::setters, None)
            | _ -> Sequential(setters, None)
    MemberDeclaration(Member(".ctor", Constructor, InstanceLoc, List.map Ident.getType args, Any), None, args, body, None)

let private makeMeth argType returnType name coreMeth =
    let arg = Ident("other", argType)
    let body =
        CoreLibCall("Util", Some coreMeth, false, [Value This; Value(IdentValue arg)])
        |> makeCall None returnType
    MemberDeclaration(Member(name, Method, InstanceLoc, [arg.Type], returnType), None, [arg], body, None)

let makeUnionEqualMethod argType = makeMeth argType Boolean "Equals" "equalsUnions"
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
    MemberDeclaration(m, None, args, body, None)

let makeReflectionMethodArgsAndBody com (ent: Fable.Entity option) extend nullable interfaces cases properties =
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
            cases |> Seq.map (fun (kv: System.Collections.Generic.KeyValuePair<_,_>) ->
                let typs = kv.Value |> List.map (makeTypeRef com genInfo)
                let typs = Fable.ArrayConst(Fable.ArrayValues typs, Any) |> Fable.Value
                Member(kv.Key, Field, InstanceLoc, [], Any), [], typs)
            |> fun decls -> ["cases", ObjExpr(Seq.toList decls, [], None, None)]
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

let makeReflectionMethod com (ent: Fable.Entity option) extend nullable interfaces cases properties =
    let m, args, body = makeReflectionMethodArgsAndBody com ent extend nullable interfaces cases properties
    MemberDeclaration(m, None, args, body, None)

let makeDelegate (com: ICompiler) arity (expr: Expr) =
    let rec flattenLambda (arity: int option) isArrow accArgs = function
        | Value (Lambda (args, body, isArrow')) when arity.IsNone || List.length accArgs < arity.Value ->
            flattenLambda arity (isArrow && isArrow') (accArgs@args) body
        | _ when arity.IsSome && List.length accArgs < arity.Value ->
            None
        | body ->
            Value (Lambda (accArgs, body, isArrow)) |> Some
    let wrap arity isArrow expr =
        match arity with
        | Some arity when arity > 1 ->
            let lambdaArgs =
                [for i=1 to arity do yield Ident(com.GetUniqueVar(), Any)]
            let lambdaBody =
                (expr, lambdaArgs)
                ||> List.fold (fun callee arg ->
                    Apply (callee, [Value (IdentValue arg)],
                        ApplyMeth, Any, expr.Range))
            Lambda (lambdaArgs, lambdaBody, isArrow) |> Value
        | _ -> expr // Do nothing
    match expr, expr.Type, arity with
    | Value (Lambda (args, body, isArrow)), _, _ ->
        match flattenLambda arity isArrow args body with
        | Some expr -> expr
        | None -> wrap arity isArrow expr
    | _, Function _, Some arity ->
        wrap (Some arity) false expr
    | _ -> expr

/// Checks if an F# let binding is being applied and
/// applies arguments as for a curried function
let makeApply range typ callee exprs =
    let callee =
        match callee with
        // If we're applying against a F# let binding, wrap it with a lambda
        | Sequential _ ->
            Apply(Value(Lambda([],callee,true)), [], ApplyMeth, callee.Type, callee.Range)
        | _ -> callee
    let lasti = (List.length exprs) - 1
    ((0, callee), exprs) ||> List.fold (fun (i, callee) expr ->
        let typ' = if i = lasti then typ else Any // makeUnknownFnType (i+1)
        i + 1, Apply (callee, [expr], ApplyMeth, typ', range))
    |> snd

/// Helper when we need to compare the types of the arguments applied to a method
/// (concrete) with the declared argument types for that method (may be generic)
/// (e.g. when resolving a TraitCall)
let compareConcreteAndGenericTypes appliedArgs declaredArgs =
    let listsEqual f li1 li2 =
        if List.length li1 <> List.length li2
        then false
        else List.fold2 (fun b x y -> if b then f x y else false) true li1 li2
    let genArgs = System.Collections.Generic.Dictionary<string, Type>()
    let rec argEqual x y =
        match x, y with
        | Option genArg1, Option genArg2
        | Array genArg1, Array genArg2 ->
            argEqual genArg1 genArg2
        | Tuple genArgs1, Tuple genArgs2 ->
            listsEqual argEqual genArgs1 genArgs2
        | Function (genArgs1, typ1), Function (genArgs2, typ2) ->
            argEqual typ1 typ2 && listsEqual argEqual genArgs1 genArgs2
        | DeclaredType(ent1, genArgs1), DeclaredType(ent2, genArgs2) ->
            ent1 = ent2 && listsEqual argEqual genArgs1 genArgs2
        | GenericParam name1, GenericParam name2 ->
            name1 = name2
        | x, GenericParam name ->
            if genArgs.ContainsKey name
            then genArgs.[name] = x
            else genArgs.Add(name, x); true
        | x, y -> x = y
    listsEqual argEqual appliedArgs declaredArgs

let addWarning (com: ICompiler) (file: string) (range: SourceLocation option) (warning: string) =
    let range =
        match range with
        | Some r -> sprintf "%i, %i" r.start.line r.start.column
        | None -> "1"
    sprintf "%s(%s) : warning FABLE: %s" file range warning
    |> Warning |> com.AddLog
