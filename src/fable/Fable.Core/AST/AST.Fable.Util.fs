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

let getSymbol (com: ICompiler) name =
    let import = Value(ImportRef("Symbol", com.Options.coreLib))
    Apply (import, [Value(StringConst name)], ApplyGet, Any, None)

let makeLoop range loopKind = Loop (loopKind, range)
let makeIdent name: Ident = {name=name; typ=Any}
let makeTypedIdent name typ: Ident = {name=name; typ=typ}
let makeIdentExpr name = makeIdent name |> IdentValue |> Value
let makeLambdaExpr args body = Value(Lambda(args, body))

let makeCoreRef (com: ICompiler) modname prop =
    let import = Value(ImportRef(modname, com.Options.coreLib))
    match prop with
    | None -> import
    | Some prop -> Apply (import, [Value(StringConst prop)], ApplyGet, Any, None)

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

let makeConst (value: obj) =
    match value with
    | :? bool as x -> BoolConst x
    | :? string as x -> StringConst x
    | :? char as x -> StringConst (string x)
    // Integer types
    | :? int as x -> NumberConst (U2.Case1 x, Int32)
    | :? byte as x -> NumberConst (U2.Case1 (int x), UInt8)
    | :? sbyte as x -> NumberConst (U2.Case1 (int x), Int8)
    | :? int16 as x -> NumberConst (U2.Case1 (int x), Int16)
    | :? uint16 as x -> NumberConst (U2.Case1 (int x), UInt16)
    | :? uint32 as x -> NumberConst (U2.Case1 (int x), UInt32)
    // Float types
    | :? float as x -> NumberConst (U2.Case2 x, Float64)
    | :? int64 as x -> NumberConst (U2.Case2 (float x), Float64)
    | :? uint64 as x -> NumberConst (U2.Case2 (float x), Float64)
    | :? float32 as x -> NumberConst (U2.Case2 (float x), Float32)
    | :? decimal as x -> NumberConst (U2.Case2 (float x), Float64)
    // TODO: Regex
    | :? unit | _ when value = null -> Null
    | _ -> failwithf "Unexpected literal %O" value
    |> Value

let makeFnType args (body: Expr) =
    Function(List.map Ident.getType args, body.Type)

let makeUnknownFnType (arity: int) =
    Function(List.init arity (fun _ -> Any), Any)

let makeGet range typ callee propExpr =
    Apply (callee, [propExpr], ApplyGet, typ, range)

let makeArray elementType arrExprs =
    ArrayConst(ArrayValues arrExprs, elementType) |> Value

let tryImported com name (decs: #seq<Decorator>) =
    decs |> Seq.tryPick (fun x ->
        match x.Name with
        | "Global" ->
            makeIdent name |> IdentValue |> Value |> Some
        | "Import" ->
            match x.Arguments with
            | [(:? string as memb);(:? string as path)] ->
                ImportRef(memb, path) |> Value |> Some
            | _ -> failwith "Import attributes must contain two string arguments"
        | _ -> None)

let makeJsObject range (props: (string * Expr) list) =
    let decls = props |> List.map (fun (name, body) ->
        MemberDeclaration(Member(name, Field, [], body.Type), None, [], body, range))
    ObjExpr(decls, [], None, Some range)

let rec makeTypeRef (com: ICompiler) (range: SourceLocation option) generics typ =
    let str s = Value(StringConst s)
    let makeInfo (kind: TypeKind) arg =
        let kind = Value(NumberConst(U2.Case1(int kind), Int32))
        match arg with
        | None -> [kind]
        | Some arg -> [kind; arg]
        |> makeArray Any
    let makeGenInfo (kind: TypeKind) genArgs =
        match genArgs with
        | [genArg] -> makeTypeRef com range generics genArg
        | genArgs ->
            let genArgs = List.map (makeTypeRef com range generics) genArgs
            ArrayConst(ArrayValues genArgs, Any) |> Value
        |> Some |> makeInfo kind
    match typ with
    | Boolean -> str "boolean"
    | String -> str "string"
    | Number _ | Enum _ -> str "number"
    | Function _ -> str "function"
    | Any -> makeInfo TypeKind.Any None
    | Unit -> makeInfo TypeKind.Unit None
    | Array genArg -> makeGenInfo TypeKind.Array [genArg]
    | Option genArg -> makeGenInfo TypeKind.Option [genArg]
    | Tuple genArgs -> makeGenInfo TypeKind.Tuple genArgs
    | GenericParam name ->
        str name |> Some |> makeInfo TypeKind.GenericParam
    | DeclaredType(ent, _) when ent.Kind = Interface ->
        str ent.FullName |> Some |> makeInfo TypeKind.Interface
    | DeclaredType(ent, genArgs) ->
        // Imported types come from JS so they don't need to be made generic
        match tryImported com ent.Name ent.Decorators with
        | Some expr -> expr
        | None when not generics || List.isEmpty genArgs ->
            Value (TypeRef ent)
        | None ->
            let genArgs =
                List.map (makeTypeRef com range generics) genArgs
                |> List.zip ent.GenericParameters
                |> makeJsObject SourceLocation.Empty
            CoreLibCall("Util", Some "makeGeneric", false, [Value (TypeRef ent); genArgs])
            |> makeCall com range Any

and makeCall com range typ kind =
    let getCallee meth args returnType owner =
        match meth with
        | None -> owner
        | Some meth ->
            let fnTyp = Function(List.map Expr.getType args, returnType)
            Apply (owner, [makeConst meth], ApplyGet, fnTyp, None)
    let apply kind args callee =
        Apply(callee, args, kind, typ, range)
    let getKind isCons =
        if isCons then ApplyCons else ApplyMeth
    match kind with
    | InstanceCall (callee, meth, args) ->
        let fnTyp = Function(List.map Expr.getType args, typ)
        Apply (callee, [makeConst meth], ApplyGet, fnTyp, None)
        |> apply ApplyMeth args
    | ImportCall (importPath, modName, meth, isCons, args) ->
        Value (ImportRef (modName, importPath))
        |> getCallee meth args typ
        |> apply (getKind isCons) args
    | CoreLibCall (modName, meth, isCons, args) ->
        makeCoreRef com modName None
        |> getCallee meth args typ
        |> apply (getKind isCons) args
    | GlobalCall (modName, meth, isCons, args) ->
        makeIdentExpr modName
        |> getCallee meth args typ
        |> apply (getKind isCons) args

let makeEmit r t args macro =
    Apply(Value(Emit macro), args, ApplyMeth, t, r) 

let rec makeTypeTest com range (typ: Type) expr =
    let jsTypeof (primitiveType: string) expr =
        let typof = makeUnOp None String [expr] UnaryTypeof
        makeBinOp range Boolean [typof; makeConst primitiveType] BinaryEqualStrict
    let jsInstanceOf (typeRef: Expr) expr =
        makeBinOp None Boolean [expr; typeRef] BinaryInstanceOf
    match typ with
    | String _ -> jsTypeof "string" expr
    | Number _ | Enum _ -> jsTypeof "number" expr
    | Boolean -> jsTypeof "boolean" expr
    | Unit -> makeBinOp range Boolean [expr; Value Null] BinaryEqual
    | Function _ -> jsTypeof "function" expr
    | Array _ | Tuple _ ->
        "Array.isArray($0) || ArrayBuffer.isView($0)"
        |> makeEmit range Boolean [expr] 
    | Any -> makeConst true
    | Option typ -> makeTypeTest com range typ expr
    | DeclaredType(typEnt, _) ->
        match typEnt.Kind with
        | Interface ->
            CoreLibCall ("Util", Some "hasInterface", false, [expr; makeConst typEnt.FullName])
            |> makeCall com range Boolean
        | _ ->
            makeBinOp range Boolean [expr; makeTypeRef com range false typ] BinaryInstanceOf
    | GenericParam name ->
        "Cannot type test generic parameter " + name
        |> attachRange range |> failwith

let makeUnionCons () =
    let args = [{name="caseName"; typ=String}; {name="fields"; typ=Array Any}]
    let argTypes = List.map Ident.getType args
    let emit = Emit "this.Case=caseName; this.Fields = fields;" |> Value
    let body = Apply (emit, [], ApplyMeth, Unit, None)
    MemberDeclaration(Member(".ctor", Constructor, argTypes, Any), None, args, body, SourceLocation.Empty)

let makeRecordCons (props: (string*Type) list) =
    let args =
        ([], props) ||> List.fold (fun args (name, typ) ->
            let name =
                Naming.lowerFirst name |> Naming.sanitizeIdent (fun x ->
                    List.exists (fun (y: Ident) -> y.name = x) args)
            {name=name; typ=typ}::args)
        |> List.rev
    let body =
        Seq.zip args props
        |> Seq.map (fun (arg, (propName, _)) ->
            let propName =
                if Naming.identForbiddenCharsRegex.IsMatch propName
                then "['" + (propName.Replace("'", "\\'")) + "']"
                else "." + propName
            "this" + propName + "=" + arg.name)
        |> String.concat ";"
        |> fun body -> makeEmit None Unit [] body
    MemberDeclaration(Member(".ctor", Constructor, List.map Ident.getType args, Any), None, args, body, SourceLocation.Empty)

let private makeMeth com argType returnType name coreMeth =
    let arg = {name="other"; typ=argType}
    let body =
        CoreLibCall("Util", Some coreMeth, false, [Value This; Value(IdentValue arg)])
        |> makeCall com None returnType
    MemberDeclaration(Member(name, Method, [arg.typ], returnType), None, [arg], body, SourceLocation.Empty)

let makeUnionEqualMethod com argType = makeMeth com argType Boolean "Equals" "equalsUnions"
let makeRecordEqualMethod com argType = makeMeth com argType Boolean "Equals" "equalsRecords"
let makeUnionCompareMethod com argType = makeMeth com argType (Number Int32) "CompareTo" "compareUnions"
let makeRecordCompareMethod com argType = makeMeth com argType (Number Int32) "CompareTo" "compareRecords"

let makeTypeNameMeth com typeFullName =
    let typeFullName = Value(StringConst typeFullName)
    MemberDeclaration(Member("typeName", Method, [], String, isSymbol=true),
                        None, [], typeFullName, SourceLocation.Empty)

let makeInterfacesMethod com extend interfaces =
    let interfaces: Expr =
        let interfaces = List.map (StringConst >> Value) interfaces
        ArrayConst(ArrayValues interfaces, String) |> Value
    let interfaces =
        if not extend then interfaces else
        CoreLibCall("Util", Some "extendInfo", false, [interfaces; Value Super; makeConst "interfaces"])
        |> makeCall com None Any
    MemberDeclaration(Member("interfaces", Method, [], Array String, isSymbol=true),
                        None, [], interfaces, SourceLocation.Empty)

let makePropertiesMethod com extend properties =
    let body =
        properties |> List.map (fun (name, typ) ->
            MemberDeclaration(Member(name, Field, [], Any), None, [], makeTypeRef com None true typ, SourceLocation.Empty))
        |> fun decls -> ObjExpr(decls, [], None, None)
    let body =
        if not extend then body else
        CoreLibCall("Util", Some "extendInfo", false, [body; Value Super; makeConst "properties"])
        |> makeCall com None Any
    MemberDeclaration(Member("properties", Method, [], Any, isSymbol=true),
                        None, [], body, SourceLocation.Empty)

let makeCasesMethod com (cases: Map<string, Type list>) =
    let body =
        cases |> Seq.map (fun kv ->
            let typs = kv.Value |> List.map (makeTypeRef com None true)
            let typs = Fable.ArrayConst(Fable.ArrayValues typs, Any) |> Fable.Value
            MemberDeclaration(Member(kv.Key, Field, [], Any), None, [], typs, SourceLocation.Empty))
        |> fun decls -> ObjExpr(Seq.toList decls, [], None, None)
    MemberDeclaration(Member("cases", Method, [], Any, isSymbol=true), None, [], body, SourceLocation.Empty)

let makeDelegate (com: ICompiler) arity (expr: Expr) =
    let rec flattenLambda (arity: int option) accArgs = function
        | Value (Lambda (args, body)) when arity.IsNone || List.length accArgs < arity.Value ->
            flattenLambda arity (accArgs@args) body
        | _ when arity.IsSome && List.length accArgs < arity.Value ->
            None
        | body ->
            Value (Lambda (accArgs, body)) |> Some
    let wrap arity expr =
        match arity with
        | Some arity when arity > 1 ->
            let lambdaArgs =
                [for i=1 to arity do
                    yield {name=com.GetUniqueVar(); typ=Any}]
            let lambdaBody =
                (expr, lambdaArgs)
                ||> List.fold (fun callee arg ->
                    Apply (callee, [Value (IdentValue arg)],
                        ApplyMeth, Any, expr.Range))
            Lambda (lambdaArgs, lambdaBody) |> Value
        | _ -> expr // Do nothing
    match expr, expr.Type, arity with
    | Value (Lambda (args, body)), _, _ ->
        match flattenLambda arity args body with
        | Some expr -> expr
        | None -> wrap arity expr
    | _, Function(args,_), Some arity ->
        wrap (Some arity) expr
    | _ -> expr

// Check if we're applying against a F# let binding
let makeApply range typ callee exprs =
    let callee =
        match callee with
        // If we're applying against a F# let binding, wrap it with a lambda
        | Sequential _ ->
            Apply(Value(Lambda([],callee)), [], ApplyMeth, callee.Type, callee.Range)
        | _ -> callee        
    let lasti = (List.length exprs) - 1
    ((0, callee), exprs) ||> List.fold (fun (i, callee) expr ->
        let typ' = if i = lasti then typ else makeUnknownFnType (i+1)
        i + 1, Apply (callee, [expr], ApplyMeth, typ', range))
    |> snd    

let getTypedArrayName (com: ICompiler) numberKind =
    match numberKind with
    | Int8 -> "Int8Array"
    | UInt8 -> if com.Options.clamp then "Uint8ClampedArray" else "Uint8Array"
    | Int16 -> "Int16Array"
    | UInt16 -> "Uint16Array"
    | Int32 -> "Int32Array"
    | UInt32 -> "Uint32Array"
    | Int64 -> "Float64Array"
    | UInt64 -> "Float64Array"
    | Float32 -> "Float32Array"
    | Float64 -> "Float64Array"
    | Decimal -> "Float64Array"
