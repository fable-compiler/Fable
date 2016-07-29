module Fable.AST.Fable.Util
open Fable
open Fable.AST

let attachRange (range: SourceLocation option) msg =
    match range with
    | Some range -> msg + " " + (string range)
    | None -> msg

type CallKind =
    | InstanceCall of callee: Expr * meth: string * args: Expr list
    | ImportCall of importPath: string * modName: string * meth: string option * isCons: bool * args: Expr list
    | CoreLibCall of modName: string * meth: string option * isCons: bool * args: Expr list
    | GlobalCall of modName: string * meth: string option * isCons: bool * args: Expr list

let makeLoop range loopKind = Loop (loopKind, range)
let makeIdent name: Ident = {name=name; typ=Any}
let makeTypedIdent name typ: Ident = {name=name; typ=typ}
let makeIdentExpr name = makeIdent name |> IdentValue |> Value
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

let makeTypeRef com (range: SourceLocation option) typ =
    match typ with
    | DeclaredType(ent, _) ->
        match tryImported com ent.Name ent.Decorators with
        | Some expr -> expr
        | None -> Value (TypeRef ent)
    | Any ->
        "Cannot reference unknown type. "
        + "If this a generic argument, try to make function inline."
        |> attachRange range |> failwith
    | _ ->
        sprintf "Cannot reference type %s" typ.FullName
        |> attachRange range |> failwith

let makeCall com range typ kind =
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

let makeTypeTest com range (typ: Type) expr =
    let checkType (primitiveType: string) expr =
        let typof = makeUnOp None String [expr] UnaryTypeof
        makeBinOp range Boolean [typof; makeConst primitiveType] BinaryEqualStrict
    match typ with
    | String _ -> checkType "string" expr
    | Number _ -> checkType "number" expr
    | Boolean -> checkType "boolean" expr
    | Unit -> makeBinOp range Boolean [expr; Value Null] BinaryEqual
    | Function _ -> checkType "function" expr
    // TODO: Regex and Array?
    | DeclaredType(typEnt, _) ->
        match typEnt.Kind with
        | Interface ->
            CoreLibCall ("Util", Some "hasInterface", false, [expr; makeConst typEnt.FullName])
            |> makeCall com range Boolean
        | _ ->
            makeBinOp range Boolean [expr; makeTypeRef com range typ] BinaryInstanceOf
    | _ -> "Unsupported type test: " + typ.FullName
            |> attachRange range |> failwith

let makeUnionCons () =
    let emit = Emit "this.Case=caseName; this.Fields = fields;" |> Value
    let body = Apply (emit, [], ApplyMeth, Unit, None)
    Member(".ctor", Constructor, SourceLocation.Empty, [makeIdent "caseName"; makeIdent "fields"], body, [], true)
    |> MemberDeclaration

let makeRecordCons props =
    let sanitizeField x =
        if Naming.identForbiddenCharsRegex.IsMatch x
        then "['" + (x.Replace("'", "\\'")) + "']"
        else "." + x
    let args, body =
        props |> List.mapi (fun i _ -> sprintf "$arg%i" i |> makeIdent),
        props |> Seq.mapi (fun i x ->
            sprintf "this%s=$arg%i" (sanitizeField x) i) |> String.concat ";"
    let body = Apply (Value (Emit body), [], ApplyMeth, Unit, None)
    Member(".ctor", Constructor, SourceLocation.Empty, args, body, [], true, false, false)
    |> MemberDeclaration

let makeUnionCompareMethods, makeRecordCompareMethods =
    let meth com typ name coreMeth =
        let arg = makeIdent "x"
        let body =
            CoreLibCall("Util", Some coreMeth, false, [Value This; Value(IdentValue arg)])
            |> makeCall com None typ
        Member(name, Method, SourceLocation.Empty, [arg], body) |> MemberDeclaration
    (fun (com: ICompiler) -> [meth com Boolean "Equals" "equalsUnions"; meth com (Number Int32) "CompareTo" "compareUnions"]),
    (fun (com: ICompiler) -> [meth com Boolean "Equals" "equalsRecords"; meth com (Number Int32) "CompareTo" "compareRecords"])

let makeDelegate arity (expr: Expr) =
    let rec flattenLambda (arity: int option) accArgs = function
        | Value (Lambda (args, body)) when arity.IsNone || List.length accArgs < arity.Value ->
            flattenLambda arity (accArgs@args) body
        | _ when arity.IsSome && List.length accArgs < arity.Value ->
            None
        | _ as body ->
            Value (Lambda (accArgs, body)) |> Some
    let wrap arity expr =
        match arity with
        | Some arity when arity > 1 ->
            let lambdaArgs =
                [for i=1 to arity do
                    yield {name=Naming.getUniqueVar(); typ=Any}]
            let lambdaBody =
                (expr, lambdaArgs)
                ||> List.fold (fun callee arg ->
                    Apply (callee, [Value (IdentValue arg)],
                        ApplyMeth, Any, expr.Range))
            Lambda (lambdaArgs, lambdaBody) |> Value
        | _ -> expr // Do nothing
    match expr, expr.Type with
    | Value (Lambda (args, body)), _ ->
        match flattenLambda arity args body with
        | Some expr -> expr
        | None -> wrap arity expr
    | _, Function(args,_) ->
        let arity = defaultArg arity (List.length args)
        wrap (Some arity) expr
    | _ -> expr

// Check if we're applying against a F# let binding
let makeApply range typ callee exprs =
    let lasti = (List.length exprs) - 1
    ((0, callee), exprs)
    ||> List.fold (fun (i, callee) expr ->
        let typ = if i = lasti then typ else makeUnknownFnType (i+1)
        let callee =
            match callee with
            | Sequential _ ->
                // F# let binding: Surround with a lambda
                Apply (Lambda ([], callee) |> Value, [], ApplyMeth, typ, range)
            | _ -> callee
        i, Apply (callee, [expr], ApplyMeth, typ, range))
    |> snd

let makeJsObject range (props: (string * Expr) list) =
    let members = props |> List.map (fun (name, body) ->
        Member(name, Field, range, [], body))
    ObjExpr(members, [], None, Some range)

let makeEmit args macro =
    let emit = Fable.Emit macro |> Fable.Value
    Fable.Apply(emit, args, Fable.ApplyMeth, Fable.Any, None)    
    
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
