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
    // TODO: Regex
    | :? unit | _ when value = null -> Null
    | _ -> failwithf "Unexpected literal %O" value
    |> Value

let makeFnType args (body: Expr) =
    Function(List.map (Ident.getType >> List.singleton) args, body.Type)

let makeUnknownFnType (arity: int) =
    Function(List.init arity (fun _ -> [Any]), Any)

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
    | GenericParam name ->
        "Cannot reference generic parameter " + name
        + ". Try to make function inline."
        |> attachRange range |> failwith
    | _ ->
        // TODO: Reference JS objects? Object, String, Number...
        sprintf "Cannot reference type %s" typ.FullName
        |> attachRange range |> failwith

let makeCall com range typ kind =
    let getCallee meth args returnType owner =
        match meth with
        | None -> owner
        | Some meth ->
            let fnTyp = Function(List.map (Expr.getType >> List.singleton) args, returnType)
            Apply (owner, [makeConst meth], ApplyGet, fnTyp, None)
    let apply kind args callee =
        Apply(callee, args, kind, typ, range)
    let getKind isCons =
        if isCons then ApplyCons else ApplyMeth
    match kind with
    | InstanceCall (callee, meth, args) ->
        let fnTyp = Function(List.map (Expr.getType >> List.singleton) args, typ)
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
        |> fun body -> Apply (Value (Emit body), [], ApplyMeth, Unit, None)
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

let makeLambdaValue args body =
    Value(Lambda(List.map List.singleton args, body))

// Check if we're applying against a F# let binding
let rec makeApply com range typ callee (args: Fable.Expr list) =
    let callee =
        match callee with
        // F# let binding: Surround with a lambda
        | Sequential _ ->
            Apply(Value(Lambda([],callee)), [], ApplyMeth, callee.Type, callee.Range)
        | _ -> callee
    match callee.Type with
    | Function(argGroups, _) ->
        if argGroups.Length <= args.Length then
            List.zip argGroups (List.take argGroups.Length args)
            |> List.map (fun (argTypeGroup, arg) ->
                if argTypeGroup.Length > 1 then
                    match arg with
                    | Value(TupleConst args) -> args, []
                    | _ ->
                        // Destruct the tuple
                        let args, decls =
                            argTypeGroup |> List.mapi (fun i _ ->
                                let var = Naming.getUniqueVar() |> makeIdent
                                Value(IdentValue var),
                                VarDeclaration(var, Apply(arg, [makeConst i], ApplyGet, Any, None), false))
                            |> List.unzip
                        args, decls
                else [arg], [])
            |> List.unzip
            |> fun (args', destruct) ->
                let destruct = List.concat destruct
                let args = (List.concat args')@(List.skip argGroups.Length args)
                let apply = Apply(callee, args, ApplyMeth, typ, range)
                if destruct.Length > 0
                then Sequential(destruct@[apply], range)
                else apply
        else // argGroups.Length > args.Length
            List.skip args.Length argGroups
            |> List.map (fun ts ->
                ts |> List.map (fun t -> {name=Naming.getUniqueVar(); typ=t}))
            |> fun argGroups2 ->
                let args2 = argGroups2 |> List.map (fun argGroup ->
                    match List.map (IdentValue >> Value) argGroup with
                    | [x] -> x
                    | xs -> TupleConst xs |> Value)
                Lambda(argGroups2, makeApply com range typ callee (args@args2)) |> Value
    | _ ->
        Apply(callee, args, ApplyMeth, typ, range)
        // let lasti = (List.length args) - 1
        // ((0, callee), args) ||> List.fold (fun (i, callee) expr ->
        //     let typ = if i = lasti then typ else makeUnknownFnType (i+1)
        //     i + 1, Apply (callee, [expr], ApplyMeth, typ, range))
        // |> snd

let makeJsObject range (props: (string * Expr) list) =
    let decls = props |> List.map (fun (name, body) ->
        MemberDeclaration(Member(name, Field, [], body.Type), None, [], body, range))
    ObjExpr(decls, [], None, Some range)

let makeEmit args macro =
    Apply(Value(Emit macro), args, ApplyMeth, Any, None) 

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
