namespace Fable.AST.Fable
open Fable
open Fable.AST

(** ##Decorators *)
type Decorator =
    | Decorator of fullName: string * args: obj list
    member x.FullName = match x with Decorator (prop,_) -> prop
    member x.Arguments = match x with Decorator (_,prop) -> prop
    member x.Name = x.FullName.Substring (x.FullName.LastIndexOf '.' + 1)

(** ##Types *)
type PrimitiveTypeKind =
    | Unit
    | Enum of fullName: string
    | Number of NumberKind
    | String
    | Regex
    | Boolean
    | Function of arity: int
    | Array of ArrayKind

and Type =
    | UnknownType
    | DeclaredType of Entity
    | PrimitiveType of PrimitiveTypeKind
    member x.FullName =
        match x with
        | UnknownType -> "unknown"
        | DeclaredType ent -> ent.FullName
        | PrimitiveType kind -> sprintf "%A" kind

(** ##Entities *)
and EntityKind =
    | Module
    | Class of baseClass: (string*Expr) option
    | Union
    | Record
    | Exception
    | Interface

and Entity(kind, file, fullName, interfaces, decorators, isPublic) =
    member x.Kind: EntityKind = kind
    member x.File: string option = file
    member x.FullName: string = fullName
    member x.Interfaces: string list = interfaces
    member x.Decorators: Decorator list = decorators
    member x.IsPublic: bool = isPublic
    member x.Name =
        x.FullName.Substring(x.FullName.LastIndexOf('.') + 1)
    member x.Namespace =
        let fullName = x.FullName
        match fullName.LastIndexOf "." with
        | -1 -> ""
        | 0 -> failwithf "Unexpected entity full name: %s" fullName
        | _ as i -> fullName.Substring(0, i)
    member x.HasInterface (fullName: string) =
        List.exists ((=) fullName) interfaces
    member x.TryGetDecorator decorator =
        decorators |> List.tryFind (fun x -> x.Name = decorator)
    static member CreateRootModule fileName modFullName =
        Entity (Module, Some fileName, modFullName, [], [], true)
    override x.ToString() = sprintf "%s %A" x.Name kind

and Declaration =
    | ActionDeclaration of Expr * SourceLocation
    | EntityDeclaration of Entity * privateName: string * Declaration list * SourceLocation
    | MemberDeclaration of Member
    member x.Range =
        match x with
        | ActionDeclaration (_,r) -> r
        | EntityDeclaration (_,_,_,r) -> r
        | MemberDeclaration m -> m.Range

and MemberKind =
    | Constructor
    | Method of name: string
    | Getter of name: string * isField: bool
    | Setter of name: string

and Member(kind, range, args, body, ?decorators, ?isPublic, ?isMutable, ?isStatic, ?hasRestParams, ?privateName) =
    member x.Kind: MemberKind = kind
    member x.Range: SourceLocation = range
    member x.Arguments: Ident list = args
    member x.Body: Expr = body
    member x.Decorators: Decorator list = defaultArg decorators []
    member x.IsPublic: bool = defaultArg isPublic true
    member x.IsMutable: bool = defaultArg isMutable false
    member x.IsStatic: bool = defaultArg isStatic false
    member x.HasRestParams: bool = defaultArg hasRestParams false
    /// Module members are also declared as variables, so they need
    /// a private name that doesn't conflict with enclosing scope (see #130)
    member x.PrivateName: string option = privateName
    member x.TryGetDecorator decorator =
        x.Decorators |> List.tryFind (fun x -> x.Name = decorator)
    override x.ToString() = sprintf "%A" kind

and ExternalEntity =
    | ImportModule of fullName: string * moduleName: string * isNs: bool
    | GlobalModule of fullName: string
    member x.FullName =
        match x with ImportModule (fullName, _, _)
                   | GlobalModule fullName -> fullName

and File(fileName, root, decls, ?logs) =
    member x.FileName: string = fileName
    member x.Root: Entity = root
    member x.Declarations: Declaration list = decls
    member x.Logs: LogMessage list = defaultArg logs []
    member x.Range =
        match decls with
        | [] -> SourceLocation.Empty
        | decls -> SourceLocation.Empty + (List.last decls).Range

and Project(projectFile, fileMap, ?assemblyFile, ?importPath) =
    member __.ProjectFileName: string = projectFile
    member __.FileMap: Map<string, string> = fileMap
    member __.AssemblyFileName: string option = assemblyFile
    member __.ImportPath: string option = importPath
    member __.Name: string = System.IO.Path.GetFileNameWithoutExtension projectFile

(** ##Expressions *)
and ArrayKind = TypedArray of NumberKind | DynamicArray | Tuple

and ApplyInfo = {
        methodName: string
        ownerFullName: string
        methodKind: MemberKind
        callee: Expr option
        args: Expr list
        returnType: Type
        range: SourceLocation option
        decorators: Decorator list
        calleeTypeArgs: Type list
        methodTypeArgs: Type list
        /// If the method accepts a lambda as first argument, indicates its arity 
        lambdaArgArity: int
    }

and ApplyKind =
    | ApplyMeth | ApplyGet | ApplyCons

and ArrayConsKind =
    | ArrayValues of Expr list
    | ArrayAlloc of Expr

and Ident = { name: string; typ: Type }

and ValueKind =
    | Null
    | This
    | Super
    | Spread of Expr
    | TypeRef of Entity
    | IdentValue of Ident
    | ImportRef of memb: string * path: string
    | NumberConst of U2<int,float> * NumberKind
    | StringConst of string
    | BoolConst of bool
    | RegexConst of source: string * flags: RegexFlag list
    | ArrayConst of ArrayConsKind * kind: ArrayKind
    | UnaryOp of UnaryOperator
    | BinaryOp of BinaryOperator
    | LogicalOp of LogicalOperator
    | Lambda of args: Ident list * body: Expr
    | Emit of string
    member x.Type =
        match x with
        | Null -> UnknownType
        | Spread x -> x.Type
        | IdentValue {typ=typ} -> typ
        | This | Super | ImportRef _ | TypeRef _ | Emit _ -> UnknownType
        | NumberConst (_,kind) -> PrimitiveType (Number kind)
        | StringConst _ -> PrimitiveType String
        | RegexConst _ -> PrimitiveType Regex
        | BoolConst _ -> PrimitiveType Boolean
        | ArrayConst (_,kind) -> PrimitiveType (Array kind)
        | UnaryOp _ -> PrimitiveType (Function 1)
        | BinaryOp _ | LogicalOp _ -> PrimitiveType (Function 2)
        | Lambda (args, _) -> PrimitiveType (Function args.Length)
    member x.Range: SourceLocation option =
        match x with
        | Lambda (_, body) -> body.Range
        | _ -> None

and LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: Ident * enumerable: Expr * body: Expr

and Expr =
    // Pure Expressions
    | Value of value: ValueKind
    | ObjExpr of members: Member list * interfaces: string list * baseClass: Expr option * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr * range: SourceLocation option
    | Apply of callee: Expr * args: Expr list * kind: ApplyKind * typ: Type * range: SourceLocation option
    | Quote of Expr

    // Pseudo-Statements
    | Throw of Expr * range: SourceLocation option
    | Loop of LoopKind * range: SourceLocation option
    | VarDeclaration of var: Ident * value: Expr * isMutable: bool
    | Set of callee: Expr * property: Expr option * value: Expr * range: SourceLocation option
    | Sequential of Expr list * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option * range: SourceLocation option

    // This wraps expressions with a different type for compile-time checkings
    // E.g. enums, ignored expressions so they don't trigger a return in functions
    | Wrapped of Expr * Type

    member x.Type =
        match x with
        | Value kind -> kind.Type
        | ObjExpr _ -> UnknownType
        | Wrapped (_,typ) | Apply (_,_,_,typ,_) -> typ
        | IfThenElse (_,thenExpr,_,_) -> thenExpr.Type
        | Throw _ | Loop _ | Set _ | VarDeclaration _ -> PrimitiveType Unit
        | Sequential (exprs,_) ->
            match exprs with
            | [] -> PrimitiveType Unit
            | exprs -> (Seq.last exprs).Type
        | TryCatch (body,_,_,_) -> body.Type
        // TODO: Quotations must have their own primitive? type
        | Quote _ -> UnknownType

    member x.Range: SourceLocation option =
        match x with
        | Value v -> v.Range
        | VarDeclaration (_,e,_) | Wrapped (e,_) | Quote e -> e.Range
        | ObjExpr (_,_,_,range)
        | Apply (_,_,_,_,range)
        | IfThenElse (_,_,_,range)
        | Throw (_,range)
        | Loop (_,range)
        | Set (_,_,_,range)
        | Sequential (_,range)
        | TryCatch (_,_,_,range) -> range

    // member x.Children: Expr list =
    //     match x with
    //     | Value _ -> []
    //     | ObjExpr (decls,_) -> decls |> List.map snd
    //     | Get (callee,prop,_) -> [callee; prop]
    //     | Emit (_,args,_,_) -> args
    //     | Apply (callee,args,_,_,_) -> (callee::args)
    //     | IfThenElse (guardExpr,thenExpr,elseExpr,_) -> [guardExpr; thenExpr; elseExpr]
    //     | Throw (ex,_) -> [ex]
    //     | Loop (kind,_) ->
    //         match kind with
    //         | While (guard,body) -> [guard; body]
    //         | For (_,start,limit,body,_) -> [start; limit; body]
    //         | ForOf (_,enumerable,body) -> [enumerable; body]
    //     | Set (callee,prop,value,_) ->
    //         match prop with
    //         | Some prop -> [callee; prop; value]
    //         | None -> [callee; value]
    //     | VarDeclaration (_,value,_) -> [value]
    //     | Sequential (exprs,_) -> exprs
    //     | Wrapped (e,_) -> [e]
    //     | TryCatch (body,catch,finalizer,_) ->
    //         match catch, finalizer with
    //         | Some (_,catch), Some finalizer -> [body; catch; finalizer]
    //         | Some (_,catch), None -> [body; catch]
    //         | None, Some finalizer -> [body; finalizer]
    //         | None, None -> [body]

module Util =
    open Fable
    
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
    let makeCoreRef (com: ICompiler) modname = Value (ImportRef (modname, com.Options.coreLib))
    let makeIdent name: Ident = {name=name; typ=UnknownType}
    let makeIdentExpr name = makeIdent name |> IdentValue |> Value

    let makeBinOp, makeUnOp, makeLogOp, makeEqOp =
        let makeOp range typ args op =
            Apply (Value op, args, ApplyMeth, typ, range)
        (fun range typ args op -> makeOp range typ args (BinaryOp op)),
        (fun range typ args op -> makeOp range typ args (UnaryOp op)),
        (fun range args op -> makeOp range (PrimitiveType Boolean) args (LogicalOp op)),
        (fun range args op -> makeOp range (PrimitiveType Boolean) args (BinaryOp op))

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

    let makeFnType args =
        PrimitiveType (List.length args |> Function)

    let makeGet range typ callee propExpr =
        Apply (callee, [propExpr], ApplyGet, typ, range)

    let makeArray elementType arrExprs =
        let arrayKind =
            match elementType with
            | PrimitiveType (Number numberKind) -> TypedArray numberKind
            | _ -> DynamicArray
        ArrayConst(ArrayValues arrExprs, arrayKind) |> Value

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
        | PrimitiveType _ ->
            "Cannot reference a primitive type"
            |> attachRange range |> failwith
        | UnknownType ->
            "Cannot reference unknown type. "
            + "If this a generic argument, try to make function inline."
            |> attachRange range |> failwith
        | DeclaredType ent ->
            match tryImported com ent.Name ent.Decorators with
            | Some expr -> expr
            | None -> Value (TypeRef ent)

    let makeCall com range typ kind =
        let getCallee meth args owner =
            match meth with
            | None -> owner
            | Some meth ->
                let fnTyp = PrimitiveType (List.length args |> Function)
                Apply (owner, [makeConst meth], ApplyGet, fnTyp, None)
        let apply kind args callee =
            Apply(callee, args, kind, typ, range)
        let getKind isCons =
            if isCons then ApplyCons else ApplyMeth
        match kind with
        | InstanceCall (callee, meth, args) ->
            let fnTyp = PrimitiveType (List.length args |> Function)
            Apply (callee, [makeConst meth], ApplyGet, fnTyp, None)
            |> apply ApplyMeth args
        | ImportCall (importPath, modName, meth, isCons, args) ->
            Value (ImportRef (modName, importPath))
            |> getCallee meth args
            |> apply (getKind isCons) args
        | CoreLibCall (modName, meth, isCons, args) ->
            makeCoreRef com modName
            |> getCallee meth args
            |> apply (getKind isCons) args
        | GlobalCall (modName, meth, isCons, args) ->
            makeIdentExpr modName
            |> getCallee meth args
            |> apply (getKind isCons) args

    let makeTypeTest com range (typ: Type) expr =
        let stringType, boolType =
            PrimitiveType String, PrimitiveType Boolean
        let checkType (primitiveType: string) expr =
            let typof = makeUnOp None stringType [expr] UnaryTypeof
            makeBinOp range boolType [typof; makeConst primitiveType] BinaryEqualStrict
        match typ with
        | PrimitiveType kind ->
            match kind with
            | String _ -> checkType "string" expr
            | Number _ -> checkType "number" expr
            | Boolean -> checkType "boolean" expr
            | Unit -> makeBinOp range boolType [expr; Value Null] BinaryEqual
            | Function _ -> checkType "function" expr
            // TODO: Regex and Array?
            | _ -> failwithf "Unsupported type test: %A" typ
        | DeclaredType typEnt ->
            match typEnt.Kind with
            | Interface ->
                CoreLibCall ("Util", Some "hasInterface", false, [expr; makeConst typEnt.FullName])
                |> makeCall com range boolType
            | _ ->
                makeBinOp range boolType [expr; makeTypeRef com range typ] BinaryInstanceOf
        | _ -> "Unsupported type test: " + typ.FullName
               |> attachRange range |> failwith

    let makeUnionCons () =
        let emit = Emit "this.Case=arguments[0]; this.Fields = []; for (var i=1; i<arguments.length; i++) { this.Fields[(i-1)]=arguments[i]; }" |> Value
        let body = Apply (emit, [], ApplyMeth, PrimitiveType Unit, None)
        Member(Constructor, SourceLocation.Empty, [], body, [], true)
        |> MemberDeclaration

    let makeExceptionCons () =
        let emit = Emit "for (var i=0; i<arguments.length; i++) { this['data'+i]=arguments[i]; }" |> Value
        let body = Apply (emit, [], ApplyMeth, PrimitiveType Unit, None)
        Member(Constructor, SourceLocation.Empty, [], body, [], true)
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
        let body = Apply (Value (Emit body), [], ApplyMeth, PrimitiveType Unit, None)
        Member(Constructor, SourceLocation.Empty, args, body, [], true, false, false)
        |> MemberDeclaration

    let makeDelegate arity (expr: Expr) =
        let rec flattenLambda (arity: int option) accArgs = function
            | Value (Lambda (args, body)) when arity.IsNone || List.length accArgs < arity.Value ->
                flattenLambda arity (accArgs@args) body
            | _ as body ->
                Value (Lambda (accArgs, body))
        match expr, expr.Type with
        | Value (Lambda (args, body)), _ ->
            flattenLambda arity args body
        | _, PrimitiveType (Function a) ->
            let arity = defaultArg arity a
            if arity > 1 then
                let lambdaArgs =
                    [for i=1 to arity do
                        yield {name=Naming.getUniqueVar(); typ=UnknownType}]
                let lambdaBody =
                    (expr, lambdaArgs)
                    ||> List.fold (fun callee arg ->
                        Apply (callee, [Value (IdentValue arg)],
                            ApplyMeth, UnknownType, expr.Range))
                Lambda (lambdaArgs, lambdaBody) |> Value
            else
                expr // Do nothing
        | _ -> expr

    // Check if we're applying against a F# let binding
    let makeApply range typ callee exprs =
        let lasti = (List.length exprs) - 1
        ((0, callee), exprs)
        ||> List.fold (fun (i, callee) expr ->
            let typ = if i = lasti then typ else PrimitiveType (Function <|i+1)
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
            Member(Getter (name, true), range, [], body))
        ObjExpr(members, [], None, Some range)
        
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
