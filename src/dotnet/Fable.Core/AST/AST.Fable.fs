namespace rec Fable.AST.Fable
open Fable
open Fable.AST

(** ##Decorators *)
type Decorator =
    | Decorator of fullName: string * args: obj list
    member x.FullName = match x with Decorator (prop,_) -> prop
    member x.Arguments = match x with Decorator (_,prop) -> prop
    member x.Name = x.FullName.Substring (x.FullName.LastIndexOf '.' + 1)

(** ##Types *)
type Type =
    | MetaType
    | Any
    | Unit
    | Boolean
    | Char
    | String
    | Number of NumberKind
    | ExtendedNumber of ExtendedNumberKind
    | Option of genericArg: Type
    | Array of genericArg: Type
    | Tuple of genericArgs: Type list
    | Function of argTypes: Type list * returnType: Type * isCurried: bool
    | GenericParam of name: string
    | Enum of fullName: string
    | DeclaredType of Entity * genericArgs: Type list
    member x.FullName =
        match x with
        | Number numberKind -> sprintf "%A" x
        | Enum fullName -> fullName
        | Array typ -> typ.FullName + "[]"
        | Function (argTypes, returnType, _) ->
            "(" + (argTypes |> Seq.map (fun x -> x.FullName) |> String.concat ", ") + ")=>" + returnType.FullName
        | DeclaredType(ent,_) -> ent.FullName
        | _ -> sprintf "%A" x
    member x.GenericArgs =
        match x with
        | Array genArg -> [genArg]
        | Tuple genArgs -> genArgs
        | Function(argTypes, returnType, _) -> argTypes@[returnType]
        | DeclaredType(_, genArgs) -> genArgs
        | _ -> []

type NonDeclaredType =
    | NonDeclAny
    | NonDeclUnit
    | NonDeclOption of genericArg: Expr
    | NonDeclArray of genericArg: Expr
    | NonDeclTuple of genericArgs: Expr list
    | NonDeclFunction of genericArgs: Expr list
    | NonDeclGenericParam of name: string
    | NonDeclInterface of name: string

(** ##Entities *)
type EntityKind =
    | Module
    | Union of cases: (string*Type list) list
    | Record of fields: (string*Type) list
    | Exception of fields: (string*Type) list
    | Class of baseClass: (string*Expr) option * properties: (string*Type) list
    | Interface

type Entity(kind: Lazy<_>, file, fullName, members: Lazy<Member list>,
            ?genParams, ?interfaces, ?decorators) =
    let genParams = defaultArg genParams []
    let decorators = defaultArg decorators []
    let interfaces = defaultArg interfaces []
    member x.Kind: EntityKind = kind.Value
    member x.File: string option = file
    member x.FullName: string = fullName
    member x.Members: Member list = members.Value
    member x.GenericParameters: string list = genParams
    member x.Interfaces: string list = interfaces
    member x.Decorators: Decorator list = decorators
    member x.Name =
        x.FullName.Substring(x.FullName.LastIndexOf('.') + 1)
    member x.Namespace =
        let fullName = x.FullName
        match fullName.LastIndexOf "." with
        | -1 -> ""
        | 0 -> failwithf "Unexpected entity full name: %s" fullName
        | i -> fullName.Substring(0, i)
    member x.HasInterface (fullName: string) =
        List.contains fullName interfaces
    /// Finds decorator by name
    member x.TryGetDecorator decorator =
        decorators |> List.tryFind (fun x -> x.Name = decorator)
    /// Finds decorator by full name
    member x.TryGetFullDecorator decorator =
        decorators |> List.tryFind (fun x -> x.FullName = decorator)
    // TODO: Parent classes should be checked if the method is not found
    member x.TryGetMember(name, kind, loc, argTypes, ?argsEqual) =
        let argsEqual = defaultArg argsEqual (=)
        members.Value |> List.tryFind (fun m ->
            if m.Location <> loc
                || m.Name <> name
                || m.Kind <> kind
            then false
            elif m.OverloadIndex.IsNone
            then true
            else
                match argTypes with
                | [Unit] -> List.isEmpty m.ArgumentTypes
                | argTypes -> argsEqual m.ArgumentTypes argTypes)
    static member CreateRootModule fileName =
        Entity (lazy Module, Some fileName, "", lazy [])

    override x.ToString() = sprintf "%s %A" x.Name x.Kind

type Declaration =
    | ActionDeclaration of Expr * SourceLocation option
    /// Module members are also declared as variables, so they need
    /// a private name that doesn't conflict with enclosing scope (see #130)
    | EntityDeclaration of Entity * isPublic: bool * privateName: string * Declaration list * SourceLocation option
    | MemberDeclaration of Member * isPublic: bool * privateName: string option * args: Ident list * body: Expr * SourceLocation option
    member x.Range =
        match x with
        | ActionDeclaration (_,r) -> r
        | EntityDeclaration (_,_,_,_,r) -> r
        | MemberDeclaration (_,_,_,_,_,r) -> r

type MemberKind =
    | Constructor
    | Method
    | Getter
    | Setter
    | Field

type MemberLoc =
    | InstanceLoc
    | StaticLoc
    | InterfaceLoc of string

type Member(name, kind, loc, argTypes, returnType, ?originalType, ?genParams, ?decorators,
            ?isMutable, ?computed, ?hasRestParams, ?overloadIndex) =
    member x.Name: string = name
    member x.Kind: MemberKind = kind
    member x.Location: MemberLoc = loc
    member x.ArgumentTypes: Type list = argTypes
    member x.ReturnType: Type = returnType
    member x.OriginalCurriedType: Type option = originalType
    member x.GenericParameters: string list = defaultArg genParams []
    member x.Decorators: Decorator list = defaultArg decorators []
    member x.IsMutable: bool = defaultArg isMutable false
    member x.Computed: Expr option = computed
    member x.HasRestParams: bool = defaultArg hasRestParams false
    member x.OverloadIndex: int option = overloadIndex
    member x.OverloadName: string =
        match overloadIndex with
        | Some i -> name + "_" + (string i)
        | None -> name
    member x.TryGetDecorator decorator =
        x.Decorators |> List.tryFind (fun x -> x.Name = decorator)
    override x.ToString() = sprintf "%A %s" kind name

type ExternalEntity =
    | ImportModule of fullName: string * moduleName: string * isNs: bool
    | GlobalModule of fullName: string
    member x.FullName =
        match x with ImportModule (fullName, _, _)
                   | GlobalModule fullName -> fullName

type File(sourcePath, root, decls, ?usedVarNames, ?dependencies) =
    member x.SourcePath: string = sourcePath
    member x.Root: Entity = root
    member x.Declarations: Declaration list = decls
    member x.UsedVarNames: Set<string> = defaultArg usedVarNames Set.empty
    member x.Dependencies: Set<string> = defaultArg dependencies Set.empty
    member x.Range =
        match decls with
        | [] -> SourceLocation.Empty
        | decls ->
            decls |> Seq.choose (fun d -> d.Range) |> Seq.tryLast
            |> function Some r -> SourceLocation.Empty + r | None -> SourceLocation.Empty

type FileInfo = {
    targetFile: string
    rootModule: string
}

type FableMap = {
    coreVersion: string
    compilerVersion: string
    files: Map<string, FileInfo>
}

(** ##Expressions *)
type ApplyInfo = {
    ownerType: Type
    /// Sometimes Fable.Type may differ from original F# name (e.g. System.Object -> Fable.Any).
    /// This field keeps the original name.
    ownerFullName: string
    methodName: string
    methodKind: MemberKind
    callee: Expr option
    args: Expr list
    returnType: Type
    range: SourceLocation option
    fileName: string
    decorators: Decorator list
    /// Generic arguments applied to the callee (for instance methods)
    calleeTypeArgs: Type list
    /// Generic arguments applied to the method (not to be confused with `methodArgTypes`)
    methodTypeArgs: Type list
    /// Types of arguments as appearing in method signature (not to be confused with `methodTypeArgs`)
    methodArgTypes: Type list
    genericAvailability: bool
    caughtException: Ident option
}

type ApplyKind =
    | ApplyMeth | ApplyGet | ApplyCons

type ArrayConsKind =
    | ArrayValues of Expr list
    | ArrayAlloc of Expr

type Ident(name: string, ?typ: Type) =
    member x.Name = name
    member x.Type = defaultArg typ Any
    static member getType (i: Ident) = i.Type
    override __.ToString() = name

type LambdaInfo(captureThis: bool, ?isDelegate: bool) =
    member __.CaptureThis = captureThis
    member __.IsDelegate = defaultArg isDelegate false

type ImportKind =
    | CoreLib
    | Internal of file: string
    | CustomImport

type ValueKind =
    | Null
    | This
    | Super
    | Spread of Expr
    | TypeRef of Entity * genArgs: (string * Expr) list
    | IdentValue of Ident
    | ImportRef of memb: string * path: string * ImportKind
    | NumberConst of float * NumberKind
    | StringConst of string
    | BoolConst of bool
    | RegexConst of source: string * flags: RegexFlag list
    | ArrayConst of ArrayConsKind * Type
    | TupleConst of Expr list
    | UnaryOp of UnaryOperator
    | BinaryOp of BinaryOperator
    | LogicalOp of LogicalOperator
    | Lambda of args: Ident list * body: Expr * info: LambdaInfo
    | Emit of string
    member x.ImmediateSubExpressions: Expr list =
        match x with
        | Null | This | Super | IdentValue _ | ImportRef _
        | NumberConst _ | StringConst _ | BoolConst _ | RegexConst _
        | UnaryOp _ | BinaryOp _ | LogicalOp _ | Emit _ -> []
        | Spread x -> [x]
        | TypeRef(_,genArgs) -> genArgs |> List.map snd
        | ArrayConst(kind,_) ->
            match kind with
            | ArrayValues exprs -> exprs
            | ArrayAlloc e -> [e]
        | TupleConst exprs -> exprs
        | Lambda(_,body,_) -> [body]
    member x.Type =
        match x with
        | Null -> Any
        | Spread x -> x.Type
        | IdentValue i -> i.Type
        | This | Super | ImportRef _ | Emit _ -> Any
        | NumberConst (_,kind) -> Number kind
        | StringConst _ -> String
        | TypeRef _ -> MetaType
        | RegexConst _ ->
            let fullName = "System.Text.RegularExpressions.Regex"
            DeclaredType(Entity(lazy Class(None, []), None, fullName, lazy []), [])
        | BoolConst _ -> Boolean
        | ArrayConst (_, typ) -> Array typ
        | TupleConst exprs -> List.map Expr.getType exprs |> Tuple
        | UnaryOp _ -> Function([Any], Any, true)
        | BinaryOp _ | LogicalOp _ -> Function([Any; Any], Any, true)
        | Lambda (args, body, info) -> Function(List.map Ident.getType args, body.Type, not info.IsDelegate)
    member x.Range: SourceLocation option =
        match x with
        | Lambda (_, body, _) -> body.Range
        | _ -> None

type LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: Ident * enumerable: Expr * body: Expr

type ObjExprMember = Member * Ident list * Expr

type Expr =
    // Pure Expressions
    | Value of value: ValueKind
    | ObjExpr of decls: ObjExprMember list * interfaces: string list * baseClass: Expr option * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr * range: SourceLocation option
    | Apply of callee: Expr * args: Expr list * kind: ApplyKind * typ: Type * range: SourceLocation option
    | Quote of Expr

    // Pseudo-Statements
    | Throw of Expr * typ: Type * range: SourceLocation option
    | DebugBreak of range: SourceLocation option
    | Loop of LoopKind * range: SourceLocation option
    | VarDeclaration of var: Ident * value: Expr * isMutable: bool
    | Set of callee: Expr * property: Expr option * value: Expr * range: SourceLocation option
    | Sequential of Expr list * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option * range: SourceLocation option
    | Switch of matchValue: Expr * cases: (Expr list * Expr) list * defaultCase: Expr option * typ: Type * range: SourceLocation option

    // This wraps expressions with a different type for compile-time checkings
    // E.g. enums, ignored expressions so they don't trigger a return in functions
    | Wrapped of Expr * Type
    static member getType (expr: Expr) = expr.Type

    member x.IsJsStatement =
        match x with
        | Value _ | ObjExpr _ | Apply _ | Quote _ -> false
        | Wrapped (e,_) -> e.IsJsStatement
        | IfThenElse (_,thenExpr,elseExpr,_) -> thenExpr.IsJsStatement || elseExpr.IsJsStatement
        | Throw _ | DebugBreak _ | Loop _ | Set _ | VarDeclaration _
        | Sequential _ | TryCatch _ | Switch _ -> true

    member x.Type =
        match x with
        | Value kind -> kind.Type
        | ObjExpr _ -> Any
        | Wrapped (_,typ) | Apply (_,_,_,typ,_) | Throw (_,typ,_) -> typ
        | IfThenElse (_,thenExpr,elseExpr,_) -> thenExpr.Type
        | DebugBreak _ | Loop _ | Set _ | VarDeclaration _ -> Unit
        | Sequential (exprs,_) ->
            match exprs with
            | [] -> Unit
            | exprs -> (Seq.last exprs).Type
        | TryCatch (body,_,_,_) -> body.Type
        | Switch (_,_,_,t,_) -> t
        // TODO: Quotations must have their own type
        | Quote _ -> Any

    member x.Range: SourceLocation option =
        match x with
        | ObjExpr (_,_,_,(Some _ as r)) -> r
        | ObjExpr (decls,_,_,None) ->
            match decls |> List.choose (fun (_,_,body) -> body.Range) with
            | [] -> None
            | [r] -> Some r
            | r1::rest -> r1 + (List.last rest) |> Some
        | Value v -> v.Range
        | VarDeclaration (_,e,_) | Wrapped (e,_) | Quote e -> e.Range
        | Apply (_,_,_,_,range)
        | IfThenElse (_,_,_,range)
        | Throw (_,_,range)
        | DebugBreak range
        | Loop (_,range)
        | Set (_,_,_,range)
        | Sequential (_,range)
        | TryCatch (_,_,_,range)
        | Switch (_,_,_,_,range) -> range

    member x.ImmediateSubExpressions: Expr list =
        match x with
        | Value v -> v.ImmediateSubExpressions
        | ObjExpr (decls,_,baseClass,_) ->
            (decls |> List.map (fun (_,_,e) -> e))@(Option.toList baseClass)
        | VarDeclaration (_,e,_) -> [e]
        | Wrapped (e,_) -> [e]
        | Quote e -> [e]
        | Throw (e,_,_) -> [e]
        | Apply (callee,args,_,_,_) -> callee::args
        | IfThenElse (cond,thenExpr,elseExpr,_) -> [cond;thenExpr;elseExpr]
        | Loop (kind,_) ->
            match kind with
            | While(e1,e2) -> [e1;e2]
            | For(_,e1,e2,e3,_) -> [e1;e2;e3]
            | ForOf(_,e1,e2) -> [e1;e2]
        | Set (callee,prop,value,_) ->
            [ yield callee
              yield! Option.toList prop
              yield value ]
        | Sequential (exprs,_) -> exprs
        | TryCatch (body,catch,finalizer,_) ->
            [ yield body
              match catch with
              | Some (_,catch) -> yield catch
              | None -> ()
              yield! Option.toList finalizer ]
        | Switch (test,cases,defCase,_,_) ->
            [ yield test
              for (labels, body) in cases do
                yield! labels
                yield body
              yield! Option.toList defCase]
        | DebugBreak _ -> []

    member x.IsNull =
        match x with
        // Check also cases when null is wrapped to represent a specific type
        | Value Null | Wrapped(Value Null, _) -> true
        | _ -> false
