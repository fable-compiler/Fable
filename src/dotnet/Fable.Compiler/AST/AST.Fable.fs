namespace rec Fable.AST.Fable
open Fable
open Fable.AST

(** ##Decorators *)
type Decorator =
    | Decorator of fullName: string * args: obj list
    member this.FullName = match this with Decorator (prop,_) -> prop
    member this.Arguments = match this with Decorator (_,prop) -> prop
    member this.Name = this.FullName.Substring (this.FullName.LastIndexOf '.' + 1)

(** ##Types *)
type Type =
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
    | DeclaredType of fullName:string * genericArgs: Type list
    member this.FullName =
        match this with
        | Enum fullName -> fullName
        | Array typ -> typ.FullName + "[]"
        | Function (argTypes, returnType, _) ->
            "(" + (argTypes |> Seq.map (fun x -> x.FullName) |> String.concat ", ") + ")=>" + returnType.FullName
        | DeclaredType(fullName,_) -> fullName
        // | Number _numberKind
        | _ -> this.ToString()
    member this.GenericArgs =
        match this with
        | Array genArg -> [genArg]
        | Tuple genArgs -> genArgs
        | Function(argTypes, returnType, _) -> argTypes@[returnType]
        | DeclaredType(_, genArgs) -> genArgs
        | _ -> []

(** ##Entities *)
type EntityKind =
    | Module
    | Union of cases: (string*Type list) list
    | Record of fields: (string*Type) list
    | Exception of fields: (string*Type) list
    | Class of baseClass: string option * properties: (string*Type) list
    | Interface

type Entity(kind: EntityKind, file, fullName, members: Member list,
            ?genParams, ?interfaces, ?decorators) =
    let genParams = defaultArg genParams []
    let decorators = defaultArg decorators []
    let interfaces = defaultArg interfaces []
    member __.Kind: EntityKind = kind
    member __.File: string option = file
    member __.FullName: string = fullName
    member __.Members: Member list = members
    member __.GenericParameters: string list = genParams
    member __.Interfaces: string list = interfaces
    member __.Decorators: Decorator list = decorators
    member this.Name =
        this.FullName.Substring(this.FullName.LastIndexOf('.') + 1)
    member this.Namespace =
        let fullName = this.FullName
        match fullName.LastIndexOf "." with
        | -1 -> ""
        | 0 -> failwithf "Unexpected entity full name: %s" fullName
        | i -> fullName.Substring(0, i)
    member __.HasInterface (fullName: string) =
        List.contains fullName interfaces
    /// Checks if it has a decorator with specified name
    member __.HasDecorator(name) =
        decorators |> List.exists (fun x -> x.Name = name)

    /// Finds decorator by name
    member __.TryGetDecorator(name) =
        decorators |> List.tryFind (fun x -> x.Name = name)
    /// Finds decorator by full name
    member __.TryGetFullDecorator(fullname) =
        decorators |> List.tryFind (fun x -> x.FullName = fullname)
    member __.TryGetMember(name, isStatic, argTypes: Type list) =
        members |> List.tryFind (fun m ->
            if m.Name <> name || m.IsStatic <> isStatic
            then false
            elif Option.isNone m.OverloadIndex
            then true
            else
                match argTypes with
                | [] | [Unit] -> List.isEmpty m.ArgumentTypes
                | argTypes -> m.ArgumentTypes = argTypes)
    static member CreateRootModule fileName =
        Entity (Module, Some fileName, "", [])

type Declaration =
    | ActionDeclaration of Expr * SourceLocation option
    /// Module members are also declared as variables, so they need
    /// a private name that doesn't conflict with enclosing scope (see #130)
    | EntityDeclaration of Entity * isPublic: bool * privateName: string * Declaration list * SourceLocation option
    | FunctionDeclaration of publicName: string option * privateName: string * args: Ident list * body: Expr * SourceLocation option
    // TODO: Add Value declaration

    member this.Range =
        match this with
        | ActionDeclaration (_,r) -> r
        | EntityDeclaration (_,_,_,_,r) -> r
        | FunctionDeclaration (_,_,_,_,r) -> r

type MemberKind =
    | Constructor
    | Method
    | Getter
    | Setter
    | Field

type Member =
    { Name: string
      Kind: MemberKind
      ArgumentTypes: Type list
      ReturnType: Type
      GenericParameters: string list
      Decorators: Decorator list
      IsStatic: bool
      IsMutable: bool
      HasRestParams: bool
      OverloadIndex: int option }
    static member Create(name, kind, argTypes, returnType, ?genParams, ?decorators,
                         ?isStatic, ?isMutable, ?hasRestParams, ?overloadIndex) =
        { Name = name
          Kind = kind
          ArgumentTypes = argTypes
          ReturnType = returnType
          GenericParameters = defaultArg genParams []
          Decorators = defaultArg decorators []
          IsStatic = defaultArg isStatic false
          IsMutable = defaultArg isMutable false
          HasRestParams = defaultArg hasRestParams false
          OverloadIndex = overloadIndex }
    member this.OverloadName: string =
        match this.OverloadIndex with
        | Some i -> this.Name + "_" + (string i)
        | None -> this.Name
    /// Checks if it has a decorator with specified name
    member this.HasDecorator(name) =
        this.Decorators |> List.exists (fun x -> x.Name = name)
    /// Finds decorator by name
    member this.TryGetDecorator(name) =
        this.Decorators |> List.tryFind (fun x -> x.Name = name)
    /// Finds decorator by full name
    member this.TryGetFullDecorator(fullname) =
        this.Decorators |> List.tryFind (fun x -> x.FullName = fullname)

type File(sourcePath, root, decls, ?usedVarNames, ?dependencies) =
    member __.SourcePath: string = sourcePath
    member __.Root: Entity = root
    member __.Declarations: Declaration list = decls
    member __.UsedVarNames: Set<string> = defaultArg usedVarNames Set.empty
    member __.Dependencies: Set<string> = defaultArg dependencies Set.empty
    member __.Range =
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
    member __.Name = name
    member __.Type = defaultArg typ Any
    override __.ToString() = name
    static member getType (i: Ident) = i.Type

type ImportKind =
    | CoreLib
    | Internal of file: string
    | CustomImport

type ValueKind =
    | Null
    | This
    | Spread of Expr
    | EntityRef of string
    // TODO: IdentValue should have own range for declarations
    | IdentValue of Ident
    | ImportRef of memb: string * path: string * ImportKind
    | NumberConst of float * NumberKind
    | StringConst of string
    | BoolConst of bool
    | RegexConst of source: string * flags: RegexFlag list
    // TODO: Add ListConst
    | ArrayConst of ArrayConsKind * Type
    | TupleConst of Expr list
    | UnaryOp of UnaryOperator
    | BinaryOp of BinaryOperator
    | LogicalOp of LogicalOperator
    | Lambda of args: Ident list * body: Expr * isDelegate: bool
    | Emit of string
    member this.ImmediateSubExpressions: Expr list =
        match this with
        | Null | This | IdentValue _ | ImportRef _
        | NumberConst _ | StringConst _ | BoolConst _ | RegexConst _
        | UnaryOp _ | BinaryOp _ | LogicalOp _ | Emit _ | EntityRef _ -> []
        | Spread x -> [x]
        | ArrayConst(kind,_) ->
            match kind with
            | ArrayValues exprs -> exprs
            | ArrayAlloc e -> [e]
        | TupleConst exprs -> exprs
        | Lambda(_,body,_) -> [body]
    member this.Type =
        match this with
        | Null -> Any
        | Spread x -> x.Type
        | IdentValue i -> i.Type
        | This | ImportRef _ | Emit _ -> Any
        | NumberConst (_,kind) -> Number kind
        | StringConst _ -> String
        | EntityRef _ -> Any // TODO
        | RegexConst _ -> DeclaredType("System.Text.RegularExpressions.Regex", [])
        | BoolConst _ -> Boolean
        | ArrayConst (_, typ) -> Array typ
        | TupleConst exprs -> List.map Expr.getType exprs |> Tuple
        | UnaryOp _ -> Function([Any], Any, true)
        | BinaryOp _ | LogicalOp _ -> Function([Any; Any], Any, true)
        | Lambda (args, body, isDelegate) -> Function(List.map Ident.getType args, body.Type, not isDelegate)
    member this.Range: SourceLocation option =
        match this with
        | Lambda (_, body, _) -> body.Range
        | _ -> None

type LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: Ident * enumerable: Expr * body: Expr

type ObjExprMember = Member * Ident list * Expr

type Expr =
    // Pure Expressions
    // TODO: Add range to values
    | Value of value: ValueKind
    | ObjExpr of decls: ObjExprMember list * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr * range: SourceLocation option
    | Apply of callee: Expr * args: Expr list * kind: ApplyKind * typ: Type * range: SourceLocation option
    | Quote of Expr

    // Pseudo-Statements
    | Throw of Expr * typ: Type * range: SourceLocation option
    | DebugBreak of range: SourceLocation option
    | Loop of LoopKind * range: SourceLocation option
    | VarDeclaration of var: Ident * value: Expr * isMutable: bool * range: SourceLocation option
    | Set of callee: Expr * property: Expr option * value: Expr * range: SourceLocation option
    | Sequential of Expr list * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option * range: SourceLocation option
    | Switch of matchValue: Expr * cases: (Expr list * Expr) list * defaultCase: Expr option * typ: Type * range: SourceLocation option

    // This wraps expressions with a different type for compile-time checkings
    // E.g. enums, ignored expressions so they don't trigger a return in functions
    | Wrapped of Expr * Type
    static member getType (expr: Expr) = expr.Type

    member this.IsJsStatement =
        match this with
        | Value _ | ObjExpr _ | Apply _ | Quote _ -> false
        | Wrapped (e,_) -> e.IsJsStatement
        | IfThenElse (_,thenExpr,elseExpr,_) -> thenExpr.IsJsStatement || elseExpr.IsJsStatement
        | Throw _ | DebugBreak _ | Loop _ | Set _ | VarDeclaration _
        | Sequential _ | TryCatch _ | Switch _ -> true

    member this.Type =
        match this with
        | Value kind -> kind.Type
        | ObjExpr _ -> Any
        | Wrapped (_,typ) | Apply (_,_,_,typ,_) | Throw (_,typ,_) -> typ
        | IfThenElse (_,thenExpr,_,_) -> thenExpr.Type
        | DebugBreak _ | Loop _ | Set _ | VarDeclaration _ -> Unit
        | Sequential (exprs,_) ->
            match exprs with
            | [] -> Unit
            | exprs -> (Seq.last exprs).Type
        | TryCatch (body,_,_,_) -> body.Type
        | Switch (_,_,_,t,_) -> t
        // TODO: Quotations must have their own type
        | Quote _ -> Any

    member this.Range: SourceLocation option =
        match this with
        | ObjExpr (_,r) -> r
        | Value v -> v.Range
        | Wrapped (e,_) | Quote e -> e.Range
        | VarDeclaration (_,_,_,range)
        | Apply (_,_,_,_,range)
        | IfThenElse (_,_,_,range)
        | Throw (_,_,range)
        | DebugBreak range
        | Loop (_,range)
        | Set (_,_,_,range)
        | Sequential (_,range)
        | TryCatch (_,_,_,range)
        | Switch (_,_,_,_,range) -> range

    member this.ImmediateSubExpressions: Expr list =
        match this with
        | Value v -> v.ImmediateSubExpressions
        | ObjExpr (decls,_) ->
            (decls |> List.map (fun (_,_,e) -> e))
        | VarDeclaration (_,e,_,_) -> [e]
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

    member this.IsNull =
        match this with
        // Check also cases when null is wrapped to represent a specific type
        | Value Null | Wrapped(Value Null, _) -> true
        | _ -> false
