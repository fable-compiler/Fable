namespace rec Fable.AST.Fable

open Fable
open Fable.AST
open Microsoft.FSharp.Compiler.SourceCodeServices

type EnumTypeKind = NumberEnum | StringEnum
type FunctionTypeKind = LambdaType of Type | DelegateType of Type list

type Type =
    | Any
    | Unit
    | Bool
    | Char
    | String
    | Regex
    | Number of NumberKind
    | ExtendedNumber of ExtendedNumberKind
    | Enum of kind: EnumTypeKind * fullName: string
    | Option of genericArg: Type
    | Tuple of genericArgs: Type list
    | Array of genericArg: Type
    | List of genericArg: Type
    | FunctionType of FunctionTypeKind * returnType: Type
    | GenericParam of name: string
    | ErasedUnion of genericArgs: Type list
    | DeclaredType of FSharpEntity * genericArgs: Type list

type Declaration =
    | ActionDeclaration of Expr
    /// Module members are also declared as variables, so they need
    /// a private name that doesn't conflict with enclosing scope (see #130)
    | ModuleDeclaration of publicName: string option * privateName: string * Declaration list
    // TODO: Getter or setter?
    | FunctionDeclaration of publicName: string option * privateName: string * args: Ident list * body: Expr
    | ValueDeclaration of name: string * value: Expr * isMutable: bool

type File(sourcePath, root, decls, ?usedVarNames, ?dependencies) =
    member __.SourcePath: string = sourcePath
    member __.Root: string = root
    member __.Declarations: Declaration list = decls
    member __.UsedVarNames: Set<string> = defaultArg usedVarNames Set.empty
    member __.Dependencies: Set<string> = defaultArg dependencies Set.empty

// TODO: Add IsCompilerGenerated to make optimizations easier later?
type Ident =
    { Name: string
      Type: Type
      IsMutable: bool
      Range: SourceLocation option }

type ImportKind =
    | CoreLib
    | Internal of file: string
    | CustomImport

type EnumConstKind = NumberEnumConst of int | StringEnumConst of string

type ConstKind =
    | This of Type
    | Null of Type
    | UnitConst
    | BoolConst of bool
    | CharConst of char
    | StringConst of string
    | RegexConst of source: string * flags: RegexFlag list
    | NumberConst of float * NumberKind
    | EnumConst of EnumConstKind * enumFullName: string
    | OptionConst of value: Expr option * Type
    | TupleConst of Expr list
    | ArrayConst of Expr list * Type
    | ArrayAlloc of int * Type
    | ListConst of headAndTail: (Expr * Expr) option * Type
    | ErasedUnionConst of Expr * genericArgs: Type list
    | UnionConst of FSharpUnionCase * Expr list * FSharpEntity * genArgs: Type list
    | RecordConst of Expr list * FSharpEntity * genArgs: Type list
    // member this.ImmediateSubExpressions: Expr list =
    //     match this with
    //     | BoolConst _ | NumberConst _
    //     | StringConst _ | RegexConst _
    //     | ListEmpty _ | ArrayAlloc _ | NoneConst _
    //     | UnitConst | Null _ | This _ -> []
    //     | ListConst(head, tail, _) -> [head; tail]
    //     | ArrayConst(exprs, _) -> exprs
    //     | TupleConst exprs -> exprs
    //     | SomeConst(expr,_) -> [expr]

    // member this.Type =
    //     match this with
    //     | UnitConst -> Unit
    //     | NumberConst (_,kind) -> Number kind
    //     | StringConst _ -> String
    //     | RegexConst _ -> Regex
    //     | BoolConst _ -> Boolean
    //     | ListConst (_,_,typ) | ListEmpty typ -> List typ
    //     | ArrayConst (_,typ) | ArrayAlloc (_,typ) -> Array typ
    //     | TupleConst exprs -> exprs |> List.map (fun x -> x.Type) |> Tuple
    //     | SomeConst (_, typ) | NoneConst typ -> Option typ
    //     | Null typ | This typ -> typ

type LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: Ident * enumerable: Expr * body: Expr

type FunctionKind =
    | Lambda of arg: Ident
    | Delegate of args: Ident list

type CallInfo =
  { ownerFullName: string
    memberName: string
    callee: Expr option
    args: Expr list
    argTypes: Type list
    genericArgs: Type list
    spreadLastArg: bool }

type OperationKind =
    | UnresolvedCall of CallInfo
    | Call of callee: Expr * memb: string option * args: Expr list * isConstructor: bool
    | Apply of applied: Expr * args: Expr list
    | DynamicApply of applied: bool * args: bool list * spreadLastArg: bool
    | UnaryOperation of UnaryOperator * Expr
    | BinaryOperation of BinaryOperator * left:Expr * right:Expr
    | LogicalOperation of LogicalOperator * left:Expr * right:Expr
    | Emit of macro: string * args: Expr list

type Expr =
    | Debugger
    | Const of ConstKind
    | IdentExpr of Ident
    | Cast of Expr * targetType: Type
    | EntityRef of FSharpEntity
    | ImportRef of memb: string * path: string * ImportKind * Type
    | Function of FunctionKind * body: Expr
    | ObjectExpr of decls: Declaration list * Type

    | Let of bindings: (Ident * Expr) list * body: Expr
    | Get of Expr * field: Expr * typ: Type * range: SourceLocation option
    | Set of callee: Expr * field: Expr option * value: Expr * range: SourceLocation option
    | Throw of Expr * typ: Type * range: SourceLocation option
    | Sequential of Expr list
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr
    | Loop of LoopKind * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option * range: SourceLocation option
    | Switch of matchValue: Expr * cases: (Expr list * Expr) list * defaultCase: Expr option * typ: Type * range: SourceLocation option

    // member this.IsJsStatement =
    //     match this with
    //     | ImportRef _ | EntityRef _
    //     | IdentExpr _ | Lambda _ | Const _ | ObjectExpr _ | Apply _ | Call _ | Get _ -> false
    //     | IfThenElse (_,thenExpr,elseExpr,_) -> thenExpr.IsJsStatement || elseExpr.IsJsStatement
    //     | Throw _ | Debugger _ | Loop _ | Set _ | VarDeclaration _
    //     | Sequential _ | TryCatch _ | Switch _ -> true

    // member this.Type =
    //     match this with
    //     | This | ObjectExpr _ | EntityRef _ -> Any
    //     | Spread e | Uncurry (e,_) -> e.Type
    //     | IdentExpr (i,_) -> i.Type
    //     | Const kind -> kind.Type
    //     | Lambda (args, body, _) -> LambdaType(List.map Ident.getType args, body.Type)
    //     | Null typ | ImportRef (_,_,_,typ) | Throw (_,typ,_)
    //     | Call (_,_,_,_,typ,_) | Get (_,_,typ,_) -> typ
    //     | IfThenElse (_,thenExpr,_,_) -> thenExpr.Type
    //     | Debugger _ | Loop _ | Set _ | VarDeclaration _ -> Unit
    //     | Sequential ([],_) -> Unit
    //     | Apply (applied,_,_) ->
    //         match applied.Type with LambdaType(_,typ) -> typ | _ -> Any
    //     | Sequential (exprs,_) -> List.last exprs |> Expr.getType
    //     | TryCatch (body,_,_,_) -> body.Type
    //     | Switch (_,_,_,t,_) -> t

    // member this.Range: SourceLocation option =
    //     match this with
    //     | This | Null _ | Const _ | ImportRef _ | EntityRef _ -> None
    //     | Spread e | Uncurry (e,_) -> e.Range
    //     | IdentExpr (_,range)
    //     | ObjectExpr (_,range)
    //     | VarDeclaration (_,_,_,range)
    //     | Apply (_,_,range)
    //     | Call (_,_,_,_,_,range)
    //     | Get (_,_,_,range)
    //     | Lambda (_,_,range)
    //     | IfThenElse (_,_,_,range)
    //     | Throw (_,_,range)
    //     | Debugger range
    //     | Loop (_,range)
    //     | Set (_,_,_,range)
    //     | Sequential (_,range)
    //     | TryCatch (_,_,_,range)
    //     | Switch (_,_,_,_,range) -> range

    // member this.ImmediateSubExpressions: Expr list =
    //     match this with
    //     | This | Null _ | IdentExpr _ | ImportRef _ | EntityRef _ | Debugger _ -> []
    //     | Const v -> v.ImmediateSubExpressions
    //     | ObjectExpr (decls,_) -> decls |> List.map (fun (_,_,e) -> e)
    //     | VarDeclaration (_,e,_,_) | Throw (e,_,_) | Lambda (_,e,_) | Spread e | Uncurry (e,_) -> [e]
    //     | IfThenElse (cond,thenExpr,elseExpr,_) -> [cond;thenExpr;elseExpr]
    //     | Get (expr,field,_,_) -> [expr; field]
    //     | Apply (callee,args,_) -> callee::args
    //     | Call (callee,field,args,_,_,_) ->
    //         let exprs = match field with Some x -> x::args | None -> args
    //         match callee with
    //         | UnaryOp _ | BinaryOp _ | LogicalOp _ | Emit _ -> exprs
    //         | Callee callee -> callee::exprs
    //     | Loop (kind,_) ->
    //         match kind with
    //         | While(e1,e2) -> [e1;e2]
    //         | For(_,e1,e2,e3,_) -> [e1;e2;e3]
    //         | ForOf(_,e1,e2) -> [e1;e2]
    //     | Set (callee,field,value,_) ->
    //         match field with
    //         | Some field -> [callee;field;value]
    //         | None -> [callee;value]
    //     | Sequential (exprs,_) -> exprs
    //     | TryCatch (body,catch,finalizer,_) ->
    //         [ yield body
    //           match catch with
    //           | Some (_,catch) -> yield catch
    //           | None -> ()
    //           yield! Option.toList finalizer ]
    //     | Switch (test,cases,defCase,_,_) ->
    //         [ yield test
    //           for (labels, body) in cases do
    //             yield! labels
    //             yield body
    //           yield! Option.toList defCase]
