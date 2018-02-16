namespace rec Fable.AST.Fable

open Fable
open Fable.AST
open Microsoft.FSharp.Compiler.SourceCodeServices

type EnumTypeKind = NumberEnumType | StringEnumType
type FunctionTypeKind = LambdaType of Type | DelegateType of Type list

type Type =
    | Any
    | Unit
    | Boolean
    | Char
    | String
    | Regex
    | Number of NumberKind
    | ExtendedNumber of ExtendedNumberKind
    | EnumType of kind: EnumTypeKind * fullName: string
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
    | FunctionDeclaration of publicName: string option * privateName: string * args: Ident list * body: Expr
    | ValueDeclaration of publicName: string option * privateName: string * value: Expr * isMutable: bool

type File(sourcePath, decls, ?usedVarNames, ?dependencies) =
    member __.SourcePath: string = sourcePath
    member __.Declarations: Declaration list = decls
    member __.UsedVarNames: Set<string> = defaultArg usedVarNames Set.empty
    member __.Dependencies: Set<string> = defaultArg dependencies Set.empty

type Ident =
    { Name: string
      Type: Type
      IsMutable: bool
      Range: SourceLocation option }

type ImportKind =
    | CoreLib
    | Internal
    | CustomImport

type MemberKind =
    | Getter
    | Setter
    | Constructor
    | Method

type EnumKind = NumberEnum of int | StringEnum of string
type NewArrayKind = ArrayValues of Expr list | ArrayAlloc of int

type ValueKind =
    | This of Type
    | Null of Type
    | UnitConstant
    | BoolConstant of bool
    | CharConstant of char
    | StringConstant of string
    | NumberConstant of float * NumberKind
    | RegexConstant of source: string * flags: RegexFlag list
    | Enum of EnumKind * enumFullName: string
    | NewOption of value: Expr option * Type
    | NewTuple of Expr list
    | NewArray of NewArrayKind * Type
    | NewList of headAndTail: (Expr * Expr) option * Type
    | NewRecord of Expr list * FSharpEntity * genArgs: Type list
    | NewErasedUnion of Expr * genericArgs: Type list
    | NewUnion of Expr list * FSharpUnionCase * FSharpEntity * genArgs: Type list
    | UnionCaseTag of FSharpUnionCase * FSharpEntity
    // member this.ImmediateSubExpressions: Expr list =
    //     match this with
    //     | BoolConstant _ | NumberConstant _
    //     | StringConstant _ | RegexConstant _
    //     | ListEmpty _ | ArrayAlloc _ | NoneConst _
    //     | UnitConstant | Null _ | This _ -> []
    //     | NewList(head, tail, _) -> [head; tail]
    //     | NewArray(exprs, _) -> exprs
    //     | NewTuple exprs -> exprs
    //     | SomeConst(expr,_) -> [expr]

    // member this.Type =
    //     match this with
    //     | UnitConstant -> Unit
    //     | NumberConstant (_,kind) -> Number kind
    //     | StringConstant _ -> String
    //     | RegexConstant _ -> Regex
    //     | BoolConstant _ -> Boolean
    //     | NewList (_,_,typ) | ListEmpty typ -> List typ
    //     | NewArray (_,typ) | ArrayAlloc (_,typ) -> Array typ
    //     | NewTuple exprs -> exprs |> List.map (fun x -> x.Type) |> Tuple
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
  { owner: FSharpEntity option
    argTypes: Type list
    genericArgs: Type list
    isConstructor: bool
    hasSpread: bool
    hasThisArg: bool }

type OperationKind =
    | Apply of applied: Expr * args: Expr list * argTypes: Type list
    | DynamicApply of applied: Expr * args: Expr list
    | UnresolvedCall of callee: Expr option * args: Expr list * info: CallInfo
    | Call of callee: Expr * memb: string option * args: Expr list * info: CallInfo
    | Emit of macro: string * argsAndCallInfo: (Expr list * CallInfo) option
    | UnaryOperation of UnaryOperator * Expr
    | BinaryOperation of BinaryOperator * left:Expr * right:Expr
    | LogicalOperation of LogicalOperator * left:Expr * right:Expr

type GetKind =
    | FieldGet of string
    | IndexGet of int
    | DynamicGet of Expr
    | ListHead
    | ListTail
    | OptionValue
    | UnionTag of FSharpEntity
    | UnionField of FSharpUnionCase * FSharpEntity
    | RecordGet of FSharpField * FSharpEntity

type SetKind =
    | VarSet
    | FieldSet of string
    | IndexSet of int
    | DynamicSet of Expr
    | RecordSet of FSharpField * FSharpEntity

type Expr =
    | Value of ValueKind
    | IdentExpr of Ident
    | Cast of Expr * targetType: Type
    | Import of memb: string * path: string * ImportKind * Type

    | Function of FunctionKind * body: Expr
    | ObjectExpr of decls: Declaration list * Type

    | Operation of OperationKind * typ: Type * range: SourceLocation option
    | Get of Expr * GetKind * typ: Type * range: SourceLocation option

    | Debugger
    | Throw of Expr * typ: Type * range: SourceLocation option

    | Sequential of Expr list
    | Let of bindings: (Ident * Expr) list * body: Expr
    | Set of Expr * SetKind * value: Expr * range: SourceLocation option
    | Loop of LoopKind * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option
    | Switch of matchValue: Expr * cases: (Expr list * Expr) list * defaultCase: Expr option * typ: Type

    member this.IsJsStatement =
        false // TODO TODO TODO
    //     match this with
    //     | Import _ | EntityRef _
    //     | IdentExpr _ | Lambda _ | Value _ | ObjectExpr _ | Apply _ | Call _ | Get _ -> false
    //     | IfThenElse (_,thenExpr,elseExpr,_) -> thenExpr.IsJsStatement || elseExpr.IsJsStatement
    //     | Throw _ | Debugger _ | Loop _ | Set _ | VarDeclaration _
    //     | Sequential _ | TryCatch _ | Switch _ -> true

    member this.Type =
        Any // TODO TODO TODO
    //     match this with
    //     | This | ObjectExpr _ | EntityRef _ -> Any
    //     | Spread e | Uncurry (e,_) -> e.Type
    //     | IdentExpr (i,_) -> i.Type
    //     | Value kind -> kind.Type
    //     | Lambda (args, body, _) -> LambdaType(List.map Ident.getType args, body.Type)
    //     | Null typ | Import (_,_,_,typ) | Throw (_,typ,_)
    //     | Call (_,_,_,_,typ,_) | Get (_,_,typ,_) -> typ
    //     | IfThenElse (_,thenExpr,_,_) -> thenExpr.Type
    //     | Debugger _ | Loop _ | Set _ | VarDeclaration _ -> Unit
    //     | Sequential ([],_) -> Unit
    //     | Apply (applied,_,_) ->
    //         match applied.Type with LambdaType(_,typ) -> typ | _ -> Any
    //     | Sequential (exprs,_) -> List.last exprs |> Expr.getType
    //     | TryCatch (body,_,_,_) -> body.Type
    //     | Switch (_,_,_,t,_) -> t

    member this.Range: SourceLocation option =
        None // TODO TODO TODO
    //     match this with
    //     | This | Null _ | Value _ | Import _ | EntityRef _ -> None
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
    //     | This | Null _ | IdentExpr _ | Import _ | EntityRef _ | Debugger _ -> []
    //     | Value v -> v.ImmediateSubExpressions
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
