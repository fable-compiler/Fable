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
    | EnumType of kind: EnumTypeKind * fullName: string
    | Option of genericArg: Type
    | Tuple of genericArgs: Type list
    | Array of genericArg: Type
    | List of genericArg: Type
    | FunctionType of FunctionTypeKind * returnType: Type
    | GenericParam of name: string
    | ErasedUnion of genericArgs: Type list
    | DeclaredType of FSharpEntity * genericArgs: Type list

type DeclarationInfo =
    { PrivateName: string
      PublicName: string option
      IsMutable: bool
      HasSpread: bool }

type Declaration =
    | ActionDeclaration of Expr
    | ValueDeclaration of Expr * DeclarationInfo
    // TODO: Special functions: implicit constructors, overrides, interface implementations

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
    member this.Type =
        match this with
        | This t | Null t -> t
        | UnitConstant -> Unit
        | BoolConstant _ -> Boolean
        | CharConstant _ -> Char
        | StringConstant _ -> String
        | NumberConstant(_,kind) -> Number kind
        | RegexConstant _ -> Regex
        | Enum(kind, fullName) ->
            let kind =
                match kind with
                | NumberEnum _ -> NumberEnumType
                | StringEnum _ -> StringEnumType
            EnumType(kind, fullName)
        | NewOption(_, t) -> Option t
        | NewTuple exprs -> exprs |> List.map (fun e -> e.Type) |> Tuple
        | NewArray(_, t) -> Array t
        | NewList(_, t) -> List t
        | NewRecord(_, ent, genArgs) -> DeclaredType(ent, genArgs)
        | NewErasedUnion(_, genArgs) -> ErasedUnion genArgs
        | NewUnion(_, _, ent, genArgs) -> DeclaredType(ent, genArgs)
        | UnionCaseTag _ -> Any // There may be different ways to represent tags

type LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool
    | ForOf of ident: Ident * enumerable: Expr * body: Expr

type FunctionKind =
    | Lambda of arg: Ident
    | Delegate of args: Ident list

type CallInfo =
  { ArgTypes: Type list
    IsConstructor: bool
    HasThisArg: bool
    HasSeqSpread: bool
    HasTupleSpread: bool
    UncurryLambdaArgs: bool
  }

type ExtraCallInfo =
  { EnclosingEntityFullName: string
    CompiledName: string
    GenericArgs: Map<string, Type> }

type OperationKind =
    | Call of callee: Expr * memb: string option * args: Expr list * info: CallInfo
    | UnresolvedCall of callee: Expr option * args: Expr list * info: CallInfo * extraInfo: ExtraCallInfo
    | CurriedApply of applied: Expr * args: Expr list
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
    | TupleGet of int
    | UnionTag of FSharpEntity
    | UnionField of FSharpField * FSharpUnionCase * FSharpEntity
    | RecordGet of FSharpField * FSharpEntity

type SetKind =
    | VarSet
    | FieldSet of string
    | IndexSet of int
    | DynamicSet of Expr
    | RecordSet of FSharpField * FSharpEntity

type ObjectMemberKind =
    | ObjectValue of hasSpread: bool
    | ObjectGetter
    | ObjectSetter

type ObjectMember = string * Expr * ObjectMemberKind

type Expr =
    | Value of ValueKind
    | IdentExpr of Ident
    | Cast of Expr * targetType: Type
    | Import of selector: string * path: string * ImportKind * Type

    | Function of FunctionKind * body: Expr
    | ObjectExpr of ObjectMember list * Type

    | Operation of OperationKind * typ: Type * range: SourceLocation option
    | Get of Expr * GetKind * typ: Type * range: SourceLocation option

    | Debugger
    | Throw of Expr * typ: Type * range: SourceLocation option

    | Sequential of Expr list
    | Let of bindings: (Ident * Expr) list * body: Expr
    | Set of Expr * SetKind * value: Expr * range: SourceLocation option
    // TODO: Check if we actually need range for loops
    | Loop of LoopKind * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option
    | Switch of matchValue: Expr * cases: (Expr list * Expr) list * defaultCase: Expr option * typ: Type

    member this.IsJsStatement =
        match this with
        | Value _ | Import _ | Cast _ | IdentExpr _ | Function _
        | ObjectExpr _ | Operation _ | Get _ -> false

        | TryCatch _ | Switch _ | Debugger
        | Sequential _ | Let _ | Set _
        | Loop _ | Throw _ -> true

        | IfThenElse (_,thenExpr,elseExpr) ->
            thenExpr.IsJsStatement || elseExpr.IsJsStatement

    member this.Type =
        match this with
        | Value kind -> kind.Type
        | IdentExpr id -> id.Type
        | Import(_,_,_,t) | Cast(_,t) | ObjectExpr(_,t)
        | Operation(_,t,_) | Get(_,_,t,_) | Throw(_,t,_) | Switch(_,_,_,t) -> t
        | Debugger | Set _ | Loop _ -> Unit
        | Sequential exprs -> (List.last exprs).Type
        | Let(_,expr) | TryCatch(expr,_,_) | IfThenElse(_,expr,_) -> expr.Type
        | Function(kind,body) ->
            match kind with
            | Lambda arg -> FunctionType(LambdaType arg.Type, body.Type)
            | Delegate args -> FunctionType(DelegateType(args |> List.map (fun a -> a.Type)), body.Type)

    member this.Range: SourceLocation option =
        match this with
        | Value _ | Import _ | Cast _ | Function _ | ObjectExpr _
        | Debugger | Sequential _ | Let _
        | IfThenElse _ | TryCatch _ | Switch _ -> None

        | IdentExpr id -> id.Range
        | Operation(_,_,r) | Get(_,_,_,r) | Throw(_,_,r) | Set(_,_,_,r) | Loop(_,r) -> r
