namespace rec Fable.AST.Fable

open Fable
open Fable.AST
open Microsoft.FSharp.Compiler.SourceCodeServices
open System

type EnumTypeKind = NumberEnumType | StringEnumType
type FunctionTypeKind = LambdaType of Type | DelegateType of Type list

type Type =
    | MetaType
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

    member this.Generics =
        match this with
        | Option gen | Array gen | List gen -> [gen]
        | FunctionType(LambdaType argType, returnType) -> [argType; returnType]
        | FunctionType(DelegateType argTypes, returnType) -> argTypes @ [returnType]
        | Tuple gen -> gen
        | ErasedUnion gen -> gen
        | DeclaredType(_,gen) -> gen
        | _ -> []
    member this.ReplaceGenerics(newGen: Type list) =
        match this with
        | Option _ -> Option newGen.Head
        | Array _  -> Array newGen.Head
        | List _   -> List newGen.Head
        | FunctionType(LambdaType _, _) ->
            let argTypes, returnType = List.splitLast newGen
            FunctionType(LambdaType argTypes.Head, returnType)
        | FunctionType(DelegateType _, _) ->
            let argTypes, returnType = List.splitLast newGen
            FunctionType(DelegateType argTypes, returnType)
        | Tuple _ -> Tuple newGen
        | ErasedUnion _ -> ErasedUnion newGen
        | DeclaredType(ent,_) -> DeclaredType(ent,newGen)
        | t -> t

type ValueDeclarationInfo =
    { Name: string
      IsPublic: bool
      IsMutable: bool
      IsEntryPoint: bool
      HasSpread: bool }

type ClassImplicitConstructorInfo =
    { Name: string
      Entity: FSharpEntity
      EntityName: string
      IsEntityPublic: bool
      IsConstructorPublic: bool
      HasSpread: bool
      Base: Expr option
      Arguments: Ident list
      BoundConstructorThis: Ident
      Body: Expr }

type UnionConstructorInfo =
    { Entity: FSharpEntity
      EntityName: string
      IsPublic: bool }

type CompilerGeneratedConstructorInfo =
    { Entity: FSharpEntity
      EntityName: string
      IsPublic: bool }

type ConstructorKind =
    | ClassImplicitConstructor of ClassImplicitConstructorInfo
    | UnionConstructor of UnionConstructorInfo
    | CompilerGeneratedConstructor of CompilerGeneratedConstructorInfo

type OverrideDeclarationInfo =
    { Name: string
      Kind: ObjectMemberKind
      EntityName: string }

type InterfaceImplementation =
    { Name: string
      IsPublic: bool
      ImplementingType: FSharpEntity
      InterfaceType: FSharpEntity
      /// Name of the casting functions for inherited interfaces
      InheritedInterfaces: string list
      Members: ObjectMember list }

type Declaration =
    | ActionDeclaration of Expr
    | ValueDeclaration of Expr * ValueDeclarationInfo
    | OverrideDeclaration of args: Ident list * body: Expr * OverrideDeclarationInfo
    | ConstructorDeclaration of ConstructorKind * InterfaceImplementation list

type File(sourcePath, decls, ?usedVarNames, ?dependencies) =
    member __.SourcePath: string = sourcePath
    member __.Declarations: Declaration list = decls
    member __.UsedVarNames: Set<string> = defaultArg usedVarNames Set.empty
    member __.Dependencies: Set<string> = defaultArg dependencies Set.empty

type IdentKind =
    | UnespecifiedIdent
    | BaseValueIdent
    | ThisArgIdentDeclaration

type Ident =
    { Name: string
      Type: Type
      Kind: IdentKind
      IsMutable: bool
      IsCompilerGenerated: bool
      Range: SourceLocation option }
      member x.IsBaseValue = match x.Kind with BaseValueIdent -> true | _ -> false
      member x.IsThisArgDeclaration = match x.Kind with ThisArgIdentDeclaration -> true | _ -> false

type ImportKind =
    | CoreLib
    | Internal
    | CustomImport

type EnumKind = NumberEnum of Expr | StringEnum of Expr
type NewArrayKind = ArrayValues of Expr list | ArrayAlloc of Expr

type ValueKind =
    | TypeInfo of Type * SourceLocation option // Error messages need location info
    | Null of Type
    | UnitConstant
    | BoolConstant of bool
    | CharConstant of char
    | StringConstant of string
    | NumberConstant of float * NumberKind
    | RegexConstant of source: string * flags: RegexFlag list
    | Enum of EnumKind * enumFullName: string
    | NewOption of value: Expr option * Type
    | NewArray of NewArrayKind * Type
    | NewList of headAndTail: (Expr * Expr) option * Type
    | NewTuple of Expr list
    | NewRecord of Expr list * FSharpEntity * genArgs: Type list
    | NewUnion of Expr list * FSharpUnionCase * FSharpEntity * genArgs: Type list
    | NewErasedUnion of Expr * genericArgs: Type list
    member this.Type =
        match this with
        | TypeInfo _ -> MetaType
        | Null t -> t
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
        | NewArray(_, t) -> Array t
        | NewList(_, t) -> List t
        | NewTuple exprs -> exprs |> List.map (fun e -> e.Type) |> Tuple
        | NewRecord(_, ent, genArgs) -> DeclaredType(ent, genArgs)
        | NewUnion(_, _, ent, genArgs) -> DeclaredType(ent, genArgs)
        | NewErasedUnion(_, genArgs) -> ErasedUnion genArgs

type LoopKind =
    | While of guard: Expr * body: Expr
    | For of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool

type FunctionKind =
    | Lambda of arg: Ident
    | Delegate of args: Ident list

type SpreadKind =
    | NoSpread
    /// The ... spread operator will be applied to last argument
    | SeqSpread
    /// If the argument is a tuple apply its members as separate arguments
    /// (Used for dynamic calls like `foo?bar(4, 5, 6)`)
    | TupleSpread

type CallKind =
    /// Constructor calls will add the `new` keyword in JS
    | ConstructorCall of Expr
    /// Static calls contain a direct reference to the function and will pass `ThisArg` as first argument
    | StaticCall of Expr
    /// Instance calls will be applied to `ThisArg` (optionally through member access)
    | InstanceCall of memb: Expr option

type SignatureKind =
    /// Argument expected types will be checked for the uncurrying optimization
    | Typed of Type list
    /// Nested function arguments will be automatically uncurried
    | AutoUncurrying
    /// Arguments won't be uncurried
    | NoUncurrying

type ArgInfo =
  { ThisArg: Expr option
    Args: Expr list
    /// Argument types as defined in the method signature, this may be slightly different to types of actual argument expressions.
    /// E.g.: signature accepts 'a->'b->'c (2-arity) but we pass int->int->int->int (3-arity)
    SignatureArgTypes: SignatureKind
    Spread: SpreadKind
    IsBaseOrSelfConstructorCall: bool }

type ReplaceCallInfo =
  { CompiledName: string
    OverloadSuffix: Lazy<string>
    /// See ArgIngo.SignatureArgTypes
    SignatureArgTypes: Type list
    Spread: SpreadKind
    DeclaringEntityFullName: string
    GenericArgs: (string * Type) list }

type OperationKind =
    | Call of kind: CallKind * info: ArgInfo
    | CurriedApply of applied: Expr * args: Expr list
    | Emit of macro: string * args: ArgInfo option
    | UnaryOperation of UnaryOperator * Expr
    | BinaryOperation of BinaryOperator * left:Expr * right:Expr
    | LogicalOperation of LogicalOperator * left:Expr * right:Expr

type GetKind =
    | ExprGet of Expr
    | TupleGet of int
    | FieldGet of string * hasDoubleEvalRisk: bool * fieldType: Type
    | UnionField of FSharpField * FSharpUnionCase * fieldType: Type
    | UnionTag
    | ListHead
    | ListTail
    | OptionValue

type SetKind =
    | VarSet
    | ExprSet of Expr
    | FieldSet of string * Type

type TestKind =
    | TypeTest of Type
    | ErasedUnionTest of Type
    | OptionTest of isSome: bool
    | ListTest of isCons: bool
    | UnionCaseTest of FSharpUnionCase * FSharpEntity

type ObjectMemberKind =
    | ObjectValue
    | ObjectMethod of hasSpread: bool
    | ObjectGetter
    | ObjectSetter
    | ObjectIterator

type ObjectMember =
    | ObjectMember of key: Expr * value: Expr * ObjectMemberKind

type DelayedResolutionKind =
    | AsInterface of source: Expr * cast: (Expr->Expr) * interfaceFullName: string
    | AsPojo of Expr * caseRules: Expr
    | AsUnit of Expr
    | Curry of Expr * arity: int

type Expr =
    | Value of ValueKind
    | IdentExpr of Ident
    /// Some expressions must be resolved in the last pass for better optimization (e.g. list to seq cast)
    | DelayedResolution of DelayedResolutionKind * Type * SourceLocation option
    | Import of selector: Expr * path: Expr * ImportKind * Type * SourceLocation option

    | Function of FunctionKind * body: Expr * name: string option
    | ObjectExpr of ObjectMember list * Type * baseCall: Expr option

    | Test of Expr * TestKind * range: SourceLocation option
    | Operation of OperationKind * typ: Type * range: SourceLocation option
    | Get of Expr * GetKind * typ: Type * range: SourceLocation option

    | Debugger
    | Throw of Expr * typ: Type * range: SourceLocation option

    | DecisionTree of Expr * targets: (Ident list * Expr) list
    | DecisionTreeSuccess of targetIndex: int * boundValues: Expr list * Type

    | Sequential of Expr list
    | Let of bindings: (Ident * Expr) list * body: Expr
    | Set of Expr * SetKind * value: Expr * range: SourceLocation option
    // TODO: Check if we actually need range for loops
    | Loop of LoopKind * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr

    member this.Type =
        match this with
        | Test _ -> Boolean
        | Value kind -> kind.Type
        | IdentExpr id -> id.Type
        | Import(_,_,_,t,_) | DelayedResolution(_,t,_) | ObjectExpr(_,t,_)
        | Operation(_,t,_) | Get(_,_,t,_) | Throw(_,t,_) | DecisionTreeSuccess(_,_,t) -> t
        | Debugger | Set _ | Loop _ -> Unit
        | Sequential exprs -> (List.last exprs).Type
        | Let(_,expr) | TryCatch(expr,_,_) | IfThenElse(_,expr,_) | DecisionTree(expr,_) -> expr.Type
        | Function(kind,body,_) ->
            match kind with
            | Lambda arg -> FunctionType(LambdaType arg.Type, body.Type)
            | Delegate args -> FunctionType(DelegateType(args |> List.map (fun a -> a.Type)), body.Type)

    member this.Range: SourceLocation option =
        match this with
        | Value _ | Import _ | DelayedResolution _
        | ObjectExpr _ | Debugger | Sequential _ | Let _
        | IfThenElse _ | TryCatch _ | DecisionTree _ | DecisionTreeSuccess _ -> None

        | Function(_,body,_) -> body.Range
        | IdentExpr id -> id.Range
        | Test(_,_,r) | Operation(_,_,r) | Get(_,_,_,r) | Throw(_,_,r) | Set(_,_,_,r) | Loop(_,r) -> r
