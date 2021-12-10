namespace rec Fable.AST.Fable

open Fable.AST

type DeclaredType =
    abstract Entity: EntityRef
    abstract GenericArgs: Type list

type Field =
    abstract Name: string
    abstract FieldType: Type
    abstract IsMutable: bool
    abstract IsStatic: bool
    abstract LiteralValue: obj option

type UnionCase =
    abstract Name: string
    abstract CompiledName: string option
    abstract UnionCaseFields: Field list

type GenericParam =
    abstract Name: string

type MemberFunctionOrValue =
    inherit MemberInfo
    abstract DisplayName: string
    abstract CompiledName: string
    abstract FullName: string
    abstract CurriedParameterGroups: Parameter list list
    abstract ReturnParameter: Parameter
    abstract IsExplicitInterfaceImplementation: bool
    abstract ApparentEnclosingEntity: EntityRef

type Entity =
    abstract Ref: EntityRef
    abstract DisplayName: string
    abstract FullName: string
    abstract Attributes: Attribute seq
    abstract BaseType: DeclaredType option
    abstract AllInterfaces: DeclaredType seq
    abstract GenericParameters: GenericParam list
    abstract MembersFunctionsAndValues: MemberFunctionOrValue seq
    abstract FSharpFields: Field list
    abstract UnionCases: UnionCase list
    abstract IsPublic: bool
    abstract IsFSharpModule: bool
    abstract IsFSharpUnion: bool
    abstract IsFSharpRecord: bool
    abstract IsValueType: bool
    abstract IsFSharpExceptionDeclaration: bool
    abstract IsInterface: bool
    abstract IsMeasure: bool

// ------------------
// Serializable types
// ------------------

type EntityPath =
    | SourcePath of string
    | AssemblyPath of string
    /// Only used to reference entities in core assemblies without a path
    | CoreAssemblyName of string

type EntityRef =
    { FullName: string
      Path: EntityPath }

type Parameter =
    { Name: string option
      Type: Type }

type Attribute =
    { Entity: EntityRef
      ConstructorArgs: obj list }

// TODO: In Fable 4 this should be a record for easier serialization
type MemberInfo =
    abstract Attributes: Attribute seq
    abstract HasSpread: bool
    abstract IsMangled: bool
    abstract IsPublic: bool
    abstract IsInstance: bool
    abstract IsValue: bool
    abstract IsMutable: bool
    abstract IsGetter: bool
    abstract IsSetter: bool
    abstract IsEnumerator: bool

type Type =
    | MetaType
    | Any
    | Unit
    | Boolean
    | Char
    | String
    | Regex
    | Number of NumberKind
    | Enum of EntityRef
    | Option of genericArg: Type
    | Tuple of genericArgs: Type list
    | Array of genericArg: Type
    | List of genericArg: Type
    | LambdaType of Type * returnType: Type
    | DelegateType of Type list * returnType: Type
    | GenericParam of name: string
    | DeclaredType of EntityRef * genericArgs: Type list
    | AnonymousRecordType of fieldNames: string [] * genericArgs: Type list

    member this.Generics =
        match this with
        | Option gen
        | Array gen
        | List gen -> [ gen ]
        | LambdaType(argType, returnType) -> [ argType; returnType ]
        | DelegateType(argTypes, returnType) -> argTypes @ [ returnType ]
        | Tuple gen -> gen
        | DeclaredType (_, gen) -> gen
        | AnonymousRecordType (_, gen) -> gen
        | _ -> []

type ActionDecl = {
    Body: Expr
    UsedNames: Set<string>
}

type MemberDecl = {
    Name: string
    FullDisplayName: string
    Args: Ident list
    Body: Expr
    Info: MemberInfo
    UsedNames: Set<string>
    /// This can only be set once per file
    /// for a declaration in the root scope
    ExportDefault: bool
}

type ClassDecl = {
    Name: string
    Entity: EntityRef
    Constructor: MemberDecl option
    BaseCall: Expr option
    AttachedMembers: MemberDecl list
}

type Declaration =
    | ActionDeclaration of ActionDecl
    | MemberDeclaration of MemberDecl
    | ClassDeclaration of ClassDecl
    member this.UsedNames =
        match this with
        | ActionDeclaration d -> d.UsedNames
        | MemberDeclaration d -> d.UsedNames
        | ClassDeclaration d ->
            d.Constructor
            |> Option.map (fun c -> c.UsedNames)
            |> Option.defaultValue Set.empty
            |> fun usedNames -> usedNames, d.AttachedMembers
            ||> List.fold (fun acc m -> Set.union acc m.UsedNames)

type File(decls, ?usedRootNames) =
    member _.Declarations: Declaration list = decls
    member _.UsedNamesInRootScope: Set<string> = defaultArg usedRootNames Set.empty

type Ident =
    { Name: string
      Type: Type
      IsMutable: bool
      IsThisArgument: bool
      IsCompilerGenerated: bool
      Range: SourceLocation option }
    member x.DisplayName =
        x.Range
        |> Option.bind (fun r -> r.identifierName)
        |> Option.defaultValue x.Name

type ValueKind =
    // The AST from F# compiler is a bit inconsistent with ThisValue and BaseValue.
    // ThisValue only appears in constructors and not in instance members (where `this` is passed as first argument)
    // BaseValue can appear both in constructor and instance members (where they're associated to this arg)
    | ThisValue of Type
    | BaseValue of boundIdent: Ident option * Type
    // TODO: Add `allowGeneric` field
    | TypeInfo of Type
    | Null of Type
    | UnitConstant
    | BoolConstant of bool
    | CharConstant of char
    | StringConstant of string
    | NumberConstant of float * NumberKind
    | RegexConstant of source: string * flags: RegexFlag list
    | EnumConstant of Expr * EntityRef
    | NewOption of value: Expr option * Type
    | NewArray of Expr list * Type
    | NewArrayFrom of Expr * Type
    | NewList of headAndTail: (Expr * Expr) option * Type
    | NewTuple of Expr list
    | NewRecord of Expr list * EntityRef * genArgs: Type list
    | NewAnonymousRecord of Expr list * fieldNames: string [] * genArgs: Type list
    | NewUnion of Expr list * tag: int * EntityRef * genArgs: Type list
    member this.Type =
        match this with
        | ThisValue t
        | BaseValue(_,t) -> t
        | TypeInfo _ -> MetaType
        | Null t -> t
        | UnitConstant -> Unit
        | BoolConstant _ -> Boolean
        | CharConstant _ -> Char
        | StringConstant _ -> String
        | NumberConstant (_, kind) -> Number kind
        | RegexConstant _ -> Regex
        | EnumConstant (_, ent) -> Enum ent
        | NewOption (_, t) -> Option t
        | NewArray (_, t) -> Array t
        | NewArrayFrom (_, t) -> Array t
        | NewList (_, t) -> List t
        | NewTuple exprs -> exprs |> List.map (fun e -> e.Type) |> Tuple
        | NewRecord (_, ent, genArgs) -> DeclaredType(ent, genArgs)
        | NewAnonymousRecord (_, fieldNames, genArgs) -> AnonymousRecordType(fieldNames, genArgs)
        | NewUnion (_, _, ent, genArgs) -> DeclaredType(ent, genArgs)

type CallMemberInfo =
    { CurriedParameterGroups: Parameter list list
      IsInstance: bool
      IsGetter: bool
      FullName: string
      CompiledName: string
      DeclaringEntity: EntityRef option }

type CallInfo =
    { ThisArg: Expr option
      Args: Expr list
      /// Argument types as defined in the method signature, this may be slightly different to types of actual argument expressions.
      /// E.g.: signature accepts 'a->'b->'c (2-arity) but we pass int->int->int->int (3-arity)
      SignatureArgTypes: Type list
      CallMemberInfo: CallMemberInfo option
      HasSpread: bool
      IsJsConstructor: bool }

type ReplaceCallInfo =
    { CompiledName: string
      OverloadSuffix: string
      /// See ArgInfo.SignatureArgTypes
      SignatureArgTypes: Type list
      HasSpread: bool
      IsModuleValue: bool
      IsInterface: bool
      DeclaringEntityFullName: string
      GenericArgs: (string * Type) list }

type EmitInfo =
    { Macro: string
      IsJsStatement: bool
      CallInfo: CallInfo }

type ImportInfo =
    { Selector: string
      Path: string
      IsCompilerGenerated: bool }

type OperationKind =
    | Unary of UnaryOperator * Expr
    | Binary of BinaryOperator * left: Expr * right: Expr
    | Logical of LogicalOperator * left: Expr * right: Expr

type FieldKey =
    { Name: string
      FieldType: Type
      IsMutable: bool }

type KeyKind =
    | FieldKey of FieldKey
    | ExprKey of Expr

type GetKind =
    | ByKey of KeyKind
    | TupleIndex of int
    | UnionField of index: int * fieldType: Type
    | UnionTag
    | ListHead
    | ListTail
    | OptionValue

type TestKind =
    | TypeTest of Type
    | OptionTest of isSome: bool
    | ListTest of isCons: bool
    | UnionCaseTest of tag: int

type MemberRefInfo =
    {
        Name: string
        Path: string
        IsMutable: bool
        IsPublic: bool
        HasOverloadSuffix: bool
    }

type UnresolvedExpr =
    // TODO: Add also MemberKind from the flags?
    | UnresolvedTraitCall of sourceTypes: Type list * traitName: string * isInstance: bool * argTypes: Type list * argExprs: Expr list * typ: Type * range: SourceLocation option
    | UnresolvedReplaceCall of thisArg: Expr option * args: Expr list * info: ReplaceCallInfo * attachedCall: Expr option * typ: Type * range: SourceLocation option
    | UnresolvedInlineCall of memberUniqueName: string * genArgs: (string * Type) list * callee: Expr option * info: CallInfo * typ: Type * range: SourceLocation option
    | UnresolvedMemberRef of MemberRefInfo * typ: Type * range: SourceLocation option
    member this.Type =
        match this with
        | UnresolvedTraitCall(_,_,_,_,_,t,_)
        | UnresolvedReplaceCall(_,_,_,_,t,_)
        | UnresolvedInlineCall(_,_,_,_,t,_)
        | UnresolvedMemberRef(_,t,_) -> t
    member this.Range =
        match this with
        | UnresolvedTraitCall(_,_,_,_,_,_,r)
        | UnresolvedReplaceCall(_,_,_,_,_,r)
        | UnresolvedInlineCall(_,_,_,_,_,r)
        | UnresolvedMemberRef(_,_,r) -> r

type Expr =
    // Values and Idents
    | Value of ValueKind * SourceLocation option
    | IdentExpr of Ident

    // Closures
    /// Lambdas are curried, they always have a single argument (which can be unit)
    | Lambda of arg: Ident * body: Expr * name: string option
    /// Delegates are uncurried functions, can have none or multiple arguments
    | Delegate of args: Ident list * body: Expr * name: string option
    | ObjectExpr of MemberDecl list * Type * baseCall: Expr option

    // Type cast and tests
    | TypeCast of Expr * Type * tag: string option
    | Test of Expr * TestKind * range: SourceLocation option

    // Operations
    | Call of callee: Expr * info: CallInfo * typ: Type * range: SourceLocation option
    | CurriedApply of applied: Expr * args: Expr list * typ: Type * range: SourceLocation option
    | Curry of Expr * arity: int * Type * SourceLocation option
    | Operation of OperationKind * typ: Type * range: SourceLocation option

    // JS related: imports and statements
    | Import of ImportInfo * Type * SourceLocation option
    | Emit of EmitInfo * Type * SourceLocation option

    // Pattern matching
    | DecisionTree of Expr * targets: (Ident list * Expr) list
    | DecisionTreeSuccess of targetIndex: int * boundValues: Expr list * Type

    // Getters, setters and bindings
    | Let of Ident * Expr * body: Expr
    | LetRec of bindings: (Ident * Expr) list * body: Expr
    | Get of Expr * GetKind * typ: Type * range: SourceLocation option
    | Set of Expr * key: KeyKind option * value: Expr * range: SourceLocation option

    // Control flow
    | Sequential of Expr list
    | WhileLoop of guard: Expr * body: Expr * range: SourceLocation option
    | ForLoop of ident: Ident * start: Expr * limit: Expr * body: Expr * isUp: bool * range: SourceLocation option
    | TryCatch of body: Expr * catch: (Ident * Expr) option * finalizer: Expr option * range: SourceLocation option
    | IfThenElse of guardExpr: Expr * thenExpr: Expr * elseExpr: Expr * range: SourceLocation option

    | Unresolved of UnresolvedExpr

    member this.Type =
        match this with
        | Unresolved e -> e.Type
        | Test _ -> Boolean
        | Value (kind, _) -> kind.Type
        | IdentExpr id -> id.Type
        | Call(_,_,t,_)
        | CurriedApply(_,_,t,_)
        | TypeCast (_, t,_)
        | Import (_, t, _)
        | Curry (_, _, t, _)
        | ObjectExpr (_, t, _)
        | Operation (_, t, _)
        | Get (_, _, t, _)
        | Emit (_,t,_)
        | DecisionTreeSuccess (_, _, t) -> t
        | Set _
        | WhileLoop _
        | ForLoop _-> Unit
        | Sequential exprs -> List.tryLast exprs |> Option.map (fun e -> e.Type) |> Option.defaultValue Unit
        | Let (_, _, expr)
        | LetRec (_, expr)
        | TryCatch (expr, _, _, _)
        | IfThenElse (_, expr, _, _)
        | DecisionTree (expr, _) -> expr.Type
        | Lambda(arg, body, _) -> LambdaType(arg.Type, body.Type)
        | Delegate(args, body, _) -> DelegateType(args |> List.map (fun a -> a.Type), body.Type)

    member this.Range: SourceLocation option =
        match this with
        | Unresolved e -> e.Range
        | ObjectExpr _
        | Sequential _
        | Let _
        | LetRec _
        | DecisionTree _
        | DecisionTreeSuccess _ -> None
        | Lambda (_, e, _)
        | Delegate (_, e, _)
        | TypeCast (e, _, _) -> e.Range
        | IdentExpr id -> id.Range
        | Call(_,_,_,r)
        | CurriedApply(_,_,_,r)
        | Emit (_,_,r)
        | Import(_,_,r)
        | Curry(_,_,_,r)
        | Value (_, r)
        | IfThenElse (_, _, _, r)
        | TryCatch (_, _, _, r)
        | Test (_, _, r)
        | Operation (_, _, r)
        | Get (_, _, _, r)
        | Set (_, _, _, r)
        | ForLoop (_,_,_,_,_,r)
        | WhileLoop (_,_,r) -> r

// module PrettyPrint =
//     let rec printType (t: Type) = "T" // TODO

//     let rec printMultiWithSep separator start' exprs end' =
//         start' + (exprs |> List.map print |> String.concat separator) + end'

//     and printMulti start' exprs end' =
//         printMultiWithSep ", " start' exprs end'

//     and printFun (args: Ident list, body) =
//         let args = args |> List.map (fun a -> a.Name) |> String.concat ", "
//         $"({args}) => {print body}"

//     and printBinaryOp e1 op e2 =
//         match op with
//         | BinaryEqual -> print e1 + " == " + print e2
//         | BinaryUnequal -> print e1 + " != " + print e2
//         | BinaryEqualStrict -> print e1 + " === " + print e2
//         | BinaryUnequalStrict -> print e1 + " !== " + print e2
//         | BinaryLess -> print e1 + " < " + print e2
//         | BinaryLessOrEqual -> print e1 + " <= " + print e2
//         | BinaryGreater -> print e1 + " > " + print e2
//         | BinaryGreaterOrEqual -> print e1 + " >= " + print e2
//         | BinaryShiftLeft -> print e1 + " << " + print e2
//         | BinaryShiftRightSignPropagating -> print e1 + " >> " + print e2
//         | BinaryShiftRightZeroFill -> print e1 + " >>> " + print e2
//         | BinaryMinus -> print e1 + " - " + print e2
//         | BinaryPlus -> print e1 + " + " + print e2
//         | BinaryMultiply -> print e1 + " * " + print e2
//         | BinaryDivide -> print e1 + " / " + print e2
//         | BinaryModulus -> print e1 + " % " + print e2
//         | BinaryExponent -> print e1 + " ** " + print e2
//         | BinaryOrBitwise -> print e1 + " | " + print e2
//         | BinaryXorBitwise -> print e1 + " ^ " + print e2
//         | BinaryAndBitwise -> print e1 + " & " + print e2
//         | BinaryIn -> print e1 + " in " + print e2
//         | BinaryInstanceOf -> print e1 + " instanceof " + print e2

//     and printUnaryOp op e =
//         match op with
//         | UnaryMinus -> "-" + print e
//         | UnaryPlus -> "+" + print e
//         | UnaryNot -> "!" + print e
//         | UnaryNotBitwise -> "~" + print e
//         | UnaryTypeof -> "typeof " + print e
//         | UnaryVoid -> "void " + print e
//         | UnaryDelete -> "delete " + print e

//     and print = function
//         | IdentExpr i -> i.Name
//         | TypeCast(e,t,_) -> $"{print e} :> {printType t}"
//         | Import(_,_,_) -> "import"
//         | Value(kind,_) ->
//             match kind with
//             | ThisValue _ -> "this"
//             | BaseValue _ -> "base"
//             | TypeInfo t -> $"typeof<{printType t}>"
//             | Null _  -> "base"
//             | UnitConstant -> "()"
//             | BoolConstant x -> string x
//             | CharConstant x -> "'" + string x + "'"
//             | StringConstant x -> "\"" + string x + "\""
//             | NumberConstant(x, kind) ->
//                 match kind with
//                 | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> string(int x)
//                 | Float32 | Float64 -> string x
//             | RegexConstant(x,_) -> "/" + string x + "/" // TODO: flags
//             | EnumConstant(e, _) -> "enum " + print e
//             | NewOption(Some e, _) -> "Some " + print e
//             | NewOption(None, _) -> "None"
//             | NewTuple exprs -> printMulti "(" exprs ")"
//             | NewArray(exprs, _) -> printMulti "[|" exprs "|]"
//             | NewArrayFrom(e, _) -> $"array({print e})"
//             | NewList(ht, _) -> "list"
//                 // match ht with Some(h,t) -> [h;t] | None -> []
//             | NewRecord(exprs, _, _) -> printMulti "{" exprs "}"
//             | NewAnonymousRecord(exprs, _, _) -> printMulti "{" exprs "}"
//             | NewUnion(exprs, _, _, _) -> printMulti "union(" exprs ")"
//         | Curry(e, _, _, _) ->  $"curry({print e})"
//         | Lambda(arg, body, _) -> printFun ([arg], body)
//         | Delegate(args, body, _) -> printFun (args, body)
//         | ObjectExpr(members, _, baseCall) -> "object"
//             // let members = members |> List.map (fun m -> m.Body)
//             // match baseCall with Some b -> b::members | None -> members
//         | CurriedApply(callee, args, _, _) -> print callee + printMultiWithSep ")(" "(" args ")"
//         | Call(e1, info, _, _) ->
//             let args = (Option.toList info.ThisArg) @ info.Args
//             print e1 + printMulti "(" args ")"
//         | Emit(info, _, _) ->
//             let args = (Option.toList info.CallInfo.ThisArg) @ info.CallInfo.Args
//             $"""emit({info.Macro},{printMulti "(" args ")"})"""
//         | Operation(kind, _, _) ->
//             match kind with
//             | Unary(op, operand) -> printUnaryOp op operand
//             | Binary(op, e1, e2) -> printBinaryOp e1 op e2
//             | Logical(op, e1, e2) ->
//                 match op with
//                 | LogicalOr -> print e1 + " || " + print e2
//                 | LogicalAnd -> print e1 + " && " + print e2
//         | Test(e, kind, _) ->
//             match kind with
//             | TypeTest t -> $"""{print e} :? {printType t}"""
//             | OptionTest isSome -> $"""{print e}.Is{if isSome then "Some" else "None"}"""
//             | ListTest isNotEmpty -> $"""{print e}.Is{if isNotEmpty then "Not" else ""}Empty"""
//             | UnionCaseTest tag -> $"""{print e}.Tag == {tag}"""
//         | Get(e, kind, _, _) ->
//             match kind with
//             | ListHead -> print e + ".head"
//             | ListTail -> print e + ".tail"
//             | OptionValue -> print e + ".Value"
//             | TupleIndex i
//             | UnionField(i,_) -> print e + "[" + string i + "]"
//             | UnionTag -> print e + ".Tag"
//             | ByKey(FieldKey k) -> print e + "." + k.Name
//             | ByKey(ExprKey e2) -> print e + "[" + print e2 + "]"
//         | Sequential exprs -> printMultiWithSep "; " "" exprs ""
//         | Let(v, value, body) -> $"""let {v.Name} = {print value} in {print body}"""
//         | LetRec(bs, body) -> "let rec"
//         | IfThenElse(cond, thenExpr, elseExpr, _) ->
//             $"""if ({print cond}) {{{print thenExpr}}} else {{{print elseExpr}}}"""
//         | Set(e, kind, v, _) ->
//             let kind =
//                 match kind with
//                 | Some(ExprKey e2) -> $"""[{print e2}]"""
//                 | Some(FieldKey fi) -> $""".{fi.Name}"""
//                 | None -> ""
//             $"""{print e}{kind} <- {print v}"""
//         | WhileLoop(e1, e2, _) ->
//             $"""while ({print e1}) {{{print e2}}}"""
//         | ForLoop(v, e1, e2, e3, isUp, _) ->
//             $"""for {v.Name} = {print e1} {if isUp then "" else "down"}to {print e2} do {print e3}"""
//         | TryCatch(body, catch, finalizer, _) ->
//             $"""try {{{print body}}} {match catch with Some(_,c) -> "catch {" + print c + "} " | None -> ""}{match finalizer with Some c -> "finally {" + print c + "} " | None -> ""}"""
//         | DecisionTree(expr, targets) ->
//             let targets = targets |> List.map printFun |> String.concat "; "
//             $"""match({print expr}) targets({targets})"""
//         | DecisionTreeSuccess(idx, boundValues, _) ->
//             $"""target({idx}, {printMulti "[" boundValues "]"})"""
