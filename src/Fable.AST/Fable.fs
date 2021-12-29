namespace rec Fable.AST.Fable

open Fable.AST

type EntityPath =
    | SourcePath of string
    | AssemblyPath of string
    /// Only used to reference entities in core assemblies without a path
    | CoreAssemblyName of string
    | PrecompiledLib of sourcePath: string * assemblyPath: string

type EntityRef =
    { FullName: string
      Path: EntityPath }
    member this.SourcePath =
        match this.Path with
        | SourcePath p
        | PrecompiledLib(p,_) -> Some p
        | AssemblyPath _ | CoreAssemblyName _ -> None

type DeclaredType =
    abstract Entity: EntityRef
    abstract GenericArgs: Type list

// TODO: In Fable 4 this should be a record as it can be serialized through MemberInfo
// (also avoid `obj` for ConstructorArgs?)
type Attribute =
    abstract Entity: EntityRef
    abstract ConstructorArgs: obj list

type Field =
    abstract Name: string
    abstract FieldType: Type
    abstract IsMutable: bool
    abstract IsStatic: bool
    abstract LiteralValue: obj option

type UnionCase =
    abstract Name: string
    abstract FullName: string
    abstract CompiledName: string option
    abstract UnionCaseFields: Field list

[<RequireQualifiedAccess>]
type Constraint =
    | HasMember of name: string * isStatic: bool
    | CoercesTo of target: Type
    | IsNullable
    | IsValueType
    | IsReferenceType
    | HasDefaultConstructor
    | HasComparison
    | HasEquality
    | IsUnmanaged
    | IsEnum

type GenericParam =
    abstract Name: string
    abstract Constraints: Constraint seq

type Parameter =
    { Name: string option
      Type: Type }

// TODO: In Fable 4 this should be a record for consistency with other serializable types
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
    abstract IsFSharpAbbreviation: bool
    abstract IsFSharpExceptionDeclaration: bool
    abstract IsValueType: bool
    abstract IsInterface: bool
    abstract IsMeasure: bool
    abstract IsEnum: bool

type Type =
    | Measure of fullname: string
    | MetaType
    | Any
    | Unit
    | Boolean
    | Char
    | String
    | Regex
    | Number of kind: NumberKind * uom: string option
    | Enum of ref: EntityRef
    | Option of genericArg: Type * isStruct: bool
    | Tuple of genericArgs: Type list * isStruct: bool
    | Array of genericArg: Type
    | List of genericArg: Type
    | LambdaType of argType: Type * returnType: Type
    | DelegateType of argTypes: Type list * returnType: Type
    | GenericParam of name: string * constraints: Constraint list
    | DeclaredType of ref: EntityRef * genericArgs: Type list
    | AnonymousRecordType of fieldNames: string [] * genericArgs: Type list

    member this.Generics =
        match this with
        | Option(gen, _)
        | Array gen
        | List gen -> [ gen ]
        | LambdaType(argType, returnType) -> [ argType; returnType ]
        | DelegateType(argTypes, returnType) -> argTypes @ [ returnType ]
        | Tuple(gen, _) -> gen
        | DeclaredType (_, gen) -> gen
        | AnonymousRecordType (_, gen) -> gen
        // TODO: Check numbers with measure?
        | MetaType | Any | Unit | Boolean | Char | String | Regex | Number _ | Enum _ | GenericParam _ | Measure _ -> []

    member this.MapGenerics f =
        match this with
        | Option(gen, isStruct) -> Option(f gen, isStruct)
        | Array gen -> Array(f gen)
        | List gen -> List(f gen)
        | LambdaType(argType, returnType) -> LambdaType(f argType, f returnType)
        | DelegateType(argTypes, returnType) -> DelegateType(List.map f argTypes, f returnType)
        | Tuple(gen, isStruct) -> Tuple(List.map f gen, isStruct)
        | DeclaredType(e, gen) -> DeclaredType(e, List.map f gen)
        | AnonymousRecordType(e, gen) -> AnonymousRecordType(e, List.map f gen)
        | MetaType | Any | Unit | Boolean | Char | String | Regex | Number _ | Enum _ | GenericParam _ | Measure _ -> this

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

type ModuleDecl = {
    Name: string
    Entity: EntityRef
    Members: Declaration list
}

and Declaration =
    | ModuleDeclaration of ModuleDecl
    | ActionDeclaration of ActionDecl
    | MemberDeclaration of MemberDecl
    | ClassDeclaration of ClassDecl
    member this.UsedNames =
        match this with
        | ModuleDeclaration d -> Set.empty
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
    | ThisValue of typ: Type
    | BaseValue of boundIdent: Ident option * typ: Type
    // TODO: Add `allowGeneric` field, see makeGenericTypeInfo hack for generic info in decorators
    | TypeInfo of typ: Type
    | Null of typ: Type
    | UnitConstant
    | BoolConstant of value: bool
    | CharConstant of value: char
    | StringConstant of value: string
    | NumberConstant of value: float * kind: NumberKind * uom: string option
    | RegexConstant of source: string * flags: RegexFlag list
    | EnumConstant of value: Expr * ref: EntityRef
    | NewOption of value: Expr option * typ: Type * isStruct: bool
    | NewArray of values: Expr list * typ: Type
    | NewArrayFrom of value: Expr * typ: Type
    | NewList of headAndTail: (Expr * Expr) option * typ: Type
    | NewTuple of values: Expr list * isStruct: bool
    | NewRecord of values: Expr list * ref: EntityRef * genArgs: Type list
    | NewAnonymousRecord of values: Expr list * fieldNames: string [] * genArgs: Type list
    | NewUnion of values: Expr list * tag: int * ref: EntityRef * genArgs: Type list
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
        | NumberConstant (_, kind, uom) -> Number(kind, uom)
        | RegexConstant _ -> Regex
        | EnumConstant (_, ent) -> Enum ent
        | NewOption (_, t, isStruct) -> Option(t, isStruct)
        | NewArray (_, t) -> Array t
        | NewArrayFrom (_, t) -> Array t
        | NewList (_, t) -> List t
        | NewTuple (exprs, isStruct) -> Tuple(exprs |> List.map (fun e -> e.Type), isStruct)
        | NewRecord (_, ent, genArgs) -> DeclaredType(ent, genArgs)
        | NewAnonymousRecord (_, fieldNames, genArgs) -> AnonymousRecordType(fieldNames, genArgs)
        | NewUnion (_, _, ent, genArgs) -> DeclaredType(ent, genArgs)

type ParamInfo =
    { Name: string option
      Type: Type }

type CallMemberInfo =
    { CurriedParameterGroups: ParamInfo list list
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
      IsConstructor: bool
      /// Tag that indicates the call can be optimized away after the AST transformation chain
      OptimizableInto: string option }
    static member Make(?thisArg: Expr,
                       ?args: Expr list,
                       ?sigArgTypes: Type list,
                       ?memberInfo: CallMemberInfo,
                       ?hasSpread: bool,
                       ?isCons: bool,
                       ?optimizable: string) =
        { ThisArg = thisArg
          Args = defaultArg args []
          SignatureArgTypes = defaultArg sigArgTypes []
          CallMemberInfo = memberInfo
          HasSpread = defaultArg hasSpread false
          IsConstructor = defaultArg isCons false
          OptimizableInto = optimizable }

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
      IsStatement: bool
      CallInfo: CallInfo }

type ImportKind =
   | UserImport of isInline: bool
   | LibraryImport
   | MemberImport of isInstance: bool * fullPath: string
   | ClassImport of fullPath: string

type ImportInfo =
    { Selector: string
      Path: string
      Kind: ImportKind }
    member this.IsCompilerGenerated =
        match this.Kind with
        | UserImport isInline -> isInline
        | LibraryImport | MemberImport _  | ClassImport _ -> true

type OperationKind =
    | Unary of operator: UnaryOperator * operand: Expr
    | Binary of operator: BinaryOperator * left: Expr * right: Expr
    | Logical of operator: LogicalOperator * left: Expr * right: Expr

type GetKind =
    | TupleIndex of index: int
    | ExprGet of expr: Expr
    | FieldGet of fieldName: string * isMutable: bool
    | UnionField of caseIndex: int * fieldIndex: int
    | UnionTag
    | ListHead
    | ListTail
    | OptionValue

type SetKind =
    | ExprSet of Expr
    | FieldSet of fieldName: string
    | ValueSet

type TestKind =
    | TypeTest of typ: Type
    | OptionTest of isSome: bool
    | ListTest of isCons: bool
    | UnionCaseTest of tag: int

type ExtendedSet =
    | Return of expr: Expr
    | Break of label: string option
    | Throw of expr: Expr * typ: Type
    | Debugger
    | Curry of expr: Expr * arity: int
    member this.Type =
        match this with
        | Return e -> e.Type
        | Throw(_,t) -> t
        | Break _ -> Unit
        | Debugger -> Unit
        /// Used in the uncurrying transformations, we'll try to remove the curried expressions
        /// with beta reduction but in some cases it may be necessary to do it at runtime
        | Curry (expr, _) -> expr.Type

type MemberRefInfo =
    {
        Name: string
        Path: string
        IsInstance: bool
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
    /// The extended set contains instructions that are not used in the first FSharp2Fable pass
    /// but later when making the AST closer to a C-like language
    | Extended of instruction: ExtendedSet * range: SourceLocation option

    /// Identifiers that reference another expression
    | IdentExpr of ident: Ident

    /// Common and literal values
    | Value of kind: ValueKind * range: SourceLocation option

    // Closures
    /// Lambdas are curried, they always have a single argument (which can be unit)
    | Lambda of arg: Ident * body: Expr * name: string option
    /// Delegates are uncurried functions, can have none or multiple arguments
    | Delegate of args: Ident list * body: Expr * name: string option //* TODO: isArrow: bool
    | ObjectExpr of members: MemberDecl list * typ: Type * baseCall: Expr option

    // Type cast and tests
    | TypeCast of expr: Expr * Type
    | Test of expr: Expr * kind: TestKind * range: SourceLocation option

    // Operations
    /// Calls to class/module members
    | Call of callee: Expr * info: CallInfo * typ: Type * range: SourceLocation option
    /// Application of arguments to a lambda (or delegate)
    | CurriedApply of applied: Expr * args: Expr list * typ: Type * range: SourceLocation option
    /// Operations that can be defined with native operators
    | Operation of kind: OperationKind * typ: Type * range: SourceLocation option

    // Imports and code emissions
    | Import of info: ImportInfo * typ: Type * range: SourceLocation option
    | Emit of info: EmitInfo * typ: Type * range: SourceLocation option

    // Pattern matching
    | DecisionTree of expr: Expr * targets: (Ident list * Expr) list
    | DecisionTreeSuccess of targetIndex: int * boundValues: Expr list * typ: Type

    // Getters, setters and bindings
    | Let of ident: Ident * value: Expr * body: Expr
    | LetRec of bindings: (Ident * Expr) list * body: Expr
    | Get of Expr * kind: GetKind * typ: Type * range: SourceLocation option
    | Set of Expr * kind: SetKind * typ: Type * value: Expr * range: SourceLocation option

    // Control flow
    | Sequential of exprs: Expr list
    | WhileLoop of guard: Expr * body: Expr * label: string option * range: SourceLocation option
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
        | Extended (kind, _) -> kind.Type
        | Call(_,_,t,_)
        | CurriedApply(_,_,t,_)
        | TypeCast (_, t)
        | Import (_, t, _)
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
        | TypeCast (e, _) -> e.Range
        | IdentExpr id -> id.Range
        | Extended(_,r)
        | Call(_,_,_,r)
        | CurriedApply(_,_,_,r)
        | Emit (_,_,r)
        | Import(_,_,r)
        | Value (_, r)
        | IfThenElse (_, _, _, r)
        | TryCatch (_, _, _, r)
        | Test (_, _, r)
        | Operation (_, _, r)
        | Get (_, _, _, r)
        | Set (_, _, _, _, r)
        | ForLoop (_,_,_,_,_,r)
        | WhileLoop (_,_,_,r) -> r

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
//             | UnionField(_,i) -> print e + "[" + string i + "]"
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
