namespace rec Fable.AST.Fable

open Fable.AST
open Fable.AST.Fable

exception FableError of string

[<RequireQualifiedAccess>]
module Tags =
    let empty: string list = []

    let (|Contains|_|) (key: string) (tags: string list) =
        if List.contains key tags then
            Some()
        else
            None

type EntityPath =
    | SourcePath of string
    | AssemblyPath of string
    /// Only used to reference entities in core assemblies without a path
    | CoreAssemblyName of string
    | PrecompiledLib of sourcePath: string * assemblyPath: string

type EntityRef =
    {
        FullName: string
        Path: EntityPath
    }

    member this.DisplayName =
        let name = this.FullName.Substring(this.FullName.LastIndexOf('.') + 1)

        match name.IndexOf('`') with
        | -1 -> name
        | i -> name.Substring(0, i)

    member this.SourcePath =
        match this.Path with
        | SourcePath p
        | PrecompiledLib(p, _) -> Some p
        | AssemblyPath _
        | CoreAssemblyName _ -> None

type Attribute =
    abstract Entity: EntityRef
    abstract ConstructorArgs: obj list

type MemberRefInfo =
    {
        IsInstance: bool
        CompiledName: string
        NonCurriedArgTypes: Type list option
        Attributes: Attribute seq
    }

type MemberRef =
    | MemberRef of declaringEntity: EntityRef * info: MemberRefInfo
    | GeneratedMemberRef of GeneratedMember

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
    abstract IsMeasure: bool
    abstract Constraints: Constraint list

type Parameter =
    abstract Attributes: Attribute seq
    abstract Name: string option
    abstract Type: Type
    abstract IsIn: bool
    abstract IsOut: bool
    abstract IsNamed: bool
    abstract IsOptional: bool
    abstract DefaultValue: Expr option

type AbstractSignature =
    abstract Name: string
    abstract DeclaringType: Type

type MemberFunctionOrValue =
    abstract DisplayName: string
    abstract CompiledName: string
    abstract FullName: string
    abstract Attributes: Attribute seq
    abstract HasSpread: bool
    abstract IsInline: bool
    abstract IsPublic: bool
    abstract IsPrivate: bool
    abstract IsInternal: bool
    abstract IsConstructor: bool
    abstract IsInstance: bool
    abstract IsExtension: bool
    abstract IsValue: bool
    abstract IsMutable: bool
    abstract IsGetter: bool
    abstract IsSetter: bool
    /// Indicates the member is a wrapper for a getter and/or setter,
    /// it evals to false for the actual getter/setter methods
    abstract IsProperty: bool
    abstract IsOverrideOrExplicitInterfaceImplementation: bool
    abstract IsDispatchSlot: bool
    abstract GenericParameters: GenericParam list
    abstract CurriedParameterGroups: Parameter list list
    abstract ReturnParameter: Parameter
    abstract DeclaringEntity: EntityRef option
    abstract ApparentEnclosingEntity: EntityRef option
    abstract ImplementedAbstractSignatures: AbstractSignature seq
    abstract XmlDoc: string option

type Entity =
    abstract Ref: EntityRef
    abstract DisplayName: string
    abstract CompiledName: string
    abstract FullName: string
    abstract DeclaringEntity: EntityRef option
    abstract Attributes: Attribute seq
    abstract BaseType: DeclaredType option
    abstract AllInterfaces: DeclaredType seq
    abstract DeclaredInterfaces: DeclaredType seq
    abstract GenericParameters: GenericParam list
    abstract MembersFunctionsAndValues: MemberFunctionOrValue seq
    abstract TryFindMember: MemberRefInfo -> MemberFunctionOrValue option
    abstract FSharpFields: Field list
    abstract UnionCases: UnionCase list
    abstract IsAbstractClass: bool
    abstract IsPublic: bool
    abstract IsPrivate: bool
    abstract IsInternal: bool
    abstract IsNamespace: bool
    abstract IsFSharpModule: bool
    abstract IsFSharpUnion: bool
    abstract IsFSharpRecord: bool
    abstract IsFSharpAbbreviation: bool
    abstract IsFSharpExceptionDeclaration: bool
    abstract IsValueType: bool
    abstract IsInterface: bool
    abstract IsMeasure: bool
    abstract IsByRef: bool
    abstract IsEnum: bool

[<RequireQualifiedAccess>]
type NumberInfo =
    | Empty
    | IsMeasure of fullname: string
    | IsEnum of ent: EntityRef

type Type =
    | Measure of fullname: string
    | MetaType
    | Any
    | Unit
    | Boolean
    | Char
    | String
    | Regex
    | Number of kind: NumberKind * info: NumberInfo
    | Option of genericArg: Type * isStruct: bool
    | Tuple of genericArgs: Type list * isStruct: bool
    | Array of genericArg: Type * kind: ArrayKind
    | List of genericArg: Type
    | LambdaType of argType: Type * returnType: Type
    | DelegateType of argTypes: Type list * returnType: Type
    | GenericParam of
        name: string *
        isMeasure: bool *
        constraints: Constraint list
    | DeclaredType of ref: EntityRef * genericArgs: Type list
    | AnonymousRecordType of
        fieldNames: string[] *
        genericArgs: Type list *
        isStruct: bool

    member this.Generics =
        match this with
        | Option(gen, _)
        | Array(gen, _)
        | List gen -> [ gen ]
        | LambdaType(argType, returnType) ->
            [
                argType
                returnType
            ]
        | DelegateType(argTypes, returnType) -> argTypes @ [ returnType ]
        | Tuple(gen, _) -> gen
        | DeclaredType(_, gen) -> gen
        | AnonymousRecordType(_, gen, _) -> gen
        // TODO: Check numbers with measure?
        | MetaType
        | Any
        | Unit
        | Boolean
        | Char
        | String
        | Regex
        | Number _
        | GenericParam _
        | Measure _ -> []

    member this.MapGenerics f =
        match this with
        | Option(gen, isStruct) -> Option(f gen, isStruct)
        | Array(gen, kind) -> Array(f gen, kind)
        | List gen -> List(f gen)
        | LambdaType(argType, returnType) -> LambdaType(f argType, f returnType)
        | DelegateType(argTypes, returnType) ->
            DelegateType(List.map f argTypes, f returnType)
        | Tuple(gen, isStruct) -> Tuple(List.map f gen, isStruct)
        | DeclaredType(e, gen) -> DeclaredType(e, List.map f gen)
        | AnonymousRecordType(e, gen, isStruct) ->
            AnonymousRecordType(e, List.map f gen, isStruct)
        | MetaType
        | Any
        | Unit
        | Boolean
        | Char
        | String
        | Regex
        | Number _
        | GenericParam _
        | Measure _ -> this

type GeneratedMemberInfo =
    {
        Name: string
        ParamTypes: Type list
        ReturnType: Type
        IsInstance: bool
        HasSpread: bool
        IsMutable: bool
        DeclaringEntity: EntityRef option
    }

type GeneratedMember =
    | GeneratedFunction of info: GeneratedMemberInfo
    | GeneratedValue of info: GeneratedMemberInfo
    | GeneratedGetter of info: GeneratedMemberInfo
    | GeneratedSetter of info: GeneratedMemberInfo

    static member Function
        (
            name,
            paramTypes,
            returnType,
            ?isInstance,
            ?hasSpread,
            ?entRef
        )
        =
        {
            Name = name
            ParamTypes = paramTypes
            ReturnType = returnType
            IsInstance = defaultArg isInstance true
            HasSpread = defaultArg hasSpread false
            IsMutable = false
            DeclaringEntity = entRef
        }
        |> GeneratedFunction
        |> GeneratedMemberRef

    static member Value(name, typ, ?isInstance, ?isMutable, ?entRef) =
        {
            Name = name
            ParamTypes = []
            ReturnType = typ
            IsInstance = defaultArg isInstance true
            IsMutable = defaultArg isMutable false
            HasSpread = false
            DeclaringEntity = entRef
        }
        |> GeneratedValue
        |> GeneratedMemberRef

    static member Getter(name, typ, ?isInstance, ?entRef) =
        {
            Name = name
            ParamTypes = []
            ReturnType = typ
            IsInstance = defaultArg isInstance true
            IsMutable = false
            HasSpread = false
            DeclaringEntity = entRef
        }
        |> GeneratedGetter
        |> GeneratedMemberRef

    static member Setter(name, typ, ?isInstance, ?entRef) =
        {
            Name = name
            ParamTypes = [ typ ]
            ReturnType = Unit
            IsInstance = defaultArg isInstance true
            IsMutable = false
            HasSpread = false
            DeclaringEntity = entRef
        }
        |> GeneratedSetter
        |> GeneratedMemberRef

    member this.Info =
        match this with
        | GeneratedFunction info -> info
        | GeneratedValue info -> info
        | GeneratedGetter info -> info
        | GeneratedSetter info -> info

    static member Param(typ, ?name) =
        { new Parameter with
            member _.Attributes = []
            member _.Name = name
            member _.Type = typ
            member _.IsIn = false
            member _.IsOut = false
            member _.IsNamed = false
            member _.IsOptional = false
            member _.DefaultValue = None
        }

    static member GenericParams(typ: Type) : GenericParam list =
        typ :: typ.Generics
        |> List.choose (
            function
            | GenericParam(name, isMeasure, constraints) ->
                { new GenericParam with
                    member _.Name = name
                    member _.IsMeasure = isMeasure
                    member _.Constraints = constraints
                }
                |> Some
            | _ -> None
        )

    interface MemberFunctionOrValue with
        member this.DeclaringEntity = this.Info.DeclaringEntity
        member this.DisplayName = this.Info.Name
        member this.CompiledName = this.Info.Name
        member this.FullName = this.Info.Name

        member this.GenericParameters =
            this.Info.ParamTypes
            |> List.collect (fun t -> GeneratedMember.GenericParams(t))

        member this.CurriedParameterGroups =
            [
                this.Info.ParamTypes
                |> List.mapi (fun i t -> GeneratedMember.Param(t, $"a{i}"))
            ]

        member this.ReturnParameter =
            GeneratedMember.Param(this.Info.ReturnType)

        member this.IsConstructor =
            this.Info.Name = ".ctor" || this.Info.Name = ".cctor"

        member this.IsInstance = this.Info.IsInstance
        member this.HasSpread = this.Info.HasSpread
        member this.IsMutable = this.Info.IsMutable

        member this.IsValue =
            match this with
            | GeneratedValue _ -> true
            | _ -> false

        member this.IsGetter =
            match this with
            | GeneratedGetter _ -> true
            | _ -> false

        member this.IsSetter =
            match this with
            | GeneratedSetter _ -> true
            | _ -> false

        member _.IsProperty = false
        member _.IsInline = false
        member _.IsPublic = true
        member _.IsPrivate = false
        member _.IsInternal = false
        member _.IsExtension = false
        member _.IsOverrideOrExplicitInterfaceImplementation = false
        member _.IsDispatchSlot = false
        member _.Attributes = []
        member _.ApparentEnclosingEntity = None
        member _.ImplementedAbstractSignatures = []
        member _.XmlDoc = None

type ObjectExprMember =
    {
        Name: string
        Args: Ident list
        Body: Expr
        MemberRef: MemberRef
        IsMangled: bool
    }

type MemberDecl =
    {
        Name: string
        Args: Ident list
        Body: Expr
        MemberRef: MemberRef
        IsMangled: bool
        ImplementedSignatureRef: MemberRef option
        UsedNames: Set<string>
        XmlDoc: string option
        Tags: string list
    }

type ClassDecl =
    {
        Name: string
        Entity: EntityRef
        Constructor: MemberDecl option
        BaseCall: Expr option
        AttachedMembers: MemberDecl list
        XmlDoc: string option
        Tags: string list
    }

type ActionDecl =
    {
        Body: Expr
        UsedNames: Set<string>
    }

type ModuleDecl =
    {
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
        | ModuleDeclaration d ->
            d.Members |> List.map (fun d -> d.UsedNames) |> Set.unionMany
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

    member _.UsedNamesInRootScope: Set<string> =
        defaultArg usedRootNames Set.empty

type Ident =
    {
        Name: string
        Type: Type
        IsMutable: bool
        IsThisArgument: bool
        IsCompilerGenerated: bool
        Range: SourceLocation option
    }

    member x.DisplayName =
        x.Range
        |> Option.bind (fun r -> r.DisplayName)
        |> Option.defaultValue x.Name

type NewArrayKind =
    | ArrayValues of values: Expr list
    | ArrayAlloc of size: Expr
    | ArrayFrom of expr: Expr

type ArrayKind =
    | ResizeArray
    | MutableArray
    | ImmutableArray

type ValueKind =
    // The AST from F# compiler is a bit inconsistent with ThisValue and BaseValue.
    // ThisValue only appears in constructors and not in instance members (where `this` is passed as first argument)
    // BaseValue can appear both in constructor and instance members (where they're associated to this arg)
    | ThisValue of typ: Type
    | BaseValue of boundIdent: Ident option * typ: Type
    | TypeInfo of typ: Type * tags: string list
    | Null of typ: Type
    | UnitConstant
    | BoolConstant of value: bool
    | CharConstant of value: char
    | StringConstant of value: string
    /// String interpolation with support for JS tagged templates
    /// String parts length should always be values.Length + 1
    | StringTemplate of
        tag: Expr option *
        parts: string list *
        values: Expr list
    | NumberConstant of value: obj * kind: NumberKind * info: NumberInfo
    | RegexConstant of source: string * flags: RegexFlag list
    | NewOption of value: Expr option * typ: Type * isStruct: bool
    | NewArray of newKind: NewArrayKind * typ: Type * kind: ArrayKind
    | NewList of headAndTail: (Expr * Expr) option * typ: Type
    | NewTuple of values: Expr list * isStruct: bool
    | NewRecord of values: Expr list * ref: EntityRef * genArgs: Type list
    | NewAnonymousRecord of
        values: Expr list *
        fieldNames: string[] *
        genArgs: Type list *
        isStruct: bool
    | NewUnion of
        values: Expr list *
        tag: int *
        ref: EntityRef *
        genArgs: Type list

    member this.Type =
        match this with
        | ThisValue t
        | BaseValue(_, t) -> t
        | TypeInfo _ -> MetaType
        | Null t -> t
        | UnitConstant -> Unit
        | BoolConstant _ -> Boolean
        | CharConstant _ -> Char
        | StringConstant _
        | StringTemplate _ -> String
        | NumberConstant(_, kind, info) -> Number(kind, info)
        | RegexConstant _ -> Regex
        | NewOption(_, t, isStruct) -> Option(t, isStruct)
        | NewArray(_, t, k) -> Array(t, k)
        | NewList(_, t) -> List t
        | NewTuple(exprs, isStruct) ->
            Tuple(exprs |> List.map (fun e -> e.Type), isStruct)
        | NewRecord(_, ent, genArgs) -> DeclaredType(ent, genArgs)
        | NewAnonymousRecord(_, fieldNames, genArgs, isStruct) ->
            AnonymousRecordType(fieldNames, genArgs, isStruct)
        | NewUnion(_, _, ent, genArgs) -> DeclaredType(ent, genArgs)

type CallInfo =
    {
        ThisArg: Expr option
        Args: Expr list
        /// Argument types as defined in the method signature, this may be slightly different to types of actual argument expressions.
        /// E.g.: signature accepts 'a->'b->'c (2-arity) but we pass int->int->int->int (3-arity)
        /// This is used for the uncurrying mechanism
        SignatureArgTypes: Type list
        GenericArgs: Type list
        MemberRef: MemberRef option
        Tags: string list
    }

    static member Create
        (
            ?thisArg: Expr,
            ?args: Expr list,
            ?genArgs: Type list,
            ?sigArgTypes: Type list,
            ?memberRef: MemberRef,
            ?isCons: bool,
            ?tag: string
        )
        =
        let tags = Option.toList tag

        {
            ThisArg = thisArg
            Args = defaultArg args []
            GenericArgs = defaultArg genArgs []
            SignatureArgTypes = defaultArg sigArgTypes []
            MemberRef = memberRef
            Tags =
                match isCons with
                | Some true -> "new" :: tags
                | Some false
                | None -> tags
        }

type ReplaceCallInfo =
    {
        CompiledName: string
        OverloadSuffix: string
        /// See ArgInfo.SignatureArgTypes
        SignatureArgTypes: Type list
        HasSpread: bool
        IsModuleValue: bool
        IsInterface: bool
        DeclaringEntityFullName: string
        GenericArgs: Type list
    }

type EmitInfo =
    {
        Macro: string
        IsStatement: bool
        CallInfo: CallInfo
    }

type LibraryImportInfo =
    {
        IsInstanceMember: bool
        IsModuleMember: bool
    }

    static member Create(?isInstanceMember, ?isModuleMember) =
        {
            IsInstanceMember = defaultArg isInstanceMember false
            IsModuleMember = defaultArg isModuleMember false
        }

type ImportKind =
    /// `isInline` is automatically set to true after applying the arguments of an inline function whose body
    /// is a user generated import, to allow patterns like the one in "importDefault works with getters when inlined" test
    | UserImport of isInline: bool
    | LibraryImport of info: LibraryImportInfo
    | MemberImport of memberRef: MemberRef
    | ClassImport of entRef: EntityRef

type ImportInfo =
    {
        Selector: string
        Path: string
        Kind: ImportKind
    }

    member this.IsCompilerGenerated =
        match this.Kind with
        | UserImport isInline -> isInline
        | LibraryImport _
        | MemberImport _
        | ClassImport _ -> true

type OperationKind =
    | Unary of operator: UnaryOperator * operand: Expr
    | Binary of operator: BinaryOperator * left: Expr * right: Expr
    | Logical of operator: LogicalOperator * left: Expr * right: Expr

type FieldInfo =
    {
        Name: string
        FieldType: Type option
        IsMutable: bool
        /// Indicates the field shouldn't be moved in beta reduction
        MaybeCalculated: bool
        Tags: string list
    }

    member this.CanHaveSideEffects = this.IsMutable || this.MaybeCalculated

    static member Create
        (
            name,
            ?fieldType: Type,
            ?isMutable: bool,
            ?maybeCalculated: bool,
            ?tag: string
        )
        =
        {
            Name = name
            FieldType = fieldType
            IsMutable = defaultArg isMutable false
            MaybeCalculated = defaultArg maybeCalculated false
            Tags = Option.toList tag
        }
        |> FieldGet

type UnionFieldInfo =
    {
        Entity: EntityRef
        GenericArgs: Type list
        CaseIndex: int
        FieldIndex: int
    }

    static member Create(entity, caseIndex, fieldIndex, ?genArgs) =
        {
            Entity = entity
            GenericArgs = defaultArg genArgs []
            CaseIndex = caseIndex
            FieldIndex = fieldIndex
        }
        |> UnionField

type GetKind =
    | TupleIndex of index: int
    | ExprGet of expr: Expr
    | FieldGet of info: FieldInfo
    | UnionField of info: UnionFieldInfo
    | UnionTag
    | ListHead
    | ListTail
    // TODO: Add isForced flag to distinguish between value accessed in pattern matching or not
    | OptionValue

type SetKind =
    | ExprSet of expr: Expr
    | FieldSet of fieldName: string
    | ValueSet

type TestKind =
    | TypeTest of typ: Type
    | OptionTest of isSome: bool
    | ListTest of isCons: bool
    | UnionCaseTest of tag: int

/// Instructions that are not coming from the F# AST but are used by Fable internally.
type ExtendedSet =
    | Throw of expr: Expr option * typ: Type
    | Debugger
    /// Used in the uncurrying transformations, we'll try to remove the curried expressions
    /// with beta reduction but in some cases it may be necessary to do it at runtime
    | Curry of expr: Expr * arity: int

    member this.Type =
        match this with
        | Throw(_, t) -> t
        | Curry(expr, _) -> expr.Type
        | Debugger -> Unit

type Witness =
    {
        TraitName: string
        IsInstance: bool
        FileName: string
        Expr: Expr
    }

    member this.ArgTypes =
        match this.Expr with
        | Delegate(args, _, _, _) -> args |> List.map (fun a -> a.Type)
        | _ -> []

/// Unresolved expressions are used in precompiled inline functions.
/// They will be resolved in the call site where the context is available.
type UnresolvedExpr =
    // TODO: Add also MemberKind from the flags?
    | UnresolvedTraitCall of
        sourceTypes: Type list *
        traitName: string *
        isInstance: bool *
        argTypes: Type list *
        argExprs: Expr list
    | UnresolvedReplaceCall of
        thisArg: Expr option *
        args: Expr list *
        info: ReplaceCallInfo *
        attachedCall: Expr option
    | UnresolvedInlineCall of
        memberUniqueName: string *
        witnesses: Witness list *
        callee: Expr option *
        info: CallInfo

type Expr =
    /// Identifiers that reference another expression
    | IdentExpr of ident: Ident

    /// Common and literal values
    | Value of kind: ValueKind * range: SourceLocation option

    // Closures
    /// Lambdas are curried, they always have a single argument (which can be unit)
    | Lambda of arg: Ident * body: Expr * name: string option
    /// Delegates are uncurried functions, can have none or multiple arguments
    | Delegate of
        args: Ident list *
        body: Expr *
        name: string option *
        tags: string list
    | ObjectExpr of
        members: ObjectExprMember list *
        typ: Type *
        baseCall: Expr option

    // Type cast and tests
    | TypeCast of expr: Expr * Type
    | Test of expr: Expr * kind: TestKind * range: SourceLocation option

    // Operations

    /// Call to a type/module member, function arguments will be uncurried
    | Call of
        callee: Expr *
        info: CallInfo *
        typ: Type *
        range: SourceLocation option
    /// Application to local lambdas, function arguments will NOT be uncurried
    | CurriedApply of
        applied: Expr *
        args: Expr list *
        typ: Type *
        range: SourceLocation option
    /// Operations that can be defined with native operators
    | Operation of
        kind: OperationKind *
        tags: string list *
        typ: Type *
        range: SourceLocation option

    // Imports and code emissions
    | Import of info: ImportInfo * typ: Type * range: SourceLocation option
    | Emit of info: EmitInfo * typ: Type * range: SourceLocation option

    // Pattern matching
    | DecisionTree of expr: Expr * targets: (Ident list * Expr) list
    | DecisionTreeSuccess of
        targetIndex: int *
        boundValues: Expr list *
        typ: Type

    // Getters, setters and bindings
    | Let of ident: Ident * value: Expr * body: Expr
    | LetRec of bindings: (Ident * Expr) list * body: Expr
    | Get of
        expr: Expr *
        kind: GetKind *
        typ: Type *
        range: SourceLocation option
    | Set of
        expr: Expr *
        kind: SetKind *
        typ: Type *
        value: Expr *
        range: SourceLocation option

    // Control flow
    | Sequential of exprs: Expr list
    | WhileLoop of guard: Expr * body: Expr * range: SourceLocation option
    | ForLoop of
        ident: Ident *
        start: Expr *
        limit: Expr *
        body: Expr *
        isUp: bool *
        range: SourceLocation option
    | TryCatch of
        body: Expr *
        catch: (Ident * Expr) option *
        finalizer: Expr option *
        range: SourceLocation option
    | IfThenElse of
        guardExpr: Expr *
        thenExpr: Expr *
        elseExpr: Expr *
        range: SourceLocation option

    | Unresolved of
        expr: UnresolvedExpr *
        typ: Type *
        range: SourceLocation option
    | Extended of expr: ExtendedSet * range: SourceLocation option

    member this.Type =
        match this with
        | Unresolved(_, t, _) -> t
        | Extended(kind, _) -> kind.Type
        | Test _ -> Boolean
        | Value(kind, _) -> kind.Type
        | IdentExpr id -> id.Type
        | Call(_, _, t, _)
        | CurriedApply(_, _, t, _)
        | TypeCast(_, t)
        | Import(_, t, _)
        | ObjectExpr(_, t, _)
        | Operation(_, _, t, _)
        | Get(_, _, t, _)
        | Emit(_, t, _)
        | DecisionTreeSuccess(_, _, t) -> t
        | Set _
        | WhileLoop _
        | ForLoop _ -> Unit
        | Sequential exprs ->
            List.tryLast exprs
            |> Option.map (fun e -> e.Type)
            |> Option.defaultValue Unit
        | Let(_, _, expr)
        | LetRec(_, expr)
        | TryCatch(expr, _, _, _)
        | IfThenElse(_, expr, _, _)
        | DecisionTree(expr, _) -> expr.Type
        | Lambda(arg, body, _) -> LambdaType(arg.Type, body.Type)
        | Delegate(args, body, _, _) ->
            DelegateType(args |> List.map (fun a -> a.Type), body.Type)

    member this.Range: SourceLocation option =
        match this with
        | Unresolved(_, _, r)
        | Extended(_, r) -> r
        | ObjectExpr _
        | Sequential _
        | Let _
        | LetRec _
        | DecisionTree _
        | DecisionTreeSuccess _ -> None
        | Lambda(_, e, _)
        | Delegate(_, e, _, _)
        | TypeCast(e, _) -> e.Range
        | IdentExpr id -> id.Range
        | Call(_, _, _, r)
        | CurriedApply(_, _, _, r)
        | Emit(_, _, r)
        | Import(_, _, r)
        | Value(_, r)
        | IfThenElse(_, _, _, r)
        | TryCatch(_, _, _, r)
        | Test(_, _, r)
        | Operation(_, _, _, r)
        | Get(_, _, _, r)
        | Set(_, _, _, _, r)
        | ForLoop(_, _, _, _, _, r)
        | WhileLoop(_, _, r) -> r

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

//     and printUnaryOp op e =
//         match op with
//         | UnaryMinus -> "-" + print e
//         | UnaryPlus -> "+" + print e
//         | UnaryNot -> "!" + print e
//         | UnaryNotBitwise -> "~" + print e
//         | UnaryAddressOf -> "&" + print e

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
//             | NewOption(Some e, _) -> "Some " + print e
//             | NewOption(None, _) -> "None"
//             | NewTuple exprs -> printMulti "(" exprs ")"
//             | NewArray(exprs, _) -> printMulti "[|" exprs "|]"
//             | NewArrayFrom(e, _) -> $"array({print e})"
//             | NewList(ht, _) -> "list"
//                 // match ht with Some(h,t) -> [h;t] | None -> []
//             | NewRecord(exprs, _, _) -> printMulti "{" exprs "}"
//             | NewAnonymousRecord(exprs, _, _, _) -> printMulti "{" exprs "}"
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
