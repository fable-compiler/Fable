namespace rec Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open Fable
open Fable.Core
open Fable.AST
open Fable.Transforms
open Fable.Transforms.FSharp2Fable

module Extensions =
    let areParamTypesEqual genArgs (args1: Fable.Type[]) (args2: IList<IList<FSharpParameter>>) =
        // Not entirely sure why, but it seems members with a single unit argument sometimes have this parameter
        // and sometimes none, so just to be sure always remove single unit arguments
        let args2 =
            if args2.Count = 1 && args2[0].Count = 1 && Helpers.isUnit args2[0].[0].Type then
                [||]
            else
                args2 |> Seq.concat |> Seq.toArray

        if args1.Length = args2.Length then
            let args2 = args2 |> Array.map (fun p -> TypeHelpers.makeType genArgs p.Type)

            Array.forall2 (typeEquals false) args1 args2
        else
            false

// type FSharpEntity with
//     member entity.EnumerateMembersFunctionsAndValues(?includeHierarchy: bool): FSharpMemberOrFunctionOrValue seq =
//         let ownMembers = entity.TryGetMembersFunctionsAndValues()
//         match includeHierarchy with
//         | Some true ->
//             match TypeHelpers.tryGetBaseEntity entity with
//             | Some(baseDef, _) ->
//                 Seq.append ownMembers (baseDef.EnumerateMembersFunctionsAndValues(includeHierarchy=true))
//             | _ -> ownMembers
//         | _ -> ownMembers

//     member entity.TryFindMember(
//         compiledName: string,
//         isInstance: bool,
//         ?argTypes: Fable.Type[],
//         ?genArgs,
//         ?searchHierarchy: bool,
//         ?requireDispatchSlot: bool
//     ) =
//         let doNotRequireDispatchSlot = not(defaultArg requireDispatchSlot false)
//         let genArgs = defaultArg genArgs Map.empty
//         let argTypes =
//             // Remove single unit argument (see note in areParamTypesEqual above)
//             argTypes |> Option.map (function
//                 | [|Fable.Unit|] -> [||]
//                 | argTypes -> argTypes)

//         entity.EnumerateMembersFunctionsAndValues(?includeHierarchy=searchHierarchy)
//         |> Seq.tryFind (fun m2 ->
//             if m2.IsInstanceMember = isInstance
//                 && m2.CompiledName = compiledName
//                 && (doNotRequireDispatchSlot || m2.IsDispatchSlot) then
//                 match argTypes with
//                 | Some argTypes -> areParamTypesEqual genArgs argTypes m2.CurriedParameterGroups
//                 | None -> true
//             else false)

type FsField(fi: FSharpField) =
    let name = FsField.FSharpFieldName fi
    let typ = TypeHelpers.makeType Map.empty fi.FieldType

    interface Fable.Field with
        member _.Name = name
        member _.FieldType = typ
        member _.LiteralValue = fi.LiteralValue
        member _.IsStatic = fi.IsStatic
        member _.IsMutable = fi.IsMutable

    static member FSharpFieldName(fi: FSharpField) =
        let rec countConflictingCases acc (ent: FSharpEntity) (name: string) =
            match TypeHelpers.tryGetBaseEntity ent with
            | None -> acc
            | Some(baseClass, _) ->
                let conflicts = baseClass.FSharpFields |> Seq.exists (fun fi -> fi.Name = name)

                let acc =
                    if conflicts then
                        acc + 1
                    else
                        acc

                countConflictingCases acc baseClass name

        let name = fi.Name

        match fi.DeclaringEntity with
        | None -> name
        | Some ent when ent.IsFSharpRecord || ent.IsFSharpUnion -> name
        | Some ent ->
            match countConflictingCases 0 ent name with
            | 0 -> name
            | n -> name + "_" + (string<int> n)

[<RequireQualifiedAccess>]
type CompiledValue =
    | Integer of int
    | Float of float
    | Boolean of bool

type FsUnionCase(uci: FSharpUnionCase) =
    /// FSharpUnionCase.CompiledName doesn't give the value of CompiledNameAttribute
    /// We must check the attributes explicitly
    static member CompiledName(uci: FSharpUnionCase) =
        uci.Attributes
        |> Helpers.tryFindAttrib Atts.compiledName
        |> Option.map (fun (att: FSharpAttribute) -> att.ConstructorArguments[0] |> snd |> string<obj>)

    static member FullName(uci: FSharpUnionCase) =
        // proper full compiled name (instead of uci.FullName)
        uci.XmlDocSig
        |> Naming.replacePrefix "T:Microsoft.FSharp." "FSharp."
        |> Naming.replacePrefix "T:" ""

    static member CompiledValue(uci: FSharpUnionCase) =
        uci.Attributes
        |> Helpers.tryFindAttrib Atts.compiledValue
        |> Option.bind (fun (att: FSharpAttribute) ->
            match snd att.ConstructorArguments[0] with
            | :? int as value -> Some(CompiledValue.Integer value)
            | :? float as value -> Some(CompiledValue.Float value)
            | :? bool as value -> Some(CompiledValue.Boolean value)
            | :? Enum as value when Enum.GetUnderlyingType(value.GetType()) = typeof<int> ->
                Some(CompiledValue.Integer(box value :?> int))
            | _ -> None
        )

    // static member HasNamedFields(uci: FSharpUnionCase) =
    //     not (uci.Fields.Count = 1 && uci.Fields[0].Name = "Item")
    //     true

    interface Fable.UnionCase with
        member _.Name = uci.Name
        member _.FullName = FsUnionCase.FullName uci
        member _.CompiledName = FsUnionCase.CompiledName uci

        member _.UnionCaseFields = uci.Fields |> Seq.mapToList (fun x -> upcast FsField(x))

type FsAtt(att: FSharpAttribute) =
    interface Fable.Attribute with
        member _.Entity = FsEnt.Ref att.AttributeType
        member _.ConstructorArgs = att.ConstructorArguments |> Seq.mapToList snd

type FsGenParam(gen: FSharpGenericParameter) =
    interface Fable.GenericParam with
        member _.Name = TypeHelpers.genParamName gen
        member _.IsMeasure = gen.IsMeasure
        member _.Constraints = FsGenParam.Constraints gen

    static member Constraint(c: FSharpGenericParameterConstraint) =
        if c.IsCoercesToConstraint then
            // It seems sometimes there are circular references so skip the constraints here
            let t = TypeHelpers.makeTypeWithConstraints false Map.empty c.CoercesToTarget
            Fable.Constraint.CoercesTo t |> Some
        elif c.IsMemberConstraint then
            let d = c.MemberConstraintData // TODO: Full member signature hash?
            Fable.Constraint.HasMember(d.MemberName, d.MemberIsStatic) |> Some
        elif c.IsSupportsNullConstraint then
            Some Fable.Constraint.IsNullable
        elif c.IsNotSupportsNullConstraint then
            Some Fable.Constraint.IsNotNullable
        elif c.IsNonNullableValueTypeConstraint then
            Some Fable.Constraint.IsValueType
        elif c.IsReferenceTypeConstraint then
            Some Fable.Constraint.IsReferenceType
        elif c.IsRequiresDefaultConstructorConstraint then
            Some Fable.Constraint.HasDefaultConstructor
        elif c.IsAllowsRefStructConstraint then
            Some Fable.Constraint.HasAllowsRefStruct
        elif c.IsComparisonConstraint then
            Some Fable.Constraint.HasComparison
        elif c.IsEqualityConstraint then
            Some Fable.Constraint.HasEquality
        elif c.IsUnmanagedConstraint then
            Some Fable.Constraint.IsUnmanaged
        elif c.IsDelegateConstraint then
            let d = c.DelegateConstraintData

            let at =
                TypeHelpers.makeTypeWithConstraints false Map.empty d.DelegateTupledArgumentType

            let rt = TypeHelpers.makeTypeWithConstraints false Map.empty d.DelegateReturnType
            Fable.Constraint.IsDelegate(at, rt) |> Some
        elif c.IsEnumConstraint then
            let t = TypeHelpers.makeTypeWithConstraints false Map.empty c.EnumConstraintTarget
            Fable.Constraint.IsEnum t |> Some
        elif c.IsSimpleChoiceConstraint then
            let types =
                c.SimpleChoices
                |> Seq.map (TypeHelpers.makeTypeWithConstraints false Map.empty)
                |> Seq.toList

            Fable.Constraint.SimpleChoice types |> Some
        else
            None // TODO: Document these cases

    static member Constraints(gen: FSharpGenericParameter) =
        gen.Constraints |> Seq.chooseToList FsGenParam.Constraint

type FsParam(p: FSharpParameter, ?isNamed) =
    let isOptional = p.IsOptionalArg

    let defValue =
        if isOptional then
            p.Attributes
            |> Helpers.tryFindAttrib "System.Runtime.InteropServices.DefaultParameterValueAttribute"
            |> Option.bind (fun att ->
                Seq.tryHead att.ConstructorArguments
                |> Option.map (fun (t, v) ->
                    if isNull v then
                        TypeHelpers.makeType Map.empty t |> makeNullTyped
                    else
                        makeConstFromObj v
                )
            )
        else
            None

    interface Fable.Parameter with
        member _.Name = p.Name
        member _.Type = TypeHelpers.makeType Map.empty p.Type

        member _.Attributes = p.Attributes |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)

        member _.IsIn = p.IsInArg
        member _.IsOut = p.IsOutArg
        member _.IsNamed = defaultArg isNamed false
        member _.IsOptional = isOptional
        member _.DefaultValue = defValue

type FsDeclaredType(ent: FSharpEntity, genArgs: IList<FSharpType>) =
    interface Fable.DeclaredType with
        member _.Entity = FsEnt.Ref ent

        member _.GenericArgs = genArgs |> Seq.mapToList (TypeHelpers.makeType Map.empty)

type FsAbstractSignature(s: FSharpAbstractSignature) =
    interface Fable.AbstractSignature with
        member _.Name = s.Name
        member _.DeclaringType = TypeHelpers.makeType Map.empty s.DeclaringType

type FsMemberFunctionOrValue(m: FSharpMemberOrFunctionOrValue) =
    static member CompiledName(m: FSharpMemberOrFunctionOrValue) =
        if FsMemberFunctionOrValue.IsGetter(m) || FsMemberFunctionOrValue.IsSetter(m) then
            Naming.removeGetSetPrefix m.CompiledName
        else
            m.CompiledName

    static member DisplayName(m: FSharpMemberOrFunctionOrValue) =
        if FsMemberFunctionOrValue.IsGetter(m) || FsMemberFunctionOrValue.IsSetter(m) then
            Naming.removeGetSetPrefix m.DisplayNameCore
        else
            m.DisplayNameCore

    // We don't consider indexer properties as getters/setters so they're always compiled as methods
    static member IsGetter(m: FSharpMemberOrFunctionOrValue) =
        m.IsPropertyGetterMethod && Util.countNonCurriedParams m = 0

    static member IsSetter(m: FSharpMemberOrFunctionOrValue) =
        m.IsPropertySetterMethod && Util.countNonCurriedParams m = 1

    interface Fable.MemberFunctionOrValue with
        member _.Attributes = m.Attributes |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)

        member _.CurriedParameterGroups =
            let mutable i = -1

            let namedParamsIndex =
                m.Attributes
                |> Helpers.tryFindAttrib Atts.paramObject
                |> Option.map (fun (att: FSharpAttribute) ->
                    match Seq.tryItem 0 att.ConstructorArguments with
                    | Some(_, (:? int as index)) -> index
                    | _ -> 0
                )

            m.CurriedParameterGroups
            |> Seq.mapToList (
                Seq.mapToList (fun p ->
                    i <- i + 1

                    let isNamed =
                        match namedParamsIndex with
                        | Some namedParamsIndex -> i >= namedParamsIndex
                        | None -> false

                    FsParam(p, isNamed = isNamed) :> Fable.Parameter
                )
            )

        member _.HasSpread = Helpers.hasParamArray m
        member _.IsInline = Helpers.isInline m
        member _.IsPublic = Helpers.isNotPrivate m
        member _.IsPrivate = m.Accessibility.IsPrivate
        member _.IsInternal = m.Accessibility.IsInternal
        // Using memb.IsValue doesn't work for function values
        // (e.g. `let ADD = adder()` when adder returns a function)
        member _.IsValue = Helpers.isModuleValueForDeclarations m
        member _.IsDispatchSlot = m.IsDispatchSlot
        member _.IsConstructor = m.IsConstructor
        member _.IsInstance = m.IsInstanceMember
        member _.IsExtension = m.IsExtensionMember
        member _.IsMutable = m.IsMutable
        member _.IsProperty = m.IsProperty
        member _.IsGetter = FsMemberFunctionOrValue.IsGetter(m)
        member _.IsSetter = FsMemberFunctionOrValue.IsSetter(m)

        member _.IsOverrideOrExplicitInterfaceImplementation =
            m.IsOverrideOrExplicitInterfaceImplementation

        member _.DisplayName = FsMemberFunctionOrValue.DisplayName m
        member _.CompiledName = m.CompiledName
        member _.FullName = m.FullName

        member _.GenericParameters =
            m.GenericParameters |> Seq.mapToList (fun p -> FsGenParam(p))

        member _.ReturnParameter = FsParam(m.ReturnParameter) :> Fable.Parameter

        member _.ImplementedAbstractSignatures =
            m.ImplementedAbstractSignatures |> Seq.map (fun s -> FsAbstractSignature(s))

        member _.ApparentEnclosingEntity = FsEnt.Ref m.ApparentEnclosingEntity |> Some

        member _.DeclaringEntity = m.DeclaringEntity |> Option.map FsEnt.Ref
        member _.XmlDoc = TypeHelpers.tryGetXmlDoc m.XmlDoc

type FsEnt(maybeAbbrevEnt: FSharpEntity) =
    let ent = Helpers.nonAbbreviatedDefinition maybeAbbrevEnt

    let members = lazy (ent.TryGetMembersFunctionsAndValues())

    static let tryArrayFullName (ent: FSharpEntity) =
        if ent.IsArrayType then
            let rank =
                match ent.ArrayRank with
                | rank when rank > 1 -> "`" + string<int> rank
                | _ -> ""

            Some("System.Array" + rank)
        else
            None

    member _.FSharpEntity = ent

    static member SourcePath(ent: FSharpEntity) =
        let ent = Helpers.nonAbbreviatedDefinition ent

        ent.DeclarationLocation.FileName |> Path.normalizePathAndEnsureFsExtension

    static member FullName(ent: FSharpEntity) : string =
        let ent = Helpers.nonAbbreviatedDefinition ent

        match tryArrayFullName ent with
        | Some fullName -> fullName
        | None when ent.IsNamespace || ent.IsByRef ->
            match ent.Namespace with
            | Some ns -> ns + "." + ent.CompiledName
            | None -> ent.CompiledName
#if !FABLE_COMPILER
        | None when ent.IsProvided -> ent.LogicalName
#endif
        | None ->
            match ent.TryFullName with
            | Some n -> n
            | None -> ent.LogicalName

    static member Ref(ent: FSharpEntity) : Fable.EntityRef =
        let ent = Helpers.nonAbbreviatedDefinition ent

        let path =
            match ent.Assembly.FileName with
            | Some asmPath ->
                let dllName = Path.GetFileName(asmPath)
                let dllName = dllName.Substring(0, dllName.Length - 4) // Remove .dll extension

                match dllName with
                // When compiling with netcoreapp target, netstandard only contains redirects
                // We can find the actual assembly name from the entity qualified name
                | "netstandard" -> ent.QualifiedName.Split(',').[1].Trim() |> Fable.CoreAssemblyName
                | Naming.fablePrecompile ->
                    let sourcePath = FsEnt.SourcePath ent
                    Fable.PrecompiledLib(sourcePath, Path.normalizePath asmPath)
                | dllName when Compiler.CoreAssemblyNames.Contains(dllName) -> Fable.CoreAssemblyName dllName
                | _ -> Path.normalizePath asmPath |> Fable.AssemblyPath
            | None -> FsEnt.SourcePath ent |> Fable.SourcePath

        {
            FullName = FsEnt.FullName ent
            Path = path
        }

    member _.TryFindMember
        (
            compiledName: string,
            isInstance: bool,
            ?argTypes: Fable.Type[],
            ?genArgs,
            // ?searchHierarchy: bool,
            ?requireDispatchSlot: bool
        )
        =
        let doNotRequireDispatchSlot = not (defaultArg requireDispatchSlot false)

        let genArgs = defaultArg genArgs Map.empty

        let argTypes =
            // Remove single unit argument (see note in areParamTypesEqual above)
            argTypes
            |> Option.map (
                function
                | [| Fable.Unit |] -> [||]
                | argTypes -> argTypes
            )

        // entity.EnumerateMembersFunctionsAndValues(?includeHierarchy=searchHierarchy)
        members.Force()
        |> Seq.tryFind (fun m ->
            if
                m.CompiledName = compiledName
                && m.IsInstanceMember = isInstance
                && (doNotRequireDispatchSlot || m.IsDispatchSlot)
            then
                match argTypes with
                | Some argTypes -> Extensions.areParamTypesEqual genArgs argTypes m.CurriedParameterGroups
                | None -> true
            else
                false
        )

    interface Fable.Entity with
        member _.Ref = FsEnt.Ref ent
        member _.DisplayName = ent.DisplayName
        member _.CompiledName = ent.CompiledName
        member _.FullName = FsEnt.FullName ent

        member _.DeclaringEntity = ent.DeclaringEntity |> Option.map FsEnt.Ref

        member _.BaseType =
            match TypeHelpers.tryGetBaseEntity ent with
            | Some(baseEntity, baseGenArgs) -> Some(upcast FsDeclaredType(baseEntity, baseGenArgs))
            | _ -> None

        member _.Attributes = ent.Attributes |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)

        member _.MembersFunctionsAndValues =
            members.Force() |> Seq.map (fun m -> FsMemberFunctionOrValue(m))

        member x.TryFindMember(info: Fable.MemberRefInfo) =
            x.TryFindMember(
                info.CompiledName,
                isInstance = info.IsInstance,
                ?argTypes = (Option.map List.toArray info.NonCurriedArgTypes)
            )
            |> Option.map (fun m -> FsMemberFunctionOrValue(m))

        member _.AllInterfaces =
            ent.AllInterfaces
            |> Seq.choose (fun ifc ->
                if ifc.HasTypeDefinition then
                    Some(upcast FsDeclaredType(ifc.TypeDefinition, ifc.GenericArguments))
                else
                    None
            )

        member _.DeclaredInterfaces =
            ent.DeclaredInterfaces
            |> Seq.choose (fun ifc ->
                if ifc.HasTypeDefinition then
                    Some(upcast FsDeclaredType(ifc.TypeDefinition, ifc.GenericArguments))
                else
                    None
            )

        member _.GenericParameters =
            ent.GenericParameters |> Seq.mapToList (fun p -> FsGenParam(p))

        member _.FSharpFields =
            ent.FSharpFields |> Seq.mapToList (fun x -> FsField(x) :> Fable.Field)

        member _.UnionCases =
            ent.UnionCases |> Seq.mapToList (fun x -> FsUnionCase(x) :> Fable.UnionCase)

        member _.IsPublic =
            not (ent.Accessibility.IsPrivate || Helpers.typeIsHiddenBySignatureFile ent)

        member _.IsPrivate =
            ent.Accessibility.IsPrivate || Helpers.typeIsHiddenBySignatureFile ent

        member _.IsInternal = ent.Accessibility.IsInternal
        member _.IsAbstractClass = ent.IsAbstractClass
        member _.IsNamespace = ent.IsNamespace
        member _.IsFSharpModule = ent.IsFSharpModule
        member _.IsFSharpUnion = ent.IsFSharpUnion
        member _.IsFSharpRecord = ent.IsFSharpRecord
        member _.IsFSharpAbbreviation = maybeAbbrevEnt.IsFSharpAbbreviation
        member _.IsFSharpExceptionDeclaration = ent.IsFSharpExceptionDeclaration
        member _.IsValueType = ent.IsValueType
        member _.IsInterface = ent.IsInterface
        member _.IsMeasure = ent.IsMeasure
        member _.IsByRef = ent.IsByRef
        member _.IsEnum = ent.IsEnum

type Scope = (FSharpMemberOrFunctionOrValue option * Fable.Ident * Fable.Expr option) list

type Context =
    {
        Scope: Scope
        ScopeInlineValues: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
        UsedNamesInRootScope: Set<string>
        UsedNamesInDeclarationScope: HashSet<string>
        CapturedBindings: HashSet<string>
        GenericArgs: Map<string, Fable.Type>
        EnclosingMember: FSharpMemberOrFunctionOrValue option
        PrecompilingInlineFunction: FSharpMemberOrFunctionOrValue option
        CaughtException: Fable.Ident option
        BoundConstructorThis: Fable.Ident option
        BoundMemberThis: Fable.Ident option
        InlinePath: Log.InlinePath list
        CaptureBaseConsCall: (FSharpEntity * (Fable.Expr -> unit)) option
        Witnesses: Fable.Witness list
    }

    static member Create(?usedRootNames) =
        {
            Scope = []
            ScopeInlineValues = []
            UsedNamesInRootScope = defaultArg usedRootNames Set.empty
            UsedNamesInDeclarationScope = Unchecked.defaultof<_>
            CapturedBindings = Unchecked.defaultof<_>
            GenericArgs = Map.empty
            EnclosingMember = None
            PrecompilingInlineFunction = None
            CaughtException = None
            BoundConstructorThis = None
            BoundMemberThis = None
            InlinePath = []
            CaptureBaseConsCall = None
            Witnesses = []
        }

type IFableCompiler =
    inherit Compiler
    abstract Transform: Context * FSharpExpr -> Fable.Expr

    abstract ResolveInlineExpr: Context * InlineExpr * Fable.Expr list -> (Fable.Ident * Fable.Expr) list * Fable.Expr

    abstract TryReplace:
        Context *
        SourceLocation option *
        Fable.Type *
        info: Fable.ReplaceCallInfo *
        thisArg: Fable.Expr option *
        args: Fable.Expr list ->
            Fable.Expr option

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Helpers =

    let rec nonAbbreviatedDefinition (ent: FSharpEntity) : FSharpEntity =
        if ent.IsFSharpAbbreviation then
            let t = ent.AbbreviatedType

            if t.HasTypeDefinition && t.TypeDefinition <> ent then
                nonAbbreviatedDefinition t.TypeDefinition
            else
                ent
        else
            ent

    let rec nonAbbreviatedType (t: FSharpType) : FSharpType =
        let isSameType (t1: FSharpType) (t2: FSharpType) =
            t1.HasTypeDefinition
            && t2.HasTypeDefinition
            && (t1.TypeDefinition = t2.TypeDefinition)

        if t.IsAbbreviation && not (isSameType t t.AbbreviatedType) then
            nonAbbreviatedType t.AbbreviatedType
        elif t.HasTypeDefinition then
            let abbr = t.AbbreviatedType
            // .IsAbbreviation doesn't eval to true for generic numbers
            // See https://github.com/Microsoft/visualfsharp/issues/5992
            if t.GenericArguments.Count = abbr.GenericArguments.Count then
                t
            else
                abbr
        else
            t

    let getGenericArguments (t: FSharpType) =
        // Accessing .GenericArguments for a generic parameter will fail
        if t.IsGenericParameter then
            [||] :> IList<_>
        else
            (nonAbbreviatedType t).GenericArguments

    type TrimRootModule =
        | TrimRootModule of Compiler
        | NoTrimRootModule

    let private getEntityMangledName trimRootModule (ent: Fable.EntityRef) =
        let fullName = ent.FullName

        match trimRootModule, ent.Path with
        | TrimRootModule com, (Fable.SourcePath sourcePath | Fable.PrecompiledLib(sourcePath, _)) ->
            let rootMod = com.GetRootModule(sourcePath)

            if fullName.StartsWith(rootMod, StringComparison.Ordinal) then
                fullName.Substring(rootMod.Length).TrimStart('.')
            else
                fullName
        // Ignore entities for which we don't have implementation file data
        | TrimRootModule _, (Fable.AssemblyPath _ | Fable.CoreAssemblyName _)
        | NoTrimRootModule, _ -> fullName

    let cleanNameAsJsIdentifier (name: string) =
        if name = ".ctor" then
            "$ctor"
        else
            name.Replace('.', '_').Replace('`', '$')

    let cleanNameAsRustIdentifier (name: string) =
        let name =
            if name.Length > 0 && Char.IsDigit(name, 0) then
                "_" + name
            else
                name

        if name |> String.exists (fun c -> not (c = '_' || Char.IsLetterOrDigit(c))) then
            name
            |> String.collect (
                function
                | '_'
                | ' '
                | '`'
                | '.'
                | '\''
                | '\"' -> "_"
                | c when Char.IsLetterOrDigit(c) -> string c
                | c -> String.Format(@"_{0:x4}", int c)
            )
        else
            name

    let memberNameAsRustIdentifier (name: string) part =
        let f = cleanNameAsRustIdentifier

        let join sep s o =
            (f s)
            + (if o = "" then
                   ""
               else
                   sep + o)

        match part with
        | Naming.InstanceMemberPart(s, o) -> join "_" s o, Naming.NoMemberPart
        | Naming.StaticMemberPart(s, o) -> join "__" s o, Naming.NoMemberPart
        | Naming.NoMemberPart -> f name, part.Replace(f)

    let getEntityDeclarationName (com: Compiler) (entRef: Fable.EntityRef) =
        let entityName = getEntityMangledName (TrimRootModule com) entRef

        let name, part = (entityName |> cleanNameAsJsIdentifier, Naming.NoMemberPart)

        let sanitizedName =
            match com.Options.Language with
            | Python -> Fable.Py.Naming.sanitizeIdent Fable.Py.Naming.pyBuiltins.Contains name part
            | Rust -> entityName |> cleanNameAsRustIdentifier
            | _ -> Naming.sanitizeIdent (fun _ -> false) name part

        sanitizedName

    let getOverloadSuffixFrom (ent: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        match ent.CompiledName with
        // HACK for compiling FSharpMap/FSharpSet in fable-library
        | "FSharpMap"
        | "FSharpSet" -> ""
        | _ ->
            let entGenParams = ent.GenericParameters |> Seq.mapToList TypeHelpers.genParamName

            memb.CurriedParameterGroups
            |> Seq.mapToList (Seq.mapToList (fun p -> TypeHelpers.makeType Map.empty p.Type))
            |> OverloadSuffix.getHash entGenParams

    let private getMemberMangledName trimRootModule (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsExtensionMember then
            let overloadSuffix =
                memb.CurriedParameterGroups
                |> Seq.mapToList (Seq.mapToList (fun p -> TypeHelpers.makeType Map.empty p.Type))
                |> OverloadSuffix.getExtensionHash

            let entName =
                FsEnt.Ref memb.ApparentEnclosingEntity |> getEntityMangledName NoTrimRootModule

            entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
        else
            match memb.DeclaringEntity with
            | Some ent ->
                let entRef = FsEnt.Ref ent
                let entName = getEntityMangledName trimRootModule entRef

                if ent.IsFSharpModule then
                    match trimRootModule, entName with
                    | TrimRootModule com, _ when com.Options.Language = Rust -> memb.CompiledName, Naming.NoMemberPart // module prefix for Rust
                    | _, "" -> memb.CompiledName, Naming.NoMemberPart
                    | _, moduleName -> moduleName, Naming.StaticMemberPart(memb.CompiledName, "")
                else
                    let overloadSuffix = getOverloadSuffixFrom ent memb

                    if memb.IsInstanceMember then
                        entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
                    else
                        // Special case of non-mangled static classes to easily expose methods with optional args, etc, to native code
                        // TODO: If entity is not mangled and Erase attribute is not present, raise warning
                        match Util.tryMangleAttribute ent.Attributes with
                        | Some false -> memb.CompiledName, Naming.NoMemberPart
                        | Some true
                        | None -> entName, Naming.StaticMemberPart(memb.CompiledName, overloadSuffix)
            | None -> memb.CompiledName, Naming.NoMemberPart

    /// Returns the sanitized name for the member declaration and whether it has an overload suffix
    let getMemberDeclarationName (com: Compiler) (memb: FSharpMemberOrFunctionOrValue) =
        let name, part = getMemberMangledName (TrimRootModule com) memb

        let name, part =
            match com.Options.Language, memb.DeclaringEntity with
            | Rust, Some ent when memb.IsExtensionMember ->
                // For Rust, add entity prefix to extension methods
                cleanNameAsRustIdentifier name, part.Replace(cleanNameAsRustIdentifier)
            | Rust, Some ent when ent.IsInterface && not memb.IsDispatchSlot ->
                // For Rust, add entity prefix to default static interface members
                cleanNameAsRustIdentifier name, part.Replace(cleanNameAsRustIdentifier)
            | Rust, _ ->
                // for Rust, no entity prefix for other members
                memberNameAsRustIdentifier name part
            | _ -> cleanNameAsJsIdentifier name, part.Replace(cleanNameAsJsIdentifier)

        let sanitizedName =
            match com.Options.Language with
            | Python ->
                let name =
                    // Don't snake_case if member has compiled name attribute
                    match memb.Attributes |> Helpers.tryFindAttrib Atts.compiledName with
                    | Some _ -> name
                    | _ -> Fable.Py.Naming.toSnakeCase name

                Fable.Py.Naming.sanitizeIdent Fable.Py.Naming.pyBuiltins.Contains name part
            | Rust -> Naming.buildNameWithoutSanitation name part
            | _ -> Naming.sanitizeIdent (fun _ -> false) name part

        let hasOverloadSuffix = not (String.IsNullOrEmpty(part.OverloadSuffix))
        sanitizedName, hasOverloadSuffix

    /// Used to identify members uniquely in the inline expressions dictionary
    let getMemberUniqueName (memb: FSharpMemberOrFunctionOrValue) : string =
        getMemberMangledName NoTrimRootModule memb ||> Naming.buildNameWithoutSanitation

    let getMemberDisplayName (memb: FSharpMemberOrFunctionOrValue) =
        FsMemberFunctionOrValue.DisplayName memb

    let isUsedName (ctx: Context) name =
        ctx.UsedNamesInRootScope.Contains name
        || ctx.UsedNamesInDeclarationScope.Contains name

    let getIdentUniqueName (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (isUsedName ctx)

        ctx.UsedNamesInDeclarationScope.Add(name) |> ignore
        name

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ

        if typ.HasTypeDefinition then
            typ.TypeDefinition.TryFullName = Some Types.unit
        else
            false

    let isByRefType (typ: FSharpType) =
        typ.HasTypeDefinition && typ.TypeDefinition.IsByRef
    // && ( typ.TypeDefinition.DisplayName = "byref" ||
    //      typ.TypeDefinition.DisplayName = "inref" ||
    //      typ.TypeDefinition.DisplayName = "outref")

    let isByRefValue (value: FSharpMemberOrFunctionOrValue) =
        // Value type "this" is passed as inref, so it has to be excluded
        // (Note: the non-abbreviated type of inref and outref is byref)
        value.IsValue && not value.IsMemberThisValue && isByRefType value.FullType

    let tryFindAttrib fullName (atts: FSharpAttribute seq) =
        atts
        |> Seq.tryPick (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some fullName2 ->
                if fullName = fullName2 then
                    Some att
                else
                    None
            | None -> None
        )

    let hasAttrib attFullName (attributes: FSharpAttribute seq) =
        attributes
        |> Seq.exists (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some attFullName2 -> attFullName = attFullName2
            | None -> false
        )

    let tryPickAttrib attFullNames (attributes: FSharpAttribute seq) =
        let attFullNames = Map attFullNames

        attributes
        |> Seq.tryPick (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some fullName -> Map.tryFind fullName attFullNames
            | None -> None
        )

    let tryAttribConsArg (att: FSharpAttribute) index (defValue: 'T) (f: obj -> 'T option) =
        let consArgs = att.ConstructorArguments

        if consArgs.Count <= index then
            defValue
        else
            consArgs[index] |> snd |> f |> Option.defaultValue defValue

    let tryBoolean: obj -> bool option =
        function
        | (:? bool as x) -> Some x
        | _ -> None

    let tryString: obj -> string option =
        function
        | (:? string as x) -> Some x
        | _ -> None

    let tryDefinition (typ: FSharpType) =
        let typ = nonAbbreviatedType typ

        if typ.HasTypeDefinition then
            let tdef = typ.TypeDefinition
            Some(tdef, tdef.TryFullName)
        else
            None

    let getFsTypeFullName (typ: FSharpType) =
        match tryDefinition typ with
        | Some(_, Some fullName) -> fullName
        | _ -> Naming.unknown

    let isInline (memb: FSharpMemberOrFunctionOrValue) =
        match memb.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.AlwaysInline
        | FSharpInlineAnnotation.AggressiveInline -> true

    let hasOwnSignatureFile (ent: FSharpEntity) =
        not ent.IsNamespace
        && (
            match ent.SignatureLocation with
            | None -> false
            | Some m -> m.FileName.EndsWith(".fsi", StringComparison.Ordinal)
        )

    let parentHasSignatureFile (declaringEntity: FSharpEntity option) =
        declaringEntity
        |> Option.map (fun ent -> hasOwnSignatureFile ent)
        |> Option.defaultValue false

    let topLevelBindingHiddenBySignatureFile (v: FSharpMemberOrFunctionOrValue) =
        v.IsModuleValueOrMember
        && not v.HasSignatureFile
        && parentHasSignatureFile v.DeclaringEntity

    let typeIsHiddenBySignatureFile (ent: FSharpEntity) =
        not (hasOwnSignatureFile ent) && parentHasSignatureFile ent.DeclaringEntity

    let isNotPrivate (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated then
            false
        elif topLevelBindingHiddenBySignatureFile memb then
            false
        else
            not memb.Accessibility.IsPrivate

    let isPublic (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated then
            false
        else
            memb.Accessibility.IsPublic

    let makeRange (r: Range) =
        SourceLocation.Create(
            start =
                {
                    line = r.StartLine
                    column = r.StartColumn
                },
            ``end`` =
                {
                    line = r.EndLine
                    column = r.EndColumn
                },
            file = r.FileName
        )

    let makeRangeFrom (fsExpr: FSharpExpr) = Some(makeRange fsExpr.Range)

    let unionCaseTag (com: IFableCompiler) (ent: FSharpEntity) (unionCase: FSharpUnionCase) =
        try
            // If the order of cases changes in the declaration, the tag has to change too.
            // Mark all files using the case tag as watch dependencies.
            com.AddWatchDependency(FsEnt.SourcePath ent)

            ent.UnionCases |> Seq.findIndex (fun uci -> unionCase.Name = uci.Name)
        with _ ->
            failwith $"Cannot find case %s{unionCase.Name} in %s{FsEnt.FullName ent}"

    /// Apply case rules to case name if there's no explicit compiled name
    let transformStringEnum (rule: CaseRules) (unionCase: FSharpUnionCase) =
        match FsUnionCase.CompiledName unionCase with
        | Some name -> name
        | None -> Naming.applyCaseRule rule unionCase.Name
        |> makeStrConst

    // let isModuleMember (memb: FSharpMemberOrFunctionOrValue) =
    //     match memb.DeclaringEntity with
    //     | Some ent -> ent.IsFSharpModule
    //     | None -> true // Compiler-generated members

    /// Using memb.IsValue doesn't work for function values
    /// (e.g. `let ADD = adder()` when adder returns a function)
    let isModuleValueForDeclarations (memb: FSharpMemberOrFunctionOrValue) =
        memb.CurriedParameterGroups.Count = 0 && memb.GenericParameters.Count = 0

    // Mutable public values must be called as functions in JS (see #986)
    let isModuleValueCompiledAsFunction (com: Compiler) (memb: FSharpMemberOrFunctionOrValue) =
        match com.Options.Language with
        | Python
        | JavaScript
        | TypeScript -> memb.IsMutable && isNotPrivate memb
        | Rust -> true // always
        | Php
        | Dart -> false

    let isModuleValueForCalls com (declaringEntity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        declaringEntity.IsFSharpModule
        && isModuleValueForDeclarations memb
        && not (isModuleValueCompiledAsFunction com memb)

    let rec getAllInterfaceMembers (ent: FSharpEntity) =
        seq {
            yield! ent.MembersFunctionsAndValues

            for parent in ent.DeclaredInterfaces do
                match tryDefinition parent with
                | Some(e, _) -> yield! getAllInterfaceMembers e
                | None -> ()
        }

    /// Test if the name corresponds to this interface or anyone in its hierarchy
    let rec testInterfaceHierarchy interfaceFullName interfaceType =
        match tryDefinition interfaceType with
        | Some(e, Some fullName) ->
            if interfaceFullName = fullName then
                true
            else
                e.DeclaredInterfaces |> Seq.exists (testInterfaceHierarchy interfaceFullName)
        | _ -> false

    let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =

        let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
            if memb.CurriedParameterGroups.Count <> 1 then
                false
            else
                let args = memb.CurriedParameterGroups[0]
                args.Count > 0 && args[args.Count - 1].IsParamArrayArg

        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.map (fun lastParam -> hasAttrib Atts.paramList lastParam.Attributes)
            |> Option.defaultValue false

        hasParamArray memb || hasParamSeq memb

    type UnionPattern =
        | OptionUnion of typ: FSharpType * isStruct: bool
        | ListUnion of FSharpType
        | ErasedUnion of tdef: FSharpEntity * genArgs: IList<FSharpType> * rule: CaseRules
        | ErasedUnionCase
        | TypeScriptTaggedUnion of tdef: FSharpEntity * genArgs: IList<FSharpType> * tagName: string * rule: CaseRules
        | StringEnum of tdef: FSharpEntity * rule: CaseRules
        | DiscriminatedUnion of tdef: FSharpEntity * genArgs: IList<FSharpType>

    let getUnionPattern (typ: FSharpType) (unionCase: FSharpUnionCase) : UnionPattern =
        let typ = nonAbbreviatedType typ

        let getCaseRule (att: FSharpAttribute) =
            match Seq.tryHead att.ConstructorArguments with
            | Some(_, (:? int as rule)) -> enum<CaseRules> (rule)
            | _ -> CaseRules.LowerFirst

        unionCase.Attributes
        |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some Atts.erase -> Some ErasedUnionCase
            | _ -> None
        )
        |> Option.defaultWith (fun () ->
            match tryDefinition typ with
            | None -> failwith "Union without definition"
            | Some(tdef, fullName) ->
                match defaultArg fullName tdef.CompiledName with
                | Types.valueOption -> OptionUnion(typ.GenericArguments[0], true)
                | Types.option -> OptionUnion(typ.GenericArguments[0], false)
                | Types.list -> ListUnion typ.GenericArguments[0]
                | _ ->
                    tdef.Attributes
                    |> Seq.tryPick (fun att ->
                        match att.AttributeType.TryFullName with
                        | Some Atts.erase -> Some(ErasedUnion(tdef, typ.GenericArguments, getCaseRule att))
                        | Some Atts.stringEnum -> Some(StringEnum(tdef, getCaseRule att))
                        | Some Atts.tsTaggedUnion ->
                            match Seq.tryItem 0 att.ConstructorArguments, Seq.tryItem 1 att.ConstructorArguments with
                            | Some(_, (:? string as name)), None ->
                                Some(TypeScriptTaggedUnion(tdef, typ.GenericArguments, name, CaseRules.LowerFirst))
                            | Some(_, (:? string as name)), Some(_, (:? int as rule)) ->
                                Some(TypeScriptTaggedUnion(tdef, typ.GenericArguments, name, enum<CaseRules> (rule)))
                            | _ -> failwith "Invalid TypeScriptTaggedUnion attribute"
                        | _ -> None
                    )
                    |> Option.defaultValue (DiscriminatedUnion(tdef, typ.GenericArguments))
        )

    let tryGetFieldTag (memb: FSharpMemberOrFunctionOrValue) =
        if Compiler.Language = Dart && hasAttrib Atts.dartIsConst memb.Attributes then
            Some "const"
        else
            None

module Patterns =
    open FSharpExprPatterns
    open Helpers

    let inline (|Rev|) x = List.rev x
    let inline (|AsArray|) x = Array.ofSeq x
    let inline (|LazyValue|) (x: Lazy<'T>) = x.Value
    let inline (|Transform|) (com: IFableCompiler) ctx e = com.Transform(ctx, e)
    let inline (|FieldName|) (fi: FSharpField) = fi.Name

    let (|CommonNamespace|_|) =
        function
        | (FSharpImplementationFileDeclaration.Entity(ent, subDecls)) :: restDecls when ent.IsNamespace ->
            let commonName = ent.CompiledName

            (Some subDecls, restDecls)
            ||> List.fold (fun acc decl ->
                match acc, decl with
                | (Some subDecls), (FSharpImplementationFileDeclaration.Entity(ent, subDecls2)) ->
                    if ent.CompiledName = commonName then
                        Some(subDecls @ subDecls2)
                    else
                        None
                | _ -> None
            )
            |> Option.map (fun subDecls -> ent, subDecls)
        | _ -> None

    let inline (|NonAbbreviatedType|) (t: FSharpType) = nonAbbreviatedType t

    let (|IgnoreAddressOf|) (expr: FSharpExpr) =
        match expr with
        | AddressOf value -> value
        | _ -> expr

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then
            Some t.TypeDefinition
        else
            None

    /// DOES NOT check if the type is abbreviated, mainly intended to identify Fable.Core.Applicable
    let (|FSharpExprTypeFullName|_|) (e: FSharpExpr) =
        let t = e.Type

        if t.HasTypeDefinition then
            t.TypeDefinition.TryFullName
        else
            None

    let (|MemberFullName|) (memb: FSharpMemberOrFunctionOrValue) = memb.FullName

    let (|UnionCaseTesterFor|_|) (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        | Some ent when ent.IsFSharpUnion ->
            // if memb.IsUnionCaseTester then // insufficient, could be an interface member
            if
                memb.IsPropertyGetterMethod
                && not memb.IsDispatchSlot
                && not memb.IsOverrideOrExplicitInterfaceImplementation
                && memb.LogicalName.StartsWith("get_Is")
            then
                let unionCaseName = memb.LogicalName |> Naming.replacePrefix "get_Is" ""
                ent.UnionCases |> Seq.tryFind (fun uc -> uc.Name = unionCaseName)
            else
                None
        | _ -> None

    let (|RefType|_|) =
        function
        | TypeDefinition tdef as t when tdef.TryFullName = Some Types.refCell -> Some t
        | _ -> None

    /// Detects AST pattern of "raise MatchFailureException()"
    let (|RaisingMatchFailureExpr|_|) (expr: FSharpExpr) =
        match expr with
        | Call(None, methodInfo, [], [ _unitType ], [ value ]) ->
            match methodInfo.FullName with
            | "Microsoft.FSharp.Core.Operators.raise" ->
                match value with
                | NewRecord(recordType, [ Const(value, _valueT); _rangeFrom; _rangeTo ]) ->
                    match recordType.TypeDefinition.TryFullName with
                    | Some Types.matchFail -> Some(value.ToString())
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

    let (|NestedLambda|_|) x =
        let rec nestedLambda args =
            function
            | Lambda(arg, body) -> nestedLambda (arg :: args) body
            | body -> List.rev args, body

        match x with
        | Lambda(arg, body) -> nestedLambda [ arg ] body |> Some
        | _ -> None

    let (|ForOf|_|) =
        function
        | Let((_, value, _), // Coercion to seq
              Let((_, Call(None, meth, _, [], []), _), TryFinally(WhileLoop(_, Let((ident, _, _), body), _), _, _, _)))
        | Let((_, Call(Some value, meth, _, [], []), _), TryFinally(WhileLoop(_, Let((ident, _, _), body), _), _, _, _)) when
            // Using only the compiled name is riskier but with the fullName we miss some cases
            // TODO: Check the return type of meth is or implements IEnumerator
            meth.CompiledName = "GetEnumerator"
            ->
            // when meth.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" ->
            Some(ident, value, body)
        // optimized "for x in list"
        | Let((_, UnionCaseGet(value, typ, unionCase, field), _), WhileLoop(_, Let((ident, _, _), body), _)) when
            (getFsTypeFullName typ) = Types.list
            && unionCase.Name = "op_ColonColon"
            && field.Name = "Tail"
            ->
            Some(ident, value, body)
        // optimized "for _x in list"
        | Let((ident, UnionCaseGet(value, typ, unionCase, field), _), WhileLoop(_, body, _)) when
            (getFsTypeFullName typ) = Types.list
            && unionCase.Name = "op_ColonColon"
            && field.Name = "Tail"
            ->
            Some(ident, value, body)
        | _ -> None

    /// This matches the boilerplate generated for TryGetValue/TryParse/DivRem (see #154, or #1744)
    /// where the F# compiler automatically passes a byref arg and returns it as a tuple
    let (|ByrefArgToTuple|_|) =
        function
        | Let((outArg1, (DefaultValue _ as def), _),
              NewTuple(_, [ Call(callee, memb, ownerGenArgs, membGenArgs, callArgs); Value outArg3 ])) when
            List.isMultiple callArgs && outArg1.IsCompilerGenerated && outArg1 = outArg3
            ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some(callee, memb, ownerGenArgs, membGenArgs, callArgs @ [ def ])
            | _ -> None
        | _ -> None

    /// This matches the boilerplate generated for TryGetValue/TryParse/DivRem (--optimize+)
    let (|ByrefArgToTupleOptimizedIf|_|) =
        function
        | Let((outArg1, (DefaultValue _ as def), _),
              IfThenElse(Call(callee, memb, ownerGenArgs, membGenArgs, callArgs), thenExpr, elseExpr)) when
            List.isMultiple callArgs && outArg1.IsCompilerGenerated
            ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some(outArg1, callee, memb, ownerGenArgs, membGenArgs, callArgs @ [ def ], thenExpr, elseExpr)
            | _ -> None
        | _ -> None

    /// This matches another boilerplate generated for TryGetValue/TryParse/DivRem (--optimize+)
    let (|ByrefArgToTupleOptimizedTree|_|) =
        function
        | Let((outArg1, (DefaultValue _ as def), _),
              DecisionTree(IfThenElse(Call(callee, memb, ownerGenArgs, membGenArgs, callArgs), thenExpr, elseExpr),
                           targetsExpr)) when List.isMultiple callArgs && outArg1.IsCompilerGenerated ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some(
                    outArg1,
                    callee,
                    memb,
                    ownerGenArgs,
                    membGenArgs,
                    callArgs @ [ def ],
                    thenExpr,
                    elseExpr,
                    targetsExpr
                )
            | _ -> None
        | _ -> None

    /// This matches another boilerplate generated for TryGetValue/TryParse/DivRem (--crossoptimize-)
    let (|ByrefArgToTupleOptimizedLet|_|) =
        function
        | Let((outArg1, (DefaultValue _ as def), _),
              Let((arg_0, Call(callee, memb, ownerGenArgs, membGenArgs, callArgs), _), restExpr)) when
            List.isMultiple callArgs && outArg1.IsCompilerGenerated
            ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some(arg_0, outArg1, callee, memb, ownerGenArgs, membGenArgs, callArgs @ [ def ], restExpr)
            | _ -> None
        | _ -> None

    /// This matches the boilerplate generated to wrap .NET events from F#
    let (|CreateEvent|_|) =
        function
        | Call(None,
               createEvent,
               _,
               _,
               [ Lambda(_eventDelegate, Call(Some callee, addEvent, [], [], [ Value _eventDelegate' ]))
                 Lambda(_eventDelegate2, Call(Some _callee2, _removeEvent, [], [], [ Value _eventDelegate2' ]))
                 Lambda(_callback,
                        NewDelegate(_,
                                    Lambda(_delegateArg0,
                                           Lambda(_delegateArg1,
                                                  Application(Value _callback',
                                                              [],
                                                              [ Value _delegateArg0'; Value _delegateArg1' ]))))) ]) when
            createEvent.FullName = Types.createEvent
            ->
            let eventName = addEvent.CompiledName.Replace("add_", "")

            match addEvent.DeclaringEntity with
            | Some klass ->
                klass.MembersFunctionsAndValues
                |> Seq.tryFind (fun m -> m.LogicalName = eventName)
                |> function
                    | Some memb -> Some(callee, memb)
                    | _ -> None
            | _ -> None
        | _ -> None

    let (|ConstructorCall|_|) =
        function
        | NewObject(baseCall, genArgs, baseArgs) -> Some(baseCall, genArgs, baseArgs)
        | Call(None, baseCall, genArgs1, genArgs2, baseArgs) when baseCall.IsConstructor ->
            Some(baseCall, genArgs1 @ genArgs2, baseArgs)
        | _ -> None

    let (|OptimizedOperator|_|) (com: Compiler) fsExpr =
        if com.Options.OptimizeFSharpAst then
            match fsExpr with
            // work-around for optimized string operator (Operators.string)
            | Let((var, Call(None, memb, _, membArgTypes, membArgs), _),
                  DecisionTree(IfThenElse(_, _, IfThenElse(TypeTest(tt, Value vv), _, _)), _)) when
                var.FullName = "matchValue"
                && memb.FullName = "Microsoft.FSharp.Core.Operators.box"
                && vv.FullName = "matchValue"
                && (getFsTypeFullName tt) = "System.IFormattable"
                ->
                Some(memb, None, "toString", membArgTypes, membArgs)
            // work-around for optimized hash operator (Operators.hash)
            | Call(Some expr, memb, _, [], [ Call(None, comp, [], [], []) ]) when
                memb.FullName.EndsWith(".GetHashCode", StringComparison.Ordinal)
                && comp.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityERComparer"
                ->
                Some(memb, Some comp, "GenericHash", [ expr.Type ], [ expr ])
            // work-around for optimized equality operator (Operators.(=))
            | Call(Some e1, memb, _, [], [ Coerce(t2, e2); Call(None, comp, [], [], []) ]) when
                memb.FullName.EndsWith(".Equals", StringComparison.Ordinal)
                && t2.HasTypeDefinition
                && t2.TypeDefinition.CompiledName = "obj"
                && comp.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityComparer"
                ->
                Some(memb, Some comp, "GenericEquality", [ e1.Type; e2.Type ], [ e1; e2 ])
            | _ -> None
        else
            None

    let inline (|FableType|) _com (ctx: Context) t = TypeHelpers.makeType ctx.GenericArgs t

module TypeHelpers =
    open Helpers
    open Patterns

    let genParamName (genParam: FSharpGenericParameter) =
        // Sometimes the names of user-declared and compiler-generated clash, see #1900 and https://github.com/dotnet/fsharp/issues/13062
        let name = genParam.Name.Replace("?", "$")

        let name =
            if genParam.IsCompilerGenerated then
                "$" + name
            else
                name

        match Compiler.Language with
        // In Dart we cannot have the same generic name as a variable or argument, so we add $ to reduce the probabilities of conflict
        // Other solutions would be to add generic names to the name deduplication context or enforce Dart case conventions:
        // Pascal case for types and camel case for variables
        | Dart -> "$" + name
        | Rust -> genParam.Name
        | _ -> name

    let resolveGenParam withConstraints ctxTypeArgs (genParam: FSharpGenericParameter) =
        let name = genParamName genParam

        match Map.tryFind name ctxTypeArgs with
        | None ->
            let constraints =
                if withConstraints then
                    FsGenParam.Constraints genParam |> Seq.toList
                else
                    []

            Fable.GenericParam(name, genParam.IsMeasure, constraints)
        | Some typ -> typ

    let resolveTypeLambdaGenArgs (ctx: Context) genArgs lambda =
        match lambda with
        | FSharpExprPatterns.Lambda(arg, body) -> ctx // leave lambda context as is
        | _ ->
            // if not a lambda, resolve the type args not already in context to Fable.Any
            let newGenArgs = genArgs |> List.map (fun arg -> genParamName arg, Fable.Any)

            let newCtxGenArgs =
                (ctx.GenericArgs, newGenArgs)
                ||> List.fold (fun map (k, v) ->
                    if Map.containsKey k map then
                        map
                    else
                        Map.add k v map
                )

            { ctx with GenericArgs = newCtxGenArgs }

    // Filter measure generic arguments here? (for that we need to pass the compiler, which needs a bigger refactoring)
    // Currently for Dart we're doing it in the Fable2Dart step
    let makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs (genArgs: seq<FSharpType>) =
        genArgs
        |> Seq.mapToList (fun genArg ->
            if genArg.IsGenericParameter then
                resolveGenParam withConstraints ctxTypeArgs genArg.GenericParameter
            else
                makeTypeWithConstraints withConstraints ctxTypeArgs genArg
        )

    let makeTypeGenArgs ctxTypeArgs (genArgs: seq<FSharpType>) =
        makeTypeGenArgsWithConstraints true ctxTypeArgs genArgs

    let makeTypeFromDelegate withConstraints ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        let invokeArgs () =
            let invokeMember =
                tdef.MembersFunctionsAndValues |> Seq.find (fun f -> f.DisplayName = "Invoke")

            invokeMember.CurriedParameterGroups[0] |> Seq.map (fun p -> p.Type), invokeMember.ReturnParameter.Type

        let argTypes, returnType =
            try
                // tdef.FSharpDelegateSignature doesn't work with System.Func & friends
                if tdef.IsFSharp then
                    tdef.FSharpDelegateSignature.DelegateArguments |> Seq.map snd,
                    tdef.FSharpDelegateSignature.DelegateReturnType
                else
                    invokeArgs ()
            with _ ->
                invokeArgs ()

        let genArgs =
            Seq.zip (tdef.GenericParameters |> Seq.map genParamName) genArgs |> Map

        let resolveType (t: FSharpType) =
            if t.IsGenericParameter then
                Map.find (genParamName t.GenericParameter) genArgs
            else
                t

        let returnType =
            returnType |> resolveType |> makeTypeWithConstraints withConstraints ctxTypeArgs

        let argTypes =
            argTypes
            |> Seq.map (resolveType >> makeTypeWithConstraints withConstraints ctxTypeArgs)
            |> Seq.toList
            |> function
                | [ Fable.Unit ] -> []
                | argTypes -> argTypes

        Fable.DelegateType(argTypes, returnType)

    let numberTypes =
        dict
            [
                Types.int8, Int8
                Types.uint8, UInt8
                Types.int16, Int16
                Types.uint16, UInt16
                Types.int32, Int32
                Types.uint32, UInt32
                Types.int64, Int64
                Types.uint64, UInt64
                Types.int128, Int128
                Types.uint128, UInt128
                Types.nativeint, NativeInt
                Types.unativeint, UNativeInt
                Types.float16, Float16
                Types.float32, Float32
                Types.float64, Float64
                Types.decimal, Decimal
                Types.bigint, BigInt
            ]

    let numbersWithMeasure =
        dict
            [
                "Microsoft.FSharp.Core.sbyte`1", Int8
                "Microsoft.FSharp.Core.byte`1", UInt8
                "FSharp.UMX.byte`1", UInt8
                "Microsoft.FSharp.Core.int16`1", Int16
                "Microsoft.FSharp.Core.uint16`1", UInt16
                "Microsoft.FSharp.Core.int`1", Int32
                "Microsoft.FSharp.Core.uint`1", UInt32
                "Microsoft.FSharp.Core.int64`1", Int64
                "Microsoft.FSharp.Core.uint64`1", UInt64
                "FSharp.UMX.uint64`1", UInt64
                "Microsoft.FSharp.Core.nativeint`1", NativeInt
                "Microsoft.FSharp.Core.unativeint`1", UNativeInt
                "Microsoft.FSharp.Core.float32`1", Float32
                "Microsoft.FSharp.Core.float`1", Float64
                "Microsoft.FSharp.Core.decimal`1", Decimal
            ]

    // FCS doesn't expose the abbreviated type of a MeasureAnnotatedAbbreviation,
    // so we need to hard-code FSharp.UMX types
    let runtimeTypesWithMeasure =
        dict
            [
                "FSharp.UMX.bool`1", Choice1Of2 Fable.Boolean
                "FSharp.UMX.string`1", Choice1Of2 Fable.String
                "FSharp.UMX.Guid`1", Choice2Of2 Types.guid
                "FSharp.UMX.TimeSpan`1", Choice2Of2 Types.timespan
                "FSharp.UMX.TimeOnly`1", Choice2Of2 Types.timeOnly
                "FSharp.UMX.DateTime`1", Choice2Of2 Types.datetime
                "FSharp.UMX.DateTimeOffset`1", Choice2Of2 Types.datetimeOffset
                "FSharp.UMX.DateOnly`1", Choice2Of2 Types.dateOnly
            ]

    let private getMeasureFullName (genArgs: IList<FSharpType>) =
        if genArgs.Count > 0 then
            // TODO: Check it's effectively measure?
            // TODO: Raise error if we cannot get the measure fullName?
            match tryDefinition genArgs[0] with
            | Some(_, Some fullName) ->
                // Not sure why, but when precompiling F# changes measure types to MeasureProduct<'M, MeasureOne>
                match fullName with
                | Types.measureProduct2 ->
                    match
                        (nonAbbreviatedType genArgs[0]).GenericArguments
                        |> Seq.map (tryDefinition >> Option.bind snd)
                        |> List.ofSeq
                    with
                    // TODO: generalize it to support aggregate units such as <m/s> or more complex
                    | [ Some measure; Some Types.measureOne ] -> measure
                    | _ -> fullName
                | _ -> fullName
            | _ -> Naming.unknown
        else
            Naming.unknown

    let private makeDeclaredType assemblyName genArgs fullName =
        let entRef: Fable.EntityRef =
            {
                FullName = fullName
                Path = Fable.CoreAssemblyName assemblyName
            }

        Fable.DeclaredType(entRef, genArgs)

    let private makeRuntimeType genArgs fullName =
        makeDeclaredType "System.Runtime" genArgs fullName

    let private makeFSharpCoreType genArgs fullName =
        makeDeclaredType "FSharp.Core" genArgs fullName

    let private makeRuntimeTypeWithMeasure (genArgs: IList<FSharpType>) fullName =
        let genArgs = [ getMeasureFullName genArgs |> Fable.Measure ]
        makeRuntimeType genArgs fullName

    let makeTypeFromDef withConstraints ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        if tdef.IsArrayType then
            Fable.Array(
                makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
                Fable.MutableArray
            )
        elif tdef.IsDelegate then
            makeTypeFromDelegate withConstraints ctxTypeArgs genArgs tdef
        elif tdef.IsEnum then
            // F# seems to include a field with this name in the underlying type
            let numberKind =
                tdef.FSharpFields
                |> Seq.tryPick (fun fi ->
                    match fi.Name with
                    | "value__" when fi.FieldType.HasTypeDefinition ->
                        match FsEnt.FullName fi.FieldType.TypeDefinition with
                        | DicContains numberTypes kind -> Some kind
                        | _ -> None
                    | _ -> None
                )
                |> Option.defaultValue Int32

            let info = FsEnt.Ref tdef |> Fable.NumberInfo.IsEnum
            Fable.Number(numberKind, info)
        else
            match FsEnt.FullName tdef with
            // Fable "primitives"
            | Types.object -> Fable.Any
            | Types.unit -> Fable.Unit
            | Types.bool -> Fable.Boolean
            | Types.char -> Fable.Char
            | Types.string -> Fable.String
            | Types.regex -> Fable.Regex
            | Types.type_ -> Fable.MetaType
            | Types.valueOption ->
                Fable.Option(makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head, true)
            | Types.option ->
                Fable.Option(makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head, false)
            | Types.resizeArray ->
                Fable.Array(
                    makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs |> List.head,
                    Fable.ResizeArray
                )
            | Types.list ->
                makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs
                |> List.head
                |> Fable.List
            | DicContains numberTypes kind -> Fable.Number(kind, Fable.NumberInfo.Empty)
            | DicContains numbersWithMeasure kind ->
                let info = getMeasureFullName genArgs |> Fable.NumberInfo.IsMeasure

                Fable.Number(kind, info)
            | DicContains runtimeTypesWithMeasure choice ->
                match choice with
                | Choice1Of2 t -> t
                | Choice2Of2 fullName -> makeRuntimeTypeWithMeasure genArgs fullName
            | fullName when tdef.IsMeasure -> Fable.Measure fullName
            | _ when hasAttrib Atts.stringEnum tdef.Attributes && Compiler.Language <> TypeScript -> Fable.String
            | _ ->
                let genArgs = makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs genArgs

                Fable.DeclaredType(FsEnt.Ref tdef, genArgs)

    let rec makeTypeWithConstraints withConstraints (ctxTypeArgs: Map<string, Fable.Type>) (NonAbbreviatedType t) =
        let typ =
            // Generic parameter (try to resolve for inline functions)
            if t.IsGenericParameter then
                resolveGenParam withConstraints ctxTypeArgs t.GenericParameter
            // Tuple
            elif t.IsTupleType then
                let genArgs =
                    makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs t.GenericArguments

                Fable.Tuple(genArgs, t.IsStructTupleType)
            // Function
            elif t.IsFunctionType then
                let argType =
                    makeTypeWithConstraints withConstraints ctxTypeArgs t.GenericArguments[0]

                let returnType =
                    makeTypeWithConstraints withConstraints ctxTypeArgs t.GenericArguments[1]

                Fable.LambdaType(argType, returnType)
            elif t.IsAnonRecordType then
                let genArgs =
                    makeTypeGenArgsWithConstraints withConstraints ctxTypeArgs t.GenericArguments

                let fields = t.AnonRecordTypeDetails.SortedFieldNames

                let isStruct =
                    match t.BaseType with
                    | Some typ -> (getFsTypeFullName typ) = Types.valueType
                    | None -> false

                Fable.AnonymousRecordType(fields, genArgs, isStruct)
            elif t.HasTypeDefinition then
                // No support for provided types when compiling FCS+Fable to JS
#if !FABLE_COMPILER
                // TODO: Discard provided generated types too?
                if t.TypeDefinition.IsProvidedAndErased then
                    Fable.Any
                else
#endif
                makeTypeFromDef withConstraints ctxTypeArgs t.GenericArguments t.TypeDefinition
            elif t.IsMeasureType then
                Fable.Measure ""
            else
                Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

        // TODO:
        // if not t.IsGenericParameter && t.HasNullAnnotation // || t.IsNullAmbivalent
        // then
        //     makeRuntimeType [ typ ] Types.nullable // represent it as Nullable<T>
        // else typ
        typ

    let makeType (ctxTypeArgs: Map<string, Fable.Type>) t =
        makeTypeWithConstraints true ctxTypeArgs t

    let tryGetBaseEntity (tdef: FSharpEntity) : (FSharpEntity * IList<FSharpType>) option =
        match tdef.BaseType with
        | Some(TypeDefinition baseEnt as baseType) when baseEnt.TryFullName <> Some Types.object ->
            Some(baseEnt, baseType.GenericArguments)
        | _ -> None

    let rec tryFindBaseEntity (filter: FSharpEntity -> bool) (tdef: FSharpEntity) =
        tryGetBaseEntity tdef
        |> Option.bind (fun (baseEnt, _) ->
            if filter baseEnt then
                Some baseEnt
            else
                tryFindBaseEntity filter baseEnt
        )

    let getArgTypes _com (memb: FSharpMemberOrFunctionOrValue) =
        // FSharpParameters don't contain the `this` arg
        Seq.concat memb.CurriedParameterGroups
        // The F# compiler "untuples" the args in methods
        |> Seq.map (fun x -> makeType Map.empty x.Type)
        |> Seq.toList

    let isAbstract (ent: FSharpEntity) =
        hasAttrib Atts.abstractClass ent.Attributes

    let tryGetXmlDoc =
        function
        | FSharpXmlDoc.FromXmlText(xmlDoc) -> xmlDoc.GetXmlText() |> Some
        | _ -> None

    let tryGetInterfaceTypeFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0 then
            nonAbbreviatedType meth.ImplementedAbstractSignatures[0].DeclaringType |> Some
        else
            None

    let tryGetInterfaceDefinitionFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0 then
            let t = nonAbbreviatedType meth.ImplementedAbstractSignatures[0].DeclaringType

            if t.HasTypeDefinition then
                Some t.TypeDefinition
            else
                None
        else
            None

    let tryFindMember (ent: Fable.Entity) genArgs compiledName isInstance (argTypes: Fable.Type list) =
        match ent with
        | :? FsEnt as entity -> entity.TryFindMember(compiledName, isInstance, List.toArray argTypes, genArgs) //, searchHierarchy=true)
        | _ -> None

    let tryFindAbstractMember
        (com: IFableCompiler)
        (ent: FSharpEntity)
        (compiledName: string)
        (isInstance: bool)
        (argTypes: Fable.Type[] option)
        =
        let entRef = FsEnt.Ref ent

        com.TryGetEntity(entRef)
        |> Option.bind (fun ent ->
            match ent with
            | :? FsEnt as entity ->
                entity.TryFindMember(compiledName, isInstance, ?argTypes = argTypes, requireDispatchSlot = true)
            | _ -> None
        )

    let tryFindWitness (ctx: Context) argTypes isInstance traitName =
        ctx.Witnesses
        |> List.tryFind (fun w ->
            w.TraitName = traitName
            && w.IsInstance = isInstance
            && listEquals (typeEquals false) argTypes w.ArgTypes
        )

module Identifiers =
    open Helpers
    open TypeHelpers

    let isMutableOrByRefValue (fsRef: FSharpMemberOrFunctionOrValue) =
        (fsRef.IsMutable || isByRefValue fsRef)
        && not (
            fsRef.IsCompilerGenerated
            && (fsRef.CompiledName = "copyOfStruct" || fsRef.CompiledName = "inputRecord")
        )

    let makeIdentFrom (com: IFableCompiler) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) : Fable.Ident =
        let part = Naming.NoMemberPart

        let name =
            // The F# compiler sometimes adds a numeric suffix. Remove it because it's not deterministic.
            // See https://github.com/fable-compiler/Fable/issues/2869#issuecomment-1169574962
            if fsRef.IsCompilerGenerated then
                // Regex.Replace(fsRef.CompiledName, @"\d+$", "", RegexOptions.Compiled)
                fsRef.CompiledName.TrimEnd([| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' |])
            else
                fsRef.CompiledName

        let sanitizedName =
            match com.Options.Language with
            | Python ->
                let name = Fable.Py.Naming.toSnakeCase name

                Fable.Py.Naming.sanitizeIdent
                    (fun name -> isUsedName ctx name || Fable.Py.Naming.pyBuiltins.Contains name)
                    name
                    part
            | Rust -> Naming.sanitizeIdent (isUsedName ctx) (name |> cleanNameAsRustIdentifier) part
            | _ -> Naming.sanitizeIdent (isUsedName ctx) name part

        let isMutable =
            match com.Options.Language with
            | Rust -> isMutableOrByRefValue fsRef // non-compiler-generated mutable or byref value
            | _ -> fsRef.IsMutable

        ctx.UsedNamesInDeclarationScope.Add(sanitizedName) |> ignore
        let r = makeRange fsRef.DeclarationLocation

        let r =
            SourceLocation.Create(start = r.start, ``end`` = r.``end``, ?file = r.File, displayName = fsRef.DisplayName)

        {
            Name = sanitizedName
            Type = makeType ctx.GenericArgs fsRef.FullType
            IsThisArgument = fsRef.IsMemberThisValue
            IsCompilerGenerated = fsRef.IsCompilerGenerated
            IsMutable = isMutable
            Range = Some r
        }

    let putIdentInScope com ctx (fsRef: FSharpMemberOrFunctionOrValue) value : Context * Fable.Ident =
        let ident = makeIdentFrom com ctx fsRef
        { ctx with Scope = (Some fsRef, ident, value) :: ctx.Scope }, ident

    let (|PutIdentInScope|) com ctx fsRef = putIdentInScope com ctx fsRef None

    let identWithRange r (ident: Fable.Ident) =
        let originalName = ident.Range |> Option.bind (fun r -> r.identifierName)

        { ident with Range = r |> Option.map (fun r -> { r with identifierName = originalName }) }

    let tryGetValueFromScope (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.Scope
        |> List.tryPick (fun (fsRef', _ident, value) ->
            fsRef' |> Option.filter fsRef.Equals |> Option.bind (fun _ -> value)
        )

    let tryGetIdentFromScopeIf (ctx: Context) r typ predicate =
        ctx.Scope
        |> List.tryPick (fun (fsRef, ident, _) ->
            fsRef
            |> Option.filter predicate
            |> Option.map (fun _ ->
                let ident = identWithRange r ident

                let ident =
                    match typ with
                    | Some t -> { ident with Type = t }
                    | None -> ident

                Fable.IdentExpr ident
            )
        )

    /// Get corresponding identifier to F# value in current scope
    let tryGetIdentFromScope (ctx: Context) r typ (fsRef: FSharpMemberOrFunctionOrValue) =
        tryGetIdentFromScopeIf ctx r typ fsRef.Equals

module Util =
    open Helpers
    open Patterns
    open TypeHelpers
    open Identifiers

    let isUnitArg (ident: Fable.Ident) =
        ident.IsCompilerGenerated && ident.Type = Fable.Unit
    // && (ident.DisplayName.StartsWith("unitVar") || ident.DisplayName.Contains("@"))

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [ arg ] when isUnitArg arg -> []
        | [ thisArg; arg ] when thisArg.IsThisArgument && isUnitArg arg -> [ thisArg ]
        | args -> args

    let dropUnitCallArg (args: Fable.Expr list) (argTypes: Fable.Type list) =
        match args, argTypes with
        // Don't remove unit arg if a generic is expected
        | [ MaybeCasted(Fable.Value(Fable.UnitConstant, _)) ], [ Fable.GenericParam _ ] -> args
        | [ MaybeCasted(Fable.Value(Fable.UnitConstant, _)) ], _ -> []
        | [ Fable.IdentExpr ident ], _ when isUnitArg ident -> []
        | _ -> args

    let unboxBoxedArgs (args: Fable.Expr list) =
        args
        |> List.map (
            function
            | Fable.TypeCast(expr, Fable.Any) -> expr // unbox boxed values
            | arg -> arg
        )

    let makeFunctionArgs com ctx (args: FSharpMemberOrFunctionOrValue list) =
        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = putIdentInScope com ctx var None
                newContext, arg :: accArgs
            )

        ctx, List.rev args

    let bindMemberArgs com ctx (args: FSharpMemberOrFunctionOrValue list list) =
        // The F# compiler "untuples" the args in methods
        let args = List.concat args

        let ctx, thisArg, args =
            match args with
            | firstArg :: restArgs when firstArg.IsMemberThisValue ->
                let ctx, thisArg = putIdentInScope com ctx firstArg None
                let thisArg = { thisArg with IsThisArgument = true }
                let ctx = { ctx with BoundMemberThis = Some thisArg }
                ctx, [ thisArg ], restArgs
            | firstArg :: restArgs when firstArg.IsConstructorThisValue ->
                let ctx, thisArg = putIdentInScope com ctx firstArg None
                let thisArg = { thisArg with IsThisArgument = true }
                let ctx = { ctx with BoundConstructorThis = Some thisArg }
                ctx, [ thisArg ], restArgs
            | _ -> ctx, [], args

        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) arg ->
                let ctx, arg = putIdentInScope com ctx arg None
                ctx, arg :: accArgs
            )

        ctx, thisArg @ (List.rev args)

    let makeTryCatch com ctx r (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some(PutIdentInScope com ctx (catchContext, catchVar), catchBody) ->
                // Add caughtException to context so it can be retrieved by `reraise`
                let catchContext = { catchContext with CaughtException = Some catchVar }

                Some(catchVar, com.Transform(catchContext, catchBody))
            | None -> None

        let finalizer =
            match finalBody with
            | Some(Transform com ctx finalBody) -> Some finalBody
            | None -> None

        Fable.TryCatch(body, catchClause, finalizer, r)

    let addGenArgsToContext (ctx: Context) (memb: FSharpMemberOrFunctionOrValue) (genArgs: Fable.Type list) =
        if not (List.isEmpty genArgs) then
            let genParams =
                match memb.DeclaringEntity with
                // It seems that for F# types memb.GenericParameters contains all generics
                // but for BCL types we need to check the DeclaringEntity generics too
                | Some ent when genArgs.Length > memb.GenericParameters.Count ->
                    Seq.append ent.GenericParameters memb.GenericParameters |> Seq.toList
                | _ -> Seq.toList memb.GenericParameters
                |> List.map genParamName

            if List.sameLength genParams genArgs then
                let ctxGenArgs =
                    (ctx.GenericArgs, List.zip genParams genArgs)
                    ||> List.fold (fun map (k, v) -> Map.add k v map)

                { ctx with GenericArgs = ctxGenArgs }
            else
                ctx
        else
            ctx

    /// Takes only the first CurriedParameterGroup into account.
    /// If there's only a single unit parameter, returns 0.
    let countNonCurriedParams (meth: FSharpMemberOrFunctionOrValue) =
        let args = meth.CurriedParameterGroups

        if args.Count = 0 then
            0
        elif args[0].Count = 1 then
            if isUnit args[0].[0].Type then
                0
            else
                1
        else
            args[0].Count

    /// Same as `countNonCurriedParams` but applied to abstract signatures
    let countNonCurriedParamsForSignature (sign: FSharpAbstractSignature) =
        let args = sign.AbstractArguments

        if args.Count = 0 then
            0
        elif args[0].Count = 1 then
            if isUnit args[0].[0].Type then
                0
            else
                1
        else
            args[0].Count

    // When importing a relative path from a different path where the member,
    // entity... is declared, we need to resolve the path
    let fixImportedRelativePath (com: Compiler) (path: string) sourcePath =
        let file = Path.normalizePathAndEnsureFsExtension sourcePath

        if file = com.CurrentFile then
            path
        else
            Path.Combine(Path.GetDirectoryName(file), path)
            |> Path.getRelativePath com.CurrentFile

    let (|GlobalAtt|ImportAtt|NoGlobalNorImport|) (atts: Fable.Attribute seq) =
        let (|AttFullName|) (att: Fable.Attribute) = att.Entity.FullName, att

        atts
        |> Seq.tryPick (
            function
            | AttFullName(Atts.global_, att) ->
                match att.ConstructorArgs with
                | [ :? string as customName ] -> GlobalAtt(Some customName) |> Some
                | _ -> GlobalAtt(None) |> Some

            | AttFullName(Naming.StartsWith Atts.import _ as fullName, att) ->
                match fullName, att.ConstructorArgs with
                | Atts.importAll, [ (:? string as path) ] -> ImportAtt("*", path.Trim()) |> Some
                | Atts.importDefault, [ (:? string as path) ] -> ImportAtt("default", path.Trim()) |> Some
                | Atts.importMember, [ (:? string as path) ] -> ImportAtt(Naming.placeholder, path.Trim()) |> Some
                | _, [ (:? string as selector); (:? string as path) ] -> ImportAtt(selector.Trim(), path.Trim()) |> Some
                | _ -> None

            | _ -> None
        )
        |> Option.defaultValue NoGlobalNorImport

    /// Function used to check if calls must be replaced by global idents or direct imports
    let tryGlobalOrImportedMember (com: Compiler) typ (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes
        |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
        |> function
            | GlobalAtt(Some customName) -> makeTypedIdent typ customName |> Fable.IdentExpr |> Some
            | GlobalAtt None -> getMemberDisplayName memb |> makeTypedIdent typ |> Fable.IdentExpr |> Some
            | ImportAtt(selector, path) ->
                let selector =
                    if selector = Naming.placeholder then
                        getMemberDisplayName memb
                    else
                        selector

                let path =
                    match Path.isRelativePath path, memb.DeclaringEntity with
                    | true, Some e ->
                        FsEnt.Ref(e).SourcePath
                        |> Option.map (fixImportedRelativePath com path)
                        |> Option.defaultValue path
                    | _ -> path

                makeImportUserGenerated None typ selector path |> Some
            | _ -> None

    let tryGlobalOrImportedAttributes (com: Compiler) (entRef: Fable.EntityRef) (attributes: Fable.Attribute seq) =
        let globalRef customName =
            defaultArg customName entRef.DisplayName
            |> makeTypedIdent Fable.Any
            |> Fable.IdentExpr
            |> Some

        match attributes with
        | _ when entRef.FullName.StartsWith("Fable.Core.JS.", StringComparison.Ordinal) -> globalRef None
        | GlobalAtt customName -> globalRef customName
        | ImportAtt(selector, path) ->
            let selector =
                if selector = Naming.placeholder then
                    entRef.DisplayName
                else
                    selector

            let path =
                if Path.isRelativePath path then
                    entRef.SourcePath
                    |> Option.map (fixImportedRelativePath com path)
                    |> Option.defaultValue path
                else
                    path

            makeImportUserGenerated None Fable.Any selector path |> Some
        | _ -> None

    let tryGlobalOrImportedEntity (com: Compiler) (ent: Fable.Entity) =
        tryGlobalOrImportedAttributes com ent.Ref ent.Attributes

    let tryGlobalOrImportedFSharpEntity (com: Compiler) (ent: FSharpEntity) =
        let entRef = FsEnt.Ref ent

        ent.Attributes
        |> Seq.map (fun a -> FsAtt(a) :> Fable.Attribute)
        |> tryGlobalOrImportedAttributes com entRef

    let isErasedOrStringEnumEntity (ent: Fable.Entity) =
        ent.Attributes
        |> Seq.exists (fun att ->
            match att.Entity.FullName with
            | Atts.erase
            | Atts.stringEnum
            | Atts.tsTaggedUnion -> true
            | _ -> false
        )

    let isErasedOrStringEnumFSharpEntity (ent: FSharpEntity) =
        ent.Attributes
        |> Seq.exists (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some(Atts.erase | Atts.stringEnum | Atts.tsTaggedUnion) -> true
            | _ -> false
        )

    let isGlobalOrImportedEntity (ent: Fable.Entity) =
        ent.Attributes
        |> Seq.exists (fun att ->
            match att.Entity.FullName with
            | Atts.global_
            | Naming.StartsWith Atts.import _ -> true
            | _ -> false
        )

    let isGlobalOrImportedFSharpEntity (ent: FSharpEntity) =
        ent.Attributes
        |> Seq.exists (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some(Atts.global_ | Naming.StartsWith Atts.import _) -> true
            | _ -> false
        )

    let isAttachMembersEntity (com: Compiler) (ent: FSharpEntity) =
        not (ent.IsFSharpModule || ent.IsInterface)
        && (
        // com.Options.Language = Php ||
        com.Options.Language = Rust
        || // attach all members for Rust
        ent.Attributes
        |> Seq.exists (fun att ->
            // Should we make sure the attribute is not an alias?
            match att.AttributeType.TryFullName with
            | Some Atts.attachMembers -> true
            | _ -> false
        ))

    let isEmittedOrImportedMember (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes
        |> Seq.exists (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Naming.StartsWith Atts.emit _ | Atts.global_ | Naming.StartsWith Atts.import _) -> true
            | _ -> false
        )

    let private isFromDllNotPrecompiled (ent: Fable.EntityRef) =
        match ent.Path with
        | Fable.AssemblyPath _
        | Fable.CoreAssemblyName _ -> true
        | Fable.SourcePath _
        | Fable.PrecompiledLib _ -> false

    let private isReplacementCandidatePrivate isFromDll (entFullName: string) =
        if
            entFullName.StartsWith("System.", StringComparison.Ordinal)
            || entFullName.StartsWith("Microsoft.FSharp.", StringComparison.Ordinal)
        then
            isFromDll ()
        // When compiling Fable itself, Fable.Core entities will be part of the code base, but still need to be replaced
        else
            entFullName.StartsWith("Fable.Core.", StringComparison.Ordinal)
            && (not (entFullName.StartsWith("Fable.Core.JS.", StringComparison.Ordinal))
                || entFullName.EndsWith("Attribute", StringComparison.Ordinal))

    let isReplacementCandidate (ent: Fable.EntityRef) =
        let isFromDll () = isFromDllNotPrecompiled ent
        isReplacementCandidatePrivate isFromDll ent.FullName

    let isReplacementCandidateFrom (ent: FSharpEntity) =
        let isFromDll () = Option.isSome ent.Assembly.FileName
        isReplacementCandidatePrivate isFromDll (FsEnt.FullName ent)

    let getEntityGenParams (ent: Fable.Entity) : (string * Fable.Type) list =
        ent.GenericParameters
        |> List.filter (fun p -> not (p.IsMeasure))
        |> List.map (fun p -> p.Name, Fable.Type.GenericParam(p.Name, p.IsMeasure, Seq.toList p.Constraints))

    let getEntityGenArgs (ent: Fable.Entity) : Fable.Type list = getEntityGenParams ent |> List.map snd

    let getEntityType (ent: Fable.Entity) : Fable.Type =
        let genArgs = getEntityGenArgs ent
        Fable.Type.DeclaredType(ent.Ref, genArgs)

    let getMemberGenParams (memb: Fable.MemberFunctionOrValue) : (string * Fable.Type) list =
        memb.GenericParameters
        |> List.filter (fun p -> not (p.IsMeasure))
        |> List.map (fun p -> p.Name, Fable.Type.GenericParam(p.Name, p.IsMeasure, Seq.toList p.Constraints))

    let getMemberGenArgs (memb: Fable.MemberFunctionOrValue) : Fable.Type list = getMemberGenParams memb |> List.map snd

    let getGenParams (types: Fable.Type list) : (string * Fable.Type) list =
        let rec findGenParams typ =
            match typ with
            | Fable.GenericParam(name, false, _) as t -> [ (name, t) ]
            | t -> t.Generics |> List.collect findGenParams

        types |> List.collect findGenParams |> List.distinctBy fst

    let getGenParamNames (types: Fable.Type list) : string list = getGenParams types |> List.map fst
    let getGenParamTypes (types: Fable.Type list) : Fable.Type list = getGenParams types |> List.map snd

    /// We can add a suffix to the entity name for special methods, like reflection declaration
    let entityIdentWithSuffix (com: Compiler) (ent: Fable.EntityRef) suffix =
        let error msg =
            $"%s{msg}: %s{ent.FullName}" |> addErrorAndReturnNull com [] None

        match com.Options.Language, ent.SourcePath with
        | _, None -> error "Cannot reference entity from .dll reference, Fable packages must include F# sources"
        | _, Some file ->
            let entityName = (getEntityDeclarationName com ent) + suffix
            // If precompiling inline function always reference with Import and not as IdentExpr
            if not com.IsPrecompilingInlineFunction && file = com.CurrentFile then
                makeIdentExpr entityName
            else
                makeInternalClassImport com ent entityName file

    let entityIdent (com: Compiler) (ent: Fable.EntityRef) = entityIdentWithSuffix com ent ""

    /// First checks if the entity is global or imported
    let tryEntityIdentMaybeGlobalOrImported (com: Compiler) (ent: Fable.Entity) =
        match tryGlobalOrImportedEntity com ent with
        | Some _importedEntity as entOpt -> entOpt
        | None ->
            if isFromDllNotPrecompiled ent.Ref then
                None
            else
                Some(entityIdent com ent.Ref)

    let memberIdent (com: Compiler) r typ (memb: FSharpMemberOrFunctionOrValue) membRef =
        let r = r |> Option.map (fun r -> { r with identifierName = Some memb.DisplayName })

        let memberName, hasOverloadSuffix = getMemberDeclarationName com memb

        let memberName =
            match com.Options.Language, memb.DeclaringEntity with
            | Rust, Some ent when not memb.IsInstanceMember || memb.IsExtensionMember ->
                // for Rust, use the namespace for default static interface calls,
                // for other non-instance calls, prefix with the full entity name
                if ent.IsInterface && not memb.IsDispatchSlot && ent.FullName.Contains(".") then
                    let ns, _ = Fable.Naming.splitLastBy "." ent.FullName
                    ns + "." + memberName
                else
                    ent.FullName + "." + memberName
            | _ -> memberName

        let file =
            memb.DeclaringEntity
            |> Option.bind (fun ent -> FsEnt.Ref(ent).SourcePath)
            // Cases when .DeclaringEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            |> Option.defaultValue com.CurrentFile

        // If precompiling inline function always reference with Import and not as IdentExpr
        if not com.IsPrecompilingInlineFunction && file = com.CurrentFile then
            { makeTypedIdent typ memberName with
                Range = r
                IsMutable = memb.IsMutable
            }
            |> Fable.IdentExpr
        else
            // If the overload suffix changes, we need to recompile the files that call this member
            if hasOverloadSuffix then
                com.AddWatchDependency(file)

            makeInternalMemberImport com typ membRef memberName file

    let getFunctionMemberRef (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        // We cannot retrieve compiler generated members from the entity
        | Some ent when not memb.IsCompilerGenerated ->
            let nonCurriedArgTypes =
                if memb.CurriedParameterGroups.Count = 1 then
                    memb.CurriedParameterGroups[0]
                    |> Seq.mapToList (fun p -> makeType Map.empty p.Type)
                    |> Some
                else
                    None

            let fableMemberFunctionOrValue =
                FsMemberFunctionOrValue(memb) :> Fable.MemberFunctionOrValue

            let attributeFullNames =
                fableMemberFunctionOrValue.Attributes
                |> Seq.map (fun attr -> attr.Entity.FullName)
                |> List.ofSeq

            Fable.MemberRef(
                FsEnt.Ref(ent),
                {
                    CompiledName = memb.CompiledName
                    IsInstance = memb.IsInstanceMember
                    NonCurriedArgTypes = nonCurriedArgTypes
                    AttributeFullNames = attributeFullNames
                }
            )
        | ent ->
            let entRef = ent |> Option.map FsEnt.Ref

            let argTypes =
                memb.CurriedParameterGroups
                |> Seq.concat
                |> Seq.mapToList (fun p -> makeType Map.empty p.Type)

            let returnType = makeType Map.empty memb.ReturnParameter.Type

            Fable.GeneratedMember.Function(
                memb.CompiledName,
                argTypes,
                returnType,
                isInstance = memb.IsInstanceMember,
                hasSpread = hasParamArray memb,
                ?entRef = entRef
            )

    let getValueMemberRef (memb: FSharpMemberOrFunctionOrValue) =
        match memb.DeclaringEntity with
        // We cannot retrieve compiler generated members from the entity
        | Some ent when not memb.IsCompilerGenerated ->
            let fableMemberFunctionOrValue =
                FsMemberFunctionOrValue(memb) :> Fable.MemberFunctionOrValue

            let attributeFullNames =
                fableMemberFunctionOrValue.Attributes
                |> Seq.map (fun attr -> attr.Entity.FullName)
                |> List.ofSeq

            Fable.MemberRef(
                FsEnt.Ref(ent),
                {
                    CompiledName = memb.CompiledName
                    IsInstance = memb.IsInstanceMember
                    NonCurriedArgTypes = None
                    AttributeFullNames = attributeFullNames
                }
            )
        | ent ->
            let entRef = ent |> Option.map FsEnt.Ref
            let typ = makeType Map.empty memb.ReturnParameter.Type

            Fable.GeneratedMember.Value(
                memb.CompiledName,
                typ,
                isInstance = memb.IsInstanceMember,
                isMutable = memb.IsMutable,
                ?entRef = entRef
            )

    let rec tryFindInTypeHierarchy (ent: FSharpEntity) filter =
        if filter ent then
            Some ent
        else
            match tryGetBaseEntity ent with
            | Some(ent, _) -> tryFindInTypeHierarchy ent filter
            | _ -> None

    /// Checks who's the actual implementor of the interface, this entity or any of its parents
    let rec tryFindImplementingEntity (ent: FSharpEntity) interfaceFullName =
        tryFindInTypeHierarchy
            ent
            (fun ent -> ent.DeclaredInterfaces |> Seq.exists (testInterfaceHierarchy interfaceFullName))

    let rec inherits (ent: FSharpEntity) baseFullName =
        tryFindInTypeHierarchy ent (fun ent -> ent.TryFullName = Some baseFullName)
        |> Option.isSome

    let tryMangleAttribute (attributes: FSharpAttribute seq) =
        attributes
        |> tryFindAttrib Atts.mangle
        |> Option.map (fun att ->
            match Seq.tryHead att.ConstructorArguments with
            | Some(_, (:? bool as value)) -> value
            | _ -> true
        )

    let isMangledAbstractEntity (com: Compiler) (ent: FSharpEntity) =
        match ent.TryFullName with
        // By default mangle interfaces in System namespace as they are not meant to interact with JS
        // except those that are used in fable-library Typescript files
        | Some fullName when fullName.StartsWith("System.", StringComparison.Ordinal) ->
            match fullName with
            | Types.object
            | Types.idisposable
            | "System.IObservable`1"
            | "System.IObserver`1"
            | Types.ienumerableGeneric
            // These are used for injections
            | Types.icomparerGeneric
            | Types.iequalityComparerGeneric -> false
            | Types.icomparable -> false
            | Types.icomparableGeneric -> com.Options.Language <> Dart
            | _ -> true
        // Don't mangle abstract classes in Fable.Core.JS and Fable.Core.Py namespaces
        | Some fullName when fullName.StartsWithAny("Fable.Core.JS.", "Fable.Core.Py.") -> false
        // Don't mangle interfaces by default (for better interop) unless they have Mangle attribute
        | _ when ent.IsInterface -> tryMangleAttribute ent.Attributes |> Option.defaultValue false
        // Mangle members from abstract classes unless they are global/imported or with explicitly attached members
        | _ -> not (isGlobalOrImportedFSharpEntity ent || isAttachMembersEntity com ent)

    let getMangledAbstractMemberName (ent: FSharpEntity) memberName overloadHash =
        // TODO: Error if entity doesn't have fullName?
        let entityName = defaultArg ent.TryFullName ""
        entityName + "." + memberName + overloadHash

    let getAbstractMemberInfo com (ent: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        let isMangled = isMangledAbstractEntity com ent
        let isGetter = FsMemberFunctionOrValue.IsGetter(memb)
        let isSetter = not isGetter && FsMemberFunctionOrValue.IsSetter(memb)

        let name =
            if isMangled then
                let overloadHash =
                    if isGetter || isSetter then
                        ""
                    else
                        getOverloadSuffixFrom ent memb

                getMangledAbstractMemberName ent memb.CompiledName overloadHash
            else if
                // use DisplayName for getters/setters (except for Rust)
                (isGetter || isSetter) && not (com.Options.Language = Rust)
            then
                getMemberDisplayName memb
            else
                memb.CompiledName

        {|
            name = name
            isMangled = isMangled
            isGetter = isGetter
            isSetter = isSetter
        |}

    // try finding a member's generic parameter that is constrained by an interface
    let tryFindGenParam (ent: FSharpEntity) (membOpt: FSharpMemberOrFunctionOrValue option) =
        membOpt
        |> Option.bind (fun m ->
            m.GenericParameters
            |> Seq.tryFind (fun p ->
                p.Constraints
                |> Seq.exists (fun c ->
                    if c.IsCoercesToConstraint then
                        let t = c.CoercesToTarget

                        if t.HasTypeDefinition then
                            t.TypeDefinition = ent
                        else
                            false
                    else
                        false
                )
            )
        )

    let callAttachedMember
        (com: Compiler)
        (ctx: Context)
        r
        typ
        (callInfo: Fable.CallInfo)
        (entity: FSharpEntity)
        (memb: FSharpMemberOrFunctionOrValue)
        =
        match callInfo.ThisArg with
        | None ->
            // for some reason the callee for generic static interface calls is empty
            // so we have to use the generic param from the enclosing member for that
            let genParamOpt = ctx.EnclosingMember |> tryFindGenParam entity

            match genParamOpt with
            | Some genParam when com.Options.Language = Rust ->
                // let memberName, _ = getMemberDeclarationName com memb
                let memberName = memb.CompiledName // TODO: handle overloads
                let memberName = genParam.FullName + "." + memberName
                let callee = makeIdentExpr memberName
                makeCall r typ callInfo callee
            | _ ->
                // TODO: add support for static interface calls in other languages
                $"Unexpected static interface/override call: %s{memb.FullName}"
                |> attachRange r
                |> failwith

        | Some callee ->
            let callInfo = { callInfo with ThisArg = None }
            let info = getAbstractMemberInfo com entity memb

            // Python do not support static getters, so we need to call a getter function instead
            let isPythonStaticMember =
                com.Options.Language = Python && not memb.IsInstanceMember

            if
                not info.isMangled
                && info.isGetter
                && not isPythonStaticMember
                && not (com.Options.Language = Rust)
            then
                // Set the field as maybe calculated so it's not displaced by beta reduction
                let kind =
                    Fable.FieldInfo.Create(
                        info.name,
                        fieldType = (memb.ReturnParameter.Type |> makeType Map.empty),
                        maybeCalculated = true,
                        ?tag = tryGetFieldTag memb
                    )

                Fable.Get(callee, kind, typ, r)
            elif not info.isMangled && info.isSetter && not (com.Options.Language = Rust) then
                let membType = memb.CurriedParameterGroups[0].[0].Type |> makeType Map.empty
                let arg = callInfo.Args |> List.tryHead |> Option.defaultWith makeNull
                Fable.Set(callee, Fable.FieldSet(info.name), membType, arg, r)
            else
                let entityGenParamsCount = entity.GenericParameters.Count

                let callInfo =
                    if callInfo.GenericArgs.Length < entityGenParamsCount then
                        callInfo
                    else
                        { callInfo with GenericArgs = List.skip entityGenParamsCount callInfo.GenericArgs }

                getField callee info.name |> makeCall r typ callInfo

    let failReplace (com: IFableCompiler) ctx r (info: Fable.ReplaceCallInfo) (thisArg: Fable.Expr option) =
        let msg =
            if info.DeclaringEntityFullName.StartsWith("Fable.Core.", StringComparison.Ordinal) then
                $"{info.DeclaringEntityFullName}.{info.CompiledName} is not supported, try updating fable tool"
            else
                com.WarnOnlyOnce(
                    "Fable only supports a subset of standard .NET API, please check https://fable.io/docs/dotnet/compatibility.html. For external libraries, check whether they are Fable-compatible in the package docs."
                )

                $"""{info.DeclaringEntityFullName}.{info.CompiledName}{if Option.isSome thisArg then
                                                                           ""
                                                                       else
                                                                           " (static)"} is not supported by Fable"""

        msg |> addErrorAndReturnNull com ctx.InlinePath r

    let (|Replaced|_|)
        (com: IFableCompiler)
        (ctx: Context)
        r
        typ
        (callInfo: Fable.CallInfo)
        (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option)
        =
        match entity with
        | Some ent when isReplacementCandidateFrom ent ->
            let info: Fable.ReplaceCallInfo =
                {
                    SignatureArgTypes = callInfo.SignatureArgTypes
                    DeclaringEntityFullName = ent.FullName
                    HasSpread = hasParamArray memb
                    IsModuleValue = isModuleValueForCalls com ent memb
                    IsInterface = ent.IsInterface
                    CompiledName = memb.CompiledName
                    OverloadSuffix =
                        if ent.IsFSharpModule then
                            ""
                        else
                            getOverloadSuffixFrom ent memb
                    GenericArgs = callInfo.GenericArgs
                }

            match ctx.PrecompilingInlineFunction with
            | Some _ ->
                // Deal with reraise so we don't need to save caught exception every time
                match ctx.CaughtException, info.DeclaringEntityFullName, info.CompiledName with
                | Some ex, "Microsoft.FSharp.Core.Operators", "Reraise" when com.Options.Language <> Dart ->
                    makeThrow r typ (Fable.IdentExpr ex) |> Some
                | _ ->
                    // If it's an interface compile the call to the attached member just in case
                    let attachedCall =
                        if info.IsInterface then
                            callAttachedMember com ctx r typ callInfo ent memb |> Some
                        else
                            None

                    let e =
                        Fable.UnresolvedReplaceCall(callInfo.ThisArg, callInfo.Args, info, attachedCall)

                    Fable.Unresolved(e, typ, r) |> Some
            | None ->
                match com.TryReplace(ctx, r, typ, info, callInfo.ThisArg, callInfo.Args) with
                | Some e -> Some e
                | None when info.IsInterface -> callAttachedMember com ctx r typ callInfo ent memb |> Some
                | None -> failReplace com ctx r info callInfo.ThisArg |> Some
        | _ -> None

    let addWatchDependencyFromMember (com: Compiler) (memb: FSharpMemberOrFunctionOrValue) =
        memb.DeclaringEntity
        |> Option.bind (fun ent -> FsEnt.Ref(ent).SourcePath)
        |> Option.iter com.AddWatchDependency

    let (|Emitted|_|)
        (com: Compiler)
        (ctx: Context)
        r
        typ
        (callInfo: Fable.CallInfo option)
        (memb: FSharpMemberOrFunctionOrValue)
        =
        memb.Attributes
        |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Naming.StartsWith Atts.emit _ as attFullName) ->
                addWatchDependencyFromMember com memb

                let callInfo =
                    match callInfo with
                    | Some i -> i
                    | None -> Fable.CallInfo.Create()
                // Allow combination of Import and Emit attributes
                let callInfo =
                    match tryGlobalOrImportedMember com Fable.Any memb with
                    | Some importExpr -> { callInfo with Fable.ThisArg = Some importExpr }
                    | _ -> callInfo

                let isStatement = tryAttribConsArg att 1 false tryBoolean
                let macro = tryAttribConsArg att 0 "" tryString

                let macro =
                    match attFullName with
                    | Atts.emitMethod -> "$0." + macro + "($1...)"
                    | Atts.emitConstructor -> "new $0($1...)"
                    | Atts.emitIndexer -> "$0[$1]{{=$2}}"
                    | Atts.emitProperty -> "$0." + macro + "{{=$1}}"
                    | _ -> macro

                let emitInfo: Fable.EmitInfo =
                    {
                        Macro = macro
                        IsStatement = isStatement
                        CallInfo = callInfo
                    }

                Fable.Emit(emitInfo, typ, r) |> Some
            | _ -> None
        )

    let (|Imported|_|)
        (com: Compiler)
        (ctx: Context)
        r
        typ
        callInfo
        (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option)
        =
        let importValueType =
            if Option.isSome callInfo then
                Fable.Any
            else
                typ

        match tryGlobalOrImportedMember com importValueType memb, callInfo, entity with
        // Import called as function
        | Some importExpr, Some callInfo, Some e ->
            let isValueOrGetter =
                isModuleValueForCalls com e memb
                || (memb.IsPropertyGetterMethod && (countNonCurriedParams memb) = 0)

            if isValueOrGetter then
                Some importExpr
            else
                makeCall r typ callInfo importExpr |> Some

        // Import called as value
        | Some importExpr, None, _ -> Some importExpr

        // The value/method is not imported, check if the declaring entity is
        | None, Some callInfo, Some e ->
            let moduleOrClassExpr =
                match tryGlobalOrImportedFSharpEntity com e with
                | Some expr -> Some expr
                // AttachMembers classes behave the same as global/imported classes
                | None when not (com.Options.Language = Rust) && isAttachMembersEntity com e ->
                    FsEnt.Ref e |> entityIdent com |> Some
                | None -> None

            match moduleOrClassExpr, callInfo.ThisArg with
            | Some _, Some _thisArg -> callAttachedMember com ctx r typ callInfo e memb |> Some

            | Some classExpr, None when memb.IsConstructor ->
                let callInfo = { callInfo with Tags = "new" :: callInfo.Tags }
                makeCall r typ callInfo classExpr |> Some

            | Some moduleOrClassExpr, None ->
                if isModuleValueForCalls com e memb then
                    // Set the field as maybe calculated so it's not displaced by beta reduction
                    let kind =
                        Fable.FieldInfo.Create(
                            getMemberDisplayName memb,
                            maybeCalculated = true,
                            ?tag = tryGetFieldTag memb
                        )

                    Fable.Get(moduleOrClassExpr, kind, typ, r) |> Some
                else
                    let callInfo = { callInfo with ThisArg = Some moduleOrClassExpr }
                    callAttachedMember com ctx r typ callInfo e memb |> Some

            | None, _ -> None
        | _ -> None
        |> Option.tap (fun _ -> addWatchDependencyFromMember com memb)

    let inlineExpr (com: IFableCompiler) (ctx: Context) r t callee (info: Fable.CallInfo) membUniqueName =
        let args: Fable.Expr list =
            match callee with
            | Some c -> c :: info.Args
            | None -> info.Args

        let inExpr = com.GetInlineExpr(membUniqueName)
        com.AddWatchDependency(inExpr.FileName)

        let fromFile, fromRange =
            match ctx.InlinePath with
            | {
                  ToFile = file
                  ToRange = r
              } :: _ -> file, r
            | [] -> com.CurrentFile, r

        let genArgs = List.zipSafe inExpr.GenericArgs info.GenericArgs |> Map

        let ctx =
            { ctx with
                GenericArgs = genArgs
                InlinePath =
                    {
                        ToFile = inExpr.FileName
                        ToRange = inExpr.Body.Range
                        FromFile = fromFile
                        FromRange = fromRange
                    }
                    :: ctx.InlinePath
            }

        let bindings, expr = com.ResolveInlineExpr(ctx, inExpr, args)

        match expr with
        // If this is a user import expression, apply the arguments, see #2280
        | Fable.Import(importInfo, ti, r) as importExpr when not importInfo.IsCompilerGenerated ->
            let isGetterOrValue () =
                info.MemberRef
                |> Option.bind com.TryGetMember
                |> Option.map (fun m -> m.IsGetter || m.IsValue)
                |> Option.defaultValue false

            // Check if import has absorbed the arguments, see #2284
            let args =
                let path = importInfo.Path

                match importInfo.Selector, info.Args with
                | sel, (StringConst selArg) :: (StringConst pathArg) :: args when sel = selArg && path = pathArg -> args
                | ("default" | "*"), (StringConst pathArg) :: args when path = pathArg -> args
                | _, args -> args

            // Don't apply args either if this is a class getter, see #2329
            if List.isEmpty args || isGetterOrValue () then
                // Set UserImport(inline=true) to prevent Fable removing args of surrounding function
                Fable.Import({ importInfo with Kind = Fable.UserImport true }, ti, r)
            else
                makeCall r t info importExpr

        | body ->
            // Check the resolved expression has the expected type, see #2644
            let body =
                if t <> body.Type then
                    Fable.TypeCast(body, t)
                else
                    body

            List.fold (fun body (ident, value) -> Fable.Let(ident, value, body)) body bindings

    let (|Inlined|_|) (com: IFableCompiler) (ctx: Context) r t callee info (memb: FSharpMemberOrFunctionOrValue) =
        if isInline memb then
            let membUniqueName = getMemberUniqueName memb

            match ctx.PrecompilingInlineFunction with
            | Some memb2 when memb.Equals(memb2) ->
                $"Recursive functions cannot be inlined: (%s{memb.FullName})"
                |> addErrorAndReturnNull com [] r
                |> Some
            | Some _ ->
                let e = Fable.UnresolvedInlineCall(membUniqueName, ctx.Witnesses, callee, info)

                Fable.Unresolved(e, t, r) |> Some
            | None -> inlineExpr com ctx r t callee info membUniqueName |> Some
        else
            None

    /// Removes optional arguments set to None in tail position
    let transformOptionalArguments
        (com: IFableCompiler)
        (_ctx: Context)
        (_r: SourceLocation option)
        (memb: FSharpMemberOrFunctionOrValue)
        (args: Fable.Expr list)
        =
        if
            memb.CurriedParameterGroups.Count <> 1
            || memb.CurriedParameterGroups[0].Count <> (List.length args)
            || com.Options.Language = Rust // keep all optional args for Rust
        then
            args
        else
            (memb.CurriedParameterGroups[0], args, (true, []))
            |||> Seq.foldBack2 (fun par arg (keepChecking, acc) ->
                if keepChecking && par.IsOptionalArg then
                    match arg with
                    | Fable.Value(Fable.NewOption(None, _, _), _) -> true, acc
                    | _ -> false, arg :: acc
                else
                    false, arg :: acc
            )
            |> snd

    let getInterfaceMembers (com: Compiler) (ent: Fable.Entity) =
        ent.AllInterfaces
        |> Seq.collect (fun ifc ->
            let ifcEnt = com.GetEntity(ifc.Entity)
            ifcEnt.MembersFunctionsAndValues |> Seq.map (fun memb -> ifc, memb)
        )

    let hasInterface fullName (ent: Fable.Entity) =
        ent.AllInterfaces |> Seq.exists (fun ifc -> ifc.Entity.FullName = fullName)

    let hasAttribute fullName (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att -> att.Entity.FullName = fullName)

    let hasStructuralEquality (ent: Fable.Entity) =
        (ent |> hasAttribute Atts.structuralEquality)
        || not (ent |> hasAttribute Atts.noEquality)
           && not (ent |> hasAttribute Atts.referenceEquality)
           && (ent.IsFSharpRecord
               || ent.IsFSharpUnion
               || ent.IsValueType
               || (ent |> hasInterface Types.iStructuralEquatable))

    let hasStructuralComparison (ent: Fable.Entity) =
        (ent |> hasAttribute Atts.structuralComparison)
        || not (ent |> hasAttribute Atts.noComparison)
           && (ent.IsFSharpRecord
               || ent.IsFSharpUnion
               || ent.IsValueType
               || (ent |> hasInterface Types.iStructuralComparable))

    let makeCallWithArgInfo
        com
        (ctx: Context)
        r
        typ
        callee
        (memb: FSharpMemberOrFunctionOrValue)
        membRef
        (callInfo: Fable.CallInfo)
        =
        match memb, memb.DeclaringEntity with
        | Emitted com ctx r typ (Some callInfo) emitted, _ -> emitted
        | Imported com ctx r typ (Some callInfo) imported -> imported
        | Replaced com ctx r typ callInfo replaced -> replaced
        | Inlined com ctx r typ callee callInfo expr, _ -> expr

        | Try (tryGetIdentFromScope ctx r None) funcExpr, Some entity ->
            if isModuleValueForCalls com entity memb then
                funcExpr
            else
                makeCall r typ callInfo funcExpr

        | _, Some entity when entity.IsDelegate ->
            match callInfo.ThisArg, memb.DisplayName with
            | Some callee, "Invoke" ->
                let callInfo = { callInfo with ThisArg = None }
                makeCall r typ callInfo callee
            | _ ->
                "Only Invoke is supported in delegates"
                |> addErrorAndReturnNull com ctx.InlinePath r

        // Check if this is an interface or abstract/overriden method
        | _, Some entity when
            entity.IsInterface && memb.IsInstanceMember
            || memb.IsOverrideOrExplicitInterfaceImplementation
            || memb.IsDispatchSlot
            ->

            // When calling `super` in an override, it may happen the method is not originally declared
            // by the immediate parent, so we need to go through the hierarchy until we find the original declaration
            // (this is important to get the correct mangled name)
            let entity =
                match memb.IsOverrideOrExplicitInterfaceImplementation, callInfo.ThisArg with
                | true, Some(Fable.Value(Fable.BaseValue _, _)) ->
                    // Only compare param types for overloads (single curried parameter group)
                    let paramTypes =
                        if memb.CurriedParameterGroups.Count = 1 then
                            memb.CurriedParameterGroups[0]
                            |> Seq.map (fun p -> makeType Map.empty p.Type)
                            |> Seq.toArray
                            |> Some
                        else
                            None

                    entity
                    |> tryFindBaseEntity (fun ent ->
                        tryFindAbstractMember com ent memb.CompiledName memb.IsInstanceMember paramTypes
                        |> Option.isSome
                    )
                    |> Option.defaultValue entity
                | _ -> entity

            callAttachedMember com ctx r typ callInfo entity memb

        | _, Some entity when isModuleValueForCalls com entity memb ->
            let typ = makeType ctx.GenericArgs memb.FullType
            memberIdent com r typ memb membRef

        | _, Some entity when com.Options.Language = Dart && memb.IsImplicitConstructor ->
            let classExpr = FsEnt.Ref entity |> entityIdent com
            let callInfo = { callInfo with Tags = "new" :: callInfo.Tags }
            makeCall r typ callInfo classExpr
        | _ ->
            // If member looks like a value but behaves like a function (has generic args) the type from F# AST is wrong (#2045).
            let typ = makeType ctx.GenericArgs memb.FullType
            let retTyp = makeType ctx.GenericArgs memb.ReturnParameter.Type
            let callInfo = { callInfo with Tags = "value" :: callInfo.Tags }
            let callExpr = memberIdent com r typ memb membRef |> makeCall r retTyp callInfo

            let fableMember = FsMemberFunctionOrValue(memb)
            // TODO: Move plugin application to FableTransforms
            com.ApplyMemberCallPlugin(fableMember, callExpr)

    let makeCallFrom
        (com: IFableCompiler)
        (ctx: Context)
        r
        typ
        (genArgs: Fable.Type list)
        callee
        args
        (memb: FSharpMemberOrFunctionOrValue)
        =
        let ctx = addGenArgsToContext ctx memb genArgs
        let memberRef = getFunctionMemberRef memb

        Fable.CallInfo.Create(
            ?thisArg = callee,
            args = transformOptionalArguments com ctx r memb args,
            genArgs = genArgs,
            sigArgTypes = getArgTypes com memb,
            // isCons = memb.IsConstructor,
            memberRef = memberRef
        )
        |> makeCallWithArgInfo com ctx r typ callee memb memberRef

    let makeValueFrom (com: IFableCompiler) (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType ctx.GenericArgs v.FullType

        match v, v.DeclaringEntity with
        | _ when typ = Fable.Unit && v.IsCompilerGenerated ->
            // if com.Options.Verbosity = Verbosity.Verbose && not v.IsCompilerGenerated then // See #1516
            //     $"Value %s{v.DisplayName} is replaced with unit constant"
            //     |> addWarning com ctx.InlinePath r
            Fable.Value(Fable.UnitConstant, r)
        | Emitted com ctx r typ None emitted, _ -> emitted
        | Imported com ctx r typ None imported -> imported
        | Try (tryGetIdentFromScope ctx r (Some typ)) expr, _ -> expr
        | _ -> getValueMemberRef v |> memberIdent com r typ v
