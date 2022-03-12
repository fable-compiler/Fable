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

type FsField(fi: FSharpField) =
    let name = FsField.FSharpFieldName fi
    let typ = TypeHelpers.makeType Map.empty fi.FieldType

    interface Fable.Field with
        member _.Name = name
        member _.FieldType = typ
        member _.LiteralValue = fi.LiteralValue
        member _.IsStatic = fi.IsStatic
        member _.IsMutable = fi.IsMutable

    static member FSharpFieldName (fi: FSharpField) =
        let rec countConflictingCases acc (ent: FSharpEntity) (name: string) =
            match TypeHelpers.getBaseEntity ent with
            | None -> acc
            | Some (baseClass, _) ->
                let conflicts =
                    baseClass.FSharpFields
                    |> Seq.exists (fun fi -> fi.Name = name)
                let acc = if conflicts then acc + 1 else acc
                countConflictingCases acc baseClass name

        let name = fi.Name
        match fi.DeclaringEntity with
        | None -> name
        | Some ent when ent.IsFSharpRecord || ent.IsFSharpUnion -> name
        | Some ent ->
            match countConflictingCases 0 ent name with
            | 0 -> name
            | n -> name + "_" + (string n)

[<RequireQualifiedAccess>]
type CompiledValue =
    | Integer of int
    | Float of float
    | Boolean of bool

type FsUnionCase(uci: FSharpUnionCase) =
    /// FSharpUnionCase.CompiledName doesn't give the value of CompiledNameAttribute
    /// We must check the attributes explicitly
    static member CompiledName (uci: FSharpUnionCase) =
        uci.Attributes
        |> Helpers.tryFindAtt Atts.compiledName
        |> Option.map (fun (att: FSharpAttribute) -> att.ConstructorArguments.[0] |> snd |> string)

    static member FullName (uci: FSharpUnionCase) =
        // proper full compiled name (instead of uci.FullName)
        uci.XmlDocSig
        |> Naming.replacePrefix "T:Microsoft.FSharp." "FSharp."
        |> Naming.replacePrefix "T:" ""

    static member CompiledValue (uci: FSharpUnionCase) =
        uci.Attributes
        |> Helpers.tryFindAtt Atts.compiledValue
        |> Option.bind (fun (att: FSharpAttribute) ->
            match snd att.ConstructorArguments.[0] with
            | :? int as value -> Some (CompiledValue.Integer value)
            | :? float as value -> Some (CompiledValue.Float value)
            | :? bool as value -> Some (CompiledValue.Boolean value)
            | :? Enum as value when Enum.GetUnderlyingType(value.GetType()) = typeof<int> -> Some (CompiledValue.Integer (box value :?> int))
            | _ -> None
        )

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
    static member Constraint(c: FSharpGenericParameterConstraint) =
        if c.IsCoercesToConstraint then
            TypeHelpers.makeType Map.empty c.CoercesToTarget
            |> Fable.Constraint.CoercesTo |> Some
        elif c.IsMemberConstraint then
            let d = c.MemberConstraintData // TODO: Full member signature hash?
            Fable.Constraint.HasMember(d.MemberName, d.MemberIsStatic) |> Some
        elif c.IsSupportsNullConstraint then Some Fable.Constraint.IsNullable
        elif c.IsRequiresDefaultConstructorConstraint then Some Fable.Constraint.HasDefaultConstructor
        elif c.IsNonNullableValueTypeConstraint then Some Fable.Constraint.IsValueType
        elif c.IsReferenceTypeConstraint then Some Fable.Constraint.IsReferenceType
        elif c.IsComparisonConstraint then Some Fable.Constraint.HasComparison
        elif c.IsEqualityConstraint then Some Fable.Constraint.HasEquality
        elif c.IsUnmanagedConstraint then Some Fable.Constraint.IsUnmanaged
        else None // TODO: Document these cases

    static member Constraints(gen: FSharpGenericParameter) =
        gen.Constraints |> Seq.choose FsGenParam.Constraint

    interface Fable.GenericParam with
        member _.Name = TypeHelpers.genParamName gen
        member _.Constraints = FsGenParam.Constraints gen

type FsParam =
    static member Make(name, typ): Fable.Parameter =
        { Name = name; Type = TypeHelpers.makeType Map.empty typ }
    static member Make(p: FSharpParameter) = FsParam.Make(p.Name, p.Type)
    static member Make(p: FSharpAbstractParameter) = FsParam.Make(p.Name, p.Type)

type FsDeclaredType(ent: FSharpEntity, genArgs: IList<FSharpType>) =
    interface Fable.DeclaredType with
        member _.Entity = FsEnt.Ref ent
        member _.GenericArgs = genArgs |> Seq.mapToList (TypeHelpers.makeType Map.empty)

type FsMemberFunctionOrValue(m: FSharpMemberOrFunctionOrValue) =
    static member CurriedParameterGroups(m: FSharpMemberOrFunctionOrValue): Fable.Parameter list list =
        m.CurriedParameterGroups
        |> Seq.mapToList (Seq.mapToList (fun p -> FsParam.Make(p)))

    static member TryParamObjectIndex(m: FSharpMemberOrFunctionOrValue) =
        m.Attributes
        |> Helpers.tryFindAtt Atts.paramObject
        |> Option.map (fun (att: FSharpAttribute) ->
            match Seq.tryItem 0 att.ConstructorArguments with
            | Some(_, (:?int as index)) -> index
            | _ -> 0)

    static member CallMemberInfo(m: FSharpMemberOrFunctionOrValue): Fable.CallMemberInfo =
        let namedParamsIndex =
            FsMemberFunctionOrValue.TryParamObjectIndex(m)
            |> Option.defaultValue -1

        { CurriedParameterGroups =
            m.CurriedParameterGroups |> Seq.mapToList (Seq.mapToListIndexed (fun i p ->
                let isNamed = namedParamsIndex > -1 && i >= namedParamsIndex
                { Name = p.Name; Type = TypeHelpers.makeType Map.empty p.Type; IsNamed = isNamed }))
          IsInstance = m.IsInstanceMember
          IsGetter = m.IsPropertyGetterMethod
          FullName = m.FullName
          CompiledName = m.CompiledName
          DeclaringEntity = m.DeclaringEntity |> Option.map (FsEnt.Ref) }

    static member DisplayName(m: FSharpMemberOrFunctionOrValue) =
        Naming.removeGetSetPrefix m.DisplayNameCore

    interface Fable.MemberFunctionOrValue with
        member _.Attributes =
            m.Attributes |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)

        // These two properties are only used for member declarations,
        // setting them to false for now
        member _.IsMangled = false
        member _.IsEnumerator = false

        member _.HasSpread = Helpers.hasParamArray m
        member _.IsInline = Helpers.isInline m
        member _.IsPublic = Helpers.isPublicMember m
        // NOTE: Using memb.IsValue doesn't work for function values
        // See isModuleValueForDeclarations below
        member _.IsValue = m.IsValue
        member _.IsInstance = m.IsInstanceMember
        member _.IsMutable = m.IsMutable
        member _.IsGetter = m.IsPropertyGetterMethod
        member _.IsSetter = m.IsPropertySetterMethod
        member _.IsProperty = m.IsProperty

        member _.DisplayName = FsMemberFunctionOrValue.DisplayName m
        member _.CompiledName = m.CompiledName
        member _.FullName = m.FullName
        member _.GenericParameters = m.GenericParameters |> Seq.mapToList (fun x -> FsGenParam(x) :> Fable.GenericParam)
        member _.CurriedParameterGroups = FsMemberFunctionOrValue.CurriedParameterGroups(m)
        member _.ReturnParameter = FsParam.Make(m.ReturnParameter)
        member _.IsExplicitInterfaceImplementation = m.IsExplicitInterfaceImplementation
        member _.ApparentEnclosingEntity = FsEnt.Ref m.ApparentEnclosingEntity

type FsEnt(ent: FSharpEntity) =
    static let tryArrayFullName (ent: FSharpEntity) =
        if ent.IsArrayType then
            let rank =
                match ent.ArrayRank with
                | rank when rank > 1 -> "`" + string rank
                | _ -> ""
            Some("System.Array" + rank)
        else None

    member _.FSharpEntity = ent

    static member SourcePath (ent: FSharpEntity) =
        ent.DeclarationLocation.FileName
        |> Path.normalizePathAndEnsureFsExtension

    static member IsPublic (ent: FSharpEntity) =
        not ent.Accessibility.IsPrivate

    static member FullName (ent: FSharpEntity): string =
        let ent = Helpers.nonAbbreviatedDefinition ent
        match tryArrayFullName ent with
        | Some fullName -> fullName
        | None when ent.IsNamespace || ent.IsByRef ->
            match ent.Namespace with
            | Some ns -> ns + "." + ent.CompiledName
            | None -> ent.CompiledName
#if !FABLE_COMPILER
        | None when ent.IsProvided ->
            ent.LogicalName
#endif
        | None ->
            match ent.TryFullName with
            | Some n -> n
            | None -> ent.LogicalName

    static member Ref (ent: FSharpEntity): Fable.EntityRef =
        let path =
            match ent.Assembly.FileName with
            | Some asmPath ->
                let dllName = Path.GetFileName(asmPath)
                let dllName = dllName.Substring(0, dllName.Length - 4) // Remove .dll extension
                match dllName with
                // When compiling with netcoreapp target, netstandard only contains redirects
                // We can find the actual assembly name from the entity qualified name
                | "netstandard" ->
                    ent.QualifiedName.Split(',').[1].Trim() |> Fable.CoreAssemblyName
                | Naming.fablePrecompile ->
                    let sourcePath = FsEnt.SourcePath ent
                    Fable.PrecompiledLib(sourcePath, Path.normalizePath asmPath)
                | dllName when Compiler.CoreAssemblyNames.Contains(dllName) ->
                    Fable.CoreAssemblyName dllName
                | _ ->
                    Path.normalizePath asmPath |> Fable.AssemblyPath
            | None ->
                FsEnt.SourcePath ent |> Fable.SourcePath
        { FullName = FsEnt.FullName ent
          Path = path }

    interface Fable.Entity with
        member _.Ref = FsEnt.Ref ent
        member _.DisplayName = ent.DisplayName
        member _.FullName = FsEnt.FullName ent

        member _.BaseType =
            match TypeHelpers.getBaseEntity ent with
            | Some(baseEntity, baseGenArgs) -> Some(upcast FsDeclaredType(baseEntity, baseGenArgs))
            | _ -> None

        member _.Attributes =
            ent.Attributes |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)

        member _.MembersFunctionsAndValues =
            ent.TryGetMembersFunctionsAndValues() |> Seq.map (fun x ->
                FsMemberFunctionOrValue(x) :> Fable.MemberFunctionOrValue)

        member _.AllInterfaces =
            ent.AllInterfaces |> Seq.choose (fun ifc ->
                if ifc.HasTypeDefinition then
                    Some(upcast FsDeclaredType(ifc.TypeDefinition, ifc.GenericArguments))
                else None)

        member _.GenericParameters =
            ent.GenericParameters |> Seq.mapToList (fun x -> FsGenParam(x) :> Fable.GenericParam)

        member _.FSharpFields =
            ent.FSharpFields |> Seq.mapToList (fun x -> FsField(x) :> Fable.Field)

        member _.UnionCases =
            ent.UnionCases |> Seq.mapToList (fun x -> FsUnionCase(x) :> Fable.UnionCase)

        member _.IsPublic = FsEnt.IsPublic ent
        member _.IsFSharpModule = ent.IsFSharpModule
        member _.IsFSharpUnion = ent.IsFSharpUnion
        member _.IsFSharpRecord = ent.IsFSharpRecord
        member _.IsFSharpAbbreviation = ent.IsFSharpAbbreviation
        member _.IsFSharpExceptionDeclaration = ent.IsFSharpExceptionDeclaration
        member _.IsValueType = ent.IsValueType
        member _.IsInterface = ent.IsInterface
        member _.IsMeasure = ent.IsMeasure
        member _.IsByRef = ent.IsByRef
        member _.IsEnum = ent.IsEnum

type MemberInfo(?attributes: FSharpAttribute seq,
                    ?hasSpread: bool,
                    ?isInline: bool,
                    ?isPublic: bool,
                    ?isInstance: bool,
                    ?isValue: bool,
                    ?isMutable: bool,
                    ?isGetter: bool,
                    ?isSetter: bool,
                    ?isProperty: bool,
                    ?isEnumerator: bool,
                    ?isMangled: bool) =
    interface Fable.MemberInfo with
        member _.Attributes =
            match attributes with
            | Some atts -> atts |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
            | None -> upcast []
        member _.HasSpread = defaultArg hasSpread false
        member _.IsInline = defaultArg isInline false
        member _.IsPublic = defaultArg isPublic true
        member _.IsInstance = defaultArg isInstance true
        member _.IsValue = defaultArg isValue false
        member _.IsMutable = defaultArg isMutable false
        member _.IsGetter = defaultArg isGetter false
        member _.IsSetter = defaultArg isSetter false
        member _.IsProperty = defaultArg isProperty false
        member _.IsEnumerator = defaultArg isEnumerator false
        member _.IsMangled = defaultArg isMangled false

type Scope = (FSharpMemberOrFunctionOrValue option * Fable.Ident * Fable.Expr option) list

type Context =
    { Scope: Scope
      ScopeInlineValues: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      ScopeInlineArgs: (Fable.Ident * Fable.Expr) list
      UsedNamesInRootScope: Set<string>
      UsedNamesInDeclarationScope: HashSet<string>
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
        { Scope = []
          ScopeInlineValues = []
          ScopeInlineArgs = []
          UsedNamesInRootScope = defaultArg usedRootNames Set.empty
          UsedNamesInDeclarationScope = Unchecked.defaultof<_>
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
    abstract ResolveInlineExpr: Context * InlineExpr * Fable.Expr list
        -> (Fable.Ident * Fable.Expr) list * Fable.Expr
    abstract TryReplace: Context * SourceLocation option * Fable.Type *
        info: Fable.ReplaceCallInfo * thisArg: Fable.Expr option * args: Fable.Expr list -> Fable.Expr option
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Helpers =

    let rec nonAbbreviatedDefinition (ent: FSharpEntity): FSharpEntity =
        if ent.IsFSharpAbbreviation then
            let t = ent.AbbreviatedType
            if t.HasTypeDefinition && t.TypeDefinition <> ent
            then nonAbbreviatedDefinition t.TypeDefinition
            else ent
        else ent

    let rec nonAbbreviatedType (t: FSharpType): FSharpType =
        let isSameType (t1: FSharpType) (t2: FSharpType) =
            t1.HasTypeDefinition && t2.HasTypeDefinition && (t1.TypeDefinition = t2.TypeDefinition)
        if t.IsAbbreviation && not (isSameType t t.AbbreviatedType) then
            nonAbbreviatedType t.AbbreviatedType
        elif t.HasTypeDefinition then
            let abbr = t.AbbreviatedType
            // .IsAbbreviation doesn't eval to true for generic numbers
            // See https://github.com/Microsoft/visualfsharp/issues/5992
            if t.GenericArguments.Count = abbr.GenericArguments.Count then t
            else abbr
        else t

    let getGenericArguments (t: FSharpType) =
        // Accessing .GenericArguments for a generic parameter will fail
        if t.IsGenericParameter
        then [||] :> IList<_>
        else (nonAbbreviatedType t).GenericArguments

    type TrimRootModule =
        | TrimRootModule of Compiler
        | NoTrimRootModule

    let private getEntityMangledName trimRootModule (ent: Fable.EntityRef) =
        let fullName = ent.FullName
        match trimRootModule, ent.Path with
        | TrimRootModule com, (Fable.SourcePath sourcePath | Fable.PrecompiledLib(sourcePath, _)) ->
            let rootMod = com.GetRootModule(sourcePath)
            if fullName.StartsWith(rootMod) then
                fullName.Substring(rootMod.Length).TrimStart('.')
            else fullName
        // Ignore entities for which we don't have implementation file data
        | TrimRootModule _, (Fable.AssemblyPath _ | Fable.CoreAssemblyName _)
        | NoTrimRootModule, _ -> fullName

    let cleanNameAsJsIdentifier (name: string) =
        if name = ".ctor" then "$ctor"
        else name.Replace('.','_').Replace('`','$')

    let cleanNameAsRustIdentifier (name: string) =
        let name = Regex.Replace(name, @"[\s`'"".]", "_")
        let name = Regex.Replace(name, @"[^\w]",
            fun c -> String.Format(@"{0:x4}", int c.Value.[0]))
        name

    let memberNameAsRustIdentifier (name: string) part =
        let f = cleanNameAsRustIdentifier
        let join sep s o = (f s) + (if o = "" then "" else sep + o)
        match part with
        | Naming.InstanceMemberPart(s, o) -> join "_" s o, Naming.NoMemberPart
        | Naming.StaticMemberPart(s, o) -> join "__" s o, Naming.NoMemberPart
        | Naming.NoMemberPart -> f name, part.Replace(f)

    let getEntityDeclarationName (com: Compiler) (entRef: Fable.EntityRef) =
        let entityName = getEntityMangledName (TrimRootModule com) entRef
        let name, part = (entityName |> cleanNameAsJsIdentifier, Naming.NoMemberPart)
        let sanitizedName =
            match com.Options.Language with
            | Python -> Fable.PY.Naming.sanitizeIdent Fable.PY.Naming.pyBuiltins.Contains name part
            | Rust -> entityName |> cleanNameAsRustIdentifier
            | _ -> Naming.sanitizeIdent (fun _ -> false) name part
        sanitizedName

    let private getMemberMangledName trimRootModule (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsExtensionMember then
            let overloadSuffix = FsMemberFunctionOrValue memb |> OverloadSuffix.getExtensionHash
            let entName = FsEnt.Ref memb.ApparentEnclosingEntity |> getEntityMangledName NoTrimRootModule
            entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
        else
            match memb.DeclaringEntity with
            | Some ent ->
                let entRef = FsEnt.Ref ent
                let entName = getEntityMangledName trimRootModule entRef
                if ent.IsFSharpModule then
                    match trimRootModule, entName with
                    | TrimRootModule com, _ when com.Options.Language = Rust ->
                        memb.CompiledName, Naming.NoMemberPart // module prefix for Rust
                    | _, "" ->
                        memb.CompiledName, Naming.NoMemberPart
                    | _, moduleName ->
                        moduleName, Naming.StaticMemberPart(memb.CompiledName, "")
                else
                    let overloadSuffix = FsMemberFunctionOrValue memb |> OverloadSuffix.getHash (FsEnt ent)
                    if memb.IsInstanceMember
                    then entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
                    else entName, Naming.StaticMemberPart(memb.CompiledName, overloadSuffix)
            | None -> memb.CompiledName, Naming.NoMemberPart

    /// Returns the sanitized name for the member declaration and whether it has an overload suffix
    let getMemberDeclarationName (com: Compiler) (memb: FSharpMemberOrFunctionOrValue) =
        let name, part = getMemberMangledName (TrimRootModule com) memb
        let name, part =
            match com.Options.Language with
            | Rust -> memberNameAsRustIdentifier name part
            | _ -> cleanNameAsJsIdentifier name, part.Replace(cleanNameAsJsIdentifier)

        let sanitizedName =
            match com.Options.Language with
            | Python ->
                let name =
                    // Don't snake_case if member has compiled name attribute
                    match memb.Attributes |> Helpers.tryFindAtt Atts.compiledName with
                    | Some _ -> name
                    | _ -> Fable.PY.Naming.toSnakeCase name
                Fable.PY.Naming.sanitizeIdent Fable.PY.Naming.pyBuiltins.Contains name part
            | Rust -> Naming.buildNameWithoutSanitation name part
            | _ -> Naming.sanitizeIdent (fun _ -> false) name part
        let hasOverloadSuffix = not (String.IsNullOrEmpty(part.OverloadSuffix))
        sanitizedName, hasOverloadSuffix

    /// Used to identify members uniquely in the inline expressions dictionary
    let getMemberUniqueName (memb: FSharpMemberOrFunctionOrValue): string =
        getMemberMangledName NoTrimRootModule memb
        ||> Naming.buildNameWithoutSanitation

    let getMemberDisplayName (memb: FSharpMemberOrFunctionOrValue) =
        FsMemberFunctionOrValue.DisplayName memb

    let isUsedName (ctx: Context) name =
        ctx.UsedNamesInRootScope.Contains name || ctx.UsedNamesInDeclarationScope.Contains name

    let getIdentUniqueName (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (isUsedName ctx)
        ctx.UsedNamesInDeclarationScope.Add(name) |> ignore
        name

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition then
            typ.TypeDefinition.TryFullName = Some Types.unit
        else false

    let isByRefValue (value: FSharpMemberOrFunctionOrValue) =
        // Value type "this" is passed as inref, so it has to be excluded
        // (Note: the non-abbreviated type of inref and outref is byref)
        let typ = value.FullType
        value.IsValue && not (value.IsMemberThisValue)
        && typ.HasTypeDefinition
        && typ.TypeDefinition.IsByRef
        // && (typ.TypeDefinition.DisplayName = "byref" ||
        //     typ.TypeDefinition.DisplayName = "inref" ||
        //     typ.TypeDefinition.DisplayName = "outref")

    let tryFindAtt fullName (atts: FSharpAttribute seq) =
        atts |> Seq.tryPick (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some fullName' ->
                if fullName = fullName' then Some att else None
            | None -> None)

    let hasAttribute attFullName (attributes: FSharpAttribute seq) =
        let mutable found = false
        let attFullName = Some attFullName
        for att in attributes do
            if not found then
                found <- (nonAbbreviatedDefinition att.AttributeType).TryFullName = attFullName
        found

    let tryPickAttribute attFullNames (attributes: FSharpAttribute seq) =
        let attFullNames = Map attFullNames
        attributes |> Seq.tryPick (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some fullName -> Map.tryFind fullName attFullNames
            | None -> None)

    let tryAttributeConsArg (att: FSharpAttribute) index (defValue: 'T) (f: obj -> 'T option) =
        let consArgs = att.ConstructorArguments
        if consArgs.Count <= index then defValue
        else
            consArgs.[index] |> snd |> f
            |> Option.defaultValue defValue

    let tryBoolean: obj -> bool option = function (:? bool as x) -> Some x | _ -> None
    let tryString: obj -> string option = function (:? string as x) -> Some x | _ -> None

    let tryDefinition (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition then
            let tdef = typ.TypeDefinition
            Some(tdef, tdef.TryFullName)
        else None

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

    let isPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then false
        else not memb.Accessibility.IsPrivate

    let isNonPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then true
        else not memb.Accessibility.IsPublic

    let makeRange (r: Range) =
        { start = { line = r.StartLine; column = r.StartColumn }
          ``end``= { line = r.EndLine; column = r.EndColumn }
          identifierName = None }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

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

    let isModuleValueForCalls (declaringEntity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        declaringEntity.IsFSharpModule
        && isModuleValueForDeclarations memb
        && memb.CurriedParameterGroups.Count = 0 && memb.GenericParameters.Count = 0
        // Mutable public values must be called as functions (see #986)
        && (not memb.IsMutable || not (isPublicMember memb))

    let rec getAllInterfaceMembers (ent: FSharpEntity) =
        seq {
            yield! ent.MembersFunctionsAndValues
            for parent in ent.DeclaredInterfaces do
                match tryDefinition parent with
                | Some(e, _) -> yield! getAllInterfaceMembers e
                | None -> ()
        }

    /// Test if the name corresponds to this interface or anyone in its hierarchy
    let rec testInterfaceHierarchy interfaceFullname interfaceType =
        match tryDefinition interfaceType with
        | Some(e, Some fullname2) ->
            if interfaceFullname = fullname2
            then true
            else e.DeclaredInterfaces
                 |> Seq.exists (testInterfaceHierarchy interfaceFullname)
        | _ -> false

    let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =

        let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
            if memb.CurriedParameterGroups.Count <> 1 then false else
            let args = memb.CurriedParameterGroups.[0]
            args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.map (fun lastParam -> hasAttribute Atts.paramList lastParam.Attributes)
            |> Option.defaultValue false

        hasParamArray memb || hasParamSeq memb

    type UnionPattern =
        | OptionUnion of FSharpType * isStruct: bool
        | ListUnion of FSharpType
        | ErasedUnion of FSharpEntity * IList<FSharpType> * CaseRules
        | ErasedUnionCase
        | TypeScriptTaggedUnion of FSharpEntity * IList<FSharpType> * tagName:string * CaseRules
        | StringEnum of FSharpEntity * CaseRules
        | DiscriminatedUnion of FSharpEntity * IList<FSharpType>

    let getUnionPattern (typ: FSharpType) (unionCase: FSharpUnionCase) : UnionPattern =
        let typ = nonAbbreviatedType typ
        let getCaseRule (att: FSharpAttribute) =
            match Seq.tryHead att.ConstructorArguments with
            | Some(_, (:? int as rule)) -> enum<CaseRules>(rule)
            | _ -> CaseRules.LowerFirst

        unionCase.Attributes |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some Atts.erase -> Some ErasedUnionCase
            | _ -> None)
        |> Option.defaultWith (fun () ->
            match tryDefinition typ with
            | None -> failwith "Union without definition"
            | Some(tdef, fullName) ->
                match defaultArg fullName tdef.CompiledName with
                | Types.valueOption -> OptionUnion (typ.GenericArguments.[0], true)
                | Types.option -> OptionUnion (typ.GenericArguments.[0], false)
                | Types.list -> ListUnion typ.GenericArguments.[0]
                | _ ->
                    tdef.Attributes |> Seq.tryPick (fun att ->
                        match att.AttributeType.TryFullName with
                        | Some Atts.erase -> Some (ErasedUnion(tdef, typ.GenericArguments, getCaseRule att))
                        | Some Atts.stringEnum -> Some (StringEnum(tdef, getCaseRule att))
                        | Some Atts.tsTaggedUnion ->
                            match Seq.tryItem 0 att.ConstructorArguments, Seq.tryItem 1 att.ConstructorArguments with
                            | Some (_, (:? string as name)), None ->
                                Some (TypeScriptTaggedUnion(tdef, typ.GenericArguments, name, CaseRules.LowerFirst))
                            | Some (_, (:? string as name)), Some (_, (:? int as rule)) ->
                                Some (TypeScriptTaggedUnion(tdef, typ.GenericArguments, name, enum<CaseRules>(rule)))
                            | _ -> failwith "Invalid TypeScriptTaggedUnion attribute"
                        | _ -> None)
                    |> Option.defaultValue (DiscriminatedUnion(tdef, typ.GenericArguments))
        )

module Patterns =
    open FSharpExprPatterns
    open Helpers

    let inline (|Rev|) x = List.rev x
    let inline (|AsArray|) x = Array.ofSeq x
    let inline (|LazyValue|) (x: Lazy<'T>) = x.Value
    let inline (|Transform|) (com: IFableCompiler) ctx e = com.Transform(ctx, e)
    let inline (|FieldName|) (fi: FSharpField) = fi.Name

    let (|CommonNamespace|_|) = function
        | (FSharpImplementationFileDeclaration.Entity(ent, subDecls))::restDecls
            when ent.IsNamespace ->
            let commonName = ent.CompiledName
            (Some subDecls, restDecls) ||> List.fold (fun acc decl ->
                match acc, decl with
                | (Some subDecls), (FSharpImplementationFileDeclaration.Entity(ent, subDecls2)) ->
                    if ent.CompiledName = commonName
                    then Some(subDecls@subDecls2)
                    else None
                | _ -> None)
            |> Option.map (fun subDecls -> ent, subDecls)
        | _ -> None

    let inline (|NonAbbreviatedType|) (t: FSharpType) =
        nonAbbreviatedType t

    let (|IgnoreAddressOf|) (expr: FSharpExpr) =
        match expr with
        | AddressOf value -> value
        | _ -> expr

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then Some t.TypeDefinition else None

    /// DOES NOT check if the type is abbreviated, mainly intended to identify Fable.Core.Applicable
    let (|FSharpExprTypeFullName|_|) (e: FSharpExpr) =
        let t = e.Type
        if t.HasTypeDefinition then t.TypeDefinition.TryFullName else None

    let (|MemberFullName|) (memb: FSharpMemberOrFunctionOrValue) =
        memb.FullName

    let (|RefType|_|) = function
        | TypeDefinition tdef as t when tdef.TryFullName = Some Types.reference -> Some t
        | _ -> None

    /// Detects AST pattern of "raise MatchFailureException()"
    let (|RaisingMatchFailureExpr|_|) (expr: FSharpExpr) =
        match expr with
        | Call(None, methodInfo, [ ], [_unitType], [value]) ->
            match methodInfo.FullName with
            | "Microsoft.FSharp.Core.Operators.raise" ->
                match value with
                | NewRecord(recordType, [Const (value, _valueT) ; _rangeFrom; _rangeTo]) ->
                    match recordType.TypeDefinition.TryFullName with
                    | Some "Microsoft.FSharp.Core.MatchFailureException" -> Some (value.ToString())
                    | _ -> None
                | _ -> None
            | _ -> None
        | _ -> None

    let (|NestedLambda|_|) x =
        let rec nestedLambda args = function
            | Lambda(arg, body) -> nestedLambda (arg::args) body
            | body -> List.rev args, body
        match x with
        | Lambda(arg, body) -> nestedLambda [arg] body |> Some
        | _ -> None

    let (|ForOf|_|) = function
        | Let((_, value), // Coercion to seq
              Let((_, Call(None, meth, _, [], [])),
                TryFinally(
                  WhileLoop(_,
                    Let((ident, _), body)), _)))
        | Let((_, Call(Some value, meth, _, [], [])),
                TryFinally(
                    WhileLoop(_,
                        Let((ident, _), body)), _))
            // Using only the compiled name is riskier but with the fullname we miss some cases
            // TODO: Check the return type of meth is or implements IEnumerator
            when meth.CompiledName = "GetEnumerator" ->
            // when meth.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator" ->
            Some(ident, value, body)
        // optimized "for x in list"
        | Let((_, UnionCaseGet(value, typ, unionCase, field)),
                WhileLoop(_, Let((ident, _), body)))
            when (getFsTypeFullName typ) = Types.list
                && unionCase.Name = "op_ColonColon" && field.Name = "Tail" ->
            Some (ident, value, body)
        // optimized "for _x in list"
        | Let((ident, UnionCaseGet(value, typ, unionCase, field)),
                WhileLoop(_, body))
            when (getFsTypeFullName typ) = Types.list
                && unionCase.Name = "op_ColonColon" && field.Name = "Tail" ->
            Some (ident, value, body)
        | _ -> None

    /// This matches the boilerplate generated for TryGetValue/TryParse/DivRem (see #154, or #1744)
    /// where the F# compiler automatically passes a byref arg and returns it as a tuple
    let (|ByrefArgToTuple|_|) = function
        | Let((outArg1, (DefaultValue _ as def)),
                NewTuple(_, [Call(callee, memb, ownerGenArgs, membGenArgs, callArgs); Value outArg3]))
                when List.isMultiple callArgs && outArg1.IsCompilerGenerated && outArg1 = outArg3 ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some (callee, memb, ownerGenArgs, membGenArgs, callArgs@[def])
            | _ -> None
        | _ -> None

    /// This matches the boilerplate generated for TryGetValue/TryParse/DivRem (--optimize+)
    let (|ByrefArgToTupleOptimizedIf|_|) = function
        | Let((outArg1, (DefaultValue _ as def)), IfThenElse
                (Call(callee, memb, ownerGenArgs, membGenArgs, callArgs), thenExpr, elseExpr))
                when List.isMultiple callArgs && outArg1.IsCompilerGenerated ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some (outArg1, callee, memb, ownerGenArgs, membGenArgs, callArgs@[def], thenExpr, elseExpr)
            | _ -> None
        | _ -> None

    /// This matches another boilerplate generated for TryGetValue/TryParse/DivRem (--optimize+)
    let (|ByrefArgToTupleOptimizedTree|_|) = function
        | Let((outArg1, (DefaultValue _ as def)), DecisionTree(IfThenElse
                (Call(callee, memb, ownerGenArgs, membGenArgs, callArgs), thenExpr, elseExpr), targetsExpr))
                when List.isMultiple callArgs && outArg1.IsCompilerGenerated ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some (outArg1, callee, memb, ownerGenArgs, membGenArgs, callArgs@[def], thenExpr, elseExpr, targetsExpr)
            | _ -> None
        | _ -> None

    /// This matches another boilerplate generated for TryGetValue/TryParse/DivRem (--crossoptimize-)
    let (|ByrefArgToTupleOptimizedLet|_|) = function
        | Let((outArg1, (DefaultValue _ as def)),
                Let((arg_0, Call(callee, memb, ownerGenArgs, membGenArgs, callArgs)), restExpr))
                when List.isMultiple callArgs && outArg1.IsCompilerGenerated ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some (arg_0, outArg1, callee, memb, ownerGenArgs, membGenArgs, callArgs@[def], restExpr)
            | _ -> None
        | _ -> None

    /// This matches the boilerplate generated to wrap .NET events from F#
    let (|CreateEvent|_|) = function
        | Call(None,createEvent,_,_,
               [Lambda(_eventDelegate, Call(Some callee, addEvent,[],[],[Value _eventDelegate']));
                Lambda(_eventDelegate2, Call(Some _callee2, _removeEvent,[],[],[Value _eventDelegate2']));
                Lambda(_callback, NewDelegate(_, Lambda(_delegateArg0, Lambda(_delegateArg1, Application(Value _callback',[],[Value _delegateArg0'; Value _delegateArg1'])))))])
          when createEvent.FullName = Types.createEvent ->
            let eventName = addEvent.CompiledName.Replace("add_","")
            match addEvent.DeclaringEntity with
            | Some klass ->
                klass.MembersFunctionsAndValues
                |> Seq.tryFind (fun m -> m.LogicalName = eventName)
                |> function
                | Some memb -> Some (callee, memb)
                | _ -> None
            | _ -> None
        | _ -> None

    let (|ConstructorCall|_|) = function
        | NewObject(baseCall, genArgs, baseArgs) -> Some(baseCall, genArgs, baseArgs)
        | Call(None, baseCall, genArgs1, genArgs2, baseArgs) when baseCall.IsConstructor ->
            Some(baseCall, genArgs1 @ genArgs2, baseArgs)
        | _ -> None

    let (|OptimizedOperator|_|) (com: Compiler) fsExpr =
        if com.Options.OptimizeFSharpAst then
            match fsExpr with
            // work-around for optimized string operator (Operators.string)
            | Let((var, Call(None, memb, _, membArgTypes, membArgs)),
                                DecisionTree(IfThenElse(_, _, IfThenElse
                                                            (TypeTest(tt, Value vv), _, _)), _))
                    when var.FullName = "matchValue" && memb.FullName = "Microsoft.FSharp.Core.Operators.box"
                        && vv.FullName = "matchValue" && (getFsTypeFullName tt) = "System.IFormattable" ->
                Some(memb, None, "toString", membArgTypes, membArgs)
            // work-around for optimized hash operator (Operators.hash)
            | Call(Some expr, memb, _, [], [Call(None, comp, [], [], [])])
                    when memb.FullName.EndsWith(".GetHashCode") &&
                         comp.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityERComparer" ->
                Some(memb, Some comp, "GenericHash", [expr.Type], [expr])
            // work-around for optimized equality operator (Operators.(=))
            | Call(Some e1, memb, _, [], [Coerce (t2, e2); Call(None, comp, [], [], [])])
                    when memb.FullName.EndsWith(".Equals") && t2.HasTypeDefinition && t2.TypeDefinition.CompiledName = "obj" &&
                         comp.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityComparer" ->
                Some(memb, Some comp, "GenericEquality", [e1.Type; e2.Type], [e1; e2])
            | _ -> None
        else None

    let inline (|FableType|) _com (ctx: Context) t = TypeHelpers.makeType ctx.GenericArgs t

module TypeHelpers =
    open Helpers
    open Patterns

    let genParamName (genParam: FSharpGenericParameter) =
        // Sometimes the names of user-declared and compiler-generated clash, see #1900
        //if genParam.IsCompilerGenerated then genParam.Name.Replace("?", "$") + "$" else genParam.Name

        // Conflicting generic param names seem to have been solved in FCS
        genParam.Name

    let resolveGenParam ctxTypeArgs (genParam: FSharpGenericParameter) =
        let name = genParamName genParam
        match Map.tryFind name ctxTypeArgs with
        | None ->
            let constraints = FsGenParam.Constraints genParam |> Seq.toList
            Fable.GenericParam(name, constraints)
        | Some typ -> typ

    // TODO: We need to filter the measure generic arguments
    // But for that we need to pass the compiler here, which needs a bigger refactoring
    let makeTypeGenArgs ctxTypeArgs (genArgs: IList<FSharpType>) =
        genArgs |> Seq.map (fun genArg ->
            if genArg.IsGenericParameter
            then resolveGenParam ctxTypeArgs genArg.GenericParameter
            else makeType ctxTypeArgs genArg)
        |> Seq.toList

    let makeTypeFromDelegate ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        let invokeArgs() =
            let invokeMember =
                tdef.MembersFunctionsAndValues
                |> Seq.find (fun f -> f.DisplayName = "Invoke")
            invokeMember.CurriedParameterGroups.[0] |> Seq.map (fun p -> p.Type),
            invokeMember.ReturnParameter.Type
        let argTypes, returnType =
            try
                // tdef.FSharpDelegateSignature doesn't work with System.Func & friends
                if tdef.IsFSharp then
                    tdef.FSharpDelegateSignature.DelegateArguments |> Seq.map snd,
                    tdef.FSharpDelegateSignature.DelegateReturnType
                else invokeArgs()
            with _ -> invokeArgs()

        let genArgs = Seq.zip (tdef.GenericParameters |> Seq.map genParamName) genArgs |> Map
        let resolveType (t: FSharpType) =
            if t.IsGenericParameter then Map.find (genParamName t.GenericParameter) genArgs else t
        let returnType = returnType |> resolveType |> makeType ctxTypeArgs
        let argTypes =
            argTypes
            |> Seq.map (resolveType >> makeType ctxTypeArgs)
            |> Seq.toList
            |> function [Fable.Unit] -> [] | argTypes -> argTypes
        Fable.DelegateType(argTypes, returnType)

    let numberTypes =
        dict [
            Types.int8, Int8
            Types.uint8, UInt8
            Types.int16, Int16
            Types.uint16, UInt16
            Types.int32, Int32
            Types.uint32 , UInt32
            Types.int64, Int64
            Types.uint64 , UInt64
            Types.bigint, BigInt
            Types.nativeint, NativeInt
            Types.unativeint, UNativeInt
            Types.float32, Float32
            Types.float64, Float64
            Types.decimal, Decimal
        ]

    let numbersWithMeasure =
        dict [
            "Microsoft.FSharp.Core.sbyte`1", Int8
            "FSharp.UMX.byte`1", UInt8
            "Microsoft.FSharp.Core.int16`1", Int16
            "Microsoft.FSharp.Core.int`1", Int32
            "Microsoft.FSharp.Core.int64`1", Int64
            "FSharp.UMX.uint64`1", UInt64
            "Microsoft.FSharp.Core.nativeint`1", NativeInt
            "Microsoft.FSharp.Core.float32`1", Float32
            "Microsoft.FSharp.Core.float`1", Float64
            "Microsoft.FSharp.Core.decimal`1", Decimal
        ]

    // FCS doesn't expose the abbreviated type of a MeasureAnnotatedAbbreviation,
    // so we need to hard-code FSharp.UMX types
    let runtimeTypesWithMeasure =
        dict [
            "FSharp.UMX.bool`1", Choice1Of2 Fable.Boolean
            "FSharp.UMX.string`1", Choice1Of2 Fable.String
            "FSharp.UMX.Guid`1", Choice2Of2 Types.guid
            "FSharp.UMX.TimeSpan`1", Choice2Of2 Types.timespan
            "FSharp.UMX.DateTime`1", Choice2Of2 Types.datetime
            "FSharp.UMX.DateTimeOffset`1", Choice2Of2 Types.datetimeOffset
        ]

    let private getMeasureFullName (genArgs: IList<FSharpType>) =
        if genArgs.Count > 0 then
            // TODO: Check it's effectively measure?
            // TODO: Raise error if we cannot get the measure fullname?
            match tryDefinition genArgs.[0] with
            | Some(_, Some fullname) -> fullname
            | _ -> Naming.unknown
        else Naming.unknown

    let private makeRuntimeTypeWithMeasure (genArgs: IList<FSharpType>) fullName =
        let genArgs = [getMeasureFullName genArgs |> Fable.Measure]
        let r: Fable.EntityRef =
            { FullName = fullName
              Path = Fable.CoreAssemblyName "System.Runtime" }
        Fable.DeclaredType(r, genArgs)

    let private makeFSharpCoreType fullName =
            let r: Fable.EntityRef =
                { FullName = fullName
                  Path = Fable.CoreAssemblyName "FSharp.Core" }
            Fable.DeclaredType(r, [])

    let makeTypeFromDef ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        if tdef.IsArrayType then
            makeTypeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.Array
        elif tdef.IsDelegate then
            makeTypeFromDelegate ctxTypeArgs genArgs tdef
        elif tdef.IsEnum then
            // F# seems to include a field with this name in the underlying type
            let numberKind =
                tdef.FSharpFields |> Seq.tryPick (fun fi ->
                    match fi.Name with
                    | "value__" when fi.FieldType.HasTypeDefinition ->
                        match FsEnt.FullName fi.FieldType.TypeDefinition with
                        | DicContains numberTypes kind -> Some kind
                        | _ -> None
                    | _ -> None)
                |>  Option.defaultValue Int32
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
            | Types.valueOption -> Fable.Option(makeTypeGenArgs ctxTypeArgs genArgs |> List.head, true)
            | Types.option -> Fable.Option(makeTypeGenArgs ctxTypeArgs genArgs |> List.head, false)
            | Types.resizeArray -> makeTypeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.Array
            | Types.list -> makeTypeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.List
            | DicContains numberTypes kind -> Fable.Number(kind, Fable.NumberInfo.Empty)
            | DicContains numbersWithMeasure kind ->
                let info = getMeasureFullName genArgs |> Fable.NumberInfo.IsMeasure
                Fable.Number(kind, info)
            | "Microsoft.FSharp.Core.CompilerServices.MeasureProduct`2" as fullName -> makeFSharpCoreType fullName
            | DicContains runtimeTypesWithMeasure choice ->
                match choice with
                | Choice1Of2 t -> t
                | Choice2Of2 fullName -> makeRuntimeTypeWithMeasure genArgs fullName
            | _ ->
                // Special attributes
                tdef.Attributes |> tryPickAttribute [
                    Atts.stringEnum, Fable.String
                    Atts.erase, Fable.Any
                    Atts.tsTaggedUnion, Fable.Any
                ]
                // Rest of declared types
                |> Option.defaultWith (fun () ->
                    Fable.DeclaredType(FsEnt.Ref tdef, makeTypeGenArgs ctxTypeArgs genArgs))

    let rec makeType (ctxTypeArgs: Map<string, Fable.Type>) (NonAbbreviatedType t) =
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter then
            resolveGenParam ctxTypeArgs t.GenericParameter
        // Tuple
        elif t.IsTupleType then
            let genArgs = makeTypeGenArgs ctxTypeArgs t.GenericArguments
            Fable.Tuple(genArgs, t.IsStructTupleType)
        // Function
        elif t.IsFunctionType then
            let argType = makeType ctxTypeArgs t.GenericArguments.[0]
            let returnType = makeType ctxTypeArgs t.GenericArguments.[1]
            Fable.LambdaType(argType, returnType)
        elif t.IsAnonRecordType then
            let genArgs = makeTypeGenArgs ctxTypeArgs t.GenericArguments
            let fields = t.AnonRecordTypeDetails.SortedFieldNames
            Fable.AnonymousRecordType(fields, genArgs)
        elif t.HasTypeDefinition then
// No support for provided types when compiling FCS+Fable to JS
#if !FABLE_COMPILER
            // TODO: Discard provided generated types too?
            if t.TypeDefinition.IsProvidedAndErased then Fable.Any
            else
#endif
                makeTypeFromDef ctxTypeArgs t.GenericArguments t.TypeDefinition
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    let getBaseEntity (tdef: FSharpEntity): (FSharpEntity * IList<FSharpType>) option =
        match tdef.BaseType with
        | Some(TypeDefinition baseEnt as baseType) when baseEnt.TryFullName <> Some Types.object ->
            Some(baseEnt, baseType.GenericArguments)
        | _ -> None

    let rec tryFindBaseEntity (filter: FSharpEntity -> bool) (tdef: FSharpEntity) =
        getBaseEntity tdef |> Option.bind (fun (baseEnt,_) ->
            if filter baseEnt then Some baseEnt
            else tryFindBaseEntity filter baseEnt)

    let rec getOwnAndInheritedFsharpMembers (tdef: FSharpEntity) = seq {
        yield! tdef.TryGetMembersFunctionsAndValues()
        match getBaseEntity tdef with
        | Some(baseDef, _) -> yield! getOwnAndInheritedFsharpMembers baseDef
        | _ -> ()
    }

    let getArgTypes _com (memb: FSharpMemberOrFunctionOrValue) =
        // FSharpParameters don't contain the `this` arg
        Seq.concat memb.CurriedParameterGroups
        // The F# compiler "untuples" the args in methods
        |> Seq.map (fun x -> makeType Map.empty x.Type)
        |> Seq.toList

    let isAbstract (ent: FSharpEntity) =
       hasAttribute Atts.abstractClass ent.Attributes

    let tryGetInterfaceTypeFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0
        then nonAbbreviatedType meth.ImplementedAbstractSignatures.[0].DeclaringType |> Some
        else None

    let tryGetInterfaceDefinitionFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0 then
            let t = nonAbbreviatedType meth.ImplementedAbstractSignatures.[0].DeclaringType
            if t.HasTypeDefinition then Some t.TypeDefinition else None
        else None

    let tryFindMember _com (entity: Fable.Entity) genArgs compiledName isInstance (argTypes: Fable.Type list) =
        let argsEqual (args1: Fable.Type list) args1Length (args2: IList<IList<FSharpParameter>>) =
            let args2Length = args2 |> Seq.sumBy (fun g -> g.Count)
            if args1Length = args2Length then
                let args2 =
                    args2
                    |> Seq.collect (fun g ->
                        g |> Seq.map (fun p -> makeType genArgs p.Type) |> Seq.toList)
                listEquals (typeEquals false) args1 (Seq.toList args2)
            else false

        match entity with
        | :? FsEnt as entity ->
            let argTypesLength = List.length argTypes
            getOwnAndInheritedFsharpMembers entity.FSharpEntity |> Seq.tryFind (fun m2 ->
                if m2.IsInstanceMember = isInstance && m2.CompiledName = compiledName
                then argsEqual argTypes argTypesLength m2.CurriedParameterGroups
                else false)
        | _ -> None

    let tryFindWitness (ctx: Context) argTypes isInstance traitName =
        ctx.Witnesses |> List.tryFind (fun w ->
            w.TraitName = traitName
            && w.IsInstance = isInstance
            && listEquals (typeEquals false) argTypes w.ArgTypes)

    [<Flags>]
    type private Allow =
        | TheUsual      = 0b0000
          /// Enums in F# are uint32
          /// -> Allow into all int & uint
        | EnumIntoInt   = 0b0001
          /// Erased Unions are reduced to `Any`
          /// -> Cannot distinguish between 'normal' Any (like `obj`) and Erased Union (like Erased Union with string field)
          ///
          /// For interface members the FSharp Type is available
          /// -> `Ux<...>` receive special treatment and its types are extracted
          /// -> `abstract Value: U2<int,string>` -> extract `int` & `string`
          /// BUT: for Expressions in Anon Records that's not possible, and `U2<int,string>` is only recognized as `Any`
          /// -> `{| Value = v |}`: `v: int` and `v: string` are recognized as matching,
          ///    but `v: U2<int,string>` isn't: only `Any`/`obj` as Type available
          /// To recognize as matching, we must allow all `Any` expressions for `U2` in interface place.
          ///
          /// Note: Only `Ux<...>` are currently handled (on interface side), not other Erased Unions!
        | AnyIntoErased = 0b0010
          /// Unlike `AnyIntoErased`, this allows all expressions of type `Any` in all interface properties.
          /// (The other way is always allow: Expression of all Types fits into `Any`)
        | AlwaysAny     = 0b0100

    let fitsAnonRecordInInterface
        (_com: IFableCompiler)
        (range: SourceLocation option)
        (argExprs: Fable.Expr list)
        (fieldNames: string array)
        (interface_: Fable.Entity)
        =
        match interface_ with
        | :? FsEnt as fsEnt ->
            let interface_ = fsEnt.FSharpEntity
            let interfaceMembers =
                getAllInterfaceMembers interface_
                |> Seq.toList

            let makeType = makeType Map.empty
            /// Returns for:
            /// * `Ux<...>`: extracted types from `<....>`: `U2<string,int>` -> `[String; Int]`
            /// * `Option<Ux<...>>`: extracted types from `<...>`, then made Optional: `Option<U2<string,int>>` -> `[Option String; Option Int]`
            /// * 'normal' type: `makeType`ed type: `string` -> `[String]`
            ///     Note: Erased Unions (except handled `Ux<...>`) are reduced to `Any`
            ///
            /// Extracting necessary: Erased Unions are reduced to `Any` -> special handling for `Ux<...>`
            ///
            /// Note: nested types aren't handled: `U2<string, U<int, float>>` -> `[Int; Any]`
            let rec collectTypes (ty: FSharpType) : Fable.Type list =
                // Special treatment for Ux<...> and Option<Ux<...>>: extract types in Ux
                // This is necessary because: `makeType` reduces Erased Unions (including Ux) to `Any` -> no type info any more
                //
                // Note: no handling of nested types: `U2<string, U<int, float>>` -> `int` & `float` don't get extract
                match ty with
                | UType tys ->
                    tys
                    |> List.map makeType
                    |> List.distinct
                | OptionType (UType tys, isStruct) ->
                    tys
                    |> List.map (fun t -> Fable.Option(makeType t, isStruct))
                    |> List.distinct
                | _ ->
                    makeType ty
                    |> List.singleton
            and (|OptionType|_|) (ty: FSharpType) =
                match ty with
                | TypeDefinition tdef ->
                    match FsEnt.FullName tdef with
                    | Types.valueOption -> Some(ty.GenericArguments.[0], true)
                    | Types.option -> Some(ty.GenericArguments.[0], false)
                    | _ -> None
                | _ -> None
            and (|UType|_|) (ty: FSharpType) =
                let (|UName|_|) (tdef: FSharpEntity) =
                    if
                        tdef.Namespace = Some "Fable.Core"
                        &&
                        (
                            let name = tdef.DisplayName
                            name.Length = 2 && name.[0] = 'U' && Char.IsDigit name.[1]
                        )
                    then
                        Some ()
                    else
                        None
                match ty with
                | TypeDefinition UName ->
                    ty.GenericArguments
                    |> Seq.toList
                    |> Some
                | _ -> None

            /// Special Rules mostly for Indexers:
            ///     For direct interface member implementation we want to be precise (-> exact_ish match)
            ///     But for indexer allow a bit more types like erased union with string field when indexer is string
            let fitsInto (rules: Allow) (expected: Fable.Type list) (actual: Fable.Type) =
                assert(expected |> List.isEmpty |> not)

                let (|IntNumber|_|) =
                    function
                    | Fable.Number((Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32), _) -> Some ()
                    | _ -> None
                let fitsIntoSingle (rules: Allow) (expected: Fable.Type) (actual: Fable.Type) =
                    match expected, actual with
                    | Fable.Any, _ -> true
                    | _, Fable.Any when rules.HasFlag Allow.AlwaysAny ->
                        // Erased Unions are reduced to `Any`
                        // -> cannot distinguish between 'normal' Any (like 'obj')
                        // and Erased Union (like Erased Union with string field)
                        true
                    | IntNumber, Fable.Number(_, Fable.NumberInfo.IsEnum _) when rules.HasFlag Allow.EnumIntoInt ->
                        // the underlying type of enum in F# is uint32
                        // For practicality: allow in all uint & int fields
                        true
                    | Fable.Option(t1,_), Fable.Option(t2,_)
                    | Fable.Option(t1,_), t2
                    | t1, t2 ->
                        typeEquals false t1 t2
                let fitsIntoMulti (rules: Allow) (expected: Fable.Type list) (actual: Fable.Type) =
                    expected |> List.contains Fable.Any
                    ||
                    (
                        // special treatment for actual=Any & multiple expected:
                        // multiple expected -> `Ux<...>` -> extracted types
                        // BUT: in actual that's not possible -> in actual `Ux<...>` = `Any`
                        //      -> no way to distinguish Ux (or other Erased Unions) from 'normal` Any (like obj)
                        rules.HasFlag Allow.AnyIntoErased
                        &&
                        expected |> List.isMultiple
                        &&
                        actual = Fable.Any
                    )
                    ||
                    expected |> List.exists (fun expected -> fitsIntoSingle rules expected actual)

                fitsIntoMulti rules expected actual

            let quote = sprintf "'%s'"
            let formatType = getTypeFullName true
            let formatTypes = List.map (formatType >> quote) >> String.concat "; "
            let unreachable () = failwith "unreachable"
            let formatMissingFieldError
                (fieldName: string)
                (expectedTypes: Fable.Type list)
                =
                assert(expectedTypes |> List.isEmpty |> not)

                let interfaceName = interface_.DisplayName

                // adjust error messages based on:
                // * 1, more expectedTypes
                let msg =
                    match expectedTypes with
                    | [] -> unreachable ()
                    | [expectedType] ->
                        let expectedType = expectedType |> formatType
                        $"Object doesn't contain field '{fieldName}' of type '{expectedType}' required by interface '{interfaceName}'"
                    | _ ->
                        let expectedTypes = expectedTypes |> formatTypes
                        $"Object doesn't contain field '{fieldName}' of any type [{expectedTypes}] required by interface '{interfaceName}'"

                (range, fieldName, msg)

            let formatUnexpectedTypeError
                (indexers: FSharpMemberOrFunctionOrValue list option)
                (fieldName: string)
                (expectedTypes: Fable.Type list)
                (actualType: Fable.Type)
                (r: SourceLocation option)
                =
                assert(expectedTypes |> List.isEmpty |> not)

                let interfaceName = interface_.DisplayName
                let actualType = actualType |> formatType

                // adjust error messages based on:
                // * 1, more expectedTypes
                // * 0 (None), 1, more indexer
                let msg =
                    match indexers with
                    | None ->
                        match expectedTypes with
                        | [] -> unreachable ()
                        | [expectedType] ->
                            let expectedType = expectedType |> formatType
                            $"Expected type '{expectedType}' for field '{fieldName}' in interface '{interfaceName}', but is '{actualType}'"
                        | _ ->
                            let expectedTypes = expectedTypes |> formatTypes
                            $"Expected any type of [{expectedTypes}] for field '{fieldName}' in interface '{interfaceName}', but is '{actualType}'"
                    | Some indexers ->
                        assert(indexers |> List.isEmpty |> not)

                        let indexers =
                            indexers
                            |> List.map (fun i -> i.DisplayName)
                            |> List.distinct

                        match indexers with
                        | [] -> unreachable ()
                        | [indexerName] ->
                            match expectedTypes with
                            | [] -> unreachable ()
                            | [expectedType] ->
                                let expectedType = expectedType |> formatType
                                $"Expected type '{expectedType}' for field '{fieldName}' because of Indexer '{indexerName}' in interface '{interfaceName}', but is '{actualType}'"
                            | _ ->
                                let expectedTypes = expectedTypes |> formatTypes
                                $"Expected any type of [{expectedTypes}] for field '{fieldName}' because of Indexer '{indexerName}' in interface '{interfaceName}', but is '{actualType}'"
                        | _ ->
                            let indexerNames =
                                indexers
                                |> List.map (quote)
                                |> String.concat "; "
                            match expectedTypes with
                            | [] -> unreachable ()
                            | [expectedType] ->
                                let expectedType = expectedType |> formatType
                                $"Expected type '{expectedType}' for field '{fieldName}' because of Indexers [{indexerNames}] in interface '{interfaceName}', but is '{actualType}'"
                            | _ ->
                                let expectedTypes = expectedTypes |> formatTypes
                                $"Expected any type of [{expectedTypes}] for field '{fieldName}' because of Indexers [{indexerNames}] in interface '{interfaceName}', but is '{actualType}'"

                let r = r |> Option.orElse range // fall back to anon record range

                (r, fieldName, msg)

            /// Returns: errors
            let fitsInterfaceMembers (fieldsToIgnore: Set<string>) =
                interfaceMembers
                |> List.filter (fun m -> not (m.Attributes |> hasAttribute Atts.emitIndexer))
                |> List.filter (fun m -> m.IsPropertyGetterMethod)
                |> List.choose (fun m ->
                    if fieldsToIgnore |> Set.contains m.DisplayName then
                        None
                    else
                        let expectedTypes = m.ReturnParameter.Type |> collectTypes
                        fieldNames
                        |> Array.tryFindIndex ((=) m.DisplayName)
                        |> function
                           | None ->
                                if expectedTypes |> List.forall (function | Fable.Option _ -> true | _ -> false) then
                                    None    // Optional fields can be missing
                                else
                                    formatMissingFieldError m.DisplayName expectedTypes
                                    |> Some
                           | Some i ->
                                let expr = List.item i argExprs
                                let ty = expr.Type
                                if ty |> fitsInto (Allow.TheUsual ||| Allow.AnyIntoErased) expectedTypes then
                                    None
                                else
                                    formatUnexpectedTypeError None m.DisplayName expectedTypes ty expr.Range
                                    |> Some
                )

            /// Returns errors
            let fitsInterfaceIndexers (fieldsToIgnore: Set<string>) =
                // Note: Indexers are assumed to be "valid" index properties (like `string` and/or `int` input (TS rules))
                let indexers =
                    interfaceMembers
                    |> List.filter (fun m -> m.Attributes |> hasAttribute Atts.emitIndexer)
                        // Indexer:
                        // * with explicit get: IsPropertyGetterMethod
                        // * with explicit set: IsPropertySetterMetod
                        // * without explicit get (readonly -> same as get): IsPropertyGetterMethod = false
                    |> List.filter (fun m -> not m.IsPropertySetterMethod)
                // far from perfect: Erased Types are `Fable.Any` instead of their actual type
                // (exception: `Ux<...>` (and `Option<Ux<...>>`) -> types get extracted)
                let validTypes =
                    indexers
                    |> List.collect (fun i -> collectTypes i.ReturnParameter.Type)
                    |> List.distinct

                match validTypes with
                | [] -> []  // no indexer
                | _ when validTypes |> List.contains Fable.Any -> []
                | _ ->
                    List.zip (fieldNames |> Array.toList) argExprs
                    |> List.filter (fun (fieldName, _) -> fieldsToIgnore |> Set.contains fieldName |> not )
                    |> List.choose (fun (name, expr) ->
                        let ty = expr.Type
                        if fitsInto (Allow.TheUsual ||| Allow.EnumIntoInt ||| Allow.AnyIntoErased) validTypes ty then
                            None
                        else
                            formatUnexpectedTypeError (Some indexers) name validTypes ty expr.Range
                            |> Some
                    )

//            let withoutErrored
//                (interfaceMembers: FSharpMemberOrFunctionOrValue list)
//                (errors: _ list)
//                =
//                let fieldsWithError = errors |> List.map (fun (_, fieldName, _) -> fieldName) |> Set.ofList
//                interfaceMembers
//                |> List.filter (fun m -> fieldsWithError |> Set.contains (m.DisplayName) |> not)

            // TODO: Check also if there are extra fields in the record not present in the interface?
            let fieldErrors = fitsInterfaceMembers (Set.empty)
            let indexerErrors =
                fitsInterfaceIndexers
                    // don't check already errored fields
                    (fieldErrors |> List.map (fun (_, fieldName, _) -> fieldName) |> Set.ofList)

            List.append fieldErrors indexerErrors
            |> List.map (fun (r,_,m) -> (r,m))
               // sort errors by their appearance in code
            |> List.sortBy fst
            |> function
               | [] -> Ok ()
               | errors -> Error errors
        | _ ->
            Ok () // TODO: Error instead if we cannot check the interface?

module Identifiers =
    open Helpers
    open TypeHelpers

    let makeIdentFrom (com: IFableCompiler) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue): Fable.Ident =
        let name, part = (fsRef.CompiledName, Naming.NoMemberPart)
        let sanitizedName =
            match com.Options.Language with
            | Python ->
                let name = Fable.PY.Naming.toSnakeCase name
                Fable.PY.Naming.sanitizeIdent (fun name -> isUsedName ctx name || Fable.PY.Naming.pyBuiltins.Contains name) name part
            | Rust -> Naming.sanitizeIdent (isUsedName ctx) (name |> cleanNameAsRustIdentifier) part
            | _ -> Naming.sanitizeIdent (isUsedName ctx) name part
        let isMutable =
            match com.Options.Language with
            | Rust -> fsRef.IsMutable || isByRefValue fsRef
            | _ -> fsRef.IsMutable

        ctx.UsedNamesInDeclarationScope.Add(sanitizedName) |> ignore

        { Name = sanitizedName
          Type = makeType ctx.GenericArgs fsRef.FullType
          IsThisArgument = fsRef.IsMemberThisValue
          IsCompilerGenerated = fsRef.IsCompilerGenerated
          IsMutable = isMutable
          Range = { makeRange fsRef.DeclarationLocation
                    with identifierName = Some fsRef.DisplayName } |> Some }

    let putIdentInScope com ctx (fsRef: FSharpMemberOrFunctionOrValue) value: Context*Fable.Ident =
        let ident = makeIdentFrom com ctx fsRef
        { ctx with Scope = (Some fsRef, ident, value)::ctx.Scope }, ident

    let (|PutIdentInScope|) com ctx fsRef = putIdentInScope com ctx fsRef None

    let identWithRange r (ident: Fable.Ident) =
        let originalName = ident.Range |> Option.bind (fun r -> r.identifierName)
        { ident with Range = r |> Option.map (fun r -> { r with identifierName = originalName }) }

    let tryGetIdentFromScopeIf (ctx: Context) r predicate =
        ctx.Scope |> List.tryPick (fun (fsRef, ident, _) ->
            fsRef
            |> Option.filter predicate
            |> Option.map (fun _ -> identWithRange r ident |> Fable.IdentExpr))

    /// Get corresponding identifier to F# value in current scope
    let tryGetIdentFromScope (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        tryGetIdentFromScopeIf ctx r (fun fsRef' -> obj.Equals(fsRef, fsRef'))

module Util =
    open Helpers
    open Patterns
    open TypeHelpers
    open Identifiers

    let makeFunctionArgs com ctx (args: FSharpMemberOrFunctionOrValue list) =
        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = putIdentInScope com ctx var None
                newContext, arg::accArgs)
        ctx, List.rev args

    let bindMemberArgs com ctx (memb: FSharpMemberOrFunctionOrValue option) (args: FSharpMemberOrFunctionOrValue list list) =
        // The F# compiler "untuples" the args in methods
        let args = List.concat args
        let paramObjIndex, args =
            match memb with
            | Some m ->
                let paramObjIndex = FsMemberFunctionOrValue.TryParamObjectIndex(m)
                let params_ = m.CurriedParameterGroups |> Seq.concat |> Seq.toList
                let args =
                    if List.sameLength args params_ then
                        params_ |> List.map Some |> List.zip args
                    else args |> List.map (fun a -> a, None)
                paramObjIndex, args
            | None -> None, args |> List.map (fun a -> a, None)

        let ctx, thisArg, args =
            match args with
            | (firstArg,_)::restArgs when firstArg.IsMemberThisValue ->
                let ctx, thisArg = putIdentInScope com ctx firstArg None
                let thisArg = { thisArg with IsThisArgument = true }
                let ctx = { ctx with BoundMemberThis = Some thisArg }
                ctx, [Fable.ArgDecl.Create thisArg], restArgs
            | (firstArg,_)::restArgs when firstArg.IsConstructorThisValue ->
                let ctx, thisArg = putIdentInScope com ctx firstArg None
                let thisArg = { thisArg with IsThisArgument = true }
                let ctx = { ctx with BoundConstructorThis = Some thisArg }
                ctx, [Fable.ArgDecl.Create thisArg], restArgs
            | _ -> ctx, [], args

        let mutable i = -1
        let ctx, args =
            ((ctx, []), args) ||> List.fold (fun (ctx, accArgs) (arg, param) ->
                i <- i + 1
                let isNamed =
                    match paramObjIndex with
                    | Some paramObjIndex -> i >= paramObjIndex
                    | None -> false
                let isOptional =
                    match param with
                    | Some p -> p.IsOptionalArg
                    | None -> false
                let ctx, argIdent = putIdentInScope com ctx arg None
                let arg = Fable.ArgDecl.Create(argIdent, isOptional=isOptional, isNamed=isNamed)
                ctx, arg::accArgs)

        ctx, thisArg @ (List.rev args)

    let makeTryCatch com ctx r (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (PutIdentInScope com ctx (catchContext, catchVar), catchBody) ->
                // Add caughtException to context so it can be retrieved by `reraise`
                let catchContext = { catchContext with CaughtException = Some catchVar }
                Some (catchVar, com.Transform(catchContext, catchBody))
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch(body, catchClause, finalizer, r)

    let matchGenericParamsFrom (memb: FSharpMemberOrFunctionOrValue) (genArgs: Fable.Type list) =
        match memb.DeclaringEntity with
        // It seems that for F# types memb.GenericParameters contains all generics
        // but for BCL types we need to check the DeclaringEntity generics too
        | Some ent when genArgs.Length > memb.GenericParameters.Count ->
            Seq.append ent.GenericParameters memb.GenericParameters
        | _ -> upcast memb.GenericParameters
        |> Seq.map genParamName
        |> fun genParams -> Seq.zip genParams genArgs

    /// Takes only the first CurriedParameterGroup into account.
    /// If there's only a single unit parameter, returns 0.
    let countNonCurriedParams (meth: FSharpMemberOrFunctionOrValue) =
        let args = meth.CurriedParameterGroups
        if args.Count = 0 then 0
        elif args.[0].Count = 1 then
            if isUnit args.[0].[0].Type then 0 else 1
        else args.[0].Count

    /// Same as `countNonCurriedParams` but applied to abstract signatures
    let countNonCurriedParamsForSignature (sign: FSharpAbstractSignature) =
        let args = sign.AbstractArguments
        if args.Count = 0 then 0
        elif args.[0].Count = 1 then
            if isUnit args.[0].[0].Type then 0 else 1
        else args.[0].Count

    // When importing a relative path from a different path where the member,
    // entity... is declared, we need to resolve the path
    let fixImportedRelativePath (com: Compiler) (path: string) sourcePath =
        let file = Path.normalizePathAndEnsureFsExtension sourcePath
        if file = com.CurrentFile then path
        else
            Path.Combine(Path.GetDirectoryName(file), path)
            |> Path.getRelativePath com.CurrentFile

    let (|GlobalAtt|ImportAtt|NoGlobalNorImport|) (atts: Fable.Attribute seq) =
        let (|AttFullName|) (att: Fable.Attribute) = att.Entity.FullName, att

        atts |> Seq.tryPick (function
            | AttFullName(Atts.global_, att) ->
                match att.ConstructorArgs with
                | [:? string as customName] -> GlobalAtt(Some customName) |> Some
                | _ -> GlobalAtt(None) |> Some

            | AttFullName(Naming.StartsWith Atts.import _ as fullName, att) ->
                match fullName, att.ConstructorArgs with
                | Atts.importAll, [(:? string as path)] ->
                    ImportAtt("*", path.Trim()) |> Some
                | Atts.importDefault, [(:? string as path)] ->
                    ImportAtt("default", path.Trim()) |> Some
                | Atts.importMember, [(:? string as path)] ->
                    ImportAtt(Naming.placeholder, path.Trim()) |> Some
                | _, [(:? string as selector); (:? string as path)] ->
                    ImportAtt(selector.Trim(), path.Trim()) |> Some
                | _ -> None

            | _ -> None)
        |> Option.defaultValue NoGlobalNorImport

    /// Function used to check if calls must be replaced by global idents or direct imports
    let tryGlobalOrImportedMember (com: Compiler) typ (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes
        |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
        |> function
        | GlobalAtt(Some customName) ->
            makeTypedIdent typ customName |> Fable.IdentExpr |> Some
        | GlobalAtt None ->
            getMemberDisplayName memb |> makeTypedIdent typ |> Fable.IdentExpr |> Some
        | ImportAtt(selector, path) ->
            let selector =
                if selector = Naming.placeholder then getMemberDisplayName memb
                else selector
            let path =
                match Path.isRelativePath path, memb.DeclaringEntity with
                | true, Some e ->
                    FsEnt.Ref(e).SourcePath
                    |> Option.map (fixImportedRelativePath com path)
                    |> Option.defaultValue path
                | _ -> path
            makeImportUserGenerated None typ selector path |> Some
        | _ -> None

    let tryGlobalOrImportedEntity (com: Compiler) (ent: Fable.Entity) =
        match ent.Attributes with
        | GlobalAtt(Some customName) ->
            makeTypedIdent Fable.Any customName |> Fable.IdentExpr |> Some
        | GlobalAtt None ->
            ent.DisplayName |> makeTypedIdent Fable.Any |> Fable.IdentExpr |> Some
        | ImportAtt(selector, path) ->
            let selector =
                if selector = Naming.placeholder then ent.DisplayName
                else selector
            let path =
                if Path.isRelativePath path then
                    ent.Ref.SourcePath
                    |> Option.map (fixImportedRelativePath com path)
                    |> Option.defaultValue path
                else path
            makeImportUserGenerated None Fable.Any selector path |> Some
        | _ -> None

    let isErasedOrStringEnumEntity (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att ->
            match att.Entity.FullName with
            | Atts.erase | Atts.stringEnum | Atts.tsTaggedUnion -> true
            | _ -> false)

    let isErasedOrStringEnumFSharpEntity (ent: FSharpEntity) =
        ent.Attributes |> Seq.exists (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some(Atts.erase | Atts.stringEnum | Atts.tsTaggedUnion) -> true
            | _ -> false)

    let isGlobalOrImportedEntity (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att ->
            match att.Entity.FullName with
            | Atts.global_ | Naming.StartsWith Atts.import _ -> true
            | _ -> false)

    let isGlobalOrImportedFSharpEntity (ent: FSharpEntity) =
        ent.Attributes |> Seq.exists (fun att ->
            match (nonAbbreviatedDefinition att.AttributeType).TryFullName with
            | Some(Atts.global_ | Naming.StartsWith Atts.import _) -> true
            | _ -> false)

    let isAttachMembersEntity (com: Compiler) (ent: FSharpEntity) =
        not ent.IsFSharpModule && (
            // com.Options.Language = Php ||
            com.Options.Language = Rust || // attach all members for Rust
            ent.Attributes |> Seq.exists (fun att ->
                // Should we make sure the attribute is not an alias?
                match att.AttributeType.TryFullName with
                | Some Atts.attachMembers -> true
                | _ -> false)
        )

    let isEmittedOrImportedMember (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes |> Seq.exists (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Naming.StartsWith Atts.emit _ | Atts.global_ | Naming.StartsWith Atts.import _) -> true
            | _ -> false)

    let private isFromDllNotPrecompiled (ent: Fable.Entity) =
        match ent.Ref.Path with
        | Fable.AssemblyPath _ | Fable.CoreAssemblyName _ -> true
        | Fable.SourcePath _
        | Fable.PrecompiledLib _ -> false

    let private isReplacementCandidatePrivate isFromDll (entFullName: string) =
        if entFullName.StartsWith("System.") || entFullName.StartsWith("Microsoft.FSharp.") then isFromDll
        // When compiling Fable itself, Fable.Core entities will be part of the code base,
        // but still need to be replaced
        else entFullName.StartsWith("Fable.Core.")

    let isReplacementCandidate (ent: Fable.Entity) =
        isReplacementCandidatePrivate (isFromDllNotPrecompiled ent) ent.FullName

    let isReplacementCandidateFrom (ent: FSharpEntity) =
        let isFromDllRef = Option.isSome ent.Assembly.FileName
        isReplacementCandidatePrivate isFromDllRef (FsEnt.FullName ent)

    let getEntityType (ent: Fable.Entity): Fable.Type =
        let genArgs = ent.GenericParameters |> List.map (fun x ->
            Fable.Type.GenericParam(x.Name, Seq.toList x.Constraints))
        Fable.Type.DeclaredType(ent.Ref, genArgs)

    /// We can add a suffix to the entity name for special methods, like reflection declaration
    let entityRefWithSuffix (com: Compiler) (ent: Fable.Entity) suffix =
        let error msg =
            $"%s{msg}: %s{ent.FullName}"
            |> addErrorAndReturnNull com [] None
        match com.Options.Language, ent.IsInterface, ent.Ref.SourcePath with
        | JavaScript, true, _ -> error "Cannot reference an interface in JS"
        | _, _, None -> error "Cannot reference entity from .dll reference, Fable packages must include F# sources"
        | _, _, Some file ->
            let entityName = (getEntityDeclarationName com ent.Ref) + suffix
            // If precompiling inline function always reference with Import and not as IdentExpr
            if not com.IsPrecompilingInlineFunction && file = com.CurrentFile then
                makeIdentExpr entityName
            else
                makeInternalClassImport com entityName file

    let entityRef (com: Compiler) (ent: Fable.Entity) =
        entityRefWithSuffix com ent ""

    /// First checks if the entity is global or imported
    let tryEntityRefMaybeGlobalOrImported (com: Compiler) (ent: Fable.Entity) =
        match tryGlobalOrImportedEntity com ent with
        | Some _importedEntity as entOpt -> entOpt
        | None ->
            if isFromDllNotPrecompiled ent
            then None
            else Some (entityRef com ent)

    let memberRef (com: Compiler) r typ (memb: FSharpMemberOrFunctionOrValue) =
        let r = r |> Option.map (fun r -> { r with identifierName = Some memb.DisplayName })
        let memberName, hasOverloadSuffix = getMemberDeclarationName com memb
        let memberName =
            match com.Options.Language, memb.DeclaringEntity with
            // for Rust use full name with non-instance calls
            | Rust, Some ent when not(memb.IsInstanceMember) ->
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
            { makeTypedIdent typ memberName with Range = r; IsMutable = memb.IsMutable }
            |> Fable.IdentExpr
        else
            // If the overload suffix changes, we need to recompile the files that call this member
            if hasOverloadSuffix then com.AddWatchDependency(file)
            makeInternalMemberImport com typ memb.IsInstanceMember memberName file

    let rec tryFindInTypeHierarchy (ent: FSharpEntity) filter =
        if filter ent then Some ent
        else
            match getBaseEntity ent with
            | Some(ent, _) ->
                tryFindInTypeHierarchy ent filter
            | _ -> None

    /// Checks who's the actual implementor of the interface, this entity or any of its parents
    let rec tryFindImplementingEntity (ent: FSharpEntity) interfaceFullName =
        tryFindInTypeHierarchy ent (fun ent ->
            ent.DeclaredInterfaces
            |> Seq.exists (testInterfaceHierarchy interfaceFullName))

    let rec inherits (ent: FSharpEntity) baseFullName =
        tryFindInTypeHierarchy ent (fun ent ->
            ent.TryFullName = Some baseFullName)
        |> Option.isSome

    let isMangledAbstractEntity com (ent: FSharpEntity) =
        match ent.TryFullName with
        // By default mangle interfaces in System namespace as they are not meant to interact with JS
        // except those that are used in fable-library Typescript files
        | Some fullName when fullName.StartsWith("System.") ->
            match fullName with
            | Types.object
            | Types.idisposable
            | Types.icomparable
            | "System.IObservable`1"
            | "System.IObserver`1"
            | Types.ienumerableGeneric
            // These are used for injections
            | Types.comparer
            | Types.equalityComparer -> false
            | _ -> true
        // Don't mangle abstract classes in Fable.Core.JS and Fable.Core.PY namespaces
        | Some fullName when fullName.StartsWith("Fable.Core.JS.") -> false
        | Some fullName when fullName.StartsWith("Fable.Core.PY.") -> false
        // Don't mangle interfaces by default (for better JS interop) unless they have Mangle attribute
        | _ when ent.IsInterface -> hasAttribute Atts.mangle ent.Attributes
        // Mangle members from abstract classes unless they are global/imported or with explicitly attached members
        | _ -> not(isGlobalOrImportedEntity(FsEnt ent) || isAttachMembersEntity com ent)

    let getMangledAbstractMemberName (ent: FSharpEntity) memberName overloadHash =
        // TODO: Error if entity doesn't have fullname?
        let entityName = defaultArg ent.TryFullName ""
        entityName + "." + memberName + overloadHash

    let callAttachedMember com r typ (callInfo: Fable.CallInfo) (entity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        let callInfo, callee =
            match callInfo.ThisArg with
            | Some callee -> { callInfo with ThisArg = None }, callee
            | None ->
                $"Unexpected static interface/override call: %s{memb.FullName}"
                |> attachRange r |> failwith
        let isGetter = memb.IsPropertyGetterMethod
        let isSetter = not isGetter && memb.IsPropertySetterMethod
        let indexedProp = (isGetter && countNonCurriedParams memb > 0) || (isSetter && countNonCurriedParams memb > 1)
        let name, isGetter, isSetter =
            if isMangledAbstractEntity com entity then
                let overloadHash =
                    if (isGetter || isSetter) && not indexedProp then ""
                    else FsMemberFunctionOrValue memb |> OverloadSuffix.getHash (FsEnt entity)
                getMangledAbstractMemberName entity memb.CompiledName overloadHash, false, false
            else
                // Indexed properties keep the get_/set_ prefix and are compiled as methods
                if indexedProp then memb.CompiledName, false, false
                else getMemberDisplayName memb, isGetter, isSetter
        if isGetter then
            let t = memb.ReturnParameter.Type |> makeType Map.empty
            // Set the field as mutable to prevent beta reduction
            Fable.Get(callee, Fable.FieldGet(name, true), t, r)
        elif isSetter then
            let membType = memb.CurriedParameterGroups.[0].[0].Type |> makeType Map.empty
            let arg = callInfo.Args |> List.tryHead |> Option.defaultWith makeNull
            Fable.Set(callee, Fable.FieldSet(name), membType, arg, r)
        else
            getAttachedMember callee name |> makeCall r typ callInfo

    let failReplace (com: IFableCompiler) ctx r (info: Fable.ReplaceCallInfo) =
        let msg =
            if info.DeclaringEntityFullName.StartsWith("Fable.Core.") then
                $"{info.DeclaringEntityFullName}.{info.CompiledName} is not supported, try updating fable tool"
            else
                com.WarnOnlyOnce("Fable only supports a subset of standard .NET API, please check https://fable.io/docs/dotnet/compatibility.html. For external libraries, check whether they are Fable-compatible in the package docs.")
                $"{info.DeclaringEntityFullName}.{info.CompiledName} is not supported by Fable"
        msg |> addErrorAndReturnNull com ctx.InlinePath r

    let (|Replaced|_|) (com: IFableCompiler) (ctx: Context) r typ genArgs (callInfo: Fable.CallInfo)
            (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        match entity with
        | Some ent when isReplacementCandidateFrom ent ->
            let info: Fable.ReplaceCallInfo =
              { SignatureArgTypes = callInfo.SignatureArgTypes
                DeclaringEntityFullName = ent.FullName
                HasSpread = callInfo.HasSpread
                IsModuleValue = isModuleValueForCalls ent memb
                IsInterface = ent.IsInterface
                CompiledName = memb.CompiledName
                OverloadSuffix =
                    if ent.IsFSharpModule then ""
                    else FsMemberFunctionOrValue memb |> OverloadSuffix.getHash (FsEnt ent)
                GenericArgs = genArgs }
            match ctx.PrecompilingInlineFunction with
            | Some _ ->
                // Deal with reraise so we don't need to save caught exception every time
                match ctx.CaughtException, info.DeclaringEntityFullName, info.CompiledName with
                | Some ex, "Microsoft.FSharp.Core.Operators", "Reraise" ->
                    makeThrow r typ (Fable.IdentExpr ex) |> Some
                | _ ->
                    // If it's an interface compile the call to the attached member just in case
                    let attachedCall =
                        if info.IsInterface then callAttachedMember com r typ callInfo ent memb |> Some
                        else None
                    let e = Fable.UnresolvedReplaceCall(callInfo.ThisArg, callInfo.Args, info, attachedCall)
                    Fable.Unresolved(e, typ, r) |> Some
            | None ->
                match com.TryReplace(ctx, r, typ, info, callInfo.ThisArg, callInfo.Args) with
                | Some e -> Some e
                | None when info.IsInterface -> callAttachedMember com r typ callInfo ent memb |> Some
                | None -> failReplace com ctx r info |> Some
        | _ -> None

    let addWatchDependencyFromMember (com: Compiler) (memb: FSharpMemberOrFunctionOrValue) =
        memb.DeclaringEntity
        |> Option.bind (fun ent -> FsEnt.Ref(ent).SourcePath)
        |> Option.iter com.AddWatchDependency

    let (|Emitted|_|) com r typ (callInfo: Fable.CallInfo option) (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Naming.StartsWith Atts.emit _ as attFullName) ->
                addWatchDependencyFromMember com memb
                let callInfo =
                    match callInfo with
                    | Some i -> i
                    | None -> Fable.CallInfo.Make()
                // Allow combination of Import and Emit attributes
                let callInfo =
                    match tryGlobalOrImportedMember com Fable.Any memb with
                    | Some importExpr -> { callInfo with Fable.ThisArg = Some importExpr }
                    | _ -> callInfo
                let isStatement = tryAttributeConsArg att 1 false tryBoolean
                let macro = tryAttributeConsArg att 0  "" tryString
                let macro =
                    match attFullName with
                    | Atts.emitMethod -> "$0." + macro + "($1...)"
                    | Atts.emitConstructor -> "new $0($1...)"
                    | Atts.emitIndexer -> "$0[$1]{{=$2}}"
                    | Atts.emitProperty -> "$0." + macro + "{{=$1}}"
                    | _ -> macro
                let emitInfo: Fable.EmitInfo =
                    { Macro = macro
                      IsStatement = isStatement
                      CallInfo = callInfo }
                Fable.Emit(emitInfo, typ, r) |> Some
            | _ -> None)

    let (|Imported|_|) com r typ callInfo (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let importValueType = if Option.isSome callInfo then Fable.Any else typ
        match tryGlobalOrImportedMember com importValueType memb, callInfo, entity with
        // Import called as function
        | Some importExpr, Some callInfo, Some e ->
            let isValueOrGetter =
                isModuleValueForCalls e memb
                || (memb.IsPropertyGetterMethod && (countNonCurriedParams memb) = 0)

            if isValueOrGetter then Some importExpr
            else makeCall r typ callInfo importExpr |> Some

        // Import called as value
        | Some importExpr, None, _ -> Some importExpr

        // The value/method is not imported, check if the declaring entity is
        | None, Some callInfo, Some e ->
            let moduleOrClassExpr =
                let fableEnt = FsEnt e
                match tryGlobalOrImportedEntity com fableEnt with
                | Some expr -> Some expr
                // AttachMembers classes behave the same as global/imported classes
                | None when com.Options.Language <> Rust && isAttachMembersEntity com e ->
                    Some(entityRef com fableEnt)
                | None -> None
            match moduleOrClassExpr, callInfo.ThisArg with
            | Some _, Some _thisArg ->
                callAttachedMember com r typ callInfo e memb |> Some

            | Some classExpr, None when memb.IsConstructor ->
                Fable.Call(classExpr, { callInfo with IsConstructor = true }, typ, r) |> Some

            | Some moduleOrClassExpr, None ->
                if isModuleValueForCalls e memb then
                    // Set the field as mutable just in case, so it's not displaced by beta reduction
                    Fable.Get(moduleOrClassExpr, Fable.FieldGet(getMemberDisplayName memb, true), typ, r) |> Some
                else
                    let callInfo = { callInfo with ThisArg = Some moduleOrClassExpr }
                    callAttachedMember com r typ callInfo e memb |> Some

            | None, _ -> None
        | _ -> None
        |> Option.tap (fun _ -> addWatchDependencyFromMember com memb)

    let inlineExpr (com: IFableCompiler) (ctx: Context) r t (genArgs: Fable.Type list) callee (info: Fable.CallInfo) membUniqueName =
        let args: Fable.Expr list =
            match callee with
            | Some c -> c::info.Args
            | None -> info.Args

        let inExpr = com.GetInlineExpr(membUniqueName)
        com.AddWatchDependency(inExpr.FileName)

        let fromFile, fromRange =
            match ctx.InlinePath with
            | { ToFile = file; ToRange = r }::_ -> file, r
            | [] -> com.CurrentFile, r

        let genArgs = List.zipSafe inExpr.GenericArgs genArgs |> Map

        let ctx = { ctx with GenericArgs = genArgs
                             InlinePath = { ToFile = inExpr.FileName
                                            ToRange = inExpr.Body.Range
                                            FromFile = fromFile
                                            FromRange = fromRange }::ctx.InlinePath }

        let bindings, expr = com.ResolveInlineExpr(ctx, inExpr, args)

        match expr with
        // If this is an import expression, apply the arguments, see #2280
        | Fable.Import(importInfo, ti, r) as importExpr when not importInfo.IsCompilerGenerated ->
            // Check if import has absorbed the arguments, see #2284
            let args =
                let path = importInfo.Path
                match importInfo.Selector, info.Args with
                | sel, (StringConst selArg)::(StringConst pathArg)::args when sel = selArg && path = pathArg -> args
                | ("default"|"*"), (StringConst pathArg)::args when path = pathArg -> args
                | _, args -> args
            // Don't apply args either if this is a class getter, see #2329
            if List.isEmpty args || info.CallMemberInfo |> Option.map (fun i -> i.IsGetter) |> Option.defaultValue false then
                // Set UserImport(inline=true) to prevent Fable removing args of surrounding function
                Fable.Import({ importInfo with Kind = Fable.UserImport true }, ti, r)
            else
                makeCall r t info importExpr
        | body ->
            // Check the resolved expression has the expected type, see #2644
            let body = if t <> body.Type then Fable.TypeCast(body, t) else body
            List.fold (fun body (ident, value) -> Fable.Let(ident, value, body)) body bindings

    let (|Inlined|_|) (com: IFableCompiler) (ctx: Context) r t genArgs callee info (memb: FSharpMemberOrFunctionOrValue) =
        if isInline memb then
            let membUniqueName = getMemberUniqueName memb
            match ctx.PrecompilingInlineFunction with
            | Some memb2 when memb.Equals(memb2) ->
                $"Recursive functions cannot be inlined: (%s{memb.FullName})"
                |> addErrorAndReturnNull com [] r |> Some
            | Some _ ->
                let e = Fable.UnresolvedInlineCall(membUniqueName, genArgs,  ctx.Witnesses, callee, info)
                Fable.Unresolved(e, t, r) |> Some
            | None ->
                inlineExpr com ctx r t genArgs callee info membUniqueName |> Some
        else None

    /// Removes optional arguments set to None in tail position
    let transformOptionalArguments (_com: IFableCompiler) (_ctx: Context) (_r: SourceLocation option)
                (memb: FSharpMemberOrFunctionOrValue) (args: Fable.Expr list) =
        if memb.CurriedParameterGroups.Count <> 1
            || memb.CurriedParameterGroups.[0].Count <> (List.length args)
        then args
        else
            (memb.CurriedParameterGroups.[0], args, (true, []))
            |||> Seq.foldBack2 (fun par arg (keepChecking, acc) ->
                if keepChecking && par.IsOptionalArg then
                    match arg with
                    | Fable.Value(Fable.NewOption(None,_,_),_) -> true, acc
                    | _ -> false, arg::acc
                else false, arg::acc)
            |> snd

    let hasInterface interfaceFullname (ent: Fable.Entity) =
        ent.AllInterfaces |> Seq.exists (fun ifc -> ifc.Entity.FullName = interfaceFullname)

    let makeCallWithArgInfo com (ctx: Context) r typ genArgs callee (memb: FSharpMemberOrFunctionOrValue) (callInfo: Fable.CallInfo) =
        match memb, memb.DeclaringEntity with
        | Emitted com r typ (Some callInfo) emitted, _ -> emitted
        | Imported com r typ (Some callInfo) imported -> imported
        | Replaced com ctx r typ genArgs callInfo replaced -> replaced
        | Inlined com ctx r typ genArgs callee callInfo expr, _ -> expr

        | Try (tryGetIdentFromScope ctx r) funcExpr, Some entity ->
            if isModuleValueForCalls entity memb then funcExpr
            else makeCall r typ callInfo funcExpr

        | _, Some entity when entity.IsDelegate ->
            match callInfo.ThisArg, memb.DisplayName with
            | Some callee, "Invoke" ->
                let callInfo = { callInfo with ThisArg = None }
                makeCall r typ callInfo callee
            | _ -> "Only Invoke is supported in delegates"
                   |> addErrorAndReturnNull com ctx.InlinePath r

        // Check if this is an interface or abstract/overriden method
        | _, Some entity when entity.IsInterface
                || memb.IsOverrideOrExplicitInterfaceImplementation
                || memb.IsDispatchSlot ->

            // When calling `super` in an override, it may happen the method is not originally declared
            // by the immediate parent, so we need to go through the hierarchy until we find the original declaration
            // (this is important to get the correct mangled name)
            let entity =
                match memb.IsOverrideOrExplicitInterfaceImplementation, callInfo.ThisArg with
                | true, Some(Fable.Value(Fable.BaseValue _, _)) ->
                    // Only compare args for overloads (single curried parameter group)
                    let compareArgs =
                        if memb.CurriedParameterGroups.Count <> 1 then None
                        else memb.CurriedParameterGroups.[0] |> Seq.toArray |> Some
                    entity |> tryFindBaseEntity (fun ent ->
                        ent.TryGetMembersFunctionsAndValues() |> Seq.exists (fun m ->
                            m.IsInstanceMember
                            && m.CompiledName = memb.CompiledName
                            && m.IsDispatchSlot
                            && (
                                match compareArgs with
                                | Some compareArgs when m.CurriedParameterGroups.Count = 1 && m.CurriedParameterGroups.[0].Count = compareArgs.Length ->
                                    let compareArgs2 = m.CurriedParameterGroups.[0] |> Seq.toArray
                                    Array.zip compareArgs compareArgs2
                                    |> Array.forall (fun (p1, p2) -> p1.Type.Equals(p2.Type))
                                | _ -> true
                            )
                        ))
                    |> Option.defaultValue entity
                | _ -> entity

            callAttachedMember com r typ callInfo entity memb

        | _, Some entity when isModuleValueForCalls entity memb && com.Options.Language <> Rust ->
            let typ = makeType ctx.GenericArgs memb.FullType
            memberRef com r typ memb
        | _ ->
            // If member looks like a value but behaves like a function (has generic args) the type from F# AST is wrong (#2045).
            let typ = makeType ctx.GenericArgs memb.ReturnParameter.Type
            let callExpr =
                memberRef com r Fable.Any memb
                |> makeCall r typ callInfo
            let fableMember = FsMemberFunctionOrValue(memb)
            com.ApplyMemberCallPlugin(fableMember, callExpr)

    let makeCallInfoMemb callee args sigArgTypes (memb: FSharpMemberOrFunctionOrValue): Fable.CallInfo =
        {
            ThisArg = callee
            Args = args
            SignatureArgTypes = sigArgTypes
            HasSpread = hasParamArray memb
            IsConstructor = false
            CallMemberInfo = Some(FsMemberFunctionOrValue.CallMemberInfo(memb))
            OptimizableInto = None
        }

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ (genArgs: Fable.Type list) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let newCtxGenArgs = matchGenericParamsFrom memb genArgs |> Seq.toList
        let ctx = { ctx with GenericArgs = (ctx.GenericArgs, newCtxGenArgs) ||> Seq.fold (fun map (k, v) -> Map.add k v map) }
        Fable.CallInfo.Make(
            ?thisArg = callee,
            args = transformOptionalArguments com ctx r memb args,
            sigArgTypes = getArgTypes com memb,
            hasSpread = hasParamArray memb,
            isCons = memb.IsConstructor,
            memberInfo = FsMemberFunctionOrValue.CallMemberInfo(memb))
        |> makeCallWithArgInfo com ctx r typ genArgs callee memb

    let makeValueFrom (com: IFableCompiler) (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType ctx.GenericArgs v.FullType
        match v, v.DeclaringEntity with
        | _ when typ = Fable.Unit ->
            if com.Options.Verbosity = Verbosity.Verbose && not v.IsCompilerGenerated then // See #1516
                $"Value %s{v.DisplayName} is replaced with unit constant"
                |> addWarning com ctx.InlinePath r
            Fable.Value(Fable.UnitConstant, r)
        | Emitted com r typ None emitted, _ -> emitted
        | Imported com r typ None imported -> imported
        | Try (tryGetIdentFromScope ctx r) expr, _ -> expr
        | _ -> memberRef com r typ v
