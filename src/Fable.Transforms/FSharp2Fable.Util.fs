namespace rec Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Core
open Fable.AST
open Fable.Transforms

type FsField(name, typ: Lazy<Fable.Type>, ?isMutable, ?isStatic, ?literalValue) =
    new (fi: FSharpField) =
        let getFSharpFieldName (fi: FSharpField) =
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

        let typ = lazy TypeHelpers.makeType Map.empty fi.FieldType
        FsField(getFSharpFieldName fi, typ, isMutable=fi.IsMutable, isStatic=fi.IsStatic, ?literalValue=fi.LiteralValue)
    interface Fable.Field with
        member _.Name = name
        member _.FieldType = typ.Value
        member _.LiteralValue = literalValue
        member _.IsStatic = defaultArg isStatic false
        member _.IsMutable = defaultArg isMutable false

type FsUnionCase(uci: FSharpUnionCase) =
    /// FSharpUnionCase.CompiledName doesn't give the value of CompiledNameAttribute
    /// We must check the attributes explicitly
    static member CompiledName (uci: FSharpUnionCase) =
        uci.Attributes
        |> Helpers.tryFindAtt Atts.compiledName
        |> Option.map (fun (att: FSharpAttribute) -> att.ConstructorArguments.[0] |> snd |> string)

    interface Fable.UnionCase with
        member _.Name = uci.Name
        member _.CompiledName = FsUnionCase.CompiledName uci
        member _.UnionCaseFields = uci.UnionCaseFields |> Seq.mapToList (fun x -> upcast FsField(x))

type FsAtt(att: FSharpAttribute) =
    interface Fable.Attribute with
        member _.Entity = FsEnt.Ref att.AttributeType
        member _.ConstructorArgs = att.ConstructorArguments |> Seq.mapToList snd

type FsGenParam(gen: FSharpGenericParameter) =
    interface Fable.GenericParam with
        member _.Name = TypeHelpers.genParamName gen

type FsParam(p: FSharpParameter) =
    interface Fable.Parameter with
        member _.Name = p.Name
        member _.Type = TypeHelpers.makeType Map.empty p.Type

type FsDeclaredType(ent: FSharpEntity, genArgs: IList<FSharpType>) =
    interface Fable.DeclaredType with
        member _.Entity = FsEnt.Ref ent
        member _.GenericArgs = genArgs |> Seq.mapToList (TypeHelpers.makeType Map.empty)

type FsMemberFunctionOrValue(m: FSharpMemberOrFunctionOrValue) =
    interface Fable.MemberFunctionOrValue with
        member _.Attributes =
            m.Attributes |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)

        // These two properties are only used for member declarations,
        // setting them to false for now
        member _.IsMangled = false
        member _.IsEnumerator = false

        member _.HasSpread = Helpers.hasParamArray m
        member _.IsPublic = Helpers.isPublicMember m
        // NOTE: Using memb.IsValue doesn't work for function values
        // See isModuleValueForDeclarations below
        member _.IsValue = m.IsValue
        member _.IsInstance = m.IsInstanceMember
        member _.IsMutable = m.IsMutable
        member _.IsGetter = m.IsPropertyGetterMethod
        member _.IsSetter = m.IsPropertySetterMethod

        member _.DisplayName = Naming.removeGetSetPrefix m.DisplayName
        member _.CompiledName = m.CompiledName
        member _.FullName = m.FullName
        member _.CurriedParameterGroups =
            m.CurriedParameterGroups
            |> Seq.mapToList (Seq.mapToList (fun p -> upcast FsParam(p)))
        member _.ReturnParameter = upcast FsParam(m.ReturnParameter)
        member _.IsExplicitInterfaceImplementation = m.IsExplicitInterfaceImplementation
        member _.ApparentEnclosingEntity = FsEnt.Ref m.ApparentEnclosingEntity

type FsEnt(ent: FSharpEntity) =
    static let rec nonAbbreviatedDefinition (ent: FSharpEntity) =
        if ent.IsFSharpAbbreviation then
            let t = ent.AbbreviatedType
            if t.HasTypeDefinition then nonAbbreviatedDefinition t.TypeDefinition
            else ent
        else ent

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

    static member QualifiedName (ent: FSharpEntity): string =
        let ent = nonAbbreviatedDefinition ent
        match tryArrayFullName ent with
        | Some fullName -> fullName
        | None ->
            try ent.QualifiedName
            with _ -> ent.LogicalName

    static member FullName (ent: FSharpEntity): string =
        let ent = nonAbbreviatedDefinition ent
        match tryArrayFullName ent with
        | Some fullName -> fullName
        | None when ent.IsNamespace ->
            match ent.Namespace with
            | Some ns -> ns + "." + ent.CompiledName
            | None -> ent.CompiledName
        | None ->
            match ent.TryFullName with
            | Some n -> n
            | None -> ent.LogicalName

    static member Ref (ent: FSharpEntity): Fable.EntityRef =
        let sourcePath =
            if Option.isSome ent.Assembly.FileName
            then None
            else Some(FsEnt.SourcePath ent)
        { QualifiedName = FsEnt.QualifiedName ent
          SourcePath = sourcePath }

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
            ent.TryGetMembersFunctionsAndValues |> Seq.map (fun x ->
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
        member _.IsFSharpUnion = ent.IsFSharpUnion
        member _.IsFSharpRecord = ent.IsFSharpRecord
        member _.IsFSharpExceptionDeclaration = ent.IsFSharpExceptionDeclaration
        member _.IsValueType = ent.IsValueType
        member _.IsInterface = ent.IsInterface

type MemberInfo(?attributes: FSharpAttribute seq,
                    ?hasSpread: bool,
                    ?isPublic: bool,
                    ?isInstance: bool,
                    ?isValue: bool,
                    ?isMutable: bool,
                    ?isGetter: bool,
                    ?isSetter: bool,
                    ?isEnumerator: bool,
                    ?isMangled: bool) =
    interface Fable.MemberInfo with
        member _.Attributes =
            match attributes with
            | Some atts -> atts |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
            | None -> upcast []
        member _.HasSpread = defaultArg hasSpread false
        member _.IsPublic = defaultArg isPublic true
        member _.IsInstance = defaultArg isInstance true
        member _.IsValue = defaultArg isValue false
        member _.IsMutable = defaultArg isMutable false
        member _.IsGetter = defaultArg isGetter false
        member _.IsSetter = defaultArg isSetter false
        member _.IsEnumerator = defaultArg isEnumerator false
        member _.IsMangled = defaultArg isMangled false

type Context =
    { Scope: (FSharpMemberOrFunctionOrValue * Fable.Ident * Fable.Expr option) list
      ScopeInlineValues: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      UsedNamesInRootScope: Set<string>
      UseNamesInDeclarationScope: HashSet<string>
      GenericArgs: Map<string, Fable.Type>
      EnclosingMember: FSharpMemberOrFunctionOrValue option
      InlinedFunction: FSharpMemberOrFunctionOrValue option
      CaughtException: Fable.Ident option
      BoundConstructorThis: Fable.Ident option
      BoundMemberThis: Fable.Ident option
      InlinePath: Log.InlinePath list
      CaptureBaseConsCall: (FSharpEntity * (Fable.Expr -> unit)) option
    }
    static member Create(usedRootNames) =
        { Scope = []
          ScopeInlineValues = []
          UsedNamesInRootScope = usedRootNames
          UseNamesInDeclarationScope = Unchecked.defaultof<_>
          GenericArgs = Map.empty
          EnclosingMember = None
          InlinedFunction = None
          CaughtException = None
          BoundConstructorThis = None
          BoundMemberThis = None
          InlinePath = []
          CaptureBaseConsCall = None
        }

type IFableCompiler =
    inherit Compiler
    abstract Transform: Context * FSharpExpr -> Fable.Expr
    abstract TryReplace: Context * SourceLocation option * Fable.Type *
        info: Fable.ReplaceCallInfo * thisArg: Fable.Expr option * args: Fable.Expr list -> Fable.Expr option
    abstract InjectArgument: Context * SourceLocation option *
        genArgs: ((string * Fable.Type) list) * FSharpParameter -> Fable.Expr
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> InlineExpr
    abstract TryGetImplementationFile: filename: string -> FSharpImplementationFileContents option

module Helpers =
    let rec nonAbbreviatedType (t: FSharpType) =
        let isSameType (t1: FSharpType) (t2: FSharpType) =
            t1.HasTypeDefinition && t2.HasTypeDefinition && (t1.TypeDefinition = t2.TypeDefinition)
        if t.IsAbbreviation && not (isSameType t t.AbbreviatedType) then
            nonAbbreviatedType t.AbbreviatedType
        // TODO!!! Do we still need to make a special check for units of measure
        // or can we just hardcode the names? We may need to do it anyway to fix #1962
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

    let private getEntityMangledName (com: Compiler) trimRootModule (ent: Fable.EntityRef) =
        let fullName = ent.FullName
        match trimRootModule, ent.SourcePath with
        | true, Some sourcePath ->
            let rootMod = com.GetRootModule(sourcePath)
            if fullName.StartsWith(rootMod) then
                fullName.Substring(rootMod.Length).TrimStart('.')
            else fullName
        | _ -> fullName

    let cleanNameAsJsIdentifier (name: string) =
        if name = ".ctor" then "$ctor"
        else name.Replace('.','_').Replace('`','$')

    let getEntityDeclarationName (com: Compiler) (ent: Fable.EntityRef) =
        let entityName = getEntityMangledName com true ent |> cleanNameAsJsIdentifier
        (entityName, Naming.NoMemberPart)
        ||> Naming.sanitizeIdent (fun _ -> false)

    let private getMemberMangledName (com: Compiler) trimRootModule (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsExtensionMember then
            let overloadSuffix = OverloadSuffix.getExtensionHash memb
            let entName = FsEnt.Ref memb.ApparentEnclosingEntity |> getEntityMangledName com false
            entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
        else
            match memb.DeclaringEntity with
            | Some ent ->
                let entFullName = FsEnt.Ref ent
                if ent.IsFSharpModule then
                    match getEntityMangledName com trimRootModule entFullName with
                    | "" -> memb.CompiledName, Naming.NoMemberPart
                    | moduleName -> moduleName, Naming.StaticMemberPart(memb.CompiledName, "")
                else
                    let overloadSuffix = OverloadSuffix.getHash ent memb
                    let entName = getEntityMangledName com trimRootModule entFullName
                    if memb.IsInstanceMember
                    then entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
                    else entName, Naming.StaticMemberPart(memb.CompiledName, overloadSuffix)
            | None -> memb.CompiledName, Naming.NoMemberPart

    /// Returns the sanitized name for the member declaration and whether it has an overload suffix
    let getMemberDeclarationName (com: Compiler) (memb: FSharpMemberOrFunctionOrValue) =
        let name, part = getMemberMangledName com true memb
        let name = cleanNameAsJsIdentifier name
        let part = part.Replace(cleanNameAsJsIdentifier)
        let sanitizedName = Naming.sanitizeIdent (fun _ -> false) name part
        sanitizedName, not(String.IsNullOrEmpty(part.OverloadSuffix))

    /// Used to identify members uniquely in the inline expressions dictionary
    let getMemberUniqueName (com: Compiler) (memb: FSharpMemberOrFunctionOrValue): string =
        getMemberMangledName com false memb
        ||> Naming.buildNameWithoutSanitation

    let getMemberDisplayName (memb: FSharpMemberOrFunctionOrValue) =
        Naming.removeGetSetPrefix memb.DisplayName

    let isUsedName (ctx: Context) name =
        ctx.UsedNamesInRootScope.Contains name || ctx.UseNamesInDeclarationScope.Contains name

    let getIdentUniqueName (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (isUsedName ctx)
        ctx.UseNamesInDeclarationScope.Add(name) |> ignore
        name

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition then
            typ.TypeDefinition.TryFullName = Some Types.unit
        else false

    let tryFindAtt fullName (atts: FSharpAttribute seq) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName' ->
                if fullName = fullName' then Some att else None
            | None -> None)

    let hasAttribute attFullName (attributes: FSharpAttribute seq) =
        let mutable found = false
        let attFullName = Some attFullName
        for att in attributes do
            found <- found || att.AttributeType.TryFullName = attFullName
        found

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
        // TODO: Add compiler option to inline also `OptionalInline`
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline
        | FSharpInlineAnnotation.AggressiveInline -> true

    let isPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then false
        else not memb.Accessibility.IsPrivate

    let makeRange (r: Range.range) =
        { start = { line = r.StartLine; column = r.StartColumn }
          ``end``= { line = r.EndLine; column = r.EndColumn }
          identifierName = None }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    let unionCaseTag (ent: FSharpEntity) (unionCase: FSharpUnionCase) =
        try
            ent.UnionCases |> Seq.findIndex (fun uci -> unionCase.Name = uci.Name)
        with _ ->
            failwithf "Cannot find case %s in %s" unionCase.Name (FsEnt.FullName ent)

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

        // TODO: Remove this after removing ParamList from Fable.React
        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.map (fun lastParam -> hasAttribute "Fable.Core.ParamListAttribute" lastParam.Attributes)
            |> Option.defaultValue false

        hasParamArray memb || hasParamSeq memb

module Patterns =
    open BasicPatterns
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
            Some (callee, eventName)
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
                Some(memb, None, "ToString", membArgTypes, membArgs)
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

    let private numberTypes =
        dict [Types.int8, Int8
              Types.uint8, UInt8
              Types.int16, Int16
              Types.uint16, UInt16
              Types.int32, Int32
              Types.uint32 , UInt32
              Types.float32, Float32
              Types.float64, Float64
               // Units of measure
              "Microsoft.FSharp.Core.sbyte`1", Int8
              "Microsoft.FSharp.Core.int16`1", Int16
              "Microsoft.FSharp.Core.int`1", Int32
              "Microsoft.FSharp.Core.float32`1", Float32
              "Microsoft.FSharp.Core.float`1", Float64]

    let (|NumberKind|_|) fullName =
        match numberTypes.TryGetValue(fullName) with
        | true, kind -> Some kind
        | false, _ -> None

    let (|OptionUnion|ListUnion|ErasedUnion|ErasedUnionCase|StringEnum|DiscriminatedUnion|)
                            (NonAbbreviatedType typ: FSharpType, unionCase: FSharpUnionCase) =
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
                | Types.valueOption
                | Types.option -> OptionUnion typ.GenericArguments.[0]
                | Types.list -> ListUnion typ.GenericArguments.[0]
                | _ ->
                    tdef.Attributes |> Seq.tryPick (fun att ->
                        match att.AttributeType.TryFullName with
                        | Some Atts.erase -> Some (ErasedUnion(tdef, typ.GenericArguments, getCaseRule att))
                        | Some Atts.stringEnum -> Some (StringEnum(tdef, getCaseRule att))
                        | _ -> None)
                    |> Option.defaultValue (DiscriminatedUnion(tdef, typ.GenericArguments))
        )

    let (|ContainsAtt|_|) (fullName: string) (ent: FSharpEntity) =
        tryFindAtt fullName ent.Attributes

module TypeHelpers =
    open Helpers
    open Patterns

    // Sometimes the names of user-declared and compiler-generated clash, see #1900
    let genParamName (genParam: FSharpGenericParameter) =
        if genParam.IsCompilerGenerated then genParam.Name + "$"
        else genParam.Name

    let resolveGenParam ctxTypeArgs (genParam: FSharpGenericParameter) =
        let name = genParamName genParam
        match Map.tryFind name ctxTypeArgs with
        | None -> Fable.GenericParam name
        | Some typ -> typ

    let makeGenArgs ctxTypeArgs (genArgs: IList<FSharpType>) =
        genArgs |> Seq.map (fun genArg ->
            if genArg.IsGenericParameter
            then resolveGenParam ctxTypeArgs genArg.GenericParameter
            else makeType ctxTypeArgs genArg)
        |> Seq.toList

    let makeTypeFromDelegate ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        let argTypes, returnType =
            try
                tdef.FSharpDelegateSignature.DelegateArguments |> Seq.map snd,
                tdef.FSharpDelegateSignature.DelegateReturnType
            with _ -> // tdef.FSharpDelegateSignature doesn't work with System.Func & friends
                let invokeMember =
                    tdef.MembersFunctionsAndValues
                    |> Seq.find (fun f -> f.DisplayName = "Invoke")
                invokeMember.CurriedParameterGroups.[0] |> Seq.map (fun p -> p.Type),
                invokeMember.ReturnParameter.Type
        let genArgs = Seq.zip (tdef.GenericParameters |> Seq.map genParamName) genArgs |> Map
        let resolveType (t: FSharpType) =
            if t.IsGenericParameter then Map.find (genParamName t.GenericParameter) genArgs else t
        let argTypes = argTypes |> Seq.map (resolveType >> makeType ctxTypeArgs) |> Seq.toList
        let returnType = returnType |> resolveType |> makeType ctxTypeArgs
        Fable.DelegateType(argTypes, returnType)

    let makeTypeFromDef ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        if tdef.IsArrayType then
            makeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.Array
        elif tdef.IsDelegate then
            makeTypeFromDelegate ctxTypeArgs genArgs tdef
        elif tdef.IsEnum then
            Fable.Enum(FsEnt.Ref tdef)
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
            | Types.valueOption
            | Types.option -> makeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.Option
            | Types.resizeArray -> makeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.Array
            | Types.list -> makeGenArgs ctxTypeArgs genArgs |> List.head |> Fable.List
            | NumberKind kind -> Fable.Number kind
            // Special attributes
            | _ when hasAttribute Atts.stringEnum tdef.Attributes -> Fable.String
            | _ when hasAttribute Atts.erase tdef.Attributes -> Fable.Any
            // Rest of declared types
            | _ -> Fable.DeclaredType(FsEnt.Ref tdef, makeGenArgs ctxTypeArgs genArgs)

    let rec makeType (ctxTypeArgs: Map<string, Fable.Type>) (NonAbbreviatedType t) =
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter then
            resolveGenParam ctxTypeArgs t.GenericParameter
        // Tuple
        elif t.IsTupleType then
            makeGenArgs ctxTypeArgs t.GenericArguments |> Fable.Tuple
        // Function
        elif t.IsFunctionType then
            let argType = makeType ctxTypeArgs t.GenericArguments.[0]
            let returnType = makeType ctxTypeArgs t.GenericArguments.[1]
            Fable.LambdaType(argType, returnType)
        elif t.IsAnonRecordType then
            let genArgs = makeGenArgs ctxTypeArgs t.GenericArguments
            Fable.AnonymousRecordType(t.AnonRecordTypeDetails.SortedFieldNames, genArgs)
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

    let rec getOwnAndInheritedFsharpMembers (tdef: FSharpEntity) = seq {
        yield! tdef.TryGetMembersFunctionsAndValues
        match getBaseEntity tdef with
        | Some(baseDef, _) -> yield! getOwnAndInheritedFsharpMembers baseDef
        | _ -> ()
    }

    let getArgTypes com (memb: FSharpMemberOrFunctionOrValue) =
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

    let tryFindMember com (entity: Fable.Entity) genArgs compiledName isInstance (argTypes: Fable.Type list) =
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

    let fitsAnonRecordInInterface com (argExprs: Fable.Expr list) fieldNames (interface_: Fable.Entity) =
        match interface_ with
        | :? FsEnt as fsEnt ->
            let interface_ = fsEnt.FSharpEntity
            // TODO: Check also if there are extra fields in the record not present in the interface?
            (Ok (), getAllInterfaceMembers interface_ |> Seq.filter (fun memb -> memb.IsPropertyGetterMethod))
            ||> Seq.fold (fun res memb ->
                match res with
                | Error _ -> res
                | Ok _ ->
                    let expectedType = memb.ReturnParameter.Type |> makeType Map.empty
                    Array.tryFindIndex ((=) memb.DisplayName) fieldNames
                    |> function
                        | None ->
                            match expectedType with
                            | Fable.Option _ -> Ok () // Optional fields can be missing
                            | _ -> sprintf "Object doesn't contain field '%s'" memb.DisplayName |> Error
                        | Some i ->
                            let e = List.item i argExprs
                            match expectedType, e.Type with
                            | Fable.Any, _ -> true
                            | Fable.Option t1, Fable.Option t2
                            | Fable.Option t1, t2
                            | t1, t2 -> typeEquals false t1 t2
                            |> function
                                | true -> Ok ()
                                | false ->
                                    let typeName = getTypeFullName true expectedType
                                    sprintf "Expecting type '%s' for field '%s'" typeName memb.DisplayName |> Error)
        | _ -> Ok () // TODO: Error instead if we cannot check the interface?



    let inline (|FableType|) com (ctx: Context) t = makeType ctx.GenericArgs t

module Identifiers =
    open Helpers
    open TypeHelpers

    let putIdentInScope (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) (ident: Fable.Ident) value =
        { ctx with Scope = (fsRef, ident, value)::ctx.Scope}

    let makeIdentFrom (com: IFableCompiler) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue): Fable.Ident =
        let sanitizedName = (fsRef.CompiledName, Naming.NoMemberPart)
                            ||> Naming.sanitizeIdent (isUsedName ctx)
        ctx.UseNamesInDeclarationScope.Add(sanitizedName) |> ignore
        { Name = sanitizedName
          Type = makeType ctx.GenericArgs fsRef.FullType
          IsThisArgument = false
          IsCompilerGenerated = fsRef.IsCompilerGenerated
          IsMutable = fsRef.IsMutable
          Range = { makeRange fsRef.DeclarationLocation
                    with identifierName = Some fsRef.DisplayName } |> Some }

    let putArgInScope com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        let ident = makeIdentFrom com ctx fsRef
        putIdentInScope ctx fsRef ident None, ident

    let (|PutArgInScope|) com ctx fsRef = putArgInScope com ctx fsRef

    let putBindingInScope com ctx (fsRef: FSharpMemberOrFunctionOrValue) value: Context*Fable.Ident =
        let ident = makeIdentFrom com ctx fsRef
        putIdentInScope ctx fsRef ident (Some value), ident

    let identWithRange r (ident: Fable.Ident) =
        let originalName = ident.Range |> Option.bind (fun r -> r.identifierName)
        { ident with Range = r |> Option.map (fun r -> { r with identifierName = originalName }) }

    let tryGetIdentFromScopeIf (ctx: Context) r predicate =
        ctx.Scope |> List.tryPick (fun (fsRef, ident, _) ->
            if predicate fsRef then identWithRange r ident |> Fable.IdentExpr |> Some
            else None)

    /// Get corresponding identifier to F# value in current scope
    let tryGetIdentFromScope (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        tryGetIdentFromScopeIf ctx r (fun fsRef' -> obj.Equals(fsRef, fsRef'))

    let rec tryGetBoundValueFromScope (ctx: Context) identName =
        match ctx.Scope |> List.tryFind (fun (_,ident,_) -> ident.Name = identName) with
        | Some(_,_,value) ->
            match value with
            | Some(Fable.IdentExpr ident) when not ident.IsMutable ->
                tryGetBoundValueFromScope ctx ident.Name
            | v -> v
        | None -> None

module Util =
    open Helpers
    open Patterns
    open TypeHelpers
    open Identifiers

    let makeFunctionArgs com ctx (args: FSharpMemberOrFunctionOrValue list) =
        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = putArgInScope com ctx var
                newContext, arg::accArgs)
        ctx, List.rev args

    let bindMemberArgs com ctx (args: FSharpMemberOrFunctionOrValue list list) =
        let ctx, transformedArgs, args =
            match args with
            | (firstArg::restArgs1)::restArgs2 when firstArg.IsMemberThisValue ->
                let ctx, thisArg = putArgInScope com ctx firstArg
                let thisArg = { thisArg with IsThisArgument = true }
                let ctx = { ctx with BoundMemberThis = Some thisArg }
                ctx, [thisArg], restArgs1::restArgs2
            | (firstArg::restArgs1)::restArgs2 when firstArg.IsConstructorThisValue ->
                let ctx, thisArg = putArgInScope com ctx firstArg
                let thisArg = { thisArg with IsThisArgument = true }
                let ctx = { ctx with BoundConstructorThis = Some thisArg }
                ctx, [thisArg], restArgs1::restArgs2
            | _ -> ctx, [], args
        let ctx, args =
            (args, (ctx, [])) ||> List.foldBack (fun tupledArg (ctx, accArgs) ->
                // The F# compiler "untuples" the args in methods
                let ctx, untupledArg = makeFunctionArgs com ctx tupledArg
                ctx, untupledArg@accArgs)
        ctx, transformedArgs @ args

    let makeTryCatch com ctx r (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (PutArgInScope com ctx (catchContext, catchVar), catchBody) ->
                // Add caughtException to context so it can be retrieved by `reraise`
                let catchContext = { catchContext with CaughtException = Some catchVar }
                Some (catchVar, com.Transform(catchContext, catchBody))
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch(body, catchClause, finalizer, r)


    let matchGenericParamsFrom (memb: FSharpMemberOrFunctionOrValue) (genArgs: Fable.Type seq) =
        let matchGenericParams (genArgs: Fable.Type seq) (genParams: FSharpGenericParameter seq) =
            Seq.zip (genParams |> Seq.map genParamName) genArgs

        let genArgsLen = Seq.length genArgs
        match memb.DeclaringEntity with
        // It seems that for F# types memb.GenericParameters contains all generics
        // but for BCL types we need to check the DeclaringEntity generics too
        | Some ent when genArgsLen > memb.GenericParameters.Count ->
            Seq.append ent.GenericParameters memb.GenericParameters
        | _ -> upcast memb.GenericParameters
        |> matchGenericParams genArgs

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
    let private fixImportedRelativePath (com: Compiler) (path: string) normalizedSourcePath =
        let file = Path.normalizePathAndEnsureFsExtension normalizedSourcePath
        if file = com.CurrentFile
        then path
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
            makeImportUserGenerated None typ (makeStrConst selector) (makeStrConst path) |> Some
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
            makeImportUserGenerated None Fable.Any (makeStrConst selector) (makeStrConst path) |> Some
        | _ -> None

    let isErasedOrStringEnumEntity (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att ->
            match att.Entity.FullName with
            | Atts.erase | Atts.stringEnum -> true
            | _ -> false)

    let isGlobalOrImportedEntity (ent: Fable.Entity) =
        ent.Attributes |> Seq.exists (fun att ->
            match att.Entity.FullName with
            | Atts.global_ | Naming.StartsWith Atts.import _ -> true
            | _ -> false)

    let private isReplacementCandidatePrivate isFromDllRef (entFullName: string) =
        if entFullName.StartsWith("System.") || entFullName.StartsWith("Microsoft.FSharp.") then isFromDllRef
        // When compiling Fable itself, Fable.Core entities will be part of the code base,
        // but still need to be replaced
        else entFullName.StartsWith("Fable.Core.")

    let isReplacementCandidate (ent: Fable.Entity) =
        let isFromDllRef = Option.isNone ent.Ref.SourcePath
        isReplacementCandidatePrivate isFromDllRef ent.FullName

    let isReplacementCandidateFrom (ent: FSharpEntity) =
        let isFromDllRef = Option.isSome ent.Assembly.FileName
        isReplacementCandidatePrivate isFromDllRef (FsEnt.FullName ent)

    /// We can add a suffix to the entity name for special methods, like reflection declaration
    let entityRefWithSuffix (com: Compiler) (ent: Fable.Entity) suffix =
        let error msg =
            sprintf "%s: %s" msg ent.FullName
            |> addErrorAndReturnNull com [] None
        match ent.IsInterface, ent.Ref.SourcePath with
        | true, _ -> error "Cannot reference an interface"
        | _, None -> error "Cannot reference entity from .dll reference"
        | _, Some file ->
            let entityName = getEntityDeclarationName com ent.Ref + suffix
            if file = com.CurrentFile then
                makeIdentExpr entityName
            elif ent.IsPublic then
                makeImportInternal com Fable.Any entityName file
            else
                error "Cannot inline functions that reference private entities"

    let entityRef (com: Compiler) (ent: Fable.Entity) =
        entityRefWithSuffix com ent ""

    /// First checks if the entity is global or imported
    let entityRefMaybeGlobalOrImported (com: Compiler) (ent: Fable.Entity) =
        match tryGlobalOrImportedEntity com ent with
        | Some importedEntity -> importedEntity
        | None -> entityRef com ent

    let memberRefTyped (com: Compiler) (ctx: Context) r typ (memb: FSharpMemberOrFunctionOrValue) =
        let r = r |> Option.map (fun r -> { r with identifierName = Some memb.DisplayName })
        let memberName, hasOverloadSuffix = getMemberDeclarationName com memb
        let file =
            memb.DeclaringEntity
            |> Option.bind (fun ent -> FsEnt.Ref(ent).SourcePath)
            // Cases when .DeclaringEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            |> Option.defaultValue com.CurrentFile
        if file = com.CurrentFile then
            { makeTypedIdent typ memberName with Range = r }
            |> Fable.IdentExpr
        elif isPublicMember memb then
            // If the overload suffix changes, we need to recompile the files that call this member
            if hasOverloadSuffix then
                com.AddWatchDependency(file)
            makeImportInternal com typ memberName file
        else
            defaultArg (memb.TryGetFullDisplayName()) memb.CompiledName
            |> sprintf "Cannot reference private members from other files: %s"
            |> addErrorAndReturnNull com ctx.InlinePath r

    let memberRef (com: IFableCompiler) ctx r (memb: FSharpMemberOrFunctionOrValue) =
        memberRefTyped com ctx r Fable.Any memb

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

    let isMangledAbstractEntity (ent: FSharpEntity) =
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
        // Don't mangle interfaces by default (for better JS interop) unless they have Mangle attribute
        | _ when ent.IsInterface -> hasAttribute Atts.mangle ent.Attributes
        // Mangle members from abstract classes unless they are global/imported
        | _ -> not(isGlobalOrImportedEntity(FsEnt ent))

    let getMangledAbstractMemberName (ent: FSharpEntity) memberName overloadHash =
        // TODO: Error if entity doesn't have fullname?
        let entityName = defaultArg ent.TryFullName ""
        entityName + "." + memberName + overloadHash

    let callInstanceMember com r typ (callInfo: Fable.CallInfo)
                    (entity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) =
        let callInfo, callee =
            match callInfo.ThisArg with
            | Some callee -> { callInfo with ThisArg = None }, callee
            | None ->
                sprintf "Unexpected static interface/override call: %s" memb.FullName
                |> attachRange r |> failwith
        let isGetter = memb.IsPropertyGetterMethod
        let isSetter = not isGetter && memb.IsPropertySetterMethod
        let indexedProp = (isGetter && countNonCurriedParams memb > 0) || (isSetter && countNonCurriedParams memb > 1)
        let name, isGetter, isSetter =
            if isMangledAbstractEntity entity then
                let overloadHash =
                    if (isGetter || isSetter) && not indexedProp then ""
                    else OverloadSuffix.getHash entity memb
                getMangledAbstractMemberName entity memb.CompiledName overloadHash, false, false
            else
                // Indexed properties keep the get_/set_ prefix and are compiled as methods
                if indexedProp then memb.CompiledName, false, false
                else getMemberDisplayName memb, isGetter, isSetter
        if isGetter then
            let t = memb.ReturnParameter.Type |> makeType Map.empty
            // Set the field as mutable to prevent beta reduction
            let key = makeFieldKey name true t
            Fable.Get(callee, Fable.ByKey key, typ, r)
        elif isSetter then
            let t = memb.CurriedParameterGroups.[0].[0].Type |> makeType Map.empty
            let arg = callInfo.Args |> List.tryHead |> Option.defaultWith makeNull
            let key = makeFieldKey name true t
            Fable.Set(callee, Some key, arg, r)
        else
            getSimple callee name |> makeCall r typ callInfo

    let (|Replaced|_|) (com: IFableCompiler) ctx r typ (genArgs: Lazy<_>) (callInfo: Fable.CallInfo)
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
                OverloadSuffix = if ent.IsFSharpModule then "" else OverloadSuffix.getHash ent memb
                GenericArgs = genArgs.Value }
            match com.TryReplace(ctx, r, typ, info, callInfo.ThisArg, callInfo.Args) with
            | Some e -> Some e
            | None when info.IsInterface ->
                callInstanceMember com r typ callInfo ent memb |> Some
            | None ->
                sprintf "Cannot resolve %s.%s" info.DeclaringEntityFullName info.CompiledName
                |> addErrorAndReturnNull com ctx.InlinePath r |> Some
        | _ -> None

    let (|Emitted|_|) com r typ (callInfo: Fable.CallInfo option) (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Naming.StartsWith Atts.emit _ as attFullName) ->
                let callInfo =
                    match callInfo with
                    | Some i -> i
                    | None -> { ThisArg = None
                                Args = []
                                SignatureArgTypes = []
                                HasSpread = false
                                IsJsConstructor = false }
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
                      IsJsStatement = isStatement
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
            match tryGlobalOrImportedEntity com (FsEnt e), callInfo.ThisArg with
            | Some _, Some _thisArg ->
                callInstanceMember com r typ callInfo e memb |> Some

            | Some classExpr, None when memb.IsConstructor ->
                Fable.Call(classExpr, { callInfo with IsJsConstructor = true }, typ, r) |> Some

            | Some moduleOrClassExpr, None ->
                if isModuleValueForCalls e memb then
                    // Set the field as mutable just in case, so it's not displaced by beta reduction
                    let fieldGet = makeFieldKey (getMemberDisplayName memb) true Fable.Any
                    Fable.Get(moduleOrClassExpr, Fable.ByKey fieldGet, typ, r) |> Some
                else
                    let callInfo = { callInfo with ThisArg = Some moduleOrClassExpr }
                    callInstanceMember com r typ callInfo e memb |> Some

            | None, _ -> None
        | _ -> None

    let inlineExpr (com: IFableCompiler) (ctx: Context) r (genArgs: Lazy<_>) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let rec foldArgs acc = function
            | argIdent::restArgIdents, argExpr::restArgExprs ->
                foldArgs ((argIdent, argExpr)::acc) (restArgIdents, restArgExprs)
            | (argIdent: FSharpMemberOrFunctionOrValue)::restArgIdents, [] ->
                let t = makeType ctx.GenericArgs argIdent.FullType
                foldArgs ((argIdent, Fable.Value(Fable.NewOption(None, t), None))::acc) (restArgIdents, [])
            | [], _ -> List.rev acc

        // Log error if the inline function is called recursively
        match ctx.InlinedFunction with
        | Some memb2 when memb.Equals(memb2) ->
            sprintf "Recursive functions cannot be inlined: (%s)" memb.FullName
            |> addErrorAndReturnNull com [] r
        | _ ->
            let args: Fable.Expr list =
                match callee with
                | Some c -> c::args
                | None -> args

            let inExpr = com.GetInlineExpr(memb)

            let ctx, bindings =
                ((ctx, []), foldArgs [] (inExpr.Args, args)) ||> List.fold (fun (ctx, bindings) (argId, arg) ->
                    // Change type and mark ident as compiler-generated so Fable also
                    // tries to inline it in DEBUG mode (some patterns depend on this)
                    let ident = { makeIdentFrom com ctx argId with
                                    Type = arg.Type
                                    IsCompilerGenerated = true }
                    let ctx = putIdentInScope ctx argId ident (Some arg)
                    ctx, (ident, arg)::bindings)

            let fromFile, fromRange =
                match ctx.InlinePath with
                | { ToFile = file; ToRange = r }::_ -> file, r
                | [] -> com.CurrentFile, r

            let ctx = { ctx with GenericArgs = genArgs.Value |> Map
                                 InlinedFunction = Some memb
                                 InlinePath = { ToFile = inExpr.FileName
                                                ToRange = makeRangeFrom inExpr.Body
                                                FromFile = fromFile
                                                FromRange = fromRange }::ctx.InlinePath }

            (com.Transform(ctx, inExpr.Body), bindings)
            ||> List.fold (fun body binding -> Fable.Let([binding], body))

    let (|Inlined|_|) (com: IFableCompiler) ctx r genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        if not(isInline memb)
        then None
        else inlineExpr com ctx r genArgs callee args memb |> Some

    /// Removes optional arguments set to None in tail position and calls the injector if necessary
    let transformOptionalArguments (com: IFableCompiler) (ctx: Context) r
                (memb: FSharpMemberOrFunctionOrValue) (genArgs: Lazy<_>) (args: Fable.Expr list) =
        if memb.CurriedParameterGroups.Count <> 1
            || memb.CurriedParameterGroups.[0].Count <> (List.length args)
        then args
        else
            (memb.CurriedParameterGroups.[0], args, ("optional", []))
            |||> Seq.foldBack2 (fun par arg (condition, acc) ->
                match condition with
                | "optional" | "inject" when par.IsOptionalArg ->
                    match arg with
                    | Fable.Value(Fable.NewOption(None,_),_) ->
                        match tryFindAtt Atts.inject par.Attributes with
                        | Some _ -> "inject", (com.InjectArgument(ctx, r, genArgs.Value, par))::acc
                        // Don't remove optional arguments if they're not in tail position
                        | None -> condition, if condition = "optional" then acc else arg::acc
                    | _ -> "inject", arg::acc // Keep checking for injects
                | _ -> "none", arg::acc)
            |> snd

    let hasInterface interfaceFullname (ent: Fable.Entity) =
        ent.AllInterfaces |> Seq.exists (fun ifc -> ifc.Entity.FullName = interfaceFullname)

    let makeCallWithArgInfo com ctx r typ genArgs callee (memb: FSharpMemberOrFunctionOrValue) (callInfo: Fable.CallInfo) =
        match memb, memb.DeclaringEntity with
        | Emitted com r typ (Some callInfo) emitted, _ -> emitted
        | Imported com r typ (Some callInfo) imported -> imported
        | Replaced com ctx r typ genArgs callInfo replaced -> replaced
        | Inlined com ctx r genArgs callee callInfo.Args expr, _ -> expr

        | Try (tryGetIdentFromScope ctx r) funcExpr, Some entity ->
            if isModuleValueForCalls entity memb then funcExpr
            else makeCall r typ callInfo funcExpr

        // Check if this is an interface or abstract/overriden method
        | _, Some entity when entity.IsInterface
                || memb.IsOverrideOrExplicitInterfaceImplementation
                || memb.IsDispatchSlot ->
            callInstanceMember com r typ callInfo entity memb

        | _, Some entity when isModuleValueForCalls entity memb ->
            let typ = makeType ctx.GenericArgs memb.FullType
            memberRefTyped com ctx r typ memb

        | _ ->
            let atts =
                memb.Attributes
                |> Seq.map (fun a -> FsAtt(a) :> Fable.Attribute)
            memberRef com ctx r memb
            |> makeCall r typ callInfo
            |> fun e -> com.ApplyCallPlugin(atts, e)

    let makeCallInfoFrom (com: IFableCompiler) ctx r genArgs callee args (memb: FSharpMemberOrFunctionOrValue): Fable.CallInfo =
        {
            ThisArg = callee
            Args = transformOptionalArguments com ctx r memb genArgs args
            SignatureArgTypes = getArgTypes com memb
            HasSpread = hasParamArray memb
            IsJsConstructor = false
        }

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ (genArgs: Fable.Type seq) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let genArgs = lazy(matchGenericParamsFrom memb genArgs |> Seq.toList)
        makeCallInfoFrom com ctx r genArgs callee args memb
        |> makeCallWithArgInfo com ctx r typ genArgs callee memb

    let makeValueFrom (com: IFableCompiler) (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType ctx.GenericArgs v.FullType
        match v, v.DeclaringEntity with
        | _ when typ = Fable.Unit ->
            if com.Options.Verbosity = Verbosity.Verbose && not v.IsCompilerGenerated then // See #1516
                sprintf "Value %s is replaced with unit constant" v.DisplayName
                |> addWarning com ctx.InlinePath r
            Fable.Value(Fable.UnitConstant, r)
        | Emitted com r typ None emitted, _ -> emitted
        | Imported com r typ None imported -> imported
        | Try (tryGetIdentFromScope ctx r) expr, _ -> expr
        | _ -> memberRefTyped com ctx r typ v
