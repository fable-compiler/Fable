namespace Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open FSharp.Compiler
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.Core
open Fable.AST
open Fable.Transforms

type Context =
    { Scope: (FSharpMemberOrFunctionOrValue * Fable.Expr) list
      ScopeInlineValues: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      GenericArgs: Map<string, Fable.Type>
      EnclosingMember: FSharpMemberOrFunctionOrValue option
      EnclosingEntity: FSharpEntity option
      InlinedFunction: FSharpMemberOrFunctionOrValue option
      CaughtException: Fable.Ident option
      BoundConstructorThis: Fable.Ident option
      BoundMemberThis: Fable.Ident option
      InlinePath: (string * (SourceLocation option)) list
      CaptureBaseConsCall: (FSharpEntity * (Fable.Expr * Fable.Expr -> unit)) option
    }
    static member Create(enclosingEntity) =
        { Scope = []
          ScopeInlineValues = []
          GenericArgs = Map.empty
          EnclosingMember = None
          EnclosingEntity = enclosingEntity
          InlinedFunction = None
          CaughtException = None
          BoundConstructorThis = None
          BoundMemberThis = None
          InlinePath = []
          CaptureBaseConsCall = None
        }

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context * FSharpExpr -> Fable.Expr
    abstract TryReplace: Context * SourceLocation option * Fable.Type *
        info: Fable.ReplaceCallInfo * thisArg: Fable.Expr option * args: Fable.Expr list -> Fable.Expr option
    abstract InjectArgument: Context * SourceLocation option *
        genArgs: ((string * Fable.Type) list) * FSharpParameter -> Fable.Expr
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> InlineExpr
    abstract AddUsedVarName: string * ?isRoot: bool -> unit
    abstract IsUsedVarName: string -> bool

module Helpers =
    let rec nonAbbreviatedType (t: FSharpType) =
        if t.IsAbbreviation then nonAbbreviatedType t.AbbreviatedType
        elif t.HasTypeDefinition then
            let abbr = t.AbbreviatedType
            // .IsAbbreviation doesn't eval to true for generic numbers
            // See https://github.com/Microsoft/visualfsharp/issues/5992
            if t.GenericArguments.Count = abbr.GenericArguments.Count then t
            else abbr
        else t

    // TODO: Report bug in FCS repo, when ent.IsNamespace, FullName doesn't work.
    let getEntityFullName (ent: FSharpEntity) =
        if ent.IsNamespace
        then match ent.Namespace with Some ns -> ns + "." + ent.CompiledName | None -> ent.CompiledName
        else defaultArg ent.TryFullName ent.CompiledName

    let getGenericArguments (t: FSharpType) =
        // Accessing .GenericArguments for a generic parameter will fail
        if t.IsGenericParameter
        then [||] :> IList<_>
        else (nonAbbreviatedType t).GenericArguments

    let inline getEntityLocation (ent: FSharpEntity) =
        ent.DeclarationLocation
        // As we're using a hash for the overload suffix, we shouldn't care
        // whether the location belongs to the implementation or the signature
        // match ent.ImplementationLocation with
        // | Some loc -> loc
        // | None -> ent.DeclarationLocation

    let inline getMemberLocation (memb: FSharpMemberOrFunctionOrValue) =
        memb.DeclarationLocation
        // match memb.ImplementationLocation with
        // | Some loc -> loc
        // | None -> memb.DeclarationLocation

    let private getEntityMangledName (com: ICompiler) trimRootModule (ent: FSharpEntity) =
        match ent.TryFullName with
        | Some fullName when not trimRootModule -> fullName
        | Some fullName ->
            let loc = getEntityLocation ent
            let rootMod = com.GetRootModule(loc.FileName)
            if fullName.StartsWith(rootMod)
            then fullName.Substring(rootMod.Length).TrimStart('.')
            else fullName
        | None -> ent.CompiledName

    let getEntityDeclarationName (com: ICompiler) (ent: FSharpEntity) =
        (getEntityMangledName com true ent, Naming.NoMemberPart)
        ||> Naming.sanitizeIdent (fun _ -> false)

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some Types.unit
        else false

    let getInterfaceImplementationName com (implementingEntity: FSharpEntity) (interfaceEntityFullName: string) =
        let entityName = getEntityMangledName com true implementingEntity
        let memberPart = Naming.StaticMemberPart(interfaceEntityFullName, "")
        Naming.sanitizeIdent (fun _ -> false) entityName memberPart

    let private getMemberMangledName (com: ICompiler) trimRootModule (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsExtensionMember then
            let overloadSuffix = OverloadSuffix.getExtensionHash memb
            let entName = getEntityMangledName com false memb.ApparentEnclosingEntity
            entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
        else
            match memb.DeclaringEntity with
            | Some ent when ent.IsFSharpModule ->
                match getEntityMangledName com trimRootModule ent with
                | "" -> memb.CompiledName, Naming.NoMemberPart
                | moduleName -> moduleName, Naming.StaticMemberPart(memb.CompiledName, "")
            | Some ent ->
                let overloadSuffix = OverloadSuffix.getHash ent memb
                let entName = getEntityMangledName com trimRootModule ent
                if memb.IsInstanceMember
                then entName, Naming.InstanceMemberPart(memb.CompiledName, overloadSuffix)
                else entName, Naming.StaticMemberPart(memb.CompiledName, overloadSuffix)
            | None -> memb.CompiledName, Naming.NoMemberPart

    let getMemberDeclarationName (com: ICompiler) (memb: FSharpMemberOrFunctionOrValue) =
        getMemberMangledName com true memb
        ||> Naming.sanitizeIdent (fun _ -> false)

    /// Used to identify members uniquely in the inline expressions dictionary
    let getMemberUniqueName (com: ICompiler) (memb: FSharpMemberOrFunctionOrValue): string =
        getMemberMangledName com false memb
        ||> Naming.buildNameWithoutSanitation

    /// TODO: Latest FCS seems to add get_/set_ to DisplayName. Bug or feature?
    let getMemberDisplayName (memb: FSharpMemberOrFunctionOrValue) =
        Naming.removeGetSetPrefix memb.DisplayName

    let tryFindAtt fullName (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName' ->
                if fullName = fullName' then Some att else None
            | None -> None)

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

    let tryEntityBase (ent: FSharpEntity) =
        ent.BaseType
        |> Option.bind tryDefinition
        |> Option.bind (fun (baseEntity, fullName) ->
            if fullName = Some Types.object then None else Some baseEntity)

    let isInline (memb: FSharpMemberOrFunctionOrValue) =
        match memb.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        // TODO: Add compiler option to inline also `OptionalInline`
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline
        | FSharpInlineAnnotation.AggressiveInline -> true

    let isPublicEntity (ent: FSharpEntity) =
        not ent.Accessibility.IsPrivate

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

    // let hasCaseWithFields (ent: FSharpEntity) =
    //     ent.UnionCases |> Seq.exists (fun uci -> uci.UnionCaseFields.Count > 0)

    let unionCaseTag (ent: FSharpEntity) (unionCase: FSharpUnionCase) =
        try
            ent.UnionCases |> Seq.findIndex (fun uci -> unionCase.Name = uci.Name)
        with _ ->
            failwithf "Cannot find case %s in %s" unionCase.Name (getEntityFullName ent)

    /// FSharpUnionCase.CompiledName doesn't give the value of CompiledNameAttribute
    /// We must check the attributes explicitly
    let unionCaseCompiledName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt Atts.compiledName
        |> Option.map (fun att -> att.ConstructorArguments.[0] |> snd |> string)

    /// Apply case rules to case name if there's no explicit compiled name
    let applyCaseRule (rule: CaseRules) (unionCase: FSharpUnionCase) =
        match unionCaseCompiledName unionCase with
        | Some name -> name
        | None ->
            match rule with
            | CaseRules.LowerFirst -> Naming.lowerFirst unionCase.Name
            | CaseRules.None | _ -> unionCase.Name
        |> makeStrConst

    // let isModuleMember (memb: FSharpMemberOrFunctionOrValue) =
    //     match memb.DeclaringEntity with
    //     | Some ent -> ent.IsFSharpModule
    //     | None -> true // Compiler-generated members

    /// Using memb.IsValue doesn't work for function values
    /// (e.g. `let ADD = adder()` when adder returns a function)
    let isModuleValueForDeclarations (memb: FSharpMemberOrFunctionOrValue) =
        memb.CurriedParameterGroups.Count = 0 && memb.GenericParameters.Count = 0

    let isModuleValueForCalls (memb: FSharpMemberOrFunctionOrValue) =
        isModuleValueForDeclarations memb
        // Mutable public values must be called as functions (see #986)
        && (not memb.IsMutable || not (isPublicMember memb))

    let isSelfConstructorCall (ctx: Context) (memb: FSharpMemberOrFunctionOrValue) =
        match memb.IsConstructor, memb.DeclaringEntity, ctx.EnclosingMember with
        | true, Some ent, Some enclosingMember when enclosingMember.IsConstructor ->
            match enclosingMember.DeclaringEntity with
            | Some enclosingEntity -> ent = enclosingEntity
            | None -> false
        | _ -> false

    let rec isInterfaceEmpty (ent: FSharpEntity) =
        ent.MembersFunctionsAndValues.Count = 0
            && (if ent.DeclaredInterfaces.Count > 0 then
                    ent.DeclaredInterfaces |> Seq.forall (fun ifc ->
                        match tryDefinition ifc with
                        | Some(e, _) -> isInterfaceEmpty e
                        | None -> true)
                else true)

    /// Test if the name corresponds to this interface or anyone in its hierarchy
    let rec testInterfaceHierarcy interfaceFullname interfaceType =
        match tryDefinition interfaceType with
        | Some(e, Some fullname2) ->
            if interfaceFullname = fullname2
            then true
            else e.DeclaredInterfaces
                 |> Seq.exists (testInterfaceHierarcy interfaceFullname)
        | _ -> false

    let hasSeqSpread (memb: FSharpMemberOrFunctionOrValue) =
        let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
            if memb.CurriedParameterGroups.Count <> 1 then false else
            let args = memb.CurriedParameterGroups.[0]
            args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.bind (fun lastParam -> tryFindAtt Atts.paramList lastParam.Attributes)
            |> Option.isSome

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

    let (|AttFullName|_|) (att: FSharpAttribute) =
        match att.AttributeType.TryFullName with
        | Some fullName -> Some(fullName, att)
        | None -> None

    let (|AttArguments|) (att: FSharpAttribute) =
        att.ConstructorArguments |> Seq.map snd |> Seq.toList

    let (|RefType|_|) = function
        | TypeDefinition tdef as t when tdef.TryFullName = Some Types.reference -> Some t
        | _ -> None

    /// Detects AST pattern of "raise MatchFailureException()"
    let (|RaisingMatchFailureExpr|_|) (expr: FSharpExpr) =
        match expr with
        | BasicPatterns.Call(None, methodInfo, [ ], [unitType], [value]) ->
            match methodInfo.FullName with
            | "Microsoft.FSharp.Core.Operators.raise" ->
                match value with
                | BasicPatterns.NewRecord(recordType, [ BasicPatterns.Const (value, valueT) ; rangeFrom; rangeTo ]) ->
                    match recordType.TypeDefinition.FullName with
                    | "Microsoft.FSharp.Core.MatchFailureException"-> Some (value.ToString())
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
                (Call(callee, memb, ownerGenArgs, membGenArgs, callArgs), Value outArg3, elseExpr))
                when List.isMultiple callArgs && outArg1.IsCompilerGenerated && outArg1 = outArg3 ->
            match List.splitLast callArgs with
            | callArgs, AddressOf(Value outArg2) when outArg1 = outArg2 ->
                Some (outArg1, callee, memb, ownerGenArgs, membGenArgs, callArgs@[def], elseExpr)
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
        | Call(Some(Call(None, createEvent,_,_,
                        [Lambda(_eventDelegate, Call(Some callee, addEvent,[],[],[Value _eventDelegate']));
                         Lambda(_eventDelegate2, Call(Some _callee2, _removeEvent,[],[],[Value _eventDelegate2']));
                         Lambda(_callback, NewDelegate(_, Lambda(_delegateArg0, Lambda(_delegateArg1, Application(Value _callback',[],[Value _delegateArg0'; Value _delegateArg1'])))))])),
                memb, typArgs, methTypArgs, args)
                when createEvent.FullName = Types.createEvent ->
            let eventName = addEvent.CompiledName.Replace("add_","")
            Some (callee, eventName, memb, typArgs, methTypArgs, args)
        | _ -> None

    let (|ConstructorCall|_|) = function
        | NewObject(baseCall, genArgs, baseArgs) -> Some(baseCall, genArgs, baseArgs)
        | Call(None, baseCall, genArgs1, genArgs2, baseArgs) when baseCall.IsConstructor ->
            Some(baseCall, genArgs1 @ genArgs2, baseArgs)
        | _ -> None

    let (|CapturedBaseConsCall|_|) com (ctx: Context) transformBaseCall = function
        | BasicPatterns.Sequential(ConstructorCall(call, genArgs, args) as expr1, expr2)
        // This pattern occurs in constructors that define a this value: `type C() as this`
        // TODO: We're discarding the bound `this` value, check if it's actually used in the base constructor arguments?
        | BasicPatterns.Sequential(BasicPatterns.Let(_, (ConstructorCall(call, genArgs, args) as expr1)), expr2) ->
            match call.DeclaringEntity, ctx.CaptureBaseConsCall with
            | Some baseEnt, Some(expectedBaseEnt, capture) when baseEnt = expectedBaseEnt ->
                transformBaseCall com ctx (makeRangeFrom expr1) baseEnt call genArgs args |> capture
                Some expr2
            | _ -> None
        | _ -> None

    let (|OptimizedOperator|_|) = function
        // work-around for optimized string operator (Operators.string)
        | BasicPatterns.Let((var, BasicPatterns.Call(None, memb, _, membArgTypes, membArgs)),
                            BasicPatterns.DecisionTree(BasicPatterns.IfThenElse(_, _, BasicPatterns.IfThenElse
                                                        (BasicPatterns.TypeTest(tt, BasicPatterns.Value vv), _, _)), _))
                when var.FullName = "matchValue" && memb.FullName = "Microsoft.FSharp.Core.Operators.box"
                    && vv.FullName = "matchValue" && (getFsTypeFullName tt) = "System.IFormattable" ->
            Some(memb, None, "ToString", membArgTypes, membArgs)
        // work-around for optimized hash operator (Operators.hash)
        | BasicPatterns.Call(Some expr, memb, _, [], [BasicPatterns.Call(None, comp, [], [], [])])
                when memb.FullName.EndsWith(".GetHashCode") &&
                     comp.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityERComparer" ->
            Some(memb, Some comp, "GenericHash", [expr.Type], [expr])
        // work-around for optimized equality operator (Operators.(=))
        | BasicPatterns.Call(Some e1, memb, _, [], [BasicPatterns.Coerce (t2, e2); BasicPatterns.Call(None, comp, [], [], [])])
                when memb.FullName.EndsWith(".Equals") && t2.HasTypeDefinition && t2.TypeDefinition.CompiledName = "obj" &&
                     comp.FullName = "Microsoft.FSharp.Core.LanguagePrimitives.GenericEqualityComparer" ->
            Some(memb, Some comp, "GenericEquality", [e1.Type; e2.Type], [e1; e2])
        | _ -> None

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

    let (|OptionUnion|ListUnion|ErasedUnion|StringEnum|DiscriminatedUnion|) (NonAbbreviatedType typ: FSharpType) =
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
                    | Some Atts.erase -> Some (ErasedUnion(tdef, typ.GenericArguments))
                    | Some Atts.stringEnum ->
                        match Seq.tryHead att.ConstructorArguments with
                        | Some(_, (:? int as rule)) -> Some (StringEnum(tdef, enum<CaseRules>(rule)))
                        | _ -> Some (StringEnum(tdef, CaseRules.LowerFirst))
                    | _ -> None)
                |> Option.defaultValue (DiscriminatedUnion(tdef, typ.GenericArguments))

    let (|ContainsAtt|_|) (fullName: string) (ent: FSharpEntity) =
        tryFindAtt fullName ent.Attributes

module TypeHelpers =
    open Helpers
    open Patterns

    let resolveGenParam ctxTypeArgs (genParam: FSharpGenericParameter) =
        match Map.tryFind genParam.Name ctxTypeArgs with
        | None -> Fable.GenericParam genParam.Name
        | Some typ -> typ

    let rec makeGenArgs (com: ICompiler) ctxTypeArgs (genArgs: IList<FSharpType>) =
        genArgs |> Seq.map (fun genArg ->
            if genArg.IsGenericParameter
            then resolveGenParam ctxTypeArgs genArg.GenericParameter
            else makeType com ctxTypeArgs genArg)
        |> Seq.toList

    and makeTypeFromDelegate com ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
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
        let genArgs = Seq.zip (tdef.GenericParameters |> Seq.map (fun x -> x.Name)) genArgs |> Map
        let resolveType (t: FSharpType) =
            if t.IsGenericParameter then Map.find t.GenericParameter.Name genArgs else t
        let argTypes = argTypes |> Seq.map (resolveType >> makeType com ctxTypeArgs) |> Seq.toList
        let returnType = returnType |> resolveType |> makeType com ctxTypeArgs
        Fable.FunctionType(Fable.DelegateType argTypes, returnType)

    and makeTypeFromDef (com: ICompiler) ctxTypeArgs (genArgs: IList<FSharpType>) (tdef: FSharpEntity) =
        match getEntityFullName tdef, tdef with
        | _ when tdef.IsArrayType -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.Array
        | _ when tdef.IsDelegate -> makeTypeFromDelegate com ctxTypeArgs genArgs tdef
        | fullName, _ when tdef.IsEnum -> Fable.EnumType(Fable.NumberEnumType, fullName)
        // Fable "primitives"
        | Types.object, _ -> Fable.Any
        | Types.unit, _ -> Fable.Unit
        | Types.bool, _ -> Fable.Boolean
        | Types.char, _ -> Fable.Char
        | Types.string, _ -> Fable.String
        | Types.regex, _ -> Fable.Regex
        | Types.valueOption, _
        | Types.option, _ -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.Option
        | Types.resizeArray, _ -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.Array
        | Types.list, _ -> makeGenArgs com ctxTypeArgs genArgs |> List.head |> Fable.List
        | NumberKind kind, _ -> Fable.Number kind
        // Special attributes
        | fullName, ContainsAtt Atts.stringEnum _ -> Fable.EnumType(Fable.StringEnumType, fullName)
        | _, ContainsAtt Atts.erase _ -> makeGenArgs com ctxTypeArgs genArgs |> Fable.ErasedUnion
        // Rest of declared types
        | _ -> Fable.DeclaredType(tdef, makeGenArgs com ctxTypeArgs genArgs)

    and makeType (com: ICompiler) (ctxTypeArgs: Map<string, Fable.Type>) (NonAbbreviatedType t) =
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter then
            resolveGenParam ctxTypeArgs t.GenericParameter
        // Tuple
        elif t.IsTupleType then
            makeGenArgs com ctxTypeArgs t.GenericArguments |> Fable.Tuple
        // Funtion
        elif t.IsFunctionType then
            let argType = makeType com ctxTypeArgs t.GenericArguments.[0]
            let returnType = makeType com ctxTypeArgs t.GenericArguments.[1]
            Fable.FunctionType(Fable.LambdaType argType, returnType)
        elif t.HasTypeDefinition then
// No support for provided types when compiling FCS+Fable to JS
#if !FABLE_COMPILER
            // TODO: Discard provided generated types too?
            if t.TypeDefinition.IsProvidedAndErased then Fable.Any
            else
#endif
                makeTypeFromDef com ctxTypeArgs t.GenericArguments t.TypeDefinition
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    // TODO: This is intended to wrap JS expressions with `| 0`, check enum as well?
    let isSignedIntType (NonAbbreviatedType t) =
        if t.HasTypeDefinition then
            match t.TypeDefinition.TryFullName with
            | Some(Types.int8 | Types.int16 | Types.int32) -> true
            | _ -> false
        else false

    let getBaseClass (tdef: FSharpEntity) =
        match tdef.BaseType with
        | Some(TypeDefinition tdef) when tdef.TryFullName <> Some Types.object ->
            Some tdef
        | _ -> None

    let rec getOwnAndInheritedFsharpMembers (tdef: FSharpEntity) = seq {
        yield! tdef.TryGetMembersFunctionsAndValues
        match tdef.BaseType with
        | Some(TypeDefinition baseDef) when tdef.TryFullName <> Some Types.object ->
            yield! getOwnAndInheritedFsharpMembers baseDef
        | _ -> ()
    }

    let getArgTypes com (memb: FSharpMemberOrFunctionOrValue) =
        // FSharpParameters don't contain the `this` arg
        Seq.concat memb.CurriedParameterGroups
        // The F# compiler "untuples" the args in methods
        |> Seq.map (fun x -> makeType com Map.empty x.Type)
        |> Seq.toList

    let isAbstract (ent: FSharpEntity) =
       tryFindAtt Atts.abstractClass ent.Attributes |> Option.isSome

    let tryGetInterfaceTypeFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0
        then nonAbbreviatedType meth.ImplementedAbstractSignatures.[0].DeclaringType |> Some
        else None

    let tryGetInterfaceDefinitionFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.ImplementedAbstractSignatures.Count > 0 then
            let t = nonAbbreviatedType meth.ImplementedAbstractSignatures.[0].DeclaringType
            if t.HasTypeDefinition then Some t.TypeDefinition else None
        else None

    let tryFindMember com (entity: FSharpEntity) genArgs membCompiledName isInstance (argTypes: Fable.Type list) =
        let argsEqual (args1: Fable.Type list) args1Length (args2: IList<IList<FSharpParameter>>) =
                let args2Length = args2 |> Seq.sumBy (fun g -> g.Count)
                if args1Length = args2Length then
                    let args2 = args2 |> Seq.collect (fun g ->
                        g |> Seq.map (fun p -> makeType com genArgs p.Type) |> Seq.toList)
                    listEquals (typeEquals false) args1 (Seq.toList args2)
                else false
        let argTypesLength = List.length argTypes
        getOwnAndInheritedFsharpMembers entity |> Seq.tryFind (fun m2 ->
            if m2.IsInstanceMember = isInstance && m2.CompiledName = membCompiledName
            then argsEqual argTypes argTypesLength m2.CurriedParameterGroups
            else false)

    let inline (|FableType|) com (ctx: Context) t = makeType com ctx.GenericArgs t

module Identifiers =
    open Helpers
    open TypeHelpers

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with Scope = (fsRef, expr)::ctx.Scope}

    let makeIdentFrom (com: IFableCompiler) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue): Fable.Ident =
        let sanitizedName = (fsRef.CompiledName, Naming.NoMemberPart)
                            ||> Naming.sanitizeIdent com.IsUsedVarName
        // Track all used var names in the file so they're not used for imports
        // Also, in some situations variable names in different scopes can conflict
        // so just try to give a unique name to each identifier per file for safety
        com.AddUsedVarName sanitizedName
        { Name = sanitizedName
          Type = makeType com ctx.GenericArgs fsRef.FullType
          Kind = Fable.UnspecifiedIdent
          IsMutable = fsRef.IsMutable
          IsCompilerGenerated = fsRef.IsCompilerGenerated
          Range = { makeRange fsRef.DeclarationLocation
                    with identifierName = Some fsRef.DisplayName } |> Some }

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        let ident = makeIdentFrom com ctx fsRef
        bindExpr ctx fsRef (Fable.IdentExpr ident), ident

    let (|BindIdent|) com ctx fsRef = bindIdentFrom com ctx fsRef

    let inline tryGetBoundExprWhere (ctx: Context) r predicate =
        match List.tryFind (fun (fsRef,_)  -> predicate fsRef) ctx.Scope with
        | Some(_, Fable.IdentExpr ident) ->
            let originalName = ident.Range |> Option.bind (fun r -> r.identifierName)
            { ident with Range = r |> Option.map (fun r -> { r with identifierName = originalName }) }
            |> Fable.IdentExpr |> Some
        | Some(_, boundExpr) -> Some boundExpr
        | None -> None

    /// Get corresponding identifier to F# value in current scope
    let tryGetBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        tryGetBoundExprWhere ctx r (fun fsRef' -> obj.Equals(fsRef, fsRef'))

module Util =
    open Helpers
    open Patterns
    open TypeHelpers
    open Identifiers

    let makeFunctionArgs com ctx (args: FSharpMemberOrFunctionOrValue list) =
        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = bindIdentFrom com ctx var
                newContext, arg::accArgs)
        ctx, List.rev args

    let bindMemberArgs com ctx (args: FSharpMemberOrFunctionOrValue list list) =
        let ctx, transformedArgs, args =
            match args with
            // Within private members (first arg is ConstructorThisValue) F# AST uses
            // ThisValue instead of Value (with .IsMemberConstructorThisValue = true)
            | (firstArg::restArgs1)::restArgs2 when firstArg.IsConstructorThisValue || firstArg.IsMemberThisValue ->
                let ctx, thisArg = bindIdentFrom com ctx firstArg
                let thisArg = { thisArg with Kind = Fable.ThisArgIdentDeclaration }
                let ctx =
                    if firstArg.IsConstructorThisValue
                    then { ctx with BoundConstructorThis = Some thisArg }
                    else { ctx with BoundMemberThis = Some thisArg }
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
            | Some (BindIdent com ctx (catchContext, catchVar), catchBody) ->
                // Add caughtException to context so it can be retrieved by `reraise`
                let catchContext = { catchContext with CaughtException = Some catchVar }
                Some (catchVar, com.Transform(catchContext, catchBody))
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch(body, catchClause, finalizer, r)

    let matchGenericParams (genArgs: Fable.Type seq) (genParams: FSharpGenericParameter seq) =
        Seq.zip (genParams |> Seq.map (fun x -> x.Name)) genArgs

    let matchGenericParamsFrom (memb: FSharpMemberOrFunctionOrValue) (genArgs: Fable.Type seq) =
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

    /// Same as `countNonCurriedParams` but applied to overrides
    let countNonCurriedParamsForOverride (over: FSharpObjectExprOverride) =
        let args = over.CurriedParameterGroups
        match args with
        | [] | [_] -> 0
        // Unlike FSharpMemberOrFunctionOrValue.CurriedParameterGroups
        // overrides DO include the name self instance as parameter
        | _thisArg::firsGroup::_ ->
            match firsGroup with
            | [arg] ->
                match arg.FullTypeSafe with
                | Some t when isUnit t -> 0
                | _ -> 1
            | args -> List.length args

    // When importing a relative path from a different path where the member,
    // entity... is declared, we need to resolve the path
    let fixImportedRelativePath (com: ICompiler) (path: string) (loc: Lazy<Range.range>) =
        if Path.isRelativePath path then
            let file = Path.normalizePathAndEnsureFsExtension loc.Value.FileName
            if file = com.CurrentFile
            then path
            else
                Path.Combine(Path.GetDirectoryName(file), path)
                |> Path.getRelativePath com.CurrentFile
        else path

    let (|ImportAtt|EmitDeclarationAtt|NoAtt|) (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (function
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)]) ->
                Choice1Of3(selector.Trim(), path.Trim()) |> Some
            | AttFullName(Atts.importAll, AttArguments [(:? string as path)]) ->
                Choice1Of3("*", path.Trim()) |> Some
            | AttFullName(Atts.importDefault, AttArguments [(:? string as path)]) ->
                Choice1Of3("default", path.Trim()) |> Some
            | AttFullName(Atts.importMember, AttArguments [(:? string as path)]) ->
                Choice1Of3(Naming.placeholder, path.Trim()) |> Some
            | AttFullName(Atts.emitDeclaration, AttArguments [(:? string as macro)]) ->
                Choice2Of3(macro) |> Some
            | _ -> None)
        |> Option.defaultValue (Choice3Of3 ())

    /// Function used to check if calls must be replaced by global idents or direct imports
    let tryGlobalOrImportedMember com typ (memb: FSharpMemberOrFunctionOrValue) =
        let getImportPath path =
            lazy getMemberLocation memb
            |> fixImportedRelativePath com path
        memb.Attributes |> Seq.tryPick (function
            | AttFullName(Atts.global_, att) ->
                match att with
                | AttArguments [:? string as customName] ->
                    makeTypedIdentNonMangled typ customName |> Fable.IdentExpr |> Some
                | _ -> getMemberDisplayName memb |> makeTypedIdentNonMangled typ |> Fable.IdentExpr |> Some
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)]) ->
                getImportPath path |> makeCustomImport typ selector |> Some
            | AttFullName(Atts.importAll, AttArguments [(:? string as path)]) ->
                getImportPath path |> makeCustomImport typ "*" |> Some
            | AttFullName(Atts.importDefault, AttArguments [(:? string as path)]) ->
                getImportPath path |> makeCustomImport typ "default" |> Some
            | AttFullName(Atts.importMember, AttArguments [(:? string as path)]) ->
                let selector = getMemberDisplayName memb
                getImportPath path |> makeCustomImport typ selector |> Some
            | _ -> None)

    let tryGlobalOrImportedEntity (com: ICompiler) (ent: FSharpEntity) =
        let getImportPath path =
            lazy getEntityLocation ent
            |> fixImportedRelativePath com path
        ent.Attributes |> Seq.tryPick (function
            | AttFullName(Atts.global_, att) ->
                match att with
                | AttArguments [:? string as customName] ->
                    makeTypedIdentNonMangled Fable.Any customName |> Fable.IdentExpr |> Some
                | _ -> ent.DisplayName |> makeTypedIdentNonMangled Fable.Any |> Fable.IdentExpr |> Some
            | AttFullName(Atts.import, AttArguments [(:? string as selector); (:? string as path)]) ->
                getImportPath path |> makeCustomImport Fable.Any selector |> Some
            | AttFullName(Atts.importAll, AttArguments [(:? string as path)]) ->
                getImportPath path |> makeCustomImport Fable.Any "*" |> Some
            | AttFullName(Atts.importDefault, AttArguments [(:? string as path)]) ->
                getImportPath path |> makeCustomImport Fable.Any "default" |> Some
            | AttFullName(Atts.importMember, AttArguments [(:? string as path)]) ->
                getImportPath path |> makeCustomImport Fable.Any ent.DisplayName |> Some
            | _ -> None)

    let isErasedEntity (ent: FSharpEntity) =
        ent.Attributes |> Seq.exists (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Atts.erase | Atts.stringEnum | Atts.global_
                    | Atts.import | Atts.importAll | Atts.importDefault | Atts.importMember) -> true
            | _ -> false)

    /// Entities coming from assemblies (we don't have access to source code) are candidates for replacement
    let isReplacementCandidate (ent: FSharpEntity) =
        match ent.Assembly.FileName, ent.TryFullName with
        | Some asmPath, _ -> not(System.String.IsNullOrEmpty(asmPath))
        // When compiling Fable itself, Fable.Core entities will be part of the code base,
        // but still need to be replaced
        | None, Some entityFullName -> entityFullName.StartsWith("Fable.Core.")
        | None, None -> false

    /// We can add a suffix to the entity name for special methods, like reflection declaration
    let entityRefWithSuffix (com: ICompiler) (ent: FSharpEntity) suffix =
        if ent.IsInterface then
            defaultArg ent.TryFullName ent.CompiledName
            |> sprintf "Cannot reference an interface %s"
            |> addErrorAndReturnNull com [] None
        else
            let entLoc = getEntityLocation ent
            let file = Path.normalizePathAndEnsureFsExtension entLoc.FileName
            let entityName = getEntityDeclarationName com ent
            let entityName = Naming.appendSuffix entityName suffix
            if file = com.CurrentFile
            then makeIdentExprNonMangled entityName
            else makeInternalImport Fable.Any entityName file

    let entityRef (com: ICompiler) (ent: FSharpEntity) =
        entityRefWithSuffix com ent ""

    /// First checks if the entity is global or imported
    let entityRefMaybeGlobalOrImported (com: ICompiler) (ent: FSharpEntity) =
        match tryGlobalOrImportedEntity com ent with
        | Some importedEntity -> importedEntity
        | None -> entityRef com ent

    let private memberRefPrivate (com: IFableCompiler) r typ (entity: FSharpEntity option) memberName =
        let file =
            match entity with
            | Some ent ->
                let entLoc = getEntityLocation ent
                Path.normalizePathAndEnsureFsExtension entLoc.FileName
            // Cases when .DeclaringEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            | None -> com.CurrentFile
        if file = com.CurrentFile then
            { makeTypedIdentNonMangled typ memberName with Range = r }
            |> Fable.IdentExpr
        else makeInternalImport typ memberName file

    let memberRefTyped (com: IFableCompiler) r typ (memb: FSharpMemberOrFunctionOrValue) =
        let r = r |> Option.map (fun r -> { r with identifierName = Some memb.DisplayName })
        getMemberDeclarationName com memb
        |> memberRefPrivate com r typ memb.DeclaringEntity

    let memberRef (com: IFableCompiler) r (memb: FSharpMemberOrFunctionOrValue) =
        memberRefTyped com r Fable.Any memb

    /// Checks who's the actual implementor of the interface, this entity or any of its parents
    let rec tryFindImplementingEntity (ent: FSharpEntity) interfaceFullName =
        ent.DeclaredInterfaces
        |> Seq.exists (testInterfaceHierarcy interfaceFullName)
        |> function
            | true -> Some ent
            | false ->
                match ent.BaseType with
                | Some(NonAbbreviatedType t) when t.HasTypeDefinition ->
                    tryFindImplementingEntity t.TypeDefinition interfaceFullName
                | _ -> None

    let callInstanceMember com r typ (argInfo: Fable.ArgInfo) (memb: FSharpMemberOrFunctionOrValue) =
        let callee =
            match argInfo.ThisArg with
            | Some callee -> callee
            | None ->
                sprintf "Unexpected static interface/override call: %s" memb.FullName
                |> attachRange r |> failwith
        let name = getMemberDisplayName memb
        match argInfo.Args with
        | [arg] when memb.IsPropertySetterMethod ->
            let t = memb.CurriedParameterGroups.[0].[0].Type |> makeType com Map.empty
            Fable.Set(callee, Fable.FieldSet(name, t), arg, r)
        | _ when memb.IsPropertyGetterMethod && countNonCurriedParams memb = 0 ->
            let t = memb.ReturnParameter.Type |> makeType com Map.empty
            let kind = Fable.FieldGet(name, true, t)
            Fable.Get(callee, kind, typ, r)
        | _ ->
            let argInfo = { argInfo with ThisArg = Some callee }
            makeStrConst name |> Some |> instanceCall r typ argInfo

    let (|Replaced|_|) (com: IFableCompiler) ctx r typ argTypes (genArgs: Lazy<_>) (argInfo: Fable.ArgInfo) isModuleValue
            (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        match entity with
        | Some ent when isReplacementCandidate ent ->
            let info: Fable.ReplaceCallInfo =
              { SignatureArgTypes = argTypes
                DeclaringEntityFullName = ent.FullName
                Spread = argInfo.Spread
                IsModuleValue = isModuleValue
                // TODO: We should also check for
                // memb.IsOverrideOrExplicitInterfaceImplementation
                // memb.IsDispatchSlot
                IsInterface = ent.IsInterface
                CompiledName = memb.CompiledName
                OverloadSuffix = lazy if ent.IsFSharpModule then "" else OverloadSuffix.getHash ent memb
                GenericArgs = genArgs.Value }
            match com.TryReplace(ctx, r, typ, info, argInfo.ThisArg, argInfo.Args) with
            | Some e -> Some e
            | None when info.IsInterface ->
                callInstanceMember com r typ argInfo memb |> Some
            | None ->
                sprintf "Cannot resolve %s.%s" info.DeclaringEntityFullName info.CompiledName
                |> addErrorAndReturnNull com ctx.InlinePath r |> Some
        | _ -> None

    let (|Emitted|_|) com r typ argInfo (memb: FSharpMemberOrFunctionOrValue) =
        memb.Attributes |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Naming.StartsWith "Fable.Core.Emit" _ as attFullName) ->
                let argInfo =
                    // Allow combination of Import and Emit attributes
                    match argInfo, tryGlobalOrImportedMember com Fable.Any memb with
                    | Some argInfo, Some importExpr ->
                        Some { argInfo with Fable.ThisArg = Some importExpr }
                    | _ -> argInfo
                let macro =
                    match Seq.tryHead att.ConstructorArguments with
                    | Some(_, (:? string as macro)) -> macro
                    | _ -> ""
                match attFullName with
                | Atts.emit -> Some macro
                | Atts.emitMethod -> "$0." + macro + "($1...)" |> Some
                | Atts.emitConstructor -> "new $0($1...)" |> Some
                | Atts.emitIndexer -> "$0[$1]{{=$2}}" |> Some
                | Atts.emitProperty -> "$0." + macro + "{{=$1}}" |> Some
                | _ -> None
                |> Option.map (fun macro -> Fable.Operation(Fable.Emit(macro, argInfo), typ, r))
            | _ -> None
        )

    let (|Imported|_|) com r typ argInfo isModuleValue (memb: FSharpMemberOrFunctionOrValue, entity: FSharpEntity option) =
        let importValueType = if Option.isSome argInfo then Fable.Any else typ
        match tryGlobalOrImportedMember com importValueType memb, argInfo, entity with
        | Some importExpr, Some argInfo, Some e ->
            if (e.IsFSharpModule && isModuleValueForCalls memb)
                || (memb.IsPropertyGetterMethod && (countNonCurriedParams memb) = 0)
            then Some importExpr
            else staticCall r typ argInfo importExpr |> Some
        | Some importExpr, None, _ ->
            Some importExpr
        | None, Some argInfo, Some e ->
            match tryGlobalOrImportedEntity com e, argInfo.IsBaseOrSelfConstructorCall, argInfo.ThisArg with
            | Some classExpr, true, _ ->
                staticCall r typ argInfo classExpr |> Some
            | Some _, false, Some _thisArg ->
                callInstanceMember com r typ argInfo memb |> Some
            | Some classExpr, false, None ->
                if memb.IsConstructor then
                    Fable.Operation(Fable.Call(Fable.ConstructorCall classExpr, argInfo), typ, r) |> Some
                elif isModuleValue then
                    let kind = Fable.FieldGet(getMemberDisplayName memb, true, Fable.Any)
                    Fable.Get(classExpr, kind, typ, r) |> Some
                else
                    let argInfo = { argInfo with ThisArg = Some classExpr }
                    callInstanceMember com r typ argInfo memb |> Some
            | None, _, _ -> None
        | _ -> None

    let inlineExpr (com: IFableCompiler) (ctx: Context) r (genArgs: Lazy<_>) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let rec foldArgs acc = function
            | argIdent::restArgIdents, argExpr::restArgExprs ->
                foldArgs ((argIdent, argExpr)::acc) (restArgIdents, restArgExprs)
            | (argIdent: FSharpMemberOrFunctionOrValue)::restArgIdents, [] ->
                let t = makeType com ctx.GenericArgs argIdent.FullType
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
                    // Change type and mark ident as compiler-generated so it can be optimized
                    let ident = { makeIdentFrom com ctx argId with
                                    Type = arg.Type
                                    IsCompilerGenerated = true }
                    let ctx = bindExpr ctx argId (Fable.IdentExpr ident)
                    ctx, (ident, arg)::bindings)
            let ctx = { ctx with GenericArgs = genArgs.Value |> Map
                                 InlinedFunction = Some memb
                                 InlinePath = (inExpr.FileName, r)::ctx.InlinePath }
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

    let hasAttribute attFullName (attributes: IList<FSharpAttribute>) =
        let mutable found = false
        let attFullName = Some attFullName
        for att in attributes do
            found <- found || att.AttributeType.TryFullName = attFullName
        found

    let hasInterface interfaceFullname (ent: FSharpEntity) =
        let mutable found = false
        let interfaceFullname = Some interfaceFullname
        for t in ent.AllInterfaces do
            found <- found || t.HasTypeDefinition && t.TypeDefinition.TryFullName = interfaceFullname
        found

    let hasImplicitConstructor (ent: FSharpEntity) =
        let mutable found = false
        for m in ent.MembersFunctionsAndValues do
            found <- found || m.IsImplicitConstructor
        found

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ isBaseCall (genArgs: Fable.Type seq) callee args (memb: FSharpMemberOrFunctionOrValue) =
        let genArgs = lazy(matchGenericParamsFrom memb genArgs |> Seq.toList)
        let args = transformOptionalArguments com ctx r memb genArgs args
        let argTypes = getArgTypes com memb
        let isModuleValue = isModuleValueForCalls memb
        let argInfo: Fable.ArgInfo =
          { ThisArg = callee
            Args = args
            SignatureArgTypes = Fable.Typed argTypes
            Spread = if hasSeqSpread memb then Fable.SeqSpread else Fable.NoSpread
            IsBaseOrSelfConstructorCall = isBaseCall
          }
        match memb, memb.DeclaringEntity with
        | Emitted com r typ (Some argInfo) emitted, _ -> emitted
        | Imported com r typ (Some argInfo) isModuleValue imported -> imported
        | Replaced com ctx r typ argTypes genArgs argInfo isModuleValue replaced -> replaced
        | Inlined com ctx r genArgs callee args expr, _ -> expr
        | Try (tryGetBoundExpr ctx r) funcExpr, _ ->
            if isModuleValue
            then funcExpr
            else staticCall r typ argInfo funcExpr
        // Check if this is an interface or abstract/overriden method
        | _, Some entity when entity.IsInterface
                || memb.IsOverrideOrExplicitInterfaceImplementation
                || memb.IsDispatchSlot ->
            callInstanceMember com r typ argInfo memb
        | _ ->
            if isModuleValue
            then memberRefTyped com r typ memb
            else
                let argInfo =
                    if not argInfo.IsBaseOrSelfConstructorCall && isSelfConstructorCall ctx memb
                    then { argInfo with IsBaseOrSelfConstructorCall = true }
                    else argInfo
                memberRef com r memb |> staticCall r typ argInfo

    let makeValueFrom (com: IFableCompiler) (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType com ctx.GenericArgs v.FullType
        match v, v.DeclaringEntity with
        | _ when typ = Fable.Unit ->
            if com.Options.verbose && not v.IsCompilerGenerated then // See #1516
                sprintf "Value %s is replaced with unit constant" v.DisplayName
                |> addWarning com ctx.InlinePath r
            Fable.Value(Fable.UnitConstant, r)
        | Emitted com r typ None emitted, _ -> emitted
        | Imported com r typ None true imported -> imported
        // TODO: Replaced? Check if there're failing tests
        | Try (tryGetBoundExpr ctx r) expr, _ -> expr
        | _ -> memberRefTyped com r typ v
