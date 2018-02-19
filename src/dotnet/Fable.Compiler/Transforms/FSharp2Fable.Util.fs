namespace Fable.Transforms.FSharp2Fable

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.AST.Fable.Util
open Fable.Transforms

type Context =
    { fileName: string
      scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
      scopedInlines: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
      /// Some expressions that create scope in F# don't do it in JS (like let bindings)
      /// so we need a mutable registry to prevent duplicated var names.
      varNames: HashSet<string>
      typeArgs: (string * FSharpType) list
      decisionTargets: Map<int, FSharpMemberOrFunctionOrValue list * FSharpExpr> option
    }
    static member Create(fileName) =
        { fileName = fileName
          scope = []
          scopedInlines = []
          varNames = HashSet()
          typeArgs = []
          decisionTargets = None }

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract TryGetInternalFile: FSharpEntity -> string option
    abstract GetRootModule: string -> string
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> InlineExpr
    abstract AddInlineExpr: FSharpMemberOrFunctionOrValue * InlineExpr -> unit
    abstract AddUsedVarName: string -> unit

module Helpers =
    let tryBoth (f1: 'a->'b option) (f2: 'a->'b option) (x: 'a) =
        match f1 x with
        | Some _ as res -> res
        | None ->
            match f2 x with
            | Some _ as res -> res
            | None -> None

    let rec nonAbbreviatedType (t: FSharpType) =
        if t.IsAbbreviation then nonAbbreviatedType t.AbbreviatedType else t

    let rec nonAbbreviatedEntity (ent: FSharpEntity) =
        if ent.IsFSharpAbbreviation
        then (nonAbbreviatedType ent.AbbreviatedType).TypeDefinition
        else ent

    let getEntityName (ent: FSharpEntity) =
        ent.CompiledName.Replace('`', '_')

    // TODO: Report bug in FCS repo, when ent.IsNamespace, FullName doesn't work.
    let getEntityFullName (ent: FSharpEntity) =
        if ent.IsNamespace
        then match ent.Namespace with Some ns -> ns + "." + ent.CompiledName | None -> ent.CompiledName
        else defaultArg ent.TryFullName ent.CompiledName

    let getEntityLocation (ent: FSharpEntity) =
        match ent.ImplementationLocation with
        | Some loc -> loc
        | None -> ent.DeclarationLocation

    let getMemberLocation (memb: FSharpMemberOrFunctionOrValue) =
        match memb.ImplementationLocation with
        | Some loc -> loc
        | None -> memb.DeclarationLocation

    let private getMemberDeclarationNamePrivate removeRootModule (com: IFableCompiler) (argTypes: Fable.Type list) (memb: FSharpMemberOrFunctionOrValue) =
        let removeRootModule' (ent: FSharpEntity) (fullName: string) =
            if not removeRootModule then
                fullName
            else
                let rootMod =
                    (getEntityLocation ent).FileName
                    |> Path.normalizePath
                    |> com.GetRootModule
                // TODO: Do we need to trim '+' too?
                fullName.Replace(rootMod, ".").TrimStart('.')
        match memb.EnclosingEntity with
        | Some ent when ent.IsFSharpModule ->
            removeRootModule' ent memb.FullName
        | Some ent ->
            let isStatic = not memb.IsInstanceMember
            let separator = if isStatic then "$$" else "$"
            // TODO: Overloads
            //let fableEnt = com.GetEntity(ent)
            //let overloadName =
            //    match fableEnt.TryGetMember(memb.CompiledName, isStatic, argTypes) with
            //    | Some m -> m.OverloadName
            //    | None -> memb.CompiledName
            //ent.CompiledName + separator + overloadName
            (removeRootModule' ent ent.FullName) + separator + memb.CompiledName
        | None -> memb.FullName
        |> Naming.sanitizeIdentForbiddenChars

    let getMemberDeclarationName (com: IFableCompiler) (argTypes: Fable.Type list) (memb: FSharpMemberOrFunctionOrValue) =
        getMemberDeclarationNamePrivate true com argTypes memb

    let getMemberDeclarationFullname (com: IFableCompiler) (argTypes: Fable.Type list) (memb: FSharpMemberOrFunctionOrValue) =

        getMemberDeclarationNamePrivate false com argTypes memb

    let tryFindAtt f (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match (nonAbbreviatedEntity att.AttributeType).TryFullName with
            | Some fullName ->
                if f fullName then Some att else None
            | None -> None)

    let hasAtt name atts =
        atts |> tryFindAtt ((=) name) |> Option.isSome

    let tryDefinition (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition then Some typ.TypeDefinition else None

    let getFsTypeFullName (typ: FSharpType) =
        match tryDefinition typ with
        | Some tdef -> defaultArg tdef.TryFullName "unknown"
        | None -> "unknown"

    let isInline (memb: FSharpMemberOrFunctionOrValue) =
        match memb.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline -> true
        | FSharpInlineAnnotation.AggressiveInline -> failwith "Not Implemented"

    let isPublicMember (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsCompilerGenerated
        then false
        else not memb.Accessibility.IsPrivate

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Core.Unit"
        else false

    let makeRange (r: Range.range) = {
        start = { line = r.StartLine; column = r.StartColumn }
        ``end``= { line = r.EndLine; column = r.EndColumn }
    }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    let getUnionIndex fsType (unionCase: FSharpUnionCase) =
        let unionCaseName = unionCase.Name
        match tryDefinition fsType with
        | None -> failwithf "Cannot find Type definition for union case %s" unionCaseName
        | Some tdef ->
            tdef.UnionCases
            |> Seq.findIndex (fun uc -> uc.Name = unionCaseName)
            |> makeIntConst

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt ((=) Atts.compiledName)
        |> function
            | Some name -> name.ConstructorArguments.[0] |> snd |> string
            | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeStrConst

    let getArgCount (memb: FSharpMemberOrFunctionOrValue) =
        let args = memb.CurriedParameterGroups
        if args.Count = 0 then 0
        elif args.Count = 1 && args.[0].Count = 1 then
            if isUnit args.[0].[0].Type then 0 else 1
        else args |> Seq.sumBy (fun li -> li.Count)

    let private isModuleValuePrivate checkPublicMutable (memb: FSharpMemberOrFunctionOrValue) =
        match memb.EnclosingEntity with
        | Some owner when owner.IsFSharpModule ->
            let preCondition =
                if checkPublicMutable
                then not memb.IsMutable || not (isPublicMember memb)
                else true
            preCondition
            && memb.CurriedParameterGroups.Count = 0
            && memb.GenericParameters.Count = 0
        | _ -> false

    // Mutable public values must be compiled as functions (see #986)
    let isModuleValueForCalls (memb: FSharpMemberOrFunctionOrValue) =
        isModuleValuePrivate true memb

    let isModuleValueForDeclaration (memb: FSharpMemberOrFunctionOrValue) =
        isModuleValuePrivate false memb

    let getObjectMemberKind (memb: FSharpMemberOrFunctionOrValue) =
        if memb.IsImplicitConstructor || memb.IsConstructor then Fable.Constructor
        elif memb.IsPropertyGetterMethod && (getArgCount memb) = 0 then Fable.Getter
        elif memb.IsPropertySetterMethod && (getArgCount memb) = 1 then Fable.Setter
        else Fable.Method

    let hasSpread (memb: FSharpMemberOrFunctionOrValue) =
        let hasParamArray (memb: FSharpMemberOrFunctionOrValue) =
            if memb.CurriedParameterGroups.Count <> 1 then false else
            let args = memb.CurriedParameterGroups.[0]
            args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

        let hasParamSeq (memb: FSharpMemberOrFunctionOrValue) =
            Seq.tryLast memb.CurriedParameterGroups
            |> Option.bind Seq.tryLast
            |> Option.map (fun lastParam -> hasAtt Atts.paramSeq lastParam.Attributes)
            |> Option.defaultValue false

        hasParamArray memb || hasParamSeq memb

module Patterns =
    open BasicPatterns
    open Helpers

    let inline (|Rev|) x = List.rev x
    let inline (|AsArray|) x = Array.ofSeq x
    let inline (|LazyValue|) (x: Lazy<'T>) = x.Value
    let inline (|Transform|) (com: IFableCompiler) ctx = com.Transform ctx
    let inline (|FieldName|) (fi: FSharpField) = fi.Name

    let inline (|NonAbbreviatedType|) (t: FSharpType) =
        nonAbbreviatedType t

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then Some t.TypeDefinition else None

    let (|RefType|_|) = function
        | NonAbbreviatedType(TypeDefinition tdef) as t
            when tdef.TryFullName = Some "Microsoft.FSharp.Core.FSharpRef`1" -> Some t
        | _ -> None

    let (|ThisVar|_|) = function
        | BasicPatterns.ThisValue _ -> Some ThisVar
        | BasicPatterns.Value var when
            var.IsMemberThisValue || var.IsConstructorThisValue ->
            Some ThisVar
        | _ -> None

    let (|ForOfLoop|_|) = function
        | Let((_, value),
              Let((_, Call(None, memb, _, [], [])),
                TryFinally(WhileLoop(_,Let((ident, _), body)), _)))
        | Let((_, Call(Some value, memb, _, [], [])),
                TryFinally(WhileLoop(_,Let((ident, _), body)), _))
            when memb.CompiledName = "GetEnumerator" ->
            Some(ident, value, body)
        | _ -> None

    let (|PrintFormat|_|) fsExpr =
        match fsExpr with
        | Let((v,(Call(None,_,_,_,args) as e)),_) when v.IsCompilerGenerated ->
            match List.tryLast args with
            | Some arg ->
                if arg.Type.HasTypeDefinition
                    && arg.Type.TypeDefinition.AccessPath = "Microsoft.FSharp.Core.PrintfModule"
                then Some e
                else None
            | None -> None
        | _ -> None

    /// This matches the boilerplate F# compiler generates for methods
    /// like Dictionary.TryGetValue (see #154)
    let (|TryGetValue|_|) = function
        | Let((outArg1, (DefaultValue _ as def)),
                NewTuple(_, [Call(callee, memb, typArgs, methTypArgs,
                                    [arg; AddressOf(Value outArg2)]); Value outArg3]))
            when outArg1 = outArg2 && outArg1 = outArg3 ->
            Some (callee, memb, typArgs, methTypArgs, [arg; def])
        | _ -> None

    /// This matches the boilerplate generated to wrap .NET events from F#
    let (|CreateEvent|_|) = function
        | Call(Some(Call(None, createEvent,_,_,
                        [Lambda(_eventDelegate, Call(Some callee, addEvent,[],[],[Value _eventDelegate']));
                         Lambda(_eventDelegate2, Call(Some _callee2, _removeEvent,[],[],[Value _eventDelegate2']));
                         Lambda(_callback, NewDelegate(_, Lambda(_delegateArg0, Lambda(_delegateArg1, Application(Value _callback',[],[Value _delegateArg0'; Value _delegateArg1'])))))])),
                memb, typArgs, methTypArgs, args)
                when createEvent.FullName = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent" ->
            let eventName = addEvent.CompiledName.Replace("add_","")
            Some (callee, eventName, memb, typArgs, methTypArgs, args)
        | _ -> None

    /// This matches the boilerplate generated to check an array's length
    /// when pattern matching
    let (|CheckArrayLength|_|) = function
        | IfThenElse
            (ILAsm ("[AI_ldnull; AI_cgt_un]",[],[matchValue]),
             Call(None,_op_Equality,[],[_typeInt],
                [ILAsm ("[I_ldlen; AI_conv DT_I4]",[],[_matchValue2])
                 Const (length,_typeInt2)]),
             Const (_falseConst,_typeBool)) -> Some (matchValue, length, _typeInt2)
        | _ -> None

    let (|NumberKind|_|) = function
        | "System.SByte" -> Some Int8
        | "System.Byte" -> Some UInt8
        | "System.Int16" -> Some Int16
        | "System.UInt16" -> Some UInt16
        | "System.Int32" -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Single" -> Some Float32
        | "System.Double" -> Some Float64
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.sbyte" _ -> Some Int8
        | Naming.StartsWith "Microsoft.FSharp.Core.int16" _ -> Some Int16
        | Naming.StartsWith "Microsoft.FSharp.Core.int" _ -> Some Int32
        | Naming.StartsWith "Microsoft.FSharp.Core.float32" _ -> Some Float32
        | Naming.StartsWith "Microsoft.FSharp.Core.float" _ -> Some Float64
        | _ -> None

    let (|ExtendedNumberKind|_|) = function
        | "System.Int64" -> Some Int64
        | "System.UInt64" -> Some UInt64
        | "System.Decimal" -> Some Decimal
        | "System.Numerics.BigInteger" -> Some BigInt
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.int64" _ -> Some Int64
        | Naming.StartsWith "Microsoft.FSharp.Core.decimal" _ -> Some Decimal
        | _ -> None

    let (|OptionUnion|ListUnion|ErasedUnion|StringEnum|DiscriminatedUnion|) (NonAbbreviatedType typ: FSharpType) =
        match tryDefinition typ with
        | None -> failwith "Union without definition"
        | Some tdef ->
            match defaultArg tdef.TryFullName tdef.CompiledName with
            | "Microsoft.FSharp.Core.FSharpOption`1" -> OptionUnion typ.GenericArguments.[0]
            | "Microsoft.FSharp.Collections.FSharpList`1" -> ListUnion typ.GenericArguments.[0]
            | _ ->
                tdef.Attributes |> Seq.tryPick (fun att ->
                    match (nonAbbreviatedEntity att.AttributeType).TryFullName with
                    | Some Atts.erase -> Some ErasedUnion
                    | Some Atts.stringEnum -> Some StringEnum
                    | _ -> None)
                |> Option.defaultValue (DiscriminatedUnion tdef)

    let (|Switch|_|) fsExpr =
        let isStringOrNumber (NonAbbreviatedType typ) =
            if not typ.HasTypeDefinition then false else
            match typ.TypeDefinition.TryFullName with
            | Some("System.String") -> true
            | Some(NumberKind _) -> true
            | _ when typ.TypeDefinition.IsEnum -> true
            | _ -> false
        let rec makeSwitch size map matchValue e =
            match e with
            | IfThenElse(Call(None,op_Equality,[],_,[Value var; Const(case,_)]), DecisionTreeSuccess(idx, bindings), elseExpr)
                    when op_Equality.CompiledName.Equals("op_Equality") ->
                let case =
                    match case with
                    | :? int as i -> makeIntConst i |> Some
                    | :? string as s -> makeStrConst s |> Some
                    | _ -> None
                match case, matchValue with
                | Some case, Some matchValue when matchValue.Equals(var) ->
                    Some(matchValue,false,idx,bindings,case,elseExpr)
                | Some case, None when isStringOrNumber var.FullType && not var.IsMemberThisValue && not(isInline var) ->
                    Some(var,false,idx,bindings,case,elseExpr)
                | _ -> None
            | IfThenElse(UnionCaseTest(Value var,typ,case), DecisionTreeSuccess(idx, bindings), elseExpr) ->
                let case = Fable.UnionCaseTag(case, typ.TypeDefinition) |> Fable.Value
                match matchValue with
                | Some matchValue when matchValue.Equals(var) ->
                    Some(matchValue,true,idx,bindings,case,elseExpr)
                | None when not var.IsMemberThisValue && not(isInline var) ->
                    match typ with
                    | DiscriminatedUnion _ -> Some(var,true,idx,bindings,case,elseExpr)
                    | OptionUnion _ | ListUnion _ | ErasedUnion | StringEnum -> None
                | _ -> None
            | _ -> None
            |> function
                | Some(matchValue,isUnionType,idx,bindings,case,elseExpr) ->
                    let map =
                        match Map.tryFind idx map with
                        | None -> Map.add idx (bindings, [case]) map |> Some
                        | Some([],cases) when List.isEmpty bindings -> Map.add idx (bindings, cases@[case]) map |> Some
                        | Some _ -> None // Multiple case with multiple var bindings, cannot optimize
                    match map, elseExpr with
                    | Some map, DecisionTreeSuccess(idx, bindings) ->
                        Some(matchValue, isUnionType, size + 1, map, (idx, bindings))
                    | Some map, elseExpr -> makeSwitch (size + 1) map (Some matchValue) elseExpr
                    | None, _ -> None
                | None -> None
        match fsExpr with
        | DecisionTree(decisionExpr, decisionTargets) ->
            match makeSwitch 0 Map.empty None decisionExpr with
            // For small sizes it's better not to convert to switch so
            // the match is still a expression and not a statement
            | Some(matchValue, isUnionType, size, cases, defaultCase) when size > 3 ->
                Some(matchValue, isUnionType, cases, defaultCase, decisionTargets)
            | _ -> None
        | _ -> None

    let (|ContainsAtt|_|) (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> tryFindAtt ((=) name) |> Option.map (fun att ->
            att.ConstructorArguments |> Seq.map snd |> Seq.toList)

module TypeHelpers =
    open Helpers
    open Patterns

    let rec isAttributeEntity (ent: FSharpEntity) =
        match ent.BaseType with
        | Some (NonAbbreviatedType t) when t.HasTypeDefinition ->
            match t.TypeDefinition.TryFullName with
            | Some "System.Attribute" -> true
            | _ -> isAttributeEntity t.TypeDefinition
        | _ -> false

    let rec makeGenArgs com ctxTypeArgs genArgs =
        Seq.map (makeType com ctxTypeArgs) genArgs |> Seq.toList

    and makeTypeFromDef (com: IFableCompiler) ctxTypeArgs (genArgs: seq<FSharpType>) (tdef: FSharpEntity) =
        let getSingleGenericArg genArgs =
            Seq.tryHead genArgs
            |> Option.map (makeType com ctxTypeArgs)
            |> Option.defaultValue Fable.Any // Raise error if not found?
        let tdef = nonAbbreviatedEntity tdef
        let fullName = getEntityFullName tdef
        if tdef.IsArrayType
        then getSingleGenericArg genArgs |> Fable.Array
        elif tdef.IsEnum
        then Fable.EnumType(Fable.NumberEnumType, fullName)
        elif tdef.IsDelegate
        then
            if fullName.StartsWith("System.Action")
            then
                let argTypes =
                    if Seq.length genArgs = 1
                    then [Seq.head genArgs |> makeType com ctxTypeArgs]
                    else [Fable.Unit]
                Fable.FunctionType(Fable.DelegateType argTypes, Fable.Unit)
            elif fullName.StartsWith("System.Func")
            then
                let argTypes, returnType =
                    match Seq.length genArgs with
                    | 0 -> [Fable.Unit], Fable.Unit
                    | 1 -> [Fable.Unit], Seq.head genArgs |> makeType com ctxTypeArgs
                    | c -> Seq.take (c-1) genArgs |> Seq.map (makeType com ctxTypeArgs) |> Seq.toList,
                            Seq.last genArgs |> makeType com ctxTypeArgs
                Fable.FunctionType(Fable.DelegateType argTypes, returnType)
            else
            try
                let argTypes =
                    tdef.FSharpDelegateSignature.DelegateArguments
                    |> Seq.map (snd >> makeType com ctxTypeArgs) |> Seq.toList
                let returnType =
                    makeType com ctxTypeArgs tdef.FSharpDelegateSignature.DelegateReturnType
                Fable.FunctionType(Fable.DelegateType argTypes, returnType)
            with _ ->
                // TODO: Log error here?
                Fable.FunctionType(Fable.DelegateType [Fable.Any], Fable.Any)
        elif fullName = Types.object
        then Fable.Any
        else
        match fullName with
        // Special cases
        | Types.guid -> Fable.String
        | Types.timespan -> Fable.Number Int32
        // Fable "primitives"
        | Types.unit -> Fable.Unit
        | Types.bool -> Fable.Boolean
        | Types.char -> Fable.Char
        | Types.string -> Fable.String
        | Types.regex -> Fable.Regex
        | Types.option -> getSingleGenericArg genArgs |> Fable.Option
        | Types.resizeArray -> getSingleGenericArg genArgs |> Fable.Array
        | Types.list -> getSingleGenericArg genArgs |> Fable.List
        | NumberKind kind -> Fable.Number kind
        | ExtendedNumberKind kind -> Fable.ExtendedNumber kind
        | _ ->
            tdef.Attributes |> Seq.tryPick (fun att ->
                match (nonAbbreviatedEntity att.AttributeType).TryFullName with
                | Some Atts.stringEnum ->
                    Fable.EnumType(Fable.NumberEnumType, fullName) |> Some
                | Some Atts.erase ->
                    makeGenArgs com ctxTypeArgs genArgs
                    |> Fable.ErasedUnion |> Some
                | _ -> None)
            |> Option.defaultWith (fun () ->
                Fable.DeclaredType(tdef, makeGenArgs com ctxTypeArgs genArgs))

    and makeType (com: IFableCompiler) ctxTypeArgs (NonAbbreviatedType t) =
        let resolveGenParam (genParam: FSharpGenericParameter) =
            match ctxTypeArgs |> List.tryFind (fun (name,_) -> name = genParam.Name) with
            // Clear typeArgs to prevent infinite recursion
            | Some (_,typ) -> makeType com [] typ
            | None -> Fable.GenericParam genParam.Name
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
        // Tuple
        elif t.IsTupleType
        then makeGenArgs com ctxTypeArgs t.GenericArguments |> Fable.Tuple
        // Funtion
        elif t.IsFunctionType
        then
            let argType = makeType com ctxTypeArgs t.GenericArguments.[0]
            let returnType = makeType com ctxTypeArgs t.GenericArguments.[1]
            Fable.FunctionType(Fable.LambdaType argType, returnType)
        elif t.HasTypeDefinition
        then makeTypeFromDef com ctxTypeArgs t.GenericArguments t.TypeDefinition
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    let getBaseClass (com: IFableCompiler) (tdef: FSharpEntity) =
        match tdef.BaseType with
        | Some(TypeDefinition tdef) when tdef.TryFullName <> Some Types.object ->
            Some (getEntityFullName tdef)
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
        |> Seq.map (fun x -> makeType com [] x.Type)
        |> Seq.toList

    let isAbstract (ent: FSharpEntity) =
       hasAtt Atts.abstractClass ent.Attributes

    //let getMembers com (tdef: FSharpEntity) =
    //    let addOverloadCount methName (cache: Map<string,int*int>) =
    //        match Map.tryFind methName cache with
    //        | None -> Map.add methName (1, 0) cache
    //        | Some(total, _) -> Map.add methName (total + 1, 0) cache
    //    let getOverloadIndex methName (cache: Map<string,int*int>) =
    //        match Map.tryFind methName cache with
    //        | None | Some(1, _) -> None, cache
    //        | Some(total, idx) -> Some idx, Map.add methName (total, idx + 1) cache
    //    let isDefaultImplementation isOwnerAbstract (x: FSharpMemberOrFunctionOrValue) =
    //        isOwnerAbstract && x.IsOverrideOrExplicitInterfaceImplementation && not x.IsExplicitInterfaceImplementation
    //    let isFakeAbstractMember isOwnerAbstract (x: FSharpMemberOrFunctionOrValue) =
    //        not isOwnerAbstract && not tdef.IsInterface && x.IsDispatchSlot
    //    let members =
    //        let isOwnerAbstract = isAbstract tdef
    //        [ for m in tdef.TryGetMembersFunctionsAndValues do
    //            // Ignore overrides and interface implementations, they won't have overload index
    //            if not m.IsOverrideOrExplicitInterfaceImplementation
    //                // Discard overrides in abstract classes (that is, default implementations)
    //                // to prevent confusing them with overloads (see #505)
    //                && not(isDefaultImplementation isOwnerAbstract m)
    //                // Property members that are no getter nor setter don't actually get implemented
    //                && not(m.IsProperty && not(m.IsPropertyGetterMethod || m.IsPropertySetterMethod))
    //                // F# allows abstract method syntax in non-abstract classes
    //                // if there's a default implementation (see #701)
    //                && not(isFakeAbstractMember isOwnerAbstract m)
    //            then yield m ]
    //    let insMembs, staMembs =
    //        ((Map.empty, Map.empty), members) ||> List.fold (fun (insMembs, staMembs) m ->
    //            if m.IsInstanceMember
    //            then addOverloadCount m.CompiledName insMembs, staMembs
    //            else insMembs, addOverloadCount m.CompiledName staMembs)
    //    (([], insMembs, staMembs), members) ||> List.fold (fun (acc, insMembs, staMembs) m ->
    //        let name = m.CompiledName
    //        let overloadIndex, insMembs, staMembs =
    //            if m.IsInstanceMember then
    //                let overloadIndex, insMembs = getOverloadIndex name insMembs
    //                overloadIndex, insMembs, staMembs
    //            else
    //                let overloadIndex, staMembs = getOverloadIndex name staMembs
    //                overloadIndex, insMembs, staMembs
    //        let argTypes = getArgTypes com m
    //        let returnType = makeType com [] m.ReturnParameter.Type
    //        let m = makeMemberFrom name (getObjectMemberKind m) argTypes returnType overloadIndex m
    //        m::acc, insMembs, staMembs)
    //    |> fun (members, _, _) -> List.rev members

    ///// Don't use this method directly, use IFableCompiler.GetEntity instead
    //let makeEntity (com: IFableCompiler) (tdef: FSharpEntity): Fable.Entity =
        //let makeFields (tdef: FSharpEntity) =
        //    tdef.FSharpFields
        //    |> Seq.map (fun x -> x.Name, makeType com [] x.FieldType)
        //    |> Seq.toList
        //let makeProperties (tdef: FSharpEntity) =
        //    tdef.TryGetMembersFunctionsAndValues
        //    |> Seq.choose (fun x ->
        //        if not x.IsPropertyGetterMethod
        //            || x.IsExplicitInterfaceImplementation
        //        then None
        //        else
        //            match makeType com [] x.FullType with
        //            | Fable.LambdaType(_, returnType) ->
        //                Some(x.DisplayName, returnType)
        //            | _ -> None)
        //    |> Seq.toList
        //let makeCases (tdef: FSharpEntity) =
        //    tdef.UnionCases |> Seq.map (fun uci ->
        //        let name =
        //            uci.Attributes
        //            |> tryFindAtt ((=) Atts.compiledName)
        //            |> function
        //                | Some name -> name.ConstructorArguments.[0] |> snd |> string
        //                | None -> uci.Name
        //        name, [for fi in uci.UnionCaseFields do yield makeType com [] fi.FieldType])
        //    |> Seq.toList
        //let getKind () =
        //    if tdef.IsInterface then Fable.Interface
        //    elif tdef.IsFSharpUnion then makeCases tdef |> Fable.Union
        //    elif tdef.IsFSharpRecord || tdef.IsValueType then makeFields tdef |> Fable.Record
        //    elif tdef.IsFSharpExceptionDeclaration then makeFields tdef |> Fable.Exception
        //    elif tdef.IsFSharpModule || tdef.IsNamespace then Fable.Module
        //    else Fable.Class(getBaseClass com tdef, makeProperties tdef)
        //let genParams =
        //    tdef.GenericParameters |> Seq.map (fun x -> x.Name) |> Seq.toList
        //let infcs =
        //    tdef.DeclaredInterfaces
        //    |> Seq.map (fun x -> getEntityFullName x.TypeDefinition)
        //    |> Seq.filter (Naming.ignoredInterfaces.Contains >> not)
        //    |> Seq.distinct
        //    |> Seq.toList
        //let decs =
        //    tdef.Attributes
        //    |> Seq.choose makeDecorator
        //    |> Seq.toList
        //Fable.Entity (getKind(), com.TryGetInternalFile tdef,
            //getEntityFullName tdef, getMembers com tdef, genParams, infcs, decs)

    let inline (|FableType|) com (ctx: Context) t = makeType com ctx.typeArgs t

module Identifiers =
    open Helpers
    open TypeHelpers

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with scope = (Some fsRef, expr)::ctx.scope}

    let private bindIdentPrivate (com: IFableCompiler) (ctx: Context) typ
                  (fsRef: FSharpMemberOrFunctionOrValue option) force name =
        let sanitizedName = name |> Naming.sanitizeIdent (fun x ->
            not force && ctx.varNames.Contains x)
        ctx.varNames.Add sanitizedName |> ignore
        // Track all used var names in the file so they're not used for imports
        com.AddUsedVarName sanitizedName
        let isMutable, range =
            match fsRef with
            | Some x -> x.IsMutable, Some(makeRange x.DeclarationLocation)
            | None -> false, None
        let ident: Fable.Ident =
            { Name = sanitizedName
              Type = typ
              IsMutable = isMutable
              Range = range }
        let identValue = Fable.IdentExpr ident
        { ctx with scope = (fsRef, identValue)::ctx.scope}, ident

    let bindIdentWithExactName com ctx typ name =
        bindIdentPrivate com ctx typ None true name

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        let typ = makeType com ctx.typeArgs fsRef.FullType
        bindIdentPrivate com ctx typ (Some fsRef) false fsRef.CompiledName

    let bindIdentWithTentativeName com ctx (fsRef: FSharpMemberOrFunctionOrValue) tentativeName: Context*Fable.Ident =
        let typ = makeType com ctx.typeArgs fsRef.FullType
        bindIdentPrivate com ctx typ (Some fsRef) false tentativeName

    let (|BindIdent|) com ctx fsRef = bindIdentFrom com ctx fsRef

    /// Get corresponding identifier to F# value in current scope
    let tryGetBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fst >> function Some fsRef' -> obj.Equals(fsRef, fsRef') | None -> false)
        |> function
            | Some(_, Fable.IdentExpr ident) -> { ident with Range = r } |> Fable.IdentExpr |> Some
            | Some(_, boundExpr) -> Some boundExpr
            | None -> None

module Util =
    open Helpers
    open Patterns
    open TypeHelpers
    open Identifiers

    let countRefs fsExpr (vars: #seq<FSharpMemberOrFunctionOrValue>) =
        let varsDic = Dictionary()
        for var in vars do varsDic.Add(var, 0)
        let rec countRefs = function
            | BasicPatterns.Value v when not v.IsModuleValueOrMember ->
                match varsDic.TryGetValue(v) with
                | true, count -> varsDic.[v] <- count + 1
                | false, _ -> ()
            | expr -> expr.ImmediateSubExpressions |> Seq.iter countRefs
        countRefs fsExpr
        varsDic

    let makeFunctionArgs com ctx (args: FSharpMemberOrFunctionOrValue list) =
        let ctx, args =
            ((ctx, []), args)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = bindIdentFrom com ctx var
                newContext, arg::accArgs)
        ctx, List.rev args

    let bindMemberArgs com ctx (args: FSharpMemberOrFunctionOrValue list list) =
        // To prevent name clashes in JS create a scope for members
        // where variables must always have a unique name
        let ctx = { ctx with varNames = HashSet(ctx.varNames) }
        /// TODO: Remove unit arg if single or with `this` arg
        (args, (ctx, [])) ||> List.foldBack (fun tupledArg (ctx, accArgs) ->
            // The F# compiler "untuples" the args in methods
            let ctx, untupledArg = makeFunctionArgs com ctx tupledArg
            ctx, untupledArg@accArgs)

    let makeTryCatch com ctx (fsExpr: FSharpExpr) (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (BindIdent com ctx (catchContext, catchVar), catchBody) ->
                Some (catchVar, com.Transform catchContext catchBody)
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch (body, catchClause, finalizer)

    // This method doesn't work, the arguments don't keep the attributes
//    let hasRestParams (args: FSharpMemberOrFunctionOrValue list list) =
//        match args with
//        | [args] when args.Length > 0 ->
//            tryFindAtt ((=) "ParamArray") (Seq.last args).Attributes
//            |> Option.isSome
//        | _ -> false

    //let tryReplace (com: IFableCompiler) ctx r typ info =
        //match Replacements.tryReplace com info with
        //| Some _ as repl -> repl
        //| None ->
            //sprintf "Cannot find replacement for %s::%s" info.ownerFullName info.memberName
            //|> addErrorAndReturnNull com ctx.fileName info.range |> Some

    // let (|Emitted|_|) r typ info callee args (memb: FSharpMemberOrFunctionOrValue) =

    let (|Replaced|_|) r typ (info: Fable.CallInfo) (extraInfo: Fable.ExtraCallInfo)
                        callee args (_: FSharpMemberOrFunctionOrValue) =
        if Replacements.isReplaceCandidate extraInfo.fullName then
            match Replacements.tryFirstPass r typ args extraInfo.fullName with
            | Some replaced -> Some replaced
            | None ->
                let unresolved = Fable.UnresolvedCall(callee, args, info, extraInfo)
                Fable.Operation(unresolved, typ, r) |> Some
        else None

    let (|ResolveGeneric|) genArgs (t: FSharpType) =
        if not t.IsGenericParameter then t else
        let genParam = t.GenericParameter
        genArgs |> List.tryPick (fun (name,t) ->
            if genParam.Name = name then Some t else None)
        // TODO: Throw error if generic cannot be resolved?
        |> Option.defaultValue t

    let matchGenericParams ctx (memb: FSharpMemberOrFunctionOrValue) ownerTypeArgs funcTypeArgs =
        ([], memb.GenericParameters, ownerTypeArgs@funcTypeArgs)
        |||> Seq.fold2 (fun acc genPar (ResolveGeneric ctx.typeArgs t) -> (genPar.Name, t)::acc)
        |> List.rev

    let (|Emitted|_|) r typ argsAndCallInfo (memb: FSharpMemberOrFunctionOrValue) =
        match memb.Attributes with
        | ContainsAtt Atts.emit attArgs ->
            match attArgs with
            | [:? string as macro] ->
                Fable.Operation(Fable.Emit(macro, argsAndCallInfo), typ, r) |> Some
            | _ -> "EmitAttribute must receive a string argument" |> attachRange r |> failwith
        | _ -> None

    /// Ignores relative imports (e.g. `[<Import("foo","./lib.js")>]`)
    let tryImported r typ (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            let attType = nonAbbreviatedEntity att.AttributeType
            match attType.TryFullName with
            | Some "Global" ->
                match Seq.tryHead att.ConstructorArguments with
                | Some(_, (:? string as customName)) -> makeTypedIdent typ customName |> Fable.IdentExpr |> Some
                | _ -> makeTypedIdent typ name |> Fable.IdentExpr |> Some
            | Some "Import" ->
                match Seq.toList att.ConstructorArguments with
                | [(_, (:? string as memb)); (_, (:? string as path))]
                        when not(isNull memb || isNull path || path.StartsWith ".") ->
                    Fable.Import(memb.Trim(), path.Trim(), Fable.CustomImport, typ) |> Some
                | _ -> None
            | _ -> None)

    let (|Imported|_|) r typ argsAndCallInfo (memb: FSharpMemberOrFunctionOrValue) =
        match tryImported r typ memb.CompiledName memb.Attributes with
        | Some expr ->
            match argsAndCallInfo with
            | Some(args: Fable.Expr list, info: Fable.CallInfo) ->
                // Allow combination of Import and Emit attributes
                let emittedCallInfo = { info with argTypes = expr.Type::info.argTypes }
                match memb with
                | Emitted r typ (Some(expr::args, emittedCallInfo)) emitted -> Some emitted
                | _ ->
                    // TODO: Check if owner is an object first
                    match getObjectMemberKind memb with
                    | Fable.Getter -> Some expr
                    | Fable.Setter -> Fable.Set(expr, Fable.VarSet, args.Head, r) |> Some
                    | Fable.Method -> Fable.Operation(Fable.Call(expr, None, args, info), typ, r) |> Some
                    | Fable.Constructor ->
                        let info = { info with isConstructor = true }
                        Fable.Operation(Fable.Call(expr, None, args, info), typ, r) |> Some
            | None ->
                Some expr
        | None -> None

    let (|Inlined|_|) r typ callInfo (memb: FSharpMemberOrFunctionOrValue) =
        None // TODO
        // if not(isInline memb)
        // then None
        // else
        //     let argIdents, fsExpr = com.GetInlineExpr memb
        //     let args = match callee with Some c -> c::args | None -> args
        //     let ctx, assignments, _ =
        //         ((ctx, [], 0), argIdents, args)
        //         |||> Seq.fold2 (fun (ctx, assignments, idx) (KeyValue(argIdent, refCount)) arg ->
        //             // If an expression is referenced more than once, assign it
        //             // to a temp var to prevent multiple evaluations
        //             if refCount > 1 && hasDoubleEvalRisk arg then
        //                 let tmpVar = com.GetUniqueVar() |> makeIdent
        //                 let tmpVarExp = Fable.Value(Fable.IdentValue tmpVar)
        //                 let assign = Fable.VarDeclaration(tmpVar, arg, false, None)
        //                 let ctx = { ctx with scope = (Some argIdent, tmpVarExp)::ctx.scope }
        //                 ctx, (assign::assignments), (idx + 1)
        //             else
        //                 let ctx = { ctx with scope = (Some argIdent, arg)::ctx.scope }
        //                 ctx, assignments, (idx + 1)
        //         )
        //     let typeArgs = matchGenericParams ctx memb (typArgs, methTypArgs)
        //     let ctx = {ctx with typeArgs=typeArgs}
        //     let expr = com.Transform ctx fsExpr
        //     if List.isEmpty assignments
        //     then Some expr
        //     else makeSequential r (assignments@[expr]) |> Some

    let memberRef com (ctx: Context) typ argTypes (memb: FSharpMemberOrFunctionOrValue) =
        let memberName = getMemberDeclarationName com argTypes memb
        let file =
            match memb.EnclosingEntity with
            | Some ent -> (getEntityLocation ent).FileName |> Path.normalizePath
            // Cases when .EnclosingEntity returns None are rare (see #237)
            // We assume the member belongs to the current file
            | None -> ctx.fileName
        if file = ctx.fileName
        then makeTypedIdent typ memberName |> Fable.IdentExpr
        else Fable.Import(memberName, file, Fable.Internal, typ)

    let makeCallFrom (com: IFableCompiler) (ctx: Context) r typ genArgs callee args (memb: FSharpMemberOrFunctionOrValue) =
        let call info callee memb args =
            Fable.Operation(Fable.Call(callee, memb, args, info), typ, r)
        // TODO: Remove optional arguments
        let info: Fable.CallInfo =
          { argTypes = getArgTypes com memb
            isConstructor = false
            hasSpread = hasSpread memb
            hasThisArg = false }
        let extraInfo: Fable.ExtraCallInfo =
          { fullName = memb.FullName
            genericArgs = List.map (makeType com ctx.typeArgs) genArgs }
        let argsAndCallInfo =
            match callee with
            | Some c -> Some(c::args, { info with hasThisArg = true })
            | None -> Some(args, info)
        match memb, memb.EnclosingEntity with
        | Imported r typ argsAndCallInfo imported, _ -> imported
        | Emitted r typ argsAndCallInfo emitted, _ -> emitted
        | Replaced r typ info extraInfo callee args replaced, _ -> replaced
        // TODO | Inlined com ctx r (typArgs, methTypArgs) (callee, args) expr -> expr
        | Try (tryGetBoundExpr ctx r) expr, _ ->
            match callee with
            | Some c -> call { info with hasThisArg = true } expr None (c::args)
            | None ->
                if isModuleValueForCalls memb
                then expr
                else call info expr None args
        // Check if this is an interface or abstract/overriden method
        | _, Some owner when owner.IsInterface
                        || memb.IsOverrideOrExplicitInterfaceImplementation
                        || isAbstract owner ->
            match callee with
            | Some callee ->
                match getObjectMemberKind memb with
                | Fable.Getter -> Fable.Get(callee, Fable.FieldGet memb.DisplayName, typ, r)
                | Fable.Setter -> Fable.Set(callee, Fable.FieldSet memb.DisplayName, args.Head, r)
                // Constructor is unexpected (abstract class cons calls are resolved in transformConstructor)
                | Fable.Constructor
                | Fable.Method -> call info callee (Some memb.DisplayName) args
            | None -> "Unexpected static interface/override call" |> attachRange r |> failwith
        | _ ->
            match callee with
            | Some callee ->
                let info = { info with hasThisArg = true }
                call info (memberRef com ctx Fable.Any info.argTypes memb) None (callee::args)
            | None ->
                if isModuleValueForCalls memb
                then memberRef com ctx typ [] memb
                else call info (memberRef com ctx Fable.Any info.argTypes memb) None args

    let makeValueFrom com (ctx: Context) r (v: FSharpMemberOrFunctionOrValue) =
        let typ = makeType com ctx.typeArgs v.FullType
        match v with
        | _ when typ = Fable.Unit -> Fable.Value Fable.UnitConstant
        | Imported r typ None imported -> imported
        | Emitted r typ None emitted -> emitted
        // TODO: Replaced? Check if there're failing tests
        | Try (tryGetBoundExpr ctx r) expr -> expr
        | _ -> memberRef com ctx typ [] v
