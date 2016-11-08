namespace Fable.FSharp2Fable

open System
open System.Collections.Generic
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.AST.Fable.Util

type DecisionTarget =
    | TargetRef of Fable.Ident
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type ThisAvailability =
    | ThisUnavailable
    | ThisAvailable
    // Object expressions must capture the `this` reference and
    // they can also be nested (see makeThisRef and the ObjectExpr pattern)
    | ThisCaptured
        of currentThis: FSharpMemberOrFunctionOrValue
        * capturedThis: (FSharpMemberOrFunctionOrValue option * Fable.Ident) list

type MemberInfo = {
    isInstance: bool
    passGenerics: bool
}

type Context =
    {
    fileName: string
    scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
    scopedInlines: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
    typeArgs: (string * FSharpType) list
    decisionTargets: Map<int, DecisionTarget>
    baseClass: string option
    thisAvailability: ThisAvailability
    genericAvailability: bool
    }
    static member Empty =
        { fileName="unknown"; scope=[]; scopedInlines=[]
        ; typeArgs=[]; baseClass=None; decisionTargets=Map.empty<_,_>
        ; thisAvailability=ThisUnavailable; genericAvailability=false }

type Role =
    | AppliedArgument
    // For now we're only interested in applied arguments
    | UnknownRole

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract GetInternalFile: FSharpEntity -> string option
    abstract GetEntity: Context -> FSharpEntity -> Fable.Entity
    abstract TryGetInlineExpr: FSharpMemberOrFunctionOrValue -> (FSharpMemberOrFunctionOrValue list * FSharpExpr) option
    abstract AddInlineExpr: string -> (FSharpMemberOrFunctionOrValue list * FSharpExpr) -> unit
    abstract AddUsedVarName: string -> unit
    abstract ReplacePlugins: (string*IReplacePlugin) list

module Atts =
    let mangle = typeof<Fable.Core.MangleAttribute>.Name.Replace("Attribute","")
    let erase = typeof<Fable.Core.EraseAttribute>.Name.Replace("Attribute","")
    let stringEnum = typeof<Fable.Core.StringEnumAttribute>.Name.Replace("Attribute","")
    let keyValueList = typeof<Fable.Core.KeyValueListAttribute>.Name.Replace("Attribute","")
    let passGenerics = typeof<Fable.Core.PassGenericsAttribute>.Name.Replace("Attribute","")
    let mutatingUpdate = typeof<Fable.Core.MutatingUpdateAttribute>.Name.Replace("Attribute","")

module Helpers =
    let rec nonAbbreviatedType (t: FSharpType) =
        if t.IsAbbreviation then nonAbbreviatedType t.AbbreviatedType else t

    let sanitizeEntityName, sanitizeEntityFullName =
        let reg = Regex("`\d+")
        (fun (ent: FSharpEntity) -> reg.Replace(ent.CompiledName, "")),
        (fun (ent: FSharpEntity) -> reg.Replace(defaultArg ent.TryFullName ent.CompiledName, ""))

    let tryFindAtt f (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName ->
                fullName.Substring(fullName.LastIndexOf "." + 1).Replace("Attribute", "")
                |> f |> function true -> Some att | false -> None
            | None -> None)

    let hasAtt name atts =
        atts |> tryFindAtt ((=) name) |> Option.isSome

    let isInline (meth: FSharpMemberOrFunctionOrValue) =
        match meth.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline -> true

    let isImported (ent: FSharpEntity) =
        ent.FullName.StartsWith "Fable.Import"
        || Option.isSome(tryFindAtt Naming.importAtts.Contains ent.Attributes)

    let isExternalEntity (com: IFableCompiler) (ent: FSharpEntity) =
        not(isImported ent) && Option.isNone(com.GetInternalFile ent)

    let isReplaceCandidate (com: IFableCompiler) (ent: FSharpEntity) =
        if ent.IsInterface
        then sanitizeEntityFullName ent |> Naming.replacedInterfaces.Contains
        else ent.FullName.StartsWith "Fable.Core" || isExternalEntity com ent

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Core.Unit"
        else false

    // TODO: Check that all record fields are immutable?
    let isMutatingUpdate (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.IsFSharpRecord
                && hasAtt Atts.mutatingUpdate typ.TypeDefinition.Attributes
        else false

    let sameMemberLoc memberLoc1 memberLoc2 =
        match memberLoc1, memberLoc2 with
        | Fable.StaticLoc, Fable.StaticLoc -> true
        | Fable.InstanceLoc, Fable.InstanceLoc -> true
        | Fable.InterfaceLoc _, Fable.InterfaceLoc _ -> true
        | _ -> false

    let makeRange (r: Range.range) = {
        start = { line = r.StartLine; column = r.StartColumn }
        ``end``= { line = r.EndLine; column = r.EndColumn }
    }

    let makeRangeFrom (fsExpr: FSharpExpr) =
        Some (makeRange fsExpr.Range)

    let rec countFuncArgs (fn: FSharpType) =
        if fn.IsFunctionType
        then countFuncArgs (Seq.last fn.GenericArguments) + 1
        else 0

    let getEntityLocation (ent: FSharpEntity) =
        match ent.ImplementationLocation with
        | Some loc -> loc
        | None -> ent.DeclarationLocation

    let getRefLocation (ent: FSharpMemberOrFunctionOrValue) =
        match ent.ImplementationLocation with
        | Some loc -> loc
        | None -> ent.DeclarationLocation

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt ((=) "CompiledName")
        |> function
            | Some name -> name.ConstructorArguments.[0] |> snd |> string
            | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeConst

    let getArgCount (meth: FSharpMemberOrFunctionOrValue) =
        let args = meth.CurriedParameterGroups
        if args.Count = 0 then 0
        elif args.Count = 1 && args.[0].Count = 1 then
            if isUnit args.[0].[0].Type then 0 else 1
        else args |> Seq.map (fun li -> li.Count) |> Seq.sum

    let tryGetInterfaceFromMethod (meth: FSharpMemberOrFunctionOrValue) =
        // Method implementations
        if meth.IsExplicitInterfaceImplementation
        then
            if meth.ImplementedAbstractSignatures.Count > 0
            then
                let x = meth.ImplementedAbstractSignatures.[0].DeclaringType
                if x.HasTypeDefinition then Some x.TypeDefinition else None
            else None
        // Method calls
        elif meth.EnclosingEntity.IsInterface
        then Some meth.EnclosingEntity
        else None

    let getMemberLoc (meth: FSharpMemberOrFunctionOrValue) =
        if not meth.IsInstanceMember && not meth.IsImplicitConstructor
        then Fable.StaticLoc
        else tryGetInterfaceFromMethod meth
             |> Option.map (sanitizeEntityFullName >> Fable.InterfaceLoc)
             |> defaultArg <| Fable.InstanceLoc

    let getMemberKind (meth: FSharpMemberOrFunctionOrValue) =
        let argCount = lazy(getArgCount meth)
        if meth.EnclosingEntity.IsFSharpModule then
            // TODO: Another way to check module values?
            match meth.XmlDocSig.[0] with
            | 'P' when argCount.Value = 0 -> Fable.Field
            | _ -> Fable.Method
        elif meth.IsImplicitConstructor then Fable.Constructor
        elif meth.IsPropertyGetterMethod && argCount.Value = 0 then Fable.Getter
        elif meth.IsPropertySetterMethod && argCount.Value = 1 then Fable.Setter
        else Fable.Method

    let sanitizeMethodName (meth: FSharpMemberOrFunctionOrValue) =
        match tryGetInterfaceFromMethod meth, getMemberKind meth with
        | Some ifc, _ when hasAtt Atts.mangle ifc.Attributes ->
            // meth.CompiledName && meth.FullName are different for interface methods
            // depending whether it's a method implementation or call
            sanitizeEntityFullName ifc + "." + meth.DisplayName
        | Some _, _ -> meth.DisplayName
        | None, (Fable.Getter | Fable.Setter) -> meth.DisplayName
        | _ -> meth.CompiledName

    let hasRestParams (meth: FSharpMemberOrFunctionOrValue) =
        if meth.CurriedParameterGroups.Count <> 1 then false else
        let args = meth.CurriedParameterGroups.[0]
        args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

module Patterns =
    open BasicPatterns
    open Helpers

    let (|Rev|) = List.rev
    let (|AsArray|) = Array.ofSeq
    let (|Transform|) (com: IFableCompiler) = com.Transform
    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprType|) (expr: Fable.Expr) = expr.Type
    let (|EntityKind|) (ent: Fable.Entity) = ent.Kind

    let (|NonAbbreviatedType|) (t: FSharpType) =
        nonAbbreviatedType t

    let (|TypeDefinition|_|) (NonAbbreviatedType t) =
        if t.HasTypeDefinition then Some t.TypeDefinition else None

    let (|RefType|_|) = function
        | NonAbbreviatedType(TypeDefinition tdef) as t
            when tdef.TryFullName = Some "Microsoft.FSharp.Core.FSharpRef`1" -> Some t
        | _ -> None

    let (|ListType|_|) = function
        | NonAbbreviatedType(TypeDefinition tdef) as t
            when tdef.TryFullName = Some "Microsoft.FSharp.Collections.FSharpList`1" -> Some t
        | _ -> None

    let (|ForOf|_|) = function
        | Let((_, value),
              Let((_, Call(None, meth, _, [], [])),
                TryFinally(
                  WhileLoop(_,
                    Let((ident, _), body)), _)))
        | Let((_, Call(Some value, meth, _, [], [])),
                TryFinally(
                    WhileLoop(_,
                        Let((ident, _), body)), _))
            when meth.CompiledName = "GetEnumerator" ->
            Some(ident, value, body)
        | _ -> None

    let (|ComposableExpr|_|) e =
        match e with
        | Call(None,_,_,_,args) -> Some (e, args)
        | NewObject(_,_,args) -> Some (e, args)
        | NewUnionCase(fsType,_,args) ->
            // Lists are usually flattened so they're not easily composable
            match fsType with ListType _ -> None | _ -> Some (e, args)
        | _ -> None

    // These are closures created by F# compiler, e.g. given `let add x y z = x+y+z`
    // `3 |> add 1 2` will become `let x=1 in let y=2 in fun z -> add(x,y,z)`
    let (|Closure|_|) fsExpr =
        let checkArgs (identAndRepls: (FSharpMemberOrFunctionOrValue*FSharpExpr) list) args =
            if identAndRepls.Length <> (List.length args) then false else
            (args, identAndRepls)
            ||> List.forall2 (fun arg (ident, _) ->
                if ident.IsMutable then false else
                match arg with
                | Coerce(_, Value arg) | Value arg -> ident = arg
                | _ -> false)
        let checkArgs2 lambdaArgs methArgs =
            (lambdaArgs, methArgs)
            ||> List.forall2 (fun larg marg ->
                match marg with
                | Coerce(_, Value marg) | Value marg -> marg = larg
                | _ -> false)
        let rec visit identAndRepls = function
            | Let((letArg, letValue), letBody) ->
                let identAndRepls = identAndRepls@[(letArg, letValue)]
                match letBody with
                | Lambda(lambdaArg1, ComposableExpr(e, Rev (last1::args))) ->
                    if checkArgs identAndRepls (List.rev args) &&
                        checkArgs2 [lambdaArg1] [last1]
                    then Some(1, e, List.map snd identAndRepls)
                    else None
                | Lambda(lambdaArg1,
                         Lambda(lambdaArg2, ComposableExpr(e, Rev (last2::last1::args)))) ->
                    if checkArgs identAndRepls (List.rev args) &&
                        checkArgs2 [lambdaArg1;lambdaArg2] [last1;last2]
                    then Some(2, e, List.map snd identAndRepls)
                    else None
                | Lambda(lambdaArg1,
                         Lambda(lambdaArg2,
                            Lambda(lambdaArg3,ComposableExpr(e, Rev (last3::last2::last1::args))))) ->
                    if checkArgs identAndRepls (List.rev args) &&
                        checkArgs2 [lambdaArg1;lambdaArg2;lambdaArg3] [last1;last2;last3]
                    then Some(3, e, List.map snd identAndRepls)
                    else None
                | _ -> visit identAndRepls letBody
            | _ -> None
        match fsExpr with
        | Lambda(larg1, ComposableExpr(e, [marg1]))
            when checkArgs2 [larg1] [marg1] ->
                Some(1, e, [])
        | Lambda(larg1, Lambda(larg2, ComposableExpr(e, [marg1;marg2])))
            when checkArgs2 [larg1;larg2] [marg1;marg2] ->
                Some(2, e, [])
        | Lambda(larg1, Lambda(larg2, Lambda(larg3, ComposableExpr(e, [marg1;marg2;marg3]))))
            when checkArgs2 [larg1;larg2;larg3] [marg1;marg2;marg3] ->
                Some(3, e, [])
        | _ -> visit [] fsExpr

    let (|PrintFormat|_|) = function
        | Let((_,(Call(None,_,_,_,[arg]) as e)),_) ->
            if arg.Type.HasTypeDefinition
                && arg.Type.TypeDefinition.AccessPath = "Microsoft.FSharp.Core.PrintfModule"
            then Some e
            else None
        | _ -> None

    let (|Pipe|_|) = function
        | Call(None, meth, _, _, [arg1; arg2]) ->
            match meth.FullName with
            | "Microsoft.FSharp.Core.Operators.( |> )" ->
                Some (arg2, [arg1])
            | "Microsoft.FSharp.Core.Operators.( <| )" ->
                Some (arg1, [arg2])
            | _ -> None
        | Call(None, meth, _, _, [arg1; arg2; arg3]) ->
            match meth.FullName with
            | "Microsoft.FSharp.Core.Operators.( ||> )" ->
                Some (arg3, [arg1; arg2])
            | "Microsoft.FSharp.Core.Operators.( <|| )" ->
                Some (arg1, [arg2; arg3])
            | _ -> None
        | Call(None, meth, _, _, [arg1; arg2; arg3; arg4]) ->
            match meth.FullName with
            | "Microsoft.FSharp.Core.Operators.( |||> )" ->
                Some (arg4, [arg1; arg2; arg3])
            | "Microsoft.FSharp.Core.Operators.( <||| )" ->
                Some (arg1, [arg2; arg3; arg4])
            | _ -> None
        | _ -> None

    // TODO: Make it recursive
    let (|Composition|_|) = function
        | Call(None, comp, _, _, [Closure(1, e1, args1); Closure(1, e2, args2)]) ->
            match comp.FullName with
            | "Microsoft.FSharp.Core.Operators.( >> )" ->
                Some (e1, args1, e2, args2)
            | "Microsoft.FSharp.Core.Operators.( << )" ->
                Some (e2, args2, e1, args1)
            | _ -> None
        | _ -> None

    let (|ErasableLambda|_|) fsExpr =
        match fsExpr with
        | Pipe(Closure(arity, e, args), exprs) when arity = exprs.Length -> Some (e, args@exprs)
        | _ -> None

    // F# compiler always wraps the result of Fable.Core.(?) operator in a closure
    let (|Applicable|_|) = function
        | Let((_, applicable),Lambda(_,Application(apArg,_,_)))->
            let ctyp = applicable.Type
            if ctyp.IsAbbreviation
                && ctyp.HasTypeDefinition
                // Apparently FullName fails for type definitions of abbreviations
                && ctyp.TypeDefinition.AccessPath = "Fable.Core"
                && ctyp.TypeDefinition.CompiledName = "Applicable"
            then Some applicable
            else None
        | _ -> None

    let (|JsFunc|_|) = function
        | NewObject(meth, _typArgs, [Lambda(thisVar, body)]) ->
            match meth.EnclosingEntity.TryFullName with
            | Some name when name.StartsWith("Fable.Core.JsInterop.JsFunc") -> Some(thisVar, body)
            | _ -> None
        | _ -> None

    let (|ImmutableBinding|_|) = function
        | Let((var, (Value v as value)), body)
            when not var.IsMutable && not v.IsMutable && not v.IsMemberThisValue -> Some((var, value), body)
        | Let((var, (UnionCaseGet(Value v,_,_,_) as value)), body)
            when not var.IsMutable && not v.IsMutable -> Some((var, value), body)
        | Let((var, (TupleGet(_,_,Value v) as value)), body)
            when not var.IsMutable && not v.IsMutable -> Some((var, value), body)
        | Let((var, (FSharpFieldGet(Some(Value v),_,fi) as value)), body)
            when not var.IsMutable && not v.IsMutable && not fi.IsMutable -> Some((var, value), body)
        | _ -> None

    /// This matches the boilerplate F# compiler generates for methods
    /// like Dictionary.TryGetValue (see #154)
    let (|TryGetValue|_|) = function
        | Let((outArg1, (DefaultValue _ as def)),
                NewTuple(_, [Call(callee, meth, typArgs, methTypArgs,
                                    [arg; AddressOf(Value outArg2)]); Value outArg3]))
            when outArg1 = outArg2 && outArg1 = outArg3 ->
            Some (callee, meth, typArgs, methTypArgs, [arg; def])
        | _ -> None

    /// This matches the boilerplate generated to wrap .NET events from F#
    let (|CreateEvent|_|) = function
        | Call(Some(Call(None, createEvent,_,_,
                        [Lambda(eventDelegate, Call(Some callee, addEvent,[],[],[Value eventDelegate']));
                         Lambda(eventDelegate2, Call(Some callee2, removeEvent,[],[],[Value eventDelegate2']));
                         Lambda(callback, NewDelegate(_, Lambda(delegateArg0, Lambda(delegateArg1, Application(Value callback',[],[Value delegateArg0'; Value delegateArg1'])))))])),
                meth, typArgs, methTypArgs, args)
                when createEvent.FullName = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent" ->
            let eventName = addEvent.CompiledName.Replace("add_","")
            Some (callee, eventName, meth, typArgs, methTypArgs, args)
        | _ -> None

    /// This matches the boilerplate generated to check an array's length
    /// when pattern matching
    let (|CheckArrayLength|_|) = function
        | IfThenElse
            (ILAsm ("[AI_ldnull; AI_cgt_un]",[],[matchValue]),
             Call(None,_op_Equality,[],[_typeInt],
                [ILAsm ("[I_ldlen; AI_conv DT_I4]",[],[_matchValue2])
                 Const (length,_typeInt2)]),
             Const (_falseConst,_typeBool)) -> Some (matchValue, length)
        | _ -> None

    let (|NumberKind|_|) = function
        | "System.SByte" -> Some Int8
        | "System.Byte" -> Some UInt8
        | "System.Int16" -> Some Int16
        | "System.UInt16" -> Some UInt16
        | "System.Int32" -> Some Int32
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.int" _ -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Int64" -> Some Int64
        | "System.UInt64" -> Some UInt64
        | "System.Single" -> Some Float32
        | "System.Double" -> Some Float64
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.float" _ -> Some Float64
        | Naming.StartsWith "Microsoft.FSharp.Core.float32" _ -> Some Float32
        | "System.Decimal" -> Some Decimal
        | _ -> None

    let (|Switch|_|) fsExpr =
        let isStringOrNumber (NonAbbreviatedType typ) =
            if not typ.HasTypeDefinition then false else
            match typ.TypeDefinition.TryFullName with
            | Some("System.String") | Some(NumberKind _) -> true
            | _ when typ.TypeDefinition.IsEnum -> true
            | _ -> false
        let rec makeSwitch map matchValue e =
            let addCase map (idx: int) (case: obj) =
                match Map.tryFind idx map with
                | Some cases -> Map.add idx (case::cases) map
                | None -> Map.add idx [case] map
            match e with
            | IfThenElse(Call(None,op_Equality,[],_,[Value var; Const(case,_)]),
                         DecisionTreeSuccess(idx, []), elseExpr)
                when op_Equality.CompiledName.Equals("op_Equality") ->
                let matchValue =
                    match matchValue with
                    | None -> if isStringOrNumber var.FullType then Some var else None
                    | Some matchValue when matchValue.Equals(var) -> Some matchValue
                    | _ -> None
                match matchValue with
                | Some matchValue ->
                    let map =
                        match Map.tryFind idx map with
                        | Some cases -> Map.add idx (cases@[case]) map
                        | None -> Map.add idx [case] map
                    match elseExpr with
                    | DecisionTreeSuccess(idx, []) ->
                        Some(matchValue, map, idx)
                    | elseExpr -> makeSwitch map (Some matchValue) elseExpr
                | None -> None
            | _ -> None
        match fsExpr with
        | DecisionTree(decisionExpr, decisionTargets) ->
            // TODO: Optimize also simple pattern matching with union types
            match makeSwitch Map.empty None decisionExpr with
            | Some(matchValue, cases, defaultCase) ->
                Some(matchValue, cases, defaultCase, decisionTargets)
            | None -> None
        | _ -> None

    /// Record updates as in `{ a with name = "Anna" }`
    let (|RecordMutatingUpdate|_|) fsExpr =
        let rec visit identAndBindings = function
            | Let((ident, binding), letBody) when ident.IsCompilerGenerated ->
                visit ((ident, binding)::identAndBindings) letBody
            | NewRecord(NonAbbreviatedType recType, argExprs) when isMutatingUpdate recType ->
                ((None, []), Seq.zip recType.TypeDefinition.FSharpFields argExprs)
                ||> Seq.fold (fun (prevRec, updatedFields) (fi, e) ->
                    match e with
                    | FSharpFieldGet(Some(Value prevRec'), recType', fi')
                        when recType' = recType && fi.Name = fi'.Name ->
                        match prevRec with
                        | Some prevRec ->
                            if prevRec = prevRec'
                            then Some prevRec', updatedFields
                            else None, []
                        | None ->
                            if not prevRec'.IsMutable
                            then Some prevRec', updatedFields
                            else None, []
                    | e -> prevRec, (fi, e)::updatedFields)
                |> function
                    | Some prevRec, updatedFields ->
                        let updatedFields =
                            let identAndBindings = dict identAndBindings
                            updatedFields |> List.map (fun (fi, e) ->
                                match e with
                                | Value ident when identAndBindings.ContainsKey ident ->
                                    fi, identAndBindings.[ident]
                                | e -> fi, e)
                        Some(recType, prevRec, updatedFields)
                    | _ -> None
            | _ -> None
        visit [] fsExpr

    let (|ContainsAtt|_|) (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> tryFindAtt ((=) name) |> Option.map (fun att ->
            att.ConstructorArguments |> Seq.map snd |> Seq.toList)

    let (|OptionUnion|ListUnion|ErasedUnion|KeyValueUnion|StringEnum|OtherType|) (typ: Fable.Type) =
        let (|FullName|) (ent: Fable.Entity) = ent.FullName
        let (|TryDecorator|_|) dec (ent: Fable.Entity) = ent.TryGetDecorator dec
        match typ with
        | Fable.Option _ -> OptionUnion
        | Fable.DeclaredType(ent,_) ->
            match ent with
            | FullName "Microsoft.FSharp.Collections.FSharpList" -> ListUnion
            | TryDecorator Atts.erase _ -> ErasedUnion
            | TryDecorator Atts.keyValueList _ -> KeyValueUnion
            | TryDecorator Atts.stringEnum _ -> StringEnum
            | _ -> OtherType
        | _ -> failwithf "Unexpected union type: %s" typ.FullName

    let (|FableNull|_|) = function
        | Fable.Wrapped(Fable.Value Fable.Null, _)
        | Fable.Value Fable.Null as e -> Some e
        | _ -> None

module Types =
    open Helpers
    open Patterns

    // TODO: Exclude attributes meant to be compiled to JS
    let rec isAttributeEntity (ent: FSharpEntity) =
        match ent.BaseType with
        | Some (NonAbbreviatedType t) when t.HasTypeDefinition ->
            match t.TypeDefinition.TryFullName with
            | Some "System.Attribute" -> true
            | _ -> isAttributeEntity t.TypeDefinition
        | _ -> false

    let rec getBaseClass (com: IFableCompiler) (ctx: Context) (tdef: FSharpEntity) =
        let isIgnored (t: FSharpType) =
            not t.HasTypeDefinition || isExternalEntity com t.TypeDefinition
        match tdef.BaseType with
        | None -> None
        | Some (NonAbbreviatedType t) ->
            if isIgnored t then None else
            let typeRef =
                makeType com Context.Empty t
                |> makeNonGenTypeRef ctx.fileName
            Some (sanitizeEntityFullName t.TypeDefinition, typeRef)

    // Some attributes (like ComDefaultInterface) will throw an exception
    // when trying to access ConstructorArguments
    and makeDecorator (com: IFableCompiler) (att: FSharpAttribute) =
        try
            let args = att.ConstructorArguments |> Seq.map snd |> Seq.toList
            let fullName =
                let fullName = sanitizeEntityFullName att.AttributeType
                if fullName.EndsWith ("Attribute")
                then fullName.Substring (0, fullName.Length - 9)
                else fullName
            Fable.Decorator(fullName, args) |> Some
        with _ ->
            None

    and makeMethodFrom com name kind loc argTypes returnType originalTyp overloadIndex
                       (meth: FSharpMemberOrFunctionOrValue) =
        Fable.Member(name, kind, loc, argTypes, returnType,
            originalType = originalTyp,
            genParams = (meth.GenericParameters |> Seq.map (fun x -> x.Name) |> Seq.toList),
            decorators = (meth.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList),
            isPublic = (not meth.Accessibility.IsPrivate && not meth.IsCompilerGenerated),
            isMutable = meth.IsMutable,
            ?overloadIndex = overloadIndex,
            hasRestParams = hasRestParams meth)

    and getArgTypes com (args: IList<IList<FSharpParameter>>) =
        // FSharpParameters don't contain the `this` arg
        match args |> Seq.map Seq.toList |> Seq.toList with
        | [] -> []
        | [[singleArg]] when isUnit singleArg.Type -> []
        // The F# compiler "untuples" the args in methods
        | args -> List.concat args |> List.map (fun x -> makeType com Context.Empty x.Type)

    and makeOriginalCurriedType com (args: IList<IList<FSharpParameter>>) returnType =
        let tys = args |> Seq.map (fun tuple ->
            let tuple = tuple |> Seq.map (fun t -> makeType com Context.Empty t.Type)
            match List.ofSeq tuple with
            | [singleArg] -> singleArg
            | args -> Fable.Tuple(args) )
        Seq.append tys [returnType] |> Seq.reduceBack (fun a b -> Fable.Function([a], b))

    and getMembers com (tdef: FSharpEntity) =
        let members =
            tdef.MembersFunctionsAndValues
            |> Seq.filter (fun x ->
                // Discard overrides to prevent confusing them with overloads (see #505)
                not(x.IsOverrideOrExplicitInterfaceImplementation && not x.IsExplicitInterfaceImplementation)
                // Property members that are no getter nor setter don't actually get implemented
                && not(x.IsProperty && not(x.IsPropertyGetterMethod || x.IsPropertySetterMethod)))
            |> Seq.map (fun meth -> sanitizeMethodName meth, getMemberKind meth, getMemberLoc meth, meth)
            |> Seq.toArray
        let isOverloadable =
            // TODO: Use overload index for interfaces too? (See isOverloaded below too)
            not(tdef.IsInterface || isImported tdef || isReplaceCandidate com tdef)
        let getMembers' loc (tdef: FSharpEntity) =
            members
            |> Seq.filter (fun (_, _, mloc, _) -> sameMemberLoc loc mloc)
            |> Seq.groupBy (fun (name, kind, _, _) -> name, kind)
            |> Seq.collect (fun ((name, kind), AsArray members) ->
                let isOverloaded =
                    match loc with
                    | Fable.InterfaceLoc _ -> false
                    | _ -> isOverloadable && members.Length > 1
                members |> Array.mapi (fun i (_, _, loc, meth) ->
                    let argTypes = getArgTypes com meth.CurriedParameterGroups
                    let returnType = makeType com Context.Empty meth.ReturnParameter.Type
                    let originalTyp = makeOriginalCurriedType com meth.CurriedParameterGroups returnType
                    let overloadIndex = if isOverloaded then Some i else None
                    makeMethodFrom com name kind loc argTypes returnType originalTyp overloadIndex meth
            ))
            |> Seq.toList
        let instanceMembers = getMembers' Fable.InstanceLoc tdef
        let staticMembers = getMembers' Fable.StaticLoc tdef
        let interfaceMembers = getMembers' (Fable.InterfaceLoc "") tdef
        instanceMembers@interfaceMembers@staticMembers

    and makeEntity (com: IFableCompiler) ctx (tdef: FSharpEntity): Fable.Entity =
        let makeFields (tdef: FSharpEntity) =
            tdef.FSharpFields
            // It's ok to use an empty context here, because we don't need to resolve generic params
            |> Seq.map (fun x -> x.Name, makeType com Context.Empty x.FieldType)
            |> Seq.toList
        let makeProperties (tdef: FSharpEntity) =
            tdef.MembersFunctionsAndValues
            |> Seq.choose (fun x ->
                if not x.IsPropertyGetterMethod then None else
                match makeType com Context.Empty x.FullType with
                | Fable.Function([Fable.Unit], returnType) ->
                    Some(x.DisplayName, returnType)
                | _ -> None)
            |> Seq.toList
        let makeCases (tdef: FSharpEntity) =
            tdef.UnionCases |> Seq.map (fun x ->
                x.Name, [for fi in x.UnionCaseFields do yield makeType com Context.Empty fi.FieldType])
            |> Map
        let getKind () =
            if tdef.IsInterface then Fable.Interface
            elif tdef.IsFSharpUnion then makeCases tdef |> Fable.Union
            elif tdef.IsFSharpRecord then makeFields tdef |> Fable.Record
            elif tdef.IsFSharpExceptionDeclaration then makeFields tdef |> Fable.Exception
            elif tdef.IsFSharpModule || tdef.IsNamespace then Fable.Module
            else Fable.Class(getBaseClass com ctx tdef, makeProperties tdef)
        let genParams =
            tdef.GenericParameters |> Seq.map (fun x -> x.Name) |> Seq.toList
        let infcs =
            tdef.DeclaredInterfaces
            |> Seq.map (fun x -> sanitizeEntityFullName x.TypeDefinition)
            |> Seq.filter (Naming.ignoredInterfaces.Contains >> not)
            |> Seq.distinct
            |> Seq.toList
        let decs =
            tdef.Attributes
            |> Seq.choose (makeDecorator com)
            |> Seq.toList
        Fable.Entity (Lazy<_>(fun () -> getKind()), com.GetInternalFile tdef,
            sanitizeEntityFullName tdef, Lazy<_>(fun () -> getMembers com tdef),
            genParams, infcs, decs, tdef.Accessibility.IsPublic || tdef.Accessibility.IsInternal)

    and makeTypeFromDef (com: IFableCompiler) ctx (tdef: FSharpEntity)
                        (genArgs: #seq<FSharpType>) =
        let fullName = defaultArg tdef.TryFullName tdef.CompiledName
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
        // Array
        elif tdef.IsArrayType
        then Fable.Array(Seq.head genArgs |> makeType com ctx)
        // Enum
        elif tdef.IsEnum
        then Fable.Enum fullName
        // Delegate
        elif tdef.IsDelegate
        then
            match Seq.length genArgs with
            | 0 -> [Fable.Unit], Fable.Unit
            | 1 -> [Seq.head genArgs |> makeType com ctx], Fable.Unit
            | c -> Seq.take (c-1) genArgs |> Seq.map (makeType com ctx) |> Seq.toList,
                    Seq.last genArgs |> makeType com ctx
            |> Fable.Function
        // Object
        elif fullName = "System.Object"
        then Fable.Any
        else
        match fullName with
        | "System.Boolean" -> Fable.Boolean
        | "System.Char" | "System.String" | "System.Guid" -> Fable.String
        | "Microsoft.FSharp.Core.Unit" -> Fable.Unit
        | "Microsoft.FSharp.Core.FSharpOption`1" ->
            let t = Seq.tryHead genArgs |> Option.map (makeType com ctx)
            Fable.Option(defaultArg t Fable.Any)
        | "System.Collections.Generic.List`1" ->
            let t = Seq.tryHead genArgs |> Option.map (makeType com ctx)
            Fable.Array(defaultArg t Fable.Any)
        | NumberKind kind -> Fable.Number kind
        // Declared Type
        | _ -> Fable.DeclaredType(com.GetEntity ctx tdef,
                genArgs |> Seq.map (makeType com ctx) |> Seq.toList)

    and makeType (com: IFableCompiler) (ctx: Context) (NonAbbreviatedType t) =
        let rec getFnGenArgs (acc: FSharpType list) (fn: FSharpType) =
            if fn.IsFunctionType
            then getFnGenArgs (fn.GenericArguments.[0]::acc) fn.GenericArguments.[1]
            else fn::acc
        let makeGenArgs (genArgs: #seq<FSharpType>) =
            Seq.map (makeType com ctx) genArgs |> Seq.toList
        let resolveGenParam (genParam: FSharpGenericParameter) =
            ctx.typeArgs
            |> List.tryFind (fun (name,_) -> name = genParam.Name)
            |> function Some (_,typ) -> makeType com ctx typ | None -> Fable.GenericParam genParam.Name
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
        // Tuple
        elif t.IsTupleType
        then Fable.Tuple(makeGenArgs t.GenericArguments)
        // Funtion
        elif t.IsFunctionType
        then
            let gs = getFnGenArgs [] t
            (List.rev gs.Tail |> List.map (makeType com ctx), makeType com ctx gs.Head)
            |> Fable.Function
        elif t.HasTypeDefinition
        then makeTypeFromDef com ctx t.TypeDefinition t.GenericArguments
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    let (|FableType|) = makeType

module Identifiers =
    open Helpers
    open Types

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with scope = (Some fsRef, expr)::ctx.scope}

    /// Make a sanitized identifier from a tentative name
    let bindIdent (com: IFableCompiler) (ctx: Context) typ
                  (fsRef: FSharpMemberOrFunctionOrValue option) tentativeName =
        let sanitizedName = tentativeName |> Naming.sanitizeIdent (fun x ->
            List.exists (fun (_,x') ->
                match x' with
                | Fable.Value (Fable.IdentValue i) -> x = i.Name
                | _ -> false) ctx.scope)
        com.AddUsedVarName sanitizedName
        let ident = Fable.Ident(sanitizedName, typ)
        let identValue = Fable.Value (Fable.IdentValue ident)
        { ctx with scope = (fsRef, identValue)::ctx.scope}, ident

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        bindIdent com ctx (makeType com ctx fsRef.FullType) (Some fsRef) fsRef.CompiledName

    let (|BindIdent|) = bindIdentFrom

    let tryGetBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fst >> function Some fsRef' -> obj.Equals(fsRef, fsRef') | None -> false)
        |> function
            | Some(_, (Fable.Value(Fable.IdentValue i) as boundExpr)) ->
                if i.IsConsumed && isMutatingUpdate fsRef.FullType then
                    FableError("Value marked as MutatingUpdate has already been consumed: " + i.Name, ?range=r) |> raise
                Some boundExpr
            | Some(_, boundExpr) -> Some boundExpr
            | None -> None

    /// Get corresponding identifier to F# value in current scope
    let getBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        match tryGetBoundExpr ctx r fsRef with
        | Some boundExpr -> boundExpr
        | None -> failwithf "Detected non-bound identifier: %s in %O"
                    fsRef.CompiledName (getRefLocation fsRef |> makeRange)

    let consumeBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        getBoundExpr ctx r fsRef
        |> function
            | Fable.Value(Fable.IdentValue i) as e -> i.Consume(); e
            | e -> e

module Util =
    open Helpers
    open Patterns
    open Types
    open Identifiers

    let makeLambdaArgs com ctx (vars: FSharpMemberOrFunctionOrValue list) =
        match vars with
        | [var] when isUnit var.FullType -> ctx, []
        | _ ->
            let ctx, args =
                ((ctx, []), vars)
                ||> List.fold (fun (ctx, accArgs) var ->
                    let newContext, arg = bindIdentFrom com ctx var
                    newContext, arg::accArgs)
            ctx, List.rev args

    let bindMemberArgs com ctx (info: MemberInfo) (args: FSharpMemberOrFunctionOrValue list list) =
        let thisArg, args =
            match args with
            | [thisArg]::args when info.isInstance ->
                Some thisArg, args
            | _ -> None, args
        match args with
        | [] -> ctx, thisArg, []
        | [[singleArg]] when isUnit singleArg.FullType -> ctx, thisArg, []
        | args ->
            List.foldBack (fun tupledArg (ctx, thisArg, accArgs) ->
                // The F# compiler "untuples" the args in methods
                let ctx, untupledArg = makeLambdaArgs com ctx tupledArg
                ctx, thisArg, untupledArg@accArgs
            ) args (ctx, thisArg, [])
        |> fun (ctx, thisArg, args) ->
            if info.passGenerics
            then
                let args = args@[makeIdent Naming.genArgsIdent]
                { ctx with genericAvailability=true }, thisArg, args
            else ctx, thisArg, args

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
        Fable.TryCatch (body, catchClause, finalizer, makeRangeFrom fsExpr)

    let makeGetFrom com ctx r typ callee propExpr =
        Fable.Apply (callee, [propExpr], Fable.ApplyGet, typ, r)

    // This method doesn't work, the arguments don't keep the attributes
//    let hasRestParams (args: FSharpMemberOrFunctionOrValue list list) =
//        match args with
//        | [args] when args.Length > 0 ->
//            tryFindAtt ((=) "ParamArray") (Seq.last args).Attributes
//            |> Option.isSome
//        | _ -> false

    let buildApplyInfo com (ctx: Context) r typ ownerType ownerFullName methName methKind
                       (atts, typArgs, methTypArgs, lambdaArgArity) (callee, args)
                       : Fable.ApplyInfo =
        {
            ownerType = ownerType
            ownerFullName = ownerFullName
            methodName = methName
            methodKind = methKind
            range = r
            fileName = ctx.fileName
            callee = callee
            args = args
            returnType = typ
            decorators = atts |> Seq.choose (makeDecorator com) |> Seq.toList
            calleeTypeArgs = typArgs |> List.map (makeType com ctx)
            methodTypeArgs = methTypArgs |> List.map (makeType com ctx)
            genericAvailability = ctx.genericAvailability
            lambdaArgArity = lambdaArgArity
        }

    let buildApplyInfoFrom com ctx r typ (typArgs, methTypArgs)
                       (callee, args) (meth: FSharpMemberOrFunctionOrValue)
                       : Fable.ApplyInfo =
        let lambdaArgArity =
            if meth.CurriedParameterGroups.Count > 0
                && meth.CurriedParameterGroups.[0].Count > 0
            then countFuncArgs meth.CurriedParameterGroups.[0].[0].Type
            else 0
        let ownerType = makeTypeFromDef com ctx meth.EnclosingEntity []
        let ownerFullName = sanitizeEntityFullName meth.EnclosingEntity
        buildApplyInfo com ctx r typ
            ownerType ownerFullName (sanitizeMethodName meth) (getMemberKind meth)
            (meth.Attributes, typArgs, methTypArgs, lambdaArgArity)
            (callee, args)

    let replace (com: IFableCompiler) (applyInfo: Fable.ApplyInfo) =
        let pluginReplace (i: Fable.ApplyInfo) =
            com.ReplacePlugins
            |> Plugins.tryPlugin i.range (fun p -> p.TryReplace com i)
        match applyInfo with
        | Try pluginReplace repl -> repl
        | Try (Replacements.tryReplace com) repl -> repl
        | _ ->
            FableError("Cannot find replacement for " + applyInfo.ownerFullName + "." +
                applyInfo.methodName, ?range=applyInfo.range) |> raise

    let matchGenericParams com ctx (meth: FSharpMemberOrFunctionOrValue) (typArgs, methTypArgs) =
        let (|ResolveGeneric|) ctx (t: FSharpType) =
            if not t.IsGenericParameter then t else
            let genParam = t.GenericParameter
            ctx.typeArgs |> List.tryFind (fun (name,_) -> name = genParam.Name)
            |> function Some (_,t) -> t | None -> t
        let genArgs =
            if meth.IsModuleValueOrMember then
                ([], meth.EnclosingEntity.GenericParameters, typArgs)
                |||> Seq.fold2 (fun acc genPar (ResolveGeneric ctx t) -> acc@[genPar.Name, t])
            else []
        (genArgs, meth.GenericParameters, methTypArgs)
        |||> Seq.fold2 (fun acc genPar (ResolveGeneric ctx t) -> acc@[genPar.Name, t])

    let (|Replaced|_|) (com: IFableCompiler) ctx r typ
                    (typArgs, methTypArgs) (callee, args)
                    (meth: FSharpMemberOrFunctionOrValue) =
        if isReplaceCandidate com meth.EnclosingEntity then
            buildApplyInfoFrom com ctx r typ
                (typArgs, methTypArgs) (callee, args) meth
            |> replace com |> Some
        else
            None

    let getEmitter =
        let cache = Dictionary<string, obj>()
        fun (tdef: FSharpEntity) ->
            cache.GetOrAdd(tdef.QualifiedName, fun _ ->
                let filePath = tdef.Assembly.FileName.Value
#if NETSTANDARD1_6
                let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
                let assembly = globalLoadContext.LoadFromAssemblyPath(filePath)
#else
                let assembly = System.Reflection.Assembly.LoadFrom(filePath)
#endif
                let typ = assembly.GetTypes() |> Seq.find (fun x ->
                    x.AssemblyQualifiedName = tdef.QualifiedName)
                System.Activator.CreateInstance(typ))

    let (|Emitted|_|) com ctx r typ (typArgs, methTypArgs) (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        match meth.Attributes with
        | ContainsAtt "Emit" attArgs ->
            match attArgs with
            | [:? string as macro] ->
                let args = List.map (makeDelegate com None) args
                let args = match callee with None -> args | Some c -> c::args
                let macro, args =
                    let mutable extraArgs = []
                    let addExtraArg arg =
                        let pos = args.Length + extraArgs.Length
                        extraArgs <- arg::extraArgs
                        "$" + string pos
                    // Trick to replace reference to generic arguments: $'T
                    if Naming.genericPlaceholderRegex.IsMatch(macro)
                    then
                        let genArgs = matchGenericParams com ctx meth (typArgs, methTypArgs) |> Map
                        let genInfo = { makeGeneric=false; genericAvailability=ctx.genericAvailability }
                        Naming.genericPlaceholderRegex.Replace(macro, fun m ->
                            match genArgs.TryFind m.Groups.[1].Value with
                            | Some t ->
                                makeType com ctx t |> makeTypeRef ctx.fileName genInfo |> addExtraArg
                            | None ->
                                sprintf "Couldn't find generic argument %s requested by Emit expression: %s"
                                    m.Groups.[1].Value macro
                                |> attachRangeAndFile r ctx.fileName
                                |> Warning |> com.AddLog
                                m.Value)
                    else macro
                    // Trick to replace calls: $replace:Fable.Core.JsInterop.toPlainJsObj($2)
                    |> fun macro -> Naming.replacePlaceholderRegex.Replace(macro, fun m ->
                        try
                            let args =
                                m.Groups.[3].Value.Split(',')
                                |> Seq.map (fun x -> x.Trim().Substring(1) |> int |> List.item <| args)
                                |> Seq.toList
                            buildApplyInfo com ctx r Fable.Any Fable.Any
                                m.Groups.[1].Value m.Groups.[2].Value
                                Fable.Method ([],[],[],0) (None, args)
                            |> replace com
                            |> addExtraArg
                        with ex ->
                            "Couldn't replace call in Emit expression: " + macro
                            |> attachRangeAndFile r ctx.fileName
                            |> Warning |> com.AddLog
                            m.Value)
                    |> fun macro -> macro, (args@(List.rev extraArgs))
                Fable.Apply(Fable.Emit(macro) |> Fable.Value, args, Fable.ApplyMeth, typ, r)
                |> Some
            | :? FSharpType as emitFsType::restAttArgs when emitFsType.HasTypeDefinition ->
                let emitMethName =
                    match restAttArgs with
                    | [:? string as emitMethName] -> emitMethName
                    | _ -> "Emit" // Default
                try
                    let emitInstance = getEmitter emitFsType.TypeDefinition
                    let emitMeth = emitInstance.GetType().GetMethod(emitMethName)
                    let applyInfo =
                        buildApplyInfoFrom com ctx r typ
                            (typArgs, methTypArgs) (callee, args) meth
                    emitMeth.Invoke(emitInstance, [|com; applyInfo|]) |> unbox |> Some
                with
                | ex -> let msg = sprintf "Cannot build instance of type %s or it doesn't contain an appropriate %s method"
                                    emitFsType.TypeDefinition.DisplayName emitMethName |> attachRange r
                        Exception(msg + ": " + ex.Message, ex) |> raise
            | _ -> "EmitAttribute must receive a string or Type argument" |> attachRange r |> failwith
        | _ -> None

    let (|Imported|_|) com ctx r typ (typArgs, methTypArgs) (args: Fable.Expr list)
                        (meth: FSharpMemberOrFunctionOrValue) =
        let srcFile = (getEntityLocation meth.EnclosingEntity).FileName
        meth.Attributes
        |> Seq.choose (makeDecorator com)
        |> tryImported meth.CompiledName srcFile ctx.fileName
        |> function
            | Some expr ->
                match meth with
                // Allo combination of Import and Emit attributes
                | Emitted com ctx r typ (typArgs, methTypArgs) (None, expr::args) emitted ->
                    emitted
                | _ ->
                    match getMemberKind meth with
                    | Fable.Getter | Fable.Field -> expr
                    | Fable.Setter -> Fable.Set (expr, None, args.Head, r)
                    | Fable.Constructor
                    | Fable.Method -> Fable.Apply(expr, args, Fable.ApplyMeth, typ, r)
                |> Some
            | None -> None

    let (|Inlined|_|) (com: IFableCompiler) (ctx: Context) r (typArgs, methTypArgs)
                      (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        if not(isInline meth) then None else
        match com.TryGetInlineExpr meth with
        | Some (vars, fsExpr) ->
            let args = match callee with Some x -> x::args | None -> args
            let ctx =
                (ctx, vars, args) |||> Seq.fold2 (fun ctx var arg ->
                    { ctx with scope = (Some var, arg)::ctx.scope })
            let resolvedCtx =
                { ctx with typeArgs = matchGenericParams com ctx meth (typArgs, methTypArgs) }
            com.Transform resolvedCtx fsExpr |> Some
        | None ->
            FableError(meth.FullName + " is inlined but is not reachable. " +
                "If it belongs to an external project try removing inline modifier.", ?range=r) |> raise

    let passGenerics com ctx r (typArgs, methTypArgs) meth =
        let genInfo = { makeGeneric=true; genericAvailability=ctx.genericAvailability }
        matchGenericParams com ctx meth (typArgs, methTypArgs)
        |> List.map (fun (genName, FableType com ctx typ) ->
            match typ with
            | Fable.GenericParam name when not ctx.genericAvailability ->
                ("An unresolved generic argument ('" + name + ") is being passed " +
                 "to a function with `PassGenericsAttribute`. This will likely fail " +
                 "at runtime. Try adding `PassGenericsAttribute` to the calling method " +
                 "or using concrete types.")
                |> attachRangeAndFile r ctx.fileName
                |> Warning |> com.AddLog
            | _ -> ()
            genName, makeTypeRef ctx.fileName genInfo typ)
        |> makeJsObject None

    let makeCallFrom (com: IFableCompiler) ctx r typ
                     (meth: FSharpMemberOrFunctionOrValue)
                     (typArgs, methTypArgs) callee args =
        let argTypes = getArgTypes com meth.CurriedParameterGroups
        let args =
            if hasRestParams meth then
                let args = List.rev args
                match args.Head with
                | Fable.Value(Fable.ArrayConst(Fable.ArrayValues items, _)) ->
                    (List.rev args.Tail)@items
                | _ ->
                    (Fable.Spread args.Head |> Fable.Value)::args.Tail |> List.rev
            else
                if hasAtt Atts.passGenerics meth.Attributes
                then
                    let genArgs = [passGenerics com ctx r (typArgs, methTypArgs) meth]
                    match args with
                    | [arg] when arg.Type = Fable.Unit -> genArgs
                    | args -> args@genArgs
                else args
        match meth with
        (** -Check for replacements, emits... *)
        | Imported com ctx r typ (typArgs, methTypArgs) args imported -> imported
        | Emitted com ctx r typ (typArgs, methTypArgs) (callee, args) emitted -> emitted
        | Replaced com ctx r typ (typArgs, methTypArgs) (callee, args) replaced -> replaced
        | Inlined com ctx r (typArgs, methTypArgs) (callee, args) expr -> expr
        (** -If the call is not resolved, then: *)
        | _ ->
            let methName = sanitizeMethodName meth
        (**     *Check if this an extension *)
            match meth.IsExtensionMember, callee with
            | true, Some callee ->
                let typRef = makeTypeFromDef com ctx meth.EnclosingEntity []
                             |> makeNonGenTypeRef ctx.fileName
                let methName =
                    let ent = makeEntity com ctx meth.EnclosingEntity
                    let loc = if meth.IsInstanceMember then Fable.InstanceLoc else Fable.StaticLoc
                    ent.TryGetMember(methName, getMemberKind meth, loc, argTypes)
                    |> function Some m -> m.OverloadName | None -> methName
                let ext = makeGet r Fable.Any typRef (makeConst methName)
                let bind = Fable.Emit("$0.bind($1)($2...)") |> Fable.Value
                Fable.Apply (bind, ext::callee::args, Fable.ApplyMeth, typ, r)
            | _ ->
                let callee =
                    match callee with
                    | Some callee -> callee
                    | None -> makeTypeFromDef com ctx meth.EnclosingEntity []
                              |> makeNonGenTypeRef ctx.fileName
        (**     *Check if this a getter or setter  *)
                match getMemberKind meth with
                | Fable.Getter | Fable.Field ->
                    match tryGetBoundExpr ctx r meth with
                    | Some e -> e
                    | _ -> makeGetFrom com ctx r typ callee (makeConst methName)
                | Fable.Setter ->
                    match tryGetBoundExpr ctx r meth with
                    | Some e -> Fable.Set (e, None, args.Head, r)
                    | _ -> Fable.Set (callee, Some (makeConst methName), args.Head, r)
        (**     *Check if this is an implicit constructor *)
                | Fable.Constructor ->
                    Fable.Apply (callee, args, Fable.ApplyCons, typ, r)
        (**     *If nothing of the above applies, call the method normally *)
                | Fable.Method as kind ->
                    match tryGetBoundExpr ctx r meth with
                    | Some e -> e
                    | _ ->
                        let methName =
                            let ent = makeEntity com ctx meth.EnclosingEntity
                            ent.TryGetMember(methName, kind, getMemberLoc meth, argTypes)
                            |> function Some m -> m.OverloadName | None -> methName
                        let calleeType = Fable.Function(argTypes, typ)
                        makeGet r calleeType callee (makeConst methName)
                    |> fun m -> Fable.Apply (m, args, Fable.ApplyMeth, typ, r)

    let wrapInLambda (com: IFableCompiler) ctx r typ (meth: FSharpMemberOrFunctionOrValue) =
        let arity =
            match typ with
            | Fable.Function(args,_) -> args.Length
            | _ -> failwithf "Expecting a function value but got %s" meth.FullName
        let lambdaArgs =
            [for i=1 to arity do yield com.GetUniqueVar() |> makeIdent]
        lambdaArgs
        |> List.map (Fable.IdentValue >> Fable.Value)
        |> makeCallFrom com ctx r typ meth ([],[]) None
        |> makeLambdaExpr lambdaArgs

    let makeThisRef _com (ctx: Context) (v: FSharpMemberOrFunctionOrValue option) =
        match ctx.thisAvailability with
        | ThisAvailable -> Fable.Value Fable.This
        | ThisCaptured(currentThis, capturedThis) ->
            match v with
            | Some v when currentThis = v ->
                Fable.Value Fable.This
            | Some v ->
                capturedThis |> List.pick (function
                    | Some fsRef, ident when v = fsRef -> Some ident
                    | Some _, _ -> None
                    // The last fsRef of capturedThis must be None
                    // (the unknown `this` ref outside nested object expressions),
                    // so this means we've reached the end of the list.
                    | None, ident -> Some ident)
                |> Fable.IdentValue |> Fable.Value
            | None ->
                capturedThis |> List.last |> snd
                |> Fable.IdentValue |> Fable.Value
        // TODO: This shouldn't happen, throw exception?
        | ThisUnavailable -> Fable.Value Fable.This

    let makeValueFrom com ctx r typ role (v: FSharpMemberOrFunctionOrValue) =
        if not v.IsModuleValueOrMember
        then
            if typ = Fable.Unit
            then Fable.Value Fable.Null
            else match role with
                 | AppliedArgument -> consumeBoundExpr ctx r v
                 | _ -> getBoundExpr ctx r v
        // External entities contain functions that will be replaced,
        // when they appear as a stand alone values, they must be wrapped in a lambda
        elif isReplaceCandidate com v.EnclosingEntity
        then wrapInLambda com ctx r typ v
        else
            match v with
            | Emitted com ctx r typ ([], []) (None, []) emitted -> emitted
            | Imported com ctx r typ ([], []) [] imported -> imported
            | Try (tryGetBoundExpr ctx r) e -> e
            | _ ->
                let typeRef =
                    makeTypeFromDef com ctx v.EnclosingEntity []
                    |> makeNonGenTypeRef ctx.fileName
                Fable.Apply (typeRef, [makeConst v.CompiledName], Fable.ApplyGet, typ, r)
