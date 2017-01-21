namespace Fable.FSharp2Fable

open System
open System.Collections.Generic
#if !FABLE_COMPILER
open System.Reflection
#endif
open System.Text.RegularExpressions
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.AST.Fable.Util

#if DOTNETCORE && !FABLE_COMPILER
[<AutoOpen>]
module ReflectionAdapters =
    type System.Reflection.Assembly with
        static member LoadFrom(filePath: string) =
            let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
            globalLoadContext.LoadFromAssemblyPath(filePath)
#endif

type DecisionTarget =
    | TargetRef of Fable.Ident
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type ThisAvailability =
    | ThisUnavailable
    | ThisAvailable
    // Object expressions must capture the `this` reference and
    // they can also be nested (see makeThisRef and the ObjectExpr pattern)
    | ThisCaptured
        of currentThis: FSharpMemberOrFunctionOrValue option
        * capturedThis: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list

type MemberInfo =
    { isInstance: bool
    ; passGenerics: bool }

type Context =
    { fileName: string
    ; enclosingEntity: Fable.Entity
    ; scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
    ; scopedInlines: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
    ; typeArgs: (string * FSharpType) list
    ; decisionTargets: Map<int, DecisionTarget>
    ; baseClass: string option
    ; thisAvailability: ThisAvailability
    ; genericAvailability: bool
    ; isDelegate: bool }
    static member Create(fileName, enclosingModule) =
        { fileName = fileName
        ; enclosingEntity = enclosingModule
        ; scope = []
        ; scopedInlines = []
        ; typeArgs = []
        ; decisionTargets = Map.empty<_,_>
        ; baseClass = None
        ; thisAvailability = ThisUnavailable
        ; genericAvailability = false
        ; isDelegate = false }

type Role =
    | AppliedArgument
    // For now we're only interested in applied arguments
    | UnknownRole

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract IsReplaceCandidate: FSharpEntity -> bool
    abstract TryGetInternalFile: FSharpEntity -> string option
    abstract GetEntity: FSharpEntity -> Fable.Entity
    abstract TryGetInlineExpr: FSharpMemberOrFunctionOrValue -> (Dictionary<FSharpMemberOrFunctionOrValue,int> * FSharpExpr) option
    abstract AddInlineExpr: string -> (Dictionary<FSharpMemberOrFunctionOrValue,int> * FSharpExpr) -> unit
    abstract AddUsedVarName: string -> unit
    abstract ReplacePlugins: (string*IReplacePlugin) list

module Atts =
    let abstractClass = typeof<AbstractClassAttribute>.FullName
    let compiledName = typeof<CompiledNameAttribute>.FullName
    let emit = typeof<Fable.Core.EmitAttribute>.FullName
    let import = typeof<Fable.Core.ImportAttribute>.FullName
    let global_ = typeof<Fable.Core.GlobalAttribute>.FullName
    let erase = typeof<Fable.Core.EraseAttribute>.FullName
    let pojo = typeof<Fable.Core.PojoAttribute>.FullName
    let stringEnum = typeof<Fable.Core.StringEnumAttribute>.FullName
    let keyValueList = typeof<Fable.Core.KeyValueListAttribute>.FullName
    let passGenerics = typeof<Fable.Core.PassGenericsAttribute>.FullName
    let mutatingUpdate = typeof<Fable.Core.MutatingUpdateAttribute>.FullName

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

    // TODO: Report bug in FCS repo, when ent.IsNamespace, FullName doesn't work.
    let getEntityFullName (ent: FSharpEntity) =
        if ent.IsNamespace
        then match ent.Namespace with Some ns -> ns + "." + ent.CompiledName | None -> ent.CompiledName
        else defaultArg ent.TryFullName ent.CompiledName

    let private genArgsCountRegex = Regex("`\d+")

    let sanitizeEntityName (ent: FSharpEntity) =
        genArgsCountRegex.Replace(ent.CompiledName, "")

    let sanitizeEntityFullName (ent: FSharpEntity) =
        genArgsCountRegex.Replace(getEntityFullName ent, "")

    let tryFindAtt f (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName ->
                if f fullName then Some att else None
            | None -> None)

    let hasAtt name atts =
        atts |> tryFindAtt ((=) name) |> Option.isSome

    let tryDefinition (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition then Some typ.TypeDefinition else None

    // Sometimes accessing `EnclosingEntity` throws an error (e.g. compiler generated
    // methods as in #237) so this prevents uncaught exceptions
    let tryEnclosingEntity (meth: FSharpMemberOrFunctionOrValue) =
        try Some meth.EnclosingEntity
        with _ -> None

    let isModuleMember (meth: FSharpMemberOrFunctionOrValue) =
        try meth.EnclosingEntity.IsFSharpModule
        // TODO: Maybe this should be true as `EnclosingEntity` only
        // fails with module members generated by the compiler
        with _ -> false

    let isInline (meth: FSharpMemberOrFunctionOrValue) =
        match meth.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline -> true

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

    let belongsToInterfaceOrImportedEntity (meth: FSharpMemberOrFunctionOrValue) =
        // TODO: Temporary HACK to fix #577
        if meth.FullName.StartsWith("Fable.Import.Node") then true else
        match tryEnclosingEntity meth with
        | Some ent ->
            meth.IsExplicitInterfaceImplementation
            || ent.IsInterface
            || (ent.Attributes
                |> tryFindAtt (fun name ->
                    name = Atts.import || name = Atts.global_)
                |> Option.isSome)
        | None -> false

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

    let getMethLocation (meth: FSharpMemberOrFunctionOrValue) =
        match meth.ImplementationLocation with
        | Some loc -> loc
        | None -> meth.DeclarationLocation

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt ((=) Atts.compiledName)
        |> function
            | Some name -> name.ConstructorArguments.[0] |> snd |> string
            | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeStrConst

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
        else
        match tryEnclosingEntity meth with
        | Some ent when ent.IsInterface -> Some ent
        | _ -> None

    let getMemberLoc (meth: FSharpMemberOrFunctionOrValue) =
        if not meth.IsInstanceMember && not meth.IsImplicitConstructor
        then Fable.StaticLoc
        else tryGetInterfaceFromMethod meth
             |> Option.map (sanitizeEntityFullName >> Fable.InterfaceLoc)
             |> defaultArg <| Fable.InstanceLoc

    let getMemberKind (meth: FSharpMemberOrFunctionOrValue) =
        let argCount = lazy(getArgCount meth)
        let ent = tryEnclosingEntity meth
        // `.EnclosingEntity` only fails for compiler generated module members
        if ent.IsNone || (ent.Value.IsFSharpModule) then
            if meth.CurriedParameterGroups.Count = 0
                && meth.GenericParameters.Count = 0
            then Fable.Field
            else Fable.Method
        elif meth.IsImplicitConstructor then Fable.Constructor
        elif meth.IsPropertyGetterMethod && argCount.Value = 0 then Fable.Getter
        elif meth.IsPropertySetterMethod && argCount.Value = 1 then Fable.Setter
        else Fable.Method

    let sanitizeMethodName (meth: FSharpMemberOrFunctionOrValue) =
        let isInterface =
            meth.IsExplicitInterfaceImplementation
            || (tryEnclosingEntity meth |> Option.toBool (fun ent -> ent.IsInterface))
        if isInterface then meth.DisplayName else
        match getMemberKind meth with
        | Fable.Getter | Fable.Setter -> meth.DisplayName
        | _ -> meth.CompiledName

    let hasRestParams (meth: FSharpMemberOrFunctionOrValue) =
        if meth.CurriedParameterGroups.Count <> 1 then false else
        let args = meth.CurriedParameterGroups.[0]
        args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

    let hasPassGenericsAtt (meth: FSharpMemberOrFunctionOrValue) =
        match hasAtt Atts.passGenerics meth.Attributes with
        | true when hasRestParams meth ->
            let r = getMethLocation meth |> makeRange
            FableError(Atts.passGenerics + " is not compatible with ParamArrayAttribute", r) |> raise
        | result -> result

    let removeOmittedOptionalArguments (meth: FSharpMemberOrFunctionOrValue) args =
        let rec removeArgs (args: (Fable.Expr*FSharpParameter) list) =
            match args with
            | (arg, p)::rest ->
                match arg with
                | (Fable.Wrapped(Fable.Value Fable.Null, _) | Fable.Value Fable.Null)
                    when p.IsOptionalArg -> removeArgs rest
                | _ -> args
            | _ -> args
        if meth.CurriedParameterGroups.Count <> 1
        then args
        elif meth.CurriedParameterGroups.[0].Count = 1
                && isUnit meth.CurriedParameterGroups.[0].[0].Type
        then []
        elif meth.CurriedParameterGroups.[0].Count <> List.length args
        then args
        else
            List.zip args (Seq.toList meth.CurriedParameterGroups.[0])
            |> List.rev |> removeArgs |> List.rev |> List.map fst
    let rec deepExists f (expr: FSharpExpr) =
        if f expr
        then true
        else List.exists (deepExists f) expr.ImmediateSubExpressions

module Patterns =
    open BasicPatterns
    open Helpers

    let inline (|Rev|) x = List.rev x
    let inline (|AsArray|) x = Array.ofSeq x
    let inline (|Transform|) (com: IFableCompiler) = com.Transform
    let inline (|FieldName|) (fi: FSharpField) = fi.Name
    let inline (|ExprType|) (expr: Fable.Expr) = expr.Type
    let inline (|EntityKind|) (ent: Fable.Entity) = ent.Kind

    let inline (|NonAbbreviatedType|) (t: FSharpType) =
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

    let (|JsThis|_|) = function
        | Call(None, jsThis, _, _, [])
            when jsThis.FullName.StartsWith("Fable.Core.JsInterop.jsThis") ->
            Some JsThis
        | _ -> None

    let (|CurriedLambda|_|) fsExpr =
        let rec curriedLambda acc = function
            | Lambda(arg, body) -> curriedLambda (arg::acc) body
            | e -> acc, e
        match curriedLambda [] fsExpr with
        | [], _ -> None
        | args, expr -> Some(List.rev args, expr)

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
        | "System.Decimal" -> Some Decimal
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.int" _ -> Some Int32
        | Naming.StartsWith "Microsoft.FSharp.Core.float32" _ -> Some Float32
        | Naming.StartsWith "Microsoft.FSharp.Core.float" _ -> Some Float64
        | _ -> None

    let (|ExtendedNumberKind|_|) = function
        | "System.Int64" -> Some Int64
        | "System.UInt64" -> Some UInt64
        | "System.Numerics.BigInteger" -> Some BigInt
        | _ -> None

    let (|Switch|_|) fsExpr =
        let isStringOrNumber (NonAbbreviatedType typ) =
            if not typ.HasTypeDefinition then false else
            match typ.TypeDefinition.TryFullName with
            | Some("System.String") -> true
            | Some(NumberKind kind) -> true
            | _ when typ.TypeDefinition.IsEnum -> true
            | _ -> false
        let rec makeSwitch map matchValue e =
            // let addCase map (idx: int) (case: obj) =
            //     match Map.tryFind idx map with
            //     | Some cases -> Map.add idx (case::cases) map
            //     | None -> Map.add idx [case] map
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

    let (|OptionUnion|ListUnion|ErasedUnion|KeyValueUnion|StringEnum|PojoUnion|OtherType|) (typ: FSharpType) =
        match tryDefinition typ with
        | None -> OtherType
        | Some tdef ->
            match defaultArg tdef.TryFullName tdef.CompiledName with
            | "Microsoft.FSharp.Core.FSharpOption`1" -> OptionUnion
            | "Microsoft.FSharp.Collections.FSharpList`1" -> ListUnion
            | _ ->
                tdef.Attributes
                |> Seq.choose (fun att -> att.AttributeType.TryFullName)
                |> Seq.tryPick (fun name ->
                    if name = Atts.erase then Some ErasedUnion
                    elif name = Atts.stringEnum then Some StringEnum
                    elif name = Atts.keyValueList then Some KeyValueUnion
                    elif name = Atts.pojo then Some PojoUnion
                    else None)
                |> defaultArg <| OtherType

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

    let rec getBaseClass (com: IFableCompiler) (tdef: FSharpEntity) =
        match tdef.BaseType with
        | Some(TypeDefinition tdef) when tdef.FullName <> "System.Object" ->
            let typeRef = makeTypeFromDef com [] tdef [] |> makeNonGenTypeRef com
            Some (sanitizeEntityFullName tdef, typeRef)
        | _ -> None

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
        | args -> List.concat args |> List.map (fun x -> makeType com [] x.Type)

    and makeOriginalCurriedType com (args: IList<IList<FSharpParameter>>) returnType =
        let tys = args |> Seq.map (fun tuple ->
            let tuple = tuple |> Seq.map (fun t -> makeType com [] t.Type)
            match List.ofSeq tuple with
            | [singleArg] -> singleArg
            | args -> Fable.Tuple(args) )
        Seq.append tys [returnType] |> Seq.reduceBack (fun a b -> Fable.Function([a], b))

    and getMembers com (tdef: FSharpEntity) =
        let isAbstract =
            hasAtt Atts.abstractClass tdef.Attributes
        let isDefaultImplementation (x: FSharpMemberOrFunctionOrValue) =
            isAbstract && x.IsOverrideOrExplicitInterfaceImplementation && not x.IsExplicitInterfaceImplementation
        let existsInterfaceMember name =
            tdef.AllInterfaces
            |> Seq.exists (fun ifc ->
                if not ifc.HasTypeDefinition then false else
                ifc.TypeDefinition.MembersFunctionsAndValues
                |> Seq.exists (fun m -> m.DisplayName = name))
        let members =
            tdef.MembersFunctionsAndValues
            |> Seq.filter (fun x ->
                // Discard overrides in abstract classes (that is, default implementations)
                // to prevent confusing them with overloads (see #505)
                not(isDefaultImplementation x)
                // Property members that are no getter nor setter don't actually get implemented
                && not(x.IsProperty && not(x.IsPropertyGetterMethod || x.IsPropertySetterMethod)))
            |> Seq.map (fun meth -> sanitizeMethodName meth, getMemberKind meth, getMemberLoc meth, meth)
            |> Seq.toArray
        let getMembers' loc (tdef: FSharpEntity) =
            members
            |> Seq.filter (fun (_, _, mloc, _) -> sameMemberLoc loc mloc)
            |> Seq.groupBy (fun (name, kind, _, _) -> name, kind)
            |> Seq.collect (fun ((name, kind), AsArray members) ->
                let isOverloaded =
                    if tdef.IsInterface then false else
                    match loc with
                    | Fable.InterfaceLoc _ -> false
                    | Fable.InstanceLoc -> members.Length > 1 || existsInterfaceMember name
                    | Fable.StaticLoc -> members.Length > 1
                members |> Array.mapi (fun i (_, _, loc, meth) ->
                    let argTypes = getArgTypes com meth.CurriedParameterGroups
                    let returnType = makeType com [] meth.ReturnParameter.Type
                    let originalTyp = makeOriginalCurriedType com meth.CurriedParameterGroups returnType
                    let overloadIndex = if isOverloaded then Some i else None
                    makeMethodFrom com name kind loc argTypes returnType originalTyp overloadIndex meth
            ))
            |> Seq.toList
        let instanceMembers = getMembers' Fable.InstanceLoc tdef
        let staticMembers = getMembers' Fable.StaticLoc tdef
        let interfaceMembers = getMembers' (Fable.InterfaceLoc "") tdef
        instanceMembers@interfaceMembers@staticMembers

    /// Don't use this method directly, use IFableCompiler.GetEntity instead
    and makeEntity (com: IFableCompiler) (tdef: FSharpEntity): Fable.Entity =
        let makeFields (tdef: FSharpEntity) =
            tdef.FSharpFields
            |> Seq.map (fun x -> x.Name, makeType com [] x.FieldType)
            |> Seq.toList
        let makeProperties (tdef: FSharpEntity) =
            tdef.MembersFunctionsAndValues
            |> Seq.choose (fun x ->
                if not x.IsPropertyGetterMethod then None else
                match makeType com [] x.FullType with
                | Fable.Function(_, returnType) ->
                    Some(x.DisplayName, returnType)
                | _ -> None)
            |> Seq.toList
        let makeCases (tdef: FSharpEntity) =
            tdef.UnionCases |> Seq.map (fun x ->
                x.Name, [for fi in x.UnionCaseFields do yield makeType com [] fi.FieldType])
            |> Map
        let getKind () =
            if tdef.IsInterface then Fable.Interface
            elif tdef.IsFSharpUnion then makeCases tdef |> Fable.Union
            elif tdef.IsFSharpRecord || tdef.IsValueType then makeFields tdef |> Fable.Record
            elif tdef.IsFSharpExceptionDeclaration then makeFields tdef |> Fable.Exception
            elif tdef.IsFSharpModule || tdef.IsNamespace then Fable.Module
            else Fable.Class(getBaseClass com tdef, makeProperties tdef)
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
        Fable.Entity (Lazy<_>(fun () -> getKind()), com.TryGetInternalFile tdef,
            sanitizeEntityFullName tdef, Lazy<_>(fun () -> getMembers com tdef),
            genParams, infcs, decs, tdef.Accessibility.IsPublic || tdef.Accessibility.IsInternal)

    and makeTypeFromDef (com: IFableCompiler) typeArgs (tdef: FSharpEntity)
                        (genArgs: seq<FSharpType>) =
        let fullName = defaultArg tdef.TryFullName tdef.CompiledName
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
        // Array
        elif tdef.IsArrayType
        then Fable.Array(Seq.head genArgs |> makeType com typeArgs)
        // Enum
        elif tdef.IsEnum
        then Fable.Enum fullName
        // Delegate
        elif tdef.IsDelegate
        then
            if fullName.StartsWith("System.Action")
            then
                if Seq.length genArgs = 1
                then [Seq.head genArgs |> makeType com typeArgs], Fable.Unit
                else [], Fable.Unit
                |> Fable.Function
            elif fullName.StartsWith("System.Func")
            then
                match Seq.length genArgs with
                | 0 -> [], Fable.Unit
                | 1 -> [], Seq.head genArgs |> makeType com typeArgs
                | c -> Seq.take (c-1) genArgs |> Seq.map (makeType com typeArgs) |> Seq.toList,
                        Seq.last genArgs |> makeType com typeArgs
                |> Fable.Function
            else
            try
                let argTypes =
                    tdef.FSharpDelegateSignature.DelegateArguments
                    |> Seq.map (snd >> makeType com typeArgs) |> Seq.toList
                let retType =
                    makeType com typeArgs tdef.FSharpDelegateSignature.DelegateReturnType
                Fable.Function(argTypes, retType)
            with _ -> Fable.Function([Fable.Any], Fable.Any)
        // Object
        elif fullName = "System.Object"
        then Fable.Any
        else
        match fullName with
        | "System.Boolean" -> Fable.Boolean
        | "System.Char" -> Fable.Char
        | "System.String" | "System.Guid" -> Fable.String
        | "Microsoft.FSharp.Core.Unit" -> Fable.Unit
        | "Microsoft.FSharp.Core.FSharpOption`1" ->
            let t = Seq.tryHead genArgs |> Option.map (makeType com typeArgs)
            Fable.Option(defaultArg t Fable.Any)
        | "System.Collections.Generic.List`1" ->
            let t = Seq.tryHead genArgs |> Option.map (makeType com typeArgs)
            Fable.Array(defaultArg t Fable.Any)
        | NumberKind kind -> Fable.Number kind
        | ExtendedNumberKind kind -> Fable.ExtendedNumber kind
        | _ ->
            // Check erased types
            tdef.Attributes
            |> Seq.choose (fun att -> att.AttributeType.TryFullName)
            |> Seq.tryPick (fun name ->
                if name = Atts.stringEnum
                then Some Fable.String
                elif name = Atts.erase
                    || name = Atts.keyValueList
                    || name = Atts.pojo
                then Some Fable.Any
                else None)
            |> defaultArg <|
                // Declared Type
                Fable.DeclaredType(com.GetEntity tdef,
                    genArgs |> Seq.map (makeType com typeArgs) |> Seq.toList)

    and makeType (com: IFableCompiler) typeArgs (NonAbbreviatedType t) =
        let makeGenArgs (genArgs: #seq<FSharpType>) =
            Seq.map (makeType com typeArgs) genArgs |> Seq.toList
        let resolveGenParam (genParam: FSharpGenericParameter) =
            match typeArgs |> List.tryFind (fun (name,_) -> name = genParam.Name) with
            // Clear typeArgs to prevent infinite recursion
            | Some (_,typ) -> makeType com [] typ
            | None -> Fable.GenericParam genParam.Name
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
        // Tuple
        elif t.IsTupleType
        then Fable.Tuple(makeGenArgs t.GenericArguments)
        // Funtion
        elif t.IsFunctionType
        then
            let argType = makeType com typeArgs t.GenericArguments.[0]
            let returnType = makeType com typeArgs t.GenericArguments.[1]
            Fable.Function([argType], returnType)
        elif t.HasTypeDefinition
        then makeTypeFromDef com typeArgs t.TypeDefinition t.GenericArguments
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    let inline (|FableEntity|) (com: IFableCompiler) e = com.GetEntity e
    let inline (|FableType|) com (ctx: Context) t = makeType com ctx.typeArgs t

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
        bindIdent com ctx (makeType com ctx.typeArgs fsRef.FullType) (Some fsRef) fsRef.CompiledName

    let (|BindIdent|) = bindIdentFrom

    /// Get corresponding identifier to F# value in current scope
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

module Util =
    open Helpers
    open Patterns
    open Types
    open Identifiers

    let validateGenArgs (ctx: Context) r (genParams: FSharpGenericParameter seq) (typArgs: FSharpType seq) =
        let fail typName genName =
            let typName = defaultArg typName ""
            sprintf "Type %s passed as generic param '%s must be decorated with %s or be `obj`" typName genName Atts.pojo
            |> fun msg -> FableError(msg, ?range=r, file=ctx.fileName) |> raise
        if Seq.length genParams = Seq.length typArgs then
            Seq.zip genParams typArgs
            |> Seq.iter (fun (par, arg) ->
                if hasAtt Atts.pojo par.Attributes then
                    match tryDefinition arg with
                    | Some argDef when argDef.TryFullName = Some "System.Object" -> ()
                    | Some argDef when hasAtt Atts.pojo argDef.Attributes -> ()
                    | None when arg.IsGenericParameter
                        && hasAtt Atts.pojo arg.GenericParameter.Attributes -> ()
                    | Some argDef -> fail (Some argDef.DisplayName) par.Name
                    | None -> fail None par.Name
                )

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
            then { ctx with genericAvailability=true }, thisArg, args, [makeIdent Naming.genArgsIdent]
            else ctx, thisArg, args, []

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
            (atts, typArgs, methTypArgs, lambdaArgArity) (callee, args): Fable.ApplyInfo =
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
            calleeTypeArgs = typArgs |> List.map (makeType com ctx.typeArgs)
            methodTypeArgs = methTypArgs |> List.map (makeType com ctx.typeArgs)
            genericAvailability = ctx.genericAvailability
            lambdaArgArity = lambdaArgArity
        }

    let buildApplyInfoFrom com (ctx: Context) r typ
            (typArgs, methTypArgs)
            (callee, args)
            (owner: FSharpEntity option)
            (meth: FSharpMemberOrFunctionOrValue)
            : Fable.ApplyInfo =
        let lambdaArgArity =
            if meth.CurriedParameterGroups.Count > 0
                && meth.CurriedParameterGroups.[0].Count > 0
            then countFuncArgs meth.CurriedParameterGroups.[0].[0].Type
            else 0
        let ownerType, ownerFullName =
            match owner with
            | Some ent -> makeTypeFromDef com ctx.typeArgs ent [], sanitizeEntityFullName ent
            | None -> Fable.Any, "System.Object"
        buildApplyInfo com ctx r typ ownerType ownerFullName (sanitizeMethodName meth) (getMemberKind meth)
            (meth.Attributes, typArgs, methTypArgs, lambdaArgArity) (callee, args)

    let tryPlugin (com: IFableCompiler) (info: Fable.ApplyInfo) =
        com.ReplacePlugins
        |> Plugins.tryPlugin info.range (fun p -> p.TryReplace com info)

    let (|Plugin|_|) (com: IFableCompiler) (info: Fable.ApplyInfo) _ =
        tryPlugin com info

    let tryReplace (com: IFableCompiler) (ent: FSharpEntity option) (info: Fable.ApplyInfo) =
        let isInterface = function
            | Fable.DeclaredType(ent, _) when ent.Kind = Fable.Interface -> true
            | _ -> false
        match ent with
        | Some ent when com.IsReplaceCandidate ent ->
            match Replacements.tryReplace com info with
            | Some _ as repl -> repl
            | None when isInterface info.ownerType -> None
            | None -> FableError("Cannot find replacement for " +
                        info.ownerFullName + "." + info.methodName, ?range=info.range) |> raise
        | _ -> None

    let (|Replaced|_|) (com: IFableCompiler) i owner (meth: FSharpMemberOrFunctionOrValue) =
        tryReplace com owner i

    let matchGenericParams com ctx (meth: FSharpMemberOrFunctionOrValue) (typArgs, methTypArgs) =
        let (|ResolveGeneric|) ctx (t: FSharpType) =
            if not t.IsGenericParameter then t else
            let genParam = t.GenericParameter
            ctx.typeArgs |> List.tryFind (fun (name,_) -> name = genParam.Name)
            |> function Some (_,t) -> t | None -> t
        // Seems that, contrary to what I believed, `meth.GenericParameters`
        // contains both the type and meth generic arguments, so this first
        // folding is not necessary
        // let genArgs =
        //     if meth.IsModuleValueOrMember then
        //         ([], meth.EnclosingEntity.GenericParameters, typArgs)
        //         |||> Seq.fold2 (fun acc genPar (ResolveGeneric ctx t) -> acc@[genPar.Name, t])
        //     else []
        ([], meth.GenericParameters, typArgs@methTypArgs)
        |||> Seq.fold2 (fun acc genPar (ResolveGeneric ctx t) -> (genPar.Name, t)::acc)
        |> List.rev

#if !FABLE_COMPILER
    let getEmitter =
        // Prevent ReflectionTypeLoadException
        // From http://stackoverflow.com/a/7889272
        let getTypes (asm: System.Reflection.Assembly) =
            let mutable types: Option<Type[]> = None
            try
                types <- Some(asm.GetTypes())
            with
            | :? ReflectionTypeLoadException as e -> types <- Some e.Types
            match types with
            | Some types -> types |> Seq.filter ((<>) null)
            | None -> Seq.empty
        let cache = Dictionary<string, obj>()
        fun (tdef: FSharpEntity) ->
            cache.GetOrAdd(tdef.QualifiedName, fun _ ->
                let filePath = tdef.Assembly.FileName.Value
                // The assembly is already loaded because it's being referenced
                // by the parsed code, so use `LoadFrom` which takes the copy in memory
                // Unlike `LoadFile`, see: http://stackoverflow.com/a/1477899
                let assembly = System.Reflection.Assembly.LoadFrom(filePath)
                let typ = getTypes assembly |> Seq.find (fun x ->
                    x.AssemblyQualifiedName = tdef.QualifiedName)
                System.Activator.CreateInstance(typ))
#endif

    let emittedGenericArguments com (ctx: Context) r meth (typArgs, methTypArgs)
                                macro (args: Fable.Expr list) =
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
                    makeType com ctx.typeArgs t |> makeTypeRef com genInfo |> addExtraArg
                | None ->
                    sprintf "Couldn't find generic argument %s requested by Emit expression: %s"
                        m.Groups.[1].Value macro
                    |> addWarning com ctx.fileName r
                    m.Value)
        else macro
        |> fun macro -> macro, args@(List.rev extraArgs)

    let (|Emitted|_|) com ctx r typ i (typArgs, methTypArgs) (callee, args)
                        (meth: FSharpMemberOrFunctionOrValue) =
        match meth.Attributes with
        | ContainsAtt Atts.emit attArgs ->
            match attArgs with
            | [:? string as macro] ->
                let args =
                    let args = List.map (makeDelegate com None) args
                    match callee with None -> args | Some c -> c::args
                let macro, args =
                    emittedGenericArguments com ctx r meth (typArgs, methTypArgs) macro args
                Fable.Apply(Fable.Emit(macro) |> Fable.Value, args, Fable.ApplyMeth, typ, r) |> Some
#if !FABLE_COMPILER
            | (:? FSharpType as emitFsType)::(:? string as emitMethName)::extraArg
                when emitFsType.HasTypeDefinition ->
                try
                    let emitInstance = getEmitter emitFsType.TypeDefinition
                    let emitMeth = emitInstance.GetType().GetMethod(emitMethName)
                    let args: obj[] =
                        match extraArg with
                        | [extraArg] -> [|com; i; extraArg|]
                        | _ -> [|com; i|]
                    emitMeth.Invoke(emitInstance, args) |> unbox |> Some
                with
                | :? AST.FableError as err -> raise err
                | ex -> let exMsg = if ex.GetType() = typeof<TargetInvocationException>
                                    then ex.InnerException.Message else ex.Message
                        sprintf "Error when invoking %s.%s"
                            emitFsType.TypeDefinition.DisplayName emitMethName
                        |> attachRange r |> fun msg -> Exception(msg + ": " + exMsg, ex) |> raise
#endif
            | _ -> "EmitAttribute must receive a string or Type argument" |> attachRange r |> failwith
        | _ -> None

    let (|Imported|_|) com ctx r typ i (typArgs, methTypArgs) (args: Fable.Expr list)
                        (meth: FSharpMemberOrFunctionOrValue) =
        meth.Attributes
        |> Seq.choose (makeDecorator com)
        |> tryImported (lazy sanitizeMethodName meth)
        |> function
            | Some expr ->
                match meth with
                // Allow combination of Import and Emit attributes
                | Emitted com ctx r typ i (typArgs, methTypArgs) (None, expr::args) emitted ->
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
            let ctx, assignments =
                ((ctx, []), vars, args)
                |||> Seq.fold2 (fun (ctx, assignments) var arg ->
                    // If an expression is referenced more than once, assign it
                    // to a temp var to prevent multiple evaluations
                    if var.Value > 1 then
                        let tmpVar = com.GetUniqueVar() |> makeIdent
                        let assign = Fable.VarDeclaration(tmpVar, arg, false)
                        let scope = (Some var.Key, Fable.Value(Fable.IdentValue tmpVar))::ctx.scope
                        { ctx with scope = scope }, (assign::assignments)
                    else
                        { ctx with scope = (Some var.Key, arg)::ctx.scope }, assignments
                )
            let typeArgs = matchGenericParams com ctx meth (typArgs, methTypArgs)
            let ctx =
                match callee with
                | Some callee -> {ctx with thisAvailability=ThisCaptured(None, [None, callee]); typeArgs=typeArgs}
                | None -> {ctx with typeArgs=typeArgs}
            let expr = com.Transform ctx fsExpr
            if List.isEmpty assignments
            then Some expr
            else makeSequential r (assignments@[expr]) |> Some
        | None ->
            FableError(meth.FullName + " is inlined but is not reachable. " +
                "If it belongs to an external project try removing inline modifier.", ?range=r) |> raise

    let passGenerics com ctx r (typArgs, methTypArgs) meth =
        let rec hasUnresolvedGenerics = function
            | Fable.GenericParam name -> Some name
            | Fable.Option genericArg -> hasUnresolvedGenerics genericArg
            | Fable.Array genericArg -> hasUnresolvedGenerics genericArg
            | Fable.Tuple genericArgs -> genericArgs |> Seq.tryPick hasUnresolvedGenerics
            | Fable.Function (argTypes, returnType ) -> returnType::argTypes |> Seq.tryPick hasUnresolvedGenerics
            | Fable.DeclaredType (_, genericArgs) -> genericArgs |> Seq.tryPick hasUnresolvedGenerics
            | _ -> None
        let genInfo = { makeGeneric=true; genericAvailability=ctx.genericAvailability }
        matchGenericParams com ctx meth (typArgs, methTypArgs)
        |> List.map (fun (genName, FableType com ctx typ) ->
            if not ctx.genericAvailability then
                match hasUnresolvedGenerics typ with
                | Some name ->
                    ("An unresolved generic argument ('" + name + ") is being passed " +
                     "to a function with `PassGenericsAttribute`. This will likely fail " +
                     "at runtime. Try adding `PassGenericsAttribute` to the calling method " +
                     "or using concrete types.")
                    |> addWarning com ctx.fileName r
                | None -> ()
            genName, makeTypeRef com genInfo typ)
        |> makeJsObject None

    let (|ExtensionMember|_|) com (ctx: Context) r typ (callee, args, argTypes) owner (meth: FSharpMemberOrFunctionOrValue) =
        match meth.IsExtensionMember, callee, owner with
        | true, Some callee, Some ent ->
            let typRef = makeTypeFromDef com ctx.typeArgs ent [] |> makeNonGenTypeRef com
            let methName =
                let methName = sanitizeMethodName meth
                let ent = com.GetEntity ent
                let loc = if meth.IsInstanceMember then Fable.InstanceLoc else Fable.StaticLoc
                match ent.TryGetMember(methName, getMemberKind meth, loc, argTypes) with
                | Some m -> m.OverloadName | None -> methName
            let ext = makeGet r Fable.Any typRef (makeStrConst methName)
            let bind = Fable.Emit("$0.bind($1)($2...)") |> Fable.Value
            Fable.Apply (bind, ext::callee::args, Fable.ApplyMeth, typ, r) |> Some
        | _ -> None

    let makeCallFrom (com: IFableCompiler) ctx r typ
                     (meth: FSharpMemberOrFunctionOrValue)
                     (typArgs, methTypArgs) callee args =
        validateGenArgs ctx r meth.GenericParameters methTypArgs
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
                let args = removeOmittedOptionalArguments meth args // See #231, #640
                if hasAtt Atts.passGenerics meth.Attributes
                then
                    let genArgs = [passGenerics com ctx r (typArgs, methTypArgs) meth]
                    match args with
                    | [arg] when arg.Type = Fable.Unit -> genArgs
                    | args -> args@genArgs
                else args
        let owner = tryEnclosingEntity meth
        let i = buildApplyInfoFrom com ctx r typ (typArgs, methTypArgs) (callee, args) owner meth
        match meth with
        (** -Check for replacements, emits... *)
        | Plugin com i replaced -> replaced
        | Imported com ctx r typ i (typArgs, methTypArgs) args imported -> imported
        | Emitted com ctx r typ i (typArgs, methTypArgs) (callee, args) emitted -> emitted
        | Replaced com i owner replaced -> replaced
        | Inlined com ctx r (typArgs, methTypArgs) (callee, args) expr -> expr
        | ExtensionMember com ctx r typ (callee, args, argTypes) owner expr -> expr
        | Try (tryGetBoundExpr ctx r) e ->
            match getMemberKind meth with
            | Fable.Getter | Fable.Field -> e
            | Fable.Setter -> Fable.Set (e, None, args.Head, r)
            // Constructors cannot be bound expressions
            | _ -> Fable.Apply(e, args, Fable.ApplyMeth, typ, r)
        (** -If the call is not resolved, then: *)
        | _ ->
            let callee =
                match callee, owner with
                | Some callee, _ -> callee
                | None, Some ent ->
                    makeTypeFromDef com ctx.typeArgs ent [] |> makeNonGenTypeRef com
                // Cases when tryEnclosingEntity returns None are rare (see #237)
                // Let's assume the method belongs to the current enclosing module
                | _ -> Fable.DeclaredType(ctx.enclosingEntity, []) |> makeNonGenTypeRef com
            let methName = sanitizeMethodName meth
    (**     *Check if this a getter or setter  *)
            match getMemberKind meth with
            | Fable.Getter | Fable.Field ->
                makeGetFrom com ctx r typ callee (makeStrConst methName)
            | Fable.Setter ->
                Fable.Set (callee, Some (makeStrConst methName), args.Head, r)
    (**     *Check if this is an implicit constructor *)
            | Fable.Constructor ->
                Fable.Apply (callee, args, Fable.ApplyCons, typ, r)
    (**     *If nothing of the above applies, call the method normally *)
            | Fable.Method as kind ->
                let applyMeth methName =
                    // let calleeType = Fable.Function(Some argTypes, typ)
                    let m = makeGet r Fable.Any callee (makeStrConst methName)
                    Fable.Apply(m, args, Fable.ApplyMeth, typ, r)
                if belongsToInterfaceOrImportedEntity meth
                then
                    if methName = ".ctor"
                    then Fable.Apply(callee, args, Fable.ApplyCons, typ, r)
                    else applyMeth methName
                else
                    match owner with
                    | Some ent ->
                        let ent = com.GetEntity ent
                        ent.TryGetMember(methName, kind, getMemberLoc meth, argTypes)
                        |> function Some m -> m.OverloadName | None -> methName
                        |> applyMeth
                    | None -> applyMeth methName

    let makeThisRef (com: ICompiler) (ctx: Context) r (v: FSharpMemberOrFunctionOrValue option) =
        match ctx.thisAvailability with
        | ThisAvailable -> Fable.Value Fable.This
        | ThisCaptured(currentThis, capturedThis) ->
            match v, currentThis with
            | Some v, Some currentThis when currentThis = v ->
                Fable.Value Fable.This
            | Some v, _ ->
                capturedThis |> List.pick (function
                    | Some fsRef, ident when v = fsRef -> Some ident
                    | Some _, _ -> None
                    // The last fsRef of capturedThis must be None
                    // (the unknown `this` ref outside nested object expressions),
                    // so this means we've reached the end of the list.
                    | None, ident -> Some ident)
            | None, _ ->
                capturedThis |> List.last |> snd
        | ThisUnavailable ->
            "`this` seems to be used in a context where it's not available, please check."
            |> addWarning com ctx.fileName r
            Fable.Value Fable.This

    let makeValueFrom com ctx r typ role (v: FSharpMemberOrFunctionOrValue) =
        if typ = Fable.Unit then Fable.Wrapped(Fable.Value Fable.Null, Fable.Unit) else
        let owner = tryEnclosingEntity v
        let i = buildApplyInfoFrom com ctx r typ ([], []) (None, []) owner v
        match v with
        | Plugin com i replaced -> replaced
        | Imported com ctx r typ i ([], []) [] imported -> imported
        | Emitted com ctx r typ i ([], []) (None, []) emitted -> emitted
        | Replaced com i owner replaced -> replaced
        | Try (tryGetBoundExpr ctx r) e ->
            match role, e with
            | AppliedArgument, Fable.Value(Fable.IdentValue i) ->
                i.Consume(); e
            | _ -> e
        | _ ->
            let typeRef =
                match owner with
                | Some ent -> makeTypeFromDef com ctx.typeArgs ent [] |> makeNonGenTypeRef com
                // Cases when tryEnclosingEntity returns None are rare (see #237)
                // Let's assume the value belongs to the current enclosing module
                | None -> Fable.DeclaredType(ctx.enclosingEntity, []) |> makeNonGenTypeRef com
            Fable.Apply (typeRef, [makeStrConst v.CompiledName], Fable.ApplyGet, typ, r)

    let makeDelegateFrom (com: IFableCompiler) ctx delegateType fsExpr =
        let ctx = { ctx with isDelegate = true}
        let fsExpr =
            let fullName t =
                tryDefinition t
                |> Option.bind (fun tdef -> tdef.TryFullName)
                |> defaultArg <| ""
            match fsExpr with
            // When System.Func has one single generic parameter and the argument
            // is a local reference (e.g. `System.Func<int>(f)`) the F# compiler
            // translates it as an application
            | BasicPatterns.Application(BasicPatterns.Value _ as e,_,[])
                when fullName delegateType = "System.Func`1" -> e
            | _ -> fsExpr
        let arity =
            match makeType com ctx.typeArgs delegateType with
            | Fable.Function([Fable.Unit],_) -> 0
            | Fable.Function(args,_) -> args.Length
            | _ ->
                "Cannot calculate arity of delegate, please report."
                |> addWarning com ctx.fileName (makeRangeFrom fsExpr)
                1
        let containsJsThis =
            fsExpr |> deepExists (function JsThis -> true | _ -> false)
        // If `this` is available, capture it to avoid conflicts (see #158)
        let capturedThis =
            match containsJsThis, ctx.thisAvailability with
            | false, _ -> None
            | true, ThisUnavailable -> None
            | true, ThisAvailable -> Some [None, com.GetUniqueVar() |> makeIdentExpr]
            | true, ThisCaptured(prevThis, prevVars) ->
                (prevThis, com.GetUniqueVar() |> makeIdentExpr)::prevVars |> Some
        let ctx =
            match capturedThis with
            | None -> ctx
            | Some(capturedThis) ->
                { ctx with thisAvailability=ThisCaptured(None, capturedThis) }
        let getArgsLength = function
            | [arg: FSharpMemberOrFunctionOrValue] when isUnit arg.FullType -> 0
            | args -> args.Length
        match fsExpr with
        | CurriedLambda(args, body) when(getArgsLength args) = arity ->
            let ctx, args = makeLambdaArgs com ctx args
            let body = com.Transform ctx body
            Fable.Lambda(args, body, not containsJsThis) |> Fable.Value
        | Transform com ctx expr ->
            let lambdaArgs = [for i=1 to arity do yield Fable.Ident(com.GetUniqueVar(), Fable.Any)]
            let body =
                (expr, lambdaArgs)
                ||> List.fold (fun callee arg ->
                    Fable.Apply (callee, [Fable.Value (Fable.IdentValue arg)],
                        Fable.ApplyMeth, Fable.Any, expr.Range))
            Fable.Lambda(lambdaArgs, body, not containsJsThis) |> Fable.Value
        |> fun lambda ->
            match capturedThis with
            | Some((_,Fable.Value(Fable.IdentValue capturedThis))::_) ->
                let varDecl = Fable.VarDeclaration(capturedThis, Fable.Value Fable.This, false)
                Fable.Sequential([varDecl; lambda], lambda.Range)
            | _ -> lambda
