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

type ThisAvailability =
    | ThisUnavailable
    | ThisAvailable
    /// Object expressions must capture the `this` reference and
    /// they can also be nested (see makeThisRef and the ObjectExpr pattern)
    | ThisCaptured
        of currentThis: FSharpMemberOrFunctionOrValue option
        * capturedThis: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list

type MemberInfo =
    { isInstance: bool
      passGenerics: bool }

type EnclosingModule(entity, isPublic) =
    member val Entity: Fable.Entity = entity
    member val IsPublic: bool = isPublic

type Context =
    { fileName: string
      enclosingModule: EnclosingModule
      scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
      scopedInlines: (FSharpMemberOrFunctionOrValue * FSharpExpr) list
    /// Some expressions that create scope in F# don't do it in JS (like let bindings)
    /// so we need a mutable registry to prevent duplicated var names.
      varNames: HashSet<string>
      typeArgs: (string * FSharpType) list
      decisionTargets: Map<int, FSharpMemberOrFunctionOrValue list * FSharpExpr> option
      thisAvailability: ThisAvailability
      genericAvailability: bool
      isLambdaBody: bool
      caughtException: Fable.Ident option }
    static member Create(fileName, enclosingModule) =
        { fileName = fileName
          enclosingModule = EnclosingModule(enclosingModule, true)
          scope = []
          scopedInlines = []
          varNames = HashSet()
          typeArgs = []
          decisionTargets = None
          thisAvailability = ThisUnavailable
          genericAvailability = false
          isLambdaBody = false
          caughtException = None }

type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract IsReplaceCandidate: FSharpEntity -> bool
    abstract TryGetInternalFile: FSharpEntity -> string option
    abstract GetEntity: FSharpEntity -> Fable.Entity
    abstract GetInlineExpr: FSharpMemberOrFunctionOrValue -> (IDictionary<FSharpMemberOrFunctionOrValue,int> * FSharpExpr)
    abstract AddInlineExpr: string * (IDictionary<FSharpMemberOrFunctionOrValue,int> * FSharpExpr) -> unit
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
    let passGenerics = typeof<Fable.Core.PassGenericsAttribute>.FullName

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
        with _ -> false

    let isInline (meth: FSharpMemberOrFunctionOrValue) =
        match meth.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline -> true

    /// .IsPrivate for members of a private module always evaluate to true (see #696)
    /// so we just make all members of a private module public until a proper solution comes in FCS
    let isPublicMethod (meth: FSharpMemberOrFunctionOrValue) =
        if meth.IsCompilerGenerated
        then false
        else
            match tryEnclosingEntity meth with
            | Some ent when ent.Accessibility.IsPrivate -> true
            | _ -> not meth.Accessibility.IsPrivate

    /// .IsPrivate for types of a private module always evaluate to true (see #841)
    /// so we just make all members of a private module public until a proper solution comes in FCS
    let isPublicEntity (ctx: Context) (ent: FSharpEntity) =
        if not ctx.enclosingModule.IsPublic
        then true
        else not ent.RepresentationAccessibility.IsPrivate

    let isUnit (typ: FSharpType) =
        let typ = nonAbbreviatedType typ
        if typ.HasTypeDefinition
        then typ.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Core.Unit"
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

    let getUnionCaseIndex fsType unionCaseName =
        match tryDefinition fsType with
        | None ->
            failwithf "Cannot find Type definition for union case %s" unionCaseName
        | Some tdef ->
            tdef.UnionCases
            |> Seq.findIndex (fun uc -> uc.Name = unionCaseName)

    /// Lower first letter if there's no explicit compiled name
    let lowerCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt ((=) Atts.compiledName)
        |> function
            | Some name -> name.ConstructorArguments.[0] |> snd |> string
            | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeStrConst

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
        let getArgCount (meth: FSharpMemberOrFunctionOrValue) =
            let args = meth.CurriedParameterGroups
            if args.Count = 0 then 0
            elif args.Count = 1 && args.[0].Count = 1 then
                if isUnit args.[0].[0].Type then 0 else 1
            else args |> Seq.sumBy (fun li -> li.Count)
        let ent = tryEnclosingEntity meth
        // `.EnclosingEntity` only fails for compiler generated module members
        if ent.IsNone || (ent.Value.IsFSharpModule) then
            if meth.CurriedParameterGroups.Count = 0
                && meth.GenericParameters.Count = 0
                && not meth.IsMutable // Mutable module values are compiled as functions (see #986)
            then Fable.Field
            else Fable.Method
        elif meth.IsImplicitConstructor then Fable.Constructor
        elif meth.IsPropertyGetterMethod && (getArgCount meth) = 0 then Fable.Getter
        elif meth.IsPropertySetterMethod && (getArgCount meth) = 1 then Fable.Setter
        else Fable.Method

    let sanitizeMethodName (meth: FSharpMemberOrFunctionOrValue) =
        let isInterface =
            meth.IsExplicitInterfaceImplementation
            || (meth.IsInstanceMember
                && (tryEnclosingEntity meth |> Option.toBool (fun ent -> ent.IsInterface)))
        if isInterface then meth.DisplayName else
        match getMemberKind meth with
        | Fable.Getter | Fable.Setter -> meth.DisplayName
        | _ -> meth.CompiledName

    let hasRestParams (meth: FSharpMemberOrFunctionOrValue) =
        if meth.CurriedParameterGroups.Count <> 1 then false else
        let args = meth.CurriedParameterGroups.[0]
        args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

    let hasPassGenericsAtt com (ctx: Context) (meth: FSharpMemberOrFunctionOrValue) =
        match hasAtt Atts.passGenerics meth.Attributes with
        | true when hasRestParams meth ->
            Atts.passGenerics + " is not compatible with ParamArrayAttribute"
            |> addError com ctx.fileName (getMethLocation meth |> makeRange |> Some)
            false
        | result -> result

    let removeOmittedOptionalArguments (meth: FSharpMemberOrFunctionOrValue) (args: Fable.Expr list) =
        let rec removeArgs (args: (Fable.Expr*FSharpParameter) list) =
            match args with
            | (arg, p)::rest ->
                if arg.IsNull && p.IsOptionalArg
                then removeArgs rest
                else args
            | _ -> args
        if meth.CurriedParameterGroups.Count <> 1
        then args
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
    let inline (|LazyValue|) (x: Lazy<'T>) = x.Value
    let inline (|Transform|) (com: IFableCompiler) x = com.Transform x
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

    let (|ThisVar|_|) = function
        | BasicPatterns.ThisValue _ -> Some ThisVar
        | BasicPatterns.Value var when
            var.IsMemberThisValue || var.IsConstructorThisValue ->
            Some ThisVar
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
            if not(List.sameLength identAndRepls args) then false else
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

    let (|FlattenedApplication|_|) fsExpr =
        // Application args can be empty sometimes so a flag is needed
        let rec flattenApplication flag typeArgs args = function
            | Application(callee, typeArgs2, args2) ->
                flattenApplication true (typeArgs2@typeArgs) (args2@args) callee
            | callee -> if flag then Some(callee, typeArgs, args) else None
        flattenApplication false [] [] fsExpr

    let (|FlattenedLambda|_|) fsExpr =
        // F# compiler puts tuple destructs in between curried arguments
        // E.g `fun (x, y) z -> x + y + z` becomes `(tupledArg) => { var x = tupledArg[0]; var y = tupledArg[1]; return z => x + y + z }`
        // so we need to detect this destructs in order to flatten the lambda
        let rec flattenDestructs tupleDestructs = function
            | Let ((var, (TupleGet(_,_,Value _) as arg)), body) -> flattenDestructs ((var,arg)::tupleDestructs) body
            | e -> tupleDestructs, e
        let rec flattenLambda args tupleDestructs = function
            | Lambda(arg, body) ->
                let tupleDestructs, body =
                    if arg.FullType.IsTupleType && arg.IsCompilerGenerated && arg.CompiledName = "tupledArg"
                    then flattenDestructs tupleDestructs body
                    else tupleDestructs, body
                flattenLambda (arg::args) tupleDestructs body
            | body ->
                if List.isEmpty args
                then None
                else Some(List.rev args, List.rev tupleDestructs, body)
        flattenLambda [] [] fsExpr

    // TODO: Careful with this. If the treatment of these expressions change
    // this needs to change as well
    let (|MaybeErased|) = function
        | Application(expr,_,[]) -> expr
        | AddressOf(expr) -> expr
        | expr -> expr

    let (|ImmutableBinding|_|) = function
        | Let((var, MaybeErased(Value v as value)), body)
            when not var.IsMutable && not v.IsMutable && not v.IsMemberThisValue -> Some((var, value), body)
        | Let((var, (Const _ as value)), body)
            when not var.IsMutable -> Some((var, value), body)
        | Let((var, (UnionCaseGet(MaybeErased(Value v),_,_,_) as value)), body)
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

    let (|ContainsAtt|_|) (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> tryFindAtt ((=) name) |> Option.map (fun att ->
            att.ConstructorArguments |> Seq.map snd |> Seq.toList)

    let (|OptionUnion|ListUnion|ErasedUnion|StringEnum|PojoUnion|OtherType|) (NonAbbreviatedType typ: FSharpType) =
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
                    elif name = Atts.pojo then Some PojoUnion
                    else None)
                |> defaultArg <| OtherType

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

    // Some attributes (like ComDefaultInterface) will throw an exception
    // when trying to access ConstructorArguments
    let makeDecorator (com: IFableCompiler) (att: FSharpAttribute) =
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

    let rec makeTypeFromDef (com: IFableCompiler) typeArgs (tdef: FSharpEntity)
                        (genArgs: seq<FSharpType>) =
        let tdef = nonAbbreviatedEntity tdef
        let fullName = getEntityFullName tdef
        // printfn "makeTypeFromDef %s" fullName
        // Array
        if tdef.IsArrayType
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
                then [Seq.head genArgs |> makeType com typeArgs], Fable.Unit, false
                else [Fable.Unit], Fable.Unit, false
                |> Fable.Function
            elif fullName.StartsWith("System.Func")
            then
                match Seq.length genArgs with
                | 0 -> [Fable.Unit], Fable.Unit, false
                | 1 -> [Fable.Unit], Seq.head genArgs |> makeType com typeArgs, false
                | c -> Seq.take (c-1) genArgs |> Seq.map (makeType com typeArgs) |> Seq.toList,
                        Seq.last genArgs |> makeType com typeArgs, false
                |> Fable.Function
            else
            try
                let argTypes =
                    tdef.FSharpDelegateSignature.DelegateArguments
                    |> Seq.map (snd >> makeType com typeArgs) |> Seq.toList
                let retType =
                    makeType com typeArgs tdef.FSharpDelegateSignature.DelegateReturnType
                Fable.Function(argTypes, retType, false)
            with _ -> Fable.Function([Fable.Any], Fable.Any, false)
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
                elif name = Atts.erase || name = Atts.pojo
                then Some Fable.Any
                else None)
            |> defaultArg <|
                // Declared Type
                Fable.DeclaredType(com.GetEntity tdef,
                    genArgs |> Seq.map (makeType com typeArgs) |> Seq.toList)

    and makeType (com: IFableCompiler) typeArgs (NonAbbreviatedType t) =
        // printfn "makeType %O" t
        let makeGenArgs (genArgs: #seq<FSharpType>) =
            Seq.map (makeType com typeArgs) genArgs |> Seq.toList
        let resolveGenParam (genParam: FSharpGenericParameter) =
            match typeArgs |> List.tryFind (fun (name,_) -> name = genParam.Name) with
            // Clear typeArgs to prevent infinite recursion
            | Some (_,typ) -> makeType com [] typ
            | None -> Fable.GenericParam genParam.Name
        let rec getFnGenArgs (acc: FSharpType list) (fn: FSharpType) =
            if fn.IsFunctionType
            then getFnGenArgs (fn.GenericArguments.[0]::acc) fn.GenericArguments.[1]
            elif fn.IsGenericParameter
            then
                match typeArgs |> List.tryFind (fun (name,_) -> name = fn.GenericParameter.Name) with
                | Some (_,fn2) when fn2.IsFunctionType -> getFnGenArgs (fn2.GenericArguments.[0]::acc) fn2.GenericArguments.[1]
                | _ -> fn::acc
            else fn::acc
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
        // Tuple
        elif t.IsTupleType
        then Fable.Tuple(makeGenArgs t.GenericArguments)
        // Funtion
        elif t.IsFunctionType
        then
            let gs = getFnGenArgs [t.GenericArguments.[0]] t.GenericArguments.[1]
            let argTypes = List.rev gs.Tail |> List.map (makeType com typeArgs)
            let returnType = makeType com typeArgs gs.Head
            Fable.Function(argTypes, returnType, true)
        elif t.HasTypeDefinition
        then makeTypeFromDef com typeArgs t.TypeDefinition t.GenericArguments
        else Fable.Any // failwithf "Unexpected non-declared F# type: %A" t

    let getBaseClass (com: IFableCompiler) (tdef: FSharpEntity) =
        match tdef.BaseType with
        | Some(TypeDefinition tdef) when tdef.FullName <> "System.Object" ->
            let typeRef = makeTypeFromDef com [] tdef [] |> makeNonGenTypeRef com
            Some (sanitizeEntityFullName tdef, typeRef)
        | _ -> None

    let makeMethodFrom com name kind loc argTypes returnType originalTyp overloadIndex
                       (meth: FSharpMemberOrFunctionOrValue) =
        Fable.Member(name, kind, loc, argTypes, returnType,
            originalType = originalTyp,
            genParams = (meth.GenericParameters |> Seq.map (fun x -> x.Name) |> Seq.toList),
            decorators = (meth.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList),
            isMutable = meth.IsMutable,
            ?overloadIndex = overloadIndex,
            hasRestParams = hasRestParams meth)

    let getArgTypes com (args: IList<IList<FSharpParameter>>) =
        // FSharpParameters don't contain the `this` arg
        Seq.concat args
        // The F# compiler "untuples" the args in methods
        |> Seq.map (fun x -> makeType com [] x.Type)
        |> Seq.toList

    let makeOriginalCurriedType com (args: IList<IList<FSharpParameter>>) returnType =
        let tys = args |> Seq.map (fun tuple ->
            let tuple = tuple |> Seq.map (fun t -> makeType com [] t.Type)
            match List.ofSeq tuple with
            | [singleArg] -> singleArg
            | args -> Fable.Tuple(args) )
        Seq.append tys [returnType] |> Seq.reduceBack (fun a b -> Fable.Function([a], b, true))

    let getMembers com (tdef: FSharpEntity) =
        let isAbstract =
            hasAtt Atts.abstractClass tdef.Attributes
        let isDefaultImplementation (x: FSharpMemberOrFunctionOrValue) =
            isAbstract && x.IsOverrideOrExplicitInterfaceImplementation && not x.IsExplicitInterfaceImplementation
        // F# allows abstract method syntax in non-abstract classes
        // if there's a default implementation (see #701)
        let isFakeAbstractMethod (x: FSharpMemberOrFunctionOrValue) =
            not isAbstract && not tdef.IsInterface && x.IsDispatchSlot
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
                && not(x.IsProperty && not(x.IsPropertyGetterMethod || x.IsPropertySetterMethod))
                && not(isFakeAbstractMethod x))
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
    let makeEntity (com: IFableCompiler) (tdef: FSharpEntity): Fable.Entity =
        let makeFields (tdef: FSharpEntity) =
            tdef.FSharpFields
            |> Seq.map (fun x -> x.Name, makeType com [] x.FieldType)
            |> Seq.toList
        let makeProperties (tdef: FSharpEntity) =
            tdef.MembersFunctionsAndValues
            |> Seq.choose (fun x ->
                if not x.IsPropertyGetterMethod then None else
                match makeType com [] x.FullType with
                | Fable.Function(_, returnType, _) ->
                    Some(x.DisplayName, returnType)
                | _ -> None)
            |> Seq.toList
        let makeCases (tdef: FSharpEntity) =
            tdef.UnionCases |> Seq.map (fun uci ->
                let name =
                    uci.Attributes
                    |> tryFindAtt ((=) Atts.compiledName)
                    |> function
                        | Some name -> name.ConstructorArguments.[0] |> snd |> string
                        | None -> uci.Name
                name, [for fi in uci.UnionCaseFields do yield makeType com [] fi.FieldType])
            |> Seq.toList
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
        Fable.Entity (lazy getKind(), com.TryGetInternalFile tdef,
            sanitizeEntityFullName tdef, lazy getMembers com tdef, genParams, infcs, decs)

    let inline (|FableEntity|) (com: IFableCompiler) e = com.GetEntity e
    let inline (|FableType|) com (ctx: Context) t = makeType com ctx.typeArgs t

module Identifiers =
    open Helpers
    open Types

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with scope = (Some fsRef, expr)::ctx.scope}

    let private bindIdentPrivate (com: IFableCompiler) (ctx: Context) typ
                  (fsRef: FSharpMemberOrFunctionOrValue option) force name =
        let sanitizedName = name |> Naming.sanitizeIdent (fun x ->
            not force && ctx.varNames.Contains x)
        ctx.varNames.Add sanitizedName |> ignore
        // We still need to keep track of all used variable names in the file
        // so they're not used for imports
        com.AddUsedVarName sanitizedName
        let ident = Fable.Ident(sanitizedName, typ)
        let identValue = Fable.Value (Fable.IdentValue ident)
        { ctx with scope = (fsRef, identValue)::ctx.scope}, ident

    let bindIdentWithExactName com ctx typ fsRef name =
        bindIdentPrivate com ctx typ fsRef true name

    /// Make a sanitized identifier from a tentative name
    let bindIdent com ctx typ fsRef tentativeName =
        bindIdentPrivate com ctx typ fsRef false tentativeName

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        bindIdent com ctx (makeType com ctx.typeArgs fsRef.FullType) (Some fsRef) fsRef.CompiledName

    let (|BindIdent|) = bindIdentFrom

    /// Get corresponding identifier to F# value in current scope
    let tryGetBoundExpr (ctx: Context) r (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fst >> function Some fsRef' -> obj.Equals(fsRef, fsRef') | None -> false)
        |> function
            | Some(_, boundExpr) -> Some boundExpr
            | None -> None

module Util =
    open Helpers
    open Patterns
    open Types
    open Identifiers

    let validateGenArgs com (ctx: Context) r (genParams: FSharpGenericParameter seq) (typArgs: FSharpType seq) =
        let fail typName genName =
            let typName = defaultArg typName ""
            sprintf "Type %s passed as generic param '%s must be decorated with %s or be `obj`/interface" typName genName Atts.pojo
            |> addError com ctx.fileName r
        if Seq.length genParams = Seq.length typArgs then
            Seq.zip genParams typArgs
            |> Seq.iter (fun (par, arg) ->
                if hasAtt Atts.pojo par.Attributes then
                    match tryDefinition arg with
                    | Some argDef when argDef.IsInterface -> ()
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
        let ctx, args =
            ((ctx, []), vars)
            ||> List.fold (fun (ctx, accArgs) var ->
                let newContext, arg = bindIdentFrom com ctx var
                newContext, arg::accArgs)
        ctx, List.rev args

    let bindMemberArgs com ctx (info: MemberInfo) (args: FSharpMemberOrFunctionOrValue list list) =
        // To prevent name clashes in JS create a scope for members
        // where variables must always have a unique name
        let ctx = { ctx with varNames = HashSet(ctx.varNames) }
        let thisArg, args =
            match args with
            | [thisArg]::args when info.isInstance ->
                Some thisArg, args
            | _ -> None, args
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
                // Add caughtException to context so it can be retrieved if `reraise` is used
                let catchContext = { catchContext with caughtException = Some catchVar }
                Some (catchVar, com.Transform catchContext catchBody)
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch (body, catchClause, finalizer, makeRangeFrom fsExpr)

    let makeGetFrom r typ callee propExpr =
        Fable.Apply (callee, [propExpr], Fable.ApplyGet, typ, r)

    // This method doesn't work, the arguments don't keep the attributes
//    let hasRestParams (args: FSharpMemberOrFunctionOrValue list list) =
//        match args with
//        | [args] when args.Length > 0 ->
//            tryFindAtt ((=) "ParamArray") (Seq.last args).Attributes
//            |> Option.isSome
//        | _ -> false

    let buildApplyInfo com (ctx: Context) r typ ownerType ownerFullName methName methKind
            (atts, typArgs, methTypArgs, methArgTypes) (callee, args): Fable.ApplyInfo =
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
            methodArgTypes = methArgTypes
            genericAvailability = ctx.genericAvailability
            caughtException = ctx.caughtException
        }

    let buildApplyInfoFrom com (ctx: Context) r typ
            (typArgs, methTypArgs, methArgTypes)
            (callee, args) (owner: FSharpEntity option)
            (meth: FSharpMemberOrFunctionOrValue)
            : Fable.ApplyInfo =
        let ownerType, ownerFullName =
            match owner with
            | Some ent -> makeTypeFromDef com ctx.typeArgs ent [], sanitizeEntityFullName ent
            | None -> Fable.Any, "System.Object"
        buildApplyInfo com ctx r typ ownerType ownerFullName (sanitizeMethodName meth) (getMemberKind meth)
            (meth.Attributes, typArgs, methTypArgs, methArgTypes) (callee, args)

    let tryPlugin (com: IFableCompiler) (info: Fable.ApplyInfo) =
        com.ReplacePlugins
        |> Plugins.tryPlugin info.range (fun p -> p.TryReplace com info)

    let (|Plugin|_|) (com: IFableCompiler) (info: Fable.ApplyInfo) _ =
        tryPlugin com info

    let tryReplace (com: IFableCompiler) ctx (ent: FSharpEntity option) (info: Fable.ApplyInfo) =
        let isInterface = function
            | Fable.DeclaredType(ent, _) when ent.Kind = Fable.Interface -> true
            | _ -> false
        match ent with
        | Some ent when com.IsReplaceCandidate ent ->
            match Replacements.tryReplace com info with
            | Some _ as repl -> repl
            | None when isInterface info.ownerType -> None
            | None ->
                sprintf "Cannot find replacement for %s::%s" info.ownerFullName info.methodName
                |> addErrorAndReturnNull com ctx.fileName info.range |> Some
        | _ -> None

    let (|Replaced|_|) (com: IFableCompiler) ctx owner i (_: FSharpMemberOrFunctionOrValue) =
        tryReplace com ctx owner i

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
        let cache = Dictionary<string, obj>()
        fun (tdef: FSharpEntity) ->
            cache.GetOrAdd(tdef.QualifiedName, fun _ ->
                let filePath = tdef.Assembly.FileName.Value
                let assembly = Reflection.loadAssembly filePath
                let typ = Reflection.getTypes assembly |> Seq.find (fun x ->
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

    let (|Erased|_|) com (ctx: Context) r typ (owner: FSharpEntity option)
                    (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        match owner with
        | Some owner ->
            match owner.Attributes with
            | ContainsAtt Atts.erase attArgs ->
                match callee with
                | Some callee ->
                    let methName = meth.DisplayName
                    match getMemberKind meth with
                    | Fable.Getter | Fable.Field ->
                        makeGetFrom r typ callee (makeStrConst methName)
                    | Fable.Setter ->
                        Fable.Set (callee, Some (makeStrConst methName), List.head args, r)
                    | Fable.Method ->
                        let m = makeGet r Fable.Any callee (makeStrConst methName)
                        Fable.Apply(m, args, Fable.ApplyMeth, typ, r)
                    | Fable.Constructor ->
                        "Erased type cannot have constructors"
                        |> addErrorAndReturnNull com ctx.fileName r
                    |> Some
                | None ->
                    "Cannot call a static method of an erased type: " + meth.DisplayName
                    |> addErrorAndReturnNull com ctx.fileName r |> Some
            | _ -> None
        | None -> None

    let (|Emitted|_|) com ctx r typ i (typArgs, methTypArgs) (callee, args)
                        (meth: FSharpMemberOrFunctionOrValue) =
        match meth.Attributes with
        | ContainsAtt Atts.emit attArgs ->
            match attArgs with
            | [:? string as macro] ->
                let args =
                    match callee with
                    | None -> args
                    | Some c -> c::args
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
        let hasDoubleEvalRisk = function
            | Fable.Value _
            // Getters may have double eval risk, but let's discard it to prevent
            // the generation of too many closures
            | Fable.Apply(_,_,Fable.ApplyGet,_,_) -> false
            | _ -> true
        if not(isInline meth)
        then None
        else
            let vars, fsExpr = com.GetInlineExpr meth
            let ctx, assignments =
                ((ctx, []), vars, args)
                |||> Seq.fold2 (fun (ctx, assignments) var arg ->
                    // If an expression is referenced more than once, assign it
                    // to a temp var to prevent multiple evaluations
                    if var.Value > 1 && hasDoubleEvalRisk arg then
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

    let passGenerics com ctx r (typArgs, methTypArgs) meth =
        let rec hasUnresolvedGenerics = function
            | Fable.GenericParam name -> Some name
            | Fable.Option genericArg -> hasUnresolvedGenerics genericArg
            | Fable.Array genericArg -> hasUnresolvedGenerics genericArg
            | Fable.Tuple genericArgs -> genericArgs |> Seq.tryPick hasUnresolvedGenerics
            | Fable.Function (argTypes, returnType, _) -> returnType::argTypes |> Seq.tryPick hasUnresolvedGenerics
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
            // Bind the extension method so `this` has the proper value: extMethod.bind(callee)(...args)
            let bind =
                let meth = makeGet None Fable.Any ext (makeStrConst "bind")
                Fable.Apply(meth, [callee], Fable.ApplyMeth, Fable.Any, None)
            Fable.Apply (bind, args, Fable.ApplyMeth, typ, r) |> Some
        | _ -> None

    let makeCallFrom (com: IFableCompiler) ctx r typ
                     (meth: FSharpMemberOrFunctionOrValue)
                     (typArgs, methTypArgs) callee args =
        validateGenArgs com ctx r meth.GenericParameters methTypArgs
        let methArgTypes = getArgTypes com meth.CurriedParameterGroups
        let args =
            let args = ensureArity com methArgTypes args
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
                then args@[passGenerics com ctx r (typArgs, methTypArgs) meth]
                else args
        let owner = tryEnclosingEntity meth
        let i = buildApplyInfoFrom com ctx r typ (typArgs, methTypArgs, methArgTypes) (callee, args) owner meth
        match meth with
        (** -Check for replacements, emits... *)
        | Plugin com i replaced -> replaced
        | Imported com ctx r typ i (typArgs, methTypArgs) args imported -> imported
        | Emitted com ctx r typ i (typArgs, methTypArgs) (callee, args) emitted -> emitted
        | Erased com ctx r typ owner (callee, args) erased -> erased
        | Replaced com ctx owner i replaced -> replaced
        | Inlined com ctx r (typArgs, methTypArgs) (callee, args) expr -> expr
        | ExtensionMember com ctx r typ (callee, args, methArgTypes) owner expr -> expr
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
                | _ -> Fable.DeclaredType(ctx.enclosingModule.Entity, []) |> makeNonGenTypeRef com
            let methName = sanitizeMethodName meth
    (**     *Check if this a getter or setter  *)
            match getMemberKind meth with
            | Fable.Getter | Fable.Field ->
                makeGetFrom r typ callee (makeStrConst methName)
            | Fable.Setter ->
                Fable.Set (callee, Some (makeStrConst methName), args.Head, r)
    (**     *Check if this is an implicit constructor *)
            | Fable.Constructor ->
                Fable.Apply (callee, args, Fable.ApplyCons, typ, r)
    (**     *If nothing of the above applies, call the method normally *)
            | Fable.Method as kind ->
                let applyMeth methName =
                    // let calleeType = Fable.Function(Some methArgTypes, typ)
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
                        ent.TryGetMember(methName, kind, getMemberLoc meth, methArgTypes)
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

    let makeValueFrom com ctx r typ eraseUnit (v: FSharpMemberOrFunctionOrValue) =
        let resolveValue com ctx r typ owner v =
            match tryGetBoundExpr ctx r v with
            | Some e -> e
            | None ->
                let typ, typeRef =
                    match owner with
                    | Some ent -> typ, makeTypeFromDef com ctx.typeArgs ent [] |> makeNonGenTypeRef com
                    // Cases when tryEnclosingEntity returns None are rare, let's assume
                    // the value belongs to the current enclosing module and use
                    // type Any to avoid issues with `AST.Fable.Util.ensureArity`
                    // See MiscTests.``Recursive values work`` (#237)
                    | None ->
                        Fable.Any, Fable.DeclaredType(ctx.enclosingModule.Entity, []) |> makeNonGenTypeRef com
                Fable.Apply (typeRef, [makeStrConst v.CompiledName], Fable.ApplyGet, typ, r)
        if eraseUnit && typ = Fable.Unit
        then Fable.Wrapped(Fable.Value Fable.Null, Fable.Unit)
        elif v.IsModuleValueOrMember
        then
            let owner = tryEnclosingEntity v
            let i = buildApplyInfoFrom com ctx r typ ([], [], []) (None, []) owner v
            match v with
            | Plugin com i replaced -> replaced
            | Imported com ctx r typ i ([], []) [] imported -> imported
            | Emitted com ctx r typ i ([], []) (None, []) emitted -> emitted
            | Replaced com ctx owner i replaced -> replaced
            | v -> resolveValue com ctx r typ owner v
        else
            resolveValue com ctx r typ None v
