module rec Fable.Transforms.Rust.Fable2Rust

open System
open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.Rust
open Fable.Transforms.Rust.AST.Helpers

module Rust = Fable.Transforms.Rust.AST.Types

type HashSet<'T> = System.Collections.Generic.HashSet<'T>

type Import =
    {
        Selector: string
        LocalIdent: string
        ModulePath: string
        Path: string
        mutable Depths: int list
    }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: Fable.Ident list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
    {
        RootScope: HashSet<string>
        DeclarationScopes: HashSet<string>
        CurrentDeclarationScope: HashSet<string>
    }

type ScopedVarAttrs =
    {
        IsArm: bool
        IsRef: bool
        IsBox: bool
        IsFunc: bool
        mutable UsageCount: int
    }

type Context =
    {
        File: Fable.File
        UsedNames: UsedNames
        DecisionTargets: (Fable.Ident list * Fable.Expr) list
        TargetAssignment: Fable.Ident option
        // HoistVars: Fable.Ident list -> bool
        // OptimizeTailCall: unit -> unit
        TailCallOpportunity: ITailCallOpportunity option
        ScopedEntityGenArgs: Set<string>
        ScopedMemberGenArgs: Set<string>
        ScopedSymbols: FSharp.Collections.Map<string, ScopedVarAttrs>
        // HasMultipleUses: bool //this could be a closure in a map, or a for loop. The point is anything leaving the scope cannot be assumed to be the only reference
        InferAnyType: bool
        IsAssocMember: bool
        IsLambda: bool
        IsParamByRefPreferred: bool
        RequiresSendSync: bool // a way to implicitly propagate Arc's down the hierarchy when it is not possible to explicitly tag
        ModuleDepth: int
    }

type IRustCompiler =
    inherit Fable.Compiler
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit
    abstract GetAllImports: Context -> Import list
    abstract ClearAllImports: Context -> unit
    abstract GetAllModules: unit -> string list
    abstract GetAllNamespaces: unit -> (string * string) list
    abstract AddNamespace: string * string -> unit

    abstract GetImportName: Context * selector: string * path: string * SourceLocation option -> string

    abstract TransformExpr: Context * Fable.Expr -> Rust.Expr
    abstract GetEntity: entRef: Fable.EntityRef -> Fable.Entity

// TODO: Centralise and find a home for this
module Helpers =
    module Map =
        let except excluded source =
            source |> Map.filter (fun key _v -> not (excluded |> Map.containsKey key))

        let merge a b =
            (a, b) ||> Map.fold (fun acc key t -> acc |> Map.add key t)

        let mergeAndAggregate aggregateFn a b =
            (a, b)
            ||> Map.fold (fun acc key value ->
                match acc |> Map.tryFind key with
                | Some old -> acc |> Map.add key (aggregateFn old value)
                | None -> acc |> Map.add key value
            )


module Namespace =

    type Trie<'K, 'V when 'K: comparison and 'V: comparison> =
        {
            Values: Set<'V>
            Children: Map<'K, Trie<'K, 'V>>
        }

    module Trie =
        let empty =
            {
                Values = Set.empty
                Children = Map.empty
            }

        let isLeaf trie = not (Set.isEmpty trie.Values)

        let isEmpty trie =
            Map.isEmpty trie.Children && not (isLeaf trie)

        let rec add path value trie =
            match path with
            | [] -> { trie with Values = Set.add value trie.Values }
            | x :: xs ->
                let child =
                    trie.Children |> Map.tryFind x |> Option.defaultValue empty |> add xs value

                let children = trie.Children |> Map.add x child
                { trie with Children = children }

        let ofSeq (xs: (string * string) seq) =
            (empty, xs)
            ||> Seq.fold (fun st (m, n) ->
                let path = n.Split('.') |> List.ofArray
                add path m st
            )

module UsageTracking =

    // type ConsumptionType =
    //     {
    //         Name: string
    //         ByRef: bool
    //         //Path: string list // for debugging purposes only
    //     }

    // let calcIdentConsumption (body: Fable.Expr) =
    //     // todo - handle shadowing of idents
    //     let rec loop pathRev decTreeTargets consumingRef expr =
    //         let mkUsage name= { Name = name
    //                             ByRef = consumingRef
    //                             //Path = pathRev |> List.rev //for debugging purposes only
    //                             }
    //         let loop pathComp =
    //             loop []
    //             //loop (pathComp::pathRev) // debugging purposes only
    //         match expr with
    //         | Fable.IdentExpr ident ->
    //             [mkUsage ident.Name]
    //         | Fable.Sequential exprs ->
    //             exprs |> List.collect (loop "seq" decTreeTargets consumingRef)
    //         | Fable.Let(_, value, body) -> loop "let" decTreeTargets false value @ loop "let" decTreeTargets false body
    //         | Fable.LetRec(bindings, body) ->
    //             let bindingUsages =
    //                 bindings
    //                 |> List.map snd
    //                 |> List.collect (loop "letrec" decTreeTargets false)
    //             bindingUsages @ loop "letrec" decTreeTargets false body
    //         | Fable.IfThenElse(cond, thenExpr, elseExpr, _) ->
    //             loop "ifelse" decTreeTargets true cond @ loop "ifelse" decTreeTargets consumingRef thenExpr @ loop "ifelse" decTreeTargets consumingRef elseExpr
    //         | Fable.DecisionTree(expr, targets) ->
    //             loop "dectree" targets true expr //@ (List.map snd targets |> List.collect (loop consumingRef))
    //         | Fable.DecisionTreeSuccess(targetIdx, boundValues, _ ) ->
    //             //getDecisionTargetAndBoundValues
    //             let dcexpr = List.tryItem targetIdx decTreeTargets |> Option.map snd |> Option.toList
    //             (boundValues @ dcexpr) |> List.collect (loop $"dtsuc{targetIdx}" decTreeTargets consumingRef)
    //         | Fable.Get(expr, kind, _, _) ->
    //             loop "get" decTreeTargets consumingRef expr
    //         | Fable.Set(expr, kind, _, value, _) ->
    //             let kindOps =
    //                 match kind with
    //                 | Fable.ExprSet expr -> loop "set" decTreeTargets true expr @ loop "set" decTreeTargets true value
    //                 | _ -> loop "set" decTreeTargets true value
    //             loop "set" decTreeTargets false expr @ kindOps @ loop "set" decTreeTargets false value
    //         | Fable.Call(callee, info, t, r) ->
    //             loop "call" decTreeTargets consumingRef callee
    //             @ (info.ThisArg |> Option.map (loop "call" decTreeTargets true) |> Option.defaultValue [])
    //             @ (info.Args |> List.collect (loop "call" decTreeTargets false))
    //         | Fable.Value (kind, _) ->
    //             match kind with
    //             | Fable.ThisValue _ | Fable.BaseValue _ -> []
    //             | Fable.TypeInfo _ | Fable.Null _ | Fable.UnitConstant | Fable.NumberConstant _
    //             | Fable.BoolConstant _ | Fable.CharConstant _ | Fable.StringConstant _ | Fable.RegexConstant _  -> []
    //             | Fable.NewList(None,_) | Fable.NewOption(None,_,_) -> []
    //             | Fable.NewOption(Some e,_,_) -> loop "val_opt" decTreeTargets false e
    //             | Fable.NewList(Some(h,t),_) -> loop "val_lst" decTreeTargets false h @ loop "val_lst" decTreeTargets false t
    //             | Fable.StringTemplate(_,_,exprs)
    //             | Fable.NewTuple(exprs,_)
    //             | Fable.NewUnion(exprs,_,_,_) -> exprs |> List.collect (loop "val_union" decTreeTargets consumingRef)
    //             | Fable.NewArray(newKind, _, kind) ->
    //                 match newKind with
    //                 | Fable.ArrayFrom expr -> loop "val_arr" decTreeTargets false expr
    //                 | Fable.ArrayAlloc expr -> loop "val_arr" decTreeTargets false expr
    //                 | Fable.ArrayValues exprs -> exprs |> List.collect (loop "val_arr" decTreeTargets consumingRef)
    //             | Fable.NewRecord (exprs, _, _) | Fable.NewAnonymousRecord (exprs, _, _, _) ->
    //                 exprs |> List.collect (loop "val_rec" decTreeTargets consumingRef)
    //         | Fable.Lambda (_, body, _)
    //         | Fable.Delegate (_, body, _, _ ) ->
    //             // this is not completely accurate. From here on out we only really want to count each ident maximum 1 time (by value) to simulate closed over ident cloning
    //             loop "del" decTreeTargets false body
    //         | Fable.Operation(kind, _, _, _) ->
    //             match kind with
    //             | Fable.Unary(_, expr) ->
    //                 loop "op_u" decTreeTargets false expr
    //             | Fable.Binary(_, l, r) ->
    //                 loop "op_b" decTreeTargets false l @ loop "op_b" decTreeTargets false r
    //             | Fable.Logical(_, l, r) -> loop "op_l" decTreeTargets true l @ loop "op_l" decTreeTargets true r
    //         | Fable.WhileLoop (guard, body, _) ->
    //             loop "while" decTreeTargets true guard @ loop "while" decTreeTargets true body
    //         | Fable.ForLoop (ident, start, limit, body, _, _) ->
    //             let identEv = mkUsage ident.Name
    //             [identEv] @ loop "for" decTreeTargets true start @ loop "for" decTreeTargets true limit @ loop "for" decTreeTargets true body
    //         | Fable.CurriedApply (applied, args, _, _) ->
    //             loop "ca" decTreeTargets false applied @ (args |> List.collect (loop "ca" decTreeTargets false))
    //         | Fable.TypeCast(e, t) ->
    //             loop "tc" decTreeTargets true e
    //         | Fable.Test(expr, kind, range) ->
    //             loop "test" decTreeTargets true expr
    //         | Fable.TryCatch (body, catch, finalizer, _) ->
    //             loop "try_catch" decTreeTargets false body
    //             @ (catch |> Option.map (snd >> loop "try_catch" decTreeTargets true) |> Option.defaultValue [])
    //             @ (finalizer |> Option.map (loop "try_catch" decTreeTargets true) |> Option.defaultValue [])
    //         | Fable.Emit (info, _, _) ->
    //             (info.CallInfo.ThisArg |> Option.map (loop "try_catch" decTreeTargets true) |> Option.defaultValue [])
    //             @ (info.CallInfo.Args |> List.collect (loop "try_catch" decTreeTargets false))
    //         | _ -> []
    //     loop [] [] false body

    // let calcIdentUsages expr =
    //     let identUsages = calcIdentConsumption expr
    //     //break here if you want to know how we got to a certain count
    //     identUsages
    //     |> List.map (fun u -> u.Name)
    //     |> List.groupBy id
    //     |> List.map (fun (identName, instances) ->
    //         match identName with
    //         //cannot get this working - It seems some call sites retain original ident names, so match value gives counts that are too low
    //         | "matchValue" -> identName, 9999
    //         | _ -> identName, instances |> List.length)
    //     |> Map.ofList

    let isArmScoped ctx name =
        ctx.ScopedSymbols
        |> Map.tryFind name
        |> Option.map (fun s -> s.IsArm)
        |> Option.defaultValue false

    let isValueScoped ctx name =
        ctx.ScopedSymbols
        |> Map.tryFind name
        |> Option.map (fun s -> not s.IsRef)
        |> Option.defaultValue false

    let isRefScoped ctx name =
        ctx.ScopedSymbols
        |> Map.tryFind name
        |> Option.map (fun s -> s.IsRef)
        |> Option.defaultValue false

    let isBoxScoped ctx name =
        ctx.ScopedSymbols
        |> Map.tryFind name
        |> Option.map (fun s -> s.IsBox)
        |> Option.defaultValue false

    let isFuncScoped ctx name =
        ctx.ScopedSymbols
        |> Map.tryFind name
        |> Option.map (fun s -> s.IsFunc)
        |> Option.defaultValue false

    let isUsedOnce ctx name =
        ctx.ScopedSymbols
        |> Map.tryFind name
        |> Option.map (fun s -> s.UsageCount = 1)
        |> Option.defaultValue false

    let usageCount name usages =
        Map.tryFind name usages |> Option.defaultValue 0

    // not an accurate count, just a metric if ident is used more than once
    // (the reason is that we want to count usage in loops as multiple uses)
    // (i.e. usage can be tested for zero, one, or more than one (not exact))
    // TODO: also adjust usage count in tail call loops
    let rec countIdentUsage name (expr: Fable.Expr) : int =
        let subCount =
            getSubExpressions expr |> List.sumBy (fun e -> countIdentUsage name e) // depth-first

        match expr with
        | Fable.IdentExpr ident when ident.Name = name -> subCount + 1 // count each ident with the same name
        | Fable.ForLoop _
        | Fable.WhileLoop _ -> subCount * 2 // usage in loops counts as multiple uses
        | Fable.DecisionTree _ -> subCount * 2 // usage in complex decision trees can vary
        | _ -> subCount + 0 // anything else is zero

    let calcIdentUsages idents exprs =
        let usageCounts =
            idents
            |> List.map (fun (ident: Fable.Ident) ->
                let count = exprs |> List.map (fun e -> countIdentUsage ident.Name e) |> List.sum

                ident.Name, count
            )

        usageCounts |> Map

module TypeInfo =

    let makeFullNamePath fullName genArgsOpt =
        let parts = splitNameParts fullName
        mkGenericPath parts genArgsOpt

    let makeStaticCallPathExpr importName membName genArgsOpt =
        let fullName = importName + "::" + membName
        let parts = splitNameParts fullName
        let offset = 1 // genArgs position offset is one before last
        mkGenericOffsetPath parts genArgsOpt offset |> mkPathExpr

    let makeFullNamePathExpr fullName genArgsOpt =
        makeFullNamePath fullName genArgsOpt |> mkPathExpr

    let makeFullNamePathTy fullName genArgsOpt =
        makeFullNamePath fullName genArgsOpt |> mkPathTy

    let makeFullNameIdentPat (fullName: string) =
        let fullName = fullName.Replace(".", "::")
        mkIdentPat fullName false false

    let primitiveType (name: string) : Rust.Ty = mkGenericPathTy [ name ] None

    let getLibraryImportName (com: IRustCompiler) ctx moduleName typeName =
        let selector = moduleName + "_::" + typeName
        let libPath = getLibPath com moduleName
        com.GetImportName(ctx, selector, libPath, None)

    let makeImportType com ctx moduleName typeName tys : Rust.Ty =
        let importName = getLibraryImportName com ctx moduleName typeName
        tys |> mkGenericTy (splitNameParts importName)

    let makeCastTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "Lrc"

    let makeFluentTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "Lrc"

    let makeLrcPtrTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "LrcPtr"

    // let makeLrcTy com ctx (ty: Rust.Ty): Rust.Ty =
    //     [ty] |> makeImportType com ctx "Native" "Lrc"

    let makeRcTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "Rc"

    let makeArcTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "Arc"

    let makeBoxTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" (rawIdent "Box")

    let makeMutTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "MutCell"

    let makeOptionTy (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> mkGenericTy [ rawIdent "Option" ]

    let makeNullableTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "Nullable"

    let makeAnyTy com ctx : Rust.Ty =
        let importName = getLibraryImportName com ctx "Native" "Any"
        let traitBound = mkTypeTraitGenericBound [ importName ] None
        mkDynTraitTy [ traitBound ]

    let getEntityGenParamNames (ent: Fable.Entity) =
        ent.GenericParameters
        |> List.filter (fun p -> not p.IsMeasure)
        |> List.map (fun p -> p.Name)
        |> Set.ofList

    let hasMutableFields (com: IRustCompiler) (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            ent.UnionCases
            |> List.exists (fun uci -> uci.UnionCaseFields |> List.exists (fun fi -> fi.IsMutable))
        else
            ent.FSharpFields |> List.exists (fun fi -> fi.IsMutable)

    let isValueType (com: IRustCompiler) typ =
        match typ with
        | Fable.Boolean -> true
        | Fable.Char -> true
        // | Fable.Number(BigInt, _) -> false
        | Fable.Number _ -> true
        | Fable.Option(_, isStruct) -> isStruct
        | Fable.Tuple(_, isStruct) -> isStruct
        | Fable.AnonymousRecordType(_, _, isStruct) -> isStruct
        | Fable.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)
            ent.IsValueType
        | _ -> false

    let isTypeOfType (com: IRustCompiler) isTypeOf isEntityOf entNames typ =
        match typ with
        | Fable.Option(genArg, _) -> isTypeOf com entNames genArg
        | Fable.Array(genArg, _) -> isTypeOf com entNames genArg
        | Fable.List genArg -> isTypeOf com entNames genArg
        | Fable.Tuple(genArgs, _) -> List.forall (isTypeOf com entNames) genArgs
        | Fable.AnonymousRecordType(_, genArgs, _) -> List.forall (isTypeOf com entNames) genArgs
        | Replacements.Util.Builtin(Replacements.Util.FSharpSet genArg) -> isTypeOf com entNames genArg
        | Replacements.Util.Builtin(Replacements.Util.FSharpMap(k, v)) ->
            isTypeOf com entNames k && isTypeOf com entNames v
        | Fable.DeclaredType(entRef, _genArgs) ->
            let ent = com.GetEntity(entRef)
            isEntityOf com entNames ent
        | _ -> true

    let hasFieldsOfType (com: IRustCompiler) isTypeOf entNames (ent: Fable.Entity) =
        if Set.contains ent.FullName entNames then
            true // already checked, avoids circular checks
        else
            let entNames = Set.add ent.FullName entNames

            if ent.IsFSharpUnion then
                ent.UnionCases
                |> Seq.forall (fun uci ->
                    uci.UnionCaseFields
                    |> List.forall (fun fi -> isTypeOf com entNames fi.FieldType)
                )
            else
                ent.FSharpFields |> Seq.forall (fun fi -> isTypeOf com entNames fi.FieldType)

    // let isPrintableType (com: IRustCompiler) entNames typ =
    //     match typ with
    //     // TODO: more unprintable types?
    //     | _ -> isTypeOfType com isPrintableType isPrintableEntity entNames typ

    let isPrintableEntity com entNames (ent: Fable.Entity) = (List.isEmpty ent.GenericParameters)
    // && (hasFieldsOfType com isPrintableType entNames ent) // commented as it kills performance

    let isDefaultableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more undefaultable types?
        | _ -> isTypeOfType com isDefaultableType isDefaultableEntity entNames typ

    let isDefaultableEntity com entNames (ent: Fable.Entity) =
        ent.IsValueType
        && not ent.IsFSharpUnion // default union cases are quite limited
        && (hasFieldsOfType com isDefaultableType entNames ent)

    // let isCopyableType (com: IRustCompiler) entNames typ =
    //     match typ with
    //     // TODO: more uncopyable types?
    //     | Fable.Any
    //     | Fable.Unit
    //     | Fable.Measure _
    //     | Fable.MetaType
    //     | Fable.LambdaType _
    //     | Fable.DelegateType _
    //     | Fable.GenericParam _
    //     | Fable.String
    //     | Fable.Regex
    //     | Fable.List _ -> false
    //     | Fable.Option(_, isStruct) when not isStruct -> false
    //     | Fable.Tuple(_, isStruct) when not isStruct -> false
    //     | Fable.AnonymousRecordType(_, _, isStruct) when not isStruct -> false
    //     | _ -> isTypeOfType com isCopyableType isCopyableEntity entNames typ

    // let isCopyableEntity com entNames (ent: Fable.Entity) =
    //     ent.IsValueType
    //     && not (hasMutableFields com ent)
    //     && (hasFieldsOfType com isCopyableType entNames ent)

    let isEquatableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more unequatable types?
        | Fable.Any
        | Fable.Unit
        | Fable.Measure _
        | Fable.MetaType
        | Fable.LambdaType _
        | Fable.DelegateType _ -> false
        // | Fable.GenericParam(_, _, constraints) ->
        //     constraints |> List.contains Fable.Constraint.HasEquality
        | _ -> isTypeOfType com isEquatableType isEquatableEntity entNames typ

    let isEquatableEntity com entNames (ent: Fable.Entity) =
        (FSharp2Fable.Util.hasStructuralEquality ent)
        && (hasFieldsOfType com isEquatableType entNames ent)

    let isComparableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more uncomparable types?
        | Fable.Any
        | Fable.Unit
        | Fable.Measure _
        | Fable.MetaType
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.Regex -> false
        // | Fable.GenericParam(_, _, constraints) ->
        //     constraints |> List.contains Fable.Constraint.HasComparison
        | _ -> isTypeOfType com isComparableType isComparableEntity entNames typ

    let isComparableEntity com entNames (ent: Fable.Entity) =
        (FSharp2Fable.Util.hasStructuralComparison ent)
        && (hasFieldsOfType com isComparableType entNames ent)

    let isHashableType com entNames typ =
        match typ with
        // TODO: more unhashable types?
        | Fable.Any
        | Fable.Unit
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Number((Float32 | Float64), _)
        | Fable.LambdaType _
        | Fable.DelegateType _ -> false
        | _ -> isTypeOfType com isHashableType isHashableEntity entNames typ

    let isHashableEntity com entNames (ent: Fable.Entity) =
        (FSharp2Fable.Util.hasStructuralEquality ent)
        && (hasFieldsOfType com isHashableType entNames ent)

    let isWrappedType com typ =
        match typ with
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.GenericParam _
        | Fable.String
        | Fable.Array _
        | Fable.List _
        | Fable.Option _
        | Fable.Number(BigInt, _)
        | Replacements.Util.Builtin(Replacements.Util.FSharpResult _)
        | Replacements.Util.Builtin(Replacements.Util.FSharpSet _)
        | Replacements.Util.Builtin(Replacements.Util.FSharpMap _)
        | Replacements.Util.Builtin(Replacements.Util.BclHashSet _)
        | Replacements.Util.Builtin(Replacements.Util.BclDictionary _)
        // interfaces implemented as the type itself
        | Replacements.Util.IsEntity (Types.iset) _
        | Replacements.Util.IsEntity (Types.idictionary) _
        | Replacements.Util.IsEntity (Types.ireadonlydictionary) _
        | Replacements.Util.IsEntity (Types.keyCollection) _
        | Replacements.Util.IsEntity (Types.valueCollection) _
        | Replacements.Util.IsEntity (Types.icollectionGeneric) _
        // already wrapped
        | Replacements.Util.IsEntity (Types.regexMatch) _
        | Replacements.Util.IsEntity (Types.regexGroup) _
        | Replacements.Util.IsEntity (Types.regexCapture) _
        // | Replacements.Util.IsEntity (Types.regexMatchCollection) _
        // | Replacements.Util.IsEntity (Types.regexGroupCollection) _
        // | Replacements.Util.IsEntity (Types.regexCaptureCollection) _
         -> true
        | _ -> false

    // Checks whether the type needs a ref counted wrapper
    // such as Rc<T> (or Arc<T> in a multithreaded context)
    let shouldBeRefCountWrapped (com: IRustCompiler) ctx typ =
        match typ with
        // passed by reference, no need to Rc-wrap
        | t when isByRefType com t -> None

        // already wrapped, no need to Rc-wrap
        | t when isWrappedType com t -> None

        // always not Rc-wrapped
        | Fable.Unit
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Boolean
        | Fable.Char
        | Fable.Number _ -> None

        // should be Rc-wrapped
        | Fable.Any
        | Fable.Regex
        | Replacements.Util.Builtin(Replacements.Util.FSharpReference _)
        | Replacements.Util.IsEnumerator _ -> Some Lrc

        // should be Arc-wrapped
        | Replacements.Util.IsEntity (Types.fsharpAsyncGeneric) _
        | Replacements.Util.IsEntity (Types.task) _
        | Replacements.Util.IsEntity (Types.taskGeneric) _ -> Some Arc

        // conditionally Rc-wrapped
        | Fable.Tuple(_, isStruct) ->
            if isStruct then
                None
            else
                Some Lrc
        | Fable.AnonymousRecordType(_, _, isStruct) ->
            if isStruct then
                None
            else
                Some Lrc
        | Fable.DeclaredType(entRef, _) ->
            match com.GetEntity(entRef) with
            | HasEmitAttribute _ -> None
            // do not make custom types Rc-wrapped by default. This prevents inconsistency between type and implementation emit
            | HasReferenceTypeAttribute ptrType -> Some ptrType
            | ent ->
                if ent.IsValueType then
                    None
                else
                    Some Lrc

        | _ -> None

    let typeImplementsCloneTrait (com: IRustCompiler) ctx typ =
        match typ with
        | Fable.String
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.Option _
        | Fable.List _
        | Fable.Array _
        | Fable.Tuple _
        | Fable.DeclaredType _
        | Fable.AnonymousRecordType _ -> true
        | Fable.Number(BigInt, _) -> true
        | Fable.GenericParam(name, isMeasure, _) -> not (isInferredGenericParam com ctx name isMeasure)
        | _ -> false

    let rec typeImplementsCopyTrait (com: IRustCompiler) ctx typ =
        match typ with
        | Fable.Unit -> true
        | Fable.Boolean -> true
        | Fable.Char -> true
        | Fable.Number(BigInt, _) -> false // BigInt does not implement Copy
        | Fable.Number _ -> true // all other numbers except BigInt
        | _ -> false

    let rec tryGetIdentName =
        function
        | Fable.IdentExpr ident -> ident.Name |> Some
        | Fable.Get(expr, Fable.OptionValue, _, _) -> tryGetIdentName expr
        | Fable.Get(expr, Fable.UnionField _, _, _) -> tryGetIdentName expr
        | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf, expr), _, _, _) -> tryGetIdentName expr
        | _ -> None

    // let getIdentName expr =
    //     tryGetIdentName expr |> Option.defaultValue ""

    let isDeclEntityKindOf (com: IRustCompiler) isKindOf (memberRef: Fable.MemberRef) =
        memberRef
        |> com.TryGetMember
        |> Option.bind (fun memb -> memb.DeclaringEntity)
        |> Option.bind com.TryGetEntity
        |> Option.map isKindOf
        |> Option.defaultValue false

    let isModuleMemberRef (com: IRustCompiler) (memberRef: Fable.MemberRef) =
        isDeclEntityKindOf com (fun ent -> ent.IsFSharpModule) memberRef

    let isModuleMemberCall (com: IRustCompiler) (callInfo: Fable.CallInfo) =
        callInfo.MemberRef
        |> Option.map (isModuleMemberRef com)
        |> Option.defaultValue false

    let transformImport (com: IRustCompiler) ctx r t (info: Fable.ImportInfo) genArgsOpt =
        if info.Selector.Contains("*") || info.Selector.Contains("{") then
            let importName = com.GetImportName(ctx, info.Selector, info.Path, r)
            mkUnitExpr () // just an import without a body
        else
            match info.Kind with
            | Fable.MemberImport memberRef when not (isModuleMemberRef com memberRef) ->
                // for non-module member imports
                let memb = com.GetMember(memberRef)

                if memb.IsInstance then
                    // no import needed (perhaps)
                    let importName = info.Selector //com.GetImportName(ctx, info.Selector, info.Path, r)
                    makeFullNamePathExpr importName genArgsOpt
                else
                    // for constructors or static members, import just the type
                    let selector, membName = Fable.Naming.splitLastBy "." info.Selector
                    let importName = com.GetImportName(ctx, selector, info.Path, r)
                    makeStaticCallPathExpr importName membName genArgsOpt
            | Fable.LibraryImport mi when not (mi.IsInstanceMember) && not (mi.IsModuleMember) ->
                // for static (non-module and non-instance) members, import just the type
                let selector, membName = Fable.Naming.splitLastBy "::" info.Selector
                let importName = com.GetImportName(ctx, selector, info.Path, r)
                makeStaticCallPathExpr importName membName genArgsOpt
            | _ ->
                // all other imports
                let importName = com.GetImportName(ctx, info.Selector, info.Path, r)
                makeFullNamePathExpr importName genArgsOpt

    let makeLibCall com ctx genArgsOpt moduleName memberName (args: Rust.Expr list) =
        let importName = getLibraryImportName com ctx moduleName memberName
        let callee = makeFullNamePathExpr importName genArgsOpt
        mkCallExpr callee args

    let libCall com ctx r genArgs moduleName memberName (args: Fable.Expr list) =
        let genArgsOpt = transformGenArgs com ctx genArgs
        let args = Util.transformCallArgs com ctx args [] []
        makeLibCall com ctx genArgsOpt moduleName memberName args

    let transformGenTypes com ctx genArgs : Rust.Ty list =
        genArgs
        |> List.filter (isUnitOfMeasure >> not)
        |> List.map (transformType com ctx)

    let transformGenArgs com ctx genArgs : Rust.GenericArgs option =
        genArgs |> transformGenTypes com ctx |> mkTypesGenericArgs

    // // if type cannot be resolved, make it unit type
    // let resolveType com ctx t =
    //     match t with
    //     | Fable.Any when ctx.InferAnyType ->
    //         Fable.Unit
    //     | Fable.GenericParam(name, isMeasure, constraints)
    //         when ctx.InferAnyType && not isMeasure && not (Set.contains name ctx.ScopedEntityGenArgs)
    //          -> Fable.Unit
    //     | _ -> t

    // let transformTypeResolved com ctx typ: Rust.Ty =
    //     transformType com ctx (resolveType com ctx typ)

    // let transformGenArgsResolved com ctx genArgs: Rust.GenericArgs option =
    //     genArgs
    //     |> List.map (resolveType com ctx)
    //     |> transformGenArgs com ctx

    let transformGenericType com ctx genArgs typeName : Rust.Ty =
        genArgs |> transformGenTypes com ctx |> mkGenericTy (splitNameParts typeName)

    let transformImportType com ctx genArgs moduleName typeName : Rust.Ty =
        let importName = getLibraryImportName com ctx moduleName typeName
        transformGenericType com ctx genArgs importName

    let transformBigIntType com ctx : Rust.Ty =
        transformImportType com ctx [] "BigInt" "bigint"

    let transformDecimalType com ctx : Rust.Ty =
        transformImportType com ctx [] "Decimal" "decimal"

    let transformListType com ctx genArg : Rust.Ty =
        transformImportType com ctx [ genArg ] "List" "List"

    let transformSetType com ctx genArg : Rust.Ty =
        transformImportType com ctx [ genArg ] "Set" "Set"

    let transformMapType com ctx genArgs : Rust.Ty =
        transformImportType com ctx genArgs "Map" "Map"

    let transformArrayType com ctx genArg : Rust.Ty =
        transformImportType com ctx [ genArg ] "NativeArray" "Array"

    let transformHashSetType com ctx genArg : Rust.Ty =
        transformImportType com ctx [ genArg ] "HashSet" "HashSet"

    let transformHashMapType com ctx genArgs : Rust.Ty =
        transformImportType com ctx genArgs "HashMap" "HashMap"

    let transformGuidType com ctx : Rust.Ty =
        transformImportType com ctx [] "Guid" "Guid"

    let transformRegexType com ctx : Rust.Ty =
        transformImportType com ctx [] "RegExp" "Regex"

    let transformTimeSpanType com ctx : Rust.Ty =
        transformImportType com ctx [] "TimeSpan" "TimeSpan"

    let transformDateTimeType com ctx : Rust.Ty =
        transformImportType com ctx [] "DateTime" "DateTime"

    let transformDateTimeOffsetType com ctx : Rust.Ty =
        transformImportType com ctx [] "DateTimeOffset" "DateTimeOffset"

    let transformDateOnlyType com ctx : Rust.Ty =
        transformImportType com ctx [] "DateOnly" "DateOnly"

    let transformTimeOnlyType com ctx : Rust.Ty =
        transformImportType com ctx [] "TimeOnly" "TimeOnly"

    let transformTimerType com ctx : Rust.Ty =
        transformImportType com ctx [] "DateTime" "Timer"

    let transformAsyncType com ctx genArg : Rust.Ty =
        transformImportType com ctx [ genArg ] "Async" "Async"

    let transformTaskType com ctx genArg : Rust.Ty =
        transformImportType com ctx [ genArg ] "Task" "Task"

    let transformTaskBuilderType com ctx : Rust.Ty =
        transformImportType com ctx [] "TaskBuilder" "TaskBuilder"

    let transformThreadType com ctx : Rust.Ty =
        transformImportType com ctx [] "Thread" "Thread"

    let transformTupleType com ctx _isStruct genArgs : Rust.Ty =
        genArgs |> List.map (transformType com ctx) |> mkTupleTy

    let transformNullableType com ctx isStruct genArg : Rust.Ty =
        if isStruct then
            transformImportType com ctx [ genArg ] "Native" "Nullable"
        else
            transformType com ctx genArg // nullable reference types are transparent

    let transformOptionType com ctx _isStruct genArg : Rust.Ty =
        transformGenericType com ctx [ genArg ] (rawIdent "Option")

    let transformClosureType com ctx argTypes returnType : Rust.Ty =
        let argTypes =
            match argTypes with
            | [ Fable.Unit ] -> []
            | _ -> argTypes

        let argCount = string<int> (List.length argTypes)
        let genArgs = argTypes @ [ returnType ]
        transformImportType com ctx genArgs "Native" ("Func" + argCount)

    let transformNumberType com ctx kind : Rust.Ty =
        match kind with
        | Int8 -> "i8" |> primitiveType
        | UInt8 -> "u8" |> primitiveType
        | Int16 -> "i16" |> primitiveType
        | UInt16 -> "u16" |> primitiveType
        | Int32 -> "i32" |> primitiveType
        | UInt32 -> "u32" |> primitiveType
        | Int64 -> "i64" |> primitiveType
        | UInt64 -> "u64" |> primitiveType
        | Int128 -> "i128" |> primitiveType
        | UInt128 -> "u128" |> primitiveType
        | NativeInt -> "isize" |> primitiveType
        | UNativeInt -> "usize" |> primitiveType
        | Float16 -> "f32" |> primitiveType
        | Float32 -> "f32" |> primitiveType
        | Float64 -> "f64" |> primitiveType
        | Decimal -> transformDecimalType com ctx
        | BigInt -> transformBigIntType com ctx

    let getEntityFullName (com: IRustCompiler) ctx (entRef: Fable.EntityRef) =
        match entRef.SourcePath with
        | Some path ->
            if path <> com.CurrentFile then
                // entity is imported from another file
                let importPath = Path.getRelativeFileOrDirPath false com.CurrentFile false path
                let importName = com.GetImportName(ctx, entRef.FullName, importPath, None)
                importName
            else
                entRef.FullName
        | None ->
            match entRef.Path with
            | Fable.AssemblyPath _
            | Fable.CoreAssemblyName _ ->
                //TODO: perhaps only import from library if it's already implemented BCL class
                let importPath = "fable_library_rust"
                let importName = com.GetImportName(ctx, entRef.FullName, importPath, None)
                importName
            | _ -> entRef.FullName

    let tryFindInterface (com: IRustCompiler) fullName (entRef: Fable.EntityRef) : Fable.DeclaredType option =
        let ent = com.GetEntity(entRef)
        ent.AllInterfaces |> Seq.tryFind (fun ifc -> ifc.Entity.FullName = fullName)

    let transformInterfaceType com ctx (entRef: Fable.EntityRef) genArgs : Rust.Ty =
        let nameParts = getEntityFullName com ctx entRef |> splitNameParts
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound nameParts genArgsOpt
        mkDynTraitTy [ traitBound ]

    let getAbstractClassImportName com ctx (entRef: Fable.EntityRef) =
        match entRef.FullName with
        | "System.Text.Encoding" -> getLibraryImportName com ctx "Encoding" "Encoding"
        | _ -> getEntityFullName com ctx entRef

    let transformAbstractClassType com ctx (entRef: Fable.EntityRef) genArgs : Rust.Ty =
        let entName = getAbstractClassImportName com ctx entRef
        let nameParts = entName |> splitNameParts
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound nameParts genArgsOpt

        match entRef.FullName with
        | "System.Text.Encoding" ->
            // some abstract classes are implemented as interfaces
            mkDynTraitTy [ traitBound ]
        | _ ->
            // most abstract classes are implemented as non-abstract
            makeFullNamePathTy entName genArgsOpt

    let (|HasEmitAttribute|_|) (ent: Fable.Entity) =
        ent.Attributes
        |> Seq.tryPick (fun att ->
            if att.Entity.FullName.StartsWith(Atts.emit, StringComparison.Ordinal) then
                match att.ConstructorArgs with
                | [ :? string as macro ] -> Some macro
                | _ -> None
            else
                None
        )

    type PointerType =
        | Lrc
        | Rc
        | Arc
        | Box

    let (|HasReferenceTypeAttribute|_|) (ent: Fable.Entity) =
        ent.Attributes
        |> Seq.tryPick (fun att ->
            if att.Entity.FullName.StartsWith(Atts.referenceType, StringComparison.Ordinal) then
                match att.ConstructorArgs with
                | [ :? int as ptrType ] ->
                    match ptrType with
                    | 0 -> Some Lrc
                    | 1 -> Some Rc
                    | 2 -> Some Arc
                    | 3 -> Some Box
                    | _ -> None
                | _ -> None
            else
                None
        )

    let (|IsNonErasedInterface|_|) (com: Compiler) =
        function
        | Fable.DeclaredType(entRef, genArgs) ->
            let ent = com.GetEntity(entRef)

            if ent.IsInterface && not (ent |> FSharp2Fable.Util.hasAttribute Atts.erase) then
                Some(entRef, genArgs)
            else
                None
        | _ -> None

    let transformEntityType (com: IRustCompiler) ctx (entRef: Fable.EntityRef) genArgs : Rust.Ty =
        match com.GetEntity(entRef) with
        | HasEmitAttribute value ->
            let genArgs = genArgs |> List.map (transformType com ctx)
            mkEmitTy value genArgs
        | ent when ent.IsInterface -> transformInterfaceType com ctx entRef genArgs
        | ent when ent.IsAbstractClass -> transformAbstractClassType com ctx entRef genArgs
        | ent ->
            let entName = getEntityFullName com ctx entRef
            let genArgsOpt = transformGenArgs com ctx genArgs
            makeFullNamePathTy entName genArgsOpt

    let transformResultType com ctx genArgs : Rust.Ty =
        transformGenericType com ctx genArgs (rawIdent "Result")

    let transformChoiceType com ctx genArgs : Rust.Ty =
        let argCount = string<int> (List.length genArgs)
        transformImportType com ctx genArgs "Choice" ("Choice`" + argCount)

    let transformRefCellType com ctx genArg : Rust.Ty =
        let ty = transformType com ctx genArg
        ty |> makeMutTy com ctx

    let isAddrOfExpr (expr: Fable.Expr) =
        match expr with
        | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf, e), _, _, _) -> true
        | _ -> false

    let isNullableValueType (com: IRustCompiler) =
        function
        | Fable.Nullable(_, true) -> true
        | _ -> false

    let isByRefType (com: IRustCompiler) =
        function
        | Replacements.Util.IsByRefType com _ -> true
        | _ -> false

    let isInRefType (com: IRustCompiler) =
        function
        | Replacements.Util.IsInRefType com _ -> true
        | _ -> false

    let isInterface (com: IRustCompiler) =
        function
        | IsNonErasedInterface com _ -> true
        | _ -> false

    let isException (com: IRustCompiler) =
        function
        | Replacements.Util.IsEntity (Types.exception_) _ -> true
        | Fable.DeclaredType(entRef, genArgs) ->
            let ent = com.GetEntity(entRef)
            ent.IsFSharpExceptionDeclaration
        | _ -> false

    let transformAnyType com ctx : Rust.Ty =
        if ctx.InferAnyType then
            mkInferTy ()
        else
            makeAnyTy com ctx

    let isInferredGenericParam com ctx name isMeasure =
        isMeasure
        || ctx.IsLambda
           && not (Set.contains name ctx.ScopedEntityGenArgs)
           && not (Set.contains name ctx.ScopedMemberGenArgs)

    // let isNullableReferenceType com ctx constraints =
    //     let isNullable = constraints |> List.contains Fable.Constraint.IsNullable
    //     let isNotNullable = constraints |> List.contains Fable.Constraint.IsNotNullable
    //     let isReferenceType = constraints |> List.contains Fable.Constraint.IsReferenceType
    //     isNullable || isNotNullable && isReferenceType

    let transformGenericParamType com ctx name isMeasure constraints : Rust.Ty =
        // if Compiler.CheckNulls && isNullableReferenceType com ctx constraints then
        //     primitiveType name |> makeNullableTy com ctx
        //     // primitiveType name |> makeLrcPtrTy com ctx
        // else
        if isInferredGenericParam com ctx name isMeasure then
            mkInferTy () // mkNeverTy ()
        else
            primitiveType name

    let transformMetaType com ctx : Rust.Ty =
        transformImportType com ctx [] "Reflection" "TypeId"

    let transformStringType com ctx : Rust.Ty =
        transformImportType com ctx [] "String" "string"

    let transformBuiltinType com ctx typ kind : Rust.Ty =
        match kind with
        | Replacements.Util.BclGuid -> transformGuidType com ctx
        | Replacements.Util.BclTimeSpan -> transformTimeSpanType com ctx
        | Replacements.Util.BclDateTime -> transformDateTimeType com ctx
        | Replacements.Util.BclDateTimeOffset -> transformDateTimeOffsetType com ctx
        | Replacements.Util.BclDateOnly -> transformDateOnlyType com ctx
        | Replacements.Util.BclTimeOnly -> transformTimeOnlyType com ctx
        | Replacements.Util.BclTimer -> transformTimerType com ctx
        | Replacements.Util.BclHashSet(genArg) -> transformHashSetType com ctx genArg
        | Replacements.Util.BclDictionary(k, v) -> transformHashMapType com ctx [ k; v ]
        | Replacements.Util.FSharpSet(genArg) -> transformSetType com ctx genArg
        | Replacements.Util.FSharpMap(k, v) -> transformMapType com ctx [ k; v ]
        | Replacements.Util.BclKeyValuePair(k, v) -> transformTupleType com ctx true [ k; v ]
        | Replacements.Util.FSharpResult(ok, err) -> transformResultType com ctx [ ok; err ]
        | Replacements.Util.FSharpChoice genArgs -> transformChoiceType com ctx genArgs
        | Replacements.Util.FSharpReference(genArg) ->
            if isInRefType com typ then
                transformType com ctx genArg
            else
                transformRefCellType com ctx genArg

    let transformType (com: IRustCompiler) ctx (typ: Fable.Type) : Rust.Ty =
        let ty =
            match typ with
            | Fable.Any -> transformAnyType com ctx
            | Fable.Unit -> mkUnitTy ()
            | Fable.Measure _ -> mkInferTy ()
            | Fable.Char -> primitiveType "char"
            | Fable.Boolean -> primitiveType "bool"
            | Fable.String -> transformStringType com ctx
            | Fable.MetaType -> transformMetaType com ctx
            | Fable.Number(kind, _) -> transformNumberType com ctx kind
            | Fable.LambdaType(argType, returnType) ->
                let argTypes, returnType = ([ argType ], returnType)
                transformClosureType com ctx argTypes returnType
            | Fable.DelegateType(argTypes, returnType) -> transformClosureType com ctx argTypes returnType
            | Fable.GenericParam(name, isMeasure, constraints) ->
                transformGenericParamType com ctx name isMeasure constraints
            | Fable.Tuple(genArgs, isStruct) -> transformTupleType com ctx isStruct genArgs
            | Fable.Nullable(genArg, isStruct) -> transformNullableType com ctx isStruct genArg
            | Fable.Option(genArg, isStruct) -> transformOptionType com ctx isStruct genArg
            | Fable.Array(genArg, _kind) -> transformArrayType com ctx genArg
            | Fable.List genArg -> transformListType com ctx genArg
            | Fable.Regex -> transformRegexType com ctx
            | Fable.AnonymousRecordType(fieldNames, genArgs, isStruct) -> transformTupleType com ctx isStruct genArgs

            // interfaces implemented as the type itself
            | Replacements.Util.IsEntity (Types.iset) (entRef, [ genArg ]) -> transformHashSetType com ctx genArg
            | Replacements.Util.IsEntity (Types.idictionary) (entRef, [ k; v ]) -> transformHashMapType com ctx [ k; v ]
            | Replacements.Util.IsEntity (Types.ireadonlydictionary) (entRef, [ k; v ]) ->
                transformHashMapType com ctx [ k; v ]
            | Replacements.Util.IsEntity (Types.keyCollection) (entRef, [ k; v ]) -> transformArrayType com ctx k
            | Replacements.Util.IsEntity (Types.valueCollection) (entRef, [ k; v ]) -> transformArrayType com ctx v
            | Replacements.Util.IsEntity (Types.icollectionGeneric) (entRef, [ t ]) -> transformArrayType com ctx t

            // pre-defined declared types
            | Replacements.Util.IsEntity (Types.fsharpAsyncGeneric) (_, [ t ]) -> transformAsyncType com ctx t
            | Replacements.Util.IsEntity (Types.taskGeneric) (_, [ t ]) -> transformTaskType com ctx t
            | Replacements.Util.IsEntity (Types.taskBuilder) (_, []) -> transformTaskBuilderType com ctx
            | Replacements.Util.IsEntity (Types.taskBuilderModule) (_, []) -> transformTaskBuilderType com ctx
            | Replacements.Util.IsEntity (Types.thread) (_, []) -> transformThreadType com ctx

            // implemented regex types
            | Replacements.Util.IsEntity (Types.regexMatch) (_, []) -> transformImportType com ctx [] "RegExp" "Match"
            | Replacements.Util.IsEntity (Types.regexGroup) (_, []) -> transformImportType com ctx [] "RegExp" "Group"
            | Replacements.Util.IsEntity (Types.regexCapture) (_, []) ->
                transformImportType com ctx [] "RegExp" "Capture"
            | Replacements.Util.IsEntity (Types.regexMatchCollection) (_, []) ->
                transformImportType com ctx [] "RegExp" "MatchCollection"
            | Replacements.Util.IsEntity (Types.regexGroupCollection) (_, []) ->
                transformImportType com ctx [] "RegExp" "GroupCollection"
            | Replacements.Util.IsEntity (Types.regexCaptureCollection) (_, []) ->
                transformImportType com ctx [] "RegExp" "CaptureCollection"

            | Replacements.Util.IsEnumerator(entRef, genArgs) ->
                // get IEnumerator interface from enumerator object
                match tryFindInterface com Types.ienumeratorGeneric entRef with
                | Some ifc -> transformInterfaceType com ctx ifc.Entity [ Fable.Any ]
                | _ -> failwith "Cannot find IEnumerator interface, should not happen."

            // built-in types
            | Replacements.Util.Builtin kind -> transformBuiltinType com ctx typ kind

            // other declared types
            | Fable.DeclaredType(entRef, genArgs) -> transformEntityType com ctx entRef genArgs

        if (typ = Fable.Any && ctx.InferAnyType) then
            ty // don't wrap inferred types
        else
            let ty =
                match shouldBeRefCountWrapped com ctx typ with
                | Some Lrc -> ty |> makeLrcPtrTy com ctx
                | Some Rc -> ty |> makeRcTy com ctx
                | Some Arc -> ty |> makeArcTy com ctx
                | Some Box -> ty |> makeBoxTy com ctx
                | _ -> ty

            if (isByRefType com typ || ctx.IsParamByRefPreferred) then
                ty |> mkRefTy None
            else
                ty

(*
    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName
            [|
                yield Expression.stringLiteral(fullname)
                match generics with
                | [||] -> yield Util.undefined None
                | generics -> yield Expression.arrayExpression(generics)
                match tryJsConstructor com ctx ent with
                | Some cons -> yield cons
                | None -> ()
                match ent.BaseType with
                | Some d ->
                    let genMap =
                        Seq.zip ent.GenericParameters generics
                        |> Seq.map (fun (p, e) -> p.Name, e)
                        |> Map
                    yield Fable.DeclaredType(d.Entity, d.GenericArgs)
                          |> transformTypeInfo com ctx r genMap
                | None -> ()
            |]
            |> libReflectionCall com ctx r "class"

    let private ofString s = Expression.stringLiteral(s)
    let private ofArray rustExprs = Expression.arrayExpression(List.toArray rustExprs)
*)

module Util =

    // open Lib
    // open Reflection
    open UsageTracking
    open TypeInfo

    let (|TransformExpr|) (com: IRustCompiler) ctx e = com.TransformExpr(ctx, e)

    let (|Function|_|) =
        function
        | Fable.Lambda(arg, body, info) -> Some([ arg ], body, info)
        | Fable.Delegate(args, body, info, []) -> Some(args, body, info)
        | _ -> None

    let (|Lets|_|) =
        function
        | Fable.Let(ident, value, body) -> Some([ ident, value ], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let (|IDisposable|_|) =
        function
        | Replacements.Util.IsEntity (Types.idisposable) _ -> Some()
        | _ -> None

    let (|IFormattable|_|) =
        function
        | Replacements.Util.IsEntity (Types.iformattable) _ -> Some()
        | _ -> None

    let (|IComparable|_|) =
        function
        | Replacements.Util.IsEntity (Types.icomparableGeneric) (_, [ genArg ]) -> Some(genArg)
        | _ -> None

    let (|IEquatable|_|) =
        function
        | Replacements.Util.IsEntity (Types.iequatableGeneric) (_, [ genArg ]) -> Some(genArg)
        | _ -> None

    let (|IEnumerable|_|) =
        function
        | Replacements.Util.IsEntity (Types.ienumerableGeneric) (_, [ genArg ]) -> Some(genArg)
        | Replacements.Util.IsEntity (Types.ienumerable) _ -> Some(Fable.Any)
        | _ -> None

    let isUnitArg (ident: Fable.Ident) =
        ident.IsCompilerGenerated
        && ident.Type = Fable.Unit
        && (ident.DisplayName.StartsWith("unitVar", StringComparison.Ordinal)
            || ident.DisplayName.Contains("@"))

    let discardUnitArg (genArgs: Fable.Type list) (args: Fable.Ident list) =
        match genArgs, args with
        | [ Fable.Unit ], [ arg ] -> args // don't drop unit arg when generic arg is unit
        | _ ->
            match args with
            | [] -> []
            | [ arg ] when isUnitArg arg -> []
            | [ thisArg; arg ] when thisArg.IsThisArgument && isUnitArg arg -> [ thisArg ]
            | args -> args

    /// Fable doesn't currently sanitize attached members/fields so we do a simple sanitation here.
    /// Should this be done in FSharp2Fable step?
    let sanitizeMember (name: string) =
        FSharp2Fable.Helpers.cleanNameAsRustIdentifier name

    let makeUniqueName name (usedNames: Set<string>) =
        name |> Fable.Naming.preventConflicts (usedNames.Contains)

    let getUniqueNameInRootScope (ctx: Context) name =
        let name =
            name
            |> Fable.Naming.preventConflicts (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.DeclarationScopes.Contains(name)
            )

        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name =
            name
            |> Fable.Naming.preventConflicts (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.CurrentDeclarationScope.Contains(name)
            )

        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity(_com: IRustCompiler, ctx, name, args: Fable.Ident list) =
        let args =
            args |> discardUnitArg [] |> List.filter (fun arg -> not (arg.IsThisArgument))

        let label = Fable.Naming.splitLast name

        interface ITailCallOpportunity with
            member _.Label = label
            member _.Args = args

            member _.IsRecursiveRef(e) =
                match e with
                | Fable.IdentExpr ident -> name = ident.Name
                | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwith $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let updateUsageCount com ctx (ident: Fable.Ident) =
        match ctx.ScopedSymbols |> Map.tryFind ident.Name with
        | Some varAttrs ->
            // ident has been seen, subtract 1
            varAttrs.UsageCount <- varAttrs.UsageCount - 1
        | None -> ()

    let transformIdent com ctx r (ident: Fable.Ident) =
        updateUsageCount com ctx ident

        // prevents emitting self on inlined code
        if ident.IsThisArgument && ctx.IsAssocMember then
            makeSelf com ctx r ident.Type
        else
            mkGenericPathExpr (splitNameParts ident.Name) None

    let isThisArgumentIdentExpr (ctx: Context) (expr: Fable.Expr) =
        match expr with
        | Fable.IdentExpr ident -> ident.IsThisArgument && ctx.IsAssocMember
        | _ -> false

    // let transformExprMaybeIdentExpr (com: IRustCompiler) ctx (expr: Fable.Expr) =
    //     match expr with
    //     | Fable.IdentExpr ident when ident.IsThisArgument && ctx.IsAssocMember ->
    //         // avoids the extra Lrc wrapping for self that transformIdentGet does
    //         transformIdent com ctx None id
    //     | _ -> com.TransformExpr(ctx, expr)

    let transformIdentGet com ctx r (ident: Fable.Ident) =
        let expr = transformIdent com ctx r ident

        if ident.IsMutable && not (isInRefType com ident.Type) then
            expr |> mutableGet
        elif isBoxScoped ctx ident.Name then
            expr |> makeLrcPtrValue com ctx
        // elif isRefScoped ctx ident.Name then
        //     expr |> makeClone // |> mkDerefExpr |> mkParenExpr
        else
            expr

    let transformIdentSet com ctx r (ident: Fable.Ident) (value: Rust.Expr) =
        // assert (ident.IsMutable)
        let expr = transformIdent com ctx r ident
        mutableSet expr value

    let transformIdentType com ctx isCaptured (ident: Fable.Ident) =
        let ty = transformType com ctx ident.Type

        if isByRefType com ident.Type then
            ty // already wrapped
        elif ident.IsMutable && isCaptured then
            ty |> makeMutTy com ctx |> makeLrcPtrTy com ctx
        elif ident.IsMutable then
            ty |> makeMutTy com ctx
        else
            ty

    let getField r (expr: Rust.Expr) (fieldName: string) =
        mkFieldExpr expr (fieldName |> sanitizeMember) // ?loc=r)

    let getExpr r (expr: Rust.Expr) (index: Rust.Expr) = mkIndexExpr expr index // ?loc=r)

    let callFunction com ctx r (callee: Rust.Expr) (args: Fable.Expr list) =
        let trArgs = transformCallArgs com ctx args [] []
        mkCallExpr callee trArgs // ?loc=r)

    // /// Immediately Invoked Function Expression
    // let iife (com: IRustCompiler) ctx (expr: Fable.Expr) =
    //     let fnExpr = transformLambda com ctx None [] expr
    //     let range = None // TODO:
    //     callFunction com ctx range fnExpr []

    let getNewGenArgsAndCtx (ctx: Context) (args: Fable.Ident list) (body: Fable.Expr) =
        let isLambdaOrGenArgNotInScope name =
            ctx.IsLambda || not (Set.contains name ctx.ScopedEntityGenArgs)

        let isNotLambdaOrGenArgInScope name =
            not (ctx.IsLambda)
            || (Set.contains name ctx.ScopedEntityGenArgs)
            || (Set.contains name ctx.ScopedMemberGenArgs)

        match body with
        | Fable.Call(callee, info, t, r) when ctx.IsLambda ->
            // for lambdas, get generic args from the call info
            let genArgs = info.GenericArgs
            genArgs, ctx
        | _ ->
            // otherwise get the genArgs from args and return types
            let argTypes = args |> List.map (fun arg -> arg.Type)

            let genParams =
                argTypes @ [ body.Type ]
                |> FSharp2Fable.Util.getGenParams
                |> List.filter (fst >> isLambdaOrGenArgNotInScope)
                |> List.filter (fst >> isNotLambdaOrGenArgInScope)

            let genArgTypes = genParams |> List.map snd
            let genArgNames = genParams |> List.map fst |> Set.ofList

            let ctx =
                if ctx.IsLambda then
                    ctx
                else
                    { ctx with ScopedMemberGenArgs = genArgNames }

            genArgTypes, ctx

    let getCellType =
        function
        | Replacements.Util.Builtin(Replacements.Util.FSharpReference t) -> t
        | t -> t

    let optimizeTailCall com ctx r (tc: ITailCallOpportunity) (args: Fable.Expr list) : Rust.Expr =
        let tempArgs =
            tc.Args
            |> List.map (fun arg ->
                { arg with
                    Name = arg.Name + "_temp"
                    IsMutable = false
                    Type = getCellType arg.Type
                }
            )

        let bindings = List.zip tempArgs args
        let emptyBody = Fable.Sequential []

        let tempLetStmts, ctx = makeLetStmts com ctx bindings emptyBody Map.empty

        let setArgStmts =
            List.zip tc.Args tempArgs
            |> List.map (fun (id, idTemp) ->
                let value = transformIdentGet com ctx r idTemp
                transformIdentSet com ctx r id value |> mkExprStmt
            )

        let continueStmt = mkContinueExpr (Some tc.Label) |> mkExprStmt
        tempLetStmts @ setArgStmts @ [ continueStmt ] |> mkStmtBlockExpr

    let makeInterfaceCast com ctx typ (expr: Rust.Expr) : Rust.Expr =
        match typ with
        | IsNonErasedInterface com (entRef, genArgs) ->
            let ifcTy = transformEntityType com ctx entRef genArgs |> makeCastTy com ctx
            let macroName = getLibraryImportName com ctx "Native" "interface_cast"

            [ mkExprToken expr; mkTyToken ifcTy ]
            |> mkParensCommaDelimitedMacCall macroName
            |> mkMacCallExpr
        | _ -> expr

    let transformCast (com: IRustCompiler) (ctx: Context) typ (fableExpr: Fable.Expr) : Rust.Expr =
        // search the typecast chain for a matching type
        let rec getNestedExpr typ expr =
            match expr with
            | Fable.TypeCast(e, t) when t <> typ -> getNestedExpr t e
            | _ -> expr

        let nestedExpr = getNestedExpr typ fableExpr

        let fableExpr =
            // optimization to eliminate unnecessary casts
            if nestedExpr.Type = typ then
                nestedExpr
            else
                fableExpr

        let fromType, toType = fableExpr.Type, typ
        let expr = transformLeaveContext com ctx (Some typ) fableExpr
        let ty = transformType com ctx typ

        match fromType, toType with
        | t1, t2 when t1 = t2 -> expr // no cast needed if types are the same
        | Fable.Number _, Fable.Number _ -> expr |> mkCastExpr ty
        | Fable.Char, Fable.Number(UInt32, Fable.NumberInfo.Empty) -> expr |> mkCastExpr ty
        | Fable.Tuple(ga1, false), Fable.Tuple(ga2, true) when ga1 = ga2 -> expr |> makeAsRef |> makeClone //.ToValueTuple()
        | Fable.Tuple(ga1, true), Fable.Tuple(ga2, false) when ga1 = ga2 -> expr |> makeLrcPtrValue com ctx //.ToTuple()

        // casts to IEnumerable
        | Replacements.Util.IsEntity (Types.keyCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.valueCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.icollectionGeneric) _, IEnumerable _
        | Fable.Array _, IEnumerable _ -> makeLibCall com ctx None "Seq" "ofArray" [ expr ]
        | Fable.List _, IEnumerable _ -> makeLibCall com ctx None "Seq" "ofList" [ expr ]
        | Fable.String, IEnumerable _ ->
            let chars = makeLibCall com ctx None "String" "toCharArray" [ expr ]
            makeLibCall com ctx None "Seq" "ofArray" [ chars ]
        | Replacements.Util.IsEntity (Types.hashset) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.iset) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "HashSet" "entries" [ expr ]
            makeLibCall com ctx None "Seq" "ofArray" [ ar ]
        | Replacements.Util.IsEntity (Types.dictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.idictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.ireadonlydictionary) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "HashMap" "entries" [ expr ]
            makeLibCall com ctx None "Seq" "ofArray" [ ar ]

        // boxing value types or wrapped types
        | t, Fable.Any when isValueType com t || isWrappedType com t -> expr |> boxValue com ctx

        // unboxing value types or wrapped types
        | Fable.Any, t when isValueType com t || isWrappedType com t -> expr |> unboxValue com ctx t

        // casts to generic param
        | _, Fable.GenericParam(name, _isMeasure, _constraints) -> makeCall (name :: "from" :: []) None [ expr ] // e.g. T::from(value)

        // casts to IDictionary, for now does nothing // TODO: fix it
        | Replacements.Util.IsEntity (Types.dictionary) _, Replacements.Util.IsEntity (Types.idictionary) _ -> expr

        // casts from object to interface
        | t1, t2 when not (isInterface com t1) && (isInterface com t2) -> makeInterfaceCast com ctx t2 expr

        // casts from interface to interface
        | _, t when isInterface com t -> expr |> makeClone |> mkCastExpr ty //TODO: not working, implement

        // // casts to System.Object
        // | _, Fable.Any ->
        //     let ty = transformType com ctx toType
        //     expr |> mkCastExpr (ty |> mkRefTy None)

        // TODO: other casts?
        | _ ->
            //TODO: add warning?
            expr // no cast is better than error

    /// This guarantees a new owned Rc<T>
    let makeClone expr =
        mkMethodCallExprOnce "clone" None expr []

    let makeUnwrap expr =
        mkMethodCallExprOnce "unwrap" None expr []

    /// Calling this on an rc guarantees a &T, regardless of if the Rc is a ref or not
    let makeAsRef expr = mkMethodCallExpr "as_ref" None expr []

    let makeCall pathNames genArgs (args: Rust.Expr list) =
        let callee = mkGenericPathExpr pathNames genArgs
        mkCallExpr callee args

    let makeNew com ctx moduleName typeName (values: Rust.Expr list) =
        let importName = getLibraryImportName com ctx moduleName typeName
        makeCall (importName :: "new" :: []) None values

    // let makeFrom com ctx moduleName typeName (value: Rust.Expr) =
    //     let importName = getLibraryImportName com ctx moduleName typeName
    //     makeCall [importName; "from"] None [value]

    let boxValue com ctx (value: Rust.Expr) =
        [ value ] |> makeLibCall com ctx None "Native" "box_"

    let unboxValue com ctx typ (value: Rust.Expr) =
        let genArgsOpt = transformGenArgs com ctx [ typ ]
        [ value ] |> makeLibCall com ctx genArgsOpt "Native" "unbox"

    let makeFluentValue com ctx (value: Rust.Expr) =
        [ value ] |> makeLibCall com ctx None "Native" "fromFluent"

    let makeLrcPtrValue com ctx (value: Rust.Expr) =
        [ value ] |> makeNew com ctx "Native" "LrcPtr"

    // let makeLrcValue com ctx (value: Rust.Expr) =
    //     [ value ] |> makeNew com ctx "Native" "Lrc"

    let makeRcValue com ctx (value: Rust.Expr) =
        [ value ] |> makeNew com ctx "Native" "Rc"

    let makeArcValue com ctx (value: Rust.Expr) =
        [ value ] |> makeNew com ctx "Native" "Arc"

    let makeBoxValue com ctx (value: Rust.Expr) =
        [ value ] |> makeNew com ctx "Native" (rawIdent "Box")

    let makeMutValue com ctx (value: Rust.Expr) =
        [ value ] |> makeNew com ctx "Native" "MutCell"

    let makeFuncValue com ctx (ident: Fable.Ident) =
        let argTypes =
            match FableTransforms.uncurryType ident.Type with
            | Fable.LambdaType(argType, returnType) -> [ argType ]
            | Fable.DelegateType(argTypes, returnType) -> argTypes
            | _ -> []

        let argTypes =
            match argTypes with
            | [ Fable.Unit ] -> []
            | _ -> argTypes

        let argCount = string<int> (List.length argTypes)
        let funcWrap = getLibraryImportName com ctx "Native" ("Func" + argCount)
        let expr = transformIdent com ctx None ident

        makeCall (funcWrap :: "from" :: []) None [ expr ]

    let maybeWrapSmartPtr com ctx ent expr =
        match ent with
        | HasReferenceTypeAttribute a ->
            match a with
            | Lrc -> expr |> makeLrcPtrValue com ctx
            | Rc -> expr |> makeRcValue com ctx
            | Arc -> expr |> makeArcValue com ctx
            | Box -> expr |> makeBoxValue com ctx
        | _ ->
            match ent.FullName with
            | Types.fsharpAsyncGeneric
            | Types.task
            | Types.taskGeneric -> expr |> makeArcValue com ctx
            | Types.result -> expr
            | _ ->
                if ent.IsValueType then
                    expr
                else
                    expr |> makeLrcPtrValue com ctx

    let parameterIsByRefPreferred idx (parameters: Fable.Parameter list) =
        parameters
        |> List.tryItem idx
        |> Option.map (fun p -> p.Attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef))
        |> Option.defaultValue false

    let transformCallArgs
        com
        ctx
        (args: Fable.Expr list)
        (argTypes: Fable.Type list)
        (parameters: Fable.Parameter list)
        =
        match args with
        | [] -> []
        // | args when hasSpread ->
        //     match List.rev args with
        //     | [] -> []
        //     | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformExpr(ctx, e))
        //         rest @ (List.map (fun e -> com.TransformExpr(ctx, e)) spreadArgs)
        //     | last::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformExpr(ctx, e))
        //         rest @ [Expression.spreadElement(com.TransformExpr(ctx, last))]
        | args ->
            let argsWithTypes =
                if argTypes.Length = args.Length then
                    args |> List.zip argTypes |> List.map (fun (t, a) -> Some t, a)
                else
                    args |> List.map (fun a -> None, a)

            argsWithTypes
            |> List.mapi (fun i (argType, arg) ->
                match arg with
                | Fable.IdentExpr ident when isFuncScoped ctx ident.Name -> makeFuncValue com ctx ident // local nested function ident
                | _ ->
                    let isByRefPreferred = parameterIsByRefPreferred i parameters

                    let ctx =
                        { ctx with IsParamByRefPreferred = isByRefPreferred || ctx.IsParamByRefPreferred }

                    transformLeaveContext com ctx argType arg
            )

    let makeRefForPatternMatch (com: IRustCompiler) ctx typ (nameOpt: string option) fableExpr =
        let expr = com.TransformExpr(ctx, fableExpr)

        if isThisArgumentIdentExpr ctx fableExpr then
            expr
        elif isInRefType com typ then
            expr
        elif nameOpt.IsSome && isRefScoped ctx nameOpt.Value then
            expr
        elif shouldBeRefCountWrapped com ctx typ |> Option.isSome then
            expr |> makeAsRef
        else
            expr |> mkAddrOfExpr

    let negateWhen isNegative expr =
        if isNegative then
            expr |> mkNegExpr
        else
            expr

    let makeNumber com ctx r t (v: Fable.NumberValue) =
        match v with
        | Fable.NumberValue.Int8 x when x = System.SByte.MinValue -> mkGenericPathExpr ("i8" :: "MIN" :: []) None
        | Fable.NumberValue.Int8 x when x = System.SByte.MaxValue -> mkGenericPathExpr ("i8" :: "MAX" :: []) None
        | Fable.NumberValue.Int16 x when x = System.Int16.MinValue -> mkGenericPathExpr ("i16" :: "MIN" :: []) None
        | Fable.NumberValue.Int16 x when x = System.Int16.MaxValue -> mkGenericPathExpr ("i16" :: "MAX" :: []) None
        | Fable.NumberValue.Int32 x when x = System.Int32.MinValue -> mkGenericPathExpr ("i32" :: "MIN" :: []) None
        | Fable.NumberValue.Int32 x when x = System.Int32.MaxValue -> mkGenericPathExpr ("i32" :: "MAX" :: []) None
        | Fable.NumberValue.Int64 x when x = System.Int64.MinValue -> mkGenericPathExpr ("i64" :: "MIN" :: []) None
        | Fable.NumberValue.Int64 x when x = System.Int64.MaxValue -> mkGenericPathExpr ("i64" :: "MAX" :: []) None
        // | Fable.NumberValue.Int128 x when x = System.Int128.MinValue ->
        //     mkGenericPathExpr ("i128"::"MIN"::[]) None
        // | Fable.NumberValue.Int128 x when x = System.Int128.MaxValue ->
        //     mkGenericPathExpr ("i128"::"MAX"::[]) None

        // | Fable.NumberValue.UInt8 x when x = System.Byte.MinValue ->
        //     mkGenericPathExpr ("u8"::"MIN"::[]) None
        | Fable.NumberValue.UInt8 x when x = System.Byte.MaxValue -> mkGenericPathExpr ("u8" :: "MAX" :: []) None
        // | Fable.NumberValue.UInt16 x when x = System.UInt16.MinValue ->
        //     mkGenericPathExpr ("u16"::"MIN"::[]) None
        | Fable.NumberValue.UInt16 x when x = System.UInt16.MaxValue -> mkGenericPathExpr ("u16" :: "MAX" :: []) None
        // | Fable.NumberValue.UInt32, (:? uint32 as x) when x = System.UInt32.MinValue ->
        //     mkGenericPathExpr ("u32"::"MIN"::[]) None
        | Fable.NumberValue.UInt32 x when x = System.UInt32.MaxValue -> mkGenericPathExpr ("u32" :: "MAX" :: []) None
        // | Fable.NumberValue.UInt64 x when x = System.UInt64.MinValue ->
        //     mkGenericPathExpr ("u64"::"MIN"::[]) None
        | Fable.NumberValue.UInt64 x when x = System.UInt64.MaxValue -> mkGenericPathExpr ("u64" :: "MAX" :: []) None
        // | Fable.NumberValue.UInt128 x when x = System.UInt128.MinValue ->
        //     mkGenericPathExpr ("u128"::"MIN"::[]) None
        // | Fable.NumberValue.UInt128 x when x = System.UInt128.MaxValue ->
        //     mkGenericPathExpr ("u128"::"MAX"::[]) None

        | Fable.NumberValue.Float32 x when System.Single.IsNaN(x) -> mkGenericPathExpr ("f32" :: "NAN" :: []) None
        | Fable.NumberValue.Float64 x when System.Double.IsNaN(x) -> mkGenericPathExpr ("f64" :: "NAN" :: []) None
        | Fable.NumberValue.Float32 x when System.Single.IsPositiveInfinity(x) ->
            mkGenericPathExpr ("f32" :: "INFINITY" :: []) None
        | Fable.NumberValue.Float64 x when System.Double.IsPositiveInfinity(x) ->
            mkGenericPathExpr ("f64" :: "INFINITY" :: []) None
        | Fable.NumberValue.Float32 x when System.Single.IsNegativeInfinity(x) ->
            mkGenericPathExpr ("f32" :: "NEG_INFINITY" :: []) None
        | Fable.NumberValue.Float64 x when System.Double.IsNegativeInfinity(x) ->
            mkGenericPathExpr ("f64" :: "NEG_INFINITY" :: []) None

        | Fable.NumberValue.NativeInt x ->
            let expr = mkIsizeLitExpr (abs x |> string<nativeint>)
            expr |> negateWhen (x < 0n)
        | Fable.NumberValue.Int8 x ->
            let expr = mkInt8LitExpr (abs x |> string<int8>)
            expr |> negateWhen (x < 0y)
        | Fable.NumberValue.Int16 x ->
            let expr = mkInt16LitExpr (abs x |> string<int16>)
            expr |> negateWhen (x < 0s)
        | Fable.NumberValue.Int32 x ->
            let expr = mkInt32LitExpr (abs x |> string<int32>)
            expr |> negateWhen (x < 0)
        | Fable.NumberValue.Int64 x ->
            let expr = mkInt64LitExpr (abs x |> string<int64>)
            expr |> negateWhen (x < 0L)
        | Fable.NumberValue.Int128(upper, lower) ->
            let bytes =
                Array.concat [ BitConverter.GetBytes(lower); BitConverter.GetBytes(upper) ] // little endian

            let big = Numerics.BigInteger(bytes)
            let s = string<Numerics.BigInteger> big
            let expr = mkInt128LitExpr (s.TrimStart('-'))
            expr |> negateWhen (s.StartsWith("-", StringComparison.Ordinal))
        | Fable.NumberValue.UNativeInt x -> mkUsizeLitExpr (x |> string<unativeint>)
        | Fable.NumberValue.UInt8 x -> mkUInt8LitExpr (x |> string<uint8>)
        | Fable.NumberValue.UInt16 x -> mkUInt16LitExpr (x |> string<uint16>)
        | Fable.NumberValue.UInt32 x -> mkUInt32LitExpr (x |> string<uint32>)
        | Fable.NumberValue.UInt64 x -> mkUInt64LitExpr (x |> string<uint64>)
        | Fable.NumberValue.UInt128(upper, lower) ->
            let bytes =
                Array.concat [ BitConverter.GetBytes(lower); BitConverter.GetBytes(upper); [| 0uy |] ] // little endian

            let big = Numerics.BigInteger(bytes)
            mkUInt128LitExpr (string<Numerics.BigInteger> big)
        | Fable.NumberValue.Float16 x ->
            let expr = mkFloat32LitExpr (abs x |> string<float32>)
            expr |> negateWhen (x < 0.0f)
        | Fable.NumberValue.Float32 x ->
            let expr = mkFloat32LitExpr (abs x |> string<float32>)
            expr |> negateWhen (x < 0.0f)
        | Fable.NumberValue.Float64 x ->
            let expr = mkFloat64LitExpr (abs x |> string<float>)
            expr |> negateWhen (x < 0.0)
        | Fable.NumberValue.Decimal x -> Replacements.makeDecimal com r t x |> transformExpr com ctx
        | _ ->
            $"Numeric literal is not supported: %A{v}" |> addError com [] r

            mkFloat64LitExpr (string<float> 0.)

    let makeStaticString com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "String" "string" [ value ]

    let makeStringFrom com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "String" "fromString" [ value ]

    let makeNullCheck com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "Native" "is_null" [ value |> mkAddrOfExpr ]

    let makeNull com ctx (typ: Fable.Type) =
        let typ =
            match typ with
            | Fable.Any -> Fable.Unit
            | t -> t

        let genArgsOpt = transformGenArgs com ctx [ typ ]
        makeLibCall com ctx genArgsOpt "Native" "null" []

    let makeInit com ctx (typ: Fable.Type) =
        let genArgsOpt = transformGenArgs com ctx [ typ ]
        makeLibCall com ctx genArgsOpt "Native" "getZero" []

    let makeDefault com ctx (typ: Fable.Type) =
        let genArgsOpt = transformGenArgs com ctx [ typ ]
        makeLibCall com ctx genArgsOpt "Native" "defaultOf" []

    let makeOption (com: IRustCompiler) ctx r typ value isStruct =
        let expr =
            match value with
            | Some arg ->
                let callee = mkGenericPathExpr [ rawIdent "Some" ] None
                callFunction com ctx r callee [ arg ]
            | None ->
                let genArgsOpt = transformGenArgs com ctx [ typ ]
                mkGenericPathExpr [ rawIdent "None" ] genArgsOpt
        // if isStruct
        // then expr
        // else expr |> makeLrcPtrValue com ctx
        expr // all options are value options

    let makeArray (com: IRustCompiler) ctx r typ (exprs: Fable.Expr list) =
        match exprs with
        | [] ->
            let genArgsOpt = transformGenArgs com ctx [ typ ]
            makeLibCall com ctx genArgsOpt "NativeArray" "new_empty" []
        | _ ->
            let arrayExpr =
                exprs
                |> List.map (transformLeaveContext com ctx None)
                |> mkArrayExpr
                |> mkAddrOfExpr

            makeLibCall com ctx None "NativeArray" "new_array" [ arrayExpr ]

    let makeArrayFrom (com: IRustCompiler) ctx r typ fableExpr =
        match fableExpr with
        | Fable.Value(Fable.NewTuple([ valueExpr; countExpr ], isStruct), _) ->
            let value = transformExpr com ctx valueExpr |> mkAddrOfExpr
            let count = transformExpr com ctx countExpr

            makeLibCall com ctx None "NativeArray" "new_init" [ value; count ]
        | expr ->
            // this assumes expr converts to a slice
            // TODO: this may not always work, make it work
            let sequence = transformExpr com ctx expr |> mkAddrOfExpr
            makeLibCall com ctx None "NativeArray" "new_array" [ sequence ]

    let makeList (com: IRustCompiler) ctx r typ headAndTail =
        // // list contruction with cons
        // match headAndTail with
        // | None ->
        //     libCall com ctx r [typ] "List" "empty" []
        // | Some(head, Fable.Value(Fable.NewList(None, _), _)) ->
        //     libCall com ctx r [] "List" "singleton" [head]
        // | Some(head, tail) ->
        //     libCall com ctx r [] "List" "cons" [head; tail]

        // list construction with List.ofArray
        let rec getItems acc =
            function
            | None -> List.rev acc, None
            | Some(head, Fable.Value(Fable.NewList(tail, _), _)) -> getItems (head :: acc) tail
            | Some(head, tail) -> List.rev (head :: acc), Some tail

        let makeNewArray r typ exprs =
            Fable.Value(Fable.NewArray(Fable.ArrayValues exprs, typ, Fable.MutableArray), r)

        match getItems [] headAndTail with
        | [], None -> libCall com ctx r [ typ ] "List" "empty" []
        | [ expr ], None -> libCall com ctx r [] "List" "singleton" [ expr ]
        | exprs, None -> [ makeNewArray r typ exprs ] |> libCall com ctx r [] "List" "ofArray"
        | [ head ], Some tail -> libCall com ctx r [] "List" "cons" [ head; tail ]
        | exprs, Some tail ->
            [ makeNewArray r typ exprs; tail ]
            |> libCall com ctx r [] "List" "ofArrayWithTail"

    let makeTuple (com: IRustCompiler) ctx r isStruct (exprs: (Fable.Expr) list) =
        let expr = exprs |> List.map (transformLeaveContext com ctx None) |> mkTupleExpr

        if isStruct then
            expr
        else
            expr |> makeLrcPtrValue com ctx

    let makeRecord (com: IRustCompiler) ctx r values entRef genArgs =
        let ent = com.GetEntity(entRef)
        let idents = getEntityFieldsAsIdents com ent

        let fields =
            List.zip idents values
            |> List.map (fun (ident, value) ->
                let expr = transformLeaveContext com ctx None value

                let expr =
                    if ident.IsMutable then
                        expr |> makeMutValue com ctx
                    else
                        expr

                let attrs = []
                let fieldName = ident.Name |> sanitizeMember
                mkExprField attrs fieldName expr false false
            )

        let phantomFields =
            getEntityPhantomGenParams com ent
            |> List.map (fun (name, typ) ->
                let genArgsOpt = transformGenArgs com ctx [ typ ]
                let expr = mkGenericPathExpr ("core" :: "marker" :: "PhantomData" :: []) genArgsOpt
                let fieldName = $"phantom_%s{name}" |> sanitizeMember
                mkExprField [] fieldName expr false false
            )

        let fields = List.append fields phantomFields

        let genArgsOpt = transformGenArgs com ctx genArgs
        let entName = getEntityFullName com ctx entRef
        let path = makeFullNamePath entName genArgsOpt
        let expr = mkStructExpr path fields // TODO: range
        expr |> maybeWrapSmartPtr com ctx ent

    let tryUseKnownUnionCaseNames fullName =
        match fullName with
        | "FSharp.Core.FSharpResult`2.Ok" -> rawIdent "Ok" |> Some
        | "FSharp.Core.FSharpResult`2.Error" -> rawIdent "Err" |> Some
        | _ ->
            if fullName.StartsWith("FSharp.Core.FSharpChoice`", StringComparison.Ordinal) then
                fullName |> Fable.Naming.replacePrefix "FSharp.Core.FSharp" "" |> Some
            else
                None

    let getUnionCaseName com ctx entRef (unionCase: Fable.UnionCase) =
        tryUseKnownUnionCaseNames unionCase.FullName
        |> Option.defaultWith (fun () ->
            let entName = getEntityFullName com ctx entRef
            entName + "::" + unionCase.Name
        )

    // let getUnionCaseFields com ctx name caseIndex (unionCase: Fable.UnionCase) =
    //     unionCase.UnionCaseFields
    //     |> List.mapi (fun i _field ->
    //         let fieldName = $"{name}_{caseIndex}_{i}"
    //         let fieldType = FableTransforms.uncurryType field.FieldType
    //         makeTypedIdent fieldType fieldName
    //     )

    let makeUnion (com: IRustCompiler) ctx r values tag entRef genArgs =
        let ent = com.GetEntity(entRef)
        let genArgsOpt = transformGenArgs com ctx genArgs
        let unionCase = ent.UnionCases |> List.item tag
        let unionCaseName = getUnionCaseName com ctx entRef unionCase
        let callee = makeFullNamePathExpr unionCaseName genArgsOpt

        let expr =
            if List.isEmpty values then
                callee
            else
                callFunction com ctx r callee values

        expr |> maybeWrapSmartPtr com ctx ent

    let baseName = "__base__"
    let selfName = "__self__"

    let makeBase (com: IRustCompiler) ctx r _typ = mkGenericPathExpr [ baseName ] None
    let makeSelf (com: IRustCompiler) ctx r _typ = mkGenericPathExpr [ selfName ] None

    let makeBaseValue (com: IRustCompiler) ctx r ident =
        let expr = transformIdent com ctx r ident
        getField r expr baseName

    let makeFormatString (parts: string list) =
        let sb = System.Text.StringBuilder()
        sb.Append(List.head parts) |> ignore

        List.tail parts
        |> List.iteri (fun i part -> sb.Append($"{{{i}}}" + part) |> ignore)

        sb.ToString()

    let makeFormatExpr (com: IRustCompiler) ctx fmt values : Rust.Expr =
        let unboxedArgs = values |> FSharp2Fable.Util.unboxBoxedArgs
        let args = transformCallArgs com ctx unboxedArgs [] []
        let fmtArgs = (mkStrLitExpr fmt) :: args
        makeLibCall com ctx None "String" "sprintf!" fmtArgs

    let makeStringTemplate (com: IRustCompiler) ctx parts values : Rust.Expr =
        let fmt = makeFormatString parts
        makeFormatExpr com ctx fmt values

    let makeTypeInfo (com: IRustCompiler) ctx r (typ: Fable.Type) : Rust.Expr =
        let importName = getLibraryImportName com ctx "Reflection" "TypeId"
        let genArgsOpt = transformGenArgs com ctx [ typ ]
        makeFullNamePathExpr importName genArgsOpt

    let transformValue (com: IRustCompiler) (ctx: Context) r value : Rust.Expr =
        let unimplemented () =
            $"Value %A{value} is not implemented yet" |> addWarning com [] None
            TODO_EXPR $"%A{value}"

        match value with
        | Fable.BaseValue(None, typ) -> makeBase com ctx r typ
        | Fable.BaseValue(Some ident, _) -> makeBaseValue com ctx r ident
        | Fable.ThisValue typ -> makeSelf com ctx r typ
        | Fable.TypeInfo(typ, _tags) -> makeTypeInfo com ctx r typ
        | Fable.Null typ -> makeNull com ctx typ
        | Fable.UnitConstant -> mkUnitExpr ()
        | Fable.BoolConstant b -> mkBoolLitExpr b //, ?loc=r)
        | Fable.CharConstant c -> mkCharLitExpr c //, ?loc=r)
        | Fable.StringConstant s -> mkStrLitExpr s |> makeStaticString com ctx
        | Fable.StringTemplate(_tag, parts, values) -> makeStringTemplate com ctx parts values
        | Fable.NumberConstant(x, _) -> makeNumber com ctx r value.Type x
        | Fable.RegexConstant(source, flags) ->
            // Expression.regExpLiteral(source, flags, ?loc=r)
            unimplemented ()
        | Fable.NewArray(Fable.ArrayValues values, typ, _kind) -> makeArray com ctx r typ values
        | Fable.NewArray((Fable.ArrayFrom expr | Fable.ArrayAlloc expr), typ, _kind) -> makeArrayFrom com ctx r typ expr
        | Fable.NewTuple(values, isStruct) -> makeTuple com ctx r isStruct values
        | Fable.NewList(headAndTail, typ) -> makeList com ctx r typ headAndTail
        | Fable.NewOption(value, typ, isStruct) -> makeOption com ctx r typ value isStruct
        | Fable.NewRecord(values, entRef, genArgs) -> makeRecord com ctx r values entRef genArgs
        | Fable.NewAnonymousRecord(values, fieldNames, genArgs, isStruct) -> makeTuple com ctx r isStruct values
        | Fable.NewUnion(values, tag, entRef, genArgs) -> makeUnion com ctx r values tag entRef genArgs

    // let calcVarAttrsAndOnlyRef com ctx (e: Fable.Expr) =
    //     let t = e.Type
    //     let name = getIdentName e
    //     let varAttrs =
    //         ctx.ScopedSymbols   // todo - cover more than just root level idents
    //         |> Map.tryFind name
    //         |> Option.defaultValue {
    //             IsArm = false
    //             IsRef = false
    //             IsBox = false
    //             IsFunc = false
    //             UsageCount = 9999 }
    //     let isOnlyReference =
    //         match e with
    //         | Fable.Let _ -> true
    //         | Fable.Call _ ->
    //             //if the source is the returned value of a function, it is never bound, so we can assume this is the only reference
    //             true
    //         | Fable.CurriedApply _ -> true
    //         | Fable.Value(kind, r) ->
    //             //an inline value kind is also never bound, so can assume this is the only reference also
    //             true
    //         | Fable.Operation(Fable.Binary _, _, _, _) ->
    //             true //Anything coming out of an operation is as good as being returned from a function
    //         | Fable.Lambda _
    //         | Fable.Delegate _ ->
    //             true
    //         | Fable.IfThenElse _
    //         | Fable.DecisionTree _
    //         | Fable.DecisionTreeSuccess _
    //         | Fable.Sequential _
    //         | Fable.ForLoop _
    //             -> true // All control constructs in f# return expressions, and as return statements are always take ownership, we can assume this is already owned, and not bound
    //         | _ ->
    //             if ctx.HasMultipleUses then
    //                 false
    //                 // If an owned value is captured, it must be cloned or it will turn a closure into a FnOnce (as value is consumed on first call).
    //                 // If an owned value leaves scope inside a for loop, it can also not be assumed to be the only usage, as there are multiple instances of that expression invocation at runtime
    //             else varAttrs.UsageCount < 2
    //     varAttrs, isOnlyReference

    let transformLeaveContext (com: IRustCompiler) ctx (tOpt: Fable.Type option) (e: Fable.Expr) : Rust.Expr =
        // let varAttrs, isOnlyRef = calcVarAttrsAndOnlyRef com ctx e

        let implCopy = typeImplementsCopyTrait com ctx e.Type
        let implClone = typeImplementsCloneTrait com ctx e.Type

        let sourceIsRef =
            match e with
            | Fable.Get(Fable.IdentExpr ident, _, _, _) -> isArmScoped ctx ident.Name
            | MaybeCasted(Fable.IdentExpr ident) -> isRefScoped ctx ident.Name
            | _ -> false

        let targetIsRef = ctx.IsParamByRefPreferred //&& not implCopy
        // || Option.exists (isByRefType com) tOpt
        // || isAddrOfExpr e

        let mustClone =
            match e with
            | MaybeCasted(Fable.IdentExpr ident) ->
                // clone non-mutable idents if used more than once
                not (ident.IsMutable) && not (isUsedOnce ctx ident.Name) //&& not (isByRefType com ident.Type)
            | Fable.Get(_, Fable.FieldGet _, _, _) -> true // always clone field get exprs
            | Fable.Get(_, Fable.UnionField _, _, _) -> true // always clone union case get exprs
            // | Fable.Get(_, _, _, _) -> //TODO: clone other gets ???
            | _ -> false

        let isUnreachable =
            match e with
            | Fable.Emit _
            | Fable.Extended _ -> true
            | _ -> false

        // Careful moving this, as idents mutably subtract their count as they are seen, so ident transforming must happen AFTER checking
        let expr =
            // match e with
            // | Fable.IdentExpr ident when isIdentAtTailPos (fun i -> isByRefType com i.Type) e ->
            //     transformIdent com ctx None ident
            // | _ ->
            //only valid for this level, so must reset for nested expressions
            let ctx = { ctx with IsParamByRefPreferred = false }
            com.TransformExpr(ctx, e)

        match sourceIsRef, targetIsRef, implCopy, implClone, mustClone, isUnreachable with
        | true, true, _, _, _, false -> expr
        | false, true, _, _, _, false -> expr |> mkAddrOfExpr
        | true, false, _, _, _, false -> expr |> makeClone
        | false, false, false, true, true, false -> expr |> makeClone
        | _ -> expr
    // |> BLOCK_COMMENT_SUFFIX (sprintf "sourceIsRef: %b, targetIsRef: %b, implCopy: %b, implClone: %b, mustClone: %b, isUnreachable: %b" sourceIsRef targetIsRef implCopy implClone mustClone isUnreachable)


    // let extractBaseExprFromBaseCall (com: IRustCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
    //     match baseCall, baseType with
    //     | Some(Fable.Call(baseRef, info, _, _)), _ ->
    //         let baseExpr =
    //             match baseRef with
    //             | Fable.IdentExpr ident -> typedIdent com ctx ident |> Expression.Identifier
    //             | _ -> transformExpr com ctx baseRef
    //         let args = transformCallArgs com ctx info.Args
    //         Some(baseExpr, args)
    //     | Some(Fable.Value _), Some baseType ->
    //         // let baseEnt = com.GetEntity(baseType.Entity)
    //         // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
    //         // let entityType = FSharp2Fable.Util.getEntityType baseEnt
    //         // let baseRefId = makeTypedIdent entityType entityName
    //         // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
    //         // Some(baseExpr, []) // default base constructor
    //         let range = baseCall |> Option.bind (fun x -> x.Range)
    //         $"Ignoring base call for %s{baseType.Entity.FullName}" |> addWarning com [] range
    //         None
    //     | Some _, _ ->
    //         let range = baseCall |> Option.bind (fun x -> x.Range)
    //         "Unexpected base call expression, please report" |> addError com [] range
    //         None
    //     | None, _ ->
    //         None

    let getDeclMember (com: IRustCompiler) (decl: Fable.MemberDecl) =
        decl.ImplementedSignatureRef
        |> Option.defaultValue decl.MemberRef
        |> com.GetMember

    let getDistinctInterfaceMembers (com: IRustCompiler) (ent: Fable.Entity) =
        assert (ent.IsInterface)

        FSharp2Fable.Util.getInterfaceMembers com ent
        |> Seq.filter (fun (ifc, memb) -> not memb.IsProperty)
        |> Seq.distinctBy (fun (ifc, memb) -> memb.CompiledName) // skip inherited overwrites

    let getInterfaceMemberNames (com: IRustCompiler) (entRef: Fable.EntityRef) =
        let ent = com.GetEntity(entRef)

        getDistinctInterfaceMembers com ent
        |> Seq.map (fun (ifc, memb) -> memb.FullName)
        |> Set.ofSeq

    let makeObjectExprMemberDecl (com: IRustCompiler) ctx (memb: Fable.ObjectExprMember) =
        let capturedIdents = getCapturedIdents com ctx (Some memb.Name) memb.Args memb.Body

        let thisArg = Fable.Value(Fable.ThisValue Fable.Any, None)

        let replacements =
            capturedIdents
            |> Map.map (fun _name (ident: Fable.Ident) ->
                let info = Fable.FieldInfo.Create(ident.Name, ident.Type, ident.IsMutable)
                Fable.Get(thisArg, info, ident.Type, None)
            )

        // replace captured idents with 'this.ident' in member body
        let body = FableTransforms.replaceValues replacements memb.Body

        let memberDecl: Fable.MemberDecl =
            {
                Name = memb.Name
                Args = memb.Args
                Body = body
                MemberRef = memb.MemberRef
                IsMangled = memb.IsMangled
                ImplementedSignatureRef = None
                UsedNames = Set.empty
                XmlDoc = None
                Tags = []
            }

        memberDecl, capturedIdents

    let makeEntRef fullName assemblyName : Fable.EntityRef =
        {
            FullName = fullName
            Path = Fable.CoreAssemblyName assemblyName
        }

    let transformObjectExpr
        (com: IRustCompiler)
        ctx
        typ
        (members: Fable.ObjectExprMember list)
        (baseCall: Fable.Expr option)
        : Rust.Expr
        =
        if members |> List.isEmpty then
            mkUnitExpr () // object constructors sometimes generate this
        else
            // TODO: support non-interface types with constructors
            // TODO: add captured idents to non-interface types

            let entRef, genArgs =
                match typ with
                | Fable.DeclaredType(entRef, genArgs) -> entRef, genArgs
                | Fable.Any -> makeEntRef "System.Object" "System.Runtime", []
                | _ ->
                    "Unsupported object expression" |> addWarning com [] None
                    makeEntRef "System.Object" "System.Runtime", []

            let ent = com.GetEntity(entRef)
            let entName = "ObjectExpr"

            // collect all captured idents as fields
            let mutable fieldsMap = Map.empty

            let members: Fable.MemberDecl list =
                members
                |> List.map (fun memb ->
                    let memberDecl, capturedIdents = makeObjectExprMemberDecl com ctx memb
                    fieldsMap <- Map.fold (fun acc k v -> Map.add k v acc) fieldsMap capturedIdents
                    memberDecl
                )

            let decl: Fable.ClassDecl =
                {
                    Name = entName
                    Entity = entRef
                    Constructor = None
                    BaseCall = baseCall
                    AttachedMembers = members
                    XmlDoc = None
                    Tags = []
                }

            let fieldIdents =
                fieldsMap.Values
                |> Seq.map (fun ident -> { ident with Type = FableTransforms.uncurryType ident.Type })
                |> Seq.toList

            let exprFields =
                fieldIdents
                |> List.map (fun ident ->
                    let expr = transformIdent com ctx None ident |> makeClone
                    let fieldName = ident.Name |> sanitizeMember
                    mkExprField [] fieldName expr false false
                )

            let fieldIdents =
                if isValidBaseType com entRef then
                    let typ = Fable.DeclaredType(entRef, genArgs)
                    let baseIdent = makeTypedIdent typ baseName
                    baseIdent :: fieldIdents
                else
                    fieldIdents

            let fields =
                fieldIdents
                |> List.map (fun ident ->
                    let fieldTy = transformIdentType com ctx true ident
                    let fieldName = ident.Name |> sanitizeMember
                    mkField [] fieldName fieldTy false
                )

            let exprFields =
                match baseCall with
                | Some ctorExpr when isValidBaseType com entRef ->
                    let baseExpr = transformExpr com ctx ctorExpr
                    let fieldName = baseName |> sanitizeMember
                    let exprField = mkExprField [] fieldName baseExpr false false
                    exprField :: exprFields
                | _ -> exprFields

            let attrs = [ mkAttr "derive" (makeDerivedFrom com ent) ]
            let generics = makeGenerics com ctx genArgs
            let genParams = FSharp2Fable.Util.getGenParamTypes genArgs

            let structItems = [ mkStructItem attrs entName fields generics ]

            let memberItems = transformClassMembers com ctx genArgs decl

            let objExpr =
                // match baseCall with
                // | Some fableExpr -> com.TransformExpr(ctx, fableExpr) //TODO:
                // | None ->
                let genArgsOpt = transformGenArgs com ctx genParams
                let path = makeFullNamePath entName genArgsOpt
                let expr = mkStructExpr path exprFields |> makeLrcPtrValue com ctx

                if ent.IsInterface then
                    makeInterfaceCast com ctx typ expr
                else
                    expr

            let declStmts = structItems @ memberItems |> List.map mkItemStmt
            let objStmts = [ objExpr |> mkExprStmt ]
            declStmts @ objStmts |> mkBlock |> mkBlockExpr

    let maybeAddParens fableExpr (expr: Rust.Expr) : Rust.Expr =
        match fableExpr with
        | Fable.Value _ -> expr
        | _ -> mkParenExpr expr

    let transformOperation com ctx range typ opKind : Rust.Expr =
        match opKind with
        | Fable.Unary(UnaryOperator.UnaryAddressOf, MaybeCasted(Fable.IdentExpr ident)) ->
            transformIdent com ctx range ident |> mkAddrOfExpr
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            match op with
            | UnaryOperator.UnaryMinus ->
                match typ with
                | Fable.Number((Int8 | Int16 | Int32 | Int64 | Int128 | NativeInt), _) ->
                    // use wrapping negate to properly handle MinValue
                    mkMethodCallExpr "wrapping_neg" None expr []
                | _ -> mkNegExpr expr
            | UnaryOperator.UnaryPlus -> expr // no unary plus
            | UnaryOperator.UnaryNot -> mkNotExpr expr // ?loc=range)
            | UnaryOperator.UnaryNotBitwise -> mkNotExpr expr // ?loc=range)
            | UnaryOperator.UnaryAddressOf -> expr |> mkAddrOfExpr

        | Fable.Binary(op, leftExpr, rightExpr) ->
            let kind =
                match op with
                | BinaryOperator.BinaryEqual -> Rust.BinOpKind.Eq
                | BinaryOperator.BinaryUnequal -> Rust.BinOpKind.Ne
                | BinaryOperator.BinaryLess -> Rust.BinOpKind.Lt
                | BinaryOperator.BinaryLessOrEqual -> Rust.BinOpKind.Le
                | BinaryOperator.BinaryGreater -> Rust.BinOpKind.Gt
                | BinaryOperator.BinaryGreaterOrEqual -> Rust.BinOpKind.Ge
                | BinaryOperator.BinaryShiftLeft -> Rust.BinOpKind.Shl
                | BinaryOperator.BinaryShiftRightSignPropagating -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryShiftRightZeroFill -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryMinus -> Rust.BinOpKind.Sub
                | BinaryOperator.BinaryPlus -> Rust.BinOpKind.Add
                | BinaryOperator.BinaryMultiply -> Rust.BinOpKind.Mul
                | BinaryOperator.BinaryDivide -> Rust.BinOpKind.Div
                | BinaryOperator.BinaryModulus -> Rust.BinOpKind.Rem
                | BinaryOperator.BinaryExponent -> failwithf "BinaryExponent not supported. TODO: implement with pow."
                | BinaryOperator.BinaryOrBitwise -> Rust.BinOpKind.BitOr
                | BinaryOperator.BinaryXorBitwise -> Rust.BinOpKind.BitXor
                | BinaryOperator.BinaryAndBitwise -> Rust.BinOpKind.BitAnd

            match op, leftExpr, rightExpr with
            | BinaryEqual, e, Fable.Value(Fable.Null _, _)
            | BinaryEqual, Fable.Value(Fable.Null _, _), e ->
                transformLeaveContext com ctx None e |> makeNullCheck com ctx
            | BinaryUnequal, e, Fable.Value(Fable.Null _, _)
            | BinaryUnequal, Fable.Value(Fable.Null _, _), e ->
                transformLeaveContext com ctx None e |> makeNullCheck com ctx |> mkNotExpr
            | _ ->
                let left = transformLeaveContext com ctx None leftExpr |> maybeAddParens leftExpr
                let right = transformLeaveContext com ctx None rightExpr |> maybeAddParens rightExpr

                match leftExpr.Type, kind with
                | Fable.String, Rust.BinOpKind.Add -> makeLibCall com ctx None "String" "append" [ left; right ]
                | _ -> mkBinaryExpr (mkBinOp kind) left right // ?loc=range)

        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            let kind =
                match op with
                | LogicalOperator.LogicalOr -> Rust.BinOpKind.Or
                | LogicalOperator.LogicalAnd -> Rust.BinOpKind.And

            mkBinaryExpr (mkBinOp kind) left right // ?loc=range)

    let transformEmit (com: IRustCompiler) ctx range (emitInfo: Fable.EmitInfo) =
        // for now only supports macro calls or function calls
        let info = emitInfo.CallInfo
        let macro = emitInfo.Macro

        if macro.EndsWith("!", StringComparison.Ordinal) then
            // if it ends with '!', it must be a Rust macro
            let macro = macro |> Fable.Naming.replaceSuffix "!" ""
            let args = transformCallArgs com ctx info.Args info.SignatureArgTypes []
            mkMacroExpr macro args
        else
            // otherwise it's an Emit
            let thisArgOpt = info.ThisArg |> Option.map (fun e -> com.TransformExpr(ctx, e))
            let unboxedArgs = info.Args |> FSharp2Fable.Util.unboxBoxedArgs
            let args = transformCallArgs com ctx unboxedArgs info.SignatureArgTypes []
            let args = args |> List.append (thisArgOpt |> Option.toList)
            //TODO: create custom macro emit! (instead of a custom AST expression)
            mkEmitExpr macro args

    let transformCallee (com: IRustCompiler) ctx calleeExpr =
        match calleeExpr with
        | Fable.IdentExpr ident -> transformIdent com ctx None ident
        | Fable.Value(Fable.ThisValue _, _) -> transformExpr com ctx calleeExpr
        | _ ->
            let expr = transformExpr com ctx calleeExpr
            expr |> mkParenExpr // if not an identifier, wrap it in parentheses

    let transformCall (com: IRustCompiler) ctx range (typ: Fable.Type) calleeExpr (callInfo: Fable.CallInfo) =
        let membOpt = callInfo.MemberRef |> Option.bind com.TryGetMember

        let isByRefPreferred =
            membOpt
            |> Option.map (fun memb -> memb.Attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef))
            |> Option.defaultValue false

        let ctx =
            let isSendSync =
                match calleeExpr with
                | Fable.Import(info, t, r) ->
                    match info.Selector with
                    | "AsyncBuilder_::delay"
                    | "AsyncBuilder_::bind"
                    | "Task_::bind"
                    | "Task_::delay"
                    | "TaskBuilder_::bind"
                    | "TaskBuilder_::delay" -> true
                    | _ -> false
                | _ -> false

            { ctx with
                RequiresSendSync = isSendSync
                IsParamByRefPreferred = isByRefPreferred
            }

        let parameters =
            membOpt
            |> Option.map (fun memb -> memb.CurriedParameterGroups |> List.concat)
            |> Option.defaultValue []

        let callArgs =
            FSharp2Fable.Util.dropUnitCallArg com callInfo.Args callInfo.SignatureArgTypes callInfo.MemberRef

        let args = transformCallArgs com ctx callArgs callInfo.SignatureArgTypes parameters

        let genArgsOpt =
            match membOpt with
            | Some memb ->
                if List.isEmpty callArgs && not (List.isEmpty memb.GenericParameters) then
                    transformGenArgs com ctx callInfo.GenericArgs
                else
                    None
            | _ -> None

        match calleeExpr with
        // mutable module values (transformed as function calls)
        | Fable.IdentExpr ident when ident.IsMutable && isModuleMemberCall com callInfo ->
            let expr = transformIdent com ctx range ident
            mkCallExpr expr [] |> mutableGet

        | Fable.Get(calleeExpr, (Fable.FieldGet info as kind), t, _r) ->
            // this is an instance call
            match t with
            | Fable.LambdaType _
            | Fable.DelegateType _ ->
                // if the field type is a function, wrap in parentheses
                let callee = transformGet com ctx None t calleeExpr kind
                mkCallExpr (callee |> mkParenExpr) args
            | _ -> makeInstanceCall com ctx info.Name calleeExpr args

        | Fable.Import(info, t, r) ->
            // library imports without args need explicit genArgs
            // this is for imports like Array.empty, Seq.empty etc.
            let needGenArgs =
                List.isEmpty callArgs || info.Selector.EndsWith("::new_with_capacity")

            match callInfo.ThisArg, info.Kind with
            | Some thisArg, Fable.MemberImport memberRef ->
                let memb = com.GetMember(memberRef)

                if memb.IsInstance && memb.IsExtension then
                    // extension method calls compiled as static method calls
                    let thisExpr = transformLeaveContext com ctx None thisArg
                    let callee = transformImport com ctx r t info genArgsOpt
                    mkCallExpr callee (thisExpr :: args)
                elif memb.IsInstance then
                    let callee = transformCallee com ctx thisArg
                    mkMethodCallExpr info.Selector None callee args
                else
                    let callee = transformImport com ctx r t info genArgsOpt
                    mkCallExpr callee args
            | None, Fable.MemberImport memberRef when isModuleMemberRef com memberRef ->
                let memb = com.GetMember(memberRef)
                let callee = transformImport com ctx r t info genArgsOpt

                if memb.IsMutable && memb.IsValue then
                    mkCallExpr callee [] |> mutableGet
                else
                    mkCallExpr callee args
            | None, Fable.LibraryImport mi ->
                let genArgsOpt =
                    if needGenArgs && (mi.IsModuleMember || not mi.IsInstanceMember) then
                        match typ with
                        | Fable.Tuple _ -> transformGenArgs com ctx [ typ ]
                        | _ -> transformGenArgs com ctx typ.Generics
                    else
                        None

                let callee = transformImport com ctx r t info genArgsOpt
                mkCallExpr callee args
            | _ ->
                let callee = transformImport com ctx r t info None
                mkCallExpr callee args

        | _ ->
            match ctx.TailCallOpportunity with
            | Some tc when tc.IsRecursiveRef(calleeExpr) && List.length tc.Args = List.length callInfo.Args ->
                optimizeTailCall com ctx range tc callInfo.Args
            | _ ->
                match callInfo.ThisArg, calleeExpr with
                | Some thisArg, Fable.IdentExpr ident ->
                    match membOpt with
                    | Some memb when memb.IsExtension ->
                        // transform extension calls as static calls
                        let thisExpr = transformLeaveContext com ctx None thisArg
                        let callee = makeFullNamePathExpr ident.Name genArgsOpt
                        mkCallExpr callee (thisExpr :: args)
                    | _ ->
                        // other instance calls
                        let callee = transformCallee com ctx thisArg
                        mkMethodCallExpr ident.Name None callee args
                | None, Fable.IdentExpr ident ->
                    let callee = makeFullNamePathExpr ident.Name genArgsOpt
                    mkCallExpr callee args
                | _ ->
                    let callee = transformCallee com ctx calleeExpr
                    mkCallExpr callee args

    let mutableGet expr =
        mkMethodCallExpr "get" None expr [] |> makeClone

    let mutableGetMut expr = mkMethodCallExpr "get_mut" None expr []

    let mutableSet expr value =
        mkMethodCallExpr "set" None expr [ value ]

    let makeInstanceCall com ctx memberName calleeExpr args =
        let membName = Fable.Naming.splitLast memberName
        let callee = com.TransformExpr(ctx, calleeExpr)
        // match calleeExpr.Type with
        // | IsNonErasedInterface com (entRef, genArgs) ->
        //     // interface instance call (using fully qualified syntax)
        //     let ifcName = getEntityFullName com ctx entRef
        //     let parts = (ifcName + "::" + membName) |> splitNameParts
        //     (callee |> makeAsRef) :: args |> makeCall parts None
        // | _ ->
        //     // normal instance call
        //     mkMethodCallExpr membName None callee args
        mkMethodCallExpr membName None callee args

    let transformGet (com: IRustCompiler) ctx range typ (fableExpr: Fable.Expr) kind =
        match kind with
        | Fable.ExprGet idx ->
            let expr = transformCallee com ctx fableExpr
            let prop = transformExpr com ctx idx

            match fableExpr.Type, idx.Type with
            | Fable.Array(t, _), Fable.Number(Int32, Fable.NumberInfo.Empty) ->
                // // when indexing an array, cast index to usize
                // let expr = expr |> mutableGetMut
                // let prop = prop |> mkCastExpr (primitiveType "usize")
                getExpr range expr prop |> makeClone
            | _ -> getExpr range expr prop

        | Fable.FieldGet info ->
            match fableExpr.Type with
            | Fable.AnonymousRecordType(fields, _genArgs, isStruct) ->
                // anonymous records are tuples, transpile as tuple get
                let idx = fields |> Array.findIndex (fun f -> f = info.Name)
                (Fable.TupleIndex(idx)) |> transformGet com ctx range typ fableExpr
            // | t when isInterface com t ->
            //     // for interfaces, transpile get_property as instance call
            //     makeInstanceCall com ctx info.Name fableExpr []
            // | Fable.GenericParam(_name, _isMeasure, _constraints) ->
            //     // for generic types, transpile get_property as instance call
            //     makeInstanceCall com ctx info.Name fableExpr []
            | _ ->
                // for everything else, transpile as field get
                let expr = transformCallee com ctx fableExpr
                let field = getField range expr info.Name

                if info.IsMutable then
                    field |> mutableGet
                else
                    field

        | Fable.ListHead ->
            // get range (com.TransformExpr(ctx, fableExpr)) "head"
            libCall com ctx range [] "List" "head" [ fableExpr ]

        | Fable.ListTail ->
            // get range (com.TransformExpr(ctx, fableExpr)) "tail"
            libCall com ctx range [] "List" "tail" [ fableExpr ]

        | Fable.TupleIndex index ->
            let expr = transformCallee com ctx fableExpr
            mkFieldExpr expr (index.ToString()) |> makeClone

        | Fable.OptionValue ->
            match fableExpr with
            | Fable.IdentExpr ident when isArmScoped ctx ident.Name ->
                // if arm scoped, just output the ident value
                let name = $"{ident.Name}_{0}_{0}"
                mkGenericPathExpr [ name ] None
            | _ ->
                // libCall com ctx range [] "Option" "getValue" [ fableExpr ]
                let expr = com.TransformExpr(ctx, fableExpr)
                expr |> makeClone |> makeUnwrap

        | Fable.UnionTag ->
            let expr = com.TransformExpr(ctx, fableExpr)
            // TODO: range
            expr

        | Fable.UnionField info ->
            match fableExpr with
            | Fable.IdentExpr ident when isArmScoped ctx ident.Name ->
                // if ident is match arm scoped, just output the ident value
                let name = $"{ident.Name}_{info.CaseIndex}_{info.FieldIndex}"
                mkGenericPathExpr [ name ] None
            | _ ->
                // Compile as: "match opt { MyUnion::Case(x, _) => x.clone() }"
                let ent = com.GetEntity(info.Entity)
                assert (ent.IsFSharpUnion)
                // let genArgsOpt = transformGenArgs com ctx genArgs // TODO:
                let unionCase = ent.UnionCases |> List.item info.CaseIndex
                let fieldName = "x"

                let fields =
                    unionCase.UnionCaseFields
                    |> List.mapi (fun i _field ->
                        if i = info.FieldIndex then
                            makeFullNameIdentPat fieldName
                        else
                            WILD_PAT
                    )

                let unionCaseName = getUnionCaseName com ctx info.Entity unionCase
                let pat = makeUnionCasePat unionCaseName fields
                let expr = makeRefForPatternMatch com ctx fableExpr.Type None fableExpr
                let thenExpr = mkGenericPathExpr [ fieldName ] None |> makeClone

                let arms = [ mkArm [] pat None thenExpr ]

                let arms =
                    if (List.length ent.UnionCases) > 1 then
                        // only add a default arm if needed
                        let defaultArm = mkArm [] WILD_PAT None (mkMacroExpr "unreachable" [])
                        arms @ [ defaultArm ]
                    else
                        arms

                mkMatchExpr expr arms

    let transformSet (com: IRustCompiler) ctx range fableExpr typ (fableValue: Fable.Expr) kind =
        let expr = transformCallee com ctx fableExpr
        let value = transformLeaveContext com ctx None fableValue

        match kind with
        | Fable.ValueSet ->
            match fableExpr with
            // mutable values
            | Fable.IdentExpr ident when ident.IsMutable -> transformIdentSet com ctx range ident value
            // mutable module values (transformed as function calls)
            | Fable.Call(Fable.IdentExpr ident, info, _, _) when ident.IsMutable && isModuleMemberCall com info ->
                let expr = transformIdent com ctx range ident
                mutableSet (mkCallExpr expr []) value
            // mutable idents captured in object expression methods
            | Fable.Get(Fable.Value(Fable.ThisValue _, _) as thisArg, Fable.GetKind.FieldGet info, _t, _r) when
                info.IsMutable
                ->
                let expr = transformCallee com ctx thisArg
                let field = getField None expr info.Name
                mutableSet field value
            | _ ->
                match fableExpr.Type with
                | Replacements.Util.Builtin(Replacements.Util.FSharpReference _) -> mutableSet expr value
                | _ -> mkAssignExpr expr value
        | Fable.ExprSet idx ->
            let prop = transformExpr com ctx idx

            match fableExpr.Type, idx.Type with
            | Fable.Array(t, _), Fable.Number(Int32, Fable.NumberInfo.Empty) ->
                // when indexing an array, cast index to usize
                let expr = expr |> mutableGetMut
                let prop = prop |> mkCastExpr (primitiveType "usize")
                let left = getExpr range expr prop
                mkAssignExpr left value
            | _ ->
                let left = getExpr range expr prop
                mkAssignExpr left value // ?loc=range)
        | Fable.FieldSet(fieldName) ->
            match fableExpr.Type with
            // | t when isInterface com t ->
            //     // for interfaces, transpile property_set as instance call
            //     makeInstanceCall com ctx fieldName fableExpr [ value ]
            | _ ->
                let field = getField None expr fieldName
                mutableSet field value

    let transformAsStmt (com: IRustCompiler) ctx (e: Fable.Expr) : Rust.Stmt =
        let expr = transformLeaveContext com ctx None e
        mkExprStmt expr

    // flatten nested Let binding expressions
    let rec flattenLet acc (expr: Fable.Expr) =
        match expr with
        | Fable.Let(ident, value, body) -> flattenLet ((ident, value) :: acc) body
        | _ -> List.rev acc, expr

    // flatten nested Sequential expressions (depth first)
    let rec flattenSequential (expr: Fable.Expr) =
        match expr with
        | Fable.Sequential exprs -> List.collect flattenSequential exprs
        | _ -> [ expr ]

    let hasFuncOrAnyType typ =
        match typ with
        | Fable.Any
        | Fable.LambdaType _
        | Fable.DelegateType _ -> true
        | t -> t.Generics |> List.exists hasFuncOrAnyType

    let getScopedIdentCtx com ctx (ident: Fable.Ident) isArm isRef isBox isFunc usages =
        let scopedVarAttrs =
            {
                IsArm = isArm
                IsRef = isRef
                IsBox = isBox
                IsFunc = isFunc
                UsageCount = usageCount ident.Name usages
            }

        let scopedSymbols = ctx.ScopedSymbols |> Map.add ident.Name scopedVarAttrs

        { ctx with ScopedSymbols = scopedSymbols }

    let makeLocalStmt com ctx (ident: Fable.Ident) isRef isMut tyOpt initOpt isByRef usages =
        let local = mkIdentLocal [] ident.Name isRef isMut tyOpt initOpt
        let ctx = getScopedIdentCtx com ctx ident false isByRef false false usages
        mkLocalStmt local, ctx

    let makeLetStmt com ctx (ident: Fable.Ident) value isCaptured usages =
        // TODO: traverse body and follow references to decide if this should be wrapped or not
        // For Box/Rc it's not needed cause the Rust compiler will optimize the allocation away
        let tyOpt =
            match value with
            | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf, Fable.IdentExpr ident2), _, _, _) when
                isByRefType com ident2.Type || ident2.IsMutable
                ->
                None
            | _ ->
                if isException com ident.Type || hasFuncOrAnyType ident.Type then
                    None
                else
                    let ctx = { ctx with InferAnyType = true }
                    transformIdentType com ctx isCaptured ident |> Some

        let initOpt =
            match value with
            | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf, Fable.IdentExpr ident2), _, _, _) when
                isByRefType com ident2.Type || ident2.IsMutable
                ->
                transformIdent com ctx None ident2 |> Some
            | Fable.Value(Fable.Null Fable.MetaType, _) -> None // special init value to skip initialization
            | Function(args, body, _name) -> transformLambda com ctx (Some ident.Name) args body |> Some
            | _ ->
                transformLeaveContext com ctx None value
                // |> BLOCK_COMMENT_SUFFIX (sprintf "usages - %i" (usageCount ident.Name usages))
                |> Some

        let initOpt =
            initOpt
            |> Option.map (fun init ->
                if isByRefType com ident.Type then
                    init // already wrapped
                elif ident.IsMutable && isCaptured then
                    init |> makeMutValue com ctx |> makeLrcPtrValue com ctx
                elif ident.IsMutable then
                    init |> makeMutValue com ctx
                else
                    init
            )

        let isByRef = isAddrOfExpr value
        makeLocalStmt com ctx ident false false tyOpt initOpt isByRef usages

    let makeLetStmts (com: IRustCompiler) ctx bindings letBody usages =
        // Context will be threaded through all let bindings, appending itself to ScopedSymbols each time
        let ctx, letStmtsRev =
            ((ctx, []), bindings)
            ||> List.fold (fun (ctx, lst) (ident: Fable.Ident, value) ->
                let stmt, ctxNext =
                    let isCaptured =
                        (bindings
                         |> List.exists (fun (_i, v) -> FableTransforms.isIdentCaptured ident.Name v))
                        || (FableTransforms.isIdentCaptured ident.Name letBody)

                    match value with
                    | Function(args, body, _name) when not (ident.IsMutable) ->
                        if hasCapturedIdents com ctx ident.Name args body then
                            makeLetStmt com ctx ident value isCaptured usages
                        else
                            transformNestedFunction com ctx ident args body usages
                    | _ -> makeLetStmt com ctx ident value isCaptured usages

                (ctxNext, stmt :: lst)
            )

        letStmtsRev |> List.rev, ctx

    let transformLet (com: IRustCompiler) ctx (bindings: (Fable.Ident * Fable.Expr) list) body =
        // let usages =
        //     let bodyUsages = calcIdentUsages body
        //     let bindingsUsages = bindings |> List.map (snd >> calcIdentUsages)
        //     (Map.empty, bodyUsages::bindingsUsages)
        //     ||> List.fold (Helpers.Map.mergeAndAggregate (+))
        let usages =
            let idents, values = List.unzip bindings
            let exprs = body :: values
            calcIdentUsages idents exprs

        let letStmts, ctx = makeLetStmts com ctx bindings body usages

        let bodyStmts =
            match body with
            | Fable.Sequential exprs ->
                let exprs = flattenSequential body
                List.map (transformAsStmt com ctx) exprs
            | _ -> [ transformAsStmt com ctx body ]

        letStmts @ bodyStmts |> mkStmtBlockExpr

    let transformSequential (com: IRustCompiler) ctx exprs =
        exprs |> List.map (transformAsStmt com ctx) |> mkStmtBlockExpr

    let transformIfThenElse (com: IRustCompiler) ctx range guard thenBody elseBody =
        // transform null checks for nullable value types
        match guard with
        | Fable.Test(Fable.IdentExpr ident, Fable.OptionTest false, r) when isNullableValueType com ident.Type ->
            let value = transformIdentGet com ctx r ident
            let pat = makeUnionCasePat (rawIdent "Some") [ makeFullNameIdentPat ident.Name ]
            let letExpr = mkLetExpr pat value
            let thenExpr = transformLeaveContext com ctx None thenBody
            let elseExpr = transformLeaveContext com ctx None elseBody

            match thenBody with
            | Fable.Value(Fable.UnitConstant, _) -> mkIfThenExpr letExpr elseExpr // ?loc=range)
            | _ -> mkIfThenElseExpr letExpr elseExpr thenExpr // ?loc=range)
        | _ ->
            // match canTransformDecisionTreeAsSwitch guard thenBody elseBody with
            // | Some(evalExpr, cases, defaultCase) ->
            //     transformSwitch com ctx evalExpr cases (Some defaultCase)
            // | _ ->
            let guardExpr =
                match guard with
                | Fable.Test(expr, Fable.TypeTest typ, r) -> transformTypeTest com ctx r true typ expr
                | Fable.Test(expr, Fable.OptionTest isSome, r) -> makeOptionTest com ctx r isSome expr
                | Fable.Test(expr, Fable.UnionCaseTest tag, r) -> makeUnionCaseTest com ctx r tag expr
                | _ -> transformLeaveContext com ctx None guard

            let thenExpr =
                let ctx =
                    match guard with
                    // | Fable.Test(Fable.IdentExpr ident, Fable.OptionTest _, _)
                    | Fable.Test(Fable.IdentExpr ident, Fable.UnionCaseTest _, _) ->
                        // add scoped ident to ctx for thenBody
                        let usages = calcIdentUsages [ ident ] [ thenBody ]
                        getScopedIdentCtx com ctx ident true false false false usages
                    | _ -> ctx

                transformLeaveContext com ctx None thenBody

            match elseBody with
            | Fable.Value(Fable.UnitConstant, _) -> mkIfThenExpr guardExpr thenExpr // ?loc=range)
            | _ ->
                let elseExpr = transformLeaveContext com ctx None elseBody
                mkIfThenElseExpr guardExpr thenExpr elseExpr // ?loc=range)

    let transformWhileLoop (com: IRustCompiler) ctx range guard body =
        let guardExpr = transformExpr com ctx guard
        let bodyExpr = com.TransformExpr(ctx, body)
        mkWhileExpr None guardExpr bodyExpr // ?loc=range)

    let transformForLoop (com: IRustCompiler) ctx range isUp (var: Fable.Ident) start limit body =
        let startExpr = transformExpr com ctx start
        let limitExpr = transformExpr com ctx limit
        // let ctx = { ctx with HasMultipleUses = true }
        let bodyExpr = com.TransformExpr(ctx, body)
        let varPat = makeFullNameIdentPat var.Name

        let rangeExpr =
            if isUp then
                mkRangeExpr (Some startExpr) (Some limitExpr) true
            else
                // downward loop
                let rangeExpr = mkRangeExpr (Some limitExpr) (Some startExpr) true |> mkParenExpr
                mkMethodCallExpr "rev" None rangeExpr []

        mkForLoopExpr None varPat rangeExpr bodyExpr // ?loc=range)

    let makeLocalLambda com ctx (args: Fable.Ident list) (body: Fable.Expr) =
        let args = args |> discardUnitArg []
        let fnDecl = transformFunctionDecl com ctx args [] Fable.Unit
        let fnBody = transformExpr com ctx body
        mkClosureExpr false fnDecl fnBody

    let transformTryCatch (com: IRustCompiler) ctx range body catch finalizer : Rust.Expr =
        // try...with
        match catch with
        | Some(catchVar, catchBody) ->
            // try...with statements cannot be tail call optimized
            let ctx = { ctx with TailCallOpportunity = None }
            let try_f = makeLocalLambda com ctx [] body
            let catch_f = makeLocalLambda com ctx [ catchVar ] catchBody

            makeLibCall com ctx None "Exception" "try_catch" [ try_f; catch_f ]

        | None ->
            // try...finally
            match finalizer with
            | Some finBody ->
                let f = transformLambda com ctx None [] finBody
                let finCall = makeLibCall com ctx None "Exception" "finally" [ f ]
                let finPat = makeFullNameIdentPat "__finally__"
                let letExpr = mkLetExpr finPat finCall
                let bodyExpr = transformExpr com ctx body
                [ letExpr |> mkSemiStmt; bodyExpr |> mkExprStmt ] |> mkStmtBlockExpr
            | _ ->
                // no catch, no finalizer
                transformExpr com ctx body

    let transformThrow (com: IRustCompiler) (ctx: Context) typ (exprOpt: Fable.Expr option) : Rust.Expr =
        match exprOpt with
        | None ->
            // should not happen, reraise is handled in Replacements
            mkMacroExpr "panic" [ mkStrLitExpr "rethrow" ]
        | Some expr ->
            let err = transformExpr com ctx expr

            let msg =
                match expr.Type with
                | Fable.String -> err
                | _ -> mkMethodCallExpr "get_Message" None err []

            mkMacroExpr "panic" [ mkStrLitExpr "{}"; msg ]

    let transformCurry (com: IRustCompiler) (ctx: Context) arity (expr: Fable.Expr) : Rust.Expr =
        com.TransformExpr(ctx, Replacements.Api.curryExprAtRuntime com arity expr)

    let transformCurriedApply (com: IRustCompiler) ctx r typ calleeExpr args =
        match ctx.TailCallOpportunity with
        | Some tc when tc.IsRecursiveRef(calleeExpr) && List.length tc.Args = List.length args ->
            optimizeTailCall com ctx r tc args
        | _ ->
            let callee = transformCallee com ctx calleeExpr

            (callee, args)
            ||> List.fold (fun expr arg ->
                let args = FSharp2Fable.Util.dropUnitCallArg com [ arg ] [] None
                callFunction com ctx r expr args
            )

    let makeUnionCasePat unionCaseName fields =
        if List.isEmpty fields then
            makeFullNameIdentPat unionCaseName
        else
            let path = makeFullNamePath unionCaseName None
            mkTupleStructPat path fields

    let transformTypeTest (com: IRustCompiler) ctx range isDowncast typ (fableExpr: Fable.Expr) : Rust.Expr =
        // try_downcast or type_test
        let expr = transformCallee com ctx fableExpr |> mkAddrOfExpr

        let genArgsOpt =
            (mkInferTy ()) :: (transformGenTypes com ctx [ typ ]) |> mkTypesGenericArgs

        match fableExpr with
        | Fable.IdentExpr ident when isDowncast ->
            let downcastExpr = makeLibCall com ctx genArgsOpt "Native" "try_downcast" [ expr ]
            let pat = makeUnionCasePat (rawIdent "Some") [ makeFullNameIdentPat ident.Name ]
            mkLetExpr pat downcastExpr
        | _ -> makeLibCall com ctx genArgsOpt "Native" "type_test" [ expr ]

    let makeUnionCasePatOpt (com: IRustCompiler) ctx typ nameOpt caseIndex =
        match typ with
        | Fable.Option(genArg, _) ->
            // let genArgsOpt = transformGenArgs com ctx [genArg]
            let unionCaseFullName = [ "Some"; "None" ] |> List.item caseIndex |> rawIdent

            let fields =
                match caseIndex with
                | 0 ->
                    match nameOpt with
                    | Some identName ->
                        let fieldName = $"{identName}_{caseIndex}_{0}"
                        [ makeFullNameIdentPat fieldName ]
                    | _ -> [ WILD_PAT ]
                | _ -> []

            let unionCaseName =
                tryUseKnownUnionCaseNames unionCaseFullName
                |> Option.defaultValue unionCaseFullName

            let pat = makeUnionCasePat unionCaseName fields
            Some(pat)
        | Fable.DeclaredType(entRef, genArgs) ->
            let ent = com.GetEntity(entRef)

            if ent.IsFSharpUnion then
                // let genArgsOpt = transformGenArgs com ctx genArgs // TODO:
                let unionCase = ent.UnionCases |> List.item caseIndex

                let fields =
                    match nameOpt with
                    | Some identName ->
                        unionCase.UnionCaseFields
                        |> List.mapi (fun i _field ->
                            let fieldName = $"{identName}_{caseIndex}_{i}"
                            makeFullNameIdentPat fieldName
                        )
                    | _ -> unionCase.UnionCaseFields |> List.map (fun _field -> WILD_PAT)

                let unionCaseName = getUnionCaseName com ctx entRef unionCase
                let pat = makeUnionCasePat unionCaseName fields
                Some(pat)
            else
                None
        | _ -> None

    let makeUnionCaseTest (com: IRustCompiler) ctx range tag (fableExpr: Fable.Expr) =
        let typ = fableExpr.Type
        let nameOpt = tryGetIdentName fableExpr
        let patOpt = makeUnionCasePatOpt com ctx typ nameOpt tag
        let pat = patOpt |> Option.defaultValue WILD_PAT
        let expr = makeRefForPatternMatch com ctx typ nameOpt fableExpr
        let letExpr = mkLetExpr pat expr
        letExpr

    let makeTest isSome thenValue elseValue =
        if isSome then
            thenValue
        else
            elseValue

    let makeOptionTest com ctx range isSome (fableExpr: Fable.Expr) =
        let test = makeTest isSome "is_some" "is_none"
        let expr = com.TransformExpr(ctx, fableExpr)
        mkMethodCallExpr test None expr []

    let transformTest com ctx range kind (fableExpr: Fable.Expr) : Rust.Expr =
        match kind with
        | Fable.TypeTest typ -> transformTypeTest com ctx range false typ fableExpr
        | Fable.OptionTest isSome -> makeOptionTest com ctx range isSome fableExpr
        | Fable.ListTest nonEmpty ->
            let expr = libCall com ctx range [] "List" "isEmpty" [ fableExpr ]

            if nonEmpty then
                mkNotExpr expr
            else
                expr //, ?loc=range
        | Fable.UnionCaseTest tag ->
            let guardExpr = makeUnionCaseTest com ctx range tag fableExpr
            let thenExpr = mkBoolLitExpr true
            let elseExpr = mkBoolLitExpr false
            mkIfThenElseExpr guardExpr thenExpr elseExpr

    let transformSwitch (com: IRustCompiler) ctx (evalExpr: Fable.Expr) cases defaultCase : Rust.Expr =
        let namesForIndex evalType evalName caseIndex = //todo refactor with below
            match evalType with
            | Fable.Option(genArg, _) ->
                match evalName with
                | Some idName ->
                    let fieldName = $"{idName}_{caseIndex}_{0}"
                    [ (fieldName, idName, genArg) ]
                | _ -> []
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)

                if ent.IsFSharpUnion then
                    let unionCase = ent.UnionCases |> List.item caseIndex

                    match evalName with
                    | Some idName ->
                        unionCase.UnionCaseFields
                        |> List.mapi (fun i field ->
                            let fieldName = $"{idName}_{caseIndex}_{i}"
                            let fieldType = FableTransforms.uncurryType field.FieldType
                            (fieldName, idName, fieldType)
                        )
                    | _ -> []
                else
                    []
            | _ -> []

        let makeArm pat targetIndex boundValues (extraVals: (string * string * Fable.Type) list) =
            let attrs = []
            let guard = None // TODO:
            let idents, (bodyExpr: Fable.Expr) = ctx.DecisionTargets |> List.item targetIndex // TODO:
            let vars = idents |> List.map (fun (ident: Fable.Ident) -> ident.Name)
            // TODO: vars, boundValues
            let body =
                //com.TransformExpr(ctx, bodyExpr)
                let extraIdents =
                    extraVals
                    |> List.map (fun (_name, friendlyName, typ) -> makeTypedIdent typ friendlyName)

                let allIdents = idents @ extraIdents
                let usages = calcIdentUsages allIdents [ bodyExpr ]

                let ctx =
                    (ctx, allIdents)
                    ||> List.fold (fun ctx ident -> getScopedIdentCtx com ctx ident true true false false usages)

                transformLeaveContext com ctx None bodyExpr

            mkArm attrs pat guard body

        let evalType, evalName =
            match evalExpr with
            | Fable.Get(Fable.IdentExpr ident, Fable.UnionTag, _, _) -> ident.Type, Some ident.Name
            | _ -> evalExpr.Type, None

        let arms =
            cases
            |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                let patOpt =
                    match caseExpr with
                    | Fable.Value(Fable.NumberConstant(Fable.NumberValue.Int32 tag, Fable.NumberInfo.Empty), r) ->
                        makeUnionCasePatOpt com ctx evalType evalName tag
                    | _ -> None

                let pat =
                    match patOpt with
                    | Some pat -> pat
                    | _ -> com.TransformExpr(ctx, caseExpr) |> mkLitPat

                let extraVals = namesForIndex evalType evalName targetIndex
                makeArm pat targetIndex (boundValues) extraVals
            )

        let defaultArms =
            match defaultCase with
            | Some(targetIndex, boundValues) ->
                // To see if the default arm should actually be a union case pattern, we have to
                // examine its body to see if it starts with union field get. // TODO: look deeper
                // If it does, we'll replace the wildcard "_" with a union case pattern
                let idents, bodyExpr = ctx.DecisionTargets |> List.item targetIndex

                let patOpt =
                    let rec getUnionPat expr =
                        match expr with
                        | Fable.Get(Fable.IdentExpr ident, Fable.OptionValue, _, _) when
                            Some ident.Name = evalName && ident.Type = evalType
                            ->
                            makeUnionCasePatOpt com ctx evalType evalName 0
                        | Fable.Get(Fable.IdentExpr ident, Fable.UnionField info, _, _) when
                            Some ident.Name = evalName && ident.Type = evalType
                            ->
                            makeUnionCasePatOpt com ctx evalType evalName info.CaseIndex
                        | _ ->
                            //need to recurse or this only works for trivial expressions
                            let subExprs = getSubExpressions expr
                            subExprs |> List.tryPick getUnionPat

                    getUnionPat bodyExpr

                let pat = patOpt |> Option.defaultValue WILD_PAT
                let extraVals = namesForIndex evalType evalName targetIndex
                let arm = makeArm pat targetIndex boundValues extraVals
                [ arm ]
            | _ -> []

        let expr = makeRefForPatternMatch com ctx evalType evalName evalExpr
        mkMatchExpr expr (arms @ defaultArms)

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then
            []
        elif List.length idents = List.length values then
            List.zip idents values
        else
            failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBoundValues (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues

        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects com expr then
                        (ident, expr) :: bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements
                )

            let target = FableTransforms.replaceValues replacements target
            target, List.rev bindings
        else
            target, identsAndValues

    let transformDecisionTreeSuccess (com: IRustCompiler) (ctx: Context) targetIndex boundValues =
        match ctx.TargetAssignment with
        | Some targetId ->
            let idents, _target = getDecisionTarget ctx targetIndex
            let identsAndValues = matchTargetIdentAndValues idents boundValues

            let assignments =
                (targetId, makeIntConst targetIndex) :: identsAndValues
                |> List.map (fun (ident, value) ->
                    let ident = transformIdent com ctx None ident
                    let value = transformLeaveContext com ctx None value
                    mkAssignExpr ident value
                )

            assignments |> List.map mkSemiStmt |> mkStmtBlockExpr
        | None ->
            let target, bindings =
                getDecisionTargetAndBoundValues com ctx targetIndex boundValues

            match bindings with
            | [] -> transformLeaveContext com ctx None target
            | bindings ->
                let target =
                    List.rev bindings |> List.fold (fun e (i, v) -> Fable.Let(i, v, e)) target

                transformLeaveContext com ctx None target

    let canTransformDecisionTreeAsSwitch guardExpr thenExpr elseExpr =
        let (|Equals|_|) =
            function
            | Fable.Operation(Fable.Binary(BinaryEqual, left, right), _, _, _) ->
                match left, right with
                | _, Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _), _) ->
                    Some(left, right)
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _), _), _ ->
                    Some(right, left)
                | _ -> None
            | Fable.Test(expr, Fable.OptionTest isSome, r) ->
                let evalExpr =
                    Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, Fable.NumberInfo.Empty), r)

                let right = makeIntConst (makeTest isSome 0 1)
                Some(evalExpr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, r) ->
                let evalExpr =
                    Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, Fable.NumberInfo.Empty), r)

                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None

        let rec sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2 -> i1.Name = i2.Name
            | Fable.Get(e1, Fable.UnionTag, _, _), Fable.Get(e2, Fable.UnionTag, _, _)
            | Fable.Get(e1, Fable.ListHead, _, _), Fable.Get(e2, Fable.ListHead, _, _)
            | Fable.Get(e1, Fable.ListTail, _, _), Fable.Get(e2, Fable.ListTail, _, _)
            | Fable.Get(e1, Fable.OptionValue, _, _), Fable.Get(e2, Fable.OptionValue, _, _) -> sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.TupleIndex i1, _, _), Fable.Get(e2, Fable.TupleIndex i2, _, _) ->
                i1 = i2 && sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.FieldGet f1, _, _), Fable.Get(e2, Fable.FieldGet f2, _, _) ->
                f1.Name = f2.Name && sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.UnionField f1, _, _), Fable.Get(e2, Fable.UnionField f2, _, _) ->
                f1.CaseIndex = f2.CaseIndex
                && f1.FieldIndex = f2.FieldIndex
                && sameEvalExprs e1 e2
            | _ -> false

        let rec checkInner cases evalExpr treeExpr =
            match treeExpr with
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                               treeExpr,
                               _) when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                    let cases = (caseExpr, targetIndex, boundValues) :: cases
                    Some(evalExpr, List.rev cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr ->
                    let cases = (caseExpr, targetIndex, boundValues) :: cases
                    checkInner cases evalExpr treeExpr
            | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                Some(evalExpr, List.rev cases, (defaultTargetIndex, defaultBoundValues))
            | _ -> None

        match guardExpr, thenExpr with
        | Equals(evalExpr, caseExpr), Fable.DecisionTreeSuccess(targetIndex, boundValues, _) ->
            let cases = [ (caseExpr, targetIndex, boundValues) ]

            match checkInner cases evalExpr elseExpr with
            | Some(evalExpr, cases, defaultCase) ->
                // let cases = groupSwitchCases cases defaultCase
                Some(evalExpr, cases, defaultCase)
            | _ -> None
        | _ -> None

    let simplifyDecisionTree (treeExpr: Fable.Expr) =
        treeExpr
        |> visitFromInsideOut (
            function
            | Fable.IfThenElse(guardExpr1,
                               Fable.IfThenElse(guardExpr2, thenExpr, Fable.DecisionTreeSuccess(index2, [], _), _),
                               Fable.DecisionTreeSuccess(index1, [], t),
                               r) when index1 = index2 ->
                Fable.IfThenElse(
                    makeLogOp None guardExpr1 guardExpr2 LogicalAnd,
                    thenExpr,
                    Fable.DecisionTreeSuccess(index2, [], t),
                    r
                )
            | e -> e
        )

    // let groupSwitchCases t (cases: (Fable.Expr * int * Fable.Expr list) list) (defaultIndex, defaultBoundValues) =
    //     cases
    //     |> List.groupBy (fun (_, idx, boundValues) ->
    //         // Try to group cases with some target index and empty bound values
    //         // If bound values are non-empty use also a non-empty Guid to prevent grouping
    //         if List.isEmpty boundValues then
    //             idx, System.Guid.Empty
    //         else
    //             idx, System.Guid.NewGuid()
    //     )
    //     |> List.map (fun ((idx, _), cases) ->
    //         let caseExprs = cases |> List.map Tuple3.item1
    //         // If there are multiple cases, it means boundValues are empty
    //         // (see `groupBy` above), so it doesn't mind which one we take as reference
    //         let boundValues = cases |> List.head |> Tuple3.item3
    //         caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t)
    //     )
    //     |> function
    //         | [] -> []
    //         // Check if the last case can also be grouped with the default branch, see #2357
    //         | cases when List.isEmpty defaultBoundValues ->
    //             match List.splitLast cases with
    //             | cases, (_, Fable.DecisionTreeSuccess(idx, [], _)) when idx = defaultIndex -> cases
    //             | _ -> cases
    //         | cases -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int, int>) =
            function
            | [] -> targetRefs
            | expr :: exprs ->
                match expr with
                // We shouldn't actually see this, but short-circuit just in case
                | Fable.DecisionTree _ -> findSuccess targetRefs exprs
                | Fable.DecisionTreeSuccess(idx, _, _) ->
                    let count = Map.tryFind idx targetRefs |> Option.defaultValue 0

                    let targetRefs = Map.add idx (count + 1) targetRefs
                    findSuccess targetRefs exprs
                | expr ->
                    let exprs2 = getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)

        findSuccess Map.empty [ expr ]
        |> Seq.choose (fun kv ->
            if kv.Value > 1 then
                Some kv.Key
            else
                None
        )
        |> Seq.toList

    let tryTransformAsSwitch =
        function
        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, _) ->
            canTransformDecisionTreeAsSwitch guardExpr thenExpr elseExpr
        | _ -> None

    let transformDecisionTreeWithTwoSwitches
        (com: IRustCompiler)
        ctx
        (targets: (Fable.Ident list * Fable.Expr) list)
        treeExpr
        =
        // Declare target and bound idents
        let typ = Fable.Number(Int32, Fable.NumberInfo.Empty)

        let targetId =
            getUniqueNameInDeclarationScope ctx "matchResult" |> makeTypedIdent typ

        // Transform decision tree
        let ctx =
            { ctx with
                DecisionTargets = targets
                TargetAssignment = Some targetId
            }

        let varIdents = targetId :: (List.collect fst targets)

        // vars will be transformed as declarations only
        let varDecls, ctx =
            (ctx, varIdents)
            ||> List.mapFold (fun ctx ident ->
                let ty = transformType com ctx ident.Type
                let init = makeInit com ctx ident.Type
                makeLocalStmt com ctx ident false true (Some ty) (Some init) false Map.empty
            // makeLocalStmt com ctx ident false false (Some ty) None false Map.empty
            )

        // Transform targets as switch
        let switch2 =
            let cases =
                targets |> List.mapi (fun i (_, target) -> makeIntConst i, i, [ target ])

            let defaultIndex = List.length targets
            let defaultValue = "unreachable!" |> AST.emitExpr None Fable.Any []
            let defaultCase = defaultIndex, []
            let targets = targets @ [ ([], defaultValue) ]
            let ctx = { ctx with DecisionTargets = targets }
            transformSwitch com ctx (targetId |> Fable.IdentExpr) cases (Some defaultCase)

        match tryTransformAsSwitch treeExpr with
        | Some(evalExpr, cases, defaultCase) ->
            // let cases = groupSwitchCases typ cases defaultCase
            let switch1 = transformSwitch com ctx evalExpr cases (Some defaultCase)

            varDecls @ [ switch1 |> mkSemiStmt ] @ [ switch2 |> mkExprStmt ]
            |> mkStmtBlockExpr
        | None ->
            let decisionTree = com.TransformExpr(ctx, treeExpr)

            varDecls @ [ decisionTree |> mkSemiStmt ] @ [ switch2 |> mkExprStmt ]
            |> mkStmtBlockExpr

    let transformDecisionTree (com: IRustCompiler) ctx targets (treeExpr: Fable.Expr) : Rust.Expr =
        let treeExpr = simplifyDecisionTree treeExpr

        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        match getTargetsWithMultipleReferences treeExpr with
        | [] ->
            let ctx = { ctx with DecisionTargets = targets }

            match tryTransformAsSwitch treeExpr with
            | Some(evalExpr, cases, defaultCase) -> transformSwitch com ctx evalExpr cases (Some defaultCase)
            | None -> com.TransformExpr(ctx, treeExpr)

        | targetsWithMultiRefs ->
            // If the bound idents are not referenced in the target, remove them
            let targets =
                targets
                |> List.map (fun (idents, expr) ->
                    idents
                    |> List.exists (fun i -> isIdentUsed i.Name expr)
                    |> function
                        | true -> idents, expr
                        | false -> [], expr
                )

            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs
                |> List.exists (fun idx -> targets[idx] |> fst |> List.isEmpty |> not)

            if not hasAnyTargetWithMultiRefsBoundValues then
                match tryTransformAsSwitch treeExpr with
                | Some(evalExpr, cases, defaultCase) ->
                    // let t = treeExpr.Type
                    // let cases = groupSwitchCases t cases defaultCase
                    let ctx = { ctx with DecisionTargets = targets }
                    transformSwitch com ctx evalExpr cases (Some defaultCase)
                | None -> transformDecisionTreeWithTwoSwitches com ctx targets treeExpr
            else
                transformDecisionTreeWithTwoSwitches com ctx targets treeExpr

    let rec transformExpr (com: IRustCompiler) ctx (fableExpr: Fable.Expr) : Rust.Expr =
        match fableExpr with
        | Fable.Unresolved(e, t, r) ->
            $"Unexpected unresolved expression: %A{e}" |> addError com [] r
            mkUnitExpr ()
        | Fable.TypeCast(e, t) -> transformCast com ctx t e
        | Fable.Value(kind, r) -> transformValue com ctx r kind
        | Fable.IdentExpr ident -> transformIdentGet com ctx None ident
        | Fable.Import(info, t, r) -> transformImport com ctx r t info None
        | Fable.Test(expr, kind, range) -> transformTest com ctx range kind expr
        | Fable.Lambda(arg, body, name) -> transformLambda com ctx name [ arg ] body
        | Fable.Delegate(args, body, name, _) -> transformLambda com ctx name args body
        | Fable.ObjectExpr(members, typ, baseCall) -> transformObjectExpr com ctx typ members baseCall
        | Fable.Call(callee, info, typ, range) -> transformCall com ctx range typ callee info
        | Fable.CurriedApply(callee, args, typ, range) -> transformCurriedApply com ctx range typ callee args
        | Fable.Operation(kind, _, typ, range) -> transformOperation com ctx range typ kind
        | Fable.Get(expr, kind, typ, range) -> transformGet com ctx range typ expr kind
        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            transformIfThenElse com ctx r guardExpr thenExpr elseExpr
        | Fable.DecisionTree(expr, targets) -> transformDecisionTree com ctx targets expr
        | Fable.DecisionTreeSuccess(idx, boundValues, _) -> transformDecisionTreeSuccess com ctx idx boundValues
        | Fable.Set(expr, kind, typ, value, range) -> transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            // flatten nested let binding expressions
            let bindings, body = flattenLet [] fableExpr
            transformLet com ctx bindings body
        // if ctx.HoistVars [ident] then
        //     let assignment = transformBindingAsExpr com ctx ident value
        //     Expression.sequenceExpression([|assignment; com.TransformExpr(ctx, body)|])
        // else iife com ctx expr

        | Fable.LetRec(bindings, body) -> transformLet com ctx bindings body
        //     let idents = List.map fst bindings
        //     if ctx.HoistVars(idents) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformExpr(ctx, body)|])
        //     else iife com ctx expr

        | Fable.Sequential exprs ->
            // flatten nested sequential expressions
            let exprs = flattenSequential fableExpr
            transformSequential com ctx exprs

        | Fable.Emit(info, _t, range) -> transformEmit com ctx range info

        | Fable.WhileLoop(guard, body, range) -> transformWhileLoop com ctx range guard body

        | Fable.ForLoop(var, start, limit, body, isUp, range) ->
            transformForLoop com ctx range isUp var start limit body

        | Fable.TryCatch(body, catch, finalizer, range) -> transformTryCatch com ctx range body catch finalizer

        | Fable.Extended(kind, r) ->
            match kind with
            | Fable.Curry(expr, arity) -> transformCurry com ctx arity expr
            | Fable.Throw(exprOpt, typ) -> transformThrow com ctx typ exprOpt
            | Fable.Debugger ->
                // TODO:
                $"Unimplemented Extended expression: %A{kind}" |> addWarning com [] r

                mkUnitExpr ()

    let rec tryFindEntryPoint (com: IRustCompiler) decl : string list option =
        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members
            |> List.tryPick (tryFindEntryPoint com)
            |> Option.map (fun name -> decl.Name :: name)
        | Fable.MemberDeclaration decl ->
            let memb = com.GetMember(decl.MemberRef)

            memb.Attributes
            |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.entryPoint)
            |> Option.map (fun _ -> [ Fable.Naming.splitLast decl.Name ])
        | Fable.ActionDeclaration decl -> None
        | Fable.ClassDeclaration decl -> None

    let isLastFileInProject (com: IRustCompiler) =
        (Array.last com.SourceFiles) = com.CurrentFile

    let getModuleItems (com: IRustCompiler) ctx =
        if isLastFileInProject com then
            // add all other source files (except signatures) as module imports
            com.SourceFiles
            |> Array.filter (fun x -> not (x.EndsWith(".fsi", StringComparison.Ordinal)))
            |> Array.iter (fun filePath ->
                if filePath <> com.CurrentFile then
                    let relPath = Path.getRelativePath com.CurrentFile filePath
                    com.GetImportName(ctx, "*", relPath, None) |> ignore
            )
            // re-export modules at crate level
            let makeModItems modulePath =
                let relPath = Path.getRelativePath com.CurrentFile modulePath
                let modName = getImportModuleName com modulePath
                let attrs = [ mkEqAttr "path" relPath ]
                let modItem = mkUnloadedModItem attrs modName
                let useItem = mkGlobUseItem [] [ modName ]

                [ modItem; useItem |> mkPublicItem ]

            let modItems = com.GetAllModules() |> List.sort |> List.collect makeModItems

            modItems
        else
            []

    let getNamespaceItems (com: IRustCompiler) ctx =
        if isLastFileInProject com then
            // convert namespace trie to modules and glob use items
            let rec toItems mods trie : Rust.Item list =
                [
                    if Namespace.Trie.isLeaf trie then
                        let modNames = List.rev mods

                        for filePath in trie.Values do
                            let modName = getImportModuleName com filePath
                            let useItem = mkGlobUseItem [] ("crate" :: modName :: modNames)
                            yield useItem |> mkPublicItem
                    for KeyValue(key, trie) in trie.Children do
                        let items = toItems (key :: mods) trie
                        let modItem = mkModItem [] key items
                        yield modItem |> mkPublicItem
                ]
            // re-export globally merged namespaces at crate level
            let nsItems = com.GetAllNamespaces() |> Namespace.Trie.ofSeq |> toItems []

            nsItems
        else
            []

    let getEntryPointItems (com: IRustCompiler) ctx decls =
        let entryPoint = decls |> List.tryPick (tryFindEntryPoint com)

        match entryPoint with
        | Some path ->
            // add some imports for main function
            let asArr = getLibraryImportName com ctx "NativeArray" "array_from"
            let asStr = getLibraryImportName com ctx "String" "fromString"

            // main entrypoint
            let mainName = String.concat "::" path

            let strBody =
                [
                    $"let args = std::env::args().skip(1).map({asStr}).collect()"
                    $"{mainName}({asArr}(args))"
                ]

            let fnBody = strBody |> Seq.map mkEmitSemiStmt |> mkBlock |> Some

            let attrs = [ mkAttr "cfg" [ "not(feature = \"no_std\")" ] ]
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            let fnItem = mkFnItem attrs "main" fnKind
            [ fnItem |> mkPublicItem ]

        | None -> []

    let getEntityPhantomGenParams _com (ent: Fable.Entity) : (string * Fable.Type) list =
        let fieldGenParamSet =
            ent.FSharpFields
            |> List.map (fun field -> field.FieldType)
            |> FSharp2Fable.Util.getGenParamNames
            |> Set.ofList

        let phantomGenParams =
            FSharp2Fable.Util.getEntityGenParams ent
            |> List.filter (fun (name, _typ) -> not (Set.contains name fieldGenParamSet))

        phantomGenParams

    let getEntityFieldsAsIdents com (ent: Fable.Entity) : Fable.Ident list =
        let fieldIdents =
            ent.FSharpFields
            |> List.map (fun field ->
                let name = field.Name
                let typ = FableTransforms.uncurryType field.FieldType
                let isMutable = field.IsMutable
                let ident = makeTypedIdent typ name
                { ident with IsMutable = isMutable }
            )
        // add the base type as a field, if applicable
        match ent.BaseType with
        | Some t when isValidBaseType com t.Entity ->
            let typ = Fable.DeclaredType(t.Entity, t.GenericArgs)
            let baseIdent = makeTypedIdent typ baseName
            baseIdent :: fieldIdents
        | _ -> fieldIdents

    let makeTypedParam (com: IRustCompiler) ctx (ident: Fable.Ident) returnType =
        if ident.IsThisArgument && ctx.IsAssocMember then
            // is this a fluent API?
            match ident.Type, shouldBeRefCountWrapped com ctx ident.Type with
            | Fable.DeclaredType(entRef, genArgs), Some ptrType when ident.Type = returnType ->
                // for fluent APIs, set the type of thisArg to (self: &Lrc<Self>)
                let ty = mkImplSelfTy ()

                let ty =
                    match ptrType with
                    | Lrc -> ty |> makeFluentTy com ctx
                    | Rc -> ty |> makeRcTy com ctx
                    | Arc -> ty |> makeArcTy com ctx
                    | Box -> ty |> makeBoxTy com ctx
                    |> mkRefTy None

                mkTypedSelfParam ty false false
            | _ -> mkImplSelfParam false false
        elif ctx.IsLambda && ident.Type = Fable.Any then
            mkInferredParam ident.Name false false
        else
            let ty = transformType com ctx ident.Type
            mkTypedParam ident.Name ty false false

    let transformFunctionDecl (com: IRustCompiler) ctx args (parameters: Fable.Parameter list) returnType =
        let inputs =
            args
            |> List.mapi (fun idx ident ->
                let isByRefPreferred = parameterIsByRefPreferred idx parameters

                let ctx =
                    { ctx with IsParamByRefPreferred = isByRefPreferred || ctx.IsParamByRefPreferred }

                makeTypedParam com ctx ident returnType
            )

        let output =
            if returnType = Fable.Unit then
                VOID_RETURN_TY
            else
                let ctx = { ctx with IsParamByRefPreferred = false }
                let ty = transformType com ctx returnType
                ty |> mkFnRetTy

        mkFnDecl inputs output

    let shouldBeCloned com ctx typ =
        (isWrappedType com typ)
        // Closures may capture Ref counted vars, so by cloning
        // the actual closure, all attached ref counted var are cloned too
        || (shouldBeRefCountWrapped com ctx typ |> Option.isSome)

    let isClosedOverIdent com ctx (ident: Fable.Ident) =
        not (ident.IsCompilerGenerated && ident.Name = "matchValue")
        && (ident.IsMutable
            || isValueScoped ctx ident.Name
            || isRefScoped ctx ident.Name
            || shouldBeCloned com ctx ident.Type)
        // skip non-local idents (e.g. module let bindings)
        && not (ident.Name.Contains("."))

    let tryFindClosedOverIdent com ctx (ignoredNames: HashSet<string>) expr =
        match expr with
        | Fable.IdentExpr ident ->
            if not (ignoredNames.Contains(ident.Name)) && (isClosedOverIdent com ctx ident) then
                Some ident
            else
                None
        // add local names in the closure to the ignore list
        // TODO: not perfect, local name shadowing will ignore captured names
        | Fable.ForLoop(ident, _, _, _, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.Lambda(arg, _, _) ->
            ignoredNames.Add(arg.Name) |> ignore
            None
        | Fable.Delegate(args, _, _, _) ->
            args |> List.iter (fun arg -> ignoredNames.Add(arg.Name) |> ignore)
            None
        | Fable.Call(Fable.IdentExpr ident, info, _, _) when info.ThisArg.IsSome ->
            // ignore instance call idents
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.Let(ident, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.LetRec(bindings, _) ->
            bindings |> List.iter (fun (ident, _) -> ignoredNames.Add(ident.Name) |> ignore)

            None
        | Fable.DecisionTree(_, targets) ->
            targets
            |> List.iter (fun (idents, _) -> idents |> List.iter (fun ident -> ignoredNames.Add(ident.Name) |> ignore))

            None
        | Fable.TryCatch(body, catch, finalizer, _) ->
            catch
            |> Option.iter (fun (ident, expr) -> ignoredNames.Add(ident.Name) |> ignore)

            None
        | _ -> None

    let getIgnoredNames (name: string option) (args: Fable.Ident list) =
        let argNames = args |> List.map (fun arg -> arg.Name)
        let allNames = name |> Option.fold (fun xs x -> x :: xs) argNames
        allNames |> Set.ofList

    let hasCapturedIdents com ctx (name: string) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames (Some name) args)

        let isClosedOver expr =
            tryFindClosedOverIdent com ctx ignoredNames expr |> Option.isSome

        deepExists isClosedOver body

    let getCapturedIdents com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let ignoredNames = HashSet(getIgnoredNames name args)
        let mutable capturedIdents = Map.empty

        let addClosedOver expr =
            tryFindClosedOverIdent com ctx ignoredNames expr
            |> Option.iter (fun ident ->
                let identName =
                    if ident.IsThisArgument && ctx.IsAssocMember then
                        selfName
                    else
                        ident.Name

                capturedIdents <- capturedIdents |> Map.add identName ident
            )

            false
        // collect all closed over names that are not arguments
        deepExists addClosedOver body |> ignore
        capturedIdents

    let getFunctionBodyCtx com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) isTailRec =

        let tco =
            if isTailRec then
                Some(NamedTailCallOpportunity(com, ctx, name.Value, args) :> ITailCallOpportunity)
            else
                None

        let usages = calcIdentUsages args [ body ]

        let ctx =
            (ctx, args)
            ||> List.fold (fun ctx arg ->
                let isRef =
                    arg.IsThisArgument && ctx.IsAssocMember
                    || isByRefType com arg.Type
                    || ctx.IsParamByRefPreferred

                getScopedIdentCtx com ctx arg false isRef false false usages
            )

        { ctx with
            IsParamByRefPreferred = false
            TailCallOpportunity = tco
        }

    let isTailRecursive (nameOpt: string option) (body: Fable.Expr) =
        match nameOpt with
        | Some name -> FableTransforms.isTailRecursive name body
        | None -> false, false

    let transformFunctionBody com ctx (args: Fable.Ident list) (body: Fable.Expr) =
        match ctx.TailCallOpportunity with
        | Some tc ->
            // tail call elimination setup (temp vars, loop, break)
            let label = tc.Label

            let args =
                args
                |> List.filter (fun arg -> not (arg.IsMutable || arg.IsThisArgument && ctx.IsAssocMember))

            let mutArgs = args |> List.map (fun arg -> { arg with IsMutable = true })

            let idExprs = args |> List.map (fun arg -> Fable.IdentExpr arg)
            let bindings = List.zip mutArgs idExprs

            let argMap =
                mutArgs |> List.map (fun arg -> arg.Name, Fable.IdentExpr arg) |> Map.ofList

            let body = FableTransforms.replaceValues argMap body
            let letStmts, ctx = makeLetStmts com ctx bindings body Map.empty
            let loopBody = transformLeaveContext com ctx None body
            let loopExpr = mkBreakExpr (Some label) (Some(mkParenExpr loopBody))
            let loopStmt = mkLoopExpr (Some label) loopExpr |> mkExprStmt
            letStmts @ [ loopStmt ] |> mkStmtBlockExpr
        | _ -> transformLeaveContext com ctx None body

    let transformFunc com ctx parameters returnType (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let isRecursive, isTailRec = isTailRecursive name body
        let genArgs, ctx = getNewGenArgsAndCtx ctx args body
        let args = args |> discardUnitArg genArgs

        let returnType =
            match body.Type with
            | t when isByRefType com t -> returnType // if body type is byref, use the actual return type
            | _ -> body.Type // otherwise, use the body type as it has better matching generic parameters

        let fnDecl = transformFunctionDecl com ctx args parameters returnType
        let ctx = getFunctionBodyCtx com ctx name args body isTailRec
        let fnBody = transformFunctionBody com ctx args body
        fnDecl, fnBody, genArgs

    let transformLambda com ctx (name: string option) (args: Fable.Ident list) (body: Fable.Expr) =
        let ctx = { ctx with IsLambda = true }
        let genArgs, ctx = getNewGenArgsAndCtx ctx args body
        let args = args |> discardUnitArg genArgs
        let isRecursive, isTailRec = isTailRecursive name body
        let fnDecl = transformFunctionDecl com ctx args [] Fable.Unit
        let ctx = getFunctionBodyCtx com ctx name args body isTailRec
        let capturedIdents = getCapturedIdents com ctx name args body

        // remove captured idents from scoped symbols, as they will be cloned
        let scopedSymbols = ctx.ScopedSymbols |> Helpers.Map.except capturedIdents

        let ctx = { ctx with ScopedSymbols = scopedSymbols } //; HasMultipleUses = true }
        let argCount = args |> List.length |> string<int>
        let fnBody = transformFunctionBody com ctx args body

        let fnBody =
            if isRecursive && not isTailRec then
                // make the closure recursive with fixed-point combinator
                let fixedArgs = (makeIdent name.Value) :: args
                let fixedDecl = transformFunctionDecl com ctx fixedArgs [] Fable.Unit
                let fixedBody = mkClosureExpr true fixedDecl fnBody
                let argExprs = args |> List.map Fable.IdentExpr
                let callArgs = transformCallArgs com ctx argExprs [] []
                let fixCallArgs = (fixedBody |> mkAddrOfExpr) :: callArgs
                makeLibCall com ctx None "Native" ("fix" + argCount) fixCallArgs
            else
                fnBody

        let cloneStmts =
            // clone captured idents (in 'move' closures)
            Map.keys capturedIdents
            |> Seq.map (fun name ->
                let pat = makeFullNameIdentPat name
                let expr = com.TransformExpr(ctx, makeIdentExpr name)
                let value = expr |> makeClone
                let letExpr = mkLetExpr pat value
                letExpr |> mkSemiStmt
            )
            |> Seq.toList

        let closureExpr =
            if List.isEmpty cloneStmts then
                mkClosureExpr true fnDecl fnBody
            else
                let fnBody =
                    // additional captured idents cloning for recursive lambdas
                    if isRecursive && not isTailRec then
                        mkStmtBlockExpr (cloneStmts @ [ fnBody |> mkExprStmt ])
                    else
                        fnBody

                let closureExpr = mkClosureExpr true fnDecl fnBody
                cloneStmts @ [ closureExpr |> mkExprStmt ] |> mkStmtBlockExpr

        let funcWrap = getLibraryImportName com ctx "Native" ("Func" + argCount)

        makeCall [ funcWrap; "new" ] None [ closureExpr ]

    let makeTypeBounds (com: IRustCompiler) ctx argName (constraints: Fable.Constraint list) =
        let makeGenBound names tyNames =
            // makes gen type bound, e.g. T: From(i32), or T: Default
            let tys = tyNames |> List.map (fun tyName -> mkGenericPathTy [ tyName ] None)

            let genArgsOpt = mkConstraintArgs tys []
            mkTypeTraitGenericBound names genArgsOpt

        let makeImportBound com ctx moduleName typeName =
            let importName = getLibraryImportName com ctx moduleName typeName
            let objectBound = mkTypeTraitGenericBound [ importName ] None
            objectBound

        let makeRawBound id = makeGenBound [ rawIdent id ] []

        let makeOpBound op =
            // makes ops type bound, e.g. T: Add(Output=T)
            let ty = mkGenericPathTy [ argName ] None
            let genArgsOpt = mkConstraintArgs [] [ "Output", ty ]

            mkTypeTraitGenericBound ("core" :: "ops" :: op :: []) genArgsOpt

        let makeConstraint c =
            match c with
            | Fable.Constraint.HasMember(membName, isStatic) ->
                match membName, isStatic with
                | Operators.addition, true -> [ makeOpBound "Add" ]
                | Operators.subtraction, true -> [ makeOpBound "Sub" ]
                | Operators.multiply, true -> [ makeOpBound "Mul" ]
                | Operators.division, true -> [ makeOpBound "Div" ]
                | Operators.modulus, true -> [ makeOpBound "Rem" ]
                | Operators.unaryNegation, true -> [ makeOpBound "Neg" ]
                | Operators.divideByInt, true -> [ makeOpBound "Div"; makeGenBound [ rawIdent "From" ] [ "i32" ] ]
                | "get_Zero", true -> [ makeRawBound "Default" ]
                | _ -> []
            | Fable.Constraint.CoercesTo(targetType) ->
                match targetType with
                | IFormattable -> [ makeGenBound ("core" :: "fmt" :: "Display" :: []) [] ]
                | IComparable _ -> [ makeRawBound "PartialOrd" ]
                | IEquatable _ -> [ makeRawBound "PartialEq" ]
                | Fable.DeclaredType(entRef, genArgs) ->
                    let ent = com.GetEntity(entRef)

                    if ent.IsInterface then
                        let nameParts = getEntityFullName com ctx entRef |> splitNameParts
                        let genArgsOpt = transformGenArgs com ctx genArgs
                        let traitBound = mkTypeTraitGenericBound nameParts genArgsOpt
                        [ traitBound ]
                    else
                        []
                | _ -> []
            | Fable.Constraint.IsNullable -> [ makeImportBound com ctx "Native" "NullableRef" ]
            | Fable.Constraint.IsNotNullable -> []
            | Fable.Constraint.IsValueType -> []
            | Fable.Constraint.IsReferenceType -> [ makeImportBound com ctx "Native" "NullableRef" ]
            | Fable.Constraint.HasDefaultConstructor -> []
            | Fable.Constraint.HasAllowsRefStruct -> []
            | Fable.Constraint.HasComparison -> [ makeRawBound "PartialOrd" ]
            | Fable.Constraint.HasEquality ->
                [ makeGenBound ("core" :: "hash" :: "Hash" :: []) []; makeRawBound "PartialEq" ]
            | Fable.Constraint.IsUnmanaged -> []
            | Fable.Constraint.IsDelegate _ -> []
            | Fable.Constraint.IsEnum _ -> []
            | Fable.Constraint.SimpleChoice _ -> []

        constraints |> List.distinct |> List.collect makeConstraint

    let defaultInterfaceTypeBounds =
        [
            mkTypeTraitGenericBound ("core" :: "fmt" :: "Debug" :: []) None
            mkTypeTraitGenericBound ("core" :: "fmt" :: "Display" :: []) None
        ]

    let defaultTypeBounds =
        [
            mkTypeTraitGenericBound [ rawIdent "Clone" ] None
            // mkTypeTraitGenericBound [ rawIdent "Send" ] None
            // mkTypeTraitGenericBound [ rawIdent "Sync" ] None
            mkLifetimeGenericBound "'static" //TODO: add it only when needed
        ]

    // let makeDefaultTypeBounds com ctx =
    //     let importName = getLibraryImportName com ctx "Native" "IObject"
    //     let objectBound = mkTypeTraitGenericBound [ importName ] None
    //     objectBound :: defaultTypeBounds

    let makeGenericParams com ctx (genParams: Fable.GenericParam list) =
        genParams
        |> List.filter (fun p -> not p.IsMeasure)
        |> List.map (fun p ->
            let typeBounds = makeTypeBounds com ctx p.Name p.Constraints
            let typeBounds = typeBounds @ defaultTypeBounds
            mkGenericParamFromName [] p.Name typeBounds
        )

    let makeGenericParamsFromArgs com ctx (genArgs: Fable.Type list) =
        genArgs
        |> List.choose (
            function
            | Fable.GenericParam(name, isMeasure, constraints) when not isMeasure ->
                let typeBounds = makeTypeBounds com ctx name constraints
                let typeBounds = typeBounds @ defaultTypeBounds
                mkGenericParamFromName [] name typeBounds |> Some
            | _ -> None
        )

    let makeGenerics com ctx (genArgs: Fable.Type list) =
        makeGenericParamsFromArgs com ctx genArgs |> mkGenerics

    let makeMemberGenerics com ctx (genParams: Fable.GenericParam list) =
        makeGenericParams com ctx genParams |> mkGenerics

    let makeFnHeader com ctx (attributes: Fable.Attribute seq) : Rust.FnHeader =
        let isAsync = attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustAsync)
        let isConst = attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustConst)

        let isUnsafe =
            attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustUnsafe)

        let extOpt =
            attributes
            |> Seq.tryPick (fun a ->
                if a.Entity.FullName = Atts.rustExtern then
                    match a.ConstructorArgs with
                    | [] -> Some("")
                    | [ :? string as abi ] -> Some(abi)
                    | _ -> None
                else
                    None
            )

        mkFnHeader isUnsafe isAsync isConst extOpt

    let transformNestedFunction com ctx (ident: Fable.Ident) (args: Fable.Ident list) (body: Fable.Expr) usages =
        let name = ident.Name

        let fnDecl, fnBody, genArgs =
            transformFunc com ctx [] body.Type (Some name) args body

        let fnBodyBlock =
            if body.Type = Fable.Unit then
                mkSemiBlock fnBody
            else
                mkExprBlock fnBody

        let header = DEFAULT_FN_HEADER
        let generics = makeGenerics com ctx genArgs
        let fnKind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs = []
        let fnItem = mkFnItem attrs name fnKind
        let isFunc = true
        let ctx = getScopedIdentCtx com ctx ident false false false isFunc usages
        mkItemStmt fnItem, ctx

    let transformXmlDoc com ctx (xmlDoc: string option) =
        // convert XmlDoc to line comment attributes
        match xmlDoc with
        | Some text when text.Length > 0 ->
            let text =
                text
                |> Fable.Naming.replacePrefix "<summary>" ""
                |> Fable.Naming.replaceSuffix "</summary>" ""
                |> Fable.Naming.xmlDecode

            text.Trim('\n').Split('\n')
            |> Array.map (fun line -> line |> mkLineCommentAttr)
            |> Array.toList
        | _ -> []

    let transformAttributes com ctx (attributes: Fable.Attribute seq) (xmlDoc: string option) =
        attributes
        |> Seq.collect (fun a ->
            // Rust outer attributes
            if a.Entity.FullName = Atts.rustOuterAttr then
                match a.ConstructorArgs with
                | [ :? string as name ] -> [ mkAttr name [] ]
                | [ :? string as name; :? string as value ] -> [ mkEqAttr name value ]
                | [ :? string as name; :? (obj[]) as items ] -> [ mkAttr name (items |> Array.map string<obj>) ]
                | _ -> []
            // translate test methods attributes
            // TODO: support more test frameworks
            elif a.Entity.FullName.EndsWith(".FactAttribute", StringComparison.Ordinal) then
                [ mkAttr "test" [] ]
            else
                []
        )
        |> Seq.toList
        |> List.append (transformXmlDoc com ctx xmlDoc)

    let transformInnerAttributes com ctx (attributes: Fable.Attribute seq) =
        attributes
        |> Seq.collect (fun att ->
            // Rust inner attributes
            if att.Entity.FullName = Atts.rustInnerAttr then
                match att.ConstructorArgs with
                | [ :? string as name ] -> [ mkInnerAttr name [] ]
                | [ :? string as name; :? string as value ] -> [ mkInnerEqAttr name value ]
                | [ :? string as name; :? (obj[]) as items ] -> [ mkInnerAttr name (items |> Array.map string<obj>) ]
                | _ -> []
            else
                []
        )
        |> Seq.toList

    let getInnerAttributes (com: IRustCompiler) ctx (decls: Fable.Declaration list) =
        decls
        |> List.collect (fun decl ->
            match decl with
            | Fable.ModuleDeclaration decl ->
                let ent = com.GetEntity(decl.Entity)
                transformInnerAttributes com ctx ent.Attributes
            | Fable.ActionDeclaration decl -> []
            | Fable.MemberDeclaration decl ->
                let memb = com.GetMember(decl.MemberRef)
                transformInnerAttributes com ctx memb.Attributes
            | Fable.ClassDeclaration decl ->
                let ent = com.GetEntity(decl.Entity)
                transformInnerAttributes com ctx ent.Attributes
        )

    let transformModuleAction (com: IRustCompiler) ctx (body: Fable.Expr) =
        // optional, uses startup::on_startup! for static execution (before main).
        // See also: https://doc.rust-lang.org/1.6.0/complement-design-faq.html#there-is-no-life-before-or-after-main-no-static-ctorsdtors
        "For Rust, support for F# static and module do bindings is disabled by default. "
        + "It can be enabled with the 'static_do_bindings' feature. Use at your own risk!"
        |> addWarning com [] body.Range

        let expr = transformExpr com ctx body
        let attrs = [] //[mkAttr "cfg" ["feature = \"static_do_bindings\""]]
        let macroName = getLibraryImportName com ctx "Native" "on_startup"
        let macroItem = mkMacroItem attrs macroName [ expr ]
        [ macroItem ]

    // // not used anymore, as extension methods are compiled as normal module members
    // let transformExtensionMethod (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
    //     let argTypes = decl.Args |> List.map (fun arg -> arg.Type)
    //     match argTypes with
    //     | Fable.DeclaredType(entRef, genArgs) :: _ ->
    //         let entName = getEntityFullName com ctx entRef
    //         let memberItem = makeMemberItem com ctx true (decl, memb)
    //         let implItem = [ memberItem ] |> makeTraitImpl com ctx entName genArgs None
    //         [ implItem ]
    //     | _ -> []

    let transformModuleFunction (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        let name = Fable.Naming.splitLast decl.Name

        let isByRefPreferred =
            memb.Attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef)

        let fnDecl, fnBody, genArgs =
            let ctx = { ctx with IsParamByRefPreferred = isByRefPreferred }
            let parameters = memb.CurriedParameterGroups |> List.concat
            let returnType = memb.ReturnParameter.Type
            transformFunc com ctx parameters returnType (Some memb.FullName) decl.Args decl.Body

        let isUnsafe =
            memb.Attributes |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustUnsafe)

        let fnBodyBlock =
            if isUnsafe then
                fnBody |> mkUnsafeBlockExpr |> mkBodyBlock
            else if decl.Body.Type = Fable.Unit then
                mkSemiBlock fnBody
            else
                mkExprBlock fnBody

        let header = makeFnHeader com ctx memb.Attributes
        let generics = makeMemberGenerics com ctx memb.GenericParameters
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs = transformAttributes com ctx memb.Attributes memb.XmlDoc
        let fnItem = mkFnItem attrs name kind

        let memberItem = fnItem |> memberItemWithVis com ctx memb
        [ memberItem ]

    let transformModuleLetValue (com: IRustCompiler) ctx (memb: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        // expected output:
        // pub fn value() -> T {
        //     static value: OnceInit<T> = OnceInit::new();
        //     value.get_or_init(|| initValue).clone()
        // }
        let name = Fable.Naming.splitLast decl.Name
        let typ = decl.Body.Type

        let initNone = makeNew com ctx "Native" "OnceInit" []
        let value = transformLeaveContext com ctx None decl.Body

        let value =
            if memb.IsMutable then
                value |> makeMutValue com ctx |> makeLrcPtrValue com ctx
            else
                value

        let ty = transformType com ctx typ

        let ty =
            if memb.IsMutable then
                ty |> makeMutTy com ctx |> makeLrcPtrTy com ctx
            else
                ty

        let staticTy = [ ty ] |> makeImportType com ctx "Native" "OnceInit"
        let staticStmt = mkStaticItem [] name staticTy (Some initNone) |> mkItemStmt

        let callee = com.TransformExpr(ctx, makeIdentExpr name)

        let closureExpr =
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            mkClosureExpr false fnDecl value

        let valueStmt =
            mkMethodCallExpr "get_or_init" None callee [ closureExpr ]
            |> makeClone
            |> mkExprStmt

        let attrs = transformAttributes com ctx memb.Attributes memb.XmlDoc

        let fnBody = [ staticStmt; valueStmt ] |> mkBlock |> Some

        let fnDecl = mkFnDecl [] (mkFnRetTy ty)
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnItem attrs name fnKind

        let memberItem = fnItem |> memberItemWithVis com ctx memb
        [ memberItem ]

    // // is the member return type the same as the entity
    // let isFluentMemberType (ent: Fable.Entity) = function
    //     | Fable.DeclaredType(entRef, _) -> entRef.FullName = ent.FullName
    //     | _ -> false

    let isInterfaceMember (com: IRustCompiler) (memb: Fable.MemberFunctionOrValue) =
        (memb.IsDispatchSlot || memb.IsOverrideOrExplicitInterfaceImplementation)
        && (memb.DeclaringEntity
            |> Option.bind com.TryGetEntity
            |> Option.map (fun ent -> ent.IsInterface)
            |> Option.defaultValue false)

    let isIdentAtTailPos (predicate: Fable.Ident -> bool) (body: Fable.Expr) =
        let rec loop =
            function
            | Fable.IdentExpr ident when predicate ident -> true
            | Fable.Sequential exprs -> loop (List.last exprs)
            | Fable.Let(_, value, body) -> loop body
            | Fable.LetRec(bindings, body) -> loop body
            | Fable.IfThenElse(cond, thenExpr, elseExpr, _) -> loop thenExpr || loop elseExpr
            | Fable.DecisionTree(expr, targets) -> List.map snd targets |> List.exists loop
            | _ -> false

        loop body

    let makeMemberAssocItem
        com
        ctx
        (memb: Fable.MemberFunctionOrValue)
        (args: Fable.Ident list)
        (bodyOpt: Rust.Block option)
        =
        let ctx = { ctx with IsAssocMember = true }
        let name = memb.CompiledName
        let args = args |> discardUnitArg []
        let parameters = memb.CurriedParameterGroups |> List.concat
        let returnType = memb.ReturnParameter.Type //|> FableTransforms.uncurryType
        let fnDecl = transformFunctionDecl com ctx args parameters returnType
        let genArgs = FSharp2Fable.Util.getMemberGenArgs memb
        let generics = makeGenerics com ctx genArgs
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics bodyOpt
        let attrs = transformAttributes com ctx memb.Attributes memb.XmlDoc

        let attrs =
            if bodyOpt.IsSome then
                attrs @ [ mkAttr "inline" [] ]
            else
                attrs

        let fnItem = mkFnAssocItem attrs name fnKind
        fnItem

    let maybeConstructorBaseCall (com: IRustCompiler) (body: Fable.Expr) =
        // support explicit field constructor base calls
        // body pattern: Sequential([Call _; NewRecord _])
        match body with
        | Fable.Sequential([ first; second ]) ->
            match first, second with
            | Fable.Call(_, info, typ, _), Fable.Value(Fable.NewRecord(values, entRef, genArgs), r) ->
                let membOpt = info.MemberRef |> Option.bind com.TryGetMember

                match membOpt, typ with
                | Some memb, Fable.DeclaredType(baseEntRef, _) when memb.IsConstructor && isValidBaseType com baseEntRef ->
                    let values = first :: values // add base call as first field value to the record
                    let body = Fable.Value(Fable.NewRecord(values, entRef, genArgs), r)
                    body
                | _ -> body
            | _ -> body
        | _ -> body

    let transformAssocMember com ctx (memb: Fable.MemberFunctionOrValue) (decl: Fable.MemberDecl) =
        let ctx = { ctx with IsAssocMember = true }

        let name =
            if isInterfaceMember com memb then
                memb.CompiledName // no name mangling for interfaces
            else
                Fable.Naming.splitLast decl.Name

        let args = decl.Args
        let body = decl.Body

        let body =
            if memb.IsInstance && not (memb.IsConstructor) then
                let ident = makeIdent selfName
                let thisArg = makeIdentExpr (rawIdent "self")
                Fable.Let(ident, thisArg, body)
            elif memb.IsConstructor then
                body |> maybeConstructorBaseCall com
            else
                body

        let fnDecl, fnBody, genArgs =
            let parameters = memb.CurriedParameterGroups |> List.concat
            let returnType = memb.ReturnParameter.Type
            transformFunc com ctx parameters returnType (Some name) args body

        let fnBody =
            if isIdentAtTailPos (fun ident -> ident.IsThisArgument) body then
                // body returns ThisArg, i.e. Fluent API
                fnBody |> makeFluentValue com ctx
            else
                fnBody

        let fnBody =
            if body.Type = Fable.Unit then
                mkSemiBlock fnBody
            else
                mkExprBlock fnBody

        let generics = makeGenerics com ctx genArgs
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics (Some fnBody)
        let attrs = transformAttributes com ctx memb.Attributes decl.XmlDoc
        let fnItem = mkFnAssocItem attrs name fnKind
        fnItem

    let makeDerivedFrom com (ent: Fable.Entity) =
        let isCopyable = false //ent |> isCopyableEntity com Set.empty
        let isCloneable = true //ent |> isCloneableEntity com Set.empty
        let isPrintable = ent |> isPrintableEntity com Set.empty
        let isDefaultable = ent |> isDefaultableEntity com Set.empty
        let isHashable = ent |> isHashableEntity com Set.empty
        let isEquatable = ent |> isEquatableEntity com Set.empty
        let isComparable = ent |> isComparableEntity com Set.empty

        let derivedFrom =
            [
                if isCopyable then
                    rawIdent "Copy"
                if isCloneable then
                    rawIdent "Clone"
                if isPrintable then
                    rawIdent "Debug"
                if isDefaultable then
                    rawIdent "Default"
                if isHashable then
                    rawIdent "Hash"
                if isEquatable then
                    rawIdent "PartialEq"
                if isComparable then
                    rawIdent "PartialOrd"
            ]

        derivedFrom

    let transformAbbrev (com: IRustCompiler) ctx (ent: Fable.Entity) (decl: Fable.ClassDecl) =
        // TODO: this is unfinished and untested
        let entName = Fable.Naming.splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound [ entName ] genArgsOpt
        let ty = mkTraitTy [ traitBound ]
        let generics = makeGenerics com ctx genArgs
        let bounds = [] //TODO:
        let attrs = transformAttributes com ctx ent.Attributes decl.XmlDoc
        let tyItem = mkTyAliasItem attrs entName ty generics bounds
        [ tyItem ]

    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) (decl: Fable.ClassDecl) =
        let entName = Fable.Naming.splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let generics = makeGenerics com ctx genArgs

        let variants =
            ent.UnionCases
            |> Seq.map (fun uci ->
                let name = uci.Name
                let isPublic = false

                let fields =
                    uci.UnionCaseFields
                    |> List.map (fun field ->
                        let typ = FableTransforms.uncurryType field.FieldType
                        let fieldTy = transformType com ctx typ
                        let fieldName = field.Name |> sanitizeMember
                        mkField [] fieldName fieldTy isPublic
                    )

                if List.isEmpty uci.UnionCaseFields then
                    mkUnitVariant [] name
                else
                    mkTupleVariant [] name fields
            )

        let attrs = transformAttributes com ctx ent.Attributes decl.XmlDoc
        let attrs = attrs @ [ mkAttr "derive" (makeDerivedFrom com ent) ]
        let enumItem = mkEnumItem attrs entName variants generics
        enumItem

    let ignoredBaseTypes = set [ Types.object; Types.valueType; Types.exception_ ]

    let isValidBaseType (com: IRustCompiler) (entRef: Fable.EntityRef) =
        if Set.contains entRef.FullName ignoredBaseTypes then
            false
        else
            let ent = com.GetEntity(entRef)
            not ent.IsInterface && not ent.IsValueType

    let transformClass (com: IRustCompiler) ctx (ent: Fable.Entity) (decl: Fable.ClassDecl) =
        let entName = Fable.Naming.splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let generics = makeGenerics com ctx genArgs
        let isPublic = ent.IsFSharpRecord
        let idents = getEntityFieldsAsIdents com ent

        let fields =
            idents
            |> List.map (fun ident ->
                let fieldTy = transformIdentType com ctx false ident
                let fieldName = ident.Name |> sanitizeMember
                mkField [] fieldName fieldTy isPublic
            )

        let phantomFields =
            getEntityPhantomGenParams com ent
            |> List.map (fun (name, typ) ->
                let genArgsOpt = transformGenArgs com ctx [ typ ]
                let fieldTy = mkGenericPathTy ("core" :: "marker" :: "PhantomData" :: []) genArgsOpt
                let fieldName = $"phantom_%s{name}" |> sanitizeMember
                mkField [] fieldName fieldTy isPublic
            )

        let fields = List.append fields phantomFields

        let attrs = transformAttributes com ctx ent.Attributes decl.XmlDoc
        let attrs = attrs @ [ mkAttr "derive" (makeDerivedFrom com ent) ]
        let structItem = mkStructItem attrs entName fields generics
        structItem

    let transformCompilerGeneratedConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) =
        // let ctor = ent.MembersFunctionsAndValues |> Seq.tryFind (fun q -> q.CompiledName = ".ctor")
        // ctor |> Option.map (fun ctor -> ctor.CurriedParameterGroups)
        let makeIdentValue (ident: Fable.Ident) =
            { ident with
                Name = ident.Name |> sanitizeMember
                IsMutable = false
            }

        let idents = getEntityFieldsAsIdents com ent
        let args = idents |> List.map makeIdentValue
        let values = args |> List.map Fable.IdentExpr
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let body = Fable.Value(Fable.NewRecord(values, ent.Ref, genArgs), None)
        let entName = getEntityFullName com ctx ent.Ref
        let paramTypes = args |> List.map (fun ident -> ident.Type)

        let memberRef =
            Fable.GeneratedMember.Function(entName, paramTypes, body.Type, isInstance = false, entRef = ent.Ref)

        let name = "new"
        let memb = com.GetMember(memberRef)

        let ctor: Fable.MemberDecl =
            {
                Name = name
                Args = args
                Body = body
                MemberRef = memberRef
                IsMangled = false
                ImplementedSignatureRef = None
                UsedNames = Set.empty
                XmlDoc = None
                Tags = []
            }

        let fnItem = transformAssocMember com ctx memb ctor
        let fnItem = fnItem |> memberAssocItemWithVis com ctx memb
        fnItem

    let transformPrimaryConstructor (com: IRustCompiler) ctx (ent: Fable.Entity) (ctor: Fable.MemberDecl) baseCall =
        let body =
            match ctor.Body with
            | Fable.Sequential exprs ->
                // get fields
                let idents = getEntityFieldsAsIdents com ent

                let argNames = ctor.Args |> List.map (fun arg -> arg.Name) |> Set.ofList

                let identMap =
                    idents
                    |> List.map (fun ident ->
                        let fieldName = ident.Name |> sanitizeMember
                        let uniqueName = makeUniqueName fieldName argNames

                        ident.Name,
                        { ident with
                            Name = uniqueName
                            IsMutable = false
                        }
                    )
                    |> Map.ofList

                let fieldIdents = idents |> List.map (fun ident -> Map.find ident.Name identMap)

                let fieldValues = fieldIdents |> List.map Fable.IdentExpr

                let exprs =
                    match ent.BaseType, baseCall with
                    | Some t, Some ctorExpr when isValidBaseType com t.Entity ->
                        let baseIdent = identMap |> Map.find baseName
                        let identExpr = baseIdent |> Fable.IdentExpr
                        let baseExpr = Fable.Set(identExpr, Fable.ValueSet, baseIdent.Type, ctorExpr, None)
                        baseExpr :: exprs
                    | _ -> exprs

                let genArgs = FSharp2Fable.Util.getEntityGenArgs ent

                // add return value after the body
                let retVal = Fable.Value(Fable.NewRecord(fieldValues, ent.Ref, genArgs), None)

                let body = Fable.Sequential(exprs @ [ retVal ])
                // replace 'this.field' getters and setters with just 'field' in body
                let body =
                    body
                    |> visitFromInsideOut (
                        function
                        | Fable.Set(Fable.Value(Fable.ThisValue _, _), Fable.SetKind.FieldSet(fieldName), t, value, r) ->
                            let identExpr = identMap |> Map.find fieldName |> Fable.IdentExpr
                            Fable.Set(identExpr, Fable.ValueSet, t, value, r)
                        | Fable.Get(Fable.Value(Fable.ThisValue _, _), Fable.GetKind.FieldGet info, t, r) ->
                            let identExpr = identMap |> Map.find info.Name |> Fable.IdentExpr
                            identExpr
                        | e -> e
                    )
                // add field declarations before body
                let body =
                    (body, fieldIdents |> List.rev)
                    ||> List.fold (fun acc ident ->
                        // use special init value to skip initialization (i.e. declaration only)
                        let nullOfT = Fable.Value(Fable.Null Fable.MetaType, None)
                        Fable.Let(ident, nullOfT, acc)
                    )

                body
            | e -> e

        let ctor = { ctor with Body = body }
        let memb = com.GetMember(ctor.MemberRef)
        let fnItem = transformAssocMember com ctx memb ctor
        let fnItem = fnItem |> memberAssocItemWithVis com ctx memb
        fnItem

    let makeInterfaceItems (com: IRustCompiler) ctx hasBody typeName (ent: Fable.Entity) =
        getDistinctInterfaceMembers com ent
        |> Seq.map (fun (ifc, memb) ->
            let ifcEnt = com.GetEntity(ifc.Entity)
            let ifcTyp = Fable.DeclaredType(ifc.Entity, ifc.GenericArgs)
            let thisArg = { makeTypedIdent ifcTyp "this" with IsThisArgument = true }

            let membName = memb.CompiledName

            let memberArgs =
                memb.CurriedParameterGroups
                |> List.collect id
                |> List.mapi (fun i p ->
                    let name = defaultArg p.Name $"arg{i}"
                    let typ = FableTransforms.uncurryType p.Type
                    makeTypedIdent typ name
                )

            let args =
                if memb.IsInstance then
                    thisArg :: memberArgs
                else
                    memberArgs

            let bodyOpt =
                if hasBody then
                    let args = memberArgs |> List.map (transformIdent com ctx None)

                    let body =
                        if memb.IsInstance then
                            // let thisExpr = makeSelf com ctx None ifcTyp
                            let thisExpr = mkGenericPathExpr [ rawIdent "self" ] None
                            let callee = thisExpr |> mkDerefExpr |> mkDerefExpr
                            mkMethodCallExpr membName None callee args
                        else
                            makeCall [ typeName; membName ] None args

                    [ mkExprStmt body ] |> mkBlock |> Some
                else
                    None

            makeMemberAssocItem com ctx memb args bodyOpt
        )

    let transformInterface (com: IRustCompiler) ctx (ent: Fable.Entity) (decl: Fable.ClassDecl) =
        let entName = Fable.Naming.splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let genArgNames = getEntityGenParamNames ent
        let typeName = makeUniqueName "V" genArgNames

        let traitItem =
            let assocItems = makeInterfaceItems com ctx false typeName ent
            let generics = makeGenerics com ctx genArgs
            let traitBounds = defaultInterfaceTypeBounds
            let attrs = transformAttributes com ctx ent.Attributes decl.XmlDoc
            mkTraitItem attrs entName assocItems traitBounds generics

        // implements the interface for the Lrc<dyn interface> type
        let implItem =
            let memberItems = makeInterfaceItems com ctx true typeName ent
            let genArgsOpt = transformGenArgs com ctx genArgs
            let traitBound = mkTypeTraitGenericBound [ entName ] genArgsOpt
            let typeBounds = traitBound :: defaultInterfaceTypeBounds
            let typeParam = mkGenericParamFromName [] typeName typeBounds
            let genParams = makeGenericParamsFromArgs com ctx genArgs
            let generics = typeParam :: genParams |> mkGenerics
            let ty = mkGenericTy [ typeName ] [] |> makeLrcPtrTy com ctx
            let path = mkGenericPath [ entName ] genArgsOpt
            let ofTrait = mkTraitRef path |> Some
            mkImplItem [] "" ty generics memberItems ofTrait

        [ traitItem |> mkPublicItem; implItem ]

    let makeFSharpExceptionItems com ctx (ent: Fable.Entity) =
        // expected output:
        // impl {entityName} {
        //     fn get_Message(&self) -> string {
        //         __self__ = self;
        //         sformat!("{} {:?}", entName, (__self__.Data0.clone(), __self__.Data1.clone(), ...)))
        //     }
        // }
        if ent.IsFSharpExceptionDeclaration then
            let entName = Fable.Naming.splitLast ent.FullName
            let entNameExpr = Fable.Value(Fable.StringConstant(entName), None)

            let thisArg = Fable.Value(Fable.ThisValue Fable.Any, None)

            let fieldValues =
                getEntityFieldsAsIdents com ent
                |> List.map (fun ident ->
                    let info = Fable.FieldInfo.Create(ident.Name, ident.Type, ident.IsMutable)
                    Fable.Get(thisArg, info, ident.Type, None)
                )

            let fieldsAsTuple = Fable.Value(Fable.NewTuple(fieldValues, true), None)

            let formatExpr =
                let rustFmt = "{} {:?}"
                let templateArgs = [ entNameExpr; fieldsAsTuple ]
                Fable.Value(Fable.StringTemplate(None, [ rustFmt ], templateArgs), None)

            let bodyExpr =
                let ident = makeIdent selfName
                let thisArg = makeIdentExpr (rawIdent "self")
                Fable.Let(ident, thisArg, formatExpr)

            let body = transformExpr com ctx bodyExpr
            let fnBody = [ mkExprStmt body ] |> mkBlock |> Some
            let fnRetTy = Fable.String |> transformType com ctx |> mkFnRetTy
            let fnDecl = mkFnDecl [ mkImplSelfParam false false ] fnRetTy
            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            let attrs = []
            let fnItem = mkFnAssocItem attrs "get_Message" fnKind
            [ fnItem ]
        else
            []

    let makeDerefTraitImpls com ctx entName genArgs (baseType: Fable.Type option) (bodyExpr: string) =
        // expected output:
        // impl<T> core::ops::Deref for EntType<T> {
        //     type Target = Lrc<BaseType<T>>;
        //
        //     #[inline]
        //     fn deref(&self) -> &Self::Target {
        //         bodyExpr
        //     }
        // }
        match baseType with
        | Some(Fable.DeclaredType(baseEntRef, baseGenArgs)) when isValidBaseType com baseEntRef ->
            let baseTy = transformEntityType com ctx baseEntRef baseGenArgs
            let targetTy = baseTy |> makeLrcPtrTy com ctx
            let tyItem = mkTyAliasAssocItem [] "Target" targetTy NO_GENERICS []

            let bodyStmt = bodyExpr |> mkEmitExprStmt
            let fnBody = [ bodyStmt ] |> mkBlock |> Some

            let fnDecl =
                let inputs = [ mkImplSelfParam false false ]
                let selfTy = mkGenericPathTy ("Self" :: "Target" :: []) None
                let output = selfTy |> mkRefTy None |> mkFnRetTy
                mkFnDecl inputs output

            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            let attrs = [ mkAttr "inline" [] ]
            let fnItem = mkFnAssocItem attrs "deref" fnKind

            let path = mkGenericPath ("core" :: "ops" :: "Deref" :: []) None
            let ofTrait = mkTraitRef path |> Some
            let assocItems = [ tyItem; fnItem ]

            let implItem = assocItems |> makeTraitImpl com ctx entName genArgs ofTrait
            [ implItem ]
        | _ -> []

    let makeDisplayTraitImpls com ctx entName genArgs hasToString hasDebug =
        // expected output:
        // impl core::fmt::Display for {self_ty} {
        //     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        //         write!(f, "{}", self.ToString_())
        //     }
        // }
        let bodyStmt =
            if hasToString then
                "write!(f, \"{}\", self.ToString_())"
            else
                "write!(f, \"{}\", core::any::type_name::<Self>())"
            |> mkEmitExprStmt

        let fnBody = [ bodyStmt ] |> mkBlock |> Some

        let fnDecl =
            let inputs =
                let ty = mkGenericPathTy ("core" :: "fmt" :: "Formatter" :: []) None
                let p1 = mkImplSelfParam false false
                let p2 = mkTypedParam "f" (ty |> mkMutRefTy None) false false
                [ p1; p2 ]

            let output =
                let ty = mkGenericPathTy ("core" :: "fmt" :: (rawIdent "Result") :: []) None
                ty |> mkFnRetTy

            mkFnDecl inputs output

        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnAssocItem [] "fmt" fnKind

        let makeItem fmtTrait =
            let path = mkGenericPath ("core" :: "fmt" :: fmtTrait :: []) None
            let ofTrait = mkTraitRef path |> Some
            let implItem = [ fnItem ] |> makeTraitImpl com ctx entName genArgs ofTrait
            implItem

        if hasDebug then
            [ makeItem "Debug"; makeItem "Display" ]
        else
            [ makeItem "Display" ]

    let op_impl_map =
        Map
            [
                Operators.unaryNegation, ("un_op", "Neg", "neg") // The unary negation operator -.
                Operators.logicalNot, ("un_op", "Not", "not") // The unary logical negation operator !.

                Operators.addition, ("bin_op", "Add", "add") // The addition operator +.
                Operators.subtraction, ("bin_op", "Sub", "sub") // The subtraction operator -.
                Operators.multiply, ("bin_op", "Mul", "mul") // The multiplication operator *.
                Operators.division, ("bin_op", "Div", "div") // The division operator /.
                Operators.modulus, ("bin_op", "Rem", "rem") // The remainder operator %.

                Operators.bitwiseAnd, ("bin_op", "BitAnd", "bitand") // The bitwise AND operator &.
                Operators.bitwiseOr, ("bin_op", "BitOr", "bitor") // The bitwise OR operator |.
                Operators.exclusiveOr, ("bin_op", "BitXor", "bitxor") // The bitwise XOR operator ^.

                Operators.leftShift, ("bin_op", "Shl", "shl") // The left shift operator <<.
                Operators.rightShift, ("bin_op", "Shr", "shr") // The right shift operator >>.
            ]

    let makeOpTraitImpls
        com
        ctx
        (ent: Fable.Entity)
        entName
        genArgs
        (members: (Fable.MemberDecl * Fable.MemberFunctionOrValue) list)
        =
        let entType = FSharp2Fable.Util.getEntityType ent
        let genArgTys = transformGenTypes com ctx genArgs
        let genArgsOpt = transformGenArgs com ctx genArgs
        let self_ty = makeFullNamePathTy entName genArgsOpt

        members
        |> List.choose (fun (decl, memb) ->
            op_impl_map
            |> Map.tryFind memb.CompiledName
            |> Option.filter (fun _ ->
                // TODO: more checks if parameter types match the operator?
                ent.IsValueType
                && not (memb.IsInstance) // operators are static
                && decl.Args.Head.Type = entType
                && decl.Body.Type = entType
            )
            |> Option.map (fun (op_macro, op_trait, op_fn) ->
                let rhs_tys =
                    decl.Args.Tail
                    |> List.map (fun arg ->
                        if arg.Type = entType then
                            mkImplSelfTy ()
                        else
                            arg.Type |> transformType com ctx
                    )

                let macroName = getLibraryImportName com ctx "Native" op_macro
                let id_tokens = [ op_trait; op_fn; decl.Name ] |> List.map mkIdentToken
                let ty_tokens = (self_ty :: rhs_tys) @ genArgTys |> List.map mkTyToken

                let implItem =
                    id_tokens @ ty_tokens
                    |> mkParensCommaDelimitedMacCall macroName
                    |> mkMacCallItem [] ""

                implItem
            )
        )

    let withCurrentScope ctx (usedNames: Set<string>) f =
        let ctx =
            { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }

        let result = f ctx
        ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
        result

    let makeMemberItem (com: IRustCompiler) ctx withVis (decl: Fable.MemberDecl, memb: Fable.MemberFunctionOrValue) =
        withCurrentScope ctx decl.UsedNames
        <| fun ctx ->
            let memberItem = transformAssocMember com ctx memb decl

            if withVis then
                memberItem |> memberAssocItemWithVis com ctx memb
            else
                memberItem

    let makePrimaryConstructorItems com ctx (ent: Fable.Entity) (decl: Fable.ClassDecl) =
        if
            ent.IsFSharpUnion
            || ent.IsFSharpRecord
            || ent.IsInterface
            || ent.IsFSharpExceptionDeclaration
        then
            []
        else
            let ctorItem =
                match decl.Constructor with
                | Some ctor ->
                    withCurrentScope ctx ctor.UsedNames
                    <| fun ctx -> transformPrimaryConstructor com ctx ent ctor decl.BaseCall
                | _ -> transformCompilerGeneratedConstructor com ctx ent

            [ ctorItem ]

    let makeTraitImpl (com: IRustCompiler) ctx entName genArgs ofTrait memberItems =
        let nameParts = entName |> splitNameParts
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound nameParts genArgsOpt
        let self_ty = mkTraitTy [ traitBound ]
        let generics = makeGenerics com ctx genArgs
        let implItem = mkImplItem [] "" self_ty generics memberItems ofTrait
        implItem

    let makeInterfaceTraitImpls (com: IRustCompiler) ctx entName genArgs ifcEntRef ifcGenArgs memberItems =
        let ifcFullName = getEntityFullName com ctx ifcEntRef
        let ifcGenArgsOpt = transformGenArgs com ctx ifcGenArgs
        let path = makeFullNamePath ifcFullName ifcGenArgsOpt
        let ofTrait = mkTraitRef path |> Some
        let implItem = memberItems |> makeTraitImpl com ctx entName genArgs ofTrait
        [ implItem ]

    let findInterfaceGenArgs (com: IRustCompiler) (ent: Fable.Entity) (ifcEntRef: Fable.EntityRef) =
        let ifcOpt =
            ent.AllInterfaces
            |> Seq.tryFind (fun ifc -> ifc.Entity.FullName = ifcEntRef.FullName)

        match ifcOpt with
        | Some ifc -> ifc.GenericArgs
        | _ ->
            // shouldn't really happen
            let ifcEnt = com.GetEntity(ifcEntRef)
            FSharp2Fable.Util.getEntityGenArgs ifcEnt

    let ignoredInterfaceNames = set [ Types.ienumerable; Types.ienumerator ]

    let transformClassMembers (com: IRustCompiler) ctx genArgs (classDecl: Fable.ClassDecl) =
        let entRef = classDecl.Entity
        let ent = com.GetEntity(entRef)

        let isObjectExpr = classDecl.Name = "ObjectExpr"

        let entName =
            if isObjectExpr then
                classDecl.Name
            else
                getEntityFullName com ctx entRef |> Fable.Naming.splitLast

        let genParams = FSharp2Fable.Util.getGenParamTypes genArgs

        let ctx = { ctx with ScopedEntityGenArgs = getEntityGenParamNames ent }

        let isIgnoredMember (memb: Fable.MemberFunctionOrValue) = ent.IsFSharpExceptionDeclaration // to filter out compiler-generated exception equality

        let interfaceMembers, nonInterfaceMembers =
            classDecl.AttachedMembers
            |> List.map (fun decl -> decl, getDeclMember com decl)
            |> List.partition (snd >> isInterfaceMember com)

        let nonInterfaceImpls =
            let memberItems =
                nonInterfaceMembers
                |> List.filter (snd >> isIgnoredMember >> not)
                |> List.map (makeMemberItem com ctx true)

            let memberItems =
                if isObjectExpr then
                    memberItems
                else
                    memberItems
                    |> List.append (makeFSharpExceptionItems com ctx ent)
                    |> List.append (makePrimaryConstructorItems com ctx ent classDecl)

            if List.isEmpty memberItems then
                []
            else
                let implItem = memberItems |> makeTraitImpl com ctx entName genArgs None
                [ implItem ]

        let baseTypeOpt =
            if isObjectExpr then
                Some(Fable.DeclaredType(entRef, genArgs))
            else
                ent.BaseType
                |> Option.map (fun t -> Fable.DeclaredType(t.Entity, t.GenericArgs))

        let derefTraitImpls =
            (baseTypeOpt, "&self." + baseName)
            ||> makeDerefTraitImpls com ctx entName genArgs

        let displayTraitImpls =
            let hasToString =
                nonInterfaceMembers |> List.exists (fun (d, m) -> m.CompiledName = "ToString")

            let hasDebug = not (List.isEmpty ent.GenericParameters)
            makeDisplayTraitImpls com ctx entName genParams hasToString hasDebug

        let operatorTraitImpls =
            nonInterfaceMembers |> makeOpTraitImpls com ctx ent entName genArgs

        let interfaceTraitImpls =
            interfaceMembers
            |> List.choose (fun (d, m) -> m.DeclaringEntity)
            |> List.distinctBy (fun ifcEntRef -> ifcEntRef.FullName)
            |> List.filter (fun ifcEntRef ->
                // throws out anything on the ignored interfaces list
                not (Set.contains ifcEntRef.FullName ignoredInterfaceNames)
            )
            |> List.collect (fun ifcEntRef ->
                let ifcGenArgs =
                    if isObjectExpr then
                        genArgs
                    else
                        ifcEntRef |> findInterfaceGenArgs com ent

                let memberNames = getInterfaceMemberNames com ifcEntRef

                let memberItems =
                    interfaceMembers
                    |> List.filter (fun (d, m) -> Set.contains m.FullName memberNames)
                    |> List.map (makeMemberItem com ctx false)

                memberItems
                |> makeInterfaceTraitImpls com ctx entName genParams ifcEntRef ifcGenArgs
            )

        derefTraitImpls
        @ nonInterfaceImpls
        @ displayTraitImpls
        @ operatorTraitImpls
        @ interfaceTraitImpls

    let transformClassDecl (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let ent = com.GetEntity(decl.Entity)

        if ent.IsFSharpAbbreviation then
            transformAbbrev com ctx ent decl
        elif ent.IsInterface then
            transformInterface com ctx ent decl
        else
            let entityItem =
                if ent.IsFSharpUnion then
                    transformUnion com ctx ent decl
                else
                    transformClass com ctx ent decl
                |> entityItemWithVis com ctx ent

            let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
            let memberItems = transformClassMembers com ctx genArgs decl
            entityItem :: memberItems

    let getVis (com: IRustCompiler) ctx declaringEntity isInternal isPrivate =
        // If the declaring entity is internal or private, it affects
        // default member visibility, so we need to compensate for that.
        match declaringEntity |> Option.bind com.TryGetEntity with
        | Some declaringEnt ->
            let isInternal = isInternal && not (declaringEnt.IsInternal)
            let isPrivate = isPrivate && not (declaringEnt.IsPrivate)
            isInternal, isPrivate
        | _ -> isInternal, isPrivate

    let entityItemWithVis com ctx (ent: Fable.Entity) entityItem =
        let isInternal, isPrivate =
            getVis com ctx ent.DeclaringEntity ent.IsInternal ent.IsPrivate

        entityItem |> mkItemWithVis isInternal isPrivate

    let memberItemWithVis com ctx (memb: Fable.MemberFunctionOrValue) memberItem =
        let isInternal, isPrivate =
            getVis com ctx memb.DeclaringEntity memb.IsInternal memb.IsPrivate

        memberItem |> mkItemWithVis isInternal isPrivate

    let memberAssocItemWithVis com ctx (memb: Fable.MemberFunctionOrValue) memberAssocItem =
        let isInternal, isPrivate =
            getVis com ctx memb.DeclaringEntity memb.IsInternal memb.IsPrivate

        memberAssocItem |> mkAssocItemWithVis isInternal isPrivate

    let mergeNamespaceDecls (com: IRustCompiler) ctx decls =
        // separate namespace decls from the others
        let namespaceDecls, otherDecls =
            decls
            |> List.partition (
                function
                | Fable.ModuleDeclaration d ->
                    let ent = com.GetEntity(d.Entity)
                    ent.IsNamespace
                | _ -> false
            )
        // merge namespace decls with the same name into a single decl
        let namespaceDecls =
            namespaceDecls
            |> List.groupBy (
                function
                | Fable.ModuleDeclaration d -> d.Name
                | _ -> failwith "unreachable"
            )
            |> List.map (fun (key, decls) ->
                match decls with
                | [ d ] -> d // no merge needed
                | _ ->
                    let members =
                        decls
                        |> List.map (
                            function
                            | Fable.ModuleDeclaration d -> d.Members
                            | _ -> []
                        )
                        |> List.concat

                    match List.head decls with
                    | Fable.ModuleDeclaration d -> Fable.ModuleDeclaration { d with Members = members }
                    | d -> d
            )
        // return merged decls
        List.append namespaceDecls otherDecls

    let transformModuleDecl (com: IRustCompiler) ctx (decl: Fable.ModuleDecl) =
        let ctx = { ctx with ModuleDepth = ctx.ModuleDepth + 1 }

        let memberDecls =
            // Instead of transforming declarations depth-first, i.e.
            // (decl.Members |> List.collect (transformDecl com ctx)),
            // this prioritizes non-module declaration transforms first,
            // so module imports can be properly deduped top to bottom.
            decl.Members
            |> mergeNamespaceDecls com ctx
            |> List.map (fun decl ->
                let lazyDecl = lazy (transformDecl com ctx decl)

                match decl with
                | Fable.ModuleDeclaration _ -> () // delay module decl transform
                | _ -> lazyDecl.Force() |> ignore // transform other decls first

                lazyDecl
            )
            |> List.collect (fun lazyDecl -> lazyDecl.Force())

        if List.isEmpty memberDecls then
            [] // don't output empty modules
        else
            let ent = com.GetEntity(decl.Entity)

            if ent.IsNamespace then
                // add the namespace to a global list to be re-exported
                com.AddNamespace(com.CurrentFile, ent.FullName)

            let useDecls =
                let useItem = mkGlobUseItem [] [ "super" ]

                let importItems = com.GetAllImports(ctx) |> transformImports com ctx

                com.ClearAllImports(ctx)
                useItem :: importItems

            let outerAttrs = transformAttributes com ctx ent.Attributes decl.XmlDoc
            let innerAttrs = getInnerAttributes com ctx decl.Members
            let attrs = innerAttrs @ outerAttrs
            let modDecls = useDecls @ memberDecls
            let modItem = modDecls |> mkModItem attrs decl.Name
            let modItem = modItem |> entityItemWithVis com ctx ent
            [ modItem ]

    let transformMemberDecl (com: IRustCompiler) ctx (decl: Fable.MemberDecl) =
        let memb = com.GetMember(decl.MemberRef)

        let memberItems =
            // if memb.IsExtension && memb.IsInstance then
            //     transformExtensionMethod com ctx memb decl
            if memb.IsValue then
                transformModuleLetValue com ctx memb decl
            else
                transformModuleFunction com ctx memb decl

        memberItems

    let transformDecl (com: IRustCompiler) ctx decl =
        match decl with
        | Fable.ModuleDeclaration decl ->
            withCurrentScope ctx (Set.singleton decl.Name)
            <| fun ctx -> transformModuleDecl com ctx decl
        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames
            <| fun ctx -> transformModuleAction com ctx decl.Body
        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames
            <| fun ctx -> transformMemberDecl com ctx decl
        | Fable.ClassDeclaration decl -> transformClassDecl com ctx decl

    let transformDeclarations (com: IRustCompiler) ctx decls =
        let items =
            decls |> mergeNamespaceDecls com ctx |> List.collect (transformDecl com ctx)
        // wrap the last file in a module to consistently handle namespaces
        if isLastFileInProject com then
            let modPath = fixFileExtension com com.CurrentFile
            let modName = getImportModuleName com modPath
            let modItem = mkModItem [] modName items
            let useItem = mkGlobUseItem [] [ modName ]

            [ modItem; useItem |> mkPublicItem ]
        else
            items

    // F# hash function is unstable and gives different results in different runs
    // Taken from fable-library-ts/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
    let stableStringHash (s: string) =
        let mutable h = 5381

        for i = 0 to s.Length - 1 do
            h <- (h * 33) ^^^ (int s[i])

        h

    let isFableLibrary (com: IRustCompiler) =
        List.contains "FABLE_LIBRARY" com.Options.Define //TODO: look in project defines too

    let isFableLibraryPath (com: IRustCompiler) (path: string) =
        not (isFableLibrary com)
        && (path.StartsWith(com.LibraryDir, StringComparison.Ordinal)
            || path.Contains("fable-library-rust")
            || path = "fable_library_rust")

    let getImportModulePath (com: IRustCompiler) (path: string) =
        let isAbsolutePath =
            path.StartsWith("/", StringComparison.Ordinal)
            || path.StartsWith("\\", StringComparison.Ordinal)
            || path.IndexOf(":", StringComparison.Ordinal) = 1

        let modulePath =
            if isAbsolutePath || (isFableLibraryPath com path) then
                Path.normalizePath path
            else
                let currentDir = Path.GetDirectoryName(com.CurrentFile)
                Path.Combine(currentDir, path) |> Path.normalizeFullPath

        modulePath

    let getImportModuleName (com: IRustCompiler) (modulePath: string) =
        let relPath = Path.getRelativePath com.ProjectFile modulePath
        System.String.Format("module_{0:x}", stableStringHash relPath)

    let transformImports (com: IRustCompiler) ctx (imports: Import list) : Rust.Item list =
        imports
        |> List.groupBy (fun import -> import.ModulePath)
        |> List.sortBy (fun (modulePath, _) -> modulePath)
        |> List.collect (fun (_modulePath, moduleImports) ->
            moduleImports
            |> List.sortBy (fun import -> import.Selector)
            |> List.map (fun import ->
                let modPath =
                    if import.Path.Length = 0 then
                        [] // empty path, means direct import of the selector
                    else if isFableLibraryPath com import.Path then
                        [ "fable_library_rust" ]
                    else
                        [ "crate" ]

                match import.Selector with
                | ""
                | "*"
                | "default" ->
                    // let useItem = mkGlobUseItem [] modPath
                    // [useItem]
                    []
                | _ ->
                    let parts = splitNameParts import.Selector

                    let alias =
                        if List.last parts <> import.LocalIdent then
                            Some(import.LocalIdent)
                        else
                            None

                    let useItem = mkSimpleUseItem [] (modPath @ parts) alias
                    [ useItem ]
            )
            |> List.concat
        )

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        match selector with
        | ""
        | "*"
        | "default" -> Path.GetFileNameWithoutExtension(path)
        | _ -> splitNameParts selector |> List.last
        |> getUniqueNameInRootScope ctx

    let fixFileExtension (com: IRustCompiler) (path: string) =
        if path.EndsWith(".fs", StringComparison.Ordinal) then
            let fileExt = com.Options.FileExtension
            Path.ChangeExtension(path, fileExt)
        else
            path

module Compiler =
    open System.Collections.Generic
    open System.Collections.Concurrent
    open Util

    // global list of import modules and namespaces (across files)
    let importModules = ConcurrentDictionary<string, bool>()
    let importNamespaces = ConcurrentDictionary<string * string, bool>()

    // per file
    type RustCompiler(com: Fable.Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IRustCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member self.GetImportName(ctx, selector, path, r) =
                if selector = Fable.Naming.placeholder then
                    "`importMember` must be assigned to a variable" |> addError com [] r

                let isMacro = selector.EndsWith("!", StringComparison.Ordinal)
                let selector = selector |> Fable.Naming.replaceSuffix "!" ""
                let selector = selector.Replace(".", "::").Replace("`", "_")
                let path = fixFileExtension self path

                let cacheKey =
                    if (isFableLibraryPath self path) then
                        "fable_library_rust::" + selector
                    elif path.Length = 0 then
                        selector
                    else
                        path + "::" + selector

                let import =
                    match imports.TryGetValue(cacheKey) with
                    | true, import ->
                        if not (import.Depths |> List.contains ctx.ModuleDepth) then
                            import.Depths <- ctx.ModuleDepth :: import.Depths

                        import
                    | false, _ ->
                        let localIdent = getIdentForImport ctx path selector
                        let modulePath = getImportModulePath self path

                        let import =
                            {
                                Selector = selector
                                LocalIdent = localIdent
                                ModulePath = modulePath
                                Path = path
                                Depths = [ ctx.ModuleDepth ]
                            }
                        // add import module to a global list (across files)
                        if
                            path.Length > 0
                            && not (path = "fable_library_rust")
                            && not (path.Contains("fable-library-rust"))
                            && not (isFableLibraryPath self path)
                        then
                            importModules.TryAdd(modulePath, true) |> ignore

                        imports.Add(cacheKey, import)
                        import

                if isMacro then
                    $"{import.LocalIdent}!"
                else
                    $"{import.LocalIdent}"

            member _.GetAllImports(ctx) =
                imports.Values
                |> Seq.filter (fun import ->
                    // return only imports at the current module depth level
                    import.Depths |> List.forall (fun d -> d = ctx.ModuleDepth)
                )
                |> Seq.toList

            member _.ClearAllImports(ctx) =
                for import in imports do
                    import.Value.Depths <-
                        // remove all import depths at this module level or deeper
                        import.Value.Depths |> List.filter (fun d -> d < ctx.ModuleDepth)

                    if import.Value.Depths.Length = 0 then
                        imports.Remove(import.Key) |> ignore

                        ctx.UsedNames.RootScope.Remove(import.Value.LocalIdent) |> ignore

            member _.GetAllModules() = importModules.Keys |> Seq.toList

            member _.GetAllNamespaces() = importNamespaces.Keys |> Seq.toList

            member self.AddNamespace(path, entFullName) =
                let path = fixFileExtension self path
                importNamespaces.TryAdd((path, entFullName), true) |> ignore

            member com.TransformExpr(ctx, e) = transformExpr com ctx e

            member _.GetEntity(fullName) =
                match com.TryGetEntity(fullName) with
                | Some ent -> ent
                | None -> failwith $"Missing entity {fullName}"

        interface Fable.Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.ProjectOptions = com.ProjectOptions
            member _.SourceFiles = com.SourceFiles
            member _.IncrementCounter() = com.IncrementCounter()
            member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
            member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)

            member _.AddLog(msg, severity, ?range, ?fileName: string, ?tag: string) =
                com.AddLog(msg, severity, ?range = range, ?fileName = fileName, ?tag = tag)

    let makeCompiler com = RustCompiler(com)

    let transformFile (com: Fable.Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IRustCompiler

        let declScopes =
            let hs = HashSet()

            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)

            hs

        let ctx =
            {
                File = file
                UsedNames =
                    {
                        RootScope = HashSet file.UsedNamesInRootScope
                        DeclarationScopes = declScopes
                        CurrentDeclarationScope = HashSet []
                    }
                DecisionTargets = []
                TargetAssignment = None
                // HoistVars = fun _ -> false
                // OptimizeTailCall = fun () -> ()
                TailCallOpportunity = None
                ScopedEntityGenArgs = Set.empty
                ScopedMemberGenArgs = Set.empty
                ScopedSymbols = Map.empty
                // HasMultipleUses = false
                InferAnyType = false
                IsAssocMember = false
                IsLambda = false
                IsParamByRefPreferred = false
                RequiresSendSync = false
                ModuleDepth = 0
            }

        let topAttrs =
            [
                if isLastFileInProject com then
                    // adds "no_std" to crate if feature is enabled
                    mkInnerAttr "cfg_attr" [ "feature = \"no_std\""; "no_std" ]

                    // TODO: make some of those conditional on compiler options
                    mkInnerAttr "allow" [ "dead_code" ]
                    mkInnerAttr "allow" [ "non_camel_case_types" ]
                    mkInnerAttr "allow" [ "non_snake_case" ]
                    mkInnerAttr "allow" [ "non_upper_case_globals" ]
                    mkInnerAttr "allow" [ "unexpected_cfgs" ]
                    mkInnerAttr "allow" [ "unreachable_code" ]
                    mkInnerAttr "allow" [ "unused_attributes" ]
                    mkInnerAttr "allow" [ "unused_imports" ]
                    mkInnerAttr "allow" [ "unused_macros" ]
                    mkInnerAttr "allow" [ "unused_parens" ]
                    mkInnerAttr "allow" [ "unused_variables" ]
                    mkInnerAttr "allow" [ "unused_assignments" ]
                    mkInnerAttr "allow" [ "unused_unsafe" ]

            // these require nightly
            // mkInnerAttr "feature" ["stmt_expr_attributes"]
            // mkInnerAttr "feature" ["destructuring_assignment"]
            ]

        let entryPointItems = file.Declarations |> getEntryPointItems com ctx
        let importItems = com.GetAllImports(ctx) |> transformImports com ctx
        let declItems = file.Declarations |> transformDeclarations com ctx
        let modItems = getModuleItems com ctx // global module imports
        let nsItems = getNamespaceItems com ctx // global namespace imports

        let crateItems = importItems @ declItems @ modItems @ nsItems @ entryPointItems

        let innerAttrs = file.Declarations |> getInnerAttributes com ctx
        let crateAttrs = topAttrs @ innerAttrs
        let crate = mkCrate crateAttrs crateItems
        crate
