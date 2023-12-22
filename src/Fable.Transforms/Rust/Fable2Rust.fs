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

    abstract GetImportName:
        Context * selector: string * path: string * SourceLocation option ->
            string

    abstract TransformExpr: Context * Fable.Expr -> Rust.Expr
    abstract GetEntity: entRef: Fable.EntityRef -> Fable.Entity

// TODO: Centralise and find a home for this
module Helpers =
    module Map =
        let except excluded source =
            source
            |> Map.filter (fun key _v -> not (excluded |> Map.containsKey key))

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
                    trie.Children
                    |> Map.tryFind x
                    |> Option.defaultValue empty
                    |> add xs value

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
    //             //getDecisionTargetAndBindValues
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
            getSubExpressions expr
            |> List.sumBy (fun e -> countIdentUsage name e) // depth-first

        match expr with
        | Fable.IdentExpr ident when ident.Name = name -> subCount + 1 // count each ident with the same name
        | Fable.ForLoop _
        | Fable.WhileLoop _ -> subCount * 2 // usage in loops counts as multiple uses
        | Fable.DecisionTree _ when subCount > 1 -> subCount * 2 // usage in complex decision trees can vary
        | _ -> subCount + 0 // anything else is zero

module TypeInfo =

    let splitName (sep: string) (fullName: string) =
        let i = fullName.LastIndexOf(sep)

        if i < 0 then
            "", fullName
        else
            fullName.Substring(0, i), fullName.Substring(i + sep.Length)

    let splitLast (fullName: string) =
        let i = fullName.LastIndexOf(".")

        if i < 0 then
            fullName
        else
            fullName.Substring(i + 1)

    let makeFullNamePath fullName genArgsOpt =
        let parts = splitNameParts fullName
        mkGenericPath parts genArgsOpt

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
        [ ty ] |> makeImportType com ctx "Native" "Box"

    // TODO: emit Lazy or SyncLazy depending on threading.
    let makeLazyTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "Lazy"

    // TODO: emit MutCell or AtomicCell depending on threading.
    let makeMutTy com ctx (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> makeImportType com ctx "Native" "MutCell"

    let makeOptionTy (ty: Rust.Ty) : Rust.Ty =
        [ ty ] |> mkGenericTy [ rawIdent "Option" ]

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
            |> List.exists (fun uci ->
                uci.UnionCaseFields |> List.exists (fun fi -> fi.IsMutable)
            )
        else
            ent.FSharpFields |> List.exists (fun fi -> fi.IsMutable)

    let isEntityOfType
        (com: IRustCompiler)
        isTypeOf
        entNames
        (ent: Fable.Entity)
        =
        if Set.contains ent.FullName entNames then
            true // already checked, avoids circular checks
        else
            let entNames = Set.add ent.FullName entNames

            if ent.IsFSharpUnion then
                ent.UnionCases
                |> Seq.forall (fun uci ->
                    uci.UnionCaseFields
                    |> List.forall (fun field ->
                        isTypeOf com entNames field.FieldType
                    )
                )
            else
                ent.FSharpFields
                |> Seq.forall (fun fi -> isTypeOf com entNames fi.FieldType)

    let isTypeOfType (com: IRustCompiler) isTypeOf isEntityOf entNames typ =
        match typ with
        | Fable.Option(genArg, _) -> isTypeOf com entNames genArg
        | Fable.Array(genArg, _) -> isTypeOf com entNames genArg
        | Fable.List genArg -> isTypeOf com entNames genArg
        | Fable.Tuple(genArgs, _) -> List.forall (isTypeOf com entNames) genArgs
        | Fable.AnonymousRecordType(_, genArgs, _isStruct) ->
            List.forall (isTypeOf com entNames) genArgs
        | Replacements.Util.Builtin(Replacements.Util.FSharpSet genArg) ->
            isTypeOf com entNames genArg
        | Replacements.Util.Builtin(Replacements.Util.FSharpMap(k, v)) ->
            isTypeOf com entNames k && isTypeOf com entNames v
        | Fable.DeclaredType(entRef, _) ->
            let ent = com.GetEntity(entRef)
            isEntityOf com entNames ent
        | _ -> true

    let isPrintableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: Any unprintable types?
        | _ -> isTypeOfType com isPrintableType isPrintableEntity entNames typ

    let isPrintableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && (isEntityOfType com isPrintableType entNames ent)

    let isDefaultableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more undefaultable types?
        | Fable.LambdaType _
        | Fable.DelegateType _ -> false
        | _ ->
            isTypeOfType com isDefaultableType isDefaultableEntity entNames typ

    let isDefaultableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && not (ent.IsFSharpUnion) // deriving 'Default' on enums is experimental
        && (isEntityOfType com isDefaultableType entNames ent)

    let isHashableType (com: IRustCompiler) entNames typ =
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
        not (ent.IsInterface)
        && (FSharp2Fable.Util.hasStructuralEquality ent)
        && (isEntityOfType com isHashableType entNames ent)

    let isCopyableType (com: IRustCompiler) entNames typ =
        match typ with
        // TODO: more uncopyable types?
        | Fable.Any
        | Fable.Unit
        | Fable.Measure _
        | Fable.MetaType
        | Fable.LambdaType _
        | Fable.DelegateType _
        | Fable.GenericParam _
        | Fable.String
        | Fable.Regex -> false
        | Fable.Tuple(genArgs, isStruct) ->
            isStruct && (List.forall (isCopyableType com entNames) genArgs)
        | Fable.AnonymousRecordType(_, genArgs, isStruct) ->
            isStruct && (List.forall (isCopyableType com entNames) genArgs)
        | _ -> isTypeOfType com isCopyableType isCopyableEntity entNames typ

    let isCopyableEntity com entNames (ent: Fable.Entity) =
        not (ent.IsInterface)
        && ent.IsValueType
        && not (hasMutableFields com ent)
        && (isEntityOfType com isCopyableType entNames ent)

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
        not (ent.IsInterface)
        && (FSharp2Fable.Util.hasStructuralEquality ent)
        && (isEntityOfType com isEquatableType entNames ent)

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
        not (ent.IsInterface)
        && (FSharp2Fable.Util.hasStructuralComparison ent)
        && (isEntityOfType com isComparableType entNames ent)

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
        | t when isByRefOrAnyType com t -> None

        // already wrapped, no need to Rc-wrap
        | t when isWrappedType com t -> None

        // always not Rc-wrapped
        | Fable.Any
        | Fable.Unit
        | Fable.Measure _
        | Fable.MetaType
        | Fable.Boolean
        | Fable.Char
        | Fable.Number _ -> None

        // should be Rc-wrapped
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
        | Fable.Array _ -> true
        | Fable.Number(BigInt, _) -> true
        | Fable.Tuple(_, isStruct) -> true
        | Fable.AnonymousRecordType _ -> true
        | Fable.DeclaredType(entRef, _) -> true
        | Fable.GenericParam(name, isMeasure, _) ->
            not (isInferredGenericParam com ctx name isMeasure)
        | _ -> false

    let typeImplementsCopyTrait (com: IRustCompiler) ctx typ =
        match typ with
        | Fable.Number(BigInt, _) -> false
        | Fable.Unit
        | Fable.Boolean
        | Fable.Char
        | Fable.Number _ -> // all numbers except BigInt
            true
        | _ -> false

    let rec tryGetIdentName =
        function
        | Fable.IdentExpr ident -> ident.Name |> Some
        | Fable.Get(expr, Fable.OptionValue, _, _) -> tryGetIdentName expr
        | Fable.Get(expr, Fable.UnionField _, _, _) -> tryGetIdentName expr
        | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf, expr),
                          _,
                          _,
                          _) -> tryGetIdentName expr
        | _ -> None

    // let getIdentName expr =
    //     tryGetIdentName expr |> Option.defaultValue ""

    let transformImport
        (com: IRustCompiler)
        ctx
        r
        t
        (info: Fable.ImportInfo)
        genArgsOpt
        =
        if info.Selector.Contains("*") || info.Selector.Contains("{") then
            let importName = com.GetImportName(ctx, info.Selector, info.Path, r)
            mkUnitExpr () // just an import without a body
        else
            match info.Kind with
            | Fable.MemberImport membRef ->
                let memb = com.GetMember(membRef)

                if memb.IsInstance then
                    // no import needed (perhaps)
                    let importName = info.Selector //com.GetImportName(ctx, info.Selector, info.Path, r)
                    makeFullNamePathExpr importName genArgsOpt
                else
                    // for constructors or static members, import just the type
                    let selector, membName = splitName "." info.Selector

                    let importName =
                        com.GetImportName(ctx, selector, info.Path, r)

                    makeFullNamePathExpr
                        (importName + "::" + membName)
                        genArgsOpt
            | Fable.LibraryImport mi when
                not (mi.IsInstanceMember) && not (mi.IsModuleMember)
                ->
                // for static (non-module and non-instance) members, import just the type
                let selector, membName = splitName "::" info.Selector
                let importName = com.GetImportName(ctx, selector, info.Path, r)
                makeFullNamePathExpr (importName + "::" + membName) genArgsOpt
            | _ ->
                let importName =
                    com.GetImportName(ctx, info.Selector, info.Path, r)

                makeFullNamePathExpr importName genArgsOpt

    let makeLibCall
        com
        ctx
        genArgsOpt
        moduleName
        memberName
        (args: Rust.Expr list)
        =
        let importName = getLibraryImportName com ctx moduleName memberName
        let callee = makeFullNamePathExpr importName genArgsOpt
        mkCallExpr callee args

    let libCall
        com
        ctx
        r
        genArgs
        moduleName
        memberName
        (args: Fable.Expr list)
        =
        let genArgsOpt = transformGenArgs com ctx genArgs
        let args = Util.transformCallArgs com ctx args [] []
        makeLibCall com ctx genArgsOpt moduleName memberName args

    let transformGenTypes com ctx genArgs : Rust.Ty list =
        genArgs
        |> List.filter (isUnitOfMeasure >> not)
        |> List.map (transformType com ctx)

    let transformGenArgs com ctx genArgs : Rust.GenericArgs option =
        genArgs |> transformGenTypes com ctx |> mkGenericTypeArgs

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
        genArgs
        |> transformGenTypes com ctx
        |> mkGenericTy (splitNameParts typeName)

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

    let transformTupleType com ctx isStruct genArgs : Rust.Ty =
        genArgs |> List.map (transformType com ctx) |> mkTupleTy

    let transformOptionType com ctx genArg : Rust.Ty =
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
                let importPath =
                    Path.getRelativeFileOrDirPath
                        false
                        com.CurrentFile
                        false
                        path

                let importName =
                    com.GetImportName(ctx, entRef.FullName, importPath, None)

                importName
            else
                entRef.FullName
        | None ->
            match entRef.Path with
            | Fable.AssemblyPath _
            | Fable.CoreAssemblyName _ when not (Util.isFableLibrary com) ->
                //TODO: perhaps only import from library if it's already implemented BCL class
                let importName =
                    com.GetImportName(
                        ctx,
                        entRef.FullName,
                        "fable_library_rust",
                        None
                    )

                importName
            | _ when (Util.isFableLibrary com) -> "crate::" + entRef.FullName
            | _ -> entRef.FullName

    let declaredInterfaces =
        Set.ofList
            [
                Types.icollection
                Types.icollectionGeneric
                Types.idictionary
                Types.ireadonlydictionary
                Types.idisposable
                Types.iformattable
                Types.iformatProvider
                Types.icomparer
                Types.icomparerGeneric
                Types.iequalityComparer
                Types.iequalityComparerGeneric
                Types.ienumerable
                Types.ienumerableGeneric
                Types.ienumerator
                Types.ienumeratorGeneric
                Types.iequatableGeneric
                Types.icomparable
                Types.icomparableGeneric
                Types.iStructuralEquatable
                Types.iStructuralComparable
            ]

    let isDeclaredInterface fullName =
        Set.contains fullName declaredInterfaces

    let getInterfaceImportName
        (com: IRustCompiler)
        ctx
        (entRef: Fable.EntityRef)
        =
        if isDeclaredInterface entRef.FullName then
            getLibraryImportName com ctx "Interfaces" entRef.FullName
        else
            getEntityFullName com ctx entRef
    // let selector = "crate::" + entRef.FullName
    // let path = ""
    // com.GetImportName(ctx, selector, path, None)

    let tryFindInterface
        (com: IRustCompiler)
        fullName
        (entRef: Fable.EntityRef)
        : Fable.DeclaredType option
        =
        let ent = com.GetEntity(entRef)

        ent.AllInterfaces
        |> Seq.tryFind (fun ifc -> ifc.Entity.FullName = fullName)

    let transformInterfaceType
        (com: IRustCompiler)
        ctx
        (entRef: Fable.EntityRef)
        genArgs
        : Rust.Ty
        =
        let nameParts = getInterfaceImportName com ctx entRef |> splitNameParts
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound nameParts genArgsOpt
        mkDynTraitTy [ traitBound ]

    let getAbstractClassImportName
        (com: IRustCompiler)
        ctx
        (entRef: Fable.EntityRef)
        =
        match entRef.FullName with
        | "System.Text.Encoding" ->
            getLibraryImportName com ctx "Encoding" "Encoding"
        | _ -> getEntityFullName com ctx entRef

    let transformAbstractClassType
        (com: IRustCompiler)
        ctx
        (entRef: Fable.EntityRef)
        genArgs
        : Rust.Ty
        =
        let nameParts =
            getAbstractClassImportName com ctx entRef |> splitNameParts

        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound nameParts genArgsOpt
        mkDynTraitTy [ traitBound ]

    let (|HasEmitAttribute|_|) (ent: Fable.Entity) =
        ent.Attributes
        |> Seq.tryPick (fun att ->
            if
                att.Entity.FullName.StartsWith(
                    Atts.emit,
                    StringComparison.Ordinal
                )
            then
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
            if
                att.Entity.FullName.StartsWith(
                    Atts.referenceType,
                    StringComparison.Ordinal
                )
            then
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

            if
                ent.IsInterface
                && not (ent |> FSharp2Fable.Util.hasAttribute Atts.erase)
            then
                Some(entRef, genArgs)
            else
                None
        | _ -> None

    let transformDeclaredType
        (com: IRustCompiler)
        ctx
        entRef
        genArgs
        : Rust.Ty
        =
        match com.GetEntity(entRef) with
        | HasEmitAttribute value ->
            let genArgs = genArgs |> List.map (transformType com ctx)
            mkEmitTy value genArgs
        | ent when ent.IsInterface ->
            transformInterfaceType com ctx entRef genArgs
        | ent when ent.IsAbstractClass ->
            transformAbstractClassType com ctx entRef genArgs
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
        | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf, e), _, _, _) ->
            true
        | _ -> false

    let isByRefOrAnyType (com: IRustCompiler) =
        function
        | Replacements.Util.IsByRefType com _ -> true
        | Fable.Any -> true
        | _ -> false

    let isInRefOrAnyType (com: IRustCompiler) =
        function
        | Replacements.Util.IsInRefType com _ -> true
        | Fable.Any -> true
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

    let transformGenericParamType com ctx name isMeasure : Rust.Ty =
        if isInferredGenericParam com ctx name isMeasure then
            mkInferTy () // makeAnyTy com ctx
        else
            primitiveType name

    let transformMetaType com ctx : Rust.Ty =
        transformImportType com ctx [] "Native" "TypeId"

    let transformStringType com ctx : Rust.Ty =
        transformImportType com ctx [] "String" "string"

    let transformBuiltinType com ctx typ kind : Rust.Ty =
        match kind with
        | Replacements.Util.BclGuid -> transformGuidType com ctx
        | Replacements.Util.BclTimeSpan -> transformTimeSpanType com ctx
        | Replacements.Util.BclDateTime -> transformDateTimeType com ctx
        | Replacements.Util.BclDateTimeOffset ->
            transformDateTimeOffsetType com ctx
        | Replacements.Util.BclDateOnly -> transformDateOnlyType com ctx
        | Replacements.Util.BclTimeOnly -> transformTimeOnlyType com ctx
        | Replacements.Util.BclTimer -> transformTimerType com ctx
        | Replacements.Util.BclHashSet(genArg) ->
            transformHashSetType com ctx genArg
        | Replacements.Util.BclDictionary(k, v) ->
            transformHashMapType
                com
                ctx
                [
                    k
                    v
                ]
        | Replacements.Util.FSharpSet(genArg) -> transformSetType com ctx genArg
        | Replacements.Util.FSharpMap(k, v) ->
            transformMapType
                com
                ctx
                [
                    k
                    v
                ]
        | Replacements.Util.BclKeyValuePair(k, v) ->
            transformTupleType
                com
                ctx
                true
                [
                    k
                    v
                ]
        | Replacements.Util.FSharpResult(ok, err) ->
            transformResultType
                com
                ctx
                [
                    ok
                    err
                ]
        | Replacements.Util.FSharpChoice genArgs ->
            transformChoiceType com ctx genArgs
        | Replacements.Util.FSharpReference(genArg) ->
            if isInRefOrAnyType com typ then
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
            | Fable.DelegateType(argTypes, returnType) ->
                transformClosureType com ctx argTypes returnType
            | Fable.GenericParam(name, isMeasure, _constraints) ->
                transformGenericParamType com ctx name isMeasure
            | Fable.Tuple(genArgs, isStruct) ->
                transformTupleType com ctx isStruct genArgs
            | Fable.Option(genArg, _isStruct) ->
                transformOptionType com ctx genArg
            | Fable.Array(genArg, _kind) -> transformArrayType com ctx genArg
            | Fable.List genArg -> transformListType com ctx genArg
            | Fable.Regex -> transformRegexType com ctx
            | Fable.AnonymousRecordType(fieldNames, genArgs, isStruct) ->
                transformTupleType com ctx isStruct genArgs

            // interfaces implemented as the type itself
            | Replacements.Util.IsEntity (Types.iset) (entRef, [ genArg ]) ->
                transformHashSetType com ctx genArg
            | Replacements.Util.IsEntity (Types.idictionary) (entRef, [ k; v ]) ->
                transformHashMapType
                    com
                    ctx
                    [
                        k
                        v
                    ]
            | Replacements.Util.IsEntity (Types.ireadonlydictionary) (entRef,
                                                                      [ k; v ]) ->
                transformHashMapType
                    com
                    ctx
                    [
                        k
                        v
                    ]
            | Replacements.Util.IsEntity (Types.keyCollection) (entRef, [ k; v ]) ->
                transformArrayType com ctx k
            | Replacements.Util.IsEntity (Types.valueCollection) (entRef,
                                                                  [ k; v ]) ->
                transformArrayType com ctx v
            | Replacements.Util.IsEntity (Types.icollectionGeneric) (entRef,
                                                                     [ t ]) ->
                transformArrayType com ctx t

            // pre-defined declared types
            | Replacements.Util.IsEntity (Types.fsharpAsyncGeneric) (_, [ t ]) ->
                transformAsyncType com ctx t
            | Replacements.Util.IsEntity (Types.taskGeneric) (_, [ t ]) ->
                transformTaskType com ctx t
            | Replacements.Util.IsEntity (Types.taskBuilder) (_, []) ->
                transformTaskBuilderType com ctx
            | Replacements.Util.IsEntity (Types.taskBuilderModule) (_, []) ->
                transformTaskBuilderType com ctx
            | Replacements.Util.IsEntity (Types.thread) (_, []) ->
                transformThreadType com ctx

            // implemented regex types
            | Replacements.Util.IsEntity (Types.regexMatch) (_, []) ->
                transformImportType com ctx [] "RegExp" "Match"
            | Replacements.Util.IsEntity (Types.regexGroup) (_, []) ->
                transformImportType com ctx [] "RegExp" "Group"
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
                | Some ifc ->
                    transformInterfaceType com ctx ifc.Entity [ Fable.Any ]
                | _ ->
                    failwith
                        "Cannot find IEnumerator interface, should not happen."

            // built-in types
            | Replacements.Util.Builtin kind ->
                transformBuiltinType com ctx typ kind

            // other declared types
            | Fable.DeclaredType(entRef, genArgs) ->
                transformDeclaredType com ctx entRef genArgs

        let ty =
            match shouldBeRefCountWrapped com ctx typ with
            | Some Lrc -> ty |> makeLrcPtrTy com ctx
            | Some Rc -> ty |> makeRcTy com ctx
            | Some Arc -> ty |> makeArcTy com ctx
            | Some Box -> ty |> makeBoxTy com ctx
            | _ -> ty

        if
            not (typ = Fable.Any && ctx.InferAnyType)
            && (isByRefOrAnyType com typ || ctx.IsParamByRefPreferred)
        then
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

    let (|IEquatable|_|) =
        function
        | Replacements.Util.IsEntity (Types.iequatableGeneric) (_, [ genArg ]) ->
            Some(genArg)
        | _ -> None

    let (|IEnumerable|_|) =
        function
        | Replacements.Util.IsEntity (Types.ienumerableGeneric) (_, [ genArg ]) ->
            Some(genArg)
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
            | [ thisArg; arg ] when thisArg.IsThisArgument && isUnitArg arg ->
                [ thisArg ]
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

    type NamedTailCallOpportunity
        (_com: IRustCompiler, ctx, name, args: Fable.Ident list)
        =
        let args =
            args
            |> discardUnitArg []
            |> List.filter (fun arg -> not (arg.IsThisArgument))

        let label = splitLast name

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

    let transformIdent com ctx r (ident: Fable.Ident) =
        match ctx.ScopedSymbols |> Map.tryFind ident.Name with
        | Some varAttrs ->
            //ident has been seen, subtract 1
            varAttrs.UsageCount <- varAttrs.UsageCount - 1
        | None -> ()

        if
            ident.IsThisArgument && ctx.IsAssocMember // prevents emitting self on inlined code
        then
            makeThis com ctx r ident.Type
        else
            mkGenericPathExpr (splitNameParts ident.Name) None

    // let transformExprMaybeIdentExpr (com: IRustCompiler) ctx (expr: Fable.Expr) =
    //     match expr with
    //     | Fable.IdentExpr ident when ident.IsThisArgument ->
    //         // avoids the extra Lrc wrapping for self that transformIdentGet does
    //         transformIdent com ctx None id
    //     | _ -> com.TransformExpr(ctx, expr)

    let transformIdentGet com ctx r (ident: Fable.Ident) =
        let expr = transformIdent com ctx r ident

        if ident.IsMutable && not (isInRefOrAnyType com ident.Type) then
            expr |> mutableGet
        elif isBoxScoped ctx ident.Name then
            expr |> makeLrcPtrValue com ctx
        // elif isRefScoped ctx ident.Name then
        //     expr |> makeClone // |> mkDerefExpr |> mkParenExpr
        else
            expr

    let transformIdentSet com ctx r (ident: Fable.Ident) (value: Rust.Expr) =
        let expr = transformIdent com ctx r ident
        // assert(ident.IsMutable)
        mutableSet expr value

    let memberFromName (memberName: string) : Rust.Expr * bool =
        match memberName with
        | "ToString" -> (mkGenericPathExpr [ "ToString" ] None), false
        // | n when n.StartsWith("Symbol.") ->
        //     Expression.memberExpression(Expression.identifier("Symbol"), Expression.identifier(n[7..]), false), true
        // | n when Naming.hasIdentForbiddenChars n -> Expression.stringLiteral(n), true
        | n -> (mkGenericPathExpr [ n ] None), false

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

    let getNewGenArgsAndCtx
        (ctx: Context)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let rec getGenParams =
            function
            | Fable.GenericParam(name, isMeasure, _constraints) as t when
                not isMeasure
                ->
                [ name, t ]
            | t -> t.Generics |> List.collect getGenParams

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
                |> List.collect getGenParams
                |> List.distinctBy fst
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

    let optimizeTailCall
        com
        ctx
        r
        (tc: ITailCallOpportunity)
        (args: Fable.Expr list)
        : Rust.Expr
        =
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

        let tempLetStmts, ctx =
            makeLetStmts com ctx bindings emptyBody Map.empty

        let setArgStmts =
            List.zip tc.Args tempArgs
            |> List.map (fun (id, idTemp) ->
                let value = transformIdentGet com ctx r idTemp
                transformIdentSet com ctx r id value |> mkExprStmt
            )

        let continueStmt = mkContinueExpr (Some tc.Label) |> mkExprStmt
        tempLetStmts @ setArgStmts @ [ continueStmt ] |> mkStmtBlockExpr

    let transformInterfaceCast com ctx typ (expr: Rust.Expr) : Rust.Expr =
        match typ with
        | IsNonErasedInterface com (entRef, genArgs) ->
            let ifcTy =
                transformDeclaredType com ctx entRef genArgs
                |> makeCastTy com ctx

            let macroName =
                getLibraryImportName com ctx "Native" "interface_cast"

            [
                mkExprToken expr
                mkTyToken ifcTy
            ]
            |> mkParensCommaDelimitedMacCall macroName
            |> mkMacCallExpr
        | _ -> expr

    let transformCast
        (com: IRustCompiler)
        (ctx: Context)
        typ
        (fableExpr: Fable.Expr)
        : Rust.Expr
        =
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
        | Fable.Char, Fable.Number(UInt32, Fable.NumberInfo.Empty) ->
            expr |> mkCastExpr ty
        | Fable.Tuple(ga1, false), Fable.Tuple(ga2, true) when ga1 = ga2 ->
            expr |> makeAsRef |> makeClone //.ToValueTuple()
        | Fable.Tuple(ga1, true), Fable.Tuple(ga2, false) when ga1 = ga2 ->
            expr |> makeLrcPtrValue com ctx //.ToTuple()

        // casts to IEnumerable
        | Replacements.Util.IsEntity (Types.keyCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.valueCollection) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.icollectionGeneric) _, IEnumerable _
        | Fable.Array _, IEnumerable _ ->
            makeLibCall com ctx None "Seq" "ofArray" [ expr ]
        | Fable.List _, IEnumerable _ ->
            makeLibCall com ctx None "Seq" "ofList" [ expr ]
        | Fable.String, IEnumerable _ ->
            let chars = makeLibCall com ctx None "String" "toCharArray" [ expr ]
            makeLibCall com ctx None "Seq" "ofArray" [ chars ]
        | Replacements.Util.IsEntity (Types.hashset) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.iset) _, IEnumerable _ ->
            let ar = makeLibCall com ctx None "HashSet" "entries" [ expr ]
            makeLibCall com ctx None "Seq" "ofArray" [ ar ]
        | Replacements.Util.IsEntity (Types.dictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.idictionary) _, IEnumerable _
        | Replacements.Util.IsEntity (Types.ireadonlydictionary) _,
          IEnumerable _ ->
            let ar = makeLibCall com ctx None "HashMap" "entries" [ expr ]
            makeLibCall com ctx None "Seq" "ofArray" [ ar ]

        // casts to generic param
        | _, Fable.GenericParam(name, _isMeasure, _constraints) ->
            makeCall
                [
                    name
                    "from"
                ]
                None
                [ expr ] // e.g. T::from(value)

        // casts to IDictionary, for now does nothing // TODO: fix it
        | Replacements.Util.IsEntity (Types.dictionary) _,
          Replacements.Util.IsEntity (Types.idictionary) _ -> expr

        // casts from object to interface
        | t1, t2 when not (isInterface com t1) && (isInterface com t2) ->
            transformInterfaceCast com ctx t2 expr

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

    /// Calling this on an rc guarantees a &T, regardless of if the Rc is a ref or not
    let makeAsRef expr = mkMethodCallExpr "as_ref" None expr []

    let makeCall pathNames genArgs (args: Rust.Expr list) =
        let callee = mkGenericPathExpr pathNames genArgs
        mkCallExpr callee args

    let makeNew com ctx moduleName typeName (value: Rust.Expr) =
        let importName = getLibraryImportName com ctx moduleName typeName

        makeCall
            [
                importName
                "new"
            ]
            None
            [ value ]

    // let makeFrom com ctx moduleName typeName (value: Rust.Expr) =
    //     let importName = getLibraryImportName com ctx moduleName typeName
    //     makeCall [importName; "from"] None [value]

    let makeFluentValue com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "Native" "fromFluent" [ value ]

    let makeLrcPtrValue com ctx (value: Rust.Expr) =
        value |> makeNew com ctx "Native" "LrcPtr"

    // let makeLrcValue com ctx (value: Rust.Expr) =
    //     value |> makeNew com ctx "Native" "Lrc"

    let makeRcValue com ctx (value: Rust.Expr) =
        value |> makeNew com ctx "Native" "Rc"

    let makeArcValue com ctx (value: Rust.Expr) =
        value |> makeNew com ctx "Native" "Arc"

    let makeBoxValue com ctx (value: Rust.Expr) =
        value |> makeNew com ctx "Native" "Box"

    let makeMutValue com ctx (value: Rust.Expr) =
        value |> makeNew com ctx "Native" "MutCell"

    let makeLazyValue com ctx (value: Rust.Expr) =
        value |> makeNew com ctx "Native" "Lazy"

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

        makeCall
            [
                funcWrap
                "from"
            ]
            None
            [ expr ]

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
        |> Option.map (fun p ->
            p.Attributes
            |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef)
        )
        |> Option.defaultValue false

    let transformCallArgs
        (com: IRustCompiler)
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
                    args
                    |> List.zip argTypes
                    |> List.map (fun (t, a) -> Some t, a)
                else
                    args |> List.map (fun a -> None, a)

            argsWithTypes
            |> List.mapi (fun i (argType, arg) ->
                match arg with
                | Fable.IdentExpr ident when isFuncScoped ctx ident.Name ->
                    makeFuncValue com ctx ident // local nested function ident
                | _ ->
                    let isByRefPreferred =
                        parameterIsByRefPreferred i parameters

                    let ctx =
                        { ctx with
                            IsParamByRefPreferred =
                                isByRefPreferred || ctx.IsParamByRefPreferred
                        }

                    transformLeaveContext com ctx argType arg
            )

    let prepareRefForPatternMatch
        (com: IRustCompiler)
        ctx
        typ
        (name: string option)
        fableExpr
        =
        let expr = com.TransformExpr(ctx, fableExpr)

        if
            (name.IsSome && isRefScoped ctx name.Value)
            || (isInRefOrAnyType com typ)
        then
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

    let makeNumber com ctx r t kind (x: obj) =
        match kind, x with

        | Int8, (:? int8 as x) when x = System.SByte.MinValue ->
            mkGenericPathExpr ("i8" :: "MIN" :: []) None
        | Int8, (:? int8 as x) when x = System.SByte.MaxValue ->
            mkGenericPathExpr ("i8" :: "MAX" :: []) None
        | Int16, (:? int16 as x) when x = System.Int16.MinValue ->
            mkGenericPathExpr ("i16" :: "MIN" :: []) None
        | Int16, (:? int16 as x) when x = System.Int16.MaxValue ->
            mkGenericPathExpr ("i16" :: "MAX" :: []) None
        | Int32, (:? int32 as x) when x = System.Int32.MinValue ->
            mkGenericPathExpr ("i32" :: "MIN" :: []) None
        | Int32, (:? int32 as x) when x = System.Int32.MaxValue ->
            mkGenericPathExpr ("i32" :: "MAX" :: []) None
        | Int64, (:? int64 as x) when x = System.Int64.MinValue ->
            mkGenericPathExpr ("i64" :: "MIN" :: []) None
        | Int64, (:? int64 as x) when x = System.Int64.MaxValue ->
            mkGenericPathExpr ("i64" :: "MAX" :: []) None
        // | Int128, (:? System.Int128 as x) when x = System.Int128.MinValue ->
        //     mkGenericPathExpr ("i128"::"MIN"::[]) None
        // | Int128, (:? System.Int128 as x) when x = System.Int128.MaxValue ->
        //     mkGenericPathExpr ("i128"::"MAX"::[]) None

        // | UInt8, (:? uint8 as x) when x = System.Byte.MinValue ->
        //     mkGenericPathExpr ("u8"::"MIN"::[]) None
        | UInt8, (:? uint8 as x) when x = System.Byte.MaxValue ->
            mkGenericPathExpr ("u8" :: "MAX" :: []) None
        // | UInt16, (:? uint16 as x) when x = System.UInt16.MinValue ->
        //     mkGenericPathExpr ("u16"::"MIN"::[]) None
        | UInt16, (:? uint16 as x) when x = System.UInt16.MaxValue ->
            mkGenericPathExpr ("u16" :: "MAX" :: []) None
        // | UInt32, (:? uint32 as x) when x = System.UInt32.MinValue ->
        //     mkGenericPathExpr ("u32"::"MIN"::[]) None
        | UInt32, (:? uint32 as x) when x = System.UInt32.MaxValue ->
            mkGenericPathExpr ("u32" :: "MAX" :: []) None
        // | UInt64, (:? uint64 as x) when x = System.UInt64.MinValue ->
        //     mkGenericPathExpr ("u64"::"MIN"::[]) None
        | UInt64, (:? uint64 as x) when x = System.UInt64.MaxValue ->
            mkGenericPathExpr ("u64" :: "MAX" :: []) None
        // | UInt128, (:? System.UInt128 as x) when x = System.UInt128.MinValue ->
        //     mkGenericPathExpr ("u128"::"MIN"::[]) None
        // | UInt128, (:? System.UInt128 as x) when x = System.UInt128.MaxValue ->
        //     mkGenericPathExpr ("u128"::"MAX"::[]) None

        | Float32, (:? float32 as x) when System.Single.IsNaN(x) ->
            mkGenericPathExpr ("f32" :: "NAN" :: []) None
        | Float64, (:? float as x) when System.Double.IsNaN(x) ->
            mkGenericPathExpr ("f64" :: "NAN" :: []) None
        | Float32, (:? float32 as x) when System.Single.IsPositiveInfinity(x) ->
            mkGenericPathExpr ("f32" :: "INFINITY" :: []) None
        | Float64, (:? float as x) when System.Double.IsPositiveInfinity(x) ->
            mkGenericPathExpr ("f64" :: "INFINITY" :: []) None
        | Float32, (:? float32 as x) when System.Single.IsNegativeInfinity(x) ->
            mkGenericPathExpr ("f32" :: "NEG_INFINITY" :: []) None
        | Float64, (:? float as x) when System.Double.IsNegativeInfinity(x) ->
            mkGenericPathExpr ("f64" :: "NEG_INFINITY" :: []) None

        | NativeInt, (:? nativeint as x) ->
            let expr = mkIsizeLitExpr (abs x |> string<nativeint>)
            expr |> negateWhen (x < 0n)
        | Int8, (:? int8 as x) ->
            let expr = mkInt8LitExpr (abs x |> string<int8>)
            expr |> negateWhen (x < 0y)
        | Int16, (:? int16 as x) ->
            let expr = mkInt16LitExpr (abs x |> string<int16>)
            expr |> negateWhen (x < 0s)
        | Int32, (:? int32 as x) ->
            let expr = mkInt32LitExpr (abs x |> string<int32>)
            expr |> negateWhen (x < 0)
        | Int64, (:? int64 as x) ->
            let expr = mkInt64LitExpr (abs x |> string<int64>)
            expr |> negateWhen (x < 0L)
        | Int128, x -> // (:? System.Int128 as x) ->
            // let expr = mkInt128LitExpr (System.Int128.Abs(x) |> string)
            // expr |> negateWhen (System.Int128.IsNegative(x))
            let s = string<obj> x
            let expr = mkInt128LitExpr (s.TrimStart('-'))
            expr |> negateWhen (s.StartsWith("-", StringComparison.Ordinal))
        | UNativeInt, (:? unativeint as x) ->
            mkUsizeLitExpr (x |> string<unativeint>)
        | UInt8, (:? uint8 as x) -> mkUInt8LitExpr (x |> string<uint8>)
        | UInt16, (:? uint16 as x) -> mkUInt16LitExpr (x |> string<uint16>)
        | UInt32, (:? uint32 as x) -> mkUInt32LitExpr (x |> string<uint32>)
        | UInt64, (:? uint64 as x) -> mkUInt64LitExpr (x |> string<uint64>)
        | UInt128, x -> // (:? System.UInt128 as x) ->
            mkUInt128LitExpr (x |> string<obj>)
        | Float16, (:? float32 as x) ->
            let expr = mkFloat32LitExpr (abs x |> string<float32>)
            expr |> negateWhen (x < 0.0f)
        | Float32, (:? float32 as x) ->
            let expr = mkFloat32LitExpr (abs x |> string<float32>)
            expr |> negateWhen (x < 0.0f)
        | Float64, (:? float as x) ->
            let expr = mkFloat64LitExpr (abs x |> string<float>)
            expr |> negateWhen (x < 0.0)
        | Decimal, (:? decimal as x) ->
            Replacements.makeDecimal com r t x |> transformExpr com ctx
        | kind, x ->
            $"Expected literal of type %A{kind} but got {x.GetType().FullName}"
            |> addError com [] r

            mkFloat64LitExpr (string<float> 0.)

    let makeStaticString com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "String" "string" [ value ]

    let makeStringFrom com ctx (value: Rust.Expr) =
        makeLibCall com ctx None "String" "fromString" [ value ]

    let makeNull com ctx (typ: Fable.Type) =
        //TODO: some other representation perhaps?
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

            makeLibCall
                com
                ctx
                None
                "NativeArray"
                "new_init"
                [
                    value
                    count
                ]
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
            | Some(head, Fable.Value(Fable.NewList(tail, _), _)) ->
                getItems (head :: acc) tail
            | Some(head, tail) -> List.rev (head :: acc), Some tail

        let makeNewArray r typ exprs =
            Fable.Value(
                Fable.NewArray(Fable.ArrayValues exprs, typ, Fable.MutableArray),
                r
            )

        match getItems [] headAndTail with
        | [], None -> libCall com ctx r [ typ ] "List" "empty" []
        | [ expr ], None -> libCall com ctx r [] "List" "singleton" [ expr ]
        | exprs, None ->
            [ makeNewArray r typ exprs ]
            |> libCall com ctx r [] "List" "ofArray"
        | [ head ], Some tail ->
            libCall
                com
                ctx
                r
                []
                "List"
                "cons"
                [
                    head
                    tail
                ]
        | exprs, Some tail ->
            [
                makeNewArray r typ exprs
                tail
            ]
            |> libCall com ctx r [] "List" "ofArrayWithTail"

    let makeTuple
        (com: IRustCompiler)
        ctx
        r
        isStruct
        (exprs: (Fable.Expr) list)
        =
        let expr =
            exprs
            |> List.map (transformLeaveContext com ctx None)
            |> mkTupleExpr

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
            if
                fullName.StartsWith(
                    "FSharp.Core.FSharpChoice`",
                    StringComparison.Ordinal
                )
            then
                fullName
                |> Fable.Naming.replacePrefix "FSharp.Core.FSharp" ""
                |> Some
            else
                None

    let getUnionCaseName com ctx entRef (unionCase: Fable.UnionCase) =
        tryUseKnownUnionCaseNames unionCase.FullName
        |> Option.defaultWith (fun () ->
            let entName = getEntityFullName com ctx entRef
            entName + "::" + unionCase.Name
        )

    let makeUnion (com: IRustCompiler) ctx r values tag entRef genArgs =
        let ent = com.GetEntity(entRef)
        // let genArgsOpt = transformGenArgs com ctx genArgs
        let unionCase = ent.UnionCases |> List.item tag
        let unionCaseName = getUnionCaseName com ctx entRef unionCase
        let callee = makeFullNamePathExpr unionCaseName None //genArgsOpt

        let expr =
            if List.isEmpty values then
                callee
            else
                callFunction com ctx r callee values

        expr |> maybeWrapSmartPtr com ctx ent

    let makeThis (com: IRustCompiler) ctx r _typ =
        mkGenericPathExpr [ rawIdent "self" ] None

    let makeFormat (parts: string list) =
        let sb = System.Text.StringBuilder()
        sb.Append(List.head parts) |> ignore

        List.tail parts
        |> List.iteri (fun i part -> sb.Append($"{{{i}}}" + part) |> ignore)

        sb.ToString()

    let formatString (com: IRustCompiler) ctx fmt values : Rust.Expr =
        let args = transformCallArgs com ctx values [] []
        let fmtArgs = (mkStrLitExpr fmt) :: args
        makeLibCall com ctx None "String" "sprintf!" fmtArgs

    let makeStringTemplate (com: IRustCompiler) ctx parts values : Rust.Expr =
        let fmt = makeFormat parts
        formatString com ctx fmt values

    let makeTypeInfo (com: IRustCompiler) ctx r (typ: Fable.Type) : Rust.Expr =
        let importName = getLibraryImportName com ctx "Native" "TypeId"
        let genArgsOpt = transformGenArgs com ctx [ typ ]
        makeFullNamePathExpr importName genArgsOpt

    let transformValue (com: IRustCompiler) (ctx: Context) r value : Rust.Expr =
        let unimplemented () =
            $"Value %A{value} is not implemented yet" |> addWarning com [] None
            TODO_EXPR $"%A{value}"

        match value with
        | Fable.BaseValue(None, _) ->
            // Super(None)
            unimplemented ()
        | Fable.BaseValue(Some boundIdent, _) ->
            // identAsExpr boundIdent
            unimplemented ()
        | Fable.ThisValue typ -> makeThis com ctx r typ
        | Fable.TypeInfo(typ, _tags) -> makeTypeInfo com ctx r typ
        | Fable.Null typ -> makeNull com ctx typ
        | Fable.UnitConstant -> mkUnitExpr ()
        | Fable.BoolConstant b -> mkBoolLitExpr b //, ?loc=r)
        | Fable.CharConstant c -> mkCharLitExpr c //, ?loc=r)
        | Fable.StringConstant s -> mkStrLitExpr s |> makeStaticString com ctx
        | Fable.StringTemplate(_tag, parts, values) ->
            makeStringTemplate com ctx parts values
        | Fable.NumberConstant(x, kind, _) ->
            makeNumber com ctx r value.Type kind x
        | Fable.RegexConstant(source, flags) ->
            // Expression.regExpLiteral(source, flags, ?loc=r)
            unimplemented ()
        | Fable.NewArray(Fable.ArrayValues values, typ, _kind) ->
            makeArray com ctx r typ values
        | Fable.NewArray((Fable.ArrayFrom expr | Fable.ArrayAlloc expr),
                         typ,
                         _kind) -> makeArrayFrom com ctx r typ expr
        | Fable.NewTuple(values, isStruct) ->
            makeTuple com ctx r isStruct values
        | Fable.NewList(headAndTail, typ) -> makeList com ctx r typ headAndTail
        | Fable.NewOption(value, typ, isStruct) ->
            makeOption com ctx r typ value isStruct
        | Fable.NewRecord(values, entRef, genArgs) ->
            makeRecord com ctx r values entRef genArgs
        | Fable.NewAnonymousRecord(values, fieldNames, genArgs, isStruct) ->
            makeTuple com ctx r isStruct values
        | Fable.NewUnion(values, tag, entRef, genArgs) ->
            makeUnion com ctx r values tag entRef genArgs

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

    let transformLeaveContext
        (com: IRustCompiler)
        ctx
        (tOpt: Fable.Type option)
        (e: Fable.Expr)
        : Rust.Expr
        =
        // let varAttrs, isOnlyRef = calcVarAttrsAndOnlyRef com ctx e

        let implCopy = typeImplementsCopyTrait com ctx e.Type
        let implClone = typeImplementsCloneTrait com ctx e.Type

        let sourceIsRef =
            match e with
            | Fable.Get(Fable.IdentExpr ident, _, _, _)
            | MaybeCasted(Fable.IdentExpr ident) -> isRefScoped ctx ident.Name
            | _ -> false

        let targetIsRef =
            ctx.IsParamByRefPreferred
            || Option.exists (fun t -> t = Fable.Any) tOpt
        // || Option.exists (isByRefOrAnyType com) tOpt
        // || isAddrOfExpr e

        let mustClone =
            match e with
            | MaybeCasted(Fable.IdentExpr ident) ->
                // clone non-mutable idents if used more than once
                not (ident.IsMutable) && not (isUsedOnce ctx ident.Name)
            | Fable.Get(_, Fable.FieldGet _, _, _) -> true // always clone field get exprs
            // | Fable.Get(_, _, _, _) -> //TODO: clone other gets ???
            | _ -> false

        let isUnreachable =
            match e with
            | Fable.Emit _
            | Fable.Extended _ -> true
            | _ -> false

        // Careful moving this, as idents mutably subtract their count as they are seen, so ident transforming must happen AFTER checking
        let expr =
            //only valid for this level, so must reset for nested expressions
            let ctx = { ctx with IsParamByRefPreferred = false }
            com.TransformExpr(ctx, e)

        match
            implCopy,
            implClone,
            sourceIsRef,
            targetIsRef,
            mustClone,
            isUnreachable
        with
        | _, _, false, true, _, false -> expr |> mkAddrOfExpr
        | _, _, true, true, _, false -> expr
        | _, _, true, false, _, false -> expr |> makeClone
        | true, _, false, false, _, false -> expr
        | false, true, _, false, true, false -> expr |> makeClone
        | _ -> expr
    //|> BLOCK_COMMENT_SUFFIX (sprintf implCopy: %b, "implClone: %b, sourceIsRef; %b, targetIsRef: %b, isOnlyRef: %b (%i), isUnreachable: %b" implCopy implClone sourceIsRef targetIsRef isOnlyRef isUnreachable varAttrs.UsageCount)

    (*
    let enumerator2iterator com ctx =
        let enumerator = Expression.callExpression(get None (Expression.identifier("this")) "GetEnumerator", [||])
        BlockStatement([| Statement.returnStatement(libCall com ctx None [] "Util" "toIterator" [|enumerator|])|])

    let extractBaseExprFromBaseCall (com: IRustCompiler) (ctx: Context) (baseType: Fable.DeclaredType option) baseCall =
        match baseCall, baseType with
        | Some(Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr =
                match baseRef with
                | Fable.IdentExpr ident -> typedIdent com ctx ident |> Expression.Identifier
                | _ -> transformExpr com ctx baseRef
            let args = transformCallArgs com ctx info.Args
            Some(baseExpr, args)
        | Some(Fable.Value _), Some baseType ->
            // let baseEnt = com.GetEntity(baseType.Entity)
            // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
            // let entityType = FSharp2Fable.Util.getEntityType baseEnt
            // let baseRefId = makeTypedIdent entityType entityName
            // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
            // Some(baseExpr, []) // default base constructor
            let range = baseCall |> Option.bind (fun x -> x.Range)
            $"Ignoring base call for %s{baseType.Entity.FullName}" |> addWarning com [] range
            None
        | Some _, _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)
            "Unexpected base call expression, please report" |> addError com [] range
            None
        | None, _ ->
            None
*)
    let transformObjectExpr
        (com: IRustCompiler)
        ctx
        typ
        (members: Fable.ObjectExprMember list)
        baseCall
        : Rust.Expr
        =
        if members |> List.isEmpty then
            mkUnitExpr () // object constructors sometimes generate this
        else
            // TODO: add captured idents to object expression struct
            let makeEntRef fullName assemblyName : Fable.EntityRef =
                {
                    FullName = fullName
                    Path = Fable.CoreAssemblyName assemblyName
                }

            let entRef, genArgs =
                match typ with
                | Fable.DeclaredType(entRef, genArgs) -> entRef, genArgs
                | Fable.Any -> makeEntRef "System.Object" "System.Runtime", []
                | _ ->
                    "Unsupported object expression" |> addWarning com [] None
                    makeEntRef "System.Object" "System.Runtime", []
            //TODO: properly handle non-interface types with constructors
            let entName = "ObjectExpr"

            let members: Fable.MemberDecl list =
                members
                |> List.map (fun memb ->
                    {
                        Name = memb.Name
                        Args = memb.Args
                        Body = memb.Body
                        MemberRef = memb.MemberRef
                        IsMangled = memb.IsMangled
                        ImplementedSignatureRef = None
                        UsedNames = Set.empty
                        XmlDoc = None
                        Tags = []
                    }
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

            let attrs = []
            let fields = []
            let generics = makeGenerics com ctx genArgs

            let structItems =
                if baseCall.IsSome then
                    [] // if base type is not an interface
                else
                    [ mkStructItem attrs entName fields generics ]

            let memberItems = transformClassMembers com ctx decl
            let genArgsOpt = transformGenArgs com ctx genArgs
            let path = makeFullNamePath entName genArgsOpt

            let objExpr =
                match baseCall with
                | Some fableExpr -> com.TransformExpr(ctx, fableExpr)
                | None ->
                    let expr =
                        mkStructExpr path fields |> makeLrcPtrValue com ctx

                    transformInterfaceCast com ctx typ expr

            let objStmt = objExpr |> mkExprStmt
            let declStmts = structItems @ memberItems |> List.map mkItemStmt
            declStmts @ [ objStmt ] |> mkBlock |> mkBlockExpr

    let maybeAddParens fableExpr (expr: Rust.Expr) : Rust.Expr =
        match fableExpr with
        | Fable.IfThenElse _ -> mkParenExpr expr
        // TODO: add more expressions that need parens
        | _ -> expr

    let transformOperation com ctx range typ opKind : Rust.Expr =
        match opKind with
        | Fable.Unary(UnaryOperator.UnaryAddressOf,
                      MaybeCasted(Fable.IdentExpr ident)) ->
            transformIdent com ctx range ident |> mkAddrOfExpr
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            match op with
            | UnaryOperator.UnaryMinus ->
                match typ with
                | Fable.Number((Int8 | Int16 | Int32 | Int64 | Int128 | NativeInt),
                               _) ->
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
                | BinaryOperator.BinaryShiftRightSignPropagating ->
                    Rust.BinOpKind.Shr
                | BinaryOperator.BinaryShiftRightZeroFill -> Rust.BinOpKind.Shr
                | BinaryOperator.BinaryMinus -> Rust.BinOpKind.Sub
                | BinaryOperator.BinaryPlus -> Rust.BinOpKind.Add
                | BinaryOperator.BinaryMultiply -> Rust.BinOpKind.Mul
                | BinaryOperator.BinaryDivide -> Rust.BinOpKind.Div
                | BinaryOperator.BinaryModulus -> Rust.BinOpKind.Rem
                | BinaryOperator.BinaryExponent ->
                    failwithf
                        "BinaryExponent not supported. TODO: implement with pow."
                | BinaryOperator.BinaryOrBitwise -> Rust.BinOpKind.BitOr
                | BinaryOperator.BinaryXorBitwise -> Rust.BinOpKind.BitXor
                | BinaryOperator.BinaryAndBitwise -> Rust.BinOpKind.BitAnd

            let left =
                transformLeaveContext com ctx None leftExpr
                |> maybeAddParens leftExpr

            let right =
                transformLeaveContext com ctx None rightExpr
                |> maybeAddParens rightExpr

            match leftExpr.Type, kind with
            | Fable.String, Rust.BinOpKind.Add ->
                makeLibCall
                    com
                    ctx
                    None
                    "String"
                    "append"
                    [
                        left
                        right
                    ]
            | _ -> mkBinaryExpr (mkBinOp kind) left right // ?loc=range)

        | Fable.Logical(op,
                        TransformExpr com ctx left,
                        TransformExpr com ctx right) ->
            let kind =
                match op with
                | LogicalOperator.LogicalOr -> Rust.BinOpKind.Or
                | LogicalOperator.LogicalAnd -> Rust.BinOpKind.And

            mkBinaryExpr (mkBinOp kind) left right // ?loc=range)

    let transformMacro
        (com: IRustCompiler)
        ctx
        range
        (emitInfo: Fable.EmitInfo)
        =
        let info = emitInfo.CallInfo
        let macro = emitInfo.Macro |> Fable.Naming.replaceSuffix "!" ""
        let args = transformCallArgs com ctx info.Args info.SignatureArgTypes []

        let args =
            // for certain macros, use unwrapped format string as first argument
            match macro with
            | "print"
            | "println"
            | "format" ->
                match info.Args with
                | [ arg ] -> (mkStrLitExpr "{0}") :: args
                | Fable.Value(Fable.StringConstant formatStr, _) :: restArgs ->
                    (mkStrLitExpr formatStr) :: (List.tail args)
                | _ -> args
            | _ -> args

        let expr = mkMacroExpr macro args

        if macro = "format" then
            expr |> makeStringFrom com ctx
        else
            expr

    let transformEmit
        (com: IRustCompiler)
        ctx
        range
        (emitInfo: Fable.EmitInfo)
        =
        // for now only supports macro calls or function calls
        let info = emitInfo.CallInfo
        let macro = emitInfo.Macro
        // if it ends with '!', it's a Rust macro
        if macro.EndsWith("!", StringComparison.Ordinal) then
            transformMacro com ctx range emitInfo
        else // otherwise it's an Emit
            let thisArg =
                info.ThisArg
                |> Option.map (fun e -> com.TransformExpr(ctx, e))
                |> Option.toList

            let args =
                transformCallArgs com ctx info.Args info.SignatureArgTypes []

            let args = args |> List.append thisArg
            //TODO: create custom macro emit! (instead of a custom AST expression)
            mkEmitExpr macro args

    let transformCallee (com: IRustCompiler) ctx calleeExpr =
        match calleeExpr with
        | Fable.IdentExpr ident -> transformIdent com ctx None ident
        | Fable.Value(Fable.ThisValue _, _) -> transformExpr com ctx calleeExpr
        | _ ->
            let expr = transformExpr com ctx calleeExpr
            expr |> mkParenExpr // if not an identifier, wrap it in parentheses

    let isDeclEntityKindOf
        (com: IRustCompiler)
        isKindOf
        (callInfo: Fable.CallInfo)
        =
        callInfo.MemberRef
        |> Option.bind com.TryGetMember
        |> Option.bind (fun mi -> mi.DeclaringEntity)
        |> Option.bind com.TryGetEntity
        |> Option.map isKindOf
        |> Option.defaultValue false

    let isModuleMember (com: IRustCompiler) (callInfo: Fable.CallInfo) =
        isDeclEntityKindOf com (fun ent -> ent.IsFSharpModule) callInfo

    let transformCall
        (com: IRustCompiler)
        ctx
        range
        (typ: Fable.Type)
        calleeExpr
        (callInfo: Fable.CallInfo)
        =
        let isByRefPreferred =
            callInfo.MemberRef
            |> Option.bind com.TryGetMember
            |> Option.map (fun memberInfo ->
                memberInfo.Attributes
                |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef)
            )
            |> Option.defaultValue false

        let argParams =
            callInfo.MemberRef
            |> Option.bind com.TryGetMember
            |> Option.map (fun memberInfo ->
                memberInfo.CurriedParameterGroups |> List.concat
            )
            |> Option.defaultValue []

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

        let args =
            FSharp2Fable.Util.dropUnitCallArg
                callInfo.Args
                callInfo.SignatureArgTypes

        let args =
            transformCallArgs com ctx args callInfo.SignatureArgTypes argParams

        match calleeExpr with
        // mutable module values (transformed as function calls)
        | Fable.IdentExpr ident when
            ident.IsMutable && isModuleMember com callInfo
            ->
            let expr = transformIdent com ctx range ident
            mutableGet (mkCallExpr expr [])

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
                Set.ofList
                    [
                        "Native_::defaultOf"
                        "Native_::getZero"
                        "NativeArray_::new_empty"
                        "NativeArray_::new_with_capacity"
                        "HashSet_::new_empty"
                        "HashSet_::new_with_capacity"
                        "HashMap_::new_empty"
                        "HashMap_::new_with_capacity"
                        "Set_::empty"
                        "Map_::empty"
                        "Seq_::empty"
                    ]

            let genArgsOpt =
                if needGenArgs |> Set.contains info.Selector then
                    match typ with
                    | Fable.Tuple _ -> transformGenArgs com ctx [ typ ]
                    | _ -> transformGenArgs com ctx typ.Generics // callInfo.GenericArgs
                else
                    None

            match callInfo.ThisArg, info.Kind with
            | Some thisArg, Fable.MemberImport membRef ->
                let memb = com.GetMember(membRef)

                if memb.IsInstance then
                    let callee = transformCallee com ctx thisArg
                    mkMethodCallExpr info.Selector None callee args
                else
                    let callee = transformImport com ctx r t info None
                    mkCallExpr callee args
            | None, Fable.LibraryImport _ ->
                let callee = transformImport com ctx r t info genArgsOpt
                mkCallExpr callee args
            | _ ->
                let callee = transformImport com ctx r t info None
                mkCallExpr callee args

        | _ ->
            match ctx.TailCallOpportunity with
            | Some tc when
                tc.IsRecursiveRef(calleeExpr)
                && List.length tc.Args = List.length callInfo.Args
                ->
                optimizeTailCall com ctx range tc callInfo.Args
            | _ ->
                match callInfo.ThisArg, calleeExpr with
                | Some thisArg, Fable.IdentExpr ident ->
                    let callee = transformCallee com ctx thisArg
                    mkMethodCallExpr ident.Name None callee args
                // | None, Fable.IdentExpr ident ->
                //     let callee = makeFullNamePathExpr ident.Name None
                //     mkCallExpr callee args
                | _ ->
                    let callee = transformCallee com ctx calleeExpr
                    mkCallExpr callee args

    let mutableGet expr = mkMethodCallExpr "get" None expr []

    let mutableGetMut expr = mkMethodCallExpr "get_mut" None expr []

    let mutableSet expr value =
        mkMethodCallExpr "set" None expr [ value ]

    let makeInstanceCall com ctx memberName calleeExpr args =
        let membName = splitLast memberName
        let callee = com.TransformExpr(ctx, calleeExpr)

        match calleeExpr.Type with
        | IsNonErasedInterface com (entRef, genArgs) ->
            // interface instance call (using fully qualified syntax)
            let ifcName = getInterfaceImportName com ctx entRef
            let parts = (ifcName + "::" + membName) |> splitNameParts
            (callee |> makeAsRef) :: args |> makeCall parts None
        | _ ->
            // normal instance call
            mkMethodCallExpr membName None callee args

    let transformGet
        (com: IRustCompiler)
        ctx
        range
        typ
        (fableExpr: Fable.Expr)
        kind
        =
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
            let fieldName = info.Name

            match fableExpr.Type with
            | Fable.AnonymousRecordType(fields, _genArgs, isStruct) ->
                // anonimous records are tuples
                let idx = fields |> Array.findIndex (fun f -> f = fieldName)

                (Fable.TupleIndex(idx))
                |> transformGet com ctx range typ fableExpr
            | t when isInterface com t ->
                // for interfaces, transpile property_get as instance call
                makeInstanceCall com ctx info.Name fableExpr []
            | _ ->
                let expr = transformCallee com ctx fableExpr
                let field = getField range expr fieldName

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
            | _ -> libCall com ctx range [] "Option" "getValue" [ fableExpr ]

        | Fable.UnionTag ->
            let expr = com.TransformExpr(ctx, fableExpr)
            // TODO: range
            expr

        | Fable.UnionField info ->
            match fableExpr with
            | Fable.IdentExpr ident when isArmScoped ctx ident.Name ->
                // if arm scoped, just output the ident value
                let name = $"{ident.Name}_{info.CaseIndex}_{info.FieldIndex}"
                mkGenericPathExpr [ name ] None
            | _ ->
                // compile as: "if let MyUnion::Case(x, _) = opt { x } else { unreachable!() }"
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

                let unionCaseName =
                    getUnionCaseName com ctx info.Entity unionCase

                let pat = makeUnionCasePat unionCaseName fields

                let expr =
                    fableExpr
                    |> prepareRefForPatternMatch com ctx fableExpr.Type None

                let thenExpr = mkGenericPathExpr [ fieldName ] None |> makeClone

                let arms = [ mkArm [] pat None thenExpr ]

                let arms =
                    if (List.length ent.UnionCases) > 1 then
                        // only add a default arm if needed
                        let defaultArm =
                            mkArm
                                []
                                WILD_PAT
                                None
                                (mkMacroExpr "unreachable" [])

                        arms @ [ defaultArm ]
                    else
                        arms

                mkMatchExpr expr arms
    // TODO : Cannot use if let because it moves references out of their Rc's, which breaks borrow checker. We cannot bind
    // let ifExpr = mkLetExpr pat expr
    // let thenExpr = mkGenericPathExpr [fieldName] None
    // let elseExpr = mkMacroExpr "unreachable" []
    // mkIfThenElseExpr ifExpr thenExpr elseExpr

    let transformSet
        (com: IRustCompiler)
        ctx
        range
        fableExpr
        typ
        (fableValue: Fable.Expr)
        kind
        =
        let expr = transformCallee com ctx fableExpr
        let value = transformLeaveContext com ctx None fableValue

        match kind with
        | Fable.ValueSet ->
            match fableExpr with
            // mutable values
            | Fable.IdentExpr ident when ident.IsMutable ->
                transformIdentSet com ctx range ident value
            // mutable module values (transformed as function calls)
            | Fable.Call(Fable.IdentExpr ident, info, _, _) when
                ident.IsMutable && isModuleMember com info
                ->
                let expr = transformIdent com ctx range ident
                mutableSet (mkCallExpr expr []) value
            | _ ->
                match fableExpr.Type with
                | Replacements.Util.Builtin(Replacements.Util.FSharpReference _) ->
                    mutableSet expr value
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
            | t when isInterface com t ->
                // for interfaces, transpile property_set as instance call
                makeInstanceCall com ctx fieldName fableExpr [ value ]
            | _ ->
                let field = getField None expr fieldName
                mutableSet field value

    let transformAsStmt (com: IRustCompiler) ctx (e: Fable.Expr) : Rust.Stmt =
        let expr = transformLeaveContext com ctx None e
        mkExprStmt expr

    // flatten nested Let binding expressions
    let rec flattenLet acc (expr: Fable.Expr) =
        match expr with
        | Fable.Let(ident, value, body) ->
            flattenLet ((ident, value) :: acc) body
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

    let makeLocalStmt com ctx (ident: Fable.Ident) tyOpt initOpt isRef usages =
        let local = mkIdentLocal [] ident.Name tyOpt initOpt

        let scopedVarAttrs =
            {
                IsArm = false
                IsRef = isRef
                IsBox = false
                IsFunc = false
                UsageCount = usageCount ident.Name usages
            }

        let scopedSymbols =
            ctx.ScopedSymbols |> Map.add ident.Name scopedVarAttrs

        let ctxNext = { ctx with ScopedSymbols = scopedSymbols }
        mkLocalStmt local, ctxNext

    let makeLetStmt com ctx (ident: Fable.Ident) value isCaptured usages =
        // TODO: traverse body and follow references to decide if this should be wrapped or not
        // For Box/Rc it's not needed cause the Rust compiler will optimize the allocation away
        let tyOpt =
            match value with
            | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf,
                                          Fable.IdentExpr ident2),
                              _,
                              _,
                              _) when
                isByRefOrAnyType com ident2.Type || ident2.IsMutable
                ->
                None
            | _ ->
                if
                    isException com ident.Type || hasFuncOrAnyType ident.Type
                then
                    None
                else
                    let ctx = { ctx with InferAnyType = true }
                    transformType com ctx ident.Type |> Some

        let tyOpt =
            tyOpt
            |> Option.map (fun ty ->
                if isByRefOrAnyType com ident.Type then
                    ty // already wrapped
                elif ident.IsMutable && isCaptured then
                    ty |> makeMutTy com ctx |> makeLrcPtrTy com ctx
                elif ident.IsMutable then
                    ty |> makeMutTy com ctx
                else
                    ty
            )

        let initOpt =
            match value with
            | Fable.Operation(Fable.Unary(UnaryOperator.UnaryAddressOf,
                                          Fable.IdentExpr ident2),
                              _,
                              _,
                              _) when
                isByRefOrAnyType com ident2.Type || ident2.IsMutable
                ->
                transformIdent com ctx None ident2 |> Some
            | Fable.Value(Fable.Null _t, _) -> None // no init value, just a name declaration, to be initialized later
            | Function(args, body, _name) ->
                transformLambda com ctx (Some ident.Name) args body |> Some
            | _ ->
                transformLeaveContext com ctx None value
                // |> BLOCK_COMMENT_SUFFIX (sprintf "usages - %i" (usageCount ident.Name usages))
                |> Some

        let initOpt =
            initOpt
            |> Option.map (fun init ->
                if isByRefOrAnyType com ident.Type then
                    init // already wrapped
                elif ident.IsMutable && isCaptured then
                    init |> makeMutValue com ctx |> makeLrcPtrValue com ctx
                elif ident.IsMutable then
                    init |> makeMutValue com ctx
                else
                    init
            )

        let isRef = isAddrOfExpr value
        makeLocalStmt com ctx ident tyOpt initOpt isRef usages

    let makeLetStmts (com: IRustCompiler) ctx bindings letBody usages =
        // Context will be threaded through all let bindings, appending itself to ScopedSymbols each time
        let ctx, letStmtsRev =
            ((ctx, []), bindings)
            ||> List.fold (fun (ctx, lst) (ident: Fable.Ident, value) ->
                let stmt, ctxNext =
                    let isCaptured =
                        (bindings
                         |> List.exists (fun (_i, v) ->
                             FableTransforms.isIdentCaptured ident.Name v
                         ))
                        || (FableTransforms.isIdentCaptured ident.Name letBody)

                    match value with
                    | Function(args, body, _name) when not (ident.IsMutable) ->
                        if hasCapturedIdents com ctx ident.Name args body then
                            makeLetStmt com ctx ident value isCaptured usages
                        else
                            transformNestedFunction
                                com
                                ctx
                                ident
                                args
                                body
                                usages
                    | _ -> makeLetStmt com ctx ident value isCaptured usages

                (ctxNext, stmt :: lst)
            )

        letStmtsRev |> List.rev, ctx

    let transformLet
        (com: IRustCompiler)
        ctx
        (bindings: (Fable.Ident * Fable.Expr) list)
        body
        =
        // let usages =
        //     let bodyUsages = calcIdentUsages body
        //     let bindingsUsages = bindings |> List.map (snd >> calcIdentUsages)
        //     (Map.empty, bodyUsages::bindingsUsages)
        //     ||> List.fold (Helpers.Map.mergeAndAggregate (+))
        let usages =
            let idents, values = List.unzip bindings
            let exprs = body :: values

            let usageCounts =
                idents
                |> List.map (fun ident ->
                    let count =
                        exprs
                        |> List.map (fun e -> countIdentUsage ident.Name e)
                        |> List.sum

                    ident.Name, count
                )

            usageCounts |> Map

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

    let transformIfThenElse
        (com: IRustCompiler)
        ctx
        range
        guard
        thenBody
        elseBody
        =
        let guardExpr =
            match guard with
            | Fable.Test(expr, Fable.TypeTest typ, r) ->
                transformTypeTest com ctx r true typ expr
            | _ -> transformExpr com ctx guard

        let thenExpr = transformLeaveContext com ctx None thenBody

        match elseBody with
        | Fable.Value(Fable.UnitConstant, _) -> mkIfThenExpr guardExpr thenExpr // ?loc=range)
        | _ ->
            let elseExpr = transformLeaveContext com ctx None elseBody
            mkIfThenElseExpr guardExpr thenExpr elseExpr // ?loc=range)

    let transformWhileLoop (com: IRustCompiler) ctx range guard body =
        let guardExpr = transformExpr com ctx guard
        let bodyExpr = com.TransformExpr(ctx, body)
        mkWhileExpr None guardExpr bodyExpr // ?loc=range)

    let transformForLoop
        (com: IRustCompiler)
        ctx
        range
        isUp
        (var: Fable.Ident)
        start
        limit
        body
        =
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
                let rangeExpr =
                    mkRangeExpr (Some limitExpr) (Some startExpr) true
                    |> mkParenExpr

                mkMethodCallExpr "rev" None rangeExpr []

        mkForLoopExpr None varPat rangeExpr bodyExpr // ?loc=range)

    let makeLocalLambda com ctx (args: Fable.Ident list) (body: Fable.Expr) =
        let args = args |> discardUnitArg []
        let fnDecl = transformFunctionDecl com ctx args [] Fable.Unit
        let fnBody = transformExpr com ctx body
        mkClosureExpr false fnDecl fnBody

    let transformTryCatch
        (com: IRustCompiler)
        ctx
        range
        body
        catch
        finalizer
        : Rust.Expr
        =
        // try...with
        match catch with
        | Some(catchVar, catchBody) ->
            // try...with statements cannot be tail call optimized
            let ctx = { ctx with TailCallOpportunity = None }
            let try_f = makeLocalLambda com ctx [] body
            let catch_f = makeLocalLambda com ctx [ catchVar ] catchBody

            makeLibCall
                com
                ctx
                None
                "Exception"
                "try_catch"
                [
                    try_f
                    catch_f
                ]

        | None ->
            // try...finally
            match finalizer with
            | Some finBody ->
                let f = makeLocalLambda com ctx [] finBody

                let finAlloc =
                    makeLibCall com ctx None "Exception" "finally" [ f ]

                let bodyExpr = transformExpr com ctx body

                [
                    finAlloc |> mkSemiStmt
                    bodyExpr |> mkExprStmt
                ]
                |> mkStmtBlockExpr

            | _ ->
                // no catch, no finalizer
                transformExpr com ctx body

    let transformThrow
        (com: IRustCompiler)
        (ctx: Context)
        typ
        (exprOpt: Fable.Expr option)
        : Rust.Expr
        =
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

            mkMacroExpr
                "panic"
                [
                    mkStrLitExpr "{}"
                    msg
                ]

    let transformCurry
        (com: IRustCompiler)
        (ctx: Context)
        arity
        (expr: Fable.Expr)
        : Rust.Expr
        =
        com.TransformExpr(
            ctx,
            Replacements.Api.curryExprAtRuntime com arity expr
        )

    let transformCurriedApply (com: IRustCompiler) ctx r typ calleeExpr args =
        match ctx.TailCallOpportunity with
        | Some tc when
            tc.IsRecursiveRef(calleeExpr)
            && List.length tc.Args = List.length args
            ->
            optimizeTailCall com ctx r tc args
        | _ ->
            let callee = transformCallee com ctx calleeExpr

            (callee, args)
            ||> List.fold (fun expr arg ->
                let args = FSharp2Fable.Util.dropUnitCallArg [ arg ] []
                callFunction com ctx r expr args
            )

    let makeUnionCasePat unionCaseName fields =
        if List.isEmpty fields then
            makeFullNameIdentPat unionCaseName
        else
            let path = makeFullNamePath unionCaseName None
            mkTupleStructPat path fields

    let transformTypeTest
        (com: IRustCompiler)
        ctx
        range
        isDowncast
        typ
        (expr: Fable.Expr)
        : Rust.Expr
        =
        // cast to Fable.Any and type test
        let callee = transformCallee com ctx expr
        let genArgsOpt = transformGenArgs com ctx [ typ ]
        let anyTy = makeAnyTy com ctx |> mkRefTy None
        let toAnyExpr = callee |> mkCastExpr anyTy

        match expr with
        | Fable.IdentExpr ident when isDowncast ->
            let downcastExpr =
                mkMethodCallExpr "downcast_ref" genArgsOpt toAnyExpr []

            let pat =
                makeUnionCasePat
                    (rawIdent "Some")
                    [ makeFullNameIdentPat ident.Name ]

            mkLetExpr pat downcastExpr
        | _ -> mkMethodCallExpr "is" genArgsOpt toAnyExpr []

    let transformTest
        (com: IRustCompiler)
        ctx
        range
        kind
        (fableExpr: Fable.Expr)
        : Rust.Expr
        =
        match kind with
        | Fable.TypeTest typ ->
            transformTypeTest com ctx range false typ fableExpr
        | Fable.OptionTest isSome ->
            let test =
                if isSome then
                    "is_some"
                else
                    "is_none"

            let expr = com.TransformExpr(ctx, fableExpr)
            mkMethodCallExpr test None expr []
        | Fable.ListTest nonEmpty ->
            let expr = libCall com ctx range [] "List" "isEmpty" [ fableExpr ]

            if nonEmpty then
                mkNotExpr expr
            else
                expr //, ?loc=range
        | Fable.UnionCaseTest tag ->
            match fableExpr.Type with
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)
                assert (ent.IsFSharpUnion)
                // let genArgsOpt = transformGenArgs com ctx genArgs // TODO:
                let unionCase = ent.UnionCases |> List.item tag

                let fields =
                    match fableExpr with
                    | Fable.IdentExpr ident ->
                        unionCase.UnionCaseFields
                        |> List.mapi (fun i _field ->
                            let fieldName = $"{ident.Name}_{tag}_{i}"
                            makeFullNameIdentPat fieldName
                        )
                    | _ ->
                        if List.isEmpty unionCase.UnionCaseFields then
                            []
                        else
                            [ WILD_PAT ]

                let unionCaseName = getUnionCaseName com ctx entRef unionCase
                let pat = makeUnionCasePat unionCaseName fields

                let expr =
                    fableExpr
                    |> prepareRefForPatternMatch
                        com
                        ctx
                        fableExpr.Type
                        (tryGetIdentName fableExpr)

                mkLetExpr pat expr
            | _ -> failwith "Should not happen"

    let transformSwitch
        (com: IRustCompiler)
        ctx
        (evalExpr: Fable.Expr)
        cases
        defaultCase
        targets
        : Rust.Expr
        =
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

                            let fieldType =
                                FableTransforms.uncurryType field.FieldType

                            (fieldName, idName, fieldType)
                        )
                    | _ -> []
                else
                    []
            | _ -> []

        let makeArm
            pat
            targetIndex
            boundValues
            (extraVals: (string * string * Fable.Type) list)
            =
            let attrs = []
            let guard = None // TODO:

            let idents, (bodyExpr: Fable.Expr) =
                targets |> List.item targetIndex // TODO:

            let vars =
                idents |> List.map (fun (ident: Fable.Ident) -> ident.Name)
            // TODO: vars, boundValues
            let body =
                //com.TransformExpr(ctx, bodyExpr)
                // let usages = calcIdentUsages bodyExpr
                let getScope name =
                    let scopedVarAttrs =
                        {
                            IsArm = true
                            IsRef = true
                            IsBox = false
                            IsFunc = false
                            UsageCount = countIdentUsage name bodyExpr
                        }

                    name, scopedVarAttrs

                let symbolsAndNames =
                    let fromIdents =
                        idents |> List.map (fun ident -> getScope ident.Name)

                    let fromExtra =
                        extraVals
                        |> List.map (fun (_name, friendlyName, _t) ->
                            getScope friendlyName
                        )

                    fromIdents @ fromExtra

                let scopedSymbols =
                    Helpers.Map.merge
                        ctx.ScopedSymbols
                        (symbolsAndNames |> Map.ofList)

                let ctx = { ctx with ScopedSymbols = scopedSymbols }
                transformLeaveContext com ctx None bodyExpr

            mkArm attrs pat guard body

        let makeUnionCasePatOpt evalType evalName caseIndex =
            match evalType with
            | Fable.Option(genArg, _) ->
                // let genArgsOpt = transformGenArgs com ctx [genArg]
                let unionCaseFullName =
                    [
                        "Some"
                        "None"
                    ]
                    |> List.item caseIndex
                    |> rawIdent

                let fields =
                    match evalName with
                    | Some idName ->
                        match caseIndex with
                        | 0 ->
                            let fieldName = $"{idName}_{caseIndex}_{0}"
                            [ makeFullNameIdentPat fieldName ]
                        | _ -> []
                    | _ -> [ WILD_PAT ]

                let unionCaseName =
                    tryUseKnownUnionCaseNames unionCaseFullName
                    |> Option.defaultValue unionCaseFullName

                Some(makeUnionCasePat unionCaseName fields)
            | Fable.DeclaredType(entRef, genArgs) ->
                let ent = com.GetEntity(entRef)

                if ent.IsFSharpUnion then
                    // let genArgsOpt = transformGenArgs com ctx genArgs
                    let unionCase = ent.UnionCases |> List.item caseIndex

                    let fields =
                        match evalName with
                        | Some idName ->
                            unionCase.UnionCaseFields
                            |> List.mapi (fun i _field ->
                                let fieldName = $"{idName}_{caseIndex}_{i}"
                                makeFullNameIdentPat fieldName
                            )
                        | _ ->
                            if List.isEmpty unionCase.UnionCaseFields then
                                []
                            else
                                [ WILD_PAT ]

                    let unionCaseName =
                        getUnionCaseName com ctx entRef unionCase

                    Some(makeUnionCasePat unionCaseName fields)
                else
                    None
            | _ -> None

        let evalType, evalName =
            match evalExpr with
            | Fable.Get(Fable.IdentExpr ident, Fable.UnionTag, _, _) ->
                ident.Type, Some ident.Name
            | _ -> evalExpr.Type, None

        let arms =
            cases
            |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                let patOpt =
                    match caseExpr with
                    | Fable.Value(Fable.NumberConstant(:? int as tag,
                                                       Int32,
                                                       Fable.NumberInfo.Empty),
                                  r) ->
                        makeUnionCasePatOpt evalType evalName tag
                    | _ -> None

                let pat =
                    match patOpt with
                    | Some pat -> pat
                    | _ -> com.TransformExpr(ctx, caseExpr) |> mkLitPat

                let extraVals = namesForIndex evalType evalName targetIndex
                makeArm pat targetIndex (boundValues) extraVals
            )

        let defaultArm =
            let targetIndex, boundValues = defaultCase
            // To see if the default arm should actually be a union case pattern, we have to
            // examine its body to see if it starts with union field get. // TODO: look deeper
            // If it does, we'll replace the wildcard "_" with a union case pattern
            let idents, bodyExpr = targets |> List.item targetIndex

            let patOpt =
                let rec getUnionPat expr =
                    match expr with
                    | Fable.Get(Fable.IdentExpr ident, Fable.OptionValue, _, _) when
                        Some ident.Name = evalName && ident.Type = evalType
                        ->
                        makeUnionCasePatOpt evalType evalName 0
                    | Fable.Get(Fable.IdentExpr ident,
                                Fable.UnionField info,
                                _,
                                _) when
                        Some ident.Name = evalName && ident.Type = evalType
                        ->
                        makeUnionCasePatOpt evalType evalName info.CaseIndex
                    | _ ->
                        //need to recurse or this only works for trivial expressions
                        let subExprs = getSubExpressions expr
                        subExprs |> List.tryPick getUnionPat

                getUnionPat bodyExpr

            let pat = patOpt |> Option.defaultValue WILD_PAT
            let extraVals = namesForIndex evalType evalName targetIndex
            makeArm pat targetIndex boundValues extraVals

        let expr =
            evalExpr |> prepareRefForPatternMatch com ctx evalType evalName

        mkMatchExpr expr (arms @ [ defaultArm ])

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then
            []
        elif List.length idents = List.length values then
            List.zip idents values
        else
            failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues
        (com: IRustCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues

        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr) :: bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements
                )

            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccess
        (com: IRustCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let bindings, target =
            getDecisionTargetAndBindValues com ctx targetIndex boundValues

        match bindings with
        | [] -> transformLeaveContext com ctx None target
        | bindings ->
            let target =
                List.rev bindings
                |> List.fold (fun e (i, v) -> Fable.Let(i, v, e)) target

            transformLeaveContext com ctx None target

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) =
            function
            | Fable.Operation(Fable.Binary(BinaryEqual, left, right), _, _, _) ->
                match left, right with
                | _,
                  Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _),
                              _) -> Some(left, right)
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _),
                              _),
                  _ -> Some(right, left)
                | _ -> None
            | Fable.Test(expr, Fable.OptionTest isSome, r) ->
                let evalExpr =
                    Fable.Get(
                        expr,
                        Fable.UnionTag,
                        Fable.Number(Int32, Fable.NumberInfo.Empty),
                        r
                    )

                let right =
                    makeIntConst (
                        if isSome then
                            0
                        else
                            1
                    )

                Some(evalExpr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, r) ->
                let evalExpr =
                    Fable.Get(
                        expr,
                        Fable.UnionTag,
                        Fable.Number(Int32, Fable.NumberInfo.Empty),
                        r
                    )

                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None

        let rec sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2 -> i1.Name = i2.Name
            | Fable.Get(e1, Fable.UnionTag, _, _),
              Fable.Get(e2, Fable.UnionTag, _, _)
            | Fable.Get(e1, Fable.ListHead, _, _),
              Fable.Get(e2, Fable.ListHead, _, _)
            | Fable.Get(e1, Fable.ListTail, _, _),
              Fable.Get(e2, Fable.ListTail, _, _)
            | Fable.Get(e1, Fable.OptionValue, _, _),
              Fable.Get(e2, Fable.OptionValue, _, _) -> sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.TupleIndex i1, _, _),
              Fable.Get(e2, Fable.TupleIndex i2, _, _) ->
                i1 = i2 && sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.FieldGet f1, _, _),
              Fable.Get(e2, Fable.FieldGet f2, _, _) ->
                f1.Name = f2.Name && sameEvalExprs e1 e2
            | Fable.Get(e1, Fable.UnionField f1, _, _),
              Fable.Get(e2, Fable.UnionField f2, _, _) ->
                f1.CaseIndex = f2.CaseIndex
                && f1.FieldIndex = f2.FieldIndex
                && sameEvalExprs e1 e2
            | _ -> false

        let rec checkInner cases evalExpr treeExpr =
            match treeExpr with
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex,
                                                         boundValues,
                                                         _),
                               treeExpr,
                               _) when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex,
                                            defaultBoundValues,
                                            _) ->
                    let cases = (caseExpr, targetIndex, boundValues) :: cases

                    Some(
                        evalExpr,
                        List.rev cases,
                        (defaultTargetIndex, defaultBoundValues)
                    )
                | treeExpr ->
                    let cases = (caseExpr, targetIndex, boundValues) :: cases
                    checkInner cases evalExpr treeExpr
            | Fable.DecisionTreeSuccess(defaultTargetIndex,
                                        defaultBoundValues,
                                        _) ->
                Some(
                    evalExpr,
                    List.rev cases,
                    (defaultTargetIndex, defaultBoundValues)
                )
            | _ -> None

        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                           treeExpr,
                           _) ->
            let cases = [ (caseExpr, targetIndex, boundValues) ]
            checkInner cases evalExpr treeExpr
        | _ -> None

    // let simplifyDecisionTree (treeExpr: Fable.Expr) =
    //     treeExpr |> visitFromInsideOut (function
    //         | Fable.IfThenElse(
    //             guardExpr1,
    //             Fable.IfThenElse(
    //                 guardExpr2,
    //                 thenExpr,
    //                 Fable.DecisionTreeSuccess(index2,[],_),_),
    //             Fable.DecisionTreeSuccess(index1,[],t),r)
    //             when index1 = index2 ->
    //             Fable.IfThenElse(
    //                 makeLogOp None guardExpr1 guardExpr2 LogicalAnd,
    //                 thenExpr,
    //                 Fable.DecisionTreeSuccess(index2,[],t),r)
    //         | e -> e)

    let transformDecisionTree
        (com: IRustCompiler)
        ctx
        targets
        (expr: Fable.Expr)
        : Rust.Expr
        =
        // let expr = simplifyDecisionTree expr
        match transformDecisionTreeAsSwitch expr with
        | Some(evalExpr, cases, defaultCase) ->
            transformSwitch com ctx evalExpr cases defaultCase targets
        | None ->
            let ctx = { ctx with DecisionTargets = targets }
            com.TransformExpr(ctx, expr)

    let rec transformExpr
        (com: IRustCompiler)
        ctx
        (fableExpr: Fable.Expr)
        : Rust.Expr
        =
        match fableExpr with
        | Fable.Unresolved(e, t, r) ->
            "Unexpected unresolved expression: %A{e}" |> addError com [] r
            mkUnitExpr ()

        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr ident -> transformIdentGet com ctx None ident

        | Fable.Import(info, t, r) -> transformImport com ctx r t info None

        | Fable.Test(expr, kind, range) -> transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformLambda com ctx name [ arg ] body

        | Fable.Delegate(args, body, name, _) ->
            transformLambda com ctx name args body

        | Fable.ObjectExpr(members, typ, baseCall) ->
            transformObjectExpr com ctx typ members baseCall

        | Fable.Call(callee, info, typ, range) ->
            transformCall com ctx range typ callee info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApply com ctx range typ callee args

        | Fable.Operation(kind, _, typ, range) ->
            transformOperation com ctx range typ kind

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            transformIfThenElse com ctx r guardExpr thenExpr elseExpr

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTree com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccess com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

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

        | Fable.WhileLoop(guard, body, range) ->
            transformWhileLoop com ctx range guard body

        | Fable.ForLoop(var, start, limit, body, isUp, range) ->
            transformForLoop com ctx range isUp var start limit body

        | Fable.TryCatch(body, catch, finalizer, range) ->
            transformTryCatch com ctx range body catch finalizer

        | Fable.Extended(kind, r) ->
            match kind with
            | Fable.Curry(expr, arity) -> transformCurry com ctx arity expr
            | Fable.Throw(exprOpt, typ) -> transformThrow com ctx typ exprOpt
            | Fable.Debugger ->
                // TODO:
                $"Unimplemented Extended expression: %A{kind}"
                |> addWarning com [] r

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
            |> Option.map (fun _ -> [ splitLast decl.Name ])
        | Fable.ActionDeclaration decl -> None
        | Fable.ClassDeclaration decl -> None

    let isLastFileInProject (com: IRustCompiler) =
        (Array.last com.SourceFiles) = com.CurrentFile

    let getModuleItems (com: IRustCompiler) ctx =
        if isLastFileInProject com then
            // add all other source files (except signatures) as module imports
            com.SourceFiles
            |> Array.filter (fun x ->
                not (x.EndsWith(".fsi", StringComparison.Ordinal))
            )
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

                [
                    modItem
                    useItem |> mkPublicItem
                ]

            let modItems =
                com.GetAllModules() |> List.sort |> List.collect makeModItems

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

                            let useItem =
                                mkGlobUseItem
                                    []
                                    ("crate" :: modName :: modNames)

                            yield useItem |> mkPublicItem
                    for KeyValue(key, trie) in trie.Children do
                        let items = toItems (key :: mods) trie
                        let modItem = mkModItem [] key items
                        yield modItem |> mkPublicItem
                ]
            // re-export globally merged namespaces at crate level
            let nsItems =
                com.GetAllNamespaces() |> Namespace.Trie.ofSeq |> toItems []

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

            let attrs = []
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            let fnItem = mkFnItem attrs "main" fnKind
            [ fnItem |> mkPublicItem ]

        | None -> []

    let getEntityFieldsAsIdents _com (ent: Fable.Entity) : Fable.Ident list =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name
            let typ = FableTransforms.uncurryType field.FieldType
            let isMutable = field.IsMutable
            let ident = makeTypedIdent typ name
            { ident with IsMutable = isMutable }
        )
        |> Seq.toList

    let makeTypedParam
        (com: IRustCompiler)
        ctx
        (ident: Fable.Ident)
        returnType
        =
        if ident.IsThisArgument then
            // is this a fluent API?
            match ident.Type, shouldBeRefCountWrapped com ctx ident.Type with
            | Fable.DeclaredType(entRef, genArgs), Some ptrType when
                ident.Type = returnType
                ->
                // for fluent APIs, set the type of thisArg to (self: &Lrc<Self>)
                let ty = mkImplSelfTy ()

                let ty =
                    match ptrType with
                    | Lrc -> ty |> makeFluentTy com ctx
                    | Rc -> ty |> makeRcTy com ctx
                    | Arc -> ty |> makeArcTy com ctx
                    | Box -> ty |> makeBoxTy com ctx
                    |> mkRefTy None

                mkParamFromType (rawIdent "self") ty false false
            | _ -> mkImplSelfParam false false
        elif ctx.IsLambda && ident.Type = Fable.Any then
            mkInferredParam ident.Name false false
        else
            let ty = transformType com ctx ident.Type
            mkParamFromType ident.Name ty false false

    let transformFunctionDecl
        (com: IRustCompiler)
        ctx
        args
        (parameters: Fable.Parameter list)
        returnType
        =
        let inputs =
            args
            |> List.mapi (fun idx ident ->
                let isByRefPreferred = parameterIsByRefPreferred idx parameters

                let ctx =
                    { ctx with
                        IsParamByRefPreferred =
                            isByRefPreferred || ctx.IsParamByRefPreferred
                    }

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
        ||
        // Closures may capture Ref counted vars, so by cloning
        // the actual closure, all attached ref counted var are cloned too
        (shouldBeRefCountWrapped com ctx typ |> Option.isSome)

    let isClosedOverIdent com ctx (ident: Fable.Ident) =
        not (ident.IsCompilerGenerated && ident.Name = "matchValue")
        && not (ident.IsThisArgument && ctx.IsAssocMember)
        && (ident.IsMutable
            || isValueScoped ctx ident.Name
            || isRefScoped ctx ident.Name
            || shouldBeCloned com ctx ident.Type)

    let tryFindClosedOverIdent com ctx (ignoredNames: HashSet<string>) expr =
        match expr with
        | Fable.IdentExpr ident ->
            if
                not (ignoredNames.Contains(ident.Name))
                && (isClosedOverIdent com ctx ident)
            then
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
        | Fable.Delegate(args, body, name, _) ->
            args |> List.iter (fun arg -> ignoredNames.Add(arg.Name) |> ignore)
            None
        | Fable.Let(ident, _, _) ->
            ignoredNames.Add(ident.Name) |> ignore
            None
        | Fable.LetRec(bindings, _) ->
            bindings
            |> List.iter (fun (ident, _) ->
                ignoredNames.Add(ident.Name) |> ignore
            )

            None
        | Fable.DecisionTree(_, targets) ->
            targets
            |> List.iter (fun (idents, _) ->
                idents
                |> List.iter (fun ident ->
                    ignoredNames.Add(ident.Name) |> ignore
                )
            )

            None
        | Fable.TryCatch(body, catch, finalizer, _) ->
            catch
            |> Option.iter (fun (ident, expr) ->
                ignoredNames.Add(ident.Name) |> ignore
            )

            None
        | _ -> None

    let getIgnoredNames (name: string option) (args: Fable.Ident list) =
        let argNames = args |> List.map (fun arg -> arg.Name)
        let allNames = name |> Option.fold (fun xs x -> x :: xs) argNames
        allNames |> Set.ofList

    let hasCapturedIdents
        com
        ctx
        (name: string)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let ignoredNames = HashSet(getIgnoredNames (Some name) args)

        let isClosedOver expr =
            tryFindClosedOverIdent com ctx ignoredNames expr |> Option.isSome

        deepExists isClosedOver body

    let getCapturedIdents
        com
        ctx
        (name: string option)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let ignoredNames = HashSet(getIgnoredNames name args)
        let mutable capturedIdents = Map.empty

        let addClosedOver expr =
            tryFindClosedOverIdent com ctx ignoredNames expr
            |> Option.iter (fun ident ->
                capturedIdents <- capturedIdents |> Map.add ident.Name ident
            )

            false
        // collect all closed over names that are not arguments
        deepExists addClosedOver body |> ignore
        capturedIdents

    let getFunctionBodyCtx
        com
        ctx
        (name: string option)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        isTailRec
        =
        // let usages = calcIdentUsages body
        let scopedSymbols =
            (ctx.ScopedSymbols, args)
            ||> List.fold (fun acc arg ->
                //TODO: optimizations go here
                let scopedVarAttrs =
                    {
                        IsArm = false
                        IsRef =
                            arg.IsThisArgument
                            || isByRefOrAnyType com arg.Type
                            || ctx.IsParamByRefPreferred
                        IsBox = false
                        IsFunc = false
                        UsageCount = countIdentUsage arg.Name body
                    }

                acc |> Map.add arg.Name scopedVarAttrs
            )

        let tco =
            if isTailRec then
                Some(
                    NamedTailCallOpportunity(com, ctx, name.Value, args)
                    :> ITailCallOpportunity
                )
            else
                None

        { ctx with
            ScopedSymbols = scopedSymbols
            IsParamByRefPreferred = false
            TailCallOpportunity = tco
        }

    let isTailRecursive (name: string option) (body: Fable.Expr) =
        if name.IsNone then
            false, false
        else
            FableTransforms.isTailRecursive name.Value body

    let transformFunctionBody
        com
        ctx
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        match ctx.TailCallOpportunity with
        | Some tc ->
            // tail call elimination setup (temp vars, loop, break)
            let label = tc.Label

            let args =
                args
                |> List.filter (fun arg ->
                    not (arg.IsMutable || arg.IsThisArgument)
                )

            let mutArgs =
                args |> List.map (fun arg -> { arg with IsMutable = true })

            let idExprs = args |> List.map (fun arg -> Fable.IdentExpr arg)
            let bindings = List.zip mutArgs idExprs

            let argMap =
                mutArgs
                |> List.map (fun arg -> arg.Name, Fable.IdentExpr arg)
                |> Map.ofList

            let body = FableTransforms.replaceValues argMap body
            let letStmts, ctx = makeLetStmts com ctx bindings body Map.empty
            let loopBody = transformLeaveContext com ctx None body
            let loopExpr = mkBreakExpr (Some label) (Some(mkParenExpr loopBody))
            let loopStmt = mkLoopExpr (Some label) loopExpr |> mkExprStmt
            letStmts @ [ loopStmt ] |> mkStmtBlockExpr
        | _ -> transformLeaveContext com ctx None body

    let transformFunc
        com
        ctx
        (name: string option)
        (parameters: Fable.Parameter list)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let isRecursive, isTailRec = isTailRecursive name body
        let genArgs, ctx = getNewGenArgsAndCtx ctx args body
        let args = args |> discardUnitArg genArgs
        let fnDecl = transformFunctionDecl com ctx args parameters body.Type
        let ctx = getFunctionBodyCtx com ctx name args body isTailRec
        let fnBody = transformFunctionBody com ctx args body
        fnDecl, fnBody, genArgs

    let transformLambda
        com
        ctx
        (name: string option)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let ctx = { ctx with IsLambda = true }
        let genArgs, ctx = getNewGenArgsAndCtx ctx args body
        let args = args |> discardUnitArg genArgs
        let isRecursive, isTailRec = isTailRecursive name body
        let fnDecl = transformFunctionDecl com ctx args [] Fable.Unit
        let ctx = getFunctionBodyCtx com ctx name args body isTailRec
        // remove captured names from scoped symbols, as they will be cloned
        let closedOverCloneableIdents = getCapturedIdents com ctx name args body

        let scopedSymbols =
            ctx.ScopedSymbols |> Helpers.Map.except closedOverCloneableIdents

        let ctx = { ctx with ScopedSymbols = scopedSymbols } //; HasMultipleUses = true }
        let argCount = args |> List.length |> string<int>
        let fnBody = transformFunctionBody com ctx args body

        let fnBody =
            if isRecursive && not isTailRec then
                // make the closure recursive with fixed-point combinator
                let fixedArgs = (makeIdent name.Value) :: args

                let fixedDecl =
                    transformFunctionDecl com ctx fixedArgs [] Fable.Unit

                let fixedBody = mkClosureExpr true fixedDecl fnBody
                let argExprs = args |> List.map Fable.IdentExpr
                let callArgs = transformCallArgs com ctx argExprs [] []
                let fixCallArgs = (fixedBody |> mkAddrOfExpr) :: callArgs
                makeLibCall com ctx None "Native" ("fix" + argCount) fixCallArgs
            else
                fnBody

        let cloneStmts =
            // clone captured idents (in move closures)
            // skip non-local idents (e.g. module let bindings)
            Map.keys closedOverCloneableIdents
            |> Seq.filter (fun name -> not (name.Contains(".")))
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
                mkStmtBlockExpr (cloneStmts @ [ closureExpr |> mkExprStmt ])

        let funcWrap = getLibraryImportName com ctx "Native" ("Func" + argCount)

        makeCall
            [
                funcWrap
                "new"
            ]
            None
            [ closureExpr ]

    let makeTypeBounds
        (com: IRustCompiler)
        ctx
        argName
        (constraints: Fable.Constraint list)
        =
        let makeGenBound names tyNames =
            // makes gen type bound, e.g. T: From(i32), or T: Default
            let tys =
                tyNames
                |> List.map (fun tyName -> mkGenericPathTy [ tyName ] None)

            let genArgsOpt = mkConstraintArgs tys []
            mkTypeTraitGenericBound names genArgsOpt

        let makeRawBound id = makeGenBound [ rawIdent id ] []

        let makeOpBound op =
            // makes ops type bound, e.g. T: Add(Output=T)
            let ty = mkGenericPathTy [ argName ] None
            let genArgsOpt = mkConstraintArgs [] [ "Output", ty ]

            mkTypeTraitGenericBound
                [
                    "core"
                    "ops"
                    op
                ]
                genArgsOpt

        let makeConstraint =
            function
            | Fable.Constraint.HasMember(membName, isStatic) ->
                match membName, isStatic with
                | Operators.addition, true -> [ makeOpBound "Add" ]
                | Operators.subtraction, true -> [ makeOpBound "Sub" ]
                | Operators.multiply, true -> [ makeOpBound "Mul" ]
                | Operators.division, true -> [ makeOpBound "Div" ]
                | Operators.modulus, true -> [ makeOpBound "Rem" ]
                | Operators.unaryNegation, true -> [ makeOpBound "Neg" ]
                | Operators.divideByInt, true ->
                    [
                        makeOpBound "Div"
                        makeGenBound [ rawIdent "From" ] [ "i32" ]
                    ]
                | "get_Zero", true -> [ makeRawBound "Default" ]
                | _ -> []
            | Fable.Constraint.CoercesTo(targetType) ->
                match targetType with
                | IFormattable ->
                    [
                        makeGenBound
                            [
                                "core"
                                "fmt"
                                "Display"
                            ]
                            []
                    ]
                | IEquatable _ ->
                    [
                        makeRawBound "Eq"
                        makeGenBound
                            [
                                "core"
                                "hash"
                                "Hash"
                            ]
                            []
                    ]
                | Fable.DeclaredType(entRef, genArgs) ->
                    let ent = com.GetEntity(entRef)

                    if ent.IsInterface then
                        let nameParts =
                            getInterfaceImportName com ctx entRef
                            |> splitNameParts

                        let genArgsOpt = transformGenArgs com ctx genArgs

                        let traitBound =
                            mkTypeTraitGenericBound nameParts genArgsOpt

                        [ traitBound ]
                    else
                        []
                | _ -> []
            | Fable.Constraint.IsNullable -> []
            | Fable.Constraint.IsValueType -> []
            | Fable.Constraint.IsReferenceType -> []
            | Fable.Constraint.HasDefaultConstructor -> []
            | Fable.Constraint.HasComparison -> [ makeRawBound "PartialOrd" ]
            | Fable.Constraint.HasEquality -> //[makeRawBound "PartialEq"]
                [
                    makeRawBound "Eq"
                    makeGenBound
                        [
                            "core"
                            "hash"
                            "Hash"
                        ]
                        []
                ]
            | Fable.Constraint.IsUnmanaged -> []
            | Fable.Constraint.IsEnum -> []

        constraints |> List.distinct |> List.collect makeConstraint

    let defaultTypeBounds =
        [
            mkTypeTraitGenericBound [ rawIdent "Clone" ] None
            mkLifetimeGenericBound "'static" //TODO: add it only when needed
        ]

    let makeGenericParams com ctx (genArgs: Fable.Type list) =
        genArgs
        |> List.choose (
            function
            | Fable.GenericParam(name, isMeasure, constraints) when
                not isMeasure
                ->
                let typeBounds = makeTypeBounds com ctx name constraints

                let p =
                    mkGenericParamFromName
                        []
                        name
                        (typeBounds @ defaultTypeBounds)

                Some p
            | _ -> None
        )

    let makeGenerics com ctx (genArgs: Fable.Type list) =
        makeGenericParams com ctx genArgs |> mkGenerics

    let makeNestedFuncCtx com ctx (ident: Fable.Ident) usages =
        let scopedVarAttrs =
            {
                IsArm = false
                IsRef = false
                IsBox = false
                IsFunc = true // means it's a local (nested) fn, not a closure
                UsageCount = usageCount ident.Name usages
            }

        let scopedSymbols =
            ctx.ScopedSymbols |> Map.add ident.Name scopedVarAttrs

        let ctxNext = { ctx with ScopedSymbols = scopedSymbols }
        ctxNext

    let makeFnHeader com ctx (attributes: Fable.Attribute seq) : Rust.FnHeader =
        let isAsync =
            attributes
            |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustAsync)

        let isConst =
            attributes
            |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustConst)

        let isUnsafe =
            attributes
            |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustUnsafe)

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

    let transformNestedFunction
        com
        ctx
        (ident: Fable.Ident)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        usages
        =
        let name = ident.Name

        let fnDecl, fnBody, genArgs =
            transformFunc com ctx (Some name) [] args body

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
        let ctxNext = makeNestedFuncCtx com ctx ident usages
        mkItemStmt fnItem, ctxNext

    let transformAttributes com ctx (attributes: Fable.Attribute seq) =
        attributes
        |> Seq.collect (fun a ->
            // Rust outer attributes
            if a.Entity.FullName = Atts.rustOuterAttr then
                match a.ConstructorArgs with
                | [ :? string as name ] -> [ mkAttr name [] ]
                | [ :? string as name; :? string as value ] ->
                    [ mkEqAttr name value ]
                | [ :? string as name; :? (obj[]) as items ] ->
                    [ mkAttr name (items |> Array.map string<obj>) ]
                | _ -> []
            // translate test methods attributes
            // TODO: support more test frameworks
            elif
                a.Entity.FullName.EndsWith(
                    ".FactAttribute",
                    StringComparison.Ordinal
                )
            then
                [ mkAttr "test" [] ]
            else
                []
        )
        |> Seq.toList

    let transformInnerAttributes com ctx (attributes: Fable.Attribute seq) =
        attributes
        |> Seq.collect (fun att ->
            // Rust inner attributes
            if att.Entity.FullName = Atts.rustInnerAttr then
                match att.ConstructorArgs with
                | [ :? string as name ] -> [ mkInnerAttr name [] ]
                | [ :? string as name; :? string as value ] ->
                    [ mkInnerEqAttr name value ]
                | [ :? string as name; :? (obj[]) as items ] ->
                    [ mkInnerAttr name (items |> Array.map string<obj>) ]
                | _ -> []
            else
                []
        )
        |> Seq.toList

    let getInnerAttributes
        (com: IRustCompiler)
        ctx
        (decls: Fable.Declaration list)
        =
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


    let transformModuleFunction
        (com: IRustCompiler)
        ctx
        (memb: Fable.MemberFunctionOrValue)
        (decl: Fable.MemberDecl)
        =
        let name = splitLast decl.Name
        //if name = "someProblematicFunction" then System.Diagnostics.Debugger.Break()
        let isByRefPreferred =
            memb.Attributes
            |> Seq.exists (fun a -> a.Entity.FullName = Atts.rustByRef)

        let fnDecl, fnBody, genArgs =
            let ctx = { ctx with IsParamByRefPreferred = isByRefPreferred }
            let parameters = memb.CurriedParameterGroups |> List.concat

            transformFunc
                com
                ctx
                (Some memb.FullName)
                parameters
                decl.Args
                decl.Body

        let fnBodyBlock =
            if decl.Body.Type = Fable.Unit then
                mkSemiBlock fnBody
            else
                mkExprBlock fnBody

        let header = makeFnHeader com ctx memb.Attributes
        let generics = makeGenerics com ctx genArgs
        let kind = mkFnKind header fnDecl generics (Some fnBodyBlock)
        let attrs = transformAttributes com ctx memb.Attributes
        let fnItem = mkFnItem attrs name kind
        fnItem

    let transformModuleLetValue
        (com: IRustCompiler)
        ctx
        (memb: Fable.MemberFunctionOrValue)
        (decl: Fable.MemberDecl)
        =
        // expected output:
        // pub fn value() -> T {
        //     static value: MutCell<Option<T>> = MutCell::new(None);
        //     value.get_or_init(|| initValue)
        // }
        let name = splitLast decl.Name
        let typ = decl.Body.Type

        let initNone =
            mkGenericPathExpr [ rawIdent "None" ] None |> makeMutValue com ctx

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

        let staticTy = ty |> makeOptionTy |> makeMutTy com ctx

        let staticStmt =
            mkStaticItem [] name staticTy (Some initNone) |> mkItemStmt

        let callee = com.TransformExpr(ctx, makeIdentExpr name)

        let closureExpr =
            let fnDecl = mkFnDecl [] VOID_RETURN_TY
            mkClosureExpr false fnDecl value

        let valueStmt =
            mkMethodCallExpr "get_or_init" None callee [ closureExpr ]
            |> mkExprStmt

        let attrs = transformAttributes com ctx memb.Attributes

        let fnBody =
            [
                staticStmt
                valueStmt
            ]
            |> mkBlock
            |> Some

        let fnDecl = mkFnDecl [] (mkFnRetTy ty)
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnItem attrs name fnKind
        fnItem

    // // is the member return type the same as the entity
    // let isFluentMemberType (ent: Fable.Entity) = function
    //     | Fable.DeclaredType(entRef, _) -> entRef.FullName = ent.FullName
    //     | _ -> false

    // does the member body return thisArg
    let isFluentMemberBody (body: Fable.Expr) =
        let rec loop =
            function
            | Fable.IdentExpr ident when ident.IsThisArgument -> true
            | Fable.Sequential exprs -> loop (List.last exprs)
            | Fable.Let(_, value, body) -> loop body
            | Fable.LetRec(bindings, body) -> loop body
            | Fable.IfThenElse(cond, thenExpr, elseExpr, _) ->
                loop thenExpr || loop elseExpr
            | Fable.DecisionTree(expr, targets) ->
                List.map snd targets |> List.exists loop
            | _ -> false

        loop body

    let makeAssocMemberItem
        (com: IRustCompiler)
        ctx
        (memb: Fable.MemberFunctionOrValue)
        (args: Fable.Ident list)
        (bodyOpt: Rust.Block option)
        =
        let ctx = { ctx with IsAssocMember = true }
        let name = memb.CompiledName
        let args = args |> discardUnitArg []
        let parameters = memb.CurriedParameterGroups |> List.concat
        let returnType = memb.ReturnParameter.Type
        let fnDecl = transformFunctionDecl com ctx args parameters returnType
        let genArgs = FSharp2Fable.Util.getMemberGenArgs memb
        let generics = makeGenerics com ctx genArgs
        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl generics bodyOpt
        let attrs = transformAttributes com ctx memb.Attributes

        let attrs =
            attrs
            @ if bodyOpt.IsSome then
                  [ mkAttr "inline" [] ]
              else
                  []

        let fnItem = mkFnAssocItem attrs name fnKind
        fnItem

    let transformAssocMember
        (com: IRustCompiler)
        ctx
        (memb: Fable.MemberFunctionOrValue)
        (membName: string)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let ctx = { ctx with IsAssocMember = true }
        let name = splitLast membName

        let fnDecl, fnBody, genArgs =
            let parameters = memb.CurriedParameterGroups |> List.concat
            transformFunc com ctx (Some name) parameters args body

        let fnBody =
            if isFluentMemberBody body then
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
        let attrs = transformAttributes com ctx memb.Attributes
        let fnItem = mkFnAssocItem attrs name fnKind
        fnItem

    let getInterfaceMemberNames (com: IRustCompiler) (entRef: Fable.EntityRef) =
        let ent = com.GetEntity(entRef)
        assert (ent.IsInterface)

        ent.AllInterfaces
        |> Seq.collect (fun i ->
            let e = com.GetEntity(i.Entity)
            e.MembersFunctionsAndValues
        )
        |> Seq.map (fun m -> m.CompiledName)
        |> Set.ofSeq

    let makeDerivedFrom com (ent: Fable.Entity) =
        let isCopyable = ent |> isCopyableEntity com Set.empty
        let isPrintable = ent |> isPrintableEntity com Set.empty
        let isDefaultable = ent |> isDefaultableEntity com Set.empty
        let isComparable = ent |> isComparableEntity com Set.empty
        let isEquatable = ent |> isEquatableEntity com Set.empty
        let isHashable = ent |> isHashableEntity com Set.empty

        let derivedFrom =
            [
                rawIdent "Clone"
                if isCopyable then
                    rawIdent "Copy"
                if isPrintable then
                    rawIdent "Debug"
                if isDefaultable then
                    rawIdent "Default"
                if isEquatable then
                    rawIdent "PartialEq"
                if isComparable then
                    rawIdent "PartialOrd"
                if isHashable then
                    rawIdent "Hash"
                if isEquatable && isHashable then
                    rawIdent "Eq"
                if isComparable && isHashable then
                    rawIdent "Ord"
            ]

        derivedFrom

    let transformAbbrev (com: IRustCompiler) ctx (ent: Fable.Entity) =
        // TODO: this is unfinished and untested
        let entName = splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound [ entName ] genArgsOpt
        let ty = mkTraitTy [ traitBound ]
        let generics = makeGenerics com ctx genArgs
        let bounds = [] //TODO:
        let tyItem = mkTyAliasItem [] entName ty generics bounds
        [ tyItem ]

    let transformUnion (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entName = splitLast ent.FullName
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

        let attrs = transformAttributes com ctx ent.Attributes
        let attrs = attrs @ [ mkAttr "derive" (makeDerivedFrom com ent) ]
        let enumItem = mkEnumItem attrs entName variants generics
        enumItem

    let transformClass (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entName = splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let generics = makeGenerics com ctx genArgs
        let isPublic = ent.IsFSharpRecord
        let idents = getEntityFieldsAsIdents com ent

        let fields =
            idents
            |> List.map (fun ident ->
                let ty = transformType com ctx ident.Type

                let fieldTy =
                    if ident.IsMutable then
                        ty |> makeMutTy com ctx
                    else
                        ty

                let fieldName = ident.Name |> sanitizeMember
                mkField [] fieldName fieldTy isPublic
            )

        let attrs = transformAttributes com ctx ent.Attributes
        let attrs = attrs @ [ mkAttr "derive" (makeDerivedFrom com ent) ]
        let structItem = mkStructItem attrs entName fields generics
        structItem

    let transformCompilerGeneratedConstructor
        (com: IRustCompiler)
        ctx
        (ent: Fable.Entity)
        =
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
            Fable.GeneratedMember.Function(
                entName,
                paramTypes,
                body.Type,
                entRef = ent.Ref
            )

        let memb = com.GetMember(memberRef)
        let name = "new"
        let fnItem = transformAssocMember com ctx memb name args body
        let fnItem = fnItem |> memberAssocItemWithVis com ctx memb
        fnItem

    let transformPrimaryConstructor
        (com: IRustCompiler)
        ctx
        (ent: Fable.Entity)
        (ctor: Fable.MemberDecl)
        =
        let body =
            match ctor.Body with
            | Fable.Sequential exprs ->
                // get fields
                let idents = getEntityFieldsAsIdents com ent

                let argNames =
                    ctor.Args |> List.map (fun arg -> arg.Name) |> Set.ofList

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

                let fieldIdents =
                    idents
                    |> List.map (fun ident -> Map.find ident.Name identMap)

                let fieldValues = fieldIdents |> List.map Fable.IdentExpr
                let genArgs = FSharp2Fable.Util.getEntityGenArgs ent

                // add return value after the body
                let retVal =
                    Fable.Value(
                        Fable.NewRecord(fieldValues, ent.Ref, genArgs),
                        None
                    )

                let body = Fable.Sequential(exprs @ [ retVal ])
                // replace 'this.field' with just 'field' in body
                let body =
                    body
                    |> visitFromInsideOut (
                        function
                        | Fable.Set(Fable.Value(Fable.ThisValue _, _),
                                    Fable.SetKind.FieldSet(fieldName),
                                    t,
                                    value,
                                    r) ->
                            let identExpr =
                                identMap
                                |> Map.find fieldName
                                |> Fable.IdentExpr

                            Fable.Set(identExpr, Fable.ValueSet, t, value, r)
                        | Fable.Get(Fable.Value(Fable.ThisValue _, _),
                                    Fable.GetKind.FieldGet info,
                                    t,
                                    r) ->
                            let identExpr =
                                identMap
                                |> Map.find info.Name
                                |> Fable.IdentExpr

                            identExpr
                        | e -> e
                    )
                // add field declarations before body
                let body =
                    (body, fieldIdents |> List.rev)
                    ||> List.fold (fun acc ident ->
                        let nullOfT = Fable.Value(Fable.Null ident.Type, None)
                        Fable.Let(ident, nullOfT, acc)
                    ) // will be transformed as declaration only

                body
            | e -> e

        let ctor = { ctor with Body = body }
        let memb = com.GetMember(ctor.MemberRef)

        let fnItem =
            transformAssocMember com ctx memb ctor.Name ctor.Args ctor.Body

        let fnItem = fnItem |> memberAssocItemWithVis com ctx memb
        fnItem

    let makeInterfaceItems
        (com: IRustCompiler)
        ctx
        hasBody
        (ent: Fable.Entity)
        =
        ent.AllInterfaces
        |> Seq.collect (fun ifc ->
            let ifcTyp = Fable.DeclaredType(ifc.Entity, ifc.GenericArgs)
            let ifcEnt = com.GetEntity(ifc.Entity)

            ifcEnt.MembersFunctionsAndValues
            |> Seq.distinctBy (fun memb -> memb.CompiledName)
            |> Seq.map (fun memb ->
                let thisArg =
                    { makeTypedIdent ifcTyp "this" with
                        IsThisArgument = true
                    }

                let membName = memb.CompiledName

                let memberArgs =
                    memb.CurriedParameterGroups
                    |> List.collect id
                    |> List.mapi (fun i p ->
                        let name = defaultArg p.Name $"arg{i}"
                        makeTypedIdent p.Type name
                    )

                let args = (thisArg :: memberArgs)

                let bodyOpt =
                    if hasBody then
                        let thisExpr = makeThis com ctx None ifcTyp
                        let callee = thisExpr |> mkDerefExpr |> mkDerefExpr

                        let args =
                            memberArgs
                            |> List.map (transformIdent com ctx None)

                        let name = memb.CompiledName
                        let body = mkMethodCallExpr name None callee args
                        [ mkExprStmt body ] |> mkBlock |> Some
                    else
                        None

                makeAssocMemberItem com ctx memb args bodyOpt
            )
        )

    let transformInterface (com: IRustCompiler) ctx (ent: Fable.Entity) =
        let entName = splitLast ent.FullName
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent

        let traitItem =
            let assocItems = makeInterfaceItems com ctx false ent
            let generics = makeGenerics com ctx genArgs
            mkTraitItem [] entName assocItems [] generics

        let implItem =
            let memberItems = makeInterfaceItems com ctx true ent
            let genArgNames = getEntityGenParamNames ent
            let typeName = makeUniqueName "V" genArgNames
            let genArgsOpt = transformGenArgs com ctx genArgs
            let traitBound = mkTypeTraitGenericBound [ entName ] genArgsOpt
            let typeBounds = traitBound :: defaultTypeBounds
            let typeParam = mkGenericParamFromName [] typeName typeBounds
            let genParams = makeGenericParams com ctx genArgs
            let generics = typeParam :: genParams |> mkGenerics
            let ty = mkGenericTy [ typeName ] [] |> makeLrcPtrTy com ctx
            let path = mkGenericPath [ entName ] genArgsOpt
            let ofTrait = mkTraitRef path |> Some
            mkImplItem [] "" ty generics memberItems ofTrait

        [
            traitItem |> mkPublicItem
            implItem
        ]

    let makeFSharpExceptionItems com ctx (ent: Fable.Entity) =
        // expected output:
        // impl {entityName} {
        //     fn get_Message(&self) -> string {
        //         sformat!("{} {:?}", entName, (self.Data0.clone(), self.Data1.clone(), ...)))
        //     }
        // }
        if ent.IsFSharpExceptionDeclaration then
            let entName =
                Fable.Value(Fable.StringConstant(splitLast ent.FullName), None)

            let thisArg = Fable.Value(Fable.ThisValue Fable.Any, None)

            let fieldValues =
                getEntityFieldsAsIdents com ent
                |> List.map (fun ident ->
                    Fable.Get(
                        thisArg,
                        Fable.FieldInfo.Create(ident.Name),
                        ident.Type,
                        None
                    )
                )

            let fieldsAsTuple =
                Fable.Value(Fable.NewTuple(fieldValues, true), None)

            let body =
                formatString
                    com
                    ctx
                    "{} {:?}"
                    [
                        entName
                        fieldsAsTuple
                    ]

            let fnBody = [ mkExprStmt body ] |> mkBlock |> Some
            let fnRetTy = Fable.String |> transformType com ctx |> mkFnRetTy
            let fnDecl = mkFnDecl [ mkImplSelfParam false false ] fnRetTy
            let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
            let attrs = []
            let fnItem = mkFnAssocItem attrs "get_Message" fnKind
            [ fnItem ]
        else
            []

    let makeDisplayTraitImpls com ctx self_ty genArgs hasToString =
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
                let ty =
                    mkGenericPathTy
                        [
                            "core"
                            "fmt"
                            "Formatter"
                        ]
                        None

                let p1 = mkImplSelfParam false false
                let p2 = mkParamFromType "f" (ty |> mkMutRefTy None) false false

                [
                    p1
                    p2
                ]

            let output =
                let ty =
                    mkGenericPathTy
                        [
                            "core"
                            "fmt"
                            rawIdent "Result"
                        ]
                        None

                ty |> mkFnRetTy

            mkFnDecl inputs output

        let fnKind = mkFnKind DEFAULT_FN_HEADER fnDecl NO_GENERICS fnBody
        let fnItem = mkFnAssocItem [] "fmt" fnKind
        let generics = makeGenerics com ctx genArgs

        let implItemFor traitName =
            let path =
                mkGenericPath
                    [
                        "core"
                        "fmt"
                        traitName
                    ]
                    None

            let ofTrait = mkTraitRef path |> Some
            mkImplItem [] "" self_ty generics [ fnItem ] ofTrait

        [
            // implItemFor "Debug"
            implItemFor "Display"
        ]

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
        entType
        self_ty
        genArgTys
        (decl: Fable.MemberDecl, memb: Fable.MemberFunctionOrValue)
        =
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

            let id_tokens =
                [
                    op_trait
                    op_fn
                    decl.Name
                ]
                |> List.map mkIdentToken

            let ty_tokens =
                (self_ty :: rhs_tys) @ genArgTys |> List.map mkTyToken

            let implItem =
                id_tokens @ ty_tokens
                |> mkParensCommaDelimitedMacCall macroName
                |> mkMacCallItem [] ""

            implItem
        )

    let withCurrentScope ctx (usedNames: Set<string>) f =
        let ctx =
            { ctx with
                UsedNames =
                    { ctx.UsedNames with
                        CurrentDeclarationScope = HashSet usedNames
                    }
            }

        let result = f ctx

        ctx.UsedNames.DeclarationScopes.UnionWith(
            ctx.UsedNames.CurrentDeclarationScope
        )

        result

    let makeMemberItem
        (com: IRustCompiler)
        ctx
        withVis
        (decl: Fable.MemberDecl, memb: Fable.MemberFunctionOrValue)
        =
        withCurrentScope ctx decl.UsedNames
        <| fun ctx ->
            let memberItem =
                transformAssocMember com ctx memb decl.Name decl.Args decl.Body

            if withVis then
                memberItem |> memberAssocItemWithVis com ctx memb
            else
                memberItem

    let makePrimaryConstructorItems
        com
        ctx
        (ent: Fable.Entity)
        (decl: Fable.ClassDecl)
        =
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
                    <| fun ctx -> transformPrimaryConstructor com ctx ent ctor
                | _ -> transformCompilerGeneratedConstructor com ctx ent

            [ ctorItem ]

    let makeInterfaceTraitImpls
        (com: IRustCompiler)
        ctx
        entName
        genArgs
        ifcEntRef
        memberItems
        =
        let genArgsOpt = transformGenArgs com ctx genArgs
        let traitBound = mkTypeTraitGenericBound [ entName ] genArgsOpt
        let ty = mkTraitTy [ traitBound ]
        let generics = makeGenerics com ctx genArgs

        let ifcEnt = com.GetEntity(ifcEntRef)
        let ifcFullName = getInterfaceImportName com ctx ifcEntRef
        let ifcGenArgs = FSharp2Fable.Util.getEntityGenArgs ifcEnt
        let ifcGenArgsOpt = transformGenArgs com ctx ifcGenArgs

        let path = makeFullNamePath ifcFullName ifcGenArgsOpt
        let ofTrait = mkTraitRef path |> Some
        let implItem = mkImplItem [] "" ty generics memberItems ofTrait
        [ implItem ]

    let objectMemberNames =
        set
            [
                "Equals"
                "GetHashCode"
                "GetType"
                "ToString"
            // "MemberwiseClone"
            // "ReferenceEquals"
            ]

    let ignoredInterfaceNames =
        set
            [
                Types.ienumerable
                Types.ienumerator
            ]

    let transformClassMembers
        (com: IRustCompiler)
        ctx
        (classDecl: Fable.ClassDecl)
        =
        let entRef = classDecl.Entity
        let ent = com.GetEntity(entRef)

        let entName =
            if ent.IsInterface then
                classDecl.Name // for interface object expressions
            else
                getEntityFullName com ctx entRef
            |> splitLast

        let entType = FSharp2Fable.Util.getEntityType ent
        let genArgs = FSharp2Fable.Util.getEntityGenArgs ent
        let self_ty = transformDeclaredType com ctx entRef genArgs
        let genArgTys = transformGenTypes com ctx genArgs

        let ctx = { ctx with ScopedEntityGenArgs = getEntityGenParamNames ent }

        // to filter out compiler-generated exception equality
        let isNotExceptionMember (_m: Fable.MemberFunctionOrValue) =
            not (ent.IsFSharpExceptionDeclaration)

        let isNonInterfaceMember (m: Fable.MemberFunctionOrValue) =
            not (
                ent.IsInterface || m.IsOverrideOrExplicitInterfaceImplementation
            )
            || m.IsConstructor
            || (Set.contains m.CompiledName objectMemberNames)

        let nonInterfaceMembers, interfaceMembers =
            classDecl.AttachedMembers
            |> List.map (fun decl -> decl, com.GetMember(decl.MemberRef))
            |> List.partition (snd >> isNonInterfaceMember)

        let nonInterfaceImpls =
            let memberItems =
                nonInterfaceMembers
                |> List.filter (snd >> isNotExceptionMember)
                |> List.map (makeMemberItem com ctx true)
                |> List.append (makeFSharpExceptionItems com ctx ent)
                |> List.append (
                    makePrimaryConstructorItems com ctx ent classDecl
                )

            if List.isEmpty memberItems then
                []
            else
                let generics = makeGenerics com ctx genArgs

                let implItem =
                    mkImplItem [] "" self_ty generics memberItems None

                [ implItem ]

        let nonInterfaceMemberNames =
            nonInterfaceMembers |> List.map (fun (d, m) -> d.Name) |> Set.ofList

        let displayTraitImpls =
            let hasToString = Set.contains "ToString" nonInterfaceMemberNames
            makeDisplayTraitImpls com ctx self_ty genArgs hasToString

        let operatorTraitImpls =
            nonInterfaceMembers
            |> List.choose (
                makeOpTraitImpls com ctx ent entType self_ty genArgTys
            )

        let interfaces =
            ent.AllInterfaces
            |> Seq.map (fun ifc ->
                ifc.Entity, ifc.Entity |> getInterfaceMemberNames com
            )
            |> Seq.filter (fun (ifcEntRef, _) ->
                // throws out anything on the ignored interfaces list
                not (Set.contains ifcEntRef.FullName ignoredInterfaceNames)
            )
            |> Seq.toList

        let interfaceTraitImpls =
            interfaces
            |> List.collect (fun (ifcEntRef, ifcMemberNames) ->
                let memberItems =
                    interfaceMembers
                    |> List.filter (fun (d, m) ->
                        //TODO: match the interface entity too, not just the member name
                        Set.contains d.Name ifcMemberNames
                    )
                    |> List.map (makeMemberItem com ctx false)

                if List.isEmpty memberItems then
                    []
                else
                    makeInterfaceTraitImpls
                        com
                        ctx
                        entName
                        genArgs
                        ifcEntRef
                        memberItems
            )

        nonInterfaceImpls
        @ displayTraitImpls
        @ operatorTraitImpls
        @ interfaceTraitImpls

    let transformClassDecl (com: IRustCompiler) ctx (decl: Fable.ClassDecl) =
        let ent = com.GetEntity(decl.Entity)

        if ent.IsFSharpAbbreviation then
            transformAbbrev com ctx ent
        elif ent.IsInterface then
            if isDeclaredInterface ent.FullName then
                []
            else
                transformInterface com ctx ent
        else
            let entityItem =
                if ent.IsFSharpUnion then
                    transformUnion com ctx ent
                else
                    transformClass com ctx ent
                |> entityItemWithVis com ctx ent

            let memberItems = transformClassMembers com ctx decl
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

    let memberItemWithVis
        com
        ctx
        (memb: Fable.MemberFunctionOrValue)
        memberItem
        =
        let isInternal, isPrivate =
            getVis com ctx memb.DeclaringEntity memb.IsInternal memb.IsPrivate

        memberItem |> mkItemWithVis isInternal isPrivate

    let memberAssocItemWithVis
        com
        ctx
        (memb: Fable.MemberFunctionOrValue)
        memberAssocItem
        =
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
                    | Fable.ModuleDeclaration d ->
                        Fable.ModuleDeclaration { d with Members = members }
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

                let importItems =
                    com.GetAllImports(ctx) |> transformImports com ctx

                com.ClearAllImports(ctx)
                useItem :: importItems

            let outerAttrs = transformAttributes com ctx ent.Attributes
            let innerAttrs = getInnerAttributes com ctx decl.Members
            let attrs = innerAttrs @ outerAttrs
            let modDecls = useDecls @ memberDecls
            let modItem = modDecls |> mkModItem attrs decl.Name
            let modItem = modItem |> entityItemWithVis com ctx ent
            [ modItem ]

    let transformMemberDecl (com: IRustCompiler) ctx (decl: Fable.MemberDecl) =
        let memb = com.GetMember(decl.MemberRef)

        let memberItem =
            if memb.IsValue then
                transformModuleLetValue com ctx memb decl
            else
                transformModuleFunction com ctx memb decl

        let memberItem = memberItem |> memberItemWithVis com ctx memb
        [ memberItem ]

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
            decls
            |> mergeNamespaceDecls com ctx
            |> List.collect (transformDecl com ctx)
        // wrap the last file in a module to consistently handle namespaces
        if isLastFileInProject com then
            let modPath = fixFileExtension com com.CurrentFile
            let modName = getImportModuleName com modPath
            let modItem = mkModItem [] modName items
            let useItem = mkGlobUseItem [] [ modName ]

            [
                modItem
                useItem |> mkPublicItem
            ]
        else
            items

    // F# hash function is unstable and gives different results in different runs
    // Taken from fable-library/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
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

    let transformImports
        (com: IRustCompiler)
        ctx
        (imports: Import list)
        : Rust.Item list
        =
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
                    "`importMember` must be assigned to a variable"
                    |> addError com [] r

                let isMacro = selector.EndsWith("!", StringComparison.Ordinal)
                let selector = selector |> Fable.Naming.replaceSuffix "!" ""
                let path = fixFileExtension self path

                let cacheKey =
                    let selector = selector.Replace(".", "::").Replace("`", "_")

                    if (isFableLibraryPath self path) then
                        "fable_library_rust::" + selector
                    elif path.Length = 0 then
                        selector
                    else
                        path + "::" + selector

                let import =
                    match imports.TryGetValue(cacheKey) with
                    | true, import ->
                        if
                            not (import.Depths |> List.contains ctx.ModuleDepth)
                        then
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
                        import.Value.Depths
                        |> List.filter (fun d -> d < ctx.ModuleDepth)

                    if import.Value.Depths.Length = 0 then
                        imports.Remove(import.Key) |> ignore

                        ctx.UsedNames.RootScope.Remove(import.Value.LocalIdent)
                        |> ignore

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
            member _.SourceFiles = com.SourceFiles
            member _.IncrementCounter() = com.IncrementCounter()

            member _.IsPrecompilingInlineFunction =
                com.IsPrecompilingInlineFunction

            member _.WillPrecompileInlineFunction(file) =
                com.WillPrecompileInlineFunction(file)

            member _.GetImplementationFile(fileName) =
                com.GetImplementationFile(fileName)

            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)

            member _.AddWatchDependency(fileName) =
                com.AddWatchDependency(fileName)

            member _.AddLog
                (
                    msg,
                    severity,
                    ?range,
                    ?fileName: string,
                    ?tag: string
                )
                =
                com.AddLog(
                    msg,
                    severity,
                    ?range = range,
                    ?fileName = fileName,
                    ?tag = tag
                )

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
                    // adds "no_std" for fable library crate if feature is enabled
                    if isFableLibrary com then
                        mkInnerAttr
                            "cfg_attr"
                            [
                                "feature = \"no_std\""
                                "no_std"
                            ]

                    // TODO: make some of those conditional on compiler options
                    mkInnerAttr "allow" [ "dead_code" ]
                    mkInnerAttr "allow" [ "non_camel_case_types" ]
                    mkInnerAttr "allow" [ "non_snake_case" ]
                    mkInnerAttr "allow" [ "non_upper_case_globals" ]
                    mkInnerAttr "allow" [ "unreachable_code" ]
                    mkInnerAttr "allow" [ "unused_attributes" ]
                    mkInnerAttr "allow" [ "unused_imports" ]
                    mkInnerAttr "allow" [ "unused_macros" ]
                    mkInnerAttr "allow" [ "unused_parens" ]
                    mkInnerAttr "allow" [ "unused_variables" ]

            // these require nightly
            // mkInnerAttr "feature" ["once_cell"]
            // mkInnerAttr "feature" ["stmt_expr_attributes"]
            // mkInnerAttr "feature" ["destructuring_assignment"]
            ]

        let entryPointItems = file.Declarations |> getEntryPointItems com ctx
        let importItems = com.GetAllImports(ctx) |> transformImports com ctx
        let declItems = file.Declarations |> transformDeclarations com ctx
        let modItems = getModuleItems com ctx // global module imports
        let nsItems = getNamespaceItems com ctx // global namespace imports

        let crateItems =
            importItems @ declItems @ modItems @ nsItems @ entryPointItems

        let innerAttrs = file.Declarations |> getInnerAttributes com ctx
        let crateAttrs = topAttrs @ innerAttrs
        let crate = mkCrate crateAttrs crateItems
        crate
