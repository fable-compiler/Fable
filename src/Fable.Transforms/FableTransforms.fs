module Fable.Transforms.FableTransforms

open Fable
open Fable.AST.Fable

let getSubExpressions = function
    | Unresolved _ -> []
    | IdentExpr _ -> []
    | TypeCast(e,_) -> [e]
    | Import _ -> []
    | Extended(kind, _) ->
        match kind with
        | Curry(e, _) -> [e]
        | Throw(e, _) -> Option.toList e
        | Debugger
        | RegionStart _ -> []
    | Value(kind,_) ->
        match kind with
        | ThisValue _ | BaseValue _
        | TypeInfo _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ -> []
        | StringTemplate(_,_,exprs) -> exprs
        | NewOption(e, _, _) -> Option.toList e
        | NewTuple(exprs, _) -> exprs
        | NewArray(kind, _, _) ->
            match kind with
            | ArrayValues exprs -> exprs
            | ArrayAlloc e
            | ArrayFrom e -> [e]
        | NewList(ht, _) ->
            match ht with Some(h,t) -> [h;t] | None -> []
        | NewRecord(exprs, _, _) -> exprs
        | NewAnonymousRecord(exprs, _, _) -> exprs
        | NewUnion(exprs, _, _, _) -> exprs
    | Test(e, _, _) -> [e]
    | Lambda(_, body, _) -> [body]
    | Delegate(_, body, _) -> [body]
    | ObjectExpr(members, _, baseCall) ->
        let members = members |> List.map (fun m -> m.Body)
        match baseCall with Some b -> b::members | None -> members
    | CurriedApply(callee, args, _, _) -> callee::args
    | Call(e1, info, _, _) -> e1 :: (Option.toList info.ThisArg) @ info.Args
    | Emit(info, _, _) -> (Option.toList info.CallInfo.ThisArg) @ info.CallInfo.Args
    | Operation(kind, _, _) ->
        match kind with
        | Unary(_, operand) -> [operand]
        | Binary(_, left, right) -> [left; right]
        | Logical(_, left, right) -> [left; right]
    | Get(e, kind, _, _) ->
        match kind with
        | ListHead | ListTail | OptionValue | TupleIndex _ | UnionTag
        | UnionField _ | FieldGet _ -> [e]
        | ExprGet e2 -> [e; e2]
    | Sequential exprs -> exprs
    | Let(_, value, body) -> [value; body]
    | LetRec(bs, body) -> (List.map snd bs) @ [body]
    | IfThenElse(cond, thenExpr, elseExpr, _) -> [cond; thenExpr; elseExpr]
    | Set(e, kind, _, v, _) ->
        match kind with
        | ExprSet e2 -> [e; e2; v]
        | FieldSet _ | ValueSet -> [e; v]
    | WhileLoop(e1, e2, _) -> [e1; e2]
    | ForLoop(_, e1, e2, e3, _, _) -> [e1; e2; e3]
    | TryCatch(body, catch, finalizer, _) ->
        match catch with
        | Some(_,c) -> body::c::(Option.toList finalizer)
        | None -> body::(Option.toList finalizer)
    | DecisionTree(expr, targets) -> expr::(List.map snd targets)
    | DecisionTreeSuccess(_, boundValues, _) -> boundValues

let deepExists (f: Expr -> bool) expr =
    let rec deepExistsInner (exprs: ResizeArray<Expr>) =
        let mutable found = false
        let subExprs = FSharp.Collections.ResizeArray()
        for e in exprs do
            if not found then
                subExprs.AddRange(getSubExpressions e)
                found <- f e
        if found then true
        elif subExprs.Count > 0 then deepExistsInner subExprs
        else false
    FSharp.Collections.ResizeArray [|expr|] |> deepExistsInner

let isIdentUsed identName expr =
    expr |> deepExists (function
        | IdentExpr i -> i.Name = identName
        | _ -> false)

let isIdentCaptured identName expr =
    let rec loop isClosure exprs =
        match exprs with
        | [] -> false
        | expr::restExprs ->
            match expr with
            | IdentExpr i when i.Name = identName -> isClosure
            | Lambda(_,body,_) -> loop true [body] || loop isClosure restExprs
            | Delegate(_,body,_) -> loop true [body] || loop isClosure restExprs
            | ObjectExpr(members, _, baseCall) ->
                let memberExprs = members |> List.map (fun m -> m.Body)
                loop true memberExprs || loop isClosure (Option.toList baseCall @ restExprs)
            | e ->
                let sub = getSubExpressions e
                loop isClosure (sub @ restExprs)
    loop false [expr]

let isTailRecursive identName expr =
    let mutable isTailRec = true
    let mutable isRecursive = false
    let rec loop inTailPos = function
        | CurriedApply(IdentExpr i, _, _, _)
        | Call(IdentExpr i, _, _, _) as e when i.Name = identName ->
            isRecursive <- true
            isTailRec <- isTailRec && inTailPos
            getSubExpressions e |> List.iter (loop false)
        | Sequential exprs ->
            let lastIndex = (List.length exprs) - 1
            exprs |> List.iteri (fun i e -> loop (i = lastIndex) e)
        | Let(_, value, body) ->
            loop false value
            loop inTailPos body
        | LetRec(bindings, body) ->
            List.map snd bindings |> List.iter (loop false)
            loop inTailPos body
        | IfThenElse(cond, thenExpr, elseExpr, _) ->
            loop false cond
            loop inTailPos thenExpr
            loop inTailPos elseExpr
        | DecisionTree(expr, targets) ->
            loop false expr
            List.map snd targets |> List.iter (loop inTailPos)
        | e ->
            getSubExpressions e |> List.iter (loop false)
    loop true expr
    isTailRec <- isTailRec && isRecursive
    isRecursive, isTailRec

let replaceValues replacements expr =
    if Map.isEmpty replacements
    then expr
    else expr |> visitFromInsideOut (function
        | IdentExpr id as e ->
            match Map.tryFind id.Name replacements with
            | Some e -> e
            | None -> e
        | e -> e)

let replaceNames replacements expr =
    if Map.isEmpty replacements
    then expr
    else expr |> visitFromInsideOut (function
        | IdentExpr id as e ->
            match Map.tryFind id.Name replacements with
            | Some name -> { id with Name=name } |> IdentExpr
            | None -> e
        | e -> e)

let countReferences limit identName body =
    let mutable count = 0
    body |> deepExists (function
        | IdentExpr id2 when id2.Name = identName ->
            count <- count + 1
            count > limit
        | _ -> false) |> ignore
    count

let noSideEffectBeforeIdent identName expr =
    let mutable sideEffect = false
    let orSideEffect found =
        if found then true
        else
            sideEffect <- true
            true

    let rec findIdentOrSideEffect = function
        | Unresolved _ -> false
        | IdentExpr id ->
            if id.Name = identName then true
            elif id.IsMutable then
                sideEffect <- true
                true
            else false
        // If the field is mutable we cannot inline, see #2683
        | Get(e, FieldGet info, _, _) ->
            if info.CanHaveSideEffects then
                sideEffect <- true
                true
            else findIdentOrSideEffect e
        // We don't have enough information here, so just assume there's a side effect just in case
        | Get(_, ExprGet _, _, _) ->
            sideEffect <- true
            true
        | Get(e, (TupleIndex _|UnionField _|UnionTag|ListHead|ListTail|OptionValue _), _, _) ->
            findIdentOrSideEffect e
        | Import _ | Lambda _ | Delegate _ -> false
        | Extended((Throw _|Debugger|RegionStart _),_) -> true
        | Extended(Curry(e,_),_) -> findIdentOrSideEffect e
        | CurriedApply(callee, args, _, _) ->
            callee::args |> findIdentOrSideEffectInList |> orSideEffect
        | Call(e1, info, _, _) ->
            match info.Tag, info.Args with
            // HACK: let beta reduction jump over keyValueList/createObj in Fable.React
            | Some "pojo", IdentExpr i::_ -> i.Name = identName
            | _ ->
                e1 :: (Option.toList info.ThisArg) @ info.Args
                |> findIdentOrSideEffectInList |> orSideEffect
        | Operation(kind, _, _) ->
            match kind with
            | Unary(_, operand) -> findIdentOrSideEffect operand
            | Binary(_, left, right)
            | Logical(_, left, right) -> findIdentOrSideEffect left || findIdentOrSideEffect right
        | Value(value,_) ->
            match value with
            | ThisValue _ | BaseValue _
            | TypeInfo _ | Null _ | UnitConstant | NumberConstant _
            | BoolConstant _ | CharConstant _ | StringConstant _ | RegexConstant _  -> false
            | NewList(None,_) | NewOption(None,_,_) -> false
            | NewOption(Some e,_,_) -> findIdentOrSideEffect e
            | NewList(Some(h,t),_) -> findIdentOrSideEffect h || findIdentOrSideEffect t
            | NewArray(kind,_,_) ->
                match kind with
                | ArrayValues exprs -> findIdentOrSideEffectInList exprs
                | ArrayAlloc e
                | ArrayFrom e -> findIdentOrSideEffect e
            | StringTemplate(_,_,exprs)
            | NewTuple(exprs,_)
            | NewUnion(exprs,_,_,_)
            | NewRecord(exprs,_,_)
            | NewAnonymousRecord(exprs,_,_) -> findIdentOrSideEffectInList exprs
        | Sequential exprs -> findIdentOrSideEffectInList exprs
        | Let(_,v,b) -> findIdentOrSideEffect v || findIdentOrSideEffect b
        | TypeCast(e,_)
        | Test(e,_,_) -> findIdentOrSideEffect e
        | IfThenElse(cond, thenExpr, elseExpr,_) ->
            findIdentOrSideEffect cond || findIdentOrSideEffect thenExpr || findIdentOrSideEffect elseExpr
        // TODO: Check member bodies in ObjectExpr
        | ObjectExpr _ | LetRec _ | Emit _ | Set _
        | DecisionTree _ | DecisionTreeSuccess _ // Check sub expressions here?
        | WhileLoop _ | ForLoop _ | TryCatch _ ->
            sideEffect <- true
            true

    and findIdentOrSideEffectInList exprs =
        (false, exprs) ||> List.fold (fun result e ->
            result || findIdentOrSideEffect e)

    findIdentOrSideEffect expr && not sideEffect

let canInlineArg _com identName value body =
    (canHaveSideEffects value |> not && countReferences 1 identName body <= 1)
     || (noSideEffectBeforeIdent identName body
         && isIdentCaptured identName body |> not
         // Make sure is at least referenced once so the expression is not erased
         && countReferences 1 identName body = 1)

/// Returns arity of lambda (or lambda option) types
let getLambdaTypeArity typ =
    let rec getLambdaTypeArity accArity accArgs = function
        | LambdaType(arg, returnType) ->
            getLambdaTypeArity (accArity + 1) (arg::accArgs) returnType
        | returnType ->
            let argTypes = List.rev accArgs
            let uncurried =
                match typ with
                | Option(_, isStruct) -> Option(DelegateType(argTypes, returnType), isStruct)
                | _ -> DelegateType(argTypes, returnType)
            accArity, uncurried
    match typ with
    | MaybeOption(LambdaType(arg, returnType)) ->
        getLambdaTypeArity 1 [arg] returnType
    | _ -> 0, typ

let tryUncurryType (typ: Type) =
    match getLambdaTypeArity typ with
    | arity, uncurriedType when arity > 1 -> Some(arity, uncurriedType)
    | _ -> None

let uncurryType (typ: Type) =
    match tryUncurryType typ with
    | Some(_arity, uncurriedType) -> uncurriedType
    | None -> typ

let rec uncurryLambdaType (revArgTypes: Type list) (returnType: Type) =
    match returnType with
    | LambdaType(paramType, returnType) -> uncurryLambdaType (paramType::revArgTypes) returnType
    | t -> List.rev revArgTypes, t

module private Transforms =
    let (|LambdaOrDelegate|_|) = function
        | Lambda(arg, body, info) -> Some([arg], body, info)
        | Delegate(args, body, info) -> Some(args, body, info)
        | _ -> None

    let (|ImmediatelyApplicable|_|) = function
        | Lambda(arg, body, _) -> Some(arg, body)
        // If the lambda is immediately applied we don't need the closures
        | NestedRevLets(bindings, Lambda(arg, body, _)) ->
            let body = List.fold (fun body (i,v) -> Let(i, v, body)) body bindings
            Some(arg, body)
        | _ -> None

    let lambdaBetaReduction (com: Compiler) e =
        let applyArgs (args: Ident list) argExprs body =
            let bindings, replacements =
                (([], Map.empty), args, argExprs)
                |||> List.fold2 (fun (bindings, replacements) ident expr ->
                    if canInlineArg com ident.Name expr body
                    then bindings, Map.add ident.Name expr replacements
                    else (ident, expr)::bindings, replacements)
            match bindings with
            | [] -> replaceValues replacements body
            | bindings ->
                let body = replaceValues replacements body
                bindings |> List.fold (fun body (i, v) -> Let(i, v, body)) body
        match e with
        // TODO: Other binary operations and numeric types
        | Operation(Binary(AST.BinaryPlus, v1, v2), _, _) ->
            match v1, v2 with
            | Value(StringConstant v1, r1), Value(StringConstant v2, r2) ->
                Value(StringConstant(v1 + v2), addRanges [r1; r2])
            // Assume NumberKind and NumberInfo are the same
            | Value(NumberConstant(:? int as v1, AST.Int32, NumberInfo.Empty), r1), Value(NumberConstant(:? int as v2, AST.Int32, NumberInfo.Empty), r2) ->
                Value(NumberConstant(v1 + v2, AST.Int32, NumberInfo.Empty), addRanges [r1; r2])
            | _ -> e
        | Call(Delegate(args, body, _), info, _, _) when List.sameLength args info.Args ->
            applyArgs args info.Args body
        | CurriedApply(applied, argExprs, t, r) ->
            let rec tryImmediateApplication r t applied argExprs =
                match argExprs with
                | [] -> applied
                | argExpr::restArgs ->
                    match applied with
                    | ImmediatelyApplicable(arg, body) ->
                        let applied = applyArgs [arg] [argExpr] body
                        tryImmediateApplication r t applied restArgs
                    | _ -> CurriedApply(applied, argExprs, t, r)
            tryImmediateApplication r t applied argExprs
        | e -> e

    let bindingBetaReduction (com: Compiler) e =
        // Don't erase user-declared bindings in debug mode for better output
        let isErasingCandidate (ident: Ident) =
            (not com.Options.DebugMode) || ident.IsCompilerGenerated
        match e with
        | Let(ident, value, letBody) when (not ident.IsMutable) && isErasingCandidate ident ->
            let canEraseBinding =
                match value with
                | Import(i,_,_) -> i.IsCompilerGenerated
                // Don't move local functions declared by user
                | Lambda _ -> ident.IsCompilerGenerated && canInlineArg com ident.Name value letBody
//                | NestedLambda lambdaBody ->
//                    match lambdaBody with
//                    | Import(i,_,_) -> i.IsCompilerGenerated
//                    // Check the lambda doesn't reference itself recursively
//                    | _ -> countReferences 0 ident.Name lambdaBody = 0
//                           && canInlineArg com ident.Name value letBody
                | _ -> canInlineArg com ident.Name value letBody
            if canEraseBinding then
                let value =
                    match value with
                    // Ident becomes the name of the function (mainly used for tail call optimizations)
                    | Lambda(arg, funBody, info) -> Lambda(arg, funBody, { info with Name = Some ident.Name })
                    | Delegate(args, funBody, info) -> Delegate(args, funBody, { info with Name = Some ident.Name })
                    | value -> value
                replaceValues (Map [ident.Name, value]) letBody
            else e
        | e -> e

    let operationReduction (_com: Compiler) e =
        match e with
        // TODO: Other binary operations and numeric types
        | Operation(Binary(AST.BinaryPlus, v1, v2), _, _) ->
            match v1, v2 with
            | Value(StringConstant v1, r1), Value(StringConstant v2, r2) ->
                Value(StringConstant(v1 + v2), addRanges [r1; r2])
            // Assume NumberKind and NumberInfo are the same
            | Value(NumberConstant(:? int as v1, AST.Int32, NumberInfo.Empty), r1), Value(NumberConstant(:? int as v2, AST.Int32, NumberInfo.Empty), r2) ->
                Value(NumberConstant(v1 + v2, AST.Int32, NumberInfo.Empty), addRanges [r1; r2])
            | _ -> e

        | Operation(Logical(AST.LogicalAnd, (Value(BoolConstant b, _) as v1), v2), _, _) -> if b then v2 else v1
        | Operation(Logical(AST.LogicalAnd, v1, (Value(BoolConstant b, _) as v2)), _, _) -> if b then v1 else v2
        | Operation(Logical(AST.LogicalOr, (Value(BoolConstant b, _) as v1), v2), _, _) -> if b then v1 else v2
        | Operation(Logical(AST.LogicalOr, v1, (Value(BoolConstant b, _) as v2)), _, _) -> if b then v2 else v1

        | IfThenElse(Value(BoolConstant b, _), thenExpr, elseExpr, _) -> if b then thenExpr else elseExpr

        | _ -> e

    let curryIdentsInBody replacements body =
        visitFromInsideOut (function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some arity -> Extended(Curry(e, arity), e.Range)
                | None -> e
            | e -> e) body

    let curryArgIdentsAndReplaceInBody (args: Ident list) body =
        let replacements, args =
            ((Map.empty, []), args) ||> List.fold (fun (replacements, uncurriedArgs) arg ->
                match tryUncurryType arg.Type with
                | Some(arity, uncurriedType) ->
                    Map.add arg.Name arity replacements, { arg with Type = uncurriedType}::uncurriedArgs
                | None ->
                    replacements, arg::uncurriedArgs)
        if Map.isEmpty replacements
        then List.rev args, body
        else List.rev args, curryIdentsInBody replacements body

    let uncurryExpr com t arity expr =
        let matches arity arity2 =
            match arity with
            // TODO: check cases where arity <> arity2
            | Some arity -> arity = arity2
            // Remove currying for dynamic operations (no arity)
            | None -> true
        match expr, arity with
        | MaybeCasted(LambdaUncurriedAtCompileTime arity lambda), _ -> lambda
        | Extended(Curry(innerExpr, arity2),_), _
            when matches arity arity2 -> innerExpr
        | Get(Extended(Curry(innerExpr, arity2),_), OptionValue, t, r), _
            when matches arity arity2 -> Get(innerExpr, OptionValue, t, r)
        | Value(NewOption(Some(Extended(Curry(innerExpr, arity2),_)), t, isStruct), r), _
            when matches arity arity2 -> Value(NewOption(Some(innerExpr), t, isStruct), r)
        | _, Some arity -> Replacements.Api.uncurryExprAtRuntime com t arity expr
        | _, None -> expr

    let uncurryArgs com autoUncurrying argTypes args =
        let mapArgs f argTypes args =
            let rec mapArgsInner f acc argTypes args =
                match argTypes, args with
                | head1::tail1, head2::tail2 ->
                    let x = f head1 head2
                    mapArgsInner f (x::acc) tail1 tail2
                | [], head2::tail2 when autoUncurrying ->
                    let x = f Any head2
                    mapArgsInner f (x::acc) [] tail2
                | [], args2 -> (List.rev acc)@args2
                | _, [] -> List.rev acc
            mapArgsInner f [] argTypes args
        (argTypes, args) ||> mapArgs (fun expectedType arg ->
            match expectedType with
            | Any when autoUncurrying -> uncurryExpr com Any None arg
            | _ ->
                match getLambdaTypeArity expectedType with
                | arity, uncurriedType when arity > 1 ->
                    uncurryExpr com uncurriedType (Some arity) arg
                | _ -> arg)

    let uncurryInnerFunctions (_: Compiler) e =
        let curryIdentInBody identName (args: Ident list) body =
            curryIdentsInBody (Map [identName, List.length args]) body
        match e with
        | Let(ident, NestedLambdaWithSameArity(args, fnBody, _), letBody) when List.isMultiple args
                                                                          && not ident.IsMutable ->
            let fnBody = curryIdentInBody ident.Name args fnBody
            let letBody = curryIdentInBody ident.Name args letBody
            Let(ident, Delegate(args, fnBody, FuncInfo.Empty), letBody)
        // Anonymous lambda immediately applied
        | CurriedApply(NestedLambdaWithSameArity(args, fnBody, { Name = Some name }), argExprs, t, r)
                        when List.isMultiple args && List.sameLength args argExprs ->
            let fnBody = curryIdentInBody name args fnBody
            let info = makeCallInfo None argExprs (args |> List.map (fun a -> a.Type))
            Delegate(args, fnBody, FuncInfo.Create(name=name))
            |> makeCall r t info
        | e -> e

    let propagateCurryingThroughLets (_: Compiler) = function
        | Let(ident, value, body) when not ident.IsMutable ->
            let ident, value, arity =
                match value with
                | Extended(Curry(innerExpr, arity),_) ->
                    ident, innerExpr, Some arity
                | Get(Extended(Curry(innerExpr, arity),_), OptionValue, t, r) ->
                    ident, Get(innerExpr, OptionValue, t, r), Some arity
                | Value(NewOption(Some(Extended(Curry(innerExpr, arity),_)), t, isStruct), r) ->
                    ident, Value(NewOption(Some(innerExpr), t, isStruct), r), Some arity
                | _ -> ident, value, None
            match arity with
            | None -> Let(ident, value, body)
            | Some arity ->
                let replacements = Map [ident.Name, arity]
                Let(ident, value, curryIdentsInBody replacements body)
        | e -> e

    let uncurryMemberArgs (m: MemberDecl) =
        if m.Info.IsValue then m
        else
            let args, body = curryArgIdentsAndReplaceInBody m.ArgIdents m.Body
            let args = List.zip m.Args args |> List.map (fun (d, i) -> { d with Ident = i })
            { m with Args = args; Body = body }

    let (|GetField|_|) (com: Compiler) = function
        | Get(callee, kind, _, r) ->
            match kind with
            | FieldGet { FieldType = Some fieldType } -> Some(callee, fieldType, r)
            | UnionField info ->
                let e = com.GetEntity(info.Entity)
                List.tryItem info.CaseIndex e.UnionCases
                |> Option.bind (fun c -> List.tryItem info.FieldIndex c.UnionCaseFields)
                |> Option.map (fun f -> callee, f.FieldType, r)
            | _ -> None
        | _ -> None

    let curryReceivedArgs (com: Compiler) e =
        match e with
        // Args passed to a lambda are not uncurried, as it's difficult to do it right, see #2657
        // | Lambda(arg, body, name)
        | Delegate(args, body, name) ->
            let args, body = curryArgIdentsAndReplaceInBody args body
            Delegate(args, body, name)
        // Uncurry also values received from getters
        | GetField com (callee, fieldType, r) ->
            match getLambdaTypeArity fieldType, callee.Type with
            // For anonymous records, if the lambda returns a generic the actual
            // arity may be higher than expected, so we need a runtime partial application
            | (arity, MaybeOption(DelegateType(_, GenericParam _))), AnonymousRecordType _ when arity > 0 ->
                let e = Replacements.Api.checkArity com fieldType arity e
                if arity > 1 then Extended(Curry(e, arity), r)
                else e
            | (arity, _), _ when arity > 1 -> Extended(Curry(e, arity), r)
            | _ -> e
        | ObjectExpr(members, t, baseCall) ->
            ObjectExpr(List.map uncurryMemberArgs members, t, baseCall)
        | e -> e

    let uncurrySendingArgs (com: Compiler) e =
        let uncurryConsArgs args (fields: seq<Field>) =
            let argTypes =
                fields
                |> Seq.map (fun fi -> fi.FieldType)
                |> Seq.toList
            uncurryArgs com false argTypes args
        match e with
        | Call(callee, info, t, r) ->
            let args = uncurryArgs com false info.SignatureArgTypes info.Args
            let info = { info with Args = args }
            Call(callee, info, t, r)
        | Emit({ CallInfo = callInfo } as emitInfo, t, r) ->
            let args = uncurryArgs com true callInfo.SignatureArgTypes callInfo.Args
            Emit({ emitInfo with CallInfo = { callInfo with Args = args } }, t, r)
        // Uncurry also values in setters or new record/union/tuple
        | Value(NewRecord(args, ent, genArgs), r) ->
            let args = com.GetEntity(ent).FSharpFields |> uncurryConsArgs args
            Value(NewRecord(args, ent, genArgs), r)
        | Value(NewAnonymousRecord(args, fieldNames, genArgs), r) ->
            let args = uncurryArgs com false genArgs args
            Value(NewAnonymousRecord(args, fieldNames, genArgs), r)
        | Value(NewUnion(args, tag, ent, genArgs), r) ->
            let uci = com.GetEntity(ent).UnionCases[tag]
            let args = uncurryConsArgs args uci.UnionCaseFields
            Value(NewUnion(args, tag, ent, genArgs), r)
        | Set(e, FieldSet(fieldName), t, value, r) ->
            let value = uncurryArgs com false [t] [value]
            Set(e, FieldSet(fieldName), t, List.head value, r)
        | ObjectExpr(members, t, baseCall) ->
            let membersMap =
                match t with
                | DeclaredType(e, _genArgs) ->
                    com.GetEntity(e).MembersFunctionsAndValues
                    |> Seq.choose (fun m ->
                        if m.IsGetter || m.IsValue then
                            Some(m.CompiledName, m.ReturnParameter.Type)
                        else None)
                    |> Map
                | _ -> Map.empty
            let members =
                members |> List.map (fun m ->
                    let hasGenerics = m.Body.Type.Generics |> List.isEmpty |> not
                    if m.Info.IsGetter || (m.Info.IsValue && not hasGenerics) then
                        let membType =
                            Map.tryFind m.Name membersMap
                            |> Option.defaultValue m.Body.Type
                        let value = uncurryArgs com false [membType] [m.Body]
                        { m with Body = List.head value }
                    else m)
            ObjectExpr(members, t, baseCall)
        | e -> e

    let rec uncurryApplications (com: Compiler) e =
        let uncurryApply r t applied args uncurriedArity =
            let argsLen = List.length args
            if uncurriedArity = argsLen then
                // This is already uncurried we don't need the signature arg types anymore,
                // just make a normal call
                let info = makeCallInfo None args []
                makeCall r t info applied |> Some
            elif uncurriedArity < argsLen then
                let appliedArgs, restArgs = List.splitAt uncurriedArity args
                let info = makeCallInfo None appliedArgs []
                let intermetiateType =
                    match List.rev restArgs with
                    | [] -> Any
                    | arg::args -> (LambdaType(arg.Type, t), args) ||> List.fold (fun t a -> LambdaType(a.Type, t))
                let applied = makeCall None intermetiateType info applied
                CurriedApply(applied, restArgs, t, r) |> Some
            else
                Replacements.Api.partialApplyAtRuntime com t (uncurriedArity - argsLen) applied args |> Some
        match e with
        | NestedApply(applied, args, t, r) ->
            let applied = visitFromOutsideIn (uncurryApplications com) applied
            let args = args |> List.map (visitFromOutsideIn (uncurryApplications com))
            match applied with
            | Extended(Curry(applied, uncurriedArity),_) ->
                uncurryApply r t applied args uncurriedArity
            | Get(Extended(Curry(applied, uncurriedArity),_), OptionValue, t2, r2) ->
                uncurryApply r t (Get(applied, OptionValue, t2, r2)) args uncurriedArity
            | _ -> CurriedApply(applied, args, t, r) |> Some
        | _ -> None

open Transforms

// ATTENTION: Order of transforms matters
let getTransformations (_com: Compiler) =
    [ // First apply beta reduction
      fun com e -> visitFromInsideOut (bindingBetaReduction com) e
      fun com e -> visitFromInsideOut (lambdaBetaReduction com) e
      // Make an extra binding reduction pass after applying lambdas
      fun com e -> visitFromInsideOut (bindingBetaReduction com) e
      fun com e -> visitFromInsideOut (operationReduction com) e
      // Then apply uncurry optimizations
      // Functions passed as arguments in calls (but NOT in curried applications) are being uncurried so we have to re-curry them
      // The next steps will uncurry them again if they're immediately applied or passed again as call arguments
      fun com e -> visitFromInsideOut (curryReceivedArgs com) e
      fun com e -> visitFromInsideOut (uncurryInnerFunctions com) e
      fun com e -> visitFromInsideOut (propagateCurryingThroughLets com) e
      fun com e -> visitFromInsideOut (uncurrySendingArgs com) e
      // uncurryApplications must come after uncurrySendingArgs as it erases argument type info
      fun com e -> visitFromOutsideIn (uncurryApplications com) e
    ]

let rec transformDeclaration transformations (com: Compiler) file decl =
    let transformExpr (com: Compiler) e =
        List.fold (fun e f -> f com e) e transformations

    let transformMemberBody com (m: MemberDecl) =
        { m with Body = transformExpr com m.Body }

    match decl with
    | ModuleDeclaration decl ->
        let members =
            decl.Members
            |> List.map (transformDeclaration transformations com file)
        { decl with Members = members }
        |> ModuleDeclaration

    | ActionDeclaration decl ->
        { decl with Body = transformExpr com decl.Body }
        |> ActionDeclaration

    | MemberDeclaration m ->
        m
        |> uncurryMemberArgs
        |> transformMemberBody com
        |> fun m -> com.ApplyMemberDeclarationPlugin(file, m)
        |> MemberDeclaration

    | ClassDeclaration decl ->
        // (ent, ident, cons, baseCall, attachedMembers)
        let attachedMembers =
            decl.AttachedMembers
            |> List.map (uncurryMemberArgs >> transformMemberBody com)

        let cons, baseCall =
            match decl.Constructor, decl.BaseCall with
            | None, _ -> None, None
            | Some cons, None ->
                uncurryMemberArgs cons |> transformMemberBody com |> Some, None
            | Some cons, Some baseCall ->
                // In order to uncurry correctly the baseCall arguments,
                // we need to include it in the constructor body
                let args, body =
                    Sequential [baseCall; cons.Body]
                    |> curryArgIdentsAndReplaceInBody cons.ArgIdents
                let args = List.zip cons.Args args |> List.map (fun (d, i) -> { d with Ident = i })
                transformExpr com body
                |> function
                    | Sequential [baseCall; body] -> Some { cons with Args = args; Body = body }, Some baseCall
                    | body -> Some { cons with Args = args; Body = body }, None // Unexpected, raise error?

        { decl with Constructor = cons
                    BaseCall = baseCall
                    AttachedMembers = attachedMembers }
        |> ClassDeclaration

let transformFile (com: Compiler) (file: File) =
    let transformations = getTransformations com
    let newDecls = List.map (transformDeclaration transformations com file) file.Declarations
    File(newDecls, usedRootNames=file.UsedNamesInRootScope)
