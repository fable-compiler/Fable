module Fable.Transforms.FableTransforms

open Fable
open Fable.AST.Fable

let visit f e =
    match e with
    | IdentExpr _ -> e
    | TypeCast(e, t, tag) -> TypeCast(f e, t, tag)
    | Import(info, t, r) ->
        Import({ info with Selector = info.Selector
                           Path = info.Path }, t, r)
    | NativeInstruction(kind, r) ->
        match kind with
        | Throw(e, t) -> NativeInstruction(Throw(f e, t), r)
        | Break _
        | Debugger -> e
    | Value(kind, r) ->
        match kind with
        | ThisValue _ | BaseValue _
        | TypeInfo _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ -> e
        | EnumConstant(exp, ent) -> EnumConstant(f exp, ent) |> makeValue r
        | NewOption(e, t, isStruct) -> NewOption(Option.map f e, t, isStruct) |> makeValue r
        | NewTuple(exprs, isStruct) -> NewTuple(List.map f exprs, isStruct) |> makeValue r
        | NewArray(exprs, t) -> NewArray(List.map f exprs, t) |> makeValue r
        | NewArrayFrom(e, t) -> NewArrayFrom(f e, t) |> makeValue r
        | NewList(ht, t) ->
            let ht = ht |> Option.map (fun (h,t) -> f h, f t)
            NewList(ht, t) |> makeValue r
        | NewRecord(exprs, ent, genArgs) ->
            NewRecord(List.map f exprs, ent, genArgs) |> makeValue r
        | NewAnonymousRecord(exprs, ent, genArgs) ->
            NewAnonymousRecord(List.map f exprs, ent, genArgs) |> makeValue r
        | NewUnion(exprs, uci, ent, genArgs) ->
            NewUnion(List.map f exprs, uci, ent, genArgs) |> makeValue r
    | Test(e, kind, r) -> Test(f e, kind, r)
    | Curry(e, arity) -> Curry(f e, arity)
    | Lambda(arg, body, name) -> Lambda(arg, f body, name)
    | Delegate(args, body, name) -> Delegate(args, f body, name)
    | ObjectExpr(members, t, baseCall) ->
        let baseCall = Option.map f baseCall
        let members = members |> List.map (fun m -> { m with Body = f m.Body })
        ObjectExpr(members, t, baseCall)
    | CurriedApply(callee, args, t, r) ->
        CurriedApply(f callee, List.map f args, t, r)
    | Call(callee, info, t, r) ->
        let info = { info with ThisArg = Option.map f info.ThisArg
                               Args = List.map f info.Args }
        Call(f callee, info, t, r)
    | Emit(info, t, r) ->
        let callInfo =
            { info.CallInfo with ThisArg = Option.map f info.CallInfo.ThisArg
                                 Args = List.map f info.CallInfo.Args }
        Emit({ info with CallInfo = callInfo }, t, r)
    | Operation(kind, t, r) ->
        match kind with
        | Unary(operator, operand) ->
            Operation(Unary(operator, f operand), t, r)
        | Binary(op, left, right) ->
            Operation(Binary(op, f left, f right), t, r)
        | Logical(op, left, right) ->
            Operation(Logical(op, f left, f right), t, r)
    | Get(e, kind, t, r) ->
        match kind with
        | ListHead | ListTail | OptionValue | TupleIndex _ | UnionTag
        | UnionField _ | FieldGet _ -> Get(f e, kind, t, r)
        | ExprGet e2 -> Get(f e, ExprGet(f e2), t, r)
    | Sequential exprs -> Sequential(List.map f exprs)
    | Let(ident, value, body) -> Let(ident, f value, f body)
    | LetRec(bs, body) ->
        let bs = bs |> List.map (fun (i,e) -> i, f e)
        LetRec(bs, f body)
    | IfThenElse(cond, thenExpr, elseExpr, r) ->
        IfThenElse(f cond, f thenExpr, f elseExpr, r)
    | Set(e, kind, t, v, r) ->
        match kind with
        | ExprSet e2 -> Set(f e, ExprSet(f e2), t, f v, r)
        | FieldSet _ | ValueSet -> Set(f e, kind, t, f v, r)
    | WhileLoop(e1, e2, label, r) -> WhileLoop(f e1, f e2, label, r)
    | ForLoop(i, e1, e2, e3, up, r) -> ForLoop(i, f e1, f e2, f e3, up, r)
    | TryCatch(body, catch, finalizer, r) ->
        TryCatch(f body,
                 Option.map (fun (i, e) -> i, f e) catch,
                 Option.map f finalizer, r)
    | DecisionTree(expr, targets) ->
        let targets = targets |> List.map (fun (idents, v) -> idents, f v)
        DecisionTree(f expr, targets)
    | DecisionTreeSuccess(idx, boundValues, t) ->
        DecisionTreeSuccess(idx, List.map f boundValues, t)

let rec visitFromInsideOut f e =
    visit (visitFromInsideOut f) e |> f

let rec visitFromOutsideIn (f: Expr->Expr option) e =
    match f e with
    | Some e -> e
    | None -> visit (visitFromOutsideIn f) e

let getSubExpressions = function
    | IdentExpr _ -> []
    | TypeCast(e,_,_) -> [e]
    | Import(_,_,_) -> []
    | NativeInstruction(kind, r) ->
        match kind with
        | Throw(e, _) -> [e]
        | Break _
        | Debugger -> []
    | Value(kind,_) ->
        match kind with
        | ThisValue _ | BaseValue _
        | TypeInfo _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ -> []
        | EnumConstant(e, _) -> [e]
        | NewOption(e, _, _) -> Option.toList e
        | NewTuple(exprs, _) -> exprs
        | NewArray(exprs, _) -> exprs
        | NewArrayFrom(e, _) -> [e]
        | NewList(ht, _) ->
            match ht with Some(h,t) -> [h;t] | None -> []
        | NewRecord(exprs, _, _) -> exprs
        | NewAnonymousRecord(exprs, _, _) -> exprs
        | NewUnion(exprs, _, _, _) -> exprs
    | Test(e, _, _) -> [e]
    | Curry(e, _) -> [e]
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
    | WhileLoop(e1, e2, _, _) -> [e1; e2]
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
        let subExprs = ResizeArray()
        for e in exprs do
            if not found then
                subExprs.AddRange(getSubExpressions e)
                found <- f e
        if found then true
        elif subExprs.Count > 0 then deepExistsInner subExprs
        else false
    ResizeArray [|expr|] |> deepExistsInner

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
        | IdentExpr id ->
            if id.Name = identName then true
            elif id.IsMutable then
                sideEffect <- true
                true
            else false
        | Import _ | Lambda _ | Delegate _ -> false
        | NativeInstruction((Throw _|Break _|Debugger),_) -> true
        // HACK: let beta reduction jump over keyValueList/createObj in Fable.React
        | TypeCast(Call(_,i,_,_),_,Some "optimizable:pojo") ->
            match i.Args with
            | IdentExpr i::_ -> i.Name = identName
            | _ -> false
        | CurriedApply(callee, args, _, _) ->
            callee::args |> findIdentOrSideEffectInList |> orSideEffect
        | Call(e1, info, _, _) ->
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
            | TypeInfo _ | Null _ | UnitConstant | NumberConstant _ | BoolConstant _
            | CharConstant _ | StringConstant _ | RegexConstant _  -> false
            | EnumConstant(e, _) -> findIdentOrSideEffect e
            | NewList(None,_) | NewOption(None,_,_) -> false
            | NewArrayFrom(e,_)
            | NewOption(Some e,_,_) -> findIdentOrSideEffect e
            | NewList(Some(h,t),_) -> findIdentOrSideEffect h || findIdentOrSideEffect t
            | NewArray(exprs,_)
            | NewTuple(exprs,_)
            | NewUnion(exprs,_,_,_)
            | NewRecord(exprs,_,_)
            | NewAnonymousRecord(exprs,_,_) -> findIdentOrSideEffectInList exprs
        | Sequential exprs -> findIdentOrSideEffectInList exprs
        | Let(_,v,b) -> findIdentOrSideEffect v || findIdentOrSideEffect b
        | TypeCast(e,_,_)
        | Get(e,_,_,_)
        | Test(e,_,_)
        | Curry(e,_) -> findIdentOrSideEffect e
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


let canInlineArg identName value body =
    (canHaveSideEffects value |> not && countReferences 1 identName body <= 1)
     || (noSideEffectBeforeIdent identName body
         && isIdentCaptured identName body |> not
         // Make sure is at least referenced once so the expression is not erased
         && countReferences 1 identName body = 1)

module private Transforms =
    let (|LambdaOrDelegate|_|) = function
        | Lambda(arg, body, name) -> Some([arg], body, name)
        | Delegate(args, body, name) -> Some(args, body, name)
        | _ -> None

    let (|ImmediatelyApplicable|_|) = function
        | Lambda(arg, body, _) -> Some(arg, body)
        // If the lambda is immediately applied we don't need the closures
        | NestedRevLets(bindings, Lambda(arg, body, _)) ->
            let body = List.fold (fun body (i,v) -> Let(i, v, body)) body bindings
            Some(arg, body)
        | _ -> None

    let lambdaBetaReduction (_com: Compiler) e =
        let applyArgs (args: Ident list) argExprs body =
            let bindings, replacements =
                (([], Map.empty), args, argExprs)
                |||> List.fold2 (fun (bindings, replacements) ident expr ->
                    if canInlineArg ident.Name expr body
                    then bindings, Map.add ident.Name expr replacements
                    else (ident, expr)::bindings, replacements)
            match bindings with
            | [] -> replaceValues replacements body
            | bindings ->
                let body = replaceValues replacements body
                bindings |> List.fold (fun body (i, v) -> Let(i, v, body)) body
        match e with
        // TODO: Other binary operations and numeric types, also recursive?
        | Operation(Binary(AST.BinaryPlus, Value(StringConstant str1, r1), Value(StringConstant str2, r2)),_,_) ->
            Value(StringConstant(str1 + str2), addRanges [r1; r2])
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
                | NestedLambda(_, lambdaBody, _) ->
                    match lambdaBody with
                    | Import(i,_,_) -> i.IsCompilerGenerated
                    // Check the lambda doesn't reference itself recursively
                    | _ -> countReferences 0 ident.Name lambdaBody = 0
                           && canInlineArg ident.Name value letBody
                | _ -> canInlineArg ident.Name value letBody
            if canEraseBinding then
                let value =
                    match value with
                    // Ident becomes the name of the function (mainly used for tail call optimizations)
                    | Lambda(arg, funBody, _) -> Lambda(arg, funBody, Some ident.Name)
                    | Delegate(args, funBody, _) -> Delegate(args, funBody, Some ident.Name)
                    | value -> value
                replaceValues (Map [ident.Name, value]) letBody
            else e
        | e -> e

    /// Returns arity of lambda (or lambda option) types
    let getLambdaTypeArity t =
        let rec getLambdaTypeArity acc = function
            | LambdaType(_, returnType) ->
                getLambdaTypeArity (acc + 1) returnType
            | t -> acc, t
        match t with
        | LambdaType(_, returnType)
        | Option(LambdaType(_, returnType),_) ->
            getLambdaTypeArity 1 returnType
        | _ -> 0, t

    let curryIdentsInBody replacements body =
        visitFromInsideOut (function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some arity -> Curry(e, arity)
                | None -> e
            | e -> e) body

    let uncurryIdentsAndReplaceInBody (idents: Ident list) body =
        let replacements =
            (Map.empty, idents) ||> List.fold (fun replacements id ->
                let arity, _ = getLambdaTypeArity id.Type
                if arity > 1
                then Map.add id.Name arity replacements
                else replacements)
        if Map.isEmpty replacements
        then body
        else curryIdentsInBody replacements body

    let uncurryExpr com arity expr =
        let matches arity arity2 =
            match arity with
            // TODO: check cases where arity <> arity2
            | Some arity -> arity = arity2
            // Remove currying for dynamic operations (no arity)
            | None -> true
        match expr, expr with
        | MaybeCasted(LambdaUncurriedAtCompileTime arity lambda), _ -> lambda
        | _, Curry(innerExpr, arity2)
            when matches arity arity2 -> innerExpr
        | _, Get(Curry(innerExpr, arity2), OptionValue, t, r)
            when matches arity arity2 -> Get(innerExpr, OptionValue, t, r)
        | _, Value(NewOption(Some(Curry(innerExpr, arity2)), t, isStruct), r)
            when matches arity arity2 -> Value(NewOption(Some(innerExpr), t, isStruct), r)
        | _ ->
            match arity with
            | Some arity -> Replacements.uncurryExprAtRuntime com arity expr
            | None -> expr

    // For function arguments check if the arity of their own function arguments is expected or not
    // TODO: Do we need to do this recursively, and check options and delegates too?
    let checkSubArguments com expectedType (expr: Expr) =
        match expectedType, expr with
        | NestedLambdaType(expectedArgs,_), ExprType(NestedLambdaType(actualArgs,_)) ->
            let expectedLength = List.length expectedArgs
            if List.length actualArgs < expectedLength then expr
            else
                let actualArgs = List.truncate expectedLength actualArgs
                let _, replacements =
                    ((0, Map.empty), expectedArgs, actualArgs)
                    |||> List.fold2 (fun (index, replacements) expected actual ->
                        match expected, actual with
                        | GenericParam _, NestedLambdaType(args2, _) when List.isMultiple args2 ->
                            index + 1, Map.add index (0, List.length args2) replacements
                        | NestedLambdaType(args1, _), NestedLambdaType(args2, _)
                                when not(List.sameLength args1 args2) ->
                            let expectedArity = List.length args1
                            let actualArity = List.length args2
                            index + 1, Map.add index (expectedArity, actualArity) replacements
                        | _ -> index + 1, replacements)
                if Map.isEmpty replacements then expr
                else
                    let mappings =
                        actualArgs |> List.mapi (fun i _ ->
                            match Map.tryFind i replacements with
                            | Some (expectedArity, actualArity) ->
                                makeTuple None [makeIntConst expectedArity; makeIntConst actualArity]
                            | None -> makeIntConst 0)
                        |> makeArray Any
                    Replacements.Helper.LibCall(com, "Util", "mapCurriedArgs", expectedType, [expr; mappings])
        | _ -> expr

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
            | Any when autoUncurrying -> uncurryExpr com None arg
            | _ ->
                let arg = checkSubArguments com expectedType arg
                let arity, _ = getLambdaTypeArity expectedType
                if arity > 1
                then uncurryExpr com (Some arity) arg
                else arg)

    let uncurryInnerFunctions (_: Compiler) e =
        let curryIdentInBody identName (args: Ident list) body =
            curryIdentsInBody (Map [identName, List.length args]) body
        match e with
        | Let(ident, NestedLambdaWithSameArity(args, fnBody, _), letBody) when List.isMultiple args
                                                                          && not ident.IsMutable ->
            let fnBody = curryIdentInBody ident.Name args fnBody
            let letBody = curryIdentInBody ident.Name args letBody
            Let(ident, Delegate(args, fnBody, None), letBody)
        // Anonymous lambda immediately applied
        | CurriedApply(NestedLambdaWithSameArity(args, fnBody, Some name), argExprs, t, r)
                        when List.isMultiple args && List.sameLength args argExprs ->
            let fnBody = curryIdentInBody name args fnBody
            let info = makeCallInfo None argExprs (args |> List.map (fun a -> a.Type))
            Delegate(args, fnBody, Some name)
            |> makeCall r t info
        | e -> e

    let propagateUncurryingThroughLets (_: Compiler) = function
        | Let(ident, value, body) when not ident.IsMutable ->
            let ident, value, arity =
                match value with
                | Curry(innerExpr, arity) ->
                    ident, innerExpr, Some arity
                | Get(Curry(innerExpr, arity), OptionValue, t, r) ->
                    ident, Get(innerExpr, OptionValue, t, r), Some arity
                | Value(NewOption(Some(Curry(innerExpr, arity)), t, isStruct), r) ->
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
        else { m with Body = uncurryIdentsAndReplaceInBody m.Args m.Body }

    let uncurryReceivedArgs (com: Compiler) e =
        match e with
        // TODO: This breaks cases when we actually need to import a curried function
        // // Sometimes users type imports as lambdas but if they come from JS they're not curried
        // | ExprTypeAs(NestedLambdaType(argTypes, retType), (Import(info, t, r) as e))
        //             when not info.IsCompilerGenerated && List.isMultiple argTypes ->
        //     Curry(e, List.length argTypes, t, r)
        | Lambda(arg, body, name) ->
            let body = uncurryIdentsAndReplaceInBody [arg] body
            Lambda(arg, body, name)
        | Delegate(args, body, name) ->
            let body = uncurryIdentsAndReplaceInBody args body
            Delegate(args, body, name)
        // Uncurry also values received from getters
        | Get(callee, (FieldGet _ | UnionField _), t, r) ->
            match getLambdaTypeArity t, callee.Type with
            // For anonymous records, if the lambda returns a generic the actual
            // arity may be higher than expected, so we need a runtime partial application
            | (arity, GenericParam _), AnonymousRecordType _ when arity > 0 ->
                let callee = makeImportLib com Any "checkArity" "Util"
                let info = makeCallInfo None [makeIntConst arity; e] []
                let e = Call(callee, info, t, r)
                if arity > 1 then Curry(e, arity)
                else e
            | (arity, _), _ when arity > 1 -> Curry(e, arity)
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
        | CurriedApply(callee, args, t, r) ->
            match callee.Type with
            | NestedLambdaType(argTypes, _) ->
                CurriedApply(callee, uncurryArgs com false argTypes args, t, r)
            | _ -> e
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
            let uci = com.GetEntity(ent).UnionCases.[tag]
            let args = uncurryConsArgs args uci.UnionCaseFields
            Value(NewUnion(args, tag, ent, genArgs), r)
        | Set(e, FieldSet(fieldName), t, value, r) ->
            let value = uncurryArgs com false [t] [value]
            Set(e, FieldSet(fieldName), t, List.head value, r)
        | e -> e

    let rec uncurryApplications (com: Compiler) e =
        let uncurryApply r t applied args uncurriedArity =
            let argsLen = List.length args
            if uncurriedArity = argsLen then
                // This is already uncurried we don't need the signature arg types anymore,
                // just make a normal call
                let info = makeCallInfo None args []
                makeCall r t info applied |> Some
            else
                Replacements.partialApplyAtRuntime com t (uncurriedArity - argsLen) applied args |> Some
        match e with
        | NestedApply(applied, args, t, r) ->
            let applied = visitFromOutsideIn (uncurryApplications com) applied
            let args = args |> List.map (visitFromOutsideIn (uncurryApplications com))
            match applied with
            | Curry(applied, uncurriedArity) ->
                uncurryApply r t applied args uncurriedArity
            | Get(Curry(applied, uncurriedArity), OptionValue, t2, r2) ->
                uncurryApply r t (Get(applied, OptionValue, t2, r2)) args uncurriedArity
            | _ -> CurriedApply(applied, args, t, r) |> Some
        | _ -> None

open Transforms

// ATTENTION: Order of transforms matters
// TODO: Optimize binary operations with numerical or string literals
let getTransformations (_com: Compiler) =
    [ // First apply beta reduction
      fun com e -> visitFromInsideOut (bindingBetaReduction com) e
      fun com e -> visitFromInsideOut (lambdaBetaReduction com) e
      // Make an extra binding reduction pass after applying lambdas
      fun com e -> visitFromInsideOut (bindingBetaReduction com) e
      // Then apply uncurry optimizations
      fun com e -> visitFromInsideOut (uncurryReceivedArgs com) e
      fun com e -> visitFromInsideOut (uncurryInnerFunctions com) e
      fun com e -> visitFromInsideOut (propagateUncurryingThroughLets com) e
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
        com.ApplyMemberDeclarationPlugin(file, m)
        |> uncurryMemberArgs
        |> transformMemberBody com
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
                Sequential [baseCall; cons.Body]
                |> uncurryIdentsAndReplaceInBody cons.Args
                |> transformExpr com
                |> function
                    | Sequential [baseCall; body] -> Some { cons with Body = body }, Some baseCall
                    | body -> Some { cons with Body = body }, None // Unexpected, raise error?

        { decl with Constructor = cons
                    BaseCall = baseCall
                    AttachedMembers = attachedMembers }
        |> ClassDeclaration

let transformFile (com: Compiler) (file: File) =
    let transformations = getTransformations com
    let newDecls = List.map (transformDeclaration transformations com file) file.Declarations
    File(newDecls, usedRootNames=file.UsedNamesInRootScope)
