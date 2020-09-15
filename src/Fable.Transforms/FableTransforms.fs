module Fable.Transforms.FableTransforms

open Fable
open Fable.AST.Fable

// TODO: Use trampoline here?
let visit f e =
    match e with
    | IdentExpr _ -> e
    | TypeCast(e, t) -> TypeCast(f e, t)
    | Import(info, t, r) ->
        Import({ info with Selector = f info.Selector
                           Path = f info.Path }, t, r)
    | Value(kind, r) ->
        match kind with
        | ThisValue _ | BaseValue _
        | TypeInfo _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ -> e
        | EnumConstant(exp, ent) -> EnumConstant(f exp, ent) |> makeValue r
        | NewOption(e, t) -> NewOption(Option.map f e, t) |> makeValue r
        | NewTuple exprs -> NewTuple(List.map f exprs) |> makeValue r
        | NewArray(exprs, t) -> NewArray(List.map f exprs, t) |> makeValue r
        | NewArrayAlloc(e, t) -> NewArrayAlloc(f e, t) |> makeValue r
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
    | Curry(e, arity, t, r) -> Curry(f e, arity, t, r)
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
        Emit({ info with Args = List.map f info.Args }, t, r)
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
        | UnionField _ | ByKey(FieldKey _) -> Get(f e, kind, t, r)
        | ByKey(ExprKey e2) -> Get(f e, ByKey(ExprKey(f e2)), t, r)
    | Sequential exprs -> Sequential(List.map f exprs)
    | Let(bs, body) ->
        let bs = bs |> List.map (fun (i,e) -> i, f e)
        Let(bs, f body)
    | IfThenElse(cond, thenExpr, elseExpr, r) ->
        IfThenElse(f cond, f thenExpr, f elseExpr, r)
    | Set(e, kind, v, r) ->
        match kind with
        | Some(ExprKey e2) ->
            Set(f e, Some(ExprKey(f e2)), f v, r)
        | Some(FieldKey _) | None -> Set(f e, kind, f v, r)
    | WhileLoop(e1, e2, r) -> WhileLoop(f e1, f e2, r)
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

// TODO: We should likely make this a property of Fable.Expr
let getSubExpressions = function
    | IdentExpr _ -> []
    | TypeCast(e,_) -> [e]
    | Import(info,_,_) -> [info.Selector; info.Path]
    | Value(kind,_) ->
        match kind with
        | ThisValue _ | BaseValue _
        | TypeInfo _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ -> []
        | EnumConstant(e, _) -> [e]
        | NewOption(e, _) -> Option.toList e
        | NewTuple exprs -> exprs
        | NewArray(exprs, _) -> exprs
        | NewArrayAlloc(e, _) -> [e]
        | NewList(ht, _) ->
            match ht with Some(h,t) -> [h;t] | None -> []
        | NewRecord(exprs, _, _) -> exprs
        | NewAnonymousRecord(exprs, _, _) -> exprs
        | NewUnion(exprs, _, _, _) -> exprs
    | Test(e, _, _) -> [e]
    | Curry(e, _, _, _) -> [e]
    | Lambda(_, body, _) -> [body]
    | Delegate(_, body, _) -> [body]
    | ObjectExpr(members, _, baseCall) ->
        let members = members |> List.map (fun m -> m.Body)
        match baseCall with Some b -> b::members | None -> members
    | CurriedApply(callee, args, _, _) -> callee::args
    | Call(e1, info, _, _) -> e1 :: (Option.toList info.ThisArg) @ info.Args
    | Emit(info, _, _) -> info.Args
    | Operation(kind, _, _) ->
        match kind with
        | Unary(_, operand) -> [operand]
        | Binary(_, left, right) -> [left; right]
        | Logical(_, left, right) -> [left; right]
    | Get(e, kind, _, _) ->
        match kind with
        | ListHead | ListTail | OptionValue | TupleIndex _ | UnionTag
        | UnionField _ | ByKey(FieldKey _) -> [e]
        | ByKey(ExprKey e2) -> [e; e2]
    | Sequential exprs -> exprs
    | Let(bs, body) -> (List.map snd bs) @ [body]
    | IfThenElse(cond, thenExpr, elseExpr, _) -> [cond; thenExpr; elseExpr]
    | Set(e, kind, v, _) ->
        match kind with
        | Some(ExprKey e2) -> [e; e2; v]
        | Some(FieldKey _) | None -> [e; v]
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
        let subExprs = ResizeArray()
        for e in exprs do
            if not found then
                subExprs.AddRange(getSubExpressions e)
                found <- f e
        if found then true
        elif subExprs.Count > 0 then deepExistsInner subExprs
        else false
    ResizeArray [|expr|] |> deepExistsInner

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

let canInlineArg identName value body =
    match value with
    | Lambda _ | Delegate _ -> countReferences 1 identName body <= 1
    | value -> canHaveSideEffects value |> not

module private Transforms =
    let (|LambdaOrDelegate|_|) = function
        | Lambda(arg, body, name) -> Some([arg], body, name)
        | Delegate(args, body, name) -> Some(args, body, name)
        | _ -> None

    let (|FieldType|) (fi: Field) = fi.FieldType

    let lambdaBetaReduction (com: Compiler) e =
        // Sometimes the F# compiler creates a lot of binding closures, as with printfn
        let (|NestedLetsAndLambdas|_|) expr =
            let rec inner accBindings accArgs body name =
                match body with
                | Lambda(arg, body, None) ->
                    inner accBindings (arg::accArgs) body name
                | Let(bindings, body) ->
                    inner (accBindings @ bindings) accArgs body name
                | _ when not(List.isEmpty accArgs) ->
                    Some(List.rev accArgs, Let(accBindings, body), name)
                | _ -> None
            match expr with
            | Let(bindings, body) ->
                inner bindings [] body None
            | Lambda(arg, body, name) ->
                inner [] [arg] body name
            | _ -> None
        let applyArgs (args: Ident list) argExprs body =
            let bindings, replacements =
                (([], Map.empty), args, argExprs)
                |||> List.fold2 (fun (bindings, replacements) ident expr ->
                    if (not com.Options.DebugMode) && canInlineArg ident.Name expr body
                    then bindings, Map.add ident.Name expr replacements
                    else (ident, expr)::bindings, replacements)
            match bindings with
            | [] -> replaceValues replacements body
            | bindings -> Let(List.rev bindings, replaceValues replacements body)
        match e with
        // TODO: Other binary operations and numeric types, also recursive?
        | Operation(Binary(AST.BinaryPlus, Value(StringConstant str1, r1), Value(StringConstant str2, r2)),_,_) ->
            Value(StringConstant(str1 + str2), addRanges [r1; r2])
        | NestedApply(NestedLetsAndLambdas(lambdaArgs, body, _) as lambda, argExprs,_,_) ->
            if List.sameLength lambdaArgs argExprs then
                applyArgs lambdaArgs argExprs body
            else
                // Partial apply
                match List.length argExprs, lambda with
                | 1, Lambda(arg, body, _) ->
                    applyArgs [arg] argExprs body
                | 2, Lambda(arg1, Lambda(arg2, body,_),_) ->
                    applyArgs [arg1; arg2] argExprs body
                | 3, Lambda(arg1, Lambda(arg2, Lambda(arg3, body,_),_),_) ->
                    applyArgs [arg1; arg2; arg3] argExprs body
                | 4, Lambda(arg1, Lambda(arg2, Lambda(arg3, Lambda(arg4, body,_),_),_),_) ->
                    applyArgs [arg1; arg2; arg3; arg4] argExprs body
                | _ -> e
        | e -> e

    /// Tuples created when pattern matching multiple elements can usually be erased
    /// after the binding and lambda beta reduction
    let tupleBetaReduction (_: Compiler) = function
        | Get(Value(NewTuple exprs, _), TupleIndex index, _, _) -> List.item index exprs
        | e -> e

    let bindingBetaReduction (com: Compiler) e =
        // Don't erase user-declared bindings in debug mode for better source maps
        let isErasingCandidate (ident: Ident) =
            (not com.Options.DebugMode) || ident.IsCompilerGenerated
        match e with
        // Don't try to optimize bindings with multiple ident-value pairs as they can reference each other
        | Let([ident, value], letBody) when (not ident.IsMutable) && isErasingCandidate ident ->
            let canEraseBinding =
                match value with
                | NestedLambda(_, lambdaBody, _) ->
                    match lambdaBody with
                    | Import _ -> false
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
            | _ -> acc
        match t with
        | LambdaType(_, returnType)
        | Option(LambdaType(_, returnType)) ->
            getLambdaTypeArity 1 returnType
        | _ -> 0

    let curryIdentsInBody replacements body =
        visitFromInsideOut (function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some arity -> Curry(e, arity, id.Type, id.Range)
                | None -> e
            | e -> e) body

    let uncurryIdentsAndReplaceInBody (idents: Ident list) body =
        let replacements =
            (Map.empty, idents) ||> List.fold (fun replacements id ->
                let arity = getLambdaTypeArity id.Type
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
        | _, Curry(innerExpr, arity2,_,_)
            when matches arity arity2 -> innerExpr
        | _, Get(Curry(innerExpr, arity2,_,_), OptionValue, t, r)
            when matches arity arity2 -> Get(innerExpr, OptionValue, t, r)
        | _, Value(NewOption(Some(Curry(innerExpr, arity2,_,_)),r1),r2)
            when matches arity arity2 -> Value(NewOption(Some(innerExpr),r1),r2)
        | _ ->
            match arity with
            | Some arity -> Replacements.uncurryExprAtRuntime com arity expr
            | None -> expr

    // For function arguments check if the arity of their own function arguments is expected or not
    // TODO: Do we need to do this recursively, and check options and delegates too?
    let checkSubArguments com expectedType (expr: Expr) =
        match expectedType, expr with
        | NestedLambdaType(expectedArgs,_), ExprType(NestedLambdaType(actualArgs,_)) ->
            let actualArgs = List.truncate expectedArgs.Length actualArgs
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
            if Map.isEmpty replacements
            then expr
            else
                let mappings =
                    actualArgs |> List.mapi (fun i _ ->
                        match Map.tryFind i replacements with
                        | Some (expectedArity, actualArity) ->
                            NewTuple [makeIntConst expectedArity; makeIntConst actualArity] |> makeValue None
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
                | [], args2 -> (List.rev acc)@args2
                | _, [] -> List.rev acc
            mapArgsInner f [] argTypes args
        match argTypes with
        | _ when autoUncurrying -> List.map (uncurryExpr com None) args
        | [] -> args // Do nothing
        | argTypes ->
            (argTypes, args) ||> mapArgs (fun expectedType arg ->
                let arg = checkSubArguments com expectedType arg
                let arity = getLambdaTypeArity expectedType
                if arity > 1
                then uncurryExpr com (Some arity) arg
                else arg)

    let uncurryInnerFunctions (_: Compiler) e =
        let curryIdentInBody identName (args: Ident list) body =
            curryIdentsInBody (Map [identName, List.length args]) body
        match e with
        | Let([ident, NestedLambdaWithSameArity(args, fnBody, _)], letBody) when List.isMultiple args ->
            let fnBody = curryIdentInBody ident.Name args fnBody
            let letBody = curryIdentInBody ident.Name args letBody
            Let([ident, Delegate(args, fnBody, None)], letBody)
        // Anonymous lambda immediately applied
        | CurriedApply(NestedLambdaWithSameArity(args, fnBody, Some name), argExprs, t, r)
                        when List.isMultiple args && List.sameLength args argExprs ->
            let fnBody = curryIdentInBody name args fnBody
            let info = makeCallInfo None argExprs (args |> List.map (fun a -> a.Type))
            Delegate(args, fnBody, Some name)
            |> makeCall r t info
        | e -> e

    let propagateUncurryingThroughLets (_: Compiler) = function
        | Let(identsAndValues, body) ->
            let identsAndValues, replacements =
                (identsAndValues, ([], Map.empty)) ||> List.foldBack (fun (id, value) (identsAndValues, replacements) ->
                    match value with
                    | Curry(innerExpr, arity,_,_) ->
                        (id, innerExpr)::identsAndValues, Map.add id.Name arity replacements
                    | Get(Curry(innerExpr, arity,_,_), OptionValue, t, r) ->
                        (id, Get(innerExpr, OptionValue, t, r))::identsAndValues, Map.add id.Name arity replacements
                    | Value(NewOption(Some(Curry(innerExpr, arity,_,_)),r1),r2) ->
                        (id, Value(NewOption(Some(innerExpr),r1),r2))::identsAndValues, Map.add id.Name arity replacements
                    | _ -> (id, value)::identsAndValues, replacements)
            if Map.isEmpty replacements
            then Let(identsAndValues, body)
            else Let(identsAndValues, curryIdentsInBody replacements body)
        | e -> e

    let uncurryMemberArgs (m: MemberDecl) =
        if m.Info.IsValue then m
        else { m with Body = uncurryIdentsAndReplaceInBody m.Args m.Body }

    let uncurryReceivedArgs (_: Compiler) e =
        match e with
        | Lambda(arg, body, name) ->
            let body = uncurryIdentsAndReplaceInBody [arg] body
            Lambda(arg, body, name)
        | Delegate(args, body, name) ->
            let body = uncurryIdentsAndReplaceInBody args body
            Delegate(args, body, name)
        // Uncurry also values received from getters
        | Get(_, (ByKey(FieldKey(FieldType fieldType)) | UnionField(_,fieldType)), t, r) ->
            let arity = getLambdaTypeArity fieldType
            if arity > 1
            then Curry(e, arity, t, r)
            else e
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
        | Emit(info, t, r) ->
            let autoUncurrying = List.isEmpty info.SignatureArgTypes
            let args = uncurryArgs com autoUncurrying info.SignatureArgTypes info.Args
            Emit({ info with Args = args }, t, r)
        // Uncurry also values in setters or new record/union/tuple
        | Value(NewRecord(args, ent, genArgs), r) ->
            let args = uncurryConsArgs args ent.FSharpFields
            Value(NewRecord(args, ent, genArgs), r)
        | Value(NewAnonymousRecord(args, fieldNames, genArgs), r) ->
            // TODO: Use field types instead of auto-uncurrying?
            let args = uncurryArgs com true [] args
            Value(NewAnonymousRecord(args, fieldNames, genArgs), r)
        | Value(NewUnion(args, tag, ent, genArgs), r) ->
            let uci = ent.UnionCases.[tag]
            let args = uncurryConsArgs args uci.UnionCaseFields
            Value(NewUnion(args, tag, ent, genArgs), r)
        | Set(e, Some(FieldKey fi), value, r) ->
            let value = uncurryArgs com false [fi.FieldType] [value]
            Set(e, Some(FieldKey fi), List.head value, r)
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
            | Curry(applied, uncurriedArity,_,_) ->
                uncurryApply r t applied args uncurriedArity
            | Get(Curry(applied, uncurriedArity,_,_), OptionValue, t2, r2) ->
                uncurryApply r t (Get(applied, OptionValue, t2, r2)) args uncurriedArity
            | _ -> CurriedApply(applied, args, t, r) |> Some
        | _ -> None

open Transforms

// ATTENTION: Order of transforms matters
// TODO: Optimize binary operations with numerical or string literals
let optimizations =
    [ // First apply beta reduction
      fun com e -> visitFromInsideOut (bindingBetaReduction com) e
      fun com e -> visitFromInsideOut (lambdaBetaReduction com) e
      fun com e -> visitFromInsideOut (tupleBetaReduction com) e
      // Then apply uncurry optimizations
      fun com e -> visitFromInsideOut (uncurryReceivedArgs com) e
      fun com e -> visitFromInsideOut (uncurryInnerFunctions com) e
      fun com e -> visitFromInsideOut (propagateUncurryingThroughLets com) e
      fun com e -> visitFromInsideOut (uncurrySendingArgs com) e
      // uncurryApplications must come after uncurrySendingArgs as it erases argument type info
      fun com e -> visitFromOutsideIn (uncurryApplications com) e
    ]

let transformExpr (com: Compiler) e =
    List.fold (fun e f -> f com e) e optimizations

let transformMemberBody com (m: MemberDecl) =
    { m with Body = transformExpr com m.Body }

let transformDeclaration (com: Compiler) = function
    | ActionDeclaration decl ->
        { decl with Body = transformExpr com decl.Body }
        |> ActionDeclaration

    | MemberDeclaration m ->
        uncurryMemberArgs m
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
    let newDecls = List.map (transformDeclaration com) file.Declarations
    File(newDecls, usedRootNames=file.UsedNamesInRootScope)
