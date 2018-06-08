module Fable.Transforms.FableTransforms

open Fable
open Fable.AST.Fable
open Microsoft.FSharp.Compiler.SourceCodeServices

// TODO: Use trampoline here?
let visit f e =
    match e with
    | IdentExpr _ | Debugger _ -> e
    | Import(e1, e2, kind, t, r) -> Import(f e1, f e2, kind, t, r)
    | Value kind ->
        match kind with
        | TypeInfo _ | This _ | Super _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ | Enum _ -> e
        | NewOption(e, t) -> NewOption(Option.map f e, t) |> Value
        | NewTuple exprs -> NewTuple(List.map f exprs) |> Value
        | NewArray(kind, t) ->
            match kind with
            | ArrayValues exprs -> NewArray(ArrayValues(List.map f exprs), t) |> Value
            | ArrayAlloc e -> NewArray(ArrayAlloc(f e), t) |> Value
        | NewList(ht, t) ->
            let ht = ht |> Option.map (fun (h,t) -> f h, f t)
            NewList(ht, t) |> Value
        | NewRecord(exprs, ent, genArgs) ->
            NewRecord(List.map f exprs, ent, genArgs) |> Value
        | NewErasedUnion(e, genArgs) ->
            NewErasedUnion(f e, genArgs) |> Value
        | NewUnion(exprs, uci, ent, genArgs) ->
            NewUnion(List.map f exprs, uci, ent, genArgs) |> Value
    | Test(e, kind, r) -> Test(f e, kind, r)
    | DelayedResolution(kind, t) ->
        match kind with
        | AsSeqFromList e -> DelayedResolution(AsSeqFromList(f e), t)
        | AsPojo(e, r) -> DelayedResolution(AsPojo(f e, r), t)
        | AsUnit e -> DelayedResolution(AsUnit(f e), t)
    | Function(kind, body, name) -> Function(kind, f body, name)
    | ObjectExpr(members, t, baseCall) ->
        let baseCall = Option.map f baseCall
        let members = members |> List.map (fun (n,v,k) -> n, f v, k)
        ObjectExpr(members, t, baseCall)
    | Operation(kind, t, r) ->
        match kind with
        | CurriedApply(callee, args) ->
            Operation(CurriedApply(f callee, List.map f args), t, r)
        | Call(kind, info) ->
            let kind =
                match kind with
                | ConstructorCall e -> ConstructorCall(f e)
                | StaticCall e -> StaticCall(f e)
                | InstanceCall memb -> InstanceCall(Option.map f memb)
            let info = { info with ThisArg = Option.map f info.ThisArg
                                   Args = List.map f info.Args }
            Operation(Call(kind, info), t, r)
        | Emit(macro, info) ->
            let info = info |> Option.map (fun info ->
                { info with ThisArg = Option.map f info.ThisArg
                            Args = List.map f info.Args })
            Operation(Emit(macro, info), t, r)
        | UnaryOperation(operator, operand) ->
            Operation(UnaryOperation(operator, f operand), t, r)
        | BinaryOperation(op, left, right) ->
            Operation(BinaryOperation(op, f left, f right), t, r)
        | LogicalOperation(op, left, right) ->
            Operation(LogicalOperation(op, f left, f right), t, r)
    | Get(e, kind, t, r) ->
        match kind with
        | ListHead | ListTail | OptionValue | TupleGet _ | UnionTag _
        | UnionField _ | RecordGet _ -> Get(f e, kind, t, r)
        | ExprGet e2 -> Get(f e, ExprGet (f e2), t, r)
    | Throw(e, typ, r) -> Throw(f e, typ, r)
    | Sequential exprs -> Sequential(List.map f exprs)
    | Let(bs, body) ->
        let bs = bs |> List.map (fun (i,e) -> i, f e)
        Let(bs, f body)
    | IfThenElse(cond, thenExpr, elseExpr) ->
        IfThenElse(f cond, f thenExpr, f elseExpr)
    | Set(e, kind, v, r) ->
        match kind with
        | VarSet | RecordSet _ ->
            Set(f e, kind, f v, r)
        | ExprSet e2 -> Set(f e, ExprSet (f e2), f v, r)
    | Loop (kind, r) ->
        match kind with
        | While(e1, e2) -> Loop(While(f e1, f e2), r)
        | For(i, e1, e2, e3, up) -> Loop(For(i, f e1, f e2, f e3, up), r)
    | TryCatch(body, catch, finalizer) ->
        TryCatch(f body,
                 Option.map (fun (i, e) -> i, f e) catch,
                 Option.map f finalizer)
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
    | None ->
        visit (visitFromOutsideIn f) e

let getSubExpressions = function
    | IdentExpr _ | Debugger _ -> []
    | Import(e1,e2,_,_,_) -> [e1;e2]
    | Value kind ->
        match kind with
        | TypeInfo _ | This _ | Super _ | Null _ | UnitConstant
        | BoolConstant _ | CharConstant _ | StringConstant _
        | NumberConstant _ | RegexConstant _ | Enum _ -> []
        | NewOption(e, _) -> Option.toList e
        | NewTuple exprs -> exprs
        | NewArray(kind, _) ->
            match kind with
            | ArrayValues exprs -> exprs
            | ArrayAlloc e -> [e]
        | NewList(ht, _) ->
            match ht with Some(h,t) -> [h;t] | None -> []
        | NewRecord(exprs, _, _) -> exprs
        | NewErasedUnion(e, _) -> [e]
        | NewUnion(exprs, _, _, _) -> exprs
    | Test(e, _, _) -> [e]
    | DelayedResolution(kind, _) ->
        match kind with
        | AsSeqFromList e
        | AsPojo(e,_)
        | AsUnit e -> [e]
    | Function(_, body, _) -> [body]
    | ObjectExpr(members, _, baseCall) ->
        let members = members |> List.map (fun (_,v,_) -> v)
        match baseCall with Some b -> b::members | None -> members
    | Operation(kind, _, _) ->
        match kind with
        | CurriedApply(callee, args) -> callee::args
        | Call(kind, info) ->
            let e1 =
                match kind with
                | ConstructorCall e -> [e]
                | StaticCall e -> [e]
                | InstanceCall memb -> Option.toList memb
            e1 @ (Option.toList info.ThisArg) @ info.Args
        | Emit(_, info) ->
            match info with Some info -> (Option.toList info.ThisArg) @ info.Args | None -> []
        | UnaryOperation(_, operand) -> [operand]
        | BinaryOperation(_, left, right) -> [left; right]
        | LogicalOperation(_, left, right) -> [left; right]
    | Get(e, kind, _, _) ->
        match kind with
        | ListHead | ListTail | OptionValue | TupleGet _ | UnionTag _
        | UnionField _ | RecordGet _ -> [e]
        | ExprGet e2 -> [e; e2]
    | Throw(e, _, _) -> [e]
    | Sequential exprs -> exprs
    | Let(bs, body) -> (List.map snd bs) @ [body]
    | IfThenElse(cond, thenExpr, elseExpr) -> [cond; thenExpr; elseExpr]
    | Set(e, kind, v, _) ->
        match kind with
        | VarSet | RecordSet _ -> [e; v]
        | ExprSet e2 -> [e; e2; v]
    | Loop (kind, _) ->
        match kind with
        | While(e1, e2) -> [e1; e2]
        | For(_, e1, e2, e3, _) -> [e1; e2; e3]
    | TryCatch(body, catch, finalizer) ->
        match catch with
        | Some(_,c) -> body::c::(Option.toList finalizer)
        | None -> body::(Option.toList finalizer)
    | DecisionTree(expr, targets) -> expr::(List.map snd targets)
    | DecisionTreeSuccess(_, boundValues, _) -> boundValues

let rec deepExists f expr =
    f expr || (getSubExpressions expr |> List.exists (deepExists f))

let rec deepExistsWithShortcircuit f expr =
    match f expr with
    | Some res -> res
    | None -> getSubExpressions expr |> List.exists (deepExistsWithShortcircuit f)

let replaceValues replacements expr =
    if Map.isEmpty replacements
    then expr
    else expr |> visitFromInsideOut (function
        | IdentExpr id as e ->
            match Map.tryFind id.Name replacements with
            | Some e -> e
            | None -> e
        | e -> e)

let canEraseBinding identName value body =
    // Don't erase expressions referenced 0 times, they may have side-effects
    let isReferencedOnlyOnce identName body =
        let limit = 1
        let mutable count = 0
        body |> deepExists (function
            | IdentExpr id2 when id2.Name = identName ->
                count <- count + 1
                count > limit
            | _ -> false) |> ignore
        count = limit
    match value with
    | Value(This _) -> // Check if body contains closures
        body |> deepExists (function
            | Function _ | ObjectExpr _ -> true
            | _ -> false) |> not
    | e -> not(hasDoubleEvalRisk e) || isReferencedOnlyOnce identName body

module private Transforms =
    let (|ExprType|) (e: Expr) = e.Type

    let (|EntFullName|_|) fullName (ent: FSharpEntity) =
        match ent.TryFullName with
        | Some fullName2 when fullName = fullName2 -> Some EntFullName
        | _ -> None

    let (|LambdaOrDelegate|_|) = function
        | Function(Lambda arg, body, name) -> Some([arg], body, name)
        | Function(Delegate args, body, name) -> Some(args, body, name)
        | _ -> None

    let rec (|NestedLambda|_|) expr =
        let rec nestedLambda accArgs body name =
            match body with
            | Function(Lambda arg, body, None) ->
                nestedLambda (arg::accArgs) body name
            | _ -> Some(List.rev accArgs, body, name)
        match expr with
        | Function(Lambda arg, body, name) -> nestedLambda [arg] body name
        | _ -> None

    let (|NestedApply|_|) expr =
        let rec nestedApply r t accArgs applied =
            match applied with
            | Operation(CurriedApply(applied, args), _, _) ->
                nestedApply r t (args@accArgs) applied
            | _ -> Some(applied, accArgs, t, r)
        match expr with
        | Operation(CurriedApply(applied, args), t, r) ->
            nestedApply r t args applied
        | _ -> None

    let (|NestedLambdaType|_|) t =
        let rec nestedLambda acc = function
            | FunctionType(LambdaType arg, returnType) ->
                nestedLambda (arg::acc) returnType
            | returnType -> Some(acc, returnType)
        match t with
        | FunctionType(LambdaType arg, returnType) -> nestedLambda [arg] returnType
        | _ -> None

    let (|EvalsMutableIdent|_|) expr =
        if deepExistsWithShortcircuit (function
            | IdentExpr id when id.IsMutable -> Some true
            | Function _ -> Some false // Ignore function bodies
            | _ -> None) expr
        then Some EvalsMutableIdent
        else None

    let lambdaBetaReduction (_: ICompiler) e =
        let applyArgs (args: Ident list) argExprs body =
            let bindings, replacements =
                (([], Map.empty), args, argExprs)
                |||> List.fold2 (fun (bindings, replacements) ident expr ->
                    if canEraseBinding ident.Name expr body
                    then bindings, Map.add ident.Name expr replacements
                    else (ident, expr)::bindings, replacements)
            match bindings with
            | [] -> replaceValues replacements body
            | bindings -> Let(List.rev bindings, replaceValues replacements body)
        match e with
        // TODO: `Invoke` calls for delegates? Partial applications too?
        // TODO: Don't inline if one of the arguments is `this`?
        | Operation(CurriedApply(NestedLambda(args, body, None), argExprs), _, _)
            when List.sameLength args argExprs ->
            applyArgs args argExprs body
        | e -> e

    // TODO: Other erasable getters? (List.head, List.item...)
    let getterBetaReduction (_: ICompiler) = function
        | Get(Value(NewTuple exprs), TupleGet index, _, _) -> List.item index exprs
        | Get(Value(NewOption(Some expr, _)), OptionValue, _, _) -> expr
        | e -> e

    let bindingBetaReduction (_: ICompiler) e =
        match e with
        // Don't try to optimize bindings with multiple ident-value pairs
        // as they can reference each other
        | Let([ident, value], body) when not ident.IsMutable ->
            let identName = ident.Name
            let replacement =
                match value with
                // Don't erase bindings if mutable variables are involved
                // as the value can change in between statements
                | EvalsMutableIdent -> None
                // When replacing an ident with an erased option use the name but keep the unwrapped type
                | Get(IdentExpr id, OptionValue, t, _)
                        when mustWrapOption t |> not ->
                    makeTypedIdent t id.Name |> IdentExpr |> Some
                | value when ident.IsCompilerGenerated && canEraseBinding identName value body ->
                    match value with
                    // TODO: Check if current name is Some? Shouldn't happen...
                    | Function(args, body, _) -> Function(args, body, Some identName) |> Some
                    | value -> Some value
                | _ -> None
            match replacement with
            | Some value -> replaceValues (Map [identName, value]) body
            | None -> e
        | e -> e

    let uncurryLambdaType t =
        let rec uncurryLambdaTypeInner acc = function
            | FunctionType(LambdaType argType, returnType) ->
                uncurryLambdaTypeInner (argType::acc) returnType
            | returnType -> List.rev acc, returnType
        match t with
        | FunctionType(LambdaType argType, returnType)
        | Option(FunctionType(LambdaType argType, returnType)) ->
            uncurryLambdaTypeInner [argType] returnType
        | FunctionType(DelegateType argTypes, returnType) ->
            argTypes, returnType
        | returnType -> [], returnType

    let replaceIdentType replacements (id: Ident) =
        match Map.tryFind id.Name replacements with
        | Some(Option nestedType as optionType) ->
            // Check if the ident to replace is an option too or not
            match id.Type with
            | Option _ -> { id with Type = optionType }
            | _ ->        { id with Type = nestedType }
        | Some typ -> { id with Type = typ }
        | None -> id

    let replaceIdentTypesInBody (identTypes: Map<string, Type>) body =
        visitFromInsideOut (function
            | IdentExpr id -> replaceIdentType identTypes id |> IdentExpr
            | e -> e) body

    let uncurryIdentsAndReplaceInBody (idents: Ident list) body =
        let uncurried =
            (Map.empty, idents) ||> List.fold (fun uncurried id ->
                match uncurryLambdaType id.Type with
                | argTypes, returnType when List.isMultiple argTypes ->
                    let delType = FunctionType(DelegateType argTypes, returnType)
                    // uncurryLambdaType ignores options, so check the original type
                    match id.Type with
                    | Option _ -> Map.add id.Name (Option delType) uncurried
                    | _ -> Map.add id.Name delType uncurried
                | _ -> uncurried)
        if Map.isEmpty uncurried
        then idents, body
        else
            let idents = idents |> List.map (replaceIdentType uncurried)
            let body = replaceIdentTypesInBody uncurried body
            idents, body

    let rec lambdaMayEscapeScope identName = function
        | Operation(CurriedApply(IdentExpr ident,_),_,_) when ident.Name = identName -> false
        | IdentExpr ident when ident.Name = identName -> true
        | e -> getSubExpressions e |> List.exists (lambdaMayEscapeScope identName)

    // TODO!!! See ApplicativeTests/Generic lambda arguments work
    let checkSubArguments com expectedArgs (expr: Expr) =
        match expr.Type with
        | NestedLambdaType(actualArgs, _) when List.sameLength expectedArgs actualArgs ->
            for (expected, actual) in List.zip expectedArgs actualArgs do
                match expected, actual with
                // Subarguments may have been uncurried already and become delegate
                | NestedLambdaType(args1, _), FunctionType(DelegateType args2, _)
                | NestedLambdaType(args1, _), NestedLambdaType(args2, _) ->
                    if not <| List.sameLength args1 args2 then
                        "Current version cannot pass a lambda with arguments of unexpected arity"
                        |> addError com expr.Range
                | _ -> ()
        | _ -> ()

    let uncurryArgs com argTypes args =
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
        | Some [] -> args // Do nothing
        | Some argTypes ->
            (argTypes, args) ||> mapArgs (fun argType arg ->
                match uncurryLambdaType argType with
                | argTypes, retType when not(List.isEmpty argTypes) ->
                    checkSubArguments com argTypes arg
                    if List.isMultiple argTypes
                    then Replacements.uncurryExpr (Some(argTypes, retType)) arg
                    else arg
                | _ -> arg)
        | None -> List.map (Replacements.uncurryExpr None) args

    let uncurryInnerFunctions (_: ICompiler) e =
        let replaceIdentInBody identName (args: Ident list) returnType body =
            let delType = FunctionType(DelegateType (args |> List.map (fun a -> a.Type)), returnType)
            replaceIdentTypesInBody (Map [identName, delType]) body
        match e with
        | Let([ident, NestedLambda(args, fnBody, _)], letBody) as e when List.isMultiple args ->
            // We need to check the function doesn't leave the current context
            if lambdaMayEscapeScope ident.Name letBody |> not then
                let fnBody = replaceIdentInBody ident.Name args fnBody.Type fnBody
                let letBody = replaceIdentInBody ident.Name args fnBody.Type letBody
                Let([ident, Function(Delegate args, fnBody, None)], letBody)
            else e
        // Anonymous lambda immediately applied
        | Operation(CurriedApply((NestedLambda(args, fnBody, Some name)), argExprs), t, r)
                        when List.isMultiple args && List.sameLength args argExprs ->
            let fnBody = replaceIdentInBody name args fnBody.Type fnBody
            let info = argInfo None argExprs (args |> List.map (fun a -> a.Type) |> Some)
            Function(Delegate args, fnBody, Some name)
            |> staticCall r t info
        | e -> e

    let uncurryReceivedArgs (_: ICompiler) e =
        match e with
        | Function(Lambda arg, body, name) ->
            let args, body = uncurryIdentsAndReplaceInBody [arg] body
            Function(Lambda (List.head args), body, name)
        | Function(Delegate args, body, name) ->
            let args, body = uncurryIdentsAndReplaceInBody args body
            Function(Delegate args, body, name)
        | e -> e

    // TODO: More tests about uncurrying record fields
    let uncurryRecordFields (com: ICompiler) = function
        | Value(NewRecord(args, ent, genArgs)) ->
            let genArgsMap =
                FSharp2Fable.Util.matchGenericParams genArgs ent.GenericParameters
                |> Map
            let argTypes =
                ent.FSharpFields
                |> Seq.map (fun fi -> FSharp2Fable.TypeHelpers.makeType com genArgsMap fi.FieldType)
                |> Seq.toList
            let args = uncurryArgs com (Some argTypes)  args
            Value(NewRecord(args, ent, genArgs))
        | Get(e, RecordGet(fi, ent), t, r) ->
            let uncurriedType =
                match t with
                | FunctionType(LambdaType _, _) ->
                    let argTypes, retType = uncurryLambdaType t
                    FunctionType(DelegateType argTypes, retType)
                | Option(FunctionType(LambdaType _, _) as t) ->
                    let argTypes, retType = uncurryLambdaType t
                    Option(FunctionType(DelegateType argTypes, retType))
                | _ -> t
            Get(e, RecordGet(fi, ent), uncurriedType, r)
        // If a record function is assigned to a value, just curry it to prevent issues
        | Let(identsAndValues, body) ->
            let identsAndValues =
                identsAndValues |> List.map (fun (ident, value) ->
                    match ident.Type, value with
                    | FunctionType(LambdaType _, _) as t, Get(_, RecordGet _, FunctionType(DelegateType delArgTypes, _), _)
                        when List.isMultiple delArgTypes ->
                            ident, Replacements.curryExprAtRuntime t (List.length delArgTypes) value
                    | _ -> ident, value)
            Let(identsAndValues, body)
        | e -> e

    let uncurrySendingArgs (com: ICompiler) = function
        | Operation(Call(kind, info), t, r) ->
            let info = { info with Args = uncurryArgs com info.SignatureArgTypes info.Args }
            Operation(Call(kind, info), t, r)
        | Operation(CurriedApply(callee, args), t, r) ->
            let argTypes = uncurryLambdaType callee.Type |> fst |> Some
            Operation(CurriedApply(callee, uncurryArgs com argTypes args), t, r)
        | Operation(Emit(macro, Some info), t, r) ->
            let info = { info with Args = uncurryArgs com info.SignatureArgTypes info.Args }
            Operation(Emit(macro, Some info), t, r)
        | e -> e

    let rec uncurryApplications (com: ICompiler) e =
        match e with
        | NestedApply(applied, args, t, r) ->
            let applied = visitFromOutsideIn (uncurryApplications com) applied
            let args = args |> List.map (visitFromOutsideIn (uncurryApplications com))
            match applied.Type with
            | FunctionType(DelegateType argTypes, _) ->
                if List.sameLength argTypes args then
                    let info = argInfo None args (Some argTypes)
                    staticCall r t info applied |> Some
                else
                    Replacements.partialApplyAtRuntime t (argTypes.Length - args.Length) applied args |> Some
            | _ -> Operation(CurriedApply(applied, args), t, r) |> Some
        | _ -> None

    let unwrapFunctions (_: ICompiler) e =
        let sameArgs args1 args2 =
            List.sameLength args1 args2
            && List.forall2 (fun (a1: Ident) -> function
                | IdentExpr a2 -> a1.Name = a2.Name
                | _ -> false) args1 args2
        let unwrapFunctionsInner = function
            // TODO: When Option.isSome info.ThisArg we could bind it (also for InstanceCall)
            | LambdaOrDelegate(args, Operation(Call(StaticCall funcExpr, info), _, _), _)
                when Option.isNone info.ThisArg && sameArgs args info.Args -> funcExpr
            | e -> e
        match e with
        // We cannot apply the unwrap optimization to the outmost function,
        // as we would be losing the ValueDeclarationInfo
        | Function(kind, body, name) -> Function(kind, visitFromInsideOut unwrapFunctionsInner body, name)
        | e -> visitFromInsideOut unwrapFunctionsInner e

open Transforms

// ATTENTION: Order of transforms matters for optimizations
// TODO: Optimize binary operations with numerical or string literals
let optimizations =
    [ // First apply beta reduction
      fun com e -> visitFromInsideOut (bindingBetaReduction com) e
      fun com e -> visitFromInsideOut (lambdaBetaReduction com) e
      fun com e -> visitFromInsideOut (getterBetaReduction com) e
      // Then apply uncurry optimizations
      fun com e -> visitFromInsideOut (uncurryReceivedArgs com) e
      fun com e -> visitFromInsideOut (uncurryRecordFields com) e
      fun com e -> visitFromInsideOut (uncurryInnerFunctions com) e
      fun com e -> visitFromOutsideIn (uncurryApplications com) e
      fun com e -> visitFromInsideOut (uncurrySendingArgs com) e
      // Don't traverse the expression for the unwrap function optimization
      unwrapFunctions
    ]

let optimizeExpr (com: ICompiler) e =
    List.fold (fun e f -> f com e) e optimizations

let rec optimizeDeclaration (com: ICompiler) = function
    | ActionDeclaration expr ->
        ActionDeclaration(optimizeExpr com expr)
    | ValueDeclaration(value, info) ->
        ValueDeclaration(optimizeExpr com value, info)
    | ImplicitConstructorDeclaration(args, body, info) ->
        ImplicitConstructorDeclaration(args, optimizeExpr com body, info)
    | OverrideDeclaration(args, body, info) ->
        OverrideDeclaration(args, optimizeExpr com body, info)
    | InterfaceCastDeclaration(members, info) ->
        let members = members |> List.map (fun (n,v,k) -> n, optimizeExpr com v, k)
        InterfaceCastDeclaration(members, info)

let optimizeFile (com: ICompiler) (file: File) =
    let newDecls = List.map (optimizeDeclaration com) file.Declarations
    File(file.SourcePath, newDecls, usedVarNames=file.UsedVarNames, dependencies=file.Dependencies)
