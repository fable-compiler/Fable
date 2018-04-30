module Fable.Transforms.Fable2Babel

open Fable
open Fable.Core
open Fable.AST
open Fable.AST.Babel
open System.Collections.Generic

type ReturnStrategy =
    | Return
    | Assign of Expression // TODO: Add SourceLocation?
    | Target of Identifier

type Import =
  { Selector: string
    LocalIdent: string option
    Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract ReplaceArgs: bool
    abstract IsRecursiveRef: Fable.Expr -> bool

type Context =
  { File: Fable.File
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit }

type IBabelCompiler =
    inherit ICompiler
    abstract GetAllImports: unit -> seq<Import>
    abstract GetAllDependencies: unit -> seq<string>
    abstract GetImportExpr: Context * selector: string * path: string * Fable.ImportKind -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement list
    abstract TransformImport: Context * selector:string * path:string * Fable.ImportKind -> Expression
    abstract TransformObjectExpr: Context * Fable.ObjectMember list * ?baseCall: Fable.Expr * ?boundThis: string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr
        -> (Pattern list) * U2<BlockStatement, Expression>

module Util =
    let inline (|ExprType|) (fexpr: Fable.Expr) = fexpr.Type
    let inline (|TransformExpr|) (com: IBabelCompiler) ctx e = com.TransformAsExpr(ctx, e)

    let (|FunctionArgs|) = function
        | Fable.Lambda arg -> [arg]
        | Fable.Delegate args -> args

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArg && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    type NamedTailCallOpportunity(com: ICompiler, name, args: Fable.Ident list) =
        let getTailCallArgIds (com: ICompiler) (args: Fable.Ident list) =
            // If some arguments are functions we need to capture the current values to
            // prevent delayed references from getting corrupted, for that we use block-scoped
            // ES2015 variable declarations. See #681
            let replaceArgs =
                args |> List.exists (fun arg ->
                    match arg.Type with
                    | Fable.FunctionType _ -> true
                    | _ -> false)
            replaceArgs, args |> List.map (fun arg ->
                if replaceArgs
                then com.GetUniqueVar("arg")
                else arg.Name)
        let replaceArgs, argIds =
            discardUnitArg args |> getTailCallArgIds com
        interface ITailCallOpportunity with
            member __.Label = name
            member __.Args = argIds
            member __.ReplaceArgs = replaceArgs
            member __.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id  -> name = id.Name | _ -> false

    let decisionTargetsReferencedMultipleTimes targets expr =
        let targetRefs = Dictionary()
        for i = 1 to (List.length targets) do
            targetRefs.Add(i - 1, 0)
        expr |> FableTransforms.deepExists (function
            | Fable.DecisionTreeSuccess(idx,_,_) ->
                let count = targetRefs.[idx]
                targetRefs.[idx] <- count + 1
                count > 0
            | _ -> false)

    let rec isJsStatement preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Value _ | Fable.Import _ | Fable.Cast _ | Fable.Test _ | Fable.IdentExpr _ | Fable.Function _
        | Fable.ObjectExpr _ | Fable.Operation _ | Fable.Get _ -> false

        | Fable.TryCatch _ | Fable.Debugger
        | Fable.Sequential _ | Fable.Let _ | Fable.Set _
        | Fable.Loop _ | Fable.Throw _ -> true

        | Fable.DecisionTreeSuccess _ -> preferStatement
        // TODO: Make it also statement if we have more than, say, 3 targets?
        // This will increase the chances to convert it into a switch
        | Fable.DecisionTree(_,targets) ->
            preferStatement
            || List.exists (snd >> (isJsStatement false)) targets

        | Fable.IfThenElse (_,thenExpr,elseExpr) ->
            preferStatement || isJsStatement false thenExpr || isJsStatement false elseExpr

    let addErrorAndReturnNull (com: ICompiler) (range: SourceLocation option) (error: string) =
        com.AddLog(error, Severity.Error, ?range=range, fileName=com.CurrentFile)
        NullLiteral () :> Expression

    let ident (id: Fable.Ident) =
        Identifier id.Name

    let identAsPattern (id: Fable.Ident): Pattern =
        Identifier id.Name :> Pattern

    let identAsExpr (id: Fable.Ident) =
        Identifier id.Name :> Expression

    let ofInt i =
        NumericLiteral(float i) :> Expression

    let memberFromName memberName: Expression * bool =
        if Naming.hasIdentForbiddenChars memberName
        then upcast StringLiteral memberName, true
        else upcast Identifier memberName, false

    let memberFromExpr (com: IBabelCompiler) ctx memberExpr: Expression * bool =
        match memberExpr with
        | Fable.Value(Fable.StringConstant name) -> memberFromName name
        | e -> com.TransformAsExpr(ctx, e), true

    let coreLibCall (com: IBabelCompiler) (ctx: Context) coreModule memb args =
        let callee = com.GetImportExpr(ctx, memb, coreModule, Fable.CoreLib)
        CallExpression(callee, args) :> Expression

    let get r left memberName =
        let expr, computed = memberFromName memberName
        MemberExpression(left, expr, computed, ?loc=r) :> Expression

    let getExpr r (object: Expression) (expr: Expression) =
        let expr, computed =
            match expr with
            | :? StringLiteral as e -> memberFromName e.value
            | e -> e, true
        MemberExpression(object, expr, computed, ?loc=r) :> Expression

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

    let jsObject methodName args =
        CallExpression(get None (Identifier "Object") methodName, args) :> Expression

    let coreUtil (com: IBabelCompiler) ctx methodName args =
        CallExpression(com.TransformImport(ctx, methodName, "Util", Fable.CoreLib), args) :> Expression

    let buildArray (com: IBabelCompiler) ctx typ (arrayKind: Fable.NewArrayKind) =
        match typ with
        | Fable.Number kind when com.Options.typedArrays ->
            let cons = getTypedArrayName com kind |> Identifier
            let args =
                match arrayKind with
                | Fable.ArrayValues args ->
                    [ List.map (fun e -> com.TransformAsExpr(ctx, e)) args
                      |> ArrayExpression :> Expression ]
                | Fable.ArrayAlloc(TransformExpr com ctx size) -> [size]
            NewExpression(cons, args) :> Expression
        | _ ->
            match arrayKind with
            | Fable.ArrayValues args ->
                List.map (fun e -> com.TransformAsExpr(ctx, e)) args
                |> ArrayExpression :> Expression
            | Fable.ArrayAlloc(TransformExpr com ctx size) ->
                upcast NewExpression(Identifier "Array", [size])

    let buildStringArray strings =
        strings
        |> List.map (fun x -> StringLiteral x :> Expression)
        |> ArrayExpression :> Expression

    let assign range left right =
        AssignmentExpression(AssignEqual, left, right, ?loc=range)
        :> Expression

    /// Immediately Invoked Function Expression
    let iife (com: IBabelCompiler) ctx (expr: Fable.Expr) =
        let _, body = com.TransformFunction(ctx, None, [], expr)
        CallExpression(ArrowFunctionExpression([], body), [])

    let multiVarDeclaration kind (namesAndValue: (string * Expression option) list) =
        let varDeclarators =
            List.distinctBy fst namesAndValue
            |> List.map (fun (name, value) ->
                VariableDeclarator(Identifier name, ?init=value))
        VariableDeclaration(kind, varDeclarators) :> Statement

    let varDeclaration (var: Pattern) (isMutable: bool) value =
        let kind = if isMutable then Let else Const
        VariableDeclaration(var, value, kind)

    let macroExpression range (txt: string) args =
        MacroExpression(txt, args, ?loc=range) :> Expression

    let getMemberArgsAndBody (com: IBabelCompiler) ctx name boundThis args hasSpread (body: Fable.Expr) =
        let args, body =
            match boundThis, args with
            | Some boundThis, (thisArg: Fable.Ident)::args ->
                let boundThis = { thisArg with Name = boundThis } |> Fable.IdentExpr
                // If the body doesn't contain closure replace calls to thisArg with `this`
                let containsClosures =
                    body |> FableTransforms.deepExists (function
                        | Fable.Function _ | Fable.ObjectExpr _ -> true
                        | _ -> false)
                let body =
                    if containsClosures
                    then Fable.Let([thisArg, boundThis], body)
                    else FableTransforms.replaceValues (Map [thisArg.Name, boundThis]) body
                args, body
            | _, args -> args, body
        let args, body = com.TransformFunction(ctx, name, args, body)
        let args =
            if not hasSpread
            then args
            else let args = List.rev args
                 (RestElement(args.Head) :> Pattern) :: args.Tail |> List.rev
        match body with
        | U2.Case1 e -> args, e
        | U2.Case2 e -> args, BlockStatement [ReturnStatement e]

    /// Wrap int expressions with `| 0` to help optimization of JS VMs
    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | :? NumericLiteral, _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number(Int8 | Int16 | Int32)
        | _, Fable.EnumType _ ->
            BinaryExpression(BinaryOrBitwise, e, NumericLiteral(0.)) :> Expression
        | _ -> e

    let makeFunctionExpression name args body: Expression =
        let id =
            match name with
            | Some name -> Some(Identifier name)
            | None -> None
        let body =
            match body with
            | U2.Case1 body -> body
            | U2.Case2 e -> BlockStatement [ReturnStatement e]
        upcast FunctionExpression(args, body, ?id=id)

    let optimizeTailCall (com: IBabelCompiler) (ctx: Context) (tc: ITailCallOpportunity) args =
        ctx.OptimizeTailCall()
        let zippedArgs = List.zip tc.Args args
        let tempVars =
            let rec checkCrossRefs acc = function
                | [] | [_] -> acc
                | (argId, _arg)::rest ->
                    rest |> List.exists (snd >> FableTransforms.deepExists
                        (function Fable.IdentExpr i -> argId = i.Name | _ -> false))
                    |> function true -> Map.add argId (com.GetUniqueVar()) acc | false -> acc
                    |> checkCrossRefs <| rest
            checkCrossRefs Map.empty zippedArgs
        [ for (argId, arg) in zippedArgs do
            let arg = com.TransformAsExpr(ctx, arg)
            match Map.tryFind argId tempVars with
            | Some tempVar ->
                yield varDeclaration (Identifier tempVar) false arg :> Statement
            | None ->
                yield assign None (Identifier argId) arg |> ExpressionStatement :> Statement
          for KeyValue(argId,tempVar) in tempVars do
            yield assign None (Identifier argId) (Identifier tempVar) |> ExpressionStatement :> Statement
          yield upcast ContinueStatement(Identifier tc.Label) ]

    let transformValue (com: IBabelCompiler) (ctx: Context) value: Expression =
        match value with
        | Fable.This _ -> upcast ThisExpression ()
        | Fable.Null _ -> upcast NullLiteral ()
        | Fable.UnitConstant -> upcast NullLiteral () // TODO: Use `void 0`?
        | Fable.BoolConstant x -> upcast BooleanLiteral (x)
        | Fable.CharConstant x -> upcast StringLiteral (string x)
        | Fable.StringConstant x -> upcast StringLiteral (x)
        | Fable.NumberConstant (x,_) ->
            if x < 0.
            // Negative numeric literals can give issues in Babel AST, see #1186
            then upcast UnaryExpression(UnaryMinus, NumericLiteral(x * -1.))
            else upcast NumericLiteral x
        | Fable.RegexConstant (source, flags) -> upcast RegExpLiteral (source, flags)
        | Fable.NewArray (arrayKind, typ) -> buildArray com ctx typ arrayKind
        | Fable.NewTuple vals -> buildArray com ctx Fable.Any (Fable.ArrayValues vals)
        | Fable.NewList (headAndTail, _) ->
            match headAndTail with
            | None -> upcast NullLiteral ()
            | Some(head, tail) -> Fable.ArrayValues [head; tail]
                                  |> buildArray com ctx Fable.Any
        | Fable.NewOption (value, t) ->
            match value with
            | Some (TransformExpr com ctx e) ->
                if mustWrapOption t
                then coreLibCall com ctx "Option" "some" [e]
                else e
            | None -> upcast NullLiteral ()
        | Fable.Enum(kind,_) ->
            match kind with
            | Fable.NumberEnum i -> ofInt i
            | Fable.StringEnum s -> upcast StringLiteral s
        | Fable.NewRecord(vals,ent,_) ->
            let members =
                (ent.FSharpFields, vals)
                ||> Seq.map2 (fun fi v -> makeStrConst fi.Name, v, Fable.ObjectValue)
                |> Seq.toList
            com.TransformObjectExpr(ctx, members)
        | Fable.NewUnion(vals,uci,_,_) ->
            let tag = Fable.Value(Fable.StringConstant uci.Name)
            Fable.ArrayValues (tag::vals) |> buildArray com ctx Fable.Any
        | Fable.NewErasedUnion(e,_) -> com.TransformAsExpr(ctx, e)

    let transformObjectExpr (com: IBabelCompiler) ctx members baseCall (boundThis: string option): Expression =
        let makeObjMethod kind prop computed hasSpread args body =
            let args, body = getMemberArgsAndBody com ctx None boundThis args hasSpread body
            ObjectMethod(kind, prop, args, body, computed=computed) |> U3.Case2 |> Some
        let pojo =
            members |> List.choose (fun (name, expr, kind) ->
                let prop, computed = memberFromExpr com ctx name
                match kind, expr with
                | Fable.ObjectValue, Fable.Function(Fable.Delegate args, body, _) ->
                    // Don't call the `makeObjMethod` helper here because function as values don't bind `this` arg
                    let args, body' = getMemberArgsAndBody com ctx None None args false body
                    ObjectMethod(ObjectMeth, prop, args, body', computed=computed) |> U3.Case2 |> Some
                | Fable.ObjectValue, TransformExpr com ctx value ->
                    ObjectProperty(prop, value, computed=computed) |> U3.Case1 |> Some
                | Fable.ObjectMethod hasSpread, Fable.Function(Fable.Delegate args, body, _) ->
                    makeObjMethod ObjectMeth prop computed hasSpread args body
                | Fable.ObjectGetter, Fable.Function(Fable.Delegate args, body, _) ->
                    makeObjMethod ObjectGetter prop computed false args body
                | Fable.ObjectSetter, Fable.Function(Fable.Delegate args, body, _) ->
                    makeObjMethod ObjectSetter prop computed false args body
                | kind, _ ->
                    sprintf "Object member has kind %A but value is not a function" kind
                    |> addError com None; None
            ) |> ObjectExpression
        match baseCall with
        | Some(TransformExpr com ctx baseCall) ->
            coreUtil com ctx "extend" [baseCall; pojo]
        | None -> pojo :> Expression

    let transformArgs (com: IBabelCompiler) ctx args spread =
        match args, spread with
        | [], _
        | [Fable.Value Fable.UnitConstant], _ -> []
        | [Fable.Value(Fable.NewTuple args)], Fable.TupleSpread ->
            List.map (fun e -> com.TransformAsExpr(ctx, e)) args
        | args, Fable.SeqSpread ->
            match List.rev args with
            | [] -> []
            // TODO: Check also lists?
            | Fable.Value(Fable.NewArray(Fable.ArrayValues spreadArgs,_))::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
            | last::rest ->
                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                rest @ [SpreadElement(com.TransformAsExpr(ctx, last))]
        | args, _ -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr t strategy babelExpr: Statement =
        match strategy with
        | None -> upcast ExpressionStatement babelExpr
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some Return -> upcast ReturnStatement(wrapIntExpression t babelExpr)
        | Some(Assign left) -> upcast ExpressionStatement(assign None left babelExpr)
        | Some(Target left) -> upcast ExpressionStatement(assign None left babelExpr)

    let transformOperation com ctx range opKind: Expression =
        match opKind with
        | Fable.UnaryOperation(op, TransformExpr com ctx expr) ->
            upcast UnaryExpression (op, expr, ?loc=range)
        | Fable.BinaryOperation(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast BinaryExpression (op, left, right, ?loc=range)
        | Fable.LogicalOperation(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            upcast LogicalExpression (op, left, right, ?loc=range)
        | Fable.Emit(emit, argInfo) ->
            match argInfo with
            | Some argInfo ->
                let args = transformArgs com ctx argInfo.Args argInfo.Spread
                match argInfo.ThisArg with
                | Some(TransformExpr com ctx thisArg) -> macroExpression range emit (thisArg::args)
                | None -> macroExpression range emit args
            | None -> macroExpression range emit []
        | Fable.Call(kind, argInfo) ->
            let args = transformArgs com ctx argInfo.Args argInfo.Spread
            match kind with
            | Fable.ConstructorCall(TransformExpr com ctx consExpr) ->
                upcast NewExpression(consExpr, args, ?loc=range)
            | Fable.StaticCall(TransformExpr com ctx funcExpr) ->
                let args =
                    match argInfo.ThisArg with
                    | Some(TransformExpr com ctx thisArg) -> thisArg::args
                    | None -> args
                if argInfo.IsSiblingConstructorCall
                then upcast CallExpression(get None funcExpr "call", (upcast Identifier "this")::args, ?loc=range)
                else upcast CallExpression(funcExpr, args, ?loc=range)
            | Fable.InstanceCall membExpr ->
                match argInfo.ThisArg with
                | None -> addErrorAndReturnNull com range "InstanceCall with empty this argument"
                | Some(TransformExpr com ctx thisArg) ->
                    match membExpr with
                    | None -> upcast CallExpression(thisArg, args, ?loc=range)
                    | Some(TransformExpr com ctx m) ->
                        upcast CallExpression(getExpr None thisArg m, args, ?loc=range)
        | Fable.CurriedApply(TransformExpr com ctx applied, args) ->
            match transformArgs com ctx args Fable.NoSpread with
            | [] -> upcast CallExpression(applied, [], ?loc=range)
            | head::rest ->
                let baseExpr = CallExpression(applied, [head], ?loc=range) :> Expression
                (baseExpr, rest) ||> List.fold (fun e arg ->
                    CallExpression(e, [arg], ?loc=range) :> Expression)

    let transformOperationAsStatements com ctx range t returnStrategy opKind =
        let argsLen (i: Fable.ArgInfo) =
            List.length i.Args + (if Option.isSome i.ThisArg then 1 else 0)
        match returnStrategy, ctx.TailCallOpportunity, opKind with
        | Some Return, Some tc, Fable.Call(Fable.StaticCall funcExpr, argInfo)
                                when not argInfo.IsSiblingConstructorCall
                                && tc.IsRecursiveRef(funcExpr)
                                && argsLen argInfo = List.length tc.Args ->
            let args =
                match argInfo.ThisArg with
                | Some thisArg -> thisArg::argInfo.Args
                | None -> argInfo.Args
            optimizeTailCall com ctx tc args
        | Some Return, Some tc, Fable.CurriedApply(funcExpr, args)
                                when tc.IsRecursiveRef(funcExpr)
                                && List.sameLength args tc.Args ->
            optimizeTailCall com ctx tc args
        | _ ->
            [transformOperation com ctx range opKind |> resolveExpr t returnStrategy]

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IBabelCompiler) ctx ret expr: BlockStatement =
        com.TransformAsStatements(ctx, ret, expr) |> BlockStatement

    let transformTryCatch com ctx returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause (ident param,
                    transformBlock com ctx returnStrategy body, ?loc=body.Range))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [TryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer) :> Statement]

    // Even if IfStatement doesn't enforce it, compile both branches as blocks
    // to prevent conflict (e.g. `then` doesn't become a block while `else` does)
    let rec transformIfStatement (com: IBabelCompiler) ctx ret guardExpr thenStmnt elseStmnt =
        let guardExpr = com.TransformAsExpr(ctx, guardExpr)
        let thenStmnt = transformBlock com ctx ret thenStmnt
        let elseStmnt =
            match elseStmnt: Fable.Expr with
            | Fable.Value(Fable.Null _) when Option.isNone ret -> None
            | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt) ->
                transformIfStatement com ctx ret guardExpr thenStmnt elseStmnt
                :> Statement |> Some
            | e -> transformBlock com ctx ret e :> Statement |> Some
        IfStatement(guardExpr, thenStmnt, ?alternate=elseStmnt)

    let transformGet (com: IBabelCompiler) ctx range typ expr (getKind: Fable.GetKind) =
        let expr = com.TransformAsExpr(ctx, expr)
        match getKind with
        | Fable.ExprGet(TransformExpr com ctx prop) -> getExpr range expr prop
        // TODO: Check if list is empty (#1341)?
        // If null represents an empty list, it'll throw anyway
        | Fable.ListHead -> getExpr range expr (ofInt 0)
        | Fable.ListTail -> getExpr range expr (ofInt 1)
        | Fable.RecordGet(fi,_) -> get range expr fi.Name
        | Fable.TupleGet index -> getExpr range expr (ofInt index)
        | Fable.OptionValue ->
            if mustWrapOption typ
            then coreLibCall com ctx "Option" "value" [expr]
            else expr
        | Fable.UnionTag _ -> getExpr range expr (ofInt 0)
        | Fable.UnionField(field, uci, _) ->
            let fieldName = field.Name
            let index =
                uci.UnionCaseFields
                |> Seq.findIndex (fun fi -> fi.Name = fieldName)
            getExpr range expr (index + 1 |> ofInt)

    let transformSet (com: IBabelCompiler) ctx range var (value: Fable.Expr) (setKind: Fable.SetKind) =
        let var = com.TransformAsExpr(ctx, var)
        let value = com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type
        let var =
            match setKind with
            | Fable.VarSet -> var
            | Fable.RecordSet(field, _) -> get None var field.Name
            | Fable.ExprSet(TransformExpr com ctx e) -> getExpr None var e
        assign range var value

    let getSetReturnStrategy com ctx (TransformExpr com ctx expr) = function
        | Fable.VarSet -> Assign expr
        | Fable.ExprSet(TransformExpr com ctx prop) -> getExpr None expr prop |> Assign
        | Fable.RecordSet(fi,_) -> get None expr fi.Name |> Assign

    let transformImport (com: IBabelCompiler) ctx (selector: string) (path: string) kind =
        let selector, parts =
            let parts = Array.toList(selector.Split('.'))
            parts.Head, parts.Tail
        com.GetImportExpr(ctx, selector, path, kind)
        |> getParts parts

    let transformBindingExprBody (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        | Fable.Function(args, body, _) ->
            let args =
                match args with
                | Fable.Lambda arg -> [arg]
                | Fable.Delegate args -> args
            com.TransformFunction(ctx, Some var.Name, args, body)
            ||> makeFunctionExpression (Some var.Name)
        // Check imports with name placeholder
        | Fable.Import(Naming.placeholder, path, kind, _) ->
            transformImport com ctx var.Name path kind
        | _ ->
            com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type

    let transformBindingAsExpr (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        transformBindingExprBody com ctx var value
        |> assign None (ident var)

    let transformBindingAsStatements (com: IBabelCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isJsStatement false value then
            let var = ident var
            let decl = VariableDeclaration var :> Statement
            let body = com.TransformAsStatements(ctx, Some(Assign var), value)
            decl::body
        else
            let value = transformBindingExprBody com ctx var value
            [varDeclaration (ident var) var.IsMutable value :> Statement]

    let transformTest (com: IBabelCompiler) ctx range kind expr: Expression =
        match kind with
        | Fable.TypeTest t
        | Fable.ErasedUnionTest t ->
            let test = Replacements.makeTypeTest com range expr t
            com.TransformAsExpr(ctx, test)
        | Fable.OptionTest nonEmpty
        | Fable.ListTest nonEmpty ->
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            upcast BinaryExpression(op, com.TransformAsExpr(ctx, expr), NullLiteral(), ?loc=range)
        | Fable.UnionCaseTest(uci, _) ->
            let tag1 = getExpr range (com.TransformAsExpr(ctx, expr)) (ofInt 0)
            upcast BinaryExpression(BinaryEqualStrict, tag1, StringLiteral uci.Name, ?loc=range)

    let transformSwitch (com: IBabelCompiler) ctx returnStrategy evalExpr cases defaultCase: Statement =
        let cases =
            cases |> List.map (fun (guard, expr) ->
                let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                SwitchCase(caseBody, com.TransformAsExpr(ctx, guard)))
        let cases =
            match defaultCase with
            | Some expr ->
                let defaultCaseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                cases @ [SwitchCase defaultCaseBody]
            | None -> cases
        SwitchStatement(com.TransformAsExpr(ctx, evalExpr), cases) :> Statement

    let getDecisionTarget (ctx: Context) targetIndex boundValues =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf "Cannot find DecisionTree target %i" targetIndex
        | Some(idents, _) when not(List.sameLength idents boundValues) ->
            failwithf "Found DecisionTree target %i but length of bindings differ" targetIndex
        | Some(idents, target) -> idents, target

    let getDecisionTargetAndBindValues (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex boundValues
        let bindings, replacements =
            (([], Map.empty), idents, boundValues)
            |||> List.fold2 (fun (bindings, replacements) ident expr ->
                if hasDoubleEvalRisk expr // && isReferencedMoreThan 1 ident.Name body
                then (ident, expr)::bindings, replacements
                else bindings, Map.add ident.Name expr replacements)
        let target = FableTransforms.replaceValues replacements target
        bindings, target

    let transformDecisionTreeSuccessAsExpr (com: IBabelCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings -> com.TransformAsExpr(ctx, Fable.Let(bindings, target))

    let transformDecisionTreeSuccessAsStatements (com: IBabelCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement list =
        match returnStrategy with
        | Some(Target targetId) ->
            let idents, _ = getDecisionTarget ctx targetIndex boundValues
            let assignments =
                List.zip idents boundValues |> List.map (fun (id, TransformExpr com ctx value) ->
                    assign None (ident id) value |> ExpressionStatement :> Statement)
            let targetAssignment = assign None targetId (ofInt targetIndex) |> ExpressionStatement :> Statement
            targetAssignment :: assignments
        | ret ->
            let bindings, target = getDecisionTargetAndBindValues ctx targetIndex boundValues
            let bindings = bindings |> List.rev |> List.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            bindings @ (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsExpr (com: IBabelCompiler) (ctx: Context) targets expr: Expression =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let transformDecisionTreeAsStaments (com: IBabelCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) treeExpr: Statement list =
        let transformCase t (caseExpr, targetIndex, boundValues) =
            caseExpr, Fable.DecisionTreeSuccess(targetIndex, boundValues, t)

        let transformDecisionTreeAsSwitch expr =
            let (|Equals|_|) = function
                | Fable.Operation(Fable.BinaryOperation(BinaryEqualStrict, Fable.IdentExpr ident, right), _, _) ->
                    Some(ident, right)
                | _ -> None
            let rec checkInner cases (evalIdent: Fable.Ident) = function
                | Fable.IfThenElse(Equals(evalIdent2, caseExpr),
                                   Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr)
                                        when evalIdent.Name = evalIdent2.Name ->
                    match treeExpr with
                    | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                        let cases = (caseExpr, targetIndex, boundValues)::cases |> List.rev
                        Some(evalIdent, cases, (defaultTargetIndex, defaultBoundValues))
                    | treeExpr -> checkInner ((caseExpr, targetIndex, boundValues)::cases) evalIdent treeExpr
                | _ -> None
            match expr with
            | Fable.IfThenElse(Equals(evalIdent, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr) ->
                match checkInner [caseExpr, targetIndex, boundValues] evalIdent treeExpr with
                | Some(evalIdent, cases, defaultCase) ->
                    Some(Fable.IdentExpr evalIdent, cases, defaultCase)
                | None -> None
            | _ -> None

        // If some targets are referenced multiple times, host bound idents,
        // resolve the decision index and compile the targets as a switch
        if decisionTargetsReferencedMultipleTimes targets treeExpr then
            // Declare $target and bound idents
            let targetId = com.GetUniqueVar("target")
            let varDeclaration =
                let boundIdents = targets |> List.collect (fun (idents,_) ->
                    idents |> List.map (fun i -> i.Name, None))
                multiVarDeclaration Var ((targetId, None)::boundIdents)
            // Transform targets as switch
            let switch2 =
                // TODO: Declare the last case as the default case?
                let cases = targets |> List.mapi (fun i (_,target) -> makeIntConst i, target)
                transformSwitch com ctx returnStrategy (makeIdent targetId |> Fable.IdentExpr) cases None
            // Transform decision tree
            let targetAssign = Target(Identifier targetId)
            match transformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                if targets |> List.forall (fun (boundValues,_) -> List.isEmpty boundValues) then
                    let cases = cases |> List.map (fun (caseExpr, idx, _) ->
                        caseExpr, (List.item idx targets |> snd))
                    let defaultCase = List.item defaultIndex targets |> snd
                    [transformSwitch com ctx returnStrategy evalExpr cases (Some defaultCase)]
                else
                    let ctx = { ctx with DecisionTargets = targets }
                    let cases = List.map (transformCase (Fable.Number Int32)) cases
                    let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number Int32)
                    let switch1 = transformSwitch com ctx (Some targetAssign) evalExpr cases (Some defaultCase)
                    [varDeclaration; switch1; switch2]
            | None ->
                let ctx = { ctx with DecisionTargets = targets }
                let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
                varDeclaration::(decisionTree @ [switch2])
        else
            let ctx = { ctx with DecisionTargets = targets }
            match transformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                let cases = List.map (transformCase treeExpr.Type) cases
                let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, treeExpr.Type)
                [transformSwitch com ctx returnStrategy evalExpr cases (Some defaultCase)]
            | None ->
                com.TransformAsStatements(ctx, returnStrategy, treeExpr)

    let rec transformAsExpr (com: IBabelCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        // TODO!!!: Warn an unhandlend cast has reached Babel pass
        | Fable.Cast(expr, _) -> transformAsExpr com ctx expr

        | Fable.Value kind -> transformValue com ctx kind

        | Fable.IdentExpr ident -> upcast Identifier ident.Name

        | Fable.Import(selector, path, kind, _) ->
            transformImport com ctx selector path kind

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Function(FunctionArgs args, body, name) ->
            com.TransformFunction(ctx, name, args, body) ||> makeFunctionExpression name

        | Fable.ObjectExpr (members, _, baseCall) ->
            Some "this" |> transformObjectExpr com ctx members baseCall

        | Fable.Operation(opKind, _, range) ->
            transformOperation com ctx range opKind

        | Fable.Get(expr, getKind, typ, range) ->
            transformGet com ctx range typ expr getKind

        | Fable.IfThenElse (TransformExpr com ctx guardExpr,
                            TransformExpr com ctx thenExpr,
                            TransformExpr com ctx elseExpr) ->
            upcast ConditionalExpression(guardExpr, thenExpr, elseExpr)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(var, setKind, value, range) ->
            transformSet com ctx range var value setKind

        | Fable.Let(bindings, body) ->
            if ctx.HoistVars(List.map fst bindings) then
                let values = bindings |> List.map (fun (id, value) ->
                    transformBindingAsExpr com ctx id value)
                upcast SequenceExpression(values @ [com.TransformAsExpr(ctx, body)])
            else upcast iife com ctx expr

        | Fable.Sequential exprs ->
            List.map (fun e -> com.TransformAsExpr(ctx, e)) exprs
            |> SequenceExpression :> Expression

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.Debugger _ | Fable.Throw _ | Fable.Loop _ | Fable.TryCatch _ ->
            iife com ctx expr :> Expression

    let rec transformAsStatements (com: IBabelCompiler) ctx returnStrategy
                                    (expr: Fable.Expr): Statement list =
        match expr with
        // TODO!!!: Warn an unhandlend cast has reached Babel pass
        | Fable.Cast(expr, _) ->
            transformAsStatements com ctx returnStrategy expr

        | Fable.Value kind ->
            [transformValue com ctx kind |> resolveExpr kind.Type returnStrategy]

        | Fable.IdentExpr ident ->
            [Identifier ident.Name :> Expression |> resolveExpr ident.Type returnStrategy]

        | Fable.Import(selector, path, kind, t) ->
            [transformImport com ctx selector path kind |> resolveExpr t returnStrategy]

        | Fable.Test(expr, kind, range) ->
            [transformTest com ctx range kind expr |> resolveExpr Fable.Boolean returnStrategy]

        | Fable.Function(FunctionArgs args, body, name) ->
            [com.TransformFunction(ctx, name, args, body)
             ||> makeFunctionExpression name |> resolveExpr expr.Type returnStrategy]

        | Fable.ObjectExpr (members, t, baseCall) ->
            [Some "this" |> transformObjectExpr com ctx members baseCall |> resolveExpr t returnStrategy]

        | Fable.Operation(callKind, t, range) ->
            transformOperationAsStatements com ctx range t returnStrategy callKind

        | Fable.Get(expr, getKind, t, range) ->
            [transformGet com ctx range t expr getKind |> resolveExpr t returnStrategy]

        | Fable.Let(bindings, body) ->
            let bindings = bindings |> List.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            bindings @ (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, setKind, value, _range) ->
            let ret = getSetReturnStrategy com ctx expr setKind
            com.TransformAsStatements(ctx, Some ret, value)

        | Fable.Throw(TransformExpr com ctx ex, _, range) ->
            [ThrowStatement(ex, ?loc=range) :> Statement]

        | Fable.Debugger ->
            [DebuggerStatement() :> Statement]

        // Even if IfStatement doesn't enforce it, compile both branches as blocks
        // to prevent conflicts (e.g. `then` doesn't become a block while `else` does)
        | Fable.IfThenElse(guardExpr, thenStmnt, elseStmnt) ->
            let asStatement =
                match returnStrategy with
                | None -> true
                | Some(Target _) -> true // Compile as statement so values can be bound
                | Some(Assign _) -> isJsStatement false expr
                | Some Return -> isJsStatement (Option.isSome ctx.TailCallOpportunity) expr
            if asStatement then
                [transformIfStatement com ctx returnStrategy guardExpr thenStmnt elseStmnt :> Statement ]
            else
                let guardExpr = transformAsExpr com ctx guardExpr
                let thenExpr = transformAsExpr com ctx thenStmnt
                let elseExpr = transformAsExpr com ctx elseStmnt
                [ConditionalExpression(guardExpr, thenExpr, elseExpr) |> resolveExpr thenStmnt.Type returnStrategy]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapi (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                com.TransformAsStatements(ctx, ret, statement))
            |> List.concat

        | Fable.TryCatch (body, catch, finalizer) ->
            transformTryCatch com ctx returnStrategy (body, catch, finalizer)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsStaments com ctx returnStrategy targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsStatements com ctx returnStrategy idx boundValues

        | Fable.Loop (loopKind, range) ->
            match loopKind with
            | Fable.While (TransformExpr com ctx guard, body) ->
                WhileStatement(guard, transformBlock com ctx None body, ?loc=range) :> Statement
            | Fable.For (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp) ->
                let op1, op2 =
                    if isUp
                    then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                    else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus
                ForStatement(
                    transformBlock com ctx None body,
                    start |> varDeclaration (ident var) true |> U2.Case1,
                    BinaryExpression (op1, ident var, limit),
                    UpdateExpression (op2, false, ident var), ?loc=range) :> Statement
            |> List.singleton

    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr) =
        let tailcallChance =
            Option.map (fun name ->
                NamedTailCallOpportunity(com, name, args) :> ITailCallOpportunity) name
        let args = discardUnitArg args |> List.map ident
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with TailCallOpportunity = tailcallChance
                       HoistVars = fun ids -> declaredVars.AddRange(ids); true
                       OptimizeTailCall = fun () -> isTailCallOptimized <- true }
        let body: U2<BlockStatement, Expression> =
            if body.Type = Fable.Unit
            then transformBlock com ctx None body |> U2.Case1
            elif isJsStatement (Option.isSome tailcallChance) body
            then transformBlock com ctx (Some Return) body |> U2.Case1
            else transformAsExpr com ctx body |> U2.Case2
        let args, body =
            match isTailCallOptimized, tailcallChance, body with
            | true, Some tc, U2.Case1 body ->
                let args, body =
                    if tc.ReplaceArgs then
                        let varDeclaration =
                            List.zip args tc.Args |> List.map (fun (arg, tempVar) ->
                                arg.name, Some(Identifier tempVar :> Expression))
                            |> multiVarDeclaration Const
                        tc.Args |> List.map Identifier, BlockStatement(varDeclaration::body.body)
                    else args, body
                args, LabeledStatement(Identifier tc.Label, WhileStatement(BooleanLiteral true, body))
                :> Statement |> List.singleton |> BlockStatement |> U2.Case1
            | _ -> args, body
        let body =
            if declaredVars.Count = 0
            then body
            else
                let varDeclStatement =
                    multiVarDeclaration Var [for v in declaredVars -> v.Name, None]
                let bodyStatements =
                    match body with
                    | U2.Case1 bodyBlock -> bodyBlock.body
                    | U2.Case2 bodyExpr -> [ReturnStatement(bodyExpr, ?loc=bodyExpr.loc) :> Statement]
                BlockStatement(varDeclStatement :: bodyStatements) |> U2.Case1
        args |> List.map (fun x -> x :> Pattern), body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = macroExpression None "process.argv.slice(2)" []
        let main = CallExpression (funcExpr, [argv]) :> Expression
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(macroExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        ExpressionStatement(main) :> Statement

    let declareModuleMember isPublic name isMutable (expr: Expression) =
        let privateIdent = Identifier name
        let decl: Declaration =
            match expr with
            | :? ClassExpression as e ->
                upcast ClassDeclaration(e.body, privateIdent,
                    ?super=e.superClass, ?typeParams=e.typeParameters)
            | :? FunctionExpression as e ->
                upcast FunctionDeclaration(privateIdent, e.``params``, e.body)
            | _ -> upcast varDeclaration privateIdent isMutable expr
        if not isPublic
        then U2.Case1 (decl :> Statement)
        else
            ExportNamedDeclaration(decl)
            :> ModuleDeclaration |> U2.Case2

    let transformModuleFunction (com: IBabelCompiler) ctx (info: Fable.ValueDeclarationInfo) args body =
        let args, body = getMemberArgsAndBody com ctx (Some info.Name) None args info.HasSpread body
        // Don't lexically bind `this` (with arrow function) or it will fail with extension members
        let expr: Expression = upcast FunctionExpression(args, body)
        declareModuleMember info.IsPublic info.Name false expr

    let transformAction (com: IBabelCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        let hasVarDeclarations =
            statements |> List.exists (function
                | :? VariableDeclaration -> true
                | _ -> false)
        if hasVarDeclarations then
            [ CallExpression(FunctionExpression([], BlockStatement(statements)), [])
              |> ExpressionStatement :> Statement |> U2.Case1 ]
        else List.map U2.Case1 statements

    let transformOverride (com: IBabelCompiler) ctx (info: Fable.OverrideDeclarationInfo) args body =
        let defineGetterOrSetter kind funcCons propName funcExpr =
            jsObject "defineProperty"
                [ get None funcCons "prototype"
                  StringLiteral propName
                  ObjectExpression [ObjectProperty(StringLiteral kind, funcExpr) |> U3.Case1] ]
        let funcCons = Identifier info.EntityName :> Expression
        let funcExpr =
            let hasSpread =
                match info.Kind with
                | Fable.ObjectMethod hasSpread -> hasSpread
                | _ -> false
            let args, body = getMemberArgsAndBody com ctx None (Some "this") args hasSpread body
            FunctionExpression(args, body)
        match info.Kind with
        | Fable.ObjectGetter -> defineGetterOrSetter "get" funcCons info.Name funcExpr
        | Fable.ObjectSetter -> defineGetterOrSetter "set" funcCons info.Name funcExpr
        | _ ->
            let protoMember =
                match info.Name with
                | Naming.StartsWith "Symbol." symbolName -> get None (Identifier "Symbol") symbolName
                | "ToString" -> upcast StringLiteral "toString"
                | name -> upcast StringLiteral name
            let protoMember = getExpr None (get None funcCons "prototype") protoMember
            assign None protoMember funcExpr
        |> ExpressionStatement :> Statement
        |> U2<_,ModuleDeclaration>.Case1 |> List.singleton

    let transformImplicitConstructor (com: IBabelCompiler) ctx (info: Fable.ImplicitConstructorDeclarationInfo) args body =
        let funcCons = Identifier info.EntityName :> Expression
        let thisIdent = Identifier "this" :> Expression
        let originalCons =
            let args, body = getMemberArgsAndBody com ctx None None args info.HasSpread body
            match info.BaseConstructor with
            | None -> FunctionExpression(args, body) :> Expression
            | Some({ BaseConsRef = TransformExpr com ctx baseCons } as b) ->
                let baseArgs =
                    if b.BaseConsHasSpread then Fable.SeqSpread else Fable.NoSpread
                    |> transformArgs com ctx b.BaseConsArgs
                let baseCall =
                    CallExpression(get None baseCons "call", thisIdent::baseArgs)
                    |> ExpressionStatement :> Statement
                FunctionExpression(args, BlockStatement(baseCall::body.body)) :> Expression
        let argIdents, argExprs: Pattern list * Expression list =
            match args with
            | [] -> [], []
            | [unitArg] when unitArg.Type = Fable.Unit -> [], []
            | args when info.HasSpread ->
                let args = List.rev args
                (RestElement(ident args.Head) :> Pattern) :: (List.map identAsPattern args.Tail) |> List.rev,
                (SpreadElement(ident args.Head) :> Expression) :: (List.map identAsExpr args.Tail) |> List.rev
            | args -> List.map identAsPattern args, List.map identAsExpr args
        let exposedCons =
            FunctionExpression(argIdents,
                BlockStatement [
                    ReturnStatement(
                        ConditionalExpression(
                            BinaryExpression(BinaryInstanceOf, thisIdent, funcCons),
                            CallExpression(get None funcCons "call", thisIdent::argExprs),
                            NewExpression(funcCons, argExprs))
                    )])
        [ yield declareModuleMember info.IsPublic info.EntityName false originalCons
          match info.BaseConstructor with
          | Some { BaseEntityRef = TransformExpr com ctx baseExpr } ->
            let proto = jsObject "create" [get None baseExpr "prototype"]
            yield assign None (get None funcCons "prototype") proto |> ExpressionStatement :> Statement |> U2.Case1
          | None -> ()
          yield declareModuleMember info.IsPublic info.Name false exposedCons ]

    let transformInterfaceCast (com: IBabelCompiler) ctx (info: Fable.InterfaceCastDeclarationInfo) members =
        let boundThis = Identifier "$this"
        let castedObj = transformObjectExpr com ctx members None (Some boundThis.name)
        let funcExpr =
            let returnedObj =
                match info.InheritedInterfaces with
                | [] -> castedObj
                | otherCasts ->
                    (otherCasts, [castedObj]) ||> List.foldBack (fun cast acc ->
                        (CallExpression(Identifier cast, [boundThis]) :> Expression)::acc
                    ) |> coreUtil com ctx "extend"
            let body = BlockStatement [ReturnStatement returnedObj]
            FunctionExpression([boundThis], body) :> Expression
        [declareModuleMember info.IsPublic info.Name false funcExpr]

    let transformDeclarations (com: IBabelCompiler) ctx decls =
        decls |> List.collect (function
            | Fable.ActionDeclaration e ->
                transformAction com ctx e
            | Fable.ValueDeclaration(value, info) ->
                match value with
                // Mutable public values must be compiled as functions (see #986)
                // because values imported from ES2015 modules cannot be modified
                | value when info.IsMutable && info.IsPublic ->
                    Replacements.createAtom value
                    |> transformAsExpr com ctx
                    |> declareModuleMember true info.Name false
                    |> List.singleton
                | Fable.Function(Fable.Delegate args, body, _) ->
                    [transformModuleFunction com ctx info args body]
                | _ ->
                    let value = transformAsExpr com ctx value
                    [declareModuleMember info.IsPublic info.Name info.IsMutable value]
            | Fable.ImplicitConstructorDeclaration(args, body, info) ->
                transformImplicitConstructor com ctx info args body
            | Fable.OverrideDeclaration(args, body, info) ->
                transformOverride com ctx info args body
            | Fable.InterfaceCastDeclaration(members, info) ->
                transformInterfaceCast com ctx info members)

    let transformImports (imports: Import seq): U2<Statement, ModuleDeclaration> list =
        imports |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier(localId)
                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId) |> U3.Case3
                    | "default" | "" -> ImportDefaultSpecifier(localId) |> U3.Case2
                    | memb -> ImportSpecifier(localId, Identifier memb) |> U3.Case1)
            import.Path, specifier)
        |> Seq.groupBy (fun (path, _) -> path)
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    let t =
                        match x with
                        | U3.Case1 x -> x.``type``
                        | U3.Case2 x -> x.``type``
                        | U3.Case3 x -> x.``type``
                    match t with
                    | "ImportNamespaceSpecifier" -> mems, defs, x::alls
                    | "ImportDefaultSpecifier" -> mems, x::defs, alls
                    | _ -> x::mems, defs, alls)
            // There seem to be errors if we mix member, default and namespace imports
            // so we must issue an import statement for each kind
            match [mems; defs; alls] with
            | [[];[];[]] ->
                // No specifiers, so this is just a import for side effects
                [ImportDeclaration([], StringLiteral path) :> ModuleDeclaration |> U2.Case2]
            | specifiers ->
                specifiers |> List.choose (function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(specifiers, StringLiteral path)
                    :> ModuleDeclaration |> U2.Case2 |> Some))
        |> Seq.toList

    let makeCompiler (com: ICompiler) =
        let getLocalIdent (ctx: Context) (imports: Dictionary<string,Import>) (path: string) (selector: string) =
            match selector with
            | "" -> None
            | "*" | "default" ->
                let x = path.TrimEnd('/')
                x.Substring(x.LastIndexOf '/' + 1) |> Some
            | selector -> Some selector
            |> Option.map (Naming.sanitizeIdent (fun s ->
                ctx.File.UsedVarNames.Contains s
                    || (imports.Values |> Seq.exists (fun i -> i.LocalIdent = Some s))))

        let dependencies = HashSet<string>()
        let imports = Dictionary<string,Import>()

        { new IBabelCompiler with
            member __.GetImportExpr(ctx, selector, path, kind) =
                match imports.TryGetValue(path + "::" + selector) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> upcast Identifier(localIdent)
                    | None -> upcast NullLiteral ()
                | false, _ ->
                    let localId = getLocalIdent ctx imports path selector
                    let sanitizedPath =
                        match kind with
                        | Fable.CustomImport | Fable.Internal _ -> path
                        | Fable.CoreLib -> com.FableCore + "/" + path + Naming.targetFileExtension
                    let i =
                      { Selector =
                            if selector = Naming.placeholder
                            then "`importMember` must be assigned to a variable"
                                 |> addError com None; selector
                            else selector
                        LocalIdent = localId
                        Path = sanitizedPath }
                    imports.Add(path + "::" + selector, i)
                    match localId with
                    | Some localId -> upcast Identifier(localId)
                    | None -> upcast NullLiteral ()
            member __.GetAllImports() = upcast imports.Values
            member __.GetAllDependencies() = upcast dependencies
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformImport(ctx, selector, path, kind) = transformImport bcom ctx selector path kind
            member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformObjectExpr(ctx, members, baseCall, boundThis) = transformObjectExpr bcom ctx members baseCall boundThis
        interface ICompiler with
            member __.Options = com.Options
            member __.FableCore = com.FableCore
            member __.CurrentFile = com.CurrentFile
            member __.GetUniqueVar(name) = com.GetUniqueVar(?name=name)
            member __.GetRootModule(fileName) = com.GetRootModule(fileName)
            member __.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)
        }

module Compiler =
    open Util

    let createFacade (sourceFiles: string[]) (facadeFile: string) =
        let decls =
            let importFile = Array.last sourceFiles
            StringLiteral(Path.getRelativeFileOrDirPath false facadeFile false importFile)
            |> ExportAllDeclaration :> ModuleDeclaration |> U2.Case2 |> List.singleton
        Program(facadeFile, decls, sourceFiles=sourceFiles)

    let transformFile (com: ICompiler) (file: Fable.File) =
        try
            // let t = PerfTimer("Fable > Babel")
            let com = makeCompiler com
            let ctx =
              { File = file
                DecisionTargets = []
                HoistVars = fun _ -> false
                TailCallOpportunity = None
                OptimizeTailCall = fun () -> () }
            let rootDecls = transformDeclarations com ctx file.Declarations
            let importDecls = transformImports <| com.GetAllImports()
            let dependencies =
                com.GetAllDependencies()
                |> Seq.append file.Dependencies
                |> Seq.toArray
            Program(file.SourcePath, importDecls@rootDecls, dependencies=dependencies)
        with
        | ex -> exn (sprintf "%s (%s)" ex.Message file.SourcePath, ex) |> raise
