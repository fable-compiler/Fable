module rec Fable.Transforms.Fable2Dart

open Fable
open Fable.AST
open Fable.AST.Dart
open System.Collections.Generic
open Fable.Transforms.AST

type ReturnStrategy =
    | Return
    | ReturnVoid
    | Assign of Expression
    | Target of Ident

type ArgsInfo =
    | CallInfo of Fable.CallInfo
    | NoCallInfo of args: Fable.Expr list

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
  { RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string> }

type Context =
  { File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit }

type MemberKind =
    | ClassConstructor
    | NonAttached of funcName: string
    | Attached of isStatic: bool

type IDartCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Import list
    abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Expression
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement list
    abstract TransformImport: Context * selector:string * path:string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> Ident list * Statement list
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Util =
    let (|TransformExpr|) (com: IDartCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, _) -> Some([arg], body)
        | Fable.Delegate(args, body, _) -> Some(args, body)
        | _ -> None

    let (|Lets|_|) = function
        | Fable.Let(ident, value, body) -> Some([ident, value], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArgument && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    let addErrorAndReturnNull (com: Compiler) (range: SourceLocation option) (error: string) =
        addError com [] range error
        NullLiteral |> Literal

    // TODO: Check conversions like ToString > toString
    let get (_: SourceLocation option) left memberName =
        PropertyAccess(left, memberName)

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

    let getUniqueNameInRootScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name)
            || ctx.UsedNames.DeclarationScopes.Contains(name))
        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.CurrentDeclarationScope.Contains(name))
        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity(_com: Compiler, ctx, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds = discardUnitArg args |> List.map (fun arg ->
            getUniqueNameInDeclarationScope ctx (arg.Name + "_mut"))
        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds
            member _.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id -> name = id.Name | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let rec isStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Unresolved _
        | Fable.Value _ | Fable.Import _  | Fable.IdentExpr _
        | Fable.Lambda _ | Fable.Delegate _ | Fable.ObjectExpr _
        | Fable.Call _ | Fable.CurriedApply _ | Fable.Operation _
        | Fable.Get _ | Fable.Test _ -> false

        | Fable.TypeCast(e,_) -> isStatement ctx preferStatement e

        | Fable.Set _ -> true // TODO: Depends on language target

        | Fable.TryCatch _
        | Fable.Sequential _ | Fable.Let _ | Fable.LetRec _
        | Fable.ForLoop _ | Fable.WhileLoop _ -> true

        | Fable.Extended(kind, _) ->
            match kind with
            | Fable.Throw _ // TODO: Depends on language target
            | Fable.Debugger | Fable.RegionStart _ -> true
            | Fable.Curry _ -> false

        // TODO: If IsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i,_,_) -> i.IsStatement

        | Fable.DecisionTreeSuccess(targetIndex,_, _) ->
            getDecisionTarget ctx targetIndex
            |> snd |> isStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_,targets) ->
            preferStatement
            || List.exists (snd >> (isStatement ctx false)) targets

        | Fable.IfThenElse(_,thenExpr,elseExpr,_) ->
            preferStatement || isStatement ctx false thenExpr || isStatement ctx false elseExpr

    let isInt64OrLess = function
        | Fable.Number(kind, _) ->
            match kind with
            | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64 -> true
            | Float32 | Float64 | Decimal | NativeInt | UNativeInt | BigInt -> false
        | _ -> false

    let makeAnonymousFunction ((args: Ident list), (body: Statement list)): Expression =
        // TODO: gen params
        // TODO: Check if body is just a single return statement
        AnonymousFunction(args, Choice1Of2 body, [])

    let assign (_range: SourceLocation option) left right =
        AssignmentExpression(left, AssignEqual, right)

    /// Immediately Invoked Function Expression
    // let iife (com: IDartCompiler) ctx (expr: Fable.Expr) =
    //     let _, body = com.TransformFunction(ctx, None, [], expr)
    //     // Use an arrow function in case we need to capture `this`
    //     Expression.callExpression(Expression.arrowFunctionExpression([||], body), [||])

    // let optimizeTailCall (com: IDartCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
    //     let rec checkCrossRefs tempVars allArgs = function
    //         | [] -> tempVars
    //         | (argId, _arg)::rest ->
    //             let found = allArgs |> List.exists (FableTransforms.deepExists (function
    //                 | Fable.IdentExpr i -> argId = i.Name
    //                 | _ -> false))
    //             let tempVars =
    //                 if found then
    //                     let tempVarName = getUniqueNameInDeclarationScope ctx (argId + "_tmp")
    //                     Map.add argId tempVarName tempVars
    //                 else tempVars
    //             checkCrossRefs tempVars allArgs rest
    //     ctx.OptimizeTailCall()
    //     let zippedArgs = List.zip tc.Args args
    //     let tempVars = checkCrossRefs Map.empty args zippedArgs
    //     let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExpr v)
    //     [|
    //         // First declare temp variables
    //         for (KeyValue(argId, tempVar)) in tempVars do
    //             yield varDeclaration (Pattern.identifier(tempVar)) false (Expression.identifier(argId)) |> Declaration.VariableDeclaration |> Declaration
    //         // Then assign argument expressions to the original argument identifiers
    //         // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
    //         for (argId, arg) in zippedArgs do
    //             let arg = FableTransforms.replaceValues tempVarReplacements arg
    //             let arg = com.TransformAsExpr(ctx, arg)
    //             yield assign None (Expression.identifier(argId)) arg |> ExpressionStatement
    //         yield Statement.continueStatement(Identifier.identifier(tc.Label), ?loc=range)
    //     |]

    // TODO: gen args
    let callFunction (_: SourceLocation option) funcExpr (args: Expression list) =
        InvocationExpression(funcExpr, [], args)

    let transformCallArgs (com: IDartCompiler) ctx (r: SourceLocation option) (info: ArgsInfo) =
        // TODO: Named params
        let paramObjInfo, hasSpread, args =
            match info with
            | CallInfo i ->
                let paramObjInfo = None // TODO
                paramObjInfo, i.HasSpread, i.Args
            | NoCallInfo args -> None, false, args

        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> []
        // | args when hasSpread ->
        //     match List.rev args with
        //     | [] -> []
        //     | (Replacements.Util.ArrayOrListLiteral(spreadArgs,_))::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
        //     | last::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr strategy expr: Statement =
        match strategy with
        | None | Some ReturnVoid -> ExpressionStatement expr
        | Some Return -> ReturnStatement expr
        | Some(Assign left) -> assign None left expr |> ExpressionStatement
        | Some(Target left) -> assign None (IdentExpression left) expr |> ExpressionStatement

    let transformType (com: IDartCompiler) ctx (t: Fable.Type) =
        match t with
        | Fable.Unit -> Void
        | Fable.Boolean -> Boolean
        | Fable.String -> String
        | Fable.Number(kind, _) ->
            match kind with
            | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64 -> Integer
            | Float32 | Float64 -> Double
            | Decimal | BigInt | NativeInt | UNativeInt -> Dynamic // TODO
        | _ -> Dynamic // TODO failwith $"todo: type %A{t}"

    let transformIdentWith (com: IDartCompiler) ctx typ name: Ident =
        { Name = name; Type = transformType com ctx typ }

    let transformIdent (com: IDartCompiler) ctx (id: Fable.Ident): Ident =
        transformIdentWith com ctx id.Type id.Name

    let transformIdentAsExpr (com: IDartCompiler) ctx (id: Fable.Ident) =
        transformIdentWith com ctx id.Type id.Name |> IdentExpression

    let transformVarDeclaration com ctx (memb: Fable.MemberDecl) =
        // TODO: Prefix non-public values with underscore or raise warning?
        let ident = transformIdentWith com ctx memb.Body.Type memb.Name
        // TODO: If value is primitive, list, union or record without mutable fields
        // we can declare it as const (if var is mutable we can make only the value const)
        let kind = if memb.Info.IsMutable then Var else Final
        let value = transformAsExpr com ctx memb.Body
        VariableDeclaration(ident, kind, value)

    let transformLocalVarDeclaration com ctx (fableIdent: Fable.Ident) value =
        let ident = transformIdent com ctx fableIdent
        // TODO: If value is primitive, list, union or record without mutable fields
        // we can declare it as const (if var is mutable we can make only the value const)
        let kind = if fableIdent.IsMutable then Var else Final
        let value = value |> Option.map (transformAsExpr com ctx)
        ident, kind, value

    let transformImport (com: IDartCompiler) ctx r (selector: string) (path: string) =
        let selector, parts =
            let parts = Array.toList(selector.Split('.'))
            parts.Head, parts.Tail
        com.GetImportExpr(ctx, selector, path, r)
        |> getParts parts

    let transformValue (com: IDartCompiler) (_: Context) (r: SourceLocation option) value: Expression =
        match value with
        | Fable.BoolConstant v -> BooleanLiteral v |> Literal
        | Fable.StringConstant v -> StringLiteral v |> Literal
        | Fable.NumberConstant(x, kind, _) ->
            match kind, x with
            | Int8, (:? int8 as x) -> IntegerLiteral(int64 x) |> Literal
            | UInt8, (:? uint8 as x) -> IntegerLiteral(int64 x) |> Literal
            | Int16, (:? int16 as x) -> IntegerLiteral(int64 x) |> Literal
            | UInt16, (:? uint16 as x) -> IntegerLiteral(int64 x) |> Literal
            | Int32, (:? int32 as x) -> IntegerLiteral(int64 x) |> Literal
            | UInt32, (:? uint32 as x) -> IntegerLiteral(int64 x) |> Literal
            | Int64, (:? int64 as x) -> IntegerLiteral(x) |> Literal
            | UInt64, (:? uint64 as x) -> IntegerLiteral(int64 x) |> Literal
            | Float32, (:? float32 as x) -> DoubleLiteral(float x) |> Literal
            | Float64, (:? float as x) -> DoubleLiteral(x) |> Literal
            | _ ->
                $"Expected literal of type %A{kind} but got {x.GetType().FullName}"
                |> addErrorAndReturnNull com r
        | v -> failwith $"TODO: value %A{v}"

    let transformOperation com ctx (_: SourceLocation option) t opKind: Expression =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            UnaryExpression(op, expr)
        | Fable.Binary(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            BinaryExpression(op, left, right, isInt64OrLess t)
        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            LogicalExpression(op, left, right)

    let transformCall (com: IDartCompiler) ctx range callee (callInfo: Fable.CallInfo) =
        // Try to optimize some patterns after FableTransforms
        let optimized =
            match callInfo.OptimizableInto, callInfo.Args with
            | Some "array" , [Replacements.Util.ArrayOrListLiteral(vals,_)] -> Fable.Value(Fable.NewArray(vals, Fable.Any), range) |> Some
            | _ -> None

        match optimized with
        | Some e -> com.TransformAsExpr(ctx, e)
        | None ->
            let callee = com.TransformAsExpr(ctx, callee)
            let args = transformCallArgs com ctx range (CallInfo callInfo)
            match callInfo.ThisArg with
            | Some(TransformExpr com ctx thisArg) -> callFunction range callee (thisArg::args)
            | None -> callFunction range callee args

    let transformCallAsStatements com ctx range (_: Fable.Type) returnStrategy callee callInfo =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args + (if Option.isSome i.ThisArg then 1 else 0)
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with // TODO
        // | Some(Return|ReturnVoid), Some tc when tc.IsRecursiveRef(callee)
        //                                     && argsLen callInfo = List.length tc.Args ->
        //     let args =
        //         match callInfo.ThisArg with
        //         | Some thisArg -> thisArg::callInfo.Args
        //         | None -> callInfo.Args
        //     optimizeTailCall com ctx range tc args
        | _ ->
            [transformCall com ctx range callee callInfo |> resolveExpr returnStrategy]

    let transformGet (com: IDartCompiler) ctx range typ fableExpr kind =
        match kind with
        | Fable.FieldGet(fieldName,_) ->
            let fableExpr =
                match fableExpr with
                // If we're accessing a virtual member with default implementation (see #701)
                // from base class, we can use `super` in JS so we don't need the bound this arg
                | Fable.Value(Fable.BaseValue(_,t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr
            let expr = com.TransformAsExpr(ctx, fableExpr)
            get range expr fieldName

        | e -> failwith $"todo: get ${e}"

    // TODO: tail calls, hoist vars
    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr): Ident list * Statement list =
        let args = discardUnitArg args |> List.map (transformIdent com ctx)
        let ret = if body.Type = Fable.Unit then ReturnVoid else Return
        let body = transformAsStatements com ctx (Some ret) body
        args, body

    // let transformBindingAsExpr (com: IDartCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
    //     transformBindingExprBody com ctx var value
    //     |> assign None (transformIdentAsExpr com ctx var)

    let transformBindingAsStatements (com: IDartCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isStatement ctx false value then
            let var, kind, _ = transformLocalVarDeclaration com ctx var None
            let varExpr = IdentExpression var
            [
                LocalVariableDeclaration(var, kind, None)
                yield! com.TransformAsStatements(ctx, Some(Assign varExpr), value)
            ]
        else
            // TODO: Check if we need a function declaration
            // (See transformBindingExprBody in Fable2Babel)
            let ident, kind, body = transformLocalVarDeclaration com ctx var (Some value)
            [LocalVariableDeclaration(ident, kind, body)]

    let rec transformAsExpr (com: IDartCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.Operation(kind, t, r) -> transformOperation com ctx r t kind

        | Fable.IdentExpr ident -> transformIdent com ctx ident |> IdentExpression

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.Lambda(arg, body, name) ->
            transformFunction com ctx name [arg] body
            |> makeAnonymousFunction

        // | Fable.Let(ident, value, body) ->
        //     if ctx.HoistVars [ident] then
        //         let assignment = transformBindingAsExpr com ctx ident value
        //         Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        // | Fable.LetRec(bindings, body) ->
        //     if ctx.HoistVars(List.map fst bindings) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        | e -> failwith $"todo: transform expr %A{e}"

    let rec transformAsStatements (com: IDartCompiler) ctx returnStrategy (expr: Fable.Expr): Statement list =
        match expr with
        | Fable.Value(kind, r) ->
            [transformValue com ctx r kind |> resolveExpr returnStrategy]

        | Fable.Operation(kind, t, r) ->
            [transformOperation com ctx r t kind |> resolveExpr returnStrategy]

        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements com ctx range typ returnStrategy callee info

        | Fable.Import({ Selector = selector; Path = path }, _t, r) ->
            [transformImport com ctx r selector path |> resolveExpr returnStrategy]

        | Fable.Get(expr, kind, t, range) ->
            [transformGet com ctx range t expr kind |> resolveExpr returnStrategy]

        | Fable.Lambda(arg, body, name) ->
            transformFunction com ctx name [arg] body
            |> makeAnonymousFunction
            |> resolveExpr returnStrategy
            |> List.singleton

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapi (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                transformAsStatements com ctx ret statement)
            |> List.concat

        | Fable.Let(ident, value, body) ->
            let binding = transformBindingAsStatements com ctx ident value
            List.append binding (transformAsStatements com ctx returnStrategy body)

        | Fable.LetRec(bindings, body) ->
            let bindings = bindings |> List.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            List.append bindings (transformAsStatements com ctx returnStrategy body)

        | e -> failwith $"todo: transformAsStatements %A{e}"

    let getMemberArgsAndBody (com: IDartCompiler) ctx kind (args: Fable.Ident list) (body: Fable.Expr) =
        let funcName, args, body =
            match kind, args with
            | Attached(isStatic=false), (thisArg::args) ->
                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if FableTransforms.isIdentUsed thisArg.Name body then
                        let thisKeyword = Fable.IdentExpr { thisArg with Name = "this" }
                        Fable.Let(thisArg, thisKeyword, body)
                    else body
                None, args, body
            | Attached(isStatic=true), _
            | ClassConstructor, _ -> None, args, body
            | NonAttached funcName, _ -> Some funcName, args, body
            | _ -> None, args, body

        transformFunction com ctx funcName args body

    let transformModuleFunction (com: IDartCompiler) ctx (memb: Fable.MemberDecl) =
        let returnType = transformType com ctx memb.Body.Type
        let args, body = getMemberArgsAndBody com ctx (NonAttached memb.Name) memb.Args memb.Body
        let isEntryPoint =
            memb.Info.Attributes
            |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)
        if isEntryPoint then
            failwith "todo: main function"
        else
            let genParams = [] // TODO
            FunctionDeclaration(memb.Name, args, body, genParams, returnType)

    let rec transformDeclaration (com: IDartCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.MemberDeclaration memb ->
            withCurrentScope ctx memb.UsedNames <| fun ctx ->
                if memb.Info.IsValue then
                    // TODO: Prefix non-public values with underscore or raise warning?
                    let ident = transformIdentWith com ctx memb.Body.Type memb.Name
                    // TODO: If value is primitive, list, union or record without mutable fields
                    // we can declare it as const (if var is mutable we can make only the value const)
                    let kind = if memb.Info.IsMutable then Var else Final
                    let value = transformAsExpr com ctx memb.Body
                    [VariableDeclaration(ident, kind, value)]
                else
                    [transformModuleFunction com ctx memb]

        // TODO: Action declarations are not supported in Dart, compile as: var _ = ...
        | Fable.ActionDeclaration _
//            withCurrentScope ctx decl.UsedNames <| fun ctx ->
//                transformAction com ctx decl.Body

        | Fable.ClassDeclaration _ -> [] // TODO

    let getIdentForImport (ctx: Context) (path: string) =
        Path.GetFileNameWithoutExtension(path).Replace(".", "_")
        |> Naming.applyCaseRule Core.CaseRules.SnakeCase
        |> getUniqueNameInRootScope ctx

module Compiler =
    open Util

    type DartCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IDartCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, selector, path, r) =
                let localId =
                    match imports.TryGetValue(path) with
                    | true, i ->
                        match i.LocalIdent with
                        | Some localId -> localId
                        | None ->
                            let localId = getIdentForImport ctx path
                            imports[path] <- { Path = path; LocalIdent = Some localId }
                            localId
                    | false, _ ->
                        let localId = getIdentForImport ctx path
                        imports.Add(path, { Path = path; LocalIdent = Some localId })
                        localId
                let expr = IdentExpression { Name = localId; Type = Object }
                match selector with
                | Naming.placeholder ->
                    "`importMember` must be assigned to a variable"
                    |> addErrorAndReturnNull com r
                | "*" -> expr
                | selector -> PropertyAccess(expr, selector)

            member _.GetAllImports() = imports.Values |> Seq.toList
            member this.TransformAsExpr(ctx, e) = transformAsExpr this ctx e
            member this.TransformAsStatements(ctx, ret, e) = transformAsStatements this ctx ret e
            member this.TransformFunction(ctx, name, args, body) = transformFunction this ctx name args body
            member this.TransformImport(ctx, selector, path) = transformImport this ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
            member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = DartCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IDartCompiler
        let declScopes =
            let hs = HashSet()
            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)
            hs

        let ctx =
          { File = file
            UsedNames = { RootScope = HashSet file.UsedNamesInRootScope
                          DeclarationScopes = declScopes
                          CurrentDeclarationScope = Unchecked.defaultof<_> }
            DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> () }
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let imports = com.GetAllImports()
        { File.Imports = imports
          Declarations = rootDecls }
