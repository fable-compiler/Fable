module Fable.Transforms.Replacements.Api

#nowarn "1182"

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

type ICompiler = FSharp2Fable.IFableCompiler

let curryExprAtRuntime (com: Compiler) arity (expr: Expr) =
    match com.Options.Language with
    | Dart -> Dart.Replacements.curryExprAtRuntime com arity expr
    | Rust -> Rust.Replacements.curryExprAtRuntime com arity expr
    | _ ->
        Helper.LibCall(com, "Util", "curry", expr.Type, [makeIntConst arity; expr])

let uncurryExprAtRuntime (com: Compiler) t arity (expr: Expr) =
    match com.Options.Language with
    | Dart -> Dart.Replacements.uncurryExprAtRuntime com t arity expr
    | Rust -> Rust.Replacements.uncurryExprAtRuntime com t arity expr
    | _ ->
        Helper.LibCall(com, "Util", "uncurry", expr.Type, [makeIntConst arity; expr])

let partialApplyAtRuntime (com: Compiler) t arity (fn: Expr) (args: Expr list) =
    match com.Options.Language with
    | Dart -> Dart.Replacements.partialApplyAtRuntime com t arity fn args
    | Rust -> Rust.Replacements.partialApplyAtRuntime com t arity fn args
    | _ ->
        let args = NewArray(ArrayValues args, Any, MutableArray) |> makeValue None
        Helper.LibCall(com, "Util", "partialApply", t, [makeIntConst arity; fn; args])

let checkArity (com: Compiler) t arity expr =
    match com.Options.Language with
    | Rust -> Rust.Replacements.checkArity com t arity expr
    | _ -> Helper.LibCall(com, "Util", "checkArity", t, [makeIntConst arity; expr])

let tryField (com: ICompiler) returnTyp ownerTyp fieldName =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryField com returnTyp ownerTyp fieldName
    | Python -> PY.Replacements.tryField com returnTyp ownerTyp fieldName
    | Dart -> Dart.Replacements.tryField com returnTyp ownerTyp fieldName
    | _ -> JS.Replacements.tryField com returnTyp ownerTyp fieldName

let tryBaseConstructor (com: ICompiler) ctx (ent: EntityRef) (argTypes: Lazy<Type list>) genArgs args =
    match com.Options.Language with
    | Python -> PY.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args
    | Dart -> Dart.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args
    | _ -> JS.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args

let makeMethodInfo (com: ICompiler) r (name: string) (parameters: (string * Type) list) (returnType: Type) =
    match com.Options.Language with
    | _ -> JS.Replacements.makeMethodInfo com r name parameters returnType

let tryType (com: ICompiler) (t: Type) =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryType t
    | Python -> PY.Replacements.tryType t
    | Dart -> Dart.Replacements.tryType t
    | _ -> JS.Replacements.tryType t

let tryCall (com: ICompiler) ctx r t info thisArg args =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryCall com ctx r t info thisArg args
    | Python -> PY.Replacements.tryCall com ctx r t info thisArg args
    | Dart -> Dart.Replacements.tryCall com ctx r t info thisArg args
    | _ -> JS.Replacements.tryCall com ctx r t info thisArg args

let error (com: ICompiler) msg =
    match com.Options.Language with
    | Python -> PY.Replacements.error msg
    | Rust -> Rust.Replacements.error msg
    | Dart -> Dart.Replacements.error msg
    | _ -> JS.Replacements.error msg

let defaultof (com: ICompiler) ctx typ =
    match com.Options.Language with
    | Rust -> Rust.Replacements.getZero com ctx typ
    | Python -> PY.Replacements.defaultof com ctx typ
    | Dart -> Dart.Replacements.getZero com ctx typ
    | _ -> JS.Replacements.defaultof com ctx typ

let createMutablePublicValue (com: ICompiler) value =
    match com.Options.Language with
    | Python -> PY.Replacements.createAtom com value
    | JavaScript | TypeScript -> JS.Replacements.createAtom com value
    | Rust | Php | Dart -> value

let getRefCell (com: ICompiler) r typ (expr: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.getRefCell com r typ expr
    | Rust -> Rust.Replacements.getRefCell com r typ expr
    | Dart -> Dart.Replacements.getRefCell com r typ expr
    | _ -> JS.Replacements.getRefCell com r typ expr

let setRefCell (com: ICompiler) r (expr: Expr) (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.setRefCell com r expr value
    | Rust -> Rust.Replacements.setRefCell com r expr value
    | Dart -> Dart.Replacements.setRefCell com r expr value
    | _ -> JS.Replacements.setRefCell com r expr value

let makeRefCell (com: ICompiler) r typ (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefCell com r typ value
    | Rust -> Rust.Replacements.makeRefCell com r typ value
    | Dart -> Dart.Replacements.makeRefCell com r typ value
    | _ -> JS.Replacements.makeRefCell com r typ value

let makeRefFromMutableFunc (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefFromMutableFunc com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableFunc com ctx r t value
    | Dart -> Dart.Replacements.makeRefFromMutableFunc com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableFunc com ctx r t value

let makeRefFromMutableValue (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefFromMutableValue com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableValue com ctx r t value
    | Dart -> Dart.Replacements.makeRefFromMutableValue com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableValue com ctx r t value

let makeRefFromMutableField (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefFromMutableField com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableField com ctx r t value
    | Dart -> Dart.Replacements.makeRefFromMutableField com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableField com ctx r t value
