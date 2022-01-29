module Fable.Transforms.Replacements.Api

#nowarn "1182"

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

type ICompiler = FSharp2Fable.IFableCompiler

let curryExprAtRuntime com arity (expr: Expr) =
    Helper.LibCall(com, "Util", "curry", expr.Type, [makeIntConst arity; expr])

let uncurryExprAtRuntime com arity (expr: Expr) =
    Helper.LibCall(com, "Util", "uncurry", expr.Type, [makeIntConst arity; expr])

let partialApplyAtRuntime com t arity (fn: Expr) (args: Expr list) =
    let args = NewArray(args, Any) |> makeValue None
    Helper.LibCall(com, "Util", "partialApply", t, [makeIntConst arity; fn; args])

let checkArity com t arity expr =
    Helper.LibCall(com, "Util", "checkArity", t, [makeIntConst arity; expr])

let tryField (com: ICompiler) returnTyp ownerTyp fieldName =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryField com returnTyp ownerTyp fieldName
    | Python -> PY.Replacements.tryField com returnTyp ownerTyp fieldName
    | _ -> JS.Replacements.tryField com returnTyp ownerTyp fieldName

let tryBaseConstructor (com: ICompiler) ctx (ent: Entity) (argTypes: Lazy<Type list>) genArgs args =
    match com.Options.Language with
    | Python -> PY.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args
    | _ -> JS.Replacements.tryBaseConstructor com ctx ent argTypes genArgs args

let makeTypeConst (com: ICompiler) r (typ: Type) (value: obj) =
    match com.Options.Language with
    | Rust -> Rust.Replacements.makeTypeConst com r typ value
    | Python -> PY.Replacements.makeTypeConst com r typ value
    | Dart -> Dart.Replacements.makeTypeConst com r typ value
    | _ -> JS.Replacements.makeTypeConst com r typ value

let makeMethodInfo (com: ICompiler) r (name: string) (parameters: (string * Type) list) (returnType: Type) =
    match com.Options.Language with
    | _ -> JS.Replacements.makeMethodInfo com r name parameters returnType

let tryType (com: ICompiler) (t: Type) =
    match com.Options.Language with
    | Rust -> Rust.Replacements.tryType t
    | Python -> PY.Replacements.tryType t
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
    | _ -> JS.Replacements.error msg

let toSeq (_com: ICompiler) t (e: Expr) =
    JS.Replacements.toSeq t e

let defaultof (com: ICompiler) ctx typ =
    match com.Options.Language with
    | Rust -> Rust.Replacements.getZero com ctx typ
    | Python -> PY.Replacements.defaultof com ctx typ
    | _ -> JS.Replacements.defaultof com ctx typ

let createAtom (com: ICompiler) value =
    match com.Options.Language with
    | Rust -> Rust.Replacements.createAtom com value
    | Python -> PY.Replacements.createAtom com value
    | _ -> JS.Replacements.createAtom com value

let getReference (com: ICompiler) r typ var =
    match com.Options.Language with
    | Python -> PY.Replacements.getReference r typ var
    | Rust -> Rust.Replacements.getReference r typ var
    | _ -> JS.Replacements.getReference r typ var

let setReference (com: ICompiler) r expr value =
    match com.Options.Language with
    | Python -> PY.Replacements.setReference r expr value
    | Rust -> Rust.Replacements.setReference r expr value
    | _ -> JS.Replacements.setReference r expr value

let newReference (com: ICompiler) r typ value =
    match com.Options.Language with
    | Python -> PY.Replacements.newReference com r typ value
    | Rust -> Rust.Replacements.newReference com r typ value
    | _ -> JS.Replacements.newReference com r typ value

let makeRefFromMutableFunc (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefFromMutableFunc com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableFunc com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableFunc com ctx r t value

let makeRefFromMutableValue (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefFromMutableValue com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableValue com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableValue com ctx r t value

let makeRefFromMutableField (com: ICompiler) ctx r t (value: Expr) =
    match com.Options.Language with
    | Python -> PY.Replacements.makeRefFromMutableField com ctx r t value
    | Rust -> Rust.Replacements.makeRefFromMutableField com ctx r t value
    | _ -> JS.Replacements.makeRefFromMutableField com ctx r t value
