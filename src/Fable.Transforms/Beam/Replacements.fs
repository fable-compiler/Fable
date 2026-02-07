[<RequireQualifiedAccess>]
module Fable.Transforms.Beam.Replacements

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

// Erlang's =:= does deep structural comparison for all types
// (atoms, numbers, binaries, tuples, maps, lists), so we can
// use BinaryEqual/BinaryUnequal directly instead of library calls.
let private equals r equal (left: Expr) (right: Expr) =
    if equal then
        makeBinOp r Boolean left right BinaryEqual
    else
        makeBinOp r Boolean left right BinaryUnequal

let private operators
    (_com: ICompiler)
    (_ctx: Context)
    r
    (_t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | (Operators.equality | "Eq"), [ left; right ] -> equals r true left right |> Some
    | (Operators.inequality | "Neq"), [ left; right ] -> equals r false left right |> Some
    | Operators.unaryNegation, [ operand ] -> Operation(Unary(UnaryMinus, operand), Tags.empty, _t, r) |> Some
    // Erlang has native arbitrary-precision integers, so Int64/UInt64/BigInt
    // use direct binary ops instead of library calls (like Python's int)
    | Patterns.SetContains Operators.standardSet, _ ->
        let argTypes = args |> List.map (fun a -> a.Type)

        match argTypes with
        | Number((Int64 | UInt64 | Int128 | UInt128 | NativeInt | UNativeInt | BigInt), _) :: _ ->
            match info.CompiledName, args with
            | Operators.addition, [ left; right ] -> makeBinOp r _t left right BinaryPlus |> Some
            | Operators.subtraction, [ left; right ] -> makeBinOp r _t left right BinaryMinus |> Some
            | Operators.multiply, [ left; right ] -> makeBinOp r _t left right BinaryMultiply |> Some
            | (Operators.division | Operators.divideByInt), [ left; right ] ->
                makeBinOp r _t left right BinaryDivide |> Some
            | Operators.modulus, [ left; right ] -> makeBinOp r _t left right BinaryModulus |> Some
            | _ -> None
        | _ -> None
    | _ -> None

let private languagePrimitives
    (_com: ICompiler)
    (_ctx: Context)
    r
    (_t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [ left; right ] -> equals r true left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [ left; right ] -> equals r true left right |> Some
    | _ -> None

let tryField (_com: ICompiler) _returnTyp _ownerTyp _fieldName : Expr option = None

let tryType (_t: Type) : Expr option = None

let tryCall
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    : Expr option
    =
    match info.DeclaringEntityFullName with
    | "Microsoft.FSharp.Core.Operators"
    | "Microsoft.FSharp.Core.Operators.Checked"
    | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives"
    | "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare" -> languagePrimitives com ctx r t info thisArg args
    | _ -> None

let tryBaseConstructor
    (_com: ICompiler)
    (_ctx: Context)
    (_ent: EntityRef)
    (_argTypes: Lazy<Type list>)
    (_genArgs: Type list)
    (_args: Expr list)
    : Expr option
    =
    None
