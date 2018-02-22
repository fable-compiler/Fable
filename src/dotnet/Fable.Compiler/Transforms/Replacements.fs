module Fable.Transforms.Replacements

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.AST.Fable.Util

let ccall r t info coreModule coreMember args =
    let callee = Import(coreMember, coreModule, CoreLib, Any)
    Operation(Call(callee, None, args, info), t, r)

// TODO: Check special cases: custom operators, bigint, dates, etc
let applyOp com r t opName args =
    let unOp operator operand =
        Operation(UnaryOperation(operator, operand), t, r)
    let binOp op left right =
        Operation(BinaryOperation(op, left, right), t, r)
    let logicOp op left right =
        Operation(LogicalOperation(op, left, right), Boolean, r)
    match opName, args with
    | "op_Addition", [left; right] -> binOp BinaryPlus left right
    | "op_Subtraction", [left; right] -> binOp BinaryMinus left right
    | "op_Multiply", [left; right] -> binOp BinaryMultiply left right
    | "op_Division", [left; right] -> binOp BinaryDivide left right
    | "op_Modulus", [left; right] -> binOp BinaryModulus left right
    | "op_LeftShift", [left; right] -> binOp BinaryShiftLeft left right
    | "op_RightShift", [left; right] ->
        match left.Type with
        | Number UInt32 -> binOp BinaryShiftRightZeroFill left right // See #646
        | _ -> binOp BinaryShiftRightSignPropagating left right
    | "op_BitwiseAnd", [left; right] -> binOp BinaryAndBitwise left right
    | "op_BitwiseOr", [left; right] -> binOp BinaryOrBitwise left right
    | "op_ExclusiveOr", [left; right] -> binOp BinaryXorBitwise left right
    | "op_BooleanAnd", [left; right] -> logicOp LogicalAnd left right
    | "op_BooleanOr", [left; right] -> logicOp LogicalOr left right
    | "op_LogicalNot", [operand] -> unOp UnaryNotBitwise operand
    | "op_UnaryNegation", [operand] -> unOp UnaryMinus operand
    | _ -> "Unknown operator: " + opName |> addErrorAndReturnNull com r

let operators com r t callee args info (extraInfo: ExtraCallInfo) =
    match extraInfo.CompiledName with
    | "op_Addition" | "op_Subtraction" | "op_Multiply" | "op_Division"
    | "op_Modulus" | "op_LeftShift" | "op_RightShift"
    | "op_BitwiseAnd" | "op_BitwiseOr" | "op_ExclusiveOr"
    | "op_LogicalNot" | "op_UnaryNegation" | "op_BooleanAnd" | "op_BooleanOr" ->
        applyOp com r t extraInfo.CompiledName args |> Some
    | _ -> None


let fableCoreLib com r t callee args info (extraInfo: ExtraCallInfo) =
    match extraInfo.CompiledName with
    | "AreEqual" -> ccall r t info "Assert" "equal" args |> Some
    | _ -> None

let isReplaceCandidate (fullName: string) =
    fullName.StartsWith("System.")
        || fullName.StartsWith("Microsoft.FSharp.")
        || fullName.StartsWith("Fable.Core.")

/// Resolve pipes and composition in a first pass
let tryFirstPass r t (args: Expr list) (fullName: string) =
    let rec curriedApply r t applied args =
        Operation(CurriedApply(applied, args), t, r)
    let compose f1 f2 =
        None // TODO
        // let tempVar = com.GetUniqueVar() |> makeIdent
        // [Fable.IdentExpr tempVar]
        // |> makeCall com info.range Fable.Any f1
        // |> List.singleton
        // |> makeCall com info.range Fable.Any f2
        // |> makeLambda [tempVar]
    if fullName.StartsWith("Microsoft.FSharp.Core.Operators") then
        match fullName.Substring(fullName.LastIndexOf(".") + 1), args with
        | "( |> )", [x; f]
        | "( <| )", [f; x] -> curriedApply r t f [x] |> Some
        // TODO: Try to untuple double and triple pipe arguments
        // | "( ||> )", [x; y; f]
        // | "( <|| )", [f; x; y] -> curriedApply r t f [x; y] |> Some
        // | "( |||> )", [x; y; z; f]
        // | "( <||| )", [f; x; y; z] -> curriedApply r t f [x; y; z] |> Some
        | "( >> )", [f1; f2] -> compose f1 f2
        | "( << )", [f2; f1] -> compose f1 f2
        | _ -> None
    else None

let trySecondPass com r t (callee: Expr option) (args: Expr list)
                    (info: CallInfo) (extraInfo: ExtraCallInfo) =
    let ownerName = extraInfo.FullName.Substring(0, extraInfo.FullName.LastIndexOf('.'))
    match ownerName with
    | "System.Math"
    | "Microsoft.FSharp.Core.Operators"
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
    | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com r t callee args info extraInfo
    | Naming.StartsWith "Fable.Core." _ -> fableCoreLib com r t callee args info extraInfo
    | _ -> None
//         | Naming.EndsWith "Exception" _ -> exceptions com info
//         | "System.Object" -> objects com info
//         | "System.Timers.ElapsedEventArgs" -> info.callee // only signalTime is available here
//         | "System.Char" -> chars com info
//         | "System.Enum" -> enums com info
//         | "System.String"
//         | "Microsoft.FSharp.Core.StringModule" -> strings com info
//         | "Microsoft.FSharp.Core.PrintfModule"
//         | "Microsoft.FSharp.Core.PrintfFormat" -> fsFormat com info
//         | "System.BitConverter" -> bitConvert com info
//         | "System.Int32" -> parse com info false
//         | "System.Single"
//         | "System.Double" -> parse com info true
//         | "System.Convert" -> convert com info
//         | "System.Console" -> console com info
//         | "System.Decimal" -> decimals com info
//         | "System.Diagnostics.Debug"
//         | "System.Diagnostics.Debugger" -> debug com info
//         | "System.DateTime"
//         | "System.DateTimeOffset" -> dates com info
//         | "System.TimeSpan" -> timeSpans com info
//         | "System.Environment" -> systemEnv com info
//         | "System.Globalization.CultureInfo" -> globalization com info
//         | "System.Action" | "System.Func"
//         | "Microsoft.FSharp.Core.FSharpFunc"
//         | "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" -> funcs com info
//         | "System.Random" -> random com info
//         | "Microsoft.FSharp.Core.FSharpOption"
//         | "Microsoft.FSharp.Core.OptionModule" -> options com info
//         | "System.Threading.CancellationToken"
//         | "System.Threading.CancellationTokenSource" -> cancels com info
//         | "Microsoft.FSharp.Core.FSharpRef" -> references com info
//         | "System.Activator" -> activator com info
//         | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings com info
//         | "Microsoft.FSharp.Core.LanguagePrimitives" -> languagePrimitives com info
//         | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
//         | "Microsoft.FSharp.Core.Operators.OperatorIntrinsics" -> intrinsicFunctions com info
//         | "System.Text.RegularExpressions.Capture"
//         | "System.Text.RegularExpressions.Match"
//         | "System.Text.RegularExpressions.Group"
//         | "System.Text.RegularExpressions.MatchCollection"
//         | "System.Text.RegularExpressions.GroupCollection"
//         | "System.Text.RegularExpressions.Regex" -> regex com info
//         | "System.Collections.Generic.IEnumerable"
//         | "System.Collections.IEnumerable" -> enumerable com info
//         | "System.Collections.Generic.Dictionary"
//         | "System.Collections.Generic.IDictionary" -> dictionaries com info
//         | "System.Collections.Generic.HashSet"
//         | "System.Collections.Generic.ISet" -> hashSets com info
//         | "System.Collections.Generic.KeyValuePair" -> keyValuePairs com info
//         | "System.Collections.Generic.Dictionary.KeyCollection"
//         | "System.Collections.Generic.Dictionary.ValueCollection"
//         | "System.Collections.Generic.ICollection" -> collectionsSecondPass com info Seq
//         | "System.Array"
//         | "System.Collections.Generic.List"
//         | "System.Collections.Generic.IList" -> collectionsSecondPass com info Array
//         | "Microsoft.FSharp.Collections.ArrayModule" -> collectionsFirstPass com info Array
//         | "Microsoft.FSharp.Collections.FSharpList"
//         | "Microsoft.FSharp.Collections.ListModule" -> collectionsFirstPass com info List
//         | "Microsoft.FSharp.Collections.SeqModule" -> collectionsSecondPass com info Seq
//         | "Microsoft.FSharp.Collections.FSharpMap"
//         | "Microsoft.FSharp.Collections.MapModule"
//         | "Microsoft.FSharp.Collections.FSharpSet"
//         | "Microsoft.FSharp.Collections.SetModule" -> mapAndSets com info
//         | "System.Type" -> types com info
//         | "Microsoft.FSharp.Core.Operators.Unchecked" -> unchecked com info
//         | "Microsoft.FSharp.Control.FSharpMailboxProcessor"
//         | "Microsoft.FSharp.Control.FSharpAsyncReplyChannel" -> mailbox com info
//         | "Microsoft.FSharp.Control.FSharpAsync" -> asyncs com info
//         | "System.Guid" -> guids com info
//         | "System.Uri" -> uris com info
//         | "System.Lazy" | "Microsoft.FSharp.Control.Lazy"
//         | "Microsoft.FSharp.Control.LazyExtensions" -> laziness com info
//         | "Microsoft.FSharp.Control.CommonExtensions" -> controlExtensions com info
//         | "System.Numerics.BigInteger"
//         | "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI" -> bigint com info
//         | "Microsoft.FSharp.Reflection.FSharpType" -> fsharpType com info info.memberName
//         | "Microsoft.FSharp.Reflection.FSharpValue" -> fsharpValue com info info.memberName
//         | "Microsoft.FSharp.Reflection.FSharpReflectionExtensions" ->
//             // In netcore F# Reflection methods become extensions
//             // with names like `FSharpType.GetExceptionFields.Static`
//             let isFSharpType = info.memberName.StartsWith("fSharpType")
//             let methName = info.memberName |> Naming.extensionMethodName |> Naming.lowerFirst
//             if isFSharpType
//             then fsharpType com info methName
//             else fsharpValue com info methName
//         | "Microsoft.FSharp.Reflection.UnionCaseInfo"
//         | "System.Reflection.PropertyInfo"
//         | "System.Reflection.MemberInfo" ->
//             match info.callee, info.memberName with
//             | _, "getFields" -> icall info "getUnionFields" |> Some
//             | Some c, "name" -> ccall info "Reflection" "getName" [c] |> Some
//             | Some c, ("tag" | "propertyType") ->
//                 let prop =
//                     if info.memberName = "tag" then "index" else info.memberName
//                     |> Fable.StringConst |> Fable.Value
//                 makeGet info.range info.returnType c prop |> Some
//             | _ -> None
//         | _ -> None
