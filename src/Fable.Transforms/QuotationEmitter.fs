module Fable.Transforms.QuotationEmitter

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

/// Emits a Fable expression that, when compiled, produces runtime calls
/// to construct a quotation AST. The input is the Fable.Expr captured
/// inside a Quote node; the output is a Fable.Expr that calls the
/// quotation runtime library to build the AST at runtime.
let rec emitQuotedExpr (com: Compiler) (expr: Expr) : Expr =
    match expr with
    | Value(kind, r) -> emitQuotedValue com kind r

    | IdentExpr ident ->
        // Reference to a variable already introduced by a lambda/let in the quotation.
        // Emit: quotation.mkVar(Var)
        // We need a var reference. Create a var and then wrap it.
        let varExpr =
            Helper.LibCall(
                com,
                "quotation",
                "mkQuotVar",
                Any,
                [
                    makeStrConst ident.Name
                    makeStrConst (typeToString ident.Type)
                    makeBoolConst false
                ]
            )

        Helper.LibCall(com, "quotation", "mkVar", Any, [ varExpr ])

    | Lambda(arg, body, _name) ->
        let varExpr =
            Helper.LibCall(
                com,
                "quotation",
                "mkQuotVar",
                Any,
                [
                    makeStrConst arg.Name
                    makeStrConst (typeToString arg.Type)
                    makeBoolConst false
                ]
            )

        let bodyExpr = emitQuotedExpr com body
        Helper.LibCall(com, "quotation", "mkLambda", Any, [ varExpr; bodyExpr ])

    | Delegate(args, body, _name, _tags) ->
        // Multi-arg delegate: nest as curried lambdas
        let rec nestLambdas args body =
            match args with
            | [] -> emitQuotedExpr com body
            | (arg: Ident) :: rest ->
                let varExpr =
                    Helper.LibCall(
                        com,
                        "quotation",
                        "mkQuotVar",
                        Any,
                        [
                            makeStrConst arg.Name
                            makeStrConst (typeToString arg.Type)
                            makeBoolConst false
                        ]
                    )

                let innerBody = nestLambdas rest body
                Helper.LibCall(com, "quotation", "mkLambda", Any, [ varExpr; innerBody ])

        nestLambdas args body

    | Let(ident, value, body) ->
        let varExpr =
            Helper.LibCall(
                com,
                "quotation",
                "mkQuotVar",
                Any,
                [
                    makeStrConst ident.Name
                    makeStrConst (typeToString ident.Type)
                    makeBoolConst ident.IsMutable
                ]
            )

        let valueExpr = emitQuotedExpr com value
        let bodyExpr = emitQuotedExpr com body

        Helper.LibCall(com, "quotation", "mkLet", Any, [ varExpr; valueExpr; bodyExpr ])

    | IfThenElse(guardExpr, thenExpr, elseExpr, _r) ->
        let guard = emitQuotedExpr com guardExpr
        let thenE = emitQuotedExpr com thenExpr
        let elseE = emitQuotedExpr com elseExpr
        Helper.LibCall(com, "quotation", "mkIfThenElse", Any, [ guard; thenE; elseE ])

    | CurriedApply(applied, args, _typ, _r) ->
        // Emit nested applications: Application(Application(f, a1), a2)
        let appliedExpr = emitQuotedExpr com applied

        args
        |> List.fold
            (fun acc arg ->
                let argExpr = emitQuotedExpr com arg
                Helper.LibCall(com, "quotation", "mkApplication", Any, [ acc; argExpr ])
            )
            appliedExpr

    | Call(callee, info, _typ, _r) ->
        let instanceExpr =
            match info.ThisArg with
            | Some thisArg -> emitQuotedExpr com thisArg
            // A static/operator call has no instance. Emit a runtime-built null node
            // (not a raw null literal) so every target — notably statically typed ones
            // like Rust — sees an Expr in the instance position.
            | None -> mkNullExpr com "null"

        let declaringType, methodName =
            match info.MemberRef with
            | Some(MemberRef(declaringEntity, mInfo)) -> declaringEntity.FullName, mInfo.CompiledName
            | _ -> "", "unknown"

        let methodExpr = makeStrConst methodName
        let declTypeExpr = makeStrConst declaringType

        let argExprs = info.Args |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "quotation", "mkCall", Any, [ instanceExpr; methodExpr; argExprs; declTypeExpr ])

    | Sequential exprs ->
        match exprs with
        | [] -> emitQuotedExpr com (Value(UnitConstant, None))
        | [ single ] -> emitQuotedExpr com single
        | first :: rest ->
            let restExpr = emitQuotedExpr com (Sequential rest)
            let firstExpr = emitQuotedExpr com first
            Helper.LibCall(com, "quotation", "mkSequential", Any, [ firstExpr; restExpr ])

    | Operation(kind, _tags, _typ, _r) ->
        // Represent operations as calls to the operator method
        let opName, args =
            match kind with
            | Unary(op, operand) ->
                let name =
                    match op with
                    | UnaryMinus -> "op_UnaryNegation"
                    | UnaryPlus -> "op_UnaryPlus"
                    | UnaryNot -> "op_LogicalNot"
                    | UnaryNotBitwise -> "op_OnesComplement"
                    | UnaryAddressOf -> "op_AddressOf"

                name, [ operand ]
            | Binary(op, left, right) ->
                let name =
                    match op with
                    | BinaryPlus -> "op_Addition"
                    | BinaryMinus -> "op_Subtraction"
                    | BinaryMultiply -> "op_Multiply"
                    | BinaryDivide -> "op_Division"
                    | BinaryModulus -> "op_Modulus"
                    | BinaryExponent -> "op_Exponentiation"
                    | BinaryOrBitwise -> "op_BitwiseOr"
                    | BinaryAndBitwise -> "op_BitwiseAnd"
                    | BinaryXorBitwise -> "op_ExclusiveOr"
                    | BinaryShiftLeft -> "op_LeftShift"
                    | BinaryShiftRightSignPropagating -> "op_RightShift"
                    | BinaryShiftRightZeroFill -> "op_UnsignedRightShift"
                    | BinaryEqual -> "op_Equality"
                    | BinaryUnequal -> "op_Inequality"
                    | BinaryLess -> "op_LessThan"
                    | BinaryLessOrEqual -> "op_LessThanOrEqual"
                    | BinaryGreater -> "op_GreaterThan"
                    | BinaryGreaterOrEqual -> "op_GreaterThanOrEqual"

                name, [ left; right ]
            | Logical(op, left, right) ->
                let name =
                    match op with
                    | LogicalAnd -> "op_BooleanAnd"
                    | LogicalOr -> "op_BooleanOr"

                name, [ left; right ]

        let methodExpr = makeStrConst opName
        let instanceExpr = mkNullExpr com "null"

        let argExprs = args |> List.map (emitQuotedExpr com) |> makeArray Any

        // Operators have no declaring type; pass an empty string.
        Helper.LibCall(com, "quotation", "mkCall", Any, [ instanceExpr; methodExpr; argExprs; makeStrConst "" ])

    | Get(expr, kind, _typ, _r) ->
        let target = emitQuotedExpr com expr

        match kind with
        | TupleIndex index -> Helper.LibCall(com, "quotation", "mkTupleGet", Any, [ target; makeIntConst index ])
        | UnionTag -> Helper.LibCall(com, "quotation", "mkUnionTag", Any, [ target ])
        | UnionField info ->
            Helper.LibCall(com, "quotation", "mkUnionField", Any, [ target; makeIntConst info.FieldIndex ])
        | FieldGet info -> Helper.LibCall(com, "quotation", "mkFieldGet", Any, [ target; makeStrConst info.Name ])
        | ListHead
        | ListTail
        | OptionValue ->
            // Represent option/list value accessors as property-getter calls,
            // mirroring how F# quotations model them (PropertyGet get_Value/get_Head/get_Tail).
            let methodName =
                match kind with
                | ListHead -> "get_Head"
                | ListTail -> "get_Tail"
                | _ -> "get_Value"

            let emptyArgs = makeArray Any []

            Helper.LibCall(
                com,
                "quotation",
                "mkCall",
                Any,
                [ target; makeStrConst methodName; emptyArgs; makeStrConst "" ]
            )
        | _ ->
            // TupleIndex/UnionTag/UnionField/FieldGet handled above; ExprGet and any
            // future kinds fall through here.
            let msg = "Unsupported quotation Get kind"
            Helper.LibCall(com, "quotation", "mkValue", Any, [ makeStrConst msg; makeStrConst "string" ])

    | Set(expr, kind, _typ, value, _r) ->
        let target = emitQuotedExpr com expr
        let valueExpr = emitQuotedExpr com value

        match kind with
        | ValueSet ->
            // Mutable variable set: expr is the ident, value is the new value
            Helper.LibCall(com, "quotation", "mkVarSet", Any, [ target; valueExpr ])
        | FieldSet fieldName ->
            Helper.LibCall(com, "quotation", "mkFieldSet", Any, [ target; makeStrConst fieldName; valueExpr ])
        | _ ->
            let msg = "Unsupported quotation Set kind"
            Helper.LibCall(com, "quotation", "mkValue", Any, [ makeStrConst msg; makeStrConst "string" ])

    | TypeCast(innerExpr, _typ) ->
        // Coerce/cast: just emit the inner expression for now
        emitQuotedExpr com innerExpr

    | DecisionTree(decisionExpr, targets) ->
        // Inline every DecisionTreeSuccess leaf into its target body, binding the
        // target's captured idents to the success's bound values with nested Lets.
        // This turns the compiled match into a plain IfThenElse/Let tree that the
        // quotation representation can express faithfully (a shared target is simply
        // inlined at each reference site).
        let inlined =
            decisionExpr
            |> visitFromInsideOut (fun e ->
                match e with
                | DecisionTreeSuccess(idx, boundValues, _) when idx < List.length targets ->
                    let idents, body = List.item idx targets

                    if List.length idents = List.length boundValues then
                        List.foldBack (fun (id, v) acc -> Let(id, v, acc)) (List.zip idents boundValues) body
                    else
                        e
                | e -> e
            )

        emitQuotedExpr com inlined

    | DecisionTreeSuccess(idx, boundValues, _typ) ->
        match boundValues with
        | [] -> emitQuotedExpr com (Value(UnitConstant, None))
        | [ single ] -> emitQuotedExpr com single
        | _ ->
            let msg = "Unsupported quotation node: DecisionTreeSuccess"
            Helper.LibCall(com, "quotation", "mkValue", Any, [ makeStrConst msg; makeStrConst "string" ])

    | Test(testExpr, kind, _r) ->
        let target = emitQuotedExpr com testExpr

        match kind with
        | UnionCaseTest tag ->
            // Represent as: (unionTag target) = tag
            let tagExpr = Helper.LibCall(com, "quotation", "mkUnionTag", Any, [ target ])
            let tagConst = emitQuotedExpr com (makeIntConst tag)

            Helper.LibCall(
                com,
                "quotation",
                "mkCall",
                Any,
                [
                    mkNullExpr com "null"
                    makeStrConst "op_Equality"
                    makeArray Any [ tagExpr; tagConst ]
                    makeStrConst ""
                ]
            )
        | OptionTest isSome ->
            let methodName =
                if isSome then
                    "get_IsSome"
                else
                    "get_IsNone"

            Helper.LibCall(
                com,
                "quotation",
                "mkCall",
                Any,
                [
                    target
                    makeStrConst methodName
                    makeArray Any []
                    makeStrConst "Microsoft.FSharp.Core.FSharpOption`1"
                ]
            )
        | ListTest isCons ->
            let methodName =
                if isCons then
                    "get_IsCons"
                else
                    "get_IsEmpty"

            Helper.LibCall(
                com,
                "quotation",
                "mkCall",
                Any,
                [
                    target
                    makeStrConst methodName
                    makeArray Any []
                    makeStrConst "Microsoft.FSharp.Collections.FSharpList`1"
                ]
            )
        | TypeTest typ ->
            Helper.LibCall(
                com,
                "quotation",
                "mkCall",
                Any,
                [
                    target
                    makeStrConst "op_TypeTest"
                    makeArray Any []
                    makeStrConst (typeToString typ)
                ]
            )

    | _ ->
        // Unsupported node: emit an error value
        let msg = $"Unsupported quotation node: %A{expr.GetType().Name}"

        Helper.LibCall(com, "quotation", "mkValue", Any, [ makeStrConst msg; makeStrConst "string" ])

and private mkNullExpr (com: Compiler) (typ: string) : Expr =
    // A runtime-built null node. Emitting this (rather than a raw null literal)
    // keeps generated code compiling on statically typed targets like Rust, while
    // producing the same ExprValue(null, typ) shape the other runtimes had before.
    Helper.LibCall(com, "quotation", "mkNull", Any, [ makeStrConst typ ])

and private emitQuotedValue (com: Compiler) (kind: ValueKind) (_r: SourceLocation option) : Expr =
    match kind with
    | BoolConstant b -> Helper.LibCall(com, "quotation", "mkValue", Any, [ makeBoolConst b; makeStrConst "bool" ])

    | NumberConstant(NumberValue.Int32 i, _) ->
        Helper.LibCall(com, "quotation", "mkValue", Any, [ makeIntConst i; makeStrConst "int32" ])

    | NumberConstant(NumberValue.Float64 f, _) ->
        let floatExpr = Value(NumberConstant(NumberValue.Float64 f, NumberInfo.Empty), None)
        Helper.LibCall(com, "quotation", "mkValue", Any, [ floatExpr; makeStrConst "float64" ])

    | StringConstant s -> Helper.LibCall(com, "quotation", "mkValue", Any, [ makeStrConst s; makeStrConst "string" ])

    | UnitConstant ->
        Helper.LibCall(com, "quotation", "mkValue", Any, [ Value(UnitConstant, None); makeStrConst "unit" ])

    | Null _ -> mkNullExpr com "null"

    | CharConstant c ->
        Helper.LibCall(com, "quotation", "mkValue", Any, [ Value(CharConstant c, None); makeStrConst "char" ])

    | NewTuple(values, _isStruct) ->
        let emittedValues = values |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "quotation", "mkNewTuple", Any, [ emittedValues ])

    | NewUnion(values, tag, entRef, _genArgs) ->
        let entName =
            match com.TryGetEntity(entRef) with
            | Some ent -> ent.FullName
            | None -> entRef.FullName

        let emittedValues = values |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "quotation", "mkNewUnion", Any, [ makeStrConst entName; makeIntConst tag; emittedValues ])

    | NewRecord(values, entRef, _genArgs) ->
        let fieldNames =
            match com.TryGetEntity(entRef) with
            | Some ent -> ent.FSharpFields |> List.map (fun f -> makeStrConst f.Name) |> makeArray Any
            | None -> makeArray Any []

        let emittedValues = values |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "quotation", "mkNewRecord", Any, [ fieldNames; emittedValues ])

    | NewOption(value, _typ, _isStruct) ->
        match value with
        | Some v ->
            let emitted = emitQuotedExpr com v
            Helper.LibCall(com, "quotation", "mkValue", Any, [ emitted; makeStrConst "option" ])
        | None -> mkNullExpr com "option"

    | NewList(headAndTail, _typ) ->
        match headAndTail with
        | Some(head, tail) ->
            let headExpr = emitQuotedExpr com head
            let tailExpr = emitQuotedExpr com tail
            Helper.LibCall(com, "quotation", "mkNewList", Any, [ headExpr; tailExpr ])
        | None -> mkNullExpr com "list"

    | _ ->
        // Fallback for other value kinds
        let msg = "Unsupported quotation value"
        Helper.LibCall(com, "quotation", "mkValue", Any, [ makeStrConst msg; makeStrConst "string" ])

and private numberKindToString (kind: NumberKind) : string =
    match kind with
    | Int8 -> "int8"
    | UInt8 -> "uint8"
    | Int16 -> "int16"
    | UInt16 -> "uint16"
    | Int32 -> "int32"
    | UInt32 -> "uint32"
    | Int64 -> "int64"
    | UInt64 -> "uint64"
    | Int128 -> "int128"
    | UInt128 -> "uint128"
    | BigInt -> "bigint"
    | NativeInt -> "nativeint"
    | UNativeInt -> "unativeint"
    | Float16 -> "float16"
    | Float32 -> "float32"
    | Float64 -> "float64"
    | Decimal -> "decimal"

and private typeToString (t: Type) : string =
    match t with
    | Boolean -> "bool"
    | Number(kind, _) -> numberKindToString kind
    | String -> "string"
    | Char -> "char"
    | Unit -> "unit"
    | Regex -> "regex"
    | Any -> "obj"
    | Option(genArg, isStruct) ->
        $"""%s{typeToString genArg} %s{if isStruct then
                                           "voption"
                                       else
                                           "option"}"""
    | List genArg -> $"%s{typeToString genArg} list"
    | Array(genArg, _) -> $"%s{typeToString genArg}[]"
    | Nullable(genArg, _) -> $"%s{typeToString genArg} nullable"
    | DeclaredType(ref, genArgs) ->
        match ref.FullName, genArgs with
        | "System.Collections.Generic.IEnumerable`1", [ t ] -> $"%s{typeToString t} seq"
        | "System.Guid", _ -> "guid"
        | "System.DateTime", _ -> "datetime"
        | "System.DateTimeOffset", _ -> "datetimeoffset"
        | "System.TimeSpan", _ -> "timespan"
        | fullName, _ -> fullName
    | GenericParam(name, _, _) -> $"'%s{name}"
    | LambdaType(argType, returnType) -> $"%s{typeToString argType} -> %s{typeToString returnType}"
    | DelegateType(argTypes, returnType) -> (argTypes @ [ returnType ]) |> List.map typeToString |> String.concat " -> "
    | Tuple(genArgs, _) -> genArgs |> List.map typeToString |> String.concat " * "
    | _ -> "obj"

and private makeArray (elementType: Type) (elements: Expr list) : Expr =
    Value(NewArray(ArrayValues elements, elementType, ImmutableArray), None)
