module Fable.Transforms.QuotationEmitter

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

/// Emits a Fable expression that, when compiled, produces runtime calls
/// to construct a quotation AST. The input is the Fable.Expr captured
/// inside a Quote node; the output is a Fable.Expr that calls the
/// fable_quotation runtime library to build the AST at runtime.
let rec emitQuotedExpr (com: Compiler) (expr: Expr) : Expr =
    match expr with
    | Value(kind, r) -> emitQuotedValue com kind r

    | IdentExpr ident ->
        // Reference to a variable already introduced by a lambda/let in the quotation.
        // Emit: fable_quotation:mk_var_expr(Var)
        // We need a var reference. Create a var and then wrap it.
        let varExpr =
            Helper.LibCall(
                com,
                "fable_quotation",
                "mk_var",
                Any,
                [
                    makeStrConst ident.Name
                    makeStrConst (typeToString ident.Type)
                    makeBoolConst false
                ]
            )

        Helper.LibCall(com, "fable_quotation", "mk_var_expr", Any, [ varExpr ])

    | Lambda(arg, body, _name) ->
        let varExpr =
            Helper.LibCall(
                com,
                "fable_quotation",
                "mk_var",
                Any,
                [
                    makeStrConst arg.Name
                    makeStrConst (typeToString arg.Type)
                    makeBoolConst false
                ]
            )

        let bodyExpr = emitQuotedExpr com body
        Helper.LibCall(com, "fable_quotation", "mk_lambda", Any, [ varExpr; bodyExpr ])

    | Delegate(args, body, _name, _tags) ->
        // Multi-arg delegate: nest as curried lambdas
        let rec nestLambdas args body =
            match args with
            | [] -> emitQuotedExpr com body
            | (arg: Ident) :: rest ->
                let varExpr =
                    Helper.LibCall(
                        com,
                        "fable_quotation",
                        "mk_var",
                        Any,
                        [
                            makeStrConst arg.Name
                            makeStrConst (typeToString arg.Type)
                            makeBoolConst false
                        ]
                    )

                let innerBody = nestLambdas rest body
                Helper.LibCall(com, "fable_quotation", "mk_lambda", Any, [ varExpr; innerBody ])

        nestLambdas args body

    | Let(ident, value, body) ->
        let varExpr =
            Helper.LibCall(
                com,
                "fable_quotation",
                "mk_var",
                Any,
                [
                    makeStrConst ident.Name
                    makeStrConst (typeToString ident.Type)
                    makeBoolConst ident.IsMutable
                ]
            )

        let valueExpr = emitQuotedExpr com value
        let bodyExpr = emitQuotedExpr com body

        Helper.LibCall(com, "fable_quotation", "mk_let", Any, [ varExpr; valueExpr; bodyExpr ])

    | IfThenElse(guardExpr, thenExpr, elseExpr, _r) ->
        let guard = emitQuotedExpr com guardExpr
        let thenE = emitQuotedExpr com thenExpr
        let elseE = emitQuotedExpr com elseExpr
        Helper.LibCall(com, "fable_quotation", "mk_if_then_else", Any, [ guard; thenE; elseE ])

    | CurriedApply(applied, args, _typ, _r) ->
        // Emit nested applications: Application(Application(f, a1), a2)
        let appliedExpr = emitQuotedExpr com applied

        args
        |> List.fold
            (fun acc arg ->
                let argExpr = emitQuotedExpr com arg
                Helper.LibCall(com, "fable_quotation", "mk_app", Any, [ acc; argExpr ])
            )
            appliedExpr

    | Call(callee, info, _typ, _r) ->
        let instanceExpr =
            match info.ThisArg with
            | Some thisArg -> emitQuotedExpr com thisArg
            | None -> Value(Null Any, None)

        let methodName =
            match info.MemberRef with
            | Some(MemberRef(_, mInfo)) -> mInfo.CompiledName
            | _ -> "unknown"

        let methodExpr = makeStrConst methodName

        let argExprs = info.Args |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "fable_quotation", "mk_call", Any, [ instanceExpr; methodExpr; argExprs ])

    | Sequential exprs ->
        match exprs with
        | [] -> emitQuotedExpr com (Value(UnitConstant, None))
        | [ single ] -> emitQuotedExpr com single
        | first :: rest ->
            let restExpr = emitQuotedExpr com (Sequential rest)
            let firstExpr = emitQuotedExpr com first
            Helper.LibCall(com, "fable_quotation", "mk_sequential", Any, [ firstExpr; restExpr ])

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
        let instanceExpr = Value(Null Any, None)

        let argExprs = args |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "fable_quotation", "mk_call", Any, [ instanceExpr; methodExpr; argExprs ])

    | Get(expr, kind, _typ, _r) ->
        let target = emitQuotedExpr com expr

        match kind with
        | TupleIndex index ->
            Helper.LibCall(com, "fable_quotation", "mk_tuple_get", Any, [ target; makeIntConst index ])
        | UnionTag -> Helper.LibCall(com, "fable_quotation", "mk_union_tag", Any, [ target ])
        | UnionField info ->
            Helper.LibCall(com, "fable_quotation", "mk_union_field", Any, [ target; makeIntConst info.FieldIndex ])
        | FieldGet info ->
            Helper.LibCall(com, "fable_quotation", "mk_field_get", Any, [ target; makeStrConst info.Name ])
        | _ ->
            // ListHead, ListTail, OptionValue, ExprGet — fall through
            let msg = $"Unsupported quotation Get kind"
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst msg; makeStrConst "string" ])

    | Set(expr, kind, _typ, value, _r) ->
        let target = emitQuotedExpr com expr
        let valueExpr = emitQuotedExpr com value

        match kind with
        | ValueSet ->
            // Mutable variable set: expr is the ident, value is the new value
            Helper.LibCall(com, "fable_quotation", "mk_var_set", Any, [ target; valueExpr ])
        | FieldSet fieldName ->
            Helper.LibCall(com, "fable_quotation", "mk_field_set", Any, [ target; makeStrConst fieldName; valueExpr ])
        | _ ->
            let msg = $"Unsupported quotation Set kind"
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst msg; makeStrConst "string" ])

    | TypeCast(innerExpr, _typ) ->
        // Coerce/cast: just emit the inner expression for now
        emitQuotedExpr com innerExpr

    | DecisionTree(matchExpr, targets) ->
        // Simple pattern: if this is a single-target decision tree (e.g. let binding),
        // emit the target body directly. Otherwise fall through to unsupported.
        match targets with
        | [ ([], body) ] -> emitQuotedExpr com body
        | _ ->
            let msg = "Unsupported quotation node: DecisionTree"
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst msg; makeStrConst "string" ])

    | DecisionTreeSuccess(idx, boundValues, _typ) ->
        match boundValues with
        | [] -> emitQuotedExpr com (Value(UnitConstant, None))
        | [ single ] -> emitQuotedExpr com single
        | _ ->
            let msg = "Unsupported quotation node: DecisionTreeSuccess"
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst msg; makeStrConst "string" ])

    | _ ->
        // Unsupported node: emit an error value
        let msg = $"Unsupported quotation node: %A{expr.GetType().Name}"

        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst msg; makeStrConst "string" ])

and private emitQuotedValue (com: Compiler) (kind: ValueKind) (_r: SourceLocation option) : Expr =
    match kind with
    | BoolConstant b ->
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeBoolConst b; makeStrConst "bool" ])

    | NumberConstant(NumberValue.Int32 i, _) ->
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeIntConst i; makeStrConst "int32" ])

    | NumberConstant(NumberValue.Float64 f, _) ->
        let floatExpr = Value(NumberConstant(NumberValue.Float64 f, NumberInfo.Empty), None)
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ floatExpr; makeStrConst "float64" ])

    | StringConstant s ->
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst s; makeStrConst "string" ])

    | UnitConstant ->
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ Value(UnitConstant, None); makeStrConst "unit" ])

    | Null _ -> Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ Value(Null Any, None); makeStrConst "null" ])

    | CharConstant c ->
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ Value(CharConstant c, None); makeStrConst "char" ])

    | NewTuple(values, _isStruct) ->
        let emittedValues = values |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "fable_quotation", "mk_new_tuple", Any, [ emittedValues ])

    | NewUnion(values, tag, entRef, _genArgs) ->
        let entName =
            match com.TryGetEntity(entRef) with
            | Some ent -> ent.FullName
            | None -> entRef.FullName

        let emittedValues = values |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(
            com,
            "fable_quotation",
            "mk_new_union",
            Any,
            [ makeStrConst entName; makeIntConst tag; emittedValues ]
        )

    | NewRecord(values, entRef, _genArgs) ->
        let fieldNames =
            match com.TryGetEntity(entRef) with
            | Some ent -> ent.FSharpFields |> List.map (fun f -> makeStrConst f.Name) |> makeArray Any
            | None -> makeArray Any []

        let emittedValues = values |> List.map (emitQuotedExpr com) |> makeArray Any

        Helper.LibCall(com, "fable_quotation", "mk_new_record", Any, [ fieldNames; emittedValues ])

    | NewOption(value, _typ, _isStruct) ->
        match value with
        | Some v ->
            let emitted = emitQuotedExpr com v
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ emitted; makeStrConst "option" ])
        | None ->
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ Value(Null Any, None); makeStrConst "option" ])

    | NewList(headAndTail, _typ) ->
        match headAndTail with
        | Some(head, tail) ->
            let headExpr = emitQuotedExpr com head
            let tailExpr = emitQuotedExpr com tail
            Helper.LibCall(com, "fable_quotation", "mk_new_list", Any, [ headExpr; tailExpr ])
        | None ->
            Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ Value(Null Any, None); makeStrConst "list" ])

    | _ ->
        // Fallback for other value kinds
        let msg = $"Unsupported quotation value"
        Helper.LibCall(com, "fable_quotation", "mk_value", Any, [ makeStrConst msg; makeStrConst "string" ])

and private typeToString (t: Type) : string =
    match t with
    | Boolean -> "bool"
    | Number(Int32, _) -> "int32"
    | Number(Float64, _) -> "float64"
    | String -> "string"
    | Unit -> "unit"
    | Any -> "obj"
    | LambdaType(argType, returnType) -> $"{typeToString argType} -> {typeToString returnType}"
    | Tuple(genArgs, _) -> genArgs |> List.map typeToString |> String.concat " * "
    | _ -> "obj"

and private makeArray (elementType: Type) (elements: Expr list) : Expr =
    Value(NewArray(ArrayValues elements, elementType, ImmutableArray), None)
