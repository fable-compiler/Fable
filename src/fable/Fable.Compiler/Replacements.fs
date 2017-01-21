module Fable.Replacements
open Fable
open Fable.AST
open Fable.AST.Fable.Util

module Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] fableCore = "Fable.Core."
    let [<Literal>] genericCollections = "System.Collections.Generic."

    let inline (=>) first second = first, second

    let (|CoreMeth|_|) coreMod meth expr =
        match expr with
        | Fable.Value(Fable.ImportRef(meth', coreMod', Fable.CoreLib))
            when meth' = meth && coreMod' = coreMod ->
            Some CoreMeth
        | _ -> None

    let (|CoreCons|_|) coreMod expr =
        match expr with
        | Fable.Apply(Fable.Value(Fable.ImportRef("default", coreMod', Fable.CoreLib)),[], Fable.ApplyCons,_,_)
            when coreMod' = coreMod -> Some CoreCons
        | _ -> None

    let (|Null|_|) = function
        | Fable.Wrapped(Fable.Value Fable.Null,_)
        | Fable.Value Fable.Null -> Some null
        | _ -> None

    let (|Type|) (expr: Fable.Expr) = expr.Type

    let (|NumberType|_|) = function
        | Fable.Number kind -> Some kind
        | _ -> None

    let (|Number|ExtNumber|NoNumber|) = function
        | Fable.Number kind -> Number kind
        | Fable.ExtendedNumber kind -> ExtNumber kind
        | _ -> NoNumber

    let (|EntFullName|_|) (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType(ent, _) -> Some ent.FullName
        | _ -> None

    let (|DeclaredKind|_|) (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType(ent, _) -> Some ent.Kind
        | _ -> None

    let (|KeyValue|_|) (key: string) (value: string) (s: string) =
        if s = key then Some value else None

    let (|OneArg|_|) (callee: Fable.Expr option, args: Fable.Expr list) =
        match callee, args with None, arg::_ -> Some arg | _ -> None

    let (|TwoArgs|_|) (callee: Fable.Expr option, args: Fable.Expr list) =
        match callee, args with None, left::right::_ -> Some (left, right) | _ -> None

    let (|ThreeArgs|_|) (callee: Fable.Expr option, args: Fable.Expr list) =
        match callee, args with None, arg1::arg2::arg3::_ -> Some (arg1, arg2, arg3) | _ -> None

    let (|Integer|Float|) = function
        | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> Integer
        | Float32 | Float64 | Decimal -> Float

    // The core lib expects non-curried lambdas
    let deleg com (info: Fable.ApplyInfo) args =
        if info.lambdaArgArity > 1
        then List.mapi (fun i x ->
            if i=0 then (makeDelegate com (Some info.lambdaArgArity) x) else x) args
        else args

    let resolveTypeRef com (info: Fable.ApplyInfo) generic t =
        let genInfo =
            { makeGeneric = generic
            ; genericAvailability = info.genericAvailability }
        match t with
        | Fable.GenericParam _ when not info.genericAvailability ->
            "`typeof` is being called on a generic parameter, "
            + "consider inlining the method (for `internal` members) "
            + "or using `PassGenericsAttribute`."
            |> addWarning com info.fileName info.range
            makeTypeRef com genInfo t
        | t -> makeTypeRef com genInfo t

    let instanceArgs (callee: Fable.Expr option) (args: Fable.Expr list) =
        match callee with
        | Some callee -> (callee, args)
        | None -> (args.Head, args.Tail)

    let staticArgs (callee: Fable.Expr option) (args: Fable.Expr list) =
        match callee with
        | Some callee -> callee::args
        | None -> args

    let icall com (i: Fable.ApplyInfo) meth =
        let c, args = instanceArgs i.callee i.args
        InstanceCall(c, meth, args)
        |> makeCall i.range i.returnType

    let ccall com (i: Fable.ApplyInfo) cmod meth args =
        CoreLibCall(cmod, Some meth, false, args)
        |> makeCall i.range i.returnType

    let emit (i: Fable.ApplyInfo) emit args =
        makeEmit i.range i.returnType args emit

    let emitNoInfo emit args =
        makeEmit None Fable.Any args emit

    let wrap typ expr =
        Fable.Wrapped (expr, typ)

    let wrapInLambda args f =
        List.map (Fable.IdentValue >> Fable.Value) args
        |> f |> makeLambdaExpr args

    let genArg (t: Fable.Type) =
        match t.GenericArgs with
        | [genArg] -> genArg
        | _ -> Fable.Any

    let defaultof (t: Fable.Type) =
        match t with
        | Fable.Number _ -> makeIntConst 0
        | Fable.Boolean -> makeBoolConst false
        | _ -> Fable.Null |> Fable.Value

    let getProp r t callee (prop: string) =
        Fable.Apply(callee, [Fable.Value(Fable.StringConst prop)], Fable.ApplyGet, t, r)

    let newError r t args =
        Fable.Apply(makeIdentExpr "Error", args, Fable.ApplyCons, t, r)

    let toChar com (i: Fable.ApplyInfo) (arg: Fable.Expr) =
        match arg.Type with
        | Fable.Char
        | Fable.String -> arg
        | _ -> GlobalCall ("String", Some "fromCharCode", false, [arg])
               |> makeCall i.range i.returnType

    let toString com (i: Fable.ApplyInfo) (arg: Fable.Expr) =
        match arg.Type with
        | Fable.Char
        | Fable.String -> arg
        | Fable.Unit | Fable.Boolean | Fable.Number _
        | Fable.Array _ | Fable.Tuple _ | Fable.Function _ | Fable.Enum _ ->
            GlobalCall ("String", None, false, [arg])
            |> makeCall i.range i.returnType
        | Fable.MetaType | Fable.Any | Fable.GenericParam _
        | Fable.ExtendedNumber _ | Fable.DeclaredType _ | Fable.Option _ ->
            CoreLibCall ("Util", Some "toString", false, [arg])
            |> makeCall i.range i.returnType

    let toFloat com (i: Fable.ApplyInfo) (args: Fable.Expr list) =
        match args.Head.Type with
        | Fable.String ->
            GlobalCall ("Number", Some "parseFloat", false, args)
            |> makeCall i.range i.returnType
        | Fable.ExtendedNumber (Int64 | UInt64) ->
            InstanceCall (args.Head, "toNumber", args.Tail)
            |> makeCall i.range (Fable.Number Float64)
        | Fable.ExtendedNumber BigInt ->
            let meth = match i.returnType with
                        | Number Float32 -> "toSingle"
                        | Number Float64 -> "toDouble"
                        | Number Decimal -> "toDecimal"
                        | _ -> failwith "Unexpected conversion"
            CoreLibCall("BigInt", Some meth, false, args)
            |> makeCall i.range i.returnType
        | _ ->
            wrap i.returnType args.Head

    let toInt com (i: Fable.ApplyInfo) (args: Fable.Expr list) =
        let notImplemented typ =
            FableError(sprintf "Converting from/to %s is not yet supported" typ, ?range=i.range)
            |> raise
        let kindIndex t =            //         0   1   2   3   4   5   6   7   8   9  10  11
            match t with             //         i8 i16 i32 i64  u8 u16 u32 u64 f32 f64 dec big
            | Number Int8 -> 0       //  0 i8   -   -   -   -   +   +   +   +   -   -   -   +
            | Number Int16 -> 1      //  1 i16  +   -   -   -   +   +   +   +   -   -   -   +
            | Number Int32 -> 2      //  2 i32  +   +   -   -   +   +   +   +   -   -   -   +
            | ExtNumber Int64 -> 3   //  3 i64  +   +   +   -   +   +   +   +   -   -   -   +
            | Number UInt8 -> 4      //  4 u8   +   +   +   +   -   -   -   -   -   -   -   +
            | Number UInt16 -> 5     //  5 u16  +   +   +   +   +   -   -   -   -   -   -   +
            | Number UInt32 -> 6     //  6 u32  +   +   +   +   +   +   -   -   -   -   -   +
            | ExtNumber UInt64 -> 7  //  7 u64  +   +   +   +   +   +   +   -   -   -   -   +
            | Number Float32 -> 8    //  8 f32  +   +   +   +   +   +   +   +   -   -   -   +
            | Number Float64 -> 9    //  9 f64  +   +   +   +   +   +   +   +   -   -   -   +
            | Number Decimal -> 10   // 10 dec  +   +   +   +   +   +   +   +   -   -   -   +
            | ExtNumber BigInt -> 11 // 11 big  +   +   +   +   +   +   +   +   +   +   +   -
            | NoNumber -> failwith "Unexpected non-number type"
        let needToCast typeFrom typeTo =
            let v = kindIndex typeFrom // argument type (vertical)
            let h = kindIndex typeTo   // return type (horizontal)
            ((v > h) || (v < 4 && h > 3)) && (h < 8) || (h <> v && (h = 11 || v = 11))
        let emitLong unsigned (args: Fable.Expr list) =
            match args.Head.Type with
            | ExtNumber (Int64|UInt64) ->
                CoreLibCall("Long", Some "fromValue", false, args)
            | _ -> CoreLibCall("Long", Some "fromNumber", false, args@[makeBoolConst unsigned])
            |> makeCall i.range i.returnType
        let emitBigInt (args: Fable.Expr list) =
            match args.Head.Type with
            | ExtNumber (Int64|UInt64) ->
                CoreLibCall("BigInt", Some "fromInt64", false, args)
            | _ -> CoreLibCall("BigInt", Some "fromInt32", false, args)
            |> makeCall i.range i.returnType
        let emitCast typeTo args =
            match typeTo with
            | ExtNumber BigInt -> emitBigInt args
            | ExtNumber UInt64 -> emitLong true args
            | ExtNumber Int64 -> emitLong false args
            | Number Int8 -> emit i "($0 + 0x80 & 0xFF) - 0x80" args
            | Number Int16 -> emit i "($0 + 0x8000 & 0xFFFF) - 0x8000" args
            | Number Int32 -> emit i "~~$0" args
            | Number UInt8 -> emit i "$0 & 0xFF" args
            | Number UInt16 -> emit i "$0 & 0xFFFF" args
            | Number UInt32 -> emit i "$0 >>> 0" args
            | Number _ -> emit i "$0" args
            | NoNumber -> failwith "Unexpected non-number type"
        let castBigIntMethod typeTo =
            match typeTo with
            | ExtNumber BigInt -> failwith "Unexpected conversion"
            | Number Int8 -> "toSByte"
            | Number Int16 -> "toInt16"
            | Number Int32 -> "toInt32"
            | ExtNumber Int64 -> "toInt64"
            | Number UInt8 -> "toByte"
            | Number UInt16 -> "toUInt16"
            | Number UInt32 -> "toUInt32"
            | ExtNumber UInt64 -> "toUInt64"
            | Number Float32 -> "toSingle"
            | Number Float64 -> "toDouble"
            | Number Decimal -> "toDecimal"
            | NoNumber -> failwith "Unexpected non-number type"
        match args.Head.Type with
        | Fable.Char ->
            InstanceCall(args.Head, "charCodeAt", [makeIntConst 0])
            |> makeCall i.range i.returnType
        | Fable.String ->
            match i.returnType with
            | Fable.ExtendedNumber (Int64|UInt64 as kind) ->
                let unsigned = kind = UInt64
                let args = [args.Head]@[makeBoolConst unsigned]@args.Tail
                CoreLibCall ("Long", Some "fromString", false, args)
                |> makeCall i.range i.returnType
            | _ ->
                GlobalCall ("Number", Some "parseInt", false, args)
                |> makeCall i.range i.returnType
        | ExtNumber BigInt ->
            let meth = castBigIntMethod i.returnType
            CoreLibCall("BigInt", Some meth, false, args)
            |> makeCall i.range i.returnType
        | Number _ | ExtNumber _ as typeFrom ->
            match i.returnType with
            | Number _ | ExtNumber _ as typeTo when needToCast typeFrom typeTo ->
                match typeFrom, typeTo with
                | ExtNumber (UInt64|Int64), Number _ ->
                    InstanceCall (args.Head, "toNumber", args.Tail)
                    |> makeCall i.range (Fable.Number Float64)
                | Number Float, (Number Integer | ExtNumber(Int64|UInt64)) when i.ownerFullName = "System.Convert" ->
                    CoreLibCall("Util", Some "round", false, args)
                    |> makeCall i.range i.returnType
                | _, _ -> args.Head
                |> List.singleton
                |> emitCast typeTo
            | ExtNumber (UInt64|Int64 as kind) ->
                emitLong (kind = UInt64) [args.Head]
            | Number _ -> emit i "$0" [args.Head]
            | _ -> wrap i.returnType args.Head
        | _ -> wrap i.returnType args.Head

    let toList com (i: Fable.ApplyInfo) expr =
        CoreLibCall ("Seq", Some "toList", false, [expr])
        |> makeCall i.range i.returnType

    let toArray (com: Fable.ICompiler) (i: Fable.ApplyInfo) expr =
        let arrayFrom arrayCons expr =
            GlobalCall (arrayCons, Some "from", false, [expr])
            |> makeCall i.range i.returnType
        match expr, i.returnType with
        // Optimization
        | Fable.Apply(CoreMeth "List" "ofArray" _, [arr], Fable.ApplyMeth,_,_), _ -> arr
        | CoreCons "List", _ ->
            Fable.ArrayConst(Fable.ArrayValues [], genArg i.returnType) |> Fable.Value
        // Typed arrays
        | _, Fable.Array(Fable.Number numberKind) when not com.Options.noTypedArrays ->
            arrayFrom (getTypedArrayName com numberKind) expr
        | _ -> arrayFrom "Array" expr

    let applyOp com (i: Fable.ApplyInfo) (args: Fable.Expr list) meth =
        let replacedEntities =
            set [ "System.TimeSpan"; "System.DateTime"; "Microsoft.FSharp.Collections.FSharpSet" ]
        let (|CustomOp|_|) meth argTypes (ent: Fable.Entity) =
            if replacedEntities.Contains ent.FullName then None else
            ent.TryGetMember(meth, Fable.Method, Fable.StaticLoc, argTypes)
            |> function None -> None | Some m -> Some(ent, m)
        let apply op args =
            Fable.Apply(Fable.Value op, args, Fable.ApplyMeth, i.returnType, i.range)
        let nativeOp leftOperand = function
            | "op_Addition" -> Fable.BinaryOp BinaryPlus
            | "op_Subtraction" -> Fable.BinaryOp BinaryMinus
            | "op_Multiply" -> Fable.BinaryOp BinaryMultiply
            | "op_Division" -> Fable.BinaryOp BinaryDivide
            | "op_Modulus" -> Fable.BinaryOp BinaryModulus
            | "op_LeftShift" -> Fable.BinaryOp BinaryShiftLeft
            | "op_RightShift" ->
                match leftOperand with
                | Fable.Number UInt32 -> Fable.BinaryOp BinaryShiftRightZeroFill // See #646
                | _ -> Fable.BinaryOp BinaryShiftRightSignPropagating
            | "op_BitwiseAnd" -> Fable.BinaryOp BinaryAndBitwise
            | "op_BitwiseOr" -> Fable.BinaryOp BinaryOrBitwise
            | "op_ExclusiveOr" -> Fable.BinaryOp BinaryXorBitwise
            | "op_LogicalNot" -> Fable.UnaryOp UnaryNotBitwise
            | "op_UnaryNegation" -> Fable.UnaryOp UnaryMinus
            | "op_BooleanAnd" -> Fable.LogicalOp LogicalAnd
            | "op_BooleanOr" -> Fable.LogicalOp LogicalOr
            | _ -> failwithf "Unknown operator: %s" meth
        let argTypes = List.map Fable.Expr.getType args
        match argTypes with
        | Fable.DeclaredType(CustomOp meth argTypes (ent, m), _)::_
        | _::[Fable.DeclaredType(CustomOp meth argTypes (ent, m), _)] ->
            let typRef = Fable.Value(Fable.TypeRef(ent,[]))
            InstanceCall(typRef, m.OverloadName, args)
            |> makeCall i.range i.returnType
        | Fable.ExtendedNumber BigInt::_
        | _::[Fable.ExtendedNumber BigInt] ->
            CoreLibCall ("BigInt", Some meth, false, args)
            |> makeCall i.range i.returnType
        | Fable.ExtendedNumber (Int64|UInt64)::_
        | _::[Fable.ExtendedNumber (Int64|UInt64)] ->
            let meth =
                match meth with
                | "op_Addition" -> "add"
                | "op_Subtraction" -> "sub"
                | "op_Multiply" -> "mul"
                | "op_Division" -> "div"
                | "op_Modulus" -> "mod"
                | "op_LeftShift" -> "shl"
                | "op_RightShift" -> "shr"
                | "op_BitwiseAnd" -> "and"
                | "op_BitwiseOr" -> "or"
                | "op_ExclusiveOr" -> "xor"
                | "op_LogicalNot" -> "not"
                | "op_UnaryNegation" -> "neg"
                | _ -> failwithf "Unknown operator: %s" meth
            InstanceCall (args.Head, meth, args.Tail)
            |> makeCall i.range i.returnType
        // Floor result of integer divisions (see #172)
        // Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
        | Fable.Number Integer::_ when meth = "op_Division" ->
            apply (Fable.BinaryOp BinaryDivide) args
            |> List.singleton |> apply (Fable.UnaryOp UnaryNotBitwise)
            |> List.singleton |> apply (Fable.UnaryOp UnaryNotBitwise)
        | EntFullName (KeyValue "System.DateTime" "Date" modName)::_
        | EntFullName (KeyValue "Microsoft.FSharp.Collections.FSharpSet" "Set" modName)::_ ->
            CoreLibCall (modName, Some meth, false, args)
            |> makeCall i.range i.returnType
        | (Fable.Boolean | Fable.Char | Fable.String | Fable.Number _ | Fable.Enum _)::_ ->
            apply (nativeOp argTypes.Head meth) args
        | _ ->
            "The type of the operands is unknown at compile, native JS operator will be applied."
            |> addWarning com i.fileName i.range
            apply (nativeOp argTypes.Head meth) args

    let equals equal com (i: Fable.ApplyInfo) (args: Fable.Expr list) =
        let op equal =
            if equal then BinaryEqualStrict else BinaryUnequalStrict
            |> Fable.BinaryOp |> Fable.Value
        let is equal expr =
            if equal then expr
            else makeUnOp i.range i.returnType [expr] UnaryNot
        let icall (args: Fable.Expr list) equal =
            InstanceCall(args.Head, "Equals", args.Tail)
            |> makeCall i.range i.returnType |> is equal |> Some
        match args.Head.Type with
        | EntFullName "System.DateTime" ->
            CoreLibCall ("Date", Some "equals", false, args)
            |> makeCall i.range i.returnType |> is equal |> Some
        | Fable.DeclaredType(ent, _)
            when (ent.HasInterface "System.IEquatable"
                    && ent.FullName <> "System.TimeSpan")
                || ent.FullName = "Microsoft.FSharp.Collections.FSharpList"
                || ent.FullName = "Microsoft.FSharp.Collections.FSharpMap"
                || ent.FullName = "Microsoft.FSharp.Collections.FSharpSet" ->
            icall args equal
        | Fable.ExtendedNumber _ ->
            icall args equal
        | Fable.Any | Fable.Unit | Fable.Boolean | Fable.Char | Fable.String
        | Fable.Number _ | Fable.Function _ | Fable.Enum _ ->
            Fable.Apply(op equal, args, Fable.ApplyMeth, i.returnType, i.range) |> Some
        | Fable.Array _ | Fable.Tuple _
        | Fable.MetaType | Fable.DeclaredType _ | Fable.GenericParam _ | Fable.Option _ ->
            CoreLibCall("Util", Some "equals", false, args)
            |> makeCall i.range i.returnType |> is equal |> Some

    /// Compare function that will call Util.compare or instance `CompareTo` as appropriate
    /// If passed an optional binary operator, it will wrap the comparison like `comparison < 0`
    let compare com r (args: Fable.Expr list) op =
        let wrapWith op comparison =
            match op with
            | None -> comparison
            | Some op -> makeEqOp r [comparison; makeIntConst 0] op
        let icall (args: Fable.Expr list) op =
            InstanceCall(args.Head, "CompareTo", args.Tail)
            |> makeCall r (Fable.Number Int32) |> wrapWith op
        match args.Head.Type with
        | Fable.DeclaredType(ent, _)
            when ent.HasInterface "System.IComparable"
                && ent.FullName <> "System.TimeSpan"
                && ent.FullName <> "System.DateTime" ->
            icall args op
        | Fable.ExtendedNumber _ ->
            icall args op
        | Fable.Any | Fable.Unit | Fable.Boolean | Fable.String
        | Fable.Number _ | Fable.Function _ | Fable.Enum _ when Option.isSome op ->
            makeEqOp r args op.Value
        | _ ->
            CoreLibCall("Util", Some "compare", false, args)
            |> makeCall r (Fable.Number Int32) |> wrapWith op

    let makeComparer com (typArg: Fable.Type option) =
        match typArg with
        | None
        | Some(Fable.Option _) | Some(Fable.Array _ | Fable.Tuple _) ->
            [makeCoreRef "Util" (Some "compare")]
        | Some(Fable.DeclaredType(ent, _))
            when ent.HasInterface "System.IComparable"
                && ent.FullName <> "System.TimeSpan"
                && ent.FullName <> "System.DateTime" ->
            [emitNoInfo "(x,y) => x.CompareTo(y)" []]
        | Some _ -> [makeCoreRef "Util" (Some "compare")]
        |> fun args -> CoreLibCall("GenericComparer", None, true, args)
        |> makeCall None Fable.Any

    let makeMapOrSetCons com (i: Fable.ApplyInfo) modName args =
        let typArg =
            match i.calleeTypeArgs, i.methodTypeArgs with
            | x::_, _ | [], x::_ -> Some x
            | [], [] -> None
        let args =
            (if List.isEmpty args then [Fable.Value Fable.Null] else args)
            @ [makeComparer com typArg]
        CoreLibCall(modName, Some "create", false, args)
        |> makeCall i.range i.returnType

module AstPass =
    open Util

    let fableCoreLib com (i: Fable.ApplyInfo) =
        let destruct = function
            | Fable.Value(Fable.TupleConst exprs) -> exprs
            | expr when expr.Type = Fable.Unit -> []
            | expr -> [expr]
        match i.methodName with
        | Naming.StartsWith "import" _ ->
            let fail() =
                FableError(sprintf "%s.%s only accepts literal strings"
                            i.ownerFullName i.methodName, ?range=i.range) |> raise
            let selector, args =
                match i.methodName with
                | "import" ->
                    match i.args with
                    | Fable.Value(Fable.StringConst selector)::args -> selector, args
                    | _ -> fail()
                | "importMember" -> Naming.placeholder, i.args
                | "importDefault" -> "default", i.args
                | _ -> "*", i.args // importAllFrom
            let path =
                match args with
                | [Fable.Value(Fable.StringConst path)] -> path
                | _ -> fail()
            Fable.ImportRef(selector, path, Fable.CustomImport) |> Fable.Value |> Some
        | "op_BangBang" ->
            Fable.Wrapped (i.args.Head, i.methodTypeArgs.Head) |> Some
        | "op_Dynamic" ->
            makeGet i.range i.returnType i.args.Head i.args.Tail.Head |> Some
        | "op_DynamicAssignment" ->
            match i.callee, i.args with
            | ThreeArgs (callee, prop, value) ->
                let value = makeDelegate com None value
                Fable.Set (callee, Some prop, value, i.range) |> Some
            | _ -> None
        | "op_Dollar" | "createNew" ->
            let args =
                destruct i.args.Tail.Head
                |> List.map (makeDelegate com None)
            let applyMeth =
                if i.methodName = "createNew"
                then Fable.ApplyCons else Fable.ApplyMeth
            Fable.Apply(i.args.Head, args, applyMeth, i.returnType, i.range) |> Some
        | "op_EqualsEqualsGreater" ->
            Fable.TupleConst(List.take 2 i.args) |> Fable.Value |> Some
        | "createObj" ->
            let (|Fields|_|) = function
                | Fable.Value(Fable.ArrayConst(Fable.ArrayValues exprs, _)) ->
                    exprs
                    |> List.choose (function
                        | Fable.Value(Fable.TupleConst [Fable.Value(Fable.StringConst key); value]) ->
                            Some(key, makeDelegate com None value)
                        | _ -> None)
                    |> function
                        | fields when fields.Length = exprs.Length -> Some fields
                        | _ -> None
                | _ -> None
            match i.args.Head with
            | CoreCons "List" ->
                makeJsObject i.range [] |> Some
            | Fable.Apply(_, [Fields fields], _, _, _) ->
                makeJsObject i.range fields |> Some
            | _ ->
                CoreLibCall("Util", Some "createObj", false, i.args)
                |> makeCall i.range i.returnType |> Some
        | "createEmpty" ->
            Fable.ObjExpr ([], [], None, i.range)
            |> wrap i.returnType |> Some
        | "areEqual" ->
            match i.args with
            | [expected; actual] -> Some [actual; expected]
            | [expected; actual; msg] -> Some [actual; expected; msg]
            | _ -> None
            |> Option.map (fun args ->
                CoreLibCall ("Assert", Some "equal", false, args)
                |> makeCall i.range i.returnType)
        | "async.AwaitPromise.Static" | "async.StartAsPromise.Static" ->
            let meth =
                if i.methodName = "async.AwaitPromise.Static"
                then "awaitPromise" else "startAsPromise"
            CoreLibCall("Async", Some meth, false, deleg com i i.args)
            |> makeCall i.range i.returnType |> Some
        | "toJson" | "ofJson" | "inflate" | "toPlainJsObj"
        | "toJsonWithTypeInfo" | "ofJsonWithTypeInfo" ->
            let modName = if i.methodName = "toPlainJsObj" then "Util" else "Serialize"
            CoreLibCall(modName, Some i.methodName, false, i.args)
            |> makeCall i.range i.returnType |> Some
        | "jsNative" ->
            // TODO: Fail at compile time?
            addWarning com i.fileName i.range "jsNative is being compiled without replacement, this will fail at runtime."
            "A function supposed to be replaced by JS native code has been called, please check."
            |> Fable.StringConst |> Fable.Value |> List.singleton
            |> newError i.range i.returnType
            |> fun err -> Fable.Throw(err, i.returnType, i.range) |> Some
        | _ -> None

    let references com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            makeJsObject i.range [("contents", i.args.Head)] |> Some
        | "contents" | "value" ->
            let prop = makeStrConst "contents"
            match i.methodKind with
            | Fable.Getter _ ->
                makeGet i.range Fable.Any i.callee.Value prop |> Some
            | Fable.Setter _ ->
                Fable.Set(i.callee.Value, Some prop, i.args.Head, i.range) |> Some
            | _ -> None
        | _ -> None

    let fsFormat com (i: Fable.ApplyInfo) =
        let emit macro =
            let emit = Fable.Emit macro |> Fable.Value
            Fable.Apply(i.args.Head, [emit], Fable.ApplyMeth, i.returnType, i.range)
            |> Some
        match i.methodName with
        | "printFormatToString"
        | "printFormatToStringThen" ->
            emit "x=>x"
        | "printFormat" ->
            addWarning com i.fileName i.range "printf will behave as printfn"
            emit "x=>{console.log(x)}"
        | "printFormatLine" ->
            emit "x=>{console.log(x)}"
        | "printFormatThen" ->
            Fable.Apply(i.args.Tail.Head, [i.args.Head], Fable.ApplyMeth, i.returnType, i.range)
            |> Some
        | "printFormatToStringThenFail" ->
            emit "x=>{throw new Error(x)}"
        | ".ctor" ->
            CoreLibCall("String", Some "fsFormat", false, i.args)
            |> makeCall i.range i.returnType |> Some
        | _ -> None

    let operators (com: ICompiler) (info: Fable.ApplyInfo) =
        let math range typ (args: Fable.Expr list) methName =
            match methName, typ with
            | "abs", Fable.ExtendedNumber Int64 ->
                InstanceCall (args.Head, "abs", args.Tail)
                |> makeCall range typ |> Some
            | "abs", Fable.ExtendedNumber BigInt ->
                ccall com info "BigInt" "abs" info.args |> Some
             | _ ->
                GlobalCall ("Math", Some methName, false, args)
                |> makeCall range typ |> Some
        let r, typ, args = info.range, info.returnType, info.args
        match info.methodName with
        | "keyValuePattern" ->
            info.args.Head |> Some
        | "defaultArg" ->
            match args with
            | [Fable.Value(Fable.IdentValue _) as arg1; arg2] ->
                let cond = makeEqOp r [arg1; Fable.Value Fable.Null] BinaryUnequal
                Fable.IfThenElse(cond, arg1, arg2, r) |> Some
            | args -> ccall com info "Util" "defaultArg" args |> Some
        | "defaultAsyncBuilder" -> makeCoreRef "AsyncBuilder" (Some "singleton") |> Some
        // Negation
        | "not" -> makeUnOp r info.returnType args UnaryNot |> Some
        // Equality
        | "op_Inequality" | "neq" ->
            match args with
            | [Fable.Value Fable.Null; _]
            | [_; Fable.Value Fable.Null] -> makeEqOp r args BinaryUnequal |> Some
            | _ -> equals false com info args
        | "op_Equality" | "eq" ->
            match args with
            | [Fable.Value Fable.Null; _]
            | [_; Fable.Value Fable.Null] -> makeEqOp r args BinaryEqual |> Some
            | _ -> equals true com info args
        | "isNull" -> makeEqOp r [args.Head; Fable.Value Fable.Null] BinaryEqual |> Some
        | "hash" ->
            CoreLibCall("Util", Some "hash", false, args)
            |> makeCall r typ |> Some
        // Comparison
        | "compare" -> compare com info.range args None |> Some
        | "op_LessThan" | "lt" -> compare com info.range args (Some BinaryLess) |> Some
        | "op_LessThanOrEqual" | "lte" -> compare com info.range args (Some BinaryLessOrEqual) |> Some
        | "op_GreaterThan" | "gt" -> compare com info.range args (Some BinaryGreater) |> Some
        | "op_GreaterThanOrEqual" | "gte" -> compare com info.range args (Some BinaryGreaterOrEqual) |> Some
        | "min" | "max" ->
            let op = if info.methodName = "min" then BinaryLess else BinaryGreater
            let comparison = compare com info.range args (Some op)
            Fable.IfThenElse(comparison, args.Head, args.Tail.Head, info.range) |> Some
        // Operators
         | "op_Addition" | "op_Subtraction" | "op_Multiply" | "op_Division"
         | "op_Modulus" | "op_LeftShift" | "op_RightShift"
         | "op_BitwiseAnd" | "op_BitwiseOr" | "op_ExclusiveOr"
         | "op_LogicalNot" | "op_UnaryNegation" | "op_BooleanAnd" | "op_BooleanOr" ->
            applyOp com info args info.methodName |> Some
        // Math functions
        // TODO: optimize square pow: x * x
        | "pow" | "powInteger" | "op_Exponentiation" -> math r typ args "pow"
        | "ceil" | "ceiling" -> math r typ args "ceil"
        | "abs" | "acos" | "asin" | "atan" | "atan2"
        | "cos"  | "exp" | "floor" | "log" | "log10"
        | "sin" | "sqrt" | "tan" ->
            math r typ args info.methodName
        | "round" ->
            CoreLibCall("Util", Some "round", false, args)
            |> makeCall r typ |> Some
        // Function composition
        | "op_ComposeRight" | "op_ComposeLeft" ->
            // If expression is a let binding we have to wrap it in a function
            let wrap expr placeholder =
                match expr with
                | Fable.Sequential _ -> sprintf "((()=>%s)())" placeholder
                | _ -> placeholder
            let args = if info.methodName = "op_ComposeRight" then args else List.rev args
            let f0 = wrap args.Head "$0"
            let f1 = wrap args.Tail.Head "$1"
            let pattern = System.String.Format("{0}=>{1}({2}({0}))", com.GetUniqueVar(), f1,f0)
            emit info pattern args |> Some
        // Reference
        | "op_Dereference" -> makeGet r Fable.Any args.Head (makeStrConst "contents") |> Some
        | "op_ColonEquals" -> Fable.Set(args.Head, Some(makeStrConst "contents"), args.Tail.Head, r) |> Some
        | "ref" -> makeJsObject r [("contents", args.Head)] |> Some
        | "increment" | "decrement" ->
            if info.methodName = "increment" then "++" else "--"
            |> sprintf "void($0.contents%s)"
            |> emit info <| args |> Some
        // Conversions
        | "createSequence" | "identity" | "box" | "unbox" -> wrap typ args.Head |> Some
        | "toSByte" | "toByte"
        | "toInt8" | "toUInt8"
        | "toInt16" | "toUInt16"
        | "toInt" | "toUInt"
        | "toInt32" | "toUInt32"
        | "toInt64" | "toUInt64"
            -> toInt com info args |> Some
        | "toSingle" | "toDouble" | "toDecimal" -> toFloat com info args |> Some
        | "toChar" -> toChar com info args.Head |> Some
        | "toString" -> toString com info args.Head |> Some
        | "toEnum" -> args.Head |> Some
        | "createDictionary" ->
            GlobalCall("Map", None, true, args) |> makeCall r typ |> Some
        | "createSet" ->
            makeMapOrSetCons com info "Set" args |> Some
        // Ignore: wrap to keep Unit type (see Fable2Babel.transformFunction)
        | "ignore" -> Fable.Wrapped (args.Head, Fable.Unit) |> Some
        // Ranges
        | "op_Range" | "op_RangeStep" ->
            let meth =
                match info.methodTypeArgs.Head with
                | Fable.Char -> "rangeChar"
                | _ -> if info.methodName = "op_Range" then "range" else "rangeStep"
            CoreLibCall("Seq", Some meth, false, args)
            |> makeCall r typ |> Some
        // Tuples
        | "fst" | "snd" ->
            if info.methodName = "fst" then 0 else 1
            |> makeIntConst
            |> makeGet r typ args.Head |> Some
        // Strings
        | "printFormatToString"             // sprintf
        | "printFormatToStringThen"         // Printf.sprintf
        | "printFormat" | "printFormatLine" // printf / printfn
        | "printFormatThen"                 // Printf.kprintf
        | "printFormatToStringThenFail" ->  // Printf.failwithf
            fsFormat com info
        // Exceptions
        | "raise" ->
            Fable.Throw (args.Head, typ, r) |> Some
        | "failWith"  | "reraise" | "invalidOp" | "invalidArg" ->
            let args =
                match info.methodName with
                | "invalidArg" -> [makeEmit None Fable.String args "$1 + '\\nParameter name: ' + $0"]
                | _ -> args
            Fable.Throw (newError None Fable.Any args, typ, r) |> Some
        // Type ref
        | "typeOf" | "typeDefOf" ->
            info.methodTypeArgs.Head
            |> resolveTypeRef com info (info.methodName = "typeOf")
            |> Some
        // Concatenates two lists
        | "op_Append" ->
          CoreLibCall("List", Some "append", false, args)
          |> makeCall r typ |> Some
        | _ -> None

    let strings com (i: Fable.ApplyInfo) =
        let icall2 meth (callee, args) =
            InstanceCall (callee, meth, args)
            |> makeCall i.range i.returnType
        match i.methodName with
        | ".ctor" ->
            match i.args.Head.Type with
            | Fable.String ->
                match i.args with
                | [c; n] -> emit i "Array($1 + 1).join($0)" i.args |> Some // String(char, int)
                | _ -> failwith "Unexpected arguments in System.String constructor."
            | Fable.Array _ ->
                match i.args with
                | [c] -> emit i "$0.join('')" i.args |> Some // String(char[])
                | [c; s; l] -> emit i "$0.join('').substr($1, $2)" i.args |> Some // String(char[], int, int)
                | _ -> failwith "Unexpected arguments in System.String constructor."
            | _ ->
                fsFormat com i
        | "length" ->
            let c, _ = instanceArgs i.callee i.args
            makeGet i.range i.returnType c (makeStrConst "length") |> Some
        | "equals" ->
            match i.callee, i.args with
            | Some x, [y]
            | None, [x; y] ->
                makeEqOp i.range [x; y] BinaryEqualStrict |> Some
            | Some x, [y; kind]
            | None, [x; y; kind] ->
                makeEqOp i.range [ccall com i "String" "compare" [x; y; kind]; makeIntConst 0] BinaryEqualStrict |> Some
            | _ -> None
        | "contains" ->
            if (List.length i.args) > 1 then addWarning com i.fileName i.range "String.Contains: second argument is ignored"
            makeEqOp i.range [icall2 "indexOf" (i.callee.Value, [i.args.Head]); makeIntConst 0] BinaryGreaterOrEqual |> Some
        | "startsWith" ->
            if (List.length i.args) > 1 then addWarning com i.fileName i.range "String.StartsWith: second argument is ignored"
            makeEqOp i.range [icall2 "indexOf" (i.callee.Value, [i.args.Head]); makeIntConst 0] BinaryEqualStrict |> Some
        | "substring" -> icall com i "substr" |> Some
        | "toUpper" -> icall com i "toLocaleUpperCase" |> Some
        | "toUpperInvariant" -> icall com i "toUpperCase" |> Some
        | "toLower" -> icall com i "toLocaleLowerCase" |> Some
        | "toLowerInvariant" -> icall com i "toLowerCase" |> Some
        | "indexOf" | "lastIndexOf" ->
            match i.args with
            | [Type Fable.Char]
            | [Type Fable.String]
            | [Type Fable.Char; Type(Fable.Number Int32)]
            | [Type Fable.String; Type(Fable.Number Int32)] -> icall com i i.methodName |> Some
            | _ -> FableError("The only extra argument accepted for String.IndexOf/LastIndexOf is startIndex.", ?range=i.range) |> raise
        | "trim" | "trimStart" | "trimEnd" ->
            let side =
                match i.methodName with
                | "trimStart" -> "start"
                | "trimEnd" -> "end"
                | _ -> "both"
            CoreLibCall("String", Some "trim", false, i.callee.Value::(makeStrConst side)::i.args)
            |> makeCall i.range i.returnType |> Some
        | "toCharArray" ->
            InstanceCall(i.callee.Value, "split", [makeStrConst ""])
            |> makeCall i.range i.returnType |> Some
        | "iterate" | "iterateIndexed" | "forAll" | "exists" ->
            CoreLibCall("Seq", Some i.methodName, false, deleg com i i.args)
            |> makeCall i.range i.returnType |> Some
        | "map" | "mapIndexed" | "collect"  ->
            CoreLibCall("Seq", Some i.methodName, false, deleg com i i.args)
            |> makeCall i.range Fable.Any
            |> List.singleton
            |> emit i "Array.from($0).join('')"
            |> Some
        | "concat" ->
            let args =
                if i.ownerFullName = "System.String"
                then (makeStrConst "")::i.args else i.args
            CoreLibCall("String", Some "join", false, args)
            |> makeCall i.range i.returnType |> Some
        | "split" ->
            match i.args with
            | [Fable.Value(Fable.StringConst _) as separator]
            | [Fable.Value(Fable.ArrayConst(Fable.ArrayValues [separator],_))] ->
                InstanceCall(i.callee.Value, "split", [separator]) // Optimization
            | [arg1; Type(Fable.Enum _) as arg2] ->
                let args = [arg1; Fable.Value Fable.Null; arg2]
                CoreLibCall("String", Some "split", false, i.callee.Value::args)
            | args -> CoreLibCall("String", Some "split", false, i.callee.Value::args)
            |> makeCall i.range Fable.String
            |> Some
        | _ -> None

    let log com (i: Fable.ApplyInfo) =
        let v =
            match i.args with
            | [] -> Fable.Value Fable.Null
            | [v] -> v
            | Type Fable.String::_ ->
                CoreLibCall("String", Some "format", false, i.args)
                |> makeCall i.range Fable.String
            | _ -> i.args.Head
        GlobalCall("console", Some "log", false, [v])
        |> makeCall i.range i.returnType

    let bitConvert com (i: Fable.ApplyInfo) =
        let methodName =
            if i.methodName = "getBytes" then
                match i.args.Head.Type with
                | Fable.Boolean -> "getBytesBoolean"
                | Fable.Char -> "getBytesChar"
                | Fable.Number Int16 -> "getBytesInt16"
                | Fable.Number Int32 -> "getBytesInt32"
                | Fable.ExtendedNumber Int64 -> "getBytesInt64"
                | Fable.Number UInt16 -> "getBytesUInt16"
                | Fable.Number UInt32 -> "getBytesUInt32"
                | Fable.ExtendedNumber UInt64 -> "getBytesUInt64"
                | Fable.Number Float32 -> "getBytesSingle"
                | Fable.Number Float64 -> "getBytesDouble"
                | x -> failwithf "Unsupported type in BitConverter.GetBytes(): %A" x
            else i.methodName
        CoreLibCall("BitConverter", Some methodName, false, i.args)
        |> makeCall i.range i.returnType |> Some

    let parse (com: ICompiler) (i: Fable.ApplyInfo) isFloat =
        let parseString str =
            let meth, args, kind =
                if isFloat
                then "parseFloat", [str], Float64
                else "parseInt", [str; makeIntConst 10], Int32
            GlobalCall("Number", Some meth, false, args)
            |> makeCall i.range (Fable.Number kind)
        match i.methodName with
        | "parse" | "tryParse" ->
            match i.methodName, i.args with
            | "parse", [str] ->
                parseString str |> Some
            | "tryParse", [str; defValue] ->
                let var = com.GetUniqueVar() |> makeIdent
                let setter = Fable.VarDeclaration(var, parseString str, false)
                let res = emit i "isNaN($0) ? [false, $1] : [true, $0]" [Fable.IdentValue var |> Fable.Value; defValue]
                Fable.Sequential([setter; res], i.range) |> Some
            | _ ->
                FableError(sprintf "%s.%s only accepts a single argument"
                            i.ownerFullName i.methodName, ?range=i.range) |> raise
        | _ -> None

    let convert com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "toSByte" | "toByte"
        | "toInt16" | "toUInt16"
        | "toInt32" | "toUInt32"
        | "toInt64" | "toUInt64"
            -> toInt com i i.args |> Some
        | "toSingle" | "toDouble" | "toDecimal"
            -> toFloat com i i.args |> Some
        | _ -> None

    let console com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "write" ->
            addWarning com i.fileName i.range "Write will behave as WriteLine"
            log com i |> Some
        | "writeLine" -> log com i |> Some
        | _ -> None

    let decimals com (i: Fable.ApplyInfo) =
        match i.methodName, i.args with
        | ".ctor", [Fable.Value (Fable.IdentValue _)] ->
            FableError("Passing bound values to the constructor is not supported.", ?range=i.range) |> raise
        | ".ctor", [Fable.Value (Fable.NumberConst (x, _))] ->
#if FABLE_COMPILER
            makeNumConst (float x) |> Some
#else
            makeDecConst (new decimal(x)) |> Some
#endif
        | ".ctor", [Fable.Value(Fable.ArrayConst(Fable.ArrayValues arVals, _))] ->
            match arVals with
            | [ Fable.Value (Fable.NumberConst (low, Int32));
                Fable.Value (Fable.NumberConst (medium, Int32));
                Fable.Value (Fable.NumberConst (high, Int32));
                Fable.Value (Fable.NumberConst (scale, Int32)) ] ->
#if FABLE_COMPILER
                    makeNumConst ((float ((uint64 (uint32 medium)) <<< 32 ||| (uint64 (uint32 low))))
                        / System.Math.Pow(10.0, float ((int scale) >>> 16 &&& 0xFF)) * (if scale < 0.0 then -1.0 else 1.0)) |> Some
#else
                    makeDecConst (new decimal([| int low; int medium; int high; int scale |])) |> Some
#endif
            | _ -> None
        | (".ctor" | "makeDecimal"),
              [ Fable.Value (Fable.NumberConst (low, Int32));
                Fable.Value (Fable.NumberConst (medium, Int32));
                Fable.Value (Fable.NumberConst (high, Int32));
                Fable.Value (Fable.BoolConst isNegative);
                Fable.Value (Fable.NumberConst (scale, UInt8)) ] ->
#if FABLE_COMPILER
                    makeNumConst ((float ((uint64 (uint32 medium)) <<< 32 ||| (uint64 (uint32 low))))
                        / System.Math.Pow(10.0, float scale) * (if isNegative then -1.0 else 1.0)) |> Some
#else
                    makeDecConst (new decimal(int low, int medium, int high, isNegative, byte scale)) |> Some
#endif
        | _,_ -> None

    let debug com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "write" ->
            addWarning com i.fileName i.range "Write will behave as WriteLine"
            log com i |> Some
        | "writeLine" -> log com i |> Some
        | "break" -> Fable.DebugBreak i.range |> Some
        | "assert" ->
            // emit i "if (!$0) { debugger; }" i.args |> Some
            let cond =
                Fable.Apply(Fable.Value(Fable.UnaryOp UnaryNot),
                    i.args, Fable.ApplyMeth, Fable.Boolean, i.range)
            Fable.IfThenElse(cond, Fable.DebugBreak i.range, Fable.Value Fable.Null, i.range)
            |> Some
        | _ -> None

    let regex com (i: Fable.ApplyInfo) =
        let propInt p callee = makeGet i.range i.returnType callee (makeIntConst p)
        let propStr p callee = makeGet i.range i.returnType callee (makeStrConst p)
        let isGroup =
            match i.callee with
            | Some (Type (EntFullName "System.Text.RegularExpressions.Group")) -> true
            | _ -> false
        match i.methodName with
        | ".ctor" ->
            // TODO: Use RegexConst if no options have been passed?
            CoreLibCall("RegExp", Some "create", false, i.args)
            |> makeCall i.range i.returnType |> Some
        | "options" ->
            CoreLibCall("RegExp", Some "options", false, [i.callee.Value])
            |> makeCall i.range i.returnType |> Some
        // Capture
        | "index" ->
            if not isGroup
            then propStr "index" i.callee.Value |> Some
            else FableError("Accessing index of Regex groups is not supported", ?range=i.range) |> raise
        | "value" ->
            if isGroup
            then i.callee.Value |> wrap i.returnType |> Some
            else propInt 0 i.callee.Value |> Some
        | "length" ->
            if isGroup
            then propStr "length" i.callee.Value |> Some
            else propInt 0 i.callee.Value |> propStr "length" |> Some
        // Group
        | "success" ->
            makeEqOp i.range [i.callee.Value; Fable.Value Fable.Null] BinaryUnequal |> Some
        // Match
        | "groups" -> wrap i.returnType i.callee.Value |> Some
        // MatchCollection & GroupCollection
        | "item" ->
            makeGet i.range i.returnType i.callee.Value i.args.Head |> Some
        | "count" ->
            propStr "length" i.callee.Value |> Some
        | _ -> None

    let languagePrimitives com (i: Fable.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "enumOfValue", OneArg (arg) -> arg |> Some
        | "genericHash", _ ->
            CoreLibCall("Util", Some "hash", false, i.args)
            |> makeCall i.range i.returnType |> Some
        | "genericComparison", _ ->
            CoreLibCall("Util", Some "compare", false, i.args)
            |> makeCall i.range i.returnType |> Some
        | "genericEquality", _ ->
            CoreLibCall("Util", Some "equals", false, i.args)
            |> makeCall i.range i.returnType |> Some
        | "physicalEquality", _ ->
            makeEqOp i.range i.args BinaryEqualStrict |> Some
        | _ -> None

    let intrinsicFunctions com (i: Fable.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "checkThis", (None, [arg]) -> Some arg
        | "unboxFast", OneArg (arg) -> wrap i.returnType arg |> Some
        | "unboxGeneric", OneArg (arg) -> wrap i.returnType arg |> Some
        | "makeDecimal", (_,_) -> decimals com i
        | "getString", TwoArgs (ar, idx)
        | "getArray", TwoArgs (ar, idx) ->
            makeGet i.range i.returnType ar idx |> Some
        | "setArray", ThreeArgs (ar, idx, value) ->
            Fable.Set (ar, Some idx, value, i.range) |> Some
        | ("getArraySlice" | "getStringSlice"), ThreeArgs (ar, lower, upper) ->
            let upper =
                let t = Fable.Number Int32
                match upper with
                | Null _ -> makeGet None t ar (makeStrConst "length")
                | _ -> Fable.Apply(Fable.Value(Fable.BinaryOp BinaryPlus),
                                [upper; makeIntConst 1], Fable.ApplyMeth, t, None)
            InstanceCall (ar, "slice", [lower; upper])
            |> makeCall i.range i.returnType |> Some
        | "setArraySlice", (None, args) ->
            CoreLibCall("Array", Some "setSlice", false, args)
            |> makeCall i.range i.returnType |> Some
        | "typeTestGeneric", (None, [expr]) ->
            makeTypeTest com i.range i.methodTypeArgs.Head expr |> Some
        | "createInstance", (None, _) ->
            let typRef, args = resolveTypeRef com i false i.methodTypeArgs.Head, []
            Fable.Apply (typRef, args, Fable.ApplyCons, i.returnType, i.range) |> Some
        | _ -> None

    let activator com (i: Fable.ApplyInfo) =
        match i.methodName, i.callee, i.args with
        | "createInstance", None, typRef::args ->
            Fable.Apply (typRef, args, Fable.ApplyCons, i.returnType, i.range) |> Some
        | _ -> None

    let funcs com (i: Fable.ApplyInfo) =
        match i.methodName, i.callee with
        | "invoke", Some callee ->
            Fable.Apply(callee, i.args, Fable.ApplyMeth, i.returnType, i.range) |> Some
        | _ -> None

    let options (com: ICompiler) (i: Fable.ApplyInfo) =
        // Prevent functions being run twice, see #198
        let runIfSome r expr defValue f =
            CoreLibCall("Util", Some  "defaultArg", false, [expr; defValue; f])
            |> makeCall r Fable.Any
        let toArray r arg =
            let ident = makeIdent "x"
            makeLambdaExpr [ident] (makeArray Fable.Any [Fable.IdentValue ident |> Fable.Value])
            |> runIfSome r arg (makeArray Fable.Any [])
        let getCallee() = match i.callee with Some c -> c | None -> i.args.Head
        match i.methodName with
        | "none" -> Fable.Null |> Fable.Value |> Some
        | "value" | "getValue" | "toObj" | "ofObj" | "toNullable" | "ofNullable" ->
           wrap i.returnType (getCallee()) |> Some
        | "isSome" | "isNone" ->
            let op = if i.methodName = "isSome" then BinaryUnequal else BinaryEqual
            let comp = makeEqOp i.range [getCallee(); Fable.Value Fable.Null] op
            match i.returnType with
            | Fable.Boolean _ -> Some comp
            // Hack to fix instance member calls (e.g., myOpt.IsSome)
            // For some reason, F# compiler expects it to be applicable
            | _ -> makeLambdaExpr [] comp |> Some
        | "map" | "bind" ->
            // emit i "$1 != null ? $0($1) : $1" i.args |> Some
            let f, arg = i.args.Head, i.args.Tail.Head
            runIfSome i.range arg (Fable.Value Fable.Null) f |> Some
        | "filter" ->
            let filter, arg = i.args.Head, i.args.Tail.Head
            "x => $0(x) ? x : null"
            |> makeEmit None Fable.Any [filter]
            |> runIfSome i.range arg (Fable.Value Fable.Null)
            |> Some
        | "toArray" ->
            toArray i.range i.args.Head |> Some
        | "foldBack" ->
            let opt = toArray None i.args.Tail.Head
            let args = i.args.Head::opt::i.args.Tail.Tail
            ccall com i "Seq" "foldBack" (deleg com i args) |> Some
        | meth ->
            let args =
                let args = List.rev i.args
                let opt = toArray None args.Head
                List.rev (opt::args.Tail)
            ccall com i "Seq" meth (deleg com i args) |> Some

    let timeSpans com (i: Fable.ApplyInfo) =
        // let callee = match i.callee with Some c -> c | None -> i.args.Head
        match i.methodName with
        | ".ctor" ->
            CoreLibCall("TimeSpan", Some "create", false, i.args)
            |> makeCall i.range i.returnType |> Some
        | "fromMilliseconds" ->
            wrap i.returnType i.args.Head |> Some
        | "totalMilliseconds" ->
            wrap i.returnType i.callee.Value |> Some
        | _ -> None

    let dates com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            let args =
                let last = List.last i.args
                match i.args.Length, last.Type with
                | 7, Fable.Enum "System.DateTimeKind" ->
                    (List.take 6 i.args)@[makeIntConst 0; last]
                | _ -> i.args
            CoreLibCall("Date", Some "create", false, args)
            |> makeCall i.range i.returnType |> Some
        | "toString" ->
            match i.args with
            | [Type Fable.String as format] ->
                let format = emitNoInfo "'{0:' + $0 + '}'" [format]
                CoreLibCall ("String", Some "format", false, [format;i.callee.Value])
                |> makeCall i.range i.returnType |> Some
            | _ -> toString com i i.callee.Value |> Some
        | _ -> None

    let keyValuePairs com (i: Fable.ApplyInfo) =
        let get (k: int) =
            makeGet i.range i.returnType i.callee.Value (makeIntConst k) |> Some
        match i.methodName with
        | ".ctor" -> Fable.Value(Fable.TupleConst i.args) |> Some
        | "key" -> get 0
        | "value" -> get 1
        | _ -> None

    let dictionaries (com: ICompiler) (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            match i.calleeTypeArgs.Head with
            | DeclaredKind(Fable.Record _) | DeclaredKind(Fable.Union _) ->
                "Structural equality is not supported for Dictionary keys, please use F# Map"
                |> addWarning com i.fileName i.range
            | _ -> ()
            let makeMap args =
                GlobalCall("Map", None, true, args) |> makeCall i.range i.returnType
            match i.args with
            | [] -> makeMap [] |> Some
            | _ ->
                match i.args.Head.Type with
                | Fable.Number Int32 -> makeMap [] |> Some
                | _ -> makeMap i.args |> Some
        | "isReadOnly" ->
            Fable.BoolConst false |> Fable.Value |> Some
        | "count" ->
            makeGet i.range i.returnType i.callee.Value (makeStrConst "size") |> Some
        | "containsValue" ->
            CoreLibCall ("Map", Some "containsValue", false, [i.args.Head; i.callee.Value])
            |> makeCall i.range i.returnType |> Some
        | "item" -> icall com i (if i.args.Length = 1 then "get" else "set") |> Some
        | "keys" -> icall com i "keys" |> Some
        | "values" -> icall com i "values" |> Some
        | "containsKey" -> icall com i "has" |> Some
        | "clear" -> icall com i "clear" |> Some
        | "add" -> icall com i "set" |> Some
        | "remove" -> icall com i "delete" |> Some
        | "tryGetValue" ->
            match i.callee, i.args with
            | Some dic, [key; defVal] ->
                CoreLibCall ("Map", Some "tryGetValue", false, [dic; key; defVal])
                |> makeCall i.range i.returnType |> Some
            | _ -> None
        | _ -> None

    let hashSets (com: ICompiler) (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            match i.calleeTypeArgs.Head with
            | DeclaredKind(Fable.Record _) | DeclaredKind(Fable.Union _) ->
                "Structural equality is not supported for HashSet, please use F# Set"
                |> addWarning com i.fileName i.range
            | _ -> ()
            let makeSet args =
                GlobalCall("Set", None, true, args) |> makeCall i.range i.returnType
            match i.args with
            | [] -> makeSet [] |> Some
            | _ ->
                match i.args.Head.Type with
                | Fable.Number Int32 -> makeSet [] |> Some
                | _ -> makeSet i.args |> Some
        | "count" ->
            makeGet i.range i.returnType i.callee.Value (makeStrConst "size") |> Some
        | "isReadOnly" ->
            Fable.BoolConst false |> Fable.Value |> Some
        | "clear" -> icall com i "clear" |> Some
        | "contains" -> icall com i "has" |> Some
        | "remove" -> icall com i "delete" |> Some
        | "isProperSubsetOf" | "isProperSupersetOf"
        | "add" ->
            CoreLibCall ("Set", Some "addInPlace", false, [i.args.Head;i.callee.Value])
            |> makeCall i.range i.returnType |> Some
        | "unionWith" | "intersectWith" | "exceptWith"
        | "isSubsetOf" | "isSupersetOf" | "copyTo" ->
            let meth =
                let m = match i.methodName with "exceptWith" -> "differenceWith" | m -> m
                m.Replace("With", "InPlace")
            CoreLibCall ("Set", Some meth, false, i.callee.Value::i.args)
            |> makeCall i.range i.returnType |> Some
        // TODO
        // | "setEquals"
        // | "overlaps"
        // | "symmetricExceptWith"
        | _ -> None

    let mapAndSets com (i: Fable.ApplyInfo) =
        let instanceArgs () =
            match i.callee with
            | Some c -> c, i.args
            | None -> List.last i.args, List.take (i.args.Length-1) i.args
        let prop (prop: string) =
            let callee, _ = instanceArgs()
            makeGet i.range i.returnType callee (makeStrConst prop)
        let icall meth =
            let callee, args = instanceArgs()
            InstanceCall (callee, meth, args)
            |> makeCall i.range i.returnType
        let modName =
            if i.ownerFullName.Contains("Map")
            then "Map" else "Set"
        match i.methodName with
        // Instance and static shared methods
        | "count" -> prop "size" |> Some
        | "contains" | "containsKey" -> icall "has" |> Some
        | "add" | "remove" | "isEmpty"
        | "find" | "tryFind" // Map-only
        | "maximumElement" | "minimumElement"
        | "maxElement" | "minElement" -> // Set-only
            let args =
                match i.methodName with
                | "isEmpty" | "maximumElement" | "minimumElement"
                | "maxElement" | "minElement" -> staticArgs i.callee i.args
                | _ -> i.args @ (Option.toList i.callee)
            CoreLibCall(modName, Some i.methodName, false, args)
            |> makeCall i.range i.returnType |> Some
        // Map indexer
        | "item" -> icall "get" |> Some
        // Constructors
        | "empty" -> makeMapOrSetCons com i modName [] |> Some
        | ".ctor" -> makeMapOrSetCons com i modName i.args |> Some
        // Conversions
        | "toArray" -> toArray com i i.args.Head |> Some
        | "toList" -> toList com i i.args.Head |> Some
        | "toSeq" -> Some i.args.Head
        | "ofArray" -> makeMapOrSetCons com i modName i.args |> Some
        | "ofList" | "ofSeq" -> makeMapOrSetCons com i modName i.args |> Some
        // Static methods
        | "exists" | "fold" | "foldBack" | "forAll" | "iterate"
        | "filter" | "map" | "partition"
        | "findKey" | "tryFindKey" | "pick" | "tryPick" -> // Map-only
            CoreLibCall(modName, Some i.methodName, false, deleg com i i.args)
            |> makeCall i.range i.returnType |> Some
        // Set only static methods
        | "singleton" ->
            [makeArray Fable.Any i.args]
            |> makeMapOrSetCons com i modName |> Some
        | _ -> None

    type CollectionKind =
        | Seq | List | Array

    // Functions which don't return a new collection of the same type
    let implementedSeqNonBuildFunctions =
        set [ "average"; "averageBy"; "compareWith"; "empty";
              "exactlyOne"; "exists"; "exists2"; "fold"; "fold2"; "foldBack"; "foldBack2";
              "forAll"; "forAll2"; "head"; "tryHead"; "item"; "tryItem";
              "iterate"; "iterateIndexed"; "iterate2"; "iterateIndexed2";
              "isEmpty"; "last"; "tryLast"; "length";
              "mapFold"; "mapFoldBack"; "max"; "maxBy"; "min"; "minBy";
              "reduce"; "reduceBack"; "sum"; "sumBy"; "tail"; "toList";
              "tryFind"; "find"; "tryFindIndex"; "findIndex"; "tryPick"; "pick";
              "tryFindBack"; "findBack"; "tryFindIndexBack"; "findIndexBack" ]

    // Functions that must return a collection of the same type
    let implementedSeqBuildFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "countBy"; "distinct"; "distinctBy";
              "except"; "filter"; "where"; "groupBy"; "initialize";
              "map"; "mapIndexed"; "map2"; "mapIndexed2"; "map3";
              "ofArray"; "pairwise"; "permute"; "replicate"; "reverse";
              "scan"; "scanBack"; "singleton"; "skip"; "skipWhile";
              "take"; "takeWhile"; "sortWith"; "unfold"; "zip"; "zip3" ]

    /// Seq functions implemented in other modules to prevent cyclic dependencies
    let seqFunctionsImplementedOutside =
        [ "Map", [ "groupBy"; "countBy" ]
        ; "Set", [ "distinct"; "distinctBy" ] ] |> Map

    let implementedListFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "filter"; "groupBy"; "where";
              "initialize"; "map"; "mapIndexed"; "ofArray"; "partition";
              "replicate"; "reverse"; "singleton"; "unzip"; "unzip3" ]

    let implementedArrayFunctions =
        set [ "copyTo"; "partition"; "permute"; "sortInPlaceBy"; "unzip"; "unzip3" ]

    let nativeArrayFunctions =
        dict [ "exists" => "some"; "filter" => "filter";
               "find" => "find"; "findIndex" => "findIndex"; "forAll" => "every";
               "iterate" => "forEach";
               "reduce" => "reduce"; "reduceBack" => "reduceRight";
               "sortInPlace" => "sort"; "sortInPlaceWith" => "sort" ]

    let collectionsSecondPass com (i: Fable.ApplyInfo) kind =
        let prop (meth: string) callee =
            makeGet i.range i.returnType callee (makeStrConst meth)
        let icall meth (callee, args) =
            InstanceCall (callee, meth, args)
            |> makeCall i.range i.returnType
        let ccall modName meth args =
            CoreLibCall (modName, Some meth, false, args)
            |> makeCall i.range i.returnType
        let meth, c, args =
            i.methodName, i.callee, i.args
        match meth with
        // Deal with special cases first
        | "cast" -> Some i.args.Head // Seq only, erase
        | "isEmpty" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array ->
                makeEqOp i.range [prop "length" args.Head; makeIntConst 0] BinaryEqualStrict
            | List ->
                let c, _ = instanceArgs c args
                makeEqOp i.range [prop "tail" c; Fable.Value Fable.Null] BinaryEqual
            |> Some
        | "head" | "tail" | "length" | "count" ->
            let meth = if meth = "count" then "length" else meth
            match kind with
            | Seq ->
                // A static 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
                let seqMeth = if meth = "length" then "count" else meth
                ccall "Seq" seqMeth (staticArgs c args)
            | List -> let c, _ = instanceArgs c args in prop meth c
            | Array ->
                let c, _ = instanceArgs c args
                if meth = "head" then makeGet i.range i.returnType c (makeIntConst 0)
                elif meth = "tail" then icall "slice" (c, [makeIntConst 1])
                else prop "length" c
            |> Some
        | "item" ->
            match i.callee, kind with
            | Some callee, Array ->
                if i.args.Length = 1
                then makeGet i.range i.returnType callee i.args.Head
                else Fable.Set (i.callee.Value, Some i.args.Head, i.args.Tail.Head, i.range)
            | _, Seq -> ccall "Seq" meth args
            | _, Array -> makeGet i.range i.returnType args.Tail.Head args.Head
            | _, List -> match i.callee with Some x -> i.args@[x] | None -> i.args
                         |> ccall "Seq" meth
            |> Some
        | "sort" | "sortDescending" | "sortBy" | "sortByDescending" ->
            let proyector, args =
                if meth = "sortBy" || meth = "sortByDescending"
                then Some args.Head, args.Tail
                else None, args
            let compareFn =
                let fnArgs = [makeIdent "x"; makeIdent "y"]
                let identValue x =
                    let x = Fable.Value(Fable.IdentValue x)
                    match proyector with
                    | Some proyector -> Fable.Apply(proyector, [x], Fable.ApplyMeth, Fable.Any, None)
                    | None -> x
                let comparison =
                    let comparison = compare com None (List.map identValue fnArgs) None
                    if meth = "sortDescending" || meth = "sortByDescending"
                    then makeUnOp None (Fable.Number Int32) [comparison] UnaryMinus
                    else comparison
                makeLambdaExpr fnArgs comparison
            match c, kind with
            // This is for calls to instance `Sort` member on ResizeArrays
            | Some c, _ ->
                match args with
                | [] -> icall "sort" (c, [compareFn]) |> Some
                | [Type (Fable.Function _)] -> icall "sort" (c, deleg com i args) |> Some
                | _ -> None
            | None, Seq -> ccall "Seq" "sortWith" (compareFn::args) |> Some
            | None, List -> ccall "Seq" "sortWith" (compareFn::args) |> toList com i |> Some
            | None, Array -> ccall "Seq" "sortWith" (compareFn::args) |> toArray com i |> Some
        // Constructors ('cons' only applies to List)
        | "empty" | "cons" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array ->
                match i.returnType with
                | Fable.Array typ ->
                    Fable.ArrayConst (Fable.ArrayAlloc (makeIntConst 0), typ) |> Fable.Value
                | _ -> "Expecting array type but got " + i.returnType.FullName
                       |> attachRange i.range |> failwith
            | List -> CoreLibCall ("List", None, true, args)
                      |> makeCall i.range i.returnType
            |> Some
        | "zeroCreate" ->
            match genArg i.returnType with
            | Fable.Number _ as t ->
                Fable.ArrayConst(Fable.ArrayAlloc i.args.Head, t)
                |> Fable.Value |> Some
            | Fable.Boolean -> emit i "new Array($0).fill(false)" i.args |> Some
            // If we don't fill the array with null values some operations
            // may behave unexpectedly, like Array.prototype.reduce
            | t -> emit i "new Array($0).fill(null)" i.args |> Some
        | "create" ->
            ccall "Seq" "replicate" args
            |> toArray com i |> Some
        // ResizeArray only
        | ".ctor" ->
            let makeJsArray arVals =
                // Use Any to prevent creation of typed arrays
                let ar = Fable.Value(Fable.ArrayConst(Fable.ArrayValues arVals, Fable.Any))
                Fable.Wrapped(ar, i.returnType) |> Some
            match i.args with
            | [] -> makeJsArray []
            | _ ->
                match i.args.Head with
                | Type(Fable.Number Int32) -> makeJsArray []
                | Fable.Value(Fable.ArrayConst(Fable.ArrayValues arVals, _)) -> makeJsArray arVals
                | _ -> emit i "Array.from($0)" i.args |> Some
        | "find" when Option.isSome c ->
            let defaultValue = defaultof i.calleeTypeArgs.Head
            ccall "Seq" "tryFind" [args.Head;c.Value;defaultValue] |> Some
        | "findAll" when Option.isSome c ->
            ccall "Seq" "filter" [args.Head;c.Value] |> toArray com i |> Some
        | "findLast" when Option.isSome c ->
            let defaultValue = defaultof i.calleeTypeArgs.Head
            ccall "Seq" "tryFindBack" [args.Head;c.Value;defaultValue] |> Some
        | "add" ->
            icall "push" (c.Value, args) |> Some
        | "addRange" ->
            ccall "Array" "addRangeInPlace" [args.Head; c.Value] |> Some
        | "clear" ->
            icall "splice" (c.Value, [makeIntConst 0]) |> Some
        | "contains" ->
            match c, args with
            | Some c, args ->
                emit i "$0.indexOf($1) > -1" (c::args) |> Some
            | None, [item;xs] ->
                let f =
                    wrapInLambda [makeIdent "x"] (fun exprs ->
                        CoreLibCall("Util", Some "equals", false, item::exprs)
                        |> makeCall None Fable.Boolean)
                ccall "Seq" "exists" [f;xs] |> Some
            | _ -> None
        | "indexOf" ->
            icall "indexOf" (c.Value, args) |> Some
        | "insert" ->
            icall "splice" (c.Value, [args.Head; makeIntConst 0; args.Tail.Head]) |> Some
        | "remove" ->
            ccall "Array" "removeInPlace" [args.Head; c.Value] |> Some
        | "removeAt" ->
            icall "splice" (c.Value, [args.Head; makeIntConst 1]) |> Some
        | "reverse" when kind = Array ->
            match i.returnType with
            | Fable.Array _ ->
                // Arrays need to be copied before sorted in place.
                emit i "$0.slice().reverse()" i.args |> Some
            | _ ->
                // ResizeArray should be sorted in place without copying.
                icall "reverse" (instanceArgs c i.args) |> Some
        // Conversions
        | "toSeq" | "ofSeq" ->
            match kind with
            | Seq -> "Unexpected method called on seq: " + meth
                     |> attachRange i.range |> failwith
            | List -> ccall "Seq" (if meth = "toSeq" then "ofList" else "toList") args
            | Array ->
                if meth = "toSeq"
                then ccall "Seq" "ofArray" args
                else toArray com i args.Head
            |> Some
        | "toArray" ->
            match c with
            | Some c -> toArray com i c
            | None -> toArray com i i.args.Head
            |> Some
        | "ofList" ->
            match kind with
            | List -> "Unexpected method called on list: " + meth
                      |> attachRange i.range |> failwith
            | Seq -> ccall "Seq" "ofList" args
            | Array -> toArray com i i.args.Head
            |> Some
        | "sum" | "sumBy" ->
            match meth, i.methodTypeArgs with
            | "sum", [Fable.DeclaredType(ent, _) as t]
            | "sumBy", [_;Fable.DeclaredType(ent, _) as t] ->
                let zero = Fable.Apply(Fable.Value(Fable.TypeRef(ent,[])), [makeStrConst "Zero"],
                                        Fable.ApplyGet, i.returnType, None)
                let fargs = [makeTypedIdent "x" t; makeTypedIdent "y" t]
                let addFn = wrapInLambda fargs (fun args -> applyOp com i args "op_Addition")
                if meth = "sum"
                then addFn, args.Head
                else emitNoInfo "((f,add)=>(x,y)=>add(x,f(y)))($0,$1)" [args.Head;addFn], args.Tail.Head
                |> fun (f, xs) -> ccall "Seq" "fold" [f; zero; xs] |> Some
            | _ ->
                ccall "Seq" meth (deleg com i args) |> Some
        | "min" | "minBy" | "max" | "maxBy" ->
            let reduce macro macroArgs xs =
                ccall "Seq" "reduce" [emitNoInfo macro macroArgs; xs] |> Some
            match meth, i.methodTypeArgs with
            // Optimization for numeric types
            | "min", [Fable.Number _] ->
                // Note: if we use just Math.min it fails with typed arrays
                reduce "(x,y) => Math.min(x,y)" [] args.Head
            | "max", [Fable.Number _] ->
                reduce "(x,y) => Math.max(x,y)" [] args.Head
            | "minBy", [_;Fable.Number _] ->
                reduce "(f=>(x,y)=>f(x)<f(y)?x:y)($0)" [args.Head] args.Tail.Head
            | "maxBy", [_;Fable.Number _] ->
                reduce "(f=>(x,y)=>f(x)>f(y)?x:y)($0)" [args.Head] args.Tail.Head
            | _ -> ccall "Seq" meth (deleg com i args) |> Some
        // Default to Seq implementation in core lib
        | Patterns.SetContains implementedSeqNonBuildFunctions meth ->
            ccall "Seq" meth (deleg com i args) |> Some
        | Patterns.SetContains implementedSeqBuildFunctions meth ->
            let mod_ =
                seqFunctionsImplementedOutside
                |> Map.tryFindKey (fun _ v -> List.contains meth v)
                |> defaultArg <| "Seq"
            match kind with
            | Seq -> ccall mod_ meth (deleg com i args)
            | List -> ccall mod_ meth (deleg com i args) |> toList com i
            | Array -> ccall mod_ meth (deleg com i args) |> toArray com i
            |> Some
        | _ -> None

    let collectionsFirstPass com (i: Fable.ApplyInfo) kind =
        let icall meth (callee, args) =
            InstanceCall (callee, meth, args)
            |> makeCall i.range i.returnType
            |> Some
        match kind with
        | List ->
            let listMeth meth args =
                CoreLibCall ("List", Some meth, false, deleg com i args)
                |> makeCall i.range i.returnType |> Some
            match i.methodName with
            | "getSlice" ->
                listMeth "slice" (i.args@[i.callee.Value])
            | "truncate" ->
                listMeth "slice" ([makeIntConst 0]@i.args)
            | Patterns.SetContains implementedListFunctions meth ->
                listMeth meth i.args
            | _ -> None
        | Array ->
            match i.methodName with
            | "get" ->
                match i.callee, i.args with
                | TwoArgs (ar, idx) ->
                    makeGet i.range i.returnType ar idx |> Some
                | _ -> None
            | "set" ->
                match i.callee, i.args with
                | ThreeArgs (ar, idx, value) ->
                    Fable.Set (ar, Some idx, value, i.range) |> Some
                | _ -> None
            | "take" -> icall "slice" (i.args.Tail.Head, [makeIntConst 0; i.args.Head])
            | "skip" -> icall "slice" (i.args.Tail.Head, [i.args.Head])
            | "copy" -> icall "slice" (i.args.Head, [])
            | "getSubArray" | "fill" ->
                ccall com i "Array" i.methodName i.args |> Some
            | "truncate" ->
                // Array.truncate count array
                emit i "$1.slice(0, $0)" i.args |> Some
            | "map" ->
                match i.methodTypeArgs with
                // Native JS map is risky with typed arrays as they coerce
                // the final result (see #120, #171)
                | Fable.Any::_ | Fable.GenericParam _::_ -> None
                | (Number _ as tin)::[tout] when tin = tout ->
                    icall "map" (i.args.[1], deleg com i [i.args.[0]])
                | (ExtNumber _|NoNumber)::[ExtNumber _|NoNumber] ->
                    icall "map" (i.args.[1], deleg com i [i.args.[0]])
                | _ -> None
            | "append" ->
                match i.methodTypeArgs with
                | [Fable.Any] | [Number _] -> None
                | _ -> icall "concat" (i.args.Head, i.args.Tail)
            | "indexed" ->
                emit i "$0.map((x, y) => [y, x])" i.args |> Some
            | Patterns.SetContains implementedArrayFunctions meth ->
                CoreLibCall ("Array", Some meth, false, deleg com i i.args)
                |> makeCall i.range i.returnType |> Some
            | Patterns.DicContains nativeArrayFunctions meth ->
                let revArgs = List.rev i.args
                icall meth (revArgs.Head, deleg com i (List.rev revArgs.Tail))
            | _ -> None
        | _ -> None
        |> function None -> collectionsSecondPass com i kind | someExpr -> someExpr

    let exceptions com (i: Fable.ApplyInfo) =
        match i.methodName, i.callee with
        // TODO: Check argument number and types?
        | ".ctor", _ ->
            Fable.Apply(makeIdentExpr "Error", i.args, Fable.ApplyCons, i.returnType, i.range) |> Some
        | "message", Some e -> getProp i.range i.returnType e "message" |> Some
        | "stackTrace", Some e -> getProp i.range i.returnType e "stack" |> Some
        | _ -> None

    let cancels com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            match i.args with
            | [Type (Fable.Number _) as arg] ->
                "(function(){var token={};setTimeout(function(){token.isCancelled=true},$0); return token;}())"
                |> emit i <| [arg]
            | [Type (Fable.Boolean _) as arg] ->
                emit i "{ isCancelled = $0 }" [arg]
            | _ -> Fable.ObjExpr ([], [], None, i.range)
            |> Some
        | "token" -> i.callee
        | "cancel" -> emit i "$0.isCancelled = true" [i.callee.Value] |> Some
        | "cancelAfter" -> emit i "setTimeout(function () { $0.isCancelled = true }, $1)" [i.callee.Value; i.args.Head] |> Some
        | "isCancellationRequested" -> emit i "$0.isCancelled" [i.callee.Value] |> Some
        // TODO: Add check so CancellationTokenSource cannot be cancelled after disposed?
        | "dispose" -> Fable.Null |> Fable.Value |> Some
        | _ -> None

    let objects com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "getHashCode" -> i.callee
        | ".ctor" -> Fable.ObjExpr ([], [], None, i.range) |> Some
        | "referenceEquals" -> makeEqOp i.range i.args BinaryEqualStrict |> Some
        | "toString" -> toString com i i.callee.Value |> Some
        | "equals" -> staticArgs i.callee i.args |> equals true com i
        | "getType" ->
            match i.callee.Value.Type with
            | Fable.Any | Fable.GenericParam _ ->
                sprintf "%s %s"
                    "Cannot resolve .GetType() at compile time."
                    "The type created at runtime won't contain generic information."
                |> addWarning com i.fileName i.range
                CoreLibCall("Reflection", Some "getType", false, [i.callee.Value])
                |> makeCall i.range i.returnType |> Some
            | t ->
                let genInfo = {makeGeneric=true; genericAvailability=false}
                makeTypeRef com genInfo t |> Some
        | _ -> None

    let types com (info: Fable.ApplyInfo) =
        let str x = Fable.Value(Fable.StringConst x)
        match info.callee with
        | Some(Fable.Value(Fable.TypeRef(ent,_))) ->
            match info.methodName with
            | "namespace" -> str ent.Namespace |> Some
            | "fullName" -> str ent.FullName |> Some
            | "name" -> str ent.Name |> Some
            | "isGenericType" -> ent.GenericParameters.Length > 0 |> makeBoolConst |> Some
            | "getGenericTypeDefinition" -> makeTypeRefFrom com ent |> Some
            | _ -> None
        | _ ->
            let getTypeFullName args =
                args |> Option.map (fun args ->
                CoreLibCall("Reflection", Some "getTypeFullName", false, args)
                |> makeCall info.range info.returnType)
            match info.methodName with
            | "fullName" -> Some [info.callee.Value] |> getTypeFullName
            | "name" -> Some [info.callee.Value; str "name"] |> getTypeFullName
            | "namespace" -> Some [info.callee.Value; str "namespace"] |> getTypeFullName
            | "isGenericType" ->
                CoreLibCall("Util", Some "isGeneric", false, [info.callee.Value])
                |> makeCall info.range info.returnType |> Some
            | "getGenericTypeDefinition" ->
                CoreLibCall("Util", Some "getDefinition", false, [info.callee.Value])
                |> makeCall info.range info.returnType |> Some
            | _ -> None
    let unchecked com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "defaultOf" ->
            defaultof info.methodTypeArgs.Head |> Some
        | "hash" ->
            CoreLibCall("Util", Some "hash", false, info.args)
            |> makeCall info.range info.returnType |> Some
        | "equals" ->
            CoreLibCall("Util", Some "equals", false, info.args)
            |> makeCall info.range info.returnType |> Some
        | "compare" ->
            CoreLibCall("Util", Some "compare", false, info.args)
            |> makeCall info.range info.returnType |> Some
        | _ -> None

    let random com (info: Fable.ApplyInfo) =
        match info.methodName with
        | ".ctor" ->
            let o = Fable.ObjExpr ([], [], None, info.range)
            Fable.Wrapped (o, info.returnType) |> Some
        | "next" ->
            let min, max =
                match info.args with
                | [] -> makeIntConst 0, makeIntConst System.Int32.MaxValue
                | [max] -> makeIntConst 0, max
                | [min; max] -> min, max
                | _ -> failwith "Unexpected arg count for Random.Next"
            ccall com info "Util" "randomNext" [min; max] |> Some
        | _ -> None

    let enumerable com (info: Fable.ApplyInfo) =
        match info.callee, info.methodName with
        | Some callee, "getEnumerator" ->
            ccall com info "Seq" "getEnumerator" [callee] |> Some
        | _ -> None

    let mailbox com (info: Fable.ApplyInfo) =
        match info.callee with
        | None ->
            match info.methodName with
            | ".ctor" -> CoreLibCall("MailboxProcessor", None, true, info.args) |> Some
            | "start" -> CoreLibCall("MailboxProcessor", Some "start", false, info.args) |> Some
            | _ -> None
            |> Option.map (makeCall info.range info.returnType)
        | Some callee ->
            match info.methodName with
            // `reply` belongs to AsyncReplyChannel
            | "start" | "receive" | "postAndAsyncReply" | "post" | "reply" ->
                InstanceCall(callee, info.methodName, info.args)
                |> makeCall info.range info.returnType |> Some
            | _ -> None

    let guids com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "newGuid" ->
            CoreLibCall("String", Some "newGuid", false, [])
            |> makeCall info.range info.returnType |> Some
        | "parse" -> info.args.Head |> Some
        | "tryParse" ->
            Fable.TupleConst [makeBoolConst true; info.args.Head]
            |> Fable.Value |> Some
        | _ -> None

    let laziness com (info: Fable.ApplyInfo) =
        let coreCall meth isCons args =
            CoreLibCall("Lazy", meth, isCons, args)
            |> makeCall info.range info.returnType
        match info.methodName with
        | ".ctor" | "create" -> coreCall None true info.args |> Some
        | "createFromValue" -> coreCall (Some info.methodName) false info.args |> Some
        | "force" | "value" | "isValueCreated" ->
            let callee, _ = instanceArgs info.callee info.args
            match info.methodName with
            | "force" -> "value" | another -> another
            |> getProp info.range info.returnType callee |> Some
        | _ -> None

    let controlExtensions com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "addToObservable" -> Some "add"
        | "subscribeToObservable" -> Some "subscribe"
        | _ -> None
        |> Option.map (fun meth ->
            let args = staticArgs info.callee info.args |> List.rev
            CoreLibCall("Observable", Some meth, false, args)
            |> makeCall info.range info.returnType)

    let asyncs com (info: Fable.ApplyInfo) =
        let asyncMeth meth args =
            CoreLibCall("Async", Some meth, false, args)
            |> makeCall info.range info.returnType |> Some
        match info.methodName with
        | "start" ->
            // Just add warning, the replacement will actually happen in coreLibPass
            "Async.Start will behave as StartImmediate"
            |> addWarning com info.fileName info.range
            None
        | "cancellationToken" ->
            // Make sure cancellationToken is called as a function and not a getter
            asyncMeth "cancellationToken" []
        | "catch" ->
            // `catch` cannot be used as a function name in JS
            asyncMeth "catchAsync" info.args
        | _ -> None

    let bigint com (i: Fable.ApplyInfo) =
        match i.callee, i.methodName with
        | Some callee, meth -> icall com i meth |> Some
        | None, ".ctor" ->
            match i.args with
            | [Type (Fable.ExtendedNumber (Int64|UInt64))] -> ccall com i "BigInt" "fromInt64" i.args
            | [_] -> ccall com i "BigInt" "fromInt32" i.args
            | _ ->
                CoreLibCall("BigInt", None, true, i.args)
                |> makeCall i.range i.returnType
            |> Some
        | None, ("zero"|"one"|"two") ->
            makeCoreRef "BigInt" (Some i.methodName) |> Some
        | None, ("fromZero"|"fromOne") ->
            let fi = if i.methodName = "fromZero" then "zero" else "one"
            makeCoreRef "BigInt" (Some fi) |> Some
        | None, "fromString" ->
            ccall com i "BigInt" "parse" i.args |> Some
        | None, meth -> ccall com i "BigInt" meth i.args |> Some

    let tryReplace com (info: Fable.ApplyInfo) =
        match info.ownerFullName with
        | Naming.StartsWith fableCore _ -> fableCoreLib com info
        | Naming.EndsWith "Exception" _ -> exceptions com info
        | "System.Object" -> objects com info
        | "System.Timers.ElapsedEventArgs" -> info.callee // only signalTime is available here
        | "System.String"
        | "Microsoft.FSharp.Core.StringModule" -> strings com info
        | "Microsoft.FSharp.Core.PrintfModule"
        | "Microsoft.FSharp.Core.PrintfFormat" -> fsFormat com info
        | "System.BitConverter" -> bitConvert com info
        | "System.Int32" -> parse com info false
        | "System.Single" | "System.Double" -> parse com info true
        | "System.Convert" -> convert com info
        | "System.Console" -> console com info
        | "System.Decimal" -> decimals com info
        | "System.Diagnostics.Debug"
        | "System.Diagnostics.Debugger" -> debug com info
        | "System.DateTime" -> dates com info
        | "System.TimeSpan" -> timeSpans com info
        | "System.Action" | "System.Func"
        | "Microsoft.FSharp.Core.FSharpFunc" -> funcs com info
        | "System.Random" -> random com info
        | "Microsoft.FSharp.Core.FSharpOption"
        | "Microsoft.FSharp.Core.OptionModule" -> options com info
        | "System.Threading.CancellationToken"
        | "System.Threading.CancellationTokenSource" -> cancels com info
        | "System.Math"
        | "Microsoft.FSharp.Core.Operators"
        | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
        | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com info
        | "Microsoft.FSharp.Core.FSharpRef" -> references com info
        | "System.Activator" -> activator com info
        | "Microsoft.FSharp.Core.LanguagePrimitives" -> languagePrimitives com info
        | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
        | "Microsoft.FSharp.Core.Operators.OperatorIntrinsics" -> intrinsicFunctions com info
        | "System.Text.RegularExpressions.Capture"
        | "System.Text.RegularExpressions.Match"
        | "System.Text.RegularExpressions.Group"
        | "System.Text.RegularExpressions.MatchCollection"
        | "System.Text.RegularExpressions.GroupCollection"
        | "System.Text.RegularExpressions.Regex" -> regex com info
        | "System.Collections.Generic.IEnumerable"
        | "System.Collections.IEnumerable" -> enumerable com info
        | "System.Collections.Generic.Dictionary"
        | "System.Collections.Generic.IDictionary" -> dictionaries com info
        | "System.Collections.Generic.HashSet"
        | "System.Collections.Generic.ISet" -> hashSets com info
        | "System.Collections.Generic.KeyValuePair" -> keyValuePairs com info
        | "System.Collections.Generic.Dictionary.KeyCollection"
        | "System.Collections.Generic.Dictionary.ValueCollection"
        | "System.Collections.Generic.ICollection" -> collectionsSecondPass com info Seq
        | "System.Array"
        | "System.Collections.Generic.List"
        | "System.Collections.Generic.IList" -> collectionsSecondPass com info Array
        | "Microsoft.FSharp.Collections.ArrayModule" -> collectionsFirstPass com info Array
        | "Microsoft.FSharp.Collections.FSharpList"
        | "Microsoft.FSharp.Collections.ListModule" -> collectionsFirstPass com info List
        | "Microsoft.FSharp.Collections.SeqModule" -> collectionsSecondPass com info Seq
        | "Microsoft.FSharp.Collections.FSharpMap"
        | "Microsoft.FSharp.Collections.MapModule"
        | "Microsoft.FSharp.Collections.FSharpSet"
        | "Microsoft.FSharp.Collections.SetModule" -> mapAndSets com info
        // For some reason `typeof<'T>.Name` translates to `System.Reflection.MemberInfo.name`
        // and not `System.Type.name` (as with `typeof<'T>.FullName` and `typeof<'T>.Namespace`)
        | "System.Reflection.MemberInfo"
        | "System.Type" -> types com info
        | "Microsoft.FSharp.Core.Operators.Unchecked" -> unchecked com info
        | "Microsoft.FSharp.Control.FSharpMailboxProcessor"
        | "Microsoft.FSharp.Control.FSharpAsyncReplyChannel" -> mailbox com info
        | "Microsoft.FSharp.Control.FSharpAsync" -> asyncs com info
        | "System.Guid" -> guids com info
        | "System.Lazy" | "Microsoft.FSharp.Control.Lazy"
        | "Microsoft.FSharp.Control.LazyExtensions" -> laziness com info
        | "Microsoft.FSharp.Control.CommonExtensions" -> controlExtensions com info
        | "System.Numerics.BigInteger"
        | "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI" -> bigint com info
        | _ -> None

module CoreLibPass =
    open Util

    /// Module methods in the core lib can be bound Static or Both (instance and static).
    /// If they're bound only statically all methods will be called statically: if there's an
    /// instance, it'll be passed as the first argument and constructors will change to `create`.
    type MapKind = Static | Both

    // Attention! If a type is added here, it will likely need
    // to be added to `tryReplaceEntity` below too.
    let mappings =
        dict [
            system + "DateTime" => ("Date", Static)
            system + "TimeSpan" => ("TimeSpan", Static)
            system + "Timers.Timer" => ("Timer", Both)
            fsharp + "Control.FSharpAsync" => ("Async", Static)
            fsharp + "Control.FSharpAsyncBuilder" => ("AsyncBuilder", Both)
            fsharp + "Control.ObservableModule" => ("Observable", Static)
            fsharp + "Core.CompilerServices.RuntimeHelpers" => ("Seq", Static)
            system + "String" => ("String", Static)
            fsharp + "Core.StringModule" => ("String", Static)
            system + "Text.RegularExpressions.Regex" => ("RegExp", Static)
            fsharp + "Collections.SeqModule" => ("Seq", Static)
            fsharp + "Collections.FSharpSet" => ("Set", Static)
            fsharp + "Collections.SetModule" => ("Set", Static)
            fsharp + "Core.FSharpChoice" => ("Choice", Both)
            fsharp + "Control.FSharpEvent" => ("Event", Both)
            fsharp + "Control.EventModule" => ("Event", Static)
        ]

open Util

let private coreLibPass com (info: Fable.ApplyInfo) =
    match info.ownerFullName with
    | Patterns.DicContains CoreLibPass.mappings (modName, kind) ->
        match kind with
        | CoreLibPass.Both ->
            match info.methodName, info.methodKind, info.callee with
            | ".ctor", _, None | _, Fable.Constructor, None ->
                CoreLibCall(modName, None, true, deleg com info info.args)
                |> makeCall info.range info.returnType |> Some
            | _, Fable.Getter _, Some callee ->
                let prop = Naming.upperFirst info.methodName |> makeStrConst
                Fable.Apply(callee, [prop], Fable.ApplyGet, info.returnType, info.range) |> Some
            | _, Fable.Setter _, Some callee ->
                let prop = Naming.upperFirst info.methodName |> makeStrConst
                Fable.Set(callee, Some prop, info.args.Head, info.range) |> Some
            | _, _, Some callee ->
                InstanceCall (callee, Naming.upperFirst info.methodName, deleg com info info.args)
                |> makeCall info.range info.returnType |> Some
            | _, _, None ->
                CoreLibCall(modName, Some info.methodName, false, staticArgs info.callee info.args |> deleg com info)
                |> makeCall info.range info.returnType |> Some
        | CoreLibPass.Static ->
            let meth =
                if info.methodName = ".ctor" then "create" else info.methodName
            CoreLibCall(modName, Some meth, false, staticArgs info.callee info.args |> deleg com info)
            |> makeCall info.range info.returnType |> Some
    | _ -> None

let tryReplace (com: ICompiler) (info: Fable.ApplyInfo) =
    let info =
        info.methodName |> Naming.removeGetSetPrefix |> Naming.lowerFirst
        |> fun methName -> { info with methodName = methName }
    match AstPass.tryReplace com info with
    | Some _ as res -> res
    | None -> coreLibPass com info

// TODO: We'll probably have to merge this with CoreLibPass.mappings
// Especially if we start making more types from the BCL compatible
let tryReplaceEntity (com: ICompiler) (ent: Fable.Entity) (genArgs: (string*Fable.Expr) list) =
    let makeGeneric genArgs expr =
        match genArgs with
        | [] -> expr
        | genArgs ->
            let genArgs = makeJsObject None genArgs
            CoreLibCall("Util", Some "makeGeneric", false, [expr; genArgs])
            |> makeCall None Fable.Any
    match ent.FullName with
    | "System.TimeSpan" -> Fable.StringConst "number" |> Fable.Value |> Some
    | "System.DateTime" -> makeIdentExpr "Date" |> Some
    | "System.Timers.Timer" -> makeCoreRef "Timer" None |> Some
    | "System.Text.RegularExpressions.Regex" -> makeIdentExpr "RegExp" |> Some
    | "System.Collections.Generic.Dictionary" ->
        makeIdentExpr "Map" |> makeGeneric genArgs |> Some
    | "System.Collections.Generic.HashSet" ->
        makeIdentExpr "Set" |> makeGeneric genArgs |> Some
    | "System.Collections.Generic.KeyValuePair" ->
        match genArgs with
        | [] -> makeIdentExpr "Array" |> Some
        | genArgs ->
            genArgs |> List.map snd
            |> Fable.NonDeclTuple
            |> makeNonDeclaredTypeRef |> Some
    | "Microsoft.FSharp.Core.FSharpChoice" -> makeCoreRef "Choice" None |> Some
    | "Microsoft.FSharp.Control.FSharpAsync" -> makeCoreRef "Async" None |> Some
    | "Microsoft.FSharp.Collections.FSharpSet" ->
        makeCoreRef "Set" None |> makeGeneric genArgs |> Some
    | "Microsoft.FSharp.Collections.FSharpMap" ->
        makeCoreRef "Map" None |> makeGeneric genArgs |> Some
    | "Microsoft.FSharp.Collections.FSharpList" ->
        makeCoreRef "List" None |> makeGeneric genArgs |> Some
    | Naming.EndsWith "Exception" _ ->
        makeIdentExpr "Error" |> Some
    | "Fable.Core.JsInterop.JsConstructor"
    | Naming.StartsWith "Fable.Core.JsInterop.JsFunc" _ ->
        Fable.StringConst "function" |> Fable.Value |> Some
    // Catch-all for unknown references to System and FSharp.Core classes
    | Naming.StartsWith "System." _
    | Naming.StartsWith "Fable.Core." _
    | Naming.StartsWith "Microsoft.FSharp." _ ->
        makeNonDeclaredTypeRef Fable.NonDeclAny |> Some
    | _ -> None
