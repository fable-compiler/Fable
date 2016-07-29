module Fable.Replacements
open Fable
open Fable.AST
open Fable.AST.Fable.Util

module Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."

    let inline (=>) first second = first, second

    let (|CoreMeth|_|) (com: ICompiler) coreMod meth expr =
        match expr with
        | Fable.Apply(Fable.Value(Fable.ImportRef(coreMod', importPath)),
                      [Fable.Value(Fable.StringConst meth')], Fable.ApplyGet,_,_)
            when importPath = com.Options.coreLib && coreMod = coreMod' && meth = meth' -> Some expr
        | _ -> None

    let (|CoreCons|_|) (com: ICompiler) coreMod expr =
        match expr with
        | Fable.Apply(Fable.Value(Fable.ImportRef(coreMod', importPath)),[], Fable.ApplyCons,_,_)
            when importPath = com.Options.coreLib && coreMod = coreMod' -> Some expr
        | _ -> None

    let (|Null|_|) = function
        | Fable.Wrapped(Fable.Value Fable.Null,_)
        | Fable.Value Fable.Null -> Some null
        | _ -> None

    let (|Type|) (expr: Fable.Expr) = expr.Type

    let (|NumberType|) = function
        | Fable.Number kind -> Some kind
        | _ -> None

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
        | Float32 | Float64 -> Float

    // The core lib expects non-curried lambdas
    let deleg (info: Fable.ApplyInfo) args =
        if info.lambdaArgArity > 1
        then List.mapi (fun i x ->
            if i=0 then (makeDelegate (Some info.lambdaArgArity) x) else x) args
        else args

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
        |> makeCall com i.range i.returnType

    let emit (i: Fable.ApplyInfo) emit args =
        Fable.Apply(Fable.Emit(emit) |> Fable.Value, args, Fable.ApplyMeth, i.returnType, i.range)

    let emitNoInfo emit args =
        Fable.Apply(Fable.Emit(emit) |> Fable.Value, args, Fable.ApplyMeth, Fable.Any, None)

    let wrap typ expr =
        Fable.Wrapped (expr, typ)

    let wrapInLambda args f =
        let argValues = List.map (Fable.IdentValue >> Fable.Value) args
        Fable.Lambda(args, f argValues) |> Fable.Value

    let genArg (t: Fable.Type) =
        match t.GenericArgs with
        | [genArg] -> genArg
        | _ -> Fable.Any

    let toChar com (i: Fable.ApplyInfo) (arg: Fable.Expr) =
        match arg.Type with
        | Fable.String -> arg
        | _ -> GlobalCall ("String", Some "fromCharCode", false, [arg])
               |> makeCall com i.range i.returnType

    let toString com (i: Fable.ApplyInfo) (toBase: Fable.Expr option) (arg: Fable.Expr) =
        match arg.Type with
        | Fable.String -> arg
        | _ -> InstanceCall (arg, "toString", Option.toList toBase)
               |> makeCall com i.range i.returnType

    let toInt, toFloat =
        let toNumber com (i: Fable.ApplyInfo) typ (arg: Fable.Expr) =
            match arg.Type with
            | Fable.String ->
                GlobalCall ("Number", Some ("parse"+typ), false, [arg])
                |> makeCall com i.range i.returnType
            | _ ->
                if typ = "Int"
                then GlobalCall ("Math", Some "floor", false, [arg])
                     |> makeCall com i.range i.returnType
                else wrap i.returnType arg
        (fun com i arg -> toNumber com i "Int" arg),
        (fun com i arg -> toNumber com i "Float" arg)

    let toList com (i: Fable.ApplyInfo) expr =
        CoreLibCall ("Seq", Some "toList", false, [expr])
        |> makeCall com i.range i.returnType

    let toArray com (i: Fable.ApplyInfo) expr =
        let arrayFrom arrayCons expr =
            GlobalCall (arrayCons, Some "from", false, [expr])
            |> makeCall com i.range i.returnType
        match expr, i.returnType with
        // Optimization
        | Fable.Apply(CoreMeth com "List" "ofArray" _, [arr], Fable.ApplyMeth,_,_), _ -> arr
        | CoreCons com "List" _, _ ->
            Fable.ArrayConst(Fable.ArrayValues [], genArg i.returnType) |> Fable.Value
        // Typed arrays
        | _, Fable.Array(Fable.Number numberKind) -> arrayFrom (getTypedArrayName com numberKind) expr
        | _ -> arrayFrom "Array" expr

    let applyOp com (i: Fable.ApplyInfo) (args: Fable.Expr list) meth =
        let apply op args =
            Fable.Apply(Fable.Value op, args, Fable.ApplyMeth, i.returnType, i.range)
        match args.Head.Type with
        // Floor result of integer divisions (see #172)
        // Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
        | Fable.Number Integer when meth = "op_Division" ->
            apply (Fable.BinaryOp BinaryDivide) args
            |> List.singleton |> apply (Fable.UnaryOp UnaryNotBitwise)
            |> List.singleton |> apply (Fable.UnaryOp UnaryNotBitwise)
        | EntFullName (KeyValue "System.DateTime" "Date" modName)
        | EntFullName (KeyValue "Microsoft.FSharp.Collections.FSharpSet" "Set" modName) ->
            CoreLibCall (modName, Some meth, false, args)
            |> makeCall com i.range i.returnType
        | Fable.DeclaredType(ent, _) when ent.FullName <> "System.TimeSpan" ->
            let typRef = Fable.Value (Fable.TypeRef ent)
            InstanceCall(typRef, meth, args)
            |> makeCall com i.range i.returnType
        | _ ->
            let op =
                match meth with
                | "op_Addition" -> Fable.BinaryOp BinaryPlus
                | "op_Subtraction" -> Fable.BinaryOp BinaryMinus
                | "op_Multiply" -> Fable.BinaryOp BinaryMultiply
                | "op_Division" -> Fable.BinaryOp BinaryDivide
                | "op_Modulus" -> Fable.BinaryOp BinaryModulus
                | "op_LeftShift" -> Fable.BinaryOp BinaryShiftLeft
                | "op_RightShift" -> Fable.BinaryOp BinaryShiftRightSignPropagating
                | "op_BitwiseAnd" -> Fable.BinaryOp BinaryAndBitwise
                | "op_BitwiseOr" -> Fable.BinaryOp BinaryOrBitwise
                | "op_ExclusiveOr" -> Fable.BinaryOp BinaryXorBitwise
                | "op_LogicalNot" -> Fable.UnaryOp UnaryNotBitwise
                | "op_UnaryNegation" -> Fable.UnaryOp UnaryMinus
                | "op_BooleanAnd" -> Fable.LogicalOp LogicalAnd
                | "op_BooleanOr" -> Fable.LogicalOp LogicalOr
                | _ -> failwithf "Unknown operator: %s" meth
            apply op args

    let equals equal com (i: Fable.ApplyInfo) (args: Fable.Expr list) =
        let op equal =
            if equal then BinaryEqualStrict else BinaryUnequalStrict
            |> Fable.BinaryOp |> Fable.Value
        let is equal expr =
            if equal then expr
            else makeUnOp i.range i.returnType [expr] UnaryNot
        match args.Head.Type with
        | EntFullName "Microsoft.FSharp.Core.FSharpOption"
        | Fable.Array _ | Fable.Tuple _ ->
            CoreLibCall("Util", Some "equals", false, args)
            |> makeCall com i.range i.returnType |> is equal |> Some
        | EntFullName "System.DateTime" ->
            CoreLibCall ("Date", Some "equals", false, args)
            |> makeCall com i.range i.returnType |> is equal |> Some
        | Fable.DeclaredType(ent, _)
            when (ent.HasInterface "System.IEquatable" && ent.FullName <> "System.TimeSpan")
                || ent.FullName = "Microsoft.FSharp.Collections.FSharpList"
                || ent.FullName = "Microsoft.FSharp.Collections.FSharpMap"
                || ent.FullName = "Microsoft.FSharp.Collections.FSharpSet" ->
            InstanceCall(args.Head, "Equals", args.Tail)
            |> makeCall com i.range i.returnType |> is equal |> Some
        | _ ->
            Fable.Apply(op equal, args, Fable.ApplyMeth, i.returnType, i.range) |> Some

    /// Compare function that will call Util.compare or instance `CompareTo` as appropriate
    /// If passed an optional binary operator, it will wrap the comparison like `comparison < 0`
    let compare com r (args: Fable.Expr list) op =
        let wrapWith op comparison =
            match op with
            | None -> comparison
            | Some op -> makeEqOp r [comparison; makeConst 0] op
        match args.Head.Type with
        | EntFullName "Microsoft.FSharp.Core.FSharpOption"
        | Fable.Array _ | Fable.Tuple _ ->
            CoreLibCall("Util", Some "compare", false, args)
            |> makeCall com r (Fable.Number Int32) |> wrapWith op
        | Fable.DeclaredType(ent, _)
            when ent.HasInterface "System.IComparable"
                && ent.FullName <> "System.TimeSpan"
                && ent.FullName <> "System.DateTime" ->
            InstanceCall(args.Head, "CompareTo", args.Tail)
            |> makeCall com r (Fable.Number Int32) |> wrapWith op
        | _ ->
            match op with
            | Some op -> makeEqOp r args op
            | None -> CoreLibCall("Util", Some "compare", false, args)
                      |> makeCall com r (Fable.Number Int32)

    let makeComparer com (typArg: Fable.Type option) =
        match typArg with
        | None
        | Some(EntFullName "Microsoft.FSharp.Core.FSharpOption")
        | Some(Fable.Array _ | Fable.Tuple _) ->
            [makeCoreRef com "Util" (Some "compare")]
        | Some(Fable.DeclaredType(ent, _))
            when ent.HasInterface "System.IComparable"
                && ent.FullName <> "System.TimeSpan"
                && ent.FullName <> "System.DateTime" ->
            [emitNoInfo "(x,y) => x.CompareTo(y)" []]
        | Some _ -> [emitNoInfo "(x,y) => x < y ? -1 : x > y ? 1 : 0" []]
        |> fun args -> CoreLibCall("GenericComparer", None, true, args)
        |> makeCall com None Fable.Any

    let makeMapOrSetCons com (i: Fable.ApplyInfo) modName args =
        let typArg =
            match i.calleeTypeArgs, i.methodTypeArgs with
            | x::_, _ | [], x::_ -> Some x
            | [], [] -> None
        let args =
            (if List.isEmpty args then [Fable.Value Fable.Null] else args)
            @ [makeComparer com typArg]
        CoreLibCall(modName, Some "create", false, args)
        |> makeCall com i.range i.returnType

module private AstPass =
    open Util

    let fableCore com (i: Fable.ApplyInfo) =
        let destruct = function
            | Fable.Value(Fable.TupleConst exprs) -> exprs
            | expr -> [expr]
        match i.methodName with
        | Naming.StartsWith "import" _ ->
            let selector =
                match i.methodName with
                | "importMember" -> Naming.placeholder
                | "importDefault" -> "default"
                | _ -> "*" // importAllFrom
            let path =
                match i.args with
                | [Fable.Value(Fable.StringConst path)] -> path
                | _ -> failwithf "%s.%s only accepts literal strings %O"
                                i.ownerFullName i.methodName i.range
            Fable.ImportRef(selector, path) |> Fable.Value |> Some
        | "op_Dynamic" ->
            makeGet i.range i.returnType i.args.Head i.args.Tail.Head |> Some
        | "op_DynamicAssignment" ->
            match i.callee, i.args with
            | ThreeArgs (callee, prop, value) ->
                Fable.Set (callee, Some prop, value, i.range) |> Some
            | _ -> None
        | "op_Dollar" ->
            Fable.Apply(i.args.Head, destruct i.args.Tail.Head,
                Fable.ApplyMeth, i.returnType, i.range) |> Some
        | "op_EqualsEqualsGreater" ->
            Fable.TupleConst(List.take 2 i.args) |> Fable.Value |> Some
        | "createNew" ->
            Fable.Apply(i.args.Head, destruct i.args.Tail.Head,
                Fable.ApplyCons, i.returnType, i.range) |> Some
        | "createObj" ->
            let (|Fields|_|) = function
                | Fable.Value(Fable.ArrayConst(Fable.ArrayValues exprs, _)) ->
                    exprs
                    |> List.choose (function
                        | Fable.Value(Fable.TupleConst [Fable.Value(Fable.StringConst key); value]) -> Some(key, value)
                        | _ -> None)
                    |> function
                        | fields when fields.Length = exprs.Length -> Some fields
                        | _ -> None
                | _ -> None
            match i.args.Head with
            | CoreCons com "List" _ ->
                makeJsObject (defaultArg i.range SourceLocation.Empty) [] |> Some
            | Fable.Apply(_, [Fields fields], _, _, _) ->
                makeJsObject (defaultArg i.range SourceLocation.Empty) fields |> Some
            | _ ->
                CoreLibCall("Util", Some "createObj", false, i.args)
                |> makeCall com i.range i.returnType |> Some
        | "createEmpty" ->
            Fable.ObjExpr ([], [], None, i.range)
            |> wrap i.returnType |> Some
        | "areEqual" ->
            ImportCall("assert", "default", Some "equal", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | "async.AwaitPromise.Static" | "async.StartAsPromise.Static" ->
            let meth =
                if i.methodName = "async.AwaitPromise.Static"
                then "awaitPromise" else "startAsPromise"
            CoreLibCall("Async", Some meth, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        | "toJson" | "ofJson" | "toPlainJsObj" ->
            CoreLibCall("Util", Some i.methodName, false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | _ -> None

    let references com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            makeJsObject i.range.Value [("contents", i.args.Head)] |> Some
        | "contents" | "value" ->
            let prop = makeConst "contents"
            match i.methodKind with
            | Fable.Getter _ ->
                makeGet i.range Fable.Any i.callee.Value prop |> Some
            | Fable.Setter _ ->
                Fable.Set(i.callee.Value, Some prop, i.args.Head, i.range) |> Some
            | _ -> None
        | _ -> None

    let operators (com: ICompiler) (info: Fable.ApplyInfo) =
        let math range typ args methName =
            GlobalCall ("Math", Some methName, false, args)
            |> makeCall com range typ |> Some
        let r, typ, args = info.range, info.returnType, info.args
        match info.methodName with
        | "defaultArg" ->
            let cond = makeEqOp r [args.Head; Fable.Value Fable.Null] BinaryUnequal
            Fable.IfThenElse(cond, args.Head, args.Tail.Head, r) |> Some
        | "defaultAsyncBuilder" -> makeCoreRef com "defaultAsyncBuilder" None |> Some
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
        | "round" | "sin" | "sqrt" | "tan" ->
            math r typ args info.methodName
        // Function composition
        | "op_ComposeRight" | "op_ComposeLeft" ->
            // If expression is a let binding we have to wrap it in a function
            let wrap expr placeholder =
                match expr with
                | Fable.Sequential _ -> sprintf "(function(){return %s}())" placeholder
                | _ -> placeholder
            let args = if info.methodName = "op_ComposeRight" then args else List.rev args
            let f0 = wrap args.Head "$0"
            let f1 = wrap args.Tail.Head "$1"
            let pattern = System.String.Format("{0}=>{1}({2}({0}))", Naming.getUniqueVar(), f1,f0)
            emit info pattern args |> Some
        // Reference
        | "op_Dereference" -> makeGet r Fable.Any args.Head (makeConst "contents") |> Some
        | "op_ColonEquals" -> Fable.Set(args.Head, Some(makeConst "contents"), args.Tail.Head, r) |> Some
        | "ref" -> makeJsObject r.Value [("contents", args.Head)] |> Some
        | "increment" | "decrement" ->
            if info.methodName = "increment" then "++" else "--"
            |> sprintf "void($0.contents%s)"
            |> emit info <| args |> Some
        // Conversions
        | "createSequence" | "identity" | "box" | "unbox" -> wrap typ args.Head |> Some
        | "toInt" -> toInt com info args.Head |> Some
        | "toDouble" -> toFloat com info args.Head |> Some
        | "toChar"  -> toChar com info args.Head |> Some
        | "toString" -> toString com info None args.Head |> Some
        | "createDictionary" ->
            GlobalCall("Map", None, true, args) |> makeCall com r typ |> Some
        | "createSet" ->
            makeMapOrSetCons com info "Set" args |> Some
        // Ignore: wrap to keep Unit type (see Fable2Babel.transformFunction)
        | "ignore" -> Fable.Wrapped (args.Head, Fable.Unit) |> Some
        // Ranges
        | "op_Range" | "op_RangeStep" ->
            let meth =
                match info.methodTypeArgs.Head with
                | Fable.String -> "rangeChar"
                | _ -> if info.methodName = "op_Range" then "range" else "rangeStep"
            CoreLibCall("Seq", Some meth, false, args)
            |> makeCall com r typ |> Some
        // Tuples
        | "fst" | "snd" ->
            if info.methodName = "fst" then 0 else 1
            |> makeConst
            |> makeGet r typ args.Head |> Some
        // Strings
        | "printFormatToString"         // sprintf
        | "printFormat" | "printFormatLine" // printf/printfn
        | "printFormatToStringThenFail" ->  // failwithf
            let emit =
                match info.methodName with
                | "printFormatToString" -> "x=>x"
                | "printFormat" | "printFormatLine" -> "x=>{console.log(x)}"
                | "printFormatToStringThenFail" | _ -> "x=>{throw x}"
                |> Fable.Emit |> Fable.Value
            Fable.Apply(args.Head, [emit], Fable.ApplyMeth, typ, r)
            |> Some
        // Exceptions
        | "failWith" | "raise" | "reraise" | "invalidOp" ->
            Fable.Throw (args.Head, typ, r) |> Some
        // Type ref
        | "typeOf" ->
            makeTypeRef com info.range info.methodTypeArgs.Head |> Some
        // Concatenates two lists
        | "op_Append" ->
          CoreLibCall("List", Some "append", false, args)
          |> makeCall com r typ |> Some
        | _ -> None

    let fsFormat com (i: Fable.ApplyInfo) =
        CoreLibCall("String", Some "fsFormat", false, i.args)
        |> makeCall com i.range i.returnType |> Some

    let strings com (i: Fable.ApplyInfo) =
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
            makeGet i.range i.returnType c (makeConst "length") |> Some
        | "contains" ->
            makeEqOp i.range [icall com i "indexOf"; makeConst 0] BinaryGreaterOrEqual |> Some
        | "startsWith" ->
            makeEqOp i.range [icall com i "indexOf"; makeConst 0] BinaryEqualStrict |> Some
        | "substring" -> icall com i "substr" |> Some
        | "toUpper" -> icall com i "toLocaleUpperCase" |> Some
        | "toUpperInvariant" -> icall com i "toUpperCase" |> Some
        | "toLower" -> icall com i "toLocaleLowerCase" |> Some
        | "toLowerInvariant" -> icall com i "toLowerCase" |> Some
        | "indexOf" | "lastIndexOf" -> icall com i i.methodName |> Some
        | "trim" | "trimStart" | "trimEnd" ->
            let side =
                match i.methodName with
                | "trimStart" -> "start"
                | "trimEnd" -> "end"
                | _ -> "both"
            CoreLibCall("String", Some "trim", false, i.callee.Value::(makeConst side)::i.args)
            |> makeCall com i.range i.returnType |> Some
        | "toCharArray" ->
            InstanceCall(i.callee.Value, "split", [makeConst ""])
            |> makeCall com i.range i.returnType |> Some
        | "iterate" | "iterateIndexed" | "forAll" | "exists" ->
            CoreLibCall("Seq", Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        | "map" | "mapIndexed" | "collect"  ->
            CoreLibCall("Seq", Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range Fable.Any
            |> List.singleton
            |> emit i "Array.from($0).join('')"
            |> Some
        | "split" ->
            match i.args with
            | [Fable.Value(Fable.StringConst _) as separator]
            | [Fable.Value(Fable.ArrayConst(Fable.ArrayValues [separator],_))] ->
                InstanceCall(i.callee.Value, "split", [separator]) // Optimization
            | [arg1; Type(Fable.Enum _) as arg2] ->
                let args = [arg1; Fable.Value Fable.Null; arg2]
                CoreLibCall("String", Some "split", false, i.callee.Value::args)
            | args -> CoreLibCall("String", Some "split", false, i.callee.Value::args)
            |> makeCall com i.range Fable.String
            |> Some
        | _ -> None

    let log com (i: Fable.ApplyInfo) =
        let v =
            match i.args with
            | [] -> Fable.Value Fable.Null
            | [v] -> v
            | Type Fable.String::_ ->
                CoreLibCall("String", Some "format", false, i.args)
                |> makeCall com i.range Fable.String
            | _ -> i.args.Head
        GlobalCall("console", Some "log", false, [v])
        |> makeCall com i.range i.returnType

    let console com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "write" | "writeLine" -> log com i |> Some
        | _ -> None

    let debug com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "write" | "writeLine" -> log com i |> Some
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
        let prop p callee =
            makeGet i.range i.returnType callee (makeConst p)
        let isGroup =
            match i.callee with
            | Some (Type (EntFullName "System.Text.RegularExpressions.Group")) -> true
            | _ -> false
        match i.methodName with
        | ".ctor" ->
            // TODO: Use RegexConst if no options have been passed?
            CoreLibCall("RegExp", Some "create", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | "options" ->
            CoreLibCall("RegExp", Some "options", false, [i.callee.Value])
            |> makeCall com i.range i.returnType |> Some
        // Capture
        | "index" ->
            if not isGroup
            then prop "index" i.callee.Value |> Some
            else "Accessing index of Regex groups is not supported"
                 |> attachRange i.range |> failwith
        | "value" ->
            if isGroup
            then i.callee.Value |> wrap i.returnType |> Some
            else prop 0 i.callee.Value |> Some
        | "length" ->
            if isGroup
            then prop "length" i.callee.Value |> Some
            else prop 0 i.callee.Value |> prop "length" |> Some
        // Group
        | "success" ->
            makeEqOp i.range [i.callee.Value; Fable.Value Fable.Null] BinaryUnequal |> Some
        // Match
        | "groups" -> wrap i.returnType i.callee.Value |> Some
        // MatchCollection & GroupCollection
        | "item" ->
            makeGet i.range i.returnType i.callee.Value i.args.Head |> Some
        | "count" ->
            prop "length" i.callee.Value |> Some
        | _ -> None

    let languagePrimitives com (i: Fable.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "enumOfValue", OneArg (arg) -> arg |> Some
        | _ -> None

    let intrinsicFunctions com (i: Fable.ApplyInfo) =
        match i.methodName, (i.callee, i.args) with
        | "checkThis", (None, [arg]) -> Some arg
        | "unboxGeneric", OneArg (arg) -> wrap i.returnType arg |> Some
        | "getString", TwoArgs (ar, idx)
        | "getArray", TwoArgs (ar, idx) ->
            makeGet i.range i.returnType ar idx |> Some
        | "setArray", ThreeArgs (ar, idx, value) ->
            Fable.Set (ar, Some idx, value, i.range) |> Some
        | "getArraySlice", ThreeArgs (ar, lower, upper) ->
            let upper =
                match upper with
                | Null _ -> emitNoInfo "$0.length" [ar]
                | _ -> emitNoInfo "$0 + 1" [upper]
            InstanceCall (ar, "slice", [lower; upper])
            |> makeCall com i.range i.returnType |> Some
        | "setArraySlice", (None, args) ->
            CoreLibCall("Array", Some "setSlice", false, args)
            |> makeCall com i.range i.returnType |> Some
        | "typeTestGeneric", (None, [expr]) ->
            makeTypeTest com i.range i.methodTypeArgs.Head expr |> Some
        | "createInstance", (None, _) ->
            let typRef, args = makeTypeRef com i.range i.methodTypeArgs.Head, []
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
        let wrapInLet f expr =
            let ident = Naming.getUniqueVar() |> makeIdent
            [
                Fable.VarDeclaration(ident, expr, false)
                f(Fable.Value(Fable.IdentValue ident))
            ]
            |> fun exprs -> Fable.Sequential(exprs, i.range)
        let toArray r optExpr =
            // "$0 != null ? [$0]: []"
            let makeArray exprs = Fable.ArrayConst(Fable.ArrayValues exprs, genArg i.returnType) |> Fable.Value
            Fable.IfThenElse(makeEqOp r [optExpr; Fable.Value Fable.Null] BinaryUnequal, makeArray [optExpr], makeArray [], r)
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
            | _ -> Fable.Lambda([], comp) |> Fable.Value |> Some
        | "map" | "bind" ->
            // emit i "$1 != null ? $0($1) : $1" i.args |> Some
            let f, arg = i.args.Head, i.args.Tail.Head
            arg |> wrapInLet (fun e ->
                Fable.IfThenElse(
                    makeEqOp i.range [e; Fable.Value Fable.Null] BinaryUnequal,
                    Fable.Apply(f, [e], Fable.ApplyMeth, Fable.Any, i.range),
                    e, i.range))
            |> Some
        | "toArray" -> toArray i.range i.args.Head |> Some
        | meth ->
            let args =
                let args = List.rev i.args
                wrapInLet (fun e -> toArray i.range e) args.Head
                |> fun argsHead -> List.rev (argsHead::args.Tail)
            CoreLibCall("Seq", Some meth, false, deleg i args)
            |> makeCall com i.range i.returnType |> Some

    let timeSpans com (i: Fable.ApplyInfo) =
        // let callee = match i.callee with Some c -> c | None -> i.args.Head
        match i.methodName with
        | ".ctor" ->
            CoreLibCall("TimeSpan", Some "create", false, i.args)
            |> makeCall com i.range i.returnType |> Some
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
                    (List.take 6 i.args)@[makeConst 0; last]
                | _ -> i.args
            CoreLibCall("Date", Some "create", false, args)
            |> makeCall com i.range i.returnType |> Some
        | "kind" ->
            makeGet i.range i.returnType i.callee.Value (makeConst "kind")
            |> Some
        | _ -> None

    let keyValuePairs com (i: Fable.ApplyInfo) =
        let get (k: obj) =
            makeGet i.range i.returnType i.callee.Value (makeConst k) |> Some
        match i.methodName with
        | ".ctor" -> Fable.Value(Fable.TupleConst i.args) |> Some
        | "key" -> get 0
        | "value" -> get 1
        | _ -> None

    let dictionaries com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            let makeMap args =
                GlobalCall("Map", None, true, args) |> makeCall com i.range i.returnType
            match i.args with
            | [] -> makeMap [] |> Some
            | _ ->
                match i.args.Head.Type with
                | Fable.Number Int32 -> makeMap [] |> Some
                | _ -> makeMap i.args |> Some
        | "isReadOnly" ->
            Fable.BoolConst false |> Fable.Value |> Some
        | "count" ->
            makeGet i.range i.returnType i.callee.Value (makeConst "size") |> Some
        | "containsValue" ->
            CoreLibCall ("Map", Some "containsValue", false, [i.args.Head; i.callee.Value])
            |> makeCall com i.range i.returnType |> Some
        | "item" -> icall com i (if i.args.Length = 1 then "get" else "set") |> Some
        | "keys" -> icall com i "keys" |> Some
        | "values" -> icall com i "values" |> Some
        | "containsKey" -> icall com i "has" |> Some
        | "clear" -> icall com i "clear" |> Some
        | "add" -> icall com i "set" |> Some
        | "remove" -> icall com i "delete" |> Some
        | "tryGetValue" ->
            match i.callee, i.args with
            | Some callee, [key; defVal] ->
                emit i "$0.has($1) ? [true, $0.get($1)] : [false, $2]" [callee; key; defVal]
                |> Some
            | _ -> None
        | _ -> None

    let systemSets com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            let makeSet args =
                GlobalCall("Set", None, true, args) |> makeCall com i.range i.returnType
            match i.args with
            | [] -> makeSet [] |> Some
            | _ ->
                match i.args.Head.Type with
                | Fable.Number Int32 -> makeSet [] |> Some
                | _ -> makeSet i.args |> Some
        | "count" ->
            makeGet i.range i.returnType i.callee.Value (makeConst "size") |> Some
        | "isReadOnly" ->
            Fable.BoolConst false |> Fable.Value |> Some
        | "add" -> icall com i "add" |> Some
        | "clear" -> icall com i "clear" |> Some
        | "contains" -> icall com i "has" |> Some
        | "remove" -> icall com i "delete" |> Some
        | "isProperSubsetOf" | "isProperSupersetOf"
        | "isSubsetOf" | "isSupersetOf" | "copyTo" ->
            CoreLibCall ("Set", Some i.methodName, false, i.callee.Value::i.args)
            |> makeCall com i.range i.returnType |> Some
        // TODO
        // | "intersectWith"
        // | "exceptWith"
        // | "unionWith"
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
            makeGet i.range i.returnType callee (makeConst prop)
        let icall meth =
            let callee, args = instanceArgs()
            InstanceCall (callee, meth, args)
            |> makeCall com i.range i.returnType
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
            |> makeCall com i.range i.returnType |> Some
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
            CoreLibCall(modName, Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        // Set only static methods
        | "singleton" ->
            [makeArray Fable.Any i.args]
            |> makeMapOrSetCons com i modName |> Some
        | _ -> None

    type CollectionKind =
        | Seq | List | Array

    // Functions which don't return a new collection of the same type
    let implementedSeqNonBuildFunctions =
        set [ "average"; "averageBy"; "countBy"; "compareWith"; "empty";
              "exactlyOne"; "exists"; "exists2"; "fold"; "fold2"; "foldBack"; "foldBack2";
              "forAll"; "forAll2"; "head"; "tryHead"; "item"; "tryItem";
              "iterate"; "iterateIndexed"; "iterate2"; "iterateIndexed2";
              "isEmpty"; "last"; "tryLast"; "length";
              "mapFold"; "mapFoldBack"; "max"; "maxBy"; "min"; "minBy";
              "reduce"; "reduceBack"; "sum"; "sumBy"; "tail"; "toList";
              "tryFind"; "find"; "tryFindIndex"; "findIndex"; "tryPick"; "pick"; "unfold";
              "tryFindBack"; "findBack"; "tryFindIndexBack"; "findIndexBack" ]

    // Functions that must return a collection of the same type
    let implementedSeqBuildFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "distinctBy"; "distinctBy";
              "filter"; "where"; "groupBy"; "initialize";
              "map"; "mapIndexed"; "map2"; "mapIndexed2"; "map3";
              "ofArray"; "pairwise"; "permute"; "replicate"; "reverse";
              "scan"; "scanBack"; "singleton"; "skip"; "skipWhile";
              "take"; "takeWhile"; "sortWith"; "zip"; "zip3" ]

    let implementedListFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "filter"; "where";
              "initialize"; "map"; "mapIndexed"; "ofArray"; "partition";
              "replicate"; "reverse"; "singleton"; "unzip"; "unzip3" ]

    let implementedArrayFunctions =
        set [ "copyTo"; "partition"; "permute"; "sortInPlaceBy"; "unzip"; "unzip3" ]

    let nativeArrayFunctions =
        dict [ "exists" => "some"; "filter" => "filter";
               "find" => "find"; "findIndex" => "findIndex"; "forAll" => "every";
               "indexed" => "entries"; "iterate" => "forEach";
               "reduce" => "reduce"; "reduceBack" => "reduceRight";
               "sortInPlace" => "sort"; "sortInPlaceWith" => "sort" ]

    let collectionsSecondPass com (i: Fable.ApplyInfo) kind =
        let prop (meth: string) callee =
            makeGet i.range i.returnType callee (makeConst meth)
        let icall meth (callee, args) =
            InstanceCall (callee, meth, args)
            |> makeCall com i.range i.returnType
        let ccall modName meth args =
            CoreLibCall (modName, Some meth, false, args)
            |> makeCall com i.range i.returnType
        let meth, c, args =
            i.methodName, i.callee, i.args
        match meth with
        // Deal with special cases first
        | "cast" -> Some i.args.Head // Seq only, erase
        | "isEmpty" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array ->
                makeEqOp i.range [prop "length" args.Head; makeConst 0] BinaryEqualStrict
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
                if meth = "head" then makeGet i.range i.returnType c (makeConst 0)
                elif meth = "tail" then icall "slice" (c, [makeConst 1])
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
                Fable.Lambda(fnArgs, comparison) |> Fable.Value
            match c, kind with
            // This is for calls to instance `Sort` member on ResizeArrays
            | Some c, _ ->
                match args with
                | [] -> icall "sort" (c, [compareFn]) |> Some
                | [Type (Fable.Function _)] -> icall "sort" (c, deleg i args) |> Some
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
                    Fable.ArrayConst (Fable.ArrayAlloc (makeConst 0), typ) |> Fable.Value
                | _ -> "Expecting array type but got " + i.returnType.FullName
                       |> attachRange i.range |> failwith
            | List -> CoreLibCall ("List", None, true, args)
                      |> makeCall com i.range i.returnType
            |> Some
        | "zeroCreate" ->
            Fable.ArrayConst(Fable.ArrayAlloc i.args.Head, genArg i.returnType)
            |> Fable.Value |> Some
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
        | "add" ->
            icall "push" (c.Value, args) |> Some
        | "addRange" ->
            ccall "Array" "addRangeInPlace" [args.Head; c.Value] |> Some
        | "clear" ->
            icall "splice" (c.Value, [makeConst 0]) |> Some
        | "contains" ->
            emit i "$0.indexOf($1) > -1" (c.Value::args) |> Some
        | "indexOf" ->
            icall "indexOf" (c.Value, args) |> Some
        | "insert" ->
            icall "splice" (c.Value, [args.Head; makeConst 0; args.Tail.Head]) |> Some
        | "remove" ->
            ccall "Array" "removeInPlace" [args.Head; c.Value] |> Some
        | "removeAt" ->
            icall "splice" (c.Value, [args.Head; makeConst 1]) |> Some
        | "reverse" ->
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
                let zero = Fable.Apply(Fable.Value(Fable.TypeRef ent), [makeConst "Zero"],
                                        Fable.ApplyGet, i.returnType, None)
                let fargs = [makeTypedIdent "x" t; makeTypedIdent "y" t]
                let addFn = wrapInLambda fargs (fun args -> applyOp com i args "op_Addition")
                if meth = "sum"
                then addFn, args.Head
                else emitNoInfo "((f,add)=>(x,y)=>add(x,f(y)))($0,$1)" [args.Head;addFn], args.Tail.Head
                |> fun (f, xs) -> ccall "Seq" "fold" [f; zero; xs] |> Some
            | _ ->
                ccall "Seq" meth (deleg i args) |> Some
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
            | _ -> ccall "Seq" meth (deleg i args) |> Some
        // Default to Seq implementation in core lib
        | Patterns.SetContains implementedSeqNonBuildFunctions meth ->
            ccall "Seq" meth (deleg i args) |> Some
        | Patterns.SetContains implementedSeqBuildFunctions meth ->
            match kind with
            | Seq -> ccall "Seq" meth (deleg i args)
            | List -> ccall "Seq" meth (deleg i args) |> toList com i
            | Array -> ccall "Seq" meth (deleg i args) |> toArray com i
            |> Some
        | _ -> None

    let collectionsFirstPass com (i: Fable.ApplyInfo) kind =
        let icall meth (callee, args) =
            InstanceCall (callee, meth, args)
            |> makeCall com i.range i.returnType
            |> Some
        match kind with
        | List ->
            match i.methodName with
            | "getSlice" -> icall "slice" (i.callee.Value, i.args)
            | Patterns.SetContains implementedListFunctions meth ->
                CoreLibCall ("List", Some meth, false, deleg i i.args)
                |> makeCall com i.range i.returnType |> Some
            | _ -> None
        | Array ->
            match i.methodName with
            | "take" -> icall "slice" (i.args.Tail.Head, [makeConst 0; i.args.Head])
            | "skip" -> icall "slice" (i.args.Tail.Head, [i.args.Head])
            | "copy" -> icall "slice" (i.args.Head, [])
            | "getSubArray" ->
                // Array.sub array startIndex count
                // array.slice(startIndex, startIndex + count)
                emit i "$0.slice($1, $1 + $2)" i.args |> Some
            | "fill" ->
                // Array.fill target targetIndex count value
                // target.fill(value, targetIndex, targetIndex + count)
                emit i "$0.fill($3, $1, $1 + $2)" i.args |> Some
            | "map" ->
                match i.methodTypeArgs with
                // Native JS map is risky with typed arrays as they coerce
                // the final result (see #120, #171)
                | Fable.Any::_ | Fable.GenericParam _::_ -> None
                | NumberType(Some _) as tin::[tout] when tin = tout ->
                    icall "map" (i.args.[1], deleg i [i.args.[0]])
                | NumberType None::[NumberType None] ->
                    icall "map" (i.args.[1], deleg i [i.args.[0]])
                | _ -> None
            | "append" ->
                match i.methodTypeArgs with
                | [Fable.Any] | [NumberType(Some _)] -> None
                | _ -> icall "concat" (i.args.Head, i.args.Tail)
            | Patterns.SetContains implementedArrayFunctions meth ->
                CoreLibCall ("Array", Some meth, false, deleg i i.args)
                |> makeCall com i.range i.returnType |> Some
            | Patterns.DicContains nativeArrayFunctions meth ->
                let revArgs = List.rev i.args
                icall meth (revArgs.Head, deleg i (List.rev revArgs.Tail))
            | _ -> None
        | _ -> None
        |> function None -> collectionsSecondPass com i kind | someExpr -> someExpr

    let asserts com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "areEqual" ->
            ImportCall("assert", "default", Some "equal", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | _ -> None

    let exceptions com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            match i.args with
            | [] -> makeConst "error"
            | [arg] -> arg
            | args -> makeArray Fable.Any args
            |> Some
        | "message" -> i.callee
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
        // | "getHashCode" -> i.callee
        | ".ctor" -> Fable.ObjExpr ([], [], None, i.range) |> Some
        | "referenceEquals" -> makeEqOp i.range i.args BinaryEqualStrict |> Some
        | "toString" -> icall com i "toString" |> Some
        | "equals" -> staticArgs i.callee i.args |> equals true com i
        | "getType" -> emit i "Object.getPrototypeOf($0).constructor" [i.callee.Value] |> Some
        | _ -> None

    let types com (info: Fable.ApplyInfo) =
        let makeString = Fable.StringConst >> Fable.Value >> Some
        match info.callee with
        | Some(Fable.Value(Fable.TypeRef t)) ->
            match info.methodName with
            | "namespace" -> makeString t.Namespace
            | "fullName" -> makeString t.FullName
            | "name" -> makeString t.Name
            | _ -> None
        | _ ->
            match info.methodName with
            | "namespace" -> Some "getTypeNamespace"
            | "fullName" -> Some "getTypeFullName"
            | "name" -> Some "getTypeName"
            | _ -> None
            |> Option.map (fun meth ->
                CoreLibCall("Util", Some meth, false, [info.callee.Value])
                |> makeCall com info.range info.returnType)

    let unchecked com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "defaultof" ->
            match info.methodTypeArgs with
            | [Fable.Number _] -> makeConst 0
            | [Fable.Boolean _] -> makeConst false
            | _ -> Fable.Null |> Fable.Value
            |> Some
        | "compare" ->
            CoreLibCall("Util", Some "compare", false, info.args)
            |> makeCall com info.range info.returnType |> Some
        | _ -> None

    let random com (info: Fable.ApplyInfo) =
        match info.methodName with
        | ".ctor" ->
            let o = Fable.ObjExpr ([], [], None, info.range)
            Fable.Wrapped (o, info.returnType) |> Some
        | "next" ->
            let min, max =
                match info.args with
                | [] -> makeConst 0, makeConst System.Int32.MaxValue
                | [max] -> makeConst 0, max
                | [min; max] -> min, max
                | _ -> failwith "Unexpected arg count for Random.Next"
            "Math.floor(Math.random() * ($1 - $0)) + $0"
            |> emit info <| [min; max]
            |> Some
        | _ -> None

    let enumerator com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "getEnumerator" ->
            emit info "$0[Symbol.iterator]()" [info.callee.Value] |> Some
        | "moveNext" ->
            emit info "$0.current = $0.next(), !$0.current.done" [info.callee.Value] |> Some
        | "current" ->
            emit info "$0.current.value" [info.callee.Value] |> Some
        | _ -> None

    let conversions com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "toString" ->
            match info.args with
            | [arg] -> toString com info None arg
            | [arg; Type(Fable.Number _) as toBase] ->
                toString com info (Some toBase) arg
            | _ -> failwithf "System.Convert.ToString with IFormatProvider is not supported %O" info.range
            |> Some
        | _ ->
            failwithf "Only System.Convert.ToString is supported %O" info.range

    let mailbox com (info: Fable.ApplyInfo) =
        match info.callee with
        | None ->
            match info.methodName with
            | ".ctor" -> CoreLibCall("MailboxProcessor", None, true, info.args) |> Some
            | "start" -> CoreLibCall("MailboxProcessor", Some "start", false, info.args) |> Some
            | _ -> None
            |> Option.map (makeCall com info.range info.returnType)
        | Some callee ->
            match info.methodName with
            // `reply` belongs to AsyncReplyChannel
            | "start" | "receive" | "postAndAsyncReply" | "post" | "reply" ->
                InstanceCall(callee, info.methodName, info.args)
                |> makeCall com info.range info.returnType |> Some
            | _ -> None

    let guids com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "newGuid" ->
            CoreLibCall("String", Some "newGuid", false, [])
            |> makeCall com info.range info.returnType |> Some
        | "parse" -> info.args.Head |> Some
        | "tryParse" ->
            Fable.TupleConst [makeConst true; info.args.Head]
            |> Fable.Value |> Some
        | _ -> None

    let laziness com (info: Fable.ApplyInfo) =
        let coreCall meth isCons args =
            CoreLibCall("Lazy", meth, isCons, args)
            |> makeCall com info.range info.returnType
        let getProp callee prop =
            Fable.Apply(callee, [makeConst prop], Fable.ApplyGet, info.returnType, info.range)
        match info.methodName with
        | ".ctor" | "create" -> coreCall None true info.args |> Some
        | "createFromValue" -> coreCall (Some info.methodName) false info.args |> Some
        | "force" | "value" | "isValueCreated" ->
            let callee, _ = instanceArgs info.callee info.args
            match info.methodName with
            | "force" -> "value" | another -> another
            |> getProp callee |> Some
        | _ -> None

    let controlExtensions com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "addToObservable" -> Some "add"
        | "subscribeToObservable" -> Some "subscribe"
        | _ -> None
        |> Option.map (fun meth ->
            let args = staticArgs info.callee info.args |> List.rev
            CoreLibCall("Observable", Some meth, false, args)
            |> makeCall com info.range info.returnType)

    let tryReplace com (info: Fable.ApplyInfo) =
        match info.ownerFullName with
        | Naming.StartsWith "Fable.Core" _ -> fableCore com info
        | Naming.EndsWith "Exception" _ -> exceptions com info
        | "System.Object" -> objects com info
        | "System.Timers.ElapsedEventArgs" -> info.callee // only signalTime is available here
        | "System.String"
        | "Microsoft.FSharp.Core.StringModule" -> strings com info
        | "Microsoft.FSharp.Core.PrintfFormat" -> fsFormat com info
        | "System.Console" -> console com info
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
        | "System.Collections.Generic.IEnumerator"
        | "System.Collections.IEnumerable"
        | "System.Collections.IEnumerator" -> enumerator com info
        | "System.Collections.Generic.Dictionary"
        | "System.Collections.Generic.IDictionary" -> dictionaries com info
        | "System.Collections.Generic.HashSet"
        | "System.Collections.Generic.ISet" -> systemSets com info
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
        | "System.Convert" -> conversions com info
        | "Microsoft.FSharp.Control.FSharpMailboxProcessor"
        | "Microsoft.FSharp.Control.FSharpAsyncReplyChannel" -> mailbox com info
        | "System.Guid" -> guids com info
        | "System.Lazy" | "Microsoft.FSharp.Control.Lazy"
        | "Microsoft.FSharp.Control.LazyExtensions" -> laziness com info
        | "Microsoft.FSharp.Control.CommonExtensions" -> controlExtensions com info
        | _ -> None

module private CoreLibPass =
    open Util

    /// Module methods in the core lib can be bound Static or Both (instance and static).
    /// If they're bound only statically all methods will be called statically: if there's an
    /// instance, it'll be passed as the first argument and constructors will change to `create`.
    /// ATTENTION: currently there are no checks for instance methods. Make sure
    /// the core library polyfills all instance methods when using MapKind.Both.
    type MapKind = Static | Both

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
                CoreLibCall(modName, None, true, deleg info info.args)
                |> makeCall com info.range info.returnType |> Some
            | _, Fable.Getter _, Some callee ->
                let prop = Naming.upperFirst info.methodName |> makeConst
                Fable.Apply(callee, [prop], Fable.ApplyGet, info.returnType, info.range) |> Some
            | _, Fable.Setter _, Some callee ->
                let prop = Naming.upperFirst info.methodName |> makeConst
                Fable.Set(callee, Some prop, info.args.Head, info.range) |> Some
            | _, _, Some callee ->
                InstanceCall (callee, Naming.upperFirst info.methodName, deleg info info.args)
                |> makeCall com info.range info.returnType |> Some
            | _, _, None ->
                CoreLibCall(modName, Some info.methodName, false, staticArgs info.callee info.args |> deleg info)
                |> makeCall com info.range info.returnType |> Some
        | CoreLibPass.Static ->
            let meth =
                if info.methodName = ".ctor" then "create" else info.methodName
            CoreLibCall(modName, Some meth, false, staticArgs info.callee info.args |> deleg info)
            |> makeCall com info.range info.returnType |> Some
    | _ -> None

let tryReplace (com: ICompiler) (info: Fable.ApplyInfo) =
    try
        let info =
            info.methodName |> Naming.removeGetSetPrefix |> Naming.lowerFirst
            |> fun methName -> { info with methodName = methName }
        match AstPass.tryReplace com info with
        | Some res -> Some res
        | None -> coreLibPass com info
    with
    | ex -> failwithf "Cannot replace %s.%s: %s"
                info.ownerFullName info.methodName ex.Message
