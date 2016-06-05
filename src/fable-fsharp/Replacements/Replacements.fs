module Fable.Replacements
open Fable
open Fable.AST
open Fable.AST.Fable.Util

module Util =
    let [<Literal>] system = "System."
    let [<Literal>] fsharp = "Microsoft.FSharp."
    let [<Literal>] genericCollections = "System.Collections.Generic."

    let inline (=>) first second = first, second
        
    let (|KnownInterfaces|_|) fullName =
        if Naming.knownInterfaces.Contains fullName then Some fullName else None
        
    let (|CoreMeth|_|) (com: ICompiler) coreMod meth expr =
        match expr with
        | Fable.Apply(Fable.Value(Fable.ImportRef(coreMod', importPath)),
                      [Fable.Value(Fable.StringConst meth')], Fable.ApplyGet,_,_)
            when importPath = com.Options.coreLib && coreMod = coreMod' && meth = meth' -> Some expr
        | _ -> None

    let (|CoreCons|_|) (com: ICompiler) coreMod expr =
        match expr with
        | Fable.Apply(Fable.Value(Fable.ImportRef(coreMod', importPath)),_, Fable.ApplyCons,_,_)
            when importPath = com.Options.coreLib && coreMod = coreMod' -> Some expr
        | _ -> None

    let (|Null|_|) = function
        | Fable.Value Fable.Null -> Some null
        | _ -> None

    let (|Type|) (expr: Fable.Expr) = expr.Type
        
    let (|FullName|_|) (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType ent -> Some ent.FullName
        | _ -> None

    let (|DeclaredKind|_|) (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType ent -> Some ent.Kind
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
        
    let emit (i: Fable.ApplyInfo) emit args =
        Fable.Apply(Fable.Emit(emit) |> Fable.Value, args, Fable.ApplyMeth, i.returnType, i.range)

    let emitNoInfo emit args =
        Fable.Apply(Fable.Emit(emit) |> Fable.Value, args, Fable.ApplyMeth, Fable.UnknownType, None)
        
    let wrap typ expr =
        Fable.Wrapped (expr, typ)

    let toChar com (i: Fable.ApplyInfo) (arg: Fable.Expr) =
        match arg.Type with
        | Fable.PrimitiveType (Fable.String) -> arg
        | _ -> GlobalCall ("String", Some "fromCharCode", false, [arg])
               |> makeCall com i.range i.returnType

    let toString com (i: Fable.ApplyInfo) (arg: Fable.Expr) =
        match arg.Type with
        | Fable.PrimitiveType (Fable.String) -> arg
        | _ -> InstanceCall (arg, "toString", [])
               |> makeCall com i.range i.returnType

    let toInt, toFloat =
        let toNumber com (i: Fable.ApplyInfo) typ (arg: Fable.Expr) =
            match arg.Type with
            | Fable.PrimitiveType Fable.String ->
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
            |> makeCall com i.range (Fable.PrimitiveType(Fable.Array Fable.DynamicArray))
        match expr, i.returnType with
        // Optimization
        | Fable.Apply(CoreMeth com "List" "ofArray" _, [arr], Fable.ApplyMeth,_,_), _ ->
            arr
        | CoreCons com "List" _, _ ->
            Fable.ArrayConst(Fable.ArrayValues [], Fable.DynamicArray) |> Fable.Value
        // Typed arrays
        | _, Fable.PrimitiveType(Fable.Array(Fable.TypedArray numberKind)) ->
            arrayFrom (getTypedArrayName com numberKind) expr
        | _ ->
            arrayFrom "Array" expr

    let applyOp com (i: Fable.ApplyInfo) (args: Fable.Expr list) meth =
        let apply op args =
            Fable.Apply(Fable.Value op, args, Fable.ApplyMeth, i.returnType, i.range)
        match args.Head.Type with
        // Floor result of integer divisions (see #172)
        // Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
        | Fable.PrimitiveType(Fable.Number Integer) when meth = "op_Division" ->
            apply (Fable.BinaryOp BinaryDivide) args
            |> List.singleton |> apply (Fable.UnaryOp UnaryNotBitwise)
            |> List.singleton |> apply (Fable.UnaryOp UnaryNotBitwise)
            |> Some
        | Fable.UnknownType
        | Fable.PrimitiveType _
        | FullName "System.TimeSpan" ->
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
            apply op args |> Some
        | FullName (KeyValue "System.DateTime" "Date" modName)
        | FullName (KeyValue "Microsoft.FSharp.Collections.Set" "Set" modName) ->
            CoreLibCall (modName, Some meth, false, args)
            |> makeCall com i.range i.returnType |> Some
        | Fable.DeclaredType ent ->
            let typRef = Fable.Value (Fable.TypeRef ent)
            InstanceCall(typRef, meth, args)
            |> makeCall com i.range i.returnType |> Some

    let equals com (i: Fable.ApplyInfo) (args: Fable.Expr list) equal =
        let compareTo args =
            CoreLibCall("Util", Some "compareTo", false, args)
            |> makeCall com i.range i.returnType        
        let op =
            if equal then BinaryEqualStrict else BinaryUnequalStrict
            |> Fable.BinaryOp |> Fable.Value
        let negateIfNeeded expr =
            if equal then expr
            else makeUnOp i.range i.returnType [expr] UnaryNot  
        match args.Head.Type with
        | Fable.PrimitiveType (Fable.Array _)
        | FullName "Microsoft.FSharp.Collections.Set"
        | FullName "Microsoft.FSharp.Collections.Map" 
        | DeclaredKind Fable.Union
        | DeclaredKind Fable.Record ->
            Fable.Apply(op, [compareTo args; makeConst 0],
                Fable.ApplyMeth, i.returnType, i.range) |> Some
        | Fable.UnknownType
        | Fable.PrimitiveType _
        | FullName "System.TimeSpan" ->
            Fable.Apply(op, args, Fable.ApplyMeth, i.returnType, i.range) |> Some
        | FullName "System.DateTime" ->
            CoreLibCall ("Date", Some "equals", false, args)
            |> makeCall com i.range i.returnType |> negateIfNeeded |> Some
        | Fable.DeclaredType ent ->
            match ent.Kind with
            | Fable.Class _ when ent.HasInterface "System.IComparable" ->
                InstanceCall(args.Head, "equals", args.Tail)
                |> makeCall com i.range i.returnType |> negateIfNeeded |> Some
            | _ ->
                Fable.Apply(op, args, Fable.ApplyMeth, i.returnType, i.range) |> Some

    let compare com (i: Fable.ApplyInfo) (args: Fable.Expr list) op =
        let wrap op comp =
            match op with
            | None -> comp
            | Some op -> Fable.Apply(op, [comp; makeConst 0], Fable.ApplyMeth, i.returnType, i.range)
        let op = Option.map (Fable.BinaryOp >> Fable.Value) op
        match args.Head.Type with
        | Fable.PrimitiveType (Fable.Array _)
        | FullName "Microsoft.FSharp.Collections.Set"
        | FullName "Microsoft.FSharp.Collections.Map" 
        | DeclaredKind Fable.Union
        | DeclaredKind Fable.Record ->
            CoreLibCall("Util", Some "compareTo", false, args)
            |> makeCall com i.range i.returnType
            |> wrap op |> Some
        | Fable.UnknownType
        | Fable.PrimitiveType _
        | FullName "System.TimeSpan"
        | FullName "System.DateTime" ->
            match op with
            | None -> CoreLibCall("Util", Some "compareTo", false, args)
                      |> makeCall com i.range i.returnType
            | Some op -> Fable.Apply(op, args, Fable.ApplyMeth, i.returnType, i.range)
            |> Some
        | Fable.DeclaredType ent ->
            match ent.Kind with
            | Fable.Class _ when ent.HasInterface "System.IComparable" ->
                InstanceCall(args.Head, "compareTo", args.Tail)
                |> makeCall com i.range (Fable.PrimitiveType (Fable.Number Int32))
                |> wrap op |> Some
            | _ -> None

module private AstPass =
    open Util
    
    let fableCore com (i: Fable.ApplyInfo) =
        let destruct = function
            | Fable.Value (Fable.ArrayConst (Fable.ArrayValues exprs, Fable.Tuple)) -> exprs
            | expr -> [expr]
        match i.methodName with
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
            (Fable.ArrayValues (List.take 2 i.args), Fable.Tuple)
            |> Fable.ArrayConst |> Fable.Value |> Some
        | "createNew" ->
            Fable.Apply(i.args.Head, destruct i.args.Tail.Head,
                Fable.ApplyCons, i.returnType, i.range) |> Some
        | "createObj" ->
            let (|Fields|_|) = function
                | Fable.Value(Fable.ArrayConst(Fable.ArrayValues exprs, _)) ->
                    exprs
                    |> List.choose (function
                        | Fable.Value
                            (Fable.ArrayConst
                                (Fable.ArrayValues [Fable.Value(Fable.StringConst key); value],
                                    Fable.Tuple)) -> Some(key, value)
                        | _ -> None)
                    |> function
                        | fields when fields.Length = exprs.Length -> Some fields
                        | _ -> None
                | _ -> None
            match i.args.Head with
            | Fable.Apply(_, [Fields fields], _, _, _) ->
                makeJsObject i.range.Value fields |> Some
            | _ ->
                CoreLibCall("Util", Some "createObj", false, i.args)
                |> makeCall com i.range i.returnType |> Some
        | "createEmpty" ->
            Fable.ObjExpr ([], [], None, i.range)
            |> wrap i.returnType |> Some
        | "areEqual" ->
            ImportCall("assert", "default", Some "equal", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        | "awaitPromise" | "startAsPromise" ->
            CoreLibCall("Async", Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        | _ -> None
            
    let references com (i: Fable.ApplyInfo) =
        let r = ref 5
        match i.methodName with
        | ".ctor" -> makeJsObject i.range.Value [("contents", i.args.Head)] |> Some
        | "contents" | "value" -> makeGet i.range Fable.UnknownType i.args.Head (makeConst "contents") |> Some
        | _ -> None
    
    let operators com (info: Fable.ApplyInfo) =
        // TODO: Check primitive args also here?
        let math range typ args methName =
            GlobalCall ("Math", Some methName, false, args)
            |> makeCall com range typ |> Some
        let r, typ, args = info.range, info.returnType, info.args
        match info.methodName with
        | "async" -> makeCoreRef com "Async" |> Some
        // Negation
        | "not" -> makeUnOp r info.returnType args UnaryNot |> Some
        // Equality
        | "op_Inequality" | "neq" ->
            match args with
            | [Fable.Value Fable.Null; _]
            | [_; Fable.Value Fable.Null] -> makeEqOp r args BinaryUnequal |> Some
            | _ -> equals com info args false
        | "op_Equality" | "eq" ->
            match args with
            | [Fable.Value Fable.Null; _]
            | [_; Fable.Value Fable.Null] -> makeEqOp r args BinaryEqual |> Some
            | _ -> equals com info args true
        // Comparison
        | "compare" -> compare com info args None
        | "op_LessThan" | "lt" -> compare com info args (Some BinaryLess)
        | "op_LessThanOrEqual" | "lte" -> compare com info args (Some BinaryLessOrEqual)
        | "op_GreaterThan" | "gt" -> compare com info args (Some BinaryGreater)
        | "op_GreaterThanOrEqual" | "gte" -> compare com info args (Some BinaryGreaterOrEqual)
        | "min" | "max" ->
            let op = if info.methodName = "min" then BinaryLess else BinaryGreater
            let comparison = compare com info args (Some op) 
            emit info "$0 ? $1 : $2" (comparison.Value::args) |> Some
        // Operators
         | "op_Addition" | "op_Subtraction" | "op_Multiply" | "op_Division"
         | "op_Modulus" | "op_LeftShift" | "op_RightShift"
         | "op_BitwiseAnd" | "op_BitwiseOr" | "op_ExclusiveOr"
         | "op_LogicalNot" | "op_UnaryNegation" | "op_BooleanAnd" | "op_BooleanOr" ->
            applyOp com info args info.methodName
        // Math functions
        // TODO: optimize square pow: x * x
        | "pow" | "pown" | "op_Exponentiation" -> math r typ args "pow"
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
        | "op_Dereference" -> makeGet r Fable.UnknownType args.Head (makeConst "contents") |> Some
        | "op_ColonEquals" -> Fable.Set(args.Head, Some(makeConst "contents"), args.Tail.Head, r) |> Some
        | "ref" -> makeJsObject r.Value [("contents", args.Head)] |> Some
        | "incr" | "decr" ->
            if info.methodName = "incr" then "++" else "--"
            |> sprintf "void($0.contents%s)" 
            |> emit info <| args |> Some
        // Conversions
        | "seq" | "id" | "box" | "unbox" -> wrap typ args.Head |> Some
        | "int" -> toInt com info args.Head |> Some
        | "float" -> toFloat com info args.Head |> Some
        | "char"  -> toChar com info args.Head |> Some
        | "string" -> toString com info args.Head |> Some
        | "dict" | "set" ->
            let modName = if info.methodName = "dict" then "Map" else "Set"
            CoreLibCall(modName, Some "ofSeq", false, args)
            |> makeCall com r typ |> Some
        // Ignore: wrap to keep Unit type (see Fable2Babel.transformFunction)
        | "ignore" -> Fable.Wrapped (args.Head, Fable.PrimitiveType Fable.Unit) |> Some
        // Ranges
        | "op_Range" | "op_RangeStep" ->
            let meth = 
                match info.methodTypeArgs.Head with
                | Fable.PrimitiveType (Fable.String) -> "rangeChar"
                | _ -> if info.methodName = "op_Range" then "range" else "rangeStep"
            CoreLibCall("Seq", Some meth, false, args)
            |> makeCall com r typ |> Some
        // Tuples
        | "fst" | "snd" ->
            if info.methodName = "fst" then 0 else 1
            |> makeConst
            |> makeGet r typ args.Head |> Some
        // Strings
        | "sprintf" | "printf" | "printfn" | "failwithf" ->
            let emit = 
                match info.methodName with
                | "sprintf" -> "x=>x"
                | "printf" | "printfn" -> "x=>{console.log(x)}"
                | "failwithf" | _ -> "x=>{throw x}" 
                |> Fable.Emit |> Fable.Value
            Fable.Apply(args.Head, [emit], Fable.ApplyMeth, typ, r)
            |> Some
        // Exceptions
        | "failwith" | "raise" | "reraise" | "invalidOp" ->
            Fable.Throw (args.Head, r) |> Some
        // Type ref
        | "typeof" ->
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
        let icall meth =
            let c, args = instanceArgs i.callee i.args
            InstanceCall(c, meth, args)
            |> makeCall com i.range i.returnType
        match i.methodName with
        | ".ctor" ->
            match i.args.Head.Type with
            | Fable.PrimitiveType (Fable.String) ->
                match i.args with
                | [c; n] -> emit i "Array($1 + 1).join($0)" i.args |> Some // String(char, int)
                | _ -> failwith "Unexpected arguments in System.String constructor."
            | Fable.PrimitiveType (Fable.Array _) ->
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
            makeEqOp i.range [icall "indexOf"; makeConst 0] BinaryGreaterOrEqual |> Some
        | "startsWith" ->
            makeEqOp i.range [icall "indexOf"; makeConst 0] BinaryEqualStrict |> Some
        | "substring" -> icall "substr" |> Some
        | "toUpper" -> icall "toLocaleUpperCase" |> Some
        | "toUpperInvariant" -> icall "toUpperCase" |> Some
        | "toLower" -> icall "toLocaleLowerCase" |> Some
        | "toLowerInvariant" -> icall "toLowerCase" |> Some
        | "indexOf" | "lastIndexOf" | "trim" -> icall i.methodName |> Some
        | "toCharArray" ->
            InstanceCall(i.callee.Value, "split", [makeConst ""])
            |> makeCall com i.range i.returnType |> Some
        | "iter" | "iteri" | "forall" | "exists" ->
            CoreLibCall("Seq", Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        | "map" | "mapi" | "collect"  ->
            CoreLibCall("Seq", Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range Fable.UnknownType
            |> List.singleton
            |> emit i "Array.from($0).join('')"
            |> Some
        | _ -> None

    let console com (i: Fable.ApplyInfo) =
        match i.methodName with
        | "write" | "writeLine" ->
            let v =
                match i.args with
                | [] -> Fable.Value Fable.Null
                | [v] -> v
                | Type (Fable.PrimitiveType Fable.String)::_ ->
                    CoreLibCall("String", Some "format", false, i.args)
                    |> makeCall com i.range (Fable.PrimitiveType Fable.String)
                | _ -> i.args.Head 
            GlobalCall("console", Some "log", false, [v])
            |> makeCall com i.range i.returnType |> Some
        | "assert" -> failwith "TODO: Assertions"
        | _ -> None

    let regex com (i: Fable.ApplyInfo) =
        let prop p callee =
            makeGet i.range i.returnType callee (makeConst p)
        let isGroup =
            match i.callee with
            | Some (Type (FullName "System.Text.RegularExpressions.Group")) -> true
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

    let options com (i: Fable.ApplyInfo) =
        let toArray r optExpr =
            Fable.Apply(Fable.Emit("$0 != null ? [$0]: []") |> Fable.Value, [optExpr],
                Fable.ApplyMeth, Fable.PrimitiveType (Fable.Array Fable.DynamicArray), r)
        let getCallee() = match i.callee with Some c -> c | None -> i.args.Head
        match i.methodName with
        | "none" -> Fable.Null |> Fable.Value |> Some
        | "value" | "get" | "toObj" | "ofObj" | "toNullable" | "ofNullable" ->
           wrap i.returnType (getCallee()) |> Some
        | "isSome" -> makeEqOp i.range [getCallee(); Fable.Value Fable.Null] BinaryUnequal |> Some
        | "isNone" -> makeEqOp i.range [getCallee(); Fable.Value Fable.Null] BinaryEqual |> Some
        | "map" | "bind" -> emit i "$1 != null ? $0($1) : $1" i.args |> Some
        | "toArray" -> toArray i.range i.args.Head |> Some
        | meth ->
            let args =
                let args = List.rev i.args
                (toArray i.range args.Head)::args.Tail |> List.rev
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
                | 7, (Fable.PrimitiveType (Fable.Enum "System.DateTimeKind")) ->
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
        | "key" -> get 0
        | "value" -> get 1
        | _ -> None

    let dictionaries com (i: Fable.ApplyInfo) =
        let icall meth =
            InstanceCall (i.callee.Value, meth, i.args)
            |> makeCall com i.range i.returnType |> Some
        match i.methodName with
        | ".ctor" ->
            let makeMap args =
                GlobalCall("Map", None, true, args) |> makeCall com i.range i.returnType
            match i.args with
            | [] -> makeMap [] |> Some
            | _ ->
                match i.args.Head.Type with
                | Fable.PrimitiveType (Fable.Number Int32) ->
                    makeMap [] |> Some
                | _ -> makeMap i.args |> Some
        | "isReadOnly" ->
            Fable.BoolConst false |> Fable.Value |> Some
        | "count" ->
            makeGet i.range i.returnType i.callee.Value (makeConst "size") |> Some
        | "containsValue" ->
            CoreLibCall ("Map", Some "containsValue", false, [i.args.Head; i.callee.Value])
            |> makeCall com i.range i.returnType |> Some
        | "item" -> icall <| if i.args.Length = 1 then "get" else "set"
        | "keys" -> icall "keys"
        | "values" -> icall "values"
        | "containsKey" -> icall "has"
        | "clear" -> icall "clear"
        | "add" -> icall "set"
        | "remove" -> icall "delete"
        | "tryGetValue" ->
            match i.callee, i.args with
            | Some callee, [key; defVal] ->
                emit i "$0.has($1) ? [true, $0.get($1)] : [false, $2]" [callee; key; defVal]
                |> Some
            | _ -> None
        | _ -> None

    let systemSets com (i: Fable.ApplyInfo) =
        let icall meth =
            InstanceCall (i.callee.Value, meth, i.args)
            |> makeCall com i.range i.returnType |> Some
        match i.methodName with
        | ".ctor" ->
            let makeSet args =
                GlobalCall("Set", None, true, args) |> makeCall com i.range i.returnType
            match i.args with
            | [] -> makeSet [] |> Some
            | _ ->
                match i.args.Head.Type with
                | Fable.PrimitiveType (Fable.Number Int32) ->
                    makeSet [] |> Some
                | _ -> makeSet i.args |> Some
        | "count" ->
            makeGet i.range i.returnType i.callee.Value (makeConst "size") |> Some
        | "isReadOnly" ->
            Fable.BoolConst false |> Fable.Value |> Some
        | "add" -> icall "add"
        | "clear" -> icall "clear"
        | "contains" -> icall "has"
        | "copyTo" ->
            CoreLibCall ("Set", Some "copyTo", false, i.callee.Value::i.args)
            |> makeCall com i.range i.returnType |> Some
        | "remove" -> icall "delete"
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
            if i.ownerFullName.EndsWith("Map")
            then "Map" else "Set"
        let _of colType expr =
            CoreLibCall(modName, Some ("of" + colType), false, [expr])
            |> makeCall com i.range i.returnType
        match i.methodName with
        // Instance and static shared methods
        | "add" ->
            let args = match i.callee with Some c -> i.args@[c] | None -> i.args
            match modName with
            | "Map" -> emit i "new Map($2).set($0,$1)" args |> Some
            | _ -> emit i "new Set($1).add($0)" args |> Some
        | "count" -> prop "size" |> Some
        | "contains" | "containsKey" -> icall "has" |> Some
        | "remove" ->
            let callee, args = instanceArgs()
            CoreLibCall(modName, Some "remove", false, [args.Head; callee])
            |> makeCall com i.range i.returnType |> Some
        | "isEmpty" ->
            makeEqOp i.range [prop "size"; makeConst 0] BinaryEqualStrict |> Some
        // Map only instance and static methods
        | "tryFind" | "find" -> icall "get" |> Some
        | "item" -> icall "get" |> Some
        // Set only instance and static methods
        | KeyValue "maximumElement" "max" meth | KeyValue "maxElement" "max" meth
        | KeyValue "minimumElement" "min" meth | KeyValue "minElement" "min" meth ->
            let args = match i.callee with Some x -> [x] | None -> i.args
            CoreLibCall("Seq", Some meth, false, args)
            |> makeCall com i.range i.returnType |> Some
        // Constructors
        | "empty" ->
            GlobalCall(modName, None, true, [])
            |> makeCall com i.range i.returnType |> Some
        | ".ctor" ->
            CoreLibCall(modName, Some "ofSeq", false, i.args)
            |> makeCall com i.range i.returnType |> Some
        // Conversions
        | "toArray" -> toArray com i i.args.Head |> Some
        | "toList" -> toList com i i.args.Head |> Some
        | "toSeq" -> Some i.args.Head
        | "ofArray" -> _of "Array" i.args.Head |> Some
        | "ofList" | "ofSeq" -> _of "Seq" i.args.Head |> Some
        // Non-build static methods shared with Seq
        | "exists" | "fold" | "foldBack" | "forall" | "iter" ->
            let modName = if modName = "Map" then "Map" else "Seq"
            CoreLibCall(modName, Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        // Build static methods shared with Seq
        | "filter" | "map" ->
            match modName with
            | "Map" ->
                CoreLibCall(modName, Some i.methodName, false, deleg i i.args)
                |> makeCall com i.range i.returnType |> Some
            | _ ->
                CoreLibCall("Seq", Some i.methodName, false, deleg i i.args)
                |> makeCall com i.range i.returnType
                |> _of "Seq" |> Some
        // Static method
        | "partition" ->
            CoreLibCall(modName, Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        // Map only static methods (make delegate)
        | "findKey" | "tryFindKey" | "pick" | "tryPick" ->
            CoreLibCall("Map", Some i.methodName, false, deleg i i.args)
            |> makeCall com i.range i.returnType |> Some
        // Set only static methods
        | "singleton" ->
            emit i "new Set([$0])" i.args |> Some
        | _ -> None

    type CollectionKind =
        | Seq | List | Array
    
    // Functions which don't return a new collection of the same type
    let implementedSeqNonBuildFunctions =
        set [ "average"; "averageBy"; "countBy"; "compareWith"; "empty";
              "exactlyOne"; "exists"; "exists2"; "fold"; "fold2"; "foldBack"; "foldBack2";
              "forall"; "forall2"; "head"; "tryHead"; "item"; "tryItem"; "iter"; "iteri"; "iter2"; "iteri2";
              "isEmpty"; "last"; "tryLast"; "length"; "max"; "maxBy"; "min"; "minBy";
              "reduce"; "reduceBack"; "sum"; "sumBy"; "tail"; "toList";
              "tryFind"; "find"; "tryFindIndex"; "findIndex"; "tryPick"; "pick"; "unfold";
              "tryFindBack"; "findBack"; "tryFindIndexBack"; "findIndexBack" ]

    // Functions that must return a collection of the same type
    let implementedSeqBuildFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "distinctBy"; "distinctBy";
              "filter"; "where"; "groupBy"; "init";
              "map"; "mapi"; "map2"; "mapi2"; "map3";
              "ofArray"; "pairwise"; "permute"; "replicate"; "rev";
              "scan"; "scanBack"; "singleton"; "skip"; "skipWhile";
              "take"; "takeWhile"; "sort"; "sortBy"; "sortWith";
              "sortDescending"; "sortByDescending"; "zip"; "zip3" ]

    let implementedListFunctions =
        set [ "append"; "choose"; "collect"; "concat"; "filter"; "where";
              "init"; "map"; "mapi"; "ofArray"; "partition";
              "replicate"; "rev"; "singleton"; "unzip"; "unzip3" ]

    let implementedArrayFunctions =
        set [ "blit"; "partition"; "permute"; "sortInPlaceBy"; "unzip"; "unzip3" ]
        
    let nativeArrayFunctions =
        dict [ "exists" => "some"; "filter" => "filter";
               "find" => "find"; "findIndex" => "findIndex"; "forall" => "every";
               "indexed" => "entries"; "iter" => "forEach";
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
        // | "sum" | "sumBy" -> // TODO: Check if we need to use a custom operator
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
            | Seq -> ccall "Seq" meth (staticArgs c args)
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
        | "sort" ->
            match c, kind with
            | Some c, _ -> icall "sort" (c, deleg i args)
            | None, Seq -> ccall "Seq" meth (deleg i args)
            | None, List -> ccall "Seq" meth (deleg i args) |> toList com i
            | None, Array -> ccall "Seq" meth (deleg i args) |> toArray com i
            |> Some
        // Constructors ('cons' only applies to List)
        | "empty" | "cons" ->
            match kind with
            | Seq -> ccall "Seq" meth args
            | Array ->
                match i.returnType with
                | Fable.PrimitiveType (Fable.Array kind) ->
                    Fable.ArrayConst (Fable.ArrayAlloc (makeConst 0), kind) |> Fable.Value
                | _ -> "Expecting array type but got " + i.returnType.FullName
                       |> attachRange i.range |> failwith
            | List -> CoreLibCall ("List", None, true, args)
                      |> makeCall com i.range i.returnType
            |> Some
        | "zeroCreate" ->
            let arrayKind =
                match i.methodTypeArgs with
                | [Fable.PrimitiveType(Fable.Number numberKind)] ->
                    Fable.TypedArray numberKind
                | _ -> Fable.DynamicArray
            Fable.ArrayConst(Fable.ArrayAlloc i.args.Head, arrayKind)
            |> Fable.Value |> Some
        | "create" ->
            ccall "Seq" "replicate" args
            |> toArray com i |> Some
        // ResizeArray only
        | ".ctor" ->
            let makeEmptyArray () =
                (Fable.ArrayValues [], Fable.DynamicArray)
                |> Fable.ArrayConst |> Fable.Value |> Some
            match i.args with
            | [] -> makeEmptyArray ()
            | _ ->
                match i.args.Head.Type with
                | Fable.PrimitiveType (Fable.Number Int32) ->
                    makeEmptyArray ()
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
            icall "reverse" (c.Value, []) |> Some
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
            toArray com i i.args.Head |> Some
        | "ofList" ->
            match kind with
            | List -> "Unexpected method called on list: " + meth
                      |> attachRange i.range |> failwith
            | Seq -> ccall "Seq" "ofList" args
            | Array -> toArray com i i.args.Head
            |> Some
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
            | "sub" ->
                // Array.sub array startIndex count
                // array.slice(startIndex, startIndex + count)
                emit i "$0.slice($1, $1 + $2)" i.args |> Some
            | "fill" ->
                // Array.fill target targetIndex count value
                // target.fill(value, targetIndex, targetIndex + count)
                emit i "$0.fill($3, $1, $1 + $2)" i.args |> Some
            // Native JS map is risky with typed arrays as they coerce
            // the final result (see #120, #171)
            // | "map" -> icall "map" (i.args.[1], deleg i [i.args.[0]])
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
            | args -> makeArray Fable.UnknownType args
            |> Some
        | "message" -> i.callee
        | _ -> None
        
    let cancels com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" ->
            match i.args with
            | [Type (Fable.PrimitiveType(Fable.Number _)) as arg] ->
                "(function(){var token={};setTimeout(function(){token.isCancelled=true},$0); return token;}())"
                |> emit i <| [arg]
            | [Type (Fable.PrimitiveType(Fable.Boolean _)) as arg] ->
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

    let knownInterfaces com (i: Fable.ApplyInfo) =
        match i.methodName with
        | ".ctor" -> Fable.ObjExpr ([], [], None, i.range) |> Some
        | "getHashCode" -> i.callee
        | "referenceEquals" -> emit i "$0 === $1" i.args |> Some
        | meth -> InstanceCall (i.callee.Value, meth, i.args)
                  |> makeCall com i.range i.returnType |> Some

    let types com (info: Fable.ApplyInfo) =
        let makeString = Fable.StringConst >> Fable.Value >> Some
        match info.callee with
        | Some(Fable.AST.Fable.Expr.Value(Fable.AST.Fable.TypeRef t)) ->
            match info.methodName with
            | "namespace" -> makeString t.Namespace
            | "fullName" -> makeString t.FullName
            | "name" -> makeString t.Name
            | _ -> None
        | _ -> None

    let unchecked com (info: Fable.ApplyInfo) =
        match info.methodName with
        | "defaultof" ->
            match info.methodTypeArgs with
            | [Fable.PrimitiveType(Fable.Number _)] -> makeConst 0
            | [Fable.PrimitiveType(Fable.Boolean _)] -> makeConst false
            | _ -> Fable.Null |> Fable.Value
            |> Some
        // | "compare" -> emit info "$0 < $1 ? -1 : ($0 > $1 ? 1 : 0)" info.args |> Some
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

    let tryReplace com (info: Fable.ApplyInfo) =
        match info.ownerFullName with
        | KnownInterfaces _ -> knownInterfaces com info
        | Naming.StartsWith "Fable.Core" _ -> fableCore com info
        | Naming.EndsWith "Exception" _ -> exceptions com info
        | "System.Timers.ElapsedEventArgs" -> info.callee // only signalTime is available here
        | "System.String"
        | "Microsoft.FSharp.Core.String" -> strings com info
        | "Microsoft.FSharp.Core.PrintfFormat" -> fsFormat com info
        | "System.Console"
        | "System.Diagnostics.Debug" -> console com info
        | "System.DateTime" -> dates com info 
        | "System.TimeSpan" -> timeSpans com info
        | "System.Action"
        | "System.Func" -> funcs com info
        | "System.Random" -> random com info
        | "Microsoft.FSharp.Core.Option" -> options com info
        | "System.Threading.CancellationToken"
        | "System.Threading.CancellationTokenSource" -> cancels com info
        | "System.Math"
        | "Microsoft.FSharp.Core.Operators"
        | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com info
        | "Microsoft.FSharp.Core.Ref" -> references com info
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
        | "System.Collections.Generic.Dictionary`2.KeyCollection"
        | "System.Collections.Generic.Dictionary`2.ValueCollection"
        | "System.Collections.Generic.ICollection" -> collectionsSecondPass com info Seq
        | "System.Array"
        | "System.Collections.Generic.List"
        | "System.Collections.Generic.IList" -> collectionsSecondPass com info Array
        | "Microsoft.FSharp.Collections.Array" -> collectionsFirstPass com info Array
        | "Microsoft.FSharp.Collections.List" -> collectionsFirstPass com info List
        | "Microsoft.FSharp.Collections.Seq" -> collectionsSecondPass com info Seq
        | "Microsoft.FSharp.Collections.Map"
        | "Microsoft.FSharp.Collections.Set" -> mapAndSets com info
        // For some reason `typeof<'T>.Name` translates to `System.Reflection.MemberInfo.name`
        // and not `System.Type.name` (as with `typeof<'T>.FullName` and `typeof<'T>.Namespace`)
        | "System.Reflection.MemberInfo"
        | "System.Type" -> types com info
        | "Microsoft.FSharp.Core.Operators.Unchecked" -> unchecked com info
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
            fsharp + "Control.Async" => ("Async", Both)
            fsharp + "Control.AsyncBuilder" => ("Async", Both)
            fsharp + "Control.Observable" => ("Observable", Static)
            fsharp + "Core.CompilerServices.RuntimeHelpers" => ("Seq", Static)
            system + "String" => ("String", Static)
            fsharp + "Core.String" => ("String", Static)
            system + "Text.RegularExpressions.Regex" => ("RegExp", Static)
            fsharp + "Collections.Seq" => ("Seq", Static)
            fsharp + "Collections.Set" => ("Set", Static)
            fsharp + "Core.Choice" => ("Choice", Both)
            fsharp + "Control.Event" => ("Event", Both)
            fsharp + "Control.CommonExtensions" => ("Event", Both)
            fsharp + "Control.IDelegateEvent" => ("Event", Both)
        ]

open Util

let private coreLibPass com (info: Fable.ApplyInfo) =
    let checkStatic modName meth =
        // TODO: Add warning here
        if CoreLibMethods.staticMethods.ContainsKey(modName) |> not then None else
        if CoreLibMethods.staticMethods.[modName].Contains(meth)
        then Some meth
        else None
    match info.ownerFullName with
    | Patterns.DicContains CoreLibPass.mappings (modName, kind) ->
        match kind with
        | CoreLibPass.Both ->
            match info.methodName, info.methodKind, info.callee with
            | ".ctor", _, None | _, Fable.Constructor, None ->
                CoreLibCall(modName, None, true, deleg info info.args)
                |> makeCall com info.range info.returnType |> Some
            | _, Fable.Getter _, Some callee ->
                Fable.Apply(callee, [makeConst info.methodName], Fable.ApplyGet, info.returnType, info.range) |> Some
            | _, Fable.Setter _, Some callee ->
                Fable.Set(callee, Some (makeConst info.methodName), info.args.Head, info.range) |> Some
            | _, _, Some callee ->
                InstanceCall (callee, info.methodName, deleg info info.args)
                |> makeCall com info.range info.returnType |> Some
            | _, _, None when checkStatic modName info.methodName |> Option.isSome ->
                CoreLibCall(modName, Some info.methodName, false, staticArgs info.callee info.args |> deleg info)
                |> makeCall com info.range info.returnType |> Some
            | _ -> None
        | CoreLibPass.Static ->
            if info.methodName = ".ctor" then "create" else info.methodName
            |> checkStatic modName |> function
            | Some meth ->
                CoreLibCall(modName, Some meth, false, staticArgs info.callee info.args |> deleg info)
                |> makeCall com info.range info.returnType |> Some
            | None -> None
    | _ -> None

let tryReplace (com: ICompiler) (info: Fable.ApplyInfo) =
    match AstPass.tryReplace com info with
    | Some res -> Some res
    | None -> coreLibPass com info
