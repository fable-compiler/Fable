namespace Fable.Transforms

[<AutoOpen>]
module Extensions =
    type System.Collections.Generic.Dictionary<'TKey,'TValue> with
        member dic.GetOrAdd(key, addFn) =
            match dic.TryGetValue(key) with
            | true, v -> v
            | false, _ ->
                let v = addFn()
                dic.Add(key, v)
                v
        member dic.AddOrUpdate(key, addFn, updateFn) =
            let v =
                match dic.TryGetValue(key) with
                | true, v ->
                    dic.Remove(key) |> ignore
                    updateFn key v
                | false, _ -> addFn key
            dic.Add(key, v)
            v

[<AutoOpen>]
module Log =
    open Fable

    let addWarning (com: ICompiler) (range: SourceLocation option) (warning: string) =
        com.AddLog(warning, Severity.Warning, ?range=range, fileName=com.CurrentFile)

    let addError (com: ICompiler) (range: SourceLocation option) (error: string) =
        com.AddLog(error, Severity.Error, ?range=range, fileName=com.CurrentFile)

    let addErrorAndReturnNull (com: ICompiler) (range: SourceLocation option) (error: string) =
        com.AddLog(error, Severity.Error, ?range=range, fileName=com.CurrentFile)
        AST.Fable.Null AST.Fable.Any |> AST.Fable.Value

    let attachRange (range: SourceLocation option) msg =
        match range with
        | Some range -> msg + " " + (string range)
        | None -> msg

    let attachRangeAndFile (range: SourceLocation option) (fileName: string) msg =
        match range with
        | Some range -> msg + " " + (string range) + " (" + fileName + ")"
        | None -> msg + " (" + fileName + ")"


[<AutoOpen>]
module AST =
    open Fable
    open Fable.AST
    open Fable.AST.Fable

    /// When referenced multiple times, is there a risk of double evaluation?
    let rec hasDoubleEvalRisk = function
        // Don't erase `this` binding as it may be called from a closure
        | Value(This _) -> false
        | IdentExpr id -> id.IsMutable
        | Value(Null _ | UnitConstant | NumberConstant _ | StringConstant _ | BoolConstant _ | Enum _) -> false
        | Value(NewTuple exprs) -> exprs |> List.exists hasDoubleEvalRisk
        | Get(_,kind,_,_) ->
            match kind with
            // OptionValue has a runtime check
            | ListHead | ListTail | TupleGet _
            | UnionTag _ | UnionField _ -> false
            | RecordGet(fi,_) -> fi.IsMutable
            | _ -> true
        | _ -> true

    /// For unit, unresolved generics, lists or nested options, create a runtime wrapper
    /// See fable-core/Option.ts for more info
    let rec mustWrapOption = function
        | Unit | GenericParam _ | Option _ | List _ -> true
        | _ -> false

    let makeIdent name =
        { Name = name
          Type = Any
          IsMutable = false
          IsThisArg = false
          IsCompilerGenerated = true
          Range = None }

    let makeTypedIdent typ name =
        { Name = name
          Type = typ
          IsMutable = false
          IsThisArg = false
          IsCompilerGenerated = true
          Range = None }

    let makeIdentExpr name =
        makeIdent name |> IdentExpr

    let makeLoop range loopKind = Loop (loopKind, range)

    let makeCoreRef t modname prop =
        Import(prop, modname, CoreLib, t)

    let makeImport t (selector: string) (path: string) =
        Import(selector.Trim(), path.Trim(), CustomImport, t)

    let makeBinOp range typ left right op =
        Operation(BinaryOperation(op, left, right), typ, range)

    let makeUnOp range typ arg op =
        Operation(UnaryOperation(op, arg), typ, range)

    let makeLogOp range left right op =
        Operation(LogicalOperation(op, left, right), Boolean, range)

    let makeEqOp range left right op =
        Operation(BinaryOperation(op, left, right), Boolean, range)

    let makeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType) |> Value

    let makeDelegate args body =
        Function(Delegate args, body, None)

    let makeBoolConst (x: bool) = BoolConstant x |> Value
    let makeStrConst (x: string) = StringConstant x |> Value
    let makeIntConst (x: int) = NumberConstant (float x, Int32) |> Value
    let makeNumConst (x: float) = NumberConstant (float x, Float64) |> Value
    let makeDecConst (x: decimal) = NumberConstant (float x, Float64) |> Value

    let argInfo thisArg args argTypes =
        { ThisArg = thisArg
          Args = args
          SignatureArgTypes = argTypes
          Spread = NoSpread
          IsSiblingConstructorCall = false }

    let staticCall r t argInfo functionExpr =
        Fable.Operation(Fable.Call(Fable.StaticCall functionExpr, argInfo), t, r)

    let constructorCall r t argInfo consExpr =
        Fable.Operation(Fable.Call(Fable.ConstructorCall consExpr, argInfo), t, r)

    let instanceCall r t argInfo memb =
        Fable.Operation(Fable.Call(Fable.InstanceCall memb, argInfo), t, r)

    let getExpr r t left memb =
        Fable.Get(left, ExprGet memb, t, r)

    let get r t left membName =
        makeStrConst membName |> getExpr r t left

    let getTypedArrayName (com: ICompiler) numberKind =
        match numberKind with
        | Int8 -> "Int8Array"
        | UInt8 -> if com.Options.clampByteArrays then "Uint8ClampedArray" else "Uint8Array"
        | Int16 -> "Int16Array"
        | UInt16 -> "Uint16Array"
        | Int32 -> "Int32Array"
        | UInt32 -> "Uint32Array"
        | Float32 -> "Float32Array"
        | Float64 | Decimal -> "Float64Array"

    let rec listEquals f li1 li2 =
        match li1, li2 with
        | [], [] -> true
        | h1::t1, h2::t2 -> f h1 h2 && listEquals f t1 t2
        | _ -> false

    let rec typeEquals typ1 typ2 =
        match typ1, typ2 with
        | Any, Any
        | Unit, Unit
        | Boolean, Boolean
        | Char, Char
        | String, String
        | Regex, Regex -> true
        | Number kind1, Number kind2 -> kind1 = kind2
        | EnumType(kind1, name1), EnumType(kind2, name2) -> kind1 = kind2 && name1 = name2
        | Option t1, Option t2
        | Array t1, Array t2
        | List t1, List t2 -> typeEquals t1 t2
        | ErasedUnion ts1, ErasedUnion ts2
        | Tuple ts1, Tuple ts2 -> listEquals typeEquals ts1 ts2
        | GenericParam n1, GenericParam n2 -> n1 = n2
        | FunctionType(LambdaType a1, t1), FunctionType(LambdaType a2, t2) ->
            typeEquals a1 a2 && typeEquals t1 t2
        | FunctionType(DelegateType as1, t1), FunctionType(DelegateType as2, t2) ->
            listEquals typeEquals as1 as2 && typeEquals t1 t2
        | DeclaredType(ent1, gen1), DeclaredType(ent2, gen2) ->
            match ent1.TryFullName, ent2.TryFullName with
            | Some n1, Some n2 when n1 = n2 -> listEquals typeEquals gen1 gen2
            | _ -> false
        | _ -> false

[<RequireQualifiedAccess>]
module Atts =
    let [<Literal>] abstractClass = "Microsoft.FSharp.Core.AbstractClassAttribute" // typeof<AbstractClassAttribute>.FullName
    let [<Literal>] compiledName = "Microsoft.FSharp.Core.CompiledNameAttribute" // typeof<CompiledNameAttribute>.FullName
    let [<Literal>] emit = "Fable.Core.EmitAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] import = "Fable.Core.ImportAttribute" // typeof<Fable.Core.ImportAttribute>.FullName
    let [<Literal>] global_ = "Fable.Core.GlobalAttribute" // typeof<Fable.Core.GlobalAttribute>.FullName
    let [<Literal>] erase = "Fable.Core.EraseAttribute" // typeof<Fable.Core.EraseAttribute>.FullName
    let [<Literal>] stringEnum = "Fable.Core.StringEnumAttribute" // typeof<Fable.Core.StringEnumAttribute>.FullName
    let [<Literal>] paramSeq = "Fable.Core.ParamSeqAttribute" // typeof<Fable.Core.ParamSeqAttribute>.FullName

[<RequireQualifiedAccess>]
module Types =
    let [<Literal>] attribute = "System.Attribute"
    let [<Literal>] object = "System.Object"
    let [<Literal>] bool = "System.Boolean"
    let [<Literal>] char = "System.Char"
    let [<Literal>] string = "System.String"
    let [<Literal>] guid = "System.Guid"
    let [<Literal>] timespan = "System.TimeSpan"
    let [<Literal>] datetime = "System.DateTime"
    let [<Literal>] datetimeOffset = "System.DateTimeOffset"
    let [<Literal>] decimal = "System.Decimal"
    let [<Literal>] regex = "System.Text.RegularExpressions.Regex"
    let [<Literal>] unit = "Microsoft.FSharp.Core.Unit"
    let [<Literal>] option = "Microsoft.FSharp.Core.FSharpOption`1"
    let [<Literal>] list = "Microsoft.FSharp.Collections.FSharpList`1"
    let [<Literal>] resizeArray = "System.Collections.Generic.List`1"
    let [<Literal>] dictionary = "System.Collections.Generic.Dictionary`2"
    let [<Literal>] hashset = "System.Collections.Generic.HashSet`1"
    let [<Literal>] fsharpMap = "Microsoft.FSharp.Collections.FSharpMap`2"
    let [<Literal>] fsharpSet = "Microsoft.FSharp.Collections.FSharpSet`1"
    let [<Literal>] enumerable = "System.Collections.Generic.IEnumerable`1"
    let [<Literal>] comparable = "System.IComparable"
    let [<Literal>] reference = "Microsoft.FSharp.Core.FSharpRef`1"
    let [<Literal>] printf = "Microsoft.FSharp.Core.PrintfModule"
    let [<Literal>] createEvent = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent"

    // Types compatible with Inject attribute
    let [<Literal>] comparer = "System.Collections.Generic.IComparer`1"
    let [<Literal>] arrayCons = "Array.IArrayCons`1"

[<RequireQualifiedAccess>]
module Operators =
    let [<Literal>] addition = "op_Addition"
    let [<Literal>] subtraction = "op_Subtraction"
    let [<Literal>] multiply = "op_Multiply"
    let [<Literal>] division = "op_Division"
    let [<Literal>] modulus = "op_Modulus"
    let [<Literal>] leftShift = "op_LeftShift"
    let [<Literal>] rightShift = "op_RightShift"
    let [<Literal>] bitwiseAnd = "op_BitwiseAnd"
    let [<Literal>] bitwiseOr = "op_BitwiseOr"
    let [<Literal>] exclusiveOr = "op_ExclusiveOr"
    let [<Literal>] booleanAnd = "op_BooleanAnd"
    let [<Literal>] booleanOr = "op_BooleanOr"
    let [<Literal>] logicalNot = "op_LogicalNot"
    let [<Literal>] unaryNegation = "op_UnaryNegation"
    // let [<Literal>] equality = "op_Equality"
    // let [<Literal>] inequality = "op_Inequality"
    // let [<Literal>] lessThan = "op_LessThan"
    // let [<Literal>] greaterThan = "op_GreaterThan"
    // let [<Literal>] lessThanOrEqual = "op_LessThanOrEqual"
    // let [<Literal>] greaterThanOrEqual = "op_GreaterThanOrEqual"

    let standardSet =
        set [ addition
              subtraction
              multiply
              division
              modulus
              leftShift
              rightShift
              bitwiseAnd
              bitwiseOr
              exclusiveOr
              booleanAnd
              booleanOr
              logicalNot
              unaryNegation ]

    // let equalitySet =
    //     set [ equality
    //           inequality
    //           lessThan
    //           greaterThan
    //           lessThanOrEqual
    //           greaterThanOrEqual ]
