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

    let addError (com: ICompiler) (range: SourceLocation option) (warning: string) =
        com.AddLog(warning, Severity.Error, ?range=range, fileName=com.CurrentFile)

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
    let hasDoubleEvalRisk = function
        | IdentExpr _
        // TODO: Add Union and List Getters here?
        | Value(This _ | Null _ | UnitConstant | NumberConstant _
                    | StringConstant _ | BoolConstant _ | Enum _) -> false
        | Get(_,kind,_,_) ->
            match kind with
            // OptionValue has a runtime check
            | ListHead | ListTail | TupleGet _
            | UnionTag _ | UnionField _ -> false
            | RecordGet(fi,_) -> fi.IsMutable
            | _ -> true
        | _ -> true

    let makeIdent name =
        { Name = name
          Type = Any
          IsMutable = false
          Range = None }

    let makeTypedIdent typ name =
        { makeIdent name with Type = typ }

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

    let makeIndexGet range typ callee idx =
        Get(callee, IndexGet idx, typ, range)

    let makeFieldGet range typ callee field =
        Get(callee, FieldGet field, typ, range)

    let makeUntypedFieldGet callee field =
        Get(callee, FieldGet field, Any, None)

    let makeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType) |> Value

    let makeCall r t i (callee: Expr) args =
        Operation(Call(callee, None, args, i), t, r)

    let makeCallNoInfo r t (callee: Expr) args =
        let callInfo =
            { ArgTypes = []
              IsConstructor = false
              IsDynamic = false
              HasSpread = false
              HasThisArg = false }
        Operation(Call(callee, None, args, callInfo), t, r)

    /// Dynamic calls will uncurry its function arguments with unknown arity
    let makeCallDynamic r (applied: Expr) args =
        let callInfo =
            { ArgTypes = []
              IsConstructor = false
              IsDynamic = true
              HasSpread = false
              HasThisArg = false }
        Operation(Call(applied, None, args, callInfo), Fable.Any, r)

    let makeBoolConst (x: bool) = BoolConstant x |> Value
    let makeStrConst (x: string) = StringConstant x |> Value
    let makeIntConst (x: int) = NumberConstant (float x, Int32) |> Value
    let makeNumConst (x: float) = NumberConstant (float x, Float64) |> Value
    let makeDecConst (x: decimal) = NumberConstant (float x, Float64) |> Value

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
    let [<Literal>] dynamicApplicable = "Fable.Core.DynamicApplicable"
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
    let [<Literal>] enumerable = "System.Collections.Generic.IEnumerable`1"
    let [<Literal>] reference = "Microsoft.FSharp.Core.FSharpRef`1"
    let [<Literal>] printf = "Microsoft.FSharp.Core.PrintfModule"
    let [<Literal>] createEvent = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent"

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

    let standard =
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
