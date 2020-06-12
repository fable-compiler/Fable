namespace Fable.Transforms

[<RequireQualifiedAccess>]
module Atts =
    let [<Literal>] customEquality = "Microsoft.FSharp.Core.CustomEqualityAttribute" // typeof<CustomEqualityAttribute>.FullName
    let [<Literal>] customComparison = "Microsoft.FSharp.Core.CustomComparisonAttribute" // typeof<CustomComparisonAttribute>.FullName
    let [<Literal>] abstractClass = "Microsoft.FSharp.Core.AbstractClassAttribute" // typeof<AbstractClassAttribute>.FullName
    let [<Literal>] compiledName = "Microsoft.FSharp.Core.CompiledNameAttribute" // typeof<CompiledNameAttribute>.FullName
    let [<Literal>] entryPoint = "Microsoft.FSharp.Core.EntryPointAttribute" // typeof<Microsoft.FSharp.Core.EntryPointAttribute>.FullName
    let [<Literal>] sealed_ = "Microsoft.FSharp.Core.SealedAttribute" // typeof<Microsoft.FSharp.Core.SealedAttribute>.FullName
    let [<Literal>] noMangle = "Fable.Core.JsInterfaceAttribute" // typeof<Fable.Core.NoMangleAttribute>.FullName
    let [<Literal>] import = "Fable.Core.ImportAttribute" // typeof<Fable.Core.ImportAttribute>.FullName
    let [<Literal>] importAll = "Fable.Core.ImportAllAttribute" // typeof<Fable.Core.ImportAllAttribute>.FullName
    let [<Literal>] importDefault = "Fable.Core.ImportDefaultAttribute" // typeof<Fable.Core.ImportDefaultAttribute>.FullName
    let [<Literal>] importMember = "Fable.Core.ImportMemberAttribute" // typeof<Fable.Core.ImportMemberAttribute>.FullName
    let [<Literal>] global_ = "Fable.Core.GlobalAttribute" // typeof<Fable.Core.GlobalAttribute>.FullName
    let [<Literal>] emit            = "Fable.Core.EmitAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitMethod      = "Fable.Core.EmitMethodAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitConstructor = "Fable.Core.EmitConstructorAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitIndexer     = "Fable.Core.EmitIndexerAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitProperty    = "Fable.Core.EmitPropertyAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitDeclaration = "Fable.Core.EmitDeclarationAttribute" // typeof<Fable.Core.EmitDeclarationAttribute>.FullName
    let [<Literal>] erase = "Fable.Core.EraseAttribute" // typeof<Fable.Core.EraseAttribute>.FullName
    let [<Literal>] stringEnum = "Fable.Core.StringEnumAttribute" // typeof<Fable.Core.StringEnumAttribute>.FullName
    let [<Literal>] paramList = "Fable.Core.ParamListAttribute" // typeof<Fable.Core.ParamListAttribute>.FullName
    let [<Literal>] inject = "Fable.Core.InjectAttribute" // typeof<Fable.Core.InjectAttribute>.FullName

[<RequireQualifiedAccess>]
module Types =
    let [<Literal>] attribute = "System.Attribute"
    let [<Literal>] object = "System.Object"
    let [<Literal>] valueType = "System.ValueType"
    let [<Literal>] array = "System.Array"
    let [<Literal>] type_ = "System.Type"
    let [<Literal>] exception_ = "System.Exception"
    let [<Literal>] bool = "System.Boolean"
    let [<Literal>] char = "System.Char"
    let [<Literal>] string = "System.String"
    let [<Literal>] guid = "System.Guid"
    let [<Literal>] timespan = "System.TimeSpan"
    let [<Literal>] datetime = "System.DateTime"
    let [<Literal>] datetimeOffset = "System.DateTimeOffset"
    let [<Literal>] int8 = "System.SByte"
    let [<Literal>] uint8 = "System.Byte"
    let [<Literal>] int16 = "System.Int16"
    let [<Literal>] uint16 = "System.UInt16"
    let [<Literal>] int32 = "System.Int32"
    let [<Literal>] uint32 = "System.UInt32"
    let [<Literal>] int64 = "System.Int64"
    let [<Literal>] uint64 = "System.UInt64"
    let [<Literal>] float32 = "System.Single"
    let [<Literal>] float64 = "System.Double"
    let [<Literal>] decimal = "System.Decimal"
    let [<Literal>] bigint = "System.Numerics.BigInteger"
    let [<Literal>] regex = "System.Text.RegularExpressions.Regex"
    let [<Literal>] unit = "Microsoft.FSharp.Core.Unit"
    let [<Literal>] option = "Microsoft.FSharp.Core.FSharpOption`1"
    let [<Literal>] valueOption = "Microsoft.FSharp.Core.FSharpValueOption`1"
    let [<Literal>] result = "Microsoft.FSharp.Core.FSharpResult`2"
    let [<Literal>] matchFail = "Microsoft.FSharp.Core.MatchFailureException"
    let [<Literal>] choiceNonGeneric = "Microsoft.FSharp.Core.FSharpChoice"
    let [<Literal>] list = "Microsoft.FSharp.Collections.FSharpList`1"
    let [<Literal>] resizeArray = "System.Collections.Generic.List`1"
    let [<Literal>] dictionary = "System.Collections.Generic.Dictionary`2"
    let [<Literal>] idictionary = "System.Collections.Generic.IDictionary`2"
    let [<Literal>] hashset = "System.Collections.Generic.HashSet`1"
    let [<Literal>] iset = "System.Collections.Generic.ISet`1"
    let [<Literal>] keyValuePair = "System.Collections.Generic.KeyValuePair`2"
    let [<Literal>] fsharpMap = "Microsoft.FSharp.Collections.FSharpMap`2"
    let [<Literal>] fsharpSet = "Microsoft.FSharp.Collections.FSharpSet`1"
    let [<Literal>] ienumerableGeneric = "System.Collections.Generic.IEnumerable`1"
    let [<Literal>] ienumerable = "System.Collections.IEnumerable"
    let [<Literal>] ienumeratorGeneric = "System.Collections.Generic.IEnumerator`1"
    let [<Literal>] ienumerator = "System.Collections.IEnumerator"
    let [<Literal>] icollectionGeneric = "System.Collections.Generic.ICollection`1"
    let [<Literal>] icollection = "System.Collections.ICollection"
    let [<Literal>] iequatableGeneric = "System.IEquatable`1"
    let [<Literal>] iequatable = "System.IEquatable"
    let [<Literal>] icomparableGeneric = "System.IComparable`1"
    let [<Literal>] icomparable = "System.IComparable"
    let [<Literal>] idisposable = "System.IDisposable"
    let [<Literal>] reference = "Microsoft.FSharp.Core.FSharpRef`1"
    let [<Literal>] printfModule = "Microsoft.FSharp.Core.PrintfModule"
    let [<Literal>] printfFormat = "Microsoft.FSharp.Core.PrintfFormat"
    let [<Literal>] createEvent = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent"

    // Types compatible with Inject attribute
    let [<Literal>] comparer = "System.Collections.Generic.IComparer`1"
    let [<Literal>] equalityComparer = "System.Collections.Generic.IEqualityComparer`1"
    let [<Literal>] arrayCons = "Array.IArrayCons`1"
    let [<Literal>] typeResolver = "Fable.Core.ITypeResolver`1"
    let [<Literal>] adder = "Fable.Core.IGenericAdder`1"
    let [<Literal>] averager = "Fable.Core.IGenericAverager`1"

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
    let [<Literal>] divideByInt = "DivideByInt"

    let [<Literal>] equality = "op_Equality"
    let [<Literal>] inequality = "op_Inequality"
    let [<Literal>] lessThan = "op_LessThan"
    let [<Literal>] greaterThan = "op_GreaterThan"
    let [<Literal>] lessThanOrEqual = "op_LessThanOrEqual"
    let [<Literal>] greaterThanOrEqual = "op_GreaterThanOrEqual"

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

    let private addLog (com: ICompiler) inlinePath range msg severity =
        let printInlineSource refPath path r =
            let path = Path.getRelativeFileOrDirPath false refPath false path
            match r with
            | Some r -> sprintf "%s(%i,%i)" path r.start.line r.start.column
            | None -> path
        let rec buildInlinePath acc refPath r = function
            | [] ->
                (printInlineSource refPath com.CurrentFile r)::acc
                |> List.rev |> String.concat " < "
                |> (+) " - Inline call from "
            | (file,r2)::rest ->
                let acc = (printInlineSource refPath file r)::acc
                buildInlinePath acc refPath r2 rest
        let actualFile, msg =
            match inlinePath with
            | [] -> com.CurrentFile, msg
            | (file,r)::inlinePath -> file, msg + (buildInlinePath [] file r inlinePath)
        com.AddLog(msg, severity, ?range=range, fileName=actualFile)

    let addWarning (com: ICompiler) inlinePath range warning =
        addLog com inlinePath range warning Severity.Warning

    let addError (com: ICompiler) inlinePath range error =
        addLog com inlinePath range error Severity.Error

    let addErrorAndReturnNull (com: ICompiler) inlinePath range error =
        addLog com inlinePath range error Severity.Error
        AST.Fable.Value(AST.Fable.Null AST.Fable.Any, None)

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

    let inline (|ExprType|) (e: Expr) = e.Type
    let inline (|IdentType|) (id: Ident) = id.Type

    let (|NestedLambdaType|_|) t =
        let rec nestedLambda acc = function
            | FunctionType(LambdaType arg, returnType) ->
                nestedLambda (arg::acc) returnType
            | returnType -> Some(List.rev acc, returnType)
        match t with
        | FunctionType(LambdaType arg, returnType) -> nestedLambda [arg] returnType
        | _ -> None

    /// Only matches lambda immediately nested within each other
    let rec nestedLambda checkArity expr =
        let rec inner accArgs body name =
            match body with
            | Function(Lambda arg, body, None) ->
                inner (arg::accArgs) body name
            | _ -> List.rev accArgs, body, name
        match expr with
        | Function(Lambda arg, body, name) ->
            let args, body, name = inner [arg] body name
            if checkArity then
                match expr.Type with
                | NestedLambdaType(argTypes, _)
                    when List.sameLength args argTypes -> Some(args, body, name)
                | _ -> None
            else
                Some(args, body, name)
        | _ -> None

    let (|NestedLambdaWithSameArity|_|) expr =
        nestedLambda true expr

    /// Doesn't check the type of lambda body has same arity as discovered arguments
    let (|NestedLambda|_|) expr =
        nestedLambda false expr

    let (|NestedApply|_|) expr =
        let rec nestedApply r t accArgs applied =
            match applied with
            | Operation(CurriedApply(applied, args), _, _) ->
                nestedApply r t (args@accArgs) applied
            | _ -> Some(applied, accArgs, t, r)
        match expr with
        | Operation(CurriedApply(applied, args), t, r) ->
            nestedApply r t args applied
        | _ -> None

    let (|LambdaUncurriedAtCompileTime|_|) arity expr =
        let rec uncurryLambdaInner name accArgs remainingArity expr =
            if remainingArity = Some 0
            then Function(Delegate(List.rev accArgs), expr, name) |> Some
            else
                match expr, remainingArity with
                | Function(Lambda arg, body, name2), _ ->
                    let remainingArity = remainingArity |> Option.map (fun x -> x - 1)
                    uncurryLambdaInner (Option.orElse name2 name) (arg::accArgs) remainingArity body
                // If there's no arity expectation we can return the flattened part
                | _, None when List.isEmpty accArgs |> not ->
                    Function(Delegate(List.rev accArgs), expr, name) |> Some
                // We cannot flatten lambda to the expected arity
                | _, _ -> None
        match expr with
        // Uncurry also function options
        | Value(NewOption(Some expr, _), r) ->
            uncurryLambdaInner None [] arity expr
            |> Option.map (fun expr -> Value(NewOption(Some expr, expr.Type), r))
        | _ -> uncurryLambdaInner None [] arity expr

    let (|MaybeCasted|) = function
        | TypeCast(e,_) -> e
        | e -> e

    /// Try to uncurry lambdas at compile time in dynamic assignments
    let (|MaybeLambdaUncurriedAtCompileTime|) = function
        | MaybeCasted(LambdaUncurriedAtCompileTime None lambda) -> lambda
        | e -> e

    // Functions return yes because we don't want to duplicate them in the code
    // TODO: Improve this, see https://github.com/fable-compiler/Fable/issues/1659#issuecomment-445071965
    let rec canHaveSideEffects = function
        | Import _ -> false
        | Value((Null _ | UnitConstant | NumberConstant _ | StringConstant _ | BoolConstant _),_) -> false
        | Value(NewTuple exprs,_) ->
            (false, exprs) ||> List.fold (fun result e -> result || canHaveSideEffects e)
        | Value(EnumConstant(e, _),_) -> canHaveSideEffects e
        | IdentExpr id -> id.IsMutable
        | Get(e,kind,_,_) ->
            match kind with
            // OptionValue has a runtime check
            | ListHead | ListTail | TupleGet _
            | UnionTag | UnionField _ -> canHaveSideEffects e
            | FieldGet(_,isFieldMutable,_) ->
                if isFieldMutable then true
                else canHaveSideEffects e
            | _ -> true
        | _ -> true

    /// For unit, unresolved generics or nested options or unknown types,
    /// create a runtime wrapper. See fable-library/Option.ts for more info.
    let rec mustWrapOption = function
        | Any | Unit | GenericParam _ | Option _ -> true
        | _ -> false

    /// ATTENTION: Make sure the ident name will be unique within the file
    let makeIdentNonMangled name =
        { Name = name
          Type = Any
          Kind = CompilerGenerated
          IsMutable = false
          Range = None }

    /// Mangles ident name to prevent conflicts in the file
    let makeIdentUnique (com: ICompiler) name =
        com.GetUniqueVar(name) |> makeIdentNonMangled

    /// ATTENTION: Make sure the ident name will be unique within the file
    let makeTypedIdentNonMangled typ name =
        { Name = name
          Type = typ
          Kind = CompilerGenerated
          IsMutable = false
          Range = None }

    /// Mangles ident name to prevent conflicts in the file
    let makeTypedIdentUnique (com: ICompiler) typ name =
        com.GetUniqueVar(name) |> makeTypedIdentNonMangled typ

    /// ATTENTION: Make sure the ident name will be unique within the file
    let makeIdentExprNonMangled name =
        makeIdentNonMangled name |> IdentExpr

    let makeLoop range loopKind = Loop (loopKind, range)

    let makeBinOp range typ left right op =
        Operation(BinaryOperation(op, left, right), typ, range)

    let makeUnOp range typ arg op =
        Operation(UnaryOperation(op, arg), typ, range)

    let makeLogOp range left right op =
        Operation(LogicalOperation(op, left, right), Boolean, range)

    let makeEqOp range left right op =
        Operation(BinaryOperation(op, left, right), Boolean, range)

    let makeNull () =
        Value(Null Any, None)

    let makeValue r value =
        Value(value, r)

    let makeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType) |> makeValue None

    let makeDelegate args body =
        Function(Delegate args, body, None)

    let makeLambda (args: Ident list) (body: Expr) =
        (args, body) ||> List.foldBack (fun arg body ->
            Function(Lambda arg, body, None))

    let makeBoolConst (x: bool) = BoolConstant x |> makeValue None
    let makeStrConst (x: string) = StringConstant x |> makeValue None
    let makeIntConst (x: int) = NumberConstant (float x, Int32) |> makeValue None
    let makeFloatConst (x: float) = NumberConstant (x, Float64) |> makeValue None

    let makeCoreRef t memberName moduleName =
        Import(makeStrConst memberName, makeStrConst moduleName, Library, t, None)

    let makeCustomImport t (selector: string) (path: string) =
        Import(selector.Trim() |> makeStrConst, path.Trim() |> makeStrConst, CustomImport, t, None)

    let makeInternalImport (com: ICompiler) t (selector: string) (path: string) =
        let path = Path.getRelativeFileOrDirPath false com.CurrentFile false path
        Import(makeStrConst selector, makeStrConst path, Internal, t, None)

    let argInfo thisArg args argTypes =
        { ThisArg = thisArg
          Args = args
          SignatureArgTypes = argTypes
          Spread = NoSpread
          IsBaseCall = false
          IsSelfConstructorCall = false }

    let staticCall r t argInfo functionExpr =
        Operation(Call(StaticCall functionExpr, argInfo), t, r)

    let constructorCall r t argInfo consExpr =
        Operation(Call(ConstructorCall consExpr, argInfo), t, r)

    let instanceCall r t argInfo memb =
        Operation(Call(InstanceCall memb, argInfo), t, r)

    let getExpr r t left memb =
        Get(left, ExprGet memb, t, r)

    let get r t left membName =
        makeStrConst membName |> getExpr r t left

    let getNumberKindName kind =
        match kind with
        | Int8 -> "int8"
        | UInt8 -> "uint8"
        | Int16 -> "int16"
        | UInt16 -> "uint16"
        | Int32 -> "int32"
        | UInt32 -> "uint32"
        | Float32 -> "float32"
        | Float64 -> "float64"

    let getTypedArrayName (com: ICompiler) numberKind =
        match numberKind with
        | Int8 -> "Int8Array"
        | UInt8 -> if com.Options.clampByteArrays then "Uint8ClampedArray" else "Uint8Array"
        | Int16 -> "Int16Array"
        | UInt16 -> "Uint16Array"
        | Int32 -> "Int32Array"
        | UInt32 -> "Uint32Array"
        | Float32 -> "Float32Array"
        | Float64 -> "Float64Array"

    let argEquals (argIdents: Ident list) argExprs =
        if List.sameLength argIdents argExprs |> not then false
        else
            (true, List.zip argIdents argExprs)
            ||> List.fold (fun eq (id, expr) ->
                    if not eq then false
                    else
                        match expr with
                        | IdentExpr id2 -> id.Name = id2.Name
                        | _ -> false)

    let rec listEquals f li1 li2 =
        match li1, li2 with
        | [], [] -> true
        | h1::t1, h2::t2 -> f h1 h2 && listEquals f t1 t2
        | _ -> false

    /// When strict is false doesn't take generic params into account (e.g. when solving SRTP)
    let rec typeEquals strict typ1 typ2 =
        let entEquals (ent1: FSharp.Compiler.SourceCodeServices.FSharpEntity) gen1 (ent2: FSharp.Compiler.SourceCodeServices.FSharpEntity) gen2 =
            match ent1.TryFullName, ent2.TryFullName with
            | Some n1, Some n2 when n1 = n2 -> listEquals (typeEquals strict) gen1 gen2
            | _ -> false
        match typ1, typ2 with
        | Any, Any
        | Unit, Unit
        | Boolean, Boolean
        | Char, Char
        | String, String
        | Regex, Regex -> true
        | Number kind1, Number kind2 -> kind1 = kind2
        | Enum ent1, Enum ent2 -> entEquals ent1 [] ent2 []
        | Option t1, Option t2
        | Array t1, Array t2
        | List t1, List t2 -> typeEquals strict t1 t2
        | ErasedUnion ts1, ErasedUnion ts2
        | Tuple ts1, Tuple ts2 -> listEquals (typeEquals strict) ts1 ts2
        | FunctionType(LambdaType a1, t1), FunctionType(LambdaType a2, t2) ->
            typeEquals strict a1 a2 && typeEquals strict t1 t2
        | FunctionType(DelegateType as1, t1), FunctionType(DelegateType as2, t2) ->
            listEquals (typeEquals strict) as1 as2 && typeEquals strict t1 t2
        | DeclaredType(ent1, gen1), DeclaredType(ent2, gen2) ->
            match ent1.TryFullName, ent2.TryFullName with
            | Some n1, Some n2 when n1 = n2 -> listEquals (typeEquals strict) gen1 gen2
            | _ -> false
        | GenericParam _, _ | _, GenericParam _ when not strict -> true
        | GenericParam name1, GenericParam name2 -> name1 = name2
        | _ -> false

    let rec getTypeFullName prettify t =
        let getEntityFullName (ent: FSharp.Compiler.SourceCodeServices.FSharpEntity) gen =
            match ent.TryFullName with
            | None -> Naming.unknown
            | Some fullname when List.isEmpty gen -> fullname
            | Some fullname ->
                let gen = (List.map (getTypeFullName prettify) gen |> String.concat ",")
                let fullname =
                    if prettify then
                        match fullname with
                        | Types.result -> "Result"
                        | Naming.StartsWith Types.choiceNonGeneric _ -> "Choice"
                        | _ -> fullname // TODO: Prettify other types?
                    else fullname
                fullname + "[" + gen + "]"
        match t with
        | AnonymousRecordType _ -> ""
        | GenericParam name -> "'" + name
        | Enum ent -> getEntityFullName ent []
        | Regex    -> Types.regex
        | MetaType -> Types.type_
        | Unit    -> Types.unit
        | Boolean -> Types.bool
        | Char    -> Types.char
        | String  -> Types.string
        // TODO: Type info forErasedUnion?
        | ErasedUnion _ | Any -> Types.object
        | Number kind ->
            match kind with
            | Int8    -> Types.int8
            | UInt8   -> Types.uint8
            | Int16   -> Types.int16
            | UInt16  -> Types.uint16
            | Int32   -> Types.int32
            | UInt32  -> Types.uint32
            | Float32 -> Types.float32
            | Float64 -> Types.float64
        | FunctionType(LambdaType argType, returnType) ->
            let argType = getTypeFullName prettify argType
            let returnType = getTypeFullName prettify returnType
            if prettify
            then argType + " -> " + returnType
            else "Microsoft.FSharp.Core.FSharpFunc`2[" + argType + "," + returnType + "]"
        | FunctionType(DelegateType argTypes, returnType) ->
            sprintf "System.Func`%i[%s,%s]"
                (List.length argTypes + 1)
                (List.map (getTypeFullName prettify) argTypes |> String.concat ",")
                (getTypeFullName prettify returnType)
        | Tuple genArgs ->
            let genArgs = List.map (getTypeFullName prettify) genArgs
            if prettify
            then String.concat " * " genArgs
            else sprintf "System.Tuple`%i[%s]" (List.length genArgs) (String.concat "," genArgs)
        | Array gen ->
            (getTypeFullName prettify gen) + "[]"
        | Option gen ->
            let gen = getTypeFullName prettify gen
            if prettify then gen + " option" else Types.option + "[" + gen + "]"
        | List gen ->
            let gen = getTypeFullName prettify gen
            if prettify then gen + " list" else Types.list + "[" + gen + "]"
        | DeclaredType(ent, gen) ->
            getEntityFullName ent gen

    let addRanges (locs: SourceLocation option seq) =
        let addTwo (r1: SourceLocation option) (r2: SourceLocation option) =
            match r1, r2 with
            | Some r1, None -> Some r1
            | None, Some r2 -> Some r2
            | None, None -> None
            | Some r1, Some r2 -> Some(r1 + r2)
        (None, locs) ||> Seq.fold addTwo
