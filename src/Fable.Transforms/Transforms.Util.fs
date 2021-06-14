namespace Fable.Transforms

[<RequireQualifiedAccess>]
module Atts =
    let [<Literal>] customEquality = "Microsoft.FSharp.Core.CustomEqualityAttribute" // typeof<CustomEqualityAttribute>.FullName
    let [<Literal>] customComparison = "Microsoft.FSharp.Core.CustomComparisonAttribute" // typeof<CustomComparisonAttribute>.FullName
    let [<Literal>] abstractClass = "Microsoft.FSharp.Core.AbstractClassAttribute" // typeof<AbstractClassAttribute>.FullName
    let [<Literal>] compiledName = "Microsoft.FSharp.Core.CompiledNameAttribute" // typeof<CompiledNameAttribute>.FullName
    let [<Literal>] entryPoint = "Microsoft.FSharp.Core.EntryPointAttribute" // typeof<Microsoft.FSharp.Core.EntryPointAttribute>.FullName
    let [<Literal>] sealed_ = "Microsoft.FSharp.Core.SealedAttribute" // typeof<Microsoft.FSharp.Core.SealedAttribute>.FullName
    let [<Literal>] mangle = "Fable.Core.MangleAttribute" // typeof<Fable.Core.MangleAttribute>.FullName
    let [<Literal>] attachMembers = "Fable.Core.AttachMembersAttribute"
    let [<Literal>] import = "Fable.Core.Import"
    let [<Literal>] importAll = "Fable.Core.ImportAllAttribute" // typeof<Fable.Core.ImportAllAttribute>.FullName
    let [<Literal>] importDefault = "Fable.Core.ImportDefaultAttribute" // typeof<Fable.Core.ImportDefaultAttribute>.FullName
    let [<Literal>] importMember = "Fable.Core.ImportMemberAttribute" // typeof<Fable.Core.ImportMemberAttribute>.FullName
    let [<Literal>] global_ = "Fable.Core.GlobalAttribute" // typeof<Fable.Core.GlobalAttribute>.FullName
    let [<Literal>] emit = "Fable.Core.Emit"
    let [<Literal>] emitMethod = "Fable.Core.EmitMethodAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitConstructor = "Fable.Core.EmitConstructorAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitIndexer = "Fable.Core.EmitIndexerAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitProperty = "Fable.Core.EmitPropertyAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] erase = "Fable.Core.EraseAttribute" // typeof<Fable.Core.EraseAttribute>.FullName
    let [<Literal>] stringEnum = "Fable.Core.StringEnumAttribute" // typeof<Fable.Core.StringEnumAttribute>.FullName
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
    let [<Literal>] ireadonlydictionary = "System.Collections.Generic.IReadOnlyDictionary`2"
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
    let [<Literal>] arrayCons = "Array.Cons`1"
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
    open Fable.AST

    type InlinePath = {
        ToFile: string
        ToRange: SourceLocation option
        FromFile: string
        FromRange: SourceLocation option
    }

    let private addLog (com: Compiler) (inlinePath: InlinePath list) range msg severity =
        let printInlineSource fromPath (p: InlinePath) =
            let path = Path.getRelativeFileOrDirPath false fromPath false p.FromFile
            match p.FromRange with
            | Some r -> sprintf "%s(%i,%i)" path r.start.line r.start.column
            | None -> path
        let actualFile, msg =
            match inlinePath with
            | [] -> com.CurrentFile, msg
            | { ToFile = file }::_ ->
                let inlinePath =
                    inlinePath
                    |> List.map (printInlineSource file)
                    |> String.concat " < "
                file, msg + " - Inline call from " + inlinePath
        com.AddLog(msg, severity, ?range=range, fileName=actualFile)

    let addWarning (com: Compiler) inlinePath range warning =
        addLog com inlinePath range warning Severity.Warning

    let addError (com: Compiler) inlinePath range error =
        addLog com inlinePath range error Severity.Error

    let addWarningAndReturnNull (com: Compiler) inlinePath range error =
        addLog com inlinePath range error Severity.Warning
        AST.Fable.Value(AST.Fable.Null AST.Fable.Any, None)

    let addErrorAndReturnNull (com: Compiler) inlinePath range error =
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
    let inline (|ExprTypeAs|) (e: Expr) = e.Type, e
    let inline (|IdentType|) (id: Ident) = id.Type

    let (|NestedLambdaType|_|) t =
        let rec nestedLambda acc = function
            | LambdaType(arg, returnType) ->
                nestedLambda (arg::acc) returnType
            | returnType -> Some(List.rev acc, returnType)
        match t with
        | LambdaType(arg, returnType) -> nestedLambda [arg] returnType
        | _ -> None

    /// Only matches lambda immediately nested within each other
    let rec nestedLambda checkArity expr =
        let rec inner accArgs body name =
            match body with
            | Lambda(arg, body, None) ->
                inner (arg::accArgs) body name
            | _ -> List.rev accArgs, body, name
        match expr with
        | Lambda(arg, body, name) ->
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
            | CurriedApply(applied, args, _, _) ->
                nestedApply r t (args@accArgs) applied
            | _ -> Some(applied, accArgs, t, r)
        match expr with
        | CurriedApply(applied, args, t, r) ->
            nestedApply r t args applied
        | _ -> None

    let (|LambdaUncurriedAtCompileTime|_|) arity expr =
        let rec uncurryLambdaInner name accArgs remainingArity expr =
            if remainingArity = Some 0
            then Delegate(List.rev accArgs, expr, name) |> Some
            else
                match expr, remainingArity with
                | Lambda(arg, body, name2), _ ->
                    let remainingArity = remainingArity |> Option.map (fun x -> x - 1)
                    uncurryLambdaInner (Option.orElse name2 name) (arg::accArgs) remainingArity body
                // If there's no arity expectation we can return the flattened part
                | _, None when List.isEmpty accArgs |> not ->
                    Delegate(List.rev accArgs, expr, name) |> Some
                // We cannot flatten lambda to the expected arity
                | _, _ -> None
        match expr with
        // Uncurry also function options
        | Value(NewOption(Some expr, _), r) ->
            uncurryLambdaInner None [] arity expr
            |> Option.map (fun expr -> Value(NewOption(Some expr, expr.Type), r))
        | _ -> uncurryLambdaInner None [] arity expr

    let (|NestedRevLets|_|) expr =
        let rec inner bindings = function
            | Let(i,v, body) -> inner ((i,v)::bindings) body
            | body -> bindings, body
        match expr with
        | Let(i, v, body) -> inner [i, v] body |> Some
        | _ -> None

    let (|MaybeCasted|) e =
        let rec inner = function
            | TypeCast(e,_,_) -> inner e
            | e -> e
        inner e

    /// Try to uncurry lambdas at compile time in dynamic assignments
    let (|MaybeLambdaUncurriedAtCompileTime|) = function
        | MaybeCasted(LambdaUncurriedAtCompileTime None lambda) -> lambda
        | e -> e

    let (|StringConst|_|) = function
        | MaybeCasted(Value(StringConstant str, _)) -> Some str
        | _ -> None

    let (|BoolConst|_|) = function
        | MaybeCasted(Value(BoolConstant v, _)) -> Some v
        | _ -> None

    // TODO: Improve this, see https://github.com/fable-compiler/Fable/issues/1659#issuecomment-445071965
    let rec canHaveSideEffects = function
        | Import _ -> false
        | Lambda _ | Delegate _ -> false
        | TypeCast(e,_,_) -> canHaveSideEffects e
        | Value(value,_) ->
            match value with
            | ThisValue _ | BaseValue _ -> true
            | TypeInfo _ | Null _ | UnitConstant | NumberConstant _ | BoolConstant _
            | CharConstant _ | StringConstant _ | RegexConstant _  -> false
            | EnumConstant(e, _) -> canHaveSideEffects e
            | NewList(None,_) | NewOption(None,_) -> false
            | NewOption(Some e,_) -> canHaveSideEffects e
            | NewList(Some(h,t),_) -> canHaveSideEffects h || canHaveSideEffects t
            | NewTuple exprs
            | NewUnion(exprs,_,_,_) -> (false, exprs) ||> List.fold (fun result e -> result || canHaveSideEffects e)
            // Arrays can be mutable
            | NewArray _ | NewArrayFrom _ -> true
            | NewRecord _ | NewAnonymousRecord _ -> true
        | IdentExpr id -> id.IsMutable
        | Get(e,kind,_,_) ->
            match kind with
            // OptionValue has a runtime check
            | ListHead | ListTail | TupleIndex _
            | UnionTag | UnionField _ -> canHaveSideEffects e
            | FieldGet(_, isMutable) ->
                if isMutable then true
                else canHaveSideEffects e
            | _ -> true
        | _ -> true

    /// For unit, unresolved generics or nested options or unknown types,
    /// create a runtime wrapper. See fable-library/Option.ts for more info.
    let rec mustWrapOption = function
        | Any | Unit | GenericParam _ | Option _ -> true
        | _ -> false

    /// ATTENTION: Make sure the ident name is unique
    let makeTypedIdent typ name =
        { Name = name
          Type = typ
          IsCompilerGenerated = true
          IsThisArgument = false
          IsMutable = false
          Range = None }

    /// ATTENTION: Make sure the ident name is unique
    let makeIdent name =
        makeTypedIdent Any name

    /// ATTENTION: Make sure the ident name is unique
    let makeIdentExpr name =
        makeIdent name |> IdentExpr

    let makeTypedIdentExpr typ name =
        makeTypedIdent typ name |> IdentExpr

    let makeWhileLoop range guardExpr bodyExpr =
        WhileLoop (guardExpr, bodyExpr, range)

    let makeForLoop range isUp ident start limit body =
        ForLoop (ident, start, limit, body, isUp, range)

    let makeBinOp range typ left right op =
        Operation(Binary(op, left, right), typ, range)

    let makeUnOp range typ arg op =
        Operation(Unary(op, arg), typ, range)

    let makeLogOp range left right op =
        Operation(Logical(op, left, right), Boolean, range)

    let makeEqOp range left right op =
        Operation(Binary(op, left, right), Boolean, range)

    let makeNull () =
        Value(Null Any, None)

    let makeValue r value =
        Value(value, r)

    let makeArray elementType arrExprs =
        NewArray(arrExprs, elementType) |> makeValue None

    let makeDelegate args body =
        Delegate(args, body, None)

    let makeLambda (args: Ident list) (body: Expr) =
        (args, body) ||> List.foldBack (fun arg body ->
            Lambda(arg, body, None))

    let makeBoolConst (x: bool) = BoolConstant x |> makeValue None
    let makeStrConst (x: string) = StringConstant x |> makeValue None
    let makeIntConst (x: int) = NumberConstant (float x, Int32) |> makeValue None
    let makeFloatConst (x: float) = NumberConstant (x, Float64) |> makeValue None

    let getLibPath (com: Compiler) moduleName =
        com.LibraryDir + "/" + moduleName + ".js"

    let makeImportUserGenerated r t (selector: string) (path: string) =
        Import({ Selector = selector.Trim()
                 Path = path.Trim()
                 IsCompilerGenerated = false }, t, r)

    let makeImportCompilerGenerated t (selector: string) (path: string) =
        Import({ Selector = selector.Trim()
                 Path = path.Trim()
                 IsCompilerGenerated = true }, t, None)

    let makeImportLib (com: Compiler) t memberName moduleName =
        makeImportCompilerGenerated t memberName (getLibPath com moduleName)

    let makeImportInternal (com: Compiler) t (selector: string) (path: string) =
        Path.getRelativeFileOrDirPath false com.CurrentFile false path
        |> makeImportCompilerGenerated t selector

    let makeCallInfo thisArg args argTypes =
        { ThisArg = thisArg
          Args = args
          SignatureArgTypes = argTypes
          CallMemberInfo = None
          HasSpread = false
          IsJsConstructor = false }

    let emitJs r t args isStatement macro =
        let callInfo =
            { ThisArg = None
              Args = args
              SignatureArgTypes = []
              CallMemberInfo = None
              HasSpread = false
              IsJsConstructor = false }
        let emitInfo =
            { Macro = macro
              IsJsStatement = isStatement
              CallInfo = callInfo }
        Emit(emitInfo, t, r)

    let emitJsExpr r t args macro =
        emitJs r t args false macro

    let emitJsStatement r t args macro =
        emitJs r t args true macro

    let makeThrow r t err =
        emitJsStatement r t [err] "throw $0"

    let makeDebugger range =
        emitJsStatement range Unit [] "debugger"

    let destructureTupleArgs = function
        | [MaybeCasted(Value(UnitConstant,_))] -> []
        | [MaybeCasted(Value(NewTuple(args),_))] -> args
        | args -> args

    let makeCall r t argInfo calleeExpr =
        Call(calleeExpr, argInfo, t, r)

    let getExpr r t left memb =
        Get(left, ExprGet memb, t, r)

    let setExpr r left memb (value: Expr) =
        Set(left, ExprSet memb, value.Type, value, r)

    let getAttachedMemberWith r t callee membName =
        Get(callee, FieldGet(membName, true), t, r)

    let getAttachedMember (e: Expr) membName =
        getAttachedMemberWith e.Range Any e membName

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

    let getTypedArrayName (com: Compiler) numberKind =
        match numberKind with
        | Int8 -> "Int8Array"
        | UInt8 -> if com.Options.ClampByteArrays then "Uint8ClampedArray" else "Uint8Array"
        | Int16 -> "Int16Array"
        | UInt16 -> "Uint16Array"
        | Int32 -> "Int32Array"
        | UInt32 -> "Uint32Array"
        | Float32 -> "Float32Array"
        | Float64 -> "Float64Array"

    /// Used to compare arg idents of a lambda wrapping a function call
    let argEquals (argIdents: Ident list) argExprs =
        // When the lambda has a single unit arg, usually the method call has no args
        // so we ignore single unit args just in case
        let argIdents = match argIdents with [i] when i.Type = Unit -> [] | _ -> argIdents
        let argExprs = match argExprs with [Value(UnitConstant,_)] -> [] | _ -> argExprs

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
        match typ1, typ2 with
        | Any, Any
        | Unit, Unit
        | Boolean, Boolean
        | Char, Char
        | String, String
        | Regex, Regex -> true
        | Number kind1, Number kind2 -> kind1 = kind2
        | Enum ent1, Enum ent2 -> ent1 = ent2
        | Option t1, Option t2
        | Array t1, Array t2
        | List t1, List t2 -> typeEquals strict t1 t2
        | Tuple ts1, Tuple ts2 -> listEquals (typeEquals strict) ts1 ts2
        | LambdaType(a1, t1), LambdaType(a2, t2) ->
            typeEquals strict a1 a2 && typeEquals strict t1 t2
        | DelegateType(as1, t1), DelegateType(as2, t2) ->
            listEquals (typeEquals strict) as1 as2 && typeEquals strict t1 t2
        | DeclaredType(ent1, gen1), DeclaredType(ent2, gen2) ->
            ent1 = ent2 && listEquals (typeEquals strict) gen1 gen2
        | GenericParam _, _ | _, GenericParam _ when not strict -> true
        | GenericParam name1, GenericParam name2 -> name1 = name2
        | _ -> false

    let rec getTypeFullName prettify t =
        let getEntityFullName (entRef: EntityRef) gen =
            let fullname = entRef.FullName
            if List.isEmpty gen then fullname
            else
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
        | Any -> Types.object
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
        | LambdaType(argType, returnType) ->
            let argType = getTypeFullName prettify argType
            let returnType = getTypeFullName prettify returnType
            if prettify
            then argType + " -> " + returnType
            else "Microsoft.FSharp.Core.FSharpFunc`2[" + argType + "," + returnType + "]"
        | DelegateType(argTypes, returnType) ->
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
