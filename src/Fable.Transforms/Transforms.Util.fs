namespace Fable.Transforms

[<RequireQualifiedAccess>]
module Atts =
    let [<Literal>] noEquality = "Microsoft.FSharp.Core.NoEqualityAttribute" // typeof<NoEqualityAttribute>.FullName
    let [<Literal>] noComparison = "Microsoft.FSharp.Core.NoComparisonAttribute" // typeof<NoComparisonAttribute>.FullName
    let [<Literal>] customEquality = "Microsoft.FSharp.Core.CustomEqualityAttribute" // typeof<CustomEqualityAttribute>.FullName
    let [<Literal>] customComparison = "Microsoft.FSharp.Core.CustomComparisonAttribute" // typeof<CustomComparisonAttribute>.FullName
    let [<Literal>] abstractClass = "Microsoft.FSharp.Core.AbstractClassAttribute" // typeof<AbstractClassAttribute>.FullName
    let [<Literal>] compiledName = "Microsoft.FSharp.Core.CompiledNameAttribute" // typeof<CompiledNameAttribute>.FullName
    let [<Literal>] compiledValue = "Fable.Core.CompiledValueAttribute" // typeof<CompiledValueAttribute>.FullName
    let [<Literal>] entryPoint = "Microsoft.FSharp.Core.EntryPointAttribute" // typeof<Microsoft.FSharp.Core.EntryPointAttribute>.FullName
    let [<Literal>] sealed_ = "Microsoft.FSharp.Core.SealedAttribute" // typeof<Microsoft.FSharp.Core.SealedAttribute>.FullName
    let [<Literal>] mangle = "Fable.Core.MangleAttribute" // typeof<Fable.Core.MangleAttribute>.FullName
    let [<Literal>] attachMembers = "Fable.Core.AttachMembersAttribute"
    let [<Literal>] import = "Fable.Core.Import"
    let [<Literal>] importAttr = "Fable.Core.ImportAttribute" // typeof<Fable.Core.ImportAttribute>.FullName
    let [<Literal>] importAll = "Fable.Core.ImportAllAttribute" // typeof<Fable.Core.ImportAllAttribute>.FullName
    let [<Literal>] importDefault = "Fable.Core.ImportDefaultAttribute" // typeof<Fable.Core.ImportDefaultAttribute>.FullName
    let [<Literal>] importMember = "Fable.Core.ImportMemberAttribute" // typeof<Fable.Core.ImportMemberAttribute>.FullName
    let [<Literal>] global_ = "Fable.Core.GlobalAttribute" // typeof<Fable.Core.GlobalAttribute>.FullName
    let [<Literal>] emit = "Fable.Core.Emit"
    let [<Literal>] emitAttr = "Fable.Core.EmitAttribute" // typeof<Fable.Core.EmitAttribute>.FullName
    let [<Literal>] emitMethod = "Fable.Core.EmitMethodAttribute" // typeof<Fable.Core.EmitMethodAttribute>.FullName
    let [<Literal>] emitConstructor = "Fable.Core.EmitConstructorAttribute" // typeof<Fable.Core.EmitConstructorAttribute>.FullName
    let [<Literal>] emitIndexer = "Fable.Core.EmitIndexerAttribute" // typeof<Fable.Core.EmitIndexerAttribute>.FullName
    let [<Literal>] emitProperty = "Fable.Core.EmitPropertyAttribute" // typeof<Fable.Core.EmitPropertyAttribute>.FullName
    let [<Literal>] erase = "Fable.Core.EraseAttribute" // typeof<Fable.Core.EraseAttribute>.FullName
    let [<Literal>] tsTaggedUnion = "Fable.Core.TypeScriptTaggedUnionAttribute" // typeof<Fable.Core.TypeScriptTaggedUnionAttribute>.FullName
    let [<Literal>] stringEnum = "Fable.Core.StringEnumAttribute" // typeof<Fable.Core.StringEnumAttribute>.FullName
    let [<Literal>] inject = "Fable.Core.InjectAttribute" // typeof<Fable.Core.InjectAttribute>.FullName
    let [<Literal>] paramList = "Fable.Core.ParamListAttribute"// typeof<Fable.Core.ParamListAttribute>.FullName
    let [<Literal>] paramObject = "Fable.Core.ParamObjectAttribute"// typeof<Fable.Core.ParamObjectAttribute>.FullName
    let [<Literal>] referenceType = "Fable.Core.Rust.ReferenceTypeAttribute" // typeof<Fable.Core.PointerTypeAttribute>.FullName
    let [<Literal>] jsDecorator = "Fable.Core.JS.DecoratorAttribute" // typeof<Fable.Core.JS.DecoratorAttribute>.FullName
    let [<Literal>] jsReflectedDecorator = "Fable.Core.JS.ReflectedDecoratorAttribute" // typeof<Fable.Core.JS.ReflectedDecoratorAttribute>.FullName
    let [<Literal>] jsxComponent = "Fable.Core.JSX.ComponentAttribute" // typeof<Fable.Core.JSX.ComponentAttribute>.FullName
    let [<Literal>] pyDecorator = "Fable.Core.PY.DecoratorAttribute" // typeof<Fable.Core.PY.DecoratorAttribute>.FullName
    let [<Literal>] pyReflectedDecorator = "Fable.Core.PY.ReflectedDecoratorAttribute" // typeof<Fable.Core.PY.ReflectedDecoratorAttribute>.FullName
    let [<Literal>] dartIsConst = "Fable.Core.Dart.IsConstAttribute" // typeof<Fable.Core.Dart.IsConstAttribute>.FullName
    let [<Literal>] rustByRef = "Fable.Core.Rust.ByRefAttribute"// typeof<Fable.Core.Rust.ByRefAttribute>.FullName
    let [<Literal>] rustOuterAttr = "Fable.Core.Rust.OuterAttrAttribute"// typeof<Fable.Core.Rust.OuterAttrAttribute>.FullName
    let [<Literal>] rustInnerAttr = "Fable.Core.Rust.InnerAttrAttribute"// typeof<Fable.Core.Rust.InnerAttrAttribute>.FullName

[<RequireQualifiedAccess>]
module Types =
    let [<Literal>] attribute = "System.Attribute"
    let [<Literal>] object = "System.Object"
    let [<Literal>] valueType = "System.ValueType"
    let [<Literal>] array = "System.Array"
    let [<Literal>] type_ = "System.Type"
    let [<Literal>] exception_ = "System.Exception"
    let [<Literal>] systemException = "System.SystemException"
    let [<Literal>] timeoutException = "System.TimeoutException"
    let [<Literal>] bool = "System.Boolean"
    let [<Literal>] char = "System.Char"
    let [<Literal>] string = "System.String"
    let [<Literal>] guid = "System.Guid"
    let [<Literal>] timespan = "System.TimeSpan"
    let [<Literal>] datetime = "System.DateTime"
    let [<Literal>] datetimeOffset = "System.DateTimeOffset"
    let [<Literal>] dateOnly = "System.DateOnly"
    let [<Literal>] timeOnly = "System.TimeOnly"
    let [<Literal>] int8 = "System.SByte"
    let [<Literal>] uint8 = "System.Byte"
    let [<Literal>] int16 = "System.Int16"
    let [<Literal>] uint16 = "System.UInt16"
    let [<Literal>] int32 = "System.Int32"
    let [<Literal>] uint32 = "System.UInt32"
    let [<Literal>] int64 = "System.Int64"
    let [<Literal>] uint64 = "System.UInt64"
    let [<Literal>] nativeint = "System.IntPtr"
    let [<Literal>] unativeint = "System.UIntPtr"
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
    let [<Literal>] byref = "Microsoft.FSharp.Core.byref`1"
    let [<Literal>] byref2 = "Microsoft.FSharp.Core.byref`2"
    let [<Literal>] ievent = "Microsoft.FSharp.Control.IEvent`2"
    let [<Literal>] byrefKindIn = "Microsoft.FSharp.Core.ByRefKinds.In"
    let [<Literal>] byrefKindInOut = "Microsoft.FSharp.Core.ByRefKinds.InOut"
    let [<Literal>] byrefKindOut = "Microsoft.FSharp.Core.ByRefKinds.Out"
    let [<Literal>] choiceNonGeneric = "Microsoft.FSharp.Core.FSharpChoice"
    let [<Literal>] list = "Microsoft.FSharp.Collections.FSharpList`1"
    let [<Literal>] resizeArray = "System.Collections.Generic.List`1"
    let [<Literal>] dictionary = "System.Collections.Generic.Dictionary`2"
    let [<Literal>] idictionary = "System.Collections.Generic.IDictionary`2"
    let [<Literal>] ireadonlydictionary = "System.Collections.Generic.IReadOnlyDictionary`2"
    let [<Literal>] hashset = "System.Collections.Generic.HashSet`1"
    let [<Literal>] iset = "System.Collections.Generic.ISet`1"
    let [<Literal>] stack = "System.Collections.Generic.Stack`1"
    let [<Literal>] queue = "System.Collections.Generic.Queue`1"
    let [<Literal>] keyValuePair = "System.Collections.Generic.KeyValuePair`2"
    let [<Literal>] keyCollection = "System.Collections.Generic.Dictionary`2.KeyCollection"
    let [<Literal>] valueCollection = "System.Collections.Generic.Dictionary`2.ValueCollection"
    let [<Literal>] fsharpMap = "Microsoft.FSharp.Collections.FSharpMap`2"
    let [<Literal>] fsharpSet = "Microsoft.FSharp.Collections.FSharpSet`1"
    let [<Literal>] fsharpAsyncGeneric = "Microsoft.FSharp.Control.FSharpAsync`1"
    let [<Literal>] mailboxProcessor = "Microsoft.FSharp.Control.FSharpMailboxProcessor`1"
    let [<Literal>] taskBuilder = "Microsoft.FSharp.Control.TaskBuilder"
    let [<Literal>] taskBuilderModule = "Microsoft.FSharp.Control.TaskBuilderModule"
    let [<Literal>] task = "System.Threading.Tasks.Task"
    let [<Literal>] taskGeneric = "System.Threading.Tasks.Task`1"
    let [<Literal>] thread = "System.Threading.Thread"
    let [<Literal>] cancellationToken = "System.Threading.CancellationToken"
    let [<Literal>] ienumerableGeneric = "System.Collections.Generic.IEnumerable`1"
    let [<Literal>] ienumerable = "System.Collections.IEnumerable"
    let [<Literal>] ienumeratorGeneric = "System.Collections.Generic.IEnumerator`1"
    let [<Literal>] ienumerator = "System.Collections.IEnumerator"
    let [<Literal>] icollectionGeneric = "System.Collections.Generic.ICollection`1"
    let [<Literal>] icollection = "System.Collections.ICollection"
    let [<Literal>] iequatableGeneric = "System.IEquatable`1"
    let [<Literal>] icomparableGeneric = "System.IComparable`1"
    let [<Literal>] icomparable = "System.IComparable"
    let [<Literal>] iStructuralEquatable = "System.Collections.IStructuralEquatable"
    let [<Literal>] iStructuralComparable = "System.Collections.IStructuralComparable"
    let [<Literal>] idisposable = "System.IDisposable"
    let [<Literal>] iformattable = "System.IFormattable"
    let [<Literal>] iobserverGeneric = "System.IObserver`1"
    let [<Literal>] iobservableGeneric = "System.IObservable`1"
    let [<Literal>] refCell = "Microsoft.FSharp.Core.FSharpRef`1"
    let [<Literal>] printfModule = "Microsoft.FSharp.Core.PrintfModule"
    let [<Literal>] printfFormat = "Microsoft.FSharp.Core.PrintfFormat"
    let [<Literal>] createEvent = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent"
    let [<Literal>] equalityComparer = "System.Collections.IEqualityComparer"

    // Types compatible with Inject attribute (fable library)
    let [<Literal>] comparer = "System.Collections.Generic.IComparer`1"    
    let [<Literal>] equalityComparerGeneric = "System.Collections.Generic.IEqualityComparer`1"                                       
    let [<Literal>] arrayCons = "Array.Cons`1"
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

    let compareSet =
        set [ equality; "Eq"
              inequality; "Neq"
              lessThan; "Lt"
              lessThanOrEqual; "Lte"
              greaterThan; "Gt"
              greaterThanOrEqual; "Gte" ]

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
            | Some r -> $"%s{path}(%i{r.start.line},%i{r.start.column})"
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
        Fable.Value(Fable.Null Fable.Any, None)

    let addErrorAndReturnNull (com: Compiler) inlinePath range error =
        addLog com inlinePath range error Severity.Error
        Fable.Value(Fable.Null Fable.Any, None)

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
    let inline (|EntFullName|) (e: EntityRef) = e.FullName

    let (|NestedLambdaType|_|) t =
        let rec nestedLambda acc = function
            | LambdaType(arg, returnType) ->
                nestedLambda (arg::acc) returnType
            | returnType -> Some(List.rev acc, returnType)
        match t with
        | LambdaType(arg, returnType) -> nestedLambda [arg] returnType
        | _ -> None

    /// In lambdas with tuple arguments, F# compiler deconstructs the tuple before the next nested lambda.
    /// This makes it harder to uncurry lambdas, so we try to move the bindings to the inner lambda.
    let flattenLambdaBodyWithTupleArgs (arg: Ident) (body: Expr) =
        let rec flattenBindings accBindings (tupleArg: Ident) (body: Expr) =
            match body with
            | Lambda(arg, body, info) ->
                let body =
                    (body, accBindings) ||> List.fold (fun body (id, value) ->
                        Let(id, value, body))
                Lambda(arg, body, info) |> Some
            | Let(id, (Get(IdentExpr tupleIdent, TupleIndex _, _, _) as value), body)
                when tupleIdent.Name = tupleArg.Name ->
                    flattenBindings ((id, value)::accBindings) tupleArg body
            | _ -> None

        match arg.Type with
        | Tuple _ ->
            flattenBindings [] arg body
            |> Option.defaultValue body
        | _ -> body

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
        let rec uncurryLambdaInner (name: string option) accArgs remainingArity expr =
            if remainingArity = Some 0 then
                Delegate(List.rev accArgs, expr, name, Tags.empty) |> Some
            else
                match expr, remainingArity with
                | Lambda(arg, body, name2), _ ->
                    let remainingArity = remainingArity |> Option.map (fun x -> x - 1)
                    uncurryLambdaInner (Option.orElse name2 name) (arg::accArgs) remainingArity body
                // If there's no arity expectation we can return the flattened part
                | _, None when List.isEmpty accArgs |> not ->
                    Delegate(List.rev accArgs, expr, name, Tags.empty) |> Some
                // We cannot flatten lambda to the expected arity
                | _, _ -> None
        match expr with
        // Uncurry also function options
        | Value(NewOption(Some expr, _, isStruct), r) ->
            uncurryLambdaInner None [] arity expr
            |> Option.map (fun expr -> Value(NewOption(Some expr, expr.Type, isStruct), r))
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
            | TypeCast(e,_) -> inner e
            | e -> e
        inner e

    let (|MaybeOption|) e =
        match e with
        | Option(e, _) -> e
        | e -> e

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

    let (|NumberConst|_|) = function
        | MaybeCasted(Value(NumberConstant(value, kind, _), _)) -> Some(value, kind)
        | _ -> None

    let (|NullConst|_|) = function
        | MaybeCasted(Value(Null _, _)) -> Some()
        | _ -> None

    // TODO: Improve this, see https://github.com/fable-compiler/Fable/issues/1659#issuecomment-445071965
    // This is mainly used for inlining so a computation is understood as a side effect too
    // (because we don't want to duplicate or change the order of execution)
    let rec canHaveSideEffects = function
        | Import _ -> false
        | Lambda _ | Delegate _ -> false
        | TypeCast(e,_) ->
            match Compiler.Language with
            | Dart -> true
            | _ -> canHaveSideEffects e
        | Value(value,_) ->
            match value with
            | ThisValue _ | BaseValue _ -> true
            | TypeInfo _ | Null _ | UnitConstant | NumberConstant _
            | BoolConstant _ | CharConstant _ | StringConstant _ | RegexConstant _  -> false
            | NewList(None,_) | NewOption(None,_,_) -> false
            | NewOption(Some e,_,_) -> canHaveSideEffects e
            | NewList(Some(h,t),_) -> canHaveSideEffects h || canHaveSideEffects t
            | StringTemplate(_,_,exprs)
            | NewTuple(exprs,_)
            | NewUnion(exprs,_,_,_) -> List.exists canHaveSideEffects exprs
            | NewArray(newKind, _, kind) ->
                match kind, newKind with
                | ImmutableArray, ArrayFrom expr -> canHaveSideEffects expr
                | ImmutableArray, ArrayValues exprs -> List.exists canHaveSideEffects exprs
                | _, ArrayAlloc _
                | _, ArrayValues [] -> false
                | _ -> true
            | NewRecord _ | NewAnonymousRecord _ -> true
        | IdentExpr id -> id.IsMutable
        | Get(e,kind,_,_) ->
            match kind with
            | OptionValue ->
                match Compiler.Language with
                | Dart -> canHaveSideEffects e
                // Other languages include a runtime check for options
                | _ -> true
            | ListHead | ListTail | TupleIndex _
            | UnionTag | UnionField _ -> canHaveSideEffects e
            | FieldGet info ->
                if info.CanHaveSideEffects then true
                else canHaveSideEffects e
            | ExprGet _ -> true
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

    let makeNullTyped t =
        Value(Null t, None)

    let makeNull () =
        Value(Null Any, None)

    let makeNone t =
        Value(NewOption(None, t, false), None)

    let makeValue r value =
        Value(value, r)

    let makeTypeInfo r t =
        TypeInfo(t, Tags.empty) |> makeValue r

    let makeTypeDefinitionInfo r t =
        let t =
            match t with
            | Option(_, isStruct) -> Option(Any, isStruct)
            | Array(_, kind) -> Array(Any, kind)
            | List _ -> List Any
            | Tuple(genArgs, isStruct) ->
                Tuple(genArgs |> List.map (fun _ -> Any), isStruct)
            | DeclaredType(ent, genArgs) ->
                let genArgs = genArgs |> List.map (fun _ -> Any)
                DeclaredType(ent, genArgs)
            // TODO: Do something with FunctionType and ErasedUnion?
            | t -> t
        makeTypeInfo r t

    let makeTuple r isStruct values =
        Value(NewTuple(values, isStruct), r)

    let makeResizeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType, ResizeArray) |> makeValue None

    let makeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType, MutableArray) |> makeValue None

    let makeArrayWithRange r elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType, MutableArray) |> makeValue r

    let makeDelegate args body =
        Delegate(args, body, None, Tags.empty)

    let makeLambda (args: Ident list) (body: Expr) =
        (args, body) ||> List.foldBack (fun arg body ->
            Lambda(arg, body, None))

    let makeBoolConst (x: bool) = BoolConstant x |> makeValue None
    let makeStrConst (x: string) = StringConstant x |> makeValue None
    let makeIntConst (x: int) = NumberConstant (x, Int32, NumberInfo.Empty) |> makeValue None
    let makeFloatConst (x: float) = NumberConstant (x, Float64, NumberInfo.Empty) |> makeValue None

    let makeRegexConst r (pattern: string) flags =
        let flags = RegexGlobal::RegexUnicode::flags // .NET regex are always global & unicode
        RegexConstant(pattern, flags) |> makeValue r

    let makeConstFromObj (value: obj) =
        match value with
        | :? bool as x -> BoolConstant x |> makeValue None
        | :? string as x -> StringConstant x |> makeValue None
        | :? char as x -> CharConstant x |> makeValue None
        // Integer types
        | :? int8 as x -> NumberConstant(x, Int8, NumberInfo.Empty) |> makeValue None
        | :? uint8 as x -> NumberConstant(x, UInt8, NumberInfo.Empty) |> makeValue None
        | :? int16 as x -> NumberConstant(x, Int16, NumberInfo.Empty) |> makeValue None
        | :? uint16 as x -> NumberConstant(x, UInt16, NumberInfo.Empty) |> makeValue None
        | :? int32 as x -> NumberConstant(x, Int32, NumberInfo.Empty) |> makeValue None
        | :? uint32 as x -> NumberConstant(x, UInt32, NumberInfo.Empty) |> makeValue None
        | :? int64 as x -> NumberConstant(x, Int64, NumberInfo.Empty) |> makeValue None
        | :? uint64 as x -> NumberConstant(x, UInt64, NumberInfo.Empty) |> makeValue None
        // Float types
        | :? float32 as x -> NumberConstant(x, Float32, NumberInfo.Empty) |> makeValue None
        | :? float as x -> NumberConstant(x, Float64, NumberInfo.Empty) |> makeValue None
        | :? decimal as x -> NumberConstant(x, Decimal, NumberInfo.Empty) |> makeValue None
        | _ -> FableError $"Cannot create expression for object {value} (%s{value.GetType().FullName})" |> raise

    let makeTypeConst r (typ: Type) (value: obj) =
        match typ, value with
        | Boolean, (:? bool as x) -> BoolConstant x |> makeValue r
        | String, (:? string as x) -> StringConstant x |> makeValue r
        | Char, (:? char as x) -> CharConstant x |> makeValue r
        | Number(kind, info), x -> NumberConstant(x, kind, info) |> makeValue r
        | Unit, _ -> UnitConstant |> makeValue r
        // Arrays with small data type (ushort, byte) are represented
        // in F# AST as BasicPatterns.Const
        | Array (Number(kind, uom), arrayKind), (:? (byte[]) as arr) ->
            let values = arr |> Array.map (fun x -> NumberConstant (x, kind, uom) |> makeValue None) |> Seq.toList
            NewArray (ArrayValues values, Number(kind, uom), arrayKind) |> makeValue r
        | Array (Number(kind, uom), arrayKind), (:? (uint16[]) as arr) ->
            let values = arr |> Array.map (fun x -> NumberConstant (x, kind, uom) |> makeValue None) |> Seq.toList
            NewArray (ArrayValues values, Number(kind, uom), arrayKind) |> makeValue r
        | _ -> FableError $"Unexpected type %A{typ} for literal {value} (%s{value.GetType().FullName})" |> raise

    let getLibPath (com: Compiler) (moduleName: string) =
        match com.Options.Language with
        | Python ->
            // Python modules should be all lower case without any dots (PEP8)
            let moduleName' = moduleName |> Naming.applyCaseRule Fable.Core.CaseRules.SnakeCase |> (fun str -> str.Replace(".", "_"))
            com.LibraryDir + "/" + moduleName' + ".py"
        | Rust -> com.LibraryDir + "/" + moduleName + ".rs"
        | Dart -> com.LibraryDir + "/" + moduleName + ".dart"
        | _ -> com.LibraryDir + "/" + moduleName + ".js"

    let makeImportUserGenerated r t (selector: string) (path: string) =
        Import({ Selector = selector.Trim()
                 Path = path.Trim()
                 Kind = UserImport false }, t, r)

    let makeImportLib (com: Compiler) t memberName moduleName =
        let selector =
            match com.Options.Language with
            | Rust -> moduleName + "_::" + memberName //TODO: fix when imports change
            | _ -> memberName
        Import({ Selector = selector
                 Path = getLibPath com moduleName
                 Kind = LibraryImport }, t, None)

    let private makeInternalImport (com: Compiler) t (selector: string) (path: string) kind =
        let path =
            if com.CurrentFile = path then "./" + Path.GetFileName(path)
            else Path.getRelativeFileOrDirPath false com.CurrentFile false path
        Import({ Selector = selector; Path = path; Kind = kind }, t, None)

    let makeInternalMemberImport com t isInstance (selector: string) (path: string) =
        MemberImport(isInstance, path) |> makeInternalImport com t selector path

    let makeInternalClassImport com (selector: string) (path: string) =
        ClassImport(path) |> makeInternalImport com Any selector path

    let makeCallInfo thisArg args sigArgTypes =
        CallInfo.Create(?thisArg=thisArg, args=args, sigArgTypes=sigArgTypes)

    let emit r t args isStatement macro =
        let emitInfo =
            { Macro = macro
              IsStatement = isStatement
              CallInfo = CallInfo.Create(args=args) }
        Emit(emitInfo, t, r)

    let emitExpr r t args macro =
        emit r t args false macro

    let emitStatement r t args macro =
        emit r t args true macro

    let makeThrow r t (err: Expr) =
        Extended(Throw(Some err, t), r)

    let makeDebugger range =
        Extended(Debugger, range)

    let destructureTupleArgs = function
        | [MaybeCasted(Value(UnitConstant,_))] -> []
        | [MaybeCasted(Value(NewTuple(args,_),_))] -> args
        | args -> args

    let makeCall r t callInfo calleeExpr =
        Call(calleeExpr, callInfo, t, r)

    let getExpr r t left memb =
        Get(left, ExprGet memb, t, r)

    let getOptionValue r t e =
        Get(e, OptionValue, t, r)

    let setExpr r left memb (value: Expr) =
        Set(left, ExprSet memb, value.Type, value, r)

    let getImmutableFieldWith r t callee membName =
        Get(callee, FieldInfo.Create(membName), t, r)

    let getFieldWith r t callee membName =
        Get(callee, FieldInfo.Create(membName, maybeCalculated=true), t, r)

    let getField (e: Expr) membName =
        getFieldWith e.Range Any e membName

    let setField r callee membName (value: Expr) =
        Set(callee, FieldSet membName, value.Type, value, r)

    let getNumberKindName kind =
        match kind with
        | Int8 -> "int8"
        | UInt8 -> "uint8"
        | Int16 -> "int16"
        | UInt16 -> "uint16"
        | Int32 -> "int32"
        | UInt32 -> "uint32"
        | Int64 -> "int64"
        | UInt64 -> "uint64"
        | BigInt  -> "bigint"
        | NativeInt -> "nativeint"
        | UNativeInt -> "unativeint"
        | Float32 -> "float32"
        | Float64 -> "float64"
        | Decimal -> "decimal"

    type ParamsInfo = {|
        NamedIndex: int option
        Parameters: Parameter list
        HasSpread: bool
    |}

    let tryGetParamsInfo (com: Compiler) (callInfo: CallInfo): ParamsInfo option =
        callInfo.MemberRef
        |> Option.bind com.TryGetMember
        |> function
        | None -> None
        // ParamObject/NamedParams attribute is not compatible with arg spread
        | Some memberInfo when memberInfo.HasSpread ->
            {| NamedIndex = None
               HasSpread = true
               Parameters = List.concat memberInfo.CurriedParameterGroups |}
            |> Some
        | Some memberInfo ->
            let parameters = List.concat memberInfo.CurriedParameterGroups
            {| HasSpread = false
               Parameters = parameters
               NamedIndex = parameters |> List.tryFindIndex (fun p -> p.IsNamed) |}
            |> Some

    let splitNamedArgs (args: Expr list) (info: ParamsInfo) =
        match info.NamedIndex with
        | None -> args, []
        | Some index when index > args.Length || index > info.Parameters.Length -> args, []
        | Some index ->
            let args, namedValues = List.splitAt index args
            let namedKeys = List.skip index info.Parameters |> List.truncate namedValues.Length
            args, List.zipSafe namedKeys namedValues

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
        | MetaType, MetaType
        | Any, Any
        | Unit, Unit
        | Boolean, Boolean
        | Char, Char
        | String, String
        | Regex, Regex -> true
        | Number(kind1, info1), Number(kind2, info2) -> kind1 = kind2 && info1 = info2
        | Option(t1, isStruct1), Option(t2, isStruct2) -> isStruct1 = isStruct2 && typeEquals strict t1 t2
        | Array(t1, kind1), Array(t2, kind2) -> kind1 = kind2 && typeEquals strict t1 t2
        | List t1, List t2 -> typeEquals strict t1 t2
        | Tuple(ts1, isStruct1), Tuple(ts2, isStruct2) -> isStruct1 = isStruct2 && listEquals (typeEquals strict) ts1 ts2
        | LambdaType(a1, t1), LambdaType(a2, t2) ->
            typeEquals strict a1 a2 && typeEquals strict t1 t2
        | DelegateType(as1, t1), DelegateType(as2, t2) ->
            listEquals (typeEquals strict) as1 as2 && typeEquals strict t1 t2
        | DeclaredType(ent1, gen1), DeclaredType(ent2, gen2) ->
            ent1 = ent2 && listEquals (typeEquals strict) gen1 gen2
        | GenericParam _, _ | _, GenericParam _ when not strict -> true
        | GenericParam(name=name1), GenericParam(name=name2) -> name1 = name2
        // Field names must be already sorted
        | AnonymousRecordType(fields1, gen1, isStruct1), AnonymousRecordType(fields2, gen2, isStruct2) ->
            fields1.Length = fields2.Length
            && Array.zip fields1 fields2 |> Array.forall (fun (f1, f2) -> f1 = f2)
            && listEquals (typeEquals strict) gen1 gen2
            && isStruct1 = isStruct2
        | _ -> false

    let rec getEntityFullName prettify (entRef: EntityRef) gen =
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

    and getNumberFullName prettify kind info =
        let getKindName = function
            | Int8    -> Types.int8
            | UInt8   -> Types.uint8
            | Int16   -> Types.int16
            | UInt16  -> Types.uint16
            | Int32   -> Types.int32
            | UInt32  -> Types.uint32
            | Int64   -> Types.int64
            | UInt64  -> Types.uint64
            | BigInt  -> Types.bigint
            | NativeInt -> Types.nativeint
            | UNativeInt -> Types.unativeint
            | Float32 -> Types.float32
            | Float64 -> Types.float64
            | Decimal -> Types.decimal
        match info with
        | NumberInfo.Empty -> getKindName kind
        | NumberInfo.IsMeasure uom -> getKindName kind + "[" + uom + "]"
        | NumberInfo.IsEnum ent -> getEntityFullName prettify ent []

    and getTypeFullName prettify t =
        match t with
        | Measure fullname -> fullname
        | AnonymousRecordType _ -> ""
        | GenericParam(name=name) -> "'" + name
        | Regex    -> Types.regex
        | MetaType -> Types.type_
        | Unit    -> Types.unit
        | Boolean -> Types.bool
        | Char    -> Types.char
        | String  -> Types.string
        | Any -> Types.object
        | Number(kind, info) -> getNumberFullName prettify kind info
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
        | Tuple(genArgs, isStruct) ->
            let genArgs = List.map (getTypeFullName prettify) genArgs
            if prettify
            then (if isStruct then "struct " else "") + String.concat " * " genArgs
            else
                let isStruct = if isStruct then "Value" else ""
                let genArgsLength = List.length genArgs
                let genArgs = String.concat "," genArgs
                $"System.{isStruct}Tuple`{genArgsLength}[{genArgs}]"
        | Array(gen, _kind) -> // TODO: Check kind
            (getTypeFullName prettify gen) + "[]"
        | Option(gen, isStruct) ->
            let gen = getTypeFullName prettify gen
            if prettify then gen + " " + (if isStruct then "v" else "") + "option"
            else (if isStruct then Types.valueOption else Types.option) + "[" + gen + "]"
        | List gen ->
            let gen = getTypeFullName prettify gen
            if prettify then gen + " list" else Types.list + "[" + gen + "]"
        | DeclaredType(ent, gen) ->
            getEntityFullName prettify ent gen

    let addRanges (locs: SourceLocation option seq) =
        let addTwo (r1: SourceLocation option) (r2: SourceLocation option) =
            match r1, r2 with
            | Some r1, None -> Some r1
            | None, Some r2 -> Some r2
            | None, None -> None
            | Some r1, Some r2 -> Some(r1 + r2)
        (None, locs) ||> Seq.fold addTwo

    let visit f e =
        match e with
        | Unresolved _ -> e // Unresolved expressions must be matched explicitly
        | IdentExpr _ -> e
        | TypeCast(e, t) -> TypeCast(f e, t)
        | Import(info, t, r) ->
            Import({ info with Selector = info.Selector
                               Path = info.Path }, t, r)
        | Extended(kind, r) ->
            match kind with
            | Curry(e, arity) -> Extended(Curry(f e, arity), r)
            | Throw(e, t) -> Extended(Throw(Option.map f e, t), r)
            | Debugger -> e
        | Value(kind, r) ->
            match kind with
            | ThisValue _ | BaseValue _
            | TypeInfo _ | Null _ | UnitConstant
            | BoolConstant _ | CharConstant _ | StringConstant _
            | NumberConstant _ | RegexConstant _ -> e
            | StringTemplate(tag, parts, exprs) -> StringTemplate(tag, parts, List.map f exprs) |> makeValue r
            | NewOption(e, t, isStruct) -> NewOption(Option.map f e, t, isStruct) |> makeValue r
            | NewTuple(exprs, isStruct) -> NewTuple(List.map f exprs, isStruct) |> makeValue r
            | NewArray(ArrayValues exprs, t, i) -> NewArray(List.map f exprs |> ArrayValues, t, i) |> makeValue r
            | NewArray(ArrayFrom expr, t, i) -> NewArray(f expr |> ArrayFrom, t, i) |> makeValue r
            | NewArray(ArrayAlloc expr, t, i) -> NewArray(f expr |> ArrayAlloc, t, i) |> makeValue r
            | NewList(ht, t) ->
                let ht = ht |> Option.map (fun (h,t) -> f h, f t)
                NewList(ht, t) |> makeValue r
            | NewRecord(exprs, ent, genArgs) ->
                NewRecord(List.map f exprs, ent, genArgs) |> makeValue r
            | NewAnonymousRecord(exprs, ent, genArgs, isStruct) ->
                NewAnonymousRecord(List.map f exprs, ent, genArgs, isStruct) |> makeValue r
            | NewUnion(exprs, uci, ent, genArgs) ->
                NewUnion(List.map f exprs, uci, ent, genArgs) |> makeValue r
        | Test(e, kind, r) -> Test(f e, kind, r)
        | Lambda(arg, body, name) -> Lambda(arg, f body, name)
        | Delegate(args, body, name, tag) -> Delegate(args, f body, name, tag)
        | ObjectExpr(members, t, baseCall) ->
            let baseCall = Option.map f baseCall
            let members = members |> List.map (fun m -> { m with Body = f m.Body })
            ObjectExpr(members, t, baseCall)
        | CurriedApply(callee, args, t, r) ->
            CurriedApply(f callee, List.map f args, t, r)
        | Call(callee, info, t, r) ->
            let info = { info with ThisArg = Option.map f info.ThisArg
                                   Args = List.map f info.Args }
            Call(f callee, info, t, r)
        | Emit(info, t, r) ->
            let callInfo =
                { info.CallInfo with ThisArg = Option.map f info.CallInfo.ThisArg
                                     Args = List.map f info.CallInfo.Args }
            Emit({ info with CallInfo = callInfo }, t, r)
        | Operation(kind, t, r) ->
            match kind with
            | Unary(operator, operand) ->
                Operation(Unary(operator, f operand), t, r)
            | Binary(op, left, right) ->
                Operation(Binary(op, f left, f right), t, r)
            | Logical(op, left, right) ->
                Operation(Logical(op, f left, f right), t, r)
        | Get(e, kind, t, r) ->
            match kind with
            | ListHead | ListTail | OptionValue | TupleIndex _ | UnionTag
            | UnionField _ | FieldGet _ -> Get(f e, kind, t, r)
            | ExprGet e2 -> Get(f e, ExprGet(f e2), t, r)
        | Sequential exprs -> Sequential(List.map f exprs)
        | Let(ident, value, body) -> Let(ident, f value, f body)
        | LetRec(bs, body) ->
            let bs = bs |> List.map (fun (i,e) -> i, f e)
            LetRec(bs, f body)
        | IfThenElse(cond, thenExpr, elseExpr, r) ->
            IfThenElse(f cond, f thenExpr, f elseExpr, r)
        | Set(e, kind, t, v, r) ->
            match kind with
            | ExprSet e2 -> Set(f e, ExprSet(f e2), t, f v, r)
            | FieldSet _ | ValueSet -> Set(f e, kind, t, f v, r)
        | WhileLoop(e1, e2, r) -> WhileLoop(f e1, f e2, r)
        | ForLoop(i, e1, e2, e3, up, r) -> ForLoop(i, f e1, f e2, f e3, up, r)
        | TryCatch(body, catch, finalizer, r) ->
            TryCatch(f body,
                     Option.map (fun (i, e) -> i, f e) catch,
                     Option.map f finalizer, r)
        | DecisionTree(expr, targets) ->
            let targets = targets |> List.map (fun (idents, v) -> idents, f v)
            DecisionTree(f expr, targets)
        | DecisionTreeSuccess(idx, boundValues, t) ->
            DecisionTreeSuccess(idx, List.map f boundValues, t)

    let rec visitFromInsideOut f e =
        visit (visitFromInsideOut f) e |> f

    let rec visitFromOutsideIn (f: Expr->Expr option) e =
        match f e with
        | Some e -> e
        | None -> visit (visitFromOutsideIn f) e
