namespace Fable.Transforms

open System

[<RequireQualifiedAccess>]
module Atts =
    [<Literal>]
    let noEquality = "Microsoft.FSharp.Core.NoEqualityAttribute" // typeof<NoEqualityAttribute>.FullName

    [<Literal>]
    let customEquality = "Microsoft.FSharp.Core.CustomEqualityAttribute" // typeof<CustomEqualityAttribute>.FullName

    [<Literal>]
    let referenceEquality = "Microsoft.FSharp.Core.ReferenceEqualityAttribute" // typeof<ReferenceEqualityAttribute>.FullName

    [<Literal>]
    let structuralEquality = "Microsoft.FSharp.Core.StructuralEqualityAttribute" // typeof<StructuralEqualityAttribute>.FullName

    [<Literal>]
    let noComparison = "Microsoft.FSharp.Core.NoComparisonAttribute" // typeof<NoComparisonAttribute>.FullName

    [<Literal>]
    let customComparison = "Microsoft.FSharp.Core.CustomComparisonAttribute" // typeof<CustomComparisonAttribute>.FullName

    [<Literal>]
    let structuralComparison = "Microsoft.FSharp.Core.StructuralComparisonAttribute" // typeof<StructuralComparisonAttribute>.FullName

    [<Literal>]
    let abstractClass = "Microsoft.FSharp.Core.AbstractClassAttribute" // typeof<AbstractClassAttribute>.FullName

    [<Literal>]
    let compiledName = "Microsoft.FSharp.Core.CompiledNameAttribute" // typeof<CompiledNameAttribute>.FullName

    [<Literal>]
    let compiledValue = "Fable.Core.CompiledValueAttribute" // typeof<CompiledValueAttribute>.FullName

    [<Literal>]
    let entryPoint = "Microsoft.FSharp.Core.EntryPointAttribute" // typeof<Microsoft.FSharp.Core.EntryPointAttribute>.FullName

    [<Literal>]
    let sealed_ = "Microsoft.FSharp.Core.SealedAttribute" // typeof<Microsoft.FSharp.Core.SealedAttribute>.FullName

    [<Literal>]
    let mangle = "Fable.Core.MangleAttribute" // typeof<Fable.Core.MangleAttribute>.FullName

    [<Literal>]
    let attachMembers = "Fable.Core.AttachMembersAttribute"

    [<Literal>]
    let import = "Fable.Core.Import"

    [<Literal>]
    let importAttr = "Fable.Core.ImportAttribute" // typeof<Fable.Core.ImportAttribute>.FullName

    [<Literal>]
    let importAll = "Fable.Core.ImportAllAttribute" // typeof<Fable.Core.ImportAllAttribute>.FullName

    [<Literal>]
    let importDefault = "Fable.Core.ImportDefaultAttribute" // typeof<Fable.Core.ImportDefaultAttribute>.FullName

    [<Literal>]
    let importMember = "Fable.Core.ImportMemberAttribute" // typeof<Fable.Core.ImportMemberAttribute>.FullName

    [<Literal>]
    let exportDefault = "Fable.Core.ExportDefaultAttribute" // typeof<Fable.Core.ExportDefaultAttribute>.FullName

    [<Literal>]
    let global_ = "Fable.Core.GlobalAttribute" // typeof<Fable.Core.GlobalAttribute>.FullName

    [<Literal>]
    let emit = "Fable.Core.Emit"

    [<Literal>]
    let emitAttr = "Fable.Core.EmitAttribute" // typeof<Fable.Core.EmitAttribute>.FullName

    [<Literal>]
    let emitMethod = "Fable.Core.EmitMethodAttribute" // typeof<Fable.Core.EmitMethodAttribute>.FullName

    [<Literal>]
    let emitConstructor = "Fable.Core.EmitConstructorAttribute" // typeof<Fable.Core.EmitConstructorAttribute>.FullName

    [<Literal>]
    let emitIndexer = "Fable.Core.EmitIndexerAttribute" // typeof<Fable.Core.EmitIndexerAttribute>.FullName

    [<Literal>]
    let emitProperty = "Fable.Core.EmitPropertyAttribute" // typeof<Fable.Core.EmitPropertyAttribute>.FullName

    [<Literal>]
    let erase = "Fable.Core.EraseAttribute" // typeof<Fable.Core.EraseAttribute>.FullName

    [<Literal>]
    let tsTaggedUnion = "Fable.Core.TypeScriptTaggedUnionAttribute" // typeof<Fable.Core.TypeScriptTaggedUnionAttribute>.FullName

    [<Literal>]
    let stringEnum = "Fable.Core.StringEnumAttribute" // typeof<Fable.Core.StringEnumAttribute>.FullName

    [<Literal>]
    let inject = "Fable.Core.InjectAttribute" // typeof<Fable.Core.InjectAttribute>.FullName

    [<Literal>]
    let paramList = "Fable.Core.ParamListAttribute" // typeof<Fable.Core.ParamListAttribute>.FullName

    [<Literal>]
    let paramObject = "Fable.Core.ParamObjectAttribute" // typeof<Fable.Core.ParamObjectAttribute>.FullName

    [<Literal>]
    let jsDecorator = "Fable.Core.JS.DecoratorAttribute" // typeof<Fable.Core.JS.DecoratorAttribute>.FullName

    [<Literal>]
    let jsReflectedDecorator = "Fable.Core.JS.ReflectedDecoratorAttribute" // typeof<Fable.Core.JS.ReflectedDecoratorAttribute>.FullName

    [<Literal>]
    let jsxComponent = "Fable.Core.JSX.ComponentAttribute" // typeof<Fable.Core.JSX.ComponentAttribute>.FullName

    [<Literal>]
    let pyDecorator = "Fable.Core.Py.DecoratorAttribute" // typeof<Fable.Core.Py.DecoratorAttribute>.FullName

    [<Literal>]
    let pyReflectedDecorator = "Fable.Core.Py.ReflectedDecoratorAttribute" // typeof<Fable.Core.Py.ReflectedDecoratorAttribute>.FullName

    [<Literal>]
    let dartIsConst = "Fable.Core.Dart.IsConstAttribute" // typeof<Fable.Core.Dart.IsConstAttribute>.FullName

    [<Literal>]
    let rustByRef = "Fable.Core.Rust.ByRefAttribute" // typeof<Fable.Core.Rust.ByRefAttribute>.FullName

    [<Literal>]
    let rustAsync = "Fable.Core.Rust.AsyncAttribute" // typeof<Fable.Core.Rust.AsyncAttribute>.FullName

    [<Literal>]
    let rustConst = "Fable.Core.Rust.ConstAttribute" // typeof<Fable.Core.Rust.ConstAttribute>.FullName

    [<Literal>]
    let rustExtern = "Fable.Core.Rust.ExternAttribute" // typeof<Fable.Core.Rust.ExternAttribute>.FullName

    [<Literal>]
    let rustUnsafe = "Fable.Core.Rust.UnsafeAttribute" // typeof<Fable.Core.Rust.UnsafeAttribute>.FullName

    [<Literal>]
    let rustOuterAttr = "Fable.Core.Rust.OuterAttrAttribute" // typeof<Fable.Core.Rust.OuterAttrAttribute>.FullName

    [<Literal>]
    let rustInnerAttr = "Fable.Core.Rust.InnerAttrAttribute" // typeof<Fable.Core.Rust.InnerAttrAttribute>.FullName

    [<Literal>]
    let referenceType = "Fable.Core.Rust.ReferenceTypeAttribute" // typeof<Fable.Core.ReferenceTypeAttribute>.FullName

[<RequireQualifiedAccess>]
module Types =
    [<Literal>]
    let attribute = "System.Attribute"

    [<Literal>]
    let object = "System.Object"

    [<Literal>]
    let valueType = "System.ValueType"

    [<Literal>]
    let array = "System.Array"

    [<Literal>]
    let type_ = "System.Type"

    [<Literal>]
    let enum_ = "System.Enum"

    [<Literal>]
    let nullable = "System.Nullable`1"

    [<Literal>]
    let exception_ = "System.Exception"

    [<Literal>]
    let systemException = "System.SystemException"

    [<Literal>]
    let timeoutException = "System.TimeoutException"

    [<Literal>]
    let bool = "System.Boolean"

    [<Literal>]
    let char = "System.Char"

    [<Literal>]
    let string = "System.String"

    [<Literal>]
    let guid = "System.Guid"

    [<Literal>]
    let timespan = "System.TimeSpan"

    [<Literal>]
    let datetime = "System.DateTime"

    [<Literal>]
    let datetimeOffset = "System.DateTimeOffset"

    [<Literal>]
    let dateOnly = "System.DateOnly"

    [<Literal>]
    let timeOnly = "System.TimeOnly"

    [<Literal>]
    let int8 = "System.SByte"

    [<Literal>]
    let uint8 = "System.Byte"

    [<Literal>]
    let int16 = "System.Int16"

    [<Literal>]
    let uint16 = "System.UInt16"

    [<Literal>]
    let int32 = "System.Int32"

    [<Literal>]
    let uint32 = "System.UInt32"

    [<Literal>]
    let int64 = "System.Int64"

    [<Literal>]
    let uint64 = "System.UInt64"

    [<Literal>]
    let int128 = "System.Int128"

    [<Literal>]
    let uint128 = "System.UInt128"

    [<Literal>]
    let nativeint = "System.IntPtr"

    [<Literal>]
    let unativeint = "System.UIntPtr"

    [<Literal>]
    let float16 = "System.Half"

    [<Literal>]
    let float32 = "System.Single"

    [<Literal>]
    let float64 = "System.Double"

    [<Literal>]
    let decimal = "System.Decimal"

    [<Literal>]
    let bigint = "System.Numerics.BigInteger"

    [<Literal>]
    let regex = "System.Text.RegularExpressions.Regex"

    [<Literal>]
    let regexMatch = "System.Text.RegularExpressions.Match"

    [<Literal>]
    let regexGroup = "System.Text.RegularExpressions.Group"

    [<Literal>]
    let regexCapture = "System.Text.RegularExpressions.Capture"

    [<Literal>]
    let regexMatchCollection = "System.Text.RegularExpressions.MatchCollection"

    [<Literal>]
    let regexGroupCollection = "System.Text.RegularExpressions.GroupCollection"

    [<Literal>]
    let regexCaptureCollection = "System.Text.RegularExpressions.CaptureCollection"

    [<Literal>]
    let unit = "Microsoft.FSharp.Core.Unit"

    [<Literal>]
    let option = "Microsoft.FSharp.Core.FSharpOption`1"

    [<Literal>]
    let valueOption = "Microsoft.FSharp.Core.FSharpValueOption`1"

    [<Literal>]
    let result = "Microsoft.FSharp.Core.FSharpResult`2"

    [<Literal>]
    let matchFail = "Microsoft.FSharp.Core.MatchFailureException"

    [<Literal>]
    let byref = "Microsoft.FSharp.Core.byref`1"

    [<Literal>]
    let byref2 = "Microsoft.FSharp.Core.byref`2"

    [<Literal>]
    let ievent2 = "Microsoft.FSharp.Control.IEvent`2"

    [<Literal>]
    let byrefKindIn = "Microsoft.FSharp.Core.ByRefKinds.In"

    [<Literal>]
    let byrefKindInOut = "Microsoft.FSharp.Core.ByRefKinds.InOut"

    [<Literal>]
    let byrefKindOut = "Microsoft.FSharp.Core.ByRefKinds.Out"

    [<Literal>]
    let choiceNonGeneric = "Microsoft.FSharp.Core.FSharpChoice"

    [<Literal>]
    let list = "Microsoft.FSharp.Collections.FSharpList`1"

    [<Literal>]
    let resizeArray = "System.Collections.Generic.List`1"

    [<Literal>]
    let dictionary = "System.Collections.Generic.Dictionary`2"

    [<Literal>]
    let idictionary = "System.Collections.Generic.IDictionary`2"

    [<Literal>]
    let ireadonlydictionary = "System.Collections.Generic.IReadOnlyDictionary`2"

    [<Literal>]
    let hashset = "System.Collections.Generic.HashSet`1"

    [<Literal>]
    let iset = "System.Collections.Generic.ISet`1"

    [<Literal>]
    let stack = "System.Collections.Generic.Stack`1"

    [<Literal>]
    let queue = "System.Collections.Generic.Queue`1"

    [<Literal>]
    let keyValuePair = "System.Collections.Generic.KeyValuePair`2"

    [<Literal>]
    let keyCollection = "System.Collections.Generic.Dictionary`2.KeyCollection"

    [<Literal>]
    let valueCollection = "System.Collections.Generic.Dictionary`2.ValueCollection"

    [<Literal>]
    let fsharpMap = "Microsoft.FSharp.Collections.FSharpMap`2"

    [<Literal>]
    let fsharpSet = "Microsoft.FSharp.Collections.FSharpSet`1"

    [<Literal>]
    let fsharpAsyncGeneric = "Microsoft.FSharp.Control.FSharpAsync`1"

    [<Literal>]
    let mailboxProcessor = "Microsoft.FSharp.Control.FSharpMailboxProcessor`1"

    [<Literal>]
    let taskBuilder = "Microsoft.FSharp.Control.TaskBuilder"

    [<Literal>]
    let taskBuilderModule = "Microsoft.FSharp.Control.TaskBuilderModule"

    [<Literal>]
    let task = "System.Threading.Tasks.Task"

    [<Literal>]
    let taskGeneric = "System.Threading.Tasks.Task`1"

    [<Literal>]
    let thread = "System.Threading.Thread"

    [<Literal>]
    let cancellationToken = "System.Threading.CancellationToken"

    [<Literal>]
    let ienumerableGeneric = "System.Collections.Generic.IEnumerable`1"

    [<Literal>]
    let ienumerable = "System.Collections.IEnumerable"

    [<Literal>]
    let ienumeratorGeneric = "System.Collections.Generic.IEnumerator`1"

    [<Literal>]
    let ienumerator = "System.Collections.IEnumerator"

    [<Literal>]
    let icollectionGeneric = "System.Collections.Generic.ICollection`1"

    [<Literal>]
    let icollection = "System.Collections.ICollection"

    [<Literal>]
    let iequatableGeneric = "System.IEquatable`1"

    [<Literal>]
    let icomparableGeneric = "System.IComparable`1"

    [<Literal>]
    let icomparable = "System.IComparable"

    [<Literal>]
    let icomparer = "System.Collections.IComparer"

    [<Literal>]
    let iequalityComparer = "System.Collections.IEqualityComparer"

    [<Literal>]
    let iStructuralEquatable = "System.Collections.IStructuralEquatable"

    [<Literal>]
    let iStructuralComparable = "System.Collections.IStructuralComparable"

    [<Literal>]
    let idisposable = "System.IDisposable"

    [<Literal>]
    let iformattable = "System.IFormattable"

    [<Literal>]
    let iformatProvider = "System.IFormatProvider"

    [<Literal>]
    let iobserverGeneric = "System.IObserver`1"

    [<Literal>]
    let iobservableGeneric = "System.IObservable`1"

    [<Literal>]
    let refCell = "Microsoft.FSharp.Core.FSharpRef`1"

    [<Literal>]
    let printfModule = "Microsoft.FSharp.Core.PrintfModule"

    [<Literal>]
    let printfFormat = "Microsoft.FSharp.Core.PrintfFormat"

    [<Literal>]
    let createEvent =
        "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent"

    [<Literal>]
    let measureProduct2 = "Microsoft.FSharp.Core.CompilerServices.MeasureProduct`2"

    [<Literal>]
    let measureOne = "Microsoft.FSharp.Core.CompilerServices.MeasureOne"

    [<Literal>]
    let measureInverse = "Microsoft.FSharp.Core.CompilerServices.MeasureInverse`1"

    // Types compatible with Inject attribute (fable library)
    [<Literal>]
    let icomparerGeneric = "System.Collections.Generic.IComparer`1"

    [<Literal>]
    let iequalityComparerGeneric = "System.Collections.Generic.IEqualityComparer`1"

    [<Literal>]
    let arrayCons = "Array.Cons`1"

    [<Literal>]
    let adder = "Fable.Core.IGenericAdder`1"

    [<Literal>]
    let averager = "Fable.Core.IGenericAverager`1"

[<RequireQualifiedAccess>]
module Operators =
    [<Literal>]
    let addition = "op_Addition"

    [<Literal>]
    let subtraction = "op_Subtraction"

    [<Literal>]
    let multiply = "op_Multiply"

    [<Literal>]
    let division = "op_Division"

    [<Literal>]
    let modulus = "op_Modulus"

    [<Literal>]
    let leftShift = "op_LeftShift"

    [<Literal>]
    let rightShift = "op_RightShift"

    [<Literal>]
    let bitwiseAnd = "op_BitwiseAnd"

    [<Literal>]
    let bitwiseOr = "op_BitwiseOr"

    [<Literal>]
    let exclusiveOr = "op_ExclusiveOr"

    [<Literal>]
    let booleanAnd = "op_BooleanAnd"

    [<Literal>]
    let booleanOr = "op_BooleanOr"

    [<Literal>]
    let logicalNot = "op_LogicalNot"

    [<Literal>]
    let unaryNegation = "op_UnaryNegation"

    [<Literal>]
    let unaryPlus = "op_UnaryPlus"

    [<Literal>]
    let divideByInt = "DivideByInt"

    [<Literal>]
    let equality = "op_Equality"

    [<Literal>]
    let inequality = "op_Inequality"

    [<Literal>]
    let lessThan = "op_LessThan"

    [<Literal>]
    let greaterThan = "op_GreaterThan"

    [<Literal>]
    let lessThanOrEqual = "op_LessThanOrEqual"

    [<Literal>]
    let greaterThanOrEqual = "op_GreaterThanOrEqual"

    let standardSet =
        set
            [
                addition
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
                unaryNegation
                unaryPlus
            ]

    let compareSet =
        set
            [
                equality
                "Eq"
                inequality
                "Neq"
                lessThan
                "Lt"
                lessThanOrEqual
                "Lte"
                greaterThan
                "Gt"
                greaterThanOrEqual
                "Gte"
            ]

[<AutoOpen>]
module Extensions =
    type System.Collections.Generic.Dictionary<'TKey, 'TValue> with

        member dic.GetOrAdd(key, addFn) =
            match dic.TryGetValue(key) with
            | true, v -> v
            | false, _ ->
                let v = addFn ()
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

    type InlinePath =
        {
            ToFile: string
            ToRange: SourceLocation option
            FromFile: string
            FromRange: SourceLocation option
        }

    let private addLog (com: Compiler) (inlinePath: InlinePath list) (range: SourceLocation option) msg severity =
        let printInlineSource fromPath (p: InlinePath) =
            let path = Path.getRelativeFileOrDirPath false fromPath false p.FromFile

            match p.FromRange with
            | Some r -> $"%s{path}(%i{r.start.line},%i{r.start.column})"
            | None -> path

        let actualFile, msg =
            match inlinePath with
            | { ToFile = file } :: _ ->
                let inlinePath =
                    inlinePath |> List.map (printInlineSource file) |> String.concat " < "

                file, msg + " - Inline call from " + inlinePath
            | [] -> range |> Option.bind (fun r -> r.File) |> Option.defaultValue com.CurrentFile, msg

        com.AddLog(msg, severity, ?range = range, fileName = actualFile)

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
        | Some range -> msg + " " + (string<SourceLocation> range)
        | None -> msg

    let attachRangeAndFile (range: SourceLocation option) (fileName: string) msg =
        match range with
        | Some range -> msg + " " + (string<SourceLocation> range) + " (" + fileName + ")"
        | None -> msg + " (" + fileName + ")"


[<AutoOpen>]
module AST =
    open Fable
    open Fable.AST
    open Fable.AST.Fable

    let inline (|ExprType|) (e: Expr) = e.Type
    let inline (|ExprTypeAs|) (e: Expr) = e.Type, e
    let inline (|IdentType|) (id: Ident) = id.Type
    let inline (|EntFullName|) (e: Entity) = e.FullName
    let inline (|EntRefFullName|) (e: EntityRef) = e.FullName

    let (|DeclaredTypeFullName|_|) =
        function
        | DeclaredType(entRef, _) -> Some entRef.FullName
        | _ -> None

    let rec uncurryLambdaType maxArity (revArgTypes: Type list) (returnType: Type) =
        match returnType with
        | LambdaType(argType, returnType) when maxArity <> 0 ->
            uncurryLambdaType (maxArity - 1) (argType :: revArgTypes) returnType
        | t -> List.rev revArgTypes, t

    let (|NestedLambdaType|_|) =
        function
        | LambdaType(argType, returnType) -> Some(uncurryLambdaType -1 [ argType ] returnType)
        | _ -> None

    /// In lambdas with tuple arguments, F# compiler deconstructs the tuple before the next nested lambda.
    /// This makes it harder to uncurry lambdas, so we try to move the bindings to the inner lambda.
    let flattenLambdaBodyWithTupleArgs (arg: Ident) (body: Expr) =
        let rec flattenBindings accBindings (tupleArg: Ident) (body: Expr) =
            match body with
            | Lambda(arg, body, info) ->
                let body =
                    (body, accBindings) ||> List.fold (fun body (id, value) -> Let(id, value, body))

                Lambda(arg, body, info) |> Some
            | Let(id, (Get(IdentExpr tupleIdent, TupleIndex _, _, _) as value), body) when
                tupleIdent.Name = tupleArg.Name
                ->
                flattenBindings ((id, value) :: accBindings) tupleArg body
            | _ -> None

        match arg.Type with
        | Tuple _ -> flattenBindings [] arg body |> Option.defaultValue body
        | _ -> body

    /// Only matches lambda immediately nested within each other
    let rec nestedLambda checkArity expr =
        let rec inner accArgs body name =
            match body with
            | Lambda(arg, body, None) -> inner (arg :: accArgs) body name
            | _ -> List.rev accArgs, body, name

        match expr with
        | Lambda(arg, body, name) ->
            let args, body, name = inner [ arg ] body name

            if checkArity then
                match expr.Type with
                | NestedLambdaType(argTypes, _) when List.sameLength args argTypes -> Some(args, body, name)
                | _ -> None
            else
                Some(args, body, name)
        | _ -> None

    /// Makes sure to capture the same number of args as the arity of the lambda
    let (|NestedLambdaWithSameArity|_|) expr = nestedLambda true expr

    /// Doesn't check the type of lambda body has same arity as discovered arguments
    let (|NestedLambda|_|) expr = nestedLambda false expr

    let (|NestedApply|_|) expr =
        let rec nestedApply r t accArgs applied =
            match applied with
            | CurriedApply(applied, args, _, _) -> nestedApply r t (args @ accArgs) applied
            | _ -> Some(applied, accArgs, t, r)

        match expr with
        | CurriedApply(applied, args, t, r) -> nestedApply r t args applied
        | _ -> None

    let (|LambdaUncurriedAtCompileTime|_|) arity expr =
        let rec uncurryLambdaInner (name: string option) accArgs remainingArity expr =
            if remainingArity = Some 0 then
                Delegate(List.rev accArgs, expr, name, Tags.empty) |> Some
            else
                match expr, remainingArity with
                | Lambda(arg, body, name2), _ ->
                    let remainingArity = remainingArity |> Option.map (fun x -> x - 1)

                    uncurryLambdaInner (Option.orElse name2 name) (arg :: accArgs) remainingArity body
                // If there's no arity expectation we can return the flattened part
                | _, None when List.isEmpty accArgs |> not -> Delegate(List.rev accArgs, expr, name, Tags.empty) |> Some
                // We cannot flatten lambda to the expected arity
                | _, _ -> None

        match expr with
        // Uncurry also function options
        | Value(NewOption(Some expr, _, isStruct), r) ->
            uncurryLambdaInner None [] arity expr
            |> Option.map (fun expr -> Value(NewOption(Some expr, expr.Type, isStruct), r))
        | _ -> uncurryLambdaInner None [] arity expr

    let (|NestedRevLets|_|) expr =
        let rec inner bindings =
            function
            | Let(i, v, body) -> inner ((i, v) :: bindings) body
            | body -> bindings, body

        match expr with
        | Let(i, v, body) -> inner [ i, v ] body |> Some
        | _ -> None

    let rec (|MaybeCasted|) =
        function
        | TypeCast(MaybeCasted e, _) -> e
        | e -> e

    let (|MaybeOption|) e =
        match e with
        | Option(e, _) -> e
        | e -> e

    /// Try to uncurry lambdas at compile time in dynamic assignments
    let (|MaybeLambdaUncurriedAtCompileTime|) =
        function
        | MaybeCasted(LambdaUncurriedAtCompileTime None lambda) -> lambda
        | e -> e

    let (|StringConst|_|) =
        function
        | MaybeCasted(Value(StringConstant str, _)) -> Some str
        | _ -> None

    let (|BoolConst|_|) =
        function
        | MaybeCasted(Value(BoolConstant v, _)) -> Some v
        | _ -> None

    let (|NumberConst|_|) =
        function
        | MaybeCasted(Value(NumberConstant(value, kind, info), _)) -> Some(value, kind, info)
        | _ -> None

    let (|NullConst|_|) =
        function
        | MaybeCasted(Value(Null _, _)) -> Some()
        | _ -> None

    let (|StringComparisonEnumValue|_|) e =
        match e with
        | Expr.Value(kind = NumberConstant(info = NumberInfo.IsEnum({ FullName = "System.StringComparison" }))) ->
            Some()
        | _ -> None

    // TODO: Improve this, see https://github.com/fable-compiler/Fable/issues/1659#issuecomment-445071965
    // This is mainly used for inlining so a computation or a reference to a mutable value are understood
    // as a side effects too (because we don't want to duplicate or change the order of execution)
    let rec canHaveSideEffects =
        function
        | Import _ -> false
        | Lambda _
        | Delegate _ -> false
        | TypeCast(e, _) ->
            match Compiler.Language with
            | JavaScript
            | Python -> canHaveSideEffects e
            | _ -> true
        | Value(value, _) ->
            match value with
            | ThisValue _
            | BaseValue _ -> true
            | TypeInfo _
            | Null _
            | UnitConstant
            | NumberConstant _
            | BoolConstant _
            | CharConstant _
            | StringConstant _
            | RegexConstant _ -> false
            | NewList(None, _)
            | NewOption(None, _, _) -> false
            | NewOption(Some e, _, _) -> canHaveSideEffects e
            | NewList(Some(h, t), _) -> canHaveSideEffects h || canHaveSideEffects t
            | StringTemplate(_, _, exprs)
            | NewTuple(exprs, _)
            | NewUnion(exprs, _, _, _) -> List.exists canHaveSideEffects exprs
            | NewArray(newKind, _, kind) ->
                match kind, newKind with
                | ImmutableArray, ArrayFrom expr -> canHaveSideEffects expr
                | ImmutableArray, ArrayValues exprs -> List.exists canHaveSideEffects exprs
                | _, ArrayAlloc _
                | _, ArrayValues [] -> false
                | _ -> true
            | NewRecord _
            | NewAnonymousRecord _ -> true
        | IdentExpr id -> id.IsMutable
        | Get(e, kind, _, _) ->
            match kind with
            | OptionValue ->
                match Compiler.Language with
                | Dart -> canHaveSideEffects e
                // Other languages include a runtime check for options
                | _ -> true
            | ListHead
            | ListTail
            | TupleIndex _
            | UnionTag -> canHaveSideEffects e
            // Don't move union field getters after union case test in case TypeScript complains
            | UnionField _ -> Compiler.Language = TypeScript || canHaveSideEffects e
            | FieldGet info ->
                if info.CanHaveSideEffects then
                    true
                else
                    canHaveSideEffects e
            | ExprGet _ -> true
        | _ -> true

    /// For unit, unresolved generics or nested options or unknown types,
    /// create a runtime wrapper. See fable-library/Option.ts for more info.
    let rec mustWrapOption =
        function
        | Any
        | Unit
        | GenericParam _
        | Option _ -> true
        | _ -> false

    let isUnitOfMeasure t =
        match t with
        | Measure _
        | GenericParam(_, true, _) -> true
        | Fable.DeclaredType(ent, _) ->
            match ent.FullName with
            | Types.measureProduct2
            | Types.measureOne
            | Types.measureInverse -> true
            | _ -> false
        | _ -> false

    /// ATTENTION: Make sure the ident name is unique
    let makeTypedIdent typ name =
        {
            Name = name
            Type = typ
            IsCompilerGenerated = true
            IsThisArgument = false
            IsMutable = false
            Range = None
        }

    /// ATTENTION: Make sure the ident name is unique
    let makeIdent name = makeTypedIdent Any name

    /// ATTENTION: Make sure the ident name is unique
    let makeIdentExpr name = makeIdent name |> IdentExpr

    let makeTypedIdentExpr typ name = makeTypedIdent typ name |> IdentExpr

    let makeWhileLoop range guardExpr bodyExpr = WhileLoop(guardExpr, bodyExpr, range)

    let makeForLoop range isUp ident start limit body =
        ForLoop(ident, start, limit, body, isUp, range)

    let makeBinOp range typ left right op =
        Operation(Binary(op, left, right), Tags.empty, typ, range)

    let makeUnOp range typ arg op =
        Operation(Unary(op, arg), Tags.empty, typ, range)

    let makeLogOp range left right op =
        Operation(Logical(op, left, right), Tags.empty, Boolean, range)

    let makeEqOp range left right op =
        Operation(Binary(op, left, right), Tags.empty, Boolean, range)

    let makeNullTyped t = Value(Null t, None)

    let makeNull () = Value(Null Any, None)

    let makeNone t = Value(NewOption(None, t, false), None)

    let makeValue r value = Value(value, r)

    let makeTypeInfo r t = TypeInfo(t, Tags.empty) |> makeValue r

    let makeTypeDefinitionInfo r t =
        let t =
            match t with
            | Option(_, isStruct) -> Option(Any, isStruct)
            | Array(_, kind) -> Array(Any, kind)
            | List _ -> List Any
            | Tuple(genArgs, isStruct) -> Tuple(genArgs |> List.map (fun _ -> Any), isStruct)
            | DeclaredType(ent, genArgs) ->
                let genArgs = genArgs |> List.map (fun _ -> Any)
                DeclaredType(ent, genArgs)
            // TODO: Do something with FunctionType and ErasedUnion?
            | t -> t

        makeTypeInfo r t

    let makeTuple r isStruct values = Value(NewTuple(values, isStruct), r)

    let makeResizeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType, ResizeArray) |> makeValue None

    let makeArray elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType, MutableArray) |> makeValue None

    let makeArrayWithRange r elementType arrExprs =
        NewArray(ArrayValues arrExprs, elementType, MutableArray) |> makeValue r

    let makeDelegate args body = Delegate(args, body, None, Tags.empty)

    let makeLambda (args: Ident list) (body: Expr) =
        (args, body) ||> List.foldBack (fun arg body -> Lambda(arg, body, None))

    let makeLambdaType (argTypes: Type list) (returnType: Type) =
        (argTypes, returnType)
        ||> List.foldBack (fun arg returnType -> LambdaType(arg, returnType))

    let makeBoolConst (x: bool) = BoolConstant x |> makeValue None
    let makeStrConst (x: string) = StringConstant x |> makeValue None

    let makeIntConst (x: int) =
        NumberConstant(x, Int32, NumberInfo.Empty) |> makeValue None

    let makeFloatConst (x: float) =
        NumberConstant(x, Float64, NumberInfo.Empty) |> makeValue None

    let makeRegexConst r (pattern: string) flags =
        let flags = RegexGlobal :: RegexUnicode :: flags // .NET regex are always global & unicode
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
        | _ ->
            FableError $"Cannot create expression for object {value} (%s{value.GetType().FullName})"
            |> raise

    let makeTypeConst r (typ: Type) (value: obj) =
        match typ, value with
        | Boolean, (:? bool as x) -> BoolConstant x |> makeValue r
        | String, (:? string as x) -> StringConstant x |> makeValue r
        | Char, (:? char as x) -> CharConstant x |> makeValue r
        | Number(kind, info), x -> NumberConstant(x, kind, info) |> makeValue r
        | Unit, _ -> UnitConstant |> makeValue r
        // Arrays with small data type (ushort, byte) are represented
        // in F# AST as BasicPatterns.Const
        | Array(Number(kind, uom), arrayKind), (:? (byte[]) as arr) ->
            let values =
                arr
                |> Array.map (fun x -> NumberConstant(x, kind, uom) |> makeValue None)
                |> Seq.toList

            NewArray(ArrayValues values, Number(kind, uom), arrayKind) |> makeValue r
        | Array(Number(kind, uom), arrayKind), (:? (uint16[]) as arr) ->
            let values =
                arr
                |> Array.map (fun x -> NumberConstant(x, kind, uom) |> makeValue None)
                |> Seq.toList

            NewArray(ArrayValues values, Number(kind, uom), arrayKind) |> makeValue r
        | _ ->
            FableError $"Unexpected type %A{typ} for literal {value} (%s{value.GetType().FullName})"
            |> raise

    let getLibPath (com: Compiler) (moduleName: string) =
        match com.Options.Language with
        | Python ->
            // Python modules should be all lower case without any dots (PEP8)
            let moduleName' =
                moduleName
                |> Naming.applyCaseRule Fable.Core.CaseRules.SnakeCase
                |> (fun str -> str.Replace(".", "_"))

            com.LibraryDir + "/" + moduleName' + ".py"
        | Rust -> com.LibraryDir + "/" + moduleName + ".rs"
        | Dart -> com.LibraryDir + "/" + moduleName + ".dart"
        | _ -> com.LibraryDir + "/" + moduleName + ".js"

    let makeImportUserGenerated r t (selector: string) (path: string) =
        Import(
            {
                Selector = selector.Trim()
                Path = path.Trim()
                Kind = UserImport false
            },
            t,
            r
        )

    let makeImportLibWithInfo (com: Compiler) t memberName (moduleName: string) info =
        let selector =
            match com.Options.Language with
            | Rust ->
                if
                    moduleName = "System"
                    || moduleName.StartsWith("System.", StringComparison.Ordinal)
                then
                    moduleName + "::" + memberName
                else
                    moduleName + "_::" + memberName
            | _ -> memberName

        Import(
            {
                Selector = selector
                Path = getLibPath com moduleName
                Kind = LibraryImport info
            },
            t,
            None
        )

    let makeImportLib (com: Compiler) t memberName moduleName =
        LibraryImportInfo.Create(isInstanceMember = false, isModuleMember = true)
        |> makeImportLibWithInfo com t memberName moduleName

    let private makeInternalImport (com: Compiler) t (selector: string) (path: string) kind =
        let path =
            if com.CurrentFile = path then
                "./" + Path.GetFileName(path)
            else
                Path.getRelativeFileOrDirPath false com.CurrentFile false path

        Import(
            {
                Selector = selector
                Path = path
                Kind = kind
            },
            t,
            None
        )

    let makeInternalMemberImport com t membRef (selector: string) (path: string) =
        MemberImport(membRef) |> makeInternalImport com t selector path

    let makeInternalClassImport com entRef (selector: string) (path: string) =
        ClassImport(entRef) |> makeInternalImport com Any selector path

    let makeCallInfo thisArg args sigArgTypes =
        CallInfo.Create(?thisArg = thisArg, args = args, sigArgTypes = sigArgTypes)

    let emit r t args isStatement macro =
        let emitInfo =
            {
                Macro = macro
                IsStatement = isStatement
                CallInfo = CallInfo.Create(args = args)
            }

        Emit(emitInfo, t, r)

    let emitTemplate r t args isStatement (templateParts, templateValues) =
        let macro =
            match templateParts with
            | [] -> ""
            | head :: tail ->
                ((head, List.length args), tail)
                ||> List.fold (fun (macro, pos) part -> $"{macro}$%i{pos}{part}", pos + 1)
                |> fst

        emit r t (args @ templateValues) isStatement macro

    let emitExpr r t args macro = emit r t args false macro

    let emitStatement r t args macro = emit r t args true macro

    let makeThrow r t (err: Expr) = Extended(Throw(Some err, t), r)

    let makeDebugger range = Extended(Debugger, range)

    let destructureTupleArgs =
        function
        | [ MaybeCasted(Value(UnitConstant, _)) ] -> []
        | [ MaybeCasted(Value(NewTuple(args, _), _)) ] -> args
        | args -> args

    let makeCall r t callInfo calleeExpr = Call(calleeExpr, callInfo, t, r)

    let getExpr r t left memb = Get(left, ExprGet memb, t, r)

    let getOptionValue r t e = Get(e, OptionValue, t, r)

    let setExpr r left memb (value: Expr) =
        Set(left, ExprSet memb, value.Type, value, r)

    let getImmutableFieldWith r t callee membName =
        Get(callee, FieldInfo.Create(membName), t, r)

    let getFieldWith r t callee membName =
        Get(callee, FieldInfo.Create(membName, maybeCalculated = true), t, r)

    let getField (e: Expr) membName = getFieldWith e.Range Any e membName

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
        | Int128 -> "int128"
        | UInt128 -> "uint128"
        | BigInt -> "bigint"
        | NativeInt -> "nativeint"
        | UNativeInt -> "unativeint"
        | Float16 -> "float16"
        | Float32 -> "float32"
        | Float64 -> "float64"
        | Decimal -> "decimal"

    type ParamsInfo =
        {|
            NamedIndex: int option
            Parameters: Parameter list
            HasSpread: bool
        |}

    let getParamsInfo (memberInfo: MemberFunctionOrValue) : ParamsInfo =
        // ParamObject/NamedParams attribute is not compatible with arg spread
        if memberInfo.HasSpread then
            {|
                NamedIndex = None
                HasSpread = true
                Parameters = List.concat memberInfo.CurriedParameterGroups
            |}
        else
            let parameters = List.concat memberInfo.CurriedParameterGroups

            {|
                HasSpread = false
                Parameters = parameters
                NamedIndex = parameters |> List.tryFindIndex (fun p -> p.IsNamed)
            |}

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
        let argIdents =
            match argIdents with
            | [ i ] when i.Type = Unit -> []
            | _ -> argIdents

        let argExprs =
            match argExprs with
            | [ Value(UnitConstant, _) ] -> []
            | _ -> argExprs

        if List.sameLength argIdents argExprs |> not then
            false
        else
            (true, List.zip argIdents argExprs)
            ||> List.fold (fun eq (id, expr) ->
                if not eq then
                    false
                else
                    match expr with
                    | IdentExpr id2 -> id.Name = id2.Name
                    | _ -> false
            )

    let rec listEquals f li1 li2 =
        match li1, li2 with
        | [], [] -> true
        | h1 :: t1, h2 :: t2 -> f h1 h2 && listEquals f t1 t2
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
        | Tuple(ts1, isStruct1), Tuple(ts2, isStruct2) ->
            isStruct1 = isStruct2 && listEquals (typeEquals strict) ts1 ts2
        | LambdaType(a1, t1), LambdaType(a2, t2) -> typeEquals strict a1 a2 && typeEquals strict t1 t2
        | DelegateType(as1, t1), DelegateType(as2, t2) ->
            listEquals (typeEquals strict) as1 as2 && typeEquals strict t1 t2
        | DeclaredType(ent1, gen1), DeclaredType(ent2, gen2) -> ent1 = ent2 && listEquals (typeEquals strict) gen1 gen2
        | GenericParam _, _
        | _, GenericParam _ when not strict -> true
        | GenericParam(name = name1), GenericParam(name = name2) -> name1 = name2
        // Field names must be already sorted
        | AnonymousRecordType(fields1, gen1, isStruct1), AnonymousRecordType(fields2, gen2, isStruct2) ->
            fields1.Length = fields2.Length
            && Array.zip fields1 fields2 |> Array.forall (fun (f1, f2) -> f1 = f2)
            && listEquals (typeEquals strict) gen1 gen2
            && isStruct1 = isStruct2
        | Measure _, Measure _ -> true
        | _ -> false

    let rec getEntityFullName prettify (entRef: EntityRef) gen =
        let fullname = entRef.FullName

        if List.isEmpty gen then
            fullname
        else
            let gen = (List.map (getTypeFullName prettify) gen |> String.concat ",")

            let fullname =
                if prettify then
                    match fullname with
                    | Types.result -> "Result"
                    | Naming.StartsWith Types.choiceNonGeneric _ -> "Choice"
                    | _ -> fullname // TODO: Prettify other types?
                else
                    fullname

            fullname + "[" + gen + "]"

    and getNumberFullName prettify kind info =
        let getKindName =
            function
            | Int8 -> Types.int8
            | UInt8 -> Types.uint8
            | Int16 -> Types.int16
            | UInt16 -> Types.uint16
            | Int32 -> Types.int32
            | UInt32 -> Types.uint32
            | Int64 -> Types.int64
            | UInt64 -> Types.uint64
            | Int128 -> Types.int128
            | UInt128 -> Types.uint128
            | NativeInt -> Types.nativeint
            | UNativeInt -> Types.unativeint
            | Float16 -> Types.float16
            | Float32 -> Types.float32
            | Float64 -> Types.float64
            | Decimal -> Types.decimal
            | BigInt -> Types.bigint

        match info with
        | NumberInfo.Empty -> getKindName kind
        | NumberInfo.IsMeasure uom -> getKindName kind + "[" + uom + "]"
        | NumberInfo.IsEnum ent -> getEntityFullName prettify ent []

    and getTypeFullName prettify t =
        match t with
        | Measure fullname -> fullname
        | AnonymousRecordType _ -> ""
        | GenericParam(name = name) -> "'" + name
        | Regex -> Types.regex
        | MetaType -> Types.type_
        | Unit -> Types.unit
        | Boolean -> Types.bool
        | Char -> Types.char
        | String -> Types.string
        | Any -> Types.object
        | Number(kind, info) -> getNumberFullName prettify kind info
        | LambdaType(argType, returnType) ->
            let argType = getTypeFullName prettify argType
            let returnType = getTypeFullName prettify returnType

            if prettify then
                argType + " -> " + returnType
            else
                "Microsoft.FSharp.Core.FSharpFunc`2[" + argType + "," + returnType + "]"
        | DelegateType(argTypes, returnType) ->
            sprintf
                "System.Func`%i[%s,%s]"
                (List.length argTypes + 1)
                (List.map (getTypeFullName prettify) argTypes |> String.concat ",")
                (getTypeFullName prettify returnType)
        | Tuple(genArgs, isStruct) ->
            let genArgs = List.map (getTypeFullName prettify) genArgs

            if prettify then
                (if isStruct then
                     "struct "
                 else
                     "")
                + String.concat " * " genArgs
            else
                let isStruct =
                    if isStruct then
                        "Value"
                    else
                        ""

                let genArgsLength = List.length genArgs
                let genArgs = String.concat "," genArgs
                $"System.{isStruct}Tuple`{genArgsLength}[{genArgs}]"
        | Array(gen, _kind) -> // TODO: Check kind
            (getTypeFullName prettify gen) + "[]"
        | Option(gen, isStruct) ->
            let gen = getTypeFullName prettify gen

            if prettify then
                gen
                + " "
                + (if isStruct then
                       "v"
                   else
                       "")
                + "option"
            else
                (if isStruct then
                     Types.valueOption
                 else
                     Types.option)
                + "["
                + gen
                + "]"
        | List gen ->
            let gen = getTypeFullName prettify gen

            if prettify then
                gen + " list"
            else
                Types.list + "[" + gen + "]"
        | DeclaredType(ent, gen) -> getEntityFullName prettify ent gen

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
            Import(
                { info with
                    Selector = info.Selector
                    Path = info.Path
                },
                t,
                r
            )
        | Extended(kind, r) ->
            match kind with
            | Curry(e, arity) -> Extended(Curry(f e, arity), r)
            | Throw(e, t) -> Extended(Throw(Option.map f e, t), r)
            | Debugger -> e
        | Value(kind, r) ->
            match kind with
            | ThisValue _
            | BaseValue _
            | TypeInfo _
            | Null _
            | UnitConstant
            | BoolConstant _
            | CharConstant _
            | StringConstant _
            | NumberConstant _
            | RegexConstant _ -> e
            | StringTemplate(tag, parts, exprs) -> StringTemplate(tag, parts, List.map f exprs) |> makeValue r
            | NewOption(e, t, isStruct) -> NewOption(Option.map f e, t, isStruct) |> makeValue r
            | NewTuple(exprs, isStruct) -> NewTuple(List.map f exprs, isStruct) |> makeValue r
            | NewArray(ArrayValues exprs, t, i) -> NewArray(List.map f exprs |> ArrayValues, t, i) |> makeValue r
            | NewArray(ArrayFrom expr, t, i) -> NewArray(f expr |> ArrayFrom, t, i) |> makeValue r
            | NewArray(ArrayAlloc expr, t, i) -> NewArray(f expr |> ArrayAlloc, t, i) |> makeValue r
            | NewList(ht, t) ->
                let ht = ht |> Option.map (fun (h, t) -> f h, f t)
                NewList(ht, t) |> makeValue r
            | NewRecord(exprs, ent, genArgs) -> NewRecord(List.map f exprs, ent, genArgs) |> makeValue r
            | NewAnonymousRecord(exprs, ent, genArgs, isStruct) ->
                NewAnonymousRecord(List.map f exprs, ent, genArgs, isStruct) |> makeValue r
            | NewUnion(exprs, uci, ent, genArgs) -> NewUnion(List.map f exprs, uci, ent, genArgs) |> makeValue r
        | Test(e, kind, r) -> Test(f e, kind, r)
        | Lambda(arg, body, name) -> Lambda(arg, f body, name)
        | Delegate(args, body, name, tags) -> Delegate(args, f body, name, tags)
        | ObjectExpr(members, t, baseCall) ->
            let baseCall = Option.map f baseCall

            let members = members |> List.map (fun m -> { m with Body = f m.Body })

            ObjectExpr(members, t, baseCall)
        | CurriedApply(callee, args, t, r) -> CurriedApply(f callee, List.map f args, t, r)
        | Call(callee, info, t, r) ->
            let info =
                { info with
                    ThisArg = Option.map f info.ThisArg
                    Args = List.map f info.Args
                }

            Call(f callee, info, t, r)
        | Emit(info, t, r) ->
            let callInfo =
                { info.CallInfo with
                    ThisArg = Option.map f info.CallInfo.ThisArg
                    Args = List.map f info.CallInfo.Args
                }

            Emit({ info with CallInfo = callInfo }, t, r)
        | Operation(kind, tags, t, r) ->
            match kind with
            | Unary(operator, operand) -> Operation(Unary(operator, f operand), tags, t, r)
            | Binary(op, left, right) -> Operation(Binary(op, f left, f right), tags, t, r)
            | Logical(op, left, right) -> Operation(Logical(op, f left, f right), tags, t, r)
        | Get(e, kind, t, r) ->
            match kind with
            | ListHead
            | ListTail
            | OptionValue
            | TupleIndex _
            | UnionTag
            | UnionField _
            | FieldGet _ -> Get(f e, kind, t, r)
            | ExprGet e2 -> Get(f e, ExprGet(f e2), t, r)
        | Sequential exprs -> Sequential(List.map f exprs)
        | Let(ident, value, body) -> Let(ident, f value, f body)
        | LetRec(bs, body) ->
            let bs = bs |> List.map (fun (i, e) -> i, f e)
            LetRec(bs, f body)
        | IfThenElse(cond, thenExpr, elseExpr, r) -> IfThenElse(f cond, f thenExpr, f elseExpr, r)
        | Set(e, kind, t, v, r) ->
            match kind with
            | ExprSet e2 -> Set(f e, ExprSet(f e2), t, f v, r)
            | FieldSet _
            | ValueSet -> Set(f e, kind, t, f v, r)
        | WhileLoop(e1, e2, r) -> WhileLoop(f e1, f e2, r)
        | ForLoop(i, e1, e2, e3, up, r) -> ForLoop(i, f e1, f e2, f e3, up, r)
        | TryCatch(body, catch, finalizer, r) ->
            TryCatch(f body, Option.map (fun (i, e) -> i, f e) catch, Option.map f finalizer, r)
        | DecisionTree(expr, targets) ->
            let targets = targets |> List.map (fun (idents, v) -> idents, f v)
            DecisionTree(f expr, targets)
        | DecisionTreeSuccess(idx, boundValues, t) -> DecisionTreeSuccess(idx, List.map f boundValues, t)

    let rec visitFromInsideOut f e = visit (visitFromInsideOut f) e |> f

    let rec visitFromOutsideIn (f: Expr -> Expr option) e =
        match f e with
        | Some e -> e
        | None -> visit (visitFromOutsideIn f) e

    let getSubExpressions =
        function
        | Unresolved _ -> []
        | IdentExpr _ -> []
        | TypeCast(e, _) -> [ e ]
        | Import _ -> []
        | Extended(kind, _) ->
            match kind with
            | Curry(e, _) -> [ e ]
            | Throw(e, _) -> Option.toList e
            | Debugger -> []
        | Value(kind, _) ->
            match kind with
            | ThisValue _
            | BaseValue _
            | TypeInfo _
            | Null _
            | UnitConstant
            | BoolConstant _
            | CharConstant _
            | StringConstant _
            | NumberConstant _
            | RegexConstant _ -> []
            | StringTemplate(_, _, exprs) -> exprs
            | NewOption(e, _, _) -> Option.toList e
            | NewTuple(exprs, _) -> exprs
            | NewArray(kind, _, _) ->
                match kind with
                | ArrayValues exprs -> exprs
                | ArrayAlloc e
                | ArrayFrom e -> [ e ]
            | NewList(ht, _) ->
                match ht with
                | Some(h, t) -> [ h; t ]
                | None -> []
            | NewRecord(exprs, _, _) -> exprs
            | NewAnonymousRecord(exprs, _, _, _) -> exprs
            | NewUnion(exprs, _, _, _) -> exprs
        | Test(e, _, _) -> [ e ]
        | Lambda(_, body, _) -> [ body ]
        | Delegate(_, body, _, _) -> [ body ]
        | ObjectExpr(members, _, baseCall) ->
            let members = members |> List.map (fun m -> m.Body)

            match baseCall with
            | Some b -> b :: members
            | None -> members
        | CurriedApply(callee, args, _, _) -> callee :: args
        | Call(e1, info, _, _) -> e1 :: (Option.toList info.ThisArg) @ info.Args
        | Emit(info, _, _) -> (Option.toList info.CallInfo.ThisArg) @ info.CallInfo.Args
        | Operation(kind, _, _, _) ->
            match kind with
            | Unary(_, operand) -> [ operand ]
            | Binary(_, left, right) -> [ left; right ]
            | Logical(_, left, right) -> [ left; right ]
        | Get(e, kind, _, _) ->
            match kind with
            | ListHead
            | ListTail
            | OptionValue
            | TupleIndex _
            | UnionTag
            | UnionField _
            | FieldGet _ -> [ e ]
            | ExprGet e2 -> [ e; e2 ]
        | Sequential exprs -> exprs
        | Let(_, value, body) -> [ value; body ]
        | LetRec(bs, body) -> (List.map snd bs) @ [ body ]
        | IfThenElse(cond, thenExpr, elseExpr, _) -> [ cond; thenExpr; elseExpr ]
        | Set(e, kind, _, v, _) ->
            match kind with
            | ExprSet e2 -> [ e; e2; v ]
            | FieldSet _
            | ValueSet -> [ e; v ]
        | WhileLoop(e1, e2, _) -> [ e1; e2 ]
        | ForLoop(_, e1, e2, e3, _, _) -> [ e1; e2; e3 ]
        | TryCatch(body, catch, finalizer, _) ->
            match catch with
            | Some(_, c) -> body :: c :: (Option.toList finalizer)
            | None -> body :: (Option.toList finalizer)
        | DecisionTree(expr, targets) -> expr :: (List.map snd targets)
        | DecisionTreeSuccess(_, boundValues, _) -> boundValues

    let deepExists (f: Expr -> bool) expr =
        let rec deepExistsInner (exprs: ResizeArray<Expr>) =
            let mutable found = false
            let subExprs = FSharp.Collections.ResizeArray()

            for e in exprs do
                if not found then
                    subExprs.AddRange(getSubExpressions e)
                    found <- f e

            if found then
                true
            elif subExprs.Count > 0 then
                deepExistsInner subExprs
            else
                false

        FSharp.Collections.ResizeArray [| expr |] |> deepExistsInner

    // depth-first search
    let rec tryFindExprDFS (f: Expr -> bool) (e: Expr) =
        getSubExpressions e
        |> List.tryPick (fun e2 -> tryFindExprDFS f e2)
        |> Option.orElse (
            if f e then
                Some e
            else
                None
        )

    let isIdentUsed identName expr =
        expr
        |> deepExists (
            function
            | IdentExpr i -> i.Name = identName
            | _ -> false
        )

    let extractGenericArgs (maybeGenericExpr: Expr) concreteType =
        let rec extractGenericArgs genArgs maybeGenericType concreteType =
            match maybeGenericType, concreteType with
            | Fable.GenericParam(name = name1), Fable.GenericParam(name = name2) when name1 = name2 -> genArgs
            | Fable.GenericParam(name = name), t -> Map.add name t genArgs
            | t1, t2 ->
                match t1.Generics with
                | [] -> genArgs
                | gen1 ->
                    let gen2 = t2.Generics

                    if List.sameLength gen1 gen2 then
                        List.fold2 extractGenericArgs genArgs gen1 gen2
                    else
                        genArgs

        extractGenericArgs Map.empty maybeGenericExpr.Type concreteType

    let rec resolveInlineType (genArgs: Map<string, Type>) =
        function
        | GenericParam(name, isMeasure, _constraints) as t ->
            match Map.tryFind name genArgs with
            | Some v when isMeasure && v = Any -> t // avoids resolving measures to Fable.Any
            | Some v -> v
            | None -> t
        | t -> t.MapGenerics(resolveInlineType genArgs)

    let resolveInlineIdent (genArgs: Map<string, Type>) (id: Ident) =
        { id with Type = resolveInlineType genArgs id.Type }

    let resolveInlineMemberRef genArgs =
        function
        | MemberRef(ent, info) ->
            let argTypes =
                Option.map (List.map (resolveInlineType genArgs)) info.NonCurriedArgTypes

            MemberRef(ent, { info with NonCurriedArgTypes = argTypes })

        | GeneratedMemberRef(gen) ->
            let mapInfo (i: GeneratedMemberInfo) =
                let paramTypes = List.map (resolveInlineType genArgs) i.ParamTypes

                let returnType = resolveInlineType genArgs i.ReturnType

                { i with
                    ParamTypes = paramTypes
                    ReturnType = returnType
                }

            match gen with
            | GeneratedFunction i -> GeneratedFunction(mapInfo i)
            | GeneratedValue i -> GeneratedValue(mapInfo i)
            | GeneratedGetter i -> GeneratedGetter(mapInfo i)
            | GeneratedSetter i -> GeneratedSetter(mapInfo i)
            |> GeneratedMemberRef

    let resolveInlineCallInfo genArgs (info: CallInfo) =
        let infoGenArgs = List.map (resolveInlineType genArgs) info.GenericArgs

        let infoSigTypes = List.map (resolveInlineType genArgs) info.SignatureArgTypes

        let memberRef = Option.map (resolveInlineMemberRef genArgs) info.MemberRef

        { info with
            GenericArgs = infoGenArgs
            SignatureArgTypes = infoSigTypes
            MemberRef = memberRef
        }

    let replaceGenericArgs expr (genArgs: Map<string, Type>) =
        if Map.isEmpty genArgs then
            expr
        else
            expr
            |> visitFromInsideOut (
                function
                | Value(kind, r) as e ->
                    match kind with
                    | ThisValue t -> Value(ThisValue(resolveInlineType genArgs t), r)
                    | BaseValue(i, t) ->
                        let i = Option.map (resolveInlineIdent genArgs) i
                        Value(BaseValue(i, resolveInlineType genArgs t), r)
                    | TypeInfo(t, tags) -> Value(TypeInfo(resolveInlineType genArgs t, tags), r)
                    | Null t -> Value(Null(resolveInlineType genArgs t), r)
                    | NewOption(v, t, isStruct) -> Value(NewOption(v, resolveInlineType genArgs t, isStruct), r)
                    | NewArray(k1, t, k2) -> Value(NewArray(k1, resolveInlineType genArgs t, k2), r)
                    | NewList(v, t) -> Value(NewList(v, resolveInlineType genArgs t), r)
                    | NewRecord(vs, ent, gen) ->
                        let gen = List.map (resolveInlineType genArgs) gen
                        Value(NewRecord(vs, ent, gen), r)
                    | NewAnonymousRecord(vs, fields, gen, isStruct) ->
                        let gen = List.map (resolveInlineType genArgs) gen
                        Value(NewAnonymousRecord(vs, fields, gen, isStruct), r)
                    | NewUnion(vs, tag, ent, gen) ->
                        let gen = List.map (resolveInlineType genArgs) gen
                        Value(NewUnion(vs, tag, ent, gen), r)
                    | _ -> e

                | IdentExpr id -> resolveInlineIdent genArgs id |> IdentExpr

                | Lambda(arg, b, n) ->
                    let arg = resolveInlineIdent genArgs arg
                    Lambda(arg, b, n)

                | Delegate(args, b, n, t) -> Delegate(List.map (resolveInlineIdent genArgs) args, b, n, t)

                | ObjectExpr(members, typ, baseCall) ->
                    let members =
                        members
                        |> List.map (fun m ->
                            let args = List.map (resolveInlineIdent genArgs) m.Args

                            { m with
                                Args = args
                                MemberRef = resolveInlineMemberRef genArgs m.MemberRef
                            }
                        )

                    ObjectExpr(members, resolveInlineType genArgs typ, baseCall)

                | TypeCast(e, t) -> TypeCast(e, resolveInlineType genArgs t)

                | Test(e, TypeTest t, r) -> Test(e, TypeTest(resolveInlineType genArgs t), r)

                | Call(callee, info, t, r) ->
                    let info = resolveInlineCallInfo genArgs info
                    Call(callee, info, resolveInlineType genArgs t, r)

                | CurriedApply(callee, args, typ, r) -> CurriedApply(callee, args, resolveInlineType genArgs typ, r)

                | Operation(kind, tags, typ, r) -> Operation(kind, tags, resolveInlineType genArgs typ, r)

                | Import(info, t, r) ->
                    let info =
                        match info.Kind with
                        | MemberImport m -> { info with Kind = resolveInlineMemberRef genArgs m |> MemberImport }
                        | UserImport _
                        | LibraryImport _
                        | ClassImport _ -> info

                    Import(info, resolveInlineType genArgs t, r)

                | Emit(info, t, r) ->
                    let info = { info with CallInfo = resolveInlineCallInfo genArgs info.CallInfo }

                    Emit(info, resolveInlineType genArgs t, r)

                | DecisionTree(expr, targets) ->
                    let targets =
                        targets
                        |> List.map (fun (bindings, body) -> List.map (resolveInlineIdent genArgs) bindings, body)

                    DecisionTree(expr, targets)

                | DecisionTreeSuccess(targetIndex, boundValues, t) ->
                    DecisionTreeSuccess(targetIndex, boundValues, resolveInlineType genArgs t)

                | Set(e, kind, t, v, r) -> Set(e, kind, resolveInlineType genArgs t, v, r)
                | Get(e, kind, t, r) ->
                    let kind =
                        match kind with
                        | FieldGet i ->
                            { i with FieldType = Option.map (resolveInlineType genArgs) i.FieldType }
                            |> FieldGet
                        | UnionField i ->
                            { i with GenericArgs = List.map (resolveInlineType genArgs) i.GenericArgs }
                            |> UnionField
                        | TupleIndex _
                        | ExprGet _
                        | UnionTag
                        | ListHead
                        | ListTail
                        | OptionValue -> kind

                    Get(e, kind, resolveInlineType genArgs t, r)

                | Let(i, v, b) -> Let(resolveInlineIdent genArgs i, v, b)

                | LetRec(bindings, b) ->
                    let bindings = bindings |> List.map (fun (i, v) -> resolveInlineIdent genArgs i, v)

                    LetRec(bindings, b)

                | Extended(Throw(e, t), r) -> Extended(Throw(e, resolveInlineType genArgs t), r)

                | e -> e
            )
