module rec Fable.Transforms.Python.Util

open System
open Fable
open Fable.AST
open Fable.Py
open Fable.Transforms
open Fable.Transforms.Python.AST
open Fable.Transforms.Python.Types

// TODO: All things that depend on the library should be moved to Replacements
// to become independent of the specific implementation
module Lib =
    let libCall (com: IPythonCompiler) ctx r moduleName memberName args =
        Expression.call (com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc = r)

    let libConsCall (com: IPythonCompiler) ctx r moduleName memberName args =
        Expression.call (com.TransformImport(ctx, memberName, getLibPath com moduleName), args, ?loc = r)

    let libValue (com: IPythonCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryPyConstructor (com: IPythonCompiler) ctx ent =
        match Py.Replacements.tryConstructor com ent with
        | Some e -> com.TransformAsExpr(ctx, e) |> Some
        | None -> None

    let pyConstructor (com: IPythonCompiler) ctx ent =
        let entRef = Py.Replacements.constructor com ent
        com.TransformAsExpr(ctx, entRef)

module Util =
    open Lib

    let hasAttribute fullName (atts: Fable.Attribute seq) =
        atts |> Seq.exists (fun att -> att.Entity.FullName = fullName)

    let hasAnyEmitAttribute (atts: Fable.Attribute seq) =
        hasAttribute Atts.emitAttr atts
        || hasAttribute Atts.emitMethod atts
        || hasAttribute Atts.emitConstructor atts
        || hasAttribute Atts.emitIndexer atts
        || hasAttribute Atts.emitProperty atts

    let hasPythonClassAttribute (atts: Fable.Attribute seq) =
        hasAttribute Atts.pyClassAttributes atts

    let parseClassStyle (styleStr: string) =
        match styleStr with
        | "attributes" -> ClassStyle.Attributes
        | "properties" -> ClassStyle.Properties
        | _ -> ClassStyle.Properties // Default to Properties for unknown values

    let getPythonClassParameters (atts: Fable.Attribute seq) =
        let defaultParams = ClassAttributes.Default

        atts
        |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.pyClassAttributes)
        |> Option.map (fun att ->
            // Extract parameters from the attribute constructor arguments
            match att.ConstructorArgs with
            | [] -> defaultParams
            | [ :? string as styleParam ] -> { defaultParams with Style = parseClassStyle styleParam }
            | [ :? string as styleParam; :? bool as initParam ] ->
                {
                    Style = parseClassStyle styleParam
                    Init = initParam
                }
            | _ -> defaultParams // Fallback for unexpected parameter combinations
        )
        |> Option.defaultValue defaultParams

    /// Parses a decorator string to extract module and function/class name
    let parseDecorator (decorator: string) =
        match decorator.Split('.') with
        | [| functionName |] -> None, functionName // No module, just function name
        | parts when parts.Length >= 2 ->
            let moduleName = parts.[0 .. (parts.Length - 2)] |> String.concat "."
            let functionName = parts.[parts.Length - 1]
            Some moduleName, functionName
        | _ -> None, decorator // Fallback

    /// Extracts decorator information from entity attributes
    let getDecoratorInfo (atts: Fable.Attribute seq) =
        atts
        |> Seq.choose (fun att ->
            if att.Entity.FullName = Atts.pyDecorate then
                match att.ConstructorArgs with
                | [ :? string as decorator ] ->
                    Some
                        {
                            Decorator = decorator
                            Parameters = ""
                        }
                | [ :? string as decorator; :? string as parameters ] ->
                    Some
                        {
                            Decorator = decorator
                            Parameters = parameters
                        }
                | _ -> None // Invalid decorator
            else
                None
        )
        |> Seq.toList

    /// Generates Python decorator expressions from DecoratorInfo
    let generateDecorators (com: IPythonCompiler) (ctx: Context) (decoratorInfos: DecoratorInfo list) =
        decoratorInfos
        |> List.map (fun info ->
            let moduleName, functionName = parseDecorator info.Decorator

            let decoratorExpr =
                match moduleName with
                | Some module_ -> com.GetImportExpr(ctx, module_, functionName)
                | None -> Expression.name functionName

            if String.IsNullOrEmpty info.Parameters then
                // Simple decorator without parameters: @decorator
                decoratorExpr
            else
                // Decorator with parameters: @decorator(param1=value1, param2=value2)
                // For parameters, we emit the full decorator call as raw Python code
                // This preserves exact parameter syntax for maximum flexibility
                Expression.emit ($"%s{functionName}(%s{info.Parameters})", [])
        )

    let getIdentifier (_com: IPythonCompiler) (_ctx: Context) (name: string) =
        let name = Helpers.clean name
        Identifier name

    let (|TransformExpr|) (com: IPythonCompiler) ctx e : Expression * Statement list = com.TransformAsExpr(ctx, e)

    let (|Function|_|) =
        function
        | Fable.Lambda(arg, body, _) -> Some([ arg ], body)
        | Fable.Delegate(args, body, _, []) -> Some(args, body)
        | _ -> None

    let getUniqueNameInRootScope (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun name ->
                name <> "str" // Do not rewrite `str`
                && (ctx.UsedNames.RootScope.Contains(name)
                    || ctx.UsedNames.DeclarationScopes.Contains(name))
            )

        ctx.UsedNames.RootScope.Add(name) |> ignore
        Helpers.clean name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.CurrentDeclarationScope.Contains(name)
            )

        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore

        name

    /// Determines if we should use the special record field naming convention (toRecordFieldSnakeCase)
    /// for the given entity. Returns true for user-defined F# records, false for built-in F# Core types.
    let shouldUseRecordFieldNaming (ent: Fable.Entity) =
        ent.IsFSharpRecord && not (ent.FullName.StartsWith("Microsoft.FSharp.Core"))

    /// Determines if we should use the special record field naming convention (toRecordFieldSnakeCase)
    /// for the given entity reference. Returns true for user-defined F# records, false for built-in F# Core types.
    let shouldUseRecordFieldNamingForRef (entityRef: Fable.EntityRef) (ent: Fable.Entity) =
        ent.IsFSharpRecord
        && not (entityRef.FullName.StartsWith("Microsoft.FSharp.Core"))

    // Helper function to determine the kind of field access for proper naming
    let getFieldNamingKind (com: IPythonCompiler) (typ: Fable.Type) (fieldName: string) : FieldNamingKind =
        match typ with
        | Fable.DeclaredType(entityRef, _) when fieldName.EndsWith("@", System.StringComparison.Ordinal) ->
            match com.TryGetEntity entityRef with
            | Some ent ->
                ent.MembersFunctionsAndValues
                |> Seq.tryFind (fun memb -> (memb.IsGetter || memb.IsSetter) && $"%s{memb.DisplayName}@" = fieldName)
                |> function
                    | Some memb when memb.IsInstance -> InstancePropertyBacking
                    | Some _ -> StaticProperty
                    | None -> RegularField
            | None -> InstancePropertyBacking // Conservative fallback for unknown entities
        | _ -> RegularField

    // Helper function to apply the appropriate naming convention based on field type and naming kind
    let applyFieldNaming
        (com: IPythonCompiler)
        (narrowedType: Fable.Type)
        (fieldName: string)
        (handleAnonymousRecords: bool)
        =
        match getFieldNamingKind com narrowedType fieldName with
        | InstancePropertyBacking -> fieldName |> Naming.toPropertyBackingFieldNaming
        | StaticProperty -> fieldName |> Naming.toPropertyNaming
        | RegularField ->
            match narrowedType with
            | Fable.AnonymousRecordType _ when handleAnonymousRecords -> fieldName // Use the field name as is for anonymous records
            | Fable.DeclaredType(entityRef, _) ->
                // Only apply naming convention for user-defined F# Records (not built-in F# Core types)
                match com.TryGetEntity entityRef with
                | Some ent when shouldUseRecordFieldNamingForRef entityRef ent ->
                    fieldName |> Naming.toRecordFieldSnakeCase |> Helpers.clean
                | _ -> fieldName |> Naming.toPythonNaming // Fallback to Python naming for other types
            | _ -> fieldName |> Naming.toPropertyNaming

    type NamedTailCallOpportunity(com: IPythonCompiler, ctx, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds =
            args
            |> FSharp2Fable.Util.discardUnitArg
            |> List.map (fun arg ->
                let name = getUniqueNameInDeclarationScope ctx (arg.Name + "_mut")
                // Ignore type annotation here as it generates unnecessary typevars
                Arg.arg name
            )

        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds

            member _.IsRecursiveRef(e) =
                match e with
                | Fable.IdentExpr id -> name = id.Name
                | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwith $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let rec isPyStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Unresolved _
        | Fable.Value _
        | Fable.Import _
        | Fable.IdentExpr _
        | Fable.Lambda _
        | Fable.Delegate _
        | Fable.ObjectExpr _
        | Fable.Call _
        | Fable.CurriedApply _
        | Fable.Operation _
        | Fable.Get _
        | Fable.Test _
        | Fable.TypeCast _ -> false

        | Fable.TryCatch _
        | Fable.Sequential _
        | Fable.Let _
        | Fable.LetRec _
        | Fable.Set _
        | Fable.ForLoop _
        | Fable.WhileLoop _ -> true
        | Fable.Extended(kind, _) ->
            match kind with
            | Fable.Throw _
            | Fable.Debugger -> true
            | Fable.Curry _ -> false

        // TODO: If IsJsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i, _, _) -> i.IsStatement

        | Fable.DecisionTreeSuccess(targetIndex, _, _) ->
            getDecisionTarget ctx targetIndex |> snd |> isPyStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_, targets) -> preferStatement || List.exists (snd >> (isPyStatement ctx false)) targets

        | Fable.IfThenElse(_, thenExpr, elseExpr, _) ->
            preferStatement
            || isPyStatement ctx false thenExpr
            || isPyStatement ctx false elseExpr

    let addErrorAndReturnNull (com: Compiler) (range: SourceLocation option) (error: string) =
        addError com [] range error
        Expression.none

    let ident (com: IPythonCompiler) (ctx: Context) (id: Fable.Ident) = com.GetIdentifier(ctx, id.Name)

    let identAsExpr (com: IPythonCompiler) (ctx: Context) (id: Fable.Ident) =
        com.GetIdentifierAsExpr(ctx, Naming.toPythonNaming id.Name)

    let getNarrowedType (ctx: Context) (id: Fable.Ident) =
        match Map.tryFind id.Name ctx.NarrowedTypes with
        | Some narrowedType -> narrowedType
        | None -> id.Type

    let thisExpr = Expression.name "self"

    let ofInt (com: IPythonCompiler) (ctx: Context) (i: int) =
        //Expression.intConstant (int i)
        libCall com ctx None "util" "int32" [ Expression.intConstant (int i) ]

    let ofString (s: string) = Expression.stringConstant s

    let memberFromName (_com: IPythonCompiler) (_ctx: Context) (memberName: string) : Expression =
        // printfn "memberFromName: %A" memberName
        match memberName with
        | "ToString" -> Expression.identifier "__str__"
        //| "GetHashCode" -> Expression.identifier "__hash__"
        | "Equals" -> Expression.identifier "__eq__"
        | "CompareTo" -> Expression.identifier "__cmp__"
        | "set" -> Expression.identifier "__setitem__"
        | "get" -> Expression.identifier "__getitem__"
        | "has" -> Expression.identifier "__contains__"
        | "delete" -> Expression.identifier "__delitem__"
        | n when n.EndsWith("get_Count", StringComparison.Ordinal) -> Expression.identifier "__len__" // TODO: find a better way
        | n when n.StartsWith("Symbol.iterator", StringComparison.Ordinal) ->
            let name = Identifier "__iter__"
            Expression.name name
        | n ->
            let n = Naming.toPythonNaming n

            (n, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun _ -> false)
            |> Expression.name

    let get (com: IPythonCompiler) ctx _r left memberName subscript =
        // printfn "get: %A" (memberName, subscript)
        match subscript with
        | true ->
            let expr = Expression.stringConstant memberName
            Expression.subscript (value = left, slice = expr, ctx = Load)
        | _ ->
            let expr = com.GetIdentifier(ctx, memberName)
            Expression.attribute (value = left, attr = expr, ctx = Load)

    let getExpr _com _ctx _r (object: Expression) (expr: Expression) =
        match expr with
        | Expression.Constant(value = StringLiteral name) ->
            let name = name |> Identifier
            Expression.attribute (value = object, attr = name, ctx = Load), []
        | e -> Expression.subscript (value = object, slice = e, ctx = Load), []

    let rec getParts com ctx (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m :: ms -> get com ctx None expr m false |> getParts com ctx ms

    let arrayExpr (com: IPythonCompiler) ctx (expr: Expression) kind typ : Expression =
        // printfn "arrayExpr: %A" typ

        let array_type =
            // printfn "Array type: %A" (kind, typ)

            match kind, typ with
            | Fable.ResizeArray, _ -> None
            | _, Fable.Type.Number(UInt8, _) -> Some "byte"
            | _, Fable.Type.Number(Int8, _) -> Some "sbyte"
            | _, Fable.Type.Number(Int16, _) -> Some "int16"
            | _, Fable.Type.Number(UInt16, _) -> Some "uint16"
            | _, Fable.Type.Number(Int32, _) -> Some "int32"
            | _, Fable.Type.Number(UInt32, _) -> Some "uint32"
            | _, Fable.Type.Number(Int64, _) -> Some "int64"
            | _, Fable.Type.Number(UInt64, _) -> Some "uint64"
            | _, Fable.Type.Number(Float32, _) -> Some "float32"
            | _, Fable.Type.Number(Float64, _) -> Some "float64"
            | _ -> Some "Any"

        // printfn "Array type: %A" array_type

        match array_type with
        | Some l ->
            let array = libValue com ctx "array_" "Array"

            let type_obj =
                if l = "Any" then
                    com.GetImportExpr(ctx, "typing", "Any")
                else
                    libValue com ctx "types" l

            let types_array = Expression.subscript (value = array, slice = type_obj, ctx = Load)
            Expression.call (types_array, [ expr ])
        | None -> expr // <-- Fix: just return expr for ResizeArray

    /// Creates an array from a list of Fable expressions.
    /// Use this when you have multiple expressions that should become array elements.
    /// Example: [1; 2; 3] -> Array[int32]([int32(1), int32(2), int32(3)])
    let makeArray (com: IPythonCompiler) ctx exprs kind typ : Expression * Statement list =
        // printfn "makeArray: %A" typ

        let exprs, stmts =
            exprs |> List.map (fun e -> com.TransformAsExpr(ctx, e)) |> Helpers.unzipArgs

        arrayExpr com ctx (Expression.list exprs) kind typ, stmts

    let makeArrayAllocated (com: IPythonCompiler) ctx typ _kind (size: Fable.Expr) =
        // printfn "makeArrayAllocated: %A" (typ, size)

        let size, stmts = com.TransformAsExpr(ctx, size)
        let array = Expression.list [ Expression.intConstant 0 ]
        Expression.binOp (array, Mult, size), stmts

    /// Creates an array from a single Fable expression.
    /// Use this when you have one expression that should be converted to an array.
    /// For literals like [1; 2; 3], it delegates to makeArray.
    /// For other expressions, it transforms the expression and wraps it in an array.
    /// Example: someList -> Array[int32](someList)
    let makeArrayFrom (com: IPythonCompiler) ctx typ kind (fableExpr: Fable.Expr) : Expression * Statement list =
        // printfn "makeArrayFrom: %A" (fableExpr, typ, kind)

        match fableExpr with
        | Replacements.Util.ArrayOrListLiteral(exprs, _) -> makeArray com ctx exprs kind typ
        | _ ->
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            arrayExpr com ctx expr kind typ, stmts

    /// Creates a Python list from Fable expressions.
    /// Note: This creates a plain Python list, not a Fable array.
    /// Use makeArray when you need a Fable array that can be passed to functions like ofArray.
    /// Example: [1; 2; 3] -> [int32(1), int32(2), int32(3)]
    let makeList (com: IPythonCompiler) ctx exprs =
        let expr, stmts =
            exprs |> List.map (fun e -> com.TransformAsExpr(ctx, e)) |> Helpers.unzipArgs

        expr |> Expression.list, stmts

    let makeTuple (com: IPythonCompiler) ctx exprs =
        let expr, stmts =
            exprs |> List.map (fun e -> com.TransformAsExpr(ctx, e)) |> Helpers.unzipArgs

        expr |> Expression.tuple, stmts

    let makeStringArray strings =
        strings |> List.map (fun x -> Expression.stringConstant x) |> Expression.list

    let makePyObject (pairs: seq<string * Expression>) =
        pairs
        |> Seq.map (fun (name, value) ->
            let prop = Expression.stringConstant name
            prop, value
        )
        |> Seq.toList
        |> List.unzip
        |> Expression.dict

    let assign range left right =
        Expression.namedExpr (left, right, ?loc = range)

    let multiVarDeclaration (ctx: Context) (variables: (Identifier * Expression option) list) =
        // printfn "multiVarDeclaration: %A" (variables)
        let ids, values =
            variables
            |> List.distinctBy (fun (Identifier(name = name), _value) -> name)
            |> List.map (
                function
                | i, Some value -> Expression.name (i, Store), value, i
                | i, _ -> Expression.name (i, Store), Expression.none, i
            )
            |> List.unzip3
            |> fun (ids, values, ids') ->
                ctx.BoundVars.Bind(ids')
                (Expression.tuple ids, Expression.tuple values)

        [ Statement.assign ([ ids ], values) ]

    let varDeclaration (ctx: Context) (var: Expression) (typ: Expression option) value =
        // printfn "varDeclaration: %A" (var, value, typ)
        match var with
        | Name { Id = id } -> do ctx.BoundVars.Bind([ id ])
        | _ -> ()

        [
            match typ with
            | Some typ -> Statement.assign (var, annotation = typ, value = value)
            | _ -> Statement.assign ([ var ], value)
        ]

    let restElement (var: Identifier) =
        let var = Expression.name var
        Expression.starred var

    let callSuper (args: Expression list) =
        let super = Expression.name "super().__init__"
        Expression.call (super, args)

    let callSuperAsStatement (args: Expression list) = Statement.expr (callSuper args)

    let getDefaultValueForType (com: IPythonCompiler) (ctx: Context) (t: Fable.Type) : Expression =
        match t with
        | Fable.Boolean -> Expression.boolConstant false
        | Fable.Number(kind, _) ->
            match kind with
            | Int8 -> makeInteger com ctx None t "int8" (0uy :> obj) |> fst
            | UInt8 -> makeInteger com ctx None t "uint8" (0uy :> obj) |> fst
            | Int16 -> makeInteger com ctx None t "int16" (0s :> obj) |> fst
            | UInt16 -> makeInteger com ctx None t "uint16" (0us :> obj) |> fst
            | Int32 -> makeInteger com ctx None t "int32" (0 :> obj) |> fst
            | UInt32 -> makeInteger com ctx None t "uint32" (0u :> obj) |> fst
            | Int64 -> makeInteger com ctx None t "int64" (0L :> obj) |> fst
            | UInt64 -> makeInteger com ctx None t "uint64" (0UL :> obj) |> fst
            | Int128
            | UInt128
            | BigInt
            | NativeInt
            | UNativeInt -> Expression.intConstant 0
            | Float16 -> makeFloat com ctx None t "float32" 0.0 |> fst
            | Float32 -> makeFloat com ctx None t "float32" 0.0 |> fst
            | Float64 -> makeFloat com ctx None t "float64" 0.0 |> fst
            | Decimal -> makeFloat com ctx None t "float64" 0.0 |> fst
        | Fable.String
        | Fable.Char -> Expression.stringConstant ""
        | Fable.DeclaredType(ent, _) -> Expression.none
        | _ -> Expression.none

    /// Extract initialization value from constructor body by looking for backing field assignments
    let tryExtractInitializationValueFromConstructor
        (com: IPythonCompiler)
        (ctx: Context)
        (constructorBody: Fable.Expr)
        (propertyName: string)
        : (Expression * Statement list) option
        =
        let backingFieldName = propertyName + "@"

        let rec findAssignment expr =
            match expr with
            | Fable.Sequential exprs -> exprs |> List.tryPick findAssignment
            | Fable.Set(_, Fable.FieldSet fieldName, _, value, _) when fieldName = backingFieldName ->
                let expr, stmts = com.TransformAsExpr(ctx, value)
                Some(expr, stmts)
            | Fable.Set(Fable.Get(_, Fable.FieldGet { Name = fieldName }, _, _), Fable.FieldSet fieldName2, _, value, _) when
                fieldName = backingFieldName
                ->
                let expr, stmts = com.TransformAsExpr(ctx, value)
                Some(expr, stmts)
            | Fable.Set(Fable.Get(_, Fable.FieldGet { Name = fieldName }, _, _), Fable.ValueSet, _, value, _) when
                fieldName = backingFieldName
                ->
                let expr, stmts = com.TransformAsExpr(ctx, value)
                Some(expr, stmts)
            | _ -> None

        findAssignment constructorBody

    let makeClassConstructor
        (args: Arguments)
        (isOptional: bool)
        (fieldTypes: Fable.Type list option)
        (com: IPythonCompiler)
        (ctx: Context)
        body
        =
        // printfn "makeClassConstructor: %A" (args.Args, body)
        let name = Identifier "__init__"
        let self = Arg.arg "self"

        let args_ =
            match args.Args, fieldTypes with
            | [ _unit ], Some types when isOptional ->
                let defaults = types |> List.map (getDefaultValueForType com ctx)

                { args with
                    Args = self :: args.Args
                    Defaults = defaults
                }
            | _, Some types when isOptional ->
                let defaults = types |> List.map (getDefaultValueForType com ctx)

                { args with
                    Args = self :: args.Args
                    Defaults = defaults
                }
            | [ _unit ], None when isOptional ->
                { args with
                    Args = self :: args.Args
                    Defaults = [ Expression.none ]
                }
            | _ -> { args with Args = self :: args.Args }

        match args.Args, body with
        | [], []
        | [], [ Statement.Pass ] -> [] // Remove empty `__init__` with no arguments
        | _ -> [ Statement.functionDef (name, args_, body = body, returns = Expression.none) ]

    let callFunction r funcExpr (args: Expression list) (kw: Keyword list) =
        Expression.call (funcExpr, args, kw = kw, ?loc = r)

    let callFunctionWithThisContext com ctx r funcExpr (args: Expression list) =
        let args = thisExpr :: args
        Expression.call (get com ctx None funcExpr "call" false, args, ?loc = r)

    let emitExpression range (txt: string) args =
        let value =
            match txt with
            | "$0.join('')" -> "''.join($0)"
            | "throw $0" -> "raise $0"
            | Naming.StartsWith "void " value
            | Naming.StartsWith "new " value -> value
            | _ -> txt

        Expression.emit (value, args, ?loc = range)

    let undefined _range : Expression = Expression.none


    // Active patterns for type matching
    let (|IEnumerableOfKeyValuePair|_|) (targetType: Fable.Type, sourceExpr: Fable.Expr) =
        match targetType, sourceExpr.Type with
        | Fable.DeclaredType(ent, [ Fable.DeclaredType(kvpEnt, [ _; _ ]) ]), Fable.DeclaredType(sourceEnt, _) when
            ent.FullName = Types.ienumerableGeneric
            && kvpEnt.FullName = Types.keyValuePair
            && sourceEnt.FullName = Types.dictionary
            ->
            Some(kvpEnt)
        | _ -> None

    let makeFieldGet (expr: Expression) (field: string) =
        Expression.attribute (expr, Identifier field, ctx = Load), []

    let makeInteger (com: IPythonCompiler) (ctx: Context) r _t intName (x: obj) =
        let cons = libValue com ctx "types" intName
        let value = Expression.intConstant (x, ?loc = r)

        // Added support for a few selected literals for performance reasons
        match intName, x with
        | _, (:? int as i) when i = 0 -> makeFieldGet cons "ZERO"
        | _, (:? int as i) when i = 1 -> makeFieldGet cons "ONE"
        | _, (:? int as i) when i = -1 -> makeFieldGet cons "NEG_ONE"
        | _, (:? int as i) when i = 2 -> makeFieldGet cons "TWO"
        | _, (:? int as i) when i = 3 -> makeFieldGet cons "THREE"
        | _, (:? int as i) when i = 4 -> makeFieldGet cons "FOUR"
        | _, (:? int as i) when i = 5 -> makeFieldGet cons "FIVE"
        | _, (:? int as i) when i = 6 -> makeFieldGet cons "SIX"
        | _, (:? int as i) when i = 7 -> makeFieldGet cons "SEVEN"
        | _, (:? int as i) when i = 8 -> makeFieldGet cons "EIGHT"
        | _, (:? int as i) when i = 9 -> makeFieldGet cons "NINE"
        | _, (:? int as i) when i = 10 -> makeFieldGet cons "TEN"
        | _, (:? int as i) when i = 16 -> makeFieldGet cons "SIXTEEN"
        | _, (:? int as i) when i = 32 -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? int as i) when i = 64 -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? int8 as i) when i = 0y -> makeFieldGet cons "ZERO"
        | _, (:? int8 as i) when i = 1y -> makeFieldGet cons "ONE"
        | _, (:? int8 as i) when i = -1y -> makeFieldGet cons "NEG_ONE"
        | _, (:? int8 as i) when i = 2y -> makeFieldGet cons "TWO"
        | _, (:? int8 as i) when i = 3y -> makeFieldGet cons "THREE"
        | _, (:? int8 as i) when i = 4y -> makeFieldGet cons "FOUR"
        | _, (:? int8 as i) when i = 5y -> makeFieldGet cons "FIVE"
        | _, (:? int8 as i) when i = 6y -> makeFieldGet cons "SIX"
        | _, (:? int8 as i) when i = 7y -> makeFieldGet cons "SEVEN"
        | _, (:? int8 as i) when i = 8y -> makeFieldGet cons "EIGHT"
        | _, (:? int8 as i) when i = 9y -> makeFieldGet cons "NINE"
        | _, (:? int8 as i) when i = 10y -> makeFieldGet cons "TEN"
        | _, (:? int8 as i) when i = 16y -> makeFieldGet cons "SIXTEEN"
        | _, (:? int8 as i) when i = 32y -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? int8 as i) when i = 64y -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? uint8 as i) when i = 0uy -> makeFieldGet cons "ZERO"
        | _, (:? uint8 as i) when i = 1uy -> makeFieldGet cons "ONE"
        | _, (:? uint8 as i) when i = 2uy -> makeFieldGet cons "TWO"
        | _, (:? uint8 as i) when i = 3uy -> makeFieldGet cons "THREE"
        | _, (:? uint8 as i) when i = 4uy -> makeFieldGet cons "FOUR"
        | _, (:? uint8 as i) when i = 5uy -> makeFieldGet cons "FIVE"
        | _, (:? uint8 as i) when i = 6uy -> makeFieldGet cons "SIX"
        | _, (:? uint8 as i) when i = 7uy -> makeFieldGet cons "SEVEN"
        | _, (:? uint8 as i) when i = 8uy -> makeFieldGet cons "EIGHT"
        | _, (:? uint8 as i) when i = 9uy -> makeFieldGet cons "NINE"
        | _, (:? uint8 as i) when i = 10uy -> makeFieldGet cons "TEN"
        | _, (:? uint8 as i) when i = 16uy -> makeFieldGet cons "SIXTEEN"
        | _, (:? uint8 as i) when i = 32uy -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? uint8 as i) when i = 64uy -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? int16 as i) when i = 0s -> makeFieldGet cons "ZERO"
        | _, (:? int16 as i) when i = 1s -> makeFieldGet cons "ONE"
        | _, (:? int16 as i) when i = -1s -> makeFieldGet cons "NEG_ONE"
        | _, (:? int16 as i) when i = 2s -> makeFieldGet cons "TWO"
        | _, (:? int16 as i) when i = 3s -> makeFieldGet cons "THREE"
        | _, (:? int16 as i) when i = 4s -> makeFieldGet cons "FOUR"
        | _, (:? int16 as i) when i = 5s -> makeFieldGet cons "FIVE"
        | _, (:? int16 as i) when i = 6s -> makeFieldGet cons "SIX"
        | _, (:? int16 as i) when i = 7s -> makeFieldGet cons "SEVEN"
        | _, (:? int16 as i) when i = 8s -> makeFieldGet cons "EIGHT"
        | _, (:? int16 as i) when i = 9s -> makeFieldGet cons "NINE"
        | _, (:? int16 as i) when i = 10s -> makeFieldGet cons "TEN"
        | _, (:? int16 as i) when i = 16s -> makeFieldGet cons "SIXTEEN"
        | _, (:? int16 as i) when i = 32s -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? int16 as i) when i = 64s -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? uint16 as i) when i = 0us -> makeFieldGet cons "ZERO"
        | _, (:? uint16 as i) when i = 1us -> makeFieldGet cons "ONE"
        | _, (:? uint16 as i) when i = 2us -> makeFieldGet cons "TWO"
        | _, (:? uint16 as i) when i = 3us -> makeFieldGet cons "THREE"
        | _, (:? uint16 as i) when i = 4us -> makeFieldGet cons "FOUR"
        | _, (:? uint16 as i) when i = 5us -> makeFieldGet cons "FIVE"
        | _, (:? uint16 as i) when i = 6us -> makeFieldGet cons "SIX"
        | _, (:? uint16 as i) when i = 7us -> makeFieldGet cons "SEVEN"
        | _, (:? uint16 as i) when i = 8us -> makeFieldGet cons "EIGHT"
        | _, (:? uint16 as i) when i = 9us -> makeFieldGet cons "NINE"
        | _, (:? uint16 as i) when i = 10us -> makeFieldGet cons "TEN"
        | _, (:? uint16 as i) when i = 16us -> makeFieldGet cons "SIXTEEN"
        | _, (:? uint16 as i) when i = 32us -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? uint16 as i) when i = 64us -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? uint32 as i) when i = 0u -> makeFieldGet cons "ZERO"
        | _, (:? uint32 as i) when i = 1u -> makeFieldGet cons "ONE"
        | _, (:? uint32 as i) when i = 2u -> makeFieldGet cons "TWO"
        | _, (:? uint32 as i) when i = 3u -> makeFieldGet cons "THREE"
        | _, (:? uint32 as i) when i = 4u -> makeFieldGet cons "FOUR"
        | _, (:? uint32 as i) when i = 5u -> makeFieldGet cons "FIVE"
        | _, (:? uint32 as i) when i = 6u -> makeFieldGet cons "SIX"
        | _, (:? uint32 as i) when i = 7u -> makeFieldGet cons "SEVEN"
        | _, (:? uint32 as i) when i = 8u -> makeFieldGet cons "EIGHT"
        | _, (:? uint32 as i) when i = 9u -> makeFieldGet cons "NINE"
        | _, (:? uint32 as i) when i = 10u -> makeFieldGet cons "TEN"
        | _, (:? uint32 as i) when i = 16u -> makeFieldGet cons "SIXTEEN"
        | _, (:? uint32 as i) when i = 32u -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? uint32 as i) when i = 64u -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? int64 as i) when i = 0L -> makeFieldGet cons "ZERO"
        | _, (:? int64 as i) when i = 1L -> makeFieldGet cons "ONE"
        | _, (:? int64 as i) when i = -1L -> makeFieldGet cons "NEG_ONE"
        | _, (:? int64 as i) when i = 2L -> makeFieldGet cons "TWO"
        | _, (:? int64 as i) when i = 3L -> makeFieldGet cons "THREE"
        | _, (:? int64 as i) when i = 4L -> makeFieldGet cons "FOUR"
        | _, (:? int64 as i) when i = 5L -> makeFieldGet cons "FIVE"
        | _, (:? int64 as i) when i = 6L -> makeFieldGet cons "SIX"
        | _, (:? int64 as i) when i = 7L -> makeFieldGet cons "SEVEN"
        | _, (:? int64 as i) when i = 8L -> makeFieldGet cons "EIGHT"
        | _, (:? int64 as i) when i = 9L -> makeFieldGet cons "NINE"
        | _, (:? int64 as i) when i = 10L -> makeFieldGet cons "TEN"
        | _, (:? int64 as i) when i = 16L -> makeFieldGet cons "SIXTEEN"
        | _, (:? int64 as i) when i = 32L -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? int64 as i) when i = 64L -> makeFieldGet cons "SIXTY_FOUR"
        | _, (:? uint64 as i) when i = 0UL -> makeFieldGet cons "ZERO"
        | _, (:? uint64 as i) when i = 1UL -> makeFieldGet cons "ONE"
        | _, (:? uint64 as i) when i = 2UL -> makeFieldGet cons "TWO"
        | _, (:? uint64 as i) when i = 3UL -> makeFieldGet cons "THREE"
        | _, (:? uint64 as i) when i = 4UL -> makeFieldGet cons "FOUR"
        | _, (:? uint64 as i) when i = 5UL -> makeFieldGet cons "FIVE"
        | _, (:? uint64 as i) when i = 6UL -> makeFieldGet cons "SIX"
        | _, (:? uint64 as i) when i = 7UL -> makeFieldGet cons "SEVEN"
        | _, (:? uint64 as i) when i = 8UL -> makeFieldGet cons "EIGHT"
        | _, (:? uint64 as i) when i = 9UL -> makeFieldGet cons "NINE"
        | _, (:? uint64 as i) when i = 10UL -> makeFieldGet cons "TEN"
        | _, (:? uint64 as i) when i = 16UL -> makeFieldGet cons "SIXTEEN"
        | _, (:? uint64 as i) when i = 32UL -> makeFieldGet cons "THIRTY_TWO"
        | _, (:? uint64 as i) when i = 64UL -> makeFieldGet cons "SIXTY_FOUR"
        | _ -> Expression.call (cons, [ value ], ?loc = r), []

    let makeFloat (com: IPythonCompiler) (ctx: Context) r _t floatName x =
        let cons = libValue com ctx "types" floatName
        let value = Expression.floatConstant (x, ?loc = r)
        Expression.call (cons, [ value ], ?loc = r), []


    let enumerator2iterator com ctx =
        let enumerator =
            Expression.call (get com ctx None (Expression.identifier "self") "GetEnumerator" false, [])

        [ Statement.return' (libCall com ctx None "util" "to_iterator" [ enumerator ]) ]


/// Common utilities for Python transformations
module Helpers =
    /// Returns true if the first field type can be None in Python
    let isOptional (fields: Fable.Ident[]) =
        if fields.Length < 1 then
            false
        else
            match fields[0].Type with
            | Fable.GenericParam _ -> true
            | Fable.Option _ -> true
            | Fable.Unit -> true
            | Fable.Any -> true
            | _ -> false

    let removeNamespace (fullName: string) =
        fullName.Split('.')
        |> Array.last
        |> (fun name -> name.Replace("`", "_"))
        |> Helpers.clean

    let getUniqueIdentifier (name: string) : Identifier =
        let idx = Naming.getUniqueIndex ()

        let deliminator =
            if Char.IsLower name[0] then
                "_"
            else
                ""

        Identifier($"%s{name}%s{deliminator}%i{idx}")

    /// Replaces all '$' and `.`with '_'
    let clean (name: string) =
        (name, Fable.Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun _ -> false)

    /// Unzips a list of (Expression * Statement list) pairs into separate lists
    let unzipArgs (args: (Expression * Statement list) list) : Expression list * Statement list =
        let stmts = args |> List.map snd |> List.collect id
        let args = args |> List.map fst
        args, stmts

    /// A few statements in the generated Python AST do not produce any effect,
    /// and should not be printed.
    let isProductiveStatement (stmt: Statement) =
        let rec hasNoSideEffects (e: Expression) =
            match e with
            | Constant _ -> true
            | Dict { Keys = keys } -> keys.IsEmpty // Empty object
            | Name _ -> true // E.g `void 0` is translated to Name(None)
            | _ -> false

        match stmt with
        // Remove `self = self`
        | Statement.Assign {
                               Targets = [ Name { Id = Identifier x } ]
                               Value = Name { Id = Identifier y }
                           } when x = y -> None
        | Statement.AnnAssign {
                                  Target = Name { Id = Identifier x }
                                  Value = Some(Name { Id = Identifier y })
                              } when x = y -> None
        | Expr expr ->
            if hasNoSideEffects expr.Value then
                None
            else
                Some stmt
        | _ -> Some stmt

    let toString (e: Fable.Expr) =
        let callInfo = Fable.CallInfo.Create(args = [ e ])
        makeIdentExpr "str" |> makeCall None Fable.String callInfo


/// Expression builders with automatic statement threading
[<RequireQualifiedAccess>]
module Expression =
    // === Core Computation Expression ===

    /// Creates a computation expression for cleaner statement threading
    type WithStmtBuilder() =
        member _.Bind((expr, stmts1): Expression * Statement list, f: Expression -> Expression * Statement list) =
            let expr2, stmts2 = f expr
            expr2, stmts1 @ stmts2

        member _.Return(expr: Expression) = expr, []

        member _.ReturnFrom(result: Expression * Statement list) = result

        member _.Zero() = Expression.none, []

    let withStmts = WithStmtBuilder()

    // === Statement Threading Utilities ===

    /// Combines multiple statement lists efficiently
    let combine (stmtLists: Statement list list) : Statement list = List.collect id stmtLists

    /// Maps a function over a list and combines the resulting statements
    let mapWith (f: 'a -> 'b * Statement list) (items: 'a list) : 'b list * Statement list =
        items
        |> List.map f
        |> List.unzip
        |> fun (results, stmtLists) -> results, combine stmtLists

    /// Transforms two expressions and combines their statements
    let transform2
        (com: IPythonCompiler)
        ctx
        (expr1: Fable.Expr)
        (expr2: Fable.Expr)
        : Expression * Expression * Statement list
        =
        let e1, stmts1 = com.TransformAsExpr(ctx, expr1)
        let e2, stmts2 = com.TransformAsExpr(ctx, expr2)
        e1, e2, stmts1 @ stmts2

    /// Transforms three expressions and combines their statements
    let transform3
        (com: IPythonCompiler)
        ctx
        (expr1: Fable.Expr)
        (expr2: Fable.Expr)
        (expr3: Fable.Expr)
        : Expression * Expression * Expression * Statement list
        =
        let e1, stmts1 = com.TransformAsExpr(ctx, expr1)
        let e2, stmts2 = com.TransformAsExpr(ctx, expr2)
        let e3, stmts3 = com.TransformAsExpr(ctx, expr3)
        e1, e2, e3, combine [ stmts1; stmts2; stmts3 ]

    /// Transforms an optional expression
    let transformOpt (com: IPythonCompiler) ctx (expr: Fable.Expr option) : Expression option * Statement list =
        match expr with
        | Some e ->
            let expr, stmts = com.TransformAsExpr(ctx, e)
            Some expr, stmts
        | None -> None, []

    /// Lifts a pure expression (no statements) into the statement threading context
    let liftPure (expr: Expression) : Expression * Statement list = expr, []

    /// Adds statements to an existing expression result
    let addStmts
        (stmts: Statement list)
        (expr: Expression, existingStmts: Statement list)
        : Expression * Statement list
        =
        expr, existingStmts @ stmts

    /// Pipes an expression through a pure transformation while preserving statements
    let mapExpr (f: Expression -> Expression) (expr: Expression, stmts: Statement list) : Expression * Statement list =
        f expr, stmts

    /// Conditional transformation - applies function only if condition is true
    let transformIf
        (condition: bool)
        (f: Expression -> Expression)
        (expr: Expression, stmts: Statement list)
        : Expression * Statement list
        =
        if condition then
            f expr, stmts
        else
            expr, stmts

    // === High-Level Expression Builders ===

    /// Builds a binary operation with statement threading
    let buildBinOp
        (com: IPythonCompiler)
        ctx
        (left: Fable.Expr)
        (op: BinaryOperator)
        (right: Fable.Expr)
        : Expression * Statement list
        =
        let left, right, stmts = transform2 com ctx left right
        Expression.binOp (left, op, right), stmts

    /// Builds a comparison with statement threading
    let buildComparison
        (com: IPythonCompiler)
        ctx
        (left: Fable.Expr)
        (ops: ComparisonOperator list)
        (rights: Fable.Expr list)
        : Expression * Statement list
        =
        let left, stmts1 = com.TransformAsExpr(ctx, left)
        let rights, stmts2 = mapWith (fun expr -> com.TransformAsExpr(ctx, expr)) rights
        Expression.compare (left, ops, rights), stmts1 @ stmts2

    /// Builds a function call with statement threading
    let buildCall (com: IPythonCompiler) ctx (func: Fable.Expr) (args: Fable.Expr list) : Expression * Statement list =
        let func, stmts1 = com.TransformAsExpr(ctx, func)
        let args, stmts2 = mapWith (fun expr -> com.TransformAsExpr(ctx, expr)) args
        Expression.call (func, args), stmts1 @ stmts2

    /// Creates a Python list from Fable expressions with statement threading
    let makeList (com: IPythonCompiler) ctx exprs =
        let expr, stmts = mapWith (fun e -> com.TransformAsExpr(ctx, e)) exprs
        expr |> Expression.list, stmts

    /// Creates a Python tuple from Fable expressions with statement threading
    let makeTuple (com: IPythonCompiler) ctx exprs =
        let expr, stmts = mapWith (fun e -> com.TransformAsExpr(ctx, e)) exprs
        expr |> Expression.tuple, stmts

    // === Enhanced Active Patterns ===

    /// Active pattern for transforming expressions with better ergonomics
    let (|TransformExpr2|) (com: IPythonCompiler) ctx (e1: Fable.Expr) (e2: Fable.Expr) = transform2 com ctx e1 e2

    let (|TransformExpr3|) (com: IPythonCompiler) ctx (e1: Fable.Expr) (e2: Fable.Expr) (e3: Fable.Expr) =
        transform3 com ctx e1 e2 e3

    /// Active pattern for transforming optional expressions
    let (|TransformExprOpt|) (com: IPythonCompiler) ctx (expr: Fable.Expr option) = transformOpt com ctx expr

    /// Active pattern for transforming lists of expressions
    let (|TransformExprList|) (com: IPythonCompiler) ctx (exprs: Fable.Expr list) =
        mapWith (fun expr -> com.TransformAsExpr(ctx, expr)) exprs
