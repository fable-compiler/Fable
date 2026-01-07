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

    /// Ensures a statement list is non-empty by adding Pass if needed.
    /// Python requires at least one statement in function/class/match bodies.
    let ensureNonEmptyBody stmts =
        match stmts with
        | [] -> [ Statement.Pass ]
        | _ -> stmts

    /// Extract the element type from an array type for vararg annotations.
    /// For Array<T>, returns T. For other types, returns Any as fallback.
    let getVarArgElementType (paramType: Fable.Type) : Fable.Type =
        match paramType with
        | Fable.Array(elemType, _) -> elemType
        | _ -> Fable.Any // Fallback if not array type

    /// Splits a list of items into regular items and an optional vararg item.
    /// When hasSpread is true and items is non-empty, the last item becomes the vararg.
    let splitVarArg<'T> (hasSpread: bool) (items: 'T list) : 'T list * 'T option =
        if hasSpread && not items.IsEmpty then
            let regular = items |> List.take (items.Length - 1)
            let vararg = items |> List.last
            regular, Some vararg
        else
            items, None

    /// Adjusts Arguments to move the last arg to vararg when hasSpread is true.
    /// Removes the annotation from vararg since Python infers it from *args.
    let adjustArgsForSpread (hasSpread: bool) (args: Arguments) : Arguments =
        let len = args.Args.Length

        if not hasSpread || len = 0 then
            args
        else
            { args with
                VarArg = Some { args.Args[len - 1] with Annotation = None }
                Args = args.Args[.. len - 2]
            }

    let hasAttribute fullName (atts: Fable.Attribute seq) =
        atts |> Seq.exists (fun att -> att.Entity.FullName = fullName)

    let hasAnyEmitAttribute (atts: Fable.Attribute seq) =
        hasAttribute Atts.emitAttr atts
        || hasAttribute Atts.emitMethod atts
        || hasAttribute Atts.emitConstructor atts
        || hasAttribute Atts.emitIndexer atts
        || hasAttribute Atts.emitProperty atts

    /// Check if an Option type has a concrete inner type (erases to T | None).
    /// Returns true when inner type is concrete, meaning Option[T] -> T | None.
    let hasConcreteOptionInner (typ: Fable.Type) =
        match typ with
        | Fable.Option(innerType, _) -> not (mustWrapOption innerType)
        | _ -> false

    /// Check if a call expression needs Option erase from Option[T] to T | None.
    /// When target type is Option<ConcreteType> (erased to T | None), we need to erase
    /// because library functions use Option[T] (wrapped form) in their signatures,
    /// but actual runtime values are not wrapped for concrete types.
    let private needsOptionEraseForCall (targetType: Fable.Type) =
        match targetType with
        | Fable.Option(tgtInner, _) when not (mustWrapOption tgtInner) -> true
        | _ -> false

    /// Check if binding needs Option erase from Option[T] to T | None.
    let needsOptionEraseForBinding (value: Fable.Expr) (targetType: Fable.Type) =
        match value with
        | Fable.Call _ -> needsOptionEraseForCall targetType
        | _ -> false

    /// Check if return expression needs Option erase from Option[T] to T | None.
    /// Used when returning from a function where the expected return type is T | None
    /// but the actual expression returns Option[T] (wrapped form).
    let needsOptionEraseForReturn (value: Fable.Expr) (expectedReturnType: Fable.Type) =
        match value with
        | Fable.Call _ -> needsOptionEraseForCall expectedReturnType
        | _ -> false

    /// Get narrowed contexts for then/else branches based on a guard expression
    /// Returns (thenCtx, elseCtx) with appropriate type narrowing applied
    let getNarrowedContexts (ctx: Context) (guardExpr: Fable.Expr) : Context * Context =
        match guardExpr with
        | Fable.Test(Fable.IdentExpr ident, Fable.TypeTest typ, _) ->
            // TypeTest: then branch has the narrowed type
            { ctx with NarrowedTypes = Map.add ident.Name typ ctx.NarrowedTypes }, ctx
        | Fable.Test(Fable.IdentExpr ident, Fable.OptionTest nonEmpty, _) ->
            // OptionTest: only narrow for erased options (T | None), not wrapped options (Option[T])
            // Wrapped options require value() call to unwrap, so the type doesn't change
            match ident.Type with
            | Fable.Option(innerType, _) when not (mustWrapOption innerType) ->
                // Erased option: type narrows from T | None to T
                if nonEmpty then
                    // v is not None: then branch has the unwrapped type
                    { ctx with NarrowedTypes = Map.add ident.Name innerType ctx.NarrowedTypes }, ctx
                else
                    // v is None: else branch has the unwrapped type
                    ctx, { ctx with NarrowedTypes = Map.add ident.Name innerType ctx.NarrowedTypes }
            | _ ->
                // Wrapped option or non-option: no type narrowing
                ctx, ctx
        | _ -> ctx, ctx

    /// Recursively check if a type contains Option with generic parameter that requires wrapping.
    /// This checks return types of lambdas for Option[GenericParam].
    let rec private hasWrappedOptionInReturnType (typ: Fable.Type) =
        match typ with
        | Fable.LambdaType(_, returnType) ->
            // Check if return type is Option[GenericParam] or recurse into nested lambdas
            match returnType with
            | Fable.Option(inner, _) -> mustWrapOption inner
            | Fable.LambdaType _ -> hasWrappedOptionInReturnType returnType
            | _ -> false
        | _ -> false

    /// Check if a type would have its Option return type erased (T | None instead of Option[T]).
    /// Returns true when the return type is Option[ConcreteType] which gets erased.
    let rec private hasErasedOptionReturnType (typ: Fable.Type) =
        match typ with
        | Fable.LambdaType(_, returnType) ->
            match returnType with
            | Fable.Option(inner, _) -> not (mustWrapOption inner) // Erased when NOT wrapped
            | Fable.LambdaType _ -> hasErasedOptionReturnType returnType
            | _ -> false
        | _ -> false

    /// Check if a callback argument needs widen() to convert erased Option callback
    /// to wrapped Option form for type checker compatibility.
    /// Returns true when:
    /// - Expected type is Callable with Option[GenericParam] in return position
    /// - Actual arg is a lambda/function that would have erased Option return type
    let needsOptionWidenForArg (expectedType: Fable.Type option) (argExpr: Fable.Expr) =
        match expectedType with
        | Some sigType when hasWrappedOptionInReturnType sigType ->
            // Check if argument is a callable with ERASED Option return type
            // If arg already has wrapped Option return, don't apply widen()
            match argExpr with
            | Fable.Lambda _
            | Fable.Delegate _ ->
                // Check if the lambda's return type would be erased
                hasErasedOptionReturnType argExpr.Type
            | Fable.IdentExpr ident ->
                // Check if the identifier's type has erased Option return
                hasErasedOptionReturnType ident.Type
            | Fable.Get(_, Fable.FieldGet fieldInfo, _, _) ->
                // Field access to a function
                match fieldInfo.FieldType with
                | Some typ -> hasErasedOptionReturnType typ
                | None -> false
            | _ -> false
        | _ -> false

    /// Wraps None values in cast(type, None) for type safety.
    /// Skips if type annotation is also None (unit type).
    let wrapNoneInCast (com: IPythonCompiler) ctx (value: Expression) (typeAnnotation: Expression) : Expression =
        match value, typeAnnotation with
        | Expression.Name { Id = Identifier "None" }, Expression.Name { Id = Identifier "None" } -> value // No cast needed for None: None = None
        | Expression.Name { Id = Identifier "None" }, _ ->
            let cast = com.GetImportExpr(ctx, "typing", "cast")
            Expression.call (cast, [ typeAnnotation; value ])
        | _ -> value

    let parseClassStyle (styleStr: int) =
        match styleStr with
        | 0 -> ClassStyle.Properties
        | 1 -> ClassStyle.Attributes
        | _ -> ClassStyle.Properties // Default to Properties for unknown values

    /// Tries to find a ClassAttributesTemplateAttribute on the attribute's type
    /// and extract the style and init parameters
    let private tryGetTemplateClassAttributes (com: Fable.Compiler) (att: Fable.Attribute) =
        com.TryGetEntity(att.Entity)
        |> Option.bind (fun attEntity ->
            attEntity.Attributes
            |> Seq.tryFind (fun a -> a.Entity.FullName = Atts.pyClassAttributesTemplate)
            |> Option.bind (fun templateAtt ->
                match templateAtt.ConstructorArgs with
                | [ :? int as styleParam ] ->
                    Some
                        {
                            Style = parseClassStyle styleParam
                            Init = false // Default for single-arg constructor
                        }
                | [ :? int as styleParam; :? bool as initParam ] ->
                    Some
                        {
                            Style = parseClassStyle styleParam
                            Init = initParam
                        }
                | _ -> None
            )
        )

    let hasPythonClassAttribute (com: Fable.Compiler) (atts: Fable.Attribute seq) =
        hasAttribute Atts.pyClassAttributes atts
        || atts
           |> Seq.exists (fun att -> tryGetTemplateClassAttributes com att |> Option.isSome)

    let getPythonClassParameters (com: Fable.Compiler) (atts: Fable.Attribute seq) =
        let defaultParams = ClassAttributes.Default

        // First check for direct ClassAttributes attribute
        let directResult =
            atts
            |> Seq.tryFind (fun att -> att.Entity.FullName = Atts.pyClassAttributes)
            |> Option.map (fun att ->
                match att.ConstructorArgs with
                | [] -> defaultParams
                | [ :? int as styleParam ] -> { defaultParams with Style = parseClassStyle styleParam }
                | [ :? int as styleParam; :? bool as initParam ] ->
                    {
                        Style = parseClassStyle styleParam
                        Init = initParam
                    }
                | _ -> defaultParams
            )

        // If no direct attribute, check for template attributes
        match directResult with
        | Some result -> result
        | None ->
            atts
            |> Seq.tryPick (fun att -> tryGetTemplateClassAttributes com att)
            |> Option.defaultValue defaultParams

    /// Tries to find a DecorateTemplateAttribute on the attribute's type
    /// and format the template with the attribute's constructor args
    let private tryGetTemplateDecoratorInfo (com: Fable.Compiler) (att: Fable.Attribute) =
        com.TryGetEntity(att.Entity)
        |> Option.bind (fun attEntity ->
            attEntity.Attributes
            |> Seq.tryFind (fun a -> a.Entity.FullName = Atts.pyDecorateTemplate)
            |> Option.bind (fun templateAtt ->
                match templateAtt.ConstructorArgs with
                | [ :? string as template ] ->
                    Some
                        {
                            Decorator = String.Format(template, att.ConstructorArgs |> List.toArray)
                            Parameters = ""
                            ImportFrom = ""
                        }
                | [ :? string as template; :? string as importFrom ] ->
                    Some
                        {
                            Decorator = String.Format(template, att.ConstructorArgs |> List.toArray)
                            Parameters = ""
                            ImportFrom = importFrom
                        }
                | _ -> None
            )
        )

    /// Extracts decorator information from entity attributes
    /// Constructor args order: (decorator, importFrom, parameters)
    let getDecoratorInfo (com: Fable.Compiler) (atts: Fable.Attribute seq) =
        atts
        |> Seq.choose (fun att ->
            if att.Entity.FullName = Atts.pyDecorate then
                match att.ConstructorArgs with
                | [ :? string as decorator ] ->
                    Some
                        {
                            Decorator = decorator
                            Parameters = ""
                            ImportFrom = ""
                        }
                | [ :? string as decorator; :? string as importFrom ] ->
                    Some
                        {
                            Decorator = decorator
                            Parameters = ""
                            ImportFrom = importFrom
                        }
                | [ :? string as decorator; :? string as importFrom; :? string as parameters ] ->
                    Some
                        {
                            Decorator = decorator
                            Parameters = parameters
                            ImportFrom = importFrom
                        }
                | _ -> None // Invalid decorator
            else
                // Try to find a DecorateTemplateAttribute on this attribute's type
                tryGetTemplateDecoratorInfo com att
        )
        |> Seq.toList

    /// Generates Python decorator expressions from DecoratorInfo
    /// The decorator string is emitted verbatim. If ImportFrom is specified,
    /// an import statement is generated automatically.
    let generateDecorators (com: IPythonCompiler) (ctx: Context) (decoratorInfos: DecoratorInfo list) =
        decoratorInfos
        |> List.map (fun info ->
            // Handle import if ImportFrom is specified
            if not (String.IsNullOrEmpty info.ImportFrom) then
                // This triggers the import: from {importFrom} import {decorator}
                com.GetImportExpr(ctx, info.ImportFrom, info.Decorator) |> ignore

            if String.IsNullOrEmpty info.Parameters then
                // Simple decorator without parameters: @decorator
                Expression.emit (info.Decorator, [])
            else
                // Decorator with parameters: @decorator(param1=value1, param2=value2)
                Expression.emit ($"%s{info.Decorator}(%s{info.Parameters})", [])
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
    /// for the given entity. Returns true for user-defined F# records, false for built-in types.
    let shouldUseRecordFieldNaming (ent: Fable.Entity) =
        ent.IsFSharpRecord
        && not (ent.FullName.StartsWith("Microsoft.FSharp.Core", StringComparison.Ordinal))

    /// Determines if we should use the special record field naming convention (toRecordFieldSnakeCase)
    /// for the given entity reference. Returns true for user-defined F# records, false for built-in types.
    let shouldUseRecordFieldNamingForRef (entityRef: Fable.EntityRef) (ent: Fable.Entity) =
        ent.IsFSharpRecord
        && not (entityRef.FullName.StartsWith("Microsoft.FSharp.Core", StringComparison.Ordinal))

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

    /// Checks if a field name is an actual record field (not a property/method)
    let isRecordField (ent: Fable.Entity) (fieldName: string) =
        ent.FSharpFields |> List.exists (fun f -> f.Name = fieldName)

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
                match com.TryGetEntity entityRef with
                | Some ent when shouldUseRecordFieldNamingForRef entityRef ent && isRecordField ent fieldName ->
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
        | Fable.DecisionTree(_, targets) -> preferStatement || List.exists (snd >> isPyStatement ctx false) targets

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
        libCall com ctx None "core" "int32" [ Expression.intConstant (int i) ]

    let ofString (s: string) = Expression.stringConstant s

    let memberFromName
        (_com: IPythonCompiler)
        (_ctx: Context)
        (memberName: string)
        (_argCount: int option)
        : Expression
        =
        // Name transformations for specific interfaces (Py.Map, etc.) should be handled
        // in Replacements.fs, not here. This function just sanitizes the name.
        let n = Naming.toPythonNaming memberName

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
                    libValue com ctx "core" l

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
        | Fable.GenericParam _ -> libValue com ctx "util" "UNIT"
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
                    Defaults = [ libValue com ctx "util" "UNIT" ]
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
            && (sourceEnt.FullName = Types.dictionary || sourceEnt.FullName = Types.idictionary)
            ->
            Some(kvpEnt)
        | _ -> None

    let makeFieldGet (expr: Expression) (field: string) =
        Expression.attribute (expr, Identifier field, ctx = Load), []

    let makeInteger (com: IPythonCompiler) (ctx: Context) r _t intName (x: obj) =
        let cons = libValue com ctx "core" intName
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
        let cons = libValue com ctx "core" floatName
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

    /// Transform return statements to wrap their values with await
    let wrapReturnWithAwait (body: Statement list) : Statement list =
        body
        |> List.map (fun stmt ->
            match stmt with
            | Statement.Return { Value = Some value } -> Statement.return' (Await value)
            | other -> other
        )

    /// Unwrap Task[T] to T for async function return types
    let unwrapTaskType (returnType: Expression) : Expression =
        match returnType with
        | Subscript { Slice = innerType } -> innerType
        | _ -> returnType


/// Expression builders with automatic statement threading
[<RequireQualifiedAccess>]
module Expression =
    /// Creates a computation expression for cleaner statement threading.
    type WithStmtBuilder() =
        member _.Bind((expr, stmts1): Expression * Statement list, f: Expression -> Expression * Statement list) =
            let expr2, stmts2 = f expr
            expr2, stmts1 @ stmts2

        member _.Return(expr: Expression) = expr, []

        member _.ReturnFrom(result: Expression * Statement list) = result

        member _.Zero() = Expression.none, []

    let withStmts = WithStmtBuilder()

    /// Combines multiple statement lists efficiently
    let combine (stmtLists: Statement list list) : Statement list = List.collect id stmtLists

    /// Maps a function over a list and combines the resulting statements
    let mapWith (f: 'a -> 'b * Statement list) (items: 'a list) : 'b list * Statement list =
        items
        |> List.map f
        |> List.unzip
        |> fun (results, stmtLists) -> results, combine stmtLists

/// Utilities for extracting exception handler patterns from F# AST.
/// F# compiles try-with expressions differently depending on the number of patterns:
/// - 1-2 patterns: nested IfThenElse with TypeTest
/// - 3+ patterns: DecisionTree with targets
module ExceptionHandling =

    /// Check if an expression is a reraise of the caught exception.
    /// Note: F# may rename the catch variable internally, so we check for any Throw(IdentExpr).
    let isReraise =
        function
        | Fable.Extended(Fable.Throw(Some(Fable.IdentExpr _), _), _) -> true
        | _ -> false

    /// Extract exception handlers from a catch body that uses type tests.
    /// Returns: (handlers as (type * body) list, fallbackExpr option)
    ///
    /// Handles three patterns:
    /// - Simple: `| :? ExceptionType -> body`
    /// - Binding: `| :? ExceptionType as ex -> body`
    /// - DecisionTree: F# compiles 3+ exception patterns as a decision tree
    let rec extractExceptionHandlers (expr: Fable.Expr) : (Fable.Type * Fable.Expr) list * Fable.Expr option =
        match expr with
        // Binding pattern: | :? ExceptionType as ex -> body (check first as it's more specific)
        // F# compiles this as: IfThenElse(Test(param, TypeTest), Let(ex, TypeCast(param, typ), body), else)
        | Fable.IfThenElse(Fable.Test(Fable.IdentExpr _, Fable.TypeTest typ, _),
                           (Fable.Let(_, Fable.TypeCast(Fable.IdentExpr _, _), _) as thenExpr),
                           elseExpr,
                           _) ->
            let restHandlers, fallback = extractExceptionHandlers elseExpr
            (typ, thenExpr) :: restHandlers, fallback

        // Simple pattern: | :? ExceptionType -> body
        | Fable.IfThenElse(Fable.Test(Fable.IdentExpr _, Fable.TypeTest typ, _), thenExpr, elseExpr, _) ->
            let restHandlers, fallback = extractExceptionHandlers elseExpr
            (typ, thenExpr) :: restHandlers, fallback

        // DecisionTree pattern: F# compiles 3+ exception patterns as a decision tree
        | Fable.DecisionTree(decisionExpr, targets) ->
            // Extract type -> targetIndex mappings from the decision expression
            let rec extractFromDecision expr =
                match expr with
                | Fable.IfThenElse(Fable.Test(Fable.IdentExpr _, Fable.TypeTest typ, _),
                                   Fable.DecisionTreeSuccess(targetIdx, boundValues, _),
                                   elseExpr,
                                   _) -> (typ, targetIdx, boundValues) :: extractFromDecision elseExpr
                | Fable.DecisionTreeSuccess(targetIdx, boundValues, _) ->
                    // Wildcard/default case
                    [ (Fable.Any, targetIdx, boundValues) ]
                | _ -> []

            let typeToTarget = extractFromDecision decisionExpr

            // Map each type test to its handler body from targets
            // If there are bound values (from `as ex` pattern), wrap the body in Let bindings
            let handlers =
                typeToTarget
                |> List.choose (fun (typ, targetIdx, boundValues) ->
                    if targetIdx < List.length targets then
                        let (idents, body) = targets.[targetIdx]
                        // Wrap body with Let bindings for each bound value
                        let wrappedBody =
                            List.zip idents boundValues
                            |> List.fold (fun acc (ident, value) -> Fable.Let(ident, value, acc)) body

                        Some(typ, wrappedBody)
                    else
                        None
                )

            // Separate the wildcard (Any) from specific type handlers
            let specificHandlers = handlers |> List.filter (fun (typ, _) -> typ <> Fable.Any)

            let wildcardHandler =
                handlers |> List.tryFind (fun (typ, _) -> typ = Fable.Any) |> Option.map snd

            specificHandlers, wildcardHandler

        | _ -> [], Some expr

    /// Check if a type is System.Exception or a subtype (including exn alias).
    /// Used to identify exception types that need to be widened to BaseException for catch-all handlers.
    let private isExceptionType (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType(entRef, _) ->
            entRef.FullName = "System.Exception"
            || entRef.FullName.StartsWith("System.") && entRef.FullName.EndsWith("Exception")
        | _ -> false

    /// Create a Fable type representing Python's BaseException.
    /// This maps to the built-in BaseException class in Python.
    let private baseExceptionType =
        let entRef: Fable.EntityRef =
            {
                FullName = "BaseException"
                Path = Fable.CoreAssemblyName "builtins"
            }

        Fable.DeclaredType(entRef, [])

    /// Rewrite exception-typed bindings in a fallback expression to use BaseException type.
    /// This is needed because Python's BaseException is broader than Exception,
    /// and type checkers reject `ex: Exception = <BaseException>`.
    let rec widenExceptionTypes (expr: Fable.Expr) : Fable.Expr =
        match expr with
        | Fable.Let(ident, value, body) when isExceptionType ident.Type ->
            // Change the ident's type to BaseException for correct Python typing
            let widenedIdent = { ident with Type = baseExceptionType }
            Fable.Let(widenedIdent, value, widenExceptionTypes body)
        | Fable.Let(ident, value, body) -> Fable.Let(ident, value, widenExceptionTypes body)
        | _ -> expr

/// Utilities for Python match statement generation (PEP 634).
/// These helpers transform F# decision trees into Python 3.10+ match/case statements.
module MatchStatements =
    open Fable.Transforms

    /// Converts a Fable constant expression to a Python Pattern for match statements.
    /// Returns None if the value cannot be converted to a pattern.
    let fableValueToPattern (value: Fable.Expr) : Pattern option =
        match value with
        | Fable.Value(Fable.CharConstant c, _) -> Some(MatchValue(Expression.stringConstant (string<char> c)))
        | Fable.Value(Fable.StringConstant s, _) -> Some(MatchValue(Expression.stringConstant s))
        | Fable.Value(Fable.NumberConstant(v, _), _) ->
            match v with
            | Fable.NumberValue.Int8 x -> Some(MatchValue(Expression.intConstant x))
            | Fable.NumberValue.UInt8 x -> Some(MatchValue(Expression.intConstant x))
            | Fable.NumberValue.Int16 x -> Some(MatchValue(Expression.intConstant x))
            | Fable.NumberValue.UInt16 x -> Some(MatchValue(Expression.intConstant x))
            | Fable.NumberValue.Int32 x -> Some(MatchValue(Expression.intConstant x))
            | Fable.NumberValue.UInt32 x -> Some(MatchValue(Expression.intConstant x))
            | Fable.NumberValue.Float32 x -> Some(MatchValue(Expression.floatConstant (float x)))
            | Fable.NumberValue.Float64 x -> Some(MatchValue(Expression.floatConstant x))
            | _ -> None
        | Fable.Value(Fable.BoolConstant b, _) -> Some(MatchSingleton(BoolLiteral b))
        | Fable.Value(Fable.Null _, _) -> Some(MatchSingleton NoneLiteral)
        | _ -> None

    /// Active pattern to detect guard let bindings in F# pattern matching (debug mode).
    /// Handles F# guards like `| n when n > 10 -> ...` which compile to:
    ///   Let(n, IdentExpr(x), guardBody)
    /// where guardBody uses n (e.g., n > 10)
    /// Returns Some(subject, param, guardBody) if the pattern matches.
    [<return: Struct>]
    let (|GuardLetBinding|_|) =
        function
        | Fable.Let(param, Fable.IdentExpr subject, guardBody) -> ValueSome(subject, param, guardBody)
        | _ -> ValueNone

    /// Extracts the subject identifier from a binary comparison operation.
    /// In release mode, guards like `n when n > 10` are inlined as direct comparisons.
    /// Returns Some(subjectIdent) if we can identify the subject being compared.
    let private tryExtractSubjectFromComparison (expr: Fable.Expr) : Fable.Ident option =
        match expr with
        | Fable.Operation(Fable.Binary(_, Fable.IdentExpr ident, _), _, _, _) -> Some ident
        | Fable.Operation(Fable.Binary(_, _, Fable.IdentExpr ident), _, _, _) -> Some ident
        | Fable.Operation(Fable.Logical(_, Fable.IdentExpr ident, _), _, _, _) -> Some ident
        | Fable.Operation(Fable.Logical(_, _, Fable.IdentExpr ident), _, _, _) -> Some ident
        | _ -> None

    /// Simplified guard case representation for the new pattern.
    /// (guardCondition, targetIndex, boundValues)
    type InlinedGuardCase = Fable.Expr * int * Fable.Expr list

    /// Extracts inlined guard cases from a decision tree (release mode).
    /// In release mode, F# inlines the guard condition directly into the IfThenElse.
    let private extractInlinedGuardCases
        (subjectName: string option)
        acc
        expr
        : (InlinedGuardCase list * (int * Fable.Expr list)) option
        =
        let rec loop subjectName acc expr =
            match expr with
            // Release mode pattern: condition is directly an Operation (comparison)
            | Fable.IfThenElse(cond, Fable.DecisionTreeSuccess(targetIndex, boundValues, _), elseExpr, _) ->
                match tryExtractSubjectFromComparison cond with
                | Some subjectIdent ->
                    // Verify the subject is the same across all cases
                    match subjectName with
                    | Some name when name <> subjectIdent.Name -> None
                    | _ ->
                        let guardCase: InlinedGuardCase = (cond, targetIndex, boundValues)
                        loop (Some subjectIdent.Name) (guardCase :: acc) elseExpr
                | None -> None
            | Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, _) ->
                // We've reached the default case
                match subjectName with
                | Some _ -> Some(List.rev acc, (defaultIndex, defaultBoundValues))
                | None -> None
            | _ ->
                // Pattern doesn't match
                None

        loop subjectName acc expr

    /// Extracts guard cases from a decision tree expression (debug mode).
    /// Returns None if the pattern doesn't match a guard chain.
    let private extractGuardCases (subjectName: string option) acc expr =
        let rec loop subjectName acc expr =
            match expr with
            | Fable.IfThenElse(GuardLetBinding(subj, param, guardBody),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                               elseExpr,
                               _) ->
                // Verify the subject is the same across all cases
                match subjectName with
                | Some name when name <> subj.Name -> None // Different subjects, can't use match
                | _ ->
                    let guardCase = (subj, param, guardBody, targetIndex, boundValues)
                    loop (Some subj.Name) (guardCase :: acc) elseExpr
            | Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, _) ->
                // We've reached the default case
                match subjectName with
                | Some _ -> Some(List.rev acc, (defaultIndex, defaultBoundValues))
                | None -> None
            | _ ->
                // Pattern doesn't match - not a pure guard chain
                None

        loop subjectName acc expr

    /// Result type for guard pattern extraction - supports both debug and release mode patterns.
    type GuardPatternResult =
        /// Debug mode: Let bindings are present, we have subject, param, guardBody per case
        | DebugModeGuards of
            subjectIdent: Fable.Ident *
            cases: (Fable.Ident * Fable.Ident * Fable.Expr * int * Fable.Expr list) list *
            defaultCase: (int * Fable.Expr list)
        /// Release mode: Guards are inlined, we have the guard condition directly
        | ReleaseModeGuards of
            subjectIdent: Fable.Ident *
            cases: InlinedGuardCase list *
            defaultCase: (int * Fable.Expr list)

    /// Detects a chain of guard expressions in a decision tree.
    /// Handles both debug mode (with Let bindings) and release mode (inlined guards).
    /// Returns Some(GuardPatternResult) if the pattern matches.
    let tryExtractGuardPattern treeExpr =
        match treeExpr with
        // Debug mode pattern: condition is a Let binding
        | Fable.IfThenElse(GuardLetBinding(subject, param, guardBody),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                           elseExpr,
                           _) ->
            let firstCase = (subject, param, guardBody, targetIndex, boundValues)

            match extractGuardCases (Some subject.Name) [ firstCase ] elseExpr with
            | Some(cases, defaultCase) when List.length cases >= 1 ->
                let subjectIdent = cases |> List.head |> (fun (s, _, _, _, _) -> s)
                Some(DebugModeGuards(subjectIdent, cases, defaultCase))
            | _ -> None

        // Release mode pattern: condition is directly an Operation (comparison)
        | Fable.IfThenElse(cond, Fable.DecisionTreeSuccess(targetIndex, boundValues, _), elseExpr, _) ->
            match tryExtractSubjectFromComparison cond with
            | Some subjectIdent ->
                let firstCase: InlinedGuardCase = (cond, targetIndex, boundValues)

                match extractInlinedGuardCases (Some subjectIdent.Name) [ firstCase ] elseExpr with
                | Some(cases, defaultCase) when List.length cases >= 1 ->
                    Some(ReleaseModeGuards(subjectIdent, cases, defaultCase))
                | _ -> None
            | None -> None

        | _ -> None

    /// Result type for tuple Option pattern extraction.
    /// Represents patterns like: match (x, y) with | Some a, Some b -> ... | _ -> None
    type TupleOptionPatternResult =
        {
            /// The variables being tested (e.g., [x; y])
            TestedVars: Fable.Ident list
            /// The target index for the success case (when all are Some)
            SuccessTargetIndex: int
            /// Bound values for the success case
            SuccessBoundValues: Fable.Expr list
            /// The target index for the None case (when any is None)
            NoneTargetIndex: int
        }

    /// Extracts nested Option test patterns from a decision tree.
    /// Detects patterns like: if x is Some then (if y is Some then success else none) else none
    /// Returns Some result if the pattern matches, None otherwise.
    let rec private extractNestedOptionTests
        (noneTargetIdx: int option)
        (testedVars: Fable.Ident list)
        (expr: Fable.Expr)
        : TupleOptionPatternResult option
        =
        match expr with
        // Test for Some (isSome=true) with nested tests or success
        | Fable.IfThenElse(Fable.Test(Fable.IdentExpr ident, Fable.OptionTest true, _), thenExpr, elseExpr, _) ->
            // The else branch should go to a simple None target
            match elseExpr with
            | Fable.DecisionTreeSuccess(elseIdx, [], _) ->
                // Check consistency with previously seen None target
                match noneTargetIdx with
                | Some idx when idx <> elseIdx -> None // Inconsistent None targets
                | _ ->
                    // Recurse into the then branch
                    extractNestedOptionTests (Some elseIdx) (ident :: testedVars) thenExpr
            | _ -> None // Else branch is not a simple target

        // Success case - all Option tests passed
        | Fable.DecisionTreeSuccess(successIdx, boundValues, _) ->
            match noneTargetIdx with
            | Some noneIdx when successIdx <> noneIdx ->
                Some
                    {
                        TestedVars = List.rev testedVars
                        SuccessTargetIndex = successIdx
                        SuccessBoundValues = boundValues
                        NoneTargetIndex = noneIdx
                    }
            | _ -> None // No None target found or success equals None target

        | _ -> None

    /// Tries to extract a tuple Option pattern from a decision tree.
    /// Returns Some result if the tree matches the pattern, None otherwise.
    /// Requires at least 2 tested variables to be a "tuple" pattern.
    /// Single Option tests should use regular if/else to avoid nonlocal issues.
    let tryExtractTupleOptionPattern (treeExpr: Fable.Expr) : TupleOptionPatternResult option =
        match extractNestedOptionTests None [] treeExpr with
        | Some result when List.length result.TestedVars >= 2 -> Some result
        | _ -> None

    /// A single case in a tuple boolean+guard pattern
    type TupleBoolGuardCase =
        {
            /// The guard expression (e.g., i > 10), or None for literal/wildcard
            GuardExpr: Fable.Expr option
            /// Index of the element used in the guard (if any)
            GuardIndex: int option
            /// Target index for this case
            TargetIndex: int
            /// Bound values for this case
            BoundValues: Fable.Expr list
        }

    /// Result type for tuple boolean+guard pattern extraction.
    /// Represents patterns like: match tuple with | true, _, i when i > -1 -> ... | _ -> ...
    type TupleBoolGuardPatternResult =
        {
            /// The tuple expression being matched
            TupleExpr: Fable.Expr
            /// The tuple type (to determine arity)
            TupleType: Fable.Type
            /// Index of the boolean element being tested (typically 0)
            BoolIndex: int
            /// The expected boolean value (true or false)
            BoolValue: bool
            /// List of guard cases (in order)
            Cases: TupleBoolGuardCase list
            /// Target index for default case
            DefaultTargetIndex: int
            /// Bound values for default case
            DefaultBoundValues: Fable.Expr list
        }

    /// Extracts tuple index from a Get expression
    [<return: Struct>]
    let private (|TupleIndexGet|_|) =
        function
        | Fable.Get(expr, Fable.TupleIndex idx, _, _) -> ValueSome(expr, idx)
        | _ -> ValueNone

    /// Checks if two expressions refer to the same identifier
    let private sameIdent expr1 expr2 =
        match expr1, expr2 with
        | Fable.IdentExpr id1, Fable.IdentExpr id2 -> id1.Name = id2.Name
        | _ -> false

    /// Extracts the tuple index from a comparison's left or right side
    let private tryExtractGuardTupleIndex tupleExpr =
        function
        | Fable.Operation(Fable.Binary(_, TupleIndexGet(expr, idx), _), _, _, _) when sameIdent expr tupleExpr ->
            Some idx
        | Fable.Operation(Fable.Binary(_, _, TupleIndexGet(expr, idx)), _, _, _) when sameIdent expr tupleExpr ->
            Some idx
        | _ -> None

    /// Tries to make a guard case from a guard expression
    let private tryMakeGuardCase tupleExpr guardExpr targetIdx boundValues =
        match tryExtractGuardTupleIndex tupleExpr guardExpr with
        | Some guardIdx ->
            Some
                {
                    GuardExpr = Some guardExpr
                    GuardIndex = Some guardIdx
                    TargetIndex = targetIdx
                    BoundValues = boundValues
                }
        | None ->
            // Try equality check pattern like tuple[2] == 0
            match guardExpr with
            | Fable.Operation(Fable.Binary(BinaryEqual, TupleIndexGet(expr, idx), _), _, _, _) when
                sameIdent expr tupleExpr
                ->
                Some
                    {
                        GuardExpr = Some guardExpr
                        GuardIndex = Some idx
                        TargetIndex = targetIdx
                        BoundValues = boundValues
                    }
            | _ -> None

    /// Recursively extracts guard cases from nested IfThenElse
    let rec private extractGuardCasesRec tupleExpr cases expr =
        match expr with
        | Fable.IfThenElse(guardExpr, Fable.DecisionTreeSuccess(targetIdx, boundValues, _), elseExpr, _) ->
            match tryMakeGuardCase tupleExpr guardExpr targetIdx boundValues with
            | Some guardCase ->
                match elseExpr with
                | Fable.DecisionTreeSuccess(elseTargetIdx, elseBoundValues, _) ->
                    Some(List.rev (guardCase :: cases), elseTargetIdx, elseBoundValues)
                | _ -> extractGuardCasesRec tupleExpr (guardCase :: cases) elseExpr
            | None -> None
        | Fable.DecisionTreeSuccess(targetIdx, boundValues, _) -> Some(List.rev cases, targetIdx, boundValues)
        | _ -> None

    /// Tries to extract a tuple boolean+guard pattern from a decision tree.
    /// Detects: if tuple[0] then (chain of guards) else default
    let tryExtractTupleBoolGuardPattern (treeExpr: Fable.Expr) : TupleBoolGuardPatternResult option =
        match treeExpr with
        | Fable.IfThenElse(TupleIndexGet(tupleExpr, boolIdx),
                           innerExpr,
                           Fable.DecisionTreeSuccess(outerDefaultIdx, outerDefaultBound, _),
                           _) ->
            match extractGuardCasesRec tupleExpr [] innerExpr with
            | Some(cases, defaultIdx, defaultBound) when not (List.isEmpty cases) && defaultIdx = outerDefaultIdx ->
                Some
                    {
                        TupleExpr = tupleExpr
                        TupleType = tupleExpr.Type
                        BoolIndex = boolIdx
                        BoolValue = true
                        Cases = cases
                        DefaultTargetIndex = defaultIdx
                        DefaultBoundValues = defaultBound
                    }
            | _ -> None
        | _ -> None

    /// Unwraps redundant Let bindings in the target expression.
    /// The pattern already captures the value, so Let(v, x, body) where x is the subject
    /// can be replaced with body (substituting v with patternIdent).
    let rec unwrapRedundantLets (subjectName: string) (patternIdent: Fable.Ident) expr =
        match expr with
        | Fable.Let(ident, Fable.IdentExpr valueIdent, body) when valueIdent.Name = subjectName ->
            // This Let binds a new variable to the subject - it's redundant
            // Replace references to ident with patternIdent in the body
            let body' =
                FableTransforms.replaceValues (Map.ofList [ ident.Name, Fable.IdentExpr patternIdent ]) body

            unwrapRedundantLets subjectName patternIdent body'
        | _ -> expr

    /// Represents a single case extracted from a decision tree for match statement generation.
    /// Contains the accumulated condition, target index, and bound values.
    type DecisionTreeCase =
        {
            /// The condition expression for this case (accumulated from IfThenElse chain)
            Condition: Fable.Expr
            /// Target index for this case
            TargetIndex: int
            /// Bound values for this case
            BoundValues: Fable.Expr list
        }

    /// Result type for decision tree extraction for match statement conversion.
    type DecisionTreeMatchResult =
        {
            /// All cases extracted from the decision tree
            Cases: DecisionTreeCase list
            /// Target index for the default case
            DefaultTargetIndex: int
            /// Bound values for the default case
            DefaultBoundValues: Fable.Expr list
        }

    /// Extracts all cases from a decision tree for match statement generation.
    /// Flattens nested IfThenElse into a list of (accumulated conditions, targetIndex, boundValues).
    /// Each path from root to DecisionTreeSuccess becomes a case with ANDed conditions.
    let tryExtractDecisionTreeCases (treeExpr: Fable.Expr) : DecisionTreeMatchResult option =
        // Helper to AND two conditions together
        let andConditions (c1: Fable.Expr) (c2: Fable.Expr) : Fable.Expr =
            Fable.Operation(Fable.Logical(LogicalAnd, c1, c2), [], Fable.Boolean, None)

        // Recursively extract all paths, accumulating conditions.
        // Uses an accumulator (acc) to avoid O(n²) list concatenation.
        // Cases are accumulated in reverse order and reversed at the end.
        let rec extractPaths
            (acc: DecisionTreeCase list)
            (currentCondition: Fable.Expr option)
            (expr: Fable.Expr)
            : (DecisionTreeCase list * (int * Fable.Expr list) option) option
            =
            match expr with
            | Fable.IfThenElse(condition, thenExpr, elseExpr, _) ->
                // Condition for then branch: currentCondition AND condition
                let thenCondition =
                    match currentCondition with
                    | Some cc -> andConditions cc condition
                    | None -> condition

                // For the else branch, we don't accumulate "NOT condition" because
                // Python match semantics guarantee sequential evaluation with fall-through.
                // If the then-branch guard fails, Python automatically tries the next case,
                // so the negation is implicit. This produces cleaner guards like:
                //   case _ if A: ...      (instead of case _ if A: ...)
                //   case _ if B: ...      (instead of case _ if (not A) and B: ...)
                //   case _: ...           (default)
                let elseCondition = currentCondition

                // Extract from then branch, passing accumulator
                match extractPaths acc (Some thenCondition) thenExpr with
                | Some(accAfterThen, thenDefault) ->
                    // Extract from else branch, continuing with accumulated cases
                    match extractPaths accAfterThen elseCondition elseExpr with
                    | Some(accAfterElse, elseDefault) ->
                        // Default should come from one of the branches (prefer else as it's typically the fallback)
                        let defaultCase = elseDefault |> Option.orElse thenDefault
                        Some(accAfterElse, defaultCase)
                    | None -> None
                | None -> None

            | Fable.DecisionTreeSuccess(targetIndex, boundValues, _) ->
                match currentCondition with
                | Some cond ->
                    // This is a case with accumulated conditions - prepend to accumulator
                    let case =
                        {
                            Condition = cond
                            TargetIndex = targetIndex
                            BoundValues = boundValues
                        }

                    Some(case :: acc, None)
                | None ->
                    // This is the default case (no conditions accumulated)
                    Some(acc, Some(targetIndex, boundValues))

            | _ ->
                // Pattern doesn't match expected structure
                None

        match extractPaths [] None treeExpr with
        | Some(casesReversed, defaultOpt) ->
            // Reverse the accumulated cases to restore original order
            let cases = List.rev casesReversed

            match defaultOpt with
            | Some(defaultIndex, defaultBoundValues) when not (List.isEmpty cases) ->
                Some
                    {
                        Cases = cases
                        DefaultTargetIndex = defaultIndex
                        DefaultBoundValues = defaultBoundValues
                    }
            | None ->
                // No explicit default case found - the last case in the tree isn't
                // necessarily a catch-all, so fall back to safer code generation
                None
            | Some(_defaultIndex, _defaultBoundValues) when List.isEmpty cases ->
                // Only default case with no discriminating cases - a match statement
                // would be trivially `case _:` with no discrimination, so fall back
                // to simpler code generation
                None
            | _ ->
                // Pattern doesn't match expected structure - fall back to other code generation
                None
        | None ->
            // Pattern doesn't match expected structure - fall back to other code generation
            None
