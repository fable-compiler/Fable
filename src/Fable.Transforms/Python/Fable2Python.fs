module rec Fable.Transforms.Fable2Python

open System
open System.Collections.Generic
open System.Text.RegularExpressions

open Fable
open Fable.AST
open Fable.AST.Python
open Fable.Py

type ReturnStrategy =
    /// Return last expression
    | Return
    | ReturnUnit
    /// Return within a with-statement (to make sure we don't TC with statements)
    | ResourceManager
    | Assign of Expression
    | Target of Identifier

type Import =
    {
        Module: string
        LocalIdent: Identifier option
        Name: string option
    }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: Arg list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
    {
        RootScope: HashSet<string>
        DeclarationScopes: HashSet<string>
        CurrentDeclarationScope: HashSet<string>
    }

/// Python specific, used for keeping track of existing variable bindings to
/// know if we need to declare an identifier as nonlocal or global.
type BoundVars =
    {
        EnclosingScope: HashSet<string>
        LocalScope: HashSet<string>
        Inceptions: int
    }

    member this.EnterScope() =
        // printfn "Entering scope"
        let enclosingScope = HashSet<string>()
        enclosingScope.UnionWith(this.EnclosingScope)
        enclosingScope.UnionWith(this.LocalScope)

        {
            LocalScope = HashSet()
            EnclosingScope = enclosingScope
            Inceptions = this.Inceptions + 1
        }

    member this.Bind(name: string) = this.LocalScope.Add name |> ignore

    member this.Bind(ids: Identifier list) =
        for Identifier name in ids do
            this.LocalScope.Add name |> ignore

    member this.NonLocals(idents: Identifier list) =
        [
            for ident in idents do
                let (Identifier name) = ident

                if
                    not (this.LocalScope.Contains name)
                    && this.EnclosingScope.Contains name
                then
                    ident
                else
                    this.Bind(name)
        ]

type Context =
    {
        File: Fable.File
        UsedNames: UsedNames
        BoundVars: BoundVars
        DecisionTargets: (Fable.Ident list * Fable.Expr) list
        HoistVars: Fable.Ident list -> bool
        TailCallOpportunity: ITailCallOpportunity option
        OptimizeTailCall: unit -> unit
        ScopedTypeParams: Set<string>
        TypeParamsScope: int
    }

type IPythonCompiler =
    inherit Compiler
    abstract AddTypeVar: ctx: Context * name: string -> Expression
    abstract AddExport: name: string -> Expression
    abstract GetIdentifier: ctx: Context * name: string -> Identifier
    abstract GetIdentifierAsExpr: ctx: Context * name: string -> Expression
    abstract GetAllImports: unit -> Import list
    abstract GetAllExports: unit -> HashSet<string>
    abstract GetAllTypeVars: unit -> HashSet<string>

    abstract GetImportExpr:
        Context * moduleName: string * ?name: string * ?loc: SourceLocation ->
            Expression

    abstract TransformAsExpr:
        Context * Fable.Expr -> Expression * Statement list

    abstract TransformAsStatements:
        Context * ReturnStrategy option * Fable.Expr -> Statement list

    abstract TransformImport:
        Context * selector: string * path: string -> Expression

    abstract TransformFunction:
        Context * string option * Fable.Ident list * Fable.Expr * Set<string> ->
            Arguments * Statement list

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

// TODO: All things that depend on the library should be moved to Replacements
// to become independent of the specific implementation
module Lib =
    let libCall (com: IPythonCompiler) ctx r moduleName memberName args =
        Expression.call (
            com.TransformImport(ctx, memberName, getLibPath com moduleName),
            args,
            ?loc = r
        )

    let libConsCall (com: IPythonCompiler) ctx r moduleName memberName args =
        Expression.call (
            com.TransformImport(ctx, memberName, getLibPath com moduleName),
            args,
            ?loc = r
        )

    let libValue (com: IPythonCompiler) ctx moduleName memberName =
        com.TransformImport(ctx, memberName, getLibPath com moduleName)

    let tryPyConstructor (com: IPythonCompiler) ctx ent =
        match Py.Replacements.tryConstructor com ent with
        | Some e -> com.TransformAsExpr(ctx, e) |> Some
        | None -> None

    let pyConstructor (com: IPythonCompiler) ctx ent =
        let entRef = Py.Replacements.constructor com ent
        com.TransformAsExpr(ctx, entRef)

module Reflection =
    open Lib

    let private libReflectionCall (com: IPythonCompiler) ctx r memberName args =
        libCall com ctx r "reflection" (memberName + "_type") args

    let private transformRecordReflectionInfo
        com
        ctx
        r
        (ent: Fable.Entity)
        generics
        =
        // TODO: Refactor these three bindings to reuse in transformUnionReflectionInfo
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringConstant fullname

        let genMap =
            let genParamNames =
                ent.GenericParameters
                |> List.mapToArray (fun x -> x.Name)
                |> Seq.toList

            List.zip genParamNames generics |> Map

        let fields, stmts =
            ent.FSharpFields
            |> Seq.map (fun fi ->
                let typeInfo, stmts =
                    transformTypeInfo com ctx r genMap fi.FieldType

                let name = fi.Name |> Naming.toSnakeCase |> Helpers.clean

                (Expression.tuple
                    [
                        Expression.stringConstant (name)
                        typeInfo
                    ]),
                stmts
            )
            |> Seq.toList
            |> Helpers.unzipArgs

        let fields =
            Expression.lambda (Arguments.arguments [], Expression.list fields)

        let py, stmts' = pyConstructor com ctx ent

        [
            fullnameExpr
            Expression.list generics
            py
            fields
        ]
        |> libReflectionCall com ctx None "record",
        stmts @ stmts'

    let private transformUnionReflectionInfo
        com
        ctx
        r
        (ent: Fable.Entity)
        generics
        =
        let fullname = ent.FullName
        let fullnameExpr = Expression.stringConstant fullname

        let genMap =
            let genParamNames =
                ent.GenericParameters
                |> List.map (fun x -> x.Name)
                |> Seq.toList

            List.zip genParamNames generics |> Map

        let cases =
            ent.UnionCases
            |> Seq.map (fun uci ->
                uci.UnionCaseFields
                |> List.map (fun fi ->
                    Expression.tuple
                        [
                            fi.Name |> Expression.stringConstant
                            let expr, _stmts =
                                transformTypeInfo
                                    com
                                    ctx
                                    r
                                    genMap
                                    fi.FieldType

                            expr
                        ]
                )
                |> Expression.list
            )
            |> Seq.toList

        let cases =
            Expression.lambda (Arguments.arguments [], Expression.list cases)

        let py, stmts = pyConstructor com ctx ent

        [
            fullnameExpr
            Expression.list generics
            py
            cases
        ]
        |> libReflectionCall com ctx None "union",
        stmts

    let transformTypeInfo
        (com: IPythonCompiler)
        ctx
        r
        (genMap: Map<string, Expression>)
        t
        : Expression * Statement list
        =
        let primitiveTypeInfo name =
            libValue com ctx "Reflection" (name + "_type")

        let numberInfo kind =
            getNumberKindName kind |> primitiveTypeInfo

        let nonGenericTypeInfo fullname =
            [ Expression.stringConstant fullname ]
            |> libReflectionCall com ctx None "class"

        let resolveGenerics generics : Expression list * Statement list =
            generics
            |> Array.map (transformTypeInfo com ctx r genMap)
            |> List.ofArray
            |> Helpers.unzipArgs

        let genericTypeInfo name genArgs =
            let resolved, stmts = resolveGenerics genArgs
            libReflectionCall com ctx None name resolved, stmts

        let genericEntity (fullname: string) (generics: Expression list) =
            libReflectionCall
                com
                ctx
                None
                "class"
                [
                    Expression.stringConstant fullname
                    if not (List.isEmpty generics) then
                        Expression.list generics
                ]

        match t with
        | Fable.Measure _
        | Fable.Any -> primitiveTypeInfo "obj", []
        | Fable.GenericParam(name = name) ->
            match Map.tryFind name genMap with
            | Some t -> t, []
            | None ->
                Replacements.Util.genericTypeInfoError name |> addError com [] r

                Expression.none, []
        | Fable.Unit -> primitiveTypeInfo "unit", []
        | Fable.Boolean -> primitiveTypeInfo "bool", []
        | Fable.Char -> primitiveTypeInfo "char", []
        | Fable.String -> primitiveTypeInfo "string", []
        | Fable.Number(kind, info) ->
            match info with
            | Fable.NumberInfo.IsEnum entRef ->
                let ent = com.GetEntity(entRef)

                let cases =
                    ent.FSharpFields
                    |> Seq.choose (fun fi ->
                        match fi.Name with
                        | "value__" -> None
                        | name ->
                            let value =
                                match fi.LiteralValue with
                                | Some v -> Convert.ToDouble v
                                | None -> 0.

                            Expression.tuple
                                [
                                    Expression.stringConstant name
                                    Expression.floatConstant value
                                ]
                            |> Some
                    )
                    |> Seq.toList
                    |> Expression.list

                [
                    Expression.stringConstant entRef.FullName
                    numberInfo kind
                    cases
                ]
                |> libReflectionCall com ctx None "enum",
                []
            | _ -> numberInfo kind, []
        | Fable.LambdaType(argType, returnType) ->
            genericTypeInfo
                "lambda"
                [|
                    argType
                    returnType
                |]
        | Fable.DelegateType(argTypes, returnType) ->
            genericTypeInfo
                "delegate"
                [|
                    yield! argTypes
                    yield returnType
                |]
        | Fable.Tuple(genArgs, _) ->
            genericTypeInfo "tuple" (List.toArray genArgs)
        | Fable.Option(genArg, _) -> genericTypeInfo "option" [| genArg |]
        | Fable.Array(genArg, _) -> genericTypeInfo "array" [| genArg |]
        | Fable.List genArg -> genericTypeInfo "list" [| genArg |]
        | Fable.Regex -> nonGenericTypeInfo Types.regex, []
        | Fable.MetaType -> nonGenericTypeInfo Types.type_, []
        | Fable.AnonymousRecordType(fieldNames, genArgs, _isStruct) ->
            let genArgs, stmts = resolveGenerics (List.toArray genArgs)

            List.zip (List.ofArray fieldNames) genArgs
            |> List.map (fun (k, t) ->
                Expression.tuple
                    [
                        Expression.stringConstant k
                        t
                    ]
            )
            |> libReflectionCall com ctx None "anonRecord",
            stmts
        | Fable.DeclaredType(entRef, generics) ->
            let fullName = entRef.FullName

            match fullName, generics with
            | Replacements.Util.BuiltinEntity kind ->
                match kind with
                | Replacements.Util.BclGuid
                | Replacements.Util.BclTimeSpan
                | Replacements.Util.BclDateTime
                | Replacements.Util.BclDateTimeOffset
                | Replacements.Util.BclDateOnly
                | Replacements.Util.BclTimeOnly
                | Replacements.Util.BclTimer -> genericEntity fullName [], []
                | Replacements.Util.BclHashSet gen
                | Replacements.Util.FSharpSet gen ->
                    let gens, stmts = transformTypeInfo com ctx r genMap gen
                    genericEntity fullName [ gens ], stmts
                | Replacements.Util.BclDictionary(key, value)
                | Replacements.Util.BclKeyValuePair(key, value)
                | Replacements.Util.FSharpMap(key, value) ->
                    let keys, stmts = transformTypeInfo com ctx r genMap key

                    let values, stmts' =
                        transformTypeInfo com ctx r genMap value

                    genericEntity
                        fullName
                        [
                            keys
                            values
                        ],
                    stmts @ stmts'
                | Replacements.Util.FSharpResult(ok, err) ->
                    let ent = com.GetEntity(entRef)
                    let ok', stmts = transformTypeInfo com ctx r genMap ok
                    let err', stmts' = transformTypeInfo com ctx r genMap err

                    let expr, stmts'' =
                        transformUnionReflectionInfo
                            com
                            ctx
                            r
                            ent
                            [
                                ok'
                                err'
                            ]

                    expr, stmts @ stmts' @ stmts''
                | Replacements.Util.FSharpChoice gen ->
                    let ent = com.GetEntity(entRef)

                    let gen, stmts =
                        List.map (transformTypeInfo com ctx r genMap) gen
                        |> Helpers.unzipArgs

                    let expr, stmts' =
                        gen |> transformUnionReflectionInfo com ctx r ent

                    expr, stmts @ stmts'
                | Replacements.Util.FSharpReference gen ->
                    let ent = com.GetEntity(entRef)
                    let gen, stmts = transformTypeInfo com ctx r genMap gen

                    let expr, stmts' =
                        [ gen ] |> transformRecordReflectionInfo com ctx r ent

                    expr, stmts @ stmts'
            | _ ->
                let ent = com.GetEntity(entRef)

                let generics, stmts =
                    generics
                    |> List.map (transformTypeInfo com ctx r genMap)
                    |> Helpers.unzipArgs
                // Check if the entity is actually declared in Python code
                if
                    ent.IsInterface
                    || FSharp2Fable.Util.isErasedOrStringEnumEntity ent
                    || FSharp2Fable.Util.isGlobalOrImportedEntity ent
                    || FSharp2Fable.Util.isReplacementCandidate entRef
                then
                    genericEntity ent.FullName generics, stmts
                else
                    let reflectionMethodExpr =
                        FSharp2Fable.Util.entityIdentWithSuffix
                            com
                            entRef
                            Naming.reflectionSuffix

                    let callee, stmts' =
                        com.TransformAsExpr(ctx, reflectionMethodExpr)

                    Expression.call (callee, generics), stmts @ stmts'

    let transformReflectionInfo com ctx r (ent: Fable.Entity) generics =
        if ent.IsFSharpRecord then
            transformRecordReflectionInfo com ctx r ent generics
        elif ent.IsFSharpUnion then
            transformUnionReflectionInfo com ctx r ent generics
        else
            let fullname = ent.FullName

            let exprs, stmts =
                [
                    yield Expression.stringConstant fullname, []
                    match generics with
                    | [] -> yield Util.undefined None, []
                    | generics -> yield Expression.list generics, []
                    match tryPyConstructor com ctx ent with
                    | Some(cons, stmts) -> yield cons, stmts
                    | None -> ()
                    match ent.BaseType with
                    | Some d ->
                        let genMap =
                            Seq.zip ent.GenericParameters generics
                            |> Seq.map (fun (p, e) -> p.Name, e)
                            |> Map

                        yield
                            Fable.DeclaredType(d.Entity, d.GenericArgs)
                            |> transformTypeInfo com ctx r genMap
                    | None -> ()
                ]
                |> Helpers.unzipArgs

            exprs |> libReflectionCall com ctx r "class", stmts

    let private ofString s = Expression.stringConstant s
    let private ofArray exprs = Expression.list exprs

    let transformTypeTest
        (com: IPythonCompiler)
        ctx
        range
        expr
        (typ: Fable.Type)
        : Expression * Statement list
        =
        let warnAndEvalToFalse msg =
            "Cannot type test (evals to false): " + msg
            |> addWarning com [] range

            Expression.boolConstant false

        let pyTypeof
            (primitiveType: string)
            (Util.TransformExpr com ctx (expr, stmts))
            : Expression * Statement list
            =
            let typeof =
                let func = Expression.name (Identifier("type"))
                let str = Expression.name (Identifier("str"))
                let typ = Expression.call (func, [ expr ])
                Expression.call (str, [ typ ])

            Expression.compare (
                typeof,
                [ Eq ],
                [ Expression.stringConstant primitiveType ],
                ?loc = range
            ),
            stmts

        let pyInstanceof
            consExpr
            (Util.TransformExpr com ctx (expr, stmts))
            : Expression * Statement list
            =
            let func = Expression.name (Identifier("isinstance"))

            let args =
                [
                    expr
                    consExpr
                ]

            Expression.call (func, args), stmts

        match typ with
        | Fable.Measure _ // Dummy, shouldn't be possible to test against a measure type
        | Fable.Any -> Expression.boolConstant true, []
        | Fable.Unit ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)

            Expression.compare (
                expr,
                [ Is ],
                [ Util.undefined None ],
                ?loc = range
            ),
            stmts
        | Fable.Boolean -> pyTypeof "<class 'bool'>" expr
        | Fable.Char
        | Fable.String -> pyTypeof "<class 'str'>" expr
        | Fable.Number(kind, _b) ->
            match kind, typ with
            | _, Fable.Type.Number(UInt8, _) ->
                pyTypeof "<fable_modules.fable_library.types.uint8'>>" expr
            | _, Fable.Type.Number(Int8, _) ->
                pyTypeof "<class 'fable_modules.fable_library.types.int8'>" expr
            | _, Fable.Type.Number(Int16, _) ->
                pyTypeof
                    "<class 'fable_modules.fable_library.types.int16'>"
                    expr
            | _, Fable.Type.Number(UInt16, _) ->
                pyTypeof
                    "<class 'fable_modules.fable_library.types.uint16'>"
                    expr
            | _, Fable.Type.Number(Int32, _) -> pyTypeof "<class 'int'>" expr
            | _, Fable.Type.Number(UInt32, _) ->
                pyTypeof
                    "<class 'fable_modules.fable_library.types.uint32>"
                    expr
            | _, Fable.Type.Number(Int64, _) ->
                pyTypeof
                    "<class 'fable_modules.fable_library.types.int64'>"
                    expr
            | _, Fable.Type.Number(UInt64, _) ->
                pyTypeof
                    "<class 'fable_modules.fable_library.types.uint32'>"
                    expr
            | _, Fable.Type.Number(Float32, _) ->
                pyTypeof
                    "<class 'fable_modules.fable_library.types.float32'>"
                    expr
            | _, Fable.Type.Number(Float64, _) ->
                pyTypeof "<class 'float'>" expr
            | _ -> pyTypeof "<class 'int'>" expr

        | Fable.Regex ->
            pyInstanceof (com.GetImportExpr(ctx, "typing", "Pattern")) expr
        | Fable.LambdaType _
        | Fable.DelegateType _ -> pyTypeof "<class 'function'>" expr
        | Fable.Array _
        | Fable.Tuple _ ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            libCall com ctx None "util" "isArrayLike" [ expr ], stmts
        | Fable.List _ ->
            pyInstanceof (libValue com ctx "List" "FSharpList") expr
        | Fable.AnonymousRecordType _ ->
            warnAndEvalToFalse "anonymous records", []
        | Fable.MetaType ->
            pyInstanceof (libValue com ctx "Reflection" "TypeInfo") expr
        | Fable.Option _ -> warnAndEvalToFalse "options", [] // TODO
        | Fable.GenericParam _ -> warnAndEvalToFalse "generic parameters", []
        | Fable.DeclaredType(ent, genArgs) ->
            match ent.FullName with
            | Types.idisposable ->
                match expr with
                | MaybeCasted(ExprType(Fable.DeclaredType(ent2, _))) when
                    com.GetEntity(ent2)
                    |> FSharp2Fable.Util.hasInterface Types.idisposable
                    ->
                    Expression.boolConstant true, []
                | _ ->
                    let expr, stmts = com.TransformAsExpr(ctx, expr)
                    libCall com ctx None "util" "isDisposable" [ expr ], stmts
            | Types.ienumerable ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)

                [ expr ] |> libCall com ctx None "util" "isIterable", stmts
            | Types.array ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)

                [ expr ] |> libCall com ctx None "util" "isArrayLike", stmts
            | Types.exception_ ->
                let expr, stmts = com.TransformAsExpr(ctx, expr)

                [ expr ] |> libCall com ctx None "types" "isException", stmts
            | Types.datetime ->
                pyInstanceof
                    (com.GetImportExpr(ctx, "datetime", "datetime"))
                    expr
            | _ ->
                let ent = com.GetEntity(ent)

                if ent.IsInterface then
                    match
                        FSharp2Fable.Util.tryGlobalOrImportedEntity com ent
                    with
                    | Some typeExpr ->
                        let typeExpr, stmts = com.TransformAsExpr(ctx, typeExpr)
                        let expr, stmts' = pyInstanceof typeExpr expr
                        expr, stmts @ stmts'
                    | None -> warnAndEvalToFalse "interfaces", []
                else
                    match tryPyConstructor com ctx ent with
                    | Some(cons, stmts) ->
                        if not (List.isEmpty genArgs) then
                            com.WarnOnlyOnce(
                                "Generic args are ignored in type testing",
                                ?range = range
                            )

                        let expr, stmts' = pyInstanceof cons expr
                        expr, stmts @ stmts'
                    | None -> warnAndEvalToFalse ent.FullName, []

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

    let index = (Seq.initInfinite id).GetEnumerator()

    let removeNamespace (fullName: string) =
        fullName.Split('.')
        |> Array.last
        |> (fun name -> name.Replace("`", "_"))
        |> Helpers.clean

    let getUniqueIdentifier (name: string) : Identifier =
        do index.MoveNext() |> ignore
        let idx = index.Current.ToString()

        let deliminator =
            if Char.IsLower name[0] then
                "_"
            else
                ""

        Identifier($"{name}{deliminator}{idx}")

    /// Replaces all '$' and `.`with '_'
    let clean (name: string) =
        (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun _ -> false)

    let unzipArgs
        (args: (Expression * Statement list) list)
        : Expression list * Statement list
        =
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

// https://www.python.org/dev/peps/pep-0484/
module Annotation =
    open Lib

    let getEntityGenParams (ent: Fable.Entity) =
        ent.GenericParameters |> Seq.map (fun x -> x.Name) |> Set.ofSeq

    let makeTypeParamDecl (com: IPythonCompiler) ctx (genParams: Set<string>) =
        if (Set.isEmpty genParams) then
            []
        else
            com.GetImportExpr(ctx, "typing", "Generic") |> ignore

            let genParams =
                genParams
                |> Set.toList
                |> List.map (fun genParam -> com.AddTypeVar(ctx, genParam))

            let generic = Expression.name "Generic"
            [ Expression.subscript (generic, Expression.tuple genParams) ]

    let private libReflectionCall (com: IPythonCompiler) ctx r memberName args =
        libCall com ctx r "reflection" (memberName + "_type") args

    let fableModuleAnnotation
        (com: IPythonCompiler)
        ctx
        moduleName
        memberName
        args
        =
        let expr =
            com.TransformImport(ctx, memberName, getLibPath com moduleName)

        match args with
        | [] -> expr
        | [ arg ] -> Expression.subscript (expr, arg)
        | args -> Expression.subscript (expr, Expression.tuple args)

    let stdlibModuleAnnotation
        (com: IPythonCompiler)
        ctx
        moduleName
        memberName
        args
        =
        let expr = com.TransformImport(ctx, memberName, moduleName)

        match memberName, args with
        | "Callable", args ->
            let returnType = List.last args

            let args =
                match args with
                | Expression.Name { Id = Identifier Ellipsis } :: _xs ->
                    Expression.ellipsis
                | _ ->
                    args
                    |> List.removeAt (args.Length - 1)
                    |> List.choose (
                        function
                        | Expression.Name { Id = Identifier "None" } when
                            args.Length = 2
                            ->
                            None
                        | x -> Some x
                    )
                    |> Expression.list

            Expression.subscript (
                expr,
                Expression.tuple
                    [
                        args
                        returnType
                    ]
            )
        | _, [] -> expr
        | _, [ arg ] -> Expression.subscript (expr, arg)
        | _, args -> Expression.subscript (expr, Expression.tuple args)

    let fableModuleTypeHint
        com
        ctx
        moduleName
        memberName
        genArgs
        repeatedGenerics
        =
        let resolved, stmts = resolveGenerics com ctx genArgs repeatedGenerics

        fableModuleAnnotation com ctx moduleName memberName resolved, stmts

    let stdlibModuleTypeHint com ctx moduleName memberName genArgs =
        let resolved, stmts = resolveGenerics com ctx genArgs None
        stdlibModuleAnnotation com ctx moduleName memberName resolved, stmts

    let makeGenTypeParamInst
        com
        ctx
        (genArgs: Fable.Type list)
        (repeatedGenerics: Set<string> option)
        =
        match genArgs with
        | [] -> []
        | _ ->
            genArgs
            |> List.map (typeAnnotation com ctx repeatedGenerics)
            |> List.map fst

    let makeGenericTypeAnnotation
        (com: IPythonCompiler)
        ctx
        (id: string)
        (genArgs: Fable.Type list)
        (repeatedGenerics: Set<string> option)
        =
        stdlibModuleAnnotation com ctx "__future__" "annotations" [] |> ignore

        let typeParamInst =
            makeGenTypeParamInst com ctx genArgs repeatedGenerics

        let name = Expression.name id

        if typeParamInst.IsEmpty then
            name
        else
            Expression.subscript (name, Expression.tuple typeParamInst)

    let makeGenericTypeAnnotation'
        (com: IPythonCompiler)
        ctx
        (id: string)
        (genArgs: string list)
        (repeatedGenerics: Set<string> option)
        =
        stdlibModuleAnnotation com ctx "__future__" "annotations" [] |> ignore

        let name = Expression.name id

        if genArgs.IsEmpty then
            name
        else
            let genArgs =
                match repeatedGenerics with
                | Some generics ->
                    let genArgs =
                        genArgs
                        |> Set.ofList
                        |> Set.intersect generics
                        |> Set.toList

                    if genArgs.IsEmpty then
                        [ stdlibModuleAnnotation com ctx "typing" "Any" [] ]
                    else
                        genArgs
                        |> List.map (fun name -> com.AddTypeVar(ctx, name))
                | _ ->
                    genArgs |> List.map (fun name -> com.AddTypeVar(ctx, name))

            Expression.subscript (name, Expression.tuple genArgs)

    let resolveGenerics
        com
        ctx
        generics
        repeatedGenerics
        : Expression list * Statement list
        =
        generics
        |> List.map (typeAnnotation com ctx repeatedGenerics)
        |> Helpers.unzipArgs

    let typeAnnotation
        (com: IPythonCompiler)
        ctx
        (repeatedGenerics: Set<string> option)
        (t: Fable.Type)
        : Expression * Statement list
        =
        // printfn "typeAnnotation: %A" (t, repeatedGenerics)
        match t with
        | Fable.Measure _
        | Fable.Any -> stdlibModuleTypeHint com ctx "typing" "Any" []
        | Fable.GenericParam(name = name) when
            name.StartsWith("$$", StringComparison.Ordinal)
            ->
            stdlibModuleTypeHint com ctx "typing" "Any" []
        | Fable.GenericParam(name = name) ->
            match repeatedGenerics with
            | Some names when names.Contains name ->
                let name = Helpers.clean name
                com.AddTypeVar(ctx, name), []
            | Some _ -> stdlibModuleTypeHint com ctx "typing" "Any" []
            | None ->
                let name = Helpers.clean name
                com.AddTypeVar(ctx, name), []
        | Fable.Unit -> Expression.none, []
        | Fable.Boolean -> Expression.name "bool", []
        | Fable.Char -> Expression.name "str", []
        | Fable.String -> Expression.name "str", []
        | Fable.Number(kind, info) -> makeNumberTypeAnnotation com ctx kind info
        | Fable.LambdaType(argType, returnType) ->
            let argTypes, returnType =
                uncurryLambdaType -1 [ argType ] returnType

            stdlibModuleTypeHint
                com
                ctx
                "collections.abc"
                "Callable"
                (argTypes @ [ returnType ])
        | Fable.DelegateType(argTypes, returnType) ->
            stdlibModuleTypeHint
                com
                ctx
                "collections.abc"
                "Callable"
                (argTypes @ [ returnType ])
        | Fable.Option(genArg, _) ->
            let resolved, stmts =
                resolveGenerics com ctx [ genArg ] repeatedGenerics

            Expression.binOp (resolved[0], BitOr, Expression.none), stmts
        | Fable.Tuple(genArgs, _) ->
            makeGenericTypeAnnotation com ctx "tuple" genArgs None, []
        | Fable.Array(genArg, _) ->
            match genArg with
            | Fable.Type.Number(UInt8, _) -> Expression.name "bytearray", []
            | Fable.Type.Number(Int8, _)
            | Fable.Type.Number(Int16, _)
            | Fable.Type.Number(UInt16, _)
            | Fable.Type.Number(Int32, _)
            | Fable.Type.Number(UInt32, _)
            | Fable.Type.Number(Float32, _)
            | Fable.Type.Number(Float64, _)
            | _ ->
                fableModuleTypeHint
                    com
                    ctx
                    "types"
                    "Array"
                    [ genArg ]
                    repeatedGenerics
        | Fable.List genArg ->
            fableModuleTypeHint
                com
                ctx
                "list"
                "FSharpList"
                [ genArg ]
                repeatedGenerics
        | Replacements.Util.Builtin kind ->
            makeBuiltinTypeAnnotation com ctx kind repeatedGenerics
        | Fable.AnonymousRecordType(_, _genArgs, _) ->
            let value = Expression.name "dict"
            let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

            Expression.subscript (
                value,
                Expression.tuple
                    [
                        Expression.name "str"
                        any
                    ]
            ),
            stmts
        | Fable.DeclaredType(entRef, genArgs) ->
            makeEntityTypeAnnotation com ctx entRef genArgs repeatedGenerics
        | _ -> stdlibModuleTypeHint com ctx "typing" "Any" []

    let makeNumberTypeAnnotation com ctx kind info =
        let numberInfo kind =
            let name =
                match kind with
                | Int8 -> "int8"
                | UInt8 -> "uint8"
                | Int16 -> "int16"
                | UInt16 -> "uint16"
                | UInt32 -> "uint32"
                | Int64 -> "int64"
                | UInt64 -> "uint64"
                | Int32
                | BigInt
                | Int128
                | UInt128
                | NativeInt
                | UNativeInt -> "int"
                | Float16
                | Float32 -> "float32"
                | Float64 -> "float"
                | _ -> failwith $"Unsupported number type: {kind}"

            match name with
            | "int"
            | "float" -> Expression.name name
            | _ -> fableModuleAnnotation com ctx "types" name []


        match kind, info with
        | _, Fable.NumberInfo.IsEnum entRef ->
            let ent = com.GetEntity(entRef)

            let cases =
                ent.FSharpFields
                |> Seq.choose (fun fi ->
                    match fi.Name with
                    | "value__" -> None
                    | name ->
                        let value =
                            match fi.LiteralValue with
                            | Some v -> Convert.ToDouble v
                            | None -> 0.

                        Expression.tuple
                            [
                                Expression.stringConstant name
                                Expression.floatConstant value
                            ]
                        |> Some
                )
                |> Seq.toList
                |> Expression.list

            [
                Expression.stringConstant entRef.FullName
                numberInfo kind
                cases
            ]
            |> libReflectionCall com ctx None "enum",
            []
        | Decimal, _ -> stdlibModuleTypeHint com ctx "decimal" "Decimal" []
        | _ -> numberInfo kind, []

    let makeImportTypeId (com: IPythonCompiler) ctx moduleName typeName =
        let expr = com.GetImportExpr(ctx, getLibPath com moduleName, typeName)

        match expr with
        | Expression.Name { Id = Identifier id } -> id
        | _ -> typeName

    let makeImportTypeAnnotation com ctx genArgs moduleName typeName =
        let id = makeImportTypeId com ctx moduleName typeName
        makeGenericTypeAnnotation com ctx id genArgs None

    let makeEntityTypeAnnotation
        com
        ctx
        (entRef: Fable.EntityRef)
        genArgs
        repeatedGenerics
        =
        // printfn "DeclaredType: %A" entRef.FullName
        match entRef.FullName, genArgs with
        | Types.result, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "choice" "FSharpResult_2" resolved,
            stmts
        | Replacements.Util.BuiltinEntity _kind ->
            stdlibModuleTypeHint com ctx "typing" "Any" []
        (*
            | Replacements.Util.BclGuid
            | Replacements.Util.BclTimeSpan
            | Replacements.Util.BclDateTime
            | Replacements.Util.BclDateTimeOffset
            | Replacements.Util.BclDateOnly
            | Replacements.Util.BclTimeOnly
            | Replacements.Util.BclTimer
            | Replacements.Util.BclBigInt -> genericEntity fullName [], []
            | Replacements.Util.BclHashSet gen
            | Replacements.Util.FSharpSet gen ->
                let gens, stmts = transformTypeInfo com ctx r genMap gen
                genericEntity fullName [ gens ], stmts
        | entName when entName.StartsWith(Types.choiceNonGeneric) ->
            makeUnionTypeAnnotation com ctx genArgs
            *)
        | Types.fsharpAsyncGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "async_builder" "Async" resolved,
            stmts
        | Types.taskGeneric, _ ->
            stdlibModuleTypeHint com ctx "typing" "Awaitable" genArgs
        | Types.icomparable, _ -> libValue com ctx "util" "IComparable", []
        | Types.iStructuralEquatable, _ ->
            libValue com ctx "util" "IStructuralEquatable", []
        | Types.iStructuralComparable, _ ->
            libValue com ctx "util" "IStructuralComparable", []
        | Types.icomparerGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "util" "IComparer_1" resolved, stmts
        | Types.iequalityComparer, _ ->
            libValue com ctx "util" "IEqualityComparer", []
        | Types.iequalityComparerGeneric, _ ->
            let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

            fableModuleAnnotation
                com
                ctx
                "util"
                "IEqualityComparer_1"
                [ resolved ],
            stmts
        | Types.ienumerator, _ ->
            let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

            fableModuleAnnotation com ctx "util" "IEnumerator" [ resolved ],
            stmts
        | Types.ienumeratorGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "util" "IEnumerator" resolved, stmts
        | Types.ienumerable, _ ->
            let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

            fableModuleAnnotation com ctx "util" "IEnumerable" [ resolved ],
            stmts
        | Types.ienumerableGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "util" "IEnumerable_1" resolved, stmts
        | Types.iequatableGeneric, _ ->
            let resolved, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

            fableModuleAnnotation com ctx "util" "IEquatable" [ resolved ],
            stmts
        | Types.icomparableGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "util" "IComparable_1" resolved, stmts
        | Types.icollection, _
        | Types.icollectionGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "util" "ICollection" resolved, stmts
        | Types.idisposable, _ -> libValue com ctx "util" "IDisposable", []
        | Types.iobserverGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "observable" "IObserver" resolved,
            stmts
        | Types.iobservableGeneric, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "observable" "IObservable" resolved,
            stmts
        | Types.idictionary, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "util" "IDictionary" resolved, stmts
        | Types.ievent2, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation com ctx "event" "IEvent_2" resolved, stmts
        | Types.cancellationToken, _ ->
            libValue com ctx "async_builder" "CancellationToken", []
        | Types.mailboxProcessor, _ ->
            let resolved, stmts =
                resolveGenerics com ctx genArgs repeatedGenerics

            fableModuleAnnotation
                com
                ctx
                "mailbox_processor"
                "MailboxProcessor"
                resolved,
            stmts
        | "Fable.Core.Py.Callable", _ ->
            let any, stmts = stdlibModuleTypeHint com ctx "typing" "Any" []

            let genArgs =
                [
                    Expression.ellipsis
                    any
                ]

            stdlibModuleAnnotation com ctx "collections.abc" "Callable" genArgs,
            stmts
        | _ ->
            let ent = com.GetEntity(entRef)
            // printfn "DeclaredType: %A" ent.FullName
            if ent.IsInterface then
                let name = Helpers.removeNamespace ent.FullName

                // If the interface is imported then it's erased and we need to add the actual imports
                match ent.Attributes with
                | FSharp2Fable.Util.ImportAtt(name, importPath) ->
                    com.GetImportExpr(ctx, importPath, name) |> ignore
                | _ ->
                    match entRef.SourcePath with
                    | Some path when path <> com.CurrentFile ->
                        // this is just to import the interface
                        let importPath =
                            Path.getRelativeFileOrDirPath
                                false
                                com.CurrentFile
                                false
                                path

                        com.GetImportExpr(ctx, importPath, name) |> ignore
                    | _ -> ()

                makeGenericTypeAnnotation com ctx name genArgs repeatedGenerics,
                []
            else
                match tryPyConstructor com ctx ent with
                | Some(entRef, stmts) ->
                    match entRef with
                    (*
                    | Literal(Literal.StringLiteral(StringLiteral(str, _))) ->
                        match str with
                        | "number" -> NumberTypeAnnotation
                        | "boolean" -> BooleanTypeAnnotation
                        | "string" -> StringTypeAnnotation
                        | _ -> AnyTypeAnnotation*)
                    | Expression.Name { Id = Identifier id } ->
                        makeGenericTypeAnnotation
                            com
                            ctx
                            id
                            genArgs
                            repeatedGenerics,
                        stmts
                    // TODO: Resolve references to types in nested modules
                    | _ -> stdlibModuleTypeHint com ctx "typing" "Any" []
                | None -> stdlibModuleTypeHint com ctx "typing" "Any" []

    let makeBuiltinTypeAnnotation com ctx kind repeatedGenerics =
        match kind with
        | Replacements.Util.BclGuid -> Expression.name "str", []
        | Replacements.Util.FSharpReference genArg ->
            makeImportTypeAnnotation com ctx [ genArg ] "types" "FSharpRef", []
        (*
        | Replacements.Util.BclTimeSpan -> NumberTypeAnnotation
        | Replacements.Util.BclDateTime -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.Util.BclDateTimeOffset -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.Util.BclDateOnly -> makeSimpleTypeAnnotation com ctx "Date"
        | Replacements.Util.BclTimeOnly -> NumberTypeAnnotation
        | Replacements.Util.BclTimer -> makeImportTypeAnnotation com ctx [] "Timer" "Timer"
        | Replacements.Util.BclDecimal -> makeImportTypeAnnotation com ctx [] "Decimal" "decimal"
        | Replacements.Util.BclBigInt -> makeImportTypeAnnotation com ctx [] "BigInt/z" "BigInteger"
        | Replacements.Util.BclHashSet key -> makeNativeTypeAnnotation com ctx [key] "Set"
        | Replacements.Util.BclDictionary (key, value) -> makeNativeTypeAnnotation com ctx [key; value] "Map"
        | Replacements.Util.BclKeyValuePair (key, value) -> makeTupleTypeAnnotation com ctx [key; value]
        | Replacements.Util.FSharpSet key -> makeImportTypeAnnotation com ctx [key] "Set" "FSharpSet"
        | Replacements.Util.FSharpMap (key, value) -> makeImportTypeAnnotation com ctx [key; value] "Map" "FSharpMap"
        | Replacements.Util.FSharpChoice genArgs ->
            $"FSharpChoice${List.length genArgs}"
            |> makeImportTypeAnnotation com ctx genArgs "Fable.Core"
        *)
        | Replacements.Util.FSharpResult(ok, err) ->
            let resolved, stmts =
                resolveGenerics
                    com
                    ctx
                    [
                        ok
                        err
                    ]
                    repeatedGenerics

            fableModuleAnnotation com ctx "choice" "FSharpResult_2" resolved,
            stmts
        | _ -> stdlibModuleTypeHint com ctx "typing" "Any" []

    let transformFunctionWithAnnotations
        (com: IPythonCompiler)
        ctx
        name
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let argTypes = args |> List.map (fun id -> id.Type)

        // In Python a generic type arg must appear both in the argument and the return type (cannot appear only once)
        let repeatedGenerics =
            Util.getRepeatedGenericTypeParams ctx (argTypes @ [ body.Type ])

        let args', body' =
            com.TransformFunction(ctx, name, args, body, repeatedGenerics)

        let returnType, stmts =
            typeAnnotation com ctx (Some repeatedGenerics) body.Type

        // If the only argument is generic, then we make the return type optional as well
        let returnType' =
            // printfn "Generic params: %A" (args, repeatedGenerics, body.Type)
            match args, body.Type with
            | [ { Type = Fable.GenericParam(name = x) } ],
              Fable.GenericParam(name = y) when
                x = y && Set.contains x repeatedGenerics
                ->
                Expression.binOp (returnType, BinaryOrBitwise, Expression.none)
            | _ -> returnType

        args', stmts @ body', returnType'

module Util =
    open Lib
    open Reflection
    open Annotation

    let getIdentifier (_com: IPythonCompiler) (_ctx: Context) (name: string) =
        let name = Helpers.clean name
        Identifier name

    let (|TransformExpr|)
        (com: IPythonCompiler)
        ctx
        e
        : Expression * Statement list
        =
        com.TransformAsExpr(ctx, e)

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

    type NamedTailCallOpportunity
        (com: IPythonCompiler, ctx, name, args: Fable.Ident list)
        =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds =
            args
            |> FSharp2Fable.Util.discardUnitArg
            |> List.map (fun arg ->
                let name =
                    getUniqueNameInDeclarationScope ctx (arg.Name + "_mut")
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
            getDecisionTarget ctx targetIndex
            |> snd
            |> isPyStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_, targets) ->
            preferStatement
            || List.exists (snd >> (isPyStatement ctx false)) targets

        | Fable.IfThenElse(_, thenExpr, elseExpr, _) ->
            preferStatement
            || isPyStatement ctx false thenExpr
            || isPyStatement ctx false elseExpr

    let addErrorAndReturnNull
        (com: Compiler)
        (range: SourceLocation option)
        (error: string)
        =
        addError com [] range error
        Expression.none

    let ident (com: IPythonCompiler) (ctx: Context) (id: Fable.Ident) =
        com.GetIdentifier(ctx, id.Name)

    let identAsExpr (com: IPythonCompiler) (ctx: Context) (id: Fable.Ident) =
        com.GetIdentifierAsExpr(ctx, id.Name)

    let thisExpr = Expression.name "self"

    let ofInt (i: int) = Expression.intConstant (int i)

    let ofString (s: string) = Expression.stringConstant s

    let memberFromName
        (_com: IPythonCompiler)
        (_ctx: Context)
        (memberName: string)
        : Expression
        =
        // printfn "memberFromName: %A" memberName
        match memberName with
        | "ToString" -> Expression.identifier "__str__"
        | "GetHashCode" -> Expression.identifier "__hash__"
        | "Equals" -> Expression.identifier "__eq__"
        | "CompareTo" -> Expression.identifier "__cmp__"
        | "set" -> Expression.identifier "__setitem__"
        | "get" -> Expression.identifier "__getitem__"
        | "has" -> Expression.identifier "__contains__"
        | "delete" -> Expression.identifier "__delitem__"
        | n when n.EndsWith("get_Count", StringComparison.Ordinal) ->
            Expression.identifier "__len__" // TODO: find a better way
        | n when n.StartsWith("Symbol.iterator", StringComparison.Ordinal) ->
            let name = Identifier "__iter__"
            Expression.name name
        | n ->
            let n = Naming.toSnakeCase n

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

    let makeArray
        (com: IPythonCompiler)
        ctx
        exprs
        kind
        typ
        : Expression * Statement list
        =
        //printfn "makeArray: %A" (exprs, kind, typ)
        let expr, stmts =
            exprs
            |> List.map (fun e -> com.TransformAsExpr(ctx, e))
            |> Helpers.unzipArgs

        let letter =
            match kind, typ with
            | Fable.ResizeArray, _ -> None
            | _, Fable.Type.Number(UInt8, _) -> Some "B"
            | _, Fable.Type.Number(Int8, _) -> Some "b"
            | _, Fable.Type.Number(Int16, _) -> Some "h"
            | _, Fable.Type.Number(UInt16, _) -> Some "H"
            | _, Fable.Type.Number(Int32, _) -> Some "l"
            | _, Fable.Type.Number(UInt32, _) -> Some "L"
            | _, Fable.Type.Number(Int64, _) -> Some "q"
            | _, Fable.Type.Number(UInt64, _) -> Some "Q"
            | _, Fable.Type.Number(Float32, _) -> Some "f"
            | _, Fable.Type.Number(Float64, _) -> Some "d"
            | _ -> None

        match letter with
        | Some "B" ->
            let bytearray = Expression.name "bytearray"
            Expression.call (bytearray, [ Expression.list expr ]), stmts
        | Some l ->
            let array = com.GetImportExpr(ctx, "array", "array")

            Expression.call (
                array,
                Expression.stringConstant l :: [ Expression.list expr ]
            ),
            stmts
        | _ -> expr |> Expression.list, stmts


    let makeArrayAllocated
        (com: IPythonCompiler)
        ctx
        _typ
        _kind
        (size: Fable.Expr)
        =
        //printfn "makeArrayAllocated"
        let size, stmts = com.TransformAsExpr(ctx, size)
        let array = Expression.list [ Expression.intConstant 0 ]
        Expression.binOp (array, Mult, size), stmts

    let makeArrayFrom
        (com: IPythonCompiler)
        ctx
        typ
        kind
        (fableExpr: Fable.Expr)
        : Expression * Statement list
        =
        match fableExpr with
        | Replacements.Util.ArrayOrListLiteral(exprs, _) ->
            makeArray com ctx exprs kind typ
        | _ ->
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            let name = Expression.name "list"
            Expression.call (name, [ expr ]), stmts

    let makeList (com: IPythonCompiler) ctx exprs =
        let expr, stmts =
            exprs
            |> List.map (fun e -> com.TransformAsExpr(ctx, e))
            |> Helpers.unzipArgs

        expr |> Expression.list, stmts

    let makeTuple (com: IPythonCompiler) ctx exprs =
        let expr, stmts =
            exprs
            |> List.map (fun e -> com.TransformAsExpr(ctx, e))
            |> Helpers.unzipArgs

        expr |> Expression.tuple, stmts

    let makeStringArray strings =
        strings
        |> List.map (fun x -> Expression.stringConstant x)
        |> Expression.list

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

    /// Immediately Invoked Function Expression
    let iife (com: IPythonCompiler) ctx (expr: Fable.Expr) =
        let afe, stmts =
            transformFunctionWithAnnotations com ctx None [] expr
            |||> makeArrowFunctionExpression com ctx None

        Expression.call (afe, []), stmts

    let multiVarDeclaration
        (ctx: Context)
        (variables: (Identifier * Expression option) list)
        =
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

    let varDeclaration
        (ctx: Context)
        (var: Expression)
        (typ: Expression option)
        value
        =
        // printfn "varDeclaration: %A" (var, value, typ)
        match var with
        | Name { Id = id } -> do ctx.BoundVars.Bind([ id ])
        | _ -> ()

        [
            match typ with
            | Some typ ->
                Statement.assign (var, annotation = typ, value = value)
            | _ -> Statement.assign ([ var ], value)
        ]

    let restElement (var: Identifier) =
        let var = Expression.name var
        Expression.starred var

    let callSuper (args: Expression list) =
        let super = Expression.name "super().__init__"
        Expression.call (super, args)

    let callSuperAsStatement (args: Expression list) =
        Statement.expr (callSuper args)

    let makeClassConstructor (args: Arguments) (isOptional: bool) body =
        // printfn "makeClassConstructor: %A" (args.Args, body)
        let name = Identifier("__init__")
        let self = Arg.arg "self"

        let args_ =
            match args.Args with
            | [ _unit ] when isOptional ->
                { args with
                    Args = self :: args.Args
                    Defaults = [ Expression.none ]
                }
            | _ -> { args with Args = self :: args.Args }

        match args.Args, body with
        | [], []
        | [], [ Statement.Pass ] -> [] // Remove empty `__init__` with no arguments
        | _ ->
            [
                Statement.functionDef (
                    name,
                    args_,
                    body = body,
                    returns = Expression.none
                )
            ]

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

    let getGenericTypeParams (types: Fable.Type list) =
        let rec getGenParams =
            function
            | Fable.GenericParam(name = name) -> [ name ]
            | t -> t.Generics |> List.collect getGenParams

        types |> List.collect getGenParams |> Set.ofList

    // Returns type parameters that is used more than once
    let getRepeatedGenericTypeParams ctx (types: Fable.Type list) =
        let rec getGenParams =
            function
            | Fable.GenericParam(name = name) -> [ name ]
            | t -> t.Generics |> List.collect getGenParams

        types
        |> List.collect getGenParams
        |> List.append (ctx.ScopedTypeParams |> Set.toList)
        |> List.countBy id
        |> List.choose (fun (param, count) ->
            if count > 1 then
                Some param
            else
                None
        )
        |> Set.ofList

    type MemberKind =
        | ClassConstructor
        | NonAttached of funcName: string
        | Attached of isStatic: bool

    let getMemberArgsAndBody
        (com: IPythonCompiler)
        ctx
        kind
        hasSpread
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        // printfn "getMemberArgsAndBody: %A" hasSpread
        let funcName, genTypeParams, args, body =
            match kind, args with
            | Attached(isStatic = false), thisArg :: args ->
                let genTypeParams =
                    Set.difference
                        (getGenericTypeParams [ thisArg.Type ])
                        ctx.ScopedTypeParams

                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if isIdentUsed thisArg.Name body then
                        let thisKeyword =
                            Fable.IdentExpr { thisArg with Name = "self" }

                        Fable.Let(thisArg, thisKeyword, body)
                    else
                        body

                None, genTypeParams, args, body
            | Attached(isStatic = true), _
            | ClassConstructor, _ -> None, ctx.ScopedTypeParams, args, body
            | NonAttached funcName, _ -> Some funcName, Set.empty, args, body
            | _ -> None, Set.empty, args, body

        let ctx =
            { ctx with
                ScopedTypeParams = Set.union ctx.ScopedTypeParams genTypeParams
            }

        let args, body, returnType =
            transformFunctionWithAnnotations com ctx funcName args body

        let args =
            let len = args.Args.Length

            if not hasSpread || len = 0 then
                args
            else
                { args with
                    VarArg = Some { args.Args[len - 1] with Annotation = None }
                    Args = args.Args[.. len - 2]
                }

        args, body, returnType

    let getUnionCaseName (uci: Fable.UnionCase) =
        match uci.CompiledName with
        | Some cname -> cname
        | None -> uci.Name

    let getUnionExprTag (com: IPythonCompiler) ctx r (fableExpr: Fable.Expr) =
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

        let expr, stmts' =
            getExpr com ctx r expr (Expression.stringConstant "tag")

        expr, stmts @ stmts'

    let wrapIntExpression typ (e: Expression) =
        match e, typ with
        | Expression.Constant _, _ -> e
        // TODO: Unsigned ints seem to cause problems, should we check only Int32 here?
        | _, Fable.Number((Int8 | Int16 | Int32), _) ->
            Expression.boolOp (
                BoolOperator.Or,
                [
                    e
                    Expression.intConstant 0
                ]
            )
        | _ -> e

    let wrapExprInBlockWithReturn (e, stmts) = stmts @ [ Statement.return' e ]

    let makeArrowFunctionExpression
        com
        ctx
        (name: string option)
        (args: Arguments)
        (body: Statement list)
        returnType
        : Expression * Statement list
        =

        let args =
            match args.Args with
            | [] ->
                let ta = com.GetImportExpr(ctx, "typing", "Any")

                Arguments.arguments (
                    args = [ Arg.arg ("__unit", annotation = ta) ],
                    defaults = [ Expression.none ]
                )
            | _ -> args

        let allDefaultsAreNone =
            args.Defaults
            |> List.forall (
                function
                | Expression.Name { Id = Identifier "None" } -> true
                | _ -> false
            )

        let (|ImmediatelyApplied|_|) =
            function
            | Expression.Call {
                                  Func = callee
                                  Args = appliedArgs
                              } when
                args.Args.Length = appliedArgs.Length && allDefaultsAreNone
                ->
                // To be sure we're not running side effects when deleting the function check the callee is an identifier
                match callee with
                | Expression.Name _ ->
                    let parameters =
                        args.Args |> List.map (fun a -> (Expression.name a.Arg))

                    List.zip parameters appliedArgs
                    |> List.forall (
                        function
                        | Expression.Name({ Id = Identifier name1 }),
                          Expression.Name({ Id = Identifier name2 }) ->
                            name1 = name2
                        | _ -> false
                    )
                    |> function
                        | true -> Some callee
                        | false -> None
                | _ -> None
            | _ -> None

        match body with
        // Check if we can remove the function
        | [ Statement.Return { Value = Some(ImmediatelyApplied(callExpr)) } ] ->
            callExpr, []
        | _ ->
            let ident =
                name
                |> Option.map Identifier
                |> Option.defaultWith (fun _ ->
                    Helpers.getUniqueIdentifier "_arrow"
                )

            let func = createFunction ident args body [] returnType
            Expression.name ident, [ func ]

    let createFunction name args body decoratorList returnType =
        let (|Awaitable|_|) expr =
            match expr with
            | Expression.Call {
                                  Func = Expression.Attribute {
                                                                  Value = Expression.Name {
                                                                                              Id = Identifier "_builder"
                                                                                          }
                                                                  Attr = Identifier "Run"
                                                              }
                              } -> Some expr
            | _ -> None

        let isAsync =
            // function is async is returnType is an Awaitable and the body return a call to _builder.Run
            match returnType with
            | Subscript { Value = Name { Id = Identifier "Awaitable" } } ->
                let rec find body : bool =
                    body
                    |> List.tryFind (
                        function
                        | Statement.Return {
                                               Value = Some(Expression.IfExp {
                                                                                 Body = Awaitable _
                                                                                 OrElse = Awaitable _
                                                                             })
                                           } -> true
                        | Statement.Return { Value = Some(Awaitable _) } -> true
                        | Statement.If {
                                           Body = body
                                           Else = orElse
                                       } -> find body && find orElse
                        | _stmt -> false
                    )
                    |> Option.isSome

                find body
            | _ -> false

        let rec replace body : Statement list =
            body
            |> List.map (
                function
                | Statement.Return {
                                       Value = Some(Expression.IfExp {
                                                                         Test = test
                                                                         Body = body
                                                                         OrElse = orElse
                                                                     })
                                   } ->
                    Statement.return' (
                        Expression.ifExp (
                            test,
                            Expression.Await(body),
                            Expression.Await(orElse)
                        )
                    )
                | Statement.Return { Value = Some(Awaitable(expr)) } ->
                    Statement.return' (Expression.Await expr)
                | Statement.If {
                                   Test = test
                                   Body = body
                                   Else = orElse
                               } ->
                    Statement.if' (test, replace body, orelse = replace orElse)
                | stmt -> stmt
            )

        match isAsync, returnType with
        | true, Subscript { Slice = returnType } ->
            let body' = replace body

            Statement.asyncFunctionDef (
                name = name,
                args = args,
                body = body',
                decoratorList = decoratorList,
                returns = returnType
            )
        | _ ->
            Statement.functionDef (
                name = name,
                args = args,
                body = body,
                decoratorList = decoratorList,
                returns = returnType
            )

    let makeFunction
        name
        (args: Arguments, body: Expression, decoratorList, returnType)
        : Statement
        =
        // printfn "makeFunction: %A" name
        let body = wrapExprInBlockWithReturn (body, [])
        createFunction name args body decoratorList returnType

    let makeFunctionExpression
        (com: IPythonCompiler)
        ctx
        name
        (args, body: Expression, decoratorList, returnType: Expression)
        : Expression * Statement list
        =
        let ctx = { ctx with BoundVars = ctx.BoundVars.EnterScope() }

        let name =
            name
            |> Option.map (fun name -> com.GetIdentifier(ctx, name))
            |> Option.defaultValue (Helpers.getUniqueIdentifier "_expr")

        let func = makeFunction name (args, body, decoratorList, returnType)
        Expression.name name, [ func ]

    let optimizeTailCall
        (com: IPythonCompiler)
        (ctx: Context)
        range
        (tc: ITailCallOpportunity)
        args
        =
        let rec checkCrossRefs tempVars allArgs =
            function
            | [] -> tempVars
            | (argId, _arg) :: rest ->
                let found =
                    allArgs
                    |> List.exists (
                        deepExists (
                            function
                            | Fable.IdentExpr i -> argId = i.Name
                            | _ -> false
                        )
                    )

                let tempVars =
                    if found then
                        let tempVarName =
                            getUniqueNameInDeclarationScope ctx (argId + "_tmp")

                        Map.add argId tempVarName tempVars
                    else
                        tempVars

                checkCrossRefs tempVars allArgs rest

        ctx.OptimizeTailCall()

        let zippedArgs =
            List.zip
                (tc.Args |> List.map (fun { Arg = Identifier id } -> id))
                args

        let tempVars = checkCrossRefs Map.empty args zippedArgs

        let tempVarReplacements =
            tempVars |> Map.map (fun _ v -> makeIdentExpr v)

        [
            // First declare temp variables
            for KeyValue(argId, tempVar) in tempVars do
                yield!
                    varDeclaration
                        ctx
                        (com.GetIdentifierAsExpr(ctx, tempVar))
                        None
                        (com.GetIdentifierAsExpr(ctx, argId))
            // Then assign argument expressions to the original argument identifiers
            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
            for argId, arg in zippedArgs do
                let arg = FableTransforms.replaceValues tempVarReplacements arg
                let arg, stmts = com.TransformAsExpr(ctx, arg)

                yield!
                    stmts
                    @ (assign None (com.GetIdentifierAsExpr(ctx, argId)) arg
                       |> exprAsStatement ctx)
            yield Statement.continue' (?loc = range)
        ]

    let transformImport
        (com: IPythonCompiler)
        ctx
        (_r: SourceLocation option)
        (name: string)
        (moduleName: string)
        =
        let name, parts =
            let parts = Array.toList (name.Split('.'))
            parts.Head, parts.Tail

        com.GetImportExpr(ctx, moduleName, name) |> getParts com ctx parts

    let transformCast
        (com: IPythonCompiler)
        (ctx: Context)
        t
        e
        : Expression * Statement list
        =
        match t with
        // Optimization for (numeric) array or list literals casted to seq
        // Done at the very end of the compile pipeline to get more opportunities
        // of matching cast and literal expressions after resolving pipes, inlining...
        | Fable.DeclaredType(ent, [ _ ]) ->
            match ent.FullName, e with
            | Types.ienumerableGeneric,
              Replacements.Util.ArrayOrListLiteral(exprs, _typ) ->
                let expr, stmts =
                    exprs
                    |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                    |> Helpers.unzipArgs

                let xs = Expression.list expr
                libCall com ctx None "util" "to_enumerable" [ xs ], stmts

            | _ -> com.TransformAsExpr(ctx, e)
        | _ -> com.TransformAsExpr(ctx, e)

    let transformCurry
        (com: IPythonCompiler)
        (ctx: Context)
        expr
        arity
        : Expression * Statement list
        =
        com.TransformAsExpr(
            ctx,
            Replacements.Api.curryExprAtRuntime com arity expr
        )


    let makeInteger
        (com: IPythonCompiler)
        (ctx: Context)
        r
        _t
        intName
        (x: obj)
        =
        let cons = libValue com ctx "types" intName
        let value = Expression.intConstant (x, ?loc = r)
        Expression.call (cons, [ value ], ?loc = r), []

    let makeFloat (com: IPythonCompiler) (ctx: Context) r _t floatName x =
        let cons = libValue com ctx "types" floatName
        let value = Expression.floatConstant (x, ?loc = r)
        Expression.call (cons, [ value ], ?loc = r), []


    let transformValue
        (com: IPythonCompiler)
        (ctx: Context)
        r
        value
        : Expression * Statement list
        =
        match value with
        | Fable.BaseValue(None, _) -> Expression.identifier "super()", []
        | Fable.BaseValue(Some boundIdent, _) ->
            identAsExpr com ctx boundIdent, []
        | Fable.ThisValue _ -> Expression.identifier "self", []
        | Fable.TypeInfo(t, _) -> transformTypeInfo com ctx r Map.empty t
        | Fable.Null _t -> Expression.none, []
        | Fable.UnitConstant -> undefined r, []
        | Fable.BoolConstant x -> Expression.boolConstant (x, ?loc = r), []
        | Fable.CharConstant x ->
            Expression.stringConstant (string<char> x, ?loc = r), []
        | Fable.StringConstant x -> Expression.stringConstant (x, ?loc = r), []
        | Fable.StringTemplate(_, parts, values) ->
            match parts with
            | [] -> makeStrConst ""
            | [ part ] -> makeStrConst part
            | part :: parts ->
                let acc = makeStrConst part

                (acc, List.zip values parts)
                ||> List.fold (fun acc (MaybeCasted(value), part) ->
                    let value =
                        match value.Type with
                        | Fable.String -> value
                        | _ -> Helpers.toString value

                    let acc = makeBinOp None Fable.String acc value BinaryPlus

                    makeBinOp
                        None
                        Fable.String
                        acc
                        (makeStrConst part)
                        BinaryPlus
                )
            |> transformAsExpr com ctx
        | Fable.NumberConstant(x, kind, _) ->
            match kind, x with
            | Decimal, (:? decimal as x) ->
                Py.Replacements.makeDecimal com r value.Type x
                |> transformAsExpr com ctx
            | Int64, (:? int64 as x) ->
                makeInteger com ctx r value.Type "int64" x
            | UInt64, (:? uint64 as x) ->
                makeInteger com ctx r value.Type "uint64" x
            | Int8, (:? int8 as x) -> makeInteger com ctx r value.Type "int8" x
            | UInt8, (:? uint8 as x) ->
                makeInteger com ctx r value.Type "uint8" x
            | Int16, (:? int16 as x) ->
                makeInteger com ctx r value.Type "int16" x
            | UInt16, (:? uint16 as x) ->
                makeInteger com ctx r value.Type "uint16" x
            | Int32, (:? int32 as x) -> Expression.intConstant (x, ?loc = r), []
            | UInt32, (:? uint32 as x) ->
                makeInteger com ctx r value.Type "uint32" x
            //| _, (:? char as x) -> makeNumber com ctx r value.Type "char" x
            | _, x when x = infinity -> Expression.name "float('inf')", []
            | _, x when x = -infinity -> Expression.name "float('-inf')", []
            | _, (:? float as x) when Double.IsNaN(x) ->
                Expression.name "float('nan')", []
            | _, (:? float32 as x) when Single.IsNaN(x) ->
                libCall
                    com
                    ctx
                    r
                    "types"
                    "float32"
                    [ Expression.stringConstant "nan" ],
                []
            | _, (:? float32 as x) ->
                makeFloat com ctx r value.Type "float32" (float x)
            | _, (:? float as x) -> Expression.floatConstant (x, ?loc = r), []
            | _ -> Expression.intConstant (x, ?loc = r), []
        | Fable.NewArray(newKind, typ, kind) ->
            match newKind with
            | Fable.ArrayValues values -> makeArray com ctx values kind typ
            | Fable.ArrayAlloc size -> makeArrayAllocated com ctx typ kind size
            | Fable.ArrayFrom expr -> makeArrayFrom com ctx typ kind expr

        | Fable.NewTuple(vals, _) -> makeTuple com ctx vals
        // Optimization for bundle size: compile list literals as List.ofArray
        | Fable.NewList(headAndTail, _) ->
            let rec getItems acc =
                function
                | None -> List.rev acc, None
                | Some(head, Fable.Value(Fable.NewList(tail, _), _)) ->
                    getItems (head :: acc) tail
                | Some(head, tail) -> List.rev (head :: acc), Some tail

            match getItems [] headAndTail with
            | [], None -> libCall com ctx r "list" "empty" [], []
            | [ TransformExpr com ctx (expr, stmts) ], None ->
                libCall com ctx r "list" "singleton" [ expr ], stmts
            | exprs, None ->
                let expr, stmts = makeList com ctx exprs
                [ expr ] |> libCall com ctx r "list" "ofArray", stmts
            | [ TransformExpr com ctx (head, stmts) ],
              Some(TransformExpr com ctx (tail, stmts')) ->
                libCall
                    com
                    ctx
                    r
                    "list"
                    "cons"
                    [
                        head
                        tail
                    ],
                stmts @ stmts'
            | exprs, Some(TransformExpr com ctx (tail, stmts)) ->
                let expr, stmts' = makeList com ctx exprs

                [
                    expr
                    tail
                ]
                |> libCall com ctx r "list" "ofArrayWithTail",
                stmts @ stmts'
        | Fable.NewOption(value, t, _) ->
            match value with
            | Some(TransformExpr com ctx (e, stmts)) ->
                if mustWrapOption t then
                    libCall com ctx r "option" "some" [ e ], stmts
                else
                    e, stmts
            | None -> undefined r, []
        | Fable.NewRecord(values, ent, _genArgs) ->
            let ent = com.GetEntity(ent)

            let values, stmts =
                List.map (fun x -> com.TransformAsExpr(ctx, x)) values
                |> Helpers.unzipArgs

            let consRef, stmts' = ent |> pyConstructor com ctx
            Expression.call (consRef, values, ?loc = r), stmts @ stmts'
        | Fable.NewAnonymousRecord(values, fieldNames, _genArgs, _isStruct) ->
            let values, stmts =
                values
                |> List.map (fun x -> com.TransformAsExpr(ctx, x))
                |> Helpers.unzipArgs

            List.zip (List.ofArray fieldNames) values |> makePyObject, stmts
        | Fable.NewUnion(values, tag, ent, _genArgs) ->
            let ent = com.GetEntity(ent)

            let values, stmts =
                List.map (fun x -> com.TransformAsExpr(ctx, x)) values
                |> Helpers.unzipArgs

            let consRef, stmts' = ent |> pyConstructor com ctx
            // let caseName = ent.UnionCases |> List.item tag |> getUnionCaseName |> ofString
            let values = (ofInt tag) :: values
            Expression.call (consRef, values, ?loc = r), stmts @ stmts'
        | _ -> failwith $"transformValue: value {value} not supported!"

    let enumerator2iterator com ctx =
        let enumerator =
            Expression.call (
                get
                    com
                    ctx
                    None
                    (Expression.identifier "self")
                    "GetEnumerator"
                    false,
                []
            )

        [
            Statement.return' (
                libCall com ctx None "util" "to_iterator" [ enumerator ]
            )
        ]

    let extractBaseExprFromBaseCall
        (com: IPythonCompiler)
        (ctx: Context)
        (baseType: Fable.DeclaredType option)
        baseCall
        =
        // printfn "extractBaseExprFromBaseCall: %A" (baseCall, baseType)
        match baseCall, baseType with
        | Some(Fable.Call(baseRef, info, _, _)), _ ->
            let baseExpr, stmts =
                match baseRef with
                | Fable.IdentExpr id ->
                    com.GetIdentifierAsExpr(ctx, id.Name), []
                | _ -> transformAsExpr com ctx baseRef

            let expr, keywords, stmts' = transformCallArgs com ctx info

            Some(baseExpr, (expr, keywords, stmts @ stmts'))
        | Some(Fable.ObjectExpr([], Fable.Unit, None)), _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)

            let name =
                baseType
                |> Option.map (fun t -> t.Entity.FullName)
                |> Option.defaultValue "unknown type"

            $"Ignoring base call for %s{name}" |> addWarning com [] range

            None
        | Some(Fable.Value _), Some baseType ->
            // let baseEnt = com.GetEntity(baseType.Entity)
            // let entityName = FSharp2Fable.Helpers.getEntityDeclarationName com baseType.Entity
            // let entityType = FSharp2Fable.Util.getEntityType baseEnt
            // let baseRefId = makeTypedIdent entityType entityName
            // let baseExpr = (baseRefId |> typedIdent com ctx) :> Expression
            // Some (baseExpr, []) // default base constructor
            let range = baseCall |> Option.bind (fun x -> x.Range)

            $"Ignoring base call for %s{baseType.Entity.FullName}"
            |> addWarning com [] range

            None
        | Some _, _ ->
            let range = baseCall |> Option.bind (fun x -> x.Range)

            "Unexpected base call expression, please report"
            |> addError com [] range

            None
        | None, _ -> None

    let transformObjectExpr
        (com: IPythonCompiler)
        ctx
        (members: Fable.ObjectExprMember list)
        typ
        baseCall
        : Expression * Statement list
        =
        // printfn "transformObjectExpr: %A" typ

        // A generic class nested in another generic class cannot use same type variables. (PEP-484)
        let ctx = { ctx with TypeParamsScope = ctx.TypeParamsScope + 1 }

        let makeMethod prop hasSpread args body decorators =
            let args, body, returnType =
                getMemberArgsAndBody
                    com
                    ctx
                    (Attached(isStatic = false))
                    hasSpread
                    args
                    body

            let name =
                let name =
                    match prop with
                    | "ToString" -> "__str__"
                    | _ -> prop

                com.GetIdentifier(ctx, Naming.toSnakeCase name)

            let self = Arg.arg "self"

            let args =
                match decorators with
                // Remove extra parameters from getters, i.e __unit=None
                | [ Expression.Name { Id = Identifier "property" } ] ->
                    { args with
                        Args = [ self ]
                        Defaults = []
                    }
                | _ -> { args with Args = self :: args.Args }

            Statement.functionDef (
                name,
                args,
                body,
                decorators,
                returns = returnType
            )

        let interfaces, stmts =
            match typ with
            | Fable.Any -> [], [] // Don't inherit from Any
            | _ ->
                let ta, stmts = typeAnnotation com ctx None typ
                [ ta ], stmts

        let members =
            members
            |> List.collect (fun memb ->
                let info = com.GetMember(memb.MemberRef)

                if not memb.IsMangled && (info.IsGetter || info.IsValue) then
                    let decorators = [ Expression.name "property" ]

                    [
                        makeMethod
                            memb.Name
                            false
                            memb.Args
                            memb.Body
                            decorators
                    ]
                elif not memb.IsMangled && info.IsSetter then
                    let decorators = [ Expression.name $"{memb.Name}.setter" ]

                    [
                        makeMethod
                            memb.Name
                            false
                            memb.Args
                            memb.Body
                            decorators
                    ]
                elif
                    info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator"
                then
                    let method =
                        makeMethod
                            memb.Name
                            info.HasSpread
                            memb.Args
                            memb.Body
                            []

                    let iterator =
                        let body = enumerator2iterator com ctx
                        let name = com.GetIdentifier(ctx, "__iter__")
                        let args = Arguments.arguments [ Arg.arg "self" ]

                        Statement.functionDef (
                            name = name,
                            args = args,
                            body = body
                        )

                    [
                        method
                        iterator
                    ]
                else
                    [
                        makeMethod
                            memb.Name
                            info.HasSpread
                            memb.Args
                            memb.Body
                            []
                    ]
            )

        let _baseExpr, classMembers =
            baseCall
            |> extractBaseExprFromBaseCall com ctx None
            |> Option.map (fun (baseExpr, (baseArgs, _kw, _stmts)) ->
                let consBody = [ callSuperAsStatement baseArgs ]
                let args = Arguments.empty
                let classCons = makeClassConstructor args false consBody
                Some baseExpr, classCons @ members
            )
            |> Option.defaultValue (None, members)
            |> (fun (expr, memb) -> expr |> Option.toList, memb)

        let classBody =
            match classMembers with
            | [] -> [ Pass ]
            | _ -> classMembers

        let name = Helpers.getUniqueIdentifier "ObjectExpr"

        let stmt =
            Statement.classDef (name, body = classBody, bases = interfaces)

        Expression.call (Expression.name name), [ stmt ] @ stmts

    let transformCallArgs
        (com: IPythonCompiler)
        ctx
        (callInfo: Fable.CallInfo)
        : Expression list * Keyword list * Statement list
        =

        let args =
            FSharp2Fable.Util.dropUnitCallArg
                callInfo.Args
                callInfo.SignatureArgTypes

        let paramsInfo =
            callInfo.MemberRef
            |> Option.bind com.TryGetMember
            |> Option.map getParamsInfo

        let args, objArg, stmts =
            paramsInfo
            |> Option.map (splitNamedArgs args)
            |> function
                | None -> args, None, []
                | Some(args, []) -> args, None, []
                | Some(args, namedArgs) ->
                    let objArg, stmts =
                        namedArgs
                        |> List.choose (fun (p, v) ->
                            match p.Name, v with
                            | Some k,
                              Fable.Value(Fable.NewOption(value, _, _), _) ->
                                value |> Option.map (fun v -> k, v)
                            | Some k, v -> Some(k, v)
                            | None, _ -> None
                        )
                        |> List.map (fun (k, v) ->
                            k, com.TransformAsExpr(ctx, v)
                        )
                        |> List.map (fun (k, (v, stmts)) -> ((k, v), stmts))
                        |> List.unzip
                        |> (fun (kv, stmts) ->
                            kv
                            |> List.map (fun (k, v) ->
                                Keyword.keyword (Identifier k, v)
                            ),
                            stmts |> List.collect id
                        )

                    args, Some objArg, stmts

        let hasSpread =
            paramsInfo
            |> Option.map (fun i -> i.HasSpread)
            |> Option.defaultValue false

        let args, stmts' =
            match args with
            | [] -> [], []
            | args when hasSpread ->
                match List.rev args with
                | [] -> [], []
                | Replacements.Util.ArrayOrListLiteral(spreadArgs, _) :: rest ->
                    let rest =
                        List.rev rest
                        |> List.map (fun e -> com.TransformAsExpr(ctx, e))

                    rest
                    @ (List.map
                        (fun e -> com.TransformAsExpr(ctx, e))
                        spreadArgs)
                    |> Helpers.unzipArgs
                | last :: rest ->
                    let rest, stmts =
                        List.rev rest
                        |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                        |> Helpers.unzipArgs

                    let expr, stmts' = com.TransformAsExpr(ctx, last)
                    rest @ [ Expression.starred expr ], stmts @ stmts'
            | args ->
                List.map (fun e -> com.TransformAsExpr(ctx, e)) args
                |> Helpers.unzipArgs

        match objArg with
        | None -> args, [], stmts @ stmts'
        | Some objArg ->
            //let name = Expression.name(Helpers.getUniqueIdentifier "kw")
            //let kw = Statement.assign([ name], objArg)
            args, objArg, stmts @ stmts'

    let resolveExpr (ctx: Context) _t strategy pyExpr : Statement list =
        // printfn "resolveExpr: %A" (pyExpr, strategy)
        match strategy with
        | None
        | Some ReturnUnit -> exprAsStatement ctx pyExpr
        // TODO: Where to put these int wrappings? Add them also for function arguments?
        | Some ResourceManager
        | Some Return -> [ Statement.return' pyExpr ]
        | Some(Assign left) -> exprAsStatement ctx (assign None left pyExpr)
        | Some(Target left) ->
            exprAsStatement
                ctx
                (assign None (left |> Expression.identifier) pyExpr)

    let transformOperation
        com
        ctx
        range
        opKind
        tags
        : Expression * Statement list
        =
        match opKind with
        // | Fable.Unary (UnaryVoid, TransformExpr com ctx (expr, stmts)) -> Expression.none, stmts
        // | Fable.Unary (UnaryTypeof, TransformExpr com ctx (expr, stmts)) ->
        //     let func = Expression.name ("type")
        //     let args = [ expr ]
        //     Expression.call (func, args), stmts

        // Transform `~(~(a/b))` to `a // b`
        | Fable.Unary(UnaryOperator.UnaryNotBitwise,
                      Fable.Operation(
                          kind = Fable.Unary(UnaryOperator.UnaryNotBitwise,
                                             Fable.Operation(
                                                 kind = Fable.Binary(BinaryOperator.BinaryDivide,
                                                                     TransformExpr com ctx (left,
                                                                                            stmts),
                                                                     TransformExpr com ctx (right,
                                                                                            stmts')))))) ->
            Expression.binOp (left, FloorDiv, right), stmts @ stmts'
        | Fable.Unary(UnaryOperator.UnaryNotBitwise,
                      Fable.Operation(
                          kind = Fable.Unary(UnaryOperator.UnaryNotBitwise,
                                             TransformExpr com ctx (left, stmts)))) ->
            let name = Expression.name "int"
            Expression.call (name, [ left ]), stmts
        | Fable.Unary(op, TransformExpr com ctx (expr, stmts)) ->
            Expression.unaryOp (op, expr, ?loc = range), stmts

        // | Fable.Binary (BinaryInstanceOf, TransformExpr com ctx (left, stmts), TransformExpr com ctx (right, stmts')) ->
        //     let func = Expression.name ("isinstance")
        //     let args = [ left; right ]
        //     Expression.call (func, args), stmts' @ stmts

        | Fable.Binary(op,
                       TransformExpr com ctx (left, stmts),
                       TransformExpr com ctx (right, stmts')) ->
            let compare op =
                Expression.compare (left, [ op ], [ right ], ?loc = range),
                stmts @ stmts'

            let (|IsNone|_|) =
                function
                | Name { Id = Identifier "None" } -> Some()
                | _ -> None

            let strict =
                match tags with
                | Fable.Tags.Contains "strict" -> true
                | _ -> false

            match op, strict with
            | BinaryEqual, true ->
                match left, right with
                // Use == with literals
                | Constant _, _ -> compare Eq
                | _, Constant _ -> compare Eq
                | _ -> compare Is
            | BinaryEqual, false ->
                match left, right with
                // Use == with literals
                | Constant _, _ -> compare Eq
                | _, Constant _ -> compare Eq
                // Use `is` with None (except literals)
                | _, IsNone -> compare Is
                | IsNone, _ -> compare Is
                // Use == for the rest
                | _ -> compare Eq
            | BinaryUnequal, true ->
                match left, right with
                // Use == with literals
                | Constant _, _ -> compare NotEq
                | _, Constant _ -> compare NotEq
                | _ -> compare IsNot
            | BinaryUnequal, false ->
                match left, right with
                // Use != with literals
                | Constant _, _ -> compare NotEq
                | _, Constant _ -> compare NotEq
                // Use `is not` with None (except literals)
                | _, IsNone -> compare IsNot
                | IsNone, _ -> compare IsNot
                // Use != for the rest
                | _ -> compare NotEq
            | BinaryLess, _ -> compare Lt
            | BinaryLessOrEqual, _ -> compare LtE
            | BinaryGreater, _ -> compare Gt
            | BinaryGreaterOrEqual, _ -> compare GtE
            | _ ->
                Expression.binOp (left, op, right, ?loc = range), stmts @ stmts'

        | Fable.Logical(op,
                        TransformExpr com ctx (left, stmts),
                        TransformExpr com ctx (right, stmts')) ->
            Expression.boolOp (
                op,
                [
                    left
                    right
                ],
                ?loc = range
            ),
            stmts @ stmts'

    let transformEmit (com: IPythonCompiler) ctx range (info: Fable.EmitInfo) =
        let macro = info.Macro
        let info = info.CallInfo

        let thisArg, stmts =
            info.ThisArg
            |> Option.map (fun e -> com.TransformAsExpr(ctx, e))
            |> Option.toList
            |> Helpers.unzipArgs

        let exprs, _, stmts' = transformCallArgs com ctx info

        if macro.StartsWith("functools", StringComparison.Ordinal) then
            com.GetImportExpr(ctx, "functools") |> ignore

        let args = exprs |> List.append thisArg
        emitExpression range macro args, stmts @ stmts'

    let transformCall
        (com: IPythonCompiler)
        ctx
        range
        callee
        (callInfo: Fable.CallInfo)
        : Expression * Statement list
        =
        // printfn "transformCall: %A" (callee, callInfo)
        let callee', stmts = com.TransformAsExpr(ctx, callee)

        let args, kw, stmts' = transformCallArgs com ctx callInfo

        match callee, callInfo.ThisArg with
        | Fable.Get(expr, Fable.FieldGet { Name = "Dispose" }, _, _), _ ->
            let expr, stmts'' = com.TransformAsExpr(ctx, expr)

            libCall com ctx range "util" "dispose" [ expr ],
            stmts @ stmts' @ stmts''
        | Fable.Get(expr, Fable.FieldGet { Name = "set" }, _, _), _ ->
            // printfn "Type: %A" expr.Type
            let right, stmts = com.TransformAsExpr(ctx, callInfo.Args.Head)

            let arg, stmts' = com.TransformAsExpr(ctx, callInfo.Args.Tail.Head)
            let value, stmts'' = com.TransformAsExpr(ctx, expr)

            Expression.none,
            Statement.assign ([ Expression.subscript (value, right) ], arg)
            :: stmts
            @ stmts'
            @ stmts''
        | Fable.Get(_, Fable.FieldGet { Name = "sort" }, _, _), _ ->
            callFunction range callee' [] kw, stmts @ stmts'

        | _, Some(TransformExpr com ctx (thisArg, stmts'')) ->
            callFunction range callee' (thisArg :: args) kw,
            stmts @ stmts' @ stmts''
        | _, None when List.contains "new" callInfo.Tags ->
            Expression.call (callee', args, kw, ?loc = range), stmts @ stmts'
        | _, None -> callFunction range callee' args kw, stmts @ stmts'

    let transformCurriedApply
        com
        ctx
        range
        (TransformExpr com ctx (applied, stmts))
        args
        =
        ((applied, stmts), args)
        ||> List.fold (fun (applied, stmts) arg ->
            let args, stmts' =
                match arg with
                // TODO: If arg type is unit but it's an expression with potential
                // side-effects, we need to extract it and execute it before the call

                // TODO: discardUnitArg may still be needed in some cases
                // | Fable.Value(Fable.UnitConstant,_) -> [], []
                // | Fable.IdentExpr ident when ident.Type = Fable.Unit -> [], []
                | TransformExpr com ctx (arg, stmts') -> [ arg ], stmts'

            callFunction range applied args [], stmts @ stmts'
        )

    let transformCallAsStatements
        com
        ctx
        range
        t
        returnStrategy
        callee
        callInfo
        =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args
            + (if Option.isSome i.ThisArg then
                   1
               else
                   0)
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return | ReturnUnit), Some tc when
            tc.IsRecursiveRef(callee) && argsLen callInfo = List.length tc.Args
            ->
            let args =
                match callInfo.ThisArg with
                | Some thisArg -> thisArg :: callInfo.Args
                | None -> callInfo.Args

            optimizeTailCall com ctx range tc args
        | _ ->
            let expr, stmts = transformCall com ctx range callee callInfo
            stmts @ (expr |> resolveExpr ctx t returnStrategy)

    let transformCurriedApplyAsStatements
        com
        ctx
        range
        t
        returnStrategy
        callee
        args
        =
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return | ReturnUnit), Some tc when
            tc.IsRecursiveRef(callee) && List.sameLength args tc.Args
            ->
            optimizeTailCall com ctx range tc args
        | _ ->
            let expr, stmts = transformCurriedApply com ctx range callee args

            stmts @ (expr |> resolveExpr ctx t returnStrategy)

    let getNonLocals ctx (body: Statement list) =
        let body, nonLocals =
            body
            |> List.partition (
                function
                | Statement.NonLocal _
                | Statement.Global _ -> false
                | _ -> true
            )

        let nonLocal =
            nonLocals
            |> List.collect (
                function
                | Statement.NonLocal nl -> nl.Names
                | Statement.Global gl -> gl.Names
                | _ -> []
            )
            |> List.distinct
            |> (fun names ->
                match ctx.BoundVars.Inceptions with
                | 1 -> Statement.global' names
                | _ -> Statement.nonLocal names
            )

        [ nonLocal ], body

    let transformBody
        (_com: IPythonCompiler)
        ctx
        _ret
        (body: Statement list)
        : Statement list
        =
        match body with
        | [] -> [ Pass ]
        | _ ->
            let nonLocals, body = getNonLocals ctx body
            nonLocals @ body

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock
        (com: IPythonCompiler)
        ctx
        ret
        (expr: Fable.Expr)
        : Statement list
        =
        let block =
            com.TransformAsStatements(ctx, ret, expr)
            |> List.choose Helpers.isProductiveStatement

        match block with
        | [] -> [ Pass ]
        | _ -> block |> transformBody com ctx ret

    let transformTryCatch
        com
        (ctx: Context)
        r
        returnStrategy
        (body, catch: option<Fable.Ident * Fable.Expr>, finalizer)
        =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }

        let handlers =
            catch
            |> Option.map (fun (param, body) ->
                let body = transformBlock com ctx returnStrategy body
                let exn = Expression.identifier "Exception" |> Some
                let identifier = ident com ctx param

                [
                    ExceptHandler.exceptHandler (
                        ``type`` = exn,
                        name = identifier,
                        body = body
                    )
                ]
            )

        let finalizer, stmts =
            match finalizer with
            | Some finalizer ->
                finalizer
                |> transformBlock com ctx None
                |> List.partition (
                    function
                    | Statement.NonLocal _
                    | Statement.Global _ -> false
                    | _ -> true
                )
            | None -> [], []

        stmts
        @ [
            Statement.try' (
                transformBlock com ctx returnStrategy body,
                ?handlers = handlers,
                finalBody = finalizer,
                ?loc = r
            )
        ]

    let rec transformIfStatement
        (com: IPythonCompiler)
        ctx
        r
        ret
        guardExpr
        thenStmnt
        elseStmnt
        =
        // printfn "transformIfStatement"
        let expr, stmts = com.TransformAsExpr(ctx, guardExpr)

        match expr with
        | Constant(value = BoolLiteral value) ->
            stmts @ com.TransformAsStatements(ctx, ret, thenStmnt)
        | guardExpr ->
            let thenStmnt, stmts' =
                transformBlock com ctx ret thenStmnt
                |> List.partition (
                    function
                    | Statement.NonLocal _
                    | Statement.Global _ -> false
                    | _ -> true
                )

            let ifStatement, stmts'' =
                let block, stmts =
                    transformBlock com ctx ret elseStmnt
                    |> List.partition (
                        function
                        | Statement.NonLocal _
                        | Statement.Global _ -> false
                        | _ -> true
                    )

                match block with
                | [] -> Statement.if' (guardExpr, thenStmnt, ?loc = r), stmts
                | [ elseStmnt ] ->
                    Statement.if' (
                        guardExpr,
                        thenStmnt,
                        [ elseStmnt ],
                        ?loc = r
                    ),
                    stmts
                | statements ->
                    Statement.if' (guardExpr, thenStmnt, statements, ?loc = r),
                    stmts

            stmts @ stmts' @ stmts'' @ [ ifStatement ]

    let transformGet
        (com: IPythonCompiler)
        ctx
        range
        typ
        (fableExpr: Fable.Expr)
        kind
        =
        // printfn "transformGet: %A" kind
        // printfn "transformGet: %A" (fableExpr.Type)

        match kind with
        | Fable.ExprGet(TransformExpr com ctx (prop, stmts)) ->
            let expr, stmts' = com.TransformAsExpr(ctx, fableExpr)
            let expr, stmts'' = getExpr com ctx range expr prop
            expr, stmts @ stmts' @ stmts''

        | Fable.FieldGet i ->
            // printfn "Fable.FieldGet: %A" (i.Name, fableExpr.Type)
            let fieldName = i.Name |> Naming.toSnakeCase // |> Helpers.clean

            let fableExpr =
                match fableExpr with
                // If we're accessing a virtual member with default implementation (see #701)
                // from base class, we can use `super` in JS so we don't need the bound this arg
                | Fable.Value(Fable.BaseValue(_, t), r) ->
                    Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr

            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

            let subscript =
                match fableExpr.Type with
                | Fable.AnonymousRecordType _ -> true
                | Fable.GenericParam(_,
                                     _,
                                     [ Fable.Constraint.HasMember(_, false) ]) ->
                    true
                | _ -> false
            // printfn "Fable.FieldGet: %A" (fieldName, fableExpr.Type)
            get com ctx range expr fieldName subscript, stmts

        | Fable.ListHead ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            libCall com ctx range "list" "head" [ expr ], stmts

        | Fable.ListTail ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)
            libCall com ctx range "list" "tail" [ expr ], stmts

        | Fable.TupleIndex index ->
            match fableExpr with
            // TODO: Check the erased expressions don't have side effects?
            | Fable.Value(Fable.NewTuple(exprs, _), _) ->
                com.TransformAsExpr(ctx, List.item index exprs)
            | TransformExpr com ctx (expr, stmts) ->
                let expr, stmts' = getExpr com ctx range expr (ofInt index)
                expr, stmts @ stmts'

        | Fable.OptionValue ->
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

            if mustWrapOption typ || com.Options.Language = TypeScript then
                libCall com ctx None "option" "value" [ expr ], stmts
            else
                expr, stmts

        | Fable.UnionTag ->
            let expr, stmts = getUnionExprTag com ctx range fableExpr
            expr, stmts

        | Fable.UnionField i ->
            let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

            let expr, stmts' =
                getExpr com ctx None expr (Expression.stringConstant "fields")

            let expr, stmts'' = getExpr com ctx range expr (ofInt i.FieldIndex)

            expr, stmts @ stmts' @ stmts''

    let transformSet
        (com: IPythonCompiler)
        ctx
        range
        fableExpr
        typ
        (value: Fable.Expr)
        kind
        =
        // printfn "transformSet: %A" (fableExpr, value)
        let expr, stmts = com.TransformAsExpr(ctx, fableExpr)

        let value, stmts' =
            let value, st = com.TransformAsExpr(ctx, value)
            value |> wrapIntExpression typ, st

        let ret, stmts'' =
            match kind with
            | Fable.ValueSet -> expr, []
            | Fable.ExprSet(TransformExpr com ctx (e, stmts'')) ->
                let expr, stmts''' = getExpr com ctx None expr e
                expr, stmts'' @ stmts'''
            | Fable.FieldSet fieldName ->
                let fieldName = fieldName |> Naming.toSnakeCase |> Helpers.clean
                get com ctx None expr fieldName false, []

        assign range ret value, stmts @ stmts' @ stmts''

    let transformBindingExprBody
        (com: IPythonCompiler)
        (ctx: Context)
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
        match value with
        | Function(args, body) ->
            let name = Some var.Name

            transformFunctionWithAnnotations com ctx name args body
            |||> makeArrowFunctionExpression com ctx name
        | _ ->
            let expr, stmt = com.TransformAsExpr(ctx, value)
            expr |> wrapIntExpression value.Type, stmt

    let transformBindingAsExpr
        (com: IPythonCompiler)
        ctx
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
        //printfn "transformBindingAsExpr: %A" (var, value)
        let expr, stmts = transformBindingExprBody com ctx var value
        expr |> assign None (identAsExpr com ctx var), stmts

    let transformBindingAsStatements
        (com: IPythonCompiler)
        ctx
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
        // printfn "transformBindingAsStatements: %A" (var, value)
        if isPyStatement ctx false value then
            let varName, varExpr =
                Expression.name var.Name, identAsExpr com ctx var

            ctx.BoundVars.Bind(var.Name)
            let ta, stmts = typeAnnotation com ctx None var.Type
            let decl = Statement.assign (varName, ta)

            let body =
                com.TransformAsStatements(ctx, Some(Assign varExpr), value)

            stmts @ [ decl ] @ body
        else
            let value, stmts = transformBindingExprBody com ctx var value
            let varName = com.GetIdentifierAsExpr(ctx, var.Name)
            let ta, stmts' = typeAnnotation com ctx None var.Type
            let decl = varDeclaration ctx varName (Some ta) value
            stmts @ stmts' @ decl

    let transformTest
        (com: IPythonCompiler)
        ctx
        range
        kind
        expr
        : Expression * Statement list
        =
        match kind with
        | Fable.TypeTest t -> transformTypeTest com ctx range expr t

        | Fable.OptionTest nonEmpty ->
            let op =
                if nonEmpty then
                    IsNot
                else
                    Is

            let expr, stmts = com.TransformAsExpr(ctx, expr)

            Expression.compare (expr, [ op ], [ Expression.none ], ?loc = range),
            stmts

        | Fable.ListTest nonEmpty ->
            let expr, stmts = com.TransformAsExpr(ctx, expr)
            let expr = libCall com ctx range "list" "isEmpty" [ expr ]

            if nonEmpty then
                Expression.unaryOp (UnaryNot, expr, ?loc = range), stmts
            else
                expr, stmts

        | Fable.UnionCaseTest tag ->
            let expected = ofInt tag
            let actual, stmts = getUnionExprTag com ctx None expr

            Expression.compare (actual, [ Eq ], [ expected ], ?loc = range),
            stmts

    let transformSwitch
        (com: IPythonCompiler)
        ctx
        _useBlocks
        returnStrategy
        evalExpr
        cases
        defaultCase
        : Statement list
        =
        let cases =
            cases
            |> List.collect (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | None, Fable.Value(Fable.UnitConstant, _), _
                | _, _, [] -> []
                | _, _, guards ->
                    let guards, lastGuard = List.splitLast guards

                    let guards =
                        guards
                        |> List.map (fun e ->
                            let expr, stmts = com.TransformAsExpr(ctx, e)
                            (stmts, Some expr)
                        )

                    let caseBody =
                        com.TransformAsStatements(ctx, returnStrategy, expr)

                    let caseBody =
                        match returnStrategy with
                        | Some Return -> caseBody
                        | _ -> List.append caseBody [ Statement.break' () ]

                    let expr, stmts = com.TransformAsExpr(ctx, lastGuard)
                    guards @ [ (stmts @ caseBody, Some expr) ]
            )

        let cases =
            match defaultCase with
            | Some expr ->
                let defaultCaseBody =
                    com.TransformAsStatements(ctx, returnStrategy, expr)

                cases @ [ (defaultCaseBody, None) ]
            | None -> cases

        let value, stmts = com.TransformAsExpr(ctx, evalExpr)

        let rec ifThenElse
            (fallThrough: Expression option)
            (cases: (Statement list * Expression option) list)
            : Statement list
            =
            match cases with
            | [] -> []
            | (body, test) :: cases ->
                match test with
                | None -> body
                | Some test ->
                    let expr =
                        Expression.compare (
                            left = value,
                            ops = [ Eq ],
                            comparators = [ test ]
                        )

                    let test =
                        match fallThrough with
                        | Some ft ->
                            Expression.boolOp (
                                op = Or,
                                values =
                                    [
                                        ft
                                        expr
                                    ]
                            )
                        | _ -> expr

                    // Check for fallthrough
                    if body.IsEmpty then
                        ifThenElse (Some test) cases
                    else
                        // Remove any break statements from body
                        let body =
                            body
                            |> List.filter (
                                function
                                | Statement.Break -> false
                                | _ -> true
                            )
                            |> function
                                // Make sure we don't have an empty body
                                | [] -> [ Statement.Pass ]
                                | body -> body

                        let nonLocals, body = getNonLocals ctx body

                        let nonLocals, orElse =
                            ifThenElse None cases
                            |> List.append nonLocals
                            |> getNonLocals ctx

                        nonLocals
                        @ [
                            Statement.if' (
                                test = test,
                                body = body,
                                orelse = orElse
                            )
                        ]

        let result = cases |> ifThenElse None

        match result with
        | [] -> []
        | ifStmt -> stmts @ ifStmt

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then
            []
        elif List.sameLength idents values then
            List.zip idents values
        else
            failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues
        (com: IPythonCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let idents, target = getDecisionTarget ctx targetIndex

        let identsAndValues = matchTargetIdentAndValues idents boundValues

        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr) :: bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements
                )

            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccessAsExpr
        (com: IPythonCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let bindings, target =
            getDecisionTargetAndBindValues com ctx targetIndex boundValues

        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target =
                List.rev bindings
                |> List.fold (fun e (i, v) -> Fable.Let(i, v, e)) target

            com.TransformAsExpr(ctx, target)

    let exprAsStatement (ctx: Context) (expr: Expression) : Statement list =
        // printfn "exprAsStatement: %A" expr
        match expr with
        // A single None will be removed (i.e transformCall may return None)
        | Name { Id = Identifier "None" } -> []
        | NamedExpr({
                        Target = target
                        Value = value
                        Loc = _
                    }) ->
            let nonLocals =
                match target with
                | Expression.Name { Id = id } ->
                    ctx.BoundVars.NonLocals([ id ])
                    |> Statement.nonLocal
                    |> List.singleton
                | _ -> []

            // printfn "Nonlocals: %A" nonLocals
            nonLocals @ [ Statement.assign ([ target ], value) ]
        | _ -> [ Statement.expr expr ]

    let transformDecisionTreeSuccessAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        returnStrategy
        targetIndex
        boundValues
        : Statement list
        =
        match returnStrategy with
        | Some(Target targetId) as _target ->
            let idents, _ = getDecisionTarget ctx targetIndex

            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.collect (fun (id, TransformExpr com ctx (value, stmts)) ->
                    let stmts' =
                        assign None (identAsExpr com ctx id) value
                        |> exprAsStatement ctx

                    stmts @ stmts'
                )

            let targetAssignment =
                assign None (targetId |> Expression.name) (ofInt targetIndex)
                |> exprAsStatement ctx

            targetAssignment @ assignments
        | ret ->
            let bindings, target =
                getDecisionTargetAndBindValues com ctx targetIndex boundValues

            let bindings =
                bindings
                |> Seq.collect (fun (i, v) ->
                    transformBindingAsStatements com ctx i v
                )
                |> Seq.toList

            bindings @ com.TransformAsStatements(ctx, ret, target)

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) =
            function
            | Fable.Operation(Fable.Binary(BinaryEqual, expr, right), _, _, _) ->
                match expr with
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _),
                              _) -> Some(expr, right)
                | _ -> None
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr =
                    Fable.Get(
                        expr,
                        Fable.UnionTag,
                        Fable.Number(Int32, Fable.NumberInfo.Empty),
                        None
                    )

                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None

        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1, Fable.UnionTag, _, _),
              Fable.Get(Fable.IdentExpr i2, Fable.UnionTag, _, _) ->
                i1.Name = i2.Name
            | _ -> false

        let rec checkInner cases evalExpr =
            function
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex,
                                                         boundValues,
                                                         _),
                               treeExpr,
                               _) when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex,
                                            defaultBoundValues,
                                            _) ->
                    let cases =
                        (caseExpr, targetIndex, boundValues) :: cases
                        |> List.rev

                    Some(
                        evalExpr,
                        cases,
                        (defaultTargetIndex, defaultBoundValues)
                    )
                | treeExpr ->
                    checkInner
                        ((caseExpr, targetIndex, boundValues) :: cases)
                        evalExpr
                        treeExpr
            | _ -> None

        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                           treeExpr,
                           _) ->
            match
                checkInner
                    [ caseExpr, targetIndex, boundValues ]
                    evalExpr
                    treeExpr
            with
            | Some(evalExpr, cases, defaultCase) ->
                Some(evalExpr, cases, defaultCase)
            | None -> None
        | _ -> None

    let transformDecisionTreeAsExpr
        (com: IPythonCompiler)
        (ctx: Context)
        targets
        expr
        : Expression * Statement list
        =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let groupSwitchCases
        t
        (cases: (Fable.Expr * int * Fable.Expr list) list)
        (defaultIndex, defaultBoundValues)
        =
        cases
        |> List.groupBy (fun (_, idx, boundValues) ->
            // Try to group cases with some target index and empty bound values
            // If bound values are non-empty use also a non-empty Guid to prevent grouping
            if List.isEmpty boundValues then
                idx, Guid.Empty
            else
                idx, Guid.NewGuid()
        )
        |> List.map (fun ((idx, _), cases) ->
            let caseExprs = cases |> List.map Tuple3.item1
            // If there are multiple cases, it means boundValues are empty
            // (see `groupBy` above), so it doesn't mind which one we take as reference
            let boundValues = cases |> List.head |> Tuple3.item3
            caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t)
        )
        |> function
            | [] -> []
            // Check if the last case can also be grouped with the default branch, see #2357
            | cases when List.isEmpty defaultBoundValues ->
                match List.splitLast cases with
                | cases, (_, Fable.DecisionTreeSuccess(idx, [], _)) when
                    idx = defaultIndex
                    ->
                    cases
                | _ -> cases
            | cases -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int, int>) =
            function
            | [] -> targetRefs
            | expr :: exprs ->
                match expr with
                // We shouldn't actually see this, but shortcircuit just in case
                | Fable.DecisionTree _ -> findSuccess targetRefs exprs
                | Fable.DecisionTreeSuccess(idx, _, _) ->
                    let count =
                        Map.tryFind idx targetRefs |> Option.defaultValue 0

                    let targetRefs = Map.add idx (count + 1) targetRefs
                    findSuccess targetRefs exprs
                | expr ->
                    let exprs2 = getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)

        findSuccess Map.empty [ expr ]
        |> Seq.choose (fun kv ->
            if kv.Value > 1 then
                Some kv.Key
            else
                None
        )
        |> Seq.toList

    /// When several branches share target create first a switch to get the target index and bind value
    /// and another to execute the actual target
    let transformDecisionTreeWithTwoSwitches
        (com: IPythonCompiler)
        ctx
        returnStrategy
        (targets: (Fable.Ident list * Fable.Expr) list)
        treeExpr
        =
        // Declare target and bound idents
        let targetId =
            getUniqueNameInDeclarationScope ctx "pattern_matching_result"
            |> makeIdent

        let multiVarDecl =
            let boundIdents =
                targets
                |> List.collect (fun (idents, _) -> idents)
                |> List.map (fun id -> ident com ctx id, None)

            multiVarDeclaration
                ctx
                ((ident com ctx targetId, None) :: boundIdents)
        // Transform targets as switch
        let switch2 =
            // TODO: Declare the last case as the default case?
            let cases =
                targets
                |> List.mapi (fun i (_, target) -> [ makeIntConst i ], target)

            transformSwitch
                com
                ctx
                true
                returnStrategy
                (targetId |> Fable.IdentExpr)
                cases
                None

        // Transform decision tree
        let targetAssign = Target(ident com ctx targetId)
        let ctx = { ctx with DecisionTargets = targets }

        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases =
                groupSwitchCases
                    (Fable.Number(Int32, Fable.NumberInfo.Empty))
                    cases
                    (defaultIndex, defaultBoundValues)

            let defaultCase =
                Fable.DecisionTreeSuccess(
                    defaultIndex,
                    defaultBoundValues,
                    Fable.Number(Int32, Fable.NumberInfo.Empty)
                )

            let switch1 =
                transformSwitch
                    com
                    ctx
                    false
                    (Some targetAssign)
                    evalExpr
                    cases
                    (Some defaultCase)

            multiVarDecl @ switch1 @ switch2
        | None ->
            let decisionTree =
                com.TransformAsStatements(ctx, Some targetAssign, treeExpr)

            multiVarDecl @ decisionTree @ switch2

    let transformDecisionTreeAsStatements
        (com: IPythonCompiler)
        (ctx: Context)
        returnStrategy
        (targets: (Fable.Ident list * Fable.Expr) list)
        (treeExpr: Fable.Expr)
        : Statement list
        =
        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        let targetsWithMultiRefs =
            if com.Options.Language = TypeScript then
                [] // no hoisting when compiled with types
            else
                getTargetsWithMultipleReferences treeExpr

        match targetsWithMultiRefs with
        | [] ->
            let ctx = { ctx with DecisionTargets = targets }

            match transformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                let t = treeExpr.Type

                let cases =
                    cases
                    |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                        [ caseExpr ],
                        Fable.DecisionTreeSuccess(targetIndex, boundValues, t)
                    )

                let defaultCase =
                    Fable.DecisionTreeSuccess(
                        defaultIndex,
                        defaultBoundValues,
                        t
                    )

                transformSwitch
                    com
                    ctx
                    true
                    returnStrategy
                    evalExpr
                    cases
                    (Some defaultCase)
            | None -> com.TransformAsStatements(ctx, returnStrategy, treeExpr)
        | targetsWithMultiRefs ->
            // If the bound idents are not referenced in the target, remove them
            let targets =
                targets
                |> List.map (fun (idents, expr) ->
                    idents
                    |> List.exists (fun i -> isIdentUsed i.Name expr)
                    |> function
                        | true -> idents, expr
                        | false -> [], expr
                )

            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs
                |> List.exists (fun idx ->
                    targets[idx] |> fst |> List.isEmpty |> not
                )

            if not hasAnyTargetWithMultiRefsBoundValues then
                match transformDecisionTreeAsSwitch treeExpr with
                | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                    let t = treeExpr.Type

                    let cases =
                        groupSwitchCases
                            t
                            cases
                            (defaultIndex, defaultBoundValues)

                    let ctx = { ctx with DecisionTargets = targets }

                    let defaultCase =
                        Fable.DecisionTreeSuccess(
                            defaultIndex,
                            defaultBoundValues,
                            t
                        )

                    transformSwitch
                        com
                        ctx
                        true
                        returnStrategy
                        evalExpr
                        cases
                        (Some defaultCase)
                | None ->
                    transformDecisionTreeWithTwoSwitches
                        com
                        ctx
                        returnStrategy
                        targets
                        treeExpr
            else
                transformDecisionTreeWithTwoSwitches
                    com
                    ctx
                    returnStrategy
                    targets
                    treeExpr

    let transformSequenceExpr
        (com: IPythonCompiler)
        ctx
        (exprs: Fable.Expr list)
        : Expression * Statement list
        =
        // printfn "transformSequenceExpr"
        let ctx = { ctx with BoundVars = ctx.BoundVars.EnterScope() }

        let body =
            exprs
            |> List.collecti (fun i e ->
                let expr, stmts = com.TransformAsExpr(ctx, e)
                // Return the last expression
                if i = exprs.Length - 1 then
                    stmts @ [ Statement.return' expr ]
                else
                    stmts @ exprAsStatement ctx expr
            )
            |> transformBody com ctx None

        let name = Helpers.getUniqueIdentifier "_expr"

        let func =
            Statement.functionDef (
                name = name,
                args = Arguments.arguments [],
                body = body
            )

        let name = Expression.name name
        Expression.call name, [ func ]

    let transformSequenceExpr'
        (_com: IPythonCompiler)
        ctx
        (exprs: Expression list)
        (stmts: Statement list)
        : Expression * Statement list
        =
        // printfn "transformSequenceExpr2', exprs: %A" exprs.Length
        // printfn "ctx: %A" ctx.BoundVars
        let ctx = { ctx with BoundVars = ctx.BoundVars.EnterScope() }

        let body =
            exprs
            |> List.collecti (fun i expr ->
                // Return the last expression
                if i = exprs.Length - 1 then
                    stmts @ [ Statement.return' expr ]
                else
                    exprAsStatement ctx expr
            )

        let name = Helpers.getUniqueIdentifier "_expr"

        let func =
            Statement.functionDef (
                name = name,
                args = Arguments.arguments [],
                body = body
            )

        let name = Expression.name name
        Expression.call name, [ func ]

    let rec transformAsExpr
        (com: IPythonCompiler)
        ctx
        (expr: Fable.Expr)
        : Expression * Statement list
        =
        // printfn "transformAsExpr: %A" expr
        match expr with
        | Fable.Unresolved(_, _, r) ->
            addErrorAndReturnNull com r "Unexpected unresolved expression", []

        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> identAsExpr com ctx id, []

        | Fable.Import({
                           Selector = selector
                           Path = path
                       },
                       _,
                       r) ->
            // printfn "Fable.Import: %A" (selector, path)
            transformImport com ctx r selector path, []

        | Fable.Test(expr, kind, range) -> transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformFunctionWithAnnotations com ctx name [ arg ] body
            |||> makeArrowFunctionExpression com ctx name

        | Fable.Delegate(args, body, name, _) ->
            transformFunctionWithAnnotations com ctx name args body
            |||> makeArrowFunctionExpression com ctx name

        | Fable.ObjectExpr([], _typ, None) -> Expression.dict (), []
        | Fable.ObjectExpr(members, typ, baseCall) ->
            // printfn "members: %A" (members, typ)
            transformObjectExpr com ctx members typ baseCall

        | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "has" }, _, _),
                     info,
                     _,
                     _range) ->
            let left, stmts = com.TransformAsExpr(ctx, info.Args.Head)
            let value, stmts' = com.TransformAsExpr(ctx, expr)

            Expression.compare (left, [ ComparisonOperator.In ], [ value ]),
            stmts @ stmts'

        | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "slice" }, _, _),
                     info,
                     _,
                     _range) -> transformAsSlice com ctx expr info

        | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "to_array" }, _, _),
                     info,
                     _,
                     _range) -> transformAsArray com ctx expr info

        | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "Equals" }, _, _),
                     { Args = [ arg ] },
                     _,
                     _range) ->
            let right, stmts = com.TransformAsExpr(ctx, arg)
            let left, stmts' = com.TransformAsExpr(ctx, expr)
            Expression.compare (left, [ Eq ], [ right ]), stmts @ stmts'

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.CurriedApply(callee, args, _, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, tags, _, range) ->
            transformOperation com ctx range kind tags

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(TransformExpr com ctx (guardExpr, stmts),
                           TransformExpr com ctx (thenExpr, stmts'),
                           TransformExpr com ctx (elseExpr, stmts''),
                           _r) ->
            Expression.ifExp (guardExpr, thenExpr, elseExpr),
            stmts @ stmts' @ stmts''

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            let expr', stmts = transformSet com ctx range expr typ value kind
            // printfn "transformAsExpr: Fable.Set: %A" expr
            match expr' with
            | Expression.NamedExpr {
                                       Target = target
                                       Value = _
                                       Loc = _
                                   } ->
                let nonLocals =
                    match target with
                    | Expression.Name { Id = id } ->
                        [
                            ctx.BoundVars.NonLocals([ id ])
                            |> Statement.nonLocal
                        ]
                    | _ -> []

                expr', nonLocals @ stmts
            | _ -> expr', stmts

        | Fable.Let(_ident, _value, _body) ->
            // printfn "Fable.Let: %A" (ident, value, body)
            iife com ctx expr

        | Fable.LetRec(bindings, body) ->
            if ctx.HoistVars(List.map fst bindings) then
                let values, stmts =
                    bindings
                    |> List.map (fun (id, value) ->
                        transformBindingAsExpr com ctx id value
                    )
                    |> List.unzip
                    |> (fun (e, s) -> (e, List.collect id s))

                let expr, stmts' = com.TransformAsExpr(ctx, body)

                let expr, stmts'' =
                    transformSequenceExpr' com ctx (values @ [ expr ]) []

                expr, stmts @ stmts' @ stmts''
            else
                iife com ctx expr

        | Fable.Sequential exprs -> transformSequenceExpr com ctx exprs

        | Fable.Emit(info, _, range) ->
            if info.IsStatement then
                iife com ctx expr
            else
                transformEmit com ctx range info

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.WhileLoop _
        | Fable.ForLoop _
        | Fable.TryCatch _ -> iife com ctx expr
        | Fable.Extended(instruction, _) ->
            match instruction with
            | Fable.Curry(e, arity) -> transformCurry com ctx e arity
            | Fable.Throw _
            | Fable.Debugger -> iife com ctx expr

    let transformAsSlice
        (com: IPythonCompiler)
        ctx
        expr
        (info: Fable.CallInfo)
        : Expression * Statement list
        =
        let left, stmts = com.TransformAsExpr(ctx, expr)

        let args, stmts' =
            info.Args
            |> List.map (fun arg -> com.TransformAsExpr(ctx, arg))
            |> List.unzip
            |> (fun (e, s) -> (e, List.collect id s))

        let slice =
            match args with
            | [] -> Expression.slice ()
            | [ lower ] -> Expression.slice (lower = lower)
            | [ Expression.Name { Id = Identifier "None" }; upper ] ->
                Expression.slice (upper = upper)
            | [ lower; upper ] ->
                Expression.slice (lower = lower, upper = upper)
            | _ -> failwith $"Array slice with {args.Length} not supported"

        Expression.subscript (left, slice), stmts @ stmts'

    let transformAsArray
        (com: IPythonCompiler)
        ctx
        expr
        (info: Fable.CallInfo)
        : Expression * Statement list
        =
        let value, stmts = com.TransformAsExpr(ctx, expr)

        match expr.Type with
        | Fable.Type.Array(typ, Fable.ArrayKind.ResizeArray) ->
            let letter =
                match typ with
                | Fable.Type.Number(UInt8, _) -> Some "B"
                | Fable.Type.Number(Int8, _) -> Some "b"
                | Fable.Type.Number(Int16, _) -> Some "h"
                | Fable.Type.Number(UInt16, _) -> Some "H"
                | Fable.Type.Number(Int32, _) -> Some "l"
                | Fable.Type.Number(UInt32, _) -> Some "L"
                | Fable.Type.Number(Int64, _) -> Some "q"
                | Fable.Type.Number(UInt64, _) -> Some "Q"
                | Fable.Type.Number(Float32, _) -> Some "f"
                | Fable.Type.Number(Float64, _) -> Some "d"
                | _ -> None

            match letter with
            | Some "B" ->
                let bytearray = Expression.name "bytearray"
                Expression.call (bytearray, [ value ]), stmts
            | Some l ->
                let array = com.GetImportExpr(ctx, "array", "array")

                Expression.call (
                    array,
                    Expression.stringConstant l :: [ value ]
                ),
                stmts
            | _ -> transformAsSlice com ctx expr info
        | _ -> transformAsSlice com ctx expr info

    let rec transformAsStatements
        (com: IPythonCompiler)
        ctx
        returnStrategy
        (expr: Fable.Expr)
        : Statement list
        =
        // printfn "transformAsStatements: %A" expr
        match expr with
        | Fable.Unresolved(_, _, r) ->
            addError com [] r "Unexpected unresolved expression"
            []

        | Fable.Extended(kind, _r) ->
            match kind with
            | Fable.Curry(e, arity) ->
                let expr, stmts = transformCurry com ctx e arity

                stmts @ (expr |> resolveExpr ctx e.Type returnStrategy)
            | Fable.Throw(expr, _) ->
                match expr with
                | None -> failwith "TODO: rethrow"
                | Some(TransformExpr com ctx (e, stmts)) ->
                    stmts @ [ Statement.raise e ]
            | Fable.Debugger -> []

        | Fable.TypeCast(e, t) ->
            let expr, stmts = transformCast com ctx t e
            stmts @ (expr |> resolveExpr ctx t returnStrategy)

        | Fable.Value(kind, r) ->
            let expr, stmts = transformValue com ctx r kind

            stmts @ (expr |> resolveExpr ctx kind.Type returnStrategy)

        | Fable.IdentExpr id ->
            identAsExpr com ctx id |> resolveExpr ctx id.Type returnStrategy

        | Fable.Import({
                           Selector = selector
                           Path = path
                           Kind = _kind
                       },
                       t,
                       r) ->
            transformImport com ctx r selector path
            |> resolveExpr ctx t returnStrategy

        | Fable.Test(expr, kind, range) ->
            let expr, stmts = transformTest com ctx range kind expr

            stmts @ (expr |> resolveExpr ctx Fable.Boolean returnStrategy)

        | Fable.Lambda(arg, body, name) ->
            let expr', stmts =
                transformFunctionWithAnnotations com ctx name [ arg ] body
                |||> makeArrowFunctionExpression com ctx name

            stmts @ (expr' |> resolveExpr ctx expr.Type returnStrategy)

        | Fable.Delegate(args, body, name, _) ->
            let expr', stmts =
                transformFunctionWithAnnotations com ctx name args body
                |||> makeArrowFunctionExpression com ctx name

            stmts @ (expr' |> resolveExpr ctx expr.Type returnStrategy)

        | Fable.ObjectExpr([], _, None) -> [] // Remove empty object expression
        | Fable.ObjectExpr(members, t, baseCall) ->
            let expr, stmts = transformObjectExpr com ctx members t baseCall
            stmts @ (expr |> resolveExpr ctx t returnStrategy)

        | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "slice" }, _, _),
                     info,
                     typ,
                     _range) ->
            let expr, stmts = transformAsSlice com ctx expr info
            stmts @ resolveExpr ctx typ returnStrategy expr
        | Fable.Call(Fable.Get(expr, Fable.FieldGet { Name = "to_array" }, _, _),
                     info,
                     typ,
                     _range) ->
            let expr, stmts = transformAsArray com ctx expr info
            stmts @ resolveExpr ctx typ returnStrategy expr
        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements
                com
                ctx
                range
                typ
                returnStrategy
                callee
                info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApplyAsStatements
                com
                ctx
                range
                typ
                returnStrategy
                callee
                args

        | Fable.Emit(info, t, range) ->
            let e, stmts = transformEmit com ctx range info

            if info.IsStatement then
                stmts @ [ Statement.expr e ] // Ignore the return strategy
            else
                stmts @ resolveExpr ctx t returnStrategy e

        | Fable.Operation(kind, tags, t, range) ->
            let expr, stmts = transformOperation com ctx range kind tags
            stmts @ (expr |> resolveExpr ctx t returnStrategy)

        | Fable.Get(expr, kind, t, range) ->
            let expr, stmts = transformGet com ctx range t expr kind
            stmts @ (expr |> resolveExpr ctx t returnStrategy)

        | Fable.Let(ident, value, body) ->
            match ident, value, body with
            // Transform F# `use` i.e TryCatch as Python `with`
            | { Name = valueName },
              value,
              Fable.TryCatch(body,
                             None,
                             Some(Fable.IfThenElse(_,
                                                   Fable.Call(Fable.Get(Fable.TypeCast(Fable.IdentExpr {
                                                                                                           Name = disposeName
                                                                                                       },
                                                                                       _),
                                                                        Fable.FieldGet {
                                                                                           Name = "Dispose"
                                                                                       },
                                                                        _t,
                                                                        _),
                                                              _,
                                                              _,
                                                              _),
                                                   _elseExpr,
                                                   _)),
                             _) when valueName = disposeName ->
                let id = Identifier valueName

                let body =
                    com.TransformAsStatements(ctx, Some ResourceManager, body)
                    |> List.choose Helpers.isProductiveStatement

                let value, stmts = com.TransformAsExpr(ctx, value)
                let items = [ WithItem.withItem (value, Expression.name id) ]
                stmts @ [ Statement.with' (items, body) ]
            | _ ->
                let binding = transformBindingAsStatements com ctx ident value

                List.append
                    binding
                    (transformAsStatements com ctx returnStrategy body)

        | Fable.LetRec(bindings, body) ->
            let bindings =
                bindings
                |> Seq.collect (fun (i, v) ->
                    transformBindingAsStatements com ctx i v
                )
                |> Seq.toList

            List.append
                bindings
                (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, kind, typ, value, range) ->
            let expr', stmts = transformSet com ctx range expr typ value kind
            // printfn "transformAsStatements: Fable.Set: %A" (expr', value)
            match expr' with
            | Expression.NamedExpr({
                                       Target = target
                                       Value = value
                                       Loc = _
                                   }) ->
                let nonLocals, ta =
                    match target with
                    | Expression.Name { Id = id } ->
                        let nonLocals =
                            [
                                ctx.BoundVars.NonLocals([ id ])
                                |> Statement.nonLocal
                            ]

                        nonLocals, None
                    | Expression.Attribute {
                                               Value = Expression.Name {
                                                                           Id = Identifier "self"
                                                                       }
                                           } ->
                        let ta, stmts = typeAnnotation com ctx None typ
                        stmts, Some ta
                    | _ -> [], None

                let assignment =
                    match ta with
                    | Some ta -> [ Statement.assign (target, ta, value) ]
                    | _ -> [ Statement.assign ([ target ], value) ]

                nonLocals @ stmts @ assignment
            | _ -> stmts @ (expr' |> resolveExpr ctx expr.Type returnStrategy)

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            let asStatement =
                match returnStrategy with
                | None
                | Some ReturnUnit -> true
                | Some(Target _) -> true // Compile as statement so values can be bound
                | Some(Assign _) ->
                    (isPyStatement ctx false thenExpr)
                    || (isPyStatement ctx false elseExpr)
                | Some ResourceManager
                | Some Return ->
                    Option.isSome ctx.TailCallOpportunity
                    || (isPyStatement ctx false thenExpr)
                    || (isPyStatement ctx false elseExpr)

            if asStatement then
                transformIfStatement
                    com
                    ctx
                    r
                    returnStrategy
                    guardExpr
                    thenExpr
                    elseExpr
            else
                let guardExpr', stmts = transformAsExpr com ctx guardExpr
                let thenExpr', stmts' = transformAsExpr com ctx thenExpr
                let elseExpr', stmts'' = transformAsExpr com ctx elseExpr

                stmts
                @ stmts'
                @ stmts''
                @ (Expression.ifExp (guardExpr', thenExpr', elseExpr', ?loc = r)
                   |> resolveExpr ctx thenExpr.Type returnStrategy)

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1

            statements
            |> List.mapiToArray (fun i statement ->
                let ret =
                    if i < lasti then
                        None
                    else
                        returnStrategy

                com.TransformAsStatements(ctx, ret, statement)
            )
            |> List.concat

        | Fable.TryCatch(body, catch, finalizer, r) ->
            transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsStatements
                com
                ctx
                returnStrategy
                targets
                expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsStatements
                com
                ctx
                returnStrategy
                idx
                boundValues

        | Fable.WhileLoop(TransformExpr com ctx (guard, stmts), body, range) ->
            stmts
            @ [
                Statement.while' (
                    guard,
                    transformBlock com ctx None body,
                    ?loc = range
                )
            ]

        | Fable.ForLoop(var,
                        TransformExpr com ctx (start, _stmts),
                        TransformExpr com ctx (limit, _stmts'),
                        body,
                        isUp,
                        _range) ->
            let limit, step =
                if isUp then
                    let limit =
                        Expression.binOp (limit, Add, Expression.intConstant 1) // Python `range` has exclusive end.

                    limit, 1
                else
                    let limit =
                        Expression.binOp (limit, Sub, Expression.intConstant 1) // Python `range` has exclusive end.

                    limit, -1

            let step = Expression.intConstant step

            let iter =
                Expression.call (
                    Expression.name (Identifier "range"),
                    args =
                        [
                            start
                            limit
                            step
                        ]
                )

            let body = transformBlock com ctx None body
            let target = com.GetIdentifierAsExpr(ctx, var.Name)

            [ Statement.for' (target = target, iter = iter, body = body) ]

    let transformFunction
        com
        ctx
        name
        (args: Fable.Ident list)
        (body: Fable.Expr)
        (repeatedGenerics: Set<string>)
        : Arguments * Statement list
        =
        let tailcallChance =
            Option.map
                (fun name ->
                    NamedTailCallOpportunity(com, ctx, name, args)
                    :> ITailCallOpportunity
                )
                name

        let args = FSharp2Fable.Util.discardUnitArg args

        /// Removes `_mut` or `_mut_1` suffix from the identifier name
        let cleanName (input: string) =
            Regex.Replace(input, @"_mut(_\d+)?$", "")

        // For Python we need to append the TC-arguments to any declared (arrow) function inside the while-loop of the
        // TCO. We will set them as default values to themselves e.g `i=i` to capture the value and not the variable.
        let tcArgs, tcDefaults =
            match ctx.TailCallOpportunity with
            | Some tc ->
                tc.Args
                |> List.choose (fun arg ->
                    let (Identifier name) = arg.Arg
                    let name = cleanName name

                    match name with
                    | "tupled_arg_m" -> None // Remove these arguments (not sure why)
                    | _ ->
                        let annotation =
                            // Cleanup type annotations to avoid non-repeated generics
                            match arg.Annotation with
                            | Some(Expression.Name { Id = Identifier _name }) ->
                                arg.Annotation
                            | Some(Expression.Subscript {
                                                            Value = value
                                                            Slice = Expression.Name {
                                                                                        Id = Identifier name
                                                                                    }
                                                        }) when
                                name.StartsWith("_", StringComparison.Ordinal)
                                ->
                                Expression.subscript (
                                    value,
                                    stdlibModuleAnnotation
                                        com
                                        ctx
                                        "typing"
                                        "Any"
                                        []
                                )
                                |> Some
                            | _ ->
                                Some(
                                    stdlibModuleAnnotation
                                        com
                                        ctx
                                        "typing"
                                        "Any"
                                        []
                                )

                        (Arg.arg (name, ?annotation = annotation),
                         Expression.name name)
                        |> Some
                )
                |> List.unzip
            | _ -> [], []

        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false

        let argTypes = args |> List.map (fun id -> id.Type)
        let genTypeParams = Util.getGenericTypeParams (argTypes @ [ body.Type ])
        let newTypeParams = Set.difference genTypeParams ctx.ScopedTypeParams

        let ctx =
            { ctx with
                TailCallOpportunity = tailcallChance
                HoistVars =
                    fun ids ->
                        declaredVars.AddRange(ids)
                        true
                OptimizeTailCall = fun () -> isTailCallOptimized <- true
                BoundVars = ctx.BoundVars.EnterScope()
                ScopedTypeParams = Set.union ctx.ScopedTypeParams newTypeParams
            }

        let body =
            if body.Type = Fable.Unit then
                transformBlock com ctx (Some ReturnUnit) body
            elif isPyStatement ctx (Option.isSome tailcallChance) body then
                transformBlock com ctx (Some Return) body
            else
                transformAsExpr com ctx body |> wrapExprInBlockWithReturn

        let isUnit =
            List.tryLast args
            |> Option.map (
                function
                | { Type = Fable.GenericParam _ } -> true
                | _ -> false
            )
            |> Option.defaultValue false

        let args, defaults, body =
            match isTailCallOptimized, tailcallChance with
            | true, Some tc ->
                // Replace args, see NamedTailCallOpportunity constructor
                let args' =
                    List.zip args tc.Args
                    |> List.map (fun (_id, { Arg = Identifier tcArg }) ->
                        let id = com.GetIdentifier(ctx, tcArg)

                        let ta, _ =
                            typeAnnotation
                                com
                                ctx
                                (Some repeatedGenerics)
                                _id.Type

                        Arg.arg (id, annotation = ta)
                    )

                let varDecls =
                    List.zip args tc.Args
                    |> List.map (fun (id, { Arg = Identifier tcArg }) ->
                        ident com ctx id,
                        Some(com.GetIdentifierAsExpr(ctx, tcArg))
                    )
                    |> multiVarDeclaration ctx

                let body = varDecls @ body
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body = body @ [ Statement.break' () ]

                args',
                [],
                Statement.while' (Expression.boolConstant true, body)
                |> List.singleton
            | _ ->
                // Make sure all of the last optional arguments will accept None as their default value
                let defaults =
                    args
                    |> List.rev
                    |> List.takeWhile (fun arg ->
                        match arg.Type with
                        | Fable.Any
                        | Fable.Option _ -> true
                        | _ -> false
                    )
                    |> List.map (fun _ -> Expression.none)

                let args' =
                    args
                    |> List.map (fun id ->
                        let ta, _ =
                            typeAnnotation
                                com
                                ctx
                                (Some repeatedGenerics)
                                id.Type

                        Arg.arg (ident com ctx id, annotation = ta)
                    )

                args', defaults, body

        let arguments =
            match args, isUnit with
            | [], _ ->
                Arguments.arguments (
                    args =
                        Arg.arg (
                            Identifier("__unit"),
                            annotation = Expression.name "None"
                        )
                        :: tcArgs,
                    defaults = Expression.none :: tcDefaults
                )
            // So we can also receive unit
            | [ arg ], true ->
                let optional =
                    match arg.Annotation with
                    | Some typeArg ->
                        Expression.binOp (
                            typeArg,
                            BitOr,
                            Expression.name "None"
                        )
                        |> Some
                    | None -> None

                let args = [ { arg with Annotation = optional } ]

                Arguments.arguments (
                    args @ tcArgs,
                    defaults = Expression.none :: tcDefaults
                )
            | _ ->
                Arguments.arguments (
                    args @ tcArgs,
                    defaults = defaults @ tcDefaults
                )

        arguments, body

    // Declares a Python entry point, i.e `if __name__ == "__main__"`
    let declareEntryPoint
        (com: IPythonCompiler)
        (ctx: Context)
        (funcExpr: Expression)
        =
        com.GetImportExpr(ctx, "sys") |> ignore
        let args = emitExpression None "sys.argv[1:]" []

        let test =
            Expression.compare (
                Expression.name "__name__",
                [ ComparisonOperator.Eq ],
                [ Expression.stringConstant "__main__" ]
            )

        let main =
            Expression.call (funcExpr, [ args ])
            |> Statement.expr
            |> List.singleton

        Statement.if' (test, main)

    let declareModuleMember
        (com: IPythonCompiler)
        ctx
        _isPublic
        (membName: Identifier)
        typ
        (expr: Expression)
        =
        let (Identifier name) = membName

        if com.OutputType = OutputType.Library then
            com.AddExport name |> ignore

        let name = Expression.name membName
        varDeclaration ctx name typ expr

    let makeEntityTypeParamDecl (com: IPythonCompiler) ctx (ent: Fable.Entity) =
        getEntityGenParams ent |> makeTypeParamDecl com ctx

    let getUnionFieldsAsIdents
        (_com: IPythonCompiler)
        _ctx
        (_ent: Fable.Entity)
        =
        let tagId =
            makeTypedIdent (Fable.Number(Int32, Fable.NumberInfo.Empty)) "tag"

        let fieldsId =
            makeTypedIdent (Fable.Array(Fable.Any, Fable.MutableArray)) "fields"

        [|
            tagId
            fieldsId
        |]

    let getEntityFieldsAsIdents _com (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name =
                (Naming.toSnakeCase field.Name, Naming.NoMemberPart)
                ||> Naming.sanitizeIdent Naming.pyBuiltins.Contains

            let typ = field.FieldType

            { makeTypedIdent typ name with IsMutable = field.IsMutable }
        )
        |> Seq.toArray

    let getEntityFieldsAsProps (com: IPythonCompiler) ctx (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            getUnionFieldsAsIdents com ctx ent
            |> Array.map (identAsExpr com ctx)
        else
            ent.FSharpFields
            |> Seq.map (fun field ->
                let prop = memberFromName com ctx field.Name
                prop
            )
            |> Seq.toArray

    let declareDataClassType
        (com: IPythonCompiler)
        (ctx: Context)
        (ent: Fable.Entity)
        (entName: string)
        (consArgs: Arguments)
        (_isOptional: bool)
        (_consBody: Statement list)
        (baseExpr: Expression option)
        (classMembers: Statement list)
        _slotMembers
        =
        let name = com.GetIdentifier(ctx, entName)

        let props =
            consArgs.Args
            |> List.map (fun arg ->
                let any _ =
                    stdlibModuleAnnotation com ctx "typing" "Any" []

                let annotation = arg.Annotation |> Option.defaultWith any

                Statement.assign (
                    Expression.name arg.Arg,
                    annotation = annotation
                )
            )

        let generics = makeEntityTypeParamDecl com ctx ent
        let bases = baseExpr |> Option.toList

        let classBody =
            let body =
                [
                    yield! props
                    yield! classMembers
                ]

            match body with
            | [] -> [ Statement.ellipsis ]
            | _ -> body

        let dataClass = com.GetImportExpr(ctx, "dataclasses", "dataclass")

        let decorators =
            [
                Expression.call (
                    dataClass,
                    kw =
                        [
                            Keyword.keyword (
                                Identifier "eq",
                                Expression.boolConstant false
                            )
                            Keyword.keyword (
                                Identifier "repr",
                                Expression.boolConstant false
                            )
                            Keyword.keyword (
                                Identifier "slots",
                                Expression.boolConstant true
                            )
                        ]
                )
            ]

        [
            Statement.classDef (
                name,
                body = classBody,
                decoratorList = decorators,
                bases = bases @ generics
            )
        ]

    let declareClassType
        (com: IPythonCompiler)
        (ctx: Context)
        (ent: Fable.Entity)
        (entName: string)
        (consArgs: Arguments)
        (isOptional: bool)
        (consBody: Statement list)
        (baseExpr: Expression option)
        (classMembers: Statement list)
        slotMembers
        =
        // printfn "declareClassType: %A" consBody
        let generics = makeEntityTypeParamDecl com ctx ent
        let classCons = makeClassConstructor consArgs isOptional consBody

        let classFields = slotMembers // TODO: annotations
        let classMembers = classCons @ classMembers
        //printfn "ClassMembers: %A" classMembers
        let classBody =
            let body =
                [
                    yield! classFields
                    yield! classMembers
                ]

            match body with
            | [] -> [ Statement.ellipsis ]
            | _ -> body


        let interfaces, stmts =
            // We only use a few interfaces as base classes. The rest is handled as Python protocols (PEP 544) to avoid a massive
            // inheritance tree that will prevent Python of finding a consistent method resolution order.
            let allowedInterfaces = [ "IDisposable" ]

            ent.AllInterfaces
            |> List.ofSeq
            |> List.filter (fun int ->
                let name = Helpers.removeNamespace (int.Entity.FullName)
                allowedInterfaces |> List.contains name
            )
            |> List.map (fun int ->
                let genericArgs =
                    match int.GenericArgs with
                    | [ Fable.DeclaredType({ FullName = fullName },
                                           _genericArgs) ] when
                        Helpers.removeNamespace (fullName) = entName
                        ->
                        [ Fable.Type.Any ]
                    | args -> args

                let expr, stmts =
                    makeEntityTypeAnnotation
                        com
                        ctx
                        int.Entity
                        genericArgs
                        None

                expr, stmts
            )
            |> Helpers.unzipArgs

        // printfn "infterfaces: %A" interfaces

        let bases = baseExpr |> Option.toList

        let name = com.GetIdentifier(ctx, entName)

        stmts
        @ [
            Statement.classDef (
                name,
                body = classBody,
                bases = bases @ interfaces @ generics
            )
        ]

    let createSlotsForRecordType
        (com: IPythonCompiler)
        ctx
        (classEnt: Fable.Entity)
        =
        let strFromIdent (ident: Identifier) = ident.Name

        if classEnt.IsValueType then
            let elements =
                getEntityFieldsAsProps com ctx classEnt
                |> Array.map (
                    nameFromKey com ctx
                    >> strFromIdent
                    >> Expression.stringConstant
                )
                |> Array.toList

            let slots = Expression.list (elements, Load)

            [
                Statement.assign (
                    [ Expression.name ("__slots__", Store) ],
                    slots
                )
            ]
        else
            []

    let declareType
        (com: IPythonCompiler)
        (ctx: Context)
        (ent: Fable.Entity)
        (entName: string)
        (consArgs: Arguments)
        (isOptional: bool)
        (consBody: Statement list)
        (baseExpr: Expression option)
        (classMembers: Statement list)
        : Statement list
        =
        let slotMembers = createSlotsForRecordType com ctx ent

        let typeDeclaration =
            match ent.IsFSharpRecord with
            | true ->
                declareDataClassType
                    com
                    ctx
                    ent
                    entName
                    consArgs
                    isOptional
                    consBody
                    baseExpr
                    classMembers
                    slotMembers
            | false ->
                declareClassType
                    com
                    ctx
                    ent
                    entName
                    consArgs
                    isOptional
                    consBody
                    baseExpr
                    classMembers
                    slotMembers

        let reflectionDeclaration, stmts =
            let ta = fableModuleAnnotation com ctx "Reflection" "TypeInfo" []

            let genArgs =
                Array.init
                    ent.GenericParameters.Length
                    (fun i -> "gen" + string<int> i |> makeIdent)

            let args =
                genArgs
                |> Array.mapToList (fun id ->
                    Arg.arg (ident com ctx id, annotation = ta)
                )

            let args = Arguments.arguments args
            let generics = genArgs |> Array.mapToList (identAsExpr com ctx)

            let body, stmts = transformReflectionInfo com ctx None ent generics

            let expr, stmts' =
                makeFunctionExpression com ctx None (args, body, [], ta)

            let name = com.GetIdentifier(ctx, entName + Naming.reflectionSuffix)

            expr |> declareModuleMember com ctx ent.IsPublic name None,
            stmts @ stmts'

        stmts @ typeDeclaration @ reflectionDeclaration

    let transformModuleFunction
        (com: IPythonCompiler)
        ctx
        (info: Fable.MemberFunctionOrValue)
        (membName: string)
        args
        body
        =
        let args, body', returnType =
            getMemberArgsAndBody
                com
                ctx
                (NonAttached membName)
                info.HasSpread
                args
                body

        let name = com.GetIdentifier(ctx, membName)
        let stmt = createFunction name args body' [] returnType
        let expr = Expression.name name

        info.Attributes
        |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)
        |> function
            | true ->
                [
                    stmt
                    declareEntryPoint com ctx expr
                ]
            | false ->
                if com.OutputType = OutputType.Library then
                    com.AddExport membName |> ignore

                [ stmt ]

    let transformAction (com: IPythonCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        // let hasVarDeclarations =
        //     statements |> List.exists (function
        //         | Declaration(_) -> true
        //         | _ -> false)
        // if hasVarDeclarations then
        //     [ Expression.call(Expression.functionExpression([||], BlockStatement(statements)), [||])
        //       |> Statement.expr |> PrivateModuleDeclaration ]
        //else
        statements

    let nameFromKey (com: IPythonCompiler) (ctx: Context) key =
        match key with
        | Expression.Name { Id = ident } -> ident
        | Expression.Constant(value = StringLiteral name) ->
            com.GetIdentifier(ctx, name)
        | name -> failwith $"Not a valid name: {name}"

    let transformAttachedProperty
        (com: IPythonCompiler)
        ctx
        (info: Fable.MemberFunctionOrValue)
        (memb: Fable.MemberDecl)
        =
        let isStatic = not info.IsInstance
        let isGetter = info.IsGetter

        let decorators =
            [
                if isStatic then
                    Expression.name "staticmethod"
                elif isGetter then
                    Expression.name "property"
                else
                    Expression.name $"{memb.Name}.setter"
            ]

        let args, body, returnType =
            getMemberArgsAndBody
                com
                ctx
                (Attached isStatic)
                false
                memb.Args
                memb.Body

        let key = memberFromName com ctx memb.Name |> nameFromKey com ctx

        let arguments =
            if isStatic then
                { args with Args = [] }
            else
                let self = Arg.arg "self"
                { args with Args = self :: args.Args }

        // Python do not support static getters, so make it a function instead
        Statement.functionDef (
            key,
            arguments,
            body = body,
            decoratorList = decorators,
            returns = returnType
        )
        |> List.singleton

    let transformAttachedMethod
        (com: IPythonCompiler)
        ctx
        (info: Fable.MemberFunctionOrValue)
        (memb: Fable.MemberDecl)
        =
        // printfn "transformAttachedMethod: %A" memb

        let isStatic = not info.IsInstance

        let decorators =
            if isStatic then
                [ Expression.name "staticmethod" ]
            else
                []

        let makeMethod name args body decorators returnType =
            let key = memberFromName com ctx name |> nameFromKey com ctx

            Statement.functionDef (
                key,
                args,
                body = body,
                decoratorList = decorators,
                returns = returnType
            )

        let args, body, returnType =
            getMemberArgsAndBody
                com
                ctx
                (Attached isStatic)
                info.HasSpread
                memb.Args
                memb.Body

        let self = Arg.arg "self"

        let arguments =
            if isStatic then
                args
            else
                { args with Args = self :: args.Args }

        [
            yield makeMethod memb.Name arguments body decorators returnType
            if
                info.FullName = "System.Collections.Generic.IEnumerable.GetEnumerator"
            then
                yield
                    makeMethod
                        "__iter__"
                        (Arguments.arguments [ self ])
                        (enumerator2iterator com ctx)
                        decorators
                        returnType
        ]

    let transformUnion
        (com: IPythonCompiler)
        ctx
        (ent: Fable.Entity)
        (entName: string)
        classMembers
        =
        let fieldIds = getUnionFieldsAsIdents com ctx ent

        let args, isOptional =
            let args =
                fieldIds[0]
                |> ident com ctx
                |> (fun id ->
                    let ta, _ = typeAnnotation com ctx None fieldIds[0].Type
                    Arg.arg (id, annotation = ta)
                )
                |> List.singleton

            let varargs =
                fieldIds[1]
                |> ident com ctx
                |> (fun id ->
                    let gen =
                        getGenericTypeParams [ fieldIds[1].Type ]
                        |> Set.toList
                        |> List.tryHead

                    let ta = Expression.name (gen |> Option.defaultValue "Any")
                    Arg.arg (id, annotation = ta)
                )

            let isOptional = Helpers.isOptional fieldIds
            Arguments.arguments (args = args, vararg = varargs), isOptional

        let body =
            [
                yield callSuperAsStatement []
                yield!
                    fieldIds
                    |> Array.map (fun id ->
                        let left = get com ctx None thisExpr id.Name false

                        let right =
                            match id.Type with
                            | Fable.Number _ ->
                                Expression.boolOp (
                                    BoolOperator.Or,
                                    [
                                        identAsExpr com ctx id
                                        Expression.intConstant 0
                                    ]
                                )
                            | Fable.Array _ ->
                                // Convert varArg from tuple to list. TODO: we might need to do this other places as well.
                                Expression.call (
                                    Expression.name "list",
                                    [ identAsExpr com ctx id ]
                                )
                            | _ -> identAsExpr com ctx id

                        let ta, _ = typeAnnotation com ctx None id.Type
                        Statement.assign (left, ta, right)
                    )
            ]

        let cases =
            let expr, stmts =
                ent.UnionCases
                |> Seq.map (getUnionCaseName >> makeStrConst)
                |> Seq.toList
                |> makeList com ctx

            let name = Identifier("cases")
            let body = stmts @ [ Statement.return' expr ]
            let decorators = [ Expression.name "staticmethod" ]

            let returnType =
                Expression.subscript (
                    Expression.name "list",
                    Expression.name "str"
                )

            Statement.functionDef (
                name,
                Arguments.arguments (),
                body = body,
                returns = returnType,
                decoratorList = decorators
            )

        let baseExpr = libValue com ctx "types" "Union" |> Some
        let classMembers = List.append [ cases ] classMembers

        declareType
            com
            ctx
            ent
            entName
            args
            isOptional
            body
            baseExpr
            classMembers

    let transformClassWithCompilerGeneratedConstructor
        (com: IPythonCompiler)
        ctx
        (ent: Fable.Entity)
        (entName: string)
        classMembers
        =
        // printfn "transformClassWithCompilerGeneratedConstructor"
        let fieldIds = getEntityFieldsAsIdents com ent

        let args =
            fieldIds
            |> Array.map (fun id ->
                com.GetIdentifier(ctx, id.Name) |> Expression.name
            )

        let isOptional =
            Helpers.isOptional fieldIds || ent.IsFSharpRecord || ent.IsValueType

        let baseExpr =
            if ent.IsFSharpExceptionDeclaration then
                libValue com ctx "types" "FSharpException" |> Some
            elif ent.IsFSharpRecord || ent.IsValueType then
                libValue com ctx "types" "Record" |> Some
            else
                None

        let body =
            [
                if Option.isSome baseExpr then
                    yield callSuperAsStatement []

                yield!
                    (ent.FSharpFields
                     |> List.collecti (fun i field ->
                         let left =
                             get
                                 com
                                 ctx
                                 None
                                 thisExpr
                                 (Naming.toSnakeCase field.Name)
                                 false

                         let right =
                             args[i] |> wrapIntExpression field.FieldType

                         assign None left right |> exprAsStatement ctx
                     ))
            ]

        let args =
            fieldIds
            |> Array.mapToList (fun id ->
                let ta, _ = typeAnnotation com ctx None id.Type
                Arg.arg (ident com ctx id, annotation = ta)
            )
            |> (fun args -> Arguments.arguments (args = args))

        declareType
            com
            ctx
            ent
            entName
            args
            isOptional
            body
            baseExpr
            classMembers

    let transformClassWithPrimaryConstructor
        (com: IPythonCompiler)
        ctx
        (classDecl: Fable.ClassDecl)
        (classMembers: Statement list)
        (cons: Fable.MemberDecl)
        =
        // printfn "transformClassWithPrimaryConstructor: %A" classDecl
        let classEnt = com.GetEntity(classDecl.Entity)

        let classIdent =
            Expression.name (com.GetIdentifier(ctx, classDecl.Name))

        let consArgs, consBody, _returnType =
            let info = com.GetMember(cons.MemberRef)

            getMemberArgsAndBody
                com
                ctx
                ClassConstructor
                info.HasSpread
                cons.Args
                cons.Body

        let isOptional = Helpers.isOptional (cons.Args |> Array.ofList)

        // Change exposed constructor's return type from None to entity type.
        let returnType =
            let availableGenerics =
                cons.Args
                |> List.map (fun arg -> arg.Type)
                |> getGenericTypeParams

            let genParams = getEntityGenParams classEnt

            makeGenericTypeAnnotation'
                com
                ctx
                classDecl.Name
                (genParams |> List.ofSeq)
                (Some availableGenerics)

        let exposedCons =
            let argExprs =
                consArgs.Args |> List.map (fun p -> Expression.identifier p.Arg)

            let exposedConsBody = Expression.call (classIdent, argExprs)
            let name = com.GetIdentifier(ctx, cons.Name)
            makeFunction name (consArgs, exposedConsBody, [], returnType)

        let baseExpr, consBody =
            classDecl.BaseCall
            |> extractBaseExprFromBaseCall com ctx classEnt.BaseType
            |> Option.orElseWith (fun () ->
                if classEnt.IsValueType then
                    Some(libValue com ctx "Types" "Record", ([], [], []))
                else
                    None
            )
            |> Option.map (fun (baseExpr, (baseArgs, _kw, stmts)) ->
                let consBody =
                    stmts @ [ callSuperAsStatement baseArgs ] @ consBody

                Some baseExpr, consBody
            )
            |> Option.defaultValue (None, consBody)

        [
            yield!
                declareType
                    com
                    ctx
                    classEnt
                    classDecl.Name
                    consArgs
                    isOptional
                    consBody
                    baseExpr
                    classMembers
            exposedCons
        ]

    let transformInterface
        (com: IPythonCompiler)
        ctx
        (classEnt: Fable.Entity)
        (_classDecl: Fable.ClassDecl)
        =
        // printfn "transformInterface"
        let classIdent =
            com.GetIdentifier(ctx, Helpers.removeNamespace classEnt.FullName)

        let members =
            classEnt.MembersFunctionsAndValues
            |> Seq.filter (fun memb -> not memb.IsProperty)
            |> List.ofSeq
            |> List.groupBy (fun memb -> memb.DisplayName)
            // Remove duplicate method when we have getters and setters
            |> List.collect (fun (_, gr) ->
                gr
                |> List.filter (fun memb ->
                    gr.Length = 1 || (memb.IsGetter || memb.IsSetter)
                )
            )

        let classMembers =
            [
                for memb in members do
                    let name =
                        memb.DisplayName |> Naming.toSnakeCase |> Helpers.clean

                    let abstractMethod =
                        com.GetImportExpr(ctx, "abc", "abstractmethod")

                    let decorators =
                        [
                            if memb.IsValue || memb.IsGetter then
                                Expression.name "property"
                            if memb.IsSetter then
                                Expression.name $"{name}.setter"

                            abstractMethod
                        ] // Must be after @property

                    let name = com.GetIdentifier(ctx, name)

                    let args =
                        let args =
                            [
                                if memb.IsInstance then
                                    Arg.arg "self"
                                for n, parameterGroup in
                                    memb.CurriedParameterGroups |> Seq.indexed do
                                    for m, pg in parameterGroup |> Seq.indexed do
                                        let ta, _ =
                                            typeAnnotation com ctx None pg.Type

                                        Arg.arg (
                                            pg.Name
                                            |> Option.defaultValue
                                                $"__arg{n + m}",
                                            annotation = ta
                                        )
                            ]

                        Arguments.arguments args

                    let returnType, _ =
                        typeAnnotation com ctx None memb.ReturnParameter.Type

                    let body = [ Statement.ellipsis ]

                    Statement.functionDef (
                        name,
                        args,
                        body,
                        returns = returnType,
                        decoratorList = decorators
                    )

                if members.IsEmpty then
                    Statement.Pass
            ]

        let bases =
            [
                let interfaces =
                    classEnt.AllInterfaces
                    |> List.ofSeq
                    |> List.map (fun int -> int.Entity)
                    |> List.filter (fun ent ->
                        ent.FullName <> classEnt.FullName
                    )

                for ref in interfaces do
                    let entity = com.TryGetEntity(ref)

                    match entity with
                    | Some entity ->
                        let expr, _stmts =
                            makeEntityTypeAnnotation com ctx entity.Ref [] None

                        expr
                    | None -> ()

                // Only add Protocol base if no interfaces (since the included interfaces will be protocols themselves)
                if List.isEmpty interfaces then
                    com.GetImportExpr(ctx, "typing", "Protocol")

                for gen in classEnt.GenericParameters do
                    Expression.subscript (
                        com.GetImportExpr(ctx, "typing", "Generic"),
                        com.AddTypeVar(ctx, gen.Name)
                    )
            ]

        [ Statement.classDef (classIdent, body = classMembers, bases = bases) ]

    let rec transformDeclaration
        (com: IPythonCompiler)
        ctx
        (decl: Fable.Declaration)
        =
        // printfn "transformDeclaration: %A" decl
        // printfn "ctx.UsedNames: %A" ctx.UsedNames

        let withCurrentScope (ctx: Context) (usedNames: Set<string>) f =
            let ctx =
                { ctx with
                    UsedNames =
                        { ctx.UsedNames with
                            CurrentDeclarationScope = HashSet usedNames
                        }
                }

            let result = f ctx

            ctx.UsedNames.DeclarationScopes.UnionWith(
                ctx.UsedNames.CurrentDeclarationScope
            )

            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames
            <| fun ctx -> transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames
            <| fun ctx ->
                let info = com.GetMember(decl.MemberRef)

                let decls =
                    if info.IsValue then
                        let value, stmts = transformAsExpr com ctx decl.Body
                        let name = com.GetIdentifier(ctx, decl.Name)
                        let ta, _ = typeAnnotation com ctx None decl.Body.Type

                        stmts
                        @ declareModuleMember
                            com
                            ctx
                            info.IsPublic
                            name
                            (Some ta)
                            value
                    else
                        transformModuleFunction
                            com
                            ctx
                            info
                            decl.Name
                            decl.Args
                            decl.Body

                decls

        | Fable.ClassDeclaration decl ->
            // printfn "Class: %A" decl
            let ent = com.GetEntity(decl.Entity)

            let classMembers =
                decl.AttachedMembers
                |> List.collect (fun memb ->
                    withCurrentScope ctx memb.UsedNames
                    <| fun ctx ->
                        let info =
                            memb.ImplementedSignatureRef
                            |> Option.map com.GetMember
                            |> Option.defaultWith (fun () ->
                                com.GetMember(memb.MemberRef)
                            )

                        if
                            not memb.IsMangled
                            && (info.IsGetter || info.IsSetter)
                        then
                            transformAttachedProperty com ctx info memb
                        else
                            transformAttachedMethod com ctx info memb
                )

            match ent, decl.Constructor with
            | ent, _ when ent.IsInterface -> transformInterface com ctx ent decl
            | ent, _ when ent.IsFSharpUnion ->
                transformUnion com ctx ent decl.Name classMembers
            | _, Some cons ->
                withCurrentScope ctx cons.UsedNames
                <| fun ctx ->
                    transformClassWithPrimaryConstructor
                        com
                        ctx
                        decl
                        classMembers
                        cons
            | _, None ->
                transformClassWithCompilerGeneratedConstructor
                    com
                    ctx
                    ent
                    decl.Name
                    classMembers

    let transformTypeVars
        (com: IPythonCompiler)
        ctx
        (typeVars: HashSet<string>)
        =
        [
            for var in typeVars do
                let targets = Expression.name var |> List.singleton
                let value = com.GetImportExpr(ctx, "typing", "TypeVar")
                let args = Expression.stringConstant var |> List.singleton
                let value = Expression.call (value, args)
                Statement.assign (targets, value)
        ]

    let transformExports
        (_com: IPythonCompiler)
        _ctx
        (exports: HashSet<string>)
        =
        let exports = exports |> List.ofSeq

        match exports with
        | [] -> []
        | _ ->
            let all = Expression.name "__all__"

            let names =
                exports |> List.map Expression.stringConstant |> Expression.list

            [ Statement.assign ([ all ], names) ]

    let transformImports
        (_com: IPythonCompiler)
        (imports: Import list)
        : Statement list
        =
        let imports =
            imports
            |> List.map (fun im ->
                let moduleName = im.Module

                match im.Name with
                | Some "*"
                | Some "default" ->
                    let (Identifier local) = im.LocalIdent.Value

                    if moduleName <> local then
                        Some moduleName, Alias.alias im.LocalIdent.Value
                    else
                        None, Alias.alias im.LocalIdent.Value
                | Some name ->
                    let name = Naming.toSnakeCase name

                    Some moduleName,
                    Alias.alias (
                        Identifier(Helpers.clean name),
                        ?asname = im.LocalIdent
                    )
                | None ->
                    None,
                    Alias.alias (
                        Identifier(moduleName),
                        ?asname = im.LocalIdent
                    )
            )
            |> List.groupBy fst
            |> List.map (fun (a, b) -> a, List.map snd b)
            |> List.sortBy (fun name ->
                let name =
                    match name with
                    | Some moduleName, _ -> moduleName.ToLower()
                    | None, { Name = name } :: _ -> name.Name
                    | _ -> ""

                match name with
                | name when name.StartsWith("__", StringComparison.Ordinal) ->
                    "A" + name
                | name when name.StartsWith("fable", StringComparison.Ordinal) ->
                    "C" + name
                | name when name.StartsWith(".", StringComparison.Ordinal) ->
                    "D" + name
                | _ -> "B" + name
            )

        [
            for moduleName, aliases in imports do
                match moduleName with
                | Some name ->
                    Statement.importFrom (Some(Identifier(name)), aliases)
                | None ->
                    // Do not put multiple imports on a single line. flake8(E401)
                    for alias in aliases do
                        Statement.import [ alias ]
        ]

    let getIdentForImport
        (ctx: Context)
        (moduleName: string)
        (name: string option)
        =
        // printfn "getIdentForImport: %A" (moduleName, name)
        match name with
        | None ->
            Path.GetFileNameWithoutExtension(moduleName) |> Identifier |> Some
        | Some name ->
            match name with
            | "default"
            | "*" -> Path.GetFileNameWithoutExtension(moduleName)
            | _ -> name
            |> Naming.toSnakeCase
            |> getUniqueNameInRootScope ctx
            |> Identifier
            |> Some

module Compiler =
    open Util

    type PythonCompiler(com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()
        let exports: HashSet<string> = HashSet()
        let typeVars: HashSet<string> = HashSet()

        interface IPythonCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, moduleName, ?name, ?r) =
                // printfn "GetImportExpr: %A" (moduleName, name)
                let cachedName = moduleName + "::" + defaultArg name "module"

                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> Expression.identifier localIdent
                    | None -> Expression.none
                | false, _ ->
                    let local_id = getIdentForImport ctx moduleName name

                    match name with
                    | Some "*"
                    | None ->
                        let i =
                            {
                                Name = None
                                Module = moduleName
                                LocalIdent = local_id
                            }

                        imports.Add(cachedName, i)
                    | Some name ->
                        let i =
                            {
                                Name =
                                    if name = Naming.placeholder then
                                        "`importMember` must be assigned to a variable"
                                        |> addError com [] r

                                        name
                                    else
                                        name
                                    |> Some
                                Module = moduleName
                                LocalIdent = local_id
                            }

                        imports.Add(cachedName, i)

                    match local_id with
                    | Some localId -> Expression.identifier localId
                    | None -> Expression.none

            member _.GetAllImports() =
                imports.Values :> Import seq |> List.ofSeq

            member _.GetAllTypeVars() = typeVars
            member _.GetAllExports() = exports

            member _.AddTypeVar(ctx, name: string) =
                // TypeVars should be private and uppercase. For auto-generated (inferred) generics we use a double-undercore.
                let name =
                    let name = name.ToUpperInvariant() |> Helpers.clean
                    $"_{name}"

                // For object expressions we need to create a new type scope so we make an extra padding to ensure uniqueness
                let name = name.PadRight(ctx.TypeParamsScope + name.Length, '_')
                typeVars.Add name |> ignore

                ctx.UsedNames.DeclarationScopes.Add(name) |> ignore

                Expression.name name

            member _.AddExport(name: string) =
                exports.Add name |> ignore
                Expression.name name

            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e

            member bcom.TransformAsStatements(ctx, ret, e) =
                transformAsStatements bcom ctx ret e

            member bcom.TransformFunction(ctx, name, args, body, generics) =
                transformFunction bcom ctx name args body generics

            member bcom.TransformImport(ctx, selector, path) =
                transformImport bcom ctx None selector path

            member bcom.GetIdentifier(ctx, name) = getIdentifier bcom ctx name

            member bcom.GetIdentifierAsExpr(ctx, name) =
                getIdentifier bcom ctx name |> Expression.name

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.SourceFiles = com.SourceFiles
            member _.IncrementCounter() = com.IncrementCounter()

            member _.IsPrecompilingInlineFunction =
                com.IsPrecompilingInlineFunction

            member _.WillPrecompileInlineFunction(file) =
                com.WillPrecompileInlineFunction(file)

            member _.GetImplementationFile(fileName) =
                com.GetImplementationFile(fileName)

            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)

            member _.AddWatchDependency(fileName) =
                com.AddWatchDependency(fileName)

            member _.AddLog
                (
                    msg,
                    severity,
                    ?range,
                    ?fileName: string,
                    ?tag: string
                )
                =
                com.AddLog(
                    msg,
                    severity,
                    ?range = range,
                    ?fileName = fileName,
                    ?tag = tag
                )

    let makeCompiler com = PythonCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IPythonCompiler

        let declScopes =
            let hs = HashSet()

            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)

            hs

        let ctx =
            {
                File = file
                UsedNames =
                    {
                        RootScope = HashSet file.UsedNamesInRootScope
                        DeclarationScopes = declScopes
                        CurrentDeclarationScope = Unchecked.defaultof<_>
                    }
                BoundVars =
                    {
                        EnclosingScope = HashSet()
                        LocalScope = HashSet()
                        Inceptions = 0
                    }
                DecisionTargets = []
                HoistVars = fun _ -> false
                TailCallOpportunity = None
                OptimizeTailCall = fun () -> ()
                ScopedTypeParams = Set.empty
                TypeParamsScope = 0
            }

        //printfn "file: %A" file.Declarations
        let rootDecls =
            List.collect (transformDeclaration com ctx) file.Declarations

        let typeVars = com.GetAllTypeVars() |> transformTypeVars com ctx
        let importDecls = com.GetAllImports() |> transformImports com
        let exports = com.GetAllExports() |> transformExports com ctx
        let body = importDecls @ typeVars @ rootDecls @ exports
        Module.module' body
