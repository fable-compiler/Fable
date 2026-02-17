/// Utilities for Python ABC base classes and protocol support.
/// Handles interface-to-ABC mapping, dunder generation, and abstract class stubs.
module Fable.Transforms.Python.Bases

open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.Python.AST
open Fable.Transforms.Python.Types
open Fable.Transforms.Python.Util
open Fable.Transforms.Python.Annotation

/// Maps F# interface names to Python ABC base class names.
/// Returns None if the interface should not be mapped to an ABC.
/// Note: F# nested modules use + not . in full names (e.g., Fable.Core.Py+Mapping+IMapping`2)
let getAbcClassesForInterface (name: string) (fullName: string) : string list option =
    match name with
    // .NET interfaces - these have method contracts that guarantee methods exist
    | "IDisposable" -> Some [ "DisposableBase" ]
    | "IEnumerator_1" -> Some [ "EnumeratorBase"; "DisposableBase" ]
    | "IEnumerable_1" -> Some [ "EnumerableBase" ]

    // Py.* marker interfaces - these assume methods exist (opt-in by F# developer)
    | "ContextManager" -> Some [ "DisposableBase" ]
    | "Stringable" -> Some [ "StringableBase" ]
    | "Equatable" -> Some [ "EquatableBase" ]
    | "Comparable" -> Some [ "ComparableBase" ]
    | "Hashable" -> Some [ "HashableBase" ]
    | "Sized" -> Some [ "SizedBase" ]
    | "Iterable" -> Some [ "EnumerableBase" ]
    | "Iterator" -> Some [ "EnumeratorBase" ]

    // Py.Mapping protocol interfaces (Fable.Core.Py.MappingModule.*)
    // These map to collections.abc classes directly
    | "IMapping_2" when fullName.Contains "Fable.Core.Py.MappingModule" -> Some [ "Mapping" ]
    | "IMutableMapping_2" when fullName.Contains "Fable.Core.Py.MappingModule" -> Some [ "MutableMapping" ]

    // Py.Set protocol interfaces (Fable.Core.Py.SetModule.*)
    // These map to collections.abc classes directly
    | "ISet_1" when fullName.Contains "Fable.Core.Py.SetModule" -> Some [ "Set" ]
    | "IMutableSet_1" when fullName.Contains "Fable.Core.Py.SetModule" -> Some [ "MutableSet" ]

    | _ -> None

/// ABC classes that accept type parameters (generic)
let genericAbcClasses =
    Set.ofList
        [
            "EnumeratorBase"
            "EnumerableBase"
            // collections.abc classes (generic)
            "Mapping"
            "MutableMapping"
            "Set"
            "MutableSet"
        ]

/// ABC classes from collections.abc (not from bases module)
let collectionsAbcClasses =
    Set.ofList [ "Mapping"; "MutableMapping"; "Set"; "MutableSet" ]

/// Interfaces that are allowed to contribute base classes
let allowedInterfaces =
    [
        // .NET interfaces - these have method contracts that guarantee methods exist
        // The base classes delegate to these known methods
        "IDisposable" // Dispose() -> DisposableBase (__enter__, __exit__)
        "IEnumerator_1" // MoveNext(), get_Current() -> EnumeratorBase (__iter__, __next__)
        "IEnumerable_1" // GetEnumerator() -> EnumerableBase (__iter__)

        // Py.* marker interfaces - these assume methods exist (opt-in by F# developer)
        // The base classes delegate to methods that should be implemented
        "ContextManager" // assumes Dispose() -> DisposableBase
        "Stringable" // assumes ToString() -> StringableBase (__str__, __repr__)
        "Equatable" // assumes Equals() -> EquatableBase (__eq__, __ne__)
        "Comparable" // assumes CompareTo() -> ComparableBase (__lt__, __le__, __gt__, __ge__)
        "Hashable" // assumes GetHashCode() -> HashableBase (__hash__)
        "Sized" // assumes Count -> SizedBase (__len__)
        "Iterable" // assumes GetEnumerator() -> EnumerableBase (__iter__)
        "Iterator" // assumes MoveNext(), get_Current() -> EnumeratorBase (__iter__, __next__)

        // Py.Mapping.* protocol interfaces - map to collections.abc.Mapping/MutableMapping
        // Compiler generates dunders (__getitem__, __contains__, __len__, __iter__)
        "IMapping_2"
        "IMutableMapping_2"

        // Py.Set.* protocol interfaces - map to collections.abc.Set/MutableSet
        // Compiler generates dunders (__contains__, __len__, __iter__)
        "ISet_1"
        "IMutableSet_1"
    ]

/// Creates an ABC base class expression with optional generic type arguments.
/// Handles importing from collections.abc or bases module as appropriate.
let makeAbcBaseExpr (com: IPythonCompiler) ctx (abcName: string) (genArgs: Fable.Type list) : Expression =
    // Determine import source: collections.abc or bases module
    let isCollectionsAbc = collectionsAbcClasses.Contains abcName

    let baseExpr =
        if isCollectionsAbc then
            com.GetImportExpr(ctx, "collections.abc", abcName)
        else
            libValue com ctx "bases" abcName

    // Only apply type args to generic ABC classes
    if List.isEmpty genArgs || not (genericAbcClasses.Contains abcName) then
        baseExpr
    else
        let typeArgs =
            genArgs
            |> List.map (fun genArg ->
                let arg, _ = typeAnnotation com ctx None genArg
                arg
            )

        Expression.subscript (baseExpr, Expression.tuple typeArgs)

/// Generate Python protocol dunder methods for Py.Mapping and Py.Set interfaces.
/// When a class implements IMapping, IMutableMapping, ISet, or IMutableSet,
/// we generate the corresponding Python dunders that call the attached interface methods.
let generatePythonProtocolDunders (com: IPythonCompiler) ctx (classEnt: Fable.Entity) : Statement list =
    let self = Expression.name "self"

    // Helper to create: self.methodName(args)
    let selfCall methodName args =
        Expression.call (Expression.attribute (self, Identifier methodName, Load), args)

    // Helper to create: return self.methodName(args)
    let returnSelfCall methodName args =
        Statement.return' (selfCall methodName args)

    // Check which Python protocol interfaces are implemented
    // Note: F# modules compile with "Module" suffix (e.g., Fable.Core.Py.MappingModule.IMapping`2)
    let hasIMapping =
        classEnt.AllInterfaces
        |> Seq.exists (fun int -> int.Entity.FullName = "Fable.Core.Py.MappingModule.IMapping`2")

    let hasIMutableMapping =
        classEnt.AllInterfaces
        |> Seq.exists (fun int -> int.Entity.FullName = "Fable.Core.Py.MappingModule.IMutableMapping`2")

    let hasISet =
        classEnt.AllInterfaces
        |> Seq.exists (fun int -> int.Entity.FullName = "Fable.Core.Py.SetModule.ISet`1")

    let hasIMutableSet =
        classEnt.AllInterfaces
        |> Seq.exists (fun int -> int.Entity.FullName = "Fable.Core.Py.SetModule.IMutableSet`1")

    // Generate IMapping dunders: __getitem__, __contains__, __len__, __iter__
    // Note: Method names use Python naming convention (lowercase with underscores)
    // __iter__ always yields keys following Python's Mapping convention.
    // F# iteration (for KeyValue(k,v) in map) compiles to GetEnumerator()/MoveNext()/Current
    // and never uses __iter__, so this is purely for Python interop.
    let mappingDunders =
        if hasIMapping || hasIMutableMapping then
            [
                // def __getitem__(self, key): return self.get_item(key)
                Statement.functionDef (
                    Identifier "__getitem__",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "key" ],
                    body = [ returnSelfCall "get_item" [ Expression.name "key" ] ]
                )
                // def __contains__(self, key): return self.ContainsKey(key)
                Statement.functionDef (
                    Identifier "__contains__",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "key" ],
                    body = [ returnSelfCall "ContainsKey" [ Expression.name "key" ] ]
                )
                // def __len__(self): return self.Count
                Statement.functionDef (
                    Identifier "__len__",
                    Arguments.arguments [ Arg.arg "self" ],
                    body = [ Statement.return' (Expression.attribute (self, Identifier "Count", Load)) ]
                )
                // def __iter__(self): yields keys only (Python Mapping convention)
                let toIterator = com.GetImportExpr(ctx, "fable_library.util", "to_iterator")
                let kvVar = Expression.name "kv"

                Statement.functionDef (
                    Identifier "__iter__",
                    Arguments.arguments [ Arg.arg "self" ],
                    body =
                        [
                            Statement.for' (
                                kvVar,
                                Expression.call (toIterator, [ selfCall "GetEnumerator" [] ]),
                                // Yield kv[0] (key) since GetEnumerator yields (key, value) tuples
                                [
                                    Statement.expr (
                                        Yield(Some(Expression.subscript (kvVar, Expression.intConstant 0, Load)))
                                    )
                                ]
                            )
                        ]
                )
            ]
        else
            []

    // Generate IMutableMapping dunders: __setitem__, __delitem__
    let mutableMappingDunders =
        if hasIMutableMapping then
            [
                // def __setitem__(self, key, value): self.set_item(key, value)
                Statement.functionDef (
                    Identifier "__setitem__",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "key"; Arg.arg "value" ],
                    body =
                        [
                            Statement.expr (selfCall "set_item" [ Expression.name "key"; Expression.name "value" ])
                        ]
                )
                // def __delitem__(self, key): self.Remove(key)
                Statement.functionDef (
                    Identifier "__delitem__",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "key" ],
                    body = [ Statement.expr (selfCall "Remove" [ Expression.name "key" ]) ]
                )
            ]
        else
            []

    // Generate ISet dunders: __contains__, __len__, __iter__
    let setDunders =
        if hasISet || hasIMutableSet then
            [
                // def __contains__(self, item): return self.Contains(item)
                Statement.functionDef (
                    Identifier "__contains__",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "item" ],
                    body = [ returnSelfCall "Contains" [ Expression.name "item" ] ]
                )
                // def __len__(self): return self.Count
                Statement.functionDef (
                    Identifier "__len__",
                    Arguments.arguments [ Arg.arg "self" ],
                    body = [ Statement.return' (Expression.attribute (self, Identifier "Count", Load)) ]
                )
                // def __iter__(self): return to_iterator(self.GetEnumerator())
                let toIterator = com.GetImportExpr(ctx, "fable_library.util", "to_iterator")

                Statement.functionDef (
                    Identifier "__iter__",
                    Arguments.arguments [ Arg.arg "self" ],
                    body =
                        [
                            Statement.return' (Expression.call (toIterator, [ selfCall "GetEnumerator" [] ]))
                        ]
                )
            ]
        else
            []

    // Generate IMutableSet dunders: add, discard
    // Note: Parameter name must be "value" to match collections.abc.MutableSet
    let mutableSetDunders =
        if hasIMutableSet then
            [
                // def add(self, value): self.Add(value)
                Statement.functionDef (
                    Identifier "add",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "value" ],
                    body = [ Statement.expr (selfCall "Add" [ Expression.name "value" ]) ]
                )
                // def discard(self, value): self.Remove(value)
                Statement.functionDef (
                    Identifier "discard",
                    Arguments.arguments [ Arg.arg "self"; Arg.arg "value" ],
                    body = [ Statement.expr (selfCall "Remove" [ Expression.name "value" ]) ]
                )
            ]
        else
            []

    // Return all generated dunders, avoiding duplicates
    // Note: IMapping and ISet both generate __contains__, __len__, __iter__
    // If a class implements both (unlikely), we'd have duplicates - but in practice
    // a collection is either a mapping or a set, not both
    mappingDunders @ mutableMappingDunders @ setDunders @ mutableSetDunders

/// Known core interfaces and their method members.
/// These interfaces are injected by the compiler and their entities may not be available.
/// Maps interface full name -> set of method member names.
let knownInterfaceMethods =
    Map
        [
            "Fable.Core.IGenericAdder`1", set [ "GetZero"; "Add" ]
            "Fable.Core.IGenericAverager`1", set [ "GetZero"; "Add"; "DivideByInt" ]
            "System.Collections.Generic.IComparer`1", set [ "Compare" ]
            "System.Collections.Generic.IEqualityComparer`1", set [ "Equals"; "GetHashCode" ]
        ]

/// All known method names from core interfaces (for untyped object expressions).
let knownInterfaceMethodNames =
    knownInterfaceMethods |> Map.values |> Seq.collect id |> Set.ofSeq

/// Check if the interface member is a method (vs property).
/// Methods have parameters; properties (even returning functions) don't.
/// Used in object expression code generation to determine whether to emit
/// a method or a @property decorator.
let isInterfaceMethod (com: Compiler) (typ: Fable.Type) (memberName: string) : bool =
    match typ with
    | Fable.DeclaredType(entRef, _) ->
        // Check known interfaces first (handles compiler-injected interfaces)
        match knownInterfaceMethods.TryFind entRef.FullName with
        | Some methods -> methods.Contains memberName
        | None ->
            // Not a known interface, try entity lookup for user-defined interfaces
            match com.TryGetEntity entRef with
            | Some ent ->
                ent.MembersFunctionsAndValues
                |> Seq.tryFind (fun m -> m.DisplayName = memberName || m.CompiledName = memberName)
                |> Option.map (fun m -> m.CurriedParameterGroups |> List.exists (not << List.isEmpty))
                |> Option.defaultValue false
            | None -> false
    | Fable.Any ->
        // For untyped object expressions (compiler-injected), check known method names
        knownInterfaceMethodNames.Contains memberName
    | _ -> false

/// Utilities for interface and abstract class member naming.
module InterfaceNaming =
    /// Computes the overload suffix for an interface/abstract class member based on parameter types.
    /// Returns empty string for getters/setters.
    let getOverloadSuffix (ent: Fable.Entity) (memb: Fable.MemberFunctionOrValue) =
        if memb.IsGetter || memb.IsSetter then
            ""
        else
            let entityGenericParameters = ent.GenericParameters |> List.map (fun g -> g.Name)

            memb.CurriedParameterGroups
            |> List.collect (List.map (fun pg -> pg.Type))
            |> List.singleton
            |> OverloadSuffix.getHash entityGenericParameters

    /// Generates a mangled member name for interfaces/abstract classes.
    /// Format: EntityFullPath_MemberName + OverloadSuffix (dots replaced with underscores)
    let getMangledMemberName (ent: Fable.Entity) (memb: Fable.MemberFunctionOrValue) =
        let overloadSuffix = getOverloadSuffix ent memb
        let lastDotIndex = memb.FullName.LastIndexOf '.'
        let fullNamePath = memb.FullName.Substring(0, lastDotIndex)
        $"%s{fullNamePath}.%s{memb.CompiledName}%s{overloadSuffix}".Replace(".", "_")

/// Generates abstract method stubs for abstract classes.
/// Creates stubs for dispatch slots (abstract members) that don't have default implementations.
let generateAbstractMethodStubs
    (com: IPythonCompiler)
    ctx
    (classEnt: Fable.Entity)
    (attachedMembers: Fable.MemberDecl list)
    : Statement list
    =
    if not classEnt.IsAbstractClass then
        []
    else
        let implementedMemberNames =
            attachedMembers
            |> List.map (fun m -> (com.GetMember m.MemberRef).CompiledName)
            |> Set.ofList

        let isAbstractMember (memb: Fable.MemberFunctionOrValue) =
            memb.IsDispatchSlot
            && not memb.IsProperty
            && not (Util.hasAnyEmitAttribute memb.Attributes)
            && not (Set.contains memb.CompiledName implementedMemberNames)

        let makeAbstractMethodStub (memb: Fable.MemberFunctionOrValue) =
            let name =
                InterfaceNaming.getMangledMemberName classEnt memb
                |> fun n -> com.GetIdentifier(ctx, n)

            let posOnlyArgs =
                [
                    if memb.IsInstance then
                        Arg.arg "self"

                    for n, parameterGroup in Seq.indexed memb.CurriedParameterGroups do
                        for m, pg in Seq.indexed parameterGroup do
                            let paramType = FableTransforms.uncurryType pg.Type
                            let annotation, _ = typeAnnotation com ctx None paramType
                            let paramName = pg.Name |> Option.defaultValue $"__arg%d{n + m}"
                            Arg.arg (paramName, annotation = annotation)
                ]

            let returnType, _ =
                memb.ReturnParameter.Type
                |> FableTransforms.uncurryType
                |> typeAnnotation com ctx None

            Statement.functionDef (
                name,
                Arguments.arguments (posonlyargs = posOnlyArgs),
                body = [ Statement.ellipsis ],
                returns = returnType,
                decoratorList = [ com.GetImportExpr(ctx, "abc", "abstractmethod") ],
                typeParams = Annotation.makeMemberTypeParams com ctx memb.GenericParameters
            )

        classEnt.MembersFunctionsAndValues
        |> Seq.choose (fun memb ->
            if isAbstractMember memb then
                Some(makeAbstractMethodStub memb)
            else
                None
        )
        |> Seq.toList
