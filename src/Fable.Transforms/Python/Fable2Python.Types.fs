module Fable.Transforms.Python.Types

open System.Collections.Generic
open Fable.AST
open Fable.Transforms.Python.AST

type ReturnStrategy =
    /// Return last expression
    | Return
    | ReturnUnit
    /// Return within a with-statement (to make sure we don't TC with statements)
    | ResourceManager of ReturnStrategy option
    | Assign of Expression
    | Target of Identifier

type Import =
    {
        Module: string
        LocalIdent: Identifier
        Name: string option
    }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: Arg list
    abstract IsRecursiveRef: Fable.Expr -> bool

type MemberKind =
    | ClassConstructor
    | NonAttached of funcName: string
    | Attached of isStatic: bool

// Represents different kinds of field access for proper naming convention selection
[<Struct>]
type FieldNamingKind =
    | RegularField
    | InstancePropertyBacking
    | StaticProperty


/// Represents different styles of Python class generation
[<RequireQualifiedAccess>]
type ClassStyle =
    | Properties
    | Attributes

/// Represents the parameters for the Python class attribute
type ClassAttributes =
    {
        Style: ClassStyle
        Init: bool
        Slots: bool
        Frozen: bool
        Repr: bool
        Eq: bool
    }

    static member Default =
        {
            Style = ClassStyle.Properties
            Init = true
            Slots = false
            Frozen = false
            Repr = false
            Eq = false
        }

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

                if not (this.LocalScope.Contains name) && this.EnclosingScope.Contains name then
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
        NarrowedTypes: Map<string, Fable.Type>
    }

type IPythonCompiler =
    inherit Fable.Compiler
    abstract AddTypeVar: ctx: Context * name: string -> Expression
    abstract AddExport: name: string -> Expression
    abstract GetIdentifier: ctx: Context * name: string -> Identifier
    abstract GetIdentifierAsExpr: ctx: Context * name: string -> Expression
    abstract GetAllImports: unit -> Import list
    abstract GetAllExports: unit -> HashSet<string>
    abstract GetAllTypeVars: unit -> HashSet<string>

    abstract GetImportExpr: Context * moduleName: string * ?name: string * ?loc: SourceLocation -> Expression

    abstract TransformAsExpr: Context * Fable.Expr -> Expression * Statement list

    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement list

    abstract TransformImport: Context * selector: string * path: string -> Expression

    abstract TransformFunction:
        Context * string option * Fable.Ident list * Fable.Expr * Set<string> -> Arguments * Statement list

    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit
