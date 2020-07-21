module Fable.Transforms.Inject

open System
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Transforms
open FSharp2Fable.Helpers
open FSharp2Fable.Patterns
open FSharp2Fable.TypeHelpers

let private resolveParamGeneric (genArg: Lazy<(string * Fable.Type) list>)= function
    | Fable.GenericParam genParamName ->
        genArg.Value
        |> List.tryPick (fun (k,v) -> if k = genParamName then Some v else None)
        // We could add an error here if not found, but it will be added anyways
        // when trying to compile TypeInfo for a generic in Fable2Babel
        |> Option.defaultValue (Fable.GenericParam genParamName)
    | t -> t

let (|TryDefinition|_|) (NonAbbreviatedType t) =
    if t.HasTypeDefinition
    then Some(t.TypeDefinition, t.GenericArguments)
    else None

let (|GeneratedInterface|_|) com ctx r t =
    match t with
    | Fable.DeclaredType(typDef,[t]) ->
        // TODO: Unify with Replacements.injectArg?
        match typDef.FullName with
        | Types.typeResolver ->
            let fn = Fable.Value(Fable.TypeInfo t, r) |> makeDelegate []
            Replacements.Helpers.objExpr ["ResolveType", fn] |> Some
        | Types.comparer ->
            Replacements.makeComparer com t |> Some
        | Types.equalityComparer ->
            Replacements.makeEqualityComparer com t |> Some
        | Types.adder ->
            Replacements.makeGenericAdder com ctx t |> Some
        | Types.averager ->
            Replacements.makeGenericAverager com ctx t |> Some
        | _ -> None
    | _ -> None

let injectArg com ctx r (genArgs: (string * Fable.Type) list) (par: FSharpParameter): Fable.Expr =
    let parType = nonAbbreviatedType par.Type
    let typ =
        // The type of the parameter must be an option
        if parType.HasTypeDefinition && parType.TypeDefinition.TryFullName = Some Types.option
        then makeType (Map genArgs) parType.GenericArguments.[0] |> Some
        else None
    match typ with
    | Some(GeneratedInterface com ctx r e) -> e
    | _ ->
        match typ with
        | Some typ -> getTypeFullName true typ
        | None -> string parType
        |> sprintf "Cannot inject argument %s of type %s" par.DisplayName
        |> addErrorAndReturnNull com ctx.InlinePath r
