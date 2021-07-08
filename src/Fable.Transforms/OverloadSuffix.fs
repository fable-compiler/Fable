[<RequireQualifiedAccess>]
module rec Fable.Transforms.OverloadSuffix

open Fable
open Fable.AST
open System.Collections.Generic

type ParamTypes = Fable.Type list

[<RequireQualifiedAccess>]
module private Atts =
    let [<Literal>] noOverloadSuffix = "Fable.Core.NoOverloadSuffixAttribute" // typeof<Fable.Core.OverloadSuffixAttribute>.FullName

let private tryFindAttributeArgs fullName (atts: Fable.Attribute seq) =
    atts |> Seq.tryPick (fun att ->
        if att.Entity.FullName = fullName then Some att.ConstructorArgs
        else None)

// -------- End of helper functions

let private hashToString (i: int) =
    if i < 0
    then "Z" + (abs i).ToString("X")
    else i.ToString("X")

// Not perfect but hopefully covers most of the cases
// Using only common constrains from https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/constraints
let private getConstraintHash genParams = function
    // TODO: Full member signature hash?
    | Fable.Constraint.HasMember(name, isStatic) ->
        (if isStatic then "static " else "") + "member " + name
    | Fable.Constraint.CoercesTo t ->
        ":>" + getTypeFastFullName genParams t
    | Fable.Constraint.IsNullable -> "null"
    | Fable.Constraint.IsValueType -> "struct"
    | Fable.Constraint.IsReferenceType -> "not struct"
    | Fable.Constraint.IsUnmanaged -> "unmanaged"
    | Fable.Constraint.HasDefaultConstructor -> "new"
    | Fable.Constraint.HasComparison -> "comparison"
    | Fable.Constraint.HasEquality -> "equality"
    | Fable.Constraint.IsEnum -> "enum"

let rec private getTypeFastFullName (genParams: IDictionary<_,_>) (t: Fable.Type) =
    match t with
    | Fable.Measure fullname -> fullname
    | Fable.GenericParam(name, constraints) ->
        match genParams.TryGetValue(name) with
        | true, i -> i
        | false, _ -> constraints |> List.map (getConstraintHash genParams) |> String.concat ","
    | Fable.Tuple(genArgs, isStruct) ->
        let genArgs = genArgs |> Seq.map (getTypeFastFullName genParams) |> String.concat " * "
        if isStruct then "struct " + genArgs
        else genArgs
    | Fable.Array genArg ->
        getTypeFastFullName genParams genArg + "[]"
    | Fable.List genArg ->
        getTypeFastFullName genParams genArg + " list"
    | Fable.Option(genArg, isStruct) ->
        (if isStruct then "struct " else "") + (getTypeFastFullName genParams genArg) + " option"
    | Fable.LambdaType(argType, returnType) ->
        [argType; returnType] |> List.map (getTypeFastFullName genParams) |> String.concat " -> "
    // TODO: Use Func` instead?
    | Fable.DelegateType(argTypes, returnType) ->
        argTypes @ [returnType] |> List.map (getTypeFastFullName genParams) |> String.concat " -> "
    | Fable.AnonymousRecordType(fieldNames, genArgs) ->
        let fields =
            Seq.zip fieldNames genArgs
            |> Seq.map (fun (key, typ) -> key + " : " + getTypeFastFullName genParams typ)
            |> String.concat "; "
        "{|" + fields + "|}"
    | Fable.DeclaredType(tdef, genArgs) ->
        let genArgs = genArgs |> Seq.map (getTypeFastFullName genParams) |> String.concat ","
        let genArgs = if genArgs = "" then "" else "[" + genArgs + "]"
        tdef.FullName + genArgs
    | Fable.MetaType -> Types.type_
    | Fable.Any -> Types.object
    | Fable.Unit -> Types.unit
    | Fable.Boolean -> Types.bool
    | Fable.Char -> Types.char
    | Fable.String -> Types.string
    | Fable.Regex -> Types.regex
    | Fable.Enum ref -> ref.FullName
    | Fable.Number(kind, uom) -> getNumberFullName uom kind

// From https://stackoverflow.com/a/37449594
let private combineHashCodes (hashes: int seq) =
    let hashes = Seq.toArray hashes
    if hashes.Length = 0
    then 0
    else hashes |> Array.reduce (fun h1 h2 -> ((h1 <<< 5) + h1) ^^^ h2)

// F# hash function gives different results in different runs
// Taken from fable-library/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
let private stringHash (s: string) =
    let mutable h = 5381
    for i = 0 to s.Length - 1 do
        h <- (h * 33) ^^^ (int s.[i])
    h

let private getHashPrivate (entAtts: Fable.Attribute seq) (paramTypes: ParamTypes) genParams =
    // TODO: This is only useful when compiling fable-library,
    // use conditional compilation?
    match tryFindAttributeArgs Atts.noOverloadSuffix entAtts with
    | Some _ -> ""
    | _ ->
        paramTypes
        |> List.map (getTypeFastFullName genParams >> stringHash)
        |> combineHashCodes
        |> hashToString

let hasEmptyOverloadSuffix (curriedParamTypes: ParamTypes) =
    // Don't use overload suffix for members without arguments
    match curriedParamTypes with
    | [] -> true
    | [Fable.Unit] -> true
    | _ -> false

let getHashFromCurriedParamGroups (entity: Fable.Entity) (curriedParamGroups: Fable.Parameter list list) =
    match curriedParamGroups with
    | [params'] ->
        let paramTypes = params' |> List.map (fun p -> p.Type)
        if hasEmptyOverloadSuffix paramTypes then ""
        else
            // Generics can have different names in signature
            // and implementation files, use the position instead
            let genParams =
                entity.GenericParameters
                |> List.mapi (fun i p -> p.Name, string i)
                |> dict
            getHashPrivate entity.Attributes paramTypes genParams
    // Members with curried params cannot be overloaded in F#
    // TODO: Also private methods defined with `let` cannot be overloaded
    // but I don't know how to identify them in the AST
    | _ -> ""

let getHash (entity: Fable.Entity) (m: Fable.MemberFunctionOrValue) =
    getHashFromCurriedParamGroups entity m.CurriedParameterGroups

/// Used for extension members
let getExtensionHash (m: Fable.MemberFunctionOrValue) =
    match m.CurriedParameterGroups with
    | [params'] ->
        let paramTypes = params' |> List.map (fun p -> p.Type)
        if hasEmptyOverloadSuffix paramTypes then ""
        else
            // Type resolution in extension member seems to be different
            // and doesn't take generics into account
            dict [] |> getHashPrivate [] paramTypes
    // Members with curried params cannot be overloaded in F#
    | _ -> ""
