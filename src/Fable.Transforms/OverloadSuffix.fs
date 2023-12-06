[<RequireQualifiedAccess>]
module rec Fable.Transforms.OverloadSuffix

open Fable
open Fable.AST
open System.Collections.Generic

type ParamTypes = Fable.Type list

let private tryFindAttributeArgs fullName (atts: Fable.Attribute seq) =
    atts
    |> Seq.tryPick (fun att ->
        if att.Entity.FullName = fullName then
            Some att.ConstructorArgs
        else
            None
    )

// -------- End of helper functions

let private hashToString (i: int) =
    if i < 0 then
        "Z" + (abs i).ToString("X")
    else
        i.ToString("X")

// Not perfect but hopefully covers most of the cases
// Using only common constrains from https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/constraints
let private getConstraintHash genParams =
    function
    // TODO: Full member signature hash?
    | Fable.Constraint.HasMember(name, isStatic) ->
        (if isStatic then
             "static "
         else
             "")
        + "member "
        + name
    | Fable.Constraint.CoercesTo t -> ":>" + getTypeFastFullName genParams t
    | Fable.Constraint.IsNullable -> "null"
    | Fable.Constraint.IsValueType -> "struct"
    | Fable.Constraint.IsReferenceType -> "not struct"
    | Fable.Constraint.IsUnmanaged -> "unmanaged"
    | Fable.Constraint.HasDefaultConstructor -> "new"
    | Fable.Constraint.HasComparison -> "comparison"
    | Fable.Constraint.HasEquality -> "equality"
    | Fable.Constraint.IsEnum -> "enum"

let rec private getTypeFastFullName
    (genParams: IDictionary<_, _>)
    (t: Fable.Type)
    =
    match t with
    | Fable.Measure fullname -> fullname
    | Fable.GenericParam(name, isMeasure, constraints) ->
        if isMeasure then
            "measure"
        else
            match genParams.TryGetValue(name) with
            | true, i -> i
            | false, _ ->
                constraints
                |> List.map (getConstraintHash genParams)
                |> String.concat ","
    | Fable.Tuple(genArgs, isStruct) ->
        let genArgs =
            genArgs
            |> Seq.map (getTypeFastFullName genParams)
            |> String.concat " * "

        if isStruct then
            "struct " + genArgs
        else
            genArgs
    | Fable.Array(genArg, kind) ->
        let name =
            match kind with
            | Fable.ResizeArray -> "array"
            | Fable.MutableArray -> "resizearray"
            | Fable.ImmutableArray -> "immutablearray"

        getTypeFastFullName genParams genArg + " " + name
    | Fable.List genArg -> getTypeFastFullName genParams genArg + " list"
    | Fable.Option(genArg, isStruct) ->
        (if isStruct then
             "struct "
         else
             "")
        + (getTypeFastFullName genParams genArg)
        + " option"
    | Fable.LambdaType(argType, returnType) ->
        [
            argType
            returnType
        ]
        |> List.map (getTypeFastFullName genParams)
        |> String.concat " -> "
    // TODO: Use Func` instead?
    | Fable.DelegateType(argTypes, returnType) ->
        argTypes @ [ returnType ]
        |> List.map (getTypeFastFullName genParams)
        |> String.concat " -> "
    | Fable.AnonymousRecordType(fieldNames, genArgs, isStruct) ->
        let fields =
            Seq.zip fieldNames genArgs
            |> Seq.map (fun (key, typ) ->
                key + " : " + getTypeFastFullName genParams typ
            )
            |> String.concat "; "

        (if isStruct then
             "struct "
         else
             "")
        + "{|"
        + fields
        + "|}"
    | Fable.DeclaredType(tdef, genArgs) ->
        let genArgs = genArgs |> Seq.mapToList (getTypeFastFullName genParams)
        // Not sure why, but when precompiling F# changes measure types to MeasureProduct<'M, MeasureOne>
        match tdef.FullName, genArgs with
        | Types.measureProduct2, [ measure; Types.measureOne ] -> measure
        | _ ->
            let genArgs = String.concat "," genArgs

            let genArgs =
                if genArgs = "" then
                    ""
                else
                    "[" + genArgs + "]"

            tdef.FullName + genArgs
    | Fable.MetaType -> Types.type_
    | Fable.Any -> Types.object
    | Fable.Unit -> Types.unit
    | Fable.Boolean -> Types.bool
    | Fable.Char -> Types.char
    | Fable.String -> Types.string
    | Fable.Regex -> Types.regex
    | Fable.Number(kind, detail) -> getNumberFullName false kind detail

// From https://stackoverflow.com/a/37449594
let private combineHashCodes (hashes: int seq) =
    let hashes = Seq.toArray hashes

    if hashes.Length = 0 then
        0
    else
        hashes |> Array.reduce (fun h1 h2 -> ((h1 <<< 5) + h1) ^^^ h2)

// F# hash function gives different results in different runs
// Taken from fable-library/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
let private stringHash (s: string) =
    let mutable h = 5381

    for i = 0 to s.Length - 1 do
        h <- (h * 33) ^^^ (int s[i])

    h

let private getHashPrivate (paramTypes: ParamTypes) genParams =
    paramTypes
    |> List.map (getTypeFastFullName genParams >> stringHash)
    |> combineHashCodes
    |> hashToString

let hasEmptyOverloadSuffix (curriedParamTypes: ParamTypes) =
    // Don't use overload suffix for members without arguments
    match curriedParamTypes with
    | [] -> true
    | [ Fable.Unit ] -> true
    | _ -> false

let getHash
    (entityGenericParams: string list)
    (curriedParamTypeGroups: Fable.Type list list)
    =
    match curriedParamTypeGroups with
    | [ paramTypes ] ->
        if hasEmptyOverloadSuffix paramTypes then
            ""
        else
            // Generics can have different names in signature
            // and implementation files, use the position instead
            let genParams =
                entityGenericParams
                |> List.mapi (fun i p -> p, string<int> i)
                |> dict

            getHashPrivate paramTypes genParams
    // Members with curried params cannot be overloaded in F#
    // TODO: Also private methods defined with `let` cannot be overloaded
    // but I don't know how to identify them in the AST
    | _ -> ""

/// Used for extension members
let getExtensionHash (curriedParamTypeGroups: Fable.Type list list) =
    match curriedParamTypeGroups with
    | [ paramTypes ] ->
        if hasEmptyOverloadSuffix paramTypes then
            ""
        else
            // Type resolution in extension member seems to be different
            // and doesn't take generics into account
            dict [] |> getHashPrivate paramTypes
    // Members with curried params cannot be overloaded in F#
    | _ -> ""
