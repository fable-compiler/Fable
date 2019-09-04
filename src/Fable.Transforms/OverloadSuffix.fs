[<RequireQualifiedAccess>]
module Fable.Transforms.OverloadSuffix

open System.Collections.Generic
open FSharp.Compiler.SourceCodeServices

type Params = IList<IList<FSharpParameter>>

[<RequireQualifiedAccess>]
module private Atts =
    let [<Literal>] replaces = "Fable.Core.ReplacesAttribute" // typeof<Fable.Core.ReplacesAttribute>.FullName
    let [<Literal>] overloadSuffix = "Fable.Core.OverloadSuffixAttribute" // typeof<Fable.Core.OverloadSuffixAttribute>.FullName

[<RequireQualifiedAccess>]
module private Types =
    let [<Literal>] object = "System.Object"
    let [<Literal>] unit = "Microsoft.FSharp.Core.Unit"

// NOTE: These helper functions are (more or less) duplicated from FSharp2Fable.Util
let rec private nonAbbreviatedType (t: FSharpType) =
    if t.IsAbbreviation then nonAbbreviatedType t.AbbreviatedType else t

let private isUnit (typ: FSharpType) =
    let typ = nonAbbreviatedType typ
    if typ.HasTypeDefinition
    then typ.TypeDefinition.TryFullName = Some Types.unit
    else false

let private tryFindAttributeArgs fullName (atts: #seq<FSharpAttribute>) =
    atts |> Seq.tryPick (fun att ->
        match att.AttributeType.TryFullName with
        | Some fullName' ->
            if fullName = fullName'
            then att.ConstructorArguments |> Seq.map snd |> Seq.toList |> Some
            else None
        | None -> None)

// -------- End of helper functions

// Attention: we need to keep this similar to FSharp2Fable.TypeHelpers.makeType
let rec private getTypeFastFullName (genParams: IDictionary<_,_>) (t: FSharpType) =
    let t = nonAbbreviatedType t
    if t.IsGenericParameter then
        match genParams.TryGetValue(t.GenericParameter.Name) with
        | true, i -> i
        | false, _ -> ""
    elif t.IsTupleType
    then t.GenericArguments |> Seq.map (getTypeFastFullName genParams) |> String.concat " * "
    elif t.IsFunctionType
    then t.GenericArguments |> Seq.map (getTypeFastFullName genParams) |> String.concat " -> "
    elif t.IsAnonRecordType then
        Seq.zip t.AnonRecordTypeDetails.SortedFieldNames t.GenericArguments
        |> Seq.map (fun (key, typ) -> key + " : " + getTypeFastFullName genParams typ)
        |> String.concat "; "
    elif t.HasTypeDefinition
    then
        let tdef = t.TypeDefinition
        let genArgs = t.GenericArguments |> Seq.map (getTypeFastFullName genParams) |> String.concat ","
        if tdef.IsArrayType
        then genArgs + "[]"
        elif tdef.IsByRef // Ignore byref
        then genArgs
        else
            match tryFindAttributeArgs Atts.replaces tdef.Attributes with
            | Some [:? string as replacedTypeFullName] ->
                let genArgs = if genArgs = "" then "" else "[" + genArgs + "]"
                replacedTypeFullName + genArgs
            | _ ->
                let genArgs = if genArgs = "" then "" else "[" + genArgs + "]"
                tdef.FullName + genArgs
    else Types.object

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

let private hashToString (i: int) =
    if i < 0
    then "Z" + (abs i).ToString("X")
    else i.ToString("X")

let private getHashPrivate (m: FSharpMemberOrFunctionOrValue) (curriedParams: Params) genParams =
    match tryFindAttributeArgs Atts.overloadSuffix m.Attributes with
    | Some [:? string as overloadSuffix] -> overloadSuffix
    | _ ->
        curriedParams.[0]
        |> Seq.map (fun p -> getTypeFastFullName genParams p.Type |> stringHash)
        |> combineHashCodes
        |> hashToString

let hasEmptyOverloadSuffix (m: FSharpMemberOrFunctionOrValue) (curriedParams: Params) =
    // Overrides and interface implementations don't have override suffix in Fable
    m.IsOverrideOrExplicitInterfaceImplementation
    // Members with curried params cannot be overloaded in F#
    || curriedParams.Count <> 1
    // Don't use overload suffix for members without arguments
    || curriedParams.[0].Count = 0
    || (curriedParams.[0].Count = 1 && isUnit curriedParams.[0].[0].Type)

let getHash (entity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
    let curriedParams = m.CurriedParameterGroups
    if hasEmptyOverloadSuffix m curriedParams
    then ""
    else
        // Generics can have different names in signature
        // and implementation files, use the position instead
        let genParams =
            entity.GenericParameters
            |> Seq.mapi (fun i p -> p.Name, string i)
            |> dict
        getHashPrivate m curriedParams genParams

/// Used for extension members
let getExtensionHash (m: FSharpMemberOrFunctionOrValue) =
    let curriedParams = m.CurriedParameterGroups
    if hasEmptyOverloadSuffix m curriedParams
    then ""
    // Type resolution in extension member seems to be different
    // and doesn't take generics into account
    else dict [] |> getHashPrivate m curriedParams
