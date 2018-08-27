[<RequireQualifiedAccess>]
module Fable.Transforms.OverloadSuffix

open Microsoft.FSharp.Compiler.SourceCodeServices

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
let rec private getTypeFastFullName genParams (t: FSharpType) =
    let t = nonAbbreviatedType t
    if t.IsGenericParameter
    // Generics can have different names in signature and implementation files, use the position
    then
        let name = t.GenericParameter.Name
        List.findIndex ((=) name) genParams |> string
    elif t.IsTupleType
    then t.GenericArguments |> Seq.map (getTypeFastFullName genParams) |> String.concat " * "
    elif t.IsFunctionType
    then t.GenericArguments |> Seq.map (getTypeFastFullName genParams) |> String.concat " -> "
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
// Taken from fable-core/Util.ts. Possible variant in https://stackoverflow.com/a/1660613
let private stringHash (s: string) =
    let mutable h = 5381
    for i = 0 to s.Length - 1 do
        h <- (h * 33) ^^^ (int s.[i])
    h

let private hashToString (i: int) =
    if i < 0
    then "Z" + (abs i).ToString("X")
    else i.ToString("X")

let private getGenParams (entity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
    // It seems that for F# types memb.GenericParameters contains all generics
    // but for BCL types we need to check the DeclaringEntity generics too
    let rec skipEntGenParams = function
        | head1::tail1, (head2::tail2 as li2) ->
            if head1 = head2
            then skipEntGenParams (tail1, tail2)
            else li2
        | [], li2 -> li2
        | _, [] -> []
    let entGenParams = entity.GenericParameters |> Seq.map (fun p -> p.Name) |> Seq.toList
    let membGenParams = m.GenericParameters |> Seq.map (fun p -> p.Name) |> Seq.toList
    entGenParams @ skipEntGenParams (entGenParams, membGenParams)

let getHash (entity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
    let curriedParams = m.CurriedParameterGroups
        // Overrides and interface implementations don't have override suffix in Fable
    if m.IsOverrideOrExplicitInterfaceImplementation
        // Members with curried params cannot be overloaded in F#
        || curriedParams.Count <> 1
        // Don't use overload suffix for members without arguments
        || curriedParams.[0].Count = 0
        || (curriedParams.[0].Count = 1 && isUnit curriedParams.[0].[0].Type)
    then ""
    else
        match tryFindAttributeArgs Atts.overloadSuffix m.Attributes with
        | Some [:? string as overloadSuffix] -> overloadSuffix
        | _ ->
            let genParams = getGenParams entity m
            curriedParams.[0]
            |> Seq.map (fun p -> getTypeFastFullName genParams p.Type |> stringHash)
            |> combineHashCodes
            |> hashToString
