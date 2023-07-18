[<RequireQualifiedAccess>]
module rec Fable.Transforms.OverloadSuffix

open Fable
open System.Collections.Generic
open FSharp.Compiler.Symbols

type ParamTypes = FSharpType list

[<RequireQualifiedAccess>]
module private Atts =
    let [<Literal>] noOverloadSuffix = "Fable.Core.NoOverloadSuffixAttribute" // typeof<Fable.Core.OverloadSuffixAttribute>.FullName

[<RequireQualifiedAccess>]
module private Types =
    let [<Literal>] object = "System.Object"
    let [<Literal>] unit = "Microsoft.FSharp.Core.Unit"

// NOTE: These helper functions are (more or less) duplicated from FSharp2Fable.Util
let rec nonAbbreviatedType (t: FSharpType): FSharpType =
    let isSameType (t1: FSharpType) (t2: FSharpType) =
        t1.HasTypeDefinition && t2.HasTypeDefinition && (t1.TypeDefinition = t2.TypeDefinition)
    if t.IsAbbreviation && not (isSameType t t.AbbreviatedType) then
        nonAbbreviatedType t.AbbreviatedType
    elif t.HasTypeDefinition then
        let abbr = t.AbbreviatedType
        // .IsAbbreviation doesn't eval to true for generic numbers
        // See https://github.com/Microsoft/visualfsharp/issues/5992
        if t.GenericArguments.Count = abbr.GenericArguments.Count then t
        else abbr
    else t

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

let private hashToString (i: int) =
    if i < 0
    then "Z" + (abs i).ToString("X")
    else i.ToString("X")

// Not perfect but hopefully covers most of the cases
// Using only common constrains from https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/constraints
let private getGenericParamConstrainsHash genParams (p: FSharpGenericParameter) =
    let getConstrainHash (c: FSharpGenericParameterConstraint) =
        if c.IsCoercesToConstraint then
            ":>" + getTypeFastFullName genParams c.CoercesToTarget
        elif c.IsSupportsNullConstraint then
            "null"
        elif c.IsMemberConstraint then
            let d = c.MemberConstraintData // TODO: Full member signature hash?
            (if d.MemberIsStatic then "static " else "") + "member " + d.MemberName
        elif c.IsRequiresDefaultConstructorConstraint then
            "new"
        elif c.IsNonNullableValueTypeConstraint then
            "struct"
        elif c.IsReferenceTypeConstraint then
            "not struct"
        elif c.IsComparisonConstraint then
            "comparison"
        elif c.IsEqualityConstraint then
            "equality"
        elif c.IsUnmanagedConstraint then
            "unmanaged"
        elif c.IsEnumConstraint then
            "enum"
        else ""
    p.Constraints |> Seq.map getConstrainHash |> String.concat ","

// Attention: we need to keep this similar to FSharp2Fable.TypeHelpers.makeType
let rec private getTypeFastFullName (genParams: IDictionary<_,_>) (t: FSharpType) =
    let t = nonAbbreviatedType t
    if t.IsGenericParameter then
        if t.GenericParameter.IsMeasure then "measure"
        else
            match genParams.TryGetValue(t.GenericParameter.Name) with
            | true, i -> i
            | false, _ -> getGenericParamConstrainsHash genParams t.GenericParameter
    elif t.IsTupleType then
        let genArgs = t.GenericArguments |> Seq.map (getTypeFastFullName genParams) |> String.concat " * "
        if t.IsStructTupleType then "struct " + genArgs
        else genArgs
    elif t.IsFunctionType then
        t.GenericArguments
        |> Seq.map (getTypeFastFullName genParams)
        |> String.concat " -> "
    elif t.IsAnonRecordType then
        Seq.zip t.AnonRecordTypeDetails.SortedFieldNames t.GenericArguments
        |> Seq.map (fun (key, typ) -> key + " : " + getTypeFastFullName genParams typ)
        |> String.concat "; "
    elif t.HasTypeDefinition then
        let tdef = t.TypeDefinition
        let genArgs = t.GenericArguments |> Seq.mapToList (getTypeFastFullName genParams)
        if tdef.IsArrayType then String.concat "," genArgs + "[]"
        // elif tdef.IsByRef then genArgs // Ignore byref
        else
            let fullName = defaultArg tdef.TryFullName tdef.LogicalName
            // Not sure why, but when precompiling F# changes measure types to MeasureProduct<'M, MeasureOne>
            match fullName, genArgs with
            | "Microsoft.FSharp.Core.CompilerServices.MeasureProduct`2",
                [measure; "Microsoft.FSharp.Core.CompilerServices.MeasureOne"] -> measure
            | _ ->
                let genArgs = String.concat "," genArgs
                let genArgs = if genArgs = "" then "" else "[" + genArgs + "]"
                fullName + genArgs
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

let private getHashPrivate (entAtts: IList<FSharpAttribute>) (paramTypes: ParamTypes) genParams =
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
    | [argType] when isUnit argType -> true
    | _ -> false

let getHash (entity: FSharpEntity) (m: FSharpMemberOrFunctionOrValue) =
    // Members with curried params cannot be overloaded in F#
    // TODO: Also private methods defined with `let` cannot be overloaded
    // but I don't know how to identify them in the AST
    if m.CurriedParameterGroups.Count <> 1 then ""
    else
        let paramTypes = m.CurriedParameterGroups.[0] |> Seq.map (fun p -> p.Type) |> Seq.toList
        let paramTypes =
            if m.CompiledName = "op_Implicit" then paramTypes @ [m.ReturnParameter.Type]
            else paramTypes
        if hasEmptyOverloadSuffix paramTypes then ""
        else
            // Generics can have different names in signature
            // and implementation files, use the position instead
            let genParams =
                entity.GenericParameters
                |> Seq.mapi (fun i p -> p.Name, string i)
                |> dict
            getHashPrivate entity.Attributes paramTypes genParams

let getAbstractSignatureHash (entity: FSharpEntity) (m: FSharpAbstractSignature) =
    // Members with curried params cannot be overloaded in F#
    if m.AbstractArguments.Count <> 1 then ""
    else
        let paramTypes = m.AbstractArguments.[0] |> Seq.map (fun p -> p.Type) |> Seq.toList
        if hasEmptyOverloadSuffix paramTypes then ""
        else
            // Generics can have different names in signature
            // and implementation, use the position instead
            let genParams =
                entity.GenericParameters
                |> Seq.mapi (fun i p -> p.Name, string i)
                |> dict
            getHashPrivate entity.Attributes paramTypes genParams

/// Used for extension members
let getExtensionHash (m: FSharpMemberOrFunctionOrValue) =
    // Members with curried params cannot be overloaded in F#
    if m.CurriedParameterGroups.Count <> 1 then ""
    else
        let paramTypes = m.CurriedParameterGroups.[0] |> Seq.map (fun p -> p.Type) |> Seq.toList
        if hasEmptyOverloadSuffix paramTypes then ""
        else
            // Type resolution in extension member seems to be different
            // and doesn't take generics into account
            dict [] |> getHashPrivate [||] paramTypes
