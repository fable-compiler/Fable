module Fable.Transforms.Inject

open System
open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Transforms
open FSharp2Fable.Util
open FSharp2Fable.Helpers
open FSharp2Fable.Patterns
open FSharp2Fable.TypeHelpers

let private fullName (ent: FSharpEntity) (genArgs: Fable.Type list) =
    Fable.DeclaredType(ent, genArgs) |> getTypeFullName

let private resolveParamGeneric com (genArg: Lazy<(string * Fable.Type) list>) (parGenArg: FSharpType) =
    if parGenArg.IsGenericParameter then
        let genParamName = parGenArg.GenericParameter.Name
        genArg.Value
        |> List.tryPick (fun (k,v) -> if k = genParamName then Some v else None)
        // We could add an error here if not found, but it will be added anyways
        // when trying to compile TypeInfo for a generic in Fable2Babel
        |> Option.defaultValue (Fable.GenericParam genParamName)
    else makeType com Map.empty parGenArg

let (|TryDefinition|_|) (NonAbbreviatedType t) =
    if t.HasTypeDefinition
    then Some(t.TypeDefinition, t.GenericArguments)
    else None

let (|Implicit|_|) com r enclosingEntity genArgs (par: FSharpParameter) (typDef: FSharpEntity, paramGen: IList<FSharpType>) =
    if hasAttribute Atts.implicit par.Attributes then
        let genArgs = paramGen |> Seq.map (fun g -> resolveParamGeneric com genArgs g) |> Seq.toList
        let fail msg =
            let msg = if String.IsNullOrEmpty(msg) then "Cannot find {0} in enclosing scope" else msg
            let msg = String.Format(msg, "implicit value with type " + fullName typDef genArgs)
            addErrorAndReturnNull com r msg |> Some
        match enclosingEntity with
        | None -> fail ""
        | Some (enclosingEntity: FSharpEntity) ->
            let candidates =
                enclosingEntity.MembersFunctionsAndValues
                |> Seq.choose (fun m ->
                    if hasAttribute Atts.implicit m.Attributes then
                        match m.FullTypeSafe with
                        | Some(TryDefinition(typDef2, genArgs2))
                            when typDef = typDef2
                                && Seq.forall2 (fun g1 g2 ->
                                    g1 = (makeType com Map.empty g2)) genArgs genArgs2
                            -> Some m
                        | _ -> None
                    else None)
                |> Seq.toList
            match candidates with
            | [] -> fail ""
            | [m] when m.IsMutable -> fail "Found {0} but it's mutable"
            | [implicitValue] ->
                let typ = Fable.DeclaredType(typDef, genArgs)
                memberRefTyped com r typ implicitValue |> Some
            | _ -> fail "Found more than one {0}, please disambiguate"
    else None

let (|TypeResolver|_|) com r genArgs (typDef: FSharpEntity, paramGen: IList<FSharpType>) =
    if typDef.TryFullName = Some Types.typeResolver then
        let f = Fable.TypeInfo(resolveParamGeneric com genArgs paramGen.[0], r) |> Fable.Value |> makeDelegate []
        let m = Fable.ObjectMember(makeStrConst "ResolveType", f, Fable.ObjectValue)
        Fable.ObjectExpr([m], Fable.Any, None) |> Some
    else None

let injectArg com enclosingEntity (genArgs: Lazy<(string * Fable.Type) list>) (par: FSharpParameter): Fable.Expr =
    let parType = nonAbbreviatedType par.Type
    let r = makeRange par.DeclarationLocation |> Some
    let typDefAndGenArgs =
        // The type of the parameter must be an option
        if parType.HasTypeDefinition && parType.TypeDefinition.TryFullName = Some Types.option then
            let typ = parType.GenericArguments.[0]
            if typ.HasTypeDefinition then Some(typ.TypeDefinition, typ.GenericArguments) else None
        else None
    match typDefAndGenArgs with
    | Some(Implicit com r enclosingEntity genArgs par e) -> e
    | Some(TypeResolver com r genArgs e) -> e
    | _ ->
        typDefAndGenArgs
        |> Option.bind (fun (x,_) -> x.TryFullName)
        |> Option.defaultValue Naming.unknown
        |> sprintf "Cannot inject argument of type %s"
        |> addErrorAndReturnNull com r
