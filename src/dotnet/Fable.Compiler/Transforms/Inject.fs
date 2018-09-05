module Fable.Transforms.Inject

open System
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Transforms
open FSharp2Fable.Util
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

let (|Implicit|_|) com (ctx: FSharp2Fable.Context) r (par: FSharpParameter) typ =
    if hasAttribute Atts.implicit par.Attributes then
        let fail msg =
            let msg = if String.IsNullOrEmpty(msg) then "Cannot find {0} in enclosing module" else msg
            let msg = String.Format(msg, "implicit for `" + par.DisplayName + "` (" + getTypeFullName true typ + ")")
            addErrorAndReturnNull com ctx.InlinePath r msg |> Some
        match ctx.EnclosingEntity with
        | None -> fail ""
        | Some (enclosingEntity: FSharpEntity) ->
            let candidates =
                enclosingEntity.MembersFunctionsAndValues
                |> Seq.choose (fun m ->
                    if hasAttribute Atts.implicit m.Attributes then
                        m.FullTypeSafe |> Option.bind (fun typ2 ->
                            let typ2 = makeType com Map.empty typ2
                            if typeEquals true typ typ2 then Some m else None)
                    else None)
                |> Seq.toList
            match candidates with
            | [] -> fail ""
            | [m] when m.IsMutable -> fail "Found {0} but it's mutable"
            | [implicitValue] ->
                let e = memberRefTyped com typ implicitValue
                match typ with  // Wrap lambda values
                | Fable.FunctionType(Fable.LambdaType _, Fable.FunctionType(Fable.LambdaType _, _)) ->
                    let paramsCount = implicitValue.CurriedParameterGroups.Count
                    if paramsCount > 0 then
                        let args = [for i=1 to paramsCount do yield makeIdent ("arg" + string i)]
                        let argInfo = argInfo None (List.map Fable.IdentExpr args) Fable.NoUncurrying
                        staticCall None Fable.Any argInfo e |> makeLambda args |> Some
                    else Some e
                | _ -> Some e
            | _ -> fail "Found more than one {0}, please disambiguate"
    else None

let (|TypeResolver|_|) r = function
    | Fable.DeclaredType(typDef,[t]) when typDef.TryFullName = Some Types.typeResolver ->
        let f = Fable.TypeInfo(t, r) |> Fable.Value |> makeDelegate []
        let m = Fable.ObjectMember(makeStrConst "ResolveType", f, Fable.ObjectValue)
        Fable.ObjectExpr([m], Fable.Any, None) |> Some
    | _ -> None

let injectArg com ctx r (genArgs: (string * Fable.Type) list) (par: FSharpParameter): Fable.Expr =
    let parType = nonAbbreviatedType par.Type
    let typ =
        // The type of the parameter must be an option
        if parType.HasTypeDefinition && parType.TypeDefinition.TryFullName = Some Types.option
        then makeType com (Map genArgs) parType.GenericArguments.[0] |> Some
        else None
    match typ with
    | Some(Implicit com ctx r par e) -> e
    | Some(TypeResolver r e) -> e
    | _ ->
        match typ with
        | Some typ -> getTypeFullName true typ
        | None -> string parType
        |> sprintf "Cannot inject argument %s of type %s" par.DisplayName
        |> addErrorAndReturnNull com ctx.InlinePath r
