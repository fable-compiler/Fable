module InjectProcessor

open System
open System.IO
open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

let typeAliases =
    Map
        [
            "System.Collections.Generic.IComparer`1", "icomparerGeneric"
            "System.Collections.Generic.IEqualityComparer`1",
            "iequalityComparerGeneric"
            "Native.Cons`1", "arrayCons"
        ]

let parse (checker: FSharpChecker) projFile =
    let projFile = Path.GetFullPath(projFile)

    let options =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile

            checker.GetProjectOptionsFromScript(
                projFile,
                projCode |> FSharp.Compiler.Text.SourceText.ofString
            )
            |> Async.RunSynchronously
            |> fst
        | ".fsproj" ->
            let opts, _, _ =
                Fable.Cli.ProjectCoreCracker.GetProjectOptionsFromProjectFile
                    "Release"
                    projFile

            opts
        | ext -> failwithf "Unexpected extension: %s" ext
    // for f in options.OtherOptions do
    //     printfn "%s" f
    options |> checker.ParseAndCheckProject |> Async.RunSynchronously


let (|InjectAttribute|_|) (arg: FSharpParameter) =
    arg.Attributes
    |> Seq.tryPick (fun att ->
        match att.AttributeType.TryFullName with
        | Some "Fable.Core.InjectAttribute" when arg.Type.HasTypeDefinition ->
            match
                arg.Type.TypeDefinition.TryFullName,
                Seq.toList arg.Type.GenericArguments
            with
            | Some typeArgName, [ genArg ] ->
                Some(typeArgName, genArg.GenericParameter.Name)
            | _ -> None
        | _ -> None
    )

let rec getInjects initialized decls =
    let processInfo
        (memb: FSharpMemberOrFunctionOrValue)
        (typeArgName)
        (genArg)
        =
        let genArgIndex =
            memb.GenericParameters |> Seq.findIndex (fun p -> p.Name = genArg)

        typeArgName, genArgIndex

    seq {
        for decl in decls do
            match decl with
            | FSharpImplementationFileDeclaration.Entity(_, sub) ->
                // If the entity contains multiple declarations it must be the root module
                if not initialized then
                    yield! getInjects (Fable.List.isMultiple sub) sub
            | FSharpImplementationFileDeclaration.InitAction _ -> ()
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb,
                                                                          _,
                                                                          _) ->
                let _, injections =
                    (Seq.concat memb.CurriedParameterGroups, (false, []))
                    ||> Seq.foldBack (fun arg (finished, acc) ->
                        match finished, arg with
                        | false, InjectAttribute(typeArg, genArg) ->
                            false, (processInfo memb typeArg genArg) :: acc
                        | _ -> true, acc
                    )

                match injections with
                | [] -> ()
                | injections ->
                    let membName =
                        match memb.DeclaringEntity with
                        | Some ent when not ent.IsFSharpModule ->
                            let suffix =
                                Fable.Transforms.FSharp2Fable.Helpers.getOverloadSuffixFrom
                                    ent
                                    memb

                            Fable.Naming.buildNameWithoutSanitationFrom
                                ent.CompiledName
                                (not memb.IsInstanceMember)
                                memb.CompiledName
                                suffix
                        | _ -> memb.CompiledName

                    yield membName, injections
    }

[<EntryPoint>]
let main _argv =
    printfn
        "Checking methods in Fable.Library with last argument decorated with Inject..."

    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let proj =
        parse
            checker
            (IO.Path.Combine(
                __SOURCE_DIRECTORY__,
                "../../fable-library/Fable.Library.fsproj"
            ))

    let lines =
        seq {
            yield
                """/// AUTOMATICALLY GENERATED - DO NOT TOUCH!
module Fable.Transforms.ReplacementsInject

let fableReplacementsModules =
  Map ["""

            for file in proj.AssemblyContents.ImplementationFiles do
                let fileName =
                    System.IO.Path.GetFileNameWithoutExtension(file.FileName)
                // Apparently FCS generates the AssemblyInfo file automatically
                if fileName.Contains("AssemblyInfo") |> not then
                    let moduleInjects =
                        getInjects false file.Declarations
                        |> Seq.map (fun (membName, infos) ->
                            infos
                            |> List.map (fun (typeArgName, genArgIndex) ->
                                let typeArgName =
                                    match
                                        Map.tryFind typeArgName typeAliases
                                    with
                                    | Some alias -> "Types." + alias
                                    | None -> "\"" + typeArgName + "\""

                                sprintf "(%s, %i)" typeArgName genArgIndex
                            )
                            |> String.concat "; "
                            |> sprintf "      \"%s\", %s" membName
                        )
                        |> Seq.toArray

                    if moduleInjects.Length > 0 then
                        yield sprintf "    \"%s\", Map [" fileName
                        yield! moduleInjects
                        yield "    ]"

            yield "  ]\n"
        }

    File.WriteAllLines(
        IO.Path.Combine(
            __SOURCE_DIRECTORY__,
            "../../Fable.Transforms/ReplacementsInject.fs"
        ),
        lines
    )

    printfn "Finished!"
    0
