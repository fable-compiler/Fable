/// Adapted from https://github.com/fsharp/FsAutoComplete/blob/45bf4a7255f8856b0164f722a82a17108ae64981/src/FsAutoComplete.Core/ProjectCoreCracker.fs
module Fable.Cli.ProjectCoreCracker

open System
open System.IO

open FSharp.Compiler.CodeAnalysis
open Ionide.ProjInfo

let private projInfo additionalMSBuildProps (file: string) =
    let projDir = Path.GetDirectoryName file

    // The following 3 calls may? throw, any need to reformat/reraise errors?
    // - Errors here are caught by retryGetCrackedProjects and the msg/error is lost.
    let toolsPath = Init.init (DirectoryInfo projDir) None
    let loader = WorkspaceLoader.Create(toolsPath, additionalMSBuildProps)
    let proj = loader.LoadProjects ([file], ["IsCrossTargetingBuild"], BinaryLogGeneration.Off) |> Seq.head

    let isCrossTargeting =
        proj.CustomProperties |> List.tryPick (fun p ->
            if p.Name = "IsCrossTargetingBuild"
            then Some (System.Boolean.Parse p.Value)
            else None)
        |> Option.defaultValue false // property may not be present even if requested?

    let proj =
        if isCrossTargeting then
            match proj.ProjectSdkInfo.TargetFrameworks with
            | [] -> failwithf "Unexpected, found cross targeting but empty target frameworks list"
            | (first :: _) ->
                // Atm setting a preferenece is not supported in FSAC
                // As workaround, lets choose the first of the target frameworks and use that
                let loader = WorkspaceLoader.Create(toolsPath, ("TargetFramework", first) :: additionalMSBuildProps)
                loader.LoadProjects ([file], [], BinaryLogGeneration.Off) |> Seq.head
        else
            proj

    //TODO cache projects info of p2p ref
    //   let p2pProjects =
    //       p2ps
    //       // do not follow others lang project, is not supported by FCS anyway
    //       |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
    //       |> List.map (fun p2p -> p2p.ProjectReferenceFullPath |> projInfo ["TargetFramework", p2p.TargetFramework] )

    let projOptions: FSharpProjectOptions =
        {
            ProjectId = None
            ProjectFileName = file
            SourceFiles = [||]
            OtherOptions = proj.OtherOptions |> Array.ofList
            ReferencedProjects = [||] //p2pProjects |> Array.ofList
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.Now
            UnresolvedReferences = None;
            OriginalLoadReferences = []
            Stamp = None
        }

    let projRefs = proj.ReferencedProjects |> List.map (fun p2p -> p2p.ProjectFileName)

    projOptions, proj.SourceFiles, projRefs

let GetProjectOptionsFromProjectFile configuration (file : string) =
    projInfo ["Configuration", configuration] file
