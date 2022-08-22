module Fable.Cli.ProjectCoreCracker

open System.IO

open Fable

open Ionide.ProjInfo

// This gets the job done -- more data/fields can be added as needed.
// Otherwise Ionide.ProjInfo.FCS could be useful, as this directly gives a FSharpProjectOptions
type ProjectInfo =
    { OtherOptions: string list
      ReferencedProjects: string list
      SourceFiles: string list
      OutputType: OutputType option }

let private projInfo additionalMSBuildProps (projFile: string) =
    // The following 3 calls may? throw, any need to reformat/reraise errors?
    // - IO errors from here are caught by retryGetCrackedProjects and the msg/error is lost (until the retry limit is reached).
    let toolsPath = Init.init (DirectoryInfo <| Path.GetDirectoryName projFile) None
    let loader = WorkspaceLoader.Create(toolsPath, additionalMSBuildProps)
    let proj = loader.LoadProjects ([projFile], ["IsCrossTargetingBuild"], BinaryLogGeneration.Off) |> Seq.head

    let isCrossTargeting =
        proj.CustomProperties |> List.tryPick (fun p ->
            if p.Name = "IsCrossTargetingBuild"
            then Some (System.Boolean.Parse p.Value)
            else None)
        |> Option.defaultValue false // property is never present even if requested?

    let proj =
        if isCrossTargeting then
            match proj.ProjectSdkInfo.TargetFrameworks with
            | [] -> failwithf "Unexpected, found cross targeting but empty target frameworks list"
            | (first :: _) ->
                // Atm setting a preferenece is not supported in FSAC
                // As workaround, lets choose the first of the target frameworks and use that
                let loader = WorkspaceLoader.Create(toolsPath, ("TargetFramework", first) :: additionalMSBuildProps)
                loader.LoadProjects ([projFile], [], BinaryLogGeneration.Off) |> Seq.head
        else
            proj

    // TODO cache projects info of p2p ref
    //   let p2pProjects =
    //       p2ps
    //       // do not follow others lang project, is not supported by FCS anyway
    //       |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
    //       |> List.map (fun p2p -> p2p.ProjectReferenceFullPath |> projInfo ["TargetFramework", p2p.TargetFramework] )

    // p2p.ProjectFileName is absolute, p2p.RelativePath is relative
    let projRefs = proj.ReferencedProjects |> List.map (fun p2p -> p2p.ProjectFileName)

    let outputType =
        match proj.ProjectOutputType with
        | Types.Library -> Some OutputType.Library
        | Types.Exe -> Some OutputType.Exe
        | Types.Custom _ -> None // what to do here? defaults to library later

    // All paths including ones in OtherOptions are absolute but not normalized?
    { OtherOptions = proj.OtherOptions
      ReferencedProjects = projRefs
      SourceFiles = proj.SourceFiles
      OutputType = outputType }

let GetProjectOptionsFromProjectFile configuration (projFile : string) =
    projInfo ["Configuration", configuration] projFile
