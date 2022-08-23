module Fable.Cli.ProjectCoreCracker

open System.IO

open Fable
open Fable.AST

open Ionide.ProjInfo
open Ionide.ProjInfo.Types

// This gets the job done -- more data/fields can be added as needed.
// Otherwise Ionide.ProjInfo.FCS could be useful, as this directly gives a FSharpProjectOptions
type ProjectInfo =
    { OtherOptions: string list
      ReferencedProjects: string list
      SourceFiles: string list
      OutputType: OutputType option }

let private failWithFailedToLoadMessage = function
    | WorkspaceProjectState.Failed (_project, reason) ->
        // TODO see if the error was due to a locked file and
        // raise a different exception to be caught by retryGetCrackedProjects
        let projFile, message =
            match reason with
            | GenericError (projFile, msg) -> projFile, $"'{msg}'"
            // these two cases are checked beforehand by Fable (except when --noRestore is given)
            | ProjectNotRestored projFile -> projFile, "the project is not restored."
            | ProjectNotFound projFile -> projFile, "the project file was not found."
            // it seems like the following cases are currently unused by Ionide.ProjInfo
            | LanguageNotSupported projFile -> projFile, "the project language is not supported."
            | ProjectNotLoaded projFile -> projFile, "the project is not loaded."
            | MissingExtraProjectInfos projFile -> projFile, "there is missing extra info."
            | InvalidExtraProjectInfos (projFile, error) -> projFile, $"there is invalid extra info: '{error}'."
            | ReferencesNotLoaded (projFile, referenceErrors) ->
                let refs = referenceErrors |> Seq.map fst |> String.concat ", "
                projFile, $"the following references are not loaded/have errors: {refs}"
        Fable.FableError $"Failed to load project '{projFile}' because {message}" |> raise
    | _ -> ()

let private projInfo additionalMSBuildProps (projFile: string) =
    let toolsPath = Init.init (DirectoryInfo <| Path.GetDirectoryName projFile) None
    let loader = WorkspaceLoader.Create(toolsPath, additionalMSBuildProps)
    loader.Notifications.Add failWithFailedToLoadMessage
    let proj = loader.LoadProjects [projFile] |> Seq.head

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
        | Library -> Some OutputType.Library
        | Exe -> Some OutputType.Exe
        | Custom _ -> None // what to do here? defaults to library later

    // All paths including ones in OtherOptions are absolute but not normalized?
    { OtherOptions = proj.OtherOptions
      ReferencedProjects = projRefs
      SourceFiles = proj.SourceFiles
      OutputType = outputType }

let GetProjectOptionsFromProjectFile configuration (projFile : string) =
    projInfo ["Configuration", configuration] projFile
