// Put the entry point in another module to prevent Fable.Core
// from being loaded ahead of time
module Fable.Client.Node.Entry

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

type MiniCompilerOptions = {
    projFile: string
    msbuild: string list
    symbols: string list
}   

let getProjectOpts (checker: FSharpChecker) (projFile: string) (symbols: string list) (msbuild: string list) =
    let rec addSymbols (symbols: string list) (opts: FSharpProjectOptions) =
        let addSymbols' (otherOpts: string[]) =
            otherOpts
            // |> Array.filter (fun s -> s.StartsWith "--define:" = false)
            |> Array.append (List.map (sprintf "--define:%s") symbols |> List.toArray)
        { opts with
            OtherOptions = addSymbols' opts.OtherOptions
            ReferencedProjects = opts.ReferencedProjects
                |> Array.map (fun (k,v) -> k, addSymbols symbols v) }
    try
        match (Path.GetExtension projFile).ToLower() with
        | ".fsx" ->
            checker.GetProjectOptionsFromScript(
                projFile, File.ReadAllText projFile)
            |> Async.RunSynchronously
        | _ -> // .fsproj
            let props = msbuild |> List.choose (fun x ->
                match x.Split('=') with
                | [|key;value|] -> Some(key,value)
                | _ -> None)
            ProjectCracker.GetProjectOptionsFromProjectFile(projFile, props)
        |> addSymbols symbols
    with
    | ex -> failwithf "Cannot read project options: %s" ex.Message

[<EntryPoint>]
let main argv =
    let opts =
        Seq.pairwise argv
        |> Seq.fold (fun (opts: MiniCompilerOptions) (a1, a2) ->
            match a1 with
            | "--projFile" -> { opts with projFile=a2 }
            | "--symbols" -> { opts with symbols=a2::opts.symbols }
            | "--msbuild" -> { opts with msbuild=a2::opts.msbuild }
            | _ -> opts) { projFile=""; symbols=[]; msbuild=[] }
    let checker =
        FSharpChecker.Create(keepAssemblyContents=true)
    let projectOpts =
        getProjectOpts checker opts.projFile opts.symbols opts.msbuild
    projectOpts.OtherOptions
    |> Seq.filter (fun x -> x.StartsWith("-r:"))
    |> Seq.tryPick (fun opt ->
        if opt.StartsWith("-r:")
            && Path.GetFileName(opt.Substring 3) = "Fable.Core.dll"
        then Path.GetFullPath(opt.Substring 3) |> Some
        else None)
    |> Option.bind (fun path ->
        // Use Fable.Core.dll referenced by the project to be compiled
        let fableCoreAsm = Assembly.LoadFrom(Path.GetFullPath path)
        let currentAsm = Assembly.GetExecutingAssembly()
        currentAsm.GetCustomAttributes(typeof<AssemblyMetadataAttribute>, false)
        |> Seq.tryPick (fun att ->
            let att = att :?> AssemblyMetadataAttribute
            if att.Key = "minimumFableCoreVersion"
            then (fableCoreAsm.GetName().Version, Version att.Value) |> Some
            else None))
    |> Option.iter (fun (coreVersion, minVersion) ->
        if coreVersion < minVersion then
            failwithf "Fable.Core %O required, please updgrade the project reference"
                        minVersion)
    Main.start argv checker getProjectOpts projectOpts
    0
