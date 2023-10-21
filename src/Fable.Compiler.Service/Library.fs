module Fable.Compiler.Service.Compiler

open System
open System.Text
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text
open Fable
open Fable.Compiler.Service.Util
open Fable.Transforms.State
open Fable.Transforms
open Fable.Compiler.Service.ProjectCracker

type BabelWriter
    (
        com: Compiler,
        pathResolver: PathResolver,
        projectFile: string,
        sourcePath: string,
        targetPath: string
    ) =
    // In imports *.ts extensions have to be converted to *.js extensions instead
    // TODO: incomplete
    let fileExt = ".js"
    let sourceDir = Path.GetDirectoryName(sourcePath)
    let targetDir = Path.GetDirectoryName(targetPath)
    let memoryStream = new IO.MemoryStream()
    let stream = new IO.StreamWriter(memoryStream)

    let sb = StringBuilder()
    // let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))

    override x.ToString() =
        sb.ToString()

    interface Printer.Writer with
        // Don't dispose the stream here because we need to access the memory stream to check if file has changed
        member _.Dispose() = ()

        member _.Write(str) =
            sb.Append(str) |> ignore
            stream.WriteAsync(str) |> Async.AwaitTask

        member _.MakeImportPath(path) =
            let projDir = IO.Path.GetDirectoryName(projectFile)

            let path =
                // TODO: Check precompiled out path for other languages too
                match pathResolver.TryPrecompiledOutPath(sourceDir, path) with
                | Some path -> Imports.getRelativePath sourceDir path
                | None -> path

            // TODO: used to be cliArgs.outDir, could be wrong.
            let path =
                Imports.getImportPath
                    pathResolver
                    sourcePath
                    targetPath
                    projDir
                    (Some targetDir)
                    path

            if path.EndsWith(".fs") then
                let isInFableModules =
                    Path.Combine(targetDir, path) |> Naming.isInFableModules

                File.changeExtensionButUseDefaultExtensionInFableModules
                    JavaScript
                    isInFableModules
                    path
                    fileExt
            else
                path

        member _.AddLog(msg, severity, ?range) =
            com.AddLog(
                msg,
                severity,
                ?range = range,
                fileName = com.CurrentFile
            )

        member _.AddSourceMapping
            (
                srcLine,
                srcCol,
                genLine,
                genCol,
                file,
                displayName
            ) =
            //
            ()
//     if cliArgs.SourceMaps then
//         let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
//         let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
//         let targetPath = Path.normalizeFullPath targetPath
//         let sourcePath = defaultArg file sourcePath |> Path.getRelativeFileOrDirPath false targetPath false
//         mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=displayName)

let mkCompilerForFile
    (cliArgs: CliArgs)
    (crackerResponse: CrackerResponse)
    (currentFile: string)
    : Async<Compiler> =
    async {
        let checker = InteractiveChecker.Create(crackerResponse.ProjectOptions)
        let! assemblies = checker.GetImportedAssemblies()

        let sourceReader _ =
            let source =
                Array.last crackerResponse.ProjectOptions.SourceFiles
                |> System.IO.File.ReadAllText

            1, lazy source

        let! checkProjectResult =
            checker.ParseAndCheckProject(
                cliArgs.ProjectFile,
                crackerResponse.ProjectOptions.SourceFiles,
                sourceReader,
                Array.last crackerResponse.ProjectOptions.SourceFiles
            )

        ignore checkProjectResult.Diagnostics

        let fableProj =
            Project.From(
                cliArgs.ProjectFile,
                crackerResponse.ProjectOptions.SourceFiles,
                checkProjectResult.AssemblyContents.ImplementationFiles,
                assemblies
            // ?precompiledInfo =
            //     (projCracked.PrecompiledInfo |> Option.map (fun i -> i :> _)),
            // getPlugin = loadType projCracked.CliArgs
            )

        let opts = cliArgs.CompilerOptions

        let fableLibDir =
            Path.getRelativePath currentFile crackerResponse.FableLibDir

        let watchDependencies = None
        // if cliArgs.IsWatch then Some(HashSet()) else None

        return
            CompilerImpl(
                currentFile,
                fableProj,
                opts,
                fableLibDir,
                crackerResponse.OutputType,
                ?outDir = cliArgs.OutDir,
                ?watchDependencies = watchDependencies
            )
    }

let compileFile (com: Compiler) (pathResolver: PathResolver) (outPath: string) =
    async {
        let babel =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Babel.Compiler.transformFile com

        use writer =
            new BabelWriter(
                com,
                pathResolver,
                com.ProjectFile,
                com.CurrentFile,
                outPath
            )

        do! BabelPrinter.run writer babel
        return writer.ToString()
    }
