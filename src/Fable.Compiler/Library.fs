namespace Fable.Compiler

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Diagnostics
open Fable
open Fable.Transforms.State
open Fable.Transforms
open Fable.Compiler.ProjectCracker
open Fable.Compiler.Util

type TypeCheckProjectResult =
    {
        Assemblies: FSharp.Compiler.Symbols.FSharpAssembly list
        ProjectCheckResults: FSharpCheckProjectResults
    }

type CompileResult =
    {
        /// A map of absolute file path to transpiled JavaScript code
        CompiledFiles: Map<string, string>
        /// Diagnostics of the entire checked F# project
        Diagnostics: FSharpDiagnostic array
    }

type BabelWriter
    (
        com: Compiler,
        pathResolver: PathResolver,
        projectFile: string,
        sourcePath: string,
        targetPath: string,
        ?fileExt: string
    )
    =
    // In imports *.ts extensions have to be converted to *.js extensions instead
    // TODO: incomplete
    let fileExt = Option.defaultValue ".js" fileExt
    let sourceDir = Path.GetDirectoryName(sourcePath)
    let targetDir = Path.GetDirectoryName(targetPath)
    let memoryStream = new MemoryStream()
    let streamWriter = new StreamWriter(memoryStream)
    do streamWriter.NewLine <- "\n"

    // let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))

    member x.ReadContentAsString() : Async<string> =
        async {
            do! streamWriter.FlushAsync() |> Async.AwaitTask
            memoryStream.Position <- 0L
            let streamReader = new StreamReader(memoryStream)
            return! (streamReader.ReadToEndAsync() |> Async.AwaitTask)
        }

    interface Printer.Writer with
        // Don't dispose the stream here because we need to access the memory stream to check if file has changed
        member _.Dispose() = ()

        member _.Write(str) =
            streamWriter.WriteAsync(str) |> Async.AwaitTask

        member _.MakeImportPath(path) =
            let projDir = Path.GetDirectoryName(projectFile)

            let path =
                // TODO: Check precompiled out path for other languages too
                match pathResolver.TryPrecompiledOutPath(sourceDir, path) with
                | Some path -> Imports.getRelativePath sourceDir path
                | None -> path

            // TODO: used to be cliArgs.outDir, could be wrong.
            let path =
                Imports.getImportPath pathResolver sourcePath targetPath projDir (Some targetDir) path

            // Here, we deviate from what the regular compiler does as we are never writing to disk.
            // We don't have the original file name collision problem.
            if path.EndsWith(".fs", StringComparison.Ordinal) then
                Fable.Path.ChangeExtension(path, fileExt)
            else
                path

        member _.AddLog(msg, severity, ?range) =
            com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

        member _.AddSourceMapping(srcLine, srcCol, genLine, genCol, file, displayName) =
            //
            ()
//     if cliArgs.SourceMaps then
//         let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
//         let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
//         let targetPath = Path.normalizeFullPath targetPath
//         let sourcePath = defaultArg file sourcePath |> Path.getRelativeFileOrDirPath false targetPath false
//         mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=displayName)

module CodeServices =

    let compileFileToJs
        (com: Compiler)
        (pathResolver: PathResolver)
        (outPath: string)
        (fileExt: string)
        : Async<string>
        =
        async {
            let babel =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Babel.Compiler.transformFile com

            use writer =
                new BabelWriter(com, pathResolver, com.ProjectFile, com.CurrentFile, outPath, fileExt = fileExt)

            do! BabelPrinter.run writer babel
            let! output = writer.ReadContentAsString()
            return output
        }

    let typeCheckProject
        (sourceReader: SourceReader)
        (checker: InteractiveChecker)
        (cliArgs: CliArgs)
        (crackerResponse: CrackerResponse)
        : Async<TypeCheckProjectResult>
        =
        async {
            let! assemblies = checker.GetImportedAssemblies()

            let! checkProjectResult =
                checker.ParseAndCheckProject(
                    cliArgs.ProjectFile,
                    crackerResponse.ProjectOptions.SourceFiles,
                    sourceReader
                )

            return
                {
                    Assemblies = assemblies
                    ProjectCheckResults = checkProjectResult
                }
        }

    let compileFileToFableAST
        (sourceReader: SourceReader)
        (checker: InteractiveChecker)
        (cliArgs: CliArgs)
        (crackerResponse: CrackerResponse)
        (currentFile: string)
        : Async<AST.Fable.File>
        =
        async {
            let! assemblies = checker.GetImportedAssemblies()

            // Type-check the project up until the current file.
            let! checkProjectResult =
                checker.ParseAndCheckProject(
                    cliArgs.ProjectFile,
                    crackerResponse.ProjectOptions.SourceFiles,
                    sourceReader,
                    lastFile = currentFile
                )

            let fableProj =
                Project.From(
                    cliArgs.ProjectFile,
                    crackerResponse.ProjectOptions.SourceFiles,
                    checkProjectResult.AssemblyContents.ImplementationFiles,
                    assemblies,
                    Log.log,
                    getPlugin = Reflection.loadType cliArgs
                )

            let opts = cliArgs.CompilerOptions

            let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir

            let compiler: Compiler =
                CompilerImpl(
                    currentFile,
                    fableProj,
                    opts,
                    fableLibDir,
                    crackerResponse.OutputType,
                    ?outDir = cliArgs.OutDir
                )

            // TODO: make it configurable if FableTransforms.transformFile is applied?
            let fableAST =
                FSharp2Fable.Compiler.transformFile compiler
                |> FableTransforms.transformFile compiler

            return fableAST
        }

    let compileMultipleFilesToJavaScript
        (pathResolver: PathResolver)
        (cliArgs: CliArgs)
        (crackerResponse: CrackerResponse)
        (typeCheckProjectResult: TypeCheckProjectResult)
        (inputFiles: string seq)
        : Async<CompileResult>
        =
        async {
            let fableProj =
                Project.From(
                    cliArgs.ProjectFile,
                    crackerResponse.ProjectOptions.SourceFiles,
                    typeCheckProjectResult.ProjectCheckResults.AssemblyContents.ImplementationFiles,
                    typeCheckProjectResult.Assemblies,
                    Log.log,
                    getPlugin = Reflection.loadType cliArgs
                // ?precompiledInfo =
                //     (projCracked.PrecompiledInfo |> Option.map (fun i -> i :> _)),
                )

            let opts = cliArgs.CompilerOptions

            let! compiledFiles =
                inputFiles
                |> Seq.map (fun currentFile ->
                    async {
                        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir

                        let compiler: Compiler =
                            CompilerImpl(
                                currentFile,
                                fableProj,
                                opts,
                                fableLibDir,
                                crackerResponse.OutputType,
                                ?outDir = cliArgs.OutDir
                            )

                        let outputPath =
                            Path.ChangeExtension(currentFile, cliArgs.CompilerOptions.FileExtension)

                        let! js =
                            compileFileToJs compiler pathResolver outputPath cliArgs.CompilerOptions.FileExtension

                        return currentFile, js
                    }
                )
                |> Async.Parallel

            return
                {
                    CompiledFiles = Map.ofArray compiledFiles
                    Diagnostics = typeCheckProjectResult.ProjectCheckResults.Diagnostics
                }
        }

    let compileFileToJavaScript
        (sourceReader: SourceReader)
        (checker: InteractiveChecker)
        (pathResolver: PathResolver)
        (cliArgs: CliArgs)
        (crackerResponse: CrackerResponse)
        (currentFile: string)
        : Async<CompileResult>
        =
        async {
            let! dependentFiles =
                checker.GetDependentFiles(currentFile, crackerResponse.ProjectOptions.SourceFiles, sourceReader)

            let combinedDependentFiles = String.concat "\n" dependentFiles
            Log.info $"Dependent files for %s{currentFile} are:\n%s{combinedDependentFiles}"

            let lastFile =
                if Array.isEmpty dependentFiles then
                    currentFile
                else
                    Array.last dependentFiles

            let! assemblies = checker.GetImportedAssemblies()

            // Type-check the project up until the last dependent file.
            let! checkProjectResult =
                checker.ParseAndCheckProject(
                    cliArgs.ProjectFile,
                    crackerResponse.ProjectOptions.SourceFiles,
                    sourceReader,
                    lastFile = lastFile
                )

            let fableProj =
                Project.From(
                    cliArgs.ProjectFile,
                    crackerResponse.ProjectOptions.SourceFiles,
                    checkProjectResult.AssemblyContents.ImplementationFiles,
                    assemblies,
                    Log.log,
                    getPlugin = Reflection.loadType cliArgs
                )

            let opts = cliArgs.CompilerOptions

            let! compiledFiles =
                dependentFiles
                |> Array.filter (fun filePath -> not (filePath.EndsWith(".fsi", StringComparison.Ordinal)))
                |> Array.map (fun currentFile ->
                    async {
                        let fableLibDir = Path.getRelativePath currentFile crackerResponse.FableLibDir

                        let compiler: Compiler =
                            CompilerImpl(
                                currentFile,
                                fableProj,
                                opts,
                                fableLibDir,
                                crackerResponse.OutputType,
                                ?outDir = cliArgs.OutDir
                            )

                        let outputPath = Path.ChangeExtension(currentFile, ".js")

                        let! js =
                            compileFileToJs compiler pathResolver outputPath cliArgs.CompilerOptions.FileExtension

                        return currentFile, js
                    }
                )
                |> Async.Parallel

            return
                {
                    CompiledFiles = Map.ofArray compiledFiles
                    Diagnostics = checkProjectResult.Diagnostics
                }
        }
