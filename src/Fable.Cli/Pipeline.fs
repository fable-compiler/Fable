module Fable.Cli.Pipeline

open System
open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.Transforms

module Js =
    type BabelWriter(cliArgs: CliArgs, pathResolver: PathResolver, sourcePath: string, targetPath: string) =
        // In imports *.ts extensions have to be converted to *.js extensions instead
        let fileExt =
            let fileExt = cliArgs.CompilerOptions.FileExtension
            if fileExt.EndsWith(".ts") then Path.ChangeExtension(".js", fileExt) else fileExt
        let sourceDir = Path.GetDirectoryName(sourcePath)
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))
        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeStringLiteral(str) =
                Web.HttpUtility.JavaScriptStringEncode(str)
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path =
                    // TODO: Check precompiled out path for other languages too
                    match pathResolver.TryPrecompiledOutPath(sourceDir, path) with
                    | Some path -> Imports.getRelativePath sourceDir path
                    | None -> path
                let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableModules = Path.Combine(targetDir, path) |> Naming.isInFableModules
                    File.changeExtensionButUseDefaultExtensionInFableModules JavaScript isInFableModules path fileExt
                else path
            member _.Dispose() = stream.Dispose()
            member _.AddLog(msg, severity, ?range) = () // TODO
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) =
                if cliArgs.SourceMaps then
                    let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
                    let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
                    let targetPath = Path.normalizeFullPath targetPath
                    let sourcePath = Path.getRelativeFileOrDirPath false targetPath false sourcePath
                    mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=name)
        member _.SourceMap =
            mapGenerator.Force().toJSON()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) = async {
        let babel =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Babel.Compiler.transformFile com

        if not isSilent then
            let! sourceMap = async {
                use writer = new BabelWriter(cliArgs, pathResolver, com.CurrentFile, outPath)
                do! BabelPrinter.run writer babel
                return if cliArgs.SourceMaps then Some writer.SourceMap else None
            }

            match sourceMap with
            | Some sourceMap ->
                let mapPath = outPath + ".map"
                do! IO.File.AppendAllLinesAsync(outPath, [$"//# sourceMappingURL={IO.Path.GetFileName(mapPath)}"]) |> Async.AwaitTask
                use fs = IO.File.Open(mapPath, IO.FileMode.Create)
                do! sourceMap.SerializeAsync(fs) |> Async.AwaitTask
            | None -> ()
    }

module Python =
    let charRequiresEncoding (c: char) =
        c < (char) 0x20 // control chars always have to be encoded
        || c = '\"' // chars which must be encoded per JSON spec
        || c = '\\'
        || c = '\''
        // HTML-sensitive chars encoded for safety
        //|| c = '<'
        //|| c = '>'
        //|| (c = '&')
        || c = '\u0085' // newline chars have to be encoded
        || c = '\u2028'
        || c = '\u2029'

    let getTargetPath (cliArgs: CliArgs) (targetPath: string) =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let targetDir = Path.GetDirectoryName(targetPath)

        // PEP8: Modules (i.e files) should have short, all-lowercase names
        // Note that Python modules cannot contain dots or it will be impossible to import them
        let fileName =
            Path.GetFileNameWithoutExtension(targetPath).Replace(".", "_")
            |> Naming.applyCaseRule Core.CaseRules.SnakeCase
            |> PY.Naming.checkPyKeywords
        Path.Combine(targetDir, fileName + fileExt)

    type PythonFileWriter(targetPath: string) =
        let stream = new IO.StreamWriter(targetPath)

        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.Dispose() = stream.Dispose()
            member _.EscapeStringLiteral(str) = Naming.encodeString charRequiresEncoding str
            member _.MakeImportPath(path) = path
            member _.AddSourceMapping(_) = ()
            member _.AddLog(msg, severity, ?range) = () // TODO

    // Writes __init__ files to all directories. This mailbox serializes and dedups.
    let initFileWriter =
        new MailboxProcessor<string>(fun mb -> async {
            let rec loop (seen: Set<string>) = async  {
                let! outPath = mb.Receive()
                if (not (seen |> Set.contains outPath || (IO.File.Exists(outPath)))) then
                    do! IO.File.WriteAllTextAsync(outPath, "") |> Async.AwaitTask

                return! loop (seen.Add outPath)
            }
            return! loop (set [])
        })
    initFileWriter.Start()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let python =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Python.Compiler.transformFile com

        let outPath = getTargetPath cliArgs outPath

        if not cliArgs.UseRegion then
            let writer = new PythonFileWriter(outPath)
            do! PythonPrinter.run writer python

        else
            let delimiter = "# " + Naming.fableRegion
            let nativeRegions =
                if IO.File.Exists(outPath) then
                    ((Some [], []), IO.File.ReadLines(outPath))
                    ||> Seq.fold (fun (currentRegion, nativeRegions) line ->
                        let isDelimiter = line.StartsWith(delimiter)
                        match currentRegion with
                        | Some currentRegion ->
                            if isDelimiter then None, (List.rev currentRegion)::nativeRegions
                            else Some(line::currentRegion), nativeRegions
                        | None ->
                            if isDelimiter then Some [], nativeRegions
                            else None, nativeRegions
                    )
                    |> function
                        | Some region, regions -> (List.rev region)::regions |> List.rev
                        | None, regions -> regions |> List.rev
                else []

            let writer = new PythonFileWriter(outPath)
            do! PythonPrinter.runWithRegions writer delimiter nativeRegions python

        match com.OutputType with
        | OutputType.Library ->
            // Make sure we include an empty `__init__.py` in every directory of a library
            let outPath = Path.Combine((Path.GetDirectoryName(outPath), "__init__.py"))
            initFileWriter.Post(outPath)

        | _ -> ()
    }

module Php =
    let compileFile (com: Compiler) (outPath: string) = async {
        let php =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Php.transformFile com

        use w = new IO.StreamWriter(outPath)
        let ctx = PhpPrinter.Output.Writer.create w
        PhpPrinter.Output.writeFile ctx php
        w.Flush()
    }

module Dart =
    type DartWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let stream = new IO.StreamWriter(targetPath)
        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeStringLiteral(str) =
                str.Replace(@"\", @"\\").Replace(@"$", @"\$").Replace("\r", @"\r").Replace("\n", @"\n").Replace("'", @"\'")
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then Path.ChangeExtension(path, fileExt) else path
            member _.AddSourceMapping(_) = ()
            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range=range, fileName=com.CurrentFile)
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let file =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Dart.Compiler.transformFile com

        use writer = new DartWriter(com, cliArgs, pathResolver, outPath)
        do! DartPrinter.run writer file
    }

module Rust =
    open Fable.Transforms.Rust

    type RustWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let stream = new IO.StreamWriter(targetPath)
        interface Printer.Writer with
            member _.EscapeStringLiteral(str) = str
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then Path.ChangeExtension(path, fileExt) else path
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) = ()
            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range=range, fileName=com.CurrentFile)
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let crate =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Rust.Compiler.transformFile com

        use writer = new RustWriter(com, cliArgs, pathResolver, outPath)
        do! RustPrinter.run writer crate
    }

let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
    match com.Options.Language with
    | JavaScript | TypeScript -> Js.compileFile com cliArgs pathResolver isSilent outPath
    | Python -> Python.compileFile com cliArgs pathResolver outPath
    | Php -> Php.compileFile com outPath
    | Dart -> Dart.compileFile com cliArgs pathResolver outPath
    | Rust -> Rust.compileFile com cliArgs pathResolver outPath
