module Fable.Cli.Pipeline

open System
open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State

module Js =
    type BabelWriter(cliArgs: CliArgs, pathResolver: PathResolver, sourcePath: string, targetPath: string) =
        // In imports *.ts extensions have to be converted to *.js extensions instead
        let fileExt =
            let fileExt = cliArgs.CompilerOptions.FileExtension
            if fileExt.EndsWith(".ts") then Path.replaceExtension ".js" fileExt else fileExt
        let sourceDir = Path.GetDirectoryName(sourcePath)
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        let mapGenerator = lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))
        interface BabelPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeJsStringLiteral(str) =
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
                    File.changeFsExtension isInFableModules path fileExt
                else path
            member _.Dispose() = stream.Dispose()
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) =
                if cliArgs.SourceMaps then
                    let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
                    let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
                    let targetPath = Path.normalizeFullPath targetPath
                    let sourcePath = Path.getRelativeFileOrDirPath false targetPath false sourcePath
                    mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=name)
        member _.SourceMap =
            mapGenerator.Force().toJSON()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let babel =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Babel.Compiler.transformFile com

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
    type PythonFileWriter(sourcePath: string, targetPath: string, cliArgs: CliArgs, pathResolver) =
        let fileExt = ".py"
        let targetDir = Path.GetDirectoryName(targetPath)
        // PEP8: Modules should have short, all-lowercase names
        let fileName =
            Path.GetFileNameWithoutExtension(Path.GetFileNameWithoutExtension(targetPath))
                .Replace(".", "_")
        let fileName =
            fileName
            |> Naming.applyCaseRule Core.CaseRules.SnakeCase
            |> Fable.PY.Naming.checkPyKeywords
        // Note that Python modules cannot contain dots or it will be impossible to import them
        let targetPath = Path.Combine(targetDir, Path.replaceExtension fileExt fileName)

        let stream = new IO.StreamWriter(targetPath)

        interface PythonPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let python =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Python.Compiler.transformFile com

        let map = { new PythonPrinter.SourceMapGenerator with
                        member _.AddMapping(_,_,_,_,_) = () }
        let writer = new PythonFileWriter(com.CurrentFile, outPath, cliArgs, pathResolver)
        do! PythonPrinter.run writer map python
        match com.OutputType with
        | OutputType.Library ->
            // Make sure we include an empty `__init__.py` in every directory of a library
            let outPath = Path.Combine((Path.GetDirectoryName(outPath), "__init__.py"))
            if not (IO.File.Exists(outPath)) then
                let writer = new PythonFileWriter(String.Empty, outPath, cliArgs, pathResolver)
                do! PythonPrinter.run writer map { Body = [] }

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
    open Fable.Transforms.Dart

    type DartWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        interface DartPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeStringLiteral(str) =
                // TODO: Check if this works for Dart
                Web.HttpUtility.JavaScriptStringEncode(str)
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableModules = Path.Combine(targetDir, path) |> Naming.isInFableModules
                    File.changeFsExtension isInFableModules path fileExt
                else path
            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range=range, fileName=com.CurrentFile)
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let _imports, fable =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Extended.Compiler.transformFile com

        use writer = new DartWriter(com, cliArgs, pathResolver, outPath)
        do! DartPrinter.run writer fable
    }

module Rust =
    open Fable.Transforms.Rust

    type RustWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        interface RustPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path = Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then Path.ChangeExtension(path, fileExt) else path
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) = ()
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) = async {
        let crate =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Rust.Compiler.transformFile com

        use writer = new RustWriter(com, cliArgs, pathResolver, outPath)
        do! RustPrinter.run writer crate
    }

let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver (outPath: string) =
    match com.Options.Language with
    | JavaScript | TypeScript -> Js.compileFile com cliArgs pathResolver outPath
    | Python -> Python.compileFile com cliArgs pathResolver outPath
    | Php -> Php.compileFile com outPath
    | Dart -> Dart.compileFile com cliArgs pathResolver outPath
    | Rust -> Rust.compileFile com cliArgs pathResolver outPath
