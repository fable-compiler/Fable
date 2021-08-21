module Fable.Cli.Pipeline

open System
open Fable
open Fable.AST
open Fable.Transforms
open Fable.Transforms.State

module Js =
    type BabelWriter(cliArgs: CliArgs, dedupTargetDir, sourcePath: string, targetPath: string) =
        // In imports *.ts extensions have to be converted to *.js extensions instead
        let fileExt =
            let fileExt = cliArgs.CompilerOptions.FileExtension
            if fileExt.EndsWith(".ts") then Path.replaceExtension ".js" fileExt else fileExt
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
                let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                    File.changeFsExtension isInFableHiddenDir path fileExt
                else path
            member _.Dispose() = stream.Dispose()
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) =
                if cliArgs.SourceMaps then
                    let generated: SourceMapSharp.Util.MappingIndex = { line = genLine; column = genCol }
                    let original: SourceMapSharp.Util.MappingIndex = { line = srcLine; column = srcCol }
                    mapGenerator.Force().AddMapping(generated, original, source=sourcePath, ?name=name)
        member _.SourceMap =
            mapGenerator.Force().toJSON()

    let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
        let babel =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Babel.Compiler.transformFile com

        let! sourceMap = async {
            use writer = new BabelWriter(cliArgs, dedupTargetDir, com.CurrentFile, outPath)
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
    type PythonFileWriter(sourcePath: string, targetPath: string, cliArgs: CliArgs, dedupTargetDir) =
        let fileExt = ".py"
        let targetDir = Path.GetDirectoryName(targetPath)
        // PEP8: Modules should have short, all-lowercase names
        let fileName = Path.GetFileNameWithoutExtension(Path.GetFileNameWithoutExtension(targetPath))
        let fileName = Naming.applyCaseRule Core.CaseRules.SnakeCase fileName
        // Note that Python modules cannot contain dots or it will be impossible to import them
        let targetPath = Path.Combine(targetDir, fileName + fileExt)

        let stream = new IO.StreamWriter(targetPath)

        interface PythonPrinter.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                    File.changeFsExtension isInFableHiddenDir path "" // Remove file extension
                else path
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
        let python =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Python.Compiler.transformFile com

        let map = { new PythonPrinter.SourceMapGenerator with
                        member _.AddMapping(_,_,_,_,_) = () }
        let writer = new PythonFileWriter(com.CurrentFile, outPath, cliArgs, dedupTargetDir)
        do! PythonPrinter.run writer map python
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

    type DartWriter(com: Compiler, cliArgs: CliArgs, dedupTargetDir, targetPath: string) =
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
                let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                    File.changeFsExtension isInFableHiddenDir path fileExt
                else path
            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range=range, fileName=com.CurrentFile)
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
        let _imports, fable =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Extended.Compiler.transformFile com

        use writer = new DartWriter(com, cliArgs, dedupTargetDir, outPath)
        do! DartPrinter.run writer fable
    }

module Lua =
    open Fable.Transforms.Lua
    type LuaWriter(com: Compiler, cliArgs: CliArgs, dedupTargetDir, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let targetDir = Path.GetDirectoryName(targetPath)
        let stream = new IO.StreamWriter(targetPath)
        interface Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask
            member _.EscapeJsStringLiteral(str) =
                Web.HttpUtility.JavaScriptStringEncode(str)
            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
                let path = Imports.getImportPath dedupTargetDir sourcePath targetPath projDir cliArgs.OutDir path
                if path.EndsWith(".fs") then
                    let isInFableHiddenDir = Path.Combine(targetDir, path) |> Naming.isInFableHiddenDir
                    File.changeFsExtension isInFableHiddenDir path fileExt
                else path
            member _.AddSourceMapping((srcLine, srcCol, genLine, genCol, name)) = ()
            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) = async {
        let babel =
            FSharp2Fable.Compiler.transformFile com
            |> FableTransforms.transformFile com
            |> Fable2Babel.Compiler.transformFile com

        use writer = new LuaWriter(com, cliArgs, dedupTargetDir, outPath)
        //do! (writer :> LuaPrinter.Writer).Write(sprintf "AST: %A" fable.Declarations)
        do! run writer babel
    }


let compileFile (com: Compiler) (cliArgs: CliArgs) dedupTargetDir (outPath: string) =
    match com.Options.Language with
    | JavaScript | TypeScript -> Js.compileFile com cliArgs dedupTargetDir outPath
    | Python -> Python.compileFile com cliArgs dedupTargetDir outPath
    | Php -> Php.compileFile com outPath
    | Dart -> Dart.compileFile com cliArgs dedupTargetDir outPath
    | Lua -> Lua.compileFile com cliArgs dedupTargetDir outPath
