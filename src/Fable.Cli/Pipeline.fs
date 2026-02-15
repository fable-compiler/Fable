module Fable.Cli.Pipeline

open System
open Fable
open Fable.AST
open Fable.Transforms
open Fable.Compiler.Util

type Stream =
    static member WriteToFile(memoryStream: IO.Stream, filePath: string) =
        async {
            memoryStream.Seek(0, IO.SeekOrigin.Begin) |> ignore
            use fileStream = new IO.StreamWriter(filePath)

            do! memoryStream.CopyToAsync(fileStream.BaseStream) |> Async.AwaitTask

            do! fileStream.FlushAsync() |> Async.AwaitTask
            return true
        }

    static member IsEqualToFile(memoryStream: IO.Stream, targetPath: string) =
        async {
            let areStreamsEqual (stream1: IO.Stream) (stream2: IO.Stream) =
                let buffer1 = Array.zeroCreate<byte> 1024
                let buffer2 = Array.zeroCreate<byte> 1024

                let areBuffersEqual count1 count2 =
                    if count1 <> count2 then
                        false
                    else
                        let mutable i = 0
                        let mutable equal = true

                        while equal && i < count1 do
                            equal <- buffer1[i] = buffer2[i]
                            i <- i + 1

                        equal

                let rec areStreamsEqual () =
                    async {
                        let! count1 = stream1.AsyncRead(buffer1, 0, buffer1.Length)

                        let! count2 = stream2.AsyncRead(buffer2, 0, buffer2.Length)

                        match count1, count2 with
                        | 0, 0 -> return true
                        | count1, count2 when areBuffersEqual count1 count2 ->
                            if count1 < buffer1.Length then
                                return true
                            else
                                return! areStreamsEqual ()
                        | _ -> return false
                    }

                areStreamsEqual ()

            memoryStream.Seek(0, IO.SeekOrigin.Begin) |> ignore
            use fileStream = IO.File.OpenRead(targetPath)

            return! areStreamsEqual memoryStream fileStream
        }

    static member WriteToFileIfChanged(memoryStream: IO.Stream, targetPath: string) : Async<bool> =
        async {
            if memoryStream.Length = 0 then
                return false
            elif not (IO.File.Exists(targetPath)) then
                return! Stream.WriteToFile(memoryStream, targetPath)
            else
                let fileInfo = new IO.FileInfo(targetPath)

                if fileInfo.Length <> memoryStream.Length then
                    return! Stream.WriteToFile(memoryStream, targetPath)
                else
                    match! Stream.IsEqualToFile(memoryStream, targetPath) with
                    | false -> return! Stream.WriteToFile(memoryStream, targetPath)
                    | true -> return false
        }

module Js =
    type BabelWriter
        (com: Compiler, cliArgs: CliArgs, pathResolver: PathResolver, sourcePath: string, targetPath: string)
        =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let sourceDir = Path.GetDirectoryName(sourcePath)
        let targetDir = Path.GetDirectoryName(targetPath)
        let memoryStream = new IO.MemoryStream()
        let stream = new IO.StreamWriter(memoryStream)

        let mapGenerator =
            lazy (SourceMapSharp.SourceMapGenerator(?sourceRoot = cliArgs.SourceMapsRoot))

        member _.WriteToFileIfChanged() =
            async {
                if cliArgs.SourceMaps then
                    let mapPath = targetPath + ".map"

                    do!
                        stream.WriteLineAsync($"//# sourceMappingURL={IO.Path.GetFileName(mapPath)}")
                        |> Async.AwaitTask

                do! stream.FlushAsync() |> Async.AwaitTask

                let! written = Stream.WriteToFileIfChanged(memoryStream, targetPath)

                if written && cliArgs.SourceMaps then
                    use fs = IO.File.Open(targetPath + ".map", IO.FileMode.Create)

                    do! mapGenerator.Force().toJSON().SerializeAsync(fs) |> Async.AwaitTask

                stream.Dispose()
            }

        interface Printer.Writer with
            // Don't dispose the stream here because we need to access the memory stream to check if file has changed
            member _.Dispose() = ()

            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask

            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)

                let path =
                    // TODO: Check precompiled out path for other languages too
                    match pathResolver.TryPrecompiledOutPath(sourceDir, path) with
                    | Some path -> Imports.getRelativePath sourceDir path
                    | None -> path

                let path =
                    Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path

                if path.EndsWith(".fs", StringComparison.Ordinal) then
                    let isInFableModules = Path.Combine(targetDir, path) |> Naming.isInFableModules

                    File.changeExtensionButUseDefaultExtensionInFableModules JavaScript isInFableModules path fileExt
                else
                    path

            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

            member _.AddSourceMapping(srcLine, srcCol, genLine, genCol, file, displayName) =
                if cliArgs.SourceMaps then
                    let generated: SourceMapSharp.Util.MappingIndex =
                        {
                            line = genLine
                            column = genCol
                        }

                    let original: SourceMapSharp.Util.MappingIndex =
                        {
                            line = srcLine
                            column = srcCol
                        }

                    let targetPath = Path.normalizeFullPath targetPath

                    let sourcePath =
                        defaultArg file sourcePath
                        |> Path.getRelativeFileOrDirPath false targetPath false

                    // This is a workaround for:
                    // https://github.com/fable-compiler/Fable/issues/3980
                    // We are still investigating why some of the F# code don't have source information
                    // I believe for now we can ship it like that because it only deteriorate the source map
                    // it should not break them completely.
                    if srcLine <> 0 && srcCol <> 0 && file <> Some "unknown" then
                        mapGenerator.Force().AddMapping(generated, original, source = sourcePath, ?name = displayName)

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
        async {
            let babel =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Babel.Compiler.transformFile com

            if not (isSilent || babel.IsEmpty) then
                use writer = new BabelWriter(com, cliArgs, pathResolver, com.CurrentFile, outPath)

                do! BabelPrinter.run writer babel
                // TODO: Check also if file has actually changed with other printers
                do! writer.WriteToFileIfChanged()
        }

module Python =
    // PEP8: Modules should have short, all-lowercase names Note that Python modules
    // cannot contain dots or it will be impossible to import them
    let normalizeFileName path =
        Path.GetFileNameWithoutExtension(path).Replace(".", "_").Replace("-", "_")
        |> Naming.applyCaseRule Core.CaseRules.SnakeCase
        |> Py.Naming.checkPyKeywords
        |> Py.Naming.checkPyStdlib

    let getTargetPath (cliArgs: CliArgs) (targetPath: string) =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let targetDir = Path.GetDirectoryName(targetPath)
        let fileName = normalizeFileName targetPath
        Path.Combine(targetDir, fileName + fileExt)

    type PythonFileWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let stream = new IO.StreamWriter(targetPath)
        let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
        let sourcePath = com.CurrentFile

        // Python: use PyPI package imports for fable_library, relative imports for other fable_modules
        // When a custom fableLib path is specified, use relative imports for fable_library too
        let usePyPIForFableLibrary =
            match cliArgs.FableLibraryPath with
            | None -> true // Default: use PyPI package for fable_library
            | Some Py.Naming.fableLibPyPIPackage -> true // Explicit PyPI package (--fableLib fable-library)
            | Some _ -> false // Custom path: use relative imports for everything

        // Everything within the Fable hidden directory will be compiled as Library. We do this since the files there will be
        // compiled as part of the main project which might be a program (Exe) or library (Library).
        let isLibrary =
            com.OutputType = OutputType.Library || Naming.isInFableModules com.CurrentFile

        let isFableLibrary = isLibrary && List.contains "FABLE_LIBRARY" com.Options.Define

        // For non-library files, import resolution must be done from the main directory
        let targetPathForResolution =
            if isLibrary then
                targetPath
            else
                IO.Path.Join(defaultArg cliArgs.OutDir projDir, IO.Path.GetFileName(targetPath))
                |> Path.normalizeFullPath

        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask

            member _.Dispose() = stream.Dispose()

            member _.AddSourceMapping(_, _, _, _, _, _) = ()

            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

            member _.MakeImportPath(path) =
                let relativePath parts =
                    parts
                    |> Array.mapi (fun i part ->
                        match part with
                        | "." when isLibrary -> Some ""
                        | ".." when isLibrary -> Some "."
                        | "."
                        | ".." -> None
                        | _ when i = parts.Length - 1 -> Some(normalizeFileName part)
                        | _ -> Some(part.Replace(".", "_")) // Do not lowercase dir names. See #3079
                    )
                    |> Array.choose id
                    |> String.concat "."

                let packagePath parts =
                    let mutable i = -1

                    parts
                    |> Array.choose (fun part ->
                        i <- i + 1

                        if part = "." then
                            if i = 0 && isLibrary then
                                Some("")
                            else
                                None
                        elif part = ".." then
                            None
                        elif part = Naming.fableModules && (not isLibrary) then
                            None
                        elif i = parts.Length - 1 then
                            Some(normalizeFileName part)
                        else
                            Some part // Do not normalize dir names. See #3079
                    )
                    |> String.concat "."

                // Convert Python-style relative module paths to file paths for resolution.
                // Paths starting with dots are treated as relative imports (Pythonic convention):
                // e.g., ".native_code" -> "./native_code.py", ".py.native_code" -> "./py/native_code.py"
                //       "..native_code" -> "../native_code.py", "...module" -> "../../module.py"
                // Paths without "." prefix are treated as absolute/package imports and pass through as-is.
                let path =
                    if path.Contains('/') || path.EndsWith(".py") then
                        path
                    elif path.StartsWith(".") then
                        let trimmed = path.TrimStart('.')
                        let dotCount = path.Length - trimmed.Length
                        // Build the relative path prefix: "." -> "./", ".." -> "../", "..." -> "../../", etc.
                        let prefix =
                            if dotCount = 1 then
                                "./"
                            else
                                String.replicate (dotCount - 1) "../"

                        prefix + trimmed.Replace(".", "/") + ".py"
                    else
                        path

                if path.Contains('/') then
                    // If inside fable-library then use relative path
                    if isFableLibrary then
                        "." + normalizeFileName path
                    else
                        let outDir =
                            match cliArgs.OutDir with
                            | Some outDir -> Some outDir
                            // For files from the main program, always use an outDir to enforce resolution using targetPathForResolution
                            | None when not isLibrary -> Some projDir
                            | None -> None

                        let resolvedPath =
                            Imports.getImportPath pathResolver sourcePath targetPathForResolution projDir outDir path

                        let parts = resolvedPath.Split('/')

                        // Use package imports for fable_library when using PyPI, relative imports for everything else
                        let isFableLibraryImport = parts |> Array.exists (fun p -> p = "fable_library")

                        if usePyPIForFableLibrary && isFableLibraryImport then
                            packagePath parts
                        else
                            relativePath parts
                else
                    path

    // Writes __init__ files to all directories. This mailbox serializes and dedups.
    let initFileWriter =
        new MailboxProcessor<string>(fun mb ->
            async {
                let rec loop (seen: Set<string>) =
                    async {
                        let! outPath = mb.Receive()

                        if (not (seen |> Set.contains outPath || (IO.File.Exists(outPath)))) then
                            do! IO.File.WriteAllTextAsync(outPath, "") |> Async.AwaitTask

                        return! loop (seen.Add outPath)
                    }

                return! loop (set [])
            }
        )

    initFileWriter.Start()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
        async {
            let python =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Python.Compiler.transformFile com

            if not (isSilent || PythonPrinter.isEmpty python) then
                let writer = new PythonFileWriter(com, cliArgs, pathResolver, outPath)

                do! PythonPrinter.run writer python

                match com.OutputType with
                | OutputType.Library ->
                    // Make sure we include an empty `__init__.py` in every directory of a library
                    let outPath = Path.Combine((Path.GetDirectoryName(outPath), "__init__.py"))

                    initFileWriter.Post(outPath)

                | _ -> ()
        }

module Php =
    type PhpWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let stream = new IO.StreamWriter(targetPath)

        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask

            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)

                let path =
                    Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path

                if path.EndsWith(".fs", StringComparison.Ordinal) then
                    Path.ChangeExtension(path, fileExt)
                else
                    path

            member _.AddSourceMapping(_, _, _, _, _, _) = ()

            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
        async {
            let php =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Php.Compiler.transformFile com

            if not (isSilent || PhpPrinter.isEmpty php) then
                use writer = new PhpWriter(com, cliArgs, pathResolver, outPath)
                do! PhpPrinter.run writer php
        }

module Dart =
    type DartWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)
        let stream = new IO.StreamWriter(targetPath)

        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask

            member _.MakeImportPath(path) =
                let path =
                    Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path

                if path.EndsWith(".fs", StringComparison.Ordinal) then
                    Path.ChangeExtension(path, fileExt)
                else
                    path

            member _.AddSourceMapping(_, _, _, _, _, _) = ()

            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
        async {
            let file =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Dart.Compiler.transformFile com

            if not (isSilent || DartPrinter.isEmpty file) then
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
            member self.Write(str) =

                let str =
                    // rewrite import paths in last file
                    if com.CurrentFile = (Array.last com.SourceFiles) then
                        System.Text.RegularExpressions.Regex.Replace(
                            str,
                            @"(#\[path\s*=\s*\"")([^""]*)(\""])",
                            fun m ->
                                let path = (self :> Printer.Writer).MakeImportPath(m.Groups[2].Value)
                                m.Groups[1].Value + path + m.Groups[3].Value
                        )
                    else
                        str

                stream.WriteAsync(str) |> Async.AwaitTask

            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)

                let path =
                    Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path

                if path.EndsWith(".fs", StringComparison.Ordinal) then
                    Path.ChangeExtension(path, fileExt)
                else
                    path

            member _.AddSourceMapping(_, _, _, _, _, _) = ()

            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
        async {
            let crate =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable2Rust.Compiler.transformFile com

            if not (isSilent || RustPrinter.isEmpty crate) then
                use writer = new RustWriter(com, cliArgs, pathResolver, outPath)
                do! RustPrinter.run writer crate
        }

module Beam =
    /// Erlang module names must be lowercase snake_case and match the filename
    let normalizeFileName path =
        Path.GetFileNameWithoutExtension(path).Replace(".", "_").Replace("-", "_")
        |> Naming.applyCaseRule Core.CaseRules.SnakeCase

    let getTargetPath (cliArgs: CliArgs) (targetPath: string) =
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let targetDir = Path.GetDirectoryName(targetPath)
        let fileName = normalizeFileName targetPath
        Path.Combine(targetDir, fileName + fileExt)

    type BeamWriter(com: Compiler, cliArgs: CliArgs, pathResolver, targetPath: string) =
        let sourcePath = com.CurrentFile
        let fileExt = cliArgs.CompilerOptions.FileExtension
        let stream = new IO.StreamWriter(targetPath)

        interface Printer.Writer with
            member _.Write(str) =
                stream.WriteAsync(str) |> Async.AwaitTask

            member _.MakeImportPath(path) =
                let projDir = IO.Path.GetDirectoryName(cliArgs.ProjectFile)

                let path =
                    Imports.getImportPath pathResolver sourcePath targetPath projDir cliArgs.OutDir path

                if path.EndsWith(".fs", StringComparison.Ordinal) then
                    let path = Path.ChangeExtension(path, fileExt)
                    // Convert filename to snake_case to match Erlang module naming
                    let dir = Path.GetDirectoryName(path)
                    let fileName = normalizeFileName path + Path.GetExtension(path)
                    Path.Combine(dir, fileName)
                else
                    path

            member _.AddSourceMapping(_, _, _, _, _, _) = ()

            member _.AddLog(msg, severity, ?range) =
                com.AddLog(msg, severity, ?range = range, fileName = com.CurrentFile)

            member _.Dispose() = stream.Dispose()

    let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
        async {
            let erlModule =
                FSharp2Fable.Compiler.transformFile com
                |> FableTransforms.transformFile com
                |> Fable.Transforms.Beam.Compiler.transformFile com

            if not (isSilent || ErlangPrinter.isEmpty erlModule) then
                use writer = new BeamWriter(com, cliArgs, pathResolver, outPath)
                do! ErlangPrinter.run writer erlModule
        }

let compileFile (com: Compiler) (cliArgs: CliArgs) pathResolver isSilent (outPath: string) =
    match com.Options.Language with
    | JavaScript
    | TypeScript -> Js.compileFile com cliArgs pathResolver isSilent outPath
    | Python -> Python.compileFile com cliArgs pathResolver isSilent outPath
    | Php -> Php.compileFile com cliArgs pathResolver isSilent outPath
    | Dart -> Dart.compileFile com cliArgs pathResolver isSilent outPath
    | Rust -> Rust.compileFile com cliArgs pathResolver isSilent outPath
    | Beam -> Beam.compileFile com cliArgs pathResolver isSilent outPath
