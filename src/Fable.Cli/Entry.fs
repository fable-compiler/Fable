module Fable.Cli.Entry

open System
open System.CommandLine.FSharp
open System.CommandLine.Help
open Fable.Cli.Spec
open Main
open Fable
open Fable.Compiler.Util
open Fable.Cli.CustomLogging
open Microsoft.Extensions.Logging

module CliArgs =
    let dim = "\x1b[2m"
    let dimOff = "\x1b[22m"

    let root =
        RootCommand("F# transpiler; supporting F# to javascript, typescript, python, rust and others.")

    let cleanCommand = Command.create "clean"
    let watchCommand = Command.create "watch" |> Mutate.Command.addAlias "w"
    let javascriptCommand = Command.create "javascript" |> Mutate.Command.addAlias "js"
    let pythonCommand = Command.create "python" |> Mutate.Command.addAlias "py"
    let typescriptCommand = Command.create "typescript" |> Mutate.Command.addAlias "ts"
    let rustCommand = Command.create "rust" |> Mutate.Command.addAlias "rs"
    let precompileCommand = Command.create "precompile" |> Mutate.hide
    let phpCommand = Command.create "php"
    let dartCommand = Command.create "dart"

    let languageCommands =
        [
            javascriptCommand
            typescriptCommand
            pythonCommand
            rustCommand
            phpCommand
            dartCommand
        ]

    let compilerCommands = watchCommand :: languageCommands
    let rootAndCompilerCommands = root :> Command :: compilerCommands
    let allCommands = cleanCommand :: compilerCommands
    let rootAndCommands = root :> Command :: allCommands
    // We keep the bindings to the opts/args because it likely
    // speeds up retrieval from parsed results when retrieving with
    // the item instead of with the name

    // Path argument for root only
    let projPath =
        Argument.create<string voption> "PATH"
        |> Mutate.Argument.defaultValue (System.Environment.CurrentDirectory |> ValueSome)
        |> Mutate.description $"{dim}Path containing project files{dimOff}"
        |> Utils.addArgToCommand root
    // extension argument for clean only
    let extensionArg =
        Argument.create<string voption> "EXT"
        |> Mutate.Argument.defaultValue (".fs.js" |> ValueSome)
        |> Mutate.description $"{dim}Path extension for cleaning{dimOff}"
        |> Utils.addArgToCommand cleanCommand

    let workingDirectory =
        CommandOption.create<string> "--cwd"
        |> Mutate.description $"{dim}Set the working directory{dimOff}"
        |> Mutate.CommandOption.filePathsOnly
        |> Mutate.CommandOption.defaultValue System.Environment.CurrentDirectory
        |> Utils.addToCommands rootAndCommands

    let verbosity =
        CommandOption.create<Verbosity> "--verbosity"
        |> Mutate.description $"{dim}Set the logging volume{dimOff}"
        |> Mutate.CommandOption.addAlias "-v"
        |> Mutate.CommandOption.valueOneOfStrings Utils.Unions.getAllCaseStringOrInitials<Verbosity>
        |> Utils.Unions.addCustomParser (
            function
            | Verbosity.Normal -> [ "n"; "normal" ]
            | Verbosity.Silent -> [ "s"; "silent" ]
            | Verbosity.Verbose -> [ "v"; "verbose" ]
        )
        |> Mutate.CommandOption.defaultValue Verbosity.Normal
        |> Mutate.CommandOption.helpName $"{dim}n|normal|s|silent|v|verbose{dimOff}"
        |> Utils.addToCommands rootAndCommands

    /// DEPRECATED - TODO remove in Fable v6
    let silent =
        CommandOption.create<bool> "--silent"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCommands

    /// DEPRECATED - TODO remove in Fable v6
    let verbose =
        CommandOption.create<bool> "--verbose"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCommands

    let language =
        CommandOption.create<Language> "--language"
        |> Mutate.description $"{dim}Set the target language for the transpiler{dimOff}"
        |> Mutate.CommandOption.addAlias "--lang"
        |> Mutate.CommandOption.addAlias "-l"
        |> Mutate.CommandOption.valueOneOfStrings
            [
                "js"
                "javascript"
                "ts"
                "typescript"
                "py"
                "python"
                "rs"
                "rust"
                "php"
                "dart"
            ]
        |> Mutate.CommandOption.helpName $"{dim}javascript|typescript|python|rust|php|dart{dimOff}"
        |> Utils.Unions.addCustomParser (
            function
            | JavaScript -> [ "js"; "javascript" ]
            | TypeScript -> [ "ts"; "typescript" ]
            | Python -> [ "py"; "python" ]
            | Rust -> [ "rs"; "rust" ]
            | Php -> [ "php" ]
            | Dart -> [ "dart" ]
        )
        |> Mutate.CommandOption.defaultValue JavaScript
        |> Utils.addToCommands [ watchCommand; root ]

    let extension =
        CommandOption.create<string> "--extension"
        |> Mutate.description $"{dim}The file extension for Fable generated source files{dimOff}"
        |> Mutate.CommandOption.addAlias "-e"
        |> Mutate.CommandOption.defaultValue ".fs.js"
        |> Utils.addToCommands rootAndCommands

    let yes =
        CommandOption.create<bool> "--yes"
        |> Mutate.description $"{dim}Automatically respond yes to prompts.{dimOff}"
        |> Utils.addToCommands rootAndCommands

    let definitions =
        CommandOption.create<string[]> "--define"
        |> Mutate.description $"{dim}Add symbols for use in conditional preprocessing{dimOff}"
        |> Mutate.CommandOption.addAlias "-d"
        |> Utils.addToCommands rootAndCompilerCommands

    let output =
        CommandOption.create<string> "--output"
        |> Mutate.CommandOption.addAlias "-o"
        |> Mutate.CommandOption.description $"{dim}Set the output directory for generated source files{dimOff}"
        |> Mutate.CommandOption.filePathsOnly
        |> Utils.addToCommands rootAndCommands

    let config =
        CommandOption.create<BuildConfig> "--config"
        |> Mutate.CommandOption.addAlias "-c"
        |> Mutate.description $"{dim}Set the build configuration type{dimOff}"
        |> Mutate.CommandOption.valueOneOfStrings [ "Debug"; "Release" ]
        |> Mutate.CommandOption.helpName $"{dim}Debug|Release{dimOff}"
        |> Utils.Unions.addCustomParser (
            function
            | Release -> [ "Release" ]
            | Debug -> [ "Debug" ]
        )
        |> Utils.addToCommands rootAndCompilerCommands

    let watch =
        CommandOption.create<bool> "--watch"
        |> Mutate.description $"{dim}Run the compiler in watch mode{dimOff}"
        |> Utils.addToCommands (rootAndCompilerCommands |> List.except [ watchCommand ])

    let watchDelay =
        CommandOption.create<int> "--watchDelay"
        |> Mutate.description $"{dim}Delay between file changes and recompilation in watch mode{dimOff}"
        |> Mutate.CommandOption.defaultValue 200
        |> Utils.addToCommands rootAndCompilerCommands

    let run =
        CommandOption.create<string> "--run"
        |> Mutate.description $"{dim}Command line arguments that are run after compilation (enclose in strings){dimOff}"
        |> Utils.addToCommands rootAndCompilerCommands

    let runFast =
        CommandOption.create<string> "--runFast"
        |> Mutate.description
            $"{dim}Command line arguments that are run before compilation (enclose in strings){dimOff}"
        |> Utils.addToCommands rootAndCompilerCommands

    let runWatch =
        CommandOption.create<string> "--runWatch"
        |> Mutate.description
            $"{dim}Command line arguments that are run whenever recompilation occurs in watch mode{dimOff}"
        |> Utils.addToCommands rootAndCompilerCommands

    let noRestore =
        CommandOption.create<bool> "--noRestore"
        |> Mutate.description $"{dim}TODO{dimOff}"
        |> Utils.addToCommands rootAndCompilerCommands

    let noCache =
        CommandOption.create<bool> "--noCache"
        |> Mutate.description $"{dim}TODO{dimOff}"
        |> Utils.addToCommands rootAndCompilerCommands

    let exclude =
        CommandOption.create<string[]> "--exclude"
        |> Mutate.description $"{dim}Exclude paths from caching - used in plugin development{dimOff}"
        |> Utils.addToCommands rootAndCompilerCommands

    let optimize =
        CommandOption.create<bool> "--optimize"
        |> Utils.addToCommands rootAndCompilerCommands

    let legacyCracker =
        CommandOption.create<bool> "--legacyCracker"
        |> Utils.addToCommands rootAndCompilerCommands

    let printAst =
        CommandOption.create<bool> "--printAst"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCompilerCommands

    let trimRootModule =
        CommandOption.create<bool> "--trimRootModule"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCompilerCommands

    let fableLib =
        CommandOption.create<string> "--fableLib"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCompilerCommands

    let replace =
        CommandOption.create<string[]> "--replace"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCompilerCommands

    let precompiledLib =
        CommandOption.create<string> "--precompiledLib"
        |> Utils.addToCommands rootAndCompilerCommands
        |> Mutate.hide

    let noReflection =
        CommandOption.create<bool> "--noReflection"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCompilerCommands

    let noParallelTypeCheck =
        CommandOption.create<bool> "--noParallelTypeCheck"
        |> Mutate.hide
        |> Utils.addToCommands rootAndCompilerCommands

    let typedArrays =
        CommandOption.create<bool> "--typedArrays"
        |> Utils.addToCommands [ javascriptCommand; typescriptCommand; watchCommand; root ]

    let sourceMap =
        CommandOption.create<bool> "--sourceMap"
        |> Utils.addToCommands [ javascriptCommand; typescriptCommand; watchCommand; root ]

    let sourceMapRoot =
        CommandOption.create<string> "--sourceMapRoot"
        |> Utils.addToCommands [ javascriptCommand; typescriptCommand; watchCommand; root ]

    let runScript =
        CommandOption.create<string> "--runScript"
        |> Utils.addToCommands [ javascriptCommand; typescriptCommand; watchCommand; root ]

// let clean (args: CliArgs) language rootDir =
//     let ignoreDirs = set [ "bin"; "obj"; "node_modules" ]
//
//     let outDir = args.Value("-o", "--outDir")
//
//     let fileExt =
//         args.Value("-e", "--extension")
//         |> Option.defaultWith (fun () ->
//             let usesOutDir = Option.isSome outDir
//             File.defaultFileExt usesOutDir language
//         )
//
//     let cleanDir = outDir |> Option.defaultValue rootDir |> IO.Path.GetFullPath
//
//     // clean is a potentially destructive operation, we need a permission before proceeding
//     Console.WriteLine("This will recursively delete all *{0}[.map] files in {1}", fileExt, cleanDir)
//
//     if not (args.FlagEnabled "--yes") then
//         Console.WriteLine("Please press 'Y' or 'y' if you want to continue: ")
//         let keyInfo = Console.ReadKey()
//         Console.WriteLine()
//
//         if keyInfo.Key <> ConsoleKey.Y then
//             Console.WriteLine("Clean was cancelled.")
//             exit 0
//
//     let mutable fileCount = 0
//     let mutable fableModulesDeleted = false
//
//     let rec recClean dir =
//         seq {
//             yield! IO.Directory.GetFiles(dir, "*" + fileExt)
//             yield! IO.Directory.GetFiles(dir, "*" + fileExt + ".map")
//         }
//         |> Seq.iter (fun file ->
//             IO.File.Delete(file)
//             fileCount <- fileCount + 1
//             Log.verbose (lazy ("Deleted " + file))
//         )
//
//         IO.Directory.GetDirectories(dir)
//         |> Array.filter (fun subdir -> ignoreDirs.Contains(IO.Path.GetFileName(subdir)) |> not)
//         |> Array.iter (fun subdir ->
//             if IO.Path.GetFileName(subdir) = Naming.fableModules then
//                 IO.Directory.Delete(subdir, true)
//                 fableModulesDeleted <- true
//
//                 Log.always $"Deleted {IO.Path.GetRelativePath(rootDir, subdir)}"
//             else
//                 recClean subdir
//         )
//
//     recClean cleanDir
//
//     if fileCount = 0 && not fableModulesDeleted then
//         Log.always ("No files have been deleted. If Fable output is in another directory, pass it as argument.")
//     else
//         Log.always ("Clean completed! Files deleted: " + string<int> fileCount)

//
// let private logPrelude commands language =
//     match commands with
//     | [ "--version" ] -> ()
//     | _ ->
//         let status =
//             match getStatus language with
//             | "stable"
//             | "" -> ""
//             | status -> $" (status: {status})"
//
//         Log.always ($"Fable {Literals.VERSION}: F# to {language} compiler{status}")
//
//         match getLibPkgVersion language with
//         | Some(repository, pkgName, version) ->
//             Log.always ($"Minimum {pkgName} version (when installed from {repository}): {version}")
//         | None -> ()
//
//         Log.always ("\nThanks to the contributor! @" + Contributors.getRandom ())
//
//         Log.always ("Stand with Ukraine! https://standwithukraine.com.ua/" + "\n")

let makeCliOptions (command: Command) (parseResult: ParseResult) : ICliOptions =
    let getValueOrDefault defaultValue opt =
        parseResult |> Utils.getOptionValue opt |> ValueOption.defaultValue defaultValue

    { new ICliOptions with
        member this.workDir =
            CliArgs.workingDirectory |> getValueOrDefault Environment.CurrentDirectory

        member this.projPath =
            parseResult
            |> Utils.getArgumentValue CliArgs.projPath
            |> ValueOption.flatten
            |> ValueOption.defaultValue Environment.CurrentDirectory

        member this.verbosity =
            let silent = CliArgs.silent |> getValueOrDefault false
            let verbose = CliArgs.verbose |> getValueOrDefault false

            CliArgs.verbosity
            |> getValueOrDefault Verbosity.Normal
            |> function
                | Verbosity.Normal when verbose -> Verbosity.Verbose
                | Verbosity.Normal when silent -> Verbosity.Silent
                | verbosity -> verbosity

        member this.language = CliArgs.language |> getValueOrDefault Language.JavaScript
        member this.yes = CliArgs.yes |> getValueOrDefault false

        member this.defines =
            CliArgs.definitions
            |> getValueOrDefault [||]
            |> Array.toList
            |> List.append
                [
                    "FABLE_COMPILER"
                    "FABLE_COMPILER_5"
                    CliArgs.language
                    |> getValueOrDefault Language.JavaScript
                    |> _.ToString().ToUpper()
                    |> sprintf "FABLE_COMPILER_%s"
                ]

        member this.output = parseResult |> Utils.getOptionValue CliArgs.output

        member this.config =
            let isWatching =
                getValueOrDefault false CliArgs.watch || command = CliArgs.watchCommand

            parseResult
            |> Utils.getOptionValue CliArgs.config
            |> function
                | ValueSome config -> config
                | ValueNone when isWatching -> BuildConfig.Debug
                | ValueNone -> BuildConfig.Release

        member this.watch =
            getValueOrDefault false CliArgs.watch || command = CliArgs.watchCommand

        member this.watchDelay = CliArgs.watchDelay |> getValueOrDefault 200
        member this.run = parseResult |> Utils.getOptionValue CliArgs.run
        member this.runFast = parseResult |> Utils.getOptionValue CliArgs.runFast
        member this.runWatch = parseResult |> Utils.getOptionValue CliArgs.runWatch
        member this.noRestore = CliArgs.noRestore |> getValueOrDefault false
        member this.noCache = CliArgs.noCache |> getValueOrDefault false
        member this.exclude = CliArgs.exclude |> getValueOrDefault [||]
        member this.optimize = CliArgs.optimize |> getValueOrDefault false
        member this.legacyCracker = CliArgs.legacyCracker |> getValueOrDefault false
        member this.printAst = CliArgs.printAst |> getValueOrDefault false
        member this.trimRootModule = CliArgs.trimRootModule |> getValueOrDefault false
        member this.fableLib = parseResult |> Utils.getOptionValue CliArgs.fableLib

        member this.replace =
            CliArgs.replace
            |> getValueOrDefault [||]
            |> Array.map _.Split(':', 2)
            // TODO
            |> Array.filter (Array.length >> (=) 2)
            |> Array.map (
                function
                | [| a; b |] -> a, b
                | _ -> failwith "Unreachable"
            )
            |> Map

        member this.precompiledLib = parseResult |> Utils.getOptionValue CliArgs.precompiledLib
        member this.noReflection = CliArgs.noReflection |> getValueOrDefault false

        member this.noParallelTypeCheck =
            CliArgs.noParallelTypeCheck |> getValueOrDefault false

        member this.typedArrays = CliArgs.typedArrays |> getValueOrDefault false
        member this.sourceMap = CliArgs.sourceMap |> getValueOrDefault false
        member this.sourceMapRoot = parseResult |> Utils.getOptionValue CliArgs.sourceMapRoot
        member this.runScript = parseResult |> Utils.getOptionValue CliArgs.runScript

    }

[<EntryPoint>]
let main argv =
    CliArgs.allCommands |> List.iter CliArgs.root.Add

    CliArgs.watchCommand
    |> Mutate.Command.mapAction (fun result ->
        result |> printfn "PRINTING FROM WATCH %A"

        result
        |> Utils.getOptionResult CliArgs.config
        |> fun x ->
            x |> printfn "%A"
            x
        |> ValueOption.iter (printfn "PRINTING CONFIG %A")

        result |> Utils.getCommandResult CliArgs.typescriptCommand |> printfn "%A"
        result |> Utils.getOptionResult CliArgs.extension |> printfn "%A"
        0
    )
    |> ignore

    CliArgs.root
    |> Mutate.Command.mapAction (fun result ->
        result
        |> Utils.getCommandResult CliArgs.watchCommand
        |> ValueOption.iter (printfn "PRINTING FROM ROOT %A")

        0
    )
    |> Command.parseAndInvoke argv
