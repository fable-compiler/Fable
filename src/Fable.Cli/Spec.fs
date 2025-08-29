module Fable.Cli.Spec

open System.CommandLine
open System.CommandLine.FSharp
open Fable


// Specification of CLI settings reflected by interfaces

// Status is turned to a union so we can control things like stringification
// for style
[<Struct>]
type LanguageStatus =
    | Stable
    | Beta
    | Alpha
    | Experimental

    override this.ToString() =
        match this with
        | Stable -> "stable"
        | Beta -> "beta"
        | Alpha -> "alpha"
        | Experimental -> "experimental"

[<Struct>]
type BuildConfig =
    | Release
    | Debug

let getStatus =
    function
    | JavaScript
    | TypeScript -> Stable
    | Dart
    | Python -> Beta
    | Rust -> Alpha
    | Php -> Experimental

let getLibPkgVersion =
    function
    | JavaScript -> ValueSome("npm", "@fable-org/fable-library-js", Literals.JS_LIBRARY_VERSION)
    | TypeScript -> ValueSome("npm", "@fable-org/fable-library-ts", Literals.JS_LIBRARY_VERSION)
    | Python
    | Rust
    | Dart
    | Php -> ValueNone

type ICommonOptions =
    abstract workDir: string
    abstract projPath: string
    abstract verbosity: Verbosity
    abstract language: Language
    abstract yes: bool

type ICompileOptions =
    inherit ICommonOptions
    abstract defines: string list
    abstract output: string voption
    abstract config: BuildConfig
    abstract watch: bool
    abstract watchDelay: int
    abstract run: string voption
    abstract runFast: string voption
    abstract runWatch: string voption
    abstract noRestore: bool
    abstract noCache: bool
    abstract exclude: string[]
    abstract optimize: bool
    abstract legacyCracker: bool
    abstract printAst: bool
    abstract trimRootModule: bool
    abstract fableLib: string voption
    abstract replace: Map<string, string>
    abstract precompiledLib: string voption
    abstract noReflection: bool
    abstract noParallelTypeCheck: bool

type IJavaScriptOptions =
    inherit ICompileOptions
    abstract typedArrays: bool
    abstract sourceMap: bool
    abstract sourceMapRoot: string voption
    abstract runScript: string voption

type ITypeScriptOptions =
    inherit IJavaScriptOptions

type IPythonOptions =
    inherit ICompileOptions

type IRustOptions =
    inherit ICompileOptions

type IDartOptions =
    inherit ICompileOptions

type IPhpOptions =
    inherit ICompileOptions

type ICliOptions =
    inherit ICommonOptions
    inherit ICompileOptions
    inherit IJavaScriptOptions
    inherit ITypeScriptOptions
    inherit IPythonOptions
    inherit IRustOptions
    inherit IDartOptions
    inherit IPhpOptions

module Utils =
    open FSharp.Reflection

    module Unions =
        let getAllCaseStringOrInitials<'T> =
            FSharpType.GetUnionCases typeof<'T>
            |> Array.map _.Name
            |> Array.collect (fun case ->
                match case.Length with
                | 0 -> [||]
                | 1 -> [| case.ToLower() |]
                | _ -> [| case.ToLower(); (string case[0]).ToLower() |]
            )

        let addCustomParser (map: 'T -> string list) (opt: CommandOption<'T>) : CommandOption<'T> =
            let caseParsers =
                FSharpType.GetUnionCases typeof<'T>
                |> Array.map (fun case ->
                    let caseValue = FSharpValue.MakeUnion(case, [||])
                    caseValue |> unbox<'T>, caseValue |> unbox<'T> |> map
                )
                |> fun possibles (inputCase: string) ->
                    possibles
                    |> Array.tryFind (snd >> List.map _.ToLower() >> List.contains (inputCase.ToLower()))
                    |> Option.map fst
                    |> Option.get

            opt.CustomParser <- (fun parseResult -> parseResult.Tokens[0].Value |> caseParsers)
            opt

    let addToCommands (commands: #Command list) (opt: CommandOption<'T>) =
        commands |> List.iter _.Add(opt)
        opt

    let addToCommand (command: #Command) (opt: CommandOption<'T>) =
        command.Add(opt)
        opt

    let addArgToCommands (commands: #Command list) (arg: Argument<'T>) =
        commands |> List.iter _.Add(arg)
        arg

    let addArgToCommand (command: #Command) (arg: Argument<'T>) =
        command.Add arg
        arg
