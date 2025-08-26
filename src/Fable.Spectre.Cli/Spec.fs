module Fable.Spectre.Cli.Settings.Spec

open Fable
open Spectre.Console.Cli

// All arguments and flags should be available to the root executable.
// The use of commands such as 'clean' with --help will show the sublist
// of flags available to them. Those commands are only interested in a
// subset of the flags anyway.
// In this way, the specification of the CLI is divided into:
// 1. Common args
// 2. Compile related args (ie related to all language targets)
// 3. Language related args (ie related to only that language)
// 4. A superset of all of the above

let getStatus =
    function
    | JavaScript
    | TypeScript -> "stable"
    | Python -> "beta"
    | Rust -> "alpha"
    | Dart -> "beta"
    | Php -> "experimental"

let getLibPkgVersion =
    function
    | JavaScript -> Some("npm", "@fable-org/fable-library-js", Literals.JS_LIBRARY_VERSION)
    | TypeScript -> Some("npm", "@fable-org/fable-library-ts", Literals.JS_LIBRARY_VERSION)
    | Python
    | Rust
    | Dart
    | Php -> None

/// Common settings for all commands
type ICommonArgs =
    abstract version: bool
    abstract workingDirectory: string
    abstract projPath: string
    abstract verbosity: Verbosity
    abstract silent: bool
    abstract extension: string
    abstract language: Language
    abstract yes: bool
    abstract uncaughtArgs: string list

/// Common settings all compiling related commands
type ICompilingArgs =
    inherit ICommonArgs
    abstract definitions: string list
    abstract outputDirectory: string option
    abstract configuration: string
    abstract watch: bool
    abstract watchDelay: int
    abstract run: string option
    abstract runFast: string option
    abstract runWatch: string option
    abstract noRestore: bool
    abstract noCache: bool
    abstract exclude: string[]
    abstract optimize: bool
    abstract legacyCracker: bool
    abstract printAst: bool
    abstract trimRootModule: bool
    abstract fableLib: string option
    abstract replace: Map<string, string>
    abstract precompiledLib: string option
    abstract noReflection: bool
    abstract noParallelTypeCheck: bool

// Compiling target specific args
type IJavaScriptArgs =
    inherit ICompilingArgs
    abstract typedArrays: bool
    abstract sourceMap: bool
    abstract sourceMapRoot: string option
    abstract runScript: string option

type ITypeScriptArgs =
    inherit IJavaScriptArgs

type IPythonArgs =
    inherit ICompilingArgs

type IRustArgs =
    inherit ICompilingArgs

type IPhpArgs =
    inherit ICompilingArgs

type IDartArgs =
    inherit ICompilingArgs

type ICliArgs =
    inherit IJavaScriptArgs
    inherit ITypeScriptArgs
    inherit IPythonArgs
    inherit IRustArgs
    inherit IPhpArgs
    inherit IDartArgs

open SpectreCoff
open Spectre.Console

module Output =
    let Dim = fun inp -> MarkupD([ Decoration.Dim ], inp)
    let dim = Dim >> toMarkedUpString

    let AnsiPanel color =
        {
            Border = BoxBorder.Ascii
            BorderColor = color
            Sizing = SizingBehaviour.Collapse
            Padding = Padding.AllEqual 1
        }

    let HeaderPanel = AnsiPanel(Some Color.Teal)
