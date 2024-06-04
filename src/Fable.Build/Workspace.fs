module Build.Workspace

open Build.Utils
open System.IO

let root = Path.Resolve()

module ProjectDir =

    let fableAst = Path.Resolve("src", "Fable.AST")
    let fableCore = Path.Resolve("src", "Fable.Core")
    let fableCli = Path.Resolve("src", "Fable.Cli")
    let fablePublishUtils = Path.Resolve("src", "Fable.PublishUtils")
    let fableCompiler = Path.Resolve("src", "Fable.Compiler")
    let temp_fable_library_js = Path.Resolve("temp", "fable-library-js")

    let temp_fable_library_ts = Path.Resolve("temp", "fable-library-ts")
    let fable_library_ts = Path.Resolve("src", "fable-library-ts")
    let fable_metadata = Path.Resolve("src", "fable-metadata")
    let fable_standalone = Path.Resolve("src", "fable-standalone")
    let fable_compiler_js = Path.Resolve("src", "fable-compiler-js")

module Fsproj =

    let fableCli = Path.Combine(ProjectDir.fableCli, "Fable.Cli.fsproj")
    let fableCore = Path.Combine(ProjectDir.fableCore, "Fable.Core.fsproj")

module Changelog =

    let fableCLi = Path.Combine(ProjectDir.fableCli, "CHANGELOG.md")
    let fableCore = Path.Combine(ProjectDir.fableCore, "CHANGELOG.md")

module FableLibrary =

    let javascript = Path.Resolve("temp", "fable-library")
    let typescript = Path.Resolve("temp", "fable-library-ts")
    let python = Path.Resolve("temp", "fable-library-py")
    let rust = Path.Resolve("temp", "fable-library-rs")
    let php = Path.Resolve("temp", "fable-library-php")
    let dart = Path.Resolve("temp", "fable-library-dart")
