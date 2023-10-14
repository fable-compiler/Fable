module Build.Workspace

open Build.Utils
open System.IO

let root = Path.Resolve()

module ProjectDir =

    let fableAst = Path.Resolve("src", "Fable.AST")
    let fableCore = Path.Resolve("src", "Fable.Core")
    let fableCli = Path.Resolve("src", "Fable.Cli")
    let fablePublishUtils = Path.Resolve("src", "Fable.PublishUtils")
    let temp_fable_library = Path.Resolve("temp", "fable-library")
    let fable_library = Path.Resolve("src", "fable-library")
    let fable_metadata = Path.Resolve("src", "fable-metadata")
    let fable_standalone = Path.Resolve("src", "fable-standalone")
    let fable_compiler_js = Path.Resolve("src", "fable-compiler-js")

module Fsproj =

    let fableCli = Path.Combine(ProjectDir.fableCli, "Fable.Cli.fsproj")

module Changelog =

    let fableCLi = Path.Combine(ProjectDir.fableCli, "CHANGELOG.md")
