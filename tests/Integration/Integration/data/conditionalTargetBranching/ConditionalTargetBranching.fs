module ConditionalTargetBranching

open Fable.Core

let target =
    if Compiler.isJavaScript then "javascript"
    elif Compiler.isPython then "python"
    else "dotnet"

let targetFromFamily =
    if Compiler.isJavaScript || Compiler.isTypeScript then "js-family"
    else "other"
