namespace Fable

type CompilerOptions =
    { symbols: string list
    ; plugins: string list
    ; clamp: bool
    ; declaration: bool
    ; typedArrays: bool }

type LogMessage =
    | Warning of string
    | Info of string
    | Error of string
    override x.ToString() =
        match x with
        | Warning s -> "[WARNING] " + s
        | Error s -> "[ERROR] " + s
        | Info s -> "[INFO] " + s

type IPlugin =
    interface end

type ICompiler =
    abstract CoreLib: string
    abstract Options: CompilerOptions
    abstract Plugins: (string*IPlugin) list
    abstract AddLog: LogMessage->unit
    abstract GetUniqueVar: unit->string
