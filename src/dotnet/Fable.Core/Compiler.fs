namespace Fable

type CompilerOptions =
    { declaration: bool
    ; typedArrays: bool
    ; clampByteArrays: bool }

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

type PluginInfo =
    { path: string; plugin: IPlugin }

type ICompiler =
    abstract CoreLib: string
    abstract Options: CompilerOptions
    abstract Plugins: PluginInfo list
    abstract AddLog: LogMessage->unit
    abstract GetUniqueVar: unit->string
