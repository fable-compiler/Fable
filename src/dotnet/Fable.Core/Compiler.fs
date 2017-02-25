namespace Fable

type CompilerOptions = {
        projFile: string
        coreLib: string
        symbols: string list
        plugins: string list
        noTypedArrays: bool
        clamp: bool
        declaration: bool
        extra: Map<string, string>
    }

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
    abstract ProjDir: string
    abstract Options: CompilerOptions
    abstract Plugins: (string*IPlugin) list
    abstract AddLog: LogMessage->unit
    abstract GetLogs: unit->seq<LogMessage>
    abstract GetUniqueVar: unit->string
