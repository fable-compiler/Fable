namespace Fable

type CompilerOptions = {
        projFile: string
        outDir: string
        coreLib: string
        symbols: string list
        plugins: string list
        msbuild: string list
        refs: Map<string, string>
        watch: bool
        dll: bool
        noTypedArrays: bool
        clamp: bool
        copyExt: bool
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
    abstract Options: CompilerOptions
    abstract Plugins: (string*IPlugin) list
    abstract AddLog: LogMessage->unit
    abstract GetLogs: unit->seq<LogMessage>
    abstract GetUniqueVar: unit->string
