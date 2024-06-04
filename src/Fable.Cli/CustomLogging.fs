// Source: https://github.com/ionide/FSharp.Analyzers.SDK/blob/main/src/FSharp.Analyzers.Cli/CustomLogging.fs
module Fable.Cli.CustomLogging

open System
open System.IO
open System.Runtime.CompilerServices
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Console
open Microsoft.Extensions.Logging.Abstractions
open Microsoft.Extensions.Options

type CustomOptions() =
    inherit ConsoleFormatterOptions()

    /// if true: no LogLevel as prefix, colored output according to LogLevel
    /// if false: LogLevel as prefix, no colored output
    member val UseNoPrefixMsgStyle = false with get, set

type CustomFormatter(options: IOptionsMonitor<CustomOptions>) as this =
    inherit ConsoleFormatter("customName")

    let mutable optionsReloadToken: IDisposable = null
    let mutable formatterOptions = options.CurrentValue
    let origColor = Console.ForegroundColor

    do optionsReloadToken <- options.OnChange(fun x -> this.ReloadLoggerOptions(x))

    member private _.ReloadLoggerOptions(opts: CustomOptions) = formatterOptions <- opts

    override this.Write<'TState>
        (
            logEntry: inref<LogEntry<'TState>>,
            _scopeProvider: IExternalScopeProvider,
            textWriter: TextWriter
        )
        =
        let message = logEntry.Formatter.Invoke(logEntry.State, logEntry.Exception)

        if formatterOptions.UseNoPrefixMsgStyle then
            this.SetColor(textWriter, logEntry.LogLevel)
            textWriter.WriteLine(message)
            this.ResetColor()
        else
            this.WritePrefix(textWriter, logEntry.LogLevel)
            textWriter.WriteLine(message)

    member private _.WritePrefix(textWriter: TextWriter, logLevel: LogLevel) =
        match logLevel with
        | LogLevel.Trace -> textWriter.Write("trace: ")
        | LogLevel.Debug -> textWriter.Write("debug: ")
        | LogLevel.Information -> textWriter.Write("info: ")
        | LogLevel.Warning -> textWriter.Write("warn: ")
        | LogLevel.Error -> textWriter.Write("error: ")
        | LogLevel.Critical -> textWriter.Write("critical: ")
        | _ -> ()

    // see https://learn.microsoft.com/en-us/dotnet/core/extensions/console-log-formatter
    member private _.SetColor(textWriter: TextWriter, logLevel: LogLevel) =

        let color =
            match logLevel with
            | LogLevel.Error -> "\x1B[31m" // ConsoleColor.DarkRed
            | LogLevel.Warning -> "\x1B[33m" // ConsoleColor.DarkYellow
            | LogLevel.Information -> "\x1B[37m" // ConsoleColor.Gray
            | LogLevel.Trace -> "\x1B[1m\x1B[36m" // ConsoleColor.Cyan
            | _ -> "\x1B[39m\x1B[22m" // default foreground color

        textWriter.Write(color)

    member private _.ResetColor() = Console.ForegroundColor <- origColor

    interface IDisposable with
        member _.Dispose() = optionsReloadToken.Dispose()

[<Extension>]
type ConsoleLoggerExtensions =

    [<Extension>]
    static member AddCustomConsole(builder: ILoggingBuilder, configure: Action<CustomOptions>) : ILoggingBuilder =
        builder
            .AddConsole(fun options -> options.FormatterName <- "customName")
            .AddConsoleFormatter<CustomFormatter, CustomOptions>(configure)
