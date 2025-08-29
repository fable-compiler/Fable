// We expose the main System.CommandLine namespace types through this
// namespace so that we can alias the `System.CommandLine.Option` type.
namespace System.CommandLine.FSharp

open System.CommandLine.Parsing

type Argument = System.CommandLine.Argument
type ArgumentArity = System.CommandLine.ArgumentArity
type ArgumentValidation = System.CommandLine.ArgumentValidation
type Argument<'T> = System.CommandLine.Argument<'T>
type Command = System.CommandLine.Command
type CompletionSourceExtensions = System.CommandLine.CompletionSourceExtensions
type DiagramDirective = System.CommandLine.DiagramDirective
type Directive = System.CommandLine.Directive
type EnvironmentVariablesDirective = System.CommandLine.EnvironmentVariablesDirective
type InvocationConfiguration = System.CommandLine.InvocationConfiguration
type CommandOption = System.CommandLine.Option
type CommandOptionValidation = System.CommandLine.OptionValidation
type CommandOption<'T> = System.CommandLine.Option<'T>
type ParserConfiguration = System.CommandLine.ParserConfiguration
type ParseResult = System.CommandLine.ParseResult
type RootCommand = System.CommandLine.RootCommand
type Symbol = System.CommandLine.Symbol
type VersionOption = System.CommandLine.VersionOption

module Utils =
    // Static type resolution is used since ParseResult and SymbolResult share some members but are
    // not within a type hierarchy.
    // If a command, argument, or option is not present, then we receive ValueNone
    // If the above has a default value, then we will receive a value.
    let inline getCommandResult<'ParseResult when 'ParseResult: (member GetResult: Command -> CommandResult)>
        (command: Command)
        : 'ParseResult -> CommandResult voption
        =
        _.GetResult(command) >> ValueOption.ofObj

    let inline getArgumentResult<'ParseResult when 'ParseResult: (member GetResult: Argument -> ArgumentResult)>
        (command: Argument)
        : 'ParseResult -> ArgumentResult voption
        =
        _.GetResult(command) >> ValueOption.ofObj

    let inline getOptionResult<'T, 'ParseResult
        when 'T :> CommandOption and 'ParseResult: (member GetResult: CommandOption -> OptionResult)>
        (command: 'T)
        : 'ParseResult -> OptionResult voption
        =
        _.GetResult(command :> CommandOption) >> ValueOption.ofObj

    let inline getNamedResult<'ParseResult when 'ParseResult: (member GetResult: string -> SymbolResult)>
        (command: string)
        : 'ParseResult -> SymbolResult voption
        =
        _.GetResult(command) >> ValueOption.ofObj

    let inline getArgumentValue<'T, 'ParseResult when 'ParseResult: (member GetValue: Argument<'T> -> 'T)>
        (arg: Argument<'T>)
        : 'ParseResult -> 'T voption
        =
        _.GetValue(arg)
        >> function
            | value when box value |> isNull -> ValueNone
            | value -> ValueSome value

    let inline getOptionValue<'T, 'ParseResult when 'ParseResult: (member GetValue: CommandOption<'T> -> 'T)>
        (cmdOption: CommandOption<'T>)
        : 'ParseResult -> 'T voption
        =
        _.GetValue(cmdOption)
        >> function
            | value when box value |> isNull -> ValueNone
            | value -> ValueSome value

    let inline getNamedValue<'ParseResult when 'ParseResult: (member GetValue: string -> obj)>
        (arg: string)
        : 'ParseResult -> obj voption
        =
        _.GetValue(arg)
        >> function
            | value when box value |> isNull -> ValueNone
            | value -> ValueSome value
// Explicit warning and hint that we are mutating C# objects. Warning is for posterity
module Mutate =
    let inline description desc (symbol: #Symbol) : #Symbol =
        symbol.Description <- desc
        symbol

    let inline hide (symbol: #Symbol) : #Symbol =
        symbol.Hidden <- true
        symbol

    module CommandOption =
        let description = description
        let hide = hide

        let addAlias alias (opt: #CommandOption) : #CommandOption =
            opt.Aliases.Add alias
            opt

        let require (opt: #CommandOption) : #CommandOption =
            opt.Required <- true
            opt

        let recursive (opt: #CommandOption) : #CommandOption =
            opt.Recursive <- true
            opt
        // It seems these are noops in the current beta of System.CommandLine
        let filePathsOnly (opt: CommandOption<string>) : CommandOption<string> = opt.AcceptLegalFilePathsOnly()
        let fileNamesOnly (opt: CommandOption<string>) : CommandOption<string> = opt.AcceptLegalFileNamesOnly()
        //
        let valueOneOf (values: 'T seq) (opt: CommandOption<'T>) : CommandOption<'T> =
            opt.AcceptOnlyFromAmong(values |> Seq.map _.ToString() |> Seq.toArray)

        let valueOneOfStrings (values: string seq) (opt: CommandOption<'T>) : CommandOption<'T> =
            opt.AcceptOnlyFromAmong(values |> Seq.toArray)

        let arity (value: ArgumentArity) (opt: #CommandOption) : #CommandOption =
            opt.Arity <- value
            opt
        // Messing with the argument result in the factory func can cause issues and is mostly unneeded
        let defaultValue (value: 'T) (opt: CommandOption<'T>) : CommandOption<'T> =
            opt.DefaultValueFactory <- (fun _ -> value)
            opt
        // Useful for options like `--language` which has many possible values (abbrevs and full names) and we want to
        // only have a select few written in the help message
        let helpName (msg: string) (opt: CommandOption<'T>) : CommandOption<'T> =
            opt.HelpName <- msg
            opt

    module Argument =
        let description = description
        let hide = hide

        let arity (value: ArgumentArity) (opt: #Argument) : #Argument =
            opt.Arity <- value
            opt

        let defaultValue (value: 'T) (arg: Argument<'T>) : Argument<'T> =
            arg.DefaultValueFactory <- (fun _ -> value)
            arg

    module Command =
        let description = description
        let hide = hide

        let addAlias alias (cmd: #Command) : #Command =
            cmd.Aliases.Add alias
            cmd

        let mapAction (func: ParseResult -> int) (cmd: #Command) : #Command =
            cmd.SetAction(func)
            cmd

        let iterAction (func: ParseResult -> int) = mapAction func >> ignore

module CommandOption =
    let create<'T> name = CommandOption<'T>(name)

module Argument =
    let create<'T> name = Argument<'T>(name)

module RootCommand =
    let create description = RootCommand(description)

module Command =
    let create name = Command(name)
    let parse (argv: string array) : #Command -> ParseResult = _.Parse(argv)
    let invoke: ParseResult -> int = _.Invoke()
    let parseAndInvoke argv = parse argv >> invoke
