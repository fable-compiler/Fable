module Fable.Import.Mocha
open System
open System.Text.RegularExpressions
open Fable.Core
open Fable.Import.JS

type MochaSetupOptions =
    abstract slow: float option with get, set
    abstract timeout: float option with get, set
    abstract ui: string option with get, set
    abstract globals: ResizeArray<obj> option with get, set
    abstract reporter: obj option with get, set
    abstract bail: bool option with get, set
    abstract ignoreLeaks: bool option with get, set
    abstract grep: obj option with get, set

type IRunnable =
    abstract title: string with get, set
    abstract fn: Function with get, set
    abstract async: bool with get, set
    abstract sync: bool with get, set
    abstract timedOut: bool with get, set

and ISuite =
    abstract parent: ISuite with get, set
    abstract title: string with get, set
    abstract fullTitle: unit -> string

and ITest =
    inherit IRunnable
    abstract parent: ISuite with get, set
    abstract pending: bool with get, set
    abstract fullTitle: unit -> string

and IRunner =
    interface end

and IContextDefinition =
    [<Emit("$0($1...)")>] abstract Invoke: description: string * spec: (unit->unit) -> ISuite
    abstract only: description: string * spec: (unit->unit) -> ISuite
    abstract skip: description: string * spec: (unit->unit) -> unit
    abstract timeout: ms: float -> unit

and ITestDefinition =
    [<Emit("$0($1...)")>] abstract Invoke: expectation: string * ?assertion: (unit->unit) -> ITest
    [<Emit("$0($1...)")>] abstract Invoke: expectation: string * ?assertion: ((Error option -> unit)->unit) -> ITest
    abstract only: expectation: string * ?assertion: (unit->unit) -> ITest
    abstract only: expectation: string * ?assertion: ((Error option -> unit)->unit) -> ITest
    abstract skip: expectation: string * ?assertion: (unit->unit) -> unit
    abstract skip: expectation: string * ?assertion: ((Error option -> unit)->unit) -> unit
    abstract timeout: ms: float -> unit

module reporters =
    type [<Import("reporters.Base","Mocha")>] Base(runner: IRunner) =
        member __.stats with get(): obj = jsNative and set(v: obj): unit = jsNative

    and [<Import("reporters.Doc","Mocha")>] Doc(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.Dot","Mocha")>] Dot(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.HTML","Mocha")>] HTML(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.HTMLCov","Mocha")>] HTMLCov(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.JSON","Mocha")>] JSON(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.JSONCov","Mocha")>] JSONCov(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.JSONStream","Mocha")>] JSONStream(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.Landing","Mocha")>] Landing(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.List","Mocha")>] List(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.Markdown","Mocha")>] Markdown(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.Min","Mocha")>] Min(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.Nyan","Mocha")>] Nyan(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.Progress","Mocha")>] Progress(runner: IRunner, ?options: obj) =
        inherit Base(runner)

    and [<Import("reporters.Spec","Mocha")>] Spec(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.TAP","Mocha")>] TAP(runner: IRunner) =
        inherit Base(runner)

    and [<Import("reporters.XUnit","Mocha")>] XUnit(runner: IRunner, ?options: obj) =
        inherit Base(runner)

type [<Import("default","Mocha")>] Mocha(?options: obj) =
    member __.setup(options: MochaSetupOptions): Mocha = jsNative
    member __.bail(?value: bool): Mocha = jsNative
    member __.addFile(file: string): Mocha = jsNative
    member __.reporter(name: string): Mocha = jsNative
    member __.reporter(reporter: Func<IRunner, obj, obj>): Mocha = jsNative
    member __.ui(value: string): Mocha = jsNative
    member __.grep(value: string): Mocha = jsNative
    member __.grep(value: Regex): Mocha = jsNative
    member __.invert(): Mocha = jsNative
    member __.ignoreLeaks(value: bool): Mocha = jsNative
    member __.checkLeaks(): Mocha = jsNative
    member __.throwError(error: Error): unit = jsNative
    member __.growl(): Mocha = jsNative
    member __.globals(value: string): Mocha = jsNative
    member __.globals(values: ResizeArray<string>): Mocha = jsNative
    member __.useColors(value: bool): Mocha = jsNative
    member __.useInlineDiffs(value: bool): Mocha = jsNative
    member __.timeout(value: float): Mocha = jsNative
    member __.slow(value: float): Mocha = jsNative
    member __.enableTimeouts(value: bool): Mocha = jsNative
    member __.asyncOnly(value: bool): Mocha = jsNative
    member __.noHighlighting(value: bool): Mocha = jsNative
    member __.run(?onComplete: float->unit): IRunner = jsNative

module Globals =
    let [<Global>] mocha: Mocha = jsNative
    let [<Global>] describe: IContextDefinition = jsNative
    let [<Global>] xdescribe: IContextDefinition = jsNative
    let [<Global>] context: IContextDefinition = jsNative
    let [<Global>] suite: IContextDefinition = jsNative
    let [<Global>] it: ITestDefinition = jsNative
    let [<Global>] xit: ITestDefinition = jsNative
    let [<Global>] test: ITestDefinition = jsNative
