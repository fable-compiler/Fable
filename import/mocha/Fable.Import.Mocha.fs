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
        member __.stats with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"

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
    member __.setup(options: MochaSetupOptions): Mocha = failwith "JS only"
    member __.bail(?value: bool): Mocha = failwith "JS only"
    member __.addFile(file: string): Mocha = failwith "JS only"
    member __.reporter(name: string): Mocha = failwith "JS only"
    member __.reporter(reporter: Func<IRunner, obj, obj>): Mocha = failwith "JS only"
    member __.ui(value: string): Mocha = failwith "JS only"
    member __.grep(value: string): Mocha = failwith "JS only"
    member __.grep(value: Regex): Mocha = failwith "JS only"
    member __.invert(): Mocha = failwith "JS only"
    member __.ignoreLeaks(value: bool): Mocha = failwith "JS only"
    member __.checkLeaks(): Mocha = failwith "JS only"
    member __.throwError(error: Error): unit = failwith "JS only"
    member __.growl(): Mocha = failwith "JS only"
    member __.globals(value: string): Mocha = failwith "JS only"
    member __.globals(values: ResizeArray<string>): Mocha = failwith "JS only"
    member __.useColors(value: bool): Mocha = failwith "JS only"
    member __.useInlineDiffs(value: bool): Mocha = failwith "JS only"
    member __.timeout(value: float): Mocha = failwith "JS only"
    member __.slow(value: float): Mocha = failwith "JS only"
    member __.enableTimeouts(value: bool): Mocha = failwith "JS only"
    member __.asyncOnly(value: bool): Mocha = failwith "JS only"
    member __.noHighlighting(value: bool): Mocha = failwith "JS only"
    member __.run(?onComplete: float->unit): IRunner = failwith "JS only"

module Globals =
    let [<Global>] mocha: Mocha = failwith "JS only"
    let [<Global>] describe: IContextDefinition = failwith "JS only"
    let [<Global>] xdescribe: IContextDefinition = failwith "JS only"
    let [<Global>] context: IContextDefinition = failwith "JS only"
    let [<Global>] suite: IContextDefinition = failwith "JS only"
    let [<Global>] it: ITestDefinition = failwith "JS only"
    let [<Global>] xit: ITestDefinition = failwith "JS only"
    let [<Global>] test: ITestDefinition = failwith "JS only"
