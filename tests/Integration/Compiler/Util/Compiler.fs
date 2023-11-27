namespace Fable.Tests.Compiler.Util

open System
open Fable
open Fable.Cli.Main
open Fable.Transforms.State
open Fable.Compiler.Util

module Compiler =

  type Result = Log list
  module Result =
    let errors = List.filter (fun (m: Log) -> m.Severity = Severity.Error)
    let warnings = List.filter (fun (m: Log) -> m.Severity = Severity.Warning)
    let wasFailure = errors >> List.isEmpty >> not

  type Settings = {
    Opens: string list
  }
  module Settings =
    let standard = {
      Opens = [
        "System"
      ]
    }

  /// NOTE: NOT threadsafe
  ///       -> don't use `parallel`
  module Cached =
    let projDir = IO.Path.Join(__SOURCE_DIRECTORY__, "../TestProject" ) |> Path.normalizeFullPath
    let projFile = IO.Path.Join(projDir, "TestProject.fsproj" ) |> Path.normalizeFullPath
    let sourceFile = IO.Path.Join(projDir, "Program.fs" ) |> Path.normalizeFullPath

    let cliArgs =
        Log.makeSilent()
        let compilerOptions = CompilerOptionsHelper.Make()
        { CliArgs.ProjectFile = projFile
          FableLibraryPath = None
          RootDir = projDir
          Configuration = "Debug"
          OutDir = None
          IsWatch = false
          Precompile = false
          PrecompiledLib = None
          PrintAst = false
          SourceMaps = false
          SourceMapsRoot = None
          NoRestore = false
          NoCache = false
          NoParallelTypeCheck = false
          Exclude = ["Fable.Core"]
          Replace = Map.empty
          RunProcess = None
          CompilerOptions = compilerOptions }

    let mutable private state = State.Create(cliArgs, recompileAllFiles=true)

    let compile settings source =
        let preamble =
          [
            yield! settings.Opens |> List.map (sprintf "open %s")

            yield Environment.NewLine
          ]
          |> String.concat Environment.NewLine

        let source = preamble + source
        IO.File.WriteAllText(sourceFile, source)

        let result =
            state
            |> startCompilation
            |> Async.RunSynchronously

        match result with
        | Error(_msg, logs) -> Array.toList logs
        | Ok(newState, logs) ->
            state <- newState
            Array.toList logs

  module Assert =
    open Util.Testing

    type private ExpectedMsg = ExpectedMsg of string
      with
        member this.Value =
          let (ExpectedMsg msg) = this
          msg

    let private expected = ExpectedMsg ""
    type ExpectedMsg with
      member e.Append (txt: string) =
        match e.Value with
        | "" ->
          if txt.Length >= 1 then
            (txt.[0] |> Char.ToUpperInvariant |> string)
            +
            txt.[1..]
          else
            ""
        | msg -> msg + " " + txt
        |> ExpectedMsg
      member e.Error = e.Append "Error"
      member e.Warning = e.Append "Warning"
      member e.Neither = e.Append "neither"
      member e.Nor = e.Append "nor"
      member e.Or = e.Append "or"
      member e.Anything = e.Append "anything"
      member e.With txt = e.Append (sprintf "with \"%s\"" txt)
      member e.Matching txt = e.Append (sprintf "matching \"%s\"" txt)
      member e.And = e.Append("and")
      member e.No = e.Append "no"
      member e.Single = e.Append "single"
      member e.All = e.Append "all"
      member e.Count (n: int) = e.Append (sprintf "%i" n)

    let private fail (ExpectedMsg expected) actual =
      equal (box expected) (box actual)
    let private orFail expected test (actual: Result) : Result =
      if not <| test actual then
        fail expected actual
      actual

    module private Test =
      module Is =
        let error (msg: Log) = msg.Severity = Severity.Error
        let warning (msg: Log) = msg.Severity = Severity.Warning
        let single = function | [_] -> true | _ -> false
        let zero = List.isEmpty
        let count n = List.length >> (=) n
      module All =
        let error = List.forall Is.error
        let warning = List.forall Is.warning
        let neither = List.isEmpty
      module Exists =
        let error = List.exists Is.error
        let warning = List.exists Is.warning
        let neither = List.isEmpty
        let either: Result -> bool = List.isEmpty >> not
        let errorOrWarning: Result -> bool = List.isEmpty >> not
        let matching = List.exists
        let withMsg (txt: string) = List.exists (fun m -> m.Message.Contains txt)
      module Text =
        let contains (txt: string) msg = msg.Message.Contains(txt, StringComparison.InvariantCultureIgnoreCase)
        let isMatch (regex: System.Text.RegularExpressions.Regex) (msg: Log) =
          regex.IsMatch msg.Message
    let private (>&>) a b = fun actual -> a actual && b actual

    module Is =
      /// Neither error nor warning
      let strictSuccess =
        Test.All.neither
        |> orFail expected.Neither.Error.Nor.Warning
      let error =
        Test.Exists.error
        |> orFail expected.Error
      let warning =
        Test.Exists.warning >&> (not << Test.Exists.error)
        |> orFail expected.Warning
      let errorOrWarning =
        Test.Exists.errorOrWarning
        |> orFail expected.Error.Or.Warning
      module No =
        let error =
          (not << Test.Exists.error)
          |> orFail expected.No.Error
        /// Same as `strictSuccess`
        let errorOrWarning =
          strictSuccess

      /// No error; Warning is allowed
      ///
      /// Same as `No.error`
      let success =
        No.error

      module Single =
        let error =
          Test.Is.single >&> Test.All.error
          |> orFail expected.Single.Error
        let warning =
          Test.Is.single >&> Test.All.warning
          |> orFail expected.Single.Warning
        let errorOrWarning =
          Test.Is.single
          |> orFail expected.Single.Error.Or.Warning

    module Are =
      let errorsOrWarnings n =
        Test.Is.count n
        |> orFail (expected.Count(n).Error.Or.Warning)
      let errors n =
        Test.Is.count n >&> Test.All.error
        |> orFail (expected.Count(n).Error)
      let warnings n =
        Test.Is.count n >&> Test.All.warning
        |> orFail (expected.Count(n).Warning)

    module All =
      let error =
        Test.All.error
        |> orFail expected.All.Error
      let warning =
        Test.All.warning
        |> orFail expected.All.Warning

    module Exists =
      let errorWith (txt: string) =
        Test.Exists.matching (Test.Is.error >&> Test.Text.contains txt)
        |> orFail (expected.Error.With txt)
      let warningWith (txt: string) =
        Test.Exists.matching (Test.Is.warning >&> Test.Text.contains txt)
        |> orFail (expected.Error.With txt)
      let errorOrWarningWith (txt: string) =
        Test.Exists.matching (Test.Text.contains txt)
        |> orFail (expected.Error.Or.Warning.With txt)

      open System.Text.RegularExpressions
      let errorMatching (regex: Regex) =
        Test.Exists.matching (Test.Is.error >&> Test.Text.isMatch regex)
        |> orFail (expected.Error.Matching (regex.ToString()))
      let warningMatching (regex: Regex) =
        Test.Exists.matching (Test.Is.warning >&> Test.Text.isMatch regex)
        |> orFail (expected.Warning.Matching (regex.ToString()))
      let errorOrWarningMatching (regex: Regex) =
        Test.Exists.matching (Test.Text.isMatch regex)
        |> orFail (expected.Error.Or.Warning.Matching (regex.ToString()))
