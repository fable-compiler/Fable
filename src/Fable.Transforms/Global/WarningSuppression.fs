/// Computes which diagnostics `// fable-disable/-enable...` comments suppress (ESLint's
/// disable-line/next-line/block model), via real comment tokens - not raw text matching.
module Fable.Transforms.WarningSuppression

open System.Text.RegularExpressions
open FSharp.Compiler.Tokenization

type private BlockState =
    | NoneDisabled
    | AllDisabledExcept of Set<string>
    | SpecificDisabled of Set<string>

type private Directive =
    | DisableLine of codes: Set<string> option * line: int
    | DisableNextLine of codes: Set<string> option * line: int
    | Disable of codes: Set<string> option * line: int
    | Enable of codes: Set<string> option * line: int

let private directiveRegex =
    Regex(@"^fable-(disable-next-line|disable-line|disable|enable)(?:\s+(.+))?$", RegexOptions.Compiled)

let private parseCodes (s: string) =
    let codes =
        s.Split([| ' '; ','; '\t' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Set.ofArray

    if Set.isEmpty codes then
        None
    else
        Some codes

let private stripCommentMarkers (raw: string) =
    let raw =
        if raw.StartsWith("//", System.StringComparison.Ordinal) then
            raw.Substring(2)
        elif raw.StartsWith("(*", System.StringComparison.Ordinal) then
            raw.Substring(2)
        else
            raw

    let raw =
        if raw.EndsWith("*)", System.StringComparison.Ordinal) then
            raw.Substring(0, raw.Length - 2)
        else
            raw

    raw.Trim()

let private tryParseDirective (line: int) (commentText: string) : Directive option =
    let text = stripCommentMarkers commentText
    let m = directiveRegex.Match(text)

    if not m.Success then
        None
    else
        let codes =
            if m.Groups[2].Success then
                parseCodes m.Groups[2].Value
            else
                None

        match m.Groups[1].Value with
        | "disable-line" -> Some(DisableLine(codes, line))
        | "disable-next-line" -> Some(DisableNextLine(codes, line))
        | "disable" -> Some(Disable(codes, line))
        | "enable" -> Some(Enable(codes, line))
        | _ -> None

/// Gathers each line's comment token runs as plain text (a line can have more than one, e.g. a
/// block comment then a trailing line comment); also returns the lexer state to carry into the
/// next line, needed to resume correctly inside multi-line block comments/strings.
let private scanLineComments
    (tokenizer: FSharpLineTokenizer)
    (initialState: FSharpTokenizerLexState)
    (line: string)
    : string list * FSharpTokenizerLexState
    =
    // Each finished run is one comment on the line; `current` is the run being built.
    let runs = ResizeArray<System.Text.StringBuilder>()
    let mutable current: System.Text.StringBuilder option = None

    // Pull tokens one at a time, threading the lexer state (needed across lines too).
    let rec loop state =
        match tokenizer.ScanToken(state) with
        | Some(tok: FSharpTokenInfo), state2 ->
            if tok.ColorClass = FSharpTokenColorKind.Comment then
                let text = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)

                match current with
                // Still inside the same comment: glue this token onto the current run.
                | Some sb -> sb.Append(text) |> ignore
                // First comment token after non-comment text: start a new run.
                | None ->
                    let sb = System.Text.StringBuilder(text: string)
                    current <- Some sb
                    runs.Add(sb)
            else
                // Non-comment token: close the current run, if any (e.g. code between two comments).
                current <- None

            loop state2
        // No more tokens on this line: return the final state for the next line.
        | None, state2 -> state2

    let endState = loop initialState
    // Materialize each run's text; endState lets the caller resume correctly on the next line.
    (runs |> Seq.map string |> List.ofSeq), endState

/// Computed, queryable suppression info for a single source file.
type FileSuppressions =
    private
        {
            /// 1-based line -> codes suppressed specifically on that line (None = all codes)
            LineOnly: Map<int, Set<string> option>
            /// index (line - 1) -> block-disable state as of (and including) that line
            BlockAtLine: BlockState[]
        }

    /// Is a diagnostic with the given code (None = no code assigned to it) suppressed on this line?
    member this.IsSuppressed(line: int, code: string option) =
        let lineSuppressed =
            match Map.tryFind line this.LineOnly with
            | Some None -> true
            | Some(Some codes) -> code |> Option.map codes.Contains |> Option.defaultValue false
            | None -> false

        let blockSuppressed =
            if line < 1 || line > this.BlockAtLine.Length then
                false
            else
                match this.BlockAtLine[line - 1] with
                | NoneDisabled -> false
                | AllDisabledExcept enabled ->
                    match code with
                    | None -> true
                    | Some c -> not (Set.contains c enabled)
                | SpecificDisabled disabled ->
                    match code with
                    | None -> false
                    | Some c -> Set.contains c disabled

        lineSuppressed || blockSuppressed

    static member Empty =
        {
            LineOnly = Map.empty
            BlockAtLine = [||]
        }

let private transition (state: BlockState) (codes: Set<string> option) (isDisable: bool) =
    match isDisable, codes, state with
    | true, None, _ -> AllDisabledExcept Set.empty
    | true, Some codes, NoneDisabled -> SpecificDisabled codes
    | true, Some codes, SpecificDisabled s -> SpecificDisabled(Set.union s codes)
    | true, Some codes, AllDisabledExcept ex -> AllDisabledExcept(Set.difference ex codes)
    | false, None, _ -> NoneDisabled
    | false, Some _, NoneDisabled -> NoneDisabled
    | false, Some codes, SpecificDisabled s -> SpecificDisabled(Set.difference s codes)
    | false, Some codes, AllDisabledExcept ex -> AllDisabledExcept(Set.union ex codes)

/// Scans the given source text for `fable-disable*`/`fable-enable*` comments and builds a
/// queryable `FileSuppressions` snapshot. Meant to be computed once per file and cached.
let compute (source: string) : FileSuppressions =
    let lines = source.Replace("\r\n", "\n").Split('\n')

    if lines.Length = 0 then
        FileSuppressions.Empty
    else
        let sourceTok = FSharpSourceTokenizer([], None, None, None)
        let directives = ResizeArray<Directive>()
        let mutable state = FSharpTokenizerLexState.Initial

        for i in 0 .. lines.Length - 1 do
            let lineNo = i + 1
            let tokenizer = sourceTok.CreateLineTokenizer(lines[i])
            let runs, newState = scanLineComments tokenizer state lines[i]
            state <- newState

            for run in runs do
                tryParseDirective lineNo run |> Option.iter directives.Add

        let lineOnly =
            (Map.empty, directives)
            ||> Seq.fold (fun acc d ->
                let merge line (codes: Set<string> option) =
                    let merged =
                        match Map.tryFind line acc, codes with
                        | Some None, _
                        | _, None -> None
                        | None, Some c -> Some c
                        | Some(Some existing), Some c -> Some(Set.union existing c)

                    Map.add line merged acc

                match d with
                | DisableLine(codes, line) -> merge line codes
                | DisableNextLine(codes, line) -> merge (line + 1) codes
                | Disable _
                | Enable _ -> acc
            )

        let blockDirectivesByLine =
            directives
            |> Seq.choose (
                function
                | Disable(codes, line) -> Some(line, codes, true)
                | Enable(codes, line) -> Some(line, codes, false)
                | DisableLine _
                | DisableNextLine _ -> None
            )
            |> Seq.groupBy (fun (line, _, _) -> line)
            |> Map.ofSeq

        // Filling this with hundreds/thousands of NoneDisabled is nearly free: F# compiles a
        // nullary DU case to a singleton, so every slot is a pointer to the same object.
        let blockAtLine = Array.create lines.Length NoneDisabled
        let mutable current = NoneDisabled

        for lineNo in 1 .. lines.Length do
            match Map.tryFind lineNo blockDirectivesByLine with
            | Some ds ->
                for _, codes, isDisable in ds do
                    current <- transition current codes isDisable
            | None -> ()

            blockAtLine[lineNo - 1] <- current

        {
            LineOnly = lineOnly
            BlockAtLine = blockAtLine
        }
