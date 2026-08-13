(*
    Computes which diagnostics `// fable-disable/-enable...` comments suppress (ESLint's
    disable-line/next-line/block model), via real comment tokens - not raw text matching.
*)
module Fable.Transforms.WarningSuppression

open Fable
open System.Collections.Concurrent
open System.Text.RegularExpressions
open FSharp.Compiler.Tokenization

type private DirectiveKind =
    | DisableLine
    | DisableNextLine
    | Disable
    | Enable

[<ReferenceEquality>]
type private Directive =
    {
        Kind: DirectiveKind
        /// `None` means "every code"
        Codes: Set<string> option
        Line: int
        /// Set the first time this directive suppresses something.
        mutable Used: bool
        /// Set when the directive was already reported for something else (typo'd code, bare
        /// block): no point telling the user it is unused on top of that.
        mutable Reported: bool
    }

type private BlockState =
    | NoneDisabled
    /// Blanket disable opened by this directive, minus the codes since re-enabled.
    | AllDisabledExcept of opener: Directive * enabled: Set<string>
    /// code -> the directive that disabled it
    | SpecificDisabled of Map<string, Directive>

/// A problem with a directive itself (typo'd code, directive that suppresses nothing, ...),
/// reported once the file it belongs to has finished compiling.
type DirectiveDiagnostic =
    {
        Code: string
        Message: string
        Line: int
    }

// The name must be followed by end-of-comment, whitespace or `:` so that `fable-disabled` or
// `fable-disable-lines` aren't mistaken for a bare directive. `:` is then optionally consumed
let private directiveRegex =
    Regex(@"^fable-(disable-next-line|disable-line|disable|enable)(?=$|\s|:)\s*:?\s*(.*)$", RegexOptions.Compiled)

let private codeSeparators = [| ' '; ','; '\t' |]

/// Cheap pre-filter: tokenizing a source file to look for directives that aren't there is pure
/// waste, and the vast majority of files contain none.
let private mayContainDirectives (source: string) =
    source.IndexOf("fable-disable", System.StringComparison.Ordinal) >= 0
    || source.IndexOf("fable-enable", System.StringComparison.Ordinal) >= 0

/// Splits a directive's argument text into codes, dropping the ESLint-style ` -- justification`
/// tail. Codes are upper-cased so `fable0001` matches, and anything not in the registry is
/// returned separately so the caller can report the typo instead of silently ignoring it.
let private parseCodes (raw: string) : Set<string> option * string list =
    let raw =
        match raw.IndexOf("--", System.StringComparison.Ordinal) with
        | -1 -> raw
        | i -> raw.Substring(0, i)

    let tokens =
        raw.Split(codeSeparators, System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun t -> t.ToUpperInvariant())

    let known, unknown = tokens |> Array.partition WarningCodes.knownCodes.Contains

    let codes =
        if Array.isEmpty tokens then
            None
        else
            Some(Set.ofArray known)

    codes, List.ofArray unknown

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

let private tryParseDirective (line: int) (commentText: string) : (Directive * string list) option =
    let text = stripCommentMarkers commentText
    let m = directiveRegex.Match(text)

    if not m.Success then
        None
    else
        let codes, unknown = parseCodes m.Groups[2].Value

        let kind =
            match m.Groups[1].Value with
            | "disable-line" -> Some DisableLine
            | "disable-next-line" -> Some DisableNextLine
            | "disable" -> Some Disable
            | "enable" -> Some Enable
            | _ -> None

        kind
        |> Option.map (fun kind ->
            {
                Kind = kind
                Codes = codes
                Line = line
                Used = false
                Reported = false
            },
            unknown
        )

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
    (runs |> Seq.map _.ToString() |> List.ofSeq), endState

/// Computed, queryable suppression info for a single source file.
type FileSuppressions =
    private
        {
            /// Every parsed directive, in source order.
            Directives: Directive[]
            /// 1-based line -> the line-scoped directives landing on it
            LineOnly: Map<int, Directive list>
            /// index (line - 1) -> block-disable state as of (and including) that line
            BlockAtLine: BlockState[]
            /// Problems found while parsing (typo'd codes, bare block disables)
            ParseDiagnostics: DirectiveDiagnostic list
        }

    /// Is a diagnostic with the given code (None = no code assigned to it) suppressed anywhere in
    /// `[startLine, endLine]`? Line-scoped directives match on any line of the range, so a
    /// trailing `// fable-disable-line` still works on a multi-line expression. The block state is
    /// read at `startLine` only: a `fable-disable` buried inside a large expression shouldn't
    /// retroactively silence a warning anchored above it.
    member this.IsSuppressed(startLine: int, endLine: int, code: string option) =
        let matches (d: Directive) =
            match d.Codes with
            | None -> true
            | Some codes -> code |> Option.map codes.Contains |> Option.defaultValue false

        let mutable suppressed = false

        for line in startLine .. max startLine endLine do
            match Map.tryFind line this.LineOnly with
            | Some directives ->
                for d in directives do
                    if matches d then
                        d.Used <- true
                        suppressed <- true
            | None -> ()

        if not suppressed && startLine >= 1 && startLine <= this.BlockAtLine.Length then
            match this.BlockAtLine[startLine - 1] with
            | NoneDisabled -> ()
            | AllDisabledExcept(opener, enabled) ->
                let hit =
                    match code with
                    | None -> true
                    | Some c -> not (Set.contains c enabled)

                if hit then
                    opener.Used <- true
                    suppressed <- true
            | SpecificDisabled disabled ->
                match code |> Option.bind (fun c -> Map.tryFind c disabled) with
                | Some opener ->
                    opener.Used <- true
                    suppressed <- true
                | None -> ()

        suppressed

    member this.IsSuppressed(line: int, code: string option) = this.IsSuppressed(line, line, code)

    /// Parse problems plus every directive that never suppressed anything. Only meaningful once
    /// the whole compilation is over: an inlined call can suppress through a directive in a file
    /// other than the one currently being compiled.
    member this.GetDiagnostics() =
        let unused =
            this.Directives
            |> Array.choose (fun d ->
                // `fable-enable` re-opens warnings rather than suppressing them, so "unused" has
                // no meaning for it - ESLint doesn't report those either. A directive whose codes
                // are already reported as typos is obviously unused too; one message is enough.
                if d.Kind = Enable || d.Used || d.Reported then
                    None
                else
                    let code, message = WarningCodes.unusedSuppressionDirective

                    Some
                        {
                            Code = code
                            Message = message
                            Line = d.Line
                        }
            )
            |> List.ofArray

        this.ParseDiagnostics @ unused

    static member Empty =
        {
            Directives = [||]
            LineOnly = Map.empty
            BlockAtLine = [||]
            ParseDiagnostics = []
        }

/// Folds one `fable-disable`/`fable-enable` into the running block state. The directive itself is
/// carried through so that whatever it ends up suppressing can mark it used.
let private transition (state: BlockState) (d: Directive) =
    match d.Kind = Disable, d.Codes, state with
    | true, None, _ -> AllDisabledExcept(d, Set.empty)
    | true, Some codes, NoneDisabled -> SpecificDisabled(codes |> Seq.map (fun c -> c, d) |> Map.ofSeq)
    | true, Some codes, SpecificDisabled m -> SpecificDisabled((m, codes) ||> Seq.fold (fun m c -> Map.add c d m))
    | true, Some codes, AllDisabledExcept(opener, ex) -> AllDisabledExcept(opener, Set.difference ex codes)
    | false, None, _ -> NoneDisabled
    | false, Some _, NoneDisabled -> NoneDisabled
    | false, Some codes, SpecificDisabled m -> SpecificDisabled((m, codes) ||> Seq.fold (fun m c -> Map.remove c m))
    | false, Some codes, AllDisabledExcept(opener, ex) -> AllDisabledExcept(opener, Set.union ex codes)

let private computeDirectives (defines: string list) (source: string) =
    let lines = source.Replace("\r\n", "\n").Split('\n')
    let sourceTok = FSharpSourceTokenizer(defines, None, None, None)
    let directives = ResizeArray<Directive>()
    let parseDiagnostics = ResizeArray<DirectiveDiagnostic>()
    let mutable state = FSharpTokenizerLexState.Initial

    for i in 0 .. lines.Length - 1 do
        let lineNo = i + 1
        let tokenizer = sourceTok.CreateLineTokenizer(lines[i])
        let runs, newState = scanLineComments tokenizer state lines[i]
        state <- newState

        for run in runs do
            match tryParseDirective lineNo run with
            | None -> ()
            | Some(directive, unknownCodes) ->
                let flag (code, message) =
                    directive.Reported <- true

                    parseDiagnostics.Add
                        {
                            Code = code
                            Message = message
                            Line = lineNo
                        }

                for unknown in unknownCodes do
                    flag (WarningCodes.unknownSuppressionCode unknown)

                // A bare `fable-disable` block silences every Fable warning to end of file, which
                // is almost never what people mean - flag it, but honour it.
                if directive.Kind = Disable && Option.isNone directive.Codes then
                    flag WarningCodes.suppressionBlockWithoutCode

                directives.Add directive

    let lineOnly =
        (Map.empty, directives)
        ||> Seq.fold (fun acc d ->
            let add line =
                let existing = Map.tryFind line acc |> Option.defaultValue []
                Map.add line (d :: existing) acc

            match d.Kind with
            | DisableLine -> add d.Line
            | DisableNextLine -> add (d.Line + 1)
            | Disable
            | Enable -> acc
        )

    let blockDirectivesByLine =
        directives
        |> Seq.filter (fun d ->
            match d.Kind with
            | Disable
            | Enable -> true
            | DisableLine
            | DisableNextLine -> false
        )
        |> Seq.groupBy (fun d -> d.Line)
        |> Seq.map (fun (line, ds) -> line, List.ofSeq ds)
        |> Map.ofSeq

    // Filling this with hundreds/thousands of NoneDisabled is nearly free: F# compiles a
    // nullary DU case to a singleton, so every slot is a pointer to the same object.
    let blockAtLine = Array.create lines.Length NoneDisabled
    let mutable current = NoneDisabled

    for lineNo in 1 .. lines.Length do
        match Map.tryFind lineNo blockDirectivesByLine with
        | Some ds ->
            for d in ds do
                current <- transition current d
        | None -> ()

        blockAtLine[lineNo - 1] <- current

    {
        Directives = Seq.toArray directives
        LineOnly = lineOnly
        BlockAtLine = blockAtLine
        ParseDiagnostics = List.ofSeq parseDiagnostics
    }

/// Scans the given source text for `fable-disable*`/`fable-enable*` comments and builds a
/// queryable `FileSuppressions` snapshot. Meant to be computed once per file and cached.
///
/// `defines` must be the same conditional-compilation symbols FCS was given (`FABLE_COMPILER`,
/// the project's `DefineConstants`, ...). Without them the tokenizer reports everything inside
/// an `#if` block as inactive code rather than comments, so directives sitting next to the very
/// warnings they're meant to suppress would be invisible.
let compute (defines: string list) (source: string) : FileSuppressions =
    if mayContainDirectives source then
        computeDirectives defines source
    else
        FileSuppressions.Empty

/// Suppression snapshots shared by every file of a project: computing them is per-file work, but
/// warnings can point at any file (inlined calls) and files compile in parallel. Keyed by the
/// content hash `SourceReader` already returns, so a file edited under watch mode recomputes
/// instead of being scanned against stale text.
type private Cache() =
    let entries = ConcurrentDictionary<string, int * FileSuppressions>()

    /// May throw `KeyNotFoundException` for a file outside the project (e.g. the source of a
    /// precompiled dll) - that's the caller's cue that there's nothing to suppress with.
    member _.GetOrCompute(fileName: string, defines: string list, read: SourceReader) =
        let hash, source = read fileName

        // GetOrAdd rather than a TryGetValue/assign pair: two threads racing on the same file
        // would otherwise end up with separate snapshots, splitting the "directive was used"
        // flags between them and producing bogus unused-directive reports.
        let cachedHash, suppressions =
            entries.GetOrAdd(fileName, fun _ -> hash, compute defines source.Value)

        if cachedHash = hash then
            suppressions
        else
            // The file changed under us, so the snapshot is against the wrong text.
            let fresh = hash, compute defines source.Value
            entries[fileName] <- fresh
            snd fresh

/// Extracts the conditional-compilation symbols out of the option list FCS is given
/// (`FSharpProjectOptions.OtherOptions`), which is the only place the project's `DefineConstants`
/// and Fable's own `FABLE_COMPILER*` symbols are both present.
let definesFromCompilerOptions (otherOptions: string seq) =
    otherOptions
    |> Seq.choose (fun opt ->
        if opt.StartsWith("--define:", System.StringComparison.Ordinal) then
            Some(opt.Substring("--define:".Length))
        else
            None
    )
    |> List.ofSeq

/// Project-wide entry point: resolves a file name to its suppressions, computing and caching on
/// first use. One instance per compiled project - it is shared across the per-file compilers.
type Resolver(defines: string list, read: SourceReader) =
    let cache = Cache()

    static member FromCompilerOptions(otherOptions: string seq, read: SourceReader) =
        Resolver(definesFromCompilerOptions otherOptions, read)

    member _.For(fileName: string) =
        try
            cache.GetOrCompute(fileName, defines, read)
        with :? System.Collections.Generic.KeyNotFoundException ->
            // Not a file of this project (e.g. the source of a precompiled dll): nothing to read,
            // so nothing can be suppressed. Any other failure is a real problem and must surface.
            FileSuppressions.Empty

    /// The directive problems of the given files. These are `FABLE0001`-band codes and so are
    /// never themselves suppressible - they flag comment text that is wrong to keep, so the only
    /// correct answer is to edit it. Call once the whole compilation is over: a warning raised
    /// while compiling one file can be suppressed by a directive living in another.
    member this.GetDiagnostics(fileNames: string seq) =
        fileNames
        |> Seq.collect (fun fileName -> this.For(fileName).GetDiagnostics() |> List.map (fun d -> fileName, d))
        |> List.ofSeq
