(*
    Central registry for Fable warnings codes

    - `FABLE0001`-`FABLE0099` describe Fable's own suppression directivers. They are never suppressible.
    - `FABLE0100` and up describe code from compilation result. Those can be suppressed
*)
module Fable.Transforms.WarningCodes

/// Can a `// fable-disable` comment silence a warning carrying this code? Codes below
/// `FABLE0100` describe the directives themselves and never can - see the band rules above.
/// This is what makes the banding an invariant rather than a naming convention: it holds
/// wherever a diagnostic is raised from, not just on the path that happens to raise them today.
///
/// `None` is a warning with no code at all, which a bare `// fable-disable-line` still catches.
let isSuppressible (code: string option) =
    match code with
    | None -> true
    // Every code is `FABLE` plus four digits, so ordinal comparison orders them numerically.
    | Some code -> System.String.CompareOrdinal(code, "FABLE0100") >= 0

(*
    FABLE0001-0099: about Fable's own directives. Never suppressible.
*)

/// A `fable-disable*` directive named something that isn't in this registry - most likely a typo.
let unknownSuppressionCode (code: string) =
    "FABLE0001", $"Unknown warning code '%s{code}' in a fable-disable directive"

/// A `fable-disable*` directive that never suppressed anything, so it can be deleted.
let unusedSuppressionDirective =
    "FABLE0002", "This fable-disable directive doesn't suppress anything"

/// A bare `// fable-disable` block, which would silence every Fable warning up to end of file.
let suppressionBlockWithoutCode =
    "FABLE0003",
    "A 'fable-disable' block must list the warning codes it suppresses, otherwise it silences every Fable warning until the end of the file"

(*
    FABLE0100 and up: about the compiled code. Suppressible.
*)

/// `String.StartsWith`/`EndsWith` with a `CultureInfo` argument: the comparison always runs
/// with the target's default culture rules, the argument is accepted but has no effect.
/// Used in both the JS/TS and Python replacements.
let cultureInfoIgnored = "FABLE0100", "CultureInfo argument is ignored"

/// `String.Contains`/`StartsWith`/`EndsWith` called with an extra argument on the Dart target
/// (a `StringComparison`, a `CultureInfo`, ...): only the comparison itself is honored. Which
/// method it was is not spelled out - the range already points at the call.
let secondArgumentIgnored =
    "FABLE0101", "Second argument is ignored: the comparison always uses the target's default rules"

/// An `IFormatProvider`/`CultureInfo` passed to a `Parse`/`TryParse` overload. The value is
/// always parsed with the invariant rules, so a culture that changes the decimal separator or
/// the day/month order silently changes which value you get.
let formatProviderIgnored =
    "FABLE0102", "Format provider argument is ignored, parsing always uses the invariant culture"

/// A `NumberStyles` value that isn't `Integer` or `HexNumber` passed to a numeric `Parse`. The
/// value is interpolated because the range can't tell you which style was discarded.
let numberStylesIgnored (style: int) =
    "FABLE0103", $"NumberStyles argument %d{style} is ignored"

/// A `DateTimeStyles` value passed to a date/time `Parse`.
let dateTimeStylesIgnored = "FABLE0104", "DateTimeStyles argument is ignored"

/// Every code the compiler can emit, both bands. A directive naming anything else is reported as
/// a typo, so a new warning MUST be added to this list as well as defined above.
let knownCodes =
    [
        // The two parameterised warnings get a throwaway argument; only the code is read.
        unknownSuppressionCode ""
        unusedSuppressionDirective
        suppressionBlockWithoutCode
        cultureInfoIgnored
        secondArgumentIgnored
        formatProviderIgnored
        numberStylesIgnored 0
        dateTimeStylesIgnored
    ]
    |> List.map fst
    |> Set.ofList
