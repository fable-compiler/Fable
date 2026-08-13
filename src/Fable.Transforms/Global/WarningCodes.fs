(*
    Central registry for Fable warnings codes

    - `FABLE0001`-`FABLE0099` describe Fable's own suppression directivers. They are never suppressible.
    - `FABLE0100` and up describe code from compilation result. Those can be suppressed
*)
module Fable.Transforms.WarningCodes

(*
    FABLE0001-0099: about Fable's own directives. Never suppressible.
*)

/// A `fable-disable*` directive named something that isn't in this registry - most likely a typo.
[<Literal>]
let UnknownSuppressionCode = "FABLE0001"

/// A `fable-disable*` directive that never suppressed anything, so it can be deleted.
[<Literal>]
let UnusedSuppressionDirective = "FABLE0002"

/// A bare `// fable-disable` block, which would silence every Fable warning up to end of file.
[<Literal>]
let SuppressionBlockWithoutCode = "FABLE0003"

(*
    FABLE0100 and up: about the compiled code. Suppressible.
*)

[<Literal>]
let private CultureInfoIgnored = "FABLE0100"

[<Literal>]
let private SecondArgumentIgnored = "FABLE0101"

[<Literal>]
let private FormatProviderIgnored = "FABLE0102"

[<Literal>]
let private NumberStylesIgnored = "FABLE0103"

[<Literal>]
let private DateTimeStylesIgnored = "FABLE0104"

/// Every code the compiler can emit, both bands. A directive naming anything else is reported as
/// a typo, so a new code MUST be added here as well as given its own function below.
let knownCodes =
    set
        [
            UnknownSuppressionCode
            UnusedSuppressionDirective
            SuppressionBlockWithoutCode
            CultureInfoIgnored
            SecondArgumentIgnored
            FormatProviderIgnored
            NumberStylesIgnored
            DateTimeStylesIgnored
        ]

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

let unknownSuppressionCode (code: string) =
    UnknownSuppressionCode, $"Unknown warning code '%s{code}' in a fable-disable directive"

let unusedSuppressionDirective =
    UnusedSuppressionDirective, "This fable-disable directive doesn't suppress anything"

let suppressionBlockWithoutCode =
    SuppressionBlockWithoutCode,
    "A 'fable-disable' block must list the warning codes it suppresses, otherwise it silences every Fable warning until the end of the file"

/// `String.StartsWith`/`EndsWith` with a `CultureInfo` argument: the comparison always runs
/// with the target's default culture rules, the argument is accepted but has no effect.
/// Used in both the JS/TS and Python replacements.
let cultureInfoIgnored = CultureInfoIgnored, "CultureInfo argument is ignored"

/// `String.Contains`/`StartsWith`/`EndsWith` called with an extra argument on the Dart target
/// (a `StringComparison`, a `CultureInfo`, ...): only the comparison itself is honored. Which
/// method it was is not spelled out - the range already points at the call.
let secondArgumentIgnored =
    SecondArgumentIgnored, "Second argument is ignored: the comparison always uses the target's default rules"

/// An `IFormatProvider`/`CultureInfo` passed to a `Parse`/`TryParse` overload. The value is
/// always parsed with the invariant rules, so a culture that changes the decimal separator or
/// the day/month order silently changes which value you get.
let formatProviderIgnored =
    FormatProviderIgnored, "Format provider argument is ignored, parsing always uses the invariant culture"

/// A `NumberStyles` value that isn't `Integer` or `HexNumber` passed to a numeric `Parse`. The
/// value is interpolated because the range can't tell you which style was discarded.
let numberStylesIgnored (style: int) =
    NumberStylesIgnored, $"NumberStyles argument %d{style} is ignored"

/// A `DateTimeStyles` value passed to a date/time `Parse`.
let dateTimeStylesIgnored =
    DateTimeStylesIgnored, "DateTimeStyles argument is ignored"
