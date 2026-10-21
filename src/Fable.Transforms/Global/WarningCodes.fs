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

/// Dart's `contains`/`startsWith`/`endsWith` are ordinal and case-sensitive, so a
/// `StringComparison` or `CultureInfo` asking for anything else has no effect.
let stringComparisonIgnored = "FABLE0101", "String comparison argument is ignored"

/// An `IFormatProvider`/`CultureInfo` passed to a `Parse`/`TryParse`, `String.Format` or
/// `StringBuilder.AppendFormat` overload. Fable is culture-independent here, so a culture that
/// changes the decimal separator or the day/month order silently changes the result.
let formatProviderIgnored = "FABLE0102", "Format provider argument is ignored"

/// A `NumberStyles` value that isn't `Integer` or `HexNumber` passed to a numeric `Parse`. The
/// value is interpolated because the range can't tell you which style was discarded.
let numberStylesIgnored (style: int) =
    "FABLE0103", $"NumberStyles argument %d{style} is ignored"

/// A `DateTimeStyles` value passed to a date/time `Parse`.
let dateTimeStylesIgnored = "FABLE0104", "DateTimeStyles argument is ignored"

/// A `TimeSpan` constructed with a microseconds argument. The runtime representation only carries
/// milliseconds, so the finer component is dropped rather than rounded.
let timeSpanPrecisionIgnored =
    "FABLE0105", "TimeSpan precision is limited to milliseconds, microsecond arguments are ignored"

/// `FSharpType.IsUnion(t, allowAccessToPrivateRepresentation)` and friends. Fable's reflection
/// has no notion of a private representation, so the flag never restricts anything.
let privateRepresentationFlagIgnored =
    "FABLE0106", "Private representation flag is ignored"

/// `Convert.ToBase64String(bytes, offset, length)` / `(bytes, options)`. Only the array is used,
/// so a slice is encoded whole and line-break options have no effect.
let base64ArgumentsIgnored =
    "FABLE0107", "Base64 offset, length and formatting arguments are ignored"

/// Every code the compiler can emit, both bands. A directive naming anything else is reported as
/// a typo, so a new warning MUST be added to this list as well as defined above.
let knownCodes =
    [
        // The two parameterised warnings get a throwaway argument; only the code is read.
        unknownSuppressionCode ""
        unusedSuppressionDirective
        suppressionBlockWithoutCode
        cultureInfoIgnored
        stringComparisonIgnored
        formatProviderIgnored
        numberStylesIgnored 0
        dateTimeStylesIgnored
        timeSpanPrecisionIgnored
        privateRepresentationFlagIgnored
        base64ArgumentsIgnored
    ]
    |> List.map fst
    |> Set.ofList
