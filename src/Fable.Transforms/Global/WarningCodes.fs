/// Central registry of stable codes + messages for `addWarningWithCode`: one function per
/// warning, so call sites sharing the same warning (e.g. StartsWith/EndsWith, JS/Python) can't
/// drift into different codes or wording. Codes are never reused/renumbered once published.
/// Usage: `WarningCodes.someWarning arg1 arg2 |> addWarningWithCode com inlinePath range`.
module Fable.Transforms.WarningCodes

[<Literal>]
let private CultureInfoIgnored = "FABLE0001"

[<Literal>]
let private StringSecondArgumentIgnored = "FABLE0002"

/// A `fable-disable*` directive named something that isn't in this registry - most likely a typo.
[<Literal>]
let UnknownSuppressionCode = "FABLE0003"

/// A `fable-disable*` directive that never suppressed anything, so it can be deleted.
[<Literal>]
let UnusedSuppressionDirective = "FABLE0004"

/// A bare `// fable-disable` block, which would silence every Fable warning up to end of file.
[<Literal>]
let SuppressionBlockWithoutCode = "FABLE0005"

/// Every code the compiler can emit. Directives naming anything else are reported as typos, so
/// a new code MUST be added here as well as given its own function below.
let knownCodes =
    set
        [
            CultureInfoIgnored
            StringSecondArgumentIgnored
            UnknownSuppressionCode
            UnusedSuppressionDirective
            SuppressionBlockWithoutCode
        ]

/// `String.StartsWith`/`EndsWith` with a `CultureInfo` argument: the comparison always runs
/// with the target's default culture rules, the argument is accepted but has no effect.
/// Used in both the JS/TS and Python replacements.
let cultureInfoIgnored = CultureInfoIgnored, "CultureInfo argument is ignored"

/// `String.Contains`/`StartsWith`/`EndsWith` called with an extra argument on the Dart target
/// (a `StringComparison`, a `CultureInfo`, ...): only the comparison itself is honored.
/// `methodName` fills in which one so the message stays specific.
let stringSecondArgumentIgnored (methodName: string) =
    StringSecondArgumentIgnored, $"String.%s{methodName}: second argument is ignored"

let unknownSuppressionCode (code: string) =
    UnknownSuppressionCode, $"Unknown warning code '%s{code}' in a fable-disable directive"

let unusedSuppressionDirective =
    UnusedSuppressionDirective, "This fable-disable directive doesn't suppress anything"

let suppressionBlockWithoutCode =
    SuppressionBlockWithoutCode,
    "A 'fable-disable' block must list the warning codes it suppresses, otherwise it silences every Fable warning until the end of the file"
