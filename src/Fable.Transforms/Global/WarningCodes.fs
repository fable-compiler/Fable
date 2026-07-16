/// Central registry of stable codes + messages for `addWarningWithCode`: one function per
/// warning, so call sites sharing the same warning (e.g. StartsWith/EndsWith, JS/Python) can't
/// drift into different codes or wording. Codes are never reused/renumbered once published.
/// Usage: `WarningCodes.someWarning arg1 arg2 |> addWarningWithCode com inlinePath range`.
module Fable.Transforms.WarningCodes

[<Literal>]
let private CultureInfoIgnored = "FABLE0001"

[<Literal>]
let private StringSecondArgumentIgnored = "FABLE0002"

/// `String.StartsWith`/`EndsWith` with a `CultureInfo` argument: the comparison always runs
/// with the target's default culture rules, the argument is accepted but has no effect.
/// Used in both the JS/TS and Python replacements.
let cultureInfoIgnored () =
    CultureInfoIgnored, "CultureInfo argument is ignored"

/// `String.Contains`/`StartsWith`/`EndsWith` called with a `StringComparison` argument on the
/// Dart target: only the comparison itself is honored, `methodName` fills in which one so the
/// message stays specific (e.g. "String.Contains: second argument is ignored").
let stringSecondArgumentIgnored (methodName: string) =
    StringSecondArgumentIgnored, $"String.{methodName}: second argument is ignored"
