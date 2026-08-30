namespace Microsoft.FSharp.Core

// F# emits `raise (MatchFailureException(file, line, col))` for any match it
// cannot prove exhaustive. The other targets import this type from their runtime
// library (Replacements.fs maps Types.matchFail to it); Rust had no definition,
// so such a match referenced a type that did not exist and the generated crate
// failed with `unresolved import ...Microsoft::FSharp::Core::MatchFailureException`.
//
// Kept in its own file rather than appended to System.fs: a second namespace in
// that file makes the generated module glob-import System's own Exception
// alongside its definition, which Rust rejects as a duplicate name.
exception MatchFailureException of string * int * int
