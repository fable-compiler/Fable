/// A whole program, run end to end through the generated `main.erl` shim.
///
/// The Beam test suite calls test functions directly, so nothing in it ever exercises the shim —
/// which is how it stayed broken while the suite was green: an F# `[<EntryPoint>]` lowers to
/// `main/1`, and the shim called `main/0`, so every Beam program with an entry point died with
/// `undef`. The runner in `Build.Test.Beam` compiles this project, runs it on the BEAM, and checks
/// what it printed and what it exited with.
module Fable.Tests.Program.Main

[<EntryPoint>]
let main argv =
    // Argv has to survive the crossing from a plain Erlang list into F#'s `string[]`.
    printfn "argc=%d" argv.Length
    printfn "argv=%s" (String.concat "," argv)

    // `Console.WriteLine` and `printfn` used to have contradictory encoding contracts — one wrote
    // the UTF-8 binary's raw bytes, the other wrote it as unicode — so no device setting made both
    // print a non-ASCII string correctly. Both go out through the shim's unicode device now, and
    // this is the only test that observes what actually reaches a terminal.
    System.Console.WriteLine("writeline=✓ ✗ · é")
    printfn "printfn=✓ ✗ · é"

    // ...and the return value has to become the process exit code, or a failing suite is
    // indistinguishable from a passing one.
    match argv with
    | [| "fail" |] -> 3
    | _ -> 0
