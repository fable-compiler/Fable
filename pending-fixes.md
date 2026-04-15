# Pending Fixes and Actions

## Fix for #4080 (Watch mode file lock)

**File**: `src/Fable.Compiler/File.fs`
**Status**: Implemented locally but branch lost (safeoutputs blocked). Re-implement next run.

### Changes needed:

1. Add `open System.Threading` after `open System.IO`

2. Replace `readAllTextNonBlocking` body with retry logic:
```fsharp
let readAllTextNonBlocking (path: string) =
    if File.Exists(path) then
        let readText () =
            use fileStream =
                new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
            use textReader = new StreamReader(fileStream)
            textReader.ReadToEnd()
        // Retry a few times with short delays to handle transient file locks
        // (e.g., editors briefly locking a file on save). See #4080.
        try
            readText ()
        with :? IOException ->
            Thread.Sleep(50)
            try
                readText ()
            with :? IOException ->
                Thread.Sleep(100)
                readText ()
    else
        ""
```

3. Changelog: add to `## Unreleased > ### Fixed` in both changelogs:
   `* [All] Fix watch mode stopping when a source file is briefly locked by an editor on save (#4080)`

## Comment to Post on #4224 (JSX match case children)

🤖 *This is an automated response from Repo Assist.*

**Root Cause**: JSX `Unroller` in `src/Fable.Transforms/Fable2Babel.fs` (~line 2157) handles simple arrow functions, but when a `match` expression has complex branches (loops, sequences), it generates a block-body lambda that doesn't match any pattern. The unevaluated lambda ends up as a JSX child — React errors "Functions are not valid as a React child."

**Workaround**: Compute children before JSX:
```fsharp
let items = [
    match counter with
    | 0 -> Html.li "No items!"
    | n -> for i in 1..n do Html.li [prop.key i; prop.text $"Item {i}"]
]
Fable.Core.JSX.create "ul" ["children" ==> items]
```

**Fix location**: The `Unroller` active pattern needs a new case to handle block-body lambdas with complex control flow.
