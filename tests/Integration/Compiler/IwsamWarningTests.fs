module Fable.Tests.Compiler.IwsamWarnings

open System
open System.IO
open Expecto
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices

let private check name source =
    let file = Path.Combine(__SOURCE_DIRECTORY__, name + ".fs")
    let references =
        (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") :?> string).Split(Path.PathSeparator)
        |> Array.map (fun path -> "-r:" + path)

    let options: FSharpProjectOptions =
        {
            ProjectFileName = file + "proj"
            ProjectId = None
            SourceFiles = [| file |]
            OtherOptions =
                Array.append
                    [| "--target:library"; "--targetprofile:netcore"; "--noframework"; "--langversion:preview" |]
                    references
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime.MaxValue
            UnresolvedReferences = None
            OriginalLoadReferences = []
            Stamp = None
        }

    let checker = InteractiveChecker.Create(options)
    checker.ParseAndCheckProject(options.ProjectFileName, options.SourceFiles, fun _ -> 0, lazy source)
    |> Async.RunSynchronously
    |> fun result -> result.Diagnostics

let private cases = [
    "ordinary inherited generic interface", 0, """module Test
type I<'T> = abstract M: 'T -> 'T
type J<'T> = inherit I<'T>
let f (x: J<int>) = x
""";
    "static abstract warns at each use", 2, """module Test
#nowarn "3535"
type I<'T> = static abstract M: 'T -> 'T
let f (x: I<int>) = x
let g (x: I<string>) = x
""";
    "inherited static abstract", 1, """module Test
#nowarn "3535"
type I<'T> = static abstract M: 'T -> 'T
type J<'T> = inherit I<'T>
let f (x: J<int>) = x
""";
    "concrete static member", 0, """module Test
[<Interface>]
type I =
    abstract M: unit -> int
    static member Create() = 1
let f (x: I) = x
""";
    "recursive interface declarations", 2, """module rec Test
#nowarn "3535"
type I = abstract M: J -> unit
and J = static abstract Create: unit -> J
let f (x: J) = x
""";
    "imported static abstract interface", 1, """module Test
let f (x: System.IParsable<int>) = x
""";
    "concrete static member hides inherited slot", 0, """module Test
#nowarn "3535"
type I = static abstract M: unit -> int
[<Interface>]
type J =
    inherit I
    static member M() = 1
let f (x: J) = x
""";
]

let tests =
    testList "IWSAM warnings" [
        for name, expectedCount, source in cases do
            testCase name <| fun _ ->
                let diagnostics = check name source
                let errors = diagnostics |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
                Expect.isEmpty errors "The interface declarations should type-check"
                let warnings = diagnostics |> Array.filter (fun d -> d.ErrorNumber = 3536)
                Expect.equal warnings.Length expectedCount "Preserve static-abstract interface usage warnings"
    ]
