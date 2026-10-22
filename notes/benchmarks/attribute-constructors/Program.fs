open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Symbols

let declaration constructors application =
    "module Test\nopen System\n[<AttributeUsage(AttributeTargets.All)>]\ntype AAttribute =\n    inherit Attribute\n"
    + constructors
    + "\n    member _.Name with get() = \"\" and set(value: string) = ()\n[<A("
    + application
    + ")>]\ntype Marked = class end\n"

let cases =
    [
        "ascending",
        false,
        declaration "    new () = { inherit Attribute() }\n    new (x: int) = { inherit Attribute() }" "1"
        "descending",
        false,
        declaration "    new (x: int) = { inherit Attribute() }\n    new () = { inherit Attribute() }" ""
        "equal-arity",
        false,
        declaration "    new (x: string) = { inherit Attribute() }\n    new (x: int) = { inherit Attribute() }" "1"
        "named-property",
        false,
        declaration
            "    new () = { inherit Attribute() }\n    new (x: int) = { inherit Attribute() }"
            "1, Name = \"named\""
        "private",
        true,
        declaration "    private new (x: int) = { inherit Attribute() }\n    new () = { inherit Attribute() }" "1"
        "bad-argument",
        true,
        declaration "    new (x: string) = { inherit Attribute() }\n    new (x: int) = { inherit Attribute() }" "true"
        "bad-arity",
        true,
        declaration "    new (x: int, y: int) = { inherit Attribute() }\n    new () = { inherit Attribute() }" "1"
        "ambiguous",
        true,
        declaration "    new (x: string) = { inherit Attribute() }\n    new (x: Type) = { inherit Attribute() }" "null"
        "three-constructors",
        false,
        declaration
            "    new (x: string) = { inherit Attribute() }\n    new (x: int) = { inherit Attribute() }\n    new () = { inherit Attribute() }"
            "\"str\""
        "imported",
        false,
        "module Test\n[<System.Obsolete(\"message\", false)>]\ntype Marked = class end\nlet x (v: Marked) = v\n"
    ]

[<EntryPoint>]
let main argv =
    let refs =
        (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") :?> string).Split(Path.PathSeparator)
        |> Array.map (fun p -> "-r:" + p)

    let snapshot = ResizeArray<string>()

    for name, expectError, source in cases do
        let path = Path.Combine(__SOURCE_DIRECTORY__, name + ".fs")

        let options: FSharpProjectOptions =
            {
                ProjectFileName = path + "proj"
                ProjectId = None
                SourceFiles = [| path |]
                OtherOptions =
                    Array.append
                        [|
                            "--target:library"
                            "--targetprofile:netcore"
                            "--noframework"
                            "--langversion:preview"
                        |]
                        refs
                ReferencedProjects = [||]
                IsIncompleteTypeCheckEnvironment = false
                UseScriptResolutionRules = false
                LoadTime = DateTime.MaxValue
                UnresolvedReferences = None
                OriginalLoadReferences = []
                Stamp = None
            }

        let checker = InteractiveChecker.Create(options)

        let result =
            checker.ParseAndCheckProject(options.ProjectFileName, [| path |], fun _ -> 0, lazy source)
            |> Async.RunSynchronously

        let hasError =
            result.Diagnostics
            |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

        if hasError <> expectError then
            failwithf "%s: unexpected diagnostics %A" name result.Diagnostics

        snapshot.Add(name)

        for d in result.Diagnostics do
            snapshot.Add(
                sprintf
                    "%d|%A|%d:%d-%d:%d|%s"
                    d.ErrorNumber
                    d.Severity
                    d.StartLine
                    d.StartColumn
                    d.EndLine
                    d.EndColumn
                    d.Message
            )

        let rec inspect (entity: FSharpEntity) =
            if entity.DisplayName = "Marked" then
                for attr in entity.Attributes do
                    snapshot.Add(
                        sprintf
                            "attribute:%s|%A|%A"
                            attr.AttributeType.FullName
                            (Seq.toList attr.ConstructorArguments)
                            (Seq.toList attr.NamedArguments)
                    )

            for child in entity.NestedEntities do
                inspect child

        for entity in result.AssemblySignature.Entities do
            inspect entity

        printfn "%s: %d diagnostics" name result.Diagnostics.Length

    File.WriteAllLines(argv[0], snapshot)

    if argv.Length > 1 && File.ReadAllText(argv[0]) <> File.ReadAllText(argv[1]) then
        failwith "Attribute diagnostics changed (including ordering, messages or ranges)"

    0
