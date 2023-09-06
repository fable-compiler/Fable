namespace Build.Tests

open Fun.Build
open Build.FableLibrary
open System.IO

type TestsJavaScript () =
    inherit TestsTarget(
        "javascript",
        BuildFableLibraryPython(),
        Path.Combine("tests", "Js", "Main"),
        Path.Combine("build", "tests", "JavaScript")
    )

    override this.TestsAgainstTargetStage =
        stage "Run tests against JavaScript" {
            workingDir this.BuildDir

            run "npx mocha --reporter dot -t 10000"
        }
