namespace Build.Tests

open Fun.Build
open Build.FableLibrary
open System.IO

type TestsPython () =
    inherit TestsTarget(
        "python",
        BuildFableLibraryPython(),
        Path.Combine("tests", "Python"),
        Path.Combine("build", "tests", "Python")
    )

    override this.TestsAgainstTargetStage =
        stage "Run tests against Python" {
            workingDir this.BuildDir

            run "poetry run pytest -x"
        }
