#r "packages/FAKE/tools/FakeLib.dll"

open System.IO
open Fake

// Directories
let mainBuildDir = "build/main/"
let testBuildDir = "build/test/"
let pluginsBuildDir = "build/plugins/"

// Filesets
let appReferences  = !! "src/**/*.fsproj"

// version info
let version = "0.1"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    !! mainBuildDir ++ testBuildDir ++ pluginsBuildDir
        ++ "src/**/bin/" ++ "test/**/bin/"
        ++ "src/**/obj/" ++ "test/**/obj/"
    |> CleanDirs
)

Target "NUnitTest" (fun _ ->
    !! "test/**/*.fsproj"
    |> MSBuildRelease testBuildDir "Build"
    |> Log "Release-Output: "
    
    [testBuildDir + "Fable.Tests.dll"]
    |> NUnit (fun p ->
        { p with
            DisableShadowCopy = true
            OutputFile = "TestResults.xml" })
)

Target "MochaTest" (fun _ ->
    let buildParam command p =
        { p with
            NpmHelper.NpmFilePath =
                environVarOrDefault "NPM_PATH" NpmHelper.defaultNpmParams.NpmFilePath
            NpmHelper.Command =
                NpmHelper.Run command }
    NpmHelper.Npm (buildParam "test-compile")
    NpmHelper.Npm (buildParam "test")
)

Target "MainRelease" (fun _ ->
    let xmlPath = Path.Combine(Path.GetFullPath mainBuildDir, "Fable.xml")
    !! "src/**/*.fsproj"
    |> MSBuild mainBuildDir "Build"
        ["Configuration","Release"; "DocumentationFile", xmlPath]
    |> Log "Release-Output: "
)

Target "MainDebug" (fun _ ->
    !! "src/**/*.fsproj"
    |> MSBuildDebug mainBuildDir "Build"
    |> Log "Debug-Output: "
)

Target "CopyLib" (fun _ ->
    FileUtils.cp "lib/fable-core.js" mainBuildDir
)

Target "Plugins" (fun _ ->
    CreateDir "build/plugins"
    [ "plugins/Fable.Plugins.NUnit.fsx" ]
    |> Seq.iter (fun fsx ->
        [fsx]
        |> FscHelper.compile [
            FscHelper.Out ("build/" + Path.ChangeExtension(fsx, ".dll"))
            FscHelper.Target FscHelper.TargetType.Library
        ]
        |> function
            | 0 -> ()
            | _ -> failwithf "Cannot compile %s" fsx)
)

Target "Release" ignore

Target "All" ignore

// Build order
"Clean"
  ==> "MainRelease"
  ==> "Plugins"
  ==> "CopyLib"
  ==> "Release"

"Plugins"
  ==> "MochaTest"
  ==> "NUnitTest"
  ==> "All"

// Start build
RunTargetOrDefault "Release"

