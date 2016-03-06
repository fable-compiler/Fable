#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open Fake

// Directories
let fableBuildDir = "build/fable/bin"
let testsBuildDir = "build/tests"
let pluginsBuildDir = "build/plugins"
let samplesBuildDir = "build/samples"

// version info
let version = "0.0.1"  // or retrieve from CI server

module Util =
    let run workingDir fileName args =
        let ok = 
            execProcess (fun info ->
                info.FileName <- fileName
                info.WorkingDirectory <- workingDir
                info.Arguments <- args) TimeSpan.MaxValue
        if not ok then failwith (sprintf "'%s> %s %s' task failed" workingDir fileName args)

module Npm =
    let run workingDir command args =
        sprintf "run %s -- %s" command (String.concat " " args)
        |> Util.run workingDir "npm"

    let install workingDir modules =
        sprintf "install %s" (String.concat " " modules)
        |> Util.run workingDir "npm"
        
module Node =
    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "node" args

// Targets
Target "Clean" (fun _ ->
    !! "build" ++ "src/**/bin/" ++ "src/**/obj/"
    |> CleanDirs
)

Target "FableRelease" (fun _ ->
    // let xmlPath = Path.Combine(Path.GetFullPath fableBuildDir, "Fable.xml")
    !! "src/fable-fsharp/Fable.fsproj"
    |> MSBuild fableBuildDir "Build"
        ["Configuration","Release"] //"DocumentationFile", xmlPath]
    |> Log "Release-Output: "
)

Target "FableDebug" (fun _ ->
    !! "src/fable-fsharp/Fable.fsproj"
    |> MSBuildDebug fableBuildDir "Build"
    |> Log "Debug-Output: "
)

Target "FableJs" (fun _ ->
    let targetDir = "build/fable"
    FileUtils.cp_r "src/fable-js" targetDir
    FileUtils.cp "README.md" targetDir
    Npm.install targetDir []
)

Target "NUnitTest" (fun _ ->
    !! "src/tests/Fable.Tests.fsproj"
    |> MSBuildRelease testsBuildDir "Build"
    |> Log "Release-Output: "
    
    [Path.Combine(testsBuildDir, "Fable.Tests.dll")]
    |> NUnit (fun p -> { p with DisableShadowCopy = true })
)

Target "MochaTest" (fun _ ->
    let testsBuildDir = Path.GetFullPath testsBuildDir
    Node.run "build/fable" "." [
        Path.GetFullPath "src/tests/Fable.Tests.fsproj"
        "--outDir"; testsBuildDir
        "--plugins"; Path.GetFullPath "build/plugins/Fable.Plugins.NUnit.dll"
        ]
    Npm.install testsBuildDir ["mocha"]
    Node.run testsBuildDir "node_modules/mocha/bin/mocha" ["."]
)

Target "Plugins" (fun _ ->
    CreateDir pluginsBuildDir
    [ "src/plugins/Fable.Plugins.NUnit.fsx" ]
    |> Seq.iter (fun fsx ->
        let dllFile = Path.ChangeExtension(Path.GetFileName fsx, ".dll")
        [fsx]
        |> FscHelper.compile [
            FscHelper.Out (Path.Combine(pluginsBuildDir, dllFile))
            FscHelper.Target FscHelper.TargetType.Library
        ]
        |> function 0 -> () | _ -> failwithf "Cannot compile %s" fsx)
)

Target "Samples" (fun _ ->
    CleanDir samplesBuildDir
    let samplesBasePath = Path.GetFullPath "samples"
    let samplesBuilDir = Path.GetFullPath samplesBuildDir
    
    !! "samples/**/index.fsx" ++ "samples/**/*.fsproj"
    |> Seq.iter (fun path ->
        let pathDir = Path.GetDirectoryName path
        let outDir = pathDir.Replace(samplesBasePath, samplesBuildDir)
        FileUtils.cp_r pathDir outDir
        Npm.install outDir []
        Node.run "build/fable" "." [Path.Combine(outDir, Path.GetFileName path) |> Path.GetFullPath]
    )
)

Target "All" ignore

// Build order
"Clean"
  ==> "FableRelease"
  ==> "FableJs"
  ==> "Plugins"
  ==> "MochaTest"
  ==> "All"

// Start build
RunTargetOrDefault "All"
