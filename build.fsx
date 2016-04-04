#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Fake

// version info
let version = "0.1.6"  // or retrieve from CI server

module Util =
    open System.Net
    
    let join pathParts =
        Path.Combine(Array.ofSeq pathParts)

    let run workingDir fileName args =
        let ok = 
            execProcess (fun info ->
                info.FileName <- fileName
                info.WorkingDirectory <- workingDir
                info.Arguments <- args) TimeSpan.MaxValue
        if not ok then failwith (sprintf "'%s> %s %s' task failed" workingDir fileName args)

    let downloadArtifact path =
        let url = "https://ci.appveyor.com/api/projects/alfonsogarciacaro/fable/artifacts/build/fable.zip"
        let tempFile = Path.ChangeExtension(Path.GetTempFileName(), ".zip")
        use client = new WebClient()
        use stream = client.OpenRead(url)
        use writer = new StreamWriter(tempFile)
        stream.CopyTo(writer.BaseStream)
        FileUtils.mkdir path
        CleanDir path
        run path "unzip" (sprintf "-q %s" tempFile)
        File.Delete tempFile

    let rmdir dir =
        if EnvironmentHelper.isUnix
        then FileUtils.rm_rf dir
        // Use this in Windows to prevent conflicts with paths too long
        else run "." "cmd" ("/C rmdir /s /q " + Path.GetFullPath dir)

    /// Reads a file line by line and rewrites it using unix line breaks
    ///  - uses a temp file to store the contents in order to prevent OutOfMemory exceptions
    let convertFileToUnixLineBreaks(fileName : string) = 
        use reader = new StreamReader(fileName, encoding)
        let tempFileName = Path.GetTempFileName()
        use writer = new StreamWriter(tempFileName, false, encoding)
        while not reader.EndOfStream do
          writer.Write (reader.ReadLine() + "\n")
        reader.Close()
        writer.Close()
        File.Delete(fileName)
        File.Move(tempFileName, fileName)

module Npm =
    let npmFilePath args =
        if EnvironmentHelper.isUnix
        then "npm", args
        else "cmd", ("/C npm " + args)

    let script workingDir script args =
        sprintf "run %s -- %s" script (String.concat " " args)
        |> npmFilePath ||> Util.run workingDir

    let install workingDir modules =
        sprintf "install %s" (String.concat " " modules)
        |> npmFilePath ||> Util.run workingDir

    let command workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> npmFilePath ||> Util.run workingDir
        
module Node =
    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "node" args

// Directories
let fableBuildDir = Util.join ["build";"fable";"bin"]
let testsBuildDir = Util.join ["build";"tests"]
let pluginsBuildDir = Util.join ["build";"plugins"]

// Targets
Target "Clean" (fun _ ->
    !! fableBuildDir ++ pluginsBuildDir
        ++ "src/**/bin/" ++ "src/**/obj/"
    |> CleanDirs
    // Exclude node_modules
    !! "build/fable/**/*.*" -- "build/fable/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
    !! "build/tests/**/*.*" -- "build/tests/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
)

Target "FableRelease" (fun _ ->
    let xmlPath = Path.Combine(Path.GetFullPath fableBuildDir, "Fable.xml")
    !! "src/fable-fsharp/Fable.fsproj"
    |> MSBuild fableBuildDir "Build"
        ["Configuration","Release"; "DocumentationFile", xmlPath]
    |> Log "Release-Output: "
    
    // For some reason, ProjectCracker targets are not working after updating the package
    !! "packages/FSharp.Compiler.Service.ProjectCracker/utilities/net45/FSharp.Compiler.Service.ProjectCrackerTool.exe*"
    |> Seq.iter (fun x -> FileUtils.cp x "build/fable/bin")
)

Target "FableDebug" (fun _ ->
    !! "src/fable-fsharp/Fable.fsproj"
    |> MSBuildDebug fableBuildDir "Build"
    |> Log "Debug-Output: "
    let targetDir = "build/fable"
    FileUtils.cp_r "src/fable-js" targetDir
    Npm.command targetDir "version" [version]
)

Target "FableJs" (fun _ ->
    let targetDir = "build/fable"
    FileUtils.cp_r "src/fable-js" targetDir
    FileUtils.cp "README.md" targetDir
    Npm.command targetDir "version" [version]
    Npm.install targetDir []
)

Target "NUnitTest" (fun _ ->
    !! "src/tests/Fable.Tests.fsproj"
    |> MSBuildRelease testsBuildDir "Build"
    |> Log "Release-Output: "
    
    [Path.Combine(testsBuildDir, "Fable.Tests.dll")]
    |> NUnit (fun p -> { p with DisableShadowCopy = true 
                                OutputFile = Path.Combine(testsBuildDir, "TestResult.xml") })
)

Target "MochaTest" (fun _ ->
    Node.run "." "build/fable" [
        "src/tests/Fable.Tests.fsproj"
        "--env"; "node"
        "--outDir"; testsBuildDir
        "--plugins"; "build/plugins/Fable.Plugins.NUnit.dll"
    ]
    FileUtils.cp "src/tests/package.json" testsBuildDir
    Npm.install testsBuildDir []
    Npm.script testsBuildDir "test" []
    
    // Clampled and non-clamped arrays
    Node.run "." "build/fable" ["src/tests/Other/nonClamped.fsx"; "--outDir"; "build/tests/Other"; "--env"; "node"]
    Node.run "." "build/fable" ["src/tests/Other/clamped.fsx"; "--clamp"; "--outDir"; "build/tests/Other"; "--env"; "node"]
    Node.run "." "build/tests/Other/nonClamped" []
    Node.run "." "build/tests/Other/clamped" []
)

Target "Plugins" (fun _ ->
    CreateDir pluginsBuildDir
    [ "src/plugins/Fable.Plugins.NUnit.fsx"; "src/plugins/Fable.Plugins.VisualStudio.UnitTests.fsx" ]
    |> Seq.iter (fun fsx ->
        let dllFile = Path.ChangeExtension(Path.GetFileName fsx, ".dll")
        [fsx]
        |> FscHelper.compile [
            FscHelper.Out (Path.Combine(pluginsBuildDir, dllFile))
            FscHelper.Target FscHelper.TargetType.Library
        ]
        |> function 0 -> () | _ -> failwithf "Cannot compile %s" fsx)
)

Target "MakeArtifactLighter" (fun _ ->
    Util.rmdir "build/fable/node_modules"
    !! "build/fable/bin/*.pdb" ++ "build/fable/bin/*.xml"
    |> Seq.iter FileUtils.rm
)

Target "Publish" (fun _ ->
    let workingDir = "temp/build"
    Util.downloadArtifact workingDir
    Util.convertFileToUnixLineBreaks (Path.Combine(workingDir, "index.js"))
    // Npm.command workingDir "version" [version]
    Npm.command workingDir "publish" []
)

Target "Import" (fun _ ->
    !! "import/core/Fable.Import.fsproj"
    |> MSBuildRelease "import/core" "Build"
    |> Log "Import-Output: "
)

Target "Samples" (fun _ ->
    let fableDir = Util.join ["build";"fable"] |> Path.GetFullPath    
    !! "samples/**/fableconfig.json"
    |> Seq.iter (fun path ->
        let pathDir = Path.GetDirectoryName path
        Node.run pathDir fableDir [])
)

Target "All" ignore

// Build order
"Clean"
  ==> "FableRelease"
  ==> "FableJs"
  ==> "Plugins"
  ==> "MochaTest"
  =?> ("MakeArtifactLighter", environVar "APPVEYOR" = "True")
  ==> "All"

// Start build
RunTargetOrDefault "All"
