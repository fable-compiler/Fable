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
let version = "0.0.9"  // or retrieve from CI server

module Util =
    open System.Net

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
    let npmFilePath =
        if EnvironmentHelper.isUnix
        then "npm"
        else NpmHelper.defaultNpmParams.NpmFilePath |> Path.GetFullPath

    let script workingDir script args =
        sprintf "run %s -- %s" script (String.concat " " args)
        |> Util.run workingDir npmFilePath

    let install workingDir modules =
        sprintf "install %s" (String.concat " " modules)
        |> Util.run workingDir npmFilePath

    let command workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.run workingDir npmFilePath
        
module Node =
    let nodeFilePath =
        if EnvironmentHelper.isUnix
        then "node"
        else "./packages/Node.js/node.exe" |> Path.GetFullPath

    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir nodeFilePath args

// Targets
Target "Clean" (fun _ ->
    !! "/build" ++ "src/**/bin/" ++ "src/**/obj/"
    |> Seq.iter Util.rmdir
)

Target "FableRelease" (fun _ ->
    let xmlPath = Path.Combine(Path.GetFullPath fableBuildDir, "Fable.xml")
    !! "src/fable-fsharp/Fable.fsproj"
    |> MSBuild fableBuildDir "Build"
        ["Configuration","Release"; "DocumentationFile", xmlPath]
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
    Path.Combine(testsBuildDir, "node_modules/mocha/bin/mocha")
    |> Path.GetFullPath
    |> Node.run testsBuildDir <| ["."]
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
    !! samplesBuildDir
        ++ "samples/**/node_modules/"
        ++ "samples/**/bin/" ++ "samples/**/obj/"
    |> CleanDirs
    let samplesBasePath = Path.GetFullPath "samples"
    let samplesBuilDir = Path.GetFullPath samplesBuildDir
    
    !! "samples/**/*.fsproj" ++ "samples/**/index.fsx"
    |> Seq.iter (fun path ->
        let pathDir = Path.GetDirectoryName path
        let outDir = pathDir.Replace(samplesBasePath, samplesBuildDir)
        FileUtils.cp_r pathDir outDir
        if Path.Combine(outDir, "package.json") |> File.Exists then
            Npm.install outDir []
        Node.run "build/fable" "." [Path.Combine(outDir, Path.GetFileName path) |> Path.GetFullPath]
    )
)

Target "DeleteNodeModules" (fun _ ->
    // Delete node_modules to make the artifact lighter
    Util.rmdir "build/fable/node_modules"
)

Target "Publish" (fun _ ->
    let workingDir = "temp/build"
    Util.downloadArtifact workingDir
    Util.convertFileToUnixLineBreaks (Path.Combine(workingDir, "index.js"))
    Npm.command workingDir "version" [version]
    Npm.command workingDir "publish" []
)

Target "All" ignore

// Build order
"Clean"
  ==> "FableRelease"
  ==> "FableJs"
  ==> "Plugins"
  ==> "MochaTest"
  =?> ("DeleteNodeModules", environVar "APPVEYOR" = "True")
  ==> "All"

// Start build
RunTargetOrDefault "All"
