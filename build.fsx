#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Fake

// version info
let version = "0.1.0"  // or retrieve from CI server

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
let samplesBuildDir = Util.join ["build";"samples"]

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
    let fableDir = Util.join ["build";"fable"] |> Path.GetFullPath
    let testsBuildDir = Path.GetFullPath testsBuildDir
    FileUtils.cp_r "src/tests" testsBuildDir
    Npm.install testsBuildDir []
    Node.run testsBuildDir fableDir []
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
    let fableDir = Util.join ["build";"fable"] |> Path.GetFullPath
    let samplesBasePath = Path.GetFullPath "samples"
    let samplesBuilDir = Path.GetFullPath samplesBuildDir
    
    !! "samples/**/fableconfig.json"
    |> Seq.iter (fun path ->
        let pathDir = Path.GetDirectoryName path
        let outDir = pathDir.Replace(samplesBasePath, samplesBuildDir)
        FileUtils.cp_r pathDir outDir
        if Path.Combine(outDir, "package.json") |> File.Exists then
            Npm.install outDir []
        Node.run outDir fableDir []
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

Target "Import" (fun _ ->
    !! "import/core/Fable.Import.fsproj"
    |> MSBuildRelease "import/core" "Build"
    |> Log "Import-Output: "
)

Target "CleanSamples" (fun _ ->
    !! "/samples/**/*.js.map"
    |> Seq.where (fun file -> not(file.Contains "node_modules"))
    |> Seq.iter FileUtils.rm

    !! "/samples/**/*.js"
    |> Seq.where (fun file -> not(file.Contains "node_modules"))
    |> Seq.where (fun file ->
        Regex.IsMatch(file,"(?:bundle|fable-core)\.js$")
        || File.Exists(Path.ChangeExtension(file, ".fs"))
        || File.Exists(Path.ChangeExtension(file, ".fsx")))
    |> Seq.iter FileUtils.rm
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
