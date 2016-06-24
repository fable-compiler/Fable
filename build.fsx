#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Fake

// version info
let version = "0.3.24"
let coreLibVersion = "0.1.9"

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

    let runAndReturn workingDir fileName args =
        ExecProcessAndReturnMessages (fun info ->
            info.FileName <- fileName
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
        |> fun p -> p.Messages |> String.concat "\n"

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

    /// Reads a file line by line and rewrites it using the visitor to modify each line
    ///  - uses a temp file to store the contents in order to prevent OutOfMemory exceptions
    let visitFile (visitor: string->string) (fileName : string) = 
        use reader = new StreamReader(fileName, encoding)
        let tempFileName = Path.GetTempFileName()
        use writer = new StreamWriter(tempFileName, false, encoding)
        while not reader.EndOfStream do
            reader.ReadLine() |> visitor |> writer.WriteLine
        reader.Close()
        writer.Close()
        File.Delete(fileName)
        File.Move(tempFileName, fileName)
        
    let compileScript symbols outDir fsxPath =
        let dllFile = Path.ChangeExtension(Path.GetFileName fsxPath, ".dll")
        let opts = [
            yield FscHelper.Out (Path.Combine(outDir, dllFile))
            yield FscHelper.Target FscHelper.TargetType.Library
            yield! symbols |> List.map FscHelper.Define
        ]
        FscHelper.compile opts [fsxPath]
        |> function 0 -> () | _ -> failwithf "Cannot compile %s" fsxPath

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

    let commandAndReturn workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> npmFilePath ||> Util.runAndReturn workingDir

module Node =
    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "node" args

module Fake = 
    let fakePath = "packages" </> "docs" </> "FAKE" </> "tools" </> "FAKE.exe"
    let fakeStartInfo script workingDirectory args fsiargs environmentVars =
        (fun (info: System.Diagnostics.ProcessStartInfo) ->
            info.FileName <- System.IO.Path.GetFullPath fakePath
            info.Arguments <- sprintf "%s --fsiargs -d:FAKE %s \"%s\"" args fsiargs script
            info.WorkingDirectory <- workingDirectory
            let setVar k v = info.EnvironmentVariables.[k] <- v
            for (k, v) in environmentVars do setVar k v
            setVar "MSBuild" msBuildExe
            setVar "GIT" Git.CommandHelper.gitPath
            setVar "FSI" fsiPath)

    /// Run the given buildscript with FAKE.exe
    let executeFAKEWithOutput workingDirectory script fsiargs envArgs =
        let exitCode =
            ExecProcessWithLambdas
                (fakeStartInfo script workingDirectory "" fsiargs envArgs)
                TimeSpan.MaxValue false ignore ignore
        System.Threading.Thread.Sleep 1000
        exitCode

// Directories
let fableBuildDir = Util.join ["build";"fable";"bin"]
let testsBuildDir = Util.join ["build";"tests"]

// Targets
Target "Clean" (fun _ ->
    !! fableBuildDir ++ "src/**/bin/" ++ "src/**/obj/"
    |> CleanDirs
    // Exclude node_modules
    !! "build/fable/**/*.*" -- "build/fable/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
    !! "build/tests/**/*.*" -- "build/tests/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
)

Target "FableRelease" (fun _ ->
    let xmlPath = Path.Combine(Path.GetFullPath fableBuildDir, "Fable.xml")
    !! "src/fable-client-node/Fable.Client.Node.fsproj"
    |> MSBuild fableBuildDir "Build"
        ["Configuration","Release"; "DocumentationFile", xmlPath]
    |> Log "Release-Output: "
    
    // For some reason, ProjectCracker targets are not working after updating the package
    !! "packages/FSharp.Compiler.Service.ProjectCracker/utilities/net45/FSharp.Compiler.Service.ProjectCrackerTool.exe*"
    |> Seq.iter (fun x -> FileUtils.cp x "build/fable/bin")
)

Target "FableDebug" (fun _ ->
    !! "src/fable-client-node/Fable.Client.Node.fsproj"
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

Target "FableSuave" (fun _ ->
    let buildDir = "build/suave"
    !! "src/fable-client-suave/Fable.Client.Suave.fsproj"
    |> MSBuildDebug buildDir "Build"
    |> Log "Debug-Output: "
    // Copy Fable.Core.dll to buildDir so it can be referenced by F# code
    FileUtils.cp "import/core/Fable.Core.dll" buildDir
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
    Node.run "." "build/fable" ["src/tests/Other"]
    Node.run "." "build/fable" ["src/tests/"]
    FileUtils.cp "src/tests/package.json" testsBuildDir
    Npm.install testsBuildDir []
    // Copy the development version of fable-core.js
    if environVar "DEV_MACHINE" = "1" then
        FileUtils.cp "import/core/fable-core.js" "build/tests/node_modules/fable-core/"
    Npm.script testsBuildDir "test" []
)

Target "Plugins" (fun _ ->
    !! "src/plugins/**/*.fsx"
    |> Seq.iter (fun fsx -> Util.compileScript [] (Path.GetDirectoryName fsx) fsx)
)

Target "Providers" (fun _ ->
    !! "src/providers/**/*.fsx"
    |> Seq.filter (fun path -> path.Contains("test") |> not)    
    |> Seq.iter (fun fsxPath ->
        let buildDir = Path.GetDirectoryName(Path.GetDirectoryName(fsxPath))
        Util.compileScript ["NO_GENERATIVE"] buildDir fsxPath)
)

Target "MakeArtifactLighter" (fun _ ->
    Util.rmdir "build/fable/node_modules"
    !! "build/fable/bin/*.pdb" ++ "build/fable/bin/*.xml"
    |> Seq.iter FileUtils.rm
)

Target "Publish" (fun _ ->
    let workingDir = "temp/build"
    Util.downloadArtifact workingDir
    // Npm.command workingDir "version" [version]
    Npm.command workingDir "publish" []
)

Target "CoreLib" (fun _ ->
    Util.run "." "babel" "import/core/es2015.js -o import/core/fable-core.js --plugins ../../build/fable/node_modules/babel-plugin-transform-es2015-modules-umd"
    Util.run "." "babel" "import/core/es2015.js -o import/core/commonjs.js --plugins ../../build/fable/node_modules/babel-plugin-transform-es2015-modules-commonjs"
    Util.run "." "uglifyjs" "import/core/fable-core.js -c -m -o import/core/fable-core.min.js"
    !! "import/core/Fable.Core.fsproj"
    |> MSBuildRelease "import/core" "Build"
    |> Log "CoreLib-Output: "
)

Target "UpdateCoreLibVersion" (fun _ -> 
    match Npm.commandAndReturn "import/core" "version" ["-v"] with
    | v when v = coreLibVersion -> ()
    | _ ->
        Npm.command "import/core" "version" [coreLibVersion]
        !! "samples/**/package.json"
        |> Seq.iter (Util.visitFile (fun line ->
            match Regex.Match(line, "^(\s*)\"(fable(?:-core)?)\": \".*?\"(,)?") with
            | m when m.Success ->
                match m.Groups.[2].Value with
                | "fable" -> sprintf "%s\"fable\": \"^%s\"%s" m.Groups.[1].Value version m.Groups.[3].Value
                | "fable-core" -> sprintf "%s\"fable-core\": \"^%s\"%s" m.Groups.[1].Value coreLibVersion m.Groups.[3].Value
                | _ -> line                 
            | _ -> line))
)

Target "Samples" (fun _ ->
    let fableDir = Util.join ["build";"fable"] |> Path.GetFullPath    
    !! "samples/**/out/" |> CleanDirs
    !! "samples/**/fableconfig.json"
    |> Seq.iter (fun path ->
        let pathDir = Path.GetDirectoryName path
        Node.run pathDir fableDir [])
)

Target "LineCount" (fun _ ->
    !! "src/fable-fsharp/**/*.fs"
    |> Seq.map (File.ReadLines >> Seq.length)
    |> Seq.sum
    |> printfn "Line count: %i"
)

Target "BrowseDocs" (fun _ ->
    let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "BrowseDocs"]
    if exit <> 0 then failwith "Browsing documentation failed"
)

Target "GenerateDocs" (fun _ ->
    let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "GenerateDocs"]
    if exit <> 0 then failwith "Generating documentation failed"
)

Target "PublishDocs" (fun _ ->
    let exit = Fake.executeFAKEWithOutput "docs" "docs.fsx" "" ["target", "PublishDocs"]
    if exit <> 0 then failwith "Publishing documentation failed"
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

"CoreLib"
  ==> "UpdateCoreLibVersion"

// Start build
RunTargetOrDefault "All"
