#r "packages/FAKE/tools/FakeLib.dll"

open System
open System.IO
open System.Text.RegularExpressions
open Fake
open Fake.AssemblyInfoFile

// version info
let fableCompilerVersion = "0.5.6"
let fableCoreVersion = "0.5.4"
let fableCompilerNetcoreVersion = "0.5.7"

module Util =
    open System.Net
    
    let join pathParts =
        Path.Combine(Array.ofSeq pathParts)

    let run workingDir fileName args =
        let fileName, args =
            if EnvironmentHelper.isUnix
            then fileName, args else "cmd", ("/C " + fileName + " " + args)
        let ok =
            execProcess (fun info ->
                info.FileName <- fileName
                info.WorkingDirectory <- workingDir
                info.Arguments <- args) TimeSpan.MaxValue
        if not ok then failwith (sprintf "'%s> %s %s' task failed" workingDir fileName args)

    let runAndReturn workingDir fileName args =
        let fileName, args =
            if EnvironmentHelper.isUnix
            then fileName, args else "cmd", ("/C " + args)
        ExecProcessAndReturnMessages (fun info ->
            info.FileName <- fileName
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
        |> fun p -> p.Messages |> String.concat "\n"

    let downloadArtifact path (url: string) =
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

    let visitFile (visitor: string->string) (fileName : string) = 
        File.ReadAllLines(fileName)
        |> Array.map (visitor)
        |> fun lines -> File.WriteAllLines(fileName, lines)

        // This code is supposed to prevent OutOfMemory exceptions but it outputs wrong BOM
        // use reader = new StreamReader(fileName, encoding)
        // let tempFileName = Path.GetTempFileName()
        // use writer = new StreamWriter(tempFileName, false, encoding)
        // while not reader.EndOfStream do
        //     reader.ReadLine() |> visitor |> writer.WriteLine
        // reader.Close()
        // writer.Close()
        // File.Delete(fileName)
        // File.Move(tempFileName, fileName)
        
    let compileScript symbols outDir fsxPath =
        let dllFile = Path.ChangeExtension(Path.GetFileName fsxPath, ".dll")
        let opts = [
            yield FscHelper.Out (Path.Combine(outDir, dllFile))
            yield FscHelper.Target FscHelper.TargetType.Library
            yield! symbols |> List.map FscHelper.Define
        ]
        FscHelper.compile opts [fsxPath]
        |> function 0 -> () | _ -> failwithf "Cannot compile %s" fsxPath

    let normalizeVersion (version: string) =
        let i = version.IndexOf("-")
        if i > 0 then version.Substring(0, i) else version

    let assemblyInfo projectDir version extra =
        let version = normalizeVersion version
        let asmInfoPath = projectDir </> "AssemblyInfo.fs"
        (Attribute.Version version)::extra
        |> CreateFSharpAssemblyInfo asmInfoPath

module Npm =
    let script workingDir script args =
        sprintf "run %s -- %s" script (String.concat " " args)
        |> Util.run workingDir "npm"

    let install workingDir modules =
        sprintf "install %s" (String.concat " " modules)
        |> Util.run workingDir "npm"

    let command workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.run workingDir "npm"

    let commandAndReturn workingDir command args =
        sprintf "%s %s" command (String.concat " " args)
        |> Util.runAndReturn workingDir "npm"

    let getLatestVersion package tag =
        let package =
            match tag with
            | Some tag -> package + "@" + tag
            | None -> package
        commandAndReturn "." "show" [package; "version"]

    let updatePackageKeyValue f pkgDir keys =
        let pkgJson = Path.Combine(pkgDir, "package.json")
        let reg =
            String.concat "|" keys
            |> sprintf "\"(%s)\"\\s*:\\s*\"(.*?)\""
            |> Regex
        File.ReadAllLines pkgJson
        |> Seq.map (fun line ->
            let m = reg.Match(line)
            if m.Success then
                match f(m.Groups.[1].Value, m.Groups.[2].Value) with
                | Some(k,v) -> reg.Replace(line, sprintf "\"%s\": \"%s\"" k v)
                | None -> line
            else line)
        |> fun lines -> File.WriteAllLines(pkgJson, lines)

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

// Targets
Target "Clean" (fun _ ->
    // Don't delete node_modules for faster builds
    !! "build/fable/bin" ++ "src/fable/*/obj/"
    |> CleanDirs

    !! "build/fable/**/*.*" -- "build/fable/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
    !! "build/tests/**/*.*" -- "build/tests/node_modules/**/*.*"
    |> Seq.iter FileUtils.rm
)

Target "FableCompilerRelease" (fun _ ->
    Util.assemblyInfo "src/fable/Fable.Core" fableCoreVersion []
    Util.assemblyInfo "src/fable/Fable.Compiler" fableCompilerVersion []
    Util.assemblyInfo "src/fable/Fable.Client.Node" fableCompilerVersion [
        Attribute.Metadata ("fableCoreVersion", Util.normalizeVersion fableCoreVersion)
    ]

    let buildDir = "build/fable"

    [ "src/fable/Fable.Core/Fable.Core.fsproj"
      "src/fable/Fable.Compiler/Fable.Compiler.fsproj"
      "src/fable/Fable.Client.Node/Fable.Client.Node.fsproj" ]
    |> MSBuildRelease (buildDir + "/bin") "Build"
    |> Log "Fable-Compiler-Release-Output: "
    
    // For some reason, ProjectCracker targets are not working after updating the package
    !! "packages/FSharp.Compiler.Service.ProjectCracker/utilities/net45/FSharp.Compiler.Service.ProjectCrackerTool.exe*"
    |> Seq.iter (fun x -> FileUtils.cp x "build/fable/bin")

    FileUtils.cp_r "src/fable/Fable.Client.Node/js" buildDir
    FileUtils.cp "README.md" buildDir
    Npm.command buildDir "version" [fableCompilerVersion]
    Npm.install buildDir []
)

Target "FableCompilerDebug" (fun _ ->
    let buildDir = "build/fable"

    [ "src/fable/Fable.Core/Fable.Core.fsproj"
      "src/fable/Fable.Compiler/Fable.Compiler.fsproj"
      "src/fable/Fable.Client.Node/Fable.Client.Node.fsproj" ]
    |> MSBuildDebug (buildDir + "/bin") "Build"
    |> Log "Fable-Compiler-Debug-Output: "

    FileUtils.cp_r "src/fable/Fable.Client.Node/js" buildDir
    Npm.command buildDir "version" [fableCompilerVersion]
)

Target "FableCompilerNetcore" (fun _ ->
    try
        // Copy JS files
        let srcDir, buildDir = "src/fable/Fable.Client.Node", "build/fable"
        FileUtils.cp_r (srcDir + "/js") buildDir
        Npm.command buildDir "version" [fableCompilerNetcoreVersion]

        // Edit package.json for NetCore
        (buildDir, ["name"; "fable"])
        ||> Npm.updatePackageKeyValue (function
            | "name", _ -> Some("name", "fable-compiler-netcore")
            | "fable", v -> Some("fable-netcore", v)
            | _ -> None)

        // Restore packages
        [ "src/fable/Fable.Core"; "src/fable/Fable.Compiler"; srcDir ]
        |> Seq.iter (fun dir -> Util.run dir "dotnet" "restore")

        // Publish Fable NetCore
        Util.run srcDir "dotnet" "publish -f netcoreapp1.0 -c Release"
        FileUtils.cp_r (srcDir + "/bin/Release/netcoreapp1.0/publish") (buildDir + "/bin")        

        // Put FSharp.Core.optdata/sigdata next to FSharp.Core.dll
        FileUtils.cp (buildDir + "/bin/runtimes/any/native/FSharp.Core.optdata") (buildDir + "/bin")
        FileUtils.cp (buildDir + "/bin/runtimes/any/native/FSharp.Core.sigdata") (buildDir + "/bin")

        // Compile tests
        Node.run "." buildDir ["src/tests/tests.fsx --symbols DOTNETCORE"]

        // Copy the development version of fable-core.js
        let fableCoreNpmDir = "src/fable/Fable.Core/npm"
        Npm.install fableCoreNpmDir []
        Npm.script fableCoreNpmDir "tsc" ["fable-core.ts --target ES2015 --declaration"]
        setEnvironVar "BABEL_ENV" "target-umd"
        Npm.script fableCoreNpmDir "babel" ["fable-core.js -o fable-core.js --compact=false"]
        FileUtils.cp "src/fable/Fable.Core/npm/fable-core.js" "build/tests/node_modules/fable-core/"

        // Run tests
        let testsBuildDir = "build/tests"
        FileUtils.cp "src/tests/package.json" testsBuildDir
        Npm.install testsBuildDir []
        Npm.script testsBuildDir "test" []
    with
    | ex ->
        printfn "Target FableCompilerNetcore didn't work, make sure of the following:"
        printfn "- You have NetCore SDK installed"
        printfn "- You cloned FSharp.Compiler.Service on same level as Fable"
        printfn "- FSharp.Compiler.Service > build All.NetCore run successfully"
        raise ex
)

Target "CompileFableImportTests" (fun _ ->
    let buildDir = "build/imports/bin"
    CleanDir buildDir

    [ "import/Fable.Import.Test.fsproj" ]
    |> MSBuildDebug buildDir "Build"
    |> Log "Fable-Import-Test-Output: "
)

// Target "FableSuave" (fun _ ->
//     let buildDir = "build/suave"
//     !! "src/fable-client-suave/Fable.Client.Suave.fsproj"
//     |> MSBuildDebug buildDir "Build"
//     |> Log "Debug-Output: "
//     // Copy Fable.Core.dll to buildDir so it can be referenced by F# code
//     FileUtils.cp "import/core/Fable.Core.dll" buildDir
// )

Target "NUnitTest" (fun _ ->
    let testsBuildDir = "build/tests"
    
    !! "src/tests/Fable.Tests.fsproj"
    |> MSBuildRelease testsBuildDir "Build"
    |> ignore
    
    [Path.Combine(testsBuildDir, "Fable.Tests.dll")]
    |> Testing.NUnit3.NUnit3 id
)

Target "MochaTest" (fun _ ->
    let testsBuildDir = "build/tests"
    MSBuildDebug "src/tests/DllRef/bin" "Build" ["src/tests/DllRef/Fable.Tests.DllRef.fsproj"] |> ignore
    Node.run "." "build/fable" ["src/tests/DllRef"]
    Node.run "." "build/fable" ["src/tests/Other"]
    Node.run "." "build/fable" ["src/tests/"]
    FileUtils.cp "src/tests/package.json" testsBuildDir
    Npm.install testsBuildDir []
    // Copy the development version of fable-core.js
    FileUtils.cp "src/fable/Fable.Core/npm/fable-core.js" "build/tests/node_modules/fable-core/"
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
    let fableCoreNpmDir = "src/fable/Fable.Core/npm"
    let applyTag = function
        | Some tag -> ["--tag"; tag]
        | None -> []

    // Check if fable-compiler and fable-core version are prerelease or not
    let fableCompilerTag, fableCoreTag =
        (if fableCompilerVersion.IndexOf("-") > 0 then Some "next" else None),
        (if fableCoreVersion.IndexOf("-") > 0 then Some "next" else None)

    if Npm.getLatestVersion "fable-core" fableCoreTag <> fableCoreVersion then
        applyTag fableCoreTag |> Npm.command fableCoreNpmDir "publish" 

    let workingDir = "temp/build"
    let url = "https://ci.appveyor.com/api/projects/alfonsogarciacaro/fable/artifacts/build/fable.zip"
    Util.downloadArtifact workingDir url
    // Npm.command workingDir "version" [fableCompilerVersion]
    applyTag fableCompilerTag |> Npm.command workingDir "publish"
)

Target "FableCore" (fun _ ->
    let fableCoreNpmDir = "src/fable/Fable.Core/npm"

    // Update fable-core npm version
    (fableCoreNpmDir, ["version"])
    ||> Npm.updatePackageKeyValue (fun (_,v) ->
        if v <> fableCoreVersion
        then Some("version", fableCoreVersion)
        else None)

    // Update Fable.Core version
    Util.assemblyInfo "src/fable/Fable.Core/" fableCoreVersion []
    !! "src/fable/Fable.Core/Fable.Core.fsproj"
    |> MSBuild fableCoreNpmDir "Build" [
        "Configuration","Release"
        "DefineConstants","IMPORT"
        "DocumentationFile","npm/Fable.Core.xml"]
    |> ignore // Log outputs all files in node_modules

    Npm.install fableCoreNpmDir []
    // Compile TypeScript
    Npm.script fableCoreNpmDir "tsc" ["fable-core.ts --target ES2015 --declaration"]
    // Compile Es2015 syntax to ES5 with different module targets 
    setEnvironVar "BABEL_ENV" "target-commonjs"
    Npm.script fableCoreNpmDir "babel" ["fable-core.js -o commonjs.js --compact=false"] 
    setEnvironVar "BABEL_ENV" "target-es2015"
    Npm.script fableCoreNpmDir "babel" ["fable-core.js -o es2015.js --compact=false"] 
    setEnvironVar "BABEL_ENV" "target-umd"
    Npm.script fableCoreNpmDir "babel" ["fable-core.js -o fable-core.js --compact=false"]
    // Minimize 
    Npm.script fableCoreNpmDir "uglifyjs" ["fable-core.js -c -m -o fable-core.min.js"] 
)

Target "UpdateSampleRequirements" (fun _ ->
    !! "samples/**/package.json"
    |> Seq.iter (Util.visitFile (fun line ->
        match Regex.Match(line, "^(\s*)\"(fable(?:-core)?)\": \".*?\"(,)?") with
        | m when m.Success ->
            match m.Groups.[2].Value with
            | "fable" -> sprintf "%s\"fable\": \"^%s\"%s" m.Groups.[1].Value fableCompilerVersion m.Groups.[3].Value
            | "fable-core" -> sprintf "%s\"fable-core\": \"^%s\"%s" m.Groups.[1].Value fableCoreVersion m.Groups.[3].Value
            | _ -> line                 
        | _ -> line))
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
  ==> "FableCore"
  ==> "FableCompilerRelease"
  ==> "CompileFableImportTests"
  ==> "Plugins"
  ==> "MochaTest"
  =?> ("MakeArtifactLighter", environVar "APPVEYOR" = "True")
  ==> "All"

"Clean"
  ==> "FableCompilerNetcore"

// Start build
RunTargetOrDefault "All"
