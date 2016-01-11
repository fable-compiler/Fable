#r "packages/FAKE/tools/FakeLib.dll"

open Fake

// Directories
let buildDir  = "./build/"
let testDir   = "./test/"

// Filesets
let appReferences  = !! "src/**/*.fsproj"
let binDirs = !! "src/**/bin" ++ "src/**/obj"

// version info
let version = "0.2"  // or retrieve from CI server

// Targets
Target "Clean" (fun _ ->
    binDirs
    |> Seq.append [buildDir; testDir]
    |> CleanDirs
)

Target "Release" (fun _ ->
    MSBuildRelease buildDir "Build" appReferences
        |> Log "Release-Output: "
)

Target "Debug" (fun _ ->
    MSBuildDebug buildDir "Build" appReferences
        |> Log "Debug-Output: "
)

// Build order
"Clean"
  ==> "Release"

// start build
RunTargetOrDefault "Debug"