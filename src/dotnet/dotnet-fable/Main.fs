module Main

open System
open System.IO
open System.Diagnostics

let rec findPaketDependenciesDir dir =
    if File.Exists(Path.Combine(dir, "paket.dependencies")) then
        dir
    else
        match Directory.GetParent(dir) with
        | null ->
            failwithf "%s %s"
                "Couldn't find `paket.dependencies` file."
                "Please make sure project dependencies are managed by Paket."
        | parent -> findPaketDependenciesDir parent.FullName

let runDotnet fileName (args: string[]) =
    let p = new Process()
    p.StartInfo.FileName <- "dotnet"
    p.StartInfo.Arguments <- fileName + " " + (String.concat " " args)
    p.StartInfo.WorkingDirectory <- Directory.GetCurrentDirectory()
    p.Start() |> ignore
    p.WaitForExit()
    p.ExitCode

[<EntryPoint>]
let main argv =
    try
        let paketDir =
            Directory.GetCurrentDirectory()
            |> findPaketDependenciesDir
        let fableCompilerPath = Path.Combine(paketDir, "packages", "Fable.Compiler", "fable", "Fable.Compiler.dll")
        if not(File.Exists(fableCompilerPath)) then
            failwithf "%s %s"
                "Cannot find Fable.Compiler package."
                "Please make sure it's been added to `paket.dependencies`."
        runDotnet fableCompilerPath argv
    with
    | ex ->
        printfn "ERROR: %s" ex.Message
        1
