namespace Fable.Cli

open System.IO

module Literals =

    let [<Literal>] VERSION = "3.0.0-nagareyama-alpha-003"
    let [<Literal>] CORE_VERSION = "2.1.0"

type CliArgs =
    { ProjectFile: string
      RootDir: string
      FableLibraryPath: string option
      Define: string[]
      ForcePackages: bool
      Exclude: string option
      CompilerOptions: Fable.CompilerOptions }

type private TypeInThisAssembly = class end

[<RequireQualifiedAccess>]
module Log =
    open System

    let mutable private verbosity = Fable.Verbosity.Normal

    /// To be called only at the beginning of the app
    let makeVerbose() =
        verbosity <- Fable.Verbosity.Verbose

    let writerLock = obj()

    let always (msg: string) =
        if verbosity <> Fable.Verbosity.Silent
            && not(String.IsNullOrEmpty(msg)) then
//            lock writerLock <| fun () ->
                Console.Out.WriteLine(msg)
                Console.Out.Flush()

    let verbose (msg: Lazy<string>) =
        if verbosity = Fable.Verbosity.Verbose then
            always msg.Value

[<RequireQualifiedAccess>]
module Process =
    open System.Diagnostics

    type Options(?envVars, ?redirectOutput) =
        member val EnvVars = defaultArg envVars Map.empty<string,string>
        member val RedirectOuput = defaultArg redirectOutput false

    let isWindows =
        #if NETFX
        true
        #else
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform
            (System.Runtime.InteropServices.OSPlatform.Windows)
        #endif

    let start workingDir fileName args (opts: Options) =
        let fileName, args =
            if isWindows
            then "cmd", ("/C \"" + fileName + "\" " + args)
            else fileName, args
        let p = new Process()
        p.StartInfo.FileName <- fileName
        p.StartInfo.Arguments <- args
        p.StartInfo.WorkingDirectory <- workingDir
        p.StartInfo.RedirectStandardOutput <- opts.RedirectOuput
        #if NETFX
        p.StartInfo.UseShellExecute <- false
        #endif
        opts.EnvVars |> Map.iter (fun k v ->
            p.StartInfo.Environment.[k] <- v)
        p.Start() |> ignore
        p

    // Adapted from https://github.com/enricosada/dotnet-proj-info/blob/1e6d0521f7f333df7eff3148465f7df6191e0201/src/dotnet-proj/Program.fs#L155
    let private runProcess (workingDir: string) (exePath: string) (args: string) =
        let logOut = System.Collections.Concurrent.ConcurrentQueue<string>()
        let logErr = System.Collections.Concurrent.ConcurrentQueue<string>()

        let psi = ProcessStartInfo()
        psi.FileName <- exePath
        psi.WorkingDirectory <- workingDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.Arguments <- args
        psi.CreateNoWindow <- true
        psi.UseShellExecute <- false

        use p = new Process()
        p.StartInfo <- psi

        p.OutputDataReceived.Add(fun ea -> logOut.Enqueue (ea.Data))
        p.ErrorDataReceived.Add(fun ea -> logErr.Enqueue (ea.Data))

        let exitCode =
            try
                p.Start() |> ignore
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()
                p.ExitCode
            with ex ->
                logErr.Enqueue ("Cannot run: " + ex.Message)
                -1

        exitCode, logOut.ToArray(), logErr.ToArray()

    // Adapted from https://github.com/enricosada/dotnet-proj-info/blob/1e6d0521f7f333df7eff3148465f7df6191e0201/src/dotnet-proj/Program.fs#L155
    let runCmd log workingDir exePath args =
        log (workingDir + "> " + exePath + " " + (args |> String.concat " "))

        let exitCode, logOut, logErr =
            String.concat " " args
            |> runProcess workingDir exePath

        Array.append logOut logErr
        |> String.concat "\n"
        |> log

        exitCode

[<RequireQualifiedAccess>]
module Async =
    let fold f (state: 'State) (xs: 'T seq) = async {
        let mutable state = state
        for x in xs do
            let! result = f state x
            state <- result
        return state
    }

    let map f x = async {
        let! x = x
        return f x
    }

    let tryPick (f: 'T->Async<'Result option>) xs: Async<'Result option> = async {
        let mutable result: 'Result option = None
        for x in xs do
            match result with
            | Some _ -> ()
            | None ->
                let! r = f x
                result <- r
        return result
    }

    let orElse (f: unit->Async<'T>) (x: Async<'T option>): Async<'T> = async {
        let! x = x
        match x with
        | Some x -> return x
        | None -> return! f ()
    }

module File =
    /// File.ReadAllText fails with locked files. See https://stackoverflow.com/a/1389172
    let readAllTextNonBlocking (path: string) =
        use fileStream = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
        use textReader = new StreamReader(fileStream)
        textReader.ReadToEnd()

    let getRelativePath path =
        Fable.Path.getRelativePath (Directory.GetCurrentDirectory()) path
