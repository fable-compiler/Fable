module Helpers
open System.IO
open Fake
open System

/// Recursively process all files in the directory tree
let rec processDirectory force indir outdir (exts:seq<_>) func = 
    let exts = exts |> Seq.map (fun (s:string) -> s.ToLower()) |> set
    ensureDirectory outdir
    for d in Directory.EnumerateDirectories(indir) do
        let name = Path.GetFileName(d)
        processDirectory force (Path.Combine(indir, name)) (Path.Combine(outdir, name)) exts func
    for file in Directory.GetFiles(indir, "*.*") do
        let dir = Path.GetDirectoryName(file)
        let name = Path.GetFileNameWithoutExtension(file)
        let output = Path.Combine(outdir, sprintf "%s.html" name)

        // Update only when needed
        let changeTime = File.GetLastWriteTime(file)
        let generateTime = File.GetLastWriteTime(output)
        let ext = Path.GetExtension file
        if force || changeTime > generateTime then
            if exts.Contains(ext) then 
                func file outdir 



/// Generates a temp file and deletes it when disposed (to be used via `use`)
type TempFile() = 
    let name = System.IO.Path.GetTempFileName()
    member x.Name = name
    static member New() = new TempFile()
    interface System.IDisposable with
        member x.Dispose() = 
            File.Delete(name)


/// Utils for running NPM and Node
module Util =
    let run workingDir fileName args =
        let ok = 
            execProcess (fun info ->
                info.FileName <- fileName
                info.WorkingDirectory <- workingDir
                info.Arguments <- args) TimeSpan.MaxValue
        if not ok then failwith (sprintf "'%s> %s %s' task failed" workingDir fileName args)

module Node =
    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "node" args
        
module Npm =
    let npmFilePath args =
        if EnvironmentHelper.isUnix
        then "npm", args
        else "cmd", ("/C npm " + args)

    let run workingDir script args =
        let args = sprintf "%s %s" script (String.concat " " args)
        Util.run workingDir "npm" args

    let install workingDir modules =
        run workingDir "install" modules
