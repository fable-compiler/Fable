module IncrementalBuild

open System
open System.IO
open System.Security.Cryptography
open System.Text
open Build.Utils

let private cacheDir = Path.Resolve(".build-cache")

/// Stable hash over file contents + arbitrary string keys.
/// File paths are hashed by content; plain strings are hashed as-is.
let private computeInputHash (inputs: string list) =
    use sha = SHA256.Create()
    use ms = new MemoryStream()

    for input in List.sort inputs do
        if File.Exists input then
            use fs = File.OpenRead input
            let contentHash = sha.ComputeHash fs
            ms.Write(contentHash, 0, contentHash.Length)
        else
            // Treat as a plain string key (flag, version, env var...)
            let bytes = Encoding.UTF8.GetBytes input
            let keyHash = sha.ComputeHash bytes
            ms.Write(keyHash, 0, keyHash.Length)

    ms.ToArray() |> sha.ComputeHash |> Convert.ToHexString

let private cachePath (name: string) =
    let safe = name.Replace('/', '_').Replace('\\', '_').Replace(' ', '_')
    Path.Combine(cacheDir, $"{safe}.cache")

let private tryLoadCache (name: string) : string option =
    try
        let path = cachePath name

        if File.Exists path then
            File.ReadAllText path |> Some
        else
            None
    with _ ->
        None

let private saveCache (name: string) (entry: string) =
    Directory.CreateDirectory cacheDir |> ignore
    File.WriteAllText(cachePath name, entry)

let run (name: string) (forceBuild: bool) (inputs: string list) (outputs: string list) (callback: unit -> unit) =
    let inputHash = computeInputHash inputs

    let outputsExist =
        outputs
        |> List.forall (fun output -> File.Exists output || Directory.Exists output)

    let isStale =
        if forceBuild then
            true
        else if not outputsExist then
            true
        else
            match tryLoadCache name with
            | None -> true
            | Some cached -> cached <> inputHash

    if isStale then
        callback ()

        saveCache name inputHash
    else
        printfn "Skipping '%s' (inputs unchanged and outputs exist)" name
