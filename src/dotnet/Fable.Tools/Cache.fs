[<RequireQualifiedAccess>]
module Fable.Tools.Cache

open System
open System.IO
open System.Text

let private cacheDir =
    lazy
        try
            let tmpDir = Path.Combine(Path.GetTempPath(), "fable")
            if Directory.Exists(tmpDir) = false then
                Directory.CreateDirectory(tmpDir) |> ignore
            Some tmpDir
        with ex ->
            Log.logVerbose("Error when creating temp directory: " + ex.Message)
            None

let private computeHash(input: string) =
    // Use input string to calculate MD5 hash
    use md5 = System.Security.Cryptography.MD5.Create()
    let inputBytes = System.Text.Encoding.UTF8.GetBytes(input)
    let hashBytes = md5.ComputeHash(inputBytes)
    // Convert the byte array to hexadecimal string
    let sb = new StringBuilder()
    for i = 0 to (hashBytes.Length - 1) do
        sb.Append(hashBytes.[i].ToString("X2")) |> ignore
    sb.ToString()

let private tryCacheDir (f: string->'T option) =
    if Flags.cacheFiles then
        cacheDir.Value |> Option.bind (fun cacheDir ->
            try
                f cacheDir
            with ex ->
                Log.logVerbose("Error when accessing cache: " + ex.Message)
                None)
    else None

let isCached(filepath: string, minTimestamp: DateTime) =
    tryCacheDir (fun cacheDir ->
        let hash = computeHash filepath
        let cachedFile = Path.Combine(cacheDir, hash)
        if File.Exists(cachedFile) then
            let cacheTimestamp = File.GetLastWriteTime(cachedFile)
            if cacheTimestamp > minTimestamp then
                Some cachedFile
            else
                // Log.logVerbose(sprintf "Cache is outdated (%O)" cacheTimestamp)
                None
        else
            // Log.logVerbose("Not cached: " + filepath)
            None
    ) |> function Some _ -> true | None -> false

let tryGetCachePath(filepath: string) =
    tryCacheDir (fun cacheDir ->
        let hash = computeHash filepath
        Path.Combine(cacheDir, hash) |> Some
    )

let tryCache(filepath: string, content: string) =
    tryCacheDir (fun cacheDir ->
        let hash = computeHash filepath
        let cachedFile = Path.Combine(cacheDir, hash)
        File.WriteAllText(cachedFile, content)
        Some cachedFile
    )
