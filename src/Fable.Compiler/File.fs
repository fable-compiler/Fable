namespace Fable.Compiler

open System.IO
open System.Threading

// TODO: Check the path is actually normalized?
type File(normalizedFullPath: string) =
    let mutable sourceHash = None

    let readAllTextNonBlocking (path: string) =
        if File.Exists(path) then
            use fileStream =
                new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)

            use textReader = new StreamReader(fileStream)
            textReader.ReadToEnd()
        else
            // Log.always("File does not exist: " + path)
            ""

    // Editors can hold a source file exclusively for a moment while saving it.
    let rec readAllTextWithRetry (retriesLeft: int) (path: string) =
        let text =
            try
                Some(readAllTextNonBlocking path)
            with :? IOException when retriesLeft > 0 ->
                None

        match text with
        | Some text -> text
        | None ->
            Thread.Sleep 100
            readAllTextWithRetry (retriesLeft - 1) path

    let readSource (path: string) = readAllTextWithRetry 20 path

    member _.NormalizedFullPath = normalizedFullPath

    member _.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy readSource normalizedFullPath
        | _ ->
            let source = readSource normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

    static member MakeSourceReader(files: File array) =
        let fileDic = files |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict

        let sourceReader f = fileDic[f].ReadSource()
        files |> Array.map (fun file -> file.NormalizedFullPath), sourceReader
