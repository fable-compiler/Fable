namespace Fable.Compiler

open System.IO

// TODO: Check the path is actually normalized?
type File(normalizedFullPath: string) =
    let mutable sourceHash = None

    let readAllTextNonBlocking (path: string) =
        if File.Exists(path) then
            use fileStream =
                new FileStream(
                    path,
                    FileMode.Open,
                    FileAccess.Read,
                    FileShare.ReadWrite
                )

            use textReader = new StreamReader(fileStream)
            textReader.ReadToEnd()
        else
            // Log.always("File does not exist: " + path)
            ""

    member _.NormalizedFullPath = normalizedFullPath

    member _.ReadSource() =
        match sourceHash with
        | Some h -> h, lazy readAllTextNonBlocking normalizedFullPath
        | _ ->
            let source = readAllTextNonBlocking normalizedFullPath
            let h = hash source
            sourceHash <- Some h
            h, lazy source

    static member MakeSourceReader(files: File array) =
        let fileDic =
            files |> Seq.map (fun f -> f.NormalizedFullPath, f) |> dict

        let sourceReader f = fileDic[f].ReadSource()
        files |> Array.map (fun file -> file.NormalizedFullPath), sourceReader
