module Fable.Tests.Compiler.LockedSourceFile

open System
open System.IO
open System.Threading
open Expecto
open Util.Testing

let private source = "module Library\n"

let private withSourceFile (test: string -> unit) =
    let dir = Directory.CreateTempSubdirectory("fable-locked-source-").FullName

    try
        let path = Path.Combine(dir, "Library.fs")
        File.WriteAllText(path, source)
        test path
    finally
        Directory.Delete(dir, true)

let private lockFile (path: string) =
    new FileStream(path, FileMode.Open, FileAccess.ReadWrite, FileShare.None)

let private releaseAfter (delayMs: int) (fileLock: FileStream) =
    let thread =
        Thread(fun () ->
            Thread.Sleep delayMs
            fileLock.Dispose()
        )

    thread.Start()
    thread

let tests =
  testList "Locked source file" [
    testCase "ReadSource waits for a short lock to be released" <| fun _ ->
      withSourceFile <| fun path ->
        let release = lockFile path |> releaseAfter 100
        let _, text = Fable.Compiler.File(path).ReadSource()
        release.Join()
        text.Value |> equal source

    testCase "ReadSource raises when the lock outlasts the retries" <| fun _ ->
      withSourceFile <| fun path ->
        use _fileLock = lockFile path

        let ex =
            Expect.throwsC
                (fun () -> Fable.Compiler.File(path).ReadSource() |> ignore)
                id

        Expect.isTrue (ex :? IOException) $"Expected an IOException, got {ex}"
        Expect.stringContains ex.Message path "The error names the locked file"
  ]
