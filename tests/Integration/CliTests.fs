module Fable.Tests.Cli

open Fable.Cli.Entry
open Expecto

type Result<'T1, 'T2> with
    member this.Value = match this with Ok v -> v | Error _ -> failwith "I'm Error!"

let tests =
  testList "Cli" [

    testCase "Can use --outdir in lower case" <| fun () ->
        let res = parseCliArgs ["--outDir"; "foo"]
        Expect.isOk res "--outDir args"
        Expect.equal (res.Value.Value("--outDir")) (Some "foo") "--outDir value"

        let res = parseCliArgs ["--outdir"; "foo"]
        Expect.isOk res "--outdir args"
        Expect.equal (res.Value.Value("--outDir")) (Some "foo") "--outdir value"

    testCase "Cannot use --outir" <| fun () ->
        let res = parseCliArgs ["--outir"; "foo"]
        Expect.isError res "--outir args"
  ]