module Bench.Platform

let testScriptPath =  System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../test_script.fsx")
let fableCoreDir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../../../build/fable-core")

let initFable() = Fable.JS.Main.init()
let readAllBytes metadataPath (fileName:string) = System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText (filePath:string) = System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath:string) (text:string) = System.IO.File.WriteAllText (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

let writeJs (filePath:string) (babelAst:obj) =
    () // Do nothing in .NET for now
