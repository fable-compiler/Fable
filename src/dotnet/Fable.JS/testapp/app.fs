module App

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.JS

let use_net45_meta = false

let metadataPath =
    if use_net45_meta then
        "/temp/metadata/"  // dotnet 4.5 binaries
    else 
        "/temp/metadata2/" // dotnet core 2.0 binaries

let references =
    if use_net45_meta then
      [|"Fable.Core";"FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics"|]
    else
      [|"Fable.Core"
        "FSharp.Core"
        "Microsoft.CSharp"
        "Microsoft.VisualBasic"
        "Microsoft.Win32.Primitives"
        "mscorlib"
        "netstandard"
        "System.AppContext"
        "System.Buffers"
        "System.Collections.Concurrent"
        "System.Collections"
        "System.Collections.Immutable"
        "System.Collections.NonGeneric"
        "System.Collections.Specialized"
        "System.ComponentModel.Annotations"
        "System.ComponentModel.Composition"
        "System.ComponentModel.DataAnnotations"
        "System.ComponentModel"
        "System.ComponentModel.EventBasedAsync"
        "System.ComponentModel.Primitives"
        "System.ComponentModel.TypeConverter"
        "System.Configuration"
        "System.Console"
        "System.Core"
        "System.Data.Common"
        "System.Data"
        "System.Diagnostics.Contracts"
        "System.Diagnostics.Debug"
        "System.Diagnostics.DiagnosticSource"
        "System.Diagnostics.FileVersionInfo"
        "System.Diagnostics.Process"
        "System.Diagnostics.StackTrace"
        "System.Diagnostics.TextWriterTraceListener"
        "System.Diagnostics.Tools"
        "System.Diagnostics.TraceSource"
        "System.Diagnostics.Tracing"
        "System"
        "System.Drawing"
        "System.Drawing.Primitives"
        "System.Dynamic.Runtime"
        "System.Globalization.Calendars"
        "System.Globalization"
        "System.Globalization.Extensions"
        "System.IO.Compression"
        "System.IO.Compression.FileSystem"
        "System.IO.Compression.ZipFile"
        "System.IO"
        "System.IO.FileSystem"
        "System.IO.FileSystem.DriveInfo"
        "System.IO.FileSystem.Primitives"
        "System.IO.FileSystem.Watcher"
        "System.IO.IsolatedStorage"
        "System.IO.MemoryMappedFiles"
        "System.IO.Pipes"
        "System.IO.UnmanagedMemoryStream"
        "System.Linq"
        "System.Linq.Expressions"
        "System.Linq.Parallel"
        "System.Linq.Queryable"
        "System.Net"
        "System.Net.Http"
        "System.Net.HttpListener"
        "System.Net.Mail"
        "System.Net.NameResolution"
        "System.Net.NetworkInformation"
        "System.Net.Ping"
        "System.Net.Primitives"
        "System.Net.Requests"
        "System.Net.Security"
        "System.Net.ServicePoint"
        "System.Net.Sockets"
        "System.Net.WebClient"
        "System.Net.WebHeaderCollection"
        "System.Net.WebProxy"
        "System.Net.WebSockets.Client"
        "System.Net.WebSockets"
        "System.Numerics"
        "System.Numerics.Vectors"
        "System.ObjectModel"
        "System.Reflection.DispatchProxy"
        "System.Reflection"
        "System.Reflection.Emit"
        "System.Reflection.Emit.ILGeneration"
        "System.Reflection.Emit.Lightweight"
        "System.Reflection.Extensions"
        "System.Reflection.Metadata"
        "System.Reflection.Primitives"
        "System.Reflection.TypeExtensions"
        "System.Resources.Reader"
        "System.Resources.ResourceManager"
        "System.Resources.Writer"
        "System.Runtime.CompilerServices.VisualC"
        "System.Runtime"
        "System.Runtime.Extensions"
        "System.Runtime.Handles"
        "System.Runtime.InteropServices"
        "System.Runtime.InteropServices.RuntimeInformation"
        "System.Runtime.InteropServices.WindowsRuntime"
        "System.Runtime.Loader"
        "System.Runtime.Numerics"
        "System.Runtime.Serialization"
        "System.Runtime.Serialization.Formatters"
        "System.Runtime.Serialization.Json"
        "System.Runtime.Serialization.Primitives"
        "System.Runtime.Serialization.Xml"
        "System.Security.Claims"
        "System.Security.Cryptography.Algorithms"
        "System.Security.Cryptography.Csp"
        "System.Security.Cryptography.Encoding"
        "System.Security.Cryptography.Primitives"
        "System.Security.Cryptography.X509Certificates"
        "System.Security"
        "System.Security.Principal"
        "System.Security.SecureString"
        "System.ServiceModel.Web"
        "System.ServiceProcess"
        "System.Text.Encoding"
        "System.Text.Encoding.Extensions"
        "System.Text.RegularExpressions"
        "System.Threading"
        "System.Threading.Overlapped"
        "System.Threading.Tasks.Dataflow"
        "System.Threading.Tasks"
        "System.Threading.Tasks.Extensions"
        "System.Threading.Tasks.Parallel"
        "System.Threading.Thread"
        "System.Threading.ThreadPool"
        "System.Threading.Timer"
        "System.Transactions"
        "System.Transactions.Local"
        "System.ValueTuple"
        "System.Web"
        "System.Web.HttpUtility"
        "System.Windows"
        "System.Xml"
        "System.Xml.Linq"
        "System.Xml.ReaderWriter"
        "System.Xml.Serialization"
        "System.Xml.XDocument"
        "System.Xml.XmlDocument"
        "System.Xml.XmlSerializer"
        "System.Xml.XPath"
        "System.Xml.XPath.XDocument"
        "WindowsBase"
        |]

#if !DOTNET_FILE_SYSTEM

[<Fable.Core.Import("readFileSync", "fs")>]
let readFileSync: System.Func<string, byte[]> = failwith "JS only"
[<Fable.Core.Import("readFileSync", "fs")>]
let readTextSync: System.Func<string, string, string> = failwith "JS only"
[<Fable.Core.Emit("process.hrtime()")>]
let hrTimeNow(): float[] = failwith "JS only"
[<Fable.Core.Emit("process.hrtime($0)")>]
let hrTimeElapsed(time: float[]): float[] = failwith "JS only"

let readAllBytes = fun (fileName:string) -> readFileSync.Invoke (metadataPath + fileName)
let readAllText = fun (filePath:string) -> readTextSync.Invoke (filePath, "utf8")
let measureTime (f: 'a -> 'b) x =
    let startTime = hrTimeNow()
    let res = f x
    let elapsed = hrTimeElapsed(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = value |> Fable.Core.JsInterop.toJson

#else // DOTNET_FILE_SYSTEM

let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res


let toJson (value: obj) = Newtonsoft.Json.JsonConvert.SerializeObject(value)

#endif


[<EntryPoint>]
let main argv =
    try
        let fileName = "test_script.fsx"
        let source = readAllText fileName
        let createChecker() = references |> createChecker readAllBytes
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        let com = Fable.State.Compiler ()
        let parseFSharp () = parseFSharpProject checker com fileName source
        let parseFable ast = compileAst com ast fileName
        let bench i =
            let ms1, fsAST = measureTime parseFSharp ()
            let ms2, babelAST = measureTime parseFable fsAST
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
            //printfn "Babel AST: %s" (toJson babelAST)
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0
