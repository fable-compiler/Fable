module Fable.Metadata

open System

let coreAssemblies =
    [|
        "Fable.Core"
        "FSharp.Core"
        "mscorlib"
        "netstandard"
        "System.Collections"
        "System.Collections.Concurrent"
        "System.ComponentModel"
        "System.ComponentModel.Primitives"
        "System.ComponentModel.TypeConverter"
        "System.Console"
        "System.Core"
        "System.Diagnostics.Debug"
        "System.Diagnostics.Tools"
        "System.Diagnostics.Tracing"
        "System.Globalization"
        "System"
        "System.IO"
        "System.Net.Requests"
        "System.Net.WebClient"
        "System.Numerics"
        "System.Reflection"
        "System.Reflection.Extensions"
        "System.Reflection.Metadata"
        "System.Reflection.Primitives"
        "System.Reflection.TypeExtensions"
        "System.Runtime"
        "System.Runtime.Extensions"
        "System.Runtime.Numerics"
        "System.Runtime.InteropServices"
        "System.Text.Encoding"
        "System.Text.Encoding.Extensions"
        "System.Text.RegularExpressions"
        "System.Threading"
        "System.Threading.Tasks"
        "System.Threading.Thread"
        "System.ValueTuple"
    |]

let isSystemPackage (pkgName: string) =
    pkgName.StartsWith("System.", StringComparison.Ordinal)
    || pkgName.StartsWith("Microsoft.", StringComparison.Ordinal)
    || pkgName.StartsWith("runtime.", StringComparison.Ordinal)
    || pkgName = "NETStandard.Library"
    || pkgName = "FSharp.Core"
    || pkgName = "Fable.Core"
    || pkgName = "testhost"
