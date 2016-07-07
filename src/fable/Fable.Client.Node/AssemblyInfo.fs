namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.4.0")>]
[<assembly: AssemblyMetadataAttribute("minimumFableCoreVersion","0.2.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.0"
    let [<Literal>] InformationalVersion = "0.4.0"
