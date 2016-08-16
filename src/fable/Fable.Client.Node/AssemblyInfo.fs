namespace System
open System.Reflection

[<assembly: AssemblyVersionAttribute("0.5.5")>]
[<assembly: AssemblyMetadataAttribute("fableCoreVersion","0.5.4")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.5.5"
    let [<Literal>] InformationalVersion = "0.5.5"
