module CallerFilePathTest

open System.Runtime.CompilerServices
open System.Runtime.InteropServices

type Log =
    static member info
        (
            message: string,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string
        ) =
        printfn "%s: %s" path message

Log.info "hello"
