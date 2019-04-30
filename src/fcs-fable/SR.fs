//------------------------------------------------------------------------
// From SR.fs
//------------------------------------------------------------------------

namespace FSharp.Compiler

module SR =
    let GetString(name: string) =
        match SR.Resources.resources.TryGetValue(name) with
        | true, value -> value
        | _ -> "Missing FSStrings error message for: " + name

module DiagnosticMessage =
    type ResourceString<'T>(sfmt: string, fmt: string) =
        member x.Format =
            let a = fmt.Split('%')
                    |> Array.filter (fun s -> String.length s > 0)
                    |> Array.map (fun s -> box("%" + s))
            let tmp = System.String.Format(sfmt, a)
            let fmt = Printf.StringFormat<'T>(tmp)
            sprintf fmt

    let postProcessString (s: string) =
        s.Replace("\\n","\n").Replace("\\t","\t")

    let DeclareResourceString (messageID: string, fmt: string) =
        let messageString = SR.GetString(messageID) |> postProcessString
        ResourceString<'T>(messageString, fmt)
