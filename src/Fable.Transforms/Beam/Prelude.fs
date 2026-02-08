namespace Fable.Beam

module Naming =
    open System.Text.RegularExpressions

    let toSnakeCase (name: string) =
        let sb = System.Text.StringBuilder()

        for i = 0 to name.Length - 1 do
            let c = name.[i]

            if System.Char.IsUpper(c) then
                if i > 0 then
                    sb.Append('_') |> ignore

                sb.Append(System.Char.ToLowerInvariant(c)) |> ignore
            else
                sb.Append(c) |> ignore

        sb.ToString()

    let sanitizeErlangName (name: string) =
        // Decode $XXXX hex sequences from F# compiled names (e.g. $0020 -> space -> _)
        Regex.Replace(
            name,
            @"\$([0-9A-Fa-f]{4})",
            fun m ->
                let c = char (System.Convert.ToInt32(m.Groups.[1].Value, 16))

                if System.Char.IsLetterOrDigit(c) then
                    c.ToString()
                else
                    "_"
        )
        |> fun s -> s.Replace("'", "").Replace("$", "_").Replace("@", "")
        |> toSnakeCase
        |> fun s -> Regex.Replace(s, "_+", "_")
        |> fun s -> s.Trim('_')

    let moduleNameFromFile (filePath: string) =
        System.IO.Path.GetFileNameWithoutExtension(filePath)
        |> toSnakeCase
        |> fun s -> s.Replace(".", "_").Replace("-", "_")

    let capitalizeFirst (s: string) =
        if s.Length = 0 then
            s
        else
            s.[0..0].ToUpperInvariant() + s.[1..]

    /// Sanitize a string for use as an Erlang variable name (must start with uppercase or _)
    let sanitizeErlangVar (name: string) =
        // Remove/replace characters invalid in Erlang variable names
        name.Replace("$", "_").Replace("@", "_")
        |> fun s -> Regex.Replace(s, "[^a-zA-Z0-9_]", "_")
