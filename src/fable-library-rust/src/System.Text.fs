namespace System.Text

open Global_

type StringBuilder(value: string, capacity: int) =
    let buf = ResizeArray<string>(capacity)

    do
        if not (System.String.IsNullOrEmpty value) then
            buf.Add(value)

    new(capacity: int) = StringBuilder("", capacity)
    new(value: string) = StringBuilder(value, 16)
    new() = StringBuilder("", 16)

    member x.Append(s: string) =
        buf.Add(s)
        x

    member x.Append(o: bool) = x.Append(string o)
    member x.Append(c: char) = x.Append(string c)
    member x.Append(o: int8) = x.Append(string o)
    member x.Append(o: byte) = x.Append(string o)
    member x.Append(o: int16) = x.Append(string o)
    member x.Append(o: uint16) = x.Append(string o)
    member x.Append(o: int32) = x.Append(string o)
    member x.Append(o: uint32) = x.Append(string o)
    member x.Append(o: int64) = x.Append(string o)
    member x.Append(o: uint64) = x.Append(string o)
    member x.Append(o: float32) = x.Append(string o)
    member x.Append(o: float) = x.Append(string o)

    member x.Append(s: string, index: int, count: int) =
        x.Append(s.Substring(index, count))

    member x.Append(cs: char[]) = x.Append(System.String(cs))
    member x.Append(sb: StringBuilder) = x.Append(sb.ToString())
    // member x.Append(o: obj) = x.Append(string o)
    // member x.AppendFormat(fmt: string, o: obj) = x.Append(System.String.Format(fmt, o))
    // member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o: obj) = x.Append(System.String.Format(provider, fmt, o))
    member x.AppendLine() = x.Append(System.Environment.NewLine)
    member x.AppendLine(s: string) = x.Append(s).AppendLine()

    member x.Clear() =
        buf.Clear()
        x

    member x.Length = buf |> asArray |> Array.sumBy (fun s -> s.Length)
    override _.ToString() = System.String.Concat(buf |> asArray)

    member x.ToString(index: int, count: int) =
        x.ToString().Substring(index, count)
