namespace System.Text

open System

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

    member x.Append(s: string, startIndex: int, count: int) =
        buf.Add(s.Substring(startIndex, count))
        x

    member x.Append(c: char) =
        buf.Add(string<char> c)
        x

    member x.Append(o: int) =
        buf.Add(string<int> o)
        x

    member x.Append(o: float) =
        buf.Add(string<float> o)
        x

    member x.Append(o: bool) =
        buf.Add(string<bool> o)
        x

    member x.Append(o: obj) =
        buf.Add(string<obj> o)
        x

    member x.Append(cs: char[]) =
        buf.Add(System.String(cs))
        x

    member x.Append(s: StringBuilder) =
        buf.Add(s.ToString())
        x

    member x.AppendFormat(fmt: string, o: obj) =
        buf.Add(System.String.Format(fmt, o))
        x

    member x.AppendFormat(provider: IFormatProvider, fmt: string, o: obj) =
        buf.Add(System.String.Format(provider, fmt, o))
        x

    member x.AppendLine() =
        buf.Add(System.Environment.NewLine)
        x

    member x.AppendLine(s: string) =
        buf.Add(s)
        buf.Add(System.Environment.NewLine)
        x

    member x.Replace(oldValue: char, newValue: char) =
        for i = buf.Count - 1 downto 0 do
            buf[i] <- buf[i].Replace(oldValue, newValue)

        x

    member x.Replace(oldValue: string, newValue: string) =
        for i = buf.Count - 1 downto 0 do
            buf[i] <- buf[i].Replace(oldValue, newValue)

        x

    member x.Length =
        let mutable len = 0

        for i = buf.Count - 1 downto 0 do
            len <- len + buf[i].Length

        len

    override _.ToString() = System.String.Concat(buf)

    member x.ToString(firstIndex: int, length: int) =
        let str = x.ToString()
        str.Substring(firstIndex, length)

    member x.Clear() =
        buf.Clear()
        x
