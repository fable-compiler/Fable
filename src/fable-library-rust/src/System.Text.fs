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

    member x.Append(o: bool) = x.Append(string<bool> o)
    member x.Append(c: char) = x.Append(string<char> c)

    member x.Append(c: char, repeatCount: int) =
        let s = String.replicate repeatCount (string<char> c)
        buf.Add(s)
        x

    member x.Append(o: int8) = x.Append(string<int8> o)
    member x.Append(o: byte) = x.Append(string<byte> o)
    member x.Append(o: int16) = x.Append(string<int16> o)
    member x.Append(o: uint16) = x.Append(string<uint16> o)
    member x.Append(o: int32) = x.Append(string<int32> o)
    member x.Append(o: uint32) = x.Append(string<uint32> o)
    member x.Append(o: int64) = x.Append(string<int64> o)
    member x.Append(o: uint64) = x.Append(string<uint64> o)
    member x.Append(o: float32) = x.Append(string<float32> o)
    member x.Append(o: float) = x.Append(string<float> o)

    member x.Append(s: string, index: int, count: int) = x.Append(s.Substring(index, count))

    member x.Append(cs: char[]) = x.Append(System.String(cs))
    member x.Append(sb: StringBuilder) = x.Append(sb.ToString())
    // member x.Append(o: obj) = x.Append(string o)

    // see Replacements for AppendFormat
    // member x.AppendFormat(fmt: string, o: obj) = x.Append(System.String.Format(fmt, o))
    // member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o: obj) = x.Append(System.String.Format(provider, fmt, o))

    member x.AppendLine() = x.Append(System.Environment.NewLine)
    member x.AppendLine(s: string) = x.Append(s).AppendLine()

    member x.Clear() =
        buf.Clear()
        x

    member x.Chars

        with get (index: int) =
            let mutable len = 0
            let mutable i = 0

            while i < buf.Count && len + buf[i].Length <= index do
                len <- len + buf[i].Length
                i <- i + 1

            if index < 0 || i >= buf.Count then
                failwith "Index was outside the bounds of the array"
            else
                let pos = index - len
                buf[i][pos]

        and set (index: int) (value: char) =
            let mutable len = 0
            let mutable i = 0

            while i < buf.Count && len + buf[i].Length <= index do
                len <- len + buf[i].Length
                i <- i + 1

            if index < 0 || i >= buf.Count then
                failwith "Index was outside the bounds of the array"
            else
                let pos = index - len
                buf[i] <- buf[i][0 .. (pos - 1)] + (string<char> value) + buf[i][(pos + 1) ..]

    member x.Length =
        let mutable len = 0

        for i = buf.Count - 1 downto 0 do
            len <- len + buf[i].Length

        len

    member x.Replace(oldValue: char, newValue: char) =
        let oldValue = string<char> oldValue
        let newValue = string<char> newValue

        for i = buf.Count - 1 downto 0 do
            buf[i] <- buf[i].Replace(oldValue, newValue)

        x

    member x.Replace(oldValue: string, newValue: string) =
        let str = x.ToString().Replace(oldValue, newValue)
        x.Clear().Append(str)

    override _.ToString() = System.String.Concat(buf |> asArray)

    member x.ToString(index: int, count: int) = x.ToString().Substring(index, count)
