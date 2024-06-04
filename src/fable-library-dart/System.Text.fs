namespace System.Text

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

    // member x.AppendFormat(fmt: string, o: obj) =
    //     buf.Add(System.String.Format(fmt, o))
    //     x

    // member x.AppendFormat(fmt: string, o1: obj, o2: obj) =
    //     buf.Add(System.String.Format(fmt, o1, o2))
    //     x

    // member x.AppendFormat(fmt: string, o1: obj, o2: obj, o3: obj) =
    //     buf.Add(System.String.Format(fmt, o1, o2, o3))
    //     x

    // member x.AppendFormat(fmt: string, arr: obj[]) =
    //     buf.Add(System.String.Format(fmt, arr))
    //     x

    // member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o: obj) =
    //     buf.Add(System.String.Format(provider, fmt, o))
    //     x

    // member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o1: obj, o2: obj) =
    //     buf.Add(System.String.Format(provider, fmt, o1, o2))
    //     x

    // member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o1: obj, o2: obj, o3: obj) =
    //     buf.Add(System.String.Format(provider, fmt, o1, o2, o3))
    //     x

    // member x.AppendFormat(provider: System.IFormatProvider, fmt: string, arr: obj[]) =
    //     buf.Add(System.String.Format(provider, fmt, arr))
    //     x

    member x.AppendLine() =
        buf.Add(System.Environment.NewLine)
        x

    member x.AppendLine(s: string) =
        buf.Add(s)
        buf.Add(System.Environment.NewLine)
        x

    member x.Clear() =
        buf.Clear()
        x

    member x.Chars

        with get (index: int) =
            let mutable len = 0
            let mutable i = -1

            while i + 1 < buf.Count && len < index do
                i <- i + 1
                len <- len + buf[i].Length

            if index < 0 || i < 0 || i >= buf.Count then
                failwith "Index was outside the bounds of the array"
            else
                let pos = len - index - 1
                buf[i][pos]

        and set (index: int) (value: char) =
            let mutable len = 0
            let mutable i = -1

            while i + 1 < buf.Count && len < index do
                i <- i + 1
                len <- len + buf[i].Length

            if index < 0 || i < 0 || i >= buf.Count then
                failwith "Index was outside the bounds of the array"
            else
                let pos = len - index - 1
                buf[i] <- buf[i][0 .. (pos - 1)] + (string value) + buf[i][(pos + 1) ..]

    member x.Replace(oldValue: char, newValue: char) =
        let oldValue = string oldValue
        let newValue = string newValue

        for i = buf.Count - 1 downto 0 do
            buf[i] <- buf[i].Replace(oldValue, newValue)

        x

    member x.Replace(oldValue: string, newValue: string) =
        let str = x.ToString().Replace(oldValue, newValue)
        x.Clear().Append(str)

    member x.Length =
        let mutable len = 0

        for i = buf.Count - 1 downto 0 do
            len <- len + buf[i].Length

        len

    override _.ToString() = System.String.Concat(buf)

    member x.ToString(firstIndex: int, length: int) =
        let str = x.ToString()
        str.Substring(firstIndex, length)
