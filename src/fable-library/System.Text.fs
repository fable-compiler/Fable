namespace System.Text

type StringBuilder(value: string, capacity: int) =
    let buf = ResizeArray<string>(capacity)
    do if not (System.String.IsNullOrEmpty value) then buf.Add(value)
    new (capacity: int) = StringBuilder("", capacity)
    new (value: string) = StringBuilder(value, 16)
    new () = StringBuilder("", 16)
    member x.Append(s: string) = buf.Add(s); x
    member x.Append(c: char) = buf.Add(string c); x
    member x.AppendFormat(fmt: string, o: obj) = buf.Add(System.String.Format(fmt, o)); x
    member x.AppendLine() = buf.Add(System.Environment.NewLine); x
    member x.AppendLine(s: string) = buf.Add(s); buf.Add(System.Environment.NewLine); x
    override __.ToString() = System.String.Concat(buf)
    member x.Length = buf |> Seq.sumBy String.length
    member x.ToString(firstIndex: int, length: int) =
        let str = x.ToString()
        str.Substring(firstIndex, length)
    member x.Clear() =
        buf.Clear()
        x
