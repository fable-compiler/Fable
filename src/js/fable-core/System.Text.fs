namespace System.Text

type StringBuilder(?s: string) =
    let buf = ResizeArray<string>()
    do if Option.isSome s then buf.Add(s.Value)
    member x.Append(s: string) = buf.Add(s); x
    member x.AppendFormat(fmt: string, o: obj) = buf.Add(System.String.Format(fmt, o)); x
    override x.ToString() = System.String.Concat(buf)
