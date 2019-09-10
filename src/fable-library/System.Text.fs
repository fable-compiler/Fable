namespace System.Text

type StringBuilder(value: string, capacity: int) =
    let buf = ResizeArray<string>(capacity)
    do if not(isNull value) then buf.Add(value)
    new (capacity: int) = StringBuilder(null, capacity)
    new (value: string) = StringBuilder(value, 16)
    new () = StringBuilder(null, 16)
    member x.Append(s: string) = buf.Add(s); x
    member x.Append(c: char) = buf.Add(string c); x
    member x.AppendFormat(fmt: string, o: obj) = buf.Add(System.String.Format(fmt, o)); x
    override __.ToString() = System.String.Concat(buf)
