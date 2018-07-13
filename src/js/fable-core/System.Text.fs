namespace System.Text

type StringBuilder(capacity: int, value: string) =
    let buf = ResizeArray<string>(capacity)
    do if not(isNull value) then buf.Add(value)
    new (capacity: int) = StringBuilder(capacity, null)
    new (value: string) = StringBuilder(16, value)
    new () = StringBuilder(16, null)
    member x.Append(s: string) = buf.Add(s); x
    member x.AppendFormat(fmt: string, o: obj) = buf.Add(System.String.Format(fmt, o)); x
    override __.ToString() = System.String.Concat(buf)
