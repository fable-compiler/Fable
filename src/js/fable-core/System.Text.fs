namespace System.Text

type StringBuilder(?capacity: int, ?value: string) =
    let buf = ResizeArray<string>(defaultArg capacity 16)
    do if Option.isSome value then buf.Add(value.Value)
    new (capacity: int) = StringBuilder(capacity=capacity, ?value=None)
    new (value: string) = StringBuilder(?capacity=None, value=value)
    member x.Append(s: string) = buf.Add(s); x
    member x.AppendFormat(fmt: string, o: obj) = buf.Add(System.String.Format(fmt, o)); x
    override __.ToString() = System.String.Concat(buf)
