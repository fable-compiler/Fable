namespace System.Text

open System
open Fable.Core

type StringBuilder(value: string, capacity: int) =
    let buf = ResizeArray<string>(capacity)

    do
        if not (System.String.IsNullOrEmpty value) then
            buf.Add(value)

    new(capacity: int) = StringBuilder("", capacity)
    new(value: string) = StringBuilder(value, 16)
    new() = StringBuilder("", 16)

    member x.Append(s: string | null) =
        buf.Add(string<string | null> s)
        x

    member x.Append(s: string | null, startIndex: int, count: int) =
        buf.Add((string<string | null> s).Substring(startIndex, count))
        x

    member x.Append(c: char) =
        buf.Add(string<char> c)
        x

    member x.Append(c: char, repeatCount: int) =
        let s = String.replicate repeatCount (string<char> c)
        buf.Add(s)
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

    member x.AppendFormat(fmt: string, o1: obj, o2: obj) =
        buf.Add(System.String.Format(fmt, o1, o2))
        x

    member x.AppendFormat(fmt: string, o1: obj, o2: obj, o3: obj) =
        buf.Add(System.String.Format(fmt, o1, o2, o3))
        x

    member x.AppendFormat(fmt: string, arr: obj[]) =
        buf.Add(System.String.Format(fmt, arr))
        x

    member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o: obj) =
        buf.Add(System.String.Format(provider, fmt, o))
        x

    member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o1: obj, o2: obj) =
        buf.Add(System.String.Format(provider, fmt, o1, o2))
        x

    member x.AppendFormat(provider: System.IFormatProvider, fmt: string, o1: obj, o2: obj, o3: obj) =
        buf.Add(System.String.Format(provider, fmt, o1, o2, o3))
        x

    member x.AppendFormat(provider: System.IFormatProvider, fmt: string, arr: obj[]) =
        buf.Add(System.String.Format(provider, fmt, arr))
        x

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

    member x.Replace(oldValue: char, newValue: char) =
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

    // -------------------------------------------------------------------------
    // Object overrides
    // -------------------------------------------------------------------------

    override x.Equals(other: obj) =
        match other with
        | :? StringBuilder as sb -> x.ToString() = sb.ToString()
        | _ -> false

    override x.GetHashCode() = x.ToString().GetHashCode()

    // -------------------------------------------------------------------------
    // Python Stringable - provides __str__ and __repr__
    // -------------------------------------------------------------------------

    interface Py.Stringable
