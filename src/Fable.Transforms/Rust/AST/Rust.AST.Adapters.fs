// Adapters for common Rust functionality

module Fable.Transforms.Rust.AST.Adapters

type i8 = int8
type i16 = int16
type i32 = int32
type i64 = int64
type i128 = int64 //System.Int128
type isize = int //nativeint
type u8 = uint8
type u16 = uint16
type u32 = uint32
type u64 = uint64
type u128 = uint64 //System.UInt128
type usize = int //unativeint // intentionally the same as isize
type f32 = float32
type f64 = float

type Symbol = string
type String = System.Text.StringBuilder

type P<'T> = 'T
type Box<'T> = 'T
type Lrc<'T> = 'T
type Into<'T> = 'T
type From<'T> = 'T

type Vec<'T> = ResizeArray<'T>
type VecDeque<'T> = ResizeArray<'T> // TODO: use Deque impl
type Map<'K, 'V> = System.Collections.Generic.Dictionary<'K, 'V>

type ToString =
    abstract to_string: unit -> string

type Option<'T> with

    member self.is_some() = self.IsSome
    member self.is_none() = self.IsNone
    member self.unwrap() = self.Value
    member self.unwrap_or(value: 'T) = Option.defaultValue value self
    member self.unwrap_or_else(f: unit -> 'T) = Option.defaultWith f self
    member self.and_then(f: 'T -> 'U option) = Option.bind f self
    member self.map(f: 'T -> 'U) = Option.map f self
    member self.iterate(f: 'T -> unit) = Option.iter f self

type System.Boolean with

    member self.then_some<'T>(t: 'T) =
        if self then
            Some t
        else
            None

type System.Collections.Generic.IList<'T> with

    member self.len() = self.Count
    member self.is_empty() = self.Count = 0

    member self.first() =
        if self.Count = 0 then
            None
        else
            Some(self.Item(0))

    member self.last() =
        if self.Count = 0 then
            None
        else
            Some(self.Item(self.Count - 1))

type System.Collections.Generic.IEnumerable<'T> with

    member self.iter() = self.GetEnumerator()

type System.Collections.Generic.IEnumerator<'T> with

    member self.next() =
        if self.MoveNext() then
            Some(self.Current)
        else
            None

    member self.enumerate() =
        { new System.Collections.Generic.IEnumerable<'T> with
            member x.GetEnumerator() = self
          interface System.Collections.IEnumerable with
              member x.GetEnumerator() =
                  (self :> System.Collections.IEnumerator)
        }

type System.Collections.Generic.List<'T> with

    static member with_capacity(capacity: usize) = ResizeArray<'T>(capacity)

    member self.GetSlice(startIndex: int option, endIndex: int option) =
        match (startIndex, endIndex) with
        | None, None -> self
        | Some(i), None -> self.GetRange(i, self.Count - i)
        | None, Some(j) -> self.GetRange(0, j)
        | Some(i), Some(j) -> self.GetRange(i, j - i + 1)

    member self.clone() = self.GetRange(0, self.Count)
    member self.map(f: 'T -> 'U) = self.ConvertAll(System.Converter(f))
    member self.push(item: 'T) = self.Add(item)

    member self.pop() =
        if self.Count > 0 then
            let res = Some(self.Item(self.Count - 1))
            self.RemoveAt(self.Count - 1)
            res
        else
            None

    member self.extend(items: 'T seq) = self.AddRange(items)

    member self.split_first() =
        if self.Count = 0 then
            None
        else
            Some(self.Item(0), self.GetRange(1, self.Count - 1))

    member self.split_last() =
        if self.Count = 0 then
            None
        else
            Some(self.Item(self.Count - 1), self.GetRange(0, self.Count - 1))

    // VecDeque: // TODO: better impl
    member self.push_front(item: 'T) = self.Insert(0, item)
    member self.push_back(item: 'T) = self.Add(item)

    member self.front() =
        if self.Count > 0 then
            Some(self.Item(0))
        else
            None

    member self.back() = self.last ()

    member self.pop_front() =
        if self.Count > 0 then
            let res = Some(self.Item(0))
            self.RemoveAt(0)
            res
        else
            None

    member self.pop_back() = self.pop ()

type System.String with

    member self.as_str() = self
    member self.to_string() = self
    member self.len() = self.Length
    member self.is_empty() = self.Length = 0
    member self.chars() = self.GetEnumerator()
    member self.repeat(n: usize) = String.replicate (int n) self

    member self.last() =
        if self.Length = 0 then
            None
        else
            Some(self.Chars(self.Length - 1))

    member self.escape_debug() =
        // escapes \\, \', \", \t, \r, \n, [\x00-\x1F]
        let res =
            self.Replace("\\", @"\\").Replace("\'", @"\'").Replace("\"", @"\""")

        let res =
            res.Replace("\t", @"\t").Replace("\r", @"\r").Replace("\n", @"\n")

        let res =
            System.Text.RegularExpressions.Regex.Replace(
                res,
                @"[\x00-\x1F]",
                fun c ->
                    System.String.Format(
                        @"\u{0}{1:x4}{2}",
                        "{",
                        int c.Value[0],
                        "}"
                    )
            )

        res

    member self.escape_default() =
        // escapes \\, \', \", \t, \r, \n, [^\x20-\x7F]
        let res =
            self.Replace("\\", @"\\").Replace("\'", @"\'").Replace("\"", @"\""")

        let res =
            res.Replace("\t", @"\t").Replace("\r", @"\r").Replace("\n", @"\n")

        let res =
            System.Text.RegularExpressions.Regex.Replace(
                res,
                @"[^\x20-\x7F]",
                fun c ->
                    System.String.Format(
                        @"\u{0}{1:x4}{2}",
                        "{",
                        int c.Value[0],
                        "}"
                    )
            )

        res

type System.Text.StringBuilder with

    static member new_() = System.Text.StringBuilder()
    static member from(s: string) = System.Text.StringBuilder(s)
    member self.as_str() = self.ToString()
    member self.push_str(s: string) = self.Append(s) |> ignore
    member self.push(c: char) = self.Append(c) |> ignore

module fmt =
    type Result = Result<unit, System.Exception>

    type Formatter =
        abstract write_str: string -> unit

    type Display =
        abstract fmt: Formatter -> Result

// // replace {} with {n}
// let fixFormat(fmt: string) =
//     let rec loop (str: string) i =
//         let pos = str.IndexOf("{}")
//         if pos < 0 then str
//         else loop (str.Insert(pos + 1, string i)) (i + 1)
//     loop fmt 0

type Macros =
    static member assert_eq(actual, expected) = assert (actual = expected)
    static member assert_ne(actual, expected) = assert (actual <> expected)
    static member unreachable() = failwith "should not happen"
    static member panic() = failwith "panic!"
    static member panic(str: string) = failwith str

    static member format(fmt: string, [<System.ParamArray>] args) =
        System.String.Format(fmt, args)

    static member write(buf: String, str: string) = buf.push_str (str)

    static member write(buf: String, fmt: string, [<System.ParamArray>] args) =
        buf.push_str (Macros.format (fmt, args))
#if false //DEBUG
    static member debug(fmt: string, [<System.ParamArray>] args) =
        System.Console.WriteLine(fmt, args)
#else
    static member debug _ = ()
#endif

[<AutoOpen>]
module ArrayHelpers =
    let split_first (arr: 'T[]) =
        if arr.Length = 0 then
            None
        else
            Some(arr[0], arr[1..])

    let split_last (arr: 'T[]) =
        if arr.Length = 0 then
            None
        else
            Some(arr[arr.Length - 1], arr[0 .. arr.Length - 1])
