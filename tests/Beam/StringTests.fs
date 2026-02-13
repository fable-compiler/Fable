module Fable.Tests.String

open System
open Fable.Tests.Util
open Util.Testing

// --- Basic string operations ---

[<Fact>]
let ``test String literal works`` () =
    "hello" |> equal "hello"

[<Fact>]
let ``test String concatenation with + works`` () =
    "hello" + " " + "world" |> equal "hello world"

[<Fact>]
let ``test String interpolation works`` () =
    let name = "world"
    $"hello {name}" |> equal "hello world"

[<Fact>]
let ``test String interpolation with expression works`` () =
    let x = 21
    $"the answer is {x * 2}" |> equal "the answer is 42"

[<Fact>]
let ``test String equality works`` () =
    ("hello" = "hello") |> equal true
    ("hello" = "world") |> equal false

[<Fact>]
let ``test String inequality works`` () =
    ("hello" <> "world") |> equal true
    ("hello" <> "hello") |> equal false

[<Fact>]
let ``test Empty string works`` () =
    "" |> equal ""

[<Fact>]
let ``test String.Empty works`` () =
    let s = String.Empty
    s |> equal ""

[<Fact>]
let ``test String concatenation with variable works`` () =
    let a = "hello"
    let b = " world"
    (a + b) |> equal "hello world"

// --- String.Length ---

[<Fact>]
let ``test String.Length works`` () =
    "hello".Length |> equal 5

[<Fact>]
let ``test String.Length of empty string works`` () =
    "".Length |> equal 0

[<Fact>]
let ``test String.length function works`` () =
    "AbC" |> String.length |> equal 3

// --- Case conversion ---

[<Fact>]
let ``test String.ToUpper works`` () =
    "AbC".ToUpper() |> equal "ABC"

[<Fact>]
let ``test String.ToLower works`` () =
    "aBc".ToLower() |> equal "abc"

[<Fact>]
let ``test String.ToUpperInvariant works`` () =
    "AbC".ToUpperInvariant() |> equal "ABC"

[<Fact>]
let ``test String.ToLowerInvariant works`` () =
    "aBc".ToLowerInvariant() |> equal "abc"

// --- Trim ---

[<Fact>]
let ``test String.Trim works`` () =
    "   abc   ".Trim() |> equal "abc"

[<Fact>]
let ``test String.TrimStart works`` () =
    "   abc   ".TrimStart() |> equal "abc   "

[<Fact>]
let ``test String.TrimEnd works`` () =
    "   abc   ".TrimEnd() |> equal "   abc"

[<Fact>]
let ``test String.Trim with chars works`` () =
    @"\\\abc///".Trim('\\','/') |> equal "abc"

[<Fact>]
let ``test String.TrimStart with chars works`` () =
    "!!--abc   ".TrimStart('!','-') |> equal "abc   "

[<Fact>]
let ``test String.TrimEnd with chars works`` () =
    "   abc??**".TrimEnd('*','?') |> equal "   abc"
    @"\foo\bar\".Replace("\\", "/").TrimEnd('/') |> equal "/foo/bar"

// --- StartsWith / EndsWith ---

[<Fact>]
let ``test String.StartsWith works`` () =
    "abcd".StartsWith("ab") |> equal true
    "abcd".StartsWith("bc") |> equal false
    "abcd".StartsWith("cd") |> equal false
    "abcd".StartsWith("abcdx") |> equal false
    "abcd".StartsWith("abcd") |> equal true

[<Fact>]
let ``test String.StartsWith char works`` () =
    "abcd".StartsWith('a') |> equal true
    "abcd".StartsWith('d') |> equal false

[<Fact>]
let ``test String.StartsWith with OrdinalIgnoreCase works`` () =
    "ABCD".StartsWith("ab", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABCD".StartsWith("AB", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABCD".StartsWith("BC", StringComparison.OrdinalIgnoreCase) |> equal false
    "ABCD".StartsWith("cd", StringComparison.OrdinalIgnoreCase) |> equal false
    "ABCD".StartsWith("abcdx", StringComparison.OrdinalIgnoreCase) |> equal false
    "ABCD".StartsWith("abcd", StringComparison.OrdinalIgnoreCase) |> equal true

[<Fact>]
let ``test String.EndsWith works`` () =
    "abcd".EndsWith("ab") |> equal false
    "abcd".EndsWith("cd") |> equal true
    "abcd".EndsWith("bc") |> equal false
    "abcd".EndsWith("abcdx") |> equal false
    "abcd".EndsWith("abcd") |> equal true

[<Fact>]
let ``test String.EndsWith char works`` () =
    "abcd".EndsWith('a') |> equal false
    "abcd".EndsWith('d') |> equal true

[<Fact>]
let ``test String.EndsWith with OrdinalIgnoreCase works`` () =
    "ABCD".EndsWith("ab", StringComparison.OrdinalIgnoreCase) |> equal false
    "ABCD".EndsWith("CD", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABCD".EndsWith("cd", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABCD".EndsWith("bc", StringComparison.OrdinalIgnoreCase) |> equal false
    "ABCD".EndsWith("xabcd", StringComparison.OrdinalIgnoreCase) |> equal false
    "ABCD".EndsWith("abcd", StringComparison.OrdinalIgnoreCase) |> equal true

// --- Substring ---

[<Fact>]
let ``test String.Substring with start index works`` () =
    "abcdefg".Substring(2) |> equal "cdefg"

[<Fact>]
let ``test String.Substring with start and length works`` () =
    "abcdefg".Substring(2, 2) |> equal "cd"

// --- Replace ---

[<Fact>]
let ``test String.Replace works`` () =
    "abc abc abc".Replace("abc", "d") |> equal "d d d"
    // String.Replace does not get stuck in endless loop
    "...".Replace(".", "..") |> equal "......"

// --- Contains ---

[<Fact>]
let ``test String.Contains works`` () =
    "ABC".Contains("B") |> equal true
    "ABC".Contains("Z") |> equal false

// --- IndexOf ---

[<Fact>]
let ``test String.IndexOf works`` () =
    "abcd".IndexOf("bc") * 100 + "abcd".IndexOf("bd") |> equal 99

[<Fact>]
let ``test String.IndexOf works with offset`` () =
    "abcdbc".IndexOf("bc", 3) |> equal 4

[<Fact>]
let ``test String.IndexOf char works`` () =
    "abcd".IndexOf('b') * 100 + "abcd".IndexOf('e') |> equal 99

[<Fact>]
let ``test String.IndexOf char works with offset`` () =
    "abcdbc".IndexOf('b', 3) |> equal 4

// --- LastIndexOf ---

[<Fact>]
let ``test String.LastIndexOf works`` () =
    "abcdbc".LastIndexOf("bc") * 100 + "abcd".LastIndexOf("bd") |> equal 399

[<Fact>]
let ``test String.LastIndexOf works with offset`` () =
    "abcdbcebc".LastIndexOf("bc", 3) |> equal 1

[<Fact>]
let ``test String.LastIndexOf char works`` () =
    "abcdbc".LastIndexOf('b') * 100 + "abcd".LastIndexOf('e') |> equal 399

[<Fact>]
let ``test String.LastIndexOf char works with offset`` () =
    "abcdbcebc".LastIndexOf('b', 3) |> equal 1

// --- Access char by index ---

[<Fact>]
let ``test Access char by index works`` () =
    let c = "abcd".[2]
    equal 'c' c

[<Fact>]
let ``test String item works`` () =
    "AbC".[1] |> equal 'b'

[<Fact>]
let ``test String.Chars works`` () =
    let input = "hello"
    input.Chars(2) |> equal 'l'

// --- Split ---

[<Fact>]
let ``test String.Split works`` () =
    let parts = "a,b,c".Split(",")
    Array.length parts |> equal 3
    parts.[0] |> equal "a"
    parts.[1] |> equal "b"
    parts.[2] |> equal "c"

[<Fact>]
let ``test String.Split with char works`` () =
    "a b c  d".Split(' ')
    |> (=) [|"a";"b";"c";"";"d"|] |> equal true

// --- Join ---

[<Fact>]
let ``test String.Join works`` () =
    System.String.Join(", ", [| "a"; "b"; "c" |]) |> equal "a, b, c"

[<Fact>]
let ``test String.Join with list works`` () =
    System.String.Join("-", [| "hello"; "world" |]) |> equal "hello-world"

// --- String module functions ---

[<Fact>]
let ``test String.concat works`` () =
    String.concat "--" ["a"; "b"; "c"] |> equal "a--b--c"
    seq { yield "a"; yield "b"; yield "c" }
    |> String.concat "-" |> equal "a-b-c"

[<Fact>]
let ``test String.replicate works`` () =
    String.replicate 3 "hi there" |> equal "hi therehi therehi there"

[<Fact>]
let ``test String.forall and exists work`` () =
    "!!!" |> String.forall (fun c -> c = '!') |> equal true
    "a!a" |> String.forall (fun c -> c = '!') |> equal false
    "aaa" |> String.forall (fun c -> c = '!') |> equal false

[<Fact>]
let ``test String.init works`` () =
    String.init 3 (fun i -> "a") |> equal "aaa"

[<Fact>]
let ``test String.collect works`` () =
    "abc" |> String.collect (fun c -> "bcd") |> equal "bcdbcdbcd"

[<Fact>]
let ``test String.iter works`` () =
    let res = ref ""
    "Hello world!"
    |> String.iter (fun c -> res.Value <- res.Value + c.ToString())
    equal "Hello world!" res.Value

[<Fact>]
let ``test String.iteri works`` () =
    let mutable res = ""
    "Hello world!"
    |> String.iteri (fun c i -> res <- res + i.ToString() + c.ToString())
    equal "H0e1l2l3o4 5w6o7r8l9d10!11" res

[<Fact>]
let ``test String.map works`` () =
    "Hello world!" |> String.map (fun c -> if c = 'H' then '_' else c)
    |> equal "_ello world!"

[<Fact>]
let ``test String.mapi works`` () =
    "Hello world!" |> String.mapi (fun i c -> if i = 1 || c = 'H' then '_' else c)
    |> equal "__llo world!"

[<Fact>]
let ``test String.filter works`` () =
    String.filter (fun x -> x <> '.') "a.b.c" |> equal "abc"

[<Fact>]
let ``test String.filter works when predicate matches everything`` () =
    String.filter (fun x -> x <> '.') "abc" |> equal "abc"

[<Fact>]
let ``test String.filter works when predicate does not match`` () =
    String.filter (fun x -> x <> '.') "..." |> equal ""

[<Fact>]
let ``test String.filter works with empty string`` () =
    String.filter (fun x -> x <> '.') "" |> equal ""

// --- IsNullOrEmpty / IsNullOrWhiteSpace ---

[<Fact>]
let ``test String.IsNullOrEmpty with empty string returns true`` () =
    System.String.IsNullOrEmpty("") |> equal true

[<Fact>]
let ``test String.IsNullOrEmpty with non-empty string returns false`` () =
    System.String.IsNullOrEmpty("hello") |> equal false

[<Fact>]
let ``test String.IsNullOrWhiteSpace works on blank only string`` () =
    String.IsNullOrWhiteSpace "      " |> equal true

[<Fact>]
let ``test String.IsNullOrWhiteSpace works on string with blanks`` () =
    String.IsNullOrWhiteSpace "Fri Jun 30 2017 12:30:00 GMT+0200"
    |> equal false

// --- String.Remove / String.Insert ---

[<Fact>]
let ``test String.Remove works`` () =
    "abcd".Remove(2) |> equal "ab"
    "abcd".Remove(1,2) |> equal "ad"
    "abcd".Remove(0,2) |> equal "cd"
    "abcd".Remove(0,4) |> equal ""
    "abcd".Remove(0,0) |> equal "abcd"

[<Fact>]
let ``test String.Insert works`` () =
    "foobar".Insert(3, " is ") |> equal "foo is bar"

// --- ToCharArray ---

[<Fact>]
let ``test String.ToCharArray works`` () =
    let arr = "abcd".ToCharArray()
    arr |> equal [|'a';'b';'c';'d'|]

[<Fact>]
let ``test String.ToCharArray with range works`` () =
    let arr = "abcd".ToCharArray(1, 2)
    arr |> equal [|'b';'c'|]

// --- String.Equals ---

[<Fact>]
let ``test System.String.Equals works`` () =
    System.String.Equals("abc", "abc") |> equal true
    System.String.Equals("ABC", "abc") |> equal false
    System.String.Equals("abc", "abd") |> equal false
    "abc".Equals("abc") |> equal true
    "ABC".Equals("abc") |> equal false
    "abc".Equals("abd") |> equal false

[<Fact>]
let ``test System.String.Equals with StringComparison works`` () =
    System.String.Equals("ABC", "abc", StringComparison.Ordinal) |> equal false
    System.String.Equals("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal true
    "ABC".Equals("abc", StringComparison.Ordinal) |> equal false
    "ABC".Equals("abc", StringComparison.OrdinalIgnoreCase) |> equal true

// --- String.Compare ---

[<Fact>]
let ``test String.Compare works`` () =
    System.String.Compare("abc", "abc") |> equal 0
    System.String.Compare("abc", "abd") |> equal -1
    System.String.Compare("bbc", "abd") |> equal 1

[<Fact>]
let ``test String.Compare with ignoreCase works`` () =
    System.String.Compare("ABC", "abc", true) |> equal 0
    System.String.Compare("ABC", "abd", true) |> equal -1
    System.String.Compare("BBC", "abd", true) |> equal 1

[<Fact>]
let ``test String.Compare with StringComparison works`` () =
    System.String.Compare("ABC", "abc", StringComparison.Ordinal) < 0 |> equal true
    System.String.Compare("ABC", "abc", StringComparison.OrdinalIgnoreCase) |> equal 0

// --- String.Concat ---

[<Fact>]
let ``test System.String.Concat works`` () =
    String.Concat("a", "b", "c") |> equal "abc"

// --- String constructors ---

[<Fact>]
let ``test String.ctor with char array works`` () =
    System.String([|'f'; 'a'; 'b'; 'l'; 'e'|]) |> equal "fable"

[<Fact>]
let ``test String.ctor with char and count works`` () =
    System.String('f', 5) |> equal "fffff"

[<Fact>]
let ``test String.ctor with char array and range works`` () =
    System.String([|'f'; 'a'; 'b'; 'l'; 'e'|], 1, 3) |> equal "abl"

// --- PadLeft / PadRight ---

[<Fact>]
let ``test String.PadLeft works`` () =
    "abc".PadLeft(6) |> equal "   abc"

[<Fact>]
let ``test String.PadRight works`` () =
    "abc".PadRight(6) |> equal "abc   "

[<Fact>]
let ``test String.PadLeft with char works`` () =
    "22".PadLeft(10, '0') |> equal "0000000022"
    "333".PadLeft(1) |> equal "333"

[<Fact>]
let ``test String.PadRight with char works`` () =
    "-22".PadRight(10, 'X') |> equal "-22XXXXXXX"

// --- String.exists ---

[<Fact>]
let ``test String.exists works`` () =
    "aaa" |> String.exists (fun c -> c = 'a') |> equal true
    "aaa" |> String.exists (fun c -> c = 'b') |> equal false
    "" |> String.exists (fun c -> c = 'a') |> equal false

// --- String.Join variants ---

[<Fact>]
let ``test String.Join with IEnumerable works`` () =
    System.String.Join("--", seq { yield "a"; yield "b"; yield "c" })
    |> equal "a--b--c"

// --- Enumerating string ---

[<Fact>]
let ``test Enumerating string with Seq.rev works`` () =
    let mutable res = ""
    for c in "HELLO" |> Seq.rev do
        res <- res + (string c)
    equal "OLLEH" res

// --- Passing Char.IsDigit as function reference ---

[<Fact>]
let ``test Passing Char.IsDigit as function reference works`` () =
    "Hello! 123" |> String.filter System.Char.IsDigit |> equal "123"

[<Fact>]
let ``test sprintf works`` () =
    sprintf "%.2f %g" 0.5468989 5.
    |> equal "0.55 5"
    let printer = sprintf "Hi %s, good %s!"
    let printer = printer "Alfonso"
    printer "morning" |> equal "Hi Alfonso, good morning!"
    printer "evening" |> equal "Hi Alfonso, good evening!"

[<Fact>]
let ``test sprintf without arguments works`` () =
    sprintf "hello" |> equal "hello"

[<Fact>]
let ``test sprintf works II`` () =
    let printer2 = sprintf "Hi %s, good %s%s" "Maxime"
    let printer2 = printer2 "afternoon"
    printer2 "?" |> equal "Hi Maxime, good afternoon?"

[<Fact>]
let ``test sprintf displays sign correctly`` () =
    sprintf "%i" 1 |> equal "1"
    sprintf "%d" 1 |> equal "1"
    sprintf "%d" 1L |> equal "1"
    sprintf "%.2f" 1. |> equal "1.00"
    sprintf "%i" -1 |> equal "-1"
    sprintf "%d" -1 |> equal "-1"
    sprintf "%d" -1L |> equal "-1"
    sprintf "%.2f" -1. |> equal "-1.00"

[<Fact>]
let ``test sprintf with different decimal digits works`` () =
    sprintf "Percent: %.0f%%" 5.0 |> equal "Percent: 5%"
    sprintf "Percent: %.2f%%" 5. |> equal "Percent: 5.00%"
    sprintf "Percent: %.1f%%" 5.24 |> equal "Percent: 5.2%"
    sprintf "Percent: %.2f%%" 5.268 |> equal "Percent: 5.27%"
    sprintf "Percent: %f%%" 5.67 |> equal "Percent: 5.670000%"

[<Fact>]
let ``test sprintf "%X" works`` () =
    sprintf "255: %X" 255 |> equal "255: FF"
    sprintf "255: %x" 255 |> equal "255: ff"
    sprintf "4095L: %X" 4095L |> equal "4095L: FFF"

// TODO: Negative hex formatting requires two's complement masking which isn't implemented
// sprintf "-255: %X" -255 |> equal "-255: FFFFFF01"
// sprintf "-4095L: %X" -4095L |> equal "-4095L: FFFFFFFFFFFFF001"

[<Fact>]
let ``test sprintf integers with sign and padding works`` () =
    sprintf "%+04i" 1 |> equal "+001"
    sprintf "%+04i" -1 |> equal "-001"
    sprintf "%5d" -5 |> equal "   -5"
    sprintf "%5d" -5L |> equal "   -5"
    sprintf "%- 4i" 5 |> equal " 5  "

[<Fact>]
let ``test failwithf works`` () =
    let mutable exceptionRaised = false
    try
        failwithf "Error: %s %d" "test" 42
    with
    | ex ->
        exceptionRaised <- true
        ex.Message |> equal "Error: test 42"
    exceptionRaised |> equal true

[<Fact>]
let ``test interpolate works`` () =
    let name = "Phillip"
    let age = 29
    $"Name: %s{name}, Age: %i{age}"
    |> equal "Name: Phillip, Age: 29"

[<Fact>]
let ``test string interpolation works with anonymous records`` () =
    let person =
        {|
            Name = "John"
            Surname = "Doe"
            Age = 32
            Country = "The United Kingdom"
        |}
    $"Hi! My name is %s{person.Name} %s{person.Surname.ToUpper()}. I'm %i{person.Age} years old and I'm from %s{person.Country}!"
    |> equal "Hi! My name is John DOE. I'm 32 years old and I'm from The United Kingdom!"

[<Fact>]
let ``test String.concat with long array works`` () =
    let n = 1_000
    let a = Array.init n (fun _i -> "a")
    let s = String.concat "" a
    s.Length |> equal n

[<Fact>]
let ``test StringBuilder works`` () =
    let sb = System.Text.StringBuilder()
    sb.Append "Hello" |> ignore
    sb.AppendLine () |> ignore
    sb.AppendLine "World!" |> ignore
    let expected = System.Text.StringBuilder()
                      .AppendFormat("Hello{0}World!{0}", Environment.NewLine)
                      .ToString()
    sb.ToString() |> equal expected

[<Fact>]
let ``test String.Format works`` () =
    let arg1, arg2, arg3 = "F#", "Fable", "Babel"
    String.Format("{2} is to {1} what {1} is to {0}", arg1, arg2, arg3)
    |> equal "Babel is to Fable what Fable is to F#"

[<Fact>]
let ``test String.Split with remove empties works`` () =
    let result = "a b c  d ".Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    result.Length |> equal 4
    result.[0] |> equal "a"
    result.[1] |> equal "b"
    result.[2] |> equal "c"
    result.[3] |> equal "d"

// TODO: String.Split 2-arg overload (char[], int) not matched by 3-arg Replacement pattern
// [<Fact>]
// let ``test String.Split with count works`` () =
//     let array = "a b  c d".Split ([|' '|], 2)
//     equal "a" array.[0]
//     equal "b  c d" array.[1]

// TODO: IndexOfAny requires array iteration
// [<Fact>]
// let ``test String.IndexOfAny works`` () =
//     "abcdbcebc".IndexOfAny([|'b'|]) |> equal 1
//     "abcdbcebc".IndexOfAny([|'b'|], 2) |> equal 4
//     "abcdbcebc".IndexOfAny([|'f';'e'|]) |> equal 6

// TODO: String.Join with indices
// [<Fact>]
// let ``test String.Join with indices works`` () =
//     String.Join("**", [|"a"; "b"; "c"; "d"|], 1, 2) |> equal "b**c"

// TODO: Uri encoding/decoding requires library support
// [<Fact>]
// let ``test System.Uri.UnescapeDataString works`` () =
//     System.Uri.UnescapeDataString("Kevin%20van%20Zonneveld%21") |> equal "Kevin van Zonneveld!"

// TODO: FormattableString not yet supported
// [<Fact>]
// let ``test Can create FormattableString`` () =
//     let orderAmount = 100
//     let convert (s: FormattableString) = s
//     let s = convert $"You owe: {orderAmount:N5} {3} {5 = 5}"
//     s.Format |> equal "You owe: {0:N5} {1} {2}"

// --- String slicing ---

[<Fact>]
let ``test String slicing with start works`` () =
    let s = "hello world"
    s.[6..] |> equal "world"

[<Fact>]
let ``test String slicing with end works`` () =
    let s = "hello world"
    s.[..4] |> equal "hello"

[<Fact>]
let ``test String slicing with start and end works`` () =
    let s = "hello world"
    s.[6..10] |> equal "world"

[<Fact>]
let ``test String slicing from beginning works`` () =
    let s = "hello world"
    s.[0..4] |> equal "hello"

[<Fact>]
let ``test String slicing with computed index works`` () =
    let s = "hello world"
    let prefix = "hello "
    s.[prefix.Length..] |> equal "world"

[<Fact>]
let ``test String slicing to computed index works`` () =
    let s = "/api/users/42"
    let path = "/api/users"
    s.[0 .. path.Length - 1] |> equal "/api/users"

// --- String conversion tests ---

[<Fact>]
let ``test Conversion string to int8 works`` () =
    equal 5y (int8 "5")
    equal "5" (string 5y)

[<Fact>]
let ``test Conversion string to negative int8 works`` () =
    equal -5y (int8 "-5")
    equal "-5" (string -5y)

[<Fact>]
let ``test Conversion string to int16 works`` () =
    equal 5s (int16 "5")
    equal "5" (string 5s)

[<Fact>]
let ``test Conversion string to negative int16 works`` () =
    equal -5s (int16 "-5")
    equal "-5" (string -5s)

[<Fact>]
let ``test Conversion string to int32 works`` () =
    equal 5 (int32 "5")
    equal "5" (string 5)

[<Fact>]
let ``test Conversion string to negative int32 works`` () =
    equal -5 (int32 "-5")
    equal "-5" (string -5)

[<Fact>]
let ``test Conversion string to int64 works`` () =
    equal 5L (int64 "5")
    equal "5" (string 5L)

[<Fact>]
let ``test Conversion string to negative int64 works`` () =
    equal -5L (int64 "-5")
    equal "-5" (string -5L)

[<Fact>]
let ``test Conversion string to uint8 works`` () =
    equal 5uy (uint8 "5")
    equal "5" (string 5uy)

[<Fact>]
let ``test Conversion string to uint16 works`` () =
    equal 5us (uint16 "5")
    equal "5" (string 5us)

[<Fact>]
let ``test Conversion string to uint32 works`` () =
    equal 5u (uint32 "5")
    equal "5" (string 5u)

[<Fact>]
let ``test Conversion string to uint64 works`` () =
    equal 5uL (uint64 "5")
    equal "5" (string 5uL)

[<Fact>]
let ``test Conversion string to single works`` () =
    equal 5.f (float32 "5.0")
    equal -5.f (float32 "-5.0")
    (string 5.f).StartsWith("5") |> equal true
    equal 5.25f (float32 "5.25")
    (string 5.25f).StartsWith("5.25") |> equal true

[<Fact>]
let ``test Conversion string to double works`` () =
    equal 5. (float "5.0")
    equal -5. (float "-5.0")
    (string 5.).StartsWith("5") |> equal true
    equal 5.25 (float "5.25")
    (string 5.25).StartsWith("5.25") |> equal true

[<Fact>]
let ``test Conversion string to decimal works`` () =
    equal 5.m (decimal "5.0")
    equal -5.m (decimal "-5.0")
    (string 5.m).StartsWith("5") |> equal true
    equal 5.25m (decimal "5.25")
    (string 5.25m).StartsWith("5.25") |> equal true

[<Fact>]
let ``test Conversion char to int works`` () =
    equal 97 (int 'a')
    equal 'a' (char 97)

[<Fact>]
let ``test Conversion string to char works`` () =
    equal 'a' (char "a")
    equal "a" (string 'a')

// --- String.Trim with special chars ---

[<Fact>]
let ``test String.Trim with special chars works`` () =
    @"()[]{}abc/.?*+-^$|\".Trim(@"()[]{}/.?*+-^$|\".ToCharArray())
    |> equal "abc"

// --- String.StartsWith / EndsWith with ignoreCase boolean ---

[<Fact>]
let ``test String.StartsWith with ignoreCase boolean works`` () =
    let args = [("ab", true); ("AB", true); ("BC", false); ("cd", false); ("abcdx", false); ("abcd", true)]
    for arg in args do
        "ABCD".StartsWith(fst arg, true, System.Globalization.CultureInfo.InvariantCulture)
        |> equal (snd arg)

[<Fact>]
let ``test String.EndsWith with ignoreCase boolean works`` () =
    let args = [("ab", false); ("CD", true); ("cd", true); ("bc", false); ("xabcd", false); ("abcd", true)]
    for arg in args do
        "ABCD".EndsWith(fst arg, true, System.Globalization.CultureInfo.InvariantCulture)
        |> equal (snd arg)

// --- String.Substring error tests ---

[<Fact>]
let ``test String.Substring throws error if startIndex or length are out of bounds`` () =
    let throws f =
        try f () |> ignore; false
        with _ -> true
    throws (fun _ -> "abcdefg".Substring(20)) |> equal true
    throws (fun _ -> "abcdefg".Substring(2, 10)) |> equal true

// --- String.Join additional tests ---

[<Fact>]
let ``test String.Join works with chars`` () =
    [0..10]
    |> List.map (fun _ -> '*')
    |> fun chars -> System.String.Join("", chars)
    |> equal "***********"

[<Fact>]
let ``test String.Join with single argument works`` () =
    System.String.Join(",", "abc") |> equal "abc"
    System.String.Join(",", [|"abc"|]) |> equal "abc"
    System.String.Join(",", ["abc"]) |> equal "abc"

// TODO: String.Join doesn't call ToString on BigInt elements (raw bytes instead of string representation)
// [<Fact>]
// let ``test String.Join with big integers works`` () =
//     System.String.Join("--", [|3I; 5I|])
//     |> equal "3--5"

// --- System.Environment.NewLine ---

#if FABLE_COMPILER
[<Fact>]
let ``test System.Environment.NewLine works`` () =
    System.Environment.NewLine
    |> equal "\n"
#endif

// --- Array slicing ---

[<Fact>]
let ``test Array slicing with start works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    arr.[2..] |> equal [| 3; 4; 5 |]

[<Fact>]
let ``test Array slicing with end works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    arr.[..2] |> equal [| 1; 2; 3 |]

[<Fact>]
let ``test Array slicing with start and end works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    arr.[1..3] |> equal [| 2; 3; 4 |]

// --- Additional String tests ported from Python ---

[<Fact>]
let ``test String.IsNullOrEmpty works`` () =
    System.String.IsNullOrEmpty("") |> equal true
    System.String.IsNullOrEmpty(null) |> equal true
    System.String.IsNullOrEmpty("test") |> equal false
    System.String.IsNullOrEmpty(" \t") |> equal false

[<Fact>]
let ``test String.IsNullOrWhiteSpace works`` () =
    System.String.IsNullOrWhiteSpace("") |> equal true
    System.String.IsNullOrWhiteSpace(null) |> equal true
    System.String.IsNullOrWhiteSpace("test") |> equal false
    System.String.IsNullOrWhiteSpace(" \t") |> equal true

[<Fact>]
let ``test String.Substring works`` () =
    "abcdefg".Substring(2) |> equal "cdefg"

[<Fact>]
let ``test String.Substring with length works`` () =
    "abcdefg".Substring(2, 3) |> equal "cde"

[<Fact>]
let ``test String.Remove with start index works`` () =
    "abcdefg".Remove(2) |> equal "ab"

[<Fact>]
let ``test String.Remove with count works`` () =
    "abcdefg".Remove(2, 3) |> equal "abfg"

[<Fact>]
let ``test Enumerating string with Seq.rev works II`` () =
    let mutable res = ""
    for c in "HELLO" |> Seq.rev do
        res <- res + (string c)
    equal "OLLEH" res

[<Fact>]
let ``test String.filter with Char.IsDigit works`` () =
    "Hello! 123" |> String.filter System.Char.IsDigit |> equal "123"

[<Fact>]
let ``test String.ctor char array works`` () =
    System.String([|'f'; 'a'; 'b'; 'l'; 'e'|]) |> equal "fable"

[<Fact>]
let ``test String.ctor char count works`` () =
    System.String('f', 5) |> equal "fffff"

[<Fact>]
let ``test String.ctor char array with offset works`` () =
    System.String([|'f'; 'a'; 'b'; 'l'; 'e'|], 1, 3) |> equal "abl"

[<Fact>]
let ``test Enumerating string works`` () =
    let mutable res = ""
    for c in "HELLO" do
        res <- res + (string c)
    equal "HELLO" res

// TODO: Typed format specifiers in string interpolation (%.8f{expr}) not yet supported by Beam target
// [<Fact>]
// let ``test string interpolation works with inline expressions`` () =
//     $"I think {3.0 + 0.14} is close to %.8f{3.14159265}!"
//     |> equal "I think 3.14 is close to 3.14159265!"

[<Fact>]
let ``test printing strings with unicode characters`` () =
    let result = sprintf "%s" "🚀"
    result |> equal "🚀"

[<Fact>]
let ``test StringBuilder.Length works`` () =
    let sb = System.Text.StringBuilder()
    sb.Append("Hello") |> ignore
    sb.Length |> equal 5

[<Fact>]
let ``test StringBuilder.Clear works`` () =
    let sb = new System.Text.StringBuilder()
    sb.Append("1111") |> ignore
    sb.Clear() |> ignore
    sb.ToString() |> equal ""

// TODO: StringBuilder.AppendFormat not yet supported in Beam
// [<Fact>]
// let ``test StringBuilder.AppendFormat works`` () =
//     let sb = System.Text.StringBuilder()
//     sb.AppendFormat("Hello{0}World{1}", " ", "!") |> ignore
//     sb.ToString() |> equal "Hello World!"

// TODO: StringBuilder.Chars (indexer) not yet supported in Beam
// [<Fact>]
// let ``test StringBuilder.Chars works`` () =
//     let sb = System.Text.StringBuilder()
//                         .Append("abc")
//                         .Append("def")
//     sb.Chars(0) |> equal 'a'

// TODO: StringBuilder.Replace not yet supported in Beam
// [<Fact>]
// let ``test StringBuilder.Replace works`` () =
//     let sb = System.Text.StringBuilder()
//                         .Append("abc")
//                         .Append("abc")
//                         .Replace('a', 'x')
//                         .Replace("cx", "yz")
//     equal "xbyzbc" (sb.ToString())

[<Fact>]
let ``test StringBuilder.ToString works with index and length`` () =
    let sb = System.Text.StringBuilder()
    sb.Append("Hello") |> ignore
    sb.AppendLine() |> ignore
    sb.ToString(2, 2) |> equal "ll"

[<Fact>]
let ``test System.String.Join with long array works`` () =
    let n = 100_000
    let a = Array.init n (fun _i -> "a")
    let s = String.Join("", a)
    s.Length |> equal n

[<Fact>]
let ``test System.String.Join with long seq works`` () =
    let n = 100_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.Join("", a)
    s.Length |> equal n

[<Fact>]
let ``test System.String.Concat with long array works`` () =
    let n = 100_000
    let a = Array.init n (fun _i -> "a")
    let s = String.Concat(a)
    s.Length |> equal n

[<Fact>]
let ``test System.String.Concat with long seq works`` () =
    let n = 100_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.Concat(a)
    s.Length |> equal n

[<Fact>]
let ``test String.concat with long seq works`` () =
    let n = 1_000_000
    let a = seq { for i in 1..n -> "a" }
    let s = String.concat "" a
    s.Length |> equal n
