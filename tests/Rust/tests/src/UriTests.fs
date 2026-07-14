module Fable.Tests.UriTests

open System
open Util.Testing

[<Fact>]
let ``Uri from absolute uri string works`` () =
    let uri = Uri("http://www.test0.com/hello?a=b#c")
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.test0.com" uri.Host
    equal "/hello" uri.AbsolutePath
    equal "/hello?a=b" uri.PathAndQuery
    equal "?a=b" uri.Query
    equal "#c" uri.Fragment
    equal "http://www.test0.com/hello?a=b#c" uri.AbsoluteUri

[<Fact>]
let ``Uri from absolute uri string with RelativeOrAbsolute works`` () =
    let uri = Uri("http://www.test1.com/hello?a=b#c", UriKind.RelativeOrAbsolute)
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.test1.com" uri.Host
    equal "/hello" uri.AbsolutePath
    equal "/hello?a=b" uri.PathAndQuery
    equal "?a=b" uri.Query
    equal "#c" uri.Fragment
    equal "http://www.test1.com/hello?a=b#c" uri.AbsoluteUri

[<Fact>]
let ``Uri from relative uri string works`` () =
    let uri = Uri("/hello.html", UriKind.Relative)
    equal false uri.IsAbsoluteUri
    equal "/hello.html" (uri.ToString())

[<Fact>]
let ``AbsoluteUri from relative uri should throw`` () =
    let uri = Uri("/hello.html", UriKind.Relative)
    throwsError "This operation is not supported for a relative URI." (fun () -> uri.AbsoluteUri)

[<Fact>]
let ``Uri from relative uri string without uri kind should throws`` () =
    let createInvalidUri () =
        Uri("hello.html")

    throwsError "Invalid URI: The format of the URI could not be determined." createInvalidUri

[<Fact>]
let ``Uri from relative uri with kind Absolute fails`` () =
    let createInvalidUri () =
        Uri("hello.html")

    throwsError "Invalid URI: The format of the URI could not be determined." createInvalidUri

[<Fact>]
let ``Uri from baseUri with relative string works`` () =
    let uri = Uri(Uri("http://www.test2.com/"), "/hello?a=b#c")
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.test2.com" uri.Host
    equal "/hello" uri.AbsolutePath
    equal "/hello?a=b" uri.PathAndQuery
    equal "?a=b" uri.Query
    equal "#c" uri.Fragment
    equal "http://www.test2.com/hello?a=b#c" uri.AbsoluteUri

[<Fact>]
let ``Uri from baseUri with relativeUri works`` () =
    let uri = Uri(Uri("http://www.test3.com/"), Uri("/hello?a=b#c", UriKind.Relative))
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.test3.com" uri.Host
    equal "/hello" uri.AbsolutePath
    equal "/hello?a=b" uri.PathAndQuery
    equal "?a=b" uri.Query
    equal "#c" uri.Fragment
    equal "http://www.test3.com/hello?a=b#c" uri.AbsoluteUri

[<Fact>]
let ``Uri from baseUri with absolute Uri works`` () =
    let absUri = Uri("http://www.example.com/", UriKind.Absolute)
    let uri = Uri(absUri, absUri)
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.example.com" uri.Host
    equal "http://www.example.com/" uri.AbsoluteUri

[<Fact>]
let ``Uri from baseUri with a different absolute Uri yields the absolute one`` () =
    // .NET: when the second URI is absolute it wins and the base is ignored.
    let baseUri = Uri("http://www.base.com/", UriKind.Absolute)
    let absUri = Uri("http://www.other.com/path", UriKind.Absolute)
    let uri = Uri(baseUri, absUri)
    equal "www.other.com" uri.Host
    equal "http://www.other.com/path" uri.AbsoluteUri

[<Fact>]
let ``TryCreate from absolute string with kind Absolute works`` () =
    let (valid, uri) = Uri.TryCreate("http://www.test0.com/hello?a=b#c", UriKind.Absolute)
    equal true valid
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.test0.com" uri.Host
    equal "/hello" uri.AbsolutePath
    equal "/hello?a=b" uri.PathAndQuery
    equal "?a=b" uri.Query
    equal "#c" uri.Fragment
    equal "http://www.test0.com/hello?a=b#c" uri.AbsoluteUri

[<Fact>]
let ``TryCreate from absolute string with kind RelativeOrAbsolute works`` () =
    let (valid, uri) = Uri.TryCreate("http://www.test1.com/hello?a=b#c", UriKind.RelativeOrAbsolute)
    equal true valid
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.test1.com" uri.Host
    equal "/hello" uri.AbsolutePath
    equal "/hello?a=b" uri.PathAndQuery
    equal "?a=b" uri.Query
    equal "#c" uri.Fragment
    equal "http://www.test1.com/hello?a=b#c" uri.AbsoluteUri

[<Fact>]
let ``TryCreate from absolute string with kind Relative fails`` () =
    let (valid, _) = Uri.TryCreate("http://www.test1.com/hello?a=b#c", UriKind.Relative)
    equal false valid

[<Fact>]
let ``TryCreate from relative string with kind Absolute fails`` () =
    let (valid, uri) = Uri.TryCreate("hello?a=b#c", UriKind.Absolute)
    equal false valid

[<Fact>]
let ``TryCreate from relative string with kind RelativeOrAbsolute works`` () =
    let (valid, uri) = Uri.TryCreate("hello?a=b#c", UriKind.RelativeOrAbsolute)
    equal true valid
    equal "hello?a=b#c" (uri.ToString())

[<Fact>]
let ``TryCreate from relative string with kind Relative works`` () =
    let (valid, uri) = Uri.TryCreate("hello?a=b#c", UriKind.Relative)
    equal true valid
    equal "hello?a=b#c" (uri.ToString())

[<Fact>]
let ``TryCreate from absolute uri and absolute uri should work`` () =
    let absUri = Uri("https://example.com", UriKind.Absolute)
    let (valid, uri) = Uri.TryCreate(absUri, absUri)
    equal true valid
    equal absUri uri

[<Fact>]
let ``TryCreate from absolute uri and different absolute uri yields the second`` () =
    // .NET: when the second URI is absolute it wins and the base is ignored.
    let baseUri = Uri("http://www.base.com/", UriKind.Absolute)
    let other = Uri("http://www.other.com/path", UriKind.Absolute)
    let (valid, uri) = Uri.TryCreate(baseUri, other)
    equal true valid
    equal "www.other.com" uri.Host

[<Fact>]
let ``TryCreate from absolute uri and relative uri should work`` () =
    let (valid, uri) = Uri.TryCreate(Uri("https://example.com", UriKind.Absolute), Uri("test", UriKind.Relative))
    equal true valid
    equal "https://example.com/test" (uri.ToString())

[<Fact>]
let ``TryCreate from relative uri and relative uri should fail`` () =
    let relativeUri = Uri("test", UriKind.Relative)
    let (valid, _) = Uri.TryCreate(relativeUri, relativeUri)
    equal false valid

[<Fact>]
let ``TryCreate from relative uri and absolute uri should fail`` () =
    let (valid, _) = Uri.TryCreate(Uri("test", UriKind.Relative), Uri("https://example.com", UriKind.Absolute))
    equal false valid

[<Fact>]
let ``Uri.ToString works`` () =
    let uri = Uri("HTTP://www.test4.com:80/a%20b%20c.html")
    uri.ToString() |> equal "http://www.test4.com/a b c.html"
    sprintf "%A" uri |> equal "http://www.test4.com/a b c.html"

[<Fact>]
let ``Uri from authority-less absolute scheme works`` () =
    // mailto:/tel:/urn: are absolute URIs without an authority; must not panic.
    let mailto = Uri("mailto:a@b.com")
    equal true mailto.IsAbsoluteUri
    equal "mailto" mailto.Scheme
    equal "mailto:a@b.com" mailto.AbsoluteUri
    let tel = Uri("tel:+1")
    equal true tel.IsAbsoluteUri
    equal "tel" tel.Scheme
    let urn = Uri("urn:x:y")
    equal true urn.IsAbsoluteUri
    equal "urn" urn.Scheme

[<Fact>]
let ``Uri relative resolution collapses dot segments`` () =
    let uri = Uri(Uri("http://h/a/b"), "../c")
    equal "http://h/c" uri.AbsoluteUri
    let uri2 = Uri(Uri("http://h/a/b/"), "../../x")
    equal "http://h/x" uri2.AbsoluteUri

[<Fact>]
let ``Uri.OriginalString works`` () =
    let cases = [
        "http://example.org"
        "http://example.org/"
        "HTTP://www.ConToso.com:80//thick%20and%20thin.htm"
        "http://www.test0.com/hello?a=b#c"
    ]
    for case in cases do
        let uri = Uri(case)
        uri.OriginalString |> equal case

[<Fact>]
let ``Uri.EscapeDataString works`` () =
    Uri.EscapeDataString("Hello World!") |> equal "Hello%20World%21"
    Uri.EscapeDataString("aA1-_.~") |> equal "aA1-_.~"
    // multi-byte chars percent-encode each UTF-8 byte
    Uri.EscapeDataString("ä") |> equal "%C3%A4"

[<Fact>]
let ``Uri.UnescapeDataString works`` () =
    Uri.UnescapeDataString("Hello%20World%21") |> equal "Hello World!"
    Uri.UnescapeDataString("%C3%A4") |> equal "ä"
    // an invalid escape sequence is left untouched
    Uri.UnescapeDataString("100%") |> equal "100%"

[<Fact>]
let ``Uri.EscapeUriString keeps reserved characters`` () =
    Uri.EscapeUriString("http://test.com/a b?c=d&e") |> equal "http://test.com/a%20b?c=d&e"
