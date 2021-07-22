module Fable.Tests.Uri

open System
open Util.Testing
open Fable.Tests

let tests =
  testList "Uri" [
    testCase "Uri from absolute uri string works" <| fun _ ->
        let uri = Uri("http://www.test0.com/hello?a=b#c")
        equal true uri.IsAbsoluteUri
        equal "http" uri.Scheme
        equal "www.test0.com" uri.Host
        equal "/hello" uri.AbsolutePath
        equal "/hello?a=b" uri.PathAndQuery
        equal "?a=b" uri.Query
        equal "#c" uri.Fragment
        equal "http://www.test0.com/hello?a=b#c" uri.AbsoluteUri

    testCase "Uri from absolute uri string with RelativeOrAbsolute works" <| fun _ ->
        let uri = Uri("http://www.test1.com/hello?a=b#c", UriKind.RelativeOrAbsolute)
        equal true uri.IsAbsoluteUri
        equal "http" uri.Scheme
        equal "www.test1.com" uri.Host
        equal "/hello" uri.AbsolutePath
        equal "/hello?a=b" uri.PathAndQuery
        equal "?a=b" uri.Query
        equal "#c" uri.Fragment
        equal "http://www.test1.com/hello?a=b#c" uri.AbsoluteUri

    testCase "Uri from relative uri string works" <| fun _ ->
        let uri = Uri("/hello.html", UriKind.Relative)
        equal false uri.IsAbsoluteUri
        equal "/hello.html" (uri.ToString())

    testCase "AbsoluteUri from relative uri should throw" <| fun _ ->
        let uri = Uri("/hello.html", UriKind.Relative)
        Util.throwsError "This operation is not supported for a relative URI." (fun () -> uri.AbsoluteUri)

    testCase "Uri from relative uri string without uri kind should throws" <| fun _ ->
        let createInvalidUri () =
            Uri("hello.html")

        Util.throwsError "Invalid URI: The format of the URI could not be determined." createInvalidUri

    testCase "Uri from relative uri with leading forward slash as kind Absolute works with file:// protocol" <| fun _ ->
        let uri = Uri("/test", UriKind.Absolute)
        equal "file" uri.Scheme
        equal "" uri.Host
        equal "/test" uri.AbsolutePath
        equal "/test" uri.PathAndQuery
        equal "file:///test" uri.AbsoluteUri

    testCase "Uri from relative uri with two leading forward slashes as kind Absolute works with file:// protocol" <| fun _ ->
        let uri = Uri("//test", UriKind.Absolute)
        equal "file" uri.Scheme
        equal "test" uri.Host
        equal "/" uri.AbsolutePath
        equal "/" uri.PathAndQuery
        equal "file://test/" uri.AbsoluteUri

    testCase "Uri from relative uri with three leading forward slashes as kind Absolute works with file:// protocol" <| fun _ ->
        let uri = Uri("///test", UriKind.Absolute)
        equal "file" uri.Scheme
        equal "test" uri.Host
        equal "/" uri.AbsolutePath
        equal "/" uri.PathAndQuery
        equal "file://test/" uri.AbsoluteUri

    testCase "Uri from baseUri with relative string works" <| fun _ ->
        let uri = Uri(Uri("http://www.test2.com/"), "/hello?a=b#c")
        equal true uri.IsAbsoluteUri
        equal "http" uri.Scheme
        equal "www.test2.com" uri.Host
        equal "/hello" uri.AbsolutePath
        equal "/hello?a=b" uri.PathAndQuery
        equal "?a=b" uri.Query
        equal "#c" uri.Fragment
        equal "http://www.test2.com/hello?a=b#c" uri.AbsoluteUri

    testCase "Uri from baseUri with relativeUri works" <| fun _ ->
        let uri = Uri(Uri("http://www.test3.com/"), Uri("/hello?a=b#c", UriKind.Relative))
        equal true uri.IsAbsoluteUri
        equal "http" uri.Scheme
        equal "www.test3.com" uri.Host
        equal "/hello" uri.AbsolutePath
        equal "/hello?a=b" uri.PathAndQuery
        equal "?a=b" uri.Query
        equal "#c" uri.Fragment
        equal "http://www.test3.com/hello?a=b#c" uri.AbsoluteUri

    testCase "Uri from baseUri with absolute Uri works" <| fun _ ->
        let absUri = Uri("http://www.example.com/", UriKind.Absolute)
        let uri = Uri(absUri, absUri)
        equal true uri.IsAbsoluteUri
        equal "http" uri.Scheme
        equal "www.example.com" uri.Host
        equal "http://www.example.com/" uri.AbsoluteUri

    testCase "TryCreate from absolute string with kind Absolute works" <| fun _ ->
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

    testCase "TryCreate from absolute string with kind RelativeOrAbsolute works" <| fun _ ->
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

    testCase "TryCreate from absolute string with kind Relative fails" <| fun _ ->
        let (valid, _) = Uri.TryCreate("http://www.test1.com/hello?a=b#c", UriKind.Relative)
        equal false valid

    testCase "TryCreate from relative string with kind Absolute works" <| fun _ ->
        let (valid, uri) = Uri.TryCreate("/hello.html", UriKind.Absolute)
        equal true valid
        equal true uri.IsAbsoluteUri
        equal "file" uri.Scheme
        equal "" uri.Host
        equal "/hello.html" uri.AbsolutePath
        equal "/hello.html" uri.PathAndQuery
        equal "" uri.Query
        equal "" uri.Fragment
        equal "file:///hello.html" uri.AbsoluteUri

    testCase "TryCreate from relative string with kind RelativeOrAbsolute works" <| fun _ ->
        let (valid, uri) = Uri.TryCreate("hello?a=b#c", UriKind.RelativeOrAbsolute)
        equal true valid
        equal "hello?a=b#c" (uri.ToString())

    testCase "TryCreate from relative string with kind Relative works" <| fun _ ->
        let (valid, uri) = Uri.TryCreate("hello?a=b#c", UriKind.Relative)
        equal true valid
        equal "hello?a=b#c" (uri.ToString())

    testCase "TryCreate from absolute uri and absolute uri should work" <| fun _ ->
        let absUri = Uri("https://example.com", UriKind.Absolute)
        let (valid, uri) = Uri.TryCreate(absUri, absUri)
        equal true valid
        equal absUri uri

    testCase "TryCreate from absolute uri and relative uri should work" <| fun _ ->
        let (valid, uri) = Uri.TryCreate(Uri("https://example.com", UriKind.Absolute), Uri("test", UriKind.Relative))
        equal true valid
        equal "https://example.com/test" (uri.ToString())

    testCase "TryCreate from relative uri and relative uri should fail" <| fun _ ->
        let relativeUri = Uri("test", UriKind.Relative)
        let (valid, _) = Uri.TryCreate(relativeUri, relativeUri)
        equal false valid

    testCase "TryCreate from relative uri and absolute uri should fail" <| fun _ ->
        let (valid, _) = Uri.TryCreate(Uri("test", UriKind.Relative), Uri("https://example.com", UriKind.Absolute))
        equal false valid

    testCase "Uri.ToString works" <| fun _ ->
        let uri = Uri("HTTP://www.test4.com:80/a%20b%20c.html")
        uri.ToString() |> equal "http://www.test4.com/a b c.html"
        sprintf "%A" uri |> equal "http://www.test4.com/a b c.html"
  ]
