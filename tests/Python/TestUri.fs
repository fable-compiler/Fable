module Fable.Tests.Uri

open System
open Util.Testing

[<Fact>]
let ``test Uri from absolute uri string works`` () =
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
let ``test Uri from absolute uri string with RelativeOrAbsolute works`` () =
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
let ``test Uri from relative uri string works`` () =
    let uri = Uri("/hello.html", UriKind.Relative)
    equal false uri.IsAbsoluteUri
    equal "/hello.html" (uri.ToString())

[<Fact>]
let ``test AbsoluteUri from relative uri should throw`` () =
    let uri = Uri("/hello.html", UriKind.Relative)
    Util.throwsError "This operation is not supported for a relative URI." (fun () -> uri.AbsoluteUri)

[<Fact>]
let ``test Uri from relative uri string without uri kind should throws`` () =
    let createInvalidUri () =
        Uri("hello.html")
    Util.throwsError "Invalid URI: The format of the URI could not be determined." createInvalidUri

[<Fact>]
let ``test Uri from relative uri with kind Absolute fails`` () =
    let createInvalidUri () =
        Uri("hello.html", UriKind.Absolute)

    Util.throwsError "Invalid URI: The format of the URI could not be determined." createInvalidUri

[<Fact>]
let ``test Uri from baseUri with relative string works`` () =
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
let ``test Uri from baseUri with relativeUri works`` () =
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
let ``test Uri from baseUri with absolute Uri works`` () =
    let absUri = Uri("http://www.example.com/", UriKind.Absolute)
    let uri = Uri(absUri, absUri)
    equal true uri.IsAbsoluteUri
    equal "http" uri.Scheme
    equal "www.example.com" uri.Host
    equal "http://www.example.com/" uri.AbsoluteUri
