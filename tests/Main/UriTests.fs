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

    testCase "Uri from absolute uri string with RelativeOrAbsolute works" <| fun _ ->
        let uri = Uri("http://www.test1.com/hello?a=b#c", UriKind.RelativeOrAbsolute)
        equal true uri.IsAbsoluteUri
        equal "http" uri.Scheme
        equal "www.test1.com" uri.Host
        equal "/hello" uri.AbsolutePath
        equal "/hello?a=b" uri.PathAndQuery
        equal "?a=b" uri.Query
        equal "#c" uri.Fragment

    testCase "Uri from relative uri string works" <| fun _ ->
        let uri = Uri("/hello.html", UriKind.Relative)
        equal false uri.IsAbsoluteUri

    testCase "Uri from relative uri string without uri kind should throws" <| fun _ ->
        let createInvalidUri () =
            Uri("hello.html")

        Util.throwsError "Invalid URI: The format of the URI could not be determined." createInvalidUri

    testCase "Uri from baseUri with relative string works" <| fun _ ->
        let uri = Uri("http://www.test2.com/")
        let newUri = Uri(uri, "/hello?a=b#c")
        equal true newUri.IsAbsoluteUri
        equal "http" newUri.Scheme
        equal "www.test2.com" newUri.Host
        equal "/hello" newUri.AbsolutePath
        equal "/hello?a=b" newUri.PathAndQuery
        equal "?a=b" newUri.Query
        equal "#c" newUri.Fragment

    testCase "Uri from baseUri with relativeUri works" <| fun _ ->
        let uri = Uri("http://www.test3.com/")
        let relativeUri = Uri("/hello?a=b#c", UriKind.Relative)
        let newUri = Uri(uri, relativeUri)
        equal true newUri.IsAbsoluteUri
        equal "http" newUri.Scheme
        equal "www.test3.com" newUri.Host
        equal "/hello" newUri.AbsolutePath
        equal "/hello?a=b" newUri.PathAndQuery
        equal "?a=b" newUri.Query
        equal "#c" newUri.Fragment
  ]
