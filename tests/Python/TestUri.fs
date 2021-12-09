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
