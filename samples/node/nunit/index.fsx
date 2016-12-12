(**
 - title: Using NUnit with Fable
 - tagline: Write unit tests compatible with both .NET and JS

This tutorial shows how to compile NUnit tests so they can be run in JS with [Mocha](https://mochajs.org).
[Fable's own tests](https://github.com/fable-compiler/Fable/tree/master/src/tests) are written using this method.
However, please note this is not a core feature of Fable as it comes through a [plugin](http://fable-compiler.github.io/docs/plugins.html).
You can view the [source code](https://github.com/fable-compiler/Fable/blob/master/samples/node/nunit/index.fsx),
[package.json](https://github.com/fable-compiler/Fable/blob/master/samples/node/nunit/package.json) and
[fableconfig.json](https://github.com/fable-compiler/Fable/blob/master/samples/node/nunit/fableconfig.json) on
GitHub. This page shows the full source code of the demo.

## Configuring Fable and packages

Fable projects usually include a [package.json](https://docs.npmjs.com/files/package.json) and a [fableconfig.json](http://fable-compiler.github.io/docs/compiling.html#fableconfig-json) files.
Let's have a look at the first one:

    [lang=js]
    {
      "private": true,
      "dependencies": {
        "fable-core": "^0.7.14",
        "fable-powerpack": "^0.0.18",
        "isomorphic-fetch": "^2.2.1"
      },
      "devDependencies": {
        "fable-plugins-nunit": "^0.7.2",
        "mocha": "^2.5.3"
      },
      "scripts": {
        "test": "mocha out"
      }
    }

There're several interesting things going on here:

- First we install our **npm dependencies**: `fable-core` which is necessary for all Fable projects,
  `fable-powerpack` which contains utilities like a helper for Fetch API and `isomorphic-fetch` in order
  to use Fetch API on node.
- Then we install the **development dependencies**: If we were to distribute this package, these dependencies
  wouldn't be installed on the machine of the final consumers as they are only necessary for development.
  `fable-plugins-nunit` will extend Fable's capabilities to allow NUnit tests compilation and `mocha` is
  our test runner on JS.
- We define a **npm script**: So tests will be run when calling `npm test` (or `npm run-script test`).
  The script is as simple as calling `mocha` and passing the directory where the tests compiled to JS are to be found.

Let's check now `fableconfig.json`:

    [lang=js]
    {
      "projFile": "./index.fsx",
      "sourceMaps": true,
      "module": "commonjs",
      "outDir": "out",
      "plugins": "./node_modules/fable-plugins-nunit/Fable.Plugins.NUnit.dll",
      "scripts": {
          "prebuild": "npm install",
          "postbuild": "npm test"
      }
    }

As in other samples, we specify the JS `module` system to target, the F# `projFile`
and the `outDir` where to put the compiled JS files. But the interesting part here is
we pass `Fable.Plugins.NUnit.dll` through the `plugins` parameter (which can also be an array)
and with `scripts.postbuild` we make Fable run `npm test` (defined in package.json above)
after building the project.

## Referencing Fable and dependencies

As this is a simple F# script, we make a reference to `Fable.Core.dll` with the `#r` directive
and open the appropriate namespaces.

*)

#r "../../node_modules/fable-core/Fable.Core.dll"
#r "../../node_modules/fable-powerpack/Fable.PowerPack.dll"

open System
open Fable.Core
open Fable.Import
open Fable.PowerPack

(**
The demo uses the [Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API) and,
as we'll be running the tests on node, we have to polyfill it using the `isomorphic-fetch` package.
*)

JsInterop.importAll "isomorphic-fetch"

(**
Here we're using the testing methods and attributes from `Fable.Core`. If you're referencing
`NUnit` library instead, just comment out the next line and adapt the two lines below to
the path where `NUnit` can be found on your machine.
*)

open Fable.Core.Testing

// #r "../../../packages/NUnit/lib/nunit.framework.dll"
// open NUnit.Framework

(**
Now we can write tests as we would do with NUnit. The most commonly used attributes
(`TestFixture` and `Test`) and their respective `SetUp`/`TearDown` counterparts are
implemented. For assertions, however, only `Assert.AreEqual` is available. But more
features will be available soon.
*)

[<TestFixture>]
module MyTests =

  // Convenience method
  let equal (expected: 'T) (actual: 'T) =
      Assert.AreEqual(expected, actual)

  [<Test>]
  let ``Structural comparison with arrays works``() =
    let xs1 = [| 1; 2; 3 |]
    let xs2 = [| 1; 2; 3 |]
    let xs3 = [| 1; 2; 4 |]
    equal xs1 xs2
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

  [<Test>]
  let ``Set.intersectMany works``() =
      let xs = set [1; 2]
      let ys = Set.singleton 2
      let zs = set [2; 3]
      let ks = Set.intersectMany [xs; ys; zs]
      (ks.Contains 2 && not(ks.Contains 1 || ks.Contains 3))
      |> equal true

(**
With some limitations, it's also possible to write asynchronous tests. For this,
you just need to **wrap the whole test** with `async { ... } |> Async.RunSynchronously`.
*)

  [<Test>]
  let ``Async.Parallel works``() =
    async {
        let getWebPageLength url =
            promise {
                let! res = Fetch.fetch url []
                let! txt = res.text()
                return txt.Length
            }
        let! results =
          [ "http://fable-compiler.github.io"
            "http://babeljs.io"
            "http://fsharp.org" ]
          |> List.map (getWebPageLength >> Async.AwaitPromise)
          |> Async.Parallel
        // The sum of lenghts of all web pages is
        // expected to be bigger than 100 characters
        (Array.sum results) > 100 |> equal true
    } |> Async.RunSynchronously

(**
> Note: Besides the tests, `Async.RunSynchronously` is not compatible with Fable as
asynchronous operations are not allowed to lock the main thread in JS.
*)