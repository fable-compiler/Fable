# Fable Helpers Sample

This is an example of how to publish a [Fable](http://fable-compiler.github.io/) package to [npm](http://fable-compiler.github.io/).
The package includes both compiled F# and JavaScript code so it can be accessed from both platforms.

Note our project is a library consisting of a single method to print key-value
pairs in padded format (because padding is one of the most demanded features in
npm packages). We have compiled the JS code in the `js` folder. So, after installing
the npm package, we can reference it from JS as follows:

```js
var myLib = require("fable-helpers-sample/js/MyLib");

myLib.printPairsPadded(3, 12, [
    ["A", 234.45],
    ["B", 23458.0214],
]);
```

This is very useful for consumers of our package, but it may be even more interesting
to reference the library from another F#/Fable project so we can have autocompletion,
access to comments and static type checking. For that, we are also including the compiled
`dll` within the `bin` folder in the npm package. In Visual Studio or Xamarin Studio you
can just build the project or, if you're using [FAKE](http://fsharp.github.io/FAKE/), do something like:

```fsharp
#r "packages/FAKE/tools/FakeLib.dll"

open Fake

MSBuildDebug "bin" "Build" ["src/Fable.Helpers.Sample.fsproj"]
|> ignore
```

Now, from the consumer project (let's use an F# script for that) we can reference
the `dll` normally:

```fsharp
#r "node_modules/fable-helpers-sample/bin/Fable.Helpers.Sample.dll"

open Fable.Helpers.Sample

[ ("A", 235.65); ("XXX", 12304294.) ]
|> MyLib.printPairsPadded 4 10
```

The tricky part is, when compiling to JS, how to tell Fable to replace
the `dll` reference with references to the JS files. This is the purpose
of the [`--refs` compiler parameter](http://fable-compiler.github.io/docs/compiling.html#Project-references).
This parameter correlates the name of the referenced project (usually the name
of the `dll` without the extension) and the path where the compiled JS files are.
This way, we could compile our consumer script file like this: 

```bash
fable MyScript.fsx --refs Fable.Helpers.Sample=fable-helpers-sample/js
```

When using [fableconfig.json](http://fable-compiler.github.io/docs/compiling.html#fableconfig-json), add this to the JSON config instead:

```json
"refs": {
    "Fable.Helpers.Sample": "fable-helpers-sample/js"
}
```

> If you're not using node or a module bundler like Webpack, you need to
make the JS import path explicit, like `./node_modules/fable-helpers-sample/js`.

> Dlls don't keep the project file name. This means Fables needs to make
a guess about what's the base directory of your project files. To prevent conflicts,
try to keep a standard file structure and put the `.fsproj` file on the base
directory of your project files, as in the example.