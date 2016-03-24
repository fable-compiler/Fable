# Compiling to JS

You can install the `fable-compiler` package from [npm](https://www.npmjs.com/package/fable-compiler)
either locally or globally. Here we'll assume it's been installed globally so the `fable`
command is available from any directory.

The simplest way to compile a F# project (`.fsproj`) or script (`.fsx`) is
to pass its path as an argument to `fable`:

```shell
npm install -g fable-compiler

fable paht/to/your/project.fsproj
```

## CLI options

Besides the default argument (`--projFile`), the following options are available:

```text
  --symbols           F# symbols for conditional compilation, like 'DEBUG'
                      (will be added to 'DefineConstants' in .fsproj).
  --env               'browser' for amd modules and 'node' for commonjs(defaults to umd).
  -m, --sourceMaps    Generate source maps: [true|inline|false]
  -w, --watch         Recompile project much faster on file modifications.
  --plugins           Paths to Fable plugins.
  --babelPlugins      Additional Babel plugins (without 'babel-plugin-' prefix, like
                      'add-module-exports'). Must be installed in the current directory.
  --code              Pass a string of code directly to Fable.
  --outDir            Where to put compiled JS files. Defaults to project directory.
  --lib               Where to find the core library. If not set, fable-core.js
                      will be copied automatically to outDir.
  -t, --target        Use options of a specific target in fableconfig.json.
  -d, --debug         Shortcut for '--target debug'.
  -p, --production    Shortcut for '--target production'.
  -h, --help          Display this usage guide.
```

## fableconfig.json

Rather than passing all the options to the CLI, it may be more convenient to put them
in JSON format in a file named `fableconfig.json` within the current directory and let the
compiler read them for you. You can combine options from the CLI and `fableconfig.json`:
when this happens the former will have preference.

There are some options exclusive to `fableconfig.json`.

* **scripts**: Commands that should be executed during specific phases of compilation.
  Currently only `prebuild` and `postbuild` are accepted. For example, if you want to
  run tests defined in the npm `package.json` file you can write. 

```json
{
    "scripts": {
        "postbuild": "npm run test"
    }
}
```

* **targets**: You can group different options in targets. If you don't want,
  say, source maps when deploying for production, you can use a config file as
  seen below. When a target is specified, the options in the target will
  override the defualt ones. Activate the target by passing it to the CLI:
  `fable --target production`.


```json
{
    "sourceMaps": true,
    "targets": {
        "production": {
            "sourceMaps": false
        }
    }
}
```

When using a node `package.json` file, it's also possible to specify the minimum
version of Fable required to compile the project.

```json
{
    "engines": {
        "fable": "0.1.3"
    }
}
```


## Polyfill

After going through Babel pipeline, the code won't include any syntax foreign
to ES5. However several ES6 classes (like `Symbol`) are used so it's advisable
to include a polyfill like [core-js](https://github.com/zloirock/core-js) to
make sure the code works fine in any browser.

You can include the polyfill in a `script` tag in your HTML file before loading
the generated JS code like:

```html
<script src="node_modules/core-js/client/core.min.js"></script>
```

Or you can require it directly in your F# code if you're using a bundler like
Webpack or Browserify.

```fsharp
Node.Globals.require.Invoke("core-js") |> ignore
```

> Babel includes [its own polyfill](http://babeljs.io/docs/usage/polyfill/)
with a lazy-sequence generator, but this is not needed as one is already included
in [fable-core.js](/src/fable-js/fable-core.js).

> The polyfill is not necessary when targeting node 4.4 or above.

## Modules

The compiler will keep the file structure of the F# project, wrapping each file in a [ES6 module](https://github.com/lukehoban/es6features#modules).
As these modules are not yet widely supported, they will be transformed again by Babel
to [amd](http://requirejs.org/docs/whyamd.html), [commonjs](https://nodejs.org/docs/latest/api/modules.html) or [umd](https://github.com/umdjs/umd)
according to the `env` argument (see above). In the browser, when not using a bundler
like Webpack or Browserify, you'll need a module loader like [require.js](http://requirejs.org) to start up the app.

When a F# file makes a reference to another, the compiler will create an [import statement](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import)
in the generated Javascript code. You can also generate imports by using
the [Import attribute](interacting.md).

As JS must import external modules with an alias, there's no risk of namespace
collision so, for convenience, the compiler will use the minimum route to access
external objects. Meaning that if you have a F# file with one root module:

```fsharp
module MyNs1.MyNs2.MyModule

let myProperty = "Hello"
```

To access `myProperty` the generated code will import the file with an alias, say `$M1`,
and directly access the property from it: `$M1.myProperty`. The route has been eluded
as it's not necessary to prevent name conflicts. In the same way, if you have a file
with two modules:

```fsharp
namespace MyNs1.MyNs2

module MyModule1 =
    let myProperty = "Hello"
    
module MyModule2 =
    let myProperty = "Bye"
```

This time the compiler will omit the namespace but keep the F# module names,
as they're necessary to prevent name conflicts in the same file:

```js
$M1.MyModule1.myProperty !== $M1.MyModule2.myProperty
```

The generated modules are exported using the default export and should be imported accordingly:

```js
import $M1 from "another/file"              // ES6
var $M1 = require("another/file").default   // CommonJS
```

If you need to export the modules as namespaces instead you can use the [add-module-exports](https://www.npmjs.com/package/babel-plugin-add-module-exports) plugin:

```shell
npm install babel-plugin-add-module-exports --save-dev
fable MyProject.fsproj --babelPlugins add-module-exports
```

## Debugging

You can debug the generated JS code normally. Also, if you pass the `sourceMaps`
option to the compiler, it'll be possible to debug the F# code (with some limitations).
This is automatic for browser apps. For node, you'll need a tool like [node-inspector](https://github.com/node-inspector/node-inspector)
or a capable IDE. In the case of Visual Studio Code, you can find instructions [here](https://code.visualstudio.com/docs/editor/debugging)
(see Node Debugging > JavaScript Source Maps). 

## Testing

You can use any JS testing library to write tests for your project, but to make it
easier to share code across platforms, a [plugin](plugins.md) is available to make
[NUnit](http://www.nunit.org) tests compatible with [Mocha](https://mochajs.org)
and this is what Fable uses for its own tests. The tests are compiled and run
automatically when building the project:

```shell
build.cmd   // on windows
./build.sh  // on unix
```

> Note: For now only `TestFixture` and `Test` attributes, and `Assert.AreEqual` are available, but more features will be available soon.

> Note: As attributes are only read by name, it's possible to use custom-defined attributes without the `NUnit` dependency if needed.

## Samples

There are some samples available in the [repository](/samples) and you can also download them from [here](https://ci.appveyor.com/api/projects/alfonsogarciacaro/fable/artifacts/samples.zip).
Every sample includes a `fableconfig.json` file so they can be compiled just by running
the `fable` command in the sample directory. Just be sure to install the npm dependencies
the first time.

```shell
npm install
fable
```

Now it's your turn to build a great app with Fable and show it to the world!
Check [Compatibility](compatibility.md) and [Interacting with JavaScript](interacting.md)
to learn what you need to take into account when diving into JS.
