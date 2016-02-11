# Compiling to JS

To trigger the `F# > JS` compilation a node script is used. To run it, from a terminal on the project root folder, type:

```
npm install     // Just the first time, install npm dependencies

node tools/fabel2babel.js --projFile paht/to/YourProject.fsproj
```

> You can also pass a F# script file `.fsx` instead.


## Project options

Besides `--projFile`, the following options are available:

```
    --code      If you want to pass directly a string of code instead.
                The result will be displayed on-screen.
    --outDir    Where do you want to put the compiled JS files.
    --symbols   Array of symbols passed to the F# compiler for
                conditional compilation.
    --lib       Where the compiler can find external JS libraries,
                mainly used to locate fabel-core.js.
    --env       'browser' for amd modules and 'node' for commonjs,
                if not specified, will default to umd.
```

However, it's usually more convenient to put them in JSON format in a file named `fabelconfig.json` in the same folder as the F# project and let the compiler read them automatically for you.


## Polyfill

After going through Babel pipeline, the code won't include any syntax foreign to ES5. However several ES6 classes (like `Symbol`) are used so it's advisable to include a polyfill like [core-js](https://github.com/zloirock/core-js) to make sure the code works fine in any browser. Babel includes [its own polyfill](http://babeljs.io/docs/usage/polyfill/) with a lazy-sequence generator, but this is not needed as one is already included in [fabel-core.js](/lib/fabel-core.js).


## Modules

The compiler will keep the file structure of the F# project, wrapping each file in a [ES6 module](https://github.com/lukehoban/es6features#modules). As these modules are not yet widely supported, they will be transformed again by Babel to [amd](http://requirejs.org/docs/whyamd.html), [commonjs](https://nodejs.org/docs/latest/api/modules.html) or [umd](https://github.com/umdjs/umd) according to the `env` argument (see above). In the browser, you'll need a module loader like [require.js](http://requirejs.org) to start up the app.

When a F# file makes a reference to another, the compiler will create an import in the generated Javascript code. You can also generate imports by yourself by using the [`Import` attribute](interacting.md).

As JS must import external modules with an alias, there's no risk of namespace collision so, for convenience, the compiler will use the minimum route to access the external objects. Meaning that if you have a F# file with one root module:

```
module MyNs1.MyNs2.MyModule

let myProperty = "Hello"
```

To access `myProperty` the generated code will import the file with alias, say, `$M1` and directly access the property from it: `$M1.myProperty`. The route has been eluded as it's not necessary to prevent name conflicts. In the same way, if you have a file with two modules:

```
namespace MyNs1.MyNs2

module MyModule1 =
    let myProperty = "Hello"
    
module MyModule2 =
    let myProperty = "Bye"
```

This time the compiler will omit the namespace but keep the F# module names, as they're necessary to prevent name conflicts in the same file:

```
$M1.MyModule1.myProperty !== $M1.MyModule2.myProperty
```

> The generated modules are exported as default. So the imports will have the form `import $M1 from "another/file"`

> To know more about the transformations in the compilation process, see [Semantics](semantics.md).