# Compiling to JS

A node script is used to trigger the compilation from F# to JS.
To execute it, make sure you've installed the dependencies with
`npm install` and from a terminal on the project root folder, type:

```
node tools/fable2babel.js --projFile paht/to/YourProject.fsproj
```

> You can also pass a F# script file (`.fsx`) instead.


## Project options

Besides `--projFile`, the following options are available:

```
    --code      If you want to pass directly a string of code instead.
                The result will be displayed on-screen.

    --outDir    Where do you want to put the compiled JS files.

    --symbols   Array of symbols passed to the F# compiler for
                conditional compilation.

    --lib       Where the compiler can find external JS libraries,
                mainly used to locate fable-core.js.

    --env       'browser' for amd modules and 'node' for commonjs.
                If not specified, it will default to umd.

    --watch     Key-only parameter. The first compilation will take
                some seconds to warm up but setting this option files
                modified later will be compiled much faster.
```

However, it's usually more convenient to put them in JSON format in a file
named `fableconfig.json` in the same folder as the F# project and let the
compiler read them automatically for you.


## Polyfill

After going through Babel pipeline, the code won't include any syntax foreign
to ES5. However several ES6 classes (like `Symbol`) are used so it's advisable
to include a polyfill like [core-js](https://github.com/zloirock/core-js) to
make sure the code works fine in any browser. Babel includes [its own polyfill](http://babeljs.io/docs/usage/polyfill/)
with a lazy-sequence generator, but this is not needed as one is already included
in [fable-core.js](/lib/fable-core.js).


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

## Samples

Some samples are included in the repo and more will be coming soon. To compile
and run a sample follow these steps:

### Node samples

Let's say you want to build the node static server. From the project root folder, type:

```
cd sample/node/server
npm install  // Install dependencies the first time
cd ../../..
node tools/fable2babel.js --projFile sample/node/server/app.fsx
```

> Note there's a `fableconfig.json` file next to the F# file with the options
  for Fable compiler.
  
You can now run the compiled file as any other node script. In this case, you'll
get a static server running locally at the specific port (or 8080 if no argument
is passed).

```
node sample/node/server/app.js 8090
```

### Browser samples

This time we'll compile the todomvc sample with [Vue](http://vuejs.org).
Again, from the project root folder type:

 ```
cd sample/browser/todomvc
npm install  // Only first time
cd ../../..
node tools/fable2babel.js --projFile sample/browser/todomvc/app.fsx
```

To start the web app we need a server so the one we built above comes
in handy. Type again:

```
node sample/node/server/app.js 8090
```

and browse to:

```
http://localhost:8090/sample/browser/todomvc
```

> Note: if you don't run the server from the root folder, `fable-core.js`
  won't be found.
  
Now it's your turn the build your own sample and show it to the world!
Check [Compatibility](compatibility.md) and [Interacting with JavaScript](interacting.md)
to learn what you need to take into account when diving into JS.
