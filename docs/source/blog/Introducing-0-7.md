 - tagline: Powered by Community™!

# Introducing Fable 0.7

So finally here it is! It took a bit longer than expected but I'm very happy to announce Fable 0.7 has been released to npm. This version includes lots of improvements thanks to the feedback from the community, it's almost a full rewrite and could be considered 1.0 (in fact, 1.0 will probably be this with minor upgrades). My experience when developing 0.7 has been completely different than in 0.1 times. Then I was coding alone in the room and this time... well, I'm still coding alone in my room but on the other side of the internet the many faces of the community have been tremendously helping me with PRs, clever suggestions and thorough testing.

Fable 0.7 has been in alpha status for several weeks and during this time it's gone through multiple iterations: for several of the new features different alternatives were implemented until picking the one that worked best for all. This means you can confidently write your apps in Fable without unexpected breaking changes (because most of them already happened in the alpha cycle). That said, there will be a short transition period until all the documentation and samples are updated so please make sure to check the list below and come to our [Gitter channel](https://gitter.im/fable-compiler/Fable) if you've questions.

Let's go with the list of things to have in mind when upgrading your app to Fable 0.7!

## Projects

Fable uses F# Compiler Services (FCS) which until now relied on MSBuild 2013 being installed on the system to parse `.fsproj` files and resolve references. This has caused problems for many users so now, thanks to the work of [Don Syme](https://github.com/dsyme) on FCS and [Jared Hester](https://github.com/cloudRoutine) and [Krzysztof Cieślak](https://github.com/Krzysztof-Cieslak) on [Forge](https://github.com/fsharp-editing/Forge), Fable can parse F# projects on its own. What's more, it's now possible to pass multiple project files (either `.fsproj` or `.fsx`) into `--projFile`. This is useful when you work with a solution in VS ([example](https://github.com/fable-compiler/Fable/blob/286523fc80246a6b593df1771ef40ec9a48aca80/src/tests/fableconfig.json)).

> **Caveats**: Like source files, project files must be in **compilation order**. Also, Forge is not as powerful as MSBuild (e.g. conditional items won't work), so please keep your `.fsproj` simple.

## References

One of biggest Fable drawbacks so far has been the difficulty to creat Fable libraries: sometimes it was necessary to distribute source files, other you could create a `.dll` but compiling and referencing it was rather convoluted. This has prevented the growth of Fable ecosystem, so it was a priority fix for 0.7.

Now there's a `--dll` compiler flag that, when activated, will make Fable generate a `.dll` assembly for your project together with a `.fablemap` file containing the mapping of the F# types in the assembly to the compiled JS files. Customer projects will only have to reference this `.dll` and Fable will automatically recognize the `.fablemap` file and generate the appropriate JS imports. [Fable.PowerPack](https://github.com/fable-compiler/fable-powerpack) is an example of such a library.

> **Caveats**: Remember to distribute the `.dll` assembly together with the `.fablemap` file and the generated JS files (keeping the same structure as in `outDir`). There's already work going on to link npm packages directly from `.fsproj` so please keep the convention of using the assembly name for your npm package with snake case (e.g. `Fable.Elmish` becomes `fable-elmish`). Also, see below how to include several distros in your package.

## Generic Resolution

In order to output standard and performant JS code, Fable is not including generic information in the runtime. However this information is still necessary in some cases to properly resolve calls. The way to solve this conundrum so far was to inline code, but this is not always desirable and Fable can neither inline if it has no access to the source code.

Several ways to fix the issue were tested until `PassGenericsAttribute` was added to `Fable.Core`. Now, if you need generic type info at runtime, just decorate a function with this attribute and Fable and you will be able to do `typeof<'T>` within its body ([example](https://github.com/fable-compiler/Fable/blob/286523fc80246a6b593df1771ef40ec9a48aca80/src/tests/Main/ReflectionTests.fs#L42-L52)). This way you can still write advanced generic code in Fable without having to pollute your whole code base or affecting compatibility with external JS code. The best of the two worlds!

> **Caveat**: Functions decorated with `PassGenericsAttribute` may work unexpectedly if called from external JS code.

## ES2015 Modules and Bundling

Bundling is often the only sane way to manage dependencies for browser JS apps and so far this has been accomplished by [Webpack](https://webpack.github.io/) but, as flexible and comprehensive as this tool is, it still has two problems: 1) it can be somewhat complex to configure and 2) doesn't understand (yet) [ES2015 modules](http://www.2ality.com/2014/09/es6-modules-final.html).

The biggest advantage of ES2015 modules is they allow static analysis of the dependencies among files which makes it much easier for a bundler to discard unused code. Fable compiles F# code using ES2015 imports and exports (provided you've a single root module per file) which makes it a perfect fit for such a bundler. Furthermore, now [fable-core](https://www.npmjs.com/package/fable-core) too is completely modular so your apps will only pay for what they get: not only the bundle will skip unused modules (say `Async` if you only use native JS `Promise`) but also any function from imported modules that you don't actually need.

Thanks to [Pauan](https://github.com/Pauan) (who's also been tremendously helping 0.7 development with many wonderful suggestions) and Rollup friends, Fable 0.7 comes with [Rollup](http://rollupjs.org/) embedded, a lightweight bundler that can understand ES2015 modules. Now bundling your app is as simple as passing the `--rollup` flag to the compiler. In `fableconfig` it's also possible to pass a configuration object instead corresponding to [Rollup JS API](https://github.com/rollup/rollup/wiki/JavaScript-API) options. The only difference is plugins, which must be passed differently as fableconfig cannot contain code. The same format as with Babel plugins is used (See "Plugin/Presets Options" [here](https://babeljs.io/docs/plugins/)). These settings are equivalent:

```js
rollup({
  plugins: [
    commonjs({ namedExports: {
        "virtual-dom": [ "h", "create", "diff", "patch" ]
      } })
  ]
});
```

```json
"rollup": {
  "plugins": [
    ["commonjs", {
      "namedExports": {
        "virtual-dom": [ "h", "create", "diff", "patch" ]
      }
    }]
  ]
}
```

The default Rollup configuration in Fable is as follows (any option in fableconfig will override them, including plugins):

```js
{
    // entry: <Last F# source file>
    dest: fableOptions.outDir + "/bundle.js",
    format: fableOptions.module || "iife",
    sourceMap: fableOptions.sourceMaps,
    moduleName: normalizeProjectName(fableOptions),
    plugins: {
        require('rollup-plugin-node-resolve')({ ignoreGlobal: true }),
        require('rollup-plugin-commonjs')({ jsnext: true, main: true, browser: true })
    }
}
```

If you're writing a Node app or using [RequireJS](http://requirejs.org/) and **don't want to bundle**, just use the UMD distribution of `fable-core`. The `--refs` compiler argument gives you a chance to replace the base directory of imported JS files when referencing assemblies ([example](https://github.com/fable-compiler/Fable/blob/85fb471ecc77b6d7296c5a1513e89e95f44c225b/samples/browser/react-todomvc/fableconfig.json)):

```json
{
    "refs": {
        "Fable.Core": "fable-core/umd"
    }
}
```

Because of this, it's recommended that Fable libraries include two distributions: the default one with ES2015 modules, and another one in the `umd` subdirectory with UMD modules. Just create an `umd` target for your library and compile it as well before publishing ([example](https://github.com/fable-compiler/fable-powerpack/blob/master/fableconfig.json)).

> **Caveat**: Though Rollup can understand `commonjs` modules, it's not as powerful as Webpack and can have problems bundling some packages (notably [React](https://facebook.github.io/react/)). In this case you may want to load the JS library globally with a `<script>` tag ([example](https://github.com/fable-compiler/fable-elmish/blob/e34232b8fbbf979fd800d9c24a386c2a2ffda966/samples/react/counter/fableconfig.json)) or with RequireJS ([example](https://github.com/fable-compiler/Fable/blob/85fb471ecc77b6d7296c5a1513e89e95f44c225b/samples/browser/react-todomvc/index.html#L25-L33)). Of course, it's still possible to skip Rollup bundling and use Webpack as before.

## JSON Serialization

It was actually [David Teasdale](https://github.com/davidtme) who started work on 0.7 with a PR to remove the need of the `$type` file when serializing and deserializing. This has led to the development of a reflection system for Fable, which can provide type information at runtime to _inflate_ JSON strings into proper instances of your F# types with all the semantics: structural equality (if applicable), access to instance and static members as well as the inheritance chain, etc. Also, thanks to [Dave Thomas](https://github.com/7sharp9)' suggestion, Fable compresses serialized union types like [FSharpLu](https://github.com/Microsoft/fsharplu/wiki/fsharplu.json) resulting in much smaller JSON payloads. To interact with [Json.NET](http://www.newtonsoft.com/json) on the server side, just use the Fable.JsonConverter available as a [Nuget package](https://www.nuget.org/packages/Fable.JsonConverter).

> Reflection info is only produced on demand so your types won't have any overhead compared to normal JS objects.

If you still want to use the old method (e.g. when you don't know the concrete type you will receive), please call the `toJsonWithTypeInfo` and `ofJsonWithTypeInfo` functions. You can then interact with Json.NET using the `TypeNameHandling.All` option, but please remember Json.NET is not adding type info to F# union types so you will need [the following converter](https://github.com/fable-compiler/Fable/blob/master/src/tools/UnionTypeInfoConverter.fsx).

> **Caveat**: The JSON is not validated as there's no Reflection API for Fable yet. Coming soon!

<br />

There are still many new features in Fable 0.7, here I'm only listing the most important points for the migration. Again, please be understanding while the documentation and samples are being updated. The community is already making wonderful with Fable like the [Gamma Project](http://rio2016.thegamma.net/) by Tomas Petricek, [Ionide](http://ionide.io/) by Krzysztof Cieślak, [fable-elmish](https://github.com/fable-compiler/fable-elmish) by Eugene Tolmachev and [fable-arch](http://fable.io/fable-arch/) by Tomas Jansson and Maxime Mangel, as well as Steffen Forkmann and François Nicaise work on React Native and Pixi.js respectively; and I'm sure there will be many other great projects coming with Fable 0.7. There's already work going on to improve editor support and Enrico Sada is bringing a surprise for the new `.fsproj` format, so there are exciting times ahead for Fable. Keep tuned!
