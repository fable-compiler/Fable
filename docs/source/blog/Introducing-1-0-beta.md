 - tagline: Welcome dotnet SDK!

# Introducing Fable 1.0 beta (codename narumi)

One year after the release of fable-compiler 0.0.1 to npm, I'm very glad to announce the availability of Fable 1.0 beta and I'm also tremendously happy to see how during this time the project has become a very important part of the F# ecosystem and even for some the main reason to write F#. I realize Fable must live up to the high standards of the F# experience, because of this, the main efforts for this new major version have been put into improving the workflow and make it as easy as possible for app and library developers to author Fable programs. If the keyword of Fable 0.7 was _community_ this time we can say it is _integration_: keeping the original spirit of bringing together the best of F#/.NET and JS worlds, Fable 1.0 integrates now in the pipeline of the best build tools in both ecosystems.

> Disclaimer: This is the announcements of a **beta** release so please expect some rough edges. If you have a critical project in Fable you may want to wait for the stable version, but it would be really helpful if you could take the time to go through this document, upgrade your project and provide feedback to help make sure the final Fable 1.0 will work for everybody.

## Integration with dotnet SDK and the new .fsproj format

Fable 1.0 coincides in time with the release of the new [dotnet SDK](https://www.microsoft.com/net/download/core). Even if some are still skeptical (considering the time it's taken to get here I cannot blame them) I'm very confident the dotnet SDK will be a game changer for F# development. Thanks to tools like [Paket](https://fsprojects.github.io/Paket/), [FAKE](http://fsharp.github.io/FAKE/) or [Forge](http://forge.run/), it has been possible to write F# outside Visual Studio for quite a long time, but now for the first time the dotnet SDK provides a simple way out of the box to create, manage and build an F# project with just a few commands on a console. Thanks to the herculean work of [Enrico Sada](https://twitter.com/enricosada), who's been also leading the development of Fable 1.0, the dotnet SDK is fully compatible with F# and with Fable, as we will see below.

Besides tooling, an ugly duckling was making hard to sell F# as a viable alternative for frontend development: the project file. Everyone approaching the language (myself included) was impacted by the bloated XML file where you had to dig among many cryptic options to find where your files and references were listed. This made incredibly difficult to edit the project file by hand without the help of other tools. Luckily, and again thanks to Enrico, these days will soon be part of the past. Fable 1.0 projects will look like this:

```xml
<Project Sdk="FSharp.NET.Sdk;Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>netstandard1.6</TargetFramework>
  </PropertyGroup>
  <ItemGroup>
    <Compile Include="../Server/Shared/Domain.fs" />
    <Compile Include="Pages/Menu.fs" />
    <Compile Include="Pages/Login.fs" />
    <Compile Include="App.fs" />
  </ItemGroup>
  <ItemGroup>
    <ProjectReference Include="../node_modules/fable-powerpack/Fable.PowerPack.fsproj" />
    <PackageReference Include="FSharp.NET.Sdk" Version="1.0.*" PrivateAssets="All" />
    <PackageReference Include="FSharp.Core" Version="4.1.*" />
    <PackageReference Include="Fable.Core" Version="1.0.0-narumi-*" />
    <DotNetCliToolReference Include="dotnet-fable" Version="1.0.0-narumi-*" />
  </ItemGroup>
</Project>
```

Still XML, not a fancy modern markup language, but with a glance we can see now our list of files, references and CLI tools, that can be easily edited without fear to break anything in the build. Did I say CLI tools? Yes, in addition to project and [nuget](https://www.nuget.org/) references, the dotnet SDK accepts references for CLI tools. And this is how Fable will be distributed now, **no more npm's fable-compiler**. With a project file such as the one above after running `dotnet restore` we will be able to run Fable commands (from the project directory) prefixing them with `dotnet fable`. For example to start the Fable server you just need to type: `dotnet fable start`.

> At the moment, the only editor that supports the new .fsproj format is [Ionide](http://ionide.io/) so you may need to wait a bit until it's supported in other IDEs. In principle, Fable 1.0 should still be compatible with the old format, but this has not been tested as the focus will now be in the new .fsproj. If you're interested please give it a try and let me know how it goes.

But what is that Fable server? Keep on reading to find the answer!

## Integration with Webpack

Under the hood Fable compiler is a hybrid of .NET and a [node](https://nodejs.org/) app. So far this has been transparent to the users, but now the distinction is made obvious as the .NET part is distributed through nuget (see above) while the JS part becomes a [Webpack loader](https://webpack.js.org/concepts/loaders/) available through [npm](https://www.npmjs.com/package/fable-loader).

Webpack is a JS bundler with many powerful features that has become the standard build tool of choice for a vast majority of JS developers, though many (I among them) have been drawn back by its complexity. Luckily with Webpack 2 this is being addressed with a much better [documentation site](https://webpack.js.org) and great community support thanks to its widespread use. Tying Fable to Webpack may seem risky, but [most of the users are already using Webpack](https://twitter.com/FableCompiler/status/829372278878126080) anyways and the integration offers several advantages, like:

- Only one configuration file. You read well, **no more fableconfig.json**, the config options can be passed now to the Fable loader.
- Better interaction with other languages like JS or Typescript. Just add the necessary loaders and Webpack will take care of merging everything into a single bundle.
- Management of tasks like file watching. This frees development resources for Fable so we can focus on improving the code generation.

So with the Fable server mentioned above running, you only need to invoke Webpack with the appropriate configuration. Please check the Webpack documentation for more info. For convenience, you can also start the Fable server and Webpack with a single command using [npm scripts](https://docs.npmjs.com/misc/scripts). If you have a `build` script to invoke Webpack in your package.json, you can type: `dotnet fable npm-run build` to start and keep Fable server active only during the time the npm script is being executed.

> By default, Fable server will listen in port 61225. You can modify it by using the `--port` argument when starting the Fable server and also passing `port` to the Fable loader options.

## Libraries

With Fable 0.7 a system was put in place to distribute .dll assemblies together with precompiled JS files. This had some advantages and make the F# tooling (autocompletion, type checking...) faster. However distributing precompiled JS files has proven to be problematic, as final users of the library may want to generate JS code with different parameters (modules, polyfills...). To solve this problem, Fable 1.0 libraries will distribute the F# source (still as npm packages) and let the final user generate the JS code to their taste. Instead of .dll references, project references (which has become much simpler in the new .fsproj as there're no guids anymore) will be used. You can check the `fable-powerpack` project reference in the sample above. In general, to add a Fable package you'll only have to install it with npm and add the reference to your F# project:

```
npm install --save-dev fable-powerpack@next
dotnet add reference ./node_modules/fable-powerpack/Fable.PowerPack.fsproj
dotnet restore
```

To make things even simpler, the Fable CLI tool provides a command to do these three steps in one:

```
dotnet fable add fable-powerpack@next
```

> The command must be executed in the same directory as the F# project. The npm package.json can be in a parent folder (for example, if it's shared with other projects). Several packages can be passed at the same time.

Currently, there are Fable 1.0 prereleases for the following packages: fable-powerpack, fable-react, fable-elmish, fable-elmish-react and fable-elmish-snabbdom. Please install them using the `next` tag (e.g. `fable-elmish-react@next`).

Not directly related with Fable 1.0 but in a happy coincidence **fable-elmish and fable-arch have just joined forces** to avoid the most frequent confusion of Fable newcomers who never knew how to decide between the two libraries. Basically, from now on [fable-elmish](https://github.com/fable-compiler/fable-elmish) is the preferred choice, but there's also a [fable-elmish-snabbdom](https://github.com/fable-compiler/fable-elmish/tree/master/samples/snabbdom) package available to write apps with an Elm-like architecture and a renderer lighter than React. I'm sure this movement will be extremely beneficial for the Fable community!

## Changes in code generation

As you can see, most of the changes in Fable 1.0 have to do with the workflow, but there are still a few important changes in code generation. Most importantly, thanks to the outstanding accomplishment of [ncave](https://github.com/ncave) (who's ncave?) who has [compiled the F# compiler itself to JS using Fable](https://github.com/fsharp/FSharp.Compiler.Service/pull/688) enabling the [Fable REPL](http://fable.io/repl.html) to run entirely on the browser with no backend, we had the chance to analyze the generated JS code for a big, big, big project, fixing many subtle bugs that surfaced and introducing several optimizations.

### Curried lambdas are not compiled as nested functions anymore

Even if Fable is designed to make interaction with JS as seamless as possible, there was a pain point for all users when sending F# code to JS and vice versa: F# curried lambdas were being translated as nested functions. This was very confusing and force users to convert the lambdas into delegates (`System.Func`) before sending them to JS. Nested functions were needed to make possible all the composition patterns in F# (there's a discussion [here](https://github.com/fable-compiler/Fable/pull/335) if you're interested).

Fable 1.0 introduces a system wherein the compiler will detect whenever more or less arguments than expected are being applied to a function, and create a nested function ad-hoc if necessary. In cases where this guess becomes too difficult (for example, [three or more nested functions with side effects](https://goo.gl/tor4uu)) Fable will create a dynamic curried lambda that will check the number of applied arguments in runtime. This is a compromise similar to the one made with generics: make possible complicated F# patterns while using standard JS code for the most common situations.

Hopefully this change will help when interacting with external JS libraries and increase the performance of generated code. It may still be possible that some edge cases are not covered by this technique, if you find one, please [submit an issue](https://github.com/fable-compiler/Fable/issues/new). At the moment whole F# compiler is being translated to JS without problem, so that's a good sign :)

### `KeyValueList` attribute has been deprecated

So far, in other to make the interaction with JS more idiomatic in F# I've introduced some attributes to change the runtime representation of certain types. This is nice because you can have the types we know and love from F# like unions and records, while they behave as JS objects, strings, etc. in runtime. But on the other hand, this was also problematic because developers had some expectations about these types that behave differently in runtime.

In order to avoid these frictions, Fable 1.0 won't introduce more _hacky_ attributes and will explore other ways to improve the interaction with JS that don't break from standard F#. In order to keep compatibility old attributes are still supported with the exception of `KeyValueList` that was used to represent options in JS (by transforming a list of union cases into a JS object). This is because this attribute was mainly used in libraries (like `fable-react`) and it's been possible to change it without modifying the API. Moreover the new solution (a `keyValueList` function) can be inlined to make the transformation happen in real time and avoid any performance penalty in runtime.

## How do I start?

Wow, I started the post saying that Fable 1.0 would make things easier but there are too many new things to learn! Luckily one of the cool features of the dotnet SDK that's been also a long request for Fable users is the template system. Thanks to that you can get a simple working project with a few commands. First, if you haven't already, install the latest [dotnet SDK](https://www.microsoft.com/net/download/core) and then type:

```shell
dotnet new -i Fable.Template::*
```

You only need to do this the first time or when you want to update the template (`::*` points to the latest version). Then create a directory and invoke the Fable template.

```shell
mkdir myfableapp
cd myfableapp
dotnet new fable
```

It's better to avoid hyphens or periods in the directory name, as this can confuse the template system (you can add them later if needed). Now you only have to install npm and nuget dependencies:

```shell
dotnet restore
npm install
```

Now it depends on your project configuration, in the simple project created you can type `dotnet fable npm-run start` (see above) to start a web server that hot reloads on code changes (the npm `start` script in package.json just invokes the webpack dev server). Remember you can also use the `dotnet fable add` command to add Fable libraries. For example if you want to add `fable-elmish-react` you can type:

```shell
dotnet fable add fable-powerpack@next fable-elmish@next fable-elmish-react@next
```

> At the moment Fable cannot resolve transitive dependencies so it's necessary to list them all.

<br />

It's been a long way to get here, but I hope Fable 1.0 will meet most of the user needs and provide better stability for the project so we can focus on expanding the ecosystem, writing tutorials and such. Please give Fable 1.0 beta (codename `narumi`) a try and provide feedback either through the [Gitter channel](https://gitter.im/fable-compiler/Fable) or [Fable Github repo](https://github.com/fable-compiler/Fable/issues/new) to help make the final Fable 1.0 stable release as polished as possible. And don't forget to follow us [on Twitter](https://twitter.com/FableCompiler) to get all Fable news first-hand!
