 - tagline: Learn about how Fable works & how to use it

# Fable documentation

The following documentation pages are generated from the `docs` pages
in the Fable repository on GitHub. Is anything unclear or missing?
Help us make Fable better by contributing!

<div class="fb-docs">
<div class="row"><div class="col-sm-6">

### [<i class="fa fa-cog" aria-hidden="true"></i> Compiling to JavaScript](docs/compiling.html)

Start here! The page covers all you need to get started
with Fable. This includes the `fable` command parameters, `fableconfig.json` file,
using Fable through Polyfill, modules, debugging, testing and more.

</div><div class="col-sm-6">

### [<i class="fa fa-refresh" aria-hidden="true"></i> F# language and library compatibility](docs/compatibility.html)

Provides details about the compatibility. This page explains how are primitive
F# and CLR types compiled to JavaScript, which F# and CLR classes are supported
and how they are translated.

</div></div>
<div class="row"><div class="col-sm-6">

### [<i class="fa fa-globe" aria-hidden="true"></i> Interacting with JavaScript](docs/interacting.html)

How to call JavaScript libraries? With Fable, you can use the dynamic operator
and write `jsObject?foo`, you can use the `Emit` attribute, or define a foreign
interface. There are a couple of special attributes too. Learn more here!

</div><div class="col-sm-6">

### [<i class="fa fa-bank" aria-hidden="true"></i> Fable architecture](docs/plugins.html)

Is it very easy to add features to Fable using plugins. The best example is [the
plugin to transform NUnit tests into Mocha](https://github.com/fable-compiler/Fable/tree/master/src/plugins/nunit/Fable.Plugins.NUnit.fsx).
In order to understand the plugin system we'll review briefly how Fable works.

</div></div>
</div>
