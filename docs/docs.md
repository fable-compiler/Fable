 - tagline: Learn about how Fable works & how to use it

# Fable documentation

The following documentation pages are generated from the `docs` pages
in the Fable repository on GitHub. Is anything unclear or missing?
Help us make Fable better by contributing!

<div class="fable-docs">
<div class="row"><div class="col-sm-12">

## Fable version 1 documentation

The following documentation pages contain the most up-to-date information on getting started
with the latest version of Fable and cover best practices for using Fable in typical projects.

</div></div>
<div class="row"><div class="col-sm-6">

### [<i class="fa fa-cog" aria-hidden="true"></i> Getting started](pages/getting-started.html)

Start here! The page covers how to create your first Fable project. It explains the
Fable template and gives a step-by-step guide for creating your first Fable project!

</div><div class="col-sm-6">

### [<i class="fa fa-folder-open" aria-hidden="true"></i> Prerequisites and editors](pages/prerequisites.html)

To get started with Fable, you will need a couple of things installed on your machine
including Node, Yarn, Dotnet SDK and a suitable editor. This page explains why & how
to get ready.

</div></div>
<div class="row"><div class="col-sm-6">

### [<i class="fa fa-cloud" aria-hidden="true"></i> What is Fable server](pages/what-is-fable-server.html)

Fable comes with a daemon that runs in the background and orchestrates the 
compilation process with Webpack. This page explains how the daemon works.

</div><div class="col-sm-6">

### [<i class="fa fa-pencil-square" aria-hidden="true"></i> Latest blog posts](blog.html)

Check out the latest blog posts on Fable. It contains the most recent information,
useful links, case studies and sample projects. If you want to keep up-to-date, 
this is the place to go.

</div></div>
<div class="row"><div class="col-sm-12">

## Background information

The following pages contain more background on Fable including the .NET and F# library support
and detailed info about the `fable` command line tool. Some of the information below still
need to be updated to cover the latest version - you can help by [sending us a pull 
request](https://github.com/fable-compiler/Fable)!

</div></div>
<div class="row"><div class="col-sm-6">

### [<i class="fa fa-cog" aria-hidden="true"></i> Compiling to JavaScript](docs/compiling.html)

The page covers all you need to get started
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
