# Prerequisites

To get started with Fable, you will need a couple of things installed on your machine. 

 - F# Compiler
 - Node (with Npm)
 - Yarn
 - Dotnet SDK
 - IDE

## F# Compiler
If you have Visual Studio or Xamarin installed with Visual F# Tools installed then you already have F# Compiler on your machine. If you are starting from scratch, refer to [fsharp.org](http://fsharp.org/) to get started with F#. 

## Node
Node is a javascript runtime. That means Node to javascript is what .NET is to F#. Node is central to the javascript ecosystem. You can insall Node from the [official website](https://nodejs.org/en/). You need it becuase Fable has dependencies that run on Node. Other than that, you could compile your F# code and run it yourself using Node.  

## Npm (Node Package Manager)
Node package manager or npm is what you use by default for dependency management in the javascript ecosystem. When you install Node, npm should also be installed on your machine so you don't need an additional install for this one.

## Yarn 
Although npm is the default dependency manager for javascript, we recommend (and use) [yarn](https://yarnpkg.com/lang/en/docs/install/) instead. The difference between yarn and npm is that yarn uses lock files when installing dependencies. When someone else is installing dependencies with that same lock file present, yarn makes sure the exact same versions of the libraries and their (transitive) dependencies are installed, leading to reproducible builds on different machines. 

## Dotnet SDK
Fable integrates with the lastest dotnet project format provided by the [dotnet sdk](https://www.microsoft.com/net/core). You will use the dotnet cli for Fable project management (referencing projects, adding projects to a solution etc.) and for Fable library management as facilitated by Paket to manage your dependencies and to build and publish Fable libraries yourself. 

## IDE 
For Cross-platform F# developement, you can use either [Visual Studio Code](https://code.visualstudio.com/) or [Atom](https://atom.io/) with the awesome [Ionide](http://ionide.io/) extension.