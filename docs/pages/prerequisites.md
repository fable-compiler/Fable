# Prerequisites

To get started with Fable, you will need a couple of things installed on your machine. 

 - Node
 - Yarn
 - Dotnet SDK
 - IDE

## Node
Node is a javascript runtime. That means Node to javascript is what .NET is to F#. Node is central to the javascript ecosystem. You can install Node from the [official website](https://nodejs.org/en/). You need it becuase Fable has dependencies that run on Node. Other than that, you could compile your F# code and run it yourself using Node.  

## Yarn 
Although npm (Node Package Manager) is the default dependency manager for javascript, we recommend (and use) [yarn](https://yarnpkg.com/lang/en/docs/install/) instead. The difference between yarn and npm is that yarn uses lock files when installing dependencies. When someone else is installing dependencies with that same lock file present, yarn makes sure the exact same versions of the libraries and their (transitive) dependencies are installed, leading to reproducible builds on different machines. 

## Dotnet SDK
Fable integrates with the lastest dotnet project format provided by the [dotnet sdk](https://www.microsoft.com/net/core). You will use the dotnet cli for Fable project management (referencing projects, adding projects to a solution etc.) and for Fable library management as facilitated by Paket to manage your dependencies and to build and publish Fable libraries yourself. 

## IDE 
You can use any F# compatible IDE with dotnet support, for example:
 - [Visual Studio Code](https://code.visualstudio.com/) (with [Ionide](http://ionide.io/))
 - [Atom](https://atom.io/) (with [Ionide](http://ionide.io/)).
 - Visual Studio 2017 
