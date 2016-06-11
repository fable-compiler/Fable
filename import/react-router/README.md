# fable-import-react-router

Fable bindings for React Router (react-router) 2.0.0 and History 2.0.0.
These were generated from Typescript definitions but required some manual
modification, most significantly of certain History module names. In
addition, setters were removed from all types.

Typescript source file is [here](https://github.com/DefinitelyTyped/DefinitelyTyped/blob/df9065dc6e93d23a0d284c57c30570eefb81d35d/react-router/react-router.d.ts).


## Installation

```sh
$ npm install --save react-router fable-core
$ npm install --save-dev fable-import-react-router
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-react-router/Fable.Import.HistoryModule.fs" />
    <Compile Include="node_modules/fable-import-react-router/Fable.Import.ReactRouter.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react-router/Fable.Import.HistoryModule.fs"
#load "node_modules/fable-import-react-router/Fable.Import.ReactRouter.fs"

open Fable.Core
open Fable.Import
```
