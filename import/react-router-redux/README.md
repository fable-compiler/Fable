# fable-import-react-router-redux

Fable bindings for the Redux React Router (react-router-redux) bindings, v4.0.0.
These were generated from Typescript, with some modifications: namely, all 
setters were removed, and type parameters were provided when `Reducer` is used.

Typescript source file is [here](https://github.com/DefinitelyTyped/DefinitelyTyped/blob/56295f5058cac7ae458540423c50ac2dcf9fc711/react-router-redux/react-router-redux.d.ts).

## Installation

```sh
$ npm install --save react-router-redux fable-core
$ npm install --save-dev fable-import-react-router-redux
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-react-router-redux/Fable.Import.ReactRouterRedux.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react-router-redux/Fable.Import.ReactRouterRedux.fs"

open Fable.Core
open Fable.Import
```
