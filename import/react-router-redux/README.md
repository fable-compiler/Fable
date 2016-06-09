# fable-import-react-router-redux

Fable bindings for the Redux React Router (react-router-redux) bindings.

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
