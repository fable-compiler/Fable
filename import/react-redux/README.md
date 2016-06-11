# fable-import-react-redux

Fable bindings for the Redux React bindings (react-redux) version 4.4.0.
These were generated from the Typescript file but modified to remove setters.
In addition, ElementClass needs to inherit from Component rather than 
trying to implement it as an interface.

Typescript source file is [here](https://github.com/DefinitelyTyped/DefinitelyTyped/blob/0b96933934c2e37a749aad8e38e23214e21e7dab/react-redux/react-redux.d.ts).

## Installation

```sh
$ npm install --save react-redux fable-core
$ npm install --save-dev fable-import-react-redux
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-react-redux/Fable.Import.ReactRedux.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react-redux/Fable.Import.ReactRedux.fs"

open Fable.Core
open Fable.Import
```
