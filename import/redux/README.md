# fable-import-redux

Fable bindings for Redux, version v3.3.1.
These were generated from Typescript, with some modifications,
namely: Reducer is type narrowing of Func, with type parameters,
and all setters are removed.

## Installation

```sh
$ npm install --save redux fable-core
$ npm install --save-dev fable-import-redux
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-redux/Fable.Import.Redux.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-redux/Fable.Import.Redux.fs"

open Fable.Core
open Fable.Import
```
