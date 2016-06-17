# fable-import-fetch

Fable bindings for Fetch API
See: https://developer.mozilla.org/en/docs/Web/API/Fetch_API

## Installation

```sh
$ npm install --save fable-core
$ npm install --save-dev fable-import-fetch
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-fetch/Fable.Import.Fetch.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-fetch/Fable.Import.Fetch.fs"

open Fable.Core
open Fable.Import
```
