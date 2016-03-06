# fable-import-browser

Fable bindings for native Browser APIs

## Installation

```sh
$ npm install --save-dev fable-core fable-import-js fable-import-browser
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Compile Include="node_modules/fable-core/Fable.Core.fs" />
    <Compile Include="node_modules/fable-import-js/Fable.Import.JS.fs" />
    <Compile Include="node_modules/fable-import-browser/Fable.Import.Browser.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#load "node_modules/fable-core/Fable.Core.fs"
#load "node_modules/fable-import-js/Fable.Import.JS.fs"
#load "node_modules/fable-import-browser/Fable.Import.Browser.fs"

open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser
```
