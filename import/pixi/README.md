# fable-import-pixi

Fable bindings for Pixi.js

## Installation

```shell
$ npm install --save fable-core
$ npm install --save-dev fable-import-pixi
```

> Note: Pixi.js is not available in npm in prebuilt form,
please download it directly from their [website](http://www.pixijs.com). 

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-pixi/Fable.Import.Pixi.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-pixi/Fable.Import.Pixi.fs"

open Fable.Core
open Fable.Import
```
