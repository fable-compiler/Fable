# fable-import-webgl-ext

Fable bindings for WebGL Extensions

## Installation

```shell
$ npm install --save fable-core
$ npm install --save-dev fable-import-webgl-ext
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
  </ItemGroup>
  <ItemGroup>
    <Compile Include="node_modules/fable-import-webgl-ext/Fable.Import.WebGLExt.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-webgl-ext/Fable.Import.WebGLExt.fs"

open Fable.Core
open Fable.Import
```
