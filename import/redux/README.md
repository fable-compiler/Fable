# fable-import-redux

Fable bindings for Redux, version v3.3.1.

These declarations were generated from Typescript, with some modifications,
namely:

  1. `Reducer` is a type narrowing of Func, with type parameters
  2. Lowercased the module names in the Import attributes
  3. All setters are removed.

Typescript source file is [here](https://github.com/DefinitelyTyped/DefinitelyTyped/blob/c2bfaedeee7ae4ed5e0f01ebcf4af7acfe2c77c3/redux/redux.d.ts).

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
