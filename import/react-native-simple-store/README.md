# fable-react-native-simple-store

Fable bindings for a simple React Native data store

## Installation

Install [fable-import-react-native](https://www.npmjs.com/package/fable-import-react-native) and follow the instructions for that package.

```sh
$ npm install --save-dev react-native-simple-store
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Compile Include="node_modules/fable-react-native-simple-store/Fable.Helpers.ReactNative.SimpleStore.fs" />        
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#load "node_modules/fable-react-native-simple-store/Fable.Helpers.ReactNative.SimpleStore.fs"

open Fable.Core
open Fable.Import
module R = Fable.Helpers.React
module RN = Fable.Import.ReactNative
open RN.Props

...

  

```
