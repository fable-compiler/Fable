# fable-import-react-native-image-picker

Fable bindings for React Native Image Picker

## Installation

```sh
$ npm install --save react-native react-dom fable-core
$ npm install --save-dev fable-import-react fable-import-react-native fable-import-react-native-image-picker
```

## Usage

Follow instructions for [react-native-image-picker](https://github.com/marcshilling/react-native-image-picker).

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.React.fs" />    
    <Compile Include="node_modules/fable-import-react/Fable.Helpers.React.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.ReactNative.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Helpers.ReactNative.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.ReactNativeImagePicker.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Helpers.ReactNativeImagePicker.fs" />            
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "node_modules/fable-import-react/Fable.Helpers.React.fs"
#load "node_modules/fable-import-react/Fable.Import.ReactNative.fs"
#load "node_modules/fable-import-react/Fable.Helpers.ReactNative.fs"
#load "node_modules/fable-import-react/Fable.Import.ReactNativeImagePicker.fs"
#load "node_modules/fable-import-react/Fable.Helpers.ReactNativeImagePicker.fs"

open Fable.Core
open Fable.Import
module R = Fable.Helpers.React
module RN = Fable.Import.ReactNative
type IP = ReactImagePicker.Globals
open Fable.Helpers.ReactNativeImagePicker
open Fable.Helpers.ReactNativeImagePicker.Props

...

showImagePicker
  [Title "Image picker"; AllowsEditing true] 
  (fun result -> 
    if not result.didCancel then
        if String.IsNullOrEmpty result.error then
            console.log("Image Uri: " + result.uri)
        else
            console.log("Error: " + result.error)
    else
        console.log("dialog canceled"))
        
```