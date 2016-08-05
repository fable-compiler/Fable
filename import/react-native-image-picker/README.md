# fable-import-react-native-image-picker

Fable bindings for React Native Image Picker

## Installation

Install [fable-import-react-native](https://www.npmjs.com/package/fable-import-react-native) and follow the instructions for that package.

```sh
$ npm install --save-dev fable-import-react-native-image-picker
```

Follow install instructions for [react-native-image-picker](https://github.com/marcshilling/react-native-image-picker).

## Usage

Follow instructions for [react-native-image-picker](https://github.com/marcshilling/react-native-image-picker).

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Compile Include="node_modules/fable-import-react/Fable.Import.ReactNativeImagePicker.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Helpers.ReactNativeImagePicker.fs" />            
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
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