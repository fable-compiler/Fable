# fable-import-react-native

Fable bindings for React

## Installation

```sh
$ npm install --save react-native react-dom fable-core 
$ npm install --save-dev fable-import-react fable-import-react-native
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.React.fs" />    
    <Compile Include="node_modules/fable-import-react/Fable.Helpers.React.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.ReactNative.fs" />
    <Compile Include="node_modules/fable-import-react/Fable.Helpers.ReactNative.fs" />    
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "node_modules/fable-import-react/Fable.Helpers.React.fs"
#load "node_modules/fable-import-react/Fable.Import.ReactNative.fs"
#load "node_modules/fable-import-react/Fable.Helpers.ReactNative.fs"

open Fable.Core
open Fable.Import
module R = Fable.Helpers.React
module RN = Fable.Import.ReactNative
open RN.Props

...

  let button =
    text [] "click me!"
    |> touchableHighlight [
        OnPress (fun () -> console.log("clicked!"))]
        
  let image =
    image 
      [ Source [ Uri "http://facebook.github.io/react/img/logo_og.png"; IsStatic true ]]      
      [ ]

                                
  view [] 
   [ image
     button ]


```
