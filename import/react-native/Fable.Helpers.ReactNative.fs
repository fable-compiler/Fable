[<Fable.Core.Erase>]
module Fable.Helpers.ReactNative

open System
open Fable.Import.ReactNative
open Fable.Core
open Fable.Import

type RN = ReactNative.Globals

module Props =
    [<KeyValueList>]
    type ITextStyle =
        interface end

    [<KeyValueList>]
    type TextStyle =
        | Color of string
        | FontFamily of string
        | FontSize of float
        | FontStyle of string
        | FontWeight of string
        | LetterSpacing of float
        | LineHeight of float
        | TextAlign of string
        | TextDecorationLine of string
        | TextDecorationStyle of string
        | TextDecorationColor of string
        | WritingDirection of string
        interface ITextStyle

    [<KeyValueList>]
    type ITextProperties =
        interface end

    [<KeyValueList>]
    type TextProperties =
        | AllowFontScaling of bool
        | NumberOfLines of int
        | OnLayout of (LayoutChangeEvent -> unit)
        | OnPress of (unit -> unit)
        | Style of TextStyle
        | TestID of string
        interface ITextProperties

    [<KeyValueList>]
    type ITouchableHighlightProperties =
        interface end

    [<KeyValueList>]
    type TouchableHighlightProperties =
        | ActiveOpacity of float
        | OnHideUnderlay of (unit -> unit)
        | OnShowUnderlay of (unit -> unit)
        | Style of ViewStyle
        | UnderlayColor of string    
        | Accessible of bool
        | DelayLongPress of float
        | DelayPressIn of float
        | DelayPressOut of float
        | OnLayout of (LayoutChangeEvent -> unit)
        | OnLongPress of (unit -> unit)
        | OnPress of (unit -> unit)
        | OnPressIn of (unit -> unit)
        | OnPressOut of (unit -> unit)  
        interface ITouchableHighlightProperties    

    [<KeyValueList>]
    type IViewProperties =
        interface end

    [<KeyValueList>]
    type ViewProperties =
        | AccessibilityLabel of string
        | Accessible of bool
        | OnAcccessibilityTap of (unit -> unit)
        | OnLayout of (LayoutChangeEvent -> unit)
        | OnMagicTap of (unit -> unit)
        | PointerEvents of string
        | RemoveClippedSubviews of bool
        | Style of ViewStyle
        | TestID of string
        interface IViewProperties

    [<KeyValueList>]
    type IImageStyle =
        interface end

    [<KeyValueList>]
    type ImageStyle =
        | ResizeMode of string
        | BackgroundColor of string
        | BorderColor of string
        | BorderWidth of float
        | BorderRadius of float
        | Overflow of string
        | TintColor of string
        | Opacity of float
        interface IImageStyle

    [<KeyValueList>]
    type IImageSourceProperties =
        interface end

    [<KeyValueList>]
    type ImageSourceProperties =
        | Uri of string
        | IsStatic of bool    
        interface IImageSourceProperties

    [<KeyValueList>]
    type IImageProperties =
        interface end

    [<KeyValueList>]
    type ImageProperties =
        | AccessibilityLabel of string
        | Accessible of bool
        | CapInsets of Insets
        | Source of ImageSourceProperties list
        | DefaultSource of obj
        | OnError of (obj -> unit)
        | OnLoad of (unit -> unit)
        | OnLoadEnd of (unit -> unit)
        | OnLoadStart of (unit -> unit)
        | OnProgress of (unit -> unit)
        | OnLayout of (LayoutChangeEvent -> unit)
        | ResizeMode of string 
        | Style of ImageStyle
        | TestID of string 
        interface IImageProperties        

open Props

let inline text (props: ITextProperties list) (text:string): React.ReactElement<obj> =
    React.createElement(
        RN.Text, 
        unbox props,
        unbox text) |> unbox

let inline image (props: IImageProperties list) (children: React.ReactElement<obj> list): React.ReactElement<obj> =
    React.createElement(
        RN.Image, 
        unbox props,
        unbox(List.toArray children)) |> unbox

let inline touchableHighlight (props: ITouchableHighlightProperties list) (child: React.ReactElement<obj>): React.ReactElement<obj> =
    React.createElement(
        RN.TouchableHighlight,
        unbox props,
        [| unbox child |]) |> unbox

let inline view (props: IViewProperties list) (children: React.ReactElement<obj> list): React.ReactElement<obj> =
    React.createElement(
        RN.View, 
        unbox props,
        unbox(List.toArray children)) |> unbox