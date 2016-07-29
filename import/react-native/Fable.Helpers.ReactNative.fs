[<Fable.Core.Erase>]
module Fable.Helpers.ReactNative

open System
open Fable.Import.ReactNative
open Fable.Core
open Fable.Import

type RN = ReactNative.Globals

module Props =

    [<StringEnum>]
    type Alignment =
    | Auto
    | [<CompiledName("flex-start")>] FlexStart
    | Center 
    | [<CompiledName("flex-end")>] FlexEnd
    | Strech
    
    [<StringEnum>]
    type ItemAlignment =
    | [<CompiledName("flex-start")>] FlexStart
    | Center 
    | [<CompiledName("flex-end")>] FlexEnd
    | Strech    
    
    [<StringEnum>]
    type TextAlignment =
    | Auto
    | Default
    | Left
    | Center 
    | Right
    | Justify

    [<KeyValueList>]
    type ITransformsStyle =
        interface end

    [<KeyValueList>]
    type TransformsStyle =
        | Transform of obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj
        | TransformMatrix of ResizeArray<float>
        | Rotation of float
        | ScaleX of float
        | ScaleY of float
        | TranslateX of float
        | TranslateY of float
        interface ITransformsStyle

    [<KeyValueList>]
    type IFlexStyle =
        interface end

    [<KeyValueList>]
    type FlexStyle =
        | AlignItems of ItemAlignment
        | AlignSelf of Alignment
        | BorderBottomWidth of float
        | BorderLeftWidth of float
        | BorderRightWidth of float
        | BorderTopWidth of float
        | BorderWidth of float
        | Bottom of float
        | Flex of int
        | FlexDirection of string
        | FlexWrap of string
        | Height of float
        | JustifyContent of string
        | Left of float
        | Margin of float
        | MarginBottom of float
        | MarginHorizontal of float
        | MarginLeft of float
        | MarginRight of float
        | MarginTop of float
        | MarginVertical of float
        | Padding of float
        | PaddingBottom of float
        | PaddingHorizontal of float
        | PaddingLeft of float
        | PaddingRight of float
        | PaddingTop of float
        | PaddingVertical of float
        | Position of string
        | Right of float
        | Top of float
        | Width of float
        interface IFlexStyle

    [<KeyValueList>]
    type IViewStyle =
        interface end

    [<KeyValueList>]
    type ViewStyle =
        // FlexStyle
        | AlignItems of ItemAlignment
        | AlignSelf of Alignment
        | BorderBottomWidth of float
        | BorderLeftWidth of float
        | BorderRightWidth of float
        | BorderTopWidth of float
        | BorderWidth of float
        | Bottom of float
        | Flex of int
        | FlexDirection of string
        | FlexWrap of string
        | Height of float
        | JustifyContent of string
        | Left of float
        | Margin of float
        | MarginBottom of float
        | MarginHorizontal of float
        | MarginLeft of float
        | MarginRight of float
        | MarginTop of float
        | MarginVertical of float
        | Padding of float
        | PaddingBottom of float
        | PaddingHorizontal of float
        | PaddingLeft of float
        | PaddingRight of float
        | PaddingTop of float
        | PaddingVertical of float
        | Position of string
        | Right of float
        | Top of float
        | Width of float
        // End FlexStyle   
        // TransformsStyle
        | Transform of obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj
        | TransformMatrix of ResizeArray<float>
        | Rotation of float
        | ScaleX of float
        | ScaleY of float
        | TranslateX of float
        | TranslateY of float
        // End TransformsStyle 
        | BackgroundColor of string
        | BorderBottomColor of string
        | BorderBottomLeftRadius of float
        | BorderBottomRightRadius of float
        | BorderColor of string
        | BorderLeftColor of string
        | BorderRadius of float
        | BorderRightColor of string
        | BorderTopColor of string
        | BorderTopLeftRadius of float
        | BorderTopRightRadius of float
        | Opacity of float
        | Overflow of string
        | ShadowColor of string
        | ShadowOffset of obj
        | ShadowOpacity of float
        | ShadowRadius of float
        interface IViewStyle

    [<KeyValueList>]
    type IImageStyle =
        interface end

    [<KeyValueList>]
    type ImageStyle =
        // FlexStyle
        | AlignItems of ItemAlignment
        | AlignSelf of Alignment
        | BorderBottomWidth of float
        | BorderLeftWidth of float
        | BorderRightWidth of float
        | BorderTopWidth of float
        | BorderWidth of float
        | Bottom of float
        | Flex of int
        | FlexDirection of string
        | FlexWrap of string
        | Height of float
        | JustifyContent of string
        | Left of float
        | Margin of float
        | MarginBottom of float
        | MarginHorizontal of float
        | MarginLeft of float
        | MarginRight of float
        | MarginTop of float
        | MarginVertical of float
        | Padding of float
        | PaddingBottom of float
        | PaddingHorizontal of float
        | PaddingLeft of float
        | PaddingRight of float
        | PaddingTop of float
        | PaddingVertical of float
        | Position of string
        | Right of float
        | Top of float
        | Width of float
        // End FlexStyle
        // TransformsStyle
        | Transform of obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj
        | TransformMatrix of ResizeArray<float>
        | Rotation of float
        | ScaleX of float
        | ScaleY of float
        | TranslateX of float
        | TranslateY of float
        // End TransformsStyle        
        | ResizeMode of string
        | BackgroundColor of string
        | BorderColor of string
        | BorderRadius of float
        | Overflow of string
        | TintColor of string
        | Opacity of float
        interface IImageStyle

    [<KeyValueList>]
    type ITextStyle =
        interface end

    [<KeyValueList>]
    type TextStyle =
        // ViewStyle
        | AlignItems of Alignment
        | AlignSelf of Alignment
        | BorderBottomWidth of float
        | BorderLeftWidth of float
        | BorderRightWidth of float
        | BorderTopWidth of float
        | BorderWidth of float
        | Bottom of float
        | Flex of int
        | FlexDirection of string
        | FlexWrap of string
        | Height of float
        | JustifyContent of string
        | Left of float
        | Margin of float
        | MarginBottom of float
        | MarginHorizontal of float
        | MarginLeft of float
        | MarginRight of float
        | MarginTop of float
        | MarginVertical of float
        | Padding of float
        | PaddingBottom of float
        | PaddingHorizontal of float
        | PaddingLeft of float
        | PaddingRight of float
        | PaddingTop of float
        | PaddingVertical of float
        | Position of string
        | Right of float
        | Top of float
        | Width of float
        | Transform of obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj
        | TransformMatrix of ResizeArray<float>
        | Rotation of float
        | ScaleX of float
        | ScaleY of float
        | TranslateX of float
        | TranslateY of float
        | BackgroundColor of string
        | BorderBottomColor of string
        | BorderBottomLeftRadius of float
        | BorderBottomRightRadius of float
        | BorderColor of string
        | BorderLeftColor of string
        | BorderRadius of float
        | BorderRightColor of string
        | BorderTopColor of string
        | BorderTopLeftRadius of float
        | BorderTopRightRadius of float
        | Opacity of float
        | Overflow of string
        | ShadowColor of string
        | ShadowOffset of obj
        | ShadowOpacity of float
        | ShadowRadius of float
        // End ViewStyle    
        | Color of string
        | FontFamily of string
        | FontSize of float
        | FontStyle of string
        | FontWeight of string
        | LetterSpacing of float
        | LineHeight of float
        | TextAlign of TextAlignment
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
        | Style of ViewStyle list
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
        | Style of ViewStyle list
        | TestID of string
        interface IViewProperties

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
        | Style of ImageStyle list
        | TestID of string 
        interface IImageProperties        

    [<KeyValueList>]
    type IListViewProperties =
        interface end

    [<KeyValueList>]
    type ListViewProperties =
        // TODO: inherit ScrollViewProperties
        | DataSource of ListViewDataSource
        | InitialListSize of float
        | OnChangeVisibleRows of Func<ResizeArray<obj>, ResizeArray<obj>, unit>
        | OnEndReached of (unit -> unit)
        | OnEndReachedThreshold of float
        | PageSize of float
        | RemoveClippedSubviews of bool
        | RenderFooter of Func<React.ReactElement<obj>>
        | RenderHeader of Func<React.ReactElement<obj>>
        | RenderRow of Func<obj, U2<string, float>, U2<string, float>, bool, React.ReactElement<obj>>
        | RenderScrollComponent of Func<ScrollViewProperties, React.ReactElement<ScrollViewProperties>>
        | RenderSectionHeader of Func<obj, U2<string, float>, React.ReactElement<obj>>
        | RenderSeparator of Func<U2<string, float>, U2<string, float>, bool, React.ReactElement<obj>>
        | ScrollRenderAheadDistance of float
        interface IListViewProperties

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

let inline listView (props: IListViewProperties list) (children: React.ReactElement<obj> list): React.ReactElement<obj> =
    React.createElement(
        RN.ListView, 
        unbox props,
        unbox(List.toArray children)) |> unbox