[<Fable.Core.Erase>]
module internal Fable.Helpers.ReactNative

open System
open Fable.Import.ReactNative
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

type RN = ReactNative.Globals

module Props =

    [<StringEnum; RequireQualifiedAccess>]
    type Alignment =
    | Auto
    | [<CompiledName("flex-start")>] FlexStart
    | Center 
    | [<CompiledName("flex-end")>] FlexEnd
    | Stretch
    
    [<StringEnum; RequireQualifiedAccess>]
    type ItemAlignment =
    | [<CompiledName("flex-start")>] FlexStart
    | Center 
    | [<CompiledName("flex-end")>] FlexEnd
    | Stretch    
    
    [<StringEnum; RequireQualifiedAccess>]
    type TextAlignment =
    | Auto
    | Default
    | Left
    | Center 
    | Right
    | Justify

    [<StringEnum; RequireQualifiedAccess>]
    type FlexDirection =
    | Row
    | [<CompiledName("row-reverse")>] RowReverse
    | Column
    | [<CompiledName("column-reverse")>] ColumnReverse

    [<StringEnum; RequireQualifiedAccess>]
    type KeyboardType =
    | Default
    | [<CompiledName("email-address")>] EmailAddress
    | Numeric
    | [<CompiledName("phone-pad")>] PhonePad
    /// only iOS
    | [<CompiledName("ascii-capable")>] AsciiCapable 
    | [<CompiledName("numbers-and-punctuation")>] NumbersAndPunctuation
    | [<CompiledName("url")>] Url
    | [<CompiledName("number-pad")>] NumberPad
    | [<CompiledName("name-phone-pad")>] NamePhonePad
    | [<CompiledName("decimal-pad")>] DecimalPad
    | Twitter
    | [<CompiledName("web-search")>] WebSearch

    [<StringEnum; RequireQualifiedAccess>]
    type ReturnKeyType =
    | Done
    | Go
    | Next
    | Search
    | Send
    | None
    | Previous
    | Default
    | [<CompiledName("emergency-call")>] EmergencyCall
    | Google
    | Join
    | Route
    | Yahoo

    [<StringEnum; RequireQualifiedAccess>]
    type AutoCapitalizeType =
    | None
    | Sentences
    | Words
    | Characters
    
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
        | FlexDirection of FlexDirection
        | FlexWrap of string
        | Height of float
        | JustifyContent of Alignment
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
    type ISwitchProperties =
        interface end

    [<KeyValueList>]
    type SwitchProperties =
        | Disabled of bool
        | OnTintColor of string
        | OnValueChange of (bool -> unit)
        | ThumbTintColor of string
        | TintColor of string
        | Value of bool
        | Style of ViewStyle list

    [<KeyValueList>]
    type IWebViewProperties =
        interface end

    [<KeyValueList>]
    type WebViewProperties =
        | JavaScriptEnabledAndroid of bool
        | ScalesPageToFit of bool
        | AutomaticallyAdjustContentInsets of bool
        | Bounces of bool
        | ContentInset of Insets
        | Html of string
        | InjectedJavaScript of string
        | OnNavigationStateChange of (NavState -> unit)
        | OnShouldStartLoadWithRequest of (unit -> bool)
        | RenderError of (unit -> ViewStatic)
        | RenderLoading of (unit -> ViewStatic)
        | ScrollEnabled of bool
        | StartInLoadingState of bool
        | Style of ViewStyle
        | Url of string

    type RouteResult = {
        id: string 
        title: string 
        passProps: obj 
        backButtonTitle: string 
        content: string 
        message: string 
        index: int 
        rightButtonTitle: string 
        sceneConfig: SceneConfig 
        wrapperStyle: obj } 

    [<KeyValueList>]
    type INavigationBarRouteMapperProperties =
        interface end

    [<KeyValueList>]
    type NavigationBarRouteMapperProperties =
        | [<CompiledName("Title")>]Title of Func<RouteResult, Navigator, float, NavState, React.ReactElement<obj>>
        | [<CompiledName("LeftButton")>]LeftButton of Func<RouteResult, Navigator, float, NavState, React.ReactElement<obj>>
        | [<CompiledName("RightButton")>]RightButton of Func<RouteResult, Navigator, float, NavState, React.ReactElement<obj>>
        interface INavigationBarRouteMapperProperties
                
    [<KeyValueList>]
    type INavigationBarProperties =
        interface end

    [<KeyValueList>]
    type NavigationBarProperties =
        | Navigator of Navigator
        | RouteMapper of NavigationBarRouteMapperProperties list
        | NavState of NavState
        | Style of ViewStyle list
        interface INavigationBarProperties

    [<KeyValueList>]
    type INavigatorProperties =
        interface end

    [<KeyValueList>]
    type NavigatorProperties =
        | ItemWrapperStyle of ViewStyle
        | NavigationBarHidden of bool
        | ShadowHidden of bool
        | TintColor of string
        | TitleTextColor of string
        | Translucent of bool
        | Style of ViewStyle
        | ConfigureScene of (Route -> SceneConfig)
        | InitialRoute of Route
        | InitialRouteStack of Route []
        | NavigationBar of React.ReactElement<obj>
        | Navigator of Navigator
        | OnWillFocus of (unit -> unit)
        | OnDidFocus of (unit -> unit)
        | SceneStyle of ViewStyle
        | DebugOverlay of bool
        | RenderScene of Func<RouteResult, Navigator, React.ReactElement<obj>>
        interface INavigatorProperties

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
    type IBreadcrumbNavigationBarRouteProperties =
        interface end

    [<KeyValueList>]
    type BreadcrumbNavigationBarRouteProperties =
        | RightContentForRoute of Func<RouteResult, Navigator, React.ReactElement<obj>>
        | TitleContentForRoute of Func<RouteResult, Navigator, React.ReactElement<obj>>
        | IconForRoute of Func<RouteResult, Navigator, React.ReactElement<obj>>
        | SeparatorForRoute of Func<RouteResult, Navigator, React.ReactElement<obj>>
        interface IBreadcrumbNavigationBarRouteProperties

    [<KeyValueList>]
    type IBreadcrumbNavigationBarProperties =
        interface end

    [<KeyValueList>]
    type BreadcrumbNavigationBarProperties = 
        | Navigator of Navigator
        | RouteMapper of BreadcrumbNavigationBarRouteProperties list
        | NavState of NavState
        | Style of ViewStyle list
        interface IBreadcrumbNavigationBarProperties

    [<KeyValueList>]
    type ITextProperties =
        interface end

    [<KeyValueList>]
    type TextProperties =
        | AllowFontScaling of bool
        | NumberOfLines of int
        | OnLayout of (LayoutChangeEvent -> unit)
        | OnPress of (unit -> unit)
        | Style of TextStyle list
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
        | Source of IImageSourceProperties list
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
    type IListViewProperties<'a> =
        interface end

    [<KeyValueList>]
    type ListViewProperties<'a> =
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
        | RenderRow of Func<'a, U2<string, float>, U2<string, float>, bool, React.ReactElement<obj>>
        | RenderScrollComponent of Func<ScrollViewProperties, React.ReactElement<ScrollViewProperties>>
        | RenderSectionHeader of Func<obj, U2<string, float>, React.ReactElement<obj>>
        | RenderSeparator of Func<U2<string, float>, U2<string, float>, bool, React.ReactElement<obj>>
        | ScrollRenderAheadDistance of float
        interface IListViewProperties<'a>


    [<KeyValueList>]
    type ITouchable =
        interface end

    [<KeyValueList>]
    type Touchable =
        | OnTouchStart of (GestureResponderEvent -> unit)
        | OnTouchMove of (GestureResponderEvent -> unit)
        | OnTouchEnd of (GestureResponderEvent -> unit)
        | OnTouchCancel of (GestureResponderEvent -> unit)
        | OnTouchEndCapture of (GestureResponderEvent -> unit)
        interface ITouchable

    [<KeyValueList>]
    type ITextInputProperties =
        interface end

    [<KeyValueList>]
    type TextInputProperties =
        | BlurOnSubmit of bool
        | ClearButtonMode of string
        | ClearTextOnFocus of bool
        | EnablesReturnKeyAutomatically of bool
        | OnKeyPress of (unit -> unit)
        | ReturnKeyType of ReturnKeyType
        | SelectTextOnFocus of bool
        | SelectionState of obj
        | NumberOfLines of float
        | TextAlign of string
        | TextAlignVertical of string
        | UnderlineColorAndroid of string
        | AutoCapitalize of AutoCapitalizeType
        | AutoCorrect of bool
        | AutoFocus of bool
        | DefaultValue of string
        | Editable of bool
        | KeyboardType of KeyboardType
        | MaxLength of float
        | Multiline of bool
        | OnBlur of (unit -> unit)
        | OnChange of (obj -> unit)
        | OnChangeText of (string -> unit)
        | OnEndEditing of (obj -> unit)
        | OnFocus of (unit -> unit)
        | OnLayout of (obj -> unit)
        | OnSubmitEditing of (obj -> unit)
        | Password of bool
        | Placeholder of string
        | PlaceholderTextColor of string
        | SecureTextEntry of bool
        | Style of TextStyle list
        | TestID of string
        | Value of string
        interface ITextInputProperties

    [<KeyValueList>]
    type IMapViewProperties =
        interface end

    [<KeyValueList>]
    type MapViewProperties =
        // from Touchable
        | OnTouchStart of (GestureResponderEvent -> unit)
        | OnTouchMove of (GestureResponderEvent -> unit)
        | OnTouchEnd of (GestureResponderEvent -> unit)
        | OnTouchCancel of (GestureResponderEvent -> unit)
        | OnTouchEndCapture of (GestureResponderEvent -> unit)
        // end touchable
        | ShowsPointsOfInterest of bool
        | Annotations of ResizeArray<MapViewAnnotation>
        | LegalLabelInsets of Insets
        | MapType of string
        | MaxDelta of float
        | MinDelta of float
        | OnAnnotationPress of (unit -> unit)
        | OnRegionChange of (MapViewRegion -> unit)
        | OnRegionChangeComplete of (MapViewRegion -> unit)
        | PitchEnabled of bool
        | Region of MapViewRegion
        | RotateEnabled of bool
        | ScrollEnabled of bool
        | ShowsUserLocation of bool
        | Style of ViewStyle list
        | ZoomEnabled of bool
        interface IMapViewProperties

open Props

let inline localImage (path:string) : IImageSourceProperties list =
    Node.require.Invoke(path) |> unbox

let inline text (props: ITextProperties list) (text:string): React.ReactElement<obj> =
    React.createElement(
        RN.Text, 
        unbox props,
        unbox text) |> unbox

let inline switch (props: ISwitchProperties list): React.ReactElement<obj> =
    React.createElement(
        RN.Switch, 
        unbox props,
        unbox [||]) |> unbox

let inline textInput (props: ITextInputProperties list) (text:string): React.ReactElement<obj> =
    React.createElement(
        RN.TextInput, 
        unbox props,
        unbox text) |> unbox

let inline image (props: IImageProperties list) : React.ReactElement<obj> =
    React.createElement(
        RN.Image, 
        unbox props,
        unbox([||])) |> unbox

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

let inline webView (props: IWebViewProperties list) : React.ReactElement<obj> =
    React.createElement(
        RN.WebView, 
        unbox props,
        unbox [||]) |> unbox

[<Emit("new ListView.DataSource({rowHasChanged: (r1, r2) => r1 !== r2})")>]
let private newDataSource() : ListViewDataSource = failwith "JS only"

let inline listView<'a> (props: IListViewProperties<'a> list) (initRows:'a []) : React.ReactElement<obj> =
    React.createElement(
        RN.ListView, 
        JS.Object.assign(
            createObj ["dataSource" ==> newDataSource().cloneWithRows(initRows |> unbox)],
            props)
        |> unbox,
        unbox [||]) |> unbox

let inline mapView (props: IMapViewProperties list) : React.ReactElement<obj> =
    React.createElement(
        RN.MapView, 
        unbox props) |> unbox

let inline navigationBar (props: INavigationBarProperties list) : React.ReactElement<obj> =
    let element : React.ComponentClass<obj> = RN.Navigator.NavigationBar |> unbox
    React.createElement(
        element, 
        unbox props,
        unbox [||]) |> unbox

let inline breadcrumbNavigationBar (props: IBreadcrumbNavigationBarProperties list) : React.ReactElement<obj> =
    let element : React.ComponentClass<obj> = RN.Navigator.BreadcrumbNavigationBar |> unbox
    React.createElement(
        element, 
        unbox props,
        unbox [||]) |> unbox

let inline navigator (props: INavigatorProperties list) : React.ReactElement<obj> =
    React.createElement(
        RN.Navigator, 
        unbox props,
        unbox [||]) |> unbox

let inline createComponent<'T,'P,'S when 'T :> React.Component<'P,'S>> (props: 'P) (children: React.ReactElement<obj> list): React.ReactElement<obj> =
    unbox(React.createElement(U2.Case1(unbox typeof<'T>), toPlainJsObj props, unbox(List.toArray children)))

let inline createScene<'T,'P,'S when 'T :> React.Component<'P,'S>> (props: 'P) : React.ReactElement<obj> =
    unbox(React.createElement(U2.Case1(unbox typeof<'T>), toPlainJsObj props, unbox([||])))

let inline createRoute(title:string,index:int) =
    let r = createEmpty<Route>
    r.title <- Some title
    r.index <- Some index
    r

module Storage =
    open Fable.Core.JsInterop

    /// Loads a value as string with the given key from the local device storage. Returns None if the key is not found.
    let inline getItem (key:string) = async {
        let! v = Globals.AsyncStorage.getItem key |> Async.AwaitPromise
        match v with
        | null -> return None
        | _ -> return Some v
    }

    /// Loads a value with the given key from the local device storage. Returns None if the key is not found.
    let inline load<'a> (key:string) : Async<'a option> = async {
        let! v = Globals.AsyncStorage.getItem key |> Async.AwaitPromise
        match v with
        | null -> return None
        | _ -> return Some (ofJson v)
    }

    /// Saves a value with the given key to the local device storage.
    let inline setItem (k:string) (v:string) = async {
        let! v = Globals.AsyncStorage.setItem(k,v) |> Async.AwaitPromise
        ()
    }

    /// Saves a value with the given key to the local device storage.
    let inline save<'a> (k:string) (v:'a) = async {
        let s:string = toJson v
        let! v = Globals.AsyncStorage.setItem(k,s) |> Async.AwaitPromise
        ()
    }    