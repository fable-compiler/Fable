namespace Fable.Import

open System
open Fable.Core
open Fable.Import
open Fable.Import.JS
open Fable.Import.Browser


module ReactNative =
    type ReactClass<'D, 'P, 'S> =
        interface end

    and Runnable =
        Func<obj, unit>

    and NativeSyntheticEvent<'T> =
        abstract bubbles: bool with get, set
        abstract cancelable: bool with get, set
        abstract currentTarget: EventTarget with get, set
        abstract defaultPrevented: bool with get, set
        abstract eventPhase: float with get, set
        abstract isTrusted: bool with get, set
        abstract nativeEvent: 'T with get, set
        abstract target: EventTarget with get, set
        abstract timeStamp: DateTime with get, set
        abstract ``type``: string with get, set
        abstract preventDefault: unit -> unit
        abstract stopPropagation: unit -> unit

    and NativeTouchEvent =
        abstract changedTouches: ResizeArray<NativeTouchEvent> with get, set
        abstract identifier: string with get, set
        abstract locationX: float with get, set
        abstract locationY: float with get, set
        abstract pageX: float with get, set
        abstract pageY: float with get, set
        abstract target: string with get, set
        abstract timestamp: float with get, set
        abstract touches: ResizeArray<NativeTouchEvent> with get, set

    and GestureResponderEvent =
        inherit NativeSyntheticEvent<NativeTouchEvent>


    and PointProperties =
        abstract x: float with get, set
        abstract y: float with get, set

    and Insets =
        abstract top: float option with get, set
        abstract left: float option with get, set
        abstract bottom: float option with get, set
        abstract right: float option with get, set

    and NativeComponent =
        abstract setNativeProps: Func<obj, unit> with get, set

    and Touchable =
        abstract onTouchStart: Func<GestureResponderEvent, unit> option with get, set
        abstract onTouchMove: Func<GestureResponderEvent, unit> option with get, set
        abstract onTouchEnd: Func<GestureResponderEvent, unit> option with get, set
        abstract onTouchCancel: Func<GestureResponderEvent, unit> option with get, set
        abstract onTouchEndCapture: Func<GestureResponderEvent, unit> option with get, set

    and AppConfig =
        obj

    and [<Import("AppRegistry", "react-native")>] AppRegistry() =
        static member registerConfig(config: ResizeArray<AppConfig>): unit = failwith "JS only"
        static member registerComponent(appKey: string, getComponentFunc: Func<React.ComponentClass<obj>>): string = failwith "JS only"
        static member registerRunnable(appKey: string, func: Runnable): string = failwith "JS only"
        static member runApplication(appKey: string, appParameters: obj): unit = failwith "JS only"

    and LayoutAnimationTypes =
        abstract spring: string with get, set
        abstract linear: string with get, set
        abstract easeInEaseOut: string with get, set
        abstract easeIn: string with get, set
        abstract easeOut: string with get, set

    and LayoutAnimationProperties =
        abstract opacity: string with get, set
        abstract scaleXY: string with get, set

    and LayoutAnimationAnim =
        abstract duration: float option with get, set
        abstract delay: float option with get, set
        abstract springDamping: float option with get, set
        abstract initialVelocity: float option with get, set
        abstract ``type``: string option with get, set
        abstract property: string option with get, set

    and LayoutAnimationConfig =
        abstract duration: float with get, set
        abstract create: LayoutAnimationAnim option with get, set
        abstract update: LayoutAnimationAnim option with get, set
        abstract delete: LayoutAnimationAnim option with get, set

    and LayoutAnimationStatic =
        abstract configureNext: Func<LayoutAnimationConfig, Func<unit>, Func<obj, unit>, unit> with get, set
        abstract create: Func<float, string, string, LayoutAnimationConfig> with get, set
        abstract Types: LayoutAnimationTypes with get, set
        abstract Properties: LayoutAnimationProperties with get, set
        abstract configChecker: Func<obj, string, string, unit> with get, set
        abstract Presets: obj with get, set

    and FlexStyle =
        abstract alignItems: string option with get, set
        abstract alignSelf: string option with get, set
        abstract borderBottomWidth: float option with get, set
        abstract borderLeftWidth: float option with get, set
        abstract borderRightWidth: float option with get, set
        abstract borderTopWidth: float option with get, set
        abstract borderWidth: float option with get, set
        abstract bottom: float option with get, set
        abstract flex: float option with get, set
        abstract flexDirection: string option with get, set
        abstract flexWrap: string option with get, set
        abstract height: float option with get, set
        abstract justifyContent: string option with get, set
        abstract left: float option with get, set
        abstract margin: float option with get, set
        abstract marginBottom: float option with get, set
        abstract marginHorizontal: float option with get, set
        abstract marginLeft: float option with get, set
        abstract marginRight: float option with get, set
        abstract marginTop: float option with get, set
        abstract marginVertical: float option with get, set
        abstract padding: float option with get, set
        abstract paddingBottom: float option with get, set
        abstract paddingHorizontal: float option with get, set
        abstract paddingLeft: float option with get, set
        abstract paddingRight: float option with get, set
        abstract paddingTop: float option with get, set
        abstract paddingVertical: float option with get, set
        abstract position: string option with get, set
        abstract right: float option with get, set
        abstract top: float option with get, set
        abstract width: float option with get, set

    and TransformsStyle =
        abstract transform: obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj * obj option with get, set
        abstract transformMatrix: ResizeArray<float> option with get, set
        abstract rotation: float option with get, set
        abstract scaleX: float option with get, set
        abstract scaleY: float option with get, set
        abstract translateX: float option with get, set
        abstract translateY: float option with get, set

    and StyleSheetProperties =
        interface end

    and LayoutRectangle =
        abstract x: float with get, set
        abstract y: float with get, set
        abstract width: float with get, set
        abstract height: float with get, set

    and LayoutChangeEvent =
        abstract nativeEvent: obj with get, set

    and TextStyle =
        inherit ViewStyle
        abstract color: string option with get, set
        abstract fontFamily: string option with get, set
        abstract fontSize: float option with get, set
        abstract fontStyle: string option with get, set
        abstract fontWeight: string option with get, set
        abstract letterSpacing: float option with get, set
        abstract lineHeight: float option with get, set
        abstract textAlign: string option with get, set
        abstract textDecorationLine: string option with get, set
        abstract textDecorationStyle: string option with get, set
        abstract textDecorationColor: string option with get, set
        abstract writingDirection: string option with get, set

    and TextPropertiesIOS =
        abstract suppressHighlighting: bool option with get, set

    and TextProperties =
        inherit React.Props<TextProperties>
        abstract allowFontScaling: bool option with get, set
        abstract numberOfLines: float option with get, set
        abstract onLayout: Func<LayoutChangeEvent, unit> option with get, set
        abstract onPress: Func<unit> option with get, set
        abstract style: TextStyle option with get, set
        abstract testID: string option with get, set

    and TextStatic =
        inherit React.ComponentClass<TextProperties>


    and TextInputIOSProperties =
        abstract blurOnSubmit: bool option with get, set
        abstract clearButtonMode: string option with get, set
        abstract clearTextOnFocus: bool option with get, set
        abstract enablesReturnKeyAutomatically: bool option with get, set
        abstract onKeyPress: Func<unit> option with get, set
        abstract returnKeyType: string option with get, set
        abstract selectTextOnFocus: bool option with get, set
        abstract selectionState: obj option with get, set

    and TextInputAndroidProperties =
        abstract numberOfLines: float option with get, set
        abstract textAlign: string option with get, set
        abstract textAlignVertical: string option with get, set
        abstract underlineColorAndroid: string option with get, set

    and TextInputProperties =
        inherit TextInputIOSProperties
        inherit TextInputAndroidProperties
        inherit React.Props<TextInputStatic>
        abstract autoCapitalize: string option with get, set
        abstract autoCorrect: bool option with get, set
        abstract autoFocus: bool option with get, set
        abstract defaultValue: string option with get, set
        abstract editable: bool option with get, set
        abstract keyboardType: string option with get, set
        abstract maxLength: float option with get, set
        abstract multiline: bool option with get, set
        abstract onBlur: Func<unit> option with get, set
        abstract onChange: Func<obj, unit> option with get, set
        abstract onChangeText: Func<string, unit> option with get, set
        abstract onEndEditing: Func<obj, unit> option with get, set
        abstract onFocus: Func<unit> option with get, set
        abstract onLayout: Func<obj, unit> option with get, set
        abstract onSubmitEditing: Func<obj, unit> option with get, set
        abstract password: bool option with get, set
        abstract placeholder: string option with get, set
        abstract placeholderTextColor: string option with get, set
        abstract secureTextEntry: bool option with get, set
        abstract style: TextStyle option with get, set
        abstract testID: string option with get, set
        abstract value: string option with get, set

    and TextInputStatic =
        inherit NativeComponent
        inherit React.ComponentClass<TextInputProperties>
        abstract blur: Func<unit> with get, set
        abstract focus: Func<unit> with get, set

    and GestureResponderHandlers =
        abstract onStartShouldSetResponder: Func<GestureResponderEvent, bool> option with get, set
        abstract onMoveShouldSetResponder: Func<GestureResponderEvent, bool> option with get, set
        abstract onResponderGrant: Func<GestureResponderEvent, unit> option with get, set
        abstract onResponderReject: Func<GestureResponderEvent, unit> option with get, set
        abstract onResponderMove: Func<GestureResponderEvent, unit> option with get, set
        abstract onResponderRelease: Func<GestureResponderEvent, unit> option with get, set
        abstract onResponderTerminationRequest: Func<GestureResponderEvent, bool> option with get, set
        abstract onResponderTerminate: Func<GestureResponderEvent, unit> option with get, set
        abstract onStartShouldSetResponderCapture: Func<GestureResponderEvent, bool> option with get, set
        abstract onMoveShouldSetResponderCapture: Func<unit> option with get, set

    and ViewStyle =
        inherit FlexStyle
        inherit TransformsStyle
        abstract backgroundColor: string option with get, set
        abstract borderBottomColor: string option with get, set
        abstract borderBottomLeftRadius: float option with get, set
        abstract borderBottomRightRadius: float option with get, set
        abstract borderColor: string option with get, set
        abstract borderLeftColor: string option with get, set
        abstract borderRadius: float option with get, set
        abstract borderRightColor: string option with get, set
        abstract borderTopColor: string option with get, set
        abstract borderTopLeftRadius: float option with get, set
        abstract borderTopRightRadius: float option with get, set
        abstract opacity: float option with get, set
        abstract overflow: string option with get, set
        abstract shadowColor: string option with get, set
        abstract shadowOffset: obj option with get, set
        abstract shadowOpacity: float option with get, set
        abstract shadowRadius: float option with get, set

    and ViewPropertiesIOS =
        abstract accessibilityTraits: U2<string, ResizeArray<string>> option with get, set
        abstract shouldRasterizeIOS: bool option with get, set

    and ViewPropertiesAndroid =
        abstract accessibilityComponentType: string option with get, set
        abstract accessibilityLiveRegion: string option with get, set
        abstract collapsable: bool option with get, set
        abstract importantForAccessibility: string option with get, set
        abstract needsOffscreenAlphaCompositing: bool option with get, set
        abstract renderToHardwareTextureAndroid: bool option with get, set

    and ViewProperties =
        inherit ViewPropertiesAndroid
        inherit ViewPropertiesIOS
        inherit GestureResponderHandlers
        inherit Touchable
        inherit React.Props<ViewStatic>
        abstract accessibilityLabel: string option with get, set
        abstract accessible: bool option with get, set
        abstract onAcccessibilityTap: Func<unit> option with get, set
        abstract onLayout: Func<LayoutChangeEvent, unit> option with get, set
        abstract onMagicTap: Func<unit> option with get, set
        abstract pointerEvents: string option with get, set
        abstract removeClippedSubviews: bool option with get, set
        abstract style: ViewStyle option with get, set
        abstract testID: string option with get, set

    and ViewStatic =
        inherit NativeComponent
        inherit React.ComponentClass<ViewProperties>


    and NavState =
        abstract url: string option with get, set
        abstract title: string option with get, set
        abstract loading: bool option with get, set
        abstract canGoBack: bool option with get, set
        abstract canGoForward: bool option with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> obj with get, set

    and WebViewPropertiesAndroid =
        abstract javaScriptEnabledAndroid: bool option with get, set

    and WebViewPropertiesIOS =
        abstract scalesPageToFit: bool option with get, set

    and WebViewProperties =
        inherit WebViewPropertiesAndroid
        inherit WebViewPropertiesIOS
        inherit React.Props<WebViewStatic>
        abstract automaticallyAdjustContentInsets: bool option with get, set
        abstract bounces: bool option with get, set
        abstract contentInset: Insets option with get, set
        abstract html: string option with get, set
        abstract injectedJavaScript: string option with get, set
        abstract onNavigationStateChange: Func<NavState, unit> option with get, set
        abstract onShouldStartLoadWithRequest: Func<bool> option with get, set
        abstract renderError: Func<ViewStatic> option with get, set
        abstract renderLoading: Func<ViewStatic> option with get, set
        abstract scrollEnabled: bool option with get, set
        abstract startInLoadingState: bool option with get, set
        abstract style: ViewStyle option with get, set
        abstract url: string with get, set

    and WebViewStatic =
        inherit React.ComponentClass<WebViewProperties>
        abstract goBack: Func<unit> with get, set
        abstract goForward: Func<unit> with get, set
        abstract reload: Func<unit> with get, set

    and SegmentedControlIOSProperties =
        interface end

    and NavigatorIOSProperties =
        inherit React.Props<NavigatorIOSStatic>
        abstract initialRoute: Route option with get, set
        abstract itemWrapperStyle: ViewStyle option with get, set
        abstract navigationBarHidden: bool option with get, set
        abstract shadowHidden: bool option with get, set
        abstract tintColor: string option with get, set
        abstract titleTextColor: string option with get, set
        abstract translucent: bool option with get, set
        abstract style: ViewStyle option with get, set

    and NavigationIOS =
        abstract push: Func<Route, unit> with get, set
        abstract pop: Func<unit> with get, set
        abstract popN: Func<float, unit> with get, set
        abstract replace: Func<Route, unit> with get, set
        abstract replacePrevious: Func<Route, unit> with get, set
        abstract replacePreviousAndPop: Func<Route, unit> with get, set
        abstract resetTo: Func<Route, unit> with get, set
        abstract popToRoute: route: Route -> unit
        abstract popToTop: unit -> unit

    and NavigatorIOSStatic =
        inherit NavigationIOS
        inherit React.ComponentClass<NavigatorIOSProperties>


    and ActivityIndicatorIOSProperties =
        inherit React.Props<ActivityIndicatorIOSStatic>
        abstract animating: bool option with get, set
        abstract color: string option with get, set
        abstract hidesWhenStopped: bool option with get, set
        abstract onLayout: Func<obj, unit> option with get, set
        abstract size: string option with get, set
        abstract style: ViewStyle option with get, set

    and ActivityIndicatorIOSStatic =
        inherit React.ComponentClass<ActivityIndicatorIOSProperties>


    and DatePickerIOSProperties =
        inherit React.Props<DatePickerIOSStatic>
        abstract date: DateTime option with get, set
        abstract maximumDate: DateTime option with get, set
        abstract minimumDate: DateTime option with get, set
        abstract minuteInterval: float option with get, set
        abstract mode: string option with get, set
        abstract onDateChange: Func<DateTime, unit> option with get, set
        abstract timeZoneOffsetInMinutes: float option with get, set

    and DatePickerIOSStatic =
        inherit React.ComponentClass<DatePickerIOSProperties>


    and PickerIOSItemProperties =
        inherit React.Props<PickerIOSItemStatic>
        abstract value: U2<string, float> option with get, set
        abstract label: string option with get, set

    and PickerIOSItemStatic =
        inherit React.ComponentClass<PickerIOSItemProperties>


    and PickerIOSProperties =
        inherit React.Props<PickerIOSStatic>
        abstract onValueChange: Func<U2<string, float>, unit> option with get, set
        abstract selectedValue: U2<string, float> option with get, set
        abstract style: ViewStyle option with get, set

    and PickerIOSStatic =
        inherit React.ComponentClass<PickerIOSProperties>
        abstract Item: PickerIOSItemStatic with get, set

    and SliderIOSProperties =
        inherit React.Props<SliderIOSStatic>
        abstract disabled: bool option with get, set
        abstract maximumValue: float option with get, set
        abstract maximumTrackTintColor: string option with get, set
        abstract minimumValue: float option with get, set
        abstract minimumTrackTintColor: string option with get, set
        abstract onSlidingComplete: Func<unit> option with get, set
        abstract onValueChange: Func<float, unit> option with get, set
        abstract step: float option with get, set
        abstract style: ViewStyle option with get, set
        abstract value: float option with get, set

    and SliderIOSStatic =
        inherit React.ComponentClass<SliderIOSProperties>


    and SwitchIOSStyle =
        inherit ViewStyle
        abstract height: float option with get, set
        abstract width: float option with get, set

    and SwitchIOSProperties =
        inherit React.Props<SwitchIOSStatic>
        abstract disabled: bool option with get, set
        abstract onTintColor: string option with get, set
        abstract onValueChange: Func<bool, unit> option with get, set
        abstract thumbTintColor: string option with get, set
        abstract tintColor: string option with get, set
        abstract value: bool option with get, set
        abstract style: SwitchIOSStyle option with get, set

    and SwitchIOSStatic =
        inherit React.ComponentClass<SwitchIOSProperties>


    and ImageResizeModeStatic =
        abstract contain: string with get, set
        abstract cover: string with get, set
        abstract stretch: string with get, set

    and ImageStyle =
        inherit FlexStyle
        inherit TransformsStyle
        abstract resizeMode: string option with get, set
        abstract backgroundColor: string option with get, set
        abstract borderColor: string option with get, set
        abstract borderWidth: float option with get, set
        abstract borderRadius: float option with get, set
        abstract overflow: string option with get, set
        abstract tintColor: string option with get, set
        abstract opacity: float option with get, set

    and ImagePropertiesIOS =
        abstract accessibilityLabel: string option with get, set
        abstract accessible: bool option with get, set
        abstract capInsets: Insets option with get, set
        abstract defaultSource: obj option with get, set
        abstract onError: Func<obj, unit> option with get, set
        abstract onLoad: Func<unit> option with get, set
        abstract onLoadEnd: Func<unit> option with get, set
        abstract onLoadStart: Func<unit> option with get, set
        abstract onProgress: Func<unit> option with get, set

    and ImageProperties =
        inherit ImagePropertiesIOS
        inherit React.Props<Image>
        abstract onLayout: Func<LayoutChangeEvent, unit> option with get, set
        abstract resizeMode: string option with get, set
        abstract source: U2<obj, string> with get, set
        abstract style: ImageStyle option with get, set
        abstract testID: string option with get, set

    and ImageStatic =
        inherit React.ComponentClass<ImageProperties>
        abstract uri: string with get, set
        abstract resizeMode: ImageResizeModeStatic with get, set

    and ListViewProperties =
        inherit ScrollViewProperties
        inherit React.Props<ListViewStatic>
        abstract dataSource: ListViewDataSource option with get, set
        abstract initialListSize: float option with get, set
        abstract onChangeVisibleRows: Func<ResizeArray<obj>, ResizeArray<obj>, unit> option with get, set
        abstract onEndReached: Func<unit> option with get, set
        abstract onEndReachedThreshold: float option with get, set
        abstract pageSize: float option with get, set
        abstract removeClippedSubviews: bool option with get, set
        abstract renderFooter: Func<React.ReactElement<obj>> option with get, set
        abstract renderHeader: Func<React.ReactElement<obj>> option with get, set
        abstract renderRow: Func<obj, U2<string, float>, U2<string, float>, bool, React.ReactElement<obj>> option with get, set
        abstract renderScrollComponent: Func<ScrollViewProperties, React.ReactElement<ScrollViewProperties>> option with get, set
        abstract renderSectionHeader: Func<obj, U2<string, float>, React.ReactElement<obj>> option with get, set
        abstract renderSeparator: Func<U2<string, float>, U2<string, float>, bool, React.ReactElement<obj>> option with get, set
        abstract scrollRenderAheadDistance: float option with get, set

    and ListViewStatic =
        inherit React.ComponentClass<ListViewProperties>
        abstract DataSource: ListViewDataSource with get, set

    and MapViewAnnotation =
        abstract latitude: float option with get, set
        abstract longitude: float option with get, set
        abstract animateDrop: bool option with get, set
        abstract title: string option with get, set
        abstract subtitle: string option with get, set
        abstract hasLeftCallout: bool option with get, set
        abstract hasRightCallout: bool option with get, set
        abstract onLeftCalloutPress: Func<unit> option with get, set
        abstract onRightCalloutPress: Func<unit> option with get, set
        abstract id: string option with get, set

    and MapViewRegion =
        abstract latitude: float with get, set
        abstract longitude: float with get, set
        abstract latitudeDelta: float with get, set
        abstract longitudeDelta: float with get, set

    and MapViewPropertiesIOS =
        abstract showsPointsOfInterest: bool option with get, set

    and MapViewProperties =
        inherit MapViewPropertiesIOS
        inherit Touchable
        inherit React.Props<MapViewStatic>
        abstract annotations: ResizeArray<MapViewAnnotation> option with get, set
        abstract legalLabelInsets: Insets option with get, set
        abstract mapType: string option with get, set
        abstract maxDelta: float option with get, set
        abstract minDelta: float option with get, set
        abstract onAnnotationPress: Func<unit> option with get, set
        abstract onRegionChange: Func<MapViewRegion, unit> option with get, set
        abstract onRegionChangeComplete: Func<MapViewRegion, unit> option with get, set
        abstract pitchEnabled: bool option with get, set
        abstract region: MapViewRegion option with get, set
        abstract rotateEnabled: bool option with get, set
        abstract scrollEnabled: bool option with get, set
        abstract showsUserLocation: bool option with get, set
        abstract style: ViewStyle option with get, set
        abstract zoomEnabled: bool option with get, set

    and MapViewStatic =
        inherit React.ComponentClass<MapViewProperties>


    and TouchableWithoutFeedbackAndroidProperties =
        abstract accessibilityComponentType: string option with get, set

    and TouchableWithoutFeedbackIOSProperties =
        abstract accessibilityTraits: U2<string, ResizeArray<string>> option with get, set

    and TouchableWithoutFeedbackProperties =
        inherit TouchableWithoutFeedbackAndroidProperties
        inherit TouchableWithoutFeedbackIOSProperties
        abstract accessible: bool option with get, set
        abstract delayLongPress: float option with get, set
        abstract delayPressIn: float option with get, set
        abstract delayPressOut: float option with get, set
        abstract onLayout: Func<LayoutChangeEvent, unit> option with get, set
        abstract onLongPress: Func<unit> option with get, set
        abstract onPress: Func<unit> option with get, set
        abstract onPressIn: Func<unit> option with get, set
        abstract onPressOut: Func<unit> option with get, set
        abstract style: ViewStyle option with get, set

    and TouchableWithoutFeedbackProps =
        inherit TouchableWithoutFeedbackProperties
        inherit React.Props<TouchableWithoutFeedbackStatic>


    and TouchableWithoutFeedbackStatic =
        inherit React.ComponentClass<TouchableWithoutFeedbackProps>


    and TouchableHighlightProperties =
        inherit TouchableWithoutFeedbackProperties
        inherit React.Props<TouchableHighlightStatic>
        abstract activeOpacity: float option with get, set
        abstract onHideUnderlay: Func<unit> option with get, set
        abstract onShowUnderlay: Func<unit> option with get, set
        abstract style: ViewStyle option with get, set
        abstract underlayColor: string option with get, set

    and TouchableHighlightStatic =
        inherit React.ComponentClass<TouchableHighlightProperties>


    and TouchableOpacityProperties =
        inherit TouchableWithoutFeedbackProperties
        inherit React.Props<TouchableOpacityStatic>
        abstract activeOpacity: float option with get, set

    and TouchableOpacityStatic =
        inherit React.ComponentClass<TouchableOpacityProperties>


    and TouchableNativeFeedbackProperties =
        inherit TouchableWithoutFeedbackProperties
        inherit React.Props<TouchableNativeFeedbackStatic>
        abstract background: obj option with get, set

    and TouchableNativeFeedbackStatic =
        inherit React.ComponentClass<TouchableNativeFeedbackProperties>
        abstract SelectableBackground: Func<TouchableNativeFeedbackStatic> with get, set
        abstract SelectableBackgroundBorderless: Func<TouchableNativeFeedbackStatic> with get, set
        abstract Ripple: Func<string, bool, TouchableNativeFeedbackStatic> with get, set

    and LeftToRightGesture =
        interface end

    and AnimationInterpolator =
        interface end

    and SceneConfig =
        abstract gestures: obj with get, set
        abstract springFriction: float with get, set
        abstract springTension: float with get, set
        abstract defaultTransitionVelocity: float with get, set
        abstract animationInterpolators: obj with get, set

    and SceneConfigs =
        abstract FloatFromBottom: SceneConfig with get, set
        abstract FloatFromRight: SceneConfig with get, set
        abstract PushFromRight: SceneConfig with get, set
        abstract FloatFromLeft: SceneConfig with get, set
        abstract HorizontalSwipeJump: SceneConfig with get, set

    and Route =
        abstract ``component``: React.ComponentClass<ViewProperties> option with get, set
        abstract id: string option with get, set
        abstract title: string option with get, set
        abstract passProps: obj option with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> obj with get, set
        abstract backButtonTitle: string option with get, set
        abstract content: string option with get, set
        abstract message: string option with get, set
        abstract index: float option with get, set
        abstract onRightButtonPress: Func<unit> option with get, set
        abstract rightButtonTitle: string option with get, set
        abstract sceneConfig: SceneConfig option with get, set
        abstract wrapperStyle: obj option with get, set

    and NavigatorProperties =
        inherit React.Props<Navigator>
        abstract configureScene: Func<Route, SceneConfig> option with get, set
        abstract initialRoute: Route option with get, set
        abstract initialRouteStack: ResizeArray<Route> option with get, set
        abstract navigationBar: obj with get,set //React.ReactElement<NavigatorStatic.NavigationBarProperties> option with get, set
        abstract navigator: Navigator option with get, set
        abstract onDidFocus: Function option with get, set
        abstract onWillFocus: Function option with get, set
        abstract renderScene: Func<Route, Navigator, React.ReactElement<ViewProperties>> option with get, set
        abstract sceneStyle: ViewStyle option with get, set
        abstract debugOverlay: bool option with get, set

    and NavigatorStatic =
        inherit React.ComponentClass<NavigatorProperties>
        abstract SceneConfigs: SceneConfigs with get, set
        abstract NavigationBar: obj with get,set //NavigatorStatic.NavigationBarStatic with get, set
        abstract BreadcrumbNavigationBar: obj with get,set //NavigatorStatic.BreadcrumbNavigationBarStatic with get, set
        abstract getContext: self: obj -> NavigatorStatic
        abstract getCurrentRoutes: unit -> ResizeArray<Route>
        abstract jumpBack: unit -> unit
        abstract jumpForward: unit -> unit
        abstract jumpTo: route: Route -> unit
        abstract push: route: Route -> unit
        abstract pop: unit -> unit
        abstract replace: route: Route -> unit
        abstract replaceAtIndex: route: Route * index: float -> unit
        abstract replacePrevious: route: Route -> unit
        abstract immediatelyResetRouteStack: routes: ResizeArray<Route> -> unit
        abstract popToRoute: route: Route -> unit
        abstract popToTop: unit -> unit

    and StyleSheetStatic =
        inherit React.ComponentClass<StyleSheetProperties>
        abstract create: styles: 'T -> 'T

    and DataSourceAssetCallback =
        abstract rowHasChanged: Func<obj, obj, bool> option with get, set
        abstract sectionHeaderHasChanged: Func<obj, obj, bool> option with get, set
        abstract getRowData: Func<obj, U2<float, string>, U2<float, string>, 'T> option with get, set
        abstract getSectionHeaderData: Func<obj, U2<float, string>, 'T> option with get, set

    and ListViewDataSource =
        [<Emit("new $0($1...)")>] abstract Create: onAsset: DataSourceAssetCallback -> ListViewDataSource
        abstract cloneWithRows: dataBlob: U2<ResizeArray<obj>, obj> * ?rowIdentities: ResizeArray<U2<string, float>> -> ListViewDataSource
        abstract cloneWithRowsAndSections: dataBlob: U2<ResizeArray<obj>, obj> * ?sectionIdentities: ResizeArray<U2<string, float>> * ?rowIdentities: ResizeArray<ResizeArray<U2<string, float>>> -> ListViewDataSource
        abstract getRowCount: unit -> float
        abstract getRowData: sectionIndex: float * rowIndex: float -> obj
        abstract getRowIDForFlatIndex: index: float -> string
        abstract getSectionIDForFlatIndex: index: float -> string
        abstract getSectionLengths: unit -> ResizeArray<float>
        abstract sectionHeaderShouldUpdate: sectionIndex: float -> bool
        abstract getSectionHeaderData: sectionIndex: float -> obj

    and TabBarItemProperties =
        inherit React.Props<TabBarItemStatic>
        abstract badge: U2<string, float> option with get, set
        abstract icon: U2<obj, string> option with get, set
        abstract onPress: Func<unit> option with get, set
        abstract selected: bool option with get, set
        abstract selectedIcon: U2<obj, string> option with get, set
        abstract style: ViewStyle option with get, set
        abstract systemIcon: string with get, set
        abstract title: string option with get, set

    and TabBarItemStatic =
        inherit React.ComponentClass<TabBarItemProperties>


    and TabBarIOSProperties =
        inherit React.Props<TabBarIOSStatic>
        abstract barTintColor: string option with get, set
        abstract style: ViewStyle option with get, set
        abstract tintColor: string option with get, set
        abstract translucent: bool option with get, set

    and TabBarIOSStatic =
        inherit React.ComponentClass<TabBarIOSProperties>
        abstract Item: TabBarItemStatic with get, set

    and PixelRatioStatic =
        abstract get: unit -> float

    and DeviceEventSubscriptionStatic =
        abstract remove: unit -> unit

    and DeviceEventEmitterStatic =
        abstract addListener: ``type``: string * onReceived: Func<'T, unit> -> DeviceEventSubscription

    and ScaledSize =
        abstract width: float with get, set
        abstract height: float with get, set
        abstract scale: float with get, set

    and InteractionManagerStatic =
        abstract runAfterInteractions: fn: Func<unit> -> unit

    and ScrollViewStyle =
        inherit FlexStyle
        inherit TransformsStyle
        abstract backfaceVisibility: string option with get, set
        abstract backgroundColor: string option with get, set
        abstract borderColor: string option with get, set
        abstract borderTopColor: string option with get, set
        abstract borderRightColor: string option with get, set
        abstract borderBottomColor: string option with get, set
        abstract borderLeftColor: string option with get, set
        abstract borderRadius: float option with get, set
        abstract borderTopLeftRadius: float option with get, set
        abstract borderTopRightRadius: float option with get, set
        abstract borderBottomLeftRadius: float option with get, set
        abstract borderBottomRightRadius: float option with get, set
        abstract borderStyle: string option with get, set
        abstract borderWidth: float option with get, set
        abstract borderTopWidth: float option with get, set
        abstract borderRightWidth: float option with get, set
        abstract borderBottomWidth: float option with get, set
        abstract borderLeftWidth: float option with get, set
        abstract opacity: float option with get, set
        abstract overflow: string option with get, set
        abstract shadowColor: string option with get, set
        abstract shadowOffset: obj option with get, set
        abstract shadowOpacity: float option with get, set
        abstract shadowRadius: float option with get, set

    and ScrollViewIOSProperties =
        abstract alwaysBounceHorizontal: bool option with get, set
        abstract alwaysBounceVertical: bool option with get, set
        abstract automaticallyAdjustContentInsets: bool option with get, set
        abstract bounces: bool option with get, set
        abstract bouncesZoom: bool option with get, set
        abstract canCancelContentTouches: bool option with get, set
        abstract centerContent: bool option with get, set
        abstract contentInset: Insets option with get, set
        abstract contentOffset: PointProperties option with get, set
        abstract decelerationRate: float option with get, set
        abstract directionalLockEnabled: bool option with get, set
        abstract maximumZoomScale: float option with get, set
        abstract minimumZoomScale: float option with get, set
        abstract onScrollAnimationEnd: Func<unit> option with get, set
        abstract pagingEnabled: bool option with get, set
        abstract scrollEnabled: bool option with get, set
        abstract scrollEventThrottle: float option with get, set
        abstract scrollIndicatorInsets: Insets option with get, set
        abstract scrollsToTop: bool option with get, set
        abstract snapToAlignment: string option with get, set
        abstract snapToInterval: float option with get, set
        abstract stickyHeaderIndices: ResizeArray<float> option with get, set
        abstract zoomScale: float option with get, set

    and ScrollViewProperties =
        inherit ScrollViewIOSProperties
        inherit Touchable
        abstract contentContainerStyle: ViewStyle option with get, set
        abstract horizontal: bool option with get, set
        abstract keyboardDismissMode: string option with get, set
        abstract keyboardShouldPersistTaps: bool option with get, set
        abstract onScroll: Func<obj, unit> option with get, set
        abstract removeClippedSubviews: bool option with get, set
        abstract showsHorizontalScrollIndicator: bool option with get, set
        abstract showsVerticalScrollIndicator: bool option with get, set
        abstract style: ScrollViewStyle option with get, set

    and ScrollViewProps =
        inherit ScrollViewProperties
        inherit React.Props<ScrollViewStatic>


    and ScrollViewStatic =
        inherit React.ComponentClass<ScrollViewProps>


    and NativeScrollRectangle =
        abstract left: float with get, set
        abstract top: float with get, set
        abstract bottom: float with get, set
        abstract right: float with get, set

    and NativeScrollPoint =
        abstract x: float with get, set
        abstract y: float with get, set

    and NativeScrollSize =
        abstract height: float with get, set
        abstract width: float with get, set

    and NativeScrollEvent =
        abstract contentInset: NativeScrollRectangle with get, set
        abstract contentOffset: NativeScrollPoint with get, set
        abstract contentSize: NativeScrollSize with get, set
        abstract layoutMeasurement: NativeScrollSize with get, set
        abstract zoomScale: float with get, set

    and ActionSheetIOSOptions =
        abstract title: string option with get, set
        abstract options: ResizeArray<string> option with get, set
        abstract cancelButtonIndex: float option with get, set
        abstract destructiveButtonIndex: float option with get, set

    and ShareActionSheetIOSOptions =
        abstract message: string option with get, set
        abstract url: string option with get, set

    and ActionSheetIOSStatic =
        abstract showActionSheetWithOptions: Func<ActionSheetIOSOptions, Func<float, unit>, unit> with get, set
        abstract showShareActionSheetWithOptions: Func<ShareActionSheetIOSOptions, Func<Error, unit>, Func<bool, string, unit>, unit> with get, set

    and AdSupportIOSStatic =
        abstract getAdvertisingId: Func<Func<string, unit>, Func<Error, unit>, unit> with get, set
        abstract getAdvertisingTrackingEnabled: Func<Func<bool, unit>, Func<Error, unit>, unit> with get, set

    and AlertIOSButton =
        abstract text: string with get, set
        abstract onPress: Func<unit> option with get, set

    and AlertIOSStatic =
        abstract alert: Func<string, string, ResizeArray<AlertIOSButton>, string, unit> with get, set
        abstract prompt: Func<string, string, ResizeArray<AlertIOSButton>, Func<string, unit>, unit> with get, set

    and AppStateIOSStatic =
        abstract currentState: string with get, set
        abstract addEventListener: ``type``: string * listener: Func<string, unit> -> unit
        abstract removeEventListener: ``type``: string * listener: Func<string, unit> -> unit

    and AsyncStorageStatic =
        abstract getItem: key: string * ?callback: Func<Error, string, unit> -> Promise<string>
        abstract setItem: key: string * value: string * ?callback: Func<Error, unit> -> Promise<string>
        abstract removeItem: key: string * ?callback: Func<Error, unit> -> Promise<string>
        abstract mergeItem: key: string * value: string * ?callback: Func<Error, unit> -> Promise<string>
        abstract clear: ?callback: Func<Error, unit> -> Promise<string>
        abstract getAllKeys: ?callback: Func<Error, ResizeArray<string>, unit> -> Promise<string>
        abstract multiGet: keys: ResizeArray<string> * ?callback: Func<ResizeArray<Error>, ResizeArray<ResizeArray<string>>, unit> -> Promise<string>
        abstract multiSet: keyValuePairs: ResizeArray<ResizeArray<string>> * ?callback: Func<ResizeArray<Error>, unit> -> Promise<string>
        abstract multiRemove: keys: ResizeArray<string> * ?callback: Func<ResizeArray<Error>, unit> -> Promise<string>
        abstract multiMerge: keyValuePairs: ResizeArray<ResizeArray<string>> * ?callback: Func<ResizeArray<Error>, unit> -> Promise<string>

    and CameraRollFetchParams =
        abstract first: float with get, set
        abstract after: string option with get, set
        abstract groupTypes: string with get, set
        abstract groupName: string option with get, set
        abstract assetType: string option with get, set

    and CameraRollNodeInfo =
        abstract image: Image with get, set
        abstract group_name: string with get, set
        abstract timestamp: float with get, set
        abstract location: obj with get, set

    and CameraRollEdgeInfo =
        abstract node: CameraRollNodeInfo with get, set

    and CameraRollAssetInfo =
        abstract edges: ResizeArray<CameraRollEdgeInfo> with get, set
        abstract page_info: obj with get, set

    and CameraRollStatic =
        abstract GroupTypesOptions: ResizeArray<string> with get, set
        abstract saveImageWithTag: tag: string * successCallback: Func<string, unit> * errorCallback: Func<Error, unit> -> unit
        abstract getPhotos: fetch: CameraRollFetchParams * callback: Func<CameraRollAssetInfo, unit> * errorCallback: Func<Error, unit> -> unit

    and FetchableListenable<'T> =
        abstract fetch: Func<Promise<'T>> with get, set
        abstract addEventListener: Func<string, Func<'T, unit>, unit> with get, set
        abstract removeEventListener: Func<string, Func<'T, unit>, unit> with get, set

    and NetInfoStatic =
        inherit FetchableListenable<string>
        abstract isConnected: FetchableListenable<bool> with get, set
        abstract isConnectionMetered: obj with get, set

    and PanResponderGestureState =
        abstract stateID: float with get, set
        abstract moveX: float with get, set
        abstract moveY: float with get, set
        abstract x0: float with get, set
        abstract y0: float with get, set
        abstract dx: float with get, set
        abstract dy: float with get, set
        abstract vx: float with get, set
        abstract vy: float with get, set
        abstract numberActiveTouches: float with get, set
        abstract _accountsForMovesUpTo: float with get, set

    and PanResponderCallbacks =
        abstract onMoveShouldSetPanResponder: Func<GestureResponderEvent, PanResponderGestureState, bool> option with get, set
        abstract onStartShouldSetPanResponder: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderGrant: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderMove: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderRelease: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderTerminate: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onMoveShouldSetPanResponderCapture: Func<GestureResponderEvent, PanResponderGestureState, bool> option with get, set
        abstract onStartShouldSetPanResponderCapture: Func<GestureResponderEvent, PanResponderGestureState, bool> option with get, set
        abstract onPanResponderReject: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderStart: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderEnd: Func<GestureResponderEvent, PanResponderGestureState, unit> option with get, set
        abstract onPanResponderTerminationRequest: Func<GestureResponderEvent, PanResponderGestureState, bool> option with get, set

    and PanResponderInstance =
        abstract panHandlers: GestureResponderHandlers with get, set

    and PanResponderStatic =
        abstract create: config: PanResponderCallbacks -> PanResponderInstance

    and PushNotificationPermissions =
        abstract alert: bool option with get, set
        abstract badge: bool option with get, set
        abstract sound: bool option with get, set

    and PushNotification =
        abstract getMessage: unit -> U2<string, obj>
        abstract getSound: unit -> string
        abstract getAlert: unit -> U2<string, obj>
        abstract getBadgeCount: unit -> float
        abstract getData: unit -> obj

    and PushNotificationIOSStatic =
        abstract setApplicationIconBadgeNumber: number: float -> unit
        abstract getApplicationIconBadgeNumber: callback: Func<float, unit> -> unit
        abstract addEventListener: ``type``: string * handler: Func<PushNotification, unit> -> unit
        abstract requestPermissions: unit -> unit
        abstract checkPermissions: callback: Func<PushNotificationPermissions, unit> -> unit
        abstract removeEventListener: ``type``: string * handler: Func<PushNotification, unit> -> unit
        abstract popInitialNotification: unit -> PushNotification

    and StatusBarStyle =
        string

    and StatusBarAnimation =
        string

    and StatusBarIOSStatic =
        abstract setStyle: style: StatusBarStyle * ?animated: bool -> unit
        abstract setHidden: hidden: bool * ?animation: StatusBarAnimation -> unit
        abstract setNetworkActivityIndicatorVisible: visible: bool -> unit

    and VibrationIOSStatic =
        abstract vibrate: unit -> unit

    and ActivityIndicatorIOS =
        ActivityIndicatorIOSStatic

    and DatePickerIOS =
        DatePickerIOSStatic

    and Image =
        ImageStatic

    and LayoutAnimation =
        LayoutAnimationStatic

    and ListView =
        ListViewStatic

    and MapView =
        MapViewStatic

    and Navigator =
        NavigatorStatic

    and NavigatorIOS =
        NavigatorIOSStatic

    and PickerIOS =
        PickerIOSStatic

    and SliderIOS =
        SliderIOSStatic

    and ScrollView =
        ScrollViewStatic

    and StyleSheet =
        StyleSheetStatic

    and SwitchIOS =
        SwitchIOSStatic

    and TabBarIOS =
        TabBarIOSStatic

    and Text =
        TextStatic

    and TextInput =
        TextInputStatic

    and TouchableHighlight =
        TouchableHighlightStatic

    and TouchableNativeFeedback =
        TouchableNativeFeedbackStatic

    and TouchableOpacity =
        TouchableOpacityStatic

    and TouchableWithoutFeedback =
        TouchableWithoutFeedbackStatic

    and View =
        ViewStatic

    and WebView =
        WebViewStatic

    and ActionSheetIOS =
        ActionSheetIOSStatic

    and AdSupportIOS =
        AdSupportIOSStatic

    and AlertIOS =
        AlertIOSStatic

    and AppStateIOS =
        AppStateIOSStatic

    and AsyncStorage =
        AsyncStorageStatic

    and CameraRoll =
        CameraRollStatic

    and NetInfo =
        NetInfoStatic

    and PanResponder =
        PanResponderStatic

    and PushNotificationIOS =
        PushNotificationIOSStatic

    and StatusBarIOS =
        StatusBarIOSStatic

    and VibrationIOS =
        VibrationIOSStatic

    and DeviceEventSubscription =
        DeviceEventSubscriptionStatic

    and GlobalStatic =
        abstract requestAnimationFrame: fn: Func<unit> -> unit

    type [<Import("*", "react-native")>] Globals =
        static member Promise with get(): PromiseConstructor = failwith "JS only" and set(v: PromiseConstructor): unit = failwith "JS only"
        static member ActivityIndicatorIOS with get(): ActivityIndicatorIOSStatic = failwith "JS only" and set(v: ActivityIndicatorIOSStatic): unit = failwith "JS only"
        static member DatePickerIOS with get(): DatePickerIOSStatic = failwith "JS only" and set(v: DatePickerIOSStatic): unit = failwith "JS only"
        static member Image with get(): ImageStatic = failwith "JS only" and set(v: ImageStatic): unit = failwith "JS only"
        static member LayoutAnimation with get(): LayoutAnimationStatic = failwith "JS only" and set(v: LayoutAnimationStatic): unit = failwith "JS only"
        static member ListView with get(): ListViewStatic = failwith "JS only" and set(v: ListViewStatic): unit = failwith "JS only"
        static member MapView with get(): MapViewStatic = failwith "JS only" and set(v: MapViewStatic): unit = failwith "JS only"
        static member Navigator with get(): NavigatorStatic = failwith "JS only" and set(v: NavigatorStatic): unit = failwith "JS only"
        static member NavigatorIOS with get(): NavigatorIOSStatic = failwith "JS only" and set(v: NavigatorIOSStatic): unit = failwith "JS only"
        static member PickerIOS with get(): PickerIOSStatic = failwith "JS only" and set(v: PickerIOSStatic): unit = failwith "JS only"
        static member SliderIOS with get(): SliderIOSStatic = failwith "JS only" and set(v: SliderIOSStatic): unit = failwith "JS only"
        static member ScrollView with get(): ScrollViewStatic = failwith "JS only" and set(v: ScrollViewStatic): unit = failwith "JS only"
        static member StyleSheet with get(): StyleSheetStatic = failwith "JS only" and set(v: StyleSheetStatic): unit = failwith "JS only"
        static member SwitchIOS with get(): SwitchIOSStatic = failwith "JS only" and set(v: SwitchIOSStatic): unit = failwith "JS only"
        static member TabBarIOS with get(): TabBarIOSStatic = failwith "JS only" and set(v: TabBarIOSStatic): unit = failwith "JS only"
        static member Text with get(): TextStatic = failwith "JS only" and set(v: TextStatic): unit = failwith "JS only"
        static member TextInput with get(): TextInputStatic = failwith "JS only" and set(v: TextInputStatic): unit = failwith "JS only"
        static member TouchableHighlight with get(): TouchableHighlightStatic = failwith "JS only" and set(v: TouchableHighlightStatic): unit = failwith "JS only"
        static member TouchableNativeFeedback with get(): TouchableNativeFeedbackStatic = failwith "JS only" and set(v: TouchableNativeFeedbackStatic): unit = failwith "JS only"
        static member TouchableOpacity with get(): TouchableOpacityStatic = failwith "JS only" and set(v: TouchableOpacityStatic): unit = failwith "JS only"
        static member TouchableWithoutFeedback with get(): TouchableWithoutFeedbackStatic = failwith "JS only" and set(v: TouchableWithoutFeedbackStatic): unit = failwith "JS only"
        static member View with get(): ViewStatic = failwith "JS only" and set(v: ViewStatic): unit = failwith "JS only"
        static member WebView with get(): WebViewStatic = failwith "JS only" and set(v: WebViewStatic): unit = failwith "JS only"
        static member ActionSheetIOS with get(): ActionSheetIOSStatic = failwith "JS only" and set(v: ActionSheetIOSStatic): unit = failwith "JS only"
        static member AdSupportIOS with get(): AdSupportIOSStatic = failwith "JS only" and set(v: AdSupportIOSStatic): unit = failwith "JS only"
        static member AlertIOS with get(): AlertIOSStatic = failwith "JS only" and set(v: AlertIOSStatic): unit = failwith "JS only"
        static member AppStateIOS with get(): AppStateIOSStatic = failwith "JS only" and set(v: AppStateIOSStatic): unit = failwith "JS only"
        static member AsyncStorage with get(): AsyncStorageStatic = failwith "JS only" and set(v: AsyncStorageStatic): unit = failwith "JS only"
        static member CameraRoll with get(): CameraRollStatic = failwith "JS only" and set(v: CameraRollStatic): unit = failwith "JS only"
        static member NetInfo with get(): NetInfoStatic = failwith "JS only" and set(v: NetInfoStatic): unit = failwith "JS only"
        static member PanResponder with get(): PanResponderStatic = failwith "JS only" and set(v: PanResponderStatic): unit = failwith "JS only"
        static member PushNotificationIOS with get(): PushNotificationIOSStatic = failwith "JS only" and set(v: PushNotificationIOSStatic): unit = failwith "JS only"
        static member StatusBarIOS with get(): StatusBarIOSStatic = failwith "JS only" and set(v: StatusBarIOSStatic): unit = failwith "JS only"
        static member VibrationIOS with get(): VibrationIOSStatic = failwith "JS only" and set(v: VibrationIOSStatic): unit = failwith "JS only"
        static member SegmentedControlIOS with get(): React.ComponentClass<SegmentedControlIOSProperties> = failwith "JS only" and set(v: React.ComponentClass<SegmentedControlIOSProperties>): unit = failwith "JS only"
        static member PixelRatio with get(): PixelRatioStatic = failwith "JS only" and set(v: PixelRatioStatic): unit = failwith "JS only"
        static member DeviceEventEmitter with get(): DeviceEventEmitterStatic = failwith "JS only" and set(v: DeviceEventEmitterStatic): unit = failwith "JS only"
        static member DeviceEventSubscription with get(): DeviceEventSubscriptionStatic = failwith "JS only" and set(v: DeviceEventSubscriptionStatic): unit = failwith "JS only"
        static member InteractionManager with get(): InteractionManagerStatic = failwith "JS only" and set(v: InteractionManagerStatic): unit = failwith "JS only"
        static member createElement(``type``: React.ReactType, props: 'P, [<ParamArray>] children: React.ReactNode[]): React.ReactElement<'P> = failwith "JS only"
        static member ___spread(target: obj, [<ParamArray>] sources: obj[]): obj = failwith "JS only"

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NavigatorStatic =
        type NavState =
            abstract routeStack: ResizeArray<Route> with get, set
            abstract idStack: ResizeArray<float> with get, set
            abstract presentedIndex: float with get, set

        and NavigationBarStyle =
            interface end

        and NavigationBarRouteMapper =
            abstract Title: Func<Route, Navigator, float, NavState, React.ReactElement<obj>> with get, set
            abstract LeftButton: Func<Route, Navigator, float, NavState, React.ReactElement<obj>> with get, set
            abstract RightButton: Func<Route, Navigator, float, NavState, React.ReactElement<obj>> with get, set

        and NavigationBarProperties =
            inherit React.Props<NavigationBarStatic>
            abstract navigator: Navigator option with get, set
            abstract routeMapper: NavigationBarRouteMapper option with get, set
            abstract navState: NavState option with get, set
            abstract style: ViewStyle option with get, set

        and NavigationBarStatic =
            inherit React.ComponentClass<NavigationBarProperties>
            abstract Styles: NavigationBarStyle with get, set

        and NavigationBar =
            NavigationBarStatic

        and BreadcrumbNavigationBarStyle =
            interface end

        and BreadcrumbNavigationBarRouteMapper =
            abstract rightContentForRoute: Func<Route, Navigator, React.ReactElement<obj>> with get, set
            abstract titleContentForRoute: Func<Route, Navigator, React.ReactElement<obj>> with get, set
            abstract iconForRoute: Func<Route, Navigator, React.ReactElement<obj>> with get, set
            abstract separatorForRoute: Func<Route, Navigator, React.ReactElement<obj>> with get, set

        and BreadcrumbNavigationBarProperties =
            inherit React.Props<BreadcrumbNavigationBarStatic>
            abstract navigator: Navigator option with get, set
            abstract routeMapper: BreadcrumbNavigationBarRouteMapper option with get, set
            abstract navState: NavState option with get, set
            abstract style: ViewStyle option with get, set

        and BreadcrumbNavigationBarStatic =
            inherit React.ComponentClass<BreadcrumbNavigationBarProperties>
            abstract Styles: BreadcrumbNavigationBarStyle with get, set

        and BreadcrumbNavigationBar =
            BreadcrumbNavigationBarStatic

        type [<Import("NavigatorStatic", "react-native")>] Globals =
            static member NavigationBar with get(): NavigationBarStatic = failwith "JS only" and set(v: NavigationBarStatic): unit = failwith "JS only"
            static member BreadcrumbNavigationBar with get(): BreadcrumbNavigationBarStatic = failwith "JS only" and set(v: BreadcrumbNavigationBarStatic): unit = failwith "JS only"



    module addons =
        type TestModuleStatic =
            abstract verifySnapshot: Func<Func<obj, unit>, unit> with get, set
            abstract markTestPassed: Func<obj, unit> with get, set
            abstract markTestCompleted: Func<unit> with get, set

        and TestModule =
            TestModuleStatic

        type [<Import("addons", "react-native")>] Globals =
            static member TestModule with get(): TestModuleStatic = failwith "JS only" and set(v: TestModuleStatic): unit = failwith "JS only"



module Dimensions =
    type Dimensions =
        abstract get: what: string -> ReactNative.ScaledSize

    type [<Import("*","Dimensions")>] Globals =
        static member ExportDimensions with get(): Dimensions = failwith "JS only" and set(v: Dimensions): unit = failwith "JS only"

type Globals =
    [<Global>] static member ``global`` with get(): ReactNative.GlobalStatic = failwith "JS only" and set(v: ReactNative.GlobalStatic): unit = failwith "JS only"

