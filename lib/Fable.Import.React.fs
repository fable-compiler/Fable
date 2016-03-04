namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser
    
module React =
    type ReactType =
        U3<string, ComponentClass<obj>, StatelessComponent<obj>>

    and ReactElement<'P> =
        abstract ``type``: U3<string, ComponentClass<'P>, StatelessComponent<'P>> with get, set
        abstract props: 'P with get, set
        abstract key: U2<string, float> with get, set
        abstract ref: U2<string, Func<U2<Component<'P, obj>, Element>, obj>> with get, set

    and ClassicElement<'P> =
        inherit ReactElement<'P>
        abstract ``type``: ClassicComponentClass<'P> with get, set
        abstract ref: U2<string, Func<ClassicComponent<'P, obj>, obj>> with get, set

    and DOMElement<'P> =
        inherit ReactElement<'P>
        abstract ``type``: string with get, set
        abstract ref: U2<string, Func<Element, obj>> with get, set

    and ReactHTMLElement =
        inherit DOMElement<HTMLProps<HTMLElement>>
        abstract ref: U2<string, Func<HTMLElement, obj>> with get, set

    and ReactSVGElement =
        inherit DOMElement<SVGProps>
        abstract ref: U2<string, Func<SVGElement, obj>> with get, set

    and Factory<'P> =
        interface end

    and ClassicFactory<'P> =
        inherit Factory<'P>

    and DOMFactory<'P> =
        inherit Factory<'P>

    and HTMLFactory =
        DOMFactory<HTMLProps<HTMLElement>>

    and SVGFactory =
        DOMFactory<SVGProps>

    and ReactText =
        U2<string, float>

    and ReactChild =
        U2<ReactElement<obj>, ReactText>

    and ReactFragment =
        U2<obj, ResizeArray<U3<ReactChild, ResizeArray<obj>, bool>>>

    and ReactNode =
        U3<ReactChild, ReactFragment, bool>

    and ReactInstance =
        U2<Component<obj, obj>, Element>

    and [<Import("react?get=Component")>] Component<'P, 'S>(?props: 'P, ?context: obj) =
        interface ComponentLifecycle<'P, 'S> with
            member __.componentWillMount(): unit = failwith "JS only"
            member __.componentDidMount(): unit = failwith "JS only"
            member __.componentWillReceiveProps(nextProps: 'P, nextContext: obj): unit = failwith "JS only"
            member __.shouldComponentUpdate(nextProps: 'P, nextState: 'S, nextContext: obj): bool = failwith "JS only"
            member __.componentWillUpdate(nextProps: 'P, nextState: 'S, nextContext: obj): unit = failwith "JS only"
            member __.componentDidUpdate(prevProps: 'P, prevState: 'S, prevContext: obj): unit = failwith "JS only"
            member __.componentWillUnmount(): unit = failwith "JS only"
        member __.props with get(): 'P = failwith "JS only" and set(v: 'P): unit = failwith "JS only"
        member __.state with get(): 'S = failwith "JS only" and set(v: 'S): unit = failwith "JS only"
        member __.context with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.refs with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        member __.setState(f: Func<'S, 'P, 'S>, ?callback: Func<obj>): unit = failwith "JS only"
        member __.setState(state: 'S, ?callback: Func<obj>): unit = failwith "JS only"
        member __.forceUpdate(?callBack: Func<obj>): unit = failwith "JS only"
        member __.render(): ReactElement<'P> = failwith "JS only"

    and [<AbstractClass; Erase>] ClassicComponent<'P, 'S>() =
        inherit Component<'P, 'S>()
        abstract replaceState: nextState: 'S * ?callback: Func<obj> -> unit
        abstract isMounted: unit -> bool
        abstract getInitialState: unit -> 'S

    and ChildContextProvider<'CC> =
        abstract getChildContext: unit -> 'CC

    and StatelessComponent<'P> =
        abstract propTypes: ValidationMap<'P> option with get, set
        abstract contextTypes: ValidationMap<obj> option with get, set
        abstract defaultProps: 'P option with get, set
        abstract displayName: string option with get, set

    and ComponentClass<'P> =
        abstract propTypes: ValidationMap<'P> option with get, set
        abstract contextTypes: ValidationMap<obj> option with get, set
        abstract childContextTypes: ValidationMap<obj> option with get, set
        abstract defaultProps: 'P option with get, set
        [<Emit("new $0($1...)")>] abstract createNew: ?props: 'P * ?context: obj -> Component<'P, obj>

    and ClassicComponentClass<'P> =
        inherit ComponentClass<'P>
        abstract displayName: string option with get, set
        [<Emit("new $0($1...)")>] abstract createNew: ?props: 'P * ?context: obj -> ClassicComponent<'P, obj>
        abstract getDefaultProps: unit -> 'P

    and ComponentLifecycle<'P, 'S> =
        abstract componentWillMount: unit -> unit
        abstract componentDidMount: unit -> unit
        abstract componentWillReceiveProps: nextProps: 'P * nextContext: obj -> unit
        abstract shouldComponentUpdate: nextProps: 'P * nextState: 'S * nextContext: obj -> bool
        abstract componentWillUpdate: nextProps: 'P * nextState: 'S * nextContext: obj -> unit
        abstract componentDidUpdate: prevProps: 'P * prevState: 'S * prevContext: obj -> unit
        abstract componentWillUnmount: unit -> unit

    and Mixin<'P, 'S> =
        inherit ComponentLifecycle<'P, 'S>
        abstract mixins: Mixin<'P, 'S> option with get, set
        abstract statics: obj option with get, set
        abstract displayName: string option with get, set
        abstract propTypes: ValidationMap<obj> option with get, set
        abstract contextTypes: ValidationMap<obj> option with get, set
        abstract childContextTypes: ValidationMap<obj> option with get, set
        abstract getDefaultProps: unit -> 'P
        abstract getInitialState: unit -> 'S

    and ComponentSpec<'P, 'S> =
        inherit Mixin<'P, 'S>
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: propertyName: string -> obj with get, set
        abstract render: unit -> ReactElement<obj>

    and SyntheticEvent =
        abstract bubbles: bool with get, set
        abstract cancelable: bool with get, set
        abstract currentTarget: EventTarget with get, set
        abstract defaultPrevented: bool with get, set
        abstract eventPhase: float with get, set
        abstract isTrusted: bool with get, set
        abstract nativeEvent: Event with get, set
        abstract target: EventTarget with get, set
        abstract timeStamp: DateTime with get, set
        abstract ``type``: string with get, set
        abstract preventDefault: unit -> unit
        abstract stopPropagation: unit -> unit

    and ClipboardEvent =
        inherit SyntheticEvent
        abstract clipboardData: DataTransfer with get, set

    and CompositionEvent =
        inherit SyntheticEvent
        abstract data: string with get, set

    and DragEvent =
        inherit SyntheticEvent
        abstract dataTransfer: DataTransfer with get, set

    and FocusEvent =
        inherit SyntheticEvent
        abstract relatedTarget: EventTarget with get, set

    and FormEvent =
        inherit SyntheticEvent

    and KeyboardEvent =
        inherit SyntheticEvent
        abstract altKey: bool with get, set
        abstract charCode: float with get, set
        abstract ctrlKey: bool with get, set
        abstract key: string with get, set
        abstract keyCode: float with get, set
        abstract locale: string with get, set
        abstract location: float with get, set
        abstract metaKey: bool with get, set
        abstract repeat: bool with get, set
        abstract shiftKey: bool with get, set
        abstract which: float with get, set
        abstract getModifierState: key: string -> bool

    and MouseEvent =
        inherit SyntheticEvent
        abstract altKey: bool with get, set
        abstract button: float with get, set
        abstract buttons: float with get, set
        abstract clientX: float with get, set
        abstract clientY: float with get, set
        abstract ctrlKey: bool with get, set
        abstract metaKey: bool with get, set
        abstract pageX: float with get, set
        abstract pageY: float with get, set
        abstract relatedTarget: EventTarget with get, set
        abstract screenX: float with get, set
        abstract screenY: float with get, set
        abstract shiftKey: bool with get, set
        abstract getModifierState: key: string -> bool

    and TouchEvent =
        inherit SyntheticEvent
        abstract altKey: bool with get, set
        abstract changedTouches: TouchList with get, set
        abstract ctrlKey: bool with get, set
        abstract metaKey: bool with get, set
        abstract shiftKey: bool with get, set
        abstract targetTouches: TouchList with get, set
        abstract touches: TouchList with get, set
        abstract getModifierState: key: string -> bool

    and UIEvent =
        inherit SyntheticEvent
        abstract detail: float with get, set
        abstract view: AbstractView with get, set

    and WheelEvent =
        inherit SyntheticEvent
        abstract deltaMode: float with get, set
        abstract deltaX: float with get, set
        abstract deltaY: float with get, set
        abstract deltaZ: float with get, set

    and EventHandler<'E> =
        interface end

    and ReactEventHandler =
        EventHandler<SyntheticEvent>

    and ClipboardEventHandler =
        EventHandler<ClipboardEvent>

    and CompositionEventHandler =
        EventHandler<CompositionEvent>

    and DragEventHandler =
        EventHandler<DragEvent>

    and FocusEventHandler =
        EventHandler<FocusEvent>

    and FormEventHandler =
        EventHandler<FormEvent>

    and KeyboardEventHandler =
        EventHandler<KeyboardEvent>

    and MouseEventHandler =
        EventHandler<MouseEvent>

    and TouchEventHandler =
        EventHandler<TouchEvent>

    and UIEventHandler =
        EventHandler<UIEvent>

    and WheelEventHandler =
        EventHandler<WheelEvent>

    and Props<'T> =
        abstract children: ReactNode option with get, set
        abstract key: U2<string, float> option with get, set
        abstract ref: U2<string, Func<'T, obj>> option with get, set

    and HTMLProps<'T> =
        inherit HTMLAttributes
        inherit Props<'T>

    and SVGProps =
        inherit SVGAttributes
        inherit Props<SVGElement>

    and DOMAttributes =
        abstract dangerouslySetInnerHTML: obj option with get, set
        abstract onCopy: ClipboardEventHandler option with get, set
        abstract onCut: ClipboardEventHandler option with get, set
        abstract onPaste: ClipboardEventHandler option with get, set
        abstract onCompositionEnd: CompositionEventHandler option with get, set
        abstract onCompositionStart: CompositionEventHandler option with get, set
        abstract onCompositionUpdate: CompositionEventHandler option with get, set
        abstract onFocus: FocusEventHandler option with get, set
        abstract onBlur: FocusEventHandler option with get, set
        abstract onChange: FormEventHandler option with get, set
        abstract onInput: FormEventHandler option with get, set
        abstract onSubmit: FormEventHandler option with get, set
        abstract onLoad: ReactEventHandler option with get, set
        abstract onError: ReactEventHandler option with get, set
        abstract onKeyDown: KeyboardEventHandler option with get, set
        abstract onKeyPress: KeyboardEventHandler option with get, set
        abstract onKeyUp: KeyboardEventHandler option with get, set
        abstract onAbort: ReactEventHandler option with get, set
        abstract onCanPlay: ReactEventHandler option with get, set
        abstract onCanPlayThrough: ReactEventHandler option with get, set
        abstract onDurationChange: ReactEventHandler option with get, set
        abstract onEmptied: ReactEventHandler option with get, set
        abstract onEncrypted: ReactEventHandler option with get, set
        abstract onEnded: ReactEventHandler option with get, set
        abstract onLoadedData: ReactEventHandler option with get, set
        abstract onLoadedMetadata: ReactEventHandler option with get, set
        abstract onLoadStart: ReactEventHandler option with get, set
        abstract onPause: ReactEventHandler option with get, set
        abstract onPlay: ReactEventHandler option with get, set
        abstract onPlaying: ReactEventHandler option with get, set
        abstract onProgress: ReactEventHandler option with get, set
        abstract onRateChange: ReactEventHandler option with get, set
        abstract onSeeked: ReactEventHandler option with get, set
        abstract onSeeking: ReactEventHandler option with get, set
        abstract onStalled: ReactEventHandler option with get, set
        abstract onSuspend: ReactEventHandler option with get, set
        abstract onTimeUpdate: ReactEventHandler option with get, set
        abstract onVolumeChange: ReactEventHandler option with get, set
        abstract onWaiting: ReactEventHandler option with get, set
        abstract onClick: MouseEventHandler option with get, set
        abstract onContextMenu: MouseEventHandler option with get, set
        abstract onDoubleClick: MouseEventHandler option with get, set
        abstract onDrag: DragEventHandler option with get, set
        abstract onDragEnd: DragEventHandler option with get, set
        abstract onDragEnter: DragEventHandler option with get, set
        abstract onDragExit: DragEventHandler option with get, set
        abstract onDragLeave: DragEventHandler option with get, set
        abstract onDragOver: DragEventHandler option with get, set
        abstract onDragStart: DragEventHandler option with get, set
        abstract onDrop: DragEventHandler option with get, set
        abstract onMouseDown: MouseEventHandler option with get, set
        abstract onMouseEnter: MouseEventHandler option with get, set
        abstract onMouseLeave: MouseEventHandler option with get, set
        abstract onMouseMove: MouseEventHandler option with get, set
        abstract onMouseOut: MouseEventHandler option with get, set
        abstract onMouseOver: MouseEventHandler option with get, set
        abstract onMouseUp: MouseEventHandler option with get, set
        abstract onSelect: ReactEventHandler option with get, set
        abstract onTouchCancel: TouchEventHandler option with get, set
        abstract onTouchEnd: TouchEventHandler option with get, set
        abstract onTouchMove: TouchEventHandler option with get, set
        abstract onTouchStart: TouchEventHandler option with get, set
        abstract onScroll: UIEventHandler option with get, set
        abstract onWheel: WheelEventHandler option with get, set

    and CSSProperties =
        abstract boxFlex: float option with get, set
        abstract boxFlexGroup: float option with get, set
        abstract columnCount: float option with get, set
        abstract flex: U2<float, string> option with get, set
        abstract flexGrow: float option with get, set
        abstract flexShrink: float option with get, set
        abstract fontWeight: U2<float, string> option with get, set
        abstract lineClamp: float option with get, set
        abstract lineHeight: U2<float, string> option with get, set
        abstract opacity: float option with get, set
        abstract order: float option with get, set
        abstract orphans: float option with get, set
        abstract widows: float option with get, set
        abstract zIndex: float option with get, set
        abstract zoom: float option with get, set
        abstract fontSize: U2<float, string> option with get, set
        abstract fillOpacity: float option with get, set
        abstract strokeOpacity: float option with get, set
        abstract strokeWidth: float option with get, set
        abstract alignContent: obj option with get, set
        abstract alignItems: obj option with get, set
        abstract alignSelf: obj option with get, set
        abstract alignmentAdjust: obj option with get, set
        abstract alignmentBaseline: obj option with get, set
        abstract animationDelay: obj option with get, set
        abstract animationDirection: obj option with get, set
        abstract animationIterationCount: obj option with get, set
        abstract animationName: obj option with get, set
        abstract animationPlayState: obj option with get, set
        abstract appearance: obj option with get, set
        abstract backfaceVisibility: obj option with get, set
        abstract backgroundBlendMode: obj option with get, set
        abstract backgroundColor: obj option with get, set
        abstract backgroundComposite: obj option with get, set
        abstract backgroundImage: obj option with get, set
        abstract backgroundOrigin: obj option with get, set
        abstract backgroundPositionX: obj option with get, set
        abstract backgroundRepeat: obj option with get, set
        abstract baselineShift: obj option with get, set
        abstract behavior: obj option with get, set
        abstract border: obj option with get, set
        abstract borderBottomLeftRadius: obj option with get, set
        abstract borderBottomRightRadius: obj option with get, set
        abstract borderBottomWidth: obj option with get, set
        abstract borderCollapse: obj option with get, set
        abstract borderColor: obj option with get, set
        abstract borderCornerShape: obj option with get, set
        abstract borderImageSource: obj option with get, set
        abstract borderImageWidth: obj option with get, set
        abstract borderLeft: obj option with get, set
        abstract borderLeftColor: obj option with get, set
        abstract borderLeftStyle: obj option with get, set
        abstract borderLeftWidth: obj option with get, set
        abstract borderRight: obj option with get, set
        abstract borderRightColor: obj option with get, set
        abstract borderRightStyle: obj option with get, set
        abstract borderRightWidth: obj option with get, set
        abstract borderSpacing: obj option with get, set
        abstract borderStyle: obj option with get, set
        abstract borderTop: obj option with get, set
        abstract borderTopColor: obj option with get, set
        abstract borderTopLeftRadius: obj option with get, set
        abstract borderTopRightRadius: obj option with get, set
        abstract borderTopStyle: obj option with get, set
        abstract borderTopWidth: obj option with get, set
        abstract borderWidth: obj option with get, set
        abstract bottom: obj option with get, set
        abstract boxAlign: obj option with get, set
        abstract boxDecorationBreak: obj option with get, set
        abstract boxDirection: obj option with get, set
        abstract boxLineProgression: obj option with get, set
        abstract boxLines: obj option with get, set
        abstract boxOrdinalGroup: obj option with get, set
        abstract breakAfter: obj option with get, set
        abstract breakBefore: obj option with get, set
        abstract breakInside: obj option with get, set
        abstract clear: obj option with get, set
        abstract clip: obj option with get, set
        abstract clipRule: obj option with get, set
        abstract color: obj option with get, set
        abstract columnFill: obj option with get, set
        abstract columnGap: obj option with get, set
        abstract columnRule: obj option with get, set
        abstract columnRuleColor: obj option with get, set
        abstract columnRuleWidth: obj option with get, set
        abstract columnSpan: obj option with get, set
        abstract columnWidth: obj option with get, set
        abstract columns: obj option with get, set
        abstract counterIncrement: obj option with get, set
        abstract counterReset: obj option with get, set
        abstract cue: obj option with get, set
        abstract cueAfter: obj option with get, set
        abstract direction: obj option with get, set
        abstract display: obj option with get, set
        abstract fill: obj option with get, set
        abstract fillRule: obj option with get, set
        abstract filter: obj option with get, set
        abstract flexAlign: obj option with get, set
        abstract flexBasis: obj option with get, set
        abstract flexDirection: obj option with get, set
        abstract flexFlow: obj option with get, set
        abstract flexItemAlign: obj option with get, set
        abstract flexLinePack: obj option with get, set
        abstract flexOrder: obj option with get, set
        abstract float: obj option with get, set
        abstract flowFrom: obj option with get, set
        abstract font: obj option with get, set
        abstract fontFamily: obj option with get, set
        abstract fontKerning: obj option with get, set
        abstract fontSizeAdjust: obj option with get, set
        abstract fontStretch: obj option with get, set
        abstract fontStyle: obj option with get, set
        abstract fontSynthesis: obj option with get, set
        abstract fontVariant: obj option with get, set
        abstract fontVariantAlternates: obj option with get, set
        abstract gridArea: obj option with get, set
        abstract gridColumn: obj option with get, set
        abstract gridColumnEnd: obj option with get, set
        abstract gridColumnStart: obj option with get, set
        abstract gridRow: obj option with get, set
        abstract gridRowEnd: obj option with get, set
        abstract gridRowPosition: obj option with get, set
        abstract gridRowSpan: obj option with get, set
        abstract gridTemplateAreas: obj option with get, set
        abstract gridTemplateColumns: obj option with get, set
        abstract gridTemplateRows: obj option with get, set
        abstract height: obj option with get, set
        abstract hyphenateLimitChars: obj option with get, set
        abstract hyphenateLimitLines: obj option with get, set
        abstract hyphenateLimitZone: obj option with get, set
        abstract hyphens: obj option with get, set
        abstract imeMode: obj option with get, set
        abstract layoutGrid: obj option with get, set
        abstract layoutGridChar: obj option with get, set
        abstract layoutGridLine: obj option with get, set
        abstract layoutGridMode: obj option with get, set
        abstract layoutGridType: obj option with get, set
        abstract left: obj option with get, set
        abstract letterSpacing: obj option with get, set
        abstract lineBreak: obj option with get, set
        abstract listStyle: obj option with get, set
        abstract listStyleImage: obj option with get, set
        abstract listStylePosition: obj option with get, set
        abstract listStyleType: obj option with get, set
        abstract margin: obj option with get, set
        abstract marginBottom: obj option with get, set
        abstract marginLeft: obj option with get, set
        abstract marginRight: obj option with get, set
        abstract marginTop: obj option with get, set
        abstract marqueeDirection: obj option with get, set
        abstract marqueeStyle: obj option with get, set
        abstract mask: obj option with get, set
        abstract maskBorder: obj option with get, set
        abstract maskBorderRepeat: obj option with get, set
        abstract maskBorderSlice: obj option with get, set
        abstract maskBorderSource: obj option with get, set
        abstract maskBorderWidth: obj option with get, set
        abstract maskClip: obj option with get, set
        abstract maskOrigin: obj option with get, set
        abstract maxFontSize: obj option with get, set
        abstract maxHeight: obj option with get, set
        abstract maxWidth: obj option with get, set
        abstract minHeight: obj option with get, set
        abstract minWidth: obj option with get, set
        abstract outline: obj option with get, set
        abstract outlineColor: obj option with get, set
        abstract outlineOffset: obj option with get, set
        abstract overflow: obj option with get, set
        abstract overflowStyle: obj option with get, set
        abstract overflowX: obj option with get, set
        abstract padding: obj option with get, set
        abstract paddingBottom: obj option with get, set
        abstract paddingLeft: obj option with get, set
        abstract paddingRight: obj option with get, set
        abstract paddingTop: obj option with get, set
        abstract pageBreakAfter: obj option with get, set
        abstract pageBreakBefore: obj option with get, set
        abstract pageBreakInside: obj option with get, set
        abstract pause: obj option with get, set
        abstract pauseAfter: obj option with get, set
        abstract pauseBefore: obj option with get, set
        abstract perspective: obj option with get, set
        abstract perspectiveOrigin: obj option with get, set
        abstract pointerEvents: obj option with get, set
        abstract position: obj option with get, set
        abstract punctuationTrim: obj option with get, set
        abstract quotes: obj option with get, set
        abstract regionFragment: obj option with get, set
        abstract restAfter: obj option with get, set
        abstract restBefore: obj option with get, set
        abstract right: obj option with get, set
        abstract rubyAlign: obj option with get, set
        abstract rubyPosition: obj option with get, set
        abstract shapeImageThreshold: obj option with get, set
        abstract shapeInside: obj option with get, set
        abstract shapeMargin: obj option with get, set
        abstract shapeOutside: obj option with get, set
        abstract speak: obj option with get, set
        abstract speakAs: obj option with get, set
        abstract tabSize: obj option with get, set
        abstract tableLayout: obj option with get, set
        abstract textAlign: obj option with get, set
        abstract textAlignLast: obj option with get, set
        abstract textDecoration: obj option with get, set
        abstract textDecorationColor: obj option with get, set
        abstract textDecorationLine: obj option with get, set
        abstract textDecorationLineThrough: obj option with get, set
        abstract textDecorationNone: obj option with get, set
        abstract textDecorationOverline: obj option with get, set
        abstract textDecorationSkip: obj option with get, set
        abstract textDecorationStyle: obj option with get, set
        abstract textDecorationUnderline: obj option with get, set
        abstract textEmphasis: obj option with get, set
        abstract textEmphasisColor: obj option with get, set
        abstract textEmphasisStyle: obj option with get, set
        abstract textHeight: obj option with get, set
        abstract textIndent: obj option with get, set
        abstract textJustifyTrim: obj option with get, set
        abstract textKashidaSpace: obj option with get, set
        abstract textLineThrough: obj option with get, set
        abstract textLineThroughColor: obj option with get, set
        abstract textLineThroughMode: obj option with get, set
        abstract textLineThroughStyle: obj option with get, set
        abstract textLineThroughWidth: obj option with get, set
        abstract textOverflow: obj option with get, set
        abstract textOverline: obj option with get, set
        abstract textOverlineColor: obj option with get, set
        abstract textOverlineMode: obj option with get, set
        abstract textOverlineStyle: obj option with get, set
        abstract textOverlineWidth: obj option with get, set
        abstract textRendering: obj option with get, set
        abstract textScript: obj option with get, set
        abstract textShadow: obj option with get, set
        abstract textTransform: obj option with get, set
        abstract textUnderlinePosition: obj option with get, set
        abstract textUnderlineStyle: obj option with get, set
        abstract top: obj option with get, set
        abstract touchAction: obj option with get, set
        abstract transform: obj option with get, set
        abstract transformOrigin: obj option with get, set
        abstract transformOriginZ: obj option with get, set
        abstract transformStyle: obj option with get, set
        abstract transition: obj option with get, set
        abstract transitionDelay: obj option with get, set
        abstract transitionDuration: obj option with get, set
        abstract transitionProperty: obj option with get, set
        abstract transitionTimingFunction: obj option with get, set
        abstract unicodeBidi: obj option with get, set
        abstract unicodeRange: obj option with get, set
        abstract userFocus: obj option with get, set
        abstract userInput: obj option with get, set
        abstract verticalAlign: obj option with get, set
        abstract visibility: obj option with get, set
        abstract voiceBalance: obj option with get, set
        abstract voiceDuration: obj option with get, set
        abstract voiceFamily: obj option with get, set
        abstract voicePitch: obj option with get, set
        abstract voiceRange: obj option with get, set
        abstract voiceRate: obj option with get, set
        abstract voiceStress: obj option with get, set
        abstract voiceVolume: obj option with get, set
        abstract whiteSpace: obj option with get, set
        abstract whiteSpaceTreatment: obj option with get, set
        abstract width: obj option with get, set
        abstract wordBreak: obj option with get, set
        abstract wordSpacing: obj option with get, set
        abstract wordWrap: obj option with get, set
        abstract wrapFlow: obj option with get, set
        abstract wrapMargin: obj option with get, set
        abstract wrapOption: obj option with get, set
        abstract writingMode: obj option with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: propertyName: string -> obj with get, set

    and HTMLAttributes =
        inherit DOMAttributes
        abstract defaultChecked: bool option with get, set
        abstract defaultValue: U2<string, ResizeArray<string>> option with get, set
        abstract accept: string option with get, set
        abstract acceptCharset: string option with get, set
        abstract accessKey: string option with get, set
        abstract action: string option with get, set
        abstract allowFullScreen: bool option with get, set
        abstract allowTransparency: bool option with get, set
        abstract alt: string option with get, set
        abstract async: bool option with get, set
        abstract autoComplete: string option with get, set
        abstract autoFocus: bool option with get, set
        abstract autoPlay: bool option with get, set
        abstract capture: bool option with get, set
        abstract cellPadding: U2<float, string> option with get, set
        abstract cellSpacing: U2<float, string> option with get, set
        abstract charSet: string option with get, set
        abstract challenge: string option with get, set
        abstract ``checked``: bool option with get, set
        abstract classID: string option with get, set
        abstract className: string option with get, set
        abstract cols: float option with get, set
        abstract colSpan: float option with get, set
        abstract content: string option with get, set
        abstract contentEditable: bool option with get, set
        abstract contextMenu: string option with get, set
        abstract controls: bool option with get, set
        abstract coords: string option with get, set
        abstract crossOrigin: string option with get, set
        abstract data: string option with get, set
        abstract dateTime: string option with get, set
        abstract ``default``: bool option with get, set
        abstract defer: bool option with get, set
        abstract dir: string option with get, set
        abstract disabled: bool option with get, set
        abstract download: obj option with get, set
        abstract draggable: bool option with get, set
        abstract encType: string option with get, set
        abstract form: string option with get, set
        abstract formAction: string option with get, set
        abstract formEncType: string option with get, set
        abstract formMethod: string option with get, set
        abstract formNoValidate: bool option with get, set
        abstract formTarget: string option with get, set
        abstract frameBorder: U2<float, string> option with get, set
        abstract headers: string option with get, set
        abstract height: U2<float, string> option with get, set
        abstract hidden: bool option with get, set
        abstract high: float option with get, set
        abstract href: string option with get, set
        abstract hrefLang: string option with get, set
        abstract htmlFor: string option with get, set
        abstract httpEquiv: string option with get, set
        abstract icon: string option with get, set
        abstract id: string option with get, set
        abstract inputMode: string option with get, set
        abstract integrity: string option with get, set
        abstract is: string option with get, set
        abstract keyParams: string option with get, set
        abstract keyType: string option with get, set
        abstract kind: string option with get, set
        abstract label: string option with get, set
        abstract lang: string option with get, set
        abstract list: string option with get, set
        abstract loop: bool option with get, set
        abstract low: float option with get, set
        abstract manifest: string option with get, set
        abstract marginHeight: float option with get, set
        abstract marginWidth: float option with get, set
        abstract max: U2<float, string> option with get, set
        abstract maxLength: float option with get, set
        abstract media: string option with get, set
        abstract mediaGroup: string option with get, set
        abstract ``method``: string option with get, set
        abstract min: U2<float, string> option with get, set
        abstract minLength: float option with get, set
        abstract multiple: bool option with get, set
        abstract muted: bool option with get, set
        abstract name: string option with get, set
        abstract noValidate: bool option with get, set
        abstract ``open``: bool option with get, set
        abstract optimum: float option with get, set
        abstract pattern: string option with get, set
        abstract placeholder: string option with get, set
        abstract poster: string option with get, set
        abstract preload: string option with get, set
        abstract radioGroup: string option with get, set
        abstract readOnly: bool option with get, set
        abstract rel: string option with get, set
        abstract required: bool option with get, set
        abstract role: string option with get, set
        abstract rows: float option with get, set
        abstract rowSpan: float option with get, set
        abstract sandbox: string option with get, set
        abstract scope: string option with get, set
        abstract scoped: bool option with get, set
        abstract scrolling: string option with get, set
        abstract seamless: bool option with get, set
        abstract selected: bool option with get, set
        abstract shape: string option with get, set
        abstract size: float option with get, set
        abstract sizes: string option with get, set
        abstract span: float option with get, set
        abstract spellCheck: bool option with get, set
        abstract src: string option with get, set
        abstract srcDoc: string option with get, set
        abstract srcLang: string option with get, set
        abstract srcSet: string option with get, set
        abstract start: float option with get, set
        abstract step: U2<float, string> option with get, set
        abstract style: CSSProperties option with get, set
        abstract summary: string option with get, set
        abstract tabIndex: float option with get, set
        abstract target: string option with get, set
        abstract title: string option with get, set
        abstract ``type``: string option with get, set
        abstract useMap: string option with get, set
        abstract value: U2<string, ResizeArray<string>> option with get, set
        abstract width: U2<float, string> option with get, set
        abstract wmode: string option with get, set
        abstract wrap: string option with get, set
        abstract about: string option with get, set
        abstract datatype: string option with get, set
        abstract inlist: obj option with get, set
        abstract prefix: string option with get, set
        abstract property: string option with get, set
        abstract resource: string option with get, set
        abstract typeof: string option with get, set
        abstract vocab: string option with get, set
        abstract autoCapitalize: string option with get, set
        abstract autoCorrect: string option with get, set
        abstract autoSave: string option with get, set
        abstract color: string option with get, set
        abstract itemProp: string option with get, set
        abstract itemScope: bool option with get, set
        abstract itemType: string option with get, set
        abstract itemID: string option with get, set
        abstract itemRef: string option with get, set
        abstract results: float option with get, set
        abstract security: string option with get, set
        abstract unselectable: bool option with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> obj with get, set

    and SVGAttributes =
        inherit HTMLAttributes
        abstract clipPath: string option with get, set
        abstract cx: U2<float, string> option with get, set
        abstract cy: U2<float, string> option with get, set
        abstract d: string option with get, set
        abstract dx: U2<float, string> option with get, set
        abstract dy: U2<float, string> option with get, set
        abstract fill: string option with get, set
        abstract fillOpacity: U2<float, string> option with get, set
        abstract fontFamily: string option with get, set
        abstract fontSize: U2<float, string> option with get, set
        abstract fx: U2<float, string> option with get, set
        abstract fy: U2<float, string> option with get, set
        abstract gradientTransform: string option with get, set
        abstract gradientUnits: string option with get, set
        abstract markerEnd: string option with get, set
        abstract markerMid: string option with get, set
        abstract markerStart: string option with get, set
        abstract offset: U2<float, string> option with get, set
        abstract opacity: U2<float, string> option with get, set
        abstract patternContentUnits: string option with get, set
        abstract patternUnits: string option with get, set
        abstract points: string option with get, set
        abstract preserveAspectRatio: string option with get, set
        abstract r: U2<float, string> option with get, set
        abstract rx: U2<float, string> option with get, set
        abstract ry: U2<float, string> option with get, set
        abstract spreadMethod: string option with get, set
        abstract stopColor: string option with get, set
        abstract stopOpacity: U2<float, string> option with get, set
        abstract stroke: string option with get, set
        abstract strokeDasharray: string option with get, set
        abstract strokeLinecap: string option with get, set
        abstract strokeMiterlimit: string option with get, set
        abstract strokeOpacity: U2<float, string> option with get, set
        abstract strokeWidth: U2<float, string> option with get, set
        abstract textAnchor: string option with get, set
        abstract transform: string option with get, set
        abstract version: string option with get, set
        abstract viewBox: string option with get, set
        abstract x1: U2<float, string> option with get, set
        abstract x2: U2<float, string> option with get, set
        abstract x: U2<float, string> option with get, set
        abstract xlinkActuate: string option with get, set
        abstract xlinkArcrole: string option with get, set
        abstract xlinkHref: string option with get, set
        abstract xlinkRole: string option with get, set
        abstract xlinkShow: string option with get, set
        abstract xlinkTitle: string option with get, set
        abstract xlinkType: string option with get, set
        abstract xmlBase: string option with get, set
        abstract xmlLang: string option with get, set
        abstract xmlSpace: string option with get, set
        abstract y1: U2<float, string> option with get, set
        abstract y2: U2<float, string> option with get, set
        abstract y: U2<float, string> option with get, set

    and ReactDOM =
        abstract a: HTMLFactory with get, set
        abstract abbr: HTMLFactory with get, set
        abstract address: HTMLFactory with get, set
        abstract area: HTMLFactory with get, set
        abstract article: HTMLFactory with get, set
        abstract aside: HTMLFactory with get, set
        abstract audio: HTMLFactory with get, set
        abstract b: HTMLFactory with get, set
        abstract ``base``: HTMLFactory with get, set
        abstract bdi: HTMLFactory with get, set
        abstract bdo: HTMLFactory with get, set
        abstract big: HTMLFactory with get, set
        abstract blockquote: HTMLFactory with get, set
        abstract body: HTMLFactory with get, set
        abstract br: HTMLFactory with get, set
        abstract button: HTMLFactory with get, set
        abstract canvas: HTMLFactory with get, set
        abstract caption: HTMLFactory with get, set
        abstract cite: HTMLFactory with get, set
        abstract code: HTMLFactory with get, set
        abstract col: HTMLFactory with get, set
        abstract colgroup: HTMLFactory with get, set
        abstract data: HTMLFactory with get, set
        abstract datalist: HTMLFactory with get, set
        abstract dd: HTMLFactory with get, set
        abstract del: HTMLFactory with get, set
        abstract details: HTMLFactory with get, set
        abstract dfn: HTMLFactory with get, set
        abstract dialog: HTMLFactory with get, set
        abstract div: HTMLFactory with get, set
        abstract dl: HTMLFactory with get, set
        abstract dt: HTMLFactory with get, set
        abstract em: HTMLFactory with get, set
        abstract embed: HTMLFactory with get, set
        abstract fieldset: HTMLFactory with get, set
        abstract figcaption: HTMLFactory with get, set
        abstract figure: HTMLFactory with get, set
        abstract footer: HTMLFactory with get, set
        abstract form: HTMLFactory with get, set
        abstract h1: HTMLFactory with get, set
        abstract h2: HTMLFactory with get, set
        abstract h3: HTMLFactory with get, set
        abstract h4: HTMLFactory with get, set
        abstract h5: HTMLFactory with get, set
        abstract h6: HTMLFactory with get, set
        abstract head: HTMLFactory with get, set
        abstract header: HTMLFactory with get, set
        abstract hgroup: HTMLFactory with get, set
        abstract hr: HTMLFactory with get, set
        abstract html: HTMLFactory with get, set
        abstract i: HTMLFactory with get, set
        abstract iframe: HTMLFactory with get, set
        abstract img: HTMLFactory with get, set
        abstract input: HTMLFactory with get, set
        abstract ins: HTMLFactory with get, set
        abstract kbd: HTMLFactory with get, set
        abstract keygen: HTMLFactory with get, set
        abstract label: HTMLFactory with get, set
        abstract legend: HTMLFactory with get, set
        abstract li: HTMLFactory with get, set
        abstract link: HTMLFactory with get, set
        abstract main: HTMLFactory with get, set
        abstract map: HTMLFactory with get, set
        abstract mark: HTMLFactory with get, set
        abstract menu: HTMLFactory with get, set
        abstract menuitem: HTMLFactory with get, set
        abstract meta: HTMLFactory with get, set
        abstract meter: HTMLFactory with get, set
        abstract nav: HTMLFactory with get, set
        abstract noscript: HTMLFactory with get, set
        abstract ``object``: HTMLFactory with get, set
        abstract ol: HTMLFactory with get, set
        abstract optgroup: HTMLFactory with get, set
        abstract option: HTMLFactory with get, set
        abstract output: HTMLFactory with get, set
        abstract p: HTMLFactory with get, set
        abstract param: HTMLFactory with get, set
        abstract picture: HTMLFactory with get, set
        abstract pre: HTMLFactory with get, set
        abstract progress: HTMLFactory with get, set
        abstract q: HTMLFactory with get, set
        abstract rp: HTMLFactory with get, set
        abstract rt: HTMLFactory with get, set
        abstract ruby: HTMLFactory with get, set
        abstract s: HTMLFactory with get, set
        abstract samp: HTMLFactory with get, set
        abstract script: HTMLFactory with get, set
        abstract section: HTMLFactory with get, set
        abstract select: HTMLFactory with get, set
        abstract small: HTMLFactory with get, set
        abstract source: HTMLFactory with get, set
        abstract span: HTMLFactory with get, set
        abstract strong: HTMLFactory with get, set
        abstract style: HTMLFactory with get, set
        abstract sub: HTMLFactory with get, set
        abstract summary: HTMLFactory with get, set
        abstract sup: HTMLFactory with get, set
        abstract table: HTMLFactory with get, set
        abstract tbody: HTMLFactory with get, set
        abstract td: HTMLFactory with get, set
        abstract textarea: HTMLFactory with get, set
        abstract tfoot: HTMLFactory with get, set
        abstract th: HTMLFactory with get, set
        abstract thead: HTMLFactory with get, set
        abstract time: HTMLFactory with get, set
        abstract title: HTMLFactory with get, set
        abstract tr: HTMLFactory with get, set
        abstract track: HTMLFactory with get, set
        abstract u: HTMLFactory with get, set
        abstract ul: HTMLFactory with get, set
        abstract var: HTMLFactory with get, set
        abstract video: HTMLFactory with get, set
        abstract wbr: HTMLFactory with get, set
        abstract svg: SVGFactory with get, set
        abstract circle: SVGFactory with get, set
        abstract defs: SVGFactory with get, set
        abstract ellipse: SVGFactory with get, set
        abstract g: SVGFactory with get, set
        abstract image: SVGFactory with get, set
        abstract line: SVGFactory with get, set
        abstract linearGradient: SVGFactory with get, set
        abstract mask: SVGFactory with get, set
        abstract path: SVGFactory with get, set
        abstract pattern: SVGFactory with get, set
        abstract polygon: SVGFactory with get, set
        abstract polyline: SVGFactory with get, set
        abstract radialGradient: SVGFactory with get, set
        abstract rect: SVGFactory with get, set
        abstract stop: SVGFactory with get, set
        abstract text: SVGFactory with get, set
        abstract tspan: SVGFactory with get, set

    and Validator<'T> =
        interface end

    and Requireable<'T> =
        inherit Validator<'T>
        abstract isRequired: Validator<'T> with get, set

    and ValidationMap<'T> =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> Validator<'T> with get, set

    and ReactPropTypes =
        abstract any: Requireable<obj> with get, set
        abstract array: Requireable<obj> with get, set
        abstract bool: Requireable<obj> with get, set
        abstract func: Requireable<obj> with get, set
        abstract number: Requireable<obj> with get, set
        abstract ``object``: Requireable<obj> with get, set
        abstract string: Requireable<obj> with get, set
        abstract node: Requireable<obj> with get, set
        abstract element: Requireable<obj> with get, set
        abstract instanceOf: expectedClass: obj -> Requireable<obj>
        abstract oneOf: types: ResizeArray<obj> -> Requireable<obj>
        abstract oneOfType: types: ResizeArray<Validator<obj>> -> Requireable<obj>
        abstract arrayOf: ``type``: Validator<obj> -> Requireable<obj>
        abstract objectOf: ``type``: Validator<obj> -> Requireable<obj>
        abstract shape: ``type``: ValidationMap<obj> -> Requireable<obj>

    and ReactChildren =
        abstract map: children: ReactNode * fn: Func<ReactChild, float, 'T> -> ResizeArray<'T>
        abstract forEach: children: ReactNode * fn: Func<ReactChild, float, obj> -> unit
        abstract count: children: ReactNode -> float
        abstract only: children: ReactNode -> ReactElement<obj>
        abstract toArray: children: ReactNode -> ResizeArray<ReactChild>

    and AbstractView =
        abstract styleMedia: StyleMedia with get, set
        abstract document: Document with get, set

    and Touch =
        abstract identifier: float with get, set
        abstract target: EventTarget with get, set
        abstract screenX: float with get, set
        abstract screenY: float with get, set
        abstract clientX: float with get, set
        abstract clientY: float with get, set
        abstract pageX: float with get, set
        abstract pageY: float with get, set

    and TouchList =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: float -> Touch with get, set
        abstract length: float with get, set
        abstract item: index: float -> Touch
        abstract identifiedTouch: identifier: float -> Touch

    type [<Erase>] Globals =
        abstract DOM: ReactDOM with get, set
        abstract PropTypes: ReactPropTypes with get, set
        abstract Children: ReactChildren with get, set
        abstract createClass: spec: ComponentSpec<'P, 'S> -> ClassicComponentClass<'P>
        abstract createFactory: ``type``: string -> DOMFactory<'P>
        abstract createFactory: ``type``: ClassicComponentClass<'P> -> ClassicFactory<'P>
        abstract createFactory: ``type``: U2<ComponentClass<'P>, StatelessComponent<'P>> -> Factory<'P>
        abstract createElement: ``type``: string * ?props: 'P * [<ParamArray>] children: ReactNode[] -> DOMElement<'P>
        abstract createElement: ``type``: ClassicComponentClass<'P> * ?props: 'P * [<ParamArray>] children: ReactNode[] -> ClassicElement<'P>
        abstract createElement: ``type``: U2<ComponentClass<'P>, StatelessComponent<'P>> * ?props: 'P * [<ParamArray>] children: ReactNode[] -> ReactElement<'P>
        abstract cloneElement: element: DOMElement<'P> * ?props: 'P * [<ParamArray>] children: ReactNode[] -> DOMElement<'P>
        abstract cloneElement: element: ClassicElement<'P> * ?props: 'P * [<ParamArray>] children: ReactNode[] -> ClassicElement<'P>
        abstract cloneElement: element: ReactElement<'P> * ?props: 'P * [<ParamArray>] children: ReactNode[] -> ReactElement<'P>
        abstract isValidElement: ``object``: obj -> bool

    let [<Import("react")>] Globals: Globals = failwith "JS only"


    module DOM =
        type [<Erase>] Globals =
            abstract version: string with get, set
            abstract findDOMNode: instance: ReactInstance -> 'E
            abstract findDOMNode: instance: ReactInstance -> Element
            abstract render: element: DOMElement<'P> * container: Element * ?callback: Func<Element, obj> -> Element
            abstract render: element: ClassicElement<'P> * container: Element * ?callback: Func<ClassicComponent<'P, 'S>, obj> -> ClassicComponent<'P, 'S>
            abstract render: element: ReactElement<'P> * container: Element * ?callback: Func<Component<'P, 'S>, obj> -> Component<'P, 'S>
            abstract unmountComponentAtNode: container: Element -> bool
            abstract unstable_batchedUpdates: callback: Func<'A, 'B, obj> * a: 'A * b: 'B -> unit
            abstract unstable_batchedUpdates: callback: Func<'A, obj> * a: 'A -> unit
            abstract unstable_batchedUpdates: callback: Func<obj> -> unit
            abstract unstable_renderSubtreeIntoContainer: parentComponent: Component<obj, obj> * nextElement: DOMElement<'P> * container: Element * ?callback: Func<Element, obj> -> Element
            abstract unstable_renderSubtreeIntoContainer: parentComponent: Component<obj, obj> * nextElement: ClassicElement<'P> * container: Element * ?callback: Func<ClassicComponent<'P, 'S>, obj> -> ClassicComponent<'P, 'S>
            abstract unstable_renderSubtreeIntoContainer: parentComponent: Component<obj, obj> * nextElement: ReactElement<'P> * container: Element * ?callback: Func<Component<'P, 'S>, obj> -> Component<'P, 'S>

        let [<Import("react-dom")>] Globals: Globals = failwith "JS only"


    module DOMServer =
        type [<Erase>] Globals =
            abstract version: string with get, set
            abstract renderToString<'a> : element: ReactElement<'a> -> string
            abstract renderToStaticMarkup<'a> : element: ReactElement<'a> -> string

        let [<Import("react-dom/server")>] Globals: Globals = failwith "JS only"


module JSX =
    type Element =
        inherit React.ReactElement<obj>

    and [<AbstractClass; Erase>] ElementClass() =
        inherit React.Component<obj, obj>()
        abstract render: unit -> Element

    and ElementAttributesProperty =
        abstract props: obj with get, set

    and IntrinsicAttributes =
        abstract key: U2<string, float> option with get, set

    and IntrinsicClassAttributes<'T> =
        abstract ref: U2<string, Func<'T, unit>> option with get, set

    and IntrinsicElements =
        abstract a: React.HTMLProps<HTMLAnchorElement> with get, set
        abstract abbr: React.HTMLProps<HTMLElement> with get, set
        abstract address: React.HTMLProps<HTMLElement> with get, set
        abstract area: React.HTMLProps<HTMLAreaElement> with get, set
        abstract article: React.HTMLProps<HTMLElement> with get, set
        abstract aside: React.HTMLProps<HTMLElement> with get, set
        abstract audio: React.HTMLProps<HTMLAudioElement> with get, set
        abstract b: React.HTMLProps<HTMLElement> with get, set
        abstract ``base``: React.HTMLProps<HTMLBaseElement> with get, set
        abstract bdi: React.HTMLProps<HTMLElement> with get, set
        abstract bdo: React.HTMLProps<HTMLElement> with get, set
        abstract big: React.HTMLProps<HTMLElement> with get, set
        abstract blockquote: React.HTMLProps<HTMLElement> with get, set
        abstract body: React.HTMLProps<HTMLBodyElement> with get, set
        abstract br: React.HTMLProps<HTMLBRElement> with get, set
        abstract button: React.HTMLProps<HTMLButtonElement> with get, set
        abstract canvas: React.HTMLProps<HTMLCanvasElement> with get, set
        abstract caption: React.HTMLProps<HTMLElement> with get, set
        abstract cite: React.HTMLProps<HTMLElement> with get, set
        abstract code: React.HTMLProps<HTMLElement> with get, set
        abstract col: React.HTMLProps<HTMLTableColElement> with get, set
        abstract colgroup: React.HTMLProps<HTMLTableColElement> with get, set
        abstract data: React.HTMLProps<HTMLElement> with get, set
        abstract datalist: React.HTMLProps<HTMLDataListElement> with get, set
        abstract dd: React.HTMLProps<HTMLElement> with get, set
        abstract del: React.HTMLProps<HTMLElement> with get, set
        abstract details: React.HTMLProps<HTMLElement> with get, set
        abstract dfn: React.HTMLProps<HTMLElement> with get, set
        abstract dialog: React.HTMLProps<HTMLElement> with get, set
        abstract div: React.HTMLProps<HTMLDivElement> with get, set
        abstract dl: React.HTMLProps<HTMLDListElement> with get, set
        abstract dt: React.HTMLProps<HTMLElement> with get, set
        abstract em: React.HTMLProps<HTMLElement> with get, set
        abstract embed: React.HTMLProps<HTMLEmbedElement> with get, set
        abstract fieldset: React.HTMLProps<HTMLFieldSetElement> with get, set
        abstract figcaption: React.HTMLProps<HTMLElement> with get, set
        abstract figure: React.HTMLProps<HTMLElement> with get, set
        abstract footer: React.HTMLProps<HTMLElement> with get, set
        abstract form: React.HTMLProps<HTMLFormElement> with get, set
        abstract h1: React.HTMLProps<HTMLHeadingElement> with get, set
        abstract h2: React.HTMLProps<HTMLHeadingElement> with get, set
        abstract h3: React.HTMLProps<HTMLHeadingElement> with get, set
        abstract h4: React.HTMLProps<HTMLHeadingElement> with get, set
        abstract h5: React.HTMLProps<HTMLHeadingElement> with get, set
        abstract h6: React.HTMLProps<HTMLHeadingElement> with get, set
        abstract head: React.HTMLProps<HTMLHeadElement> with get, set
        abstract header: React.HTMLProps<HTMLElement> with get, set
        abstract hgroup: React.HTMLProps<HTMLElement> with get, set
        abstract hr: React.HTMLProps<HTMLHRElement> with get, set
        abstract html: React.HTMLProps<HTMLHtmlElement> with get, set
        abstract i: React.HTMLProps<HTMLElement> with get, set
        abstract iframe: React.HTMLProps<HTMLIFrameElement> with get, set
        abstract img: React.HTMLProps<HTMLImageElement> with get, set
        abstract input: React.HTMLProps<HTMLInputElement> with get, set
        abstract ins: React.HTMLProps<HTMLModElement> with get, set
        abstract kbd: React.HTMLProps<HTMLElement> with get, set
        abstract keygen: React.HTMLProps<HTMLElement> with get, set
        abstract label: React.HTMLProps<HTMLLabelElement> with get, set
        abstract legend: React.HTMLProps<HTMLLegendElement> with get, set
        abstract li: React.HTMLProps<HTMLLIElement> with get, set
        abstract link: React.HTMLProps<HTMLLinkElement> with get, set
        abstract main: React.HTMLProps<HTMLElement> with get, set
        abstract map: React.HTMLProps<HTMLMapElement> with get, set
        abstract mark: React.HTMLProps<HTMLElement> with get, set
        abstract menu: React.HTMLProps<HTMLElement> with get, set
        abstract menuitem: React.HTMLProps<HTMLElement> with get, set
        abstract meta: React.HTMLProps<HTMLMetaElement> with get, set
        abstract meter: React.HTMLProps<HTMLElement> with get, set
        abstract nav: React.HTMLProps<HTMLElement> with get, set
        abstract noscript: React.HTMLProps<HTMLElement> with get, set
        abstract ``object``: React.HTMLProps<HTMLObjectElement> with get, set
        abstract ol: React.HTMLProps<HTMLOListElement> with get, set
        abstract optgroup: React.HTMLProps<HTMLOptGroupElement> with get, set
        abstract option: React.HTMLProps<HTMLOptionElement> with get, set
        abstract output: React.HTMLProps<HTMLElement> with get, set
        abstract p: React.HTMLProps<HTMLParagraphElement> with get, set
        abstract param: React.HTMLProps<HTMLParamElement> with get, set
        abstract picture: React.HTMLProps<HTMLElement> with get, set
        abstract pre: React.HTMLProps<HTMLPreElement> with get, set
        abstract progress: React.HTMLProps<HTMLProgressElement> with get, set
        abstract q: React.HTMLProps<HTMLQuoteElement> with get, set
        abstract rp: React.HTMLProps<HTMLElement> with get, set
        abstract rt: React.HTMLProps<HTMLElement> with get, set
        abstract ruby: React.HTMLProps<HTMLElement> with get, set
        abstract s: React.HTMLProps<HTMLElement> with get, set
        abstract samp: React.HTMLProps<HTMLElement> with get, set
        abstract script: React.HTMLProps<HTMLElement> with get, set
        abstract section: React.HTMLProps<HTMLElement> with get, set
        abstract select: React.HTMLProps<HTMLSelectElement> with get, set
        abstract small: React.HTMLProps<HTMLElement> with get, set
        abstract source: React.HTMLProps<HTMLSourceElement> with get, set
        abstract span: React.HTMLProps<HTMLSpanElement> with get, set
        abstract strong: React.HTMLProps<HTMLElement> with get, set
        abstract style: React.HTMLProps<HTMLStyleElement> with get, set
        abstract sub: React.HTMLProps<HTMLElement> with get, set
        abstract summary: React.HTMLProps<HTMLElement> with get, set
        abstract sup: React.HTMLProps<HTMLElement> with get, set
        abstract table: React.HTMLProps<HTMLTableElement> with get, set
        abstract tbody: React.HTMLProps<HTMLTableSectionElement> with get, set
        abstract td: React.HTMLProps<HTMLTableDataCellElement> with get, set
        abstract textarea: React.HTMLProps<HTMLTextAreaElement> with get, set
        abstract tfoot: React.HTMLProps<HTMLTableSectionElement> with get, set
        abstract th: React.HTMLProps<HTMLTableHeaderCellElement> with get, set
        abstract thead: React.HTMLProps<HTMLTableSectionElement> with get, set
        abstract time: React.HTMLProps<HTMLElement> with get, set
        abstract title: React.HTMLProps<HTMLTitleElement> with get, set
        abstract tr: React.HTMLProps<HTMLTableRowElement> with get, set
        abstract track: React.HTMLProps<HTMLTrackElement> with get, set
        abstract u: React.HTMLProps<HTMLElement> with get, set
        abstract ul: React.HTMLProps<HTMLUListElement> with get, set
        abstract var: React.HTMLProps<HTMLElement> with get, set
        abstract video: React.HTMLProps<HTMLVideoElement> with get, set
        abstract wbr: React.HTMLProps<HTMLElement> with get, set
        abstract svg: React.SVGProps with get, set
        abstract circle: React.SVGProps with get, set
        abstract clipPath: React.SVGProps with get, set
        abstract defs: React.SVGProps with get, set
        abstract ellipse: React.SVGProps with get, set
        abstract g: React.SVGProps with get, set
        abstract image: React.SVGProps with get, set
        abstract line: React.SVGProps with get, set
        abstract linearGradient: React.SVGProps with get, set
        abstract mask: React.SVGProps with get, set
        abstract path: React.SVGProps with get, set
        abstract pattern: React.SVGProps with get, set
        abstract polygon: React.SVGProps with get, set
        abstract polyline: React.SVGProps with get, set
        abstract radialGradient: React.SVGProps with get, set
        abstract rect: React.SVGProps with get, set
        abstract stop: React.SVGProps with get, set
        abstract text: React.SVGProps with get, set
        abstract tspan: React.SVGProps with get, set


