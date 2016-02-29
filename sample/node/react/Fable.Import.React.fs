namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

module React =
    type ReactType =
        U3<string, ComponentClass<obj>, StatelessComponent<obj>>

    and IReactElement<'P> =
        abstract ``type``: U3<string, ComponentClass<'P>, StatelessComponent<'P>> with get, set
        abstract props: 'P with get, set
        abstract key: U2<string, float> with get, set
        abstract ref: U2<string, Func<U2<Component<'P, obj>, Element>, obj>> with get, set

    and [<AbstractClass>] ReactElement<'P>() =
        interface IReactElement<'P> with
            member __.``type`` with get(): U3<string, ComponentClass<'P>, StatelessComponent<'P>> = failwith "JS only" and set(v: U3<string, ComponentClass<'P>, StatelessComponent<'P>>): unit = failwith "JS only"
            member __.props with get(): 'P = failwith "JS only" and set(v: 'P): unit = failwith "JS only"
            member __.key with get(): U2<string, float> = failwith "JS only" and set(v: U2<string, float>): unit = failwith "JS only"
            member __.ref with get(): U2<string, Func<U2<Component<'P, obj>, Element>, obj>> = failwith "JS only" and set(v: U2<string, Func<U2<Component<'P, obj>, Element>, obj>>): unit = failwith "JS only"

    and IClassicElement<'P> =
        inherit IReactElement<'P>
        abstract ``type``: ClassicComponentClass<'P> with get, set
        abstract ref: U2<string, Func<ClassicComponent<'P, obj>, obj>> with get, set

    and [<AbstractClass>] ClassicElement<'P>() =
        inherit ReactElement<'P>()
        interface IClassicElement<'P> with
            member __.``type`` with get(): ClassicComponentClass<'P> = failwith "JS only" and set(v: ClassicComponentClass<'P>): unit = failwith "JS only"
            member __.ref with get(): U2<string, Func<ClassicComponent<'P, obj>, obj>> = failwith "JS only" and set(v: U2<string, Func<ClassicComponent<'P, obj>, obj>>): unit = failwith "JS only"

    and IDOMElement<'P> =
        inherit IReactElement<'P>
        abstract ``type``: string with get, set
        abstract ref: U2<string, Func<Element, obj>> with get, set

    and [<AbstractClass>] DOMElement<'P>() =
        inherit ReactElement<'P>()
        interface IDOMElement<'P> with
            member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.ref with get(): U2<string, Func<Element, obj>> = failwith "JS only" and set(v: U2<string, Func<Element, obj>>): unit = failwith "JS only"

    and IReactHTMLElement =
        inherit IDOMElement<HTMLProps<HTMLElement>>
        abstract ref: U2<string, Func<HTMLElement, obj>> with get, set

    and [<AbstractClass>] ReactHTMLElement() =
        inherit DOMElement<HTMLProps<HTMLElement>>()
        interface IReactHTMLElement with
            member __.ref with get(): U2<string, Func<HTMLElement, obj>> = failwith "JS only" and set(v: U2<string, Func<HTMLElement, obj>>): unit = failwith "JS only"

    and IReactSVGElement =
        inherit IDOMElement<SVGProps>
        abstract ref: U2<string, Func<SVGElement, obj>> with get, set

    and [<AbstractClass>] ReactSVGElement() =
        inherit DOMElement<SVGProps>()
        interface IReactSVGElement with
            member __.ref with get(): U2<string, Func<SVGElement, obj>> = failwith "JS only" and set(v: U2<string, Func<SVGElement, obj>>): unit = failwith "JS only"

    and IFactory<'P> =
        interface end

    and [<AbstractClass>] Factory<'P>() =
        interface IFactory<'P>


    and IClassicFactory<'P> =
        inherit IFactory<'P>

    and [<AbstractClass>] ClassicFactory<'P>() =
        inherit Factory<'P>()
        interface IClassicFactory<'P>


    and IDOMFactory<'P> =
        inherit IFactory<'P>

    and [<AbstractClass>] DOMFactory<'P>() =
        inherit Factory<'P>()
        interface IDOMFactory<'P>


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

    and [<Import("React?get=Component")>] IComponent<'P, 'S> =
        inherit IComponentLifecycle<'P, 'S>
        abstract props: 'P with get, set
        abstract state: 'S with get, set
        abstract context: obj with get, set
        abstract refs: obj with get, set
        abstract setState: f: Func<'S, 'P, 'S> * ?callback: Func<obj> -> unit
        abstract setState: state: 'S * ?callback: Func<obj> -> unit
        abstract forceUpdate: ?callBack: Func<obj> -> unit
        abstract render: unit -> obj

    and [<Import("React?get=Component")>] Component<'P, 'S>() =
        inherit ComponentLifecycle<'P, 'S>()
        interface IComponent<'P, 'S> with
            member __.props with get(): 'P = failwith "JS only" and set(v: 'P): unit = failwith "JS only"
            member __.state with get(): 'S = failwith "JS only" and set(v: 'S): unit = failwith "JS only"
            member __.context with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.refs with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
            member __.setState(f: Func<'S, 'P, 'S>, ?callback: Func<obj>): unit = failwith "JS only"
            member __.setState(state: 'S, ?callback: Func<obj>): unit = failwith "JS only"
            member __.forceUpdate(?callBack: Func<obj>): unit = failwith "JS only"
            member __.render(): obj = failwith "JS only"

    and IClassicComponent<'P, 'S> =
        inherit IComponent<'P, 'S>
        abstract replaceState: nextState: 'S * ?callback: Func<obj> -> unit
        abstract isMounted: unit -> bool
        abstract getInitialState: unit -> 'S

    and [<AbstractClass>] ClassicComponent<'P, 'S>() =
        inherit Component<'P, 'S>()
        interface IClassicComponent<'P, 'S> with
            member __.replaceState(nextState: 'S, ?callback: Func<obj>): unit = failwith "JS only"
            member __.isMounted(): bool = failwith "JS only"
            member __.getInitialState(): 'S = failwith "JS only"

    and IChildContextProvider<'CC> =
        abstract getChildContext: unit -> 'CC

    and [<AbstractClass>] ChildContextProvider<'CC>() =
        interface IChildContextProvider<'CC> with
            member __.getChildContext(): 'CC = failwith "JS only"

    and IStatelessComponent<'P> =
        abstract propTypes: ValidationMap<'P> option with get, set
        abstract contextTypes: ValidationMap<obj> option with get, set
        abstract defaultProps: 'P option with get, set
        abstract displayName: string option with get, set

    and [<AbstractClass>] StatelessComponent<'P>() =
        interface IStatelessComponent<'P> with
            member __.propTypes with get(): ValidationMap<'P> option = failwith "JS only" and set(v: ValidationMap<'P> option): unit = failwith "JS only"
            member __.contextTypes with get(): ValidationMap<obj> option = failwith "JS only" and set(v: ValidationMap<obj> option): unit = failwith "JS only"
            member __.defaultProps with get(): 'P option = failwith "JS only" and set(v: 'P option): unit = failwith "JS only"
            member __.displayName with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"

    and IComponentClass<'P> =
        abstract createNew: ?props: 'P * ?context: obj -> Component<'P, obj>
        abstract propTypes: ValidationMap<'P> option with get, set
        abstract contextTypes: ValidationMap<obj> option with get, set
        abstract childContextTypes: ValidationMap<obj> option with get, set
        abstract defaultProps: 'P option with get, set

    and [<AbstractClass>] ComponentClass<'P>() =
        interface IComponentClass<'P> with
            member __.createNew(?props: 'P, ?context: obj): Component<'P, obj> = failwith "JS only"
            member __.propTypes with get(): ValidationMap<'P> option = failwith "JS only" and set(v: ValidationMap<'P> option): unit = failwith "JS only"
            member __.contextTypes with get(): ValidationMap<obj> option = failwith "JS only" and set(v: ValidationMap<obj> option): unit = failwith "JS only"
            member __.childContextTypes with get(): ValidationMap<obj> option = failwith "JS only" and set(v: ValidationMap<obj> option): unit = failwith "JS only"
            member __.defaultProps with get(): 'P option = failwith "JS only" and set(v: 'P option): unit = failwith "JS only"

    and IClassicComponentClass<'P> =
        inherit IComponentClass<'P>
        abstract createNew: ?props: 'P * ?context: obj -> ClassicComponent<'P, obj>
        abstract displayName: string option with get, set
        abstract getDefaultProps: unit -> 'P

    and [<AbstractClass>] ClassicComponentClass<'P>() =
        inherit ComponentClass<'P>()
        interface IClassicComponentClass<'P> with
            member __.createNew(?props: 'P, ?context: obj): ClassicComponent<'P, obj> = failwith "JS only"
            member __.displayName with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.getDefaultProps(): 'P = failwith "JS only"

    and IComponentLifecycle<'P, 'S> =
        abstract componentWillMount: unit -> unit
        abstract componentDidMount: unit -> unit
        abstract componentWillReceiveProps: nextProps: 'P * nextContext: obj -> unit
        abstract shouldComponentUpdate: nextProps: 'P * nextState: 'S * nextContext: obj -> bool
        abstract componentWillUpdate: nextProps: 'P * nextState: 'S * nextContext: obj -> unit
        abstract componentDidUpdate: prevProps: 'P * prevState: 'S * prevContext: obj -> unit
        abstract componentWillUnmount: unit -> unit

    and [<AbstractClass>] ComponentLifecycle<'P, 'S>() =
        interface IComponentLifecycle<'P, 'S> with
            member __.componentWillMount(): unit = failwith "JS only"
            member __.componentDidMount(): unit = failwith "JS only"
            member __.componentWillReceiveProps(nextProps: 'P, nextContext: obj): unit = failwith "JS only"
            member __.shouldComponentUpdate(nextProps: 'P, nextState: 'S, nextContext: obj): bool = failwith "JS only"
            member __.componentWillUpdate(nextProps: 'P, nextState: 'S, nextContext: obj): unit = failwith "JS only"
            member __.componentDidUpdate(prevProps: 'P, prevState: 'S, prevContext: obj): unit = failwith "JS only"
            member __.componentWillUnmount(): unit = failwith "JS only"

    and IMixin<'P, 'S> =
        inherit IComponentLifecycle<'P, 'S>
        abstract mixins: Mixin<'P, 'S> option with get, set
        abstract statics: obj option with get, set
        abstract displayName: string option with get, set
        abstract propTypes: ValidationMap<obj> option with get, set
        abstract contextTypes: ValidationMap<obj> option with get, set
        abstract childContextTypes: ValidationMap<obj> option with get, set
        abstract getDefaultProps: unit -> 'P
        abstract getInitialState: unit -> 'S

    and [<AbstractClass>] Mixin<'P, 'S>() =
        inherit ComponentLifecycle<'P, 'S>()
        interface IMixin<'P, 'S> with
            member __.mixins with get(): Mixin<'P, 'S> option = failwith "JS only" and set(v: Mixin<'P, 'S> option): unit = failwith "JS only"
            member __.statics with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.displayName with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.propTypes with get(): ValidationMap<obj> option = failwith "JS only" and set(v: ValidationMap<obj> option): unit = failwith "JS only"
            member __.contextTypes with get(): ValidationMap<obj> option = failwith "JS only" and set(v: ValidationMap<obj> option): unit = failwith "JS only"
            member __.childContextTypes with get(): ValidationMap<obj> option = failwith "JS only" and set(v: ValidationMap<obj> option): unit = failwith "JS only"
            member __.getDefaultProps(): 'P = failwith "JS only"
            member __.getInitialState(): 'S = failwith "JS only"

    and IComponentSpec<'P, 'S> =
        inherit IMixin<'P, 'S>
        abstract render: unit -> ReactElement<obj>

    and [<AbstractClass>] ComponentSpec<'P, 'S>() =
        inherit Mixin<'P, 'S>()
        interface IComponentSpec<'P, 'S> with
            member __.render(): ReactElement<obj> = failwith "JS only"

    and ISyntheticEvent =
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

    and [<AbstractClass>] SyntheticEvent() =
        interface ISyntheticEvent with
            member __.bubbles with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.cancelable with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.currentTarget with get(): EventTarget = failwith "JS only" and set(v: EventTarget): unit = failwith "JS only"
            member __.defaultPrevented with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.eventPhase with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.isTrusted with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.nativeEvent with get(): Event = failwith "JS only" and set(v: Event): unit = failwith "JS only"
            member __.target with get(): EventTarget = failwith "JS only" and set(v: EventTarget): unit = failwith "JS only"
            member __.timeStamp with get(): DateTime = failwith "JS only" and set(v: DateTime): unit = failwith "JS only"
            member __.``type`` with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.preventDefault(): unit = failwith "JS only"
            member __.stopPropagation(): unit = failwith "JS only"

    and IClipboardEvent =
        inherit ISyntheticEvent
        abstract clipboardData: DataTransfer with get, set

    and [<AbstractClass>] ClipboardEvent() =
        inherit SyntheticEvent()
        interface IClipboardEvent with
            member __.clipboardData with get(): DataTransfer = failwith "JS only" and set(v: DataTransfer): unit = failwith "JS only"

    and ICompositionEvent =
        inherit ISyntheticEvent
        abstract data: string with get, set

    and [<AbstractClass>] CompositionEvent() =
        inherit SyntheticEvent()
        interface ICompositionEvent with
            member __.data with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"

    and IDragEvent =
        inherit ISyntheticEvent
        abstract dataTransfer: DataTransfer with get, set

    and [<AbstractClass>] DragEvent() =
        inherit SyntheticEvent()
        interface IDragEvent with
            member __.dataTransfer with get(): DataTransfer = failwith "JS only" and set(v: DataTransfer): unit = failwith "JS only"

    and IFocusEvent =
        inherit ISyntheticEvent
        abstract relatedTarget: EventTarget with get, set

    and [<AbstractClass>] FocusEvent() =
        inherit SyntheticEvent()
        interface IFocusEvent with
            member __.relatedTarget with get(): EventTarget = failwith "JS only" and set(v: EventTarget): unit = failwith "JS only"

    and IFormEvent =
        inherit ISyntheticEvent

    and [<AbstractClass>] FormEvent() =
        inherit SyntheticEvent()
        interface IFormEvent


    and IKeyboardEvent =
        inherit ISyntheticEvent
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

    and [<AbstractClass>] KeyboardEvent() =
        inherit SyntheticEvent()
        interface IKeyboardEvent with
            member __.altKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.charCode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.ctrlKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.key with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.keyCode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.locale with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
            member __.location with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.metaKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.repeat with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.shiftKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.which with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.getModifierState(key: string): bool = failwith "JS only"

    and IMouseEvent =
        inherit ISyntheticEvent
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

    and [<AbstractClass>] MouseEvent() =
        inherit SyntheticEvent()
        interface IMouseEvent with
            member __.altKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.button with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.buttons with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.clientX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.clientY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.ctrlKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.metaKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.pageX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.pageY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.relatedTarget with get(): EventTarget = failwith "JS only" and set(v: EventTarget): unit = failwith "JS only"
            member __.screenX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.screenY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.shiftKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.getModifierState(key: string): bool = failwith "JS only"

    and ITouchEvent =
        inherit ISyntheticEvent
        abstract altKey: bool with get, set
        abstract changedTouches: TouchList with get, set
        abstract ctrlKey: bool with get, set
        abstract metaKey: bool with get, set
        abstract shiftKey: bool with get, set
        abstract targetTouches: TouchList with get, set
        abstract touches: TouchList with get, set
        abstract getModifierState: key: string -> bool

    and [<AbstractClass>] TouchEvent() =
        inherit SyntheticEvent()
        interface ITouchEvent with
            member __.altKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.changedTouches with get(): TouchList = failwith "JS only" and set(v: TouchList): unit = failwith "JS only"
            member __.ctrlKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.metaKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.shiftKey with get(): bool = failwith "JS only" and set(v: bool): unit = failwith "JS only"
            member __.targetTouches with get(): TouchList = failwith "JS only" and set(v: TouchList): unit = failwith "JS only"
            member __.touches with get(): TouchList = failwith "JS only" and set(v: TouchList): unit = failwith "JS only"
            member __.getModifierState(key: string): bool = failwith "JS only"

    and IUIEvent =
        inherit ISyntheticEvent
        abstract detail: float with get, set
        abstract view: AbstractView with get, set

    and [<AbstractClass>] UIEvent() =
        inherit SyntheticEvent()
        interface IUIEvent with
            member __.detail with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.view with get(): AbstractView = failwith "JS only" and set(v: AbstractView): unit = failwith "JS only"

    and IWheelEvent =
        inherit ISyntheticEvent
        abstract deltaMode: float with get, set
        abstract deltaX: float with get, set
        abstract deltaY: float with get, set
        abstract deltaZ: float with get, set

    and [<AbstractClass>] WheelEvent() =
        inherit SyntheticEvent()
        interface IWheelEvent with
            member __.deltaMode with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.deltaX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.deltaY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.deltaZ with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and IEventHandler<'E> =
        interface end

    and [<AbstractClass>] EventHandler<'E>() =
        interface IEventHandler<'E>


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

    and IProps<'T> =
        abstract children: ReactNode option with get, set
        abstract key: U2<string, float> option with get, set
        abstract ref: U2<string, Func<'T, obj>> option with get, set

    and [<AbstractClass>] Props<'T>() =
        interface IProps<'T> with
            member __.children with get(): ReactNode option = failwith "JS only" and set(v: ReactNode option): unit = failwith "JS only"
            member __.key with get(): U2<string, float> option = failwith "JS only" and set(v: U2<string, float> option): unit = failwith "JS only"
            member __.ref with get(): U2<string, Func<'T, obj>> option = failwith "JS only" and set(v: U2<string, Func<'T, obj>> option): unit = failwith "JS only"

    and IHTMLProps<'T> =
        inherit IHTMLAttributes
        // TODO: Multiple inheritance?

    and [<AbstractClass>] HTMLProps<'T>() =
        inherit HTMLAttributes()
        // TODO: Multiple inheritance?
        interface IHTMLProps<'T>


    and ISVGProps =
        inherit ISVGAttributes
        // TODO: Multiple inheritance?

    and [<AbstractClass>] SVGProps() =
        inherit SVGAttributes()
        // TODO: Multiple inheritance?
        interface ISVGProps


    and IDOMAttributes =
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

    and [<AbstractClass>] DOMAttributes() =
        interface IDOMAttributes with
            member __.dangerouslySetInnerHTML with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.onCopy with get(): ClipboardEventHandler option = failwith "JS only" and set(v: ClipboardEventHandler option): unit = failwith "JS only"
            member __.onCut with get(): ClipboardEventHandler option = failwith "JS only" and set(v: ClipboardEventHandler option): unit = failwith "JS only"
            member __.onPaste with get(): ClipboardEventHandler option = failwith "JS only" and set(v: ClipboardEventHandler option): unit = failwith "JS only"
            member __.onCompositionEnd with get(): CompositionEventHandler option = failwith "JS only" and set(v: CompositionEventHandler option): unit = failwith "JS only"
            member __.onCompositionStart with get(): CompositionEventHandler option = failwith "JS only" and set(v: CompositionEventHandler option): unit = failwith "JS only"
            member __.onCompositionUpdate with get(): CompositionEventHandler option = failwith "JS only" and set(v: CompositionEventHandler option): unit = failwith "JS only"
            member __.onFocus with get(): FocusEventHandler option = failwith "JS only" and set(v: FocusEventHandler option): unit = failwith "JS only"
            member __.onBlur with get(): FocusEventHandler option = failwith "JS only" and set(v: FocusEventHandler option): unit = failwith "JS only"
            member __.onChange with get(): FormEventHandler option = failwith "JS only" and set(v: FormEventHandler option): unit = failwith "JS only"
            member __.onInput with get(): FormEventHandler option = failwith "JS only" and set(v: FormEventHandler option): unit = failwith "JS only"
            member __.onSubmit with get(): FormEventHandler option = failwith "JS only" and set(v: FormEventHandler option): unit = failwith "JS only"
            member __.onLoad with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onError with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onKeyDown with get(): KeyboardEventHandler option = failwith "JS only" and set(v: KeyboardEventHandler option): unit = failwith "JS only"
            member __.onKeyPress with get(): KeyboardEventHandler option = failwith "JS only" and set(v: KeyboardEventHandler option): unit = failwith "JS only"
            member __.onKeyUp with get(): KeyboardEventHandler option = failwith "JS only" and set(v: KeyboardEventHandler option): unit = failwith "JS only"
            member __.onAbort with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onCanPlay with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onCanPlayThrough with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onDurationChange with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onEmptied with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onEncrypted with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onEnded with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onLoadedData with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onLoadedMetadata with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onLoadStart with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onPause with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onPlay with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onPlaying with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onProgress with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onRateChange with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onSeeked with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onSeeking with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onStalled with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onSuspend with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onTimeUpdate with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onVolumeChange with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onWaiting with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onClick with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onContextMenu with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onDoubleClick with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onDrag with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDragEnd with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDragEnter with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDragExit with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDragLeave with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDragOver with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDragStart with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onDrop with get(): DragEventHandler option = failwith "JS only" and set(v: DragEventHandler option): unit = failwith "JS only"
            member __.onMouseDown with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onMouseEnter with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onMouseLeave with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onMouseMove with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onMouseOut with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onMouseOver with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onMouseUp with get(): MouseEventHandler option = failwith "JS only" and set(v: MouseEventHandler option): unit = failwith "JS only"
            member __.onSelect with get(): ReactEventHandler option = failwith "JS only" and set(v: ReactEventHandler option): unit = failwith "JS only"
            member __.onTouchCancel with get(): TouchEventHandler option = failwith "JS only" and set(v: TouchEventHandler option): unit = failwith "JS only"
            member __.onTouchEnd with get(): TouchEventHandler option = failwith "JS only" and set(v: TouchEventHandler option): unit = failwith "JS only"
            member __.onTouchMove with get(): TouchEventHandler option = failwith "JS only" and set(v: TouchEventHandler option): unit = failwith "JS only"
            member __.onTouchStart with get(): TouchEventHandler option = failwith "JS only" and set(v: TouchEventHandler option): unit = failwith "JS only"
            member __.onScroll with get(): UIEventHandler option = failwith "JS only" and set(v: UIEventHandler option): unit = failwith "JS only"
            member __.onWheel with get(): WheelEventHandler option = failwith "JS only" and set(v: WheelEventHandler option): unit = failwith "JS only"

    and ICSSProperties =
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

    and [<AbstractClass>] CSSProperties() =
        interface ICSSProperties with
            member __.boxFlex with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.boxFlexGroup with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.columnCount with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.flex with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.flexGrow with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.flexShrink with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.fontWeight with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.lineClamp with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.lineHeight with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.opacity with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.order with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.orphans with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.widows with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.zIndex with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.zoom with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.fontSize with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.fillOpacity with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.strokeOpacity with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.strokeWidth with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.alignContent with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.alignItems with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.alignSelf with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.alignmentAdjust with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.alignmentBaseline with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.animationDelay with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.animationDirection with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.animationIterationCount with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.animationName with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.animationPlayState with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.appearance with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backfaceVisibility with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundBlendMode with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundComposite with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundImage with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundOrigin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundPositionX with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.backgroundRepeat with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.baselineShift with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.behavior with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.border with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderBottomLeftRadius with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderBottomRightRadius with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderBottomWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderCollapse with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderCornerShape with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderImageSource with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderImageWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderLeft with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderLeftColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderLeftStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderLeftWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderRight with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderRightColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderRightStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderRightWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderSpacing with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderTop with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderTopColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderTopLeftRadius with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderTopRightRadius with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderTopStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderTopWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.borderWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.bottom with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.boxAlign with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.boxDecorationBreak with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.boxDirection with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.boxLineProgression with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.boxLines with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.boxOrdinalGroup with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.breakAfter with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.breakBefore with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.breakInside with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.clear with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.clip with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.clipRule with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.color with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnFill with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnGap with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnRule with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnRuleColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnRuleWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnSpan with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columnWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.columns with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.counterIncrement with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.counterReset with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.cue with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.cueAfter with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.direction with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.display with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fill with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fillRule with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.filter with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexAlign with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexBasis with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexDirection with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexFlow with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexItemAlign with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexLinePack with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flexOrder with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.float with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.flowFrom with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.font with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontFamily with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontKerning with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontSizeAdjust with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontStretch with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontSynthesis with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontVariant with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.fontVariantAlternates with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridArea with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridColumn with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridColumnEnd with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridColumnStart with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridRow with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridRowEnd with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridRowPosition with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridRowSpan with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridTemplateAreas with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridTemplateColumns with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.gridTemplateRows with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.height with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.hyphenateLimitChars with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.hyphenateLimitLines with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.hyphenateLimitZone with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.hyphens with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.imeMode with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.layoutGrid with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.layoutGridChar with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.layoutGridLine with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.layoutGridMode with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.layoutGridType with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.left with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.letterSpacing with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.lineBreak with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.listStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.listStyleImage with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.listStylePosition with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.listStyleType with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.margin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.marginBottom with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.marginLeft with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.marginRight with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.marginTop with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.marqueeDirection with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.marqueeStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.mask with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskBorder with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskBorderRepeat with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskBorderSlice with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskBorderSource with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskBorderWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskClip with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maskOrigin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maxFontSize with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maxHeight with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.maxWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.minHeight with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.minWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.outline with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.outlineColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.outlineOffset with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.overflow with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.overflowStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.overflowX with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.padding with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.paddingBottom with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.paddingLeft with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.paddingRight with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.paddingTop with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pageBreakAfter with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pageBreakBefore with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pageBreakInside with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pause with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pauseAfter with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pauseBefore with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.perspective with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.perspectiveOrigin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.pointerEvents with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.position with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.punctuationTrim with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.quotes with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.regionFragment with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.restAfter with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.restBefore with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.right with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.rubyAlign with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.rubyPosition with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.shapeImageThreshold with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.shapeInside with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.shapeMargin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.shapeOutside with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.speak with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.speakAs with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.tabSize with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.tableLayout with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textAlign with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textAlignLast with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecoration with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationLine with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationLineThrough with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationNone with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationOverline with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationSkip with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textDecorationUnderline with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textEmphasis with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textEmphasisColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textEmphasisStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textHeight with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textIndent with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textJustifyTrim with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textKashidaSpace with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textLineThrough with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textLineThroughColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textLineThroughMode with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textLineThroughStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textLineThroughWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textOverflow with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textOverline with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textOverlineColor with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textOverlineMode with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textOverlineStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textOverlineWidth with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textRendering with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textScript with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textShadow with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textTransform with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textUnderlinePosition with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.textUnderlineStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.top with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.touchAction with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transform with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transformOrigin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transformOriginZ with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transformStyle with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transition with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transitionDelay with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transitionDuration with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transitionProperty with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.transitionTimingFunction with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.unicodeBidi with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.unicodeRange with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.userFocus with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.userInput with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.verticalAlign with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.visibility with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceBalance with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceDuration with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceFamily with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voicePitch with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceRange with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceRate with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceStress with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.voiceVolume with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.whiteSpace with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.whiteSpaceTreatment with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.width with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.wordBreak with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.wordSpacing with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.wordWrap with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.wrapFlow with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.wrapMargin with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.wrapOption with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.writingMode with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"

    and IHTMLAttributes =
        inherit IDOMAttributes
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

    and [<AbstractClass>] HTMLAttributes() =
        inherit DOMAttributes()
        interface IHTMLAttributes with
            member __.defaultChecked with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.defaultValue with get(): U2<string, ResizeArray<string>> option = failwith "JS only" and set(v: U2<string, ResizeArray<string>> option): unit = failwith "JS only"
            member __.accept with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.acceptCharset with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.accessKey with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.action with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.allowFullScreen with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.allowTransparency with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.alt with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.async with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.autoComplete with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.autoFocus with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.autoPlay with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.capture with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.cellPadding with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.cellSpacing with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.charSet with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.challenge with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.``checked`` with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.classID with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.className with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.cols with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.colSpan with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.content with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.contentEditable with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.contextMenu with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.controls with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.coords with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.crossOrigin with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.data with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.dateTime with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.``default`` with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.defer with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.dir with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.disabled with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.download with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.draggable with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.encType with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.form with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.formAction with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.formEncType with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.formMethod with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.formNoValidate with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.formTarget with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.frameBorder with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.headers with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.height with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.hidden with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.high with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.href with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.hrefLang with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.htmlFor with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.httpEquiv with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.icon with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.id with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.inputMode with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.integrity with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.is with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.keyParams with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.keyType with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.kind with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.label with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.lang with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.list with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.loop with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.low with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.manifest with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.marginHeight with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.marginWidth with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.max with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.maxLength with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.media with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.mediaGroup with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.``method`` with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.min with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.minLength with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.multiple with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.muted with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.name with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.noValidate with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.``open`` with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.optimum with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.pattern with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.placeholder with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.poster with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.preload with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.radioGroup with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.readOnly with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.rel with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.required with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.role with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.rows with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.rowSpan with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.sandbox with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.scope with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.scoped with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.scrolling with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.seamless with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.selected with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.shape with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.size with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.sizes with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.span with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.spellCheck with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.src with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.srcDoc with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.srcLang with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.srcSet with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.start with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.step with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.style with get(): CSSProperties option = failwith "JS only" and set(v: CSSProperties option): unit = failwith "JS only"
            member __.summary with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.tabIndex with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.target with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.title with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.``type`` with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.useMap with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.value with get(): U2<string, ResizeArray<string>> option = failwith "JS only" and set(v: U2<string, ResizeArray<string>> option): unit = failwith "JS only"
            member __.width with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.wmode with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.wrap with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.about with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.datatype with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.inlist with get(): obj option = failwith "JS only" and set(v: obj option): unit = failwith "JS only"
            member __.prefix with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.property with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.resource with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.typeof with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.vocab with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.autoCapitalize with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.autoCorrect with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.autoSave with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.color with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.itemProp with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.itemScope with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"
            member __.itemType with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.itemID with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.itemRef with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.results with get(): float option = failwith "JS only" and set(v: float option): unit = failwith "JS only"
            member __.security with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.unselectable with get(): bool option = failwith "JS only" and set(v: bool option): unit = failwith "JS only"

    and ISVGAttributes =
        inherit IHTMLAttributes
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

    and [<AbstractClass>] SVGAttributes() =
        inherit HTMLAttributes()
        interface ISVGAttributes with
            member __.clipPath with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.cx with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.cy with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.d with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.dx with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.dy with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.fill with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.fillOpacity with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.fontFamily with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.fontSize with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.fx with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.fy with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.gradientTransform with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.gradientUnits with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.markerEnd with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.markerMid with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.markerStart with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.offset with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.opacity with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.patternContentUnits with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.patternUnits with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.points with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.preserveAspectRatio with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.r with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.rx with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.ry with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.spreadMethod with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.stopColor with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.stopOpacity with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.stroke with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.strokeDasharray with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.strokeLinecap with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.strokeMiterlimit with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.strokeOpacity with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.strokeWidth with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.textAnchor with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.transform with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.version with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.viewBox with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.x1 with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.x2 with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.x with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.xlinkActuate with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xlinkArcrole with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xlinkHref with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xlinkRole with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xlinkShow with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xlinkTitle with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xlinkType with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xmlBase with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xmlLang with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.xmlSpace with get(): string option = failwith "JS only" and set(v: string option): unit = failwith "JS only"
            member __.y1 with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.y2 with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"
            member __.y with get(): U2<float, string> option = failwith "JS only" and set(v: U2<float, string> option): unit = failwith "JS only"

    and IReactDOM =
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

    and [<AbstractClass>] ReactDOM() =
        interface IReactDOM with
            member __.a with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.abbr with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.address with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.area with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.article with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.aside with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.audio with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.b with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.``base`` with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.bdi with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.bdo with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.big with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.blockquote with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.body with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.br with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.button with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.canvas with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.caption with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.cite with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.code with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.col with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.colgroup with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.data with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.datalist with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.dd with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.del with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.details with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.dfn with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.dialog with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.div with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.dl with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.dt with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.em with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.embed with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.fieldset with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.figcaption with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.figure with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.footer with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.form with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.h1 with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.h2 with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.h3 with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.h4 with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.h5 with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.h6 with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.head with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.header with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.hgroup with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.hr with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.html with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.i with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.iframe with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.img with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.input with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.ins with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.kbd with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.keygen with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.label with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.legend with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.li with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.link with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.main with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.map with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.mark with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.menu with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.menuitem with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.meta with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.meter with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.nav with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.noscript with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.``object`` with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.ol with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.optgroup with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.option with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.output with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.p with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.param with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.picture with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.pre with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.progress with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.q with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.rp with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.rt with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.ruby with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.s with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.samp with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.script with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.section with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.select with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.small with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.source with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.span with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.strong with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.style with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.sub with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.summary with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.sup with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.table with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.tbody with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.td with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.textarea with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.tfoot with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.th with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.thead with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.time with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.title with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.tr with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.track with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.u with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.ul with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.var with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.video with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.wbr with get(): HTMLFactory = failwith "JS only" and set(v: HTMLFactory): unit = failwith "JS only"
            member __.svg with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.circle with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.defs with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.ellipse with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.g with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.image with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.line with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.linearGradient with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.mask with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.path with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.pattern with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.polygon with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.polyline with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.radialGradient with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.rect with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.stop with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.text with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"
            member __.tspan with get(): SVGFactory = failwith "JS only" and set(v: SVGFactory): unit = failwith "JS only"

    and IValidator<'T> =
        interface end

    and [<AbstractClass>] Validator<'T>() =
        interface IValidator<'T>


    and IRequireable<'T> =
        inherit IValidator<'T>
        abstract isRequired: Validator<'T> with get, set

    and [<AbstractClass>] Requireable<'T>() =
        inherit Validator<'T>()
        interface IRequireable<'T> with
            member __.isRequired with get(): Validator<'T> = failwith "JS only" and set(v: Validator<'T>): unit = failwith "JS only"

    and IValidationMap<'T> =
        interface end

    and [<AbstractClass>] ValidationMap<'T>() =
        interface IValidationMap<'T>


    and IReactPropTypes =
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

    and [<AbstractClass>] ReactPropTypes() =
        interface IReactPropTypes with
            member __.any with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.array with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.bool with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.func with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.number with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.``object`` with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.string with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.node with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.element with get(): Requireable<obj> = failwith "JS only" and set(v: Requireable<obj>): unit = failwith "JS only"
            member __.instanceOf(expectedClass: obj): Requireable<obj> = failwith "JS only"
            member __.oneOf(types: ResizeArray<obj>): Requireable<obj> = failwith "JS only"
            member __.oneOfType(types: ResizeArray<Validator<obj>>): Requireable<obj> = failwith "JS only"
            member __.arrayOf(``type``: Validator<obj>): Requireable<obj> = failwith "JS only"
            member __.objectOf(``type``: Validator<obj>): Requireable<obj> = failwith "JS only"
            member __.shape(``type``: ValidationMap<obj>): Requireable<obj> = failwith "JS only"

    and IReactChildren =
        abstract map: children: ReactNode * fn: Func<ReactChild, float, 'T> -> ResizeArray<'T>
        abstract forEach: children: ReactNode * fn: Func<ReactChild, float, obj> -> unit
        abstract count: children: ReactNode -> float
        abstract only: children: ReactNode -> ReactElement<obj>
        abstract toArray: children: ReactNode -> ResizeArray<ReactChild>

    and [<AbstractClass>] ReactChildren() =
        interface IReactChildren with
            member __.map(children: ReactNode, fn: Func<ReactChild, float, 'T>): ResizeArray<'T> = failwith "JS only"
            member __.forEach(children: ReactNode, fn: Func<ReactChild, float, obj>): unit = failwith "JS only"
            member __.count(children: ReactNode): float = failwith "JS only"
            member __.only(children: ReactNode): ReactElement<obj> = failwith "JS only"
            member __.toArray(children: ReactNode): ResizeArray<ReactChild> = failwith "JS only"

    and IAbstractView =
        abstract styleMedia: StyleMedia with get, set
        abstract document: Document with get, set

    and [<AbstractClass>] AbstractView() =
        interface IAbstractView with
            member __.styleMedia with get(): StyleMedia = failwith "JS only" and set(v: StyleMedia): unit = failwith "JS only"
            member __.document with get(): Document = failwith "JS only" and set(v: Document): unit = failwith "JS only"

    and ITouch =
        abstract identifier: float with get, set
        abstract target: EventTarget with get, set
        abstract screenX: float with get, set
        abstract screenY: float with get, set
        abstract clientX: float with get, set
        abstract clientY: float with get, set
        abstract pageX: float with get, set
        abstract pageY: float with get, set

    and [<AbstractClass>] Touch() =
        interface ITouch with
            member __.identifier with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.target with get(): EventTarget = failwith "JS only" and set(v: EventTarget): unit = failwith "JS only"
            member __.screenX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.screenY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.clientX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.clientY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.pageX with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.pageY with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"

    and ITouchList =
        abstract length: float with get, set
        abstract item: index: float -> Touch
        abstract identifiedTouch: identifier: float -> Touch

    and [<AbstractClass>] TouchList() =
        interface ITouchList with
            member __.length with get(): float = failwith "JS only" and set(v: float): unit = failwith "JS only"
            member __.item(index: float): Touch = failwith "JS only"
            member __.identifiedTouch(identifier: float): Touch = failwith "JS only"

    type Globals =
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

    let [<Import("React")>] Globals: Globals = failwith "JS only"

