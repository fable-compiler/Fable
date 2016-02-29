namespace Fable.Import
open System
open Fable.Core
open Fable.Import.JS

type String =
    abstract localeCompare: that: string * locales: ResizeArray<string> * ?options: Intl.CollatorOptions -> float
    abstract localeCompare: that: string * locale: string * ?options: Intl.CollatorOptions -> float

and Number =
    abstract toLocaleString: ?locales: ResizeArray<string> * ?options: Intl.NumberFormatOptions -> string
    abstract toLocaleString: ?locale: string * ?options: Intl.NumberFormatOptions -> string

and Date =
    abstract toLocaleString: ?locales: ResizeArray<string> * ?options: Intl.DateTimeFormatOptions -> string
    abstract toLocaleDateString: ?locales: ResizeArray<string> * ?options: Intl.DateTimeFormatOptions -> string
    abstract toLocaleTimeString: ?locale: ResizeArray<string> * ?options: Intl.DateTimeFormatOptions -> string
    abstract toLocaleString: ?locale: string * ?options: Intl.DateTimeFormatOptions -> string
    abstract toLocaleDateString: ?locale: string * ?options: Intl.DateTimeFormatOptions -> string
    abstract toLocaleTimeString: ?locale: string * ?options: Intl.DateTimeFormatOptions -> string

and Algorithm =
    abstract name: string option with get, set

and AriaRequestEventInit =
    inherit EventInit
    abstract attributeName: string option with get, set
    abstract attributeValue: string option with get, set

and ClipboardEventInit =
    inherit EventInit
    abstract data: string option with get, set
    abstract dataType: string option with get, set

and CommandEventInit =
    inherit EventInit
    abstract commandName: string option with get, set
    abstract detail: string option with get, set

and CompositionEventInit =
    inherit UIEventInit
    abstract data: string option with get, set

and ConfirmSiteSpecificExceptionsInformation =
    inherit ExceptionInformation
    abstract arrayOfDomainStrings: ResizeArray<string> option with get, set

and CustomEventInit =
    inherit EventInit
    abstract detail: obj option with get, set

and DeviceAccelerationDict =
    abstract x: float option with get, set
    abstract y: float option with get, set
    abstract z: float option with get, set

and DeviceRotationRateDict =
    abstract alpha: float option with get, set
    abstract beta: float option with get, set
    abstract gamma: float option with get, set

and EventInit =
    abstract bubbles: bool option with get, set
    abstract cancelable: bool option with get, set

and ExceptionInformation =
    abstract domain: string option with get, set

and FocusEventInit =
    inherit UIEventInit
    abstract relatedTarget: EventTarget option with get, set

and HashChangeEventInit =
    inherit EventInit
    abstract newURL: string option with get, set
    abstract oldURL: string option with get, set

and KeyAlgorithm =
    abstract name: string option with get, set

and KeyboardEventInit =
    inherit SharedKeyboardAndMouseEventInit
    abstract key: string option with get, set
    abstract location: float option with get, set
    abstract repeat: bool option with get, set

and MouseEventInit =
    inherit SharedKeyboardAndMouseEventInit
    abstract screenX: float option with get, set
    abstract screenY: float option with get, set
    abstract clientX: float option with get, set
    abstract clientY: float option with get, set
    abstract button: float option with get, set
    abstract buttons: float option with get, set
    abstract relatedTarget: EventTarget option with get, set

and MsZoomToOptions =
    abstract contentX: float option with get, set
    abstract contentY: float option with get, set
    abstract viewportX: string option with get, set
    abstract viewportY: string option with get, set
    abstract scaleFactor: float option with get, set
    abstract animate: string option with get, set

and MutationObserverInit =
    abstract childList: bool option with get, set
    abstract attributes: bool option with get, set
    abstract characterData: bool option with get, set
    abstract subtree: bool option with get, set
    abstract attributeOldValue: bool option with get, set
    abstract characterDataOldValue: bool option with get, set
    abstract attributeFilter: ResizeArray<string> option with get, set

and ObjectURLOptions =
    abstract oneTimeOnly: bool option with get, set

and PointerEventInit =
    inherit MouseEventInit
    abstract pointerId: float option with get, set
    abstract width: float option with get, set
    abstract height: float option with get, set
    abstract pressure: float option with get, set
    abstract tiltX: float option with get, set
    abstract tiltY: float option with get, set
    abstract pointerType: string option with get, set
    abstract isPrimary: bool option with get, set

and PositionOptions =
    abstract enableHighAccuracy: bool option with get, set
    abstract timeout: float option with get, set
    abstract maximumAge: float option with get, set

and SharedKeyboardAndMouseEventInit =
    inherit UIEventInit
    abstract ctrlKey: bool option with get, set
    abstract shiftKey: bool option with get, set
    abstract altKey: bool option with get, set
    abstract metaKey: bool option with get, set
    abstract keyModifierStateAltGraph: bool option with get, set
    abstract keyModifierStateCapsLock: bool option with get, set
    abstract keyModifierStateFn: bool option with get, set
    abstract keyModifierStateFnLock: bool option with get, set
    abstract keyModifierStateHyper: bool option with get, set
    abstract keyModifierStateNumLock: bool option with get, set
    abstract keyModifierStateOS: bool option with get, set
    abstract keyModifierStateScrollLock: bool option with get, set
    abstract keyModifierStateSuper: bool option with get, set
    abstract keyModifierStateSymbol: bool option with get, set
    abstract keyModifierStateSymbolLock: bool option with get, set

and StoreExceptionsInformation =
    inherit ExceptionInformation
    abstract siteName: string option with get, set
    abstract explanationString: string option with get, set
    abstract detailURI: string option with get, set

and StoreSiteSpecificExceptionsInformation =
    inherit StoreExceptionsInformation
    abstract arrayOfDomainStrings: ResizeArray<string> option with get, set

and UIEventInit =
    inherit EventInit
    abstract view: Window option with get, set
    abstract detail: float option with get, set

and WebGLContextAttributes =
    abstract alpha: bool option with get, set
    abstract depth: bool option with get, set
    abstract stencil: bool option with get, set
    abstract antialias: bool option with get, set
    abstract premultipliedAlpha: bool option with get, set
    abstract preserveDrawingBuffer: bool option with get, set

and WebGLContextEventInit =
    inherit EventInit
    abstract statusMessage: string option with get, set

and WheelEventInit =
    inherit MouseEventInit
    abstract deltaX: float option with get, set
    abstract deltaY: float option with get, set
    abstract deltaZ: float option with get, set
    abstract deltaMode: float option with get, set

and EventListener =
    interface end

and ANGLE_instanced_arrays =
    abstract VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE: float with get, set
    abstract drawArraysInstancedANGLE: mode: float * first: float * count: float * primcount: float -> unit
    abstract drawElementsInstancedANGLE: mode: float * count: float * ``type``: float * offset: float * primcount: float -> unit
    abstract vertexAttribDivisorANGLE: index: float * divisor: float -> unit

and AnalyserNode =
    inherit AudioNode
    abstract fftSize: float with get, set
    abstract frequencyBinCount: float with get, set
    abstract maxDecibels: float with get, set
    abstract minDecibels: float with get, set
    abstract smoothingTimeConstant: float with get, set
    abstract getByteFrequencyData: array: Uint8Array -> unit
    abstract getByteTimeDomainData: array: Uint8Array -> unit
    abstract getFloatFrequencyData: array: Float32Array -> unit
    abstract getFloatTimeDomainData: array: Float32Array -> unit

and AnimationEvent =
    inherit Event
    abstract animationName: string with get, set
    abstract elapsedTime: float with get, set
    abstract initAnimationEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * animationNameArg: string * elapsedTimeArg: float -> unit

and ApplicationCache =
    inherit EventTarget
    abstract oncached: Func<Event, obj> with get, set
    abstract onchecking: Func<Event, obj> with get, set
    abstract ondownloading: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onnoupdate: Func<Event, obj> with get, set
    abstract onobsolete: Func<Event, obj> with get, set
    abstract onprogress: Func<ProgressEvent, obj> with get, set
    abstract onupdateready: Func<Event, obj> with get, set
    abstract status: float with get, set
    abstract CHECKING: float with get, set
    abstract DOWNLOADING: float with get, set
    abstract IDLE: float with get, set
    abstract OBSOLETE: float with get, set
    abstract UNCACHED: float with get, set
    abstract UPDATEREADY: float with get, set
    abstract abort: unit -> unit
    abstract swapCache: unit -> unit
    abstract update: unit -> unit
    [<Emit("$0.addEventListener('cached',$1...)")>] abstract addEventListener_cached: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('checking',$1...)")>] abstract addEventListener_checking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('downloading',$1...)")>] abstract addEventListener_downloading: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('noupdate',$1...)")>] abstract addEventListener_noupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('obsolete',$1...)")>] abstract addEventListener_obsolete: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('updateready',$1...)")>] abstract addEventListener_updateready: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and AriaRequestEvent =
    inherit Event
    abstract attributeName: string with get, set
    abstract attributeValue: string with get, set

and Attr =
    inherit Node
    abstract name: string with get, set
    abstract ownerElement: Element with get, set
    abstract specified: bool with get, set
    abstract value: string with get, set

and AudioBuffer =
    abstract duration: float with get, set
    abstract length: float with get, set
    abstract numberOfChannels: float with get, set
    abstract sampleRate: float with get, set
    abstract getChannelData: channel: float -> Float32Array

and AudioBufferSourceNode =
    inherit AudioNode
    abstract buffer: AudioBuffer with get, set
    abstract loop: bool with get, set
    abstract loopEnd: float with get, set
    abstract loopStart: float with get, set
    abstract onended: Func<Event, obj> with get, set
    abstract playbackRate: AudioParam with get, set
    abstract start: ?``when``: float * ?offset: float * ?duration: float -> unit
    abstract stop: ?``when``: float -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and AudioContext =
    inherit EventTarget
    abstract currentTime: float with get, set
    abstract destination: AudioDestinationNode with get, set
    abstract listener: AudioListener with get, set
    abstract sampleRate: float with get, set
    abstract state: string with get, set
    abstract createAnalyser: unit -> AnalyserNode
    abstract createBiquadFilter: unit -> BiquadFilterNode
    abstract createBuffer: numberOfChannels: float * length: float * sampleRate: float -> AudioBuffer
    abstract createBufferSource: unit -> AudioBufferSourceNode
    abstract createChannelMerger: ?numberOfInputs: float -> ChannelMergerNode
    abstract createChannelSplitter: ?numberOfOutputs: float -> ChannelSplitterNode
    abstract createConvolver: unit -> ConvolverNode
    abstract createDelay: ?maxDelayTime: float -> DelayNode
    abstract createDynamicsCompressor: unit -> DynamicsCompressorNode
    abstract createGain: unit -> GainNode
    abstract createMediaElementSource: mediaElement: HTMLMediaElement -> MediaElementAudioSourceNode
    abstract createOscillator: unit -> OscillatorNode
    abstract createPanner: unit -> PannerNode
    abstract createPeriodicWave: real: Float32Array * imag: Float32Array -> PeriodicWave
    abstract createScriptProcessor: ?bufferSize: float * ?numberOfInputChannels: float * ?numberOfOutputChannels: float -> ScriptProcessorNode
    abstract createStereoPanner: unit -> StereoPannerNode
    abstract createWaveShaper: unit -> WaveShaperNode
    abstract decodeAudioData: audioData: ArrayBuffer * successCallback: DecodeSuccessCallback * ?errorCallback: DecodeErrorCallback -> unit

and AudioDestinationNode =
    inherit AudioNode
    abstract maxChannelCount: float with get, set

and AudioListener =
    abstract dopplerFactor: float with get, set
    abstract speedOfSound: float with get, set
    abstract setOrientation: x: float * y: float * z: float * xUp: float * yUp: float * zUp: float -> unit
    abstract setPosition: x: float * y: float * z: float -> unit
    abstract setVelocity: x: float * y: float * z: float -> unit

and AudioNode =
    inherit EventTarget
    abstract channelCount: float with get, set
    abstract channelCountMode: string with get, set
    abstract channelInterpretation: string with get, set
    abstract context: AudioContext with get, set
    abstract numberOfInputs: float with get, set
    abstract numberOfOutputs: float with get, set
    abstract connect: destination: AudioNode * ?output: float * ?input: float -> unit
    abstract disconnect: ?output: float -> unit
    abstract disconnect: destination: AudioNode * ?output: float * ?input: float -> unit
    abstract disconnect: destination: AudioParam * ?output: float -> unit

and AudioParam =
    abstract defaultValue: float with get, set
    abstract value: float with get, set
    abstract cancelScheduledValues: startTime: float -> unit
    abstract exponentialRampToValueAtTime: value: float * endTime: float -> unit
    abstract linearRampToValueAtTime: value: float * endTime: float -> unit
    abstract setTargetAtTime: target: float * startTime: float * timeConstant: float -> unit
    abstract setValueAtTime: value: float * startTime: float -> unit
    abstract setValueCurveAtTime: values: Float32Array * startTime: float * duration: float -> unit

and AudioProcessingEvent =
    inherit Event
    abstract inputBuffer: AudioBuffer with get, set
    abstract outputBuffer: AudioBuffer with get, set
    abstract playbackTime: float with get, set

and AudioTrack =
    abstract enabled: bool with get, set
    abstract id: string with get, set
    abstract kind: string with get, set
    abstract label: string with get, set
    abstract language: string with get, set
    abstract sourceBuffer: SourceBuffer with get, set

and AudioTrackList =
    inherit EventTarget
    abstract length: float with get, set
    abstract onaddtrack: Func<TrackEvent, obj> with get, set
    abstract onchange: Func<Event, obj> with get, set
    abstract onremovetrack: Func<TrackEvent, obj> with get, set
    abstract getTrackById: id: string -> AudioTrack
    abstract item: index: float -> AudioTrack
    [<Emit("$0.addEventListener('addtrack',$1...)")>] abstract addEventListener_addtrack: listener: Func<TrackEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('removetrack',$1...)")>] abstract addEventListener_removetrack: listener: Func<TrackEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and BarProp =
    abstract visible: bool with get, set

and BeforeUnloadEvent =
    inherit Event
    abstract returnValue: obj with get, set

and BiquadFilterNode =
    inherit AudioNode
    abstract Q: AudioParam with get, set
    abstract detune: AudioParam with get, set
    abstract frequency: AudioParam with get, set
    abstract gain: AudioParam with get, set
    abstract ``type``: string with get, set
    abstract getFrequencyResponse: frequencyHz: Float32Array * magResponse: Float32Array * phaseResponse: Float32Array -> unit

and Blob =
    abstract size: float with get, set
    abstract ``type``: string with get, set
    abstract msClose: unit -> unit
    abstract msDetachStream: unit -> obj
    abstract slice: ?start: float * ?``end``: float * ?contentType: string -> Blob

and CDATASection =
    inherit Text

and CSS =
    abstract supports: property: string * ?value: string -> bool

and CSSConditionRule =
    inherit CSSGroupingRule
    abstract conditionText: string with get, set

and CSSFontFaceRule =
    inherit CSSRule
    abstract style: CSSStyleDeclaration with get, set

and CSSGroupingRule =
    inherit CSSRule
    abstract cssRules: CSSRuleList with get, set
    abstract deleteRule: ?index: float -> unit
    abstract insertRule: rule: string * ?index: float -> float

and CSSImportRule =
    inherit CSSRule
    abstract href: string with get, set
    abstract media: MediaList with get, set
    abstract styleSheet: CSSStyleSheet with get, set

and CSSKeyframeRule =
    inherit CSSRule
    abstract keyText: string with get, set
    abstract style: CSSStyleDeclaration with get, set

and CSSKeyframesRule =
    inherit CSSRule
    abstract cssRules: CSSRuleList with get, set
    abstract name: string with get, set
    abstract appendRule: rule: string -> unit
    abstract deleteRule: rule: string -> unit
    abstract findRule: rule: string -> CSSKeyframeRule

and CSSMediaRule =
    inherit CSSConditionRule
    abstract media: MediaList with get, set

and CSSNamespaceRule =
    inherit CSSRule
    abstract namespaceURI: string with get, set
    abstract prefix: string with get, set

and CSSPageRule =
    inherit CSSRule
    abstract pseudoClass: string with get, set
    abstract selector: string with get, set
    abstract selectorText: string with get, set
    abstract style: CSSStyleDeclaration with get, set

and CSSRule =
    abstract cssText: string with get, set
    abstract parentRule: CSSRule with get, set
    abstract parentStyleSheet: CSSStyleSheet with get, set
    abstract ``type``: float with get, set
    abstract CHARSET_RULE: float with get, set
    abstract FONT_FACE_RULE: float with get, set
    abstract IMPORT_RULE: float with get, set
    abstract KEYFRAMES_RULE: float with get, set
    abstract KEYFRAME_RULE: float with get, set
    abstract MEDIA_RULE: float with get, set
    abstract NAMESPACE_RULE: float with get, set
    abstract PAGE_RULE: float with get, set
    abstract STYLE_RULE: float with get, set
    abstract SUPPORTS_RULE: float with get, set
    abstract UNKNOWN_RULE: float with get, set
    abstract VIEWPORT_RULE: float with get, set

and CSSRuleList =
    abstract length: float with get, set
    abstract item: index: float -> CSSRule

and CSSStyleDeclaration =
    abstract alignContent: string with get, set
    abstract alignItems: string with get, set
    abstract alignSelf: string with get, set
    abstract alignmentBaseline: string with get, set
    abstract animation: string with get, set
    abstract animationDelay: string with get, set
    abstract animationDirection: string with get, set
    abstract animationDuration: string with get, set
    abstract animationFillMode: string with get, set
    abstract animationIterationCount: string with get, set
    abstract animationName: string with get, set
    abstract animationPlayState: string with get, set
    abstract animationTimingFunction: string with get, set
    abstract backfaceVisibility: string with get, set
    abstract background: string with get, set
    abstract backgroundAttachment: string with get, set
    abstract backgroundClip: string with get, set
    abstract backgroundColor: string with get, set
    abstract backgroundImage: string with get, set
    abstract backgroundOrigin: string with get, set
    abstract backgroundPosition: string with get, set
    abstract backgroundPositionX: string with get, set
    abstract backgroundPositionY: string with get, set
    abstract backgroundRepeat: string with get, set
    abstract backgroundSize: string with get, set
    abstract baselineShift: string with get, set
    abstract border: string with get, set
    abstract borderBottom: string with get, set
    abstract borderBottomColor: string with get, set
    abstract borderBottomLeftRadius: string with get, set
    abstract borderBottomRightRadius: string with get, set
    abstract borderBottomStyle: string with get, set
    abstract borderBottomWidth: string with get, set
    abstract borderCollapse: string with get, set
    abstract borderColor: string with get, set
    abstract borderImage: string with get, set
    abstract borderImageOutset: string with get, set
    abstract borderImageRepeat: string with get, set
    abstract borderImageSlice: string with get, set
    abstract borderImageSource: string with get, set
    abstract borderImageWidth: string with get, set
    abstract borderLeft: string with get, set
    abstract borderLeftColor: string with get, set
    abstract borderLeftStyle: string with get, set
    abstract borderLeftWidth: string with get, set
    abstract borderRadius: string with get, set
    abstract borderRight: string with get, set
    abstract borderRightColor: string with get, set
    abstract borderRightStyle: string with get, set
    abstract borderRightWidth: string with get, set
    abstract borderSpacing: string with get, set
    abstract borderStyle: string with get, set
    abstract borderTop: string with get, set
    abstract borderTopColor: string with get, set
    abstract borderTopLeftRadius: string with get, set
    abstract borderTopRightRadius: string with get, set
    abstract borderTopStyle: string with get, set
    abstract borderTopWidth: string with get, set
    abstract borderWidth: string with get, set
    abstract bottom: string with get, set
    abstract boxShadow: string with get, set
    abstract boxSizing: string with get, set
    abstract breakAfter: string with get, set
    abstract breakBefore: string with get, set
    abstract breakInside: string with get, set
    abstract captionSide: string with get, set
    abstract clear: string with get, set
    abstract clip: string with get, set
    abstract clipPath: string with get, set
    abstract clipRule: string with get, set
    abstract color: string with get, set
    abstract colorInterpolationFilters: string with get, set
    abstract columnCount: obj with get, set
    abstract columnFill: string with get, set
    abstract columnGap: obj with get, set
    abstract columnRule: string with get, set
    abstract columnRuleColor: obj with get, set
    abstract columnRuleStyle: string with get, set
    abstract columnRuleWidth: obj with get, set
    abstract columnSpan: string with get, set
    abstract columnWidth: obj with get, set
    abstract columns: string with get, set
    abstract content: string with get, set
    abstract counterIncrement: string with get, set
    abstract counterReset: string with get, set
    abstract cssFloat: string with get, set
    abstract cssText: string with get, set
    abstract cursor: string with get, set
    abstract direction: string with get, set
    abstract display: string with get, set
    abstract dominantBaseline: string with get, set
    abstract emptyCells: string with get, set
    abstract enableBackground: string with get, set
    abstract fill: string with get, set
    abstract fillOpacity: string with get, set
    abstract fillRule: string with get, set
    abstract filter: string with get, set
    abstract flex: string with get, set
    abstract flexBasis: string with get, set
    abstract flexDirection: string with get, set
    abstract flexFlow: string with get, set
    abstract flexGrow: string with get, set
    abstract flexShrink: string with get, set
    abstract flexWrap: string with get, set
    abstract floodColor: string with get, set
    abstract floodOpacity: string with get, set
    abstract font: string with get, set
    abstract fontFamily: string with get, set
    abstract fontFeatureSettings: string with get, set
    abstract fontSize: string with get, set
    abstract fontSizeAdjust: string with get, set
    abstract fontStretch: string with get, set
    abstract fontStyle: string with get, set
    abstract fontVariant: string with get, set
    abstract fontWeight: string with get, set
    abstract glyphOrientationHorizontal: string with get, set
    abstract glyphOrientationVertical: string with get, set
    abstract height: string with get, set
    abstract imeMode: string with get, set
    abstract justifyContent: string with get, set
    abstract kerning: string with get, set
    abstract left: string with get, set
    abstract length: float with get, set
    abstract letterSpacing: string with get, set
    abstract lightingColor: string with get, set
    abstract lineHeight: string with get, set
    abstract listStyle: string with get, set
    abstract listStyleImage: string with get, set
    abstract listStylePosition: string with get, set
    abstract listStyleType: string with get, set
    abstract margin: string with get, set
    abstract marginBottom: string with get, set
    abstract marginLeft: string with get, set
    abstract marginRight: string with get, set
    abstract marginTop: string with get, set
    abstract marker: string with get, set
    abstract markerEnd: string with get, set
    abstract markerMid: string with get, set
    abstract markerStart: string with get, set
    abstract mask: string with get, set
    abstract maxHeight: string with get, set
    abstract maxWidth: string with get, set
    abstract minHeight: string with get, set
    abstract minWidth: string with get, set
    abstract msContentZoomChaining: string with get, set
    abstract msContentZoomLimit: string with get, set
    abstract msContentZoomLimitMax: obj with get, set
    abstract msContentZoomLimitMin: obj with get, set
    abstract msContentZoomSnap: string with get, set
    abstract msContentZoomSnapPoints: string with get, set
    abstract msContentZoomSnapType: string with get, set
    abstract msContentZooming: string with get, set
    abstract msFlowFrom: string with get, set
    abstract msFlowInto: string with get, set
    abstract msFontFeatureSettings: string with get, set
    abstract msGridColumn: obj with get, set
    abstract msGridColumnAlign: string with get, set
    abstract msGridColumnSpan: obj with get, set
    abstract msGridColumns: string with get, set
    abstract msGridRow: obj with get, set
    abstract msGridRowAlign: string with get, set
    abstract msGridRowSpan: obj with get, set
    abstract msGridRows: string with get, set
    abstract msHighContrastAdjust: string with get, set
    abstract msHyphenateLimitChars: string with get, set
    abstract msHyphenateLimitLines: obj with get, set
    abstract msHyphenateLimitZone: obj with get, set
    abstract msHyphens: string with get, set
    abstract msImeAlign: string with get, set
    abstract msOverflowStyle: string with get, set
    abstract msScrollChaining: string with get, set
    abstract msScrollLimit: string with get, set
    abstract msScrollLimitXMax: obj with get, set
    abstract msScrollLimitXMin: obj with get, set
    abstract msScrollLimitYMax: obj with get, set
    abstract msScrollLimitYMin: obj with get, set
    abstract msScrollRails: string with get, set
    abstract msScrollSnapPointsX: string with get, set
    abstract msScrollSnapPointsY: string with get, set
    abstract msScrollSnapType: string with get, set
    abstract msScrollSnapX: string with get, set
    abstract msScrollSnapY: string with get, set
    abstract msScrollTranslation: string with get, set
    abstract msTextCombineHorizontal: string with get, set
    abstract msTextSizeAdjust: obj with get, set
    abstract msTouchAction: string with get, set
    abstract msTouchSelect: string with get, set
    abstract msUserSelect: string with get, set
    abstract msWrapFlow: string with get, set
    abstract msWrapMargin: obj with get, set
    abstract msWrapThrough: string with get, set
    abstract opacity: string with get, set
    abstract order: string with get, set
    abstract orphans: string with get, set
    abstract outline: string with get, set
    abstract outlineColor: string with get, set
    abstract outlineStyle: string with get, set
    abstract outlineWidth: string with get, set
    abstract overflow: string with get, set
    abstract overflowX: string with get, set
    abstract overflowY: string with get, set
    abstract padding: string with get, set
    abstract paddingBottom: string with get, set
    abstract paddingLeft: string with get, set
    abstract paddingRight: string with get, set
    abstract paddingTop: string with get, set
    abstract pageBreakAfter: string with get, set
    abstract pageBreakBefore: string with get, set
    abstract pageBreakInside: string with get, set
    abstract parentRule: CSSRule with get, set
    abstract perspective: string with get, set
    abstract perspectiveOrigin: string with get, set
    abstract pointerEvents: string with get, set
    abstract position: string with get, set
    abstract quotes: string with get, set
    abstract right: string with get, set
    abstract rubyAlign: string with get, set
    abstract rubyOverhang: string with get, set
    abstract rubyPosition: string with get, set
    abstract stopColor: string with get, set
    abstract stopOpacity: string with get, set
    abstract stroke: string with get, set
    abstract strokeDasharray: string with get, set
    abstract strokeDashoffset: string with get, set
    abstract strokeLinecap: string with get, set
    abstract strokeLinejoin: string with get, set
    abstract strokeMiterlimit: string with get, set
    abstract strokeOpacity: string with get, set
    abstract strokeWidth: string with get, set
    abstract tableLayout: string with get, set
    abstract textAlign: string with get, set
    abstract textAlignLast: string with get, set
    abstract textAnchor: string with get, set
    abstract textDecoration: string with get, set
    abstract textFillColor: string with get, set
    abstract textIndent: string with get, set
    abstract textJustify: string with get, set
    abstract textKashida: string with get, set
    abstract textKashidaSpace: string with get, set
    abstract textOverflow: string with get, set
    abstract textShadow: string with get, set
    abstract textTransform: string with get, set
    abstract textUnderlinePosition: string with get, set
    abstract top: string with get, set
    abstract touchAction: string with get, set
    abstract transform: string with get, set
    abstract transformOrigin: string with get, set
    abstract transformStyle: string with get, set
    abstract transition: string with get, set
    abstract transitionDelay: string with get, set
    abstract transitionDuration: string with get, set
    abstract transitionProperty: string with get, set
    abstract transitionTimingFunction: string with get, set
    abstract unicodeBidi: string with get, set
    abstract verticalAlign: string with get, set
    abstract visibility: string with get, set
    abstract webkitAlignContent: string with get, set
    abstract webkitAlignItems: string with get, set
    abstract webkitAlignSelf: string with get, set
    abstract webkitAnimation: string with get, set
    abstract webkitAnimationDelay: string with get, set
    abstract webkitAnimationDirection: string with get, set
    abstract webkitAnimationDuration: string with get, set
    abstract webkitAnimationFillMode: string with get, set
    abstract webkitAnimationIterationCount: string with get, set
    abstract webkitAnimationName: string with get, set
    abstract webkitAnimationPlayState: string with get, set
    abstract webkitAnimationTimingFunction: string with get, set
    abstract webkitAppearance: string with get, set
    abstract webkitBackfaceVisibility: string with get, set
    abstract webkitBackground: string with get, set
    abstract webkitBackgroundAttachment: string with get, set
    abstract webkitBackgroundClip: string with get, set
    abstract webkitBackgroundColor: string with get, set
    abstract webkitBackgroundImage: string with get, set
    abstract webkitBackgroundOrigin: string with get, set
    abstract webkitBackgroundPosition: string with get, set
    abstract webkitBackgroundPositionX: string with get, set
    abstract webkitBackgroundPositionY: string with get, set
    abstract webkitBackgroundRepeat: string with get, set
    abstract webkitBackgroundSize: string with get, set
    abstract webkitBorderBottomLeftRadius: string with get, set
    abstract webkitBorderBottomRightRadius: string with get, set
    abstract webkitBorderImage: string with get, set
    abstract webkitBorderImageOutset: string with get, set
    abstract webkitBorderImageRepeat: string with get, set
    abstract webkitBorderImageSlice: string with get, set
    abstract webkitBorderImageSource: string with get, set
    abstract webkitBorderImageWidth: string with get, set
    abstract webkitBorderRadius: string with get, set
    abstract webkitBorderTopLeftRadius: string with get, set
    abstract webkitBorderTopRightRadius: string with get, set
    abstract webkitBoxAlign: string with get, set
    abstract webkitBoxDirection: string with get, set
    abstract webkitBoxFlex: string with get, set
    abstract webkitBoxOrdinalGroup: string with get, set
    abstract webkitBoxOrient: string with get, set
    abstract webkitBoxPack: string with get, set
    abstract webkitBoxSizing: string with get, set
    abstract webkitColumnBreakAfter: string with get, set
    abstract webkitColumnBreakBefore: string with get, set
    abstract webkitColumnBreakInside: string with get, set
    abstract webkitColumnCount: obj with get, set
    abstract webkitColumnGap: obj with get, set
    abstract webkitColumnRule: string with get, set
    abstract webkitColumnRuleColor: obj with get, set
    abstract webkitColumnRuleStyle: string with get, set
    abstract webkitColumnRuleWidth: obj with get, set
    abstract webkitColumnSpan: string with get, set
    abstract webkitColumnWidth: obj with get, set
    abstract webkitColumns: string with get, set
    abstract webkitFilter: string with get, set
    abstract webkitFlex: string with get, set
    abstract webkitFlexBasis: string with get, set
    abstract webkitFlexDirection: string with get, set
    abstract webkitFlexFlow: string with get, set
    abstract webkitFlexGrow: string with get, set
    abstract webkitFlexShrink: string with get, set
    abstract webkitFlexWrap: string with get, set
    abstract webkitJustifyContent: string with get, set
    abstract webkitOrder: string with get, set
    abstract webkitPerspective: string with get, set
    abstract webkitPerspectiveOrigin: string with get, set
    abstract webkitTapHighlightColor: string with get, set
    abstract webkitTextFillColor: string with get, set
    abstract webkitTextSizeAdjust: obj with get, set
    abstract webkitTransform: string with get, set
    abstract webkitTransformOrigin: string with get, set
    abstract webkitTransformStyle: string with get, set
    abstract webkitTransition: string with get, set
    abstract webkitTransitionDelay: string with get, set
    abstract webkitTransitionDuration: string with get, set
    abstract webkitTransitionProperty: string with get, set
    abstract webkitTransitionTimingFunction: string with get, set
    abstract webkitUserSelect: string with get, set
    abstract webkitWritingMode: string with get, set
    abstract whiteSpace: string with get, set
    abstract widows: string with get, set
    abstract width: string with get, set
    abstract wordBreak: string with get, set
    abstract wordSpacing: string with get, set
    abstract wordWrap: string with get, set
    abstract writingMode: string with get, set
    abstract zIndex: string with get, set
    abstract zoom: string with get, set
    abstract getPropertyPriority: propertyName: string -> string
    abstract getPropertyValue: propertyName: string -> string
    abstract item: index: float -> string
    abstract removeProperty: propertyName: string -> string
    abstract setProperty: propertyName: string * value: string * ?priority: string -> unit

and CSSStyleRule =
    inherit CSSRule
    abstract readOnly: bool with get, set
    abstract selectorText: string with get, set
    abstract style: CSSStyleDeclaration with get, set

and CSSStyleSheet =
    inherit StyleSheet
    abstract cssRules: CSSRuleList with get, set
    abstract cssText: string with get, set
    abstract href: string with get, set
    abstract id: string with get, set
    abstract imports: StyleSheetList with get, set
    abstract isAlternate: bool with get, set
    abstract isPrefAlternate: bool with get, set
    abstract ownerRule: CSSRule with get, set
    abstract owningElement: Element with get, set
    abstract pages: StyleSheetPageList with get, set
    abstract readOnly: bool with get, set
    abstract rules: CSSRuleList with get, set
    abstract addImport: bstrURL: string * ?lIndex: float -> float
    abstract addPageRule: bstrSelector: string * bstrStyle: string * ?lIndex: float -> float
    abstract addRule: bstrSelector: string * ?bstrStyle: string * ?lIndex: float -> float
    abstract deleteRule: ?index: float -> unit
    abstract insertRule: rule: string * ?index: float -> float
    abstract removeImport: lIndex: float -> unit
    abstract removeRule: lIndex: float -> unit

and CSSSupportsRule =
    inherit CSSConditionRule

and CanvasGradient =
    abstract addColorStop: offset: float * color: string -> unit

and CanvasPattern =
    interface end

and CanvasRenderingContext2D =
    abstract canvas: HTMLCanvasElement with get, set
    abstract fillStyle: U3<string, CanvasGradient, CanvasPattern> with get, set
    abstract font: string with get, set
    abstract globalAlpha: float with get, set
    abstract globalCompositeOperation: string with get, set
    abstract lineCap: string with get, set
    abstract lineDashOffset: float with get, set
    abstract lineJoin: string with get, set
    abstract lineWidth: float with get, set
    abstract miterLimit: float with get, set
    abstract msFillRule: string with get, set
    abstract msImageSmoothingEnabled: bool with get, set
    abstract shadowBlur: float with get, set
    abstract shadowColor: string with get, set
    abstract shadowOffsetX: float with get, set
    abstract shadowOffsetY: float with get, set
    abstract strokeStyle: U3<string, CanvasGradient, CanvasPattern> with get, set
    abstract textAlign: string with get, set
    abstract textBaseline: string with get, set
    abstract arc: x: float * y: float * radius: float * startAngle: float * endAngle: float * ?anticlockwise: bool -> unit
    abstract arcTo: x1: float * y1: float * x2: float * y2: float * radius: float -> unit
    abstract beginPath: unit -> unit
    abstract bezierCurveTo: cp1x: float * cp1y: float * cp2x: float * cp2y: float * x: float * y: float -> unit
    abstract clearRect: x: float * y: float * w: float * h: float -> unit
    abstract clip: ?fillRule: string -> unit
    abstract closePath: unit -> unit
    abstract createImageData: imageDataOrSw: U2<float, ImageData> * ?sh: float -> ImageData
    abstract createLinearGradient: x0: float * y0: float * x1: float * y1: float -> CanvasGradient
    abstract createPattern: image: U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement> * repetition: string -> CanvasPattern
    abstract createRadialGradient: x0: float * y0: float * r0: float * x1: float * y1: float * r1: float -> CanvasGradient
    abstract drawImage: image: U3<HTMLImageElement, HTMLCanvasElement, HTMLVideoElement> * offsetX: float * offsetY: float * ?width: float * ?height: float * ?canvasOffsetX: float * ?canvasOffsetY: float * ?canvasImageWidth: float * ?canvasImageHeight: float -> unit
    abstract fill: ?fillRule: string -> unit
    abstract fillRect: x: float * y: float * w: float * h: float -> unit
    abstract fillText: text: string * x: float * y: float * ?maxWidth: float -> unit
    abstract getImageData: sx: float * sy: float * sw: float * sh: float -> ImageData
    abstract getLineDash: unit -> ResizeArray<float>
    abstract isPointInPath: x: float * y: float * ?fillRule: string -> bool
    abstract lineTo: x: float * y: float -> unit
    abstract measureText: text: string -> TextMetrics
    abstract moveTo: x: float * y: float -> unit
    abstract putImageData: imagedata: ImageData * dx: float * dy: float * ?dirtyX: float * ?dirtyY: float * ?dirtyWidth: float * ?dirtyHeight: float -> unit
    abstract quadraticCurveTo: cpx: float * cpy: float * x: float * y: float -> unit
    abstract rect: x: float * y: float * w: float * h: float -> unit
    abstract restore: unit -> unit
    abstract rotate: angle: float -> unit
    abstract save: unit -> unit
    abstract scale: x: float * y: float -> unit
    abstract setLineDash: segments: ResizeArray<float> -> unit
    abstract setTransform: m11: float * m12: float * m21: float * m22: float * dx: float * dy: float -> unit
    abstract stroke: unit -> unit
    abstract strokeRect: x: float * y: float * w: float * h: float -> unit
    abstract strokeText: text: string * x: float * y: float * ?maxWidth: float -> unit
    abstract transform: m11: float * m12: float * m21: float * m22: float * dx: float * dy: float -> unit
    abstract translate: x: float * y: float -> unit

and ChannelMergerNode =
    inherit AudioNode

and ChannelSplitterNode =
    inherit AudioNode

and CharacterData =
    inherit Node
    inherit ChildNode
    abstract data: string with get, set
    abstract length: float with get, set
    abstract appendData: arg: string -> unit
    abstract deleteData: offset: float * count: float -> unit
    abstract insertData: offset: float * arg: string -> unit
    abstract replaceData: offset: float * count: float * arg: string -> unit
    abstract substringData: offset: float * count: float -> string
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and ClientRect =
    abstract bottom: float with get, set
    abstract height: float with get, set
    abstract left: float with get, set
    abstract right: float with get, set
    abstract top: float with get, set
    abstract width: float with get, set

and ClientRectList =
    abstract length: float with get, set
    abstract item: index: float -> ClientRect

and ClipboardEvent =
    inherit Event
    abstract clipboardData: DataTransfer with get, set

and CloseEvent =
    inherit Event
    abstract code: float with get, set
    abstract reason: string with get, set
    abstract wasClean: bool with get, set
    abstract initCloseEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * wasCleanArg: bool * codeArg: float * reasonArg: string -> unit

and CommandEvent =
    inherit Event
    abstract commandName: string with get, set
    abstract detail: string with get, set

and Comment =
    inherit CharacterData
    abstract text: string with get, set

and CompositionEvent =
    inherit UIEvent
    abstract data: string with get, set
    abstract locale: string with get, set
    abstract initCompositionEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * dataArg: string * locale: string -> unit

and Console =
    abstract ``assert``: ?test: bool * ?message: string * [<ParamArray>] optionalParams: obj[] -> unit
    abstract clear: unit -> unit
    abstract count: ?countTitle: string -> unit
    abstract debug: ?message: string * [<ParamArray>] optionalParams: obj[] -> unit
    abstract dir: ?value: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract dirxml: value: obj -> unit
    abstract error: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract group: ?groupTitle: string -> unit
    abstract groupCollapsed: ?groupTitle: string -> unit
    abstract groupEnd: unit -> unit
    abstract info: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract log: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract msIsIndependentlyComposed: element: Element -> bool
    abstract profile: ?reportName: string -> unit
    abstract profileEnd: unit -> unit
    abstract select: element: Element -> unit
    abstract time: ?timerName: string -> unit
    abstract timeEnd: ?timerName: string -> unit
    abstract trace: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
    abstract warn: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit

and ConvolverNode =
    inherit AudioNode
    abstract buffer: AudioBuffer with get, set
    abstract normalize: bool with get, set

and Coordinates =
    abstract accuracy: float with get, set
    abstract altitude: float with get, set
    abstract altitudeAccuracy: float with get, set
    abstract heading: float with get, set
    abstract latitude: float with get, set
    abstract longitude: float with get, set
    abstract speed: float with get, set

and Crypto =
    inherit obj
    inherit RandomSource
    abstract subtle: SubtleCrypto with get, set

and CryptoKey =
    abstract algorithm: KeyAlgorithm with get, set
    abstract extractable: bool with get, set
    abstract ``type``: string with get, set
    abstract usages: ResizeArray<string> with get, set

and CryptoKeyPair =
    abstract privateKey: CryptoKey with get, set
    abstract publicKey: CryptoKey with get, set

and CustomEvent =
    inherit Event
    abstract detail: obj with get, set
    abstract initCustomEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * detailArg: obj -> unit

and DOMError =
    abstract name: string with get, set
    abstract toString: unit -> string

and DOMException =
    abstract code: float with get, set
    abstract message: string with get, set
    abstract name: string with get, set
    abstract ABORT_ERR: float with get, set
    abstract DATA_CLONE_ERR: float with get, set
    abstract DOMSTRING_SIZE_ERR: float with get, set
    abstract HIERARCHY_REQUEST_ERR: float with get, set
    abstract INDEX_SIZE_ERR: float with get, set
    abstract INUSE_ATTRIBUTE_ERR: float with get, set
    abstract INVALID_ACCESS_ERR: float with get, set
    abstract INVALID_CHARACTER_ERR: float with get, set
    abstract INVALID_MODIFICATION_ERR: float with get, set
    abstract INVALID_NODE_TYPE_ERR: float with get, set
    abstract INVALID_STATE_ERR: float with get, set
    abstract NAMESPACE_ERR: float with get, set
    abstract NETWORK_ERR: float with get, set
    abstract NOT_FOUND_ERR: float with get, set
    abstract NOT_SUPPORTED_ERR: float with get, set
    abstract NO_DATA_ALLOWED_ERR: float with get, set
    abstract NO_MODIFICATION_ALLOWED_ERR: float with get, set
    abstract PARSE_ERR: float with get, set
    abstract QUOTA_EXCEEDED_ERR: float with get, set
    abstract SECURITY_ERR: float with get, set
    abstract SERIALIZE_ERR: float with get, set
    abstract SYNTAX_ERR: float with get, set
    abstract TIMEOUT_ERR: float with get, set
    abstract TYPE_MISMATCH_ERR: float with get, set
    abstract URL_MISMATCH_ERR: float with get, set
    abstract VALIDATION_ERR: float with get, set
    abstract WRONG_DOCUMENT_ERR: float with get, set
    abstract toString: unit -> string

and DOMImplementation =
    abstract createDocument: namespaceURI: string * qualifiedName: string * doctype: DocumentType -> Document
    abstract createDocumentType: qualifiedName: string * publicId: string * systemId: string -> DocumentType
    abstract createHTMLDocument: title: string -> Document
    abstract hasFeature: feature: string * version: string -> bool

and DOMParser =
    abstract parseFromString: source: string * mimeType: string -> Document

and DOMSettableTokenList =
    inherit DOMTokenList
    abstract value: string with get, set

and DOMStringList =
    abstract length: float with get, set
    abstract contains: str: string -> bool
    abstract item: index: float -> string

and DOMStringMap =
    interface end

and DOMTokenList =
    abstract length: float with get, set
    abstract add: [<ParamArray>] token: string[] -> unit
    abstract contains: token: string -> bool
    abstract item: index: float -> string
    abstract remove: [<ParamArray>] token: string[] -> unit
    abstract toString: unit -> string
    abstract toggle: token: string * ?force: bool -> bool

and DataCue =
    inherit TextTrackCue
    abstract data: ArrayBuffer with get, set

and DataTransfer =
    abstract dropEffect: string with get, set
    abstract effectAllowed: string with get, set
    abstract files: FileList with get, set
    abstract items: DataTransferItemList with get, set
    abstract types: DOMStringList with get, set
    abstract clearData: ?format: string -> bool
    abstract getData: format: string -> string
    abstract setData: format: string * data: string -> bool

and DataTransferItem =
    abstract kind: string with get, set
    abstract ``type``: string with get, set
    abstract getAsFile: unit -> File
    abstract getAsString: _callback: FunctionStringCallback -> unit

and DataTransferItemList =
    abstract length: float with get, set
    abstract add: data: File -> DataTransferItem
    abstract clear: unit -> unit
    abstract item: index: float -> DataTransferItem
    abstract remove: index: float -> unit

and DeferredPermissionRequest =
    abstract id: float with get, set
    abstract ``type``: string with get, set
    abstract uri: string with get, set
    abstract allow: unit -> unit
    abstract deny: unit -> unit

and DelayNode =
    inherit AudioNode
    abstract delayTime: AudioParam with get, set

and DeviceAcceleration =
    abstract x: float with get, set
    abstract y: float with get, set
    abstract z: float with get, set

and DeviceMotionEvent =
    inherit Event
    abstract acceleration: DeviceAcceleration with get, set
    abstract accelerationIncludingGravity: DeviceAcceleration with get, set
    abstract interval: float with get, set
    abstract rotationRate: DeviceRotationRate with get, set
    abstract initDeviceMotionEvent: ``type``: string * bubbles: bool * cancelable: bool * acceleration: DeviceAccelerationDict * accelerationIncludingGravity: DeviceAccelerationDict * rotationRate: DeviceRotationRateDict * interval: float -> unit

and DeviceOrientationEvent =
    inherit Event
    abstract absolute: bool with get, set
    abstract alpha: float with get, set
    abstract beta: float with get, set
    abstract gamma: float with get, set
    abstract initDeviceOrientationEvent: ``type``: string * bubbles: bool * cancelable: bool * alpha: float * beta: float * gamma: float * absolute: bool -> unit

and DeviceRotationRate =
    abstract alpha: float with get, set
    abstract beta: float with get, set
    abstract gamma: float with get, set

and Document =
    inherit Node
    inherit GlobalEventHandlers
    inherit NodeSelector
    inherit DocumentEvent
    abstract URL: string with get, set
    abstract URLUnencoded: string with get, set
    abstract activeElement: Element with get, set
    abstract alinkColor: string with get, set
    abstract all: HTMLCollection with get, set
    abstract anchors: HTMLCollection with get, set
    abstract applets: HTMLCollection with get, set
    abstract bgColor: string with get, set
    abstract body: HTMLElement with get, set
    abstract characterSet: string with get, set
    abstract charset: string with get, set
    abstract compatMode: string with get, set
    abstract cookie: string with get, set
    abstract defaultCharset: string with get, set
    abstract defaultView: Window with get, set
    abstract designMode: string with get, set
    abstract dir: string with get, set
    abstract doctype: DocumentType with get, set
    abstract documentElement: HTMLElement with get, set
    abstract domain: string with get, set
    abstract embeds: HTMLCollection with get, set
    abstract fgColor: string with get, set
    abstract forms: HTMLCollection with get, set
    abstract fullscreenElement: Element with get, set
    abstract fullscreenEnabled: bool with get, set
    abstract head: HTMLHeadElement with get, set
    abstract hidden: bool with get, set
    abstract images: HTMLCollection with get, set
    abstract implementation: DOMImplementation with get, set
    abstract inputEncoding: string with get, set
    abstract lastModified: string with get, set
    abstract linkColor: string with get, set
    abstract links: HTMLCollection with get, set
    abstract location: Location with get, set
    abstract media: string with get, set
    abstract msCSSOMElementFloatMetrics: bool with get, set
    abstract msCapsLockWarningOff: bool with get, set
    abstract msHidden: bool with get, set
    abstract msVisibilityState: string with get, set
    abstract onabort: Func<Event, obj> with get, set
    abstract onactivate: Func<UIEvent, obj> with get, set
    abstract onbeforeactivate: Func<UIEvent, obj> with get, set
    abstract onbeforedeactivate: Func<UIEvent, obj> with get, set
    abstract onblur: Func<FocusEvent, obj> with get, set
    abstract oncanplay: Func<Event, obj> with get, set
    abstract oncanplaythrough: Func<Event, obj> with get, set
    abstract onchange: Func<Event, obj> with get, set
    abstract onclick: Func<MouseEvent, obj> with get, set
    abstract oncontextmenu: Func<PointerEvent, obj> with get, set
    abstract ondblclick: Func<MouseEvent, obj> with get, set
    abstract ondeactivate: Func<UIEvent, obj> with get, set
    abstract ondrag: Func<DragEvent, obj> with get, set
    abstract ondragend: Func<DragEvent, obj> with get, set
    abstract ondragenter: Func<DragEvent, obj> with get, set
    abstract ondragleave: Func<DragEvent, obj> with get, set
    abstract ondragover: Func<DragEvent, obj> with get, set
    abstract ondragstart: Func<DragEvent, obj> with get, set
    abstract ondrop: Func<DragEvent, obj> with get, set
    abstract ondurationchange: Func<Event, obj> with get, set
    abstract onemptied: Func<Event, obj> with get, set
    abstract onended: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onfocus: Func<FocusEvent, obj> with get, set
    abstract onfullscreenchange: Func<Event, obj> with get, set
    abstract onfullscreenerror: Func<Event, obj> with get, set
    abstract oninput: Func<Event, obj> with get, set
    abstract onkeydown: Func<KeyboardEvent, obj> with get, set
    abstract onkeypress: Func<KeyboardEvent, obj> with get, set
    abstract onkeyup: Func<KeyboardEvent, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onloadeddata: Func<Event, obj> with get, set
    abstract onloadedmetadata: Func<Event, obj> with get, set
    abstract onloadstart: Func<Event, obj> with get, set
    abstract onmousedown: Func<MouseEvent, obj> with get, set
    abstract onmousemove: Func<MouseEvent, obj> with get, set
    abstract onmouseout: Func<MouseEvent, obj> with get, set
    abstract onmouseover: Func<MouseEvent, obj> with get, set
    abstract onmouseup: Func<MouseEvent, obj> with get, set
    abstract onmousewheel: Func<MouseWheelEvent, obj> with get, set
    abstract onmscontentzoom: Func<UIEvent, obj> with get, set
    abstract onmsgesturechange: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturedoubletap: Func<MSGestureEvent, obj> with get, set
    abstract onmsgestureend: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturehold: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturestart: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturetap: Func<MSGestureEvent, obj> with get, set
    abstract onmsinertiastart: Func<MSGestureEvent, obj> with get, set
    abstract onmsmanipulationstatechanged: Func<MSManipulationEvent, obj> with get, set
    abstract onmspointercancel: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerdown: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerenter: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerleave: Func<MSPointerEvent, obj> with get, set
    abstract onmspointermove: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerout: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerover: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerup: Func<MSPointerEvent, obj> with get, set
    abstract onmssitemodejumplistitemremoved: Func<MSSiteModeEvent, obj> with get, set
    abstract onmsthumbnailclick: Func<MSSiteModeEvent, obj> with get, set
    abstract onpause: Func<Event, obj> with get, set
    abstract onplay: Func<Event, obj> with get, set
    abstract onplaying: Func<Event, obj> with get, set
    abstract onpointerlockchange: Func<Event, obj> with get, set
    abstract onpointerlockerror: Func<Event, obj> with get, set
    abstract onprogress: Func<ProgressEvent, obj> with get, set
    abstract onratechange: Func<Event, obj> with get, set
    abstract onreadystatechange: Func<ProgressEvent, obj> with get, set
    abstract onreset: Func<Event, obj> with get, set
    abstract onscroll: Func<UIEvent, obj> with get, set
    abstract onseeked: Func<Event, obj> with get, set
    abstract onseeking: Func<Event, obj> with get, set
    abstract onselect: Func<UIEvent, obj> with get, set
    abstract onselectstart: Func<Event, obj> with get, set
    abstract onstalled: Func<Event, obj> with get, set
    abstract onstop: Func<Event, obj> with get, set
    abstract onsubmit: Func<Event, obj> with get, set
    abstract onsuspend: Func<Event, obj> with get, set
    abstract ontimeupdate: Func<Event, obj> with get, set
    abstract ontouchcancel: Func<TouchEvent, obj> with get, set
    abstract ontouchend: Func<TouchEvent, obj> with get, set
    abstract ontouchmove: Func<TouchEvent, obj> with get, set
    abstract ontouchstart: Func<TouchEvent, obj> with get, set
    abstract onvolumechange: Func<Event, obj> with get, set
    abstract onwaiting: Func<Event, obj> with get, set
    abstract onwebkitfullscreenchange: Func<Event, obj> with get, set
    abstract onwebkitfullscreenerror: Func<Event, obj> with get, set
    abstract plugins: HTMLCollection with get, set
    abstract pointerLockElement: Element with get, set
    abstract readyState: string with get, set
    abstract referrer: string with get, set
    abstract rootElement: SVGSVGElement with get, set
    abstract scripts: HTMLCollection with get, set
    abstract security: string with get, set
    abstract styleSheets: StyleSheetList with get, set
    abstract title: string with get, set
    abstract visibilityState: string with get, set
    abstract vlinkColor: string with get, set
    abstract webkitCurrentFullScreenElement: Element with get, set
    abstract webkitFullscreenElement: Element with get, set
    abstract webkitFullscreenEnabled: bool with get, set
    abstract webkitIsFullScreen: bool with get, set
    abstract xmlEncoding: string with get, set
    abstract xmlStandalone: bool with get, set
    abstract xmlVersion: string with get, set
    abstract currentScript: HTMLScriptElement with get, set
    abstract adoptNode: source: Node -> Node
    abstract captureEvents: unit -> unit
    abstract clear: unit -> unit
    abstract close: unit -> unit
    abstract createAttribute: name: string -> Attr
    abstract createAttributeNS: namespaceURI: string * qualifiedName: string -> Attr
    abstract createCDATASection: data: string -> CDATASection
    abstract createComment: data: string -> Comment
    abstract createDocumentFragment: unit -> DocumentFragment
    [<Emit("$0.createElement('a')")>] abstract createElement_a: unit -> HTMLAnchorElement
    [<Emit("$0.createElement('abbr')")>] abstract createElement_abbr: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('acronym')")>] abstract createElement_acronym: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('address')")>] abstract createElement_address: unit -> HTMLBlockElement
    [<Emit("$0.createElement('applet')")>] abstract createElement_applet: unit -> HTMLAppletElement
    [<Emit("$0.createElement('area')")>] abstract createElement_area: unit -> HTMLAreaElement
    [<Emit("$0.createElement('audio')")>] abstract createElement_audio: unit -> HTMLAudioElement
    [<Emit("$0.createElement('b')")>] abstract createElement_b: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('base')")>] abstract createElement_base: unit -> HTMLBaseElement
    [<Emit("$0.createElement('basefont')")>] abstract createElement_basefont: unit -> HTMLBaseFontElement
    [<Emit("$0.createElement('bdo')")>] abstract createElement_bdo: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('big')")>] abstract createElement_big: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('blockquote')")>] abstract createElement_blockquote: unit -> HTMLBlockElement
    [<Emit("$0.createElement('body')")>] abstract createElement_body: unit -> HTMLBodyElement
    [<Emit("$0.createElement('br')")>] abstract createElement_br: unit -> HTMLBRElement
    [<Emit("$0.createElement('button')")>] abstract createElement_button: unit -> HTMLButtonElement
    [<Emit("$0.createElement('canvas')")>] abstract createElement_canvas: unit -> HTMLCanvasElement
    [<Emit("$0.createElement('caption')")>] abstract createElement_caption: unit -> HTMLTableCaptionElement
    [<Emit("$0.createElement('center')")>] abstract createElement_center: unit -> HTMLBlockElement
    [<Emit("$0.createElement('cite')")>] abstract createElement_cite: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('code')")>] abstract createElement_code: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('col')")>] abstract createElement_col: unit -> HTMLTableColElement
    [<Emit("$0.createElement('colgroup')")>] abstract createElement_colgroup: unit -> HTMLTableColElement
    [<Emit("$0.createElement('datalist')")>] abstract createElement_datalist: unit -> HTMLDataListElement
    [<Emit("$0.createElement('dd')")>] abstract createElement_dd: unit -> HTMLDDElement
    [<Emit("$0.createElement('del')")>] abstract createElement_del: unit -> HTMLModElement
    [<Emit("$0.createElement('dfn')")>] abstract createElement_dfn: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('dir')")>] abstract createElement_dir: unit -> HTMLDirectoryElement
    [<Emit("$0.createElement('div')")>] abstract createElement_div: unit -> HTMLDivElement
    [<Emit("$0.createElement('dl')")>] abstract createElement_dl: unit -> HTMLDListElement
    [<Emit("$0.createElement('dt')")>] abstract createElement_dt: unit -> HTMLDTElement
    [<Emit("$0.createElement('em')")>] abstract createElement_em: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('embed')")>] abstract createElement_embed: unit -> HTMLEmbedElement
    [<Emit("$0.createElement('fieldset')")>] abstract createElement_fieldset: unit -> HTMLFieldSetElement
    [<Emit("$0.createElement('font')")>] abstract createElement_font: unit -> HTMLFontElement
    [<Emit("$0.createElement('form')")>] abstract createElement_form: unit -> HTMLFormElement
    [<Emit("$0.createElement('frame')")>] abstract createElement_frame: unit -> HTMLFrameElement
    [<Emit("$0.createElement('frameset')")>] abstract createElement_frameset: unit -> HTMLFrameSetElement
    [<Emit("$0.createElement('h1')")>] abstract createElement_h1: unit -> HTMLHeadingElement
    [<Emit("$0.createElement('h2')")>] abstract createElement_h2: unit -> HTMLHeadingElement
    [<Emit("$0.createElement('h3')")>] abstract createElement_h3: unit -> HTMLHeadingElement
    [<Emit("$0.createElement('h4')")>] abstract createElement_h4: unit -> HTMLHeadingElement
    [<Emit("$0.createElement('h5')")>] abstract createElement_h5: unit -> HTMLHeadingElement
    [<Emit("$0.createElement('h6')")>] abstract createElement_h6: unit -> HTMLHeadingElement
    [<Emit("$0.createElement('head')")>] abstract createElement_head: unit -> HTMLHeadElement
    [<Emit("$0.createElement('hr')")>] abstract createElement_hr: unit -> HTMLHRElement
    [<Emit("$0.createElement('html')")>] abstract createElement_html: unit -> HTMLHtmlElement
    [<Emit("$0.createElement('i')")>] abstract createElement_i: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('iframe')")>] abstract createElement_iframe: unit -> HTMLIFrameElement
    [<Emit("$0.createElement('img')")>] abstract createElement_img: unit -> HTMLImageElement
    [<Emit("$0.createElement('input')")>] abstract createElement_input: unit -> HTMLInputElement
    [<Emit("$0.createElement('ins')")>] abstract createElement_ins: unit -> HTMLModElement
    [<Emit("$0.createElement('isindex')")>] abstract createElement_isindex: unit -> HTMLIsIndexElement
    [<Emit("$0.createElement('kbd')")>] abstract createElement_kbd: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('keygen')")>] abstract createElement_keygen: unit -> HTMLBlockElement
    [<Emit("$0.createElement('label')")>] abstract createElement_label: unit -> HTMLLabelElement
    [<Emit("$0.createElement('legend')")>] abstract createElement_legend: unit -> HTMLLegendElement
    [<Emit("$0.createElement('li')")>] abstract createElement_li: unit -> HTMLLIElement
    [<Emit("$0.createElement('link')")>] abstract createElement_link: unit -> HTMLLinkElement
    [<Emit("$0.createElement('listing')")>] abstract createElement_listing: unit -> HTMLBlockElement
    [<Emit("$0.createElement('map')")>] abstract createElement_map: unit -> HTMLMapElement
    [<Emit("$0.createElement('marquee')")>] abstract createElement_marquee: unit -> HTMLMarqueeElement
    [<Emit("$0.createElement('menu')")>] abstract createElement_menu: unit -> HTMLMenuElement
    [<Emit("$0.createElement('meta')")>] abstract createElement_meta: unit -> HTMLMetaElement
    [<Emit("$0.createElement('nextid')")>] abstract createElement_nextid: unit -> HTMLNextIdElement
    [<Emit("$0.createElement('nobr')")>] abstract createElement_nobr: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('object')")>] abstract createElement_object: unit -> HTMLObjectElement
    [<Emit("$0.createElement('ol')")>] abstract createElement_ol: unit -> HTMLOListElement
    [<Emit("$0.createElement('optgroup')")>] abstract createElement_optgroup: unit -> HTMLOptGroupElement
    [<Emit("$0.createElement('option')")>] abstract createElement_option: unit -> HTMLOptionElement
    [<Emit("$0.createElement('p')")>] abstract createElement_p: unit -> HTMLParagraphElement
    [<Emit("$0.createElement('param')")>] abstract createElement_param: unit -> HTMLParamElement
    [<Emit("$0.createElement('plaintext')")>] abstract createElement_plaintext: unit -> HTMLBlockElement
    [<Emit("$0.createElement('pre')")>] abstract createElement_pre: unit -> HTMLPreElement
    [<Emit("$0.createElement('progress')")>] abstract createElement_progress: unit -> HTMLProgressElement
    [<Emit("$0.createElement('q')")>] abstract createElement_q: unit -> HTMLQuoteElement
    [<Emit("$0.createElement('rt')")>] abstract createElement_rt: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('ruby')")>] abstract createElement_ruby: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('s')")>] abstract createElement_s: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('samp')")>] abstract createElement_samp: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('script')")>] abstract createElement_script: unit -> HTMLScriptElement
    [<Emit("$0.createElement('select')")>] abstract createElement_select: unit -> HTMLSelectElement
    [<Emit("$0.createElement('small')")>] abstract createElement_small: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('source')")>] abstract createElement_source: unit -> HTMLSourceElement
    [<Emit("$0.createElement('span')")>] abstract createElement_span: unit -> HTMLSpanElement
    [<Emit("$0.createElement('strike')")>] abstract createElement_strike: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('strong')")>] abstract createElement_strong: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('style')")>] abstract createElement_style: unit -> HTMLStyleElement
    [<Emit("$0.createElement('sub')")>] abstract createElement_sub: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('sup')")>] abstract createElement_sup: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('table')")>] abstract createElement_table: unit -> HTMLTableElement
    [<Emit("$0.createElement('tbody')")>] abstract createElement_tbody: unit -> HTMLTableSectionElement
    [<Emit("$0.createElement('td')")>] abstract createElement_td: unit -> HTMLTableDataCellElement
    [<Emit("$0.createElement('textarea')")>] abstract createElement_textarea: unit -> HTMLTextAreaElement
    [<Emit("$0.createElement('tfoot')")>] abstract createElement_tfoot: unit -> HTMLTableSectionElement
    [<Emit("$0.createElement('th')")>] abstract createElement_th: unit -> HTMLTableHeaderCellElement
    [<Emit("$0.createElement('thead')")>] abstract createElement_thead: unit -> HTMLTableSectionElement
    [<Emit("$0.createElement('title')")>] abstract createElement_title: unit -> HTMLTitleElement
    [<Emit("$0.createElement('tr')")>] abstract createElement_tr: unit -> HTMLTableRowElement
    [<Emit("$0.createElement('track')")>] abstract createElement_track: unit -> HTMLTrackElement
    [<Emit("$0.createElement('tt')")>] abstract createElement_tt: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('u')")>] abstract createElement_u: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('ul')")>] abstract createElement_ul: unit -> HTMLUListElement
    [<Emit("$0.createElement('var')")>] abstract createElement_var: unit -> HTMLPhraseElement
    [<Emit("$0.createElement('video')")>] abstract createElement_video: unit -> HTMLVideoElement
    [<Emit("$0.createElement('x-ms-webview')")>] abstract ``createElement_x-ms-webview``: unit -> MSHTMLWebViewElement
    [<Emit("$0.createElement('xmp')")>] abstract createElement_xmp: unit -> HTMLBlockElement
    abstract createElement: tagName: string -> HTMLElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'a')")>] abstract createElementNS_a: unit -> SVGAElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'circle')")>] abstract createElementNS_circle: unit -> SVGCircleElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'clipPath')")>] abstract createElementNS_clipPath: unit -> SVGClipPathElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'componentTransferFunction')")>] abstract createElementNS_componentTransferFunction: unit -> SVGComponentTransferFunctionElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'defs')")>] abstract createElementNS_defs: unit -> SVGDefsElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'desc')")>] abstract createElementNS_desc: unit -> SVGDescElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'ellipse')")>] abstract createElementNS_ellipse: unit -> SVGEllipseElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feBlend')")>] abstract createElementNS_feBlend: unit -> SVGFEBlendElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feColorMatrix')")>] abstract createElementNS_feColorMatrix: unit -> SVGFEColorMatrixElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feComponentTransfer')")>] abstract createElementNS_feComponentTransfer: unit -> SVGFEComponentTransferElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feComposite')")>] abstract createElementNS_feComposite: unit -> SVGFECompositeElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feConvolveMatrix')")>] abstract createElementNS_feConvolveMatrix: unit -> SVGFEConvolveMatrixElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feDiffuseLighting')")>] abstract createElementNS_feDiffuseLighting: unit -> SVGFEDiffuseLightingElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feDisplacementMap')")>] abstract createElementNS_feDisplacementMap: unit -> SVGFEDisplacementMapElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feDistantLight')")>] abstract createElementNS_feDistantLight: unit -> SVGFEDistantLightElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feFlood')")>] abstract createElementNS_feFlood: unit -> SVGFEFloodElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feFuncA')")>] abstract createElementNS_feFuncA: unit -> SVGFEFuncAElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feFuncB')")>] abstract createElementNS_feFuncB: unit -> SVGFEFuncBElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feFuncG')")>] abstract createElementNS_feFuncG: unit -> SVGFEFuncGElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feFuncR')")>] abstract createElementNS_feFuncR: unit -> SVGFEFuncRElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feGaussianBlur')")>] abstract createElementNS_feGaussianBlur: unit -> SVGFEGaussianBlurElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feImage')")>] abstract createElementNS_feImage: unit -> SVGFEImageElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feMerge')")>] abstract createElementNS_feMerge: unit -> SVGFEMergeElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feMergeNode')")>] abstract createElementNS_feMergeNode: unit -> SVGFEMergeNodeElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feMorphology')")>] abstract createElementNS_feMorphology: unit -> SVGFEMorphologyElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feOffset')")>] abstract createElementNS_feOffset: unit -> SVGFEOffsetElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'fePointLight')")>] abstract createElementNS_fePointLight: unit -> SVGFEPointLightElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feSpecularLighting')")>] abstract createElementNS_feSpecularLighting: unit -> SVGFESpecularLightingElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feSpotLight')")>] abstract createElementNS_feSpotLight: unit -> SVGFESpotLightElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feTile')")>] abstract createElementNS_feTile: unit -> SVGFETileElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'feTurbulence')")>] abstract createElementNS_feTurbulence: unit -> SVGFETurbulenceElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'filter')")>] abstract createElementNS_filter: unit -> SVGFilterElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'foreignObject')")>] abstract createElementNS_foreignObject: unit -> SVGForeignObjectElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'g')")>] abstract createElementNS_g: unit -> SVGGElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'image')")>] abstract createElementNS_image: unit -> SVGImageElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'gradient')")>] abstract createElementNS_gradient: unit -> SVGGradientElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'line')")>] abstract createElementNS_line: unit -> SVGLineElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'linearGradient')")>] abstract createElementNS_linearGradient: unit -> SVGLinearGradientElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'marker')")>] abstract createElementNS_marker: unit -> SVGMarkerElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'mask')")>] abstract createElementNS_mask: unit -> SVGMaskElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'path')")>] abstract createElementNS_path: unit -> SVGPathElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'metadata')")>] abstract createElementNS_metadata: unit -> SVGMetadataElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'pattern')")>] abstract createElementNS_pattern: unit -> SVGPatternElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'polygon')")>] abstract createElementNS_polygon: unit -> SVGPolygonElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'polyline')")>] abstract createElementNS_polyline: unit -> SVGPolylineElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'radialGradient')")>] abstract createElementNS_radialGradient: unit -> SVGRadialGradientElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'rect')")>] abstract createElementNS_rect: unit -> SVGRectElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'svg')")>] abstract createElementNS_svg: unit -> SVGSVGElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'script')")>] abstract createElementNS_script: unit -> SVGScriptElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'stop')")>] abstract createElementNS_stop: unit -> SVGStopElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'style')")>] abstract createElementNS_style: unit -> SVGStyleElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'switch')")>] abstract createElementNS_switch: unit -> SVGSwitchElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'symbol')")>] abstract createElementNS_symbol: unit -> SVGSymbolElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'tspan')")>] abstract createElementNS_tspan: unit -> SVGTSpanElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'textContent')")>] abstract createElementNS_textContent: unit -> SVGTextContentElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'text')")>] abstract createElementNS_text: unit -> SVGTextElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'textPath')")>] abstract createElementNS_textPath: unit -> SVGTextPathElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'textPositioning')")>] abstract createElementNS_textPositioning: unit -> SVGTextPositioningElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'title')")>] abstract createElementNS_title: unit -> SVGTitleElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'use')")>] abstract createElementNS_use: unit -> SVGUseElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg', 'view')")>] abstract createElementNS_view: unit -> SVGViewElement
    [<Emit("$0.createElementNS('http://www.w3.org/2000/svg',$1...)")>] abstract ``createElementNS_http://www.w3.org/2000/svg``: qualifiedName: string -> SVGElement
    abstract createElementNS: namespaceURI: string * qualifiedName: string -> Element
    abstract createExpression: expression: string * resolver: XPathNSResolver -> XPathExpression
    abstract createNSResolver: nodeResolver: Node -> XPathNSResolver
    abstract createNodeIterator: root: Node * ?whatToShow: float * ?filter: NodeFilter * ?entityReferenceExpansion: bool -> NodeIterator
    abstract createProcessingInstruction: target: string * data: string -> ProcessingInstruction
    abstract createRange: unit -> Range
    abstract createTextNode: data: string -> Text
    abstract createTouch: view: obj * target: EventTarget * identifier: float * pageX: float * pageY: float * screenX: float * screenY: float -> Touch
    abstract createTouchList: [<ParamArray>] touches: Touch[] -> TouchList
    abstract createTreeWalker: root: Node * ?whatToShow: float * ?filter: NodeFilter * ?entityReferenceExpansion: bool -> TreeWalker
    abstract elementFromPoint: x: float * y: float -> Element
    abstract evaluate: expression: string * contextNode: Node * resolver: XPathNSResolver * ``type``: float * result: XPathResult -> XPathResult
    abstract execCommand: commandId: string * ?showUI: bool * ?value: obj -> bool
    abstract execCommandShowHelp: commandId: string -> bool
    abstract exitFullscreen: unit -> unit
    abstract exitPointerLock: unit -> unit
    abstract focus: unit -> unit
    abstract getElementById: elementId: string -> HTMLElement
    abstract getElementsByClassName: classNames: string -> NodeListOf<Element>
    abstract getElementsByName: elementName: string -> NodeListOf<Element>
    [<Emit("$0.getElementsByTagName('a')")>] abstract getElementsByTagName_a: unit -> NodeListOf<HTMLAnchorElement>
    [<Emit("$0.getElementsByTagName('abbr')")>] abstract getElementsByTagName_abbr: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('acronym')")>] abstract getElementsByTagName_acronym: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('address')")>] abstract getElementsByTagName_address: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('applet')")>] abstract getElementsByTagName_applet: unit -> NodeListOf<HTMLAppletElement>
    [<Emit("$0.getElementsByTagName('area')")>] abstract getElementsByTagName_area: unit -> NodeListOf<HTMLAreaElement>
    [<Emit("$0.getElementsByTagName('article')")>] abstract getElementsByTagName_article: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('aside')")>] abstract getElementsByTagName_aside: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('audio')")>] abstract getElementsByTagName_audio: unit -> NodeListOf<HTMLAudioElement>
    [<Emit("$0.getElementsByTagName('b')")>] abstract getElementsByTagName_b: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('base')")>] abstract getElementsByTagName_base: unit -> NodeListOf<HTMLBaseElement>
    [<Emit("$0.getElementsByTagName('basefont')")>] abstract getElementsByTagName_basefont: unit -> NodeListOf<HTMLBaseFontElement>
    [<Emit("$0.getElementsByTagName('bdo')")>] abstract getElementsByTagName_bdo: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('big')")>] abstract getElementsByTagName_big: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('blockquote')")>] abstract getElementsByTagName_blockquote: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('body')")>] abstract getElementsByTagName_body: unit -> NodeListOf<HTMLBodyElement>
    [<Emit("$0.getElementsByTagName('br')")>] abstract getElementsByTagName_br: unit -> NodeListOf<HTMLBRElement>
    [<Emit("$0.getElementsByTagName('button')")>] abstract getElementsByTagName_button: unit -> NodeListOf<HTMLButtonElement>
    [<Emit("$0.getElementsByTagName('canvas')")>] abstract getElementsByTagName_canvas: unit -> NodeListOf<HTMLCanvasElement>
    [<Emit("$0.getElementsByTagName('caption')")>] abstract getElementsByTagName_caption: unit -> NodeListOf<HTMLTableCaptionElement>
    [<Emit("$0.getElementsByTagName('center')")>] abstract getElementsByTagName_center: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('circle')")>] abstract getElementsByTagName_circle: unit -> NodeListOf<SVGCircleElement>
    [<Emit("$0.getElementsByTagName('cite')")>] abstract getElementsByTagName_cite: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('clippath')")>] abstract getElementsByTagName_clippath: unit -> NodeListOf<SVGClipPathElement>
    [<Emit("$0.getElementsByTagName('code')")>] abstract getElementsByTagName_code: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('col')")>] abstract getElementsByTagName_col: unit -> NodeListOf<HTMLTableColElement>
    [<Emit("$0.getElementsByTagName('colgroup')")>] abstract getElementsByTagName_colgroup: unit -> NodeListOf<HTMLTableColElement>
    [<Emit("$0.getElementsByTagName('datalist')")>] abstract getElementsByTagName_datalist: unit -> NodeListOf<HTMLDataListElement>
    [<Emit("$0.getElementsByTagName('dd')")>] abstract getElementsByTagName_dd: unit -> NodeListOf<HTMLDDElement>
    [<Emit("$0.getElementsByTagName('defs')")>] abstract getElementsByTagName_defs: unit -> NodeListOf<SVGDefsElement>
    [<Emit("$0.getElementsByTagName('del')")>] abstract getElementsByTagName_del: unit -> NodeListOf<HTMLModElement>
    [<Emit("$0.getElementsByTagName('desc')")>] abstract getElementsByTagName_desc: unit -> NodeListOf<SVGDescElement>
    [<Emit("$0.getElementsByTagName('dfn')")>] abstract getElementsByTagName_dfn: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('dir')")>] abstract getElementsByTagName_dir: unit -> NodeListOf<HTMLDirectoryElement>
    [<Emit("$0.getElementsByTagName('div')")>] abstract getElementsByTagName_div: unit -> NodeListOf<HTMLDivElement>
    [<Emit("$0.getElementsByTagName('dl')")>] abstract getElementsByTagName_dl: unit -> NodeListOf<HTMLDListElement>
    [<Emit("$0.getElementsByTagName('dt')")>] abstract getElementsByTagName_dt: unit -> NodeListOf<HTMLDTElement>
    [<Emit("$0.getElementsByTagName('ellipse')")>] abstract getElementsByTagName_ellipse: unit -> NodeListOf<SVGEllipseElement>
    [<Emit("$0.getElementsByTagName('em')")>] abstract getElementsByTagName_em: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('embed')")>] abstract getElementsByTagName_embed: unit -> NodeListOf<HTMLEmbedElement>
    [<Emit("$0.getElementsByTagName('feblend')")>] abstract getElementsByTagName_feblend: unit -> NodeListOf<SVGFEBlendElement>
    [<Emit("$0.getElementsByTagName('fecolormatrix')")>] abstract getElementsByTagName_fecolormatrix: unit -> NodeListOf<SVGFEColorMatrixElement>
    [<Emit("$0.getElementsByTagName('fecomponenttransfer')")>] abstract getElementsByTagName_fecomponenttransfer: unit -> NodeListOf<SVGFEComponentTransferElement>
    [<Emit("$0.getElementsByTagName('fecomposite')")>] abstract getElementsByTagName_fecomposite: unit -> NodeListOf<SVGFECompositeElement>
    [<Emit("$0.getElementsByTagName('feconvolvematrix')")>] abstract getElementsByTagName_feconvolvematrix: unit -> NodeListOf<SVGFEConvolveMatrixElement>
    [<Emit("$0.getElementsByTagName('fediffuselighting')")>] abstract getElementsByTagName_fediffuselighting: unit -> NodeListOf<SVGFEDiffuseLightingElement>
    [<Emit("$0.getElementsByTagName('fedisplacementmap')")>] abstract getElementsByTagName_fedisplacementmap: unit -> NodeListOf<SVGFEDisplacementMapElement>
    [<Emit("$0.getElementsByTagName('fedistantlight')")>] abstract getElementsByTagName_fedistantlight: unit -> NodeListOf<SVGFEDistantLightElement>
    [<Emit("$0.getElementsByTagName('feflood')")>] abstract getElementsByTagName_feflood: unit -> NodeListOf<SVGFEFloodElement>
    [<Emit("$0.getElementsByTagName('fefunca')")>] abstract getElementsByTagName_fefunca: unit -> NodeListOf<SVGFEFuncAElement>
    [<Emit("$0.getElementsByTagName('fefuncb')")>] abstract getElementsByTagName_fefuncb: unit -> NodeListOf<SVGFEFuncBElement>
    [<Emit("$0.getElementsByTagName('fefuncg')")>] abstract getElementsByTagName_fefuncg: unit -> NodeListOf<SVGFEFuncGElement>
    [<Emit("$0.getElementsByTagName('fefuncr')")>] abstract getElementsByTagName_fefuncr: unit -> NodeListOf<SVGFEFuncRElement>
    [<Emit("$0.getElementsByTagName('fegaussianblur')")>] abstract getElementsByTagName_fegaussianblur: unit -> NodeListOf<SVGFEGaussianBlurElement>
    [<Emit("$0.getElementsByTagName('feimage')")>] abstract getElementsByTagName_feimage: unit -> NodeListOf<SVGFEImageElement>
    [<Emit("$0.getElementsByTagName('femerge')")>] abstract getElementsByTagName_femerge: unit -> NodeListOf<SVGFEMergeElement>
    [<Emit("$0.getElementsByTagName('femergenode')")>] abstract getElementsByTagName_femergenode: unit -> NodeListOf<SVGFEMergeNodeElement>
    [<Emit("$0.getElementsByTagName('femorphology')")>] abstract getElementsByTagName_femorphology: unit -> NodeListOf<SVGFEMorphologyElement>
    [<Emit("$0.getElementsByTagName('feoffset')")>] abstract getElementsByTagName_feoffset: unit -> NodeListOf<SVGFEOffsetElement>
    [<Emit("$0.getElementsByTagName('fepointlight')")>] abstract getElementsByTagName_fepointlight: unit -> NodeListOf<SVGFEPointLightElement>
    [<Emit("$0.getElementsByTagName('fespecularlighting')")>] abstract getElementsByTagName_fespecularlighting: unit -> NodeListOf<SVGFESpecularLightingElement>
    [<Emit("$0.getElementsByTagName('fespotlight')")>] abstract getElementsByTagName_fespotlight: unit -> NodeListOf<SVGFESpotLightElement>
    [<Emit("$0.getElementsByTagName('fetile')")>] abstract getElementsByTagName_fetile: unit -> NodeListOf<SVGFETileElement>
    [<Emit("$0.getElementsByTagName('feturbulence')")>] abstract getElementsByTagName_feturbulence: unit -> NodeListOf<SVGFETurbulenceElement>
    [<Emit("$0.getElementsByTagName('fieldset')")>] abstract getElementsByTagName_fieldset: unit -> NodeListOf<HTMLFieldSetElement>
    [<Emit("$0.getElementsByTagName('figcaption')")>] abstract getElementsByTagName_figcaption: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('figure')")>] abstract getElementsByTagName_figure: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('filter')")>] abstract getElementsByTagName_filter: unit -> NodeListOf<SVGFilterElement>
    [<Emit("$0.getElementsByTagName('font')")>] abstract getElementsByTagName_font: unit -> NodeListOf<HTMLFontElement>
    [<Emit("$0.getElementsByTagName('footer')")>] abstract getElementsByTagName_footer: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('foreignobject')")>] abstract getElementsByTagName_foreignobject: unit -> NodeListOf<SVGForeignObjectElement>
    [<Emit("$0.getElementsByTagName('form')")>] abstract getElementsByTagName_form: unit -> NodeListOf<HTMLFormElement>
    [<Emit("$0.getElementsByTagName('frame')")>] abstract getElementsByTagName_frame: unit -> NodeListOf<HTMLFrameElement>
    [<Emit("$0.getElementsByTagName('frameset')")>] abstract getElementsByTagName_frameset: unit -> NodeListOf<HTMLFrameSetElement>
    [<Emit("$0.getElementsByTagName('g')")>] abstract getElementsByTagName_g: unit -> NodeListOf<SVGGElement>
    [<Emit("$0.getElementsByTagName('h1')")>] abstract getElementsByTagName_h1: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h2')")>] abstract getElementsByTagName_h2: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h3')")>] abstract getElementsByTagName_h3: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h4')")>] abstract getElementsByTagName_h4: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h5')")>] abstract getElementsByTagName_h5: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h6')")>] abstract getElementsByTagName_h6: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('head')")>] abstract getElementsByTagName_head: unit -> NodeListOf<HTMLHeadElement>
    [<Emit("$0.getElementsByTagName('header')")>] abstract getElementsByTagName_header: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('hgroup')")>] abstract getElementsByTagName_hgroup: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('hr')")>] abstract getElementsByTagName_hr: unit -> NodeListOf<HTMLHRElement>
    [<Emit("$0.getElementsByTagName('html')")>] abstract getElementsByTagName_html: unit -> NodeListOf<HTMLHtmlElement>
    [<Emit("$0.getElementsByTagName('i')")>] abstract getElementsByTagName_i: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('iframe')")>] abstract getElementsByTagName_iframe: unit -> NodeListOf<HTMLIFrameElement>
    [<Emit("$0.getElementsByTagName('image')")>] abstract getElementsByTagName_image: unit -> NodeListOf<SVGImageElement>
    [<Emit("$0.getElementsByTagName('img')")>] abstract getElementsByTagName_img: unit -> NodeListOf<HTMLImageElement>
    [<Emit("$0.getElementsByTagName('input')")>] abstract getElementsByTagName_input: unit -> NodeListOf<HTMLInputElement>
    [<Emit("$0.getElementsByTagName('ins')")>] abstract getElementsByTagName_ins: unit -> NodeListOf<HTMLModElement>
    [<Emit("$0.getElementsByTagName('isindex')")>] abstract getElementsByTagName_isindex: unit -> NodeListOf<HTMLIsIndexElement>
    [<Emit("$0.getElementsByTagName('kbd')")>] abstract getElementsByTagName_kbd: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('keygen')")>] abstract getElementsByTagName_keygen: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('label')")>] abstract getElementsByTagName_label: unit -> NodeListOf<HTMLLabelElement>
    [<Emit("$0.getElementsByTagName('legend')")>] abstract getElementsByTagName_legend: unit -> NodeListOf<HTMLLegendElement>
    [<Emit("$0.getElementsByTagName('li')")>] abstract getElementsByTagName_li: unit -> NodeListOf<HTMLLIElement>
    [<Emit("$0.getElementsByTagName('line')")>] abstract getElementsByTagName_line: unit -> NodeListOf<SVGLineElement>
    [<Emit("$0.getElementsByTagName('lineargradient')")>] abstract getElementsByTagName_lineargradient: unit -> NodeListOf<SVGLinearGradientElement>
    [<Emit("$0.getElementsByTagName('link')")>] abstract getElementsByTagName_link: unit -> NodeListOf<HTMLLinkElement>
    [<Emit("$0.getElementsByTagName('listing')")>] abstract getElementsByTagName_listing: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('map')")>] abstract getElementsByTagName_map: unit -> NodeListOf<HTMLMapElement>
    [<Emit("$0.getElementsByTagName('mark')")>] abstract getElementsByTagName_mark: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('marker')")>] abstract getElementsByTagName_marker: unit -> NodeListOf<SVGMarkerElement>
    [<Emit("$0.getElementsByTagName('marquee')")>] abstract getElementsByTagName_marquee: unit -> NodeListOf<HTMLMarqueeElement>
    [<Emit("$0.getElementsByTagName('mask')")>] abstract getElementsByTagName_mask: unit -> NodeListOf<SVGMaskElement>
    [<Emit("$0.getElementsByTagName('menu')")>] abstract getElementsByTagName_menu: unit -> NodeListOf<HTMLMenuElement>
    [<Emit("$0.getElementsByTagName('meta')")>] abstract getElementsByTagName_meta: unit -> NodeListOf<HTMLMetaElement>
    [<Emit("$0.getElementsByTagName('metadata')")>] abstract getElementsByTagName_metadata: unit -> NodeListOf<SVGMetadataElement>
    [<Emit("$0.getElementsByTagName('nav')")>] abstract getElementsByTagName_nav: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('nextid')")>] abstract getElementsByTagName_nextid: unit -> NodeListOf<HTMLNextIdElement>
    [<Emit("$0.getElementsByTagName('nobr')")>] abstract getElementsByTagName_nobr: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('noframes')")>] abstract getElementsByTagName_noframes: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('noscript')")>] abstract getElementsByTagName_noscript: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('object')")>] abstract getElementsByTagName_object: unit -> NodeListOf<HTMLObjectElement>
    [<Emit("$0.getElementsByTagName('ol')")>] abstract getElementsByTagName_ol: unit -> NodeListOf<HTMLOListElement>
    [<Emit("$0.getElementsByTagName('optgroup')")>] abstract getElementsByTagName_optgroup: unit -> NodeListOf<HTMLOptGroupElement>
    [<Emit("$0.getElementsByTagName('option')")>] abstract getElementsByTagName_option: unit -> NodeListOf<HTMLOptionElement>
    [<Emit("$0.getElementsByTagName('p')")>] abstract getElementsByTagName_p: unit -> NodeListOf<HTMLParagraphElement>
    [<Emit("$0.getElementsByTagName('param')")>] abstract getElementsByTagName_param: unit -> NodeListOf<HTMLParamElement>
    [<Emit("$0.getElementsByTagName('path')")>] abstract getElementsByTagName_path: unit -> NodeListOf<SVGPathElement>
    [<Emit("$0.getElementsByTagName('pattern')")>] abstract getElementsByTagName_pattern: unit -> NodeListOf<SVGPatternElement>
    [<Emit("$0.getElementsByTagName('plaintext')")>] abstract getElementsByTagName_plaintext: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('polygon')")>] abstract getElementsByTagName_polygon: unit -> NodeListOf<SVGPolygonElement>
    [<Emit("$0.getElementsByTagName('polyline')")>] abstract getElementsByTagName_polyline: unit -> NodeListOf<SVGPolylineElement>
    [<Emit("$0.getElementsByTagName('pre')")>] abstract getElementsByTagName_pre: unit -> NodeListOf<HTMLPreElement>
    [<Emit("$0.getElementsByTagName('progress')")>] abstract getElementsByTagName_progress: unit -> NodeListOf<HTMLProgressElement>
    [<Emit("$0.getElementsByTagName('q')")>] abstract getElementsByTagName_q: unit -> NodeListOf<HTMLQuoteElement>
    [<Emit("$0.getElementsByTagName('radialgradient')")>] abstract getElementsByTagName_radialgradient: unit -> NodeListOf<SVGRadialGradientElement>
    [<Emit("$0.getElementsByTagName('rect')")>] abstract getElementsByTagName_rect: unit -> NodeListOf<SVGRectElement>
    [<Emit("$0.getElementsByTagName('rt')")>] abstract getElementsByTagName_rt: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('ruby')")>] abstract getElementsByTagName_ruby: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('s')")>] abstract getElementsByTagName_s: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('samp')")>] abstract getElementsByTagName_samp: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('script')")>] abstract getElementsByTagName_script: unit -> NodeListOf<HTMLScriptElement>
    [<Emit("$0.getElementsByTagName('section')")>] abstract getElementsByTagName_section: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('select')")>] abstract getElementsByTagName_select: unit -> NodeListOf<HTMLSelectElement>
    [<Emit("$0.getElementsByTagName('small')")>] abstract getElementsByTagName_small: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('source')")>] abstract getElementsByTagName_source: unit -> NodeListOf<HTMLSourceElement>
    [<Emit("$0.getElementsByTagName('span')")>] abstract getElementsByTagName_span: unit -> NodeListOf<HTMLSpanElement>
    [<Emit("$0.getElementsByTagName('stop')")>] abstract getElementsByTagName_stop: unit -> NodeListOf<SVGStopElement>
    [<Emit("$0.getElementsByTagName('strike')")>] abstract getElementsByTagName_strike: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('strong')")>] abstract getElementsByTagName_strong: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('style')")>] abstract getElementsByTagName_style: unit -> NodeListOf<HTMLStyleElement>
    [<Emit("$0.getElementsByTagName('sub')")>] abstract getElementsByTagName_sub: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('sup')")>] abstract getElementsByTagName_sup: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('svg')")>] abstract getElementsByTagName_svg: unit -> NodeListOf<SVGSVGElement>
    [<Emit("$0.getElementsByTagName('switch')")>] abstract getElementsByTagName_switch: unit -> NodeListOf<SVGSwitchElement>
    [<Emit("$0.getElementsByTagName('symbol')")>] abstract getElementsByTagName_symbol: unit -> NodeListOf<SVGSymbolElement>
    [<Emit("$0.getElementsByTagName('table')")>] abstract getElementsByTagName_table: unit -> NodeListOf<HTMLTableElement>
    [<Emit("$0.getElementsByTagName('tbody')")>] abstract getElementsByTagName_tbody: unit -> NodeListOf<HTMLTableSectionElement>
    [<Emit("$0.getElementsByTagName('td')")>] abstract getElementsByTagName_td: unit -> NodeListOf<HTMLTableDataCellElement>
    [<Emit("$0.getElementsByTagName('text')")>] abstract getElementsByTagName_text: unit -> NodeListOf<SVGTextElement>
    [<Emit("$0.getElementsByTagName('textpath')")>] abstract getElementsByTagName_textpath: unit -> NodeListOf<SVGTextPathElement>
    [<Emit("$0.getElementsByTagName('textarea')")>] abstract getElementsByTagName_textarea: unit -> NodeListOf<HTMLTextAreaElement>
    [<Emit("$0.getElementsByTagName('tfoot')")>] abstract getElementsByTagName_tfoot: unit -> NodeListOf<HTMLTableSectionElement>
    [<Emit("$0.getElementsByTagName('th')")>] abstract getElementsByTagName_th: unit -> NodeListOf<HTMLTableHeaderCellElement>
    [<Emit("$0.getElementsByTagName('thead')")>] abstract getElementsByTagName_thead: unit -> NodeListOf<HTMLTableSectionElement>
    [<Emit("$0.getElementsByTagName('title')")>] abstract getElementsByTagName_title: unit -> NodeListOf<HTMLTitleElement>
    [<Emit("$0.getElementsByTagName('tr')")>] abstract getElementsByTagName_tr: unit -> NodeListOf<HTMLTableRowElement>
    [<Emit("$0.getElementsByTagName('track')")>] abstract getElementsByTagName_track: unit -> NodeListOf<HTMLTrackElement>
    [<Emit("$0.getElementsByTagName('tspan')")>] abstract getElementsByTagName_tspan: unit -> NodeListOf<SVGTSpanElement>
    [<Emit("$0.getElementsByTagName('tt')")>] abstract getElementsByTagName_tt: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('u')")>] abstract getElementsByTagName_u: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('ul')")>] abstract getElementsByTagName_ul: unit -> NodeListOf<HTMLUListElement>
    [<Emit("$0.getElementsByTagName('use')")>] abstract getElementsByTagName_use: unit -> NodeListOf<SVGUseElement>
    [<Emit("$0.getElementsByTagName('var')")>] abstract getElementsByTagName_var: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('video')")>] abstract getElementsByTagName_video: unit -> NodeListOf<HTMLVideoElement>
    [<Emit("$0.getElementsByTagName('view')")>] abstract getElementsByTagName_view: unit -> NodeListOf<SVGViewElement>
    [<Emit("$0.getElementsByTagName('wbr')")>] abstract getElementsByTagName_wbr: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('x-ms-webview')")>] abstract ``getElementsByTagName_x-ms-webview``: unit -> NodeListOf<MSHTMLWebViewElement>
    [<Emit("$0.getElementsByTagName('xmp')")>] abstract getElementsByTagName_xmp: unit -> NodeListOf<HTMLBlockElement>
    abstract getElementsByTagName: tagname: string -> NodeListOf<Element>
    abstract getElementsByTagNameNS: namespaceURI: string * localName: string -> NodeListOf<Element>
    abstract getSelection: unit -> Selection
    abstract hasFocus: unit -> bool
    abstract importNode: importedNode: Node * deep: bool -> Node
    abstract msElementsFromPoint: x: float * y: float -> NodeList
    abstract msElementsFromRect: left: float * top: float * width: float * height: float -> NodeList
    abstract ``open``: ?url: string * ?name: string * ?features: string * ?replace: bool -> Document
    abstract queryCommandEnabled: commandId: string -> bool
    abstract queryCommandIndeterm: commandId: string -> bool
    abstract queryCommandState: commandId: string -> bool
    abstract queryCommandSupported: commandId: string -> bool
    abstract queryCommandText: commandId: string -> string
    abstract queryCommandValue: commandId: string -> string
    abstract releaseEvents: unit -> unit
    abstract updateSettings: unit -> unit
    abstract webkitCancelFullScreen: unit -> unit
    abstract webkitExitFullscreen: unit -> unit
    abstract write: [<ParamArray>] content: string[] -> unit
    abstract writeln: [<ParamArray>] content: string[] -> unit
    [<Emit("$0.createElement('picture')")>] abstract createElement_picture: unit -> HTMLPictureElement
    [<Emit("$0.getElementsByTagName('picture')")>] abstract getElementsByTagName_picture: unit -> NodeListOf<HTMLPictureElement>
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('fullscreenchange',$1...)")>] abstract addEventListener_fullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('fullscreenerror',$1...)")>] abstract addEventListener_fullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mssitemodejumplistitemremoved',$1...)")>] abstract addEventListener_mssitemodejumplistitemremoved: listener: Func<MSSiteModeEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('msthumbnailclick',$1...)")>] abstract addEventListener_msthumbnailclick: listener: Func<MSSiteModeEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerlockchange',$1...)")>] abstract addEventListener_pointerlockchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerlockerror',$1...)")>] abstract addEventListener_pointerlockerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('readystatechange',$1...)")>] abstract addEventListener_readystatechange: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stop',$1...)")>] abstract addEventListener_stop: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and DocumentFragment =
    inherit Node
    inherit NodeSelector
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and DocumentType =
    inherit Node
    inherit ChildNode
    abstract entities: NamedNodeMap with get, set
    abstract internalSubset: string with get, set
    abstract name: string with get, set
    abstract notations: NamedNodeMap with get, set
    abstract publicId: string with get, set
    abstract systemId: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and DragEvent =
    inherit MouseEvent
    abstract dataTransfer: DataTransfer with get, set
    abstract initDragEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * ctrlKeyArg: bool * altKeyArg: bool * shiftKeyArg: bool * metaKeyArg: bool * buttonArg: float * relatedTargetArg: EventTarget * dataTransferArg: DataTransfer -> unit
    abstract msConvertURL: file: File * targetType: string * ?targetURL: string -> unit

and DynamicsCompressorNode =
    inherit AudioNode
    abstract attack: AudioParam with get, set
    abstract knee: AudioParam with get, set
    abstract ratio: AudioParam with get, set
    abstract reduction: AudioParam with get, set
    abstract release: AudioParam with get, set
    abstract threshold: AudioParam with get, set

and EXT_texture_filter_anisotropic =
    abstract MAX_TEXTURE_MAX_ANISOTROPY_EXT: float with get, set
    abstract TEXTURE_MAX_ANISOTROPY_EXT: float with get, set

and Element =
    inherit Node
    inherit GlobalEventHandlers
    inherit ElementTraversal
    inherit NodeSelector
    inherit ChildNode
    abstract classList: DOMTokenList with get, set
    abstract clientHeight: float with get, set
    abstract clientLeft: float with get, set
    abstract clientTop: float with get, set
    abstract clientWidth: float with get, set
    abstract msContentZoomFactor: float with get, set
    abstract msRegionOverflow: string with get, set
    abstract onariarequest: Func<AriaRequestEvent, obj> with get, set
    abstract oncommand: Func<CommandEvent, obj> with get, set
    abstract ongotpointercapture: Func<PointerEvent, obj> with get, set
    abstract onlostpointercapture: Func<PointerEvent, obj> with get, set
    abstract onmsgesturechange: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturedoubletap: Func<MSGestureEvent, obj> with get, set
    abstract onmsgestureend: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturehold: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturestart: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturetap: Func<MSGestureEvent, obj> with get, set
    abstract onmsgotpointercapture: Func<MSPointerEvent, obj> with get, set
    abstract onmsinertiastart: Func<MSGestureEvent, obj> with get, set
    abstract onmslostpointercapture: Func<MSPointerEvent, obj> with get, set
    abstract onmspointercancel: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerdown: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerenter: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerleave: Func<MSPointerEvent, obj> with get, set
    abstract onmspointermove: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerout: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerover: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerup: Func<MSPointerEvent, obj> with get, set
    abstract ontouchcancel: Func<TouchEvent, obj> with get, set
    abstract ontouchend: Func<TouchEvent, obj> with get, set
    abstract ontouchmove: Func<TouchEvent, obj> with get, set
    abstract ontouchstart: Func<TouchEvent, obj> with get, set
    abstract onwebkitfullscreenchange: Func<Event, obj> with get, set
    abstract onwebkitfullscreenerror: Func<Event, obj> with get, set
    abstract scrollHeight: float with get, set
    abstract scrollLeft: float with get, set
    abstract scrollTop: float with get, set
    abstract scrollWidth: float with get, set
    abstract tagName: string with get, set
    abstract id: string with get, set
    abstract className: string with get, set
    abstract innerHTML: string with get, set
    abstract getAttribute: ?name: string -> string
    abstract getAttributeNS: namespaceURI: string * localName: string -> string
    abstract getAttributeNode: name: string -> Attr
    abstract getAttributeNodeNS: namespaceURI: string * localName: string -> Attr
    abstract getBoundingClientRect: unit -> ClientRect
    abstract getClientRects: unit -> ClientRectList
    [<Emit("$0.getElementsByTagName('a')")>] abstract getElementsByTagName_a: unit -> NodeListOf<HTMLAnchorElement>
    [<Emit("$0.getElementsByTagName('abbr')")>] abstract getElementsByTagName_abbr: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('acronym')")>] abstract getElementsByTagName_acronym: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('address')")>] abstract getElementsByTagName_address: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('applet')")>] abstract getElementsByTagName_applet: unit -> NodeListOf<HTMLAppletElement>
    [<Emit("$0.getElementsByTagName('area')")>] abstract getElementsByTagName_area: unit -> NodeListOf<HTMLAreaElement>
    [<Emit("$0.getElementsByTagName('article')")>] abstract getElementsByTagName_article: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('aside')")>] abstract getElementsByTagName_aside: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('audio')")>] abstract getElementsByTagName_audio: unit -> NodeListOf<HTMLAudioElement>
    [<Emit("$0.getElementsByTagName('b')")>] abstract getElementsByTagName_b: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('base')")>] abstract getElementsByTagName_base: unit -> NodeListOf<HTMLBaseElement>
    [<Emit("$0.getElementsByTagName('basefont')")>] abstract getElementsByTagName_basefont: unit -> NodeListOf<HTMLBaseFontElement>
    [<Emit("$0.getElementsByTagName('bdo')")>] abstract getElementsByTagName_bdo: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('big')")>] abstract getElementsByTagName_big: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('blockquote')")>] abstract getElementsByTagName_blockquote: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('body')")>] abstract getElementsByTagName_body: unit -> NodeListOf<HTMLBodyElement>
    [<Emit("$0.getElementsByTagName('br')")>] abstract getElementsByTagName_br: unit -> NodeListOf<HTMLBRElement>
    [<Emit("$0.getElementsByTagName('button')")>] abstract getElementsByTagName_button: unit -> NodeListOf<HTMLButtonElement>
    [<Emit("$0.getElementsByTagName('canvas')")>] abstract getElementsByTagName_canvas: unit -> NodeListOf<HTMLCanvasElement>
    [<Emit("$0.getElementsByTagName('caption')")>] abstract getElementsByTagName_caption: unit -> NodeListOf<HTMLTableCaptionElement>
    [<Emit("$0.getElementsByTagName('center')")>] abstract getElementsByTagName_center: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('circle')")>] abstract getElementsByTagName_circle: unit -> NodeListOf<SVGCircleElement>
    [<Emit("$0.getElementsByTagName('cite')")>] abstract getElementsByTagName_cite: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('clippath')")>] abstract getElementsByTagName_clippath: unit -> NodeListOf<SVGClipPathElement>
    [<Emit("$0.getElementsByTagName('code')")>] abstract getElementsByTagName_code: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('col')")>] abstract getElementsByTagName_col: unit -> NodeListOf<HTMLTableColElement>
    [<Emit("$0.getElementsByTagName('colgroup')")>] abstract getElementsByTagName_colgroup: unit -> NodeListOf<HTMLTableColElement>
    [<Emit("$0.getElementsByTagName('datalist')")>] abstract getElementsByTagName_datalist: unit -> NodeListOf<HTMLDataListElement>
    [<Emit("$0.getElementsByTagName('dd')")>] abstract getElementsByTagName_dd: unit -> NodeListOf<HTMLDDElement>
    [<Emit("$0.getElementsByTagName('defs')")>] abstract getElementsByTagName_defs: unit -> NodeListOf<SVGDefsElement>
    [<Emit("$0.getElementsByTagName('del')")>] abstract getElementsByTagName_del: unit -> NodeListOf<HTMLModElement>
    [<Emit("$0.getElementsByTagName('desc')")>] abstract getElementsByTagName_desc: unit -> NodeListOf<SVGDescElement>
    [<Emit("$0.getElementsByTagName('dfn')")>] abstract getElementsByTagName_dfn: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('dir')")>] abstract getElementsByTagName_dir: unit -> NodeListOf<HTMLDirectoryElement>
    [<Emit("$0.getElementsByTagName('div')")>] abstract getElementsByTagName_div: unit -> NodeListOf<HTMLDivElement>
    [<Emit("$0.getElementsByTagName('dl')")>] abstract getElementsByTagName_dl: unit -> NodeListOf<HTMLDListElement>
    [<Emit("$0.getElementsByTagName('dt')")>] abstract getElementsByTagName_dt: unit -> NodeListOf<HTMLDTElement>
    [<Emit("$0.getElementsByTagName('ellipse')")>] abstract getElementsByTagName_ellipse: unit -> NodeListOf<SVGEllipseElement>
    [<Emit("$0.getElementsByTagName('em')")>] abstract getElementsByTagName_em: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('embed')")>] abstract getElementsByTagName_embed: unit -> NodeListOf<HTMLEmbedElement>
    [<Emit("$0.getElementsByTagName('feblend')")>] abstract getElementsByTagName_feblend: unit -> NodeListOf<SVGFEBlendElement>
    [<Emit("$0.getElementsByTagName('fecolormatrix')")>] abstract getElementsByTagName_fecolormatrix: unit -> NodeListOf<SVGFEColorMatrixElement>
    [<Emit("$0.getElementsByTagName('fecomponenttransfer')")>] abstract getElementsByTagName_fecomponenttransfer: unit -> NodeListOf<SVGFEComponentTransferElement>
    [<Emit("$0.getElementsByTagName('fecomposite')")>] abstract getElementsByTagName_fecomposite: unit -> NodeListOf<SVGFECompositeElement>
    [<Emit("$0.getElementsByTagName('feconvolvematrix')")>] abstract getElementsByTagName_feconvolvematrix: unit -> NodeListOf<SVGFEConvolveMatrixElement>
    [<Emit("$0.getElementsByTagName('fediffuselighting')")>] abstract getElementsByTagName_fediffuselighting: unit -> NodeListOf<SVGFEDiffuseLightingElement>
    [<Emit("$0.getElementsByTagName('fedisplacementmap')")>] abstract getElementsByTagName_fedisplacementmap: unit -> NodeListOf<SVGFEDisplacementMapElement>
    [<Emit("$0.getElementsByTagName('fedistantlight')")>] abstract getElementsByTagName_fedistantlight: unit -> NodeListOf<SVGFEDistantLightElement>
    [<Emit("$0.getElementsByTagName('feflood')")>] abstract getElementsByTagName_feflood: unit -> NodeListOf<SVGFEFloodElement>
    [<Emit("$0.getElementsByTagName('fefunca')")>] abstract getElementsByTagName_fefunca: unit -> NodeListOf<SVGFEFuncAElement>
    [<Emit("$0.getElementsByTagName('fefuncb')")>] abstract getElementsByTagName_fefuncb: unit -> NodeListOf<SVGFEFuncBElement>
    [<Emit("$0.getElementsByTagName('fefuncg')")>] abstract getElementsByTagName_fefuncg: unit -> NodeListOf<SVGFEFuncGElement>
    [<Emit("$0.getElementsByTagName('fefuncr')")>] abstract getElementsByTagName_fefuncr: unit -> NodeListOf<SVGFEFuncRElement>
    [<Emit("$0.getElementsByTagName('fegaussianblur')")>] abstract getElementsByTagName_fegaussianblur: unit -> NodeListOf<SVGFEGaussianBlurElement>
    [<Emit("$0.getElementsByTagName('feimage')")>] abstract getElementsByTagName_feimage: unit -> NodeListOf<SVGFEImageElement>
    [<Emit("$0.getElementsByTagName('femerge')")>] abstract getElementsByTagName_femerge: unit -> NodeListOf<SVGFEMergeElement>
    [<Emit("$0.getElementsByTagName('femergenode')")>] abstract getElementsByTagName_femergenode: unit -> NodeListOf<SVGFEMergeNodeElement>
    [<Emit("$0.getElementsByTagName('femorphology')")>] abstract getElementsByTagName_femorphology: unit -> NodeListOf<SVGFEMorphologyElement>
    [<Emit("$0.getElementsByTagName('feoffset')")>] abstract getElementsByTagName_feoffset: unit -> NodeListOf<SVGFEOffsetElement>
    [<Emit("$0.getElementsByTagName('fepointlight')")>] abstract getElementsByTagName_fepointlight: unit -> NodeListOf<SVGFEPointLightElement>
    [<Emit("$0.getElementsByTagName('fespecularlighting')")>] abstract getElementsByTagName_fespecularlighting: unit -> NodeListOf<SVGFESpecularLightingElement>
    [<Emit("$0.getElementsByTagName('fespotlight')")>] abstract getElementsByTagName_fespotlight: unit -> NodeListOf<SVGFESpotLightElement>
    [<Emit("$0.getElementsByTagName('fetile')")>] abstract getElementsByTagName_fetile: unit -> NodeListOf<SVGFETileElement>
    [<Emit("$0.getElementsByTagName('feturbulence')")>] abstract getElementsByTagName_feturbulence: unit -> NodeListOf<SVGFETurbulenceElement>
    [<Emit("$0.getElementsByTagName('fieldset')")>] abstract getElementsByTagName_fieldset: unit -> NodeListOf<HTMLFieldSetElement>
    [<Emit("$0.getElementsByTagName('figcaption')")>] abstract getElementsByTagName_figcaption: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('figure')")>] abstract getElementsByTagName_figure: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('filter')")>] abstract getElementsByTagName_filter: unit -> NodeListOf<SVGFilterElement>
    [<Emit("$0.getElementsByTagName('font')")>] abstract getElementsByTagName_font: unit -> NodeListOf<HTMLFontElement>
    [<Emit("$0.getElementsByTagName('footer')")>] abstract getElementsByTagName_footer: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('foreignobject')")>] abstract getElementsByTagName_foreignobject: unit -> NodeListOf<SVGForeignObjectElement>
    [<Emit("$0.getElementsByTagName('form')")>] abstract getElementsByTagName_form: unit -> NodeListOf<HTMLFormElement>
    [<Emit("$0.getElementsByTagName('frame')")>] abstract getElementsByTagName_frame: unit -> NodeListOf<HTMLFrameElement>
    [<Emit("$0.getElementsByTagName('frameset')")>] abstract getElementsByTagName_frameset: unit -> NodeListOf<HTMLFrameSetElement>
    [<Emit("$0.getElementsByTagName('g')")>] abstract getElementsByTagName_g: unit -> NodeListOf<SVGGElement>
    [<Emit("$0.getElementsByTagName('h1')")>] abstract getElementsByTagName_h1: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h2')")>] abstract getElementsByTagName_h2: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h3')")>] abstract getElementsByTagName_h3: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h4')")>] abstract getElementsByTagName_h4: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h5')")>] abstract getElementsByTagName_h5: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('h6')")>] abstract getElementsByTagName_h6: unit -> NodeListOf<HTMLHeadingElement>
    [<Emit("$0.getElementsByTagName('head')")>] abstract getElementsByTagName_head: unit -> NodeListOf<HTMLHeadElement>
    [<Emit("$0.getElementsByTagName('header')")>] abstract getElementsByTagName_header: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('hgroup')")>] abstract getElementsByTagName_hgroup: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('hr')")>] abstract getElementsByTagName_hr: unit -> NodeListOf<HTMLHRElement>
    [<Emit("$0.getElementsByTagName('html')")>] abstract getElementsByTagName_html: unit -> NodeListOf<HTMLHtmlElement>
    [<Emit("$0.getElementsByTagName('i')")>] abstract getElementsByTagName_i: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('iframe')")>] abstract getElementsByTagName_iframe: unit -> NodeListOf<HTMLIFrameElement>
    [<Emit("$0.getElementsByTagName('image')")>] abstract getElementsByTagName_image: unit -> NodeListOf<SVGImageElement>
    [<Emit("$0.getElementsByTagName('img')")>] abstract getElementsByTagName_img: unit -> NodeListOf<HTMLImageElement>
    [<Emit("$0.getElementsByTagName('input')")>] abstract getElementsByTagName_input: unit -> NodeListOf<HTMLInputElement>
    [<Emit("$0.getElementsByTagName('ins')")>] abstract getElementsByTagName_ins: unit -> NodeListOf<HTMLModElement>
    [<Emit("$0.getElementsByTagName('isindex')")>] abstract getElementsByTagName_isindex: unit -> NodeListOf<HTMLIsIndexElement>
    [<Emit("$0.getElementsByTagName('kbd')")>] abstract getElementsByTagName_kbd: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('keygen')")>] abstract getElementsByTagName_keygen: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('label')")>] abstract getElementsByTagName_label: unit -> NodeListOf<HTMLLabelElement>
    [<Emit("$0.getElementsByTagName('legend')")>] abstract getElementsByTagName_legend: unit -> NodeListOf<HTMLLegendElement>
    [<Emit("$0.getElementsByTagName('li')")>] abstract getElementsByTagName_li: unit -> NodeListOf<HTMLLIElement>
    [<Emit("$0.getElementsByTagName('line')")>] abstract getElementsByTagName_line: unit -> NodeListOf<SVGLineElement>
    [<Emit("$0.getElementsByTagName('lineargradient')")>] abstract getElementsByTagName_lineargradient: unit -> NodeListOf<SVGLinearGradientElement>
    [<Emit("$0.getElementsByTagName('link')")>] abstract getElementsByTagName_link: unit -> NodeListOf<HTMLLinkElement>
    [<Emit("$0.getElementsByTagName('listing')")>] abstract getElementsByTagName_listing: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('map')")>] abstract getElementsByTagName_map: unit -> NodeListOf<HTMLMapElement>
    [<Emit("$0.getElementsByTagName('mark')")>] abstract getElementsByTagName_mark: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('marker')")>] abstract getElementsByTagName_marker: unit -> NodeListOf<SVGMarkerElement>
    [<Emit("$0.getElementsByTagName('marquee')")>] abstract getElementsByTagName_marquee: unit -> NodeListOf<HTMLMarqueeElement>
    [<Emit("$0.getElementsByTagName('mask')")>] abstract getElementsByTagName_mask: unit -> NodeListOf<SVGMaskElement>
    [<Emit("$0.getElementsByTagName('menu')")>] abstract getElementsByTagName_menu: unit -> NodeListOf<HTMLMenuElement>
    [<Emit("$0.getElementsByTagName('meta')")>] abstract getElementsByTagName_meta: unit -> NodeListOf<HTMLMetaElement>
    [<Emit("$0.getElementsByTagName('metadata')")>] abstract getElementsByTagName_metadata: unit -> NodeListOf<SVGMetadataElement>
    [<Emit("$0.getElementsByTagName('nav')")>] abstract getElementsByTagName_nav: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('nextid')")>] abstract getElementsByTagName_nextid: unit -> NodeListOf<HTMLNextIdElement>
    [<Emit("$0.getElementsByTagName('nobr')")>] abstract getElementsByTagName_nobr: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('noframes')")>] abstract getElementsByTagName_noframes: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('noscript')")>] abstract getElementsByTagName_noscript: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('object')")>] abstract getElementsByTagName_object: unit -> NodeListOf<HTMLObjectElement>
    [<Emit("$0.getElementsByTagName('ol')")>] abstract getElementsByTagName_ol: unit -> NodeListOf<HTMLOListElement>
    [<Emit("$0.getElementsByTagName('optgroup')")>] abstract getElementsByTagName_optgroup: unit -> NodeListOf<HTMLOptGroupElement>
    [<Emit("$0.getElementsByTagName('option')")>] abstract getElementsByTagName_option: unit -> NodeListOf<HTMLOptionElement>
    [<Emit("$0.getElementsByTagName('p')")>] abstract getElementsByTagName_p: unit -> NodeListOf<HTMLParagraphElement>
    [<Emit("$0.getElementsByTagName('param')")>] abstract getElementsByTagName_param: unit -> NodeListOf<HTMLParamElement>
    [<Emit("$0.getElementsByTagName('path')")>] abstract getElementsByTagName_path: unit -> NodeListOf<SVGPathElement>
    [<Emit("$0.getElementsByTagName('pattern')")>] abstract getElementsByTagName_pattern: unit -> NodeListOf<SVGPatternElement>
    [<Emit("$0.getElementsByTagName('plaintext')")>] abstract getElementsByTagName_plaintext: unit -> NodeListOf<HTMLBlockElement>
    [<Emit("$0.getElementsByTagName('polygon')")>] abstract getElementsByTagName_polygon: unit -> NodeListOf<SVGPolygonElement>
    [<Emit("$0.getElementsByTagName('polyline')")>] abstract getElementsByTagName_polyline: unit -> NodeListOf<SVGPolylineElement>
    [<Emit("$0.getElementsByTagName('pre')")>] abstract getElementsByTagName_pre: unit -> NodeListOf<HTMLPreElement>
    [<Emit("$0.getElementsByTagName('progress')")>] abstract getElementsByTagName_progress: unit -> NodeListOf<HTMLProgressElement>
    [<Emit("$0.getElementsByTagName('q')")>] abstract getElementsByTagName_q: unit -> NodeListOf<HTMLQuoteElement>
    [<Emit("$0.getElementsByTagName('radialgradient')")>] abstract getElementsByTagName_radialgradient: unit -> NodeListOf<SVGRadialGradientElement>
    [<Emit("$0.getElementsByTagName('rect')")>] abstract getElementsByTagName_rect: unit -> NodeListOf<SVGRectElement>
    [<Emit("$0.getElementsByTagName('rt')")>] abstract getElementsByTagName_rt: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('ruby')")>] abstract getElementsByTagName_ruby: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('s')")>] abstract getElementsByTagName_s: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('samp')")>] abstract getElementsByTagName_samp: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('script')")>] abstract getElementsByTagName_script: unit -> NodeListOf<HTMLScriptElement>
    [<Emit("$0.getElementsByTagName('section')")>] abstract getElementsByTagName_section: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('select')")>] abstract getElementsByTagName_select: unit -> NodeListOf<HTMLSelectElement>
    [<Emit("$0.getElementsByTagName('small')")>] abstract getElementsByTagName_small: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('source')")>] abstract getElementsByTagName_source: unit -> NodeListOf<HTMLSourceElement>
    [<Emit("$0.getElementsByTagName('span')")>] abstract getElementsByTagName_span: unit -> NodeListOf<HTMLSpanElement>
    [<Emit("$0.getElementsByTagName('stop')")>] abstract getElementsByTagName_stop: unit -> NodeListOf<SVGStopElement>
    [<Emit("$0.getElementsByTagName('strike')")>] abstract getElementsByTagName_strike: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('strong')")>] abstract getElementsByTagName_strong: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('style')")>] abstract getElementsByTagName_style: unit -> NodeListOf<HTMLStyleElement>
    [<Emit("$0.getElementsByTagName('sub')")>] abstract getElementsByTagName_sub: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('sup')")>] abstract getElementsByTagName_sup: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('svg')")>] abstract getElementsByTagName_svg: unit -> NodeListOf<SVGSVGElement>
    [<Emit("$0.getElementsByTagName('switch')")>] abstract getElementsByTagName_switch: unit -> NodeListOf<SVGSwitchElement>
    [<Emit("$0.getElementsByTagName('symbol')")>] abstract getElementsByTagName_symbol: unit -> NodeListOf<SVGSymbolElement>
    [<Emit("$0.getElementsByTagName('table')")>] abstract getElementsByTagName_table: unit -> NodeListOf<HTMLTableElement>
    [<Emit("$0.getElementsByTagName('tbody')")>] abstract getElementsByTagName_tbody: unit -> NodeListOf<HTMLTableSectionElement>
    [<Emit("$0.getElementsByTagName('td')")>] abstract getElementsByTagName_td: unit -> NodeListOf<HTMLTableDataCellElement>
    [<Emit("$0.getElementsByTagName('text')")>] abstract getElementsByTagName_text: unit -> NodeListOf<SVGTextElement>
    [<Emit("$0.getElementsByTagName('textpath')")>] abstract getElementsByTagName_textpath: unit -> NodeListOf<SVGTextPathElement>
    [<Emit("$0.getElementsByTagName('textarea')")>] abstract getElementsByTagName_textarea: unit -> NodeListOf<HTMLTextAreaElement>
    [<Emit("$0.getElementsByTagName('tfoot')")>] abstract getElementsByTagName_tfoot: unit -> NodeListOf<HTMLTableSectionElement>
    [<Emit("$0.getElementsByTagName('th')")>] abstract getElementsByTagName_th: unit -> NodeListOf<HTMLTableHeaderCellElement>
    [<Emit("$0.getElementsByTagName('thead')")>] abstract getElementsByTagName_thead: unit -> NodeListOf<HTMLTableSectionElement>
    [<Emit("$0.getElementsByTagName('title')")>] abstract getElementsByTagName_title: unit -> NodeListOf<HTMLTitleElement>
    [<Emit("$0.getElementsByTagName('tr')")>] abstract getElementsByTagName_tr: unit -> NodeListOf<HTMLTableRowElement>
    [<Emit("$0.getElementsByTagName('track')")>] abstract getElementsByTagName_track: unit -> NodeListOf<HTMLTrackElement>
    [<Emit("$0.getElementsByTagName('tspan')")>] abstract getElementsByTagName_tspan: unit -> NodeListOf<SVGTSpanElement>
    [<Emit("$0.getElementsByTagName('tt')")>] abstract getElementsByTagName_tt: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('u')")>] abstract getElementsByTagName_u: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('ul')")>] abstract getElementsByTagName_ul: unit -> NodeListOf<HTMLUListElement>
    [<Emit("$0.getElementsByTagName('use')")>] abstract getElementsByTagName_use: unit -> NodeListOf<SVGUseElement>
    [<Emit("$0.getElementsByTagName('var')")>] abstract getElementsByTagName_var: unit -> NodeListOf<HTMLPhraseElement>
    [<Emit("$0.getElementsByTagName('video')")>] abstract getElementsByTagName_video: unit -> NodeListOf<HTMLVideoElement>
    [<Emit("$0.getElementsByTagName('view')")>] abstract getElementsByTagName_view: unit -> NodeListOf<SVGViewElement>
    [<Emit("$0.getElementsByTagName('wbr')")>] abstract getElementsByTagName_wbr: unit -> NodeListOf<HTMLElement>
    [<Emit("$0.getElementsByTagName('x-ms-webview')")>] abstract ``getElementsByTagName_x-ms-webview``: unit -> NodeListOf<MSHTMLWebViewElement>
    [<Emit("$0.getElementsByTagName('xmp')")>] abstract getElementsByTagName_xmp: unit -> NodeListOf<HTMLBlockElement>
    abstract getElementsByTagName: name: string -> NodeListOf<Element>
    abstract getElementsByTagNameNS: namespaceURI: string * localName: string -> NodeListOf<Element>
    abstract hasAttribute: name: string -> bool
    abstract hasAttributeNS: namespaceURI: string * localName: string -> bool
    abstract msGetRegionContent: unit -> MSRangeCollection
    abstract msGetUntransformedBounds: unit -> ClientRect
    abstract msMatchesSelector: selectors: string -> bool
    abstract msReleasePointerCapture: pointerId: float -> unit
    abstract msSetPointerCapture: pointerId: float -> unit
    abstract msZoomTo: args: MsZoomToOptions -> unit
    abstract releasePointerCapture: pointerId: float -> unit
    abstract removeAttribute: ?name: string -> unit
    abstract removeAttributeNS: namespaceURI: string * localName: string -> unit
    abstract removeAttributeNode: oldAttr: Attr -> Attr
    abstract requestFullscreen: unit -> unit
    abstract requestPointerLock: unit -> unit
    abstract setAttribute: name: string * value: string -> unit
    abstract setAttributeNS: namespaceURI: string * qualifiedName: string * value: string -> unit
    abstract setAttributeNode: newAttr: Attr -> Attr
    abstract setAttributeNodeNS: newAttr: Attr -> Attr
    abstract setPointerCapture: pointerId: float -> unit
    abstract webkitMatchesSelector: selectors: string -> bool
    abstract webkitRequestFullScreen: unit -> unit
    abstract webkitRequestFullscreen: unit -> unit
    abstract getElementsByClassName: classNames: string -> NodeListOf<Element>
    abstract matches: selector: string -> bool
    [<Emit("$0.getElementsByTagName('picture')")>] abstract getElementsByTagName_picture: unit -> NodeListOf<HTMLPictureElement>
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and ErrorEvent =
    inherit Event
    abstract colno: float with get, set
    abstract error: obj with get, set
    abstract filename: string with get, set
    abstract lineno: float with get, set
    abstract message: string with get, set
    abstract initErrorEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * messageArg: string * filenameArg: string * linenoArg: float -> unit

and Event =
    abstract bubbles: bool with get, set
    abstract cancelBubble: bool with get, set
    abstract cancelable: bool with get, set
    abstract currentTarget: EventTarget with get, set
    abstract defaultPrevented: bool with get, set
    abstract eventPhase: float with get, set
    abstract isTrusted: bool with get, set
    abstract returnValue: bool with get, set
    abstract srcElement: Element with get, set
    abstract target: EventTarget with get, set
    abstract timeStamp: float with get, set
    abstract ``type``: string with get, set
    abstract AT_TARGET: float with get, set
    abstract BUBBLING_PHASE: float with get, set
    abstract CAPTURING_PHASE: float with get, set
    abstract initEvent: eventTypeArg: string * canBubbleArg: bool * cancelableArg: bool -> unit
    abstract preventDefault: unit -> unit
    abstract stopImmediatePropagation: unit -> unit
    abstract stopPropagation: unit -> unit

and EventTarget =
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit
    abstract dispatchEvent: evt: Event -> bool
    abstract removeEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and External =
    interface end

and File =
    inherit Blob
    abstract lastModifiedDate: obj with get, set
    abstract name: string with get, set

and FileList =
    abstract length: float with get, set
    abstract item: index: float -> File

and FileReader =
    inherit EventTarget
    inherit MSBaseReader
    abstract error: DOMError with get, set
    abstract readAsArrayBuffer: blob: Blob -> unit
    abstract readAsBinaryString: blob: Blob -> unit
    abstract readAsDataURL: blob: Blob -> unit
    abstract readAsText: blob: Blob * ?encoding: string -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and FocusEvent =
    inherit UIEvent
    abstract relatedTarget: EventTarget with get, set
    abstract initFocusEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * relatedTargetArg: EventTarget -> unit

and FormData =
    abstract append: name: obj * value: obj * ?blobName: string -> unit

and GainNode =
    inherit AudioNode
    abstract gain: AudioParam with get, set

and Gamepad =
    abstract axes: ResizeArray<float> with get, set
    abstract buttons: ResizeArray<GamepadButton> with get, set
    abstract connected: bool with get, set
    abstract id: string with get, set
    abstract index: float with get, set
    abstract mapping: string with get, set
    abstract timestamp: float with get, set

and GamepadButton =
    abstract pressed: bool with get, set
    abstract value: float with get, set

and GamepadEvent =
    inherit Event
    abstract gamepad: Gamepad with get, set

and Geolocation =
    abstract clearWatch: watchId: float -> unit
    abstract getCurrentPosition: successCallback: PositionCallback * ?errorCallback: PositionErrorCallback * ?options: PositionOptions -> unit
    abstract watchPosition: successCallback: PositionCallback * ?errorCallback: PositionErrorCallback * ?options: PositionOptions -> float

and HTMLAllCollection =
    inherit HTMLCollection
    abstract namedItem: name: string -> Element

and HTMLAnchorElement =
    inherit HTMLElement
    abstract Methods: string with get, set
    abstract charset: string with get, set
    abstract coords: string with get, set
    abstract hash: string with get, set
    abstract host: string with get, set
    abstract hostname: string with get, set
    abstract href: string with get, set
    abstract hreflang: string with get, set
    abstract mimeType: string with get, set
    abstract name: string with get, set
    abstract nameProp: string with get, set
    abstract pathname: string with get, set
    abstract port: string with get, set
    abstract protocol: string with get, set
    abstract protocolLong: string with get, set
    abstract rel: string with get, set
    abstract rev: string with get, set
    abstract search: string with get, set
    abstract shape: string with get, set
    abstract target: string with get, set
    abstract text: string with get, set
    abstract ``type``: string with get, set
    abstract urn: string with get, set
    abstract toString: unit -> string

and HTMLAppletElement =
    inherit HTMLElement
    abstract BaseHref: string with get, set
    abstract align: string with get, set
    abstract alt: string with get, set
    abstract altHtml: string with get, set
    abstract archive: string with get, set
    abstract border: string with get, set
    abstract code: string with get, set
    abstract codeBase: string with get, set
    abstract codeType: string with get, set
    abstract contentDocument: Document with get, set
    abstract data: string with get, set
    abstract declare: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract height: string with get, set
    abstract hspace: float with get, set
    abstract name: string with get, set
    abstract ``object``: string with get, set
    abstract standby: string with get, set
    abstract ``type``: string with get, set
    abstract useMap: string with get, set
    abstract vspace: float with get, set
    abstract width: float with get, set

and HTMLAreaElement =
    inherit HTMLElement
    abstract alt: string with get, set
    abstract coords: string with get, set
    abstract hash: string with get, set
    abstract host: string with get, set
    abstract hostname: string with get, set
    abstract href: string with get, set
    abstract noHref: bool with get, set
    abstract pathname: string with get, set
    abstract port: string with get, set
    abstract protocol: string with get, set
    abstract rel: string with get, set
    abstract search: string with get, set
    abstract shape: string with get, set
    abstract target: string with get, set
    abstract toString: unit -> string

and HTMLAreasCollection =
    inherit HTMLCollection
    abstract add: element: HTMLElement * ?before: U2<HTMLElement, float> -> unit
    abstract remove: ?index: float -> unit

and HTMLAudioElement =
    inherit HTMLMediaElement

and HTMLBRElement =
    inherit HTMLElement
    abstract clear: string with get, set

and HTMLBaseElement =
    inherit HTMLElement
    abstract href: string with get, set
    abstract target: string with get, set

and HTMLBaseFontElement =
    inherit HTMLElement
    inherit DOML2DeprecatedColorProperty
    abstract face: string with get, set
    abstract size: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLBlockElement =
    inherit HTMLElement
    abstract cite: string with get, set
    abstract clear: string with get, set
    abstract width: float with get, set

and HTMLBodyElement =
    inherit HTMLElement
    abstract aLink: obj with get, set
    abstract background: string with get, set
    abstract bgColor: obj with get, set
    abstract bgProperties: string with get, set
    abstract link: obj with get, set
    abstract noWrap: bool with get, set
    abstract onafterprint: Func<Event, obj> with get, set
    abstract onbeforeprint: Func<Event, obj> with get, set
    abstract onbeforeunload: Func<BeforeUnloadEvent, obj> with get, set
    abstract onblur: Func<FocusEvent, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onfocus: Func<FocusEvent, obj> with get, set
    abstract onhashchange: Func<HashChangeEvent, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onmessage: Func<MessageEvent, obj> with get, set
    abstract onoffline: Func<Event, obj> with get, set
    abstract ononline: Func<Event, obj> with get, set
    abstract onorientationchange: Func<Event, obj> with get, set
    abstract onpagehide: Func<PageTransitionEvent, obj> with get, set
    abstract onpageshow: Func<PageTransitionEvent, obj> with get, set
    abstract onpopstate: Func<PopStateEvent, obj> with get, set
    abstract onresize: Func<UIEvent, obj> with get, set
    abstract onstorage: Func<StorageEvent, obj> with get, set
    abstract onunload: Func<Event, obj> with get, set
    abstract text: obj with get, set
    abstract vLink: obj with get, set
    abstract createTextRange: unit -> TextRange
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('afterprint',$1...)")>] abstract addEventListener_afterprint: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeprint',$1...)")>] abstract addEventListener_beforeprint: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeunload',$1...)")>] abstract addEventListener_beforeunload: listener: Func<BeforeUnloadEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('hashchange',$1...)")>] abstract addEventListener_hashchange: listener: Func<HashChangeEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('message',$1...)")>] abstract addEventListener_message: listener: Func<MessageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('offline',$1...)")>] abstract addEventListener_offline: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('online',$1...)")>] abstract addEventListener_online: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('orientationchange',$1...)")>] abstract addEventListener_orientationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pagehide',$1...)")>] abstract addEventListener_pagehide: listener: Func<PageTransitionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pageshow',$1...)")>] abstract addEventListener_pageshow: listener: Func<PageTransitionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('popstate',$1...)")>] abstract addEventListener_popstate: listener: Func<PopStateEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('resize',$1...)")>] abstract addEventListener_resize: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('storage',$1...)")>] abstract addEventListener_storage: listener: Func<StorageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('unload',$1...)")>] abstract addEventListener_unload: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLButtonElement =
    inherit HTMLElement
    abstract autofocus: bool with get, set
    abstract disabled: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract formAction: string with get, set
    abstract formEnctype: string with get, set
    abstract formMethod: string with get, set
    abstract formNoValidate: string with get, set
    abstract formTarget: string with get, set
    abstract name: string with get, set
    abstract status: obj with get, set
    abstract ``type``: string with get, set
    abstract validationMessage: string with get, set
    abstract validity: ValidityState with get, set
    abstract value: string with get, set
    abstract willValidate: bool with get, set
    abstract checkValidity: unit -> bool
    abstract createTextRange: unit -> TextRange
    abstract setCustomValidity: error: string -> unit

and HTMLCanvasElement =
    inherit HTMLElement
    abstract height: float with get, set
    abstract width: float with get, set
    [<Emit("$0.getContext('2d')")>] abstract getContext_2d: unit -> CanvasRenderingContext2D
    [<Emit("$0.getContext('experimental-webgl')")>] abstract ``getContext_experimental-webgl``: unit -> WebGLRenderingContext
    abstract getContext: contextId: string * [<ParamArray>] args: obj[] -> U2<CanvasRenderingContext2D, WebGLRenderingContext>
    abstract msToBlob: unit -> Blob
    abstract toDataURL: ?``type``: string * [<ParamArray>] args: obj[] -> string
    abstract toBlob: unit -> Blob

and HTMLCollection =
    abstract length: float with get, set
    abstract item: ?nameOrIndex: obj * ?optionalIndex: obj -> Element
    abstract namedItem: name: string -> Element

and HTMLDDElement =
    inherit HTMLElement
    abstract noWrap: bool with get, set

and HTMLDListElement =
    inherit HTMLElement
    abstract compact: bool with get, set

and HTMLDTElement =
    inherit HTMLElement
    abstract noWrap: bool with get, set

and HTMLDataListElement =
    inherit HTMLElement
    abstract options: HTMLCollection with get, set

and HTMLDirectoryElement =
    inherit HTMLElement
    abstract compact: bool with get, set

and HTMLDivElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract noWrap: bool with get, set

and HTMLDocument =
    inherit Document

and HTMLElement =
    inherit Element
    abstract accessKey: string with get, set
    abstract children: HTMLCollection with get, set
    abstract contentEditable: string with get, set
    abstract dataset: DOMStringMap with get, set
    abstract dir: string with get, set
    abstract draggable: bool with get, set
    abstract hidden: bool with get, set
    abstract hideFocus: bool with get, set
    abstract innerHTML: string with get, set
    abstract innerText: string with get, set
    abstract isContentEditable: bool with get, set
    abstract lang: string with get, set
    abstract offsetHeight: float with get, set
    abstract offsetLeft: float with get, set
    abstract offsetParent: Element with get, set
    abstract offsetTop: float with get, set
    abstract offsetWidth: float with get, set
    abstract onabort: Func<Event, obj> with get, set
    abstract onactivate: Func<UIEvent, obj> with get, set
    abstract onbeforeactivate: Func<UIEvent, obj> with get, set
    abstract onbeforecopy: Func<DragEvent, obj> with get, set
    abstract onbeforecut: Func<DragEvent, obj> with get, set
    abstract onbeforedeactivate: Func<UIEvent, obj> with get, set
    abstract onbeforepaste: Func<DragEvent, obj> with get, set
    abstract onblur: Func<FocusEvent, obj> with get, set
    abstract oncanplay: Func<Event, obj> with get, set
    abstract oncanplaythrough: Func<Event, obj> with get, set
    abstract onchange: Func<Event, obj> with get, set
    abstract onclick: Func<MouseEvent, obj> with get, set
    abstract oncontextmenu: Func<PointerEvent, obj> with get, set
    abstract oncopy: Func<DragEvent, obj> with get, set
    abstract oncuechange: Func<Event, obj> with get, set
    abstract oncut: Func<DragEvent, obj> with get, set
    abstract ondblclick: Func<MouseEvent, obj> with get, set
    abstract ondeactivate: Func<UIEvent, obj> with get, set
    abstract ondrag: Func<DragEvent, obj> with get, set
    abstract ondragend: Func<DragEvent, obj> with get, set
    abstract ondragenter: Func<DragEvent, obj> with get, set
    abstract ondragleave: Func<DragEvent, obj> with get, set
    abstract ondragover: Func<DragEvent, obj> with get, set
    abstract ondragstart: Func<DragEvent, obj> with get, set
    abstract ondrop: Func<DragEvent, obj> with get, set
    abstract ondurationchange: Func<Event, obj> with get, set
    abstract onemptied: Func<Event, obj> with get, set
    abstract onended: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onfocus: Func<FocusEvent, obj> with get, set
    abstract oninput: Func<Event, obj> with get, set
    abstract onkeydown: Func<KeyboardEvent, obj> with get, set
    abstract onkeypress: Func<KeyboardEvent, obj> with get, set
    abstract onkeyup: Func<KeyboardEvent, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onloadeddata: Func<Event, obj> with get, set
    abstract onloadedmetadata: Func<Event, obj> with get, set
    abstract onloadstart: Func<Event, obj> with get, set
    abstract onmousedown: Func<MouseEvent, obj> with get, set
    abstract onmouseenter: Func<MouseEvent, obj> with get, set
    abstract onmouseleave: Func<MouseEvent, obj> with get, set
    abstract onmousemove: Func<MouseEvent, obj> with get, set
    abstract onmouseout: Func<MouseEvent, obj> with get, set
    abstract onmouseover: Func<MouseEvent, obj> with get, set
    abstract onmouseup: Func<MouseEvent, obj> with get, set
    abstract onmousewheel: Func<MouseWheelEvent, obj> with get, set
    abstract onmscontentzoom: Func<UIEvent, obj> with get, set
    abstract onmsmanipulationstatechanged: Func<MSManipulationEvent, obj> with get, set
    abstract onpaste: Func<DragEvent, obj> with get, set
    abstract onpause: Func<Event, obj> with get, set
    abstract onplay: Func<Event, obj> with get, set
    abstract onplaying: Func<Event, obj> with get, set
    abstract onprogress: Func<ProgressEvent, obj> with get, set
    abstract onratechange: Func<Event, obj> with get, set
    abstract onreset: Func<Event, obj> with get, set
    abstract onscroll: Func<UIEvent, obj> with get, set
    abstract onseeked: Func<Event, obj> with get, set
    abstract onseeking: Func<Event, obj> with get, set
    abstract onselect: Func<UIEvent, obj> with get, set
    abstract onselectstart: Func<Event, obj> with get, set
    abstract onstalled: Func<Event, obj> with get, set
    abstract onsubmit: Func<Event, obj> with get, set
    abstract onsuspend: Func<Event, obj> with get, set
    abstract ontimeupdate: Func<Event, obj> with get, set
    abstract onvolumechange: Func<Event, obj> with get, set
    abstract onwaiting: Func<Event, obj> with get, set
    abstract outerHTML: string with get, set
    abstract outerText: string with get, set
    abstract spellcheck: bool with get, set
    abstract style: CSSStyleDeclaration with get, set
    abstract tabIndex: float with get, set
    abstract title: string with get, set
    abstract blur: unit -> unit
    abstract click: unit -> unit
    abstract dragDrop: unit -> bool
    abstract focus: unit -> unit
    abstract insertAdjacentElement: position: string * insertedElement: Element -> Element
    abstract insertAdjacentHTML: where: string * html: string -> unit
    abstract insertAdjacentText: where: string * text: string -> unit
    abstract msGetInputContext: unit -> MSInputMethodContext
    abstract scrollIntoView: ?top: bool -> unit
    abstract setActive: unit -> unit
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLEmbedElement =
    inherit HTMLElement
    inherit GetSVGDocument
    abstract height: string with get, set
    abstract hidden: obj with get, set
    abstract msPlayToDisabled: bool with get, set
    abstract msPlayToPreferredSourceUri: string with get, set
    abstract msPlayToPrimary: bool with get, set
    abstract msPlayToSource: obj with get, set
    abstract name: string with get, set
    abstract palette: string with get, set
    abstract pluginspage: string with get, set
    abstract readyState: string with get, set
    abstract src: string with get, set
    abstract units: string with get, set
    abstract width: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLFieldSetElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract disabled: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract validationMessage: string with get, set
    abstract validity: ValidityState with get, set
    abstract willValidate: bool with get, set
    abstract checkValidity: unit -> bool
    abstract setCustomValidity: error: string -> unit

and HTMLFontElement =
    inherit HTMLElement
    inherit DOML2DeprecatedColorProperty
    inherit DOML2DeprecatedSizeProperty
    abstract face: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLFormElement =
    inherit HTMLElement
    abstract acceptCharset: string with get, set
    abstract action: string with get, set
    abstract autocomplete: string with get, set
    abstract elements: HTMLCollection with get, set
    abstract encoding: string with get, set
    abstract enctype: string with get, set
    abstract length: float with get, set
    abstract ``method``: string with get, set
    abstract name: string with get, set
    abstract noValidate: bool with get, set
    abstract target: string with get, set
    abstract checkValidity: unit -> bool
    abstract item: ?name: obj * ?index: obj -> obj
    abstract namedItem: name: string -> obj
    abstract reset: unit -> unit
    abstract submit: unit -> unit

and HTMLFrameElement =
    inherit HTMLElement
    inherit GetSVGDocument
    abstract border: string with get, set
    abstract borderColor: obj with get, set
    abstract contentDocument: Document with get, set
    abstract contentWindow: Window with get, set
    abstract frameBorder: string with get, set
    abstract frameSpacing: obj with get, set
    abstract height: U2<string, float> with get, set
    abstract longDesc: string with get, set
    abstract marginHeight: string with get, set
    abstract marginWidth: string with get, set
    abstract name: string with get, set
    abstract noResize: bool with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract scrolling: string with get, set
    abstract security: obj with get, set
    abstract src: string with get, set
    abstract width: U2<string, float> with get, set
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLFrameSetElement =
    inherit HTMLElement
    abstract border: string with get, set
    abstract borderColor: obj with get, set
    abstract cols: string with get, set
    abstract frameBorder: string with get, set
    abstract frameSpacing: obj with get, set
    abstract name: string with get, set
    abstract onafterprint: Func<Event, obj> with get, set
    abstract onbeforeprint: Func<Event, obj> with get, set
    abstract onbeforeunload: Func<BeforeUnloadEvent, obj> with get, set
    abstract onblur: Func<FocusEvent, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onfocus: Func<FocusEvent, obj> with get, set
    abstract onhashchange: Func<HashChangeEvent, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onmessage: Func<MessageEvent, obj> with get, set
    abstract onoffline: Func<Event, obj> with get, set
    abstract ononline: Func<Event, obj> with get, set
    abstract onorientationchange: Func<Event, obj> with get, set
    abstract onpagehide: Func<PageTransitionEvent, obj> with get, set
    abstract onpageshow: Func<PageTransitionEvent, obj> with get, set
    abstract onresize: Func<UIEvent, obj> with get, set
    abstract onstorage: Func<StorageEvent, obj> with get, set
    abstract onunload: Func<Event, obj> with get, set
    abstract rows: string with get, set
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeprint',$1...)")>] abstract addEventListener_beforeprint: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeunload',$1...)")>] abstract addEventListener_beforeunload: listener: Func<BeforeUnloadEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('hashchange',$1...)")>] abstract addEventListener_hashchange: listener: Func<HashChangeEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('message',$1...)")>] abstract addEventListener_message: listener: Func<MessageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('offline',$1...)")>] abstract addEventListener_offline: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('online',$1...)")>] abstract addEventListener_online: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('orientationchange',$1...)")>] abstract addEventListener_orientationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pagehide',$1...)")>] abstract addEventListener_pagehide: listener: Func<PageTransitionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pageshow',$1...)")>] abstract addEventListener_pageshow: listener: Func<PageTransitionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('resize',$1...)")>] abstract addEventListener_resize: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('storage',$1...)")>] abstract addEventListener_storage: listener: Func<StorageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('unload',$1...)")>] abstract addEventListener_unload: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLHRElement =
    inherit HTMLElement
    inherit DOML2DeprecatedColorProperty
    inherit DOML2DeprecatedSizeProperty
    abstract align: string with get, set
    abstract noShade: bool with get, set
    abstract width: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLHeadElement =
    inherit HTMLElement
    abstract profile: string with get, set

and HTMLHeadingElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract clear: string with get, set

and HTMLHtmlElement =
    inherit HTMLElement
    abstract version: string with get, set

and HTMLIFrameElement =
    inherit HTMLElement
    inherit GetSVGDocument
    abstract align: string with get, set
    abstract allowFullscreen: bool with get, set
    abstract border: string with get, set
    abstract contentDocument: Document with get, set
    abstract contentWindow: Window with get, set
    abstract frameBorder: string with get, set
    abstract frameSpacing: obj with get, set
    abstract height: string with get, set
    abstract hspace: float with get, set
    abstract longDesc: string with get, set
    abstract marginHeight: string with get, set
    abstract marginWidth: string with get, set
    abstract name: string with get, set
    abstract noResize: bool with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract sandbox: DOMSettableTokenList with get, set
    abstract scrolling: string with get, set
    abstract security: obj with get, set
    abstract src: string with get, set
    abstract vspace: float with get, set
    abstract width: string with get, set
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLImageElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract alt: string with get, set
    abstract border: string with get, set
    abstract complete: bool with get, set
    abstract crossOrigin: string with get, set
    abstract currentSrc: string with get, set
    abstract height: float with get, set
    abstract hspace: float with get, set
    abstract isMap: bool with get, set
    abstract longDesc: string with get, set
    abstract msPlayToDisabled: bool with get, set
    abstract msPlayToPreferredSourceUri: string with get, set
    abstract msPlayToPrimary: bool with get, set
    abstract msPlayToSource: obj with get, set
    abstract name: string with get, set
    abstract naturalHeight: float with get, set
    abstract naturalWidth: float with get, set
    abstract src: string with get, set
    abstract srcset: string with get, set
    abstract useMap: string with get, set
    abstract vspace: float with get, set
    abstract width: float with get, set
    abstract x: float with get, set
    abstract y: float with get, set
    abstract msGetAsCastingSource: unit -> obj

and HTMLInputElement =
    inherit HTMLElement
    abstract accept: string with get, set
    abstract align: string with get, set
    abstract alt: string with get, set
    abstract autocomplete: string with get, set
    abstract autofocus: bool with get, set
    abstract border: string with get, set
    abstract ``checked``: bool with get, set
    abstract complete: bool with get, set
    abstract defaultChecked: bool with get, set
    abstract defaultValue: string with get, set
    abstract disabled: bool with get, set
    abstract files: FileList with get, set
    abstract form: HTMLFormElement with get, set
    abstract formAction: string with get, set
    abstract formEnctype: string with get, set
    abstract formMethod: string with get, set
    abstract formNoValidate: string with get, set
    abstract formTarget: string with get, set
    abstract height: string with get, set
    abstract hspace: float with get, set
    abstract indeterminate: bool with get, set
    abstract list: HTMLElement with get, set
    abstract max: string with get, set
    abstract maxLength: float with get, set
    abstract min: string with get, set
    abstract multiple: bool with get, set
    abstract name: string with get, set
    abstract pattern: string with get, set
    abstract placeholder: string with get, set
    abstract readOnly: bool with get, set
    abstract required: bool with get, set
    abstract selectionEnd: float with get, set
    abstract selectionStart: float with get, set
    abstract size: float with get, set
    abstract src: string with get, set
    abstract status: bool with get, set
    abstract step: string with get, set
    abstract ``type``: string with get, set
    abstract useMap: string with get, set
    abstract validationMessage: string with get, set
    abstract validity: ValidityState with get, set
    abstract value: string with get, set
    abstract valueAsDate: DateTime with get, set
    abstract valueAsNumber: float with get, set
    abstract vspace: float with get, set
    abstract width: string with get, set
    abstract willValidate: bool with get, set
    abstract checkValidity: unit -> bool
    abstract createTextRange: unit -> TextRange
    abstract select: unit -> unit
    abstract setCustomValidity: error: string -> unit
    abstract setSelectionRange: start: float * ``end``: float -> unit
    abstract stepDown: ?n: float -> unit
    abstract stepUp: ?n: float -> unit

and HTMLIsIndexElement =
    inherit HTMLElement
    abstract action: string with get, set
    abstract form: HTMLFormElement with get, set
    abstract prompt: string with get, set

and HTMLLIElement =
    inherit HTMLElement
    abstract ``type``: string with get, set
    abstract value: float with get, set

and HTMLLabelElement =
    inherit HTMLElement
    abstract form: HTMLFormElement with get, set
    abstract htmlFor: string with get, set

and HTMLLegendElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract form: HTMLFormElement with get, set

and HTMLLinkElement =
    inherit HTMLElement
    inherit LinkStyle
    abstract charset: string with get, set
    abstract disabled: bool with get, set
    abstract href: string with get, set
    abstract hreflang: string with get, set
    abstract media: string with get, set
    abstract rel: string with get, set
    abstract rev: string with get, set
    abstract target: string with get, set
    abstract ``type``: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLMapElement =
    inherit HTMLElement
    abstract areas: HTMLAreasCollection with get, set
    abstract name: string with get, set

and HTMLMarqueeElement =
    inherit HTMLElement
    abstract behavior: string with get, set
    abstract bgColor: obj with get, set
    abstract direction: string with get, set
    abstract height: string with get, set
    abstract hspace: float with get, set
    abstract loop: float with get, set
    abstract onbounce: Func<Event, obj> with get, set
    abstract onfinish: Func<Event, obj> with get, set
    abstract onstart: Func<Event, obj> with get, set
    abstract scrollAmount: float with get, set
    abstract scrollDelay: float with get, set
    abstract trueSpeed: bool with get, set
    abstract vspace: float with get, set
    abstract width: string with get, set
    abstract start: unit -> unit
    abstract stop: unit -> unit
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('bounce',$1...)")>] abstract addEventListener_bounce: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('finish',$1...)")>] abstract addEventListener_finish: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('start',$1...)")>] abstract addEventListener_start: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLMediaElement =
    inherit HTMLElement
    abstract audioTracks: AudioTrackList with get, set
    abstract autoplay: bool with get, set
    abstract buffered: TimeRanges with get, set
    abstract controls: bool with get, set
    abstract currentSrc: string with get, set
    abstract currentTime: float with get, set
    abstract defaultMuted: bool with get, set
    abstract defaultPlaybackRate: float with get, set
    abstract duration: float with get, set
    abstract ended: bool with get, set
    abstract error: MediaError with get, set
    abstract loop: bool with get, set
    abstract msAudioCategory: string with get, set
    abstract msAudioDeviceType: string with get, set
    abstract msGraphicsTrustStatus: MSGraphicsTrust with get, set
    abstract msKeys: MSMediaKeys with get, set
    abstract msPlayToDisabled: bool with get, set
    abstract msPlayToPreferredSourceUri: string with get, set
    abstract msPlayToPrimary: bool with get, set
    abstract msPlayToSource: obj with get, set
    abstract msRealTime: bool with get, set
    abstract muted: bool with get, set
    abstract networkState: float with get, set
    abstract onmsneedkey: Func<MSMediaKeyNeededEvent, obj> with get, set
    abstract paused: bool with get, set
    abstract playbackRate: float with get, set
    abstract played: TimeRanges with get, set
    abstract preload: string with get, set
    abstract readyState: float with get, set
    abstract seekable: TimeRanges with get, set
    abstract seeking: bool with get, set
    abstract src: string with get, set
    abstract textTracks: TextTrackList with get, set
    abstract videoTracks: VideoTrackList with get, set
    abstract volume: float with get, set
    abstract HAVE_CURRENT_DATA: float with get, set
    abstract HAVE_ENOUGH_DATA: float with get, set
    abstract HAVE_FUTURE_DATA: float with get, set
    abstract HAVE_METADATA: float with get, set
    abstract HAVE_NOTHING: float with get, set
    abstract NETWORK_EMPTY: float with get, set
    abstract NETWORK_IDLE: float with get, set
    abstract NETWORK_LOADING: float with get, set
    abstract NETWORK_NO_SOURCE: float with get, set
    abstract addTextTrack: kind: string * ?label: string * ?language: string -> TextTrack
    abstract canPlayType: ``type``: string -> string
    abstract load: unit -> unit
    abstract msClearEffects: unit -> unit
    abstract msGetAsCastingSource: unit -> obj
    abstract msInsertAudioEffect: activatableClassId: string * effectRequired: bool * ?config: obj -> unit
    abstract msSetMediaKeys: mediaKeys: MSMediaKeys -> unit
    abstract msSetMediaProtectionManager: ?mediaProtectionManager: obj -> unit
    abstract pause: unit -> unit
    abstract play: unit -> unit
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('msneedkey',$1...)")>] abstract addEventListener_msneedkey: listener: Func<MSMediaKeyNeededEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLMenuElement =
    inherit HTMLElement
    abstract compact: bool with get, set
    abstract ``type``: string with get, set

and HTMLMetaElement =
    inherit HTMLElement
    abstract charset: string with get, set
    abstract content: string with get, set
    abstract httpEquiv: string with get, set
    abstract name: string with get, set
    abstract scheme: string with get, set
    abstract url: string with get, set

and HTMLModElement =
    inherit HTMLElement
    abstract cite: string with get, set
    abstract dateTime: string with get, set

and HTMLNextIdElement =
    inherit HTMLElement
    abstract n: string with get, set

and HTMLOListElement =
    inherit HTMLElement
    abstract compact: bool with get, set
    abstract start: float with get, set
    abstract ``type``: string with get, set

and HTMLObjectElement =
    inherit HTMLElement
    inherit GetSVGDocument
    abstract BaseHref: string with get, set
    abstract align: string with get, set
    abstract alt: string with get, set
    abstract altHtml: string with get, set
    abstract archive: string with get, set
    abstract border: string with get, set
    abstract code: string with get, set
    abstract codeBase: string with get, set
    abstract codeType: string with get, set
    abstract contentDocument: Document with get, set
    abstract data: string with get, set
    abstract declare: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract height: string with get, set
    abstract hspace: float with get, set
    abstract msPlayToDisabled: bool with get, set
    abstract msPlayToPreferredSourceUri: string with get, set
    abstract msPlayToPrimary: bool with get, set
    abstract msPlayToSource: obj with get, set
    abstract name: string with get, set
    abstract ``object``: obj with get, set
    abstract readyState: float with get, set
    abstract standby: string with get, set
    abstract ``type``: string with get, set
    abstract useMap: string with get, set
    abstract validationMessage: string with get, set
    abstract validity: ValidityState with get, set
    abstract vspace: float with get, set
    abstract width: string with get, set
    abstract willValidate: bool with get, set
    abstract checkValidity: unit -> bool
    abstract setCustomValidity: error: string -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLOptGroupElement =
    inherit HTMLElement
    abstract defaultSelected: bool with get, set
    abstract disabled: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract index: float with get, set
    abstract label: string with get, set
    abstract selected: bool with get, set
    abstract text: string with get, set
    abstract value: string with get, set

and HTMLOptionElement =
    inherit HTMLElement
    abstract defaultSelected: bool with get, set
    abstract disabled: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract index: float with get, set
    abstract label: string with get, set
    abstract selected: bool with get, set
    abstract text: string with get, set
    abstract value: string with get, set

and HTMLParagraphElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract clear: string with get, set

and HTMLParamElement =
    inherit HTMLElement
    abstract name: string with get, set
    abstract ``type``: string with get, set
    abstract value: string with get, set
    abstract valueType: string with get, set

and HTMLPhraseElement =
    inherit HTMLElement
    abstract cite: string with get, set
    abstract dateTime: string with get, set

and HTMLPreElement =
    inherit HTMLElement
    abstract cite: string with get, set
    abstract clear: string with get, set
    abstract width: float with get, set

and HTMLProgressElement =
    inherit HTMLElement
    abstract form: HTMLFormElement with get, set
    abstract max: float with get, set
    abstract position: float with get, set
    abstract value: float with get, set

and HTMLQuoteElement =
    inherit HTMLElement
    abstract cite: string with get, set
    abstract dateTime: string with get, set

and HTMLScriptElement =
    inherit HTMLElement
    abstract async: bool with get, set
    abstract charset: string with get, set
    abstract defer: bool with get, set
    abstract ``event``: string with get, set
    abstract htmlFor: string with get, set
    abstract src: string with get, set
    abstract text: string with get, set
    abstract ``type``: string with get, set

and HTMLSelectElement =
    inherit HTMLElement
    abstract autofocus: bool with get, set
    abstract disabled: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract length: float with get, set
    abstract multiple: bool with get, set
    abstract name: string with get, set
    abstract options: HTMLCollection with get, set
    abstract required: bool with get, set
    abstract selectedIndex: float with get, set
    abstract size: float with get, set
    abstract ``type``: string with get, set
    abstract validationMessage: string with get, set
    abstract validity: ValidityState with get, set
    abstract value: string with get, set
    abstract willValidate: bool with get, set
    abstract selectedOptions: HTMLCollection with get, set
    abstract add: element: HTMLElement * ?before: U2<HTMLElement, float> -> unit
    abstract checkValidity: unit -> bool
    abstract item: ?name: obj * ?index: obj -> obj
    abstract namedItem: name: string -> obj
    abstract remove: ?index: float -> unit
    abstract setCustomValidity: error: string -> unit

and HTMLSourceElement =
    inherit HTMLElement
    abstract media: string with get, set
    abstract msKeySystem: string with get, set
    abstract src: string with get, set
    abstract ``type``: string with get, set

and HTMLSpanElement =
    inherit HTMLElement

and HTMLStyleElement =
    inherit HTMLElement
    inherit LinkStyle
    abstract media: string with get, set
    abstract ``type``: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLTableCaptionElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract vAlign: string with get, set

and HTMLTableCellElement =
    inherit HTMLElement
    inherit HTMLTableAlignment
    abstract abbr: string with get, set
    abstract align: string with get, set
    abstract axis: string with get, set
    abstract bgColor: obj with get, set
    abstract cellIndex: float with get, set
    abstract colSpan: float with get, set
    abstract headers: string with get, set
    abstract height: obj with get, set
    abstract noWrap: bool with get, set
    abstract rowSpan: float with get, set
    abstract scope: string with get, set
    abstract width: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLTableColElement =
    inherit HTMLElement
    inherit HTMLTableAlignment
    abstract align: string with get, set
    abstract span: float with get, set
    abstract width: obj with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLTableDataCellElement =
    inherit HTMLTableCellElement

and HTMLTableElement =
    inherit HTMLElement
    abstract align: string with get, set
    abstract bgColor: obj with get, set
    abstract border: string with get, set
    abstract borderColor: obj with get, set
    abstract caption: HTMLTableCaptionElement with get, set
    abstract cellPadding: string with get, set
    abstract cellSpacing: string with get, set
    abstract cols: float with get, set
    abstract frame: string with get, set
    abstract height: obj with get, set
    abstract rows: HTMLCollection with get, set
    abstract rules: string with get, set
    abstract summary: string with get, set
    abstract tBodies: HTMLCollection with get, set
    abstract tFoot: HTMLTableSectionElement with get, set
    abstract tHead: HTMLTableSectionElement with get, set
    abstract width: string with get, set
    abstract createCaption: unit -> HTMLTableCaptionElement
    abstract createTBody: unit -> HTMLTableSectionElement
    abstract createTFoot: unit -> HTMLTableSectionElement
    abstract createTHead: unit -> HTMLTableSectionElement
    abstract deleteCaption: unit -> unit
    abstract deleteRow: ?index: float -> unit
    abstract deleteTFoot: unit -> unit
    abstract deleteTHead: unit -> unit
    abstract insertRow: ?index: float -> HTMLTableRowElement

and HTMLTableHeaderCellElement =
    inherit HTMLTableCellElement
    abstract scope: string with get, set

and HTMLTableRowElement =
    inherit HTMLElement
    inherit HTMLTableAlignment
    abstract align: string with get, set
    abstract bgColor: obj with get, set
    abstract cells: HTMLCollection with get, set
    abstract height: obj with get, set
    abstract rowIndex: float with get, set
    abstract sectionRowIndex: float with get, set
    abstract deleteCell: ?index: float -> unit
    abstract insertCell: ?index: float -> HTMLTableCellElement
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLTableSectionElement =
    inherit HTMLElement
    inherit HTMLTableAlignment
    abstract align: string with get, set
    abstract rows: HTMLCollection with get, set
    abstract deleteRow: ?index: float -> unit
    abstract insertRow: ?index: float -> HTMLTableRowElement
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLTextAreaElement =
    inherit HTMLElement
    abstract autofocus: bool with get, set
    abstract cols: float with get, set
    abstract defaultValue: string with get, set
    abstract disabled: bool with get, set
    abstract form: HTMLFormElement with get, set
    abstract maxLength: float with get, set
    abstract name: string with get, set
    abstract placeholder: string with get, set
    abstract readOnly: bool with get, set
    abstract required: bool with get, set
    abstract rows: float with get, set
    abstract selectionEnd: float with get, set
    abstract selectionStart: float with get, set
    abstract status: obj with get, set
    abstract ``type``: string with get, set
    abstract validationMessage: string with get, set
    abstract validity: ValidityState with get, set
    abstract value: string with get, set
    abstract willValidate: bool with get, set
    abstract wrap: string with get, set
    abstract checkValidity: unit -> bool
    abstract createTextRange: unit -> TextRange
    abstract select: unit -> unit
    abstract setCustomValidity: error: string -> unit
    abstract setSelectionRange: start: float * ``end``: float -> unit

and HTMLTitleElement =
    inherit HTMLElement
    abstract text: string with get, set

and HTMLTrackElement =
    inherit HTMLElement
    abstract ``default``: bool with get, set
    abstract kind: string with get, set
    abstract label: string with get, set
    abstract readyState: float with get, set
    abstract src: string with get, set
    abstract srclang: string with get, set
    abstract track: TextTrack with get, set
    abstract ERROR: float with get, set
    abstract LOADED: float with get, set
    abstract LOADING: float with get, set
    abstract NONE: float with get, set

and HTMLUListElement =
    inherit HTMLElement
    abstract compact: bool with get, set
    abstract ``type``: string with get, set

and HTMLUnknownElement =
    inherit HTMLElement

and HTMLVideoElement =
    inherit HTMLMediaElement
    abstract height: float with get, set
    abstract msHorizontalMirror: bool with get, set
    abstract msIsLayoutOptimalForPlayback: bool with get, set
    abstract msIsStereo3D: bool with get, set
    abstract msStereo3DPackingMode: string with get, set
    abstract msStereo3DRenderMode: string with get, set
    abstract msZoom: bool with get, set
    abstract onMSVideoFormatChanged: Func<Event, obj> with get, set
    abstract onMSVideoFrameStepCompleted: Func<Event, obj> with get, set
    abstract onMSVideoOptimalLayoutChanged: Func<Event, obj> with get, set
    abstract poster: string with get, set
    abstract videoHeight: float with get, set
    abstract videoWidth: float with get, set
    abstract webkitDisplayingFullscreen: bool with get, set
    abstract webkitSupportsFullscreen: bool with get, set
    abstract width: float with get, set
    abstract getVideoPlaybackQuality: unit -> VideoPlaybackQuality
    abstract msFrameStep: forward: bool -> unit
    abstract msInsertVideoEffect: activatableClassId: string * effectRequired: bool * ?config: obj -> unit
    abstract msSetVideoRectangle: left: float * top: float * right: float * bottom: float -> unit
    abstract webkitEnterFullScreen: unit -> unit
    abstract webkitEnterFullscreen: unit -> unit
    abstract webkitExitFullScreen: unit -> unit
    abstract webkitExitFullscreen: unit -> unit
    [<Emit("$0.addEventListener('MSContentZoom',$1...)")>] abstract addEventListener_MSContentZoom: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSManipulationStateChanged',$1...)")>] abstract addEventListener_MSManipulationStateChanged: listener: Func<MSManipulationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSVideoFormatChanged',$1...)")>] abstract addEventListener_MSVideoFormatChanged: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSVideoFrameStepCompleted',$1...)")>] abstract addEventListener_MSVideoFrameStepCompleted: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSVideoOptimalLayoutChanged',$1...)")>] abstract addEventListener_MSVideoOptimalLayoutChanged: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('activate',$1...)")>] abstract addEventListener_activate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeactivate',$1...)")>] abstract addEventListener_beforeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecopy',$1...)")>] abstract addEventListener_beforecopy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforecut',$1...)")>] abstract addEventListener_beforecut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforedeactivate',$1...)")>] abstract addEventListener_beforedeactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforepaste',$1...)")>] abstract addEventListener_beforepaste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('copy',$1...)")>] abstract addEventListener_copy: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('cut',$1...)")>] abstract addEventListener_cut: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deactivate',$1...)")>] abstract addEventListener_deactivate: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('msneedkey',$1...)")>] abstract addEventListener_msneedkey: listener: Func<MSMediaKeyNeededEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('paste',$1...)")>] abstract addEventListener_paste: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('selectstart',$1...)")>] abstract addEventListener_selectstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HashChangeEvent =
    inherit Event
    abstract newURL: string with get, set
    abstract oldURL: string with get, set

and History =
    abstract length: float with get, set
    abstract state: obj with get, set
    abstract back: ?distance: obj -> unit
    abstract forward: ?distance: obj -> unit
    abstract go: ?delta: obj -> unit
    abstract pushState: statedata: obj * ?title: string * ?url: string -> unit
    abstract replaceState: statedata: obj * ?title: string * ?url: string -> unit

and IDBCursor =
    abstract direction: string with get, set
    abstract key: obj with get, set
    abstract primaryKey: obj with get, set
    abstract source: U2<IDBObjectStore, IDBIndex> with get, set
    abstract NEXT: string with get, set
    abstract NEXT_NO_DUPLICATE: string with get, set
    abstract PREV: string with get, set
    abstract PREV_NO_DUPLICATE: string with get, set
    abstract advance: count: float -> unit
    abstract ``continue``: ?key: obj -> unit
    abstract delete: unit -> IDBRequest
    abstract update: value: obj -> IDBRequest

and IDBCursorWithValue =
    inherit IDBCursor
    abstract value: obj with get, set

and IDBDatabase =
    inherit EventTarget
    abstract name: string with get, set
    abstract objectStoreNames: DOMStringList with get, set
    abstract onabort: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract version: float with get, set
    abstract close: unit -> unit
    abstract createObjectStore: name: string * ?optionalParameters: IDBObjectStoreParameters -> IDBObjectStore
    abstract deleteObjectStore: name: string -> unit
    abstract transaction: storeNames: U2<string, ResizeArray<string>> * ?mode: string -> IDBTransaction
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and IDBFactory =
    abstract cmp: first: obj * second: obj -> float
    abstract deleteDatabase: name: string -> IDBOpenDBRequest
    abstract ``open``: name: string * ?version: float -> IDBOpenDBRequest

and IDBIndex =
    abstract keyPath: U2<string, ResizeArray<string>> with get, set
    abstract name: string with get, set
    abstract objectStore: IDBObjectStore with get, set
    abstract unique: bool with get, set
    abstract multiEntry: bool with get, set
    abstract count: ?key: obj -> IDBRequest
    abstract get: key: obj -> IDBRequest
    abstract getKey: key: obj -> IDBRequest
    abstract openCursor: ?range: IDBKeyRange * ?direction: string -> IDBRequest
    abstract openKeyCursor: ?range: IDBKeyRange * ?direction: string -> IDBRequest

and IDBKeyRange =
    abstract lower: obj with get, set
    abstract lowerOpen: bool with get, set
    abstract upper: obj with get, set
    abstract upperOpen: bool with get, set

and IDBObjectStore =
    abstract indexNames: DOMStringList with get, set
    abstract keyPath: U2<string, ResizeArray<string>> with get, set
    abstract name: string with get, set
    abstract transaction: IDBTransaction with get, set
    abstract autoIncrement: bool with get, set
    abstract add: value: obj * ?key: obj -> IDBRequest
    abstract clear: unit -> IDBRequest
    abstract count: ?key: obj -> IDBRequest
    abstract createIndex: name: string * keyPath: U2<string, ResizeArray<string>> * ?optionalParameters: IDBIndexParameters -> IDBIndex
    abstract delete: key: obj -> IDBRequest
    abstract deleteIndex: indexName: string -> unit
    abstract get: key: obj -> IDBRequest
    abstract index: name: string -> IDBIndex
    abstract openCursor: ?range: obj * ?direction: string -> IDBRequest
    abstract put: value: obj * ?key: obj -> IDBRequest

and IDBOpenDBRequest =
    inherit IDBRequest
    abstract onblocked: Func<Event, obj> with get, set
    abstract onupgradeneeded: Func<IDBVersionChangeEvent, obj> with get, set
    [<Emit("$0.addEventListener('blocked',$1...)")>] abstract addEventListener_blocked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('success',$1...)")>] abstract addEventListener_success: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('upgradeneeded',$1...)")>] abstract addEventListener_upgradeneeded: listener: Func<IDBVersionChangeEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and IDBRequest =
    inherit EventTarget
    abstract error: DOMError with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onsuccess: Func<Event, obj> with get, set
    abstract readyState: string with get, set
    abstract result: obj with get, set
    abstract source: U3<IDBObjectStore, IDBIndex, IDBCursor> with get, set
    abstract transaction: IDBTransaction with get, set
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('success',$1...)")>] abstract addEventListener_success: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and IDBTransaction =
    inherit EventTarget
    abstract db: IDBDatabase with get, set
    abstract error: DOMError with get, set
    abstract mode: string with get, set
    abstract onabort: Func<Event, obj> with get, set
    abstract oncomplete: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract READ_ONLY: string with get, set
    abstract READ_WRITE: string with get, set
    abstract VERSION_CHANGE: string with get, set
    abstract abort: unit -> unit
    abstract objectStore: name: string -> IDBObjectStore
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('complete',$1...)")>] abstract addEventListener_complete: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and IDBVersionChangeEvent =
    inherit Event
    abstract newVersion: float with get, set
    abstract oldVersion: float with get, set

and ImageData =
    abstract data: Uint8ClampedArray with get, set
    abstract height: float with get, set
    abstract width: float with get, set

and KeyboardEvent =
    inherit UIEvent
    abstract altKey: bool with get, set
    abstract char: string with get, set
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
    abstract DOM_KEY_LOCATION_JOYSTICK: float with get, set
    abstract DOM_KEY_LOCATION_LEFT: float with get, set
    abstract DOM_KEY_LOCATION_MOBILE: float with get, set
    abstract DOM_KEY_LOCATION_NUMPAD: float with get, set
    abstract DOM_KEY_LOCATION_RIGHT: float with get, set
    abstract DOM_KEY_LOCATION_STANDARD: float with get, set
    abstract getModifierState: keyArg: string -> bool
    abstract initKeyboardEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * keyArg: string * locationArg: float * modifiersListArg: string * repeat: bool * locale: string -> unit

and Location =
    abstract hash: string with get, set
    abstract host: string with get, set
    abstract hostname: string with get, set
    abstract href: string with get, set
    abstract origin: string with get, set
    abstract pathname: string with get, set
    abstract port: string with get, set
    abstract protocol: string with get, set
    abstract search: string with get, set
    abstract assign: url: string -> unit
    abstract reload: ?forcedReload: bool -> unit
    abstract replace: url: string -> unit
    abstract toString: unit -> string

and LongRunningScriptDetectedEvent =
    inherit Event
    abstract executionTime: float with get, set
    abstract stopPageScriptExecution: bool with get, set

and MSApp =
    abstract CURRENT: string with get, set
    abstract HIGH: string with get, set
    abstract IDLE: string with get, set
    abstract NORMAL: string with get, set
    abstract clearTemporaryWebDataAsync: unit -> MSAppAsyncOperation
    abstract createBlobFromRandomAccessStream: ``type``: string * seeker: obj -> Blob
    abstract createDataPackage: ``object``: obj -> obj
    abstract createDataPackageFromSelection: unit -> obj
    abstract createFileFromStorageFile: storageFile: obj -> File
    abstract createStreamFromInputStream: ``type``: string * inputStream: obj -> MSStream
    abstract execAsyncAtPriority: asynchronousCallback: MSExecAtPriorityFunctionCallback * priority: string * [<ParamArray>] args: obj[] -> unit
    abstract execAtPriority: synchronousCallback: MSExecAtPriorityFunctionCallback * priority: string * [<ParamArray>] args: obj[] -> obj
    abstract getCurrentPriority: unit -> string
    abstract getHtmlPrintDocumentSourceAsync: htmlDoc: obj -> obj
    abstract getViewId: view: obj -> obj
    abstract isTaskScheduledAtPriorityOrHigher: priority: string -> bool
    abstract pageHandlesAllApplicationActivations: enabled: bool -> unit
    abstract suppressSubdownloadCredentialPrompts: suppress: bool -> unit
    abstract terminateApp: exceptionObject: obj -> unit

and MSAppAsyncOperation =
    inherit EventTarget
    abstract error: DOMError with get, set
    abstract oncomplete: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract readyState: float with get, set
    abstract result: obj with get, set
    abstract COMPLETED: float with get, set
    abstract ERROR: float with get, set
    abstract STARTED: float with get, set
    abstract start: unit -> unit
    [<Emit("$0.addEventListener('complete',$1...)")>] abstract addEventListener_complete: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and MSBlobBuilder =
    abstract append: data: obj * ?endings: string -> unit
    abstract getBlob: ?contentType: string -> Blob

and MSCSSMatrix =
    abstract a: float with get, set
    abstract b: float with get, set
    abstract c: float with get, set
    abstract d: float with get, set
    abstract e: float with get, set
    abstract f: float with get, set
    abstract m11: float with get, set
    abstract m12: float with get, set
    abstract m13: float with get, set
    abstract m14: float with get, set
    abstract m21: float with get, set
    abstract m22: float with get, set
    abstract m23: float with get, set
    abstract m24: float with get, set
    abstract m31: float with get, set
    abstract m32: float with get, set
    abstract m33: float with get, set
    abstract m34: float with get, set
    abstract m41: float with get, set
    abstract m42: float with get, set
    abstract m43: float with get, set
    abstract m44: float with get, set
    abstract inverse: unit -> MSCSSMatrix
    abstract multiply: secondMatrix: MSCSSMatrix -> MSCSSMatrix
    abstract rotate: angleX: float * ?angleY: float * ?angleZ: float -> MSCSSMatrix
    abstract rotateAxisAngle: x: float * y: float * z: float * angle: float -> MSCSSMatrix
    abstract scale: scaleX: float * ?scaleY: float * ?scaleZ: float -> MSCSSMatrix
    abstract setMatrixValue: value: string -> unit
    abstract skewX: angle: float -> MSCSSMatrix
    abstract skewY: angle: float -> MSCSSMatrix
    abstract toString: unit -> string
    abstract translate: x: float * y: float * ?z: float -> MSCSSMatrix

and MSGesture =
    abstract target: Element with get, set
    abstract addPointer: pointerId: float -> unit
    abstract stop: unit -> unit

and MSGestureEvent =
    inherit UIEvent
    abstract clientX: float with get, set
    abstract clientY: float with get, set
    abstract expansion: float with get, set
    abstract gestureObject: obj with get, set
    abstract hwTimestamp: float with get, set
    abstract offsetX: float with get, set
    abstract offsetY: float with get, set
    abstract rotation: float with get, set
    abstract scale: float with get, set
    abstract screenX: float with get, set
    abstract screenY: float with get, set
    abstract translationX: float with get, set
    abstract translationY: float with get, set
    abstract velocityAngular: float with get, set
    abstract velocityExpansion: float with get, set
    abstract velocityX: float with get, set
    abstract velocityY: float with get, set
    abstract MSGESTURE_FLAG_BEGIN: float with get, set
    abstract MSGESTURE_FLAG_CANCEL: float with get, set
    abstract MSGESTURE_FLAG_END: float with get, set
    abstract MSGESTURE_FLAG_INERTIA: float with get, set
    abstract MSGESTURE_FLAG_NONE: float with get, set
    abstract initGestureEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * offsetXArg: float * offsetYArg: float * translationXArg: float * translationYArg: float * scaleArg: float * expansionArg: float * rotationArg: float * velocityXArg: float * velocityYArg: float * velocityExpansionArg: float * velocityAngularArg: float * hwTimestampArg: float -> unit

and MSGraphicsTrust =
    abstract constrictionActive: bool with get, set
    abstract status: string with get, set

and MSHTMLWebViewElement =
    inherit HTMLElement
    abstract canGoBack: bool with get, set
    abstract canGoForward: bool with get, set
    abstract containsFullScreenElement: bool with get, set
    abstract documentTitle: string with get, set
    abstract height: float with get, set
    abstract settings: MSWebViewSettings with get, set
    abstract src: string with get, set
    abstract width: float with get, set
    abstract addWebAllowedObject: name: string * applicationObject: obj -> unit
    abstract buildLocalStreamUri: contentIdentifier: string * relativePath: string -> string
    abstract capturePreviewToBlobAsync: unit -> MSWebViewAsyncOperation
    abstract captureSelectedContentToDataPackageAsync: unit -> MSWebViewAsyncOperation
    abstract getDeferredPermissionRequestById: id: float -> DeferredPermissionRequest
    abstract getDeferredPermissionRequests: unit -> ResizeArray<DeferredPermissionRequest>
    abstract goBack: unit -> unit
    abstract goForward: unit -> unit
    abstract invokeScriptAsync: scriptName: string * [<ParamArray>] args: obj[] -> MSWebViewAsyncOperation
    abstract navigate: uri: string -> unit
    abstract navigateToLocalStreamUri: source: string * streamResolver: obj -> unit
    abstract navigateToString: contents: string -> unit
    abstract navigateWithHttpRequestMessage: requestMessage: obj -> unit
    abstract refresh: unit -> unit
    abstract stop: unit -> unit

and MSInputMethodContext =
    inherit EventTarget
    abstract compositionEndOffset: float with get, set
    abstract compositionStartOffset: float with get, set
    abstract oncandidatewindowhide: Func<Event, obj> with get, set
    abstract oncandidatewindowshow: Func<Event, obj> with get, set
    abstract oncandidatewindowupdate: Func<Event, obj> with get, set
    abstract target: HTMLElement with get, set
    abstract getCandidateWindowClientRect: unit -> ClientRect
    abstract getCompositionAlternatives: unit -> ResizeArray<string>
    abstract hasComposition: unit -> bool
    abstract isCandidateWindowVisible: unit -> bool
    [<Emit("$0.addEventListener('MSCandidateWindowHide',$1...)")>] abstract addEventListener_MSCandidateWindowHide: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSCandidateWindowShow',$1...)")>] abstract addEventListener_MSCandidateWindowShow: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSCandidateWindowUpdate',$1...)")>] abstract addEventListener_MSCandidateWindowUpdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and MSManipulationEvent =
    inherit UIEvent
    abstract currentState: float with get, set
    abstract inertiaDestinationX: float with get, set
    abstract inertiaDestinationY: float with get, set
    abstract lastState: float with get, set
    abstract MS_MANIPULATION_STATE_ACTIVE: float with get, set
    abstract MS_MANIPULATION_STATE_CANCELLED: float with get, set
    abstract MS_MANIPULATION_STATE_COMMITTED: float with get, set
    abstract MS_MANIPULATION_STATE_DRAGGING: float with get, set
    abstract MS_MANIPULATION_STATE_INERTIA: float with get, set
    abstract MS_MANIPULATION_STATE_PRESELECT: float with get, set
    abstract MS_MANIPULATION_STATE_SELECTING: float with get, set
    abstract MS_MANIPULATION_STATE_STOPPED: float with get, set
    abstract initMSManipulationEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * lastState: float * currentState: float -> unit

and MSMediaKeyError =
    abstract code: float with get, set
    abstract systemCode: float with get, set
    abstract MS_MEDIA_KEYERR_CLIENT: float with get, set
    abstract MS_MEDIA_KEYERR_DOMAIN: float with get, set
    abstract MS_MEDIA_KEYERR_HARDWARECHANGE: float with get, set
    abstract MS_MEDIA_KEYERR_OUTPUT: float with get, set
    abstract MS_MEDIA_KEYERR_SERVICE: float with get, set
    abstract MS_MEDIA_KEYERR_UNKNOWN: float with get, set

and MSMediaKeyMessageEvent =
    inherit Event
    abstract destinationURL: string with get, set
    abstract message: Uint8Array with get, set

and MSMediaKeyNeededEvent =
    inherit Event
    abstract initData: Uint8Array with get, set

and MSMediaKeySession =
    inherit EventTarget
    abstract error: MSMediaKeyError with get, set
    abstract keySystem: string with get, set
    abstract sessionId: string with get, set
    abstract close: unit -> unit
    abstract update: key: Uint8Array -> unit

and MSMediaKeys =
    abstract keySystem: string with get, set
    abstract createSession: ``type``: string * initData: Uint8Array * ?cdmData: Uint8Array -> MSMediaKeySession

and MSMimeTypesCollection =
    abstract length: float with get, set

and MSPluginsCollection =
    abstract length: float with get, set
    abstract refresh: ?reload: bool -> unit

and MSPointerEvent =
    inherit MouseEvent
    abstract currentPoint: obj with get, set
    abstract height: float with get, set
    abstract hwTimestamp: float with get, set
    abstract intermediatePoints: obj with get, set
    abstract isPrimary: bool with get, set
    abstract pointerId: float with get, set
    abstract pointerType: obj with get, set
    abstract pressure: float with get, set
    abstract rotation: float with get, set
    abstract tiltX: float with get, set
    abstract tiltY: float with get, set
    abstract width: float with get, set
    abstract getCurrentPoint: element: Element -> unit
    abstract getIntermediatePoints: element: Element -> unit
    abstract initPointerEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * ctrlKeyArg: bool * altKeyArg: bool * shiftKeyArg: bool * metaKeyArg: bool * buttonArg: float * relatedTargetArg: EventTarget * offsetXArg: float * offsetYArg: float * widthArg: float * heightArg: float * pressure: float * rotation: float * tiltX: float * tiltY: float * pointerIdArg: float * pointerType: obj * hwTimestampArg: float * isPrimary: bool -> unit

and MSRangeCollection =
    abstract length: float with get, set
    abstract item: index: float -> Range

and MSSiteModeEvent =
    inherit Event
    abstract actionURL: string with get, set
    abstract buttonID: float with get, set

and MSStream =
    abstract ``type``: string with get, set
    abstract msClose: unit -> unit
    abstract msDetachStream: unit -> obj

and MSStreamReader =
    inherit EventTarget
    inherit MSBaseReader
    abstract error: DOMError with get, set
    abstract readAsArrayBuffer: stream: MSStream * ?size: float -> unit
    abstract readAsBinaryString: stream: MSStream * ?size: float -> unit
    abstract readAsBlob: stream: MSStream * ?size: float -> unit
    abstract readAsDataURL: stream: MSStream * ?size: float -> unit
    abstract readAsText: stream: MSStream * ?encoding: string * ?size: float -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and MSWebViewAsyncOperation =
    inherit EventTarget
    abstract error: DOMError with get, set
    abstract oncomplete: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract readyState: float with get, set
    abstract result: obj with get, set
    abstract target: MSHTMLWebViewElement with get, set
    abstract ``type``: float with get, set
    abstract COMPLETED: float with get, set
    abstract ERROR: float with get, set
    abstract STARTED: float with get, set
    abstract TYPE_CAPTURE_PREVIEW_TO_RANDOM_ACCESS_STREAM: float with get, set
    abstract TYPE_CREATE_DATA_PACKAGE_FROM_SELECTION: float with get, set
    abstract TYPE_INVOKE_SCRIPT: float with get, set
    abstract start: unit -> unit
    [<Emit("$0.addEventListener('complete',$1...)")>] abstract addEventListener_complete: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and MSWebViewSettings =
    abstract isIndexedDBEnabled: bool with get, set
    abstract isJavaScriptEnabled: bool with get, set

and MediaElementAudioSourceNode =
    inherit AudioNode

and MediaError =
    abstract code: float with get, set
    abstract msExtendedCode: float with get, set
    abstract MEDIA_ERR_ABORTED: float with get, set
    abstract MEDIA_ERR_DECODE: float with get, set
    abstract MEDIA_ERR_NETWORK: float with get, set
    abstract MEDIA_ERR_SRC_NOT_SUPPORTED: float with get, set
    abstract MS_MEDIA_ERR_ENCRYPTED: float with get, set

and MediaList =
    abstract length: float with get, set
    abstract mediaText: string with get, set
    abstract appendMedium: newMedium: string -> unit
    abstract deleteMedium: oldMedium: string -> unit
    abstract item: index: float -> string
    abstract toString: unit -> string

and MediaQueryList =
    abstract matches: bool with get, set
    abstract media: string with get, set
    abstract addListener: listener: MediaQueryListListener -> unit
    abstract removeListener: listener: MediaQueryListListener -> unit

and MediaSource =
    inherit EventTarget
    abstract activeSourceBuffers: SourceBufferList with get, set
    abstract duration: float with get, set
    abstract readyState: string with get, set
    abstract sourceBuffers: SourceBufferList with get, set
    abstract addSourceBuffer: ``type``: string -> SourceBuffer
    abstract endOfStream: ?error: float -> unit
    abstract removeSourceBuffer: sourceBuffer: SourceBuffer -> unit

and MessageChannel =
    abstract port1: MessagePort with get, set
    abstract port2: MessagePort with get, set

and MessageEvent =
    inherit Event
    abstract data: obj with get, set
    abstract origin: string with get, set
    abstract ports: obj with get, set
    abstract source: Window with get, set
    abstract initMessageEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * dataArg: obj * originArg: string * lastEventIdArg: string * sourceArg: Window -> unit

and MessagePort =
    inherit EventTarget
    abstract onmessage: Func<MessageEvent, obj> with get, set
    abstract close: unit -> unit
    abstract postMessage: ?message: obj * ?ports: obj -> unit
    abstract start: unit -> unit
    [<Emit("$0.addEventListener('message',$1...)")>] abstract addEventListener_message: listener: Func<MessageEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and MimeType =
    abstract description: string with get, set
    abstract enabledPlugin: Plugin with get, set
    abstract suffixes: string with get, set
    abstract ``type``: string with get, set

and MimeTypeArray =
    abstract length: float with get, set
    abstract item: index: float -> Plugin
    abstract namedItem: ``type``: string -> Plugin

and MouseEvent =
    inherit UIEvent
    abstract altKey: bool with get, set
    abstract button: float with get, set
    abstract buttons: float with get, set
    abstract clientX: float with get, set
    abstract clientY: float with get, set
    abstract ctrlKey: bool with get, set
    abstract fromElement: Element with get, set
    abstract layerX: float with get, set
    abstract layerY: float with get, set
    abstract metaKey: bool with get, set
    abstract movementX: float with get, set
    abstract movementY: float with get, set
    abstract offsetX: float with get, set
    abstract offsetY: float with get, set
    abstract pageX: float with get, set
    abstract pageY: float with get, set
    abstract relatedTarget: EventTarget with get, set
    abstract screenX: float with get, set
    abstract screenY: float with get, set
    abstract shiftKey: bool with get, set
    abstract toElement: Element with get, set
    abstract which: float with get, set
    abstract x: float with get, set
    abstract y: float with get, set
    abstract getModifierState: keyArg: string -> bool
    abstract initMouseEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * ctrlKeyArg: bool * altKeyArg: bool * shiftKeyArg: bool * metaKeyArg: bool * buttonArg: float * relatedTargetArg: EventTarget -> unit

and MouseWheelEvent =
    inherit MouseEvent
    abstract wheelDelta: float with get, set
    abstract wheelDeltaX: float with get, set
    abstract wheelDeltaY: float with get, set
    abstract initMouseWheelEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * buttonArg: float * relatedTargetArg: EventTarget * modifiersListArg: string * wheelDeltaArg: float -> unit

and MutationEvent =
    inherit Event
    abstract attrChange: float with get, set
    abstract attrName: string with get, set
    abstract newValue: string with get, set
    abstract prevValue: string with get, set
    abstract relatedNode: Node with get, set
    abstract ADDITION: float with get, set
    abstract MODIFICATION: float with get, set
    abstract REMOVAL: float with get, set
    abstract initMutationEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * relatedNodeArg: Node * prevValueArg: string * newValueArg: string * attrNameArg: string * attrChangeArg: float -> unit

and MutationObserver =
    abstract disconnect: unit -> unit
    abstract observe: target: Node * options: MutationObserverInit -> unit
    abstract takeRecords: unit -> ResizeArray<MutationRecord>

and MutationRecord =
    abstract addedNodes: NodeList with get, set
    abstract attributeName: string with get, set
    abstract attributeNamespace: string with get, set
    abstract nextSibling: Node with get, set
    abstract oldValue: string with get, set
    abstract previousSibling: Node with get, set
    abstract removedNodes: NodeList with get, set
    abstract target: Node with get, set
    abstract ``type``: string with get, set

and NamedNodeMap =
    abstract length: float with get, set
    abstract getNamedItem: name: string -> Attr
    abstract getNamedItemNS: namespaceURI: string * localName: string -> Attr
    abstract item: index: float -> Attr
    abstract removeNamedItem: name: string -> Attr
    abstract removeNamedItemNS: namespaceURI: string * localName: string -> Attr
    abstract setNamedItem: arg: Attr -> Attr
    abstract setNamedItemNS: arg: Attr -> Attr

and NavigationCompletedEvent =
    inherit NavigationEvent
    abstract isSuccess: bool with get, set
    abstract webErrorStatus: float with get, set

and NavigationEvent =
    inherit Event
    abstract uri: string with get, set

and NavigationEventWithReferrer =
    inherit NavigationEvent
    abstract referer: string with get, set

and Navigator =
    inherit obj
    inherit NavigatorID
    inherit NavigatorOnLine
    inherit NavigatorContentUtils
    inherit NavigatorStorageUtils
    inherit NavigatorGeolocation
    inherit MSNavigatorDoNotTrack
    inherit MSFileSaver
    abstract appCodeName: string with get, set
    abstract appMinorVersion: string with get, set
    abstract browserLanguage: string with get, set
    abstract connectionSpeed: float with get, set
    abstract cookieEnabled: bool with get, set
    abstract cpuClass: string with get, set
    abstract language: string with get, set
    abstract maxTouchPoints: float with get, set
    abstract mimeTypes: MSMimeTypesCollection with get, set
    abstract msManipulationViewsEnabled: bool with get, set
    abstract msMaxTouchPoints: float with get, set
    abstract msPointerEnabled: bool with get, set
    abstract plugins: MSPluginsCollection with get, set
    abstract pointerEnabled: bool with get, set
    abstract systemLanguage: string with get, set
    abstract userLanguage: string with get, set
    abstract webdriver: bool with get, set
    abstract getGamepads: unit -> ResizeArray<Gamepad>
    abstract javaEnabled: unit -> bool
    abstract msLaunchUri: uri: string * ?successCallback: MSLaunchUriCallback * ?noHandlerCallback: MSLaunchUriCallback -> unit
    abstract vibrate: pattern: U2<float, ResizeArray<float>> -> bool
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and Node =
    inherit EventTarget
    abstract attributes: NamedNodeMap with get, set
    abstract baseURI: string with get, set
    abstract childNodes: NodeList with get, set
    abstract firstChild: Node with get, set
    abstract lastChild: Node with get, set
    abstract localName: string with get, set
    abstract namespaceURI: string with get, set
    abstract nextSibling: Node with get, set
    abstract nodeName: string with get, set
    abstract nodeType: float with get, set
    abstract nodeValue: string with get, set
    abstract ownerDocument: Document with get, set
    abstract parentElement: HTMLElement with get, set
    abstract parentNode: Node with get, set
    abstract prefix: string with get, set
    abstract previousSibling: Node with get, set
    abstract textContent: string with get, set
    abstract ATTRIBUTE_NODE: float with get, set
    abstract CDATA_SECTION_NODE: float with get, set
    abstract COMMENT_NODE: float with get, set
    abstract DOCUMENT_FRAGMENT_NODE: float with get, set
    abstract DOCUMENT_NODE: float with get, set
    abstract DOCUMENT_POSITION_CONTAINED_BY: float with get, set
    abstract DOCUMENT_POSITION_CONTAINS: float with get, set
    abstract DOCUMENT_POSITION_DISCONNECTED: float with get, set
    abstract DOCUMENT_POSITION_FOLLOWING: float with get, set
    abstract DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: float with get, set
    abstract DOCUMENT_POSITION_PRECEDING: float with get, set
    abstract DOCUMENT_TYPE_NODE: float with get, set
    abstract ELEMENT_NODE: float with get, set
    abstract ENTITY_NODE: float with get, set
    abstract ENTITY_REFERENCE_NODE: float with get, set
    abstract NOTATION_NODE: float with get, set
    abstract PROCESSING_INSTRUCTION_NODE: float with get, set
    abstract TEXT_NODE: float with get, set
    abstract appendChild: newChild: Node -> Node
    abstract cloneNode: ?deep: bool -> Node
    abstract compareDocumentPosition: other: Node -> float
    abstract hasAttributes: unit -> bool
    abstract hasChildNodes: unit -> bool
    abstract insertBefore: newChild: Node * ?refChild: Node -> Node
    abstract isDefaultNamespace: namespaceURI: string -> bool
    abstract isEqualNode: arg: Node -> bool
    abstract isSameNode: other: Node -> bool
    abstract lookupNamespaceURI: prefix: string -> string
    abstract lookupPrefix: namespaceURI: string -> string
    abstract normalize: unit -> unit
    abstract removeChild: oldChild: Node -> Node
    abstract replaceChild: newChild: Node * oldChild: Node -> Node
    abstract contains: node: Node -> bool

and NodeFilter =
    abstract acceptNode: n: Node -> float

and NodeIterator =
    abstract expandEntityReferences: bool with get, set
    abstract filter: NodeFilter with get, set
    abstract root: Node with get, set
    abstract whatToShow: float with get, set
    abstract detach: unit -> unit
    abstract nextNode: unit -> Node
    abstract previousNode: unit -> Node

and NodeList =
    abstract length: float with get, set
    abstract item: index: float -> Node

and OES_element_index_uint =
    interface end

and OES_standard_derivatives =
    abstract FRAGMENT_SHADER_DERIVATIVE_HINT_OES: float with get, set

and OES_texture_float =
    interface end

and OES_texture_float_linear =
    interface end

and OfflineAudioCompletionEvent =
    inherit Event
    abstract renderedBuffer: AudioBuffer with get, set

and OfflineAudioContext =
    inherit AudioContext
    abstract oncomplete: Func<Event, obj> with get, set
    abstract startRendering: unit -> unit
    [<Emit("$0.addEventListener('complete',$1...)")>] abstract addEventListener_complete: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and OscillatorNode =
    inherit AudioNode
    abstract detune: AudioParam with get, set
    abstract frequency: AudioParam with get, set
    abstract onended: Func<Event, obj> with get, set
    abstract ``type``: string with get, set
    abstract setPeriodicWave: periodicWave: PeriodicWave -> unit
    abstract start: ?``when``: float -> unit
    abstract stop: ?``when``: float -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and PageTransitionEvent =
    inherit Event
    abstract persisted: bool with get, set

and PannerNode =
    inherit AudioNode
    abstract coneInnerAngle: float with get, set
    abstract coneOuterAngle: float with get, set
    abstract coneOuterGain: float with get, set
    abstract distanceModel: string with get, set
    abstract maxDistance: float with get, set
    abstract panningModel: string with get, set
    abstract refDistance: float with get, set
    abstract rolloffFactor: float with get, set
    abstract setOrientation: x: float * y: float * z: float -> unit
    abstract setPosition: x: float * y: float * z: float -> unit
    abstract setVelocity: x: float * y: float * z: float -> unit

and PerfWidgetExternal =
    abstract activeNetworkRequestCount: float with get, set
    abstract averageFrameTime: float with get, set
    abstract averagePaintTime: float with get, set
    abstract extraInformationEnabled: bool with get, set
    abstract independentRenderingEnabled: bool with get, set
    abstract irDisablingContentString: string with get, set
    abstract irStatusAvailable: bool with get, set
    abstract maxCpuSpeed: float with get, set
    abstract paintRequestsPerSecond: float with get, set
    abstract performanceCounter: float with get, set
    abstract performanceCounterFrequency: float with get, set
    abstract addEventListener: eventType: string * callback: (obj->obj) -> unit
    abstract getMemoryUsage: unit -> float
    abstract getProcessCpuUsage: unit -> float
    abstract getRecentCpuUsage: last: float -> obj
    abstract getRecentFrames: last: float -> obj
    abstract getRecentMemoryUsage: last: float -> obj
    abstract getRecentPaintRequests: last: float -> obj
    abstract removeEventListener: eventType: string * callback: (obj->obj) -> unit
    abstract repositionWindow: x: float * y: float -> unit
    abstract resizeWindow: width: float * height: float -> unit

and Performance =
    abstract navigation: PerformanceNavigation with get, set
    abstract timing: PerformanceTiming with get, set
    abstract clearMarks: ?markName: string -> unit
    abstract clearMeasures: ?measureName: string -> unit
    abstract clearResourceTimings: unit -> unit
    abstract getEntries: unit -> obj
    abstract getEntriesByName: name: string * ?entryType: string -> obj
    abstract getEntriesByType: entryType: string -> obj
    abstract getMarks: ?markName: string -> obj
    abstract getMeasures: ?measureName: string -> obj
    abstract mark: markName: string -> unit
    abstract ``measure``: measureName: string * ?startMarkName: string * ?endMarkName: string -> unit
    abstract now: unit -> float
    abstract setResourceTimingBufferSize: maxSize: float -> unit
    abstract toJSON: unit -> obj

and PerformanceEntry =
    abstract duration: float with get, set
    abstract entryType: string with get, set
    abstract name: string with get, set
    abstract startTime: float with get, set

and PerformanceMark =
    inherit PerformanceEntry

and PerformanceMeasure =
    inherit PerformanceEntry

and PerformanceNavigation =
    abstract redirectCount: float with get, set
    abstract ``type``: float with get, set
    abstract TYPE_BACK_FORWARD: float with get, set
    abstract TYPE_NAVIGATE: float with get, set
    abstract TYPE_RELOAD: float with get, set
    abstract TYPE_RESERVED: float with get, set
    abstract toJSON: unit -> obj

and PerformanceNavigationTiming =
    inherit PerformanceEntry
    abstract connectEnd: float with get, set
    abstract connectStart: float with get, set
    abstract domComplete: float with get, set
    abstract domContentLoadedEventEnd: float with get, set
    abstract domContentLoadedEventStart: float with get, set
    abstract domInteractive: float with get, set
    abstract domLoading: float with get, set
    abstract domainLookupEnd: float with get, set
    abstract domainLookupStart: float with get, set
    abstract fetchStart: float with get, set
    abstract loadEventEnd: float with get, set
    abstract loadEventStart: float with get, set
    abstract navigationStart: float with get, set
    abstract redirectCount: float with get, set
    abstract redirectEnd: float with get, set
    abstract redirectStart: float with get, set
    abstract requestStart: float with get, set
    abstract responseEnd: float with get, set
    abstract responseStart: float with get, set
    abstract ``type``: string with get, set
    abstract unloadEventEnd: float with get, set
    abstract unloadEventStart: float with get, set

and PerformanceResourceTiming =
    inherit PerformanceEntry
    abstract connectEnd: float with get, set
    abstract connectStart: float with get, set
    abstract domainLookupEnd: float with get, set
    abstract domainLookupStart: float with get, set
    abstract fetchStart: float with get, set
    abstract initiatorType: string with get, set
    abstract redirectEnd: float with get, set
    abstract redirectStart: float with get, set
    abstract requestStart: float with get, set
    abstract responseEnd: float with get, set
    abstract responseStart: float with get, set

and PerformanceTiming =
    abstract connectEnd: float with get, set
    abstract connectStart: float with get, set
    abstract domComplete: float with get, set
    abstract domContentLoadedEventEnd: float with get, set
    abstract domContentLoadedEventStart: float with get, set
    abstract domInteractive: float with get, set
    abstract domLoading: float with get, set
    abstract domainLookupEnd: float with get, set
    abstract domainLookupStart: float with get, set
    abstract fetchStart: float with get, set
    abstract loadEventEnd: float with get, set
    abstract loadEventStart: float with get, set
    abstract msFirstPaint: float with get, set
    abstract navigationStart: float with get, set
    abstract redirectEnd: float with get, set
    abstract redirectStart: float with get, set
    abstract requestStart: float with get, set
    abstract responseEnd: float with get, set
    abstract responseStart: float with get, set
    abstract unloadEventEnd: float with get, set
    abstract unloadEventStart: float with get, set
    abstract toJSON: unit -> obj

and PeriodicWave =
    interface end

and PermissionRequest =
    inherit DeferredPermissionRequest
    abstract state: string with get, set
    abstract defer: unit -> unit

and PermissionRequestedEvent =
    inherit Event
    abstract permissionRequest: PermissionRequest with get, set

and Plugin =
    abstract description: string with get, set
    abstract filename: string with get, set
    abstract length: float with get, set
    abstract name: string with get, set
    abstract version: string with get, set
    abstract item: index: float -> MimeType
    abstract namedItem: ``type``: string -> MimeType

and PluginArray =
    abstract length: float with get, set
    abstract item: index: float -> Plugin
    abstract namedItem: name: string -> Plugin
    abstract refresh: ?reload: bool -> unit

and PointerEvent =
    inherit MouseEvent
    abstract currentPoint: obj with get, set
    abstract height: float with get, set
    abstract hwTimestamp: float with get, set
    abstract intermediatePoints: obj with get, set
    abstract isPrimary: bool with get, set
    abstract pointerId: float with get, set
    abstract pointerType: obj with get, set
    abstract pressure: float with get, set
    abstract rotation: float with get, set
    abstract tiltX: float with get, set
    abstract tiltY: float with get, set
    abstract width: float with get, set
    abstract getCurrentPoint: element: Element -> unit
    abstract getIntermediatePoints: element: Element -> unit
    abstract initPointerEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * ctrlKeyArg: bool * altKeyArg: bool * shiftKeyArg: bool * metaKeyArg: bool * buttonArg: float * relatedTargetArg: EventTarget * offsetXArg: float * offsetYArg: float * widthArg: float * heightArg: float * pressure: float * rotation: float * tiltX: float * tiltY: float * pointerIdArg: float * pointerType: obj * hwTimestampArg: float * isPrimary: bool -> unit

and PopStateEvent =
    inherit Event
    abstract state: obj with get, set
    abstract initPopStateEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * stateArg: obj -> unit

and Position =
    abstract coords: Coordinates with get, set
    abstract timestamp: float with get, set

and PositionError =
    abstract code: float with get, set
    abstract message: string with get, set
    abstract PERMISSION_DENIED: float with get, set
    abstract POSITION_UNAVAILABLE: float with get, set
    abstract TIMEOUT: float with get, set
    abstract toString: unit -> string

and ProcessingInstruction =
    inherit CharacterData
    abstract target: string with get, set

and ProgressEvent =
    inherit Event
    abstract lengthComputable: bool with get, set
    abstract loaded: float with get, set
    abstract total: float with get, set
    abstract initProgressEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * lengthComputableArg: bool * loadedArg: float * totalArg: float -> unit

and Range =
    abstract collapsed: bool with get, set
    abstract commonAncestorContainer: Node with get, set
    abstract endContainer: Node with get, set
    abstract endOffset: float with get, set
    abstract startContainer: Node with get, set
    abstract startOffset: float with get, set
    abstract END_TO_END: float with get, set
    abstract END_TO_START: float with get, set
    abstract START_TO_END: float with get, set
    abstract START_TO_START: float with get, set
    abstract cloneContents: unit -> DocumentFragment
    abstract cloneRange: unit -> Range
    abstract collapse: toStart: bool -> unit
    abstract compareBoundaryPoints: how: float * sourceRange: Range -> float
    abstract createContextualFragment: fragment: string -> DocumentFragment
    abstract deleteContents: unit -> unit
    abstract detach: unit -> unit
    abstract expand: Unit: string -> bool
    abstract extractContents: unit -> DocumentFragment
    abstract getBoundingClientRect: unit -> ClientRect
    abstract getClientRects: unit -> ClientRectList
    abstract insertNode: newNode: Node -> unit
    abstract selectNode: refNode: Node -> unit
    abstract selectNodeContents: refNode: Node -> unit
    abstract setEnd: refNode: Node * offset: float -> unit
    abstract setEndAfter: refNode: Node -> unit
    abstract setEndBefore: refNode: Node -> unit
    abstract setStart: refNode: Node * offset: float -> unit
    abstract setStartAfter: refNode: Node -> unit
    abstract setStartBefore: refNode: Node -> unit
    abstract surroundContents: newParent: Node -> unit
    abstract toString: unit -> string

and SVGAElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGURIReference
    abstract target: SVGAnimatedString with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGAngle =
    abstract unitType: float with get, set
    abstract value: float with get, set
    abstract valueAsString: string with get, set
    abstract valueInSpecifiedUnits: float with get, set
    abstract SVG_ANGLETYPE_DEG: float with get, set
    abstract SVG_ANGLETYPE_GRAD: float with get, set
    abstract SVG_ANGLETYPE_RAD: float with get, set
    abstract SVG_ANGLETYPE_UNKNOWN: float with get, set
    abstract SVG_ANGLETYPE_UNSPECIFIED: float with get, set
    abstract convertToSpecifiedUnits: unitType: float -> unit
    abstract newValueSpecifiedUnits: unitType: float * valueInSpecifiedUnits: float -> unit

and SVGAnimatedAngle =
    abstract animVal: SVGAngle with get, set
    abstract baseVal: SVGAngle with get, set

and SVGAnimatedBoolean =
    abstract animVal: bool with get, set
    abstract baseVal: bool with get, set

and SVGAnimatedEnumeration =
    abstract animVal: float with get, set
    abstract baseVal: float with get, set

and SVGAnimatedInteger =
    abstract animVal: float with get, set
    abstract baseVal: float with get, set

and SVGAnimatedLength =
    abstract animVal: SVGLength with get, set
    abstract baseVal: SVGLength with get, set

and SVGAnimatedLengthList =
    abstract animVal: SVGLengthList with get, set
    abstract baseVal: SVGLengthList with get, set

and SVGAnimatedNumber =
    abstract animVal: float with get, set
    abstract baseVal: float with get, set

and SVGAnimatedNumberList =
    abstract animVal: SVGNumberList with get, set
    abstract baseVal: SVGNumberList with get, set

and SVGAnimatedPreserveAspectRatio =
    abstract animVal: SVGPreserveAspectRatio with get, set
    abstract baseVal: SVGPreserveAspectRatio with get, set

and SVGAnimatedRect =
    abstract animVal: SVGRect with get, set
    abstract baseVal: SVGRect with get, set

and SVGAnimatedString =
    abstract animVal: string with get, set
    abstract baseVal: string with get, set

and SVGAnimatedTransformList =
    abstract animVal: SVGTransformList with get, set
    abstract baseVal: SVGTransformList with get, set

and SVGCircleElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract cx: SVGAnimatedLength with get, set
    abstract cy: SVGAnimatedLength with get, set
    abstract r: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGClipPathElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGUnitTypes
    abstract clipPathUnits: SVGAnimatedEnumeration with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGComponentTransferFunctionElement =
    inherit SVGElement
    abstract amplitude: SVGAnimatedNumber with get, set
    abstract exponent: SVGAnimatedNumber with get, set
    abstract intercept: SVGAnimatedNumber with get, set
    abstract offset: SVGAnimatedNumber with get, set
    abstract slope: SVGAnimatedNumber with get, set
    abstract tableValues: SVGAnimatedNumberList with get, set
    abstract ``type``: SVGAnimatedEnumeration with get, set
    abstract SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE: float with get, set
    abstract SVG_FECOMPONENTTRANSFER_TYPE_GAMMA: float with get, set
    abstract SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY: float with get, set
    abstract SVG_FECOMPONENTTRANSFER_TYPE_LINEAR: float with get, set
    abstract SVG_FECOMPONENTTRANSFER_TYPE_TABLE: float with get, set
    abstract SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN: float with get, set

and SVGDefsElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGDescElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGLangSpace
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGElement =
    inherit Element
    abstract id: string with get, set
    abstract onclick: Func<MouseEvent, obj> with get, set
    abstract ondblclick: Func<MouseEvent, obj> with get, set
    abstract onfocusin: Func<FocusEvent, obj> with get, set
    abstract onfocusout: Func<FocusEvent, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onmousedown: Func<MouseEvent, obj> with get, set
    abstract onmousemove: Func<MouseEvent, obj> with get, set
    abstract onmouseout: Func<MouseEvent, obj> with get, set
    abstract onmouseover: Func<MouseEvent, obj> with get, set
    abstract onmouseup: Func<MouseEvent, obj> with get, set
    abstract ownerSVGElement: SVGSVGElement with get, set
    abstract viewportElement: SVGElement with get, set
    abstract xmlbase: string with get, set
    abstract className: obj with get, set
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focusin',$1...)")>] abstract addEventListener_focusin: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focusout',$1...)")>] abstract addEventListener_focusout: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGElementInstance =
    inherit EventTarget
    abstract childNodes: SVGElementInstanceList with get, set
    abstract correspondingElement: SVGElement with get, set
    abstract correspondingUseElement: SVGUseElement with get, set
    abstract firstChild: SVGElementInstance with get, set
    abstract lastChild: SVGElementInstance with get, set
    abstract nextSibling: SVGElementInstance with get, set
    abstract parentNode: SVGElementInstance with get, set
    abstract previousSibling: SVGElementInstance with get, set

and SVGElementInstanceList =
    abstract length: float with get, set
    abstract item: index: float -> SVGElementInstance

and SVGEllipseElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract cx: SVGAnimatedLength with get, set
    abstract cy: SVGAnimatedLength with get, set
    abstract rx: SVGAnimatedLength with get, set
    abstract ry: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEBlendElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract in2: SVGAnimatedString with get, set
    abstract mode: SVGAnimatedEnumeration with get, set
    abstract SVG_FEBLEND_MODE_COLOR: float with get, set
    abstract SVG_FEBLEND_MODE_COLOR_BURN: float with get, set
    abstract SVG_FEBLEND_MODE_COLOR_DODGE: float with get, set
    abstract SVG_FEBLEND_MODE_DARKEN: float with get, set
    abstract SVG_FEBLEND_MODE_DIFFERENCE: float with get, set
    abstract SVG_FEBLEND_MODE_EXCLUSION: float with get, set
    abstract SVG_FEBLEND_MODE_HARD_LIGHT: float with get, set
    abstract SVG_FEBLEND_MODE_HUE: float with get, set
    abstract SVG_FEBLEND_MODE_LIGHTEN: float with get, set
    abstract SVG_FEBLEND_MODE_LUMINOSITY: float with get, set
    abstract SVG_FEBLEND_MODE_MULTIPLY: float with get, set
    abstract SVG_FEBLEND_MODE_NORMAL: float with get, set
    abstract SVG_FEBLEND_MODE_OVERLAY: float with get, set
    abstract SVG_FEBLEND_MODE_SATURATION: float with get, set
    abstract SVG_FEBLEND_MODE_SCREEN: float with get, set
    abstract SVG_FEBLEND_MODE_SOFT_LIGHT: float with get, set
    abstract SVG_FEBLEND_MODE_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEColorMatrixElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract ``type``: SVGAnimatedEnumeration with get, set
    abstract values: SVGAnimatedNumberList with get, set
    abstract SVG_FECOLORMATRIX_TYPE_HUEROTATE: float with get, set
    abstract SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA: float with get, set
    abstract SVG_FECOLORMATRIX_TYPE_MATRIX: float with get, set
    abstract SVG_FECOLORMATRIX_TYPE_SATURATE: float with get, set
    abstract SVG_FECOLORMATRIX_TYPE_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEComponentTransferElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFECompositeElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract in2: SVGAnimatedString with get, set
    abstract k1: SVGAnimatedNumber with get, set
    abstract k2: SVGAnimatedNumber with get, set
    abstract k3: SVGAnimatedNumber with get, set
    abstract k4: SVGAnimatedNumber with get, set
    abstract operator: SVGAnimatedEnumeration with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_ARITHMETIC: float with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_ATOP: float with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_IN: float with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_OUT: float with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_OVER: float with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_UNKNOWN: float with get, set
    abstract SVG_FECOMPOSITE_OPERATOR_XOR: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEConvolveMatrixElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract bias: SVGAnimatedNumber with get, set
    abstract divisor: SVGAnimatedNumber with get, set
    abstract edgeMode: SVGAnimatedEnumeration with get, set
    abstract in1: SVGAnimatedString with get, set
    abstract kernelMatrix: SVGAnimatedNumberList with get, set
    abstract kernelUnitLengthX: SVGAnimatedNumber with get, set
    abstract kernelUnitLengthY: SVGAnimatedNumber with get, set
    abstract orderX: SVGAnimatedInteger with get, set
    abstract orderY: SVGAnimatedInteger with get, set
    abstract preserveAlpha: SVGAnimatedBoolean with get, set
    abstract targetX: SVGAnimatedInteger with get, set
    abstract targetY: SVGAnimatedInteger with get, set
    abstract SVG_EDGEMODE_DUPLICATE: float with get, set
    abstract SVG_EDGEMODE_NONE: float with get, set
    abstract SVG_EDGEMODE_UNKNOWN: float with get, set
    abstract SVG_EDGEMODE_WRAP: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEDiffuseLightingElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract diffuseConstant: SVGAnimatedNumber with get, set
    abstract in1: SVGAnimatedString with get, set
    abstract kernelUnitLengthX: SVGAnimatedNumber with get, set
    abstract kernelUnitLengthY: SVGAnimatedNumber with get, set
    abstract surfaceScale: SVGAnimatedNumber with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEDisplacementMapElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract in2: SVGAnimatedString with get, set
    abstract scale: SVGAnimatedNumber with get, set
    abstract xChannelSelector: SVGAnimatedEnumeration with get, set
    abstract yChannelSelector: SVGAnimatedEnumeration with get, set
    abstract SVG_CHANNEL_A: float with get, set
    abstract SVG_CHANNEL_B: float with get, set
    abstract SVG_CHANNEL_G: float with get, set
    abstract SVG_CHANNEL_R: float with get, set
    abstract SVG_CHANNEL_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEDistantLightElement =
    inherit SVGElement
    abstract azimuth: SVGAnimatedNumber with get, set
    abstract elevation: SVGAnimatedNumber with get, set

and SVGFEFloodElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEFuncAElement =
    inherit SVGComponentTransferFunctionElement

and SVGFEFuncBElement =
    inherit SVGComponentTransferFunctionElement

and SVGFEFuncGElement =
    inherit SVGComponentTransferFunctionElement

and SVGFEFuncRElement =
    inherit SVGComponentTransferFunctionElement

and SVGFEGaussianBlurElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract stdDeviationX: SVGAnimatedNumber with get, set
    abstract stdDeviationY: SVGAnimatedNumber with get, set
    abstract setStdDeviation: stdDeviationX: float * stdDeviationY: float -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEImageElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    inherit SVGLangSpace
    inherit SVGURIReference
    inherit SVGExternalResourcesRequired
    abstract preserveAspectRatio: SVGAnimatedPreserveAspectRatio with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEMergeElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEMergeNodeElement =
    inherit SVGElement
    abstract in1: SVGAnimatedString with get, set

and SVGFEMorphologyElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract operator: SVGAnimatedEnumeration with get, set
    abstract radiusX: SVGAnimatedNumber with get, set
    abstract radiusY: SVGAnimatedNumber with get, set
    abstract SVG_MORPHOLOGY_OPERATOR_DILATE: float with get, set
    abstract SVG_MORPHOLOGY_OPERATOR_ERODE: float with get, set
    abstract SVG_MORPHOLOGY_OPERATOR_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEOffsetElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract dx: SVGAnimatedNumber with get, set
    abstract dy: SVGAnimatedNumber with get, set
    abstract in1: SVGAnimatedString with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFEPointLightElement =
    inherit SVGElement
    abstract x: SVGAnimatedNumber with get, set
    abstract y: SVGAnimatedNumber with get, set
    abstract z: SVGAnimatedNumber with get, set

and SVGFESpecularLightingElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract kernelUnitLengthX: SVGAnimatedNumber with get, set
    abstract kernelUnitLengthY: SVGAnimatedNumber with get, set
    abstract specularConstant: SVGAnimatedNumber with get, set
    abstract specularExponent: SVGAnimatedNumber with get, set
    abstract surfaceScale: SVGAnimatedNumber with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFESpotLightElement =
    inherit SVGElement
    abstract limitingConeAngle: SVGAnimatedNumber with get, set
    abstract pointsAtX: SVGAnimatedNumber with get, set
    abstract pointsAtY: SVGAnimatedNumber with get, set
    abstract pointsAtZ: SVGAnimatedNumber with get, set
    abstract specularExponent: SVGAnimatedNumber with get, set
    abstract x: SVGAnimatedNumber with get, set
    abstract y: SVGAnimatedNumber with get, set
    abstract z: SVGAnimatedNumber with get, set

and SVGFETileElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract in1: SVGAnimatedString with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFETurbulenceElement =
    inherit SVGElement
    inherit SVGFilterPrimitiveStandardAttributes
    abstract baseFrequencyX: SVGAnimatedNumber with get, set
    abstract baseFrequencyY: SVGAnimatedNumber with get, set
    abstract numOctaves: SVGAnimatedInteger with get, set
    abstract seed: SVGAnimatedNumber with get, set
    abstract stitchTiles: SVGAnimatedEnumeration with get, set
    abstract ``type``: SVGAnimatedEnumeration with get, set
    abstract SVG_STITCHTYPE_NOSTITCH: float with get, set
    abstract SVG_STITCHTYPE_STITCH: float with get, set
    abstract SVG_STITCHTYPE_UNKNOWN: float with get, set
    abstract SVG_TURBULENCE_TYPE_FRACTALNOISE: float with get, set
    abstract SVG_TURBULENCE_TYPE_TURBULENCE: float with get, set
    abstract SVG_TURBULENCE_TYPE_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGFilterElement =
    inherit SVGElement
    inherit SVGUnitTypes
    inherit SVGStylable
    inherit SVGLangSpace
    inherit SVGURIReference
    inherit SVGExternalResourcesRequired
    abstract filterResX: SVGAnimatedInteger with get, set
    abstract filterResY: SVGAnimatedInteger with get, set
    abstract filterUnits: SVGAnimatedEnumeration with get, set
    abstract height: SVGAnimatedLength with get, set
    abstract primitiveUnits: SVGAnimatedEnumeration with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract setFilterRes: filterResX: float * filterResY: float -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGForeignObjectElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract height: SVGAnimatedLength with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGGElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGGradientElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGExternalResourcesRequired
    inherit SVGURIReference
    inherit SVGUnitTypes
    abstract gradientTransform: SVGAnimatedTransformList with get, set
    abstract gradientUnits: SVGAnimatedEnumeration with get, set
    abstract spreadMethod: SVGAnimatedEnumeration with get, set
    abstract SVG_SPREADMETHOD_PAD: float with get, set
    abstract SVG_SPREADMETHOD_REFLECT: float with get, set
    abstract SVG_SPREADMETHOD_REPEAT: float with get, set
    abstract SVG_SPREADMETHOD_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGImageElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGURIReference
    abstract height: SVGAnimatedLength with get, set
    abstract preserveAspectRatio: SVGAnimatedPreserveAspectRatio with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGLength =
    abstract unitType: float with get, set
    abstract value: float with get, set
    abstract valueAsString: string with get, set
    abstract valueInSpecifiedUnits: float with get, set
    abstract SVG_LENGTHTYPE_CM: float with get, set
    abstract SVG_LENGTHTYPE_EMS: float with get, set
    abstract SVG_LENGTHTYPE_EXS: float with get, set
    abstract SVG_LENGTHTYPE_IN: float with get, set
    abstract SVG_LENGTHTYPE_MM: float with get, set
    abstract SVG_LENGTHTYPE_NUMBER: float with get, set
    abstract SVG_LENGTHTYPE_PC: float with get, set
    abstract SVG_LENGTHTYPE_PERCENTAGE: float with get, set
    abstract SVG_LENGTHTYPE_PT: float with get, set
    abstract SVG_LENGTHTYPE_PX: float with get, set
    abstract SVG_LENGTHTYPE_UNKNOWN: float with get, set
    abstract convertToSpecifiedUnits: unitType: float -> unit
    abstract newValueSpecifiedUnits: unitType: float * valueInSpecifiedUnits: float -> unit

and SVGLengthList =
    abstract numberOfItems: float with get, set
    abstract appendItem: newItem: SVGLength -> SVGLength
    abstract clear: unit -> unit
    abstract getItem: index: float -> SVGLength
    abstract initialize: newItem: SVGLength -> SVGLength
    abstract insertItemBefore: newItem: SVGLength * index: float -> SVGLength
    abstract removeItem: index: float -> SVGLength
    abstract replaceItem: newItem: SVGLength * index: float -> SVGLength

and SVGLineElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract x1: SVGAnimatedLength with get, set
    abstract x2: SVGAnimatedLength with get, set
    abstract y1: SVGAnimatedLength with get, set
    abstract y2: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGLinearGradientElement =
    inherit SVGGradientElement
    abstract x1: SVGAnimatedLength with get, set
    abstract x2: SVGAnimatedLength with get, set
    abstract y1: SVGAnimatedLength with get, set
    abstract y2: SVGAnimatedLength with get, set

and SVGMarkerElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGFitToViewBox
    abstract markerHeight: SVGAnimatedLength with get, set
    abstract markerUnits: SVGAnimatedEnumeration with get, set
    abstract markerWidth: SVGAnimatedLength with get, set
    abstract orientAngle: SVGAnimatedAngle with get, set
    abstract orientType: SVGAnimatedEnumeration with get, set
    abstract refX: SVGAnimatedLength with get, set
    abstract refY: SVGAnimatedLength with get, set
    abstract SVG_MARKERUNITS_STROKEWIDTH: float with get, set
    abstract SVG_MARKERUNITS_UNKNOWN: float with get, set
    abstract SVG_MARKERUNITS_USERSPACEONUSE: float with get, set
    abstract SVG_MARKER_ORIENT_ANGLE: float with get, set
    abstract SVG_MARKER_ORIENT_AUTO: float with get, set
    abstract SVG_MARKER_ORIENT_UNKNOWN: float with get, set
    abstract setOrientToAngle: angle: SVGAngle -> unit
    abstract setOrientToAuto: unit -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGMaskElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGUnitTypes
    abstract height: SVGAnimatedLength with get, set
    abstract maskContentUnits: SVGAnimatedEnumeration with get, set
    abstract maskUnits: SVGAnimatedEnumeration with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGMatrix =
    abstract a: float with get, set
    abstract b: float with get, set
    abstract c: float with get, set
    abstract d: float with get, set
    abstract e: float with get, set
    abstract f: float with get, set
    abstract flipX: unit -> SVGMatrix
    abstract flipY: unit -> SVGMatrix
    abstract inverse: unit -> SVGMatrix
    abstract multiply: secondMatrix: SVGMatrix -> SVGMatrix
    abstract rotate: angle: float -> SVGMatrix
    abstract rotateFromVector: x: float * y: float -> SVGMatrix
    abstract scale: scaleFactor: float -> SVGMatrix
    abstract scaleNonUniform: scaleFactorX: float * scaleFactorY: float -> SVGMatrix
    abstract skewX: angle: float -> SVGMatrix
    abstract skewY: angle: float -> SVGMatrix
    abstract translate: x: float * y: float -> SVGMatrix

and SVGMetadataElement =
    inherit SVGElement

and SVGNumber =
    abstract value: float with get, set

and SVGNumberList =
    abstract numberOfItems: float with get, set
    abstract appendItem: newItem: SVGNumber -> SVGNumber
    abstract clear: unit -> unit
    abstract getItem: index: float -> SVGNumber
    abstract initialize: newItem: SVGNumber -> SVGNumber
    abstract insertItemBefore: newItem: SVGNumber * index: float -> SVGNumber
    abstract removeItem: index: float -> SVGNumber
    abstract replaceItem: newItem: SVGNumber * index: float -> SVGNumber

and SVGPathElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGAnimatedPathData
    abstract createSVGPathSegArcAbs: x: float * y: float * r1: float * r2: float * angle: float * largeArcFlag: bool * sweepFlag: bool -> SVGPathSegArcAbs
    abstract createSVGPathSegArcRel: x: float * y: float * r1: float * r2: float * angle: float * largeArcFlag: bool * sweepFlag: bool -> SVGPathSegArcRel
    abstract createSVGPathSegClosePath: unit -> SVGPathSegClosePath
    abstract createSVGPathSegCurvetoCubicAbs: x: float * y: float * x1: float * y1: float * x2: float * y2: float -> SVGPathSegCurvetoCubicAbs
    abstract createSVGPathSegCurvetoCubicRel: x: float * y: float * x1: float * y1: float * x2: float * y2: float -> SVGPathSegCurvetoCubicRel
    abstract createSVGPathSegCurvetoCubicSmoothAbs: x: float * y: float * x2: float * y2: float -> SVGPathSegCurvetoCubicSmoothAbs
    abstract createSVGPathSegCurvetoCubicSmoothRel: x: float * y: float * x2: float * y2: float -> SVGPathSegCurvetoCubicSmoothRel
    abstract createSVGPathSegCurvetoQuadraticAbs: x: float * y: float * x1: float * y1: float -> SVGPathSegCurvetoQuadraticAbs
    abstract createSVGPathSegCurvetoQuadraticRel: x: float * y: float * x1: float * y1: float -> SVGPathSegCurvetoQuadraticRel
    abstract createSVGPathSegCurvetoQuadraticSmoothAbs: x: float * y: float -> SVGPathSegCurvetoQuadraticSmoothAbs
    abstract createSVGPathSegCurvetoQuadraticSmoothRel: x: float * y: float -> SVGPathSegCurvetoQuadraticSmoothRel
    abstract createSVGPathSegLinetoAbs: x: float * y: float -> SVGPathSegLinetoAbs
    abstract createSVGPathSegLinetoHorizontalAbs: x: float -> SVGPathSegLinetoHorizontalAbs
    abstract createSVGPathSegLinetoHorizontalRel: x: float -> SVGPathSegLinetoHorizontalRel
    abstract createSVGPathSegLinetoRel: x: float * y: float -> SVGPathSegLinetoRel
    abstract createSVGPathSegLinetoVerticalAbs: y: float -> SVGPathSegLinetoVerticalAbs
    abstract createSVGPathSegLinetoVerticalRel: y: float -> SVGPathSegLinetoVerticalRel
    abstract createSVGPathSegMovetoAbs: x: float * y: float -> SVGPathSegMovetoAbs
    abstract createSVGPathSegMovetoRel: x: float * y: float -> SVGPathSegMovetoRel
    abstract getPathSegAtLength: distance: float -> float
    abstract getPointAtLength: distance: float -> SVGPoint
    abstract getTotalLength: unit -> float
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGPathSeg =
    abstract pathSegType: float with get, set
    abstract pathSegTypeAsLetter: string with get, set
    abstract PATHSEG_ARC_ABS: float with get, set
    abstract PATHSEG_ARC_REL: float with get, set
    abstract PATHSEG_CLOSEPATH: float with get, set
    abstract PATHSEG_CURVETO_CUBIC_ABS: float with get, set
    abstract PATHSEG_CURVETO_CUBIC_REL: float with get, set
    abstract PATHSEG_CURVETO_CUBIC_SMOOTH_ABS: float with get, set
    abstract PATHSEG_CURVETO_CUBIC_SMOOTH_REL: float with get, set
    abstract PATHSEG_CURVETO_QUADRATIC_ABS: float with get, set
    abstract PATHSEG_CURVETO_QUADRATIC_REL: float with get, set
    abstract PATHSEG_CURVETO_QUADRATIC_SMOOTH_ABS: float with get, set
    abstract PATHSEG_CURVETO_QUADRATIC_SMOOTH_REL: float with get, set
    abstract PATHSEG_LINETO_ABS: float with get, set
    abstract PATHSEG_LINETO_HORIZONTAL_ABS: float with get, set
    abstract PATHSEG_LINETO_HORIZONTAL_REL: float with get, set
    abstract PATHSEG_LINETO_REL: float with get, set
    abstract PATHSEG_LINETO_VERTICAL_ABS: float with get, set
    abstract PATHSEG_LINETO_VERTICAL_REL: float with get, set
    abstract PATHSEG_MOVETO_ABS: float with get, set
    abstract PATHSEG_MOVETO_REL: float with get, set
    abstract PATHSEG_UNKNOWN: float with get, set

and SVGPathSegArcAbs =
    inherit SVGPathSeg
    abstract angle: float with get, set
    abstract largeArcFlag: bool with get, set
    abstract r1: float with get, set
    abstract r2: float with get, set
    abstract sweepFlag: bool with get, set
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegArcRel =
    inherit SVGPathSeg
    abstract angle: float with get, set
    abstract largeArcFlag: bool with get, set
    abstract r1: float with get, set
    abstract r2: float with get, set
    abstract sweepFlag: bool with get, set
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegClosePath =
    inherit SVGPathSeg

and SVGPathSegCurvetoCubicAbs =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract x1: float with get, set
    abstract x2: float with get, set
    abstract y: float with get, set
    abstract y1: float with get, set
    abstract y2: float with get, set

and SVGPathSegCurvetoCubicRel =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract x1: float with get, set
    abstract x2: float with get, set
    abstract y: float with get, set
    abstract y1: float with get, set
    abstract y2: float with get, set

and SVGPathSegCurvetoCubicSmoothAbs =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract x2: float with get, set
    abstract y: float with get, set
    abstract y2: float with get, set

and SVGPathSegCurvetoCubicSmoothRel =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract x2: float with get, set
    abstract y: float with get, set
    abstract y2: float with get, set

and SVGPathSegCurvetoQuadraticAbs =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract x1: float with get, set
    abstract y: float with get, set
    abstract y1: float with get, set

and SVGPathSegCurvetoQuadraticRel =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract x1: float with get, set
    abstract y: float with get, set
    abstract y1: float with get, set

and SVGPathSegCurvetoQuadraticSmoothAbs =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegCurvetoQuadraticSmoothRel =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegLinetoAbs =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegLinetoHorizontalAbs =
    inherit SVGPathSeg
    abstract x: float with get, set

and SVGPathSegLinetoHorizontalRel =
    inherit SVGPathSeg
    abstract x: float with get, set

and SVGPathSegLinetoRel =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegLinetoVerticalAbs =
    inherit SVGPathSeg
    abstract y: float with get, set

and SVGPathSegLinetoVerticalRel =
    inherit SVGPathSeg
    abstract y: float with get, set

and SVGPathSegList =
    abstract numberOfItems: float with get, set
    abstract appendItem: newItem: SVGPathSeg -> SVGPathSeg
    abstract clear: unit -> unit
    abstract getItem: index: float -> SVGPathSeg
    abstract initialize: newItem: SVGPathSeg -> SVGPathSeg
    abstract insertItemBefore: newItem: SVGPathSeg * index: float -> SVGPathSeg
    abstract removeItem: index: float -> SVGPathSeg
    abstract replaceItem: newItem: SVGPathSeg * index: float -> SVGPathSeg

and SVGPathSegMovetoAbs =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPathSegMovetoRel =
    inherit SVGPathSeg
    abstract x: float with get, set
    abstract y: float with get, set

and SVGPatternElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGFitToViewBox
    inherit SVGURIReference
    inherit SVGUnitTypes
    abstract height: SVGAnimatedLength with get, set
    abstract patternContentUnits: SVGAnimatedEnumeration with get, set
    abstract patternTransform: SVGAnimatedTransformList with get, set
    abstract patternUnits: SVGAnimatedEnumeration with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGPoint =
    abstract x: float with get, set
    abstract y: float with get, set
    abstract matrixTransform: matrix: SVGMatrix -> SVGPoint

and SVGPointList =
    abstract numberOfItems: float with get, set
    abstract appendItem: newItem: SVGPoint -> SVGPoint
    abstract clear: unit -> unit
    abstract getItem: index: float -> SVGPoint
    abstract initialize: newItem: SVGPoint -> SVGPoint
    abstract insertItemBefore: newItem: SVGPoint * index: float -> SVGPoint
    abstract removeItem: index: float -> SVGPoint
    abstract replaceItem: newItem: SVGPoint * index: float -> SVGPoint

and SVGPolygonElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGAnimatedPoints
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGPolylineElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGAnimatedPoints
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGPreserveAspectRatio =
    abstract align: float with get, set
    abstract meetOrSlice: float with get, set
    abstract SVG_MEETORSLICE_MEET: float with get, set
    abstract SVG_MEETORSLICE_SLICE: float with get, set
    abstract SVG_MEETORSLICE_UNKNOWN: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_NONE: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_UNKNOWN: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMAXYMAX: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMAXYMID: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMAXYMIN: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMIDYMAX: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMIDYMID: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMIDYMIN: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMINYMAX: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMINYMID: float with get, set
    abstract SVG_PRESERVEASPECTRATIO_XMINYMIN: float with get, set

and SVGRadialGradientElement =
    inherit SVGGradientElement
    abstract cx: SVGAnimatedLength with get, set
    abstract cy: SVGAnimatedLength with get, set
    abstract fx: SVGAnimatedLength with get, set
    abstract fy: SVGAnimatedLength with get, set
    abstract r: SVGAnimatedLength with get, set

and SVGRect =
    abstract height: float with get, set
    abstract width: float with get, set
    abstract x: float with get, set
    abstract y: float with get, set

and SVGRectElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract height: SVGAnimatedLength with get, set
    abstract rx: SVGAnimatedLength with get, set
    abstract ry: SVGAnimatedLength with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGSVGElement =
    inherit SVGElement
    inherit DocumentEvent
    inherit SVGLocatable
    inherit SVGTests
    inherit SVGStylable
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGFitToViewBox
    inherit SVGZoomAndPan
    abstract contentScriptType: string with get, set
    abstract contentStyleType: string with get, set
    abstract currentScale: float with get, set
    abstract currentTranslate: SVGPoint with get, set
    abstract height: SVGAnimatedLength with get, set
    abstract onabort: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onresize: Func<UIEvent, obj> with get, set
    abstract onscroll: Func<UIEvent, obj> with get, set
    abstract onunload: Func<Event, obj> with get, set
    abstract onzoom: Func<SVGZoomEvent, obj> with get, set
    abstract pixelUnitToMillimeterX: float with get, set
    abstract pixelUnitToMillimeterY: float with get, set
    abstract screenPixelToMillimeterX: float with get, set
    abstract screenPixelToMillimeterY: float with get, set
    abstract viewport: SVGRect with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract checkEnclosure: element: SVGElement * rect: SVGRect -> bool
    abstract checkIntersection: element: SVGElement * rect: SVGRect -> bool
    abstract createSVGAngle: unit -> SVGAngle
    abstract createSVGLength: unit -> SVGLength
    abstract createSVGMatrix: unit -> SVGMatrix
    abstract createSVGNumber: unit -> SVGNumber
    abstract createSVGPoint: unit -> SVGPoint
    abstract createSVGRect: unit -> SVGRect
    abstract createSVGTransform: unit -> SVGTransform
    abstract createSVGTransformFromMatrix: matrix: SVGMatrix -> SVGTransform
    abstract deselectAll: unit -> unit
    abstract forceRedraw: unit -> unit
    abstract getComputedStyle: elt: Element * ?pseudoElt: string -> CSSStyleDeclaration
    abstract getCurrentTime: unit -> float
    abstract getElementById: elementId: string -> Element
    abstract getEnclosureList: rect: SVGRect * referenceElement: SVGElement -> NodeList
    abstract getIntersectionList: rect: SVGRect * referenceElement: SVGElement -> NodeList
    abstract pauseAnimations: unit -> unit
    abstract setCurrentTime: seconds: float -> unit
    abstract suspendRedraw: maxWaitMilliseconds: float -> float
    abstract unpauseAnimations: unit -> unit
    abstract unsuspendRedraw: suspendHandleID: float -> unit
    abstract unsuspendRedrawAll: unit -> unit
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGotPointerCapture',$1...)")>] abstract addEventListener_MSGotPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSLostPointerCapture',$1...)")>] abstract addEventListener_MSLostPointerCapture: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('SVGAbort',$1...)")>] abstract addEventListener_SVGAbort: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('SVGError',$1...)")>] abstract addEventListener_SVGError: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('SVGUnload',$1...)")>] abstract addEventListener_SVGUnload: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('SVGZoom',$1...)")>] abstract addEventListener_SVGZoom: listener: Func<SVGZoomEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ariarequest',$1...)")>] abstract addEventListener_ariarequest: listener: Func<AriaRequestEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('command',$1...)")>] abstract addEventListener_command: listener: Func<CommandEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focusin',$1...)")>] abstract addEventListener_focusin: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focusout',$1...)")>] abstract addEventListener_focusout: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('gotpointercapture',$1...)")>] abstract addEventListener_gotpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('lostpointercapture',$1...)")>] abstract addEventListener_lostpointercapture: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('resize',$1...)")>] abstract addEventListener_resize: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchcancel',$1...)")>] abstract addEventListener_touchcancel: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchend',$1...)")>] abstract addEventListener_touchend: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchmove',$1...)")>] abstract addEventListener_touchmove: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('touchstart',$1...)")>] abstract addEventListener_touchstart: listener: Func<TouchEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenchange',$1...)")>] abstract addEventListener_webkitfullscreenchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('webkitfullscreenerror',$1...)")>] abstract addEventListener_webkitfullscreenerror: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGScriptElement =
    inherit SVGElement
    inherit SVGExternalResourcesRequired
    inherit SVGURIReference
    abstract ``type``: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGStopElement =
    inherit SVGElement
    inherit SVGStylable
    abstract offset: SVGAnimatedNumber with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGStringList =
    abstract numberOfItems: float with get, set
    abstract appendItem: newItem: string -> string
    abstract clear: unit -> unit
    abstract getItem: index: float -> string
    abstract initialize: newItem: string -> string
    abstract insertItemBefore: newItem: string * index: float -> string
    abstract removeItem: index: float -> string
    abstract replaceItem: newItem: string * index: float -> string

and SVGStyleElement =
    inherit SVGElement
    inherit SVGLangSpace
    abstract media: string with get, set
    abstract title: string with get, set
    abstract ``type``: string with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGSwitchElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGSymbolElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGFitToViewBox
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGTSpanElement =
    inherit SVGTextPositioningElement

and SVGTextContentElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    abstract lengthAdjust: SVGAnimatedEnumeration with get, set
    abstract textLength: SVGAnimatedLength with get, set
    abstract LENGTHADJUST_SPACING: float with get, set
    abstract LENGTHADJUST_SPACINGANDGLYPHS: float with get, set
    abstract LENGTHADJUST_UNKNOWN: float with get, set
    abstract getCharNumAtPosition: point: SVGPoint -> float
    abstract getComputedTextLength: unit -> float
    abstract getEndPositionOfChar: charnum: float -> SVGPoint
    abstract getExtentOfChar: charnum: float -> SVGRect
    abstract getNumberOfChars: unit -> float
    abstract getRotationOfChar: charnum: float -> float
    abstract getStartPositionOfChar: charnum: float -> SVGPoint
    abstract getSubStringLength: charnum: float * nchars: float -> float
    abstract selectSubString: charnum: float * nchars: float -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGTextElement =
    inherit SVGTextPositioningElement
    inherit SVGTransformable
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGTextPathElement =
    inherit SVGTextContentElement
    inherit SVGURIReference
    abstract ``method``: SVGAnimatedEnumeration with get, set
    abstract spacing: SVGAnimatedEnumeration with get, set
    abstract startOffset: SVGAnimatedLength with get, set
    abstract TEXTPATH_METHODTYPE_ALIGN: float with get, set
    abstract TEXTPATH_METHODTYPE_STRETCH: float with get, set
    abstract TEXTPATH_METHODTYPE_UNKNOWN: float with get, set
    abstract TEXTPATH_SPACINGTYPE_AUTO: float with get, set
    abstract TEXTPATH_SPACINGTYPE_EXACT: float with get, set
    abstract TEXTPATH_SPACINGTYPE_UNKNOWN: float with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGTextPositioningElement =
    inherit SVGTextContentElement
    abstract dx: SVGAnimatedLengthList with get, set
    abstract dy: SVGAnimatedLengthList with get, set
    abstract rotate: SVGAnimatedNumberList with get, set
    abstract x: SVGAnimatedLengthList with get, set
    abstract y: SVGAnimatedLengthList with get, set

and SVGTitleElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGLangSpace
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGTransform =
    abstract angle: float with get, set
    abstract matrix: SVGMatrix with get, set
    abstract ``type``: float with get, set
    abstract SVG_TRANSFORM_MATRIX: float with get, set
    abstract SVG_TRANSFORM_ROTATE: float with get, set
    abstract SVG_TRANSFORM_SCALE: float with get, set
    abstract SVG_TRANSFORM_SKEWX: float with get, set
    abstract SVG_TRANSFORM_SKEWY: float with get, set
    abstract SVG_TRANSFORM_TRANSLATE: float with get, set
    abstract SVG_TRANSFORM_UNKNOWN: float with get, set
    abstract setMatrix: matrix: SVGMatrix -> unit
    abstract setRotate: angle: float * cx: float * cy: float -> unit
    abstract setScale: sx: float * sy: float -> unit
    abstract setSkewX: angle: float -> unit
    abstract setSkewY: angle: float -> unit
    abstract setTranslate: tx: float * ty: float -> unit

and SVGTransformList =
    abstract numberOfItems: float with get, set
    abstract appendItem: newItem: SVGTransform -> SVGTransform
    abstract clear: unit -> unit
    abstract consolidate: unit -> SVGTransform
    abstract createSVGTransformFromMatrix: matrix: SVGMatrix -> SVGTransform
    abstract getItem: index: float -> SVGTransform
    abstract initialize: newItem: SVGTransform -> SVGTransform
    abstract insertItemBefore: newItem: SVGTransform * index: float -> SVGTransform
    abstract removeItem: index: float -> SVGTransform
    abstract replaceItem: newItem: SVGTransform * index: float -> SVGTransform

and SVGUnitTypes =
    abstract SVG_UNIT_TYPE_OBJECTBOUNDINGBOX: float with get, set
    abstract SVG_UNIT_TYPE_UNKNOWN: float with get, set
    abstract SVG_UNIT_TYPE_USERSPACEONUSE: float with get, set

and SVGUseElement =
    inherit SVGElement
    inherit SVGStylable
    inherit SVGTransformable
    inherit SVGTests
    inherit SVGLangSpace
    inherit SVGExternalResourcesRequired
    inherit SVGURIReference
    abstract animatedInstanceRoot: SVGElementInstance with get, set
    abstract height: SVGAnimatedLength with get, set
    abstract instanceRoot: SVGElementInstance with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGViewElement =
    inherit SVGElement
    inherit SVGExternalResourcesRequired
    inherit SVGFitToViewBox
    inherit SVGZoomAndPan
    abstract viewTarget: SVGStringList with get, set
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and SVGZoomAndPan =
    abstract zoomAndPan: float with get, set

and SVGZoomEvent =
    inherit UIEvent
    abstract newScale: float with get, set
    abstract newTranslate: SVGPoint with get, set
    abstract previousScale: float with get, set
    abstract previousTranslate: SVGPoint with get, set
    abstract zoomRectScreen: SVGRect with get, set

and Screen =
    inherit EventTarget
    abstract availHeight: float with get, set
    abstract availWidth: float with get, set
    abstract bufferDepth: float with get, set
    abstract colorDepth: float with get, set
    abstract deviceXDPI: float with get, set
    abstract deviceYDPI: float with get, set
    abstract fontSmoothingEnabled: bool with get, set
    abstract height: float with get, set
    abstract logicalXDPI: float with get, set
    abstract logicalYDPI: float with get, set
    abstract msOrientation: string with get, set
    abstract onmsorientationchange: Func<Event, obj> with get, set
    abstract pixelDepth: float with get, set
    abstract systemXDPI: float with get, set
    abstract systemYDPI: float with get, set
    abstract width: float with get, set
    abstract msLockOrientation: orientations: U2<string, ResizeArray<string>> -> bool
    abstract msUnlockOrientation: unit -> unit
    [<Emit("$0.addEventListener('MSOrientationChange',$1...)")>] abstract addEventListener_MSOrientationChange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and ScriptNotifyEvent =
    inherit Event
    abstract callingUri: string with get, set
    abstract value: string with get, set

and ScriptProcessorNode =
    inherit AudioNode
    abstract bufferSize: float with get, set
    abstract onaudioprocess: Func<AudioProcessingEvent, obj> with get, set
    [<Emit("$0.addEventListener('audioprocess',$1...)")>] abstract addEventListener_audioprocess: listener: Func<AudioProcessingEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and Selection =
    abstract anchorNode: Node with get, set
    abstract anchorOffset: float with get, set
    abstract focusNode: Node with get, set
    abstract focusOffset: float with get, set
    abstract isCollapsed: bool with get, set
    abstract rangeCount: float with get, set
    abstract ``type``: string with get, set
    abstract addRange: range: Range -> unit
    abstract collapse: parentNode: Node * offset: float -> unit
    abstract collapseToEnd: unit -> unit
    abstract collapseToStart: unit -> unit
    abstract containsNode: node: Node * partlyContained: bool -> bool
    abstract deleteFromDocument: unit -> unit
    abstract empty: unit -> unit
    abstract extend: newNode: Node * offset: float -> unit
    abstract getRangeAt: index: float -> Range
    abstract removeAllRanges: unit -> unit
    abstract removeRange: range: Range -> unit
    abstract selectAllChildren: parentNode: Node -> unit
    abstract setBaseAndExtent: baseNode: Node * baseOffset: float * extentNode: Node * extentOffset: float -> unit
    abstract toString: unit -> string

and SourceBuffer =
    inherit EventTarget
    abstract appendWindowEnd: float with get, set
    abstract appendWindowStart: float with get, set
    abstract audioTracks: AudioTrackList with get, set
    abstract buffered: TimeRanges with get, set
    abstract mode: string with get, set
    abstract timestampOffset: float with get, set
    abstract updating: bool with get, set
    abstract videoTracks: VideoTrackList with get, set
    abstract abort: unit -> unit
    abstract appendBuffer: data: U2<ArrayBuffer, ArrayBufferView> -> unit
    abstract appendStream: stream: MSStream * ?maxSize: float -> unit
    abstract remove: start: float * ``end``: float -> unit

and SourceBufferList =
    inherit EventTarget
    abstract length: float with get, set
    abstract item: index: float -> SourceBuffer

and StereoPannerNode =
    inherit AudioNode
    abstract pan: AudioParam with get, set

and Storage =
    abstract length: float with get, set
    abstract clear: unit -> unit
    abstract getItem: key: string -> obj
    abstract key: index: float -> string
    abstract removeItem: key: string -> unit
    abstract setItem: key: string * data: string -> unit

and StorageEvent =
    inherit Event
    abstract url: string with get, set
    abstract key: string option with get, set
    abstract oldValue: string option with get, set
    abstract newValue: string option with get, set
    abstract storageArea: Storage option with get, set

and StyleMedia =
    abstract ``type``: string with get, set
    abstract matchMedium: mediaquery: string -> bool

and StyleSheet =
    abstract disabled: bool with get, set
    abstract href: string with get, set
    abstract media: MediaList with get, set
    abstract ownerNode: Node with get, set
    abstract parentStyleSheet: StyleSheet with get, set
    abstract title: string with get, set
    abstract ``type``: string with get, set

and StyleSheetList =
    abstract length: float with get, set
    abstract item: ?index: float -> StyleSheet

and StyleSheetPageList =
    abstract length: float with get, set
    abstract item: index: float -> CSSPageRule

and SubtleCrypto =
    abstract decrypt: algorithm: U2<string, Algorithm> * key: CryptoKey * data: ArrayBufferView -> obj
    abstract deriveBits: algorithm: U2<string, Algorithm> * baseKey: CryptoKey * length: float -> obj
    abstract deriveKey: algorithm: U2<string, Algorithm> * baseKey: CryptoKey * derivedKeyType: U2<string, Algorithm> * extractable: bool * keyUsages: ResizeArray<string> -> obj
    abstract digest: algorithm: U2<string, Algorithm> * data: ArrayBufferView -> obj
    abstract encrypt: algorithm: U2<string, Algorithm> * key: CryptoKey * data: ArrayBufferView -> obj
    abstract exportKey: format: string * key: CryptoKey -> obj
    abstract generateKey: algorithm: U2<string, Algorithm> * extractable: bool * keyUsages: ResizeArray<string> -> obj
    abstract importKey: format: string * keyData: ArrayBufferView * algorithm: U2<string, Algorithm> * extractable: bool * keyUsages: ResizeArray<string> -> obj
    abstract sign: algorithm: U2<string, Algorithm> * key: CryptoKey * data: ArrayBufferView -> obj
    abstract unwrapKey: format: string * wrappedKey: ArrayBufferView * unwrappingKey: CryptoKey * unwrapAlgorithm: U2<string, Algorithm> * unwrappedKeyAlgorithm: U2<string, Algorithm> * extractable: bool * keyUsages: ResizeArray<string> -> obj
    abstract verify: algorithm: U2<string, Algorithm> * key: CryptoKey * signature: ArrayBufferView * data: ArrayBufferView -> obj
    abstract wrapKey: format: string * key: CryptoKey * wrappingKey: CryptoKey * wrapAlgorithm: U2<string, Algorithm> -> obj

and Text =
    inherit CharacterData
    abstract wholeText: string with get, set
    abstract replaceWholeText: content: string -> Text
    abstract splitText: offset: float -> Text

and TextEvent =
    inherit UIEvent
    abstract data: string with get, set
    abstract inputMethod: float with get, set
    abstract locale: string with get, set
    abstract DOM_INPUT_METHOD_DROP: float with get, set
    abstract DOM_INPUT_METHOD_HANDWRITING: float with get, set
    abstract DOM_INPUT_METHOD_IME: float with get, set
    abstract DOM_INPUT_METHOD_KEYBOARD: float with get, set
    abstract DOM_INPUT_METHOD_MULTIMODAL: float with get, set
    abstract DOM_INPUT_METHOD_OPTION: float with get, set
    abstract DOM_INPUT_METHOD_PASTE: float with get, set
    abstract DOM_INPUT_METHOD_SCRIPT: float with get, set
    abstract DOM_INPUT_METHOD_UNKNOWN: float with get, set
    abstract DOM_INPUT_METHOD_VOICE: float with get, set
    abstract initTextEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * dataArg: string * inputMethod: float * locale: string -> unit

and TextMetrics =
    abstract width: float with get, set

and TextRange =
    abstract boundingHeight: float with get, set
    abstract boundingLeft: float with get, set
    abstract boundingTop: float with get, set
    abstract boundingWidth: float with get, set
    abstract htmlText: string with get, set
    abstract offsetLeft: float with get, set
    abstract offsetTop: float with get, set
    abstract text: string with get, set
    abstract collapse: ?start: bool -> unit
    abstract compareEndPoints: how: string * sourceRange: TextRange -> float
    abstract duplicate: unit -> TextRange
    abstract execCommand: cmdID: string * ?showUI: bool * ?value: obj -> bool
    abstract execCommandShowHelp: cmdID: string -> bool
    abstract expand: Unit: string -> bool
    abstract findText: string: string * ?count: float * ?flags: float -> bool
    abstract getBookmark: unit -> string
    abstract getBoundingClientRect: unit -> ClientRect
    abstract getClientRects: unit -> ClientRectList
    abstract inRange: range: TextRange -> bool
    abstract isEqual: range: TextRange -> bool
    abstract move: unit: string * ?count: float -> float
    abstract moveEnd: unit: string * ?count: float -> float
    abstract moveStart: unit: string * ?count: float -> float
    abstract moveToBookmark: bookmark: string -> bool
    abstract moveToElementText: element: Element -> unit
    abstract moveToPoint: x: float * y: float -> unit
    abstract parentElement: unit -> Element
    abstract pasteHTML: html: string -> unit
    abstract queryCommandEnabled: cmdID: string -> bool
    abstract queryCommandIndeterm: cmdID: string -> bool
    abstract queryCommandState: cmdID: string -> bool
    abstract queryCommandSupported: cmdID: string -> bool
    abstract queryCommandText: cmdID: string -> string
    abstract queryCommandValue: cmdID: string -> obj
    abstract scrollIntoView: ?fStart: bool -> unit
    abstract select: unit -> unit
    abstract setEndPoint: how: string * SourceRange: TextRange -> unit

and TextRangeCollection =
    abstract length: float with get, set
    abstract item: index: float -> TextRange

and TextTrack =
    inherit EventTarget
    abstract activeCues: TextTrackCueList with get, set
    abstract cues: TextTrackCueList with get, set
    abstract inBandMetadataTrackDispatchType: string with get, set
    abstract kind: string with get, set
    abstract label: string with get, set
    abstract language: string with get, set
    abstract mode: obj with get, set
    abstract oncuechange: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract readyState: float with get, set
    abstract DISABLED: float with get, set
    abstract ERROR: float with get, set
    abstract HIDDEN: float with get, set
    abstract LOADED: float with get, set
    abstract LOADING: float with get, set
    abstract NONE: float with get, set
    abstract SHOWING: float with get, set
    abstract addCue: cue: TextTrackCue -> unit
    abstract removeCue: cue: TextTrackCue -> unit
    [<Emit("$0.addEventListener('cuechange',$1...)")>] abstract addEventListener_cuechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and TextTrackCue =
    inherit EventTarget
    abstract endTime: float with get, set
    abstract id: string with get, set
    abstract onenter: Func<Event, obj> with get, set
    abstract onexit: Func<Event, obj> with get, set
    abstract pauseOnExit: bool with get, set
    abstract startTime: float with get, set
    abstract text: string with get, set
    abstract track: TextTrack with get, set
    abstract getCueAsHTML: unit -> DocumentFragment
    [<Emit("$0.addEventListener('enter',$1...)")>] abstract addEventListener_enter: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('exit',$1...)")>] abstract addEventListener_exit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and TextTrackCueList =
    abstract length: float with get, set
    abstract getCueById: id: string -> TextTrackCue
    abstract item: index: float -> TextTrackCue

and TextTrackList =
    inherit EventTarget
    abstract length: float with get, set
    abstract onaddtrack: Func<TrackEvent, obj> with get, set
    abstract item: index: float -> TextTrack
    [<Emit("$0.addEventListener('addtrack',$1...)")>] abstract addEventListener_addtrack: listener: Func<TrackEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and TimeRanges =
    abstract length: float with get, set
    abstract ``end``: index: float -> float
    abstract start: index: float -> float

and Touch =
    abstract clientX: float with get, set
    abstract clientY: float with get, set
    abstract identifier: float with get, set
    abstract pageX: float with get, set
    abstract pageY: float with get, set
    abstract screenX: float with get, set
    abstract screenY: float with get, set
    abstract target: EventTarget with get, set

and TouchEvent =
    inherit UIEvent
    abstract altKey: bool with get, set
    abstract changedTouches: TouchList with get, set
    abstract ctrlKey: bool with get, set
    abstract metaKey: bool with get, set
    abstract shiftKey: bool with get, set
    abstract targetTouches: TouchList with get, set
    abstract touches: TouchList with get, set

and TouchList =
    abstract length: float with get, set
    abstract item: index: float -> Touch

and TrackEvent =
    inherit Event
    abstract track: obj with get, set

and TransitionEvent =
    inherit Event
    abstract elapsedTime: float with get, set
    abstract propertyName: string with get, set
    abstract initTransitionEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * propertyNameArg: string * elapsedTimeArg: float -> unit

and TreeWalker =
    abstract currentNode: Node with get, set
    abstract expandEntityReferences: bool with get, set
    abstract filter: NodeFilter with get, set
    abstract root: Node with get, set
    abstract whatToShow: float with get, set
    abstract firstChild: unit -> Node
    abstract lastChild: unit -> Node
    abstract nextNode: unit -> Node
    abstract nextSibling: unit -> Node
    abstract parentNode: unit -> Node
    abstract previousNode: unit -> Node
    abstract previousSibling: unit -> Node

and UIEvent =
    inherit Event
    abstract detail: float with get, set
    abstract view: Window with get, set
    abstract initUIEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float -> unit

and URL =
    abstract createObjectURL: ``object``: obj * ?options: ObjectURLOptions -> string
    abstract revokeObjectURL: url: string -> unit

and UnviewableContentIdentifiedEvent =
    inherit NavigationEventWithReferrer
    abstract mediaType: string with get, set

and ValidityState =
    abstract badInput: bool with get, set
    abstract customError: bool with get, set
    abstract patternMismatch: bool with get, set
    abstract rangeOverflow: bool with get, set
    abstract rangeUnderflow: bool with get, set
    abstract stepMismatch: bool with get, set
    abstract tooLong: bool with get, set
    abstract typeMismatch: bool with get, set
    abstract valid: bool with get, set
    abstract valueMissing: bool with get, set

and VideoPlaybackQuality =
    abstract corruptedVideoFrames: float with get, set
    abstract creationTime: float with get, set
    abstract droppedVideoFrames: float with get, set
    abstract totalFrameDelay: float with get, set
    abstract totalVideoFrames: float with get, set

and VideoTrack =
    abstract id: string with get, set
    abstract kind: string with get, set
    abstract label: string with get, set
    abstract language: string with get, set
    abstract selected: bool with get, set
    abstract sourceBuffer: SourceBuffer with get, set

and VideoTrackList =
    inherit EventTarget
    abstract length: float with get, set
    abstract onaddtrack: Func<TrackEvent, obj> with get, set
    abstract onchange: Func<Event, obj> with get, set
    abstract onremovetrack: Func<TrackEvent, obj> with get, set
    abstract selectedIndex: float with get, set
    abstract getTrackById: id: string -> VideoTrack
    abstract item: index: float -> VideoTrack
    [<Emit("$0.addEventListener('addtrack',$1...)")>] abstract addEventListener_addtrack: listener: Func<TrackEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('removetrack',$1...)")>] abstract addEventListener_removetrack: listener: Func<TrackEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and WEBGL_compressed_texture_s3tc =
    abstract COMPRESSED_RGBA_S3TC_DXT1_EXT: float with get, set
    abstract COMPRESSED_RGBA_S3TC_DXT3_EXT: float with get, set
    abstract COMPRESSED_RGBA_S3TC_DXT5_EXT: float with get, set
    abstract COMPRESSED_RGB_S3TC_DXT1_EXT: float with get, set

and WEBGL_debug_renderer_info =
    abstract UNMASKED_RENDERER_WEBGL: float with get, set
    abstract UNMASKED_VENDOR_WEBGL: float with get, set

and WEBGL_depth_texture =
    abstract UNSIGNED_INT_24_8_WEBGL: float with get, set

and WaveShaperNode =
    inherit AudioNode
    abstract curve: Float32Array with get, set
    abstract oversample: string with get, set

and WebGLActiveInfo =
    abstract name: string with get, set
    abstract size: float with get, set
    abstract ``type``: float with get, set

and WebGLBuffer =
    inherit WebGLObject

and WebGLContextEvent =
    inherit Event
    abstract statusMessage: string with get, set

and WebGLFramebuffer =
    inherit WebGLObject

and WebGLObject =
    interface end

and WebGLProgram =
    inherit WebGLObject

and WebGLRenderbuffer =
    inherit WebGLObject

and WebGLRenderingContext =
    abstract canvas: HTMLCanvasElement with get, set
    abstract drawingBufferHeight: float with get, set
    abstract drawingBufferWidth: float with get, set
    abstract ACTIVE_ATTRIBUTES: float with get, set
    abstract ACTIVE_TEXTURE: float with get, set
    abstract ACTIVE_UNIFORMS: float with get, set
    abstract ALIASED_LINE_WIDTH_RANGE: float with get, set
    abstract ALIASED_POINT_SIZE_RANGE: float with get, set
    abstract ALPHA: float with get, set
    abstract ALPHA_BITS: float with get, set
    abstract ALWAYS: float with get, set
    abstract ARRAY_BUFFER: float with get, set
    abstract ARRAY_BUFFER_BINDING: float with get, set
    abstract ATTACHED_SHADERS: float with get, set
    abstract BACK: float with get, set
    abstract BLEND: float with get, set
    abstract BLEND_COLOR: float with get, set
    abstract BLEND_DST_ALPHA: float with get, set
    abstract BLEND_DST_RGB: float with get, set
    abstract BLEND_EQUATION: float with get, set
    abstract BLEND_EQUATION_ALPHA: float with get, set
    abstract BLEND_EQUATION_RGB: float with get, set
    abstract BLEND_SRC_ALPHA: float with get, set
    abstract BLEND_SRC_RGB: float with get, set
    abstract BLUE_BITS: float with get, set
    abstract BOOL: float with get, set
    abstract BOOL_VEC2: float with get, set
    abstract BOOL_VEC3: float with get, set
    abstract BOOL_VEC4: float with get, set
    abstract BROWSER_DEFAULT_WEBGL: float with get, set
    abstract BUFFER_SIZE: float with get, set
    abstract BUFFER_USAGE: float with get, set
    abstract BYTE: float with get, set
    abstract CCW: float with get, set
    abstract CLAMP_TO_EDGE: float with get, set
    abstract COLOR_ATTACHMENT0: float with get, set
    abstract COLOR_BUFFER_BIT: float with get, set
    abstract COLOR_CLEAR_VALUE: float with get, set
    abstract COLOR_WRITEMASK: float with get, set
    abstract COMPILE_STATUS: float with get, set
    abstract COMPRESSED_TEXTURE_FORMATS: float with get, set
    abstract CONSTANT_ALPHA: float with get, set
    abstract CONSTANT_COLOR: float with get, set
    abstract CONTEXT_LOST_WEBGL: float with get, set
    abstract CULL_FACE: float with get, set
    abstract CULL_FACE_MODE: float with get, set
    abstract CURRENT_PROGRAM: float with get, set
    abstract CURRENT_VERTEX_ATTRIB: float with get, set
    abstract CW: float with get, set
    abstract DECR: float with get, set
    abstract DECR_WRAP: float with get, set
    abstract DELETE_STATUS: float with get, set
    abstract DEPTH_ATTACHMENT: float with get, set
    abstract DEPTH_BITS: float with get, set
    abstract DEPTH_BUFFER_BIT: float with get, set
    abstract DEPTH_CLEAR_VALUE: float with get, set
    abstract DEPTH_COMPONENT: float with get, set
    abstract DEPTH_COMPONENT16: float with get, set
    abstract DEPTH_FUNC: float with get, set
    abstract DEPTH_RANGE: float with get, set
    abstract DEPTH_STENCIL: float with get, set
    abstract DEPTH_STENCIL_ATTACHMENT: float with get, set
    abstract DEPTH_TEST: float with get, set
    abstract DEPTH_WRITEMASK: float with get, set
    abstract DITHER: float with get, set
    abstract DONT_CARE: float with get, set
    abstract DST_ALPHA: float with get, set
    abstract DST_COLOR: float with get, set
    abstract DYNAMIC_DRAW: float with get, set
    abstract ELEMENT_ARRAY_BUFFER: float with get, set
    abstract ELEMENT_ARRAY_BUFFER_BINDING: float with get, set
    abstract EQUAL: float with get, set
    abstract FASTEST: float with get, set
    abstract FLOAT: float with get, set
    abstract FLOAT_MAT2: float with get, set
    abstract FLOAT_MAT3: float with get, set
    abstract FLOAT_MAT4: float with get, set
    abstract FLOAT_VEC2: float with get, set
    abstract FLOAT_VEC3: float with get, set
    abstract FLOAT_VEC4: float with get, set
    abstract FRAGMENT_SHADER: float with get, set
    abstract FRAMEBUFFER: float with get, set
    abstract FRAMEBUFFER_ATTACHMENT_OBJECT_NAME: float with get, set
    abstract FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE: float with get, set
    abstract FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE: float with get, set
    abstract FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL: float with get, set
    abstract FRAMEBUFFER_BINDING: float with get, set
    abstract FRAMEBUFFER_COMPLETE: float with get, set
    abstract FRAMEBUFFER_INCOMPLETE_ATTACHMENT: float with get, set
    abstract FRAMEBUFFER_INCOMPLETE_DIMENSIONS: float with get, set
    abstract FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: float with get, set
    abstract FRAMEBUFFER_UNSUPPORTED: float with get, set
    abstract FRONT: float with get, set
    abstract FRONT_AND_BACK: float with get, set
    abstract FRONT_FACE: float with get, set
    abstract FUNC_ADD: float with get, set
    abstract FUNC_REVERSE_SUBTRACT: float with get, set
    abstract FUNC_SUBTRACT: float with get, set
    abstract GENERATE_MIPMAP_HINT: float with get, set
    abstract GEQUAL: float with get, set
    abstract GREATER: float with get, set
    abstract GREEN_BITS: float with get, set
    abstract HIGH_FLOAT: float with get, set
    abstract HIGH_INT: float with get, set
    abstract IMPLEMENTATION_COLOR_READ_FORMAT: float with get, set
    abstract IMPLEMENTATION_COLOR_READ_TYPE: float with get, set
    abstract INCR: float with get, set
    abstract INCR_WRAP: float with get, set
    abstract INT: float with get, set
    abstract INT_VEC2: float with get, set
    abstract INT_VEC3: float with get, set
    abstract INT_VEC4: float with get, set
    abstract INVALID_ENUM: float with get, set
    abstract INVALID_FRAMEBUFFER_OPERATION: float with get, set
    abstract INVALID_OPERATION: float with get, set
    abstract INVALID_VALUE: float with get, set
    abstract INVERT: float with get, set
    abstract KEEP: float with get, set
    abstract LEQUAL: float with get, set
    abstract LESS: float with get, set
    abstract LINEAR: float with get, set
    abstract LINEAR_MIPMAP_LINEAR: float with get, set
    abstract LINEAR_MIPMAP_NEAREST: float with get, set
    abstract LINES: float with get, set
    abstract LINE_LOOP: float with get, set
    abstract LINE_STRIP: float with get, set
    abstract LINE_WIDTH: float with get, set
    abstract LINK_STATUS: float with get, set
    abstract LOW_FLOAT: float with get, set
    abstract LOW_INT: float with get, set
    abstract LUMINANCE: float with get, set
    abstract LUMINANCE_ALPHA: float with get, set
    abstract MAX_COMBINED_TEXTURE_IMAGE_UNITS: float with get, set
    abstract MAX_CUBE_MAP_TEXTURE_SIZE: float with get, set
    abstract MAX_FRAGMENT_UNIFORM_VECTORS: float with get, set
    abstract MAX_RENDERBUFFER_SIZE: float with get, set
    abstract MAX_TEXTURE_IMAGE_UNITS: float with get, set
    abstract MAX_TEXTURE_SIZE: float with get, set
    abstract MAX_VARYING_VECTORS: float with get, set
    abstract MAX_VERTEX_ATTRIBS: float with get, set
    abstract MAX_VERTEX_TEXTURE_IMAGE_UNITS: float with get, set
    abstract MAX_VERTEX_UNIFORM_VECTORS: float with get, set
    abstract MAX_VIEWPORT_DIMS: float with get, set
    abstract MEDIUM_FLOAT: float with get, set
    abstract MEDIUM_INT: float with get, set
    abstract MIRRORED_REPEAT: float with get, set
    abstract NEAREST: float with get, set
    abstract NEAREST_MIPMAP_LINEAR: float with get, set
    abstract NEAREST_MIPMAP_NEAREST: float with get, set
    abstract NEVER: float with get, set
    abstract NICEST: float with get, set
    abstract NONE: float with get, set
    abstract NOTEQUAL: float with get, set
    abstract NO_ERROR: float with get, set
    abstract ONE: float with get, set
    abstract ONE_MINUS_CONSTANT_ALPHA: float with get, set
    abstract ONE_MINUS_CONSTANT_COLOR: float with get, set
    abstract ONE_MINUS_DST_ALPHA: float with get, set
    abstract ONE_MINUS_DST_COLOR: float with get, set
    abstract ONE_MINUS_SRC_ALPHA: float with get, set
    abstract ONE_MINUS_SRC_COLOR: float with get, set
    abstract OUT_OF_MEMORY: float with get, set
    abstract PACK_ALIGNMENT: float with get, set
    abstract POINTS: float with get, set
    abstract POLYGON_OFFSET_FACTOR: float with get, set
    abstract POLYGON_OFFSET_FILL: float with get, set
    abstract POLYGON_OFFSET_UNITS: float with get, set
    abstract RED_BITS: float with get, set
    abstract RENDERBUFFER: float with get, set
    abstract RENDERBUFFER_ALPHA_SIZE: float with get, set
    abstract RENDERBUFFER_BINDING: float with get, set
    abstract RENDERBUFFER_BLUE_SIZE: float with get, set
    abstract RENDERBUFFER_DEPTH_SIZE: float with get, set
    abstract RENDERBUFFER_GREEN_SIZE: float with get, set
    abstract RENDERBUFFER_HEIGHT: float with get, set
    abstract RENDERBUFFER_INTERNAL_FORMAT: float with get, set
    abstract RENDERBUFFER_RED_SIZE: float with get, set
    abstract RENDERBUFFER_STENCIL_SIZE: float with get, set
    abstract RENDERBUFFER_WIDTH: float with get, set
    abstract RENDERER: float with get, set
    abstract REPEAT: float with get, set
    abstract REPLACE: float with get, set
    abstract RGB: float with get, set
    abstract RGB565: float with get, set
    abstract RGB5_A1: float with get, set
    abstract RGBA: float with get, set
    abstract RGBA4: float with get, set
    abstract SAMPLER_2D: float with get, set
    abstract SAMPLER_CUBE: float with get, set
    abstract SAMPLES: float with get, set
    abstract SAMPLE_ALPHA_TO_COVERAGE: float with get, set
    abstract SAMPLE_BUFFERS: float with get, set
    abstract SAMPLE_COVERAGE: float with get, set
    abstract SAMPLE_COVERAGE_INVERT: float with get, set
    abstract SAMPLE_COVERAGE_VALUE: float with get, set
    abstract SCISSOR_BOX: float with get, set
    abstract SCISSOR_TEST: float with get, set
    abstract SHADER_TYPE: float with get, set
    abstract SHADING_LANGUAGE_VERSION: float with get, set
    abstract SHORT: float with get, set
    abstract SRC_ALPHA: float with get, set
    abstract SRC_ALPHA_SATURATE: float with get, set
    abstract SRC_COLOR: float with get, set
    abstract STATIC_DRAW: float with get, set
    abstract STENCIL_ATTACHMENT: float with get, set
    abstract STENCIL_BACK_FAIL: float with get, set
    abstract STENCIL_BACK_FUNC: float with get, set
    abstract STENCIL_BACK_PASS_DEPTH_FAIL: float with get, set
    abstract STENCIL_BACK_PASS_DEPTH_PASS: float with get, set
    abstract STENCIL_BACK_REF: float with get, set
    abstract STENCIL_BACK_VALUE_MASK: float with get, set
    abstract STENCIL_BACK_WRITEMASK: float with get, set
    abstract STENCIL_BITS: float with get, set
    abstract STENCIL_BUFFER_BIT: float with get, set
    abstract STENCIL_CLEAR_VALUE: float with get, set
    abstract STENCIL_FAIL: float with get, set
    abstract STENCIL_FUNC: float with get, set
    abstract STENCIL_INDEX: float with get, set
    abstract STENCIL_INDEX8: float with get, set
    abstract STENCIL_PASS_DEPTH_FAIL: float with get, set
    abstract STENCIL_PASS_DEPTH_PASS: float with get, set
    abstract STENCIL_REF: float with get, set
    abstract STENCIL_TEST: float with get, set
    abstract STENCIL_VALUE_MASK: float with get, set
    abstract STENCIL_WRITEMASK: float with get, set
    abstract STREAM_DRAW: float with get, set
    abstract SUBPIXEL_BITS: float with get, set
    abstract TEXTURE: float with get, set
    abstract TEXTURE0: float with get, set
    abstract TEXTURE1: float with get, set
    abstract TEXTURE10: float with get, set
    abstract TEXTURE11: float with get, set
    abstract TEXTURE12: float with get, set
    abstract TEXTURE13: float with get, set
    abstract TEXTURE14: float with get, set
    abstract TEXTURE15: float with get, set
    abstract TEXTURE16: float with get, set
    abstract TEXTURE17: float with get, set
    abstract TEXTURE18: float with get, set
    abstract TEXTURE19: float with get, set
    abstract TEXTURE2: float with get, set
    abstract TEXTURE20: float with get, set
    abstract TEXTURE21: float with get, set
    abstract TEXTURE22: float with get, set
    abstract TEXTURE23: float with get, set
    abstract TEXTURE24: float with get, set
    abstract TEXTURE25: float with get, set
    abstract TEXTURE26: float with get, set
    abstract TEXTURE27: float with get, set
    abstract TEXTURE28: float with get, set
    abstract TEXTURE29: float with get, set
    abstract TEXTURE3: float with get, set
    abstract TEXTURE30: float with get, set
    abstract TEXTURE31: float with get, set
    abstract TEXTURE4: float with get, set
    abstract TEXTURE5: float with get, set
    abstract TEXTURE6: float with get, set
    abstract TEXTURE7: float with get, set
    abstract TEXTURE8: float with get, set
    abstract TEXTURE9: float with get, set
    abstract TEXTURE_2D: float with get, set
    abstract TEXTURE_BINDING_2D: float with get, set
    abstract TEXTURE_BINDING_CUBE_MAP: float with get, set
    abstract TEXTURE_CUBE_MAP: float with get, set
    abstract TEXTURE_CUBE_MAP_NEGATIVE_X: float with get, set
    abstract TEXTURE_CUBE_MAP_NEGATIVE_Y: float with get, set
    abstract TEXTURE_CUBE_MAP_NEGATIVE_Z: float with get, set
    abstract TEXTURE_CUBE_MAP_POSITIVE_X: float with get, set
    abstract TEXTURE_CUBE_MAP_POSITIVE_Y: float with get, set
    abstract TEXTURE_CUBE_MAP_POSITIVE_Z: float with get, set
    abstract TEXTURE_MAG_FILTER: float with get, set
    abstract TEXTURE_MIN_FILTER: float with get, set
    abstract TEXTURE_WRAP_S: float with get, set
    abstract TEXTURE_WRAP_T: float with get, set
    abstract TRIANGLES: float with get, set
    abstract TRIANGLE_FAN: float with get, set
    abstract TRIANGLE_STRIP: float with get, set
    abstract UNPACK_ALIGNMENT: float with get, set
    abstract UNPACK_COLORSPACE_CONVERSION_WEBGL: float with get, set
    abstract UNPACK_FLIP_Y_WEBGL: float with get, set
    abstract UNPACK_PREMULTIPLY_ALPHA_WEBGL: float with get, set
    abstract UNSIGNED_BYTE: float with get, set
    abstract UNSIGNED_INT: float with get, set
    abstract UNSIGNED_SHORT: float with get, set
    abstract UNSIGNED_SHORT_4_4_4_4: float with get, set
    abstract UNSIGNED_SHORT_5_5_5_1: float with get, set
    abstract UNSIGNED_SHORT_5_6_5: float with get, set
    abstract VALIDATE_STATUS: float with get, set
    abstract VENDOR: float with get, set
    abstract VERSION: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_BUFFER_BINDING: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_ENABLED: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_NORMALIZED: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_POINTER: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_SIZE: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_STRIDE: float with get, set
    abstract VERTEX_ATTRIB_ARRAY_TYPE: float with get, set
    abstract VERTEX_SHADER: float with get, set
    abstract VIEWPORT: float with get, set
    abstract ZERO: float with get, set
    abstract activeTexture: texture: float -> unit
    abstract attachShader: program: WebGLProgram * shader: WebGLShader -> unit
    abstract bindAttribLocation: program: WebGLProgram * index: float * name: string -> unit
    abstract bindBuffer: target: float * buffer: WebGLBuffer -> unit
    abstract bindFramebuffer: target: float * framebuffer: WebGLFramebuffer -> unit
    abstract bindRenderbuffer: target: float * renderbuffer: WebGLRenderbuffer -> unit
    abstract bindTexture: target: float * texture: WebGLTexture -> unit
    abstract blendColor: red: float * green: float * blue: float * alpha: float -> unit
    abstract blendEquation: mode: float -> unit
    abstract blendEquationSeparate: modeRGB: float * modeAlpha: float -> unit
    abstract blendFunc: sfactor: float * dfactor: float -> unit
    abstract blendFuncSeparate: srcRGB: float * dstRGB: float * srcAlpha: float * dstAlpha: float -> unit
    abstract bufferData: target: float * size: U3<float, ArrayBufferView, ArrayBuffer> * usage: float -> unit
    abstract bufferSubData: target: float * offset: float * data: U2<ArrayBufferView, ArrayBuffer> -> unit
    abstract checkFramebufferStatus: target: float -> float
    abstract clear: mask: float -> unit
    abstract clearColor: red: float * green: float * blue: float * alpha: float -> unit
    abstract clearDepth: depth: float -> unit
    abstract clearStencil: s: float -> unit
    abstract colorMask: red: bool * green: bool * blue: bool * alpha: bool -> unit
    abstract compileShader: shader: WebGLShader -> unit
    abstract compressedTexImage2D: target: float * level: float * internalformat: float * width: float * height: float * border: float * data: ArrayBufferView -> unit
    abstract compressedTexSubImage2D: target: float * level: float * xoffset: float * yoffset: float * width: float * height: float * format: float * data: ArrayBufferView -> unit
    abstract copyTexImage2D: target: float * level: float * internalformat: float * x: float * y: float * width: float * height: float * border: float -> unit
    abstract copyTexSubImage2D: target: float * level: float * xoffset: float * yoffset: float * x: float * y: float * width: float * height: float -> unit
    abstract createBuffer: unit -> WebGLBuffer
    abstract createFramebuffer: unit -> WebGLFramebuffer
    abstract createProgram: unit -> WebGLProgram
    abstract createRenderbuffer: unit -> WebGLRenderbuffer
    abstract createShader: ``type``: float -> WebGLShader
    abstract createTexture: unit -> WebGLTexture
    abstract cullFace: mode: float -> unit
    abstract deleteBuffer: buffer: WebGLBuffer -> unit
    abstract deleteFramebuffer: framebuffer: WebGLFramebuffer -> unit
    abstract deleteProgram: program: WebGLProgram -> unit
    abstract deleteRenderbuffer: renderbuffer: WebGLRenderbuffer -> unit
    abstract deleteShader: shader: WebGLShader -> unit
    abstract deleteTexture: texture: WebGLTexture -> unit
    abstract depthFunc: func: float -> unit
    abstract depthMask: flag: bool -> unit
    abstract depthRange: zNear: float * zFar: float -> unit
    abstract detachShader: program: WebGLProgram * shader: WebGLShader -> unit
    abstract disable: cap: float -> unit
    abstract disableVertexAttribArray: index: float -> unit
    abstract drawArrays: mode: float * first: float * count: float -> unit
    abstract drawElements: mode: float * count: float * ``type``: float * offset: float -> unit
    abstract enable: cap: float -> unit
    abstract enableVertexAttribArray: index: float -> unit
    abstract finish: unit -> unit
    abstract flush: unit -> unit
    abstract framebufferRenderbuffer: target: float * attachment: float * renderbuffertarget: float * renderbuffer: WebGLRenderbuffer -> unit
    abstract framebufferTexture2D: target: float * attachment: float * textarget: float * texture: WebGLTexture * level: float -> unit
    abstract frontFace: mode: float -> unit
    abstract generateMipmap: target: float -> unit
    abstract getActiveAttrib: program: WebGLProgram * index: float -> WebGLActiveInfo
    abstract getActiveUniform: program: WebGLProgram * index: float -> WebGLActiveInfo
    abstract getAttachedShaders: program: WebGLProgram -> ResizeArray<WebGLShader>
    abstract getAttribLocation: program: WebGLProgram * name: string -> float
    abstract getBufferParameter: target: float * pname: float -> obj
    abstract getContextAttributes: unit -> WebGLContextAttributes
    abstract getError: unit -> float
    abstract getExtension: name: string -> obj
    abstract getFramebufferAttachmentParameter: target: float * attachment: float * pname: float -> obj
    abstract getParameter: pname: float -> obj
    abstract getProgramInfoLog: program: WebGLProgram -> string
    abstract getProgramParameter: program: WebGLProgram * pname: float -> obj
    abstract getRenderbufferParameter: target: float * pname: float -> obj
    abstract getShaderInfoLog: shader: WebGLShader -> string
    abstract getShaderParameter: shader: WebGLShader * pname: float -> obj
    abstract getShaderPrecisionFormat: shadertype: float * precisiontype: float -> WebGLShaderPrecisionFormat
    abstract getShaderSource: shader: WebGLShader -> string
    abstract getSupportedExtensions: unit -> ResizeArray<string>
    abstract getTexParameter: target: float * pname: float -> obj
    abstract getUniform: program: WebGLProgram * location: WebGLUniformLocation -> obj
    abstract getUniformLocation: program: WebGLProgram * name: string -> WebGLUniformLocation
    abstract getVertexAttrib: index: float * pname: float -> obj
    abstract getVertexAttribOffset: index: float * pname: float -> float
    abstract hint: target: float * mode: float -> unit
    abstract isBuffer: buffer: WebGLBuffer -> bool
    abstract isContextLost: unit -> bool
    abstract isEnabled: cap: float -> bool
    abstract isFramebuffer: framebuffer: WebGLFramebuffer -> bool
    abstract isProgram: program: WebGLProgram -> bool
    abstract isRenderbuffer: renderbuffer: WebGLRenderbuffer -> bool
    abstract isShader: shader: WebGLShader -> bool
    abstract isTexture: texture: WebGLTexture -> bool
    abstract lineWidth: width: float -> unit
    abstract linkProgram: program: WebGLProgram -> unit
    abstract pixelStorei: pname: float * param: float -> unit
    abstract polygonOffset: factor: float * units: float -> unit
    abstract readPixels: x: float * y: float * width: float * height: float * format: float * ``type``: float * pixels: ArrayBufferView -> unit
    abstract renderbufferStorage: target: float * internalformat: float * width: float * height: float -> unit
    abstract sampleCoverage: value: float * invert: bool -> unit
    abstract scissor: x: float * y: float * width: float * height: float -> unit
    abstract shaderSource: shader: WebGLShader * source: string -> unit
    abstract stencilFunc: func: float * ref: float * mask: float -> unit
    abstract stencilFuncSeparate: face: float * func: float * ref: float * mask: float -> unit
    abstract stencilMask: mask: float -> unit
    abstract stencilMaskSeparate: face: float * mask: float -> unit
    abstract stencilOp: fail: float * zfail: float * zpass: float -> unit
    abstract stencilOpSeparate: face: float * fail: float * zfail: float * zpass: float -> unit
    abstract texImage2D: target: float * level: float * internalformat: float * width: float * height: float * border: float * format: float * ``type``: float * pixels: ArrayBufferView -> unit
    abstract texImage2D: target: float * level: float * internalformat: float * format: float * ``type``: float * image: HTMLImageElement -> unit
    abstract texImage2D: target: float * level: float * internalformat: float * format: float * ``type``: float * canvas: HTMLCanvasElement -> unit
    abstract texImage2D: target: float * level: float * internalformat: float * format: float * ``type``: float * video: HTMLVideoElement -> unit
    abstract texImage2D: target: float * level: float * internalformat: float * format: float * ``type``: float * pixels: ImageData -> unit
    abstract texParameterf: target: float * pname: float * param: float -> unit
    abstract texParameteri: target: float * pname: float * param: float -> unit
    abstract texSubImage2D: target: float * level: float * xoffset: float * yoffset: float * width: float * height: float * format: float * ``type``: float * pixels: ArrayBufferView -> unit
    abstract texSubImage2D: target: float * level: float * xoffset: float * yoffset: float * format: float * ``type``: float * image: HTMLImageElement -> unit
    abstract texSubImage2D: target: float * level: float * xoffset: float * yoffset: float * format: float * ``type``: float * canvas: HTMLCanvasElement -> unit
    abstract texSubImage2D: target: float * level: float * xoffset: float * yoffset: float * format: float * ``type``: float * video: HTMLVideoElement -> unit
    abstract texSubImage2D: target: float * level: float * xoffset: float * yoffset: float * format: float * ``type``: float * pixels: ImageData -> unit
    abstract uniform1f: location: WebGLUniformLocation * x: float -> unit
    abstract uniform1fv: location: WebGLUniformLocation * v: Float32Array -> unit
    abstract uniform1i: location: WebGLUniformLocation * x: float -> unit
    abstract uniform1iv: location: WebGLUniformLocation * v: Int32Array -> unit
    abstract uniform2f: location: WebGLUniformLocation * x: float * y: float -> unit
    abstract uniform2fv: location: WebGLUniformLocation * v: Float32Array -> unit
    abstract uniform2i: location: WebGLUniformLocation * x: float * y: float -> unit
    abstract uniform2iv: location: WebGLUniformLocation * v: Int32Array -> unit
    abstract uniform3f: location: WebGLUniformLocation * x: float * y: float * z: float -> unit
    abstract uniform3fv: location: WebGLUniformLocation * v: Float32Array -> unit
    abstract uniform3i: location: WebGLUniformLocation * x: float * y: float * z: float -> unit
    abstract uniform3iv: location: WebGLUniformLocation * v: Int32Array -> unit
    abstract uniform4f: location: WebGLUniformLocation * x: float * y: float * z: float * w: float -> unit
    abstract uniform4fv: location: WebGLUniformLocation * v: Float32Array -> unit
    abstract uniform4i: location: WebGLUniformLocation * x: float * y: float * z: float * w: float -> unit
    abstract uniform4iv: location: WebGLUniformLocation * v: Int32Array -> unit
    abstract uniformMatrix2fv: location: WebGLUniformLocation * transpose: bool * value: Float32Array -> unit
    abstract uniformMatrix3fv: location: WebGLUniformLocation * transpose: bool * value: Float32Array -> unit
    abstract uniformMatrix4fv: location: WebGLUniformLocation * transpose: bool * value: Float32Array -> unit
    abstract useProgram: program: WebGLProgram -> unit
    abstract validateProgram: program: WebGLProgram -> unit
    abstract vertexAttrib1f: indx: float * x: float -> unit
    abstract vertexAttrib1fv: indx: float * values: Float32Array -> unit
    abstract vertexAttrib2f: indx: float * x: float * y: float -> unit
    abstract vertexAttrib2fv: indx: float * values: Float32Array -> unit
    abstract vertexAttrib3f: indx: float * x: float * y: float * z: float -> unit
    abstract vertexAttrib3fv: indx: float * values: Float32Array -> unit
    abstract vertexAttrib4f: indx: float * x: float * y: float * z: float * w: float -> unit
    abstract vertexAttrib4fv: indx: float * values: Float32Array -> unit
    abstract vertexAttribPointer: indx: float * size: float * ``type``: float * normalized: bool * stride: float * offset: float -> unit
    abstract viewport: x: float * y: float * width: float * height: float -> unit

and WebGLShader =
    inherit WebGLObject

and WebGLShaderPrecisionFormat =
    abstract precision: float with get, set
    abstract rangeMax: float with get, set
    abstract rangeMin: float with get, set

and WebGLTexture =
    inherit WebGLObject

and WebGLUniformLocation =
    interface end

and WebKitCSSMatrix =
    abstract a: float with get, set
    abstract b: float with get, set
    abstract c: float with get, set
    abstract d: float with get, set
    abstract e: float with get, set
    abstract f: float with get, set
    abstract m11: float with get, set
    abstract m12: float with get, set
    abstract m13: float with get, set
    abstract m14: float with get, set
    abstract m21: float with get, set
    abstract m22: float with get, set
    abstract m23: float with get, set
    abstract m24: float with get, set
    abstract m31: float with get, set
    abstract m32: float with get, set
    abstract m33: float with get, set
    abstract m34: float with get, set
    abstract m41: float with get, set
    abstract m42: float with get, set
    abstract m43: float with get, set
    abstract m44: float with get, set
    abstract inverse: unit -> WebKitCSSMatrix
    abstract multiply: secondMatrix: WebKitCSSMatrix -> WebKitCSSMatrix
    abstract rotate: angleX: float * ?angleY: float * ?angleZ: float -> WebKitCSSMatrix
    abstract rotateAxisAngle: x: float * y: float * z: float * angle: float -> WebKitCSSMatrix
    abstract scale: scaleX: float * ?scaleY: float * ?scaleZ: float -> WebKitCSSMatrix
    abstract setMatrixValue: value: string -> unit
    abstract skewX: angle: float -> WebKitCSSMatrix
    abstract skewY: angle: float -> WebKitCSSMatrix
    abstract toString: unit -> string
    abstract translate: x: float * y: float * ?z: float -> WebKitCSSMatrix

and WebKitPoint =
    abstract x: float with get, set
    abstract y: float with get, set

and WebSocket =
    inherit EventTarget
    abstract binaryType: string with get, set
    abstract bufferedAmount: float with get, set
    abstract extensions: string with get, set
    abstract onclose: Func<CloseEvent, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onmessage: Func<MessageEvent, obj> with get, set
    abstract onopen: Func<Event, obj> with get, set
    abstract protocol: string with get, set
    abstract readyState: float with get, set
    abstract url: string with get, set
    abstract CLOSED: float with get, set
    abstract CLOSING: float with get, set
    abstract CONNECTING: float with get, set
    abstract OPEN: float with get, set
    abstract close: ?code: float * ?reason: string -> unit
    abstract send: data: obj -> unit
    [<Emit("$0.addEventListener('close',$1...)")>] abstract addEventListener_close: listener: Func<CloseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('message',$1...)")>] abstract addEventListener_message: listener: Func<MessageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('open',$1...)")>] abstract addEventListener_open: listener: Func<Event, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and WheelEvent =
    inherit MouseEvent
    abstract deltaMode: float with get, set
    abstract deltaX: float with get, set
    abstract deltaY: float with get, set
    abstract deltaZ: float with get, set
    abstract DOM_DELTA_LINE: float with get, set
    abstract DOM_DELTA_PAGE: float with get, set
    abstract DOM_DELTA_PIXEL: float with get, set
    abstract getCurrentPoint: element: Element -> unit
    abstract initWheelEvent: typeArg: string * canBubbleArg: bool * cancelableArg: bool * viewArg: Window * detailArg: float * screenXArg: float * screenYArg: float * clientXArg: float * clientYArg: float * buttonArg: float * relatedTargetArg: EventTarget * modifiersListArg: string * deltaXArg: float * deltaYArg: float * deltaZArg: float * deltaMode: float -> unit

and Window =
    inherit EventTarget
    inherit WindowTimers
    inherit WindowSessionStorage
    inherit WindowLocalStorage
    inherit WindowConsole
    inherit GlobalEventHandlers
    inherit IDBEnvironment
    inherit WindowBase64
    abstract animationStartTime: float with get, set
    abstract applicationCache: ApplicationCache with get, set
    abstract clientInformation: Navigator with get, set
    abstract closed: bool with get, set
    abstract crypto: Crypto with get, set
    abstract defaultStatus: string with get, set
    abstract devicePixelRatio: float with get, set
    abstract doNotTrack: string with get, set
    abstract document: Document with get, set
    abstract ``event``: Event with get, set
    abstract ``external``: External with get, set
    abstract frameElement: Element with get, set
    abstract frames: Window with get, set
    abstract history: History with get, set
    abstract innerHeight: float with get, set
    abstract innerWidth: float with get, set
    abstract length: float with get, set
    abstract location: Location with get, set
    abstract locationbar: BarProp with get, set
    abstract menubar: BarProp with get, set
    abstract msAnimationStartTime: float with get, set
    abstract name: string with get, set
    abstract navigator: Navigator with get, set
    abstract offscreenBuffering: U2<string, bool> with get, set
    abstract onabort: Func<Event, obj> with get, set
    abstract onafterprint: Func<Event, obj> with get, set
    abstract onbeforeprint: Func<Event, obj> with get, set
    abstract onbeforeunload: Func<BeforeUnloadEvent, obj> with get, set
    abstract onblur: Func<FocusEvent, obj> with get, set
    abstract oncanplay: Func<Event, obj> with get, set
    abstract oncanplaythrough: Func<Event, obj> with get, set
    abstract onchange: Func<Event, obj> with get, set
    abstract onclick: Func<MouseEvent, obj> with get, set
    abstract oncompassneedscalibration: Func<Event, obj> with get, set
    abstract oncontextmenu: Func<PointerEvent, obj> with get, set
    abstract ondblclick: Func<MouseEvent, obj> with get, set
    abstract ondevicemotion: Func<DeviceMotionEvent, obj> with get, set
    abstract ondeviceorientation: Func<DeviceOrientationEvent, obj> with get, set
    abstract ondrag: Func<DragEvent, obj> with get, set
    abstract ondragend: Func<DragEvent, obj> with get, set
    abstract ondragenter: Func<DragEvent, obj> with get, set
    abstract ondragleave: Func<DragEvent, obj> with get, set
    abstract ondragover: Func<DragEvent, obj> with get, set
    abstract ondragstart: Func<DragEvent, obj> with get, set
    abstract ondrop: Func<DragEvent, obj> with get, set
    abstract ondurationchange: Func<Event, obj> with get, set
    abstract onemptied: Func<Event, obj> with get, set
    abstract onended: Func<Event, obj> with get, set
    abstract onerror: ErrorEventHandler with get, set
    abstract onfocus: Func<FocusEvent, obj> with get, set
    abstract onhashchange: Func<HashChangeEvent, obj> with get, set
    abstract oninput: Func<Event, obj> with get, set
    abstract onkeydown: Func<KeyboardEvent, obj> with get, set
    abstract onkeypress: Func<KeyboardEvent, obj> with get, set
    abstract onkeyup: Func<KeyboardEvent, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onloadeddata: Func<Event, obj> with get, set
    abstract onloadedmetadata: Func<Event, obj> with get, set
    abstract onloadstart: Func<Event, obj> with get, set
    abstract onmessage: Func<MessageEvent, obj> with get, set
    abstract onmousedown: Func<MouseEvent, obj> with get, set
    abstract onmouseenter: Func<MouseEvent, obj> with get, set
    abstract onmouseleave: Func<MouseEvent, obj> with get, set
    abstract onmousemove: Func<MouseEvent, obj> with get, set
    abstract onmouseout: Func<MouseEvent, obj> with get, set
    abstract onmouseover: Func<MouseEvent, obj> with get, set
    abstract onmouseup: Func<MouseEvent, obj> with get, set
    abstract onmousewheel: Func<MouseWheelEvent, obj> with get, set
    abstract onmsgesturechange: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturedoubletap: Func<MSGestureEvent, obj> with get, set
    abstract onmsgestureend: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturehold: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturestart: Func<MSGestureEvent, obj> with get, set
    abstract onmsgesturetap: Func<MSGestureEvent, obj> with get, set
    abstract onmsinertiastart: Func<MSGestureEvent, obj> with get, set
    abstract onmspointercancel: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerdown: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerenter: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerleave: Func<MSPointerEvent, obj> with get, set
    abstract onmspointermove: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerout: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerover: Func<MSPointerEvent, obj> with get, set
    abstract onmspointerup: Func<MSPointerEvent, obj> with get, set
    abstract onoffline: Func<Event, obj> with get, set
    abstract ononline: Func<Event, obj> with get, set
    abstract onorientationchange: Func<Event, obj> with get, set
    abstract onpagehide: Func<PageTransitionEvent, obj> with get, set
    abstract onpageshow: Func<PageTransitionEvent, obj> with get, set
    abstract onpause: Func<Event, obj> with get, set
    abstract onplay: Func<Event, obj> with get, set
    abstract onplaying: Func<Event, obj> with get, set
    abstract onpopstate: Func<PopStateEvent, obj> with get, set
    abstract onprogress: Func<ProgressEvent, obj> with get, set
    abstract onratechange: Func<Event, obj> with get, set
    abstract onreadystatechange: Func<ProgressEvent, obj> with get, set
    abstract onreset: Func<Event, obj> with get, set
    abstract onresize: Func<UIEvent, obj> with get, set
    abstract onscroll: Func<UIEvent, obj> with get, set
    abstract onseeked: Func<Event, obj> with get, set
    abstract onseeking: Func<Event, obj> with get, set
    abstract onselect: Func<UIEvent, obj> with get, set
    abstract onstalled: Func<Event, obj> with get, set
    abstract onstorage: Func<StorageEvent, obj> with get, set
    abstract onsubmit: Func<Event, obj> with get, set
    abstract onsuspend: Func<Event, obj> with get, set
    abstract ontimeupdate: Func<Event, obj> with get, set
    abstract ontouchcancel: obj with get, set
    abstract ontouchend: obj with get, set
    abstract ontouchmove: obj with get, set
    abstract ontouchstart: obj with get, set
    abstract onunload: Func<Event, obj> with get, set
    abstract onvolumechange: Func<Event, obj> with get, set
    abstract onwaiting: Func<Event, obj> with get, set
    abstract opener: Window with get, set
    abstract orientation: U2<string, float> with get, set
    abstract outerHeight: float with get, set
    abstract outerWidth: float with get, set
    abstract pageXOffset: float with get, set
    abstract pageYOffset: float with get, set
    abstract parent: Window with get, set
    abstract performance: Performance with get, set
    abstract personalbar: BarProp with get, set
    abstract screen: Screen with get, set
    abstract screenLeft: float with get, set
    abstract screenTop: float with get, set
    abstract screenX: float with get, set
    abstract screenY: float with get, set
    abstract scrollX: float with get, set
    abstract scrollY: float with get, set
    abstract scrollbars: BarProp with get, set
    abstract self: Window with get, set
    abstract status: string with get, set
    abstract statusbar: BarProp with get, set
    abstract styleMedia: StyleMedia with get, set
    abstract toolbar: BarProp with get, set
    abstract top: Window with get, set
    abstract window: Window with get, set
    abstract URL: URL with get, set
    abstract alert: ?message: obj -> unit
    abstract blur: unit -> unit
    abstract cancelAnimationFrame: handle: float -> unit
    abstract captureEvents: unit -> unit
    abstract close: unit -> unit
    abstract confirm: ?message: string -> bool
    abstract focus: unit -> unit
    abstract getComputedStyle: elt: Element * ?pseudoElt: string -> CSSStyleDeclaration
    abstract getMatchedCSSRules: elt: Element * ?pseudoElt: string -> CSSRuleList
    abstract getSelection: unit -> Selection
    abstract matchMedia: mediaQuery: string -> MediaQueryList
    abstract moveBy: ?x: float * ?y: float -> unit
    abstract moveTo: ?x: float * ?y: float -> unit
    abstract msCancelRequestAnimationFrame: handle: float -> unit
    abstract msMatchMedia: mediaQuery: string -> MediaQueryList
    abstract msRequestAnimationFrame: callback: FrameRequestCallback -> float
    abstract msWriteProfilerMark: profilerMarkName: string -> unit
    abstract ``open``: ?url: string * ?target: string * ?features: string * ?replace: bool -> Window
    abstract postMessage: message: obj * targetOrigin: string * ?ports: obj -> unit
    abstract print: unit -> unit
    abstract prompt: ?message: string * ?_default: string -> string
    abstract releaseEvents: unit -> unit
    abstract requestAnimationFrame: callback: FrameRequestCallback -> float
    abstract resizeBy: ?x: float * ?y: float -> unit
    abstract resizeTo: ?x: float * ?y: float -> unit
    abstract scroll: ?x: float * ?y: float -> unit
    abstract scrollBy: ?x: float * ?y: float -> unit
    abstract scrollTo: ?x: float * ?y: float -> unit
    abstract webkitConvertPointFromNodeToPage: node: Node * pt: WebKitPoint -> WebKitPoint
    abstract webkitConvertPointFromPageToNode: node: Node * pt: WebKitPoint -> WebKitPoint
    [<Emit("$0.addEventListener('MSGestureChange',$1...)")>] abstract addEventListener_MSGestureChange: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureDoubleTap',$1...)")>] abstract addEventListener_MSGestureDoubleTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureEnd',$1...)")>] abstract addEventListener_MSGestureEnd: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureHold',$1...)")>] abstract addEventListener_MSGestureHold: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureStart',$1...)")>] abstract addEventListener_MSGestureStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSGestureTap',$1...)")>] abstract addEventListener_MSGestureTap: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSInertiaStart',$1...)")>] abstract addEventListener_MSInertiaStart: listener: Func<MSGestureEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerCancel',$1...)")>] abstract addEventListener_MSPointerCancel: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerDown',$1...)")>] abstract addEventListener_MSPointerDown: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerEnter',$1...)")>] abstract addEventListener_MSPointerEnter: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerLeave',$1...)")>] abstract addEventListener_MSPointerLeave: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerMove',$1...)")>] abstract addEventListener_MSPointerMove: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOut',$1...)")>] abstract addEventListener_MSPointerOut: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerOver',$1...)")>] abstract addEventListener_MSPointerOver: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('MSPointerUp',$1...)")>] abstract addEventListener_MSPointerUp: listener: Func<MSPointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('afterprint',$1...)")>] abstract addEventListener_afterprint: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeprint',$1...)")>] abstract addEventListener_beforeprint: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('beforeunload',$1...)")>] abstract addEventListener_beforeunload: listener: Func<BeforeUnloadEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('blur',$1...)")>] abstract addEventListener_blur: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplay',$1...)")>] abstract addEventListener_canplay: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('canplaythrough',$1...)")>] abstract addEventListener_canplaythrough: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('change',$1...)")>] abstract addEventListener_change: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('click',$1...)")>] abstract addEventListener_click: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('compassneedscalibration',$1...)")>] abstract addEventListener_compassneedscalibration: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('contextmenu',$1...)")>] abstract addEventListener_contextmenu: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dblclick',$1...)")>] abstract addEventListener_dblclick: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('devicemotion',$1...)")>] abstract addEventListener_devicemotion: listener: Func<DeviceMotionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('deviceorientation',$1...)")>] abstract addEventListener_deviceorientation: listener: Func<DeviceOrientationEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drag',$1...)")>] abstract addEventListener_drag: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragend',$1...)")>] abstract addEventListener_dragend: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragenter',$1...)")>] abstract addEventListener_dragenter: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragleave',$1...)")>] abstract addEventListener_dragleave: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragover',$1...)")>] abstract addEventListener_dragover: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('dragstart',$1...)")>] abstract addEventListener_dragstart: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('drop',$1...)")>] abstract addEventListener_drop: listener: Func<DragEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('durationchange',$1...)")>] abstract addEventListener_durationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('emptied',$1...)")>] abstract addEventListener_emptied: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ended',$1...)")>] abstract addEventListener_ended: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('focus',$1...)")>] abstract addEventListener_focus: listener: Func<FocusEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('hashchange',$1...)")>] abstract addEventListener_hashchange: listener: Func<HashChangeEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('input',$1...)")>] abstract addEventListener_input: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keydown',$1...)")>] abstract addEventListener_keydown: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keypress',$1...)")>] abstract addEventListener_keypress: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('keyup',$1...)")>] abstract addEventListener_keyup: listener: Func<KeyboardEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadeddata',$1...)")>] abstract addEventListener_loadeddata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadedmetadata',$1...)")>] abstract addEventListener_loadedmetadata: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('message',$1...)")>] abstract addEventListener_message: listener: Func<MessageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousedown',$1...)")>] abstract addEventListener_mousedown: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseenter',$1...)")>] abstract addEventListener_mouseenter: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseleave',$1...)")>] abstract addEventListener_mouseleave: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousemove',$1...)")>] abstract addEventListener_mousemove: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseout',$1...)")>] abstract addEventListener_mouseout: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseover',$1...)")>] abstract addEventListener_mouseover: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mouseup',$1...)")>] abstract addEventListener_mouseup: listener: Func<MouseEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('mousewheel',$1...)")>] abstract addEventListener_mousewheel: listener: Func<MouseWheelEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('offline',$1...)")>] abstract addEventListener_offline: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('online',$1...)")>] abstract addEventListener_online: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('orientationchange',$1...)")>] abstract addEventListener_orientationchange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pagehide',$1...)")>] abstract addEventListener_pagehide: listener: Func<PageTransitionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pageshow',$1...)")>] abstract addEventListener_pageshow: listener: Func<PageTransitionEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pause',$1...)")>] abstract addEventListener_pause: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('play',$1...)")>] abstract addEventListener_play: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('playing',$1...)")>] abstract addEventListener_playing: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('popstate',$1...)")>] abstract addEventListener_popstate: listener: Func<PopStateEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('ratechange',$1...)")>] abstract addEventListener_ratechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('readystatechange',$1...)")>] abstract addEventListener_readystatechange: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('reset',$1...)")>] abstract addEventListener_reset: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('resize',$1...)")>] abstract addEventListener_resize: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('scroll',$1...)")>] abstract addEventListener_scroll: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeked',$1...)")>] abstract addEventListener_seeked: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('seeking',$1...)")>] abstract addEventListener_seeking: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('select',$1...)")>] abstract addEventListener_select: listener: Func<UIEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('stalled',$1...)")>] abstract addEventListener_stalled: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('storage',$1...)")>] abstract addEventListener_storage: listener: Func<StorageEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('submit',$1...)")>] abstract addEventListener_submit: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('suspend',$1...)")>] abstract addEventListener_suspend: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeupdate',$1...)")>] abstract addEventListener_timeupdate: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('unload',$1...)")>] abstract addEventListener_unload: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('volumechange',$1...)")>] abstract addEventListener_volumechange: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('waiting',$1...)")>] abstract addEventListener_waiting: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and Worker =
    inherit EventTarget
    inherit AbstractWorker
    abstract onmessage: Func<MessageEvent, obj> with get, set
    abstract postMessage: message: obj * ?ports: obj -> unit
    abstract terminate: unit -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('message',$1...)")>] abstract addEventListener_message: listener: Func<MessageEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and XMLDocument =
    inherit Document

and XMLHttpRequest =
    inherit EventTarget
    inherit XMLHttpRequestEventTarget
    abstract msCaching: string with get, set
    abstract onreadystatechange: Func<ProgressEvent, obj> with get, set
    abstract readyState: float with get, set
    abstract response: obj with get, set
    abstract responseBody: obj with get, set
    abstract responseText: string with get, set
    abstract responseType: string with get, set
    abstract responseXML: obj with get, set
    abstract status: float with get, set
    abstract statusText: string with get, set
    abstract timeout: float with get, set
    abstract upload: XMLHttpRequestUpload with get, set
    abstract withCredentials: bool with get, set
    abstract DONE: float with get, set
    abstract HEADERS_RECEIVED: float with get, set
    abstract LOADING: float with get, set
    abstract OPENED: float with get, set
    abstract UNSENT: float with get, set
    abstract abort: unit -> unit
    abstract getAllResponseHeaders: unit -> string
    abstract getResponseHeader: header: string -> string
    abstract msCachingEnabled: unit -> bool
    abstract ``open``: ``method``: string * url: string * ?async: bool * ?user: string * ?password: string -> unit
    abstract overrideMimeType: mime: string -> unit
    abstract send: ?data: Document -> unit
    abstract send: ?data: string -> unit
    abstract send: ?data: obj -> unit
    abstract setRequestHeader: header: string * value: string -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadend',$1...)")>] abstract addEventListener_loadend: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('readystatechange',$1...)")>] abstract addEventListener_readystatechange: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeout',$1...)")>] abstract addEventListener_timeout: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and XMLHttpRequestUpload =
    inherit EventTarget
    inherit XMLHttpRequestEventTarget
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and XMLSerializer =
    abstract serializeToString: target: Node -> string

and XPathEvaluator =
    abstract createExpression: expression: string * resolver: XPathNSResolver -> XPathExpression
    abstract createNSResolver: ?nodeResolver: Node -> XPathNSResolver
    abstract evaluate: expression: string * contextNode: Node * resolver: XPathNSResolver * ``type``: float * result: XPathResult -> XPathResult

and XPathExpression =
    abstract evaluate: contextNode: Node * ``type``: float * result: XPathResult -> XPathExpression

and XPathNSResolver =
    abstract lookupNamespaceURI: prefix: string -> string

and XPathResult =
    abstract booleanValue: bool with get, set
    abstract invalidIteratorState: bool with get, set
    abstract numberValue: float with get, set
    abstract resultType: float with get, set
    abstract singleNodeValue: Node with get, set
    abstract snapshotLength: float with get, set
    abstract stringValue: string with get, set
    abstract ANY_TYPE: float with get, set
    abstract ANY_UNORDERED_NODE_TYPE: float with get, set
    abstract BOOLEAN_TYPE: float with get, set
    abstract FIRST_ORDERED_NODE_TYPE: float with get, set
    abstract NUMBER_TYPE: float with get, set
    abstract ORDERED_NODE_ITERATOR_TYPE: float with get, set
    abstract ORDERED_NODE_SNAPSHOT_TYPE: float with get, set
    abstract STRING_TYPE: float with get, set
    abstract UNORDERED_NODE_ITERATOR_TYPE: float with get, set
    abstract UNORDERED_NODE_SNAPSHOT_TYPE: float with get, set
    abstract iterateNext: unit -> Node
    abstract snapshotItem: index: float -> Node

and XSLTProcessor =
    abstract clearParameters: unit -> unit
    abstract getParameter: namespaceURI: string * localName: string -> obj
    abstract importStylesheet: style: Node -> unit
    abstract removeParameter: namespaceURI: string * localName: string -> unit
    abstract reset: unit -> unit
    abstract setParameter: namespaceURI: string * localName: string * value: obj -> unit
    abstract transformToDocument: source: Node -> Document
    abstract transformToFragment: source: Node * document: Document -> DocumentFragment

and AbstractWorker =
    abstract onerror: Func<Event, obj> with get, set
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and ChildNode =
    abstract remove: unit -> unit

and DOML2DeprecatedColorProperty =
    abstract color: string with get, set

and DOML2DeprecatedSizeProperty =
    abstract size: float with get, set

and DocumentEvent =
    [<Emit("$0.createEvent('AnimationEvent')")>] abstract createEvent_AnimationEvent: unit -> AnimationEvent
    [<Emit("$0.createEvent('AriaRequestEvent')")>] abstract createEvent_AriaRequestEvent: unit -> AriaRequestEvent
    [<Emit("$0.createEvent('AudioProcessingEvent')")>] abstract createEvent_AudioProcessingEvent: unit -> AudioProcessingEvent
    [<Emit("$0.createEvent('BeforeUnloadEvent')")>] abstract createEvent_BeforeUnloadEvent: unit -> BeforeUnloadEvent
    [<Emit("$0.createEvent('ClipboardEvent')")>] abstract createEvent_ClipboardEvent: unit -> ClipboardEvent
    [<Emit("$0.createEvent('CloseEvent')")>] abstract createEvent_CloseEvent: unit -> CloseEvent
    [<Emit("$0.createEvent('CommandEvent')")>] abstract createEvent_CommandEvent: unit -> CommandEvent
    [<Emit("$0.createEvent('CompositionEvent')")>] abstract createEvent_CompositionEvent: unit -> CompositionEvent
    [<Emit("$0.createEvent('CustomEvent')")>] abstract createEvent_CustomEvent: unit -> CustomEvent
    [<Emit("$0.createEvent('DeviceMotionEvent')")>] abstract createEvent_DeviceMotionEvent: unit -> DeviceMotionEvent
    [<Emit("$0.createEvent('DeviceOrientationEvent')")>] abstract createEvent_DeviceOrientationEvent: unit -> DeviceOrientationEvent
    [<Emit("$0.createEvent('DragEvent')")>] abstract createEvent_DragEvent: unit -> DragEvent
    [<Emit("$0.createEvent('ErrorEvent')")>] abstract createEvent_ErrorEvent: unit -> ErrorEvent
    [<Emit("$0.createEvent('Event')")>] abstract createEvent_Event: unit -> Event
    [<Emit("$0.createEvent('Events')")>] abstract createEvent_Events: unit -> Event
    [<Emit("$0.createEvent('FocusEvent')")>] abstract createEvent_FocusEvent: unit -> FocusEvent
    [<Emit("$0.createEvent('GamepadEvent')")>] abstract createEvent_GamepadEvent: unit -> GamepadEvent
    [<Emit("$0.createEvent('HashChangeEvent')")>] abstract createEvent_HashChangeEvent: unit -> HashChangeEvent
    [<Emit("$0.createEvent('IDBVersionChangeEvent')")>] abstract createEvent_IDBVersionChangeEvent: unit -> IDBVersionChangeEvent
    [<Emit("$0.createEvent('KeyboardEvent')")>] abstract createEvent_KeyboardEvent: unit -> KeyboardEvent
    [<Emit("$0.createEvent('LongRunningScriptDetectedEvent')")>] abstract createEvent_LongRunningScriptDetectedEvent: unit -> LongRunningScriptDetectedEvent
    [<Emit("$0.createEvent('MSGestureEvent')")>] abstract createEvent_MSGestureEvent: unit -> MSGestureEvent
    [<Emit("$0.createEvent('MSManipulationEvent')")>] abstract createEvent_MSManipulationEvent: unit -> MSManipulationEvent
    [<Emit("$0.createEvent('MSMediaKeyMessageEvent')")>] abstract createEvent_MSMediaKeyMessageEvent: unit -> MSMediaKeyMessageEvent
    [<Emit("$0.createEvent('MSMediaKeyNeededEvent')")>] abstract createEvent_MSMediaKeyNeededEvent: unit -> MSMediaKeyNeededEvent
    [<Emit("$0.createEvent('MSPointerEvent')")>] abstract createEvent_MSPointerEvent: unit -> MSPointerEvent
    [<Emit("$0.createEvent('MSSiteModeEvent')")>] abstract createEvent_MSSiteModeEvent: unit -> MSSiteModeEvent
    [<Emit("$0.createEvent('MessageEvent')")>] abstract createEvent_MessageEvent: unit -> MessageEvent
    [<Emit("$0.createEvent('MouseEvent')")>] abstract createEvent_MouseEvent: unit -> MouseEvent
    [<Emit("$0.createEvent('MouseEvents')")>] abstract createEvent_MouseEvents: unit -> MouseEvent
    [<Emit("$0.createEvent('MouseWheelEvent')")>] abstract createEvent_MouseWheelEvent: unit -> MouseWheelEvent
    [<Emit("$0.createEvent('MutationEvent')")>] abstract createEvent_MutationEvent: unit -> MutationEvent
    [<Emit("$0.createEvent('MutationEvents')")>] abstract createEvent_MutationEvents: unit -> MutationEvent
    [<Emit("$0.createEvent('NavigationCompletedEvent')")>] abstract createEvent_NavigationCompletedEvent: unit -> NavigationCompletedEvent
    [<Emit("$0.createEvent('NavigationEvent')")>] abstract createEvent_NavigationEvent: unit -> NavigationEvent
    [<Emit("$0.createEvent('NavigationEventWithReferrer')")>] abstract createEvent_NavigationEventWithReferrer: unit -> NavigationEventWithReferrer
    [<Emit("$0.createEvent('OfflineAudioCompletionEvent')")>] abstract createEvent_OfflineAudioCompletionEvent: unit -> OfflineAudioCompletionEvent
    [<Emit("$0.createEvent('PageTransitionEvent')")>] abstract createEvent_PageTransitionEvent: unit -> PageTransitionEvent
    [<Emit("$0.createEvent('PermissionRequestedEvent')")>] abstract createEvent_PermissionRequestedEvent: unit -> PermissionRequestedEvent
    [<Emit("$0.createEvent('PointerEvent')")>] abstract createEvent_PointerEvent: unit -> PointerEvent
    [<Emit("$0.createEvent('PopStateEvent')")>] abstract createEvent_PopStateEvent: unit -> PopStateEvent
    [<Emit("$0.createEvent('ProgressEvent')")>] abstract createEvent_ProgressEvent: unit -> ProgressEvent
    [<Emit("$0.createEvent('SVGZoomEvent')")>] abstract createEvent_SVGZoomEvent: unit -> SVGZoomEvent
    [<Emit("$0.createEvent('SVGZoomEvents')")>] abstract createEvent_SVGZoomEvents: unit -> SVGZoomEvent
    [<Emit("$0.createEvent('ScriptNotifyEvent')")>] abstract createEvent_ScriptNotifyEvent: unit -> ScriptNotifyEvent
    [<Emit("$0.createEvent('StorageEvent')")>] abstract createEvent_StorageEvent: unit -> StorageEvent
    [<Emit("$0.createEvent('TextEvent')")>] abstract createEvent_TextEvent: unit -> TextEvent
    [<Emit("$0.createEvent('TouchEvent')")>] abstract createEvent_TouchEvent: unit -> TouchEvent
    [<Emit("$0.createEvent('TrackEvent')")>] abstract createEvent_TrackEvent: unit -> TrackEvent
    [<Emit("$0.createEvent('TransitionEvent')")>] abstract createEvent_TransitionEvent: unit -> TransitionEvent
    [<Emit("$0.createEvent('UIEvent')")>] abstract createEvent_UIEvent: unit -> UIEvent
    [<Emit("$0.createEvent('UIEvents')")>] abstract createEvent_UIEvents: unit -> UIEvent
    [<Emit("$0.createEvent('UnviewableContentIdentifiedEvent')")>] abstract createEvent_UnviewableContentIdentifiedEvent: unit -> UnviewableContentIdentifiedEvent
    [<Emit("$0.createEvent('WebGLContextEvent')")>] abstract createEvent_WebGLContextEvent: unit -> WebGLContextEvent
    [<Emit("$0.createEvent('WheelEvent')")>] abstract createEvent_WheelEvent: unit -> WheelEvent
    abstract createEvent: eventInterface: string -> Event

and ElementTraversal =
    abstract childElementCount: float with get, set
    abstract firstElementChild: Element with get, set
    abstract lastElementChild: Element with get, set
    abstract nextElementSibling: Element with get, set
    abstract previousElementSibling: Element with get, set

and GetSVGDocument =
    abstract getSVGDocument: unit -> Document

and GlobalEventHandlers =
    abstract onpointercancel: Func<PointerEvent, obj> with get, set
    abstract onpointerdown: Func<PointerEvent, obj> with get, set
    abstract onpointerenter: Func<PointerEvent, obj> with get, set
    abstract onpointerleave: Func<PointerEvent, obj> with get, set
    abstract onpointermove: Func<PointerEvent, obj> with get, set
    abstract onpointerout: Func<PointerEvent, obj> with get, set
    abstract onpointerover: Func<PointerEvent, obj> with get, set
    abstract onpointerup: Func<PointerEvent, obj> with get, set
    abstract onwheel: Func<WheelEvent, obj> with get, set
    [<Emit("$0.addEventListener('pointercancel',$1...)")>] abstract addEventListener_pointercancel: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerdown',$1...)")>] abstract addEventListener_pointerdown: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerenter',$1...)")>] abstract addEventListener_pointerenter: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerleave',$1...)")>] abstract addEventListener_pointerleave: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointermove',$1...)")>] abstract addEventListener_pointermove: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerout',$1...)")>] abstract addEventListener_pointerout: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerover',$1...)")>] abstract addEventListener_pointerover: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('pointerup',$1...)")>] abstract addEventListener_pointerup: listener: Func<PointerEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('wheel',$1...)")>] abstract addEventListener_wheel: listener: Func<WheelEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and HTMLTableAlignment =
    abstract ch: string with get, set
    abstract chOff: string with get, set
    abstract vAlign: string with get, set

and IDBEnvironment =
    abstract indexedDB: IDBFactory with get, set
    abstract msIndexedDB: IDBFactory with get, set

and LinkStyle =
    abstract sheet: StyleSheet with get, set

and MSBaseReader =
    abstract onabort: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onloadend: Func<ProgressEvent, obj> with get, set
    abstract onloadstart: Func<Event, obj> with get, set
    abstract onprogress: Func<ProgressEvent, obj> with get, set
    abstract readyState: float with get, set
    abstract result: obj with get, set
    abstract DONE: float with get, set
    abstract EMPTY: float with get, set
    abstract LOADING: float with get, set
    abstract abort: unit -> unit
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadend',$1...)")>] abstract addEventListener_loadend: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and MSFileSaver =
    abstract msSaveBlob: blob: obj * ?defaultName: string -> bool
    abstract msSaveOrOpenBlob: blob: obj * ?defaultName: string -> bool

and MSNavigatorDoNotTrack =
    abstract confirmSiteSpecificTrackingException: args: ConfirmSiteSpecificExceptionsInformation -> bool
    abstract confirmWebWideTrackingException: args: ExceptionInformation -> bool
    abstract removeSiteSpecificTrackingException: args: ExceptionInformation -> unit
    abstract removeWebWideTrackingException: args: ExceptionInformation -> unit
    abstract storeSiteSpecificTrackingException: args: StoreSiteSpecificExceptionsInformation -> unit
    abstract storeWebWideTrackingException: args: StoreExceptionsInformation -> unit

and NavigatorContentUtils =
    interface end

and NavigatorGeolocation =
    abstract geolocation: Geolocation with get, set

and NavigatorID =
    abstract appName: string with get, set
    abstract appVersion: string with get, set
    abstract platform: string with get, set
    abstract product: string with get, set
    abstract productSub: string with get, set
    abstract userAgent: string with get, set
    abstract vendor: string with get, set
    abstract vendorSub: string with get, set

and NavigatorOnLine =
    abstract onLine: bool with get, set

and NavigatorStorageUtils =
    interface end

and NodeSelector =
    abstract querySelector: selectors: string -> Element
    abstract querySelectorAll: selectors: string -> NodeListOf<Element>

and RandomSource =
    abstract getRandomValues: array: ArrayBufferView -> ArrayBufferView

and SVGAnimatedPathData =
    abstract pathSegList: SVGPathSegList with get, set

and SVGAnimatedPoints =
    abstract animatedPoints: SVGPointList with get, set
    abstract points: SVGPointList with get, set

and SVGExternalResourcesRequired =
    abstract externalResourcesRequired: SVGAnimatedBoolean with get, set

and SVGFilterPrimitiveStandardAttributes =
    inherit SVGStylable
    abstract height: SVGAnimatedLength with get, set
    abstract result: SVGAnimatedString with get, set
    abstract width: SVGAnimatedLength with get, set
    abstract x: SVGAnimatedLength with get, set
    abstract y: SVGAnimatedLength with get, set

and SVGFitToViewBox =
    abstract preserveAspectRatio: SVGAnimatedPreserveAspectRatio with get, set
    abstract viewBox: SVGAnimatedRect with get, set

and SVGLangSpace =
    abstract xmllang: string with get, set
    abstract xmlspace: string with get, set

and SVGLocatable =
    abstract farthestViewportElement: SVGElement with get, set
    abstract nearestViewportElement: SVGElement with get, set
    abstract getBBox: unit -> SVGRect
    abstract getCTM: unit -> SVGMatrix
    abstract getScreenCTM: unit -> SVGMatrix
    abstract getTransformToElement: element: SVGElement -> SVGMatrix

and SVGStylable =
    abstract className: obj with get, set
    abstract style: CSSStyleDeclaration with get, set

and SVGTests =
    abstract requiredExtensions: SVGStringList with get, set
    abstract requiredFeatures: SVGStringList with get, set
    abstract systemLanguage: SVGStringList with get, set
    abstract hasExtension: extension: string -> bool

and SVGTransformable =
    inherit SVGLocatable
    abstract transform: SVGAnimatedTransformList with get, set

and SVGURIReference =
    abstract href: SVGAnimatedString with get, set

and WindowBase64 =
    abstract atob: encodedString: string -> string
    abstract btoa: rawString: string -> string

and WindowConsole =
    abstract console: Console with get, set

and WindowLocalStorage =
    abstract localStorage: Storage with get, set

and WindowSessionStorage =
    abstract sessionStorage: Storage with get, set

and WindowTimers =
    inherit obj
    inherit WindowTimersExtension
    abstract clearInterval: handle: float -> unit
    abstract clearTimeout: handle: float -> unit
    abstract setInterval: handler: obj * ?timeout: obj * [<ParamArray>] args: obj[] -> float
    abstract setTimeout: handler: obj * ?timeout: obj * [<ParamArray>] args: obj[] -> float

and WindowTimersExtension =
    abstract clearImmediate: handle: float -> unit
    abstract msClearImmediate: handle: float -> unit
    abstract msSetImmediate: expression: obj * [<ParamArray>] args: obj[] -> float
    abstract setImmediate: expression: obj * [<ParamArray>] args: obj[] -> float

and XMLHttpRequestEventTarget =
    abstract onabort: Func<Event, obj> with get, set
    abstract onerror: Func<Event, obj> with get, set
    abstract onload: Func<Event, obj> with get, set
    abstract onloadend: Func<ProgressEvent, obj> with get, set
    abstract onloadstart: Func<Event, obj> with get, set
    abstract onprogress: Func<ProgressEvent, obj> with get, set
    abstract ontimeout: Func<ProgressEvent, obj> with get, set
    [<Emit("$0.addEventListener('abort',$1...)")>] abstract addEventListener_abort: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('error',$1...)")>] abstract addEventListener_error: listener: Func<ErrorEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('load',$1...)")>] abstract addEventListener_load: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadend',$1...)")>] abstract addEventListener_loadend: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('loadstart',$1...)")>] abstract addEventListener_loadstart: listener: Func<Event, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('progress',$1...)")>] abstract addEventListener_progress: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    [<Emit("$0.addEventListener('timeout',$1...)")>] abstract addEventListener_timeout: listener: Func<ProgressEvent, obj> * ?useCapture: bool -> unit
    abstract addEventListener: ``type``: string * listener: EventListenerOrEventListenerObject * ?useCapture: bool -> unit

and StorageEventInit =
    inherit EventInit
    abstract key: string option with get, set
    abstract oldValue: string option with get, set
    abstract newValue: string option with get, set
    abstract url: string with get, set
    abstract storageArea: Storage option with get, set

and IDBObjectStoreParameters =
    abstract keyPath: U2<string, ResizeArray<string>> option with get, set
    abstract autoIncrement: bool option with get, set

and IDBIndexParameters =
    abstract unique: bool option with get, set
    abstract multiEntry: bool option with get, set

and NodeListOf<'TNode> =
    inherit NodeList
    abstract length: float with get, set
    abstract item: index: float -> TNode

and BlobPropertyBag =
    abstract ``type``: string option with get, set
    abstract endings: string option with get, set

and FilePropertyBag =
    abstract ``type``: string option with get, set
    abstract lastModified: float option with get, set

and EventListenerObject =
    abstract handleEvent: evt: Event -> unit

and MessageEventInit =
    inherit EventInit
    abstract data: obj option with get, set
    abstract origin: string option with get, set
    abstract lastEventId: string option with get, set
    abstract channel: string option with get, set
    abstract source: obj option with get, set
    abstract ports: ResizeArray<MessagePort> option with get, set

and ProgressEventInit =
    inherit EventInit
    abstract lengthComputable: bool option with get, set
    abstract loaded: float option with get, set
    abstract total: float option with get, set

and HTMLTemplateElement =
    inherit HTMLElement
    abstract content: DocumentFragment with get, set

and HTMLPictureElement =
    inherit HTMLElement

and EventListenerOrEventListenerObject =
    U2<EventListener, EventListenerObject>

and ErrorEventHandler =
    interface end

and PositionCallback =
    interface end

and PositionErrorCallback =
    interface end

and MediaQueryListListener =
    interface end

and MSLaunchUriCallback =
    interface end

and FrameRequestCallback =
    interface end

and MSUnsafeFunctionCallback =
    interface end

and MSExecAtPriorityFunctionCallback =
    interface end

and MutationCallback =
    interface end

and DecodeSuccessCallback =
    interface end

and DecodeErrorCallback =
    interface end

and FunctionStringCallback =
    interface end

module Globals =
    let [<Global>] ANGLE_instanced_arrays: obj = failwith "JS only"
    let [<Global>] AnalyserNode: obj = failwith "JS only"
    let [<Global>] AnimationEvent: obj = failwith "JS only"
    let [<Global>] ApplicationCache: obj = failwith "JS only"
    let [<Global>] AriaRequestEvent: obj = failwith "JS only"
    let [<Global>] Attr: obj = failwith "JS only"
    let [<Global>] AudioBuffer: obj = failwith "JS only"
    let [<Global>] AudioBufferSourceNode: obj = failwith "JS only"
    let [<Global>] AudioContext: obj = failwith "JS only"
    let [<Global>] AudioDestinationNode: obj = failwith "JS only"
    let [<Global>] AudioListener: obj = failwith "JS only"
    let [<Global>] AudioNode: obj = failwith "JS only"
    let [<Global>] AudioParam: obj = failwith "JS only"
    let [<Global>] AudioProcessingEvent: obj = failwith "JS only"
    let [<Global>] AudioTrack: obj = failwith "JS only"
    let [<Global>] AudioTrackList: obj = failwith "JS only"
    let [<Global>] BarProp: obj = failwith "JS only"
    let [<Global>] BeforeUnloadEvent: obj = failwith "JS only"
    let [<Global>] BiquadFilterNode: obj = failwith "JS only"
    let [<Global>] Blob: obj = failwith "JS only"
    let [<Global>] CDATASection: obj = failwith "JS only"
    let [<Global>] CSS: CSS = failwith "JS only"
    let [<Global>] CSSConditionRule: obj = failwith "JS only"
    let [<Global>] CSSFontFaceRule: obj = failwith "JS only"
    let [<Global>] CSSGroupingRule: obj = failwith "JS only"
    let [<Global>] CSSImportRule: obj = failwith "JS only"
    let [<Global>] CSSKeyframeRule: obj = failwith "JS only"
    let [<Global>] CSSKeyframesRule: obj = failwith "JS only"
    let [<Global>] CSSMediaRule: obj = failwith "JS only"
    let [<Global>] CSSNamespaceRule: obj = failwith "JS only"
    let [<Global>] CSSPageRule: obj = failwith "JS only"
    let [<Global>] CSSRule: obj = failwith "JS only"
    let [<Global>] CSSRuleList: obj = failwith "JS only"
    let [<Global>] CSSStyleDeclaration: obj = failwith "JS only"
    let [<Global>] CSSStyleRule: obj = failwith "JS only"
    let [<Global>] CSSStyleSheet: obj = failwith "JS only"
    let [<Global>] CSSSupportsRule: obj = failwith "JS only"
    let [<Global>] CanvasGradient: obj = failwith "JS only"
    let [<Global>] CanvasPattern: obj = failwith "JS only"
    let [<Global>] CanvasRenderingContext2D: obj = failwith "JS only"
    let [<Global>] ChannelMergerNode: obj = failwith "JS only"
    let [<Global>] ChannelSplitterNode: obj = failwith "JS only"
    let [<Global>] CharacterData: obj = failwith "JS only"
    let [<Global>] ClientRect: obj = failwith "JS only"
    let [<Global>] ClientRectList: obj = failwith "JS only"
    let [<Global>] ClipboardEvent: obj = failwith "JS only"
    let [<Global>] CloseEvent: obj = failwith "JS only"
    let [<Global>] CommandEvent: obj = failwith "JS only"
    let [<Global>] Comment: obj = failwith "JS only"
    let [<Global>] CompositionEvent: obj = failwith "JS only"
    let [<Global>] Console: obj = failwith "JS only"
    let [<Global>] ConvolverNode: obj = failwith "JS only"
    let [<Global>] Coordinates: obj = failwith "JS only"
    let [<Global>] Crypto: obj = failwith "JS only"
    let [<Global>] CryptoKey: obj = failwith "JS only"
    let [<Global>] CryptoKeyPair: obj = failwith "JS only"
    let [<Global>] CustomEvent: obj = failwith "JS only"
    let [<Global>] DOMError: obj = failwith "JS only"
    let [<Global>] DOMException: obj = failwith "JS only"
    let [<Global>] DOMImplementation: obj = failwith "JS only"
    let [<Global>] DOMParser: obj = failwith "JS only"
    let [<Global>] DOMSettableTokenList: obj = failwith "JS only"
    let [<Global>] DOMStringList: obj = failwith "JS only"
    let [<Global>] DOMStringMap: obj = failwith "JS only"
    let [<Global>] DOMTokenList: obj = failwith "JS only"
    let [<Global>] DataCue: obj = failwith "JS only"
    let [<Global>] DataTransfer: obj = failwith "JS only"
    let [<Global>] DataTransferItem: obj = failwith "JS only"
    let [<Global>] DataTransferItemList: obj = failwith "JS only"
    let [<Global>] DeferredPermissionRequest: obj = failwith "JS only"
    let [<Global>] DelayNode: obj = failwith "JS only"
    let [<Global>] DeviceAcceleration: obj = failwith "JS only"
    let [<Global>] DeviceMotionEvent: obj = failwith "JS only"
    let [<Global>] DeviceOrientationEvent: obj = failwith "JS only"
    let [<Global>] DeviceRotationRate: obj = failwith "JS only"
    let [<Global>] Document: obj = failwith "JS only"
    let [<Global>] DocumentFragment: obj = failwith "JS only"
    let [<Global>] DocumentType: obj = failwith "JS only"
    let [<Global>] DragEvent: obj = failwith "JS only"
    let [<Global>] DynamicsCompressorNode: obj = failwith "JS only"
    let [<Global>] EXT_texture_filter_anisotropic: obj = failwith "JS only"
    let [<Global>] Element: obj = failwith "JS only"
    let [<Global>] ErrorEvent: obj = failwith "JS only"
    let [<Global>] Event: obj = failwith "JS only"
    let [<Global>] EventTarget: obj = failwith "JS only"
    let [<Global>] External: obj = failwith "JS only"
    let [<Global>] File: obj = failwith "JS only"
    let [<Global>] FileList: obj = failwith "JS only"
    let [<Global>] FileReader: obj = failwith "JS only"
    let [<Global>] FocusEvent: obj = failwith "JS only"
    let [<Global>] FormData: obj = failwith "JS only"
    let [<Global>] GainNode: obj = failwith "JS only"
    let [<Global>] Gamepad: obj = failwith "JS only"
    let [<Global>] GamepadButton: obj = failwith "JS only"
    let [<Global>] GamepadEvent: obj = failwith "JS only"
    let [<Global>] Geolocation: obj = failwith "JS only"
    let [<Global>] HTMLAllCollection: obj = failwith "JS only"
    let [<Global>] HTMLAnchorElement: obj = failwith "JS only"
    let [<Global>] HTMLAppletElement: obj = failwith "JS only"
    let [<Global>] HTMLAreaElement: obj = failwith "JS only"
    let [<Global>] HTMLAreasCollection: obj = failwith "JS only"
    let [<Global>] HTMLAudioElement: obj = failwith "JS only"
    let [<Global>] HTMLBRElement: obj = failwith "JS only"
    let [<Global>] HTMLBaseElement: obj = failwith "JS only"
    let [<Global>] HTMLBaseFontElement: obj = failwith "JS only"
    let [<Global>] HTMLBlockElement: obj = failwith "JS only"
    let [<Global>] HTMLBodyElement: obj = failwith "JS only"
    let [<Global>] HTMLButtonElement: obj = failwith "JS only"
    let [<Global>] HTMLCanvasElement: obj = failwith "JS only"
    let [<Global>] HTMLCollection: obj = failwith "JS only"
    let [<Global>] HTMLDDElement: obj = failwith "JS only"
    let [<Global>] HTMLDListElement: obj = failwith "JS only"
    let [<Global>] HTMLDTElement: obj = failwith "JS only"
    let [<Global>] HTMLDataListElement: obj = failwith "JS only"
    let [<Global>] HTMLDirectoryElement: obj = failwith "JS only"
    let [<Global>] HTMLDivElement: obj = failwith "JS only"
    let [<Global>] HTMLDocument: obj = failwith "JS only"
    let [<Global>] HTMLElement: obj = failwith "JS only"
    let [<Global>] HTMLEmbedElement: obj = failwith "JS only"
    let [<Global>] HTMLFieldSetElement: obj = failwith "JS only"
    let [<Global>] HTMLFontElement: obj = failwith "JS only"
    let [<Global>] HTMLFormElement: obj = failwith "JS only"
    let [<Global>] HTMLFrameElement: obj = failwith "JS only"
    let [<Global>] HTMLFrameSetElement: obj = failwith "JS only"
    let [<Global>] HTMLHRElement: obj = failwith "JS only"
    let [<Global>] HTMLHeadElement: obj = failwith "JS only"
    let [<Global>] HTMLHeadingElement: obj = failwith "JS only"
    let [<Global>] HTMLHtmlElement: obj = failwith "JS only"
    let [<Global>] HTMLIFrameElement: obj = failwith "JS only"
    let [<Global>] HTMLImageElement: obj = failwith "JS only"
    let [<Global>] HTMLInputElement: obj = failwith "JS only"
    let [<Global>] HTMLIsIndexElement: obj = failwith "JS only"
    let [<Global>] HTMLLIElement: obj = failwith "JS only"
    let [<Global>] HTMLLabelElement: obj = failwith "JS only"
    let [<Global>] HTMLLegendElement: obj = failwith "JS only"
    let [<Global>] HTMLLinkElement: obj = failwith "JS only"
    let [<Global>] HTMLMapElement: obj = failwith "JS only"
    let [<Global>] HTMLMarqueeElement: obj = failwith "JS only"
    let [<Global>] HTMLMediaElement: obj = failwith "JS only"
    let [<Global>] HTMLMenuElement: obj = failwith "JS only"
    let [<Global>] HTMLMetaElement: obj = failwith "JS only"
    let [<Global>] HTMLModElement: obj = failwith "JS only"
    let [<Global>] HTMLNextIdElement: obj = failwith "JS only"
    let [<Global>] HTMLOListElement: obj = failwith "JS only"
    let [<Global>] HTMLObjectElement: obj = failwith "JS only"
    let [<Global>] HTMLOptGroupElement: obj = failwith "JS only"
    let [<Global>] HTMLOptionElement: obj = failwith "JS only"
    let [<Global>] HTMLParagraphElement: obj = failwith "JS only"
    let [<Global>] HTMLParamElement: obj = failwith "JS only"
    let [<Global>] HTMLPhraseElement: obj = failwith "JS only"
    let [<Global>] HTMLPreElement: obj = failwith "JS only"
    let [<Global>] HTMLProgressElement: obj = failwith "JS only"
    let [<Global>] HTMLQuoteElement: obj = failwith "JS only"
    let [<Global>] HTMLScriptElement: obj = failwith "JS only"
    let [<Global>] HTMLSelectElement: obj = failwith "JS only"
    let [<Global>] HTMLSourceElement: obj = failwith "JS only"
    let [<Global>] HTMLSpanElement: obj = failwith "JS only"
    let [<Global>] HTMLStyleElement: obj = failwith "JS only"
    let [<Global>] HTMLTableCaptionElement: obj = failwith "JS only"
    let [<Global>] HTMLTableCellElement: obj = failwith "JS only"
    let [<Global>] HTMLTableColElement: obj = failwith "JS only"
    let [<Global>] HTMLTableDataCellElement: obj = failwith "JS only"
    let [<Global>] HTMLTableElement: obj = failwith "JS only"
    let [<Global>] HTMLTableHeaderCellElement: obj = failwith "JS only"
    let [<Global>] HTMLTableRowElement: obj = failwith "JS only"
    let [<Global>] HTMLTableSectionElement: obj = failwith "JS only"
    let [<Global>] HTMLTextAreaElement: obj = failwith "JS only"
    let [<Global>] HTMLTitleElement: obj = failwith "JS only"
    let [<Global>] HTMLTrackElement: obj = failwith "JS only"
    let [<Global>] HTMLUListElement: obj = failwith "JS only"
    let [<Global>] HTMLUnknownElement: obj = failwith "JS only"
    let [<Global>] HTMLVideoElement: obj = failwith "JS only"
    let [<Global>] HashChangeEvent: obj = failwith "JS only"
    let [<Global>] History: obj = failwith "JS only"
    let [<Global>] IDBCursor: obj = failwith "JS only"
    let [<Global>] IDBCursorWithValue: obj = failwith "JS only"
    let [<Global>] IDBDatabase: obj = failwith "JS only"
    let [<Global>] IDBFactory: obj = failwith "JS only"
    let [<Global>] IDBIndex: obj = failwith "JS only"
    let [<Global>] IDBKeyRange: obj = failwith "JS only"
    let [<Global>] IDBObjectStore: obj = failwith "JS only"
    let [<Global>] IDBOpenDBRequest: obj = failwith "JS only"
    let [<Global>] IDBRequest: obj = failwith "JS only"
    let [<Global>] IDBTransaction: obj = failwith "JS only"
    let [<Global>] IDBVersionChangeEvent: obj = failwith "JS only"
    let [<Global>] ImageData: obj = failwith "JS only"
    let [<Global>] KeyboardEvent: obj = failwith "JS only"
    let [<Global>] Location: obj = failwith "JS only"
    let [<Global>] LongRunningScriptDetectedEvent: obj = failwith "JS only"
    let [<Global>] MSApp: MSApp = failwith "JS only"
    let [<Global>] MSAppAsyncOperation: obj = failwith "JS only"
    let [<Global>] MSBlobBuilder: obj = failwith "JS only"
    let [<Global>] MSCSSMatrix: obj = failwith "JS only"
    let [<Global>] MSGesture: obj = failwith "JS only"
    let [<Global>] MSGestureEvent: obj = failwith "JS only"
    let [<Global>] MSGraphicsTrust: obj = failwith "JS only"
    let [<Global>] MSHTMLWebViewElement: obj = failwith "JS only"
    let [<Global>] MSInputMethodContext: obj = failwith "JS only"
    let [<Global>] MSManipulationEvent: obj = failwith "JS only"
    let [<Global>] MSMediaKeyError: obj = failwith "JS only"
    let [<Global>] MSMediaKeyMessageEvent: obj = failwith "JS only"
    let [<Global>] MSMediaKeyNeededEvent: obj = failwith "JS only"
    let [<Global>] MSMediaKeySession: obj = failwith "JS only"
    let [<Global>] MSMediaKeys: obj = failwith "JS only"
    let [<Global>] MSMimeTypesCollection: obj = failwith "JS only"
    let [<Global>] MSPluginsCollection: obj = failwith "JS only"
    let [<Global>] MSPointerEvent: obj = failwith "JS only"
    let [<Global>] MSRangeCollection: obj = failwith "JS only"
    let [<Global>] MSSiteModeEvent: obj = failwith "JS only"
    let [<Global>] MSStream: obj = failwith "JS only"
    let [<Global>] MSStreamReader: obj = failwith "JS only"
    let [<Global>] MSWebViewAsyncOperation: obj = failwith "JS only"
    let [<Global>] MSWebViewSettings: obj = failwith "JS only"
    let [<Global>] MediaElementAudioSourceNode: obj = failwith "JS only"
    let [<Global>] MediaError: obj = failwith "JS only"
    let [<Global>] MediaList: obj = failwith "JS only"
    let [<Global>] MediaQueryList: obj = failwith "JS only"
    let [<Global>] MediaSource: obj = failwith "JS only"
    let [<Global>] MessageChannel: obj = failwith "JS only"
    let [<Global>] MessageEvent: obj = failwith "JS only"
    let [<Global>] MessagePort: obj = failwith "JS only"
    let [<Global>] MimeType: obj = failwith "JS only"
    let [<Global>] MimeTypeArray: obj = failwith "JS only"
    let [<Global>] MouseEvent: obj = failwith "JS only"
    let [<Global>] MouseWheelEvent: obj = failwith "JS only"
    let [<Global>] MutationEvent: obj = failwith "JS only"
    let [<Global>] MutationObserver: obj = failwith "JS only"
    let [<Global>] MutationRecord: obj = failwith "JS only"
    let [<Global>] NamedNodeMap: obj = failwith "JS only"
    let [<Global>] NavigationCompletedEvent: obj = failwith "JS only"
    let [<Global>] NavigationEvent: obj = failwith "JS only"
    let [<Global>] NavigationEventWithReferrer: obj = failwith "JS only"
    let [<Global>] Navigator: obj = failwith "JS only"
    let [<Global>] Node: obj = failwith "JS only"
    let [<Global>] NodeFilter: obj = failwith "JS only"
    let [<Global>] NodeIterator: obj = failwith "JS only"
    let [<Global>] NodeList: obj = failwith "JS only"
    let [<Global>] OES_element_index_uint: obj = failwith "JS only"
    let [<Global>] OES_standard_derivatives: obj = failwith "JS only"
    let [<Global>] OES_texture_float: obj = failwith "JS only"
    let [<Global>] OES_texture_float_linear: obj = failwith "JS only"
    let [<Global>] OfflineAudioCompletionEvent: obj = failwith "JS only"
    let [<Global>] OfflineAudioContext: obj = failwith "JS only"
    let [<Global>] OscillatorNode: obj = failwith "JS only"
    let [<Global>] PageTransitionEvent: obj = failwith "JS only"
    let [<Global>] PannerNode: obj = failwith "JS only"
    let [<Global>] PerfWidgetExternal: obj = failwith "JS only"
    let [<Global>] Performance: obj = failwith "JS only"
    let [<Global>] PerformanceEntry: obj = failwith "JS only"
    let [<Global>] PerformanceMark: obj = failwith "JS only"
    let [<Global>] PerformanceMeasure: obj = failwith "JS only"
    let [<Global>] PerformanceNavigation: obj = failwith "JS only"
    let [<Global>] PerformanceNavigationTiming: obj = failwith "JS only"
    let [<Global>] PerformanceResourceTiming: obj = failwith "JS only"
    let [<Global>] PerformanceTiming: obj = failwith "JS only"
    let [<Global>] PeriodicWave: obj = failwith "JS only"
    let [<Global>] PermissionRequest: obj = failwith "JS only"
    let [<Global>] PermissionRequestedEvent: obj = failwith "JS only"
    let [<Global>] Plugin: obj = failwith "JS only"
    let [<Global>] PluginArray: obj = failwith "JS only"
    let [<Global>] PointerEvent: obj = failwith "JS only"
    let [<Global>] PopStateEvent: obj = failwith "JS only"
    let [<Global>] Position: obj = failwith "JS only"
    let [<Global>] PositionError: obj = failwith "JS only"
    let [<Global>] ProcessingInstruction: obj = failwith "JS only"
    let [<Global>] ProgressEvent: obj = failwith "JS only"
    let [<Global>] Range: obj = failwith "JS only"
    let [<Global>] SVGAElement: obj = failwith "JS only"
    let [<Global>] SVGAngle: obj = failwith "JS only"
    let [<Global>] SVGAnimatedAngle: obj = failwith "JS only"
    let [<Global>] SVGAnimatedBoolean: obj = failwith "JS only"
    let [<Global>] SVGAnimatedEnumeration: obj = failwith "JS only"
    let [<Global>] SVGAnimatedInteger: obj = failwith "JS only"
    let [<Global>] SVGAnimatedLength: obj = failwith "JS only"
    let [<Global>] SVGAnimatedLengthList: obj = failwith "JS only"
    let [<Global>] SVGAnimatedNumber: obj = failwith "JS only"
    let [<Global>] SVGAnimatedNumberList: obj = failwith "JS only"
    let [<Global>] SVGAnimatedPreserveAspectRatio: obj = failwith "JS only"
    let [<Global>] SVGAnimatedRect: obj = failwith "JS only"
    let [<Global>] SVGAnimatedString: obj = failwith "JS only"
    let [<Global>] SVGAnimatedTransformList: obj = failwith "JS only"
    let [<Global>] SVGCircleElement: obj = failwith "JS only"
    let [<Global>] SVGClipPathElement: obj = failwith "JS only"
    let [<Global>] SVGComponentTransferFunctionElement: obj = failwith "JS only"
    let [<Global>] SVGDefsElement: obj = failwith "JS only"
    let [<Global>] SVGDescElement: obj = failwith "JS only"
    let [<Global>] SVGElement: obj = failwith "JS only"
    let [<Global>] SVGElementInstance: obj = failwith "JS only"
    let [<Global>] SVGElementInstanceList: obj = failwith "JS only"
    let [<Global>] SVGEllipseElement: obj = failwith "JS only"
    let [<Global>] SVGFEBlendElement: obj = failwith "JS only"
    let [<Global>] SVGFEColorMatrixElement: obj = failwith "JS only"
    let [<Global>] SVGFEComponentTransferElement: obj = failwith "JS only"
    let [<Global>] SVGFECompositeElement: obj = failwith "JS only"
    let [<Global>] SVGFEConvolveMatrixElement: obj = failwith "JS only"
    let [<Global>] SVGFEDiffuseLightingElement: obj = failwith "JS only"
    let [<Global>] SVGFEDisplacementMapElement: obj = failwith "JS only"
    let [<Global>] SVGFEDistantLightElement: obj = failwith "JS only"
    let [<Global>] SVGFEFloodElement: obj = failwith "JS only"
    let [<Global>] SVGFEFuncAElement: obj = failwith "JS only"
    let [<Global>] SVGFEFuncBElement: obj = failwith "JS only"
    let [<Global>] SVGFEFuncGElement: obj = failwith "JS only"
    let [<Global>] SVGFEFuncRElement: obj = failwith "JS only"
    let [<Global>] SVGFEGaussianBlurElement: obj = failwith "JS only"
    let [<Global>] SVGFEImageElement: obj = failwith "JS only"
    let [<Global>] SVGFEMergeElement: obj = failwith "JS only"
    let [<Global>] SVGFEMergeNodeElement: obj = failwith "JS only"
    let [<Global>] SVGFEMorphologyElement: obj = failwith "JS only"
    let [<Global>] SVGFEOffsetElement: obj = failwith "JS only"
    let [<Global>] SVGFEPointLightElement: obj = failwith "JS only"
    let [<Global>] SVGFESpecularLightingElement: obj = failwith "JS only"
    let [<Global>] SVGFESpotLightElement: obj = failwith "JS only"
    let [<Global>] SVGFETileElement: obj = failwith "JS only"
    let [<Global>] SVGFETurbulenceElement: obj = failwith "JS only"
    let [<Global>] SVGFilterElement: obj = failwith "JS only"
    let [<Global>] SVGForeignObjectElement: obj = failwith "JS only"
    let [<Global>] SVGGElement: obj = failwith "JS only"
    let [<Global>] SVGGradientElement: obj = failwith "JS only"
    let [<Global>] SVGImageElement: obj = failwith "JS only"
    let [<Global>] SVGLength: obj = failwith "JS only"
    let [<Global>] SVGLengthList: obj = failwith "JS only"
    let [<Global>] SVGLineElement: obj = failwith "JS only"
    let [<Global>] SVGLinearGradientElement: obj = failwith "JS only"
    let [<Global>] SVGMarkerElement: obj = failwith "JS only"
    let [<Global>] SVGMaskElement: obj = failwith "JS only"
    let [<Global>] SVGMatrix: obj = failwith "JS only"
    let [<Global>] SVGMetadataElement: obj = failwith "JS only"
    let [<Global>] SVGNumber: obj = failwith "JS only"
    let [<Global>] SVGNumberList: obj = failwith "JS only"
    let [<Global>] SVGPathElement: obj = failwith "JS only"
    let [<Global>] SVGPathSeg: obj = failwith "JS only"
    let [<Global>] SVGPathSegArcAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegArcRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegClosePath: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoCubicAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoCubicRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoCubicSmoothAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoCubicSmoothRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoQuadraticAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoQuadraticRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoQuadraticSmoothAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegCurvetoQuadraticSmoothRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegLinetoAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegLinetoHorizontalAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegLinetoHorizontalRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegLinetoRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegLinetoVerticalAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegLinetoVerticalRel: obj = failwith "JS only"
    let [<Global>] SVGPathSegList: obj = failwith "JS only"
    let [<Global>] SVGPathSegMovetoAbs: obj = failwith "JS only"
    let [<Global>] SVGPathSegMovetoRel: obj = failwith "JS only"
    let [<Global>] SVGPatternElement: obj = failwith "JS only"
    let [<Global>] SVGPoint: obj = failwith "JS only"
    let [<Global>] SVGPointList: obj = failwith "JS only"
    let [<Global>] SVGPolygonElement: obj = failwith "JS only"
    let [<Global>] SVGPolylineElement: obj = failwith "JS only"
    let [<Global>] SVGPreserveAspectRatio: obj = failwith "JS only"
    let [<Global>] SVGRadialGradientElement: obj = failwith "JS only"
    let [<Global>] SVGRect: obj = failwith "JS only"
    let [<Global>] SVGRectElement: obj = failwith "JS only"
    let [<Global>] SVGSVGElement: obj = failwith "JS only"
    let [<Global>] SVGScriptElement: obj = failwith "JS only"
    let [<Global>] SVGStopElement: obj = failwith "JS only"
    let [<Global>] SVGStringList: obj = failwith "JS only"
    let [<Global>] SVGStyleElement: obj = failwith "JS only"
    let [<Global>] SVGSwitchElement: obj = failwith "JS only"
    let [<Global>] SVGSymbolElement: obj = failwith "JS only"
    let [<Global>] SVGTSpanElement: obj = failwith "JS only"
    let [<Global>] SVGTextContentElement: obj = failwith "JS only"
    let [<Global>] SVGTextElement: obj = failwith "JS only"
    let [<Global>] SVGTextPathElement: obj = failwith "JS only"
    let [<Global>] SVGTextPositioningElement: obj = failwith "JS only"
    let [<Global>] SVGTitleElement: obj = failwith "JS only"
    let [<Global>] SVGTransform: obj = failwith "JS only"
    let [<Global>] SVGTransformList: obj = failwith "JS only"
    let [<Global>] SVGUnitTypes: SVGUnitTypes = failwith "JS only"
    let [<Global>] SVGUseElement: obj = failwith "JS only"
    let [<Global>] SVGViewElement: obj = failwith "JS only"
    let [<Global>] SVGZoomAndPan: obj = failwith "JS only"
    let [<Global>] SVGZoomEvent: obj = failwith "JS only"
    let [<Global>] Screen: obj = failwith "JS only"
    let [<Global>] ScriptNotifyEvent: obj = failwith "JS only"
    let [<Global>] ScriptProcessorNode: obj = failwith "JS only"
    let [<Global>] Selection: obj = failwith "JS only"
    let [<Global>] SourceBuffer: obj = failwith "JS only"
    let [<Global>] SourceBufferList: obj = failwith "JS only"
    let [<Global>] StereoPannerNode: obj = failwith "JS only"
    let [<Global>] Storage: obj = failwith "JS only"
    let [<Global>] StorageEvent: obj = failwith "JS only"
    let [<Global>] StyleMedia: obj = failwith "JS only"
    let [<Global>] StyleSheet: obj = failwith "JS only"
    let [<Global>] StyleSheetList: obj = failwith "JS only"
    let [<Global>] StyleSheetPageList: obj = failwith "JS only"
    let [<Global>] SubtleCrypto: obj = failwith "JS only"
    let [<Global>] Text: obj = failwith "JS only"
    let [<Global>] TextEvent: obj = failwith "JS only"
    let [<Global>] TextMetrics: obj = failwith "JS only"
    let [<Global>] TextRange: obj = failwith "JS only"
    let [<Global>] TextRangeCollection: obj = failwith "JS only"
    let [<Global>] TextTrack: obj = failwith "JS only"
    let [<Global>] TextTrackCue: obj = failwith "JS only"
    let [<Global>] TextTrackCueList: obj = failwith "JS only"
    let [<Global>] TextTrackList: obj = failwith "JS only"
    let [<Global>] TimeRanges: obj = failwith "JS only"
    let [<Global>] Touch: obj = failwith "JS only"
    let [<Global>] TouchEvent: obj = failwith "JS only"
    let [<Global>] TouchList: obj = failwith "JS only"
    let [<Global>] TrackEvent: obj = failwith "JS only"
    let [<Global>] TransitionEvent: obj = failwith "JS only"
    let [<Global>] TreeWalker: obj = failwith "JS only"
    let [<Global>] UIEvent: obj = failwith "JS only"
    let [<Global>] URL: URL = failwith "JS only"
    let [<Global>] UnviewableContentIdentifiedEvent: obj = failwith "JS only"
    let [<Global>] ValidityState: obj = failwith "JS only"
    let [<Global>] VideoPlaybackQuality: obj = failwith "JS only"
    let [<Global>] VideoTrack: obj = failwith "JS only"
    let [<Global>] VideoTrackList: obj = failwith "JS only"
    let [<Global>] WEBGL_compressed_texture_s3tc: obj = failwith "JS only"
    let [<Global>] WEBGL_debug_renderer_info: obj = failwith "JS only"
    let [<Global>] WEBGL_depth_texture: obj = failwith "JS only"
    let [<Global>] WaveShaperNode: obj = failwith "JS only"
    let [<Global>] WebGLActiveInfo: obj = failwith "JS only"
    let [<Global>] WebGLBuffer: obj = failwith "JS only"
    let [<Global>] WebGLContextEvent: obj = failwith "JS only"
    let [<Global>] WebGLFramebuffer: obj = failwith "JS only"
    let [<Global>] WebGLObject: obj = failwith "JS only"
    let [<Global>] WebGLProgram: obj = failwith "JS only"
    let [<Global>] WebGLRenderbuffer: obj = failwith "JS only"
    let [<Global>] WebGLRenderingContext: obj = failwith "JS only"
    let [<Global>] WebGLShader: obj = failwith "JS only"
    let [<Global>] WebGLShaderPrecisionFormat: obj = failwith "JS only"
    let [<Global>] WebGLTexture: obj = failwith "JS only"
    let [<Global>] WebGLUniformLocation: obj = failwith "JS only"
    let [<Global>] WebKitCSSMatrix: obj = failwith "JS only"
    let [<Global>] WebKitPoint: obj = failwith "JS only"
    let [<Global>] WebSocket: obj = failwith "JS only"
    let [<Global>] WheelEvent: obj = failwith "JS only"
    let [<Global>] Window: obj = failwith "JS only"
    let [<Global>] Worker: obj = failwith "JS only"
    let [<Global>] XMLDocument: obj = failwith "JS only"
    let [<Global>] XMLHttpRequest: obj = failwith "JS only"
    let [<Global>] XMLHttpRequestUpload: obj = failwith "JS only"
    let [<Global>] XMLSerializer: obj = failwith "JS only"
    let [<Global>] XPathEvaluator: obj = failwith "JS only"
    let [<Global>] XPathExpression: obj = failwith "JS only"
    let [<Global>] XPathNSResolver: obj = failwith "JS only"
    let [<Global>] XPathResult: obj = failwith "JS only"
    let [<Global>] XSLTProcessor: obj = failwith "JS only"
    let [<Global>] HTMLTemplateElement: obj = failwith "JS only"
    let [<Global>] HTMLPictureElement: obj = failwith "JS only"
    let [<Global>] Audio: obj = failwith "JS only"
    let [<Global>] Image: obj = failwith "JS only"
    let [<Global>] Option: obj = failwith "JS only"
    let [<Global>] animationStartTime: float = failwith "JS only"
    let [<Global>] applicationCache: ApplicationCache = failwith "JS only"
    let [<Global>] clientInformation: Navigator = failwith "JS only"
    let [<Global>] closed: bool = failwith "JS only"
    let [<Global>] crypto: Crypto = failwith "JS only"
    let [<Global>] defaultStatus: string = failwith "JS only"
    let [<Global>] devicePixelRatio: float = failwith "JS only"
    let [<Global>] doNotTrack: string = failwith "JS only"
    let [<Global>] document: Document = failwith "JS only"
    let [<Global>] ``event``: Event = failwith "JS only"
    let [<Global>] ``external``: External = failwith "JS only"
    let [<Global>] frameElement: Element = failwith "JS only"
    let [<Global>] frames: Window = failwith "JS only"
    let [<Global>] history: History = failwith "JS only"
    let [<Global>] innerHeight: float = failwith "JS only"
    let [<Global>] innerWidth: float = failwith "JS only"
    let [<Global>] length: float = failwith "JS only"
    let [<Global>] location: Location = failwith "JS only"
    let [<Global>] locationbar: BarProp = failwith "JS only"
    let [<Global>] menubar: BarProp = failwith "JS only"
    let [<Global>] msAnimationStartTime: float = failwith "JS only"
    let [<Global>] name: string = failwith "JS only"
    let [<Global>] navigator: Navigator = failwith "JS only"
    let [<Global>] offscreenBuffering: U2<string, bool> = failwith "JS only"
    let [<Global>] onabort: Func<Event, obj> = failwith "JS only"
    let [<Global>] onafterprint: Func<Event, obj> = failwith "JS only"
    let [<Global>] onbeforeprint: Func<Event, obj> = failwith "JS only"
    let [<Global>] onbeforeunload: Func<BeforeUnloadEvent, obj> = failwith "JS only"
    let [<Global>] onblur: Func<FocusEvent, obj> = failwith "JS only"
    let [<Global>] oncanplay: Func<Event, obj> = failwith "JS only"
    let [<Global>] oncanplaythrough: Func<Event, obj> = failwith "JS only"
    let [<Global>] onchange: Func<Event, obj> = failwith "JS only"
    let [<Global>] onclick: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] oncompassneedscalibration: Func<Event, obj> = failwith "JS only"
    let [<Global>] oncontextmenu: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] ondblclick: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] ondevicemotion: Func<DeviceMotionEvent, obj> = failwith "JS only"
    let [<Global>] ondeviceorientation: Func<DeviceOrientationEvent, obj> = failwith "JS only"
    let [<Global>] ondrag: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondragend: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondragenter: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondragleave: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondragover: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondragstart: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondrop: Func<DragEvent, obj> = failwith "JS only"
    let [<Global>] ondurationchange: Func<Event, obj> = failwith "JS only"
    let [<Global>] onemptied: Func<Event, obj> = failwith "JS only"
    let [<Global>] onended: Func<Event, obj> = failwith "JS only"
    let [<Global>] onerror: ErrorEventHandler = failwith "JS only"
    let [<Global>] onfocus: Func<FocusEvent, obj> = failwith "JS only"
    let [<Global>] onhashchange: Func<HashChangeEvent, obj> = failwith "JS only"
    let [<Global>] oninput: Func<Event, obj> = failwith "JS only"
    let [<Global>] onkeydown: Func<KeyboardEvent, obj> = failwith "JS only"
    let [<Global>] onkeypress: Func<KeyboardEvent, obj> = failwith "JS only"
    let [<Global>] onkeyup: Func<KeyboardEvent, obj> = failwith "JS only"
    let [<Global>] onload: Func<Event, obj> = failwith "JS only"
    let [<Global>] onloadeddata: Func<Event, obj> = failwith "JS only"
    let [<Global>] onloadedmetadata: Func<Event, obj> = failwith "JS only"
    let [<Global>] onloadstart: Func<Event, obj> = failwith "JS only"
    let [<Global>] onmessage: Func<MessageEvent, obj> = failwith "JS only"
    let [<Global>] onmousedown: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmouseenter: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmouseleave: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmousemove: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmouseout: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmouseover: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmouseup: Func<MouseEvent, obj> = failwith "JS only"
    let [<Global>] onmousewheel: Func<MouseWheelEvent, obj> = failwith "JS only"
    let [<Global>] onmsgesturechange: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmsgesturedoubletap: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmsgestureend: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmsgesturehold: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmsgesturestart: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmsgesturetap: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmsinertiastart: Func<MSGestureEvent, obj> = failwith "JS only"
    let [<Global>] onmspointercancel: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointerdown: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointerenter: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointerleave: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointermove: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointerout: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointerover: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onmspointerup: Func<MSPointerEvent, obj> = failwith "JS only"
    let [<Global>] onoffline: Func<Event, obj> = failwith "JS only"
    let [<Global>] ononline: Func<Event, obj> = failwith "JS only"
    let [<Global>] onorientationchange: Func<Event, obj> = failwith "JS only"
    let [<Global>] onpagehide: Func<PageTransitionEvent, obj> = failwith "JS only"
    let [<Global>] onpageshow: Func<PageTransitionEvent, obj> = failwith "JS only"
    let [<Global>] onpause: Func<Event, obj> = failwith "JS only"
    let [<Global>] onplay: Func<Event, obj> = failwith "JS only"
    let [<Global>] onplaying: Func<Event, obj> = failwith "JS only"
    let [<Global>] onpopstate: Func<PopStateEvent, obj> = failwith "JS only"
    let [<Global>] onprogress: Func<ProgressEvent, obj> = failwith "JS only"
    let [<Global>] onratechange: Func<Event, obj> = failwith "JS only"
    let [<Global>] onreadystatechange: Func<ProgressEvent, obj> = failwith "JS only"
    let [<Global>] onreset: Func<Event, obj> = failwith "JS only"
    let [<Global>] onresize: Func<UIEvent, obj> = failwith "JS only"
    let [<Global>] onscroll: Func<UIEvent, obj> = failwith "JS only"
    let [<Global>] onseeked: Func<Event, obj> = failwith "JS only"
    let [<Global>] onseeking: Func<Event, obj> = failwith "JS only"
    let [<Global>] onselect: Func<UIEvent, obj> = failwith "JS only"
    let [<Global>] onstalled: Func<Event, obj> = failwith "JS only"
    let [<Global>] onstorage: Func<StorageEvent, obj> = failwith "JS only"
    let [<Global>] onsubmit: Func<Event, obj> = failwith "JS only"
    let [<Global>] onsuspend: Func<Event, obj> = failwith "JS only"
    let [<Global>] ontimeupdate: Func<Event, obj> = failwith "JS only"
    let [<Global>] ontouchcancel: obj = failwith "JS only"
    let [<Global>] ontouchend: obj = failwith "JS only"
    let [<Global>] ontouchmove: obj = failwith "JS only"
    let [<Global>] ontouchstart: obj = failwith "JS only"
    let [<Global>] onunload: Func<Event, obj> = failwith "JS only"
    let [<Global>] onvolumechange: Func<Event, obj> = failwith "JS only"
    let [<Global>] onwaiting: Func<Event, obj> = failwith "JS only"
    let [<Global>] opener: Window = failwith "JS only"
    let [<Global>] orientation: U2<string, float> = failwith "JS only"
    let [<Global>] outerHeight: float = failwith "JS only"
    let [<Global>] outerWidth: float = failwith "JS only"
    let [<Global>] pageXOffset: float = failwith "JS only"
    let [<Global>] pageYOffset: float = failwith "JS only"
    let [<Global>] parent: Window = failwith "JS only"
    let [<Global>] performance: Performance = failwith "JS only"
    let [<Global>] personalbar: BarProp = failwith "JS only"
    let [<Global>] screen: Screen = failwith "JS only"
    let [<Global>] screenLeft: float = failwith "JS only"
    let [<Global>] screenTop: float = failwith "JS only"
    let [<Global>] screenX: float = failwith "JS only"
    let [<Global>] screenY: float = failwith "JS only"
    let [<Global>] scrollX: float = failwith "JS only"
    let [<Global>] scrollY: float = failwith "JS only"
    let [<Global>] scrollbars: BarProp = failwith "JS only"
    let [<Global>] self: Window = failwith "JS only"
    let [<Global>] status: string = failwith "JS only"
    let [<Global>] statusbar: BarProp = failwith "JS only"
    let [<Global>] styleMedia: StyleMedia = failwith "JS only"
    let [<Global>] toolbar: BarProp = failwith "JS only"
    let [<Global>] top: Window = failwith "JS only"
    let [<Global>] window: Window = failwith "JS only"
    let [<Global>] URL: URL = failwith "JS only"
    let [<Global>] sessionStorage: Storage = failwith "JS only"
    let [<Global>] localStorage: Storage = failwith "JS only"
    let [<Global>] console: Console = failwith "JS only"
    let [<Global>] onpointercancel: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointerdown: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointerenter: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointerleave: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointermove: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointerout: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointerover: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onpointerup: Func<PointerEvent, obj> = failwith "JS only"
    let [<Global>] onwheel: Func<WheelEvent, obj> = failwith "JS only"
    let [<Global>] indexedDB: IDBFactory = failwith "JS only"
    let [<Global>] msIndexedDB: IDBFactory = failwith "JS only"

module Intl =
    type CollatorOptions =
        abstract usage: string option with get, set
        abstract localeMatcher: string option with get, set
        abstract numeric: bool option with get, set
        abstract caseFirst: string option with get, set
        abstract sensitivity: string option with get, set
        abstract ignorePunctuation: bool option with get, set

    and ResolvedCollatorOptions =
        abstract locale: string with get, set
        abstract usage: string with get, set
        abstract sensitivity: string with get, set
        abstract ignorePunctuation: bool with get, set
        abstract collation: string with get, set
        abstract caseFirst: string with get, set
        abstract numeric: bool with get, set

    and Collator =
        abstract compare: x: string * y: string -> float
        abstract resolvedOptions: unit -> ResolvedCollatorOptions

    and NumberFormatOptions =
        abstract localeMatcher: string option with get, set
        abstract style: string option with get, set
        abstract currency: string option with get, set
        abstract currencyDisplay: string option with get, set
        abstract useGrouping: bool option with get, set
        abstract minimumIntegerDigits: float option with get, set
        abstract minimumFractionDigits: float option with get, set
        abstract maximumFractionDigits: float option with get, set
        abstract minimumSignificantDigits: float option with get, set
        abstract maximumSignificantDigits: float option with get, set

    and ResolvedNumberFormatOptions =
        abstract locale: string with get, set
        abstract numberingSystem: string with get, set
        abstract style: string with get, set
        abstract currency: string option with get, set
        abstract currencyDisplay: string option with get, set
        abstract minimumIntegerDigits: float with get, set
        abstract minimumFractionDigits: float with get, set
        abstract maximumFractionDigits: float with get, set
        abstract minimumSignificantDigits: float option with get, set
        abstract maximumSignificantDigits: float option with get, set
        abstract useGrouping: bool with get, set

    and NumberFormat =
        abstract format: value: float -> string
        abstract resolvedOptions: unit -> ResolvedNumberFormatOptions

    and DateTimeFormatOptions =
        abstract localeMatcher: string option with get, set
        abstract weekday: string option with get, set
        abstract era: string option with get, set
        abstract year: string option with get, set
        abstract month: string option with get, set
        abstract day: string option with get, set
        abstract hour: string option with get, set
        abstract minute: string option with get, set
        abstract second: string option with get, set
        abstract timeZoneName: string option with get, set
        abstract formatMatcher: string option with get, set
        abstract hour12: bool option with get, set
        abstract timeZone: string option with get, set

    and ResolvedDateTimeFormatOptions =
        abstract locale: string with get, set
        abstract calendar: string with get, set
        abstract numberingSystem: string with get, set
        abstract timeZone: string with get, set
        abstract hour12: bool option with get, set
        abstract weekday: string option with get, set
        abstract era: string option with get, set
        abstract year: string option with get, set
        abstract month: string option with get, set
        abstract day: string option with get, set
        abstract hour: string option with get, set
        abstract minute: string option with get, set
        abstract second: string option with get, set
        abstract timeZoneName: string option with get, set

    and DateTimeFormat =
        abstract format: ?date: U2<DateTime, float> -> string
        abstract resolvedOptions: unit -> ResolvedDateTimeFormatOptions

    type Globals =
        abstract Collator: obj with get, set
        abstract NumberFormat: obj with get, set
        abstract DateTimeFormat: obj with get, set

    let [<Import("Intl")>] Globals: Globals = failwith "JS only"

