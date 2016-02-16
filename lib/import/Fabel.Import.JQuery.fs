namespace Fabel.Import
open System
    
module Unknown =
    type private ImportAttribute(path) =
        inherit System.Attribute()

    type JQueryAjaxSettings =
        abstract accepts: obj option with get, set
        abstract async: bool option with get, set
        abstract cache: bool option with get, set
        abstract contents: obj option with get, set
        abstract contentType: obj option with get, set
        abstract context: obj option with get, set
        abstract converters: obj option with get, set
        abstract crossDomain: bool option with get, set
        abstract data: obj option with get, set
        abstract dataType: string option with get, set
        abstract global: bool option with get, set
        abstract headers: obj option with get, set
        abstract ifModified: bool option with get, set
        abstract isLocal: bool option with get, set
        abstract jsonp: obj option with get, set
        abstract jsonpCallback: obj option with get, set
        abstract method: string option with get, set
        abstract mimeType: string option with get, set
        abstract password: string option with get, set
        abstract processData: bool option with get, set
        abstract scriptCharset: string option with get, set
        abstract statusCode: obj option with get, set
        abstract timeout: float option with get, set
        abstract traditional: bool option with get, set
        abstract type: string option with get, set
        abstract url: string option with get, set
        abstract username: string option with get, set
        abstract xhr: obj option with get, set
        abstract xhrFields: obj option with get, set
        abstract beforeSend: jqXHR: JQueryXHR * settings: JQueryAjaxSettings -> obj
        abstract complete: jqXHR: JQueryXHR * textStatus: string -> obj
        abstract dataFilter: data: obj * ty: obj -> obj
        abstract error: jqXHR: JQueryXHR * textStatus: string * errorThrown: string -> obj
        abstract success: data: obj * textStatus: string * jqXHR: JQueryXHR -> obj

    type JQueryXHR =
        abstract responseJSON: obj option with get, set
        abstract overrideMimeType: mimeType: string -> obj
        abstract abort: ?statusText: string -> unit
        abstract then: doneCallback: Func<obj, string, JQueryXHR, unit> * ?failCallback: Func<JQueryXHR, string, obj, unit> -> JQueryPromise
        abstract error: xhr: JQueryXHR * textStatus: string * errorThrown: string -> unit

    type JQueryCallback =
        abstract add: callbacks: Function -> JQueryCallback
        abstract add: callbacks: ResizeArray<Function> -> JQueryCallback
        abstract disable: unit -> JQueryCallback
        abstract disabled: unit -> bool
        abstract empty: unit -> JQueryCallback
        abstract fire: [<ParamArray>] arguments: obj[] -> JQueryCallback
        abstract fired: unit -> bool
        abstract fireWith: ?context: obj * ?args: ResizeArray<obj> -> JQueryCallback
        abstract has: callback: Function -> bool
        abstract lock: unit -> JQueryCallback
        abstract locked: unit -> bool
        abstract remove: callbacks: Function -> JQueryCallback
        abstract remove: callbacks: ResizeArray<Function> -> JQueryCallback

    type JQueryGenericPromise =
        abstract then: doneFilter: Func<T, ResizeArray<obj>, obj> * ?failFilter: Func<ResizeArray<obj>, obj> * ?progressFilter: Func<ResizeArray<obj>, obj> -> JQueryPromise
        abstract then: doneFilter: Func<T, ResizeArray<obj>, unit> * ?failFilter: Func<ResizeArray<obj>, obj> * ?progressFilter: Func<ResizeArray<obj>, obj> -> JQueryPromise

    type JQueryPromiseCallback =
        interface end

    type JQueryPromiseOperator =
        interface end

    type JQueryPromise =
        abstract state: unit -> string
        abstract always: ?alwaysCallback1: obj * [<ParamArray>] alwaysCallbacksN: unknown -> JQueryPromise
        abstract done: ?doneCallback1: obj * [<ParamArray>] doneCallbackN: unknown -> JQueryPromise
        abstract fail: ?failCallback1: obj * [<ParamArray>] failCallbacksN: unknown -> JQueryPromise
        abstract progress: ?progressCallback1: obj * [<ParamArray>] progressCallbackN: unknown -> JQueryPromise
        abstract pipe: ?doneFilter: Func<obj, obj> * ?failFilter: Func<obj, obj> * ?progressFilter: Func<obj, obj> -> JQueryPromise

    type JQueryDeferred =
        abstract state: unit -> string
        abstract always: ?alwaysCallback1: obj * [<ParamArray>] alwaysCallbacksN: unknown -> JQueryDeferred
        abstract done: ?doneCallback1: obj * [<ParamArray>] doneCallbackN: unknown -> JQueryDeferred
        abstract fail: ?failCallback1: obj * [<ParamArray>] failCallbacksN: unknown -> JQueryDeferred
        abstract progress: ?progressCallback1: obj * [<ParamArray>] progressCallbackN: unknown -> JQueryDeferred
        abstract notify: ?value: obj * [<ParamArray>] args: obj[] -> JQueryDeferred
        abstract notifyWith: context: obj * ?value: ResizeArray<obj> -> JQueryDeferred
        abstract reject: ?value: obj * [<ParamArray>] args: obj[] -> JQueryDeferred
        abstract rejectWith: context: obj * ?value: ResizeArray<obj> -> JQueryDeferred
        abstract resolve: ?value: T * [<ParamArray>] args: obj[] -> JQueryDeferred
        abstract resolveWith: context: obj * ?value: ResizeArray<T> -> JQueryDeferred
        abstract promise: ?target: obj -> JQueryPromise
        abstract pipe: ?doneFilter: Func<obj, obj> * ?failFilter: Func<obj, obj> * ?progressFilter: Func<obj, obj> -> JQueryPromise

    type BaseJQueryEventObject =
        abstract data: obj with get, set
        abstract delegateTarget: Element with get, set
        abstract namespace: string with get, set
        abstract originalEvent: Event with get, set
        abstract relatedTarget: Element with get, set
        abstract result: obj with get, set
        abstract target: Element with get, set
        abstract pageX: float with get, set
        abstract pageY: float with get, set
        abstract which: float with get, set
        abstract metaKey: bool with get, set
        abstract isDefaultPrevented: unit -> bool
        abstract isImmediatePropagationStopped: unit -> bool
        abstract isPropagationStopped: unit -> bool
        abstract preventDefault: unit -> obj
        abstract stopImmediatePropagation: unit -> unit
        abstract stopPropagation: unit -> unit

    type JQueryInputEventObject =
        abstract altKey: bool with get, set
        abstract ctrlKey: bool with get, set
        abstract metaKey: bool with get, set
        abstract shiftKey: bool with get, set

    type JQueryMouseEventObject =
        abstract button: float with get, set
        abstract clientX: float with get, set
        abstract clientY: float with get, set
        abstract offsetX: float with get, set
        abstract offsetY: float with get, set
        abstract pageX: float with get, set
        abstract pageY: float with get, set
        abstract screenX: float with get, set
        abstract screenY: float with get, set

    type JQueryKeyEventObject =
        abstract char: obj with get, set
        abstract charCode: float with get, set
        abstract key: obj with get, set
        abstract keyCode: float with get, set

    type JQueryEventObject =
        interface end

    type JQuerySupport =
        abstract ajax: bool option with get, set
        abstract boxModel: bool option with get, set
        abstract changeBubbles: bool option with get, set
        abstract checkClone: bool option with get, set
        abstract checkOn: bool option with get, set
        abstract cors: bool option with get, set
        abstract cssFloat: bool option with get, set
        abstract hrefNormalized: bool option with get, set
        abstract htmlSerialize: bool option with get, set
        abstract leadingWhitespace: bool option with get, set
        abstract noCloneChecked: bool option with get, set
        abstract noCloneEvent: bool option with get, set
        abstract opacity: bool option with get, set
        abstract optDisabled: bool option with get, set
        abstract optSelected: bool option with get, set
        abstract style: bool option with get, set
        abstract submitBubbles: bool option with get, set
        abstract tbody: bool option with get, set
        abstract scriptEval: unit -> bool

    type JQueryParam =
        interface end

    type JQueryEventConstructor =
        abstract createNew: name: string * ?eventProperties: obj -> JQueryEventObject

    type JQueryCoordinates =
        abstract left: float with get, set
        abstract top: float with get, set

    type JQuerySerializeArrayElement =
        abstract name: string with get, set
        abstract value: string with get, set

    type JQueryAnimationOptions =
        abstract duration: obj option with get, set
        abstract easing: string option with get, set
        abstract complete: Function option with get, set
        abstract step: Func<float, obj, obj> option with get, set
        abstract progress: Func<JQueryPromise, float, float, obj> option with get, set
        abstract start: Func<JQueryPromise, obj> option with get, set
        abstract done: Func<JQueryPromise, bool, obj> option with get, set
        abstract fail: Func<JQueryPromise, bool, obj> option with get, set
        abstract always: Func<JQueryPromise, bool, obj> option with get, set
        abstract queue: obj option with get, set
        abstract specialEasing: Object option with get, set

    type JQueryEasingFunction =
        interface end

    type JQueryEasingFunctions =
        abstract linear: JQueryEasingFunction with get, set
        abstract swing: JQueryEasingFunction with get, set

    type JQueryStatic =
        abstract ajaxSettings: JQueryAjaxSettings with get, set
        abstract param: JQueryParam with get, set
        abstract cssHooks: obj with get, set
        abstract cssNumber: obj with get, set
        abstract easing: JQueryEasingFunctions with get, set
        abstract fx: obj with get, set
        abstract Event: JQueryEventConstructor with get, set
        abstract expr: obj with get, set
        abstract fn: obj with get, set
        abstract isReady: bool with get, set
        abstract support: JQuerySupport with get, set
        abstract ajax: settings: JQueryAjaxSettings -> JQueryXHR
        abstract ajax: url: string * ?settings: JQueryAjaxSettings -> JQueryXHR
        abstract ajaxPrefilter: dataTypes: string * handler: Func<obj, JQueryAjaxSettings, JQueryXHR, obj> -> unit
        abstract ajaxPrefilter: handler: Func<obj, JQueryAjaxSettings, JQueryXHR, obj> -> unit
        abstract ajaxSetup: options: JQueryAjaxSettings -> unit
        abstract get: url: string * ?success: Func<obj, string, JQueryXHR, obj> * ?dataType: string -> JQueryXHR
        abstract get: url: string * ?data: obj * ?success: Func<obj, string, JQueryXHR, obj> * ?dataType: string -> JQueryXHR
        abstract getJSON: url: string * ?success: Func<obj, string, JQueryXHR, obj> -> JQueryXHR
        abstract getJSON: url: string * ?data: obj * ?success: Func<obj, string, JQueryXHR, obj> -> JQueryXHR
        abstract getScript: url: string * ?success: Func<string, string, JQueryXHR, obj> -> JQueryXHR
        abstract post: url: string * ?success: Func<obj, string, JQueryXHR, obj> * ?dataType: string -> JQueryXHR
        abstract post: url: string * ?data: obj * ?success: Func<obj, string, JQueryXHR, obj> * ?dataType: string -> JQueryXHR
        abstract Callbacks: ?flags: string -> JQueryCallback
        abstract holdReady: hold: bool -> unit
        abstract noConflict: ?removeAll: bool -> Object
        abstract when: [<ParamArray>] deferreds: unknown -> JQueryPromise
        abstract data: element: Element * key: string * value: T -> T
        abstract data: element: Element * key: string -> obj
        abstract data: element: Element -> obj
        abstract dequeue: element: Element * ?queueName: string -> unit
        abstract hasData: element: Element -> bool
        abstract queue: element: Element * ?queueName: string -> ResizeArray<obj>
        abstract queue: element: Element * queueName: string * newQueue: ResizeArray<Function> -> JQuery
        abstract queue: element: Element * queueName: string * callback: Function -> JQuery
        abstract removeData: element: Element * ?name: string -> JQuery
        abstract Deferred: ?beforeStart: Func<JQueryDeferred, obj> -> JQueryDeferred
        abstract proxy: fnction: Func<ResizeArray<obj>, obj> * context: Object * [<ParamArray>] additionalArguments: obj[] -> obj
        abstract proxy: context: Object * name: string * [<ParamArray>] additionalArguments: obj[] -> obj
        abstract error: message: obj -> JQuery
        abstract contains: container: Element * contained: Element -> bool
        abstract each: collection: ResizeArray<T> * callback: Func<float, T, obj> -> obj
        abstract each: collection: obj * callback: Func<obj, obj, obj> -> obj
        abstract extend: target: obj * ?object1: obj * [<ParamArray>] objectN: obj[] -> obj
        abstract extend: deep: bool * target: obj * ?object1: obj * [<ParamArray>] objectN: obj[] -> obj
        abstract globalEval: code: string -> obj
        abstract grep: array: ResizeArray<T> * func: Func<T, float, bool> * ?invert: bool -> ResizeArray<T>
        abstract inArray: value: T * array: ResizeArray<T> * ?fromIndex: float -> float
        abstract isArray: obj: obj -> bool
        abstract isEmptyObject: obj: obj -> bool
        abstract isFunction: obj: obj -> bool
        abstract isNumeric: value: obj -> bool
        abstract isPlainObject: obj: obj -> bool
        abstract isWindow: obj: obj -> bool
        abstract isXMLDoc: node: Node -> bool
        abstract makeArray: obj: obj -> ResizeArray<obj>
        abstract map: array: ResizeArray<T> * callback: Func<T, float, U> -> ResizeArray<U>
        abstract map: arrayOrObject: obj * callback: Func<obj, obj, obj> -> obj
        abstract merge: first: ResizeArray<T> * second: ResizeArray<T> -> ResizeArray<T>
        abstract noop: unit -> obj
        abstract now: unit -> float
        abstract parseJSON: json: string -> obj
        abstract parseXML: data: string -> XMLDocument
        abstract trim: str: string -> string
        abstract type: obj: obj -> string
        abstract unique: array: ResizeArray<Element> -> ResizeArray<Element>
        abstract parseHTML: data: string * ?context: HTMLElement * ?keepScripts: bool -> ResizeArray<obj>
        abstract parseHTML: data: string * ?context: Document * ?keepScripts: bool -> ResizeArray<obj>

    type JQuery =
        abstract context: Element with get, set
        abstract jquery: string with get, set
        abstract length: float with get, set
        abstract selector: string with get, set
        abstract ajaxComplete: handler: Func<JQueryEventObject, XMLHttpRequest, obj, obj> -> JQuery
        abstract ajaxError: handler: Func<JQueryEventObject, JQueryXHR, JQueryAjaxSettings, obj, obj> -> JQuery
        abstract ajaxSend: handler: Func<JQueryEventObject, JQueryXHR, JQueryAjaxSettings, obj> -> JQuery
        abstract ajaxStart: handler: Func<obj> -> JQuery
        abstract ajaxStop: handler: Func<obj> -> JQuery
        abstract ajaxSuccess: handler: Func<JQueryEventObject, XMLHttpRequest, JQueryAjaxSettings, obj> -> JQuery
        abstract load: url: string * ?data: obj * ?complete: Func<string, string, XMLHttpRequest, obj> -> JQuery
        abstract serialize: unit -> string
        abstract serializeArray: unit -> ResizeArray<JQuerySerializeArrayElement>
        abstract addClass: className: string -> JQuery
        abstract addClass: func: Func<float, string, string> -> JQuery
        abstract addBack: ?selector: string -> JQuery
        abstract attr: attributeName: string -> string
        abstract attr: attributeName: string * value: obj -> JQuery
        abstract attr: attributeName: string * func: Func<float, string, obj> -> JQuery
        abstract attr: attributes: Object -> JQuery
        abstract hasClass: className: string -> bool
        abstract html: unit -> string
        abstract html: htmlString: string -> JQuery
        abstract html: func: Func<float, string, string> -> JQuery
        abstract prop: propertyName: string -> obj
        abstract prop: propertyName: string * value: obj -> JQuery
        abstract prop: properties: Object -> JQuery
        abstract prop: propertyName: string * func: Func<float, obj, obj> -> JQuery
        abstract removeAttr: attributeName: string -> JQuery
        abstract removeClass: ?className: string -> JQuery
        abstract removeClass: func: Func<float, string, string> -> JQuery
        abstract removeProp: propertyName: string -> JQuery
        abstract toggleClass: className: string * ?swtch: bool -> JQuery
        abstract toggleClass: ?swtch: bool -> JQuery
        abstract toggleClass: func: Func<float, string, bool, string> * ?swtch: bool -> JQuery
        abstract val: unit -> obj
        abstract val: value: obj -> JQuery
        abstract val: func: Func<float, string, string> -> JQuery
        abstract css: propertyName: string -> string
        abstract css: propertyName: string * value: obj -> JQuery
        abstract css: propertyName: string * value: Func<float, string, obj> -> JQuery
        abstract css: properties: Object -> JQuery
        abstract height: unit -> float
        abstract height: value: obj -> JQuery
        abstract height: func: Func<float, float, obj> -> JQuery
        abstract innerHeight: unit -> float
        abstract innerHeight: height: obj -> JQuery
        abstract innerWidth: unit -> float
        abstract innerWidth: width: obj -> JQuery
        abstract offset: unit -> JQueryCoordinates
        abstract offset: coordinates: JQueryCoordinates -> JQuery
        abstract offset: func: Func<float, JQueryCoordinates, JQueryCoordinates> -> JQuery
        abstract outerHeight: ?includeMargin: bool -> float
        abstract outerHeight: height: obj -> JQuery
        abstract outerWidth: ?includeMargin: bool -> float
        abstract outerWidth: width: obj -> JQuery
        abstract position: unit -> JQueryCoordinates
        abstract scrollLeft: unit -> float
        abstract scrollLeft: value: float -> JQuery
        abstract scrollTop: unit -> float
        abstract scrollTop: value: float -> JQuery
        abstract width: unit -> float
        abstract width: value: obj -> JQuery
        abstract width: func: Func<float, float, obj> -> JQuery
        abstract clearQueue: ?queueName: string -> JQuery
        abstract data: key: string * value: obj -> JQuery
        abstract data: key: string -> obj
        abstract data: obj: obj -> JQuery
        abstract data: unit -> obj
        abstract dequeue: ?queueName: string -> JQuery
        abstract removeData: name: string -> JQuery
        abstract removeData: list: ResizeArray<string> -> JQuery
        abstract removeData: unit -> JQuery
        abstract promise: ?type: string * ?target: Object -> JQueryPromise
        abstract animate: properties: Object * ?duration: obj * ?complete: Function -> JQuery
        abstract animate: properties: Object * ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract animate: properties: Object * options: JQueryAnimationOptions -> JQuery
        abstract delay: duration: float * ?queueName: string -> JQuery
        abstract fadeIn: ?duration: obj * ?complete: Function -> JQuery
        abstract fadeIn: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract fadeIn: options: JQueryAnimationOptions -> JQuery
        abstract fadeOut: ?duration: obj * ?complete: Function -> JQuery
        abstract fadeOut: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract fadeOut: options: JQueryAnimationOptions -> JQuery
        abstract fadeTo: duration: obj * opacity: float * ?complete: Function -> JQuery
        abstract fadeTo: duration: obj * opacity: float * ?easing: string * ?complete: Function -> JQuery
        abstract fadeToggle: ?duration: obj * ?complete: Function -> JQuery
        abstract fadeToggle: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract fadeToggle: options: JQueryAnimationOptions -> JQuery
        abstract finish: ?queue: string -> JQuery
        abstract hide: ?duration: obj * ?complete: Function -> JQuery
        abstract hide: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract hide: options: JQueryAnimationOptions -> JQuery
        abstract show: ?duration: obj * ?complete: Function -> JQuery
        abstract show: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract show: options: JQueryAnimationOptions -> JQuery
        abstract slideDown: ?duration: obj * ?complete: Function -> JQuery
        abstract slideDown: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract slideDown: options: JQueryAnimationOptions -> JQuery
        abstract slideToggle: ?duration: obj * ?complete: Function -> JQuery
        abstract slideToggle: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract slideToggle: options: JQueryAnimationOptions -> JQuery
        abstract slideUp: ?duration: obj * ?complete: Function -> JQuery
        abstract slideUp: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract slideUp: options: JQueryAnimationOptions -> JQuery
        abstract stop: ?clearQueue: bool * ?jumpToEnd: bool -> JQuery
        abstract stop: ?queue: string * ?clearQueue: bool * ?jumpToEnd: bool -> JQuery
        abstract toggle: ?duration: obj * ?complete: Function -> JQuery
        abstract toggle: ?duration: obj * ?easing: string * ?complete: Function -> JQuery
        abstract toggle: options: JQueryAnimationOptions -> JQuery
        abstract toggle: showOrHide: bool -> JQuery
        abstract bind: eventType: string * eventData: obj * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract bind: eventType: string * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract bind: eventType: string * eventData: obj * preventBubble: bool -> JQuery
        abstract bind: eventType: string * preventBubble: bool -> JQuery
        abstract bind: events: obj -> JQuery
        abstract blur: unit -> JQuery
        abstract blur: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract blur: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract change: unit -> JQuery
        abstract change: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract change: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract click: unit -> JQuery
        abstract click: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract click: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract dblclick: unit -> JQuery
        abstract dblclick: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract dblclick: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract delegate: selector: obj * eventType: string * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract delegate: selector: obj * eventType: string * eventData: obj * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract focus: unit -> JQuery
        abstract focus: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract focus: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract focusin: unit -> JQuery
        abstract focusin: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract focusin: eventData: Object * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract focusout: unit -> JQuery
        abstract focusout: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract focusout: eventData: Object * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract hover: handlerIn: Func<JQueryEventObject, obj> * handlerOut: Func<JQueryEventObject, obj> -> JQuery
        abstract hover: handlerInOut: Func<JQueryEventObject, obj> -> JQuery
        abstract keydown: unit -> JQuery
        abstract keydown: handler: Func<JQueryKeyEventObject, obj> -> JQuery
        abstract keydown: ?eventData: obj * ?handler: Func<JQueryKeyEventObject, obj> -> JQuery
        abstract keypress: unit -> JQuery
        abstract keypress: handler: Func<JQueryKeyEventObject, obj> -> JQuery
        abstract keypress: ?eventData: obj * ?handler: Func<JQueryKeyEventObject, obj> -> JQuery
        abstract keyup: unit -> JQuery
        abstract keyup: handler: Func<JQueryKeyEventObject, obj> -> JQuery
        abstract keyup: ?eventData: obj * ?handler: Func<JQueryKeyEventObject, obj> -> JQuery
        abstract load: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract load: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract mousedown: unit -> JQuery
        abstract mousedown: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mousedown: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseenter: unit -> JQuery
        abstract mouseenter: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseenter: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseleave: unit -> JQuery
        abstract mouseleave: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseleave: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mousemove: unit -> JQuery
        abstract mousemove: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mousemove: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseout: unit -> JQuery
        abstract mouseout: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseout: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseover: unit -> JQuery
        abstract mouseover: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseover: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseup: unit -> JQuery
        abstract mouseup: handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract mouseup: eventData: Object * handler: Func<JQueryMouseEventObject, obj> -> JQuery
        abstract off: unit -> JQuery
        abstract off: events: string * ?selector: string * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract off: events: string * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract off: events: obj * ?selector: string -> JQuery
        abstract on: events: string * handler: Func<JQueryEventObject, ResizeArray<obj>, obj> -> JQuery
        abstract on: events: string * data: obj * handler: Func<JQueryEventObject, ResizeArray<obj>, obj> -> JQuery
        abstract on: events: string * selector: string * handler: Func<JQueryEventObject, ResizeArray<obj>, obj> -> JQuery
        abstract on: events: string * selector: string * data: obj * handler: Func<JQueryEventObject, ResizeArray<obj>, obj> -> JQuery
        abstract on: events: obj * ?selector: string * ?data: obj -> JQuery
        abstract on: events: obj * ?data: obj -> JQuery
        abstract one: events: string * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract one: events: string * data: Object * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract one: events: string * selector: string * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract one: events: string * selector: string * data: obj * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract one: events: obj * ?selector: string * ?data: obj -> JQuery
        abstract one: events: obj * ?data: obj -> JQuery
        abstract ready: handler: Func<JQueryStatic, obj> -> JQuery
        abstract resize: unit -> JQuery
        abstract resize: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract resize: eventData: Object * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract scroll: unit -> JQuery
        abstract scroll: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract scroll: eventData: Object * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract select: unit -> JQuery
        abstract select: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract select: eventData: Object * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract submit: unit -> JQuery
        abstract submit: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract submit: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract trigger: eventType: string * ?extraParameters: obj -> JQuery
        abstract trigger: event: JQueryEventObject * ?extraParameters: obj -> JQuery
        abstract triggerHandler: eventType: string * [<ParamArray>] extraParameters: obj[] -> Object
        abstract triggerHandler: event: JQueryEventObject * [<ParamArray>] extraParameters: obj[] -> Object
        abstract unbind: ?eventType: string * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract unbind: eventType: string * fls: bool -> JQuery
        abstract unbind: evt: obj -> JQuery
        abstract undelegate: unit -> JQuery
        abstract undelegate: selector: string * eventType: string * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract undelegate: selector: string * events: Object -> JQuery
        abstract undelegate: namespace: string -> JQuery
        abstract unload: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract unload: ?eventData: obj * ?handler: Func<JQueryEventObject, obj> -> JQuery
        abstract error: handler: Func<JQueryEventObject, obj> -> JQuery
        abstract error: eventData: obj * handler: Func<JQueryEventObject, obj> -> JQuery
        abstract pushStack: elements: ResizeArray<obj> -> JQuery
        abstract pushStack: elements: ResizeArray<obj> * name: string * arguments: ResizeArray<obj> -> JQuery
        abstract after: content1: obj * [<ParamArray>] content2: obj[] -> JQuery
        abstract after: func: Func<float, string, obj> -> JQuery
        abstract append: content1: obj * [<ParamArray>] content2: obj[] -> JQuery
        abstract append: func: Func<float, string, obj> -> JQuery
        abstract appendTo: target: obj -> JQuery
        abstract before: content1: obj * [<ParamArray>] content2: obj[] -> JQuery
        abstract before: func: Func<float, string, obj> -> JQuery
        abstract clone: ?withDataAndEvents: bool * ?deepWithDataAndEvents: bool -> JQuery
        abstract detach: ?selector: string -> JQuery
        abstract empty: unit -> JQuery
        abstract insertAfter: target: obj -> JQuery
        abstract insertBefore: target: obj -> JQuery
        abstract prepend: content1: obj * [<ParamArray>] content2: obj[] -> JQuery
        abstract prepend: func: Func<float, string, obj> -> JQuery
        abstract prependTo: target: obj -> JQuery
        abstract remove: ?selector: string -> JQuery
        abstract replaceAll: target: obj -> JQuery
        abstract replaceWith: newContent: obj -> JQuery
        abstract replaceWith: func: Func<obj> -> JQuery
        abstract text: unit -> string
        abstract text: text: obj -> JQuery
        abstract text: func: Func<float, string, string> -> JQuery
        abstract toArray: unit -> ResizeArray<obj>
        abstract unwrap: unit -> JQuery
        abstract wrap: wrappingElement: obj -> JQuery
        abstract wrap: func: Func<float, obj> -> JQuery
        abstract wrapAll: wrappingElement: obj -> JQuery
        abstract wrapAll: func: Func<float, string> -> JQuery
        abstract wrapInner: wrappingElement: obj -> JQuery
        abstract wrapInner: func: Func<float, string> -> JQuery
        abstract each: func: Func<float, Element, obj> -> JQuery
        abstract get: index: float -> HTMLElement
        abstract get: unit -> ResizeArray<obj>
        abstract index: unit -> float
        abstract index: selector: obj -> float
        abstract add: selector: string * ?context: Element -> JQuery
        abstract add: [<ParamArray>] elements: Element[] -> JQuery
        abstract add: html: string -> JQuery
        abstract add: obj: JQuery -> JQuery
        abstract children: ?selector: string -> JQuery
        abstract closest: selector: string -> JQuery
        abstract closest: selector: string * ?context: Element -> JQuery
        abstract closest: obj: JQuery -> JQuery
        abstract closest: element: Element -> JQuery
        abstract closest: selectors: obj * ?context: Element -> ResizeArray<obj>
        abstract contents: unit -> JQuery
        abstract end: unit -> JQuery
        abstract eq: index: float -> JQuery
        abstract filter: selector: string -> JQuery
        abstract filter: func: Func<float, Element, obj> -> JQuery
        abstract filter: element: Element -> JQuery
        abstract filter: obj: JQuery -> JQuery
        abstract find: selector: string -> JQuery
        abstract find: element: Element -> JQuery
        abstract find: obj: JQuery -> JQuery
        abstract first: unit -> JQuery
        abstract has: selector: string -> JQuery
        abstract has: contained: Element -> JQuery
        abstract is: selector: string -> bool
        abstract is: func: Func<float, Element, bool> -> bool
        abstract is: obj: JQuery -> bool
        abstract is: elements: obj -> bool
        abstract last: unit -> JQuery
        abstract map: callback: Func<float, Element, obj> -> JQuery
        abstract next: ?selector: string -> JQuery
        abstract nextAll: ?selector: string -> JQuery
        abstract nextUntil: ?selector: string * ?filter: string -> JQuery
        abstract nextUntil: ?element: Element * ?filter: string -> JQuery
        abstract nextUntil: ?obj: JQuery * ?filter: string -> JQuery
        abstract not: selector: string -> JQuery
        abstract not: func: Func<float, Element, bool> -> JQuery
        abstract not: elements: obj -> JQuery
        abstract not: obj: JQuery -> JQuery
        abstract offsetParent: unit -> JQuery
        abstract parent: ?selector: string -> JQuery
        abstract parents: ?selector: string -> JQuery
        abstract parentsUntil: ?selector: string * ?filter: string -> JQuery
        abstract parentsUntil: ?element: Element * ?filter: string -> JQuery
        abstract parentsUntil: ?obj: JQuery * ?filter: string -> JQuery
        abstract prev: ?selector: string -> JQuery
        abstract prevAll: ?selector: string -> JQuery
        abstract prevUntil: ?selector: string * ?filter: string -> JQuery
        abstract prevUntil: ?element: Element * ?filter: string -> JQuery
        abstract prevUntil: ?obj: JQuery * ?filter: string -> JQuery
        abstract siblings: ?selector: string -> JQuery
        abstract slice: start: float * ?end: float -> JQuery
        abstract queue: ?queueName: string -> ResizeArray<obj>
        abstract queue: newQueue: ResizeArray<Function> -> JQuery
        abstract queue: callback: Function -> JQuery
        abstract queue: queueName: string * newQueue: ResizeArray<Function> -> JQuery
        abstract queue: queueName: string * callback: Function -> JQuery

    type Global =
        abstract jQuery: JQueryStatic with get, set
        abstract $: JQueryStatic with get, set

    [<Import("Unknown")>]
    let Global: Global = failwith "JS only"

module jquery =
    type private ImportAttribute(path) =
        inherit System.Attribute()




