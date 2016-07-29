namespace Fable.Import

open System
open System.Text.RegularExpressions
open System.Collections.Generic
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

module MithrilBase =

    [<Erase>]
    type Children =
    | Child of obj
    | Array of ResizeArray<obj>

    [<Erase>]   
    type Child =
    | String of string
    | Element of VirtualElement
    | Component of Component<Controller>

    and Static =
        abstract redraw: Redraw with get, set
        abstract route: obj with get, set
        abstract deferred: obj with get, set
        [<Emit("$0($1...)")>] abstract Invoke: selector: string * [<ParamArray>] children: Children[] -> VirtualElement
        [<Emit("$0($1...)")>] abstract Invoke: selector: string * attributes: Attributes * [<ParamArray>] children: Children[] -> VirtualElement
        [<Emit("$0($1...)")>] abstract Invoke: ``component``: Component<'T> * [<ParamArray>] args: obj[] -> Component<'T>
        abstract prop: promise: Thennable<'T> -> Promise<'T>
        abstract prop: value: 'T -> BasicProperty<'T>
        abstract prop: unit -> BasicProperty<'T>
        abstract withAttr: property: string * callback: Func<obj, obj> * ?callbackThis: obj -> Func<Event, unit>
        abstract ``module``: rootElement: Node * ``component``: Component<'T> -> 'T
        abstract mount: rootElement: Node * ``component``: Component<'T> -> 'T
        abstract ``component``: ``component``: Component<'T> * [<ParamArray>] args: obj[] -> Component<'T>
        abstract trust: html: string -> string
        abstract render: rootElement: Element * children: U2<VirtualElement, ResizeArray<VirtualElement>> * ?forceRecreation: bool -> unit
        abstract request: options: XHROptions -> Promise<obj>
        abstract request: options: JSONPOptions -> Promise<obj>
        abstract sync: promises: ResizeArray<Thennable<'T>> -> Promise<ResizeArray<'T>>
        abstract startComputation: unit -> unit
        abstract endComputation: unit -> unit
        abstract deps: mockWindow: Window -> Window

    and VirtualElement =
        abstract tag: string with get, set
        abstract attrs: Attributes with get, set
        abstract children: ResizeArray<Children> with get, set

    and Event =
        abstract preventDefault: unit -> unit

    and Context =
        abstract retain: bool option with get, set
        abstract onunload: unit -> obj

    and ElementConfig =
        [<Emit("$0($1...)")>] abstract Invoke: element: Element * isInitialized: bool * context: Context * vdom: VirtualElement -> unit

    and Attributes =
        abstract className: string option with get, set
        abstract ``class``: string option with get, set
        abstract config: ElementConfig option with get, set
        abstract key: U2<string, float> option with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: property: string -> obj with get, set
       

    and Controller =
        abstract onunload: evt: Event -> obj

    and Component<'T> =
        abstract controller: [<ParamArray>] args: obj[] -> 'T 
        abstract view: ctrl: 'T * [<ParamArray>] args: obj[] -> VirtualElement

    and Property<'T> =
        [<Emit("$0()")>] abstract get: 'T
        [<Emit("$0($1...)")>] abstract set: value: 'T -> 'T

    and BasicProperty<'T> =
        inherit Property<'T>
        abstract toJSON: unit -> 'T

    and Routes =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> Component<Controller> with get, set

    and Deferred<'T> =
        abstract promise: Promise<'T> with get, set
        abstract resolve: ?value: 'T -> unit
        abstract reject: ?reason: obj -> unit

    and SuccessCallback<'T, 'U> =
        [<Emit("$0($1...)")>] abstract Invoke: value: 'T -> U2<'U, Thennable<'U>>

    and ErrorCallback<'T> =
        [<Emit("$0($1...)")>] abstract Invoke: value: Error -> U2<'T, Thennable<'T>>

    and Thennable<'T> =
        abstract ``then``: success: SuccessCallback<'T, 'U> -> Thennable<'U>
        abstract ``then``: success: SuccessCallback<'T, 'U> * error: ErrorCallback<'V> -> Thennable<U2<'U, 'V>>
        abstract catch: error: ErrorCallback<'T> -> Thennable<'T>
        abstract catch: error: ErrorCallback<'U> -> Thennable<U2<'T, 'U>>

    and Promise<'T> =
        inherit Thennable<'T>
        inherit Property<U2<'T, Promise<'T>>>
        abstract ``then``: success: SuccessCallback<'T, 'U> -> Promise<'U>
        abstract ``then``: success: SuccessCallback<'T, 'U> * error: ErrorCallback<'V> -> Promise<U2<'U, 'V>>
        abstract catch: error: ErrorCallback<'U> -> Promise<U2<'T, 'U>>

    and RequestOptions =
        abstract data: obj option with get, set
        abstract background: bool option with get, set
        abstract initialValue: obj option with get, set
        abstract ``type``: obj option with get, set
        abstract url: string with get, set
        abstract unwrapSuccess: data: obj -> obj
        abstract unwrapError: data: obj -> obj
        abstract serialize: dataToSerialize: obj -> string
        abstract deserialize: dataToDeserialize: string -> obj
        abstract extract: xhr: XMLHttpRequest * options: obj -> string

    and JSONPOptions =
        inherit RequestOptions
        abstract dataType: obj with get, set
        abstract callbackKey: string option with get, set
        abstract data: obj option with get, set

    and XHROptions =
        inherit RequestOptions
        abstract ``method``: (* TODO StringEnum GET | POST | PUT | DELETE | HEAD | OPTIONS *) string with get, set
        abstract user: string option with get, set
        abstract password: string option with get, set
        abstract data: obj option with get, set
        abstract config: xhr: XMLHttpRequest * options: obj -> obj

    and Redraw =
        [<Emit("$0()")>] abstract redraw: unit -> unit
        abstract strategy: string with get, set

type Globals =
    [<Global>] static member m with get(): MithrilBase.Static = failwith "JS only" and set(v: MithrilBase.Static): unit = failwith "JS only"




module Mithril =
    open MithrilBase

    [<AutoOpen>]
    module VirtualDOM =
         
        let inline elem (tagName :string) (attr :Attributes option) (children :obj list) :VirtualElement = 
                children |> List.map (fun x -> match x with
                                                | :? string as s -> Child (String s)
                                                | :? VirtualElement as v -> Child v
                                                | :? Component<Controller> as c -> Child c 
                                                | :? (VirtualElement list) as r -> r |> List.map (fun x -> x :> obj) |> ResizeArray<obj> |> Array )
                         |> (fun c -> match attr with
                                | Some(a) -> (Globals.m.Invoke(tagName, a, List<Children>(c)))
                                | None -> (Globals.m.Invoke(tagName,List<Children>(c))))
                

        // Elements - list of ELEM.elements here: https://developer.mozilla.org/en-US/docs/Web/HTML/Element
        // Void ELEM.elements
        let inline br a b = elem "br" a b
        let inline area a b = elem "area" a b
        let inline baseHtml a b = elem "base" a b
        let inline col a b = elem "col" a b
        let inline embed a b = elem "embed" a b
        let inline hr a b = elem "hr" a b
        let inline img a b = elem "img" a b
        let inline input a b = elem "input" a b
        let inline link a b = elem "link" a b
        let inline meta a b = elem "meta" a b
        let inline param a b = elem "param" a b
        let inline source a b = elem "source" a b
        let inline track a b = elem "track" a b
        let inline wbr a b = elem "wbr" a b

        // Metadata
        let inline head a b = elem "head" a b
        let inline style a b = elem "style" a b
        let inline title a b = elem "title" a b

        // Content sectioning
        let inline address a b = elem "address" a b
        let inline article a b = elem "article" a b
        let inline aside a b = elem "aside" a b
        let inline footer a b = elem "footer" a b
        let inline header a b = elem "header" a b
        let inline h1 a b = elem "h1" a b
        let inline h2 a b = elem "h2" a b
        let inline h3 a b = elem "h3" a b
        let inline h4 a b = elem "h4" a b
        let inline h5 a b = elem "h5" a b
        let inline h6 a b = elem "h6" a b
        let inline hgroup a b = elem "hgroup" a b
        let inline nav a b = elem "nav" a b

        // Text content
        let inline dd a b = elem "ddr" a b
        let inline div a b = elem "div" a b
        let inline dl a b = elem "dl" a b
        let inline dt a b = elem "dt" a b
        let inline figcaption a b = elem "figcaption" a b
        let inline figure a b = elem "figure" a b
        let inline li a b = elem "li" a b
        let inline main a b = elem "main" a b
        let inline ol a b = elem "ol" a b
        let inline p a b = elem "p" a b
        let inline pre a b = elem "pre" a b
        let inline section a b = elem "section" a b
        let inline ul a b = elem "ul" a b

        // Inline text semantics
        let inline a a b = elem "a" a b
        let inline abbr a b = elem "abbr" a b
        let inline b a b = elem "b" a b
        let inline bdi a b = elem "bdi" a b
        let inline bdo a b = elem "bdo" a b
        let inline cite a b = elem "cite" a b
        let inline code a b = elem "code" a b
        let inline data a b = elem "data" a b
        let inline dfn a b = elem "dfn" a b
        let inline em a b = elem "em" a b
        let inline i a b = elem "i" a b
        let inline kbd a b = elem "kbd" a b
        let inline mark a b = elem "mark" a b
        let inline q a b = elem "q" a b
        let inline rp a b = elem "rp" a b
        let inline rt a b = elem "rt" a b
        let inline rtc a b = elem "rtc" a b
        let inline ruby a b = elem "ruby" a b
        let inline s a b = elem "s" a b
        let inline samp a b = elem "samp" a b
        let inline small a b = elem "small" a b
        let inline span a b = elem "span" a b
        let inline strong a b = elem "strong" a b
        let inline sub a b = elem "sub" a b
        let inline sup a b = elem "sup" a b
        let inline time a b = elem "time" a b
        let inline u a b = elem "u" a b
        let inline var a b = elem "var" a b

        // Image and multimedia
        let inline audio a b = elem "audio" a b
        let inline map a b = elem "map" a b
        let inline video a b = elem "video" a b

        // Embedded content
        let inline objectHtml a b = elem "object" a b

        // Demarcasting edits
        let inline del a b = elem "del" a b
        let inline ins a b = elem "ins" a b

        // Table content
        let inline caption a b = elem "caption" a b
        let inline colgroup a b = elem "colgroup" a b
        let inline table a b = elem "table" a b
        let inline tbody a b = elem "tbody" a b
        let inline td a b = elem "td" a b
        let inline tfoot a b = elem "tfoot" a b
        let inline th a b = elem "th" a b
        let inline thead a b = elem "thead" a b
        let inline tr a b = elem "tr" a b

        // Forms
        let inline button a b = elem "button" a b
        let inline datalist a b = elem "datalist" a b
        let inline fieldset a b = elem "fieldset" a b
        let inline form a b = elem "form" a b
        let inline label a b = elem "label" a b
        let inline legend a b = elem "legend" a b
        let inline meter a b = elem "meter" a b
        let inline optgroup a b = elem "optgroup" a b
        let inline option a b = elem "option" a b
        let inline output a b = elem "output" a b
        let inline progress a b = elem "progress" a b
        let inline select a b = elem "select" a b
        let inline textarea a b = elem "textarea" a b

        // Interactive ELEM.elements
        let inline details a b = elem "details" a b
        let inline dialog a b = elem "dialog" a b
        let inline menu a b = elem "menu" a b
        let inline menuitem a b = elem "menuitem" a b
        let inline summary a b = elem "summary" a b

    [<AutoOpen>]
    module Events =

        let inline onEvent (eventType :string) f :string*obj = 
            match f with 
             | :? Func<Event,unit> as fnc -> (eventType,fnc :> obj) 
             | :? (Event -> unit) as fn -> (eventType, Func<Event,unit>fn :> obj)
             | _ -> Exception("event binding error")

        let inline onClick x = onEvent "onclick" x
        let inline onContextMenu x = onEvent "oncontextmenu" x
        let inline onDblClick x = onEvent "ondblclick" x
        let inline onMouseDown x = onEvent "onmousedown" x
        let inline onMouseEnter x = onEvent "onmouseenter" x
        let inline onMouseLeave x = onEvent "onmouseleave" x
        let inline onMouseMove x = onEvent "onmousemove" x
        let inline onMouseOut x = onEvent "onmouseout" x
        let inline onMouseOver x = onEvent "onmouseover" x
        let inline onMouseUp x = onEvent "onmouseup" x
        let inline onShow x = onEvent "onshow" x

        let inline onKeydown x = onEvent "onkeydown" x
        let inline onKeypress x = onEvent "onkeypress" x
        let inline onKeyup x = onEvent "onkeyup" x


        let inline onAbort x = onEvent "onabort" x
        let inline onAfterPrint x = onEvent "onafterprint" x
        let inline onAudioEnd x = onEvent "onaudioend" x
        let inline onAudioStart x = onEvent "onaudiostart" x 
        let inline onBeforePrint x = onEvent "onbeforeprint" x
        let inline onCached x = onEvent "oncached" x
        let inline onCanPlay x = onEvent "oncanplay" x
        let inline onCanPlayThrough x = onEvent "oncanplaythrough" x
        let inline onChange x = onEvent "onchange" x
        let inline onChargingChange x = onEvent "onchargingchange" x
        let inline onChargingTimeChange x = onEvent "onchargingtimechange" x
        let inline onChecking x = onEvent "onchecking" x
        let inline onClose x = onEvent "onclose" x
        let inline onDischargingTimeChange x = onEvent "ondischargingtimechange" x
        let inline onDOMContentLoaded x = onEvent "onDOMContentLoaded" x
        let inline onDownloading x = onEvent "ondownloading" x
        let inline onDurationchange x = onEvent "ondurationchange" x
        let inline onEmptied x = onEvent "onemptied" x
        let inline onEnd x = onEvent "onend" x
        let inline onEnded x = onEvent "onended" x
        let inline onError x = onEvent "onerror" x
        let inline onCullScreenChange x = onEvent "onfullscreenchange" x
        let inline onCullScreenError x = onEvent "onfullscreenerror" x
        let inline onInput x = onEvent "oninput" x
        let inline onInvalid x = onEvent "oninvalid" x
        let inline onLanguageChange x = onEvent "onlanguagechange" x
        let inline onLevelChange x = onEvent "onlevelchange" x
        let inline onLoadedData x = onEvent "onloadeddata" x
        let inline onLoadedMetaData x = onEvent "onloadedmetadata" x
        let inline onNoUpdate x = onEvent "onnoupdate" x
        let inline onObsolete x = onEvent "onobsolete" x
        let inline onOffline x = onEvent "onoffline" x
        let inline onOnline x = onEvent "ononline" x
        let inline onOpen x = onEvent "onopen" x
        let inline onOrientationChange x = onEvent "onorientationchange" x
        let inline onPause x = onEvent "onpause" x
        let inline onPointerlockchange x = onEvent "onpointerlockchange" x
        let inline onPointerlockerror x = onEvent "onpointerlockerror" x
        let inline onPlay x = onEvent "onplay" x
        let inline onPlaying x = onEvent "onplaying" x
        let inline onRateChange x = onEvent "onratechange" x
        let inline onReadyStateChange x = onEvent "onreadystatechange" x
        let inline onReset x = onEvent "onreset" x
        let inline onSeeked x = onEvent "onseeked" x
        let inline onSeeking x = onEvent "onseeking" x
        let inline onSelectStart x = onEvent "onselectstart" x
        let inline onSelectionChange x = onEvent "onselectionchange" x
        let inline onSoundEnd x = onEvent "onsoundend" x
        let inline onSoundStart x = onEvent "onsoundstart" x
        let inline onSpeechEnd x = onEvent "onspeechend" x
        let inline onSpeechStart x = onEvent "onspeechstart" x
        let inline onStalled x = onEvent "onstalled" x
        let inline onStart x = onEvent "onstart" x
        let inline onSubmit x = onEvent "onsubmit" x
        let inline onSuccess x = onEvent "onsuccess" x
        let inline onSuspend x = onEvent "onsuspend" x
        let inline onTimeUpdate x = onEvent "ontimeupdate" x
        let inline onUpdateReady x = onEvent "onupdateready" x
        let inline onVoicesChanged x = onEvent "onvoiceschanged" x
        let inline onVisibilityChange x = onEvent "onvisibilitychange" x
        let inline onVolumeChange x = onEvent "onvolumechange" x
        let inline onVrdisplayConnected x = onEvent "onvrdisplayconnected" x
        let inline onVrdisplayDisconnected x = onEvent "onvrdisplaydisconnected" x
        let inline onVrdisplayPresentChange x = onEvent "onvrdisplaypresentchange" x
        let inline onWaiting x = onEvent "onwaiting"  x

        let inline onBlur x = onEvent "onblur" x
        let inline onFocus x = onEvent "onfocus" x

//    [<AutoOpen>]
//    module Svg = 
//        let svgNS = Attribute.Property("namespace","http://www.w3.org/2000/svg")
//        let inline svgElem tagName attrs children = Element((tagName, svgNS::attrs), children)
//
//        let inline svg x = svgElem "svg" x
//        let inline circle x = svgElem "circle"  x
//        let inline rect x = svgElem "rect"  x
//
//        let inline width x = attribute "width" x
//        let inline height x = attribute "height" x
//        let inline viewBox x = attribute "viewBox" x
//        let inline cx x = attribute "cx" x
//        let inline cy x = attribute "cy" x
//        let inline r x = attribute "r" x
//        let inline stroke x = attribute "stroke" x
//        let inline strokeWidth x = attribute "stroke-width" x
//        let inline fill x = attribute "fill" x


    [<AutoOpen>]
    module Extentions =
        let inline attr ls =
            let m = createEmpty<Attributes>
            for (s,o) in ls do
                match o with
                | :? string as o2 -> if s = "class" then m.className <- Some(o2) else m.Item(s) <- o
                | _ -> m.Item(s) <- o
            Some(m)

        let inline prop str (o :obj) =
            (str,o)

        let event evt v func =
            let f = (fun (a :obj) -> func (a :?> 'a) :> obj)
            prop evt (Globals.m.withAttr(v,Func<obj,obj>(f)) :> obj)

        let bindattr str func =
            let f = (fun (a :obj) -> func (a :?> 'a) :> obj)
            Globals.m.withAttr(v,Func<obj,obj>(f))

        let css str =
            ("class",str)

        let name str =
            ("name",str)

        let style ls =
            let s = createEmpty
            for (key,value) in ls do
                s?key <- value
            ("style",s)

        let newComponent (c :obj [] -> 'a) (v :'a -> VirtualElement) =
            let o = createEmpty<Component<'a>>
            o?controller <- (fun x -> c x)
            o?view <- v
            o
     
     [<RequireQualifiedAccess>]
     module M =       

        let mount ((elm:Node),(componen:Component<'T>))=
            Globals.m.mount(elm,componen)

        let prop ob =
            Globals.m.prop ob

       // abstract route: obj with get, set
      //  abstract deferred: obj with get, set

        let defmodule ((elm:Node),(componen:Component<'T>)) =
            Globals.m.``module``(elm,componen)

        let defcomponent ((componen:Component<'T>),(args:obj[])) =
            Globals.m.``component``(componen,args)

        let redraw() =
            Globals.m.redraw.redraw()

        let trust html =
            Globals.m.trust html

        let redrawStrategy(rdw:string) =
            Globals.m.redraw.strategy <- rdw

        let render (elm,children) =
            Globals.m.render (elm,children)

        let forceRender (elm,children,force) =
            Globals.m.render (elm,children,force)

        let requestJSON (opt: JSONPOptions) =
            Globals.m.request opt

        let requestXHR (opt: XHROptions) =
            Globals.m.request opt

        let sync promises =
            Globals.m.sync promises

        let startComputation() =
            Globals.m.startComputation()
    
        let endComputation() =
            Globals.m.endComputation()

        //    type AttributeBuilder a b = elem
//
//        //member this.Zero  a b = elem createEmpty<Attributes>
//        member this.Bind (m :Attributes) f =
//            let (s,o) = f ()
//            m.Item(s) <- o
//            m
//
//        member this.Return _ = createEmpty<Attributes>
//
//        [<CustomOperation("prop",MaintainsVariableSpaceUsingBind = true)>]    
//        member this.Add m str obj =
//            (fun () -> (str,obj))
//
//        [<CustomOperation("event",MaintainsVariableSpaceUsingBind = true)>]
//        member this.With m evt (value :string) (func :('a -> 'b)) =
//            let f = (fun (a :obj) ->  func (a :?> 'a) :> obj)
//            (fun () -> (evt,Globals.m.withAttr(value,Func<obj,obj>(f))))
//        
//    let attr = AttributeBuilder()