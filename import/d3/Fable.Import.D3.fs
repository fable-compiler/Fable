// Type definitions for d3JS
// Project: http://d3js.org/
// Definitions by: Alex Ford <https://github.com/gustavderdrache>, Boris Yankov <https://github.com/borisyankov>
// Definitions: https://github.com/DefinitelyTyped/DefinitelyTyped

namespace Fable.Import

open System
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

type TouchList =
    interface end

module D3 =
    type Primitive =
        U3<float, string, bool>

    and Numeric =
        abstract valueOf: unit -> float

    and Selection<'Datum> =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: float -> (* TODO selection.Group *) obj with get, set
        abstract length: float with get, set
        abstract attr: name: string -> string
        abstract attr: name: string * value: Primitive -> Selection<'Datum>
        abstract attr: name: string * value: Func<'Datum, float, float, Primitive> -> Selection<'Datum>
        abstract attr: obj: obj -> Selection<'Datum>
        abstract classed: name: string -> bool
        abstract classed: name: string * value: bool -> Selection<'Datum>
        abstract classed: name: string * value: Func<'Datum, float, float, bool> -> Selection<'Datum>
        abstract classed: obj: obj -> Selection<'Datum>
        abstract style: name: string -> string
        abstract style: name: string * value: Primitive * ?priority: string -> Selection<'Datum>
        abstract style: name: string * value: Func<'Datum, float, float, Primitive> * ?priority: string -> Selection<'Datum>
        abstract style: obj: obj * ?priority: string -> Selection<'Datum>
        abstract property: name: string -> obj
        abstract property: name: string * value: obj -> Selection<'Datum>
        abstract property: name: string * value: Func<'Datum, float, float, obj> -> Selection<'Datum>
        abstract property: obj: obj -> Selection<'Datum>
        abstract text: unit -> string
        abstract text: value: Primitive -> Selection<'Datum>
        abstract text: value: Func<'Datum, float, float, Primitive> -> Selection<'Datum>
        abstract html: unit -> string
        abstract html: value: string -> Selection<'Datum>
        abstract html: value: Func<'Datum, float, float, string> -> Selection<'Datum>
        abstract append: name: string -> Selection<'Datum>
        abstract append: name: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
        abstract insert: name: string * before: string -> Selection<'Datum>
        abstract insert: name: string * before: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
        abstract insert: name: Func<'Datum, float, float, EventTarget> * before: string -> Selection<'Datum>
        abstract insert: name: Func<'Datum, float, float, EventTarget> * before: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
        abstract remove: unit -> Selection<'Datum>
        abstract data: unit -> 'Datum[]
        abstract data: data: 'NewDatum[] * ?key: Func<'NewDatum, float, float, string> -> (* TODO selection.Update<'NewDatum> *) obj
        abstract data: data: Func<'Datum, float, float, 'NewDatum[]> * ?key: Func<'NewDatum, float, float, string> -> (* TODO selection.Update<'NewDatum> *) obj
        abstract filter: selector: string -> Selection<'Datum>
        abstract filter: selector: Func<'Datum, float, float, bool> -> Selection<'Datum>
        abstract datum: unit -> 'Datum
        abstract datum: value: Func<'Datum, float, float, 'NewDatum> -> Selection<'NewDatum>
        abstract datum: value: 'NewDatum -> Selection<'NewDatum>
        abstract sort: ?comparator: Func<'Datum, 'Datum, float> -> Selection<'Datum>
        abstract order: unit -> Selection<'Datum>
        abstract on: ``type``: string -> Func<'Datum, float, float, obj>
        abstract on: ``type``: string * listener: Func<'Datum, float, float, obj> * ?capture: bool -> Selection<'Datum>
        abstract transition: ?name: string -> Transition<'Datum>
        abstract interrupt: ?name: string -> Selection<'Datum>
        abstract select: selector: string -> Selection<'Datum>
        abstract select: selector: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
        abstract selectAll: selector: string -> Selection<obj>
        abstract selectAll: selector: Func<'Datum, float, float, U2<EventTarget[], NodeList>> -> Selection<obj>
        abstract each: func: Func<'Datum, float, float, obj> -> Selection<'Datum>
        abstract call: func: Func<Selection<'Datum>, obj, obj> * [<ParamArray>] args: obj[] -> Selection<'Datum>
        abstract empty: unit -> bool
        abstract node: unit -> Node
        abstract size: unit -> float

    and Transition<'Datum> =
        abstract transition: unit -> Transition<'Datum>
        abstract delay: unit -> float
        abstract delay: delay: float -> Transition<'Datum>
        abstract delay: delay: Func<'Datum, float, float, float> -> Transition<'Datum>
        abstract duration: unit -> float
        abstract duration: duration: float -> Transition<'Datum>
        abstract duration: duration: Func<'Datum, float, float, float> -> Transition<'Datum>
        abstract ease: unit -> Func<float, float>
        abstract ease: value: string * [<ParamArray>] args: obj[] -> Transition<'Datum>
        abstract ease: value: Func<float, float> -> Transition<'Datum>
        abstract attr: name: string * value: Primitive -> Transition<'Datum>
        abstract attr: name: string * value: Func<'Datum, float, float, Primitive> -> Transition<'Datum>
        abstract attr: obj: obj -> Transition<'Datum>
        abstract attrTween: name: string * tween: Func<'Datum, float, string, Func<float, Primitive>> -> Transition<'Datum>
        abstract style: name: string * value: Primitive * ?priority: string -> Transition<'Datum>
        abstract style: name: string * value: Func<'Datum, float, float, Primitive> * ?priority: string -> Transition<'Datum>
        abstract style: obj: obj * ?priority: string -> Transition<'Datum>
        abstract styleTween: name: string * tween: Func<'Datum, float, string, Func<float, Primitive>> * ?priority: string -> Transition<'Datum>
        abstract text: value: Primitive -> Transition<'Datum>
        abstract text: value: Func<'Datum, float, float, Primitive> -> Transition<'Datum>
        abstract tween: name: string * factory: Func<unit,Func<float, obj>> -> Transition<'Datum>
        abstract remove: unit -> Transition<'Datum>
        abstract select: selector: string -> Transition<'Datum>
        abstract select: selector: Func<'Datum, float, EventTarget> -> Transition<'Datum>
        abstract selectAll: selector: string -> Transition<obj>
        abstract selectAll: selector: Func<'Datum, float, EventTarget[]> -> Transition<obj>
        abstract filter: selector: string -> Transition<'Datum>
        abstract filter: selector: Func<'Datum, float, bool> -> Transition<'Datum>
        abstract each: ``type``: string * listener: Func<'Datum, float, obj> -> Transition<'Datum>
        abstract each: listener: Func<'Datum, float, obj> -> Transition<'Datum>
        abstract call: func: Func<Transition<'Datum>, obj, obj> * [<ParamArray>] args: obj[] -> Transition<'Datum>
        abstract empty: unit -> bool
        abstract node: unit -> Node
        abstract size: unit -> float

    and BaseEvent =
        abstract ``type``: string with get, set
        abstract sourceEvent: Event option with get, set

    and ZoomEvent =
        inherit BaseEvent
        abstract scale: float with get, set
        abstract translate: float * float with get, set

    and DragEvent =
        inherit BaseEvent
        abstract x: float with get, set
        abstract y: float with get, set
        abstract dx: float with get, set
        abstract dy: float with get, set

    and Map<'T> =
        abstract has: key: string -> bool
        abstract get: key: string -> 'T
        abstract set: key: string * value: 'T -> 'T
        abstract remove: key: string -> bool
        abstract keys: unit -> string[]
        abstract values: unit -> 'T[]
        abstract entries: unit -> obj[]
        abstract forEach: func: Func<string, 'T, obj> -> unit
        abstract empty: unit -> bool
        abstract size: unit -> float

    and Set =
        abstract has: value: string -> bool
        abstract add: value: string -> string
        abstract remove: value: string -> bool
        abstract values: unit -> string[]
        abstract forEach: func: Func<string, obj> -> unit
        abstract empty: unit -> bool
        abstract size: unit -> float

    and Nest<'T> =
        abstract key: func: Func<'T, string> -> Nest<'T>
        abstract sortKeys: comparator: Func<string, string, float> -> Nest<'T>
        abstract sortValues: comparator: Func<'T, 'T, float> -> Nest<'T>
        abstract rollup: func: Func<'T[], 'U> -> Nest<'T>
        abstract map: array: 'T[] -> obj
        abstract map: array: 'T[] * mapType: obj -> Map<obj>
        abstract entries: array: 'T[] -> obj[]

    and Transform =
        abstract rotate: float with get, set
        abstract translate: float * float with get, set
        abstract skew: float with get, set
        abstract scale: float * float with get, set
        abstract toString: unit -> string

    and FormatPrefix =
        abstract symbol: string with get, set
        abstract scale: n: float -> float

    and rgbType =
        [<Emit("new $0($1...)")>] abstract Create: r: float * g: float * b: float -> Rgb
        [<Emit("new $0($1...)")>] abstract Create: color: string -> Rgb
        [<Emit("$0($1...)")>] abstract Invoke: r: float * g: float * b: float -> Rgb
        [<Emit("$0($1...)")>] abstract Invoke: color: string -> Rgb

    and Rgb =
        inherit Color
        abstract r: float with get, set
        abstract g: float with get, set
        abstract b: float with get, set
        abstract brighter: ?k: float -> Rgb
        abstract darker: ?k: float -> Rgb
        abstract hsl: unit -> Hsl
        abstract toString: unit -> string

    and hslType =
        [<Emit("new $0($1...)")>] abstract Create: h: float * s: float * l: float -> Hsl
        [<Emit("new $0($1...)")>] abstract Create: color: string -> Hsl
        [<Emit("$0($1...)")>] abstract Invoke: h: float * s: float * l: float -> Hsl
        [<Emit("$0($1...)")>] abstract Invoke: color: string -> Hsl

    and Hsl =
        inherit Color
        abstract h: float with get, set
        abstract s: float with get, set
        abstract l: float with get, set
        abstract brighter: ?k: float -> Hsl
        abstract darker: ?k: float -> Hsl
        abstract rgb: unit -> Rgb
        abstract toString: unit -> string

    and hclType =
        [<Emit("new $0($1...)")>] abstract Create: h: float * c: float * l: float -> Hcl
        [<Emit("new $0($1...)")>] abstract Create: color: string -> Hcl
        [<Emit("$0($1...)")>] abstract Invoke: h: float * c: float * l: float -> Hcl
        [<Emit("$0($1...)")>] abstract Invoke: color: string -> Hcl

    and Hcl =
        inherit Color
        abstract h: float with get, set
        abstract c: float with get, set
        abstract l: float with get, set
        abstract brighter: ?k: float -> Hcl
        abstract darker: ?k: float -> Hcl

    and labType =
        [<Emit("new $0($1...)")>] abstract Create: l: float * a: float * b: float -> Lab
        [<Emit("new $0($1...)")>] abstract Create: color: string -> Lab
        [<Emit("$0($1...)")>] abstract Invoke: l: float * a: float * b: float -> Lab
        [<Emit("$0($1...)")>] abstract Invoke: color: string -> Lab

    and Lab =
        inherit Color
        abstract l: float with get, set
        abstract a: float with get, set
        abstract b: float with get, set
        abstract brighter: ?k: float -> Lab
        abstract darker: ?k: float -> Lab
        abstract rgb: unit -> Rgb
        abstract toString: unit -> string

    and colorType =
        [<Emit("$0($1...)")>] abstract Invoke: unit -> Color
        [<Emit("new $0($1...)")>] abstract Create: unit -> Color

    and Color =
        abstract rgb: unit -> Rgb

    and Dispatch =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: ``event``: string -> Func<obj, unit> with get, set
        abstract on: ``type``: string -> Func<obj, unit>
        abstract on: ``type``: string * listener: Func<obj, obj> -> Dispatch

    and Xhr =
        abstract header: name: string -> string
        abstract header: name: string * value: string -> Xhr
        abstract mimeType: unit -> string
        abstract mimeType: ``type``: string -> Xhr
        abstract responseType: unit -> string
        abstract responseType: ``type``: string -> Xhr
        abstract response: unit -> Func<XMLHttpRequest, obj>
        abstract response: value: Func<XMLHttpRequest, obj> -> Xhr
        abstract get: ?callback: Func<obj, obj, unit> -> Xhr
        abstract post: ?data: obj * ?callback: Func<obj, obj, unit> -> Xhr
        abstract post: callback: Func<obj, obj, unit> -> Xhr
        abstract send: ``method``: string * ?data: obj * ?callback: Func<obj, obj, unit> -> Xhr
        abstract send: ``method``: string * callback: Func<obj, obj, unit> -> Xhr
        abstract abort: unit -> Xhr
        [<Emit("$0.on('beforesend')")>] abstract on_beforesend: unit -> Func<XMLHttpRequest, unit>
        [<Emit("$0.on('progress')")>] abstract on_progress: unit -> Func<XMLHttpRequest, unit>
        [<Emit("$0.on('load')")>] abstract on_load: unit -> Func<obj, unit>
        [<Emit("$0.on('error')")>] abstract on_error: unit -> Func<obj, unit>
        abstract on: ``type``: string -> Func<obj, unit>
        [<Emit("$0.on('beforesend',$1...)")>] abstract on_beforesend: listener: Func<XMLHttpRequest, unit> -> Xhr
        [<Emit("$0.on('progress',$1...)")>] abstract on_progress: listener: Func<XMLHttpRequest, unit> -> Xhr
        [<Emit("$0.on('load',$1...)")>] abstract on_load: listener: Func<obj, unit> -> Xhr
        [<Emit("$0.on('error',$1...)")>] abstract on_error: listener: Func<obj, unit> -> Xhr
        abstract on: ``type``: string * listener: Func<obj, unit> -> Xhr

    and Dsv =
        [<Emit("$0($1...)")>] abstract Invoke: url: string * callback: Func<obj[], unit> -> DsvXhr<obj>
        [<Emit("$0($1...)")>] abstract Invoke: url: string * callback: Func<obj, obj[], unit> -> DsvXhr<obj>
        [<Emit("$0($1...)")>] abstract Invoke: url: string -> DsvXhr<obj>
        [<Emit("$0($1...)")>] abstract Invoke: url: string * accessor: Func<obj, 'T> * callback: Func<'T[], unit> -> DsvXhr<'T>
        [<Emit("$0($1...)")>] abstract Invoke: url: string * accessor: Func<obj, 'T> * callback: Func<obj, 'T[], unit> -> DsvXhr<'T>
        [<Emit("$0($1...)")>] abstract Invoke: url: string * accessor: Func<obj, 'T> -> DsvXhr<'T>
        abstract parse: string: string -> obj[]
        abstract parse: string: string * accessor: Func<obj, float, 'T> -> 'T[]
        abstract parseRows: string: string -> string[][]
        abstract parseRows: string: string * accessor: Func<string[], float, 'T> -> 'T[]
        abstract format: rows: obj[] -> string
        abstract formatRows: rows: string[][] -> string

    and DsvXhr<'T> =
        inherit Xhr
        abstract row: unit -> Func<obj, 'T>
        abstract row: accessor: Func<obj, 'U> -> DsvXhr<'U>
        abstract header: name: string -> string
        abstract header: name: string * value: string -> DsvXhr<'T>
        abstract mimeType: unit -> string
        abstract mimeType: ``type``: string -> DsvXhr<'T>
        abstract responseType: unit -> string
        abstract responseType: ``type``: string -> DsvXhr<'T>
        abstract response: unit -> Func<XMLHttpRequest, obj>
        abstract response: value: Func<XMLHttpRequest, obj> -> DsvXhr<'T>
        abstract get: ?callback: Func<XMLHttpRequest, 'T[], unit> -> DsvXhr<'T>
        abstract post: ?data: obj * ?callback: Func<XMLHttpRequest, 'T[], unit> -> DsvXhr<'T>
        abstract post: callback: Func<XMLHttpRequest, 'T[], unit> -> DsvXhr<'T>
        abstract send: ``method``: string * ?data: obj * ?callback: Func<XMLHttpRequest, 'T[], unit> -> DsvXhr<'T>
        abstract send: ``method``: string * callback: Func<XMLHttpRequest, 'T[], unit> -> DsvXhr<'T>
        abstract abort: unit -> DsvXhr<'T>
        [<Emit("$0.on('beforesend')")>] abstract on_beforesend: unit -> Func<XMLHttpRequest, unit>
        [<Emit("$0.on('progress')")>] abstract on_progress: unit -> Func<XMLHttpRequest, unit>
        [<Emit("$0.on('load')")>] abstract on_load: unit -> Func<'T[], unit>
        [<Emit("$0.on('error')")>] abstract on_error: unit -> Func<obj, unit>
        abstract on: ``type``: string -> Func<obj, unit>
        [<Emit("$0.on('beforesend',$1...)")>] abstract on_beforesend: listener: Func<XMLHttpRequest, unit> -> DsvXhr<'T>
        [<Emit("$0.on('progress',$1...)")>] abstract on_progress: listener: Func<XMLHttpRequest, unit> -> DsvXhr<'T>
        [<Emit("$0.on('load',$1...)")>] abstract on_load: listener: Func<'T[], unit> -> DsvXhr<'T>
        [<Emit("$0.on('error',$1...)")>] abstract on_error: listener: Func<obj, unit> -> DsvXhr<'T>
        abstract on: ``type``: string * listener: Func<obj, unit> -> DsvXhr<'T>

    and LocaleDefinition =
        abstract decimal: string with get, set
        abstract thousands: string with get, set
        abstract grouping: float[] with get, set
        abstract currency: string * string with get, set
        abstract dateTime: string with get, set
        abstract date: string with get, set
        abstract time: string with get, set
        abstract periods: string * string with get, set
        abstract days: string * string * string * string * string * string * string with get, set
        abstract shortDays: string * string * string * string * string * string * string with get, set
        abstract months: string * string * string * string * string * string * string * string * string * string * string * string with get, set
        abstract shortMonths: string * string * string * string * string * string * string * string * string * string * string * string with get, set

    and Locale =
        abstract timeFormat: obj with get, set
        abstract numberFormat: specifier: string -> Func<float, string>

    type [<Import("*","d3")>] Globals =
        static member version with get(): string = failwith "JS only" and set(v: string): unit = failwith "JS only"
        static member ``event`` with get(): U2<Event, BaseEvent> = failwith "JS only" and set(v: U2<Event, BaseEvent>): unit = failwith "JS only"
        static member bisect with get(): obj = failwith "JS only" and set(v: obj): unit = failwith "JS only"
        static member rgb with get(): rgbType = failwith "JS only" and set(v: rgbType): unit = failwith "JS only"
        static member hsl with get(): hslType = failwith "JS only" and set(v: hslType): unit = failwith "JS only"
        static member hcl with get(): hclType = failwith "JS only" and set(v: hclType): unit = failwith "JS only"
        static member lab with get(): labType = failwith "JS only" and set(v: labType): unit = failwith "JS only"
        static member color with get(): colorType = failwith "JS only" and set(v: colorType): unit = failwith "JS only"
        static member interpolators with get(): Func<obj, obj, Func<float, obj[]>> = failwith "JS only" and set(v: Func<obj, obj, Func<float, obj[]>>): unit = failwith "JS only"
        static member csv with get(): Dsv = failwith "JS only" and set(v: Dsv): unit = failwith "JS only"
        static member tsv with get(): Dsv = failwith "JS only" and set(v: Dsv): unit = failwith "JS only"
        static member select(selector: string): Selection<obj> = failwith "JS only"
        static member select(node: EventTarget): Selection<obj> = failwith "JS only"
        static member selectAll(selector: string): Selection<obj> = failwith "JS only"
        static member selectAll(nodes: EventTarget[]): Selection<obj> = failwith "JS only"
        static member selection(): Selection<obj> = failwith "JS only"
        static member transition(): Transition<obj> = failwith "JS only"
        [<Emit("$0.ease('linear')")>] static member ease_linear(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('linear-in')")>] static member ``ease_linear-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('linear-out')")>] static member ``ease_linear-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('linear-in-out')")>] static member ``ease_linear-in-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('linear-out-in')")>] static member ``ease_linear-out-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('poly',$1...)")>] static member ease_poly(k: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('poly-in',$1...)")>] static member ``ease_poly-in``(k: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('poly-out',$1...)")>] static member ``ease_poly-out``(k: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('poly-in-out',$1...)")>] static member ``ease_poly-in-out``(k: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('poly-out-in',$1...)")>] static member ``ease_poly-out-in``(k: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('quad')")>] static member ease_quad(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('quad-in')")>] static member ``ease_quad-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('quad-out')")>] static member ``ease_quad-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('quad-in-out')")>] static member ``ease_quad-in-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('quad-out-in')")>] static member ``ease_quad-out-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('cubic')")>] static member ease_cubic(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('cubic-in')")>] static member ``ease_cubic-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('cubic-out')")>] static member ``ease_cubic-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('cubic-in-out')")>] static member ``ease_cubic-in-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('cubic-out-in')")>] static member ``ease_cubic-out-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('sin')")>] static member ease_sin(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('sin-in')")>] static member ``ease_sin-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('sin-out')")>] static member ``ease_sin-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('sin-in-out')")>] static member ``ease_sin-in-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('sin-out-in')")>] static member ``ease_sin-out-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('circle')")>] static member ease_circle(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('circle-in')")>] static member ``ease_circle-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('circle-out')")>] static member ``ease_circle-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('circle-in-out')")>] static member ``ease_circle-in-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('circle-out-in')")>] static member ``ease_circle-out-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('elastic',$1...)")>] static member ease_elastic(?a: float, ?b: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('elastic-in',$1...)")>] static member ``ease_elastic-in``(?a: float, ?b: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('elastic-out',$1...)")>] static member ``ease_elastic-out``(?a: float, ?b: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('elastic-in-out',$1...)")>] static member ``ease_elastic-in-out``(?a: float, ?b: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('elastic-out-in',$1...)")>] static member ``ease_elastic-out-in``(?a: float, ?b: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('back',$1...)")>] static member ease_back(s: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('back-in',$1...)")>] static member ``ease_back-in``(s: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('back-out',$1...)")>] static member ``ease_back-out``(s: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('back-in-out',$1...)")>] static member ``ease_back-in-out``(s: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('back-out-in',$1...)")>] static member ``ease_back-out-in``(s: float): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('bounce')")>] static member ease_bounce(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('bounce-in')")>] static member ``ease_bounce-in``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('bounce-out')")>] static member ``ease_bounce-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('bounce-in-out')")>] static member ``ease_bounce-in-out``(): Func<float, float> = failwith "JS only"
        [<Emit("$0.ease('bounce-out-in')")>] static member ``ease_bounce-out-in``(): Func<float, float> = failwith "JS only"
        static member ease(``type``: string, [<ParamArray>] args: obj[]): Func<float, float> = failwith "JS only"
        static member timer(func: Func<unit,obj>, ?delay: float, ?time: float): unit = failwith "JS only"
        static member mouse(container: EventTarget): float * float = failwith "JS only"
        static member touch(container: EventTarget, identifer: float): float * float = failwith "JS only"
        static member touch(container: EventTarget, touches: TouchList, identifer: float): float * float = failwith "JS only"
        static member touches(container: EventTarget, ?touches: TouchList): (float * float)[] = failwith "JS only"
        static member ascending(a: Primitive, b: Primitive): float = failwith "JS only"
        static member descending(a: Primitive, b: Primitive): float = failwith "JS only"
        static member min(array: float[]): float = failwith "JS only"
        static member min(array: string[]): string = failwith "JS only"
        static member min(array: 'T[]): 'T = failwith "JS only"
        static member min(array: 'T[], accessor: Func<'T, float, float>): float = failwith "JS only"
        static member min(array: 'T[], accessor: Func<'T, float, string>): string = failwith "JS only"
        static member min(array: 'T[], accessor: Func<'T, float, 'U>): 'U = failwith "JS only"
        static member max(array: float[]): float = failwith "JS only"
        static member max(array: string[]): string = failwith "JS only"
        static member max(array: 'T[]): 'T = failwith "JS only"
        static member max(array: 'T[], accessor: Func<'T, float, float>): float = failwith "JS only"
        static member max(array: 'T[], accessor: Func<'T, float, string>): string = failwith "JS only"
        static member max(array: 'T[], accessor: Func<'T, float, 'U>): 'U = failwith "JS only"
        static member extent(array: float[]): float * float = failwith "JS only"
        static member extent(array: string[]): string * string = failwith "JS only"
        static member extent(array: 'T[]): 'T * 'T = failwith "JS only"
        static member extent(array: U2<'T, Primitive[]>): U2<'T, Primitive> * U2<'T, Primitive> = failwith "JS only"
        static member extent(array: 'T[], accessor: Func<'T, float, float>): float * float = failwith "JS only"
        static member extent(array: 'T[], accessor: Func<'T, float, string>): string * string = failwith "JS only"
        static member extent(array: 'U[], accessor: Func<'T, float, 'U>): U2<'U, Primitive> * U2<'U, Primitive> = failwith "JS only"
        static member sum(array: float[]): float = failwith "JS only"
        static member sum(array: 'T[], accessor: Func<'T, float, float>): float = failwith "JS only"
        static member mean(array: float[]): float = failwith "JS only"
        static member mean(array: 'T[], accessor: Func<'T, float, float>): float = failwith "JS only"
        static member quantile(array: float[], p: float): float = failwith "JS only"
        static member variance(array: float[]): float = failwith "JS only"
        static member variance(array: 'T[], accessor: Func<'T, float, float>): float = failwith "JS only"
        static member deviation(array: float[]): float = failwith "JS only"
        static member deviation(array: 'T[], accessor: Func<'T, float, float>): float = failwith "JS only"
        static member bisectLeft(array: float[], x: float, ?lo: float, ?hi: float): float = failwith "JS only"
        static member bisectLeft(array: string[], x: string, ?lo: float, ?hi: float): float = failwith "JS only"
        static member bisectRight(array: 'T[], x: 'T, ?lo: float, ?hi: float): float = failwith "JS only"
        static member bisector(accessor: Func<'T, 'U>): obj = failwith "JS only"
        static member bisector(comparator: Func<'T, 'U, float>): obj = failwith "JS only"
        static member shuffle(array: 'T[], ?lo: float, ?hi: float): 'T[] = failwith "JS only"
        static member keys(``object``: obj): string[] = failwith "JS only"
        static member values(``object``: obj): obj[] = failwith "JS only"
        static member entries(``object``: obj): obj[] = failwith "JS only"
        static member map(): Map<'T> = failwith "JS only"
        static member map(``object``: Map<'T>): Map<'T> = failwith "JS only"
        static member map(``object``: obj): Map<'T> = failwith "JS only"
        static member map(array: 'T[], key: Func<'T, float, string>): Map<'T> = failwith "JS only"
        static member map(``object``: obj): Map<obj> = failwith "JS only"
        static member set(): Set = failwith "JS only"
        static member set(array: string[]): Set = failwith "JS only"
        static member merge(arrays: 'T[][]): 'T[] = failwith "JS only"
        static member range(stop: float): float[] = failwith "JS only"
        static member range(start: float, stop: float, ?step: float): float[] = failwith "JS only"
        static member permute(array: obj, keys: float[]): 'T[] = failwith "JS only"
        static member permute(``object``: obj, keys: string[]): 'T[] = failwith "JS only"
        static member zip([<ParamArray>] arrays: 'T[][]): 'T[][] = failwith "JS only"
        static member transpose(matrix: 'T[][]): 'T[][] = failwith "JS only"
        static member pairs(array: 'T[]): ('T * 'T)[] = failwith "JS only"
        static member nest(): Nest<'T> = failwith "JS only"
        static member transform(transform: string): Transform = failwith "JS only"
        static member format(specifier: string): Func<float, string> = failwith "JS only"
        static member formatPrefix(value: float, ?precision: float): FormatPrefix = failwith "JS only"
        static member round(x: float, ?n: float): float = failwith "JS only"
        static member requote(string: string): string = failwith "JS only"
        static member ``functor``(value: 'T): Func<unit,'T> = failwith "JS only"
        static member rebind(target: obj, source: obj, [<ParamArray>] names: string[]): obj = failwith "JS only"
        static member dispatch([<ParamArray>] names: string[]): Dispatch = failwith "JS only"
        static member interpolate(a: float, b: float): Func<float, float> = failwith "JS only"
        static member interpolate(a: string, b: string): Func<float, string> = failwith "JS only"
        static member interpolate(a: U2<string, Color>, b: Color): Func<float, string> = failwith "JS only"
        static member interpolate(a: U2<string, Color[]>, b: Color[]): Func<float, string> = failwith "JS only"
        static member interpolate(a: 'Range[], b: 'Output[]): Func<float, 'Output[]> = failwith "JS only"
        static member interpolate(a: 'Range[], b: 'Range[]): Func<float, 'Output[]> = failwith "JS only"
        static member interpolate(a: obj, b: obj): Func<float, obj> = failwith "JS only"
        static member interpolateNumber(a: float, b: float): Func<float, float> = failwith "JS only"
        static member interpolateRound(a: float, b: float): Func<float, float> = failwith "JS only"
        static member interpolateString(a: string, b: string): Func<float, string> = failwith "JS only"
        static member interpolateRgb(a: U2<string, Color>, b: U2<string, Color>): Func<float, string> = failwith "JS only"
        static member interpolateHsl(a: U2<string, Color>, b: U2<string, Color>): Func<float, string> = failwith "JS only"
        static member interpolateLab(a: U2<string, Color>, b: U2<string, Color>): Func<float, string> = failwith "JS only"
        static member interpolateHcl(a: U2<string, Color>, b: U2<string, Color>): Func<float, string> = failwith "JS only"
        static member interpolateArray(a: U2<string, Color[]>, b: Color[]): Func<float, string[]> = failwith "JS only"
        static member interpolateArray(a: 'Range[], b: 'Range[]): Func<float, 'Output[]> = failwith "JS only"
        static member interpolateArray(a: 'Range[], b: 'Output[]): Func<float, 'Output[]> = failwith "JS only"
        static member interpolateObject(a: obj, b: obj): Func<float, obj> = failwith "JS only"
        static member interpolateTransform(a: U2<string, Transform>, b: U2<string, Transform>): Func<float, string> = failwith "JS only"
        static member interpolateZoom(a: float * float * float, b: float * float * float): obj = failwith "JS only"
        static member xhr(url: string, ?mimeType: string, ?callback: Func<obj, obj, unit>): Xhr = failwith "JS only"
        static member xhr(url: string, callback: Func<obj, obj, unit>): Xhr = failwith "JS only"
        static member text(url: string, ?mimeType: string, ?callback: Func<obj, string, unit>): Xhr = failwith "JS only"
        static member text(url: string, callback: Func<obj, string, unit>): Xhr = failwith "JS only"
        static member json(url: string, ?callback: Func<obj, obj, unit>): Xhr = failwith "JS only"
        static member xml(url: string, ?mimeType: string, ?callback: Func<obj, obj, unit>): Xhr = failwith "JS only"
        static member xml(url: string, callback: Func<obj, obj, unit>): Xhr = failwith "JS only"
        static member html(url: string, ?callback: Func<obj, DocumentFragment, unit>): Xhr = failwith "JS only"
        static member dsv(delimiter: string, mimeType: string): Dsv = failwith "JS only"
        static member locale(definition: LocaleDefinition): Locale = failwith "JS only"

    module Selection =
        type Group =
            // inherit EventTarget[] // TODO
            abstract parentNode: EventTarget with get, set

        and Update<'Datum> =
            [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: float -> Group with get, set
            abstract length: float with get, set
            abstract attr: name: string -> string
            abstract attr: name: string * value: Primitive -> Update<'Datum>
            abstract attr: name: string * value: Func<'Datum, float, float, Primitive> -> Update<'Datum>
            abstract attr: obj: obj -> Update<'Datum>
            abstract classed: name: string -> bool
            abstract classed: name: string * value: bool -> Update<'Datum>
            abstract classed: name: string * value: Func<'Datum, float, float, bool> -> Update<'Datum>
            abstract classed: obj: obj -> Update<'Datum>
            abstract style: name: string -> string
            abstract style: name: string * value: Primitive * ?priority: string -> Update<'Datum>
            abstract style: name: string * value: Func<'Datum, float, float, Primitive> * ?priority: string -> Update<'Datum>
            abstract style: obj: obj * ?priority: string -> Update<'Datum>
            abstract property: name: string -> obj
            abstract property: name: string * value: obj -> Update<'Datum>
            abstract property: name: string * value: Func<'Datum, float, float, obj> -> Update<'Datum>
            abstract property: obj: obj -> Update<'Datum>
            abstract text: unit -> string
            abstract text: value: Primitive -> Update<'Datum>
            abstract text: value: Func<'Datum, float, float, Primitive> -> Update<'Datum>
            abstract html: unit -> string
            abstract html: value: string -> Selection<'Datum>
            abstract html: value: Func<'Datum, float, float, string> -> Selection<'Datum>
            abstract append: name: string -> Selection<'Datum>
            abstract append: name: Func<'Datum, float, float, EventTarget> -> Update<'Datum>
            abstract insert: name: string * before: string -> Update<'Datum>
            abstract insert: name: string * before: Func<'Datum, float, float, EventTarget> -> Update<'Datum>
            abstract insert: name: Func<'Datum, float, float, EventTarget> * before: string -> Update<'Datum>
            abstract insert: name: Func<'Datum, float, float, EventTarget> * before: Func<'Datum, float, float, EventTarget> -> Update<'Datum>
            abstract remove: unit -> Update<'Datum>
            abstract data: unit -> 'Datum[]
            abstract data: data: 'NewDatum[] * ?key: Func<'NewDatum, float, float, string> -> Update<'NewDatum>
            abstract data: data: Func<'Datum, float, float, 'NewDatum[]> * ?key: Func<'NewDatum, float, float, string> -> Update<'NewDatum>
            abstract filter: selector: string -> Update<'Datum>
            abstract filter: selector: Func<'Datum, float, float, bool> -> Update<'Datum>
            abstract datum: unit -> 'Datum
            abstract datum: value: 'NewDatum -> Update<'NewDatum>
            abstract datum: value: Func<'Datum, float, float, 'NewDatum> -> Update<'NewDatum>
            abstract sort: ?comparator: Func<'Datum, 'Datum, float> -> Update<'Datum>
            abstract order: unit -> Update<'Datum>
            abstract on: ``type``: string -> Func<'Datum, float, float, obj>
            abstract on: ``type``: string * listener: Func<'Datum, float, float, obj> * ?capture: bool -> Update<'Datum>
            abstract transition: ?name: string -> Transition<'Datum>
            abstract interrupt: ?name: string -> Update<'Datum>
            abstract select: selector: string -> Update<'Datum>
            abstract select: selector: Func<'Datum, float, float, EventTarget> -> Update<'Datum>
            abstract selectAll: selector: string -> Update<'Datum>
            abstract selectAll: selector: Func<'Datum, float, float, U2<EventTarget[], NodeList>> -> Update<obj>
            abstract each: func: Func<'Datum, float, float, obj> -> Update<'Datum>
            abstract call: func: Func<Update<'Datum>, obj, obj> * [<ParamArray>] args: obj[] -> Update<'Datum>
            abstract empty: unit -> bool
            abstract node: unit -> Node
            abstract size: unit -> float
            abstract enter: unit -> Enter<'Datum>
            abstract exit: unit -> Selection<'Datum>

        and Enter<'Datum> =
            abstract append: name: string -> Selection<'Datum>
            abstract append: name: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
            abstract insert: name: string * ?before: string -> Selection<'Datum>
            abstract insert: name: string * before: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
            abstract insert: name: Func<'Datum, float, float, EventTarget> * ?before: string -> Selection<'Datum>
            abstract insert: name: Func<'Datum, float, float, EventTarget> * before: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
            abstract select: name: Func<'Datum, float, float, EventTarget> -> Selection<'Datum>
            abstract call: func: Func<Enter<'Datum>, obj, obj> * [<ParamArray>] args: obj[] -> Enter<'Datum>
            abstract empty: unit -> bool
            abstract size: unit -> float

        type [<Import("selection.prototype","d3")>] Globals =
            static member prototype: Selection<obj> = failwith "JS only"

    module Transition =
        type [<Import("transition.prototype","d3")>] Globals =
            static member  prototype: Transition<obj> = failwith "JS only"

    module Timer =
        type [<Import("timer","d3")>] Globals =
            static member flush(): unit = failwith "JS only"

    module Random =
        type [<Import("random","d3")>] Globals =
            static member normal(?mean: float, ?deviation: float): Func<unit, float> = failwith "JS only"
            static member logNormal(?mean: float, ?deviation: float): Func<unit, float> = failwith "JS only"
            static member bates(count: float): Func<unit, float> = failwith "JS only"
            static member irwinHall(count: float): Func<unit, float> = failwith "JS only"

    module Ns =
        type Qualified =
            abstract space: string with get, set
            abstract local: string with get, set

        and prefixType =
            [<Emit("$0[$1]{{=$2}}")>] abstract Item: key: string -> string with get, set

        type [<Import("ns","d3")>] Globals =
            static member prefix with get(): prefixType = failwith "JS only" and set(v: prefixType): unit = failwith "JS only"
            static member qualify(name: string): U2<Qualified, string> = failwith "JS only"

    module Scale =
        type Identity =
            [<Emit("$0($1...)")>] abstract Invoke: n: float -> float
            abstract invert: n: float -> float
            abstract domain: unit -> float[]
            abstract domain: numbers: float[] -> Identity
            abstract range: unit -> float[]
            abstract range: numbers: float[] -> Identity
            abstract ticks: ?count: float -> float[]
            abstract tickFormat: ?count: float * ?format: string -> Func<float, string>
            abstract copy: unit -> Identity

        and Linear<'Range, 'Output> =
            [<Emit("$0($1...)")>] abstract Invoke: x: float -> 'Output
            abstract invert: y: float -> float
            abstract domain: unit -> float[]
            abstract domain: numbers: float[] -> Linear<'Range, 'Output>
            abstract range: unit -> 'Range[]
            abstract range: values: 'Range[] -> Linear<'Range, 'Output>
            abstract rangeRound: values: float[] -> Linear<float, float>
            abstract interpolate: unit -> Func<'Range, 'Range, Func<float, 'Output>>
            abstract interpolate: factory: Func<'Range, 'Range, Func<float, 'Output>> -> Linear<'Range, 'Output>
            abstract clamp: unit -> bool
            abstract clamp: clamp: bool -> Linear<'Range, 'Output>
            abstract nice: ?count: float -> Linear<'Range, 'Output>
            abstract ticks: ?count: float -> float[]
            abstract tickFormat: ?count: float * ?format: string -> Func<float, string>
            abstract copy: unit -> Linear<'Range, 'Output>

        and Pow<'Range, 'Output> =
            [<Emit("$0($1...)")>] abstract Invoke: x: float -> 'Output
            abstract invert: y: float -> float
            abstract domain: unit -> float[]
            abstract domain: numbers: float[] -> Pow<'Range, 'Output>
            abstract range: unit -> 'Range[]
            abstract range: values: 'Range[] -> Pow<'Range, 'Output>
            abstract rangeRound: values: float[] -> Pow<float, float>
            abstract exponent: unit -> float
            abstract exponent: k: float -> Pow<'Range, 'Output>
            abstract interpolate: unit -> Func<'Range, 'Range, Func<float, 'Output>>
            abstract interpolate: factory: Func<'Range, 'Range, Func<float, 'Output>> -> Pow<'Range, 'Output>
            abstract clamp: unit -> bool
            abstract clamp: clamp: bool -> Pow<'Range, 'Output>
            abstract nice: ?m: float -> Pow<'Range, 'Output>
            abstract ticks: ?count: float -> float[]
            abstract tickFormat: ?count: float * ?format: string -> Func<float, string>
            abstract copy: unit -> Pow<'Range, 'Output>

        and Log<'Range, 'Output> =
            [<Emit("$0($1...)")>] abstract Invoke: x: float -> 'Output
            abstract invert: y: float -> float
            abstract domain: unit -> float[]
            abstract domain: numbers: float[] -> Log<'Range, 'Output>
            abstract range: unit -> 'Range[]
            abstract range: values: 'Range[] -> Log<'Range, 'Output>
            abstract rangeRound: values: float[] -> Log<float, float>
            abstract ``base``: unit -> float
            abstract ``base``: ``base``: float -> Log<'Range, 'Output>
            abstract interpolate: unit -> Func<'Range, 'Range, Func<float, 'Output>>
            abstract interpolate: factory: Func<'Range, 'Range, Func<float, 'Output>> -> Log<'Range, 'Output>
            abstract clamp: unit -> bool
            abstract clamp: clamp: bool -> Log<'Range, 'Output>
            abstract nice: unit -> Log<'Range, 'Output>
            abstract ticks: unit -> float[]
            abstract tickFormat: ?count: float * ?format: string -> Func<float, string>
            abstract copy: unit -> Log<'Range, 'Output>

        and Quantize<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: x: float -> 'T
            abstract invertExtent: y: 'T -> float * float
            abstract domain: unit -> float[]
            abstract domain: numbers: float[] -> Quantize<'T>
            abstract range: unit -> 'T[]
            abstract range: values: 'T[] -> Quantize<'T>
            abstract copy: unit -> Quantize<'T>

        and Quantile<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: x: float -> 'T
            abstract invertExtent: y: 'T -> float * float
            abstract domain: unit -> float[]
            abstract domain: numbers: float[] -> Quantile<'T>
            abstract range: unit -> 'T[]
            abstract range: values: 'T[] -> Quantile<'T>
            abstract quantiles: unit -> float[]
            abstract copy: unit -> Quantile<'T>

        and Threshold<'Domain, 'Range> =
            [<Emit("$0($1...)")>] abstract Invoke: x: float -> 'Range
            abstract invertExtent: y: 'Range -> 'Domain * 'Domain
            abstract domain: unit -> 'Domain[]
            abstract domain: domain: 'Domain[] -> Threshold<'Domain, 'Range>
            abstract range: unit -> 'Range[]
            abstract range: values: 'Range[] -> Threshold<'Domain, 'Range>
            abstract copy: unit -> Threshold<'Domain, 'Range>

        and Ordinal<'Domain, 'Range> =
            [<Emit("$0($1...)")>] abstract Invoke: x: 'Domain -> 'Range
            abstract domain: unit -> 'Domain[]
            abstract domain: values: 'Domain[] -> Ordinal<'Domain, 'Range>
            abstract range: unit -> 'Range[]
            abstract range: values: 'Range[] -> Ordinal<'Domain, 'Range>
            abstract rangePoints: interval: float * float * ?padding: float -> Ordinal<'Domain, float>
            abstract rangeRoundPoints: interval: float * float * ?padding: float -> Ordinal<'Domain, float>
            abstract rangeBands: interval: float * float * ?padding: float * ?outerPadding: float -> Ordinal<'Domain, float>
            abstract rangeRoundBands: interval: float * float * ?padding: float * ?outerPadding: float -> Ordinal<'Domain, float>
            abstract rangeBand: unit -> float
            abstract rangeExtent: unit -> float * float
            abstract copy: unit -> Ordinal<'Domain, 'Range>

        type [<Import("scale","d3")>] Globals =
            static member identity(): Identity = failwith "JS only"
            static member linear(): Linear<float, float> = failwith "JS only"
            static member linear(): Linear<'Output, 'Output> = failwith "JS only"
            static member linear(): Linear<'Range, 'Output> = failwith "JS only"
            static member sqrt(): Pow<float, float> = failwith "JS only"
            static member sqrt(): Pow<'Output, 'Output> = failwith "JS only"
            static member sqrt(): Pow<'Range, 'Output> = failwith "JS only"
            static member pow(): Pow<float, float> = failwith "JS only"
            static member pow(): Pow<'Output, 'Output> = failwith "JS only"
            static member pow(): Pow<'Range, 'Output> = failwith "JS only"
            static member log(): Log<float, float> = failwith "JS only"
            static member log(): Log<'Output, 'Output> = failwith "JS only"
            static member log(): Log<'Range, 'Output> = failwith "JS only"
            static member quantize(): Quantize<'T> = failwith "JS only"
            static member quantile(): Quantile<'T> = failwith "JS only"
            static member threshold(): Threshold<float, 'Range> = failwith "JS only"
            static member threshold(): Threshold<'Domain, 'Range> = failwith "JS only"
            static member ordinal(): Ordinal<string, 'Range> = failwith "JS only"
            static member ordinal(): Ordinal<'Domain, 'Range> = failwith "JS only"
            static member category10(): Ordinal<string, string> = failwith "JS only"
            static member category10(): Ordinal<'Domain, string> = failwith "JS only"
            static member category20(): Ordinal<string, string> = failwith "JS only"
            static member category20(): Ordinal<'Domain, string> = failwith "JS only"
            static member category20b(): Ordinal<string, string> = failwith "JS only"
            static member category20b(): Ordinal<'Domain, string> = failwith "JS only"
            static member category20c(): Ordinal<string, string> = failwith "JS only"
            static member category20c(): Ordinal<'Domain, string> = failwith "JS only"

    module Time =
        type Interval =
            abstract utc: obj with get, set
            [<Emit("$0($1...)")>] abstract Invoke: d: DateTime -> DateTime
            abstract floor: d: DateTime -> DateTime
            abstract round: d: DateTime -> DateTime
            abstract ceil: d: DateTime -> DateTime
            abstract range: start: DateTime * stop: DateTime * ?step: float -> DateTime[]
            abstract offset: date: DateTime * step: float -> DateTime

        and Format =
            [<Emit("$0($1...)")>] abstract Invoke: d: DateTime -> string
            abstract parse: input: string -> DateTime

        and Scale<'Range, 'Output> =
            [<Emit("$0($1...)")>] abstract Invoke: x: DateTime -> 'Output
            abstract invert: y: float -> DateTime
            abstract domain: unit -> DateTime[]
            abstract domain: dates: float[] -> Scale<'Range, 'Output>
            abstract domain: dates: DateTime[] -> Scale<'Range, 'Output>
            abstract nice: unit -> Scale<'Range, 'Output>
            abstract nice: interval: Interval * ?step: float -> Scale<'Range, 'Output>
            abstract range: unit -> 'Range[]
            abstract range: values: 'Range[] -> Scale<'Range, 'Output>
            abstract rangeRound: values: float[] -> Scale<float, float>
            abstract interpolate: unit -> Func<'Range, 'Range, Func<float, 'Output>>
            abstract interpolate: factory: Func<'Range, 'Range, Func<float, 'Output>> -> Scale<'Range, 'Output>
            abstract clamp: unit -> bool
            abstract clamp: clamp: bool -> Scale<'Range, 'Output>
            abstract ticks: unit -> DateTime[]
            abstract ticks: interval: Interval * ?step: float -> DateTime[]
            abstract ticks: count: float -> DateTime[]
            abstract tickFormat: count: float -> Func<DateTime, string>
            abstract copy: unit -> Scale<'Range, 'Output>

        type [<Import("time","d3")>] Globals =
            static member second with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member minute with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member hour with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member day with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member week with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member sunday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member monday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member tuesday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member wednesday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member thursday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member friday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member saturday with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member month with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member year with get(): Interval = failwith "JS only" and set(v: Interval): unit = failwith "JS only"
            static member seconds(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member minutes(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member hours(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member days(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member weeks(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member sundays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member mondays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member tuesdays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member wednesdays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member thursdays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member fridays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member saturdays(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member months(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member years(start: DateTime, stop: DateTime, ?step: float): DateTime[] = failwith "JS only"
            static member dayOfYear(d: DateTime): float = failwith "JS only"
            static member weekOfYear(d: DateTime): float = failwith "JS only"
            static member sundayOfYear(d: DateTime): float = failwith "JS only"
            static member mondayOfYear(d: DateTime): float = failwith "JS only"
            static member tuesdayOfYear(d: DateTime): float = failwith "JS only"
            static member wednesdayOfYear(d: DateTime): float = failwith "JS only"
            static member fridayOfYear(d: DateTime): float = failwith "JS only"
            static member saturdayOfYear(d: DateTime): float = failwith "JS only"
            static member format(specifier: string): Format = failwith "JS only"
            static member scale(): Scale<float, float> = failwith "JS only"
            static member scale(): Scale<'Output, 'Output> = failwith "JS only"
            static member scale(): Scale<'Range, 'Output> = failwith "JS only"

        module FormatModule =
            type [<Import("time.format","d3")>] Globals =
                static member iso with get(): Format = failwith "JS only" and set(v: Format): unit = failwith "JS only"
                static member multi(formats: string * Func<DateTime, U2<bool, float[]>>): Format = failwith "JS only"
                static member utc(specifier: string): Format = failwith "JS only"

            module Utc =
                type [<Import("time.format.utc","d3")>] Globals =
                    static member multi(formats: string * Func<DateTime, U2<bool, float[]>>): Format = failwith "JS only"

        module Scale =
            type [<Import("time.scale","d3")>] Globals =
                static member utc(): Scale<float, float> = failwith "JS only"
                static member utc(): Scale<'Output, 'Output> = failwith "JS only"
                static member utc(): Scale<'Range, 'Output> = failwith "JS only"

    module Behavior =
        module Zoom =
            type Scale =
                abstract domain: unit -> float[]
                abstract domain: values: float[] -> Scale
                abstract invert: y: float -> float
                abstract range: values: float[] -> Scale
                abstract range: unit -> float[]
    
        type Drag<'Datum> =
            [<Emit("$0($1...)")>] abstract Invoke: selection: Selection<'Datum> -> unit
            abstract on: ``type``: string -> Func<'Datum, float, obj>
            abstract on: ``type``: string * listener: Func<'Datum, float, obj> -> Drag<'Datum>
            abstract origin: unit -> Func<'Datum, float, obj>
            abstract origin: accessor: Func<'Datum, float, obj> -> Drag<'Datum>

        and Zoom<'Datum> =
            [<Emit("$0($1...)")>] abstract Invoke: selection: Selection<'Datum> -> unit
            abstract translate: unit -> float * float
            abstract translate: translate: (float * float) -> Zoom<'Datum>
            abstract scale: unit -> float
            abstract scale: scale: float -> Zoom<'Datum>
            abstract scaleExtent: unit -> float * float
            abstract scaleExtent: extent: (float * float) -> Zoom<'Datum>
            abstract center: unit -> float * float
            abstract center: center: (float * float) -> Zoom<'Datum>
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Zoom<'Datum>
            abstract x: unit -> Zoom.Scale
            abstract x: x: Zoom.Scale -> Zoom<'Datum>
            abstract y: unit -> Zoom.Scale
            abstract y: y: Zoom.Scale -> Zoom<'Datum>
            abstract on: ``type``: string -> Func<'Datum, float, obj>
            abstract on: ``type``: string * listener: Func<'Datum, float, obj> -> Zoom<'Datum>
            abstract ``event``: selection: Selection<'Datum> -> unit
            abstract ``event``: transition: Transition<'Datum> -> unit

        type [<Import("behavior","d3")>] Globals =
            static member drag(): Drag<'Datum> = failwith "JS only"
            static member zoom(): Zoom<'Datum> = failwith "JS only"

    module Geo =
        type Path =
            [<Emit("$0($1...)")>] abstract Invoke: feature: obj * ?index: float -> string
            abstract area: feature: obj -> float
            abstract centroid: feature: obj -> float * float
            abstract bounds: feature: obj -> float * float * float * float
            abstract projection: unit -> U2<Transform, Func<float * float, float * float>>
            abstract projection: stream: Transform -> Path
            abstract projection: projection: Func<float * float, float * float> -> Path
            abstract pointRadius: unit -> U2<float, Func<obj, float, float>>
            abstract pointRadius: radius: float -> Path
            abstract pointRadius: radius: Func<obj, float, float> -> Path
            abstract context: unit -> CanvasRenderingContext2D
            abstract context: context: CanvasRenderingContext2D -> Path

        and Graticule =
            [<Emit("$0($1...)")>] abstract Invoke: unit -> obj
            abstract lines: unit -> obj[]
            abstract outline: unit -> obj
            abstract extent: unit -> float * float * float * float
            abstract extent: extent: (float * float * float * float) -> Graticule
            abstract majorExtent: unit -> float * float * float * float
            abstract majorExtent: extent: (float * float * float * float) -> Graticule
            abstract minorExtent: unit -> float * float * float * float
            abstract minorExtent: extent: (float * float * float * float) -> Graticule
            abstract step: unit -> float * float
            abstract step: step: (float * float) -> Graticule
            abstract majorStep: unit -> float * float
            abstract majorStep: step: (float * float) -> Graticule
            abstract minorStep: unit -> float * float
            abstract minorStep: step: (float * float) -> Graticule
            abstract precision: unit -> float
            abstract precision: precision: float -> Graticule

        and Circle =
            [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: obj[] -> obj
            abstract origin: unit -> U2<float * float, Func<obj, float * float>>
            abstract origin: origin: (float * float) -> Circle
            abstract origin: origin: Func<obj, float * float> -> Circle
            abstract angle: unit -> float
            abstract angle: angle: float -> Circle
            abstract precision: unit -> float
            abstract precision: precision: float -> Circle

        and Rotation =
            [<Emit("$0($1...)")>] abstract Invoke: location: (float * float) -> float * float
            abstract invert: location: (float * float) -> float * float

        and Listener =
            abstract point: x: float * y: float * z: float -> unit
            abstract lineStart: unit -> unit
            abstract lineEnd: unit -> unit
            abstract polygonStart: unit -> unit
            abstract polygonEnd: unit -> unit
            abstract sphere: unit -> unit

        and TransformMethods =
            abstract point: x: float * y: float * z: float -> unit
            abstract lineStart: unit -> unit
            abstract lineEnd: unit -> unit
            abstract polygonStart: unit -> unit
            abstract polygonEnd: unit -> unit
            abstract sphere: unit -> unit

        and Transform =
            abstract stream: stream: Listener -> Listener

        and ClipExtent =
            inherit Transform
            abstract extent: unit -> float * float * float * float
            abstract extent: extent: (float * float * float * float) -> ClipExtent

        and Projection =
            [<Emit("$0($1...)")>] abstract Invoke: location: (float * float) -> float * float
            abstract rotate: unit -> float * float * float
            abstract rotate: rotation: (float * float * float) -> Projection
            abstract center: unit -> float * float
            abstract center: location: (float * float) -> Projection
            abstract translate: unit -> float * float
            abstract translate: point: (float * float) -> Projection
            abstract scale: unit -> float
            abstract scale: scale: float -> Projection
            abstract clipAngle: unit -> float
            abstract clipAngle: angle: float -> Projection
            abstract clipExtent: unit -> float * float * float * float
            abstract clipExtent: extent: (float * float * float * float) -> Projection
            abstract precision: unit -> float
            abstract precision: precision: float -> Projection
            abstract stream: listener: Listener -> Listener

        and InvertibleProjection =
            inherit Projection
            abstract invert: point: (float * float) -> float * float

        and ConicProjection =
            inherit InvertibleProjection
            abstract parallels: unit -> float * float
            abstract parallels: parallels: (float * float) -> ConicProjection
            abstract rotate: unit -> float * float * float
            abstract rotate: rotation: (float * float * float) -> ConicProjection
            abstract center: unit -> float * float
            abstract center: location: (float * float) -> ConicProjection
            abstract translate: unit -> float * float
            abstract translate: point: (float * float) -> ConicProjection
            abstract scale: unit -> float
            abstract scale: scale: float -> ConicProjection
            abstract clipAngle: unit -> float
            abstract clipAngle: angle: float -> ConicProjection
            abstract clipExtent: unit -> float * float * float * float
            abstract clipExtent: extent: (float * float * float * float) -> ConicProjection
            abstract precision: unit -> float
            abstract precision: precision: float -> ConicProjection

        and RawProjection =
            [<Emit("$0($1...)")>] abstract Invoke: lambda: float * phi: float -> float * float

        and RawInvertibleProjection =
            inherit RawProjection
            abstract invert: x: float * y: float -> float * float

        type [<Import("geo","d3")>] Globals =
            static member path(): Path = failwith "JS only"
            static member graticule(): Graticule = failwith "JS only"
            static member circle(): Circle = failwith "JS only"
            static member area(feature: obj): float = failwith "JS only"
            static member centroid(feature: obj): float * float = failwith "JS only"
            static member bounds(feature: obj): float * float * float * float = failwith "JS only"
            static member distance(a: float * float, b: float * float): float = failwith "JS only"
            static member length(feature: obj): float = failwith "JS only"
            static member interpolate(a: float * float, b: float * float): Func<float, float * float> = failwith "JS only"
            static member rotation(rotate: U2<float * float, float * float * float>): Rotation = failwith "JS only"
            static member stream(``object``: obj, listener: Listener): unit = failwith "JS only"
            static member transform(methods: TransformMethods): Transform = failwith "JS only"
            static member clipExtent(): ClipExtent = failwith "JS only"
            static member projection(raw: RawInvertibleProjection): InvertibleProjection = failwith "JS only"
            static member projection(raw: RawProjection): Projection = failwith "JS only"
            static member projectionMutator(factory: Func<obj, RawInvertibleProjection>): Func<obj, InvertibleProjection> = failwith "JS only"
            static member projectionMutator(factory: Func<obj, RawProjection>): Func<obj, Projection> = failwith "JS only"
            static member albers(): ConicProjection = failwith "JS only"
            static member albersUsa(): ConicProjection = failwith "JS only"
            static member azimuthalEqualArea(): InvertibleProjection = failwith "JS only"
            static member azimuthalEquidistant(): InvertibleProjection = failwith "JS only"
            static member conicConformal(): ConicProjection = failwith "JS only"
            static member conicEqualArea(): ConicProjection = failwith "JS only"
            static member conicEquidistant(): ConicProjection = failwith "JS only"
            static member equirectangular(): InvertibleProjection = failwith "JS only"
            static member gnomonic(): InvertibleProjection = failwith "JS only"
            static member mercator(): InvertibleProjection = failwith "JS only"
            static member orthographic(): InvertibleProjection = failwith "JS only"
            static member stereographic(): InvertibleProjection = failwith "JS only"
            static member transverseMercator(): InvertibleProjection = failwith "JS only"

        module AzimuthalEqualArea =
            [<Import("geo.azimuthalEqualArea.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.azimuthalEqualArea.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module AzimuthalEquidistant =
            [<Import("geo.azimuthalEquidistant.raw.invert","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.azimuthalEquidistant.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module ConicConformal =
            [<Import("geo.conicConformal.raw","d3")>]
            let raw(phi0: float, phi1: float): RawInvertibleProjection = failwith "JS only"

        module ConicEqualArea =
            [<Import("geo.conicEqualArea.raw","d3")>]
            let raw(phi0: float, phi1: float): RawInvertibleProjection = failwith "JS only"

        module ConicEquidistant =
            [<Import("geo.conicEquidistant.raw","d3")>]
            let raw(phi0: float, phi1: float): RawInvertibleProjection = failwith "JS only"

        module Equirectangular =
            [<Import("geo.equirectangular.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.equirectangular.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module Gnomonic =
            [<Import("geo.gnomonic.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.gnomonic.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module Mercator =
            [<Import("geo.mercator.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.mercator.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module Orthographic =
            [<Import("geo.orthographic.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.orthographic.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module Stereographic =
            [<Import("geo.stereographic.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.stereographic.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

        module TransverseMercator =
            [<Import("geo.transverseMercator.raw","d3")>]
            let raw(lambda: float, phi: float): float * float = failwith "JS only"

            module Raw =
                [<Import("geo.transverseMercator.raw.invert","d3")>]
                let invert(x: float, y: float): float * float = failwith "JS only"

    module Svg =
        module Line =
            type Radial<'T> =
                [<Emit("$0($1...)")>] abstract Invoke: data: 'T[] -> string
                abstract radius: unit -> U2<float, Func<'T, float, float>>
                abstract radius: radius: float -> Radial<'T>
                abstract radius: radius: Func<'T, float, float> -> Radial<'T>
                abstract angle: unit -> U2<float, Func<'T, float, float>>
                abstract angle: angle: float -> Radial<'T>
                abstract angle: angle: Func<'T, float, float> -> Radial<'T>
                abstract interpolate: unit -> U2<string, Func<(float * float)[], string>>
                [<Emit("$0.interpolate('linear')")>] abstract interpolate_linear: unit -> Radial<'T>
                [<Emit("$0.interpolate('linear-closed')")>] abstract ``interpolate_linear-closed``: unit -> Radial<'T>
                [<Emit("$0.interpolate('step')")>] abstract interpolate_step: unit -> Radial<'T>
                [<Emit("$0.interpolate('step-before')")>] abstract ``interpolate_step-before``: unit -> Radial<'T>
                [<Emit("$0.interpolate('step-after')")>] abstract ``interpolate_step-after``: unit -> Radial<'T>
                [<Emit("$0.interpolate('basis')")>] abstract interpolate_basis: unit -> Radial<'T>
                [<Emit("$0.interpolate('basis-open')")>] abstract ``interpolate_basis-open``: unit -> Radial<'T>
                [<Emit("$0.interpolate('basis-closed')")>] abstract ``interpolate_basis-closed``: unit -> Radial<'T>
                [<Emit("$0.interpolate('bundle')")>] abstract interpolate_bundle: unit -> Radial<'T>
                [<Emit("$0.interpolate('cardinal')")>] abstract interpolate_cardinal: unit -> Radial<'T>
                [<Emit("$0.interpolate('cardinal-open')")>] abstract ``interpolate_cardinal-open``: unit -> Radial<'T>
                [<Emit("$0.interpolate('cardinal-closed')")>] abstract ``interpolate_cardinal-closed``: unit -> Radial<'T>
                [<Emit("$0.interpolate('monotone')")>] abstract interpolate_monotone: unit -> Radial<'T>
                abstract interpolate: interpolate: U2<string, Func<(float * float)[], string>> -> Radial<'T>
                abstract tension: unit -> float
                abstract tension: tension: float -> Radial<'T>
                abstract defined: unit -> Func<'T, float, bool>
                abstract defined: defined: Func<'T, float, bool> -> Radial<'T>

            type [<Import("svg.line.radial","d3")>] Globals =
                static member radial(): Radial<'T> = failwith "JS only"

        module Area =
            type Radial<'T> =
                [<Emit("$0($1...)")>] abstract Invoke: data: 'T[] -> string
                abstract radius: unit -> U2<float, Func<'T, float, float>>
                abstract radius: radius: float -> Radial<'T>
                abstract radius: radius: Func<'T, float, float> -> Radial<'T>
                abstract innerRadius: unit -> U2<float, Func<'T, float, float>>
                abstract innerRadius: innerRadius: float -> Radial<'T>
                abstract innerRadius: innerRadius: Func<'T, float, float> -> Radial<'T>
                abstract outerRadius: unit -> U2<float, Func<'T, float, float>>
                abstract outerRadius: outerRadius: float -> Radial<'T>
                abstract outerRadius: outerRadius: Func<'T, float, float> -> Radial<'T>
                abstract angle: unit -> U2<float, Func<'T, float, float>>
                abstract angle: angle: float -> Radial<'T>
                abstract angle: angle: Func<'T, float, float> -> Radial<'T>
                abstract startAngle: unit -> U2<float, Func<'T, float, float>>
                abstract startAngle: startAngle: float -> Radial<'T>
                abstract startAngle: startAngle: Func<'T, float, float> -> Radial<'T>
                abstract endAngle: unit -> U2<float, Func<'T, float, float>>
                abstract endAngle: endAngle: float -> Radial<'T>
                abstract endAngle: endAngle: Func<'T, float, float> -> Radial<'T>
                abstract interpolate: unit -> U2<string, Func<(float * float)[], string>>
                [<Emit("$0.interpolate('linear')")>] abstract interpolate_linear: unit -> Radial<'T>
                [<Emit("$0.interpolate('step')")>] abstract interpolate_step: unit -> Radial<'T>
                [<Emit("$0.interpolate('step-before')")>] abstract ``interpolate_step-before``: unit -> Radial<'T>
                [<Emit("$0.interpolate('step-after')")>] abstract ``interpolate_step-after``: unit -> Radial<'T>
                [<Emit("$0.interpolate('basis')")>] abstract interpolate_basis: unit -> Radial<'T>
                [<Emit("$0.interpolate('basis-open')")>] abstract ``interpolate_basis-open``: unit -> Radial<'T>
                [<Emit("$0.interpolate('cardinal')")>] abstract interpolate_cardinal: unit -> Radial<'T>
                [<Emit("$0.interpolate('cardinal-open')")>] abstract ``interpolate_cardinal-open``: unit -> Radial<'T>
                [<Emit("$0.interpolate('monotone')")>] abstract interpolate_monotone: unit -> Radial<'T>
                abstract interpolate: interpolate: U2<string, Func<(float * float)[], string>> -> Radial<'T>
                abstract tension: unit -> float
                abstract tension: tension: float -> Radial<'T>
                abstract defined: unit -> Func<'T, float, bool>
                abstract defined: defined: Func<'T, float, bool> -> Radial<'T>

            type [<Import("svg.area.radial","d3")>] Globals =
                static member radial(): Radial<'T> = failwith "JS only"

        module Arc =
            type Arc =
                abstract innerRadius: float with get, set
                abstract outerRadius: float with get, set
                abstract startAngle: float with get, set
                abstract endAngle: float with get, set
                abstract padAngle: float with get, set

        module ChordModule =
            type Link<'Node> =
                abstract source: 'Node with get, set
                abstract target: 'Node with get, set

            and Node =
                abstract radius: float with get, set
                abstract startAngle: float with get, set
                abstract endAngle: float with get, set

        module Diagonal =
            type Link<'Node> =
                abstract source: 'Node with get, set
                abstract target: 'Node with get, set

            and Node =
                abstract x: float with get, set
                abstract y: float with get, set

            type Radial<'Link, 'Node> =
                [<Emit("$0($1...)")>] abstract Invoke: d: 'Link * i: float -> string
                abstract source: unit -> Func<'Link, float, 'Node>
                abstract source: source: 'Node -> Radial<'Link, 'Node>
                abstract source: source: Func<'Link, float, 'Node> -> Radial<'Link, 'Node>
                abstract target: unit -> Func<'Link, float, 'Node>
                abstract target: target: 'Node -> Radial<'Link, 'Node>
                abstract target: target: Func<'Link, float, 'Node> -> Radial<'Link, 'Node>
                abstract projection: unit -> Func<'Node, float, float * float>
                abstract projection: projection: Func<'Node, float, float * float> -> Radial<'Link, 'Node>

            type [<Import("svg.diagonal","d3")>] Globals =
                static member radial(): Radial<Link<Node>, Node> = failwith "JS only"
                static member radial(): Radial<Link<'Node>, 'Node> = failwith "JS only"
                static member radial(): Radial<'Link, 'Node> = failwith "JS only"

        module Brush =
            type Scale =
                abstract domain: unit -> float[]
                abstract domain: domain: float[] -> Scale
                abstract range: unit -> float[]
                abstract range: range: float[] -> Scale
                abstract invert: y: float -> float
    
        type Line<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: data: 'T[] -> string
            abstract x: unit -> U2<float, Func<'T, float, float>>
            abstract x: x: float -> Line<'T>
            abstract x: x: Func<'T, float, float> -> Line<'T>
            abstract y: unit -> U2<float, Func<'T, float, float>>
            abstract y: x: float -> Line<'T>
            abstract y: y: Func<'T, float, float> -> Line<'T>
            abstract interpolate: unit -> U2<string, Func<(float * float)[], string>>
            [<Emit("$0.interpolate('linear')")>] abstract interpolate_linear: unit -> Line<'T>
            [<Emit("$0.interpolate('linear-closed')")>] abstract ``interpolate_linear-closed``: unit -> Line<'T>
            [<Emit("$0.interpolate('step')")>] abstract interpolate_step: unit -> Line<'T>
            [<Emit("$0.interpolate('step-before')")>] abstract ``interpolate_step-before``: unit -> Line<'T>
            [<Emit("$0.interpolate('step-after')")>] abstract ``interpolate_step-after``: unit -> Line<'T>
            [<Emit("$0.interpolate('basis')")>] abstract interpolate_basis: unit -> Line<'T>
            [<Emit("$0.interpolate('basis-open')")>] abstract ``interpolate_basis-open``: unit -> Line<'T>
            [<Emit("$0.interpolate('basis-closed')")>] abstract ``interpolate_basis-closed``: unit -> Line<'T>
            [<Emit("$0.interpolate('bundle')")>] abstract interpolate_bundle: unit -> Line<'T>
            [<Emit("$0.interpolate('cardinal')")>] abstract interpolate_cardinal: unit -> Line<'T>
            [<Emit("$0.interpolate('cardinal-open')")>] abstract ``interpolate_cardinal-open``: unit -> Line<'T>
            [<Emit("$0.interpolate('cardinal-closed')")>] abstract ``interpolate_cardinal-closed``: unit -> Line<'T>
            [<Emit("$0.interpolate('monotone')")>] abstract interpolate_monotone: unit -> Line<'T>
            abstract interpolate: interpolate: U2<string, Func<(float * float)[], string>> -> Line<'T>
            abstract tension: unit -> float
            abstract tension: tension: float -> Line<'T>
            abstract defined: unit -> Func<'T, float, bool>
            abstract defined: defined: Func<'T, float, bool> -> Line<'T>

        and Area<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: data: 'T[] -> string
            abstract x: unit -> U2<float, Func<'T, float, float>>
            abstract x: x: float -> Area<'T>
            abstract x: x: Func<'T, float, float> -> Area<'T>
            abstract x0: unit -> U2<float, Func<'T, float, float>>
            abstract x0: x0: float -> Area<'T>
            abstract x0: x0: Func<'T, float, float> -> Area<'T>
            abstract x1: unit -> U2<float, Func<'T, float, float>>
            abstract x1: x1: float -> Area<'T>
            abstract x1: x1: Func<'T, float, float> -> Area<'T>
            abstract y: unit -> U2<float, Func<'T, float, float>>
            abstract y: y: float -> Area<'T>
            abstract y: y: Func<'T, float, float> -> Area<'T>
            abstract y0: unit -> U2<float, Func<'T, float, float>>
            abstract y0: y0: float -> Area<'T>
            abstract y0: y0: Func<'T, float, float> -> Area<'T>
            abstract y1: unit -> U2<float, Func<'T, float, float>>
            abstract y1: y1: float -> Area<'T>
            abstract y1: y1: Func<'T, float, float> -> Area<'T>
            abstract interpolate: unit -> U2<string, Func<(float * float)[], string>>
            [<Emit("$0.interpolate('linear')")>] abstract interpolate_linear: unit -> Area<'T>
            [<Emit("$0.interpolate('step')")>] abstract interpolate_step: unit -> Area<'T>
            [<Emit("$0.interpolate('step-before')")>] abstract ``interpolate_step-before``: unit -> Area<'T>
            [<Emit("$0.interpolate('step-after')")>] abstract ``interpolate_step-after``: unit -> Area<'T>
            [<Emit("$0.interpolate('basis')")>] abstract interpolate_basis: unit -> Area<'T>
            [<Emit("$0.interpolate('basis-open')")>] abstract ``interpolate_basis-open``: unit -> Area<'T>
            [<Emit("$0.interpolate('cardinal')")>] abstract interpolate_cardinal: unit -> Area<'T>
            [<Emit("$0.interpolate('cardinal-open')")>] abstract ``interpolate_cardinal-open``: unit -> Area<'T>
            [<Emit("$0.interpolate('monotone')")>] abstract interpolate_monotone: unit -> Area<'T>
            abstract interpolate: interpolate: U2<string, Func<(float * float)[], string>> -> Area<'T>
            abstract tension: unit -> float
            abstract tension: tension: float -> Area<'T>
            abstract defined: unit -> Func<'T, float, bool>
            abstract defined: defined: Func<'T, float, bool> -> Area<'T>

        and Arc<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: d: 'T * ?i: float -> string
            abstract innerRadius: unit -> Func<'T, float, float>
            abstract innerRadius: radius: float -> Arc<'T>
            abstract innerRadius: radius: Func<'T, float, float> -> Arc<'T>
            abstract outerRadius: unit -> Func<'T, float, float>
            abstract outerRadius: radius: float -> Arc<'T>
            abstract outerRadius: radius: Func<'T, float, float> -> Arc<'T>
            abstract cornerRadius: unit -> Func<'T, float, float>
            abstract cornerRadius: radius: float -> Arc<'T>
            abstract cornerRadius: radius: Func<'T, float, float> -> Arc<'T>
            abstract padRadius: unit -> U2<string, Func<'T, float, float>>
            [<Emit("$0.padRadius('auto')")>] abstract padRadius_auto: unit -> Arc<'T>
            abstract padRadius: radius: string -> Arc<'T>
            abstract padRadius: radius: Func<'T, float, float> -> Arc<'T>
            abstract startAngle: unit -> Func<'T, float, float>
            abstract startAngle: angle: float -> Arc<'T>
            abstract startAngle: angle: Func<'T, float, float> -> Arc<'T>
            abstract endAngle: unit -> Func<'T, float, float>
            abstract endAngle: angle: float -> Arc<'T>
            abstract endAngle: angle: Func<'T, float, float> -> Arc<'T>
            abstract padAngle: unit -> Func<'T, float, float>
            abstract padAngle: angle: float -> Arc<'T>
            abstract padAngle: angle: Func<'T, float, float> -> Arc<'T>
            abstract centroid: d: 'T * ?i: float -> float * float

        and Symbol<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: d: 'T * ?i: float -> string
            abstract ``type``: unit -> Func<'T, float, string>
            abstract ``type``: ``type``: string -> Symbol<'T>
            abstract ``type``: ``type``: Func<'T, float, string> -> Symbol<'T>
            abstract size: unit -> Func<'T, string, float>
            abstract size: size: float -> Symbol<'T>
            abstract size: size: Func<'T, float, float> -> Symbol<'T>

        and Chord<'Link, 'Node> =
            [<Emit("$0($1...)")>] abstract Invoke: d: 'Link * i: float -> string
            abstract source: unit -> Func<'Link, float, 'Node>
            abstract source: source: 'Node -> Chord<'Link, 'Node>
            abstract source: source: Func<'Link, float, 'Node> -> Chord<'Link, 'Node>
            abstract target: unit -> Func<'Link, float, 'Node>
            abstract target: target: 'Node -> Chord<'Link, 'Node>
            abstract target: target: Func<'Link, float, 'Node> -> Chord<'Link, 'Node>
            abstract radius: unit -> Func<'Node, float, float>
            abstract radius: radius: float -> Chord<'Link, 'Node>
            abstract radius: radius: Func<'Node, float, float> -> Chord<'Link, 'Node>
            abstract startAngle: unit -> Func<'Node, float, float>
            abstract startAngle: angle: float -> Chord<'Link, 'Node>
            abstract startAngle: angle: Func<'Node, float, float> -> Chord<'Link, 'Node>
            abstract endAngle: unit -> Func<'Node, float, float>
            abstract endAngle: angle: float -> Chord<'Link, 'Node>
            abstract endAngle: angle: Func<'Node, float, float> -> Chord<'Link, 'Node>

        and Diagonal<'Link, 'Node> =
            [<Emit("$0($1...)")>] abstract Invoke: d: 'Link * ?i: float -> string
            abstract source: unit -> Func<'Link, float, 'Node>
            abstract source: source: 'Node -> Diagonal<'Link, 'Node>
            abstract source: source: Func<'Link, float, obj> -> Diagonal<'Link, 'Node>
            abstract target: unit -> Func<'Link, float, 'Node>
            abstract target: target: 'Node -> Diagonal<'Link, 'Node>
            abstract target: target: Func<'Link, float, obj> -> Diagonal<'Link, 'Node>
            abstract projection: unit -> Func<'Node, float, float * float>
            abstract projection: projection: Func<'Node, float, float * float> -> Diagonal<'Link, 'Node>

        and Axis =
            [<Emit("$0($1...)")>] abstract Invoke: selection: Selection<obj> -> unit
            [<Emit("$0($1...)")>] abstract Invoke: selection: Transition<obj> -> unit
            abstract scale: unit -> obj
            abstract scale: scale: obj -> Axis
            abstract orient: unit -> string
            abstract orient: orientation: string -> Axis
            abstract ticks: unit -> obj[]
            abstract ticks: [<ParamArray>] args: obj[] -> Axis
            abstract tickValues: unit -> obj[]
            abstract tickValues: values: obj[] -> Axis
            abstract tickSize: unit -> float
            abstract tickSize: size: float -> Axis
            abstract tickSize: inner: float * outer: float -> Axis
            abstract innerTickSize: unit -> float
            abstract innerTickSize: size: float -> Axis
            abstract outerTickSize: unit -> float
            abstract outerTickSize: size: float -> Axis
            abstract tickPadding: unit -> float
            abstract tickPadding: padding: float -> Axis
            abstract tickFormat: unit -> Func<obj, string>
            abstract tickFormat: format: Func<obj, string> -> Axis
            abstract tickFormat: format: string -> Axis

        and Brush<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: selection: Selection<'T> -> unit
            [<Emit("$0($1...)")>] abstract Invoke: selection: Transition<'T> -> unit
            abstract ``event``: selection: Selection<'T> -> unit
            abstract ``event``: selection: Transition<'T> -> unit
            abstract x: unit -> Brush.Scale
            abstract x: x: Brush.Scale -> Brush<'T>
            abstract y: unit -> Brush.Scale
            abstract y: y: Brush.Scale -> Brush<'T>
            abstract extent: unit -> U2<float * float, float * float * float * float>
            abstract extent: extent: U2<float * float, float * float * float * float> -> Brush<'T>
            abstract clamp: unit -> U2<bool, bool * bool>
            abstract clamp: clamp: U2<bool, bool * bool> -> Brush<'T>
            abstract clear: unit -> unit
            abstract empty: unit -> bool
            [<Emit("$0.on('brushstart')")>] abstract on_brushstart: unit -> Func<'T, float, unit>
            [<Emit("$0.on('brush')")>] abstract on_brush: unit -> Func<'T, float, unit>
            [<Emit("$0.on('brushend')")>] abstract on_brushend: unit -> Func<'T, float, unit>
            abstract on: ``type``: string -> Func<'T, float, unit>
            [<Emit("$0.on('brushstart',$1...)")>] abstract on_brushstart: listener: Func<'T, float, unit> -> Brush<'T>
            [<Emit("$0.on('brush',$1...)")>] abstract on_brush: listener: Func<'T, float, unit> -> Brush<'T>
            [<Emit("$0.on('brushend',$1...)")>] abstract on_brushend: listener: Func<'T, float, unit> -> Brush<'T>
            abstract on: ``type``: string * listener: Func<'T, float, unit> -> Brush<'T>

        type [<Import("svg","d3")>] Globals =
            static member symbolTypes with get(): string[] = failwith "JS only" and set(v: string[]): unit = failwith "JS only"
            static member line(): Line<float * float> = failwith "JS only"
            static member line(): Line<'T> = failwith "JS only"
            static member area(): Area<float * float> = failwith "JS only"
            static member area(): Area<'T> = failwith "JS only"
            static member arc(): Arc<Arc.Arc> = failwith "JS only"
            static member arc(): Arc<'T> = failwith "JS only"
            static member symbol(): Symbol<obj> = failwith "JS only"
            static member symbol(): Symbol<'T> = failwith "JS only"
            static member chord(): Chord<ChordModule.Link<ChordModule.Node>, ChordModule.Node> = failwith "JS only"
            static member chord(): Chord<ChordModule.Link<'Node>, 'Node> = failwith "JS only"
            static member chord(): Chord<'Link, 'Node> = failwith "JS only"
            static member diagonal(): Diagonal<Diagonal.Link<Diagonal.Node>, Diagonal.Node> = failwith "JS only"
            static member diagonal(): Diagonal<Diagonal.Link<'Node>, 'Node> = failwith "JS only"
            static member diagonal(): Diagonal<'Link, 'Node> = failwith "JS only"
            static member axis(): Axis = failwith "JS only"
            static member brush(): Brush<obj> = failwith "JS only"
            static member brush(): Brush<'T> = failwith "JS only"

    module Layout =
        module Bundle =
            type Node =
                abstract parent: Node with get, set

            and Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

        module ChordModule =
            type Link =
                abstract source: Node with get, set
                abstract target: Node with get, set

            and Node =
                abstract index: float with get, set
                abstract subindex: float with get, set
                abstract startAngle: float with get, set
                abstract endAngle: float with get, set
                abstract value: float with get, set

            and Group =
                abstract index: float with get, set
                abstract startAngle: float with get, set
                abstract endAngle: float with get, set
                abstract value: float with get, set

        module Cluster =
            type Result =
                abstract parent: Result option with get, set
                abstract children: Result[] option with get, set
                abstract depth: float option with get, set
                abstract x: float option with get, set
                abstract y: float option with get, set

            and Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

        module Force =
            type Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

            and Node =
                abstract index: float option with get, set
                abstract x: float option with get, set
                abstract y: float option with get, set
                abstract px: float option with get, set
                abstract py: float option with get, set
                abstract ``fixed``: bool option with get, set
                abstract weight: float option with get, set

            and Event =
                abstract ``type``: string with get, set
                abstract alpha: float with get, set

        module Hierarchy =
            type Result =
                abstract parent: Result option with get, set
                abstract children: Result[] option with get, set
                abstract value: float option with get, set
                abstract depth: float option with get, set

        module Histogram =
            type Bin<'T> =
                // inherit 'T[] // TODO
                abstract x: float with get, set
                abstract dx: float with get, set
                abstract y: float with get, set

        module Pack =
            type Node =
                abstract parent: Node option with get, set
                abstract children: Node[] option with get, set
                abstract value: float option with get, set
                abstract depth: float option with get, set
                abstract x: float option with get, set
                abstract y: float option with get, set
                abstract r: float option with get, set

            and Link<'T> =
                abstract source: Node with get, set
                abstract target: Node with get, set

        module Partition =
            type Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

            and Node =
                abstract parent: Node option with get, set
                abstract children: float option with get, set
                abstract value: float option with get, set
                abstract depth: float option with get, set
                abstract x: float option with get, set
                abstract y: float option with get, set
                abstract dx: float option with get, set
                abstract dy: float option with get, set

        module Pie =
            type Arc<'T> =
                abstract value: float with get, set
                abstract startAngle: float with get, set
                abstract endAngle: float with get, set
                abstract padAngle: float with get, set
                abstract data: 'T with get, set

        module Stack =
            type Value =
                abstract x: float with get, set
                abstract y: float with get, set
                abstract y0: float option with get, set

        module Tree =
            type Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

            and Node =
                abstract parent: Node option with get, set
                abstract children: Node[] option with get, set
                abstract depth: float option with get, set
                abstract x: float option with get, set
                abstract y: float option with get, set

        module Treemap =
            type Node =
                abstract parent: Node option with get, set
                abstract children: Node[] option with get, set
                abstract value: float option with get, set
                abstract depth: float option with get, set
                abstract x: float option with get, set
                abstract y: float option with get, set
                abstract dx: float option with get, set
                abstract dy: float option with get, set

            and Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

            and Padding =
                U2<float, float * float * float * float>
    
        type Bundle<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: links: Bundle.Link<'T[]> -> 'T[][]

        and Chord =
            abstract matrix: unit -> float[][]
            abstract matrix: matrix: float[][] -> Chord
            abstract padding: unit -> float
            abstract padding: padding: float -> Chord
            abstract sortGroups: unit -> Func<float, float, float>
            abstract sortGroups: comparator: Func<float, float, float> -> Chord
            abstract sortSubgroups: unit -> Func<float, float, float>
            abstract sortSubgroups: comparator: Func<float, float, float> -> Chord
            abstract sortChords: unit -> Func<float, float, float>
            abstract sortChords: comparator: Func<float, float, float> -> Chord
            abstract chords: unit -> ChordModule.Link[]
            abstract groups: unit -> ChordModule.Group[]

        and Cluster<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: root: 'T -> 'T[]
            abstract nodes: root: 'T -> 'T[]
            abstract links: nodes: 'T[] -> Cluster.Link<'T[]>
            abstract children: unit -> Func<'T, 'T[]>
            abstract children: accessor: Func<'T, 'T[]> -> Cluster<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Cluster<'T>
            abstract separation: unit -> Func<'T, 'T, float>
            abstract separation: separation: Func<'T, 'T, float> -> Cluster<'T>
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Cluster<'T>
            abstract nodeSize: unit -> float * float
            abstract nodeSize: nodeSize: (float * float) -> Cluster<'T>
            abstract value: unit -> Func<'T, float>
            abstract value: value: Func<'T, float> -> Cluster<'T>

        and Force<'Link, 'Node> =
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Force<'Link, 'Node>
            abstract linkDistance: unit -> U2<float, Func<'Link, float, float>>
            abstract linkDistance: distance: float -> Force<'Link, 'Node>
            abstract linkDistance: distance: Func<'Link, float, float> -> Force<'Link, 'Node>
            abstract linkStrength: unit -> U2<float, Func<'Link, float, float>>
            abstract linkStrength: strength: float -> Force<'Link, 'Node>
            abstract linkStrength: strength: Func<'Link, float, float> -> Force<'Link, 'Node>
            abstract friction: unit -> float
            abstract friction: friction: float -> Force<'Link, 'Node>
            abstract charge: unit -> U2<float, Func<'Node, float, float>>
            abstract charge: charge: float -> Force<'Link, 'Node>
            abstract charge: charge: Func<'Node, float, float> -> Force<'Link, 'Node>
            abstract chargeDistance: unit -> float
            abstract chargeDistance: distance: float -> Force<'Link, 'Node>
            abstract theta: unit -> float
            abstract theta: theta: float -> Force<'Link, 'Node>
            abstract gravity: unit -> float
            abstract gravity: gravity: float -> Force<'Link, 'Node>
            abstract nodes: unit -> 'Node[]
            abstract nodes: nodes: 'Node[] -> Force<'Link, 'Node>
            abstract links: unit -> 'Link[]
            abstract links: links: obj[] -> Force<'Link, 'Node>
            abstract links: links: 'Link[] -> Force<'Link, 'Node>
            abstract start: unit -> Force<'Link, 'Node>
            abstract alpha: unit -> float
            abstract alpha: value: float -> Force<'Link, 'Node>
            abstract resume: unit -> Force<'Link, 'Node>
            abstract stop: unit -> Force<'Link, 'Node>
            abstract on: ``type``: string -> Func<Force.Event, unit>
            abstract on: ``type``: string * listener: Func<Force.Event, unit> -> Force<'Link, 'Node>
            abstract drag: unit -> Behavior.Drag<'Node>
            abstract drag: selection: Selection<'Node> -> unit

        and Hierarchy<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: root: 'T -> 'T[]
            abstract children: unit -> Func<'T, 'T[]>
            abstract children: accessor: Func<'T, 'T[]> -> Hierarchy<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Hierarchy<'T>
            abstract value: unit -> Func<'T, float>
            abstract value: accessor: Func<'T, float> -> Hierarchy<'T>
            abstract revalue: root: 'T -> 'T[]

        and Histogram<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: values: 'T[] * ?index: float -> Histogram.Bin<'T[]>
            abstract value: unit -> Func<'T, float, float>
            abstract value: value: Func<'T, float, float> -> Histogram<'T>
            abstract range: unit -> Func<'T[], float, float * float>
            abstract range: range: Func<'T[], float, float * float> -> Histogram<'T>
            abstract bins: unit -> Func<float * float, 'T[], float, float[]>
            abstract bins: count: float -> Histogram<'T>
            abstract bins: thresholds: float[] -> Histogram<'T>
            abstract bins: func: Func<float * float, 'T[], float, float[]> -> Histogram<'T>
            abstract frequency: unit -> bool
            abstract frequency: frequency: bool -> Histogram<'T>

        and Pack<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: root: 'T -> 'T[]
            abstract nodes: root: 'T -> 'T[]
            abstract links: nodes: 'T[] -> Pack.Link<'T[]>
            abstract children: unit -> Func<'T, float, 'T[]>
            abstract children: children: Func<'T, float, 'T[]> -> Pack<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Pack<'T>
            abstract value: unit -> Func<'T, float>
            abstract value: value: Func<'T, float> -> Pack<'T>
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Pack<'T>
            abstract radius: unit -> U2<float, Func<'T, float>>
            abstract radius: radius: float -> Pack<'T>
            abstract radius: radius: Func<'T, float> -> Pack<'T>
            abstract padding: unit -> float
            abstract padding: padding: float -> Pack<'T>

        and Partition<'T> =
            abstract nodes: root: 'T -> 'T[]
            abstract links: nodes: 'T[] -> Partition.Link<'T[]>
            abstract children: unit -> Func<'T, float, 'T[]>
            abstract children: children: Func<'T, float, 'T[]> -> Partition<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Partition<'T>
            abstract value: unit -> Func<'T, float>
            abstract value: value: Func<'T, float> -> Partition<'T>
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Partition<'T>

        and Pie<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: data: 'T[] * ?index: float -> Pie.Arc<'T[]>
            abstract value: unit -> Func<'T, float, float>
            abstract value: accessor: Func<'T, float, float> -> Pie<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Pie<'T>
            abstract startAngle: unit -> U2<float, Func<'T[], float, float>>
            abstract startAngle: angle: float -> Pie<'T>
            abstract startAngle: angle: Func<'T[], float, float> -> Pie<'T>
            abstract endAngle: unit -> U2<float, Func<'T[], float, float>>
            abstract endAngle: angle: float -> Pie<'T>
            abstract endAngle: angle: Func<'T[], float, float> -> Pie<'T>
            abstract padAngle: unit -> U2<float, Func<'T[], float, float>>
            abstract padAngle: angle: float -> Pie<'T>
            abstract padAngle: angle: Func<'T[], float, float> -> Pie<'T>

        and Stack<'Series, 'Value> =
            [<Emit("$0($1...)")>] abstract Invoke: layers: 'Series[] * ?index: float -> 'Series[]
            abstract values: unit -> Func<'Series, float, 'Value[]>
            abstract values: accessor: Func<'Series, float, 'Value[]> -> Stack<'Series, 'Value>
            abstract offset: unit -> Func<(float * float)[], float[]>
            [<Emit("$0.offset('silhouette')")>] abstract offset_silhouette: unit -> Stack<'Series, 'Value>
            [<Emit("$0.offset('wiggle')")>] abstract offset_wiggle: unit -> Stack<'Series, 'Value>
            [<Emit("$0.offset('expand')")>] abstract offset_expand: unit -> Stack<'Series, 'Value>
            [<Emit("$0.offset('zero')")>] abstract offset_zero: unit -> Stack<'Series, 'Value>
            abstract offset: offset: string -> Stack<'Series, 'Value>
            abstract offset: offset: Func<(float * float)[], float[]> -> Stack<'Series, 'Value>
            abstract order: unit -> Func<(float * float)[], float[]>
            [<Emit("$0.order('inside-out')")>] abstract ``order_inside-out``: unit -> Stack<'Series, 'Value>
            [<Emit("$0.order('reverse')")>] abstract order_reverse: unit -> Stack<'Series, 'Value>
            [<Emit("$0.order('default')")>] abstract order_default: unit -> Stack<'Series, 'Value>
            abstract order: order: string -> Stack<'Series, 'Value>
            abstract order: order: Func<(float * float)[], float[]> -> Stack<'Series, 'Value>
            abstract x: unit -> Func<'Value, float, float>
            abstract x: accessor: Func<'Value, float, float> -> Stack<'Series, 'Value>
            abstract y: unit -> Func<'Value, float, float>
            abstract y: accesor: Func<'Value, float, float> -> Stack<'Series, 'Value>
            abstract out: unit -> Func<'Value, float, float, unit>
            abstract out: setter: Func<'Value, float, float, unit> -> Stack<'Series, 'Value>

        and Tree<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: root: 'T * ?index: float -> 'T[]
            abstract nodes: root: 'T * ?index: float -> 'T[]
            abstract links: nodes: 'T[] -> Tree.Link<'T[]>
            abstract children: unit -> Func<'T, float, 'T[]>
            abstract children: children: Func<'T, float, 'T[]> -> Tree<'T>
            abstract separation: unit -> Func<'T, 'T, float>
            abstract separation: separation: Func<'T, 'T, float> -> Tree<'T>
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Tree<'T>
            abstract nodeSize: unit -> float * float
            abstract nodeSize: size: (float * float) -> Tree<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Tree<'T>
            abstract value: unit -> Func<'T, float, float>
            abstract value: value: Func<'T, float, float> -> Tree<'T>

        and Treemap<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: root: 'T * ?index: float -> 'T[]
            abstract nodes: root: 'T * ?index: float -> 'T[]
            abstract links: nodes: 'T[] -> Treemap.Link<'T[]>
            abstract children: unit -> Func<'T, float, 'T[]>
            abstract children: children: Func<'T, float, 'T[]> -> Treemap<'T>
            abstract sort: unit -> Func<'T, 'T, float>
            abstract sort: comparator: Func<'T, 'T, float> -> Treemap<'T>
            abstract value: unit -> Func<'T, float, float>
            abstract value: value: Func<'T, float, float> -> Treemap<'T>
            abstract size: unit -> float * float
            abstract size: size: (float * float) -> Treemap<'T>
            abstract padding: unit -> Func<'T, float, Treemap.Padding>
            abstract padding: padding: Treemap.Padding -> Treemap<'T>
            abstract padding: padding: Func<'T, float, Treemap.Padding> -> Treemap<'T>
            abstract round: unit -> bool
            abstract round: round: bool -> Treemap<'T>
            abstract sticky: unit -> bool
            abstract sticky: sticky: bool -> bool
            abstract mode: unit -> string
            [<Emit("$0.mode('squarify')")>] abstract mode_squarify: unit -> Treemap<'T>
            [<Emit("$0.mode('slice')")>] abstract mode_slice: unit -> Treemap<'T>
            [<Emit("$0.mode('dice')")>] abstract mode_dice: unit -> Treemap<'T>
            [<Emit("$0.mode('slice-dice')")>] abstract ``mode_slice-dice``: unit -> Treemap<'T>
            abstract mode: mode: string -> Treemap<'T>
            abstract ratio: unit -> float
            abstract ratio: ratio: float -> Treemap<'T>

        type [<Import("layout","d3")>] Globals =
            static member bundle(): Bundle<Bundle.Node> = failwith "JS only"
            static member bundle(): Bundle<'T> = failwith "JS only"
            static member chord(): Chord = failwith "JS only"
            static member cluster(): Cluster<Cluster.Result> = failwith "JS only"
            static member cluster(): Cluster<'T> = failwith "JS only"
            static member force(): Force<Force.Link<Force.Node>, Force.Node> = failwith "JS only"
            static member force(): Force<Force.Link<'Node>, 'Node> = failwith "JS only"
            static member force(): Force<'Link, 'Node> = failwith "JS only"
            static member hierarchy(): Hierarchy<Hierarchy.Result> = failwith "JS only"
            static member hierarchy(): Hierarchy<'T> = failwith "JS only"
            static member histogram(): Histogram<float> = failwith "JS only"
            static member histogram(): Histogram<'T> = failwith "JS only"
            static member pack(): Pack<Pack.Node> = failwith "JS only"
            static member pack(): Pack<'T> = failwith "JS only"
            static member partition(): Partition<Partition.Node> = failwith "JS only"
            static member partition(): Partition<'T> = failwith "JS only"
            static member pie(): Pie<float> = failwith "JS only"
            static member pie(): Pie<'T> = failwith "JS only"
            static member stack(): Stack<Stack.Value[], Stack.Value> = failwith "JS only"
            static member stack(): Stack<'Value[], 'Value> = failwith "JS only"
            static member stack(): Stack<'Series, 'Value> = failwith "JS only"
            static member tree(): Tree<Tree.Node> = failwith "JS only"
            static member tree(): Tree<'T> = failwith "JS only"
            static member treemap(): Treemap<Treemap.Node> = failwith "JS only"
            static member treemap(): Treemap<'T> = failwith "JS only"

    module Geom =
        module Voronoi =
            type Link<'T> =
                abstract source: 'T with get, set
                abstract target: 'T with get, set

        module Quadtree =
            type Node<'T> =
                abstract nodes: Node<'T> * Node<'T> * Node<'T> * Node<'T> with get, set
                abstract leaf: bool with get, set
                abstract point: 'T with get, set
                abstract x: float with get, set
                abstract y: float with get, set

            and Quadtree<'T> =
                inherit Node<'T>
                abstract add: point: 'T -> unit
                abstract visit: callback: Func<Node<'T>, float, float, float, float, U2<bool, unit>> -> unit
                abstract find: point: (float * float) -> 'T
    
        type Voronoi<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: data: 'T[] -> (float * float)[]
            abstract x: unit -> Func<'T, float>
            abstract x: x: Func<'T, float> -> Voronoi<'T>
            abstract y: unit -> Func<'T, float>
            abstract y: y: Func<'T, float> -> Voronoi<'T>
            abstract clipExtent: unit -> float * float * float * float
            abstract clipExtent: extent: (float * float * float * float) -> Voronoi<'T>
            abstract links: data: 'T[] -> Voronoi.Link<'T[]>
            abstract triangles: data: 'T[] -> 'T * ('T * 'T)[]

        and Quadtree<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: points: 'T[] -> Quadtree.Quadtree<'T>
            abstract x: unit -> Func<'T, float, float>
            abstract x: x: float -> Quadtree<'T>
            abstract x: x: Func<'T, float, float> -> Quadtree<'T>
            abstract y: unit -> Func<'T, float, float>
            abstract y: y: float -> Quadtree<'T>
            abstract y: y: Func<'T, float, float> -> Quadtree<'T>
            abstract extent: unit -> float * float * float * float
            abstract extent: extent: (float * float * float * float) -> Quadtree<'T>

        and Hull<'T> =
            [<Emit("$0($1...)")>] abstract Invoke: vertices: 'T[] -> (float * float)[]
            abstract x: unit -> Func<'T, float>
            abstract x: x: Func<'T, float> -> Hull<'T>
            abstract y: unit -> Func<'T, float>
            abstract y: y: Func<'T, float> -> Hull<'T>

        and Polygon =
            abstract area: unit -> float
            abstract centroid: unit -> float * float
            abstract clip: subject: (float * float)[] -> (float * float)[]

        type [<Import("geom","d3")>] Globals =
            static member voronoi(): Voronoi<float * float> = failwith "JS only"
            static member voronoi(): Voronoi<'T> = failwith "JS only"
            static member delaunay(vertices: (float * float)[]): float * float * float * float * (float * float)[] = failwith "JS only"
            static member quadtree(): Quadtree<float * float> = failwith "JS only"
            static member quadtree(nodes: 'T[], ?x1: float, ?y1: float, ?x2: float, ?y2: float): Quadtree.Quadtree<'T> = failwith "JS only"
            static member hull(vertices: (float * float)[]): (float * float)[] = failwith "JS only"
            static member hull(): Hull<float * float> = failwith "JS only"
            static member hull(): Hull<'T> = failwith "JS only"
            static member polygon(vertices: (float * float)[]): Polygon = failwith "JS only"
