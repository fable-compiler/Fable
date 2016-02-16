namespace Fabel.Import
open System
    
module Backbone =
    type private ImportAttribute(path) =
        inherit System.Attribute()

    type AddOptions =
        abstract at: float option with get, set

    type HistoryOptions =
        abstract pushState: bool option with get, set
        abstract root: string option with get, set

    type NavigateOptions =
        abstract trigger: bool option with get, set
        abstract replace: bool option with get, set

    type RouterOptions =
        abstract routes: obj with get, set

    type Silenceable =
        abstract silent: bool option with get, set

    type Validable =
        abstract validate: bool option with get, set

    type Waitable =
        abstract wait: bool option with get, set

    type Parseable =
        abstract parse: obj option with get, set

    type PersistenceOptions =
        abstract url: string option with get, set
        abstract data: obj option with get, set
        abstract beforeSend: Func<JQueryXHR, unit> option with get, set
        abstract success: Func<obj, obj, obj, unit> option with get, set
        abstract error: Func<obj, JQueryXHR, obj, unit> option with get, set

    type ModelSetOptions =
        interface end

    type ModelFetchOptions =
        interface end

    type ModelSaveOptions =
        abstract patch: bool option with get, set

    type ModelDestroyOptions =
        interface end

    type CollectionFetchOptions =
        abstract reset: bool option with get, set

    type ObjectHash =
        interface end

    type RoutesHash =
        interface end

    type EventsHash =
        interface end

    type Events =
        abstract on: eventName: string * ?callback: Function * ?context: obj -> obj
        abstract on: eventMap: EventsHash -> obj
        abstract off: ?eventName: string * ?callback: Function * ?context: obj -> obj
        abstract trigger: eventName: string * [<ParamArray>] args: obj[] -> obj
        abstract bind: eventName: string * callback: Function * ?context: obj -> obj
        abstract unbind: ?eventName: string * ?callback: Function * ?context: obj -> obj
        abstract once: events: string * callback: Function * ?context: obj -> obj
        abstract listenTo: object: obj * events: string * callback: Function -> obj
        abstract listenToOnce: object: obj * events: string * callback: Function -> obj
        abstract stopListening: ?object: obj * ?events: string * ?callback: Function -> obj

    type ModelBase =
        abstract url: obj with get, set
        abstract parse: response: obj * ?options: obj -> obj
        abstract toJSON: ?options: obj -> obj
        abstract sync: [<ParamArray>] arg: obj[] -> JQueryXHR

    type Model =
        abstract attributes: obj with get, set
        abstract changed: ResizeArray<obj> with get, set
        abstract cid: string with get, set
        abstract collection: Collection with get, set
        abstract id: obj with get, set
        abstract idAttribute: string with get, set
        abstract validationError: obj with get, set
        abstract urlRoot: obj with get, set
        abstract extend: properties: obj * ?classProperties: obj -> obj
        abstract defaults: unit -> ObjectHash
        abstract initialize: ?attributes: obj * ?options: obj -> unit
        abstract fetch: ?options: ModelFetchOptions -> JQueryXHR
        abstract get: attributeName: string -> obj
        abstract set: attributeName: string * value: obj * ?options: ModelSetOptions -> Model
        abstract set: obj: obj * ?options: ModelSetOptions -> Model
        abstract change: unit -> obj
        abstract changedAttributes: ?attributes: obj -> ResizeArray<obj>
        abstract clear: ?options: Silenceable -> obj
        abstract clone: unit -> Model
        abstract destroy: ?options: ModelDestroyOptions -> obj
        abstract escape: attribute: string -> string
        abstract has: attribute: string -> bool
        abstract hasChanged: ?attribute: string -> bool
        abstract isNew: unit -> bool
        abstract isValid: ?options: obj -> bool
        abstract previous: attribute: string -> obj
        abstract previousAttributes: unit -> ResizeArray<obj>
        abstract save: ?attributes: obj * ?options: ModelSaveOptions -> obj
        abstract unset: attribute: string * ?options: Silenceable -> Model
        abstract validate: attributes: obj * ?options: obj -> obj
        abstract _validate: attributes: obj * options: obj -> bool
        abstract keys: unit -> ResizeArray<string>
        abstract values: unit -> ResizeArray<obj>
        abstract pairs: unit -> ResizeArray<obj>
        abstract invert: unit -> obj
        abstract pick: keys: ResizeArray<string> -> obj
        abstract pick: [<ParamArray>] keys: string[] -> obj
        abstract omit: keys: ResizeArray<string> -> obj
        abstract omit: [<ParamArray>] keys: string[] -> obj

    type Collection =
        abstract model: obj with get, set
        abstract models: ResizeArray<TModel> with get, set
        abstract length: float with get, set
        abstract extend: properties: obj * ?classProperties: obj -> obj
        abstract initialize: ?models: obj * ?options: obj -> unit
        abstract fetch: ?options: CollectionFetchOptions -> JQueryXHR
        abstract comparator: element: TModel -> float
        abstract comparator: compare: TModel * ?to: TModel -> float
        abstract add: model: obj * ?options: AddOptions -> TModel
        abstract add: models: ResizeArray<obj> * ?options: AddOptions -> ResizeArray<TModel>
        abstract at: index: float -> TModel
        abstract get: id: obj -> TModel
        abstract create: attributes: obj * ?options: ModelSaveOptions -> TModel
        abstract pluck: attribute: string -> ResizeArray<obj>
        abstract push: model: TModel * ?options: AddOptions -> TModel
        abstract pop: ?options: Silenceable -> TModel
        abstract remove: model: TModel * ?options: Silenceable -> TModel
        abstract remove: models: ResizeArray<TModel> * ?options: Silenceable -> ResizeArray<TModel>
        abstract reset: ?models: ResizeArray<TModel> * ?options: Silenceable -> ResizeArray<TModel>
        abstract set: ?models: ResizeArray<TModel> * ?options: Silenceable -> ResizeArray<TModel>
        abstract shift: ?options: Silenceable -> TModel
        abstract sort: ?options: Silenceable -> Collection
        abstract unshift: model: TModel * ?options: AddOptions -> TModel
        abstract where: properties: obj -> ResizeArray<TModel>
        abstract findWhere: properties: obj -> TModel
        abstract _prepareModel: ?attributes: obj * ?options: obj -> obj
        abstract _removeReference: model: TModel -> unit
        abstract _onModelEvent: event: string * model: TModel * collection: Collection * options: obj -> unit
        abstract all: iterator: Func<TModel, float, bool> * ?context: obj -> bool
        abstract any: iterator: Func<TModel, float, bool> * ?context: obj -> bool
        abstract collect: iterator: Func<TModel, float, obj, ResizeArray<obj>> * ?context: obj -> ResizeArray<obj>
        abstract chain: unit -> obj
        abstract contains: value: obj -> bool
        abstract countBy: iterator: Func<TModel, float, obj> -> undefined
        abstract countBy: attribute: string -> undefined
        abstract detect: iterator: Func<obj, bool> * ?context: obj -> obj
        abstract drop: unit -> TModel
        abstract drop: n: float -> ResizeArray<TModel>
        abstract each: iterator: Func<TModel, float, obj, unit> * ?context: obj -> obj
        abstract every: iterator: Func<TModel, float, bool> * ?context: obj -> bool
        abstract filter: iterator: Func<TModel, float, bool> * ?context: obj -> ResizeArray<TModel>
        abstract find: iterator: Func<TModel, float, bool> * ?context: obj -> TModel
        abstract first: unit -> TModel
        abstract first: n: float -> ResizeArray<TModel>
        abstract foldl: iterator: Func<obj, TModel, float, obj> * initialMemo: obj * ?context: obj -> obj
        abstract forEach: iterator: Func<TModel, float, obj, unit> * ?context: obj -> obj
        abstract groupBy: iterator: Func<TModel, float, string> * ?context: obj -> undefined
        abstract groupBy: attribute: string * ?context: obj -> undefined
        abstract include: value: obj -> bool
        abstract indexOf: element: TModel * ?isSorted: bool -> float
        abstract initial: unit -> TModel
        abstract initial: n: float -> ResizeArray<TModel>
        abstract inject: iterator: Func<obj, TModel, float, obj> * initialMemo: obj * ?context: obj -> obj
        abstract isEmpty: object: obj -> bool
        abstract invoke: methodName: string * ?args: ResizeArray<obj> -> obj
        abstract last: unit -> TModel
        abstract last: n: float -> ResizeArray<TModel>
        abstract lastIndexOf: element: TModel * ?fromIndex: float -> float
        abstract map: iterator: Func<TModel, float, obj, obj> * ?context: obj -> ResizeArray<obj>
        abstract max: ?iterator: Func<TModel, float, obj> * ?context: obj -> TModel
        abstract min: ?iterator: Func<TModel, float, obj> * ?context: obj -> TModel
        abstract reduce: iterator: Func<obj, TModel, float, obj> * initialMemo: obj * ?context: obj -> obj
        abstract select: iterator: obj * ?context: obj -> ResizeArray<obj>
        abstract size: unit -> float
        abstract shuffle: unit -> ResizeArray<obj>
        abstract slice: min: float * ?max: float -> ResizeArray<TModel>
        abstract some: iterator: Func<TModel, float, bool> * ?context: obj -> bool
        abstract sortBy: iterator: Func<TModel, float, float> * ?context: obj -> ResizeArray<TModel>
        abstract sortBy: attribute: string * ?context: obj -> ResizeArray<TModel>
        abstract sortedIndex: element: TModel * ?iterator: Func<TModel, float, float> -> float
        abstract reduceRight: iterator: Func<obj, TModel, float, obj> * initialMemo: obj * ?context: obj -> ResizeArray<obj>
        abstract reject: iterator: Func<TModel, float, bool> * ?context: obj -> ResizeArray<TModel>
        abstract rest: unit -> TModel
        abstract rest: n: float -> ResizeArray<TModel>
        abstract tail: unit -> TModel
        abstract tail: n: float -> ResizeArray<TModel>
        abstract toArray: unit -> ResizeArray<obj>
        abstract without: [<ParamArray>] values: obj[] -> ResizeArray<TModel>

    type Router =
        abstract routes: obj with get, set
        abstract extend: properties: obj * ?classProperties: obj -> obj
        abstract initialize: ?options: RouterOptions -> unit
        abstract route: route: obj * name: string * ?callback: Function -> Router
        abstract navigate: fragment: string * ?options: NavigateOptions -> Router
        abstract navigate: fragment: string * ?trigger: bool -> Router
        abstract _bindRoutes: unit -> unit
        abstract _routeToRegExp: route: string -> RegExp
        abstract _extractParameters: route: RegExp * fragment: string -> ResizeArray<string>

    type History =
        abstract handlers: ResizeArray<obj> with get, set
        abstract interval: float with get, set
        abstract started: bool with get, set
        abstract options: obj with get, set
        abstract start: ?options: HistoryOptions -> bool
        abstract getHash: ?window: Window -> string
        abstract getFragment: ?fragment: string * ?forcePushState: bool -> string
        abstract stop: unit -> unit
        abstract route: route: string * callback: Function -> float
        abstract checkUrl: ?e: obj -> unit
        abstract loadUrl: fragmentOverride: string -> bool
        abstract navigate: fragment: string * ?options: obj -> bool
        abstract _updateHash: location: Location * fragment: string * replace: bool -> unit

    type ViewOptions =
        abstract model: TModel option with get, set
        abstract collection: undefined option with get, set
        abstract el: obj option with get, set
        abstract id: string option with get, set
        abstract className: string option with get, set
        abstract tagName: string option with get, set
        abstract attributes: obj option with get, set

    type View =
        abstract model: TModel with get, set
        abstract collection: Collection with get, set
        abstract id: string with get, set
        abstract cid: string with get, set
        abstract className: string with get, set
        abstract tagName: string with get, set
        abstract el: obj with get, set
        abstract $el: JQuery with get, set
        abstract attributes: obj with get, set
        abstract extend: properties: obj * ?classProperties: obj -> obj
        abstract initialize: ?options: ViewOptions -> unit
        abstract events: unit -> EventsHash
        abstract $: selector: string -> JQuery
        abstract setElement: element: obj * ?delegate: bool -> View
        abstract setElement: element: obj -> View
        abstract $: selector: obj -> JQuery
        abstract render: unit -> View
        abstract remove: unit -> View
        abstract make: tagName: obj * ?attributes: obj * ?content: obj -> obj
        abstract delegateEvents: ?events: EventsHash -> obj
        abstract delegate: eventName: string * selector: string * listener: Function -> View
        abstract undelegateEvents: unit -> obj
        abstract undelegate: eventName: string * ?selector: string * ?listener: Function -> View
        abstract _ensureElement: unit -> unit

    type Global =
        abstract history: History with get, set
        abstract emulateHTTP: bool with get, set
        abstract emulateJSON: bool with get, set
        abstract $: JQueryStatic with get, set
        abstract sync: method: string * model: Model * ?options: JQueryAjaxSettings -> obj
        abstract ajax: ?options: JQueryAjaxSettings -> JQueryXHR
        abstract noConflict: unit -> obj

    [<Import("Backbone")>]
    let Global: Global = failwith "JS only"

module backbone =
    type private ImportAttribute(path) =
        inherit System.Attribute()




