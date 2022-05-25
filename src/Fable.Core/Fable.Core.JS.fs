namespace Fable.Core

open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module JSX =
    type ComponentAttribute() =
        inherit Attribute()

    type Prop = string * obj
    type Style = string * string

    [<AllowNullLiteral>]
    type Element =
        class end

    let create (componentOrTag: obj) (props: Prop list): Element = nativeOnly
    let html (template: string): Element = nativeOnly

module JS =
    type [<AllowNullLiteral>] Function =
        abstract name: string
        abstract length: int
        abstract apply: thisArg: obj * args: obj[] -> obj
        abstract bind: thisArg: obj * [<ParamArray>] args: obj[] -> Function
        abstract call: thisArg: obj * [<ParamArray>] args: obj[] -> obj
        [<Emit "$0($1...)">] abstract Invoke: [<ParamArray>] args: obj[] -> obj
        [<Emit "new $0($1...)">] abstract Create: [<ParamArray>] args: obj[] -> obj

    [<AbstractClass>]
    type DecoratorAttribute() =
        inherit Attribute()
        abstract Decorate: fn: Function -> Function

    [<AbstractClass>]
    type ReflectedDecoratorAttribute() =
        inherit Attribute()
        abstract Decorate: fn: Function * info: Reflection.MethodInfo -> Function

    // Hack because currently Fable doesn't keep information about spread for anonymous function
    // We also use function (instead of an arrow) to make sure `this` is bound correctly
    [<Emit("function (...args) { return $0(args) }")>]
    let spreadFunc (fn: obj[] -> obj): Function = jsNative

    type [<AllowNullLiteral>] PropertyDescriptor =
        abstract configurable: bool option with get, set
        abstract enumerable: bool option with get, set
        abstract value: obj option with get, set
        abstract writable: bool option with get, set
        abstract get: unit -> obj
        abstract set: v: obj -> unit

    and [<AllowNullLiteral>] ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> 'T[]
        abstract isArray: arg: obj -> bool
        abstract from: arg: obj -> 'T[]

    and [<AllowNullLiteral>] NumberConstructor =
        abstract isNaN: float -> bool

    and [<AllowNullLiteral>] Object =
        abstract toString: unit -> string
        abstract toLocaleString: unit -> string
        abstract valueOf: unit -> obj
        abstract hasOwnProperty: v: string -> bool
        abstract isPrototypeOf: v: obj -> bool
        abstract propertyIsEnumerable: v: string -> bool
        abstract hasOwnProperty: v: obj -> bool
        abstract propertyIsEnumerable: v: obj -> bool

    and [<AllowNullLiteral>] ObjectConstructor =
        abstract getPrototypeOf: o: obj -> obj
        abstract getOwnPropertyDescriptor: o: obj * p: string -> PropertyDescriptor
        abstract getOwnPropertyNames: o: obj -> ResizeArray<string>
        abstract create: o: obj * ?properties: obj -> obj
        abstract defineProperty: o: obj * p: string * attributes: PropertyDescriptor -> obj
        abstract defineProperties: o: obj * properties: obj -> obj
        abstract seal: o: 'T -> 'T
        abstract freeze: o: 'T -> 'T
        abstract preventExtensions: o: 'T -> 'T
        abstract isSealed: o: obj -> bool
        abstract isFrozen: o: obj -> bool
        abstract isExtensible: o: obj -> bool
        abstract keys: o: obj -> ResizeArray<string>
        abstract values: o: obj -> ResizeArray<obj>
        abstract entries: o: obj -> ResizeArray<string * obj>
        abstract assign: target: 'T * source: 'U -> obj
        abstract assign: target: 'T * source1: 'U * source2: 'V -> obj
        abstract assign: target: 'T * source1: 'U * source2: 'V * source3: 'W -> obj
        abstract assign: target: obj * [<ParamArray>] sources: obj[] -> obj
        // abstract getOwnPropertySymbols: o: obj -> ResizeArray<Symbol>
        abstract is: value1: obj * value2: obj -> bool
        abstract setPrototypeOf: o: obj * proto: obj -> obj
        abstract getOwnPropertyDescriptor: o: obj * propertyKey: obj -> PropertyDescriptor
        abstract defineProperty: o: obj * propertyKey: obj * attributes: PropertyDescriptor -> obj

    and [<AllowNullLiteral>] Math =
        abstract E: float
        abstract LN10: float
        abstract LN2: float
        abstract LOG2E: float
        abstract LOG10E: float
        abstract PI: float
        abstract SQRT1_2: float
        abstract SQRT2: float
        abstract abs: x: float -> float
        abstract acos: x: float -> float
        abstract asin: x: float -> float
        abstract atan: x: float -> float
        abstract atan2: y: float * x: float -> float
        abstract ceil: x: float -> float
        abstract cos: x: float -> float
        abstract exp: x: float -> float
        abstract floor: x: float -> float
        abstract log: x: float -> float
        abstract max: [<ParamArray>] values: float[] -> float
        abstract min: [<ParamArray>] values: float[] -> float
        abstract pow: x: float * y: float -> float
        abstract random: unit -> float
        abstract round: x: float -> float
        abstract sin: x: float -> float
        abstract sqrt: x: float -> float
        abstract tan: x: float -> float
        abstract clz32: x: float -> float
        abstract imul: x: float * y: float -> float
        abstract sign: x: float -> float
        abstract log10: x: float -> float
        abstract log2: x: float -> float
        abstract log1p: x: float -> float
        abstract expm1: x: float -> float
        abstract cosh: x: float -> float
        abstract sinh: x: float -> float
        abstract tanh: x: float -> float
        abstract acosh: x: float -> float
        abstract asinh: x: float -> float
        abstract atanh: x: float -> float
        abstract hypot: [<ParamArray>] values: float[] -> float
        abstract trunc: x: float -> float
        abstract fround: x: float -> float
        abstract cbrt: x: float -> float

    and [<AllowNullLiteral>] Date =
        abstract toString: unit -> string
        abstract toDateString: unit -> string
        abstract toTimeString: unit -> string
        abstract toLocaleString: unit -> string
        abstract toLocaleDateString: unit -> string
        abstract toLocaleTimeString: unit -> string
        abstract valueOf: unit -> float
        abstract getTime: unit -> float
        abstract getFullYear: unit -> float
        abstract getUTCFullYear: unit -> float
        abstract getMonth: unit -> float
        abstract getUTCMonth: unit -> float
        abstract getDate: unit -> float
        abstract getUTCDate: unit -> float
        abstract getDay: unit -> float
        abstract getUTCDay: unit -> float
        abstract getHours: unit -> float
        abstract getUTCHours: unit -> float
        abstract getMinutes: unit -> float
        abstract getUTCMinutes: unit -> float
        abstract getSeconds: unit -> float
        abstract getUTCSeconds: unit -> float
        abstract getMilliseconds: unit -> float
        abstract getUTCMilliseconds: unit -> float
        abstract getTimezoneOffset: unit -> float
        abstract setTime: time: float -> float
        abstract setMilliseconds: ms: float -> float
        abstract setUTCMilliseconds: ms: float -> float
        abstract setSeconds: sec: float * ?ms: float -> float
        abstract setUTCSeconds: sec: float * ?ms: float -> float
        abstract setMinutes: min: float * ?sec: float * ?ms: float -> float
        abstract setUTCMinutes: min: float * ?sec: float * ?ms: float -> float
        abstract setHours: hours: float * ?min: float * ?sec: float * ?ms: float -> float
        abstract setUTCHours: hours: float * ?min: float * ?sec: float * ?ms: float -> float
        abstract setDate: date: float -> float
        abstract setUTCDate: date: float -> float
        abstract setMonth: month: float * ?date: float -> float
        abstract setUTCMonth: month: float * ?date: float -> float
        abstract setFullYear: year: float * ?month: float * ?date: float -> float
        abstract setUTCFullYear: year: float * ?month: float * ?date: float -> float
        abstract toUTCString: unit -> string
        abstract toISOString: unit -> string
        abstract toJSON: ?key: obj -> string

    and [<AllowNullLiteral>] DateConstructor =
        [<Emit("new $0()")>] abstract Create: unit -> DateTime
        [<Emit("new $0($1)")>] abstract Create: value: float -> DateTime
        [<Emit("new $0($1)")>] abstract Create: value: string -> DateTime
        [<Emit("new $0($1...)")>] abstract Create: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> DateTime
        [<Emit("$0()")>] abstract Invoke: unit -> string
        abstract parse: s: string -> float
        abstract UTC: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> float
        abstract now: unit -> float

    and [<AllowNullLiteral>] JSON =
        abstract parse: text: string * ?reviver: (obj->obj->obj) -> obj
        abstract stringify: value: obj * ?replacer: (string->obj->obj) * ?space: obj -> string

    and [<AllowNullLiteral>] Map<'K, 'V> =
        abstract size: int
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract entries: unit -> seq<'K * 'V>
        abstract forEach: callbackfn: ('V->'K->Map<'K, 'V>->unit) * ?thisArg: obj -> unit
        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract keys: unit -> seq<'K>
        abstract set: key: 'K * value: 'V -> Map<'K, 'V>
        abstract values: unit -> seq<'V>

    and [<AllowNullLiteral>] MapConstructor =
        [<Emit("new $0($1...)")>] abstract Create: ?iterable: seq<'K * 'V> -> Map<'K, 'V>

    and [<AllowNullLiteral>] WeakMap<'K, 'V> =
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract set: key: 'K * value: 'V -> WeakMap<'K, 'V>

    and [<AllowNullLiteral>] WeakMapConstructor =
        [<Emit("new $0($1...)")>] abstract Create: ?iterable: seq<'K * 'V> -> WeakMap<'K, 'V>

    and [<AllowNullLiteral>] Set<'T> =
        abstract size: int
        abstract add: value: 'T -> Set<'T>
        abstract clear: unit -> unit
        abstract delete: value: 'T -> bool
        abstract entries: unit -> seq<'T * 'T>
        abstract forEach: callbackfn: ('T->'T->Set<'T>->unit) * ?thisArg: obj -> unit
        abstract has: value: 'T -> bool
        abstract keys: unit -> seq<'T>
        abstract values: unit -> seq<'T>

    and [<AllowNullLiteral>] SetConstructor =
        [<Emit("new $0($1...)")>] abstract Create: ?iterable: seq<'T> -> Set<'T>

    and [<AllowNullLiteral>] WeakSet<'T> =
        abstract add: value: 'T -> WeakSet<'T>
        abstract clear: unit -> unit
        abstract delete: value: 'T -> bool
        abstract has: value: 'T -> bool

    and [<AllowNullLiteral>] WeakSetConstructor =
        [<Emit("new $0($1...)")>] abstract Create: ?iterable: seq<'T> -> WeakSet<'T>

    and [<AllowNullLiteral>] Promise<'T> =
        abstract ``then``: ?onfulfilled: ('T->'TResult) * ?onrejected: (obj->'TResult) -> Promise<'TResult>
        abstract catch: ?onrejected: (obj->'T) -> Promise<'T>

    and [<AllowNullLiteral>] PromiseConstructor =
        [<Emit("new $0($1...)")>] abstract Create: executor: ((obj->unit) -> (obj->unit) -> unit) -> Promise<'T>
        abstract all: [<ParamArray>] values: obj[] -> Promise<obj>
        abstract race: values: obj seq -> Promise<obj>
        abstract reject: reason: obj -> Promise<unit>
        abstract reject: reason: obj -> Promise<'T>
        abstract resolve: value: 'T -> Promise<'T>
        abstract resolve: unit -> Promise<unit>

    and [<AllowNullLiteral>] RegExpConstructor =
        [<Emit("new $0($1...)")>] abstract Create: pattern: string * ?flags: string -> Regex

    and [<AllowNullLiteral>] ArrayBuffer =
        abstract byteLength: int
        abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer

    and [<AllowNullLiteral>] ArrayBufferConstructor =
        [<Emit("new $0($1...)")>] abstract Create: byteLength: int -> ArrayBuffer
        abstract isView: arg: obj -> bool

    and [<AllowNullLiteral>] ArrayBufferView =
        abstract buffer: ArrayBuffer
        abstract byteLength: int
        abstract byteOffset: int

    and ArrayBufferViewConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> ArrayBufferView

    and [<AllowNullLiteral>] DataView =
        abstract buffer: ArrayBuffer
        abstract byteLength: int
        abstract byteOffset: int
        abstract getFloat32: byteOffset: int * ?littleEndian: bool -> float32
        abstract getFloat64: byteOffset: int * ?littleEndian: bool -> float
        abstract getInt8: byteOffset: int -> sbyte
        abstract getInt16: byteOffset: int * ?littleEndian: bool -> int16
        abstract getInt32: byteOffset: int * ?littleEndian: bool -> int32
        abstract getUint8: byteOffset: int -> byte
        abstract getUint16: byteOffset: int * ?littleEndian: bool -> uint16
        abstract getUint32: byteOffset: int * ?littleEndian: bool -> uint32
        abstract setFloat32: byteOffset: int * value: float32 * ?littleEndian: bool -> unit
        abstract setFloat64: byteOffset: int * value: float * ?littleEndian: bool -> unit
        abstract setInt8: byteOffset: int * value: sbyte -> unit
        abstract setInt16: byteOffset: int * value: int16 * ?littleEndian: bool -> unit
        abstract setInt32: byteOffset: int * value: int32 * ?littleEndian: bool -> unit
        abstract setUint8: byteOffset: int * value: byte -> unit
        abstract setUint16: byteOffset: int * value: uint16 * ?littleEndian: bool -> unit
        abstract setUint32: byteOffset: int * value: uint32 * ?littleEndian: bool -> unit

    and [<AllowNullLiteral>] DataViewConstructor =
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: int * ?byteLength: float -> DataView

    and TypedArray =
        abstract buffer: ArrayBuffer
        abstract byteLength: int
        abstract byteOffset: int
        abstract length: int
        abstract copyWithin: targetStartIndex:int * start:int * ? ``end``:int -> unit
        abstract entries: unit -> obj
        abstract keys: unit -> obj
        abstract join: separator:string -> string

    and TypedArray<'T> =
        inherit TypedArray
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> 'T with get, set
        abstract fill: value:'T * ?``begin``:int * ?``end``:int -> TypedArray<'T>
        abstract filter: ('T -> int -> TypedArray<'T> -> bool) -> TypedArray<'T>
        abstract filter: ('T -> int -> bool) -> TypedArray<'T>
        abstract filter: ('T -> bool) -> TypedArray<'T>
        abstract find: ('T -> int -> TypedArray<'T> -> bool) -> 'T option
        abstract find: ('T -> int -> bool) -> 'T option
        abstract find: ('T -> bool) -> 'T option
        abstract findIndex: ('T -> int -> TypedArray<'T> -> bool) -> int
        abstract findIndex: ('T -> int -> bool) -> int
        abstract findIndex: ('T -> bool) -> int
        abstract forEach: ('T -> int -> TypedArray<'T> -> bool) -> unit
        abstract forEach: ('T -> int -> bool) -> unit
        abstract forEach: ('T -> bool) -> unit
        abstract includes: searchElement:'T * ?fromIndex:int -> bool
        abstract indexOf: searchElement:'T * ?fromIndex:int -> int
        abstract lastIndexOf: searchElement:'T * ?fromIndex:int -> int
        abstract map: ('T -> int -> TypedArray<'T> -> 'U) -> TypedArray<'U>
        abstract map: ('T -> int -> 'U) -> TypedArray<'U>
        abstract map: ('T -> 'U) -> TypedArray<'U>
        abstract reduce: ('State -> 'T -> int -> TypedArray<'T> -> 'State) * state:'State -> 'State
        abstract reduce: ('State -> 'T -> int -> 'State) * state:'State -> 'State
        abstract reduce: ('State -> 'T -> 'State) * state:'State -> 'State
        abstract reduceRight: ('State -> 'T -> int -> TypedArray<'T> -> 'State) * state:'State -> 'State
        abstract reduceRight: ('State -> 'T -> int -> 'State) * state:'State -> 'State
        abstract reduceRight: ('State -> 'T -> 'State) * state:'State -> 'State
        abstract reverse: unit -> TypedArray<'T>
        abstract set: source:Array * ?offset:int -> unit
        abstract set: source:#TypedArray * ?offset:int -> unit
        abstract slice: ?``begin``:int * ?``end``:int -> TypedArray<'T>
        abstract some: ('T -> int -> TypedArray<'T> -> bool) -> bool
        abstract some: ('T -> int -> bool) -> bool
        abstract some: ('T -> bool) -> bool
        abstract sort: ?sortFunction:('T -> 'T -> int) -> TypedArray<'T>
        abstract subarray: ?``begin``:int * ?``end``:int -> TypedArray<'T>
        abstract values: unit -> obj


    and Int8Array = TypedArray<int8>

    and Int8ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Int8Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Int8Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Int8Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Int8Array


    and Uint8Array = TypedArray<uint8>

    and Uint8ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Uint8Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Uint8Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Uint8Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Uint8Array


    and Uint8ClampedArray = TypedArray<uint8>

    and Uint8ClampedArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Uint8ClampedArray
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Uint8ClampedArray
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Uint8ClampedArray
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Uint8ClampedArray


    and Int16Array = TypedArray<int16>

    and Int16ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Int16Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Int16Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Int16Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Int16Array


    and Uint16Array = TypedArray<uint16>

    and Uint16ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Uint16Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Uint16Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Uint16Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Uint16Array


    and Int32Array = TypedArray<int32>

    and Int32ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Int32Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Int32Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Int32Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Int32Array


    and Uint32Array = TypedArray<uint32>

    and Uint32ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Uint32Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Uint32Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Uint32Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Uint32Array


    and Float32Array = TypedArray<float32>

    and Float32ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Float32Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Float32Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Float32Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Float32Array


    and Float64Array = TypedArray<float>

    and Float64ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> Float64Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> Float64Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> Float64Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> Float64Array


    and BigInt64Array = TypedArray<bigint>

    and BigInt64ArrayConstructor =
        [<Emit "new $0($1...)">] abstract Create: size: int -> BigInt64Array
        [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> BigInt64Array
        [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> BigInt64Array
        [<Emit "new $0($1...)">] abstract Create: data:obj -> BigInt64Array


    // no equivalent ?
    //and BigUint64Array = TypedArray<BigUint64Array>

    // and BigUint64ArrayConstructor =
    //   [<Emit "new $0($1...)">] abstract Create: size: int -> BigUint64Array
    //   [<Emit "new $0($1...)">] abstract Create: typedArray: TypedArray -> BigUint64Array
    //   [<Emit "new $0($1...)">] abstract Create: buffer: ArrayBuffer * ?offset:int * ?length:int -> BigUint64Array
    //   [<Emit "new $0($1...)">] abstract Create: data:obj -> BigUint64Array


    and [<AllowNullLiteral>] Console =
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
        abstract profile: ?reportName: string -> unit
        abstract profileEnd: unit -> unit
        abstract time: ?timerName: string -> unit
        abstract timeEnd: ?timerName: string -> unit
        abstract trace: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
        abstract warn: ?message: obj * [<ParamArray>] optionalParams: obj[] -> unit
        abstract table: ?data: obj -> unit

    let [<Global>] NaN: float = nativeOnly
    let [<Global>] Infinity: float = nativeOnly
    let [<Global>] Math: Math = nativeOnly
    let [<Global>] JSON: JSON = nativeOnly
    let [<Global>] eval: string -> string = nativeOnly
    let [<Global>] isFinite: float -> bool = nativeOnly
    let [<Global>] isNaN: float -> bool = nativeOnly
    let [<Global>] parseFloat: string -> float = nativeOnly
    let [<Global>] parseInt: string -> int -> int = nativeOnly
    let [<Global>] decodeURI: string -> string = nativeOnly
    let [<Global>] decodeURIComponent: string -> string = nativeOnly
    let [<Global>] encodeURI: string -> string = nativeOnly
    let [<Global>] encodeURIComponent: string -> string = nativeOnly
    let [<Global>] console : Console = nativeOnly
    let [<Global>] setTimeout (callback: unit -> unit) (ms: int): int = nativeOnly
    let [<Global>] clearTimeout (token: int): unit = nativeOnly
    let [<Global>] setInterval(callback: unit -> unit) (ms: int) : int = nativeOnly
    let [<Global>] clearInterval (token: int): unit = nativeOnly
    let [<Emit("debugger")>] debugger () : unit = nativeOnly
    let [<Emit("void 0")>] undefined<'a> : 'a = nativeOnly

    [<Literal>]
    let private CONSTRUCTORS_WARNING = "JS constructors are now in Fable.Core.JS.Constructors module to prevent conflicts with modules with same name"

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Number: NumberConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Object: ObjectConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Date: DateConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Map: MapConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] WeakMap: WeakMapConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Set: SetConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] WeakSet: WeakSetConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Promise: PromiseConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] RegExp: RegExpConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Array: ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] DataView: DataViewConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] ArrayBuffer: ArrayBufferConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] ArrayBufferView: ArrayBufferViewConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Int8Array: Int8ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Uint8Array: Uint8ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Uint8ClampedArray: Uint8ClampedArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Int16Array: Int16ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Uint16Array: Uint16ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Int32Array: Int32ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Uint32Array: Uint32ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Float32Array: Float32ArrayConstructor = nativeOnly

    [<Obsolete(CONSTRUCTORS_WARNING)>]
    let [<Global>] Float64Array: Float64ArrayConstructor = nativeOnly

    // [<ObsoleCONSTRUCTORS_WARNING)>]
    // let [<Global>] BigInt64Array: BigInt64ArrayConstructor = nativeOnly

    [<RequireQualifiedAccess>]
    module Constructors =

        let [<Global>] Number: NumberConstructor = nativeOnly
        let [<Global>] Object: ObjectConstructor = nativeOnly
        let [<Global>] Date: DateConstructor = nativeOnly
        let [<Global>] Map: MapConstructor = nativeOnly
        let [<Global>] WeakMap: WeakMapConstructor = nativeOnly
        let [<Global>] Set: SetConstructor = nativeOnly
        let [<Global>] WeakSet: WeakSetConstructor = nativeOnly
        let [<Global>] Promise: PromiseConstructor = nativeOnly
        let [<Global>] RegExp: RegExpConstructor = nativeOnly
        let [<Global>] Array: ArrayConstructor = nativeOnly
        let [<Global>] DataView: DataViewConstructor = nativeOnly
        let [<Global>] ArrayBuffer: ArrayBufferConstructor = nativeOnly
        let [<Global>] ArrayBufferView: ArrayBufferViewConstructor = nativeOnly
        let [<Global>] Int8Array: Int8ArrayConstructor = nativeOnly
        let [<Global>] Uint8Array: Uint8ArrayConstructor = nativeOnly
        let [<Global>] Uint8ClampedArray: Uint8ClampedArrayConstructor = nativeOnly
        let [<Global>] Int16Array: Int16ArrayConstructor = nativeOnly
        let [<Global>] Uint16Array: Uint16ArrayConstructor = nativeOnly
        let [<Global>] Int32Array: Int32ArrayConstructor = nativeOnly
        let [<Global>] Uint32Array: Uint32ArrayConstructor = nativeOnly
        let [<Global>] Float32Array: Float32ArrayConstructor = nativeOnly
        let [<Global>] Float64Array: Float64ArrayConstructor = nativeOnly
        let [<Global>] BigInt64Array: BigInt64ArrayConstructor = nativeOnly
        // let [<Global>] BigUint64Array: BigUint64ArrayConstructor = nativeOnly
