namespace Fable.Import.JS
open System

type private ImportAttribute(path) =
    inherit Attribute()

type private GlobalAttribute() =
    inherit Attribute()

type EraseAttribute() =
    inherit System.Attribute()

type [<Erase>] U2<'a, 'b> =
    | Case1 of 'a | Case2 of 'b

type [<Erase>] U3<'a, 'b, 'c> =
    | Case1 of 'a | Case2 of 'b | Case3 of 'c

type PropertyDescriptor =
    abstract configurable: bool option with get, set
    abstract enumerable: bool option with get, set
    abstract value: obj option with get, set
    abstract writable: bool option with get, set
    abstract get: unit -> obj
    abstract set: v: obj -> unit

and PropertyDescriptorMap =
    interface end

and Object =
    abstract ``constructor``: Function with get, set
    abstract toString: unit -> string
    abstract toLocaleString: unit -> string
    abstract valueOf: unit -> obj
    abstract hasOwnProperty: v: string -> bool
    abstract isPrototypeOf: v: obj -> bool
    abstract propertyIsEnumerable: v: string -> bool

and ObjectConstructor =
    abstract createNew: ?value: obj -> obj
    abstract prototype: obj with get, set
    abstract getPrototypeOf: o: obj -> obj
    abstract getOwnPropertyDescriptor: o: obj * p: string -> PropertyDescriptor
    abstract getOwnPropertyNames: o: obj -> ResizeArray<string>
    abstract create: o: obj * ?properties: PropertyDescriptorMap -> obj
    abstract defineProperty: o: obj * p: string * attributes: PropertyDescriptor -> obj
    abstract defineProperties: o: obj * properties: PropertyDescriptorMap -> obj
    abstract seal: o: 'T -> 'T
    abstract freeze: o: 'T -> 'T
    abstract preventExtensions: o: 'T -> 'T
    abstract isSealed: o: obj -> bool
    abstract isFrozen: o: obj -> bool
    abstract isExtensible: o: obj -> bool
    abstract keys: o: obj -> ResizeArray<string>

and Function =
    abstract prototype: obj with get, set
    abstract length: float with get, set
    abstract arguments: obj with get, set
    abstract caller: Function with get, set
    abstract apply: thisArg: obj * ?argArray: obj -> obj
    abstract call: thisArg: obj * [<ParamArray>] argArray: obj[] -> obj
    abstract bind: thisArg: obj * [<ParamArray>] argArray: obj[] -> obj

and FunctionConstructor =
    abstract createNew: [<ParamArray>] args: string[] -> Function
    abstract prototype: Function with get, set

and IArguments =
    abstract length: float with get, set
    abstract callee: Function with get, set

and String =
    abstract length: float with get, set
    abstract toString: unit -> string
    abstract charAt: pos: float -> string
    abstract charCodeAt: index: float -> float
    abstract concat: [<ParamArray>] strings: string[] -> string
    abstract indexOf: searchString: string * ?position: float -> float
    abstract lastIndexOf: searchString: string * ?position: float -> float
    abstract localeCompare: that: string -> float
    abstract ``match``: regexp: string -> RegExpMatchArray
    abstract ``match``: regexp: RegExp -> RegExpMatchArray
    abstract replace: searchValue: string * replaceValue: string -> string
    abstract replace: searchValue: string * replacer: Func<string, ResizeArray<obj>, string> -> string
    abstract replace: searchValue: RegExp * replaceValue: string -> string
    abstract replace: searchValue: RegExp * replacer: Func<string, ResizeArray<obj>, string> -> string
    abstract search: regexp: string -> float
    abstract search: regexp: RegExp -> float
    abstract slice: ?start: float * ?``end``: float -> string
    abstract split: separator: string * ?limit: float -> ResizeArray<string>
    abstract split: separator: RegExp * ?limit: float -> ResizeArray<string>
    abstract substring: start: float * ?``end``: float -> string
    abstract toLowerCase: unit -> string
    abstract toLocaleLowerCase: unit -> string
    abstract toUpperCase: unit -> string
    abstract toLocaleUpperCase: unit -> string
    abstract trim: unit -> string
    abstract substr: from: float * ?length: float -> string
    abstract valueOf: unit -> string

and StringConstructor =
    abstract createNew: ?value: obj -> String
    abstract prototype: String with get, set
    abstract fromCharCode: [<ParamArray>] codes: float[] -> string

and Boolean =
    abstract valueOf: unit -> bool

and BooleanConstructor =
    abstract createNew: ?value: obj -> Boolean
    abstract prototype: Boolean with get, set

and Number =
    abstract toString: ?radix: float -> string
    abstract toFixed: ?fractionDigits: float -> string
    abstract toExponential: ?fractionDigits: float -> string
    abstract toPrecision: ?precision: float -> string
    abstract valueOf: unit -> float

and NumberConstructor =
    abstract createNew: ?value: obj -> Number
    abstract prototype: Number with get, set
    abstract MAX_VALUE: float with get, set
    abstract MIN_VALUE: float with get, set
    abstract NaN: float with get, set
    abstract NEGATIVE_INFINITY: float with get, set
    abstract POSITIVE_INFINITY: float with get, set

and TemplateStringsArray =
    inherit Array<string>
    abstract raw: ResizeArray<string> with get, set

and Math =
    abstract E: float with get, set
    abstract LN10: float with get, set
    abstract LN2: float with get, set
    abstract LOG2E: float with get, set
    abstract LOG10E: float with get, set
    abstract PI: float with get, set
    abstract SQRT1_2: float with get, set
    abstract SQRT2: float with get, set
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

and Date =
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

and DateConstructor =
    abstract createNew: unit -> DateTime
    abstract createNew: value: float -> DateTime
    abstract createNew: value: string -> DateTime
    abstract createNew: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> DateTime
    abstract prototype: DateTime with get, set
    abstract parse: s: string -> float
    abstract UTC: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> float
    abstract now: unit -> float

and RegExpMatchArray =
    inherit Array<string>
    abstract index: float option with get, set
    abstract input: string option with get, set

and RegExpExecArray =
    inherit Array<string>
    abstract index: float with get, set
    abstract input: string with get, set

and RegExp =
    abstract source: string with get, set
    abstract ``global``: bool with get, set
    abstract ignoreCase: bool with get, set
    abstract multiline: bool with get, set
    abstract lastIndex: float with get, set
    abstract exec: string: string -> RegExpExecArray
    abstract test: string: string -> bool
    abstract compile: unit -> RegExp

and RegExpConstructor =
    abstract createNew: pattern: string * ?flags: string -> RegExp
    abstract prototype: RegExp with get, set
    abstract ``$1``: string with get, set
    abstract ``$2``: string with get, set
    abstract ``$3``: string with get, set
    abstract ``$4``: string with get, set
    abstract ``$5``: string with get, set
    abstract ``$6``: string with get, set
    abstract ``$7``: string with get, set
    abstract ``$8``: string with get, set
    abstract ``$9``: string with get, set
    abstract lastMatch: string with get, set

and Error =
    abstract name: string with get, set
    abstract message: string with get, set

and ErrorConstructor =
    abstract createNew: ?message: string -> Error
    abstract prototype: Error with get, set

and EvalError =
    inherit Error

and EvalErrorConstructor =
    abstract createNew: ?message: string -> EvalError
    abstract prototype: EvalError with get, set

and RangeError =
    inherit Error

and RangeErrorConstructor =
    abstract createNew: ?message: string -> RangeError
    abstract prototype: RangeError with get, set

and ReferenceError =
    inherit Error

and ReferenceErrorConstructor =
    abstract createNew: ?message: string -> ReferenceError
    abstract prototype: ReferenceError with get, set

and SyntaxError =
    inherit Error

and SyntaxErrorConstructor =
    abstract createNew: ?message: string -> SyntaxError
    abstract prototype: SyntaxError with get, set

and TypeError =
    inherit Error

and TypeErrorConstructor =
    abstract createNew: ?message: string -> TypeError
    abstract prototype: TypeError with get, set

and URIError =
    inherit Error

and URIErrorConstructor =
    abstract createNew: ?message: string -> URIError
    abstract prototype: URIError with get, set

and JSON =
    abstract parse: text: string * ?reviver: Func<obj, obj, obj> -> obj
    abstract stringify: value: obj -> string
    abstract stringify: value: obj * replacer: Func<string, obj, obj> -> string
    abstract stringify: value: obj * replacer: ResizeArray<obj> -> string
    abstract stringify: value: obj * replacer: Func<string, obj, obj> * space: U2<string, float> -> string
    abstract stringify: value: obj * replacer: ResizeArray<obj> * space: U2<string, float> -> string

and Array<'T> =
    abstract length: float with get, set
    abstract toString: unit -> string
    abstract toLocaleString: unit -> string
    abstract push: [<ParamArray>] items: 'T[] -> float
    abstract pop: unit -> 'T
    abstract concat: [<ParamArray>] items: 'U[] -> ResizeArray<'T>
    abstract concat: [<ParamArray>] items: 'T[] -> ResizeArray<'T>
    abstract join: ?separator: string -> string
    abstract reverse: unit -> ResizeArray<'T>
    abstract shift: unit -> 'T
    abstract slice: ?start: float * ?``end``: float -> ResizeArray<'T>
    abstract sort: ?compareFn: Func<'T, 'T, float> -> ResizeArray<'T>
    abstract splice: start: float -> ResizeArray<'T>
    abstract splice: start: float * deleteCount: float * [<ParamArray>] items: 'T[] -> ResizeArray<'T>
    abstract unshift: [<ParamArray>] items: 'T[] -> float
    abstract indexOf: searchElement: 'T * ?fromIndex: float -> float
    abstract lastIndexOf: searchElement: 'T * ?fromIndex: float -> float
    abstract every: callbackfn: Func<'T, float, ResizeArray<'T>, bool> * ?thisArg: obj -> bool
    abstract some: callbackfn: Func<'T, float, ResizeArray<'T>, bool> * ?thisArg: obj -> bool
    abstract forEach: callbackfn: Func<'T, float, ResizeArray<'T>, unit> * ?thisArg: obj -> unit
    abstract map: callbackfn: Func<'T, float, ResizeArray<'T>, 'U> * ?thisArg: obj -> ResizeArray<'U>
    abstract filter: callbackfn: Func<'T, float, ResizeArray<'T>, bool> * ?thisArg: obj -> ResizeArray<'T>
    abstract reduce: callbackfn: Func<'T, 'T, float, ResizeArray<'T>, 'T> * ?initialValue: 'T -> 'T
    abstract reduce: callbackfn: Func<'U, 'T, float, ResizeArray<'T>, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<'T, 'T, float, ResizeArray<'T>, 'T> * ?initialValue: 'T -> 'T
    abstract reduceRight: callbackfn: Func<'U, 'T, float, ResizeArray<'T>, 'U> * initialValue: 'U -> 'U

and ArrayConstructor =
    abstract createNew: ?arrayLength: float -> ResizeArray<obj>
    abstract createNew: arrayLength: float -> ResizeArray<'T>
    abstract createNew: [<ParamArray>] items: 'T[] -> ResizeArray<'T>
    abstract prototype: Array<obj> with get, set
    abstract isArray: arg: obj -> obj

and TypedPropertyDescriptor<'T> =
    abstract enumerable: bool option with get, set
    abstract configurable: bool option with get, set
    abstract writable: bool option with get, set
    abstract value: 'T option with get, set
    abstract get: Func<'T> option with get, set
    abstract set: Func<'T, unit> option with get, set

and PromiseConstructorLike =
    obj

and PromiseLike<'T> =
    abstract ``then``: ?onfulfilled: Func<'T, U2<'TResult, PromiseLike<'TResult>>> * ?onrejected: Func<obj, U2<'TResult, PromiseLike<'TResult>>> -> PromiseLike<'TResult>
    abstract ``then``: ?onfulfilled: Func<'T, U2<'TResult, PromiseLike<'TResult>>> * ?onrejected: Func<obj, unit> -> PromiseLike<'TResult>

and ArrayLike<'T> =
    abstract length: float with get, set

and ArrayBuffer =
    abstract byteLength: float with get, set
    abstract slice: ``begin``: float * ?``end``: float -> ArrayBuffer

and ArrayBufferConstructor =
    abstract createNew: byteLength: float -> ArrayBuffer
    abstract prototype: ArrayBuffer with get, set
    abstract isView: arg: obj -> obj

and ArrayBufferView =
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set

and DataView =
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract getFloat32: byteOffset: float * ?littleEndian: bool -> float
    abstract getFloat64: byteOffset: float * ?littleEndian: bool -> float
    abstract getInt8: byteOffset: float -> float
    abstract getInt16: byteOffset: float * ?littleEndian: bool -> float
    abstract getInt32: byteOffset: float * ?littleEndian: bool -> float
    abstract getUint8: byteOffset: float -> float
    abstract getUint16: byteOffset: float * ?littleEndian: bool -> float
    abstract getUint32: byteOffset: float * ?littleEndian: bool -> float
    abstract setFloat32: byteOffset: float * value: float * ?littleEndian: bool -> unit
    abstract setFloat64: byteOffset: float * value: float * ?littleEndian: bool -> unit
    abstract setInt8: byteOffset: float * value: float -> unit
    abstract setInt16: byteOffset: float * value: float * ?littleEndian: bool -> unit
    abstract setInt32: byteOffset: float * value: float * ?littleEndian: bool -> unit
    abstract setUint8: byteOffset: float * value: float -> unit
    abstract setUint16: byteOffset: float * value: float * ?littleEndian: bool -> unit
    abstract setUint32: byteOffset: float * value: float * ?littleEndian: bool -> unit

and DataViewConstructor =
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?byteLength: float -> DataView

and Int8Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Int8Array
    abstract every: callbackfn: Func<float, float, Int8Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Int8Array
    abstract filter: callbackfn: Func<float, float, Int8Array, bool> * ?thisArg: obj -> Int8Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Int8Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Int8Array, float> * ?thisArg: obj -> Int8Array
    abstract reduce: callbackfn: Func<float, float, float, Int8Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Int8Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Int8Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Int8Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Int8Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Int8Array
    abstract some: callbackfn: Func<float, float, Int8Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Int8Array
    abstract subarray: ``begin``: float * ?``end``: float -> Int8Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Int8ArrayConstructor =
    abstract createNew: length: float -> Int8Array
    abstract createNew: array: ArrayLike<float> -> Int8Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int8Array
    abstract prototype: Int8Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Int8Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int8Array

and Uint8Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Uint8Array
    abstract every: callbackfn: Func<float, float, Uint8Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Uint8Array
    abstract filter: callbackfn: Func<float, float, Uint8Array, bool> * ?thisArg: obj -> Uint8Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Uint8Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Uint8Array, float> * ?thisArg: obj -> Uint8Array
    abstract reduce: callbackfn: Func<float, float, float, Uint8Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Uint8Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Uint8Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Uint8Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Uint8Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Uint8Array
    abstract some: callbackfn: Func<float, float, Uint8Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Uint8Array
    abstract subarray: ``begin``: float * ?``end``: float -> Uint8Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Uint8ArrayConstructor =
    abstract createNew: length: float -> Uint8Array
    abstract createNew: array: ArrayLike<float> -> Uint8Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint8Array
    abstract prototype: Uint8Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint8Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8Array

and Uint8ClampedArray =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Uint8ClampedArray
    abstract every: callbackfn: Func<float, float, Uint8ClampedArray, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Uint8ClampedArray
    abstract filter: callbackfn: Func<float, float, Uint8ClampedArray, bool> * ?thisArg: obj -> Uint8ClampedArray
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Uint8ClampedArray, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Uint8ClampedArray, float> * ?thisArg: obj -> Uint8ClampedArray
    abstract reduce: callbackfn: Func<float, float, float, Uint8ClampedArray, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Uint8ClampedArray, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Uint8ClampedArray, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Uint8ClampedArray, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Uint8ClampedArray
    abstract set: index: float * value: float -> unit
    abstract set: array: Uint8ClampedArray * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Uint8ClampedArray
    abstract some: callbackfn: Func<float, float, Uint8ClampedArray, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Uint8ClampedArray
    abstract subarray: ``begin``: float * ?``end``: float -> Uint8ClampedArray
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Uint8ClampedArrayConstructor =
    abstract createNew: length: float -> Uint8ClampedArray
    abstract createNew: array: ArrayLike<float> -> Uint8ClampedArray
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint8ClampedArray
    abstract prototype: Uint8ClampedArray with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint8ClampedArray
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8ClampedArray

and Int16Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Int16Array
    abstract every: callbackfn: Func<float, float, Int16Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Int16Array
    abstract filter: callbackfn: Func<float, float, Int16Array, bool> * ?thisArg: obj -> Int16Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Int16Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Int16Array, float> * ?thisArg: obj -> Int16Array
    abstract reduce: callbackfn: Func<float, float, float, Int16Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Int16Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Int16Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Int16Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Int16Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Int16Array
    abstract some: callbackfn: Func<float, float, Int16Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Int16Array
    abstract subarray: ``begin``: float * ?``end``: float -> Int16Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Int16ArrayConstructor =
    abstract createNew: length: float -> Int16Array
    abstract createNew: array: ArrayLike<float> -> Int16Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int16Array
    abstract prototype: Int16Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Int16Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int16Array

and Uint16Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Uint16Array
    abstract every: callbackfn: Func<float, float, Uint16Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Uint16Array
    abstract filter: callbackfn: Func<float, float, Uint16Array, bool> * ?thisArg: obj -> Uint16Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Uint16Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Uint16Array, float> * ?thisArg: obj -> Uint16Array
    abstract reduce: callbackfn: Func<float, float, float, Uint16Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Uint16Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Uint16Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Uint16Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Uint16Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Uint16Array
    abstract some: callbackfn: Func<float, float, Uint16Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Uint16Array
    abstract subarray: ``begin``: float * ?``end``: float -> Uint16Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Uint16ArrayConstructor =
    abstract createNew: length: float -> Uint16Array
    abstract createNew: array: ArrayLike<float> -> Uint16Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint16Array
    abstract prototype: Uint16Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint16Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint16Array

and Int32Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Int32Array
    abstract every: callbackfn: Func<float, float, Int32Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Int32Array
    abstract filter: callbackfn: Func<float, float, Int32Array, bool> * ?thisArg: obj -> Int32Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Int32Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Int32Array, float> * ?thisArg: obj -> Int32Array
    abstract reduce: callbackfn: Func<float, float, float, Int32Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Int32Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Int32Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Int32Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Int32Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Int32Array
    abstract some: callbackfn: Func<float, float, Int32Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Int32Array
    abstract subarray: ``begin``: float * ?``end``: float -> Int32Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Int32ArrayConstructor =
    abstract createNew: length: float -> Int32Array
    abstract createNew: array: ArrayLike<float> -> Int32Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int32Array
    abstract prototype: Int32Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Int32Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int32Array

and Uint32Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Uint32Array
    abstract every: callbackfn: Func<float, float, Uint32Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Uint32Array
    abstract filter: callbackfn: Func<float, float, Uint32Array, bool> * ?thisArg: obj -> Uint32Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Uint32Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Uint32Array, float> * ?thisArg: obj -> Uint32Array
    abstract reduce: callbackfn: Func<float, float, float, Uint32Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Uint32Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Uint32Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Uint32Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Uint32Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Uint32Array
    abstract some: callbackfn: Func<float, float, Uint32Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Uint32Array
    abstract subarray: ``begin``: float * ?``end``: float -> Uint32Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Uint32ArrayConstructor =
    abstract createNew: length: float -> Uint32Array
    abstract createNew: array: ArrayLike<float> -> Uint32Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint32Array
    abstract prototype: Uint32Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint32Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint32Array

and Float32Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Float32Array
    abstract every: callbackfn: Func<float, float, Float32Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Float32Array
    abstract filter: callbackfn: Func<float, float, Float32Array, bool> * ?thisArg: obj -> Float32Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Float32Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Float32Array, float> * ?thisArg: obj -> Float32Array
    abstract reduce: callbackfn: Func<float, float, float, Float32Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Float32Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Float32Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Float32Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Float32Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Float32Array
    abstract some: callbackfn: Func<float, float, Float32Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Float32Array
    abstract subarray: ``begin``: float * ?``end``: float -> Float32Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Float32ArrayConstructor =
    abstract createNew: length: float -> Float32Array
    abstract createNew: array: ArrayLike<float> -> Float32Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Float32Array
    abstract prototype: Float32Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Float32Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float32Array

and Float64Array =
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract buffer: ArrayBuffer with get, set
    abstract byteLength: float with get, set
    abstract byteOffset: float with get, set
    abstract length: float with get, set
    abstract copyWithin: target: float * start: float * ?``end``: float -> Float64Array
    abstract every: callbackfn: Func<float, float, Float64Array, bool> * ?thisArg: obj -> bool
    abstract fill: value: float * ?start: float * ?``end``: float -> Float64Array
    abstract filter: callbackfn: Func<float, float, Float64Array, bool> * ?thisArg: obj -> Float64Array
    abstract find: predicate: Func<float, float, Array<float>, bool> * ?thisArg: obj -> float
    abstract findIndex: predicate: Func<float, bool> * ?thisArg: obj -> float
    abstract forEach: callbackfn: Func<float, float, Float64Array, unit> * ?thisArg: obj -> unit
    abstract indexOf: searchElement: float * ?fromIndex: float -> float
    abstract join: ?separator: string -> string
    abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
    abstract map: callbackfn: Func<float, float, Float64Array, float> * ?thisArg: obj -> Float64Array
    abstract reduce: callbackfn: Func<float, float, float, Float64Array, float> * ?initialValue: float -> float
    abstract reduce: callbackfn: Func<'U, float, float, Float64Array, 'U> * initialValue: 'U -> 'U
    abstract reduceRight: callbackfn: Func<float, float, float, Float64Array, float> * ?initialValue: float -> float
    abstract reduceRight: callbackfn: Func<'U, float, float, Float64Array, 'U> * initialValue: 'U -> 'U
    abstract reverse: unit -> Float64Array
    abstract set: index: float * value: float -> unit
    abstract set: array: ArrayLike<float> * ?offset: float -> unit
    abstract slice: ?start: float * ?``end``: float -> Float64Array
    abstract some: callbackfn: Func<float, float, Float64Array, bool> * ?thisArg: obj -> bool
    abstract sort: ?compareFn: Func<float, float, float> -> Float64Array
    abstract subarray: ``begin``: float * ?``end``: float -> Float64Array
    abstract toLocaleString: unit -> string
    abstract toString: unit -> string

and Float64ArrayConstructor =
    abstract createNew: length: float -> Float64Array
    abstract createNew: array: ArrayLike<float> -> Float64Array
    abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Float64Array
    abstract prototype: Float64Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Float64Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float64Array

// ES6
type PropertyKey =
    U3<string, float, Symbol>

and Thenable<'T> =
    inherit PromiseLike<'T>

and Symbol =
    abstract toString: unit -> string
    abstract valueOf: unit -> obj

and SymbolConstructor =
    abstract prototype: Symbol with get, set
    abstract hasInstance: Symbol with get, set
    abstract isConcatSpreadable: Symbol with get, set
    abstract iterator: Symbol with get, set
    abstract ``match``: Symbol with get, set
    abstract replace: Symbol with get, set
    abstract search: Symbol with get, set
    abstract species: Symbol with get, set
    abstract split: Symbol with get, set
    abstract toPrimitive: Symbol with get, set
    abstract toStringTag: Symbol with get, set
    abstract unscopables: Symbol with get, set
    abstract ``for``: key: string -> Symbol
    abstract keyFor: sym: Symbol -> string
    
and IteratorResult<'T> =
    abstract ``done``: bool with get, set
    abstract value: 'T option with get, set

and Iterator<'T> =
    abstract next: ?value: obj -> IteratorResult<'T>
    abstract ``return``: ?value: obj -> IteratorResult<'T>
    abstract throw: ?e: obj -> IteratorResult<'T>

and Iterable<'T> =
    interface end

and IterableIterator<'T> =
    inherit Iterator<'T>

and GeneratorFunction =
    inherit Function

and GeneratorFunctionConstructor =
    abstract createNew: [<ParamArray>] args: string[] -> GeneratorFunction
    abstract prototype: GeneratorFunction with get, set
    
and Map<'K, 'V> =
    abstract size: float with get, set
    abstract clear: unit -> unit
    abstract delete: key: 'K -> bool
    abstract entries: unit -> IterableIterator<obj>
    abstract forEach: callbackfn: Func<'V, 'K, Map<'K, 'V>, unit> * ?thisArg: obj -> unit
    abstract get: key: 'K -> 'V
    abstract has: key: 'K -> bool
    abstract keys: unit -> IterableIterator<'K>
    abstract set: key: 'K * ?value: 'V -> Map<'K, 'V>
    abstract values: unit -> IterableIterator<'V>

and MapConstructor =
    abstract createNew: unit -> Map<obj, obj>
    abstract createNew: unit -> Map<'K, 'V>
    abstract createNew: iterable: Iterable<obj> -> Map<'K, 'V>
    abstract prototype: Map<obj, obj> with get, set

and WeakMap<'K, 'V> =
    abstract clear: unit -> unit
    abstract delete: key: 'K -> bool
    abstract get: key: 'K -> 'V
    abstract has: key: 'K -> bool
    abstract set: key: 'K * ?value: 'V -> WeakMap<'K, 'V>

and WeakMapConstructor =
    abstract createNew: unit -> WeakMap<obj, obj>
    abstract createNew: unit -> WeakMap<'K, 'V>
    abstract createNew: iterable: Iterable<obj> -> WeakMap<'K, 'V>
    abstract prototype: WeakMap<obj, obj> with get, set

and Set<'T> =
    abstract size: float with get, set
    abstract add: value: 'T -> Set<'T>
    abstract clear: unit -> unit
    abstract delete: value: 'T -> bool
    abstract entries: unit -> IterableIterator<obj>
    abstract forEach: callbackfn: Func<'T, 'T, Set<'T>, unit> * ?thisArg: obj -> unit
    abstract has: value: 'T -> bool
    abstract keys: unit -> IterableIterator<'T>
    abstract values: unit -> IterableIterator<'T>

and SetConstructor =
    abstract createNew: unit -> Set<obj>
    abstract createNew: unit -> Set<'T>
    abstract createNew: iterable: Iterable<'T> -> Set<'T>
    abstract prototype: Set<obj> with get, set

and WeakSet<'T> =
    abstract add: value: 'T -> WeakSet<'T>
    abstract clear: unit -> unit
    abstract delete: value: 'T -> bool
    abstract has: value: 'T -> bool

and WeakSetConstructor =
    abstract createNew: unit -> WeakSet<obj>
    abstract createNew: unit -> WeakSet<'T>
    abstract createNew: iterable: Iterable<'T> -> WeakSet<'T>
    abstract prototype: WeakSet<obj> with get, set

and ProxyHandler<'T> =
    abstract getPrototypeOf: target: 'T -> obj
    abstract setPrototypeOf: target: 'T * v: obj -> bool
    abstract isExtensible: target: 'T -> bool
    abstract preventExtensions: target: 'T -> bool
    abstract getOwnPropertyDescriptor: target: 'T * p: PropertyKey -> PropertyDescriptor
    abstract has: target: 'T * p: PropertyKey -> bool
    abstract get: target: 'T * p: PropertyKey * receiver: obj -> obj
    abstract set: target: 'T * p: PropertyKey * value: obj * receiver: obj -> bool
    abstract deleteProperty: target: 'T * p: PropertyKey -> bool
    abstract defineProperty: target: 'T * p: PropertyKey * attributes: PropertyDescriptor -> bool
    abstract enumerate: target: 'T -> ResizeArray<PropertyKey>
    abstract ownKeys: target: 'T -> ResizeArray<PropertyKey>
    abstract apply: target: 'T * thisArg: obj * ?argArray: obj -> obj
    abstract construct: target: 'T * thisArg: obj * ?argArray: obj -> obj

and ProxyConstructor =
    abstract createNew: target: 'T * handler: ProxyHandler<'T> -> 'T
    abstract revocable: target: 'T * handler: ProxyHandler<'T> -> obj

and Promise<'T> =
    abstract ``then``: ?onfulfilled: Func<'T, U2<'TResult, PromiseLike<'TResult>>> * ?onrejected: Func<obj, U2<'TResult, PromiseLike<'TResult>>> -> Promise<'TResult>
    abstract ``then``: ?onfulfilled: Func<'T, U2<'TResult, PromiseLike<'TResult>>> * ?onrejected: Func<obj, unit> -> Promise<'TResult>
    abstract catch: ?onrejected: Func<obj, U2<'T, PromiseLike<'T>>> -> Promise<'T>
    abstract catch: ?onrejected: Func<obj, unit> -> Promise<'T>

and PromiseConstructor =
    abstract createNew: executor: Func<Func<U2<'T, PromiseLike<'T>>, unit>, Func<obj, unit>, unit> -> Promise<'T>
    abstract prototype: Promise<obj> with get, set
    abstract all: values: Iterable<U2<'T, PromiseLike<'T>>> -> Promise<ResizeArray<'T>>
    abstract race: values: Iterable<U2<'T, PromiseLike<'T>>> -> Promise<'T>
    abstract reject: reason: obj -> Promise<unit>
    abstract reject: reason: obj -> Promise<'T>
    abstract resolve: value: U2<'T, PromiseLike<'T>> -> Promise<'T>
    abstract resolve: unit -> Promise<unit>
        
module ES6Extensions =
    type Object with
        member __.hasOwnProperty with get(): v: PropertyKey -> bool = failwith "JS only"
        member __.propertyIsEnumerable with get(): v: PropertyKey -> bool = failwith "JS only"

    type ObjectConstructor with
        member __.assign with get(): target: 'T * source: 'U -> obj = failwith "JS only"
        member __.assign with get(): target: 'T * source1: 'U * source2: 'V -> obj = failwith "JS only"
        member __.assign with get(): target: 'T * source1: 'U * source2: 'V * source3: 'W -> obj = failwith "JS only"
        member __.assign with get(): target: obj * [<ParamArray>] sources: obj[] -> obj = failwith "JS only"
        member __.getOwnPropertySymbols with get(): o: obj -> ResizeArray<Symbol> = failwith "JS only"
        member __.is with get(): value1: obj * value2: obj -> bool = failwith "JS only"
        member __.setPrototypeOf with get(): o: obj * proto: obj -> obj = failwith "JS only"
        member __.getOwnPropertyDescriptor with get(): o: obj * propertyKey: PropertyKey -> PropertyDescriptor = failwith "JS only"
        member __.defineProperty with get(): o: obj * propertyKey: PropertyKey * attributes: PropertyDescriptor -> obj = failwith "JS only"

    type Function with
        member __.name with get(): string = failwith "JS only"

    type NumberConstructor with
        member __.EPSILON with get(): float = failwith "JS only"
        member __.MAX_SAFE_INTEGER with get(): float = failwith "JS only"
        member __.MIN_SAFE_INTEGER with get(): float = failwith "JS only"
        member __.isFinite with get(): number: float -> bool = failwith "JS only"
        member __.isInteger with get(): number: float -> bool = failwith "JS only"
        member __.isNaN with get(): number: float -> bool = failwith "JS only"
        member __.isSafeInteger with get(): number: float -> bool = failwith "JS only"
        member __.parseFloat with get(): string: string -> float = failwith "JS only"
        member __.parseInt with get(): string: string * ?radix: float -> float = failwith "JS only"

    type Array<'T> with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<'T> = failwith "JS only"
        member __.find with get(): predicate: Func<'T, float, Array<'T>, bool> * ?thisArg: obj -> 'T = failwith "JS only"
        member __.findIndex with get(): predicate: Func<'T, bool> * ?thisArg: obj -> float = failwith "JS only"
        member __.fill with get(): value: 'T * ?start: float * ?``end``: float -> ResizeArray<'T> = failwith "JS only"
        member __.copyWithin with get(): target: float * start: float * ?``end``: float -> ResizeArray<'T> = failwith "JS only"

    type ArrayConstructor with
        member __.from with get(): arrayLike: ArrayLike<'T> * mapfn: Func<'T, float, 'U> * ?thisArg: obj -> Array<'U> = failwith "JS only"
        member __.from with get(): iterable: Iterable<'T> * mapfn: Func<'T, float, 'U> * ?thisArg: obj -> Array<'U> = failwith "JS only"
        member __.from with get(): arrayLike: ArrayLike<'T> -> Array<'T> = failwith "JS only"
        member __.from with get(): iterable: Iterable<'T> -> Array<'T> = failwith "JS only"
        member __.``of`` with get(): [<ParamArray>] items: 'T[] -> Array<'T> = failwith "JS only"

    type String with
        member __.codePointAt with get(): pos: float -> float = failwith "JS only"
        member __.includes with get(): searchString: string * ?position: float -> bool = failwith "JS only"
        member __.endsWith with get(): searchString: string * ?endPosition: float -> bool = failwith "JS only"
        member __.normalize with get(): ?form: string -> string = failwith "JS only"
        member __.repeat with get(): count: float -> string = failwith "JS only"
        member __.startsWith with get(): searchString: string * ?position: float -> bool = failwith "JS only"
        member __.``match`` with get(): matcher: obj -> RegExpMatchArray = failwith "JS only"
        member __.replace with get(): searchValue: obj * replaceValue: string -> string = failwith "JS only"
        member __.replace with get(): searchValue: obj * replacer: Func<string, ResizeArray<obj>, string> -> string = failwith "JS only"
        member __.search with get(): searcher: obj -> float = failwith "JS only"
        member __.split with get(): splitter: obj * ?limit: float -> ResizeArray<string> = failwith "JS only"
        member __.anchor with get(): name: string -> string = failwith "JS only"
        member __.big with get(): unit -> string = failwith "JS only"
        member __.blink with get(): unit -> string = failwith "JS only"
        member __.bold with get(): unit -> string = failwith "JS only"
        member __.``fixed`` with get(): unit -> string = failwith "JS only"
        member __.fontcolor with get(): color: string -> string = failwith "JS only"
        member __.fontsize with get(): size: float -> string = failwith "JS only"
        member __.fontsize with get(): size: string -> string = failwith "JS only"
        member __.italics with get(): unit -> string = failwith "JS only"
        member __.link with get(): url: string -> string = failwith "JS only"
        member __.small with get(): unit -> string = failwith "JS only"
        member __.strike with get(): unit -> string = failwith "JS only"
        member __.sub with get(): unit -> string = failwith "JS only"
        member __.sup with get(): unit -> string = failwith "JS only"

    type StringConstructor with
        member __.fromCodePoint with get(): [<ParamArray>] codePoints: float[] -> string = failwith "JS only"
        member __.raw with get(): template: TemplateStringsArray * [<ParamArray>] substitutions: obj[] -> string = failwith "JS only"

    type Math with
        member __.clz32 with get(): x: float -> float = failwith "JS only"
        member __.imul with get(): x: float * y: float -> float = failwith "JS only"
        member __.sign with get(): x: float -> float = failwith "JS only"
        member __.log10 with get(): x: float -> float = failwith "JS only"
        member __.log2 with get(): x: float -> float = failwith "JS only"
        member __.log1p with get(): x: float -> float = failwith "JS only"
        member __.expm1 with get(): x: float -> float = failwith "JS only"
        member __.cosh with get(): x: float -> float = failwith "JS only"
        member __.sinh with get(): x: float -> float = failwith "JS only"
        member __.tanh with get(): x: float -> float = failwith "JS only"
        member __.acosh with get(): x: float -> float = failwith "JS only"
        member __.asinh with get(): x: float -> float = failwith "JS only"
        member __.atanh with get(): x: float -> float = failwith "JS only"
        member __.hypot with get(): [<ParamArray>] values: float[] -> float = failwith "JS only"
        member __.trunc with get(): x: float -> float = failwith "JS only"
        member __.fround with get(): x: float -> float = failwith "JS only"
        member __.cbrt with get(): x: float -> float = failwith "JS only"

    type RegExp with
        member __.flags with get(): string = failwith "JS only"
        member __.sticky with get(): bool = failwith "JS only"
        member __.unicode with get(): bool = failwith "JS only"

    type Int8Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Int8ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Int8Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int8Array = failwith "JS only"

    type Uint8Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Uint8ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Uint8Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8Array = failwith "JS only"

    type Uint8ClampedArray with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Uint8ClampedArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Uint8ClampedArray = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8ClampedArray = failwith "JS only"

    type Int16Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Int16ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Int16Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int16Array = failwith "JS only"

    type Uint16Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Uint16ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Uint16Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint16Array = failwith "JS only"

    type Int32Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Int32ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Int32Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int32Array = failwith "JS only"

    type Uint32Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Uint32ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Uint32Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint32Array = failwith "JS only"

    type Float32Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Float32ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Float32Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float32Array = failwith "JS only"

    type Float64Array with
        member __.entries with get(): unit -> IterableIterator<obj> = failwith "JS only"
        member __.keys with get(): unit -> IterableIterator<float> = failwith "JS only"
        member __.values with get(): unit -> IterableIterator<float> = failwith "JS only"

    type Float64ArrayConstructor with
        member __.createNew with get(): elements: Iterable<float> -> Float64Array = failwith "JS only"
        member __.from with get(): arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float64Array = failwith "JS only"

module Globals =
    let [<Global>] NaN: float = failwith "JS only"
    let [<Global>] Infinity: float = failwith "JS only"
    let [<Global>] Object: ObjectConstructor = failwith "JS only"
    let [<Global>] Function: FunctionConstructor = failwith "JS only"
    let [<Global>] String: StringConstructor = failwith "JS only"
    let [<Global>] Boolean: BooleanConstructor = failwith "JS only"
    let [<Global>] Number: NumberConstructor = failwith "JS only"
    let [<Global>] Math: Math = failwith "JS only"
    let [<Global>] Date: DateConstructor = failwith "JS only"
    let [<Global>] RegExp: RegExpConstructor = failwith "JS only"
    let [<Global>] Error: ErrorConstructor = failwith "JS only"
    let [<Global>] EvalError: EvalErrorConstructor = failwith "JS only"
    let [<Global>] RangeError: RangeErrorConstructor = failwith "JS only"
    let [<Global>] ReferenceError: ReferenceErrorConstructor = failwith "JS only"
    let [<Global>] SyntaxError: SyntaxErrorConstructor = failwith "JS only"
    let [<Global>] TypeError: TypeErrorConstructor = failwith "JS only"
    let [<Global>] URIError: URIErrorConstructor = failwith "JS only"
    let [<Global>] JSON: JSON = failwith "JS only"
    let [<Global>] Array: ArrayConstructor = failwith "JS only"
    let [<Global>] ArrayBuffer: ArrayBufferConstructor = failwith "JS only"
    let [<Global>] DataView: DataViewConstructor = failwith "JS only"
    let [<Global>] Int8Array: Int8ArrayConstructor = failwith "JS only"
    let [<Global>] Uint8Array: Uint8ArrayConstructor = failwith "JS only"
    let [<Global>] Uint8ClampedArray: Uint8ClampedArrayConstructor = failwith "JS only"
    let [<Global>] Int16Array: Int16ArrayConstructor = failwith "JS only"
    let [<Global>] Uint16Array: Uint16ArrayConstructor = failwith "JS only"
    let [<Global>] Int32Array: Int32ArrayConstructor = failwith "JS only"
    let [<Global>] Uint32Array: Uint32ArrayConstructor = failwith "JS only"
    let [<Global>] Float32Array: Float32ArrayConstructor = failwith "JS only"
    let [<Global>] Float64Array: Float64ArrayConstructor = failwith "JS only"
    let [<Global>] Symbol: SymbolConstructor = failwith "JS only"
    let [<Global>] GeneratorFunction: GeneratorFunctionConstructor = failwith "JS only"
    let [<Global>] Map: MapConstructor = failwith "JS only"
    let [<Global>] WeakMap: WeakMapConstructor = failwith "JS only"
    let [<Global>] Set: SetConstructor = failwith "JS only"
    let [<Global>] WeakSet: WeakSetConstructor = failwith "JS only"
    let [<Global>] Proxy: ProxyConstructor = failwith "JS only"
    let [<Global>] Promise: PromiseConstructor = failwith "JS only"

module Reflect =
    type Globals =
        abstract apply: target: Function * thisArgument: obj * argumentsList: ArrayLike<obj> -> obj
        abstract construct: target: Function * argumentsList: ArrayLike<obj> * ?newTarget: obj -> obj
        abstract defineProperty: target: obj * propertyKey: PropertyKey * attributes: PropertyDescriptor -> bool
        abstract deleteProperty: target: obj * propertyKey: PropertyKey -> bool
        abstract enumerate: target: obj -> IterableIterator<obj>
        abstract get: target: obj * propertyKey: PropertyKey * ?receiver: obj -> obj
        abstract getOwnPropertyDescriptor: target: obj * propertyKey: PropertyKey -> PropertyDescriptor
        abstract getPrototypeOf: target: obj -> obj
        abstract has: target: obj * propertyKey: string -> bool
        abstract has: target: obj * propertyKey: Symbol -> bool
        abstract isExtensible: target: obj -> bool
        abstract ownKeys: target: obj -> Array<PropertyKey>
        abstract preventExtensions: target: obj -> bool
        abstract set: target: obj * propertyKey: PropertyKey * value: obj * ?receiver: obj -> bool
        abstract setPrototypeOf: target: obj * proto: obj -> bool

    [<Import("Reflect")>]
    let Globals: Globals = failwith "JS only"


