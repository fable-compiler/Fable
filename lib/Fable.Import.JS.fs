namespace Fable.Import.JS
open System
open Fable.Core

type PropertyDescriptor =
    abstract configurable: bool option with get, set
    abstract enumerable: bool option with get, set
    abstract value: obj option with get, set
    abstract writable: bool option with get, set
    abstract get: unit -> obj
    abstract set: v: obj -> unit

and PropertyDescriptorMap =
    interface end
    
and PropertyKey =
    U3<string, float, Symbol>

and Object =
    abstract ``constructor``: Function with get, set
    abstract toString: unit -> string
    abstract toLocaleString: unit -> string
    abstract valueOf: unit -> obj
    abstract hasOwnProperty: v: string -> bool
    abstract isPrototypeOf: v: obj -> bool
    abstract propertyIsEnumerable: v: string -> bool
    abstract hasOwnProperty: v: PropertyKey -> bool
    abstract propertyIsEnumerable: v: PropertyKey -> bool

and ObjectConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?value: obj -> obj
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
    abstract assign: target: 'T * source: 'U -> obj
    abstract assign: target: 'T * source1: 'U * source2: 'V -> obj
    abstract assign: target: 'T * source1: 'U * source2: 'V * source3: 'W -> obj
    abstract assign: target: obj * [<ParamArray>] sources: obj[] -> obj
    abstract getOwnPropertySymbols: o: obj -> ResizeArray<Symbol>
    abstract is: value1: obj * value2: obj -> bool
    abstract setPrototypeOf: o: obj * proto: obj -> obj
    abstract getOwnPropertyDescriptor: o: obj * propertyKey: PropertyKey -> PropertyDescriptor
    abstract defineProperty: o: obj * propertyKey: PropertyKey * attributes: PropertyDescriptor -> obj

and Function =
    abstract prototype: obj with get, set
    abstract length: float with get, set
    abstract arguments: obj with get, set
    abstract caller: Function with get, set
    abstract apply: thisArg: obj * ?argArray: obj -> obj
    abstract call: thisArg: obj * [<ParamArray>] argArray: obj[] -> obj
    abstract bind: thisArg: obj * [<ParamArray>] argArray: obj[] -> obj
    abstract name: string

and FunctionConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: [<ParamArray>] args: string[] -> Function
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
    abstract codePointAt: pos: float -> float
    abstract includes: searchString: string * ?position: float -> bool
    abstract endsWith: searchString: string * ?endPosition: float -> bool
    abstract normalize: ?form: string -> string
    abstract repeat: count: float -> string
    abstract startsWith: searchString: string * ?position: float -> bool
    abstract ``match``: matcher: obj -> RegExpMatchArray
    abstract replace: searchValue: obj * replaceValue: string -> string
    abstract replace: searchValue: obj * replacer: Func<string, ResizeArray<obj>, string> -> string
    abstract search: searcher: obj -> float
    abstract split: splitter: obj * ?limit: float -> ResizeArray<string>
    abstract anchor: name: string -> string
    abstract big: unit -> string
    abstract blink: unit -> string
    abstract bold: unit -> string
    abstract ``fixed``: unit -> string
    abstract fontcolor: color: string -> string
    abstract fontsize: size: float -> string
    abstract fontsize: size: string -> string
    abstract italics: unit -> string
    abstract link: url: string -> string
    abstract small: unit -> string
    abstract strike: unit -> string
    abstract sub: unit -> string
    abstract sup: unit -> string

and StringConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?value: obj -> String
    abstract prototype: String with get, set
    abstract fromCharCode: [<ParamArray>] codes: float[] -> string
    abstract fromCodePoint: [<ParamArray>] codePoints: float[] -> string
    abstract raw: template: TemplateStringsArray * [<ParamArray>] substitutions: obj[] -> string

and Boolean =
    abstract valueOf: unit -> bool

and BooleanConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?value: obj -> Boolean
    abstract prototype: Boolean with get, set

and Number =
    abstract toString: ?radix: float -> string
    abstract toFixed: ?fractionDigits: float -> string
    abstract toExponential: ?fractionDigits: float -> string
    abstract toPrecision: ?precision: float -> string
    abstract valueOf: unit -> float

and NumberConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?value: obj -> Number
    abstract prototype: Number with get, set
    abstract MAX_VALUE: float with get, set
    abstract MIN_VALUE: float with get, set
    abstract NaN: float with get, set
    abstract NEGATIVE_INFINITY: float with get, set
    abstract POSITIVE_INFINITY: float with get, set
    abstract EPSILON: float
    abstract MAX_SAFE_INTEGER: float
    abstract MIN_SAFE_INTEGER: float
    abstract isFinite: number: float -> bool
    abstract isInteger: number: float -> bool
    abstract isNaN: number: float -> bool
    abstract isSafeInteger: number: float -> bool
    abstract parseFloat: string: string -> float
    abstract parseInt: string: string * ?radix: float -> float

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
    [<Emit("new $0($1...)")>] abstract createNew: unit -> DateTime
    [<Emit("new $0($1...)")>] abstract createNew: value: float -> DateTime
    [<Emit("new $0($1...)")>] abstract createNew: value: string -> DateTime
    [<Emit("new $0($1...)")>] abstract createNew: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> DateTime
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
    abstract flags: string
    abstract sticky: bool
    abstract unicode: bool

and RegExpConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: pattern: string * ?flags: string -> RegExp
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
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> Error
    abstract prototype: Error with get, set

and EvalError =
    inherit Error

and EvalErrorConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> EvalError
    abstract prototype: EvalError with get, set

and RangeError =
    inherit Error

and RangeErrorConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> RangeError
    abstract prototype: RangeError with get, set

and ReferenceError =
    inherit Error

and ReferenceErrorConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> ReferenceError
    abstract prototype: ReferenceError with get, set

and SyntaxError =
    inherit Error

and SyntaxErrorConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> SyntaxError
    abstract prototype: SyntaxError with get, set

and TypeError =
    inherit Error

and TypeErrorConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> TypeError
    abstract prototype: TypeError with get, set

and URIError =
    inherit Error

and URIErrorConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?message: string -> URIError
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
    abstract entries: unit -> IterableIterator<obj>
    abstract keys: unit -> IterableIterator<float>
    abstract values: unit -> IterableIterator<'T>
    abstract find: predicate: Func<'T, float, Array<'T>, bool> * ?thisArg: obj -> 'T
    abstract findIndex: predicate: Func<'T, bool> * ?thisArg: obj -> float
    abstract fill: value: 'T * ?start: float * ?``end``: float -> ResizeArray<'T>
    abstract copyWithin: target: float * start: float * ?``end``: float -> ResizeArray<'T>

and ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: ?arrayLength: float -> ResizeArray<obj>
    [<Emit("new $0($1...)")>] abstract createNew: arrayLength: float -> ResizeArray<'T>
    [<Emit("new $0($1...)")>] abstract createNew: [<ParamArray>] items: 'T[] -> ResizeArray<'T>
    abstract prototype: Array<obj> with get, set
    abstract isArray: arg: obj -> obj
    abstract from: arrayLike: ArrayLike<'T> * mapfn: Func<'T, float, 'U> * ?thisArg: obj -> Array<'U>
    abstract from: iterable: Iterable<'T> * mapfn: Func<'T, float, 'U> * ?thisArg: obj -> Array<'U>
    abstract from: arrayLike: ArrayLike<'T> -> Array<'T>
    abstract from: iterable: Iterable<'T> -> Array<'T>
    abstract ``of``: [<ParamArray>] items: 'T[] -> Array<'T>

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
    [<Emit("new $0($1...)")>] abstract createNew: byteLength: float -> ArrayBuffer
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
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?byteLength: float -> DataView

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Int8ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Int8Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Int8Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int8Array
    abstract prototype: Int8Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Int8Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int8Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Int8Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int8Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Uint8ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Uint8Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Uint8Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint8Array
    abstract prototype: Uint8Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint8Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Uint8Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Uint8ClampedArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Uint8ClampedArray
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Uint8ClampedArray
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint8ClampedArray
    abstract prototype: Uint8ClampedArray with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint8ClampedArray
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8ClampedArray
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Uint8ClampedArray
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint8ClampedArray

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Int16ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Int16Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Int16Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int16Array
    abstract prototype: Int16Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Int16Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int16Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Int16Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int16Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Uint16ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Uint16Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Uint16Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint16Array
    abstract prototype: Uint16Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint16Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint16Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Uint16Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint16Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Int32ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Int32Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Int32Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int32Array
    abstract prototype: Int32Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Int32Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int32Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Int32Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Int32Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Uint32ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Uint32Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Uint32Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint32Array
    abstract prototype: Uint32Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Uint32Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint32Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Uint32Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Uint32Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Float32ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Float32Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Float32Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Float32Array
    abstract prototype: Float32Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Float32Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float32Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Float32Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float32Array

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
    abstract entries: unit -> IterableIterator<int*float>
    abstract keys: unit -> IterableIterator<int>
    abstract values: unit -> IterableIterator<float>

and Float64ArrayConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: length: float -> Float64Array
    [<Emit("new $0($1...)")>] abstract createNew: array: ArrayLike<float> -> Float64Array
    [<Emit("new $0($1...)")>] abstract createNew: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Float64Array
    abstract prototype: Float64Array with get, set
    abstract BYTES_PER_ELEMENT: float with get, set
    abstract ``of``: [<ParamArray>] items: float[] -> Float64Array
    abstract from: arrayLike: ArrayLike<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float64Array
    [<Emit("new $0($1...)")>] abstract createNew: elements: Iterable<float> -> Float64Array
    abstract from: arrayLike: Iterable<float> * ?mapfn: Func<float, float, float> * ?thisArg: obj -> Float64Array

// ES6
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
    [<Emit("new $0($1...)")>] abstract createNew: [<ParamArray>] args: string[] -> GeneratorFunction
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
    [<Emit("new $0($1...)")>] abstract createNew: unit -> Map<obj, obj>
    [<Emit("new $0($1...)")>] abstract createNew: unit -> Map<'K, 'V>
    [<Emit("new $0($1...)")>] abstract createNew: iterable: Iterable<obj> -> Map<'K, 'V>
    abstract prototype: Map<obj, obj> with get, set

and WeakMap<'K, 'V> =
    abstract clear: unit -> unit
    abstract delete: key: 'K -> bool
    abstract get: key: 'K -> 'V
    abstract has: key: 'K -> bool
    abstract set: key: 'K * ?value: 'V -> WeakMap<'K, 'V>

and WeakMapConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: unit -> WeakMap<obj, obj>
    [<Emit("new $0($1...)")>] abstract createNew: unit -> WeakMap<'K, 'V>
    [<Emit("new $0($1...)")>] abstract createNew: iterable: Iterable<obj> -> WeakMap<'K, 'V>
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
    [<Emit("new $0($1...)")>] abstract createNew: unit -> Set<obj>
    [<Emit("new $0($1...)")>] abstract createNew: unit -> Set<'T>
    [<Emit("new $0($1...)")>] abstract createNew: iterable: Iterable<'T> -> Set<'T>
    abstract prototype: Set<obj> with get, set

and WeakSet<'T> =
    abstract add: value: 'T -> WeakSet<'T>
    abstract clear: unit -> unit
    abstract delete: value: 'T -> bool
    abstract has: value: 'T -> bool

and WeakSetConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: unit -> WeakSet<obj>
    [<Emit("new $0($1...)")>] abstract createNew: unit -> WeakSet<'T>
    [<Emit("new $0($1...)")>] abstract createNew: iterable: Iterable<'T> -> WeakSet<'T>
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
    [<Emit("new $0($1...)")>] abstract createNew: target: 'T * handler: ProxyHandler<'T> -> 'T
    abstract revocable: target: 'T * handler: ProxyHandler<'T> -> obj

and Promise<'T> =
    abstract ``then``: ?onfulfilled: Func<'T, U2<'TResult, PromiseLike<'TResult>>> * ?onrejected: Func<obj, U2<'TResult, PromiseLike<'TResult>>> -> Promise<'TResult>
    abstract ``then``: ?onfulfilled: Func<'T, U2<'TResult, PromiseLike<'TResult>>> * ?onrejected: Func<obj, unit> -> Promise<'TResult>
    abstract catch: ?onrejected: Func<obj, U2<'T, PromiseLike<'T>>> -> Promise<'T>
    abstract catch: ?onrejected: Func<obj, unit> -> Promise<'T>

and PromiseConstructor =
    [<Emit("new $0($1...)")>] abstract createNew: executor: Func<Func<U2<'T, PromiseLike<'T>>, unit>, Func<obj, unit>, unit> -> Promise<'T>
    abstract prototype: Promise<obj> with get, set
    abstract all: values: Iterable<U2<'T, PromiseLike<'T>>> -> Promise<ResizeArray<'T>>
    abstract race: values: Iterable<U2<'T, PromiseLike<'T>>> -> Promise<'T>
    abstract reject: reason: obj -> Promise<unit>
    abstract reject: reason: obj -> Promise<'T>
    abstract resolve: value: U2<'T, PromiseLike<'T>> -> Promise<'T>
    abstract resolve: unit -> Promise<unit>

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


