namespace Fable.Import
open System
open Fable.Core

module JS =
    type [<AllowNullLiteral>] PropertyDescriptor =
        abstract configurable: bool option with get, set
        abstract enumerable: bool option with get, set
        abstract value: obj option with get, set
        abstract writable: bool option with get, set
        abstract get: unit -> obj
        abstract set: v: obj -> unit

    and [<AllowNullLiteral>] PropertyDescriptorMap =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: s: string -> PropertyDescriptor with get, set

    and [<AllowNullLiteral>] Object =
        abstract ``constructor``: Function with get, set
        abstract toString: unit -> string
        abstract toLocaleString: unit -> string
        abstract valueOf: unit -> obj
        abstract hasOwnProperty: v: string -> bool
        abstract isPrototypeOf: v: obj -> bool
        abstract propertyIsEnumerable: v: string -> bool
        abstract hasOwnProperty: v: obj -> bool
        abstract propertyIsEnumerable: v: obj -> bool

    and [<AllowNullLiteral>] ObjectConstructor =
        abstract prototype: obj with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?value: obj -> obj
        [<Emit("$0($1...)")>] abstract Invoke: unit -> obj
        [<Emit("$0($1...)")>] abstract Invoke: value: obj -> obj
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
        abstract getOwnPropertyDescriptor: o: obj * propertyKey: obj -> PropertyDescriptor
        abstract defineProperty: o: obj * propertyKey: obj * attributes: PropertyDescriptor -> obj

    and [<AllowNullLiteral>] Function =
        abstract prototype: obj with get, set
        abstract length: float with get, set
        abstract arguments: obj with get, set
        abstract caller: Function with get, set
        abstract name: string with get, set
        abstract apply: thisArg: obj * ?argArray: obj -> obj
        abstract call: thisArg: obj * [<ParamArray>] argArray: obj[] -> obj
        abstract bind: thisArg: obj * [<ParamArray>] argArray: obj[] -> obj
        [<Emit("$0[Symbol.hasInstance]($1...)")>] abstract ``[Symbol.hasInstance]``: value: obj -> bool
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: obj[] -> obj

    and [<AllowNullLiteral>] FunctionConstructor =
        abstract prototype: Function with get, set
        [<Emit("new $0($1...)")>] abstract Create: [<ParamArray>] args: string[] -> Function
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: string[] -> Function

    and [<AllowNullLiteral>] IArguments =
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> obj with get, set
        abstract length: float with get, set
        abstract callee: Function with get, set
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<obj>

    and [<AllowNullLiteral>] String =
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> string with get, set
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
        abstract replace: searchValue: string * replacer: (string->obj->string) -> string
        abstract replace: searchValue: RegExp * replaceValue: string -> string
        abstract replace: searchValue: RegExp * replacer: (string->obj->string) -> string
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
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<string>
        abstract codePointAt: pos: float -> float
        abstract includes: searchString: string * ?position: float -> bool
        abstract endsWith: searchString: string * ?endPosition: float -> bool
        abstract normalize: ?form: string -> string
        abstract repeat: count: float -> string
        abstract startsWith: searchString: string * ?position: float -> bool
        abstract ``match``: matcher: obj -> RegExpMatchArray
        abstract replace: searchValue: obj * replaceValue: string -> string
        abstract replace: searchValue: obj * replacer: (string->obj->string) -> string
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

    and [<AllowNullLiteral>] StringConstructor =
        abstract prototype: String with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?value: obj -> String
        [<Emit("$0($1...)")>] abstract Invoke: ?value: obj -> string
        abstract fromCharCode: [<ParamArray>] codes: float[] -> string
        abstract fromCodePoint: [<ParamArray>] codePoints: float[] -> string
        abstract raw: template: TemplateStringsArray * [<ParamArray>] substitutions: obj[] -> string

    and [<AllowNullLiteral>] Boolean =
        abstract valueOf: unit -> bool

    and [<AllowNullLiteral>] BooleanConstructor =
        abstract prototype: Boolean with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?value: obj -> Boolean
        [<Emit("$0($1...)")>] abstract Invoke: ?value: obj -> bool

    and [<AllowNullLiteral>] Number =
        abstract toString: ?radix: float -> string
        abstract toFixed: ?fractionDigits: float -> string
        abstract toExponential: ?fractionDigits: float -> string
        abstract toPrecision: ?precision: float -> string
        abstract valueOf: unit -> float

    and [<AllowNullLiteral>] NumberConstructor =
        abstract prototype: Number with get, set
        abstract MAX_VALUE: float with get, set
        abstract MIN_VALUE: float with get, set
        abstract NaN: float with get, set
        abstract NEGATIVE_INFINITY: float with get, set
        abstract POSITIVE_INFINITY: float with get, set
        abstract EPSILON: float with get, set
        abstract MAX_SAFE_INTEGER: float with get, set
        abstract MIN_SAFE_INTEGER: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?value: obj -> Number
        [<Emit("$0($1...)")>] abstract Invoke: ?value: obj -> float
        abstract isFinite: number: float -> bool
        abstract isInteger: number: float -> bool
        abstract isNaN: number: float -> bool
        abstract isSafeInteger: number: float -> bool
        abstract parseFloat: string: string -> float
        abstract parseInt: string: string * ?radix: float -> float

    and [<AllowNullLiteral>] TemplateStringsArray =
        inherit Array<string>
        abstract raw: ResizeArray<string> with get, set

    and [<AllowNullLiteral>] Math =
        abstract E: float with get, set
        abstract LN10: float with get, set
        abstract LN2: float with get, set
        abstract LOG2E: float with get, set
        abstract LOG10E: float with get, set
        abstract PI: float with get, set
        abstract SQRT1_2: float with get, set
        abstract SQRT2: float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
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
        [<Emit("$0[Symbol.toPrimitive]($1...)")>] abstract ``[Symbol.toPrimitive]_default``: unit -> string
        [<Emit("$0[Symbol.toPrimitive]($1...)")>] abstract ``[Symbol.toPrimitive]_string``: unit -> string
        [<Emit("$0[Symbol.toPrimitive]($1...)")>] abstract ``[Symbol.toPrimitive]_number``: unit -> float
        [<Emit("$0[Symbol.toPrimitive]($1...)")>] abstract ``[Symbol.toPrimitive]``: hint: string -> U2<string, float>

    and [<AllowNullLiteral>] DateConstructor =
        abstract prototype: DateTime with get, set
        [<Emit("new $0($1...)")>] abstract Create: unit -> DateTime
        [<Emit("new $0($1...)")>] abstract Create: value: float -> DateTime
        [<Emit("new $0($1...)")>] abstract Create: value: string -> DateTime
        [<Emit("new $0($1...)")>] abstract Create: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> DateTime
        [<Emit("$0($1...)")>] abstract Invoke: unit -> string
        abstract parse: s: string -> float
        abstract UTC: year: float * month: float * ?date: float * ?hours: float * ?minutes: float * ?seconds: float * ?ms: float -> float
        abstract now: unit -> float

    and [<AllowNullLiteral>] RegExpMatchArray =
        inherit Array<string>
        abstract index: float option with get, set
        abstract input: string option with get, set

    and [<AllowNullLiteral>] RegExpExecArray =
        inherit Array<string>
        abstract index: float with get, set
        abstract input: string with get, set

    and [<AllowNullLiteral>] RegExp =
        abstract source: string with get, set
        abstract ``global``: bool with get, set
        abstract ignoreCase: bool with get, set
        abstract multiline: bool with get, set
        abstract lastIndex: float with get, set
        abstract flags: string with get, set
        abstract sticky: bool with get, set
        abstract unicode: bool with get, set
        abstract exec: string: string -> RegExpExecArray
        abstract test: string: string -> bool
        abstract compile: unit -> RegExp
        [<Emit("$0[Symbol.match]($1...)")>] abstract ``[Symbol.match]``: string: string -> RegExpMatchArray
        [<Emit("$0[Symbol.replace]($1...)")>] abstract ``[Symbol.replace]``: string: string * replaceValue: string -> string
        [<Emit("$0[Symbol.replace]($1...)")>] abstract ``[Symbol.replace]``: string: string * replacer: (string->obj->string) -> string
        [<Emit("$0[Symbol.search]($1...)")>] abstract ``[Symbol.search]``: string: string -> float
        [<Emit("$0[Symbol.split]($1...)")>] abstract ``[Symbol.split]``: string: string * ?limit: float -> ResizeArray<string>

    and [<AllowNullLiteral>] RegExpConstructor =
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
        [<Emit("new $0($1...)")>] abstract Create: pattern: string * ?flags: string -> RegExp
        [<Emit("$0($1...)")>] abstract Invoke: pattern: string * ?flags: string -> RegExp
        [<Emit("$0[Symbol.species]($1...)")>] abstract ``[Symbol.species]``: unit -> RegExpConstructor

    and [<AllowNullLiteral>] Error =
        abstract name: string with get, set
        abstract message: string with get, set

    and [<AllowNullLiteral>] ErrorConstructor =
        abstract prototype: Error with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> Error
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> Error

    and [<AllowNullLiteral>] EvalError =
        inherit Error


    and [<AllowNullLiteral>] EvalErrorConstructor =
        abstract prototype: EvalError with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> EvalError
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> EvalError

    and [<AllowNullLiteral>] RangeError =
        inherit Error


    and [<AllowNullLiteral>] RangeErrorConstructor =
        abstract prototype: RangeError with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> RangeError
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> RangeError

    and [<AllowNullLiteral>] ReferenceError =
        inherit Error


    and [<AllowNullLiteral>] ReferenceErrorConstructor =
        abstract prototype: ReferenceError with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> ReferenceError
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> ReferenceError

    and [<AllowNullLiteral>] SyntaxError =
        inherit Error


    and [<AllowNullLiteral>] SyntaxErrorConstructor =
        abstract prototype: SyntaxError with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> SyntaxError
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> SyntaxError

    and [<AllowNullLiteral>] TypeError =
        inherit Error


    and [<AllowNullLiteral>] TypeErrorConstructor =
        abstract prototype: TypeError with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> TypeError
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> TypeError

    and [<AllowNullLiteral>] URIError =
        inherit Error


    and [<AllowNullLiteral>] URIErrorConstructor =
        abstract prototype: URIError with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?message: string -> URIError
        [<Emit("$0($1...)")>] abstract Invoke: ?message: string -> URIError

    and [<AllowNullLiteral>] JSON =
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract parse: text: string * ?reviver: (obj->obj->obj) -> obj
        abstract stringify: value: obj -> string
        abstract stringify: value: obj * replacer: (string->obj->obj) -> string
        abstract stringify: value: obj * replacer: (string->obj->obj) * space: obj -> string

    and [<AllowNullLiteral>] ReadonlyArray<'T> =
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: n: int -> 'T with get, set
        abstract toString: unit -> string
        abstract toLocaleString: unit -> string
        abstract concat: [<ParamArray>] items: 'U[] -> ResizeArray<'T>
        abstract concat: [<ParamArray>] items: 'T[] -> ResizeArray<'T>
        abstract join: ?separator: string -> string
        abstract slice: ?start: float * ?``end``: float -> ResizeArray<'T>
        abstract indexOf: searchElement: 'T * ?fromIndex: float -> float
        abstract lastIndexOf: searchElement: 'T * ?fromIndex: float -> float
        abstract every: callbackfn: ('T->float->ReadonlyArray<'T>->bool) * ?thisArg: obj -> bool
        abstract some: callbackfn: ('T->float->ReadonlyArray<'T>->bool) * ?thisArg: obj -> bool
        abstract forEach: callbackfn: ('T->float->ReadonlyArray<'T>->unit) * ?thisArg: obj -> unit
        abstract map: callbackfn: ('T->float->ReadonlyArray<'T>->'U) * ?thisArg: obj -> ResizeArray<'U>
        abstract filter: callbackfn: ('T->float->ReadonlyArray<'T>->bool) * ?thisArg: obj -> ResizeArray<'T>
        abstract reduce: callbackfn: ('T->'T->float->ReadonlyArray<'T>->'T) * ?initialValue: 'T -> 'T
        abstract reduce: callbackfn: ('U->'T->float->ReadonlyArray<'T>->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: ('T->'T->float->ReadonlyArray<'T>->'T) * ?initialValue: 'T -> 'T
        abstract reduceRight: callbackfn: ('U->'T->float->ReadonlyArray<'T>->'U) * initialValue: 'U -> 'U

    and [<AllowNullLiteral>] Array<'T> =
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: n: int -> 'T with get, set
        abstract toString: unit -> string
        abstract toLocaleString: unit -> string
        abstract push: [<ParamArray>] items: 'T[] -> float
        abstract pop: unit -> 'T
        abstract concat: [<ParamArray>] items: ResizeArray<'T>[] -> ResizeArray<'T>
        abstract join: ?separator: string -> string
        abstract reverse: unit -> ResizeArray<'T>
        abstract shift: unit -> 'T
        abstract slice: ?start: float * ?``end``: float -> ResizeArray<'T>
        abstract sort: ?compareFn: ('T->'T->float) -> ResizeArray<'T>
        abstract splice: start: float -> ResizeArray<'T>
        abstract splice: start: float * deleteCount: float * [<ParamArray>] items: 'T[] -> ResizeArray<'T>
        abstract unshift: [<ParamArray>] items: 'T[] -> float
        abstract indexOf: searchElement: 'T * ?fromIndex: float -> float
        abstract lastIndexOf: searchElement: 'T * ?fromIndex: float -> float
        abstract every: callbackfn: ('T->float->ResizeArray<'T>->bool) * ?thisArg: obj -> bool
        abstract some: callbackfn: ('T->float->ResizeArray<'T>->bool) * ?thisArg: obj -> bool
        abstract forEach: callbackfn: ('T->float->ResizeArray<'T>->unit) * ?thisArg: obj -> unit
        abstract map: callbackfn: ('T->float->ResizeArray<'T>->'U) * ?thisArg: obj -> ResizeArray<'U>
        abstract filter: callbackfn: ('T->float->ResizeArray<'T>->bool) * ?thisArg: obj -> ResizeArray<'T>
        abstract reduce: callbackfn: ('T->'T->float->ResizeArray<'T>->'T) * ?initialValue: 'T -> 'T
        abstract reduce: callbackfn: ('U->'T->float->ResizeArray<'T>->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: ('T->'T->float->ResizeArray<'T>->'T) * ?initialValue: 'T -> 'T
        abstract reduceRight: callbackfn: ('U->'T->float->ResizeArray<'T>->'U) * initialValue: 'U -> 'U
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<'T>
        [<Emit("$0[Symbol.unscopables]($1...)")>] abstract ``[Symbol.unscopables]``: unit -> obj
        abstract entries: unit -> IterableIterator<float * 'T>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<'T>
        abstract find: predicate: ('T->float->ResizeArray<'T>->bool) * ?thisArg: obj -> 'T
        abstract findIndex: predicate: ('T->bool) * ?thisArg: obj -> float
        abstract fill: value: 'T * ?start: float * ?``end``: float -> ResizeArray<'T>
        abstract copyWithin: target: float * start: float * ?``end``: float -> ResizeArray<'T>

    and [<AllowNullLiteral>] ArrayConstructor =
        abstract prototype: ResizeArray<obj> with get, set
        [<Emit("new $0($1...)")>] abstract Create: ?arrayLength: float -> ResizeArray<obj>
        [<Emit("new $0($1...)")>] abstract Create: arrayLength: float -> ResizeArray<'T>
        [<Emit("new $0($1...)")>] abstract Create: [<ParamArray>] items: 'T[] -> ResizeArray<'T>
        [<Emit("$0($1...)")>] abstract Invoke: ?arrayLength: float -> ResizeArray<obj>
        [<Emit("$0($1...)")>] abstract Invoke: arrayLength: float -> ResizeArray<'T>
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] items: 'T[] -> ResizeArray<'T>
        abstract isArray: arg: obj -> bool
        abstract from: arrayLike: ArrayLike<'T> * mapfn: ('T->float->'U) * ?thisArg: obj -> ResizeArray<'U>
        abstract from: iterable: Iterable<'T> * mapfn: ('T->float->'U) * ?thisArg: obj -> ResizeArray<'U>
        abstract from: arrayLike: ArrayLike<'T> -> ResizeArray<'T>
        abstract from: iterable: Iterable<'T> -> ResizeArray<'T>
        abstract ``of``: [<ParamArray>] items: 'T[] -> ResizeArray<'T>

    and [<AllowNullLiteral>] TypedPropertyDescriptor<'T> =
        abstract enumerable: bool option with get, set
        abstract configurable: bool option with get, set
        abstract writable: bool option with get, set
        abstract value: 'T option with get, set
        abstract get: (unit->'T) option with get, set
        abstract set: ('T->unit) option with get, set

    and PromiseConstructorLike =
        obj

    and [<AllowNullLiteral>] PromiseLike<'T> =
        abstract ``then``: ?onfulfilled: ('T->'TResult) * ?onrejected: (obj->'TResult) -> PromiseLike<'TResult>

    and [<AllowNullLiteral>] ArrayLike<'T> =
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: n: int -> 'T with get, set

    and [<AllowNullLiteral>] ArrayBuffer =
        abstract byteLength: float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract slice: ``begin``: float * ?``end``: float -> ArrayBuffer

    and [<AllowNullLiteral>] ArrayBufferConstructor =
        abstract prototype: ArrayBuffer with get, set
        [<Emit("new $0($1...)")>] abstract Create: byteLength: float -> ArrayBuffer
        abstract isView: arg: obj -> bool

    and [<AllowNullLiteral>] ArrayBufferView =
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set

    and [<AllowNullLiteral>] DataView =
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
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

    and [<AllowNullLiteral>] DataViewConstructor =
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?byteLength: float -> DataView

    and [<AllowNullLiteral>] Int8Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Int8Array
        abstract every: callbackfn: (float->float->Int8Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Int8Array
        abstract filter: callbackfn: (float->float->Int8Array->bool) * ?thisArg: obj -> Int8Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Int8Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Int8Array->float) * ?thisArg: obj -> Int8Array
        abstract reduce: callbackfn: (float->float->float->Int8Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Int8Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Int8Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Int8Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Int8Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Int8Array
        abstract some: callbackfn: (float->float->Int8Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Int8Array
        abstract subarray: ``begin``: float * ?``end``: float -> Int8Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Int8ArrayConstructor =
        abstract prototype: Int8Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Int8Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Int8Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int8Array
        abstract ``of``: [<ParamArray>] items: float[] -> Int8Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Int8Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Int8Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Int8Array

    and [<AllowNullLiteral>] Uint8Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Uint8Array
        abstract every: callbackfn: (float->float->Uint8Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Uint8Array
        abstract filter: callbackfn: (float->float->Uint8Array->bool) * ?thisArg: obj -> Uint8Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Uint8Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Uint8Array->float) * ?thisArg: obj -> Uint8Array
        abstract reduce: callbackfn: (float->float->float->Uint8Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Uint8Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Uint8Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Uint8Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Uint8Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Uint8Array
        abstract some: callbackfn: (float->float->Uint8Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Uint8Array
        abstract subarray: ``begin``: float * ?``end``: float -> Uint8Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Uint8ArrayConstructor =
        abstract prototype: Uint8Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Uint8Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Uint8Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint8Array
        abstract ``of``: [<ParamArray>] items: float[] -> Uint8Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint8Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Uint8Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint8Array

    and [<AllowNullLiteral>] Uint8ClampedArray =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Uint8ClampedArray
        abstract every: callbackfn: (float->float->Uint8ClampedArray->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Uint8ClampedArray
        abstract filter: callbackfn: (float->float->Uint8ClampedArray->bool) * ?thisArg: obj -> Uint8ClampedArray
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Uint8ClampedArray->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Uint8ClampedArray->float) * ?thisArg: obj -> Uint8ClampedArray
        abstract reduce: callbackfn: (float->float->float->Uint8ClampedArray->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Uint8ClampedArray->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Uint8ClampedArray->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Uint8ClampedArray->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Uint8ClampedArray
        abstract set: index: float * value: float -> unit
        abstract set: array: Uint8ClampedArray * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Uint8ClampedArray
        abstract some: callbackfn: (float->float->Uint8ClampedArray->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Uint8ClampedArray
        abstract subarray: ``begin``: float * ?``end``: float -> Uint8ClampedArray
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Uint8ClampedArrayConstructor =
        abstract prototype: Uint8ClampedArray with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Uint8ClampedArray
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Uint8ClampedArray
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint8ClampedArray
        abstract ``of``: [<ParamArray>] items: float[] -> Uint8ClampedArray
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint8ClampedArray
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Uint8ClampedArray
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint8ClampedArray

    and [<AllowNullLiteral>] Int16Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Int16Array
        abstract every: callbackfn: (float->float->Int16Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Int16Array
        abstract filter: callbackfn: (float->float->Int16Array->bool) * ?thisArg: obj -> Int16Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Int16Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Int16Array->float) * ?thisArg: obj -> Int16Array
        abstract reduce: callbackfn: (float->float->float->Int16Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Int16Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Int16Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Int16Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Int16Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Int16Array
        abstract some: callbackfn: (float->float->Int16Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Int16Array
        abstract subarray: ``begin``: float * ?``end``: float -> Int16Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Int16ArrayConstructor =
        abstract prototype: Int16Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Int16Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Int16Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int16Array
        abstract ``of``: [<ParamArray>] items: float[] -> Int16Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Int16Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Int16Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Int16Array

    and [<AllowNullLiteral>] Uint16Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Uint16Array
        abstract every: callbackfn: (float->float->Uint16Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Uint16Array
        abstract filter: callbackfn: (float->float->Uint16Array->bool) * ?thisArg: obj -> Uint16Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Uint16Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Uint16Array->float) * ?thisArg: obj -> Uint16Array
        abstract reduce: callbackfn: (float->float->float->Uint16Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Uint16Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Uint16Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Uint16Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Uint16Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Uint16Array
        abstract some: callbackfn: (float->float->Uint16Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Uint16Array
        abstract subarray: ``begin``: float * ?``end``: float -> Uint16Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Uint16ArrayConstructor =
        abstract prototype: Uint16Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Uint16Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Uint16Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint16Array
        abstract ``of``: [<ParamArray>] items: float[] -> Uint16Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint16Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Uint16Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint16Array

    and [<AllowNullLiteral>] Int32Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Int32Array
        abstract every: callbackfn: (float->float->Int32Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Int32Array
        abstract filter: callbackfn: (float->float->Int32Array->bool) * ?thisArg: obj -> Int32Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Int32Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Int32Array->float) * ?thisArg: obj -> Int32Array
        abstract reduce: callbackfn: (float->float->float->Int32Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Int32Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Int32Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Int32Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Int32Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Int32Array
        abstract some: callbackfn: (float->float->Int32Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Int32Array
        abstract subarray: ``begin``: float * ?``end``: float -> Int32Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Int32ArrayConstructor =
        abstract prototype: Int32Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Int32Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Int32Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Int32Array
        abstract ``of``: [<ParamArray>] items: float[] -> Int32Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Int32Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Int32Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Int32Array

    and [<AllowNullLiteral>] Uint32Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Uint32Array
        abstract every: callbackfn: (float->float->Uint32Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Uint32Array
        abstract filter: callbackfn: (float->float->Uint32Array->bool) * ?thisArg: obj -> Uint32Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Uint32Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Uint32Array->float) * ?thisArg: obj -> Uint32Array
        abstract reduce: callbackfn: (float->float->float->Uint32Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Uint32Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Uint32Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Uint32Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Uint32Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Uint32Array
        abstract some: callbackfn: (float->float->Uint32Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Uint32Array
        abstract subarray: ``begin``: float * ?``end``: float -> Uint32Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Uint32ArrayConstructor =
        abstract prototype: Uint32Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Uint32Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Uint32Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Uint32Array
        abstract ``of``: [<ParamArray>] items: float[] -> Uint32Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint32Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Uint32Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Uint32Array

    and [<AllowNullLiteral>] Float32Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Float32Array
        abstract every: callbackfn: (float->float->Float32Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Float32Array
        abstract filter: callbackfn: (float->float->Float32Array->bool) * ?thisArg: obj -> Float32Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Float32Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Float32Array->float) * ?thisArg: obj -> Float32Array
        abstract reduce: callbackfn: (float->float->float->Float32Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Float32Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Float32Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Float32Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Float32Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Float32Array
        abstract some: callbackfn: (float->float->Float32Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Float32Array
        abstract subarray: ``begin``: float * ?``end``: float -> Float32Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Float32ArrayConstructor =
        abstract prototype: Float32Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Float32Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Float32Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Float32Array
        abstract ``of``: [<ParamArray>] items: float[] -> Float32Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Float32Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Float32Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Float32Array

    and [<AllowNullLiteral>] Float64Array =
        abstract BYTES_PER_ELEMENT: float with get, set
        abstract buffer: ArrayBuffer with get, set
        abstract byteLength: float with get, set
        abstract byteOffset: float with get, set
        abstract length: float with get, set
        [<Emit("$0[$1]{{=$2}}")>] abstract Item: index: int -> float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract copyWithin: target: float * start: float * ?``end``: float -> Float64Array
        abstract every: callbackfn: (float->float->Float64Array->bool) * ?thisArg: obj -> bool
        abstract fill: value: float * ?start: float * ?``end``: float -> Float64Array
        abstract filter: callbackfn: (float->float->Float64Array->bool) * ?thisArg: obj -> Float64Array
        abstract find: predicate: (float->float->ResizeArray<float>->bool) * ?thisArg: obj -> float
        abstract findIndex: predicate: (float->bool) * ?thisArg: obj -> float
        abstract forEach: callbackfn: (float->float->Float64Array->unit) * ?thisArg: obj -> unit
        abstract indexOf: searchElement: float * ?fromIndex: float -> float
        abstract join: ?separator: string -> string
        abstract lastIndexOf: searchElement: float * ?fromIndex: float -> float
        abstract map: callbackfn: (float->float->Float64Array->float) * ?thisArg: obj -> Float64Array
        abstract reduce: callbackfn: (float->float->float->Float64Array->float) * ?initialValue: float -> float
        abstract reduce: callbackfn: ('U->float->float->Float64Array->'U) * initialValue: 'U -> 'U
        abstract reduceRight: callbackfn: (float->float->float->Float64Array->float) * ?initialValue: float -> float
        abstract reduceRight: callbackfn: ('U->float->float->Float64Array->'U) * initialValue: 'U -> 'U
        abstract reverse: unit -> Float64Array
        abstract set: index: float * value: float -> unit
        abstract set: array: ArrayLike<float> * ?offset: float -> unit
        abstract slice: ?start: float * ?``end``: float -> Float64Array
        abstract some: callbackfn: (float->float->Float64Array->bool) * ?thisArg: obj -> bool
        abstract sort: ?compareFn: (float->float->float) -> Float64Array
        abstract subarray: ``begin``: float * ?``end``: float -> Float64Array
        abstract toLocaleString: unit -> string
        abstract toString: unit -> string
        abstract entries: unit -> IterableIterator<float * float>
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<float>

    and [<AllowNullLiteral>] Float64ArrayConstructor =
        abstract prototype: Float64Array with get, set
        abstract BYTES_PER_ELEMENT: float with get, set
        [<Emit("new $0($1...)")>] abstract Create: length: float -> Float64Array
        [<Emit("new $0($1...)")>] abstract Create: array: ArrayLike<float> -> Float64Array
        [<Emit("new $0($1...)")>] abstract Create: buffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Float64Array
        abstract ``of``: [<ParamArray>] items: float[] -> Float64Array
        abstract from: arrayLike: ArrayLike<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Float64Array
        [<Emit("new $0($1...)")>] abstract Create: elements: Iterable<float> -> Float64Array
        abstract from: arrayLike: Iterable<float> * ?mapfn: (float->float->float) * ?thisArg: obj -> Float64Array

    and [<AllowNullLiteral>] Symbol =
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract toString: unit -> string
        abstract valueOf: unit -> obj

    and [<AllowNullLiteral>] SymbolConstructor =
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
        [<Emit("$0($1...)")>] abstract Invoke: ?description: U2<string, float> -> Symbol
        abstract ``for``: key: string -> Symbol
        abstract keyFor: sym: Symbol -> string

    and [<AllowNullLiteral>] IteratorResult<'T> =
        abstract ``done``: bool with get, set
        abstract value: 'T option with get, set

    and [<AllowNullLiteral>] Iterator<'T> =
        abstract next: ?value: obj -> IteratorResult<'T>
        abstract ``return``: ?value: obj -> IteratorResult<'T>
        abstract throw: ?e: obj -> IteratorResult<'T>

    and [<AllowNullLiteral>] Iterable<'T> =
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> Iterator<'T>

    and [<AllowNullLiteral>] IterableIterator<'T> =
        inherit Iterator<'T>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<'T>

    and [<AllowNullLiteral>] GeneratorFunction =
        inherit Function
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set

    and [<AllowNullLiteral>] GeneratorFunctionConstructor =
        abstract prototype: GeneratorFunction with get, set
        [<Emit("new $0($1...)")>] abstract Create: [<ParamArray>] args: string[] -> GeneratorFunction
        [<Emit("$0($1...)")>] abstract Invoke: [<ParamArray>] args: string[] -> GeneratorFunction

    and [<AllowNullLiteral>] Map<'K, 'V> =
        abstract size: float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract entries: unit -> IterableIterator<'K * 'V>
        abstract forEach: callbackfn: ('V->'K->Map<'K, 'V>->unit) * ?thisArg: obj -> unit
        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract keys: unit -> IterableIterator<'K>
        abstract set: key: 'K * ?value: 'V -> Map<'K, 'V>
        abstract values: unit -> IterableIterator<'V>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<'K * 'V>

    and [<AllowNullLiteral>] MapConstructor =
        abstract prototype: Map<obj, obj> with get, set
        [<Emit("new $0($1...)")>] abstract Create: unit -> Map<obj, obj>
        [<Emit("new $0($1...)")>] abstract Create: unit -> Map<'K, 'V>
        [<Emit("new $0($1...)")>] abstract Create: iterable: Iterable<'K * 'V> -> Map<'K, 'V>

    and [<AllowNullLiteral>] WeakMap<'K, 'V> =
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract set: key: 'K * ?value: 'V -> WeakMap<'K, 'V>

    and [<AllowNullLiteral>] WeakMapConstructor =
        abstract prototype: WeakMap<obj, obj> with get, set
        [<Emit("new $0($1...)")>] abstract Create: unit -> WeakMap<obj, obj>
        [<Emit("new $0($1...)")>] abstract Create: unit -> WeakMap<'K, 'V>
        [<Emit("new $0($1...)")>] abstract Create: iterable: Iterable<'K * 'V> -> WeakMap<'K, 'V>

    and [<AllowNullLiteral>] Set<'T> =
        abstract size: float with get, set
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract add: value: 'T -> Set<'T>
        abstract clear: unit -> unit
        abstract delete: value: 'T -> bool
        abstract entries: unit -> IterableIterator<'T * 'T>
        abstract forEach: callbackfn: ('T->'T->Set<'T>->unit) * ?thisArg: obj -> unit
        abstract has: value: 'T -> bool
        abstract keys: unit -> IterableIterator<'T>
        abstract values: unit -> IterableIterator<'T>
        [<Emit("$0[Symbol.iterator]($1...)")>] abstract ``[Symbol.iterator]``: unit -> IterableIterator<'T>

    and [<AllowNullLiteral>] SetConstructor =
        abstract prototype: Set<obj> with get, set
        [<Emit("new $0($1...)")>] abstract Create: unit -> Set<obj>
        [<Emit("new $0($1...)")>] abstract Create: unit -> Set<'T>
        [<Emit("new $0($1...)")>] abstract Create: iterable: Iterable<'T> -> Set<'T>

    and [<AllowNullLiteral>] WeakSet<'T> =
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract add: value: 'T -> WeakSet<'T>
        abstract clear: unit -> unit
        abstract delete: value: 'T -> bool
        abstract has: value: 'T -> bool

    and [<AllowNullLiteral>] WeakSetConstructor =
        abstract prototype: WeakSet<obj> with get, set
        [<Emit("new $0($1...)")>] abstract Create: unit -> WeakSet<obj>
        [<Emit("new $0($1...)")>] abstract Create: unit -> WeakSet<'T>
        [<Emit("new $0($1...)")>] abstract Create: iterable: Iterable<'T> -> WeakSet<'T>

    and [<AllowNullLiteral>] ProxyHandler<'T> =
        abstract getPrototypeOf: target: 'T -> obj
        abstract setPrototypeOf: target: 'T * v: obj -> bool
        abstract isExtensible: target: 'T -> bool
        abstract preventExtensions: target: 'T -> bool
        abstract getOwnPropertyDescriptor: target: 'T * p: obj -> PropertyDescriptor
        abstract has: target: 'T * p: obj -> bool
        abstract get: target: 'T * p: obj * receiver: obj -> obj
        abstract set: target: 'T * p: obj * value: obj * receiver: obj -> bool
        abstract deleteProperty: target: 'T * p: obj -> bool
        abstract defineProperty: target: 'T * p: obj * attributes: PropertyDescriptor -> bool
        abstract enumerate: target: 'T -> ResizeArray<obj>
        abstract ownKeys: target: 'T -> ResizeArray<obj>
        abstract apply: target: 'T * thisArg: obj * ?argArray: obj -> obj
        abstract construct: target: 'T * thisArg: obj * ?argArray: obj -> obj

    and [<AllowNullLiteral>] ProxyConstructor =
        abstract revocable: target: 'T * handler: ProxyHandler<'T> -> obj
        [<Emit("new $0($1...)")>] abstract Create: target: 'T * handler: ProxyHandler<'T> -> 'T

    and [<AllowNullLiteral>] Promise<'T> =
        [<Emit("$0[Symbol.toStringTag]{{=$1}}")>] abstract ``[Symbol.toStringTag]``: obj with get, set
        abstract ``then``: ?onfulfilled: ('T->'TResult) * ?onrejected: (obj->'TResult) -> Promise<'TResult>
        abstract catch: ?onrejected: (obj->'T) -> Promise<'T>

    and [<AllowNullLiteral>] PromiseConstructor =
        abstract prototype: Promise<obj> with get, set
        [<Emit("$0[Symbol.species]{{=$1}}")>] abstract ``[Symbol.species]``: Function with get, set
        [<Emit("new $0($1...)")>] abstract Create: executor: ((obj->unit) -> (obj->unit) -> unit) -> Promise<'T>
        abstract all: [<ParamArray>] values: obj[] -> Promise<obj>
        abstract race: values: obj seq -> Promise<obj>
        abstract reject: reason: obj -> Promise<unit>
        abstract reject: reason: obj -> Promise<'T>
        abstract resolve: value: 'T -> Promise<'T>
        abstract resolve: unit -> Promise<unit>

    type [<AllowNullLiteral>] [<Global>] Reflect =
        static member apply(target: Function, thisArgument: obj, argumentsList: ArrayLike<obj>): obj = jsNative
        static member construct(target: Function, argumentsList: ArrayLike<obj>, ?newTarget: obj): obj = jsNative
        static member defineProperty(target: obj, propertyKey: obj, attributes: PropertyDescriptor): bool = jsNative
        static member deleteProperty(target: obj, propertyKey: obj): bool = jsNative
        static member enumerate(target: obj): IterableIterator<obj> = jsNative
        static member get(target: obj, propertyKey: obj, ?receiver: obj): obj = jsNative
        static member getOwnPropertyDescriptor(target: obj, propertyKey: obj): PropertyDescriptor = jsNative
        static member getPrototypeOf(target: obj): obj = jsNative
        static member has(target: obj, propertyKey: string): bool = jsNative
        static member has(target: obj, propertyKey: Symbol): bool = jsNative
        static member isExtensible(target: obj): bool = jsNative
        static member ownKeys(target: obj): ResizeArray<obj> = jsNative
        static member preventExtensions(target: obj): bool = jsNative
        static member set(target: obj, propertyKey: obj, value: obj, ?receiver: obj): bool = jsNative
        static member setPrototypeOf(target: obj, proto: obj): bool = jsNative

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

    and [<Erase>] SetTimeoutToken =
        | SetTimeoutToken of obj

    and [<Erase>] SetIntervalToken =
        | SetIntervalToken of obj

    let [<Global>] NaN: float = jsNative
    let [<Global>] Infinity: float = jsNative
    let [<Global>] Object: ObjectConstructor = jsNative
    let [<Global>] Function: FunctionConstructor = jsNative
    let [<Global>] String: StringConstructor = jsNative
    let [<Global>] Boolean: BooleanConstructor = jsNative
    let [<Global>] Number: NumberConstructor = jsNative
    let [<Global>] Math: Math = jsNative
    let [<Global>] Date: DateConstructor = jsNative
    let [<Global>] RegExp: RegExpConstructor = jsNative
    let [<Global>] Error: ErrorConstructor = jsNative
    let [<Global>] EvalError: EvalErrorConstructor = jsNative
    let [<Global>] RangeError: RangeErrorConstructor = jsNative
    let [<Global>] ReferenceError: ReferenceErrorConstructor = jsNative
    let [<Global>] SyntaxError: SyntaxErrorConstructor = jsNative
    let [<Global>] TypeError: TypeErrorConstructor = jsNative
    let [<Global>] URIError: URIErrorConstructor = jsNative
    let [<Global>] JSON: JSON = jsNative
    let [<Global>] Array: ArrayConstructor = jsNative
    let [<Global>] ArrayBuffer: ArrayBufferConstructor = jsNative
    let [<Global>] DataView: DataViewConstructor = jsNative
    let [<Global>] Int8Array: Int8ArrayConstructor = jsNative
    let [<Global>] Uint8Array: Uint8ArrayConstructor = jsNative
    let [<Global>] Uint8ClampedArray: Uint8ClampedArrayConstructor = jsNative
    let [<Global>] Int16Array: Int16ArrayConstructor = jsNative
    let [<Global>] Uint16Array: Uint16ArrayConstructor = jsNative
    let [<Global>] Int32Array: Int32ArrayConstructor = jsNative
    let [<Global>] Uint32Array: Uint32ArrayConstructor = jsNative
    let [<Global>] Float32Array: Float32ArrayConstructor = jsNative
    let [<Global>] Float64Array: Float64ArrayConstructor = jsNative
    let [<Global>] Symbol: SymbolConstructor = jsNative
    let [<Global>] GeneratorFunction: GeneratorFunctionConstructor = jsNative
    let [<Global>] Map: MapConstructor = jsNative
    let [<Global>] WeakMap: WeakMapConstructor = jsNative
    let [<Global>] Set: SetConstructor = jsNative
    let [<Global>] WeakSet: WeakSetConstructor = jsNative
    let [<Global>] Proxy: ProxyConstructor = jsNative
    let [<Global>] Promise: PromiseConstructor = jsNative
    let [<Global>] eval: string -> string = jsNative
    let [<Global>] isFinite: float -> bool = jsNative
    let [<Global>] isNaN: float -> bool = jsNative
    let [<Global>] parseFloat: string -> float = jsNative
    let [<Global>] parseInt: string -> int -> int = jsNative
    let [<Global>] decodeURI: string -> string = jsNative
    let [<Global>] decodeURIComponent: string -> string = jsNative
    let [<Global>] encodeURI: string -> string = jsNative
    let [<Global>] encodeURIComponent: string -> string = jsNative
    let [<Global>] console : Console = jsNative
    let [<Global>] setTimeout (callback: unit -> unit) (ms: int): SetTimeoutToken = jsNative
    let [<Emit("setTimeout($0, $1, $2)")>] setTimeout1<'A> (callback: 'A -> unit) (ms: int) (arg1:'A) : SetTimeoutToken = jsNative
    let [<Emit("setTimeout($0, $1, $2, $3)")>] setTimeout2<'A, 'B> (callback: 'A -> 'B -> unit) (ms: int) (arg1: 'A) (arg2: 'B) : SetTimeoutToken = jsNative
    let [<Emit("setTimeout($0, $1, $2, $3, $4)")>] setTimeout3<'A, 'B, 'C> (callback: 'A -> 'B -> 'C -> unit) (ms: int) (arg1: 'A) (arg2: 'B) (arg3: 'C) : SetTimeoutToken = jsNative
    let [<Emit("setTimeout($0, $1, $2, $3, $4, $5)")>] setTimeout4<'A, 'B, 'C, 'D> (callback: 'A -> 'B -> 'C -> 'D -> unit) (ms:int) (arg1: 'A) (arg2: 'B) (arg3: 'C) (arg4: 'D): SetTimeoutToken = jsNative
    let [<Emit("setTimeout($0, $1, $2, $3, $4, $5, $6)")>] setTimeout5<'A, 'B, 'C, 'D, 'E> (callback: 'A -> 'B -> 'C -> 'D -> 'E -> unit) (ms: int) (arg1: 'A) (arg2: 'B) (arg3: 'C) (arg4: 'D) (arg5: 'E) : SetTimeoutToken = jsNative
    let [<Global>] clearTimeout (token: SetTimeoutToken): unit = jsNative
    let [<Global>] setInterval(callback: unit -> unit) (ms: int) : SetIntervalToken = jsNative
    let [<Emit("setInterval($0, $1, $2)")>] setInterval1<'A> (callback: 'A -> unit) (ms: int) (arg1: 'A) : SetIntervalToken = jsNative
    let [<Emit("setInterval($0, $1, $2, $3)")>] setInterval2<'A, 'B> (callback: 'A -> 'B -> unit) (ms: int) (arg1: 'A) (arg2: 'B) : SetIntervalToken = jsNative
    let [<Emit("setInterval($0, $1, $2, $3, $4)")>] setInterval3<'A, 'B, 'C> (callback: 'A -> 'B -> 'C -> unit) (ms: int) (arg1: 'A) (arg2: 'B) (arg3: 'C) : SetIntervalToken = jsNative
    let [<Emit("setInterval($0, $1, $2, $3, $4, $5)")>] setInterval4<'A, 'B, 'C, 'D> (callback: 'A -> 'B -> 'C -> 'D -> unit) (ms: int) (arg1: 'A) (arg2: 'B) (arg3: 'C) (arg4: 'D) : SetIntervalToken = jsNative
    let [<Emit("setInterval($0, $1, $2, $3, $4, $5, $6)")>] setInterval5<'A, 'B, 'C, 'D, 'E> (callback: 'A -> 'B -> 'C -> 'D -> 'E -> unit) (ms: int) (arg1: 'A) (arg2: 'B) (arg3: 'C) (arg4: 'D) (arg5: 'E) : SetIntervalToken = jsNative
    let [<Global>] clearInterval (token:SetIntervalToken): unit = jsNative

    let [<Emit("debugger;")>] debugger () : unit = jsNative
