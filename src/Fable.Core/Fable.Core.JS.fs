namespace Fable.Core

open System
open System.Text.RegularExpressions

[<Erase>]
type JS =
    [<Emit("new RegExp($0...)")>] 
    static member RegExp (pattern: string, ?flags: string) : Regex = jsNative

module JS =
    [<AllowNullLiteral;Global>]
    type PropertyDescriptor =
        member _.configurable
            with get () : bool option = jsNative
            and set (x: bool option) = jsNative
        member _.enumerable
            with get () : bool option = jsNative
            and set (x: bool option) = jsNative
        member _.value
            with get () : obj option  = jsNative
            and set (x: obj option) = jsNative
        member _.writable
            with get () : bool option = jsNative
            and set (x: bool option) = jsNative
        member _.get () : obj = jsNative
        member _.set (v: obj) : unit = jsNative

    [<AllowNullLiteral;Global>]
    type Array =
        static member isArray (arg: obj) : bool = jsNative

    [<AllowNullLiteral;Global>]
    type Number =
        static member isNaN (value: float) : bool = jsNative

    [<AllowNullLiteral;Global>]
    type Object =
        member _.toString () : string = jsNative
        member _.toLocaleString () : string = jsNative
        member _.valueOf () : Object = jsNative
        member _.hasOwnProperty (v: string) : bool = jsNative
        member _.isPrototypeOf (v: obj) : bool = jsNative
        member _.propertyIsEnumerable (v: string) : bool = jsNative
        member _.hasOwnProperty (v: obj) : bool = jsNative
        member _.propertyIsEnumerable (v: obj) : bool = jsNative

        static member getPrototypeOf (o: obj) : Object = jsNative
        static member getOwnPropertyDescriptor (o: obj, p: string) : PropertyDescriptor = jsNative
        static member getOwnPropertyNames (o: obj) : ResizeArray<string> = jsNative
        static member create (o: obj, ?properties: obj) : Object = jsNative
        static member defineProperty (o: obj, p: string, attributes: PropertyDescriptor) : Object = jsNative
        static member defineProperties (o: obj, properties: obj) : Object = jsNative
        static member seal (o: 'T) : 'T = jsNative
        static member freeze (o: 'T) : 'T = jsNative
        static member preventExtensions (o: 'T) : 'T = jsNative
        static member isSealed (o: obj) : bool = jsNative
        static member isFrozen (o: obj) : bool = jsNative
        static member isExtensible (o: obj) : bool = jsNative
        static member keys (o: obj) : ResizeArray<string> = jsNative
        static member assign (target: 'T, source: 'U) : Object = jsNative
        static member assign (target: 'T, source1: 'U, source2: 'V) : Object = jsNative
        static member assign (target: 'T, source1: 'U, source2: 'V, source3: 'W) : Object = jsNative
        static member assign (target: obj, [<ParamArray>] sources: obj []) : Object = jsNative
        // static member getOwnPropertySymbols (o: obj) : ResizeArray<Symbol> = jsNative
        static member is (value1: obj, value2: obj) : bool = jsNative
        static member setPrototypeOf (o: obj, proto: obj) : Object = jsNative
        static member getOwnPropertyDescriptor (o: obj, propertyKey: obj) : PropertyDescriptor = jsNative
        static member defineProperty (o: obj, propertyKey: obj, attributes: PropertyDescriptor) : Object = jsNative

    [<Global>]
    type Math =
        static member E: float = jsNative
        static member LN10: float = jsNative
        static member LN2: float = jsNative
        static member LOG2E: float = jsNative
        static member LOG10E: float = jsNative
        static member PI: float = jsNative
        static member SQRT1_2: float = jsNative
        static member SQRT2: float = jsNative
        static member abs (x: float) : float = jsNative
        static member acos (x: float) : float = jsNative
        static member asin (x: float) : float = jsNative
        static member atan (x: float) : float = jsNative
        static member atan2 (y: float, x: float) : float = jsNative
        static member ceil (x: float) : float = jsNative
        static member cos (x: float) : float = jsNative
        static member exp (x: float) : float = jsNative
        static member floor (x: float) : float = jsNative
        static member log (x: float) : float = jsNative
        static member max ([<ParamArray>] values: float[]) : float = jsNative
        static member min ([<ParamArray>] values: float[]) : float = jsNative
        static member pow (x: float, y: float) : float = jsNative
        static member random () : float = jsNative
        static member round (x: float) : float = jsNative
        static member sin (x: float) : float = jsNative
        static member sqrt (x: float) : float = jsNative
        static member tan (x: float) : float = jsNative
        static member clz32 (x: float) : float = jsNative
        static member imul (x: float, y: float) : float = jsNative
        static member sign (x: float) : float = jsNative
        static member log10 (x: float) : float = jsNative
        static member log2 (x: float) : float = jsNative
        static member log1p (x: float) : float = jsNative
        static member expm1 (x: float) : float = jsNative
        static member cosh (x: float) : float = jsNative
        static member sinh (x: float) : float = jsNative
        static member tanh (x: float) : float = jsNative
        static member acosh (x: float) : float = jsNative
        static member asinh (x: float) : float = jsNative
        static member atanh (x: float) : float = jsNative
        static member hypot ([<ParamArray>] values: float[]) : float = jsNative
        static member trunc (x: float) : float = jsNative
        static member fround (x: float) : float = jsNative
        static member cbrt (x: float) : float = jsNative

    [<AllowNullLiteral;Global>]
    type Date [<Emit("new Date($0)")>] private (inp: obj) =
        new (ticks: int) = Date(unbox<int> ticks)
        new (ticks: int64) = Date(unbox<int> ticks)
        new (value: string) = Date(value)
        new (year: float, month: float, ?date: float, ?hours: float, ?minutes: float, ?seconds: float, ?ms: float) = 
            Date(box (year, month, date, hours, minutes, seconds, ms))
        new (year: int, month: int, ?date: int, ?hours: int, ?minutes: int, ?seconds: int, ?ms: int ) = 
            Date(box (year, month, date, hours, minutes, seconds, ms))
        
        member _.toString () : string = jsNative
        member _.toDateString () : string = jsNative
        member _.toTimeString () : string = jsNative
        member _.toLocaleString () : string = jsNative
        member _.toLocaleDateString () : string = jsNative
        member _.toLocaleTimeString () : string = jsNative
        member _.valueOf () : float = jsNative
        member _.getTime () : float = jsNative
        member _.getFullYear () : float = jsNative
        member _.getUTCFullYear () : float = jsNative
        member _.getMonth () : float = jsNative
        member _.getUTCMonth () : float = jsNative
        member _.getDate () : float = jsNative
        member _.getUTCDate () : float = jsNative
        member _.getDay () : float = jsNative
        member _.getUTCDay () : float = jsNative
        member _.getHours () : float = jsNative
        member _.getUTCHours () : float = jsNative
        member _.getMinutes () : float = jsNative
        member _.getUTCMinutes () : float = jsNative
        member _.getSeconds () : float = jsNative
        member _.getUTCSeconds () : float = jsNative
        member _.getMilliseconds () : float = jsNative
        member _.getUTCMilliseconds () : float = jsNative
        member _.getTimezoneOffset () : float = jsNative
        member _.setTime (time: float) : float = jsNative
        member _.setMilliseconds (ms: float) : float = jsNative
        member _.setUTCMilliseconds (ms: float) : float = jsNative
        member _.setSeconds (sec: float, ?ms: float) : float = jsNative
        member _.setUTCSeconds (sec: float, ?ms: float) : float = jsNative
        member _.setMinutes (min: float, ?sec: float, ?ms: float) : float = jsNative
        member _.setUTCMinutes (min: float, ?sec: float, ?ms: float) : float = jsNative
        member _.setHours (hours: float, ?min: float, ?sec: float, ?ms: float) : float = jsNative
        member _.setUTCHours (hours: float, ?min: float, ?sec: float, ?ms: float) : float = jsNative
        member _.setDate (date: float) : float = jsNative
        member _.setUTCDate (date: float) : float = jsNative
        member _.setMonth (month: float, ?date: float) : float = jsNative
        member _.setUTCMonth (month: float, ?date: float) : float = jsNative
        member _.setFullYear (year: float, ?month: float, ?date: float) : float = jsNative
        member _.setUTCFullYear (year: float, ?month: float, ?date: float) : float = jsNative
        member _.toUTCString () : string = jsNative
        member _.toISOString () : string = jsNative
        member _.toJSON (?key: obj) : string = jsNative

        [<Emit("Date()")>]
        static member Invoke () : string = jsNative

        static member parse (s: string) : float = jsNative

        static member UTC (year: float, month: float, ?date: float, ?hours: float, ?minutes: float, ?seconds: float, ?ms: float) : float = jsNative

        static member now () : float = jsNative

    [<AllowNullLiteral;Global>]
    type JSON =
        static member parse (text: string, ?reviver: obj->obj->obj) : obj = jsNative
        static member stringify (value: obj, ?replacer: string->obj->obj, ?space: obj) : string = jsNative

    [<AllowNullLiteral;Global>]
    type Map<'K, 'V> [<Emit("new Map($0...)")>] (?iterable: seq<'K * 'V>) =
        member _.size: int = jsNative
        member _.clear () : unit = jsNative
        member _.delete (key: 'K) : bool = jsNative
        member _.entries () : seq<'K * 'V> = jsNative
        member _.forEach (callbackfn: 'V->'K->Map<'K, 'V>->unit, ?thisArg: obj) : unit = jsNative
        member _.get (key: 'K) : 'V = jsNative
        member _.has (key: 'K) : bool = jsNative
        member _.keys () : seq<'K> = jsNative
        member _.set (key: 'K, ?value: 'V) : Map<'K, 'V> = jsNative
        member _.values () : seq<'V> = jsNative

    [<AllowNullLiteral;Global>]
    type WeakMap<'K, 'V> [<Emit("new WeakMap($0...)")>] (?iterable: seq<'K * 'V>) =
        member _.clear () : unit = jsNative
        member _.delete (key: 'K) : bool = jsNative
        member _.get (key: 'K) : 'V = jsNative
        member _.has (key: 'K) : bool = jsNative
        member _.set (key: 'K, ?value: 'V) : WeakMap<'K, 'V> = jsNative

    [<AllowNullLiteral;Global>]
    type Set<'T> [<Emit("new Set($0...)")>] (?iterable: seq<'T>) =
        member _.size: int = jsNative
        member _.add (value: 'T) : Set<'T> = jsNative
        member _.clear () : unit = jsNative
        member _.delete (value: 'T) : bool = jsNative
        member _.entries () : seq<'T * 'T> = jsNative
        member _.forEach (callbackfn: 'T -> 'T -> Set<'T> -> unit, ?thisArg: obj) : unit = jsNative
        member _.has (value: 'T) : bool = jsNative
        member _.keys () : seq<'T> = jsNative
        member _.values () : seq<'T> = jsNative

    [<AllowNullLiteral;Global>]
    type WeakSet<'T> [<Emit("new WeakSet($0...)")>] (?iterable: seq<'T>) =
        member _.add (value: 'T) : WeakSet<'T> = jsNative
        member _.clear () : unit = jsNative
        member _.delete (value: 'T) : bool = jsNative
        member _.has (value: 'T) : bool = jsNative

    [<AllowNullLiteral;Global>]
    type Promise<'T> [<Emit("new Promise($0)")>] (executor: ('T -> unit) -> (obj -> unit) -> unit) =
        [<Emit("$0.then($1...)")>]
        member _.then'<'Reject,'TResult> (?onfulfilled: 'T -> 'TResult, ?onrejected: 'Reject -> 'TResult) : Promise<'TResult> = jsNative
        [<Emit("$0.catch($1...)")>]
        member _.catch<'Reject> (?onrejected: 'Reject -> 'T) : Promise<'T> = jsNative

        static member all ([<ParamArray>] values: obj []) : Promise<obj> = jsNative
        static member race (values: seq<_>) : Promise<_> = jsNative
        static member reject<'Reason> (reason: 'Reason) : Promise<'Reason> = jsNative
        static member reject () : Promise<unit> = jsNative
        static member resolve (value: 'T) : Promise<'T> = jsNative
        static member resolve () : Promise<unit> = jsNative

    [<AllowNullLiteral;Global>]
    type ArrayBuffer [<Emit("new ArrayBuffer($0)")>] (byteLength: int) =
        member _.byteLength: int = jsNative
        member _.slice (begin': int, ?end': int) : ArrayBuffer = jsNative
        
        static member isView (arg: obj) : bool = jsNative

    [<AllowNullLiteral;Global>]
    type ArrayBufferView [<Emit "new ArrayBufferView($0)">] (size: int) =
        member _.buffer: ArrayBuffer = jsNative
        member _.byteLength: int = jsNative
        member _.byteOffset: int = jsNative

    [<AllowNullLiteral;Global>]
    type DataView [<Emit("new DataView($0...)")>] (buffer: ArrayBuffer, ?byteOffset: int, ?byteLength: float) =
        member _.buffer: ArrayBuffer = jsNative
        member _.byteLength: int = jsNative
        member _.byteOffset: int = jsNative
        member _.getFloat32 (byteOffset: int, ?littleEndian: bool) : float32 = jsNative
        member _.getFloat64 (byteOffset: int, ?littleEndian: bool) : float = jsNative
        member _.getInt8 (byteOffset: int) : sbyte = jsNative
        member _.getInt16 (byteOffset: int, ?littleEndian: bool) : int16 = jsNative
        member _.getInt32 (byteOffset: int, ?littleEndian: bool) : int32 = jsNative
        member _.getUint8 (byteOffset: int) : byte = jsNative
        member _.getUint16 (byteOffset: int, ?littleEndian: bool) : uint16 = jsNative
        member _.getUint32 (byteOffset: int, ?littleEndian: bool) : uint32 = jsNative
        member _.setFloat32 (byteOffset: int, value: float32, ?littleEndian: bool) : unit = jsNative
        member _.setFloat64 (byteOffset: int, value: float, ?littleEndian: bool) : unit = jsNative
        member _.setInt8 (byteOffset: int, value: sbyte) : unit = jsNative
        member _.setInt16 (byteOffset: int, value: int16, ?littleEndian: bool) : unit = jsNative
        member _.setInt32 (byteOffset: int, value: int32, ?littleEndian: bool) : unit = jsNative
        member _.setUint8 (byteOffset: int, value: byte) : unit = jsNative
        member _.setUint16 (byteOffset: int, value: uint16, ?littleEndian: bool) : unit = jsNative
        member _.setUint32 (byteOffset: int, value: uint32, ?littleEndian: bool) : unit = jsNative

    [<Global>]
    type TypedArray =
        member _.buffer: ArrayBuffer = jsNative
        member _.byteLength: int = jsNative
        member _.byteOffset: int = jsNative
        member _.length: int = jsNative
        member _.copyWithin (targetStartIndex: int, start: int, ?end': int) : unit = jsNative
        member _.entries () : obj = jsNative
        member _.keys () : obj = jsNative
        member _.join (separator: string) : string = jsNative

    [<Global>]
    type TypedArray<'T> =
        inherit TypedArray

        member _.Item
            with [<Emit("$0[$1]")>] get (index: int) : 'T = jsNative
            and [<Emit("$0[$1] = $2")>] set (index: int) (value: int) = jsNative
        member _.fill (value:'T, ?begin': int, ?end': int) : TypedArray<'T> = jsNative
        member _.filter (f: 'T -> int -> TypedArray<'T> -> bool) : TypedArray<'T> = jsNative
        member _.filter (f: 'T -> int -> bool) : TypedArray<'T> = jsNative
        member _.filter (f: 'T -> bool) : TypedArray<'T> = jsNative
        member _.find (f: 'T -> int -> TypedArray<'T> -> bool) : 'T option = jsNative
        member _.find (f: 'T -> int -> bool) : 'T option = jsNative
        member _.find (f: 'T -> bool) : 'T option = jsNative
        member _.findIndex (f: 'T -> int -> TypedArray<'T> -> bool) : int = jsNative
        member _.findIndex (f: 'T -> int -> bool) : int = jsNative
        member _.findIndex (f: 'T -> bool) : int = jsNative
        member _.forEach (f: 'T -> int -> TypedArray<'T> -> bool) : unit = jsNative
        member _.forEach (f: 'T -> int -> bool) : unit = jsNative
        member _.forEach (f: 'T -> bool) : unit = jsNative
        member _.includes (searchElement:'T, ?fromIndex: int) : bool = jsNative
        member _.indexOf (searchElement:'T, ?fromIndex: int) : int = jsNative
        member _.lastIndexOf (searchElement:'T, ?fromIndex: int) : int = jsNative
        member _.map (f: 'T -> int -> TypedArray<'T> -> 'U) : TypedArray<'U> = jsNative
        member _.map (f: 'T -> int -> 'U) : TypedArray<'U> = jsNative
        member _.map (f: 'T -> 'U) : TypedArray<'U> = jsNative
        member _.reduce (f: 'State -> 'T -> int -> TypedArray<'T> -> 'State, state:'State) : 'State = jsNative
        member _.reduce (f: 'State -> 'T -> int -> 'State, state:'State) : 'State = jsNative
        member _.reduce (f: 'State -> 'T -> 'State, state: 'State) : 'State = jsNative
        member _.reduceRight (f: 'State -> 'T -> int -> TypedArray<'T> -> 'State, state:'State) : 'State = jsNative
        member _.reduceRight (f: 'State -> 'T -> int -> 'State, state: 'State) : 'State = jsNative
        member _.reduceRight (f: 'State -> 'T -> 'State, state:'State) : 'State = jsNative
        member _.reverse () : TypedArray<'T> = jsNative
        member _.set (source: Array, ?offset: int) : unit = jsNative
        member _.set (source: #TypedArray, ?offset: int) : unit = jsNative
        member _.slice (?begin': int, ?end': int) : TypedArray<'T> = jsNative
        member _.some (f: 'T -> int -> TypedArray<'T> -> bool) : bool = jsNative
        member _.some (f: 'T -> int -> bool) : bool = jsNative
        member _.some (f: 'T -> bool) : bool = jsNative
        member _.sort (?sortFunction: 'T -> 'T -> int) : bool = jsNative
        member _.subarray (?begin': int, ?end': int) : TypedArray<'T> = jsNative
        member _.values () : obj = jsNative

    [<Erase>]
    type Int8Array = 
        inherit TypedArray<int8>

        [<Emit "new Int8Array($0...)">]
        static member Create (size: int) : Int8Array = jsNative
        [<Emit "new Int8Array($0...)">]
        static member Create (typedArray: TypedArray) : Int8Array = jsNative
        [<Emit "new Int8Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Int8Array = jsNative
        [<Emit "new Int8Array($0...)">]
        static member Create (data: obj) : Int8Array = jsNative

    [<Erase>]
    type Uint8Array = 
        inherit TypedArray<uint8>
        
        [<Emit "new Uint8Array($0...)">]
        static member Create (size: int) : Uint8Array = jsNative
        [<Emit "new Uint8Array($0...)">]
        static member Create (typedArray: TypedArray) : Uint8Array = jsNative
        [<Emit "new Uint8Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Uint8Array = jsNative
        [<Emit "new Uint8Array($0...)">]
        static member Create (data: obj) : Uint8Array = jsNative

    [<Erase>]
    type Uint8ClampedArray = 
        inherit TypedArray<uint8>
        
        [<Emit "new Uint8ClampedArray($0...)">]
        static member Create (size: int) : Uint8ClampedArray = jsNative
        [<Emit "new Uint8ClampedArray($0...)">]
        static member Create (typedArray: TypedArray) : Uint8ClampedArray = jsNative
        [<Emit "new Uint8ClampedArray($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Uint8ClampedArray = jsNative
        [<Emit "new Uint8ClampedArray($0...)">]
        static member Create (data: obj) : Uint8ClampedArray = jsNative
    
    [<Erase>]
    type Int16Array = 
        inherit TypedArray<int16>
        
        [<Emit "new Int16Array($0...)">]
        static member Create (size: int) : Int16Array = jsNative
        [<Emit "new Int16Array($0...)">]
        static member Create (typedArray: TypedArray) : Int16Array = jsNative
        [<Emit "new Int16Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Int16Array = jsNative
        [<Emit "new Int16Array($0...)">]
        static member Create (data: obj) : Int16Array = jsNative
        
    [<Erase>]
    type Uint16Array = 
        inherit TypedArray<uint16>
        
        [<Emit "new Uint16Array($0...)">]
        static member Create (size: int) : Uint16Array = jsNative
        [<Emit "new Uint16Array($0...)">]
        static member Create (typedArray: TypedArray) : Uint16Array = jsNative
        [<Emit "new Uint16Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Uint16Array = jsNative
        [<Emit "new Uint16Array($0...)">]
        static member Create (data: obj) : Uint16Array = jsNative
        
    [<Erase>]
    type Int32Array = 
        inherit TypedArray<int32>
        
        [<Emit "new Int32Array($0...)">]
        static member Create (size: int) : Int32Array = jsNative
        [<Emit "new Int32Array($0...)">]
        static member Create (typedArray: TypedArray) : Int32Array = jsNative
        [<Emit "new Int32Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Int32Array = jsNative
        [<Emit "new Int32Array($0...)">]
        static member Create (data: obj) : Int32Array = jsNative
        
    [<Erase>]
    type Uint32Array = 
        inherit TypedArray<uint32>
        
        [<Emit "new Uint32Array($0...)">]
        static member Create (size: int) : Uint32Array = jsNative
        [<Emit "new Uint32Array($0...)">]
        static member Create (typedArray: TypedArray) : Uint32Array = jsNative
        [<Emit "new Uint32Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Uint32Array = jsNative
        [<Emit "new Uint32Array($0...)">]
        static member Create (data: obj) : Uint32Array = jsNative
        
    [<Erase>]
    type Float32Array = 
        inherit TypedArray<float32>
        
        [<Emit "new Float32Array($0...)">]
        static member Create (size: int) : Float32Array = jsNative
        [<Emit "new Float32Array($0...)">]
        static member Create (typedArray: TypedArray) : Float32Array = jsNative
        [<Emit "new Float32Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Float32Array = jsNative
        [<Emit "new Float32Array($0...)">]
        static member Create (data: obj) : Float32Array = jsNative
        
    [<Erase>]
    type Float64Array = 
        inherit TypedArray<float>
        
        [<Emit "new Float64Array($0...)">]
        static member Create (size: int) : Float64Array = jsNative
        [<Emit "new Float64Array($0...)">]
        static member Create (typedArray: TypedArray) : Float64Array = jsNative
        [<Emit "new Float64Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : Float64Array = jsNative
        [<Emit "new Float64Array($0...)">]
        static member Create (data: obj) : Float64Array = jsNative
        
    [<Erase>]
    type BigInt64Array = 
        inherit TypedArray<bigint>
        
        [<Emit "new BigInt64Array($0...)">]
        static member Create (size: int) : BigInt64Array = jsNative
        [<Emit "new BigInt64Array($0...)">]
        static member Create (typedArray: TypedArray) : BigInt64Array = jsNative
        [<Emit "new BigInt64Array($0...)">]
        static member Create (buffer: ArrayBuffer, ?offset: int, ?length: int) : BigInt64Array = jsNative
        [<Emit "new BigInt64Array($0...)">]
        static member Create (data: obj) : BigInt64Array = jsNative

    [<AllowNullLiteral;Global>]
    type Console =
        [<Emit("$0.assert($1...)")>]
        member _.assert' (test: bool, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        [<Emit("$0.assert($1...)")>]
        member _.assert' (test: bool, message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.clear () : unit = jsNative
        member _.count (?countTitle: string) : unit = jsNative
        member _.debug (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.debug (message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.dir (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.dirxml (value: obj) : unit = jsNative
        member _.error (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.error (message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.group (?groupTitle: string) : unit = jsNative
        member _.groupCollapsed (?groupTitle: string) : unit = jsNative
        member _.groupEnd () : unit = jsNative
        member _.info (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.info (message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.log (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.log (message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.profile (?reportName: string) : unit = jsNative
        member _.profileEnd () : unit = jsNative
        member _.time (?timerName: string) : unit = jsNative
        member _.timeEnd (?timerName: string) : unit = jsNative
        member _.trace (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.trace (message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.warn (value: obj, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.warn (message: string, [<ParamArray>] optionalParams: obj []) : unit = jsNative
        member _.table (?data: obj) : unit = jsNative

    let [<Global>] NaN: float = jsNative
    let [<Global>] Infinity: float = jsNative
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
    let [<Global>] setTimeout (callback: unit -> unit) (ms: int): int = jsNative
    let [<Global>] clearTimeout (token: int): unit = jsNative
    let [<Global>] setInterval(callback: unit -> unit) (ms: int) : int = jsNative
    let [<Global>] clearInterval (token: int): unit = jsNative
    let [<Emit("debugger")>] debugger () : unit = jsNative
    let [<Emit("void 0")>] undefined<'a> : 'a = jsNative
