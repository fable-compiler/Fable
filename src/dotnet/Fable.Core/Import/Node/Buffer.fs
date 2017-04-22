module rec Fable.Import.Node.Buffer

open Fable.Core
open Fable.Import.Node
open Fable.Import.JS

module buffer_types =
    type BufferEncoding =
    (* TODO StringEnum ascii | utf8 | utf16le | ucs2 | binary | hex *) string

    type Buffer = 
        abstract write: string: string * ?offset: float * ?length: float * ?encoding: string -> float
        abstract toString: ?encoding: string * ?start: float * ?``end``: float -> string
        abstract toJSON: unit -> obj
        abstract equals: otherBuffer: Buffer -> bool
        abstract compare: otherBuffer: Buffer * ?targetStart: float * ?targetEnd: float * ?sourceStart: float * ?sourceEnd: float -> float
        abstract copy: targetBuffer: Buffer * ?targetStart: float * ?sourceStart: float * ?sourceEnd: float -> float
        abstract slice: ?start: float * ?``end``: float -> Buffer
        abstract writeUIntLE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
        abstract writeUIntBE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
        abstract writeIntLE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
        abstract writeIntBE: value: float * offset: float * byteLength: float * ?noAssert: bool -> float
        abstract readUIntLE: offset: float * byteLength: float * ?noAssert: bool -> float
        abstract readUIntBE: offset: float * byteLength: float * ?noAssert: bool -> float
        abstract readIntLE: offset: float * byteLength: float * ?noAssert: bool -> float
        abstract readIntBE: offset: float * byteLength: float * ?noAssert: bool -> float
        abstract readUInt8: offset: float * ?noAssert: bool -> float
        abstract readUInt16LE: offset: float * ?noAssert: bool -> float
        abstract readUInt16BE: offset: float * ?noAssert: bool -> float
        abstract readUInt32LE: offset: float * ?noAssert: bool -> float
        abstract readUInt32BE: offset: float * ?noAssert: bool -> float
        abstract readInt8: offset: float * ?noAssert: bool -> float
        abstract readInt16LE: offset: float * ?noAssert: bool -> float
        abstract readInt16BE: offset: float * ?noAssert: bool -> float
        abstract readInt32LE: offset: float * ?noAssert: bool -> float
        abstract readInt32BE: offset: float * ?noAssert: bool -> float
        abstract readFloatLE: offset: float * ?noAssert: bool -> float
        abstract readFloatBE: offset: float * ?noAssert: bool -> float
        abstract readDoubleLE: offset: float * ?noAssert: bool -> float
        abstract readDoubleBE: offset: float * ?noAssert: bool -> float
        abstract swap16: unit -> Buffer
        abstract swap32: unit -> Buffer
        abstract swap64: unit -> Buffer
        abstract writeUInt8: value: float * offset: float * ?noAssert: bool -> float
        abstract writeUInt16LE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeUInt16BE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeUInt32LE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeUInt32BE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeInt8: value: float * offset: float * ?noAssert: bool -> float
        abstract writeInt16LE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeInt16BE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeInt32LE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeInt32BE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeFloatLE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeFloatBE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeDoubleLE: value: float * offset: float * ?noAssert: bool -> float
        abstract writeDoubleBE: value: float * offset: float * ?noAssert: bool -> float
        abstract fill: value: obj * ?offset: float * ?``end``: float -> obj
        abstract indexOf: value: U3<string, float, Buffer> * ?byteOffset: float * ?encoding: string -> float
        abstract lastIndexOf: value: U3<string, float, Buffer> * ?byteOffset: float * ?encoding: string -> float
        abstract entries: unit -> IterableIterator<float * float>
        abstract includes: value: U3<string, float, Buffer> * ?byteOffset: float * ?encoding: string -> bool
        abstract keys: unit -> IterableIterator<float>
        abstract values: unit -> IterableIterator<float>
        abstract from: array: ResizeArray<obj> -> Buffer
        abstract from: arrayBuffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Buffer
        abstract from: buffer: Buffer -> Buffer
        abstract from: str: string * ?encoding: string -> Buffer
        abstract isBuffer: obj: obj -> obj
        abstract isEncoding: encoding: string -> bool
        abstract byteLength: string: string * ?encoding: string -> float
        abstract concat: list: ResizeArray<Buffer> * ?totalLength: float -> Buffer
        abstract compare: buf1: Buffer * buf2: Buffer -> float
        abstract alloc: size: float * ?fill: U3<string, Buffer, float> * ?encoding: string -> Buffer
        abstract allocUnsafe: size: float -> Buffer
        abstract allocUnsafeSlow: size: float -> Buffer
    
    type [<AllowNullLiteral>] BufferStatic =
        [<Emit("new $0($1...)")>] abstract Create: str: string * ?encoding: string -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: str: float -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: Uint8Array -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: arrayBuffer: ArrayBuffer -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: ResizeArray<obj> -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: buffer: Buffer -> Buffer
        abstract from: array: ResizeArray<obj> -> Buffer
        abstract from: buffer: Buffer -> Buffer
        abstract from: arrayBuffer: ArrayBuffer * ?byteOffset: float * ?length: float -> Buffer
        abstract from: str: string * ?encoding: string -> Buffer

    type [<AllowNullLiteral>] SlowBuffer =
        abstract prototype: Buffer with get, set
        abstract isBuffer: obj: obj -> bool
        abstract byteLength: string: string * ?encoding: string -> float
        abstract concat: list: ResizeArray<Buffer> * ?totalLength: float -> Buffer

    type [<AllowNullLiteral>] SlowBufferStatic =
        [<Emit("new $0($1...)")>] abstract Create: str: string * ?encoding: string -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: str: float -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: Uint8Array -> Buffer
        [<Emit("new $0($1...)")>] abstract Create: array: ResizeArray<obj> -> Buffer

    type Globals = 
        abstract Buffer: BufferStatic with get, set
        abstract SlowBuffer: SlowBufferStatic with get, set

[<Import("*", "buffer")>] 
let buffer: buffer_types.Globals = jsNative