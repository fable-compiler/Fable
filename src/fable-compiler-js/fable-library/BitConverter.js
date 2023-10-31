const littleEndian = true;
export function isLittleEndian() {
    return littleEndian;
}
export function getBytesBoolean(value) {
    const bytes = new Uint8Array(1);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setUint8(0, value ? 1 : 0);
    return bytes;
}
export function getBytesChar(value) {
    const bytes = new Uint8Array(2);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setUint16(0, value.charCodeAt(0), littleEndian);
    return bytes;
}
export function getBytesInt16(value) {
    const bytes = new Uint8Array(2);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setInt16(0, value, littleEndian);
    return bytes;
}
export function getBytesInt32(value) {
    const bytes = new Uint8Array(4);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setInt32(0, value, littleEndian);
    return bytes;
}
export function getBytesInt64(value) {
    const bytes = new Uint8Array(8);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setBigInt64(0, value, littleEndian);
    return bytes;
}
export function getBytesUInt16(value) {
    const bytes = new Uint8Array(2);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setUint16(0, value, littleEndian);
    return bytes;
}
export function getBytesUInt32(value) {
    const bytes = new Uint8Array(4);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setUint32(0, value, littleEndian);
    return bytes;
}
export function getBytesUInt64(value) {
    const bytes = new Uint8Array(8);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setBigUint64(0, value, littleEndian);
    return bytes;
}
export function getBytesSingle(value) {
    const bytes = new Uint8Array(4);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setFloat32(0, value, littleEndian);
    return bytes;
}
export function getBytesDouble(value) {
    const bytes = new Uint8Array(8);
    const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
    view.setFloat64(0, value, littleEndian);
    return bytes;
}
export function int64BitsToDouble(value) {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    view.setBigInt64(0, value, littleEndian);
    return view.getFloat64(0, littleEndian);
}
export function doubleToInt64Bits(value) {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer);
    view.setFloat64(0, value, littleEndian);
    return view.getBigInt64(0, littleEndian);
}
export function toBoolean(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getUint8(offset) === 1 ? true : false;
}
export function toChar(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    const code = view.getUint16(offset, littleEndian);
    return String.fromCharCode(code);
}
export function toInt16(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getInt16(offset, littleEndian);
}
export function toInt32(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getInt32(offset, littleEndian);
}
export function toInt64(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getBigInt64(offset, littleEndian);
}
export function toUInt16(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getUint16(offset, littleEndian);
}
export function toUInt32(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getUint32(offset, littleEndian);
}
export function toUInt64(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getBigUint64(offset, littleEndian);
}
export function toSingle(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getFloat32(offset, littleEndian);
}
export function toDouble(bytes, offset) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
    return view.getFloat64(offset, littleEndian);
}
export function toString(bytes, offset, count) {
    const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
    let buffer = new Uint8Array(array.buffer, array.byteOffset, array.byteLength);
    if (offset != null && count != null) {
        buffer = buffer.subarray(offset, offset + count);
    }
    else if (offset != null) {
        buffer = buffer.subarray(offset);
    }
    return Array.from(buffer).map((b) => ("0" + b.toString(16)).slice(-2)).join("-");
}
