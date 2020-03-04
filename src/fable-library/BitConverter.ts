import { uint8 } from "./Int32";
import Long, { fromBits, getHighBits, getHighBitsUnsigned, getLowBits, getLowBitsUnsigned } from "./Long";

const littleEndian = true;

export function isLittleEndian() {
  return littleEndian;
}

export function getBytesBoolean(value: boolean) {
  const bytes = new Uint8Array(1);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint8(0, value ? 1 : 0);
  return bytes;
}

export function getBytesChar(value: string) {
  const bytes = new Uint8Array(2);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint16(0, value.charCodeAt(0), littleEndian);
  return bytes;
}

export function getBytesInt16(value: number) {
  const bytes = new Uint8Array(2);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setInt16(0, value, littleEndian);
  return bytes;
}

export function getBytesInt32(value: number) {
  const bytes = new Uint8Array(4);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setInt32(0, value, littleEndian);
  return bytes;
}

export function getBytesInt64(value: Long) {
  const bytes = new Uint8Array(8);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setInt32(littleEndian ? 0 : 4, getLowBits(value), littleEndian);
  view.setInt32(littleEndian ? 4 : 0, getHighBits(value), littleEndian);
  return bytes;
}

export function getBytesUInt16(value: number) {
  const bytes = new Uint8Array(2);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint16(0, value, littleEndian);
  return bytes;
}

export function getBytesUInt32(value: number) {
  const bytes = new Uint8Array(4);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint32(0, value, littleEndian);
  return bytes;
}

export function getBytesUInt64(value: Long) {
  const bytes = new Uint8Array(8);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setUint32(littleEndian ? 0 : 4, getLowBitsUnsigned(value), littleEndian);
  view.setUint32(littleEndian ? 4 : 0, getHighBitsUnsigned(value), littleEndian);
  return bytes;
}

export function getBytesSingle(value: number) {
  const bytes = new Uint8Array(4);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setFloat32(0, value, littleEndian);
  return bytes;
}

export function getBytesDouble(value: number) {
  const bytes = new Uint8Array(8);
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  view.setFloat64(0, value, littleEndian);
  return bytes;
}

export function int64BitsToDouble(value: Long) {
  const buffer = new ArrayBuffer(8);
  const view = new DataView(buffer);
  view.setInt32(littleEndian ? 0 : 4, getLowBits(value), littleEndian);
  view.setInt32(littleEndian ? 4 : 0, getHighBits(value), littleEndian);
  return view.getFloat64(0, littleEndian);
}

export function doubleToInt64Bits(value: number) {
  const buffer = new ArrayBuffer(8);
  const view = new DataView(buffer);
  view.setFloat64(0, value, littleEndian);
  const lowBits = view.getInt32(littleEndian ? 0 : 4, littleEndian);
  const highBits = view.getInt32(littleEndian ? 4 : 0, littleEndian);
  return fromBits(lowBits, highBits, false);
}

export function toBoolean(bytes: ArrayLike<uint8>, offset: number): boolean {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getUint8(offset) === 1 ? true : false;
}

export function toChar(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  const code = view.getUint16(offset, littleEndian);
  return String.fromCharCode(code);
}

export function toInt16(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getInt16(offset, littleEndian);
}

export function toInt32(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getInt32(offset, littleEndian);
}

export function toInt64(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  const lowBits = view.getInt32(offset + (littleEndian ? 0 : 4), littleEndian);
  const highBits = view.getInt32(offset + (littleEndian ? 4 : 0), littleEndian);
  return fromBits(lowBits, highBits, false);
}

export function toUInt16(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getUint16(offset, littleEndian);
}

export function toUInt32(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getUint32(offset, littleEndian);
}

export function toUInt64(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  const lowBits = view.getUint32(offset + (littleEndian ? 0 : 4), littleEndian);
  const highBits = view.getUint32(offset + (littleEndian ? 4 : 0), littleEndian);
  return fromBits(lowBits, highBits, true);
}

export function toSingle(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getFloat32(offset, littleEndian);
}

export function toDouble(bytes: ArrayLike<uint8>, offset: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  const view = new DataView(array.buffer, array.byteOffset, array.byteLength);
  return view.getFloat64(offset, littleEndian);
}

export function toString(bytes: ArrayLike<uint8>, offset?: number, count?: number) {
  const array = ArrayBuffer.isView(bytes) ? bytes : Uint8Array.from(bytes);
  let buffer = Buffer.from(array.buffer, array.byteOffset, array.byteLength);
  if (offset != null && count != null) {
    buffer = buffer.subarray(offset, offset + count);
  } else if (offset != null) {
    buffer = buffer.subarray(offset);
  }
  return Array.from(buffer).map((b) => ("0" + b.toString(16)).slice(-2)).join("-");
}
