import Long, { fromBits } from "./Long";

const littleEndian = true;

export function isLittleEndian() {
  return littleEndian;
}
export function getBytesBoolean(value: boolean) {
  const bytes = new Uint8Array(1);
  new DataView(bytes.buffer).setUint8(0, value ? 1 : 0);
  return bytes;
}
export function getBytesChar(value: string) {
  const bytes = new Uint8Array(2);
  new DataView(bytes.buffer).setUint16(0, value.charCodeAt(0), littleEndian);
  return bytes;
}
export function getBytesInt16(value: number) {
  const bytes = new Uint8Array(2);
  new DataView(bytes.buffer).setInt16(0, value, littleEndian);
  return bytes;
}
export function getBytesInt32(value: number) {
  const bytes = new Uint8Array(4);
  new DataView(bytes.buffer).setInt32(0, value, littleEndian);
  return bytes;
}
export function getBytesInt64(value: Long) {
  const bytes = new Uint8Array(8);
  new DataView(bytes.buffer).setInt32(littleEndian ? 0 : 4, value.getLowBits(), littleEndian);
  new DataView(bytes.buffer).setInt32(littleEndian ? 4 : 0, value.getHighBits(), littleEndian);
  return bytes;
}
export function getBytesUInt16(value: number) {
  const bytes = new Uint8Array(2);
  new DataView(bytes.buffer).setUint16(0, value, littleEndian);
  return bytes;
}
export function getBytesUInt32(value: number) {
  const bytes = new Uint8Array(4);
  new DataView(bytes.buffer).setUint32(0, value, littleEndian);
  return bytes;
}
export function getBytesUInt64(value: Long) {
  const bytes = new Uint8Array(8);
  new DataView(bytes.buffer).setUint32(littleEndian ? 0 : 4, value.getLowBitsUnsigned(), littleEndian);
  new DataView(bytes.buffer).setUint32(littleEndian ? 4 : 0, value.getHighBitsUnsigned(), littleEndian);
  return bytes;
}
export function getBytesSingle(value: number) {
  const bytes = new Uint8Array(4);
  new DataView(bytes.buffer).setFloat32(0, value, littleEndian);
  return bytes;
}
export function getBytesDouble(value: number) {
  const bytes = new Uint8Array(8);
  new DataView(bytes.buffer).setFloat64(0, value, littleEndian);
  return bytes;
}
export function int64BitsToDouble(value: Long) {
  const buffer = new ArrayBuffer(8);
  new DataView(buffer).setInt32(littleEndian ? 0 : 4, value.getLowBits(), littleEndian);
  new DataView(buffer).setInt32(littleEndian ? 4 : 0, value.getHighBits(), littleEndian);
  return new DataView(buffer).getFloat64(0, littleEndian);
}
export function doubleToInt64Bits(value: number) {
  const buffer = new ArrayBuffer(8);
  new DataView(buffer).setFloat64(0, value, littleEndian);
  const lowBits = new DataView(buffer).getInt32(littleEndian ? 0 : 4, littleEndian);
  const highBits = new DataView(buffer).getInt32(littleEndian ? 4 : 0, littleEndian);
  return fromBits(lowBits, highBits, false);
}
export function toBoolean(bytes: Uint8Array, offset: number): boolean {
  return new DataView(bytes.buffer).getUint8(offset) === 1 ? true : false;
}
export function toChar(bytes: Uint8Array, offset: number) {
  const code = new DataView(bytes.buffer).getUint16(offset, littleEndian);
  return String.fromCharCode(code);
}
export function toInt16(bytes: Uint8Array, offset: number) {
  return new DataView(bytes.buffer).getInt16(offset, littleEndian);
}
export function toInt32(bytes: Uint8Array, offset: number) {
  return new DataView(bytes.buffer).getInt32(offset, littleEndian);
}
export function toInt64(bytes: Uint8Array, offset: number) {
  const lowBits = new DataView(bytes.buffer).getInt32(offset + (littleEndian ? 0 : 4), littleEndian);
  const highBits = new DataView(bytes.buffer).getInt32(offset + (littleEndian ? 4 : 0), littleEndian);
  return fromBits(lowBits, highBits, false);
}
export function toUInt16(bytes: Uint8Array, offset: number) {
  return new DataView(bytes.buffer).getUint16(offset, littleEndian);
}
export function toUInt32(bytes: Uint8Array, offset: number) {
  return new DataView(bytes.buffer).getUint32(offset, littleEndian);
}
export function toUInt64(bytes: Uint8Array, offset: number) {
  const lowBits = new DataView(bytes.buffer).getUint32(offset + (littleEndian ? 0 : 4), littleEndian);
  const highBits = new DataView(bytes.buffer).getUint32(offset + (littleEndian ? 4 : 0), littleEndian);
  return fromBits(lowBits, highBits, true);
}
export function toSingle(bytes: Uint8Array, offset: number) {
  return new DataView(bytes.buffer).getFloat32(offset, littleEndian);
}
export function toDouble(bytes: Uint8Array, offset: number) {
  return new DataView(bytes.buffer).getFloat64(offset, littleEndian);
}
export function toString(bytes: Uint8Array, offset?: number, count?: number) {
  let ar = bytes;
  if (typeof offset !== "undefined" && typeof count !== "undefined") {
    ar = bytes.subarray(offset, offset + count);
  } else if (typeof offset !== "undefined") {
    ar = bytes.subarray(offset);
  }
  return Array.from(ar).map((b) => ("0" + b.toString(16)).slice(-2)).join("-");
}
