import { uint8 } from "./Int32.js";
declare class UTF16LE {
    getBytes(str: string | string[], index?: number, count?: number): Uint8Array;
    getString(bytes: ArrayLike<uint8>, index?: number, count?: number): string;
}
declare class UTF8 {
    getBytes(str: string | string[], index?: number, count?: number): Uint8Array;
    getString(bytes: ArrayLike<uint8>, index?: number, count?: number): string;
}
export declare function get_Unicode(): UTF16LE;
export declare function get_UTF8(): UTF8;
export {};
