import { FSharpRef } from "./Types.js";
export declare function toString(str: string, format?: string, _provider?: any): string;
/** Validates UUID as specified in RFC4122 (versions 1-5). */
export declare function parse(str: string): string;
export declare function tryParse(str: string, defValue: FSharpRef<string>): boolean;
export declare function newGuid(): string;
/** Parse a UUID into it's component bytes */
export declare function guidToArray(s: string): number[];
/** Convert UUID byte array into a string */
export declare function arrayToGuid(buf: ArrayLike<number>): string;
