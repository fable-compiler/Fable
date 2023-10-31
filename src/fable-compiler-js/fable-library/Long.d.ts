import { FSharpRef } from "./Types.js";
export declare function parse(str: string, style: number, unsigned: boolean, _bitsize: number, radix?: number): bigint;
export declare function tryParse(str: string, style: number, unsigned: boolean, bitsize: number, defValue: FSharpRef<bigint>): boolean;
