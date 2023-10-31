import { FSharpRef } from "./Types.js";
export type int8 = number;
export type uint8 = number;
export type int16 = number;
export type uint16 = number;
export type int32 = number;
export type uint32 = number;
export type float16 = number;
export type float32 = number;
export type float64 = number;
export declare enum NumberStyles {
    AllowHexSpecifier = 512
}
export declare function getPrefix(radix: number): string;
export declare function getRadix(prefix: string, style: number): 2 | 8 | 16 | 10;
export declare function isValid(str: string, style: number, radix?: number): {
    sign: string;
    prefix: string;
    digits: string;
    radix: number;
} | null;
export declare function parse(str: string, style: number, unsigned: boolean, bitsize: number, radix?: number): number;
export declare function tryParse(str: string, style: number, unsigned: boolean, bitsize: number, defValue: FSharpRef<number>): boolean;
export declare function op_UnaryNegation_Int8(x: number): number;
export declare function op_UnaryNegation_Int16(x: number): number;
export declare function op_UnaryNegation_Int32(x: number): number;
export declare function divRem(x: number, y: number): [number, number];
export declare function divRem(x: number, y: number, out: FSharpRef<number>): number;
