import { IComparable } from "./Util.js";
export declare const symbol: unique symbol;
export interface CustomNumeric extends IComparable<Numeric> {
    [symbol](): {
        multiply(y: Numeric): Numeric;
        toPrecision(sd?: number): string;
        toExponential(dp?: number): string;
        toFixed(dp?: number): string;
        toHex(): string;
    };
}
export type Numeric = number | bigint | CustomNumeric;
export declare function isNumeric(x: any): any;
export declare function compare(x: Numeric, y: number): number;
export declare function multiply(x: Numeric, y: number): Numeric;
export declare function toFixed(x: Numeric, dp?: number): string | bigint;
export declare function toPrecision(x: Numeric, sd?: number): string | bigint;
export declare function toExponential(x: Numeric, dp?: number): string | bigint;
export declare function toHex(x: Numeric): string;
