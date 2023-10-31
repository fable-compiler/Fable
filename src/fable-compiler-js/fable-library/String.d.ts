export declare function compare(...args: any[]): number;
export declare function compareOrdinal(x: string, y: string): number;
export declare function compareTo(x: string, y: string): number;
export declare function startsWith(str: string, pattern: string, ic: number): boolean;
export declare function indexOfAny(str: string, anyOf: string[], ...args: number[]): number;
export type IPrintfFormatContinuation = (f: (x: string) => any) => any;
export interface IPrintfFormat {
    input: string;
    cont: IPrintfFormatContinuation;
}
export declare function printf(input: string): IPrintfFormat;
export declare function interpolate(str: string, values: any[]): string;
export declare function toConsole(arg: IPrintfFormat | string): any;
export declare function toConsoleError(arg: IPrintfFormat | string): any;
export declare function toText(arg: IPrintfFormat | string): any;
export declare function toFail(arg: IPrintfFormat | string): any;
export declare function fsFormat(str: string): (cont: (...args: any[]) => any) => any;
export declare function format(str: string | object, ...args: any[]): string;
export declare function endsWith(str: string, search: string): boolean;
export declare function initialize(n: number, f: (i: number) => string): string;
export declare function insert(str: string, startIndex: number, value: string): string;
export declare function isNullOrEmpty(str: string | any): boolean;
export declare function isNullOrWhiteSpace(str: string | any): boolean;
export declare function concat(...xs: any[]): string;
export declare function join<T>(delimiter: string, xs: Iterable<T>): string;
export declare function joinWithIndices(delimiter: string, xs: string[], startIndex: number, count: number): string;
export declare function toBase64String(inArray: ArrayLike<number>): string;
export declare function fromBase64String(b64Encoded: string): number[];
export declare function padLeft(str: string, len: number, ch?: string): string;
export declare function padRight(str: string, len: number, ch?: string): string;
export declare function remove(str: string, startIndex: number, count?: number): string;
export declare function replace(str: string, search: string, replace: string): string;
export declare function replicate(n: number, x: string): string;
export declare function getCharAtIndex(input: string, index: number): string;
export declare function split(str: string, splitters: string[], count?: number, options?: number): string[];
export declare function trim(str: string, ...chars: string[]): string;
export declare function trimStart(str: string, ...chars: string[]): any;
export declare function trimEnd(str: string, ...chars: string[]): any;
export declare function filter(pred: (char: string) => boolean, x: string): string;
export declare function substring(str: string, startIndex: number, length?: number): string;
interface FormattableString {
    strs: TemplateStringsArray;
    args: any[];
    fmts?: string[];
}
export declare function fmt(strs: TemplateStringsArray, ...args: any[]): FormattableString;
export declare function fmtWith(fmts: string[]): (strs: TemplateStringsArray, ...args: any[]) => FormattableString;
export declare function getFormat(s: FormattableString): string;
export {};
