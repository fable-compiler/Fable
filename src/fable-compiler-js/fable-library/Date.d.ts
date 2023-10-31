/**
 * DateTimeOffset functions.
 *
 * Note: Date instances are always DateObjects in local
 * timezone (because JS dates are all kinds of messed up).
 * A local date returns UTC epoch when `.getTime()` is called.
 *
 * Basically; invariant: date.getTime() always return UTC time.
 */
import { int64 } from "./BigInt.js";
import { FSharpRef } from "./Types.js";
import { compareDates, DateKind, IDateTime, IDateTimeOffset } from "./Util.js";
export type OffsetInMinutes = number;
export type Offset = "Z" | OffsetInMinutes | null;
export declare function kind(value: IDateTime): number;
export declare function unixEpochMillisecondsToTicks(ms: number, offset: number): int64;
export declare function ticksToUnixEpochMilliseconds(ticks: number | bigint): number;
export declare function dateOffsetToString(offset: number): string;
export declare function dateToHalfUTCString(date: IDateTime, half: "first" | "second"): string;
export declare function toString(date: IDateTime | IDateTimeOffset, format?: string, _provider?: any): string;
export declare function DateTime(value: number, kind?: DateKind): IDateTime;
export declare function fromTicks(ticks: number | bigint, kind?: DateKind): IDateTime;
export declare function fromDateTimeOffset(date: IDateTimeOffset, kind: DateKind): IDateTime;
export declare function getTicks(date: IDateTime | IDateTimeOffset): bigint;
export declare function minValue(): IDateTime;
export declare function maxValue(): IDateTime;
export declare function parseRaw(input: string): [Date, Offset];
export declare function parse(str: string, detectUTC?: boolean): IDateTime;
export declare function tryParse(v: string, defValue: FSharpRef<IDateTime>): boolean;
export declare function create(year: number, month: number, day: number, h?: number, m?: number, s?: number, ms?: number, kind?: DateKind): IDateTime;
export declare function now(): IDateTime;
export declare function utcNow(): IDateTime;
export declare function today(): IDateTime;
export declare function isLeapYear(year: number): boolean;
export declare function daysInMonth(year: number, month: number): 29 | 28 | 31 | 30;
export declare function toUniversalTime(date: IDateTime): IDateTime;
export declare function toLocalTime(date: IDateTime): IDateTime;
export declare function specifyKind(d: IDateTime, kind: DateKind): IDateTime;
export declare function timeOfDay(d: IDateTime): number;
export declare function date(d: IDateTime): IDateTime;
export declare function day(d: IDateTime): number;
export declare function hour(d: IDateTime): number;
export declare function millisecond(d: IDateTime): number;
export declare function minute(d: IDateTime): number;
export declare function month(d: IDateTime): number;
export declare function second(d: IDateTime): number;
export declare function year(d: IDateTime): number;
export declare function dayOfWeek(d: IDateTime): number;
export declare function dayOfYear(d: IDateTime): number;
export declare function add(d: IDateTime, ts: number): IDateTime;
export declare function addDays(d: IDateTime, v: number): IDateTime;
export declare function addHours(d: IDateTime, v: number): IDateTime;
export declare function addMinutes(d: IDateTime, v: number): IDateTime;
export declare function addSeconds(d: IDateTime, v: number): IDateTime;
export declare function addMilliseconds(d: IDateTime, v: number): IDateTime;
export declare function addTicks(d: IDateTime, v: int64): IDateTime;
export declare function addYears(d: IDateTime, v: number): IDateTime;
export declare function addMonths(d: IDateTime, v: number): IDateTime;
export declare function subtract<Input extends number | IDateTime, Output = Input extends number ? IDateTime : number>(d: IDateTime, that: Input): Output;
export declare function toLongDateString(d: IDateTime): string;
export declare function toShortDateString(d: IDateTime): string;
export declare function toLongTimeString(d: IDateTime): string;
export declare function toShortTimeString(d: IDateTime): string;
export declare function equals(d1: IDateTime, d2: IDateTime): boolean;
export declare const compare: typeof compareDates;
export declare const compareTo: typeof compareDates;
export declare function op_Addition(x: IDateTime, y: number): IDateTime;
export declare function op_Subtraction<Input extends number | IDateTime, Output = Input extends number ? IDateTime : number>(x: IDateTime, y: Input): Output;
export declare function isDaylightSavingTime(x: IDateTime): boolean;
export default DateTime;
