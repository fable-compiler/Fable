/**
 * DateTimeOffset functions.
 *
 * Note: DateOffset instances are always DateObjects in local
 * timezone (because JS dates are all kinds of messed up).
 * A local date returns UTC epoch when `.getTime()` is called.
 *
 * However, this means that in order to construct an UTC date
 * from a DateOffset with offset of +5 hours, you first need
 * to subtract those 5 hours, than add the "local" offset.
 * As said, all kinds of messed up.
 *
 * Basically; invariant: date.getTime() always return UTC time.
 */
import { int64 } from "./BigInt.js";
import { FSharpRef } from "./Types.js";
import { IDateTime, IDateTimeOffset } from "./Util.js";
export default function DateTimeOffset(value: number, offset?: number): IDateTimeOffset;
export declare function offset(value: IDateTimeOffset): number;
export declare function fromDate(date: IDateTime, offset?: number): IDateTimeOffset;
export declare function fromTicks(ticks: int64, offset: number): IDateTimeOffset;
export declare function fromUnixTimeMilliseconds(ms: int64): IDateTimeOffset;
export declare function fromUnixTimeSeconds(seconds: int64): IDateTimeOffset;
export declare function getUtcTicks(date: IDateTimeOffset): bigint;
export declare function minValue(): IDateTimeOffset;
export declare function maxValue(): IDateTimeOffset;
export declare function parse(str: string): IDateTimeOffset;
export declare function tryParse(v: string, defValue: FSharpRef<IDateTimeOffset>): boolean;
export declare function create(year: number, month: number, day: number, h: number, m: number, s: number, ms: number, offset?: number): IDateTimeOffset;
export declare function now(): IDateTimeOffset;
export declare function utcNow(): IDateTimeOffset;
export declare function toUniversalTime(date: IDateTimeOffset): Date;
export declare function toLocalTime(date: IDateTimeOffset): Date;
export declare function timeOfDay(d: IDateTimeOffset): number;
export declare function date(d: IDateTimeOffset): IDateTime;
export declare function day(d: IDateTimeOffset): number;
export declare function hour(d: IDateTimeOffset): number;
export declare function millisecond(d: IDateTimeOffset): number;
export declare function minute(d: IDateTimeOffset): number;
export declare function month(d: IDateTimeOffset): number;
export declare function second(d: IDateTimeOffset): number;
export declare function year(d: IDateTimeOffset): number;
export declare function dayOfWeek(d: IDateTimeOffset): number;
export declare function dayOfYear(d: IDateTimeOffset): number;
export declare function add(d: IDateTimeOffset, ts: number): IDateTimeOffset;
export declare function addDays(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function addHours(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function addMinutes(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function addSeconds(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function addMilliseconds(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function addTicks(d: IDateTimeOffset, v: int64): IDateTimeOffset;
export declare function addYears(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function addMonths(d: IDateTimeOffset, v: number): IDateTimeOffset;
export declare function subtract<Input extends number | IDateTimeOffset, Output = Input extends number ? IDateTimeOffset : number>(d: IDateTimeOffset, that: Input): Output;
export declare function equals(d1: IDateTimeOffset, d2: IDateTimeOffset): boolean;
export declare function equalsExact(d1: IDateTimeOffset, d2: IDateTimeOffset): boolean;
export declare function compare(d1: IDateTimeOffset, d2: IDateTimeOffset): 0 | 1 | -1;
export declare const compareTo: typeof compare;
export declare function op_Addition(x: IDateTimeOffset, y: number): IDateTimeOffset;
export declare function op_Subtraction<Input extends number | IDateTimeOffset, Output = Input extends number ? IDateTimeOffset : number>(x: IDateTimeOffset, y: Input): Output;
export declare function toOffset(d: IDateTimeOffset, offset: number): IDateTimeOffset;
export declare function toUnixTimeMilliseconds(d: IDateTimeOffset): int64;
export declare function toUnixTimeSeconds(d: IDateTimeOffset): int64;
