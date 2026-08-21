/**
 * DateTime as an extended Temporal.PlainDateTime.
 *
 * .NET DateTime is a wall-clock date+time (no offset) plus a Kind (Utc | Local |
 * Unspecified) that is metadata only. PlainDateTime models the wall-clock exactly
 * and, unlike the JS Date representation, is tick (100ns) precise and DST-agnostic
 * for arithmetic — which matches .NET, where DateTime math operates on the raw
 * wall-clock ticks.
 *
 * Kind has no Temporal home, so it is attached to the instance as a `kind` property
 * (Temporal objects are extensible). Because Temporal operations return fresh,
 * un-stamped instances, every operation must funnel through `dateTime(...)` to
 * re-stamp the kind.
 *
 * toString / parse / ToUniversalTime / ToLocalTime bridge to the JS-Date-based
 * Date.ts: those paths are host-timezone- and culture-sensitive and their exact
 * behavior is pinned by the test suite, so we reuse them rather than reimplement.
 */

import { int64, toInt64 } from "./BigInt.ts";
import { FSharpRef } from "./Types.ts";
import { DateTimeKind, IDateTime } from "./Util.ts";
import {
  TimeSpan, fromTicks as TimeSpan_fromTicks, totalNanoseconds as TimeSpan_totalNanoseconds,
} from "./TimeSpanTemporal.ts";
import {
  create as Date_create,
  toString as Date_toString,
  parse as Date_parse,
  toUniversalTime as Date_toUniversalTime,
  toLocalTime as Date_toLocalTime,
  isDaylightSavingTime as Date_isDaylightSavingTime,
  toLongDateString as Date_toLongDateString,
  toShortDateString as Date_toShortDateString,
  toLongTimeString as Date_toLongTimeString,
  toShortTimeString as Date_toShortTimeString,
  year as Date_year, month as Date_month, day as Date_day,
  hour as Date_hour, minute as Date_minute, second as Date_second, millisecond as Date_millisecond,
} from "./Date.ts";

declare global {
  namespace Temporal {
    class PlainDateTime {
      constructor(isoYear: number, isoMonth: number, isoDay: number, hour?: number, minute?: number, second?: number,
        millisecond?: number, microsecond?: number, nanosecond?: number);
      static compare(one: PlainDateTime, two: PlainDateTime): number;
      readonly year: number;
      readonly month: number;
      readonly day: number;
      readonly hour: number;
      readonly minute: number;
      readonly second: number;
      readonly millisecond: number;
      readonly microsecond: number;
      readonly nanosecond: number;
      readonly dayOfWeek: number;
      readonly dayOfYear: number;
      add(duration: Duration | DurationLike): PlainDateTime;
      subtract(duration: Duration | DurationLike): PlainDateTime;
      until(other: PlainDateTime, options?: { largestUnit?: string }): Duration;
      with(fields: {
        year?: number, month?: number, day?: number, hour?: number, minute?: number, second?: number,
        millisecond?: number, microsecond?: number, nanosecond?: number,
      }): PlainDateTime;
      round(options: { smallestUnit: string, roundingIncrement?: number, roundingMode?: string }): PlainDateTime;
      equals(other: PlainDateTime): boolean;
    }
    interface DurationLike {
      years?: number; months?: number; days?: number; hours?: number; minutes?: number;
      seconds?: number; milliseconds?: number; microseconds?: number; nanoseconds?: number;
    }
    namespace Now {
      function plainDateTimeISO(timeZone?: string): PlainDateTime;
    }
  }
}

export type DateTime = Temporal.PlainDateTime & { kind?: DateTimeKind };

export const PlainDateTime = Temporal.PlainDateTime;
export type PlainDateTime = Temporal.PlainDateTime;

// Generic equality/comparison/hashing dispatch (Util.ts) uses .NET-style methods.
const proto = Temporal.PlainDateTime.prototype as any;
proto.Equals = function (this: DateTime, other: DateTime): boolean { return Temporal.PlainDateTime.compare(this, other) === 0; };
proto.CompareTo = function (this: DateTime, other: DateTime): number { return Temporal.PlainDateTime.compare(this, other); };
proto.GetHashCode = function (this: DateTime): number { return hash(this); };
// Lets String.Format format this value without String.ts importing this module (see String.ts)
proto[Symbol.for("Fable.DateTimeFormattable")] = function (this: DateTime, format?: string): string { return toString(this, format); };

const minDateTime = new Temporal.PlainDateTime(1, 1, 1);

export function getKind(value: DateTime): DateTimeKind {
  return value.kind ?? DateTimeKind.Unspecified;
}

export function dateTime(value: Temporal.PlainDateTime, kind: DateTimeKind = DateTimeKind.Unspecified): DateTime {
  const d = value as DateTime;
  d.kind = kind;
  return d;
}

// --- Bridge to the JS-Date representation for host-timezone / culture-sensitive paths ---
function toJsDateTime(d: DateTime): IDateTime {
  return Date_create(d.year, d.month, d.day, d.hour, d.minute, d.second, d.millisecond, getKind(d));
}

function fromJsDateTime(d: IDateTime): DateTime {
  return dateTime(
    new Temporal.PlainDateTime(Date_year(d), Date_month(d), Date_day(d), Date_hour(d), Date_minute(d), Date_second(d), Date_millisecond(d)),
    d.kind ?? DateTimeKind.Unspecified);
}

export function create(
  year: number, month: number, day: number,
  h: number = 0, m: number = 0, s: number = 0,
  ms: number = 0, kind?: DateTimeKind): DateTime {
  return dateTime(new Temporal.PlainDateTime(year, month, day, h, m, s, ms), kind);
}

export function fromTicks(ticks: number | bigint, kind?: DateTimeKind): DateTime {
  return dateTime(minDateTime.add(TimeSpan_fromTicks(ticks)), kind);
}

export function getTicks(date: DateTime): int64 {
  return toInt64(TimeSpan_totalNanoseconds(minDateTime.until(date, { largestUnit: "day" })) / 100n);
}

export function minValue(): DateTime {
  return dateTime(minDateTime, DateTimeKind.Unspecified);
}

export function maxValue(): DateTime {
  return dateTime(new Temporal.PlainDateTime(9999, 12, 31, 23, 59, 59, 999, 999, 900), DateTimeKind.Unspecified);
}

// .NET DateTime has 100ns (tick) precision; Temporal.Now is nanosecond-precise.
function truncateToTicks(d: Temporal.PlainDateTime): Temporal.PlainDateTime {
  return d.round({ smallestUnit: "nanosecond", roundingIncrement: 100, roundingMode: "trunc" });
}

export function now(): DateTime {
  return dateTime(truncateToTicks(Temporal.Now.plainDateTimeISO()), DateTimeKind.Local);
}

export function utcNow(): DateTime {
  return dateTime(truncateToTicks(Temporal.Now.plainDateTimeISO("UTC")), DateTimeKind.Utc);
}

export function today(): DateTime {
  return date(now());
}

export function specifyKind(d: DateTime, kind: DateTimeKind): DateTime {
  return create(d.year, d.month, d.day, d.hour, d.minute, d.second, d.millisecond, kind);
}

export function dayOfWeek(d: DateTime): number {
  // Temporal: Monday = 1 ... Sunday = 7, .NET: Sunday = 0 ... Saturday = 6
  return d.dayOfWeek % 7;
}

export function date(d: DateTime): DateTime {
  return dateTime(d.with({ hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }), getKind(d));
}

export function timeOfDay(d: DateTime): TimeSpan {
  // Elapsed since midnight. DateTime values are tick-aligned, so this is too.
  return d.with({ hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 }).until(d);
}

export function add(d: DateTime, ts: TimeSpan): DateTime {
  return dateTime(d.add(ts), getKind(d));
}

export function addYears(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ years: v }), getKind(d));
}

export function addMonths(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ months: v }), getKind(d));
}

export function addDays(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ days: v }), getKind(d));
}

export function addHours(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ hours: v }), getKind(d));
}

export function addMinutes(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ minutes: v }), getKind(d));
}

export function addSeconds(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ seconds: v }), getKind(d));
}

export function addMilliseconds(d: DateTime, v: number): DateTime {
  return dateTime(d.add({ milliseconds: v }), getKind(d));
}

export function addTicks(d: DateTime, v: int64): DateTime {
  return dateTime(d.add(TimeSpan_fromTicks(v)), getKind(d));
}

export function subtract(d: DateTime, that: DateTime | TimeSpan): DateTime | TimeSpan {
  return that instanceof Temporal.PlainDateTime
    ? that.until(d, { largestUnit: "day" }) as TimeSpan // DateTime - DateTime -> TimeSpan
    : dateTime(d.subtract(that), getKind(d));
}

export function equals(d1: DateTime, d2: DateTime): boolean {
  return Temporal.PlainDateTime.compare(d1, d2) === 0;
}

export function compare(d1: DateTime, d2: DateTime): number {
  return Temporal.PlainDateTime.compare(d1, d2);
}

export const compareTo = compare;

export function op_Addition(x: DateTime, y: TimeSpan): DateTime {
  return add(x, y);
}

export function op_Subtraction(x: DateTime, y: DateTime | TimeSpan): DateTime | TimeSpan {
  return subtract(x, y);
}

export function hash(d: DateTime): number {
  return Number(getTicks(d) % 2147483647n);
}

export function isLeapYear(year: number): boolean {
  return year % 4 === 0 && year % 100 !== 0 || year % 400 === 0;
}

export function daysInMonth(year: number, month: number): number {
  return month === 2
    ? (isLeapYear(year) ? 29 : 28)
    : (month >= 8 ? (month % 2 === 0 ? 31 : 30) : (month % 2 === 0 ? 30 : 31));
}

// --- Host-timezone / culture-sensitive: bridge to Date.ts ---
export function toUniversalTime(d: DateTime): DateTime {
  return getKind(d) === DateTimeKind.Utc ? d : fromJsDateTime(Date_toUniversalTime(toJsDateTime(d)));
}

export function toLocalTime(d: DateTime): DateTime {
  return getKind(d) === DateTimeKind.Local ? d : fromJsDateTime(Date_toLocalTime(toJsDateTime(d)));
}

export function isDaylightSavingTime(d: DateTime): boolean {
  return Date_isDaylightSavingTime(toJsDateTime(d));
}

export function toLongDateString(d: DateTime): string {
  return Date_toLongDateString(toJsDateTime(d));
}

export function toShortDateString(d: DateTime): string {
  return Date_toShortDateString(toJsDateTime(d));
}

export function toLongTimeString(d: DateTime): string {
  return Date_toLongTimeString(toJsDateTime(d));
}

export function toShortTimeString(d: DateTime): string {
  return Date_toShortTimeString(toJsDateTime(d));
}

export function toString(d: DateTime, format?: string, _provider?: any): string {
  return Date_toString(toJsDateTime(d), format);
}

export function parse(str: string, detectUTC = false): DateTime {
  return fromJsDateTime(Date_parse(str, detectUTC));
}

export function tryParse(v: string, defValue: FSharpRef<DateTime>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
