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
 * Formatting, parsing and time-zone conversion are done on the Temporal value
 * itself (see DateTimeFormat.ts), never by round-tripping through a JS Date —
 * that would silently truncate every value to milliseconds. The one exception is
 * the four ToLong/ShortDate/TimeString members, which are host-locale display
 * strings with no .NET-defined layout and no sub-second component.
 */

import { int64, toInt64 } from "./BigInt.ts";
import { FSharpRef } from "./Types.ts";
import { DateTimeKind } from "./Util.ts";
import {
  TimeSpan, fromTicks as TimeSpan_fromTicks, fromUnits as TimeSpan_fromUnits,
  totalNanoseconds as TimeSpan_totalNanoseconds,
} from "./TimeSpanTemporal.ts";
import * as Format from "./DateTimeFormat.ts";

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

// `dateTime` stamps in place, so anything handing out a value derived from a
// shared instance (a module constant, or the argument of SpecifyKind) has to
// copy first, or it would mutate the caller's value.
function copy(d: Temporal.PlainDateTime): Temporal.PlainDateTime {
  return new Temporal.PlainDateTime(
    d.year, d.month, d.day, d.hour, d.minute, d.second, d.millisecond, d.microsecond, d.nanosecond);
}

function hostTimeZone(): string {
  return Temporal.Now.timeZoneId();
}

// Host-zone offset at this wall-clock, e.g. "+01:00". Unlike building a JS Date
// from the fields, this is correct for years 0-99 (which JS maps to 1900-1999).
function hostOffsetString(d: Temporal.PlainDateTime): string {
  return d.toZonedDateTime(hostTimeZone()).offset;
}

// Wall-clock fields for the formatter. The sub-second component is carried in
// .NET ticks so that "O" and the f/F specifiers can print all 7 digits.
function toDateInfo(d: DateTime): Format.DateInfo {
  return {
    year: d.year,
    month: d.month,
    day: d.day,
    hour: d.hour,
    minute: d.minute,
    second: d.second,
    tick: d.millisecond * 10_000 + d.microsecond * 10 + Math.trunc(d.nanosecond / 100),
    dayOfWeek: dayOfWeek(d),
  };
}

// Display-only bridge for the host-locale strings below. Sub-second precision is
// irrelevant to all four, but years 0-99 still need correcting.
function toJsDate(d: DateTime): Date {
  const jsDate = new Date(d.year, d.month - 1, d.day, d.hour, d.minute, d.second, d.millisecond);
  jsDate.setFullYear(d.year, d.month - 1, d.day);
  return jsDate;
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
  return dateTime(copy(minDateTime), DateTimeKind.Unspecified);
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
  // Kind is metadata: every tick of the wall-clock is preserved.
  return dateTime(copy(d), kind);
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
  return dateTime(d.add(TimeSpan_fromUnits(v, "days")), getKind(d));
}

export function addHours(d: DateTime, v: number): DateTime {
  return dateTime(d.add(TimeSpan_fromUnits(v, "hours")), getKind(d));
}

export function addMinutes(d: DateTime, v: number): DateTime {
  return dateTime(d.add(TimeSpan_fromUnits(v, "minutes")), getKind(d));
}

export function addSeconds(d: DateTime, v: number): DateTime {
  return dateTime(d.add(TimeSpan_fromUnits(v, "seconds")), getKind(d));
}

export function addMilliseconds(d: DateTime, v: number): DateTime {
  return dateTime(d.add(TimeSpan_fromUnits(v, "milliseconds")), getKind(d));
}

export function addTicks(d: DateTime, v: int64): DateTime {
  return dateTime(d.add(TimeSpan_fromTicks(v)), getKind(d));
}

export function subtractDate(d: DateTime, that: DateTime): TimeSpan {
  return that.until(d, { largestUnit: "day" });
}

export function subtractTimeSpan(d: DateTime, ts: TimeSpan): DateTime {
  return dateTime(d.subtract(ts), getKind(d));
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

// --- Host time zone ---

export function toUniversalTime(d: DateTime): DateTime {
  // .NET reads an Unspecified value as local time here.
  return getKind(d) === DateTimeKind.Utc
    ? d
    : dateTime(d.toZonedDateTime(hostTimeZone()).withTimeZone("UTC").toPlainDateTime(), DateTimeKind.Utc);
}

export function toLocalTime(d: DateTime): DateTime {
  // .NET reads an Unspecified value as UTC here — the mirror image of the above.
  return getKind(d) === DateTimeKind.Local
    ? d
    : dateTime(d.toZonedDateTime("UTC").withTimeZone(hostTimeZone()).toPlainDateTime(), DateTimeKind.Local);
}

export function isDaylightSavingTime(d: DateTime): boolean {
  const tz = hostTimeZone();
  const offsetAt = (month: number) => new Temporal.PlainDateTime(d.year, month, 1).toZonedDateTime(tz).offsetNanoseconds;
  // The larger of the two mid-season offsets is the daylight-saving one.
  return Math.max(offsetAt(1), offsetAt(7)) === d.toZonedDateTime(tz).offsetNanoseconds;
}

// --- Host-locale display strings (no .NET-defined layout) ---

export function toLongDateString(d: DateTime): string {
  return toJsDate(d).toDateString();
}

export function toShortDateString(d: DateTime): string {
  return toJsDate(d).toLocaleDateString();
}

export function toLongTimeString(d: DateTime): string {
  return toJsDate(d).toLocaleTimeString();
}

export function toShortTimeString(d: DateTime): string {
  return toJsDate(d).toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}

// --- Formatting and parsing ---

export function toString(d: DateTime, format?: string, _provider?: any): string {
  const kind = getKind(d);

  return Format.dateToString(
    {
      info: toDateInfo(d),
      utcInfo: () => toDateInfo(toUniversalTime(d)),
      // An Unspecified DateTime prints no zone at all, which is what makes "O"
      // round-trip it back to Unspecified.
      roundTripSuffix: () => kind === DateTimeKind.Utc
        ? "Z"
        : kind === DateTimeKind.Local ? hostOffsetString(d) : "",
      defaultSuffix: "",
      kind,
      hostOffsetString: () => hostOffsetString(d),
    },
    format);
}

export function parse(str: string, detectUTC = false): DateTime {
  const [parsed, offset] = Format.parseRaw(str);
  // .NET always parses DateTime as Local if there's offset info (even "Z")
  // Newtonsoft.Json uses UTC if the offset is "Z"
  const kind = offset != null
    ? (detectUTC && offset === "Z" ? DateTimeKind.Utc : DateTimeKind.Local)
    : DateTimeKind.Unspecified;

  // parseRaw resolves the instant through JS Date, so anything below the
  // millisecond has to be recovered from the input separately.
  const epochNs = BigInt(parsed.getTime()) * 1_000_000n + BigInt(Format.subMillisecondTicks(str)) * 100n;
  const zone = kind === DateTimeKind.Utc ? "UTC" : hostTimeZone();

  return dateTime(new Temporal.Instant(epochNs).toZonedDateTimeISO(zone).toPlainDateTime(), kind);
}

export function tryParse(v: string, defValue: FSharpRef<DateTime>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
