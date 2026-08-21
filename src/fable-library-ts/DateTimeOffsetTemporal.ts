/**
 * DateTimeOffset as an offset-only Temporal.ZonedDateTime.
 *
 * .NET DateTimeOffset is a wall-clock date+time plus an explicit UTC offset (a
 * TimeSpan of whole minutes). A ZonedDateTime whose time zone is a fixed offset
 * (e.g. "-08:00") carries the wall-clock, the offset, and the absolute instant
 * natively — so there is no extra metadata to stamp and no re-stamping after
 * operations (arithmetic returns a ZonedDateTime that keeps the offset).
 *
 * The offset is exchanged with the rest of the runtime as a TimeSpan (Temporal
 * .Duration), matching how .NET types it. Equality/comparison are by instant
 * (offset-independent), matching .NET '=='; EqualsExact also compares the offset.
 *
 * toString / parse bridge to the JS-Date-based DateOffset.ts/Date.ts, whose
 * culture-sensitive output is pinned by the test suite.
 */

import { int64, fromFloat64 } from "./BigInt.ts";
import { FSharpRef } from "./Types.ts";
import { Exception, DateTimeKind, IDateTimeOffset, padWithZeros } from "./Util.ts";
import { TimeSpan, fromTicks as TimeSpan_fromTicks, totalNanoseconds as TimeSpan_totalNanoseconds } from "./TimeSpanTemporal.ts";
import { DateTime, dateTime, create as createDateTime, getTicks as DateTime_getTicks } from "./DateTimeTemporal.ts";
import { toString as Date_toString } from "./Date.ts";
import { parse as DateOffset_parse } from "./DateOffset.ts";

declare global {
  namespace Temporal {
    class ZonedDateTime {
      constructor(epochNanoseconds: bigint, timeZone: string);
      static from(item: {
        year: number, month: number, day: number, hour?: number, minute?: number, second?: number,
        millisecond?: number, microsecond?: number, nanosecond?: number, timeZone: string, offset?: string,
      }): ZonedDateTime;
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
      readonly offset: string;
      readonly offsetNanoseconds: number;
      readonly epochMilliseconds: number;
      readonly epochNanoseconds: bigint;
      add(duration: Temporal.Duration | Temporal.DurationLike): ZonedDateTime;
      subtract(duration: Temporal.Duration | Temporal.DurationLike): ZonedDateTime;
      until(other: ZonedDateTime, options?: { largestUnit?: string }): Temporal.Duration;
      startOfDay(): ZonedDateTime;
      withTimeZone(timeZone: string): ZonedDateTime;
      toPlainDateTime(): Temporal.PlainDateTime;
    }
    namespace Now {
      function timeZoneId(): string;
    }
  }
}

export type DateTimeOffset = Temporal.ZonedDateTime;

export const ZonedDateTime = Temporal.ZonedDateTime;
export type ZonedDateTime = Temporal.ZonedDateTime;

// Generic equality/comparison/hashing dispatch (Util.ts) uses .NET-style methods.
const proto = Temporal.ZonedDateTime.prototype as any;
proto.Equals = function (this: DateTimeOffset, other: DateTimeOffset): boolean { return equals(this, other); };
proto.CompareTo = function (this: DateTimeOffset, other: DateTimeOffset): number { return compare(this, other); };
proto.GetHashCode = function (this: DateTimeOffset): number { return hash(this); };
// Lets String.Format format this value without String.ts importing this module (see String.ts)
proto[Symbol.for("Fable.DateTimeFormattable")] = function (this: DateTimeOffset, format?: string): string { return toString(this, format); };

const nsPerMinute = 60_000_000_000n;

// Nanoseconds between the Unix epoch (1970-01-01) and DateTime.MinValue (0001-01-01)
const epochToMinValueNs = 62_135_596_800_000n * 1_000_000n;

function checkOffsetInRange(offsetNs: bigint) {
  if (offsetNs % nsPerMinute !== 0n) {
    throw new Exception("Offset must be specified in whole minutes.");
  }
  if (offsetNs > 14n * 60n * nsPerMinute || offsetNs < -14n * 60n * nsPerMinute) {
    throw new Exception("Offset must be within plus or minus 14 hours.");
  }
}

function offsetNanosecondsToZoneString(offsetNs: bigint): string {
  const sign = offsetNs < 0n ? "-" : "+";
  const abs = offsetNs < 0n ? -offsetNs : offsetNs;
  const totalMinutes = Number(abs / nsPerMinute);
  return `${sign}${padWithZeros(Math.trunc(totalMinutes / 60), 2)}:${padWithZeros(totalMinutes % 60, 2)}`;
}

function offsetToNanoseconds(offset: TimeSpan): bigint {
  return TimeSpan_totalNanoseconds(offset);
}

function fromEpoch(epochNs: bigint, offsetNs: bigint = 0n): DateTimeOffset {
  if (offsetNs === 0n)
    return new Temporal.ZonedDateTime(epochNs, "UTC");

  checkOffsetInRange(offsetNs);
  return new Temporal.ZonedDateTime(epochNs, offsetNanosecondsToZoneString(offsetNs));
}

export function offset(d: DateTimeOffset): TimeSpan {
  return TimeSpan_fromTicks(BigInt(d.offsetNanoseconds) / 100n);
}

export function create(
  year: number, month: number, day: number,
  h: number, m: number, s: number,
  ms: number | TimeSpan, offset?: TimeSpan): DateTimeOffset {
  // Overload without milliseconds: the 7th argument is the offset
  if (offset == null) {
    offset = ms as TimeSpan;
    ms = 0;
  }
  const offsetNs = offsetToNanoseconds(offset);
  checkOffsetInRange(offsetNs);
  return Temporal.ZonedDateTime.from({
    year, month, day, hour: h, minute: m, second: s, millisecond: ms as number,
    timeZone: offsetNanosecondsToZoneString(offsetNs),
  });
}

// Offset (ns) of the host time zone for a given wall-clock, DST-aware.
function hostOffsetNanoseconds(d: DateTime): bigint {
  const jsOffsetMinutes = new Date(d.year, d.month - 1, d.day, d.hour, d.minute, d.second, d.millisecond).getTimezoneOffset();
  return BigInt(-jsOffsetMinutes) * nsPerMinute;
}

export function fromDate(date: DateTime, offset?: TimeSpan): DateTimeOffset {
  const kind = date.kind ?? DateTimeKind.Unspecified;
  let offsetNs: bigint;
  switch (kind) {
    case DateTimeKind.Utc:
      if (offset != null && offsetToNanoseconds(offset) !== 0n) {
        throw new Exception("The UTC Offset for Utc DateTime instances must be 0.");
      }
      offsetNs = 0n;
      break;
    case DateTimeKind.Local:
      offsetNs = hostOffsetNanoseconds(date);
      if (offset != null && offsetToNanoseconds(offset) !== offsetNs) {
        throw new Exception("The UTC Offset of the local dateTime parameter does not match the offset argument.");
      }
      break;
    default:
      offsetNs = offset != null ? offsetToNanoseconds(offset) : hostOffsetNanoseconds(date);
      break;
  }
  checkOffsetInRange(offsetNs);
  return Temporal.ZonedDateTime.from({
    year: date.year, month: date.month, day: date.day,
    hour: date.hour, minute: date.minute, second: date.second, millisecond: date.millisecond,
    timeZone: offsetNanosecondsToZoneString(offsetNs),
  });
}

export function fromDateTime(dateOnly: DateTime, timeOnly: TimeSpan, offset: TimeSpan): DateTimeOffset {
  // dateOnly carries the date part (midnight); timeOnly is the time of day
  const offsetNs = offsetToNanoseconds(offset);
  return Temporal.ZonedDateTime.from({
    year: dateOnly.year, month: dateOnly.month, day: dateOnly.day,
    hour: 0, minute: 0, second: 0, timeZone: offsetNanosecondsToZoneString(offsetNs),
  }).add(timeOnly);
}

export function fromTicks(ticks: int64, offset: TimeSpan): DateTimeOffset {
  const offsetNs = offsetToNanoseconds(offset);
  // ticks are the local wall-clock ticks; the instant is that minus the offset
  const localNs = BigInt(ticks) * 100n - epochToMinValueNs;
  return fromEpoch(localNs - offsetNs, offsetNs);
}

export function fromUnixTimeMilliseconds(ms: int64): DateTimeOffset {
  return fromEpoch(BigInt(ms) * 1_000_000n);
}

export function fromUnixTimeSeconds(seconds: int64): DateTimeOffset {
  return fromEpoch(BigInt(seconds) * 1_000_000_000n);
}

export function minValue(): DateTimeOffset {
  return fromEpoch(-epochToMinValueNs);
}

export function maxValue(): DateTimeOffset {
  // 9999-12-31T23:59:59.9999999 UTC
  return fromEpoch(253_402_300_799_999_999_900n, 0n);
}

export function dayOfWeek(d: DateTimeOffset): number {
  return d.dayOfWeek % 7;
}

export function timeOfDay(d: DateTimeOffset): TimeSpan {
  // Elapsed since midnight (local wall-clock). DateTimeOffset values are tick-aligned, so this is too.
  return d.startOfDay().until(d);
}

export function date(d: DateTimeOffset): DateTime {
  return createDateTime(d.year, d.month, d.day, 0, 0, 0, 0, DateTimeKind.Unspecified);
}

export function dateTimeProp(d: DateTimeOffset): DateTime {
  return dateTime(d.toPlainDateTime(), DateTimeKind.Unspecified);
}

export function toUniversalTime(d: DateTimeOffset): DateTimeOffset {
  return fromEpoch(d.epochNanoseconds);
}

export function toLocalTime(d: DateTimeOffset): DateTimeOffset {
  const local = d.withTimeZone(Temporal.Now.timeZoneId());
  return fromEpoch(local.epochNanoseconds, BigInt(local.offsetNanoseconds));
}

export function utcDateTime(d: DateTimeOffset): DateTime {
  return dateTime(d.withTimeZone("UTC").toPlainDateTime(), DateTimeKind.Utc);
}

export function localDateTime(d: DateTimeOffset): DateTime {
  return dateTime(d.withTimeZone(Temporal.Now.timeZoneId()).toPlainDateTime(), DateTimeKind.Local);
}

export function add(d: DateTimeOffset, ts: TimeSpan): DateTimeOffset {
  return d.add(ts);
}

export function addYears(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ years: v });
}

export function addMonths(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ months: v });
}

export function addDays(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ days: v });
}

export function addHours(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ hours: v });
}

export function addMinutes(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ minutes: v });
}

export function addSeconds(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ seconds: v });
}

export function addMilliseconds(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add({ milliseconds: v });
}

export function addTicks(d: DateTimeOffset, v: int64): DateTimeOffset {
  return d.add(TimeSpan_fromTicks(v));
}

export function subtract(d: DateTimeOffset, that: DateTimeOffset | TimeSpan): DateTimeOffset | TimeSpan {
  return that instanceof Temporal.ZonedDateTime
    ? TimeSpan_fromTicks((d.epochNanoseconds - that.epochNanoseconds) / 100n) // instant difference
    : d.subtract(that);
}

export function equals(d1: DateTimeOffset, d2: DateTimeOffset): boolean {
  return d1.epochNanoseconds === d2.epochNanoseconds;
}

export function equalsExact(d1: DateTimeOffset, d2: DateTimeOffset): boolean {
  return d1.epochNanoseconds === d2.epochNanoseconds && d1.offsetNanoseconds === d2.offsetNanoseconds;
}

export function compare(d1: DateTimeOffset, d2: DateTimeOffset): number {
  const a = d1.epochNanoseconds, b = d2.epochNanoseconds;
  return a < b ? -1 : a > b ? 1 : 0;
}

export const compareTo = compare;

export function op_Addition(x: DateTimeOffset, y: TimeSpan): DateTimeOffset {
  return add(x, y);
}

export function op_Subtraction(x: DateTimeOffset, y: DateTimeOffset | TimeSpan): DateTimeOffset | TimeSpan {
  return subtract(x, y);
}

export function hash(d: DateTimeOffset): number {
  return Number(d.epochNanoseconds % 2147483647n);
}

export function toOffset(d: DateTimeOffset, offset: TimeSpan): DateTimeOffset {
  return fromEpoch(d.epochNanoseconds, offsetToNanoseconds(offset));
}

export function getUtcTicks(d: DateTimeOffset): int64 {
  return DateTime_getTicks(utcDateTime(d));
}

export function getTicks(d: DateTimeOffset): int64 {
  return DateTime_getTicks(dateTimeProp(d));
}

export function toUnixTimeMilliseconds(d: DateTimeOffset): int64 {
  return fromFloat64(d.epochMilliseconds);
}

export function toUnixTimeSeconds(d: DateTimeOffset): int64 {
  return fromFloat64(Math.floor(d.epochMilliseconds / 1000));
}

export function now(): DateTimeOffset {
  const z = new Temporal.ZonedDateTime(BigInt(Date.now()) * 1_000_000n, Temporal.Now.timeZoneId());
  return fromEpoch(z.epochNanoseconds, BigInt(z.offsetNanoseconds));
}

export function utcNow(): DateTimeOffset {
  return fromEpoch(BigInt(Date.now()) * 1_000_000n);
}

function toJsDateTimeOffset(d: DateTimeOffset): IDateTimeOffset {
  const jsDate = new Date(d.epochMilliseconds) as IDateTimeOffset;
  jsDate.offset = d.offsetNanoseconds / 1_000_000;
  return jsDate;
}

export function toString(d: DateTimeOffset, format?: string, _provider?: any): string {
  return Date_toString(toJsDateTimeOffset(d), format);
}

export function parse(str: string): DateTimeOffset {
  const jsDto = DateOffset_parse(str);
  const offsetNs = BigInt(jsDto.offset ?? 0) * 1_000_000n;
  return fromEpoch(BigInt(jsDto.getTime()) * 1_000_000n, offsetNs);
}

export function tryParse(v: string, defValue: FSharpRef<DateTimeOffset>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
