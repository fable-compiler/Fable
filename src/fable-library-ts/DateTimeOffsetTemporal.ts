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
 * Formatting and parsing run on the Temporal value's own fields (see
 * DateTimeFormat.ts) rather than through a JS Date, which would truncate every
 * value to milliseconds.
 */

import { int64, fromFloat64 } from "./BigInt.ts";
import { FSharpRef } from "./Types.ts";
import { Exception, DateTimeKind, padWithZeros } from "./Util.ts";
import {
  TimeSpan, fromTicks as TimeSpan_fromTicks, fromUnits as TimeSpan_fromUnits,
  totalNanoseconds as TimeSpan_totalNanoseconds,
} from "./TimeSpanTemporal.ts";
import { DateTime, dateTime, create as createDateTime, getTicks as DateTime_getTicks } from "./DateTimeTemporal.ts";
import * as Format from "./DateTimeFormat.ts";

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

function hostTimeZone(): string {
  return Temporal.Now.timeZoneId();
}

// .NET DateTimeOffset has 100ns (tick) precision; Temporal instants are
// nanosecond-precise. Floors, so the tick count from year 1 is floored too.
function truncateToTicks(epochNs: bigint): bigint {
  const remainder = epochNs % 100n;
  return remainder >= 0n ? epochNs - remainder : epochNs - remainder - 100n;
}

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

// Offset (ns) of the host time zone for a given wall-clock, DST-aware. Resolved
// through Temporal rather than `new Date(year, ...)`, which maps years 0-99 into
// 1900-1999 and would pick that era's rules.
function hostOffsetNanoseconds(d: DateTime): bigint {
  return BigInt(d.toZonedDateTime(hostTimeZone()).offsetNanoseconds);
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
    hour: date.hour, minute: date.minute, second: date.second,
    millisecond: date.millisecond, microsecond: date.microsecond, nanosecond: date.nanosecond,
    timeZone: offsetNanosecondsToZoneString(offsetNs),
  });
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
  const local = d.withTimeZone(hostTimeZone());
  return fromEpoch(local.epochNanoseconds, BigInt(local.offsetNanoseconds));
}

export function utcDateTime(d: DateTimeOffset): DateTime {
  return dateTime(d.withTimeZone("UTC").toPlainDateTime(), DateTimeKind.Utc);
}

export function localDateTime(d: DateTimeOffset): DateTime {
  return dateTime(d.withTimeZone(hostTimeZone()).toPlainDateTime(), DateTimeKind.Local);
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
  return d.add(TimeSpan_fromUnits(v, "days"));
}

export function addHours(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add(TimeSpan_fromUnits(v, "hours"));
}

export function addMinutes(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add(TimeSpan_fromUnits(v, "minutes"));
}

export function addSeconds(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add(TimeSpan_fromUnits(v, "seconds"));
}

export function addMilliseconds(d: DateTimeOffset, v: number): DateTimeOffset {
  return d.add(TimeSpan_fromUnits(v, "milliseconds"));
}

export function addTicks(d: DateTimeOffset, v: int64): DateTimeOffset {
  return d.add(TimeSpan_fromTicks(v));
}

// Split rather than returning a union — see the note in DateTimeTemporal.ts.
export function subtractDate(d: DateTimeOffset, that: DateTimeOffset): TimeSpan {
  return TimeSpan_fromTicks((d.epochNanoseconds - that.epochNanoseconds) / 100n); // instant difference
}

export function subtractTimeSpan(d: DateTimeOffset, ts: TimeSpan): DateTimeOffset {
  return d.subtract(ts);
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
  const z = Temporal.Now.zonedDateTimeISO(hostTimeZone());
  return fromEpoch(truncateToTicks(z.epochNanoseconds), BigInt(z.offsetNanoseconds));
}

export function utcNow(): DateTimeOffset {
  return fromEpoch(truncateToTicks(Temporal.Now.instant().epochNanoseconds));
}

// --- Formatting and parsing ---

// Wall-clock fields for the formatter, with the sub-second component in .NET
// ticks so "O" and the f/F specifiers can print all 7 digits.
function toDateInfo(z: DateTimeOffset): Format.DateInfo {
  return {
    year: z.year,
    month: z.month,
    day: z.day,
    hour: z.hour,
    minute: z.minute,
    second: z.second,
    tick: z.millisecond * 10_000 + z.microsecond * 10 + Math.trunc(z.nanosecond / 100),
    dayOfWeek: z.dayOfWeek % 7,
  };
}

export function toString(d: DateTimeOffset, format?: string, _provider?: any): string {
  const offsetString = () => offsetNanosecondsToZoneString(BigInt(d.offsetNanoseconds));

  return Format.dateToString(
    {
      info: toDateInfo(d),
      utcInfo: () => toDateInfo(d.withTimeZone("UTC")),
      roundTripSuffix: offsetString,
      defaultSuffix: " " + offsetString(),
      // K/z print the value's own offset, never the host zone's, so Local is
      // passed as the kind and that offset stands in for the host one.
      kind: DateTimeKind.Local,
      hostOffsetString: offsetString,
    },
    format);
}

export function parse(str: string): DateTimeOffset {
  const [parsed, offsetMatch] = Format.parseRaw(str);
  // parseRaw resolves the instant through JS Date, so anything below the
  // millisecond has to be recovered from the input separately.
  const epochNs = BigInt(parsed.getTime()) * 1_000_000n + BigInt(Format.subMillisecondTicks(str)) * 100n;

  const offsetNs = offsetMatch == null
    ? BigInt(new Temporal.Instant(epochNs).toZonedDateTimeISO(hostTimeZone()).offsetNanoseconds)
    : (offsetMatch === "Z" ? 0n : BigInt(offsetMatch) * nsPerMinute);

  return fromEpoch(epochNs, offsetNs);
}

export function tryParse(v: string, defValue: FSharpRef<DateTimeOffset>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
