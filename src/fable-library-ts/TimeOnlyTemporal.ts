import { FSharpRef } from "./Types.ts";
import { toInt64, fromFloat64, int64 } from "./BigInt.ts";
import { TimeSpan, fromTicks as TimeSpan_fromTicks, totalNanoseconds as TimeSpan_totalNanoseconds } from "./TimeSpanTemporal.ts";
import { Exception } from "./Util.ts";

export type TimeOnly = Temporal.PlainTime;
export const PlainTime = Temporal.PlainTime;
export type PlainTime = Temporal.PlainTime;

// The generic equality/comparison/hashing helpers in Util.ts dispatch at runtime
// on .NET-style Equals/CompareTo/GetHashCode methods. Attach them so TimeOnly
// also works in generic contexts: records, tuples, erased generics, etc.
const proto = Temporal.PlainTime.prototype as any;
proto.Equals = function (this: TimeOnly, other: TimeOnly): boolean { return this.equals(other); };
proto.CompareTo = function (this: TimeOnly, other: TimeOnly): number { return Temporal.PlainTime.compare(this, other); };
proto.GetHashCode = function (this: TimeOnly): number { return hash(this); };
// Lets String.Format format this value without String.ts importing this module (see String.ts)
proto[Symbol.for("Fable.DateTimeFormattable")] = function (this: TimeOnly, format?: string): string { return toString(this, format); };

const nanosecondsPerDay = 86_400_000_000_000;
const nanosecondsPerDayBig = 86_400_000_000_000n;

function totalNanoseconds(t: TimeOnly): number {
  return ((t.hour * 60 + t.minute) * 60 + t.second) * 1_000_000_000
    + t.millisecond * 1_000_000 + t.microsecond * 1_000 + t.nanosecond;
}

function fromNanoseconds(n: number): TimeOnly {
  return new Temporal.PlainTime(
    Math.floor(n / 3_600_000_000_000),
    Math.floor(n / 60_000_000_000) % 60,
    Math.floor(n / 1_000_000_000) % 60,
    Math.floor(n / 1_000_000) % 1000,
    Math.floor(n / 1_000) % 1000,
    n % 1000);
}

export function create(h: number = 0, m: number = 0, s: number = 0, ms: number = 0): TimeOnly {
  return new Temporal.PlainTime(h, m, s, ms);
}

export function fromTicks(ticks: number | bigint): TimeOnly {
  return fromNanoseconds(Number(BigInt(ticks) * 100n));
}

export function fromTimeSpan(timeSpan: TimeSpan): TimeOnly {
  const ns = Number(TimeSpan_totalNanoseconds(timeSpan));
  if (ns < 0 || ns >= nanosecondsPerDay)
    throw new Exception("The TimeSpan describes an unrepresentable TimeOnly.");

  return fromNanoseconds(ns);
}

export function fromDateTime(d: Temporal.PlainDateTime): TimeOnly {
  // Under the Temporal representation a DateTime is a PlainDateTime (kind-agnostic wall-clock)
  return new Temporal.PlainTime(d.hour, d.minute, d.second, d.millisecond, d.microsecond, d.nanosecond);
}

export function minValue(): TimeOnly {
  return new Temporal.PlainTime();
}

export function maxValue(): TimeOnly {
  // This is "23:59:59.9999999" (.NET tick precision)
  return new Temporal.PlainTime(23, 59, 59, 999, 999, 900);
}

export function ticks(t: TimeOnly): int64 {
  return toInt64(fromFloat64(totalNanoseconds(t) / 100));
}

export function toTimeSpan(t: TimeOnly): TimeSpan {
  // TimeOnly is tick-precise, so the nanosecond count is always a multiple of 100.
  return TimeSpan_fromTicks(Math.round(totalNanoseconds(t) / 100));
}

// Kept in bigint: a TimeOnly itself always fits a float64 exactly (under 2^47 ns),
// but the amount added does not — a TimeSpan of more than ~104 days already
// exceeds Number.MAX_SAFE_INTEGER in nanoseconds, and .NET wraps it modulo a day
// rather than saturating, so the low ticks are the part that must survive.
function addNanoseconds(t: TimeOnly, deltaNs: bigint, wrappedDays?: FSharpRef<number>): TimeOnly {
  const totalNs = BigInt(totalNanoseconds(t)) + deltaNs;
  let days = totalNs / nanosecondsPerDayBig;
  let remainder = totalNs % nanosecondsPerDayBig;

  // BigInt division truncates toward zero; .NET wraps toward the previous day.
  if (remainder < 0n) {
    remainder += nanosecondsPerDayBig;
    days -= 1n;
  }

  if (wrappedDays !== undefined) {
    wrappedDays.contents = Number(days);
  }

  return fromNanoseconds(Number(remainder));
}

export function add(t: TimeOnly, ts: TimeSpan, wrappedDays?: FSharpRef<number>): TimeOnly {
  return addNanoseconds(t, TimeSpan_totalNanoseconds(ts), wrappedDays);
}

export function addHours(t: TimeOnly, h: number): TimeOnly {
  // Scale to ticks first, as .NET does, then widen — `h` hours in nanoseconds
  // overflows the exact float64 range two orders of magnitude sooner.
  return addNanoseconds(t, BigInt(Math.round(h * 36_000_000_000)) * 100n);
}

export function addMinutes(t: TimeOnly, m: number): TimeOnly {
  return addNanoseconds(t, BigInt(Math.round(m * 600_000_000)) * 100n);
}

export function isBetween(t: TimeOnly, start: TimeOnly, end: TimeOnly): boolean {
  return Temporal.PlainTime.compare(start, end) <= 0
    ? (Temporal.PlainTime.compare(start, t) <= 0 && Temporal.PlainTime.compare(end, t) > 0)
    : (Temporal.PlainTime.compare(start, t) <= 0 || Temporal.PlainTime.compare(end, t) > 0);
}

export function equals(x: TimeOnly, y: TimeOnly): boolean {
  return x.equals(y);
}

export function compare(x: TimeOnly, y: TimeOnly): number {
  return Temporal.PlainTime.compare(x, y);
}

export function hash(t: TimeOnly): number {
  return totalNanoseconds(t) % 2147483647;
}

export function op_Subtraction(left: TimeOnly, right: TimeOnly): TimeSpan {
  // Returns the elapsed TimeSpan, wrapping around midnight
  const ns = (totalNanoseconds(left) - totalNanoseconds(right) + nanosecondsPerDay) % nanosecondsPerDay;
  return TimeSpan_fromTicks(Math.round(ns / 100));
}

export function toString(t: TimeOnly, format = "t", _provider?: any): string {
  switch (format) {
    case "t":
      return t.toString({ smallestUnit: "minute" });
    case "r":
    case "R":
    case "T":
      return t.toString({ smallestUnit: "second" });
    case "o":
    case "O":
      // .NET tick precision is 7 fractional digits
      return t.toString({ fractionalSecondDigits: 7 });
    default:
      throw new Exception("Custom formats are not supported");
  }
}

export function parse(str: string): TimeOnly {
  // Allowed format types:
  // hh:mm
  // hh:mm:ss
  // hh:mm:ss.fffffff
  const r = /^\s*([0-1]?\d|2[0-3])\s*:\s*([0-5]?\d)(\s*:\s*([0-5]?\d)(\.(\d+))?)?\s*$/.exec(str);
  if (r != null && r[1] != null && r[2] != null) {
    const h = +r[1];
    const m = +r[2];
    const s = r[4] != null ? +r[4] : 0;
    // .NET resolves the fraction down to a single tick (7 digits) and truncates
    // anything finer, so the digits are read as ticks rather than milliseconds.
    const subSecondTicks = r[6] != null ? +r[6].padEnd(7, "0").substring(0, 7) : 0;

    return fromNanoseconds(((h * 60 + m) * 60 + s) * 1_000_000_000 + subSecondTicks * 100);
  }

  throw new Exception(`String '${str}' was not recognized as a valid TimeOnly.`);
}

export function tryParse(v: string, defValue: FSharpRef<TimeOnly>): boolean {
  try {
    defValue.contents = parse(v);
    return true;
  } catch {
    return false;
  }
}
